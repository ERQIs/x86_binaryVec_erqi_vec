#include "bolt/Passes/ErqiVec.h"
#include "bolt/Core/ParallelUtilities.h"
#include "bolt/Passes/DataflowInfoManager.h"
#include "bolt/Passes/SimLoopAnalysis.h"

#include <map>

namespace llvm {
namespace bolt {

using namespace llvm::bolt::simLoop;

    // static const MCPhysReg GPR64[] = {
    //     X86::RAX, X86::RBX, X86::RCX, X86::RDX,
    //     X86::RSI, X86::RDI, X86::RBP, X86::RSP,
    //     X86::R8,  X86::R9,  X86::R10, X86::R11,
    //     X86::R12, X86::R13, X86::R14, X86::R15
    // };


class PhisicalRegManager {
    
private:
    int vectorReg_ID;
    int GPR_ID;
    BinaryContext* BC;
    int allocateNewVReg() {
        return vectorReg_ID ++;
    }
    int allocateNewGPReg() {
        return GPR_ID --;
    }

    std::map<std::string, int> vRegToPeg;

public:
    PhisicalRegManager()  {
        vectorReg_ID = 0;
        GPR_ID = 15;
    };
    PhisicalRegManager(BinaryContext& BC) : BC(&BC) {
        vectorReg_ID = 0;
        GPR_ID = 15;
    };

    int getPhisicalRegister(RegOperand* ro) {
        if (! ro->symbolic ) {
            return ro->getId();
        }
        std::string regName = ro->getName();
        if (vRegToPeg.find(regName) == vRegToPeg.end()) {
            assert(BC != nullptr && "BC uninitialized");
            int newPhisicalReg;
            switch(ro->getWidth()) {
                case 4:
                    newPhisicalReg = BC->MIB->getGPR32(allocateNewGPReg());
                    break;
                case 8:
                    newPhisicalReg = BC->MIB->getGPR64(allocateNewGPReg());
                    break;
                case 64:
                    newPhisicalReg = BC->MIB->getZMM(allocateNewVReg());
                    break;
                default:
                    assert(false && "unsupported register length");
                    break;
            }
            vRegToPeg.emplace(ro->getName(), newPhisicalReg);
            return newPhisicalReg;
        } else {
            return vRegToPeg.find(regName)->second;
        }
    }
};
PhisicalRegManager simLoopPREGManager;


std::map<int, MCOperand> SimLoopExpOps;

class DataflowInfoManager;

// 这个函数应该把基本块分割成两个——一个preheader和一个loopBody。传进来的基本块会变成preheader，然后返回的基本块会变成loopBody
BinaryBasicBlock* splitSingleLoop(BinaryBasicBlock* SingleLoop) {
    if (!SingleLoop) {
        llvm::errs() << "Error: SingleLoop is nullptr\n";
        return nullptr;
    }
    BinaryFunction *BF = SingleLoop->getFunction();
    BinaryContext &BC = BF->getBinaryContext();

    // // Step 1: 创建新的基本块 loopBlock

    std::unique_ptr<BinaryBasicBlock> LoopBlockUniPtr = BF->createBasicBlock();
    BinaryBasicBlock* LoopBlock = LoopBlockUniPtr.get();

    // Step 2: 找到 singleLoop 的出口基本块
    BinaryBasicBlock *ExitBlock = nullptr;
    for (BinaryBasicBlock *Succ : SingleLoop->successors()) {
        if (Succ != SingleLoop) {
            ExitBlock = Succ;
            break;
        }
    }

    // Step 3: 将 SingleLoop 的所有指令移动到 loopBlock
    LoopBlock->addInstructions(SingleLoop->begin(), SingleLoop->end());
    SingleLoop->clear();

    // Step 4: 修改 loopBlock 的最后一条条件跳转指令的目标为 loopBlock

    MCInst *LastInst = LoopBlock->getLastNonPseudoInstr();
    if (BC.MIB->isConditionalBranch(*LastInst)) {
        BC.MIB->replaceBranchTarget(*LastInst, LoopBlock->getLabel(), BC.Ctx.get());
    } else {
        llvm::errs() << "Error: Last instruction is not a conditional branch\n";
        return nullptr;
    }

    // Step 5: 将 loopBlock 插入到 SingleLoop 的后面
    std::vector<std::unique_ptr<BinaryBasicBlock>> NewBlocks;
    NewBlocks.push_back(std::move(LoopBlockUniPtr));
    BF->insertBasicBlocks(SingleLoop, std::move(NewBlocks));

    // Step 6: 更新后继块信息
    SingleLoop->removeAllSuccessors();
    SingleLoop->addSuccessor(LoopBlock);

    LoopBlock->removeAllSuccessors();
    LoopBlock->addSuccessor(LoopBlock);
    if (ExitBlock) {
        LoopBlock->addSuccessor(ExitBlock);
    }

    llvm::errs() << "Successfully split the loop into SingleLoop and LoopBlock\n";

    return LoopBlock;
}

void simLoopToMCI(std::vector<MCInst>& MCInsts, std::vector<std::shared_ptr<SimLoopInst>>& SMInsts, BinaryContext& BC, std::vector<MCInst>& appendToHeader);

bool isCmpInstruction(const MCInst &inst, BinaryContext& BC) {
    // 创建一个字符串缓冲区
    std::string instStr;
    llvm::raw_string_ostream rso(instStr);

    // 将指令打印到字符串流
    BC.printInstruction(rso, inst);

    // 确保将流的内容刷新到字符串中
    rso.flush();

    // 检查字符串是否以 "cmp" 开头（大小写敏感）
    return instStr.find("cmp") != std::string::npos;
}

void simLoopToMCI(std::vector<MCInst>& MCInsts, std::vector<std::shared_ptr<SimLoopInst>>& SMInsts, BinaryContext& BC, std::vector<MCInst>& appendToHeader) {
   for (auto SMInstPtr : SMInsts) {
        SimLoopInst* SMInst = SMInstPtr.get();
        if (BinaryInst* bi =  dynamic_cast<BinaryInst*>(SMInst)) {
            assert(bi->getDst()->getType() == OprType::Register && "non register dst");
            switch (bi->getType()) {
                case InstType::Add : {

                    if (bi->getSrc1()->getType() == OprType::Immediate ||
                        bi->getSrc2()->getType() == OprType::Immediate) {
                        assert((bi->getSrc2()->getType() == OprType::Register || bi->getSrc1()->getType() == OprType::Register)
                               && "both are imm in sim Loop Add");
                        ImmOperand *immsrc;
                        RegOperand *regsrc;
                        if (bi->getSrc1()->getType() == OprType::Immediate){
                            immsrc = static_cast<ImmOperand*>(bi->getSrc1().get());
                            regsrc = static_cast<RegOperand*>(bi->getSrc2().get());
                        } else {
                            immsrc = static_cast<ImmOperand*>(bi->getSrc2().get());
                            regsrc = static_cast<RegOperand*>(bi->getSrc1().get());
                        }
                        RegOperand* dstr = static_cast<RegOperand*>(bi->getDst().get());
                        if (regsrc->getId() == dstr->getId()) {
                            if (immsrc->isBoltExp) {
                                MCInst lea;
                                MCOperand expOpr = SimLoopExpOps.find(immsrc->value)->second;
                                int srcReg = simLoopPREGManager.getPhisicalRegister(regsrc);
                                int dstReg = simLoopPREGManager.getPhisicalRegister(dstr);
                                BC.MIB->createLeaAsAdd(lea, srcReg, expOpr, dstReg);
                                MCInsts.push_back(lea);
                                // assert(false && "unimplemented");
                            } else {
                                MCInst add;
                                int reg = simLoopPREGManager.getPhisicalRegister(dstr);
                                BC.MIB->createGPRinc(add, reg, immsrc->value);
                                MCInsts.push_back(add);
                            }
                        } else {
                            assert(false && "unimplemented");
                        }
                    } else if (bi->getSrc1()->getType() == OprType::Register &&
                        bi->getSrc2()->getType() == OprType::Register) {
                            MCInst add;
                            RegOperand* src1r = static_cast<RegOperand*>(bi->getSrc1().get());
                            RegOperand* src2r = static_cast<RegOperand*>(bi->getSrc2().get());
                            RegOperand* dstr = static_cast<RegOperand*>(bi->getDst().get());
                            int src1pr = simLoopPREGManager.getPhisicalRegister(src1r);
                            int src2pr = simLoopPREGManager.getPhisicalRegister(src2r);
                            int dstpr = simLoopPREGManager.getPhisicalRegister(dstr);
                            if (src1r->getId() == dstr->getId()) {
                                BC.MIB->createADD64rr(add, src2pr, dstpr); 
                            } else if (src2r->getId() == dstr->getId()) {
                                BC.MIB->createADD64rr(add, src1pr, dstpr); 
                            } else {
                                assert(false && "unimplemented");
                            }
                            MCInsts.push_back(add);
                        }
                }

                case InstType::Sub : {
                    break;
                }
                case InstType::Mul : {
                    if (bi->getSrc1()->getType() == OprType::Immediate ||
                        bi->getSrc2()->getType() == OprType::Immediate) {
                        assert((bi->getSrc2()->getType() == OprType::Register || bi->getSrc1()->getType() == OprType::Register)
                               && "both are imm in sim Loop Mul");
                        ImmOperand *immsrc;
                        RegOperand *regsrc;
                        if (bi->getSrc1()->getType() == OprType::Immediate){
                            immsrc = static_cast<ImmOperand*>(bi->getSrc1().get());
                            regsrc = static_cast<RegOperand*>(bi->getSrc2().get());
                        } else {
                            immsrc = static_cast<ImmOperand*>(bi->getSrc2().get());
                            regsrc = static_cast<RegOperand*>(bi->getSrc1().get());
                        }
                        RegOperand* dstr = static_cast<RegOperand*>(bi->getDst().get());
                        if (regsrc->getId() == dstr->getId()) {
                            if (immsrc->isBoltExp) {
                                assert(false && "multiplying with exp");
                            } else {
                                MCInst mul;
                                int reg = simLoopPREGManager.getPhisicalRegister(dstr);
                                BC.MIB->createGPRMulImm(mul, reg, immsrc->value);
                                MCInsts.push_back(mul);
                            }
                        } else {
                            assert(false && "unimplemented");
                        }
                    } else {
                        assert(false && "unimplemented");
                    }
                    break;
                }
                case InstType::Load : {
                    break;
                }
                case InstType::Store : {
                    break;
                }
                default : {
                    assert(false && "unimplemented");
                }
            }
        } else if (LoadInst* li =  dynamic_cast<LoadInst*>(SMInst)) {
            RegOperand* src1r = static_cast<RegOperand*>(li->getSrc1().get());
            RegOperand* dstr = static_cast<RegOperand*>(li->getDst().get());
            if (src1r->getType() == OprType::Register && dstr->getType() == OprType::Register) {
                    int srcpr = simLoopPREGManager.getPhisicalRegister(src1r);
                    int dstpr = simLoopPREGManager.getPhisicalRegister(dstr);
                    MCInst load;
                    BC.MIB->createLoadFromPtrReg(load, srcpr, dstpr);
                    MCInsts.push_back(load);
            } else {
                assert(false && "unimplemented");
            }
            // assert(false && "unimplemented");
        } else if (StoreInst* si = dynamic_cast<StoreInst*>(SMInst)) {
            assert(false && "unimplemented");
        } else if (VecInst* vi = dynamic_cast<VecInst*>(SMInst)) {
            assert(vi->getDst()->getType() == OprType::Register && "non register dst");
            if (vi->getType() == InstType::Mov) {
                if (vi->getSrc1()->getType() == OprType::Immediate) {
                    MCInst mci;
                    ImmOperand* immOp = static_cast<ImmOperand*>(vi->getSrc1().get());
                    RegOperand* immVReg = new RegOperand(-immOp->value, "zmm_with_" + std::to_string(immOp->value), 64, true);
                    int immPR = simLoopPREGManager.getPhisicalRegister(immVReg);

                    RegOperand* dstOp = static_cast<RegOperand*>(vi->getDst().get());
                    int dstPR = simLoopPREGManager.getPhisicalRegister(dstOp);

                    int tmpGPR = BC.MIB->getGPR32(15);
                    BC.MIB->createPushRegister(mci, tmpGPR, 4);
                    appendToHeader.push_back(mci);

                    BC.MIB->createMovImmToGPR(mci, tmpGPR, immOp->value);
                    appendToHeader.push_back(mci);

                    BC.MIB->createBroadCast32ToZmm(mci, tmpGPR, immPR);
                    appendToHeader.push_back(mci);

                    BC.MIB->createPopRegister(mci, tmpGPR, 4);
                    appendToHeader.push_back(mci);

                    BC.MIB->createZmmMov(mci, immPR, dstPR);
                    MCInsts.push_back(mci);

                    continue;
                } else {
                    assert(false && "unimplemented");
                }
            }

            assert(vi->getSrc1()->getType() == OprType::Register && "nonregister src");
            if (vi->getSrc2() != nullptr)
                assert(vi->getSrc2()->getType() == OprType::Register && "nonregister src");

            RegOperand* left = static_cast<RegOperand*> (vi->getSrc1().get());
            RegOperand* dst = static_cast<RegOperand*> (vi->getDst().get());
            int pRegLeft = simLoopPREGManager.getPhisicalRegister(left);
            int pRegDst = simLoopPREGManager.getPhisicalRegister(dst);
            MCInst vMCI;
            switch (vi->getType()) {
                case InstType::Add : {
                    RegOperand* right = static_cast<RegOperand*> (vi->getSrc2().get());
                    int pRegRight = simLoopPREGManager.getPhisicalRegister(right);
                    if (vi->getElemWidth() == 4){
                        BC.MIB->createZmmAdd(vMCI, pRegDst, pRegRight, pRegLeft);
                    } else if (vi->getElemWidth() == 1) {
                        BC.MIB->createZmmAddb(vMCI, pRegDst, pRegRight, pRegLeft);
                    }
                    break;
                }
                case InstType::Sub : {
                    RegOperand* right = static_cast<RegOperand*> (vi->getSrc2().get());
                    int pRegRight = simLoopPREGManager.getPhisicalRegister(right);
                    BC.MIB->createZmmSub(vMCI, pRegDst, pRegLeft, pRegRight);
                    break;
                }
                case InstType::Mul : {
                    RegOperand* right = static_cast<RegOperand*> (vi->getSrc2().get());
                    int pRegRight = simLoopPREGManager.getPhisicalRegister(right);
                    BC.MIB->createZmmMul(vMCI, pRegDst, pRegRight, pRegLeft);
                    break;
                }
                case InstType::Load : {
                    BC.MIB->createZmmMovRm(vMCI, pRegLeft, pRegDst);
                    break;
                }
                case InstType::Store : {
                    BC.MIB->createZmmMovMr(vMCI, pRegLeft, pRegDst);
                    break;
                }
                default : {
                    assert(false && "unimplemented");
                }

            }
            MCInsts.push_back(vMCI);
        } else if (MovInst* mi =  dynamic_cast<MovInst*>(SMInst)) {
            RegOperand* src = static_cast<RegOperand*> (mi->getSrc1().get());
            RegOperand* dst = static_cast<RegOperand*> (mi->getDst().get());
            int pRegSrc = simLoopPREGManager.getPhisicalRegister(src);
            int pRegDst = simLoopPREGManager.getPhisicalRegister(dst);
            
            MCInst mov;
            BC.MIB->createMovqrr(mov,pRegDst, pRegSrc);
            MCInsts.push_back(mov);
        } else {
            assert(false && "unsupported simLoopInst type");
        }
   }
}

bool isCallInst(MCInst inst, BinaryContext &BC) {
    std::string InstStr;
    raw_string_ostream RS(InstStr);

    // 将指令打印到字符串
    BC.printInstruction(RS, inst);
    RS.flush();

    // 检查是否包含 "call" 指令
    if (InstStr.find("call") != std::string::npos) {
        return true; // 基本块不能简化
    }
    return false;
}

bool isJumpInst(MCInst inst, BinaryContext &BC) {
    std::string InstStr;
    raw_string_ostream RS(InstStr);

    // 将指令打印到字符串
    BC.printInstruction(RS, inst);
    RS.flush();

    // 检查是否包含 "call" 指令
    if (InstStr.find("j") != std::string::npos) {
        return true; // 基本块不能简化
    }
    return false;
}

bool canSimplify(const BinaryBasicBlock &BB) {
    // 遍历基本块中的所有指令
    for (const auto &Inst : BB) {
        std::string InstStr;
        raw_string_ostream RS(InstStr);

        // 将指令打印到字符串
        BB.getFunction()->getBinaryContext().printInstruction(RS, Inst);
        RS.flush();

        // 检查是否包含 "call" 指令
        if (InstStr.find("call") != std::string::npos) {
            return false; // 基本块不能简化
        }
    }

    return true; // 基本块可以简化
}

std::vector<std::shared_ptr<SimLoopInst>> *simLoopLifter(BinaryBasicBlock* BB) {
    errs() << "\n\ntesting lifter\n";
    BinaryContext &BC = BB->getFunction()->getBinaryContext();
    std::vector<MCInst> MCInsts;
    std::vector<std::shared_ptr< simLoop::SimLoopInst>> *simLoopInsts = new std::vector<std::shared_ptr< simLoop::SimLoopInst>>();
    for (auto MCInst : *BB) {
        MCInsts.push_back(MCInst);
    }

    BC.MIB->simLoopLifter(MCInsts, *simLoopInsts, SimLoopExpOps);

    for (auto simLoopInst : *simLoopInsts) {
        errs() << simLoopInst->getDump() << "\n";
    }

    return simLoopInsts;
}

void handleSingleBlockLoop(BinaryBasicBlock* singleLoop)
{
    // this is for testing
    BinaryContext &BC = singleLoop->getFunction()->getBinaryContext();
    errs() << singleLoop->getFunction()->getNames().at(0) << "\n";
    auto funcName = singleLoop->getFunction()->getNames().at(0);

    if (! canSimplify(*singleLoop)) {
        return;
    }

    BinaryBasicBlock* loopBody = splitSingleLoop(singleLoop);
    BinaryBasicBlock* loopHeader = singleLoop;

    std::vector<std::shared_ptr<SimLoopInst>> *simLoopInsts;

    simLoopInsts = simLoopLifter(loopBody);

    SimLoopInfo info;
    std::vector<std::shared_ptr<SimLoopInst>> simLoopHeader;
    std::vector<std::shared_ptr<SimLoopInst>> simLoopBody;
    

    resetAlgorithm();
    // analyze
    SimLoopAnalysis( *simLoopInsts, info );

    // reconstruct vectorized loop header and loop body 
    initializeBasesInHeader(simLoopHeader, info);
    generateLoopBody(simLoopBody, info, 64);

    errs() << "\n\nloop header:\n";
    for (auto inst : simLoopHeader) {
        errs() << inst->getDump()  << "\n";
    }

    // rewrite header to MCInst
    std::vector<MCInst>  headerMCInsts;
    std::vector<MCInst>  appendToHeader;
    simLoopToMCI(headerMCInsts, simLoopHeader, BC, appendToHeader);

    for (auto inst : headerMCInsts) {
        BC.printInstruction(errs(), inst);
    }


    errs() << "\n\nloop body:\n";
    for (auto inst : simLoopBody) {
        errs() << inst->getDump() << "\n";
    }

    // rewrite body to MCInst
    std::vector<MCInst>  bodyMCInsts;
    simLoopToMCI(bodyMCInsts, simLoopBody, BC, appendToHeader);

    for (auto inst : bodyMCInsts) {
        BC.printInstruction(errs(), inst);
    }

    // save conditional instructions
    MCInst jmp = *loopBody->rbegin();
    MCInst cmp = *std::next(loopBody->rbegin());

    errs() << "here is the flag";
    BC.printInstruction(errs(), jmp);
    BC.printInstruction(errs(), cmp);

    // put rewritten header insts to basic block
    loopHeader->clear();
    loopBody->clear();
    for (auto inst: headerMCInsts) {
        loopHeader->addInstruction(inst);
    }

    for (auto inst: appendToHeader) {
        loopHeader->addInstruction(inst);
    }

    for (auto inst: bodyMCInsts) {
        loopBody->addInstruction(inst);
    }

    // put the conditional insts to BasicBlock
    if (isCmpInstruction(cmp, BC)) {

        if (cmp.getOperand(0).isReg()) {
            int preg = simLoopPREGManager.getPhisicalRegister(info.indicatorReg.get());
            cmp.getOperand(0) = MCOperand::createReg(preg);
        }

        errs() << "set cmp to:";
        BC.printInstruction(errs(), cmp);
        loopBody->addInstruction(cmp);
    }
    loopBody->addInstruction(jmp);
}

void handleForLoop(BinaryBasicBlock* conditions, BinaryBasicBlock* operations) {
    BinaryFunction* BF = conditions->getFunction();
    BinaryContext &BC = BF->getBinaryContext();
    MCInst condiLastInstr = *(conditions->getLastNonPseudoInstr());
    MCInst operLastInstr = *(operations->getLastNonPseudoInstr());
    if (isJumpInst(operLastInstr, BC)) {
        return;
    }
    if (!isJumpInst(condiLastInstr, BC)) {
        return;
    }

    for (MCInst conInst : *conditions) {
        operations->addInstruction(conInst);
    }

    // 找到循环出口的基本块
    BinaryBasicBlock* exit = nullptr;
    for (auto BB : conditions->successors()) {
        if (BB != operations) {
            exit = BB;
        }
    }
    assert(exit != nullptr && "for loop have no exit block");

    // 把操作块的跳转指令的目标变成自己，其实没啥必要
    MCInst *LastInst = operations->getLastNonPseudoInstr();
    if (BC.MIB->isConditionalBranch(*LastInst)) {
        BC.MIB->replaceBranchTarget(*LastInst, operations->getLabel(), BC.Ctx.get());
    } else {
        llvm::errs() << "Error: Last instruction is not a conditional branch\n";
        return;
    }

    MCInst jmp;
    BC.MIB->createUncondBranch(jmp, exit->getLabel(), BC.Ctx.get());
    // operations->addInstruction(jmp);

    // 创建一个exitblock，这里面只有跳到exit的一个jmp指令
    std::unique_ptr<BinaryBasicBlock> exitBlockUniPtr = BF->createBasicBlock();
    BinaryBasicBlock* exitBlock = exitBlockUniPtr.get();
    exitBlock->addInstruction(jmp);

    // 把exitBlock放到operation的后面变成fallthrough
    std::vector<std::unique_ptr<BinaryBasicBlock>> NewBlocks;
    NewBlocks.push_back(std::move(exitBlockUniPtr));
    BF->insertBasicBlocks(operations, std::move(NewBlocks));

    // 调整后继块的信息
    operations->removeAllSuccessors();
    operations->addSuccessor(operations);
    operations->addSuccessor(exitBlock);
    exitBlock->addSuccessor(exit);


    // 把for loop 变成single block loop 处理
    handleSingleBlockLoop(operations);
}

void processInnerLoops(BinaryLoop *Loop) {
    // 检查是否有子循环
    if (Loop->begin() == Loop->end()) {
        // 没有子循环，当前循环是最内层循环
        errs() << "Inner loop size: " << Loop->getBlocks().size() << "\n";

        // single block loop
        if (Loop->getBlocks().size() == 1) {
            errs() << "Found single-block inner loop at "
                   << Loop->getHeader()->getName() << "\n";
            handleSingleBlockLoop(Loop->getHeader());
        }

        // for loop, we can convert it to single block loop
        if (Loop->getBlocks().size() == 2) {
            bool hasCall = false;
            for (auto BB : Loop->getBlocks()) {
                for (auto MCInst : *BB) {
                    BinaryContext &BC = BB->getFunction()->getBinaryContext();
                    if (isCallInst(MCInst, BC)) {
                        hasCall = true;
                    }
                }
            }
            
            if (! hasCall) {
                BinaryBasicBlock* conditions = Loop->getHeader();
                BinaryBasicBlock* operations;
                for (auto BB : Loop->getBlocks()) {
                    if(BB != conditions) {
                        operations = BB;
                    }
                }
                if (operations->pred_size() == 1 && operations->succ_size() == 1) {
                    // for loop checked
                    errs() << "detected for loop at " << operations->getName() << "\n";
                    handleForLoop(conditions, operations);
                }
            }
         }
    } else {
        // 如果有子循环，递归处理每个子循环
        for (BinaryLoop *SubLoop : *Loop) {
            processInnerLoops(SubLoop);
        }
    }
}

Error ErqiVec::runOnFunctions(BinaryContext &BC) {
    if (!BC.isX86())
        return Error::success();

    std::unique_ptr<RegAnalysis> RA(
        new RegAnalysis(BC, &BC.getBinaryFunctions(), nullptr));

    simLoopPREGManager = PhisicalRegManager(BC);

    for (auto &pair : BC.getBinaryFunctions()) {
        BinaryFunction &BF = pair.second;  // 获取 BinaryFunction 对象
        if (BF.empty()) {
            continue;
        }
        if (!BF.getNames().empty())
            errs() << "in function: " << BF.getNames().at(0) << "\n";
        // 检查并计算循环信息
        if (!BF.hasLoopInfo()) {
            BF.calculateLoopInfo();  // 如果没有循环信息，先计算
        }

        const BinaryLoopInfo &LoopInfo = BF.getLoopInfo();
        BF.printLoopInfo(errs());

        for (BinaryLoop *loop : LoopInfo) {
            processInnerLoops(loop);
        }
    }

    
    return Error::success();
}


} // end of bolt
} // end of llvm