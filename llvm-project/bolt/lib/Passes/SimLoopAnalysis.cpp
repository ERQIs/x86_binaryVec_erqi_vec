
#include <iostream>
#include <memory>
#include <vector>
#include <map>
#include <tuple>
#include <set>

#include "bolt/Passes/SimLoop.h"
#include "bolt/Passes/SimLoopAnalysis.h"

#include "llvm/Support/raw_ostream.h"

#define R9 120
#define EAX 22
#define R10 121
#define R11 122
#define R12 123
#define TMP 0

#define DEBUGTAG 1

namespace llvm {
namespace bolt {
namespace simLoop{

int flowCnt = 0;

/* 
RegMap records register values. Register valus can be pointers and integer types in simple loop.

from perspective of class design, all register values are expressions.
*/
class RegMap {
public:
    /*
    retrive the exp from table.  
    returns nullptr if not marked (when coming to a new register)
    */
    Exp *find(Reg &r) {
        std::map<int, Exp*>::iterator ret = table.find(r.id);
        return ret == table.end() ? nullptr : ret->second;
    }

    void set(Reg &r, Exp* exp) {
        table[r.id] = exp;
    }

    void dump() {
        errs() << "dumping regMap " << "\n";
        for (const auto& [key, value] : table) {
            errs() << "regID: " << key << ", Value: " << (value ? value->toString() : "nullptr") << "\n";
        }
    }

    void reset() {
        table.clear();
    }
private:
    // marks the value in certain register    reg_id to Exp
    std::map<int, Exp*> table;
};

RegMap regMap;

// and also, the flow coming out from a simple loop have two outlets: register and memory.
// as for the register outlet, we have recored them in regMap.
// and to record the memory outlets, we record every 
//      STORE  srcExp (value to be stored on memory)   dstExp (memory address) 
// pairs.
// accessing this vector inversely can also get a knowledge of what is stored in memory positions now.

//                     ptr   src
std::vector<std::tuple<Exp*, Exp*>> store_history;
std::vector<std::tuple<Exp*, Exp*>> memory;
// BEWARE! this is not the order on IR definition
// address -> value
// it just fits the k-v and address-val

// checks memory and return value in memory. nullptr if not found.

Exp* readMemory (Exp* checkMe) {
    Exp* ret = nullptr;
    for (const auto& [ptr, val] : memory) {
        if (ptr->isEquivalent(*checkMe)) {
            ret = val;
        }
    }
    return ret;
}


// this vector records all the flow we defined.
std::vector<Flow*> defined_flows;


// this thing makes this:
// 1. check up regMap for register r
//      if we have recorded r in regMap, then return the value (exp) recorded
//      else, create a flow to represent what is in r, and assign it to r by writing it to regMap, and returns that value (exp)
Exp* readRegister(Reg* r) {
    Exp* ret;
    ret = regMap.find(*r);

    if (ret == nullptr) {
        Flow* newFlow = new Flow(r);
        defined_flows.push_back(newFlow);

        FlowExp* srcExp = new FlowExp(std::make_shared<Flow>(*newFlow));

        regMap.set(*r, srcExp);

        ret = srcExp;
    }
    return ret;
}


void debug(std::string debugInfo) {
    if (DEBUGTAG) {
        errs() << debugInfo << "\n";
    }
}


std::shared_ptr<Exp> makeSharedExp(const Exp& exp) {
    if (const auto* flowExp = dynamic_cast<const FlowExp*>(&exp)) {
        return std::make_shared<FlowExp>(*flowExp);
    } else if (const auto* immExp = dynamic_cast<const ImmExp*>(&exp)) {
        return std::make_shared<ImmExp>(*immExp);
    } else if (const auto* binExp = dynamic_cast<const BinExp*>(&exp)) {
        return std::make_shared<BinExp>(*binExp);
    }else {
        assert(false && "Unsupported Exp type in makeSharedExp");
        return nullptr; // Should never reach here
    }
}

bool Flow::initIncremental() {
    /*
    first get the after loop value of src, see if it is :
        1. this + imm
        2. imm + this
        3. this - imm
    */

    // get after loop value of src
    Exp* afterLoop;
    if (this->type == FlowSrcType::Memory) {

        /* jsut adding stupid things here */ 
        /* turns out this adding helped a lot */
        Exp* srcExp = this->addr.exp;
        srcExp->initIncremental();
        if (srcExp->is_incremental && srcExp->incrementStep != 0) {
            return false;
        }


        afterLoop = readMemory(this->addr.exp);
        assert(afterLoop != nullptr && "src of memory positioned Flow contains nothing");
    } else if (this->type == FlowSrcType::Register) {
        afterLoop = regMap.find(* (this->addr.r) );
        assert(afterLoop != nullptr && "src of register positioned Flow contains nothing");
    } else {
        assert(false && "unsupported flow type");
    }

    // Lambda to check if an Exp is `this`
    auto isThisExp = [this](Exp* exp) -> bool {
        if (FlowExp* fe = dynamic_cast<FlowExp*>(exp)) {
            return fe->getFlow()->id == this->id;
        }
        return false;
    };

    // Lambda to check if an Exp is an immediate value
    auto isImmExp = [](Exp* exp) -> bool {
        if (auto imm = dynamic_cast<ImmExp*>(exp)) {
            return true;
        }
        return false;
    };

    // Check condition
    if (BinExp* be = dynamic_cast<BinExp*>(afterLoop)) {
        Exp* lhs = be->getSub1().get();
        Exp* rhs = be->getSub2().get();
        if (be->getOpType() == BinExpOpType::Add) {
            if (isThisExp(lhs) && isImmExp(rhs)) {
                // this + imm
                this->step = static_cast<ImmExp*>(rhs)->getValue();
                this->is_incremental = true;
                return true;
            } else if (isImmExp(lhs) && isThisExp(rhs)) {
                // imm + this
                this->step = static_cast<ImmExp*>(lhs)->getValue();
                this->is_incremental = true;
                return true;
            }
        } else if (be->getOpType() == BinExpOpType::Sub) {
            if (isThisExp(lhs) && isImmExp(rhs)) {
                // this - imm
                this->step = -static_cast<ImmExp*>(rhs)->getValue();
                this->is_incremental = true;
                return true;
            }
        }
    } else if (FlowExp* fe = dynamic_cast<FlowExp*>(afterLoop)) {
        // now I decided that fixed flow is increment flow with step = 0
        if (fe->getFlow()->id == this->id) {
            this->step = 0;
            this->is_fixed = true;
            this->is_incremental = true;
            return true;
        }
    } else if (ImmExp* imme = dynamic_cast<ImmExp*>(afterLoop)) {
            this->step = 0;
            this->is_fixed = true;
            this->is_incremental = true;
            return true;
    }
    this->is_incremental = false;
    return false;
}

bool Flow::initElementary() {
    /*
    check if Flow is on memory, and if so, check if address exp is incremental.
    */
    // debug("at flow.initElementary flag 1" + this->name);
    if (this->type != FlowSrcType::Memory){
        return false;
    }

    Exp* src = this->addr.exp;
    
    src->initIncremental();
    // debug("at flow.initElementary flag 2" + this->name);
    if (src->is_incremental && src->incrementStep != 0) {
        this->is_elementary = true;
        // debug("detected elementry set true");
        this->step = src->incrementStep;
        return true;
    }

    return false;
};

bool Flow::initFixed() {
    return false;
};

bool Exp::initIncremental() {
    if (ImmExp* ie = dynamic_cast<ImmExp*>(this)) {
        this->is_incremental = true;
        this->incrementStep = 0;
        return true;
    } else if (FlowExp* fe = dynamic_cast<FlowExp*>(this)) {
        // debug("in exp initIncremental flow exp");
        // debug(fe->getFlow().get()->name);
        // debug(fe->getFlow().get()->is_incremental ? "is inc" : "not inc");

        fe->getFlow().get()->initIncremental();
        if (fe->getFlow().get()->is_incremental) {
            this->is_incremental = true;
            this->incrementStep = fe->getFlow()->step;
            return true;
        }
    } else if (BinExp* be = dynamic_cast<BinExp*>(this)) {
        Exp* left = be->getSub1().get();
        Exp* right = be->getSub2().get();
        left->initIncremental();
        right->initIncremental();
        if (left->is_incremental && right->is_incremental) {
            switch (be->getOpType()) {
                case BinExpOpType::Add:
                    this->is_incremental = true;
                    this->incrementStep = left->incrementStep + right->incrementStep;
                    return true;
                    break;
                case BinExpOpType::Sub:
                    this->is_incremental = true;
                    this->incrementStep = left->incrementStep - right->incrementStep;
                    return true;
                    break;
                case BinExpOpType::Mul:
                    // a Mul Exp is Increment only when:
                    //        fixedValue * incrementValue
                    // and the step of Mul Exp will be  ->  fuedValue * (step of incrementValue)

                    // connsider only increment Exp multiplied with imm.(should be fixed value)
                    // (but we don't not know the value of a unknown fixed value, which we need)
                    // (so we just add a comstraint that this must be a Imm)

                    // incrementExp * imm
                    if (ImmExp* imer = dynamic_cast<ImmExp*>(right)) {
                        this->is_incremental = true;
                        this->incrementStep = imer->getValue() * left->incrementStep;
                        return true;
                    }

                    // imm * incremnetExp
                    if (ImmExp* imel = dynamic_cast<ImmExp*>(left)) {
                        this->is_incremental = true;
                        this->incrementStep = imel->getValue() * right->incrementStep;
                        return true;
                    }
                    break;
                default:
                    assert(false && "unsupported BinExpType for incremental checking");
            }
        }
    }
    this->is_incremental = false;
    return false;
}

bool Exp::isElementary() {
    // 114514 is a number that flow step would possibly not take
    uint64_t expStep = 114514;
    for (auto flow : this->involvedFlows) {
        flow->initIncremental();
        flow->initElementary();
        if (! flow->is_elementary) {
            return false;
        }
        if (expStep == 114514) {
            expStep = flow->step;
        } else if (expStep != flow->step){
            return false;
        }
    }
    if (expStep == 114514) {
        // this means Exp does not contain any flow hence is a fixed value, which can be elementary
        expStep = 0;
    }
    this->width = expStep;
    this->incrementStep = expStep;
    return true;
}



void SimLoopAnalysis(std::vector<std::shared_ptr<SimLoopInst>> &simLoop, SimLoopInfo &info) {
    regMap = RegMap();
    for (const auto& inst : simLoop) {

        // errs() << inst->getDump() << "\n";

        switch (inst->getType()) {
        case InstType::Load: 
        {
            /*
            A load inst loads some exp from memory to dst register

                1. handle pointerExp. (check RegMap for src register, if not, create flow, store it to register by recording in RegMap, finally get pointerExp)
                2. handle value exp ( get *pointerExp by checking memory for pointerExp, if not, create flow, store it on memory ).
                3. move value exp into dst register (record in RegMap).
            */
            if ( LoadInst* li = dynamic_cast<LoadInst*>(inst.get())) {
                
                debug("at load inst: " + li->getDump());
                
                Exp *pointerExp, *valueExp;
                SimLoopOperand* src = li->getSrc1().get();
                if (RegOperand* srcRegOp = dynamic_cast<RegOperand*>(src)) {
                    // this register contains a pointer to load src
                    Reg *srcReg = new Reg(srcRegOp->getId(), srcRegOp->getName());
                    // check RegMap for srcReg
                    pointerExp = readRegister(srcReg);
                } else {
                    // otherwise it is a imm operand that used as a pointer to src.
                    // this rare situation is passed in first implementation.
                    // honestly I donno if this could ever happen.
                    assert(false && "imm src in load inst not implemented");
                }

                debug("got ptrExp: " +  pointerExp->toString());

            
                // 2. handle value Exp
                valueExp = readMemory(pointerExp);

                if (valueExp == nullptr) {
                    // not found in memory
                    Flow* newFlow = new Flow(pointerExp);
                    defined_flows.push_back(newFlow);
                    
                    FlowExp* srcExp = new FlowExp(std::make_shared<Flow>(*newFlow));
                    memory.emplace_back(pointerExp, srcExp);

                    valueExp = srcExp;
                }
                
                debug("got valExp: " + valueExp->toString());

                // 3. assignment
                SimLoopOperand* dst = li->getDst().get();
                if (RegOperand* dstRegOp = dynamic_cast<RegOperand*>(dst)) {
                    Reg *dstReg = new Reg(dstRegOp->getId(), dstRegOp->getName());
                    regMap.set(*dstReg, valueExp);
                } else {
                    assert(false && "failed to extract dst register from load inst");
                }

                debug("assigned");

            } else {
                std::cerr << "Failed to cast to LoadInst" << "\n";
            }
            break;
        }
        case InstType::Store: 
        {
            // 动态转换为 StoreInst 类型

            // A store inst put a expr in register(or imm) to somewhere in memory.

            // 1. handle valueExp (check RegMap for src reg, if not, create flow, assign flowExp to src register)
            // 2. handle ptrExp (check RegMap for dst reg, if not, create flow, assign flowExp to dst register)
            // 3. assign valueExp to memory (insert (ptrExp, valueExp) to memory)
            StoreInst* si = dynamic_cast<StoreInst*>(inst.get());
            if (si) {

                debug("at sotre inst: " + si->getDump());
                // step 1. handle valueExp
                Exp* valueExp;
                
                // check src opperand
                SimLoopOperand* srcOp = si->getSrc1().get();
                if (RegOperand* srcRegOp = dynamic_cast<RegOperand*>(srcOp)) {
                    // src opperand is a register, extract its value
                    Reg* srcReg = new Reg(srcRegOp->getId(), srcRegOp->getName());
                    valueExp = readRegister(srcReg);
                    
                } else if (ImmOperand* immOp = dynamic_cast<ImmOperand*>(srcOp)){
                    // src operand is a imm operand
                    valueExp = new ImmExp(immOp->value, immOp->isBoltExp);
                } else {
                    assert(false && "invalid type of operand at src in store inst");
                }

                debug("got valExp: " + valueExp->toString());

                // step 2. handle ptr Exp
                Exp* ptrExp;

                // check dst opperand
                SimLoopOperand* dstOp = si->getDst().get();
                if (RegOperand* dstRegOp = dynamic_cast<RegOperand*>(dstOp)) {
                    // dst opperand is a register, extract its value
                    Reg* dstReg = new Reg(dstRegOp->getId(), dstRegOp->getName());
                    ptrExp = readRegister(dstReg);
                }  else {
                    assert(false && "invalid type of operand at dst (not register) in store inst");
                }

                debug("got ptrExp: " + ptrExp->toString());

                // 3. assign valueExp to memory
                memory.emplace_back(ptrExp, valueExp);
                store_history.emplace_back(ptrExp, valueExp);

            } else {
                std::cerr << "Failed to cast to StoreInst" << "\n";
            }
            break;
        }
        case InstType::Mov: 
        {
            MovInst* mi = dynamic_cast<MovInst*>(inst.get());
            if (mi) {
                // 使用 OtherInst 的特定方法
                debug("at mov inst");

                Exp* srcExp;
                SimLoopOperand* srcOp = mi->getSrc1().get();
                if (RegOperand* regOp = dynamic_cast<RegOperand*>(srcOp)) {
                    Reg* srcReg = new Reg(regOp->getId(), regOp->getName());
                    srcExp = readRegister(srcReg);
                } else if (ImmOperand* immOp = dynamic_cast<ImmOperand*>(srcOp)) {
                    srcExp = new ImmExp(immOp->value, immOp->isBoltExp);
                } else {
                    assert(false && "invalid operand as src in mov inst, not reg nor imm");
                }
                
                debug("got srcExp " + srcExp->toString());

                SimLoopOperand* dstOp = mi->getDst().get();
                if (RegOperand* regOp = dynamic_cast<RegOperand*>(dstOp)) {
                    Reg* dstReg = new Reg(regOp->getId(), regOp->getName());
                    regMap.set(*dstReg, srcExp);

                    debug("assigned to: " + dstReg->name);
                } else {
                    assert(false && "invalid operand as dst in mov inst, not reg");
                }

                

            } else {
                std::cerr << "Failed to cast to movInst" << "\n";
            }
            break;
        }
        case InstType::Add:
        case InstType::Mul:
        case InstType::Sub:
        {
            // bin insts:
            // check RegMap for src1, if not, createflow, assign flowExp to src1, and get srcExp1
            // check RegMap for src2, if not, createflow, assign flowExp to src2 ,and get srcExp2
            // execute binary operation by creating resultExp (binOp, srcExp1, srcExp2)
            // sotre resultExp to dst by writing RegMap.

            BinaryInst* bi = dynamic_cast<BinaryInst*>(inst.get());
            if (bi) {
                debug("at bin inst: " + bi->getDump());
                // get left operand value
                Exp* leftExp;
                SimLoopOperand* leftOp = bi->getSrc1().get();
                if (RegOperand* regOp = dynamic_cast<RegOperand*>(leftOp)) {
                    Reg* srcReg = new Reg(regOp->getId(), regOp->getName());
                    leftExp = readRegister(srcReg);
                } else if (ImmOperand* immOp = dynamic_cast<ImmOperand*>(leftOp)) {
                    leftExp = new ImmExp(immOp->value, immOp->isBoltExp);
                } else {
                    assert(false && "invalid operand as src1 in bin inst, not reg nor imm");
                }

                debug("got leftExp " + leftExp->toString());

                // get right operand value
                Exp* rightExp;
                SimLoopOperand* rightOp = bi->getSrc2().get();
                if (RegOperand* regOp = dynamic_cast<RegOperand*>(rightOp)) {
                    Reg* srcReg = new Reg(regOp->getId(), regOp->getName());
                    rightExp = readRegister(srcReg);
                } else if (ImmOperand* immOp = dynamic_cast<ImmOperand*>(rightOp)) {
                    rightExp = new ImmExp(immOp->value, immOp->isBoltExp);
                } else {
                    assert(false && "invalid operand as src2 in bin inst, not reg nor imm");
                }

                debug("got rightExp " + rightExp->toString());
                
                // compute result value
                BinExpOpType type;
                switch (bi->getType()) {
                    case InstType::Add:
                        type = BinExpOpType::Add;
                        break;
                    case InstType::Sub:
                        type = BinExpOpType::Sub;
                        break;
                    case InstType::Mul:
                        type = BinExpOpType::Mul;
                        break;
                    default:
                        assert(false && "unsupported binop type");
                        break;
                }
                Exp* resultExp = new BinExp(type,
                                         makeSharedExp(*leftExp),
                                         makeSharedExp(*rightExp)  );

                debug("got resultExp " + resultExp->toString());

                // save result into dst register
                SimLoopOperand* dstOp = bi->getDst().get();
                if (RegOperand* regOp = dynamic_cast<RegOperand*>(dstOp)) {
                    Reg* dstReg = new Reg(regOp->getId(), regOp->getName());
                    regMap.set(*dstReg, resultExp);

                    debug("assigned to " + dstReg->name);
                } else {
                    assert(false && "invalid operand as dst in mov inst, not reg");
                }

            } else {
                std::cerr << "Failed to cast to OtherInst" << "\n";
            }
            break;
        }

        default:
            std::cerr << "Unknown instruction type" << "\n";
            break;
        }
    }

    // now the running of loop is completed.
    // the result, should lie in storeHistory. we should check it out.


    errs() << "\n\n dumping memory " << "\n";
    for (const auto& [exp1, exp2] : memory) {
        // 处理 exp1 和 exp2
        errs() << "ptr: " << exp1->toString() << "\n";
        errs() << "val: " << exp2->toString() << "\n";
    }

    errs() << "\n\n dumping defined flows" << "\n";
    for(const auto& flow : defined_flows) {
        errs() << flow->name << " got address " 
        << ((flow->type == FlowSrcType::Register)  ? flow->addr.r->name + "(id: " + std::to_string(flow->addr.r->id) + ")"  : flow->addr.exp->toString())
        << "\n";
        // errs() << std::to_string(isFlowIterative(flow))<< "\n";

    }

    regMap.dump();

    // now that the looping is over, we should extract loop info

    for (auto flow : defined_flows) {
        flow->initIncremental();
    }

    for (auto flow : defined_flows) {
        flow->initElementary();
    }


    errs() << "\n\ndumping flow analysis" << "\n";
    for(const auto& flow : defined_flows) {
        errs() << flow->name << " got address " 
        << ((flow->type == FlowSrcType::Register)  ? flow->addr.r->name + "(id: " + std::to_string(flow->addr.r->id) + ")"  : flow->addr.exp->toString())
        << "\n";

        errs() << "is incremtal: " << (flow->is_incremental == true ? 1 : 0) << "\n"; 
        errs() << "is elementary: " << (flow->is_elementary ? 1 : 0) << "\n"; 
        errs() << "step " << static_cast<int>(flow->step) << "\n";
        
        // errs() << std::to_string(isFlowIterative(flow))<< "\n";
    }


    errs() << "\n\n dumping store history " << "\n";
    for (const auto& [exp1, exp2] : store_history) {
        // 处理 exp1 和 exp2
        exp1->initIncremental();
        exp2->initIncremental();
        errs() << "ptr: " << exp1->toString() << (exp1->is_incremental ? "  inc" : "") <<
                     (exp1->isElementary() ? "    elem" : "")  << "   " << exp1->incrementStep << "\n";

        exp2->isElementary();
        // we assign a fake flow to represent output array

        errs() << "val: " << exp2->toString() << (exp2->is_incremental ? "  inc" : "") <<
                     (exp2->isElementary() ? "    elem" : "")  << "   " << exp2->incrementStep << "\n";

    }

    for (const auto& [ptr, val] : store_history) {
        // 处理 exp1 和 exp2
        // 先不管init Loop了
        // init loop 里面有一个  incremental到不变量的赋值
        if (!ptr->is_incremental || !val->isElementary()) {
            continue;
        } 
        Flow* outputSymbol = new Flow(ptr);
        outputSymbol->step = ptr->incrementStep;
        info.bases.push_back(outputSymbol);

        for (auto flow : val->involvedFlows) {
            // flows in inputBases and outputBases are no longer flows but a representation of arrays
            // this might be confusing but I don't want to make a abstraction of arrays and handle with that.
            info.bases.push_back(flow);
        }
        
        info.loopLogic.emplace(outputSymbol, val);
    }

    Flow* indicator = nullptr;
    for (const auto& flow : defined_flows) {
        if (flow->is_incremental &&  (flow->step == 1 || flow->step == -1)) {
            if (indicator != nullptr) {
                debug("more then one step 1 increment flow");
                info.canSimplify = false;
            }
            indicator = flow;
        }
    }
    info.indicator = indicator;
    if (indicator != nullptr)
        debug("indicator : " + indicator->name);
}



//********************************************************************************** */
//*********************************************************************************** */

// now these are for emitting code from LoopInfo


SimpleLoopRegManager emittingManager;
 
// flow_id to RegOperand, only register input bases and output bases
std::map<uint64_t, std::shared_ptr<RegOperand>> flowToRegTable;



// these sets contains flow ids    the point is that I didn't use unique ptr, so I can only Identify flow by ids.
// std::set<uint64_t> inputBaseIDs;   // they are all elem flows
// std::set<uint64_t> outputBaseIDs;   // they are all increment flows



void generateAsmFromExp (Exp* exp, std::vector<std::shared_ptr<SimLoopInst>>& targetBlock, std::shared_ptr<RegOperand> dst);


void generateAsmFromFlow (Flow* flow, std::vector<std::shared_ptr<SimLoopInst>>& targetBlock, std::shared_ptr<RegOperand> dst, bool getAddr = false) {
    if (flow->type == FlowSrcType::Register) {
        std::shared_ptr<RegOperand> src = std::make_shared<RegOperand>(flow->addr.r->id, flow->addr.r->name, 8); 
        std::shared_ptr<SimLoopInst> mov = std::make_shared<MovInst>(src, dst);
        targetBlock.push_back(mov);
        debug("made mov inst " + mov->getDump());
    } else if (flow->type == FlowSrcType::Memory) {
        if(getAddr) {
            generateAsmFromExp(flow->addr.exp, targetBlock, dst);
            return;
        }

        // get src
        std::shared_ptr<RegOperand> src = emittingManager.getTmpGPR(8);
        generateAsmFromExp(flow->addr.exp, targetBlock, src);


        // emit load inst to targetBlock
        std::shared_ptr<LoadInst> li = std::make_shared<LoadInst>(src, dst);
        targetBlock.push_back(li);
        emittingManager.releaseTmpGPR(src);
        debug("made load inst " + li->getDump());
    } else {
        assert(false && "should never be here");
    }
}

void generateAsmFromExp (Exp* exp, std::vector<std::shared_ptr<SimLoopInst>>& targetBlock, std::shared_ptr<RegOperand> dst) {
    if (FlowExp* fe = dynamic_cast<FlowExp*>(exp)) {
        generateAsmFromFlow(fe->getFlow().get(), targetBlock, dst);
        
    } else if (BinExp* be = dynamic_cast<BinExp*>(exp)) {

        // Lambda to check if an Exp is an immediate value
        auto isImmExp = [](Exp* exp) -> bool {
            if (auto imm = dynamic_cast<ImmExp*>(exp)) {
                return true;
            }
            return false;
        };

        Exp* left = be->getSub1().get();
        Exp* right = be->getSub2().get();
        
        // see if we ever allocate a tmp register, if so, we have to release it
        std::shared_ptr<RegOperand> tmp = nullptr;
        

        // init left, right operand
        std::shared_ptr<SimLoopOperand> leftOpr, rightOpr;

        if (ImmExp *immel = dynamic_cast<ImmExp*>(left)) {
            leftOpr = std::make_shared<ImmOperand>(immel->getValue(), immel->isBoltExp);
            debug("made imm leftOpr " + leftOpr->getName());
            if (ImmExp *immer = dynamic_cast<ImmExp*>(right)) {
                rightOpr = std::make_shared<ImmOperand>(immer->getValue(), immer->isBoltExp);
                debug("made imm rightOpr " + rightOpr->getName());
            } else {
                generateAsmFromExp(right, targetBlock, dst);
                rightOpr = dst;
            }
        } else if (ImmExp *immer = dynamic_cast<ImmExp*>(right)) {
            generateAsmFromExp(left, targetBlock, dst);
            leftOpr = dst;
            rightOpr = std::make_shared<ImmOperand>(immer->getValue(), immer->isBoltExp);
        } else {
            generateAsmFromExp(left, targetBlock, dst);
            leftOpr = dst;

            tmp = emittingManager.getTmpGPR(8);
            generateAsmFromExp(right, targetBlock, tmp);
            rightOpr = tmp;
        }

        
        // get inst type
        InstType type;
        switch (be->getOpType())
        {
        case BinExpOpType::Add:
            type = InstType::Add;
            break;
        case BinExpOpType::Mul:
            type = InstType::Mul;
            break;
        case BinExpOpType::Sub:
            type = InstType::Sub;
        default:
            assert(false && "should never be here");
            break;
        }

        // get dst reg
        // std::shared_ptr<SimLoopOperand> dst;
        // if (isOutMost) {
        //     dst = emittingManager.getGPR(8);
        // } else {
        //     dst = emittingManager.getTmpGPR(8);
        // }

        // ADD  0  regX   regX
        if (type == InstType::Add ) {
            if (ImmOperand *imo = dynamic_cast<ImmOperand*>(leftOpr.get())) {
                if (imo->value == 0) {
                    if (RegOperand* ro = dynamic_cast<RegOperand*>(rightOpr.get())) {
                        if (ro->getId() == dst.get()->getId()) {
                            return;
                        }
                    }
                }
            }
        }

        // ADD  regX   0   regX
        if (type == InstType::Add ) {
            if (ImmOperand *imo = dynamic_cast<ImmOperand*>(rightOpr.get())) {
                if (imo->value == 0) {
                    if (RegOperand* ro = dynamic_cast<RegOperand*>(leftOpr.get())) {
                        if (ro->getId() == dst.get()->getId()) {
                            return;
                        }
                    }
                }
            }
        }

        std::shared_ptr<BinaryInst> bi = std::make_shared<BinaryInst>(type, leftOpr, rightOpr, dst);
        targetBlock.push_back(bi);
        if (tmp != nullptr) {
            emittingManager.releaseTmpGPR(tmp);
        }
        debug("made binary inst" + bi->getDump());
    } else {
        assert(false && "should never be here");
    }
}

void initializeBasesInHeader (std::vector<std::shared_ptr<SimLoopInst>>& loopHeader, SimLoopInfo &info) {
    // all the bases are elementry here. since they are all referenced as array elements
    for (auto flow : info.bases) {
        // we didn't use unique pointer, so this testing is neccessary, because there are repeated flows
        if (flowToRegTable.find(flow->id) == flowToRegTable.end()) {
            FlowExp* flowSrcExp = dynamic_cast<FlowExp*>(flow->addr.exp);
            if (flowSrcExp && flowSrcExp->getFlow()->type == FlowSrcType::Register) {
                Flow* src = flowSrcExp->getFlow().get();
                flowToRegTable.emplace(flow->id,
                                       std::make_shared<RegOperand>(src->addr.r->id,
                                                                    src->addr.r->name,
                                                                    8,
                                                                    false)
                                       );
            } else {
                auto reg = emittingManager.getGPR(8);
                generateAsmFromFlow(flow, loopHeader, reg, true);
                flowToRegTable.emplace(flow->id, reg);
            }
        }
    }

    // load indicator
    if (info.indicator->type == FlowSrcType::Register) {
        info.indicatorReg = std::make_shared<RegOperand>(info.indicator->addr.r->id,
                                                         info.indicator->addr.r->name,
                                                         8,
                                                        false);
    } else {
        info.indicatorReg = emittingManager.getGPR(8);
        generateAsmFromFlow(info.indicator, loopHeader, info.indicatorReg, false);
    }
    flowToRegTable.emplace(info.indicator->id, info.indicatorReg);
    debug("indicatorReg: " + info.indicatorReg->getName());
}

// some elementry flow (array elements) will be used more then once in one loop interation
// I don't want to load then again.
std::map<uint64_t, std::shared_ptr<RegOperand>> loadedInLoop;

void generateVecFromExp (Exp* exp, std::vector<std::shared_ptr<SimLoopInst>>& targetBlock, std::shared_ptr<RegOperand> dst, int vectorWidth, int elementWidth) {
    if (FlowExp* fe = dynamic_cast<FlowExp*>(exp)) {
        auto inputArray_id = fe->getFlow()->id;
        if (flowToRegTable.find(inputArray_id) == flowToRegTable.end()) {
            assert(false && "uninitialized input array base");
            return;
        }
        auto inputPtrReg = flowToRegTable.find(inputArray_id)->second;
        std::shared_ptr<VecInst> vecLoad = std::make_shared<VecInst>(InstType::Load, elementWidth, vectorWidth,
                                                                      inputPtrReg, dst);
        targetBlock.push_back(vecLoad);

    } else if (ImmExp* imme = dynamic_cast<ImmExp*>(exp)) {
        auto src = std::make_shared<ImmOperand>(imme->getValue(), imme->isBoltExp);
        std::shared_ptr<VecInst> vecMov = std::make_shared<VecInst>(InstType::Mov, elementWidth, vectorWidth,
                                                                      src, dst);
        targetBlock.push_back(vecMov);
    } else if (BinExp* be = dynamic_cast<BinExp*>(exp)) {


        Exp* left = be->getSub1().get();
        Exp* right = be->getSub2().get();
        
        // see if we ever allocate a tmp register, if so, we have to release it
        std::shared_ptr<RegOperand> tmp = nullptr;
        

        // init left, right operand
        std::shared_ptr<SimLoopOperand> leftOpr, rightOpr;
        

        // this flow exp must be an elementry flow and have the address has already been initialized in header
        if (FlowExp *fel = dynamic_cast<FlowExp*>(left)) {

            int flow_id = fel->getFlow()->id;
            if (flowToRegTable.find(flow_id) == flowToRegTable.end()) {
                assert(false && "found not initializd input array base");
            } else if (loadedInLoop.find(flow_id) != loadedInLoop.end()){
                // we have loaded this array element before
                auto regOpr = loadedInLoop.find(flow_id)->second;
                leftOpr = regOpr;
                // debug
                if (leftOpr.get() == nullptr) {
                    debug("break here");
                }
            }   else {
                // we haven't loaded this array element before
                // we assign this vector register to this flow. this contains this only
                auto vReg = emittingManager.getVecReg(vectorWidth);
                generateVecFromExp(left, targetBlock, vReg, vectorWidth, elementWidth);
                leftOpr = vReg;
                loadedInLoop.emplace(flow_id, vReg);
                // debug
                if (leftOpr.get() == nullptr) {
                    debug("break here");
                }
            }

        } else {
            // not a flow exp
            generateVecFromExp(left, targetBlock, dst, vectorWidth, elementWidth);
            leftOpr = dst;
            // debug
            if (leftOpr.get() == nullptr) {
                debug("break here");
            }
        }

        if (FlowExp *fer = dynamic_cast<FlowExp*>(right)) {

            uint64_t flow_id = fer->getFlow()->id;
            if (flowToRegTable.find(flow_id) == flowToRegTable.end()) {
                assert(false && "found not initializd input array base");
            } else if (loadedInLoop.find(flow_id) != loadedInLoop.end()){
                // we have loaded this array element before
                auto regOpr = loadedInLoop.find(flow_id)->second;
                rightOpr = regOpr;
            }   else {
                // we haven't loaded this array element before
                // we assign this vector register to this flow. this contains this only
                auto vReg = emittingManager.getVecReg(vectorWidth);
                generateVecFromExp(right, targetBlock, vReg, vectorWidth, elementWidth);
                rightOpr = vReg;
                loadedInLoop.emplace(flow_id, vReg);
            }

        } else {
            // not a flow exp
            if (leftOpr == dst) {
                tmp = emittingManager.getVecReg(vectorWidth);
                generateVecFromExp(right, targetBlock, tmp, vectorWidth, elementWidth);
                rightOpr = tmp;

            } else {
                generateVecFromExp(right, targetBlock, dst, vectorWidth, elementWidth);
                rightOpr = dst;
            }
            
        }

        
        // get inst type
        InstType type;
        switch (be->getOpType())
        {
        case BinExpOpType::Add:
            type = InstType::Add;
            break;
        case BinExpOpType::Mul:
            type = InstType::Mul;
            break;
        case BinExpOpType::Sub:
            type = InstType::Sub;
            break;
        default:
            assert(false && "should never be here");
            break;
        }

        // get dst reg
        // std::shared_ptr<SimLoopOperand> dst;
        // if (isOutMost) {
        //     dst = emittingManager.getGPR(8);
        // } else {
        //     dst = emittingManager.getTmpGPR(8);
        // }

        // ADD  0  regX   regX
        if (type == InstType::Add ) {
            if (ImmOperand *imo = dynamic_cast<ImmOperand*>(leftOpr.get())) {
                if (imo->value == 0) {
                    if (RegOperand* ro = dynamic_cast<RegOperand*>(rightOpr.get())) {
                        if (ro->getId() == dst.get()->getId()) {
                            return;
                        }
                    }
                }
            }
        }

        // ADD  regX   0   regX
        if (type == InstType::Add ) {
            if (ImmOperand *imo = dynamic_cast<ImmOperand*>(rightOpr.get())) {
                if (imo->value == 0) {
                    if (RegOperand* ro = dynamic_cast<RegOperand*>(leftOpr.get())) {
                        if (ro->getId() == dst.get()->getId()) {
                            return;
                        }
                    }
                }
            }
        }

        std::shared_ptr<VecInst> bi = std::make_shared<VecInst>(type,elementWidth, vectorWidth, leftOpr, rightOpr, dst);
        targetBlock.push_back(bi);
        if (tmp != nullptr) {
            emittingManager.releaseVecReg(tmp);
        }
        debug("made vec inst" + bi->getDump());
    } else {
        assert(false && "should never be here");
    }
}

void generateLoopBody (std::vector<std::shared_ptr<SimLoopInst>>& LoopBody, SimLoopInfo &info, int vectorWidth) {
    for (const auto& pair : info.loopLogic) {
        Flow* output = pair.first;    // 获取键
        Exp* srcTree = pair.second;  // 获取值

        int elemWidth = output->step;

        if (flowToRegTable.find(output->id) == flowToRegTable.end()) {
            assert(false && "uninitialized output array base");
            return;
        }
        auto outputPtrReg = flowToRegTable.find(output->id)->second;
        auto tmp = emittingManager.getVecReg(vectorWidth);
        generateVecFromExp(srcTree, LoopBody, tmp, vectorWidth, elemWidth);
        std::shared_ptr<VecInst> vecStore = std::make_shared<VecInst>(InstType::Store, elemWidth, vectorWidth,
                                                                      tmp, outputPtrReg);
        LoopBody.push_back(vecStore);
        emittingManager.releaseVecReg(tmp);
    }

    // 更新步长
    int paralFac;
    std::set<uint64_t> steppedBase;
    for (const auto& flow : info.bases) {
        uint64_t flow_id = flow->id;
        if (steppedBase.find(flow_id) != steppedBase.end()) {
            continue;
        }
        steppedBase.emplace(flow_id);
        std::shared_ptr<RegOperand> flowReg;
        if (flowToRegTable.find(flow_id) != flowToRegTable.end()) {
            flowReg = flowToRegTable.find(flow_id)->second;
        } else {
            assert(false && "array based flow not initialized");
        }
        
        std::shared_ptr<BinaryInst>  addInst;
        addInst = std::make_shared<BinaryInst> (InstType::Add,
                                                std::make_shared<ImmOperand>(vectorWidth),
                                                flowReg,
                                                flowReg);
        LoopBody.push_back(addInst);
        paralFac = vectorWidth / flow->step;
    }

    // 更新indicator
    info.indicator->step *= paralFac;

    std::shared_ptr<BinaryInst>  addInst;
    addInst = std::make_shared<BinaryInst> (InstType::Add,
                                            std::make_shared<ImmOperand>(info.indicator->step),
                                            info.indicatorReg,
                                            info.indicatorReg);
    LoopBody.push_back(addInst);
}

void resetAlgorithm (){
    flowCnt = 0;
    regMap.reset();
    store_history.clear();
    memory.clear();
    defined_flows.clear();

    emittingManager = SimpleLoopRegManager();
    flowToRegTable.clear();
    loadedInLoop.clear();
}

}  // end of namespace simLoop
}  // end of namespace bolt
}  // end of namespace llvm