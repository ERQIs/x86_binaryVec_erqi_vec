#ifndef SIMLOOP_ANALYSIS_H
#define SIMLOOP_ANALYSIS_H

#include <iostream>
#include <memory>
#include <vector>
#include <map>
#include <tuple>
#include <set>
#include "bolt/Passes/SimLoop.h"

namespace llvm {
namespace bolt {
namespace simLoop {

class Reg {
public:
    int id;
    std::string name;
    Reg(int id, std::string name) : id(id), name(name) {}
};

class Exp;

extern int flowCnt;

enum class FlowSrcType {
    Register,
    Memory
};

/*

a Flow flows into the loop either from inside a register or from some
memory address.
and what is computed within a simple loop flows out from this loop either by a 
register or by memory.

thus a flow, or flows, is the input and output of simple loop. On level of both 
single iteration and full thorough loop.

a simple loop accepts flows, tangle them, convert them and finally let them out.

Flows --( simple Loop )--> converted Flows

*/
class Flow {
public:

    FlowSrcType type;

    /*

    these are to see if a Flow is incremental, elementary or fixed
    but they are only meaningful when initIncremental, initElementary, initFixed
    are called on every flows:

    for flow in ALL_FLOWS:
        flow.initIterative();
        flow.initElementary();
        flow.initFixed();
    */
    bool is_incremental, is_elementary, is_fixed;
    uint64_t step;

    union srcAddr{
        Exp* exp;
        Reg* r;
    } addr;

    std::string name;

    int id;

    std::string toString() {return name;}

    Flow(Reg* reg) {
        name = "flow" + std::to_string(flowCnt);
        id = flowCnt ++;
        type = FlowSrcType::Register;
        addr.r = reg;
        is_incremental = false;
        is_elementary = false;
        is_fixed = false;
    }

    Flow(Exp* exp) {
        name = "flow" + std::to_string(flowCnt);
        id = flowCnt ++;
        type = FlowSrcType::Memory;
        addr.exp = exp;
        is_incremental = false;
        is_elementary = false;
        is_fixed = false;
    }

    // initializes bool variables and step. returns true if Flow is incremental. Only call after looping is done.
    bool initIncremental();
    // only call after initIterative is called on all Flows. return true if Flow is Elementary. initialize step and is_iter
    bool initElementary();
    // you know what I'm going to say.
    bool initFixed();
};

enum class BinExpOpType {
    Add,
    Sub,
    Mul
};

class Exp;
class Flow;


/*

An expression, is what comes out from tangled or converted flows.

the conversions include  +, -, *.

Also, since Flow is origin of all data in simple loop, and expression, as the representation of
any conversions of any flow combination, can represent all data in a simple loop.

so pointers, integers are all expressions in simple loop.
*/
class Exp {
public:

    std::vector<Flow*> involvedFlows;

    // initialized by getIncrementStep()
    uint64_t incrementStep;
    uint64_t width;

    // this can only be called after called initIncremental() on all Flows.
    bool initIncremental();
    bool is_incremental;

    // this can only be called after called initElementary() on all Flows.
    bool isElementary();

    // // get the step of incremental exp. make sure only checked after 
    // int getStep();

    virtual ~Exp() = default;

    virtual std::string toString() const = 0;

    virtual bool isEquivalent(const Exp& other) const = 0;

    // returns a new Exp that is unified in form, which is to say  a+b+c  b+c+a  c+b+a  will be all a+b+c (hopefully)
    virtual std::shared_ptr<Exp> simplify() const = 0;
};

// expression constructed form two expressions.
class BinExp : public Exp {
private:
    BinExpOpType opType;             // 操作类型
    std::shared_ptr<Exp> left;       // 左操作数
    std::shared_ptr<Exp> right;       // 右操作数

public:
    // 构造函数
    BinExp(BinExpOpType opType, std::shared_ptr<Exp> sub1, std::shared_ptr<Exp> sub2)
        : opType(opType), left(std::move(sub1)), right(std::move(sub2)) {
            std::vector<Flow*> *leftVec = &left.get()->involvedFlows;
            std::vector<Flow*> *rightVec = &right.get()->involvedFlows;
            involvedFlows.reserve(leftVec->size() + rightVec->size());
            involvedFlows.insert(involvedFlows.end(), leftVec->begin(), leftVec->end());
            involvedFlows.insert(involvedFlows.end(), rightVec->begin(), rightVec->end());
        }

    // 获取操作类型
    BinExpOpType getOpType() const { return opType; }

    // 获取左操作数
    const std::shared_ptr<Exp>& getSub1() const { return left; }

    // 获取右操作数
    const std::shared_ptr<Exp>& getSub2() const { return right; }

    bool isEquivalent(const Exp& other) const override {
        // const auto* otherBin = dynamic_cast<const BinExp*>(&other);
        // if (!otherBin || opType != otherBin->opType) return false;

        // // 考虑交换律
        // if (opType == BinExpOpType::Add || opType == BinExpOpType::Mul) {
        //     return (left->isEquivalent(*otherBin->left) && right->isEquivalent(*otherBin->right)) ||
        //            (left->isEquivalent(*otherBin->right) && right->isEquivalent(*otherBin->left));
        // } else {
        //     return left->isEquivalent(*otherBin->left) && right->isEquivalent(*otherBin->right);
        // }
        return this->simplify()->toString() == other.simplify()->toString();

        // this method is actually buggy when mixing many + and -.
        // but it is enough to handle to check ab + c == c + ba.
        // it should be enough in simple loops.
    }

    std::shared_ptr<Exp> simplify() const override {
        auto simplifiedLeft = left->simplify();
        auto simplifiedRight = right->simplify();

        if (opType == BinExpOpType::Add || opType == BinExpOpType::Mul) {
            // 交换律：对操作数排序
            if (simplifiedLeft->toString() > simplifiedRight->toString()) {
                std::swap(simplifiedLeft, simplifiedRight);
            }
        }

        if (opType == BinExpOpType::Mul) {
            // 分配律：尝试展开乘法
            auto leftBin = dynamic_cast<BinExp*>(simplifiedLeft.get());
            if (leftBin && leftBin->opType == BinExpOpType::Add) {
                return std::make_shared<BinExp>(
                    BinExpOpType::Add,
                    std::make_shared<BinExp>(BinExpOpType::Mul, leftBin->left, simplifiedRight)->simplify(),
                    std::make_shared<BinExp>(BinExpOpType::Mul, leftBin->right, simplifiedRight)->simplify());
            }
            if (leftBin && leftBin->opType == BinExpOpType::Sub) {
                return std::make_shared<BinExp>(
                    BinExpOpType::Sub,
                    std::make_shared<BinExp>(BinExpOpType::Mul, leftBin->left, simplifiedRight)->simplify(),
                    std::make_shared<BinExp>(BinExpOpType::Mul, leftBin->right, simplifiedRight)->simplify());
            }
        }

        return std::make_shared<BinExp>(opType, simplifiedLeft, simplifiedRight);
    }

    // 返回字符串表示
    std::string toString() const override {
        static const std::map<BinExpOpType, std::string> opStrings = {
            {BinExpOpType::Add, "+"},
            {BinExpOpType::Sub, "-"},
            {BinExpOpType::Mul, "*"}
        };
        return "(" + left->toString() + " " + opStrings.at(opType) + " " + right->toString() + ")";
    }
};

// a single Flow can also be an expression, which is the smallest. And that is an FlowExp.
class FlowExp : public Exp {
private:
    std::shared_ptr<Flow> f; // 变量

public:
    // 构造函数
    explicit FlowExp(std::shared_ptr<Flow> flow) : f(std::move(flow)) {
        involvedFlows.push_back(f.get());
    }

    // 获取变量
    const std::shared_ptr<Flow>& getFlow() const { return f; }

    // 返回字符串表示
    std::string toString() const override {
        return f->toString();
    }

    bool isEquivalent(const Exp& other) const override {
        const auto* otherUni = dynamic_cast<const FlowExp*>(&other);
        return otherUni && f->id == otherUni->f->id;
    }

    std::shared_ptr<Exp> simplify() const override {
        return std::make_shared<FlowExp>(*this);
    }
};


class ImmExp : public Exp {
private:
    uint64_t value; // 立即数

public:
    bool isBoltExp;
    bool isLabel;
    // 构造函数
    explicit ImmExp(uint64_t val, bool isBoltExp) : value(val), isBoltExp(isBoltExp) {
        this->isLabel = false;
    }

    // 获取立即数的值
    uint64_t getValue() const { return value; }

    // 返回字符串表示
    std::string toString() const override {
        return std::to_string(static_cast<int>(this->value));
    }

    // 判断是否等价
    bool isEquivalent(const Exp& other) const override {
        const auto* otherImm = dynamic_cast<const ImmExp*>(&other);
        return otherImm && value == otherImm->value;
    }

    // 简化表达式（立即数本身不能再简化）
    std::shared_ptr<Exp> simplify() const override {
        return std::make_shared<ImmExp>(*this);
    }
};

class SimLoopInfo {
public:
    bool canSimplify;
    std::vector<Flow*> bases;

    // keys are output array.  exps are elementary, with input arrays as involved flows. these are the main loopLogic.
    std::map<Flow*, Exp*> loopLogic;

    Flow* indicator;
    std::shared_ptr<RegOperand> indicatorReg;
};


class SimpleLoopRegManager {
private:
    int GPR_id;
    int GVECR_id;
    std::vector<int> tempRegs = {0, 1, 2, 3};
    std::vector<int> vecRegs = {7, 6, 5, 4, 3, 2 ,1, 0};
public:
    SimpleLoopRegManager() {GPR_id = 4; GVECR_id = 1;}
    std::shared_ptr<RegOperand> getGPR (int width) {
        std::shared_ptr<RegOperand> ret =  std::make_shared<RegOperand>(
                    GPR_id, 
                    "GPR" + std::to_string(GPR_id) + "_w" + std::to_string(width), 
                    width,
                    true); 
        GPR_id ++;
        return ret;
    }

    std::shared_ptr<RegOperand> getTmpGPR (int width) {
        assert(!tempRegs.empty() && "tmp regs not enough");
        int id = tempRegs.back();
        tempRegs.pop_back();
        return std::make_shared<RegOperand>(id, "TmpGPR" + std::to_string(id) + "_w" + std::to_string(width), width, true);
    }

    void releaseTmpGPR (std::shared_ptr<RegOperand> r) {
        tempRegs.push_back(r->getId());
    }

    std::shared_ptr<RegOperand> getVecReg (int width) {
        assert(!vecRegs.empty() && "vec regs not enough");
        int id = vecRegs.back();
        vecRegs.pop_back();
        return std::make_shared<RegOperand>(id, "vReg" + std::to_string(id) + "_w" + std::to_string(width), width, true);
    }

    void releaseVecReg (std::shared_ptr<RegOperand> r) {
        vecRegs.push_back(r->getId());
    }

};


void SimLoopAnalysis(std::vector<std::shared_ptr<SimLoopInst>>& simLoop, SimLoopInfo &info);

void initializeBasesInHeader (std::vector<std::shared_ptr<SimLoopInst>>& loopHeader, SimLoopInfo &info);

void generateLoopBody (std::vector<std::shared_ptr<SimLoopInst>>& LoopBody, SimLoopInfo &info, int vectorWidth);

void resetAlgorithm ();
} // end of namespace simLoop
} // end of namespace bolt
} // end of namespace llvm


#endif // SIMLOOP_H