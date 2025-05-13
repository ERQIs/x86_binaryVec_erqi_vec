#ifndef SIMLOOP_H
#define SIMLOOP_H

#include <iostream>
#include <memory>
#include <vector>
#include <map>
#include <string>
#include <cassert>

namespace llvm {
namespace bolt {
namespace simLoop {

// 操作数类型
enum class OprType { Uninit, Register, Immediate };

// 操作数基类
class SimLoopOperand {
public:
    virtual ~SimLoopOperand() = default;

    // 获取操作数名称
    virtual std::string getName() const = 0;

    // 获取操作数类型
    virtual OprType getType() const = 0;

    // 获取操作数 ID，默认实现返回 0
    virtual uint64_t getId() const { return 0; }

    // 获取操作数宽度，默认实现返回 0
    virtual uint64_t getWidth() const { return 0; }
};

// 寄存器操作数
class RegOperand : public SimLoopOperand {
public:
    uint64_t id;
    uint64_t width;
    std::string name;

    // a symbolic register is not a real physical register
    // when transforming sinLoopIR symbolic register should be assigned to a physical register.
    bool symbolic;

    RegOperand(uint64_t id, const std::string &name, uint64_t width, bool symbolic=false)
        : id(id), width(width), name(name), symbolic(symbolic) {}

    std::string getName() const override { return name; }
    OprType getType() const override { return OprType::Register; }
    uint64_t getId() const override { return id; }
    uint64_t getWidth() const override { return width; }
};

// 立即数操作数
class ImmOperand : public SimLoopOperand {
public:
    uint64_t value;
    bool isBoltExp;

    explicit ImmOperand(uint64_t value, bool isBoltExp = false) : value(value), isBoltExp(isBoltExp) {}

    std::string getName() const override { return std::to_string(static_cast<int>(value)); }
    OprType getType() const override { return OprType::Immediate; }
};

// 指令类型
enum class InstType { Uninit, Add, Sub, Mul, Load, Mov, Store };

// 指令基类
class SimLoopInst {
public:
    virtual ~SimLoopInst() = default;

    // 获取指令的文本表示
    virtual std::string getDump() const = 0;

    // 获取指令类型
    virtual InstType getType() const = 0;

    // 获取指令的源操作数 1
    virtual std::shared_ptr<SimLoopOperand> getSrc1() const { return nullptr; }

    // 获取指令的源操作数 2
    virtual std::shared_ptr<SimLoopOperand> getSrc2() const { return nullptr; }

    // 获取指令的目标操作数
    virtual std::shared_ptr<SimLoopOperand> getDst() const { return nullptr; }
};

// 通用二元指令
class BinaryInst : public SimLoopInst {
protected:
    std::shared_ptr<SimLoopOperand> src1;
    std::shared_ptr<SimLoopOperand> src2;
    std::shared_ptr<SimLoopOperand> dst;
    InstType instType;

public:
    BinaryInst(InstType type,
               std::shared_ptr<SimLoopOperand> src1,
               std::shared_ptr<SimLoopOperand> src2,
               std::shared_ptr<SimLoopOperand> dst)
        : instType(type), src1(std::move(src1)), src2(std::move(src2)), dst(std::move(dst)) {
        assert(this->dst->getType() == OprType::Register && "Destination must be a register.");
    }

    InstType getType() const override { return instType; }

    std::string getDump() const override {
        static const std::map<InstType, std::string> instNames{
            {InstType::Add, "ADD"}, {InstType::Sub, "SUB"}, {InstType::Mul, "MUL"}};
        return instNames.at(instType) + "\t" + src1->getName() + "\t" + src2->getName() + "\t" + dst->getName();
    }

    std::shared_ptr<SimLoopOperand> getSrc1() const override { return src1; }
    std::shared_ptr<SimLoopOperand> getSrc2() const override { return src2; }
    std::shared_ptr<SimLoopOperand> getDst() const override { return dst; }
};

// Load 指令
class LoadInst : public SimLoopInst {
protected:
    std::shared_ptr<SimLoopOperand> src;
    std::shared_ptr<SimLoopOperand> dst;

public:
    LoadInst(std::shared_ptr<SimLoopOperand> src, std::shared_ptr<SimLoopOperand> dst)
        : src(std::move(src)), dst(std::move(dst)) {
        assert(this->dst->getType() == OprType::Register && "Destination must be a register.");
    }

    InstType getType() const override { return InstType::Load; }

    std::string getDump() const override {
        return "LOAD\t" + src->getName() + "\t" + dst->getName();
    }

    std::shared_ptr<SimLoopOperand> getSrc1() const override { return src; }
    std::shared_ptr<SimLoopOperand> getDst() const override { return dst; }
};

// Store 指令
class StoreInst : public SimLoopInst {
protected:
    std::shared_ptr<SimLoopOperand> src;
    std::shared_ptr<SimLoopOperand> dst;

public:
    StoreInst(std::shared_ptr<SimLoopOperand> src, std::shared_ptr<SimLoopOperand> dst)
        : src(std::move(src)), dst(std::move(dst)) {}

    InstType getType() const override { return InstType::Store; }

    std::string getDump() const override {
        return "STORE\t" + src->getName() + "\t" + dst->getName();
    }

    std::shared_ptr<SimLoopOperand> getSrc1() const override { return src; }
    std::shared_ptr<SimLoopOperand> getDst() const override { return dst; }
};



// Mov 指令
class MovInst : public SimLoopInst {
protected:
    std::shared_ptr<SimLoopOperand> src;
    std::shared_ptr<SimLoopOperand> dst;

public:
    MovInst(std::shared_ptr<SimLoopOperand> src, std::shared_ptr<SimLoopOperand> dst)
        : src(std::move(src)), dst(std::move(dst)) {
        assert(this->dst->getType() == OprType::Register && "Destination must be a register.");
    }

    InstType getType() const override { return InstType::Mov; }

    std::string getDump() const override {
        return "MOVE\t" + src->getName() + "\t" + dst->getName();
    }

    std::shared_ptr<SimLoopOperand> getSrc1() const override { return src; }
    std::shared_ptr<SimLoopOperand> getDst() const override { return dst; }
};


class VecInst : public SimLoopInst {
protected:
    std::shared_ptr<SimLoopOperand> src1;
    std::shared_ptr<SimLoopOperand> src2;
    std::shared_ptr<SimLoopOperand> dst;
    InstType vecInstType;
    int elemWidth;
    int vectorWidth;

public:
    VecInst(InstType type, int elemWidth, int vectorWidth, 
               std::shared_ptr<SimLoopOperand> src1,
               std::shared_ptr<SimLoopOperand> src2,
               std::shared_ptr<SimLoopOperand> dst)
        : vecInstType(type), elemWidth(elemWidth), vectorWidth(vectorWidth), src1(std::move(src1)), src2(std::move(src2)), dst(std::move(dst)) {
        assert(this->vecInstType == InstType::Add || this->vecInstType == InstType::Sub || 
               this->vecInstType == InstType::Mul && "three operands can only initialoze to bin vec insts");
        assert(this->dst->getType() == OprType::Register && "Destination must be a register.");
    }

    VecInst(InstType type, int elemWidth, int vectorWidth, 
               std::shared_ptr<SimLoopOperand> src1,
               std::shared_ptr<SimLoopOperand> dst)
        : vecInstType(type), elemWidth(elemWidth), vectorWidth(vectorWidth), src1(std::move(src1)), dst(std::move(dst)) {
        assert((this->vecInstType == InstType::Load || this->vecInstType == InstType::Store || this->vecInstType == InstType::Mov)
               && "two operands can only initialoze to load or store vec or mov insts");
        assert(this->dst->getType() == OprType::Register && "Destination must be a register.");
        src2 = nullptr;
    }

    InstType getType() const override { return vecInstType; }

    std::string getDump() const override {
        static const std::map<InstType, std::string> instNames{
            {InstType::Add, "vADD"}, {InstType::Sub, "vSUB"}, {InstType::Mul, "vMUL"},
            {InstType::Load, "vLOAD"}, {InstType::Store, "vSTORE"}, {InstType::Mov, "vMOV"}};
        return instNames.at(vecInstType) + "_e" + std::to_string(elemWidth) + "_v" + std::to_string(vectorWidth)
               + "\t" + src1->getName() + "\t" + (src2 == nullptr ? "" : src2->getName()) + "\t" + dst->getName();
    }
    int getElemWidth() { return elemWidth; }
    int getVecWidth() { return vectorWidth; }
    std::shared_ptr<SimLoopOperand> getSrc1() const override { return src1; }
    std::shared_ptr<SimLoopOperand> getSrc2() const override { return src2; }
    std::shared_ptr<SimLoopOperand> getDst() const override { return dst; }
};

} // namespace simLoop
} // namespace bolt
} // namespace llvm

#endif // SIMLOOP_H