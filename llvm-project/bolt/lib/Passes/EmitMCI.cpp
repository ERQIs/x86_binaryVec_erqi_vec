#include "bolt/Passes/EmitMCI.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
namespace bolt {

Error EmitMCI::runOnFunctions(BinaryContext &BC) {
    if (!BC.isX86())
        return Error::success();

    for (auto &pair : BC.getBinaryFunctions()) {
        BinaryFunction &BF = pair.second;  // 获取 BinaryFunction 对象
        outs() << "Function: " << BF.getOneName() << "\n";

        for (auto &BB : BF) {  // 遍历每个基本块
            outs() << "  BasicBlock: " << BB.getName() << "\n";

            for (auto &Instr : BB) {  // 遍历每个指令
                outs() << "    ";
                BC.printInstruction(errs(), Instr);
                Instr.dump_pretty(errs(), nullptr, "\n", nullptr);
                outs() << "\n\n\n";
            }
        }
    }
    return Error::success();
}

} // end of bolt
} // end of llvm