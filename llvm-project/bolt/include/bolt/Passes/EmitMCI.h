#include "bolt/Passes/BinaryPasses.h"

namespace llvm {
namespace bolt {

class DataflowInfoManager;

class EmitMCI : public BinaryFunctionPass {
    public:
        explicit EmitMCI(const cl::opt<bool> &PrintPass): BinaryFunctionPass(PrintPass) {}

        void runOnFunction(BinaryFunction &BF, DataflowInfoManager &Info);

        const char *getName() const override {
            return "Emit-MCI";
        }

        Error runOnFunctions(BinaryContext &) override;
};

} // end namespace bolt
} // end namespace llvm