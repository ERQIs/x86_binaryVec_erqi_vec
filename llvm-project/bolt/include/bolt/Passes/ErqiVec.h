#include "bolt/Passes/BinaryPasses.h"

namespace llvm {
namespace bolt {

class DataflowInfoManager;

class ErqiVec : public BinaryFunctionPass {
    public:
        explicit ErqiVec(const cl::opt<bool> &PrintPass): BinaryFunctionPass(PrintPass) {}

        void runOnFunction(BinaryFunction &BF, DataflowInfoManager &Info);

        const char *getName() const override {
            return "erqi-vectorization";
        }

        Error runOnFunctions(BinaryContext &) override;
};

} // end namespace bolt
} // end namespace llvm