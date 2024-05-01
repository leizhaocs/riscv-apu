================================================================
Folder meanings:

configurations/
    inst_macros.h: the macros to generate inline assembly of custom instructions (modified from repo: gemmini-rocc-tests/inlcude/gemmini.h)
    params.h: accelerator parameters (modified from repo: gemmini-rocc-tests/inlcude/gemmini-params.h)

compile/
    crt.S:------| this files are used to compile with our own benchmark, they provide some utility functions such as printf, and the link 
    syscall.c:--| script. (copied from repo: riscv-tests/benchmarks/common/)
    test.ld:----|
    util.h:-----|
    xcustom.h: macros to generate rocc custum instruction assembly (copied from repo: rocc-software/src/xcustom.h)

spike-extension/
    accelerator.h:---| this class extends the spike simulator, it implements the functional simulation of our custom accelerator.
    accelerator.cc:--| (modified from repo: libgemmini)

benchmarks/
    all the files in this folder are the actual custom benchmarks.

================================================================
compile functional simulator (extension of spike):

    need to add environmen variable first, in order to find the install spike:

        export RISCV_ISA_SIM=/home/zhaol/install/riscv-isa-sim

    then in spike-extension/:

        make

================================================================
compile benchmarks:

    in benchmarks/:

        make

================================================================
run benchmarks in spike:

    need to add the spike extension in LD_LIBRARY_PATH, for example:

        export LD_LIBRARY_PATH=/home/zhaol/workspace/riscv-apu/test-custom/spike-extension:$LD_LIBRARY_PATH

    then in benchmarks/:

        spike --isa=RV32GC --extension=custom_acc -m0x80000000:0x81920,0x20000000:0x100 <path to compiled benchmark>
