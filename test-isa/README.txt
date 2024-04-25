copy riscv-tests/env/encoding.h
copy riscv-tests/env/p/link.ld
copy riscv-tests/env/p/riscv_test.h
    modify #include "../encoding.h" to #include "encoding.h"

copy riscv-tests/isa/macros
copy riscv-tests/isa/Makefile
    some modifications
riscv-tests/isa/rv32ui
    copy the content of each file in riscv-tests/isa/rv64ui to the corrsponding file in riscv-tests/isa/rv32ui