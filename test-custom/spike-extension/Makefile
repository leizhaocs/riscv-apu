ifndef RISCV_ISA_SIM
$(error RISCV_ISA_SIM is unset)
else
$(info Running with RISCV_ISA_SIM=$(RISCV_ISA_SIM))
endif

default: libcustom_acc.so

libcustom_acc.so: accelerator.cc
	g++ -L $(RISCV_ISA_SIM)/lib -Wl,-rpath,$(RISCV_ISA_SIM)/lib -shared -o $@ -std=c++20 -I $(RISCV_ISA_SIM)/include -I ../configuration -fPIC -O3 $^

clean:
	rm -rf *.o *.so
