CC_BAREMETAL := riscv32-unknown-elf-gcc

src_dir = .

out_dir := output
$(shell mkdir -p $(out_dir))

COMPILE = ../compile
CONFIGURATION = ../configuration
TEST_ISA = ../../test-isa

tests = \
	matmul_ws \
	template \

tests_baremetal = $(addprefix $(out_dir)/, $(tests:=-baremetal))

CFLAGS := \
	-mcmodel=medany \
	-std=gnu99 \
	-O2 \
	-ffast-math \
	-fno-common \
	-fno-builtin-printf \
	-fno-tree-loop-distribute-patterns \
	-march=rv32g -Wa,-march=rv32g \
	-lm \
	-lgcc \
	-I$(COMPILE) \
	-I$(CONFIGURATION) \
	-I$(TEST_ISA) \
	-nostartfiles \
	-static \
	-T $(COMPILE)/test.ld \

vpath %.c $(src_dir)

$(out_dir)/%-baremetal: %.c
	$(CC_BAREMETAL) $(CFLAGS) $< $(LFLAGS) -o $@ \
		$(wildcard $(COMPILE)/*.c) $(wildcard $(COMPILE)/*.S) $(LIBS)

all: $(tests_baremetal)

.PHONY: all clean

clean:
	rm -rf $(tests_baremetal) $(out_dir)