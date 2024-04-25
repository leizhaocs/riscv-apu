#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

XLEN ?= 32

src_dir := .
out_dir := output

include $(src_dir)/rv32ui/Makefrag

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_PREFIX ?= riscv$(XLEN)-unknown-elf-
RISCV_GCC ?= $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS ?= -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles
RISCV_OBJDUMP ?= $(RISCV_PREFIX)objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data
RISCV_ELF2HEX ?= elf2hex 16 2048
RISCV_SIM ?= spike

vpath %.S $(src_dir)

$(shell mkdir -p $(out_dir))

#------------------------------------------------------------
# Build assembly tests

$(out_dir)/%.dump: %
	$(RISCV_OBJDUMP) $(out_dir)/$< > $@

$(out_dir)/%.hex: %
	$(RISCV_ELF2HEX) $(out_dir)/$< > $@

$(out_dir)/%.out32: %
	$(RISCV_SIM) --isa=rv32gc_zfh_zicboz_svnapot_zicntr_zba_zbb_zbc_zbs --misaligned $(out_dir)/$< 2> $@

define compile_template

$$($(1)_p_tests): $(1)-p-%: $(1)/%.S
	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(src_dir) -I$(src_dir)/macros/scalar -T$(src_dir)/link.ld $$< -o $(out_dir)/$$@
$(1)_tests += $$($(1)_p_tests)

$(1)_tests_dump = $$(addsuffix .dump, $$($(1)_tests))

$(1): $$($(1)_tests_dump)

.PHONY: $(1)

COMPILER_SUPPORTS_$(1) := $$(shell $$(RISCV_GCC) $(2) -c -x c /dev/null -o /dev/null 2> /dev/null; echo $$$$?)

ifeq ($$(COMPILER_SUPPORTS_$(1)),0)
tests += $$($(1)_tests)
endif

endef

$(eval $(call compile_template,rv32ui,-march=rv32g -mabi=ilp32))

tests_dump = $(addprefix $(out_dir)/, $(addsuffix .dump, $(tests)))
tests_hex = $(addprefix $(out_dir)/, $(addsuffix .hex, $(tests)))
tests32_out = $(addprefix $(out_dir)/, $(addsuffix .out32, $(filter rv32%,$(tests))))

hex: $(tests_hex)

run: $(tests32_out)

junk += $(tests) $(tests_dump) $(tests_hex) $(tests32_out)

#------------------------------------------------------------
# Default

all: $(tests_dump)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(junk) $(out_dir)
