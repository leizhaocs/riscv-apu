default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main
gen_dir    = $(base_dir)/generated-src
out_dir    = $(base_dir)/outputs
threads   ?= 1

SBT       = sbt
SBT_FLAGS = -ivy $(base_dir)/.ivy2

########################################################################
# compile scala into sv

$(gen_dir)/Tile.sv: $(wildcard $(src_dir)/scala/*.scala)
	$(SBT) $(SBT_FLAGS) "run --target-dir=$(gen_dir)"

compile: $(gen_dir)/Tile.sv

########################################################################
# compile sv into c++ simulator using verilator

CXXFLAGS += -std=c++14 -Wall -Wno-unused-variable

VERILATOR = verilator --cc --exe
VERILATOR_FLAGS = --assert -Wno-STMTDLY -O3 --trace --threads $(threads)\
	--top-module Tile -Mdir $(gen_dir)/VTile.csrc \
	-CFLAGS "$(CXXFLAGS) -include $(gen_dir)/VTile.csrc/VTile.h" 

$(base_dir)/VTile: $(gen_dir)/Tile.sv $(src_dir)/cc/top.cc $(src_dir)/cc/mm.cc $(src_dir)/cc/mm.h
	$(VERILATOR) $(VERILATOR_FLAGS) -o $@ $< $(word 2, $^) $(word 3, $^)
	$(MAKE) -C $(gen_dir)/VTile.csrc -f VTile.mk

verilator: $(base_dir)/VTile

########################################################################
# isa tests + benchmarks with verilator

test_hex_files = $(wildcard $(base_dir)/tests/*.hex)
test_out_files = $(foreach f,$(test_hex_files),$(patsubst %.hex,%.out,$(out_dir)/$(notdir $f)))

$(test_out_files): $(out_dir)/%.out: $(base_dir)/VTile $(base_dir)/tests/%.hex
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-tests: $(test_out_files)

########################################################################
# run isa tests benchmarks with verilator

run-test-isa:
	$(MAKE) -C test-isa
	$(MAKE) -C test-isa hex
	$(MAKE) -C test-isa run

########################################################################
# run custom tests benchmarks with verilator

run-test-custom:
	$(MAKE) -C test-custom/spike-extension
	$(MAKE) -C test-custom/benchmarks

########################################################################
# run custom benchmark

custom_bmark_hex ?= $(base_dir)/custom-bmark/main.hex
custom_bmark_out  = $(patsubst %.hex,%.out,$(out_dir)/$(notdir $(custom_bmark_hex)))

$(custom_bmark_hex):
	$(MAKE) -C custom-bmark

$(custom_bmark_out): $(base_dir)/VTile $(custom_bmark_hex)
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-custom-bmark: $(custom_bmark_out)

########################################################################

clean:
	rm -rf $(gen_dir) $(out_dir) test_run_dir
	$(MAKE) -C test-isa clean
	$(MAKE) -C test-custom/spike-extension clean
	$(MAKE) -C test-custom/benchmarks clean

cleanall: clean
	rm -rf target project/target project/project

.PHONY: compile verilator run-tests run-custom-bmark clean cleanall
