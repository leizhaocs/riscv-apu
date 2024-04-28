#ifndef ACCELERATOR_H
#define ACCELERATOR_H

#include <riscv/extension.h>
#include <riscv/rocc.h>
#include <random>
#include <limits>
#include "params.h"

typedef acc_t output_t; // Systolic array output datatype (coming down from PEs, moving into accumulator)
static const uint32_t sp_matrices = (BANK_NUM * BANK_ROWS) / DIM; // Size the scratchpad to fit sp_matrices matrices
static const uint32_t accum_rows = ACC_ROWS; // Number of systolic array rows in the accumulator

#define MAKECUSTOMFN(opcode) custom ## opcode
#define CUSTOMFN(opcode) MAKECUSTOMFN(opcode)

struct gemmini_state_t
{
  enum Dataflow {OS, WS};
  void reset();

  uint32_t output_sp_addr;
  uint32_t preload_sp_addr;
  uint16_t preload_cols, preload_rows;
  uint16_t output_cols, output_rows;
  Dataflow mode;
  reg_t sys_shift;
  reg_t load_strides;
  reg_t store_stride;
  uint16_t load_block_strides;

  bool resetted = false;

  std::vector<std::vector<elem_t>> spad; // Scratchpad constructed as systolic array rows
  std::vector<std::vector<acc_t>> pe_state; // Stores each PE's internal accumulator state
  std::vector<std::vector<acc_t>> accumulator;
};

class gemmini_t : public extension_t
{
public:
  gemmini_t() {}
  const char* name() { return "custom_acc"; }
  reg_t CUSTOMFN(XCUSTOM_ACC)( rocc_insn_t insn, reg_t xs1, reg_t xs2);
  void reset();

  void mvin(reg_t dram_addr, reg_t sp_addr);
  void mvout(reg_t dram_addr, reg_t sp_addr);
  void preload(reg_t bd_addr, reg_t c_addr);
  void config(reg_t rs1, reg_t rs2);
  void compute(reg_t a_addr, reg_t bd_addr, bool preload);

  virtual std::vector<insn_desc_t> get_instructions();
  virtual std::vector<disasm_insn_t*> get_disasms();

private:
  gemmini_state_t gemmini_state;

  const unsigned config_funct = 0;
  const unsigned mvin_funct = 2;
  const unsigned mvout_funct = 3;
  const unsigned compute_preloaded_funct = 4;
  const unsigned compute_accumulated_funct = 5;
  const unsigned preload_funct = 6;
  const unsigned flush_funct = 7;

  elem_t mvin_scale(elem_t value, scale_t scale);
  elem_t acc_scale(acc_t value, acc_scale_t acc);
  elem_t sys_shift(output_t value, unsigned int shift);

  template <class T>
  T read_from_dram(reg_t addr);

  template <class T>
  void write_to_dram(reg_t addr, T data);
};

#endif
