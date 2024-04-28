#ifndef INST_MACROS_H
#define INST_MACROS_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdbool.h>

#include "params.h"

#include "xcustom.h"

#define k_CONFIG 0
#define k_MVIN 2
#define k_MVOUT 3
#define k_COMPUTE_PRELOADED 4
#define k_COMPUTE_ACCUMULATE 5
#define k_PRELOAD 6
#define k_FLUSH 7

#define CONFIG_EX 0
#define CONFIG_LD 1
#define CONFIG_ST 2

#define GARBAGE_ADDR ((uint32_t)(-1))
#define OUTPUT_STATIONARY 0
#define WEIGHT_STATIONARY 1

#define ROCC_INSTRUCTION_RS1_RS2(x, rs1, rs2, funct) \
  ROCC_INSTRUCTION_0_R_R(x, rs1, rs2, funct)

//========================================================================

#define gemmini_mvin(dram_addr, spad_addr) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, dram_addr, spad_addr, k_MVIN)

//========================================================================

#define gemmini_mvout(dram_addr, spad_addr) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, dram_addr, spad_addr, k_MVOUT)

//========================================================================

#define gemmini_compute_preloaded(A, BD) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, A, BD, k_COMPUTE_PRELOADED)

#define gemmini_compute_accumulated(A, BD) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, A, BD, k_COMPUTE_ACCUMULATE)

//========================================================================

#define gemmini_preload(BD, C) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, BD, C, k_PRELOAD)

//========================================================================

#define gemmini_config_ex(dataflow, sys_shift) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, ((dataflow) << 2) | CONFIG_EX, (sys_shift), k_CONFIG)

//========================================================================

#define gemmini_config_ld(stride) \
  ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, ((DIM) << 16) | CONFIG_LD, stride, k_CONFIG)

//========================================================================

#define gemmini_config_st(stride) \
    ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, CONFIG_ST, ((uint32_t)stride), k_CONFIG)

//========================================================================

#define gemmini_flush(skip) \
  ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, skip, 0, k_FLUSH)

//========================================================================

#define gemmini_fence() asm volatile("fence")

#endif

