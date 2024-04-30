#include "accelerator.h"
#include <riscv/mmu.h>
#include <riscv/trap.h>
#include <stdexcept>
#include <iostream>
#include <assert.h>
#include <math.h>

using namespace std;

REGISTER_EXTENSION(custom_acc, []() { return new gemmini_t; })

void gemmini_state_t::reset()
{
    spad.clear();
    spad.resize(sp_matrices*DIM, std::vector<elem_t>(DIM, 0));

    pe_state.clear();
    pe_state.resize(DIM, std::vector<acc_t>(DIM));

    accumulator.clear();
    accumulator.resize(accum_rows, std::vector<acc_t>(DIM, 0));

    resetted = true;

    printf("Gemmini extension configured with:\n");
    printf("    dim = %u\n", DIM);
}

void gemmini_t::reset()
{
    gemmini_state.reset();
}

template <class T>
T gemmini_t::read_from_dram(reg_t addr)
{
    T value = 0;
    for (size_t byte_idx = 0; byte_idx < sizeof(T); ++byte_idx)
    {
        value |= p->get_mmu()->load<uint8_t>(addr + byte_idx) << (byte_idx*8);
    }
    return value;
}

template <class T>
void gemmini_t::write_to_dram(reg_t addr, T data)
{
    for (size_t byte_idx = 0; byte_idx < sizeof(T); ++byte_idx)
    {
        p->get_mmu()->store<uint8_t>(addr + byte_idx, (data >> (byte_idx*8)) & 0xFF);
    }
}

void gemmini_t::mvin(reg_t dram_addr, reg_t sp_addr)
{
    bool const accumulator = (sp_addr >> 31) & 0x1;
    bool const accumulate = (sp_addr >> 30) & 0x1;
    auto const base_row_addr = (sp_addr & 0x1FFFFFFF); // Strip accumulator addressing bits [31:29]
    auto const cols = gemmini_state.load_block_strides;
    auto const rows = gemmini_state.load_block_strides;

    bool is_zeros = dram_addr == 0;

    auto const load_stride = gemmini_state.load_strides;
    auto const load_block_stride = gemmini_state.load_block_strides;

    for (size_t row = 0; row < rows; ++row)
    {
        auto const dram_row_addr = dram_addr + row * load_stride;

        for (size_t col = 0; col < cols; ++col)
        {
            const size_t block = col / DIM;
            const size_t spad_col = col % DIM;
            const size_t spad_row = base_row_addr + row + block * load_block_stride;

            for (size_t pixel = 0; pixel < 1 && pixel <= spad_row; pixel++)
            {
                if (accumulator)
                {
                    auto const dram_byte_addr = dram_row_addr + col * sizeof(acc_t);

                    acc_t value;
                    if (is_zeros)
                    {
                        value = 0;
                    }
                    else
                    {
                        value = read_from_dram<acc_t>(dram_byte_addr);
                    }

                    if (accumulate)
                    {
                        gemmini_state.accumulator.at(spad_row - pixel).at(spad_col + pixel * cols) += value;
                    }
                    else
                    {
                        gemmini_state.accumulator.at(spad_row - pixel).at(spad_col + pixel * cols) = value;
                    }
                }
                else
                {
                    auto const dram_byte_addr = dram_row_addr + col * sizeof(elem_t);

                    elem_t value;
                    if (is_zeros)
                    {
                        value = 0;
                    }
                    else
                    {
                        value = read_from_dram<elem_t>(dram_byte_addr);
                        value = mvin_scale(value, 1.0);
                    }

                    gemmini_state.spad.at(spad_row - pixel).at(spad_col + pixel * cols) = value;
                }
            }
        }
    }
}

void gemmini_t::mvout(reg_t dram_addr, reg_t sp_addr)
{
    bool const accumulator = (sp_addr >> 31) & 0x1;
    bool const full = (sp_addr >> 29) & 0x1;
    auto const base_row_addr = (sp_addr & 0x3FFFFFF); // Strip accumulator addressing bits [31:26]
    auto const cols = gemmini_state.load_block_strides;
    auto const rows = gemmini_state.load_block_strides;

    const int block_stride = DIM;

    for (size_t i = 0; i < rows; ++i)
    {
        auto const dram_row_addr = dram_addr + i * gemmini_state.store_stride;

        bool should_write = true;
        for (size_t j = 0; j < cols; j += DIM)
        {
            const size_t block = j / DIM;
            const size_t spad_row = base_row_addr + block * block_stride + i;
            const size_t len = cols - j > DIM ? DIM : cols - j;
        }

        for (size_t j = 0; j < cols; ++j)
        {
            const size_t block = j / DIM;
            const size_t spad_col = j % DIM;
            const size_t spad_row = base_row_addr + block * block_stride + i;

            if (accumulator)
            { // Apply shift and activation when moving out of accumulator
                acc_t acc_value = gemmini_state.accumulator.at(spad_row).at(spad_col);

                elem_t activated = acc_scale(acc_value, 1.0);

                auto const sizeof_output = full ? sizeof(acc_t) : sizeof(elem_t);

                auto const dram_byte_addr = dram_row_addr + j * sizeof_output;
                if (full)
                {
                    write_to_dram<acc_t>(dram_byte_addr, acc_value);
                }
                else
                {
                    write_to_dram<elem_t>(dram_byte_addr, activated);
                }
            }
            else
            { // Scratchpad, write to DRAM directly
                auto const dram_byte_addr = dram_row_addr + j * sizeof(elem_t);
                elem_t value = gemmini_state.spad.at(spad_row).at(spad_col);

                write_to_dram<elem_t>(dram_byte_addr, value);
            }
        }
    }
}

void gemmini_t::preload(reg_t bd_addr, reg_t c_addr)
{
    gemmini_state.preload_sp_addr = static_cast<uint32_t>(bd_addr & 0xFFFFFFFF);
    gemmini_state.output_sp_addr = static_cast<uint32_t>(c_addr & 0xFFFFFFFF);

    gemmini_state.preload_cols = gemmini_state.load_block_strides;
    gemmini_state.preload_rows = gemmini_state.load_block_strides;
    gemmini_state.output_cols = gemmini_state.load_block_strides;
    gemmini_state.output_rows = gemmini_state.load_block_strides;
}

void gemmini_t::config(reg_t rs1, reg_t rs2)
{
    // rs1[1:0] == 2'b00, config_ex, configure execute pipeline
    if ((rs1 & 0b11) == 0)
    {
        gemmini_state_t::Dataflow new_mode;
        reg_t new_sys_shift;

        auto rs1_2 = (rs1 >> 2) & 0b1; // extract rs1[2], 0 = output stationary, 1 = weight stationary
        if (rs1_2 == 0)
        {
            new_mode = gemmini_state_t::OS;
        }
        else
        {
            new_mode = gemmini_state_t::WS;
        }

        new_sys_shift = (rs2)&0xFFFFFFFF;
        assert(new_sys_shift >= 0 && new_sys_shift < sizeof(output_t) * 8);

        gemmini_state.mode = new_mode;
        gemmini_state.sys_shift = new_sys_shift;
    }
    // rs1[1:0] == 2'b01, config_mvin, configure load pipeline
    else if ((rs1 & 0b11) == 1)
    {
        gemmini_state.load_strides = rs2;
        gemmini_state.load_block_strides = (rs1 >> 16) & 0xFFFF;
    }
    // rs1[1:0] == 2'b10, config_mvout, configure store pipeline
    else if ((rs1 & 0b11) == 2)
    {
        gemmini_state.store_stride = rs2 & 0xFFFFFFFF;
    }
}

void gemmini_t::compute(reg_t a_addr, reg_t bd_addr, bool preload)
{
    auto a_addr_real = static_cast<uint32_t>(a_addr & 0xFFFFFFFF);
    auto bd_addr_real = static_cast<uint32_t>(bd_addr & 0xFFFFFFFF);

    const uint16_t a_cols = gemmini_state.load_block_strides;
    const uint16_t a_rows = gemmini_state.load_block_strides;

    const uint16_t bd_cols = gemmini_state.load_block_strides;
    const uint16_t bd_rows = gemmini_state.load_block_strides;

    // Preload
    if (preload)
    {
        for (size_t i = 0; i < DIM; i++)
        {
            for (size_t j = 0; j < DIM; j++)
            {
                // TODO: Handle preloads from accumulator, values are shifted and activated before preload
                if (~gemmini_state.preload_sp_addr != 0)
                {
                    assert(((gemmini_state.preload_sp_addr >> 30) & 0b11) == 0); // Preloads from accumulator not supported
                }

                // In OS mode, pe_state stores the accumulator values
                // In WS mode, pe_state stores the persistent weight matrix
                if (i < gemmini_state.preload_rows && j < gemmini_state.preload_cols)
                {
                    auto preload_value = (~gemmini_state.preload_sp_addr == 0) ? 0 : gemmini_state.spad.at(gemmini_state.preload_sp_addr + i).at(j);
                    gemmini_state.pe_state.at(i).at(j) = preload_value;
                }
                else
                {
                    gemmini_state.pe_state.at(i).at(j) = 0;
                }
            }
        }
    }

    // Compute
    // For OS, accumulate the PE results internally in pe_state
    // For WS, allocate a new results array which won't affect pe_state, seed the results array with the bias (D) matrix
    auto results = std::vector<std::vector<acc_t>>(DIM, std::vector<acc_t>(DIM));
    for (size_t i = 0; i < DIM; ++i)
    {
        for (size_t j = 0; j < DIM; ++j)
        {
            if (i < bd_rows && j < bd_cols)
            {
                results.at(i).at(j) = (~bd_addr_real == 0) ? 0 : gemmini_state.spad.at(bd_addr_real + i).at(j);
            }
            else
            {
                results.at(i).at(j) = 0;
            }
        }
    }

    for (size_t i = 0; i < DIM; ++i)
    {
        for (size_t j = 0; j < DIM; ++j)
        {
            for (size_t k = 0; k < DIM; ++k)
            {
                elem_t a;
                if (~a_addr_real != 0)
                {
                    a = i < a_rows && k < a_cols ? gemmini_state.spad.at(a_addr_real + i).at(k) : 0;
                }

                if (gemmini_state.mode == gemmini_state_t::WS)
                {
                    results.at(i).at(j) += a * gemmini_state.pe_state.at(k).at(j);
                }
                else
                {
                    elem_t b = 0;
                    if (~bd_addr_real != 0)
                    {
                        b = k < bd_rows && j < bd_cols ? gemmini_state.spad.at(bd_addr_real + k).at(j) : 0;
                    }

                    gemmini_state.pe_state.at(i).at(j) += a * b;
                }
            }
        }
    }

    // Write results
    if (~gemmini_state.output_sp_addr != 0)
    {
        bool const acc = (gemmini_state.output_sp_addr >> 31) & 0x1;
        bool const acc_accum = (gemmini_state.output_sp_addr >> 30) & 0x1;
        auto const base_sp_addr = gemmini_state.output_sp_addr & 0x1FFFFFFF;

        for (size_t i = 0; i < gemmini_state.output_rows; ++i)
        {
            for (size_t j = 0; j < gemmini_state.output_cols; ++j)
            {
                acc_t value = gemmini_state.mode == gemmini_state_t::OS ? gemmini_state.pe_state.at(i).at(j) : results.at(i).at(j);
                if (acc)
                {
                    output_t shifted = gemmini_state.mode == gemmini_state_t::OS ? sys_shift(value, gemmini_state.sys_shift) : sys_shift(value, 0);

                    if (acc_accum)
                    {
                        gemmini_state.accumulator.at(base_sp_addr + i).at(j) += value;
                    }
                    else
                    {
                        gemmini_state.accumulator.at(base_sp_addr + i).at(j) = value;
                    }
                }
                else
                {
                    elem_t activated = gemmini_state.mode == gemmini_state_t::OS ? sys_shift(value, gemmini_state.sys_shift) : sys_shift(value, 0);
                    gemmini_state.spad.at(base_sp_addr + i).at(j) = activated;
                }
            }
        }
    }
}

reg_t gemmini_t::CUSTOMFN(XCUSTOM_ACC)(rocc_insn_t insn, reg_t xs1, reg_t xs2)
{
    if (!gemmini_state.resetted)
    {
        reset();
    }

    if (insn.funct == mvin_funct)
    {
        mvin(xs1, xs2);
    }
    else if (insn.funct == mvout_funct)
    {
        mvout(xs1, xs2);
    }
    else if (insn.funct == preload_funct)
    {
        preload(xs1, xs2);
    }
    else if (insn.funct == config_funct)
    {
        config(xs1, xs2);
    }
    else if (insn.funct == compute_preloaded_funct)
    {
        compute(xs1, xs2, true);
    }
    else if (insn.funct == compute_accumulated_funct)
    {
        compute(xs1, xs2, false);
    }
    else
    {
        illegal_instruction();
    }
    return 0;
}

elem_t gemmini_t::mvin_scale(elem_t value, scale_t scale)
{
    acc_t scaled = MVIN_SCALE(value, scale);

    // Saturate and cast element
    const auto elem_t_max = std::numeric_limits<elem_t>::max();
    const auto elem_t_min = std::numeric_limits<elem_t>::min();
    scaled = scaled > elem_t_max ? elem_t_max : (scaled < elem_t_min ? elem_t_min : scaled);

    return scaled;
}

elem_t gemmini_t::acc_scale(acc_t value, acc_scale_t scale) {
    acc_t scaled = ACC_SCALE(value, scale);

    // Saturate and cast element
    const auto elem_t_max = std::numeric_limits<elem_t>::max();
    const auto elem_t_min = std::numeric_limits<elem_t>::min();
    scaled = scaled > elem_t_max ? elem_t_max : (scaled < elem_t_min ? elem_t_min : scaled);

    return scaled;
}

elem_t gemmini_t::sys_shift(acc_t value, unsigned int shift)
{
    acc_t shifted = ROUNDING_RIGHT_SHIFT(value, shift);

    // Saturate and cast element
    const auto elem_t_max = std::numeric_limits<elem_t>::max();
    const auto elem_t_min = std::numeric_limits<elem_t>::min();
    shifted = shifted > elem_t_max ? elem_t_max : (shifted < elem_t_min ? elem_t_min : shifted);

    return shifted;
}

/* generate a function that calls the accelerator's function to handle each received instruction
 *
 *  Param1: the class that implements the accelerator (need to inherit extension_t)
 *  Param2: the name of the extended class
 *  Param3: the function name to be generated by this macro
 *  Param4: the function in our accelerator class to handles each instruction
 */
define_custom_func(gemmini_t, "custom_acc", gemmini_custom3, custom3)

std::vector<insn_desc_t> gemmini_t::get_instructions()
{
  std::vector<insn_desc_t> insns;
  push_custom_insn(insns, ROCC_OPCODE3, ROCC_OPCODE_MASK, gemmini_custom3, ILLEGAL_INSN_FUNC);
  return insns;
}

std::vector<disasm_insn_t*> gemmini_t::get_disasms()
{
  std::vector<disasm_insn_t*> insns;
  return insns;
}
