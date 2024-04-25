package mini

import chisel3._
import chisel3.util._

/* the actual decoding logic */
object Control {
  val Y = true.B
  val N = false.B

  // pc_sel
  val PC_4   = 0.U(2.W)  // PC = PC + 4
  val PC_ALU = 1.U(2.W)  // PC is computed by ALU, for jump and branch instrucions
  val PC_0   = 2.U(2.W)  // PC = PC, i.e., fetch the same instruction again
  val PC_EPC = 3.U(2.W)  // PC is set to exception handling address, for ERET instruction

  // A_sel
  val A_XXX = 0.U(1.W)  // no first operand
  val A_PC  = 0.U(1.W)  // first operand is from PC
  val A_RS1 = 1.U(1.W)  // first operand is from rs1

  // B_sel
  val B_XXX = 0.U(1.W)  // no second operand
  val B_IMM = 0.U(1.W)  // second operand is from imm
  val B_RS2 = 1.U(1.W)  // second operand is from rs2

  // imm_sel
  val IMM_X = 0.U(3.W)  // no immediate in the instruction
  val IMM_I = 1.U(3.W)  // I type
  val IMM_S = 2.U(3.W)  // S type
  val IMM_U = 3.U(3.W)  // U type
  val IMM_J = 4.U(3.W)  // J type
  val IMM_B = 5.U(3.W)  // B type
  val IMM_Z = 6.U(3.W)  // Z type

  // br_type
  val BR_XXX = 0.U(3.W)  // not a branch instruction
  val BR_EQ  = 3.U(3.W)  // branch on equal
  val BR_NE  = 6.U(3.W)  // branch on not equal
  val BR_LT  = 2.U(3.W)  // branch on less then
  val BR_GE  = 5.U(3.W)  // branch on great equal
  val BR_LTU = 1.U(3.W)  // branch on less than
  val BR_GEU = 4.U(3.W)  // branch on great equal

  // st_type
  val ST_XXX = 0.U(2.W)  // not storing into memory
  val ST_SB  = 3.U(2.W)  // store a byte
  val ST_SH  = 2.U(2.W)  // store a half word
  val ST_SW  = 1.U(2.W)  // store a word

  // ld_type
  val LD_XXX = 0.U(3.W)  // not loading from memory
  val LD_LB  = 3.U(3.W)  // load a byte      (sign extended)
  val LD_LH  = 2.U(3.W)  // load a half word (sign extended)
  val LD_LW  = 1.U(3.W)  // load a word
  val LD_LBU = 5.U(3.W)  // load a byte      (unsign extended)
  val LD_LHU = 4.U(3.W)  // load a half word (unsign extended)

  // wb_sel
  val WB_ALU = 0.U(2.W)  // write back data is from ALU
  val WB_MEM = 1.U(2.W)  // write back data is from memory
  val WB_PC4 = 2.U(2.W)  // write back data is from PC register
  val WB_CSR = 3.U(2.W)  // write back data is from CSR register

  //                          pc_sel  A_sel   B_sel  imm_sel   alu_op      br_type   kill st_type ld_type wb_sel wb_en csr_cmd illegal?
  //                            |       |       |     |          |             |       |    |       |       |     |     |      |
  val default =           List(PC_4  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, Y)

  val map = Array(
    Instructions.LUI   -> List(PC_4  , A_PC,   B_IMM, IMM_U, AluOP.ALU_COPY_B, BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.AUIPC -> List(PC_4  , A_PC,   B_IMM, IMM_U, AluOP.ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),

    Instructions.JAL   -> List(PC_ALU, A_PC,   B_IMM, IMM_J, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_XXX, WB_PC4, Y,    CSR.N, N),
    Instructions.JALR  -> List(PC_ALU, A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_XXX, WB_PC4, Y,    CSR.N, N),

    Instructions.BEQ   -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_EQ , N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.BNE   -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_NE , N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.BLT   -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_LT , N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.BGE   -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_GE , N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.BLTU  -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_LTU, N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.BGEU  -> List(PC_4  , A_PC,   B_IMM, IMM_B, AluOP.ALU_ADD   , BR_GEU, N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),

    Instructions.LB    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LB , WB_MEM, Y,    CSR.N, N),
    Instructions.LH    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LH , WB_MEM, Y,    CSR.N, N),
    Instructions.LW    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LW , WB_MEM, Y,    CSR.N, N),
    Instructions.LBU   -> List(PC_0  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LBU, WB_MEM, Y,    CSR.N, N),
    Instructions.LHU   -> List(PC_0  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LHU, WB_MEM, Y,    CSR.N, N),

    Instructions.SB    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, AluOP.ALU_ADD   , BR_XXX, N, ST_SB , LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.SH    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, AluOP.ALU_ADD   , BR_XXX, N, ST_SH , LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.SW    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, AluOP.ALU_ADD   , BR_XXX, N, ST_SW , LD_XXX, WB_ALU, N,    CSR.N, N),

    Instructions.ADDI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLTI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_SLT   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLTIU -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_SLTU  , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.XORI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_XOR   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.ORI   -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_OR    , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.ANDI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_AND   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLLI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_SLL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SRLI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_SRL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SRAI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, AluOP.ALU_SRA   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),

    Instructions.ADD   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SUB   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SUB   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLL   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SLL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLT   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SLT   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SLTU  -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SLTU  , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.XOR   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_XOR   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SRL   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SRL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.SRA   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_SRA   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.OR    -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_OR    , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),
    Instructions.AND   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, AluOP.ALU_AND   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y,    CSR.N, N),

    Instructions.FENCE -> List(PC_4  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),
    Instructions.ECALL -> List(PC_4  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_CSR, N,    CSR.P, N),
    Instructions.EBREAK-> List(PC_4  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_CSR, N,    CSR.P, N),

    Instructions.FENCEI-> List(PC_0  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N),

    Instructions.CSRRW -> List(PC_0  , A_RS1,  B_XXX, IMM_X, AluOP.ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.W, N),
    Instructions.CSRRS -> List(PC_0  , A_RS1,  B_XXX, IMM_X, AluOP.ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.S, N),
    Instructions.CSRRC -> List(PC_0  , A_RS1,  B_XXX, IMM_X, AluOP.ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.C, N),
    Instructions.CSRRWI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, AluOP.ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.W, N),
    Instructions.CSRRSI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, AluOP.ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.S, N),
    Instructions.CSRRCI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, AluOP.ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y,    CSR.C, N),

    Instructions.ERET  -> List(PC_EPC, A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, N,    CSR.P, N),
    Instructions.WFI   -> List(PC_4  , A_XXX,  B_XXX, IMM_X, AluOP.ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N,    CSR.N, N))
}

/* IO of control unit */
class ControlSignals extends Bundle {
  val inst      = Input(UInt(32.W))  // the undecoded instruction
  val pc_sel    = Output(UInt(2.W))  // how to calculate the PC, i.e., the address of the next instruction to fetch
  val inst_kill = Output(Bool())     //
  val A_sel     = Output(UInt(1.W))  // where does the first operand of ALU come from
  val B_sel     = Output(UInt(1.W))  // where does the second operand of ALU come from
  val imm_sel   = Output(UInt(3.W))  // immediate type, i.e., how to assemble the immediate from the instruction
  val alu_op    = Output(UInt(4.W))  // ALU operation type, i.e., what type of operation are needed from ALU
  val br_type   = Output(UInt(3.W))  // branch type (only for branch instructions)
  val st_type   = Output(UInt(2.W))  // store type (only for store instructions)
  val ld_type   = Output(UInt(3.W))  // load type (only for load instructions)
  val wb_sel    = Output(UInt(2.W))  // the source of the write back data if it needs to write back to register
  val wb_en     = Output(Bool())     // whether the instruction needs to write back to register
  val csr_cmd   = Output(UInt(3.W))  // the type of operation on CSR registers
  val illegal   = Output(Bool())     // whether the instruction is an illegal instruction
}

class Control extends Module {
  // IO
  val io = IO(new ControlSignals)

  val ctrlSignals = ListLookup(io.inst, Control.default, Control.map)

  // control signals for Fetch
  io.pc_sel    := ctrlSignals(0)
  io.inst_kill := ctrlSignals(6).asBool

  // control signals for Execute
  io.A_sel   := ctrlSignals(1)
  io.B_sel   := ctrlSignals(2)
  io.imm_sel := ctrlSignals(3)
  io.alu_op  := ctrlSignals(4)
  io.br_type := ctrlSignals(5)
  io.st_type := ctrlSignals(7)

  // control signals for Write Back
  io.ld_type := ctrlSignals(8)
  io.wb_sel  := ctrlSignals(9)
  io.wb_en   := ctrlSignals(10).asBool
  io.csr_cmd := ctrlSignals(11)
  io.illegal := ctrlSignals(12)
}
