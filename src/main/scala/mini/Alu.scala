package mini

import chisel3._
import chisel3.util._

/* ALU operation type */
object AluOP {
  val ALU_ADD    = 0.U(4.W)
  val ALU_SUB    = 1.U(4.W)
  val ALU_AND    = 2.U(4.W)
  val ALU_OR     = 3.U(4.W)
  val ALU_XOR    = 4.U(4.W)
  val ALU_SLT    = 5.U(4.W)
  val ALU_SLL    = 6.U(4.W)
  val ALU_SLTU   = 7.U(4.W)
  val ALU_SRL    = 8.U(4.W)
  val ALU_SRA    = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_XXX    = 15.U(4.W)
}

/* IO of ALU */
class AluIO(width: Int) extends Bundle {
  val A      = Input(UInt(width.W))   // first operand
  val B      = Input(UInt(width.W))   // second operand
  val alu_op = Input(UInt(4.W))       // operation type
  val out    = Output(UInt(width.W))  //
  val sum    = Output(UInt(width.W))  //
}

/* ALU */
class Alu(val width: Int) extends Module {
  // IO
  val io = IO(new AluIO(width))

  val shamt = io.B(4, 0).asUInt

  io.out := MuxLookup(io.alu_op, io.B)(
    Seq(
      AluOP.ALU_ADD    -> (io.A + io.B),
      AluOP.ALU_SUB    -> (io.A - io.B),
      AluOP.ALU_SRA    -> (io.A.asSInt >> shamt).asUInt,
      AluOP.ALU_SRL    -> (io.A >> shamt),
      AluOP.ALU_SLL    -> (io.A << shamt),
      AluOP.ALU_SLT    -> (io.A.asSInt < io.B.asSInt),
      AluOP.ALU_SLTU   -> (io.A < io.B),
      AluOP.ALU_AND    -> (io.A & io.B),
      AluOP.ALU_OR     -> (io.A | io.B),
      AluOP.ALU_XOR    -> (io.A ^ io.B),
      AluOP.ALU_COPY_A -> io.A
    )
  )

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B)
}
