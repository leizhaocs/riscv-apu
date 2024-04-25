package mini

import chisel3._
import mini.Control._

/* IO of branch condition */
class BrCondIO(xlen: Int) extends Bundle {
  val rs1     = Input(UInt(xlen.W))  // first compare operand
  val rs2     = Input(UInt(xlen.W))  // second compare operand
  val br_type = Input(UInt(3.W))     // branch type
  val taken   = Output(Bool())       // result: taken or not
}

/* branch condition */
class BrCond(val xlen: Int) extends Module {
  // IO
  val io = IO(new BrCondIO(xlen))

  // all possible conditions
  val eq  = io.rs1 === io.rs2
  val neq = !eq
  val lt  = io.rs1.asSInt < io.rs2.asSInt
  val ge  = !lt
  val ltu = io.rs1 < io.rs2
  val geu = !ltu

  // select a specific condition based on branch type
  io.taken := ((io.br_type === BR_EQ) && eq) ||
              ((io.br_type === BR_NE) && neq) ||
              ((io.br_type === BR_LT) && lt) ||
              ((io.br_type === BR_GE) && ge) ||
              ((io.br_type === BR_LTU) && ltu) ||
              ((io.br_type === BR_GEU) && geu)
}
