package mini

import chisel3._
import chisel3.util._
import mini.Control._

/* IO of immediate generator */
class ImmGenIO(xlen: Int) extends Bundle {
  val inst = Input(UInt(xlen.W))   // instruction
  val sel  = Input(UInt(3.W))      // select which type of immediate
  val out  = Output(UInt(xlen.W))  // the extracted immediate
}

/* immediate generator */
class ImmGen(val xlen: Int) extends Module {
  // IO
  val io = IO(new ImmGenIO(xlen))

  // all possible immediate types
  val Iimm = io.inst(31, 20).asSInt
  val Simm = Cat(io.inst(31, 25), io.inst(11, 7)).asSInt
  val Bimm = Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W)).asSInt
  val Uimm = Cat(io.inst(31, 12), 0.U(12.W)).asSInt
  val Jimm = Cat(io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 25), io.inst(24, 21), 0.U(1.W)).asSInt
  val Zimm = io.inst(19, 15).zext

  // output a specific immediate type based on sel
  io.out := MuxLookup(io.sel, Iimm & (-2).S)(
    Seq(
      IMM_I -> Iimm,
      IMM_S -> Simm,
      IMM_B -> Bimm,
      IMM_U -> Uimm,
      IMM_J -> Jimm,
      IMM_Z -> Zimm
    )
  ).asUInt
}
