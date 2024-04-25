// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util.Valid

case class CoreConfig(
  xlen:       Int,
  makeAlu:    Int => Alu = new Alu(_),
  makeBrCond: Int => BrCond = new BrCond(_),
  makeImmGen: Int => ImmGen = new ImmGen(_))

/* IO inside the core, i.e., communicate with CSR */
class HostIO(xlen: Int) extends Bundle {
  val fromhost = Flipped(Valid(UInt(xlen.W)))  // [valid, bits]
  val tohost   = Output(UInt(xlen.W))          //
}

/* IO of core */
class CoreIO(xlen: Int) extends Bundle {
  val host   = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
}

class Core(val conf: CoreConfig) extends Module {
  val io = IO(new CoreIO(conf.xlen))
  val dpath = Module(new Datapath(conf))
  val ctrl = Module(new Control)

  io.host <> dpath.io.host
  dpath.io.icache <> io.icache
  dpath.io.dcache <> io.dcache
  dpath.io.ctrl <> ctrl.io
}
