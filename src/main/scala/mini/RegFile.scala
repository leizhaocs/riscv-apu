package mini

import chisel3._

/* IO of register file */
class RegFileIO(xlen: Int) extends Bundle {
  val raddr1 = Input(UInt(5.W))      // rs1 read address
  val raddr2 = Input(UInt(5.W))      // rs2 read address
  val rdata1 = Output(UInt(xlen.W))  // rs1 read data
  val rdata2 = Output(UInt(xlen.W))  // rs2 read data
  val wen    = Input(Bool())         // rd write enable
  val waddr  = Input(UInt(5.W))      // rd write address
  val wdata  = Input(UInt(xlen.W))   // rd write data
}

/* register file */
class RegFile(xlen: Int) extends Module {
  // IO
  val io = IO(new RegFileIO(xlen))

  // registers
  val regs = Mem(32, UInt(xlen.W))

  // read
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)

  // write
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}
