package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// constant for default PC addresses
object Consts {
  val PC_START = 0x200  // first instruction to fetch when CPU starts
  val PC_EVEC  = 0x100  // exception handling address
}

/* Fetch-Execute pipeline registers */
class FetchExecutePipelineRegister(xlen: Int) extends Bundle {
  val pc   = UInt(xlen.W)                    // PC, i.e., the address of the next instuction to be fetched
  val inst = chiselTypeOf(Instructions.NOP)  // fetched instruction
}

/* Execute-Writeback pipeline registers */
class ExecuteWritebackPipelineRegister(xlen: Int) extends Bundle {
  val pc     = UInt(xlen.W)                    //
  val inst   = chiselTypeOf(Instructions.NOP)  //
  val alu    = UInt(xlen.W)                    //
  val csr_in = UInt(xlen.W)                    //
}

/* IO of datapath */
class DatapathIO(xlen: Int) extends Bundle {
  val host   = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
  val ctrl   = Flipped(new ControlSignals)
}

/* datapath, i.e., pipeline */
class Datapath(val conf: CoreConfig) extends Module {
  // IO
  val io = IO(new DatapathIO(conf.xlen))

  //=========================================================================
  // datapath components

  val csr     = Module(new CSR(conf.xlen))
  val regFile = Module(new RegFile(conf.xlen))
  val alu     = Module(conf.makeAlu(conf.xlen))
  val immGen  = Module(conf.makeImmGen(conf.xlen))
  val brCond  = Module(conf.makeBrCond(conf.xlen))

  //=========================================================================
  // pipeline registers

  // Fetch-Execute pipeline register
  val fe_reg = RegInit(
    (new FetchExecutePipelineRegister(conf.xlen)).Lit(
      _.pc   -> 0.U,
      _.inst -> Instructions.NOP
    )
  )

  // Execute-Writeback pipeline register
  val ew_reg = RegInit(
    (new ExecuteWritebackPipelineRegister(conf.xlen)).Lit(
      _.pc     -> 0.U,
      _.inst   -> Instructions.NOP,
      _.alu    -> 0.U,
      _.csr_in -> 0.U
    )
  )

  /****** Control signals ******/
  val st_type  = Reg(io.ctrl.st_type.cloneType)
  val ld_type  = Reg(io.ctrl.ld_type.cloneType)
  val wb_sel   = Reg(io.ctrl.wb_sel.cloneType)
  val wb_en    = Reg(Bool())
  val csr_cmd  = Reg(io.ctrl.csr_cmd.cloneType)
  val illegal  = Reg(Bool())
  val pc_check = Reg(Bool())

  //=========================================================================
  // Fetch stage

  val started = RegNext(reset.asBool)  // reset CPU

  val stall = !io.icache.resp.valid || !io.dcache.resp.valid  // stall pipeline if waiting for icache or dcache

  // compute the next PC and update PC
  val pc = RegInit(Consts.PC_START.U(conf.xlen.W) - 4.U(conf.xlen.W))  // PC register
  val next_pc = MuxCase(
    pc + 4.U,                                                                                    // defaule is PC+4, i.e., next sequential instruction
    IndexedSeq(
      stall -> pc,                                                                               // if pipeline is stalled, fetch the same instruction again
      csr.io.expt -> csr.io.evec,                                                                // if CSR exception, go to the exception handling address specified by CSR
      (io.ctrl.pc_sel === Control.PC_EPC) -> csr.io.epc,                                         // if ERET instruction, go to the next instruction that generates the exception
      ((io.ctrl.pc_sel === Control.PC_ALU) || (brCond.io.taken)) -> (alu.io.sum >> 1.U << 1.U),  // if jump or taken branch instrution, go to address computed by ALU
      (io.ctrl.pc_sel === Control.PC_0) -> pc                                                    // fetch the same instruction again
    )
  )
  pc := next_pc  // update PC

  // access icache to fetch instruction
  io.icache.req.valid     := !stall   // access icache only if pipeline is not stalled
  io.icache.req.bits.addr := next_pc  // access address
  io.icache.req.bits.data := 0.U      // never write into icache
  io.icache.req.bits.mask := 0.U      // read
  io.icache.abort         := false.B  // never write into icache, so never abort write
  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.resp.bits.data)

  // update pipeline register
  when(!stall) {
    fe_reg.pc   := pc
    fe_reg.inst := inst
  }

  //=========================================================================
  // Execute stage

  // send fetched instruction to controller for decoding
  io.ctrl.inst := fe_reg.inst

  // generate immediate
  immGen.io.inst := fe_reg.inst
  immGen.io.sel  := io.ctrl.imm_sel

  // read register file
  val rs1_addr = fe_reg.inst(19, 15)
  val rs2_addr = fe_reg.inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // bypass (select either from Writeback stage or register file)
  val wb_rd_addr = ew_reg.inst(11, 7)
  val rs1hazard = wb_en && (wb_sel === Control.WB_ALU) && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard = wb_en && (wb_sel === Control.WB_ALU) && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  val rs1 = Mux(rs1hazard, ew_reg.alu, regFile.io.rdata1)
  val rs2 = Mux(rs2hazard, ew_reg.alu, regFile.io.rdata2)

  // ALU operations
  alu.io.A      := Mux(io.ctrl.A_sel === Control.A_RS1, rs1, fe_reg.pc)
  alu.io.B      := Mux(io.ctrl.B_sel === Control.B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // branch condition calculation
  brCond.io.rs1     := rs1
  brCond.io.rs2     := rs2
  brCond.io.br_type := io.ctrl.br_type

  // access dcache
  val daddr = Mux(stall, ew_reg.alu, alu.io.sum) >> 2.U << 2.U
  val woffset = (alu.io.sum(1) << 4.U).asUInt | (alu.io.sum(0) << 3.U).asUInt
  io.dcache.req.valid     := !stall && (io.ctrl.st_type.orR || io.ctrl.ld_type.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), "b0000".U)(
    Seq(
      Control.ST_SW -> "b1111".U,
      Control.ST_SH -> ("b11".U << alu.io.sum(1, 0)),
      Control.ST_SB -> ("b1".U << alu.io.sum(1, 0))
    )
  )

  // Pipelining
  when(reset.asBool || !stall && csr.io.expt) {
    st_type := 0.U
    ld_type := 0.U
    wb_en := false.B
    csr_cmd := 0.U
    illegal := false.B
    pc_check := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    ew_reg.pc     := fe_reg.pc
    ew_reg.inst   := fe_reg.inst
    ew_reg.alu    := alu.io.out
    ew_reg.csr_in := Mux(io.ctrl.imm_sel === Control.IMM_Z, immGen.io.out, rs1)
    st_type       := io.ctrl.st_type
    ld_type       := io.ctrl.ld_type
    wb_sel        := io.ctrl.wb_sel
    wb_en         := io.ctrl.wb_en
    csr_cmd       := io.ctrl.csr_cmd
    illegal       := io.ctrl.illegal
    pc_check      := io.ctrl.pc_sel === Control.PC_ALU
  }

  //=========================================================================
  // Writeback stage

  // get data from dcache
  val loffset = (ew_reg.alu(1) << 4.U).asUInt | (ew_reg.alu(0) << 3.U).asUInt
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    = MuxLookup(ld_type, io.dcache.resp.bits.data.zext)(
    Seq(
      Control.LD_LH  -> lshift(15, 0).asSInt,
      Control.LD_LB  -> lshift(7, 0).asSInt,
      Control.LD_LHU -> lshift(15, 0).zext,
      Control.LD_LBU -> lshift(7, 0).zext
    )
  )

  // CSR access
  csr.io.stall    := stall
  csr.io.cmd      := csr_cmd
  csr.io.in       := ew_reg.csr_in
  csr.io.pc       := ew_reg.pc
  csr.io.addr     := ew_reg.alu
  csr.io.inst     := ew_reg.inst
  csr.io.illegal  := illegal
  csr.io.st_type  := st_type
  csr.io.ld_type  := ld_type
  csr.io.pc_check := pc_check
  io.host         <> csr.io.host

  // write register file
  val regWrite = MuxLookup(wb_sel, ew_reg.alu.zext)(
    Seq(
      Control.WB_MEM -> load,
      Control.WB_PC4 -> (ew_reg.pc + 4.U).zext,
      Control.WB_CSR -> csr.io.out.zext)
  ).asUInt
  regFile.io.wen   := wb_en && !stall && !csr.io.expt
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite

  // abort store when there's an excpetion
  io.dcache.abort := csr.io.expt
}
