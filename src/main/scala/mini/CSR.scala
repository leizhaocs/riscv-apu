package mini

import chisel3._
import chisel3.util._

/* constants and CSR addresses */
object CSR {
  // operation type on CSR registers
  val N = 0.U(3.W)  // no operation on CSR
  val W = 1.U(3.W)  // read and write (for CSRRW and CSRRWI)
  val S = 2.U(3.W)  // read and set   (for CSRRS and CSRRSI)
  val C = 3.U(3.W)  // read and clear (for CSRRC and CSRRCI)
  val P = 4.U(3.W)  // privileged instruction (for ECALL EBREAK and ERET)

  // Supports machine & user modes, these are acutally the highest 2 bits in CSR instructions
  val PRV_U = 0x0.U(2.W)  // user mode
  val PRV_M = 0x3.U(2.W)  // machine mode

  //-------------------------------------------
  // user-level CSR addresses

  // User Counter/Timers
  val cycle    = 0xc00.U(12.W)
  val time     = 0xc01.U(12.W)
  val instret  = 0xc02.U(12.W)
  val cycleh   = 0xc80.U(12.W)
  val timeh    = 0xc81.U(12.W)
  val instreth = 0xc82.U(12.W)

  //-------------------------------------------
  // supervisor-level CSR addresses

  // Supervisor Read/Write Shadow of User Read-Only registers
  val cyclew    = 0x900.U(12.W)
  val timew     = 0x901.U(12.W)
  val instretw  = 0x902.U(12.W)
  val cyclehw   = 0x980.U(12.W)
  val timehw    = 0x981.U(12.W)
  val instrethw = 0x982.U(12.W)

  //-------------------------------------------
  // machine-level CSR addresses

  // Machine Information Registers
  val mcpuid  = 0xf00.U(12.W)
  val mimpid  = 0xf01.U(12.W)
  val mhartid = 0xf10.U(12.W)

  // Machine Trap Setup
  val mstatus  = 0x300.U(12.W)
  val mtvec    = 0x301.U(12.W)
  val mtdeleg  = 0x302.U(12.W)
  val mie      = 0x304.U(12.W)
  val mtimecmp = 0x321.U(12.W)

  // Machine Timers and Counters
  val mtime  = 0x701.U(12.W)
  val mtimeh = 0x741.U(12.W)

  // Machine Trap Handling
  val mscratch = 0x340.U(12.W)
  val mepc     = 0x341.U(12.W)
  val mcause   = 0x342.U(12.W)
  val mbadaddr = 0x343.U(12.W)
  val mip      = 0x344.U(12.W)

  // Machine Host-Target Interface
  val mtohost   = 0x780.U(12.W)
  val mfromhost = 0x781.U(12.W)

  // put all the above CSR addresses in a list
  val regs = List(
    cycle,
    time,
    instret,
    cycleh,
    timeh,
    instreth,
    cyclew,
    timew,
    instretw,
    cyclehw,
    timehw,
    instrethw,
    mcpuid,
    mimpid,
    mhartid,
    mstatus,
    mtvec,
    mtdeleg,
    mie,
    mtimecmp,
    mtime,
    mtimeh,
    mscratch,
    mepc,
    mcause,
    mbadaddr,
    mip,
    mtohost,
    mfromhost
  )
}

/* cause of an exception */
object Cause {
  val InstAddrMisaligned  = 0x0.U  // Instruction address misaligned
  val IllegalInst         = 0x2.U  // Illegal instruction
  val Breakpoint          = 0x3.U  // Breakpoint
  val LoadAddrMisaligned  = 0x4.U  // Load address misaligned
  val StoreAddrMisaligned = 0x6.U  // Store/AMO address misaligned
  val Ecall               = 0x8.U  // Environment call from U-mode
}

/* IO of CSR */
class CSRIO(xlen: Int) extends Bundle {
  val stall = Input(Bool())            // whether pipeline has been stalled
  val cmd   = Input(UInt(3.W))         // the type of operation on CSR registers, only used if currently executing instruction is CSR instruction or privileged instruction (for ECALL EBREAK and ERET)
  val in    = Input(UInt(xlen.W))      // the data to write into CSR registers, only used if currently executing instruction is CSR instruction
  val out   = Output(UInt(xlen.W))     // the data read out from CSR registers

  // Excpetion
  val pc       = Input(UInt(xlen.W))   // next PC of the currently executing instruction, used to record the return address if exception happens
  val addr     = Input(UInt(xlen.W))   // the address computed by ALU (only used when the the currently executing instruction is load, store or JAL JALR)
  val inst     = Input(UInt(xlen.W))   // the currently executing instruction (only used if it is a CSR instruction)
  val illegal  = Input(Bool())         // whether the currently executing instruction is an illegal instruction (determined when decoding the instruction)
  val st_type  = Input(UInt(2.W))      // the store type of the currently executing instruction, used for checking the alignment of the computed store address
  val ld_type  = Input(UInt(3.W))      // the load type of the currently executing instruction, used for checking the alignment of the computed load address
  val pc_check = Input(Bool())         // inidicate if the currently executing instruction is JAL or JALR, used for checking the alignment of the computed PC
  val expt     = Output(Bool())        // true: if an exception happens
  val evec     = Output(UInt(xlen.W))  // the address of exception handler
  val epc      = Output(UInt(xlen.W))  // the return address after handling an exception

  // HTIF
  val host = new HostIO(xlen)          // to communicate with host
}

/* CSR module */
class CSR(val xlen: Int) extends Module {
  // IO
  val io = IO(new CSRIO(xlen))

  // similar to decoding the instruction
  val csr_addr = io.inst(31, 20)
  val rs1_addr = io.inst(19, 15)

  //--------------------------------------------------------
  // the actual CSR registers

  // counter and timer
  val time     = RegInit(0.U(xlen.W))  // timer
  val timeh    = RegInit(0.U(xlen.W))  // timer (high 32 bits)
  val cycle    = RegInit(0.U(xlen.W))  // cycle counter
  val cycleh   = RegInit(0.U(xlen.W))  // cycle counter (high 32 bits)
  val instret  = RegInit(0.U(xlen.W))  // retired instruction counter
  val instreth = RegInit(0.U(xlen.W))  // retired instruction counter (high 32 bits)
  val mtimecmp = Reg(UInt(xlen.W))     // generate timer interrupt when its value is equal to timer register

  // machine information
  val mcpuid = Cat(
    0.U(2.W),               // RV32I
    0.U((xlen - 28).W),
    (1 << ('I' - 'A') |     // I: Base ISA
     1 << ('U' - 'A')       // U: User Mode
    ).U(26.W)
  )
  val mimpid  = 0.U(xlen.W) // not implemented
  val mhartid = 0.U(xlen.W) // only one hart

  // interrupt handling
  val PRV  = RegInit(CSR.PRV_M)  // interrupt enable stack: 
  val PRV1 = RegInit(CSR.PRV_M)  // interrupt enable stack: 
  val PRV2 = 0.U(2.W)            // interrupt enable stack: 
  val PRV3 = 0.U(2.W)            // interrupt enable stack: 
  val IE   = RegInit(false.B)    // interrupt enable stack: 
  val IE1  = RegInit(false.B)    // interrupt enable stack: 
  val IE2  = false.B             // interrupt enable stack: 
  val IE3  = false.B             // interrupt enable stack: 
  val VM = 0.U(5.W)              // virtualization management: no memory management or translation
  val MPRV = false.B             // memory privilege: normal memory access
  val XS = 0.U(2.W)              // extention context status: no additional user-mode extension and associated state
  val FS = 0.U(2.W)              // extention context status: no floating point unit context switch
  val SD = 0.U(1.W)              // extention context status: when either the FS or XS bits encode a Dirty state, i.e., SD=((FS==11) OR (XS==11))
  val mstatus = Cat(SD, 0.U((xlen - 23).W), VM, MPRV, XS, FS, PRV3, IE3, PRV2, IE2, PRV1, IE1, PRV, IE)
  val mtvec = Consts.PC_EVEC.U(xlen.W)  // base address of the M-mode trap vector
  val mtdeleg = 0x0.U(xlen.W)           // all 0: always direct all traps to machine mode

  // interrupt pending and enable
  val MTIP = RegInit(false.B)  // timer interrupt-pending bits in machine mode
  val HTIP = false.B           // timer interrupt-pending bits in hypervisor mode
  val STIP = false.B           // timer interrupt-pending bits in supervisor mode
  val MTIE = RegInit(false.B)  // timer interrupt-enable bit in machine mode
  val HTIE = false.B           // timer interrupt-enable bit in hypervisor mode
  val STIE = false.B           // timer interrupt-enable bit in supervisor mode
  val MSIP = RegInit(false.B)  // software interrupt-pending bit in machine mode
  val HSIP = false.B           // software interrupt-pending bit in hypervisor mode
  val SSIP = false.B           // software interrupt-pending bit in supervisor mode
  val MSIE = RegInit(false.B)  // software interrupt-enable bit in machine mode
  val HSIE = false.B           // software interrupt-enable bit in hypervisor mode
  val SSIE = false.B           // software interrupt-enable bit in supervisor mode
  val mip = Cat(0.U((xlen - 8).W), MTIP, HTIP, STIP, false.B, MSIP, HSIP, SSIP, false.B)
  val mie = Cat(0.U((xlen - 8).W), MTIE, HTIE, STIE, false.B, MSIE, HSIE, SSIE, false.B)

  val mscratch = Reg(UInt(xlen.W))

  // registers that record information of the current exception
  val mepc     = Reg(UInt(xlen.W))  // When a trap is taken, it is written with the virtual address of the instruction that encountered the exception.
  val mcause   = Reg(UInt(xlen.W))  // record the cause of the exception
  val mbadaddr = Reg(UInt(xlen.W))  // the faulting address that causes address related exception

  // communicate with host
  val mtohost   = RegInit(0.U(xlen.W))
  val mfromhost = Reg(UInt(xlen.W))

  // mapping from CSR addresses to CSR registers
  val csrFile = Seq(
    BitPat(CSR.cycle)     -> cycle,
    BitPat(CSR.time)      -> time,
    BitPat(CSR.instret)   -> instret,
    BitPat(CSR.cycleh)    -> cycleh,
    BitPat(CSR.timeh)     -> timeh,
    BitPat(CSR.instreth)  -> instreth,
    BitPat(CSR.cyclew)    -> cycle,
    BitPat(CSR.timew)     -> time,
    BitPat(CSR.instretw)  -> instret,
    BitPat(CSR.cyclehw)   -> cycleh,
    BitPat(CSR.timehw)    -> timeh,
    BitPat(CSR.instrethw) -> instreth,
    BitPat(CSR.mcpuid)    -> mcpuid,
    BitPat(CSR.mimpid)    -> mimpid,
    BitPat(CSR.mhartid)   -> mhartid,
    BitPat(CSR.mstatus)   -> mstatus,
    BitPat(CSR.mtvec)     -> mtvec,
    BitPat(CSR.mtdeleg)   -> mtdeleg,
    BitPat(CSR.mie)       -> mie,
    BitPat(CSR.mtimecmp)  -> mtimecmp,
    BitPat(CSR.mtime)     -> time,
    BitPat(CSR.mtimeh)    -> timeh,
    BitPat(CSR.mscratch)  -> mscratch,
    BitPat(CSR.mepc)      -> mepc,
    BitPat(CSR.mcause)    -> mcause,
    BitPat(CSR.mbadaddr)  -> mbadaddr,
    BitPat(CSR.mip)       -> mip,
    BitPat(CSR.mtohost)   -> mtohost,
    BitPat(CSR.mfromhost) -> mfromhost
  )

  //--------------------------------------------------------
  // exception checking

  // check if the current privilege level has the access right to the requested CSR register
  val privValid = csr_addr(9, 8) <= PRV

  // if it is a privileged instruction (ECALL EBREAK or ERET), check which one it is
  val privInst = io.cmd === CSR.P
  val isEcall  = privInst && !csr_addr(0) && !csr_addr(8)
  val isEbreak = privInst && csr_addr(0) && !csr_addr(8)
  val isEret   = privInst && !csr_addr(0) && csr_addr(8)

  // check if the requested CSR register is one of our implemented ones
  val csrValid = csrFile.map(_._1 === csr_addr).reduce(_ || _)

  // check if the requested CSR register is a read-only one
  val csrRO = csr_addr(11, 10).andR || csr_addr === CSR.mtvec || csr_addr === CSR.mtdeleg

  // compute the data that is going to be written into CSR register
  val wen = io.cmd === CSR.W || io.cmd(1) && rs1_addr.orR
  val wdata = MuxLookup(io.cmd, 0.U)(
    Seq(
      CSR.W -> io.in,
      CSR.S -> (io.out | io.in),
      CSR.C -> (io.out & ~io.in)
    )
  )

  // check if the currently executing instruction address is a valid one, this is only done when executing a JAL or JALR instruction
  val iaddrInvalid = io.pc_check && io.addr(1)

  // chech if the computed load address is valid (aligned), this is only done when executing a load instruction
  val laddrInvalid = MuxLookup(io.ld_type, false.B)(
    Seq(
      Control.LD_LW  -> io.addr(1, 0).orR,
      Control.LD_LH  -> io.addr(0),
      Control.LD_LHU -> io.addr(0)
    )
  )

  // chech if the computed store address is valid (aligned), this is only done when executing a store instruction
  val saddrInvalid = MuxLookup(io.st_type, false.B)(
    Seq(
      Control.ST_SW -> io.addr(1, 0).orR,
      Control.ST_SH -> io.addr(0)
    )
  )

  //--------------------------------------------------------
  // increment counters

  time := time + 1.U
  when(time.andR) { timeh := timeh + 1.U }

  cycle := cycle + 1.U
  when(cycle.andR) { cycleh := cycleh + 1.U }

  val isInstRet = io.inst =/= Instructions.NOP && (!io.expt || isEcall || isEbreak) && !io.stall
  when(isInstRet) { instret := instret + 1.U }
  when(isInstRet && instret.andR) { instreth := instreth + 1.U }

  //--------------------------------------------------------
  // io

  // communicate with host
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }

  // exception
  io.expt := io.illegal || iaddrInvalid || laddrInvalid || saddrInvalid || io.cmd(1, 0).orR && (!csrValid || !privValid) || wen && csrRO || (privInst && !privValid) || isEcall || isEbreak
  io.evec := mtvec + (PRV << 6)
  io.epc  := mepc

  // read CSR registers
  io.out := Lookup(csr_addr, 0.U, csrFile).asUInt

  // write CSR registers
  when(!io.stall) {
    when(io.expt) {
      mepc := io.pc >> 2 << 2
      mcause := Mux(
        iaddrInvalid, Cause.InstAddrMisaligned,
        Mux(
          laddrInvalid, Cause.LoadAddrMisaligned,
          Mux(
            saddrInvalid, Cause.StoreAddrMisaligned,
            Mux(
              isEcall, Cause.Ecall + PRV,
              Mux(
                isEbreak, Cause.Breakpoint,
                Cause.IllegalInst
              )
            )
          )
        )
      )
      PRV  := CSR.PRV_M
      IE   := false.B
      PRV1 := PRV
      IE1  := IE
      when(iaddrInvalid || laddrInvalid || saddrInvalid) { mbadaddr := io.addr }
    }.elsewhen(isEret) {
      PRV  := PRV1
      IE   := IE1
      PRV1 := CSR.PRV_U
      IE1  := true.B
    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) {
        PRV1 := wdata(5, 4)
        IE1 := wdata(3)
        PRV := wdata(2, 1)
        IE := wdata(0)
      }.elsewhen(csr_addr === CSR.mip) {
        MTIP := wdata(7)
        MSIP := wdata(3)
      }.elsewhen(csr_addr === CSR.mie) {
        MTIE := wdata(7)
        MSIE := wdata(3)
      }
      .elsewhen(csr_addr === CSR.mtime)     { time := wdata }
      .elsewhen(csr_addr === CSR.mtimeh)    { timeh := wdata }
      .elsewhen(csr_addr === CSR.mtimecmp)  { mtimecmp := wdata }
      .elsewhen(csr_addr === CSR.mscratch)  { mscratch := wdata }
      .elsewhen(csr_addr === CSR.mepc)      { mepc := wdata >> 2.U << 2.U }
      .elsewhen(csr_addr === CSR.mcause)    { mcause := wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
      .elsewhen(csr_addr === CSR.mbadaddr)  { mbadaddr := wdata }
      .elsewhen(csr_addr === CSR.mtohost)   { mtohost := wdata }
      .elsewhen(csr_addr === CSR.mfromhost) { mfromhost := wdata }
      .elsewhen(csr_addr === CSR.cyclew)    { cycle := wdata }
      .elsewhen(csr_addr === CSR.timew)     { time := wdata }
      .elsewhen(csr_addr === CSR.instretw)  { instret := wdata }
      .elsewhen(csr_addr === CSR.cyclehw)   { cycleh := wdata }
      .elsewhen(csr_addr === CSR.timehw)    { timeh := wdata }
      .elsewhen(csr_addr === CSR.instrethw) { instreth := wdata }
    }
  }
}
