package mini

import chisel3._
import chisel3.util._
import junctions._

/* IO of arbiter */
class MemArbiterIO(params: NastiBundleParameters) extends Bundle {
  val icache = Flipped(new NastiBundle(params))  // connect to icache          [aw, w, b, ar, r]
  val dcache = Flipped(new NastiBundle(params))  // connect to dcache          [aw, w, b, ar, r]
  val nasti  = new NastiBundle(params)           // connect to lower level mem [aw, w, b, ar, r]
}

/* arbiter states */
object MemArbiterState extends ChiselEnum {
  val sIdle, sICacheRead, sDCacheRead, sDCacheWrite, sDCacheAck = Value
}

/* arbiter that communicate between icache/dcache and lower level mem */
class MemArbiter(params: NastiBundleParameters) extends Module {
  // IO
  val io = IO(new MemArbiterIO(params))

  // state register
  val state = RegInit(MemArbiterState.sIdle)

  //============================================================================================================
  // dcache writes to lower level mem

  // send write address to lower level mem
  io.nasti.aw.bits   := io.dcache.aw.bits                                      // send write address from dcache to lower level mem
  io.nasti.aw.valid  := io.dcache.aw.valid && state === MemArbiterState.sIdle  // tell lower level mem that write address is available
  io.dcache.aw.ready := io.nasti.aw.ready && state === MemArbiterState.sIdle   // tell dcache that lower level mem is ready to receive write address
  io.icache.aw       := DontCare                                               // icache never writes to lower level mem

  // send write data to lower level mem
  io.nasti.w.bits   := io.dcache.w.bits                                             // send write data from dcache to lower level mem
  io.nasti.w.valid  := io.dcache.w.valid && state === MemArbiterState.sDCacheWrite  // tell lower level mem that write data is available
  io.dcache.w.ready := io.nasti.w.ready && state === MemArbiterState.sDCacheWrite   // tell dcache that lower level mem is ready to receive write data
  io.icache.w       := DontCare                                                     // icache never writes to lower level mem

  // send write ack to icache/dcache
  io.dcache.b.bits  := io.nasti.b.bits                                            // send write ack to dcache
  io.dcache.b.valid := io.nasti.b.valid && state === MemArbiterState.sDCacheAck   // tell dcache that write ack is available
  io.nasti.b.ready  := io.dcache.b.ready && state === MemArbiterState.sDCacheAck  // tell lower levem mem that dcache is ready to receive write ack
  io.icache.b       := DontCare                                                   // icache never writes to lower level mem

  //============================================================================================================
  // icache/dcache reads from lower level mem

  // send read address to lower level mem
  io.nasti.ar.bits := NastiAddressBundle(params)(                                                   // send read address to lower level mem, choose from icache and dcache
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.id,   io.icache.ar.bits.id),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.addr, io.icache.ar.bits.addr),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.size, io.icache.ar.bits.size),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.len,  io.icache.ar.bits.len)
  )
  io.nasti.ar.valid  := (io.icache.ar.valid || io.dcache.ar.valid) &&                               // tell lower level mem that read address is available
                        !io.nasti.aw.valid && state === MemArbiterState.sIdle
  io.dcache.ar.ready := io.nasti.ar.ready && !io.nasti.aw.valid && state === MemArbiterState.sIdle  // tell dcache that lower level mem is ready to receive read address
  io.icache.ar.ready := io.dcache.ar.ready && !io.dcache.ar.valid                                   // tell icache that lower level mem is ready to receive read address

  // read data from lower level mem
  io.icache.r.bits  := io.nasti.r.bits                                                // send data to icache
  io.dcache.r.bits  := io.nasti.r.bits                                                // send data to dcache
  io.icache.r.valid := io.nasti.r.valid && state === MemArbiterState.sICacheRead      // tell icache that data is available
  io.dcache.r.valid := io.nasti.r.valid && state === MemArbiterState.sDCacheRead      // tell dcache that data is available
  io.nasti.r.ready  := io.icache.r.ready && state === MemArbiterState.sICacheRead ||  // tell lower level mem that icache or dcache is ready to receive data
                       io.dcache.r.ready && state === MemArbiterState.sDCacheRead

  //============================================================================================================
  // state transition, need to implement this when adding lower level mem outside the CPU

  switch(state) {
    is(MemArbiterState.sIdle) {
      when(io.dcache.aw.fire) {
        state := MemArbiterState.sDCacheWrite
      }.elsewhen(io.dcache.ar.fire) {
        state := MemArbiterState.sDCacheRead
      }.elsewhen(io.icache.ar.fire) {
        state := MemArbiterState.sICacheRead
      }
    }
    is(MemArbiterState.sICacheRead) {
      when(io.nasti.r.fire && io.nasti.r.bits.last) {
        state := MemArbiterState.sIdle
      }
    }
    is(MemArbiterState.sDCacheRead) {
      when(io.nasti.r.fire && io.nasti.r.bits.last) {
        state := MemArbiterState.sIdle
      }
    }
    is(MemArbiterState.sDCacheWrite) {
      when(io.dcache.w.fire && io.dcache.w.bits.last) {
        state := MemArbiterState.sDCacheAck
      }
    }
    is(MemArbiterState.sDCacheAck) {
      when(io.nasti.b.fire) {
        state := MemArbiterState.sIdle
      }
    }
  }
}

/* IO of the CPU */
class TileIO(xlen: Int, nastiParams: NastiBundleParameters) extends Bundle {
  val host  = new HostIO(xlen)              //
  val nasti = new NastiBundle(nastiParams)  //
}

object Tile {
  def apply(config: Config): Tile = new Tile(config.core, config.nasti, config.cache)
}

/* a tile is actually a CPU */
class Tile(val coreParams: CoreConfig, val nastiParams: NastiBundleParameters, val cacheParams: CacheConfig) extends Module {
  // IO
  val io = IO(new TileIO(coreParams.xlen, nastiParams))

  // all components
  val core   = Module(new Core(coreParams))
  val icache = Module(new Cache(cacheParams, nastiParams, coreParams.xlen))
  val dcache = Module(new Cache(cacheParams, nastiParams, coreParams.xlen))
  val arb    = Module(new MemArbiter(nastiParams))

  // connect to outside world
  io.host  <> core.io.host
  io.nasti <> arb.io.nasti

  // connect with the CPU
  core.io.icache <> icache.io.cpu    // core can directly access icache
  core.io.dcache <> dcache.io.cpu    // core can directly access dcache
  arb.io.icache  <> icache.io.nasti  // icache comminicate with outside world through arbiter (with NASTI inferface)
  arb.io.dcache  <> dcache.io.nasti  // dcache comminicate with outside world through arbiter (with NASTI inferface)

}
