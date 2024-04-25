package mini

import junctions.NastiBundleParameters

case class Config(core: CoreConfig, cache: CacheConfig, nasti: NastiBundleParameters)

object MiniConfig {
  def apply(): Config = {
    // we are designing 32-bit CPU
    val xlen = 32

    Config(
      // configuration for core
      core = CoreConfig(
        xlen       = xlen,           // bit width (32-bit or 64-bit)
        makeAlu    = new Alu(_),     // ALU
        makeBrCond = new BrCond(_),  // brand condition
        makeImmGen = new ImmGen(_)   // immediate generator
      ),

      // configuration for cache
      cache = CacheConfig(
        nWays      = 1,              // namely direct mapped cache
        nSets      = 256,            // number of cache sets
        blockBytes = 4 * (xlen / 8)  // block size in bytes, i.e., 4 words, 4 * 32 bits = 16B
      ),

      // configuration for NOC
      nasti = NastiBundleParameters(
        addrBits = 32,
        dataBits = 64,
        idBits   = 5
      )
    )
  }
}
