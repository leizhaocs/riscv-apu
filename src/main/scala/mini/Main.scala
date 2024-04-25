package mini

import circt.stage.ChiselStage

object Main extends App {
  // configuration of the CPU
  val config = MiniConfig()

  // emit systemverolog file
  ChiselStage.emitSystemVerilogFile(
    new Tile(
      coreParams  = config.core,
      nastiParams = config.nasti,
      cacheParams = config.cache
    ),
    args
  )
}
