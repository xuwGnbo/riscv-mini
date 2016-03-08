package mini

import Chisel._
import cde.Parameters
import junctions._

class MemArbiter(implicit p: Parameters) extends Module {
  val io = new ParameterizedBundle {
    val icache = (new NastiIO).flip
    val dcache = (new NastiIO).flip
    val nasti  =  new NastiIO
  }

  val s_IDLE :: s_ICACHE_READ :: s_DCACHE_READ :: s_DCACHE_WRITE :: Nil = Enum(UInt(), 4)
  val state = RegInit(s_IDLE)

  // Write Address
  io.nasti.aw.bits := io.dcache.aw.bits
  io.nasti.aw.valid := io.dcache.aw.valid && state === s_IDLE
  io.dcache.aw.ready := io.nasti.aw.ready && state === s_IDLE

  // Write Data 
  io.nasti.w.bits  := io.dcache.w.bits
  io.nasti.w.valid := io.dcache.w.valid && state === s_DCACHE_WRITE
  io.dcache.w.ready := io.nasti.w.ready && state === s_DCACHE_WRITE
  io.nasti.b.ready := Bool(true)

  // Read Address
  io.nasti.ar.bits := NastiReadAddressChannel(
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.id,   io.icache.ar.bits.id),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.addr, io.icache.ar.bits.addr),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.size, io.icache.ar.bits.size),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.len,  io.icache.ar.bits.len))
  io.nasti.ar.valid := (io.icache.ar.valid || io.dcache.ar.valid) && 
    !io.nasti.aw.valid && state === s_IDLE
  io.dcache.ar.ready := io.nasti.ar.ready && !io.nasti.aw.valid && state === s_IDLE
  io.icache.ar.ready := io.dcache.ar.ready && !io.dcache.ar.valid

  // Read Data
  io.icache.r.bits  := io.nasti.r.bits
  io.dcache.r.bits  := io.nasti.r.bits
  io.icache.r.valid := io.nasti.r.valid && state === s_ICACHE_READ
  io.dcache.r.valid := io.nasti.r.valid && state === s_DCACHE_READ
  io.nasti.r.ready := io.icache.r.ready && state === s_ICACHE_READ || 
                      io.dcache.r.ready && state === s_DCACHE_READ

  switch(state) {
    is(s_IDLE) {
      when(io.dcache.aw.fire()) {
        state := s_DCACHE_WRITE
      }.elsewhen(io.dcache.ar.fire()) {
        state := s_DCACHE_READ
      }.elsewhen(io.icache.ar.fire()) {
        state := s_ICACHE_READ
      }
    }
    is(s_ICACHE_READ) {
      when(io.nasti.r.fire() && io.nasti.r.bits.last) {
        state := s_IDLE
      }
    }
    is(s_DCACHE_READ) {
      when(io.nasti.r.fire() && io.nasti.r.bits.last) {
        state := s_IDLE
      }
    }
    is(s_DCACHE_WRITE) {
      when(io.dcache.w.fire() && io.dcache.w.bits.last) {
        state := s_IDLE
      }
    }
  }
}

class HTIFIO(implicit p: Parameters) extends ParameterizedBundle {
  val host = new HostIO
}

class TileIO(implicit p: Parameters) extends ParameterizedBundle {
  val htif  = new HTIFIO
  val mem   = new MemIO
  val nasti = new NastiIO
}

class Tile(implicit val p: Parameters) extends Module with CacheParams {
  val io     = new TileIO
  val core   = Module(new Core)
  val icache = Module(new Cache)
  val dcache = Module(new Cache)
  val arb    = Module(new MemArbiter)
  
  io.htif.host <> core.io.host
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  arb.io.icache <> icache.io.nasti
  arb.io.dcache <> dcache.io.nasti
  
  if (core.useNasti) {
    io.nasti <> arb.io.nasti
  } else {
    val conv = Module(new MemIONastiIOConverter(blen))
    conv.io.nasti <> arb.io.nasti
    io.mem <> conv.io.mem
  }
}