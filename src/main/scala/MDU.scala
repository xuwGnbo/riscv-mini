package mini

import chisel3._
import chisel3.util._

object MDU {
  val MDU_XXX = 0.U(3.W)
  val MDU_MUL = 1.U(3.W)
}

import MDU._

class MDUIo extends Bundle {
  val mdu_op = Input(UInt(3.W))
  val rs1    = Input(UInt(32.W))
  val rs2    = Input(UInt(32.W))
  val out    = Output(UInt(32.W))
}

class MDU extends Module{
  val io = IO(new MDUIo)

  io.out := MuxLookup(io.mdu_op, 0.U, Seq(
    MDU_MUL -> (io.rs1.asSInt() * io.rs2.asSInt()).asUInt()
  ))
}
