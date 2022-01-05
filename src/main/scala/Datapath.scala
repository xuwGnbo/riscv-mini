// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

object Const {
  val PC_START = 0x200
  val PC_EVEC  = 0x100
}

class DatapathIO(implicit p: Parameters) extends CoreBundle()(p) {
  val host = new HostIO
  val icache = Flipped(new CacheIO)
  val dcache = Flipped(new CacheIO)
  val ctrl = Flipped(new ControlSignals)
}

class Datapath(implicit val p: Parameters) extends Module with CoreParams {
  val io      = IO(new DatapathIO)
  val csr     = Module(new CSR)
  val regFile = Module(new RegFile) 
  val alu     = p(BuildALU)(p)
  val immGen  = p(BuildImmGen)(p)
  val brCond  = p(BuildBrCond)(p)

  import Control._

  // /***** Fetch / Execute Registers *****/
  // val fe_inst = RegInit(Instructions.NOP)
  // val fe_pc   = Reg(UInt())

  // /***** Execute / Write Back Registers *****/
  // val ew_inst = RegInit(Instructions.NOP) 
  // val ew_pc   = Reg(UInt())
  // val ew_alu  = Reg(UInt())
  // val csr_in  = Reg(UInt())

  // /****** Control signals *****/
  // val st_type  = Reg(io.ctrl.st_type.cloneType)
  // val ld_type  = Reg(io.ctrl.ld_type.cloneType)
  // val wb_sel   = Reg(io.ctrl.wb_sel.cloneType)
  // val wb_en    = Reg(Bool())
  // val csr_cmd  = Reg(io.ctrl.csr_cmd.cloneType)
  // val illegal  = Reg(Bool())
  // val pc_check = Reg(Bool())

  // ===========================================================================
  val ID_pc         = Reg(UInt())                    // fe_pc
  val ID_inst       = RegInit(Instructions.NOP)      // fe_inst

  // ---------------------------------------------------------------------------
  val EXE_pc        = Reg(UInt())
  val EXE_inst      = RegInit(Instructions.NOP)

  val EXE_rs1       = Reg(UInt(xlen.W))
  val EXE_rs2       = Reg(UInt(xlen.W))
  val EXE_immout    = Reg(UInt(xlen.W))
  val EXE_rs1_addr  = Reg(UInt(5.W))
  val EXE_rs2_addr  = Reg(UInt(5.W))

  val EXE_br_type   = Reg(UInt(3.W))

  val EXE_alu_op    = Reg(UInt(4.W))
  val EXE_a_sel     = Reg(UInt(1.W))
  val EXE_b_sel     = Reg(UInt(1.W))
  val EXE_wb_sel    = Reg(UInt(2.W))
  val EXE_wb_en     = Reg(Bool())

  val EXE_csr_in    = Reg(UInt(xlen.W))
  val EXE_st_type   = Reg(io.ctrl.st_type.cloneType)
  val EXE_ld_type   = Reg(io.ctrl.ld_type.cloneType)
  val EXE_csr_cmd   = Reg(io.ctrl.csr_cmd.cloneType)
  val EXE_illegal   = Reg(Bool())
  val EXE_pc_check  = Reg(Bool())

  // ---------------------------------------------------------------------------
  val MEM_pc        = Reg(UInt())
  val MEM_inst      = RegInit(Instructions.NOP)

  val MEM_alu       = Reg(UInt(xlen.W))
  val MEM_sum       = Reg(UInt(xlen.W))
  val MEM_rs2       = Reg(UInt(xlen.W))

  val MEM_wb_sel    = Reg(UInt(2.W))
  val MEM_wb_en     = Reg(Bool())
  
  val MEM_csr_in    = Reg(UInt(xlen.W))
  val MEM_st_type   = Reg(io.ctrl.st_type.cloneType)
  val MEM_ld_type   = Reg(io.ctrl.ld_type.cloneType)
  val MEM_csr_cmd   = Reg(io.ctrl.csr_cmd.cloneType)
  val MEM_illegal   = Reg(Bool())
  val MEM_pc_check  = Reg(Bool())

  // ---------------------------------------------------------------------------
  val WB_pc         = Reg(UInt())                    //
  val WB_inst       = RegInit(Instructions.NOP)      //

  val WB_alu        = Reg(UInt(xlen.W))

  val WB_wb_sel     = Reg(UInt(2.W))
  val WB_wb_en      = Reg(Bool())

  val WB_data       = Reg(UInt(xlen.W))

  val WB_csr_in     = Reg(UInt(xlen.W))
  val WB_st_type    = Reg(io.ctrl.st_type.cloneType)
  val WB_ld_type    = Reg(io.ctrl.ld_type.cloneType)
  val WB_csr_cmd    = Reg(io.ctrl.csr_cmd.cloneType)
  val WB_illegal    = Reg(Bool())
  val WB_pc_check   = Reg(Bool())

  // =======================================================================  IF
  val started = RegNext(reset.toBool)
  val stall = !io.icache.resp.valid || !io.dcache.resp.valid
  val pc   = RegInit(Const.PC_START.U(xlen.W) - 4.U(xlen.W))
  val npc  = Mux(stall, pc, Mux(csr.io.expt, csr.io.evec,
             Mux(io.ctrl.pc_sel === PC_EPC,  csr.io.epc,
             Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U,
             Mux(io.ctrl.pc_sel === PC_0, pc, pc + 4.U)))))
  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.resp.bits.data)
  pc                      := npc 
  io.icache.req.bits.addr := npc
  io.icache.req.bits.data := 0.U
  io.icache.req.bits.mask := 0.U
  io.icache.req.valid     := !stall
  io.icache.abort         := false.B
 
  // ------------------------------ IF/ID pipelining ---------------------------
  when (!stall) {
    ID_pc   := pc
    ID_inst := inst
  }

  // ======================================================================== ID
  io.ctrl.inst  := _id_inst

  // regFile read
  val rd_addr  = ID_inst(11, 7)
  val rs1_addr = ID_inst(19, 15)
  val rs2_addr = ID_inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  immGen.io.inst := ID_inst
  immGen.io.sel  := io.ctrl.imm_sel

  // -------------------------------------------------------------------  bypass
  // val MEM_rd_addr = MEM_inst(11, 7)
  // val WB_rd_addr  = WB_inst(11, 7)

  // val id_rs1_hazard = rs1_addr === regFile.io.waddr
  // val id_rs2_hazard = rs2_addr === regFile.io.waddr
  // val rs1 = Mux(id_rs1_hazard, regFile.io.wdata, regFile.io.rdata1)
  // val rs2 = Mux(id_rs2_hazard, regFile.io.wdata, regFile.io.rdata2)
  // 冒险检测单元 (load 指令优化)
  // ID_rs1_ld_hazard := (EXE_ld_type === LD_XXX) && (rs1_addr === MEM_rd_addr)
  // ID_rs2_ld_hazard := (EXE_ld_type === LD_XXX) && (rs2_addr === MEM_rd_addr)

  // val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  // val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  // val rs1 = Mux(wb_sel === WB_ALU && rs1hazard, ew_alu, regFile.io.rdata1) 
  // val rs2 = Mux(wb_sel === WB_ALU && rs2hazard, ew_alu, regFile.io.rdata2)
  
  // no bypass, 将在 EXE阶段处理
  val rs1 = regFile.io.rdata1
  val rs2 = regFile.io.rdata2

  // ------------------------------ ID/EXE pipelining --------------------------
  when (reset.toBool || !stall && csr.io.expt) {
    EXE_st_type   := 0.U
    EXE_ld_type   := 0.U
    EXE_wb_en     := false.B
    EXE_csr_cmd   := 0.U
    EXE_illegal   := false.B
    EXE_pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    EXE_pc        := ID_pc
    EXE_inst      := ID_inst

    EXE_rs1       := rs1
    EXE_rs2       := rs2
    EXE_immout    := immGen.io.out
    EXE_rs1_addr  := rs1_addr         //- bypass
    EXE_rs2_addr  := rs2_addr         //- bypass

    EXE_br_type   := io.ctrl.br_type

    EXE_alu_op    := io.ctrl.alu_op   //- ALU Op
    EXE_a_sel     := io.ctrl.A_sel    //- ALU Src
    EXE_b_sel     := io.ctrl.B_sel    //- ALU Src
    EXE_wb_sel    := io.ctrl.wb_sel   //---- 写寄存器来源（不是从内存读出来的）
    EXE_wb_en     := io.ctrl.wb_en    //---- 写寄存器

    EXE_csr_in    := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
    EXE_st_type   := io.ctrl.st_type
    EXE_ld_type   := io.ctrl.ld_type
    EXE_csr_cmd   := io.ctrl.csr_cmd
    EXE_illegal   := io.ctrl.illegal
    EXE_pc_check  := io.ctrl.pc_sel === PC_ALU
  }

  // ======================================================================= EXE
  val MEM_rd_addr = MEM_inst(11, 7)  // 流水线中没有存储 rd_addr
  val WB_rd_addr  = WB_inst(11, 7)
  // .orR 表示不等于0
  val mem2exe_rs1_bypass = MEM_wb_en && EXE_rs1_addr.orR && (EXE_rs1_addr == MEM_rd_addr)
  val mem2exe_rs2_bypass = MEM_wb_en && EXE_rs2_addr.orR && (EXE_rs2_addr == MEM_rd_addr)
  val wb2exe_rs1_bypass  = WB_wb_en  && EXE_rs1_addr.orR && (EXE_rs1_addr == WB_rd_addr)
  val wb2exe_rs2_bypass  = WB_wb_en  && EXE_rs2_addr.orR && (EXE_rs2_addr == WB_rd_addr)

  // MEM级(新) 的结果要优先于 WB级(旧)
  // !注意 要有 MEM_wb_sel === WB_ALU, 计系3可能有点遗漏
  val _rs1 = Mux(MEM_wb_sel === WB_ALU && mem2exe_rs1_bypass, MEM_alu,
             Mux(wb2exe_rs1_bypass, WB_data, EXE_rs1))
  val _rs2 = Mux(MEM_wb_sel === WB_ALU && mem2exe_rs2_bypass, MEM_alu,
             Mux(wb2exe_rs2_bypass, WB_data, EXE_rs2))

  // ALU operations
  alu.io.A := Mux(EXE_a_sel === A_RS1, EXE_rs1, EXE_pc)
  alu.io.B := Mux(EXE_b_sel === B_RS2, EXE_rs2, EXE_immout)
  alu.io.alu_op := EXE_alu_op

  // Branch condition calc
  brCond.io.rs1 := _rs1
  brCond.io.rs2 := _rs2
  brCond.io.br_type := EXE_br_type

  // ------------------------------ EXE/MEM pipelining -------------------------
  when (reset.toBool || !stall && csr.io.expt) {
    MEM_st_type   := 0.U
    MEM_ld_type   := 0.U
    MEM_wb_en     := false.B
    MEM_csr_cmd   := 0.U
    MEM_illegal   := false.B
    MEM_pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    MEM_pc        := EXE_pc
    MEM_inst      := EXE_inst

    MEM_alu       := alu.io.out
    MEM_sum       := alu.io.sum
    MEM_rs2       := _rs2      //- data for store

    MEM_wb_sel    := EXE_wb_sel
    MEM_wb_en     := EXE_wb_en

    MEM_csr_in    := EXE_csr_in
    MEM_st_type   := EXE_st_type
    MEM_ld_type   := EXE_ld_type
    MEM_csr_cmd   := EXE_csr_cmd
    MEM_illegal   := EXE_illegal
    MEM_pc_check  := EXE_pc_check
  }

  // ======================================================================= MEM
  // D$ access
  val daddr   = Mux(stall, WB_alu, MEM_sum) >> 2.U << 2.U        // ?ew_alu >> wb_alu ??!
  val woffset = MEM_sum(1) << 4.U | MEM_sum(0) << 3.U
  io.dcache.req.valid     := !stall && (MEM_st_type.orR || MEM_ld_type.orR)
  io.dcache.req.bits.addr := daddr 
  io.dcache.req.bits.data := MEM_rs2 << woffset
  // io.dcache.req.bits.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), // !!
  io.dcache.req.bits.mask := MuxLookup(Mux(stall, WB_st_type, MEM_st_type), // 发生错误，使用上一条指令
              "b0000".U, Seq(
    ST_SW ->  "b1111".U,
    ST_SH -> ("b11".U << MEM_sum(1,0)),
    ST_SB -> ("b1".U  << MEM_sum(1,0))))
  
  // // Pipelining
  // when(reset.toBool || !stall && csr.io.expt) {
  //   st_type   := 0.U
  //   ld_type   := 0.U
  //   wb_en     := false.B
  //   csr_cmd   := 0.U
  //   illegal   := false.B
  //   pc_check  := false.B
  // }.elsewhen(!stall && !csr.io.expt) {
  //   ew_pc     := fe_pc
  //   ew_inst   := fe_inst
  //   // ew_alu    := alu.io.out
  //   ew_alu    := Mux(io.ctrl.mdu_op === MDU.MDU_XXX, alu.io.out, mdu.io.out)
  //   csr_in    := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
  //   st_type   := io.ctrl.st_type
  //   ld_type   := io.ctrl.ld_type
  //   wb_sel    := io.ctrl.wb_sel
  //   wb_en     := io.ctrl.wb_en
  //   csr_cmd   := io.ctrl.csr_cmd
  //   illegal   := io.ctrl.illegal
  //   pc_check  := io.ctrl.pc_sel === PC_ALU
  // }

  // ------------------------------ MEM/WB pipelining --------------------------
  when(reset.toBool || !stall && csr.io.expt) {
    WB_st_type    := 0.U
    WB_ld_type    := 0.U
    WB_wb_en      := false.B
    WB_csr_cmd    := 0.U
    WB_illegal    := false.B
    WB_pc_check   := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    WB_pc         := MEM_pc
    WB_inst       := MEM_inst

    WB_alu        := MEM_alu

    WB_wb_sel     := MEM_wb_sel
    WB_wb_en      := MEM_wb_en

    WB_csr_in     := MEM_csr_in
    WB_st_type    := MEM_st_type
    WB_ld_type    := MEM_ld_type
    WB_csr_cmd    := MEM_csr_cmd
    WB_illegal    := MEM_illegal
    WB_pc_check   := MEM_pc_check
  }

  // ======================================================================== WB
  // Load
  val loffset = WB_alu(1) << 4.U | WB_alu(0) << 3.U
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    = MuxLookup(WB_ld_type, io.dcache.resp.bits.data.zext, Seq(
    LD_LH  -> lshift(15, 0).asSInt, LD_LB  -> lshift(7, 0).asSInt,
    LD_LHU -> lshift(15, 0).zext,   LD_LBU -> lshift(7, 0).zext) )
    
  // CSR access
  csr.io.stall    := stall
  csr.io.in       := WB_csr_in   // csr_in
  csr.io.cmd      := WB_csr_cmd  // csr_cmd
  csr.io.inst     := WB_inst     // ew_inst
  csr.io.pc       := WB_pc       // ew_pc
  csr.io.addr     := WB_alu      // ew_alu
  csr.io.illegal  := WB_illegal  // illegal
  csr.io.pc_check := WB_pc_check // pc_check
  csr.io.ld_type  := WB_ld_type  // ld_type
  csr.io.st_type  := WB_st_type  // st_type
  io.host <> csr.io.host 

  // Regfile Write
  val regWrite = MuxLookup(WB_wb_sel, WB_alu.zext, Seq(
    WB_MEM -> load,
    WB_PC4 -> (WB_pc + 4.U).zext,
    WB_CSR -> csr.io.out.zext) ).asUInt 

  regFile.io.wen   := WB_wb_en && !stall && !csr.io.expt 
  regFile.io.waddr := WB_rd_addr //! 就离谱                                          // !
  regFile.io.wdata := regWrite
  WB_data          := regWrite // 流水线旁路 !!?

  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.expt

  if (p(Trace)) {
    when(regFile.io.wen) {
      printf("PC: %x, INST: %x, REG[%d] <- %x\n", WB_pc, WB_inst,
        Mux(regFile.io.wen, wb_rd_addr, 0.U),                             // !
        Mux(regFile.io.wen, regFile.io.wdata, 0.U))
    }
  }
}
