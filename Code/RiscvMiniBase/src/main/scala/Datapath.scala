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
  val io   		= IO(new DatapathIO)
  val csr  		= Module(new CSR)
  val regFile 	= Module(new RegFile) 
  val alu     	= p(BuildALU)(p)
  val dispatch 	= Module(new ALU_Dispatcher)
  val immGen  	= p(BuildImmGen)(p)
  val brCond  	= p(BuildBrCond)(p)


  import Control._

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(Instructions.NOP)
  val fe_pc   = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = RegInit(Instructions.NOP) 
  val ew_pc   = Reg(UInt())
  val ew_alu  = Reg(UInt())
  val csr_in  = Reg(UInt())

  /****** Control signals *****/
  val st_type  = Reg(io.ctrl.st_type.cloneType)	//UInt 2bits long
  val ld_type  = Reg(io.ctrl.ld_type.cloneType)	//UInt 3bits long
  val wb_sel   = Reg(io.ctrl.wb_sel.cloneType)	//UInt 2bits long
  val wb_en    = Reg(Bool())
  val csr_cmd  = Reg(io.ctrl.csr_cmd.cloneType)	//UInt 3bits long
  val illegal  = Reg(Bool())
  val pc_check = Reg(Bool())
 
  /****** Fetch *****/
  val started = RegNext(reset.toBool) //reg initialized to 1bit reset signal 
  val stall = !io.icache.resp.valid || !io.dcache.resp.valid //none of the valid signal of the caches are high
  val pc   = RegInit(Const.PC_START.U(xlen.W) - 4.U(xlen.W)) //PC = PC_START.toUIntOn(xlen_Bits) - 4 => why -4
  val npc  = Mux(stall, pc, Mux(csr.io.expt, csr.io.evec,
             Mux(io.ctrl.pc_sel === PC_EPC,  csr.io.epc,
             Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U, //sum >> 1bit << 1bit => least significant bit is 0
             Mux(io.ctrl.pc_sel === PC_0, pc, pc + 4.U)))))
  //if stall, npc = pc => stalling implies redo the same instruction
  //else if exception (csr.io.expt), npc = csr.io.evec <= exception handler base address
  //else if PC is PC_EPC, npc == csr.io.epc <= exception program counter
  //else if PC is PC_ALU or BranchPredictor says it is a taken branch, npc = alu.io.sum (with the last bit put to 0 => even number)
  //else if PC is PC_0, npc = pc
  //else ncp = pc + 4bits //should be 4bytes -> 32bits isn't it ?

  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.resp.bits.data) 
  //the instruction to execute if there is one, NOP otherwise
  pc                      := npc //update pc to the new instaruction address
  io.icache.req.bits.addr := npc
  io.icache.req.bits.data := 0.U
  io.icache.req.bits.mask := 0.U
  io.icache.req.valid     := !stall
  io.icache.abort         := false.B
 
  // Pipelining
  when (!stall) {
    fe_pc   := pc
    fe_inst := inst
  }

  /****** Execute *****/
  // Decode
  io.ctrl.inst  := fe_inst

  // regFile read
  // Addressing in RISCV R-type 32bits instructions :
  // 31->25 funct7, 24->20 source register2, 19->15 source register1, 14->12 funct3, 11->7 destination register, 6->0 opcode (bounds included) 
  val rd_addr  = fe_inst(11, 7) 
  val rs1_addr = fe_inst(19, 15) 
  val rs2_addr = fe_inst(24, 20)
  //put operand addresses in regFile so the regFile outputs the corresponding operands rs1 and rs2
  regFile.io.raddr1 := rs1_addr 
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  immGen.io.inst := fe_inst
  immGen.io.sel  := io.ctrl.imm_sel

  // bypass write back to data cache if input is the output of the ALU
  val wb_rd_addr = ew_inst(11, 7)
  // we read data directly from the alu if wb_en, the address of the data is not the reg0 and
  // the data address correspond to the destination register of the previous computation
  val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  // operand might either be in the register ew_alu or in one of the register of the regFile
  val rs1 = Mux(wb_sel === WB_ALU && rs1hazard, ew_alu, regFile.io.rdata1) 
  val rs2 = Mux(wb_sel === WB_ALU && rs2hazard, ew_alu, regFile.io.rdata2)
 
  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1 
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  val daddr   = Mux(stall, ew_alu, alu.io.sum) >> 2.U << 2.U //last 2bits are zeros
  val woffset = alu.io.sum(1) << 4.U | alu.io.sum(0) << 3.U //second least significant bit shifted left by 4 OR first least significant bit shifted left by 3
  io.dcache.req.valid     := !stall && (io.ctrl.st_type.orR || io.ctrl.ld_type.orR)
  io.dcache.req.bits.addr := daddr 
  io.dcache.req.bits.data := rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), 
              "b0000".U, Seq(
    ST_SW ->  "b1111".U,
    ST_SH -> ("b11".U << alu.io.sum(1,0)),
    ST_SB -> ("b1".U  << alu.io.sum(1,0))))
  
  // Pipelining
  when(reset.toBool || !stall && csr.io.expt) {
    st_type   := 0.U
    ld_type   := 0.U
    wb_en     := false.B
    csr_cmd   := 0.U
    illegal   := false.B
    pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    ew_pc     := fe_pc
    ew_inst   := fe_inst
    ew_alu    := alu.io.out
    csr_in    := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
    st_type   := io.ctrl.st_type
    ld_type   := io.ctrl.ld_type
    wb_sel    := io.ctrl.wb_sel
    wb_en     := io.ctrl.wb_en
    csr_cmd   := io.ctrl.csr_cmd
    illegal   := io.ctrl.illegal
    pc_check  := io.ctrl.pc_sel === PC_ALU
  }

  // Load
  val loffset = ew_alu(1) << 4.U | ew_alu(0) << 3.U
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    = MuxLookup(ld_type, io.dcache.resp.bits.data.zext, Seq(
    LD_LH  -> lshift(15, 0).asSInt, LD_LB  -> lshift(7, 0).asSInt,
    LD_LHU -> lshift(15, 0).zext,   LD_LBU -> lshift(7, 0).zext) )
    
  // CSR access
  csr.io.stall    := stall
  csr.io.in       := csr_in
  csr.io.cmd      := csr_cmd
  csr.io.inst     := ew_inst
  csr.io.pc       := ew_pc
  csr.io.addr     := ew_alu
  csr.io.illegal  := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type  := ld_type
  csr.io.st_type  := st_type
  io.host <> csr.io.host 

  // Regfile Write
  val regWrite = MuxLookup(wb_sel, ew_alu.zext, Seq(
    WB_MEM -> load,
    WB_PC4 -> (ew_pc + 4.U).zext,
    WB_CSR -> csr.io.out.zext) ).asUInt 

  regFile.io.wen   := wb_en && !stall && !csr.io.expt 
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite

  // Abort store when there's an exception
  io.dcache.abort := csr.io.expt

  if (p(Trace)) {
    printf("PC: %x, INST: %x, REG[%d] <- %x\n", ew_pc, ew_inst,
      Mux(regFile.io.wen, wb_rd_addr, 0.U),
      Mux(regFile.io.wen, regFile.io.wdata, 0.U))
  }
}
