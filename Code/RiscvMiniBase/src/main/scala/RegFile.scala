// See LICENSE for license details.

package mini

import chisel3._
import freechips.rocketchip.config.Parameters

class RegFileIO(implicit p: Parameters)  extends CoreBundle()(p) {
  val raddr1 = Input(UInt(5.W))
  val raddr2 = Input(UInt(5.W))
  val rdata1 = Output(UInt(xlen.W))
  val rdata2 = Output(UInt(xlen.W))
  val wen    = Input(Bool())
  val waddr  = Input(UInt(5.W)) //from 0 to 32-1
  val wdata  = Input(UInt(xlen.W))
}

class RegFile(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new RegFileIO)
  val regs = Mem(32, UInt(xlen.W)) //32 registers that can contain unsigned integers of xlen bit width
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)
  when(io.wen & io.waddr.orR) { //write enable and register number is different than 0
    regs(io.waddr) := io.wdata
  }
}
