// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class ALUIo_concat(implicit p: Parameters) extends CoreBundle()(p) {
  val A            = Input(UInt(xlen.W)) //first operand
  val B            = Input(UInt(xlen.W)) //second operand
  val alu_op       = Input(UInt(4.W)) //operation
  val flipped_bit  = Input(UInt(1.W))
  val out          = Output(UInt((xlen+1).W)) //output
  val sum          = Output(UInt(xlen.W)) //sum of A and B
}

class ALU_concat(implicit val p: Parameters) extends Module with CoreParams {
  val io  = IO(new ALUIo_concat)
  val alu = p(BuildALU)(p)
  alu.io.A      := io.A
  alu.io.B      := io.B
  alu.io.alu_op := io.alu_op
  io.out        := Cat(~io.flipped_bit, alu.io.out)
  io.sum        := alu.io.sum
}