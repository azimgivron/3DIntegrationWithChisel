// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

object ALU { //15 different possible operations doable by the ALU encoded on 4bits
  val ALU_ADD    = "b0000".U(4.W)
  val ALU_SUB    = "b0001".U(4.W) 
  val ALU_AND    = "b0010".U(4.W)
  val ALU_OR     = "b0011".U(4.W)
  val ALU_XOR    = "b0100".U(4.W)
  val ALU_SLT    = "b0101".U(4.W) //shift left
  val ALU_SLL    = "b0110".U(4.W) //signed integer comparison, smaller than
  val ALU_SLTU   = "b0111".U(4.W) //unsigned integer comparison, smaller than
  val ALU_SRL    = "b1000".U(4.W)
  val ALU_SRA    = "b1001".U(4.W)
  val ALU_COPY_A = "b1010".U(4.W)
  val ALU_COPY_B = "b1011".U(4.W)
  val ALU_XXX    = "b1111".U(4.W) //no match 
}

class ALUIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A = Input(UInt(xlen.W)) //first operand
  val B = Input(UInt(xlen.W)) //second operand
  val alu_op = Input(UInt(4.W)) //operation
  val to_dispatcher = Output(Bool())
  val out = Output(UInt(xlen.W)) //output
  val sum = Output(UInt(xlen.W)) //sum of A and B
}

import ALU._

abstract class ALU(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new ALUIo)
  val to_dispatcher = RegInit(false.B)
  to_dispatcher := !to_dispatcher
  io.to_dispatcher := to_dispatcher
}

class ALUSimple(implicit p: Parameters) extends ALU()(p) {
  val shamt = io.B(4,0).asUInt //4 least significant bits of B converted into an unsigned integer

  io.out := MuxLookup(io.alu_op, io.B, Seq(
      ALU_ADD  -> (io.A + io.B),
      ALU_SUB  -> (io.A - io.B),
      ALU_SRA  -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL  -> (io.A >> shamt),
      ALU_SLL  -> (io.A << shamt),
      ALU_SLT  -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND  -> (io.A & io.B),
      ALU_OR   -> (io.A | io.B),
      ALU_XOR  -> (io.A ^ io.B),
      ALU_COPY_A -> io.A))

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B) //addition of A and B if the least significant bit of alu_op is 1, -B otherwise
  												//do we obtain 2adders ? (when doing synthesis) one for the MuxLookup and one here
}

class ALUArea(implicit p: Parameters) extends ALU()(p) { 
  //result are precomputed and then selected while in simpleALU we select the right operation and THEN we compute the right result
  val sum = io.A + Mux(io.alu_op(0), -io.B, io.B)
  val cmp = Mux(io.A(xlen-1) === io.B(xlen-1), sum(xlen-1),
            Mux(io.alu_op(1), io.B(xlen-1), io.A(xlen-1)))
  // 1. if the most significant bit of A is equal to B's one return most
  // significant bit of their sum
  // 2. if the second bit of alu_op is 1 return B's most significant bit, A's one otherwise

  val shamt  = io.B(4,0).asUInt
  val shin   = Mux(io.alu_op(3), io.A, Reverse(io.A)) //look for right or left shift, if right shin = A, reverse(A) otherwise
  val shiftr = (Cat(io.alu_op(0) && shin(xlen-1), shin).asSInt >> shamt)(xlen-1, 0) 
  //most significant bit is 1 or 0 ? 
  //unsigned right/left shift -> 0
  //signed right shift and most significant bit is of shin is 1 -> 1 , 0 otherwise
  val shiftl = Reverse(shiftr)

  val out = 
    Mux(io.alu_op === ALU_ADD || io.alu_op === ALU_SUB, sum,
    Mux(io.alu_op === ALU_SLT || io.alu_op === ALU_SLTU, cmp,
    Mux(io.alu_op === ALU_SRA || io.alu_op === ALU_SRL, shiftr,
    Mux(io.alu_op === ALU_SLL, shiftl,
    Mux(io.alu_op === ALU_AND, (io.A & io.B),
    Mux(io.alu_op === ALU_OR,  (io.A | io.B),
    Mux(io.alu_op === ALU_XOR, (io.A ^ io.B), 
    Mux(io.alu_op === ALU_COPY_A, io.A, io.B))))))))


  io.out := out
  io.sum := sum
}


