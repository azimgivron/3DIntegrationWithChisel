// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class DispatcherIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A 			= Input(UInt(xlen.W)) //first operand
  val B 			= Input(UInt(xlen.W)) //second operand
  val alu_op 		= Input(UInt(4.W)) //operation
  val from_alu1		= Input(Bool())
  val from_alu2		= Input(Bool())
  val A_toALU1 		= Output(UInt(xlen.W)) //first operand
  val B_toALU1 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU1 = Output(UInt(4.W)) //operation
  val A_toALU2 		= Output(UInt(xlen.W)) //first operand
  val B_toALU2 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU2 = Output(UInt(4.W)) //operation
}

import ALU_Dispatcher._

abstract class Dispatcher(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new DispatcherIo)
}

class ALU_Dispatcher(implicit p: Parameters) extends Dispatcher()(p) { 
	val alu1_busy = RegInit(false.B)
	val alu2_busy = RegInit(false.B)
	val from_alu1 = RegInit(io.from_alu1)
	val from_alu2 = RegInit(io.from_alu2)
	val on_hold = Reg(UInt(xlen.W))

	val DispatchToALU1 :: DispatchToALU2 :: HoldInMem :: Nil = Enum(3)
	val state = RegInit(DispatchToALU1)

	switch(state) {
		is(DispatchToALU1) {
	    	io.A := io.A_toALU1
	    	io.B := io.B_toALU1
	    	io.alu_op := io.alu_op_toALU1
	    	alu1_busy := true.B
	    	.when(!alu1_busy){
	    		state := DispatchToALU1
	    	}.elsewhen(alu1_busy & !alu2_busy){
	    		state := DispatchToALU2
	    	}.elsewhen(alu1_busy & alu2_busy)
	    		state := HoldInMem
	    	}
    	}
    	is(DispatchToALU2) {
    		io.A := io.A_toALU2
	    	io.B := io.B_toALU2
	    	io.alu_op := io.alu_op_toALU2
	    	alu2_busy := true.B
    		.when(!alu1_busy) {
		    	state := DispatchToALU1
		    }.elsewhen(alu1_busy & alu2_busy){
		    	state := HoldInMem
		    }
    	}
    	is(HoldInMem) {
	    	.when(!alu1_busy) {
		    	state := DispatchToALU1
		    }.elsewhen(alu1_busy & !alu2_busy){
		    	state := DispatchToALU2
		    }
    	}
  	}

  	.when(io.from_alu1 =/= from_alu1) {
  		alu1_busy := false.B
  		from_alu1 := io.from_alu1
  	}
  	.when(io.from_alu2 =/= from_alu2) {
  		alu2_busy := false.B
  		from_alu2 := io.from_alu2
  	}


}