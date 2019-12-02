// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class DispatcherIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A 			= Input(UInt(xlen.W)) //first operand
  val B 			= Input(UInt(xlen.W)) //second operand
  val alu_op 		= Input(UInt(4.W)) //operator
  val from_alu1		= Input(Bool()) //status updater signal for alu1
  val from_alu2		= Input(Bool())	//status updater signal for alu2
  val A_toALU1 		= Output(UInt(xlen.W)) //first operand
  val B_toALU1 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU1 = Output(UInt(4.W)) //operator
  val A_toALU2 		= Output(UInt(xlen.W)) //first operand
  val B_toALU2 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU2 = Output(UInt(4.W)) //operator
  val select 		= Output(Bool()) //Tells which one of the ALUs contains the oldest result
}

class ALU_Dispatcher(implicit val p: Parameters) extends Module with CoreParams {
	val io			= IO(new DispatcherIo)
	val alu1_busy 	= RegInit(false.B)
	val alu2_busy 	= RegInit(false.B)
	val select		= RegInit(false.B)

	//FSM states
	val dispatchToALU1 :: dispatchToALU2 :: waitStatus :: Nil = Enum(3)
	//init state
	val state = RegInit(dispatchToALU1)


  	//free ALU
  	when(io.from_alu1 && state =/= dispatchToALU1) { 
  	//You can only reach dispatchToALU1 if alu1_busy was already set to false
  		alu1_busy := false.B
  	}

  	when(io.from_alu2 && state =/= dispatchToALU2) {
  	//Similarly you can only reach dispatchToALU2 if alu2_busy was already set to false
  		alu2_busy := false.B
  	}

  	when((io.from_alu2 && state =/= dispatchToALU2) && (alu1_busy || state === dispatchToALU1)) {
		select 		:= false.B
		io.select 	:= false.B
  	}.elsewhen((io.from_alu1 && state =/= dispatchToALU1) && (alu2_busy || state === dispatchToALU2)) {
		select 		:= true.B
		io.select 	:= true.B
  	}.otherwise {
  		io.select := select
  	}

	//FSM
	//match the current state and act in consequence
	when(state === dispatchToALU1) { //used when instead of switch because switch doesn't allow default case
    	when(!alu1_busy) {
    		//assign outputs
	    	io.A_toALU1 		:= io.A
	    	io.B_toALU1			:= io.B
	    	io.alu_op_toALU1	:= io.alu_op
	    	io.A_toALU2 		:= 0.U
	    	io.B_toALU2 		:= 0.U
	    	io.alu_op_toALU2 	:= 0.U
	    	//change internal status to busy
		  	alu1_busy 			:= true.B
    	}.otherwise {
    		io.A_toALU1 		:= 0.U
	    	io.B_toALU1 		:= 0.U
	    	io.alu_op_toALU1 	:= 0.U
			io.A_toALU2 		:= 0.U
	    	io.B_toALU2 		:= 0.U
	    	io.alu_op_toALU2 	:= 0.U
    	}
    	//transition of state    		
		when(!alu2_busy || io.from_alu2) {
			state := dispatchToALU2
		}.otherwise{
			state := waitStatus
		}
    }
    .elsewhen(state === dispatchToALU2) {
		when(!alu2_busy) {
			io.A_toALU1 		:= 0.U
	    	io.B_toALU1 		:= 0.U
	    	io.alu_op_toALU1 	:= 0.U
			io.A_toALU2 		:= io.A
	    	io.B_toALU2 		:= io.B
	    	io.alu_op_toALU2 	:= io.alu_op

	    	alu2_busy 			:= true.B
		}.otherwise {
			io.A_toALU1 		:= 0.U
	    	io.B_toALU1 		:= 0.U
	    	io.alu_op_toALU1 	:= 0.U
			io.A_toALU2 		:= 0.U
	    	io.B_toALU2 		:= 0.U
	    	io.alu_op_toALU2 	:= 0.U
		}
		when(!alu1_busy || io.from_alu1) {
	    	state := dispatchToALU1
	    }.otherwise {
	    	state := waitStatus
	    }
	}
    .elsewhen(state === waitStatus) {
		io.A_toALU1 		:= 0.U
    	io.B_toALU1 		:= 0.U
    	io.alu_op_toALU1 	:= 0.U
		io.A_toALU2 		:= 0.U
    	io.B_toALU2 		:= 0.U
    	io.alu_op_toALU2 	:= 0.U
		
		when(!alu1_busy || io.from_alu1) {
	    	state := dispatchToALU1
	    }.elsewhen(!alu2_busy || io.from_alu2) {
	    	state := dispatchToALU2
	    }.otherwise{
	    	state := waitStatus
	    }
	}
	.otherwise { //unreachable default case
		io.A_toALU1 		:= 0.U
    	io.B_toALU1 		:= 0.U
    	io.alu_op_toALU1 	:= 0.U
		io.A_toALU2 		:= 0.U
    	io.B_toALU2 		:= 0.U
    	io.alu_op_toALU2 	:= 0.U
		state := waitStatus
  	}
}