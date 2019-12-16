// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class DispatcherIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A 			= Input(UInt(xlen.W)) //first operand
  val B 			= Input(UInt(xlen.W)) //second operand
  val alu_op 		= Input(UInt(4.W)) //operator
  val from_alu1		= Input(UInt(1.W)) //status updater signal for alu1
  val from_alu2		= Input(UInt(1.W))	//status updater signal for alu2
  val A_toALU1 		= Output(UInt(xlen.W)) //first operand
  val B_toALU1 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU1 = Output(UInt(4.W)) //operator
  val alu_flipped 	= Output(UInt(1.W))
  val A_toALU2 		= Output(UInt(xlen.W)) //first operand
  val B_toALU2 		= Output(UInt(xlen.W)) //second operand
  val alu_op_toALU2 = Output(UInt(4.W)) //operator
  val alu2_flipped 	= Output(UInt(1.W))
  val select 		= Output(Bool()) //Tells which one of the ALUs contains the oldest result
  									 //with false designated the first one
}

class ALU_Dispatcher(implicit val p: Parameters) extends Module with CoreParams {
	val io			= IO(new DispatcherIo)
	val alu1_busy 	= RegInit(false.B)
	val alu2_busy 	= RegInit(false.B)
	val select		= RegInit(false.B)
	val alu_done  	= RegInit(0.U(1.W)) //correspond to 1bit, flipped every time an operation is made for the alu 1
  	val alu2_done 	= RegInit(0.U(1.W))

	//FSM states
	val dispatchToALU1 :: dispatchToALU2 :: waitStatus :: Nil = Enum(3)
	//init state
	val state = RegInit(dispatchToALU1)

	io.select := select //one cycle latency

  	//free ALU
  	when((alu_done =/= io.from_alu1) && (state =/= dispatchToALU1)) { 
  	//You can only reach dispatchToALU1 if alu1_busy was already set to false
  		alu_done := io.from_alu1
  		when(state =/= waitStatus) {
  			alu1_busy := false.B
  		}
  		when(alu2_busy || (state === dispatchToALU2)) {
  			select := true.B
  			io.select := true.B
  		}
  	}

  	when((alu2_done =/= io.from_alu2) && (state =/= dispatchToALU2)) {
  	//Similarly you can only reach dispatchToALU2 if alu2_busy was already set to false
  		alu2_done := io.from_alu2
  		when(!((state === waitStatus) && (alu_done === io.from_alu1))) {
  			alu2_busy := false.B
  		}
  		when(alu1_busy || (state === dispatchToALU1) || (state === waitStatus && alu_done =/= io.from_alu1)) {
  			select := false.B
  			io.select := false.B
  		}
  	}
	//FSM
	//match the current state and act in consequence
	when((state === dispatchToALU1) ||
	 (state === waitStatus && alu_done =/= io.from_alu1)) { //used when instead of switch because switch doesn't allow default case
		//assign outputs
    	io.A_toALU1 		:= io.A
    	io.B_toALU1			:= io.B
    	io.alu_op_toALU1	:= io.alu_op
    	io.alu_flipped 		:= alu_done	
    	io.A_toALU2 		:= 0.U
    	io.B_toALU2 		:= 0.U
    	io.alu_op_toALU2 	:= 0.U
    	io.alu2_flipped 	:= alu2_done	
    	//change internal status to busy
	  	alu1_busy 			:= true.B
    	//transition of state   
    	state := Mux(!alu2_busy || (alu2_done =/= io.from_alu2), dispatchToALU2, waitStatus) 		
    }
    .elsewhen((state === dispatchToALU2) || (state === waitStatus && alu2_done =/= io.from_alu2)) {
		io.A_toALU1 		:= 0.U
    	io.B_toALU1 		:= 0.U
    	io.alu_op_toALU1 	:= 0.U
    	io.alu_flipped 		:= alu_done	
		io.A_toALU2 		:= io.A
    	io.B_toALU2 		:= io.B
    	io.alu_op_toALU2 	:= io.alu_op
    	io.alu2_flipped 	:= alu2_done	

    	alu2_busy 			:= true.B
	    state := Mux(!alu1_busy || (alu_done =/= io.from_alu1), dispatchToALU1, waitStatus) 		
	}
    .otherwise{	
		io.A_toALU1 		:= 0.U
		io.B_toALU1 		:= 0.U
		io.alu_op_toALU1 	:= 0.U
		io.alu_flipped 		:= alu_done	
		io.A_toALU2 		:= 0.U
		io.B_toALU2 		:= 0.U
		io.alu_op_toALU2 	:= 0.U
		io.alu2_flipped 	:= alu2_done
		
		state := Mux(!alu1_busy, dispatchToALU1, 
			  	 Mux(!alu2_busy, dispatchToALU2,
						waitStatus))
	}
}