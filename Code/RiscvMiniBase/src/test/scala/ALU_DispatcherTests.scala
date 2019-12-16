// See LICENSE for license details.

package mini
import chisel3._
import chisel3.util._
import chisel3.testers._
import scala.math.pow

object State extends Enumeration {
  type State = Value
  val dispatch_alu1, dispatch_alu2, waitStatus = Value
}

import State._

class ALU_Dispatcher_SW {
  var state: State = dispatch_alu1;
  var alu1_busy    = false;
  var alu2_busy    = false;
  var alu1_done    = 0;
  var alu2_done    = 0;
  var select       = 0;

  def step(A: Int, B: Int, alu_op: Int, from_alu1: Boolean, from_alu2: Boolean): Array[Int]= {
    //return a list containing A_toALU1, B_toALU1, alu_op_toALU1, A_toALU2, B_toALU2, alu_op_toALU2
    val output        = new Array[Int](9)
    val previousState = this.state

    if(this.state == dispatch_alu1 || (this.state == waitStatus && (this.alu1_done==1) != from_alu1)) {  
      output(0) = A
      output(1) = B
      output(2) = alu_op
      output(3) = alu1_done
      output(4) = 0
      output(5) = 0
      output(6) = 0
      output(7) = alu2_done
      this.alu1_busy = true
                          
      this.state = if(!this.alu2_busy || ((alu2_done==1) != from_alu2)) dispatch_alu2 else waitStatus
    }else if( this.state == dispatch_alu2 || (this.state == waitStatus && (this.alu2_done==1) != from_alu2)) {
        output(0) = 0
        output(1) = 0
        output(2) = 0
        output(3) = alu1_done
        output(4) = A
        output(5) = B
        output(6) = alu_op
        output(7) = alu2_done
        this.alu2_busy = true
        
        this.state = if(!this.alu1_busy || ((alu1_done==1) != from_alu1)) dispatch_alu1 else waitStatus

    }else {
      output(0) = 0
      output(1) = 0
      output(2) = 0
      output(3) = alu1_done
      output(4) = 0
      output(5) = 0
      output(6) = 0
      output(7) = alu2_done
      
      if(!this.alu1_busy){
        this.state = dispatch_alu1
      }else if(!this.alu2_busy){
        this.state = dispatch_alu2
      }else{
        this.state = waitStatus
      }
    }
    val tmp_done = (this.alu1_done==1)
    val tmp_busy = this.alu1_busy

    if(((alu1_done==1) != from_alu1) && (previousState != dispatch_alu1)) {
      this.alu1_done = if(from_alu1) 1 else 0
      if(previousState != waitStatus){
        this.alu1_busy = false
      }
      if(this.alu2_busy || (previousState == dispatch_alu2)) {
        this.select = 1
      }
    }
    if(((alu2_done==1) != from_alu2) && (previousState != dispatch_alu2)) {
      this.alu2_done = if(from_alu2) 1 else 0
      if(!((state == waitStatus) && (tmp_done == from_alu1))) {
        this.alu2_busy = false
      }
      if(tmp_busy || (state == dispatch_alu1) || ((state == waitStatus) && (tmp_done != from_alu1))) {
        this.select = 0
      }
    }
    output(8) = this.select
    
    return output
  }
}

class ALU_DispatcherTester(c: => ALU_Dispatcher)(implicit p: freechips.rocketchip.config.Parameters) extends BasicTester with TestUtils {
  val dut              = Module(c)
  val xlen             = p(XLEN)
  val dispatcher_model = new ALU_Dispatcher_SW

  val size             = 100
  val (cntr, done)     = Counter(true.B, size)
  val rs1              = Seq.fill(size)(rnd.nextInt(pow(2, xlen).intValue)) map toBigInt
  val rs2              = Seq.fill(size)(rnd.nextInt(pow(2, xlen).intValue)) map toBigInt
  val alu_op           = Seq.fill(size)(rnd.nextInt(16))
  val from_alu1_init   = Seq.fill(size)(rnd.nextBoolean())
  val from_alu1        = from_alu1_init.updated(0, false)
  val from_alu2_init   = Seq.fill(size)(rnd.nextBoolean())
  val from_alu2        = from_alu2_init.updated(0, false)
  val out              = (rs1 zip rs2 zip alu_op zip from_alu1 zip from_alu2) map { case ((((a, b), c), d), e) => dispatcher_model.step(a, b, c, d, e)}
  val a_toALU1         = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => a.U})
  val b_toALU1         = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => b.U})
  val alu_op_toALU1    = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => c.U})
  val alu_flipped      = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => d.U})
  val a_toALU2         = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => e.U})
  val b_toALU2         = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => f.U})
  val alu_op_toALU2    = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => g.U})
  val alu2_flipped     = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => h.U})
  val select           = VecInit(out map { case Array(a, b, c, d, e, f, g, h, i) => (i==1).B})

  dut.io.alu_op := VecInit(alu_op map (_.U))(cntr)
  dut.io.A := VecInit(rs1 map (_.U))(cntr)
  dut.io.B := VecInit(rs2 map (_.U))(cntr)
  dut.io.from_alu1 := VecInit(from_alu1 map (_.B))(cntr)
  dut.io.from_alu2 := VecInit(from_alu2 map (_.B))(cntr)

  when(done) { stop(); stop() } // from VendingMachine example...

  assert(dut.io.A_toALU1 === a_toALU1(cntr))
  assert(dut.io.B_toALU1 === b_toALU1(cntr))
  assert(dut.io.alu_op_toALU1 === alu_op_toALU1(cntr))
  assert(dut.io.alu_flipped === alu_flipped(cntr))
  assert(dut.io.A_toALU2 === a_toALU2(cntr))
  assert(dut.io.B_toALU2 === b_toALU2(cntr))
  assert(dut.io.alu_op_toALU2 === alu_op_toALU2(cntr))
  assert(dut.io.alu2_flipped === alu2_flipped(cntr))
  assert(dut.io.select === select(cntr))
  printf("Counter: %d, OP: %d, A: %d, B: %d, from_alu1: %d, from_alu2: %d\nOUT:\nhw ?= sw\nA to alu1:%d %d\nB to alu1:%d %d\nOP to alu1:%d %d\nFlip bit to alu1:%d %d\nA to alu2:%d %d\nB to alu2:%d %d\nOP to alu2:%d %d\nFlip bit to alu2:%d %d\nSelect:%d %d\n",
         cntr, dut.io.alu_op, dut.io.A, dut.io.B, dut.io.from_alu1, dut.io.from_alu2, dut.io.A_toALU1, a_toALU1(cntr), dut.io.B_toALU1, 
         b_toALU1(cntr), dut.io.alu_op_toALU1, alu_op_toALU1(cntr), dut.io.alu_flipped, alu_flipped(cntr), dut.io.A_toALU2, a_toALU2(cntr),
         dut.io.B_toALU2, b_toALU2(cntr), dut.io.alu_op_toALU2, alu_op_toALU2(cntr), dut.io.alu2_flipped, alu2_flipped(cntr),
         dut.io.select, select(cntr)) 
}

class ALU_DispatcherTests extends org.scalatest.FlatSpec {
  implicit val p = (new MiniConfig).toInstance
  "Dispatcher" should "pass" in {
    assert(TesterDriver execute (() => new ALU_DispatcherTester(new ALU_Dispatcher)))
  }
}