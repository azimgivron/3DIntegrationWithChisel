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

class ALU_Dispatcher_SW{
  var state: State = dispatch_alu1;
  var alu1_busy    = false;
  var alu2_busy    = false;
  var select       = 0;

  def step(A: Int, B: Int, alu_op: Int, from_alu1: Boolean, from_alu2: Boolean): Array[Int]={
    //return a list containing A_toALU1, B_toALU1, alu_op_toALU1, A_toALU2, B_toALU2, alu_op_toALU2
    val output        = new Array[Int](7)
    val previousState = this.state

    this.state match{
      case `dispatch_alu1` => if(!this.alu1_busy){
                                output(0) = A
                                output(1) = B
                                output(2) = alu_op
                                output(3) = 0
                                output(4) = 0
                                output(5) = 0
                                this.alu1_busy = true
                              }
                              else{
                                output(0) = 0
                                output(1) = 0
                                output(2) = 0
                                output(3) = 0
                                output(4) = 0
                                output(5) = 0
                              } 
                              if(!this.alu2_busy || from_alu2){
                                this.state = dispatch_alu2
                              } 
                              else{
                                this.state = waitStatus
                              }
      case `dispatch_alu2` => if(!this.alu2_busy){
                                output(0) = 0
                                output(1) = 0
                                output(2) = 0
                                output(3) = A
                                output(4) = B
                                output(5) = alu_op
                                this.alu2_busy = true
                              }
                              else{
                                output(0) = 0
                                output(1) = 0
                                output(2) = 0
                                output(3) = 0
                                output(4) = 0
                                output(5) = 0
                              }
                              if(!this.alu1_busy || from_alu1){
                                this.state = dispatch_alu1
                              } 
                              else{
                                this.state = waitStatus
                              } 
      case `waitStatus` =>    output(0) = 0
                              output(1) = 0
                              output(2) = 0
                              output(3) = 0
                              output(4) = 0
                              output(5) = 0
                              if(!this.alu1_busy || from_alu1){
                                this.state = dispatch_alu1
                              } 
                              else if(!this.alu2_busy || from_alu2){
                                this.state = dispatch_alu2
                              }
                              else{
                                this.state = waitStatus
                              }
      case _ => 
    }

    if((from_alu2 && previousState!=dispatch_alu2) && (previousState==dispatch_alu2 || this.alu1_busy)){
        this.select = 0
        output(6)   = 0
    }else if((from_alu1 && previousState!=dispatch_alu1) && (previousState==dispatch_alu1 || this.alu2_busy)){
        this.select = 1
        output(6)   = 1
    }else{
      output(6) = this.select
    }

    if(from_alu1 && previousState!=dispatch_alu1){
      this.alu1_busy = false
    }

    if(from_alu2 && previousState!=dispatch_alu2){
      this.alu2_busy = false
    }
    return output
  }
}

class ALU_DispatcherTester(c: => ALU_Dispatcher)(implicit p: freechips.rocketchip.config.Parameters) extends BasicTester with TestUtils {
  val dut              = Module(c)
  val xlen             = p(XLEN)
  val dispatcher_model = new ALU_Dispatcher_SW

  val size             = 1000
  val (cntr, done)     = Counter(true.B, size)
  val rs1              = Seq.fill(size)(rnd.nextInt(pow(2, xlen).intValue)) map toBigInt
  val rs2              = Seq.fill(size)(rnd.nextInt(pow(2, xlen).intValue)) map toBigInt
  val alu_op           = Seq.fill(size)(rnd.nextInt(16))
  val from_alu1_init   = Seq.fill(size)(rnd.nextBoolean())
  val from_alu1        = from_alu1_init.updated(0, false)
  val from_alu2_init   = Seq.fill(size)(rnd.nextBoolean())
  val from_alu2        = from_alu2_init.updated(0, false)
  val out              = (rs1 zip rs2 zip alu_op zip from_alu1 zip from_alu2) map { case ((((a, b), c), d), e) => dispatcher_model.step(a, b, c, d, e)}
  val a_toALU1         = VecInit(out map { case Array(a, b, c, d, e, f, g) => a.U})
  val b_toALU1         = VecInit(out map { case Array(a, b, c, d, e, f, g) => b.U})
  val alu_op_toALU1    = VecInit(out map { case Array(a, b, c, d, e, f, g) => c.U})
  val a_toALU2         = VecInit(out map { case Array(a, b, c, d, e, f, g) => d.U})
  val b_toALU2         = VecInit(out map { case Array(a, b, c, d, e, f, g) => e.U})
  val alu_op_toALU2    = VecInit(out map { case Array(a, b, c, d, e, f, g) => f.U})
  val select           = VecInit(out map { case Array(a, b, c, d, e, f, g) => (g==1).B})

  dut.io.alu_op := VecInit(alu_op map (_.U))(cntr)
  dut.io.A := VecInit(rs1 map (_.U))(cntr)
  dut.io.B := VecInit(rs2 map (_.U))(cntr)
  dut.io.from_alu1 := VecInit(from_alu1 map (_.B))(cntr)
  dut.io.from_alu2 := VecInit(from_alu2 map (_.B))(cntr)

  when(done) { stop(); stop() } // from VendingMachine example...

  assert(dut.io.A_toALU1 === a_toALU1(cntr))
  assert(dut.io.B_toALU1 === b_toALU1(cntr))
  assert(dut.io.alu_op_toALU1 === alu_op_toALU1(cntr))
  assert(dut.io.A_toALU2 === a_toALU2(cntr))
  assert(dut.io.B_toALU2 === b_toALU2(cntr))
  assert(dut.io.alu_op_toALU2 === alu_op_toALU2(cntr))
  assert(dut.io.select === select(cntr))
  printf("Counter: %d, OP: %d, A: %d, B: %d, from_alu1: %d, from_alu2: %d, OUT: (hw) %d %d %d %d %d %d %d ?= (sw) %d %d %d %d %d %d %d\n",
         cntr, dut.io.alu_op, dut.io.A, dut.io.B, dut.io.from_alu1, dut.io.from_alu2, dut.io.A_toALU1, dut.io.B_toALU1, 
         dut.io.alu_op_toALU1, dut.io.A_toALU2, dut.io.B_toALU2, dut.io.alu_op_toALU2, dut.io.select, a_toALU1(cntr),
         b_toALU1(cntr), alu_op_toALU1(cntr), a_toALU2(cntr), b_toALU2(cntr), alu_op_toALU2(cntr), select(cntr)) 
}

class ALU_DispatcherTests extends org.scalatest.FlatSpec {
  implicit val p = (new MiniConfig).toInstance
  "Dispatcher" should "pass" in {
    assert(TesterDriver execute (() => new ALU_DispatcherTester(new ALU_Dispatcher)))
  }
}