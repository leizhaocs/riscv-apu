// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class AluTester(alu: => Alu) extends BasicTester with TestUtils {
  val dut = Module(alu)
  val ctrl = Module(new Control)
  val xlen = dut.width

  val (cntr, done) = Counter(true.B, insts.size)
  val rs1 = Seq.fill(insts.size)(rnd.nextInt()).map(toBigInt)
  val rs2 = Seq.fill(insts.size)(rnd.nextInt()).map(toBigInt)
  val sum = VecInit(rs1.zip(rs2).map { case (a, b) => toBigInt(a.toInt + b.toInt).U(xlen.W) })
  val diff = VecInit(rs1.zip(rs2).map { case (a, b) => toBigInt(a.toInt - b.toInt).U(xlen.W) })
  val and = VecInit(rs1.zip(rs2).map { case (a, b) => (a & b).U(xlen.W) })
  val or = VecInit(rs1.zip(rs2).map { case (a, b) => (a | b).U(xlen.W) })
  val xor = VecInit(rs1.zip(rs2).map { case (a, b) => (a ^ b).U(xlen.W) })
  val slt = VecInit(rs1.zip(rs2).map { case (a, b) => (if (a.toInt < b.toInt) 1 else 0).U(xlen.W) })
  val sltu = VecInit(rs1.zip(rs2).map { case (a, b) => (if (a < b) 1 else 0).U(xlen.W) })
  val sll = VecInit(rs1.zip(rs2).map { case (a, b) => toBigInt(a.toInt << (b.toInt & 0x1f)).U(xlen.W) })
  val srl = VecInit(rs1.zip(rs2).map { case (a, b) => toBigInt(a.toInt >>> (b.toInt & 0x1f)).U(xlen.W) })
  val sra = VecInit(rs1.zip(rs2).map { case (a, b) => toBigInt(a.toInt >> (b.toInt & 0x1f)).U(xlen.W) })
  val out = (
    Mux(
      dut.io.alu_op === AluOP.ALU_ADD,
      sum(cntr),
      Mux(
        dut.io.alu_op === AluOP.ALU_SUB,
        diff(cntr),
        Mux(
          dut.io.alu_op === AluOP.ALU_AND,
          and(cntr),
          Mux(
            dut.io.alu_op === AluOP.ALU_OR,
            or(cntr),
            Mux(
              dut.io.alu_op === AluOP.ALU_XOR,
              xor(cntr),
              Mux(
                dut.io.alu_op === AluOP.ALU_SLT,
                slt(cntr),
                Mux(
                  dut.io.alu_op === AluOP.ALU_SLTU,
                  sltu(cntr),
                  Mux(
                    dut.io.alu_op === AluOP.ALU_SLL,
                    sll(cntr),
                    Mux(
                      dut.io.alu_op === AluOP.ALU_SRL,
                      srl(cntr),
                      Mux(dut.io.alu_op === AluOP.ALU_SRA, sra(cntr), Mux(dut.io.alu_op === AluOP.ALU_COPY_A, dut.io.A, dut.io.B))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Mux(dut.io.alu_op(0), diff(cntr), sum(cntr))
  )

  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.alu_op := ctrl.io.alu_op
  dut.io.A := VecInit(rs1.map(_.U))(cntr)
  dut.io.B := VecInit(rs2.map(_.U))(cntr)

  when(done) { stop() }
  assert(dut.io.out === out._1)
  assert(dut.io.sum === out._2)
  printf(
    "Counter: %d, OP: 0x%x, A: 0x%x, B: 0x%x, OUT: 0x%x ?= 0x%x, SUM: 0x%x ?= 0x%x\n",
    cntr,
    dut.io.alu_op,
    dut.io.A,
    dut.io.B,
    dut.io.out,
    out._1,
    dut.io.sum,
    out._2
  )
}

class ALUTests extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "ALUSimple" should "pass" in {
    test(new AluTester(new Alu(32))).runUntilStop()
  }
}
