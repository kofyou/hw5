package hw5

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec

import treadle._
import chisel3.tester.experimental.TestOptionBuilder._

import hw5.MatMulModel.Matrix


object MatMulTestData {
  def genIdentity(n: Int): Matrix = Seq.tabulate(n,n) { (i,j) => if (i==j) 1 else 0 }

  def genOnesRow(n: Int): Matrix = Seq(Seq.fill(n)(1))

  def genOnesCol(n: Int): Matrix = Seq.fill(n)(Seq(1))

  val in2x4  = Seq(Seq(1,2,3,4),
                   Seq(5,6,7,8))
  val in4x2  = Seq(Seq(1,2),
                   Seq(3,4),
                   Seq(5,6),
                   Seq(7,8))
  val out2x2 = Seq(Seq(50, 60),
                   Seq(114,140))
  val out4x4 = Seq(Seq(11, 14, 17, 20),
                   Seq(23, 30, 37, 44),
                   Seq(35, 46, 57, 68),
                   Seq(47, 62, 77, 92))
}


class MatMulModelTester extends FreeSpec with ChiselScalatestTester {
  "MatMulModel should multiply identity" in {
    val n = 4
    val identity4x4 = MatMulTestData.genIdentity(n)
    val p = MatMulModelParams(n,n,n)
    assert(MatMulModel(p, identity4x4, identity4x4) == identity4x4)
  }

  "MatMulModel should multiply identity x in4x2" in {
    assert(MatMulModel(MatMulModelParams(4,4,2), MatMulTestData.genIdentity(4), MatMulTestData.in4x2) == MatMulTestData.in4x2)
  }

  "MatMulModel should multiply identity x in2x4" in {
    assert(MatMulModel(MatMulModelParams(2,2,4), MatMulTestData.genIdentity(2), MatMulTestData.in2x4) == MatMulTestData.in2x4)
  }

  "MatMulModel should multiply in2x4 x in4x2" in {
    assert(MatMulModel(MatMulModelParams(2,4,2), MatMulTestData.in2x4, MatMulTestData.in4x2) == MatMulTestData.out2x2)
  }

  "MatMulModel should multiply in4x2 x in2x4" in {
    assert(MatMulModel(MatMulModelParams(4,2,4), MatMulTestData.in4x2, MatMulTestData.in2x4) == MatMulTestData.out4x4)
  }
}


class MatMulTester extends FreeSpec with ChiselScalatestTester {
  def doMatMulTest(a: Matrix, b: Matrix, cyclesPerTransfer: Int, parallelism: Int): Boolean = {
    val p = MatMulParams(a.size, a.head.size, b.head.size, cyclesPerTransfer, parallelism)
    test(new MatMulWithReg(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // request a transfer to start
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.outBlock.valid.expect(false.B)
      dut.clock.step()
      // transfer input matrices
      val aChunked = a.flatten.grouped(p.aElementsPerTransfer).toSeq
      val bChunked = b.flatten.grouped(p.bElementsPerTransfer).toSeq
      assert(aChunked.length == cyclesPerTransfer)
      assert(bChunked.length == cyclesPerTransfer)
      aChunked.zip(bChunked) foreach { case (aChunk, bChunk) =>
        dut.io.in.bits.aBlock.zip(aChunk).foreach{ case (dutIO, elem) => dutIO.poke(elem.S) }
        dut.io.in.bits.bBlock.zip(bChunk).foreach{ case (dutIO, elem) => dutIO.poke(elem.S) }
        dut.clock.step()
      }
      dut.io.in.valid.poke(false.B)
      // wait for completion
      dut.io.in.ready.expect(false.B)
      dut.io.outBlock.valid.expect(false.B)
      dut.clock.step(p.m * p.k * p.n / p.parallelism)
      // check for completion & result
      dut.io.outBlock.valid.expect(true.B)
      val expected = MatMulModel(p, a, b)
      val cChunked = expected.flatten.grouped(p.cElementsPerTransfer).toSeq
      for (cChunk <- cChunked) {
        dut.io.outBlock.bits.zip(cChunk).foreach{ case (dutIO, elem) => dutIO.expect(elem.S) }
        dut.clock.step()
      }
      dut.io.in.ready.expect(true.B)
    }
    true
  }

  "MatMul should multiply (1s row) x (1s column)" in {
    val k = 4
    doMatMulTest(MatMulTestData.genOnesRow(k), MatMulTestData.genOnesCol(k), k, 1)
  }

  "MatMul should multiply identity x (1s column) (matrix-vector)" in {
    val k = 4
    doMatMulTest(MatMulTestData.genIdentity(k), MatMulTestData.genOnesCol(k), k, 1)
  }

  "MatMul should multiply (1s column) x (1s row)" in {
    val k = 4
    doMatMulTest(MatMulTestData.genOnesCol(k), MatMulTestData.genOnesRow(k), k, 1)
  }

  "MatMul should multiply identity (no parallelism)" in {
    val i4x4 = MatMulTestData.genIdentity(4)
    doMatMulTest(i4x4,i4x4,4,1)
  }

  "MatMul should multiply in2x4 x in4x2 (no parallelism)" in {
    doMatMulTest(MatMulTestData.in2x4, MatMulTestData.in4x2,4,1)
  }

  "MatMul should multiply in4x2 x in2x4 (no parallelism)" in {
    doMatMulTest(MatMulTestData.in4x2, MatMulTestData.in2x4,4,1)
  }

  "MatMul should multiply identity (full parallelism)" in {
    val i4x4 = MatMulTestData.genIdentity(4)
    doMatMulTest(i4x4,i4x4,4,4)
  }

  "MatMul should multiply in2x4 x in4x2 (full parallelism)" in {
    doMatMulTest(MatMulTestData.in2x4, MatMulTestData.in4x2,4,2)
  }

  "MatMul should multiply in4x2 x in2x4 (full parallelism)" in {
    doMatMulTest(MatMulTestData.in4x2, MatMulTestData.in2x4,4,4)
  }
}