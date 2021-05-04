package hw5

import chisel3._
import chisel3.util._


// A (m x k) X B (k x n) = C (m x k)
abstract class MatMulProblem(m: Int, k: Int, n: Int) {
  val aRows = m
  val aCols = k
  val bRows = k
  val bCols = n
  val cRows = m
  val cCols = n
}

// concrete class is necessary to instantiate MatMulProblem
case class MatMulModelParams(m: Int, k: Int, n: Int) extends MatMulProblem(m,k,n)

case class MatMulParams(m: Int, k: Int, n: Int, cyclesPerTransfer: Int, parallelism: Int = 1) extends MatMulProblem(m,k,n) {
  val w = 32.W
  require((aRows * aCols) % cyclesPerTransfer == 0)
  val aElementsPerTransfer = (aRows * aCols) / cyclesPerTransfer
  require(aElementsPerTransfer <= aCols)
  require((bRows * bCols) % cyclesPerTransfer == 0)
  val bElementsPerTransfer = (bRows * bCols) / cyclesPerTransfer
  require(bElementsPerTransfer <= bCols)
  if ((cRows * cCols) > cyclesPerTransfer)
    require((cRows * cCols) % cyclesPerTransfer == 0)
  val cElementsPerTransfer = ((cRows * cCols) / cyclesPerTransfer).max(1)
  require(cElementsPerTransfer <= cCols)
  require(cCols >= parallelism)
  require(cCols % parallelism == 0)
}


class MatMul(p: MatMulParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val aBlock = Vec(p.aElementsPerTransfer, SInt(p.w))
      val bBlock = Vec(p.bElementsPerTransfer, SInt(p.w))
    }))
    val outBlock = Valid(Vec(p.cElementsPerTransfer, SInt(p.w)))
  })
}


class MatMulWithReg(p: MatMulParams) extends MatMul(p) {
  // BEGIN SOLUTION
  ???
}
