package hw5

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

object Blake2b {
    // Initialization Vectors - "first 64 bits of the fractional parts of the positive square roots of the first eight prime numbers")
    val IV = Blake2Model.IV.map(_.U) // 8, 64b constants

    /**
      * Rotates a right by b bits and wraps MSBs back around
      * @param a: p.wordSize bit UInt to be rotated right by b
      * @param b: Shift amount
      * @return
      */
    def ROTR(p: Blake2Params, a: UInt, b: Int): UInt =  {
        // BEGIN SOLUTION
        ???
    }

    /**
      * https://en.wikipedia.org/wiki/BLAKE_(hash_function)#Mix
      *
      * @param p
      * @param s
      * @param idxs
      * @param x
      * @param y
      * @return
      */
    def Mix(p: Blake2Params, stateVec: Seq[UInt], idxs: Seq[Int], x: UInt, y: UInt): ArrayBuffer[UInt] = {
        // BEGIN SOLUTION
        ???
    }

    // pretty print chisel to match test vector
    def pHex(m: Seq[UInt], width: Int=64) = {
      if (m.length == 16) {
        for (i <- 0 until 16) {
          val nl = if (Seq(2, 6, 10, 14).contains(i)) "\n" else ""
          printf(p"${Hexadecimal(m(i)(width - 1, 0))} $nl")
        }
        printf("\n")
      }
    }
        
    // Concats a Seq[UInt] to a single UInt 
    def collapseSeq(m : Seq[UInt]): UInt = m.tail.foldLeft(m.head) { case(a, b)  => Cat(b, a) }

    val defParams = Blake2Params(hashLen = 64, msgLen = 64)
}


class Blake2bIO(p: Blake2Params) extends Bundle {
    require (p.msgLen == 128, "FUTURE: accept msgLen != 128B")
    val msg = Flipped(Decoupled(UInt((p.msgLen * 8).W)))
    val msgBytes = Input(UInt(128.W)) // 128-bit t
    val digest = Decoupled(UInt((p.hashLen * 8).W)) 
    override def cloneType = (new Blake2bIO(p)).asInstanceOf[this.type]
}

/**
  * When a valid msg arrives, the next cycle ready goes low and the hash will begin.
  * 
  *
  * @param p
  */
class Blake2b(p: Blake2Params=Blake2b.defParams) extends Module {
    val io = IO(new Blake2bIO(p))
    // BEGIN SOLUTION
    ???
}