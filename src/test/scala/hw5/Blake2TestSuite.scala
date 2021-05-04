package hw5

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec

import treadle._
import chisel3.tester.experimental.TestOptionBuilder._

class Blake2ROTRTester extends FreeSpec with ChiselScalatestTester {
    "Hardware 64b ROTR should work" in {
        val p = Blake2Params()
        test(new Module {
            val io = IO(new Bundle {
                val out = Output(Vec(4, UInt(p.wordSize.W)))
            })
            val in = Seq(7, 0, 1, 1).map(_.U(p.wordSize.W))
            val shifts = Seq(5, 5, 0, 1)
            in.zipWithIndex.foreach { case (v, i) => io.out(i) := Blake2b.ROTR(p, in(i), shifts(i)) }
        }) { c => 
            for (a <- 0 until 31) {
                val exp: Seq[BigInt] = Seq(BigInt("4035225266123964416"), 0, 1, BigInt(1) << 63)
                exp.zipWithIndex.foreach { case (e, i) => c.io.out(i).expect(e.U) }
            }
        }
    }

    "Hardware 32b ROTR should work" in {
        val p = Blake2Params(useBlake2s = true)
        test(new Module {
            val io = IO(new Bundle {
                val out = Output(Vec(4, UInt(p.wordSize.W)))
            })
            val in = Seq(7, 0, 1, 1).map(_.U(p.wordSize.W))
            val shifts = Seq(5, 5, 0, 1)
            in.zipWithIndex.foreach { case (v, i) => io.out(i) := Blake2b.ROTR(p, in(i), shifts(i)) }
        }) { c => 
            for (a <- 0 until 31) {
                val exp: Seq[BigInt] = Seq(BigInt("939524096"), 0, 1, BigInt(1) << 31)
                exp.zipWithIndex.foreach { case (e, i) => c.io.out(i).expect(e.U) }
            }
        }
    }
}

class Blake2MixTester extends FreeSpec with ChiselScalatestTester {
    "Hardware 64b MIX should work" in {
        val p = Blake2Params(hashLen = 64, msgLen = 128)
        val inState = Seq.tabulate(16)(BigInt(_))
        val idxs = Seq(0, 4, 8, 12)
        val x = 1234
        val y = 5678

        val exp = Blake2Model.Mix(p, inState, idxs, x, y)

        test(new Module {
            val io = IO(new Bundle {
                val out = Output(Vec(16, UInt(p.wordSize.W)))
            })
            val regs = inState.map(i => RegInit(i.U(p.wordSize.W)))

            io.out := Blake2b.Mix(p, regs, idxs, x.U(p.wordSize.W), y.U(p.wordSize.W))
        }).withAnnotations(Seq(WriteVcdAnnotation)) { c => 
                c.clock.step(1)
                exp.zipWithIndex.map { case (e, i) => c.io.out(i).expect(e.U)}
        }
    }
}


class Blake2HashTester extends FreeSpec with ChiselScalatestTester {
    def testBlake2(p: Blake2Params, m: Message): Boolean = {
        val hashModel = new Blake2Model(p)
        val expHash = hashModel(m)
        test(new Blake2b(p)).withAnnotations(Seq(WriteVcdAnnotation)) { c => 
            c.io.msg.bits.poke(m.U)
            c.io.msg.valid.poke(true.B)
            c.io.msgBytes.poke(m.length.U)
            c.clock.step(1)
            c.io.msg.valid.poke(false.B)
            c.clock.step(p.numRounds) // proceed through half of the rounds
            // attempt to enter another valid msg that should be ignored 
            c.io.msg.bits.poke(12345678.U)
            c.io.msg.valid.poke(true.B)
            c.clock.step(p.numRounds) // 2nd half of rounds (remember 4-col OR 4-diag per cycle)
            c.io.digest.valid.expect(true.B)

            // Process the output in Scala land and compare to the expected
            val got = c.io.digest.bits.peek().litValue
            val gotWords = (0 until 8).map(i => (got >> i*p.wordSize) & p.MASK)
            val gotHash = Blake2Model.wordsToMessage(gotWords, p.wordSize)
            println(s"hash('${m.str}') = ${gotHash.str}")
            assert (gotHash == expHash)
        }
        true
    }

    "Hardware Blake2 module should compute correct hash of ''" in {
        val p = Blake2Params(hashLen = 64, msgLen = 128)
        val m = Message("", wordSize = p.wordSize)
        testBlake2(p, m) 
    }

    "Hardware Blake2 module should compute correct hash of 'abc'" in {
        val p = Blake2Params(hashLen = 64, msgLen = 128)
        val m = Message("abc", wordSize = p.wordSize)
        testBlake2(p, m) 
    }

    "Hardware Blake2 module should compute correct hash of 'Chisel is too much fun!'" in {
        val p = Blake2Params(hashLen = 64, msgLen = 128)
        val m = Message("Chisel is too much fun!", wordSize = p.wordSize)
        testBlake2(p, m) 
    }
}
