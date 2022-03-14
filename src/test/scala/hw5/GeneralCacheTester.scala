// Acknowledgement: 
// This files uses CacheTester.scala as a template.
// Thank you for the testing harness!, Amogh and Prof. Beamer!

package hw5

import chisel3._
import chiseltest._
import chisel3.util._
import chisel3.experimental.VecLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class GeneralCacheTester extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "DMCacheWay"
    it should "be able to write & read it back" in {
        val p = CacheParams(32, 4, 1)
        test(new DMCacheWay(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            val addr = 4
            val block = Vec.Lit(0.U(p.bitsPerWord.W), 1.U(p.bitsPerWord.W), 2.U(p.bitsPerWord.W), 3.U(p.bitsPerWord.W))

            dut.io.in.ready.expect(true.B)
            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.addr.poke(addr.U)
            dut.io.in.bits.write.poke(true.B)
            dut.io.in.bits.wLine.poke(block)
            dut.clock.step()

            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.addr.poke(addr.U)
            dut.io.in.bits.write.poke(false.B)
            dut.clock.step()

            dut.io.out.valid.expect(true.B)
            dut.io.out.bits.rTag.expect(0.U)
            dut.io.out.bits.rLine.expect(block)
            dut.io.out.bits.validLine.expect(true.B)
            dut.io.out.bits.hit.expect(true.B)
            dut.clock.step()

            dut.io.in.ready.expect(true.B)
        }
    }

    def performReadTest(dut: Cache, m: CacheModel, addr: Int): Unit = {
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.addr.poke(addr.U)
        dut.io.in.bits.write.poke(false.B)
        dut.io.in.ready.expect(true.B)

        dut.clock.step()
        dut.io.in.valid.poke(false.B)
        dut.io.in.ready.expect(false.B)
        if (m.isHit(addr)) {
            dut.io.hit.expect(true.B)
        } else {
            dut.io.hit.expect(false.B)
            dut.clock.step()
        }
        m.read(addr)

        dut.io.out.valid.expect(true.B)
        dut.io.in.ready.expect(false.B)
        dut.io.out.bits.expect(m.read(addr).U)

        dut.clock.step()
        dut.io.out.valid.expect(false.B)
        dut.io.in.ready.expect(true.B)
    }

    def performWriteTest(dut: Cache, m: CacheModel, addr: Int, wData: Int): Unit = {
        require(wData < (1 << dut.p.bitsPerWord))
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.addr.poke(addr.U)
        dut.io.in.bits.write.poke(true.B)
        dut.io.in.bits.wData.poke(wData.U)
        dut.io.in.ready.expect(true.B)

        dut.clock.step()
        dut.io.in.valid.poke(false.B)
        dut.io.in.ready.expect(false.B)
        if (m.isHit(addr)) {
            dut.io.hit.expect(true.B)
        } else {
            dut.io.hit.expect(false.B)
            dut.io.wayToReplace.expect(m.wayToReplace(addr).U)
            dut.clock.step()
        }
        m.write(addr, wData)

        dut.io.out.valid.expect(true.B)
        dut.io.in.ready.expect(false.B)

        dut.clock.step()
        dut.io.out.valid.expect(false.B)
        dut.io.in.ready.expect(true.B)
    }


    def performGeneralTest(p : CacheParams, replPolicy: String) = {
        it should "be able to read (miss, then hit) a block" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performReadTest(dut, m, 8)
                performReadTest(dut, m, 8)
            }
        }

        it should "be able to write miss then read hit a block" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performWriteTest(dut, m, 8, 8)
                performReadTest(dut, m, 8)
            }
        }

        it should "load in a block" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                val addr = 4
                // first miss to bring block in
                performReadTest(dut, m, addr)
                // now all hits
                for (w <- 0 until p.blockSize) {
                    performReadTest(dut, m, addr+w)
                }
            }
        }

        it should "be able to write to all words and then read all in cache" in {
            // val p = CacheParams(32, 4, 1)
            // val p = CacheParams(8, 4, 1, 4)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                for(addr <- 0 until (1 << p.addrLen)) {
                    performWriteTest(dut, m, addr, addr)
                }
                for(addr <- 0 until (1 << p.addrLen)) {
                    performReadTest(dut, m, addr)
                }
            }
        }

        it should "handle thrashing 0 -> 32 -> 0" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performReadTest(dut, m, 0)        // Read miss to addr 0 at block 0
                performWriteTest(dut, m, 1, 1)    // Write hit to addr 0 at block 0
                performWriteTest(dut, m, 32, 32)  // Write miss to addr 32 at block 0
                performWriteTest(dut, m, 1, 1)    // Read miss to addr 0 at block 0
            }
        }

        it should "handle random accesses" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(GeCache(p, replPolicy)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                for(round <- 0 until 2 * (1 << p.addrLen)) {
                    // ref: https://stackoverflow.com/q/39402567/15670192
                    // [0, 1 << p.addrLen)
                    val addr = Random.between(0, 1 << p.addrLen)
                    // [0, 1)
                    val read = Random.between(0, 2)
                    if (read == 1) {
                        performReadTest(dut, m, addr)
                    } else {
                        performWriteTest(dut, m, addr, addr)
                    }
                }
            }
        }
    }

    def performGeneralTestParams(p: CacheParams) = {
        behavior of "roundRobin Direct-Mapped GeCache General Functionality"
        performGeneralTest(p, replPolicy = "roundRobin")

        behavior of "roundRobin Fully-Associative GeCache General Functionality"
        performGeneralTest(p.copy(associativity = p.numSets), replPolicy = "roundRobin")

        behavior of "roundRobin Set-Associative GeCache General Functionality"
        performGeneralTest(p.copy(associativity = p.numSets / 2), replPolicy = "roundRobin")

        behavior of "LRU Direct-Mapped GeCache General Functionality"
        performGeneralTest(p, replPolicy = "LRU")

        behavior of "LRU Fully-Associative GeCache General Functionality"
        performGeneralTest(p.copy(associativity = p.numSets), replPolicy = "LRU")

        behavior of "LRU Set-Associative GeCache General Functionality"
        performGeneralTest(p.copy(associativity = p.numSets / 2), replPolicy = "LRU")
    }

    // TODO: duplicate test name
    // behavior of "GeCache CacheParams(8, 1, x)"
    // performGeneralTestParams(p = CacheParams(8, 2, 1))
    behavior of "GeCache CacheParams(32, 4, x)"
    performGeneralTestParams(p = CacheParams(32, 4, 1))
    // behavior of "GeCache CacheParams(128, 4, x)"
    // performGeneralTestParams(p = CacheParams(128, 4, 1))

    behavior of "LRU GeCache Replacement"
    it should "replace first half non-valid, update order, replace second half, and then evict the eldest" in {
        val p = CacheParams(32, 4, 8)
        val m = CacheModel(p, "LRU")()
        test(GeCache(p, "LRU")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

            for(addr <- 0 until (1 << p.addrLen)) {
                performWriteTest(dut, m, addr, addr)
            }
            for(addr <- 0 until (1 << p.addrLen)) {
                performReadTest(dut, m, addr)
            }

            // fill up first half blocks in a set in order
            for (w <- 0 until p.associativity / 2) {
                val addr = w * p.numSets * p.blockSize
                performReadTest(dut, m, addr)
            }

            // flip first half order
            for (w <- p.associativity / 2 - 1 to 0 by -1) {
                val addr = w * p.numSets * p.blockSize
                performReadTest(dut, m, addr)
            }

            // fill up second half blocks in a set in order
            for (w <- p.associativity / 2 until p.associativity) {
                val addr = w * p.numSets * p.blockSize
                performReadTest(dut, m, addr)
            }

            // evict
            val evictSeq = Seq(1,0,2,3)
            for (w <- 0 until p.associativity) {
                val addr = w * p.numSets * p.blockSize + p.capacity
                performReadTest(dut, m, addr)
            }
        }
    }
}
