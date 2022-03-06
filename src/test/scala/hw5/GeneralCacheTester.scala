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
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performReadTest(dut, m, 8)
                performReadTest(dut, m, 8)
            }
        }

        it should "be able to write miss then read hit a block" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performWriteTest(dut, m, 8, 8)
                performReadTest(dut, m, 8)
            }
        }

        it should "load in a block" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
                performReadTest(dut, m, 0)        // Read miss to addr 0 at block 0
                performWriteTest(dut, m, 1, 1)    // Write hit to addr 0 at block 0
                performWriteTest(dut, m, 32, 32)  // Write miss to addr 32 at block 0
                performWriteTest(dut, m, 1, 1)    // Read miss to addr 0 at block 0
            }
        }

        it should "handle random accesses" in {
            // val p = CacheParams(32, 4, 1)
            val m = CacheModel(p, replPolicy)()
            test(new GeCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

    behavior of "Direct-Mapped GeneralCache"
    performGeneralTest(p = CacheParams(32, 4, 1), replPolicy = "roundRobin")

    behavior of "Fully-Associative GeneralCache"
    performGeneralTest(p = CacheParams(32, 4, 8), replPolicy = "roundRobin")

    behavior of "Set-Associative GeneralCache"
    performGeneralTest(p = CacheParams(32, 4, 2), replPolicy = "roundRobin")
}
