package hw5

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer


class CacheModelTester extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "CacheModel"

    it should "Split address into proper fields" in {
        val p = CacheParams(128, 4, 1)
        val m = CacheModel(p)()
        assert(m.findCacheAddressFields(133) == (1, 1, 1))
    }

    it should "Split address into proper fields for Set-Associative Cache (4-way)" in {
        val p = CacheParams(128, 4, 4)
        val m = CacheModel(p)()
        assert(m.findCacheAddressFields(133) == (4, 1, 1))
    }

    def testRead(m: CacheModel, addr: Int, expValue: Int, expHit: Boolean): Unit = {
        assert(m.isHit(addr) == expHit)
        assert(m.read(addr) == expValue)
        assert(m.isHit(addr))       // should be there after access
    }

    def testWrite(m: CacheModel, addr: Int, wData: Int, expHit: Boolean): Unit = {
        assert(m.isHit(addr) == expHit)
        m.write(addr, wData)
        assert(m.read(addr) == wData)
        assert(m.isHit(addr))       // should be there after access
    }


    // these are no longer testing DMCacheModel
    behavior of "DMCacheModel"
    it should "transfer block from main memory for a write and then a read" in {
        val p = CacheParams(128, 4, 1)
        val m = CacheModel(p)()

        // Write miss to block 1
        testWrite(m, 4, 1, false)

        // Read miss to block 0
        testRead(m, 4, 1, true)
    }

    it should "load in all words of a block" in {
        val p = CacheParams(128, 4, 1)
        val m = CacheModel(p)()

        // first all misses
        for (w <- 0 until p.associativity) {
            assert(!m.isHit(w))
        }

        // Read block 0
        testRead(m, 0, 0, false)

        // Now all hits
        for (w <- 0 until p.associativity) {
            assert(m.isHit(w))
        }
    }

    it should "be able to set all words to 1 and read result in cache" in {
        val p = CacheParams(128, 4, 1)
        val m = CacheModel(p)()

        for(addr <- 0 until (1 << p.addrLen)) {
            m.write(addr, addr)
            testRead(m, addr, addr, true)
        }
    }

    it should "handle thrashing 0 -> 32 -> 0" in {
        val p = CacheParams(32, 4, 1)
        val m = CacheModel(p)()

        // Read miss to block 0
        testRead(m, 0, 0, false)

        // Write hit to block 0
        testWrite(m, 0, 1, true)

        // Write hit to block 32
        testWrite(m, 32, 32, false)

        // Read miss to block 0
        testRead(m, 0, 1, false)
    }


    def testSACacheModel(replPolicy: String) = {
    it should "transfer block from main memory for a write and then a read (SA)" in {
        val p = CacheParams(128, 4, 4)
        val m = CacheModel(p, replPolicy)()

        // Read miss to block 0
        testRead(m, 0, 0, false)

        // Write miss to block 1
        testWrite(m, 4, 1, false)
    }

    it should "load in all words of a block (SA)" in {
        val p = CacheParams(128, 4, 4)
        val m = CacheModel(p, replPolicy)()

        // first all misses
        for (w <- 0 until p.associativity) {
            assert(!m.isHit(w))
        }

        // Read block 0
        testRead(m, 0, 0, false)

        // Now all hits
        for (w <- 0 until p.associativity) {
            assert(m.isHit(w))
        }
    }

    it should "be able to set all words to 1 and read result in cache (SA)" in {
        val p = CacheParams(128, 4, 4)
        val m = CacheModel(p, replPolicy)()

        for(addr <- 0 until (1 << p.addrLen)) {
            m.write(addr, addr)
            testRead(m, addr, addr, true)
        }
    }

    it should "handle thrashing 0 -> 16 -> 0" in {
        val p = CacheParams(32, 4, 2)
        val m = CacheModel(p, replPolicy)()

        // Read miss to block 0
        testRead(m, 0, 0, false)

        // Write hit to block 0
        testWrite(m, 0, 1, true)

        // Write miss to block 16
        testWrite(m, 16, 16, false)

        // Read hit to block 0
        testRead(m, 0, 1, true)
    }

    it should "handle thrashing 0 -> 16 -> 32 -> 0" in {
        val p = CacheParams(32, 4, 2)
        val m = CacheModel(p, replPolicy)()

        // Read miss to block 0
        testRead(m, 0, 0, false)

        // Write hit to block 0
        testWrite(m, 0, 1, true)

        // Write miss to block 16
        testWrite(m, 16, 16, false)

        // Write hit to block 32
        testWrite(m, 32, 32, false)

        // Read miss to block 0
        testRead(m, 0, 1, false)
    }

    it should "replace first non-valid, and then evict in order for round-robin or LRU" in {
        val p = CacheParams(128, 4, 4)
        // val m = new SARBCacheModel(p, ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0)))
        val m = CacheModel(p, replPolicy)()

        // fill up all blocks in a set in order
        for (w <- 0 until p.associativity) {
            // line in one set are not continuous babe
            val addr = w * p.numSets * p.blockSize
            assert(m.wayToReplace(addr) == w)
            testRead(m, addr, 0, false)
        }

        // find replacement slots all valid going round robin
        for (w <- 0 until p.associativity) {
            // add capacity, so tags are different
            val addr = w * p.numSets * p.blockSize + p.capacity
            assert(m.wayToReplace(addr) == w)
            testRead(m, addr, 0, false)
        }
    }

    }

    behavior of "RoundRobin SACacheModel General Functionality"
    testSACacheModel(replPolicy = "roundRobin")

    behavior of "LRU SACacheModel General Functionality"
    // use linkedHashMap to test the model
    testSACacheModel(replPolicy = "LRU")

    behavior of "LRU SACacheModel Replacement"
    it should "replace first non-valid, and then evict the eldest" in {
        val p = CacheParams(128, 4, 4)
        // val m = new SARBCacheModel(p, ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0)))
        val m = CacheModel(p, "LRU")()

        // fill up all blocks in a set in order
        for (w <- 0 until p.associativity) {
            // line in one set are not continuous babe
            val addr = w * p.numSets * p.blockSize
            assert(m.wayToReplace(addr) == w)
            testRead(m, addr, 0, false)
        }

        // age: 3,2,1,0 -> 3,2,1,0 -> 3,2,0,1 -> 3,0,1,2 -> 0,1,2,3
        for (w <- p.associativity - 1 to 0 by -1) {
            val addr = w * p.numSets * p.blockSize
            testRead(m, addr, 0, true)
            if (w != 0) {
                assert(m.wayToReplace(addr) == 0)
            } else {
                assert(m.wayToReplace(addr) == p.associativity - 1)
            }
        }

        // replace index 3,2,1,0 in order
        for (w <- 0 until p.associativity) {
            // add capacity, so tags are different
            val addr = w * p.numSets * p.blockSize + p.capacity
            assert(m.wayToReplace(addr) == p.associativity - 1 - w)
            testRead(m, addr, 0, false)
        }
    }

    it should "replace first half non-valid, update order, replace second half, and then evict the eldest" in {
        val p = CacheParams(128, 4, 4)
        // val m = new SARBCacheModel(p, ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0)))
        val m = CacheModel(p, "LRU")()

        // fill up first half blocks in a set in order
        for (w <- 0 until p.associativity / 2) {
            // line in one set are not continuous babe
            val addr = w * p.numSets * p.blockSize
            assert(m.wayToReplace(addr) == w)
            testRead(m, addr, 0, false)
        }

        // age: 1,0,4,4 -> 1,0,4,4 -> 0,1,4,4
        for (w <- p.associativity / 2 - 1 to 0 by -1) {
            val addr = w * p.numSets * p.blockSize
            testRead(m, addr, 0, true)
            assert(m.wayToReplace(addr) == p.associativity / 2)
        }

        // fill up second half blocks in a set in order
        // age: 0,1,4,4 -> 1,2,0,4 -> 2,3,1,0
        for (w <- p.associativity / 2 until p.associativity) {
            // line in one set are not continuous babe
            val addr = w * p.numSets * p.blockSize
            assert(m.wayToReplace(addr) == w)
            testRead(m, addr, 0, false)
        }

        // replace index 1,0,2,3 in order
        val evictSeq = Seq(1,0,2,3)
        for (w <- 0 until p.associativity) {
            // add capacity, so tags are different
            val addr = w * p.numSets * p.blockSize + p.capacity
            assert(m.wayToReplace(addr) == evictSeq(w))
            testRead(m, addr, 0, false)
        }
    }
}
