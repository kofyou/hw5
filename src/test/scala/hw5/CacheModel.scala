package hw5

import CacheModel.CacheBlockModel
import scala.collection.mutable.ArrayBuffer


abstract class CacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) {
    require(p.bitsPerWord < 32)

    def isHit(addr: Int): Boolean
    def getReferenceToBlock(addr: Int): CacheBlockModel

    def findCacheAddressFields(addr: Int): (Int, Int, Int) = {
        def extractBits(hiIndex: Int, loIndex: Int): Int = {  // bit indices are inclusive (like Chisel/Verilog)
            val nativeLength = 32
            require(p.addrLen < nativeLength - 1)
            val lShamt = nativeLength - hiIndex - 1
            val rShamt = loIndex + lShamt
            (addr << lShamt) >>> rShamt
        }
        val offset = extractBits(p.numOffsetBits - 1, 0)
        val index =
            if (p.associativity * p.blockSize != p.capacity)
                extractBits(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
            else
                0
        val tag = extractBits(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
        (tag, index, offset)
    }

    // amazing
    def calcBlockAddr(tag: Int, index: Int): Int = (tag << p.numIndexBits) | index

    // a load request
    def read(addr: Int): Int = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        getReferenceToBlock(addr)(offset)
    }

    // a store request
    def write(addr: Int, wData: Int): Unit = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        getReferenceToBlock(addr)(offset) = wData
    }
}


class DMCacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends CacheModel(p, externalMem) {
    require(p.associativity == 1)
    // BEGIN SOLUTION
    val tags: ArrayBuffer[Int] = ArrayBuffer.fill(p.numSets)(0)
    val data: ArrayBuffer[CacheBlockModel] = ArrayBuffer.fill(p.numSets)(ArrayBuffer.fill(p.blockSize)(0))
    val valids: ArrayBuffer[Boolean] = ArrayBuffer.fill(p.numSets)(false)

    def isHit(addr: Int): Boolean = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        if (valids(index) == true && tags(index) == tag) {
            true
        } else {
            // compulsary miss or conflict/capacity miss
            false
        }
    }

    def getReferenceToBlock(addr: Int): CacheBlockModel = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        if (isHit(addr) == false) {
            // always write back
            if (valids(index)) {
                externalMem(calcBlockAddr(tags(index), index)) = data(index).clone()
            }
            // fetch whole line
            data(index) = externalMem(addr / p.blockSize).clone()
            tags(index) = tag
            valids(index) = true
        }
        data(index)
    }
}

abstract class SACacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends CacheModel(p, externalMem) {
    val wayParams = p.copy(capacity=p.capacity/p.associativity, associativity = 1)
    val ways = Seq.fill(p.associativity)(new DMCacheModel(wayParams, externalMem))
    val fillIndices = ArrayBuffer.fill(p.numSets)(0)

    // BEGIN SOLUTION
    def lookUpReplPolicy(index: Int): Int
    def updateReplPolicy(addr: Int, way: Int): Unit

    def lookUpFillPolicy(index: Int): Int = {
        fillIndices(index)
    }

    def updateFillPolicy(addr: Int): Unit = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        if (fillIndices(index) < p.associativity) {
            fillIndices(index) = fillIndices(index) + 1
        }
    }

    def wayToReplace(addr: Int): Int = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        if (fillIndices(index) < p.associativity) {
            lookUpFillPolicy(index)
        } else {
            lookUpReplPolicy(index)
        }
    }

    // external for tests
    def isHit(addr: Int): Boolean = {
        val theWay = ways.filter(_.isHit(addr) == true)
        if (theWay.size == 1) {
            true
        } else if(theWay.size == 0) {
            false
        } else {
            println("OH NO ISHIT BUGGY")
            false
        }
    }

    // internal for lookups
    def findHit(addr: Int) = {
        val theWay = ways.zipWithIndex.filter{ case(way, wayIndex) => way.isHit(addr) == true}
        if (theWay.size == 1) {
            (true, theWay.head._2)
        } else if(theWay.size == 0) {
            (false, -1)
        } else {
            println("OH NO ISHIT BUGGY")
            (false, -1)
        }
    }

    def getReferenceToBlock(addr: Int): CacheBlockModel = {
        // index of seq -> which slot of the sets
        val (isHit, hitWayIndex) = findHit(addr)
        if (isHit == false) {
            val wayReplaceIndex = wayToReplace(addr)
            // based on fill / repl, not always need to update two policies
            // cannot switch order
            updateReplPolicy(addr, wayReplaceIndex)
            updateFillPolicy(addr)
            ways(wayReplaceIndex).getReferenceToBlock(addr)
        } else {
            updateReplPolicy(addr, hitWayIndex)
            ways(hitWayIndex).getReferenceToBlock(addr)
        }
    }
}

class SARBCacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends SACacheModel(p, externalMem) {
    val replacementIndices = ArrayBuffer.fill(p.numSets)(0)

    def lookUpReplPolicy(index: Int): Int = {
        val replaceWay = replacementIndices(index)
        replacementIndices(index) = (replacementIndices(index) + 1) % p.associativity
        replaceWay
    }

    // lookUpReplPolicy does it all at once
    // would be better if i carefully distinguish two diffrent types of update
    def updateReplPolicy(addr: Int, way: Int): Unit = {}
}

class SALRUCacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends SACacheModel(p, externalMem) {
    val setWayUsage = Seq.fill(p.numSets)(ArrayBuffer.fill(p.associativity)(-1))

    // return the oldest index of block (way)
    def lookUpReplPolicy(index: Int): Int = {
        setWayUsage(index).indexOf(setWayUsage(index).max)
    }

    // uniformly update ranks
    def updateReplPolicy(addr: Int, way: Int): Unit = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        if (setWayUsage(index)(way) == -1) {
            assert(fillIndices(index) < p.associativity)
            for (wi <- 0 until way) {
                setWayUsage(index)(wi) += 1
            }
        } else {
            val theRank = setWayUsage(index)(way)
            // for those nonempty blocks, update their ranks comparatively
            for (wi <- 0 until p.associativity) {
                if (setWayUsage(index)(wi) < theRank) {
                    setWayUsage(index)(wi) += 1
                }
            }
        }
        setWayUsage(index)(way) = 0
    }
}

object CacheModel {
    type CacheBlockModel = ArrayBuffer[Int]

    def apply(p: CacheParams, replPolicy: String = "roundRobin")
             (externalMem: ArrayBuffer[CacheBlockModel] = ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0))): CacheModel = {
        if (replPolicy == "roundRobin") new SARBCacheModel(p, externalMem)
        else new SALRUCacheModel(p, externalMem)
    }
}
