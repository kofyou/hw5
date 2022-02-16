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
        val index = extractBits(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
        val tag = extractBits(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
        (tag, index, offset)
    }

    def calcBlockAddr(tag: Int, index: Int): Int = (tag << p.numIndexBits) | index

    def read(addr: Int): Int = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        getReferenceToBlock(addr)(offset)
    }

    def write(addr: Int, wData: Int): Unit = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        getReferenceToBlock(addr)(offset) = wData
    }
}


class DMCacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends CacheModel(p, externalMem) {
    require(p.associativity == 1)
    // BEGIN SOLUTION
    ???
}


class SACacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends CacheModel(p, externalMem) {
    val wayParams = p.copy(capacity=p.capacity/p.associativity, associativity = 1)
    val ways = Seq.fill(p.associativity)(new DMCacheModel(wayParams, externalMem))
    val replacementIndices = ArrayBuffer.fill(p.numSets)(0)
    
    // BEGIN SOLUTION
    ???

    def wayToReplace(addr: Int): Int = ???
}


object CacheModel {
    type CacheBlockModel = ArrayBuffer[Int]

    def apply(p: CacheParams)
             (externalMem: ArrayBuffer[CacheBlockModel] = ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0))): CacheModel = {
        if (p.associativity == 1) new DMCacheModel(p, externalMem)
        else new SACacheModel(p, externalMem)
    }
}
