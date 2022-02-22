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

    def isValid(addr: Int): Boolean = {
        val (tag, index, offset) = findCacheAddressFields(addr)
        valids(index)
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

class SACacheModel(p: CacheParams, externalMem: ArrayBuffer[CacheBlockModel]) extends CacheModel(p, externalMem) {
    val wayParams = p.copy(capacity=p.capacity/p.associativity, associativity = 1)
    val ways = Seq.fill(p.associativity)(new DMCacheModel(wayParams, externalMem))
    val replacementIndices = ArrayBuffer.fill(p.numSets)(0)
    
    // BEGIN SOLUTION
    def wayToReplace(addr: Int): Int = {
        val idleWaysWithIndex = ways.zipWithIndex.filter{ case(way, wayIndex) => way.isValid(addr) == false}
        // val idleWaysWithIndex = ways.zipWithIndex.filter( case(way, wayIndex) => way.isValid(addr) == false)
        // illegal start of simple expression
        if (idleWaysWithIndex.size > 0) {
            idleWaysWithIndex.head._2
        } else {
            val (tag, index, offset) = findCacheAddressFields(addr)
            val returnIndex = replacementIndices(index)
            replacementIndices(index) = (replacementIndices(index) + 1) % p.associativity
            returnIndex
        }
    }

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

    def getReferenceToBlock(addr: Int): CacheBlockModel = {
        // index of seq -> which slot of the sets
        // for(isHit == true) -> seq index
        if (isHit(addr) == false) {
            val wayReplaceIndex = wayToReplace(addr)
            // will miss and replace
            ways(wayReplaceIndex).getReferenceToBlock(addr)
        } else {
            val theWay = ways.filter(_.isHit(addr) == true)
            // will hit
            theWay.head.getReferenceToBlock(addr)
        }
    }
}


object CacheModel {
    type CacheBlockModel = ArrayBuffer[Int]

    def apply(p: CacheParams)
             (externalMem: ArrayBuffer[CacheBlockModel] = ArrayBuffer.fill(p.numExtMemBlocks)(ArrayBuffer.fill(p.blockSize)(0))): CacheModel = {
        if (p.associativity == 1) new DMCacheModel(p, externalMem)
        else new SACacheModel(p, externalMem)
    }
}
