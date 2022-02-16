package hw5

import chisel3._
import chisel3.util._

case class CacheParams(capacity: Int, blockSize: Int, associativity: Int, addrLen: Int = 8, bitsPerWord: Int = 8) {
	require((1 << addrLen) >= capacity)
	require(capacity > blockSize)
	require(isPow2(capacity) && isPow2(blockSize) && isPow2(associativity) && isPow2(bitsPerWord))
	// inputs capacity & blockSize are in units of words

	val numExtMemBlocks = (1 << addrLen) / blockSize
	val memBlockAddrBits = log2Ceil(numExtMemBlocks)

	val numSets = capacity / blockSize / associativity
	val numOffsetBits = log2Ceil(blockSize)
	val numIndexBits = log2Ceil(numSets)
	val numTagBits = addrLen - (numOffsetBits + numIndexBits)
}


class MockDRAM(p: CacheParams) extends Module {
	def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

	// addresses in terms of blocks
	val io = IO(new Bundle {
		val rAddr = Input(UInt(p.memBlockAddrBits.W))
		val rEn = Input(Bool())
		val rData = Output(CacheBlock())
		val wAddr = Input(UInt(p.memBlockAddrBits.W))
		val wEn = Input(Bool())
		val wData = Input(CacheBlock())
	})
	// Fixed memory latency of 1 cycle
	val dram = SyncReadMem(p.numExtMemBlocks, CacheBlock())
	io.rData := DontCare
	when (io.rEn) {
		io.rData := dram(io.rAddr)
	}
	when (io.wEn) {
		dram(io.wAddr) := io.wData
	}
}


class Cache(val p: CacheParams) extends Module {
	val io = IO(new Bundle {
		val in = Flipped(Decoupled(new Bundle {
			val addr = UInt(p.addrLen.W)
			val write = Bool()
			val wData = UInt(p.bitsPerWord.W)
		}))
		val hit = Output(Bool())									// helpful for testing
		val out = Valid(UInt(p.bitsPerWord.W))		// sets valid to true to indicate completion (even for writes)
	})

	// extract fields from address
	val tag    = io.in.bits.addr(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
	val index  = io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
	val offset = io.in.bits.addr(p.numOffsetBits - 1, 0)

	// essentially making a type alias to make it easy to declare
	def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

	// backing memory
	val extMem = Module(new MockDRAM(p))
}


class DMCache(p: CacheParams) extends Cache(p) {
	require(p.associativity == 1)
  // BEGIN SOLUTION
  ???
}
