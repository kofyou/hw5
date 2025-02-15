package hw5

import chisel3._
import chisel3.util._

case class CacheParams(capacity: Int, blockSize: Int, associativity: Int, replPolicy: String = "roundRobin", addrLen: Int = 8, bitsPerWord: Int = 8) {
	require((1 << addrLen) >= capacity)
	require(capacity >= blockSize)
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
		val wayToReplace = Output(UInt(p.associativity.W))
		val out = Valid(UInt(p.bitsPerWord.W))		// sets valid to true to indicate completion (even for writes)
	})

	// extract fields from address
	// amazing
	val tag    = io.in.bits.addr(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
	// val index  = io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
	// if the cache is fully-associative, it has no index bits
	val index  =
		if (p.associativity * p.blockSize != p.capacity)
			io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
		else
			0.U
	val offset = io.in.bits.addr(p.numOffsetBits - 1, 0)

	// essentially making a type alias to make it easy to declare
	def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

	// backing memory
	val extMem = Module(new MockDRAM(p))
}


class DMCache(p: CacheParams) extends Cache(p) {
	require(p.capacity > p.blockSize)
	require(p.associativity == 1)
  // BEGIN SOLUTION
	// quick ref: https://stackoverflow.com/questions/43646015/how-to-initialize-a-register-of-vectors
	// RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
	val valids = RegInit(VecInit(Seq.fill(p.numSets)(false.B)))
	// val valids = RegInit(Vec(p.numSets, false.B))
	// val valids = Reg(Vec(p.numSets, Bool()))
	val data = SyncReadMem(p.numSets, CacheBlock())
	val tags = SyncReadMem(p.numSets, UInt(p.numTagBits.W))

	// 0: ready, 1: lookup, 2: fetch
	val state = RegInit(0.U)
	
	val dataReg = Reg(CacheBlock())
	val tagReg = Reg(UInt(p.numTagBits.W))

	// just init
	io.in.ready := true.B
	io.hit := false.B
	io.wayToReplace := 0.U
	io.out.valid := false.B
	io.out.bits := 0.U

	extMem.io.rAddr := 0.U
	extMem.io.rEn := false.B
	extMem.io.wAddr := 0.U
	extMem.io.wEn := false.B
	extMem.io.wData := VecInit(Seq.fill(p.blockSize)(0.U))

	// ref: https://stackoverflow.com/questions/24340295/chisel-synchronous-read-memory

	// think of these variants, and should i read inside state 0?
	// dataReg := data.read(index, io.in.valid)
	// tagReg := tags.read(index, io.in.valid)

	// dataReg := data.read(index)
	// tagReg := tags.read(index)

	// dataReg := data.read(index, io.in.fire)
	// tagReg := tags.read(index, io.in.fire)

	val memReadWire = Wire(CacheBlock())
	val dataReadWire = Wire(CacheBlock())
	val tagReadWire = Wire(UInt(p.numTagBits.W))
	memReadWire := extMem.io.rData
	dataReadWire := data.read(index, io.in.fire)
	tagReadWire := tags.read(index, io.in.fire)
	dataReg := dataReadWire
	tagReg := tagReadWire

	when (state === 0.U) {
		io.in.ready := true.B
		io.hit := false.B
		io.out.valid := false.B
		extMem.io.wEn := false.B
		extMem.io.rEn := false.B

		when (io.in.fire) {
			state := 1.U
		}
	} .elsewhen (state === 1.U) {
		io.in.ready := false.B

		// for debug
		// val p = CacheParams(8, 4, 1, 4)
		// 2b offset, 1b index, 1b tag
		// addr: 0 ~ 15
		// 0 ()()()() {0,1,2,3 with tag 0} {8,9,10,11 with tag 1}
		// 1 ()()()() {4,5,6,7 with tag 0} {12,13,14,15 with tag 1}
		// printf("addr: %d, off: %d, index: %d, tag: %d, tagcmp: %d, valid: %d\n", io.in.bits.addr, offset, index, tag, tagReadWire, valids(index))
		// printf("wen: %d, wdata: %d\n", io.in.bits.write, io.in.bits.wData)
		// printf("wire: %d %d %d %d\n\n\n", dataReadWire(0.U),  dataReadWire(1.U),  dataReadWire(2.U),  dataReadWire(3.U))

		when (valids(index) === true.B && tag === tagReadWire) {
			io.hit := true.B

			// output for read or update for write
			when (io.in.bits.write) {
				dataReadWire(offset) := io.in.bits.wData
				data.write(index, dataReadWire)
			} .otherwise {
				io.out.bits := dataReadWire(offset)
			}
			// shouldn't here be a one-cycle delay?
			io.out.valid := true.B
			state := 0.U
		} .otherwise {
			io.hit := false.B

			extMem.io.rAddr := io.in.bits.addr / p.blockSize.U
			extMem.io.rEn := true.B

			state := 2.U
		}
	} .elsewhen (state === 2.U) {
		io.hit := false.B

		extMem.io.rEn := false.B
		io.in.ready := false.B
		// cache write back and write to cache
		when (valids(index)) {
			extMem.io.wAddr := (tagReg << p.numIndexBits.U) | index
			extMem.io.wEn := true.B
			extMem.io.wData := dataReg
			// printf("wb: %d %d %d %d to %d\n\n", dataReg(0.U),  dataReg(1.U),  dataReg(2.U),  dataReg(3.U), (tagReg << p.numIndexBits.U) | index)
		}
		when (io.in.bits.write) {
			memReadWire(offset) := io.in.bits.wData
		}
		// printf("br: %d %d %d %d from %d\n\n", memReadWire(0.U),  memReadWire(1.U),  memReadWire(2.U),  memReadWire(3.U), io.in.bits.addr / p.blockSize.U)
		data.write(index, memReadWire)
		tags.write(index, tag)
		valids(index) := true.B

		when (io.in.bits.write === false.B) {
			io.out.bits := memReadWire(offset)
		}
		io.out.valid := true.B
		state := 0.U
	}
}
