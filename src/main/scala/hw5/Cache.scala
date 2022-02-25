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
	// amazing
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
	// quick ref: https://stackoverflow.com/questions/43646015/how-to-initialize-a-register-of-vectors
	// RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
	val valids = RegInit(VecInit(Seq.fill(p.numSets)(false.B)))
	// val valids = RegInit(Vec(p.numSets, false.B))
	// val valids = Reg(Vec(p.numSets, Bool()))
	// why not syncmem of syncmem
	val data = SyncReadMem(p.numSets, CacheBlock())
	val tags = SyncReadMem(p.numSets, UInt(p.numTagBits.W))


	val state = RegInit(0.U)
	// 0: ready, 1: lookup, 2: fetch
	// val isHit = RegInit(false.B)
	// val memdata = RegInit(UInt(p.bitsPerWord.W))
	// val cachedata = RegInit(UInt(p.bitsPerWord.W))
	//reg vec and reginit?
	// val memdata = Reg(CacheBlock())
	val cachedata = Reg(CacheBlock())
	val tagdata = Reg(UInt(p.numTagBits.W))
	val syncReadMemAddr = Reg(UInt(p.numIndexBits.W))

	// just init
	io.in.ready := true.B
	io.hit := false.B
	io.out.valid := false.B
	io.out.bits := 0.U

	extMem.io.rAddr :=  0.U // Input(UInt(p.memBlockAddrBits.W))
	extMem.io.rEn := false.B // Input(Bool())
	// extMem. rData = Output(CacheBlock())
	extMem.io.wAddr := 0.U // Input(UInt(p.memBlockAddrBits.W))
	extMem.io.wEn := false.B // Input(Bool())
	// Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))
	extMem.io.wData := VecInit(Seq.fill(p.blockSize)(0.U)) // Input(CacheBlock())

	// ref: https://stackoverflow.com/questions/24340295/chisel-synchronous-read-memory

	// cachedata := data.read(index, io.in.valid)
	// tagdata := tags.read(index, io.in.valid)

	// cachedata := data.read(index)
	// tagdata := tags.read(index)

	// cachedata := data.read(index, io.in.fire)
	// tagdata := tags.read(index, io.in.fire)

	val memdataWire = Wire(CacheBlock())
	val cachedataWire = Wire(CacheBlock())
	val tagdataWire = Wire(UInt(p.numTagBits.W))
	memdataWire := extMem.io.rData
	cachedataWire := data.read(index, io.in.fire)
	tagdataWire := tags.read(index, io.in.fire)
	cachedata := cachedataWire
	tagdata := tagdataWire

	when (state === 0.U) {
		io.in.ready := true.B
		io.hit := false.B
		io.out.valid := false.B
		extMem.io.wEn := false.B // Input(Bool())
		extMem.io.rEn := false.B // Input(Bool())

		// cachedata := data.read(index)
		// tagdata := tags.read(index)
		when (io.in.fire) {
			// basically isHit() from the Model
			// printf("gonna read from: %d\n", index)
			// syncReadMemAddr := index
			// cachedata := data.read(index, io.in.fire)
			// tagdata := tags.read(index, io.in.fire)
			// printf("data: %d %d %d %d\n\n", cachedata(0.U),  cachedata(1.U),  cachedata(2.U),  cachedata(3.U))
			// cachedata := data.read(index, io.in.valid)
			// tagdata := tags.read(index, io.in.valid)
			// syncReadMemAddr := index
			// when (valids(index) === true.B && tag === tags.read(index)) {
			// // TODO: not very good...?
			// 	when(io.in.bits.write) {
			// 		cachedata(offset) := io.in.bits.wData
			// 	}
			// 	isHit := true.B
			// } .otherwise {
			// 	isHit := false.B
			// }
			state := 1.U
		}
	} .elsewhen (state === 1.U) {
		io.in.ready := false.B
		// io.hit := isHit

		// val p = CacheParams(8, 4, 1, 4)
		// 2b offset, 1b index, 1b tag
		// addr: 0 ~ 15
		// 0 ()()()() {0,1,2,3 with tag 0} {8,9,10,11 with tag 1}
		// 1 ()()()() {4,5,6,7 with tag 0} {12,13,14,15 with tag 1}

		printf("addr: %d, off: %d, index: %d, tag: %d, tagcmp: %d, valid: %d\n", io.in.bits.addr, offset, index, tag, tagdataWire, valids(index))
		printf("wen: %d, wdata: %d\n", io.in.bits.write, io.in.bits.wData)
		// printf("data: %d %d %d %d\n\n", cachedata(0.U),  cachedata(1.U),  cachedata(2.U),  cachedata(3.U))
		printf("wire: %d %d %d %d\n\n\n", cachedataWire(0.U),  cachedataWire(1.U),  cachedataWire(2.U),  cachedataWire(3.U))
		// printf("tag0: %d, tag1: %d, ", tags.read(0.U), tags.read(1.U))
		// printf("index: %d, tags(index): %d, ", index, tags.read(index))
		// printf("index === 1.U: %d\n", index === 1.U

		when (valids(index) === true.B && tag === /*tagdata*/tagdataWire) {
		// TODO: not very good...?
			// cannot do this
			// when(io.in.bits.write) {
			// 	cachedata(offset) := io.in.bits.wData
			// }
			// isHit := true.B
			io.hit := true.B

			// output for read or update for write
			when (io.in.bits.write) {
				// Ahhhhhhhhh
				// cachedata
				// val towriteUInt = cachedata.asUInt >> ((offset + 1.U) * p.bitsPerWord.U) << ((offset + 1.U) * p.bitsPerWord.U) | io.in.bits.wData << (offset * p.bitsPerWord.U) | cachedata.asUInt << ((p.blockSize.U - offset - 1.U) * p.bitsPerWord.U) >> ((p.blockSize.U - offset - 1.U) * p.bitsPerWord.U)
				// printf("towrite: %d %d %d %d\n", towrite(0.U), towrite(1.U), towrite(2.U), towrite(3.U))
				// val towriteVec = VecInit(Seq.tabulate(p.blockSize) { i => if ()i.U } )
				// val towrite = Wire(Vec(p.blockSize, UInt(p.bitsPerWord.W)))
				// towrite := cachedata
				cachedataWire(offset) := io.in.bits.wData
				data.write(index, cachedataWire)
				// data.write(index, cachedata)
			} .otherwise {
				io.out.bits := cachedataWire(offset)
			}
			// shouldn't here be a one-cycle delay?
			io.out.valid := true.B
			// io.hit := isHit
			state := 0.U

		} .otherwise {
			// isHit := false.B
			io.hit := false.B

			// read from mem
			// TODO: addr
			// // fetch whole line
			// data(index) = externalMem(addr / p.blockSize).clone()
			// TODO: fix extMem
			// memdata := extMem.read(io.in.bits.addr / p.blockSize.U)
			extMem.io.rAddr := io.in.bits.addr / p.blockSize.U // Input(UInt(p.memBlockAddrBits.W))
			extMem.io.rEn := true.B // Input(Bool())
			// memdata := extMem.io.rData // Output(CacheBlock())

			state := 2.U

		}

		// a miss
		/*
		when (isHit === false.B) {
			// read from mem
			// TODO: addr
			// // fetch whole line
			// data(index) = externalMem(addr / p.blockSize).clone()
			// TODO: fix extMem
			// memdata := extMem.read(io.in.bits.addr / p.blockSize.U)
			extMem.io.rAddr := io.in.bits.addr / p.blockSize.U // Input(UInt(p.memBlockAddrBits.W))
			extMem.io.rEn := true.B // Input(Bool())
			memdata := extMem.io.rData // Output(CacheBlock())
			// TODO: not very good..?
			when (io.in.bits.write) {
				memdata(offset) := io.in.bits.wData
			}

			state := 2.U
		} .otherwise {
			// output for read or update for write
			when (io.in.bits.write) {
				// Ahhhhhhhhh
				// cachedata
				// data.write(index, | io.in.bits.wData | )
				data.write(index, cachedata)
			} .otherwise {
				io.out.bits := cachedata(offset)
			}
			// shouldn't here be a one-cycle delay?
			io.out.valid := true.B
			io.hit := isHit
			state := 0.U
		}*/
	} .elsewhen (state === 2.U) {
		extMem.io.rEn := false.B
		io.in.ready := false.B
		// cache write back and write to cache
		when (valids(index)) {
			// TODO: fix addr
			// extMem.write(io.in.bits.addr, cachedata)
			// extMem.write((tag << p.numIndexBits.U) | index, cachedata)

			extMem.io.wAddr := (tagdata << p.numIndexBits.U) | index // Input(UInt(p.memBlockAddrBits.W))
			extMem.io.wEn := true.B // Input(Bool())
			extMem.io.wData := cachedata// Input(CacheBlock())
			printf("wb: %d %d %d %d to %d\n\n", cachedata(0.U),  cachedata(1.U),  cachedata(2.U),  cachedata(3.U), (tagdata << p.numIndexBits.U) | index)
		}
		// TODO: not very good..?
		when (io.in.bits.write) {
			memdataWire(offset) := io.in.bits.wData
		}
		printf("br: %d %d %d %d from %d\n\n", memdataWire(0.U),  memdataWire(1.U),  memdataWire(2.U),  memdataWire(3.U), io.in.bits.addr / p.blockSize.U)
		data.write(index, memdataWire)
		tags.write(index, tag)
		valids(index) := true.B

		// output for read or update for write
		// when (io.in.bits.write) {
		// 	// Ahhhhhhhhhhhhhhhhhhhhhhhhh
		// 	memdata
		// 	data.write(index, | io.in.bits.wData | )
		// } .otherwise {
		// 	io.out.bits := cachedata(offset)
		// }
		when (io.in.bits.write === false.B) {
			io.out.bits := memdataWire(offset)
		}
		io.out.valid := true.B
		// io.hit := isHit
		io.hit := false.B
		state := 0.U
	}
}
