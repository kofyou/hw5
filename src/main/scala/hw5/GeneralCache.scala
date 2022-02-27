package hw5

import chisel3._
import chisel3.util._

// for storing data
class DMCacheSet(p: CacheParams) extends Module {
    // essentially making a type alias to make it easy to declare
    def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val addr = UInt(p.addrLen.W)
            val write = Bool()
            // the whole line
            val wLine = CacheBlock()
        }))
        val out = Valid(new Bundle {
            val rTag = Output(UInt(p.numTagBits.W))
            // the whole line
            val rLine = CacheBlock()
            // new data valid bit
            val validLine = Output(Bool())
            // TODO: is hit available
            val hit = Output(Bool())
        })		// sets valid to true to indicate completion (even for writes)
    })

    // extract fields from address
    // amazing
    val tag    = io.in.bits.addr(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
    val index  = io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
    val offset = io.in.bits.addr(p.numOffsetBits - 1, 0)

    require(p.associativity == 1)
  
    // BEGIN SOLUTION
    val valids = RegInit(VecInit(Seq.fill(p.numSets)(false.B)))
    val data = SyncReadMem(p.numSets, CacheBlock())
    val tags = SyncReadMem(p.numSets, UInt(p.numTagBits.W))

    // 0: ready & (write / lookup) 2: return lookup
    val state = RegInit(0.U)

    // just init
    // TODO: wrap reset?
    io.in.ready := true.B
    io.out.valid := false.B
    io.out.bits.hit := false.B
    io.out.bits.validLine := false.B
    io.out.bits.rTag := 0.U
    io.out.bits.rLine := VecInit(Seq.fill(p.blockSize)(0.U))

    val dataReadWire = Wire(CacheBlock())
    val tagReadWire = Wire(UInt(p.numTagBits.W))
    dataReadWire := data.read(index, io.in.fire && !io.in.bits.write)
    tagReadWire := tags.read(index, io.in.fire && !io.in.bits.write)

    when (state === 0.U) {
        // TODO
        io.in.ready := true.B
        io.out.valid := false.B
        io.out.bits.hit := false.B
        io.out.bits.validLine := false.B

        when (io.in.fire) {
            when (io.in.bits.write) {
                // no output for write
                data.write(index, io.in.bits.wLine)
                tags.write(index, tag)
                valids(index) := true.B
            } .otherwise {
                state := 1.U
            }
        }
    } .elsewhen (state === 1.U) {
        io.in.ready := false.B
        when (valids(index) === true.B && tag === tagReadWire) {
            io.out.bits.hit := true.B
        } .otherwise {
            io.out.bits.hit := false.B
        }
        io.out.valid := true.B
        io.out.bits.validLine := valids(index)
        io.out.bits.rTag := tagReadWire
        io.out.bits.rLine := dataReadWire

        state := 0.U
    }
}

// for control
class GeCache(p: CacheParams) extends Cache(p) {
    // TODO: seq or vec?
    // seq only takes scala index? if indexing by chisel should use vec?
    val setSeq = Seq.fill(p.numSets)(Module(new DMCacheSet(p)))
    val setSeqIOVec = VecInit(setSeq.map(_.io))

    // ref: https://stackoverflow.com/questions/62809878/how-to-create-a-array-vec-of-chisel-modules
    // val vec_of_elements = Vec.fill(n) {Module(new MyElement(my_args)).io }
    // "In general you only need a Vec if you want to use hardware based indexing of the elements,
    // or your Elements are part of an IO. If you don't need that you can just use a Scala collection like Seq,
    // Array or similar."
    // val setVec = Vec.fill(p.numSets) {Module(new DMCacheSet(p)).io}

    // ref: https://stackoverflow.com/questions/33621533/how-to-do-a-vector-of-modules
    // val vec_of_elements = Vec(10, Module(SaturatingCounter(4)).io)
    // val setVec = Vec(p.numSets, Module(new DMCacheSet(p)).io)

    // 0: ready, 1: lookup, 2: fetch
    val state = RegInit(0.U)
    
    // TODO: how to store the right value. Or have to use more regs?
    val dataReg = Reg(CacheBlock())
    val tagReg = Reg(UInt(p.numTagBits.W))
    val wbReg = Reg(Bool())
    val replSetIndexReg = UInt(log2Ceil(p.numSets).W)

    // just init
    io.in.ready := true.B
    io.hit := false.B
    io.out.valid := false.B
    io.out.bits := 0.U

    extMem.io.rAddr := 0.U
    extMem.io.rEn := false.B
    extMem.io.wAddr := 0.U
    extMem.io.wEn := false.B
    extMem.io.wData := VecInit(Seq.fill(p.blockSize)(0.U))

    val memReadWire = Wire(CacheBlock())
    
    // hit bits -> one hot -> index, indexing by vec
    // so no longer need the following references to read ports
    // val dataReadWire = Wire(CacheBlock())
    // val tagReadWire = Wire(UInt(p.numTagBits.W))
    memReadWire := extMem.io.rData
    // dataReadWire := data.read(index, io.in.fire)
    // tagReadWire := tags.read(index, io.in.fire)
    // dataReg := dataReadWire
    // tagReg := tagReadWire

    // TODO: parameterize replacement policy
    val roundRobinRegs = RegInit(VecInit(Seq.fill(p.numSets)(0.U(log2Ceil(p.associativity).W))))
    def getReplIndex(setIndex: UInt): UInt = {
        roundRobinRegs(setIndex) := (roundRobinRegs(setIndex) + 1.U) % p.associativity.U
        roundRobinRegs(setIndex)
    }

    // TODO: opt set io, like is decouple necessarry?
    // TODO: index is not used in this level?
    when (state === 0.U) {
        io.in.ready := true.B
        io.hit := false.B
        io.out.valid := false.B
        extMem.io.wEn := false.B
        extMem.io.rEn := false.B

        wbReg := false.B

        when (io.in.fire) {
            setSeq.foreach(set => assert(set.io.in.ready === true.B))
            // setSeq.reduce((set1,set2) => set1.io.in.bits.ready && set2.io.in.bits.ready)
            // lookup in sets
            setSeq.foreach(set => set.io.in.valid := true.B)
            // TODO: hoist to outside?
            setSeq.foreach(set => set.io.in.bits.addr := io.in.bits.addr)
            setSeq.foreach(set => set.io.in.bits.write := false.B)
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

        // TODO: how is this excuted?
        setSeq.foreach(set => assert(set.io.out.valid === true.B))        
        val hitSeq = setSeq.map(_.io.out.bits.hit)
        io.hit := hitSeq.reduce((hit1,hit2) => hit1 && hit2)

        when (io.hit) {
            // OHToUInt("b0100".U) // results in 2.U
            // PriorityEncoder
            // shall not be of more than one high bit tho... TODO: count high bits
            // https://www.chisel-lang.org/api/latest/chisel3/util/OHToUInt$.html
            val hitSetIndex = OHToUInt(hitSeq)
            // TODO
            val hitSetData = Wire(CacheBlock())
            hitSetData := setSeqIOVec(hitSetIndex).out.bits.rLine
            // output for read or update for write
            when (io.in.bits.write) {
                // TODO:
                assert(setSeqIOVec(hitSetIndex).in.ready === true.B)

                hitSetData(offset) := io.in.bits.wData

                assert(setSeqIOVec(hitSetIndex).in.ready === true.B)
                setSeqIOVec(hitSetIndex).in.valid := true.B
                // TODO: hoist to outside?
                setSeqIOVec(hitSetIndex).in.bits.addr := io.in.bits.addr
                setSeqIOVec(hitSetIndex).in.bits.write := true.B
                setSeqIOVec(hitSetIndex).in.bits.wLine := hitSetData
            } .otherwise {
                io.out.bits := hitSetData(offset)
            }
            io.out.valid := true.B
            state := 0.U
        } .otherwise {
            extMem.io.rAddr := io.in.bits.addr / p.blockSize.U
            extMem.io.rEn := true.B

            val replSetIndexWire = Wire(UInt(log2Ceil(p.associativity).W))
            val validLineSeq = setSeq.map(!_.io.out.bits.validLine)
            // when (VecInit(validLineSeq).asUInt =/= 0.U) {
            when (validLineSeq.reduce((valid1,valid2) => valid1 && valid2) === true.B) {
                // get first invalid set index
                // TODO: why do not take vec of bool?
                // - 1.U: seems start from 0
                // https://www.chisel-lang.org/api/latest/chisel3/util/PriorityEncoder$.html
                replSetIndexWire := PriorityEncoder(validLineSeq)
            } .otherwise {
                replSetIndexWire := getReplIndex(index)
            }
            replSetIndexReg := replSetIndexWire
            when (setSeqIOVec(replSetIndexWire).out.bits.validLine) {
                dataReg := setSeqIOVec(replSetIndexWire).out.bits.rLine
                tagReg := setSeqIOVec(replSetIndexWire).out.bits.rTag
                wbReg := true.B
            }
            state := 2.U
        }
    } .elsewhen (state === 2.U) {
        io.hit := false.B

        extMem.io.rEn := false.B
        io.in.ready := false.B

        // cache write back and write to cache
        when (wbReg) {
            extMem.io.wAddr := (tagReg << p.numIndexBits.U) | index
            extMem.io.wEn := true.B
            extMem.io.wData := dataReg
            // printf("wb: %d %d %d %d to %d\n\n", dataReg(0.U),  dataReg(1.U),  dataReg(2.U),  dataReg(3.U), (tagReg << p.numIndexBits.U) | index)
        }
        when (io.in.bits.write) {
            memReadWire(offset) := io.in.bits.wData
        }
        // printf("br: %d %d %d %d from %d\n\n", memReadWire(0.U),  memReadWire(1.U),  memReadWire(2.U),  memReadWire(3.U), io.in.bits.addr / p.blockSize.U)
        // TODO: an interface for submodule read/wite?
        assert(setSeqIOVec(replSetIndexReg).in.ready === true.B)
        setSeqIOVec(replSetIndexReg).in.valid := true.B
        setSeqIOVec(replSetIndexReg).in.bits.addr
        setSeqIOVec(replSetIndexReg).in.bits.write := true.B
        setSeqIOVec(replSetIndexReg).in.bits.wLine := memReadWire

        when (io.in.bits.write === false.B) {
            io.out.bits := memReadWire(offset)
        }
        io.out.valid := true.B
        state := 0.U
    }
}