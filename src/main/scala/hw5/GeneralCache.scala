// Acknowledgement:
// This files uses Cache.scala as a template.
// Thank you for the code provided, Amogh and Prof. Beamer!

package hw5

import chisel3._
import chisel3.util._

// for storing data
class DMCacheWay(p: CacheParams) extends Module {
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
    // if the way is fully-associative (has only one block), it has no index bits
    val index  =
        if (p.associativity * p.blockSize != p.capacity)
            io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
        else
            0.U
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

    // always ready for writes. always
    when (io.in.bits.write) {
        // no output for write
        data.write(index, io.in.bits.wLine)
        tags.write(index, tag)
        valids(index) := true.B
    }

    // states are only for read
    // TODO: can i also make it always ready?
    when (state === 0.U) {
        // TODO
        io.in.ready := true.B
        io.out.valid := false.B
        io.out.bits.hit := false.B
        io.out.bits.validLine := false.B

        when (io.in.fire) {
            when (!io.in.bits.write) {
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
abstract class GeCache(p: CacheParams) extends Cache(p) {
    // seq only takes scala index. if indexing by chisel should use vec
    val wayParams = p.copy(capacity=p.capacity/p.associativity, associativity = 1)
    // val waySeq = Seq.fill(p.associativity)(Module(new DMCacheWay(wayParams)).io)
    // VecInit connects the modules - last-connect semantics
    val wayIOVec = VecInit(Seq.fill(p.associativity)(Module(new DMCacheWay(wayParams)).io))

    // ref: https://stackoverflow.com/questions/62809878/how-to-create-a-array-vec-of-chisel-modules
    // val vec_of_elements = Vec.fill(n) {Module(new MyElement(my_args)).io }
    // "In general you only need a Vec if you want to use hardware based indexing of the elements,
    // or your Elements are part of an IO. If you don't need that you can just use a Scala collection like Seq,
    // Array or similar."
    // val wayVec = Vec.fill(p.associativity) {Module(new DMCacheWay(p)).io}

    // ref: https://stackoverflow.com/questions/33621533/how-to-do-a-vector-of-modules
    // val vec_of_elements = Vec(10, Module(SaturatingCounter(4)).io)
    // val wayVec = Vec(p.associativity, Module(new DMCacheWay(p)).io)

    // 0: ready, 1: lookup, 2: fetch
    val state = RegInit(0.U)
    
    // TODO: how to store the right value. Or have to use more regs?
    val dataReg = Reg(CacheBlock())
    val tagReg = Reg(UInt(p.numTagBits.W))
    val wbReg = Reg(Bool())
    val replWayIndexReg = Reg(UInt(log2Ceil(p.associativity + 1).W))

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

    // addr signal hoisted
    wayIOVec.foreach(wayIO => wayIO.in.valid := false.B)
    wayIOVec.foreach(wayIO => wayIO.in.bits.addr := io.in.bits.addr)
    wayIOVec.foreach(wayIO => wayIO.in.bits.write := false.B)
    wayIOVec.foreach(wayIO => wayIO.in.bits.wLine := VecInit(Seq.fill(p.blockSize)(0.U)))

    // TODO: WireInit
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

    val roundRobinRegs = RegInit(VecInit(Seq.fill(p.numSets)(0.U(log2Ceil(p.associativity + 1).W))))

    // set-way ages
    val LRURelativeOrder = RegInit(VecInit.fill(p.numSets, p.associativity)(p.associativity.U(log2Ceil(p.associativity + 1).W)))

    def getReplIndex(): UInt

    // TODO: UInt and Wire(UInt)?
    def updatePolicyWhenHit(wayIndex: UInt): Unit

    def updatePolicyWhenMissFilling(wayIndex: UInt): Unit

    def updatePolicyWhenMissFilled(wayIndex: UInt): Unit

    // TODO: opt way io, like is decouple necessarry?
    // TODO: index is not used in this level?
    when (state === 0.U) {
        io.in.ready := true.B
        io.hit := false.B
        io.out.valid := false.B
        extMem.io.wEn := false.B
        extMem.io.rEn := false.B

        wbReg := false.B

        when (io.in.fire) {
            wayIOVec.foreach(wayIO => assert(wayIO.in.ready === true.B, "assert ready at state 0"))
            // wayIOVec.reduce((way1,way2) => way1.io.in.bits.ready && way2.io.in.bits.ready)
            // lookup in ways
            wayIOVec.foreach(wayIO => wayIO.in.valid := true.B)
            wayIOVec.foreach(wayIO => wayIO.in.bits.write := false.B)
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

        // for debug
        // printf("addr: %d, off: %d, index: %d, tag: %d\n", io.in.bits.addr, offset, index, tag)
        // printf("wen: %d, wdata: %d\n", io.in.bits.write, io.in.bits.wData)
        // printf("all tags: ")
        // wayIOVec.foreach(x => printf("%d, ", x.out.bits.rTag))
        // printf("\n")
        // printf("all valids: ")
        // wayIOVec.foreach(x => printf("%d, ", x.out.bits.validLine))
        // printf("\n")
        // printf("all lines: ")
        // for( x <- wayIOVec ){
        //     Seq.range(0, p.blockSize).foreach(index => printf("%d, ", x.out.bits.rLine(index.U)))
        //     printf("||")
        // }
        // printf("\n")
        // printf("hit: %d\n", io.hit)

        // TODO: how is this excuted?
        wayIOVec.foreach(wayIO => assert(wayIO.out.valid === true.B, "assert valid at state 1"))
        val hitVec = wayIOVec.map(_.out.bits.hit)
        io.hit := hitVec.reduce((hit1,hit2) => hit1 || hit2)

        when (io.hit) {
            // OHToUInt("b0100".U) // results in 2.U
            // PriorityEncoder
            // shall not be of more than one high bit tho... TODO: count high bits
            // https://www.chisel-lang.org/api/latest/chisel3/util/OHToUInt$.html
            val hitWayIndex = OHToUInt(hitVec)

            // for debug
            // printf("hitIndex: %d\n\n\n", hitWayIndex)
            // TODO
            val hitWayData = Wire(CacheBlock())
            hitWayData := wayIOVec(hitWayIndex).out.bits.rLine
            // output for read or update for write
            when (io.in.bits.write) {
                // TODO:
                hitWayData(offset) := io.in.bits.wData
                // write to the way while it is still holding the read output, always ready
                // assert(wayIOVec(hitWayIndex).in.ready === true.B, "assert ready at state 1")
                wayIOVec(hitWayIndex).in.valid := true.B
                // TODO: hoist to outside?
                // wayIOVec(hitWayIndex).in.bits.addr := io.in.bits.addr
                wayIOVec(hitWayIndex).in.bits.write := true.B
                wayIOVec(hitWayIndex).in.bits.wLine := hitWayData
            } .otherwise {
                io.out.bits := hitWayData(offset)
            }
            updatePolicyWhenHit(hitWayIndex)
            io.out.valid := true.B
            state := 0.U
        } .otherwise {
            extMem.io.rAddr := io.in.bits.addr / p.blockSize.U
            extMem.io.rEn := true.B

            val replWayIndexWire = Wire(UInt(log2Ceil(p.associativity + 1).W))
            val invalidVec = wayIOVec.map(!_.out.bits.validLine)
            val atLeastOneInvalid = invalidVec.reduce((invalid1, invalid2) => invalid1 || invalid2)
            when (atLeastOneInvalid) {
                // get first invalid way index
                // TODO: why do not take vec of bool?
                // - 1.U: seems start from 0
                // https://www.chisel-lang.org/api/latest/chisel3/util/PriorityEncoder$.html
                replWayIndexWire := PriorityEncoder(invalidVec)
                updatePolicyWhenMissFilling(replWayIndexWire)
            } .otherwise {
                replWayIndexWire := getReplIndex()
                updatePolicyWhenMissFilled(replWayIndexWire)
                dataReg := wayIOVec(replWayIndexWire).out.bits.rLine
                tagReg := wayIOVec(replWayIndexWire).out.bits.rTag
                wbReg := true.B
            }

            // for debug
            // printf("replIndex: %d\n\n\n", replWayIndexWire)
            replWayIndexReg := replWayIndexWire
            io.wayToReplace := replWayIndexWire
            state := 2.U
        }
    } .elsewhen (state === 2.U) {
        io.hit := false.B

        extMem.io.rEn := false.B
        io.in.ready := false.B

        // cache write back and write to cache
        when (wbReg) {
            // val flagsOut = flagsIn \| overflow	Bitwise OR ?
            // TODO: bit extraction and cat
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
        assert(wayIOVec(replWayIndexReg).in.ready === true.B, "assert ready at state 2")
        wayIOVec(replWayIndexReg).in.valid := true.B
        // wayIOVec(replWayIndexReg).in.bits.addr := io.in.bits.addr
        wayIOVec(replWayIndexReg).in.bits.write := true.B
        wayIOVec(replWayIndexReg).in.bits.wLine := memReadWire

        when (io.in.bits.write === false.B) {
            io.out.bits := memReadWire(offset)
        }
        io.out.valid := true.B
        state := 0.U
    }
}

class GeRBCache(p: CacheParams) extends GeCache(p) {
    // TODO: figure out why
    // val roundRobinRegs = RegInit(VecInit(Seq.fill(p.numSets)(0.U(log2Ceil(p.associativity + 1).W))))
    def getReplIndex(): UInt = {
        roundRobinRegs(index)
    }

    def updatePolicyWhenHit(wayIndex: UInt): Unit = {}

    def updatePolicyWhenMissFilling(wayIndex: UInt): Unit = {}

    def updatePolicyWhenMissFilled(wayIndex: UInt): Unit = {
        roundRobinRegs(index) := (roundRobinRegs(index) + 1.U) % p.associativity.U
    }
}

class GeLRUCache(p: CacheParams) extends GeCache(p) {
    def getReplIndex(): UInt = {
        LRURelativeOrder(index).indexWhere(age => age === (p.associativity - 1).U)
    }

    def updatePolicy(wayIndex: UInt): Unit = {
        LRURelativeOrder(index).foreach(age => age := Mux(age < LRURelativeOrder(index)(wayIndex), age + 1.U, age))
        LRURelativeOrder(index)(wayIndex) := 0.U
    }

    def updatePolicyWhenHit(wayIndex: UInt): Unit = {
        updatePolicy(wayIndex)
    }

    def updatePolicyWhenMissFilling(wayIndex: UInt): Unit = {
        updatePolicy(wayIndex)
    }

    def updatePolicyWhenMissFilled(wayIndex: UInt): Unit = {
        updatePolicy(wayIndex)
    }
}

object GeCache {
    def apply(p: CacheParams, replPolicy: String = "roundRobin"): GeCache = {
        if (replPolicy == "roundRobin") new GeRBCache(p)
        else new GeLRUCache(p)
    }
}