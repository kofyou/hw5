package hw5

import chisel3._
import scala.collection.mutable.ArrayBuffer


object Blake2Model {
    // Initialization Vectors - "first 64 bits of the fractional parts of the positive square roots of the first eight prime numbers")
    val IV = Seq(
              BigInt("6A09E667F3BCC908", 16),   // Frac(sqrt(2))
              BigInt("BB67AE8584CAA73B", 16),   // Frac(sqrt(3))
              BigInt("3C6EF372FE94F82B", 16),   // Frac(sqrt(5))
              BigInt("A54FF53A5F1D36F1", 16),   // Frac(sqrt(7))
              BigInt("510E527FADE682D1", 16),   // Frac(sqrt(11))
              BigInt("9B05688C2B3E6C1F", 16),   // Frac(sqrt(13))
              BigInt("1F83D9ABFB41BD6B", 16),   // Frac(sqrt(17))
              BigInt("5BE0CD19137E2179", 16))   // Frac(sqrt(19))

    // 10, 16-element permutations
    val wordPermutes = Seq(
        Seq( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
        Seq(14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3),
        Seq(11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4),
        Seq( 7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8),
        Seq( 9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13),
        Seq( 2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9),
        Seq(12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11),
        Seq(13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10),
        Seq( 6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5),
        Seq(10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0),
        Seq( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
        Seq(14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3))

    /**
      * Rotates a right by b bits and wraps LSBs back around
      * @param a: 32b number to be rotated left by b
      * @param b: Shift amount
      * @return
      */
    def ROTR(p: Blake2Params, a: BigInt, b: Int): BigInt =  {
        ((a >> b) & p.MASK) | ((a << (p.wordSize - b)) & p.MASK)
    }

    /**
      * https://en.wikipedia.org/wiki/BLAKE_(hash_function)#Mix
      *
      * @param p: parameters
      * @param workVec: The 16 word working state of our hash
      * @param idxs: 4 indices to grab words from workVec
      * @param x: a word from our Message
      * @param y: a word from our Message
      * @return
      */
    def Mix(p: Blake2Params, workVec: Seq[BigInt], idxs: Seq[Int], x: BigInt, y: BigInt): ArrayBuffer[BigInt] = {
        assert (workVec.length == 16 && idxs.length == 4)
        val Seq(a, b, c, d) = idxs.map(_.toInt)
        val v: ArrayBuffer[BigInt] = (new ArrayBuffer(16)) ++ workVec
        v(a) = (v(a) + v(b) + x) & p.MASK // mixin x
        v(d) = ROTR(p, v(d) ^ v(a), p.rot1)
        v(c) = (v(c) + v(d))     & p.MASK
        v(b) = ROTR(p, v(b) ^ v(c), p.rot2)
        v(a) = (v(a) + v(b) + y) & p.MASK // mixin y
        v(d) = ROTR(p, v(d) ^ v(a), p.rot3)
        v(c) = (v(c) + v(d))     & p.MASK
        v(b) = ROTR(p, v(b) ^ v(c), p.rot4)
        v
    }


    /**
      * https://en.wikipedia.org/wiki/BLAKE_(hash_function)#Compress
      * The Compress function takes a full 128-byte chunk of the input message and mixes it into the ongoing state array:
      * @param stateVec: 8, p.wordSize-bit words
      * @param msgChunk: 16, p.wordSize-bit words
      * @param msgBytes: 128b Counter that tracks elapsed bytes
      * @param isLastBlock: flag to invert everything during the last block
      */
    def Compress(p: Blake2Params, stateVec: ArrayBuffer[BigInt], 
                msgChunk: Seq[BigInt], msgBytes: BigInt, isLastBlock: Boolean): ArrayBuffer[BigInt] = {
        println(s"stateVec: ${stateVec.length}, msg: ${msgChunk.length}, $msgChunk, ${msgChunk(0)}")
        assert(stateVec.length == 8 && msgChunk.length == 16)
        // setup local work vector V (lower 8 from stateVec, upper 8 from IV)
        var workVec: ArrayBuffer[BigInt] = (new ArrayBuffer[BigInt]) ++ Seq.fill(16)(BigInt(0))
        (0 until 8).foreach(i => { workVec(i) = stateVec(i) ; workVec(i + 8) = Blake2Model.IV(i) }) 

        // Mix the 128-bit counter t into V12:V13
        workVec(12) ^= (msgBytes & p.MASK) // low 64b
        workVec(13) ^= ((msgBytes >> 64) & p.MASK) // high 64b

        if (isLastBlock) {
            println("this is last block")
            workVec(14) ^= p.MASK // invert all bits
        }

        print(s"before mixing: ") ; pHex(workVec)
        // Cryptographic mixing
        for (i <- 0 until p.numRounds) {
            val s: Seq[Int] = Blake2Model.wordPermutes(i % 10)
            // Mix columns
            workVec = Mix(p, workVec, Seq(0, 4, 8, 12),  msgChunk(s(0)), msgChunk(s(1)))
            workVec = Mix(p, workVec, Seq(1, 5, 9, 13),  msgChunk(s(2)), msgChunk(s(3)))
            workVec = Mix(p, workVec, Seq(2, 6, 10, 14), msgChunk(s(4)), msgChunk(s(5)))
            workVec = Mix(p, workVec, Seq(3, 7, 11, 15), msgChunk(s(6)), msgChunk(s(7)))
            print(s"after col round: "); pHex(workVec)
            // Mix diags
            workVec = Mix(p, workVec, Seq(0, 5, 10, 15), msgChunk(s(8)),  msgChunk(s(9)))
            workVec = Mix(p, workVec, Seq(1, 6, 11, 12), msgChunk(s(10)), msgChunk(s(11)))
            workVec = Mix(p, workVec, Seq(2, 7, 8, 13),  msgChunk(s(12)), msgChunk(s(13)))
            workVec = Mix(p, workVec, Seq(3, 4, 9, 14),  msgChunk(s(14)), msgChunk(s(15)))
            print(s"$i: "); pHex(workVec)
        }

        // XOR the two halves and return as the new state
        val newStateVec: ArrayBuffer[BigInt] = (new ArrayBuffer[BigInt]) ++ (0 until 8).map(i => stateVec(i) ^ workVec(i) ^ workVec(i + 8))
        print("final: ") ; pHex(newStateVec ++ Seq.fill(8)(BigInt(0)))
        newStateVec
    }

    def pHex(m: Seq[BigInt]) = {
        for (i <- 0 until 16) {
          val nl = if (Seq(2, 6, 10, 14).contains(i)) "\n" else ""
          val c = m(i)
          print(f"$c%x $nl")
        }
        println()
    }

    def hexStrFromBytes(bs: Seq[BigInt], width: Int=64): String = {
        bs.map(byte => f"$byte%02x").reduce(_ + _)
    }

    def wordsToBytes(ws: Seq[BigInt], wordSize: Int=64) = {
        val byteMask = 255
        ws.flatMap(w => {
             (0 until wordSize/8).map(i => (w >> (i*8)) & byteMask)
        })
    }

    def wordsToMessage(ws: Seq[BigInt], wordSize: Int): Message = {
        val outBytes = wordsToBytes(ws, wordSize)
        val outStr = hexStrFromBytes(outBytes)
        Message(outStr, wordSize)
    }
}

case class Blake2Params(hashLen: Int=64, msgLen: Int=64, useBlake2s: Boolean=false) {
    val numRounds = if (useBlake2s) 10 else 12
    val blockBytes = 128
    val wordSize = if (useBlake2s) 32 else 64

    // 32 or 64b mask of all 1's
    val MOD = BigInt(1) << wordSize
    val MASK = MOD - BigInt(1)

    // G Rotation constants
    val rotConsts = if (useBlake2s) Seq(16, 12, 8, 7) else Seq(32, 24, 16, 63)
    val Seq(rot1, rot2, rot3, rot4) = rotConsts.map(_.toInt)

    // Paramter block, 8 words where pBlock[0] = 0x0101kknn padded w/ 0's
    val kk = "00" // FUTURE parameterize for keyed hashing
    val nn = f"$hashLen%x" // hex
    val pBlock = BigInt(s"0101$kk$nn", 16)
}

case class Message(str: String, wordSize: Int) {
    val bytesPerWord = wordSize / 8
    val blockBytes = bytesPerWord * 16
    val strBytes = str.getBytes
    val length = strBytes.length

    // Pad message to be 16 words
    val block = (strBytes ++ Seq.fill(blockBytes - length)(0.toByte)).map(_.toInt)

    def bytesToWord(bs: Seq[Int]) = {
        bs.zipWithIndex.foldLeft(BigInt(0)) { 
            case(sum, (byte, i)) => sum + (BigInt(byte) << (i * 8)) 
        }
    }

    def grabBytesAsWords(r: Range): Seq[BigInt] = {
        val bs = r.map(block)
        bs.grouped(bytesPerWord).toSeq.map(bytesToWord)
    }

    def U: UInt = bytesToWord(strBytes.map(_.toInt)).U

    def printBytes = block.foreach(b => print(f"0x$b%x "))
    def printWords = grabBytesAsWords(0 until blockBytes).foreach(b => print(f"0x$b%x "))
}

class Blake2Model(val p: Blake2Params) {
    var stateVec: ArrayBuffer[BigInt] = new ArrayBuffer[BigInt]() ++ Seq.fill(8)(BigInt(0))

    def apply(m: Message): Message = {
        // Initialize State vector with IV
        (0 until 8).foreach(i => this.stateVec(i) = Blake2Model.IV(i))

        // mixin key size and hash length
        stateVec(0) ^= p.pBlock

        // FUTURE prepend the key for keyed hash
        var bytesCompressed = 0
        // var bytesRemaining = m.blockBytes
        var bytesRemaining = m.length

        // Compress whole 128-byte chunks of the message, except the last chunk
        while (bytesRemaining > 128) {
            // get next 128B chunk from (padded) msg
            val chunk = m.grabBytesAsWords(bytesCompressed until bytesCompressed + 128)
            bytesCompressed += 128
            bytesRemaining  -= 128

            this.stateVec = Blake2Model.Compress(p, this.stateVec, chunk, bytesCompressed, isLastBlock=false)
        }

        // Compress the final bytes from message
        val chunk = m.grabBytesAsWords(bytesCompressed until m.blockBytes)
        bytesCompressed += bytesRemaining

        this.stateVec = Blake2Model.Compress(p, this.stateVec, chunk, bytesCompressed, isLastBlock=true)

        // return the first phashLen bytes from stateVec
        val outWords = (0 until p.hashLen / m.bytesPerWord).map(this.stateVec)
        Blake2Model.wordsToMessage(outWords, p.wordSize)
    }
}