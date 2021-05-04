package hw5

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec

import treadle._
import chisel3.tester.experimental.TestOptionBuilder._


class Blake2ModelTester extends FreeSpec with ChiselScalatestTester {
    "Software 64b ROTR should work" in {
        val p = Blake2Params()
        assert(Blake2Model.ROTR(p, BigInt(7), 5) == BigInt("4035225266123964416"))
        assert(Blake2Model.ROTR(p, BigInt(0), 5) == 0)
        assert(Blake2Model.ROTR(p, BigInt(1), 0) == 1)
        assert(Blake2Model.ROTR(p, BigInt(1), 1) == BigInt(1) << 63)
    }
    "Software 32b ROTR should work" in {
        val p = Blake2Params(useBlake2s = true)
        assert(Blake2Model.ROTR(p, BigInt(7), 5) == 939524096)
        assert(Blake2Model.ROTR(p, BigInt(0), 5) == 0)
        assert(Blake2Model.ROTR(p, BigInt(1), 0) == 1)
        assert(Blake2Model.ROTR(p, BigInt(1), 1) == BigInt(1) << 31)
    }

    "Software Blake2Model should compute correct hash of 'abc'" in {
        val p = Blake2Params(hashLen = 64, msgLen = 64)
        val m = Message("abc", wordSize = p.wordSize)
        m.printWords
        val hash = new Blake2Model(p)

        val out: Message = hash(m)

        val expStr = "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
        println(f"got: ${out.str}, exp: $expStr")
        assert(out.str == expStr)
    }

    "Software Blake2Model should compute correct hash of ''" in {
        val p = Blake2Params(hashLen = 64, msgLen = 64)
        val m = Message("", wordSize = p.wordSize)
        val hash = new Blake2Model(p)

        val out: Message = hash(m)
        val exp = "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
        assert(out.str == exp)
    }

    "Software Blake2Model should compute correct hash of 'Chisel is too much fun!'" in {
        val p = Blake2Params(hashLen = 64, msgLen = 64)
        val m = Message("Chisel is too much fun!", wordSize = p.wordSize)
        val hash = new Blake2Model(p)
        val out: Message = hash(m)

        val exp = "3224a6a4869b3e3dd79e75edab31f789d484616c8bd1d0c605871495f67079851710d256170e33f875d82173e68d5e77c291c6f078074d90dc753398f5498626"
        assert(out.str == exp)
    }
}

class MessageTester extends FreeSpec with ChiselScalatestTester {
    "Message should convert string to little endian words" in {
        val m32 = Message("abcdefgh", wordSize=32) // 0x61 0x62 0x63 0x64 0x65 0x66 0x67 0x68 
        val exp0_32 = BigInt("64636261", 16)       // 0x61 0x62 0x63 0x64 
        val exp1_32 = BigInt("68676665", 16)       // 0x65 0x66 0x67 0x68 
        assert(m32.bytesPerWord == 4)
        val w0_32 = m32.grabBytesAsWords(0 until m32.bytesPerWord)
        val w1_32 = m32.grabBytesAsWords(m32.bytesPerWord until 2*m32.bytesPerWord)
        assert (w0_32.head == exp0_32)
        assert (w1_32.head == exp1_32)

        val m64 = Message("abcdefgh", wordSize=64) // 0x61 0x62 0x63 0x64 0x65 0x66 0x67 0x68 
        val exp_64 = BigInt("6867666564636261", 16)
        assert(m64.bytesPerWord == 8)
        val w_64 = m64.grabBytesAsWords(0 until m64.bytesPerWord)
        println(s"w_64: $w_64, exp_64: $exp_64")
        assert (w_64.head == exp_64)

        val block = m32.block
        val blockWords = m32.bytesToWord(block)
        println(s"block: $block, blockWords: $blockWords")
        m32.printBytes
        m32.printWords
    }
}