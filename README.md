Homework 5
=======================

# Problem 1 - Blake2b (50 pts)
In the past homeworks we have built and elaborated upon our `ChaCha` module. We're going to switch things up and develop the [Blake2b](https://en.wikipedia.org/wiki/BLAKE_(hash_function)) hash function. Blake and later Blake2b are based on ChaCha, so many of the ideas from our `ChaCha` implementation can carry over, which we can improve upon in an agile way. 

A [hash function](https://en.wikipedia.org/wiki/Cryptographic_hash_function) is a one-way function that takes an arbitrarily large *message* and scrambles it into a small *digest*. Given a digest AKA hash, it is infeasible to try to determine what the original message is. One use-case is similar to the idea of a checksum, i.e. we can hash a large binary file to a `512b` digest, and anyone who downloads the file can hash it and verify it matches this digest to check for tampering.

For our implementation of `Blake2b` we are following the [specs](https://tools.ietf.org/html/rfc7693#appendix-A), except we will fix our *message* size to be up to `1024b (128B)`, and our *digest* will always be `512b (64B)`. 

Our Blake2 implementation operates as follows:
* When a valid `msg` arrives, the `Blake2b` module will begin the hashing process and should not accept a new message until the `digest` (output) is valid. 
* Initilization: 
  * Stateful elements: 8x64b `stateVec`, 16x64b `workVec`, and 16x64b `msg`.
  * The valid `io.msg` will be saved to our `msg` to later be mixed into our `workVec`. 
  * The `io.msgBytes` specifies how many unpadded bytes our valid `msg` contains and should also be saved, i.e. if our `msg` is the ASCII string "dcba", this is encoded as `"h6463_6261".U(1024.W)` padded with 0s. Since "dcba" is four ASCII characters,  our `msgBytes` will equal `4` bytes (the encoding and decoding process is handled for you). `msgBytes` is equivalent to the `t` counter in the specs.
  * The `stateVec` will be initialized as [follows](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2b_algorithm):
    * All `8` words are copied from the initialization vectors in `Blake2b.IV`.
    * The 0th word is `XORed`  with the parameter block (calculated for you in `BlakeParams.pBlock`).
  * The `workVec` will be initialized as [follows](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#Compress):
    * The first `8` words are copied from our `stateVec`, and the last `8` from the initialization vectors. 
    * The lower 64b of `msgBytes` will be `XORed` into our `12th` slot of `workVec` and the upper 64b `XORed` into the `13th` slot.
    * For this homework, we will only process a hash up to one message block, so the `isLastBlock` argument seen in the `Blake2Model.Compress` method will always be true, meaning we have to invert the 14th word of `workVec`. 
*  Over the next `2 * numRounds` cycles, we will `Mix` words from `msg` into `workVec`. If this number is familiar, that is because it is the same number of `QR` operations our `ChaCha` module took during HW3. Here we perform a `Mix` operation on either `4` of our columns in parallel or `4` of our diagonals in parallel. 
* The `Mix` operation takes `4` words from `workVec` and `2` words from `msg` and performs a series of `64b` additions and right bit rotations (`ROTR` is opposite of `ROTL` from `ChaCha`). 
* After the rounds have completed, the final digest is equal to the `stateVec` is `XORed` with the lower `8` words of `workVec` `XORed` with the upper `8` words of `workVec`.

### Part 1 - ROTR and Mix
Given `Blake2Model.scala`, implement the `ROTR` and `Mix` functions. For this assignment do not use the Chisel `%` or bitwise `&` to truncate bits. Instead use the Chisel bit-extractions, i.e. ```someUInt(31, 0)``` or even methods on `UInt` such as `head`, `tail`, and `Cat`. Your tests should pass the `Blake2ROTRTester` and `Blake2MixTester` tests before moving to part 2.

### Part 2 - Blake2b
Implement the `Blake2b` Chisel module using `Blake2bIO`. The provided testers will be crucial for encoding and decoding the data and should be used for timing specifications. The `Blake2Model.Compress` method is a model of the core logic to be implemented in this module. 


### Part 3 - Theoretical Speedup 
This assignment also contains a manual submission. Typically when designing hardware, we have a target benchmark in mind. [Here](https://www.blake2.net) it is shown that software on a `3310MHz` processor can process Blake2b at `2^30Bps`. Our `Blake2b` module can process `128B` every `2*numRounds=24` cycles. 
- What is the relative speedup/slowdown compared to the CPU benchmark on an FPGA at `100MHz`?
- What frequency will our hardware need to run at to equal the hashing throughput of the CPU?

Include a file `src/main/scala/hw5/part3.txt` that contains your solution, and zip it with your submission.


### What is provided
- `src/main/scala/hw5/Blake2Model.Scala`
    * `Blake2Model` - object contains `ROTR`, `Mix`, `Compress`, as well as the `IV` and `wordPermutes` constants, and helper functions. 
    * `Blake2Params` - case class containing useful parameters
    * `Message` - handles much of the serialization
    * `Blake2Model` - class to perform the hash using `apply`. Some initialization happens here and then the `Compress` is used as the core operation.
- `src/main/scala/hw5/Blake2b.Scala`
    * `Blake2b` - object contains `ROTR` and `Mix` to be implemented, as well as `pHex` and `collapseSeq` helper methods.
    * `Blake2bIO` - the module's IO
    * `Blake2b` - the Chisel module to be implemented

As usual the tests are provided and should be used to drive development.

### Tips
* The `Blake2Model` is almost equivalent to the pseudocode [here](https://en.wikipedia.org/wiki/BLAKE_(hash_function)) and [here](https://tools.ietf.org/html/rfc7693#appendix-A) which contains more detail on what needs to be completed.
* The `pHex` helper methods will pretty print the state. They are sprinkled throughout the model to show the current state of the `workVec` which is helpful for debugging intermediate states in your `Blake2b` module.
* The provided tests will automatically generate a `.vcd` file containing your module's waveforms during the instance of that test and may be useful for debugging timing issues. Example file: `target/test_run_dir/Hardware_Blake2_module_should_compute_correct_hash_of_abc/Blake2.vcd`. You may view this waveform using the `WaveTrace` VCD waveform viewer in VSCode or by SSHing into the server and using the pre-installed `gtkwave` to view:
  ```
  ssh -Y <your_user>@<server>.soe.ucsc.edu 
  gtkwave path/to/hw4/target/test_run_dir/Hardware_Blake2_module_should_compute_correct_hash_of_abc/Blake2.vcd
  ```


# Problem 2 - Matrix Multiplication [Revised] (50 points)
We will revise our `MatMul` module from [HW4](https://github.com/agile-hw/hw4) to improve its scalability. Our original MatMul loaded the input matrices in a single cycle which may be impractical for matrices of non-trivial size. Instead, we will add a parameter `cyclesPerTransfer` which will set how many cycles it takes to load in the input matrices as well as output the resulting product matrix. By performing the transfers over multiple cycles, we can greatly reduce the bandwidth required. Although this change may sound simple, it will require us to generalize and revise several parts of the MatMul design. Like HW4, you will need to complete `MatMulModel` and `MatMul`.

### Input/Output Behavior
Performing a matrix multiplication will go through the following steps:
* _Idle_ - the MatMul module indicates it is idle (ready to accept work) with `io.in.ready` set high
* _Loading a matrix in_ - the user sets `io.in.valid` to high to indicate the availability of work. When `io.in.valid` and `io.in.ready` are high the same cycle, the next cycle starts the loading process for the input matrices. The matrices are read in over `cyclesPerTransfer` cycles in a row-major order (via `io.in.bits.aBlock` and `io.in.bits.bBlock`). You can assume the amount transfered in a cycle is never greater than a row (may be less).
* _Multiplying the matrices_ - once the matrices are loaded in, the MatMul unit immediately starts performing the matrix multiplication
* _Output the result_ - as soon as the multiplication finishes, the MatMul unit outputs the product matrix over `cyclesPerTransfer` (via `io.outBlock.bits`) cycles while asserting `io.outBlock.valid`
* _Change from HW4_ - MatMul should only have `io.outBlock.valid` set high while it is streaming out the product matrix over `cyclesPerTransfer` cycles. Otherwise `io.outBlock.valid` should be low.

### Organization
The revisions for this week also let use _inheritance_ to tidy some other aspects:
* We can encapsulate the parameters specific to MatMul in `MatMulParams` while using the simpler `MatMulProblem` to work with the Scala model.
* The result of this assignment will be `MatMulWithReg`, a concrete subclass of `MatMul`.

### Tips
* To keep your design organized, avoid repetition, and ease unit testing, you may want to consider making submodules or even Scala functions to encapsulate commonly performed operations. For example, can you see a way to share code for loading in the two input matrices?
* Like HW4, you will want to be sure to use the Chisel's `Counter` module to make sense of all of the ranges.
* You will probably need to make some sort of FSM to manage the progression of states (e.g. idle, loading matrices, multiplying matrices, outputting product).
