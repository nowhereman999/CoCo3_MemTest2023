# CoCo3_MemTest2023
A program to stress test the RAM of a CoCo 3

My MemTest2023+ does the following upon startup:

Goes into High Speed and if you have a 6309 go into 6309 native mode to make the RAM access as fast as possible.  Detect the GIME version (for fun) then performs the following RAM tests:

1 - Blast bytes - Stack blast values of 0 to 255 in every byte of RAM and compare it by pulling the values off the stack as fast as possible

2 - Walking Ones - Tests all address bits in all memory banks by using a walking ones pattern.

3 - Moving Inversions - Clear all RAM, then set bit 0 of each byte, then set bit 1 and so on, write the 
byte, then invert it, then invert it again back to original.

4 - Random Patterns - Set all RAM, then fill a block with random values, Read back random values to see if they match.

5 - Surround Read Disturb - Clear all RAM, then each cell in turn becomes the test cell. The test cell is complemented and the previous byte and the next byte are repeatedly read 32 times then the test cell is read to determine if it has been affected by the reading of its neighbour's. The operation is then repeated for a background of 1s. The intent is to find disturbances caused by adjacent cell operations.

6 - Bit Fade - writes a byte to the memory and then repeatedly reads it back (256 times), gradually reducing the strength of the pattern until it becomes almost imperceptible. This test is designed to detect any "bit fade" issues, where the voltage levels in the memory chips can gradually decay over time, causing errors.

It keeps track of how many times it has completed all the tests and will run until the CoCo is reset or powered off.

If it detects some bad RAM it will halt the test and indicate which 8K block is failing and the byte number inside that 8k block.

The program hasn't been thoroughly tested, but it works for me on my two CoCo 3's.  Both have 6309's in them curtesy of Frank and Retro Rewind another great CoCo company.  One has an 87' GIME the other has a '86 GIME and one has 512k RAM the other has 2 Megs.
