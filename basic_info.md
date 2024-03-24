CCC hardware:
-------------
6502 CPU
6520 PIA x2
8k x 8 EPROM (2764) [some units may have used 16k x 8, 27128 EPROMs]
2k x 8 CMOS SRAM [typically battery backed]

CCC Memory map:
---------------
0x0000-0x07FF: 2KB RAM
0x0800-0x1FFF: [unused?]
0x2000-0x2003: 6520 PIA registers
0x2004-0x3FFF: [unused?]
0x4000-0x4003: 6520 PIA registers
0x4004-0xDFFF: [unused?]
0xE000-0xFFFF: 8KB EPROM code/data [from C000 for 27128 versions]

Getting started:
----------------
The EPROM is memory-mapped to the top of the 6502 address space.  The highest 6 bytes of address space ($fffa/$fffb, $fffc/$fffd and $fffe/$ffff) are the NMI, RESET and INT vectors.  When the 6502 comes out of reset, it fetches the RESET vector ($fffc/$fffd) and puts it into the PC register (program counter; address of next instruction to be executed).  Scroll to the very bottom of the disassembly, and you'll find that all 3 vectors point to $e1f2 (note that the 6502 is little-endian).  Therefore, code execution will begin at that address.

The CCC hardware does NOT have any INT or NMI signal wired to the CPU (the lines are tied high), and no BRK opcodes are found in the code.  So we need not worry about any interrupts or interrupt handler code.  All code appears to be executed our of ROM, so there is no "self-modifying" code.  There are no indirect jumps or even jump tables used.  In these ways, this Rowe CCC code is fairly simple for 6502 code, "real-time", and able to be relatively easily followed.  The main challenge comes from trying to reverse-engineer the function of the many "variables" used (values stored in RAM).
