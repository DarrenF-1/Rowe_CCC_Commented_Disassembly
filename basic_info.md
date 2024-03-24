CCC Hardware:
-------------
- 6502 CPU
- 6520 PIA x2
- 8k x 8 EPROM (2764) [some units may have used 16k x 8, 27128 EPROMs]
- 2k x 8 CMOS SRAM [typically battery backed]

CCC Memory Map:
---------------
    $0000-$07FF: 2KB RAM  
    $0800-$1FFF: [unused]
    $2000-$2003: 6520 PIA registers
    $2004-$3FFF: [unused]
    $4000-$4003: 6520 PIA registers
    $4004-$DFFF: [unused][to $BFFF for 27128 versions]
    $E000-$FFFF: 8KB EPROM code/data [from $C000 for 27128 versions]

Getting Started:
----------------
The EPROM is memory-mapped to the top of the 6502 address space.  The highest 6 bytes of address space ($fffa/$fffb, $fffc/$fffd and $fffe/$ffff) are the NMI, RESET and INT vectors.  When the 6502 comes out of reset, it fetches the RESET vector ($fffc/$fffd) and puts it into the PC register (program counter; address of next instruction to be executed).  Scroll to the very bottom of the disassembly, and you'll find that all 3 vectors point to $e1f2 (note that the 6502 is little-endian).  Therefore, code execution will begin at that address.

The CCC hardware does NOT have any INT or NMI signal wired to the CPU (the lines are tied high), and no BRK opcodes are found in the code.  So we need not worry about any interrupts or interrupt handler code.  All code appears to be executed our of ROM, so there is no "self-modifying" code.  There are no indirect jumps or even jump tables used.  In these ways, this Rowe CCC code is fairly simple for 6502 code, "real-time", and able to be relatively easily followed.  The main challenge comes from trying to reverse-engineer the function of the many "variables" used (values stored in RAM).
