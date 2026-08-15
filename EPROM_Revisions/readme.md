# EPROM Revisions

Thanks to several helpful and generous people on the International Arcade Museum forums (https://forums.arcade-museum.com/), I also have dumps of other EPROMs used on CCCs in the R-89 thru R-94 series.

This folder is an archive of them, until I have time to disassemble and compare code among versions, tracking changes, upgrades, bugfixes, etc.

If you have an EPROM dump from an R89-R94 CCC and would like to share it with me to help this project, please contact me via the Discussions for this repo.  If you have an such an EPROM and are not able to dump it, but are willing to mail to to me to be dumped (that just means copied off using an EPROM reading tool) please also contact me.  I will pay for return postage.

This collection is by no means complete.  I expect a number of other revisions are lurking in the wild.  However, I have tried to create a system to identify and organize these various revisions I have collected, since the printed EPROM labels do not seem to conform to any consistent system.  Below is a table, organized in what I believe is chronological order, based on the content and features of the EPROMs.

![EPROM Version Table](./EPROM_version_table.png)

Explanation of table:
- filename (as found) is pretty self explanatory.  Whoever sent me the file, this is the filename it had when I got it.
- CRC32 is the checksum (of that type) of the EPROM file.  This is unlikely to ever result in identical checksums, so it is the ID I plan to use for different revisions.
- hex sum full/16-bit are the sum of all data bytes in the file, expressed in hex.  The 16-bit is just the rightmost 4 hex digits of the full sum.
- EPROM type is just the JEDEC designation of a standard EPROM of the dumped file's size (e.g. 2764 for 8kB files, 27128 for 16kB files).
- compressed file size is a crude attempt to quantify how much "information" is in each file.  They were zipped in a common file compression utility, and this is the resulting compressed size.
- vectors refer to the address pointed to by the 6502 INT/NMI/RESET vectors in the final bytes of the file.
- 2nd byte is the (hex) value of the 2nd byte of the file.  Rowe appears to have used this byte as an adjustment to make the hex sum of all bytes in the EPROM end in "00" (this is how the ROM checksum code functions).
- lines of code is an approximation of the number of 6502 opcodes, based on a preliminary disassembly.
- code bytes/non-code bytes/data bytes/unused bytes are an analysis of the makeup of the EPROM, based on a preliminary disassembly.
- indirect jmp? indicates (yes or no) if the preliminary disassembly contains any 6502 indirect jmp opcode(s).
- 
