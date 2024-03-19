# Rowe_CCC_Commented_Disassembly
My work to understand and comment a disassembly of my Rowe R-89 jukebox CCC code.

The Rowe/AMI R-89 is a jukebox released in late 1984.  Its "brains" are the "CCC" or "Central Control Computer".  The CCC is a simple computer, with a 6502 microprocessor, two 6532 RIOT I/O ICs, 2KB of batter-backed CMOS RAM, and code (and data) programmed to an EPROM.  This project is primarily about documenting how the code (and data) on the EPROM work.

While my work is focuses on the R-89 model jukebox (because that's what I own), it has wider applicability.  Largely similar (and perhaps identical) CCC code was used for subsequent models for several years, such as the R-90, R-91, R-92 and R-93.

The main purpose of the work is to document (by commending a disassembly of original 6502 code) as much as possible of the CCC EPROM code/data, to facilitate modifications/improvements/add-ons/etc.

There will also be side-projects, undertaken to support the main purpose.  For example, documenting and understanding all of the connections and interfaces to the CCC.  Some of these interfaces are essential (the LED displays in the top of the jukebox, the record magazine controller, the keypad, etc.) and some are optional (the InterROWEgator device, wallboxes, the V/MEC video  system, a CD changer used with later versions of the CCC, etc.); but better understanding of these devices will help to understand the code in the CCC which communicates with them.

Another side-project to help support the commented disassembly is an emulator.  It uses a public-domain 6502 CPU emulator (Fake6502 CPU emulator core v1.1, by Mike Chambers) as the core of a CCC emulator written by me that I am calling "rowem".  It is currently very bare-bones, but functional (it boots up, allows service and programming modes, permits coining-up, and simulating playing selections).
