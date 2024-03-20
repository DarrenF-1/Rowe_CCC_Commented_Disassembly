# Rowe_CCC_Commented_Disassembly
My work to understand and comment a disassembly of my Rowe R-89 jukebox CCC code.

The Rowe/AMI R-89 is a jukebox released in late 1984.  Its "brains" are the "CCC" or "Central Control Computer".  The CCC is a simple 8-bit embedded computer, with a 6502 microprocessor, two 6520 PIA I/O ICs, 2KB of battery-backed CMOS RAM, and code (and data) programmed to an EPROM.  This project is primarily about documenting how the code (and data) on the EPROM functions.

While my work focuses on the R-89 model jukebox (because that's what I own), it has slightly wider applicability.  Largely similar (and perhaps identical) CCC code was used for subsequent models for several years, such as the R-90, R-91, R-92 and R-93.

The main purpose of the work is to document (by commending a disassembly of original 6502 code) as much as possible of the CCC EPROM code/data, to facilitate modifications/improvements/add-ons/upgrades/homebrew/etc.

There will also be side-projects, undertaken to support the main purpose.  For example, documenting and understanding all of the connections and interfaces to the CCC.  Some of these interfaces are all but essential (the LED displays in the top of the jukebox, the record magazine controller, the keypad, etc.) and some are quite optional (the InterROWEgator device, wallboxes, the V/MEC video  system, a CD changer used with later versions of the CCC, etc.).  In any case, better understanding of these devices will help to understand more of the code in the CCC which communicates with them.

Another side-project to help support the commented disassembly is the creation of an emulator.  It uses a public-domain 6502 CPU emulator (Fake6502 CPU emulator core v1.1, by Mike Chambers) as the core of a CCC/jukebox emulator written by me that I am calling "rowem".  It is currently very bare-bones, but functional.  It boots up, allows service and programming modes, permits coining-up, and simulating playing selections, saves RAM between sessions, has a (text-based) display and the ability to monitor any selected page (256-bytes) of RAM during operation.

Note that I am *not* a professional programmer (barely an amateur one).  The 6502 comments and emulator (written in C) do not conform to any sort of standards or best-practices, but currently exist primarily to make sense to me (in the case of 6502 comments) or to function (in the case of the rowem emulator).

Description of files:
  - <u>ROWE_R-89_V.3_70039704_2764.bin</u>: a binary dump of the EPROM in my jukebox.  It is the starting point for this whole effort.  ROWE_R-89 is th make/model of the jukebox; V.3_70039704 is the printing on the label found on the EPROM, and 2764 is the type/size of EPROM on which it was programmed.
  - <u>R89_EPROM_disassembly.txt<u>: is the primary work-product.  It was started as an automatically-generated disassembly of the above binary EPROM dump.  The raw disassembly was generated using an online disassembly tool: https://www.white-flame.com/wfdis/.  All comments are made using a simple text editor.
  - [more to come]
  - 
