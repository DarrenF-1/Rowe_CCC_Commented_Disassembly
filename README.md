# Rowe_CCC_Commented_Disassembly

My project to understand and comment a disassembly of my Rowe R-89 jukebox's CCC code.

The Rowe/AMI R-89 is a jukebox released in late 1984.  Its "brains" are the CCC (Central Control Computer).  The CCC is a simple 8-bit embedded computer, with a 6502 microprocessor, two 6520 PIA I/O ICs, 2KB of battery-backed CMOS RAM, and code (and data) programmed to an EPROM.  This project is primarily about documenting how the code (and data) on the EPROM functions.

While my project focuses on the R-89 model jukebox (because that's what I own), it has slightly wider applicability.  Largely similar (and perhaps identical) CCC circuit boards and code was used for subsequent models for several years, such as the R-90, R-91, R-92, R-93 and R-94 (collectively "R-9X").

> Note that this project relates specifically to the "CCC" used in th R-88 _video_ and R-89 through the R-94 models with CCC assembly part number 4-07773-XX and CCC PCB part number 6-09738-XX. It does *not* relate to the identically-named "CCC" used in the earlier R-84 throuh R-88 (_non_-video) models, with assembly P/N 6-08870-XX and PCB P/N 6-08871-XX; _nor_ to the "CCC" used in the later CD-100 and up models, with assembly P/N 4-08322-XX and PCB P/N 6-10311-XX.

The main purpose of the project is to document, by commenting a disassembly of original 6502 code, as much as possible of the CCC EPROM code/data.  This in turn may facilitate hacks/modifications/improvements/add-ons/upgrades/homebrew versions/etc.

> COMMENTED DISSASSEMBLY PROJECT STATUS: ~65% complete

## Sub-Projects

There are also sub-projects, undertaken to support the main purpose.  For example, documenting and understanding all of the connections and interfaces to the CCC.  Some of these interfaces are all but essential (the LED displays in the top of the jukebox, the record magazine controller, the keypad, etc.) and some are very much optional (the InterROWEgator device, wallboxes, the V/MEC video  system, a CD changer used with later versions of the CCC, etc.).  In any case, better understanding of these devices will help to understand more of the code in the CCC which communicates with them.

### Emulator

Another sub-project to help support the commented disassembly is the creation of an emulator.  It uses a public-domain 6502 CPU emulator (Fake6502 CPU emulator core v1.1, by Mike Chambers, as found here: http://rubbermallet.org/fake6502.c) as the core of a CCC/jukebox emulator written by me that I am calling "rowem".  It is currently very bare-bones, but functional.  It boots up, allows service and programming modes, permits coining-up, and simulating playing selections, saves RAM between sessions, has a (text-based) display and the ability to monitor any selected page (256-bytes) of RAM during operation.  This is a helpful tool to help build and verify understanding of the 6502 code function (especially variable/RAM use).  It will also be helpful to do simulated testing of future EPROM code modifications.  It could potentially be much more useful, if it had additional features such as breakpoints/watchpoints, live disassembly, register monitoring, etc.  Some of these features may be added in the future.

> EMULATOR PROJECT STATUS: v0.1 FUNCTIONAL AND RELEASED.  PLENTY OF OPPROTUNITY TO ADD FEATURES AND OTHER IMPROVEMENTS.

### Note

Note that I am *not* a professional programmer.  The 6502 comments and emulator (written in C) do not conform to any sort of standards or best-practices.  They currently exist primarily to make sense to me (in the case of 6502 comments) or to function (in the case of the rowem emulator).  I am certainly not against improving the coding and commenting standards--but it's going to be an ongoing learning process.

## Description of files:
  - **ROWE_R-89_V.3_70039704_2764.bin** - a binary dump of the EPROM in my jukebox.  It is the starting point for this whole effort.  "ROWE_R-89" is the make and model of the jukebox; "V.3_70039704" is the printing on the label found on the EPROM, and "2764" is the type/size of EPROM on which it was programmed.
  
  - **R89_EPROM_disassembly.txt** - the primary work-product.  It was started as an automatically-generated disassembly of the above binary EPROM dump.  The raw disassembly was generated using an online disassembly tool: https://www.white-flame.com/wfdis/.  All comments are made using a simple text editor.
    
  - **basic_info.txt** - provides a brief description of the CCC's notable electronic hardware components; a high-level memory map of the CCC; and a short explaination of how a 6502 CPU starts up, which provides an entry point into the CCC's EPROM code.
    
  - **R-93_CCC_schematic.png** - an electronic schematic diagram of a CCC circuit board.  This is from an R-93, which is believed to be electronically identical (or nearly so) to the PCBs used for the entire R-89 thru R-94 series (as well as the R-88 video model).

  - **R-89_CCC_assembly.jpg** - a CCC unit assembly.  The CCC PCB is enclosed in the plastic housing.  The connector labels, switch labels, and button labels, and LED readout labels are are visible.  This CCC is installed into a jukebox with a bill validator (P3 connector), but no video system (P5 connector) or wallboxes (P4 connector).
    
  - **rowem** (directory) - files for the rowem CCC emulator.  See readme file in that directory for more information of those files.
    
