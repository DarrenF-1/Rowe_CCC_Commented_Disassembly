# ROM Hacking

One of the consequences of having a commented disassembly, is that it enables the creation of modifications, improvements, and small added features ("hacks").

I use the term "hack" also because I do not (yet) create a re-assemble-able 6502 source file and use an assembler to create a new binary file from the source code.  Instead, I keep my changes minimum and manually modify & add bytes to the binary EPROM file to accomplish the changes.

So far, I have used this method to implement the following hacks to my CCC EPROM:

-  Know What Played Last: keeps last record played in "selection playing" display
   when jukebox is idle. (Original code returned display to "100" when the jukebox was idle.)

-  Mute Toggle: toggle amp mute with door closed (e.g. for external BT audio).
   Top LED displays show "bLU" "too" "th " as indication.

-  More Programmable Slots: 21 slots (originally 15) for custom autoplay mode #5.

-  Autoplay Style #7: All A-sides then All B-sides. Backported from later revisions of code.

-  New Random Autoplay Modes: #8 = A-sides, #9 = B-sides, #10 = all sides.

-  Updates Factory Defaults: More relavent for home-use, not on-location.

