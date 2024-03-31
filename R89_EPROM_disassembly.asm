;*****************************************************************************
;* Rowe R-89 Jukebox CCC 6502 code
;*   original presumably written by an employee or
;*   contractor of Rowe International, circa 1983
;*
;* NO copyright notice was found on PROM label, nor in embedded in code, nor
;* displayed on, in, or by the jukebox.  Nonetheless, Rowe or its successor(s)
;* *may* still hold copyright of the original binary file.
;*
;* comments by DarrenF, 2023-2024
;* https://github.com/DarrenF-1/Rowe_CCC_Commented_Disassembly/
;*
;* disassembly performed using:
;*   WFDis v1.4 In
;*   https://www.white-flame.com/wfdis/
;*
;* ROWE_R-89_V.3_70039704_2764.bin
;*
;* NOTE: this is a commented disassembly, and is NOT currently intended as
;*       input to an assembler.
;*****************************************************************************

Le000               ea		; Rowe appears to always start their PROMs with a single $ea (nop) for unknown reasons
Le001		    40		; the 2nd byte ($e001 here) is apparently set to make the LSB of the sum of the entire PROM 0
				;
				;***************************
				; "InterROWEgator" RAM DUMP
				;***************************
				;  transfers data to a hand-held unit for operators
				;  (little information about them is available online)
				;  asynchronous serial, 2400, 5800 or 9600 baud (selectable)
				;  8 bit word, no parity, 1 stop bit (8-N-1)
				;
				;  uses temporary variable $0c (for baud rate delay timer control)
				;  uses $06/$07 as an address pointer (into RAM)
				;  uses $08 temp var first as a loop counter, then for a checksum
				;
				;  transmits (bytes of each page sent in reverse order): 
				;    $07e6-$7000, $06e6-$0600, $05e6-$0500, $04e6-$0400  
				;    $0380-$0300, $02f1-$02d8, a 1-byte checksum
				;  (note that bits are transmitted starting with the least significant bit)
				;
Se002               jsr Sfb4a	; set a base value for PIA2-A, A & var $0a (%1000 0011 or %1000 1011) based on $78 var
                    sta $4000	; put base value on PIA2-A
				;
				; set the baud rate
				;
                    lda $0337	; get RAM dump baud rate setting
                    ldx #$1f	; (31) delay loop count for 4800 baud
                    cmp #$30	; is the setting to 48? (for 4800)
                    beq Le019	; if so, skip ahead and begin
                    ldx #$44	; (68) delay loop count for 2400 baud
                    cmp #$60	; is the setting 96? (for 9600 baud)
                    bne Le019   ; if NOT, skip ahead with the 2400 baud setting
                    ldx #$0c	; if SO, use (13) delay loop count for 9600 baud
Le019               stx $0c	; store delay constant (based baud rate setting) as a temporary variable
				;	
				; set pointer and a short delay
				;
                    lda #$07	; \ $06/$07 will be an address pointer 
                    sta $07	; / start the MSB for page 7 of RAM
                    sta $08	; also use 7 to init a delay loop counter
Le021               jsr Se3dc	; \  10ms delay (this surboutine returns with X=0)
                    dec $08	;  } decrement counter
                    bne Le021	; /  loopback for 80ms total delay (leaves $08 as 0)
                    stx $06	; set LSB of $06/$07 pointer to 0 (X=0 was returned from Se3dc above)
				;
				; top of outer loop (pages)
				; adjust pointer and number of bytes per page page
				;
Le02a               ldy #$e6	; default transmit (230+1) bytes for all but pages 2 & 3
                    cmp #$03	; is this page 3 of RAM?
                    bne Le034	; if NOT skip ahead; if on page 3 (operator settings) then:
                    ldy #$80	;   transmit fewer bytes of page 3 (128+1)
                    bne Le03e	;   (unconditional branch)
Le034               cmp #$02	; is this page 2 of RAM?
                    bne Le03e	; if NOT skip to inner loop; if on page 2 (counters) then:
                    ldy #$19	;   only transmit 25(+1) bytes of page 2
                    lda #$d8	;   \ also, for page 2 use pointer base of $02d8
                    sta $06	;   / (set LSB of pointer)
				;
				; inner loop (bytes, on a given page)
				;
Le03e               tya		; \ put Y (in-page offset/count) onto the stack for now
                    pha		; / (so Y can be used for something else)
                    lda ($06),y	; get byte from RAM based on pointer and Y-offset
                    sta $b1	; put the byte to transmit in a temp var $b1 (used by subroutine below)
                    clc		; clear carry for addition \
                    adc $08	; add this value to a       } keep a checksum of all bytes transmitted
                    sta $08	; running 1-byte checksum  /
Le049               ldy $0c	; get baud-rate delay constant [pointless; Sfcad fetches it from RAM]
                    ldx #$14  	; \  20 loops
Le04d               dex		;  } small delay loopback (X=0 when this is done) 
                    bne Le04d	; /
                    jsr Sfcad	; output byte (at $b1) via serial (at baud rate per $0a)
                    pla		; \_ retrieve Y from stack (in-page offset/count)
                    tay		; / 
                    lda $07	; \ did was just finish page 1?
                    cmp #$01	; /
                    bne Le069	; more pages to do, branch ahead
				;
				; finish up and exit subroutine
				;
                    jsr Se3dc	; \ 20ms delay
                    jsr Se3dc	; / 
                    lda $0a	; retrieve PIA-2B base value
                    ora #$04	; (%0000 0100) set bit 2
                    sta $4000	; leave output line high
                    rts		; 
				;
				; bottom part of inner loop
				; 
Le069               dey		; decrement offset into current page of RAM
                    cpy #$ff	; \ are we done with this page of RAM? (Y wrapped around) 
                    bne Le03e	; / if not, loopback for next byte
				;
				; bottom part of outer loop
				;				
                    dec $07	; decrement to the next page
                    lda $07	; \ check new page
                    cmp #$01	; / have we reached page 1?
                    bne Le02a	; if not, simply loopback to do next page
				;
				; instead of doing page 1...
				;
                    lda $08	; fetch checksum value
                    eor #$ff	; \ 2's complement: convert
                    adc #$01	; / it to signed binary number
                    sta $b1	; store as byte to be transmitted
                    jmp Le049	; loopback to transmit the checksum and finish
				;
				;************************************
				; recieve message from video system?
				;************************************
				;  called from 1 place in code; near $e910
				;
Se081               ldy #$0f	; init counter to 15 
                    lda #$80	; A=$80 (128)
                    sta $08	; put $80 in variable $08
Le087               lda $4000	; get PIA2-A
                    dey		;   decrement counter
                    beq Le0a1	;   if counter is 0 branch out of loop
                    ora $4000	;   get PIA2-A (again?)
                    and #$20	;   isolate bit 5 %0010 0000: data from video system
                    bne Le087	; loopback if bit 5 is 0
                    lda #$7f	; \ %0111 1111
                    sta $4000	; / write to PIA2-A 
                    ldx #$00	; start at beginning of input buffer
				;
				; top of byte loop
				;
Le09b               ldy #$3c	; init counter Y (60)
Le09d               lda $4000	; get PIA2-A 
                    dey		;   decrement counter
Le0a1               beq Le10f	;   if counter is 0 branch way down
                    and $4000	;   get PIA2-A (again? why?)
                    and #$20	;   %0010 0000 isolate bit 5: data from video system?
                    beq Le09d	; loopback if bit is 0
				;
				; start bit recieved?
				;
                    lda #$77	; \ %0111 0111
                    sta $4000	; / write to PIA2-A, low bits out to video system and CD player?
                    ldy #$32	; init loop counter (50) 
Le0b1               dey		; countdown (2 cycles)         \_total loop=50*5=250 cycles ~1/3600s
                    bne Le0b1	;   tight delay loop (3 cycles)/
				;
				; recieve a byte from video system
				;
                    ldy #$08	; init counter (8 bits to recieve)
Le0b6               lda $4000	; get PIA2-A							(4 cycles) (185 cycles/bit)
                    and #$20	; %0010 0000 isolate bit 5 - data from video system		(2 cycles)
                    eor #$20	; flip bit 5							(2 cycles)
                    adc #$f0	; add #$f0; this sets/clears carry flag based on bit 5 of A	(2 cycles)
                    ror $4d,x	; rotate byte (offset by X) [carry flag goes to bit 7]		(6 cycles)
                    sec		; set carry flag 						(2 cycles)
                    lda #$20	; (32)								(2 cycles)
Le0c4               adc #$fe	; \ $fe + $20 + carry flag = $1f (31)... I think	2*32 =	(64 cycles)
                    bne Le0c4	; / loopback until A=0					3*32 =	(96 cycles)
                    dey		;   decrement Y counter						(2 cycles)	
                    bne Le0b6	; loopback for next bit						(3 cycles)
				;
                    txa		; X->A (sets flags)
                    bne Le0dc	; skip ahead if this wasn't the first byte recieved
				;
				; only for first byte received
				;
                    lda #$07	; \ %0000 0111
                    sta $08	; / set $08 var to 7
                    and $4d	; isolate lower 3 bits of 1st byte recieved
                    sta $07	; value (0-7) in $07 var
                    bne Le0dc	; is value is non-zero skip ahead
                    lda #$07	;   \ if value is 0 
                    sta $07	;   / set $07 var to 7
				;
				; bottom of outer (byte) loop
				;
Le0dc               inx		; increment counter to next byte
                    cpx $07	; are we on byte 7?
                    bne Le09b	; if not, loopback for another byte
				;
				; calculate a checksum?
				;
                    dex		; decrement X counter (from 7 back to 6)
                    lda #$00	; zero out A
Le0e4               clc		; clear carry flag for addition
                    adc $4d,x	;   add Xth byte recieved from video system 
                    dex		;   decrement byte counter
                    bpl Le0e4	; loopback until all 6 bytes added up
                    and #$ff	; set zero flag based on checksum? [curious instruction]
                    bne Le10f	; if nonzero branch to near end of subroutine (checksum wrong? bail?)
				;
				; wait for video system ack?
				;
                    ldy #$3c	; init counter (60)
Le0f0               lda $4000	; read PIA2-A
                    dey		;   decrement loop counter
                    beq Le10f	;   if counter expired, branch down to exit subroutine
                    and $4000	;   \ get PIA2-A
                    and #$20	;   / isolate bit 5 (%0010 0000) data from video system
                    beq Le0f0	; loopback if bit is 0
				;
                    ldy #$23	; \  35 loops
                    nop		;  \ 
Le100               dey		;  / decrement counter
                    bne Le100	; /  delay loop
				;
                    lda #$7f	; \ %0111 1111 
                    sta $4000	; / write to PIA2-A
				;
                    ldy #$23	; \  Y=35
Le10a               dey		;  } decrement counter
                    bne Le10a	; /  delay loop
				;
                    sty $08	; Y now has 0, store that at $08
Le10f               lda #$77	; \ %0111 0111 - data sent to video system
                    sta $4000	; / write to PIA2-A
                    lda $08	; get variable $08 to A (return status code?)
                    rts		; done
				; 
				;*********************************
				; send a message to video system?
				;*********************************
				;  (called from 1 place in code - near $ea12)
				;  uses $08 as temp var for error/exit code(?)
				;  expects bytes to xfer in $79-$7f
				;    1st byte ($79) is size of message?
				; 
Se117               lda $4000	; read PIA2-A
                    and #$20	; %0010 0000 isolate bit 5: data from video system
                    bne Le129	; branch down if high
				;
				; video system not ready/responding, bail?
				; 
Le11e               lda #$77	; %0111 0111: bit 3, data to video system
                    sta $4000	; write to PIA2-A
                    lda #$40	; \
                    sta $08	; / store #$40 in $08 var (exit code for failure?)
                    bne Le19f	; (always) branch to near end of subroutine
				;
				; handshake with video system?
				;
Le129               lda #$7f	; %0111 1111: video system out bit
                    sta $4000	; write to PIA2-A
                    lda #$20	; %0010 0000 to mask bit 5
                    nop		; micro-pause
                    nop		; micro-pause
                    and $4000	; read bit 5 of PIA2-A - data from video system
                    beq Le11e	; loopback if low (to bail?)
				;
				; alternate entry point - called from 1 place
				;
Se137               lda #$80	; \
                    sta $08	; / store exit code of #$80(?)
                    ldy #$0f	; init counter to 15
Le13d               lda $4000	;   read PIA2-A
                    dey		;   decrement counter
                    beq Le1a3	;   if counter reached 0 exit loop
                    ora $4000	;   read PIA2-A again
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    bne Le13d   ;   if high loopback; else done looping
				;
                    lda $79	; fetch 1st byte of message to video system (# of bytes?)
                    and #$07	; %0000 0111 isolate low 3 bits
                    sta $07	; store as $07 var
                    ldx #$00	; init byte counter/index to 0 (start at location $79)
				;
				; top of outer (byte) loop to transmit
				; $79-$7f to video system, using 8-N-1 serial
				;
Le152               ldy #$0a	; init bit loop counter (do 10 bits total per byte)
                    sec		; set carry flag (will serve as the stop bit after a full loop)
                    bcs Le159	; always branch over next opcode (to send start bit of "space")
				;
				; send a byte over the line to the video system (~4800 baud?)
				; including start & stop bits
				;
Le157               bcs Le15d	; branch over next depending on carry flag	(2.5 cycles)
Le159               lda #$77	;   %0111 0111 clear bit 3 (send 0)		(2 cycles)
                    bne Le160	;   (always branch) 				(3 cycles)
Le15d               lda #$7f	; %0111 1111 set bit 3 (send 1) 		(2 cycles)
                    nop		; micro-pause					(2 cycles)
Le160               sta $4000	; write to PIA2-A - send the bit to video system(4 cycles)
				;
				; serial xfer timing delay loop (163 cycles total)
				;
                    lda #$13	; do 19 loops		(2 cycles)
                    sta $0a	; set temporary variable(3 cycles)
                    nop		; 		     \
                    nop		; do nothing	      } (6 cycles)
                    nop		; 		     /
Le16a               dec $0a	; decrement counter 	(5*19 cycles)
                    bne Le16a	; loopback		(3*19 cycles)
				;
				; bottom part of inner (bit) loop
				;
                    ror $79,x	; rotate data byte right (bit 0 to carry flag)	(6 cycles)
                    dey		; decrement bit counter				(2 cycles)
                    bne Le157	; loopback until all 10 bits sent 		(3 cycles)
				;
				; bottom part of outer (byte) loop
				;
                    rol $79,x	; rotate Xth data byte back left (reason not understood)
                    inx		; move up to next byte (in $79-$7f)
                    cpx $07	; are we up to (value is temp var $07; # of bytes?)
                    bne Le152	; if not, loopback for another byte
				;
				; wait for handshake from video system?
				;
                    lda #$77	; %0111 0111 clear bit 3
                    sta $4000	; write to PIA2-A: send a 0 to video system
                    lda $4000	; read PIA2-A status back
                    and #$20	; %0010 0000 isolate bit 5: data from video system
                    beq Le19b	; if bit 5 low, branch ahead to finish [with exit code $88]
				;
				; check for acknowledgement from video system?
				;
                    ldy #$1e	; init loop counter to 30
                    lda #$00	; \ set $08 var to 0 [default exit code?]
                    sta $08	; /
Le18c               lda $4000	; read PIA2-A
                    dey		; decrement loop counter
                    beq Le19b	; if counter 0, exit loop [setting exit code $88]
                    ora $4000	;   read PIA2-A again
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    bne Le18c	;   if bit 5 high, loopback
                    beq Le19f	;   if bit low, exit loop [leaving exit code $00]
Le19b               lda #$88	; \ reset $08 var to $88 [exit code?]
                    sta $08	; /
Le19f               lda #$00	; \ reset $78 var to 0
                    sta $78	; /
Le1a3               rts		; done
                  		;
				;*********************	
				; VERIFY ROM CHECKSUM
				;*********************
				;   all bytes from $e000 to $ffff should
				;   sum to (LSB of) $00
				;   ($e001 appears adjusted to make it do so)
				; 
Se1a4               lda #$e0	; \
                    sta $01	;  | set pointer $00/$01 to $e000 (ROM base)
                    lda #$00	;  |
                    sta $00	; /
Le1ac               clc		; clear carry flag for addition
                    adc ($00),y	;     add to running checksum
                    iny		;     increment Y counter
                    bne Le1ac	;   loopback until Y wraps to 0
                    inc $01	;   increment MSB to point to next page 
                    bne Le1ac	; loopback until MSB wraps from $ff to $00
                    cmp #$00	; compare to the correct checksum (of $00)
                    rts		; done! returns Z flag status (and A)
                    		;
				;***************************************
				; copy a serial buffer to page 1 table?
				;***************************************
				;  (called from 2 places: $e91f and $ea37)
				;  expects X (buffer location) and A (number of bytes)
				;    X-1 is a zero-page address base for up to 8 bytes of data
				;    called with X of $4d and $79: locations of serial buffers
				;    A's low 3 bits form an offset into X-1
				;  uses $0133 as index into $0134 - $016c table of 56 bytes
				;
Se1b9               dex		; decrement X (to byte before beginning of buffer)
                    stx $06	; store X to $06 LSB of pointer (either $4c or $78)
                    ldx #$00	; \ init MSB of pointer to $00
                    stx $07	; / $06/$07 points to $004c or $0078
                    and #$07	; %0000 0111: isolate low 3 bits of A (now 0-7)
                    cmp #$02	; compare A to 2
                    bcc Le1f1	; if A<2 (i.e. 1 or 0) branch to rts (2-7 continues)
                    tay		; copy it to Y
                    dey		; decrement Y counter (Y now 1-6; bytes to copy)
				;
				; top of a loop
				;
Le1c8               inc $0133	; increment table index
                    ldx $0133	; fetch table index to X
                    cpx #$6d	; compare table index to end of table
                    bcs Le1d6	; if table end reached, branch down
                    cpx #$34	; compare table index to start of table
                    bcs Le1db	; index is in table, branch over next block
				;
				; end of table reached
				;
Le1d6               ldx #$34	; \ wrap table index back to      
                    stx $0133	; / the beginning
				;
				; 
				;
Le1db               lda ($06),y	; fetch Yth byte from selected serial buffer
                    cpy #$01	; on byte 1?
                    bne Le1eb	; if not 1, branch down
				;
				; when byte 1 is reached
				;
                    ldy $06	; \ get LSB of pointer
                    cpy #$58	; / compare to $58
                    bcs Le1e9	; skip next opcode if copying output buffer
                    ora #$80	;   set bit 7 of byte (for input buffer byte 1)
Le1e9               ldy #$01	; return loop counter 1 (since we used the Y register)
				;
				; (common path)
				;
Le1eb               sta $0100,x	; put byte from serial buffer into table
                    dey		; next byte
                    bne Le1c8	; loopback until 0
Le1f1               rts		; done
				;
				;****************
				; POWER ON RESET
				;****************
				;  initializes and boots the CCC at powerup
				;  also performs warm-reboots
				;
				; standard 6502 startup tasks
				;
Le1f2               ldx #$ff	; \ initialize stack pointer to $ff
                    txs		; / 
                    cld		; clear decimal mode flag
				;
				; initialize the PIAs
				;
                    lda #$00	; \
                    sta $2001	;  \
                    sta $2003	;   } set both control registers of both PIAs to 0
                    sta $4001	;  /
                    sta $4003	; /
                    stx $2002	; \ set data direction register B of both PIAs to $ff (all outputs)
                    stx $4002	; /
                    lda #$01	; \ %0000 0001 (0=in 1=out for PIA data direction settings)
                    sta $2000	; / set data direction register A of PIA 1 (all IN except for PA0) 
                    lda #$8c	; \ %1000 1100, set data direction register A of PIA 2
                    sta $4000	; / all IN except OUT on PA2, PA3 & PA7
                    lda #$04	; \  set control register A of PIA 1 to %0000 0100
                    sta $2001	;  } this turns off access to the DDR & allows regular operation
                    sta $2003	; /  ...do the same for B register
                    sta $4001	; \ ...do the same for the other PIA
                    sta $4003	; /
				;
				; start checking for some connected devices
				;
                    stx $4002	; %1111 1111 to PIA 2 Port B (all outputs high=off)
                    sta $4000	; put %0000 0100 onto PIA 2 Port A (high on PA2, wallbox serial)
                    lda #$00	; put %0000 0000 onto PIA 1 Port B
                    sta $2002	; (these are inverted to high, controls are active-low; so all OFF)
                    ldy #$03	; init Y counter (3 tries?)
Le22f               ldx #$ef	; \ %1110 1111
                    stx $4002	; / byte to PIAB: all high except PB4 (top LED clock?)
                    nop		; 
                    lda $4000	; read PIA 2 Port A
                    and #$40	; %0100 0000 isolate PA6 input (in from CD player?)
                    bne Le255	; branch down if bit high (CD interface present?), otherwise...
				;
				; read ADVANCE button
				;
Le23c               lda $4000	; read PIA 2 Port A
                    and #$02	; isolate PA1 bit: input D7, S4 switch = ADVANCE button on CCC PCB
                    bne Le252	; branch if advance button is NOT pressed
				;
				; ADVANCE button IS pressed at boot time; read the MEMOREC RESET button
				;
                    lda #$be	; \ %1011 1110
                    sta $4002	; / PIA2-B all high but PB0 and PB6 low, S3=input D4, S1 = RESET on CCC PCB 
                    lda $4000	; get PIA2-A input 
                    and #$02	; isolate PA1 bit RESET button
                    bne Le252	; branch over jump if reset button is NOT also pressed
                    jmp Le65e	;     unconditional jump (both buttons are pressed, "Err0" is triggered) [A=0]
Le252               jmp Le5c6	; jump to continue boot-up process (verify RAM/ROM checksums etc.)
				;
				; factory test rig seen at boot time?
				; 
Le255               ldx #$ff	; \ %1111 1111
                    stx $4002	; / set PIA 2 Port B all high
                    nop		; no-op (pause)
                    lda $4000	; read PIA 2 Port A 
                    and #$40	; %0100 0000 isolate bit 6: in from CD player
                    bne Le23c	; if high, branch back up to main code
                    dey		;   decrement counter
                    bne Le22f	;   if counter not zero, branch back into regular routine
                    jmp Le29f	;   (jump down past data tables)
				; 
				;********************
				; CHARACTER SET DATA 
				;********************                   
				;
Le268                                       77 41 3b 6b 4d 6e 7e 43 ; character sets for LEDs
                    7f 6f 08 2a 00 18 00 1e 77 14 6d 5d 1e 5b 7b 15 ; 2 sets of 16 characters
                    7f 5f 08 49 6b 28 00 2b                         ; used by code near $e4de
				;
				; 0-9 are digits
				; $0a = "-"
				; $0b = "=" [3 horizontal line programming prompt]
				; $0c = "E" [or blank]
				; $0d = "r" 
				; $0e = " " [blank space]
				; $0f = "F" [no known use]
				; (mapping of bits to LED segments is complex and different on 
				;  the CCC PCB and the remote display PCB in the top of the jukebox)
				;
				;*************************
				; LED DATA LOCATION TABLE
				;*************************
				;  (ref code at $e4b5)
				;
Le288                                       61 5f 5e 60 84 40 96 95
                    90 94 84 40 92 91 8c 8d 84 40 8e 28 29 2a 38
				;
				; 61 5f 5e 60 RAM addresses of data for the 4 LEDs on the CCC board
				; 84 40	      stop/start bits for the LED controllers
				; 96 95 90 94 RAM addresses of data for an LED controller on the top of the juke
				; 84 40       stop/start bits for the LED controllers
				; 92 91 8c 8d RAM addresses of data for an LED controller on the top of the juke
				; 84 40       stop/start bits for the LED controllers
				; 8e 28 29 2a RAM addresses of data for an LED controller on the top of the juke
				; 38          RAM address of data to control the LED-lit indicators	
				;             ("MAKE A SELECTION" and "THANK YOU")
				;
				;**************************
				; FACTORY SYSTEM TEST MODE
				;**************************
				;  reached by jmp from before data tables above
				;  also reach by jmp from 2 other places
				;
Le29f               lda #$02	; \ loop flashes "888" on LEDs, on and off twice (on/off/on/off)
                    sta $07	; / init loop counter
Le2a3               lda #$08	;   \
                    sta $5e	;    \  
                    sta $5f	;     } put "8" on all 4 digits
                    sta $60	;    / of the CCC LED display
                    sta $61	;   /  
                    jsr Se453	;   update LED display controllers
                    lda $4002	;   \  get PIA2-B
                    and #$ef	;    } %1110 1111 clear bit 4: top LED display clock
                    sta $4002	;   /  write back to PIA2-B
                    jsr Se3cb	;   0.5s delay
                    jsr Sff35	;   clear key buffer and all LED display data
                    jsr Se453	;   update LED display controllers
                    lda $4002	;   \  get PIA2-B
                    ora #$10	;    } %0001 0000 set bit 4: top LED display clock
                    sta $4002	;   /  write back to PIA2-B
                    jsr Se3cb	;   0.5s delay
                    dec $07	;   decrement loop counter
                    bne Le2a3	; loopback until 0 (2 loops)
				;
				; activate each PIA2B mech output in sequence 
				;
                    ldx #$80	; \ %1000 0000
                    stx $08	; / store in $08 temp var 
Le2d4               jsr Se3e5	;   [wait for some external signal]
                    lda $08	;   \ get temp var 
                    sta $2002	;   / write to PIA1-B
                    jsr Se3ed	;   [wait for some external signal]
                    lsr $08	;   moves the high bit to the right
                    bcc Le2d4	; loopback until high bit emerges to carry flag
				;
				; activate ALL mech outputs
				;
                    lda #$00	; \ %0000 0000
                    sta $2002	; / put value on PIA1-B (activate all mech outputs at once!)
                    jsr Se3e5	; [wait for some external signal]
				;
                    lda #$88	; \ %1000 1000
                    sta $4000	; / put value on PIA2-A (data to vid system? out to CD player?)
                    jsr Se3ed	; [wait for some external signal]
                    jsr Se3e5	; [wait for some external signal]
				;
                    lda #$00	; \ %0000 0000
                    sta $4000	; / put value on PIA2-A
                    jsr Se3ed	; [wait for some external signal]
                    jsr Se3e5	; [wait for some external signal]
				;
                    lda #$0c	; \ %0000 1100
                    sta $4000	; / put value on PIA2-A (wallbox and video serial?)
                    jsr Se3ed	; [wait for some external signal]
                    lda #$08	; \ %0000 1000
                    sta $4000	; / put value on PIA2-A
				;
				; activate 4 outputs on PIA2B in sequence
				;
                    lda #$08	; \ %0000 1000 [instruction seems redundant]
                    sta $08	; / init loop variable
Le312               jsr Se3e5	;   [wait for external signal]
                    lda $08	;   get $08 back
                    eor #$ff	;   invert every bit of A [now %1111 0111]
                    sta $4002	;   write to PIA2-B
                    jsr Se3ed	;   [wait for some external signal]
                    lsr $08	;   moves high bit to the right
                    bcc Le312	; loopback until high bit emerges to carry flag
                    lda #$ff	; \ %1111 1111
                    sta $4002	; / write to PIA2-B (return to normal state)
				;
                    lda $4000	; \ read PIA2-A
                    and #$20	; / %0010 0000 isolate bit 5: data from video system?
                    bne Le332	; if not 0, skip next opcode
                    jmp Le29f	;   restart factory test?
Le332               lda #$04	; %0000 0100 bit to check first?
                    sta $08	; store bit at $08 var
Le336               jsr Se3e5	; [wait for some external signal]
Le339               lda $2000	; read PIA1-A
                    eor #$ff	;   invert all bits
                    and #$fc	;   (%1111 1100) isolate bits 2-7 (all signals from record mech)
                    cmp $08	;   compare to variable at $08
                    bne Le339	; tight loopback if not zero
                    jsr Se3ed	; [wait for some external signal]
                    asl $08	; shift variable at $08 left 1 bit (to check next bit of input from mech)
                    bcc Le336	; loopback if we didn't shift the high bit out of A
                    jsr Se3e5	; [wait for some external signal]
Le34e               lda $4000	; get PIA2-A
                    and #$20	;   (%0010 0000) isolate bit 5, video system data?
                    bne Le34e	; loopback
                    jsr Se3ed	; [
                    jsr Se3e5	; [wait for some external signal]
Le35b               lda $4000	; \ get PIA2-A
                    and #$10	; / %0001 0000 isolate bit 4: wallbox serial in
                    bne Le35b	; [wait for some external signal]
                    jsr Se3ed	; [wait for some external signal]
                    lda #$f1	; \ %1111 0001
                    sta $08	; / store as $08 variable
Le369               jsr Se3e5	; [wait for some external signal]
Le36c               lda $08	; get $08 variable back (%1111 0001)
                    sta $4002	;   store at PIA2-B [???]
                    nop		;   pause
                    lda $4000	;   get PIA2-A
                    and #$01	;   (%0000 0001) isolate b0, coin inputs (multiplexed)
                    bne Le36c	; loopback
                    jsr Se3ed	; [wait for some external signal]
                    lda #$02	; A=2
                    adc $08	; increase $08 variable by 2
                    sta $08	; store it back
                    bcc Le369	; loopback
                    ldy #$00	; init Y=0
Le386               jsr Se3e5	; [wait for some external signal]
                    lda $e3c3,y	;   data table lookup of PIA settings to select inputs (keypad?)
                    sta $4002	;   write PIA2-B
Le38f               nop		;   micro-pause
                    lda $4000	;     get PIA2-A
                    and #$02	;     (%0000 0010) isolate multiplexed keypad/buttons to read
                    bne Le38f	;   loopback until low?
                    jsr Se3ed	;   [
                    iny		;   increment Y counter
                    cpy #$04	;   up to 4 yet?
                    bne Le386	; loopback if not
                    jsr Se3e5	; [wait for some external signal]
                    ldx #$00	; init pointer
                    stx $00	; $00/$01 to 
                    stx $01	;   $0000
                    dex		; X=0
                    stx $02	; init var $02 to 0
                    jmp Le5d6	; jump to RAM/ROM test
				;
				;************************
				; part of selftest code?
				;************************
                    		;   (reached by JMP from 1 place - near $e63d)
				;
Le3ae               bcs Le3ae	; infinite loop if carry flag set (hang on certain error codes?)
                    jsr Se3ed	; [comms subroutine?] [reached only if carry flag is clear]
Le3b3               lda $4000	; get PIA2-A
                    and #$40	;   isolate (%0100 0000) bit 6: input from CD player?
                    bne Le3bd	;   if non-zero skip next opcode
                    jmp Le29f	;     jump to top of ___ routine (exits this loop)
Le3bd               jsr Se433	;   put 8888 on CCC LEDs
                    jmp Le3b3	; loopback
				;
				;*******************
				; PIA2-B DATA TABLE
				;*******************
				;
Le3c3               3e 3f 7e 7f	; data table of values for PIA2-B (ref. code at $e386)
				;
				; 3e = %0011 1110  b6: PA1 S1, b0 PA1 S0  [000]=D0=p4 N/C?
				; 3f = %0011 1111  b7: PA1 S3		  [001]=D1=p3 N/C?
				; 7e = %0111 1110			  [010]=D2=p2 keypad return 0
				; 7f = %0111 1111			  [011]=D3=p1 keypad return 1
				;
				;*********************
				; VARIOUS DELAY LOOPS
				;*********************
				;    2s delay - e3c7 entry
				;  0.5s delay - e3cb entry
				;   5ms delay - e3d8 entry
				;  10ms delay - e3dc entry
				; 100ms delay - e3d4 entry
				;
Se3c7               ldy #$14	; (20 * 100ms = 2s delay)
                    bne Le3cd	; unconditional branch (zero flag clear from LDY)
				;
Se3cb               ldy #$05	; Y=5 (5 * 100ms = 500ms = 0.5s delay)
Le3cd               jsr Se3d4	; do a 100ms delay
                    dey		;   Y=Y-1
                    bne Le3cd	; loopback until Y=0
                    rts		; done
				;
Se3d4               ldx #$64	; alternate entry - X=$64 (100ms delay)
                    bne Le3de	; unconditional branch to ms delay loop
				;
Se3d8               ldx #$05	; alternate entry - X=5 (5ms delay)
                    bne Le3de	; unconditional branch ms delay loop
				;
Se3dc               ldx #$0a	; alternate entry - X=$0a (10ms delay)
				;
Le3de               jsr Sef4d	;   inner 1ms delay loop subroutine
                    dex		;   decrement counter
                    bne Le3de	; loopback until X=0
                    rts		; done
                    		;
				;******************************************
				; wait for signal low in factory test mode
				;******************************************
				;  test rig attached to CD interface lines
				;
Se3e5               lda $4000	; \ PIA2-A inputs
                    and #$40	; / %0100 0000 isolate bit 6: input from CD player
                    bne Se3e5	; infinite loopback until bit goes low
                    rts		; 
				;
				;*******************************************
				; wait for signal high in factory test mode
				;*******************************************
				;
Se3ed               lda $4002	; \  get PIA2-B settings
                    and #$ef	;  } %1110 1111 all select bits high
                    sta $4002	; / store PIA2-B outputs back
Le3f5               lda $4000	; \ get PIA2-A inputs
                    and #$40	; / %0100 0000 isolate bit 6: input from CD player
                    beq Le3f5	; infinite loopback until bit goes high
                    lda $4002	; \  get PIA2-B outputs
                    ora #$10	;  } %0001 0000 all select bits low
                    sta $4002	; /  store PIA2-B outputs back
                    rts		; done
                   		;
				;
Le405                              a6 0b f0 0c a0 00 91 00 c8 d0 fb	; un-reachable code, 29 bytes
Le410               e6 01 ca d0 f6 a6 0a f0 08 a0 00 91 00 c8 ca d0	; (thus not disassembled)
Le420               fa 60
		;   		uncalled code, disassembled for reference
		;   ldx $0b		
		;   beq [$0c]
		;   ldy #$00
		;   sta ($00),y
		;   iny
		;   bne [$fb]
		;   inc $01
		;   dex
		;   bne [$f6]
		;   ldx $0a
		;   beq [$08]
		;   ldy #$00
		;   sta ($00),y
		;   iny
		;   bne [$fa]
		;   rts
				;
				;*******************************
				; DATA TABLE: LED RAM LOCATIONS
				;*******************************
				;  only used by code below at $e435
				;
Le422                     5e 5f 60 61 38 2a 29 28 8e 8d 8c 92 91 90
Le430               96 95 94
				;
				; 5e-61: CCC LEDs
				; 38   : indicator LEDs ("THANK YOU" & "MAKE SELECTION")
				; 2a-28: top of juke LEDs
				; 8e-8c: top of juke LEDs
				; 92-90: top of juke LEDs
				; 96-94: top of juke LEDs
				;
				;************************
				; put "8888" on CCC LEDs?
				;************************
				;  only called from factory system test code
				;
Se433               ldy #$03	; init loop counter to Y=3 (alt. entry)
				;
				;***********************
				; put "888" on all LEDs
				;***********************
				;  expects a value in Y 
				;  only called from factory system test code
				; 
Se435               jsr Sff35	; clear all LED display data (Y not used)
                    ldx $e422,y	;   get table value (from above)
                    lda #$0b	;   A=(%0000 1011) (for case of X=$38) [turn on both indicator lamps?]
                    cpx #$38	;   compare X to $38 [only 1 in table, at Y=4]
                    beq Le443	;   skip next opcode if X=$38 (leaving A=$0b)
                    lda #$08	;     A=8 (if X is NOT $38)
Le443               sta $00,x	;   store A ($08 or $0b) at address from table (LED digit value)
                    tya		;   \
                    pha		;   /put Y (loop counter) on stack
                    jsr Se453	;   update LED displays
                    jsr Se3d4	;   100ms delay
                    pla		;   \
                    tay		;   /get Y (loop counter) from stack
                    dey		;   decrement loop counter
                    bpl Se435	; loopback until Y<0
                    rts		; done here
				;
				;*************
				; UPDATE LEDS
				;*************
				;  update output to all LED display controllers
				;  per the contents of RAM
				;
Se453               lda #$00	; \
                    sta $98	; / init var $98 to 0
Le457               jsr Se45f	; process a digit of data to LED displays
                    lda $98	; get variable $98
                    bne Le457	; loopback until it's zero again
                    rts		; done
				;
				;******************************
				; PROCESS DIGIT - LED DISPLAYS
				;******************************
				; (driven by parent loop above: $e453)
				; (also called from 1 other place; not fully understood)
				;
Se45f               tya		; \
                    pha		; / put Y on stack
                    lda $4000	; \  read PIA2-A
                    and #$20	;  } %0010 0000 isolate bit 5: data from video system [why??]
                    sta $0d	; /  store to $0d variable(?): will be $20 or $00 [ref. $e517?]
                    lda #$0e	; \ %0111 0000 (LED controller stop bits?)
                    sta $84	; / store to $84 
                    lda #$31	; %0011 0001 clock data 0 to LEDs?
                    sta $4002	; write to PIA2-B
                    ldx $98	; get counter variable $98 to X
                    bne Le48d	; branch down if it's non-zero (is 0 on first time thru)
				;
				; first time through set up
				; 
                    stx $99	;   init $99 var to 0 (X must be 0 to get here)
                    lda #$19	;   (25) init the digit counter/offset?
                    sta $98	;   store initialized counter back to $98
                    ldx #$25	;   set loop counter to 37 (bits to clock to LED controller)
Le47d               lda #$01	;   %0000 0001 H clock data 0 to LEDs (CCC and top group 0?)
                    sta $4002	;     write to PIA2-B
                    lda #$31	;     %0011 0001 L clock data 0 to LEDs (CCC and top group 0?)
                    dex		;     decrement X counter
                    sta $4002	;     write to PIA2-B
                    bne Le47d	;   loopback until X=0
                    jmp Le52c	;   jump to very near end of subroutine
				;
				; after 1st time thru...
				;
Le48d               lda #$20	; \ 
                    sta $06	; / default charset offset of $20 for CCC LEDs
                    lda #$e0	; 
                    cpx #$18	; compare X (counter var $98) to 24 [1st time here]
                    bne Le49a	; skip jmp if not 24
                    jmp Le529	;   if X=$18, jump to send $e0 (%1110 0000) [0s+start bit+two n/c lines]
				;
				; select appropriate LED controller
				;
Le49a               ldx $99	; fetch variable $99 (digit position?)
                    cpx #$05	; are we at digit position 5?
                    bcc Le4b5	; branch down if pos < 5 (CCC LEDs; controller already selected)
				;  but for digits 5 and up (all top LEDs)
                    lda #$10	;   \
                    sta $06	;   / change charset offset to $10 for top LEDs
                    lda #$ff	;   %1111 1111 LED sel [11] select LED controller 3
                    cpx #$0b	;   compare digit position to 12
                    bcc Le4b2	;   branch down if < 12
                    lda #$fb	;     %1111 1011 LED sel [10] select LED controller 2
                    cpx #$11	;     compare digit position to 17
                    bcc Le4b2	;     branch down if < 17
                    lda #$f7	;       %1111 0111 LED sel [01] select LED controller 1
Le4b2               sta $4002	;   write to PIA2-B - sets select lines based on $99
				;
				; loop up and store the RAM location for this digit
				;
Le4b5               ldy $e288,x	; Y = 0-page RAM address from ROM table for current digit position (0-22)
				;
				;	   	        [------------] [-------
				;		        00 01 02 03 04 05 06 07
				;Le288 (for ref.)       /CCC  LEDs\ :: :: / TOP 
				;	                61 5f 5e 60 84(40)96 95 
				;
                		;  90 94 84(40)92 91 8c 8d 84(40)8e 28 29 2a 38  
				;  LEDs/ :: :: \TOP LEDs / :: :: \TOP LEDs / ^^-indicator lights?
				;  08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16
				;  -------] [---------------] [---------------]
				;
                    sty $0a	; store (RAM address for an LED digit's value) to $0a 
				;
				; handle special start bytes; $40 (%0010 0000)
				;  [start bit & two 0s for n/c segments?]
				;
                    tya		; Y->A (also store RAM address in A)
                    cpx #$05	; compare X to 5 ("digit" position)
                    beq Le527	; if == branch way down (straight to output of $40 values, start bits?)
                    cpx #$0b	; compare X to 12 (again)
                    beq Le527	; if == branch way down (straight to output of $40 values, start bits?)
                    cpx #$11	; compare X to 17 (again) 
                    beq Le527	; if == branch way down (straight to output of $40 values, start bits?)
				;
                    lda $0000,y	; otherwise, get a digit charcode (Y holds RAM address of current digit)
                    eor #$ff	; invert every bit of A (why? only used inverted for indicator lamps)
				;
				; special treatment for last byte
				;   ("THANK YOU" and "MAKE SELECTION" lights)
				;   (keypress related code NOT understood)
				;
                    cpx #$16	; \ (22) last byte of display data
                    bne Le4de	; / for all bytes before the last one, skip this code
                    ora #$04	;   set bit 2 (%0000 0100) of (inverted) A
                    ldx $9c	;   get $9c variable to X (current keypress code?)
                    bmi Le527	;   branch based on bit 7 of X? (no kepress?); inc counter & send byte [not charcode]
                    and #$fb	;   clear bit 2 (%1111 1011) of (inverted) A
                    ldx #$ff	;   \
                    stx $9c	;   / store $ff to $9c variable (keypress code?)
                    bne Le527	;   unconditional branch; increment counter and send byte [not a charcode]
				;
Le4de               eor #$ff	; un-invert all bits of A (back to original)
                    sta $08	; store data byte to temp var $08
                    lda $06	; get charset offset ($20 or $10)
                    sec		; set carry flag for subtraction
                    sbc #$10	; A=A-16 (reduce $20 to $10, or $10 to $00)
                    ora $08	; OR with temp var $08 (potentially sets bit 4 to select 2nd charset)
                    tay		; A->Y copy updated output charcode to Y
                    lda $e268,y	; get LED-code from ROM table for charcode (Y should be $00-$1f)
                    ldy $0a	; get variable $0a (RAM address for this digit's charcode) to Y
                    cpy #$96	; is variable RAM address $96? (one of the top LED digits)
                    bne Le503	; if not skip to next check...
				;
				; special case: $96, 1s digit of "MOST POPULAR SELECTION" on top of juke
				;
                    pha		;   push LED code
                    and #$2f	;   %0010 1111 isolate some bits [LED segments bafg and d?]
                    sta $08	;   store to $08 temp var
                    pla		;   pull LED code back to A
                    and #$50	;   %0101 0000 isolate some bits [LED segments c and e?]
                    asl a	;   shift A left
                    asl a	;   ...twice (bit 6 goes into carry bit)
                    bcc Le501	;   skip next opcode if bit 6 was high
                    ora #$10	;     %0001 0000 set bit 5 of A [effectively swaps bits 4 & 6, segments c & e]
Le501               ora $08	;   stored modified byte at $08 temp var [is this LED wired differently??]
				;
Le503               cpy #$5f	; do we have the RAM address to 2nd digit of the CCC LEDs?
                    beq Le521	; if so, branch down to special case
				;
                    ldx $35	; get service/normal mode flag
                    bne Le527	; in service mode, branch down
				;
				; normal ("on") mode
				;
                    cpy #$61	; compare Y to $61 (rightmost CCC LED digit)
                    bne Le517	; if not equal branch down
                    ldy $78	;   get $78 var(?)
                    beq Le527	;   branch ahead if 0
Le513               ora #$08	;     %0000 1000 set bit 3 of A to turn a blank in to "-" 
                    bne Le527	;     always branch
Le517               cpy #$60	; compare Y to $60 (3rd CCC LED digit)
                    bne Le527	; if Y <> $60, branch down
                    ldy $0d	;   get $0d var(?) (video system related?)
                    beq Le513	;   if $0d=0, branch back up
                    bne Le527	;   (always) branch ahead
				;
				; special case: $5f - 2nd digit of CCC LEDs
				;
Le521               ldy $86	; check $86 variable(CCC LED decimal point flag?)
                    beq Le527	; if variable is 0, skip over next opcode
                    ora #$80	;   %1000 0000 set high bit of (light the decimal point segment?)
				;
				; (common code)
				;
Le527               inc $99	; increment digit position counter
Le529               jsr Se531	; transmit out a byte (A) to LED controller
Le52c               pla		; \
                    tay		; / pull Y from stack (ref $e45f)
                    dec $98	; decrement counter variable
                    rts		; done
				;
				;*****************************
				; SEND BYTE TO LED CONTROLLER
				;*****************************
				;   expects A as input (byte to send to LED controller)
				;   $06: selects 7 or 8 bit outut and charset 
				;     ($10 for 7 bits and top LED charset
				;      $20 for 8 bits and bottom LED charset)
				;   (only ever called from above routine)
				;
Se531               ldy $06	; get charset offset ($10 or $20) to Y
                    ldx $4002	; get initial PIA2-B status to X
                    cpy #$11	; compare charset offset to $11 (17) to set/clear carry flag
                    ldy #$07	; set bit counter to 7 bits (default for top LEDs)
                    bcc Le53e	; skip next opcode if Y < $11 ($10, top charset)
                    ldy #$08	;   Y=8 (8 bits) [7 for $06=$10, 8 for $06=$20]
Le53e               sta $08	; store A at $08 (byte to output)
Le540               ror $08	;   rotate $08 right (puts bit 0 into carry bit)
                    lda $4002	;   get PIA-2-B status to A
                    and #$fe	;   (%1111 1110) clear bit 0 (data to LED displays)
                    ora #$30	;   (%0011 0000) set bits 4 & 5 (both LED clocks)
                    stx $4002	;   store *X* to PIA2-B (original state)
                    bcs Le550	;   skip next if carry set (from ROR at $e540, if bit 0 was high)
                    ora #$01	;     set bit 0 (data to LED displays)
Le550               sta $4002	;   write A to PIA2-B (data bit for an LED controller)
                    lda $06	;   get charset offset (%0001 0000 or %0010 0000)
                    eor #$ff	;   invert all bits
                    and $4002	;   clear a bit from PIA-2 based on $06 (clock bit for appropriate display)
                    sta $4002	;   write A to PIA2-B (strobe to clock the data bit to the controller)
                    ora $06	;   set a bit back in A based on $06 (clear clock bit for display)
                    tax		;   A->X (update X, was initial PIA2-B status)
                    dey		;   decrement Y, bit counter
                    bne Le540	; loopback 7 or 8 times to rotate $08 around and clock out each bit
                    txa		; X -> A
                    ora #$01	; set bit 0 (data to LED displays - inverted in h/w)
                    sta $4002	; write to PIA2-B
                    rts		; 
				;
				;*********************************
				; HANDLE SERVICE SWITCH, OVERRIDE
				; AND STORE PIA1-A STATUS
				;*********************************
				;
Se56a               lda $2000	; get PIA1-A input status
                    eor #$01	; flip bit 0 (unknown output function; N/C on schematics)
                    sta $2000	; update PIA1-A
                    and $2000	; read back again (why?)
Le575               sta $68	; store PIA1-A status to $68 (read many places)
                    and #$04	; %0000 0100 isolate bit 2: service mode switch
                    beq Le57d	; skip next opcode if bit 2 is low
                    lda #$ff	;   A=$ff
Le57d               sta $35	; store boolean to indicate service mode ($ff = service mode)
                    beq Le588	; branch down if 0 (i.e. NOT service mode)
                    lda $cb	; get $cb var (service switch debounce timer?)
                    beq Le587	; skip next if 0
                    dec $cb	;   decrement counter/timer
Le587               rts		; done
				;
				; handle service mode switch off
				;
Le588               ldx $cb	; get decounce timer(?) var
                    beq Le591	; branch down if 0
                    ldy #$ff	;     Y=$ff
                    sty $cb	;     init var $cb to $ff
Le590               rts		;     done
Le591               ldx $0338	; get operator setting for SERVICE SWITCH OVERRIDE (program w/door closed)
                    beq Le590	; branch to rts if 0 (factory setting = no override) otherwise...
                    lda $68	;     get PIA1-A status
                    ora #$04	;     set bit 2 (%0000 0100) to override into service mode regardless of switch
                    bne Le575	; (always) branch back near top of routine to stay in service mode
				;
				;*************************************
				; ADD CREDITS & UPDATE CREDIT DISPLAY
				;*************************************
				;
Se59c               ldx #$ff	; X=255 (used to check for freeplay mode)
                    lda $02f5	; get current credit count
                    clc		; (clear carry for addition)
                    adc $02f4	; add newly-entered credits to current count
                    bcs Le5ac	; branch ahead if that overruns a byte
                    cpx $031b	;   check freeplay mode setting
                    bne Le5ad	;   if not freeplay, mode skip next opcode
Le5ac               txa		;     (A=255)
Le5ad               sta $24	; store updated current credits for conversion to digits
                    lda #$70	; \ offset for output
                    sta $26	; / results will be placed on "SELECTIONS REMAINING" LEDs
                    jsr See45	; parse value into digits
				;
				; suppress any leading 0s from display
				;
                    ldx #$00	; init loop counter
Le5b8               lda $8c,x	;   get digit contents [ignore $8b digit; it's always 0]
                    bne Le5c5	;   if digit is non-zero, exit loop
                    lda #$0e	;   (charcode for blank digit)
                    sta $8c,x	;   replace a leading zero with blank space
                    inx		;   next digit
                    cpx #$02	;   have we tried 2 digits? [100s and 10s]
                    bne Le5b8	; loopback if not [could be 1 byte shorter by unrolling loop]
Le5c5               rts		; done
                  		;
				;********************************
				; SELFTEST RAM & ROM FINISH INIT
				;********************************
				;  (reached by JMP from $e252)
				;
Le5c6               jsr Sef1e	; verify RAM checksum (of programmable values)
                    bne Le5ce	; if checksum bad, skip over next opcode
                    jmp Le699	;   RAM checksum OK, jump (way) down...
Le5ce               lda #$00	; RAM checksum was BAD, so let's do a full RAM test...
				;
				; perform full RAM test
				;
                    sta $00	; pointer $00/$01 
                    sta $01	; to $0000
                    sta $02	; $02 = 0 (RAM test indicator?)
Le5d6               ldy #$03	; init Y counter to 3 (start past pointer)
Le5d8               lda #$00	; init counter A=0
Le5da               clc		; clear carry flag
                    adc #$01	;     A=A+1
                    cmp #$fd	;     have we reached $fd?
                    beq Le5d8	;     if SO loop back
                    sta ($00),y	;     store A counter to $00/$01 pointer + Y 
                    iny		;     increment Y counter
                    bne Le5da	;   loopback until Y wraps to 0
                    inc $01	;   increment MSB of pointer
                    ldx #$08	;   X=8 (page 8)
                    cpx $01	;   is MSB of pointer up to 8 yet?
                    bne Le5da	; if not, loopback until done with all of RAM $0003-$07ff
                    lda #$00	; \
                    sta $00	;  |
                    sta $01	;  |
                    ldy #$03	;  | init same loop as above
Le5f6               lda #$00	;  |
Le5f8               clc		; / 
                    adc #$01	; \
                    cmp #$fd	;  | same loop as above, but checking values  
                    beq Le5f6	;  | 
                    cmp ($00),y	;  |check value in RAM vs what it should be
                    bne Le63c	;  | if byte is WRONG branch down to give RAM error
                    iny		;  |increment Y counter
                    bne Le5f8	;  |loopback until wrapped
                    inc $01	;  |increment pointer MSB (page) by 1
                    ldx #$08	;  |X=8
                    cpx $01	;  |are we to page 8?
                    bne Le5f8	; /loopback until we are
                    lda #$00	; \
                    sta $00	;  |
                    sta $01	;  |
                    ldy #$03	;  | init same loop again
Le616               lda #$55	;  | A=(%0101 0101) bit stripe test
                    sta ($00),y	;  |write byte
                    cmp ($00),y	;  |verify byte
                    bne Le63c	;  |if WRONG, branch down to give RAM error
                    lda #$aa	;  |A=(%1010 1010) opposite bits
                    sta ($00),y	;  |write byte
                    cmp ($00),y	;  |verify it
                    bne Le63c	;  |if WRONG, branch down to give RAM error
                    lda #$00	;  |now zero
                    sta ($00),y	;  |write byte 
                    cmp ($00),y	;  |verify it
                    bne Le63c	;  |if WRONG, branch down to give RAM error
                    iny		;  |increment Y counter
                    bne Le616	;  |loopback untio Y=0
                    inc $01	;  |increment MSB (page number)
                    ldx #$08	;  |X=8
                    cpx $01	;  |compare page number to 8
                    bne Le616	; /loopback until done with all of RAM
                    clc		; clear carry flag (since RAM test was OK!)
                    bcc Le63d	; unconditional branch over setting carry flag
Le63c               sec		;   set carry flag (to signal RAM test fail)
Le63d               lda $02	; get variable at $02
                    beq Le644	; if 0 branch over next
                    jmp Le3ae	;   jump if $02 var is NOT 0  
Le644               bcc Le64a	; skip over if carry clear (RAM was OK)
                    lda #$02	;   error code 2 (defective RAM)
                    bne Le65e	;   reset the factory settings
				;
				; perform ROM test
				;
Le64a               jsr Se1a4	; verify ROM checksum
                    beq Le65c	; if ROM is OK (z flag set) branch to next check
				;
				; ROM error detected
				;
                    sta $24	; store A (ROM checksum) at $24 (to convert to decimal digits)
                    lda #$0c	; \ set offset
                    sta $26	; / so output will be on "SELECTION BEING MADE" LEDs
                    jsr See45	; parse binary number into decimal digits
                    lda #$03	; flag error code 3 (defective ROM)
                    bne Le65e	; unconditional branch past next instruction
				;
Le65c               lda #$01	; error code 1 (checksum fault) [only issue in $0300-$03__]
Le65e               sta $6b	; store error code [error 0 if jmped here; factory settings were reset]
                    jsr Sfb2d	; reset the factory settings
                    jsr Sf4b7	; clear regular selection list
                    lda #$16	; \ 
                    sta $fb	; / init $fb variable to $16 (22)
                    lda #$2b	; \
                    sta $fc	;  | put $2b at $fc
                    sta $fe	; /  ...and at $fe
                    lda #$03	; \
                    sta $fd	;  | put $03 at $fd
                    sta $ff	; /  ...and at $ff [$fc/$fd and $fe/$ff point to $032b??]
                    jsr Sf9df	; [appears to init a bunch of tables etc.]
                    lda $6b	; fetch error code
                    bne Le680	; if (non-zero) error present, skip RAM checksum update
                    jsr Sef18	;   update the RAM checksum
Le680               jsr Sfb65	; display error code on CCC LEDs
                    jsr Se3c7	; 2s delay
                    lda $2000	; get PIA1-A status
                    and #$04	; (%0000 0100) isolate bit 2, SERVICE/ON mode
                    beq Le699	; if 0 (bit 2 low, "ON" mode, NOT svc mode) branch ahead
                    lda $6b	; get error code
                    cmp #$01	; compare to 1 (checksum fault)
                    bne Le696	; skip next opcode if not 1
                    jsr Sec99	; enter programming mode if RAM error was detected (it will set a new checksum)
Le696               jmp Le5c6	; jump back to start of RAM/ROM check routine
                    		;
				; RAM checksum was OK,
				; continue boot-up in "ON" mode...
				;
Le699               jsr Sff67	; zero & init variables
                    jsr Se56a	; check service mode switch?
                    lda $68	; get PIA1-A status
                    and #$02	; check the CCC battery level
                    bne Le6af	; branch down if battery OK...
				;
				; handle CCC battery-low condition
				;
                    lda #$04	;   \ set error code #4 for battery low
                    sta $6b	;   / error code storage variable
                    jsr Sfb65	;   display Err code on CCC LEDs
                    jsr Se3c7	;   2s delay
				;
Le6af               lda $031c	; get setting to retain/forget credits at power-up
                    bne Le6c6	; if not 0 (i.e. retain credits) branch ahead...
				;
				; cancel credits on power-up if so programmed
				;
                    ldx $cd	; get $cd variable (check for warm-reboot from svc mode?)
                    bne Le6be	; if non-zero skip down
                    ldx $cc	;   get $cc variable
                    beq Le6c3	;   if is zero, branch to forget credits anyway
                    sta $cc	; store setting (should be $ff to get here?) at $cc variable
Le6be               sta $cd	; and at $cd variable
                    jmp Le6c6	; skip over next jsr (don't clear credits)
Le6c3               jsr Se749	; clear credits (and some other stuff)
				;
				; init more stuff?
				;
Le6c6               jsr Sf73c	; [subroutine]
                    jsr Se3d4	; 100ms delay
                    jsr Sff35	; clear LED display data
                    jsr Se59c	; add any credits and update the credit display
                    lda #$12	; \  A=$12 (18) 
                    sta $eb	;  | init var $eb
                    sta $e0	; /  init var $e0
                    lda #$00	; \  A=0
                    sta $39	;  | init var $39 (video mode off?)
                    sta $28	;  | init var $28 \
                    sta $29	;  | init var $29  top "SELECTION BEING MADE" LEDs = 000
                    sta $2a	; /  init var $2a /
                    lda #$72	; \  A=$72 (114) init pointers into "event" table?
                    sta $0171	;  | init var $0171 (page 1 offset pointing to $0172?)
                    sta $0170	; /  init var $0170 (page 1 offset pointing to $0172?)
                    jsr Sfeac	; (enter 6 codes into event table?)
                    ldx $ce	; get $ce var (?)
                    beq Le701	; if 0, down to main loop...
				;
Le6f1               dec $ce	;   decrement counter $ce
                    bne Le6fa	;   skip ahead if 0
                    jsr Sf501	;     clear play/money counters
                    beq Le701	;     always branch to main loop? (subroutine returns A=0)
Le6fa               jsr Sf9df	;   [subroutine]
                    dec $ce	;   decrement counter $ce
                    bne Le6f1	;   loopback unless 0, otherwise fall into...
				;
				;***********
				; MAIN LOOP
				;***********
				;
Le701               cld		; clear decimal flag (just in case)
                    dec $e4	; decrement $e4 counter (fast counter)
                    bne Le712	; branch ahead unless 0
                    lda $cf	;   get MUTE(?) flag?
                    beq Le712	;   branch ahead if 0
                    lda $2002	;     get PIA1-B state
                    ora #$01	;     (%0000 0001) set bit 0 - MUTE off?
                    sta $2002	;     output to PIA1-B
Le712               jsr Sf646	; [calls 4 other subroutines]
                    jsr Se75c	; [main loop sub-routine 1]
                    jsr Se885	; [main loop sub-routine 2]
                    jsr Sef5d	; [handle user entry normal mode?]
                    lda $35	; get (service mode flag)
                    beq Le728	; skip ahead if in regular mode
                    jsr Sf35b	;   handle user entry in service mode
                    jmp Le701	;   back to top of main loop
Le728               lda $ed	; get $ed var(?)
                    bne Le701	; if $ed var not 0, loopback to top of main loop
                    bit $68	; set flags per PIA1-A status
                    bpl Le740	; branch based on bit 7 PIA1-A (CANCEL button)
                    lda $65	;   get $65 counter var (debounce?)
                    bne Le701	;     if $65 counter var not 0, back to top of main loop
                    sta $cf	;     init $cf to 0 (mute status?)
                    lda #$af	;     \ 
                    sta $65	;     / init $65 counter var to $af (debounce?)
                    lda #$4a	;     \ 
                    sta $eb	;     / init $eb var to $4a
                    bne Le701	;     (unconditional branch) loopback to top
				;
				; CANCEL (not?) pressed
				;
Le740               lda $65	;   get $65 var
                    beq Le701	;   if $65 var = 0 loopback to top (w/o decrementing it)
                    dec $65	;     decrement $65 counter (debounce?)
                    jmp Le701	;     jump back to top of loop
				;
				;***************
				; CLEAR CREDITS
				;***************
				;   (and another data table?)
				;                  
Se749               ldx #$09	; X=9
                    lda #$00	; A=0
Le74d               sta $02f4,x	; loop clears $02f4-$02fd
                    dex		;   next byte
                    bpl Le74d	; loopback until done
                    ldx #$31	; init loop counter
Le755               sta $0100,x	; loop clears $0100-$0131 (?)
                    dex		;   next byte
                    bpl Le755	; loopback until done
                    rts		; done here!
                    		;
				;************************
				; main-loop subroutine 1 (once per second??)
				;************************
				;   (only called from main loop)
				;
Se75c               dec $3d	; \ small delay: decrement $3d
                    bne Le7c5	; /              loopback until 0
                    dec $9a	; decrement $9a var (THANK YOU indicator lamp timer)
                    bpl Le77e	; branch if not a rollunder
                    lda #$00	;   \ init $38 to 0
                    sta $38	;   / turn off THANK YOU (and MAKE SELECTION?) lamps
                    lda $ed	;   get $ed var(?)
                    beq Le77e	;   branch down if $ed var = 0
                    lda $2002	;     get PIA1-B status
                    and #$10	;     isolate bit 4 (%0001 0000) - TURNTABLE MOTOR
                    bne Le77e	;     branch if bit is high (turntable on?)
                    lda $35	;       get service/normal boolean
                    bne Le77e	;       branch if in service mode
                    dec $e0	;         decrement $e0 counter
                    bne Le77e	;         branch if non-zero
                    jsr Sf877	;           [subroutine]
				;
Le77e               lda $f1	; get $f1 var (video system timer related?)
                    beq Le788	; skip ahead of $f1 var = 0
                    eor #$ff	;   invert $f1 var's bits
                    beq Le788	;   skip next opcode if $f1 var now = 0 (i.e. was #$ff)
                    dec $f1	;     decrement $f1 counter
Le788               lda #$e7	; \ reset the $3d timer var to $e7
                    sta $3d	; / 
                    dec $62	; decrement $62 counter (seconds timer?)
                    bne Le79e	; branch ahead if not 0 yet
                    dec $df	;   decrement $df counter (minutes timer?)
                    bne Le79a	;   branch ahead if not 0 yet
                    lda #$00	;     \
                    sta $ec	;      | init $ec and $f6 vars to 0
                    sta $f6	;     /
Le79a               lda #$3c	;   \ init $62 var to $3c (60) (seconds timer?)
                    sta $62	;   /
Le79e               lda #$ff	; \ invert $f1 var
                    eor $f1	; /
                    ora $70	; OR inverted $f1 var with $70 var? (selection playing?)
                    beq Le7aa	; branch down result is 0
                    lda $70	;   get $70 var
                    ora $39	;   OR it with $39 var (video mode boolean?)
Le7aa               ora $f3	; OR A with $f3 var? (turntable motor flag?)
                    pha		; A to stack
                    cmp $9b	; compare A to $9b var(?)
                    beq Le7ba	; skip ahead if equal
                    lda $35	;   get service/normal mode flag
                    bne Le7ba	;   branch down if in service mode
                    lda #$40	;     \
                    jsr Sfcff	;     / put #$40 into event table (idle?)
Le7ba               pla		; A back from stack
                    sta $9b	; store it to $9b var
                    bne Le7e4	; 
                    lda $35	; get service/normal mode flag
                    bne Le7e4	; branch down if in service mode
                    lda $23	; get current keyed entry position
Le7c5               bne Le7e8	; if in entry process, skip ahead... (don't autoplay while keying)
                    sta $28	;   store 0s (not blank spaces)
                    sta $29	;   on "SELECTION BEING MADE" LEDs
                    sta $2a	;   
				;
				; check for autoplay mode and time 
				;
                    lda $0320	; get autoplay mode setting
                    beq Le7e4	; if autoplay is off (mode 0) move along...
                    lda $fa	; get autoplay timer
                    beq Le7de	; if it has reached 0, branch down play something
                    dec $83	;   decrement seconds timer(?)
                    bne Le7e8	;   if seconds timer > 0, move along...
                    dec $fa	;     decrement autoplay minutes timer
                    bne Le7e4	;     if minutes timer hasn't reached 0, reset seconds timer
Le7de               jsr Sf535	; do autoplay
                    jsr Sf95a	; [do some background tasks]
				;
Le7e4               lda #$3c	; \ init seconds timer to (60) seconds
                    sta $83	; /
				;
Le7e8               lda $35	; check service/normal mode?
                    bne Le834	; branch down for service mode
                    lda $89	; get $89 flag to A
                    beq Le7f2	; skip next opcode if $89 flag = 0
                    lda #$ff	;   A=$ff
Le7f2               eor #$ff	; invert every bit of A
                    sta $89	; store it back at flag $89
                    beq Le80e	; branch if flag is now 0
				;
                    lda $0700	;   get $0700 (table index?)
                    cmp #$40	;   compare to $40 (64)
                    bcc Le801	;   skip next opcode if A < $40
                    adc #$23	;     A=A+50
Le801               ldx $39	;   video mode boolean?
                    bne Le808	;   skip next opcode if $39 > 0
                    lda $0500	;     get $0500 (table index?)
Le808               sta $24	;   store to $24 var
                    lda #$78	;   A=$78 (offset to $1b = $93: "MOST POPULAR SELECTION" LEDs)
                    bne Le818	;   (always branch over next block of code)
				;
Le80e               lda $9b	; get $9b var (playing or not boolean?)
                    beq Le814	; skip next opcode if $9b=0 (not playing music)
                    lda $ef	;   get $ef var (selection #)
Le814               sta $24	; place value to be converted to decimal digits (selection # or 0)
                    lda #$74	; direct output to $8f-$92, top "SELECTION PLAYING" LEDs
				;
Le818               sta $26	; set offset for output to selected set of LEDs
                    tax		; A->X (X now has address offset: $74 or $78)
                    jsr See45	; parse 2-byte value into 4 digits (onto selected LEDs)
                    lda $39	; video mode boolean?
                    beq Le832	; branch ahead if $39=0
                    cpx #$78	;   compare X offset to $78 (for "MOST POPULAR...")
                    beq Le82e	;   if "MOST POPULAR..." branch ahead
                    lda $9b	;     get $9b var(?)
                    beq Le834	;     branch if $9b=0
                    lda $ed	;       get $ed (selection #?)
                    bne Le832	;       branch if $ed > 0
Le82e               inc $1c,x	;   \ increment 1st digit twice (of selected LEDs) (why? B-side?)
                    inc $1c,x	;   / 
Le832               inc $1c,x	; increment 1st digit (of selected LEDs)
				;
Le834               lda $38	; get $38 var (THANK YOU & MAKE SELECTION lights?)
                    ora #$09	; (%0000 1001) isolate bits 0 and 3
                    sta $38	; store $38 var back (clearing bit 1, the THANK YOU bit)
                    lda #$ff	; \
                    sta $41	;  set $41 & $6d vars to $ff	
                    sta $6d	; /
                    eor $031b	; check freeplay mode setting
                    beq Le881	; branch if freeplay mode
				;
				; handle regular coin-op mode (not freeplay)
				;
                    lda $02f4	; get credits just recieved for money in
                    clc		; clear carry flag (for addition)
                    adc $02f5	; add current credit count to A
                    bcs Le858	; if this rolls over 255 branch down
                    sta $41	;   store credit total to $41 var
                    bne Le858	;   if non-zero branch ahead
                    lda $38	;     get $38 var (THANK YOU & MAKE SELECTION lights?)
                    and #$fe	;     (%1111 1110) clear bit 0 (THANK YOU control bit?)
                    sta $38	;     store updated value
Le858               lda $02f6	;   get credits just recieved for money in (video mode)
                    clc		;   clear carry flag (for additition)
                    adc $02f7	;   add current credit count to A
                    bcs Le881	;   if this rolls over, branch to near end
                    sta $6d	;   store to $6d var
                    bne Le881	;   if non-zero branch to near end
                    lda $41	;   get $41 var (# of credits?)
                    beq Le87b	;   if out of cretids, branch down
                    cmp $02fb	;     compare $41 var to $02fb (selection-has-been-made boolean?)
                    bcc Le87b	;     branch if...
                    bne Le875	;     branch if not 0
                    lda $02fc	;       get $02fc var(?) MSB?
                    bne Le87b	;       branch if non-zero
Le875               lda #$01	;     A=1
                    sta $6d	;     init $6d var to 1 (video credit related?)
                    bne Le881	;     unconditional branch down
Le87b               lda $38	;   get $38 var (THANK YOU & MAKE SELECTION lights?)
                    and #$f7	;   (%1111 0111) clear bit 3 (MAKE SELECTION control bit?)
                    sta $38	;   store updated value
				;
Le881               jsr Se45f	; (update LED displays - doesn't use parent loop?)
                    rts		; done
				; 
                    		;*************************
				; main loop subroutine 2? (lots of video related stuff?)
				;*************************
				;  (only called from main loop)
				;  [needs more attention]
				; 
Se885               lda $eb	; get $eb var
                    beq Le88c	; skip next opcode if $eb=0
                    jmp Le9d6	;   jump down a ways
Le88c               lda $ed	; get $ed var
                    bne Le8db	; 
                    lda $f9	;
                    beq Le898	;
                    eor $f1	;
                    bne Le8db	;
Le898               ldx $3c	; 
                    bne Le8c8	;
                    lda $f1	; 
                    cmp #$ff	; 
                    beq Le8db	; if $f1 var = $ff, branch
                    cmp $032b	; check fill time during video search setting
                    bcc Le8db	; if $f1 var < $032b var, branch
                    inc $e9	;
                    lda $e9	;
                    cmp #$62	; (98)
                    bcc Le8b3	;
                    lda #$00	; \  
                    sta $e9	;  | init $e9 var and $24 var (LSB) to 0
Le8b3               sta $24	; /  (where is $25 set? $26?)
                    jsr See45	; parse binary value into decimal digits
                    lda $1e	; get ones digit?
                    cmp #$08	; compare to 8
                    bcc Le8c2	;
                    inc $e9	;
                    inc $e9	;
Le8c2               lda #$1a	;
                    sta $eb	;
                    bne Le8fb	;
Le8c8               lda $032c	; get record/video mix setting
                    beq Le8d4	; if 0 branch
                    lda $f5	;   get $f5 var
                    cmp $032c	;   compare to record/video mix setting
                    bcs Le8e7	;   
Le8d4               lda $f1	; 
                    cmp $032b	; compare to fill-time during video search setting
                    bcs Le8e7	;
Le8db               lda $ec	;
                    bne Le8fb	;
                    lda $ed	;
                    beq Le8ef	;
                    lda $ea	;
                    bne Le8c2	;
Le8e7               lda #$00	;
                    sta $f5	;
                    lda #$22	;
                    bne Le8f9	;
Le8ef               lda $f9	;
                    bne Le8f7	;
                    lda $35	;
                    bne Le8e7	;
Le8f7               lda #$2a	;
Le8f9               sta $eb	;
Le8fb               lda $eb	;
                    ora $f6	;
                    bne Le910	;
                    ldx $f7	; video playlist selection pointer?
                    cpx $f8	; video playlist selection pointer?
                    beq Le910	; skip down if 0
                    lda $0300,x	;   get value video playlist?
                    sta $7a	;   store as 2nd byte of message to video system?
                    lda #$33	;   \ set $eb var to #$33
                    sta $eb	;   /
Le910               lda #$00	;   \ set $78 var to #$00
                    sta $78	;   /
                    jsr Se081	; check for & receive message from video system?
                    beq Le91f	; branch if return code is 0 (success?)
                    bmi Le91c	; branch if return code is >= $80
                    rts		;
				;
				; return code >= $80
				;
Le91c               jmp Lfb7a	; do wallbox comms?
				;
				; return code == 0 (success?)
				;
Le91f               lda $4d	; \  get 1st byte of signal from video system (# of bytes to copy)
                    ldx #$4d	;  } select serial input buffer location
                    jsr Se1b9	; /  copy serial input buffer contents to page-1 table
                    lda $4d	; get $4d (some sort of signal from video system?)
				;
				; check for $02 command (from video system?)
				;
                    cmp #$02	; compare to $02
                    bne Le938	; if not, skip ahead to next check
                    sta $fb	;   store A ($02) in $fb var
                    ldx #$ff	;   X=$ff
                    stx $f1	;   set $f1 var to $ff
                    inx		;   X=0
                    stx $39	;   init video mode boolean? to 0
                    stx $f9	;   init $f9 to 0
                    rts		;   done here!
				;
				; check for $16 command (from video system?)
				;
Le938               cmp #$16	; compare to $16
                    bne Le959	; if not, skip ahead to next check
                    ldx #$04	;   init loop counter 4
Le93e               lda $4d,x	;   loop to copy
                    sta $fb,x	;     from $4d-$52
                    dex		;     ..to $fb-$ff
                    bpl Le93e	;   loopback 'til X roll-under
                    stx $39	;   init video mode boolean? to $ff
                    inx		;   \  (X=0)
                    stx $ec	;    | init $ec var to 0
                    stx $f6	;   /  init $f6 var to 0
                    lda $fc	;   get $fc var
                    beq Le952	;   skip next opcode if $fc=0
                    dec $fc	;     decrement $fc counter
Le952               lda $fe	;   get $fe var
                    beq Le958	;   skip next opcode if $fe=0
                    dec $fe	;     decrement $fe counter
Le958               rts		;   done here!
				;
				; check for $2a command (from video system?)
				;
Le959               cmp #$2a	; compare to $2a
                    bne Le970	; if not, skip ahead to next check
                    lda $ed	;   get $ed var
                    bne Le963	;   skip next opcode if not 0
                    sta $ec	;     copy $ed var to $ec var
Le963               lda $ea	;   get $ea var
                    beq Le969	;   skip next opcode if 0
                    sta $e8	;     copy $ea var to $e8
Le969               lda #$00	;   A=0
                    sta $f6	;   init $f6
                    sta $f1	;   init $f1
                    rts		;   done here!
				;
				; check for $1b command (from video system?)
				;
Le970               cmp #$1b	; compare to $1b
                    bne Le98f	; if no, skip ahead to next check 
                    lda $4e	;   get $4e var
                    sta $ef	;   copy to $ef var
                    lda #$ff	;   \
                    sta $f9	;    >
                    sta $cf	;   /  mute on/off status?
                    lda #$e8	;   A=$e8
                    jsr Sf72e	;   [subroutine]
                    inc $f5	;   increment $f5 var
                    lda #$e1	;   \ init $e4 var to $e1 (225)
                    sta $e4	;   /
                    rts		;   done
				;
				; check for $23 command
				;
Le98a               cmp #$23	; compare to $23
                    beq Le9aa	;   if so, branch ahead to code
                    rts		; otherwise, done here!
				;
				; check for $33 command
				;
Le98f               cmp #$33	; compare to $33
                    bne Le98a	; if not, branch up to last check
                    lda $ed	;   get $ed var
                    bne Le9a4	;   if $ed not zero, branch ahead
                    lda #$ee	;   A=$ee
                    jsr Sf72e	;   [store some selection-related variables]
                    lda $2002	;   get PIA1-B
                    and #$fe	;   clear bit 0 (1111 1110) - MUTE off?
                    sta $2002	;   output to PIA1-B
Le9a4               lda #$00	;   A=0
                    sta $f9	;   init $f9 var (?)
                    sta $cf	;   and $cf var(?)
				;
				; handle $23 command
				;
Le9aa               lda $4e	; get $4e var(?)   
                    cmp #$03	; compare to 3
                    bcs Le9c8	; if $4e var >= 3 branch down
                    cmp #$01	; compare to 1
                    beq Le9d0	; if $4e var == 1 branch down
                    sta $f1	; store ($4e var) to $f1 var
                    ldx #$00	; \ init $f6 var to 0 
                    stx $f6	; /
                    cmp #$02	; compare ($4e var ) to 2
                    bne Le9c3	; if $4e var <> 2 branch down
                    stx $f9	;   init $f9 var to 0
                    dex		;   X is now $ff
                    stx $f1	;   init $f1 var to $ff
Le9c3               lda #$00	; \ init $ec var to 0
                    sta $ec	; /
                    rts		; done
				;
Le9c8               sta $f1	; store ($4e var) to $f1 var
                    cmp #$ff	; compare it to $ff
                    bne Le9d0	; if <> $ff skip next opcode
                    dec $f1	;   decrement $f1 counter
Le9d0               lda #$ff	; \ init $f6 var to $ff
                    sta $f6	; /
                    bne Le9c3	; always branch up
				;
Le9d6               lda $eb	; get $eb var(?)
                    sta $79	; copy it to $79 var [1st byte to video system?]
                    ldx $fb	; get $fb var (video-related?)
                    cpx #$02	; is it 2?
                    bne Le9e7	; if not, branch ahead
                    lda $3c	;   get $3c var
                    sta $ed	;   copy it to $ed var
Le9e4               jmp Le910	;   jump back up a ways
Le9e7               ldx #$00	; X=0
                    cmp #$33	; compare $fb var to $33 (51)
                    bne Lea12	; branch down if not equal
                    pha		;   push A
                    lda $fc	;   get $fc var (video autoplay counter?)
                    clc		;   clear carry flag for addition
                    adc $fd	;   add $fd var to A
                    cmp $7a	;   compare to $7a var (byte to video system?)
                    bcs Lea0d	;   branch if A >= $7a var
                    ldy $7a	;   get $7a var to Y
                    cpy #$64	;   compare $7a var to $64 (100)
                    bcc Lea0b	;   branch if Y < 100
                    ldy $fe	;   get $fe var to Y
                    beq Lea0b	;   branch if it's 0
                    tya		;   copy Y to A
                    clc		;   clear carry flag for addition
                    adc $ff	;   add the value of the $ff var to A
                    adc #$64	;   add $64 (100) to A
                    cmp $7a	;   compare result to $7a var
                    bcs Lea0d	;   branch if A >= $7a var
Lea0b               sta $7a	;
				;
				; add byte to running total
				;
Lea0d               pla		; pull A
                    clc		; \ clear carry flag for addition
                    adc $7a	; / add a to running total in $7a
                    inx		; increment X (to next byte)
				;
				; calculate checksum byte for message to video system?
				;
Lea12               eor #$ff	; \ 
                    sec		;  } calculate 2's complement of A [so that total of all bytes = 0]
                    adc #$00	; /
                    sta $7a,x	; put checksum into buffer (last byte?) 
                    lda $78	; \ check a flag at $78
                    bne Lea27	; / if set skip ahead 
                    lda #$ff	; \ set a flag at $78
                    sta $78	; /
                    jsr Se117	; send (up to) 6-byte message to video system
                    jmp Lea2a	; skip next opcode
				;
Lea27               jsr Se137	; send (up to) 6-byte message to video system; skip initial handshake?
				;
Lea2a               lda $08	; get exit status 
                    beq Lea37	; on exit status 0 (success?) branch
                    bpl Le9e4	; of exit status >#$80 branch
                    asl a	; shift exit status left (move bit 7 to carry)
                    bne Lea36	; branch to rts if non-zero (e.g. for #$88 exit status) 
                    jsr Lfb7a	; for exit status #$80(?) do wallbox comms then return?
Lea36               rts		;
				;
				; success sending to video system
				;
Lea37               lda $df	; (minutes timer?)
                    beq Lea42	; if 0, skip over next block
                    lda $79	;   \  get $79 var (only lower 3 bits used) [# of bytes in buffer?]
                    ldx #$79	;    } select serial output buffer
                    jsr Se1b9	;   /  copy buffer contents to page-1 table [serial log for debugging?]
Lea42               ldx $39	; check video mode boolean(?)
                    bne Lea5a	; if non-zero, skip ahead...
                    stx $031e	;   change records/video setting? (to 0; video)
                    dec $39	;   decrement $39 var (from 0 to $ff)
                    jsr Sef18	;   update RAM checksum
                    lda #$01	;   \
                    jsr Sfcff	;   / put #$01 into event table
                    lda #$60	;   \
                    jsr Sfcff	;   / put #$60 into event table (selection-related?)
                    ldx #$ff	;   X=$ff
				;
Lea5a               lda $eb	; get $eb var
                    cmp #$2a	; is $eb var $2a?
                    bne Lea64	; if not, skip to next check...
                    stx $f9	;   
                    beq Lea80	;   
Lea64               cmp #$1a	; if $eb var $1a?
                    bne Lea6e	; if not, skip to next check...
                    stx $ea	;   
                    stx $3c	; 
                    beq Lea7a	;   
Lea6e               cmp #$22	; is $eb var $22?
                    bne Lea89	; if not, skip to next check...
                    lda $35	;
                    beq Lea7a	;
                    lda $3c	; 
                    beq Lea80	;
Lea7a               lda #$19	;
                    sta $e0	;
                    stx $ed	;
Lea80               stx $ec	;
                    ldx #$07	; 
                    stx $df	; (set minutes timer to 7?)
                    jmp Lea95	;
Lea89               cmp #$33	; is $eb var $33?
                    bne Lea95	; if not, skip ahead...
                    inc $f7	;
                    bne Lea95	;
                    lda #$90	; \ set $f7 to #$90
                    sta $f7	; /
Lea95               lda #$00	; \
                    sta $eb	;  } zero out $eb and $78 vars
                    sta $78	; /
                    rts		;
				;
				;***********************************
				; TAKE & COUNT MONEY - GIVE CREDITS
				;***********************************
				;
Sea9c               lda $02d7	; get nickels to be added to mechanical counter
                    beq Leac2	; branch ahead if no more needed
                    dec $3a	;   decrement counter/timer
                    bne Leac2	;   branch ahead if counter non-zero
                    lda #$12	;     \ 
                    sta $3a	;     / reset counter to $12 (18)
                    lda $2002	;     get PIA1-B
                    eor #$80	;     flip bit 7 (%1000 0000): MONEY COUNTER output
                    sta $2002	;     output to PIA1-B
                    and #$80	;     isolate b7 (%1000 0000)
                    bne Leac2	;     branch if counter bit high
                    dec $02d7	;     decrement nickels to be added to mechanical counter
                    ldx #$e4	;   \ point to 2-byte counter
                    jsr Sf18d	;   / increment total money counter (0-9999)
                    ldx #$fe	;   \ point to another 2-byte counter
                    jsr Sf18d	;   / increment total money counter (unresettable) (0-9999)
				;
				; read coin switches
				;
Leac2               ldx #$05	; init loop counter X=5
Leac4               lda $ff86,x	;   get value from ROM table
                    sta $4002	;   write to PIA2-B (select a coin input line)
                    lda $4000	;   read PIA2-A (see if coin switch is closed)
                    cpx #$05	;   is this the 1st time through the loop?
                    bne Lead6	;   if NOT branch down
                    lsr a	;     if SO shift A right 1 bit (to set/clear carry flag based on bit 0)
                    bcs Leadc	;     if carry set (bit 0 of A was 1) exit this loop [bill detected]
                    bcc Lead9	;     if carry clear (bit 0 of A was 0) continue with loop
Lead6               lsr a	;   2nd-5th loop; shift A left 1 bit
                    bcc Leadc	;   if carry clear (bit 0 of A was 0) exit this loop [coin detected]
Lead9               dex		;   decrement loop counter (next coin switch)
                    bne Leac4	; loopback unless we've reached X=0
Leadc               stx $44	; store coin input to variable (value 0-5)
				;
                    txa		; X->A
                    bne Leaee	; if non-zero (money entered) branch down
                    lda $45	;   get $45 var (bill-related?)
                    beq Leae7	;   if $45=0 branch over next opcode
                    dec $45	;     decrement $45 var (bill-related?)
Leae7               lda $80	;   get $80 var(?)
                    beq Leaed	;   if $80=0 skip next opcode
Leaeb               dec	 $80	;     decrement $80 counter
Leaed               rts		;   done here!
Leaee               lda $80	; get $80 counter(?)
                    beq Leaeb	; loopback if 0
                    bmi Leaf5	; skip next opcode if underrun
                    rts		;   done here!
Leaf5               lda #$04	; \ init $80 timer to 4
                    sta $80	; /
                    lda #$d6	; \ A=$d6
                    sta $0d	;  |store to variable $0d (initial value)
                    sta $cb	; / ..and to variable $cb
                    ldx $44	; init a loop counter/offset to money level entered; multiply-by-2 loop
Leb01               clc		; clear carry flag (ready for addition)
                    lda #$02	;   A=2
                    adc $0d	;   add 2 to temp variable $0d
                    dex		;   decrement loop counter
                    sta $0d	;   store updated temp var $0d
                    bne Leb01	; loopback unless X=0
                    ldx $0d	; point to 2-byte counter based on variable at $0d just calculated
                    jsr Sf18d	; increment a counter (0-9999) [for denomination just entered]
				;
                    lda $44	; get money-level entered again
                    cmp #$05	; was it a dollar-bill?
                    bne Leb46	; if not branch way down
                    lda #$02	;   \ A=2
                    sta $80	;   / set variable $80 to 2
                    lda $031a	;   get setting for dollar bill number of extra credits
                    clc		;   clear carry flag (for addition)
                    adc $02f5	;   add extra credits to current credit count
                    sta $02f5	;   store new sum back in variable location
                    lda $45	;   get $45 (variable related to $1 and/or $5 bills)
                    bne Leb2a	;   if <> 0 skip next opcode
                    sta $46	;     copy $45 var to $46 var
Leb2a               lda #$80	;   A=$80
                    sta $45	;   reset $45 variable?
                    inc $46	;   increment counter (1/5 bills?)
                    lda $46	;   get counter to A (1/5 bills?)
                    cmp #$05	;   check if $5 bill was inserted
                    bne Leb46	;   if not, branch down
				;
				; handle $5 bill (fix accounting)
				;
                    ldx #$e2	;     point to $02e2/$02e3 counter
                    jsr Sf18d	;     increment counter (0-9999) for $5 bills
                    lda $02e0	;     get LSB of 2-byte $1-bill counter
                    sec		;     set carry bit for subtraction
                    sbc #$05	;     subtract 5 (un-count 5 $1 bills if $5 bill was inserted)
                    bcs Leb46	;     if carry bit still set skip next opcode
                    dec $02e1	;       decrement MSB of $1-bill counter if needed
				;
Leb46               ldx $44	; get money-level entered (1-5) again
                    lda $0336	; get programmable setting [undocumented location 54?]
                    bne Leb53	; if non-zero branch down
                    lda #$02	;   if zero:
                    sta $38	;   store 2 at $38 var (THANK YOU & MAKE SELECTION lights?)
                    sta $9a	;   ...and $9a var (timer value for 2 secs of THANK YOU?)
				;
Leb53               lda $02d7	; get var (nickels to be added to physical counter?)
                    clc		; clear carry (for addition)
                    adc $0313,x	; add number of nickels for this coin level to A (from settings table)
                    sta $02d7   ; store updated total to (nickels to be added to physical counter?)
                    lda $0313,x ; now get number of nickels in for this coin level to A
                    sta $a0	; store at variable $a0
Leb62               sta $0c	; ...and at $0c
                    lda #$e0	; \ 
                    jsr Sfcff	; / put #$e0 into event table (coin-related)
                    lda $0c	; get variable $0c (recently set money deposited, in nickels)
                    clc		; clear carry for addition
                    adc $02f8	; add to money-left-over(?) variable
                    bcc Leb77	; branch down if that did NOT overflow into carry flag
                    sta $97	;   store A to $97 var
                    inc $97	;   increment $97 var (store overflowed value to $97?)
                    lda #$ff	;   A=$ff (set A to maximum byte value)
Leb77               sta $02f8	; store updated money-left-over(?) variable back
                    lda #$00	; A=0
                    sta $0f	; init $0f var to 0 (set to records-only mode pricing/credits)
                    jsr Sec14	; calculate credits for money recieved
                    lda $0b	; get newly earned credit total
                    sta $02f4	; store to $02f4
                    lda $0a	; any money left-over (but not enough for any credit)
                    sta $02f9	; store to $02f9 
                    lda #$ff	; ($ff is for records-only mode)
                    cmp $031e	; check records/video setting (loc 30)
                    beq Lebf7	; branch way down for records-only mode; continue for video mode...
				;
				; video mode credits?
				;
                    sta $0f	;   store $ff at $0f to indicate video mode to subroutine
                    jsr Sec14	;   calculate credits earned for the money (video pricing/credits)
                    lda $0a	;   any money left-over (but not enough for any credit)
                    sta $02fa	;   store to $02fa
                    lda $0b	;   get newly earned credit total
                    sta $02f6	;   store to $02f6
                    lda #$05	;   A=5
                    sta $06	;   store to $06 (?)
                    ldx #$04	;   init loop counter X=4
Leba7               ldy #$04	;     innter loop counter Y=4
Leba9               lda $0300,x	;       get record pricing level
                    beq Lebb3	;       skip down if pricing = 0 (unused slot?)
                    cmp $030a,y	;       compare to video pricing level
                    beq Lebb8	;       branch down if equal
Lebb3               dey		;       decrement video pricing level counter
                    bpl Leba9	;     loopback til done with Y video pricing levels
                    bmi Lebde	;     always branch to next X if no matches found
				;     for any record/video pricing levels found matching:
Lebb8               lda $030f,y	;     get Y-th video credit level
                    beq Lebde	;     branch to next X if credit level = 0 (unused slot)
                    sta $17	;     store (video level) credits in $17 var (LSB of divisor?)
                    lda #$00	;     \
                    sta $18	;      init $15 & $18 to 0 (inputs to division routine)
                    sta $15	;     /
                    lda $0305,x	;     get X-th record credit level
                    beq Lebde	;     branch down if credit level = 0 (unused slot?)
                    sta $16	;     store credits at $16 var (MSB of dividend?)
                    stx $06	;     store pricing level to $06 var(?)
                    lda $02f5	;     get current credit count
                    clc		;     clear carry flag
                    adc $02f4	;     add credit count to credits just recieved
                    bcc Lebd9	;     if no rollover, skip next opcode
                    lda #$ff	;       if rollover, A=255 (maximum credits)
Lebd9               cmp $0305,x	;     compare A to X-th credit level
                    bcs Lebea	;     branch out of loop if A>= credit level
Lebde               dex		;     next X   
                    bpl Leba7	;   loopback until done with all record pricing levels
                    lda #$05	;   A=5
                    cmp $06	;   compare to $06 var (if no matches found above?)
                    bne Lebea	;   skip next opcode if $06 var <> 5
                    jmp Sec99	;     jump to programming mode?!?
Lebea               jsr Sec5c	;   division routine (divides $16 by $17)
                    lda $16	;   get $16 var (result of division?)
                    sta $02fb	;   store to $02fb (?)
                    lda $15	;   get $15 var (result of division?)
                    sta $02fc	;   store to $02fc (?)
				;
				; rejoining records-only mode path
				;
Lebf7               lda #$00	; \ 
                    sta $02fd	; / zero out $02fd(?)
                    sta $37	; \ ...and $37 (credits recently spent?)
                    sta $3b	; / ...and $3b (records/video mode?)
                    jsr Se59c	; update credit display
                    lda $97	; get $97 var
                    beq Lec13	; if $97 var = 0, branch to rts
                    jsr Sf256	;   [something credit related]
                    lda $97	;   get $97 var to A
                    ldx #$00	;   \ init $97 var to 0
                    stx $97	;   /
                    jmp Leb77	;   jump back into this routine
Lec13               rts		; done
				;
				;*********************
				; CREDIT CALCULATIONS
				;*********************
				;   expects $02f8 - total money deposited but uncredited (in nickels)
				;   expects $0f - records-only/video mode (boolean)
				;   returns $0b - credits given for money just deposited
				;   returns $0a - balance of money left over (if any)
				;
Sec14               lda $02f8	; get value of money deposited (+left-over) [in nickels]
                    sta $0a	; put that in $0a var (value of money in) [in nickels]
                    lda #$00	; \ init $0b var to 0 (running credit total)
                    sta $0b	; /
                    lda $0f	; get $0f boolean (records-only/video pricing/credit select)
                    beq Lec2b	; branch down if 0 (for records-only mode)
				;
				; set pointers for video mode
				;
                    lda #$0a	;     		\
                    sta $11	; store $11 var	 |
                    lda #$0f	; 		 | LSB of pointers for video mode
                    sta $13	; store $13 var	/
                    bne Lec33	; (always branch)
				;
				; set pointers for records-only mode
				;
Lec2b               lda #$00	; 		\
                    sta $11	; store a$11 var |-LSB pointers for records-only mode
                    lda #$05	; 		 |
                    sta $13	; store $13 var /
				;
Lec33               lda #$03	; MSB of pointers
                    sta $12	; $11/$12 points to $0300 or $030a (base of pricing levels)
                    sta $14	; $13/$14 points to $0305 or $030f (base of credit levels)
				;
				; loop to calcualte credits paid for
				;
                    ldy #$04	; init loop counter to 4 (offset into tables) to start w/highest price levels
Lec3b               lda ($13),y	;   get credit level programmed
                    beq Lec56	;   if 0: this slot is unused; exit to continue the outer loop
                    lda ($11),y	;   get pricing level programmed
                    beq Lec56	;   if 0: this slot is unused; exit to continue the outer loop
Lec43               sec		;   found a valid price/credit level! see if there's enough money for it
                    lda $0a	;     fetch recently inserted money (in nickels)
                    sbc ($11),y	;     subtract this pricing level from money in
                    bcc Lec56	;     exit to continue outer loop if not enough $ (for this pricing level)
                    sta $0a	;       OK, have enough money for this level, store remainder back to $0a
                    clc		;       clear carry flag for addition
                    lda ($13),y	;       get credits earned (for this pricing level)
                    adc $0b	;       \ add these credits
                    sta $0b	;       / to a running new-credit total
                    jmp Lec43	;   loopback, to checking this SAME pricing/credit level again
				;   only get out by branching when insufficient balance to buy this level again
				;
				; bottom of loop
				;
Lec56               dey		; move down to the next pricing/credit level
                    cpy #$ff	; done yet?
                    bne Lec3b	; if not, loopback to check another pricing level
                    rts		; 
                    		;
				;*******************
				; DIVISION (2-BYTE)
				;*******************
				;   expects: $15/$16 (dividend), $17/$18 (divisor)
				;   out: $19, $1a (remainder), $15/$16 (quotient)
				;
				;  similar to example 6502 division code shown here:
				;    https://www.llx.com/Neil/a2/mult.html
				;
Sec5c               pha		; \
                    txa		;  |
                    pha		;  | put AX&Y registers on stack
                    tya		;  |
                    pha		; /
                    lda #$00	; init A to 0
                    sta $19	; init remainder to 0 (LSB)
                    sta $1a	; init remainder to 0 (MSB)
                    ldx #$10	; init counter X=$10 (for 16 bit calculation)
Lec69               asl $15	; shift $15 left
                    rol $16	;   rotate $16 left
                    rol $19	;   rotate $19 left
                    rol $1a	;   rotate $1a left
                    lda $19	;   get $19 to A
                    sec		;   set carry flag for subtraction
                    sbc $17	;   A=A-$17 var
                    tay		;   A->Y
                    lda $1a	;   get $1a var
                    sbc $18	;   A=A-$18 var
                    bcc Lec84	;   branch down if carry set
                    inc $15	;     increment $15 var
                    sta $1a	;     put A in $1a var
                    tya		;     Y->A
                    sta $19	;     put (former Y) in $19 var
Lec84               dex		;   decrement X counter
                    bne Lec69	; loopback until X=0
                    pla		; \
                    tay		;  |
                    pla		;  |
                    tax		;  |restore registers from stack
                    pla		; /
                    rts		; done
				;
				;******************
                    		; PROGRAMMING MODE
				;******************
				;  (initial entry point is actually $ec99 as if RESET was pressed)
				;  (note: no "background" tasks occur in programming mode)
				;
Lec8d               jsr Se3d8	; 5ms delay
                    jsr Sf21b	; check keypad
                    lda $36	; get keypress code
                    cmp #$0a	; was the RESET key pressed?
                    bne Lecae	; if NOT skip down to next check, if so...
				;
				; handle RESET keypress
				;
Sec99               jsr Sff31	; clear key buffer and top LED displays
                    lda #$0b	; code for a blank LED digit
                    sta $8c	; blank out an 100s digit of "SELECTIONS REMAINING" LEDs 
                    lda #$00	; 
                    sta $22	; init memory location selected
                    sta $23	; init keyed entry location to 0
                    dec $22	; decrement $22 to $ff (indicate none-selected?)
                    jsr Se453	; update LED display controllers
                    jmp Lec8d	; back to entry of programming mode
				;
				; check for POPULAR keypress
				;
Lecae               lda $36	; get keypress code
                    bmi Lec8d	; branch back to entry of programming mode if no keypress
                    cmp #$0b	; was it the POPULAR key?
                    bne Leccd	; if NOT branch down to handle numbers
				; 
				; handle a POPULAR keypress
				; 
Lecb6               inc $22	; increment current memory location
Lecb8               lda #$00	; \ MSB = 0 
                    sta $25	; / (no memory locations require a 2-byte number)
                    lda $22	; get currently-selected memory location
                    sta $24	; store value for parsing into digits
                    lda #$70	; \ set output offset
                    sta $26	; / numbers will be put in "SELECTIONS REMAINING" LED digits
                    jsr See45	; parse binary value into decimal digits
                    lda #$0b	; (code for a blank LED digit)
                    sta $8c	; blank the leftmost (100s) digit (all locations are 2-digit)
                    bne Lecde	; (always branch)
				; 
				; handle a numerical keypress
				; 
Leccd               ldx $23	; fetch current digit position/offset
                    lda $36	; get the keypress code
                    sta $8d,x	; store keypress to appropriate LED position
                    inc $23	; move to next position
                    jsr Se453	; update LED display controllers
                    lda #$02	; \	
                    cmp $23	; / are we not at position 2?
                    bne Lec8d	; if not, branch back to entry of programming mode
				;
				; handle when a 2-digit entry is complete
				; (or when location was advanced with POPULAR key)
				;
Lecde               lda #$00	; \ reset key entry position back to 0
                    sta $23	; /
                    lda $8d	; \ get 1st digit displayed
                    sta $1c	; / copy to $1c (10s digit)
                    lda $8e	; \ get (2nd digit entered)
                    sta $1d	; / copy to $1d (1s digit)
                    lda #$00	; \
                    sta $1b	; / value is 2-digit; 100s digit is 0
                    jsr See9e	; convert 3-digit value from decimal to binary ($2b/$2c returned)
                    lda $2c	; get LSB (MSB will be 0, as we only had 0-99)
                    sta $22	; store selected memory location
                    cmp #$63	; compare to 99 (exit service mode)
                    bne Led00	; if not, branch down to next check; if so...
				;
				; location 99: exit programming mode
				;
                    jsr Sff31	; clear key buffer and top LED digits
                    jsr Se453	; update LED displays
                    rts		; done here! (exits programming mode)
				;
Led00               cmp #$61	; check for location 97 (clear money/play counters)
                    bne Led0e	; if not, branch to next check...
				;
				; location 97: clear money/play counters
				;
                    ldx #$1a	; init a loop counter/offset
                    lda #$00	; will be filling with 0s
Led08               sta $02d7,x	; \  
                    dex		;  } clear out RAM $02d8-$02f1 (money/play counters)
                    bne Led08	; /
				;
Led0e               lda #$39	; \ compare current location to 57
                    cmp $22	; / 
                    bcc Led24	; if location > 57, branch down
				;
				; handle memory locations <= 57
				; (single-byte locations)
				;
                    ldx $22	; get entered memory location
                    txa		; X->A
                    sta $75	; copy location to $75 temp var
                    lda $0300,x	; fetch current value from RAM
                    sta $0a	; copy it to $0a temp var
                    lda #$00	; \ set $0b var to 0
                    sta $0b	; /
                    beq Led3b	; (unconditional branch down)
				;
				; handle memory locations > 57
				;   (these are each actually 2-byte values, so
				;    RAM addresses and "location" numbers diverge)
				;
Led24               lda $22	; get selected location number
                    sec		; set carry flag for subtraction
                    sbc #$39	; subtract 57 (locations over 57 have 2-byte values)
                    asl a	; multiply remainder by 2
                    clc		; \ add 56
                    adc #$38	; / "memory location" is now converted to actual address offset
                    sta $75	; store actual RAM offset to $75 temp var
                    tax		; copy A to X
                    lda $0300,x	; \ get the LSB of stored memory value
                    sta $0a	; / store LSB to local var $0a
                    inx		; move to next byte of stored value
                    lda $0300,x	; \ get MSB of stored memory value
                    sta $0b	; / store MSB to local var $0b
				;
				; (common path for all memory locations)
				;
Led3b               cpx #$0f	; \ compare locatoin to 15
                    bcs Led61	; / branch if >= 15 (video credits, and everything else)
                    cpx #$05	; \ compare X to 5
                    bcc Led47	; / branch if < 5 (record pricing has special code)
                    cpx #$0a	; \ compare X to 10
                    bcc Led61	; / branch if < 10 (record credit levels)
				; [9<X<15 falls thru: video pricing has special code] 
				;
				; handle locations 0-4 (record pricing)
				; and locations 10-14 (video pricing)
				;  (must convert stored value, in nickels, to 
				;   displayed the value in cents)
				;
Led47               lda $0a	; get currently stored value in this memory location
                    sta $2d	; store it as $2d (1st number to be multiplied)
                    lda $0319	; get coin multiplier from settings (mem loc 25; usually=5)
                    sta $2f	; store it as $2f (2nd number to be multiplied)
                    lda #$00	; \  
                    sta $2e	;  | MSB of both multiplicands is 0
                    sta $30	; /  (i.e. multiplying two 1-byte numbers)
                    jsr Seeec	; multiply $2d/$2e by $2f/$30
                    lda $31	; get LSB of result of multiplication
                    sta $0a	; store to $0a (now represents cents)
                    lda $32	; get MSB of result of multiplication
                    sta $0b	; store to $0b (MSB)
				;
				; (common path for all memory locations)
				;
Led61               lda $0b	; \
                    sta $25	;  \ copy stored value from memory location
                    lda $0a	;  / to for parsing into decimal digits
                    sta $24	; /
                    lda #$0c	; \ set offset such that
                    sta $26	; / results are placed in "SELECTION BEING MADE" LED positions
                    jsr See45	; parse 2-byte value into 4 digits ($27 never set?)
                    lda #$00	; \ 
                    sta $23	; / reset keyed entry position to 0
				;
				; supress any leading 0s from display
				;
                    lda $28	; get 1st digit of decimal value stored in current memory location
                    bne Led82	; skip ahead if it's not a zero
                    ldx #$0e	;   X=$0e (15)
                    stx $28	;   store $0e as 1st digit (suppress a leading 0 in 100s digit)
                    lda $29	;   get 2nd digit
                    bne Led82	;   branch ahead if it is not a zero
                    stx $29	;     store $0e as 2nd digit (suppress a leading 0 it 10s digit)
				;
Led82               jsr Se453	; update LED displays to show current stored memory value
				;
				; location contents are displayed - time to (possibly) edit it
				; [this section of code still needs more analysis and comments...]
				;
                    lda #$64	; \
                    sta $1f	; / init loop counter variable $1f to 100
Led89               jsr Se3d8	; 5ms delay
                    jsr Sf21b	; read keypad
                    lda #$75	; \ %0111 0101: select the RESET key
                    sta $4002	; / write to PIA2-B
                    lda $4000	; \ get PIA2-A
                    and #$02	; / %0000 0010: isolate bit 1 - keypad/button input (RESET selected)
                    bne Leda2	; if bit high (RESET key inactive) exit this loop
                    dec $1f	;   decrement debounce counter while RESET is pressed
                    bne Led89	;   loopback until counter is 0 or until RESET is released
                    jmp Sec99	;     if RESET held for a long press, back to start of programming mode
				;
Leda2               lda $1f	; get $1f counter (RESET debounce timer)
                    cmp #$64	; compare to 100
                    bne Leddb	; if counter < 100, branch down (RESET short-press?)
                    lda #$80	; \ %1000 0000
                    bit $36	; / bitwise AND with keypress code
                    bne Led89	; loopback if no keypress
				;
				; handle a non-RESET keypress (while editing value in prog mode)
				;
                    lda $36	; get keypress code
                    cmp #$0b	; \ was it the POPULAR key?
                    beq Ledd5	; / if so, branch ahead
                    lda $23	; get position of current key entry
                    cmp #$03	; \ is position = 3?
                    beq Leddb	; / if so, branch to jmp (to re-display current memory value)
                    cmp #$00	; \ "0" key pressed?
                    bne Ledc4	; / if not, skip ahead
				;
				; handle 0 key pressed (while editing value in prog mode)
				; [this code needs more attention]	
				;
                    lda #$0e	; replace "0" with a blank space character
                    sta $29	; put in middle LED digit?
                    sta $2a	; put in last LED digit?
Ledc4               lda $29	; get middle digit?
                    sta $28	; move to first digit?
                    lda $2a	; get last digit?
                    sta $29	; move to middle digit?
                    lda $36	; get ?? digit?
                    sta $2a	; move to last digit?
                    inc $23	; increment entry position
                    jmp Led82	; loopback to update the display
				;
				; handle POPULAR keypress (while editing value in prog mode)
				;
Ledd5               lda $23	; check position of current key entry
                    bne Ledde	; branch ahead if value partially entered
                    beq Lee42	; branch to loopback; nothing was changed
				;
				; handle RESET short-press
				;
Leddb               jmp Lecb8	; loop way back to re-select current memory location
				;
Ledde               lda #$0e	; charcode for a blank LED digit
                    cmp $29	; is the middle digit blank (i.e. 0)?
                    beq Ledeb	; if so, branch ahead
                    cmp $28	;   is first digit blank (i.e. 0)?
                    beq Ledef	;   if so, branch ahead
                    jmp Ledf3	;   no blank digits, jump ahead
				;
Ledeb               lda #$00	; \ 
                    sta $29	;  \ fill any blank digits
Ledef               lda #$00	;  / with numerical 0
                    sta $28	; /
				;
				;  an updated value has been entered
				;  time to do something with it
				;
Ledf3               jsr Sfb55	; convert 3-digit entry to binary (output to $2b/$2c)
                    ldx $22	; get memory location being edited
                    cpx #$0f	; compare to (15)
                    bcs Lee20	; if >= 15 branch to normal code
                    cpx #$05	; compare to 5
                    bcc Lee04	; if < 5 branch to special code
                    cpx #$0a	; compare to 10
                    bcc Lee20	; if < 10 branch to normal code
				; [if 10<X<15, fall thru to special code]
				;
				; special code for pricing values 
				; converts entered value (in cents) to stored value (in nickels)
				; [or other units, depending on multiplier]
				;
Lee04               lda $2c	; \
                    sta $15	; / LSB of dividend
                    lda $2b	; \
                    sta $16	; / MSB of dividend
                    lda $0319	; \ get coin multiplier value (typically 5)
                    sta $17	; / LSB of divisor
                    lda #$00	; \
                    sta $18	; / MSB of divisor = 0
                    jsr Sec5c	; division subroutine
                    lda $15	; get quotient LSB
                    sta $2c	; copy to $2c
                    lda $16	; get quotient MSB
                    sta $2b	; copy to $2b
				;
				; (back to common path)
 				; store newly entered value and update the RAM checksum 
				;
Lee20               lda $2b	; get MSB of new value
				;
				; validate value fits size of memory location
				;
                    beq Lee2c	; if 0 (single byte value entered), skip ahead
				;   2-byte value was entered
                    lda #$39	;   \ (57)
                    cmp $22	;   / compare memory location being edited to 57
                    bcc Lee2c	;   if 57 < mem location, skip next opcode
                    bcs Leddb	;     trying to enter 2-byte value into 1-byte location; reject it
				;
				; actually put the new value in RAM 
				;
Lee2c               ldx $75	; get actual RAM offset; correct for all locations
                    lda $2c	; get (LSB of) new value
                    sta $0300,x	; store in programmable RAM
                    lda #$39	; \ memory location being edited to 57
                    cmp $22	; / 
                    bcs Lee3f	; if 57 >= memory location, branch ahead 
				;
				; for 2-byte locations (57 & up) store the other byte too
				;
                    inx		; increment X to next location in RAM
                    lda $2b	; get MSB of new value
                    sta $0300,x	; store MSB in programmable RAM
				;
				; (common path) finish up
				;
Lee3f               jsr Sef18	; update the RAM checksum
Lee42               jmp Lecb6	; loop back (move to the next memory location)
            			;
				;***********************
				; PARSE 2-BYTE (0-9999)
				; VALUE INTO 4 DIGITS
				;***********************
				;  takes a 2-byte binary value; returns 4 single decimal digits
				;  can also take a 1-byte value; return 3 single decimal digits
				;    in: $24/$25 (2-byte number; 0-9999 in decimal)
				;    in: $26 as offset past $1b
				;    out: 4 single-digit values, starting at $1b + offset
				;  note: this section of code uses decimal mode to perform BCD math
				;
See45               pha		; \
                    php		;  |
                    txa		;  | put all registers, including flags
                    pha		;  | onto the stack
                    tya		;  |
                    pha		; /
                    ldy $26	; get offset variable to Y
                    sed		; set decimal mode (affects only ACD and SBC opcodes)
                    lda #$00	; A=0
                    clc		; clear carry flag
                    sta $06	; \ init $06
                    sta $07	; / and $07 to 0
                    ldx #$10	; init loop counter for 16-bit (multiplication?) routine
Lee57               lda $24	;   get $24 variable
                    rol a	;   rotate it left [with wraparound, thru carry bit]
                    sta $24	;   store it back
                    lda $25	;   get $25 variable
                    rol a	;   rotate it left
                    sta $25	;   store it back
                    lda $06	;   get $06 variable
                    adc $06	;   double it (in BCD mode*)+carry flag from prev
                    sta $06	;   store it back
                    lda $07	;   get $07 variable
                    adc $07	;   double it (in BCD mode*)+carry flag from prev
                    sta $07	;   store it back
                    dex		;   decrement counter
                    bne Lee57	; loopback until 0
                    cld		; clear decimal mode (back to "normal" ACD/SBC)
                    lda $06	; get $06 variable
                    and #$0f	; isolate lower nibble (ones BCD digit)
                    sta $001e,y	; store it at $001e offset by Y
                    lda $06	; get $06 variable again
                    lsr a	;   rotate right 
                    lsr a	;   four times
                    lsr a	;   isolates the upper nibble
                    lsr a	;   (tens BCD digit)
                    sta $001d,y	; store it at $001d offset by Y
                    lda $07	; get $07 variable
                    and #$0f	; isolate lower nibble
                    sta $001c,y	; store at $001c offset by Y
                    lda $07	; get $07 variable again
                    lsr a	;  
                    lsr a	;  isolate the
                    lsr a	;  upper nibble
                    lsr a	;
                    sta $001b,y	; store at $001b offset by Y
                    lda #$00	; \
                    sta $25	;   clear out variables $25 and $26
                    sta $26	; /
                    pla		; \
                    tay		;  |
                    pla		;  | restore all registers
                    tax		;  | including flags
                    plp		;  |
                    pla		; /
                    rts		; done here
				;
				;****************************************
     				; CONVERT 3-DIGIT (1 digit per byte)
				; NUMBER FROM DECIMAL TO BINARY (2-byte)
				;****************************************
				;   expects 3-digit table: $1b/$1c/$1d
				;   returns 2-byte binary value: $2b/$2c
				;   returned values are MSB=$2b / LSB=$2c
				;
See9e               pha		; \ 
                    txa		;  | A, X and flags to the stack
                    pha		;  | (Y register is not used)
                    php		; /
                    lda $1b	; \  
                    pha		;  | 
                    lda $1c	;  | put the 3-digit entry
                    pha		;  | onto the stack, too
                    lda $1d	;  | 
                    pha		; / 
                    ldx $1b	; get 1st digit (100s) to X
                    lda #$00	; \
                    sta $2b	;  | init $2b/$2c to 0
                    sta $2c	; /
Leeb3               lda $1b	; get 1st digit to A
                    beq Leec7	; branch down if 1st digit is 0 
                    lda #$64	; (100)
                    clc		; clear carry flag for addition
                    adc $2c	; add to 100 to LSB
                    sta $2c	; store updated LSB back
                    lda #$00	; reset A
                    adc $2b	; add to MSB (in case carry flag got set)
                    sta $2b	; store back
                    dex		; decrement 1st digit counter
                    bne Leeb3	; loopback until 100s digit is counted down to 0
Leec7               lda $1c	; get 2nd digit (10s) to A (0 to 9)
                    asl a	; \
                    asl a	;  } shift left 3 times to multiply by 8
                    asl a	; /
                    adc $1c	; add to self (now multiplied by 9)
                    adc $1c	; add to self (now multiplied by 10, and is 0-90)
                    clc		; clear carry flag for addition
                    adc $1d	; add on 3rd digit (1s) to total (now 0-99)
                    clc		; clear carry flag again
                    adc $2c	; add to LSB
                    sta $2c	; store LSB back
                    lda #$00	; reset A
                    adc $2b	; add (carry flag) to MSB
                    sta $2b	; store MSB back
                    pla		; \
                    sta $1d	;  |
                    pla		;  | 
                    sta $1c	;  | restore $1b-$1d to prior values	
                    pla		;  |
                    sta $1b	; /
                    plp		; \
                    pla		;  | restore flags, X and A registers
                    tax		;  |
                    pla		; /
                    rts		; 
                    		;
				;*************************
				; MULTIPLICATION FUNCTION
				;*************************
				;  multiplies 2-byte values
				;  returns a 4-byte result
				;    in:  $2d/$2e & $2f/$30
				;    out: $31/$32 & $33/$34
				;    (only used as credit multiplier
				;     in programming mode)
				;
				;  similar to example 6502 2-byte mult code shown here:
				;    https://www.llx.com/Neil/a2/mult.html
				;
Seeec               pha		; \ 
                    txa		;  } put A & X on stack (Y not used in this subroutine)
                    pha		; /
                    lda #$00	; \
                    sta $33	;  } init $33/$34 (high 2 bytes of result)
                    sta $34	; /
                    ldx #$10	; init X loop counter for 16-bit multiplication
Leef7               lsr $30	;   shift $30 var right 
                    ror $2f	;   rotate $2f var right
                    bcc Lef08	;   if carry clear (bit 0 of $2f was low) branch down
                    clc		;     clear carry flag
                    lda $33	;     get $33 to A
                    adc $2d	;     add $2d to A
                    sta $33	;     store it back to $33
                    lda $34	;     get $34 to A
                    adc $2e	;     add var $2e to A
Lef08               lsr a	;   divide by 2
                    sta $34	;   store $34 back
                    ror $33	;   rotate $33 right
                    ror $32	;   rotate $32 right
                    ror $31	;   rotate $31 right
                    dex		;   decrement loop counter
                    bne Leef7	; loopback until X=0
                    pla		; \
                    tax		;  } restore X and A from stack
                    pla		; /
                    rts		; 
				;
				;******************************************
				; PROGRAMMABLE-RAM CHECKSUM (SET & VERIFY)
				;******************************************
				;   only RAM locations $0300-$038c (operator settings)
				;   checksum value   stored at $038f
				;   inverse checksum stored at $038e
				;
				; entry point for setting a new checksum
				;
Sef18               lda #$00	; \
                    sta $06	; / clear boolean flag (for setting the checksum)
                    beq Lef22	; (unconditional branch down past alternate entry)
				;
				; entry point for verifying the current checksum
				;
Sef1e               lda #$ff	; \
                    sta $06	; / set boolean flag (for verifying the checksum)
				;
				; (common path)
				;
Lef22               ldx #$00	; init loop counter
                    lda #$00	; init the checksum
Lef26               clc		;   clear carry flag every time (ignore overflows)
                    adc $0300,x	;   add byte to running checksum in A
                    inx		;   move to next byte
                    cpx #$8c	;   continue up to $038c
                    bne Lef26	; loopback
                    sta $07	; store the computed 1-byte checksum as temp variable
                    lda $06	; check boolean flag (setting or verifying checksum)
                    beq Lef42	; if setting (0) branch down to store the updated checksum
				;
				; verifying the checksum
				;
                    lda $07	; retrieve checksum from temp var
                    cmp $038f	; compare to checksum already in RAM
                    bne Lef41	; if not equal, return from subroutine, with z flag clear as status
                    eor #$ff	; flip all bits of A
                    cmp $038e	; compare to previously-stored inverse checksum (z flag set if they match)
Lef41               rts		; done; returns z flag as result sucess/failure indicator
				;
				; setting the checksum
				;
Lef42               lda $07	; retrieve checksum from temp var
                    sta $038f	; store the computed checksum
                    eor #$ff	; flip all bits of the checksum
                    sta	$038e	; store inverse checksum
                    rts		; 
				;
				;**************************
				; DELAY LOOP (BASE) - ~1ms
				;**************************
				;
Sef4d               pha		; \			3 cycles \
                    tya		;  |			2 cycles  \ 
                    pha		;  | AY&X to stack	3 cycles   \_ 15 cycles
                    txa		;  |			2 cycles   /
                    pha		; /			3 cycles  /
                    ldx #$af	; init X to 175 loops	2 cycles /
Lef54               dex		; 			2 cycles \_5*175=875 cycles
                    bne Lef54	; loopback until X=0	3 cycles /
                    pla		; \			4 cycles \
                    tax		;  |			2 cycles  \
                    pla		;  | restore XY&A	4 cycles   \_ 22 cycles
                    tay		;  |			2 cycles   /
                    pla		; /			4 cycles  /
                    rts		; done			6 cycles /   ~912 cycles total
                    		;
				;*****************************
				; handle input in normal mode
				;*****************************
				;
Sef5d               lda $35	; get service mode flag
                    beq Lef62	; if 0 (regular mode) skip next opcode
                    rts		;   done here! (we're in service mode)
Lef62               jsr Sf21b	; read keypad
                    lda $36	; get keycode
                    bpl Lef6a	; skip next opcode if any key pressed
                    rts		;   if no keypress, done here!
Lef6a               cmp #$0a	; was it the RESET key?
                    bne Lef80	; if not, skip down to the the next bit...
				;
				; handle a RESET keypress
				;
Lef6e               lda #$00	; zero out...
                    sta $23	;   key entry position
                    sta $37	;   (something credit related?)
                    sta $3b	;   (records mode?)
                    jsr Sff31	; clear top LED displays
                    jsr Se59c	; update credit display
Lef7c               rts		; done
				;
Lef7d               jmp Lf07a	; (only reached from below) jump ahead
				;
				; handle non-RESET key press
				;
Lef80               lda $38	; get $38 var (status of THANK YOU / MAKE SELECTION lights?)
                    and #$01	; isolate bit 0
                    bne Lef87	; skip next opcode if bit 0 set (i.e. MAKE SELECTION is lit?)
                    rts		;   done here! (MAKE SELECTION is NOT lit, no key entry allowed!
Lef87               lda $36	; get keycode of key just pressed
                    cmp #$0b	; was it the POPULAR key? 
                    bne Lef7d	; no? branch back to jmp way down, to handle a number keypress
				;
				; handle a POPULAR keypress
				;
Lef8d               lda #$00	; \ 
                    sta $3b	;  > clear $3b(records mode?)...
                    sta $06	; /  ...and $06 (LSB of pointer)
                    tay		; Y=0 too
                    lda #$05	; \
                    sta $07	; / set MSB of pointer, $06/$07 -> $0500
                    lda #$01	; A=1
                    sta $0b	; put 1 in $0b var(?)
                    lda $39	; check video/record mode boolean?
                    beq Lefbc	; if 0 branch down
                    lda $38	;   get $38 var (status of THANK YOU / MAKE SELECTION lights?)
                    and #$08	;   %0000 1000 isolate bit 3: (MAKE SELECTION indicator bit?)
                    beq Lefbc	;   if bit 3 = 0 branch down
                    lda $fb	;     get $fb var(?)
                    cmp #$02	;     comare to 2
                    beq Lefbc	;     if 2 branch down
                    lda #$ff	;       A=$ff
                    sta $3b	;       set $3b var to $ff (video mode?)
                    lda #$00	;       \
                    sta $06	;	/ set LSB of pointer 
                    lda #$07	;	\
                    sta $07	;	/ set MSB of pointer ($06/$07 point to $0700)
                    lda #$02	;       A=2
                    sta $0b	;       set $0b var to 2
Lefbc               lda $0b	; get $0b var to A
                    sta $08	;   copy it to $08
                    lda ($06),y	;   get value from table (either $0700 or $0500)
                    cmp #$c8	;   compare it to $c8 (200)
                    bcc Lefc8	;   branch over next opcode if A < 200
                    lda #$00	;     A=0
Lefc8               sta $0a	;   put A in $0a var
                    iny		;   next Y
                    beq Lef7c	;   if Y=0 branch to RTS
Lefcd               cmp #$32	;   compare A to $32 (50)
                    bcc Lefdb	;   branch down if A < 50 (exit loops)
                    sbc #$32	;     A=A-50
                    asl $08	;     \ shift $08 var left
                    asl $08	;     / shift $08 var left
                    bcs Lefbc	;   loopback if carry is set
                    bcc Lefcd	; loopback differently otherwise
Lefdb               tax		; A->X
                    lda $08	; get $08 var to A (bitmask)
                    and $0100,x	; isolate bit from table value
                    bne Lefbc	; 
                    lda $08	; 
                    ora $0100,x	; 
                    sta $0100,x	; 
                    lda $0a	; get $0a var to A
                    pha		; push A to stack
                    lda $87	; 
                    bne Lf008	; 
                    ldx #$ee	; init X=$ee (location on page 2 for records counter)
                    lda $3b	; get variable $3b (video/record?)
                    beq Leffa	; if variable $3b=0 skip down (records-only)
                    ldx #$e8	;   X=$e8 (location on page 2 for video counter)
Leffa               txa		; X->A
                    pha		; push A (X)
                    dex		; X=X-1
                    dex		; X=X-1
                    jsr Sf18d	; increments 2-byte counter: records/videos w/POPULAR button
                    pla		; pull A back
                    tax		; A->X
                    jsr Sf18d	; increments 2-byte counter: records/videos total selections
                    inc $88	; increment $88 var(?)
Lf008               pla		; pull A from stack (ref $efcd)
                    tax		; A->X ($0a var)
                    lda #$00	; 
                    sta $87	;
                    lda $3b	; check $3b var
                    beq Lf01a	; branch ahead of $3b var = 0
                    cpx #$40	;   is X = $40 (64)?
                    bcc Lf01a	;   if __ branch ahead
                    txa		;     copy X to A
                    adc #$23	; A=A+$23 (35)?
                    tax		; A->X (<- here's where X is set)
Lf01a               txa		; X->A copy X (selection) to A
                    sta $24	; number to convert to decimal digits
                    lda #$0c	; \
                    sta $26	; / set offset; output to $27-$2a ("SELECTION BEING MADE" LEDs)
                    jsr See45	; parse 2-byte value into 4 decimal digits
                    inc $28	; increment 100s digit of result (0XX -> 1XX and 1XX -> 2XX)
                    lda $3b	; get $3b var - record/video mode?
                    beq Lf068	; if record-mode, branch down
				;
				; handle POPULAR keypress in video mode(?)
				;
                    inc $28	; \ increment 100s digit
                    inc $28	; / twice
                    txa		; X->A (what was X?)
                    cmp #$64	; compare A to $64 (100)
                    bcc Lf035	; branch over next opcode if A < 100
                    sbc #$64	;   A=A-100
Lf035               cmp #$0a	; compare A to $0a (10)
                    beq Lf073	; branch way down if A=10
                    cmp #$0b	; compare A to $0b (11)
                    beq Lf073	; branch way down if A=11
                    cmp #$1e	; compare A to $1e (30)
                    beq Lf073	; branch way down if A=30
                    cmp #$1f	; compare A to $1f (31)
                    beq Lf073	; branch way down if A=31
				;
Lf045               jsr Sfb55	; convert 3-digit entry to binary ($28-$2a -> $2b/$2c)
				;
				; check if (video) selection is on the "lockout" list
				;
                    ldy #$07	; init loop counter/offset Y=7 (4 x 2-byte entries to check)
Lf04a               lda $0366,y	; get memory locations 80-83 (video selection lockouts)
                    dey		;   decrement loop counter/offset
                    cmp $2b	;   compare A to $2b var (LSB of converted 3-digit number)
                    bne Lf05c	;   if no match, branch down
                    lda $0366,y	;     if matched, get the MSB of selection number
                    cmp $2c	;     compare to MSB of selection
                    bne Lf05c	;     skip next opcode if no match
                    jmp Lef6e	;       selection on lockout list! go simulate a RESET keypress
Lf05c               dey		;   next entry on lockout list
                    bpl Lf04a	; loopback until all 4 entries checked
				;
                    jsr Sf1b2	; add X to selection list
                    jsr Sf95a	; [do some background tasks]
                    jmp Lf24c	; jump to credit-related(?) subroutine...
				;
				; (records-mode path)
				;
Lf068               lda $2a	; get the 1s digit
                    cmp #$08	; compare it to 8
                    bcc Lf045	; if < 8, branch back to normal flow
                    lda $031d	; check setting for (dis)allow XX8/XX9 selections
                    beq Lf045	; if "enable all", branch back to normal flow
Lf073               lda #$ff	;   \ 
                    sta $87	;   / put $ff in $87 var
                    jmp Lef8d	;   jump to POPULAR keypress code?
				; 
				; handle a number key entry (in regular mode)
				; 
Lf07a               lda $23	; get number of digits already entered
                    beq Lf0c5	; if 0 then this is the 1st digit, branch ahead to handle 1st digit
                    lda $28	; at least 1 digit already in, get what the already-entered 1st digit was
                    cmp #$03	; compare already-enetered 1st digit to 3
                    bcc Lf091	; if 1st digit was < 3, branch forward to jmp then down to handle 2nd/3rd digits
				; to get here: at least 1 digit in; 1st digit was >= 3 (!)
                    lda $23	; get the number of digits already entered (again)
                    cmp #$01	; compare # of digits already entered to 1
                    bne Lf09c	; if not 1, branch down (must be 2 already entered, go handle last digit)
				; to get here: 1 digit in; 1st digit was >= 3 (!)
                    lda $3f	; get $3f var (???)
                    cmp $36	; compare to keypress code
                    bcs Lf094	; branch ahead if $3f var >= keypress code
                    rts		; otherwise, ignore keypress(?) and return
				;
Lf091               jmp Lf101	; jump down to handle 2nd digit entry for 1st digit of 1-2
				;
				; handle 2nd digit for 1st digit >=3 (only 4 allowed)
				;
Lf094               beq Lf0c2	; if (?) branch down (to jmp)
                    lda #$09	; \ set $40 var to 9
                    sta $40	; /
                    bne Lf0c2	; unconditional branch down (to jmp)
				;
				; handle 3rd (last) digit of 3XX & 4XX entries(?)
				;
Lf09c               lda $29	; get 10s digit of entry
                    asl a	; \
                    asl a	;  |
                    asl a	;  | shift 10s digit into upper nibble
                    asl a	; /
                    clc		; clear carry for addition
                    adc $36	; add new keypress code (now a 2-digit BCD value of 2nd & 3rd digits)
                    cmp #$10	; compare to $10
                    bne Lf0aa	; skip next opcode if not $10
                    rts    	;   done here (310/410 not allowed?)
Lf0aa               cmp #$11	; compare to $11
                    bne Lf0af	; skip next opcode if not $11 
                    rts		;   done here (311/411 not allowed?)
Lf0af               cmp #$30	; compare to $30
                    bne Lf0b4	; skip next opcode if not $30
                    rts   	;   done here (330/430 not allowed?)
Lf0b4               cmp #$31	; compare to $31
                    bne Lf0b9	; skip next opcode if not $31
                    rts		;   done here (331/431 not allowed?)
Lf0b9               lda $40	; get $40 var (maximum allowed video selection?)
                    cmp $36	; compare to current digit entered
                    bcs Lf0c2	; branch if $40 var >= $36 var(?)
                    beq Lf0c2	; branch if = (should never be reached?)
Lf0c1               rts		; otherwise ignore keypress
				; 
Lf0c2               jmp Lf13e	; (long jump fron branch above) 
				; 
				; handle the 1st digit of a new entry
				; 
Lf0c5               lda $36	; get the keycode
                    cmp #$03	; \
                    beq Lf0d2	;  | if it was a 3 or 4
                    cmp #$04	;  | branch down to handle
                    beq Lf0d2	; /
                    jmp Lf12f	; otherwise, jump down to check more...
				; 
				; handle a 1st digit of 3 or 4
				; 
Lf0d2               lda $39	; video mode boolean(?)
                    beq Lf0c1	; if 0, branch to nearby rts (to ignore keypress)
                    lda $38	; get $38 var (status of indicator lamps)
                    and #$08	; %0000 1000 isolate bit 3: (MAKE A SELECTION status?)
                    beq Lf0c1	; if MAKE A SELECTION is unlit, branch to nearby (to ignore keypress)
                    lda $fb	; get $fb var (?)
                    cmp #$02	; compare to 2
                    beq Lf0c1	; if $fb = 2, branch to nearby rts (to ignore keypress)
                    lda $36	; get the keypress code
                    cmp #$04	; keypress was a 4?
                    bne Lf0ee	; if not, branch ahead (must have been 3)
				; 
				; 1st keypress was a 4:
				; 
                    lda $fe	; check $fe var(?)
                    beq Lf0c1	; is $fe = 0, branch to nearby rts (to ignore keypress)
                    bne Lf0f2	; (unconditional branch ahead)
				;
				; 1st keypress was a 3:
				;
Lf0ee               lda $fc	; check $fc var (video autoplay mode?)
                    beq Lf0c1	; if $fc = 0, branch to nearby rts (to ignore keypress)
				;
				; allowable 1st keypress of 3 or 4? (lots of hoops to jump thru to get here)
				;
Lf0f2               sta $24	; copy $fc or $fe var value to $24 var (value for parsing to digits)
                    lda #$22	; \
                    sta $26	; / set offset to $22 (output to $3d-$40?)
                    jsr See45	; parse 2-byte value into 4 decimal digits
                    lda #$ff	; \ set video mode flag $3b?
                    sta $3b	; / 
                    bne Lf0c2	; unconditional branch (to jmp)
				;
				; handle 2nd/3rd digits when 1st digit was 1 or 2 
				;
Lf101               lda $23	; get digit position
                    cmp #$01	; is it 1?
                    beq Lf113	; branch down if 1
                    lda $031d	;   get setting location 29 (enable/disable XX8/XX9 selections)
                    beq Lf13e	;   if setting = 0 (enable all) branch down
                    lda $36	;   get keypress code
                    cmp #$08	;   was it 8?
                    bcc Lf13e	;   branch if < 8
                    rts		;     otherwise return (ignore 8 or 9 entry when not allowed)
Lf113               ldx $36	; get keypress code
                    lda #$ff	; preload A=$ff
                    cpx #$06	; was 6 pressed?
                    bcc Lf13e	; branch if keypress was < 6
                    cmp $0328,x ;   compare $ff to $0328+$06, +$07... (memory location 46, 47...)
                    bne Lf13e	;   if no match, branch to normal code
                    lda $41	;   get var $41 (copy of number of credits?)
                    cmp $032d	;   compare to memory location 45 (undocumented?)
                    bcs Lf128	;   branch over next opcode if ___
                    rts		;   return, ignoring keypress
				;
Lf128               lda #$ff	; \
                    sta $37	; / set $37 var to $ff
                    jmp Lf13e	; jump to update entry table
				;
				; handle a 1st digit that's NOT 3 or 4
				;
Lf12f               lda $36	; get key just pressed
                    cmp #$01	; \ 
                    beq Lf13a	;  | if it was a 1 or 2
                    cmp #$02	;  | branch over rts
                    beq Lf13a	; /  
                    rts		; otherwise rts (to ignore all 1st digits not 1-4)
				;
				; handle 1st digits of 1 or 2
				;
Lf13a               ldx #$00	; \
                    stx $3b	; / set $3b var to 0 (record mode, not video?)
				;
Lf13e               jsr Sf89c	; update the multi-digit entry table
                    lda $23	; get # of digits-entered 
                    cmp #$03	; do we have all 3 digits?
                    beq Lf148	; if so, branch over the next opcode...
                    rts		; otherwise done here (wait for another keypress)
				;
				; 3 digits have been entered ($28/$29/$2a)
				;
Lf148               lda #$00	; \
                    sta $23	; / reset digit position to 0
                    lda $28	; get 1st digit entered to A (1 or 2)
                    ldx #$00	; init X=0
                    lsr a	; shift bits of A right (bit 0 to carry) [checks odd or even; 1 or 2]
                    bcs Lf154	; if odd (carry set) skip next opcode (increment)
                    inx		;   increment X to 1, if 1XX was entered (for 2XX, X remains 0)
Lf154               stx $28	; store X (even/odd flag, 0/1) at $28 (entry now 000-199 vice 100-299)
                    jsr Sfb55	; convert 3-digit entry to 2-byte binary (returns $2b/$2c)
                    ldx #$ee	; point to 'total record selections' counter by default
                    lda $3b	; check variable $3b (video/records mode)
                    beq Lf161	; if 0 (records) skip over next opcode
                    ldx #$e8	;   point to 'total video' counter instead
Lf161               jsr Sf18d	; increment total records/videos counter (0-9999)
                    jsr Sf8b1	; update MEMOREC plays tables
                    lda #$02	; %0000 0010
                    and $3b	; isolate bit 1 of $3b: (video/records mode) (why?)
                    bne Lf16f	; if bit set, skip next opcode (leaving A=2)
                    lda #$01	;   A=1
Lf16f               sta $08	; put A (1 or 2) at $08 temp var
                    lda $2c	; get (LSB of) converted selection number (0-199)
Lf173               cmp #$32	; compare selection to $32 (50)
                    bcc Lf17f	;   exit loop (or never enter) if selection < 50
                    sbc #$32	;   reduce A by 50
                    asl $08	;   \ 
                    asl $08	;   / shift $08 var left twice (%0000 0010 -> %0000 1000)
                    bcc Lf173	; loopback until carry gets set (a bit of $08 comes out)
Lf17f               tax		; A-> X (modified/reduced selection number?)
                    lda $08	; get $08 var to A (bitmask?)
                    ora $0100,x	; get a table value, OR'd with A (possibly sets a bit) 
                    sta $0100,x	; store updated value back in table
                    ldx $2c	; get $2c; LSB of converted selection (unmodified) to X
                    jmp Lf01a	; (jump back a ways?)
				;
				;**********************************
				; INCREMENT COUNTER (e.g. MEMOREC) 
				;**********************************   
				;   uses 2-byte value, at X & X+1, into page 2 of RAM
				;   increments counter up to $270f (9999) maximum
				;
Sf18d               lda $0200,x	; get current LSB value, offset by X into page 2 RAM
				;	
				; check for max-out condition
				;
                    cmp #$0f	; compare to $0f (15)
                    bne Lf19d	; branch down if NOT equal to $0f
                    inx		;   point X to MSB value
                    lda $0200,x	;   get current MSB value from RAM table
                    cmp #$27	;   compare to $27 ($270f=9999) 
                    beq Lf1a8	;   MSB is $47, branch to max-out
                    dex		;   if not, decrement X back to LSB
				;
				; increment the 2-byte counter
				;
Lf19d               inc $0200,x	; increment LSB of counter 
                    bne Lf1a7	; branch down unless it rolled over the LSB
                    inx		;   \ if LSB rolled to 0, go back
                    inc $0200,x	;   / and increment the MSB
                    dex		;   return X to its original value
Lf1a7               rts		; done
				;
				; handle counter max-out
				; 
Lf1a8               lda #$00	; in the event we went reached count of $270f (9999), reset to 0
                    sta $0200,x	; \  reset both bytes
                    dex		;  } of counter to zero and 
                    sta $0200,x	; /  leave X at original value
                    rts		; 
                    		;
				;***********************
				; ADD SELECTION TO LIST
				;***********************
				;  new selection in X register
				;
Sf1b2               ldy $3b	; check video/record mode flag
                    beq Lf1ca	; if in record mode, branch down
				;
				; handle video playlist(?)
				;
Sf1b6               txa		; X->A [called w/X=$40 & X=$a4]
                    sta $ca	; store as $ca var(?)
                    ldy $f8	; get the video selection pointer(?)
                    sta $0300,y	; store selection (or code?) in video selection table 
                    iny		; increment video selection pointer
                    bne Lf1c3	; if not 0 skip next opcode
                    ldy #$90	;   Y=$90 (wrap around?)
Lf1c3               cpy $f7	; compare new Y to $f7 var(?)
                    beq Lf1c9	; if Y == $f7 var, skip next opcode
                    sty $f8	; update the video selection pointer
Lf1c9               rts		; done
				;
				; handle record playlist(?)
				;
Lf1ca               stx $ca	; store X (new selection) in $ca var
                    ldy #$ff	; \
                    sty $3c	;  > set 2 boolean flag variables(?)
                    sty $70	; / (playing flag?)
                    lda $0339	; get the FIFO/conventional setting
                    beq Lf1ff	; if conventional branch down
				;
				;   FIFO selection mode
				;
                    ldy #$c7	; init Y to 199, the last byte of the selection table
                    txa		; move new selection to A
Lf1da               cmp $0200,y	; compare new selection to Yth table value
                    beq Lf1c9	;   if a match branch to rts (new selection already in playlist; ignore it)
                    dey		;   decrement counter
                    bne Lf1da	; loopback until done checking entire table
                    cmp $0200	; finally, check $0200 (the 0 offset) [poorly designed loop?]
                    beq Lf1c9	; if a match, branch to rts (new selection already in playlist; ignore it)
                    ldy $e5	; get current position in selection table
                    sta $0200,y	; put new selection into the table at position Y
                    iny		; increment position in selection table
                    cpy #$c8	; compare new position in selection table to 200
                    bne Lf1f3	; skip next opcode if Y<>200
                    ldy #$00	;   wraparound back to 0 when Y=200
Lf1f3               cpy $ee	; compare Y to 238                \ unsure why/how would this happen?
                    beq Lf1c9	; if Y=238 branch to a nearby rts /
                    sty $e5	; update position in selection table
                    lda #$61	; \ 
                    jsr Sfcff	; / put #$61 int event table (selection made?)
                    rts		; 
				;
				;   conventional selection mode (not FIFO)
				;
Lf1ff               lda #$00	; \ flag Xth entry in selection table
                    sta $0200,x	; / (dead-simple; doesn't matter if it was already flagged)
                    rts		; done (note no event code entered into table)
                    		;
				;*****************************
				; READ THE KEYPAD (low level)
				;*****************************
				;   returns keycode at $36 & $9c, keycodes:
				;      $80 = no key pressed
				;      0-9  = # key pressed
				;      $0a = RESET key pressed
				;      $0b = POPULAR key pressed
				;   updates POPULAR key flag at $66
				;   uses debounce counter at $47
				;
				;   alternate entry at $f21b omits POPULAR+# capability
				;
Sf205               lda $ff97	; bitmask from ROM table to select "POPULAR" key input
                    sta $4002	; write to PIA2-B
                    lda #$02	; %0000 0010 bit-1 mask for reading PIA
                    ldx #$ff	; init a boolean flag in X
                    and $4000	; read keypad "POPULAR" key
                    beq Lf215	; if 0 (active-low, POPULAR key is pressed) skip next opcode
                    inx		;   increment X to toggle it from $ff to 0
Lf215               stx $66	; store POPULAR key status boolean
                    ldx #$0a	; X = 10 keys remaining to read (don't re-check the POPULAR key)
                    bne Lf21d	; always skip next opcode instruction
				;
Sf21b               ldx #$0b	; init counter to 11 (read 12 keys: 0-9, RESET, POPULAR) [alt entry pt]
Lf21d               lda #$80	; A = %1000 0000
                    sta $36	; store $80 to $36 (default if no key is pressed)
Lf221               lda $ff8c,x	; get Xth value from ROM table (select keypad selects/return)
                    sta $4002	;   write to PIA2-B
                    nop		;   micro-pause
                    lda $4000	;   read PIA2-A
                    ora $4000	;   ...again?
                    ora $4000	;   ...again?
                    and #$02	;   %0000 0010 isolate bit 1: keypad/button inputs
                    beq Lf23f	;   if we have a hit (selected key is pressed) exit loop
                    dex		;   if no hit, decrement counter...
                    bpl Lf221	; loopback until we underrun (finished 0 then rolled X)
                    lda $47	; get debounce counter(?)
                    beq Lf23e	; if 0, branch down to RTS
                    dec $47	;   otherwise decrement debounce counter(?)
Lf23e               rts		; no key pressed, done here!
Lf23f               lda $47	; get debounce counter(?)
                    bne Lf247	; if non-zero branch down
                    stx $36	;   otherwise, store X (key pressed) at $36
                    stx $9c	;   ...and at $9c
Lf247               lda #$0a	; init debounce counter(?) to 10?
                    sta $47	; store debounce counter(?)
                    rts		; done here!
                    		;
				;*****************
				; credit related?
				;*****************
				;
Lf24c               lda $02fb	; get $02fb counter
                    bne Sf256	; if not zero, branch ahead
                    lda #$01	;   \ set $02fb counter to 1
                    sta $02fb	;   /
				;
				; process new regular (records) credits
				;
Sf256               lda $02f5	; get number of credits [alt. entry point]
                    clc		; clear carry
                    adc $02f4	; add credits just recieved for money (records)
                    bcc Lf261	; skip next opcode if that doesn't roll over
                    lda #$ff	;   if it did roll over, cap at 255
Lf261               sta $02f5	; store updated credit total
				;
				; process new video credits
				;
                    lda $02f6	; get number of credits just recieved for money (video)
                    clc		; clear carry flag for addition
                    adc $02f7	; add new video credits?
                    bcc Lf26f	; skip next opcode if that doesn't roll over
                    lda #$ff	;   if it did roll over, cap at 255
Lf26f               sta $02f7	; store updated video credits value back
				;
                    lda #$00	; \ 
                    sta $02f6	;  |0 out credits recieved [they've now been added to running total]
                    sta $02f4	; / both for video and for records
                    lda $97	; get $97 var(?)
                    beq Lf27f	; if $97=0 skip next opcode
                    rts		;   done here!
Lf27f               lda $3b	; get records/video mode (record mode=0)
                    bne Lf298	; branch down for video mode
                    lda $37	;   get $37 var(?)
                    beq Lf29b	;   (exit loop if 0)
                    lda $02f5	;   get credit total
                    sec		;   set carry flag (ready for subtraction)
                    sbc $032d	;   subtract $032d var (mem loc 45?) from credit total
                    bcs Lf292	;   if that doesn't go below 0 branch down
                    lda #$00	;   if negative, A=0
Lf292               sta $02f5	;   store updated credit total
                    jmp Lf2a3	;   jump down
				;
Lf298               jmp Lf316	; jump down a bit
				;
Lf29b               lda $02f5	; get credit total
                    beq Lf2a3	; if it's 0 branch down
                    dec $02f5	; if non-zero, decrement credits by 1
Lf2a3               lda $031e	; check for records/video mode
                    eor #$ff	; flip all bits ($00 -> $ff, $ff -> $00)
                    beq Lf2c2	; branch down if 0 now (i.e. was $ff for records-only)
                    lda $02f5	;   get credit total (records)
                    sta $16	;   copy it to $16 var
                    lda #$00	;   \ A=0
                    sta $15	;   / init $15 var
                    lda $02fb	;   get ?
                    sta $18	;   copy to $18 var
                    lda $02fc	;   get ?
                    sta $17	;   copy to $17 var
                    jsr Sec5c	;   division routine
                    lda $15	;   get ...
Lf2c2               sta $02f7	; for record-only mode
                    lda $02f9	; copy $02f9 (money left over)
Lf2c8               sta $02f8	;   to $02f8 (money-in but uncredited)
                    lda #$00	; \
                    sta $3b	;  zero out $37 and $3b vars ($3b record/video?)
                    sta $37	; /
                    lda #$ff	; \ 
                    sta $4c	; / init $4c var to $ff
                    cmp $031b	; check for freeplay mode
                    bne Lf2e6	; if NOT freeplay mode branch down
				;
				; handle freeplay mode
				;
                    lda $88	;   yes FREEPLAY; get $88 var
                    cmp #$0b	;   compare to $0b (11)
                    bcc Lf312	;   branch way down if $88 var < 11
                    lda #$00	;   \
                    sta $88	;   / zero out $88 variable
                    beq Lf302	;   always branch
				;
				; handle normal coinop mode (not freeplay)
				;
Lf2e6               lda $02f5	; get current credit total
                    bne Lf2f9	; if record credits are present branch down
				;
				; handle running out of credits
				;
                    lda #$0a	;   \
                    sta $9a	;   / put $0a at $9a var (timer value for 10 secs of THANK YOU)
                    lda #$02	;   \
                    sta $38	;   / put 2 at $38 var (turn on THANK YOU?)
                    lda #$aa	;   \ bitmask %1010 1010
                    and $4c	;   / clear even bits of $4c?
                    sta $4c	;   store it back
				;
Lf2f9               lda $02f7	; have credits; check $02f7 (video credits?)
                    bne Lf304	; if non-zero branch ahead
                    lda #$55	;   bitmask %0101 0101
                    and $4c	;   clear some bits of $4c
Lf302               sta $4c	;   write it back
Lf304               ldy #$32	; init loop counter Y=50
Lf306               lda $4c	;   get $4c var(?)
                    dey		;   decrement counter
                    and $0100,y	;   adjust bits based on table value
                    sta $0100,y	;   update table value
                    tya		;   (seems silly)(end with A=0 too?)
                    bne Lf306	; loopback
Lf312               jsr Se59c	; add credits and update LED displays
                    rts		; done here!
				;
				; video mode
				;
Lf316               lda $02f7	; get current video credit count
                    beq Lf31e	; skip next opcode if 0 video credits
                    dec $02f7	;   decrement video credit count
Lf31e               clc		; clear carry flag
                    lda $02fc	; 
                    adc $02fd	;
                    sta $02fd	;
                    lda #$00	; A=0
                    adc $02fb	; add (carry bit) to A
                    eor #$ff	; invert every bit
                    sec		; set carry flag (why?)
                    adc $02f5	; add number of credits
                    bcs Lf337	; skip next opcode if carry flag set?
                    lda #$00	;   A=0
Lf337               sta $02f5	; store updated credit total
                    cmp #$01	; do we have just 1 credit?
                    bne Lf348	; if no, branch down
                    lda #$e0	;   A=$e0 (224)
                    cmp $02fd	;   compare to ???
                    bcs Lf348	;   skip next opcode if carry flag set
                    dec $02f5	;     decrement credit count by 1
Lf348               lda $02f5	; get number of credits
                    bne Lf355	; branch if non-zero
                    lda $02f7	;   get  ???
                    beq Lf355	;   if ??? = 0 skip next opcode
                    inc $02f5	;     increment credit count by 1
Lf355               lda $02fa	; get money-left-over var
                    jmp Lf2c8	; loopback near top
                    		;
				;***********************************
				; HANDLE USER ENTRY IN SERVICE MODE
				;***********************************
				;
Sf35b               jsr Sf975	; process CCC buttons/switches(?)
                    jsr Sf205	; read the keypad (including POPULAR + #)
                    lda $36	; get keypress code
                    bmi Lf372	; branch to rts if no key is pressed ($80, minus flag set) 
                    cmp #$0a	; is the RESET key pressed?
                    bne Lf373	; if not, branch down to check the POPULAR key next
				;
				; handle RESET keypress
				;
                    lda #$00	; \
                    sta $23	; / reset key entry position to 0
                    sta $58	; clear POPULAR key-pressed status
                    jsr Sff31	;   clear top LED displays
Lf372               rts		; done
				;
Lf373               lda $66	; get POPULAR key status
                    bne Lf37b	; if not pressed, branch ahead
                    lda #$ff	;   \ incidate popular key status
                    sta $58	;   / using a counter?
Lf37b               lda $23	; get current position in multi-digit entry
                    bne Lf382	; skip next opcode if non-zero
                    jsr Sff31	;   clear top LED displays
Lf382               inc $58	; increment $58, POPULAR key status/counter?
                    jsr Sf89c	; update 3-digit entry table
                    lda $23	; get current position in multi-digit entry
                    cmp #$03	; have 3 digits been entered yet?
                    bne Lf372	; if NOT, branch back to rts
				;
				; 3 digits have been entered
				;
                    lda #$00	; \
                    sta $23	; / set entry position back to 0
                    ldx $58	; get POPULAR status to X (number of keys pressed while holding POPULAR?)
                    sta $58	; reset POPULAR status/counter to 0
                    sta $81	; set $81 var = 0 (copy of POPULAR status?)
                    cpx #$03	; is X==3? (were all 3 digits entered while holding POPULAR?)
                    bne Lf3c9	; if not, branch to handle a regular entry
				;
				; check an entered security code
				;
                    jsr Sfb55	; convert 3-digit entry to binary
                    lda $2c	; get LSB to A
                    cmp $033a	; compare to memory location 58 (security code LSB)
                    bne Lf3b3	; NOT a match? branch down
                    lda $2b	; get MSB to A
                    cmp $033b	; compare to memory location 59 (security code MSB)
                    bne Lf3b3	; NOT a match? branch down
                    lda #$00	;   \ security code correct!
                    sta $8a	;   / reset wrong-security-code counter to 0
                    jmp Sec99	;   to programming mode
				;
				; incorrect security code entered :(
				;
Lf3b3               inc $8a	; increment wrong-security-code counter
                    ldy $8a	; get the count
                    cpy #$03	; has counter reached 3?
                    bcc Lf372	; if not, branch to RTS
                    lda #$00	;   if so, someone entered POPULAR+000 3 times
                    sta $8a	;   zero out wrong-security-code counter
                    sta $033a	;   \ reset security code to 000
                    sta $033b	;   /
                    jsr Sef18	;   update RAM checksum
                    rts		; done
                    		;
				; handle 3-digit input (not a security code)
				;
Lf3c9               lda #$00	; \
                    sta $58	;  } clear popular key status flag (again)
                    sta $81	; /  clear LSB of pointer
                    lda $29	; get middle (10s) digit of entry
                    asl a	; \			\
                    asl a	;  } multiply by 8	 |
                    asl a	; /		 	 |  overall effect
                    sta $06	; store to temp var       > is multiply by 10
                    lsr a	; \ now divide by 4	 |  & convert to binary
                    lsr a	; / (original*2)	 |
                    adc $06	; add temp var back in	/
                    adc $2a	; add the ones digit; now holds 2nd-3rd digits (in binary)
                    tay		; copy to Y (Y holds last 2 digits of entry in binary)
                    sta $06	; also store in $06
                    lda $28	; get 1st digit entered into A
                    bne Lf3ff	; if <> 0 branch down to handle more codes...
				;
				; 0XX CODES: UNDOCUMENTED 
				; (shows certain page 1 or page 0 RAM contents)
				;
                    tya		; Y->A (Y and A now hold "XX" part of entered code)
                    cmp #$3b	; compare A to 59
                    bcs Lf3f7	; if A >= 59, branch down
				;
				; handle 000-058 (display logged serial bytes?)
				;
                    adc #$33	; add 51 to A (now 51-109, $33-$6d)
                    tax		; copy A to X
                    lda $0100,x	; get a serial log table value ($0133-$016d)
                    cpx #$33	; compare X to 51 (code 000)
                    bne Lf3f4	; skip next opcode if not equal
                    sbc #$33	;   for code 000: subtract 51 from table value
Lf3f4               jmp Lf40e	; end of 5XX code handler (display single-byte value)
				;
				; handle 059-099 (display page 0 values, mostly)
				;
Lf3f7               adc #$9f	; add 159 to A (now 218-258, $da-$102)
                    tax		; copy A to X
                    lda $00,x	; get zero-page value, address X
                    jmp Lf40e	; end of 5XX code handler (display a single-byte value)
				;
				; 5XX CODE: SHOW COUNTER TOTALS
				;
Lf3ff               cmp #$05	; A==5? (5XX command handler)
                    bne Lf41c	; if not branch ahead to continue checks...
                    ldx $ffe4,y	;   get offset to appropriate counter on page 2 from ROM table
                    lda $0201,x	;   get MSB of counter
                    sta $25	;   put at $25
                    lda $0200,x	;   get LSB of counter
Lf40e               sta $24	;   put at $24
Lf410               lda #$74	;   A=$74 set offset for output
                    sta $26	;   put $74 offset ($1b+$74 = $8f-$92 ouput; "SELECTION PLAYING" LEDs)
                    jsr See45	;   parse 2-byte number into 4 decimal digits
                    lda $8f	;   get 1st digit (1000s)
                    sta $96	;   copy to $96 (put 1st digit on rightmost "MOST POPULAR SELECTION" LED)
                    rts		;   done here!
				;
Lf41c               ldx #$05	; \ set default pointer MSB to page 5 (for 1XX/2XX)
                    stx $82	; / 
                    ldy #$00	; init offset for A-sides to 0
                    cmp #$01	; compare A to 1 (1XX command)
                    beq Lf43a	;   if so branch down into shared handler
                    cmp #$03	; compare A to 3 (3XX command)
                    beq Lf436	;   if so branch down to change MSB of pointer
                    ldy #$64	; init offset for B-sides to 100
                    cmp #$02	; compare A to 2 (2XX command)
                    beq Lf43a	;   if so branch down into shared handler
                    ldy #$40	; init offset for video B-sides to 64?
                    cmp #$04	; compare A to 4 (4XX command; undocumented)
                    bne Lf456	; if NOT equal, branch down to continue checks...
				;
				; 4XX/3XX CODE: [undocumented] (video related?)
				;
Lf436               lda #$07	; \
                    sta $82	; / set MSB of pointer to page 7 (?)
				;
				; SHARED CODE FOR 1XX/2XX/3XX/4XX CODES
				;  
Lf43a               tya		; offset of $00, $64 (100) or $40 (64, for video)
                    clc		; clear carry flag
                    adc $06	; add XX digits of code to offset
                    sta $06	;   put back to temp var (full selection #)
                    ldy #$00	;   init counter to 0
Lf442               lda ($81),y	;     get value from table
                    cmp $06	;     compare it to selected #
                    beq Lf44d	;     if so, exit loop (we have a hit, of some sort?)
                    iny		;     next Y
                    beq Lf456	;     exit loop if wrapped to 0
                    bne Lf442	;   otherwise loopback
Lf44d               dec $82	; decrement MSB of pointer from 5 to 4 (or from 7 to 6)
                    lda ($81),y	; get value from 2nd table
                    sta $24	; store byte to output (plays, 0-255)
                    jmp Lf410	; jump up to end of 5XX handler to output single-byte value
                    		;
Lf456               cmp #$07	; compare A to 7 (for 7XX codes)
                    beq Lf47c	; if A=7 branch down to handle 7XX codes
				;
				; 8XX CODE [any 8XX works, not just 800]	
				;
                    cmp #$08	; compare A to 8 (for 8XX codes)
                    bne Lf467	; if A<>8 branch down to continue checks
                    jsr Se453	;   update LED displays
                    jsr Se002	;   perform RAM dump for an "inteROWEgator"
                    jmp Sff31	;   clear top LED displays (then rts from there)
				;
				; 9XX CODE
				;
Lf467               cmp #$09	; compare A to 9 (for 9XX code)
                    bne Lf47b	; if not, branch to RTS
                    ldy #$63	; \ 
                    cpy $06	; / are 2nd & 3rd digits 99?
                    bne Lf47b	; if not 99, branch to RTS [ignores all 9XX except 999]
                    sty $cb	;   \ put Y (99 decimal)
                    sty $cd	;   / at $cb and $cd (why?) (indicate warm reboot)
                    jsr Se453	;   update LED displays
                    jmp Reset	;   reboot the juke!
Lf47b               rts		; done here!
				;
				; 7XX CODE
				;
Lf47c               ldy $06	; get XX part of code to Y
                    bne Lf48b	; if > 0, branch down to check more codes...
				;
				;   CODE 700: ADD $0.25 CREDIT
				;
                    lda #$ff	; \
                    sta $a0	;  init $a0 and $cc variables to $ff (why?)
                    sta $cc	; /
                    lda #$05	; (5 nickels)
                    jmp Leb62	; jump to add credits code [gives money credit; not counted] (RTS from there)
				; 
				;   701 CODE: CLEAR CREDITS
				;
Lf48b               cpy #$01	; is Y=1?
                    bne Lf493	; if not, branch down to check more codes...
                    jsr Se749	;   clear some variables, including current credit-count
                    rts		;   done here!    
				;
				;   702 CODE: CLEAR AUTOPLAY LIST
				;
Lf493               cpy #$02	; is Y ==2?
                    bne Lf4b3	; if not, branch down to next check...
                    lda #$00	;   \
                    sta $0320	;   / first turn autoplay mode off (loc 32=0)
                    ldy #$03	;   \ MSB of pointer      
                    sty $82	;   /
                    ldy #$3a	;   \ LSB of pointer
                    sty $81	;   / $81/$82 -> $033a (2 below autoplay list)
                    ldy #$1f	;   init offset of $1f (31)
Lf4a6               sta ($81),y	;   zero out a RAM table value
                    dey		;     next Y down
                    bne Lf4a6	;   loopback until Y=0 (zeros $033c-$0359; locs 60-89)
                    jsr Sef18	;   update the programmable RAM checksum
                    lda #$00	;   \
                    sta $e6	;   / reset counter for custom autoplay to 0
                    rts  	;   done
				;
				;   799 CODE: CLEAR REGULAR SELECTION LIST
				;
Lf4b3               cpy #$63	; is Y = 99?
                    bne Lf4f6	; if not, branch down for more checks...
Sf4b7               lda #$00	;   [alt. entry point]
                    sta $e5	;   zero (current selection position)
                    sta $ee	;   zero (next FIFO position)
                    sta $81	;   \
                    lda #$02	;    |
                    sta $82	;   / $81/$82 -> $0200 (base of selection list)
                    inc $dc	;   increment $dc(?)
                    lda #$ff	;   A = $ff
                    ldy #$c7	;   Y = $c7 (199)
Lf4c9               sta ($81),y	;   put $ff at $0200 + Y    \
                    dey		;       decrement Y counter  > $0200-$02c7 filled w/$ff 1st tmie
                    bne Lf4c9	;     loopback until Y=0     > $0100-$0131 filled w/$00 2nd time
                    sta ($81),y	;     once more with Y=0    /
                    dec $82	;     decrement $82 from 2 to 1 (now -> $0100)
                    beq Lf4db	;     if decremented a 2nd time, exit loop
                    lda #$00	;     A=0 (fill with 0 this time)
                    ldy #$31	;     (50)
                    jmp Lf4c9 	;   jump back up
Lf4db               lda #$90	;   \
                    sta $f7	;    >init $f7 & $f8 vars to $90
                    sta $f8	;   /
                    lda #$3a	;   \ init $eb var to $3a
                    sta $eb	;   /
                    ldx #$90	;   init counter X=$90 (144)
                    lda #$00	;   A=0
Lf4e9               sta $0300,x	;   zero out RAM values from $0390 to $03ff (video playlist?)
                    inx		;     increment counter
                    bne Lf4e9	;   loopback until X=0
                    lda $ed	;   get $ed var
                    beq Lf4f5	;   branch to RTS if $ed==0
                    sta $f4	;     copy $ed var to $f4 var
Lf4f5               rts		;   done here!
				;
				;   CODE 750: CLEAR PLAY/MONEY TOTALS
				;
Lf4f6               cpy #$32	; is Y = 50?
                    bne Lf50c	; if not, branch to next check...
                    lda $0388	;   get setting loc 97 (disable/allow clearing in service mode)
                    cmp #$02	;   compare to 2
                    bcs Lf50b	;   if A >= 2 branch to RTS (ignore 750 command)
Sf501               lda #$00	;     [alt. entry point]
                    ldy #$1a	;     init loop counter Y=$1a (13 2-byte counters = 26)
Lf505               sta $02d7,y	;     clear $02d8-$02f1 
                    dey		;       decrement Y counter
                    bne Lf505	;     loopback until done
Lf50b               rts		;   done here!
				;
				;   CODE 770: VIDEO SYSTEM INIT
				; 
Lf50c               cpy #$46	; compare Y to 70 (for code 770: video system init)
                    bne Lf523	; if not, branch to next check...
                    lda #$00	;   apparently "video system init"
                    sta $f9	;   only entails setting 6
                    sta $e8	;   default variable values:
                    sta $ea	;   $f9,$e8,$ea = 0
                    lda #$ff	;   \
                    sta $f1	;   / $f1 = $ff
                    lda #$0a	;   \
                    sta $eb	;     $eb,$fb = $0a
                    sta $fb	;   /
                    rts		;   done here!
				;
				;   CODE 703: [undocumented]	
				;
Lf523               cpy #$03	; Y=3?
                    bne Lf50b	; if not, branch to nearby rts (sorry, no more valid codes)
                    jsr Sf4b7	;   clear regular selection list
                    ldx #$40	;   X=$40 (64)
                    jsr Sf1b6	;   [add to video playlist?]
                    ldx #$a4	;   X=$a4 (164)
                    jsr Sf1b6	;   [add to video playlist?]
                    rts		;   done here!
                    		;
				;*************
				; DO AUTOPLAY
				;*************
				; [needs attention in video portion]
				;
Sf535               lda $0321	; get autoplay time (in minutes) from programmable memory
                    sta $fa	; store autoplay time (mins) in var $fa (resetting the timer)
                    ldx #$ea	; point to total autoplays counter
                    jsr Sf18d	; increment total autoplays (0-9999) counter
                    inc $e1	; increment the autoplay counter
                    lda $e1	; ...and get it to A
                    cmp #$c8	; compare counter to $c8 (200 decimal)
                    lda #$00	; in case we went past 200 (to 201) set A back to 0
                    bcc Lf54b	; skip the next instruction if we did NOT go past 200
                    sta $e1	;   store (0) to autoplay counter if we wrapped around
Lf54b               sta $84	; zero out $84 variable(?)
                    lda $0320	; get autoplay mode setting
                    cmp #$05	; is it set to "custom list" mode?
                    bcc Lf5ba	; if A < 5 branch way down (A holds mode #)
                    cmp #$05	; is it set to custom list? (again?)
                    beq Lf55b	; if so, skip this jump
                    jmp Lf5f2	;   if not set to custom list; A>5 (treat modes > 5 as mode 1?)
				; 
                    		; handle "custom list" autoplay mode 5
				; 
Lf55b               ldx $e6	; get variable from $e6 into X (list autoplay position)
                    txa		; \ 
                    pha		; / put X to stack (list autoplay position)
                    lda $033c,x	; fetch selection LSB from the custom list
                    sta $24	; store LSB at $24 (to be converted to digits)
                    inx		; increment to next byte in memory
                    lda $033c,x	; fetch MSB from list [these are 2-byte entries]
                    sta $25	; store MSB at $25 (to be converted to digits)
                    lda #$00	; \ 0 offset for output
                    sta $26	; / will be stored at $1b-1e ($1b not used here)
                    jsr See45	; parse binary number into decimal digits (100-299)
                    dec $1c	; decrement 100s digit (2XX becomes 1XX or 1XX becomes 0XX)
Lf573               lda $1c	; get 100s digit
                    cmp #$02	;   compare it to 2
                    bcc Lf583	;   exit loop if < 2
                    dec $1c	;   \ decrement 1st digit
                    dec $1c	;   / twice
                    lda #$ff	;   \ 
                    sta $84	;   / set $84 var to $ff (flag for what?)
                    bne Lf573	; loopback (unconditional)
Lf583               lda $1c	; get 100s digit of selection
                    sta $1b	;   put it in 100s digit position for $ee9e routine
                    lda $1d	; get 10s digit of selection
                    sta $1c	;   put it in 10s digit position for $ee9e routine 
                    lda $1e	; get 1s digit of selection
                    sta $1d	;   put at in 1s digit position for $ee9e routine
                    jsr See9e	; convert 3-digit selection ($1b-$1d) back to binary ($2b/$2c) [now 000-199]
                    ldx $2c	; get binary selection to X
                    lda $84	; get $84 (flag for what?)
                    beq Lf5a1	; if $84=0 branch down
                    jsr Sf1b6	;   [add to video playlist?]
                    lda #$00	;   A=0
                    sta $f1	;   init $f1 var to 0
                    beq Lf5a4	;   unconditional branch
Lf5a1               jsr Lf1ca	; put selection onto playlist
Lf5a4               pla		; pull A back off stack (autoplay list position?)
                    clc		; clear carry for addition
                    adc #$02	; add 2 to autoplay selection list (2 bytes per entry)
                    tax		; A->X
                    cmp #$1d	; compare A to $1d (29)
                    bcc Lf5b2	; if A < 29 branch down
Lf5ad               ldx #$00	;     X=0 at 29, roll back to 0
                    stx $e6	;     reset autoplay list position to 0
                    rts		;     done here
Lf5b2               stx $e6	;   store autoplay list position
                    lda $033c,x	;   get autoplay custom programmed table value based on X
                    beq Lf5ad	; init position if autoplay table value is 0
                    rts		;   if non-zero, done here! (returning selection # in A)
				;
				; handle non-list autoplay modes
				; [needs more attention]
				;
Lf5ba               cmp #$03	; auto play mode 3? (video)
                    bcc Lf5ed	; is A < 3 branch down
                    lda $fc	;   get variable - video autoplay mode...
                    clc		;   clear carry flag for addition
                    adc $fd	;   add $fd var(?)
                    cmp $e1	;   compare to $e1 var (autoplay counter?)
                    bcs Lf5e2	;   branch if > 
                    lda $fe	;   get $fe var
                    beq Lf5de	;   branch if	
                    ldx $e1	;   get $e1 var
                    cpx #$64	;   compare to $64
                    bcs Lf5d5	;
                    ldx #$64	;
                    stx $e1	;
Lf5d5               clc		;
                    adc $ff	;
                    adc #$64	;
                    cmp $e1	;
                    bcs Lf5e2	;
Lf5de               lda #$00	;
                    sta $e1	;
Lf5e2               lda $e1	;
                    tax		;
                    lda #$00	;
                    sta $f1	;
                    jsr Sf1b6	;
                    rts		;   done here!
				;
Lf5ed               tax		; X->A
                    cpx #$02	; autoplay mode 2? (B-sides)
                    beq Lf5fd	; if so, branch down
				; 
				; handle autoplay mode 1 (A-sides only) [by process of elimination]
				;
Lf5f2               lda $e1	; get autoplay position counter
                    cmp #$64	; \
                    bcc Lf605	; / branch ahead if A < 100
                    sbc #$64	;   subtract 100 (make B-sides into A-sides?) 
                    jmp Lf605	; (jump ahead to XX8/XX9 check in common part of code)
                    		;
				; handle autoplay mode 2 (B-sides only)
				;
Lf5fd               lda $e1	; get current autoplay counter
                    cmp #$64	; compare to 100
                    bcs Lf605	; if A >= 100 skip next opcode
Lf603               adc #$64	;   A=A+100 (to make A-sides into B-sides?)
				;
				; [modes 1 and 2 join here]
				;
Lf605               ldx $031d	; are XX8 & XX9 selections disabled?
                    bne Lf611	; branch down if disabled
                    ldx $39	;   video mode boolean?
                    bne Lf611	;   if $39 is non-zero, branch over next opcode
                    jmp Lf634	;     jump down a bit
Lf611               sta $24	; store A (modified selection #?) at $24 for conv. to digits
                    ldx #$00	; X=0
                    stx $26	; offset of 0 (output to $1b-$1e)
                    stx $25	; MSB of 0 [selection number will be < 256]
                    jsr See45	; convert binary to decimal digits
                    ldx $1e	; get last digit
                    cpx #$08	; compare last digit to 8
                    bcc Lf634	; if < 8 branch ahead
                    adc #$01	;   add one to A (+1 more for the carry bit?)
                    cmp #$64	;   compare A to 100
                    bne Lf62c	;   if !=100 skip ahead
                    lda #$00	;     if A=100, make it 0 instead
                    beq Lf632	;     always branch
Lf62c               cmp #$c8	;   compare A to 200
                    bcc Lf632	;   if < 200 branch over next
                    lda #$64	;     if A=200 make it 100 instead
Lf632               sta $e1	; store updated selection # at $e1
Lf634               tax		; copy A to X (A & X both old selection #)
                    pha		; push A (selection #)
                    jsr Lf1ca	; put selection into playlist
                    pla		; pull A (selection #)
				;
				; tweak for autoplay mode 6 [and 7?] (A and B sides)
				;
                    ldx $0320	; get autoplay mode variable
                    cpx #$06	; \
                    bcc Lf645	; / if mode < 6, branch to rts
                    cmp #$64	;   compare A (selection #) to 100 
                    bcc Lf603	;   if < 100 branch back to cue up the B-side with each A-side
Lf645               rts		; 
                    		;
				;****************************************
				; MONITOR SERVICE SWITCH, MECH AND COINS
				;****************************************
				;  called from 2 places
				;
Sf646               jsr Se56a	; read PIA1-A and service switch (including override)
                    jsr Sf653	; manage magaine position
                    jsr Sf775	; handle record mech
                    jsr Sea9c	; take & count money; give credits
                    rts		;
                    		;***************************
				; manage magazine position?
				;***************************
				;
Sf653               lda $68	; get PIA1-A status
                    and #$20	; %0010 0000 isolate bit 5: INDEX opto
                    bne Lf660	; branch if bit 5 high (active)
                    lda $59	;   get $59 counter (debounce?)
                    beq Lf65f	;   if $59=0, branch to rts
                    dec $59	;   decrement $59 counter (debounce?)
Lf65f               rts		;   done
Lf660               lda $59	; get $59 debounce counter
                    bne Lf65f	; branch to rts if not 0 yet
				;
				; index opto now debounced
				;
                    lda $2002	; get PIA1-B
                    and #$06	; isolate bits 1 & 2 (%0000 0110) DETENT & MAGazine MOTOR
                    cmp #$06	; check if both are high (active)
                    bne Lf65f	; if not, branch to rts
                    lda #$01	; \ 
                    sta $59	; / reset index debounce counter to 1 (why?)
                    inc $67	; increment $67 counter (current magazine position)
                    lda #$ff	; \ set $f0 boolean var
                    sta $f0	; /
                    lda $68	; get PIA1-A status
                    and #$10	; %0001 0000 isolate bit 4: HOME opto
                    bne Lf683	; branch ahead if bit 4 is high (active)
                    lda $67	;   get current magazine position
                    cmp #$64	;   compare to 100
                    bcc Lf68a	;   branch down if not at position 100 yet
Lf683               lda #$00	;     \ wrap magazine position around to 0
                    sta $67	;     /
                    jsr Sf73c	;    [subroutine] 
Lf68a               lda $68	; get PIA1-A status
                    and #$04	; %0000 0100 isolate bit 2: SERVICE / ON
                    beq Lf69a	; branch ahead if low (ON mode?)
                    lda $68	;   get PIA1-A status
                    and #$80	;   %1000 0000 isolate bit 7: CANCEL
                    bne Lf699	;   branch to rts if CANCEL pressed?
                    jsr Sff4d	;   turn DETENT off
Lf699               rts		;   done
				;
Lf69a               lda $74	; get $74 var
                    beq Lf6a1	; branch down if 0
                    dec $74	;   decrement $74 counter
                    rts		;   done
Lf6a1               lda $ea	; get $ea var
                    beq Lf6be	; branch down if 0
                    lda $e9	;   get $e9 var(?)
                    cmp $67	;   compare to current magazine position
                    bne Lf6d0	;   branch ahead if not equal
                    sta $ef	;     store A to $ef ($e9=$67=selection=magazine position?)
                    lda #$eb	;     A=$eb
                    jsr Sf72e	;     [subroutine]
                    lda #$ff	;     \ init $f3 var to $ff (A-side flag?)(turntable motor flag?)
                    sta $f3	;     /
                    lda #$00	;     \ init $f2 var to $00 (B-side flag?)
                    sta $f2	;     /
Lf6ba               jsr Sff4d	;     turn DETENT off
                    rts		;  done
				;
Lf6be               lda $70	; get $70 var (selection playing flag?)
                    beq Lf6ba	; if 0, branch back
                    inc $5a	;   increment $5a var
                    lda #$00	;   \ init $f3 var to $00 (A-side flag?)
                    sta $f3	;   /
                    lda #$ff	;   \ init $f2 var to $ff (B-side flag?)
                    sta $f2	;   /
                    jsr Sf6d9	;   [subroutine to check spinning magazine?]
                    rts		;   done here!
				;
Lf6d0               cmp #$64	; compare ($e9 var) to $64 (100)
                    bcc Lf6d8	; if A < 100 branch to RTS 
                    lda #$63	;   \ init $e9 var to $63 (99)
                    sta $e9	;   /
Lf6d8               rts		; done
				;
                    		;***************************************
				; check to see if spinning magazine has
				; reached next selection in list?
				;***************************************
				;
Sf6d9               lda $67	; get $67 var to A (current magazine position 0-99)
                    ldx $f2	; get $f2 var to X (B-side flag?)
                    bne Lf6e2	; if $f2 var > 0, skip ahead
                    clc		;   clear carry flag
                    adc #$64	;   A=A+100
Lf6e2               sta $ef	; store A to $ef (magazine position + 100=B-side selection #?)
                    tax		; A -> X [X now holds selection number of current mag pos]
                    lda $0339	; get FIFO/conventional setting
                    bne Lf6fe	; branch ahead for FIFO mode
				; 
				; (conventional mode)
				;
                    lda $0200,x	; get entry in $0200 table for record passing by
                    cmp #$c8	; compare entry to $c8 (200) [has this record been selected?]
                    bcc Lf735	; branch down if A < 200 [branch if record is selected]
Lf6f1               lda $f0	;   get $f0 var(?)
                    beq Lf6fd	;   branch to RTS is $f0=0
                    lda $f2	;     get $f2 var (B-side flag?)
                    eor #$ff	;     flip boolean sense
                    sta $f2	;     store $f2 back
                    beq Sf6d9	;     branch back if $f2 is now 0 [check other side of this record?]
Lf6fd               rts		; done
				;
				; (FIFO mode)
				;
Lf6fe               ldx $ee	; get current position in FIFO list to X
                    lda $0200,x	; get Xth entry in $0200 table [next record to play]
                    cmp $ef	; compare to $ef (selection # of this position?)
                    bne Lf6f1	; branch back if not equal
                    lda #$ff	; \ store $ff in Xth entry in $0200 table
                    sta $0200,x	; / (unmark this in list?)
                    inc $ee	; increment position in FIFO list position
                    lda #$61	; \ 
                    jsr Sfcff	; / put #$61 into event table (selection made?)
                    lda $ee	; get position in FIFO list to A
                    cmp #$c8	; compare to $c8 (200, maximum number of selections)
                    bcc Lf71d	; if A < 200, branch ahead
                    lda #$00	;   \ wrap FIFO list position back to $00
                    sta $ee	;   /
				;
				; need to play this record!
				;
Lf71d               lda #$ff	; \
                    eor $f3	; / get $f3 var, inverting every bit (flag for turntable motor?)
                    sta $f3	; store $f3 back, with every bit inverted (toggling flag?)
                    lda #$ff	; \
                    eor $f2	; / get $f2 var, inverting every bit (B-side flag?)
                    sta $f2	; store $f2 back, with every bit inverted
                    jsr Sff4d	; turn DETENT off (stop the magazine here)
                    lda #$db	; A=$db
				; [alt. entry point]
Sf72e               sta $9e	; store A to $9e var(?)
                    lda $ef	; get $ef var (selection 0-199)
                    sta $c9	; copy to $c9
                    rts		; done
				;
				; (conventional mode, found a record)
				;
Lf735               lda #$ff	; A=$ff 
                    sta $0200,x	; put $ff in $0200 table at Xth position [clear the mark]
                    bne Lf71d	; (unconditional) branch up to play it
				;
				;*****
				; ???
				;*****
				; (called from 3 places)
				; [needs attention, obviously]
				;
Sf73c               lda $ea	; get $ea var
                    bne Lf759	; if non-zero branch down
                    ldx #$00	; init X=0
Lf742               lda $0200,x	;   get selection table value
                    cmp #$c8	;   compare to $c8 (200)
                    bcs Lf75c	;   branch ahead if A >= 200 (video selection?)
                    lda $ee	;   get $ee var (position of selection index in FIFO mode?)
                    cmp #$c8	;   compare to $c8 (200)
                    bcc Lf76d	;   if A < 200 branch way down
Lf74f               jsr Sf4b7	;     clear regular selection list 
                    jmp Lf761	;     jump down...
Lf755               lda #$ff	;   \
Lf757               sta $70	;    | store $ff in two selection-related variables
Lf759               sta $3c	;   /
                    rts		; done
				;
Lf75c               inx		;   next X
                    cpx #$c8	;   is X up to 200?
                    bcc Lf742	; loopback if not
Lf761               lda #$00	;	 
                    cmp $ea	; is $ea var 0?
                    beq Lf757	; if so, branch back 
                    cmp $f3	; is $ea var = $f3 var? (turntable motor flag?)
                    beq Lf755	; if equal
                    bne Lf757	; if not
				;
Lf76d               lda $e5	; get $e5 var
                    cmp #$c8	; compare to $c8 (200)
                    bcc Lf755	; loopback 
                    bcs Lf74f	; loopback to clear selection list
				;
				;********************
				; handle record mech
				;********************
				;
Sf775               lda $63	; get detent counter/timer?
                    bne Lf78b	; skip down if it's not zero yet
				;
				; detent timer reached 0
				; 
                    lda $2002	; get PIA1-B status
                    and #$02	; isolate bit 1 %0000 0010: DETENT output
                    asl a	; shift left (?)
                    sta $06	; store as temp var
                    lda $2002	; get PIA1-B status
                    and #$fb	; bitmask to clear bit 2 (%1111 1011)
                    ora $06	; OR with temp var, clearing bit 2
                    sta $2002	; output to PIA1-B - MAGAZINE MOTOR OFF
				;
				; detent timer running(?)
				;
Lf78b               dec $63	; decrement detent timer
                    lda $e8	; get $e8 var (?)
                    bne Lf7b8	; nonzero branch to jmp
                    lda $f3	; get $f3 flag (turntable motor)
                    bne Lf7ec	; if turntable is on, manage transfer
				;
				; when turntable is off
				;
                    lda $68	; get PIA1-A status
                    and #$40	; %0100 0000 isolate bit 6: INNER CAM
                    beq Lf802	; if 0, turn off tranfer motor
                    lda $39	; check video mode boolean(?)
                    bne Lf7b1	; branch in video mode?
                    lda $031e	; get records/video setting
                    bne Lf7bb	; branch if records-only mode
                    lda $69	; get $69 var (?) [stays $08 normally]
                    beq Lf7bb	; branch if 0
                    dec $85	; timer?
                    bne Lf7ae	; skip next opcode if nonzero
                    dec $69	;   timer?
Lf7ae               jmp Lf864	; jump to 'transfer in progress'?
				;
				; video mode related?
				;
Lf7b1               lda $ed	; unknown var
                    bne Lf7bb	; 
Lf7b5               jmp Lf864	; jump to 'transfer in progress'?
				;
Lf7b8               jmp Lf837	; (extents a branch above)
				;
Lf7bb               lda $f4	; (transfer-related flag?)
                    bne Lf7b5	; to 'transfer in progress'
                    lda $3c	; (selection-related flag?)
                    beq Lf7b5	; to 'transfer in progress'
                    lda $35	; service/normal mode flag
                    bne Lf7b5	; in service mode, to 'transfer in progress'
                    lda $74	; 
                    bne Lf7de	;
                    lda $e7	;
                    beq Lf7de	;
                    lda $f0	;
                    beq Lf7de	;
                    jsr Sf6d9	; check on the spinning magazine 
                    lda #$00	; \
                    sta $f0	;  } set 2 vars to 0
                    sta $e7	; /
                    beq Sf775	; always branch to top of mech routine
				;
Lf7de               lda #$00	; \ init $3c var to 0
                    sta $3c	; / (selection-related flag?)
                    jsr Sff5a	; turn on DETENT output
                    lda #$ff	; \ init $e7 to $ff
                    sta $e7	; / 
                    jmp Lf864	; jump to...
				;
				; manage transfer?
				;
Lf7ec               lda #$10	; %0001 0000
                    ora $2002	; set bit 4 of PIA1-B: TURNTABLE MOTOR
                    sta $2002	; output to PIA1-B
                    lda $68	; get PIA1-A status	
                    and #$40	; %0100 0000 isolate bit 6: INNER CAM
                    beq Lf80f	; 
                    lda $2002	; get PIA-1B status
                    ora #$40	; set bit 6 %0100 0000: PLAY COUNTER
                    sta $2002	; output to PIA1-B
				;
				; turn on transfer motor
				;
Lf802               lda #$08	; %0000 1000
                    ora $2002	; set bit 3 of PIA1-B: TRANSFER MOTOR
                    sta $2002	; output to PIA1-B (turn transfer motor on)
                    lda #$18	; \ init $42 var to $18 (transfer counter?)
                    sta $42	; /
                    rts		; done here
				;
				; manage transfer?
				;
Lf80f               lda $42	; get $42 variable (transfer counter?)
                    beq Lf828	; if 0, branch down
                    lda $f2	; get $f2 variable (B-side flag?)
                    beq Lf81f	; if 0, branch ahead
                    lda #$20	;   %0010 0000
                    ora $2002	;   set bit 5 of PIA1-B: TOGGLE COIL
                    sta $2002	;   output to PIA1-B: turn on toggle coil
Lf81f               lda $68	; get PIA1-A status
                    and #$08	; %0000 1000 isolate bit 3: OUTER CAM
                    beq Lf802	; if low loopback
                    dec $42	;   decrement $42 variable (transfer counter?)
                    rts		;   done here!
				;
				; tranfer complete, unmute and wait for cancel?
				;
Lf828               lda $2002	; get PIA1-B status
                    and #$97	; isolate some bits %1001 0111
                    ora #$01	; set bit 0 - MUTE (to unmute)
                    sta $2002	; output to PIA1-B
                    bit $68	; set flags based on PIA1-A status
                    bmi Lf837	; skip next opcode  based on bit 7 of PIA1-A (CANCEL)
                    rts		;   done here if bit 7 is clear
				;
				; end of record (or cancel): mute amp & start transfer
				;
Lf837               lda #$ff	; \
                    sta $f4	;  | set $f0 and $f4 vars to $ff
                    sta $f0	; /
                    ldy #$00	; \ 
                    sty $e8	;  | set $e8 and $f3 vars to $00
                    sty $f3	; /  (turns off turntable motor? and ???)
                    lda #$dd	; A=$dd
                    ldx $ea	; check $ea var
                    beq Lf84b	; skip next opcode if $ea=0
                    lda #$ed	;   A=$ed
Lf84b               jsr Sf72e	; (adjust a couple variables?)
                    sty $ea	; 
                    lda #$fe	; %1111 1110
                    and $2002	; clear bit 0 of PIA1-B: MUTE
                    sta $2002	; output to PIA1-B
                    jsr Sf73c	; 
                    lda $68	; get PIA1-A status
                    and #$40	; %0100 0000 isolate bit 6: INNER CAM
                    bne Lf86a	; if already active, branch to rts
                    jmp Lf802	; jump back to turn on transfer motor
				;
				; (only reached by jmps)
				; transfer off in progress?
				;
Lf864               lda $42	; get $42 counter (transfer counter?)
                    beq Lf86b	; if $42 counter = 0, skip ahead
                    dec $42	;   decrement $42 (transfer?) counter
Lf86a               rts		;   done
				;
				; transfer off complete?
				;
Lf86b               lda #$87	; %1000 0111
                    and $2002	; clear bits 3-6 of PIA1-B
                    sta $2002	; output to PIA1-B (deactivate: transfer, TT, toggle, play counter)
                    lda $f4	; get $f4 var 
                    beq Lf881	; branch down if $f4 = 0
				;
Sf877               lda #$00	;   \ init $f4 to 0  [alternate entry point]
                    sta $f4	;   /
                    sta $ed	;   \
                    sta $ec	;    | init $ea, $ec, $ed vars to 0
                    sta $ea	;   /
Lf881               lda $68	; get PIA1-A status
                    and #$04	; %0000 0100 isolate bit 2: SERVICE/ON
                    beq Lf89b	; branch to rts if inactive (ON mode?)
                    lda $68	; get PIA1-A status
                    and #$80	; %1000 0000 isolate bit 7: CANCEL
                    beq Lf89b	; branch to RTS if CANCEL inactive
				;
				;   cancel pressed (in service mode?)
				;
                    lda #$00	; \ init $74 var to $00
                    sta $74	; /
                    lda $2002	; get PIA1-B (outputs)
                    and #$02	; %0000 0010 isolate bit 1: DETENT
                    bne Lf89b	; skip next opcode if bit high (DETENT already active)
                    jsr Sff5a	;   turn on DETENT
Lf89b               rts		; done
				;
                    		;**********************
				; UPDATE 3-DIGIT ENTRY
				;**********************
				;
Sf89c               lda $23	; get current entry position
                    bne Lf8a8	; if position > 0, branch down 
                    lda #$0e	;   ($0e is code for blank digit)
                    sta $28	;   \  blank the key entry table (rightmost LEDs on top)
                    sta $29	;    | (3 bytes)
                    sta $2a	;   /  ($28-$2a)
Lf8a8               lda $36	; get keypress code
                    ldx $23	; get current entry position
                    sta $28,x	; store keycode on top LEDs
                    inc $23	; increment current entry position
                    rts		; 
				; 
				;*********************************
				; UPDATE ALL MEMOREC PLAYS TABLES
				;*********************************
				;
Sf8b1               jsr Sf95a	; [do some important background tasks before we get started]
                    lda #$00	; \
                    sta $76	;  \
                    lda #$05	;  / init $76/$77 pointer to $0500 (records MEMOREC table)
                    sta $77	; /
                    lda $3b	; \ get record/video boolean
                    beq Lf8e1	; / branch down for records
				;
				; handle video mode
				;
                    clc		; clear carry flag for addition
                    lda $fc	; fetch $fc var (video autoplay mode?)
                    adc $fe	; add $fe var to A
                    beq Lf8d1	; branch down if 0
                    ldy $fc	;   get $fc var to Y (video autoplay mode?)
                    beq Lf8d1	;   branch down if 0
                    ldy $fe	;     get $fe var to Y
                    beq Lf8d1	;     branch down if 0
                    adc #$01	;       A=A+1, if an only if: $fc+$fe<>0 AND $fc<>0 AND $fe<>0(?)
Lf8d1               tay		; A->Y
                    lda #$07	; \ change MSB of pointer
                    sta $77	; / now $76/$77 -> $0700 (video selections table)
                    lda $2c	; get selection # (0-199?)
                    cmp #$64	; compare to 100
                    bcc Lf8de	; if A < 100 skip next opcode
                    sbc #$24	;   A=A-36 [not understood]
Lf8de               jmp Lf8fa	; jump ahead (to common path)
				;
				; handle record mode
				;
Lf8e1               lda $2c	; get selection # (0-199)
                    ldy #$c7	; size of the plays table (200)
                    jsr Sf912	; update MEMOREC plays table
                    lda #$80	; \
                    sta $76	;  \ $76/$77 -> $0780 (by-record selections table)
                    lda #$07	;  /
                    sta $77	; /
                    ldy #$63	; size of plays table (100)
                    lda $2c	; get selection # (0-199)
                    cmp #$64	; compare selection to 100
                    bcc Lf8fa	; if A < 100 skip next opcode
                    sbc #$64	;   A=A-100 (ignore A/B-side, just the record number)
				;
				; common path (records and video)
				;
Lf8fa               jsr Sf912	; update MEMOREC plays table
                    lda #$60	; \
                    jsr Sfcff	; / put #$60 into event table
                    rts		; done
                    		;
				;********************
				; SWAP TABLE ENTRIES
				;********************
				;  (only used by sorting routine below)
				;
Sf903               lda ($76),y	; get table value, offset by Y
                    pha		; push it
                    dey		; previous offset
                    lda ($76),y	; get table value, offset by Y-1
                    tax		; move it to X
                    pla		; retrieve 1st value
                    sta ($76),y	; put it in table, offset by Y-1
                    iny		; original offset
                    txa		; move X back to A
                    sta ($76),y ; put it in table, offset by Y
                    rts		; done
                    		;
				;******************************************
				; INCREMENT AND SORT A MEMOREC PLAYS TABLE
				;******************************************
				;  expects: $76/$77 pointer to a table
				;          Y: size of table
				;	   A: selection # (modified to fit into 1 byte if needed)
				;
Sf912               cmp ($76),y	; compare A to table value at pointer+Y 
                    beq Lf91c	; branch down if equal
                    dey		;   move to next table entry
                    cpy #$ff	;   did we wrap around?
                    bne Sf912	; loopback until it does
                    rts		; done (selection not found in table?)
				;
				; selection "A" found in selections table (at position Y)
				;
Lf91c               lda #$08	; \
                    sta $06	; / init counter var to 8 (why?)
                    dec $77	; move from selection table to plays table
                    lda ($76),y	; get value from plays table entry
                    clc		; (clear carry flag for addition)
                    adc #$01	; add 1
                    beq Lf939	; if A wrapped around, branch to rts (leave play count at $ff)
                    sta ($76),y	; store updated play count back into table
				;
Lf92b               lda ($76),y	; get value from plays table
                    dec $06	; decrement counter var
                    bne Lf934	; skip next opcode if counter > 0
                    jsr Sf95a	;   do some important "background" tasks if sorting is taking a while
Lf934               dey		; move to next entry in tables
                    cpy #$ff	; did it wrap around?
                    bne Lf93a	; skip next opcode unless Y wrapped
Lf939               rts		;   done here!
Lf93a               cmp ($76),y	; compare (incremented value) to next entry in table  
                    bcs Lf93f	; if A >= table value, skip next opcode...
                    rts		;   done here! (already in numerical order)
Lf93f               bne Lf94b	; if A <> table value, branch down
                    inc $77	;   (point to selection table)
                    lda ($76),y	;   get value from selection table
                    iny		;   move back to previous table position
                    cmp ($76),y	;   compare
                    bcs Lf951	;   branch if A >= previous value, swap [sorts by selection for those with same # of plays]
                    rts		; done here!
Lf94b               iny		; move to previous table position
                    jsr Sf903	; swap table entries
                    inc $77	; (point to selections table)
Lf951               jsr Sf903	; swap table entries
                    dey		; move to next table position
                    dec $77	; (point back to plays table)
                    jmp Lf92b	; loopback
                    		;
				;********************************
				; perform some background tasks?
				;********************************
				; (preserves all registers & $2c variable?)
				; (called from 4 places)
				;
Sf95a               pha		; \
                    txa		;  |
                    pha		;  | push all registers
                    tya		;  |
                    pha		; /
                    lda $2c	; \ get $2c variable 
                    pha		; / push that variable
                    jsr Sf646	;   [calls 4 other basic function subroutines]
                    pla		; \ pull previously stored $2c variable
                    sta $2c	; / return to $2c
                    pla		; \
                    tay		;  |
                    pla		;  |pull X & Y back
                    tax		; /
                    lda #$14	; \ reset counter $06 (to 20)
                    sta $06	; /
                    pla		; pull A back
                    rts		; 
                    		;
				;***********************************
				; record-mode CCC buttons/switches?
				;***********************************
				; (main entry is actually $f975)
				;
Lf972               jmp Lfa32	; (to video mode code?)
				;
Sf975               lda $72	; get var $72
                    beq Lf972	; branch up to jump if 0
                    lda #$b1	; %1011 0001 sel=[101]=D5=S3: most/least switch
                    sta $4002	; write to PIA2-B
                    lda $43	; get $43 var (most/least switch status?)
                    eor $4000	; flip bits with PIA2-A status?
                    and #$02	; isolate bit 1 %0000 0010: keypad/button input (most/least switch?)
                    bne Lf972	; branch back to jmp if high
                    lda #$f0	; %1111 0000 sel=[110]=D6=S2: video/record switch
                    sta $4002	; write to PIA2-B
                    lda #$02	; isolate bit 1 %0000 0010: button input
                    and $4000	; get PIA2-A inputs (ANDed with A's bitmask)
                    sta $06	; store video/record switch status at $06? (either 0 or 2?)
                    lda $76	; get var $76
                    cmp #$ff	; is it $ff?
                    bne Lf99b	; if not, skip next opcode
                    lda #$00	;   A=0
Lf99b               and #$80	; isolate bit 7 of A
                    lsr a	; \
                    lsr a	; | shift A to the right
                    lsr a	; | 6 times
                    lsr a	; | this moves bit 7 to bit 1
                    lsr a	; |
                    lsr a	; /
                    eor $06	; flip bits based on var $06 (either 0 or 2?)
                    bne Lf972	; if not zero branch up to JMP
                    lda #$b0	; %1011 0000
                    sta $4002	; write to PIA2-B - sel=[100]=D4=pin15 = S1 = reset button
                    lda #$02	; %0000 0010 isolate bit 1: buttons/keypad (CCC RESET button)
                    and $4000	; read CCC RESET button
                    beq Lf9cd	; branch down if pressed
                    lda #$f1	;   %1111 0001
                    sta $4002	;   write to PIA2-B: sel=[111]=D7=pin12 = S4 = advance button
                    lda #$02	;   %0000 0010 isolate bit 1: buttons/keypad
                    and $4000	;   read CCC advance button
                    bne Lf9c6	;   skip ahead if not pressed
                    lda $73	;     get $73 var
                    bne Lf9cc	;     if $73 not 0 branch to RTS
                    jmp Lfa86	;     jump into another routine?
Lf9c6               lda $73	; get $73 var
                    beq Lf9cc	; skip next opcode if $73 var = 0
                    dec $73	;   decrement $73 var
Lf9cc               rts		; done here!
				;
				; CCC RESET button pressed
				;
Lf9cd               lda #$02	; \ set MSB of pointer
                    sta $07	; / to $02xx
                    lda $0388	; get setting to dis/allow resetting 5xx stats (0, 1 or 2)
                    bne Sf9df	; if not 0, branch down (reset other stuff, but NOT money/play counters)
Sf9d6               lda #$d8	;   \ LSB of pointer
                    sta $06	;   / $06/$07 -> $02d8 (money and play counters)
                    ldy #$19	;   size of range = $19 bytes ($02d8-$02f1)
                    jsr Sfb19	;   zero out RAM range (money and play counters)
				;
Sf9df               lda #$00	; \
                    sta $06	; |
                    lda #$04	; |
                    sta $07	; | $06/$07 point to $0400
                    ldy #$c7	; / target range: $0400 - $04c7 (000-199)
                    jsr Sfb19	; zero out RAM range
                    lda #$06	; \
                    sta $07	; |
                    ldy #$c7	; / target range: $0600 - $06c7 (000-199)
                    jsr Sfb19	; zero out RAM range
                    lda #$80	; \
                    sta $06	; |	
                    ldy #$63	; / target range: $0680? - $06E3???
                    jsr Sfb19	; zero out RAM range
                    lda #$00	; \
                    sta $06	; |LSB of pointer $00
                    sta $72	; |
                    lda #$05	;
                    sta $07	;  MSB of pointer $05
                    ldy #$c7	;  target range $0500-$05c7 (0-199)
                    jsr Sfb23	;  fill with sequential values?
                    lda #$07	; \
                    sta $07	; / MSB of pointer $07
                    ldy #$3f	; target range $0700-$073f
                    jsr Sfb23	; fill range with sequential values
                    lda $fc	; get $fc variable ($2b from one calling routine)
                    clc		; clear carry flag for addition
                    adc #$01	; A=A+1
                    sta $06	; LSB of pointer (e.g. $2c)
                    ldx #$7f	; init byte counter
                    ldy #$3f	; init loop counter 
Lfa21               txa		; X->A
                    sta ($06),y	;   put A at pointer+Y
                    dex		;   decrement X
                    dey		;   decrement Y
                    bpl Lfa21	; loopback
                    lda #$80	; \
                    sta $06	; / LSB of pointer ($0780?)
                    ldy #$63	; $63 bytes in size ($0780-$07e3?)
                    jsr Sfb23	; fill RAM with sequential values
                    rts		; done
                    		;
				;***************************************
				; video-mode CCC button/switch handling?
				;***************************************
				;
Lfa32               lda #$e3	; \
                    sta $76	;  | 
                    lda #$06	;  | $76/$77 -> $06e3?
                    sta $77	; /
                    lda #$00	; \ init $43 var to 0 (most/least flag?)
                    sta $43	; /
                    lda #$ff	; \ init $72 var to $ff
                    sta $72	; /
                    lda #$f0	; %1111 0000
                    sta $4002	; write to PIA2-B: sel=[110]=D6=pin13=S4 = CCC ADVANCE button
                    lda #$02	; %0000 0010 isolate bit 1
                    and $4000	; read button CCC ADVANCE button via PIA2-A
                    bne Lfa74	; branch ahead if not pressed
				;
				; advance button pressed?
				;
                    clc		; clear carry flag
                    lda $fe	; get $fe var
                    beq Lfa58	; branch down if $fe=0
                    ldy $fc	;   get $fc var to Y
                    beq Lfa58	;   skip next opcode if $fc=0
                    sec		;     set carry flag
Lfa58               adc $fc	; A=A+ $fc var + carry flag
                    sta $6f	; store new A total to $6f var
                    sta $76	; ...and to $76 var
                    lda #$b1	; %1011 0001
                    sta $4002	; write to PIA2-B - sel=[101]=D5=S3 = most/least switch
                    lda #$02	; %0000 0010 isolate bit 1
                    and $4000	; read most/least switch position via PIA2-A
                    beq Lfa86	; if MOST (or LEAST, not sure which is closed...) branch down
                    lda #$00	;   \ A=0
                    sta $76	;   / init $76 var to $00 (LSB of pointer)
Lfa6e               lda #$ff	;   \ init $43 var to $ff (most/least flag?)
                    sta $43	;   /
                    bne Lfa86	;   always branch down
				;
				; (advance not pressed?)
				;
Lfa74               lda #$b1	; %1011 0001
                    sta $4002	; write to PIA2-B - sel=[101]=D5=S3 = most/least switch
                    lda #$02	; %0000 0010 isolate bit 1
                    and $4000	; read most/least switch position via PIA2-A
                    beq Lfa86	; if MOST (or LEAST, not sure which is closed...) branch down
                    lda #$80	;   \ init $76 to $80 (LSB of pointer)
                    sta $76	;   /
                    bne Lfa6e	;   always branch, back up
				;
				; (rejoin common code)
				;
Lfa86               ldy #$00	; init Y offset = 0
                    lda ($76),y	; get value at $76/$77 pointer
                    cmp #$63	; compare it to $63 (99)
                    bcc Lfa90	; if A < 99 skip next opcode
                    lda #$63	;   A=99
Lfa90               sta $24	; put A in $24 var
                    lda #$00	; \ init $86 var to 0 (CCC LED decimal point flag?)
                    sta $86	; /
                    lda #$43	; \ set offset for output
                    sta $26	; / output will be at $5e-$61 (CCC LEDs)
                    jsr See45	; convert binary number to decimal digits
				;
                    inc $77	; increment MSB of pointer
                    lda ($76),y	; get value at $76/$77 pointer (now incremented)
                    sta $24	; store it in $24 var
                    cmp #$40	; compare it to $40 (64)
                    bcc Lfab8	; branch if A < $40
                    lda $76	;   get MSB of pointer
                    cmp #$80	;   compare to $80
                    bcs Lfab8	;   branch down if carry set
                    lda $24	;   get variable $24 (selection?)
                    sec		;   set carry flag (get ready for subtraction w/carry)
                    sbc #$40	;   subtract $40 (64) from A
                    sta $24	;   store new total at $24 for conversion to digits
                    lda #$ff	;   \
                    sta $86	;   / init $86 var to $ff (CCC LED decimal point flag?)
				;
Lfab8               dec $77	; decrement MSB of pointer $77 (back to original?)
                    lda #$41	; \ 
                    sta $26	; / direct output to $5c-$5f: CCC LED digits
                    jsr See45	; convert 2-byte number to 4 digits
				;
				; supress leading 0s on CCC readouts
				;
                    lda $5e	; get left-most LED digits
                    bne Lfac9	; if non-zero digit, skip down
                    lda #$0e	;   [if digit 0] charcode for a blank digit
                    sta $5e	;   make digit blank (supress a leading 0)
Lfac9               lda $60	; get 3rd LED digit (leading digit of 2nd pair)
                    bne Lfad1	; skip ahead if not 0
                    lda #$0e	;   if digit 0, charcode for a blank digit
                    sta $60	;   make digit blank (supress leading 0)
				;
Lfad1               inc $76	; increment LSB of pointer
                    lda #$ff	; \ init $71 var to $ff
                    sta $71	; / 
                    lda #$03	; \ init $73 var to $03
                    sta $73	; /
                    lda $43	; get $43 var (most/least switch flag)
                    bne Lfaee	; branch forward if set to "MOST"
				;
				; switch on "LEAST" 
				;
                    dec $76	;   decrement $76 var
                    dec $76	;   ...twice
                    lda $76	;   get the twice-decremented $76 var
                    cmp #$fe	;   
                    beq Lfaf4	;   if $76 var = #$fe branch ahead
                    cmp #$7e	;   
                    beq Lfaf4	;   if $76 var = #$7e branch ahead
                    rts		;   done
				;
				; switch on "MOST"
				;
Lfaee               lda $76	; get $76 var
                    cmp #$e5	; compare to $e5
                    bcc Lfaf9	; branch ahead if $76 var < 229
Lfaf4               lda #$00	;   \ init $72 var to 0
                    sta $72	;   /
                    rts		;   done
				;
Lfaf9               lda #$f0	; %1111 0000
                    sta $4002	; write to PIA2-B: sel=[110]=D6=S4= ADVANCE button
                    lda #$02	; %0000 0010
                    and $4000	; read CCC ADVANCE button via PIA2-A
                    beq Lfb06	; skip next opcode if pressed
                    rts		;   not pressed, rts
Lfb06               lda $fe	; get $fe var
                    clc		; clear carry flag
                    beq Lfb10	; branch down if $fe = 0
                    ldy $fc	;   get $fc var to Y
                    beq Lfb10	;   branch down if $fc = 0
                    sec		;     set carry flag
Lfb10               adc $fc	; A=A+$fc + carry flag
                    adc #$01	; A=A+1
                    cmp $76	; compare to $76 var
                    bcc Lfaf4	; loopback if A < $76 var
                    rts		; done
                    		;
				;********************
				; FILL RANGE WITH 0s
				;********************
				; expects $06/$07 as pointer & Y offset from pointer
				; zeros out address range
				;
Sfb19               lda #$00	; A=0
Lfb1b               sta ($06),y	; zero out Yth address offset from $06/$07
                    dey		;   decrement counter
                    bne Lfb1b	; loopback until Y=0
                    sta ($06),y	; one last time when Y=0
                    rts		; done here!
				;
				;************************************
				; FILL RANGE WITH SEQUENTIAL NUMBERS
				;************************************
				;   expects $06/$07 as pointer to RAM
				;   and Y as number of bytes to fill
				;                    
Sfb23               tya		; Y->A
                    sta ($06),y	;   put A (Y) in RAM at $06/$07 pointer + Y
                    dey		;   decrement counter
                    bne Sfb23	; loopback til Y=0
                    tya		; Y->A
                    sta ($06),y	; one last time with Y=0
                    rts		; done here!
                    		;
				;**************************
				; RESTORE FACTORY SETTINGS
				;**************************
				;   (does not update checksum)
				;
Sfb2d               lda #$00	; \
                    sta $06	;  | $06/$07 point to $0300
                    lda #$03	;  |
                    sta $07	; /
                    ldy #$8b	; set size of range
                    jsr Sfb19	; zero out $0300-$038b
                    ldy #$1b	; init Y counter
Lfb3c               lda $ffac,y	; get Yth value in table of memory locations
                    tax		;   A -> X
                    lda $ffc8,y	;   get Yth value in table of factory settings
                    sta $0300,x	;   store setting at Xth location
                    dey		;   decrement counter
                    bne Lfb3c	; loopback until Y=0
                    rts		; done here!
				;
				;*******************************
				; set $0a var based on $78 var?
				; serial comm related?
				;*******************************
				;                  
Sfb4a               lda #$7b	; A=120 (%0111 1000)
                    ldx $78	; check variable $78
                    bne Lfb52	; if var $78 non-zero, skip down
                    lda #$73	;   A=115 (%0111 0011)
Lfb52               sta $0a	; store $7b or $73 in $0a var
                    rts		;
           			;
				;*********************************
				; CONVERT 3-DIGIT ENTRY TO BINARY
				;*********************************
				;   expects: 3-digit entry at $28-$2a
				;   returns: 2-byte value at $2b/$2c
				;
Sfb55               lda $28	; \ get 1st digit keyed in
                    sta $1b	; / copy to $1b
                    lda $29	; \ get 2nd digit keyed in
                    sta $1c	; / store to $1c
                    lda $2a	; \ get 3rd digit keyed in
                    sta $1d	; / store to $1d
                    jsr See9e	; convert value to binary and store at $2b/$2c
                    rts		; done here!
                    		;
				;********************
				; DISPLAY ERROR CODE
				;********************
				;  (on CCC LEDs)
				;
Sfb65               ldx #$0c	; \ 
                    stx $5e	; / put code for "E" on 1st CCC LED digit
                    inx		; X is now $0d, code for "r" on CCC LED
                    stx $5f	; \ put "r" code on 
                    stx $60	; / next two CCC LED digits
                    lda $6b	; get error code #
                    sta $61	; put it on last CCC LED digit [only supports single-digit error codes]
                    cmp #$05	; compare error code to 5
                    bcs Lfb79	; branch to RTS if >= 5 (do not immediately display errors > 4?)
                    jsr Se453	;   update LED displays
Lfb79               rts		; done here
                    		;
				;***************
				; wallbox comm?
				;***************
				;  [needs lots of attention]
				;
Lfb7a               jsr Sfb4a	; \ set A & $0a var based on $78 boolean (a PIA "base" value?)
                    sta $4000	; / write to PIA2-B ("ping" for a wallbox?)
                    lda $4000	; \  read PIA2-B
                    ora $4000	;  } (again?)
                    and #$10	; /  %0001 0000 isolate bit 4: wallbox serial in
                    bne Lfb99	; branch down if bit high
				;
				; wallbox signal is stuck high error
				;
                    jsr Sef4d	; 1ms delay
                    ldy #$06	; set error code 6: wallbox stuck high
Lfb8f               sty $6b	; error code storage variable
                    lda $35	; get service/normal mode boolean
                    bne Lfb98	; skip next opcode (and rts) if in service mode
                    jsr Sfb65	;   display the error code
Lfb98               rts		; done
				;
Lfb99               ldx #$1c	; init delay counter (28) \
Lfb9b               dex		;   decrement counter	   } 
                    bpl Lfb9b	; loopback for delay      /
                    lda $9d	; get variable $9d (?)
                    bne Lfba5	; skip next opcode if non-zero
                    jmp Lfc29	;   jump down a ways
Lfba5               lda $9f	; get variable $9f (?)
                    bne Lfc11	; 
                    lda $bf	; 
                    bpl Lfc11	; 
                    ldx #$1c	; set initial delay value
                    lda $b0	; get $b0 var (index of byte to send?)
                    bmi Lfc11	; branch way down if bit 7 of $b0 is set (nothing to send?)
                    beq Lfbb7	; skip next opcode if $b0=0
                    ldx #$0a	;   set different initial delay value
Lfbb7               jsr Sfca4	; transmit a byte to wallboxes
                    inc $b0	; increment index byte
                    lda $b2	; 
                    cmp $b0	;
                    bne Lfbe5	;
                    lda $b1	;
                    cmp #$02	;
                    beq Lfbdf	;
                    cmp #$62	;
                    bne Lfbd0	;
                    ldx #$00	;
                    stx $9e	;
				;
				; increment "event" table offset, wrapping if needed
				;
Lfbd0               ldx $0170	; get offset into "event" table?
                    stx $c8	; put it into variable $c8
                    inx		; increment table offset to next slot
                    cpx #$90	; did we reach end of table?
                    bcc Lfbdc	; if not, skip next opcode
                    ldx #$72	;   wrap back to start of table
Lfbdc               stx $0170	; put updated table offset back
				;
Lfbdf               lda #$80	; \
                    sta $9f	;  } set a couple variables to 128
                    sta $b0	; /
Lfbe5               lda $0a	; \  get $0a var (PIA base value)
                    ora #$04	;  } %0000 0100 set bit 2
                    sta $4000	; /  write to PIA2-A: wallbox serial out
Lfbec               lda $4000	; \ readback PIA2-A status
                    and $4000	; / (again?)(why?)
                    ldy #$05	; Y=5 (error code 5: wallbox stuck low)
                    and #$10	; %0001 0000 isolate bit 4: wallbox serial in
                    bne Lfb8f	; branch back if not 0
                    lda $6b	; get error code
                    cmp #$07	; compare to 7 
                    bcs Lfc10	; branch to rts if error code >= 7
                    cmp #$05	; compare to 5
                    bcc Lfc10	; branch to rts if error code < 5
                    lda $35	; check service/normal mode boolean
                    bne Lfc10	; branch to rts if in service mode
                    lda #$0e	;   \  (LED code for blank digit)
                    sta $5e	;    \  
                    sta $5f	;     } clear all 4 digits of the CCC LED display
                    sta $60	;    / 
                    sta $61	;   /
Lfc10               rts		; done
				;
				; [branched to from several places above]
				;
Lfc11               ldx #$37	; init X counter to $37
Lfc13               dex		; decrement X counter
                    bne Lfc3e	; if X not 0 branch down [into serial comms code]
                    lda #$80	; 
                    cmp $bf	;   
                    beq Lfc21	;
                    inx		;
                    stx $9d	;
                    sta $bf	;
Lfc21               lda #$00	;
                    sta $9f	;
                    dec $9d	;
                    bpl Lfc2d	;
				;
				; $9f was zero? (or other ways to get here)
				;
Lfc29               lda #$19	; \ put #$19 in $9d var
                    sta $9d	; / 
Lfc2d               lda $0a	; \  get sored PIA base state
                    ora #$04	;  } %0000 0100 set bit 2: wallbox serial out
                    sta $4000	; /  write to PIA2-A
                    lda $b0	; 
                    bpl Lfbec	;
                    jsr Sfd15	; [event table related]
                    jmp Lfbec	; jmp back up 
				;
Lfc3e               lda $4000	; \  get PIA2-A status
                    ora $4000	;  } again?
                    and #$10	; /  %0001 0000 isolate bit 4: wallbox serial in
                    bne Lfc13	;
                    ldy #$67	; delay loop counter
                    ldx $bf	; check $bf var(?)
                    bpl Lfc51	;
                    sta $bf	;
                    tax		;
Lfc51               dey		; \ delay loop
                    bpl Lfc51	; /
				;
				; recieve 1 byte via serial from wallbox controller?
				;
                    ldy #$08	; init counter (8 bits)
Lfc56               lda $4000	; \  get PIA2-A status
                    ora $4000	;  } again?
                    and #$10	; /  %0001 0000 isolate bit 4: wallbox serial in
                    adc #$f0	; add %1111 0000 (sets/clears carry based on serial bit?)
                    ror $c0,x	; rotate carry flag into byte in wallbox recieve buffer
                    lda #$2a	; \ init delay loop counter
                    sta $08	; /
Lfc66               dec $08	;   \ delay loop [need to add up cycles & estimate baud rate]
                    bpl Lfc66	;   /
                    nop		; (fine-tune delay by 2 cycles?)
                    dey		; decrement bit counter
                    bne Lfc56	; loopback for another bit until counter reaches 0
				;
                    inc $bf	; increment $bf (byte counter?)
                    lda $bf	; get it to A
                    cmp #$09	; rec'd 9 bytes yet?
                    bcc Lfc78	; branch of < 9 bytes 
                    dec $bf	; decrement $bf (byte counter?) (why?)
Lfc78               cmp #$03	; compare to 3
                    bcc Lfca1	;
                    cmp $c1	;
                    bne Lfca1	;
                    lda #$00	; \ reset $9d var to 0
                    sta $9d	; /
Lfc84               clc		; clear carry for addition
                    adc $c0,x	;   keep running sum of bytes
                    dex		;   next byte
                    bpl Lfc84	; loopback until counter wrap around
                    tax		; move sum to X
                    bne Lfc9d	; branch if sum is not 0 (checksum error?)
				;
				; checksum is OK
				;
                    jsr Sfe32	; take action based on recieved signal from wallbox
                    lda $c0	; get command code recieved from wallbox
                    cmp #$f0	; compare to $f0
                    bcs Lfc9d	; branch if command was >=$f0 (i.e. $fX)
                    lda #$02	;   \ store 2 in $b1 var
                    sta $b1	;   / 
                    jsr Lfcde	;  
Lfc9d               lda #$80	; \ store #$80 in $bf var
                    sta $bf	; / error return code? 
Lfca1               jmp Lfbe5	; loopback
                    		;
				;**************************
				; SEND BYTE TO WALLBOX(EN) 
				;**************************
				;  (only called from $fbb7)
				;  X: sets length of initial delay
				;  $b0: byte to send over wallbox serial line
				;  $0a: initial value of PIA2-B register
				;  $0c: loop constant to control baud rate
				;
Sfca4               dex		; \ brief delay based on calling X vlaue
                    bpl Sfca4	; / 
                    ldy #$44	; \ set baud rate timer to #$44 (2400 baud)
                    sty $0c	; / 
                    ldx $b0	; fetch $b0 to X (index of byte to transmit)
				; (continue into next subroutine)
				;
				;*******************************
				; OUTPUT BYTE OVER WALLBOX LINE
				;*******************************
				;  $0a   (initial value of PIA2-B register)
				;  $0c   a loop constant that sets the baud rate
				;  $b1,X is the byte to send out [only called w/X=0]
				;  this alternate entry only called by "InterROWEgator" code
				;
Sfcad               lda #$0a	; \
                    sta $0b	; / init counter to 10 (total # of bits, with start & stop bits)
                    lda $0a	; get initial value of PIA2-B
                    sec		; set carry flag (for the start bit)
                    bcs Lfcba	; branch into middle of loop to send the start bit
				;
				; top of 10-bit serial transmit loop
				;
Lfcb6               lda $0a	; fetch initial PIA2-B register value
                    bcs Lfcbc	; skip next opcode if carry flag set ("space")
Lfcba               ora #$04	; if carry is clear, raise bit 3 of PIA2-B 
Lfcbc               sta $4000	; output bit to "wallbox" serial (InterROWEgator connected to wallbox plug) 
                    bcs Lfcc1	; branch _to_next_instruction if carry set [an attmpt at timing adjustment?]
Lfcc1               nop		; no op (micro delay)
                    ldy $0c	; get delay loop (baud rate) parameter
                    nop		;   micro delay		; 2 cycles \
Lfcc5               dey		;   countdown		; 2 cycles  } 7 cycles per loop
                    bne Lfcc5	;   loopback until Y=0	; 3 cycles /
                    ror $b1,x	; rotate least signficant bit into the carry flag [X=0, from calling code]
                    dec $0b	; decrement bit counter
                    bne Lfcb6	; loopback until counter is 0 (start bit + 8 data bits + stop bit)
                    rol $b1,x	; rotate byte left (reason not understood)
                    rts		; 
				;
				;************************************
				; copy Y values from $06/$07 pointer?
				;************************************
				;   to table starting at $00b3
				;
Sfcd1               lda ($06),y	; get A from $06/$07 pointer offset by Y
                    sta $00b3,y	; put A in table  $00b3 offset by Y
                    dey		; decrement counter
                    bpl Sfcd1	; loopback until Y wraps past 0
                    rts		; done here!
                    		;
				;*****************************************
				; calculate a checksum for serial message?
				;*****************************************
				;  appears to use a 0-page table $b1-$b4? (serial out data?)
				;
				;
Lfcda		    lda #$02	; A=2 (alt entry) [never used?]
		    bne Lfce4	; always branch
				;
Lfcde               lda #$00	; A=0 (main entry) (1 jsr from wallbox code, 1 jmp)
                    beq Lfce4	; always branch over next opcode
				;
Lfce2               lda #$01	; A=1 (alt entry) (reached only by jmp below)
				;
Lfce4               clc		; clear carry flag
                    adc #$03	; A is now 3 or 4 (depending on entry point above)
                    sta $b2	; store as $b2 var
                    tax		; \ (all 3 registers now have same value: 3 or 4)
                    tay		; / 
                    dex		;
                    dex		; X is now 1 or 2
                    dey		; Y is now 2 or 3
                    lda #$00	; \
                    sta $b0	; / $b0 var & A to 0
Lfcf2               clc		; clear carry for addition
                    adc $b1,x	;   A = var at $b1+x [X was 1 or 2 before this loop]
                    dex		;   decrement X counter
                    bpl Lfcf2	; loopback until X wraps under 0
                    eor #$ff	; flip every bit of A	\
                    tax		; A->X			 } calculates 2's complement of A to X
                    inx		; increment X		/  (signed binary number, used as checksum?)
                    stx $b1,y	; store at $b1+y (one past $b1+x)
                    rts		;
				;
				;*******************************
				; put value into "event" table?
				;*******************************
				;   subroutine appears to take A as input
				;   A values used by calling locations include: 
				;     $01, $08, $40, $60, $61, $62, $7c, $e0 
				;
Sfcff               ldx $0171	; get offset to X register (table pointer)
                    sta $0100,x	; store A on page 1, offset by X
                    inx		; increment X offset
                    cpx #$90	; compare to end of table
                    bcc Lfd0c	; branch if not past end of table
                    ldx #$72	;   wrap back to start of table if needed
Lfd0c               cpx $0170	; compare X to $0170
                    beq Lfd14	; if equal branch to rts
                    stx $0171	;   otherwise store updated table index
Lfd14               rts		; done here!
				;
				;*****
				; ???
				;*****
				;
Sfd15               ldx $0170	; event table index?
                    cpx $0171	; compare to duplicate index?
                    beq Lfd29	; if they're equal, skip down
				;
				; if $0170 != $0171
				;
                    lda $0100,x	;   retrieve last value in event table (per $0170)
                    sta $b1	;   save it at $b1
                    cmp #$40	;   compare it to 64
                    bcs Lfd44	;   branch down if it is >= $40
                    jmp Lfcde	;   otherwise jmp (if A < $40)
				;
				; if $0170 == $0171
				;
Lfd29               ldx $5a	; get $5a var
                    beq Lfd3a	; branch down if 0
                    lda #$04	;   (table entry value) (magazine-related?)
                    dec $5a	;   decrement counter $5a
                    beq Lfd37	;   if counter 0 skip down
                    dec $5a	;     decrement counter $5a again
                    lda #$08	; \  
Lfd37               jsr Sfcff	; / put $#04 or $#08 into event table?
Lfd3a               lda $9e	; 
                    beq Lfd43	;
                    lda #$62	; \
                    jsr Sfcff	; /put #$62 into event table? (cancel-related?)
Lfd43               rts		;
				;
Lfd44               cmp #$60	; A = $60?
                    bcs Lfd4f	; branch forward if A >= $60
                    lda $9b	;   get $9b var
Lfd4a               sta $b3	;   copy it to $b3 var
                    jmp Lfce2	;   jump back up
				;
Lfd4f               cmp #$60	; A = $60?
                    bne Lfd6b	; if not, branch to next check...
                    lda #$00	;   \ A=0
                    sta $06	;   / set LSB of pointer to $00
                    ldy #$05	;   Y=5
                    lda $39	;   video mode boolean?
                    beq Lfd5f	;   if var $39 = 0, skip over next opcode
                    ldy #$07	;     Y=7
Lfd5f               sty $07	;   set pointer $06/$07 to page 5 or page 7
                    ldy #$06	;   set size to 6 for subroutine
                    jsr Sfcd1	;   copy 6 values from $0500 or $0700 to table 
                    lda #$07	;   A=7
                    jmp Lfce4	;   jump back up
				;
Lfd6b               cmp #$61	; A = $61?
                    bne Lfd7c	; if not, branch to next check...
                    lda $e5	;   get $e5 var
                    sta $b3	;   copy it to $b3 var
                    lda $ee	;   get $ee var
Lfd75               sta $b4	;   copy it to $b4 var
Lfd77               lda #$02	;   A=2
                    jmp Lfce4	;   jump back up
				;
Lfd7c               cmp #$7c	; A = $7c?
                    bne Lfd8c	; if not, branch to next check...
                    ldx #$00	;   LSB of pointer of 0 ($0300)
Lfd82               ldy #$09	;   Y=9 number of bytes to copy (pricing settings)
                    jsr Sfde3	;   copy Y bytes from page 3 to page 0 area
Lfd87               lda #$0a	;   A=$0a
                    jmp Lfce4	;   jump back up
				;
Lfd8c               ldx #$0a	; X=$0a (LSB -> $030a video pricing)
                    cmp #$7d	; A = $7d?
                    beq Lfd82	;   if so, branch back
                    cmp #$7e	; A = $7e?
                    bne Lfdce	; if not, branch down to next check...
				;
                    ldy #$03	; Y=3 number of bytes to copy
                    ldx #$1c	; LSB of pointer: $031c (several unrelated settings)
                    jsr Sfde3	; copy Y bytes from page 3 to $b3-$b6
                    lda $031b	; get freeplay setting (255 or 0)
                    sta $b3	; copy it to $b3
                    lda $fc	; \ get $fc var
                    sta $b7	; / copy if to $b7 var
                    lda $fe	; \ get $fe var
                    sta $b8	; / copy it to $b8 var
                    lda #$09	; \ init temp var $06 to 9
                    sta $06	; /
                    ldx #$07	; init X loop counter
Lfdb0               ldy #$ff	; init Y at $ff
                    lda $0366,x	;   get MSB value from video lockout table
                    beq Lfdc0	;   if empty, skip ahead
                    dex		;   move to LSB
                    lda $0366,x	;   get value from video lockout table
                    sec		;   (set carry flag for subtraction)
                    sbc #$2c	;   subtract 44 (?) from value
                    inx		;   back to MSB?
                    tay		;   copy A to Y
Lfdc0               tya		;   copy Y to A ($ff if slot empty, otherwise a value)
                    ldy $06	;   get $06 temp var (ref. near $fdb0, starts at 9)
                    sta $00b3,y	;   put value in table: $00b3-$00bc?
                    dec $06	;   countdown $06 temp var
                    dex		;   \ move X to next lock entry
                    dex		;   / 
                    bpl Lfdb0	; if X doesn't roll under, loopback for more video lockout entries
                    bmi Lfd87	; otherwise, loop back further?
				;
Lfdce               cmp #$7f	; A = $7f?
                    bne Lfded	; if not, branch to more checks...
                    ldy #$09	;   Y=9 (number of bytes to copy)
                    ldx #$2d	;   LSB of pointer; $032d
                    jsr Sfde3	;   finish setting up and do RAM copy
                    ldy #$04	;   Y=4 (number of bytes to copy)
                    ldx #$25	; LSB of pointer; $0325 (wallbox related?)
                    jsr Sfde3	; finish setting up and do RAM copy
                    jmp Lfd87	; jump back up to (?)
				;
				;********************************
				; COPY BYTES FROM PAGE 3 TO $B3-
				;********************************
				;   X: LSB of source pointer (start point)
				;   Y: number of bytes to copy (3, 4 & 9 are used)
				;   (source ptr MSB fixed to $03; RAM page 3)
				;   dest: $00b3 to $00b3+Y
				;
Sfde3               stx $06	; set LSB of pointer from X
                    lda #$03	; \
                    sta $07	; / pointer MSB (programmable settings page)
                    jsr Sfcd1	; RAM copy Y bytes from $06/$07 pointer to $00b3-
                    rts		; done
                    		;
				;*****
				; ???
				;*****
				; takes action based on contents of A
				; (commands? modes? $62, $e0, $e1, $e2)
				; (continued from above)
				; [needs lots of attention]
				;
Lfded               cmp #$62	; A = $62? 
                    bne Lfe0f	; if not, branch down to next check...
                    lda $9e	;   get $9e var
                    ldx $c9	;   get selection # to be played(?)
                    stx $b3	;   copy it to $b3 var
                    sta $b4	;   copy $9e var to $b4 var
                    cpx $ca	;   compare $c9 var to 202(?)
                    bne Lfe0c	;   if not equal, skip rest of this bit...
                    ora #$f0	;     %1111 0000 set high 4 bits?
                    ora $032a	;     OR with memory loc. 42 [disable early cancel of fill-in?]
                    sta $b4	;     store as $b4 var?
                    cmp #$fd	;     A = $fd?
                    bcc Lfe0c	;     if A < $fd(?) branch down
                    ldx #$ff	;       \ set $ca var to $ff
                    stx $ca	;       /
Lfe0c               jmp Lfd77	;   jump back up
				;
Lfe0f               cmp #$e0	; A = $e0?
                    bne Lfe18	; if not, branch to next check...
                    lda $a0	;   get $a0 var
                    jmp Lfd4a	;   jump back up
Lfe18               cmp #$e1	; A = $e1?
                    bne Lfe26	; if not, branch to next check...
                    lda $68	;   get PIA1-A status
                    sta $b3	;   copy it to $b3(?)
                    lda $2002	;   get PIA1-B output status
                    jmp Lfd75	;   jump back up
				;
Lfe26               cmp #$e2	; A = $e2?
                    bne Lfe31	; if not, branch to rts...
                    ldy #$00	;   \
                    lda ($c2),y	;   / get A from $c2/$c3 pointer (0 offset)
                    jmp Lfd4a	;   jump back up
Lfe31               rts		; done
                    		;
				;***********************************
				; take action based wallbox command?
				;***********************************
				;  expects a code/command at $c0
				;  may have additional parameters ($c1-$c8?)
				;  only called from 1 place; near $fc9d
				;
Sfe32               lda $c0	; command code from wallbox
                    cmp #$10	; check for command code $10
                    bne Lfe3e	; if not, skip ahead to next check...
				;
				; command code $10?
				; 
                    lda $c8	; get $c8 var (part of message from wallbox?)
                    sta $0170	; update $0170 table pointer
                    rts		; done
				;
Lfe3e               ldx #$ec	; point to "records w/popular" counter
                    ldy #$00	; boolean flag
                    cmp #$50	; check for command code $50
                    beq Lfe4e	; if so, branch ahead to increment RECORDS (vice videos) counters
                    cmp #$52	; check for command code $52
                    bne Lfe59	; if not, skip ahead to next check...
				;
				; command codes $52 and $50
				;
                    ldx #$e6	; point to "total videos w/POPULAR" counter
                    ldy #$ff	; boolean flag [and conitinue into code below...]
				; (command code $50 joins here)
Lfe4e               lda $3b	; get record/video mode variable
                    pha		; push it to the stack
                    sty $3b	; set $3b boolean to $ff (video mode?)
                    jsr Sf18d	; increment "videos w/POPULAR counter" counter
                    jmp Lfe6f   ; finish in common code below
				;
Lfe59               cmp #$54	; check for command code $54
                    beq Lfe63	; if so, jump part way into $58 code below
				;
                    cmp #$58	; check for command code $58
                    bne Lfe83	; if not, skip ahead to next check...
				;
				; command codes $58 (play a video?) and $54 (play a record?)
				;
                    ldy #$ff	; boolean flag #$ff in Y
				; (command code $54 joins here)
Lfe63               lda $3b	; get $3b var to A (records/video mode?)
                    pha		; push A ($3b var) to stack
                    sty $3b	; store boolean flag in $3b var
                    lda $c2	; get $c2 var
                    sta $2c	; copy it to $2c var
                    jsr Sf8b1	; update MEMOREC plays tables
Lfe6f               ldx #$ee	; point to total records counter
                    lda $3b	; fetch $3b var
                    beq Lfe77	; skip next opcode if 0 (records mode)
                    ldx #$e8	;   point to total videos 2-byte counter instead
Lfe77               jsr Sf18d	; increment total records (or videos) counter (0-9999)
                    ldx $c2	; get $c2 to X [selection # sent from wallbox]
                    jsr Sf1b2	; add X to the selection list
                    pla		; pull A (former $3b var)
                    sta $3b	; return prior to $3b var
                    rts		; done 
				;
Lfe83               cmp #$59	; check for command code $59 
                    bne Lfea8	; if not, skip ahead to next check...
				;
				; command code $59 (money in a wallbox)
				;
                    lda $c2	; get $c2 var to A (to be added to wallbox money counter)
                    ldx $02f1	; get $02f1 (MSB) to X
                    clc		; clear carry flag
                    adc $02f0	; A=A+$02f0 (LSB)
                    bcc Lfe93	; if carry stayed clear, branch over next opcode
                    inx		;   increment X (MSB)
Lfe93               cpx #$27	; compare X (MSB) to $27 (max for 9999 decimal)
                    bcc Lfea1	; if A < 27 branch down
                    bne Lfe9d	; if A != 27 branch different
                    cmp #$0f	; compare A (LSB) to $0f (max for 9999 decimal)
                    bcc Lfea1	; if A < $0f branch down
Lfe9d               sbc #$0f	; A = A - $0f
                    ldx #$00	; \  re-init
Lfea1               stx $02f1	;  | $02f0/$02f1
                    sta $02f0	; /  to 0s
                    rts		; done
				;
Lfea8               cmp #$25	; check for command code $25
                    bne Lfec1	; if not, skip ahead to next check...
				;
				; put $7c, $7d, $7e, $7f, $60, $40 into event table
				;
Sfeac               lda #$7c	; [alt. entry point; used once]
Lfeae               jsr Sfcff	; put #$7c value into event table?
                    clc		;   clear carry flag for addition
                    adc #$01	;   A=A+1
                    bpl Lfeae	; loopback A<$80 (puts #$7d, #$7e, #$7f into event table)
                    lda #$60	; \	
                    jsr Sfcff	; / put #$60 into event table (selection-related?)
                    lda #$40	; \
Lfebd               jsr Sfcff	; / put #$40 into event table (idle-related?)
                    rts		; done 
				;
Lfec1               cmp #$f0	; check for command code $f0
                    bne Lfed3	; if not, skip ahead to next check...
				;
				; command code $f0
				;
                    lda #$02	; A=2
                    sta $07	; update var $07
                    jsr Sf9d6	; zero out money & play counters
                    jsr Sf4b7	; clear regular selection list
                    jsr Se749	; clear credits (and some other stuff)
                    rts		; done
				;
Lfed3               cmp #$f1	; check for command code $f1
                    bne Lfede	; if not, skip ahead to next check...
				;
				; command code $f1
				;
                    ldy #$00	; Y=0
                    lda $c4	; get $c4 var
                    sta ($c2),y	; put at location pointed to by $c2/c3
                    rts		; done
				;
Lfede               cmp #$f2	; check for command code $f2
                    bne Lff02	; if not, skip ahead to next check...
				;
				; command code $f2
				;
Lfee2               jsr Sf21b	;   read keypad
                    lda $36	;     get keycode
                    bmi Lfee2	;   tight loopback until any key is pressed
                    cmp #$0a	;   was the keypress "RESET"?
                    bcc Lfef4	;   if not, branch?
                    clc		;     clear carry flag
                    beq Lfef2	;     branch if 0 key pressed?
                    adc #$01	;       A=A+1
Lfef2               adc #$03	;     A=A+3
Lfef4               jsr Sff37	;   put A digit on all LEDs (using most of clear LED code)
                    jsr Se453	;   update LED display controllers
Lfefa               jsr Sf21b	;   read keypad			\
                    lda $47	;     check debounce counter     } wait for any keypress
                    bne Lfefa	;   loopback until debounced	/
                    rts		;   done
				;
Lff02               cmp #$f3	; check for command code $f3
                    bne Lff10	; if not, skip to next check...
				;
				; command code $f3
				;
Lff06               ldy #$10	; set loop counter to do all 16 LEDs 
                    jsr Se435	; put 888s on all LEDs
                    dec $c2	; decrement $c2 counter [part of message from wallbox]
                    bne Lff06	; loopback untip $c2 var is 0
                    rts		; done
				;
Lff10               cmp #$f4	; check for command code $f4
                    bne Lff18	; if not, skip to next check...
				;
				; command code $f4
				;
                    lda #$e1	;   \
                    bne Lfebd	;   / (unconditional) put $e1 into event table & rts
				;
Lff18               cmp #$f5	; check for command code $f5
                    bne Lff21	; if not, skip to next check...
				;
				; command code $f5 (turn off turntable motor?)
				;
                    lda #$00	;   \
                    sta $f3	;   / $f3 var = 0 (turntable motor boolean?)
                    rts		;   done
				;
Lff21               cmp #$f6	; check for command code $f6
                    bne Lff29	; if not, skip to next check...
				;
				; command code $f6
				;
                    lda #$e2	;   \
                    bne Lfebd	;   / branch back to put $e2 into event table & rts
				;
Lff29               cmp #$f7	; check for command code $f7
                    bne Lff30	; if not, skip to rts (no more valid command codes)
				;
				; command code $f7 (update RAM checksum)
				;
                    jsr Sef18	; update RAM checksum
Lff30               rts		; done
				;
                    		;************************
				; CLEAR TOP LED DISPLAYS
				;************************
				;
Sff31               lda #$0e	; blank digit code 
                    bne Lff3f	; unconditional branch, skips clearing the CCC LEDs
				;
				;************************
				; CLEAR ALL LED DISPLAYS
				;************************
				;   both top and CCC LEDs
				;   (clears data from RAM only
				;   does not send bits to LED controllers)
				;   (called from 3 places)
				;
Sff35               lda #$0e	; A=$0e [blank digit code for LEDs]
Sff37               sta $5e	; \
                    sta $5f	;  | all 4 CCC digits
                    sta $60	;  |
                    sta $61	; /
Lff3f               sta $28	; \ 
                    sta $29	;  } 3 top "SELECTION BEING MADE" digits
                    sta $2a	; / 
                    ldx #$0b	; init counter/offset to 11
Lff47               sta $8b,x	; loop to init RAM variables to $0e
                    dex		;   blank the remaining top digits
                    bpl Lff47	; loopback until X wraps
                    rts		; 
				;
				;************
				; DETENT OFF
				;************
				; 
Sff4d               lda $2002	; get PIA1-B
                    and #$fd	; clear bit 1 %1111 1101 [output low; inverted; inactive/high oupout]
                    sta $2002	; output to PIA1-B
                    lda #$12	; 
                    sta $63	; set detent counter/timer
                    rts		; 
                    		;
				;***********
				; DETENT ON
				;***********
				; 
Sff5a               lda $2002	; get PIA1-B
                    ora #$02	; set bit 1 %0000 0010 [output high; inverted; active low output]
                    sta $2002	; output to PIA1-B
                    lda #$12	; 
                    sta $63	; set detent counter/timer
                    rts		; 
                    		;
				;**********************
				; INITIALIZE VARIABLES
				;**********************
				; zeros much of page 0, inits some variables
				; only called at boot time
				;
Sff67               lda #$00	; \
                    ldx #$af	;  \
Lff6b               sta $00,x	;   } zeros out $00-$af
                    dex		;  /
                    bne Lff6b	; /
                    sta $00,x	; zero out one last byte [suboptimal loop design?]
                    ldy #$09	; \
Lff74               lda $ff98,y	;  \
                    tax		;   \  init 10 variables
                    lda $ffa2,y	;    } on zero-page RAM 
                    sta $00,x	;   /  with table values
                    dey		;  /
                    bne Lff74	; /
                    lda $0321	; get autoplay time setting (in minutes)
                    sta $fa	; init the autoplay timer
                    rts		; 
				;
				;**********************************
				; DATA TABLE: READING MONEY INPUTS
				;**********************************
				; list of values used to read money inputs
				;
Lff86               00 35 39 3d 31 3f
				; 
				; 3f= 0011 1111 [111] selects D7 (dollar bill reader)
				; 31= 0011 0001 [000] selects D0 (50-cent)
				; 3d= 0011 1101 [110] selects D3 (25-cent)
				; 39= 0011 1001 [100] selects D2 (10-cent)
				; 35= 0011 0101 [010] selects D1 (5-cent)
				;
				;****************************
				; DATA TABLE: READING KEYPAD
				;****************************
				; ref. $f221, selects each keypad input via PIA2-B
				;
Lff8c		    7d 7c 78 74 7f 73 72 76 7a 79 75 71
				; 
				;     rrxx sssr
				; 7d= 0111 1101 selects (011) D3 rtn, (100) S4, "0" key
				; 7c= 0111 1100 selects (010) D2 rtn, (100) S4, "1" key
				; 78= 0111 1000 selects (010) D2 rtn, (101) S5, "2" key
				; 74= 0111 0100 selects (010) D2 rtn, (110) S6, "3" key
				; 7f= 0111 1111 selects (011) D3 rtn, (000) S0, "4" key
				; 73= 0111 0011 selects (011) D3 rtn, (011) S3, "5" key
				; 72= 0111 0010 selects (010) D2 rtn, (011) S3, "6" key
				; 76= 0111 0110 selects (010) D2 rtn, (010) S2, "7" key
				; 7a= 0111 1010 selects (010) D2 rtn, (001) S1, "8" key
				; 79= 0111 1001 selects (011) D3 rtn, (101) S5, "9" key
				; 75= 0111 0101 selects (011) D3 rtn, (110) S6, "RESET" key
				; 71= 0111 0001 selects (011) D3 rtn, (111) S7, "POPULAR" key
				;
				;*************************************
				; DATA TABLE: VARIABLE INITILIZATIONS
				;*************************************
				;
Lff98               00 3a 42 62 64 69 74 83 b0 bf	; list of zero-page addresses
Lffa2		    00 12 0b 3c ff 08 64 3c 80 80	; list of initial values
				;
				; location $3a (init $12) is a timer for pulsing the coin-counter
				; location $42 (init $0b) is a counter/timer for the transfer mechanism
				; location $62 (init $3c) is a seconds countdown timer
				; location $64 (init $ff) (unknown use)
				; location $69 (init $08) (unknown use, possibly video related)
				; location $74 (init $64) (unknown use, possibly magazine/opto related)
				; location $83 (init $3c) is a seconds countdown timer used for autoplay mode
				; location $b0 (init $80) (unknown use)
				; location $bf (init $80) (unknown use)
				;
				;******************************
				; DATA TABLE: FACTORY SETTINGS
				;******************************
				;
Lffac		    00 00 01 02 03 04 05 06 08 09 0a 0b 0e 0f 10 13	; offsets into $0300
                    14 15 16 17 18 19 1c 1e 20 21 2b 39 		; (programmable memory)
				;	
Lffc8		    00 05 0a 0f 14 64 01 02 05 1e 0a 14 64 01 02 0a	; values copied to RAM
		    01 02 05 0a 14 05 ff ff 02 14 1e ff 		; (factory settings for programmable features)
				;
				;*******************************
				; DATA TABLE: COUNTER LOCATIONS
				;*******************************
				; table of offsets into $0200
				;
Lffe4               ec ee e6 e8 ea d8 da dc de e0 e2 e4 f0 fe
				;
				; code loc    function 
				; 500: 02ec = total record selections w/popular button
				; 501: 02ee = total record selections
				; 502: 02e6 = total video selections w/popular button
				; 503: 02e8 = total video selections
				; 504: 02ea = total autoplay
				; 505: 02d8 = total nickels
				; 506: 02da = total dimes
				; 507: 02dc = total quarters
				; 508: 02de = total 50c coins
				; 509: 02e0 = total dollar bills
				; 510: 02e2 = total $5 bills
				; 511: 02e4 = total money (in nickels)
				; 512: 02f0 = total wallbox money (in nickels?)
				; 513: 02fe = total money (not resettable) (nickels?)
				;
Lfff2                     ff ff ff ff ff ff ff ff			; [8 extra/unused bytes]
				;
Lfffa		                                  f2 e1 f2 e1 f2 e1 	; 6502 vectors
