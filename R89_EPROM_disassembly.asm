;*****************************************************************************
;* Rowe R-89 Jukebox CCC 6502 code
;*   original presumably written by an employee or
;*   contractor of Rowe International, circa 1984
;*
;* NO copyright notice was found on PROM label, nor in embedded in code, nor
;* displayed on, in, or by the jukebox.  Nonetheless, Rowe or its successor(s)
;* *may* still hold copyright of the original binary file.
;*
;* comments by DarrenF, 2023-2026
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
;* NOTE: execution starts at $e1f2 (per the reset vector)
;*****************************************************************************
				;
Le000               ea		; Rowe appears to always start their PROMs with a single $ea (nop) for unknown reasons
Le001		    40		; 2nd byte ($e001 here) is apparently set to make the LSB of the sum of the entire PROM equal to 0
				;
				;****************************
				; "InterROWEgator" DATA DUMP
				;****************************
				;  activated by an "800/8XX" service code
				;  transfers data to a hand-held unit for operators
				;  (little information about them is available online)
				;  asynchronous serial, 2400, 4800 or 9600 baud (selectable)
				;  8 bit word, no parity, 1 stop bit (8-N-1)
				;  (communicated over the existing WALLBOX line)
				;
				;  uses temporary variable $0c (for baud rate delay timer control)
				;  uses $06/$07 as an address pointer
				;  uses $08 temp var first as a loop counter, then for a checksum
				;
				;  transmits (bytes of each page sent in reverse order): 
				;    $07e6-$7000, $06e6-$0600, $05e6-$0500, $04e6-$0400  
				;    $0380-$0300, $02f1-$02d8, a 1-byte checksum
				;  (note that bits are transmitted starting with the least significant bit)
				;
Se002               jsr Sfb4a	; set a base value for PIA2-A, A & var $0a (%0111 0011 or %0111 1011) based on $78 var
                    sta $4000	; put base value on PIA2-A
				;
				; set the baud rate
				;
                    lda $0337	; get RAM dump baud rate setting
                    ldx #$1f	; delay loop count for 4800 baud
                    cmp #$30	; is baud rate setting "48"? (for 4800)
                    beq Le019	; if so, skip ahead and begin
                    ldx #$44	; delay loop count for 2400 baud
                    cmp #$60	; is baud rate setting "96"? (for 9600 baud)
                    bne Le019   ; if NOT, skip ahead with the default 2400 baud setting
                    ldx #$0c	; if SO, use delay loop count for 9600 baud
Le019               stx $0c	; store delay constant (based on baud rate setting) as a temporary variable
				;
				; set pointer and a short delay
				;
                    lda #$07	; \ MSB of $06/$07 address pointer 
                    sta $07	; / set for page 7 of RAM
                    sta $08	; also use 7 to init a delay loop counter
Le021               jsr Se3dc	; \  10ms delay (returns with X=0)
                    dec $08	;  } loopback for 80ms total delay (leaves $08 as 0)
                    bne Le021	; /  
                    stx $06	; set LSB of $06/$07 pointer (X=0 was returned above)
				;
				; top of outer loop (pages)
				; adjust pointer & bytes per page page
				;
Le02a               ldy #$e6	; default to transmit (230+1) bytes (for all but pages 2 & 3)
                    cmp #$03	; are we on page 3?
                    bne Le034	; if NOT skip ahead; on page 3 (operator settings) then:
                    ldy #$80	;   transmit fewer bytes of page 3 (128+1)
                    bne Le03e	;   (unconditional branch to inner loop)
Le034               cmp #$02	; on page 2?
                    bne Le03e	; if NOT skip to inner loop; if on page 2 (counters) then:
                    ldy #$19	;   only transmit 25(+1) bytes of page 2
                    lda #$d8	;   \ also, for page 2 use pointer base of $02d8
                    sta $06	;   / (set LSB of pointer)
				;
				; inner loop (bytes on a given page)
				;
Le03e               tya		; \ put Y (byte offset/counter) onto the stack for a moment
                    pha		; / so Y can be used for baud rate delay constant below
                    lda ($06),y	; get byte from RAM based on pointer and Y-offset
                    sta $b1	; put the byte to transmit in serial output buffer at $b1
                    clc		; clear carry for addition \
                    adc $08	; add this value to a       } keep a checksum of all bytes transmitted
                    sta $08	; running 1-byte checksum  /
Le049               ldy $0c	; get baud-rate delay constant [redundant; Sfcad fetches it again]
                    ldx #$14  	; \  20 loops
Le04d               dex		;  } small delay loopback (X=0 when done) 
                    bne Le04d	; /
                    jsr Sfcad	; transmit byte (from buffer at $b1) via serial (at selected baud rate)
                    pla		; \ retrieve byte offset/counter from stack
                    tay		; / 
                    lda $07	; \ did we just finish "page 1" (the checksum byte)?
                    cmp #$01	; /
                    bne Le069	; more pages to do, branch ahead
				;
				; finish up and exit subroutine
				;
                    jsr Se3dc	; \ 20ms delay
                    jsr Se3dc	; / 
                    lda $0a	; \  retrieve PIA-2B base value
                    ora #$04	;  } %0000 0100 set bit 2
                    sta $4000	; /  leave output line high (quiescent)
                    rts		; 
				;
				; bottom part of inner loop
				; 
Le069               dey		; decrement byte offset/counter
                    cpy #$ff	; \ done with this page? (did Y wrap around?) 
                    bne Le03e	; / if not, loopback for next byte
				;
				; bottom part of outer loop
				;		
                    dec $07	; decrement to next page
                    lda $07	; \ check new page
                    cmp #$01	; / have we reached page 1?
                    bne Le02a	; if not, simply loopback to do next page
				;
				; instead of doing page 1:
				;
                    lda $08	; fetch checksum value
                    eor #$ff	; \ 2's complement: convert it to signed binary number
                    adc #$01	; /
                    sta $b1	; store checksum in output buffer
                    jmp Le049	; jump to transmit the checksum byte and finish
				;
				; NOTE:
				;  This subroutine contains a bug that was fixed in later versions.
				;  There is a 'pla' in the reused code for the last byte that causes
				;  a stack underflow condition.
				;  Current priority is very low; likely no one still uses an InterROWEgator.
				;
				;***********************************
				; RECEIVE MESSAGE FROM VIDEO SYSTEM
				;***********************************
				;  called from 1 place (near Se910)
				;  receives up to 6(???) byte message, including a checksum byte
				;    byte 0: command code (length embedded as low 3 bits)
				;    byte 1 thru n-1: parameters
				;    byte n: checksum
				;    [all n bytes sum to 0, ignoring carry, for a correct checksum]
				;
				;  $07: temp var for length of message
				;  $08: temp var for return/error code
				;  $4d-$53: input buffer space
				;  X register : index into buffer space
				;  returns status in A and ZP var $08 (0=success, $80=fail???)
				;
Se081               ldy #$0f	; init counter to 15 
                    lda #$80	; \ set default return status code
                    sta $08	; / (no message or error = $80)
Le087               lda $4000	; get PIA2-A
                    dey		;   decrement counter
                    beq Le0a1	;   if counter is 0, bail (video system not ready?)
                    ora $4000	;   get PIA2-A (again? why???)
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    bne Le087	; loopback if bit 5 is high
                    lda #$7f	; \ %0111 1111
                    sta $4000	; / write to PIA2-A (high to video system?)
                    ldx #$00	; start index at beginning of input buffer
				;
				; top of byte loop - wait for start-bit transition
				;
Le09b               ldy #$3c	; init counter to 60
Le09d               lda $4000	; get PIA2-A 
                    dey		;   decrement counter
Le0a1               beq Le10f	;   if counter is 0 branch to near end (exit with $80, no message)
                    and $4000	;   get PIA2-A (again? why???)
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    beq Le09d	; loopback if bit is low
				;
				; start bit received, wait ~1.5 bit periods
				;
                    lda #$77	; \ %0111 0111
                    sta $4000	; / write to PIA2-A (low to video system???)
                    ldy #$32	; init loop counter (50) 
Le0b1               dey		; countdown (2 cycles) \ total = (50*5)+2+4+2= 258 cycles [seems a bit low]
                    bne Le0b1	; delay loop (3 cycles)/
				;
				; receive a byte from video system 
				; (895000 cycles/s)/(~185 cycles/bit) = ~4800 baud
				;
                    ldy #$08	; init counter (8 bits to get)
Le0b6               lda $4000	; get PIA2-A							(4 cycles)
                    and #$20	; %0010 0000 isolate bit 5 - data from video system		(2 cycles)
                    eor #$20	; flip bit 5							(2 cycles)
                    adc #$f0	; add #$f0; this sets/clears carry flag based on bit 5 of A	(2 cycles)
                    ror $4d,x	; rotate byte in buffer (carry flag goes to bit 7)		(6 cycles)
                    sec		; set carry flag						(2 cycles)
                    lda #$20	; (32)								(2 cycles)
Le0c4               adc #$fe	; \ $fe + $20 + carry flag = $1f (31)... I think	2*32 =	(64 cycles)
                    bne Le0c4	; / loopback until A=0					3*32 =	(96 cycles)
                    dey		;   decrement Y counter						(2 cycles)	
                    bne Le0b6	; loopback for next bit						(3 cycles)
				;
                    txa		; X->A (sets flags) X is index into receive buffer
                    bne Le0dc	; skip ahead if this wasn't the first byte recieved (byte 0)
				;
				; for first byte of message, extract the message length from command code
				;
                    lda #$07	; \ %0000 0111
                    sta $08	; / set $08 var and A
                    and $4d	; isolate low 3 bits of 1st byte recieved \ msg length encoded in low 3 bits of command
                    sta $07	; put length of message in $07 var	  /
                    bne Le0dc	; is value is non-zero skip ahead
                    lda #$07	;   \ if command suggests length is 0, 
                    sta $07	;   / use length of 7 instead
				;
				; bottom of outer (byte) loop
				;
Le0dc               inx		; increment index to next position in buffer
                    cpx $07	; did we just receive the last byte of this message?
                    bne Le09b	; if not, loopback for another byte
				;
				; got all the bytes; calculate a checksum
				;
                    dex		; decrement X index (it was incremented 1 past end)
                    lda #$00	; start with 0 in A
Le0e4               clc		; clear carry flag for addition
                    adc $4d,x	;   add Xth byte recieved from video system to a running sum 
                    dex		;   decrement byte index
                    bpl Le0e4	; loopback until all rec'd bytes are summed
                    and #$ff	; set flags based on checksum result
                    bne Le10f	; if nonzero, branch to near end (checksum bad; bail with error status)
				;
				; wait for ACK (???) from video system
				;
                    ldy #$3c	; init counter (60)
Le0f0               lda $4000	; read PIA2-A
                    dey		;   decrement loop counter
                    beq Le10f	;   if counter expired, branch down to exit subroutine with error status
                    and $4000	;   \ get PIA2-A
                    and #$20	;   / isolate bit 5 (%0010 0000) data from video system
                    beq Le0f0	; loopback if bit is 0
				;
                    ldy #$23	; \  35 loops
                    nop		;  \ 
Le100               dey		;  / 
                    bne Le100	; /  delay
				;
                    lda #$7f	; \ %0111 1111 
                    sta $4000	; / write to PIA2-A (high signal to video system)
				;
                    ldy #$23	; \  35 loops
Le10a               dey		;  } 
                    bne Le10a	; /  delay
				;
                    sty $08	; (Y now has 0) store at $08 to indicate sucess
				;
				; cases with no message or error status branch here
				;
Le10f               lda #$77	; \ %0111 0111 - low signal to video system
                    sta $4000	; / write to PIA2-A
                    lda $08	; get return status code to A
                    rts		; 
				; 
				;********************************
				; SEND A MESSAGE TO VIDEO SYSTEM
				;********************************
				;  called from 1 place in code, near $ea12
				;  uses $08 as temp var for error/exit code
				;  expects bytes to send in buffer at $79-$7f
				;  1st byte ($79) encodes total size of message in its low 3 bits
				;    theoretically, this could support a 4-byte message
				;    however, only 2- and 3-byte messages are found in this code
				; 
Se117               lda $4000	; \ read PIA2-A
                    and #$20	; / %0010 0000 isolate bit 5: data from video system
                    bne Le129	; branch down if high (video system ready???)
				;
				; video system not ready??? or using line? bail(?)
				; 
Le11e               lda #$77	; \ %0111 0111: bit 3, data to video system (low)
                    sta $4000	; / write to PIA2-A
                    lda #$40	; \
                    sta $08	; / store #$40 in $08 var (exit code for failure?)
                    bne Le19f	; (always) branch to near end of subroutine
				;
				; handshake with video system???
				;
Le129               lda #$7f	; \ %0111 1111: video system out bit (high)
                    sta $4000	; / write to PIA2-A
                    lda #$20	; %0010 0000 to mask bit 5
                    nop		; \ 4us pause
                    nop		; / 
                    and $4000	; read bit 5 of PIA2-A - data from video system
                    beq Le11e	; loopback if low (to bail)
				;
				; (alternate entry point)
				;
Se137               lda #$80	; \ store default exit code
                    sta $08	; /
                    ldy #$0f	; init loop counter to 15
Le13d               lda $4000	;   read PIA2-A
                    dey		;   decrement counter
                    beq Le1a3	;   if counter expired, exit loop
                    ora $4000	;   read PIA2-A (again? why???)
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    bne Le13d   ;   if bit high, loopback; else done looping
				;
                    lda $79	; get 1st byte of message buffer to video system
                    and #$07	; isolate low 3 bits (length encoded into command code)
                    sta $07	; store as temp var (length of message)
                    ldx #$00	; init buffer index to 0 (start at location $79)
				;
				; top of outer (byte) loop to transmit output buffer
				; to video system, using 8-N-1 serial
				;
Le152               ldy #$0a	; init bit loop counter (do 10 bits total per byte)
                    sec		; set carry flag (it will serve as the stop bit after going around)
                    bcs Le159	; always branch over next opcode (to send start bit of "space")
				;
				; send a byte to the video system, including start & stop bits
				; (~895000 cycles/s)/(~190 cycles/bit) = ~4800 baud hardcoded
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
                    rol $79,x	; rotate Xth data byte back left (return byte to original???)
                    inx		; move up to next byte (in $79-$7f buffer)
                    cpx $07	; are we up to # of bytes in message?
                    bne Le152	; if not, loopback for another byte
				;
				; check for (something???) from video system
				;
                    lda #$77	; \ %0111 0111 clear bit 3
                    sta $4000	; / write to PIA2-A: send 0 to video system
                    lda $4000	; \ read PIA2-A status back
                    and #$20	; / %0010 0000 isolate bit 5: data from video system
                    beq Le19b	; if bit 5 low, branch ahead to finish [with exit code $88]
				;
				; wait for acknowledgement from video system???
				;
                    ldy #$1e	; init loop counter to 30
                    lda #$00	; \ set $08 var to 0 [exit code for success?]
                    sta $08	; /
Le18c               lda $4000	; read PIA2-A
                    dey		; decrement loop counter
                    beq Le19b	; if counter 0, exit loop [setting exit code $88]
                    ora $4000	;   read PIA2-A again
                    and #$20	;   %0010 0000 isolate bit 5: data from video system
                    bne Le18c	;   if bit 5 high, loopback
                    beq Le19f	;   if bit low, exit loop [leaving exit code as $00]
Le19b               lda #$88	; \ reset $08 var to $88 [exit code indicating error???]
                    sta $08	; /
Le19f               lda #$00	; \ reset $78 var to 0 [flag indicating message rec'd???]
                    sta $78	; /
Le1a3               rts		; done
                  		;
				;*********************	
				; VERIFY ROM CHECKSUM
				;*********************
				;   all bytes of EPROM ($e000 to $ffff) should
				;   sum to (an LSB of) $00
				;   $e001 appears chosen to make it do so
				; 
Se1a4               lda #$e0	; \
                    sta $01	;  \ set pointer $00/$01 to $e000 (ROM base)
                    lda #$00	;  /
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
				; LOG VIDEO COMM BUFFER IN PAGE-1 TABLE
				;***************************************
				;  called from 2 places: $e91f and $ea37
				;  uses X (source buffer location) and A (command code)
				;    X-1 is a zero-page address base for up to 8 bytes (theoretically) of data
				;    (longest known video message is for command $16: 6 bytes long)
				;    called w/ X = $4d & $79: video in/out serial buffers
				;    A's low 3 bits form an offset into X-1
				;  uses $0133 as index into table at $0134 - $016c (56 bytes)
				;  no known use in firmware??? presumed only for debugging
				;  not understood why byte #1 is modified during input buffer logging???
				;
Se1b9               dex		; decrement X (point to flag/index byte before start of the buffer)
                    stx $06	; store X as LSB of a temp pointer (X is now either $4c or $78)
                    ldx #$00	; \ MSB of pointer = $00
                    stx $07	; / now $06/$07 -> $004c or $0078 (video input/output buffer)
                    and #$07	; %0000 0111: isolate low 3 bits of A (now 0-7) (length encoded in command code)
                    cmp #$02	; compare message length to 2
                    bcc Le1f1	; if <2 (1 or 0) branch to rts (no valid video messages are < 2 bytes long)
                    tay		; copy message length to Y
                    dey		; decrement Y counter (Y now 1-6: offset to last byte in message)
				;
				; top of a loop
				;
Le1c8               inc $0133	; \ increment table index and fetch it to X
                    ldx $0133	; / 
                    cpx #$6d	; end of table reached?
                    bcs Le1d6	; if so, branch down
                    cpx #$34	; compare table index to start of table
                    bcs Le1db	; index is in table, branch over next block
				;
				; end of table reached
				;
Le1d6               ldx #$34	; \ wrap table index back to      
                    stx $0133	; / the beginning
				;
Le1db               lda ($06),y	; fetch Yth byte from selected serial buffer
                    cpy #$01	; is this byte 1?
                    bne Le1eb	; if not 1, branch down
				;
				; when byte 1 is reached (why byte 1???)
				; this modifies byte 1 of the messages from the input buffer
				; not sure why??? would make more sense to modify byte 0 (command code)
				; to distinguish between identical in/out command code numbers
				;
                    ldy $06	; \ get LSB of pointer [$4c or $78]
                    cpy #$58	; / compare to $58
                    bcs Le1e9	; skip next opcode if copying output buffer
                    ora #$80	;   set bit 7 of byte (for input buffer byte 1)
Le1e9               ldy #$01	; return loop counter to 1 (since Y register was used)
				;
				; (common path)
				;
Le1eb               sta $0100,x	; put byte from serial buffer into table
                    dey		; next byte
                    bne Le1c8	; loopback until 0
Le1f1               rts		; done
				;
				;******************
				; POWER ON & RESET
				;******************
				;  boots the CCC at power-up and warm-reboots
				;
				;
				; basic 6502 startup tasks
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
				; set up I/O and check for factory test rig presence
				;
                    stx $4002	; %1111 1111 to PIA 2 Port B (all outputs high=off)
                    sta $4000	; put %0000 0100 onto PIA 2 Port A (high on PA2, wallbox serial)
                    lda #$00	; put %0000 0000 onto PIA 1 Port B
                    sta $2002	; (these are inverted to high, controls are active-low; so all OFF)
                    ldy #$03	; init Y counter (for entering factory test mode)
Le22f               ldx #$ef	; \ %1110 1111
                    stx $4002	; / byte to PIAB: all high except PB4 (top LED clock)
                    nop		; (propogation delay)
                    lda $4000	; \ read PIA 2 Port A
                    and #$40	; / %0100 0000 isolate PA6 input: unused line (CD input in later models)
                    bne Le255	; branch down if bit high (factory test equipment connected), otherwise...
				;
				; check ADVANCE button
				;
Le23c               lda $4000	; read PIA 2 Port A
                    and #$02	; isolate PA1 bit: input D7, S4 switch = ADVANCE button on CCC PCB
                    bne Le252	; branch if advance button is NOT pressed
				;
				; ADVANCE button IS pressed at boot; check the (CCC/MEMOREC) RESET button
				;
                    lda #$be	; \ %1011 1110
                    sta $4002	; / PIA2-B all high but PB0 and PB6 low, S3=input D4, S1 = RESET on CCC PCB 
                    lda $4000	; get PIA2-A input 
                    and #$02	; isolate PA1 bit: RESET button
                    bne Le252	; branch over next if reset button is NOT also pressed
                    jmp Le65e	;     both buttons pressed: "Err0" is triggered [A=0]
Le252               jmp Le5c6	; continue boot-up process (verify RAM/ROM checksums etc.)
				;
				; check for factory test rig connected boot time
				; (appears to require a connection between P1/pin3 & P5/pin10 to activate???)
				; 
Le255               ldx #$ff	; \ %1111 1111
                    stx $4002	; / set PIA 2 Port B: PB4 high (top LED clock)
                    nop		; (propogation delay)
                    lda $4000	; \ read PIA 2 Port A
                    and #$40	; / %0100 0000 isolate bit 6: unused line (CD player input on later models)
                    bne Le23c	; if high, branch back to main code
                    dey		;   decrement counter
                    bne Le22f	;   if counter not zero, back into regular routine
                    jmp Le29f	;     to factory system test code
				; 
				;***********************
				; LED CHARACTER SET DATA 
				;***********************                   
				;
Le268                                       77 41 3b 6b 4d 6e 7e 43 ; character sets for LEDs
                    7f 6f 08 2a 00 18 00 1e 77 14 6d 5d 1e 5b 7b 15 ; 2 sets of 16 characters
                    7f 5f 08 49 6b 28 00 2b                         ; used by code near $e4de
				;
				; 0-9 are digits
				; $0a = "-"
				; $0b = "=" [3 horizontal line programming prompt]
				; $0c = "E" [blank on top LEDs]
				; $0d = "r" 
				; $0e = " " [blank space]
				; $0f = "F" [no known use]
				;
				; mappings of LED segments to charset bits:
				;   top LEDs: MSB .cdegfab LSB (no DP exists on those LEDs)
				;   CCC LEDs: MSB .decgbfa LSB (DP does exist)
				;
				; standard LED segment names for reference:   
				;    aaaa
				;   f    b
				;   f    b
				;    gggg
				;   e    c
				;   e    c
				;    dddd  DP
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
				; 38          RAM address of data to control the LED indicators	
				;             ("MAKE A SELECTION" and "THANK YOU")
				;
				;**************************
				; FACTORY SYSTEM TEST MODE
				;**************************
				;  only reached by jmp from before data tables above
				;  requires certain pin connections to activate
				;
Le29f               lda #$02	; \ flashes "888" on LEDs, on and off twice (on/off/on/off)
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
                    jsr Sff35	;   clear LED displays
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
Le2d4               jsr Se3e5	;   wait for user to flip switch on test rig
                    lda $08	;   \ get temp var 
                    sta $2002	;   / write to PIA1-B
                    jsr Se3ed	;   wait for user to flip switch on test rig
                    lsr $08	;   moves the high bit to the right
                    bcc Le2d4	; loopback until high bit emerges to carry flag
				;
				; activate ALL mech outputs
				;
                    lda #$00	; \ %0000 0000
                    sta $2002	; / put value on PIA1-B (activate all mech outputs at once!)
                    jsr Se3e5	; wait for user to flip switch on test rig
				;
                    lda #$88	; \ %1000 1000
                    sta $4000	; / put value on PIA2-A (data to vid sys; unused output)
                    jsr Se3ed	; wait for user to flip switch on test rig
                    jsr Se3e5	; wait for user to flip switch on test rig again
				;
                    lda #$00	; \ %0000 0000
                    sta $4000	; / put value on PIA2-A
                    jsr Se3ed	; wait for user to flip switch on test rig
                    jsr Se3e5	; wait for user to flip switch on test rig again
				;
                    lda #$0c	; \ %0000 1100
                    sta $4000	; / put value on PIA2-A (wallbox and video serial???)
                    jsr Se3ed	; wait for user to flip switch on test rig
                    lda #$08	; \ %0000 1000
                    sta $4000	; / put value on PIA2-A
				;
				; activate 4 outputs on PIA2B in sequence
				;
                    lda #$08	; \ %0000 1000 [instruction seems redundant]
                    sta $08	; / init loop variable
Le312               jsr Se3e5	;   wait for user to flip switch on test rig
                    lda $08	;   get $08 back
                    eor #$ff	;   invert every bit of A [now %1111 0111]
                    sta $4002	;   write to PIA2-B
                    jsr Se3ed	;   wait for user to flip switch on test rig
                    lsr $08	;   moves high bit to the right
                    bcc Le312	; loopback until high bit emerges to carry flag
                    lda #$ff	; \ %1111 1111
                    sta $4002	; / write to PIA2-B (return to normal state)
				;
                    lda $4000	; \ read PIA2-A
                    and #$20	; / %0010 0000 isolate bit 5: data from video system
                    bne Le332	; if not 0, skip next opcode
                    jmp Le29f	;   restart factory test
				;
Le332               lda #$04	; %0000 0100 bit to check first???
                    sta $08	; store bit at $08 temp var
Le336               jsr Se3e5	; wait for user to flip switch on test rig
				;
Le339               lda $2000	; read PIA1-A
                    eor #$ff	;   invert all bits
                    and #$fc	;   (%1111 1100) isolate bits 2-7 (all signals from record mech)
                    cmp $08	;   compare to temp variable at $08
                    bne Le339	; loopback if not zero
                    jsr Se3ed	; wait for user to flip switch on test rig
				;
                    asl $08	; shift variable at $08 left 1 bit (to check next bit of input from mech)
                    bcc Le336	; loopback if we didn't shift the high bit out of A
                    jsr Se3e5	; wait for user to flip switch on test rig
				;
				; wait for activity on video system data line
				;
Le34e               lda $4000	; \ get PIA2-A
                    and #$20	; / (%0010 0000) isolate bit 5, video system data
                    bne Le34e	; loopback until bit low
				;
                    jsr Se3ed	; wait for user to flip switch on test rig
                    jsr Se3e5	; wait for user to flip switch on test rig
				;
				; wait for activity on wallbox line
				;
Le35b               lda $4000	; \ get PIA2-A
                    and #$10	; / %0001 0000 isolate bit 4: wallbox serial in
                    bne Le35b	; loopback
                    jsr Se3ed	; wait for user to flip switch on test rig
				;
                    lda #$f1	; \ %1111 0001 [all coin select bits low]
                    sta $08	; / store as $08 variable
Le369               jsr Se3e5	; wait for user to flip switch on test rig
				;
				; wait for a certain coin/bill input
				;
Le36c               lda $08	;   get $08 variable back (%1111 0001)  [$0.50 coin switch???]
                    sta $4002	;     store at PIA2-B
                    nop		;     pause
                    lda $4000	;     get PIA2-A
                    and #$01	;     (%0000 0001) isolate b0, coin inputs (multiplexed)
                    bne Le36c	;   loopback until low
				;
                    jsr Se3ed	;   wait for user to flip switch on test rig
				;
                    lda #$02	;   \
                    adc $08	;   / increase $08 variable by 2 (change select bits)
                    sta $08	;   store it back
                    bcc Le369	; outer loopback... until carry flag set???
                    ldy #$00	; init Y=0
Le386               jsr Se3e5	; wait for user to flip switch on test rig
				;
                    lda $e3c3,y	;   \ data table lookup of PIA settings to select inputs (keypad?)
                    sta $4002	;   / write PIA2-B to select input line
				;
Le38f               nop		;   micro-pause
                    lda $4000	;     get PIA2-A
                    and #$02	;     (%0000 0010) isolate multiplexed keypad/buttons to read
                    bne Le38f	;   loopback until selected keypad button is pressed
				;
                    jsr Se3ed	;   wait for user to flip switch on test rig
				;
                    iny		;   increment Y counter
                    cpy #$04	;   up to 4 yet?
                    bne Le386	; loopback if not
                    jsr Se3e5	; wait for user to flip switch on test rig
				;
                    ldx #$00	; \
                    stx $00	;  } init pointer $00/$01 -> $0000
                    stx $01	; /  
                    dex		; \ set flag to indicate return to finish factory test mode
                    stx $02	; /
                    jmp Le5d6	; jump into RAM test (we'll return by jmp based on $02 flag just set)
Le3ae               bcs Le3ae	; infinite loop if carry flag set (hang here on RAM test error)
                    jsr Se3ed	; wait for user to flip switch on test rig
				;
Le3b3               lda $4000	; get PIA2-A
                    and #$40	;   %0100 0000 isolate bit 6: unused input (used here for factory test mode)
                    bne Le3bd	;   if non-zero skip next opcode
                    jmp Le29f	;     jump to re-start factory test mode
Le3bd               jsr Se433	;   put 8888 on CCC LEDs
                    jmp Le3b3	; loopback
				;
				;*******************
				; PIA2-B DATA TABLE
				;*******************
				;  only used by factory test mode code
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
Se3c7               ldy #$14	; 20 * 100ms = 2s
                    bne Le3cd	; (unconditional branch)
				;
Se3cb               ldy #$05	; 5 * 100ms = 500ms = 0.5s
Le3cd               jsr Se3d4	; \  
                    dey		;  } 100ms delay 5 times
                    bne Le3cd	; /
                    rts		; done
				;
Se3d4               ldx #$64	; loop counter for 100ms delay
                    bne Le3de	; (unconditional branch to ms delay loop)
				;
Se3d8               ldx #$05	; loop counter for 5ms delay
                    bne Le3de	; (unconditional branch to ms delay loop)
				;
Se3dc               ldx #$0a	; loop counter for 10ms delay
				;
Le3de               jsr Sef4d	; \  do base 1ms delay loop subroutine
                    dex		;  } loop X times
                    bne Le3de	; /
                    rts		; done
                    		;
				;**********************
				; PAUSE FOR SIGNAL LOW
				;**********************
				;  only used in factory test mode code
				;  test rig attached to CD interface lines (pins 9 & 10) on P5???
				;
Se3e5               lda $4000	; \ PIA2-A inputs
                    and #$40	; / %0100 0000 isolate bit 6: input from CD player
                    bne Se3e5	; wait until bit goes low
                    rts		; 
				;
				;***********************
				; PAUSE FOR SIGNAL HIGH
				;***********************
				;  only used in factory test mode code
				;  test rig attached to CD interface lines (pins 9 & 10) on P5???
				;
Se3ed               lda $4002	; \  get PIA2-B settings
                    and #$ef	;  } %1110 1111 all select bits high
                    sta $4002	; / store PIA2-B outputs back
Le3f5               lda $4000	; \ get PIA2-A inputs
                    and #$40	; / %0100 0000 isolate bit 6: input from CD player
                    beq Le3f5	; wait until bit goes high
                    lda $4002	; \  get PIA2-B outputs
                    ora #$10	;  } %0001 0000 all select bits low
                    sta $4002	; /  store PIA2-B outputs back
                    rts		; done
				;
				;
Le405                              a6 0b f0 0c a0 00 91 00 c8 d0 fb	; un-reachable code, 29 bytes
Le410               e6 01 ca d0 f6 a6 0a f0 08 a0 00 91 00 c8 ca d0	; (thus not auto-disassembled)
Le420               fa 60
		;
		;   		unused code (disassembled just for reference/curiosity)
		;
Le405		;   ldx $0b	get zero-page variable
		;   beq Le415	skip ahead if it is 0
		;   ldy #$00	init y to 0
Le40b		;   sta ($00),y	\  
		;   iny		 > inner loop to fill a page of RAM
		;   bne Le40b	/
		;   inc $01	increment LSB to another page
		;   dex		decrement page counter
		;   bne Le40b   outer loop to do X pages
Le415		;   ldx $0a	get zero-page variable
		;   beq Le421	exit if it is 0
		;   ldy #$00
Le41b		;   sta ($00),y
		;   iny
		;   dex
		;   bne Le41b
Le421		;   rts
				;
				;*******************************
				; DATA TABLE: LED RAM LOCATIONS
				;*******************************
				;  only used by factory test mode code
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
				; PUT "8888" ON CCC LEDS
				;************************
				;  only used by factory test mode code
				;
Se433               ldy #$03	; init loop counter to Y=3 (alt. entry)
				;
				;***********************
				; PUT "888" ON ALL LEDS
				;***********************
				;  only used by factory test mode code & wallbox code $f3
				;  expects a value in Y 
				; 
Se435               jsr Sff35	; clear all LED display data (Y not used)
                    ldx $e422,y	;   get table value (from above)
                    lda #$0b	;   %0000 1011 (for case of X=$38) turn on both indicator lamps
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
				;*********************
				; UPDATE LED DISPLAYS
				;*********************
				;  update output to all LED display controllers
				;  per the contents of RAM
				;
Se453               lda #$00	; \ init location counter to 0
                    sta $98	; /
Le457               jsr Se45f	; process a digit of data to LED displays
                    lda $98	; get location counter
                    bne Le457	; loopback until it's zero again
                    rts		; done
				;
				;******************************
				; PROCESS DIGIT - LED DISPLAYS
				;******************************
				;  usually driven by parent loop above
				;    (called directly from 1 place; not fully understood,
				;    appears to clear the CCC LED display???)
				;  mystery: code related to the keypad, possible for a lamp (backlight?)
				;    activated by a keypress???
				;
Se45f               tya		; \
                    pha		; / put Y on stack
                    lda $4000	; \  read PIA2-A
                    and #$20	;  } %0010 0000 isolate bit 5: data from video system bit
                    sta $0d	; /  store temp var, used later as video comm activity indicator [ref. $e517]
                    lda #$0e	; \ %0000 1110 (bitstream used at end???)
                    sta $84	; / store to $84 
                    lda #$31	; \ %0011 0001: put both LED clocks low (hardware inverted)
                    sta $4002	; / write to PIA2-B
                    ldx $98	; get location counter variable to X
                    bne Le48d	; branch down if it's non-zero (0 on first time thru)
				;
				; first time through; set up
				; 
                    stx $99	; init $99 var to 0 (X must be 0 to get here)
                    lda #$19	; \ init the digit counter/offset (25 bytes to use)
                    sta $98	; /
				;
				; this loops appears to send (37) 0s to the LED controllers
				; to flush/sync them (in case they were waiting for more bits)
				;
                    ldx #$25	; set loop counter to 37 (bits to clock to LED controller)
Le47d               lda #$01	;   \ %0000 0001: clock H, data 0 to LEDs (CCC and top group 0?)
                    sta $4002	;   / write to PIA2-B
                    lda #$31	;   \  %0011 0001: clock L, data 0 to LEDs (CCC and top group 0?)
                    dex		;    } (decrement bit counter)
                    sta $4002	;   /  write to PIA2-B
                    bne Le47d	; loopback until X=0
                    jmp Le52c	; jump to very near end of subroutine
				;
				; after 1st time thru
				;
Le48d               lda #$20	; \ default charset offset of $20 for CCC LEDs 
                    sta $06	; /
                    lda #$e0	; %1110 0000 bitstream that might be needed
                    cpx #$18	; compare (counter var $98) to 24 (1st time here)
                    bne Le49a	; skip next opcode if not 1st time here
                    jmp Le529	;   if X=$18, jump to send %1110 0000 [0s + start bit + 2 n/c lines]
				;
				; select appropriate LED controller
				;
Le49a               ldx $99	; fetch (digit position)
                    cpx #$05	; compare (digit position) to 5
                    bcc Le4b5	; branch down if posisition < 5 (CCC LEDs; controller already selected)
				;  but for digits 5 and up (all top LEDs)
                    lda #$10	;   \
                    sta $06	;   / change charset offset to $10 for top LEDs
                    lda #$ff	;   %1111 1111 LED sel [00] select LED controller 0
                    cpx #$0b	;   compare digit position to 12
                    bcc Le4b2	;   branch down if < 12
                    lda #$fb	;     %1111 1011 LED sel [01] select LED controller 1
                    cpx #$11	;     compare digit position to 17
                    bcc Le4b2	;     branch down if < 17
                    lda #$f7	;       %1111 0111 LED sel [10] select LED controller 2
Le4b2               sta $4002	;   write to PIA2-B - sets select lines based on $99
				;
				; loop up and store the RAM location for this digit
				;
Le4b5               ldy $e288,x	; get 0-page RAM address from ROM table for current digit position (0-22)
				;
				; Le288 (for ref.)      /CCC  LEDs\ :: :: / TOP 
				;	                61 5f 5e 60 84(40)96 95 
				;		        00 01 02 03 04 05 06 07
				;	   	        [------------] [-------
				;
				;  LEDs\ :: :: /TOP LEDs \ :: :: /TOP LEDs \ vv- indicator lights
                		;  90 94 84(40)92 91 8c 8d 84(40)8e 28 29 2a 38  
				;  08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16
				;  -------] [---------------] [---------------]
				;
                    sty $0a	; store (RAM address for LED digit's value) to $0a 
				;
				; handle special start bytes; $40 (%0100 0000)
				;   (6) 0s + start bit (only 7 bits used on top LEDs) 
				;
                    tya		; Y->A (also store RAM address in A)
                    cpx #$05	; compare X to 5 ("digit" position)
                    beq Le527	; if == branch way down (straight to output start bit)
                    cpx #$0b	; compare X to 12 (again)
                    beq Le527	; if == branch way down (straight to output start bit)
                    cpx #$11	; compare X to 17 (again) 
                    beq Le527	; if == branch way down (straight to output start bit)
				;
                    lda $0000,y	; otherwise, get a digit charcode (Y is RAM address of current digit)
                    eor #$ff	; invert every bit of A (why? only used inverted for indicator lamps)
				;
				; special treatment for last byte
				;   ("THANK YOU" and "MAKE SELECTION" lights)
				;   (keypress related code NOT understood; possibly provision for a
				;    lighted keypad that activates when a key is pressed???)
				;
                    cpx #$16	; \ is this the last byte of display data?
                    bne Le4de	; / for all other bytes, skip this block of code
                    ora #$04	;   %0000 0100 set bit 2 of (inverted) A
                    ldx $9c	;   get (current keypress code) to X
                    bmi Le527	;   branch based on bit 7 of X (no kepress); go send the byte (not charcode)
                    and #$fb	;     clear bit 2 (%1111 1011) of (inverted) A [control bit for unused transistor???]
                    ldx #$ff	;     \
                    stx $9c	;     / store $ff as (keypress code)
                    bne Le527	;     unconditional branch; increment counter and send byte [not a charcode]
				;
Le4de               eor #$ff	; un-invert all bits of A (back to original)
                    sta $08	; store data byte to temp var $08
                    lda $06	; get charset offset ($20 or $10)
                    sec		; \ 
                    sbc #$10	; / (reduce $20 to $10, or $10 to $00)
                    ora $08	; OR with temp var $08 (potentially sets bit 4 to select 2nd charset)
                    tay		; A->Y copy updated output charcode to Y
                    lda $e268,y	; get LED-code from ROM table for charcode (Y should be $00-$1f)
                    ldy $0a	; get (RAM address for this digit's charcode) to Y
                    cpy #$96	; RAM address $96? (one of the top LED digits)
                    bne Le503	; if not skip to next check...
				;
				; special case: $96, 1s digit of "MOST POPULAR SELECTION" on top of juke
				; (this LED is wired differently for unknown reason - design error?)
				;
                    pha		; push LED code
                    and #$2f	; %0010 1111 isolate some bits [LED segments bafg and d]
                    sta $08	; store to $08 temp var
                    pla		; pull LED code back to A
                    and #$50	; %0101 0000 isolate some bits [LED segments c and e]
                    asl a	; \ shift A left
                    asl a	; / ...twice (bit 6 goes into carry bit)
                    bcc Le501	; skip next opcode if bit 6 was high
                    ora #$10	;   %0001 0000 set bit 5 of A [effectively swaps bits 4 & 6, segments c & e]
Le501               ora $08	; stored modified byte at $08 temp var 
				;
Le503               cpy #$5f	; is this the 2nd digit of the CCC LEDs?
                    beq Le521	; if so, branch down to special case for decimal point
				;
                    ldx $35	; check service/normal mode flag
                    bne Le527	; in service mode, branch down
				;
				; normal ("on") mode
				; this appears to use the CCC digits 3 & 4 as
				; video system comm activity indicators using
				;   "-" in digit 3 to indicate receiving data from video system
				;   "-" in digit 4 to incicate sending   data to   video system
				;
                    cpy #$61	; is this (rightmost CCC LED digit)?
                    bne Le517	; if not, branch down
                    ldy $78	;   get (flag for message to send to video system???)
                    beq Le527	;   branch ahead if 0
Le513               ora #$08	;     %0000 1000 set bit 3 of A to turn a blank in to "-" 
                    bne Le527	;     (always branch)
				;
Le517               cpy #$60	; is this (3rd CCC LED digit)
                    bne Le527	; if not, branch down
                    ldy $0d	;   get temp var (active data from video system???)
                    beq Le513	;   if temp var is 0, branch back up to make this a "-" 
                    bne Le527	;   (always) branch ahead
				;
				; special case: $5f - 2nd digit of CCC LEDs
				;
Le521               ldy $86	; check (CCC LED decimal point flag)
                    beq Le527	; if variable is 0, skip over next opcode
                    ora #$80	;   %1000 0000 set high bit of (light the decimal point)
				;
				; (common path)
				;
Le527               inc $99	; increment digit position counter
Le529               jsr Se531	; transmit out a byte (A) to LED controller
Le52c               pla		; \
                    tay		; / pull Y from stack (ref $e460)
                    dec $98	; decrement counter variable
                    rts		; done
				;
				;*****************************
				; SEND BYTE TO LED CONTROLLER
				;*****************************
				;   only called from above routine
				;   expects A as input (byte to send to LED controller)
				;   $06: selects 7 or 8 bit outut and charset 
				;     ($10 for 7 bits and top LED charset
				;      $20 for 8 bits and bottom LED charset)
				;
Se531               ldy $06	; get charset offset ($10 or $20) to Y
                    ldx $4002	; get initial PIA2-B status to X
                    cpy #$11	; compare charset offset to $11 (17) to set/clear carry flag
                    ldy #$07	; set bit counter to 7 bits (default for top LEDs)
                    bcc Le53e	; skip next opcode if Y < $11 ($10, top charset)
                    ldy #$08	;   bit counter to 8 bits [7 for $06=$10, 8 for $06=$20]
Le53e               sta $08	; store A at $08 (byte to output)
Le540               ror $08	;   rotate $08 right (puts bit 0 into carry bit)
                    lda $4002	;   get PIA-2-B status to A
                    and #$fe	;   %1111 1110 clear bit 0 (data to LED displays)
                    ora #$30	;   %0011 0000 set bits 4 & 5 (both LED clocks)
                    stx $4002	;   store *X* to PIA2-B (original state)
                    bcs Le550	;   skip next if carry set (from ROR at $e540, if bit 0 was high)
                    ora #$01	;     set bit 0 (data to LED displays)
Le550               sta $4002	;   write A to PIA2-B (data bit for an LED controller)
                    lda $06	;   get charset offset (%0001 0000 or %0010 0000)
                    eor #$ff	;   invert all bits
                    and $4002	;   clear a bit from PIA-2 based on $06 (clock bit for appropriate display)
                    sta $4002	;   write A to PIA2-B (strobe to clock a data bit to the controller)
                    ora $06	;   set a bit back in A based on $06 (clear clock bit for display)
                    tax		;   A->X (update X, was initial PIA2-B status)
                    dey		;   decrement bit counter
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
                    eor #$01	; flip bit 0 (unknown output function???; n/c on schematics)
                    sta $2000	; update PIA1-A
                    and $2000	; read back again (why???)
Le575               sta $68	; store PIA1-A status to $68 (read many places)
                    and #$04	; %0000 0100 isolate bit 2: service mode switch
                    beq Le57d	; skip next opcode if bit 2 is low
                    lda #$ff	;   A=$ff
Le57d               sta $35	; store boolean to indicate mode ($ff = service mode, $00 = on mode)
                    beq Le588	; branch down NOT in service mode
                    lda $cb	; get (service switch debounce timer)
                    beq Le587	; skip next if 0
                    dec $cb	;   decrement counter/timer
Le587               rts		; done
				;
				; handle service mode switch off
				;
Le588               ldx $cb	; get decounce timer
                    beq Le591	; branch down if 0
                    ldy #$ff	;   \ reset debounce timer
                    sty $cb	;   / 
Le590               rts		;   done
				;
Le591               ldx $0338	; get operator setting SERVICE SWITCH OVERRIDE (i.e. program w/door closed)
                    beq Le590	; branch to rts if 0 (factory setting = no override) otherwise...
                    lda $68	;   get PIA1-A status
                    ora #$04	;   set bit 2 (%0000 0100) to override into service mode, regardless of switch
                    bne Le575	; (always) branch back near top of routine to stay in service mode
				;
				;*************************************
				; ADD CREDITS & UPDATE CREDIT DISPLAY
				;*************************************
				;
Se59c               ldx #$ff	; (used to check for freeplay mode)
                    lda $02f5	; get current credit count
                    clc		; (clear carry for addition)
                    adc $02f4	; add newly-entered credits to current count
                    bcs Le5ac	; branch ahead if that overruns a byte
                    cpx $031b	;   check freeplay mode setting
                    bne Le5ad	;   if not freeplay, mode skip next opcode
Le5ac               txa		;     copy $ff to A
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
				;*********************************
				; SELFTEST RAM & ROM, FINISH BOOT
				;*********************************
				;  only reached by jmp from $e252 & $e696
				;  mystery relating to $cc, $cd and $ce near end
				;
Le5c6               jsr Sef1e	; verify RAM checksum (of programmable values)
                    bne Le5ce	; if checksum bad, skip over next opcode
                    jmp Le699	;   RAM checksum OK, jump (way) down...
Le5ce               lda #$00	; RAM checksum was BAD, so do a full RAM test:
				;
				; perform full RAM test
				;
                    sta $00	; \ pointer $00/$01 -> $0000
                    sta $01	; /
                    sta $02	; flag for when done (normal boot = $00, factory test = $ff)
				;
				; fill RAM with sequential values
				;
Le5d6               ldy #$03	; init Y counter (start after pointer & flag)
Le5d8               lda #$00	; init A counter
Le5da               clc		; clear carry for addition
                    adc #$01	;     A=A+1
                    cmp #$fd	;     have we reached $fd?
                    beq Le5d8	;     if SO loop back
                    sta ($00),y	;     store A to $00/$01 pointer + Y 
                    iny		;     increment Y counter
                    bne Le5da	;   loopback until Y wraps to 0
                    inc $01	;   increment MSB of pointer
                    ldx #$08	;   (page 8)
                    cpx $01	;   is MSB of pointer up to 8 yet?
                    bne Le5da	; loopback until done with all RAM $0003-$07ff
				;
				; verify sequential values in RAM
				;
                    lda #$00	; \
                    sta $00	;  |
                    sta $01	;  | init same loop as above
                    ldy #$03	;  |
Le5f6               lda #$00	;  |
Le5f8               clc		; / 
                    adc #$01	; \
                    cmp #$fd	;  | same loop as above, but checking values  
                    beq Le5f6	;  |
                    cmp ($00),y	;  | check value in RAM vs what it should be
                    bne Le63c	;  | if byte is WRONG branch down to give RAM error
                    iny		;  |
                    bne Le5f8	;  |
                    inc $01	;  |
                    ldx #$08	;  |
                    cpx $01	;  |
                    bne Le5f8	; /
				;
				; "bit stripe" RAM test
				;
                    lda #$00	; \
                    sta $00	;  |
                    sta $01	;  |
                    ldy #$03	;  | init same loop again
Le616               lda #$55	;  | %0101 0101 bit stripe
                    sta ($00),y	;  | write byte
                    cmp ($00),y	;  | verify byte
                    bne Le63c	;  | if WRONG, branch down to give RAM error
                    lda #$aa	;  | %1010 1010 opposite bit stripe
                    sta ($00),y	;  | write byte
                    cmp ($00),y	;  | verify it
                    bne Le63c	;  | if WRONG, branch down to give RAM error
                    lda #$00	;  | now zero
                    sta ($00),y	;  | write byte 
                    cmp ($00),y	;  | verify it
                    bne Le63c	;  | if WRONG, branch down to give RAM error
                    iny		;  |
                    bne Le616	;  |
                    inc $01	;  |
                    ldx #$08	;  |
                    cpx $01	;  |
                    bne Le616	; /
                    clc		; clear carry flag (since RAM test was OK!)
                    bcc Le63d	; unconditional branch over setting carry flag
				;
Le63c               sec		;   set carry flag to signal RAM test fail
				;
Le63d               lda $02	; \  check to see if we came from "factory test mode"
                    beq Le644	;  } if not, skip next opcode
                    jmp Le3ae	; /  return to factory test mode  
Le644               bcc Le64a	; skip over if carry clear (RAM was OK)
                    lda #$02	;   error code 2 (defective RAM)
                    bne Le65e	;   (always branch) reset the factory settings
				;
				; perform ROM test
				;
Le64a               jsr Se1a4	; verify ROM checksum
                    beq Le65c	; if ROM is OK (z flag set) branch to next check
				;
				; deal with ROM error
				;
                    sta $24	; store A (ROM checksum) at $24 (to convert to decimal digits)
                    lda #$0c	; \ set offset
                    sta $26	; / so output will be on "SELECTION BEING MADE" LEDs
                    jsr See45	; parse binary number into decimal digits
                    lda #$03	; set error code 3 (for "defective ROM")
                    bne Le65e	; (unconditional) branch past next instruction
				;
Le65c               lda #$01	; error code 1 (checksum fault) [only issue in $0300-$03__]
				;
				; code common to errors 1-3 & intentional factory settings reset
				;
Le65e               sta $6b	; store error code [error 0 if jmped here b/c factory settings were reset]
                    jsr Sfb2d	; reset the factory settings
                    jsr Sf4b7	; clear regular selection list
                    lda #$16	; \ 
                    sta $fb	; / init (video system state???) to 22
                    lda #$2b	; \
                    sta $fc	;  } default to 42 3XX & 4XX videos available
                    sta $fe	; /
                    lda #$03	; \
                    sta $fd	;  } default to 3 add'l 3XX & 4XX videos available
                    sta $ff	; /
                    jsr Sf9df	; reset MEMOREC data
                    lda $6b	; fetch error code
                    bne Le680	; if (non-zero) error present, skip RAM checksum update
                    jsr Sef18	;   update the RAM checksum (for error 0)
Le680               jsr Sfb65	; \ display error code on CCC LEDs
                    jsr Se3c7	; / pause for 2 seconds
                    lda $2000	; \  get PIA1-A status
                    and #$04	;  } %0000 0100 isolate bit 2, SERVICE/ON mode
                    beq Le699	; /  if 0 (bit 2 low, "ON" mode, NOT svc mode) branch ahead
                    lda $6b	; \ get error code
                    cmp #$01	; / compare to 1 (checksum fault)
                    bne Le696	; skip next opcode if not 1
                    jsr Sec99	;   do programming mode if RAM error detected
Le696               jmp Le5c6	; back to start of RAM/ROM check routine
                    		;
				; RAM checksum was OK (or in service mode with an error)
				; continue bootup
				;
Le699               jsr Sff67	; zero & init variables
                    jsr Se56a	; check service mode switch (among other things)
				;
				; check for CCC battery-low status
				;
                    lda $68	; \ get PIA1-A status
                    and #$02	; / check the CCC battery level
                    bne Le6af	; branch if battery level OK
				;
				; handle CCC battery-low condition
				;
                    lda #$04	; \ set error code #4 for battery low
                    sta $6b	; /
                    jsr Sfb65	; \ display Err code on CCC LEDs
                    jsr Se3c7	; / 2s pause
				;
Le6af               lda $031c	; get setting for "retain/forget credits at power-up"
                    bne Le6c6	; if not 0 (i.e. retain credits) branch ahead...
				;
				; clear credits at power-up, if programmed to do so
				;
                    ldx $cd	; (check for warm-reboot from service mode?)
                    bne Le6be	; if non-zero skip down
                    ldx $cc	;   get (flag for free credit from svc code 700)
                    beq Le6c3	;   if is zero, branch to forget credits anyway
                    sta $cc	; store setting (should be $ff to get here) at $cc variable
Le6be               sta $cd	; and at $cd variable
                    jmp Le6c6	; skip next opcode (don't clear credits)
Le6c3               jsr Se749	; clear credits (and some other stuff)
				;
				; init more stuff
				;
Le6c6               jsr Sf73c	; [subroutine; check selection list???]
                    jsr Se3d4	; 100ms delay
                    jsr Sff35	; clear LED display data
                    jsr Se59c	; add any credits and update the credit display
                    lda #$12	; \  (video system command code to initialize?)
                    sta $eb	;  } init var $eb (command to video system)
                    sta $e0	; /  init var $e0 
                    lda #$00	; \
                    sta $39	;  | init var $39 (video mode off)
                    sta $28	;  | init var $28 \
                    sta $29	;  | init var $29  } "SELECTION BEING MADE" LEDs
                    sta $2a	; /  init var $2a /
                    lda #$72	; \  init indices into event table
                    sta $0171	;  } (point to $0172)
                    sta $0170	; /  (point to $0172)
                    jsr Sfeac	; put 6 codes into event table start up wallbox comms
                    ldx $ce	; get $ce var (?) (never initialized?)
                    beq Le701	; if 0, down to main loop...
				;
Le6f1               dec $ce	;   decrement counter $ce (?)
                    bne Le6fa	;   skip ahead if 0
                    jsr Sf501	;     clear play/money counters
                    beq Le701	;     always branch to main loop (subroutine returns A=0)
				;
Le6fa               jsr Sf9df	;   clear MEMOREC tables?
                    dec $ce	;   decrement counter $ce (?)
                    bne Le6f1	;   loopback unless 0, otherwise fall into...
				;
				;***********
				; MAIN LOOP
				;***********
				; (estimated ~232 loops per second)
				;
Le701               cld		; clear decimal flag (just in case)
                    dec $e4	; decrement a fast counter
                    bne Le712	; branch ahead unless 0
				;
				; periodically (approx. once per second) check a flag to unmute amp (for video system?)
				;
                    lda $cf	; \ get a mute-off for video request flag(?)
                    beq Le712	; / skip if flag is 0
                    lda $2002	;   \  get PIA1-B state
                    ora #$01	;    } %0000 0001 set bit 0 - MUTE off
                    sta $2002	;   /  output to PIA1-B
				;
Le712               jsr Sf646	; monitor service switch, mech & coins
                    jsr Se75c	; [main loop sub-routine 1, needs name???]
                    jsr Se885	; handle video functions
                    jsr Sef5d	; handle user entry normal mode
                    lda $35	; \ check service mode flag
                    beq Le728	; / skip ahead in regular mode
				;
				; bottom of main loop in service mode
				;
                    jsr Sf35b	;   \ handle user entry in service mode
                    jmp Le701	;   / back to top of main loop
				;
				; more video-related main loop in "on" mode
				;
Le728               lda $ed	; \ check if (video-is-playing?)
                    bne Le701	; / if not 0, loopback to top of main loop
                    bit $68	; \ set flags per PIA1-A status
                    bpl Le740	; / branch based on bit 7 PIA1-A (CANCEL button)
                    lda $65	;   get debounce counter
                    bne Le701	;     if debounce counter not yet 0, back to top of main loop
                    sta $cf	;     set mute status flag(?) to 0
                    lda #$af	;     \ reinit debounce counter
                    sta $65	;     / 
                    lda #$4a	;     \ send command code to video system to cancel video
                    sta $eb	;     / 
                    bne Le701	;     (unconditional branch) loopback to top
				;
				; CANCEL (not???) pressed
				;
Le740               lda $65	; get $65 var
                    beq Le701	; if $65 var = 0 loopback to top (w/o decrementing it)
                    dec $65	;   decrement $65 counter (debounce?)
                    jmp Le701	;   jump back to top of main loop
				;
				;***************
				; CLEAR CREDITS
				;***************
				;  (and 50-byte POPULAR data table)
				;
Se749               ldx #$09	; \
                    lda #$00	;  \
Le74d               sta $02f4,x	;   } loop clears $02f4-$02fd
                    dex		;  /
                    bpl Le74d	; / 
                    ldx #$31	; \ 
Le755               sta $0100,x	;  \ loop clears the 50-byte bitwise POPULAR table
                    dex		;  /
                    bpl Le755	; /
                    rts		; 
                    		;
				;**********************
				; GENERAL HOUSEKEEPING 
				;**********************
				;  (only called from main loop)
				;  ISSUES REMAINING:
				;	$ed (video is playing???) Boolean
				;	$e0 (video-related) timer???
				;	jmp $f877 into mech handling code???
				;	$ec (video-related) flag???
				;	$70 flag???
				;	$f7 (video-credit-flag bit???)
				;  (mostly state variables and flags related to video mode)
				;
Se75c               dec $3d	; decrement main-loop timer/counter (rolls over ~once per second)
                    bne Le7c5	; if not 0 yet, branch down (thru another bne)
				;
				; once-per-second tasks
				;
                    dec $9a	; decrement (THANK YOU indicator lamp timer)
                    bpl Le77e	; branch if not a rollunder
                    lda #$00	;   \ turn off THANK YOU (and MAKE SELECTION) lamps
                    sta $38	;   / 
                    lda $ed	;   \ get (video is playing???) Boolean
                    beq Le77e	;   / branch down if no video is playing(???)
                    lda $2002	;     \  get PIA1-B status
                    and #$10	;      } isolate bit 4 (%0001 0000) - TURNTABLE MOTOR
                    bne Le77e	;     /  branch if (turntable is on)
                    lda $35	;       get service/normal Boolean
                    bne Le77e	;       branch if in service mode
                    dec $e0	;         decrement (video-related) timer???
                    bne Le77e	;         branch if timer not expired
                    jsr Sf877	;           (tail end of mech handling???)
				;
Le77e               lda $f1	; check (time until video ready timer)
                    beq Le788	; skip ahead if (video system timer) is expired
                    eor #$ff	;   invert $f1 var's bits
                    beq Le788	;   skip next opcode if timer was inhibited ($ff)
                    dec $f1	;     decrement timer one second only if not inhibited or expired
				;
Le788               lda #$e7	; \ reset main-loop timer to 231 (loops per second)
                    sta $3d	; /
                    dec $62	; decrement general seconds timer
                    bne Le79e	; branch ahead if not 0 yet
                    dec $df	;   decrement general minutes timer
                    bne Le79a	;   branch ahead if not 0 yet
                    lda #$00	;     \  (once per minute)
                    sta $ec	;      } init video-related flags??? to 0
                    sta $f6	;     /
Le79a               lda #$3c	;   \ reset seconds timer to 60
                    sta $62	;   /
				;
Le79e               lda #$ff	; \  
                    eor $f1	;  } invert video-ready timer/flag
                    ora $70	; /  OR with (record-selection-active???) flag
                    beq Le7aa	; branch down result is 0
                    lda $70	;   get (record-selection-active???) flag
                    ora $39	;   OR with (video mode boolean)
Le7aa               ora $f3	; OR A with (turntable motor flag)
                    pha		; A to stack
                    cmp $9b	; compare A to (record-is-playing boolean)
                    beq Le7ba	; skip ahead if equal
                    lda $35	;   check service/normal mode flag
                    bne Le7ba	;   branch down if in service mode
                    lda #$40	;     \
                    jsr Sfcff	;     / put #$40 into wallbox queue (indicate active/idle change)
Le7ba               pla		; A back from stack
                    sta $9b	; store to (overall system state???)
                    bne Le7e4	; skip autoplay check if playing anything???
                    lda $35	; get service/on mode flag
                    bne Le7e4	; skip autoplay check in service mode
                    lda $23	; get current keyed entry position
Le7c5               bne Le7e8	; if in entry process, skip ahead... (i.e. don't autoplay while keying)
                    sta $28	; \
                    sta $29	;  } put 0s on "SELECTION BEING MADE" LEDs
                    sta $2a	; /
				;
				; check autoplay timer, if autoplay mode is on 
				;
                    lda $0320	; get autoplay mode setting
                    beq Le7e4	; if autoplay is off (mode 0), move along
                    lda $fa	; get autoplay timer
                    beq Le7de	; if it has reached 0, branch down play something
                    dec $83	;   decrement autoplay seconds timer
                    bne Le7e8	;   if autoplay seconds timer > 0, move along
                    dec $fa	;     decrement autoplay minutes timer
                    bne Le7e4	;     if minutes timer hasn't reached 0, just reset the seconds timer
Le7de               jsr Sf535	;       go do an autoplay (minutes timer reached 0)
                    jsr Sf95a	;       [do tine-critical background tasks]
				;
Le7e4               lda #$3c	; \ reset autoplay seconds timer to 60 seconds
                    sta $83	; /
				;
				; rest of this subroutine used every loop (~232 per second)
				;
Le7e8               lda $35	; check service/normal mode
                    bne Le834	; branch down for service mode
				;
				; normal "on" mode
				;
                    lda $89	; \ check boolean flag (toggle)
                    beq Le7f2	; / skip next opcode if flag = 0
                    lda #$ff	;   if non-zero, make it a $ff 
Le7f2               eor #$ff	; toggle A (from $00 to $ff or from $ff to $00)
                    sta $89	; store boolean flag back
                    beq Le80e	; branch if flag is now 0
				;
				; only do this block every-other time (when $89 is high)
				;
                    lda $0700	; get most popular video selection #
                    cmp #$40	; compare to 64
                    bcc Le801	; skip next opcode if A < 64 (convert internal video numbering to display number???)
                    adc #$23	;   add 35 to selection numer (0-64 -> 300-364; 65- -> 400-464???)
Le801               ldx $39	; \ check video mode boolean
                    bne Le808	; / skip next opcode in video mode
                    lda $0500	;   get most popular record selection #
Le808               sta $24	; to be converted to decimal digits 
                    lda #$78	; (offset to $1b = $93: "MOST POPULAR SELECTION" LEDs)
                    bne Le818	; (always branch over next block of code)
				;
				; only do this block every-other time (when $89 is low)
				;
Le80e               lda $9b	; get (record-is-playing) boolean
                    beq Le814	; skip next opcode if 0 (not playing music)
                    lda $ef	;   get (selection # playing)
Le814               sta $24	; to be converted to decimal digits (selection # or 0 if not playing)
                    lda #$74	; direct output to $8f-$92, top "SELECTION PLAYING" LEDs
				;
				; (paths merge)
				;
Le818               sta $26	; \ set offset for output to selected set of LEDs
                    tax		; / X now has address offset: $74 or $78
                    jsr See45	; parse 2-byte value into 4 digits (onto selected LEDs)
                    lda $39	; \ check (video mode boolean)
                    beq Le832	; / branch ahead in records mode
                    cpx #$78	;   compare X offset to $78 (for "MOST POPULAR...")
                    beq Le82e	;   if "MOST POPULAR..." branch ahead
                    lda $9b	;     get (record-is-playing boolean?)
                    beq Le834	;     branch if $9b=0
                    lda $ed	;       get (video-is-playing Boolean???)
                    bne Le832	;       branch video not playing??
Le82e               inc $1c,x	;   \ increment 1st digit twice (of selected LEDs) (video selections are 3XX & 4XX)
                    inc $1c,x	;   / 
Le832               inc $1c,x	; increment 1st digit (of selected LEDs) (convert 0-199 to 100-299)
				;
				; regular "on" mode and service mode
				;
Le834               lda $38	; get (THANK YOU & MAKE SELECTION lamp control byte)
                    ora #$09	; %0000 1001 isolate bits 0 and 3 (bit 3 not understood)
                    sta $38	; store $38 var back (clearing bit 1, the "THANK YOU" control bit)
                    lda #$ff	; \
                    sta $41	;  } set record and video credits to $ff	
                    sta $6d	; /
                    eor $031b	; check freeplay mode setting
                    beq Le881	; skip next block of code in freeplay mode
				;
				; handle regular coin-op mode (not freeplay)
				;
                    lda $02f4	; get credits just recieved for money
                    clc		; \
                    adc $02f5	; / add current master record credit count to A
                    bcs Le858	; if sum rolls over, branch down
                    sta $41	;   store updated record credit total
                    bne Le858	;   if non-zero branch ahead
                    lda $38	;     \  get (THANK YOU & MAKE SELECTION lights) control byte
                    and #$fe	;      } %1111 1110 clear bit 0: THANK YOU control bit
                    sta $38	;     /  store updated value
Le858               lda $02f6	; get credits just recieved for money (videos)
                    clc		; \
                    adc $02f7	; / add current master video credit count to A
                    bcs Le881	; if this rolls over, branch to near end
                    sta $6d	; store updated video credit total
                    bne Le881	; if non-zero branch to near end
                    lda $41	;   get (# of record credits)
                    beq Le87b	;   if out of credits, branch down
                    cmp $02fb	;     compare $41 var to $02fb (selection-has-been-made boolean?)
                    bcc Le87b	;     branch if <
                    bne Le875	;     branch if not equal
                    lda $02fc	;       get $02fc var(?) MSB?
                    bne Le87b	;       branch if non-zero
Le875               lda #$01	;     \ A=1
                    sta $6d	;     / set video credit total to 1???
                    bne Le881	;     unconditional branch down
Le87b               lda $38	;   get (THANK YOU & MAKE SELECTION lights) control byte
                    and #$f7	;   %1111 0111 clear bit 3: (video-credit-flag bit???)
                    sta $38	;   store updated value
				;
Le881               jsr Se45f	; (update LED displays w/o parent loop; just flushes the LED controllers???)
                    rts		; done
				; 
                    		;**********************
				; VIDEO SYSTEM MANAGER
				;**********************
				;  only called from main loop
				;  [needs more attention]
				;
Se885               lda $eb	; get (video system command code queued)
                    beq Le88c	; skip next opcode if no video command waiting
                    jmp Le9d6	;   jump to outgoing command builder
				;
				; no command queued to send to video system
				;
Le88c               lda $ed	; get (video is playing flag???)
                    bne Le8db	; skip way ahead if video playing???
				;
                    lda $f9	; get (video mute request flag???) 
                    beq Le898	; skip ahead if 0
                    eor $f1	;   compare (vid mute req. flag???) with (time-until-video-ready flag/timer)
                    bne Le8db	; branch way ahead if ???
				;
Le898               ldx $3c	; (pending record selection for mech???) flag 
                    bne Le8c8	;
				;
                    lda $f1	; (video system timer/flag?) 
                    cmp #$ff	; inhibited?
                    beq Le8db	; if timer inhibited, branch
				;
				; time-until-video timer is active
				;
                    cmp $032b	; compare to "fill time during video search" setting
                    bcc Le8db	; if $f1 var < setting, branch (video ready soon)
				;
				; time-til-video is > programmed fill-time
				; calculated a fill-in selection # to use
				;
                    inc $e9	; \ increment (fill-in selection) and fetch it to A
                    lda $e9	; /
                    cmp #$62	; compare to 98 (wraparound)
                    bcc Le8b3	; branch on <98 (skipping 98 and 99; no XX8/XX9 fill-ins)
                    lda #$00	; \  
                    sta $e9	;  } reset (fill-in selection) and $24 var (LSB) to 0
Le8b3               sta $24	; /  (where were $25 and $26 set???)
                    jsr See45	; parse value into decimal digits
                    lda $1e	; get the resulting ones digit
                    cmp #$08	; compare to 8
                    bcc Le8c2	; branch if < 8
                    inc $e9	;   \ increment (fill-in counter) twice
                    inc $e9	;   / to avoid XX8/XX9 selections 
Le8c2               lda #$1a	; \ set video video command code $1a
                    sta $eb	; / queue it
                    bne Le8fb	; (always branch)
				;
				;
Le8c8               lda $032c	; get "max videos per record" setting
                    beq Le8d4	; if 0 skip ahead (0 = video priority mode)
                    lda $f5	;   get (videos played???)
                    cmp $032c	;   compare to record/video mix setting
                    bcs Le8e7	;   
				;
Le8d4               lda $f1	; check timer (until video is ready)
                    cmp $032b	; compare to setting: "fill-time during video search"
                    bcs Le8e7	; branch if $f1 >= setting value
				;
Le8db               lda $ec	; get (video search/fill inhibit flag???)
                    bne Le8fb	; if inhibited branch ahead
                    lda $ed	; get (video-is-playing???) Boolean
                    beq Le8ef	; if not branch down
                    lda $ea	; \ (fill-in-related???) flag
                    bne Le8c2	; /
				;
				; exceeded fill-time setting?
				;
Le8e7               lda #$00	; \ zero out (consecutive video counter)
                    sta $f5	; /
                    lda #$22	; command code $22 = "playing a fill-in record"???
                    bne Le8f9	; (unconditional branch to queue command code)
				;
Le8ef               lda $f9	; get (video_is_playing???) Boolean
                    bne Le8f7	; if video playing branch ahead
                    lda $35	; get (service mode flag)
                    bne Le8e7	; branch if in service mode
Le8f7               lda #$2a	; \ command code $2a = "idle/ready"???
Le8f9               sta $eb	; / set command code to send to video system
				;
Le8fb               lda $eb	; get (command code to video system)
                    ora $f6	; OR with (video system ready flag???)
                    bne Le910	; branch down if system not ready or any message???
				;
                    ldx $f7	; video playlist selection read  index (next to play)
                    cpx $f8	; video playlist selection write index (next new entry)
                    beq Le910	; skip down if $f7==$f8 (everything in queue done)
				;
				; video is pending in playlist
				; 
                    lda $0300,x	;   get next selection from video playlist
                    sta $7a	;   store as 2nd byte of message to video system
                    lda #$33	;   \ command code $33 to video system = "play video #"
                    sta $eb	;   /
Le910               lda #$00	;   \ (flag for a video system message)
                    sta $78	;   /
				;
                    jsr Se081	; check for & receive message FROM video system
                    beq Le91f	; branch if return code is 0 (success)
                    bmi Le91c	; branch if return code is >= $80 (no message rec'd)
                    rts		; otherwise, rts
				;
				; return code >= $80
				;
Le91c               jmp Lfb7a	; do wallbox comms (then rts from there)
				;
				; return code == 0 (message from video system rec'd)
				; process a message from the video system
				;
Le91f               lda $4d	; \  get command code from video system 
                    ldx #$4d	;  } select serial buffer location
                    jsr Se1b9	; /  log received message command in video comm table
                    lda $4d	; get command code (again)
				;
				; process/handle commands from video system:
				;
				; command from video system: $02 = "video system unavailable???" 
				;
                    cmp #$02	; compare to $02
                    bne Le938	; if not, skip to next check
                    sta $fb	;   store #$02 as video system command/state
                    ldx #$ff	;   \ set video timer/flag high (inhibit)
                    stx $f1	;   /
                    inx		;   \  (X=0)
                    stx $39	;    } clear video-mode and video-is-playing Booleans
                    stx $f9	;   /
                    rts		; done
				;
				; command from video system: $16 = "video counts for 3XX and 4XX available"
				;
Le938               cmp #$16	; compare to $16
                    bne Le959	; if not, skip ahead to next check
                    ldx #$04	; loop counter for 4(+1) bytes (parameters & command from video system)
Le93e               lda $4d,x	; \
                    sta $fb,x	;  } copy from video system input buffer to $fb-$ff
                    dex		; /
                    bpl Le93e	; loopback til X rolls under (command code goes in $fb)
                    stx $39	; set video mode Boolean high
                    inx		; \  (X=0 now)
                    stx $ec	;  } clear "video search inhibit???" flag
                    stx $f6	; /  clear "video system ready???" flag
                    lda $fc	; get (# of 3XX videos available)
                    beq Le952	; skip next opcode if 0 3XX videos available
                    dec $fc	;   convert from # of videos to maximum selection number???
Le952               lda $fe	; get (# of 4XX videos available)
                    beq Le958	; skip next opcode if 0 4XX videos available
                    dec $fe	;   convert from # of videos to maximum selection number???
Le958               rts		; done
				;
				; $2a command from video system
				; "video system idle/ready???"
				;
Le959               cmp #$2a	; compare to $2a
                    bne Le970	; if not, skip ahead to next check
                    lda $ed	; get (video_is_playing???) Boolean
                    bne Le963	; skip next opcode if a video is playing???
                    sta $ec	;   copy state to video fill/search inhibit flag???
Le963               lda $ea	; \ get (record fill-in-related flag???)
                    beq Le969	; / skip next opcode if 0
                    sta $e8	;   copy state to (mech related flag???)
Le969               lda #$00	; \
                    sta $f6	;  } clear (time-until-video) and (video_system_ready) flags
                    sta $f1	; /
                    rts		; done
				;
				; $1b command from video system
				; "video selection starting to play???"
				;
Le970               cmp #$1b	; compare to $1b
                    bne Le98f	; if no, skip ahead to next check 
                    lda $4e	; \ get parameter from video system message buffer
                    sta $ef	; / copy to (current selection #)
                    lda #$ff	; \
                    sta $f9	;  } signal for mute off
                    sta $cf	; /
                    lda #$e8	; 2nd parameter for wallbox command $62 (selection type???)
                    jsr Sf72e	; prepare and queue a wallbox message
                    inc $f5	; increment (consecutive video counter???)
                    lda #$e1	; \ reset (fast main-loop mute timer) to 225
                    sta $e4	; /
                    rts		; done
				;
				; $23 command from video system
				; "seconds until video is ready???"
				;
Le98a               cmp #$23	; compare to $23
                    beq Le9aa	;   if so, branch ahead (into code for $33 command)
                    rts		; otherwise, done here
				;
				; $33 command from video system
				; "end of video???"
				;
Le98f               cmp #$33	; compare to $33
                    bne Le98a	; if not, branch up to last check
                    lda $ed	; \ get (video_is_playing???) Boolean
                    bne Le9a4	; / if no video playing, branch ahead
                    lda #$ee	;   2nd parameter for wallbox command code $62 (selection type???)
                    jsr Sf72e	;   prepare and queue wallbox command
                    lda $2002	;   \  get PIA1-B
                    and #$fe	;    } %1111 1110 clear bit 0 - MUTE on
                    sta $2002	;   /  output to PIA1-B
Le9a4               lda #$00	; \  
                    sta $f9	;  } clear (video_is_playing???) flag
                    sta $cf	; /  clear (unmute_for_video???) flag 
				; [continue into $23 code]
				;
				; handle $23 command (and finish $33 command)
				; 
Le9aa               lda $4e	; get first parameter from video system input buffer
                    cmp #$03	; compare parameter to 3
                    bcs Le9c8	; if parameter >= 3 branch down
                    cmp #$01	; compare parameter to 1
                    beq Le9d0	; if parameter == 1 branch down
                    sta $f1	; store parameter as (time_until_video_ready)
                    ldx #$00	; \ clear (video_system_ready???) flag
                    stx $f6	; /
                    cmp #$02	; compare parameter to 2
                    bne Le9c3	; if parameter != 2 branch down
				;
				; parameter == 2
				;
                    stx $f9	; clear (video_is_playing???) flag
                    dex		; (X is now $ff)
                    stx $f1	; set (timer???) to inhibit
				;
				; parameter == 0???
				;
Le9c3               lda #$00	; \ clear (video_search_inhibit???) flag
                    sta $ec	; /
                    rts		; done
				;
				; parameter >= 3 (time until video is ready)
				;
Le9c8               sta $f1	; store parameter as (time_until_video_ready)
                    cmp #$ff	; is it set to "inhibit"?
                    bne Le9d0	; if not, skip next opcode
                    dec $f1	;   decrement $f1 counter (start at $fe to avoid inhibit state)
				;
				; parameter == 1
				;
Le9d0               lda #$ff	; \ set (video_system_ready???) flag
                    sta $f6	; /
                    bne Le9c3	; (always branch up to clear another flag then finish)
				;
				; video system command builder
				;
Le9d6               lda $eb	; get command code to send to video system
                    sta $79	; copy it to video output buffer as 1st byte of msg to vid syst
                    ldx $fb	; \ check current video system state
                    cpx #$02	; / is it 2? (idle???)
                    bne Le9e7	; if not, branch ahead
                    lda $3c	;   get (pending-selection???) flag
                    sta $ed	;   copy it to (video-playing???) flag
Le9e4               jmp Le910	;   jump back up to flag a msg to video system
				;
Le9e7               ldx #$00	; X=0
                    cmp #$33	; compare command code to $33
                    bne Lea12	; branch down if not equal [all other commands need no parameters]
				;
				; command code to video system: $33 = "play a video"
				;
                    pha		; push (command code #$33)
                    lda $fc	; get (# of 3XX videos available)
                    clc		; clear carry flag for addition
                    adc $fd	; add (add'l 3XX videos???) to A, result if maximum valid 3XX selection
                    cmp $7a	; compare sum to requested selection # (already in output buffer)
                    bcs Lea0d	; branch if selection in range (valid)
				;
				; selection invalid (too high)
				;
                    ldy $7a	; get requested selection to Y
                    cpy #$64	; compare it to (100, i.e. a 4XX selection instead)
                    bcc Lea0b	; branch if Y < 100, selecting max available selection (A) instead
				;
				; it must be a 4XX selection
				;
                    ldy $fe	; get (# of 4XX videos available) to Y
                    beq Lea0b	; if no 4XX videos, branch to play the maximum valid 3XX selection (in A)
				;
				; 4XX selected and 1 or more 4XX videos available
				;
                    tya		; copy (# of 4XX videos available) to A
                    clc		; clear carry flag for addition
                    adc $ff	; add (additional 4XX videos) to A to get total 4XX videos available
                    adc #$64	; add $64 (100) to A to get a maximum valid selection #
                    cmp $7a	; compare result to requested selection #
                    bcs Lea0d	; if selection is valid branch to process it; otherwise use max valid 4XX selection
				;
Lea0b               sta $7a	; store A in serial output buffer
				;
				; begin calculating checksum
				;
Lea0d               pla		; pull A (command code #$33)
                    clc		; \
                    adc $7a	; / add parameter byte for calculating a checksum
                    inx		; increment X (to next byte, buffer index for checksum)
				;
				; calculate checksum byte for message to video system
				;
Lea12               eor #$ff	; \ 
                    sec		;  } calculate 2's complement of A [so that total of all bytes = 0]
                    adc #$00	; /
                    sta $7a,x	; put checksum into buffer (last byte in message) 
                    lda $78	; \ check a flag at $78 [flag related to video system messages?]
                    bne Lea27	; / if already set, skip ahead 
                    lda #$ff	;   \ set a flag at $78
                    sta $78	;   /
                    jsr Se117	;   send message to video system
                    jmp Lea2a	;   skip next opcode
				;
Lea27               jsr Se137	; send message to video system (skip initial handshake???)
				;
Lea2a               lda $08	; get exit status 
                    beq Lea37	; on exit status 0 (success) branch down
                    bpl Le9e4	; on exit status >= $80 branch back to check for msg from video system
                    asl a	; shift exit status left (move bit 7 to carry)
                    bne Lea36	; branch to rts if non-zero (for $08 or $40 exit status?) 
                    jsr Lfb7a	; (what else is there?) do wallbox comms then return
Lea36               rts		;
				;
				; success sending to video system
				;
Lea37               lda $df	; check (minutes timer for video???)
                    beq Lea42	; if 0, skip over next block
                    lda $79	;   \  get command code (incl. # of bytes in message)
                    ldx #$79	;    } select video output buffer
                    jsr Se1b9	;   /  copy video output buffer contents to page-1 table
				;
Lea42               ldx $39	; check video-mode Boolean
                    bne Lea5a	; if non-zero, skip ahead
                    stx $031e	;   change programmable records/video setting! (to 0=video)
                    dec $39	;   set video mode flag (from $00 to $ff)
                    jsr Sef18	;   update RAM checksum
                    lda #$01	;   \ put #$01 into wallbox queue (?)
                    jsr Sfcff	;   / 
                    lda #$60	;   \ put #$60 into wallbox queue (send most popular selections)
                    jsr Sfcff	;   / 

                    ldx #$ff	; X=$ff (for setting booleans)
Lea5a               lda $eb	; get (last command to video system???)
                    cmp #$2a	; was it the $2a (idle/ready???) command?
                    bne Lea64	; if not, skip to next check...
                    stx $f9	;   set (video_is_playing) flag
                    beq Lea80	;   always branch
				;
				; command to video system: $1a = "begin play handshake???"
				;
Lea64               cmp #$1a	; $1a command?
                    bne Lea6e	; if not, skip to next check...
                    stx $ea	;   \ set flags for 
                    stx $3c	;   / (related-to-fill-in-records???) and (pending-selection-for-mech???)
                    beq Lea7a	;   always branch
				;
				; command to video system: $22 = "playing a fill-in-record???"
				;
Lea6e               cmp #$22	; $22 command?
                    bne Lea89	; if not, skip to next check...
                    lda $35	;   \ check service/normal mode
                    beq Lea7a	;   / skip in normal mode
                    lda $3c	;     \ check (pending-selection-for-mech???) flag
                    beq Lea80	;     / skip ahead if 0  
Lea7a               lda #$19	;   \
                    sta $e0	;   / set (video-related-timer) to 25 seconds
                    stx $ed	;   \ set flag for (video-is-playing???)
Lea80               stx $ec	;   / set flag for (video-search/fill-inhibit???)
                    ldx #$07	;   \  
                    stx $df	;   / set a minutes timer to 7
                    jmp Lea95	;   to near end of routine
				;
				; command to video system: $33 = "play a video selection???"
				; 
Lea89               cmp #$33	; $33 command?
                    bne Lea95	; if not, skip ahead
                    inc $f7	; increment video playlist index
                    bne Lea95	; if not wrapped around, skip ahead
                    lda #$90	;   \ reset video playlist index to $0390
                    sta $f7	;   / (start point) [continue]
				;
				; (shared ending)
				;
Lea95               lda #$00	; \
                    sta $eb	;  } zero out (video command)
                    sta $78	; /       and (video output buffer flag)
                    rts		; done
				;
				;***********************************
				; TAKE & COUNT MONEY - GIVE CREDITS
				;***********************************
				; (most remaining issues relate to "overflow" and other special conditions)
				;
Sea9c               lda $02d7	; get nickels to be added to mechanical counter
                    beq Leac2	; branch ahead if no more needed
                    dec $3a	;   decrement counter/timer
                    bne Leac2	;   branch ahead if counter non-zero
                    lda #$12	;     \ 
                    sta $3a	;     / reset counter to $12 (18)
                    lda $2002	;     \ get PIA1-B
                    eor #$80	;      } %1000 0000 flip bit 7: MONEY COUNTER output
                    sta $2002	;     /  output to PIA1-B
                    and #$80	;     %1000 0000 isolate bit 7 
                    bne Leac2	;     branch if counter bit high
                    dec $02d7	;     decrement nickels to be added to mechanical counter
                    ldx #$e4	;   \ point to 2-byte counter
                    jsr Sf18d	;   / increment total money counter (0-9999)
                    ldx #$fe	;   \ point to another 2-byte counter
                    jsr Sf18d	;   / increment total money counter (unresettable) (0-9999)
				;
				; read coin switches
				;
Leac2               ldx #$05	; loop counter: 5 switches to check
Leac4               lda $ff86,x	;   \ get select code from ROM table
                    sta $4002	;   / write to PIA2-B (select a coin input line)
                    lda $4000	;   read PIA2-A (see if coin switch is closed)
                    cpx #$05	;   is this the 1st time through the loop?
                    bne Lead6	;   if NOT branch down
				;
				;     first loop
				;
                    lsr a	;     shift A right to set/clear carry flag based on bit 0
                    bcs Leadc	;     bill inserted; exit loop with X=5
                    bcc Lead9	;     nothing; continue looping
				;
				;   2nd-5th loops
				;
Lead6               lsr a	;   shift A right another bit (each time)
                    bcc Leadc	;   if carry clear, exit this loop with X=4,3,2 or 1
Lead9               dex		;   next next coin switch
                    bne Leac4	; loopback unless we've reached 0
Leadc               stx $44	; store coin input (a value 0-5)
				;
                    txa		; copy coin input value to A
                    bne Leaee	; if money was entered branch down
                    lda $45	; \
                    beq Leae7	;  } decrement the $5/$1 bill pulse timer (only if active)
                    dec $45	; /
Leae7               lda $80	; get (debounce counter)
                    beq Leaed	;   \ unless already 0,
Leaeb               dec	 $80	;   / decrement (debounce counter)
Leaed               rts		; done
				;
Leaee               lda $80	; get debounce counter
                    beq Leaeb	; loopback if 0
                    bmi Leaf5	; skip next opcode if underrun
                    rts		; done
				;
				; calculate address of money counter to use
				;
Leaf5               lda #$04	; \ init (debounce counter) to 4
                    sta $80	; /
                    lda #$d6	; \
                    sta $0d	;  } set base address for $0d temp var
                    sta $cb	; /  ($cb var not understood here???)
                    ldx $44	; init a loop counter/offset to money level entered (multiply-by-2 loop)
Leb01               clc		; clear carry flag (for addition)
                    lda #$02	;   \
                    adc $0d	;   / add 2 to temp variable $0d
                    dex		;   decrement loop counter
                    sta $0d	;   store updated temp var $0d
                    bne Leb01	; loopback until 0
				;
                    ldx $0d	; \ point to 2-byte counter based on temp var just calculated
                    jsr Sf18d	; / increment a counter (0-9999) [for denomination just entered]
				;
                    lda $44	; get money-level entered again
                    cmp #$05	; was it a dollar-bill?
                    bne Leb46	; if not branch way down
				;
				; handle bill signals
				;
                    lda #$02	; \ 
                    sta $80	; / set debounce counter to 2?
                    lda $031a	; get setting for dollar bill extra credits
                    clc		; clear carry flag (for addition)
                    adc $02f5	; \ add extra credits to current credit count
                    sta $02f5	; / store new sum back
				;
				; handle $5 detection
				;
                    lda $45	; get ($1/$5 bill pulse timer)
                    bne Leb2a	; if counter active, skip next opcode
                    sta $46	;   reset $1/$5 bill counter to 0 (A=0 to get here)
Leb2a               lda #$80	; \ A=$80
                    sta $45	; / reset $5-bill pulse timer?
                    inc $46	; increment counter (1/5 bills?)
                    lda $46	; \ get counter to A (1/5 bills?)
                    cmp #$05	; / check if $5 bill was inserted
                    bne Leb46	; if not, branch down
				;
				; handle $5 bill (fix accounting)
				; (contains a bug fixed in a later version;
				;  $02e0 is not stored after subtraction)
				;
                    ldx #$e2	; point to $02e2/$02e3 counter
                    jsr Sf18d	; increment counter (0-9999) for $5 bills
                    lda $02e0	; get LSB of 2-byte $1-bill counter
                    sec		; set carry bit for subtraction
                    sbc #$05	; subtract 5 (un-count 5 $1 bills if $5 bill was inserted)
                    bcs Leb46	; if carry bit still set skip next opcode
                    dec $02e1	;   decrement MSB of $1-bill counter if needed
				;
				; common path for all money in
				;
Leb46               ldx $44	; get money-level entered (1-5) again
                    lda $0336	; get (undocumented memory location 54, "disable THANK YOU"???)
                    bne Leb53	; if non-zero branch down
                    lda #$02	;   \  if zero:
                    sta $38	;    } turn on THANK YOU & MAKE SELECTION lights
                    sta $9a	;   /  set timer value for 2 secs of THANK YOU
				;
Leb53               lda $02d7	; get (nickels to be added to mechanical counter)
                    clc		; \
                    adc $0313,x	; / add number of nickels for this coin level to A (from settings table)
                    sta $02d7   ; store updated total to (nickels to be added to physical counter?)
                    lda $0313,x ; now get number of nickels in for this coin level to A
                    sta $a0	; \ store value of money just paid for sending to wallboxes
Leb62               sta $0c	; / and in a temp var
                    lda #$e0	; \ 
                    jsr Sfcff	; / put #$e0 into wallbox queue/log (money deposited)
                    lda $0c	; get (recently money deposited, in nickels) from temp var
                    clc		; \
                    adc $02f8	; / add to money-in total
                    bcc Leb77	; branch down if that did NOT set the carry flag
                    sta $97	;   store (credit-overflow???_
                    inc $97	;   increment (credit-overflow???)
                    lda #$ff	;   A=$ff (set A to maximum byte value)
Leb77               sta $02f8	; store updated money-left-over(???) variable back
                    lda #$00	; \ 
                    sta $0f	;  } set $0f var to 0 (set to records pricing/credits)
                    jsr Sec14	; /  calculate (records) credits earned for money in
                    lda $0b	; \ get newly earned credit total
                    sta $02f4	; / store to $02f4
                    lda $0a	; \ get any money left-over (but not enough for any credit)
                    sta $02f9	; / store to $02f9
                    lda #$ff	; \ ($ff is for records-only mode)
                    cmp $031e	; / check records/video setting (loc 30)
                    beq Lebf7	; branch way down for records-only mode; continue for video mode...
				;
				; video mode credits
				;
                    sta $0f	; \ set flag to indicate video mode to subroutine
                    jsr Sec14	; / calculate (video) credits earned for the money in
                    lda $0a	; \ any money left-over (but not enough for any credit)
                    sta $02fa	; / store to $02fa
                    lda $0b	; \ get newly earned credit total
                    sta $02f6	; / store to $02f6
                    lda #$05	; \ set $06 temp var to 5 (to indicate no match?) 
                    sta $06	; /
                    ldx #$04	; outer loop counter, X=4
Leba7               ldy #$04	;   inner loop counter, Y=4
Leba9               lda $0300,x	;     get Xth record pricing level
                    beq Lebb3	;     skip down if unused slot
                    cmp $030a,y	;     compare to Yth video pricing level
                    beq Lebb8	;     branch down if equal
Lebb3               dey		;     decrement video pricing level counter
                    bpl Leba9	;   loopback til done with Y video pricing levels
                    bmi Lebde	;   always branch to next X if no matches found
				;
				; for any video pricing levels found matching:
				;
Lebb8               lda $030f,y	; get Yth video credit level
                    beq Lebde	; branch to next X if unused slot
                    sta $17	; store (video level) credits as (LSB of divisor)
                    lda #$00	; \
                    sta $18	;  } init $15 & $18 to 0 (inputs to division routine)
                    sta $15	; /
                    lda $0305,x	; get Xth record credit level
                    beq Lebde	; branch down if unused slot (programmed 0)
                    sta $16	; store credit level as (MSB of dividend?)
                    stx $06	; store X index (0-4) to $06 temp var
                    lda $02f5	; get current credit count
                    clc		; \
                    adc $02f4	; / add credit count to credits just recieved
                    bcc Lebd9	; if no rollover, skip next opcode
                    lda #$ff	;   limit to maximum of 255 credits
Lebd9               cmp $0305,x	; compare A to X-th record credit level
                    bcs Lebea	; branch out of loop if A>= credit level
Lebde               dex		; next X   
                    bpl Leba7	; loopback until done with all record pricing levels
                    lda #$05	; A=5
                    cmp $06	; compare to $06 var (if no matches found above?)
                    bne Lebea	; skip next opcode if $06 var <> 5
                    jmp Sec99	;   jump into programming mode???!
Lebea               jsr Sec5c	; division routine (divides $16 by $17)
                    lda $16	; get (quotient of division?)
                    sta $02fb	; store to $02fb (???)
                    lda $15	; get (quotient of division?)
                    sta $02fc	; store to $02fc (???)
				;
				; rejoining records-only mode path
				;
Lebf7               lda #$00	; \ 
                    sta $02fd	; / zero out $02fd (???)
                    sta $37	; \ ...(alt. price selection) flag
                    sta $3b	; / ...(current selection type records/video) flag
                    jsr Se59c	; update credit display
                    lda $97	; get (overflow-value???)
                    beq Lec13	; if var = 0, branch to rts
                    jsr Sf256	;   [something credit related??]
                    lda $97	;   get (overflow-value???) to A
                    ldx #$00	;   \ clear (overflow-value???)
                    stx $97	;   /
                    jmp Leb77	;   jump back into this routine
Lec13               rts		; done
				;
				;*********************************
				; CREDIT/PRICE TABLE CALCULATIONS
				;*********************************
				;   expects $02f8 - total money deposited but uncredited (in nickels)
				;   expects $0f - records-only/video mode (boolean)
				;   returns $0b - credits given for money just deposited
				;   returns $0a - balance of money left over (if any)
				;
Sec14               lda $02f8	; get value of money deposited (+left-over) [in nickels]
                    sta $0a	; put that in $0a var (value of money in) [in nickels]
                    lda #$00	; \ init temp var to 0 (running credit total)
                    sta $0b	; /
                    lda $0f	; get temp boolean (records-only/video pricing/credit select)
                    beq Lec2b	; branch down for (records-only mode)
				;
				; set pointers for video mode
				;
                    lda #$0a	; \
                    sta $11	;  \ LSBs of pointers for video mode
                    lda #$0f	;  / 
                    sta $13	; /
                    bne Lec33	; (always branch)
				;
				; set pointers for records-only mode
				;
Lec2b               lda #$00	; \
                    sta $11	;  \ LSBs of pointers for records-only mode
                    lda #$05	;  /
                    sta $13	; /
				;
Lec33               lda #$03	; \  MSBs of pointers
                    sta $12	;  } $11/$12 points to $0300 or $030a (base of pricing levels)
                    sta $14	; /  $13/$14 points to $0305 or $030f (base of credit levels)
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
				;  similar to example 6502 division code shown at:
				;    https://www.llx.com/Neil/a2/mult.html
				;    (not commented here, see above URL for explanation)
				;
Sec5c               pha		; \
                    txa		;  \
                    pha		;   } put AX&Y registers on stack
                    tya		;  /
                    pha		; /
                    lda #$00	; init A to 0
                    sta $19	; init remainder to 0 (LSB)
                    sta $1a	; init remainder to 0 (MSB)
                    ldx #$10	; init counter for 16 bit calculation
Lec69               asl $15	; 
                    rol $16	; 
                    rol $19	; 
                    rol $1a	; 
                    lda $19	; 
                    sec		; 
                    sbc $17	; 
                    tay		;
                    lda $1a	;
                    sbc $18	;
                    bcc Lec84	;
                    inc $15	;
                    sta $1a	;
                    tya		;
                    sta $19	;
Lec84               dex		; decrement X counter
                    bne Lec69	; loopback until X=0
                    pla		; \
                    tay		;  \
                    pla		;   } restore registers from stack
                    tax		;  /
                    pla		; /
                    rts		; 
				;
				;******************
                    		; PROGRAMMING MODE
				;******************
				;  entry point is actually $ec99 as if RESET was pressed
				;  note: no "background" tasks occur in programming mode
				;        it is pure and simple real-time
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
                    rts		; done (exits programming mode)
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
                    sta $75	; copy location to temp var (memory offset)
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
                    sta $75	; store actual RAM offset to temp var (memory offset)
                    tax		; copy A to X
                    lda $0300,x	; \ get the LSB of stored memory value
                    sta $0a	; / store LSB to local var $0a
                    inx		; move to next byte of stored value
                    lda $0300,x	; \ get MSB of stored memory value
                    sta $0b	; / store MSB to local var $0b
				;
				; (common path for all memory locations)
				;
Led3b               cpx #$0f	; \ compare location to 15
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
                    sta $2d	; store it as $2d (1st multiplicand)
                    lda $0319	; get coin multiplier from settings (mem loc 25; usually=5)
                    sta $2f	; store it as $2f (2nd multiplicand)
                    lda #$00	; \  
                    sta $2e	;  } MSB of both multiplicands is 0
                    sta $30	; /  (i.e. multiplying two 1-byte numbers)
                    jsr Seeec	; multiply $2d/$2e by $2f/$30
                    lda $31	; \ get LSB of product
                    sta $0a	; / store to $0a (now represents cents)
                    lda $32	; \ get MSB of product
                    sta $0b	; / store to $0b (MSB)
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
				;
                    lda #$64	; \
                    sta $1f	; / init debounce counter variable to 100
Led89               jsr Se3d8	; 5ms delay
                    jsr Sf21b	; read keypad
                    lda #$75	; \ %0111 0101: select the RESET key
                    sta $4002	; / write to PIA2-B
                    lda $4000	; \ get PIA2-A
                    and #$02	; / %0000 0010: isolate bit 1 - keypad/button input (RESET selected)
                    bne Leda2	; if bit high (RESET key inactive) exit this loop
                    dec $1f	;   decrement debounce counter while RESET is pressed
                    bne Led89	;   loopback until counter is 0 or until RESET is released
                    jmp Sec99	;     if RESET held for a long press (0.5s) go back to start of programming mode
				;
Leda2               lda $1f	; get $1f counter (RESET debounce timer)
                    cmp #$64	; compare to 100
                    bne Leddb	; if counter < 100, branch down (RESET short-press?)
                    lda #$80	; \ %1000 0000
                    bit $36	; / bitwise AND with keypress code
                    bne Led89	; loopback if no keypress
				;
				; handle a non-RESET keypress while editing value in programming mode
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
                    sta $29	; clear 2nd LED digit
                    sta $2a	; clear 3rd    LED digit
				;
				; handle a digits entered while editing in prog mode
				; shift entered digits left as more are entered
				;
Ledc4               lda $29	; get 2nd LED digit
                    sta $28	; copy it to 1st LED digit
                    lda $2a	; get 3rd digit
                    sta $29	; copy it to 2nd LED digit
                    lda $36	; get keypress code
                    sta $2a	; put it as 3rd LED digit
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
                    sta $29	;  \ replace any blank digits
Ledef               lda #$00	;  / with 0s for upcoming numerical conversion
                    sta $28	; /
				;
				;  an updated value has been entered
				;  now do something with it
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
				; (common path)
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
                    bcs Leddb	;     trying to enter 2-byte value into 1-byte location: reject it
				;
				; actually put the new value in RAM 
				;
Lee2c               ldx $75	; get actual RAM offset; corrected for all locations
                    lda $2c	; get (LSB of) new value
                    sta $0300,x	; store in programmable RAM
                    lda #$39	; \ compare memory location being edited to 57
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
				;  NOTE: this section of code uses 6502 decimal mode to perform BCD math
				;
See45               pha		; \
                    php		;  \
                    txa		;   \ put all registers, including flags, onto the stack
                    pha		;   / some serious shit's about to happen!
                    tya		;  /
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
                    sta $25	;  } reset variables $25 and $26 to 0
                    sta $26	; /
                    pla		; \
                    tay		;  \
                    pla		;   \ restore all registers
                    tax		;   / including flags
                    plp		;  /
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
                    txa		;  \ A, X and flags to the stack
                    pha		;  / (Y register is not used)
                    php		; /
                    lda $1b	; \  
                    pha		;  \ 
                    lda $1c	;   \ put the 3-digit entry
                    pha		;   / onto the stack, too
                    lda $1d	;  / 
                    pha		; / 
                    ldx $1b	; get 1st digit (100s) to X
                    lda #$00	; \
                    sta $2b	;  } init $2b/$2c to 0
                    sta $2c	; /
Leeb3               lda $1b	; get 1st digit to A
                    beq Leec7	; branch down if 1st digit is 0 
                    lda #$64	; (100)
                    clc		; \
                    adc $2c	; / add to 100 to LSB
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
                    clc		; \ 
                    adc $1d	; / add on 3rd digit (1s) to total (now 0-99)
                    clc		; \ 
                    adc $2c	; / add to LSB
                    sta $2c	; store LSB back
                    lda #$00	; reset A
                    adc $2b	; add (carry flag) to MSB
                    sta $2b	; store MSB back
                    pla		; \
                    sta $1d	;  \
                    pla		;   \ restore $1b-$1d to prior values
                    sta $1c	;   /
                    pla		;  /
                    sta $1b	; /
                    plp		; \
                    pla		;  \ restore flags, X and A registers
                    tax		;  /
                    pla		; /
                    rts		; 
                    		;
				;*************************
				; MULTIPLICATION FUNCTION
				;*************************
				;  multiplies 2-byte values; returns a 4-byte result
				;    in:  $2d/$2e & $2f/$30
				;    out: $31/$32 & $33/$34
				;    (only used as credit multiplier in programming mode)
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
Leef7               lsr $30	;
                    ror $2f	;
                    bcc Lef08	;
                    clc		;
                    lda $33	;
                    adc $2d	;
                    sta $33	;
                    lda $34	;
                    adc $2e	;
Lef08               lsr a	;
                    sta $34	;
                    ror $33	;
                    ror $32	;
                    ror $31	;
                    dex		; decrement loop counter
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
Lef26               clc		;   clear carry every time (ignore overflows)
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
                    tya		;  \			2 cycles  \ 
                    pha		;   } AY&X to stack	3 cycles   \ 15 cycles
                    txa		;  /			2 cycles   /
                    pha		; /			3 cycles  /
                    ldx #$af	; init X to 175 loops	2 cycles /
Lef54               dex		;   (delay		2 cycles \ 5*175=875 cycles
                    bne Lef54	;    loop)		3 cycles /
                    pla		; \			4 cycles \
                    tax		;  \			2 cycles  \
                    pla		;   } restore XY&A	4 cycles   \ 22 cycles
                    tay		;  /			2 cycles   /
                    pla		; /			4 cycles  /
                    rts		; done			6 cycles / ~912 cycles total
                    		;
				;************************************
				; HANDLE INPUT IN "ON" (NORMAL) MODE
				;************************************
				;
				; nothing to do in service mode
				;
Sef5d               lda $35	; check service mode flag
                    beq Lef62	; if in regular mode skip next opcode
                    rts		;   done (in service mode)
				;
				; read keypad and rts if no keypress
				;
Lef62               jsr Sf21b	; read keypad
                    lda $36	; get keycode
                    bpl Lef6a	; skip next opcode if any key pressed
                    rts		;   if no keypress, done here
				;
				; check for RESET key
				;
Lef6a               cmp #$0a	; was it the RESET key?
                    bne Lef80	; if not, skip down
				;
				; handle a RESET keypress
				;
Lef6e               lda #$00	; \  zero out...
                    sta $23	;  \ key entry position
                    sta $37	;  / alternate credit-price selection flag
                    sta $3b	; /  set to records mode
                    jsr Sff31	; \ clear top LED displays
                    jsr Se59c	; / update credit display
Lef7c               rts		; 
				;
Lef7d               jmp Lf07a	; (only reached from bne below) jump to handle number keypress
				;
				; handle non-RESET key presses
				;
Lef80               lda $38	; (THANK YOU / MAKE SELECTION lamp control) is used to determine if entry is allowed
                    and #$01	; isolate bit 0 ("MAKE SELECTION" bit)
                    bne Lef87	; skip next opcode if MAKE SELECTION is lit
                    rts		;   done here (MAKE SELECTION is NOT lit, no key entry allowed!)
				;
Lef87               lda $36	; get keycode of key pressed
                    cmp #$0b	; was it the POPULAR key? 
                    bne Lef7d	; no? branch to jmp (to handle a number keypress)
				;
				; handle POPULAR keypress
				;
Lef8d               lda #$00	; \ 
                    sta $3b	;  } clear $3b var (selection-type default to records???)
                    sta $06	; /  LSB of pointer
                    tay		; init 50-byte table offset
                    lda #$05	; \ MSB of pointer $06/$07 -> $0500
                    sta $07	; / (record selections MEMOREC table)
                    lda #$01	; \
                    sta $0b	; / init temp var to 1 (bitmask for records)
                    lda $39	; \ check video mode boolean
                    beq Lefbc	; / if (records mode) branch down
				;
				; video mode???
				;
                    lda $38	;   \ get (THANK YOU / MAKE SELECTION light control byte)
                    and #$08	;   / %0000 1000 isolate bit 3: (video-credits-present bit???)
                    beq Lefbc	;   if bit 3 = 0 branch down
                    lda $fb	;     \ get (current video system state???)
                    cmp #$02	;     / compare it to 2 ("video system unavailable" state???)
                    beq Lefbc	;     if "unavailable", branch down to records mode code
                    lda #$ff	;       \
                    sta $3b	;       / set selection-type Boolean to video mode
                    lda #$00	;       \
                    sta $06	;	 \ $06/$07 -> $0700
                    lda #$07	;	 / (video selections MEMOREC table)
                    sta $07	;	/ 
                    lda #$02	;       \ set temp var to 2 (bitmask for videos)
                    sta $0b	;       /
				;
				; loop to ensure a POPULAR selection not already picked by "this customer"
				; (i.e. with a group of credits; table reset when all credits used)
				;
Lefbc               lda $0b	; \ get temp var
                    sta $08	; / copy to temp var $08 (bitmask)
                    lda ($06),y	; get Yth entry from record/video popular selections table ($0500/$0700)
                    cmp #$c8	; compare selection # to 200
                    bcc Lefc8	; branch over next opcode if selection # < 200
                    lda #$00	;   make selection number 0 (really?)
Lefc8               sta $0a	; put selection in temp var
                    iny		; increment the table offset (to next most popular)
                    beq Lef7c	; if Y wrapped to 0 (would it?) branch to nearby rts
				;
				; calculate table position (A) and bitmask ($08) for selection (A)
				;
Lefcd               cmp #$32	; compare selection # to 50
                    bcc Lefdb	;   branch down if A < 50 (exits loop)
                    sbc #$32	;   reduce A by 50 (carry was set; should never be cleared)
                    asl $08	;   \ shift the bitmask left twice
                    asl $08	;   /
                    bcs Lefbc	;   loopback to top if a 1 was shifted out of $08
                    bcc Lefcd	; loopback to possibly reduce by 50 again
				;
				; check Yth most popular selection in the 50-byte table
				; loop back for next most popular if it's found
				;
Lefdb               tax		; move "folded" table offset to X
                    lda $08	; get bitmask to A
                    and $0100,x	; \ check bit in 50-byte table
                    bne Lefbc	; / if bit is set (selection already made by this customer) loopback for another
				;
				; if not found in 50-byte table, mark it
				;
                    lda $08	; \  get bitmask again
                    ora $0100,x	;  } set bit per bitmask
                    sta $0100,x	; /  update table to mark this selection 
				;
                    lda $0a	; get (selection #) from temp var
                    pha		; push selection # to stack
                    lda $87	; \ check (invalid selection???) flag
                    bne Lf008	; / skip ahead if nonzero
                    ldx #$ee	;   page 2 location of "total records" counter
                    lda $3b	;   \ check video/record boolean
                    beq Leffa	;   / for record mode, skip next opcode
                    ldx #$e8	;     page 2 "total videos" counter instead
Leffa               txa		;   \ put counter location on the stack for a moment
                    pha		;   /
                    dex		;   \ move X 2 bytes lower (point to the previous page-2 counter)
                    dex		;   /
                    jsr Sf18d	;   increment 2-byte counter: (records/videos) w/POPULAR button
                    pla		;   \ retrieve original counter loctation from stack
                    tax		;   /
                    jsr Sf18d	;   increments 2-byte counter: total (records/videos)
                    inc $88	;   increment POPULAR use counter (?)
				;
Lf008               pla		; pull selection from stack (ref $efed)
                    tax		; put selection into X
                    lda #$00	; \ clear (invalid-selection???) flag
                    sta $87	; /
                    lda $3b	; check video/records boolean
                    beq Lf01a	; branch ahead for records
                    cpx #$40	;   compare to 64 (why???)
                    bcc Lf01a	;   if X < 64, branch ahead
                    txa		;     copy X to A
                    adc #$23	;     A=A+36 (35 + carry flag)
                    tax		;     copy A back to X
				;
Lf01a               txa		; copy X (selection) to A
                    sta $24	; place to convert to decimal digits
                    lda #$0c	; \ set offset; output to $27-$2a ("SELECTION BEING MADE" LEDs)
                    sta $26	; /
                    jsr See45	; parse value to 4 decimal digits
                    inc $28	; increment 100s digit of result (0XX -> 1XX, 1XX -> 2XX)
                    lda $3b	; check record/video mode?
                    beq Lf068	; if record-mode, branch down
				;
				; handle POPULAR keypress in video mode(?)
				;
                    inc $28	; \ increment 100s digit
                    inc $28	; / twice
                    txa		; X->A (selectoin???)
                    cmp #$64	; compare A to $64 (100)
                    bcc Lf035	; branch over next opcode if A < 100
                    sbc #$64	;   A=A-100
Lf035               cmp #$0a	; compare A to (10)
                    beq Lf073	; branch way down if A=10
                    cmp #$0b	; compare A to (11)
                    beq Lf073	; branch way down if A=11
                    cmp #$1e	; compare A to (30)
                    beq Lf073	; branch way down if A=30
                    cmp #$1f	; compare A to (31)
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
                    jsr Sf95a	; (do some time-critical background tasks)
                    jmp Lf24c	; jump to credit-related(???) subroutine (rts from there)
				;
				; (records-mode path)
				;
Lf068               lda $2a	; get the 1s digit
                    cmp #$08	; compare it to 8
                    bcc Lf045	; if < 8, branch back to normal flow
                    lda $031d	; check setting for (dis)allow XX8/XX9 selections
                    beq Lf045	; if "enable all", branch back to normal flow
Lf073               lda #$ff	;   \ 
                    sta $87	;   / set (invalid-selecton???) flag (disallowed XX8/XX9 selection)
                    jmp Lef8d	;   jump to POPULAR keypress code?
				; 
				; handle a number-key entry (in regular mode)
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
                    lda $3f	; get (maximum valid 10s digit entry)
                    cmp $36	; compare to keypress code
                    bcs Lf094	; branch ahead if (maximum valid 10s digit entry) >= keypress code
                    rts		; otherwise, ignore keypress(?) and return
				;
Lf091               jmp Lf101	; jump down to handle 2nd digit entry for 1st digit of 1-2
				;
				; handle 2nd digit for 1st digit of 3 or 4
				;
Lf094               beq Lf0c2	; if (???) branch down (to jmp)
                    lda #$09	; \ set (maximum valid 1s digit entry) to 9
                    sta $40	; /
                    bne Lf0c2	; unconditional branch down (to jmp)
				;
				; handle 3rd (last) digit of 3XX & 4XX entries(?)
				;
Lf09c               lda $29	; get 10s digit of entry
                    asl a	; \
                    asl a	;  \ shift 10s digit into upper nibble
                    asl a	;  /
                    asl a	; /
                    clc		; clear carry for addition
                    adc $36	; add new keypress code (now a 2-digit BCD value of 2nd & 3rd digits)
                    cmp #$10	; compare to $10
                    bne Lf0aa	; skip next opcode if not $10
                    rts    	;   done here (310/410 not allowed)
Lf0aa               cmp #$11	; compare to $11
                    bne Lf0af	; skip next opcode if not $11 
                    rts		;   done here (311/411 not allowed)
Lf0af               cmp #$30	; compare to $30
                    bne Lf0b4	; skip next opcode if not $30
                    rts   	;   done here (330/430 not allowed)
Lf0b4               cmp #$31	; compare to $31
                    bne Lf0b9	; skip next opcode if not $31
                    rts		;   done here (331/431 not allowed?)
Lf0b9               lda $40	; get (maximum valid 1s digit)
                    cmp $36	; compare to last digit entered
                    bcs Lf0c2	; branch if (max valid 1st digit) >= last digit entered
                    beq Lf0c2	; branch if = [opcode should never be reached?]
Lf0c1               rts		; otherwise ignore keypress
				; 
Lf0c2               jmp Lf13e	; (long jump fron branch above) 
				; 
				; handle the 1st digit of a new entry
				; 
Lf0c5               lda $36	; get the keycode
                    cmp #$03	; \
                    beq Lf0d2	;  \ if it was a 3 or 4
                    cmp #$04	;  / branch down to handle
                    beq Lf0d2	; /
                    jmp Lf12f	; otherwise, jump down to check more...
				; 
				; handle a 1st digit of 3 or 4
				; 
Lf0d2               lda $39	; check video mode boolean
                    beq Lf0c1	; if 0, branch to nearby rts (to ignore keypress)
                    lda $38	; get (indicator lamp control byte)
                    and #$08	; %0000 1000 isolate bit 3: (video credits available bit???)
                    beq Lf0c1	; if (bit 3 low) branch to nearby (to ignore keypress)
                    lda $fb	; get (current-video-system-state???)
                    cmp #$02	; compare to 2
                    beq Lf0c1	; if (video system state = unavailable???) branch to nearby rts (to ignore keypress)
                    lda $36	; get the keypress code
                    cmp #$04	; keypress was a 4?
                    bne Lf0ee	; if not, branch ahead (must have been 3)
				; 
				; 1st keypress was a 4
				; 
                    lda $fe	; check (number of 4xx videos available)
                    beq Lf0c1	; is no 4xx videos available, branch to nearby rts (to ignore keypress)
                    bne Lf0f2	; (unconditional branch ahead)
				;
				; 1st keypress was a 3
				;
Lf0ee               lda $fc	; check (number of 3xx videos available)
                    beq Lf0c1	; if no 3xx videos available, branch to nearby rts (to ignore keypress)
				;
				; allowable 1st keypress of 3 or 4
				;
Lf0f2               sta $24	; copy $fc (or $fe) var value to $24 var (max allowable 3xx/4xx entry)
                    lda #$22	; \ set offset; output to $3d-$40 (really just $3f & $40)
                    sta $26	; / 
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
				;
Lf113               ldx $36	; get keypress code
                    lda #$ff	; A=$ff (for upcoming cmp)
                    cpx #$06	; was 6 pressed?
                    bcc Lf13e	; branch if keypress was < 6
				;
				; alternative credits per record for X6X, X7X, X8X and X9X
				; if memory location 45 is set
				; credit levels are per memory locations 46, 47, 48 and 49, respectively
				;
                    cmp $0328,x ;   compare ($ff) to $0328+X where X=6-9 (undocumented mem locs 46-49???)
                    bne Lf13e	;   if no match, branch to normal code
				;
				;   this group of selections is flagged for higher credit-pricing
				;   make sure enough credits are available
				;
                    lda $41	;   get (number of record credits)
                    cmp $032d	;   compare to memory location 45 (undocumented; alternative credit cost)
                    bcs Lf128	;   branch over next opcode if there are enough credits for the alt. cost
                    rts		;   return, ignoring keypress
Lf128               lda #$ff	;   \
                    sta $37	;   / raise (alternative-price-selection-used) flag
                    jmp Lf13e	;   jump to update entry table
				;
				; handle a 1st digit that's NOT 3 or 4
				;
Lf12f               lda $36	; get key just pressed
                    cmp #$01	; \ 
                    beq Lf13a	;  \ if it was a 1 or 2
                    cmp #$02	;  / branch over rts
                    beq Lf13a	; /  
                    rts		; otherwise rts (to ignore all 1st digits not 1-4)
				;
				; handle 1st digits of 1 or 2
				;
Lf13a               ldx #$00	; \ set selection type to records (not video)
                    stx $3b	; /
				;
Lf13e               jsr Sf89c	; update the multi-digit entry table
                    lda $23	; get # of digits-entered 
                    cmp #$03	; do we have all 3 digits?
                    beq Lf148	; if so, branch over the next opcode...
                    rts		; otherwise done here (wait for another keypress)
				;
				; all 3 digits have been entered ($28/$29/$2a)
				;
Lf148               lda #$00	; \
                    sta $23	; / reset digit position to 0
                    lda $28	; get 1st digit entered to A (1 or 2) [or 3 or 4?]
                    ldx #$00	; init X=0
                    lsr a	; shift bits of A right (bit 0 to carry) [checks odd or even; 1 or 2]
                    bcs Lf154	; if odd (carry set) skip next opcode (increment)
                    inx		;   increment X to 1, if 1XX was entered (for 2XX, X remains 0)
Lf154               stx $28	; store X (even/odd flag, 0/1) at $28 (entry now 000-199 vice 100-299)
                    jsr Sfb55	; convert 3-digit decimal entry to 2-byte binary (returns $2b/$2c)
				;
				; update appropriate counter and MEMOREC plays table
				;
                    ldx #$ee	; point to 'total record selections' counter by default
                    lda $3b	; check (video/records mode) flag
                    beq Lf161	; if (records) skip over next opcode
                    ldx #$e8	;   point to 'total video' counter instead
Lf161               jsr Sf18d	; increment total records (or videos) counter
                    jsr Sf8b1	; update MEMOREC plays tables
				;
				; update 50-byte page 1 table for POPULAR function
				;
                    lda #$02	; \ %0000 0010
                    and $3b	; / isolate bit 1 of (video/records mode) flag (why?)
                    bne Lf16f	; if bit set, skip opcode (leaving A %0000 0010 for videos)
                    lda #$01	;   use %0000 0001 bitmask for records
Lf16f               sta $08	; store bitmask in a temp var
                    lda $2c	; get converted selection number (0-199)
				;
				; calculate table offset (A) and bitmask ($08) for a selection # (A)
				;
Lf173               cmp #$32	; compare selection to 50
                    bcc Lf17f	;   exit loop (or never enter) if A < 50
                    sbc #$32	;   reduce A by 50
                    asl $08	;   \ shift bitmask left 2 bits
                    asl $08	;   / 
                    bcc Lf173	; loopback until carry gets set (a bit of $08 comes out)
				;
				; mark selection in 50-byte table for POPULAR function
				;
Lf17f               tax		; put 50-byte table offset in X
                    lda $08	; get bitmask to A
                    ora $0100,x	; \ sets bit in 50-byte table 
                    sta $0100,x	; /
                    ldx $2c	; selection number back
                    jmp Lf01a	; [finish by re-using code for the POPULAR button???]
				;
				;**********************************
				; INCREMENT COUNTER (e.g. MEMOREC) 
				;**********************************   
				;  uses 2-byte value, at X & X+1, into page 2 of RAM
				;  increments counter up to $270f (9999) maximum
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
				; handle video playlist
				; [alternate subroutine entry point]
				;
Sf1b6               txa		; X->A [called w/X=$40 & X=$a4]
                    sta $ca	; store as (selection-related data for wallbox???)
                    ldy $f8	; get the video selection write pointer (next entry)
                    sta $0300,y	; put selection # in video selection table 
                    iny		; increment video selection pointer
                    bne Lf1c3	; if not 0 skip next opcode
                    ldy #$90	;   wrap around index to $0390
Lf1c3               cpy $f7	; compare incremented Y to video playlist read index (next to play)
                    beq Lf1c9	; if playlist is full, skip next opcode (playlist full, don't move write index)
                    sty $f8	; update the video selection pointer
Lf1c9               rts		; done
				;
				; handle record playlist
				;
Lf1ca               stx $ca	; store X (new selection) as (selection-related data for wallbox???)
                    ldy #$ff	; \
                    sty $3c	;  } set (pending-selection-for-mech) flag
                    sty $70	; /  set (record-selections-active) flag
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
Lf1f3               cpy $ee	; compare to read FIFO selection index
                    beq Lf1c9	; if equal, FIFO queue is full; branch to a nearby rts (new selections overwrite until queue moves)
                    sty $e5	; update write FIFO selection index
                    lda #$61	; \ 
                    jsr Sfcff	; / put $61 command in wallbox queue (selection position update)
                    rts		; 
				;
				;   conventional selection mode (not FIFO)
				;
Lf1ff               lda #$00	; \ flag Xth entry in selection table
                    sta $0200,x	; / (dead-simple; doesn't matter if it was already flagged)
                    rts		; done (note no wallbox command queued)
                    		;
				;*****************
				; SCAN THE KEYPAD
				;*****************
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
				; alternate entry point
				;
Sf21b               ldx #$0b	; init counter to 11 (read 12 keys: 0-9, RESET, POPULAR)
Lf21d               lda #$80	; \
                    sta $36	; / store $80 as result (default if no key is pressed)
Lf221               lda $ff8c,x	; get Xth value from ROM table (select keypad selects/return)
                    sta $4002	;   write to PIA2-B
                    nop		;   micro-pause
                    lda $4000	;   \  read PIA2-A
                    ora $4000	;    } ...again???
                    ora $4000	;   /  ...again???
                    and #$02	;   %0000 0010 isolate bit 1: keypad/button inputs
                    beq Lf23f	;   if we have a hit (selected key is pressed) exit loop
                    dex		;   if no hit, decrement counter...
                    bpl Lf221	; loopback until we underrun (finished 0 then rolled X)
                    lda $47	; get debounce counter
                    beq Lf23e	; if 0, branch down to RTS
                    dec $47	;   otherwise decrement debounce counter
Lf23e               rts		; no key pressed, done here
				;
				; keypress detected?
				;
Lf23f               lda $47	; get debounce counter
                    bne Lf247	; if non-zero branch down
                    stx $36	;   otherwise, store X (key pressed) at $36
                    stx $9c	;   and at $9c
Lf247               lda #$0a	; \ init debounce counter to 10
                    sta $47	; /
                    rts		; done
                    		;
				;*********************
				; CREDIT CALCULATIONS
				;*********************
				; first entry: called from only 1 place, near f068
				; 2nd   entry: called from only 1 place, near ec13
				;
				;  remaining issues mostly relate to variables: $02f9, $02fb, $02fc, $02fd
				;
Lf24c               lda $02fb	; get (credit-related counter???)
                    bne Sf256	; if not zero, branch ahead
                    lda #$01	;   \ set it to 1
                    sta $02fb	;   /
				;
				; process new regular (records) credits
				; (alternate entry point to subroutine)
				;
Sf256               lda $02f5	; get master count of (record) credits remaining
                    clc		; \ add pending credits (records)
                    adc $02f4	; /
                    bcc Lf261	; \ 
                    lda #$ff	;  } prevent rollover; clamp to max of 255
Lf261               sta $02f5	; /
				;
				; process new video credits
				;
                    lda $02f6	; get master count of pending video credits
                    clc		; \ add new video credits
                    adc $02f7	; /
                    bcc Lf26f	; \
                    lda #$ff	;  } prevent rollover; clamp to max of 255
Lf26f               sta $02f7	; /
				;
                    lda #$00	; \ 
                    sta $02f6	;  } 0 out pending credits (they've now been added)
                    sta $02f4	; /  for both videos and for records
                    lda $97	; \ check (credit overflow variable???)
                    beq Lf27f	; / if 0, skip next opcode
                    rts		;   done
Lf27f               lda $3b	; get records/video mode (record mode=0)
                    bne Lf298	; branch down for video mode
				;
				; records mode
				;
                    lda $37	; was an alternative credit selection just made? (Boolean)
                    beq Lf29b	; branch down if not
				;
				; handle an "alternative price" (nulti-credit) selection [undocumented feature]
				;
                    lda $02f5	; get master count of records credits remaining
                    sec		; \
                    sbc $032d	; / subtract value (undocumented mem loc 45; alternate credit cost) from credit total
                    bcs Lf292	; \  
                    lda #$00	;  } prevent underflow; clamp min to 0
Lf292               sta $02f5	; /
                    jmp Lf2a3	; jump down
				;
Lf298               jmp Lf316	; jump further for video mode (this extends a branch from above)
				; 
				; regular (not "alternative") credit use
				;
Lf29b               lda $02f5	; get master count of records credits remaining
                    beq Lf2a3	; if 0 skip next opcode
                    dec $02f5	;   if there's at least 1 credit, decrement 1 credit
				;
Lf2a3               lda $031e	; get records/video system setting (0=video, $ff=records-only)
                    eor #$ff	; flip its Boolean value (now 0=records-only, $ff=video)
                    beq Lf2c2	; branch down if 0 (ie. records-only mode)
				;
				; in video mode: calc records-to-video-credit ratio???
				;
                    lda $02f5	;   \ copy credit total (records) to dividend
                    sta $16	;   /
                    lda #$00	;   \ dividend MSB to 0 
                    sta $15	;   /
                    lda $02fb	;   \ copy (???) to divisor
                    sta $18	;   /
                    lda $02fc	;   \ copy (???) to divisor
                    sta $17	;   /
                    jsr Sec5c	;   do division with $15-$18
                    lda $15	;   get quotient to A
				;
Lf2c2               sta $02f7	; store 0 or calculated conversation factor???
                    lda $02f9	; \ $02f9 (money left over???) to $02f8 (money-in)
Lf2c8               sta $02f8	; /
                    lda #$00	; \
                    sta $3b	;  } zero out (alt. price selection) flag and (record/video) flag
                    sta $37	; /
                    lda #$ff	; \ 
                    sta $4c	; / set all bits of $4c bitmask var high
				;
                    cmp $031b	; \ compare A=$ff to freeplay setting
                    bne Lf2e6	; / in regular pay (non-freeplay) mode branch down
				;
				; freeplay mode
				;
                    lda $88	; check (consecutive POPULAR counter)
                    cmp #$0b	; compare to 11
                    bcc Lf312	; branch way down if < 11
                    lda #$00	;   \ zero out the counter
                    sta $88	;   / (A will also server to clear bitmask next)
                    beq Lf302	;   (always branch)
				;
				; in regular pay (non-freeplay) mode
				;
Lf2e6               lda $02f5	; get current (records) credit total
                    bne Lf2f9	; if record credits are present branch down
				;
				; handle running out of credits (end of a customer transaction)
				;
                    lda #$0a	; \ 10 seconds
                    sta $9a	; / set timer value for 10s of "THANK YOU"
                    lda #$02	; \ %0000 0010
                    sta $38	; / turn on THANK YOU lamp
                    lda #$aa	; \ %1010 1010 (clear record bits?)
                    and $4c	; / clear even bits of $4c bitmask (for 50-byte table)
                    sta $4c	; store it back
				;
Lf2f9               lda $02f7	; check $02f7 (video credits)
                    bne Lf304	; if non-zero branch ahead
                    lda #$55	;   bitmask %0101 0101 (clear video bits?)
                    and $4c	;   clear some bits of $4c bitmask (for 50-byte table)
				;
				; freeplay mode path rejoins here
				; update 50-byte table
				;
Lf302               sta $4c	; write bitmask back
Lf304               ldy #$32	; loop counter for the 50-byte table
Lf306               lda $4c	;   get $4c bitmask
                    dey		;   decrement counter
                    and $0100,y	;   \ clear bits based in table per bitmask
                    sta $0100,y	;   / update table value
                    tya		;   (set Z flag based on loop counter)
                    bne Lf306	; loopback for entire table
				;
Lf312               jsr Se59c	; add credits and update LED displays
                    rts		; done
				;
				; video mode
				;
Lf316               lda $02f7	; get current video credits
                    beq Lf31e	; \  decrement video credits, unless already 0
                    dec $02f7	; /
Lf31e               clc		; (clear carry flag for addition)
                    lda $02fc	; get (video credit related???)
                    adc $02fd	; add (video credit related???)
                    sta $02fd	; store to (video credit related???)
                    lda #$00	; \
                    adc $02fb	; / add (carry bit) to (money left over???)
                    eor #$ff	; \ invert every bit \ 2's complement?
                    sec		; / set carry flag   /
                    adc $02f5	; add # of record credits (adding 2s compl is really subtracting)
                    bcs Lf337	; skip next opcode if carry flag set?
                    lda #$00	;   A=0
Lf337               sta $02f5	; store updated record credit total
                    cmp #$01	; do we have just 1 record credit?
                    bne Lf348	; if not, branch down
                    lda #$e0	;   A=224
                    cmp $02fd	;   compare to (credit-related???)
                    bcs Lf348	;   skip next opcode if carry flag set
                    dec $02f5	;     decrement record credit count
Lf348               lda $02f5	; get number of record credits
                    bne Lf355	; branch if non-zero
                    lda $02f7	;   \ get (video credits remaining)
                    beq Lf355	;   / if 0 skip next opcode
                    inc $02f5	;     increment records credit count
Lf355               lda $02fa	; get money-left-over var
                    jmp Lf2c8	; loopback a ways and continue
                    		;
				;***********************************
				; HANDLE USER ENTRY IN SERVICE MODE
				;***********************************
				;
Sf35b               jsr Sf975	; handle the CCC-mounted buttons & switches
                    jsr Sf205	; read the keypad (including POPULAR + #)
                    lda $36	; get keypress code
                    bmi Lf372	; branch to rts if no key is pressed ($80, minus flag set) 
                    cmp #$0a	; is the RESET key pressed?
                    bne Lf373	; if not, branch down to check the POPULAR key next
				;
				; handle RESET keypress
				;
                    lda #$00	; \ reset key entry position to 0
                    sta $23	; /
                    sta $58	; clear entry counter/flag
                    jsr Sff31	;   clear top LED displays
Lf372               rts		; done
				;
Lf373               lda $66	; get POPULAR key status
                    bne Lf37b	; if not pressed, branch ahead
                    lda #$ff	;   \ incidate popular key status
                    sta $58	;   / using a counter/flag
Lf37b               lda $23	; get current position in multi-digit entry
                    bne Lf382	; skip next opcode if non-zero
                    jsr Sff31	;   clear top LED displays
Lf382               inc $58	; increment entry counter/flag
                    jsr Sf89c	; update 3-digit entry table
                    lda $23	; get current position in multi-digit entry
                    cmp #$03	; have 3 digits been entered yet?
                    bne Lf372	; if NOT, branch back to rts
				;
				; 3 digits have been entered
				;
                    lda #$00	; \ set entry position back to 0
                    sta $23	; /
                    ldx $58	; get counter/flag status to X (was this a security code entry?)
                    sta $58	; reset POPULAR status/counter to 0
                    sta $81	; init (LSB of MEMOREC pointer) to 0 (why???)
                    cpx #$03	; were 3 digits entered(?)
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
				; incorrect "security code" entered :(
				;
Lf3b3               inc $8a	; increment wrong-security-code counter
                    ldy $8a	; get the count
                    cpy #$03	; has counter reached 3?
                    bcc Lf372	; if not, branch to RTS
                    lda #$00	;   if so, someone entered POPULAR+000 3 times
                    sta $8a	;   zero out wrong-security-code counter
                    sta $033a	;   \ reset the "security" code to 000
                    sta $033b	;   /
                    jsr Sef18	;   update RAM checksum
                    rts		; done
                    		;
				; handle a 3-digit input (NOT a security code)
				;
Lf3c9               lda #$00	; \
                    sta $58	;  } clear popular key status flag (again)
                    sta $81	; /  clear LSB of pointer
                    lda $29	; get middle (10s) digit of entry
                    asl a	; \		       \
                    asl a	;  } multiply by 8	\
                    asl a	; /		 	 \  overall effect
                    sta $06	; store to temp var       } is multiply by 10
                    lsr a	; \ now divide by 4	 /  & convert to binary
                    lsr a	; / (original*2)	/
                    adc $06	; add temp var back in /
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
				; handle 000-058 (display logged serial data)
				;
                    adc #$33	; add 51 to A (now 51-109, $33-$6d)
                    tax		; copy A to X
                    lda $0100,x	; get a serial log table value ($0133-$016d)
                    cpx #$33	; compare X to 51 (i.e. code 000)
                    bne Lf3f4	; skip next opcode if not equal
                    sbc #$33	;   for code 000: subtract 51, resulting in the effective table position/index
Lf3f4               jmp Lf40e	; use end of 5XX code handler (display single-byte value)
				;
				; handle 059-099 (display page 0 values)
				;
Lf3f7               adc #$9f	; add 159 to A (now 218-002, $da-$02, due to wraparound) [mostly video-related variables]
                    tax		; copy A to X
                    lda $00,x	; get zero-page value, address X
                    jmp Lf40e	; use end of 5XX code handler (display a single-byte value)
				;
				; 5XX CODES: SHOW COUNTER TOTALS
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
                    rts		;   done here
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
				; 4XX/3XX CODES: [undocumented] (video MEMOREC data)
				;
Lf436               lda #$07	; \
                    sta $82	; / set MSB of pointer to page 7 for video data
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
				; 8XX CODES [any 8XX works, not just 800]	
				;
                    cmp #$08	; compare A to 8 (for 8XX codes)
                    bne Lf467	; if not 8, branch down next check...
                    jsr Se453	; update LED displays
                    jsr Se002	; perform RAM dump for an "inteROWEgator"
                    jmp Sff31	; clear top LED displays (then rts from there)
				;
				; 9XX CODES
				;
Lf467               cmp #$09	; compare A to 9 (for 9XX code)
                    bne Lf47b	; if not, branch to rts
                    ldy #$63	; \ 
                    cpy $06	; / are 2nd & 3rd digits 99?
                    bne Lf47b	; if not 99, branch to rts [ignores all 9XX except 999]
                    sty $cb	;   \ put (99 decimal)
                    sty $cd	;   / at $cb and $cd (to indicate warm reboot?)
                    jsr Se453	;   update LED displays
                    jmp Reset	;   reboot the juke!
Lf47b               rts		; done
				;
				; 7XX CODES
				;
Lf47c               ldy $06	; get XX part of code to Y
                    bne Lf48b	; if > 0, branch down to check more codes...
				;
				;   CODE 700: ADD $0.25 CREDIT
				;
                    lda #$ff	; \
                    sta $a0	;  } set $a0 flag (indicate free credit to wallbox???)
                    sta $cc	; /  set $cc flag (checked at boot-time???)
                    lda #$05	; 5 nickels
                    jmp Leb62	; to add credits code [gives credit; but NOT counted] (rts from there)
				; 
				;   701 CODE: CLEAR CREDITS
				;
Lf48b               cpy #$01	; is Y=1?
                    bne Lf493	; if not, branch down to check more codes...
                    jsr Se749	;   clear some variables, including current credit-count
                    rts		;   done here
				;
				;   702 CODE: CLEAR AUTOPLAY LIST
				;
Lf493               cpy #$02	; is Y ==2 ?
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
                    sta $e5	;   zero (FIFO write index)
                    sta $ee	;   zero (FIFO read index)
                    sta $81	;   \
                    lda #$02	;    } $81/$82 -> $0200 (base of selection list)
                    sta $82	;   / 
                    inc $dc	;   increment counter for usages of thie code (never used in firmware???)
                    lda #$ff	;   A = $ff
                    ldy #$c7	;   Y = $c7 (199)
Lf4c9               sta ($81),y	;   put $ff at $0200 + Y    \
                    dey		;       decrement Y counter  \ $0200-$02c7 filled w/$ff 1st tmie
                    bne Lf4c9	;     loopback until Y=0     / $0100-$0131 filled w/$00 2nd time
                    sta ($81),y	;     once more with Y=0    /
                    dec $82	;     decrement $82 from 2 to 1 (now -> $0100)
                    beq Lf4db	;     if decremented a 2nd time, exit loop
                    lda #$00	;     A=0 (fill with 0 this time)
                    ldy #$31	;     (50)
                    jmp Lf4c9 	;   jump back up
Lf4db               lda #$90	;   \
                    sta $f7	;    } init video selection playlist indicies -> $0390
                    sta $f8	;   /
                    lda #$3a	;   \ send $3a command to video system
                    sta $eb	;   / (playlist cleared)
                    ldx #$90	;   init counter X=$90 (144)
                    lda #$00	;   A=0
Lf4e9               sta $0300,x	;   \  
                    inx		;    } zero out RAM values from $0390 to $03ff (video playlist?)
                    bne Lf4e9	;   /
                    lda $ed	;   get (video-is-playing???) flag
                    beq Lf4f5	;   branch to rts if no video playing
                    sta $f4	;     copy flag status (high) to (transfer-in-progress???) flag
Lf4f5               rts		;   done
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
Lf50b               rts		;   done here
				;
				;   CODE 770: VIDEO SYSTEM INIT
				;
Lf50c               cpy #$46	; compare Y to 70 (for code 770: video system init)
                    bne Lf523	; if not, branch to next check...
                    lda #$00	;   \
                    sta $f9	;    \ init 3 video-related variables to 0
                    sta $e8	;    /
                    sta $ea	;   /
                    lda #$ff	;   \
                    sta $f1	;   / inhibit (time-until-video???) flag/timer
                    lda #$0a	;   \
                    sta $eb	;    } send command code $0a to video system
                    sta $fb	;   /  set (video-system-state???) to $0a
                    rts		;   done
				;
				;   CODE 703: [undocumented; adds video selections 64 and 164 to video playlist]
				;             possibly causes the video system to enumerate videos available???
				;
Lf523               cpy #$03	; is Y=3?
                    bne Lf50b	; if not, branch to a nearby rts (no more valid codes)
                    jsr Sf4b7	;   clear selection list
                    ldx #$40	;   \ X=$40 (64)
                    jsr Sf1b6	;   / add to video playlist
                    ldx #$a4	;   \ X=$a4 (164)
                    jsr Sf1b6	;   / add to video playlist
                    rts		;   done
                    		;
				;*************
				; DO AUTOPLAY
				;*************
				; [needs more work in the video portion]
				;
Sf535               lda $0321	; \ reset autoplay timer ($fa) with from programmed value
                    sta $fa	; /
                    ldx #$ea	; \ increment "total autoplays" (0-9999) counter
                    jsr Sf18d	; /
                    inc $e1	; \ increment the autoplay counter and get to A
                    lda $e1	; /
                    cmp #$c8	; \
                    lda #$00	;  \ wrap around from 200 to 0 if needed 
                    bcc Lf54b	;  /
                    sta $e1	; /
Lf54b               sta $84	; set record/video flag to record 
                    lda $0320	; get autoplay style setting
                    cmp #$05	; \
                    bcc Lf5ba	; / if style < 5 branch way down (A holds mode #)
                    cmp #$05	; \ if set to style 5,
                    beq Lf55b	; / skip this jmp
                    jmp Lf5f2	; for style > 5, use style 1 code (and a tweak to come later)
				;
                    		; handle autoplay style 5: custom list
				;
Lf55b               ldx $e6	; get (list autoplay position) to X 
                    txa		; \ put (list autoplay position) on stack  
                    pha		; /
                    lda $033c,x	; \ fetch selection LSB from the custom list
                    sta $24	; / store LSB (to be converted to digits)
                    inx		; increment to next byte in memory
                    lda $033c,x	; \ fetch selection MSB from the custom list [these are 2-byte entries]
                    sta $25	; / store MSB (to be converted to digits)
                    lda #$00	; \ offset for output
                    sta $26	; / will be stored at $1b-1e ($1b not used here)
                    jsr See45	; parse binary number into decimal digits
				;
                    dec $1c	; decrement 100s digit (4XX->3XX; 3XX->2XX; 2XX -> 1XX; 1XX -> 0XX)
Lf573               lda $1c	; get 100s digit
                    cmp #$02	;   \ compare 100s digit to 2
                    bcc Lf583	;   / exit loop if < 2 (i.e if selection was a record)
                    dec $1c	;   \ decrement 1st digit twice more
                    dec $1c	;   / (3XX -> 1XX; 2XX -> 0XX)
                    lda #$ff	;   \ 
                    sta $84	;   / set record/video flag to indicate a video selection was used
                    bne Lf573	; (always loopback)
				;
Lf583               lda $1c	; \ put 100s digit position for conversion to binary
                    sta $1b	; /   
                    lda $1d	; \ put 10s digit position for conversion to binary
                    sta $1c	; / 
                    lda $1e	; \ put 1s digit position for conversion to binary
                    sta $1d	; / 
                    jsr See9e	; convert 3-digit selection [now 000-199] back to binary
				;
                    ldx $2c	; get binary selection to X
                    lda $84	; get record/video selection flag
                    beq Lf5a1	; if NOT a video, skip the next block
				;
				; selection is a video (300-499)
				;
                    jsr Sf1b6	;   add selection to video playlist
                    lda #$00	;   \
                    sta $f1	;   / init (video countdown timer/flag?)
                    beq Lf5a4	;   unconditional branch over next opcode
				;
Lf5a1               jsr Lf1ca	; put (record) selection onto playlist
				;
Lf5a4               pla		; pull (autoplay list position) off stack (ref. $f55e)
                    clc		; \
                    adc #$02	; / add 2 to position (there are 2 bytes per entry)
                    tax		; copy (autoplay list position) into X
                    cmp #$1d	; compare (autoplay list postion) to 29 [edit to $29, 41 decimal]
                    bcc Lf5b2	; if < 29 branch down
Lf5ad               ldx #$00	;   X=0 at 30; roll back to 0
                    stx $e6	;   reset autoplay list position to 0
                    rts		;   done here
Lf5b2               stx $e6	; store autoplay list position
                    lda $033c,x	; get autoplay custom programmed table value based on X
                    beq Lf5ad	; init position if autoplay table value is 0
                    rts		;   if non-zero, done here (returning selection # in A)
				;
				; handle non-list autoplay modes
				; [needs more attention]
				;
Lf5ba               cmp #$03	; compare mode to 3
                    bcc Lf5ed	; is A < 3 branch down
				;
				; autoplay styles 3 and 4 (video)
				;
                    lda $fc	; get (number of 3XX videos available)
                    clc		; \
                    adc $fd	; / add add'l 3XX videos avail
                    cmp $e1	; compare to (autoplay counter)
                    bcs Lf5e2	; branch number of 3XX videos > counter 
                    lda $fe	; \ get (number of 4xx videos available)
                    beq Lf5de	; / branch if 0	
                    ldx $e1	; get (autoplay counter)
                    cpx #$64	; compare to 100
                    bcs Lf5d5	; 
                    ldx #$64	; \ set autoplay counter to 100
                    stx $e1	; /
Lf5d5               clc		; \ 
                    adc $ff	;  } add (number of add'l 4XX videos avail)
                    adc #$64	; /  and add 100
                    cmp $e1	; \ compare to autoplay counter 
                    bcs Lf5e2	; / if that works, skip ahead and use it
Lf5de               lda #$00	; \ otherwie reset the autoplay counter to 0
                    sta $e1	; /
Lf5e2               lda $e1	; \ get autoplay counter and copy to X
                    tax		; /
                    lda #$00	; \ set (time until video is ready) to 0???
                    sta $f1	; /
                    jsr Sf1b6	; add video in X to the playlist
                    rts		; done here
				;
				; autoplay style < 3 
				;
Lf5ed               tax		; A->X
                    cpx #$02	; set to autoplay style 2? (B-sides)
                    beq Lf5fd	; if so, branch down
				; 
				; handle autoplay style 1 (A-sides only) [by process of elimination]
				;
Lf5f2               lda $e1	; get autoplay position counter
                    cmp #$64	; \ branch ahead if A < 100
                    bcc Lf605	; /
                    sbc #$64	;   subtract 100 (make B-sides into A-sides) 
                    jmp Lf605	; (jump ahead to XX8/XX9 check in common part of code)
                    		;
				; handle autoplay mode 2 (B-sides only)
				;
Lf5fd               lda $e1	; get current autoplay counter
                    cmp #$64	; compare to 100
                    bcs Lf605	; if A >= 100 skip next opcode
Lf603               adc #$64	;   A=A+100 (to make A-sides into B-sides?)
				;
				; check for XX8/XX9 selection disable
				; [modes 1 and 2 join here]
				;
Lf605               ldx $031d	; are XX8 & XX9 selections disabled?
                    bne Lf611	; branch down if disabled
                    ldx $39	;   check video mode boolean
                    bne Lf611	;   if (video mode?) branch over next opcode
                    jmp Lf634	;     jump down a bit
				;
				; skip over XX8/XX9 if necessary
				;
Lf611               sta $24	; store (selection #) at $24 for conv. to digits
                    ldx #$00	; \ offset of 0 (output to $1b-$1e)
                    stx $26	; /
                    stx $25	; MSB of 0 (selection number always < 256)
                    jsr See45	; convert selection # to decimal digits
                    ldx $1e	; get the ones digit
                    cpx #$08	; \ compare ones digit to 8
                    bcc Lf634	; / if < 8 branch ahead (this selection is OK)
                    adc #$01	;   add 2 to selection (+1 for the set carry bit)
                    cmp #$64	;   compare selection to 100
                    bne Lf62c	;   if !=100 skip ahead
                    lda #$00	;     \ if 100, make it 0 instead
                    beq Lf632	;     / always branch
Lf62c               cmp #$c8	;   compare A to 200
                    bcc Lf632	;   if < 200 branch over next
                    lda #$64	;     if 200, make it 100 instead
				;
Lf632               sta $e1	; store updated selection #
Lf634               tax		; copy A to X (A & X both old selection #)
                    pha		; push (selection #)
                    jsr Lf1ca	; put selection into playlist
                    pla		; pull (selection #)
				;
				; tweak for autoplay style 6 (A and B sides)
				;
                    ldx $0320	; \  get autoplay style setting
                    cpx #$06	;  } if style < 6
                    bcc Lf645	; /  branch to rts
                    cmp #$64	; \ compare selection # to 100 
                    bcc Lf603	; / if < 100, also cue up the B-side with each A-side
Lf645               rts		; 
                    		;
				;****************************************
				; MONITOR SERVICE SWITCH, MECH AND COINS
				;****************************************
				;  called from 2 places
				;
Sf646               jsr Se56a	; read PIA1-A and service switch (incl. override)
                    jsr Sf653	; manage magaine position
                    jsr Sf775	; handle record mech (transfer?)
                    jsr Sea9c	; take & count money; give credits
                    rts		;
                    		;**************************
				; MANAGE MAGAZINE POSITION
				;**************************
				;
Sf653               lda $68	; \ get PIA1-A status
                    and #$20	; / %0010 0000 isolate bit 5: INDEX opto
                    bne Lf660	; branch if bit 5 high (active)
                    lda $59	;   get (edge detector)
                    beq Lf65f	;   if (edge detector)=0 branch to rts
                    dec $59	;   decrement (edge detector) counter
Lf65f               rts		;   done
				;
Lf660               lda $59	; get debounce counter
                    bne Lf65f	; branch to rts if not 0 yet
				;
				; index opto now debounced
				;
                    lda $2002	; \  get PIA1-B
                    and #$06	;  } isolate bits 1 & 2 (%0000 0110) DETENT & MAGazine MOTOR
                    cmp #$06	; /  check if both are high (active) i.e. magazine is spinning
                    bne Lf65f	; if not, branch to rts (magazine stationary)
				;
				; magazine spinning
				;
                    lda #$01	; \
                    sta $59	; / reset index debounce counter to 1
                    inc $67	; increment (current magazine position)
                    lda #$ff	; \ set (index pulse rec'd???) flag
                    sta $f0	; /
				;
				; check for HOME position
				;
                    lda $68	; \  get PIA1-A status
                    and #$10	;  } %0001 0000 isolate bit 4: HOME opto
                    bne Lf683	; /  branch ahead if bit 4 is high (active)
				;
				; HOME opto not active, but still check for position wraparound
				;
                    lda $67	; get current magazine position
                    cmp #$64	; compare magazine position 100
                    bcc Lf68a	; branch down if position < 100
				;
Lf683               lda #$00	; \ wrap magazine position around to 0
                    sta $67	; / (if HOME or if position counter reaches 100)
                    jsr Sf73c	; [check selection list & set variables???]
				;
Lf68a               lda $68	; \ get PIA1-A status
                    and #$04	; / %0000 0100 isolate bit 2: SERVICE / ON
                    beq Lf69a	; branch ahead if low (ON mode???)
				;
				; allow magazine rotation with CANCEL in SERVICE mode???
				;
                    lda $68	;   \ get PIA1-A status
                    and #$80	;   / %1000 0000 isolate bit 7: CANCEL
                    bne Lf699	;   branch to rts if CANCEL pressed???
                    jsr Sff4d	;     turn DETENT off
Lf699               rts		;   done
				;
Lf69a               lda $74	; \ check (magazine init counter) var
                    beq Lf6a1	; / branch down if magazine already initialized
				;
				; initializing magazine (make full rotation)
				;
                    dec $74	; decrement (magazine init counter)
                    rts		; done
				;
				; initialized magazine
				;
Lf6a1               lda $ea	; \ check (flag related to fill-in record???)
                    beq Lf6be	; / branch down if 0
                    lda $e9	;   get (fill-in target???)
                    cmp $67	;   compare to current magazine position
                    bne Lf6d0	;   branch ahead if not equal
                    sta $ef	;     store A to $ef ($e9=$67=selection=magazine position)
                    lda #$eb	;     \ 2nd parameter for wallbox command $62???
                    jsr Sf72e	;     / prepare and queue a wallbox message (playing fill-in???)
                    lda #$ff	;     \ set (turntable motor???) flag
                    sta $f3	;     /
                    lda #$00	;     \ set (B-side flag???) low
                    sta $f2	;     /
Lf6ba               jsr Sff4d	;     turn DETENT off (meaning lock magazine???)
                    rts		;  done
				;
				; (fill-in record) flag is low
				;
Lf6be               lda $70	; get (record selection active???) flag
                    beq Lf6ba	; if flag low, branch back
                    inc $5a	;   increment (wallbox comm counter???)
                    lda #$00	;   \ clear (turntable motor???) flag
                    sta $f3	;   /
                    lda #$ff	;   \ set (B-side flag???) high
                    sta $f2	;   /
                    jsr Sf6d9	;   [subroutine to check spinning magazine???]
                    rts		;   done
				;
Lf6d0               cmp #$64	; compare (fill-in target???) to (100)
                    bcc Lf6d8	; if A < 100 branch to rts
                    lda #$63	;   \ set (fill-in target???) to (99) instead
                    sta $e9	;   /
Lf6d8               rts		; done
				;
                    		;*******************************************
				; CHECK SPINNING MAGAZINE for SELECTION HIT
				;*******************************************
				; called from 2 places
				;
Sf6d9               lda $67	; get (current magazine position 0-99) to A 
                    ldx $f2	; get (A/B-side Boolean) to X 
                    bne Lf6e2	; if (A/B Boolean) not 0, skip next 2 opcodes
                    clc		;   \ if A/B Boolean is low (0) then
                    adc #$64	;   / add 100 to magazine position 
Lf6e2               sta $ef	; store (side-corrected magazine position)
                    tax		; copy (side-corrected magazine position) to X register
                    lda $0339	; get FIFO/conventional setting
                    bne Lf6fe	; branch ahead for FIFO mode
				; 
				; conventional selection mode
				;
                    lda $0200,x	; get entry in selection list for record passing by
                    cmp #$c8	; compare entry to (200) [has this record been selected?]
                    bcc Lf735	; branch down if A < 200 [branch if record is selected]
Lf6f1               lda $f0	;   get (index pulse rec'd???) flag
                    beq Lf6fd	;   branch to rts flag is low
                    lda $f2	;     get (A/B Boolean) flag
                    eor #$ff	;     flip boolean sense
                    sta $f2	;     store (flag) back
                    beq Sf6d9	;     branch back if $f2 is now 0 (check other side of this record???)
Lf6fd               rts		; done
				;
				; FIFO selection mode
				;
Lf6fe               ldx $ee	; get (read position in FIFO list) to X
                    lda $0200,x	; get Xth entry in FIFO list (selection to play next)
                    cmp $ef	; compare to (side-corrected magazine position)
                    bne Lf6f1	; if not equal, branch back to check the flip size???
				;
				; found record to play next (in FIFO mode)
				;
                    lda #$ff	; \ clear this slot in the playlist
                    sta $0200,x	; /
                    inc $ee	; increment read position in FIFO list position
                    lda #$61	; \ 
                    jsr Sfcff	; / put $61 code onto wallbox event queue ("positions in selection lists")
                    lda $ee	; \
                    cmp #$c8	;  \  check if selection list position reached end (200)
                    bcc Lf71d	;   }
                    lda #$00	;  /  if so, wrap around to position 0
                    sta $ee	; /
				;
				; need to play this record (common to both modes)
				;
Lf71d               lda #$ff	; \
                    eor $f3	;  } toggle boolean flag for turntable motor
                    sta $f3	; /
                    lda #$ff	; \
                    eor $f2	;  } toggle (A/B-side???) Boolean
                    sta $f2	; /
                    jsr Sff4d	; turn DETENT off (stop the magazine here)
                    lda #$db	; 2nd param for wallbox command $62 (selection notification) $db="record start"
				;
				; alt. entry point; reused by other (selection notification) codes
				; causes a wallbox command $62 to be generated by setting a selection type code
				;
Sf72e               sta $9e	; second parameter for wallbox command $62 (this will cause generation of command $62)
                    lda $ef	; get (A/B-side corrected selection)
                    sta $c9	; copy to (current selection sent to wallbox)
                    rts		; done
				;
				; (conventional mode, found a record)
				;
Lf735               lda #$ff	; A=$ff 
                    sta $0200,x	; put $ff in $0200 table at Xth position to [clear the mark]
                    bne Lf71d	; (unconditional) branch up to play it
				;
				;************************************
				; CHECK SELECTION LIST, UPDATE STATE
				;************************************
				;  called from 3 places:
				;      at $e6c6 (at boot time)
				;    near $f84b (after records ends/cancel)
				;    near $f68a (as magazine passes home)
				;  maybe some sort of selection list validation
				;    and/or state machine update/sync???
				;
				;  unknowns preventing progress:
				;	$70 (record selection active???) 
				;	$3c (selection cued up???)
				;	$f3 (fill-in record related???)
				;	$ea (fill-in record related???)
				;
Sf73c               lda $ea	; \ get (fill-in record related???) flag
                    bne Lf759	; / if high, branch to rts (setting another boolean first)
				;
                    ldx #$00	; start at beginning of selection table
Lf742               lda $0200,x	;   get selection table value
                    cmp #$c8	;   compare (value from table) to 200
                    bcs Lf75c	;   branch ahead if A >= 200 (conventional mode empty slots are $ff)
				;
				; non-$ff slot found
				;
                    lda $ee	;   get (position of selection index in FIFO mode)
                    cmp #$c8	;   compare to 200
                    bcc Lf76d	;   if A < 200 branch
Lf74f               jsr Sf4b7	;     clear selection list (invalid FIFO index???)
                    jmp Lf761	;     jump down
				;
Lf755               lda #$ff	;   set selection-related Booleans
				;
				;    branch arrives here with A=0 for clear flags instead
				;
Lf757               sta $70	;    (record selection active???) flag
Lf759               sta $3c	;    (selection cued up???) flag
                    rts		; done
				;
				; $ff slot in selection table
				;
Lf75c               inx		; next table position
                    cpx #$c8	; end of table? (200)
                    bcc Lf742	; if not, loopback to check next slot
				;
Lf761               lda #$00	; \ check (fill-in record related???) flag	 
                    cmp $ea	; / is it low?
                    beq Lf757	; if so, branch back to clear flags then rts
                    cmp $f3	; is (fill-in record related???) flag = (turntable motor???) Boolean?
                    beq Lf755	; if equal, branch back to set flags to $ff and rts
                    bne Lf757	; if not, branch back to set flags to 0 and rts
				;
Lf76d               lda $e5	; \ get (write position in selection table)
                    cmp #$c8	; / compare to 200
                    bcc Lf755	; if < 200, branch back to set flags then rts
                    bcs Lf74f	; if >=200, branch back to continue where we left off
				;
				;********************
				; MANAGE RECORD MECH
				;********************
				; unknowns preventing progress:
				;	$3c (pending-selection-for-mech???) flag
				;	$69 (video-related counter/timer???)
				;	$74
				;	$85 (transfer timer???)
				;	$e7
				;	$e8 (transfer or end-of-play???) flag
				;	$ea
				;	$ec
				;	$ed (video playing???) flag
				;	$f0
				;	$f4 (transfer-in-progress???) flag
				;
Sf775               lda $63	; get detent timer
                    bne Lf78b	; skip down if not expired
				;
				; detent timer has expired
				; 
                    lda $2002	; \ get PIA1-B status
                    and #$02	; / %0000 0010 isolate bit 1: DETENT output
                    asl a	; shift bit left (from b1 to b2)
                    sta $06	; store as temp var
                    lda $2002	; \  get PIA1-B status
                    and #$fb	;  \ %1111 1011 AND to clear bit 2
                    ora $06	;  / OR with temp var; re-sets bit 2 if DETENT is on
                    sta $2002	; /  output to PIA1-B: MAGAZINE MOTOR off unless DETENT is on
				;
				; detent timer not expired
				;
Lf78b               dec $63	; decrement detent timer
                    lda $e8	; \ check (transfer or end-of-play???) flag
                    bne Lf7b8	; / nonzero branch (to jmp, to 'cancel' function)
                    lda $f3	; \ check (turntable motor boolean)
                    bne Lf7ec	; / if turntable is on, manage transfer
				;
				; when turntable is off (transferring off???)
				;
                    lda $68	; \ get PIA1-A status
                    and #$40	; / %0100 0000 isolate bit 6: INNER CAM
                    beq Lf802	; if 0, go turn on tranfer motor
				; otherwise,
                    lda $39	; \ check video/records mode boolean
                    bne Lf7b1	; / branch in video mode
                    lda $031e	; \ get records/video SETTING
                    bne Lf7bb	; / branch in records-only SETTING
                    lda $69	; \ check (video-related counter/timer???) [stays $08 normally]
                    beq Lf7bb	; / branch if 0
                    dec $85	; decrement transfer timer???
                    bne Lf7ae	; skip next opcode if timer not expired
                    dec $69	;   decrement (video-related counter???)
Lf7ae               jmp Lf864	; jump to 'transfer in progress'????
				;
				; video mode related
				;
Lf7b1               lda $ed	; \ check (video playing???) flag
                    bne Lf7bb	; /
Lf7b5               jmp Lf864	; jump to 'transfer in progress'?
				;
Lf7b8               jmp Lf837	; (extents a branch above)
				;
Lf7bb               lda $f4	; \ (transfer-in-progress???) flag
                    bne Lf7b5	; / to 'transfer in progress'
                    lda $3c	; \ (pending-selection-for-mech???) flag
                    beq Lf7b5	; / to 'transfer in progress'
                    lda $35	; \ service/normal mode flag
                    bne Lf7b5	; / in service mode, to 'transfer in progress'
                    lda $74	; \ check if magazine is initialized
                    bne Lf7de	; / if not, branch to ???
                    lda $e7	; \ check (mech-index-position???) flag
                    beq Lf7de	; /
                    lda $f0	; \ check (new-magazine-index-pulse???) flag
                    beq Lf7de	; /
                    jsr Sf6d9	; check the spinning magazine for a selection hit
                    lda #$00	; \
                    sta $f0	;  } clear 2 flags, (new-magazine-index-pulse???)
                    sta $e7	; /  and (mech-index-position???)
                    beq Sf775	; (always) branch to top of mech routine
				;
				; ???
				;
Lf7de               lda #$00	; \ clear (pending-selection-for-mech???) flag
                    sta $3c	; /
                    jsr Sff5a	; turn on DETENT output (to release magazine for rotation)
                    lda #$ff	; \ set (magazine-index-position???) flag
                    sta $e7	; / 
                    jmp Lf864	; jump to 'transfer in progress'????
				;
				; begin/continue transfer???
				;
Lf7ec               lda #$10	; \  %0001 0000
                    ora $2002	;  } set bit 4 of PIA1-B: TURNTABLE MOTOR
                    sta $2002	; /  output to PIA1-B (turntable motor on)
                    lda $68	; \ get PIA1-A status	
                    and #$40	; / %0100 0000 isolate bit 6: INNER CAM
                    beq Lf80f	;   brance if INNER CAM switch
                    lda $2002	; \  get PIA-1B status
                    ora #$40	;  } set bit 6 %0100 0000: PLAY COUNTER
                    sta $2002	; /  output to PIA1-B
				;
				; turn on transfer motor
				;
Lf802               lda #$08	; \  %0000 1000
                    ora $2002	;  } set bit 3 of PIA1-B: TRANSFER MOTOR
                    sta $2002	; /  output to PIA1-B (turn transfer motor on)
                    lda #$18	; \ init (outer cam debounce???) to (24)
                    sta $42	; /
                    rts		; done here
				;
				; wait for transfer to finish???
				;
Lf80f               lda $42	; get (outer cam debounce??)
                    beq Lf828	; if debounce counter has expired, branch down
                    lda $f2	; \ check (A/B-side flag)
                    beq Lf81f	; / if 0, branch ahead (A-side???)
                    lda #$20	;   \  %0010 0000
                    ora $2002	;    } set bit 5 of PIA1-B: TOGGLE COIL
                    sta $2002	;   /  output to PIA1-B: turn on toggle coil (B-side???)
Lf81f               lda $68	; \ get PIA1-A status
                    and #$08	; / %0000 1000 isolate bit 3: OUTER CAM
                    beq Lf802	; if low, keep transfer motor on and rts
                    dec $42	;   otherwise, decrement (outer cam debounce???)
                    rts		;   done
				;
				; tranfer complete: unmute and check for cancel?
				;
Lf828               lda $2002	; \  get PIA1-B status
                    and #$97	;  \ clear bits %1001 0111 (TRANSFER, TOGGLE & PLAY COUNTER all off)
                    ora #$01	;  / set bit 0 - MUTE (to unmute amp)
                    sta $2002	; /  output to PIA1-B
                    bit $68	; set flags based on PIA1-A status
                    bmi Lf837	; skip next opcode based on bit 7 of PIA1-A (CANCEL signal)
                    rts		;   done here if bit 7 is low
				;
				; no record on TT or CANCEL button: mute amp & start transfer-off
				;
Lf837               lda #$ff	; \
                    sta $f4	;  } set (transfer-in-progress) flag
                    sta $f0	; /  set (new-magazine-index-pulse???) flag
                    ldy #$00	; \ 
                    sty $e8	;  } clear (mech-xfer-end-of-play) flag
                    sty $f3	; /  clear (turntable-playing) flag
                    lda #$dd	; 2nd parameter for wallbox code $62 (selection notification: "record end")
                    ldx $ea	; \ check (fill-in-record-related???) flag
                    beq Lf84b	; / skip next opcode flag is low
                    lda #$ed	;   instead, 2nd param for wallbox code $62 ("fill-in end")
Lf84b               jsr Sf72e	; prepare wallbox message with code $62
                    sty $ea	; (Y=0 here) clear (fill-in-related???) flag
                    lda #$fe	; \  %1111 1110
                    and $2002	;  } clear bit 0 of PIA1-B: MUTE
                    sta $2002	; /  output to PIA1-B
                    jsr Sf73c	; [check selection list, update state]
                    lda $68	; \ get PIA1-A status
                    and #$40	; / %0100 0000 isolate bit 6: INNER CAM
                    bne Lf86a	; if already active, branch to rts
                    jmp Lf802	; jump back to turn (keep) on transfer motor and rts
				;
				; (only reached by jmps)
				; transfer off in progress?
				;
Lf864               lda $42	; get (transfer debounce???)
                    beq Lf86b	; if debounce expired, skip ahead
                    dec $42	;   decrement (debounce counter)
Lf86a               rts		;   done
				;
				; transfer on/off complete?
				;
Lf86b               lda #$87	; \  %1000 0111
                    and $2002	;  } clear bits 3-6 of PIA1-B
                    sta $2002	; /  to PIA1-B (deactivates TFR MTR, TT, TOGGLE, PLAY CNTR)
                    lda $f4	; get (transfer-in-progress) flag
                    beq Lf881	; branch down if (transfer-in-progress) is clear
				;
				; (alt. entry point from housekeeping)
				;
Sf877               lda #$00	;   \ clear (transfer-in-process???) flag
                    sta $f4	;   /
                    sta $ed	;   \
                    sta $ec	;    } clear (video fill-in record-related flags???)
                    sta $ea	;   /
Lf881               lda $68	; \ get PIA1-A status
                    and #$04	; / %0000 0100 isolate bit 2: SERVICE/ON
                    beq Lf89b	; branch to rts if inactive (ON mode?)
                    lda $68	; \ get PIA1-A status
                    and #$80	; / %1000 0000 isolate bit 7: CANCEL
                    beq Lf89b	; branch to rts if CANCEL inactive
				;
				;   CANCEL (in service mode?)
				;
                    lda #$00	; \ set (magazine init counter???) to 0
                    sta $74	; /
                    lda $2002	; \ get PIA1-B (outputs)
                    and #$02	; / %0000 0010 isolate bit 1: DETENT
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
                    sta $29	;    } ($28-$2a)
                    sta $2a	;   /  
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
Sf8b1               jsr Sf95a	; do some time-critical background tasks before starting
                    lda #$00	; \
                    sta $76	;  \
                    lda #$05	;  / init pointer to $0500 (records MEMOREC table)
                    sta $77	; /
                    lda $3b	; \ get record/video Boolean
                    beq Lf8e1	; / branch down for records
				;
				; video mode
				;
                    clc		; clear carry flag for addition
                    lda $fc	; fetch (max 3xx selection available)
                    adc $fe	; add   (max 4xx selection available)
                    beq Lf8d1	; branch down if no videos available
                    ldy $fc	;   check (number of 3xx videos available)
                    beq Lf8d1	;   branch down if 0 (use 4xx total only)
                    ldy $fe	;     check (number of 4xx videos available)
                    beq Lf8d1	;     branch down if 0 (use 3xx total only)
                    adc #$01	;       1 greater than sum if both 3xx and 4xx selections exist (extra x00 count???)
Lf8d1               tay		; put calculated maximum in Y
                    lda #$07	; \ change MSB of pointer
                    sta $77	; / now -> $0700 (video selections table)
                    lda $2c	; get selection # (0-199)
                    cmp #$64	; compare to 100
                    bcc Lf8de	; if A < 100 skip next opcode
                    sbc #$24	;   A=A-36, convert video selection numbering to MEMOREC table numbering
				;          [300-363 = 000-063 => $00-$3F, 400-463 = 100-163 => $40-$7F]
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
Lf8fa               jsr Sf912	; go increment & sort a MEMOREC plays table
                    lda #$60	; \ wallbox command code $60 = "7 most popular selections"
                    jsr Sfcff	; / put it into wallbox queue
                    rts		; done
                    		;
				;****************************
				; SWAP MEMOREC TABLE ENTRIES
				;****************************
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
                    sta $06	; / init counter var to 8 (why???)
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
Lf939               rts		;   done here
Lf93a               cmp ($76),y	; compare (incremented value) to next entry in table  
                    bcs Lf93f	; if A >= table value, skip next opcode...
                    rts		;   done here (already in numerical order)
Lf93f               bne Lf94b	; if A <> table value, branch down
                    inc $77	;   (point to selection table)
                    lda ($76),y	;   get value from selection table
                    iny		;   move back to previous table position
                    cmp ($76),y	;   compare
                    bcs Lf951	;   branch if A >= previous value, swap [sorts by selection for those with same # of plays]
                    rts		; done here
Lf94b               iny		; move to previous table position
                    jsr Sf903	; swap table entries
                    inc $77	; (point to selections table)
Lf951               jsr Sf903	; swap table entries
                    dey		; move to next table position
                    dec $77	; (point back to plays table)
                    jmp Lf92b	; loopback
                    		;
				;****************************************
				; PERFORM TIME-CRITICAL BACKGROUND TASKS
				;****************************************
				; (preserves all registers & $2c variable?)
				; (called from 4 places)
				;
Sf95a               pha		; \
                    txa		;  \
                    pha		;   } push all registers
                    tya		;  /
                    pha		; /
                    lda $2c	; \ get $2c variable 
                    pha		; / push that variable to stack
				;
                    jsr Sf646	; [calls 4 other basic function subroutines: mech, svc switch, & coins]
				;
                    pla		; \ pull previously stored $2c variable
                    sta $2c	; / return to $2c
                    pla		; \
                    tay		;  \ pull X & Y back
                    pla		;  /
                    tax		; /
                    lda #$14	; \ reset counter $06 to 20 
                    sta $06	; / (used by MEMOREC sorting subroutine)
                    pla		; pull A back
                    rts		; 
                    		;
				;*************************************
				; SERVICE MODE CCC BUTTONS & SWITCHES
				;*************************************
				;  only used in service mode (called from 1 place)
				;  entry point is $f975
				;
Lf972               jmp Lfa32	; (branch extender to video mode code)
				;
				; entry point
				;
Sf975               lda $72	; \ get (service-mode-video???) flag
                    beq Lf972	; / branch to video mode(???) if 0
				;
				; records mode
				;
                    lda #$b1	; \ %1011 0001 sel=[101]=D5=S3: most/least switch
                    sta $4002	; / write to PIA2-B (select most/least switch input)
                    lda $43	; get (most/least switch status) [least=$00, most=$ff]
                    eor $4000	; xor bits with PIA2-A status, see if switch position has changed???
                    and #$02	; %0000 0010 isolate bit 1 : keypad/button input (selected most/least switch)
                    bne Lf972	; branch to video mode (???) if high
                    lda #$f0	; \ %1111 0000 sel=[110]=D6=S2: select video/record switch
                    sta $4002	; / write to PIA2-B
                    lda #$02	; \ %0000 0010 isolate bit 1: keypad button input (video/record switch)
                    and $4000	; / get PIA2-A inputs (ANDed with A's bitmask)
                    sta $06	; store video/record switch status in temp var (0 or 2)
                    lda $76	; \ is (LSB of pointer) = $ff?
                    cmp #$ff	; / (at "least" end of table???)
                    bne Lf99b	; if not, skip next opcode
                    lda #$00	;   instead of $ff, A=0
Lf99b               and #$80	; isolate bit 7 of A (now either $80 or $00???)
                    lsr a	; \
                    lsr a	;  \
                    lsr a	;   \ move bit 7 over to bit 1 (why???)
                    lsr a	;   / 
                    lsr a	;  /
                    lsr a	; /
                    eor $06	; xor bits based with temp var $06 (also bit 1, video/record switch state)
                    bne Lf972	; if not zero, branch to video mode (???)
				;
				; check CCC RESET button
				;
                    lda #$b0	; \ %1011 0000 - sel=[100]=D4=pin15 = S1 = reset button
                    sta $4002	; / write to PIA2-B (set input selector)
                    lda #$02	; \ %0000 0010 isolate bit 1: buttons/keypad (CCC RESET button)
                    and $4000	; / read CCC RESET button
                    beq Lf9cd	; branch down if RESET pressed
				;
				; check CCC ADVANCE button
				;
                    lda #$f1	; \ %1111 0001: sel=[111]=D7=pin12 = S4 = advance button
                    sta $4002	; / write to PIA2-B (set input selector)
                    lda #$02	; \ %0000 0010 isolate bit 1: buttons/keypad (CCC ADVANCE button)
                    and $4000	; / read CCC advance button
                    bne Lf9c6	; skip ahead if ADVANCE not pressed
				;
				; CCC ADVANCE button pressed
				;
                    lda $73	; get (debounce counter)
                    bne Lf9cc	; if (debounce counter) not 0 branch to rts
                    jmp Lfa86	;   jump down to handle advance button press
				;
				; CCC ADVANCE button not pressed 
				; [this doesn't make sense to me???]
				;
Lf9c6               lda $73	; get (debounce counter)
                    beq Lf9cc	; \ unless already 0
                    dec $73	; / decrement (debounce counter)
Lf9cc               rts		; done
				;
				; CCC RESET button pressed
				;
Lf9cd               lda #$02	; \ set MSB of pointer
                    sta $07	; / to $02xx
                    lda $0388	; get setting to (dis)allow resetting 5XX stats (0, 1 or 2)
                    bne Sf9df	; if not 0, branch down (reset MEMOREC tables, but NOT money/play counters)
				;
				; setting 0: zero out the money & play counters
				; 
Sf9d6               lda #$d8	;   \ $06/$07 -> $02d8 (money and play counters)
                    sta $06	;   /
                    ldy #$19	;   size of range = 25+1 bytes ($02d8-$02f1)
                    jsr Sfb19	;   zero out RAM range (money and play counters)
				;
				; (alternate entry point)
				; reset all MEMOREC tables
				;
Sf9df               lda #$00	; \
                    sta $06	;  \
                    lda #$04	;   } $06/$07 point to $0400
                    sta $07	;  / 
                    ldy #$c7	; /   target range: $0400 - $04c7 (000-199)
                    jsr Sfb19	; zero out RAM range
                    lda #$06	; \
                    sta $07	;  } target range: $0600 - $06c7 (000-199)
                    ldy #$c7	; / 
                    jsr Sfb19	; zero out RAM range
                    lda #$80	; \
                    sta $06	;  } target range: $0680 - $06e3
                    ldy #$63	; /
                    jsr Sfb19	; zero out RAM range
                    lda #$00	; \
                    sta $06	;  \ 
                    sta $72	;   } $05/$06 -> $0500
                    lda #$05	;  /  also set (service-mode-video) to 0
                    sta $07	; /
                    ldy #$c7	; target range $0500-$05c7 (0-199)
                    jsr Sfb23	; fill with sequential values
                    lda #$07	; \
                    sta $07	; / change MSB of pointer to $07
                    ldy #$3f	; target range $0700-$073f
                    jsr Sfb23	; fill range with sequential values
				;
				; initialize 2nd part of video MEMOREC table (4XX selections)
				;
                    lda $fc	; get (last 3XX video available)
                    clc		; \ add 1 to for (# of 3XX videos available)
                    adc #$01	; /
                    sta $06	; $06/$07 -> $07xx, where xx is (# of 3XX videos)
                    ldx #$7f	; init byte counter to 127
                    ldy #$3f	; init index counter 63
Lfa21               txa		; copy byte counter to A
                    sta ($06),y	;   put byte at pointer+Y
                    dex		;   decrement byte counter
                    dey		;   decrement index counter
                    bpl Lfa21	; loopback until Y<0 (do $40 bytes)
				;
				; init record #s (not selection #s) for MEMOREC
				;
                    lda #$80	; \
                    sta $06	; / LSB of pointer ($0780)
                    ldy #$63	; 100 bytes in size ($0780-$07e3)
                    jsr Sfb23	; fill RAM with sequential values
                    rts		; done
                    		;
				; video-mode CCC button/switch handling
				;
Lfa32               lda #$e3	; \
                    sta $76	;  \ $76/$77 -> $06e3 (end of records plays table)
                    lda #$06	;  / (default to "least" end of table)
                    sta $77	; /
                    lda #$00	; \ default most/least boolean to 0 (for "least")
                    sta $43	; /
                    lda #$ff	; \ set (service mode video???) flag
                    sta $72	; /
                    lda #$f0	; \  %1111 0000
                    sta $4002	;  \ write to PIA2-B: sel=[110]=D6=pin13=S4 = CCC ADVANCE button
                    lda #$02	;  / %0000 0010 isolate bit 1
                    and $4000	; /  read ADVANCE button
                    bne Lfa74	; branch ahead if ADVANCE not pressed
				;
				; ADVANCE button pressed (with video selected)
				;
                    clc		; clear carry flag for addition (at $fa58)
                    lda $fe	; get (# of 4XX videos available)
                    beq Lfa58	; branch down if 0
                    ldy $fc	;   get (# of 3XX videos available)
                    beq Lfa58	;   skip next opcode if 0
                    sec		;     set carry flag (add 1 for add'l zero-numbered selection)
Lfa58               adc $fc	; A = $fe var + $fc var + carry flag (grand total videos)
                    sta $6f	; \ store new A total in $6f var
                    sta $76	; / and pointer LSB
				;
                    lda #$b1	; \  %1011 0001
                    sta $4002	;  \ write to PIA2-B - sel=[101]=D5=S3 = most/least switch
                    lda #$02	;  / %0000 0010 isolate bit 1
                    and $4000	; /  read most/least switch position via PIA2-A
                    beq Lfa86	; if LEAST (or MOST, not sure which) branch down
                    lda #$00	;   \ set pointer LSB to $00
                    sta $76	;   / (move to "most" end of table)
Lfa6e               lda #$ff	;   \ set most/least boolean to $ff (for "most")
                    sta $43	;   /
                    bne Lfa86	;   (always) branch down
				;
				; ADVANCE not pressed(???)
				;
Lfa74               lda #$b1	; \  %1011 0001
                    sta $4002	;  \ write to PIA2-B - sel=[101]=D5=S3 = most/least switch
                    lda #$02	;  / %0000 0010 isolate bit 1
                    and $4000	; /  read most/least switch position via PIA2-A
                    beq Lfa86	; if MOST (???) branch down
                    lda #$80	;   \ LSB of pointer to $80
                    sta $76	;   / (least end of table???)
                    bne Lfa6e	;   (always) branch up
				;
				; common path
				; get plays count and display it on CCC LEDs
				;
Lfa86               ldy #$00	; offset = 0
                    lda ($76),y	; get value at $76/$77 pointer (plays)
                    cmp #$63	; \  compare it to 99
                    bcc Lfa90	;  } limit value for 2-digit display
                    lda #$63	; /  (display max of 99)
Lfa90               sta $24	; copy count to be converted to decimal digits
                    lda #$00	; \ set (CCC LED decimal point flag) to 0
                    sta $86	; /
                    lda #$43	; \ set offset for output
                    sta $26	; / to be on CCC LEDs
                    jsr See45	; convert binary number to decimal digits
				;
                    inc $77	; increment MSB of pointer (from page 6 to page 7, video data)
                    lda ($76),y	; get value at $76/$77 pointer (video selection number)
                    sta $24	; copy selection # to be converted to decimal digits
                    cmp #$40	; compare it to (64) maximum number of 3xx selections
                    bcc Lfab8	; branch if A < 64
                    lda $76	;   get LSB of pointer
                    cmp #$80	;   compare to $80
                    bcs Lfab8	;   branch if A >= $80 (invalid video data???)
                    lda $24	;   get (selection number)
                    sec		;   \
                    sbc #$40	;   / subtract 64 from A (convert selection number to MEMOREC table)
                    sta $24	;   store corrected selection number for conversion to digits
                    lda #$ff	;   \
                    sta $86	;   / set (CCC LED decimal point flag) high
				;
Lfab8               dec $77	; decrement MSB of pointer (back to page 6, records data)
                    lda #$41	; \ 
                    sta $26	; / direct output to $5c-$5f: CCC LED digits
                    jsr See45	; convert 2-byte number to 4 digits
				;
				; supress leading 0s on CCC readouts
				;
                    lda $5e	; get left-most LED digits
                    bne Lfac9	; if non-zero digit, skip down
                    lda #$0e	; \ [if digit 0] charcode for a blank digit
                    sta $5e	; / make digit blank (supress leading 0)
Lfac9               lda $60	; get 3rd LED digit (leading digit of 2nd pair)
                    bne Lfad1	; skip ahead if not 0
                    lda #$0e	; \ if digit 0, charcode for a blank digit
                    sta $60	; / make digit blank (supress leading 0)
				;
Lfad1               inc $76	; increment LSB of pointer
                    lda #$ff	; \ 
                    sta $71	; / init (unused variable???) to $ff
                    lda #$03	; \ 
                    sta $73	; / init (ADVANCE button debounce???) to 3
                    lda $43	; get (most/least switch flag)
                    bne Lfaee	; branch forward if set to "MOST"
				;
				; switch on "LEAST" 
				;
                    dec $76	; \ decrement LSB of $76/$77 pointer twice
                    dec $76	; / (once to undo the increment, another to actually decrement)
                    lda $76	; get the twice-decremented $76 var
                    cmp #$fe	; \ 
                    beq Lfaf4	; / if LSB wrapped around, branch
                    cmp #$7e	; \
                    beq Lfaf4	; / if LSB wrapped around, branch
                    rts		; done
				;
				; switch on "MOST"
				;
Lfaee               lda $76	; get LSB of pointer (already incremented)
                    cmp #$e5	; compare (end of table)
                    bcc Lfaf9	; branch ahead if (still in table)
Lfaf4               lda #$00	;   \ init (service mode video???) to 0
                    sta $72	;   /
                    rts		;   done
				;
Lfaf9               lda #$f0	; \  %1111 0000
                    sta $4002	;  \ write to PIA2-B: sel=[110]=D6=S4= ADVANCE button
                    lda #$02	;  / %0000 0010
                    and $4000	; /  read CCC ADVANCE button via PIA2-A
                    beq Lfb06	; skip next opcode if pressed
                    rts		;   not pressed, rts
				;
Lfb06               lda $fe	; get (# of 4XX videos available)
                    clc		; clear carry flag for addition at $fb10
                    beq Lfb10	; branch down if no 4XX videos exist
                    ldy $fc	;   get (# of 3XX videos available) to Y
                    beq Lfb10	;   branch down if no 3XX videos exist
                    sec		;     set carry flag (extra count for add'l 0-numbered selection)
Lfb10               adc $fc	; A=$fe+$fc + carry flag (grand total of videos)
                    adc #$01	; A=A+1 (why???)
                    cmp $76	; compare to LSB of pointer
                    bcc Lfaf4	; loopback if A < $76 var
                    rts		; done
                    		;
				;********************
				; FILL RANGE WITH 0s
				;********************
				; expects $06/$07 as pointer & Y offset from pointer
				; zeros out address range
				;
Sfb19               lda #$00	; fill with 0s
Lfb1b               sta ($06),y	; zero out Yth address, offset from $06/$07 pointer
                    dey		; next byte
                    bne Lfb1b	; loopback
                    sta ($06),y	; one last time with Y=0
                    rts		; 
				;
				;************************************
				; FILL RANGE WITH SEQUENTIAL NUMBERS
				;************************************
				;   expects $06/$07 as pointer to RAM
				;   and Y as number of bytes to fill
				;                    
Sfb23               tya		; duplicate Y in A
                    sta ($06),y	; put A (calling Y) in RAM at $06/$07 pointer + Y
                    dey		; next byte
                    bne Sfb23	; loopback til Y=0
                    tya		; make A=0, too
                    sta ($06),y	; one last time with Y (and A) = 0 
                    rts		; done
                    		;
				;**************************
				; RESTORE FACTORY SETTINGS
				;**************************
				;   (does *not* update the RAM checksum)
				;
Sfb2d               lda #$00	; \
                    sta $06	;  \ $06/$07 point to $0300
                    lda #$03	;  /
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
                    rts		; 
				;
				;****************
				; PIA base value?
				;****************
				;  set A & $0a var based on $78 var (serial to video system flag???)
				;  the two values differ only at PA3, data bit to video system high/low
				;  in both cases, PA2 is LOW (data bit to wallbox)
				;  literal code understood; purpose of subroutine not understood???
				;
Sfb4a               lda #$7b	; %0111 1011, value to use if message ready for video system(?)
                    ldx $78	; \ check (video transmit???) flag
                    bne Lfb52	; / if non-zero skip next opcode
                    lda #$73	;   %0111 0011, value to use if no message for video system(?)
Lfb52               sta $0a	; set $0a var (value also returns in A)
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
                    rts		; done
                    		;
				;********************
				; DISPLAY ERROR CODE
				;********************
				;  on the CCC LEDs
				;
Sfb65               ldx #$0c	; \ charcode for "E"
                    stx $5e	; / put it on 1st CCC LED digit
                    inx		; X -> $0d, charcode for "r"
                    stx $5f	; \ put "r" on the
                    stx $60	; / other two CCC LED digits
                    lda $6b	; get error code #
                    sta $61	; put on last CCC LED digit (only supports single-digit error codes!)
                    cmp #$05	; compare error code to 5
                    bcs Lfb79	; branch to RTS if error >= 5 (don't immediately display errors 5 & 6???)
                    jsr Se453	;   update LED displays
Lfb79               rts		;
                    		;
				;***********************
				; WALLBOX COMMUNICATION
				;***********************
				;  called from 2 places: e91c & ea36 (both in video manager) 
				;
Lfb7a               jsr Sfb4a	; \ set A & $0a var based on $78 boolean (a PIA "base" value???)
                    sta $4000	; / write to PIA2-A (pulls wallbox data line low???)
                    lda $4000	; \  read PIA2-A
                    ora $4000	;  } (again???)
                    and #$10	; /  %0001 0000 isolate wallbox serial in
                    bne Lfb99	; branch down if bit high (signal is hardware-inverted)
				;
				; "wallbox signal is stuck high" error
				;
                    jsr Sef4d	; 1ms delay
                    ldy #$06	; set error code 6: wallbox stuck high
Lfb8f               sty $6b	; error-code storage variable
                    lda $35	; get service/normal mode boolean
                    bne Lfb98	; skip next opcode (and rts) in service mode
                    jsr Sfb65	;   display error code 6 on LEDs???
Lfb98               rts		; done
				;
				; wallbox line responds to being pulled low
				;
Lfb99               ldx #$1c	; init delay counter (28) \
Lfb9b               dex		;   decrement counter	   } 147 cycle pause (~0.16ms)
                    bpl Lfb9b	; loopback for delay      /  
				;
				; 3 ways to branch off
				;
                    lda $9d	; \ check (wallbox rec'v timer???)
                    bne Lfba5	; / skip next opcode if non-zero
                    jmp Lfc29	;   jump down a ways
Lfba5               lda $9f	; \ check (wallbox comm state???) flag
                    bne Lfc11	; / branch if non-zero
                    lda $bf	; \ check (wallbox INPUT buffer index/flag???)
                    bpl Lfc11	; / branch if bit 7 set (i.e. flag set)
				;
				; (fall thru)
				; transmit a byte from buffer to wallbox
				;
                    ldx #$1c	; set initial delay value
                    lda $b0	; get (index into output buffer)
                    bmi Lfc11	; branch way down if bit 7 set (nothing to send)
                    beq Lfbb7	; skip next opcode if index is 0
                    ldx #$0a	;   set different (smaller) initial delay value
Lfbb7               jsr Sfca4	; transmit a byte from buffer to wallboxes (2400 baud)
                    inc $b0	; increment index to next byte in buffer
                    lda $b2	; \ check length of message in buffer
                    cmp $b0	; / compare to current index value
                    bne Lfbe5	; if not done with msg yet, branch to finish up(???)
				;
				; just did last byte, message was fully transmitted
				;
                    lda $b1	; command code of message just sent
                    cmp #$02	; was it a command $02? (ACK???)
                    beq Lfbdf	; if so, branch to clear flags and finish up (skip event logging)
                    cmp #$62	; was it a command $62? (selection # playing)
                    bne Lfbd0	; if not, skip resetting a variable
                    ldx #$00	;   \ reset (selection notification type) to 0
                    stx $9e	;   / if command $62 was sent
				;
				; increment "wallbox queue/log" read index, wrapping if needed
				; (skipped for command code $02)
				; (does not remove bytes from table, they apparently just get overwritten)
				;
Lfbd0               ldx $0170	; read offset into "wallbox queue/log"
                    stx $c8	; put it into (end of wallbox recieve buffer???)
                    inx		; increment table read offset to next slot
                    cpx #$90	; end of table reached?
                    bcc Lfbdc	; if not, skip an opcode
                    ldx #$72	;   wrap back to table start
Lfbdc               stx $0170	; put updated table offset back
				;
				; clean up
				;
Lfbdf               lda #$80	; \  clear flags
                    sta $9f	;  } (wallbox comm state flag???)
                    sta $b0	; /  (wallbox output buffer flag???)
				;
				; finish up a byte
				;
Lfbe5               lda $0a	; \  get PIA base value
                    ora #$04	;  } %0000 0100 set bit 2
                    sta $4000	; /  write to PIA2-A: wallbox serial out (leave high/quienscent)
Lfbec               lda $4000	; \ readback PIA2-A status
                    and $4000	; / (again???)
                    ldy #$05	; Y=5 (error code 5: wallbox stuck low)
                    and #$10	; \ %0001 0000 isolate bit 4: wallbox serial in
                    bne Lfb8f	; / branch BACK to give error if not 0 (signal is hardware-inverted)
				;
                    lda $6b	; get error code
                    cmp #$07	; compare to 7 (not sure how it would ever be 7???)
                    bcs Lfc10	; branch to rts if error code >= 7 (none exist???)
                    cmp #$05	; compare to 5
                    bcc Lfc10	; branch to rts if error code < 5 (including 0, no error)
				;
				; error codes 5 & 6 (wallbox line stuck)
				;
                    lda $35	; check service/normal mode boolean
                    bne Lfc10	; branch to rts in service mode
                    lda #$0e	;   \   (LED code for blank digit)
                    sta $5e	;    \  
                    sta $5f	;     } clear all 4 digits of the CCC LED display
                    sta $60	;    / 
                    sta $61	;   /
				;
Lfc10               rts		; done
				;
				; [branched to from several places above]
				; (not transmitting a wallbox message currently???)
				; (listen to line for a wallbox talking???)
				;
Lfc11               ldx #$37	; init X counter to (55) times to check wallbox line
				;
Lfc13               dex		; decrement X counter
                    bne Lfc3e	; if X not expired yet, branch down to check for a start bit???
				;
				; X counter expired, no activity on wallbox line???
				;
                    lda #$80	; \
                    cmp $bf	;  } check if input buffer flag/index is $80 (inactive???)
                    beq Lfc21	; /  if so, branch down
				;
				; input buffer flag/index is not $80 (receiving a msg???)
				; 
                    inx		;   X=1 (X expired to 0 above)
                    stx $9d	;   \ set (wallbox rec'v counter/timer???) to 1
                    sta $bf	;   / set (wallbox input buffer flag/index) to 1
				;
Lfc21               lda #$00	; \
                    sta $9f	; / set (wallbox comm state flag???) to 0
                    dec $9d	; \ decrement (wallbox rec'v counter/timer)
                    bpl Lfc2d	; / branch if bit 7 clear 
				;
				; $9f was zero??? [or other ways to get here]
				;
Lfc29               lda #$19	; \ 
                    sta $9d	; / set (wallbox rec'v counter/timer???) to 25
				;
Lfc2d               lda $0a	; \  get previously stored PIA base state
                    ora #$04	;  } %0000 0100 set bit 2: wallbox serial out
                    sta $4000	; /  write to PIA2-A
                    lda $b0	; check serial output buffer/flag
                    bpl Lfbec	; if bit 7 clear, branch back to finish up
                    jsr Sfd15	;   prepare messages to send to wallbox
                    jmp Lfbec	;   jmp to finish up
				;
				; check for start-bit
				;
Lfc3e               lda $4000	; \  get PIA2-A status
                    ora $4000	;  } (again)
                    and #$10	; /  %0001 0000 isolate bit 4: wallbox serial in
                    bne Lfc13	; if bit high, branch up to loop(???)
				;
				; start bit transition recieved, wait ~1.5 bit-periods
				; (A=0 to get here)
				; 
                    ldy #$67	; delay loop counter (103)
                    ldx $bf	; \ check (wallbox input buffer flag/index)
                    bpl Lfc51	; / if bit 7 low, skip to delay loop
                    sta $bf	;   \ reset (wallbox input buffer flag/index) to 0
                    tax		;   / reset X to 0 also
				;
Lfc51               dey		; 2 cycles \ delay loop 
                    bpl Lfc51	; 3 cycles / 103*5=515+(2+3+3) = 523 cycles (seems a tad low???)
				;
				; recieve a byte via serial from wallbox
				; (895000cycles/s)/(365cycles/bit) = ~2400 baud
				;
                    ldy #$08	; init counter for 8 bits
Lfc56               lda $4000	; \  get PIA2-A status						4\
                    ora $4000	;  } again???							4 |
                    and #$10	; /  %0001 0000 isolate bit 4: wallbox serial in		2 |
                    adc #$f0	; add %1111 0000 (sets/clears carry based on serial bit)	2 } 23
                    ror $c0,x	; rotate carry flag into Xth byte in wallbox recieve buffer	6 |
                    lda #$2a	; \ init delay loop counter					2 |
                    sta $08	; /								3/
Lfc66               dec $08	;   \ delay loop						5*42=210
                    bpl Lfc66	;   /								3*42-1=125
                    nop		; (fine-tune delay by 2 cycles)					2\
                    dey		; decrement bit counter						2 } 7
                    bne Lfc56	; loopback for another bit until counter reaches 0		3/
				;
                    inc $bf	; increment (wallbox input buffer index)
                    lda $bf	; copy index to A
                    cmp #$09	; are we at byte 9 in buffer?
                    bcc Lfc78	; branch if < 9 bytes 
                    dec $bf	;   decrement (input buffer index to prevent a buffer overrun)
Lfc78               cmp #$03	; compare index to 3
                    bcc Lfca1	;   if < 3, branch down to loopback (ALL messages are at least 3 bytes long)
                    cmp $c1	; compare index to (message length) byte in buffer
                    bne Lfca1	;   if full message not rec'd yet, branch down to loopback
				;
				; a complete message has been received!
				;
                    lda #$00	; \ reset (wallbox receive counter/timer???) to 0
                    sta $9d	; /
				;
				; verify checksum of message received from wallbox
				;
Lfc84               clc		; clear carry for addition
                    adc $c0,x	;   keep running sum of all bytes in message
                    dex		;   next byte
                    bpl Lfc84	; loopback until counter wrap
                    tax		; move checksum to X
                    bne Lfc9d	; branch if checksum is not 0 (a checksum error)
				;
				; checksum is good :)
				;
                    jsr Sfe32	; take action based on recieved mesage from wallbox
                    lda $c0	; get command code recieved from wallbox
                    cmp #$f0	; compare to $f0 (service-type commands)
                    bcs Lfc9d	; branch down if command was >=$f0 (i.e. $fX)
                    lda #$02	;   \ set $02 command (ACK) to wallbox
                    sta $b1	;   / for all commands <$f0 rec'd
                    jsr Lfcde	; finalize a 0-parameter command to wallbox
				; 
				; checksum bad :(
				;
Lfc9d               lda #$80	; \ store #$80 in (serial input buffer flag/index)
                    sta $bf	; / (is that normal, or an error return???)
Lfca1               jmp Lfbe5	; loopback to finish up
                    		;
				;*****************************
				; SEND BYTE OVER WALLBOX LINE
				;*****************************
				;  Two entry points:
				;    $fca4 (from $fbb7) for actual wallbox use
				;    $fcad (from near $e04d) for "InterROWEgator" use
				;  X: sets length of and initial delay
				;  X: used afterward as index into serial buffer
				;  $b1,X is the byte to send
				;  $0a: initial value of PIA2-B register
				;  $0c: loop constant to control baud rate
				;
Sfca4               dex		; \ brief delay based on calling X vlaue
                    bpl Sfca4	; / 
                    ldy #$44	; \ set baud rate timer to #$44 (2400 baud for wallboxes)
                    sty $0c	; /
                    ldx $b0	; fetch $b0 to X (index of byte to transmit)
				;
				;  (this alternate entry only called by "InterROWEgator" code)
				;  ("InterROWEgator" calls with X=0 and byte in $b1)
				;  ("InterROWEgator" may use different values of $0c)
				;
Sfcad               lda #$0a	; \ init counter to 10 (total # of bits, including start & stop bits)
                    sta $0b	; /
                    lda $0a	; get initial value of PIA2-B
                    sec		; set carry flag (for the start bit)
                    bcs Lfcba	; branch into middle of loop to send a start bit
				;
				; top of 10-bit serial transmit loop ("bit banging")
				;
Lfcb6               lda $0a	; fetch initial PIA2-B register value
                    bcs Lfcbc	; skip next opcode if carry flag set ("space")
Lfcba               ora #$04	; if carry is clear, raise bit 3 of PIA2-B 
Lfcbc               sta $4000	; output bit to "wallbox" serial (InterROWEgator connected to wallbox plug) 
                    bcs Lfcc1	; branch to very next instruction if carry set (loop timing adjustment???)
Lfcc1               nop		; delay
                    ldy $0c	; get delay loop (baud rate) parameter
                    nop		;   delay		; 2 cycles \
Lfcc5               dey		;   countdown		; 2 cycles  } 7 cycles per loop
                    bne Lfcc5	;   loopback until Y=0	; 3 cycles /
                    ror $b1,x	; rotate least signficant bit into the carry flag
                    dec $0b	; decrement bit counter
                    bne Lfcb6	; loopback until counter is 0 (start bit + 8 data bits + stop bit)
                    rol $b1,x	; rotate byte left (return byte to original?, why???)
                    rts		; 
				;
				;************************************
				; COPY BYTES TO SERIAL OUTPUT BUFFER
				;************************************
				;   copy Y+1 bytes from $06/$07 pointer
				;   to serial buffer, starting at $00b3
				;
Sfcd1               lda ($06),y	; get A from $06/$07 pointer, offset by Y
                    sta $00b3,y	; put A in table  $00b3 offset by Y
                    dey		; decrement counter
                    bpl Sfcd1	; loopback until Y wraps past 0
                    rts		;
                    		;
				;****************************************
				; SERIAL OUTPUT BUFFER LENGTH & CHECKSUM
				;****************************************
				;  A: number of parameter bytes required
				;  $b0: serial output flag
				;  $b2: total length of serial message
				;      (command code, length, [parameter(s)], checksum) 
				;
				; (alternate entry point, 2 parameters)
				;
Lfcda		    lda #$02	; A=2           \ 2 parameters
		    bne Lfce4	; always branch / this entry never used in code(???)
				;
				; (primary entry point, 0 parameters)
				;
Lfcde               lda #$00	; A=0           \ 0 parameters
                    beq Lfce4	; always branch / (1 jsr from wallbox code, 1 jmp)
				;
				; (alternate entry point, 1 parameter)
				;
Lfce2               lda #$01	; A=1 parameter
				;
				; (this point also reached by jmp from other code that
				;  uses more than 2 parameters)
				;
Lfce4               clc		; clear carry for addition
                    adc #$03	; add 3 overhead bytes to # of params (command code, length byte, checksum)
                    sta $b2	; store total length of serial message as 2nd byte in buffer
                    tax		; \ copy message length to both other registers
                    tay		; / 
                    dex		; \ subtract 2 from X: offset to the last parameter
                    dex		; / 
                    dey		; subtract 1 from Y: offset to checksum byte (last byte in message)
                    lda #$00	; \
                    sta $b0	; / reset (serial buffer output index/flag???)
				;
				; add up sum of bytes in message
				;
Lfcf2               clc		; clear carry for addition
                    adc $b1,x	;   keep running sum of bytes in buffer
                    dex		;   decrement X counter
                    bpl Lfcf2	; loopback until X wraps under 0
				;
				; calculate correct checksum
				;
                    eor #$ff	; flip every bit of A \
                    tax		; A->X		       } calculates 2's complement of A to X
                    inx		; increment X	      /  this ensures the sum of all msg bytes = 0
				;
                    stx $b1,y	; store checksum byte at end of message in buffer
                    rts		;
				;
				;*****************************************
				; put command into wallbox comm log/queue
				;*****************************************
				;   subroutine takes A (command code) as input
				;   A values used by calling locations include: 
				;     $01, $08, $40, $60, $61, $62, $7c, $e0 
				;
Sfcff               ldx $0171	; get (table write index offset) to X 
                    sta $0100,x	; store (command code) into log/queue
                    inx		; increment offset
                    cpx #$90	; compare to (end of table)
                    bcc Lfd0c	; branch if not past end of table
                    ldx #$72	;   wrap back to start of table if needed
Lfd0c               cpx $0170	; compare (table write pointer) to (table read index)
                    beq Lfd14	; if equal branch to rts(???)
                    stx $0171	;   otherwise store updated table write index
Lfd14               rts		; done
				;
				;*****************************
				; PREPARE MESSAGE FOR WALLBOX 
				;*****************************
				;
Sfd15               ldx $0170	; get (wallbox log/queue read index)
                    cpx $0171	; compare to (write index)
                    beq Lfd29	; if they're equal, skip down (nothing is new)
				;
				; $0170 != $0171: message(s) queued for wallbox
				;
                    lda $0100,x	; retrieve new code in queue/log
                    sta $b1	; copy command code into output buffer
                    cmp #$40	; compare command code #$40
                    bcs Lfd44	; branch down if command code >= $40 (these need parameters)
                    jmp Lfcde	; jmp to send the command codes with no parameters
				;
				; $0170 == $0171: no new message waiting
				;
Lfd29               ldx $5a	; get (pending event counter for wallbox???) [only inc'd near f6be]
                    beq Lfd3a	; branch down if 0
                    lda #$04	;   command code $04
                    dec $5a	;   decrement (pending even counter for wallbox???)
                    beq Lfd37	;   if counter 0 skip down
                    dec $5a	;     decrement counter again
                    lda #$08	;     command code $08
Lfd37               jsr Sfcff	;   put $04 or $08 code into wallbox log/queue
				;
Lfd3a               lda $9e	; \ check 2nd parameter for wallbox command $62 (selection notification type)
                    beq Lfd43	; / if 0, branch to rts
                    lda #$62	; \ 
                    jsr Sfcff	; / put #$62 into wallbox log/queue (selection notifications)
Lfd43               rts		;
				;
				; command code >= $40, needs parameter(s)
				;
Lfd44               cmp #$60	; compare command code to $60
                    bcs Lfd4f	; branch down if command code >= $60
				;
				; wallbox command codes: $40 <= command code < $60
				; (reached by jmp and fallthru)
				;
                    lda $9b	; \ get (jukebox busy Boolean???)
Lfd4a               sta $b3	; / put A at into output buffer (as 1st message parameter)
                    jmp Lfce2	; calculate checksum & flag message for wallbox
				;
				; wallbox command $60: 7 most popular selections
				;
Lfd4f               cmp #$60	; A = $60?
                    bne Lfd6b	; if not, branch to next check...
				;
                    lda #$00	; \ set LSB of pointer to $00
                    sta $06	; /
                    ldy #$05	; MSB of pointer $05 (default for records)
                    lda $39	; \ check video mode boolean
                    beq Lfd5f	; / if in records mode, skip next opcode
                    ldy #$07	;     MSB of pointer $07 for video
Lfd5f               sty $07	; $06/$07 -> $0500 or $0700 (per records/video mode)
                    ldy #$06	; \ initial offset for subroutine
                    jsr Sfcd1	; / copy top 7 selections from selected table into serial output buffer
                    lda #$07	; 7 parameters for message to wallbox
                    jmp Lfce4	; calculate checksum & flag message for wallbox
				;
				; to wallbox command $61: selection list positions
				;
Lfd6b               cmp #$61	; A = $61?
                    bne Lfd7c	; if not, branch to next check...
				;
                    lda $e5	; \ get (position in record selection table)
                    sta $b3	; / copy it to output buffer (1st param to wallbox)
                    lda $ee	; \ get (position in FIFO selection list)
Lfd75               sta $b4	; / copy it to output buffer (2nd param to wallbox)
Lfd77               lda #$02	; indicate a 2-parameter message
                    jmp Lfce4	; calculate checksum & flag message for wallbox
				;
				; to wallbox command $7c: record price settings
				;
Lfd7c               cmp #$7c	; A = $7c?
                    bne Lfd8c	; if not, branch to next check...
				;
                    ldx #$00	; LSB of pointer of 0 ($0300)
Lfd82               ldy #$09	; copy $0300-$0309 (record price settings)
                    jsr Sfde3	; copy 10 bytes from page 3 to serial wallbox output buffer
Lfd87               lda #$0a	; indicate a 10-parameter message
                    jmp Lfce4	; calculate checksum & flag message for wallbox
				;
				; to wallbox command codes $7d: video price settings
				;
Lfd8c               ldx #$0a	; X=$0a (LSB -> $030a video pricing)
                    cmp #$7d	; A = $7d?
                    beq Lfd82	; if so, branch back to use end of command $7c's code
				;
                    cmp #$7e	; A = $7e?
                    bne Lfdce	; if not, branch down to next check...
				;
				; to wallbox command code $7e: other settings
				;	pos	addr	content
				;	1	b3	freeplay setting
				;	2	b4	disable XX8/XX9 setting
				;	3	b5	records/video mode setting
				;	4	b6	WRF coin ratio setting
				;	5	b7	3XX videos
				;	6	b8	4XX videos
				;	7	b9	video lockout 1
				;	8	ba	video lockout 2
				;	9	bb	video lockout 3
				;	10	bc	video lockout 4
				;
                    ldy #$03	; offset to last byte to copy
                    ldx #$1c	; LSB of pointer: $031c-$031f (4 settings)
                    jsr Sfde3	; copy Y+1 bytes from page 3 into output buffer
                    lda $031b	; get freeplay setting (255 or 0)
                    sta $b3	; copy it into output buffer (overwrite value just copied there)
                    lda $fc	; \ get (# of 3XX videos available)
                    sta $b7	; / copy into output buffer
                    lda $fe	; \ get (# of 4XX videos available)
                    sta $b8	; / copy into output buffer
                    lda #$09	; \ init temp var $06 to 9
                    sta $06	; /
                    ldx #$07	; init X loop counter
Lfdb0               ldy #$ff	; init Y at $ff
                    lda $0366,x	;   get MSB value from video lockout table
                    beq Lfdc0	;   if empty, skip ahead
                    dex		;   move to LSB
                    lda $0366,x	;   get value from video lockout table
                    sec		;   (set carry flag for subtraction)
                    sbc #$2c	;   subtract 44 (why???) from value
                    inx		;   back to MSB
                    tay		;   copy A to Y
Lfdc0               tya		;   copy Y to A ($ff if slot empty, otherwise a value)
                    ldy $06	;   get $06 temp var??? (ref. near $fdb0, starts at 9)
                    sta $00b3,y	;   put value into output buffer: $00b3-$00bc???
                    dec $06	;   decrement $06 temp var
                    dex		;   \ move X to next lockout entry
                    dex		;   / 
                    bpl Lfdb0	; if X doesn't roll under, loopback for more video lockout entries
                    bmi Lfd87	; otherwise, branch to finish a 10-parameter message
				;
				; to wallbox command $7f: (settings)
				;	pos	addr	setting
				;	1	b3	MEMLOC 37, WRE wallbox data?
				;	2	b4	MEMLOC 38, WRE wallbox data?
				;	3	b5	MEMLOC 39, WRE wallbox data?
				;	4	b6	MEMLOC 40, WRE wallbox data?
				;	5	b7	MEMLOC 41, WRE wallbox data?
				;	6	b8	MEMLOC 50, WRE wallbox data? 
				;	7	b9	MEMLOC 51, WRE wallbox data?
				;	8	ba	MEMLOC 52, WRE wallbox data?
				;	9	bb	MEMLOC 53, WRE wallbox data?
				;	10	bc	MEMLOC 54, disable THANK YOU lamp?
				;
Lfdce               cmp #$7f	; A = $7f?
                    bne Lfded	; if not, branch (across another subroutine) to more checks...
				;
                    ldy #$09	; Y=9 (offset of last byte to copy)
                    ldx #$2d	; LSB of pointer to $032d (alternate credit cost flags???)
                    jsr Sfde3	; copy 10 bytes from settings to serial output buffer
                    ldy #$04	; Y=4 (offset of last byte to copy)
                    ldx #$25	; LSB of pointer; $0325 (WRE wallbox data???)
                    jsr Sfde3	; copy 5 bytes from settings to serial output buffer (partial overwrite)
                    jmp Lfd87	; jump back to finish a 10-parameter message
				;
				;*******************************************
				; COPY FROM PAGE 3 TO WALLBOX OUTPUT BUFFER
				;*******************************************
				;   X: LSB of source pointer (start point)
				;   Y+1: number of bytes to copy (Y=3, 4 & 9 are used)
				;   (source pointer MSB fixed to $03: RAM page 3)
				;   dest: fixed to $00b3 to $00b3+Y (serial output buffer to wallbox)
				;
Sfde3               stx $06	; set LSB of pointer from X
                    lda #$03	; \
                    sta $07	; / pointer MSB (programmable settings page)
                    jsr Sfcd1	; RAM copy Y bytes from $06/$07 pointer to $00b3-
                    rts		; done
                    		;
				;**********************************************
				; (continuation of PREPARE MESSAGE FOR WALLBOX 
				;**********************************************
				; takes action based on contents of A
				; this section handles commands $62, $e0, $e1, $e2
				;
				; to wallbox command code $62: (selection notification)
				;   this includes selection starting & ending,
				;   for records, videos, and fill-in (records)
				;
Lfded               cmp #$62	; A = $62? 
                    bne Lfe0f	; if not, branch down to next check...
                    lda $9e	; get 2nd parameter (selection notification type code) 
                    ldx $c9	; \ get selection # involved
                    stx $b3	; / copy selection # to output buffer (1st parameter to wallbox)
                    sta $b4	; put (notification type) into output buffer (2nd parameter)
                    cpx $ca	; compare selection # to (wallbox-related selection variable???)
                    bne Lfe0c	; if not equal, skip a block 
				;
                    ora #$f0	; %1111 0000 set high 4 bits of A (changes 1st hex digit of type code to $f)
                    ora $032a	; OR w/mem loc 42 (disable early cancel of fill-in???) Boolean
				;   A was a code:    $db, $dd, $e8, $eb, $ed, $ee
				;   after ora #$f0:  $fb, $fd, $f8, $fb, $fd, $fe
				;   after ora $034a: $fb, $fd, $f8, $fb, $fd, $fe, $ff
                    sta $b4	; store result into output buffer (2nd parameter to send to wallbox)
                    cmp #$fd	; \
                    bcc Lfe0c	; / branch unless code $fe or $ff 
                    ldx #$ff	;   \ set (wallbox-selection-related???) Boolean flag
                    stx $ca	;   /
				;
				; $c9 != $ca
				;
Lfe0c               jmp Lfd77	; jump to finish a 2-parameter message
				;
				; command code to wallbox: $e0 = "money deposited"
				;
Lfe0f               cmp #$e0	; A = $e0?
                    bne Lfe18	; if not, branch to next check...
                    lda $a0	; get (money just deposited)
                    jmp Lfd4a	; jump to send single-parameter message (parameter in A)
				;
				; command code to wallbox: $e1 = "PIA1 port status"
				;
Lfe18               cmp #$e1	; A = $e1?
                    bne Lfe26	; if not, branch to next check...
                    lda $68	; \ get PIA1-A status
                    sta $b3	; / copy into serial output buffer [to send to wallbox]
                    lda $2002	; \ get PIA1-B output status
                    jmp Lfd75	; / jump to put A in output buffer as 2nd parameter & finish 2-parameter message
				;
				; command code to wallbox: $e2 = "PEEK value at address"
				;
Lfe26               cmp #$e2	; A = $e2?
                    bne Lfe31	; if not, branch to rts (there are no more valid command codes)
                    ldy #$00	; \
                    lda ($c2),y	; / get A from $c2/$c3 pointer (0 offset)
                    jmp Lfd4a	; jump to send A as a single parameter message
				;
Lfe31               rts		; done
                    		;
				;*****************************
				; RESPOND TO WALLBOX COMMANDS
				;*****************************
				;  expect data in wallbox input buffer
				;	code/command at 		$c0
				;	length of message at		$c1
				;	may have add'l parameters	$c2-$c_
				;	last byte is checksum to ensure all bytes sum to $00
				;
				;  only called from 1 place; near $fc9d
				;
Sfe32               lda $c0	; get command code from wallbox input buffer
                    cmp #$10	; check for command code $10
                    bne Lfe3e	; if not, skip ahead to next check...
				;
				; command code from wallbox: $10 = "set index pointer???"
				; (appears to require 7 parameter bytes, and 1-6 are ignored???)
				; 
                    lda $c8	; get (parameter byte 7 from wallbox input buffer)
                    sta $0170	; update $0170 wallbox log/queue table read pointer
                    rts		; done
				;
Lfe3e               ldx #$ec	; point to "records w/popular" counter
                    ldy #$00	; Boolean flag default to "records" mode
                    cmp #$50	; check for command code $50
                    beq Lfe4e	; if SO, branch ahead to increment RECORDS (vice videos) counters
                    cmp #$52	; check for command code $52
                    bne Lfe59	; if NOT, skip ahead to next check...
				; if SO, fallthru...
				;
				; command code from wallbox: $52 = "play a POPULAR video selection"
				;
                    ldx #$e6	; point to "total videos w/POPULAR" counter
                    ldy #$ff	; set Boolean flag for "video" mode
				; fallthru into common code for records mode...
				;
				; command code from wallbox: $50 = "play a POPULAR record selection"
				; (command codes $50/$52 join here)
				;
Lfe4e               lda $3b	; get current record/video mode state variable
                    pha		; push it to the stack
                    sty $3b	; set to records/video mode per Y flag
                    jsr Sf18d	; increment the appropriate counter
                    jmp Lfe6f   ; finish in common path below
				;
				; command code from wallbox: $54 = "play a record selection (regular)"
				;
Lfe59               cmp #$54	; check for command code $54
                    beq Lfe63	; if so, jump part way into path below
				;
                    cmp #$58	; check for command code $58
                    bne Lfe83	; if not, skip ahead to next check...
				;
				; command code from wallbox: $58 = "play a video selection (regular)"
				;
                    ldy #$ff	; Boolean flag high for video mode
				;
				; (commands $54 & $58 merge here)
				;
Lfe63               lda $3b	; get (records/video mode) flag
                    pha		; push it to the stack
                    sty $3b	; set (records/video mode) based on flag in Y
                    lda $c2	; get the selection number from the wallbox input buffer
                    sta $2c	; copy it to temp var used by the next MEMOREC subroutine 
                    jsr Sf8b1	; update MEMOREC plays tables
				;
				; ($50, $52, $54, $58 all merge here
				;  all "play a selection" commands
				;  both record & video, regular & POPULAR)
				;
Lfe6f               ldx #$ee	; point to total records counter
                    lda $3b	; \ check (records/video) flag
                    beq Lfe77	; / skip next opcode if (records mode)
                    ldx #$e8	;   point instead to total videos counter
Lfe77               jsr Sf18d	; increment total records/videos counter (0-9999)
                    ldx $c2	; get (selection # sent from wallbox, from receive buffer)
                    jsr Sf1b2	; add X to the selection (appropriate record/video) playlist
                    pla		; \ retrieve the original (records/video mode) flag
                    sta $3b	; / return to prior records/video mode state
                    rts		; done 
				;
Lfe83               cmp #$59	; check for command code $59 
                    bne Lfea8	; if not, skip ahead to next check...
				;
				; command code from wallbox: $59 = "money in a wallbox"
				;
                    lda $c2	; get parameter from wallbox input buffer (nickels to be added to wallbox money counter)
				;
				; increment 2-byte counter for wallbox money
				;
                    ldx $02f1	; get $02f1 (MSB) to X
                    clc		; clear carry flag for addition
                    adc $02f0	; A=A+$02f0 (LSB)
                    bcc Lfe93	; if carry stayed clear, branch over next opcode
                    inx		;   increment X (MSB)
Lfe93               cpx #$27	; compare X (MSB) to $27 (max for 9999 decimal)
                    bcc Lfea1	; if A < 27 branch down
                    bne Lfe9d	; if A != 27 branch different
                    cmp #$0f	; compare A (LSB) to $0f (max for 9999 decimal)
                    bcc Lfea1	; if A < $0f branch down
				;
				; wallbox money counter exceeded maximum of 9999, rollover
				;
Lfe9d               sbc #$0f	; A = A - $0f (calculate rollover beyond 9999 or $270f???)
                    ldx #$00	; \  re-init
Lfea1               stx $02f1	;  } $02f0/$02f1
                    sta $02f0	; /  to 0s (plus the rollover)
                    rts		; done
				;
Lfea8               cmp #$25	; check for command code $25: re-send start-up data?
                    bne Lfec1	; if not, skip ahead to next check...
				;
				; command code from wallbox: $25 = "send all settings"
				;    puts commands $7c, $7d, $7e, $7f, $60, and $40 
				;    into wallbox send queue
				;    [alt. entry point; used once]
Sfeac               lda #$7c	; start with code $7c
Lfeae               jsr Sfcff	; put value into wallbox queue/log table
                    clc		;   clear carry flag for addition
                    adc #$01	;   A=A+1
                    bpl Lfeae	; loopback A<$80 (adds commands #$7d, #$7e, #$7f to wallbox queue)
                    lda #$60	; \
                    jsr Sfcff	; / put #$60 into wallbox queue (top selections)
                    lda #$40	; \
Lfebd               jsr Sfcff	; / put #$40 into wallbox queue (busy/idle state)
                    rts		; done
				;
Lfec1               cmp #$f0	; check for command code $f0
                    bne Lfed3	; if not, skip ahead to next check...
				;
				; command code from wallbox: $f0 = "clear all counters, selections & credits"
				;
                    lda #$02	; \ set MSB of pointer for page 2 (used in next subroutine)
                    sta $07	; / 
                    jsr Sf9d6	; zero out money & play counters (re-uses code)
                    jsr Sf4b7	; clear regular selection list
                    jsr Se749	; clear credits (and some other stuff)
                    rts		; done
				;
Lfed3               cmp #$f1	; check for command code $f1
                    bne Lfede	; if not, skip ahead to next check...
				;
				; command code from wallbox: $f1 = "POKE a byte into memory"
				;
                    ldy #$00	; set index/offset to 0
                    lda $c4	; get byte/value to place from wallbox input buffer
                    sta ($c2),y	; put byte/value at location pointed to by $c2/c3 in serial input buffer
                    rts		; done
				;
Lfede               cmp #$f2	; check for command code $f2
                    bne Lff02	; if not, skip ahead to next check...
				;
				; command code from wallbox: $f2 = "test keypad (& displays)"
				;
Lfee2               jsr Sf21b	; \  read keypad
                    lda $36	;  } get keycode
                    bmi Lfee2	; /  tight loop until any key is pressed
                    cmp #$0a	; \
                    bcc Lfef4	; / if it was a digit (0-9) branch down
				;
				; RESET or POPULAR was pressed
				;
                    clc		; (clear carry flag for addition)
                    beq Lfef2	; branch if RESET key was pressed
                    adc #$01	; if POPULAR was pressed, add 1 and 3, making charcode $0f for "F"
Lfef2               adc #$03	; if RESET was pressed, add 3, making charcode $0b for "=" (prompt char)
				;
				; a digit was pressed (and fallthru from above)
				;
Lfef4               jsr Sff37	; put charcode on all LEDs (using most of the clear LED code)
                    jsr Se453	; update LED display controllers
Lfefa               jsr Sf21b	; read keypad		   \
                    lda $47	; check debounce counter    } wait for any keypress
                    bne Lfefa	; loopback until debounced /
                    rts		; done (leaves digits on displays; likely overwritten quickly)
				;
Lff02               cmp #$f3	; check for command code $f3
                    bne Lff10	; if not, skip to next check...
				;
				; command code from wallbox: $f3 = "test LED segments with all 8s"
				;
Lff06               ldy #$10	; set loop counter to do all 16 LEDs 
                    jsr Se435	; put 888s on all LEDs
                    dec $c2	; \ decrement a counter in the recieve buffer (1st parameter of message from wallbox)
                    bne Lff06	; / delay loop until counter is 0
                    rts		; done
				;
Lff10               cmp #$f4	; check for command code $f4
                    bne Lff18	; if not, skip to next check...
				;
				; command code from wallbox: $f4 = "send PIA port status"
				;
                    lda #$e1	;   \ put $e1 command code into wallbox queue/log & rts
                    bne Lfebd	;   / (unconditional)
				;
Lff18               cmp #$f5	; check for command code $f5
                    bne Lff21	; if not, skip to next check...
				;
				; command code from wallbox: $f5 = "turntable motor off"
				; (doesn't appear to actually control hardware, just a variable???)
				;
                    lda #$00	;   \
                    sta $f3	;   / clear (turntable motor boolean???)
                    rts		;   done
				;
Lff21               cmp #$f6	; check for command code $f6
                    bne Lff29	; if not, skip to next check...
				;
				; command code from wallbox: $f6 = "PEEK a value in memory"
				;
                    lda #$e2	;   \ branch back to put $e2 into wallbox queue/log & rts
                    bne Lfebd	;   / address bytes in buffer will be used to fetch the byte
				;
Lff29               cmp #$f7	; check for command code $f7
                    bne Lff30	; if not, skip to rts (there are no other valid command codes)
				;
				; command code from wallbox: $f7 = "update the RAM checksum"
				;   (presumably to be used after POKEing into programmable RAM)
				;
                    jsr Sef18	; update RAM checksum
Lff30               rts		; done
				;
                    		;********************
				; CLEAR LED DISPLAYS
				;********************
				;  $ff31 entry does not clear CCC display
				;  $ff35 entry clears all LEDs
				;  only clears RAM storage, does *not* transmit to LED controllers
				;
				; clear top LEDs
				;
Sff31               lda #$0e	; blank digit code 
                    bne Lff3f	; unconditional branch, skips clearing the CCC LEDs
				;
				; clear all LEDs
				;
Sff35               lda #$0e	; A=$0e [blank digit code for LEDs]
Sff37               sta $5e	; \
                    sta $5f	;  \ all 4 CCC digits
                    sta $60	;  /
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
Sff4d               lda $2002	; \  get PIA1-B
                    and #$fd	;  } clear bit 1 %1111 1101 [output low; inverted; inactive/high oupout]
                    sta $2002	; /  output to PIA1-B
                    lda #$12	; \
                    sta $63	; / set detent counter/timer
                    rts		; 
                    		;
				;***********
				; DETENT ON
				;***********
				; 
Sff5a               lda $2002	; \  get PIA1-B
                    ora #$02	;  } set bit 1 %0000 0010 [output high; inverted; active low output]
                    sta $2002	; /  output to PIA1-B
                    lda #$12	; \
                    sta $63	; / set detent counter/timer
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
                    sta $00,x	; zero out last byte [suboptimal loop design?]
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
				; list of values used to read money inputs (ref. $eac4)
				;
Lff86               00 35 39 3d 31 3f
				; 
				; 35= 0011 0101 [010] selects D1 (5-cent)
				; 39= 0011 1001 [100] selects D2 (10-cent)
				; 3d= 0011 1101 [110] selects D3 (25-cent)
				; 31= 0011 0001 [000] selects D0 (50-cent)
				; 3f= 0011 1111 [111] selects D7 (dollar bill validator)
				;
				;****************************
				; DATA TABLE: READING KEYPAD
				;****************************
				; ref. $f221, selects each keypad input via PIA2-B
				;
Lff8c		    7d 7c 78 74 7f 73 72 76 7a 79 75 71
				;
				;     21   1020
				;          ___  
				;     rrxx sssr
				; 7d= 0111 1101 selects (011) D3 rtn, (100) S4: "0" key
				; 7c= 0111 1100 selects (010) D2 rtn, (100) S4: "1" key
				; 78= 0111 1000 selects (010) D2 rtn, (101) S5: "2" key
				; 74= 0111 0100 selects (010) D2 rtn, (110) S6: "3" key
				; 7f= 0111 1111 selects (011) D3 rtn, (000) S0: "4" key
				; 73= 0111 0011 selects (011) D3 rtn, (011) S3: "5" key
				; 72= 0111 0010 selects (010) D2 rtn, (011) S3: "6" key
				; 76= 0111 0110 selects (010) D2 rtn, (010) S2: "7" key
				; 7a= 0111 1010 selects (010) D2 rtn, (001) S1: "8" key
				; 79= 0111 1001 selects (011) D3 rtn, (101) S5: "9" key
				; 75= 0111 0101 selects (011) D3 rtn, (110) S6: "RESET" key
				; 71= 0111 0001 selects (011) D3 rtn, (111) S7: "POPULAR" key
				;
				;*************************************
				; DATA TABLE: VARIABLE INITILIZATIONS
				;*************************************
				;
Lff98               00 3a 42 62 64 69 74 83 b0 bf	; list of zero-page addresses
Lffa2		    00 12 0b 3c ff 08 64 3c 80 80	; list of initial values
				;
				; location $3a (init $12) timer for pulsing the coin-counter
				; location $42 (init $0b) counter/timer for the transfer mechanism
				; location $62 (init $3c) seconds countdown timer
				; location $64 (init $ff) (unknown use)
				; location $69 (init $08) (unknown use, possibly video related)
				; location $74 (init $64) (magazine initialization counter?)
				; location $83 (init $3c) seconds countdown timer used for autoplay mode
				; location $b0 (init $80) (serial buffer out flag/length?)
				; location $bf (init $80) (serial buffer in  flag/length?)
				;
				;******************************
				; DATA TABLE: FACTORY SETTINGS
				;******************************
				;
Lffac		    00 00 01 02 03 04 05 06 08 09 0a 0b 0e 0f 10 13	; offsets into page 3
                    14 15 16 17 18 19 1c 1e 20 21 2b 39 		; (programmable memory)
				;	
Lffc8		    00 05 0a 0f 14 64 01 02 05 1e 0a 14 64 01 02 0a	; values copied to RAM
		    01 02 05 0a 14 05 ff ff 02 14 1e ff 		; (factory settings for programmable features)
				;
				;  0 -  5 (*5 =  .25)\
				;  1 - 10 (*5 =  .50) \
				;  2 - 15 (*5 =  .75)  } record credit pricing levels
				;  3 - 20 (*5 = 1.00) /
				;  4 - 100(*5 = 5.00)/
				;  5 - 1             \
				;  6 - 2              \
				;                      } record credits by level
				;  8 - 5              /
				;  9 - 30            /
				; 10 - 10 (*5 = .50) \
				; 11 - 20 (*5 = 1.00) } video credit pricing levels
				; 14 - 100(*5 = 5.00)/
 				; 15 - 1             \
				; 16 - 2              } video credits by level
				; 19 - 10            /
				;
				; 20 - 1 (*5 = .05) \
				; 21 - 2 (*5 = .10)  \
				; 22 - 5 (*5 = .25)   } coin switch levels (and bill value)
				; 23 - 10(*5 = .50)  /
				; 24 - 20(*5 =1.00) /
				; 25 - 5            coin multiplier
				; 28 - 255          retain credits on power-cycle
				; 30 - 255          records-only mode
				; 32 - 2            autoplay style 2 (B-sides)
				; 33 - 20           autoplay time (minutes)
				; 43 - 30           fill time with record during video search (seconds)
				; 57 - 255          FIFO mode
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
Lfff2                     ff ff ff ff ff ff ff ff			; (8 unused bytes)
				;
Lfffa		                                  f2 e1 f2 e1 f2 e1 	; 6502 vectors
