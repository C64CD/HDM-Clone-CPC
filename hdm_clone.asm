;
; HDM CLONE
;

; Code, graphics and music by TMR


; A quick, written-from-scratch copy of the Amstrad CPC demo
; HDM Demo 1 by Happy DemoCoder using a cover of Hymn To Yezz
; by Karsten Obarski - coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the Pasmo cross assembler from
; http://pasmo.speccy.org/
; Disk images are constructed with CPCDiskXP, available from
; http://www.cpcmania.com/cpcdiskxp/cpcdiskxp.htm-

; build.bat will call both to create an assembled file, crunch
; it and bundle the results into a DSK image.


; Memory Map
; $2000 -		program code/data
; $4000 - $4bff		music driver
; $4c00 - $4fff		music data
; $5000 - $8fff		bitmapped screen


; Label assignments
rt_store	equ $1ff0

scroll_1_timer	equ $1ff1
char_1_read_pos	equ $1ff2		; two bytes used
scroll_1_pos	equ $1ff4		; two bytes used

scroll_2_timer	equ $1ff7
char_2_read_pos	equ $1ff8		; two bytes used
scroll_2_pos	equ $1ffa		; two bytes used

colour_pos	equ $1ffc		; two bytes used
sine_at		equ $1ffe


scroll_1_ram	equ $c230
scroll_2_ram	equ $c730


; Entry point for the code
		org $2000

; Change screen resolution to mode 1
code_start	ld a,$01
		call $bc0e

; Set up a simple interrupt handler (taken from a Kev Thacker example)
		di			; disable interrupts
		im 1			; interrupt mode 1
		ld hl,$c9fb		; C9 / FB == instructions EI:RET
		ld ($0038),hl		; Set interrupt vector
		ei			; enable interrupts

; Initialise the bitmap colours to black for now
		ld bc,$7f10
		out (c),c
		ld c,$54
		out (c),c

		ld bc,$7f00
		out (c),c
		ld c,$54
		out (c),c

		ld bc,$7f01
		out (c),c
		ld c,$54
		out (c),c

		ld bc,$7f02
		out (c),c
		ld c,$54
		out (c),c

		ld bc,$7f03
		out (c),c
		ld c,$54
		out (c),c

; Copy the picture up to the screen RAM
		ld hl,picture_data		; set source
		ld de,$c000			; set destination
		ld bc,$3fd0			; set copy length
		ldir

; Set up the music driver
		ld de,music_data
		call music_driver+$00

; Reset the scrolling messages
		ld de,scroll_chars_1
		ld (char_1_read_pos),de
		ld a,$02
		ld (scroll_1_timer),a
		call reset_1

		ld de,scroll_chars_2
		ld (char_2_read_pos),de
		ld a,$04
		ld (scroll_2_timer),a
		call reset_2

; Reset the colour movement
		ld a,$00
		ld (sine_at),a
		call colour_set_pos


; Wait for the top of the frame
main_loop	ld b,$f5
raster_wait	in a,(c)
		rra
		jr nc,raster_wait

; Set colour registers for the top of the frame
		ld bc,$7f10			; border
		out (c),c
		ld c,$44
		out (c),c

		ld bc,$7f01			; %01 pixels
		out (c),c
		ld c,$5c
		out (c),c

		ld bc,$7f02			; %10 pixels
		out (c),c
		ld c,$40
		out (c),c

		ld bc,$7f03			; %11 pixels
		out (c),c
		ld c,$4b
		out (c),c

; Start updating the lower scrolling message
scroll_mover_2	ld hl,scroll_2_ram+$0001	; set source
		ld de,scroll_2_ram+$0000	; set destination
		ld bc,$009f			; set copy length
		ldir

		ld hl,scroll_2_ram+$0801	; set source
		ld de,scroll_2_ram+$0800	; set destination
		ld bc,$009f			; set copy length
		ldir

		ld hl,scroll_2_ram+$1001	; set source
		ld de,scroll_2_ram+$1000	; set destination
		ld bc,$009f			; set copy length
		ldir

; "Pause" updating the scroll to do the colour splitter
		halt

		ld e,$68
splitter_wait	nop
		dec e
		jp nz,splitter_wait
		nop
		nop
		nop

; First colour split loop
		ld e,$2a		; number of scanlines
		ld hl,(colour_pos)	; positions of colour tables
		ld ix,colour_table_2

splitter_1	ld a,(hl)		; read colour value
		ld bc,$7f00		; set colour register
		out (c),c
		out (c),a		; write to register

		ld bc,$7f10		; set colour register
		out (c),c
		out (c),a		; write to register

		ld a,(ix)		; read colour value
		ld bc,$7f02		; set colour register
		out (c),c
		out (c),a		; write to register

		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		inc hl
		inc ix
		dec e
		jp nz,splitter_1

; One scanline worth of split to time out where the interrupt is
		ld a,(hl)		; read colour value
		ld bc,$7f00		; set colour register
		out (c),c
		out (c),a		; write to register

		ld bc,$7f10		; set colour register
		out (c),c
		out (c),a		; write to register

		ld a,(ix)		; read colour value
		ld bc,$7f02		; set colour register
		out (c),c
		out (c),a		; write to register

		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		inc hl
		inc ix

		ld a,(ix)		; read colour value
		ld bc,$7f02		; set colour register
		out (c),c
		out (c),a		; write to register


		ld a,(hl)		; read colour value
		ld bc,$7f00		; set colour register
		out (c),c
		out (c),a		; write to register

		ld bc,$7f10		; set colour register
		out (c),c
		out (c),a		; write to register



		nop
		nop

		inc hl
		inc ix

		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		ld e,$10		; number of scanlines
splitter_2	ld a,(hl)		; read colour value
		ld bc,$7f00		; set colour register
		out (c),c
		out (c),a		; write to register

		ld bc,$7f10		; set colour register
		out (c),c
		out (c),a		; write to register

		ld a,(ix)		; read colour value
		ld bc,$7f02		; set colour register
		out (c),c
		out (c),a		; write to register

		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		inc hl
		inc ix
		dec e
		jp nz,splitter_2



; Set colour registers for the upper scroller
		nop
		nop
		nop
		nop

		ld bc,$7f10			; border
		out (c),c
		ld c,$44
		out (c),c

		ld bc,$7f00			; background colour to black
		out (c),c
		ld c,$44
		out (c),c

		ld bc,$7f01			; %01 pixels
		out (c),c
		ld c,$5c
		out (c),c

		ld bc,$7f02			; %10 pixels
		out (c),c
		ld c,$40
		out (c),c

		ld bc,$7f03			; %11 pixels
		out (c),c
		ld c,$4b
		out (c),c

; Continue updating the lower scrolling message
		ld hl,scroll_2_ram+$1801	; set source
		ld de,scroll_2_ram+$1800	; set destination
		ld bc,$009f			; set copy length
		ldir

; Set colour registers for the picture
		ld bc,$7f01			; %01 pixels
		out (c),c
		ld c,$54
		out (c),c

		ld bc,$7f02			; %10 pixels
		out (c),c
		ld c,$4c
		out (c),c

		ld bc,$7f03			; %11 pixels
		out (c),c
		ld c,$4b
		out (c),c

; Continue updating the lower scrolling message
		ld hl,scroll_2_ram+$2001	; set source
		ld de,scroll_2_ram+$2000	; set destination
		ld bc,$009f			; set copy length
		ldir

		ld hl,scroll_2_ram+$2801	; set source
		ld de,scroll_2_ram+$2800	; set destination
		ld bc,$009f			; set copy length
		ldir

		ld hl,scroll_2_ram+$3001	; set source
		ld de,scroll_2_ram+$3000	; set destination
		ld bc,$009f			; set copy length
		ldir

		ld hl,scroll_2_ram+$3801	; set source
		ld de,scroll_2_ram+$3800	; set destination
		ld bc,$009f			; set copy length
		ldir

; Copy in a new column of graphics data (using less-than-neat unrolled code)
		ld bc,(char_2_read_pos)

		ld a,(bc)
		ld (scroll_2_ram+$004f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$084f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$104f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$184f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$204f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$284f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$304f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$384f),a

		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$009f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$089f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$109f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$189f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$209f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$289f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$309f),a
		inc bc
		ld a,(bc)
		ld (scroll_2_ram+$389f),a

		inc bc
		ld (char_2_read_pos),bc

; Check if we need a new character from the scroll text...
		ld a,(scroll_2_timer)
		dec a
		jp nz,no_new_char_2

; ...and if so, grab one
		ld hl,(scroll_2_pos)		; load HL with current text position
mread_2		ld a,(hl)			; read text from HL's pos. into A
		cp $ff
		jp nz,okay_2

		call reset_2
		jp mread_2

okay_2		ld (rt_store),a
		ld de,char_2_decode

		ld h,$00			; load the character code to HL
		ld l,a
		ld de,char_2_decode		; add the start in RAM of the decode table
		add hl,de
		ld a,(hl)

		ld h,a				; incoming char to upper byte of hl and...
		ld l,$00
		ccf
		sra h				; ...divide by four (saves doing x64 from)
		rr l				; the lower byte!)
		sra h
		rr l

		ld bc,scroll_chars_2		; add on the start of font
		add hl,bc
		ld (char_2_read_pos),hl

; Get the width of the new character
		ld a,(rt_store)
		ld h,$00			; load the character code to HL
		ld l,a
		ld de,char_2_width		; add the start in RAM of the decode table
		add hl,de
		ld a,(hl)

; Bump the text counter
		ld hl,(scroll_2_pos)
		inc hl
		ld (scroll_2_pos),hl

; Skip to here if a new character isn't needed
no_new_char_2	ld (scroll_2_timer),a


; Update the upper scrolling message
scroll_mover_1	ld hl,scroll_1_ram+$0001	; set source
		ld de,scroll_1_ram+$0000	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$0801	; set source
		ld de,scroll_1_ram+$0800	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$1001	; set source
		ld de,scroll_1_ram+$1000	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$1801	; set source
		ld de,scroll_1_ram+$1800	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$2001	; set source
		ld de,scroll_1_ram+$2000	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$2801	; set source
		ld de,scroll_1_ram+$2800	; set destination
		ld bc,$004f			; set copy length
		ldir

; Set colour registers for the lower scroller
		ld e,$3c
colour_wait_2	nop
		dec e
		jp nz,colour_wait_2

		ld bc,$7f01			; %01 pixels
		out (c),c
		ld c,$5c
		out (c),c

		ld bc,$7f02			; %10 pixels
		out (c),c
		ld c,$40
		out (c),c

		ld bc,$7f03			; %11 pixels
		out (c),c
		ld c,$4b
		out (c),c

; Back to the upper scrolling message
		ld hl,scroll_1_ram+$3001	; set source
		ld de,scroll_1_ram+$3000	; set destination
		ld bc,$004f			; set copy length
		ldir

		ld hl,scroll_1_ram+$3801	; set source
		ld de,scroll_1_ram+$3800	; set destination
		ld bc,$004f			; set copy length
		ldir

; Copy in a new column of graphics data (using less-than-neat unrolled code)
		ld bc,(char_1_read_pos)

		ld a,(bc)
		ld (scroll_1_ram+$004f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$084f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$104f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$184f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$204f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$284f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$304f),a
		inc bc
		ld a,(bc)
		ld (scroll_1_ram+$384f),a

		inc bc
		ld (char_1_read_pos),bc

; Check if we need a new character from the scroll text...
		ld a,(scroll_1_timer)
		dec a
		jp nz,no_new_char_1

; ...and if so, grab one
		ld hl,(scroll_1_pos)		; load HL with current text position
mread_1		ld a,(hl)			; read text from HL's pos. into A
		cp $ff
		jp nz,okay_1

		call reset_1
		jp mread_1

okay_1		ld (rt_store),a
		ld de,char_2_decode

		ld h,$00			; load the character code to HL
		ld l,a
		ld de,char_2_decode		; add the start in RAM of the decode table
		add hl,de
		ld a,(hl)

		ld h,a				; incoming char to upper byte of hl and...
		ld l,$00
		ccf
		sra h				; ...divide by 16
		rr l
		sra h
		rr l
		sra h
		rr l
		sra h
		rr l

		ld bc,scroll_chars_1		; add on the start of font
		add hl,bc
		ld (char_1_read_pos),hl

; Set the width of the character
		ld a,$02

; Bump the text counter
		ld hl,(scroll_1_pos)
		inc hl
		ld (scroll_1_pos),hl

; Skip to here if a new character isn't needed
no_new_char_1	ld (scroll_1_timer),a

; Update the colour split position
		call colour_set_pos


; Play music and jump to the start of the main loop
		call music_driver+$03

		jp main_loop


; Scroll 1 reset
reset_1		ld hl,scroll_text_1
		ld (scroll_1_pos),hl
		ret

; Scroll 2 reset
reset_2		ld hl,scroll_text_2
		ld (scroll_2_pos),hl
		ret

; Update and set the colour split position
colour_set_pos	ld a,(sine_at)
		inc a
		ld (sine_at),a

		ld h,$00		; load curve position to HL
		ld l,a
		ld de,colour_sinus	; add the start in RAM of the curve to HL
		add hl,de
		ld c,(hl)		; load low byte of BC from the curve
		ld b,$00

		ld hl,colour_table_1
		add hl,bc
		ld (colour_pos),hl

		ret


; Colour tables for splits
colour_table_1	defb $54,$54,$54,$54,$5c,$54,$5c,$5c		; red bars
		defb $5c,$4c,$5c,$4c,$4c,$4c,$4e,$4c
		defb $4e,$4e,$4e,$4a,$4e,$4a,$4a,$4a
		defb $4b,$4a,$4b,$4b,$4b,$4a,$4b,$4a
		defb $4a,$4a,$4e,$4a,$4e,$4e,$4e,$4c
		defb $4e,$4c,$4c,$4c,$5c,$4c,$5c,$5c
		defb $5c,$54,$5c

		defb $54,$54,$54,$54,$58,$54,$58,$58		; purple bars
		defb $58,$45,$58,$45,$45,$45,$4d,$45
		defb $4d,$4d,$4d,$4f,$4d,$4f,$4f,$4f
		defb $4b,$4f,$4b,$4b,$4b,$4f,$4b,$4f
		defb $4f,$4f,$4d,$4f,$4d,$4d,$4d,$45
		defb $4d,$45,$45,$45,$58,$45,$58,$58
		defb $58,$54,$58

		defb $54,$54,$54,$54,$44,$54,$44,$44		;blue bars
		defb $44,$55,$44,$55,$55,$55,$57,$55
		defb $57,$57,$57,$53,$57,$53,$53,$53
		defb $4b,$53,$4b,$4b,$4b,$53,$4b,$53
		defb $53,$53,$57,$53,$57,$57,$57,$55
		defb $57,$55,$55,$55,$44,$55,$44,$44
		defb $44,$54,$44

		defb $54,$54,$54,$54

colour_table_2	defb $44,$44,$44,$44,$44,$44,$44,$44
		defb $44,$44,$44,$44,$44,$44,$55,$44
		defb $55,$55,$55,$57,$55,$57
		defb $57,$57,$53,$57,$53,$53,$53
		defb $4b,$53,$4b,$4b,$4b

		defb $5c,$5c,$5c
		defb $5e,$5c,$5e,$5e,$5e,$4e,$5e,$4e
		defb $4e,$4e,$4a,$4e,$4a,$4a,$4a,$4a

; Colour movement sine table
colour_sinus	defb $31,$32,$33,$34,$36,$37,$38,$39
		defb $3a,$3c,$3d,$3e,$3f,$40,$41,$42
		defb $44,$45,$46,$47,$48,$49,$4a,$4b
		defb $4c,$4d,$4e,$4f,$50,$51,$52,$53
		defb $53,$54,$55,$56,$57,$57,$58,$59
		defb $59,$5a,$5b,$5b,$5c,$5c,$5d,$5d
		defb $5e,$5e,$5f,$5f,$5f,$60,$60,$60
		defb $61,$61,$61,$61,$61,$61,$61,$61

		defb $61,$61,$61,$61,$61,$61,$61,$61
		defb $61,$60,$60,$60,$5f,$5f,$5f,$5e
		defb $5e,$5d,$5d,$5c,$5c,$5b,$5b,$5a
		defb $59,$59,$58,$57,$56,$56,$55,$54
		defb $53,$52,$51,$50,$50,$4f,$4e,$4d
		defb $4c,$4b,$4a,$49,$48,$46,$45,$44
		defb $43,$42,$41,$40,$3f,$3d,$3c,$3b
		defb $3a,$39,$38,$36,$35,$34,$33,$32

		defb $30,$2f,$2e,$2d,$2c,$2a,$29,$28
		defb $27,$26,$24,$23,$22,$21,$20,$1f
		defb $1e,$1d,$1b,$1a,$19,$18,$17,$16
		defb $15,$14,$13,$12,$11,$10,$0f,$0f
		defb $0e,$0d,$0c,$0b,$0b,$0a,$09,$08
		defb $08,$07,$06,$06,$05,$05,$04,$04
		defb $03,$03,$02,$02,$02,$01,$01,$01
		defb $00,$00,$00,$00,$00,$00,$00,$00

		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$01,$01,$01,$02,$02,$02,$03
		defb $03,$04,$04,$05,$05,$06,$07,$07
		defb $08,$09,$09,$0a,$0b,$0c,$0c,$0d
		defb $0e,$0f,$10,$11,$12,$13,$13,$14
		defb $15,$16,$18,$19,$1a,$1b,$1c,$1d
		defb $1e,$1f,$20,$21,$23,$24,$25,$26
		defb $27,$28,$2a,$2b,$2c,$2d,$2e,$30

; Yank in the bitmapped fonts (8x8 then 16x16 pixel)
scroll_chars_1	incbin binary/font_1.raw
scroll_chars_2	incbin binary/font_2.raw

; Decoder tables for the scrolltext - position in graphics data
char_2_decode	defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00

		defb $00,$1d,$00,$00,$00,$00,$00,$2a	; space ! [null5] '
		defb $2e,$2f,$00,$00,$1c,$1f,$1b,$2d	; ( ) [null2] , - . /
		defb $20,$21,$22,$23,$24,$25,$26,$27	; 0 to 7
		defb $28,$29,$2b,$2c,$00,$00,$00,$1e	; 8 to 9 : ; [null3] ?

		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00
		defb $00,$00,$00,$00,$00,$00,$00,$00

		defb $00,$01,$02,$03,$04,$05,$06,$07	; a to g
		defb $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	; h to o
		defb $10,$11,$12,$13,$14,$15,$16,$17	; p to w
		defb $18,$19,$1a,$00,$00,$00,$00,$00	; x to z

; Decoder tables for the scrolltext - width of character in bytes
char_2_width	defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02

		defb $04,$02,$02,$02,$02,$02,$02,$02	; space ! [null5] '
		defb $03,$03,$02,$02,$02,$04,$02,$02	; ( ) [null2] , - . /
		defb $04,$02,$04,$04,$04,$04,$04,$04	; 0 to 7
		defb $04,$04,$02,$02,$02,$02,$02,$04	; 8 to 9 : ; [null3] ?

		defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02
		defb $02,$02,$02,$02,$02,$02,$02,$02

		defb $02,$04,$04,$04,$04,$04,$04,$04	; a to g
		defb $04,$02,$04,$04,$04,$04,$04,$04	; h to o
		defb $04,$04,$04,$04,$04,$04,$04,$04	; p to w
		defb $04,$04,$04,$02,$02,$02,$02,$02	; x to z

; Upper scrolling message text
scroll_text_1	defm "c64cd send a couple of super fast scrolling greetings to "
		defm "the harlow cracking service ... "
		defm "rob hubbard ... "
		defm "happy democoder"
		defm "               "

		defm "and a quick wave to the nice people at the cpcwiki forums "
		defm "who will probably be annoyed by all the c64-style colour "
		defm "tables in this demo."
		defm "               "

		defm "anti-greetings to c64hater for banging on endlessly about "
		defm "programming but not actually releasing anything!"
		defm "               "

		defm "               "

		defb $ff		; end of text marker

; Lower scrolling message text
scroll_text_2	defm "hello!!!   welcome to another little production from c64cd "
		defm "called   --- hdm clone ---"
		defm "               "

		defm "code, graphics and music conversion by t.m.r"
		defm "               "

		defm "based on the 1991 release hdm demo 1 by happy demomaker."
		defm "               "

		defm "i really have to agree with what hdm member arkad said in "
		defm "their release about writing scrolltexts, it can sometimes be "
		defm "harder than actually coding the demo!    this has been my "
		defm "second text this week and the ideas well is again pretty barren, "
		defm "so perhaps c64cd really needs another member to deal with that "
		defm "side of things; i can concentrate on the blog and churning out "
		defm "demos, they can hammer out text...?    assuming anyone is daft "
		defm "enough to join of course!"
		defm "               "

		defm "speaking of the greetings, what appears in the upper "
		defm "scroller are groups and people whose demos have been an "
		defm "inspiration to c64cd releases so it'll grow as that catalogue "
		defm "expands over time - a few more names may appear if other "
		defm "groups actually start greeting c64cd but i'm hoping most of "
		defm "them are at least a little more sensible than that."
		defm "               "

		defm "i've nothing left to say now so might as well wrap the "
		defm "scroller - hopefully people are finding the source code "
		defm "interesting at least, although i'm pretty sure that seasoned "
		defm "z80 coders will just be laughing at my n00bishness over this "
		defm "one..."
		defm "               "

		defm "look out for the next c64cd demo which should be a c64 release "
		defm "and this was t.m.r, signing off in early november 2015... .. .  ."
		defm "               "

		defb $ff		; end of text marker


; Arkos Tracker music driver and binary data
		org $4000
music_driver	incbin binary/arkos_driver.bin
		org $4c00
music_data	incbin binary/hymn_to_yezz.bin


; Bitmapped picture
		org $5000
picture_data	incbin binary/screen.raw


; Set the initialisation address for the code
		end $2000