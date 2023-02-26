;our list of files found is stored at the end of ram like so: {u8 TRH, u8 TRL}, this var stores next write loc (and thus total number of files indirectly)
;we allow at most 100 files to leave ourselves space for other data structures
;we look for non-game files with names ending in ".C23"
;just like the ROM, we assume hi bytes of superblock structs are all zeroes
;dir entries are 32 bytes each:
;	{
;		[00h]	u8 type;			//0x33 - savedata, 0xcc - game, 0x00 - empty dir entry, other - other file (ignored by rom)
;		[01h]	u8 flags;			//set to 0xff to disable copying, else 0
;		[02h]	u16 firstBlock;		//first block on disk
;		[04h]	char name[12];		//name (padded with space or zero)
;		[10h]	u8 timestamp[8];	//a mess of BCD
;		[18h]	u16 fileSz;			//in blocks
;		[1Ah]	u16 hdrOfst;		//in blocks, header offset in file (used by games)
;		[1Ch]	u8 reserved[4];		//no use observed
;	}



TRH_VAL_FAT			EQU		$10
TRH_VAL_DIR			EQU		$11
FILE_LIST_WPTR		EQU		$12
NUM_FILES			EQU		$13		;not authoritative 0 use din ui only - and calced there
TOP_FILE			EQU		$14		;on screen now
SEL_FILE			EQU		$15		;index of selected file onscreen

str_no_files:
	.string 60 "No files    found. Load some  beforerunning uM23(c) dmitrygr"

str_cluster:
	.string 9 "Cluster:"

str_size:
	.string 6 "Size:"

str_file_sel_instr:
	.string 13 "A=go  B=back"

str_file_fragmented:
	.string 60 "This file isfragmented &can't be runDefragment? A=yes   B=no"

ui_main:
	MOV   #$92, OCR			;fast
	MOV   #$7F, SP
	MOV   #0, STAD
	SET1  PSW, RAMBK0
	MOV   #$FF, FILE_LIST_WPTR
	SET1  FLSHCTRL, 0
	CALLF read_superblock				;init flash access
	CALLF read_directory				;enum all our files
	LD    FILE_LIST_WPTR				;see if we found any files, if so go to ui
	BNE   #$FF, ui_with_files
ui_no_files:

	CALLF clear_screen
	MOV   #<str_no_files, TRL
	MOV   #>str_no_files, TRH
	
	AND   #$00
	ST    VRMAD2
	ST    B
	ST    C
	CALLF ui_rom_str

hang:
	MOV   #$81, OCR
	CALLF ui_waitkey
	JMP   hang
	
ui_with_files:
	DEC   FILE_LIST_WPTR
	SET1  FILE_LIST_WPTR, 0				;if we arrive here from our own code that already INC-ed it, this will restore it whereas if we did not, it is safe
	OR    #$FF
	SUB   FILE_LIST_WPTR
	ROR									;guaranteed to not have low bit set so this is safe
	ST    NUM_FILES
	AND   #$00
	ST    TOP_FILE
	ST    SEL_FILE
	INC   FILE_LIST_WPTR				;now points to first entry
	
ui_loop:
										;first, we draw the ui
	CALLF clear_screen
	MOV   #0, B

ui_draw_items_loop:
										;get pointer (in FLASHCTL:TRH:TRL) to name of the onscreen file entry
	LD    TOP_FILE
	ADD   B
	ROL
	AND   #$FE
	ADD   FILE_LIST_WPTR
	BZ    ui_draw_loop_done				;no more files - stop drawing
	ST    RR0
	LD    @R0
	ST    TRH
	INC   RR0
	LD    @R0
	ADD   #$04							;safe to do since we know dir entry is 0x20-aligned
	ST    TRL
										;figure out if we shoudl draw it selected or not
	CLR1  VRMAD2, 0
	LD    B
	BNE   SEL_FILE, cur_file_not_selected
	SET1  VRMAD2, 0
cur_file_not_selected:
										;draw the name onscreen (the entire one - this is safe), also keep track of where the last period was (in C)
	CALLF ui_draw_item
	
	INC   B
	LD    B
	BNE   #5, ui_draw_items_loop		;we show 5 entries onscreen=
										;clear last column of the firts 4 rows

ui_draw_loop_done:
	CLR1  VRMAD2, 0
	MOV   #0, B
	MOV   #11, C
clear_last_col_loop:
	MOV   #$20, ACC
	CALLF ui_draw_char_noclobber
	INC   B
	LD    B
	BNE   #5, clear_last_col_loop
										;draw arrows if needed
	LD    TOP_FILE
	BZ    up_arrow_processed
	MOV   #$80, ACC						;up arrow
	MOV   #0, B
	MOV   #11, C
	CALLF ui_draw_char_noclobber
up_arrow_processed:
	
	LD    NUM_FILES
	AND   #$FC
	BZ    dn_arrow_processed			;not enough files for an arrow
	LD    NUM_FILES
	SUB   TOP_FILE
	SUB   #5
	BZ    dn_arrow_processed			;lowest page - no arrow
	MOV   #$81, ACC						;down arrow
	MOV   #4, B
	MOV   #11, C
	CALLF ui_draw_char_noclobber
dn_arrow_processed:
										;read buttons and see what to do
wait_buttons:
	CALLF ui_waitkey

	BP    B, 4, ui_btn_sel
	BP    B, 1, ui_btn_down
	BP    B, 0, ui_btn_up
	JMPF  wait_buttons

ui_btn_up:
	LD    SEL_FILE
	BZ    ui_btn_up_move_top
	DEC   SEL_FILE
	JMPF  ui_loop
ui_btn_up_move_top:
	LD    TOP_FILE
	BZ    wait_buttons
	DEC   TOP_FILE
	JMPF  ui_loop

ui_btn_down:
	LD    SEL_FILE
	BE    #4, ui_btn_dn_move_top
	ADD   #1
	SUB   NUM_FILES
	BN    PSW, CY, wait_buttons
	INC   SEL_FILE
	JMPF  ui_loop
ui_btn_dn_move_top:
	LD    TOP_FILE
	ADD   SEL_FILE
	ADD   #1
	SUB   NUM_FILES
	BN    PSW, CY, wait_buttons
	INC   TOP_FILE
	JMPF  ui_loop

ui_btn_sel:								;file is selected - show file selected ui
	CALLF clear_screen
	LD    TOP_FILE
	ADD   SEL_FILE
	ROL
	AND   #$FE
	ADD   FILE_LIST_WPTR
	ST    RR0
	LD    @R0
	ST    TRH
	INC   RR0
	LD    @R0
	ADD   #$04
	ST    TRL
										;draw name on top row
	MOV   #0, B
	SET1  VRMAD2, 0
	CALLF ui_draw_item
	CLR1  VRMAD2, 0
										;draw cluster info
	MOV   #<str_cluster, TRL
	MOV   #>str_cluster, TRH
	MOV   #2, B
	MOV   #0, C
	CALLF ui_rom_str
	
	LD    @R0							;draw cluster number
	ADD   #$02
	ST    TRL
	DEC   RR0
	LD    @R0
	ST    TRH
	.byte $50;LDF
	ST    VRMAD1
	AND   #$00
	MOV   #2, B
	MOV   #11, C
	CALLF ui_drawnum16
	
										;draw size info
	MOV   #<str_size, TRL
	MOV   #>str_size, TRH
	MOV   #3, B
	MOV   #0, C
	CALLF ui_rom_str
	
	
	LD    @R0							;draw file size number
	ST    TRH
	INC   RR0
	LD    @R0
	ADD   #$18
	ST    TRL
	.byte $50;LDF
	ROL
	PUSH  ACC
	AND   #$80
	ST    VRMAD1
	POP   ACC
	AND   #$7F
	MOV   #3, B
	MOV   #11, C
	CALLF ui_drawnum16
	
	MOV   #<str_file_sel_instr, TRL
	MOV   #>str_file_sel_instr, TRH
	CLR1  VRMAD2, 0
	MOV   #4, B
	MOV   #0, C
	CALLF ui_rom_str
	
sel_item_btn_loop:
	CALLF ui_waitkey
	BP    B, 4, ui_file_go				;A = go
	BN    B, 5, sel_item_btn_loop		;other buttons = ignore
	JMPF  ui_with_files					;B = back

ui_file_go:
	CALLF clear_screen
	LD    @R0							;get cluster number for code_start value core needs
	ADD   #$02
	ST    TRL
	DEC   RR0
	LD    @R0
	ST    TRH
	.byte $50;LDF
	PUSH  ACC
	CALLF verify_file_sequential
	BZ    ui_file_fragmented
	POP   ACC
	CLR1  PSW, RAMBK0					;values we're writing are in bank 0
	ROL
	PUSH  ACC
	AND   #$FE
	ST    CODE_START_MI
	POP   ACC
	AND   #$01
	ST    CODE_START_HI
	JMPF  core_start

ui_file_fragmented:
	MOV   #<str_file_fragmented, TRL
	MOV   #>str_file_fragmented, TRH
	AND   #$00
	ST    VRMAD2
	ST    B
	ST    C
	CALLF ui_rom_str
ui_frag_btn_loop:
	CALLF ui_waitkey
	BP    B, 4, ui_defrag
	BN    B, 5, ui_frag_btn_loop
	DEC   SP							;ACC was pushed - no need for pop
	JMPF  ui_btn_sel

ui_defrag:
	POP   ACC 	; put cluster number in ACC
	JMPF defrag	; no need to call as defrag will jump back to ui_main
				; since we need to recheck files after moving them around
	

ui_drawnum16:			;low byte in VRMAD1, high byte in ACC, B & C preset for screen coords for drawing
	PUSH  TRH
	PUSH  TRL
	PUSH  B
	PUSH  C
	
	PUSH  ACC
	LD    VRMAD1
	ST    C
	POP   ACC
	JMPF  ui_drawnum16_loop_has_work	;always draw at least a zero
	
ui_drawnum16_loop:
	BNZ   ui_drawnum16_loop_has_work
	BE    C, ui_drawnum16_loop_done
ui_drawnum16_loop_has_work:
	MOV   #10, B
	DIV
	ST    TRH
	LD    C
	ST    TRL
	LD    B
	ADD   #$30
	POP   C
	POP   B
	CALLF ui_draw_char_noclobber
	DEC   C
	PUSH  B
	PUSH  C
	LD    TRL
	ST    C
	LD    TRH
	JMPF  ui_drawnum16_loop
ui_drawnum16_loop_done:
	POP   C
	POP   B
	POP   TRL
	POP   TRH
	RET
	
	



ui_rom_str:								;assumes TRH:TRL points to name in low ROM (LDC is used), B and C set for starting coords (r, c). Clobbers VRMAD1, B, C
										;stops after screen end or NULL char. assumes VRMAD2 set properly too
	MOV   #0, VRMAD1
ui_rom_str_loop:
	LD    VRMAD1
	LDC
	BZ    ui_rom_str_out
	CALLF ui_draw_char_noclobber
	INC   VRMAD1
	INC   C
	LD    C
	SUB   #12
	BNZ   ui_rom_str_loop
	MOV   #0, C
	INC   B
	LD    B
	SUB   #5
	BNZ   ui_rom_str_loop

ui_rom_str_out:
	RET

ui_draw_item:							;assumes FLASHCTRL:TRH:TRL points to name, VRMAD2 has "is highlited" bit set, B is current row to draw on
	MOV   #0, C
	
item_draw_loop:
	.byte $50;LDF
	BNZ   ui_char_not_zero				;replace zeroes with spaces
	MOV   #$20, ACC
ui_char_not_zero:
	PUSH  C
	ST    VRMAD1
	LD    TRL
	AND   #$1F
	SUB   #4
	ST    C
	LD    VRMAD1
	CALLF ui_draw_char_noclobber
	LD    VRMAD1
	BNE   #$2E, item_char_not_dot
	POP   ACC							;throw away the pushed C and replace with current
	PUSH  C
item_char_not_dot:
	POP   C
	INC   TRL							;when addr gets to 0x1? from start of direntry, we're done with the name
	BN    TRL, 4, item_draw_loop
										;now replace all columns on and after the last dot with spaces
item_spaces_loop:
	MOV   #$20, ACC
	CALLF ui_draw_char_noclobber
	INC   C
	LD    C
	BNE   #12, item_spaces_loop
										;if we're not donw drawing all files yet, go on to the next iteration
	RET


ui_waitkey:								;returns single key mask in B
	MOV   #$FF, P3
	LD    P3
	BNE   #$FF, ui_waitkey				;wait for all buttons up
	
ui_waitkey2:							;returns single key mask in B
	MOV   #$FF, P3
	LD    P3
	XOR   #$FF							;invert
	BZ   ui_waitkey2					;wait for some buttons down
	
	ST    B								;save button(s)
	SUB   #1
	AND   B
	BNZ   ui_waitkey					;if more than one, go wait some more
	
	BP    B, 7, waitkey_btn_sleep
	BP    B, 6, waitkey_btn_quit
	RET

waitkey_btn_sleep:
	PUSH  OCR
	MOV   #$81, OCR
	CALLF API_sleep
	POP   OCR
	JMPF  ui_main

waitkey_btn_quit:
	MOV   #$81, OCR
	JMPF  API_game_end

read_superblock:
										;pepare to read superblock
	MOV   #$FE, TRH
										;read where FAT is
	MOV   #$46, TRL
	.byte $50;LDF
	ROL
	AND   #$FE
	ST    TRH_VAL_FAT
										;read where directory starts
	MOV   #$4A, TRL
	.byte $50;LDF
	ROL
	AND   #$FE
	ST    TRH_VAL_DIR
	RET


clear_screen:
	PUSH  OCR
	MOV   #$81, OCR
	PUSH  RR2
	AND   #$00
	ST    XBNK
clr_screen_loop_outer:
	MOV   #$80, RR2
clr_screen_loop_inner:
	ST    @R2
	INC   RR2
	BP    RR2, 7, clr_screen_loop_inner
	INC   XBNK
	BN    XBNK, 1, clr_screen_loop_outer
	POP   RR2
	POP   OCR
	RET

read_directory:

	LD    FILE_LIST_WPTR
	ST    RR0
	LD    TRH_VAL_DIR

dir_block_read_loop_start_iter:
	ST    TRH
	MOV   #0, TRL

dir_block_read_loop:
	PUSH  TRL
	.byte $50;LDF
	BNE   #$33, dir_ent_is_not_interesting
										;check name
	LD    TRL
	ADD   #12 + 4 - 1					;point to last byte of name
	ST    TRL

	MOV   #12, B
name_search_loop:
	.byte $50;LDF
	DEC   TRL
	BE    #$33, name_search_loop_mrkr_found
	AND   #$DF							;convert 0x20 and 0x00 to 0x00
	BNZ   dir_ent_is_not_interesting
	DBNZ  B, name_search_loop
	JMP   dir_ent_is_not_interesting

name_search_loop_mrkr_found:
	LD    TRL
	AND   #$1F							;check for enough length left to contain our full ext and at least a char of name
	SUB   #5
	BP    PSW, CY, dir_ent_is_not_interesting
	.byte $50;LDF
	BNE   #$32, dir_ent_is_not_interesting
	DEC   TRL
	.byte $50;LDF
	BNE   #$4D, dir_ent_is_not_interesting
	DEC   TRL
	.byte $50;LDF
	BNE   #$2E, dir_ent_is_not_interesting
										;we found something of interest - record it
	POP   ACC
	PUSH  ACC							;makes life easier later to re-push trl
	ST    @R0
	DEC   RR0
	LD    TRH
	ST    @R0
	DEC   RR0
	LD    RR0							;prevent overflow with too many files
	BE    #255 - 100 * 2, no_more_dir_blocks

dir_ent_is_not_interesting:
	POP   ACC
	ADD   #$20
	ST    TRL
	BN    PSW, CY, dir_block_read_loop	; no trl overflow - we did not even finish a single 256-byte area
	BP    TRH, 0, dir_block_done		; we just finished a 512-byte block
	INC   TRH
	JMP   dir_block_read_loop

dir_block_done:
	LD    TRH							;has low bit set - good - we'l ROR it to top bit which we need set
	ROR
	CALLF get_next_cluster
	BE    #$FF, no_more_dir_blocks		;if no more blocks then no more blocks
	CLR1  PSW, CY
	ROLC								;convert to TRH address (and simultaneously move top bit to CY for checking that it is high)
	ST    TRH
	MOV   #0, TRL
	BP    PSW, CY, dir_block_read_loop_start_iter

no_more_dir_blocks:
	LD    RR0
	ST    FILE_LIST_WPTR
	RET


; verify if all blocks of file are sequential
; input: 	ACC = first block
; output:	ACC == 0 if not sequential else sequential
verify_file_sequential:
	PUSH  B			; save B to stack

verify_file_sequential_loop:
	ST    B							; store block in B
	CALLF get_next_cluster			; Input ACC as current block
									; Output ACC as next block
									; 0xFF if none
	BE    #$FF, verify_file_sequential_success	; branch if no more blocks
	SUB   B							; calc difference between current block
									; and next block found
	BNE   #$01, verify_file_sequential_fail	; anything other than 1 block
											; difference is a failure
	ADD   B								; advance to next block
	JMPF  verify_file_sequential_loop	; check next block

verify_file_sequential_success:
	OR    #$FF		; set ACC to 0xFF
	POP   B			; restore B from stack
	RET
verify_file_sequential_fail:
	AND   #$00		; clear ACC
	POP   B			; restore B from stack
	RET
	
; get next block from flash
; input: 	ACC = block
; output:	ACC = next block, 0xFF if none
get_next_cluster:
	; store values to be restored later
	PUSH  TRL				
	PUSH  TRH
	PUSH  FLSHCTRL			; store existing FLASH page reg
	SET1  FLSHCTRL, 0		; select upper (DATA) flash area

	; Calculate offset into Fat Block from Last Block
	SET1  PSW, CY			; Set Carry Flag
	ROLC					; Multiply Block x2 and add 1 from carry
							; this is because the FatBlock is formated
							; as uint16_t[256]. Adding 1 to the address 
							; selects the upper byte (normally lower is 
							; 0 due to VMU flash size)
	ST    TRL				; Store in lower flash addr reg

	AND   #$00				; Clear ACC
	ADDC  TRH_VAL_FAT		; Add Carry from rotate to TRH_VAL_FAT
	ST    TRH				; Write upper Table Reference Reg
	LDF						; load flash byte into ACC
	BZ    get_next_cluster_continue		; if upper byte == 0 then goto reading lower byte
										; is it stores the next block positions.
										; This is only an acceptable check as the VMU has
										; 0-255 (0x0000-0x00FF) blocks. 0xFFFA are
										; an end of chain, and 0xFFFC is an
										; empty block
	OR    #$FF					; set return value for no block found
	JMP   get_next_cluster_out
get_next_cluster_continue:
	DEC   TRL				; Decrement Flash Address to lower byte
	LDF						; Read next block number into ACC
get_next_cluster_out:
	POP   FLSHCTRL			; restore FLASH page reg
	POP   TRH 				; restore flash address from stack
	POP   TRL
	RET


	
	









ui_draw_char_noclobber:				;char in A, row in B, col in C; VRMAD2's bottom bit set means invert char, else not, chars are 4 wide and 6 tall
	
	PUSH  B
	PUSH  C
	PUSH  RR2
	PUSH  TRH
	PUSH  TRL
	PUSH  TMPBYTE0
	PUSH  TMPBYTE1
	PUSH  TMPBYTE2
	CALL  ui_draw_char
	POP   TMPBYTE2
	POP   TMPBYTE1
	POP   TMPBYTE0
	POP   TRL
	POP   TRH
	POP   RR2
	POP   C
	POP   B
	RET
	
ui_draw_char:				;char in A, row in B, col in C; VRMAD2's bottom bit set means invert char, else not, chars are 4 wide and 6 tall, clobbers R2, B, C, TRH, TRL, 
	
	SUB   #$20
							;get pointer to char in TRH:TRL
	PUSH  ACC
	ADD   #<font_4x6
	ST    TRL
	AND   #$00
	ADDC  #>font_4x6
	ST    TRH
	POP   ACC
	CLR1  PSW, CY
	ROLC
	ADD   TRL
	ST    TRL
	AND   #$00
	ADDC  TRH
	ST    TRH
							;convert B from "char row" to "pixel row", then get pointer to said row into XBNK: R2
	LD    B
	ROL
	ADD   B					;A = B * 3. each 2 rows of LCD are 16 bytes, mul by 16
	MOV   #$80, RR2
	MOV   #0, XBNK
	CALLF ror4
	CALLF adjRR2			;now carry is our bank number and A is the pointer in bank to the row
							;get pointer to byte of the char we'll be using
	LD    C
	ROR
	AND   #$7F
	ADD   RR2
	ST    RR2				;guaranteed to nor overflow
							;prepare to draw
	MOV   #3, B
char_two_rows:
	MOV   #3, ACC
	SUB   B
	LDC
	BN    VRMAD2, 0, no_invert
	XOR   #$FF
no_invert:
	ST    TMPBYTE0
	AND   #$F0
	MOV   #2, TMPBYTE2
	ST    TMPBYTE1
char_row:
	PUSH  OCR
	MOV   #$81, OCR
	LD    @R2
	BN    C, 0, char_not_second_1
	CALLF ror4
char_not_second_1:
	AND   #$0F
	OR    TMPBYTE1
	BN    C, 0, char_not_second_2
	CALLF ror4
char_not_second_2:
	ST    @R2
	POP   OCR
	MOV   #6, ACC
	CALLF adjRR2
	LD    TMPBYTE0
	CALLF ror4
	AND   #$F0
	ST    TMPBYTE1
	
	DEC   TMPBYTE2
	BP    TMPBYTE2, 0, char_row
	MOV   #4, ACC
	CALLF adjRR2
	DBNZ  B, char_two_rows
	
	RET

ror4:
	ROR
	ROR
	ROR
	ROR
	RET

adjRR2:
	BP    XBNK, 0, adjRR2_bank0
	CLR1  RR2, 7
adjRR2_bank0:
	ADD   RR2
	MOV   #0, XBNK
	BN    ACC, 7, adjRR2_bank1
	INC   XBNK
adjRR2_bank1:
	OR    #$80
	ST    RR2
	RET


font_4x6:
	;top nibble is higher row. LSB is left. 0x20..0x7f. missing char 0x7f
	;"tom thumb" font 
	.byte $00
	.byte $00
	.byte $00
	.byte $44
	.byte $40
	.byte $40
	.byte $AA
	.byte $00
	.byte $00
	.byte $AE
	.byte $AE
	.byte $A0
	.byte $6C
	.byte $6C
	.byte $40
	.byte $82
	.byte $48
	.byte $20
	.byte $CC
	.byte $EA
	.byte $60
	.byte $44
	.byte $00
	.byte $00
	.byte $24
	.byte $44
	.byte $20
	.byte $42
	.byte $22
	.byte $40
	.byte $A4
	.byte $A0
	.byte $00
	.byte $04
	.byte $E4
	.byte $00
	.byte $00
	.byte $04
	.byte $80
	.byte $00
	.byte $E0
	.byte $00
	.byte $00
	.byte $00
	.byte $40
	.byte $22
	.byte $48
	.byte $80
	.byte $6A
	.byte $AA
	.byte $C0
	.byte $4C
	.byte $44
	.byte $40
	.byte $C2
	.byte $48
	.byte $E0
	.byte $C2
	.byte $42
	.byte $C0
	.byte $AA
	.byte $E2
	.byte $20
	.byte $E8
	.byte $C2
	.byte $C0
	.byte $68
	.byte $EA
	.byte $E0
	.byte $E2
	.byte $48
	.byte $80
	.byte $EA
	.byte $EA
	.byte $E0
	.byte $EA
	.byte $E2
	.byte $C0
	.byte $04
	.byte $04
	.byte $00
	.byte $04
	.byte $04
	.byte $80
	.byte $24
	.byte $84
	.byte $20
	.byte $0E
	.byte $0E
	.byte $00
	.byte $84
	.byte $24
	.byte $80
	.byte $E2
	.byte $40
	.byte $40
	.byte $4A
	.byte $E8
	.byte $60
	.byte $4A
	.byte $EA
	.byte $A0
	.byte $CA
	.byte $CA
	.byte $C0
	.byte $68
	.byte $88
	.byte $60
	.byte $CA
	.byte $AA
	.byte $C0
	.byte $E8
	.byte $E8
	.byte $E0
	.byte $E8
	.byte $E8
	.byte $80
	.byte $68
	.byte $EA
	.byte $60
	.byte $AA
	.byte $EA
	.byte $A0
	.byte $E4
	.byte $44
	.byte $E0
	.byte $22
	.byte $2A
	.byte $40
	.byte $AA
	.byte $CA
	.byte $A0
	.byte $88
	.byte $88
	.byte $E0
	.byte $AE
	.byte $EA
	.byte $A0
	.byte $AE
	.byte $EE
	.byte $A0
	.byte $4A
	.byte $AA
	.byte $40
	.byte $CA
	.byte $C8
	.byte $80
	.byte $4A
	.byte $AE
	.byte $60
	.byte $CA
	.byte $EC
	.byte $A0
	.byte $68
	.byte $42
	.byte $C0
	.byte $E4
	.byte $44
	.byte $40
	.byte $AA
	.byte $AA
	.byte $60
	.byte $AA
	.byte $A4
	.byte $40
	.byte $AA
	.byte $EE
	.byte $A0
	.byte $AA
	.byte $4A
	.byte $A0
	.byte $AA
	.byte $44
	.byte $40
	.byte $E2
	.byte $48
	.byte $E0
	.byte $E8
	.byte $88
	.byte $E0
	.byte $08
	.byte $42
	.byte $00
	.byte $E2
	.byte $22
	.byte $E0
	.byte $4A
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $E0
	.byte $84
	.byte $00
	.byte $00
	.byte $0C
	.byte $6A
	.byte $E0
	.byte $8C
	.byte $AA
	.byte $C0
	.byte $06
	.byte $88
	.byte $60
	.byte $26
	.byte $AA
	.byte $60
	.byte $06
	.byte $AC
	.byte $60
	.byte $24
	.byte $E4
	.byte $40
	.byte $06
	.byte $AE
	.byte $24
	.byte $8C
	.byte $AA
	.byte $A0
	.byte $40
	.byte $44
	.byte $40
	.byte $20
	.byte $22
	.byte $A4
	.byte $8A
	.byte $CC
	.byte $A0
	.byte $C4
	.byte $44
	.byte $E0
	.byte $0E
	.byte $EE
	.byte $A0
	.byte $0C
	.byte $AA
	.byte $A0
	.byte $04
	.byte $AA
	.byte $40
	.byte $0C
	.byte $AA
	.byte $C8
	.byte $06
	.byte $AA
	.byte $62
	.byte $06
	.byte $88
	.byte $80
	.byte $06
	.byte $C6
	.byte $C0
	.byte $4E
	.byte $44
	.byte $60
	.byte $0A
	.byte $AA
	.byte $60
	.byte $0A
	.byte $AE
	.byte $40
	.byte $0A
	.byte $EE
	.byte $E0
	.byte $0A
	.byte $44
	.byte $A0
	.byte $0A
	.byte $A6
	.byte $24
	.byte $0E
	.byte $6C
	.byte $E0
	.byte $64
	.byte $84
	.byte $60
	.byte $44
	.byte $04
	.byte $40
	.byte $C4
	.byte $24
	.byte $C0
	.byte $6C
	.byte $00
	.byte $00
	.byte $EE
	.byte $EE
	.byte $E0
	;up arrow - 0x80
	.byte $4E
	.byte $44
	.byte $44
	;down arrow - 0x81
	.byte $44
	.byte $44
	.byte $E4
	
