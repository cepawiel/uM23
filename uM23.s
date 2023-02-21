CODE_START_MI	EQU $0E ;mid byte of code start addr
CODE_START_HI	EQU $0F ;hi byte of code start addr
REGS_START		EQU	$80


	.include "header.s"		;provides requireds and headers
	
	.include "core.s"		;provides cpu core:
							;	core_start() no return, expects flash base address of code divided by 256 in $0F:$0E in bank 0
							;	vector_take(), takes vector number in ACC
							;requires:
							;	core_hypercall_handler() gets hypercall number in A, return ACC as 0 for success
							;	readmem()	ACC has pointer in ram where we have address, B has number of bytes we need to read, C is destination pointer
							;	writemem()	ACC has pointer in ram where we have address, B has number of bytes we need to read, C is destination pointer
							;	soc_wfe()	sleep using wfi
							;	soc_wfi()	sleep using wfe

	.include "soc.s"		;provides the SoC:
							;	readmem()	ACC has pointer in ram where we have address, B has number of bytes we need to read, C is destination pointer
							;	writemem()	ACC has pointer in ram where we have address, B has number of bytes we need to read, C is destination pointer
	.include "ui.s"			;provides 4x6 font, ability to draw it, and our main ui.
							;	entry point is ui_main()
							;	ui_draw_char() char in A, row in B, col in C; chars are 4 wide and 6 tall, clobbers R2, B, C, A, TMPBYTE0, TMPBYTE1, TMPBYTE2
							;	font_4x6 - the actual font structure
	.include "defrag.s"		;code for defragging (does ui for it too)
							;	defrag()	ACC has cluster number of the file, R0 points to u16 addr of direntry

_vec_start:
	JMPF  ui_main

core_hypercall_handler:
	ST    C
	SUB   #9
	LD    C
	BP    PSW, CY, hypercall_valid_number
hypercall_out_err:
	OR    #$FF
	RET
hypercall_valid_number:
	ROL
	ADD   #<hypercall_dispatch_table
	PUSH  ACC
	MOV   #>hypercall_dispatch_table, ACC
	ADDC  #0
	PUSH  ACC
	RET

hypercall_dispatch_table:
	JMP   hypercall_out_err			;0x00 => error
	JMP   hypercall_dbg_putchar		;0x01 => debug putchar
	JMP   hypercall_dbg_putval		;0x02 => debug print word
	JMP   hypercall_get_self_addr	;0x03 => get cur app's address in whole flash address map
	JMP   hypercall_get_font_addr	;0x04 => get address of 4x6 font in whole flash address map
	JMP   hypercall_flash_write		;0x05 => (R0 = addr, data at offset 0x80 in RAM1)
	JMP   hypercall_draw_char		;0x06 => draw a given char to screen using 4x6 font (R0 = char, R1 = row, R2 = col, R3 = bool highlight)
	JMP   hypercall_exec			;0x07 => exec to app at given flash address (R0 = address in whole-f;ash address space)
	JMP   hypercall_exit			;0x08 => exit to our UI

hypercall_exit:
	JMPF  ui_main

hypercall_dbg_putchar:
	;DBG
	.byte $51
	.byte $04
	;DBG
	JMPF  hypercall_out_success

hypercall_dbg_putval:
	;DBG
	;.byte $51
	;.byte $05
	;DBG
	JMPF  hypercall_out_success

hypercall_get_self_addr:
	LD    CODE_START_MI
	ST    REGS_START + 0 * 4 + 1
	LD    CODE_START_HI
	ST    REGS_START + 0 * 4 + 2
	MOV   #0, REGS_START + 0 * 4 + 0
	MOV   #3, REGS_START + 0 * 4 + 3
	JMPF  hypercall_out_success

hypercall_get_font_addr:
	MOV   #<font_4x6, REGS_START + 0 * 4 + 0
	MOV   #>font_4x6, REGS_START + 0 * 4 + 1
	MOV   #0, REGS_START + 0 * 4 + 2
	MOV   #3, REGS_START + 0 * 4 + 3
	JMPF  hypercall_out_success

hypercall_out_success:
	AND   #$00
	RET

hypercall_flash_write:
	LD    REGS_START + 0 * 4 + 3
	BNE   #$30, hypercall_out_err
	LD    REGS_START + 0 * 4 + 2
	ROR
	OR    REGS_START + 0 * 4 + 0
	AND   #$7F
	BNZ   hypercall_out_err

	LD    REGS_START + 0 * 4 + 0
	ST    B
	LD    REGS_START + 0 * 4 + 1
	ST    C
	LD    REGS_START + 0 * 4 + 2
	ST    VRMAD2

	SET1  PSW, RAMBK0
	PUSH  $7C
	PUSH  $7D
	PUSH  $7E
	PUSH  $7F
	
	MOV   #$00, $7C
	LD    VRMAD2
	ST    $7D
	LD    C
	ST    $7E
	LD    B
	ST    $7F
	
	PUSH  OCR
	MOV   #$81, OCR
	CALLF vmu_flash_write
	POP   OCR

	POP   $7F
	POP   $7E
	POP   $7D
	POP   $7C
	CLR1  PSW, RAMBK0
	JMP   hypercall_out_success

hypercall_draw_char:
	LD    REGS_START + 3 * 4 + 0
	ST    VRMAD2
	LD    REGS_START + 2 * 4 + 0
	ST    C
	LD    REGS_START + 1 * 4 + 0
	ST    B
	LD    REGS_START + 0 * 4 + 0
	CALLF ui_draw_char
	JMPF  hypercall_out_success

hypercall_exec:
	LD    REGS_START + 0 * 4 + 3
	SUB   #3
	OR    REGS_START + 0 * 4 + 0
	BNZ   hypercall_out_success			;invalid addr cuses a return but not an invalid opcode trap
	LD    REGS_START + 0 * 4 + 2
	AND   #$FE
	BNZ   hypercall_out_success			;invalid addr cuses a return but not an invalid opcode trap
	LD    REGS_START + 0 * 4 + 2
	ST    CODE_START_HI
	LD    REGS_START + 0 * 4 + 1
	ST    CODE_START_MI
	JMPF  core_start







