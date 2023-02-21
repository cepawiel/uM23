;our memory map:
;	in bank0:
;		0x00-0x7f	unused/os
;		0x80-0xBF	arm regs
;		0xC0-0xC0	arm SR's important bits
;		0xC1-0xFF	stack
;	in bank1:
;		0x00-0xFF	RAM1 for arm
;	in work ram
;		0x00-0x1FF	RAM2 for arm
;
;ARM memory map (all address spaces repeat on size boundaries, except GFX which repeats on 512 byte boundary)
;	0x00000000 - 0x0001FFFF	code  (accesses where app is as addr 0, limited to app end [TBD])
;	0x10000000 - 0x100000ff RAM1
;	0x20000000 - 0x200001ff RAM2
;	0x30000000 - 0x3001ffff flash (access to all flash as it is)
;	0x40000000 - 0x4000007F LC68K SFRs
;	0x50000000 - 0x50000104 LC68K graphics memory
;	0x60000000 - 0x60000004 our fake irq controller regs
;
;	loads or stores crossing address spaces arent supported (not possible using aligned accesses anyways)
;	we assume code starts on a 512-byte boundary (block of flash) as we expect code to be in a savefile [we only need 256-byte boundary]
;	invalid opcodes may execute unpredictably
;
;
;IRQ numbers LO:
;	16 - INT0
;	17 - INT1
;	18 - INT2
;	19 - INT3
;	20 - SIO0
;	21 - SIO1
;	22 - MAPLE
;	23 - P3
;IRQ numbers HI:
;	24 - T0L.OVF
;	25 - T0H.OVF
;	26 - T1L.OVF
;	27 - T1H.OVF
;	28 - BASE_TIMER_0
;	29 - BASE_TIMER_1
;	30 - unused
;	31 - unused



;some SFRs
FLSHCTRL	EQU	$154



soc_wfe:
	JMPF  API_sleep

soc_wfi:
	SET1  PCON, 0
	RET

readwritemem_setup_ram2:
	LD   @R1					;get addr_low
	ST   VRMAD1
	INC  RR1					;point to addr_mid
	LD   @R1					;get addr_mid
	ST   VRMAD2
	RET

readwritemem_setup_gfx:
	LD   @R1					;get addr_low
	ST   RR2					;put into R2
	SET1 RR2, 7					;top bit must be set to acess GFX
	ROLC						;prevous top bit now in carry bit
	INC  RR1					;point to addr_mid
	LD   @R1					;get addr_mid
	ROLC						;bits 7..15 of addr are now in ACC (only lower 2 matter)
	ST   XBNK					;save them
	RET

readmem_sfr:
	LD    @R1					;get addr_low
	ST    RR2					;put into R2
	CLR1  RR2, 7				;top bit must be clear to acess SFRs
	PUSH  OCR					;must be done
	JMPF  readmem_hi			;now do the read

readmem_irqc:
	LD   C
	ST   RR0
	LD   B
	BNE  #2, readmem_fault
	LD   @R1
	BE   #0, readmem_irqc_sta
	BE   #2, readmem_irqc_ena
	JMPF readmem_fault
readmem_irqc_sta:
	MOV  #ARM_IRQS_LO, RR0
	JMPF readmem_irqc_common
readmem_irqc_ena:
	MOV  #ARM_IRQE_LO, RR0
	
readmem_irqc_common:
	LD   @R0
	ST   @R1
	INC  RR0
	INC  RR1
	DBNZ B, readmem_irqc_common
	RET


readmem:						;ACC has pointer in ram where we have address, B has number of bytes we need to read, C is destination pointer, clobbers R0 & R1
	;DBG
	;.byte $51
	;.byte $F0
	;DBG
	ST   RR1					;R1 - point to lowest word of address
	ADD  #3						;R0 - point to word that has bits 31..24 (what we use to tell apart the memory space)
	ST   RR0					;save it to R0 so we can deref it
	
								;do an alignment check before reading
	LD   B
	SUB  #1
	AND  @R1
	BZ   read_align_check_ok
	MOV  #3, ACC
	JMPF vector_take
	
read_align_check_ok:
	LD   @R0
	ROL
	ROL
	ROL
	ROL
	AND  #$0F
	BZ   readmem_code
	BE   #2, readmem_ram2
	BP   PSW, CY, readmem_ram1
	BE   #4, readmem_sfr
	BP   PSW, CY, readmem_flash
	BE   #6, readmem_irqc
	BP   PSW, CY, readmem_gfx
	;default case: fault

readmem_fault:
	JMPF mem_fault


readmem_ram1:
	LD   C
	ST   RR0					;destination address in bank0
	LD   @R1					;byte in bank1 we need
	BZ   readmem_ram1_addr_0	;special case (if asked to load byte 0, we cannot use R0 as pointer as we'll return bad data. we'll use another R0)
readmem_ram1_addr_nonzero:
	SET1 PSW, RAMBK0			;go to bank 1
	PUSH RR0					;save R0 (it has program data)
	ST   RR0
readmem_ram1_nz_byt:
	LD   @R0					;get byte
	INC  RR0
	CLR1 PSW, RAMBK0			;go to bank 0
	ST   @R0					;put byte
	INC  RR0
	SET1 PSW, RAMBK0			;go to bank 1
	DBNZ B, readmem_ram1_nz_byt	;again?
	POP  RR0					;restore R0
	CLR1 PSW, RAMBK0			;go to bank 0
	RET

readmem_code:
	LD    @R1					;get addr_low
	INC   RR1					;point to addr_mid_lo
	ST    TRL					;low byte of flash addr is ready
	LD    @R1					;get addr_mid
	INC   RR1					;point to addr_mid_hi
	ADD   CODE_START_MI			;add code start address
	ST    TRH					;mid byte of flash addr is ready
	LD    @R1					;get addr_hi
	ADDC  CODE_START_HI			;add code start address
	AND   #$01
	ST    FLSHCTRL				;address is ready

readmem_ldf:					;common code for flash reading (we know we'll never overflow TRL inc since alignment)
	LD    C
	ST    RR0					;destination address in bank0
	
readmem_ldf_byt:
	.byte $50					;LDF
	INC   TRL
	ST    @R0					;put byte
	INC   RR0
	DBNZ  B, readmem_ldf_byt		;again?
	RET

readmem_ram2:
	CALLF readwritemem_setup_ram2
	LD    C
	ST    RR0					;destination address in bank0
	
readmem_ram2_byt:
	LD    VTRBF
	ST    @R0					;put byte
	INC   RR0
	DBNZ  B, readmem_ram2_byt	;again?
	RET

readmem_flash:
	LD    @R1					;get addr_low
	INC   RR1					;point to addr_mid_lo
	ST    TRL					;low byte of flash addr is ready
	LD    @R1					;get addr_mid
	INC   RR1					;point to addr_mid_hi
	ST    TRH					;mid byte of flash addr is ready
	LD    @R1					;get addr_hi
	AND   #$01
	ST    FLSHCTRL				;address is ready
	JMPF  readmem_ldf			;common code

readmem_gfx:
	CALLF readwritemem_setup_gfx
	PUSH  OCR
	MOV   #$81, OCR
	
readmem_hi:						;common code
	LD    C
	ST    RR0					;destination address in bank0
	
readmem_hi_byt:
	LD    @R2
	INC   RR2
	ST    @R0					;put byte
	INC   RR0
	DBNZ  B, readmem_hi_byt		;again?
	POP   OCR					;undo OCR push
	RET

;here for range reasons
readmem_ram1_addr_0:			;same as above but using another R0
	SET1 PSW, RAMBK0			;go to bank 1
	PUSH RR0_2					;save R0#2 (it has program data)
	ST   RR0_2
readmem_ram1_z_byt:
	SET1 PSW, IRBK1				;setup cpu to use Rj set #2
	LD   @R0					;get byte
	CLR1 PSW, IRBK1				;setup cpu to use Rj set #0 (default)
	INC  RR0_2
	CLR1 PSW, RAMBK0			;go to bank 0
	ST   @R0					;put byte
	INC  RR0
	SET1 PSW, RAMBK0			;go to bank 1
	DBNZ B, readmem_ram1_z_byt	;again?
	POP  RR0_2					;restore R0#2
	CLR1 PSW, RAMBK0			;go to bank 0
	RET


writemem_irqc:
	LD   B
	BNE  #2, writemem_fault
	LD   C
	ST   RR0
	LD   @R1
	BE   #0, writemem_irqc_sta
	BE   #2, writemem_irqc_ena
	JMPF writemem_fault
writemem_irqc_sta:
	LD   @R0
	XOR  #$FF
	AND  ARM_IRQS_LO
	ST   ARM_IRQS_LO
	INC  RR0
	LD   @R0
	XOR  #$FF
	AND  ARM_IRQS_HI
	ST   ARM_IRQS_HI
	RET
writemem_irqc_ena:
	LD   @R0
	ST   ARM_IRQE_LO
	INC  RR0
	LD   @R0
	ST   ARM_IRQE_HI
	RET

writemem:						;ACC has pointer in ram where we have address, B has number of bytes we need to write, C is pointer in RAM where we have data
	;DBG
	;.byte $51
	;.byte $F1
	;DBG
	ST   RR1					;R1 - point to lowest word of address
	ADD  #3						;R0 - point to word that has bits 24..31 (what we use to tell apart the memory space)
	ST   RR0					;save it to R0 so we can deref it
	
								;do an alignment check before writing
	LD   B
	SUB  #1
	AND  @R1
	BZ   write_align_check_ok
	MOV  #3, ACC
	JMPF vector_take
	
write_align_check_ok:

	LD   @R0
	ROL
	ROL
	ROL
	ROL
	AND  #$0F
	BZ   writemem_fault				;code is not writeable
	BE   #2, writemem_ram2
	BP   PSW, CY, writemem_ram1
	BE   #4, writemem_sfr
	BP   PSW, CY, writemem_fault	;flash is not writeable
	BE   #6, writemem_irqc
	BP   PSW, CY, writemem_gfx

writemem_fault:
mem_fault:
	MOV  #3, ACC
	JMPF vector_take


writemem_ram1:					;pretty much same story as readmem_ram1
	LD   C						;source address (in bank 0)
	ST   RR0					;save in R0
	LD   @R1					;byte in bank1 we need
	BZ   writemem_ram1_addr_0	;special case (if asked to store byte 0, we cannot use R0 as pointer as we'll write bad data. we'll use another R0)
writemem_ram1_addr_nonzero:
	SET1 PSW, RAMBK0			;go to bank 1
	PUSH RR0					;save R0 (it has program data)
	ST   RR0
writemem_ram1_nz_byt:
	CLR1 PSW, RAMBK0			;go to bank 0
	LD   @R0					;get byte
	INC  RR0
	SET1 PSW, RAMBK0			;go to bank 1
	ST   @R0					;put byte
	INC  RR0
	DBNZ B, writemem_ram1_nz_byt;again?
	POP  RR0					;restore R0
	CLR1 PSW, RAMBK0			;go to bank 0
	RET

writemem_ram1_addr_0:
	SET1  PSW, RAMBK0			;go to bank 1
	PUSH  RR0_2					;save R0 (it has program data)
	ST    RR0_2
writemem_ram1_z_byt:
	CLR1  PSW, RAMBK0			;go to bank 0
	LD    @R0					;get byte
	INC   RR0
	SET1  PSW, RAMBK0			;go to bank 1
	SET1  PSW, IRBK1			;setup cpu to use Rj set #2
	ST    @R0					;put byte
	CLR1  PSW, IRBK1			;setup cpu to use Rj set #0 (default)
	INC   RR0_2
	DBNZ  B, writemem_ram1_z_byt;again?
	POP   RR0_2					;restore R0
	CLR1  PSW, RAMBK0			;go to bank 0
	RET

writemem_ram2:
	LD    C						;source address (in bank 0)
	ST    RR0					;save in R0
	CALLF readwritemem_setup_ram2

writemem_ram2_byt:
	LD    @R0					;get byte
	INC   RR0
	ST    VTRBF					;put it
	DBNZ  B, writemem_ram2_byt	;again?
	RET

writemem_gfx:
	CALLF readwritemem_setup_gfx
	PUSH  OCR
	MOV   #$81, OCR

writemem_hi:
	LD    C						;source address (in bank 0)
	ST    RR0					;save in R0

writemem_hi_byt:
	LD    @R0					;get byte
	INC   RR0
	ST    @R2					;put byte
	INC   RR2
	DBNZ  B, writemem_hi_byt	;again?
	POP   OCR					;undo that push
	RET
	
writemem_sfr:
	LD   @R1					;get addr_low
	ST   RR2					;put into R2
	CLR1 RR2, 7					;top bit must be clear to acess SFRs
	PUSH OCR					;must be done
	JMPF writemem_hi			;now do the write




