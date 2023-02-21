;	invalid opcodes may execute unpredictably (this is OK, that is what they're "invalid")
;	we sometimes use B,C,TRL,TRH as a temporary word in SFR space
;	0x004..0x00F are used for temporaries in bank0 as they are unused by us otherwise
;	regs are stored little endian

RR0				EQU $00 ;R0 pointer
RR1				EQU $01 ;R1 pointer
RR2				EQU $02
RR3				EQU $03
WORD_VAL		EQU	$04 ;$4..$7 in bank 0
WORD_VAL_NEXT2	EQU $08	;$8..$9 (used in B.W/BL, IRQ handling)
WORD_VAL_NEXT4	EQU $08	;$8..$9 (used in {U,S}M{ULL,LAL})
RR0_2			EQU $08	;R0 #2 pointer
TMPBYTE3		EQU $0A ;bank 0
TMPBYTE2		EQU $0B ;bank 0
TMPBYTE1		EQU $0C ;bank 0
TMPBYTE0		EQU $0D ;bank 0

WORD_IN_SFRS	EQU $02	;actually $102 = <B,C,TRL,TRH>

REGS_R12		EQU $B0
REGS_SP			EQU $B4
REGS_LR			EQU $B8
REGS_PC			EQU $BC
ARM_SR			EQU $C0			; [CNZV---I]NOTE that it is not in ARM order! all can be reordered except C, which we ASSUME is in top position, I = in irq (not taking nay more)
SP_START		EQU	$C3
ARM_IRQS_LO		EQU $FE			;our code is NEVER allowed to clear any bits here, assumptions exist about them not getting cleared suddenly. only ARM code may clear. at end of ram since we'll be modifying it even when not in emulator and we dont want to overwrite random UI stack bytes
ARM_IRQS_HI		EQU $FF			;our code is NEVER allowed to clear any bits here, assumptions exist about them not getting cleared suddenly. only ARM code may clear. at end of ram since we'll be modifying it even when not in emulator and we dont want to overwrite random UI stack bytes
ARM_IRQE_LO		EQU $C1
ARM_IRQE_HI		EQU $C2

SR_BIT_C		EQU $7
SR_BIT_N		EQU $6
SR_BIT_Z		EQU $5
SR_BIT_V		EQU $4
SR_BIT_I		EQU $0

;;dummy handlers for various shit
core_start:
	SET1 VSEL, 4				;we need auto-inc on VSEL
	MOV  #SP_START - 1, SP		;stack is full-ascending
	MOV  #0, PSW

								;now that setup is done, take the reset vector
	AND   #$00
	ST    WORD_VAL + 0
	ST    WORD_VAL + 1
	ST    WORD_VAL + 2
	ST    WORD_VAL + 3
	ST    ARM_IRQS_LO
	ST    ARM_IRQS_HI
	ST    ARM_IRQE_LO
	ST    ARM_IRQE_HI
	MOV   #REGS_SP, C
	MOV   #4, B
	MOV   #WORD_VAL, ACC
	CALLF readmem
	MOV   #4, WORD_VAL + 0

vec_take_jump:					;jump to *(uint32_t*)WORD_VAL
	MOV   #REGS_PC, C
	MOV   #4, B
	MOV   #WORD_VAL, ACC
	CALLF readmem
	CLR1  REGS_PC + 0, 0

fetch_instr_maybe_pc_changed:

	LD    REGS_PC + 3			;we treat anything with top word of FF as vec return - we just do not care about the rest of it
	BNE   #$FF, fetch_instr
	JMPF  vector_return
	; fallthrough

fetch_instr:					;we must arrive here with ram bank 0 selected

	;DBG
	;.byte $51
	;.byte $01
	;DBG

	BP    ARM_SR, SR_BIT_I, all_irqs_processed	;not accepring IRQs? ok
	LD    ARM_IRQS_LO
	AND   ARM_IRQE_LO
	BZ    lo_irqs_processed
	MOV   #0, B
	CALLF core_irqs_process
	JMPF  vector_take

lo_irqs_processed:
	LD    ARM_IRQS_HI
	AND   ARM_IRQE_HI
	BZ    all_irqs_processed
	MOV   #8, B
	CALLF core_irqs_process
	JMPF  vector_take
	
all_irqs_processed:
	MOV   #WORD_VAL, C			;where to store the result
	CALLF read_instr_16b
	
	;DBG
	;.byte $51
	;.byte $02
	;DBG
	
	;dispatch based on top 7 bits
	LD    WORD_VAL + 1			;we need top 7 bits in their current positions
	AND   #0xFE
	PUSH  ACC					;push bottom word of addr of our dispatch table
	MOV   #5, ACC				;we canot push 5 directly
	PUSH  ACC					;push top word now
	RET							;go there


;;dispatch table for instructions (exactly 256 bytes)
	.org $500
	JMP  instr_lsl_imm			;0000000
	JMP  instr_lsl_imm			;0000001
	JMP  instr_lsl_imm			;0000010
	JMP  instr_lsl_imm			;0000011
	JMP  instr_lsr_imm			;0000100
	JMP  instr_lsr_imm			;0000101
	JMP  instr_lsr_imm			;0000110
	JMP  instr_lsr_imm			;0000111
	JMP  instr_asr_imm			;0001000
	JMP  instr_asr_imm			;0001001
	JMP  instr_asr_imm			;0001010
	JMP  instr_asr_imm			;0001011
	JMP  instr_add_reg			;0001100
	JMP  instr_sub_reg			;0001101
	JMP  instr_add_imm3			;0001110
	JMP  instr_sub_imm3			;0001111
	JMP  instr_mov_imm			;0010000
	JMP  instr_mov_imm			;0010001
	JMP  instr_mov_imm			;0010010
	JMP  instr_mov_imm			;0010011
	JMP  instr_cmp_imm			;0010100
	JMP  instr_cmp_imm			;0010101
	JMP  instr_cmp_imm			;0010110
	JMP  instr_cmp_imm			;0010111
	JMP  instr_add_imm			;0011000
	JMP  instr_add_imm			;0011001
	JMP  instr_add_imm			;0011010
	JMP  instr_add_imm			;0011011
	JMP  instr_sub_imm			;0011100
	JMP  instr_sub_imm			;0011101
	JMP  instr_sub_imm			;0011110
	JMP  instr_sub_imm			;0011111
	JMP  instr_dp_part0			;0100000	AND EOR LSL LSR ASR ADC SBC ROR (register)
	JMP  instr_dp_part1			;0100001	TST CMP CMN ORR MUL BIC MVN (register) ; RSB (imm)
	JMP  instr_add_cmp_hi		;0100010	ADD or CMP on high regs
	JMP  instr_mov_hi_bx_blx	;0100011	MOV on high regs or BX/BLX
	JMP  instr_ldr_literal		;0100100
	JMP  instr_ldr_literal		;0100101
	JMP  instr_ldr_literal		;0100110
	JMP  instr_ldr_literal		;0100111
	JMP  instr_str_reg			;0101000
	JMP  instr_strh_reg			;0101001
	JMP  instr_strb_reg			;0101010
	JMP  instr_ldrsb_reg		;0101011
	JMP  instr_ldr_reg			;0101100
	JMP  instr_ldrh_reg			;0101101
	JMP  instr_ldrb_reg			;0101110
	JMP  instr_ldrsh_reg		;0101111
	JMP  instr_str_imm			;0110000
	JMP  instr_str_imm			;0110001
	JMP  instr_str_imm			;0110010
	JMP  instr_str_imm			;0110011
	JMP  instr_ldr_imm			;0110100
	JMP  instr_ldr_imm			;0110101
	JMP  instr_ldr_imm			;0110110
	JMP  instr_ldr_imm			;0110111
	JMP  instr_strb_imm			;0111000
	JMP  instr_strb_imm			;0111001
	JMP  instr_strb_imm			;0111010
	JMP  instr_strb_imm			;0111011
	JMP  instr_ldrb_imm			;0111100
	JMP  instr_ldrb_imm			;0111101
	JMP  instr_ldrb_imm			;0111110
	JMP  instr_ldrb_imm			;0111111
	JMP  instr_strh_imm			;1000000
	JMP  instr_strh_imm			;1000001
	JMP  instr_strh_imm			;1000010
	JMP  instr_strh_imm			;1000011
	JMP  instr_ldrh_imm			;1000100
	JMP  instr_ldrh_imm			;1000101
	JMP  instr_ldrh_imm			;1000110
	JMP  instr_ldrh_imm			;1000111
	JMP  instr_str_sp_imm		;1001000
	JMP  instr_str_sp_imm		;1001001
	JMP  instr_str_sp_imm		;1001010
	JMP  instr_str_sp_imm		;1001011
	JMP  instr_ldr_sp_imm		;1001100
	JMP  instr_ldr_sp_imm		;1001101
	JMP  instr_ldr_sp_imm		;1001110
	JMP  instr_ldr_sp_imm		;1001111
	JMP  instr_add_pc_imm		;1010000
	JMP  instr_add_pc_imm		;1010001
	JMP  instr_add_pc_imm		;1010010
	JMP  instr_add_pc_imm		;1010011
	JMP  instr_add_sp_imm		;1010100
	JMP  instr_add_sp_imm		;1010101
	JMP  instr_add_sp_imm		;1010110
	JMP  instr_add_sp_imm		;1010111
	JMP  instr_adj_sp_cbz		;1011000		//<<add sp, #xx>>; <<sub sp, #xx>>; cbz; maybe undef instrs
	JMP  instr_extend_cbz		;1011001		//{S,U}XT{,B,H}; cbz; maybe undef instrs
	JMP  instr_push				;1011010
	JMP  instr_cps_and_undef	;1011011		//CPS and undef
	JMP  instr_cbnz				;1011100		//CBNZ, incl some undef
	JMP  instr_cbnz_rev			;1011101		//REV*, CBNZ, bkpt, incl some undef
	JMP  instr_pop				;1011110
	JMP  instr_bkpt_hints		;1011111		//NOP hints & 32-bit we don't support
	JMP  instr_stmia			;1100000
	JMP  instr_stmia			;1100001
	JMP  instr_stmia			;1100010
	JMP  instr_stmia			;1100011
	JMP  instr_ldmia			;1100100
	JMP  instr_ldmia			;1100101
	JMP  instr_ldmia			;1100110
	JMP  instr_ldmia			;1100111
	JMP  instr_beq_bne			;1101000
	JMP  instr_bcs_bcc			;1101001
	JMP  instr_bmi_bpl			;1101010
	JMP  instr_bvs_bvc			;1101011
	JMP  instr_bhi_bls			;1101100
	JMP  instr_bge_blt			;1101101
	JMP  instr_bgt_ble			;1101110
	JMP  instr_svc_hypercall	;1101111		//incl permanently undefined instr 0xDExx which we use as hypercall
	JMP  instr_b				;1110000
	JMP  instr_b				;1110001
	JMP  instr_b				;1110010
	JMP  instr_b				;1110011
	;32-bit first half instrs
	JMP  instr_32b_prefix		;1110100		//32b first half: shit we have no use for but must support
	JMP  instr_undef			;1110101		//32b first half: shit we do not support here
	JMP  instr_undef			;1110110		//32b first half: shit we do not support here
	JMP  instr_undef			;1110111		//32b first half: shit we do not support here
	JMP  instr_32b_prefix		;1111000		//32b first half: things we support including (B BL)
	JMP  instr_32b_prefix		;1111001		//32b first half: things we support including (B BL MOVT MOVW MRS MSR)
	JMP  instr_32b_prefix		;1111010		//32b first half: things we support including (B BL)
	JMP  instr_32b_prefix		;1111011		//32b first half: things we support including (B BL MOVT MOVW)
	JMP  instr_undef			;1111100		//32b first half: shit we do not support here
	JMP  instr_sdiv_udiv_lmul	;1111101		//32b first half: things we support including (SDIV UDIV, long multiplies) dispartched differently since only those are here
	JMP  instr_undef			;1111110		//32b first half: shit we do not support here
	JMP  instr_undef			;1111111		//32b first half: shit we do not support here


read_instr_16b:					;dst ptr in C, clobbers B, R0, R1
	MOV   #REGS_PC, ACC			;pointer to addr passed in ACC
	MOV   #2, B					;number of bytes passed in B
	CALLF readmem
	MOV   #2, ACC
	MOV   #REGS_PC, RR0
	JMPF  add_R0_8bit

instr_32b_prefix:
	MOV  #WORD_VAL + 2, C
	CALLF read_instr_16b
	;DBG
	;.byte $51
	;.byte $03
	;DBG
	JMPF instr_32b

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 LSL(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_lsl_imm:
	CALLF get_regno3_0
	ST    RR0				;Rd pointer in R0
	CALLF get_regno3_3
	ST    RR1				;Rm pointer in R1
	CALLF cpy_word_R1_to_R0	;Rd = Rm (we no longer need Rm pointer)
	CALLF get_immed_5

inst_lsl_common:			;called for shifts <= 31 with R0 pointing to thing to shift plus 4, and ACC having shift amount
							;plus 4 because the above copy makes it so, and LSL Rx, Rx is rare, but LSL Rx, Ry, #0 is common (it is MOV)
							;so for this case we optimize for MOV speed
	BZ    instr_lsl_imm_shift_done
	
	;we have a nonzero amount of shifting to do - do it
	ST    B
instr_lsl_imm_shift_once:
	LD    RR0				;R0 -= 4
	SUB   #4
	ST    RR0
	CALLF lsl_R0
	DBNZ  B, instr_lsl_imm_shift_once

	CALLF copy_C_bit

instr_lsl_imm_shift_done:	;we arrive with R0 one byte past our reg and carry in SR properly set. set others
	JMPF  set_NZ_R0_with_rewind_and_goto_next_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 LSR(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_lsr_imm:
	CALLF get_regno3_0
	ST    RR0				;Rd pointer in R0
	CALLF get_regno3_3
	ST    RR1				;Rm pointer in R1
	CALLF get_immed_5
	BZ    instr_lsr_imm_is_0
	
	;we have a nonzero amount of shifting to do - do it
	ST    B
	CALLF cpy_word_R1_to_R0	;Rd = Rm (we no longer need Rm pointer)

inst_lsr_common:			;called with shift amount in B for shifts 1..31 bits and R0 pointing 4 past start of word
instr_lsr_imm_shift_once:
	MOV   #4, C
	CLR1  PSW, CY
instr_lsr_imm_shift_once_byt:
	DEC   RR0
	LD    @R0
	RORC
	ST    @R0
	DBNZ  C, instr_lsr_imm_shift_once_byt
	DBNZ  B, instr_lsr_imm_shift_more
	JMPF  instr_lsr_imm_shift_done	;to make sure carry survives
instr_lsr_imm_shift_more:
	LD    RR0
	ADD   #4
	ST    RR0
	JMPF  instr_lsr_imm_shift_once
instr_lsr_imm_shift_done:
	CALLF copy_C_bit
	JMPF  set_NZ_R0_no_rewind_and_goto_next_instr
	
instr_lsr_imm_is_0:			;special easy case - dst and flags almost all known (N = 0, Z = 1, C = Rm[31]
	AND  #$0
	MOV  #4, B
instr_lsr_imm_is_0_byt:
	ST   @R0
	INC  RR0
	DBNZ B, instr_lsr_imm_is_0_byt
	LD   RR1
	ADD  #3
	ST   RR1				;point to top byte of source
	LD   @R1				;get top byte
	CLR1 ARM_SR, SR_BIT_C
	BN   ACC, CY, instr_lsr_imm_is_0_no_c
	SET1 ARM_SR, SR_BIT_C
instr_lsr_imm_is_0_no_c:
	CLR1 ARM_SR, SR_BIT_N
	SET1 ARM_SR, SR_BIT_Z
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ASR(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_asr_imm:
	CALLF get_regno3_0
	ST    RR0				;Rd pointer in R0
	CALLF get_regno3_3
	ST    RR1				;Rm pointer in R1
	CALLF get_immed_5
	BZ    instr_asr_imm_is_0
	
	;we have a nonzero amount of shifting to do - do it
	ST    B
	CALLF cpy_word_R1_to_R0	;Rd = Rm (we no longer need Rm pointer)

instr_asr_imm_shift_once:
	;put top bit into CY and predecrementR0
	DEC   RR0
	;that continues here
instr_asr_common:			;call with shift amount in B, R0 pointing TO last byte of dst for shifts 1..31 times
	LD    @R0
	ROLC
	MOV   #4, C
	
instr_asr_imm_shift_once_byt:
	LD    @R0
	RORC
	ST    @R0
	DEC   RR0
	DBNZ  C, instr_asr_imm_shift_once_byt
	DBNZ  B, instr_asr_imm_shift_more
	JMPF  instr_asr_imm_shift_done	;to make sure carry survives
instr_asr_imm_shift_more:
	LD    RR0
	ADD   #5
	ST    RR0
	JMPF  instr_asr_imm_shift_once
instr_asr_imm_shift_done:
	CALLF copy_C_bit
	INC   RR0				;re-point it to the actual data maybe?
	JMPF  set_NZ_R0_no_rewind_and_goto_next_instr

instr_asr_imm_is_0:			;special easy case <<ASR #32>> - slightly easier than usual
	CALLF inc_R1_3_times
	LD    @R1

instr_asr_32_or_more_common:	;called with ACC containing top byte of source, R0 = dst
	BN    ACC, 7, instr_asr_32_or_more_val_0x00
	CLR1  ARM_SR, SR_BIT_Z
	SET1  ARM_SR, SR_BIT_N
	SET1  ARM_SR, SR_BIT_C
	OR    #$FF
	JMPF  instr_asr_32_or_more_val_calced
instr_asr_32_or_more_val_0x00:
	SET1  ARM_SR, SR_BIT_Z
	CLR1  ARM_SR, SR_BIT_N
	CLR1  ARM_SR, SR_BIT_C
	AND   #$00
instr_asr_32_or_more_val_calced:
	MOV   #4, B
instr_asr_32_or_more_byt:
	ST    @R0
	INC   RR0
	DBNZ  B, instr_asr_32_or_more_byt
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ADD(3)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_reg:
	CALLF add_3_sub_3_common_intro	;&Rn -> R0, &Rm -> R1, &Rd -> R3 (not deref)

instr_add_common:			;expects pointer to Rn in R0, Rm in R1, Rd in R3
	CLR1  PSW, CY
	
instr_add_common_wth_carry:	;expects pointer to Rn in R0, Rm in R1, Rd in R3, CY set
	MOV   #WORD_IN_SFRS, RR2	;B,C,TRL,TRH space to use as temporary space
	MOV   #4, TMPBYTE0		;used for counter as B is currently occupied
instr_add_reg_loop:
	LD    @R0
	INC   RR0
	ADDC  @R1
	INC   RR1
	ST    @R2
	INC   RR2
	DBNZ  TMPBYTE0, instr_add_reg_loop
	CALLF copy_C_bit
	CALLF calc_V_bit_addition

instr_add_sub_common_store_result:
	LD    RR3
	ST    RR0				;Rd pointer in R0
	DEC   RR2
	DEC   RR2
	DEC   RR2
	CALLF cpy_word_R2_to_R0
	JMPF  set_NZ_R0_with_rewind_and_goto_next_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 SUB(3)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_sub_reg:
	CALLF add_3_sub_3_common_intro	;&Rn -> R0, &Rm -> R1, &Rd -> R3 (not deref)
	
instr_sub_common:			;expects pointer to Rn in R0, Rm in R1, Rd in R3
	CLR1  PSW, CY
	
instr_sub_common_wth_carry:	;expects pointer to Rn in R0, Rm in R1, Rd in R3, CY set
	MOV   #WORD_IN_SFRS, RR2	;B,C,TRL,TRH space to use as temporary space
	CALLF instr_sub_R1_from_R0_to_R2
	CALLF copy_C_bit_inverted
	CALLF calc_V_bit_subtraction
	JMPF  instr_add_sub_common_store_result

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ADD(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_imm3:
	CALLF get_regno3_3
	ST    RR0					;Rn pointer in R0
	CALLF get_regno3_0
	ST    RR3					;Rd pointer in R3
	CALLF get_immed_3
	CALLF convert_acc_to_reg_R1	;Rm (fake) pointer in R1
	JMPF  instr_add_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 SUB(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_sub_imm3:
	CALLF get_regno3_3
	ST    RR0					;Rn pointer in R0
	CALLF get_regno3_0
	ST    RR3					;Rd pointer in R3
	CALLF get_immed_3
	CALLF convert_acc_to_reg_R1	;Rm (fake) pointer in R1
	JMPF  instr_sub_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 MOV(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mov_imm:
	CALLF get_regno3_8
	ST    RR0				;Rd pointer in R0
	CLR1  ARM_SR, SR_BIT_N
	CLR1  ARM_SR, SR_BIT_Z
	LD    WORD_VAL + 0		;get the byte we're storing
	BNZ   instr_mov_imm_z
	SET1  ARM_SR, SR_BIT_Z
instr_mov_imm_z:
	ST    @R0				;save the byte into register
	AND   #$0
	MOV   #3, B
instr_mov_loop_z:
	INC   RR0
	ST    @R0
	DBNZ  B, instr_mov_loop_z
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 CMP(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cmp_imm:
	CALLF get_regno3_8
	ST    RR0					;Rn pointer in R0
	LD    WORD_VAL + 0			;get the byte we're storing
	CALLF convert_acc_to_reg_R1	;Rm (fake) pointer in R1
	MOV   #WORD_VAL, RR3			;give it a fake place to write result (smaller than trying to not write it)
	JMPF  instr_sub_common		;note our Rm and Rd overlap (WORD_VAL) this is OK

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ADD(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_imm:
	CALLF get_regno3_8
	ST    RR0					;Rn pointer in R0
	ST    RR3					;Rd is also there (not dereferencable as always for R3)
	LD    WORD_VAL + 0			;get the byte we're storing
	CALLF convert_acc_to_reg_R1	;Rm (fake) pointer in R1
	JMPF  instr_add_common
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 SUB(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_sub_imm:
	CALLF get_regno3_8
	ST    RR0					;Rn pointer in R0
	ST    RR3					;Rd is also there (not dereferencable as always for R3)
	LD    WORD_VAL + 0			;get the byte we're storing
	CALLF convert_acc_to_reg_R1	;Rm (fake) pointer in R1
	JMPF  instr_sub_common
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//               AND EOR LSL LSR ASR ADC SBC ROR (register)               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_dp_part0:
	CALLF dp_instrs_common		;&Rn/&Rd -> R0, &Rm/&Rs-> R1
	CALLF dispatch_bits_6_to_8	;8-entry jumptable follows
	JMP   instr_and
	JMP   instr_eor
	JMP   instr_lsl_reg
	JMP   instr_lsr_reg
	JMP   instr_asr_reg
	JMP   instr_adc
	JMP   instr_sbc
	JMP   instr_ror

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//               TST NEG CMP CMN ORR MUL BIC MVN (register)               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_dp_part1:
	CALLF dp_instrs_common		;&Rn/&Rd -> R0, &Rm/&Rs-> R1
	CALLF dispatch_bits_6_to_8	;8-entry jumptable follows
	JMP   instr_tst
	JMP   instr_neg
	JMP   instr_cmp_reg
	JMP   instr_cmn
	JMP   instr_orr
	JMP   instr_mul
	JMP   instr_bic
	JMP   instr_mvn
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  AND                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_and:	;R0 points to Rd/Rn, R1 to Rm
	MOV  #4, B
	MOV  #0, C
instr_and_loop:
	LD   @R0
	AND  @R1
	ST   @R0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_and_loop
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  EOR                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_eor:	;R0 points to Rd/Rn, R1 to Rm
	MOV  #4, B
	MOV  #0, C
instr_eor_loop:
	LD   @R0
	XOR  @R1
	ST   @R0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_eor_loop
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  ORR                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_orr:	;R0 points to Rd/Rn, R1 to Rm
	MOV  #4, B
	MOV  #0, C
instr_orr_loop:
	LD   @R0
	OR   @R1
	ST   @R0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_orr_loop
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  BIC                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bic:	;R0 points to Rd/Rn, R1 to Rm
	MOV  #4, B
	MOV  #0, C
instr_bic_loop:
	LD   @R1
	XOR  #$FF
	AND  @R0
	ST   @R0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_bic_loop
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  MVN                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mvn:
	MOV  #4, B
	MOV  #0, C
instr_mvn_loop:
	LD   @R1
	XOR  #$FF
	ST   @R0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_mvn_loop
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  TST                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_tst:	;R0 points to Rd/Rn, R1 to Rm
	MOV  #4, B
	MOV  #0, C
instr_tst_loop:
	LD   @R0
	AND  @R1
	ST   TMPBYTE0
	OR   C
	ST   C
	INC  RR0
	INC  RR1
	DBNZ B, instr_tst_loop
	MOV  #TMPBYTE0 + 1, RR0	;instr_logical_op_set_NZ expects pointer one past last byte produces in RR0 - give it to it
	JMPF instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  MUL                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mul:	;R0 points to Rd/Rn, R1 to Rm (at this point WORDVAL is safe to overwrite)
	
	LD    RR0
	ST    RR2
	
								;for fast path both top words must be zero
	PUSH  RR0
	INC   RR0
	INC   RR0
	LD    @R0
	INC   RR0
	OR    @R0
	POP   RR0
	BNZ   instr_mul_slowpath
	
	PUSH  RR1
	INC   RR1
	INC   RR1
	LD    @R1
	INC   RR1
	OR    @R1
	POP   RR1
	BNZ   instr_mul_slowpath
	
	INC   RR0
	LD    @R0
	DEC   RR0
	BZ    instr_mul_fastpath_Rn_small
	
	INC   RR1
	LD    @R1
	DEC   RR1
	BZ    instr_mul_fastpath_Rm_small

instr_mul_medium_path:			;medium path - both words are only 16 bits long

	LD    @R1
	ST    B
	LD    @R0
	ST    C
	INC   RR0
	LD    @R0
	MUL
	ST    WORD_VAL + 1
	LD    C
	ST    WORD_VAL + 0
	LD    B
	ST    WORD_VAL + 2
	
	INC   RR1
	DEC   RR0
	LD    @R1
	ST    B
	LD    @R0
	ST    C
	INC   RR0
	LD    @R0
	MUL
	PUSH  ACC
	LD    C
	ADD   WORD_VAL + 1
	ST    WORD_VAL + 1
	POP   ACC
	ADDC  WORD_VAL + 2
	ST    WORD_VAL + 2
	LD    B
	ADDC  #0
	ST    WORD_VAL + 3
	JMPF  instr_mul_done

instr_mul_fastpath_Rn_small:	;R0 points to small val
	LD    RR0
	XCH   RR1
	ST    RR0
	;fallthrough

instr_mul_fastpath_Rm_small:	;R1 points to small val

	LD    @R1
	ST    B
	LD    @R0
	ST    C
	INC   RR0
	LD    @R0
	MUL
	ST    WORD_VAL + 1
	LD    C
	ST    WORD_VAL + 0
	LD    B
	ST    WORD_VAL + 2
	MOV   #0, WORD_VAL + 3
	JMPF  instr_mul_done
	
instr_mul_slowpath:
	AND   #$0
	ST    TMPBYTE2
	CALLF convert_acc_to_reg	;clears WORD_VAL
	
instr_mul_outer:
	MOV   #0, TMPBYTE1

instr_mul_inner:
	;verify this operation has a purpose (we need to produce 32 bits of result - do not bother producing more)
	LD    TMPBYTE2
	ADD   TMPBYTE1
	ST    RR3
	BP    ACC, 2, instr_mul_inner_done
	
	;get pointer to R0's data bytes we need
	LD    RR2
	ADD   TMPBYTE1
	ST    RR0
	
	;get data and mul
	LD    @R1
	BZ    instr_mul_inner_done	;shortcut on zero
	ST    B
	LD    @R0
	ST    C
	INC   RR0					;we could shortcut here too, but chances of two bytes being zero are 256 times lower so we dont want to pay the cycle cost to do it
	LD    @R0

	MUL
	
	;save ACC
	ST    TMPBYTE0
	
	;get destiantion pointer into R0
	LD    RR3
	ADD   #WORD_VAL
	ST    RR0
	
	;first byte guaranteed to be needed (we checked at start of iteration)
	LD    @R0
	ADD   C
	ST    @R0
	INC   RR3
	BP    RR3, 2, instr_mul_inner_done	;jump if result 4 (no more carrying to do)
	
	;second byte time
	INC   RR0
	LD    @R0
	ADDC  TMPBYTE0
	ST    @R0
	INC   RR3
	BP    RR3, 2, instr_mul_inner_done	;jump if result 4 (no more carrying to do)
	
	;third byte
	INC   RR0
	LD    @R0
	ADDC  B
	ST    @R0
	INC   RR3
	BP    RR3, 2, instr_mul_inner_done	;jump if result 4 (no more carrying to do)
	
	;last byte (carry-through)
	INC   RR0
	LD    @R0
	ADDC  #0
	ST    @R0
	
instr_mul_inner_done:

	INC   TMPBYTE1
	INC   TMPBYTE1
	BN    TMPBYTE1, 2, instr_mul_inner	;jump if result not 4

	INC   RR1
	INC   TMPBYTE2
	BN    TMPBYTE2, 2, instr_mul_outer	;jump if result not 4
	
instr_mul_done:
;calculation is done, result is in WORD_VAL
	LD    RR2
	ST    RR0
	MOV   #WORD_VAL, RR1
	CALLF cpy_word_R1_to_R0
	LD    RR2
	ST    RR0
	CALLF orr_all_bytes_of_R0		;instr_logical_op_set_NZ needs C
	ST    C
	INC   RR0
	JMPF  instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LSL(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_lsl_reg:	;R0 points to Rd, R1 to Rs
	LD    @R1	;get shift amount
	BE    #32, instr_lsl_reg_32
	BP    PSW, CY, instr_lsl_under_32	;less than 32 - normal lsl

instr_lsl_over_32:
	CLR1  ARM_SR, SR_BIT_C

instr_lsl_32_or_more:
	AND   #$0
	MOV   #4, B
	
instr_lsl_32_or_more_loop:
	ST    @R0
	INC   RR0
	DBNZ  B, instr_lsl_32_or_more_loop

instr_lsl_lsr_32_or_more_set_NZ:
	SET1  ARM_SR, SR_BIT_Z
	CLR1  ARM_SR, SR_BIT_N
	JMPF  fetch_instr
	
instr_lsl_under_32:
	CALLF inc_R0_4_times
	JMPF  inst_lsl_common

instr_lsl_reg_32:
	LD    @R0
	RORC		;Rd[0] into carry
	CALLF copy_C_bit
	JMPF  instr_lsl_32_or_more

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LSR(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_lsr_reg:
	CALLF inc_R0_3_times
	LD    @R1	;get shift amount
	BE    #32, instr_lsr_reg_32
	BP    PSW, CY, instr_lsr_under_32	;less than 32 - normal lsl

instr_lsr_over_32:
	CLR1  ARM_SR, SR_BIT_C
	JMPF  instr_lsr_32_or_more

instr_lsr_reg_32:
	LD    @R0
	ROLC		;Rd[31] into carry
	CALLF copy_C_bit
	
instr_lsr_32_or_more:
	AND   #$0
	MOV   #4, B
	
instr_lsr_32_or_more_loop:
	ST    @R0
	DEC   RR0
	DBNZ  B, instr_lsr_32_or_more_loop
	JMPF  instr_lsl_lsr_32_or_more_set_NZ

instr_lsr_under_32:
	INC   RR0
	BZ    instr_lsr_reg_0
	ST    B
	JMPF  inst_lsr_common

instr_asr_ror_set_nz_0:		;set NZ based on an modified Rd (expects R0 to point TO highest byte of Rd)
	INC   RR0
instr_lsr_reg_0:

	LD    RR0
	SUB   #4
	ST    RR0
	CALLF orr_all_bytes_of_R0		;instr_logical_op_set_NZ needs C
	ST    C
	INC   RR0
	JMPF  instr_logical_op_set_NZ

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                ASR(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_asr_reg:		;&Rn/&Rd -> R0, &Rm/&Rs-> R1
	LD	  RR0
	ST    C
	CALLF inc_R0_3_times
	LD    @R1	;get shift amount
	BZ    instr_asr_ror_set_nz_0
	ST    B
	;anything over 32 should be treated as 32
	SUB   #32
	BN    PSW, CY, instr_asr_32_or_more

instr_asr_reg_1_to_31:
	JMPF  instr_asr_common

instr_asr_32_or_more:	;@R0->ACC, C->R0
	PUSH  C
	LD    @R0
	POP   RR0
	JMPF  instr_asr_32_or_more_common
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  ROR                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ror:
	LD	  RR0
	ST    C
	CALLF inc_R0_3_times
	LD    @R1	;get shift amount
	BZ    instr_asr_ror_set_nz_0
	AND   #$0F
	BZ    instr_ror_0
	
instr_ror_nonzero:
	ST    B
	LD    C
	ST    RR1
	LD    RR0
	ST    RR2
	;now R0==R2==&Rd[24..31], R1==&Rd[0..7], B = rotatecount

instr_ror_loop:
	LD    @R1
	RORC		;populate CARRY wih bottom bit
	MOV   #4, C	;inner loop

instr_ror_inner:
	LD    @R0
	RORC
	ST    @R0
	DEC   RR0
	DBNZ  C, instr_ror_inner
	
	LD    RR2
	ST    RR0
	DBNZ  B, instr_ror_loop
	
	;we're done, C still has the carry out bit, R0 points to last byte
	JMPF  instr_ror_save_c_bit_and_set_nz

instr_ror_0:
	LD    @R0
	ROLC 

instr_ror_save_c_bit_and_set_nz:
	CALLF copy_C_bit
	JMPF  instr_asr_ror_set_nz_0

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  NEG                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_neg:			;have: R0=&dst, R1=&src, want: R3=&dst, R0=&0, R1=&src
	LD    RR0
	ST    RR3
	AND   #$0
	CALLF convert_acc_to_reg_R0	;fake pointer in R0 to value 0
	JMPF  instr_sub_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                CMP(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cmp_reg:
	MOV  #WORD_VAL, RR3			;give it a fake place to write result (smaller than trying to not write it)
	JMPF instr_sub_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  CMN                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cmn:
	MOV  #WORD_VAL, RR3			;give it a fake place to write result (smaller than trying to not write it)
	JMPF instr_add_common
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  ADC                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_adc:
	LD   RR0
	ST   RR3					;Rd = Rn
	LD   ARM_SR
	ROLC 						;copy carry bit into CY
	JMPF instr_add_common_wth_carry
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  SBC                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_sbc:
	LD   RR0
	ST   RR3					;Rd = Rn
	LD   ARM_SR
	ROLC 						;copy carry bit into CY
	NOT1 PSW, CY				;SUB expects NOT(C) so use that
	JMPF instr_sub_common_wth_carry

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                             ADD(4)  CMP(3)                             //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_cmp_hi:
	BP   WORD_VAL + 1, 0, instr_cmp_hi
	;fallthrough

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                ADD(4)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_high:
	MOV   #0, TMPBYTE0
	CALLF get_regno4_0
	ST    RR0
	ST    RR2
	BNE   #REGS_PC, instr_add_high_Rd_not_pc
	SET1  TMPBYTE0, 1
instr_add_high_Rd_not_pc:
	CALLF get_regno4_3
	ST    RR1
	BNE   #REGS_PC, instr_add_high_Rm_not_pc
	SET1  TMPBYTE0, 1			;if both are PC, we'll only add 2 and not 4. this is ok as v8-M spec says this is undefined anyways
instr_add_high_Rm_not_pc:

	CALLF instr_add_R1_to_R0
	
	BN    TMPBYTE0, 1, instr_add_high_no_pc_adj
;we're here if we need to adjust for PC access
	LD    RR2
	ST    RR0
	MOV   #2, ACC
	CALLF add_R0_8bit
	
instr_add_high_no_pc_adj:
	JMPF  fetch_instr
	

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                CMP(3)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cmp_hi:	;PC not supported for either reg in v8-M
	CALLF get_regno4_0
	ST    RR0
	CALLF get_regno4_3
	ST    RR1
	MOV   #WORD_VAL, RR3			;give it a fake place to write result (smaller than trying to not write it)
	JMPF  instr_sub_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                            MOV(3) BX BLX(2)                            //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mov_hi_bx_blx:
	BP   WORD_VAL + 1, 0, instr_bx_blx_reg
	;fallthrough

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                MOV(3)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mov_high:
	CALLF get_regno4_0
	ST    RR0
	CALLF get_regno4_3
	ST    RR1
	BE    #REGS_PC, instr_mov_high_reads_pc

;not pc read
	CALLF cpy_word_R1_to_R0
	JMPF  fetch_instr_maybe_pc_changed	;may have written PC

instr_mov_high_reads_pc:		;@R0 = @R1 + 2
	MOV   #3, B
	CLR1  PSW, CY
	LD    @R1
	ADD   #2
	ST    @R0
instr_mov_high_pc_rd_loop:
	INC   RR0
	INC   RR1
	LD    @R0
	ADDC  @R1
	ST    @R0
	DBNZ  B, instr_mov_high_pc_rd_loop
	JMPF  fetch_instr_maybe_pc_changed	;may have written PC


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                               BX  BLX(2)                               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bx_blx_reg:
	;if BLX, we must save PC in a temp space (we cannot write LR since BLX LR is a valid instr)
	;first save BLX flag somewhere (VRMAD2[0]) safe so we can overwrite WORD_VAL
	LD    WORD_VAL + 0
	ROL
	ST    VRMAD2
	
	;then get reg num we'll be using and save it
	CALLF get_regno4_3
	PUSH  ACC
	
	;if  BLX, save LR to temp space
	BN    VRMAD2, 0, instr_bx_blx_reg_not_blx
	MOV   #REGS_PC, RR1	
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0
	SET1  WORD_VAL, 0			;make sure we save LR with low bit set
instr_bx_blx_reg_not_blx:
	
	;move provided reg to PC, clear bottom bit
	MOV   #REGS_PC, RR0
	POP   RR1					;user-provided reg
	CALLF cpy_word_R1_to_R0
	CLR1  REGS_PC, 0			;make sure we set PC with low bit clear
	
	;if BLX, move saved data to LR
	BN    VRMAD2, 0, instr_bx_blx_reg_not_blx2	
	MOV   #WORD_VAL, RR1
	MOV   #REGS_LR, RR0
	CALLF cpy_word_R1_to_R0

instr_bx_blx_reg_not_blx2:
	JMPF  fetch_instr_maybe_pc_changed	;definitely wrote PC

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ADD(5)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_pc_imm:

	;we must return "(PC_read & 0xFFFFFFFC) + (imm * 4)"
	;since "PC_read" is "PC_cur + 2":
	;we must return "((PC_cur + 2) & 0xFFFFFFFC) + (imm * 4)"
	;since (imm * 4) always has 2 low bits as "00", the low bits of "(imm * 4) + X" are always the low 2 bits of "X"
	;and thus "(imm * 4) + (X &~ 3)" is the same as "((imm * 4) + X) &~ 3"
	;in our case that means that "((PC_cur + 2) & 0xFFFFFFFC) + (imm * 4)" becomes
	;"((PC_cur + 2) + (imm * 4)) & 0xFFFFFFFC", which in turn becomes
	;"(PC_cur +  ( (imm * 4) + 2) ) & 0xFFFFFFFC". This matters because
	;"(X * 4) + 2" is the same as "(X * 4) | 2" since "X * 4" has low 2 bits of "00"
	;and thus our final result can be: "(PC_cur +  ( (imm * 4) | 2) ) & 0xFFFFFFFC"
	;and this helps us avoid a few annoying 32-bit additions
	CALLF get_regno3_8
	ST    RR0							;R0 = &dst
	PUSH  RR0							;push(&dst)
	
	
	CALLF get_imm8_mul_4_into_B_C_TR		;imm * 4 -> <B,C,TRL,TRH>
	SET1  B, 1							;<B,C,TRL,TRH> |= 2
	MOV   #REGS_PC, RR1					;R1 = &PC
	CALLF instr_add_B_C_TR_and_R1_to_R0	;dst = PC + 2 + imm * 4

	POP   RR0							;pop(&dst)
	LD    @R0
	AND   #$FC							;as needed, align result to 32-bits
	ST    @R0
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 ADD(6)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_add_sp_imm:
	CALLF get_regno3_8
	ST    RR0					;R0 = &dst
	
	MOV   #REGS_SP, RR1			;R1 = &SP
	
	CALLF get_imm8_mul_4_into_B_C_TR
	CALLF instr_add_B_C_TR_and_R1_to_R0
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                          ADD(7) SUB(4) CBZ                             //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_adj_sp_cbz:
	BP   WORD_VAL + 1, 0, instr_cbz
	;fallthrough
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                             ADD(7)  SUB(4)                             //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
	LD    WORD_VAL + 0
	ST    B
	ROLC
	ROLC
	AND   #$FC
	ST    WORD_VAL + 0
	AND   #$0
	ST    WORD_VAL + 2
	ST    WORD_VAL + 3
	ADDC  #0
	ST    WORD_VAL + 1
	MOV   #WORD_VAL, RR1
	MOV   #REGS_SP, RR0
	BP    B, 7, instr_sub4
	CALLF instr_add_R1_to_R0
	JMPF  fetch_instr
instr_sub4:
	CALLF instr_sub_R1_from_R0
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                      SXTH  SXTB  UXTH  UXTB  CBZ                       //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_extend_cbz:
	BP   WORD_VAL + 1, 0, instr_cbz
	;fallthrough
	

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                          SXTH SXTB UXTH UXTB                           //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_extends:
	CALLF get_regno3_0
	ST    RR0			;R0 = &Rd
	CALLF get_regno3_3
	ST    RR1			;R1 = &Rm
	MOV   #1, C
	MOV   #3, B
	BP    WORD_VAL + 0, 6, instr_extend_sz_is_byte
	INC   C
	DEC   B
instr_extend_sz_is_byte:
	;C is now the number of bytes we want to copy and B bytes we must extend to
	
instr_extend_copy_bytes:
	LD    @R1
	ST    @R0
	INC   RR0
	INC   RR1
	DBNZ  C, instr_extend_copy_bytes
	
	NOT1  WORD_VAL + 0, 7	;we want top bit set for SXT but encoding has it clear - fix
	AND   WORD_VAL + 0		;top bit now set IFF we have an SXT and top bit of data we copied was one (needs extend)

instr_extend_exported:		;exported for others' use. ACC top bit is extended to R0... for B bytes
	BP    ACC, 7, instr_extend_with_0xFF
instr_extend_with_0x00:
	AND   #$0
	JMPF  instr_extend_finish_loop
instr_extend_with_0xFF:
	OR    #$FF
instr_extend_finish_loop:
	ST    @R0
	INC   RR0
	DBNZ  B, instr_extend_finish_loop
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                   CBZ                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cbz:
	CALLF get_regno3_0
	ST    RR0			;R0 = &Rd
	CALLF orr_all_bytes_of_R0
	BZ    instr_cbz_cbnz_jump_execute
	JMPF  fetch_instr

instr_cbz_cbnz_jump_execute:		;common to CBZ/CBNZ - execute the jump
	LD    WORD_VAL + 0
	ROR
	ROR
	AND   #$3E
	BN    WORD_VAL + 1, 1, instr_cbz_cbnz_jump_execute_no_hi_bit
	OR    #$40
instr_cbz_cbnz_jump_execute_no_hi_bit:
	ADD   #2						;because PC...
	MOV   #REGS_PC, RR0
	CALLF add_R0_8bit
	JMPF  fetch_instr_maybe_pc_changed	; PC may have changed
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  CBNZ                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cbnz:
	CALLF get_regno3_0
	ST    RR0			;R0 = &Rd
	CALLF orr_all_bytes_of_R0
	BNZ   instr_cbz_cbnz_jump_execute
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                          REV REV16 REVSH CBNZ                          //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cbnz_rev:
	BP    WORD_VAL + 1, 0, instr_cbnz
	;fallthrough
	
	CALLF get_regno3_0
	ST    RR0			;R0 = &Rd
	CALLF get_regno3_3	;ACC = &Rm

	BP    WORD_VAL + 0, 7, instr_revsh
	BP    WORD_VAL + 0, 6, instr_rev16
	;fallthrough

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                   REV                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_rev:	;R0 = &Rd, ACC = &Rm

	PUSH  RR0					;we must make a copy, else things like "REV R0, R0" corrupt reg as we both read and write it
	ST    RR1
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0
	POP   RR0
	MOV   #WORD_VAL + 3, RR1
	MOV   #4, B

instr_rev_loop:
	LD    @R1
	ST    @R0
	INC   RR0
	DEC   RR1
	DBNZ  B, instr_rev_loop
	JMPF  fetch_instr


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  REV16                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_rev16:	;R0 = &Rd, ACC = &Rm

	PUSH  RR0					;we must make a copy, else things like "REV R0, R0" corrupt reg as we both read and write it
	ST    RR1
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0
	POP   RR0
	MOV   #WORD_VAL + 1, RR1
	MOV   #2, C

instr_rev16_loop_outer:
	MOV   #2, B
instr_rev16_loop:
	LD    @R1
	ST    @R0
	INC   RR0
	DEC   RR1
	DBNZ  B, instr_rev16_loop
	LD    RR1
	ADD   #4
	ST    RR1
	DBNZ  C, instr_rev16_loop_outer
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  REVSH                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_revsh:	;R0 = &Rd, ACC = &Rm
	PUSH  RR0					;we must make a copy, else things like "REV R0, R0" corrupt reg as we both read and write it
	ST    RR1
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0
	POP   RR0
	MOV   #WORD_VAL + 1, RR1
	MOV   #2, B
instr_revsh_loop:
	LD    @R1
	ST    @R0
	INC   RR0
	DEC   RR1
	DBNZ  B, instr_revsh_loop
	
	MOV   #2, B
	JMPF instr_extend_exported
	

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                               SDIV  UDIV                               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
;we mutate denom, but since we return it back to normal, this is ok
;we are not allowed to mutate num so we copy it
;we cannot mutate dst till the very end because it might overlap with one of the others
;we need 4 WORDS to make div work: MASK, RES, NUM, DENOM
;denom stays in place, so we need space for the other 3, we also need temp space for loops, etc
;we'll also use B,C,TRL,TRH in SFR space for temp storage, and VRMAD1 and VRMAD2 and XBNK as well
;We only implement unsigned div, we flip signs as needed to do signed div.
;This does indeed work on 0x80000000 due to how we do it.



;thisis NOT the entry point for SDIV/UDIV. that one is below. thisis here for relative jump length limits
;long mul dispatch:
inst_was_long_multiply:
	JMPF  inst_lmul

DIV_pDENOM				EQU RR3
DIV_MASK				EQU $4
DIV_RET					EQU $8
DIV_NUM					EQU $C
DIV_MUST_NEGATE_RET		EQU VRMAD2			;one bit only
DIV_DENOM_WAS_INVERTED	EQU XBNK			;one bit only
DIV_WORD_IN_SFRS		EQU WORD_IN_SFRS	;actually $102 = <B,C,TRL,TRH>

instr_sdiv_udiv_lmul:
	BN   WORD_VAL + 0, 4, inst_was_long_multiply	;disambiguate long multiplies from divides
	
	PUSH  CODE_START_MI
	PUSH  CODE_START_HI
	PUSH  WORD_VAL + 0				;preserves it so no matter where our vals are, we'll not clobber it

	LD    WORD_VAL + 0
	CALLF get_regno4_0_32bit
	PUSH  ACC						;push(&num)
	MOV   #DIV_RET, C				;where to store the result of reading instr
	CALLF read_instr_16b
	LD    DIV_RET + 0
	CALLF get_regno4_0_32bit
	ST    DIV_pDENOM
	LD    DIV_RET + 1
	CALLF get_regno4_0_32bit
	ST    B							;B = &quotient
	
	;now copy NUM (instr word still not overwritten, so we're ok to check SDIV/UDIV later)
	MOV   #DIV_NUM, RR0
	POP   RR1						;pop() -> &num
	CALLF cpy_word_R1_to_R0
	
	;now check instr type and negate words if needed
	CLR1  DIV_MUST_NEGATE_RET, 0
	CLR1  DIV_DENOM_WAS_INVERTED, 0
	POP   ACC						;formely low byte of first 16 bits of instr
	PUSH  B							;push(B aka &quotient)
	BP    ACC, 5, instr_do_udiv
instr_was_sdiv:
	
	LD    DIV_pDENOM
	ST    RR0
	CALLF neg_if_neg
	ST    DIV_MUST_NEGATE_RET
	ST    DIV_DENOM_WAS_INVERTED
	
	MOV   #DIV_NUM, RR0
	CALLF neg_if_neg
	XOR   DIV_MUST_NEGATE_RET
	ST    DIV_MUST_NEGATE_RET
	
instr_do_udiv:						;see if we can do it quickly using hw?
	LD    DIV_NUM + 2
	OR    DIV_NUM + 3
	BNZ   instr_do_udiv_slow_path
	LD    DIV_pDENOM
	ADD   #3
	ST    RR0
	LD    @R0
	DEC   RR0
	OR    @R0
	DEC   RR0
	OR    @R0
	BNZ   instr_do_udiv_slow_path

instr_do_udiv_fast:
	DEC   RR0
	LD    @R0
	BZ    instr_udiv_divided_by_zero
	ST    B
	LD    DIV_NUM + 0
	ST    C
	LD    DIV_NUM + 1
	DIV
	ST    DIV_RET + 1
	LD    C
	ST    DIV_RET + 0
	AND   #$00
	ST    DIV_RET + 2
	ST    DIV_RET + 3
	;if we wanted to provide remainder in NUM, we could actually
	JMPF  div_result_ready

instr_udiv_divided_by_zero:
	AND   #$00
	ST    DIV_RET + 0
	ST    DIV_RET + 1
	ST    DIV_RET + 2
	ST    DIV_RET + 3
	JMPF  div_result_ready

;step 0: prepare result (aka DIV_RET)=0, mask (aka DIV_MASK) = 1
instr_do_udiv_slow_path:			;called when hardware divide is not possible
	MOV   #1, DIV_MASK + 0
	AND   #$00
	ST    DIV_MASK + 1
	ST    DIV_MASK + 2
	ST    DIV_MASK + 3
	ST    DIV_RET + 0
	ST    DIV_RET + 1
	ST    DIV_RET + 2
	ST    DIV_RET + 3

;step 1: shift denom and mask as far left as needed to get top bit set in denom. we also carefully watch for CARRY in shifting mask (meaning divide by zero)
div_normalize_denom_loop:
	LD    DIV_pDENOM
	ADD   #3
	ST    RR0
	LD    @R0
	BP    ACC, 7, div_divide_iteration
	
	MOV   #DIV_MASK, RR0
	CALLF lsl_R0
	BP    PSW, CY, div_by_zero	;if we shifted so far that mask has rolled over, denom was zero	(will not happen if fast path is used - keeping this here because it is only 2 cy and someone later might rip out fast path when they port this to 6502 which has no HW DIV)
	LD    DIV_pDENOM
	ST    RR0
	CALLF lsl_R0
	JMPF  div_normalize_denom_loop

;step 2: iteratively divide (and keep an eye out on mask carry-out meaning we're done)
div_divide_iteration:
	MOV   #DIV_NUM, RR0
	LD    DIV_pDENOM
	ST    RR1
	MOV   #DIV_WORD_IN_SFRS, RR2
	CALLF instr_sub_R1_from_R0_to_R2
	BP    PSW, CY, div_divide_iteration_done	;carry means DENOM > NUM, meaning we do not get to set bit in result or record result of this subtraction
	
;step 2.1: if subtraction didnt borrow, store its result and ORR mask into div's result
	
	;store subtraction result
	MOV   #DIV_WORD_IN_SFRS, RR2
	MOV   #DIV_NUM, RR0
	CALLF cpy_word_R2_to_R0
	
	;ORR mask into div result
	MOV   #DIV_RET, RR0
	MOV   #DIV_MASK, RR1
	MOV   #4, C
	
div_orr_result_loop:
	LD    @R0
	OR    @R1
	ST    @R0
	INC   RR0
	INC   RR1
	DBNZ  C, div_orr_result_loop

;step 2.2: LSR mask and denom. quit if mask now zero (aka: carry out !=0 after shift)
div_divide_iteration_done:	;expects proper R0 pushed

	MOV   #DIV_MASK, RR0
	CALLF lsr_R0
	BP    PSW, CY, div_result_ready
	LD    DIV_pDENOM
	ST    RR0
	CALLF lsr_R0
	JMPF  div_divide_iteration

div_result_ready: ;unsigned division done. MASK is 0, RET is result, NUM is remainder (IFF we did not go fast path, though we could do that there too)

;step 3.1: if denom was negated, un-negate it
	BN    DIV_DENOM_WAS_INVERTED, 0, div_denom_wasnt_negated
	LD    DIV_pDENOM
	ST    RR0
	CALLF neg_R0
div_denom_wasnt_negated:

;step 3.2: if result needs negation, do it
	BN    DIV_MUST_NEGATE_RET, 0, div_result_negated_if_needed
	MOV   #DIV_RET, RR0
	CALLF neg_R0
div_result_negated_if_needed:

;step 3.3: store result into destination
	MOV   #DIV_RET, RR1

div_result_ptr_in_R1:			;div by zero points R1 to a zero-valued word and then jumps here
	POP   RR0					;pops quotient pointer
	CALLF cpy_word_R1_to_R0
	
	POP   CODE_START_HI
	POP   CODE_START_MI
	JMPF  fetch_instr

;special: divide by zero (we arrive here with mask == 0, we use this)
div_by_zero:
	MOV   #DIV_MASK, RR1
	JMPF  div_result_ptr_in_R1

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  B(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_b:
	;first we must sign-extend the offset (and prepare in B the bytes to sign-extend to full word)
	MOV  #0, B
	LD   WORD_VAL + 1
	AND  #$07
	BN   ACC, 2, instr_b_not_neg
	OR   #$F8
	MOV  #$FF, B
instr_b_not_neg:
	ST   WORD_VAL + 1
	
instr_b_do_execute_branch:	;assumes 16-bit signed offset in WORD_VAL, sign extension byte in B
	;then we add 1 to it (before shifting) to account for us needing a plus 2 on PC read
	INC  WORD_VAL + 0
	LD   WORD_VAL + 0
	BNZ  instr_b_add_not_ovf
	INC  WORD_VAL + 1
instr_b_add_not_ovf:
	
	;then we shift it left
	CLR1 PSW, CY
	LD   WORD_VAL + 0
	ROLC
	ST   WORD_VAL + 0
	LD   WORD_VAL + 1
	ROLC 
	ST   WORD_VAL + 1

	;and then add to PC
	MOV  #REGS_PC, RR0
	MOV  #WORD_VAL, RR1
	MOV  #2, C
	CLR1 PSW, CY
instr_b_add_first_half_loop:
	LD   @R0
	ADDC @R1
	ST   @R0
	INC  RR0
	INC  RR1
	DBNZ C, instr_b_add_first_half_loop
	
	MOV  #2, C
instr_b_add_second_half_loop:
	LD   @R0
	ADDC B
	ST   @R0
	INC  RR0
	DBNZ C, instr_b_add_second_half_loop
	JMPF fetch_instr_maybe_pc_changed		;PC may have changed
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BEQ  BNE                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_beq_bne:
	BN   ARM_SR, SR_BIT_Z, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	INC  WORD_VAL + 1
instr_cond_b_jump_if_low_bit_of_instr_top_word_set:
	BP   WORD_VAL + 1, 0, instr_cond_b_do
	JMPF fetch_instr
	
instr_cond_b_do:
	AND  #$0
	BN   WORD_VAL + 0, 7, instr_cond_b_do_sign_ext_calced
	OR   #$FF
instr_cond_b_do_sign_ext_calced:
	ST   B
	ST   WORD_VAL + 1
	JMPF instr_b_do_execute_branch
	

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BCS  BCC                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bcs_bcc:
	BN   ARM_SR, SR_BIT_C, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	NOT1  WORD_VAL + 1, 0
	JMPF instr_cond_b_jump_if_low_bit_of_instr_top_word_set

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BMI  BPL                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bmi_bpl:
	BN   ARM_SR, SR_BIT_N, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	NOT1  WORD_VAL + 1, 0
	JMPF instr_cond_b_jump_if_low_bit_of_instr_top_word_set

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BVS  BVC                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bvs_bvc:
	BN   ARM_SR, SR_BIT_V, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	NOT1  WORD_VAL + 1, 0
	JMPF instr_cond_b_jump_if_low_bit_of_instr_top_word_set

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BHI  BLS                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bhi_bls:
	BN   ARM_SR, SR_BIT_C, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	BP   ARM_SR, SR_BIT_Z, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	NOT1  WORD_VAL + 1, 0
	JMPF instr_cond_b_jump_if_low_bit_of_instr_top_word_set

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BGT  BLE                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bgt_ble:
	BP   ARM_SR, SR_BIT_Z, instr_cond_b_jump_if_low_bit_of_instr_top_word_set
	;fall through
	;the logic behind this one is very convoluted, but trust me, it works. if unsure, please examine the following table:
	;	INS LOW BIT:	0	1	0	1
	;	------------------------------
	;	N	V	Z		GE	LT	GT	LE
	;	0	0	0		1	0	1	0
	;	1	1	0		1	0	1	0
	;	0	1	0		0	1	0	1
	;	1	0	0		0	1	0	1
	;	1	0	1		0	1	0	1
	;	0	1	1		0	1	0	1
	;	1	1	1		1	0	0	1
	;	0	0	1		1	0	0	1

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                BGE  BLT                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bge_blt:
	NOT1  WORD_VAL + 1, 0
	BN   ARM_SR, SR_BIT_N, instr_bge_blt_n_clr
	NOT1  WORD_VAL + 1, 0
instr_bge_blt_n_clr:
	BN   ARM_SR, SR_BIT_V, instr_bge_blt_v_clr
	NOT1  WORD_VAL + 1, 0
instr_bge_blt_v_clr:

	;if N == V, we've NOTted low bit 3 times, else 2 times, this if condition is met, we've inverted it as we expect
	JMPF instr_cond_b_jump_if_low_bit_of_instr_top_word_set

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDRB(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrb_imm:
	CALLF get_immed_5
	ST    RR3
	MOV   #1, ACC
	PUSH  ACC
	
instr_load_imm_common:
	CALLF get_regno3_3
	ST    RR0
	
	CALLF get_regno3_0
	PUSH  ACC
	PUSH  ACC
	
	;calc effective address into WORD_VAL
	LD    RR3
	MOV   #WORD_VAL, RR1
	CALLF add_R0_and_8bit_to_R1

	;clear dst reg entirely
	POP   RR1
	MOV   #4, B
	AND   #$00
instr_load_imm_common_loop:
	ST    @R1
	INC   RR1
	DBNZ  B, instr_load_imm_common_loop
	
	;now read
	MOV   #WORD_VAL, ACC
	POP   C
	POP   B
	CALLF readmem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDRH(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrh_imm:
	CALLF get_immed_5
	ROL
	ST    RR3
	MOV   #2, ACC
	PUSH  ACC
	JMPF  instr_load_imm_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDR(1)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldr_imm:
	CALLF get_immed_5
	ROL
	ROL
	ST    RR3
	MOV   #4, ACC
	PUSH  ACC
	JMPF  instr_load_imm_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STRB(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_strb_imm:
	MOV   #1, ACC
	PUSH  ACC
	CALLF get_immed_5

instr_store_imm_common:
	PUSH  ACC
	CALLF get_regno3_0		;source of data
	ST    C
	CALLF get_regno3_3		;source of address
	ST    RR0
	POP   ACC
	MOV   #WORD_VAL, RR1
	CALLF add_R0_and_8bit_to_R1
	POP   B
	MOV   #WORD_VAL, ACC
	CALLF writemem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STRH(1)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_strh_imm:
	MOV   #2, ACC
	PUSH  ACC
	CALLF get_immed_5
	ROL
	JMPF  instr_store_imm_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STR(1)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_str_imm:
	MOV   #4, ACC
	PUSH  ACC
	CALLF get_immed_5
	ROL
	ROL
	JMPF  instr_store_imm_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STR(3)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_str_sp_imm:
	CALLF instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine_SP
	CALLF writemem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDR(4)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldr_sp_imm:
	CALLF instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine_SP
	CALLF readmem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDR(3)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldr_literal:
	MOV   #REGS_PC, RR0
	MOV   #2, C
	CALLF instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine
	CALLF readmem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STRB(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_strb_reg:
	MOV   #1, ACC
	PUSH  ACC
	JMPF  instr_str_reg_common


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STRH(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_strh_reg:
	MOV   #2, ACC
	PUSH  ACC
	JMPF  instr_str_reg_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                STR(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_str_reg:
	MOV   #4, ACC
	PUSH  ACC

instr_str_reg_common:
	CALLF instr_ldr_str_reg_calc_ea
	ST    C
	
	MOV   #WORD_VAL, ACC
	POP   B
	CALLF writemem
	JMPF  fetch_instr


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDRB(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrb_reg:
	CALLF instr_ldr_str_reg_calc_ea
	PUSH  ACC
	ST    C
	MOV   #WORD_VAL, ACC
	MOV   #1, B
	CALLF readmem
	POP   RR0
instr_ldrb_zero_extend:
	MOV   #3, C
	AND   #$00
instr_ldrb_reg_zero:
	INC   RR0
	ST    @R0
	DBNZ  C, instr_ldrb_reg_zero
	JMPF  fetch_instr
	
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDRH(2)                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrh_reg:
	CALLF instr_ldr_str_reg_calc_ea
	PUSH  ACC
	ST    C
	MOV   #WORD_VAL, ACC
	MOV   #2, B
	CALLF readmem
	POP   RR0
	INC   RR0
instr_ldrh_zero_extend:
	MOV   #2, C
	AND   #$00
instr_ldrh_reg_zero:
	INC   RR0
	ST    @R0
	DBNZ  C, instr_ldrh_reg_zero
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                LDR(2)                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldr_reg:
	CALLF instr_ldr_str_reg_calc_ea
	ST    C
	MOV   #WORD_VAL, ACC
	MOV   #4, B
	CALLF readmem
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 LDRSB                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrsb_reg:
	CALLF instr_ldr_str_reg_calc_ea
	PUSH  ACC
	ST    C
	MOV   #WORD_VAL, ACC
	MOV   #1, B
	CALLF readmem
	POP   RR0
	LD    @R0
	BN    ACC, 7, instr_ldrb_zero_extend
	MOV   #3, C
	OR    #$FF
instr_ldrb_reg_sext:
	INC   RR0
	ST    @R0
	DBNZ  C, instr_ldrb_reg_sext
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 LDRSH                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldrsh_reg:
	CALLF instr_ldr_str_reg_calc_ea
	PUSH  ACC
	ST    C
	MOV   #WORD_VAL, ACC
	MOV   #2, B
	CALLF readmem
	POP   RR0
	INC   RR0
	LD    @R0
	BN    ACC, 7, instr_ldrh_zero_extend
	MOV   #2, C
	OR    #$FF
instr_ldrsh_reg_sext:
	INC   RR0
	ST    @R0
	DBNZ  C, instr_ldrsh_reg_sext
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                               BKPT  NOPs                               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bkpt_hints:
	BN   WORD_VAL + 1, 0, instr_bkpt
	;various NOPs & hints
	LD   WORD_VAL + 0
	BE   #$30, instr_wfi
	BE   #$20, instr_wfe
	;various actual NOPs
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  WFI                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_wfi:
	CALLF soc_wfi
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  WFE                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_wfe:
	CALLF soc_wfe
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                              SVC hypercall                             //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_svc_hypercall:
	BP    WORD_VAL + 1, 0, instr_svc
	LD    WORD_VAL + 0
	BNZ   have_hypercall_num
	LD    REGS_R12 + 0
have_hypercall_num:
	BZ    instr_hypercall_inval
	CALLF core_hypercall_handler
	BNZ   instr_hypercall_inval
	JMPF  fetch_instr_maybe_pc_changed	;PC may have been changed by hypercall handler (why not)
instr_hypercall_inval:		;HardFault for invalid hypercalls
	MOV  #3, ACC
	JMPF vector_take

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  SVC                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_svc:
	MOV  #11, ACC
	JMPF vector_take

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  BKPT                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bkpt:
	;fallthrough to undef

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  UND                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
;we just have these loop forever
instr_undef:
	MOV  #3, ACC
	JMPF vector_take

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  CPS                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_cps_and_undef:
	BP   WORD_VAL + 1, 0, instr_undef
	LD   WORD_VAL + 0
	AND  #$EC
	BNE  #$60, instr_undef
	
	;TODO: ? cps?
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  POP                                   //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_pop:
	LD    WORD_VAL + 1
	AND   #$1
	ST    TMPBYTE0			;record if we should load PC, do LOAD not STORE
	MOV   #13, ACC			;number of src reg (SP)
	JMPF  instr_ldmia_stmia_pop_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                  PUSH                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_push:
	;we'll use TMPBYTE0 as pointer for regs
	
									;first see if LR push was requested and handle that right away
	BN    WORD_VAL + 1, 0, push_lr_dealt_with
	
									;LR push was requedsted - do it
	MOV   #REGS_LR, C
	CALLF instr_push_one
	
push_lr_dealt_with:
	MOV   #REGS_START + 8 * 4, TMPBYTE0	;point it at R8

instr_push_loop:

	LD    TMPBYTE0					;decrement the pointer to the "cur reg" being considered for pushing
	SUB   #4
	ST    TMPBYTE0

	BN    WORD_VAL + 0, 7, push_reg_done
	
	LD    TMPBYTE0					;do the write
	ST    C
	CALLF instr_push_one

push_reg_done:
	LD    WORD_VAL + 0				;rotate mask
	ROL
	AND   #$FE
	BZ    instr_push_shortcout_out	;out if no more regs left
	ST    WORD_VAL + 0
	
	LD    TMPBYTE0					;if our pointer has not yet reached R0, keep going (we're still on low regs)
	BNE   #REGS_START, instr_push_loop

instr_push_shortcout_out:
	JMPF  fetch_instr


instr_push_one:		;pointer to word to push in C
	CALLF decrement_SP_by_4
	MOV   #REGS_SP, ACC
	MOV   #4, B
	JMPF  writemem


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 STMIA                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_stmia:
	MOV   #4, TMPBYTE0			;do not store PC, do STORE not LOAD
	
	LD    WORD_VAL + 1
	AND   #$07					;calc number of src reg (to determine if writeback is done)
	JMPF  instr_ldmia_stmia_pop_common

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 LDMIA                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_ldmia:
	MOV   #0, TMPBYTE0			;do not load PC, do LOAD not STORE
	
	LD    WORD_VAL + 1
	AND   #$07					;calc number of src reg (to determine if writeback is done)
	
instr_ldmia_stmia_pop_common:
	;on input:
	;	TMPBYTE0[0] is set to load PC
	;	TMPBYTE0[2] is set to store instead of loading (stmia)
	;	ACC is reg number we'll load from
	;	WORD_VAL + 0  still has low reg bitmask
	;we'll also use:
	;	TMPBYTE0[1] to indicate that we DO want to writeback
	;	TMPBYTE1  to store the source reg number
	;	TMPBYTE2  to store the log reg bitmask
	;	TMPBYTE3  for loop iterator "reg number"

	SET1  TMPBYTE0, 1			;we do writeback by deault

	ST    TMPBYTE1				;save the reg number of our source reg
	CALLF reg_num_to_reg_ptr
	ST    RR1					;R1 = &source_reg
	
	LD    WORD_VAL + 0
	ST    TMPBYTE2				;save the low reg bitmask
	
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0		;since we do not always write back, we cannot clobber src reg, so store address we'll modify in WROD_VAL

;now load regs as needed
	MOV   #0, TMPBYTE3			;TRL is reg num
	
instr_ldmia_regs_loop:
	BN    TMPBYTE2, 0, instr_ldmia_regs_loop_reg_processed
	;if we get here, we need to load this reg
	
	LD    TMPBYTE3				;Read reg number TMPBYTE3 from addr WORD_VAL
	CALLF reg_num_to_reg_ptr
	ST    C
	MOV   #4, B
	MOV   #WORD_VAL, ACC
	
	BP    TMPBYTE0, 2, ldmia_stmia_pop_needs_write
	CALLF readmem
	JMPF  ldmia_stmia_pop_mem_access_done
ldmia_stmia_pop_needs_write:
	CALLF writemem
ldmia_stmia_pop_mem_access_done:
	
	MOV   #WORD_VAL, RR0		;WORD_VAL += 4
	LD    @R0
	ADD   #4
	ST    @R0
	MOV   #3, B
instr_ldmia_regs_loop_incr_addr:
	INC   RR0
	LD    @R0
	ADDC  #0
	ST    @R0
	DBNZ  B, instr_ldmia_regs_loop_incr_addr

	LD    TMPBYTE3				;see if we need to cancel our writeback (source reg is being loaded)
	BNE   TMPBYTE1, instr_ldmia_regs_loop_reg_processed
	CLR1  TMPBYTE0, 1			;cancel writeback

instr_ldmia_regs_loop_reg_processed:
	LD    TMPBYTE2				;shift bitmask
	ROR
	AND   #$7F
	BZ    instr_ldmia_no_more_bits_shortcut
	ST    TMPBYTE2
	
	INC   TMPBYTE3				;increment "current" reg num, if it is less than 8, go to next loop iteration
	BN    TMPBYTE3, 3, instr_ldmia_regs_loop

instr_ldmia_no_more_bits_shortcut:

								;if we do not plan to load PC, go to writeback
	BN    TMPBYTE0, 0, instr_ldmia_writeback_if_needed
	
	CLR1  TMPBYTE0, 0			;clear "load pc" bit as we only need it once
	MOV   #$0F, TMPBYTE3		;reg num  = PC
	MOV   #$01, TMPBYTE2		;mask = one bit
	JMPF  instr_ldmia_regs_loop

								;we get here when all the loading is done and we should writeback if it is required
instr_ldmia_writeback_if_needed:
	BN    TMPBYTE0, 1, instr_ldmia_writeback_done
	
	LD    TMPBYTE1				;we need to do writeback. Do it
	CALLF reg_num_to_reg_ptr
	ST    RR0					;get addr of src reg into R0
	MOV   #WORD_VAL, RR1
	CALLF cpy_word_R1_to_R0		;do the copy

								;we are all done. last step is to precautionarily clear the lower PC bit in case we loaded PC
instr_ldmia_writeback_done:
	CLR1  REGS_PC + 0, 0		;in case we loaded PC, clear bottom bit
	JMPF  fetch_instr_maybe_pc_changed	; PC may have changed (if this was a POP). checking takes longer than this path

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;           UTILITY FUNCS IN USE BY ALL. HERE FOR RANGE REASONS           ;;
;;                        (more opcode logic below)                        ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

instr_logical_op_set_NZ:	;expects R0 to point one past the end of result, OR of all bytes in C
	DEC  RR0
	LD   @R0
	CLR1 ARM_SR, SR_BIT_N
	BN   ACC, 7, instr_logical_op_set_NZ_not_neg
	SET1 ARM_SR, SR_BIT_N
instr_logical_op_set_NZ_not_neg:
	LD   C
	CLR1 ARM_SR, SR_BIT_Z
	BNZ  instr_logical_op_set_NZ_not_zero
	SET1 ARM_SR, SR_BIT_Z
instr_logical_op_set_NZ_not_zero:
	JMPF fetch_instr


;get pointer to proper register in ram from reg value in bits 0..2
get_regno3_0:
	LD   WORD_VAL + 0
	ROL
	ROL
	AND  #$1C
	ADD  #REGS_START
	RET

;get pointer to proper register in ram from reg value in bits 3..5
get_regno3_3:
	LD   WORD_VAL + 0
	ROR
	AND  #$1C
	ADD  #REGS_START
	RET
	
;get pointer to proper register in ram from reg value in bits 6..8
get_regno3_6:
	LD   WORD_VAL + 1
	RORC
	LD   WORD_VAL + 0
	RORC
	ROR
	ROR
	ROR
	AND  #$1C
	ADD  #REGS_START
	RET

;get pointer to proper register in ram from reg value in bits 8..10
get_regno3_8:
	LD   WORD_VAL + 1
	ROL
	ROL
	AND  #$1C
	ADD  #REGS_START
	RET

add_R0_and_8bit_to_R1:					;@R0 + ACC -> R1, clobbers R0, R1, B
	ADD  @R0
	ST   @R1
	MOV  #3, B
	
add_R0_and_8bit_to_R1_loop:
	INC  RR0
	INC  RR1
	LD   @R0
	ADDC #0
	ST   @R1
	DBNZ B, add_R0_and_8bit_to_R1_loop
	RET

set_NZ_R0_no_rewind_and_goto_next_instr:	;R0 = start of result word
	LD   RR0
	ADD  #4
	ST   RR0
	;fallthrough
set_NZ_R0_with_rewind_and_goto_next_instr:	;R0 = one past end of result word, this order actually is easier
	DEC  RR0
	LD   @R0	;high byte
	MOV  #3, B
	CLR1 ARM_SR, SR_BIT_N
	BN   ACC, 7, set_NZ_R0_with_rewind_or_all_bytes
	SET1 ARM_SR, SR_BIT_N
set_NZ_R0_with_rewind_or_all_bytes:
	DEC  RR0
	OR   @R0
	DBNZ B, set_NZ_R0_with_rewind_or_all_bytes
	CLR1 ARM_SR, SR_BIT_Z
	BNZ  set_NZ_R0_with_rewind_not_zero
	SET1 ARM_SR, SR_BIT_Z
set_NZ_R0_with_rewind_not_zero:
	JMPF fetch_instr

;get pointer to proper register in ram from reg value in bits 0..3
get_regno4_0:
	LD   WORD_VAL + 0
;get pointer to proper register in ram from reg value in bits 0..3 OF LOADED value (for high words)
	ST   C
	ROL
	ROL
	AND  #$1C
	BN   WORD_VAL + 0, 7, get_regno4_0_nohi
	OR   #$20
get_regno4_0_nohi:
	ADD  #REGS_START
	RET

;get pointer to proper register in ram from reg value in bits 3..6
get_regno4_3:
	LD   WORD_VAL + 0
	ROR
	AND  #$3C
	ADD  #REGS_START
	RET

inc_R0_4_times:
	INC  RR0
inc_R0_3_times:
	INC  RR0
	INC  RR0
	INC  RR0
	RET

orr_all_bytes_of_R0:			;clobbers B, moves R0 3bytes forward
	MOV  #3, B
	LD   @R0
orr_all_bytes_of_R0_loop:
	INC  RR0
	OR   @R0
	DBNZ B, orr_all_bytes_of_R0_loop
	RET

instr_add_R1_to_R0:				;clobbers B
	MOV  #4, B
	CLR1 PSW, CY
instr_add_R1_to_R0_loop:
	LD   @R0
	ADDC @R1
	ST   @R0
	INC  RR0
	INC  RR1
	DBNZ B, instr_add_R1_to_R0_loop
	RET

instr_sub_R1_from_R0:			;clobbers B
	MOV  #4, B
	CLR1 PSW, CY
instr_sub_R1_from_R0_loop:
	LD   @R0
	SUBC @R1
	ST   @R0
	INC  RR0
	INC  RR1
	DBNZ B, instr_sub_R1_from_R0_loop
	RET

inc_R1_3_times:
	INC  RR1
	INC  RR1
	INC  RR1
	RET

;get immed_3 from a 16-bit instr into ACC
get_immed_3:
	LD   WORD_VAL + 1
	RORC
	LD   WORD_VAL + 0
	RORC				;top 3 bits are now the bits we want
	ROL
	ROL
	ROL					;now it is the bottom 3 bits
	AND  #$07
	RET

;get immed_5 from a 16-bit instr into ACC (clobbers B & C)
get_immed_5:
	LD   WORD_VAL + 0
	ROLC
	ST   C
	LD   WORD_VAL + 1
	ROLC
	ST   B
	LD   C
	ROLC
	LD   B
	ROLC
	AND  #$1F
	RET

cpy_word_R1_to_R0:		;warning: moves both pointers forward 4 bytes, clobbers C
	MOV   #4, C
cpy_word_R1_to_R0_byt:
	LD    @R1
	INC   RR1
	ST    @R0
	INC   RR0
	DBNZ  C, cpy_word_R1_to_R0_byt
	RET
	
;convert ACC value into a pseudoreg (write to RAM (WORD_VAL), zero-extend), clobbers ACC
convert_acc_to_reg:
	ST    WORD_VAL + 0
	AND   #$0
	ST    WORD_VAL + 1
	ST    WORD_VAL + 2
	ST    WORD_VAL + 3
	RET
	
;convert ACC value into a pseudoreg (write to RAM (WORD_VAL), zero-extend, point R1 at it), clobbers ACC
convert_acc_to_reg_R1:
	CALLF convert_acc_to_reg
	MOV   #WORD_VAL, RR1	;Rm (fake) pointer in R1
	RET

;convert ACC value into a pseudoreg (write to RAM (WORD_VAL), zero-extend, point R1 at it), clobbers ACC
convert_acc_to_reg_R0:
	CALLF convert_acc_to_reg
	MOV   #WORD_VAL, RR0	;Rm (fake) pointer in R0
	RET

cpy_word_R2_to_R0:		;warning: moves both pointers forward 4 bytes
	MOV   #4, VRMAD1
cpy_word_R2_to_R0_byt:
	LD    @R2
	INC   RR2
	ST    @R0
	INC   RR0
	DBNZ  VRMAD1, cpy_word_R2_to_R0_byt
	RET

copy_C_bit_inverted:	;copies NOT C into ARM's C	(used after SUB/SBC)
	NOT1 PSW, CY
copy_C_bit:
	CLR1 ARM_SR, SR_BIT_C
	BN   PSW, CY, copy_C_bit_no_c
	SET1 ARM_SR, SR_BIT_C
copy_C_bit_no_c:
	RET

calc_V_bit_addition:	;need R0 pointing one byte past end of one op, R1 for other op, R2 for result, moves each to last point
	CLR1 ARM_SR, SR_BIT_V
	DEC  RR0
	DEC  RR1
	DEC  RR2
	LD   @R0
	XOR  @R1
	BP   ACC, 7, calc_V_bit_addition_bit_calced	;top bits not equal means we cannot overflow
	LD   @R0
	XOR  @R2
	BN   ACC, 7, calc_V_bit_addition_bit_calced	;top bits of result equal to operand - not overflow
	SET1 ARM_SR, SR_BIT_V
calc_V_bit_addition_bit_calced:
	RET

calc_V_bit_subtraction:	;need R0 pointing one byte past end of one op, R1 for other op, R2 for result, moves each to last point
	CLR1 ARM_SR, SR_BIT_V
	DEC  RR0
	DEC  RR1
	DEC  RR2
	LD   @R0
	XOR  @R1
	BN   ACC, 7, calc_V_bit_subtraction_bit_calced	;top bits equal means we cannot overflow
	LD   @R0
	XOR  @R2
	BN   ACC, 7, calc_V_bit_subtraction_bit_calced	;top bits of result equal to operand - not overflow
	SET1 ARM_SR, SR_BIT_V
calc_V_bit_subtraction_bit_calced:
	RET


	
dispatch_bits_6_to_8:
	LD    WORD_VAL + 1
	RORC
	LD    WORD_VAL + 0
	RORC						;selector now in top 3 bits
	ROR
	ROR
	ROR
	ROR							;selector now in 0x0E
	AND   #$0E
	POP   C						;hi
	POP   B						;lo
	ADD   B
	PUSH  ACC					;lo
	AND   #$0
	ADDC  C
	PUSH  C						;hi
	RET

neg_if_neg:						;if @R0 is negative, negate it and return something with low bit set, else return low bit clear, clobbers B, R0
	PUSH RR0
	LD   RR0
	ADD  #3
	ST   RR0
	LD   @R0
	POP  RR0
	BP   ACC, 7, neg_R0
;was not negative
	AND  #$0
	RET
neg_R0:							;negate @R0, clobbers B, R0
	MOV  #4, B
neg_R0_counter_in_B:			;in case we want to neg non-word-length
	CLR1 PSW, CY
neg_R0_loop:
	AND  #$0
	SUBC @R0
	ST   @R0
	INC  RR0
	DBNZ B, neg_R0_loop
	OR   #1
	RET


instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine_SP:
	MOV   #REGS_SP, RR0
	MOV   #0, C
	;fallthrough
instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine:					;C has extra val to add to effective address (we use it to adjust for PC reads). only vals of 2 or 0 allowed
	MOV  #2, B
instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine_shift_all_left:
	LD   WORD_VAL + 0
	ROLC
	ST   WORD_VAL + 0
	LD   WORD_VAL + 1
	ROLC
	ST   WORD_VAL + 1
	DBNZ B, instr_ldr_str_hardcoded_reg_plus_imm_lsl_2_subroutine_shift_all_left
	;we shift the entire instr left 2 which actually helps us here
	
	PUSH WORD_VAL + 1		;for later
	
	LD   WORD_VAL + 0		;calculate effective address -> WORD_VAL
	AND  #$FC
	OR   C
	ADD  @R0
	AND  #$FC				;in case we added more
	INC  RR0
	ST   WORD_VAL + 0
	LD   WORD_VAL + 1
	AND  #$03
	ADDC @R0
	INC  RR0
	ST   WORD_VAL + 1
	AND  #0
	ADDC @R0
	INC  RR0
	ST   WORD_VAL + 2
	AND  #0
	ADDC @R0
	ST   WORD_VAL + 3
	
	POP  ACC				;get Rd
	AND  #$1C
	ADD  #REGS_START
	ST   C
	MOV  #4, B
	MOV  #WORD_VAL, ACC
	RET

instr_ldr_str_reg_calc_ea:			;returns ACC = &Rd, WORD_VAL = addr
	CALLF get_regno3_0
	PUSH  ACC
	CALLF get_regno3_3
	PUSH  ACC
	CALLF get_regno3_6
	ST    RR1
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0			;WORD_VAL = Rm
	
	POP   RR1
	MOV   #WORD_VAL, RR0
	CALLF instr_add_R1_to_R0		;WORD_VAL = Rm + Rn
	POP   ACC
	RET

get_imm8_mul_4_into_B_C_TR:
	LD   WORD_VAL + 0
	ROLC
	ST   B
	AND  #$0
	ST   TRH
	ST   TRL
	ROLC 
	ST   C
	LD   B
	ROLC
	AND  #$FC
	ST   B
	LD   C
	ROLC
	ST   C
	RET

instr_add_B_C_TR_and_R1_to_R0:
	MOV  #2, RR2				;R2 = addend
	;fallthrough
	
instr_add_R2_and_R1_to_R0:			;@R0 = @R1 + @R2
	CLR1 PSW, CY
	MOV  #4, VRMAD1
	
instr_add_R2_and_R1_to_R0_loop:
	LD   @R1
	ADDC @R2
	ST   @R0
	INC  RR0
	INC  RR1
	INC  RR2
	DBNZ VRMAD1, instr_add_R2_and_R1_to_R0_loop
	RET

instr_sub_R1_from_R0_to_R2:
	MOV  #4, VRMAD1
	CLR1 PSW, CY
instr_sub_R1_from_R0_to_R2_loop:
	LD   @R0
	INC  RR0
	SUBC @R1
	INC  RR1
	ST   @R2
	INC  RR2
	DBNZ VRMAD1, instr_sub_R1_from_R0_to_R2_loop
	RET

lsl_R0:								;clobbers R0, C
	MOV  #4, C
	CLR1 PSW, CY
lsl_R0_loop:
	LD   @R0
	ROLC
	ST   @R0
	INC  RR0
	DBNZ C, lsl_R0_loop
	RET

lsr_R0:								;clobbers C
	MOV  #4, C
	CLR1 PSW, CY
	LD   RR0
	ADD  #3
	ST   RR0
lsr_R0_loop:
	LD   @R0
	RORC
	ST   @R0
	DEC  RR0
	DBNZ C, lsr_R0_loop
	RET

dp_instrs_common:				;&Rn/&Rd -> R0, &Rm/&Rs-> R1
	CALLF get_regno3_0
	ST    RR0					;Rn/Rd pointer in R0
	CALLF get_regno3_3
	ST    RR1					;Rm/Rs pointer in R1
	RET

get_regno4_4_32bit:				;expects reg value loaded in ACC
	ROR
	ROR
	AND   #$1C
	ADD   #REGS_START
	RET

get_regno4_0_32bit:				;expects reg value loaded in ACC
	AND   #$0F
	;fallthrough

reg_num_to_reg_ptr:
	ROL
	ROL
	ADD   #REGS_START
	RET

add_3_sub_3_common_intro:	;&Rn -> R0, &Rm -> R1, &Rd -> R3 (not deref)
	CALLF get_regno3_3
	ST    RR0				;Rn pointer in R0
	CALLF get_regno3_6
	ST    RR1				;Rm pointer in R1
	CALLF get_regno3_0
	ST    RR3				;Rd pointer in R3 (not dereferencable as R3 points to SFRs)
	RET

decrement_SP_by_4:			;Clobbers B, R0
	MOV   #REGS_SP, RR0
	LD    @R0
	SUB   #4
	ST    @R0
	MOV   #3, B
instr_push_sp_decr_loop:
	INC   RR0
	LD    @R0
	SUBC  #0
	ST    @R0
	DBNZ  B, instr_push_sp_decr_loop
	RET

add_R0_8bit:					;A has value to add, R0 the pointer
	ADD  @R0
	ST   @R0
	MOV  #3, B
	
add_R0_8bit_loop:
	INC  RR0
	LD   @R0
	ADDC #0
	ST   @R0
	DBNZ B, add_R0_8bit_loop
	RET

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                MRS  MSR                                //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mrs_msr:
	;TODO: ?
	JMPF  instr_undef
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 CLREX                                  //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_clrex:
	;TODO: ?
	JMPF  instr_undef

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                     MRS  MSR  CLREX  DSB  DMB  ISB                     //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_mrs_msr_clrex_barriers:
	BN   WORD_VAL + 0, 4, instr_mrs_msr
	BN   WORD_VAL + 2, 6, instr_clrex
	;fallthrough
	
;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                              DSB DMB ISB                               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_barriers:
	;all done :)
	JMPF fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//          B.W BL MOVW MOVT MRS MSR CLREX DSB DMB ISB LDA LDAB           //
;//          LDAEX LDAEXB LDAEXH LDAH LDREX LDREXB LDREXH SG STL           //
;//          STLB STLEX STLEXB STLEXH STLH STREX STREXB STREXH TT          //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
j_instr_weird_loads_stores_secure_shit:	;here for range reasons
	JMPF  instr_weird_loads_stores_secure_shit

instr_32b:
	BN    WORD_VAL + 1, 4, j_instr_weird_loads_stores_secure_shit
	BN    WORD_VAL + 3, 7, instr_movt_movw
	BN    WORD_VAL + 3, 4, instr_mrs_msr_clrex_barriers
	;fallthrough

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                                 B.W BL                                 //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_bw_bl:					;uses WORD_VAL_NEXT2
								;save LR if requested
	BN    WORD_VAL + 3, 6, instr_bw_bl_lr_saved_if_needed
	MOV   #REGS_PC, RR1
	MOV   #REGS_LR, RR0
	CALLF cpy_word_R1_to_R0
	SET1  REGS_LR + 0, 0		;set low bit in LR
instr_bw_bl_lr_saved_if_needed:
								;first handle the weirdness with S bit converting J1 into I1 and J2 into I2
								;The change from Jx to Ix is:	<<if S is clear, invert them, else do not>>
								;since S also ends up being our sign bit, use this opportunity to figure out out sign-extend byte into B
	MOV   #$FF, WORD_VAL + 5
	BP    WORD_VAL + 1, 2, instr_bw_bl_S_bit_processed
	MOV   #$00, WORD_VAL + 5
	NOT1  WORD_VAL + 3, 5		;prepare top byte of result as well (it is all sext bits)
	NOT1  WORD_VAL + 3, 3
instr_bw_bl_S_bit_processed:
	
								;move I1 right one bit	(yes bit 4 is always set otherwise for B.W/BL)
	BP    WORD_VAL + 3, 5, instr_bw_bl_I1_bit_processed
	CLR1  WORD_VAL + 3, 4
instr_bw_bl_I1_bit_processed:

	LD   WORD_VAL + 2			;lsl #1 the bottom 2 bytes and mask out unneeded bits (it is easier to do it now)
	ROLC
	AND   #$FE
	ST    WORD_VAL + 2
	LD    WORD_VAL + 3
	ROLC
	AND   #$3F
	ST    WORD_VAL + 3
	
								;copy bottom 2 bits form top part to top 2 bits
	BN    WORD_VAL + 0, 0, instr_bw_bl_top_bit_0_processed
	SET1  WORD_VAL + 3, 6
instr_bw_bl_top_bit_0_processed:
	BN    WORD_VAL + 0, 1, instr_bw_bl_top_bit_1_processed
	SET1  WORD_VAL + 3, 7
instr_bw_bl_top_bit_1_processed:

								;lsr mid-top byte twice
	LD    WORD_VAL + 0
	ROR
	ROR
	AND   #$3F
	ST    WORD_VAL + 4
	
								;copy bottom 2 bits form top top part to top 2 bits of mid-top byte
	BN    WORD_VAL + 1, 0, instr_bw_bl_top_top_bit_0_processed
	SET1  WORD_VAL + 4, 6
instr_bw_bl_top_top_bit_0_processed:
	BN    WORD_VAL + 1, 1, instr_bw_bl_top_top_bit_1_processed
	SET1  WORD_VAL + 4, 7
instr_bw_bl_top_top_bit_1_processed:
	
								;add to pc and we're done (we do not need to add 2 to PC in this case, yes realy)
	MOV   #REGS_PC, RR0
	MOV   #WORD_VAL + 2, RR1
	CALLF instr_add_R1_to_R0
	JMPF fetch_instr_maybe_pc_changed	; PC may have changed


;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                               MOVW  MOVT                               //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_movt_movw:
								;get pointer to dst reg
	LD    WORD_VAL + 3
	CALLF get_regno4_0_32bit
	ST    RR0
	ADD   #2
								;dispatch between MOVW/MOVT for initial work
	BP    WORD_VAL + 0, 7, instr_long_mov_is_movt
	
instr_long_mov_is_movw:			;for MOVW, clear top 2 bytes
	ST    RR1
	AND   #$00
	ST    @R1
	INC   RR1
	ST    @R1
	JMPF  instr_long_mov_common

instr_long_mov_is_movt:			; for MOVT, make R0 point to top bytes
	ST    RR0

instr_long_mov_common:			;R0 points to the two bytes we'll write
								;lower byte is easy - it is just the lower byte of second word of instr
	LD    WORD_VAL + 2
	ST    @R0
	INC   RR0
								;mov i bit to the position above imm3 (currently guaranteed to be 0)
	BN    WORD_VAL + 1, 2, instr_long_mov_i_processed
	SET1  WORD_VAL + 3, 7
instr_long_mov_i_processed:
								;now put together the last word (RORed4 for now)
	LD    WORD_VAL + 0
	AND   #$0F
	ST    WORD_VAL + 0
	LD    WORD_VAL + 3
	AND   #$F0
	OR    WORD_VAL + 0
								;now ROR it into proper orientation
	ROR
	ROR
	ROR
	ROR
								;and store it
	ST    @R0
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//        LDA LDAB LDAEX LDAEXB LDAEXH LDAH LDREX LDREXB LDREXH SG        //
;//        STL STLB STLEX STLEXB STLEXH STLH STREX STREXB STREXH TT        //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
instr_weird_loads_stores_secure_shit:
	LD    WORD_VAL + 1
	BE    #$E8, instr_might_be_tt
	BNE   #$E9, instr_weird_loads_stores_secure_shit_not_sg_or_tt
	LD    WORD_VAL + 0
	BNE   #$7F, instr_weird_loads_stores_secure_shit_not_sg_or_tt
	LD    WORD_VAL + 2
	BNE   #$7F, instr_weird_loads_stores_secure_shit_not_sg_or_tt
	LD    WORD_VAL + 3
	BNE   #$E9, instr_weird_loads_stores_secure_shit_not_sg_or_tt
	
instr_SG:	;SG acts as NOP, as needed
	jmpf  fetch_instr
	
instr_weird_loads_stores_secure_shit_not_sg_or_tt:
	;we have no secure mode support .. duh
	JMPF  instr_undef

instr_might_be_tt:
	LD    WORD_VAL + 0
	AND   #$F0
	BNE   #$40, instr_weird_loads_stores_secure_shit_not_sg_or_tt
	LD    WORD_VAL + 2
	AND   #$FC
	BNZ   instr_weird_loads_stores_secure_shit_not_sg_or_tt
	LD    WORD_VAL + 3
	AND   #$F0
	BNE   #$F0, instr_weird_loads_stores_secure_shit_not_sg_or_tt

instr_TT:	;in non-secure state TT always returns zero - do that
	LD    WORD_VAL + 3
	CALLF get_regno4_0_32bit
	ST    RR0
	AND   #$00
	MOV   #4, B
	
inst_TT_loop:
	ST    @R0
	INC   RR0
	DBNZ  B, inst_TT_loop
	JMPF  fetch_instr

;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                          VECTOR HANDLING CODE                          //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
vector_take:						;vector number in ACC
	PUSH  ACC
									;NZCV is order fo flags for arm - do that
	AND   #$00
	ST    WORD_VAL + 0
	ST    WORD_VAL + 1
	ST    WORD_VAL + 2
	MOV   #1, WORD_VAL + 3			;T bit
	
	BN    ARM_SR, SR_BIT_C, vec_take_no_C
	SET1  WORD_VAL + 3, 5
vec_take_no_C:
	BN    ARM_SR, SR_BIT_N, vec_take_no_N
	SET1  WORD_VAL + 3, 7
vec_take_no_N:
	BN    ARM_SR, SR_BIT_Z, vec_take_no_Z
	SET1  WORD_VAL + 3, 6
vec_take_no_Z:
	BN    ARM_SR, SR_BIT_V, vec_take_no_V
	SET1  WORD_VAL + 3, 4
vec_take_no_V:
	BN    ARM_SR, SR_BIT_I, vec_take_no_I	;exception number is "1" when we are not taking interrupts, 0 else
	SET1  WORD_VAL + 0, 0
vec_take_no_I:

	MOV   #WORD_VAL, C
	CALLF instr_push_one
	MOV   #REGS_PC, C
	CALLF instr_push_one
	MOV   #REGS_LR, C
	CALLF instr_push_one
	MOV   #REGS_R12, C
	CALLF instr_push_one
	MOV   #REGS_START + 4 * 3, C
	CALLF instr_push_one
	MOV   #REGS_START + 4 * 2, C
	CALLF instr_push_one
	MOV   #REGS_START + 4 * 1, C
	CALLF instr_push_one
	MOV   #REGS_START + 4 * 0, C
	CALLF instr_push_one
	OR    #$FF
	ST    REGS_LR + 3
	ST    REGS_LR + 2
	ST    REGS_LR + 1
	AND   #$F9
	ST    REGS_LR + 0
	POP   ACC

vector_take_nopush:				;used for chaining
	ROL
	ROL
	ST    WORD_VAL + 0
	AND   #$03
	ST    WORD_VAL + 1
	CLR1  WORD_VAL + 0, 0
	CLR1  WORD_VAL + 0, 1
	MOV   #0, WORD_VAL + 2
	MOV   #0, WORD_VAL + 3
	SET1  ARM_SR, SR_BIT_I
	JMPF  vec_take_jump

vector_return:
								;see if we can chain another irq
	LD    ARM_IRQS_LO			;certainly not if none are pending
	AND   ARM_IRQE_LO
	BNZ   vector_return_irq_not_handled
	LD    ARM_IRQS_HI
	AND   ARM_IRQE_HI
	BZ    vector_return_irq_handled

vector_return_irq_not_handled:
	MOV   #REGS_SP, RR1			;get addr of our pushed SR's top byte in WORD_VAL
	MOV   #WORD_VAL, RR0
	CALLF cpy_word_R1_to_R0
	MOV   #WORD_VAL, RR0
	MOV   #31, ACC
	CALLF add_R0_8bit
	MOV   #WORD_VAL_NEXT2, C	;read it into WORD_VAL_NEXT2 + 0
	MOV   #WORD_VAL, ACC
	MOV   #1, B
	CALLF readmem
								;if we're returning to context that does not allow iterrupts, do not chain
	BP    WORD_VAL_NEXT2 + 0, 0, vector_return_irq_handled
	
	LD    ARM_IRQS_LO
	AND   ARM_IRQE_LO
	BZ    vector_return_lo_irqs_processed
	MOV   #0, B
	CALLF core_irqs_process
	JMPF  vector_take_nopush

vector_return_lo_irqs_processed:
	LD    ARM_IRQS_HI
	AND   ARM_IRQE_HI
	BZ    vector_return_hi_irqs_processed
	MOV   #8, B
	CALLF core_irqs_process
	JMPF  vector_take_nopush
	
vector_return_hi_irqs_processed:


vector_return_irq_handled:
	MOV   #REGS_START + 4 * 0, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_START + 4 * 1, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_START + 4 * 2, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_START + 4 * 3, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_R12, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_LR, C
	CALLF instr_vec_ret_pop_one
	MOV   #REGS_PC, C
	CALLF instr_vec_ret_pop_one
	MOV   #WORD_VAL, C
	CALLF instr_vec_ret_pop_one
	CLR1  REGS_PC + 0, 0		;just in case
								;now convert ARM SR to our SR
	MOV   #0, ARM_SR
	BN    WORD_VAL + 0, 0, vec_ret_no_I
	SET1  ARM_SR, SR_BIT_I		;exception number is "1" when we are not taking itnerrupts, 0 else
vec_ret_no_I:
	BN    WORD_VAL + 3, 5, vec_ret_no_C
	SET1  ARM_SR, SR_BIT_C
vec_ret_no_C:
	BN    WORD_VAL + 3, 7, vec_ret_no_N
	SET1  ARM_SR, SR_BIT_N
vec_ret_no_N:
	BN    WORD_VAL + 3, 6, vec_ret_no_Z
	SET1  ARM_SR, SR_BIT_Z
vec_ret_no_Z:
	BN    WORD_VAL + 3, 4, vec_ret_no_V
	SET1  ARM_SR, SR_BIT_V
vec_ret_no_V:
	BN    WORD_VAL + 3, 0, vector_return_nonthumb
	JMPF  fetch_instr_maybe_pc_changed	; PC may have changed
vector_return_nonthumb:
	MOV  #11, ACC
	JMPF vector_take



instr_vec_ret_pop_one:		;pointer to word to pop in C
	MOV   #REGS_SP, ACC
	MOV   #4, B
	CALLF readmem
	MOV   #REGS_SP, RR0
	MOV   #4, ACC
	JMPF  add_R0_8bit



core_irqs_process:			;ACC has irq mask, B has number to add to irq num (bottom 3 bits must be zero)
							;returns vector to jump to in ACC
							;this naive solution is actually faster than the clever one
	BP    ACC, 0, core_irqs_process_0
	BP    ACC, 1, core_irqs_process_1
	BP    ACC, 2, core_irqs_process_2
	BP    ACC, 3, core_irqs_process_3
	BP    ACC, 4, core_irqs_process_4
	BP    ACC, 5, core_irqs_process_5
	BP    ACC, 6, core_irqs_process_6
core_irqs_process_7:
	MOV   #7, ACC
	JMP   core_irqs_process_common
core_irqs_process_6:
	MOV   #6, ACC
	JMP   core_irqs_process_common
core_irqs_process_5:
	MOV   #5, ACC
	JMP   core_irqs_process_common
core_irqs_process_4:
	MOV   #4, ACC
	JMP   core_irqs_process_common
core_irqs_process_3:
	MOV   #3, ACC
	JMP   core_irqs_process_common
core_irqs_process_2:
	MOV   #2, ACC
	JMP   core_irqs_process_common
core_irqs_process_1:
	MOV   #1, ACC
	JMP   core_irqs_process_common
core_irqs_process_0:
	MOV   #0, ACC

core_irqs_process_common:
	ADD   B
	ADD   #16		;irq vectors start at 16
	RET

;;this clever solution is actually slower than the naive one, so we do not use it
;							;we set bits there when we shoudl really not, then XOR bottom bits
;	ST    C
;	AND   #$F0
;	BNZ   core_irqs_add_4
;	LD    C
;	SET1  B, 2
;core_irqs_add_4:
;
;	ST    C
;	AND   #$CC
;	BNZ   core_irqs_add_2
;	LD    C
;	SET1  B, 1
;core_irqs_add_2:
;
;	ST    C
;	AND   #$AA
;	BNZ   core_irqs_add_1
;	LD    C
;	SET1  B, 0
;core_irqs_add_1:
;
;	LD    B
;	XOR   #7
	



;////////////////////////////////////////////////////////////////////////////
;//                                                                        //
;//                        UMULL UMLAL SMULL SMLAL                         //
;//                                                                        //
;////////////////////////////////////////////////////////////////////////////
inst_lmul:		;uses WORD_VAL_NEXT4
	;we'll use bits in TMPBYTE0:
	; [0] - we sign inverted Rm (and must undo it)
	; [1] - we sign inverted Rn (and must undo it)
	; [2] - we will sign invert result
									;first capture the data from the instr so we can then overwrite it
	MOV   #WORD_VAL + 2, C
	CALLF read_instr_16b
	
	;DBG
	;.byte $51
	;.byte $03
	;DBG
	
	LD    WORD_VAL + 3
	CALLF get_regno4_0_32bit
	PUSH  ACC						;RdHi
	LD    WORD_VAL + 3
	CALLF get_regno4_4_32bit
	PUSH  ACC						;RdLo
	LD    WORD_VAL + 0
	ST    C							;save it so we can figure out what to do later
	CALLF get_regno4_0_32bit
	ST    RR3						;Rn
	LD    WORD_VAL + 2
	CALLF get_regno4_0_32bit
	ST    RR2						;Rm
	ST    RR0
									;now invert shit as needed, record it
	MOV   #0, TMPBYTE0
	BP    C, 5, lmul_initial_signed_shit_done
	
lmul_is_signed:

	SUB   RR3
	BZ    lmul_same_reg_both_ops
	
	CALLF neg_if_neg
	BN    ACC, 0, lmul_did_not_negate_Rm
	SET1  TMPBYTE0, 0
	NOT1  TMPBYTE0, 2
lmul_did_not_negate_Rm:
	LD    RR3
	ST    RR0
	CALLF neg_if_neg
	BN    ACC, 0, lmul_did_not_negate_Rn
	SET1  TMPBYTE0, 1
	NOT1  TMPBYTE0, 2
lmul_did_not_negate_Rn:
	JMPF  lmul_initial_signed_shit_done

lmul_same_reg_both_ops:
	CALLF neg_if_neg
	BN    ACC, 0, lmul_did_not_negate_same_reg_Rm_Rn
	SET1  TMPBYTE0, 0
lmul_did_not_negate_same_reg_Rm_Rn:

lmul_initial_signed_shit_done:
									;now sort out if we need to accumulate shit
	BP    C, 6, lmul_is_accumulate
	MOV   #WORD_VAL, RR1
	MOV   #8, C
	AND   #$00
lmul_zero_initial_loop:
	ST    @R1
	INC   RR1
	DBNZ  C, lmul_zero_initial_loop
	JMPF  lmul_is_accumulate_resolved

lmul_is_accumulate:
	POP   ACC
	ST    TMPBYTE1
	ST    RR1
	MOV   #WORD_VAL + 0, RR0
	CALLF cpy_word_R1_to_R0
	POP   RR1
	PUSH  RR1
	PUSH  TMPBYTE1
	MOV   #WORD_VAL + 4, RR0
	CALLF cpy_word_R1_to_R0
	
	;if we're negating result, we must negate this value now here
	BN    TMPBYTE0, 2, lmul_is_accumulate_resolved
	MOV   #WORD_VAL, RR0
	MOV   #8, C
	CLR1  PSW, CY
lmul_accumulate_preinvert_result_loop:
	AND   #$00
	SUBC  @R0
	ST    @R0
	INC   RR0
	DBNZ  C, lmul_accumulate_preinvert_result_loop

lmul_is_accumulate_resolved:
	PUSH  TMPBYTE0
	;at this point we can do actual multiplication
	LD   RR3
	ST   RR1
	MOV   #0, TMPBYTE0
	
instr_lmul_outer:
	MOV   #0, TMPBYTE1
	
instr_lmul_inner:
	;calc dest offset
	LD    TMPBYTE0
	ADD   TMPBYTE1
	ST    RR3
	
	;get pointer to R0's data bytes we need
	LD    RR2
	ADD   TMPBYTE1
	ST    RR0
	
	;get data and mul
	LD    @R1
	BZ    instr_lmul_inner_done		;shortcut!
	ST    B
	LD    @R0
	ST    C
	INC   RR0
	LD    @R0

	MUL
	
	;save ACC
	ST    VRMAD1
	
	;get destination pointer into R0
	LD    RR3
	ADD   #WORD_VAL
	ST    RR0
	
	;first byte guaranteed to be needed
	LD    @R0
	ADD   C
	ST    @R0
	INC   RR3
	BP    RR3, 3, instr_lmul_inner_done	;jump if result 8 (no more carrying to do)
	
	;second byte time
	INC   RR0
	LD    @R0
	ADDC  VRMAD1
	ST    @R0
	INC   RR3
	BP    RR3, 3, instr_lmul_inner_done	;jump if result 8 (no more carrying to do)
	
	;third byte
	INC   RR0
	LD    @R0
	ADDC  B
	ST    @R0
	INC   RR3
	BP    RR3, 3, instr_lmul_inner_done	;jump if result 8 (no more carrying to do)
	
	;carry through as long as there is carry and len < 8

lmul_carrythrough_loop:
	BN    PSW, CY, instr_lmul_inner_done
	INC   RR0
	INC   RR3
	LD    @R0
	ADDC  #0
	ST    @R0
	BN    RR3, 3, lmul_carrythrough_loop	;jump if result < 8 (more carrying to do)
	
instr_lmul_inner_done:

	INC   TMPBYTE1
	INC   TMPBYTE1
	BN    TMPBYTE1, 2, instr_lmul_inner	;jump if result not 4

	INC   RR1
	INC   TMPBYTE0
	BN    TMPBYTE0, 2, instr_lmul_outer	;jump if result not 4
	
;calculation is done, result is in WORD_VAL (8 bytes)
	POP   TMPBYTE0
	BN    TMPBYTE0, 0, lmul_do_not_undo_neg_Rm
	LD    RR2
	ST    RR0
	CALLF neg_R0
lmul_do_not_undo_neg_Rm:

	BN    TMPBYTE0, 1, lmul_do_not_undo_neg_Rn
	LD    RR1
	SUB   #4
	ST    RR0
	CALLF neg_R0
lmul_do_not_undo_neg_Rn:

	BN    TMPBYTE0, 2, lmul_do_not_undo_neg_ret
	MOV   #WORD_VAL, RR0
	MOV   #8, B
	CALLF neg_R0_counter_in_B
lmul_do_not_undo_neg_ret:

	;time to return result
	POP   RR0
	MOV   #WORD_VAL + 0, RR1
	CALLF cpy_word_R1_to_R0
	
	POP   RR0
	MOV   #WORD_VAL + 4, RR1
	CALLF cpy_word_R1_to_R0
	JMPF  fetch_instr





