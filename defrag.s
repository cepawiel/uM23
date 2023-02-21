

DEFRAG_NEED_BLOCKS	EQU		$16		; how many blocks do we need?
DEFRAG_FREE_IN_SEQ	EQU		$17		; how mnay in seq have we seen so far
DEFRAG_FREE_FIRST	EQU		$18		; first free block we saw
DEFRAG_PREV_START	EQU		$18		; previosu fule start (overlaps in mem with DEFRAG_FREE_FIRST as they ar enever used at once)
DEFRAG_BEST_FIT		EQU		$19		; where we decided to put the file

defrag:								;ACC has cluster number we care about, R0 points to u16 addr of direntry
									;this is not a generic defragging tool, it will not move files other than the current one
									;the general idea of operation:
									; 0. calculate number of clusters the file needs
									; 1. create a bitmap of clusters that are free
									; 2. find last N free consecutive clusters
									; 3. swap each file block with where it should be in this place
									;this requires free space at least equal to file size. expected. we could do better, but we dont bother
									;if file chain is longer than advertised length, we'll do bad things...
									
									
									;point FLSHCTRL:TRH:TRL to dir entry's "num blocks" value
	INC   RR0
	LD    @R0
	ADD   #$18
	ST    TRL
	DEC   RR0
	LD    @R0
	ADDC  #0
	ST    TRH
	SET1  FLSHCTRL, 0

							;read num blocks and save it	(guaranteed >= 2, else we're not fragmented by definition)
	.byte $50;LDF
	ST    DEFRAG_NEED_BLOCKS


									;find a string of free blocks that matches our needs
	MOV   #$FF, DEFRAG_FREE_FIRST	;sentinel value
	MOV   #$FF, DEFRAG_BEST_FIT

	MOV   #0, C
defrag_calc_bitmap_loop:
	CALLF defrag_fat_entry_get
	LD    RR3
	BNE   #$FF, defrag_block_not_free
	LD    RR2
	BNE   #$FC, defrag_block_not_free
	
defrag_map_block_is_free:
	LD    DEFRAG_FREE_FIRST
	BNE   #$FF, defrag_map_not_first_free_block_seen

defrag_map_first_free_block_seen:
	LD    C
	ST    DEFRAG_FREE_FIRST
	MOV   #1, DEFRAG_FREE_IN_SEQ
	JMPF  defrag_map_free_block_accounted_for
	
defrag_map_not_first_free_block_seen:
	INC   DEFRAG_FREE_IN_SEQ

defrag_map_free_block_accounted_for:
	;at this point DEFRAG_FREE_FIRST is first free block in this row of free blocks, DEFRAG_FREE_IN_SEQ is number of blocks in this row of free blocks
	LD    DEFRAG_FREE_IN_SEQ
	SUB   DEFRAG_NEED_BLOCKS
	BP    PSW, CY, defrag_map_block_processed		;if not enough block to fit our file, go look for more
	BZ    defrag_just_enough_blocks					;if just enough, go record this
	
defrag_too_many_blocks:								;if more than we need (N+1) , pretend we found N block just starting one later
	INC   DEFRAG_FREE_FIRST
	DEC   DEFRAG_FREE_IN_SEQ

defrag_just_enough_blocks:							;now we have just the right number of blocks - record this
	LD    DEFRAG_FREE_FIRST
	ST    DEFRAG_BEST_FIT
	JMP   defrag_map_block_processed
	
defrag_block_not_free:
	MOV   #$FF, DEFRAG_FREE_FIRST

defrag_map_block_processed:
	INC   C
	LD    C
	BNE   #200, defrag_calc_bitmap_loop

									;if we did not find free blocks we need, bail out
	LD    DEFRAG_BEST_FIT
	BNE   #$FF, defrag_fit_found
	RET

defrag_fit_found:					;we have blocks - let's write the file there

	ST    C							;cur dst cluster
	LD    TRL
	AND   #$E0
	ADD   #$02
	ST    TRL
	.byte $50;LDF
	ST    B							;cur src cluster
	ST    DEFRAG_PREV_START			;save it for later
	
defrag_file_copy_loop:
	CALLF defrag_copy_cluster
	INC   C
	PUSH  C
	LD    B
	ST    C
	CALLF defrag_fat_entry_get
	LD    RR3
	; TODO: add check that follows spec better
	; 		currently checks if (nextblock & 0xFF) != 0
	; 		to see if end of chain when it should be
	; 		checking to see if nextblock == 0xFFFA to determine
	; 		doneness. If by chance its 0xFFFC that's probably
	;		not great, but unsure at the moment how to handle
	BNZ   defrag_file_copy_loop_done
	LD    RR2
	ST    B
	POP   C
	JMPF  defrag_file_copy_loop

defrag_file_copy_loop_done:
									;now let's link the fat chain for this file
defrag_file_link_in:
	LD    DEFRAG_BEST_FIT
	ST    C
	LD    DEFRAG_NEED_BLOCKS
	ST    B

defrag_file_link_in_loop:
	LD    B
	BNE   #1, defrag_file_link_in_loop_not_last

defrag_file_link_in_loop_is_last:
	MOV   #$FF, RR3
	MOV   #$FA, RR2
	JMPF  defrag_file_link_in_loop_write_fat

defrag_file_link_in_loop_not_last:
	LD    C
	ADD   #1
	ST    RR2
	MOV   #$00, RR3

defrag_file_link_in_loop_write_fat:
	CALLF defrag_fat_entry_set
	INC   C
	DBNZ  B, defrag_file_link_in_loop

									;now link new file chain into direntry
	LD    TRH
	RORC
	OR    #$80
	ST    C
	LD    TRL
	AND   #$E0
	OR    #$02
	RORC
	ST    B
	MOV   #$00, RR3
	LD    DEFRAG_BEST_FIT
	ST    RR2
	CALLF defrag_write_2_bytes

									;now free the previous chain
	LD    DEFRAG_PREV_START
	
defrag_file_unlink_prev_chain_loop:
	ST    C
	CALLF defrag_fat_entry_get
	PUSH  RR2
	PUSH  RR3
	MOV   #$FF, RR3
	MOV   #$FC, RR2
	CALLF defrag_fat_entry_set
	POP   ACC	;hi
	BNZ   defrag_file_unlink_prev_chain_loop_done
	POP   ACC
	JMP   defrag_file_unlink_prev_chain_loop

defrag_file_unlink_prev_chain_loop_done:
									;all done!
	; this is faster than cleaing up the stack
	; and returning since we'd need to jump back
	; to ui_main at the caller level to 
	; recheck files anyway
	JMPF ui_main 	; restart the program














defrag_clus_and_ofst_to_addr:	;expects cluster number in C, offset (in multiple of 2 bytes) in B
	LD    B
	ROLC
	AND   #$FE
	ST    TRL
	LD    C
	ROLC
	ST    TRH
	AND   #$00
	ROLC
	ST    FLSHCTRL
	RET

defrag_read_128b:				;expects FLSHCTRL:TRH:TRL set up
	PUSH  RR0
	PUSH  TRL
	LD    TRL
	AND   #$80
	ST    TRL
	MOV   #$80, RR0
defrag_read_128b_loop:
	.byte $50;LDF
	ST    @R0
	INC   RR0
	INC   TRL
	BP    RR0, 7, defrag_read_128b_loop
	POP   TRL
	POP   RR0
	RET

defrag_write_128b:				;expects FLSHCTRL:TRH:TRL set up
	PUSH  B
	PUSH  C
	PUSH  RR0
	PUSH  TRL
	PUSH  TRH
	PUSH  FLSHCTRL
	MOV   #$00, $7C
	LD    FLSHCTRL
	ST    $7D
	LD    TRH
	ST    $7E
	LD    TRL
	AND   #$80
	ST    $7F
	CALLF vmu_flash_write
	POP   FLSHCTRL
	POP   TRH
	POP   TRL
	POP   RR0
	POP   C
	POP   B
	RET

defrag_write_2_bytes:			;expects cluster number in B, offset (in multiple of 2 bytes) in C, value in RR2:RR3 (LE)
	PUSH  TRH
	PUSH  TRL
	PUSH  RR0
	CALLF defrag_clus_and_ofst_to_addr
	CALLF defrag_read_128b
	LD    TRL
	OR    #$80
	ST    RR0
	LD    RR2
	ST    @R0
	INC   RR0
	LD    RR3
	ST    @R0
	CALLF defrag_write_128b
	POP   RR0
	POP   TRL
	POP   TRH
	RET

defrag_copy_cluster:			;expects source cluster in B, destination in C
	PUSH  TRH
	PUSH  TRL
	PUSH  RR2
	MOV   #0, RR2

defrag_copy_cluster_loop:
	PUSH  B
	PUSH  C
	LD    B
	ST    C
	LD    RR2
	ROR
	ROR
	ST    B
	PUSH  B
	CALLF defrag_clus_and_ofst_to_addr
	CALLF defrag_read_128b
	POP   B
	POP   C
	CALLF defrag_clus_and_ofst_to_addr
	CALLF defrag_write_128b
	POP   B
	INC   RR2
	BN    RR2, 2, defrag_copy_cluster_loop
	POP   RR2
	POP   TRL
	POP   TRH
	RET

defrag_fat_entry_get:			;cur cluster in C, return in RR2:RR3 (LE)
	PUSH  FLSHCTRL
	PUSH  TRH
	PUSH  TRL
	LD    C
	ROLC
	AND   #$FE
	ST    TRL
	LD    TRH_VAL_FAT
	ADDC  #0
	ST    TRH
	SET1  FLSHCTRL, 0
	.byte $50;LDF
	ST    RR2
	INC   TRL
	.byte $50;LDF
	ST    RR3
	POP   TRL
	POP   TRH
	POP   FLSHCTRL
	RET

defrag_fat_entry_set:			;cluster to set for in C, val to set in RR2:RR3 (LE)
	PUSH  C
	PUSH  B
	LD    C
	ST    B
	LD    TRH_VAL_FAT
	ROR
	OR    #$80					;convert to cluster number
	ST    C
	CALLF defrag_write_2_bytes
	POP   B
	POP   C
	RET


