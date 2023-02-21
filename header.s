	.include "sfr.i"


;;vector table first

	.org $0000				;;reset vector
	JMPF _vec_start
	
	.org $0003				;;int0 vector
	CLR1 I01CR, 1
	SET1 ARM_IRQS_LO, 0
	RETI

	.org $000B				;;int1 vector
	CLR1 I01CR, 5
	SET1 ARM_IRQS_LO, 1
	RETI

	.org $0013				;;INT2 / T0L.OVF vector
	JMP  irq_int2_t0l_ovf

	.org $001B				;;int3 / base timer vector
	JMP	_vec_int3_or_base_timer_builtin

	.org $0023				;;T0H.OVF vector
	CLR1 T0CON, 3
	SET1 ARM_IRQS_HI, 1
	RETI

	.org $002B				;;T1L.OVF / T1H.OVF vector
	JMP  irq_t1

	.org $0033				;;sio0 vector
	CLR1 SCON0, 0
	SET1 ARM_IRQS_LO, 4
	RETI

	.org $003B				;;sio1 vector
	CLR1 SCON1, 0
	SET1 ARM_IRQS_LO, 5
	RETI

	.org $0043				;;MAPLE vector
	CLR1 $161, 2
	SET1 ARM_IRQS_LO, 6
	RETI

	.org $004B				;;P3 vector
	CLR1 P3INT, 0
	SET1 ARM_IRQS_LO, 7
	RETI

irq_t1:
	BN   T1CNT, 1, irq_t1_not_lo_ovf
	CLR1 T1CNT, 1
	SET1 ARM_IRQS_HI, 2
irq_t1_not_lo_ovf:
	BN   T1CNT, 3, irq_t1_not_hi_ovf
	CLR1 T1CNT, 3
	SET1 ARM_IRQS_HI, 3
irq_t1_not_hi_ovf:
	RETI

irq_int2_t0l_ovf:
	BN   T0CON, 1, irq_not_t0l_ovf
	CLR1 T0CON, 1
	SET1 ARM_IRQS_HI, 0
irq_not_t0l_ovf:
	BN   I23CR, 1, irq_not_int_2
	CLR1 I23CR, 1
	SET1 ARM_IRQS_LO, 2
irq_not_int_2:
	RETI

			
_vec_int3_or_base_timer_builtin:
	PUSH  IE
	CLR1  IE, 7				;;disable ints
	PUSH  ACC
	PUSH  BTCR
	NOT1  EXT, 0
	JMPF  $0130				;returns to 0x139, whih cwill jump to this next label

_vec_int3_or_base_timer_builtin_ret:

	POP   IE				;;restore ints as needed IE
	
	BN   I23CR, 2, irq_not_int_3
	CLR1 I23CR, 2
	SET1 ARM_IRQS_LO, 3
irq_not_int_3:
	POP  ACC

	BN   ACC, 1, irq_not_base_timer_0
	CLR1 BTCR, 1
	SET1 ARM_IRQS_HI, 4
irq_not_base_timer_0:

	BN   ACC, 3, irq_not_base_timer_1
	CLR1 BTCR, 3
	SET1 ARM_IRQS_HI, 5
irq_not_base_timer_1:
	POP  ACC
	
	RETI

vmu_flash_write_guts:
	NOT1  EXT, 0
	JMPF  $E024


	.org $0100				;;api: flash write
API_flash_write:
	NOT1 EXT, 0
	JMPF $0100
	RET

	.org $108
vmu_flash_write:			;params are in ram locations as per api, actually, but are unchecked for correctness, no flash writing icon is shown
							;address must be 128-byte aligned and is in $7D:$7E:$7F, big endian (only bottom bit of $7D is used, only top of $7F)
							;at $7C is type of checking we ant for completion, set to 0x00
							;data is at 0x80..0xFF
							;current data bank (whatever it is) is used
							;B, C, R0, TRL, TRH, ACC are clobbered, very few bytes of stack ar eused (func itself uses 2, our method to clal it another 4)
							;the way this works is a bit of a mess, but ti does. this call places return addr of $10B on stack, guts func jumps to real rom write func, it returns to $10b in rom, which has a "change $10b" there, which jumps here, and that returns to our caller from stack's saved return addr
	CALLF vmu_flash_write_guts
	RET

	.org $0110				;;api: flash verify
API_flash_verify:
	NOT1 EXT, 0
	JMPF $0110
	RET

	.org $0120				;;api: flash read
API_flash_read:
	NOT1 EXT, 0
	JMPF $0120
	RET

	.org $0139				;;return address for ROM base timer handler
	JMP  _vec_int3_or_base_timer_builtin_ret

	.org $0140				;;api: sleep now
API_sleep:
	NOT1 EXT, 0
	JMPF $0140
	RET

	.org $01f0				;;api: end game (noreturn)
API_game_end:
	NOT1 EXT, 0
	JMPF $01f0

;;game header
	.org $0200

	.byte	"uM23 launcher   "
	.byte	"VMU C-M23 emulator by Dmitry.GR "

;;icon header
	.org	$240
	.word	1, 10		;; one frame, animation speed irrelevant (but 10)

;;icon pallette (RGR565, BE)
	.org	$260
	.word	0xffff, 0xf79e, 0x4208, 0x39e7, 0xe71c, 0xd69a
	.word	0xc618, 0xb596, 0xa514, 0x8410, 0x738e, 0x528a

;;icon data (32x32x4bits, BE first)
	.org	$280
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10
	.byte	$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$01,$10,$11,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$00,$00,$01
	.byte	$00,$10,$01,$01,$44,$8a,$b9,$54,$40,$10,$00,$10,$11,$00,$00,$01
	.byte	$11,$11,$01,$41,$6a,$de,$ee,$b8,$11,$10,$00,$00,$10,$01,$00,$01
	.byte	$00,$00,$11,$18,$be,$a7,$23,$ec,$86,$10,$01,$10,$00,$01,$00,$00
	.byte	$10,$00,$16,$9c,$e2,$94,$82,$2d,$d9,$65,$00,$11,$10,$11,$00,$00
	.byte	$00,$15,$7a,$dd,$22,$b4,$5a,$22,$de,$b6,$44,$00,$00,$00,$00,$00
	.byte	$04,$47,$cd,$29,$8c,$c6,$a1,$b2,$3d,$dc,$84,$11,$10,$00,$00,$01
	.byte	$15,$92,$d2,$27,$15,$99,$9a,$82,$33,$de,$c8,$61,$01,$11,$00,$14
	.byte	$7b,$ed,$22,$2b,$59,$75,$7c,$22,$23,$d3,$de,$96,$51,$00,$04,$68
	.byte	$be,$22,$22,$98,$b0,$bb,$92,$c2,$2d,$d3,$dd,$eb,$64,$40,$55,$8d
	.byte	$e3,$32,$22,$c6,$9a,$4b,$22,$22,$2d,$dd,$3d,$dd,$c8,$41,$68,$cd
	.byte	$22,$23,$98,$cb,$0a,$b2,$2c,$c9,$92,$dd,$3d,$dd,$eb,$76,$57,$bd
	.byte	$32,$33,$c7,$9c,$7a,$23,$c2,$89,$6b,$dd,$3d,$dd,$d2,$97,$66,$8f
	.byte	$e2,$22,$3b,$56,$a2,$c2,$33,$53,$70,$7d,$dd,$dd,$fb,$76,$57,$99
	.byte	$bf,$d2,$2c,$cb,$2c,$22,$ab,$ad,$92,$6c,$dd,$e2,$c8,$66,$78,$86
	.byte	$ab,$2d,$22,$22,$22,$27,$85,$bd,$bc,$72,$dd,$fb,$69,$75,$67,$79
	.byte	$96,$ce,$33,$22,$22,$b8,$37,$bc,$76,$bd,$f2,$89,$88,$87,$05,$78
	.byte	$79,$98,$fe,$d3,$22,$c8,$d7,$bb,$cd,$ef,$aa,$88,$96,$64,$00,$46
	.byte	$89,$79,$ab,$fd,$32,$3d,$d6,$a5,$bf,$cc,$87,$a7,$76,$10,$00,$04
	.byte	$67,$8a,$5a,$c2,$ed,$d3,$31,$5c,$dd,$a6,$a8,$87,$50,$00,$00,$00
	.byte	$06,$87,$99,$5c,$e3,$dd,$d6,$bf,$c7,$a8,$88,$61,$10,$00,$00,$00
	.byte	$00,$47,$87,$99,$9e,$fe,$ef,$f9,$a8,$89,$75,$10,$00,$00,$00,$00
	.byte	$00,$04,$68,$96,$9a,$bf,$fb,$b8,$7a,$76,$50,$00,$00,$00,$00,$00
	.byte	$00,$01,$56,$78,$95,$93,$c8,$79,$77,$75,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$01,$68,$77,$99,$88,$78,$86,$00,$01,$11,$10,$11,$00,$00
	.byte	$00,$00,$00,$14,$77,$66,$67,$76,$10,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$01,$11,$11,$11,$44,$01,$00,$01,$10,$10,$00,$00,$00
	.byte	$00,$00,$00,$00,$10,$01,$10,$00,$00,$00,$11,$10,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00


