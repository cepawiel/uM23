

all: core.s defrag.s header.s soc.s ui.s uM23.s
	# ~/bin/waterbear assemble uM23.s -o uM23.vms
	~/bin/aslc86k -I . uM23.s

# 	XOR   ACC						; set acc to 0
# 	ADD   #$A4						; add 1 to acc
# 	ST	  DEBUG_ADDR				; store acc to dbg addr
# loop:
# 	JMP	  loop	