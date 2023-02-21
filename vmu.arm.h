#ifndef _VMU_ARM_H_
#define _VMU_ARM_H_

#include <stdint.h>

#define VMU_BASE_ADDR_CODE		0x00000000ul		//up to 128K - flash rotated so the current executable is at 0x00000, repeats every 128K
#define VMU_BASE_ADDR_RAM1		0x10000000ul		//256 bytes, repeating every 256 btes
#define VMU_BASE_ADDR_RAM2		0x20000000ul		//512 bytes, repeating every 512 bytes
#define VMU_BASE_ADDR_FLASH		0x30000000ul		//128K - the entire flash is viewable here, repeats every 128K
#define VMU_BASE_ADDR_SFRS		0x40000000ul		//LC86K's MMIO regs (accessible ones are documented below), repeats evry 128 bytes
#define VMU_BASE_ADDR_GFX		0x50000000ul		//48x32. MSB is left. 4 bytes of unused memory after each *TWO* rows, repeats every 512 bytes
#define VMU_BASE_ADDR_IRQC		0x60000000ul		//fake IRQ controller. halfword-only accesses, wraps every 256 bytes

struct UM23irqc {
	volatile uint16_t pending;		//0x00	write 1 to clear, 0 writes ignored
	volatile uint16_t irqEna;		//0x02	bit == 1 -> irq on, else ignored ("pending" will still be set)
};

struct SfrsType {
	uint8_t unused1[8];				//0x00
	volatile uint8_t IE;			//0x08
	volatile uint8_t IP;			//0x09
	uint8_t unused2[4];				//0x0A
	volatile uint8_t OCR;			//0x0E
	uint8_t unused3[1];				//0x0F
	volatile uint8_t T0CNT;			//0x10
	volatile uint8_t T0PRR;			//0x11
	volatile uint8_t T0L;			//0x12
	volatile uint8_t T0LR;			//0x13
	volatile uint8_t T0H;			//0x14
	volatile uint8_t T0HR;			//0x15
	uint8_t unused4[2];				//0x16
	volatile uint8_t T1CNT;			//0x18
	uint8_t unused5[1];				//0x19
	volatile uint8_t T1LC;			//0x1A
	volatile uint8_t T1L_R;			//0x1B	//reads as T1L, writes as T1LR
	volatile uint8_t T1HC;			//0x1C
	volatile uint8_t T1H_R;			//0x1D	//reads as T1H, writes as T1HR
	uint8_t unused6[2];				//0x1E
	volatile uint8_t MCR;			//0x20
	uint8_t unused7[1];				//0x21
	volatile uint8_t STAD;			//0x22
	uint8_t unused8[4];				//0x23
	volatile uint8_t VCCR;			//0x27
	uint8_t unused9[8];				//0x28
	volatile uint8_t SCON0;			//0x30
	volatile uint8_t SBUF0;			//0x31
	volatile uint8_t SBR0;			//0x32
	uint8_t unusedA[1];				//0x33
	volatile uint8_t SCON1;			//0x34
	volatile uint8_t SBUF1;			//0x35
	uint8_t unusedB[14];			//0x36
	volatile uint8_t P1;			//0x44
	volatile uint8_t P1DDR;			//0x45
	volatile uint8_t P1FCR;			//0x46
	uint8_t unusedC[5];				//0x47
	volatile uint8_t P3;			//0x4C
	volatile uint8_t P3DDR;			//0x4D
	volatile uint8_t P3INT;			//0x4E
	uint8_t unusedD[13];			//0x4F
	volatile uint8_t P7;			//0x5C
	volatile uint8_t I01CR;			//0x5D
	volatile uint8_t I23CR;			//0x5E
	volatile uint8_t ISL;			//0x5F
	volatile uint8_t MAPLETXRXCTL;	//0x60
	volatile uint8_t MAPLESTA;		//0x61
	volatile uint8_t MAPLERST;		//0x62
	uint8_t unusedE[4];				//0x63
	volatile uint8_t MAPLELEN;		//0x67
	uint8_t unusedF[36];			//0x68
	volatile uint8_t BTCR;			//0x7F
};

#define SFR		((struct SfrsType*)(VMU_BASE_ADDR_SFRS))
#define IRQC	((struct UM23irqc*)(VMU_BASE_ADDR_IRQC))

#endif
