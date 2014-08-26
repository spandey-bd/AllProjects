//=============================================================================
// cpu.h
//
// This is the include file for all C code that needs access to ASM CPU core code.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2013 Patrick "Pate" Aalto
//	
// Redistribution and use in source or binary form, with or without modifications,
// is NOT permitted without specific prior written permission from the author.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//=============================================================================

#ifdef __cplusplus
extern "C" {
#endif

extern int registers[];
extern int *INTVectors;
extern char *BIOSData;
extern char *BIOS_F000;
extern char *EGAVGA_A000;
extern char *CGA_B800;
extern volatile int *IRQFlagAddr;
extern char *BreakReason;

extern int *EMSPageTable[];
#define EMSPages (*EMSPageTable)

#define	PAGE_FLAG_EGA	0x40000000
#define	PAGE_FLAG_MODEX	0x80000000

extern int IRQPending[];
#define	IRQ_ON		0
#define	IRQ_OFF		0xFF
#define	IRQ_TIMER	0
#define	IRQ_KEYB	1
#define	IRQ_PS2		2
#define	IRQ_MOUSE	3
#define	IRQ_COM1	4
#define	IRQ_EXIT	6
#define	IRQ_SB		7


#define	REG_AX 			registers[0]
#define	REG_CX 			registers[1]
#define	REG_DX 			registers[2]
#define	REG_BX 			registers[3]
#define	REG_SP 			registers[4]
#define	REG_BP 			registers[5]
#define	REG_SI 			registers[6]
#define	REG_DI 			registers[7]
#define	REG_EIP			registers[8]
#define	REG_FLAGS 		registers[9]
#define	REG_ES 			registers[10]
#define	REG_CS 			registers[11]
#define	REG_SS 			registers[12]
#define	REG_DS 			registers[13]
#define	REG_FS 			registers[14]
#define	REG_GS 			registers[15]
#define	REG_CURF3TBL 	registers[16]
#define	REG_SCREENSEG 	registers[17]
#define	REG_VRAMBASE 	registers[18]

#define FLAG_NEGATIVE	0x80000000
#define FLAG_ZERO		0x40000000
#define FLAG_CARRY		0x20000000
#define	FLAG_OVERFLOW	0x10000000

#define	AL	(REG_AX&0xFF)
#define	AH	((REG_AX>>8)&0xFF)
#define	BL	(REG_BX&0xFF)
#define	BH	((REG_BX>>8)&0xFF)
#define	CL	(REG_CX&0xFF)
#define	CH	((REG_CX>>8)&0xFF)
#define	DL	(REG_DX&0xFF)
#define	DH	((REG_DX>>8)&0xFF)

#define	AX	(REG_AX&0xFFFF)
#define	BX	(REG_BX&0xFFFF)
#define	CX	(REG_CX&0xFFFF)
#define	DX	(REG_DX&0xFFFF)
#define	SP	(REG_SP&0xFFFF)
#define	BP	(REG_BP&0xFFFF)
#define	SI	(REG_SI&0xFFFF)
#define	DI	(REG_DI&0xFFFF)

#define		FLAG_CF		1
#define		FLAG_PF		(1<<2)
#define		FLAG_AF		(1<<4)
#define		FLAG_ZF		(1<<6)
#define		FLAG_SF		(1<<7)
#define		FLAG_TF		(1<<8)
#define		FLAG_IF		(1<<9)
#define		FLAG_DF		(1<<10)
#define		FLAG_OF		(1<<11)
#define		FLAG_IOPL	(3<<12)
#define		FLAG_NT		(1<<14)
#define		FLAG_VM		(1<<17)

#define	SET_CF	(REG_FLAGS |= FLAG_CF)
#define	CLR_CF	(REG_FLAGS &= ~FLAG_CF)

#define	SET_AL(p) (REG_AX = (REG_AX & 0xFFFFFF00) | ((p)&0xFF))
#define	SET_AH(p) (REG_AX = (REG_AX & 0xFFFF00FF) | (((p)<<8)&0xFF00))
#define	SET_BL(p) (REG_BX = (REG_BX & 0xFFFFFF00) | ((p)&0xFF))
#define	SET_BH(p) (REG_BX = (REG_BX & 0xFFFF00FF) | (((p)<<8)&0xFF00))
#define	SET_CL(p) (REG_CX = (REG_CX & 0xFFFFFF00) | ((p)&0xFF))
#define	SET_CH(p) (REG_CX = (REG_CX & 0xFFFF00FF) | (((p)<<8)&0xFF00))
#define	SET_DL(p) (REG_DX = (REG_DX & 0xFFFFFF00) | ((p)&0xFF))
#define	SET_DH(p) (REG_DX = (REG_DX & 0xFFFF00FF) | (((p)<<8)&0xFF00))
#define	SET_AX(p) (REG_AX = (REG_AX & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_BX(p) (REG_BX = (REG_BX & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_CX(p) (REG_CX = (REG_CX & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_DX(p) (REG_DX = (REG_DX & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_SP(p) (REG_SP = (REG_SP & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_BP(p) (REG_BP = (REG_BP & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_SI(p) (REG_SI = (REG_SI & 0xFFFF0000) | ((p)&0xFFFF))
#define	SET_DI(p) (REG_DI = (REG_DI & 0xFFFF0000) | ((p)&0xFFFF))

#define ENVSTART (BIOSData+0x100+0x1300)
#define PSPSTART (BIOSData+0x100+0x1500)
#define PRGSTART (BIOSData+0x100+0x1600)

extern u8* phys_ptr(int seg, int offs);

#define PHYS_PTR(seg,offs) (phys_ptr(seg, offs))
#define	LOG_SEG(phys) (((int)phys-(int)INTVectors)>>4)

#define DS_DX_PTR ((char *)PHYS_PTR(REG_DS, DX))
#define DS_SI_PTR ((char *)PHYS_PTR(REG_DS, SI))
#define DS_BX_PTR ((char *)PHYS_PTR(REG_DS, BX))
#define ES_DI_PTR ((char *)PHYS_PTR(REG_ES, DI))
#define ES_BX_PTR ((char *)PHYS_PTR(REG_ES, BX))
#define ES_BP_PTR ((char *)PHYS_PTR(REG_ES, BP))
#define ES_DX_PTR ((char *)PHYS_PTR(REG_ES, DX))

	int run_core(char mode);
	void IRQRequest(int num);
	
#ifdef __cplusplus
}
#endif
