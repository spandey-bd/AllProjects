//=============================================================================
// defines.inc
//
// This is an include file containing various global defines used by all the
// assembler source modules.
//
//	USE_UNALIGNED		If 1, unaligned word accesses are OK. Speeds up the
//						code considerably if accesses are mostly aligned and
//						only occasionally unaligned.
//	SOFTWARE_DIV		If 1, use software division routines. If 0, use the
//						math coprocessor of Nintendo DS.
//	EXTERNAL_TLB		If 1, the emulation core expects to receive a pointer
//						to an external virtual TLB table, where each 32bit
//						value gives the start address of the corresponding
//						memory block relative to the beginning of the logical
//						emulated memory area. If 0, the emulation core uses
//						internal PC RAM emulation and TLB tables.
//	USE_EGA_MODEX		If 1, the emulation core calls special memory access
//						handlers for many opcodes that (may) access VGA card
//						VRAM. These special handlers need to check the VGA
//						registers to determine the memory plane to read/write.
//						If 0, all memory is treated equally.
//	USE_SEIBUSPI		If 1, the core calls routines in "seibu.S" when the
//						lowest bit of an item in the external TLBTable is set
//						and a memory access is needed.
//	TLB_GRAN_BITS		Granularity of the TLB table, in bits.
//	USE_FPU_OPCODES		If 1, FPU opcodes are supported. Not implemented!
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

#if defined(RPi) || defined(Roku)
#define	USE_UNALIGNED		0
#else
#define	USE_UNALIGNED		1
#endif
#define	SOFTWARE_DIV		1
#define	EXTERNAL_TLB		0
#define	USE_EGA_MODEX		1
#define	USE_SEIBUSPI		0
#define	LDMIA_LOOP			0

#if USE_SEIBUSPI
#define	TLB_GRAN_BITS		11
#else
#define	TLB_GRAN_BITS		14
#endif

#define	USE_FPU_OPCODES		1

#ifdef RPi
#define	USECOM2				1
#endif

#define	REAL_SEGMENT_SHIFT	4

#define	eff_seg				r2
#define	mem_mask			r3
#define	eax					r4
#define	ecx					r5
#define	edx					r6
#define	ebx					r7
#define	esp					r8
#define	ebp					r9
#define	esi					r10
#define	edi					r11

// Flags bits:
// 	bit 0x80000000 = Negative Flag
//	bit 0x40000000 = Zero Flag
//	bit 0x20000000 = Carry flag
//	bit	0x10000000 = Overflow Flag
#define	ARM_NEG		0x80000000
#define	ARM_ZERO	0x40000000
#define	ARM_CARRY	0x20000000
#define	ARM_OVER	0x10000000

#define	REG_FLAGS =registers+(4*9)
#define	REG_ES =registers+(4*10)
#define	REG_CS =registers+(4*11)
#define	REG_SS =registers+(4*12)
#define	REG_DS =registers+(4*13)
#define	REG_FS =registers+(4*14)
#define	REG_GS =registers+(4*15)

// X86 Flags bits:
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
#define		FLAG_AC		0x00040000

#define		FMASK_TEST		(FLAG_CF | FLAG_PF | FLAG_AF | FLAG_ZF | FLAG_SF | FLAG_OF)
#define		FMASK_NORMAL	(FMASK_TEST | FLAG_DF | FLAG_TF | FLAG_IF | FLAG_AC )	
#define		FMASK_ALL		(FMASK_NORMAL | FLAG_IOPL | FLAG_NT)

#define CR0_PROTECTION			0x00000001
#define CR0_MONITORPROCESSOR	0x00000002
#define CR0_FPUEMULATION		0x00000004
#define CR0_TASKSWITCH			0x00000008
#define CR0_FPUPRESENT			0x00000010
#define CR0_PAGING				0x80000000

#define DESC_INVALID				0x00
#define DESC_286_TSS_A				0x01
#define DESC_LDT					0x02
#define DESC_286_TSS_B				0x03
#define DESC_286_CALL_GATE			0x04
#define DESC_TASK_GATE				0x05
#define DESC_286_INT_GATE			0x06
#define DESC_286_TRAP_GATE			0x07

#define DESC_386_TSS_A				0x09
#define DESC_386_TSS_B				0x0b
#define DESC_386_CALL_GATE			0x0c
#define DESC_386_INT_GATE			0x0e
#define DESC_386_TRAP_GATE			0x0f

/* EU/ED Expand UP/DOWN RO/RW Read Only/Read Write NA/A Accessed */
#define DESC_DATA_EU_RO_NA			0x10
#define DESC_DATA_EU_RO_A			0x11
#define DESC_DATA_EU_RW_NA			0x12
#define DESC_DATA_EU_RW_A			0x13
#define DESC_DATA_ED_RO_NA			0x14
#define DESC_DATA_ED_RO_A			0x15
#define DESC_DATA_ED_RW_NA			0x16
#define DESC_DATA_ED_RW_A			0x17

/* N/R Readable  NC/C Confirming A/NA Accessed */
#define DESC_CODE_N_NC_A			0x18
#define DESC_CODE_N_NC_NA			0x19
#define DESC_CODE_R_NC_A			0x1a
#define DESC_CODE_R_NC_NA			0x1b
#define DESC_CODE_N_C_A				0x1c
#define DESC_CODE_N_C_NA			0x1d
#define DESC_CODE_R_C_A				0x1e
#define DESC_CODE_R_C_NA			0x1f

#define	DESC_DATA_OR_CODE_BIT		0x10
#define	DESC_CODE_BIT				0x08
#define	DESC_GATE_32BIT				0x08
#define	DESC_CODE_CONFORMING_BIT	0x04
#define	DESC_CODE_READABLE_BIT		0x02
#define	DESC_DATA_READWRITE_BIT		0x02

#define	IRQ_ON		0
#define	IRQ_OFF		0xFF

#define	R4_INT_HW		1
#define	R4_INT_ERROR	2

#define	EXCEPTION_NP	0x0b

#define	SP_TLBTABLE		(296*4)

#define	SP_R11SAVE		(295*4)
#define	SP_R10SAVE		(294*4)
#define	SP_R9SAVE		(293*4)
#define	SP_R8SAVE		(292*4)
#define	SP_R7SAVE		(291*4)
#define	SP_R6SAVE		(290*4)
#define	SP_R5SAVE		(289*4)
#define	SP_R4SAVE		(288*4)

#define	SP_FREE1		(287*4)
#define	SP_FREE2		(286*4)
#define	SP_FREE3		(285*4)
#define	SP_FREE4		(284*4)
#define	SP_FREE5		(283*4)
#define	SP_STR_SEG		(282*4)

#define	SP_VGA_LATCH	(281*4)
#define	SP_VGA_LATCH_1	(280*4)
#define	SP_VGA_LATCH_2	(279*4)
#define	SP_VGA_LATCH_3	(278*4)

#define	SP_EX_CSIP		(277*4)
#define	SP_SP_MASK		(276*4)

#define	SP_PARITY_BYTE	(275*4+3)
#define	SP_CPU_BIG		(275*4+2)
#define	SP_CPU_CPL		(275*4+1)
#define	SP_CPU_CR0		(275*4)

#define	SP_MASK_16		(274*4)

#define	SP_GS_VALUE		(273*4)
#define	SP_FS_VALUE		(272*4)
#define	SP_DS_VALUE		(271*4)
#define	SP_SS_VALUE		(270*4)
#define	SP_CS_VALUE		(269*4)
#define	SP_ES_VALUE		(268*4)

#define	SP_GS_BASE		(267*4)
#define	SP_FS_BASE		(266*4)
#if !LDMIA_LOOP
#define	SP_DS_BASE		(265*4)
#define	SP_SS_BASE		(264*4)
#endif
#define	SP_CS_BASE		(263*4)
#define	SP_ES_BASE		(262*4)

#define	SP_PHYS_CS		(261*4)
#define	SP_PHYS_SS		(260*4)

#define	SP_FLAGS		(259*4)
#if LDMIA_LOOP
#define	SP_SS_BASE		(258*4)
#define	SP_DS_BASE		(257*4)
#endif
#define	SP_IRQFLAG		(256*4)

