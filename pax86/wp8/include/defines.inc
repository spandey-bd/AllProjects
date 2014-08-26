;=============================================================================
; defines.inc
;
; This is an include file containing various global defines used by all the
; assembler source modules.
;
;	USE_UNALIGNED		If 1, unaligned word accesses are OK. Speeds up the
;						code considerably if accesses are mostly aligned and
;						only occasionally unaligned.
;	SOFTWARE_DIV		If 1, use software division routines. If 0, use the
;						math coprocessor of Nintendo DS.
;	EXTERNAL_TLB		If 1, the emulation core expects to receive a pointer
;						to an external virtual TLB table, where each 32bit
;						value gives the start address of the corresponding
;						memory block relative to the beginning of the logical
;						emulated memory area. If 0, the emulation core uses
;						internal PC RAM emulation and TLB tables.
;	USE_EGA_MODEX		If 1, the emulation core calls special memory access
;						handlers for many opcodes that (may) access VGA card
;						VRAM. These special handlers need to check the VGA
;						registers to determine the memory plane to read/write.
;						If 0, all memory is treated equally.
;	USE_SEIBUSPI		If 1, the core calls routines in "seibu.S" when the
;						lowest bit of an item in the external TLBTable is set
;						and a memory access is needed.
;	TLB_GRAN_BITS		Granularity of the TLB table, in bits.
;	USE_FPU_OPCODES		If 1, FPU opcodes are supported. Not implemented!
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
;
; Copyright (c) 2009-2013 Patrick "Pate" Aalto
;	
; Redistribution and use in source or binary form, with or without modifications,
; is NOT permitted without specific prior written permission from the author.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;=============================================================================

USE_UNALIGNED		EQU		1
SOFTWARE_DIV		EQU		1
EXTERNAL_TLB		EQU		0
USE_EGA_MODEX		EQU		1
USE_SEIBUSPI		EQU		0
LDMIA_LOOP			EQU		0

TLB_GRAN_BITS		EQU		14

USE_FPU_OPCODES		EQU		1

REAL_SEGMENT_SHIFT	EQU		4

eff_seg				RN		2
mem_mask			RN		3
eax					RN		4
ecx					RN		5
edx					RN		6
ebx					RN		7
esp					RN		8
ebp					RN		9
esi					RN		10
edi					RN		11

; Flags bits:
; 	bit 0x80000000 = Negative Flag
;	bit 0x40000000 = Zero Flag
;	bit 0x20000000 = Carry flag
;	bit	0x10000000 = Overflow Flag
ARM_NEG				EQU		0x80000000
ARM_ZERO			EQU		0x40000000
ARM_CARRY			EQU		0x20000000
ARM_OVER			EQU		0x10000000

; X86 Flags bits:
FLAG_CF		EQU		1
FLAG_PF		EQU		(1<<2)
FLAG_AF		EQU		(1<<4)
FLAG_ZF		EQU		(1<<6)
FLAG_SF		EQU		(1<<7)
FLAG_TF		EQU		(1<<8)
FLAG_IF		EQU		(1<<9)
FLAG_DF		EQU		(1<<10)
FLAG_OF		EQU		(1<<11)
FLAG_IOPL	EQU		(3<<12)
FLAG_NT		EQU		(1<<14)
FLAG_VM		EQU		(1<<17)
FLAG_AC		EQU		0x00040000
	
FMASK_TEST		EQU		(FLAG_CF + FLAG_PF + FLAG_AF + FLAG_ZF + FLAG_SF + FLAG_OF)
FMASK_NORMAL	EQU		(FMASK_TEST + FLAG_DF + FLAG_TF + FLAG_IF + FLAG_AC )	
FMASK_ALL		EQU		(FMASK_NORMAL + FLAG_IOPL + FLAG_NT)

CR0_PROTECTION			EQU		0x00000001
CR0_MONITORPROCESSOR	EQU		0x00000002
CR0_FPUEMULATION		EQU		0x00000004
CR0_TASKSWITCH			EQU		0x00000008
CR0_FPUPRESENT			EQU		0x00000010
CR0_PAGING				EQU		0x80000000

DESC_INVALID				EQU		0x00
DESC_286_TSS_A				EQU		0x01
DESC_LDT					EQU		0x02
DESC_286_TSS_B				EQU		0x03
DESC_286_CALL_GATE			EQU		0x04
DESC_TASK_GATE				EQU		0x05
DESC_286_INT_GATE			EQU		0x06
DESC_286_TRAP_GATE			EQU		0x07

DESC_386_TSS_A				EQU		0x09
DESC_386_TSS_B				EQU		0x0b
DESC_386_CALL_GATE			EQU		0x0c
DESC_386_INT_GATE			EQU		0x0e
DESC_386_TRAP_GATE			EQU		0x0f

; EU/ED Expand UP/DOWN RO/RW Read Only/Read Write NA/A Accessed */
DESC_DATA_EU_RO_NA			EQU		0x10
DESC_DATA_EU_RO_A			EQU		0x11
DESC_DATA_EU_RW_NA			EQU		0x12
DESC_DATA_EU_RW_A			EQU		0x13
DESC_DATA_ED_RO_NA			EQU		0x14
DESC_DATA_ED_RO_A			EQU		0x15
DESC_DATA_ED_RW_NA			EQU		0x16
DESC_DATA_ED_RW_A			EQU		0x17

; N/R Readable  NC/C Confirming A/NA Accessed */
DESC_CODE_N_NC_A			EQU		0x18
DESC_CODE_N_NC_NA			EQU		0x19
DESC_CODE_R_NC_A			EQU		0x1a
DESC_CODE_R_NC_NA			EQU		0x1b
DESC_CODE_N_C_A				EQU		0x1c
DESC_CODE_N_C_NA			EQU		0x1d
DESC_CODE_R_C_A				EQU		0x1e
DESC_CODE_R_C_NA			EQU		0x1f

DESC_DATA_OR_CODE_BIT		EQU		0x10
DESC_CODE_BIT				EQU		0x08
DESC_GATE_32BIT				EQU		0x08
DESC_CODE_CONFORMING_BIT	EQU		0x04
DESC_CODE_READABLE_BIT		EQU		0x02
DESC_DATA_READWRITE_BIT		EQU		0x02

IRQ_ON		EQU		0x100000
IRQ_MAXC	EQU		0

R4_INT_HW		EQU		1
R4_INT_ERROR	EQU		2

EXCEPTION_NP	EQU		0x0b

SP_TLBTABLE		EQU		(296*4)

SP_R11SAVE		EQU		(295*4)
SP_R10SAVE		EQU		(294*4)
SP_R9SAVE		EQU		(293*4)
SP_R8SAVE		EQU		(292*4)
SP_R7SAVE		EQU		(291*4)
SP_R6SAVE		EQU		(290*4)
SP_R5SAVE		EQU		(289*4)
SP_R4SAVE		EQU		(288*4)

SP_FREE1		EQU		(287*4)
SP_FREE2		EQU		(286*4)
SP_FREE3		EQU		(285*4)
SP_FREE4		EQU		(284*4)
SP_FREE5		EQU		(283*4)
SP_STR_SEG		EQU		(282*4)

SP_VGA_LATCH	EQU		(281*4)
SP_VGA_LATCH_1	EQU		(280*4)
SP_VGA_LATCH_2	EQU		(279*4)
SP_VGA_LATCH_3	EQU		(278*4)

SP_WAS_CSIP		EQU		(277*4)
SP_SP_MASK		EQU		(276*4)

SP_PARITY_BYTE	EQU		(275*4+3)
SP_CPU_BIG		EQU		(275*4+2)
SP_CPU_CPL		EQU		(275*4+1)
SP_CPU_CR0		EQU		(275*4)

SP_MASK_16		EQU		(274*4)

SP_GS_VALUE		EQU		(273*4)
SP_FS_VALUE		EQU		(272*4)
SP_DS_VALUE		EQU		(271*4)
SP_SS_VALUE		EQU		(270*4)
SP_CS_VALUE		EQU		(269*4)
SP_ES_VALUE		EQU		(268*4)

SP_GS_BASE		EQU		(267*4)
SP_FS_BASE		EQU		(266*4)
SP_DS_BASE		EQU		(265*4)
SP_SS_BASE		EQU		(264*4)
SP_CS_BASE		EQU		(263*4)
SP_ES_BASE		EQU		(262*4)

SP_PHYS_CS		EQU		(261*4)
SP_PHYS_SS		EQU		(260*4)

SP_FLAGS		EQU		(259*4)
SP_IRQMAXC		EQU		(258*4)
SP_EX_CSIP		EQU		(257*4)
SP_IRQFLAG		EQU		(256*4)

	END
