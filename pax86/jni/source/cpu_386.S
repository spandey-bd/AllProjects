//=============================================================================
// cpu_386.s
//
// This file contains normal opcode handlers used when running in a USE32 segment
// (by default 32-bit addressing and 32-bit operands).
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

#ifdef RPi
	.arch armv6
#else
	.arch armv5te
#endif	
	.file	"cpu_386.s"

#define	S2TEST	0

#include "defines.inc"
#include "macros.inc"
#include "gen_32.inc"

	.text
	.align	2

// ------------------- 00 = ADD r/m8, r8 -------------------------------
//
op_00_USE32:
	//-------
	// Check if we are to handle an interrupt instead of op_00
	//-------
	mrs		r1,cpsr								// Save flags to r1. We don't need to restore them if not an IRQ
	cmp		r0, #0								// Do we really need to handle opcode 00 instead of an IRQ?
	bne		IRQStart_r1							// Nope, we need to handle an IRQ instead.
	//-------
	// Handle opcode 00.
	//-------
	modrm_jump_32
	modrm_tbl_0 add
	.word add_al_al, add_cl_al, add_dl_al, add_bl_al, add_ah_al, add_ch_al, add_dh_al, add_bh_al
	.word add_al_cl, add_cl_cl, add_dl_cl, add_bl_cl, add_ah_cl, add_ch_cl, add_dh_cl, add_bh_cl
	.word add_al_dl, add_cl_dl, add_dl_dl, add_bl_dl, add_ah_dl, add_ch_dl, add_dh_dl, add_bh_dl
	.word add_al_bl, add_cl_bl, add_dl_bl, add_bl_bl, add_ah_bl, add_ch_bl, add_dh_bl, add_bh_bl
	.word add_al_ah, add_cl_ah, add_dl_ah, add_bl_ah, add_ah_ah, add_ch_ah, add_dh_ah, add_bh_ah
	.word add_al_ch, add_cl_ch, add_dl_ch, add_bl_ch, add_ah_ch, add_ch_ch, add_dh_ch, add_bh_ch
	.word add_al_dh, add_cl_dh, add_dl_dh, add_bl_dh, add_ah_dh, add_ch_dh, add_dh_dh, add_bh_dh
	.word add_al_bh, add_cl_bh, add_dl_bh, add_bl_bh, add_ah_bh, add_ch_bh, add_dh_bh, add_bh_bh

	modrm_0_genall add

// ------------------- 01 = ADD r/m32, r32 -----------------------------
op_01_USE32:
	modrm_jump_32
	modrm_tbl_1 add

.macro add_t0_r32 reg
	.global	add_t0_r32_bp_\reg
add_t0_r32_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	add_t0_r32_\reg
add_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	// t0 contains the memory offset to add to eff_seg.
	// t3 contains mask, either 0x0000FFFF or 0xFFFFFFFF to select between 16-bit and 32-bit memory addressing.
	//-------
	mem_handler_jump_r0r3 .op_01_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_01_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	adds	r0, \reg
	str		r0, [r2]
	b		loop
#else
#if USE_SEIBUSPI
	//-------
	// Raiden Fighters Jet, statistics after running two demo games:
	// Aligned RAM access: 1325171 times, unaligned RAM access: 2 times.
	// It is worth to test for aligned access and use faster code in that situation.
	//-------
	tst		r2, #3
	bne		1f
	ldr		r0, [r2]
	adds	r0, \reg
	str		r0, [r2]
	b		loop
1:	
#endif	
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	adds	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
	b		loop
#endif
.endm

	add_t0_r32 eax
	add_t0_r32 ecx
	add_t0_r32 edx
	add_t0_r32 ebx
	add_t0_r32 esp
	add_t0_r32 ebp
	add_t0_r32 esi
	add_t0_r32 edi

	modrm_1_genall add add_t0_r32

// ------------------- 02 = ADD r8, r/m8 -------------------------------
op_02_USE32:
	modrm_jump_32
	modrm_tbl_2 add
	.word add_al_al, add_al_cl, add_al_dl, add_al_bl, add_al_ah, add_al_ch, add_al_dh, add_al_bh
	.word add_cl_al, add_cl_cl, add_cl_dl, add_cl_bl, add_cl_ah, add_cl_ch, add_cl_dh, add_cl_bh
	.word add_dl_al, add_dl_cl, add_dl_dl, add_dl_bl, add_dl_ah, add_dl_ch, add_dl_dh, add_dl_bh
	.word add_bl_al, add_bl_cl, add_bl_dl, add_bl_bl, add_bl_ah, add_bl_ch, add_bl_dh, add_bl_bh
	.word add_ah_al, add_ah_cl, add_ah_dl, add_ah_bl, add_ah_ah, add_ah_ch, add_ah_dh, add_ah_bh
	.word add_ch_al, add_ch_cl, add_ch_dl, add_ch_bl, add_ch_ah, add_ch_ch, add_ch_dh, add_ch_bh
	.word add_dh_al, add_dh_cl, add_dh_dl, add_dh_bl, add_dh_ah, add_dh_ch, add_dh_dh, add_dh_bh
	.word add_bh_al, add_bh_cl, add_bh_dl, add_bh_bl, add_bh_ah, add_bh_ch, add_bh_dh, add_bh_bh

	modrm_2_genall add

// ------------------- 03 = ADD r32, r/m32 ----------------------------
// 6667030590140000 = add eax,[00001490]
//
	.global	op_03_USE32
op_03_USE32:
	modrm_jump_32
	modrm_tbl_3 add

.macro add_r32_t0 reg
	.global	add_r32_t0_bp_\reg
add_r32_t0_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	add_r32_t0_\reg
add_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	// t0 contains the memory offset to add to eff_seg.
	// t3 contains mask, either 0x0000FFFF or 0xFFFFFFFF to select between 16-bit and 32-bit memory addressing.
	//-------
	mem_handler_jump_r0r3 .op_03_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_03_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	adds	\reg, r0
	b		loop
.endm

	add_r32_t0 eax
	add_r32_t0 ecx
	add_r32_t0 edx
	add_r32_t0 ebx
	add_r32_t0 esp
	add_r32_t0 ebp
	add_r32_t0 esi
	add_r32_t0 edi

.macro add_reg32_reg32 rl rr
	adds	\rl, \rr
	b		loop
.endm

	modrm_3_genall add add_r32_t0

// ------------------- 05 = ADD EAX,imm32 ------------------------------
	.global	op_05_USE32
op_05_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	adds	eax, r0
	b		loop

// ------------------- 06 = PUSH ES -----------------------------------
	.global	op_06_USE32
op_06_USE32:
	ldr		r1, [sp, #SP_ES_VALUE]	
	push_dword r1 r0 r2
	b		loop

// ------------------- 07 = POP ES ------------------------------------
	.global	op_07_USE32
op_07_USE32:									// For REAL MODE!
	pop_dword r2 r0 r1
	lsl		r2, #16								// Only use the low 16 bits
	lsr		r2, #16
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		mov_es_r0r2_prot					// Yes we are, go handle protected mode version!
	//-------
	// We are in real mode, so use the simple handling.
	//-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_ES_VALUE]	
	str		r1, [sp, #SP_ES_BASE]	
	b		restore_flags_from_r0				// Go back to the opcode loop, restoring flags

// ------------------- 08 = OR r/m8, r8 -------------------------------
//
op_08_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_0 or
	.word or_al_al, or_cl_al, or_dl_al, or_bl_al, or_ah_al, or_ch_al, or_dh_al, or_bh_al
	.word or_al_cl, or_cl_cl, or_dl_cl, or_bl_cl, or_ah_cl, or_ch_cl, or_dh_cl, or_bh_cl
	.word or_al_dl, or_cl_dl, or_dl_dl, or_bl_dl, or_ah_dl, or_ch_dl, or_dh_dl, or_bh_dl
	.word or_al_bl, or_cl_bl, or_dl_bl, or_bl_bl, or_ah_bl, or_ch_bl, or_dh_bl, or_bh_bl
	.word or_al_ah, or_cl_ah, or_dl_ah, or_bl_ah, or_ah_ah, or_ch_ah, or_dh_ah, or_bh_ah
	.word or_al_ch, or_cl_ch, or_dl_ch, or_bl_ch, or_ah_ch, or_ch_ch, or_dh_ch, or_bh_ch
	.word or_al_dh, or_cl_dh, or_dl_dh, or_bl_dh, or_ah_dh, or_ch_dh, or_dh_dh, or_bh_dh
	.word or_al_bh, or_cl_bh, or_dl_bh, or_bl_bh, or_ah_bh, or_ch_bh, or_dh_bh, or_bh_bh

	modrm_0_genall or

// ------------------- 09 = OR r/m32, r32 -----------------------------
//
op_09_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 or

.macro or_t0_reg reg
	.global	or_t0_r32_bp_\reg
or_t0_r32_bp_\reg:
	mem_handler_bp
	.global	or_t0_r32_\reg
or_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	// t0 contains the memory offset to add to eff_seg.
	// t3 contains mask, either 0x0000FFFF or 0xFFFFFFFF to select between 16-bit and 32-bit memory addressing.
	//-------
	mem_handler_jump_r0r3 .op_09_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_09_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	orrs	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	orrs	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		loop
.endm

	or_t0_reg eax
	or_t0_reg ecx
	or_t0_reg edx
	or_t0_reg ebx
	or_t0_reg esp
	or_t0_reg ebp
	or_t0_reg esi
	or_t0_reg edi

	modrm_1_genall or or_t0_r32

// ------------------- 0A = OR r8, r/m8 -------------------------------
//
op_0a_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_2 or
	.word or_al_al, or_al_cl, or_al_dl, or_al_bl, or_al_ah, or_al_ch, or_al_dh, or_al_bh
	.word or_cl_al, or_cl_cl, or_cl_dl, or_cl_bl, or_cl_ah, or_cl_ch, or_cl_dh, or_cl_bh
	.word or_dl_al, or_dl_cl, or_dl_dl, or_dl_bl, or_dl_ah, or_dl_ch, or_dl_dh, or_dl_bh
	.word or_bl_al, or_bl_cl, or_bl_dl, or_bl_bl, or_bl_ah, or_bl_ch, or_bl_dh, or_bl_bh
	.word or_ah_al, or_ah_cl, or_ah_dl, or_ah_bl, or_ah_ah, or_ah_ch, or_ah_dh, or_ah_bh
	.word or_ch_al, or_ch_cl, or_ch_dl, or_ch_bl, or_ch_ah, or_ch_ch, or_ch_dh, or_ch_bh
	.word or_dh_al, or_dh_cl, or_dh_dl, or_dh_bl, or_dh_ah, or_dh_ch, or_dh_dh, or_dh_bh
	.word or_bh_al, or_bh_cl, or_bh_dl, or_bh_bl, or_bh_ah, or_bh_ch, or_bh_dh, or_bh_bh

	modrm_2_genall or

// ------------------- 0B = OR r32, r/m32 -----------------------------
//
op_0b_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 or

.macro or_r32_t0 reg
	.global	or_r32_t0_bp_\reg
or_r32_t0_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	or_r32_t0_\reg
or_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_0b_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_0b_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	orrs	\reg, r0
	b		loop
.endm

	or_r32_t0 eax
	or_r32_t0 ecx
	or_r32_t0 edx
	or_r32_t0 ebx
	or_r32_t0 esp
	or_r32_t0 ebp
	or_r32_t0 esi
	or_r32_t0 edi

.macro or_reg32_reg32 rl rr
	orrs	\rl, \rr
	b		loop						// Back to loop
.endm

	modrm_3_genall or or_r32_t0

// ------------------- 0D = OR EAX,imm32 -------------------------------
	.global	op_0d_USE32
op_0d_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	orrs	eax, r0
	b		loop


// ------------------- 0E = PUSH CS ------------------------------------

	.global	op_0e_USE32
op_0e_USE32:
	ldr		r1, [sp, #SP_CS_VALUE]	
	push_dword r1 r0 r2
	b		loop

// ------------------- 0F = various opcodes ----------------------------
// op_0f = see "cpu_0F_USE32.S"

// ------------------- 10 = ADC r/m8, r8 -------------------------------
//
	.global	op_10_USE32
op_10_USE32:
	modrm_jump_32
	modrm_tbl_0 adc
	.word adc_al_al, adc_cl_al, adc_dl_al, adc_bl_al, adc_ah_al, adc_ch_al, adc_dh_al, adc_bh_al
	.word adc_al_cl, adc_cl_cl, adc_dl_cl, adc_bl_cl, adc_ah_cl, adc_ch_cl, adc_dh_cl, adc_bh_cl
	.word adc_al_dl, adc_cl_dl, adc_dl_dl, adc_bl_dl, adc_ah_dl, adc_ch_dl, adc_dh_dl, adc_bh_dl
	.word adc_al_bl, adc_cl_bl, adc_dl_bl, adc_bl_bl, adc_ah_bl, adc_ch_bl, adc_dh_bl, adc_bh_bl
	.word adc_al_ah, adc_cl_ah, adc_dl_ah, adc_bl_ah, adc_ah_ah, adc_ch_ah, adc_dh_ah, adc_bh_ah
	.word adc_al_ch, adc_cl_ch, adc_dl_ch, adc_bl_ch, adc_ah_ch, adc_ch_ch, adc_dh_ch, adc_bh_ch
	.word adc_al_dh, adc_cl_dh, adc_dl_dh, adc_bl_dh, adc_ah_dh, adc_ch_dh, adc_dh_dh, adc_bh_dh
	.word adc_al_bh, adc_cl_bh, adc_dl_bh, adc_bl_bh, adc_ah_bh, adc_ch_bh, adc_dh_bh, adc_bh_bh

	modrm_0_genall adc

// ------------------- 11 = ADC r/m32, r32 -----------------------------
//
	.global	op_11_USE32
op_11_USE32:
	modrm_jump_32
	modrm_tbl_1 adc

.macro adc_t0_reg reg
	.global	adc_t0_r32_bp_\reg
adc_t0_r32_bp_\reg:
	mem_handler_bp
	.global	adc_t0_r32_\reg
adc_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	// t0 contains the memory offset to add to eff_seg.
	// t3 contains mask, either 0x0000FFFF or 0xFFFFFFFF to select between 16-bit and 32-bit memory addressing.
	//-------
	mem_handler_jump_r0r3 .op_11_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_11_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	adcs	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	adcs	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		loop
.endm

	adc_t0_reg eax
	adc_t0_reg ecx
	adc_t0_reg edx
	adc_t0_reg ebx
	adc_t0_reg esp
	adc_t0_reg ebp
	adc_t0_reg esi
	adc_t0_reg edi

	modrm_1_genall adc adc_t0_r32

// ------------------- 12 = ADC r8, r/m8 ------------------------------
//
	.global	op_12_USE32
op_12_USE32:
	modrm_jump_32
	modrm_tbl_2 adc
	.word adc_al_al, adc_al_cl, adc_al_dl, adc_al_bl, adc_al_ah, adc_al_ch, adc_al_dh, adc_al_bh
	.word adc_cl_al, adc_cl_cl, adc_cl_dl, adc_cl_bl, adc_cl_ah, adc_cl_ch, adc_cl_dh, adc_cl_bh
	.word adc_dl_al, adc_dl_cl, adc_dl_dl, adc_dl_bl, adc_dl_ah, adc_dl_ch, adc_dl_dh, adc_dl_bh
	.word adc_bl_al, adc_bl_cl, adc_bl_dl, adc_bl_bl, adc_bl_ah, adc_bl_ch, adc_bl_dh, adc_bl_bh
	.word adc_ah_al, adc_ah_cl, adc_ah_dl, adc_ah_bl, adc_ah_ah, adc_ah_ch, adc_ah_dh, adc_ah_bh
	.word adc_ch_al, adc_ch_cl, adc_ch_dl, adc_ch_bl, adc_ch_ah, adc_ch_ch, adc_ch_dh, adc_ch_bh
	.word adc_dh_al, adc_dh_cl, adc_dh_dl, adc_dh_bl, adc_dh_ah, adc_dh_ch, adc_dh_dh, adc_dh_bh
	.word adc_bh_al, adc_bh_cl, adc_bh_dl, adc_bh_bl, adc_bh_ah, adc_bh_ch, adc_bh_dh, adc_bh_bh

	modrm_2_genall adc

// ------------------- 13 = ADC r32, r/m32 ------------------------------
//
op_13_USE32:
	modrm_jump_32
	modrm_tbl_3 adc

.macro adc_r32_t0 reg
	.global	adc_r32_t0_bp_\reg
adc_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	adc_r32_t0_\reg
adc_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_13_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_13_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	adcs	\reg, r0
	b		loop
.endm

	adc_r32_t0 eax
	adc_r32_t0 ecx
	adc_r32_t0 edx
	adc_r32_t0 ebx
	adc_r32_t0 esp
	adc_r32_t0 ebp
	adc_r32_t0 esi
	adc_r32_t0 edi

.macro adc_reg32_reg32 rl rr
	adcs	\rl, \rr
	b		loop
.endm

	modrm_3_genall adc adc_r32_t0

// ------------------- 15 = ADC EAX,imm32 ------------------------------
	.global	op_15_USE32
op_15_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	adcs	eax, r0
	b		loop

// ------------------- 16 = PUSH SS -----------------------------------
	.global	op_16_USE32
op_16_USE32:
	ldr		r1, [sp, #SP_SS_VALUE]	
	push_dword r1 r0 r2
	b		loop

// ------------------- 17 = POP SS ------------------------------------
	.global	op_17_USE32
op_17_USE32:									// For REAL MODE!
	pop_dword r2 r0 r1
	lsl		r2, #16								// Only use the low 16 bits
	lsr		r2, #16
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		mov_ss_r0r2_prot					// Yes we are, go handle protected mode version!
	msr		cpsr_f, r0							// Restore flags
	//-------
	// We are in real mode, so use the simple handling.
	//-------
	str		r2, [sp, #SP_SS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_SS_BASE]
	mov		lr, r2								// Set the default segment base for BP-relative addressing
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]
	//-------
	// NOTE! x86 disables interrupts until the next instruction has been executed.
	// Thus we must handle the next opcode immediately!
	//-------
	ldrb	r0, [r12], #1						// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				// r2 = logical DS segment
	str		r12, [sp, #SP_EX_CSIP]				// Remember where this opcode started, for division-by-zero and exception handling
	ldr		pc,[sp, r0, lsl #2]					// Jump to the opcode handler

// ------------------- 18 = SBB r/m8, r8 -------------------------------
//
	.global	op_18_USE32
op_18_USE32:
	modrm_jump_32
	modrm_tbl_0 sbb
	.word sbb_al_al, sbb_cl_al, sbb_dl_al, sbb_bl_al, sbb_ah_al, sbb_ch_al, sbb_dh_al, sbb_bh_al
	.word sbb_al_cl, sbb_cl_cl, sbb_dl_cl, sbb_bl_cl, sbb_ah_cl, sbb_ch_cl, sbb_dh_cl, sbb_bh_cl
	.word sbb_al_dl, sbb_cl_dl, sbb_dl_dl, sbb_bl_dl, sbb_ah_dl, sbb_ch_dl, sbb_dh_dl, sbb_bh_dl
	.word sbb_al_bl, sbb_cl_bl, sbb_dl_bl, sbb_bl_bl, sbb_ah_bl, sbb_ch_bl, sbb_dh_bl, sbb_bh_bl
	.word sbb_al_ah, sbb_cl_ah, sbb_dl_ah, sbb_bl_ah, sbb_ah_ah, sbb_ch_ah, sbb_dh_ah, sbb_bh_ah
	.word sbb_al_ch, sbb_cl_ch, sbb_dl_ch, sbb_bl_ch, sbb_ah_ch, sbb_ch_ch, sbb_dh_ch, sbb_bh_ch
	.word sbb_al_dh, sbb_cl_dh, sbb_dl_dh, sbb_bl_dh, sbb_ah_dh, sbb_ch_dh, sbb_dh_dh, sbb_bh_dh
	.word sbb_al_bh, sbb_cl_bh, sbb_dl_bh, sbb_bl_bh, sbb_ah_bh, sbb_ch_bh, sbb_dh_bh, sbb_bh_bh

	modrm_0_genall sbb

// ------------------- 19 = SBB r/m32, r32 -----------------------------
//
op_19_USE32:
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_32
	modrm_tbl_1 sbb

.macro sbb_t0_r32 reg
	.global	sbb_t0_r32_bp_\reg
sbb_t0_r32_bp_\reg:
	mem_handler_bp
	.global	sbb_t0_r32_\reg
sbb_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_19_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_19_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	sbcs	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	sbcs	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		complement_carry
.endm

	sbb_t0_r32 eax
	sbb_t0_r32 ecx
	sbb_t0_r32 edx
	sbb_t0_r32 ebx
	sbb_t0_r32 esp
	sbb_t0_r32 ebp
	sbb_t0_r32 esi
	sbb_t0_r32 edi

	modrm_1_genall sbb sbb_t0_r32

// ------------------- 1A = SBB r8, r/m8 -------------------------------
//
	.global	op_1a_USE32
op_1a_USE32:
	modrm_jump_32
	modrm_tbl_2 sbb
	.word sbb_al_al, sbb_al_cl, sbb_al_dl, sbb_al_bl, sbb_al_ah, sbb_al_ch, sbb_al_dh, sbb_al_bh
	.word sbb_cl_al, sbb_cl_cl, sbb_cl_dl, sbb_cl_bl, sbb_cl_ah, sbb_cl_ch, sbb_cl_dh, sbb_cl_bh
	.word sbb_dl_al, sbb_dl_cl, sbb_dl_dl, sbb_dl_bl, sbb_dl_ah, sbb_dl_ch, sbb_dl_dh, sbb_dl_bh
	.word sbb_bl_al, sbb_bl_cl, sbb_bl_dl, sbb_bl_bl, sbb_bl_ah, sbb_bl_ch, sbb_bl_dh, sbb_bl_bh
	.word sbb_ah_al, sbb_ah_cl, sbb_ah_dl, sbb_ah_bl, sbb_ah_ah, sbb_ah_ch, sbb_ah_dh, sbb_ah_bh
	.word sbb_ch_al, sbb_ch_cl, sbb_ch_dl, sbb_ch_bl, sbb_ch_ah, sbb_ch_ch, sbb_ch_dh, sbb_ch_bh
	.word sbb_dh_al, sbb_dh_cl, sbb_dh_dl, sbb_dh_bl, sbb_dh_ah, sbb_dh_ch, sbb_dh_dh, sbb_dh_bh
	.word sbb_bh_al, sbb_bh_cl, sbb_bh_dl, sbb_bh_bl, sbb_bh_ah, sbb_bh_ch, sbb_bh_dh, sbb_bh_bh

	modrm_2_genall sbb

// ------------------- 1B = SBB r32, r/m32 ------------------------------
//
op_1b_USE32:
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_32
	modrm_tbl_3 sbb

.macro sbb_r32_t0 reg
	.global	sbb_r32_t0_bp_\reg
sbb_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	sbb_r32_t0_\reg
sbb_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_1b_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_1b_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	sbcs	\reg, r0
	b		complement_carry
.endm

	sbb_r32_t0 eax
	sbb_r32_t0 ecx
	sbb_r32_t0 edx
	sbb_r32_t0 ebx
	sbb_r32_t0 esp
	sbb_r32_t0 ebp
	sbb_r32_t0 esi
	sbb_r32_t0 edi

.macro sbb_reg32_reg32 rl rr
	sbcs	\rl, \rr
	b		complement_carry
.endm

	modrm_3_genall sbb sbb_r32_t0

// ------------------- 1D = SBB EAX,imm32 ------------------------------
	.global	op_1d_USE32
op_1d_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
#else
	mrs		r0,cpsr
	eor		r0, r0, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r0
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	sbcs	eax, r0
	b		complement_carry

// ------------------- 1E = PUSH DS ------------------------------------
	.global	op_1e_USE32
op_1e_USE32:
	ldr		r1, [sp, #SP_DS_VALUE]	
	push_dword r1 r0 r2
	b		loop

// ------------------- 1F = POP DS ------------------------------------
	.global	op_1f_USE32
op_1f_USE32:									// For REAL MODE!
	pop_dword r2 r0 r1
	lsl		r2, #16								// Only use the low 16 bits
	lsr		r2, #16
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		mov_ds_r0r2_prot					// Yes we are, go handle protected mode version!
	//-------
	// We are in real mode, so use the simple handling.
	//-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_DS_VALUE]
	str		r1, [sp, #SP_DS_BASE]
	b		restore_flags_from_r0				// Go back to the opcode loop, restoring flags

// ------------------- 20 = AND r/m8, r8 -------------------------------
//
	.global	op_20_USE32
op_20_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_0 and
	.word and_al_al, and_cl_al, and_dl_al, and_bl_al, and_ah_al, and_ch_al, and_dh_al, and_bh_al
	.word and_al_cl, and_cl_cl, and_dl_cl, and_bl_cl, and_ah_cl, and_ch_cl, and_dh_cl, and_bh_cl
	.word and_al_dl, and_cl_dl, and_dl_dl, and_bl_dl, and_ah_dl, and_ch_dl, and_dh_dl, and_bh_dl
	.word and_al_bl, and_cl_bl, and_dl_bl, and_bl_bl, and_ah_bl, and_ch_bl, and_dh_bl, and_bh_bl
	.word and_al_ah, and_cl_ah, and_dl_ah, and_bl_ah, and_ah_ah, and_ch_ah, and_dh_ah, and_bh_ah
	.word and_al_ch, and_cl_ch, and_dl_ch, and_bl_ch, and_ah_ch, and_ch_ch, and_dh_ch, and_bh_ch
	.word and_al_dh, and_cl_dh, and_dl_dh, and_bl_dh, and_ah_dh, and_ch_dh, and_dh_dh, and_bh_dh
	.word and_al_bh, and_cl_bh, and_dl_bh, and_bl_bh, and_ah_bh, and_ch_bh, and_dh_bh, and_bh_bh

	modrm_0_genall and

// ------------------- 21 = AND r/m32, r32 -----------------------------
//
op_21_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 and

.macro and_t0_r32 reg
	.global	and_t0_r32_bp_\reg
and_t0_r32_bp_\reg:
	mem_handler_bp
	.global	and_t0_r32_\reg
and_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_21_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_21_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	ands	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	ands	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		loop
.endm

	and_t0_r32 eax
	and_t0_r32 ecx
	and_t0_r32 edx
	and_t0_r32 ebx
	and_t0_r32 esp
	and_t0_r32 ebp
	and_t0_r32 esi
	and_t0_r32 edi

	modrm_1_genall and and_t0_r32

// ------------------- 22 = AND r8, r/m8 ------------------------------
//
op_22_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_2 and
	.word and_al_al, and_al_cl, and_al_dl, and_al_bl, and_al_ah, and_al_ch, and_al_dh, and_al_bh
	.word and_cl_al, and_cl_cl, and_cl_dl, and_cl_bl, and_cl_ah, and_cl_ch, and_cl_dh, and_cl_bh
	.word and_dl_al, and_dl_cl, and_dl_dl, and_dl_bl, and_dl_ah, and_dl_ch, and_dl_dh, and_dl_bh
	.word and_bl_al, and_bl_cl, and_bl_dl, and_bl_bl, and_bl_ah, and_bl_ch, and_bl_dh, and_bl_bh
	.word and_ah_al, and_ah_cl, and_ah_dl, and_ah_bl, and_ah_ah, and_ah_ch, and_ah_dh, and_ah_bh
	.word and_ch_al, and_ch_cl, and_ch_dl, and_ch_bl, and_ch_ah, and_ch_ch, and_ch_dh, and_ch_bh
	.word and_dh_al, and_dh_cl, and_dh_dl, and_dh_bl, and_dh_ah, and_dh_ch, and_dh_dh, and_dh_bh
	.word and_bh_al, and_bh_cl, and_bh_dl, and_bh_bl, and_bh_ah, and_bh_ch, and_bh_dh, and_bh_bh

	modrm_2_genall and

// ------------------- 23 = AND r32, r/m32 ------------------------------
//
op_23_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 and

.macro and_r32_t0 reg
	.global	and_r32_t0_bp_\reg
and_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	and_r32_t0_\reg
and_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_23_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_23_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
#endif
	ands	\reg, r0
	b		loop
.endm

	and_r32_t0 eax
	and_r32_t0 ecx
	and_r32_t0 edx
	and_r32_t0 ebx
	and_r32_t0 esp
	and_r32_t0 ebp
	and_r32_t0 esi
	and_r32_t0 edi

.macro and_reg32_reg32 rl rr
	ands	\rl, \rr
	b		loop
.endm

	modrm_3_genall and and_r32_t0

// ------------------- 25 = AND EAX,imm32 ------------------------------
	.global	op_25_USE32
op_25_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	ands	eax, r0
	b		loop

// ------------------- 28 = SUB r/m8, r8 -------------------------------
//
op_28_USE32:
	modrm_jump_32
	modrm_tbl_0 sub
	.word sub_al_al, sub_cl_al, sub_dl_al, sub_bl_al, sub_ah_al, sub_ch_al, sub_dh_al, sub_bh_al
	.word sub_al_cl, sub_cl_cl, sub_dl_cl, sub_bl_cl, sub_ah_cl, sub_ch_cl, sub_dh_cl, sub_bh_cl
	.word sub_al_dl, sub_cl_dl, sub_dl_dl, sub_bl_dl, sub_ah_dl, sub_ch_dl, sub_dh_dl, sub_bh_dl
	.word sub_al_bl, sub_cl_bl, sub_dl_bl, sub_bl_bl, sub_ah_bl, sub_ch_bl, sub_dh_bl, sub_bh_bl
	.word sub_al_ah, sub_cl_ah, sub_dl_ah, sub_bl_ah, sub_ah_ah, sub_ch_ah, sub_dh_ah, sub_bh_ah
	.word sub_al_ch, sub_cl_ch, sub_dl_ch, sub_bl_ch, sub_ah_ch, sub_ch_ch, sub_dh_ch, sub_bh_ch
	.word sub_al_dh, sub_cl_dh, sub_dl_dh, sub_bl_dh, sub_ah_dh, sub_ch_dh, sub_dh_dh, sub_bh_dh
	.word sub_al_bh, sub_cl_bh, sub_dl_bh, sub_bl_bh, sub_ah_bh, sub_ch_bh, sub_dh_bh, sub_bh_bh

	modrm_0_genall sub

// ------------------- 29 = SUB r/m32, r32 -----------------------------
op_29_USE32:
	modrm_jump_32
	modrm_tbl_1 sub

.macro sub_t0_r32 reg
	.global	sub_t0_r32_bp_\reg
sub_t0_r32_bp_\reg:
	mem_handler_bp
	.global	sub_t0_r32_\reg
sub_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_29_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_29_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	subs	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	subs	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		complement_carry
.endm

	sub_t0_r32 eax
	sub_t0_r32 ecx
	sub_t0_r32 edx
	sub_t0_r32 ebx
	sub_t0_r32 esp
	sub_t0_r32 ebp
	sub_t0_r32 esi
	sub_t0_r32 edi

	modrm_1_genall sub sub_t0_r32

// ------------------- 2A = SUB r8, r/m8 -------------------------------
//
op_2a_USE32:
	modrm_jump_32
	modrm_tbl_2 sub
	.word sub_al_al, sub_al_cl, sub_al_dl, sub_al_bl, sub_al_ah, sub_al_ch, sub_al_dh, sub_al_bh
	.word sub_cl_al, sub_cl_cl, sub_cl_dl, sub_cl_bl, sub_cl_ah, sub_cl_ch, sub_cl_dh, sub_cl_bh
	.word sub_dl_al, sub_dl_cl, sub_dl_dl, sub_dl_bl, sub_dl_ah, sub_dl_ch, sub_dl_dh, sub_dl_bh
	.word sub_bl_al, sub_bl_cl, sub_bl_dl, sub_bl_bl, sub_bl_ah, sub_bl_ch, sub_bl_dh, sub_bl_bh
	.word sub_ah_al, sub_ah_cl, sub_ah_dl, sub_ah_bl, sub_ah_ah, sub_ah_ch, sub_ah_dh, sub_ah_bh
	.word sub_ch_al, sub_ch_cl, sub_ch_dl, sub_ch_bl, sub_ch_ah, sub_ch_ch, sub_ch_dh, sub_ch_bh
	.word sub_dh_al, sub_dh_cl, sub_dh_dl, sub_dh_bl, sub_dh_ah, sub_dh_ch, sub_dh_dh, sub_dh_bh
	.word sub_bh_al, sub_bh_cl, sub_bh_dl, sub_bh_bl, sub_bh_ah, sub_bh_ch, sub_bh_dh, sub_bh_bh

	modrm_2_genall sub

// ------------------- 2B = SUB r32, r/m32 ------------------------------
//
	.global	op_2b_USE32
op_2b_USE32:
	modrm_jump_32
	modrm_tbl_3 sub

.macro sub_r32_t0 reg
	.global	sub_r32_t0_bp_\reg
sub_r32_t0_bp_\reg:
	//-------
	// Setup the correct segment when indexing by BP register.
	//-------
	mem_handler_bp
	.global	sub_r32_t0_\reg
sub_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_2b_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_2b_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	subs	\reg, r0
	b		complement_carry
.endm

	sub_r32_t0 eax
	sub_r32_t0 ecx
	sub_r32_t0 edx
	sub_r32_t0 ebx
	sub_r32_t0 esp
	sub_r32_t0 ebp
	sub_r32_t0 esi
	sub_r32_t0 edi

.macro sub_reg32_reg32 rl rr
	subs	\rl, \rr
	b		complement_carry
.endm

	modrm_3_genall sub sub_r32_t0

// ------------------- 2D = SUB EAX,imm32 ------------------------------
	.global	op_2d_USE32
op_2d_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	subs	eax, r0
	b		complement_carry

// ------------------- 30 = XOR r/m8, r8 ------------------------------
//
op_30_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_0 xor
	.word xor_al_al, xor_cl_al, xor_dl_al, xor_bl_al, xor_ah_al, xor_ch_al, xor_dh_al, xor_bh_al
	.word xor_al_cl, xor_cl_cl, xor_dl_cl, xor_bl_cl, xor_ah_cl, xor_ch_cl, xor_dh_cl, xor_bh_cl
	.word xor_al_dl, xor_cl_dl, xor_dl_dl, xor_bl_dl, xor_ah_dl, xor_ch_dl, xor_dh_dl, xor_bh_dl
	.word xor_al_bl, xor_cl_bl, xor_dl_bl, xor_bl_bl, xor_ah_bl, xor_ch_bl, xor_dh_bl, xor_bh_bl
	.word xor_al_ah, xor_cl_ah, xor_dl_ah, xor_bl_ah, xor_ah_ah, xor_ch_ah, xor_dh_ah, xor_bh_ah
	.word xor_al_ch, xor_cl_ch, xor_dl_ch, xor_bl_ch, xor_ah_ch, xor_ch_ch, xor_dh_ch, xor_bh_ch
	.word xor_al_dh, xor_cl_dh, xor_dl_dh, xor_bl_dh, xor_ah_dh, xor_ch_dh, xor_dh_dh, xor_bh_dh
	.word xor_al_bh, xor_cl_bh, xor_dl_bh, xor_bl_bh, xor_ah_bh, xor_ch_bh, xor_dh_bh, xor_bh_bh
	
	modrm_0_genall xor

// ------------------- 31 = XOR r/m32, r32 ----------------------------
//
op_31_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 xor

.macro xor_t0_r32 reg
	.global	xor_t0_r32_bp_\reg
xor_t0_r32_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	xor_t0_r32_\reg
xor_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_31_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_31_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	eors	r0, \reg
	str		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	eors	r0, \reg
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	lsr		r0, #8
	strb	r0,[r2, #2]
	lsr		r0, #8
	strb	r0,[r2, #3]
#endif
	b		loop
.endm

	xor_t0_r32 eax
	xor_t0_r32 ecx
	xor_t0_r32 edx
	xor_t0_r32 ebx
	xor_t0_r32 esp
	xor_t0_r32 ebp
	xor_t0_r32 esi
	xor_t0_r32 edi

	modrm_1_genall xor xor_t0_r32

// ------------------- 32 = XOR r8, r/m8 ------------------------------
//
op_32_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_2 xor
	.word xor_al_al, xor_al_cl, xor_al_dl, xor_al_bl, xor_al_ah, xor_al_ch, xor_al_dh, xor_al_bh
	.word xor_cl_al, xor_cl_cl, xor_cl_dl, xor_cl_bl, xor_cl_ah, xor_cl_ch, xor_cl_dh, xor_cl_bh
	.word xor_dl_al, xor_dl_cl, xor_dl_dl, xor_dl_bl, xor_dl_ah, xor_dl_ch, xor_dl_dh, xor_dl_bh
	.word xor_bl_al, xor_bl_cl, xor_bl_dl, xor_bl_bl, xor_bl_ah, xor_bl_ch, xor_bl_dh, xor_bl_bh
	.word xor_ah_al, xor_ah_cl, xor_ah_dl, xor_ah_bl, xor_ah_ah, xor_ah_ch, xor_ah_dh, xor_ah_bh
	.word xor_ch_al, xor_ch_cl, xor_ch_dl, xor_ch_bl, xor_ch_ah, xor_ch_ch, xor_ch_dh, xor_ch_bh
	.word xor_dh_al, xor_dh_cl, xor_dh_dl, xor_dh_bl, xor_dh_ah, xor_dh_ch, xor_dh_dh, xor_dh_bh
	.word xor_bh_al, xor_bh_cl, xor_bh_dl, xor_bh_bl, xor_bh_ah, xor_bh_ch, xor_bh_dh, xor_bh_bh

	modrm_2_genall xor

// ------------------- 33 = XOR r32, r/m32 -----------------------------
//
op_33_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 xor

.macro xor_r32_t0 reg
	.global	xor_r32_t0_bp_\reg
xor_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	xor_r32_t0_\reg
xor_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_33_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_33_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	eors	\reg, r0
	b		loop
.endm

	xor_r32_t0 eax
	xor_r32_t0 ecx
	xor_r32_t0 edx
	xor_r32_t0 ebx
	xor_r32_t0 esp
	xor_r32_t0 ebp
	xor_r32_t0 esi
	xor_r32_t0 edi

.macro xor_reg32_reg32 rl rr
	eors	\rl, \rr
	b		loop						// Back to loop
.endm

	modrm_3_genall xor xor_r32_t0

// ------------------- 35 = XOR EAX,imm32 -----------------------------
	.global	op_35_USE32
op_35_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	eors	eax, r0
	b		loop

// ------------------- 38 = CMP r/m8, r8 -------------------------------
//
op_38_USE32:
	modrm_jump_32
	modrm_tbl_0 cmp
	.word cmp_al_al, cmp_cl_al, cmp_dl_al, cmp_bl_al, cmp_ah_al, cmp_ch_al, cmp_dh_al, cmp_bh_al
	.word cmp_al_cl, cmp_cl_cl, cmp_dl_cl, cmp_bl_cl, cmp_ah_cl, cmp_ch_cl, cmp_dh_cl, cmp_bh_cl
	.word cmp_al_dl, cmp_cl_dl, cmp_dl_dl, cmp_bl_dl, cmp_ah_dl, cmp_ch_dl, cmp_dh_dl, cmp_bh_dl
	.word cmp_al_bl, cmp_cl_bl, cmp_dl_bl, cmp_bl_bl, cmp_ah_bl, cmp_ch_bl, cmp_dh_bl, cmp_bh_bl
	.word cmp_al_ah, cmp_cl_ah, cmp_dl_ah, cmp_bl_ah, cmp_ah_ah, cmp_ch_ah, cmp_dh_ah, cmp_bh_ah
	.word cmp_al_ch, cmp_cl_ch, cmp_dl_ch, cmp_bl_ch, cmp_ah_ch, cmp_ch_ch, cmp_dh_ch, cmp_bh_ch
	.word cmp_al_dh, cmp_cl_dh, cmp_dl_dh, cmp_bl_dh, cmp_ah_dh, cmp_ch_dh, cmp_dh_dh, cmp_bh_dh
	.word cmp_al_bh, cmp_cl_bh, cmp_dl_bh, cmp_bl_bh, cmp_ah_bh, cmp_ch_bh, cmp_dh_bh, cmp_bh_bh

	modrm_0_genall cmp

// ------------------- 39 = CMP r/m32, r32 -----------------------------
//
op_39_USE32:
	modrm_jump_32
	modrm_tbl_1 cmp

.macro cmp_t0_r32 reg
	.global	cmp_t0_r32_bp_\reg
cmp_t0_r32_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	cmp_t0_r32_\reg
cmp_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_39_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_39_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	cmp		r0, \reg
	b		complement_carry
.endm

	cmp_t0_r32 eax
	cmp_t0_r32 ecx
	cmp_t0_r32 edx
	cmp_t0_r32 ebx
	cmp_t0_r32 esp
	cmp_t0_r32 ebp
	cmp_t0_r32 esi
	cmp_t0_r32 edi

	modrm_1_genall cmp cmp_t0_r32

// ------------------- 3A = CMP r8, r/m8 -------------------------------
//
op_3a_USE32:
	modrm_jump_32
	modrm_tbl_2 cmp
	.word cmp_al_al, cmp_al_cl, cmp_al_dl, cmp_al_bl, cmp_al_ah, cmp_al_ch, cmp_al_dh, cmp_al_bh
	.word cmp_cl_al, cmp_cl_cl, cmp_cl_dl, cmp_cl_bl, cmp_cl_ah, cmp_cl_ch, cmp_cl_dh, cmp_cl_bh
	.word cmp_dl_al, cmp_dl_cl, cmp_dl_dl, cmp_dl_bl, cmp_dl_ah, cmp_dl_ch, cmp_dl_dh, cmp_dl_bh
	.word cmp_bl_al, cmp_bl_cl, cmp_bl_dl, cmp_bl_bl, cmp_bl_ah, cmp_bl_ch, cmp_bl_dh, cmp_bl_bh
	.word cmp_ah_al, cmp_ah_cl, cmp_ah_dl, cmp_ah_bl, cmp_ah_ah, cmp_ah_ch, cmp_ah_dh, cmp_ah_bh
	.word cmp_ch_al, cmp_ch_cl, cmp_ch_dl, cmp_ch_bl, cmp_ch_ah, cmp_ch_ch, cmp_ch_dh, cmp_ch_bh
	.word cmp_dh_al, cmp_dh_cl, cmp_dh_dl, cmp_dh_bl, cmp_dh_ah, cmp_dh_ch, cmp_dh_dh, cmp_dh_bh
	.word cmp_bh_al, cmp_bh_cl, cmp_bh_dl, cmp_bh_bl, cmp_bh_ah, cmp_bh_ch, cmp_bh_dh, cmp_bh_bh

	modrm_2_genall cmp

// ------------------- 3B = CMP r32, r/m32 ----------------------------
//
op_3b_USE32:
	modrm_jump_32
	modrm_tbl_3 cmp

.macro cmp_r32_t0 reg
	.global	cmp_r32_t0_bp_\reg
cmp_r32_t0_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	cmp_r32_t0_\reg
cmp_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_3b_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_3b_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
#if USE_SEIBUSPI
	//-------
	// Raiden Fighters Jet, statistics after running two demo games:
	// Aligned RAM access: 173715579 times, unaligned RAM access: 2 times.
	// It is worth to test for aligned access and use faster code in that situation.
	//-------
	tst		r2, #3
	bne		1f
	ldr		r0, [r2]
	cmp		\reg, r0
	b		complement_carry
1:	
#endif	
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	cmp		\reg, r0
	b		complement_carry
.endm

	cmp_r32_t0 eax
	cmp_r32_t0 ecx
	cmp_r32_t0 edx
	cmp_r32_t0 ebx
	cmp_r32_t0 esp
	cmp_r32_t0 ebp
	cmp_r32_t0 esi
	cmp_r32_t0 edi

.macro cmp_reg32_reg32 rl rr
	cmp		\rl, \rr
	b		complement_carry
.endm

	modrm_3_genall cmp cmp_r32_t0

// ------------------- 3D = CMP EAX,imm32 -----------------------------
	.global	op_3d_USE32
op_3d_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	cmp		eax, r0
	b		complement_carry

// =================== 40..47 = INC reg32 ==============================
// Opcodes 40..47 for AX, CX, DX, BX, SP, BP, SI, DI
// INC reg16 is like ADD reg16,1, except the Carry flag is not changed.
.macro inc_reg32 reg
	mrs		r0,cpsr					// r0 = Current flags
	adds	\reg, #1
	mrs		r1,cpsr					// r1 = Flags after increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
.endm

	.global	op_40_USE32
	.global	op_41_USE32
	.global	op_42_USE32
	.global	op_43_USE32
	.global	op_44_USE32
	.global	op_45_USE32
	.global	op_46_USE32
	.global	op_47_USE32

op_40_USE32:
	inc_reg32 eax
op_41_USE32:
	inc_reg32 ecx
op_42_USE32:
	inc_reg32 edx
op_43_USE32:
	inc_reg32 ebx
op_44_USE32:
	inc_reg32 esp
op_45_USE32:
	inc_reg32 ebp
op_46_USE32:
	inc_reg32 esi
op_47_USE32:
	inc_reg32 edi

// =================== 48..4F = DEC reg32 ==============================
// Opcodes 48..4F for AX, CX, DX, BX, SP, BP, SI, DI
// DEC reg16 is like SUB reg16,1, except the Carry flag is not changed.
.macro dec_reg32 reg
	mrs		r0,cpsr					// r0 = Current flags
	subs	\reg, #1
	mrs		r1,cpsr					// r1 = Flags after increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
.endm

	.global	op_48_USE32
	.global	op_49_USE32
	.global	op_4a_USE32
	.global	op_4b_USE32
	.global	op_4c_USE32
	.global	op_4d_USE32
	.global	op_4e_USE32
	.global	op_4f_USE32

op_48_USE32:
	dec_reg32 eax
op_49_USE32:
	dec_reg32 ecx
op_4a_USE32:
	dec_reg32 edx
op_4b_USE32:
	dec_reg32 ebx
op_4c_USE32:
	dec_reg32 esp
op_4d_USE32:
	dec_reg32 ebp
op_4e_USE32:
	dec_reg32 esi
op_4f_USE32:
	dec_reg32 edi


// =================== 50..57 = PUSH reg32 ==============================
// Opcodes 50..57 for AX, CX, DX, BX, SP, BP, SI, DI
.macro push_reg32 reg
	push_dword \reg r0 r2
	b		loop
.endm

	.global	op_50_USE32
	.global	op_51_USE32
	.global	op_52_USE32
	.global	op_53_USE32
	.global	op_54_USE32
	.global	op_55_USE32
	.global	op_56_USE32
	.global	op_57_USE32
	
op_50_USE32:
	push_reg32 eax
op_51_USE32:
	push_reg32 ecx
op_52_USE32:
	push_reg32 edx
op_53_USE32:
	push_reg32 ebx
op_54_USE32:
	mov		r1, esp
	push_reg32 r1
op_55_USE32:
	push_reg32 ebp
op_56_USE32:
	push_reg32 esi
op_57_USE32:
	push_reg32 edi

// =================== 58..5F = POP reg32 ==============================
// Opcodes 58..5F for AX, CX, DX, BX, SP, BP, SI, DI
.macro pop_reg32 reg
	pop_dword \reg r0 r1
	b		loop
.endm

	.global	op_58_USE32
	.global	op_59_USE32
	.global	op_5a_USE32
	.global	op_5b_USE32
	.global	op_5c_USE32
	.global	op_5d_USE32
	.global	op_5e_USE32
	.global	op_5f_USE32

op_58_USE32:
	pop_reg32 eax
op_59_USE32:
	pop_reg32 ecx
op_5a_USE32:
	pop_reg32 edx
op_5b_USE32:
	pop_reg32 ebx
op_5c_USE32:
	pop_dword r0 r1 r2
	mov		esp, r0								// Can not directly pop to esp, as it is used several times in the macro!
	b		loop
op_5d_USE32:
	pop_reg32 ebp
op_5e_USE32:
	pop_reg32 esi
op_5f_USE32:
	pop_reg32 edi

// ------------------- 60 = PUSHAD --------------------------------------
// Push EAX, ECX, EDX, EBX, orig ESP, EBP, ESI, EDI
//
	.global	op_60_USE32
op_60_USE32:
	mov			r1, esp
	push_dword 	eax r0 r2
	push_dword 	ecx r0 r2
	push_dword 	edx r0 r2
	push_dword 	ebx r0 r2
	push_dword 	r1 r0 r2
	push_dword 	ebp r0 r2
	push_dword 	esi r0 r2
	push_dword 	edi r0 r2
	b		loop

// ------------------- 61 = POPAD --------------------------------------
// Pop EDI, ESI, EBP, (nothing), EBX, EDX, ECX, EAX
//
	.global	op_61_USE32
op_61_USE32:
	pop_dword	edi, r0, r1
	pop_dword	esi, r0, r1
	pop_dword	ebp, r0, r1
	add			esp, #4
	pop_dword	ebx, r0, r1
	pop_dword	edx, r0, r1
	pop_dword	ecx, r0, r1
	pop_dword	eax, r0, r1
	b		loop

// ------------------- Segment Overrides using "opcodetable_32_32" -----------------------

op_26_USE32:
	ldr		r2, [sp, #SP_ES_BASE]				// r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+5*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_2e_USE32:
	ldr		r2, [sp, #SP_CS_BASE]				// r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+4*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_36_USE32:
	ldr		r2, [sp, #SP_SS_BASE]				// r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+3*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_3e_USE32:
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				// r2 = logical DS segment in high halfword
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+2*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_64_USE32:
	ldr		r2, [sp, #SP_FS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+1*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_65_USE32:
	ldr		r2, [sp, #SP_GS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+0*5)*4					// (opcodetable_32_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

// ------------------- 6667 = Operand- and Address-size Prefixes -------
	.global	op_66_67_USE16
op_66_67_USE16:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
	//---------------------------------------------
	// Operand Size = 32bit, Address Size = 32bit
	//---------------------------------------------
	.global	opcodetable_32_32
opcodetable_32_32:	
// 0x00 (prefix 6667)
	.word op_00_USE32, op_01_USE32, op_02_USE32, op_03_USE32, op_04, op_05_USE32, op_06_USE32, op_07_USE32
	.word op_08_USE32, op_09_USE32, op_0a_USE32, op_0b_USE32, op_0c, op_0d_USE32, op_0e_USE32, op_0f_USE32
	.word op_10_USE32, op_11_USE32, op_12_USE32, op_13_USE32, op_14, op_15_USE32, op_16_USE32, op_17_USE32
	.word op_18_USE32, op_19_USE32, op_1a_USE32, op_1b_USE32, op_1c, op_1d_USE32, op_1e_USE32, op_1f_USE32
	.word op_20_USE32, op_21_USE32, op_22_USE32, op_23_USE32, op_24, op_25_USE32, op_26_USE32, op_27
	.word op_28_USE32, op_29_USE32, op_2a_USE32, op_2b_USE32, op_2c, op_2d_USE32, op_2e_USE32, op_2f
	.word op_30_USE32, op_31_USE32, op_32_USE32, op_33_USE32, op_34, op_35_USE32, op_36_USE32, op_37
	.word op_38_USE32, op_39_USE32, op_3a_USE32, op_3b_USE32, op_3c, op_3d_USE32, op_3e_USE32, op_3f
// 0x40 (prefix 6667)
	.word op_40_USE32, op_41_USE32, op_42_USE32, op_43_USE32, op_44_USE32, op_45_USE32, op_46_USE32, op_47_USE32
	.word op_48_USE32, op_49_USE32, op_4a_USE32, op_4b_USE32, op_4c_USE32, op_4d_USE32, op_4e_USE32, op_4f_USE32
	.word op_50_USE32, op_51_USE32, op_52_USE32, op_53_USE32, op_54_USE32, op_55_USE32, op_56_USE32, op_57_USE32
	.word op_58_USE32, op_59_USE32, op_5a_USE32, op_5b_USE32, op_5c_USE32, op_5d_USE32, op_5e_USE32, op_5f_USE32
	.word op_60_USE32, op_61_USE32, .unknown, .unknown, op_64_USE32, op_65_USE32, op_67_USE16, op_67_USE32
	.word op_68_USE32, op_69_USE32, op_6a_USE32, op_6b_USE32, op_6c_insb_USE32, .unknown, op_6e_outsb_USE32, .unknown
	.word op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	.word op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
// 0x80 (prefix 6667)
	.word op_80_USE32, op_81_USE32, op_82_USE32, op_83_USE32, op_84_USE32, op_85_USE32, op_86_USE32, op_87_USE32
	.word op_88_USE32, op_89_USE32, op_8a_USE32, op_8b_USE32, op_8c_USE32, op_8d_USE32, op_8e_USE32, op_8f_USE32
	.word loop
	modrm_reg_reg_word xchg ecx eax
	modrm_reg_reg_word xchg edx eax
	modrm_reg_reg_word xchg ebx eax
	modrm_reg_reg_word xchg esp eax
	modrm_reg_reg_word xchg ebp eax
	modrm_reg_reg_word xchg esi eax
	modrm_reg_reg_word xchg edi eax
	.word op_98_USE32, op_99_USE32, op_9a_USE32, loop, op_9c_USE32, op_9d_USE32, op_9e, op_9f
	.word op_a0_USE32, op_a1_USE32, op_a2_USE32, op_a3_USE32, op_a4_movsb_USE32, op_a5_movsd_USE32, op_a6_cmpsb_USE32, op_a7_cmpsd_USE32
	.word op_a8, op_a9_USE32, op_aa_stosb_USE32, op_ab_stosd_USE32, op_ac_lodsb_USE32, op_ad_lodsd_USE32, op_ae_scasb_USE32, op_af_scasd_USE32
	.word op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	.word op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
// 0xC0 (prefix 6667)
	.word op_c0_USE32, op_c1_USE32, op_c2_USE32, op_c3_USE32, op_c4_USE32, op_c5_USE32, op_c6_USE32, op_c7_USE32
	.word op_c8_USE32, op_c9_USE32, op_ca_USE32, op_cb_USE32, op_cc, op_cd, .unknown, op_cf_USE32
	.word op_d0_USE32, op_d1_USE32, op_d2_USE32, op_d3_USE32, op_d4, .unknown, .unknown, op_d7_USE32
#if USE_FPU_OPCODES
	.word op_d8_USE32, op_d9_USE32, op_da_USE32, op_db_USE32, op_dc_USE32, op_dd_USE32, op_de_USE32, op_df_USE32
#else
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
#endif
	.word op_e0_USE32, op_e1_USE32, op_e2_USE32, op_e3_USE32, op_e4_in_al_imm8, op_e5_in_eax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_eax
	.word op_e8_rel32, op_e9_rel32, op_ea_USE32, op_eb, op_ec_in_al_dx, op_ed_in_eax_dx, op_ee_out_dx_al, op_ef_out_dx_eax
	.word .unknown, .unknown, op_f2_USE32, op_f3_USE32, op_f4, op_f5, op_f6_USE32, op_f7_USE32
	.word op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe_USE32, op_ff_USE32


// ------------------- 68 = PUSH imm32 --------------------------------
	.global	op_68_USE32
op_68_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	push_dword r0 r1 r2
	b		loop

// ------------------- 69 = IMUL r32,r/m32,imm32 ------------------------
op_69_USE32:
	modrm_jump_32
	modrm_tbl_3 imul_imm32
	
.macro imul_imm32_r32_t0 reg
	.global	imul_imm32_r32_t0_bp_\reg
imul_imm32_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	imul_imm32_r32_t0_\reg
imul_imm32_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_69_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_69_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
	ldr		r1, [r12], #4
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24						// r0 == r/m32 value
	ldrb	r1, [r12],#1
	ldrb	r2, [r12],#1
	ldrb	r3, [r12],#1
	orr		r1, r2, lsl #8
	ldrb	r2, [r12],#1
	orr		r1, r3, lsl #16
	orr		r1, r2, lsl #24						// r1 = imm32 value
#endif
	mul		\reg, r0, r1
	b		loop
.endm

	imul_imm32_r32_t0 eax
	imul_imm32_r32_t0 ecx
	imul_imm32_r32_t0 edx
	imul_imm32_r32_t0 ebx
	imul_imm32_r32_t0 esp
	imul_imm32_r32_t0 ebp
	imul_imm32_r32_t0 esi
	imul_imm32_r32_t0 edi

.macro imul_imm32_reg32_reg32 rl rr
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	mul		\rl, r0, \rr
	b		loop	
.endm

	modrm_3_genall imul_imm32 imul_imm32_r32_t0

// ------------------- 6A = PUSH imm8 ----------------------------------
//
	.global	op_6a_USE32
op_6a_USE32:
	ldrsb	r1, [r12],#1
	push_dword r1 r0 r2
	b		loop
	
// ------------------- 6B = IMUL r32,r/m32,imm8 ------------------------
// TODO! Handle Overflow and Carry flags!
op_6b_USE32:
	modrm_jump_32
	modrm_tbl_3 imul_imm8

.macro imul_imm8_r32_t0 reg
	.global	imul_imm8_r32_t0_bp_\reg
imul_imm8_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	imul_imm8_r32_t0_\reg
imul_imm8_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 .op_6b_RAM_\reg bad_EGA_opcode bad_EGA_opcode
.op_6b_RAM_\reg:
#if USE_UNALIGNED
	ldr		r0, [r2]
#else
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24						// r0 == r/m32 value
#endif
	ldrsb	r1, [r12],#1						// r1 = imm8 value
	mul		\reg, r0, r1
	b		loop
.endm

	imul_imm8_r32_t0 eax
	imul_imm8_r32_t0 ecx
	imul_imm8_r32_t0 edx
	imul_imm8_r32_t0 ebx
	imul_imm8_r32_t0 esp
	imul_imm8_r32_t0 ebp
	imul_imm8_r32_t0 esi
	imul_imm8_r32_t0 edi

.macro imul_imm8_reg32_reg32 rl rr
	ldrsb	r1, [r12],#1						// r1 = imm8 value
	mul		\rl, r1, \rr
	b		loop	
.endm

	modrm_3_genall imul_imm8 imul_imm8_r32_t0

// ------------------- 80 = ??? r/m8,imm8 ------------------------------
	.global	op_80_USE32
op_80_USE32:
	.global	op_82_USE32
op_82_USE32:
	modrm_jump_32
	modrm_tbl_0_oper add or adc sbb and sub xor cmp imm8
// ADD r/m8,imm8
	.word add_al_imm8, add_cl_imm8, add_dl_imm8, add_bl_imm8, add_ah_imm8, add_ch_imm8, add_dh_imm8, add_bh_imm8
// OR r/m8,imm8
	.word or_al_imm8, or_cl_imm8, or_dl_imm8, or_bl_imm8, or_ah_imm8, or_ch_imm8, or_dh_imm8, or_bh_imm8
// ADC r/m8,imm8
	.word adc_al_imm8, adc_cl_imm8, adc_dl_imm8, adc_bl_imm8, adc_ah_imm8, adc_ch_imm8, adc_dh_imm8, adc_bh_imm8
// SBB r/m8,imm8
	.word sbb_al_imm8, sbb_cl_imm8, sbb_dl_imm8, sbb_bl_imm8, sbb_ah_imm8, sbb_ch_imm8, sbb_dh_imm8, sbb_bh_imm8
// AND r/m8,imm8
	.word and_al_imm8, and_cl_imm8, and_dl_imm8, and_bl_imm8, and_ah_imm8, and_ch_imm8, and_dh_imm8, and_bh_imm8
// SUB r/m8,imm8
	.word sub_al_imm8, sub_cl_imm8, sub_dl_imm8, sub_bl_imm8, sub_ah_imm8, sub_ch_imm8, sub_dh_imm8, sub_bh_imm8
// XOR r/m8,imm8
	.word xor_al_imm8, xor_cl_imm8, xor_dl_imm8, xor_bl_imm8, xor_ah_imm8, xor_ch_imm8, xor_dh_imm8, xor_bh_imm8
// CMP r/m8,imm8
	.word cmp_al_imm8, cmp_cl_imm8, cmp_dl_imm8, cmp_bl_imm8, cmp_ah_imm8, cmp_ch_imm8, cmp_dh_imm8, cmp_bh_imm8

	modrm_genall_0_oper add or adc sbb and sub xor cmp _t0 imm8

// ------------------- 81 = ??? r/m32,imm32 ----------------------------
op_81_USE32:
	modrm_jump_32
	modrm_tbl_1_oper add or adc sbb and sub xor cmp imm32
// 0xc0 = mod = 11b => two register operands
// ADD r/m16,imm32
	.word add_eax_imm32, add_ecx_imm32, add_edx_imm32, add_ebx_imm32, add_esp_imm32, add_ebp_imm32, add_esi_imm32, add_edi_imm32
// OR r/m16,imm32
	.word or_eax_imm32, or_ecx_imm32, or_edx_imm32, or_ebx_imm32, or_esp_imm32, or_ebp_imm32, or_esi_imm32, or_edi_imm32
// ADC r/m16,imm32
	.word adc_eax_imm32, adc_ecx_imm32, adc_edx_imm32, adc_ebx_imm32, adc_esp_imm32, adc_ebp_imm32, adc_esi_imm32, adc_edi_imm32
// SBB r/m16,imm32
	.word sbb_eax_imm32, sbb_ecx_imm32, sbb_edx_imm32, sbb_ebx_imm32, sbb_esp_imm32, sbb_ebp_imm32, sbb_esi_imm32, sbb_edi_imm32
// AND r/m16,imm32
	.word and_eax_imm32, and_ecx_imm32, and_edx_imm32, and_ebx_imm32, and_esp_imm32, and_ebp_imm32, and_esi_imm32, and_edi_imm32
// SUB r/m16,imm32
	.word sub_eax_imm32, sub_ecx_imm32, sub_edx_imm32, sub_ebx_imm32, sub_esp_imm32, sub_ebp_imm32, sub_esi_imm32, sub_edi_imm32
// XOR r/m16,imm32
	.word xor_eax_imm32, xor_ecx_imm32, xor_edx_imm32, xor_ebx_imm32, xor_esp_imm32, xor_ebp_imm32, xor_esi_imm32, xor_edi_imm32
// CMP r/m16,imm32
	.word cmp_eax_imm32, cmp_ecx_imm32, cmp_edx_imm32, cmp_ebx_imm32, cmp_esp_imm32, cmp_ebp_imm32, cmp_esi_imm32, cmp_edi_imm32

	modrm_genall_1_oper add or adc sbb and sub xor cmp _dw_t0 imm32

.macro op81common oper
	.global	\oper\()_dw_t0_bp_imm32
\oper\()_dw_t0_bp_imm32:
	mem_handler_bp
	.global	\oper\()_dw_t0_imm32
\oper\()_dw_t0_imm32:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr		r0, [r2]
	ldr		r1, [r12], #4
#else
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24						// r0 == r/m32 value
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	orr		r1, r3, lsl #8
	ldrb	r3, [r12],#1
	orr		r1, r3, lsl #16
	ldrb	r3, [r12],#1
	orr		r1, r3, lsl #24						// r1 = imm32 value
#endif
.endm

	//-------
	// Add: memory location in normal RAM.
	//-------
	op81common add
	adds	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Or: memory location in normal RAM.
	//-------
	op81common or
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orrs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Adc: memory location in normal RAM.
	//-------
	op81common adc
	adcs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Sbb: memory location in normal RAM.
	//-------
	op81common sbb
	mrs		r3,cpsr
	eor		r3, r3, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r3
	sbcs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		complement_carry

	//-------
	// And: memory location in normal RAM.
	//-------
	op81common and
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	ands	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Sub: memory location in normal RAM.
	//-------
	op81common sub
	subs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		complement_carry

	//-------
	// Xor: memory location in normal RAM.
	//-------
	op81common xor
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	eors	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Cmp: memory location in normal RAM.
	//-------
	op81common cmp
	cmp		r0, r1
	b		complement_carry

// ----- ADD -----

	.global add_eax_imm32 
	.global add_ecx_imm32 
	.global add_edx_imm32 
	.global add_ebx_imm32 
	.global add_esp_imm32 
	.global add_ebp_imm32 
	.global add_esi_imm32 
	.global add_edi_imm32

.macro add_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	adds	\reg, r0
#else
	ands	r0, r12, #3
	ldr		r1, [r12, -r0]
	add		r12, #4
	ldr		r2, [r12, -r0]
	lsl		r0, #3
	rsb		r3, r0, #32
	mov		r1, r1, lsr r0
	orr		r1, r2, lsl r3
	adds	\reg, r1
#endif
	b		loop
.endm

add_eax_imm32:
	add_reg32_imm32 eax
add_ecx_imm32:
	add_reg32_imm32 ecx
add_edx_imm32:
	add_reg32_imm32 edx
add_ebx_imm32:
	add_reg32_imm32 ebx
add_esp_imm32:
	add_reg32_imm32 esp
add_ebp_imm32:
	add_reg32_imm32 ebp
add_esi_imm32:
	add_reg32_imm32 esi
add_edi_imm32:
	add_reg32_imm32 edi

// ----- OR -----

	.global or_eax_imm32 
	.global or_ecx_imm32 
	.global or_edx_imm32 
	.global or_ebx_imm32 
	.global or_esp_imm32 
	.global or_ebp_imm32 
	.global or_esi_imm32 
	.global or_edi_imm32

.macro or_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	orrs	\reg, r0
	b		loop
.endm

or_eax_imm32:
	or_reg32_imm32 eax
or_ecx_imm32:
	or_reg32_imm32 ecx
or_edx_imm32:
	or_reg32_imm32 edx
or_ebx_imm32:
	or_reg32_imm32 ebx
or_esp_imm32:
	or_reg32_imm32 esp
or_ebp_imm32:
	or_reg32_imm32 ebp
or_esi_imm32:
	or_reg32_imm32 esi
or_edi_imm32:
	or_reg32_imm32 edi

// ----- ADC -----

	.global adc_eax_imm32 
	.global adc_ecx_imm32 
	.global adc_edx_imm32 
	.global adc_ebx_imm32 
	.global adc_esp_imm32 
	.global adc_ebp_imm32 
	.global adc_esi_imm32 
	.global adc_edi_imm32

.macro adc_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	adcs	\reg, r0
	b		loop
.endm

adc_eax_imm32:
	adc_reg32_imm32 eax
adc_ecx_imm32:
	adc_reg32_imm32 ecx
adc_edx_imm32:
	adc_reg32_imm32 edx
adc_ebx_imm32:
	adc_reg32_imm32 ebx
adc_esp_imm32:
	adc_reg32_imm32 esp
adc_ebp_imm32:
	adc_reg32_imm32 ebp
adc_esi_imm32:
	adc_reg32_imm32 esi
adc_edi_imm32:
	adc_reg32_imm32 edi

// ----- SBB -----

	.global sbb_eax_imm32 
	.global sbb_ecx_imm32 
	.global sbb_edx_imm32 
	.global sbb_ebx_imm32 
	.global sbb_esp_imm32 
	.global sbb_ebp_imm32 
	.global sbb_esi_imm32 
	.global sbb_edi_imm32

.macro sbb_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
#else
	mrs		r0,cpsr
	eor		r0, r0, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r0
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	sbcs	\reg, r0
	b		complement_carry
.endm

sbb_eax_imm32:
	sbb_reg32_imm32 eax
sbb_ecx_imm32:
	sbb_reg32_imm32 ecx
sbb_edx_imm32:
	sbb_reg32_imm32 edx
sbb_ebx_imm32:
	sbb_reg32_imm32 ebx
sbb_esp_imm32:
	sbb_reg32_imm32 esp
sbb_ebp_imm32:
	sbb_reg32_imm32 ebp
sbb_esi_imm32:
	sbb_reg32_imm32 esi
sbb_edi_imm32:
	sbb_reg32_imm32 edi

// ----- AND -----

	.global and_eax_imm32 
	.global and_ecx_imm32 
	.global and_edx_imm32 
	.global and_ebx_imm32 
	.global and_esp_imm32 
	.global and_ebp_imm32 
	.global and_esi_imm32 
	.global and_edi_imm32

.macro and_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	ands	\reg, r0
	b		loop
.endm

and_eax_imm32:
	and_reg32_imm32 eax
and_ecx_imm32:
	and_reg32_imm32 ecx
and_edx_imm32:
	and_reg32_imm32 edx
and_ebx_imm32:
	and_reg32_imm32 ebx
and_esp_imm32:
	and_reg32_imm32 esp
and_ebp_imm32:
	and_reg32_imm32 ebp
and_esi_imm32:
	and_reg32_imm32 esi
and_edi_imm32:
	and_reg32_imm32 edi

// ----- SUB -----

	.global sub_eax_imm32 
	.global sub_ecx_imm32 
	.global sub_edx_imm32 
	.global sub_ebx_imm32 
	.global sub_esp_imm32 
	.global sub_ebp_imm32 
	.global sub_esi_imm32 
	.global sub_edi_imm32

.macro sub_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	subs	\reg, r0
	b		complement_carry
.endm

sub_eax_imm32:
	sub_reg32_imm32 eax
sub_ecx_imm32:
	sub_reg32_imm32 ecx
sub_edx_imm32:
	sub_reg32_imm32 edx
sub_ebx_imm32:
	sub_reg32_imm32 ebx
sub_esp_imm32:
	sub_reg32_imm32 esp
sub_ebp_imm32:
	sub_reg32_imm32 ebp
sub_esi_imm32:
	sub_reg32_imm32 esi
sub_edi_imm32:
	sub_reg32_imm32 edi

// ----- XOR -----

	.global xor_eax_imm32 
	.global xor_ecx_imm32 
	.global xor_edx_imm32 
	.global xor_ebx_imm32 
	.global xor_esp_imm32 
	.global xor_ebp_imm32 
	.global xor_esi_imm32 
	.global xor_edi_imm32

.macro xor_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	eors	\reg, r0
	b		loop
.endm

xor_eax_imm32:
	xor_reg32_imm32 eax
xor_ecx_imm32:
	xor_reg32_imm32 ecx
xor_edx_imm32:
	xor_reg32_imm32 edx
xor_ebx_imm32:
	xor_reg32_imm32 ebx
xor_esp_imm32:
	xor_reg32_imm32 esp
xor_ebp_imm32:
	xor_reg32_imm32 ebp
xor_esi_imm32:
	xor_reg32_imm32 esi
xor_edi_imm32:
	xor_reg32_imm32 edi

// ----- CMP -----

	.global cmp_eax_imm32 
	.global cmp_ecx_imm32 
	.global cmp_edx_imm32 
	.global cmp_ebx_imm32 
	.global cmp_esp_imm32 
	.global cmp_ebp_imm32 
	.global cmp_esi_imm32 
	.global cmp_edi_imm32

.macro cmp_reg32_imm32 reg
#if USE_UNALIGNED
	ldr		r0, [r12], #4
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	cmp		\reg, r0
	b		complement_carry
.endm

cmp_eax_imm32:
	cmp_reg32_imm32 eax
cmp_ecx_imm32:
	cmp_reg32_imm32 ecx
cmp_edx_imm32:
	cmp_reg32_imm32 edx
cmp_ebx_imm32:
	cmp_reg32_imm32 ebx
cmp_esp_imm32:
	cmp_reg32_imm32 esp
cmp_ebp_imm32:
	cmp_reg32_imm32 ebp
cmp_esi_imm32:
	cmp_reg32_imm32 esi
cmp_edi_imm32:
	cmp_reg32_imm32 edi

// ------------------- 83 = ??? r/m32,+imm8 ----------------------------
op_83_USE32:
	modrm_jump_32
	modrm_tbl_1_oper add or adc sbb and sub xor cmp simm8
	.word add_eax_simm8, add_ecx_simm8, add_edx_simm8, add_ebx_simm8, add_esp_simm8, add_ebp_simm8, add_esi_simm8, add_edi_simm8
	.word or_eax_simm8, or_ecx_simm8, or_edx_simm8, or_ebx_simm8, or_esp_simm8, or_ebp_simm8, or_esi_simm8, or_edi_simm8
	.word adc_eax_simm8, adc_ecx_simm8, adc_edx_simm8, adc_ebx_simm8, adc_esp_simm8, adc_ebp_simm8, adc_esi_simm8, adc_edi_simm8
	.word sbb_eax_simm8, sbb_ecx_simm8, sbb_edx_simm8, sbb_ebx_simm8, sbb_esp_simm8, sbb_ebp_simm8, sbb_esi_simm8, sbb_edi_simm8
	.word and_eax_simm8, and_ecx_simm8, and_edx_simm8, and_ebx_simm8, and_esp_simm8, and_ebp_simm8, and_esi_simm8, and_edi_simm8
	.word sub_eax_simm8, sub_ecx_simm8, sub_edx_simm8, sub_ebx_simm8, sub_esp_simm8, sub_ebp_simm8, sub_esi_simm8, sub_edi_simm8
	.word xor_eax_simm8, xor_ecx_simm8, xor_edx_simm8, xor_ebx_simm8, xor_esp_simm8, xor_ebp_simm8, xor_esi_simm8, xor_edi_simm8
	.word cmp_eax_simm8, cmp_ecx_simm8, cmp_edx_simm8, cmp_ebx_simm8, cmp_esp_simm8, cmp_ebp_simm8, cmp_esi_simm8, cmp_edi_simm8

	modrm_genall_1_oper add or adc sbb and sub xor cmp _dw_t0 simm8

.macro op83common oper
	.global	\oper\()_dw_t0_bp_simm8
\oper\()_dw_t0_bp_simm8:
	mem_handler_bp
	.global	\oper\()_dw_t0_simm8
\oper\()_dw_t0_simm8:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode op_83_MODEX_32_\oper
#if USE_UNALIGNED
1:	ldr		r0, [r2]
#else
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24						// r0 == r/m32 value
#endif
	ldrsb	r1, [r12],#1						// r1 = +imm8 value
.endm

	//-------
	// Add: memory location in normal RAM.
	//-------
	op83common add
	adds	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Or: memory location in normal RAM.
	//-------
	op83common or
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orrs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Adc: memory location in normal RAM.
	//-------
	op83common adc
	adcs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Sbb: memory location in normal RAM.
	//-------
	op83common sbb
	mrs		r3,cpsr
	eor		r3, r3, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r3
	sbcs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		complement_carry

	//-------
	// And: memory location in normal RAM.
	//-------
	op83common and
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	ands	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Sub: memory location in normal RAM.
	//-------
	op83common sub
	subs	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		complement_carry

	//-------
	// Xor: memory location in normal RAM.
	//-------
	op83common xor
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	eors	r0, r1
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	//-------
	// Cmp: memory location in normal RAM.
	//-------
	op83common cmp
	cmp		r0, r1
	b		complement_carry

// ----- ADD -----

	.global add_eax_simm8 
	.global add_ecx_simm8 
	.global add_edx_simm8 
	.global add_ebx_simm8 
	.global add_esp_simm8 
	.global add_ebp_simm8 
	.global add_esi_simm8 
	.global add_edi_simm8

.macro add_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	adds	\reg, r0
	b		loop
.endm

add_eax_simm8:
	add_reg32_simm8 eax
add_ecx_simm8:
	add_reg32_simm8 ecx
add_edx_simm8:
	add_reg32_simm8 edx
add_ebx_simm8:
	add_reg32_simm8 ebx
add_esp_simm8:
	add_reg32_simm8 esp
add_ebp_simm8:
	add_reg32_simm8 ebp
add_esi_simm8:
	add_reg32_simm8 esi
add_edi_simm8:
	add_reg32_simm8 edi

// ----- OR -----

	.global or_eax_simm8 
	.global or_ecx_simm8 
	.global or_edx_simm8 
	.global or_ebx_simm8 
	.global or_esp_simm8 
	.global or_ebp_simm8 
	.global or_esi_simm8 
	.global or_edi_simm8

.macro or_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orrs	\reg, r0
	b		loop
.endm

or_eax_simm8:
	or_reg32_simm8 eax
or_ecx_simm8:
	or_reg32_simm8 ecx
or_edx_simm8:
	or_reg32_simm8 edx
or_ebx_simm8:
	or_reg32_simm8 ebx
or_esp_simm8:
	or_reg32_simm8 esp
or_ebp_simm8:
	or_reg32_simm8 ebp
or_esi_simm8:
	or_reg32_simm8 esi
or_edi_simm8:
	or_reg32_simm8 edi

// ----- ADC -----

	.global adc_eax_simm8 
	.global adc_ecx_simm8 
	.global adc_edx_simm8 
	.global adc_ebx_simm8 
	.global adc_esp_simm8 
	.global adc_ebp_simm8 
	.global adc_esi_simm8 
	.global adc_edi_simm8

.macro adc_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	adcs	\reg, r0
	b		loop
.endm

adc_eax_simm8:
	adc_reg32_simm8 eax
adc_ecx_simm8:
	adc_reg32_simm8 ecx
adc_edx_simm8:
	adc_reg32_simm8 edx
adc_ebx_simm8:
	adc_reg32_simm8 ebx
adc_esp_simm8:
	adc_reg32_simm8 esp
adc_ebp_simm8:
	adc_reg32_simm8 ebp
adc_esi_simm8:
	adc_reg32_simm8 esi
adc_edi_simm8:
	adc_reg32_simm8 edi

// ----- SBB -----

	.global sbb_eax_simm8 
	.global sbb_ecx_simm8 
	.global sbb_edx_simm8 
	.global sbb_ebx_simm8 
	.global sbb_esp_simm8 
	.global sbb_ebp_simm8 
	.global sbb_esi_simm8 
	.global sbb_edi_simm8

.macro sbb_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
	sbcs	\reg, r0
	b		complement_carry
.endm

sbb_eax_simm8:
	sbb_reg32_simm8 eax
sbb_ecx_simm8:
	sbb_reg32_simm8 ecx
sbb_edx_simm8:
	sbb_reg32_simm8 edx
sbb_ebx_simm8:
	sbb_reg32_simm8 ebx
sbb_esp_simm8:
	sbb_reg32_simm8 esp
sbb_ebp_simm8:
	sbb_reg32_simm8 ebp
sbb_esi_simm8:
	sbb_reg32_simm8 esi
sbb_edi_simm8:
	sbb_reg32_simm8 edi

// ----- AND -----

	.global and_eax_simm8 
	.global and_ecx_simm8 
	.global and_edx_simm8 
	.global and_ebx_simm8 
	.global and_esp_simm8 
	.global and_ebp_simm8 
	.global and_esi_simm8 
	.global and_edi_simm8

.macro and_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	ands	\reg, r0
	b		loop
.endm

and_eax_simm8:
	and_reg32_simm8 eax
and_ecx_simm8:
	and_reg32_simm8 ecx
and_edx_simm8:
	and_reg32_simm8 edx
and_ebx_simm8:
	and_reg32_simm8 ebx
and_esp_simm8:
	and_reg32_simm8 esp
and_ebp_simm8:
	and_reg32_simm8 ebp
and_esi_simm8:
	and_reg32_simm8 esi
and_edi_simm8:
	and_reg32_simm8 edi

// ----- SUB -----

	.global sub_eax_simm8 
	.global sub_ecx_simm8 
	.global sub_edx_simm8 
	.global sub_ebx_simm8 
	.global sub_esp_simm8 
	.global sub_ebp_simm8 
	.global sub_esi_simm8 
	.global sub_edi_simm8

.macro sub_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	subs	\reg, r0
	b		complement_carry
.endm

sub_eax_simm8:
	sub_reg32_simm8 eax
sub_ecx_simm8:
	sub_reg32_simm8 ecx
sub_edx_simm8:
	sub_reg32_simm8 edx
sub_ebx_simm8:
	sub_reg32_simm8 ebx
sub_esp_simm8:
	sub_reg32_simm8 esp
sub_ebp_simm8:
	sub_reg32_simm8 ebp
sub_esi_simm8:
	sub_reg32_simm8 esi
sub_edi_simm8:
	sub_reg32_simm8 edi

// ----- XOR -----

	.global xor_eax_simm8 
	.global xor_ecx_simm8 
	.global xor_edx_simm8 
	.global xor_ebx_simm8 
	.global xor_esp_simm8 
	.global xor_ebp_simm8 
	.global xor_esi_simm8 
	.global xor_edi_simm8

.macro xor_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	eors	\reg, r0
	b		loop
.endm

xor_eax_simm8:
	xor_reg32_simm8 eax
xor_ecx_simm8:
	xor_reg32_simm8 ecx
xor_edx_simm8:
	xor_reg32_simm8 edx
xor_ebx_simm8:
	xor_reg32_simm8 ebx
xor_esp_simm8:
	xor_reg32_simm8 esp
xor_ebp_simm8:
	xor_reg32_simm8 ebp
xor_esi_simm8:
	xor_reg32_simm8 esi
xor_edi_simm8:
	xor_reg32_simm8 edi

// ----- CMP -----

	.global cmp_eax_simm8 
	.global cmp_ecx_simm8 
	.global cmp_edx_simm8 
	.global cmp_ebx_simm8 
	.global cmp_esp_simm8 
	.global cmp_ebp_simm8 
	.global cmp_esi_simm8 
	.global cmp_edi_simm8

.macro cmp_reg32_simm8 reg
	ldrsb	r0, [r12],#1
	cmp		\reg, r0
	b		complement_carry
.endm

cmp_eax_simm8:
	cmp_reg32_simm8 eax
cmp_ecx_simm8:
	cmp_reg32_simm8 ecx
cmp_edx_simm8:
	cmp_reg32_simm8 edx
cmp_ebx_simm8:
	cmp_reg32_simm8 ebx
cmp_esp_simm8:
	cmp_reg32_simm8 esp
cmp_ebp_simm8:
	cmp_reg32_simm8 ebp
cmp_esi_simm8:
	cmp_reg32_simm8 esi
cmp_edi_simm8:
	cmp_reg32_simm8 edi

// ------------------- 84 = TEST r8,r/m8 -------------------------------
op_84_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_0 test
	.word test_al_al, test_cl_al, test_dl_al, test_bl_al, test_ah_al, test_ch_al, test_dh_al, test_bh_al
	.word test_al_cl, test_cl_cl, test_dl_cl, test_bl_cl, test_ah_cl, test_ch_cl, test_dh_cl, test_bh_cl
	.word test_al_dl, test_cl_dl, test_dl_dl, test_bl_dl, test_ah_dl, test_ch_dl, test_dh_dl, test_bh_dl
	.word test_al_bl, test_cl_bl, test_dl_bl, test_bl_bl, test_ah_bl, test_ch_bl, test_dh_bl, test_bh_bl
	.word test_al_ah, test_cl_ah, test_dl_ah, test_bl_ah, test_ah_ah, test_ch_ah, test_dh_ah, test_bh_ah
	.word test_al_ch, test_cl_ch, test_dl_ch, test_bl_ch, test_ah_ch, test_ch_ch, test_dh_ch, test_bh_ch
	.word test_al_dh, test_cl_dh, test_dl_dh, test_bl_dh, test_ah_dh, test_ch_dh, test_dh_dh, test_bh_dh
	.word test_al_bh, test_cl_bh, test_dl_bh, test_bl_bh, test_ah_bh, test_ch_bh, test_dh_bh, test_bh_bh

	modrm_0_genall test

// ------------------- 85 = TEST r32,r/m32 -----------------------------
op_85_USE32:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 test

.macro test_t0_r32 reg
	.global	test_t0_r32_bp_\reg
test_t0_r32_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	test_t0_r32_\reg
test_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr		r0, [r2]
#else
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24						// r0 == r/m32 value
#endif
	tst		\reg, r0
	b		loop
.endm

	test_t0_r32 eax
	test_t0_r32 ecx
	test_t0_r32 edx
	test_t0_r32 ebx
	test_t0_r32 esp
	test_t0_r32 ebp
	test_t0_r32 esi
	test_t0_r32 edi

	modrm_1_genall test test_t0_r32

.macro test_reg32_reg32 rl rr
	tst		\rl, \rr
	b		loop						// Back to loop
.endm

	modrm_gen_reg_oper test eax
	modrm_gen_reg_oper test ecx
	modrm_gen_reg_oper test edx
	modrm_gen_reg_oper test ebx
	modrm_gen_reg_oper test esp
	modrm_gen_reg_oper test ebp
	modrm_gen_reg_oper test esi
	modrm_gen_reg_oper test edi

// ------------------- 86 = XCHG r/m8,r8 -----------------------------
op_86_USE32:
	modrm_jump_32
	modrm_tbl_0 xchg
	.word loop, xchg_cl_al, xchg_dl_al, xchg_bl_al, xchg_ah_al, xchg_ch_al, xchg_dh_al, xchg_bh_al
	.word xchg_al_cl, loop, xchg_dl_cl, xchg_bl_cl, xchg_ah_cl, xchg_ch_cl, xchg_dh_cl, xchg_bh_cl
	.word xchg_al_dl, xchg_cl_dl, loop, xchg_bl_dl, xchg_ah_dl, xchg_ch_dl, xchg_dh_dl, xchg_bh_dl
	.word xchg_al_bl, xchg_cl_bl, xchg_dl_bl, loop, xchg_ah_bl, xchg_ch_bl, xchg_dh_bl, xchg_bh_bl
	.word xchg_al_ah, xchg_cl_ah, xchg_dl_ah, xchg_bl_ah, loop, xchg_ch_ah, xchg_dh_ah, xchg_bh_ah
	.word xchg_al_ch, xchg_cl_ch, xchg_dl_ch, xchg_bl_ch, xchg_ah_ch, loop, xchg_dh_ch, xchg_bh_ch
	.word xchg_al_dh, xchg_cl_dh, xchg_dl_dh, xchg_bl_dh, xchg_ah_dh, xchg_ch_dh, loop, xchg_bh_dh
	.word xchg_al_bh, xchg_cl_bh, xchg_dl_bh, xchg_bl_bh, xchg_ah_bh, xchg_ch_bh, xchg_dh_bh, loop

	modrm_0_genall xchg

// ------------------- 87 = XCHG r32,r/m32 -----------------------------
op_87_USE32:
	modrm_jump_32
	modrm_tbl_1 xchg

.macro xchg_t0_reg reg
	.global	xchg_t0_r32_bp_\reg
xchg_t0_r32_bp_\reg:
	//-------
	// Setup the correct segment when indexing by BP register.
	//-------
	mem_handler_bp
	.global	xchg_t0_r32_\reg
xchg_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr		r0, [r2]
	str		\reg, [r2]
	mov		\reg, r0
#else
1:	ldrb	r0, [r2]
	strb	\reg, [r2]
	lsr		\reg, #8
	ldrb	r1, [r2, #1]
	strb	\reg, [r2, #1]
	lsr		\reg, #8
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #2]
	strb	\reg, [r2, #2]
	lsr		\reg, #8
	orr		r0, r1, lsl #16
	ldrb	r1, [r2, #3]
	strb	\reg, [r2, #3]
	orr		\reg, r0, r1, lsl #24
#endif
	b		loop
.endm

	xchg_t0_reg eax
	xchg_t0_reg ecx
	xchg_t0_reg edx
	xchg_t0_reg ebx
	xchg_t0_reg esp
	xchg_t0_reg ebp
	xchg_t0_reg esi
	xchg_t0_reg edi
	
	modrm_1_genall xchg xchg_t0_r32
	
.macro xchg_reg32_reg32 rl rr
	mov		r0, \rl
	mov		\rl, \rr
	mov		\rr, r0
	b		loop						// Back to loop
.endm

	modrm_gen_reg_oper xchg eax
	modrm_gen_reg_oper xchg ecx
	modrm_gen_reg_oper xchg edx
	modrm_gen_reg_oper xchg ebx
	modrm_gen_reg_oper xchg esp
	modrm_gen_reg_oper xchg ebp
	modrm_gen_reg_oper xchg esi
	modrm_gen_reg_oper xchg edi

// ------------------- 88 = MOV r/m8,r8 -------------------------------
	.global	op_88_USE32
op_88_USE32:
	modrm_jump_32
	modrm_tbl_0 mov
	.word loop, mov_cl_al, mov_dl_al, mov_bl_al, mov_ah_al, mov_ch_al, mov_dh_al, mov_bh_al
	.word mov_al_cl, loop, mov_dl_cl, mov_bl_cl, mov_ah_cl, mov_ch_cl, mov_dh_cl, mov_bh_cl
	.word mov_al_dl, mov_cl_dl, loop, mov_bl_dl, mov_ah_dl, mov_ch_dl, mov_dh_dl, mov_bh_dl
	.word mov_al_bl, mov_cl_bl, mov_dl_bl, loop, mov_ah_bl, mov_ch_bl, mov_dh_bl, mov_bh_bl
	.word mov_al_ah, mov_cl_ah, mov_dl_ah, mov_bl_ah, loop, mov_ch_ah, mov_dh_ah, mov_bh_ah
	.word mov_al_ch, mov_cl_ch, mov_dl_ch, mov_bl_ch, mov_ah_ch, loop, mov_dh_ch, mov_bh_ch
	.word mov_al_dh, mov_cl_dh, mov_dl_dh, mov_bl_dh, mov_ah_dh, mov_ch_dh, loop, mov_bh_dh
	.word mov_al_bh, mov_cl_bh, mov_dl_bh, mov_bl_bh, mov_ah_bh, mov_ch_bh, mov_dh_bh, loop

	modrm_0_genall mov
	
.macro mov_disp32_reg8l reg
	r0_from_disp32
	b		mov_r0_r8l_\reg
.endm

	.global	op_a2_USE32
op_a2_USE32:
	mvn		r3, #0				// Use 32-bit memory address masking
	mov_disp32_reg8l eax

// ------------------- 89 = MOV r/m32,r32 ------------------------------
// 6667891DA8140000 = mov [000014A8],ebx
//
op_89_USE32:
	modrm_jump_32
	modrm_tbl_1 mov

.macro mov_t0_reg reg
	.global	mov_t0_r32_bp_\reg
mov_t0_r32_bp_\reg:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	mov_t0_r32_\reg
mov_t0_r32_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f op_89_EGA_USE32_\reg op_89_MODEX_USE32_\reg
#if USE_UNALIGNED
1:	str		\reg, [r2]
#else
1:	strb	\reg, [r2]
	lsr		r0, \reg, #8
	strb	r0, [r2, #1]
	lsr		r0, \reg, #16
	strb	r0, [r2, #2]
	lsr		r0, \reg, #24
	strb	r0, [r2, #3]
#endif
	b		loop
.endm

	mov_t0_reg eax
	mov_t0_reg ecx
	mov_t0_reg edx
	mov_t0_reg ebx
	mov_t0_reg esp
	mov_t0_reg ebp
	mov_t0_reg esi
	mov_t0_reg edi

	modrm_1_genall mov mov_t0_r32

op_a3_USE32:
	mvn		r3, #0				// Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_t0_r32_r4

// ------------------- 8A = MOV r8,r/m8 --------------------------------
	.global	op_8a_USE32
op_8a_USE32:
	modrm_jump_32
	modrm_tbl_2 mov
	.word loop, mov_al_cl, mov_al_dl, mov_al_bl, mov_al_ah, mov_al_ch, mov_al_dh, mov_al_bh
	.word mov_cl_al, loop, mov_cl_dl, mov_cl_bl, mov_cl_ah, mov_cl_ch, mov_cl_dh, mov_cl_bh
	.word mov_dl_al, mov_dl_cl, loop, mov_dl_bl, mov_dl_ah, mov_dl_ch, mov_dl_dh, mov_dl_bh
	.word mov_bl_al, mov_bl_cl, mov_bl_dl, loop, mov_bl_ah, mov_bl_ch, mov_bl_dh, mov_bl_bh
	.word mov_ah_al, mov_ah_cl, mov_ah_dl, mov_ah_bl, loop, mov_ah_ch, mov_ah_dh, mov_ah_bh
	.word mov_ch_al, mov_ch_cl, mov_ch_dl, mov_ch_bl, mov_ch_ah, loop, mov_ch_dh, mov_ch_bh
	.word mov_dh_al, mov_dh_cl, mov_dh_dl, mov_dh_bl, mov_dh_ah, mov_dh_ch, loop, mov_dh_bh
	.word mov_bh_al, mov_bh_cl, mov_bh_dl, mov_bh_bl, mov_bh_ah, mov_bh_ch, mov_bh_dh, loop

	modrm_2_genall mov

.macro mov_reg8l_disp32 reg
	r0_from_disp32
	b		mov_r8l_r0_\reg
.endm
	.global	op_a0_USE32
op_a0_USE32:
	mvn		r3, #0				// Use 32-bit memory address masking
	mov_reg8l_disp32 eax

// ------------------- 8B = MOV r32,r/m32 -------------------------------
// 66678B04B543120000 = mov eax,[00001234+esi*4]
//
	.global	op_8b_USE32
op_8b_USE32:
	modrm_jump_32
	modrm_tbl_3 mov

.macro mov_reg32_t0 reg
	.global	mov_r32_t0_bp_\reg
mov_r32_t0_bp_\reg:
	mem_handler_bp
	.global	mov_r32_t0_\reg
mov_r32_t0_\reg:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f op_8b_EGA_USE32_\reg op_8b_MODEX_USE32_\reg
#if USE_UNALIGNED
1:	ldr		\reg, [r2]
#else
1:	ldrb	\reg, [r2]
	ldrb	r0, [r2, #1]
	ldrb	r1, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		\reg, r0, lsl #8
	orr		\reg, r1, lsl #16
	orr		\reg, r2, lsl #24
#endif
	b		loop
.endm

	mov_reg32_t0 eax
	mov_reg32_t0 ecx
	mov_reg32_t0 edx
	mov_reg32_t0 ebx
	mov_reg32_t0 esp
	mov_reg32_t0 ebp
	mov_reg32_t0 esi
	mov_reg32_t0 edi


.macro mov_reg32_reg32 rl rr
	mov		\rl, \rr
	b		loop
.endm

	modrm_3_genall mov mov_r32_t0

op_a1_USE32:
	mvn		r3, #0				// Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_r32_t0_r4

// ------------------- 8C = MOV r/m32,Sreg -----------------------------
//

	.global	op_8c_USE32
op_8c_USE32:
	modrm_jump_32
// 0
	modrm_help_1_0 mov es
	modrm_help_1_0 mov cs
	modrm_help_1_0 mov ss
	modrm_help_1_0 mov ds
	modrm_help_1_0 mov fs
	modrm_help_1_0 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 mov es
	modrm_help_1_40 mov cs
	modrm_help_1_40 mov ss
	modrm_help_1_40 mov ds
	modrm_help_1_40 mov fs
	modrm_help_1_40 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 mov es
	modrm_help_1_80 mov cs
	modrm_help_1_80 mov ss
	modrm_help_1_80 mov ds
	modrm_help_1_80 mov fs
	modrm_help_1_80 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0 = mod = 11b = register
	modrm_help_1_C0 mov es_r
	modrm_help_1_C0 mov cs_r
	modrm_help_1_C0 mov ss_r
	modrm_help_1_C0 mov ds_r
	modrm_help_1_C0 mov fs_r
	modrm_help_1_C0 mov gs_r
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	// Call the handlers in "cpu.s", as we are to write only 16 bits to RAM

	modrm_1_help mov mov_r0 es
	modrm_1_help mov mov_r0 cs
	modrm_1_help mov mov_r0 ss
	modrm_1_help mov mov_r0 ds
	modrm_1_help mov mov_r0 fs
	modrm_1_help mov mov_r0 gs

	// These handlers are called also from "cpu_67.s", so publish them:
	
	.global	mov_eaxidx_es, mov_ecxidx_es, mov_edxidx_es, mov_SIB_es, mov_disp32_es
	.global	mov_eaxidx_cs, mov_ecxidx_cs, mov_edxidx_cs, mov_SIB_cs, mov_disp32_cs
	.global	mov_eaxidx_ss, mov_ecxidx_ss, mov_edxidx_ss, mov_SIB_ss, mov_disp32_ss
	.global	mov_eaxidx_ds, mov_ecxidx_ds, mov_edxidx_ds, mov_SIB_ds, mov_disp32_ds
	.global	mov_eaxidx_fs, mov_ecxidx_fs, mov_edxidx_fs, mov_SIB_fs, mov_disp32_fs
	.global	mov_eaxidx_gs, mov_ecxidx_gs, mov_edxidx_gs, mov_SIB_gs, mov_disp32_gs
	.global	mov_eaxd8_es, mov_ecxd8_es, mov_edxd8_es, mov_SIBd8_es
	.global	mov_eaxd8_cs, mov_ecxd8_cs, mov_edxd8_cs, mov_SIBd8_cs
	.global	mov_eaxd8_ss, mov_ecxd8_ss, mov_edxd8_ss, mov_SIBd8_ss
	.global	mov_eaxd8_ds, mov_ecxd8_ds, mov_edxd8_ds, mov_SIBd8_ds
	.global	mov_eaxd8_fs, mov_ecxd8_fs, mov_edxd8_fs, mov_SIBd8_fs
	.global	mov_eaxd8_gs, mov_ecxd8_gs, mov_edxd8_gs, mov_SIBd8_gs
	.global	mov_eaxd32_es, mov_ecxd32_es, mov_edxd32_es, mov_ebxd32_es, mov_SIBd32_es, mov_ebpd32_es, mov_esid32_es, mov_edid32_es
	.global	mov_eaxd32_cs, mov_ecxd32_cs, mov_edxd32_cs, mov_ebxd32_cs, mov_SIBd32_cs, mov_ebpd32_cs, mov_esid32_cs, mov_edid32_cs
	.global	mov_eaxd32_ss, mov_ecxd32_ss, mov_edxd32_ss, mov_ebxd32_ss, mov_SIBd32_ss, mov_ebpd32_ss, mov_esid32_ss, mov_edid32_ss
	.global	mov_eaxd32_ds, mov_ecxd32_ds, mov_edxd32_ds, mov_ebxd32_ds, mov_SIBd32_ds, mov_ebpd32_ds, mov_esid32_ds, mov_edid32_ds
	.global	mov_eaxd32_fs, mov_ecxd32_fs, mov_edxd32_fs, mov_ebxd32_fs, mov_SIBd32_fs, mov_ebpd32_fs, mov_esid32_fs, mov_edid32_fs
	.global	mov_eaxd32_gs, mov_ecxd32_gs, mov_edxd32_gs, mov_ebxd32_gs, mov_SIBd32_gs, mov_ebpd32_gs, mov_esid32_gs, mov_edid32_gs
	
	.ltorg
	
.macro mov_reg32_Sreg_r reg rr sreg
	.global mov_\reg\()_\rr
mov_\reg\()_\rr:
	ldr		\reg, [sp, #\sreg]
	b		loop
.endm

	mov_reg32_Sreg_r eax, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r ecx, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r edx, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r ebx, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r esp, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r ebp, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r esi, es_r, SP_ES_VALUE
	mov_reg32_Sreg_r edi, es_r, SP_ES_VALUE

	mov_reg32_Sreg_r eax, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r ecx, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r edx, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r ebx, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r esp, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r ebp, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r esi, cs_r, SP_CS_VALUE
	mov_reg32_Sreg_r edi, cs_r, SP_CS_VALUE

	mov_reg32_Sreg_r eax, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r ecx, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r edx, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r ebx, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r esp, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r ebp, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r esi, ss_r, SP_SS_VALUE
	mov_reg32_Sreg_r edi, ss_r, SP_SS_VALUE

	mov_reg32_Sreg_r eax, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r ecx, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r edx, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r ebx, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r esp, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r ebp, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r esi, ds_r, SP_DS_VALUE
	mov_reg32_Sreg_r edi, ds_r, SP_DS_VALUE

	mov_reg32_Sreg_r eax, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r ecx, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r edx, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r ebx, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r esp, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r ebp, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r esi, fs_r, SP_FS_VALUE
	mov_reg32_Sreg_r edi, fs_r, SP_FS_VALUE

	mov_reg32_Sreg_r eax, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r ecx, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r edx, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r ebx, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r esp, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r ebp, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r esi, gs_r, SP_GS_VALUE
	mov_reg32_Sreg_r edi, gs_r, SP_GS_VALUE

	
// ------------------- 8D = LEA r32,m ----------------------------------
// 66678D4304 = lea eax,[ebx+04]
// 66678D042B = lea eax,[ebx+ebp]
//
op_8d_USE32:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0								// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	modrm_help_3_0 lea eax
	modrm_help_3_0 lea ecx
	modrm_help_3_0 lea edx
	modrm_help_3_0 lea ebx
	modrm_help_3_0 lea esp
	modrm_help_3_0 lea ebp
	modrm_help_3_0 lea esi
	modrm_help_3_0 lea edi
// 0x40
	modrm_help_3_40 lea eax
	modrm_help_3_40 lea ecx
	modrm_help_3_40 lea edx
	modrm_help_3_40 lea ebx
	modrm_help_3_40 lea esp
	modrm_help_3_40 lea ebp
	modrm_help_3_40 lea esi
	modrm_help_3_40 lea edi
// 0x80
	modrm_help_3_80 lea eax
	modrm_help_3_80 lea ecx
	modrm_help_3_80 lea edx
	modrm_help_3_80 lea ebx
	modrm_help_3_80 lea esp
	modrm_help_3_80 lea ebp
	modrm_help_3_80 lea esi
	modrm_help_3_80 lea edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

.macro lea_help reg
	//------
	// Need to have stack-relative code 4 bytes before the actual handler,
	// as the SIB code returns to 4 bytes before the handler when using
	// ESP or EBP as the base register!
	//------ 
	nop	
lea_SIB_return_\reg:
	and		\reg, r0, r3
	b		loop
// ----- 0x00 -----
lea_\reg\()_eaxidx:
	mov		\reg, eax
	b		loop
lea_\reg\()_ecxidx:
	mov		\reg, ecx
	b		loop
lea_\reg\()_edxidx:
	mov		\reg, edx
	b		loop
lea_\reg\()_ebxidx:
	mov		\reg, ebx
	b		loop
lea_\reg\()_SIB:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_disp32:
	get_cseip_dword \reg
	b		loop
lea_\reg\()_esiidx:
	mov		\reg, esi
	b		loop
lea_\reg\()_ediidx:
	mov		\reg, edi
	b		loop
// ----- 0x40 = disp8 -----
lea_\reg\()_eaxd8:
	ldrsb	r0,[r12],#1
	add		\reg, eax, r0
	b		loop
lea_\reg\()_ecxd8:
	ldrsb	r0,[r12],#1
	add		\reg, ecx, r0
	b		loop
lea_\reg\()_edxd8:
	ldrsb	r0,[r12],#1
	add		\reg, edx, r0
	b		loop
lea_\reg\()_ebxd8:
	ldrsb	r0,[r12],#1
	add		\reg, ebx, r0
	b		loop
lea_\reg\()_SIBd8:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_ebpd8:
	ldrsb	r0,[r12],#1
	add		\reg, ebp, r0
	b		loop
lea_\reg\()_esid8:
	ldrsb	r0,[r12],#1
	add		\reg, esi, r0
	b		loop
lea_\reg\()_edid8:
	ldrsb	r0,[r12],#1
	add		\reg, edi, r0
	b		loop
// ----- 0x80 = disp32 -----
lea_\reg\()_eaxd32:
	get_cseip_dword r0
	add		\reg, r0, eax
	b		loop
lea_\reg\()_ecxd32:
	get_cseip_dword r0
	add		\reg, r0, ecx
	b		loop
lea_\reg\()_edxd32:
	get_cseip_dword r0
	add		\reg, r0, edx
	b		loop
lea_\reg\()_ebxd32:
	get_cseip_dword r0
	add		\reg, r0, ebx
	b		loop
lea_\reg\()_SIBd32:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_ebpd32:
	get_cseip_dword r0
	add		\reg, r0, ebp
	b		loop
lea_\reg\()_esid32:
	get_cseip_dword r0
	add		\reg, r0, esi
	b		loop
lea_\reg\()_edid32:
	get_cseip_dword r0
	add		\reg, r0, edi
	b		loop
.endm

	lea_help eax
	lea_help ecx
	lea_help edx
	lea_help ebx
	lea_help esp
	lea_help ebp
	lea_help esi
	lea_help edi

	.ltorg
	
// ------------------- 8E = MOV Sreg,r/m16 -----------------------------

	.global	op_8e_USE32
op_8e_USE32:
	modrm_jump_32
// 0
	modrm_help_2_0 mov es
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_2_0 mov ss
	modrm_help_2_0 mov ds
	modrm_help_2_0 mov fs
	modrm_help_2_0 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_2_40 mov es
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_2_40 mov ss
	modrm_help_2_40 mov ds
	modrm_help_2_40 mov fs
	modrm_help_2_40 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_2_80 mov es
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_2_80 mov ss
	modrm_help_2_80 mov ds
	modrm_help_2_80 mov fs
	modrm_help_2_80 mov gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0 = mod = 11b = register
	.word mov_es_ax, mov_es_cx, mov_es_dx, mov_es_bx, mov_es_sp, mov_es_bp, mov_es_si, mov_es_di
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word mov_ss_ax, mov_ss_cx, mov_ss_dx, mov_ss_bx, mov_ss_sp, mov_ss_bp, mov_ss_si, mov_ss_di
	.word mov_ds_ax, mov_ds_cx, mov_ds_dx, mov_ds_bx, mov_ds_sp, mov_ds_bp, mov_ds_si, mov_ds_di
	.word mov_fs_ax, mov_fs_cx, mov_fs_dx, mov_fs_bx, mov_fs_sp, mov_fs_bp, mov_fs_si, mov_fs_di
	.word mov_gs_ax, mov_gs_cx, mov_gs_dx, mov_gs_bx, mov_gs_sp, mov_gs_bp, mov_gs_si, mov_gs_di
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_2_help mov mov es r0_to_es
	modrm_2_help mov mov ss r0_to_ss
	modrm_2_help mov mov ds r0_to_ds
	modrm_2_help mov mov fs r0_to_fs
	modrm_2_help mov mov gs r0_to_gs


// ------------------- 8F = POP m32 ------------------------------------
op_8f_USE32:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0				// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	modrm_help_1_0 pop m32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 pop m32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 pop m32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0
	.word op_58_USE32, op_59_USE32, op_5a_USE32, op_5b_USE32, op_5c_USE32, op_5d_USE32, op_5e_USE32, op_5f_USE32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	.global	pop_t0_bp_m32
pop_t0_bp_m32:
	mem_handler_bp
	.global	pop_t0_m32
pop_t0_m32:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	pop_dword	r0, r1, r3
#if USE_UNALIGNED
	str		r0, [r2]
#else
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
#endif
	b		loop

	modrm_1_help pop pop_t0 m32

	.ltorg

// ------------------- 98 = CWDE --------------------------------------
	.global op_98_USE32
op_98_USE32:
	lsl		eax, #16
	asr		eax, #16					// eax = sign-extended ax value
	b		loop

// ------------------- 99 = CDQ ---------------------------------------
	.global op_99_USE32
op_99_USE32:
	asr		edx, eax, #31
	b		loop

// ------------------- 9A = CALL FAR ptr16:32 --------------------------
// BC Racers: 0160:001884E0 9A45CD18006001  call 0160:0018CD45
//
	.global op_9a_USE32
op_9a_USE32:
	//-------
	// First get the call target :
	//	r1 = new IP (offset)
	//	r2 = new CS (segment)
	//-------
	mov		r0, #32
	str		r0, [sp, #SP_FREE3]		// Save the flag telling this is a USE32 call far
#if USE_UNALIGNED
	ldr		r1, [r12], #4
	ldrh	r2, [r12], #2
#else
	ldrb	r0,[r12], #1
	ldrb	r1,[r12], #1
	ldrb	r2,[r12], #1
	orr		r1, r0, r1, lsl #8
	ldrb	r0,[r12], #1
	orr		r1, r2, lsl #16
	ldrb	r2,[r12], #1
	ldrb	r3,[r12], #1
	orr		r1, r0, lsl #24			// r1 = new logical IP
	orr		r2, r3, lsl #8			// r2 = new CS value
#endif
	b		cpu_call_far_r1r2

// ------------------- 9C = PUSHFD ------------------------------------
//
//	if (cpu.pmode && GETFLAG(VM) && (GETFLAG(IOPL)!=FLAG_IOPL)) {
//		/* Not enough privileges to execute PUSHF */
//		return CPU_PrepareException(EXCEPTION_GP,0);
//	}
//
	.global	op_9c_USE32
op_9c_USE32:
	ldrb	r3, [sp, #SP_CPU_CR0]	// Get the lowest byte of cpu_cr0
	ldr		r1, [sp, #SP_FLAGS]		// Get the "Trap", "Interrupts Enabled" and "Direction" flags
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	tst		r3, #1					// Are we in protected mode?
	bne		.op_9c_prot
.op_9c_cont:
	msr		cpsr_f, r0				// Restore flags
	join_ARM_to_X86_flags r1
	bic		r1, #0x00030000
	bic		r1, #0xFF000000			// CPU_Push32(reg_flags & 0xfcffff);
	push_dword r1 r0 r2
	b		loop
.op_9c_prot:
	tst		r1, #FLAG_VM
	beq		.op_9c_cont
	and		r2, r1, #FLAG_IOPL
	cmp		r2, #FLAG_IOPL
	beq		.op_9c_cont
	b		.unknown				// EXCEPTION_GP!

// ------------------- 9D = POPF --------------------------------------
//	if (cpu.pmode && GETFLAG(VM) && (GETFLAG(IOPL)!=FLAG_IOPL)) {
//		/* Not enough privileges to execute POPF */
//		return CPU_PrepareException(EXCEPTION_GP,0);
//	}
//	Bitu mask=FMASK_ALL;
//	/* IOPL field can only be modified when CPL=0 or in real mode: */
//	if (cpu.pmode && (cpu.cpl>0)) mask &= (~FLAG_IOPL);
//	if (cpu.pmode && !GETFLAG(VM) && (GETFLAG_IOPL<cpu.cpl)) mask &= (~FLAG_IF);
//	if (use32)
//		CPU_SetFlags(CPU_Pop32(),mask);
//	else CPU_SetFlags(CPU_Pop16(),mask & 0xffff);
//	DestroyConditionFlags();
//	return false;
//
	.global	op_9d_USE32
op_9d_USE32:
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	ldr		r2, =FMASK_ALL						// r2 = mask of the flag bits we can change
	tst		r3, #1								// Are we in protected mode?
	bne		.op_9d_prot
.op_9d_cont:
	pop_dword r0 r1 r3
	ldr		r1, [sp, #SP_FLAGS]					// Get the #SP_FLAGS
	and		r0, r2								// Leave only the bits we can change to r0
	bic		r2, r1, r2							// r1 = flags bits that will not change
	orr		r0, r2
	str		r0, [sp, #SP_FLAGS]					// Store the new #SP_FLAGS
	b		iret_cont_flags_old_r1_new_r0
.op_9d_prot:
	ldr		r1, [sp, #SP_FLAGS]					// Get the #SP_FLAGS
	ldrb	r0, [sp, #SP_CPU_CPL]
	tst		r1, #FLAG_VM						// Are we in VM mode?
	and		r3, r1, #FLAG_IOPL
	cmpne	r3, #FLAG_IOPL						// If we are in VM mode, check FLAG_IOPL
	bne		.unknown							// EXCEPTION_GP!
	cmp		r0, #0								// if (cpu.pmode && (cpu.cpl>0)) 
	bicgt	r2, #FLAG_IOPL						//	mask &= (~FLAG_IOPL);
	cmp		r3, r0, lsl #12						// (GETFLAG_IOPL<cpu.cpl)
	biclt	r2, #FLAG_IF						//	mask &= (~FLAG_IF);
	b		.op_9d_cont

// ------------------- A9 = TEST EAX,imm32 -----------------------------
	.global	op_a9_USE32
op_a9_USE32:
#if USE_UNALIGNED
	ldr		r0, [r12], #4
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
#else
	ldrb	r0, [r12],#1
	ldrb	r1, [r12],#1
	ldrb	r3, [r12],#1
	ldrb	r2, [r12],#1
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
#endif
	tst		eax, r0
	b		loop						// Back to loop

// =================== B8..BF = MOV reg32,imm32 ========================
// Opcodes B8..BF for AX, CX, DX, BX, SP, BP, SI, DI
//
.macro mov_reg_imm32 reg
	get_cseip_dword \reg
	b		loop
.endm

	.global	op_b8_USE32
	.global	op_b9_USE32
	.global	op_ba_USE32
	.global	op_bb_USE32
	.global	op_bc_USE32
	.global	op_bd_USE32
	.global	op_be_USE32
	.global	op_bf_USE32
	
op_b8_USE32:
	mov_reg_imm32 eax
op_b9_USE32:
	mov_reg_imm32 ecx
op_ba_USE32:
	mov_reg_imm32 edx
op_bb_USE32:
	mov_reg_imm32 ebx
op_bc_USE32:
	mov_reg_imm32 esp
op_bd_USE32:
	mov_reg_imm32 ebp
op_be_USE32:
	mov_reg_imm32 esi
op_bf_USE32:
	mov_reg_imm32 edi

	.ltorg
	
// ------------------- C0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,imm8 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_c0_USE32:
	modrm_jump_32
	modrm_tbl_0_oper rol_b ror_b rcl_b rcr_b shl_b shr_b shl_b sar_b imm8
	.word rol_al_imm8, rol_cl_imm8, rol_dl_imm8, rol_bl_imm8, rol_ah_imm8, rol_ch_imm8, rol_dh_imm8, rol_bh_imm8
	.word ror_al_imm8, ror_cl_imm8, ror_dl_imm8, ror_bl_imm8, ror_ah_imm8, ror_ch_imm8, ror_dh_imm8, ror_bh_imm8
	.word rcl_al_imm8, rcl_cl_imm8, rcl_dl_imm8, rcl_bl_imm8, rcl_ah_imm8, rcl_ch_imm8, rcl_dh_imm8, rcl_bh_imm8
	.word rcr_al_imm8, rcr_cl_imm8, rcr_dl_imm8, rcr_bl_imm8, rcr_ah_imm8, rcr_ch_imm8, rcr_dh_imm8, rcr_bh_imm8
	.word shl_al_imm8, shl_cl_imm8, shl_dl_imm8, shl_bl_imm8, shl_ah_imm8, shl_ch_imm8, shl_dh_imm8, shl_bh_imm8
	.word shr_al_imm8, shr_cl_imm8, shr_dl_imm8, shr_bl_imm8, shr_ah_imm8, shr_ch_imm8, shr_dh_imm8, shr_bh_imm8
	.word shl_al_imm8, shl_cl_imm8, shl_dl_imm8, shl_bl_imm8, shl_ah_imm8, shl_ch_imm8, shl_dh_imm8, shl_bh_imm8
	.word sar_al_imm8, sar_cl_imm8, sar_dl_imm8, sar_bl_imm8, sar_ah_imm8, sar_ch_imm8, sar_dh_imm8, sar_bh_imm8

	modrm_genall_0_oper rol_b ror_b rcl_b rcr_b shl_b shr_b "skip" sar_b _r0 imm8

// ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,imm8 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_c1_USE32:
	modrm_jump_32
	modrm_tbl_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw imm8
	//0xc0 = mod = 11b => two register operands
	.word rol_eax_imm8, rol_ecx_imm8, rol_edx_imm8, rol_ebx_imm8, rol_esp_imm8, rol_ebp_imm8, rol_esi_imm8, rol_edi_imm8
	.word ror_eax_imm8, ror_ecx_imm8, ror_edx_imm8, ror_ebx_imm8, ror_esp_imm8, ror_ebp_imm8, ror_esi_imm8, ror_edi_imm8
	.word rcl_eax_imm8, rcl_ecx_imm8, rcl_edx_imm8, rcl_ebx_imm8, rcl_esp_imm8, rcl_ebp_imm8, rcl_esi_imm8, rcl_edi_imm8
	.word rcr_eax_imm8, rcr_ecx_imm8, rcr_edx_imm8, rcr_ebx_imm8, rcr_esp_imm8, rcr_ebp_imm8, rcr_esi_imm8, rcr_edi_imm8
	.word shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	.word shr_eax_imm8, shr_ecx_imm8, shr_edx_imm8, shr_ebx_imm8, shr_esp_imm8, shr_ebp_imm8, shr_esi_imm8, shr_edi_imm8
	.word shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	.word sar_eax_imm8, sar_ecx_imm8, sar_edx_imm8, sar_ebx_imm8, sar_esp_imm8, sar_ebp_imm8, sar_esi_imm8, sar_edi_imm8

	modrm_genall_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw "skip" sar_dw _t0 imm8

.macro opc1common oper
	.global	\oper\()_dw_t0_bp_imm8
\oper\()_dw_t0_bp_imm8:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	\oper\()_dw_t0_imm8
\oper\()_dw_t0_imm8:
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r1, [r12], #1						// r1 = rotation count
\oper\()_d3_cont:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr								// Save flags to r0
	//=======
	// Check for special cases (rotate by 0, 1, or 16 bit positions)
	//=======
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
.endm

// ----- ROL -----

	opc1common rol
	beq		1f 						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Rotate the value by 2..31 bit positions. Only Carry flag will change.
	//=======
	bic		r0, #ARM_CARRY			// Clear the current Carry flag
	msr		cpsr_f, r0				// Restore flags
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r0, r3, lsl #24
	rsb		r1, #32
	ror		r1, r0, r1
	mrs		r0,cpsr					// Get flags to r0
	tst		r1, #1					// Is the lowest bit set (need to set Carry)?
	strb	r1, [r2]
	lsr		r1, #8
	orrne	r0, #ARM_CARRY			// If it is set, set the Carry in the saved flags in r0
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate the value by 1 bit position. Carry and Overflow flags will change.
	//=======
1:	msr		cpsr_f,r0				// Restore flags from r0
rol_dw_r2_1_RAM:					// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				// Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	ror		r1, r0, #31				// == rol r1,r0, #1
	mrs		r0,cpsr					// Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	tst		r1, #1					// If lowest bit is set, need to set Carry
	orrne	r0, #ARM_CARRY
	teq		r1, r1, lsl #31			// Negative flag set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

	.global rol_eax_imm8 
	.global rol_ecx_imm8 
	.global rol_edx_imm8 
	.global rol_ebx_imm8 
	.global rol_esp_imm8 
	.global rol_ebp_imm8 
	.global rol_esi_imm8 
	.global rol_edi_imm8

.macro rol_reg32_imm8 reg
	//=======
	// Check for special cases (rotate by 0 or 1 bit positions)
	//=======
	ldrb	r1, [r12], #1						// r1 = rotation count
rol_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		1f						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Rotate the value by 2..31 bit positions. Only Carry flag will change.
	//=======
	bic		r0, #ARM_CARRY			// Clear the current Carry flag
	rsb		r1, #32
	ror		\reg, \reg, r1
	tst		\reg, #1				// Is the lowest bit set (need to set Carry)?
	orrne	r0, #ARM_CARRY			// If it is set, set the Carry in the saved flags in r0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate by a single bit position.
	//=======	
	.global	rol_reg32_1_\reg
rol_reg32_1_\reg:					// Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					// Save flags to r0
1:	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	ror		\reg, \reg, #31
	tst		\reg, #1				// If lowest bit is set, need to set Carry
	orrne	r0, #ARM_CARRY
	teq		\reg, \reg, lsl #31		// Negative flag set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
.endm

rol_eax_imm8:
	rol_reg32_imm8 eax
rol_ecx_imm8:
	rol_reg32_imm8 ecx
rol_edx_imm8:
	rol_reg32_imm8 edx
rol_ebx_imm8:
	rol_reg32_imm8 ebx
rol_esp_imm8:
	rol_reg32_imm8 esp
rol_ebp_imm8:
	rol_reg32_imm8 ebp
rol_esi_imm8:
	rol_reg32_imm8 esi
rol_edi_imm8:
	rol_reg32_imm8 edi

// ----- ROR -----

	opc1common ror
	beq		1f 						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Rotate the value by 2..31 bit positions. Only Carry flag will change.
	//=======
	bic		r0, #ARM_CARRY			// Clear the current Carry flag
	msr		cpsr_f, r0				// Restore flags
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r3, r0, r3, lsl #24
	mrs		r0,cpsr					// Get flags to r0
	movs	r1, r3, ror r1
	strb	r1, [r2]
	lsr		r1, #8
	orrcs	r0, #ARM_CARRY			// If Carry is set, set the Carry in the saved flags in r0
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate the value by 1 bit position. Carry and Overflow flags will change.
	//=======
1:	msr		cpsr_f,r0				// Restore flags from r0
ror_dw_r2_1_RAM:					// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				// Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r3, r0, r1, lsl #24
	mrs		r0,cpsr					// Save flags to r0
	movs	r1, r3, ror #1
	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	orrcs	r0, #ARM_CARRY			// If Carry is set, set the Carry in the saved flags in r0
	teq		r1, r1, lsl #1			// Negative flag set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

	.global ror_eax_imm8 
	.global ror_ecx_imm8 
	.global ror_edx_imm8 
	.global ror_ebx_imm8 
	.global ror_esp_imm8 
	.global ror_ebp_imm8 
	.global ror_esi_imm8 
	.global ror_edi_imm8

.macro ror_reg32_imm8 reg
	//=======
	// Check for special cases (rotate by 0 or 1 bit positions)
	//=======
	ldrb	r1, [r12], #1						// r1 = rotation count
ror_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		1f						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Rotate the value by 2..31 bit positions. Only Carry flag will change.
	//=======
	bic		r0, #ARM_CARRY			// Clear the current Carry flag
	movs	\reg, \reg, ror r1
	orrcs	r0, #ARM_CARRY			// If Carry is set, set the Carry in the saved flags in r0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate by a single bit position.
	//=======	
	.global	ror_reg32_1_\reg
ror_reg32_1_\reg:					// Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					// Save flags to r0
1:	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	movs	\reg, \reg, ror #1
	orrcs	r0, #ARM_CARRY
	teq		\reg, \reg, lsl #1		// Negative flag set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
.endm

ror_eax_imm8:
	ror_reg32_imm8 eax
ror_ecx_imm8:
	ror_reg32_imm8 ecx
ror_edx_imm8:
	ror_reg32_imm8 edx
ror_ebx_imm8:
	ror_reg32_imm8 ebx
ror_esp_imm8:
	ror_reg32_imm8 esp
ror_ebp_imm8:
	ror_reg32_imm8 ebp
ror_esi_imm8:
	ror_reg32_imm8 esi
ror_edi_imm8:
	ror_reg32_imm8 edi

// ----- RCL -----

	opc1common rcl
	beq		1f									// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//-------
	// Perform a 33-bit value rotate. Carry goes to the lowest bit, original highest bit will be the new Carry.
	//-------
	bic		r0, #0xFF							// Clean up the r0 low byte
	orr		r0, r1								// r0 high byte = saved flags, low byte = rotation count
	ldrb	r1, [r2]
	ldrb	r3, [r2, #1]
	orr		r1, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r1, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r1, r3, lsl #24
	//------
	// Perform the rotation
	//------
	movs	r3, r0, lsl #3						// Shift the Carry bit in r0 to the flags (Other result in r3 is irrelevant)
	bic		r0, #ARM_CARRY						// Clear the input carry from r0, it is now in the flags
2:	adcs	r1, r1, r1							// Shift the value 1 bit left, set lowest bit from Carry flag, set Carry flag from the highest bit
	sub		r0, #1								// Decrease the loop counter
	tst		r0, #31								// Loop counter zero? (This does not change the Carry flag)
	bne		2b									// Not yet, rotate one more bit
	//-------
	// Save the result
	//-------
	strb	r1, [r2]
	lsr		r1, #8
	orrcs	r0, #ARM_CARRY						// Put the final Carry flag to r0
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate the value by 1 bit position. Carry and Overflow flags will change.
	//=======
1:	msr		cpsr_f,r0				// Restore flags from r0
rcl_dw_r2_1_RAM:					// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				// Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r1, r0, r1, lsl #24
	//------
	// Perform the rotation
	//------
	mrs		r0,cpsr					// Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	adcs	r1, r1, r1							// Shift the value 1 bit left, set lowest bit from Carry flag, set Carry flag from the highest bit
	//------
	// Save the result
	//------
	orrcs	r0, #ARM_CARRY
	teq		r1, r0, lsl #2					// Negative flag set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

	.global rcl_eax_imm8 
	.global rcl_ecx_imm8 
	.global rcl_edx_imm8 
	.global rcl_ebx_imm8 
	.global rcl_esp_imm8 
	.global rcl_ebp_imm8 
	.global rcl_esi_imm8 
	.global rcl_edi_imm8

.macro rcl_reg32_imm8 reg
	//=======
	// Check for special cases (rotate by 0 or 1 bit positions)
	//=======
	ldrb	r1, [r12], #1						// r1 = rotation count
rcl_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		1f						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//-------
	// Perform a 33-bit value rotate. Carry goes to the lowest bit, original highest bit will be the new Carry.
	//-------
	movs	r3, r0, lsl #3						// Shift the Carry bit in r0 to the flags (Other result in r3 is irrelevant)
	bic		r0, #ARM_CARRY						// Clear the input carry from r0, it is now in the flags
2:	adcs	\reg, \reg, \reg					// Shift the value 1 bit left, set lowest bit from Carry flag, set Carry flag from the highest bit
	sub		r1, #1								// Decrease the loop counter
	tst		r1, #31								// Loop counter zero? (This does not change the Carry flag)
	bne		2b									// Not yet, rotate one more bit
	//-------
	// Save the result
	//-------
	orrcs	r0, #ARM_CARRY						// Put the final Carry flag to r0
	b		restore_flags_from_r0				// Jump back to loop, restoring the flags
	//=======
	// Rotate by a single bit position.
	//=======	
1:	msr		cpsr_f, r0							// Restore flags from r0
	.global	rcl_reg32_1_\reg
rcl_reg32_1_\reg:								// Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr								// Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER				// Clear the Carry and Overflow bits of the saved CPU flags
	adcs	\reg, \reg, \reg					// Shift the value 1 bit left, set lowest bit from Carry flag, set Carry flag from the highest bit
	orrcs	r0, #ARM_CARRY
	eors	r3, \reg, r0, lsl #2				// r3 highest bit (and flags sign bit) set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	b		restore_flags_from_r0				// Jump back to loop, restoring the flags
.endm

rcl_eax_imm8:
	rcl_reg32_imm8 eax
rcl_ecx_imm8:
	rcl_reg32_imm8 ecx
rcl_edx_imm8:
	rcl_reg32_imm8 edx
rcl_ebx_imm8:
	rcl_reg32_imm8 ebx
rcl_esp_imm8:
	rcl_reg32_imm8 esp
rcl_ebp_imm8:
	rcl_reg32_imm8 ebp
rcl_esi_imm8:
	rcl_reg32_imm8 esi
rcl_edi_imm8:
	rcl_reg32_imm8 edi

// ----- RCR -----

	opc1common rcr
	beq		1f									// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//-------
	// Perform a 33-bit value rotate. Carry goes to the lowest bit, original highest bit will be the new Carry.
	//-------
	bic		r0, #0xFF							// Clean up the r0 low byte
	orr		r0, r1								// r0 high byte = saved flags, low byte = rotation count
	ldrb	r1, [r2]
	ldrb	r3, [r2, #1]
	orr		r1, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r1, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r1, r3, lsl #24
	//------
	// Perform the rotation
	//------
	movs	r3, r0, lsl #3						// Shift the Carry bit in r0 to the flags (Other result in r3 is irrelevant)
	bic		r0, #ARM_CARRY						// Clear the input carry from r0, it is now in the flags
2:	movs	r1, r1, rrx							// Shift the value 1 bit right, set highest bit from Carry flag, set Carry flag from the lowest bit
	sub		r0, #1								// Decrease the loop counter
	tst		r0, #31								// Loop counter zero? (This does not change the Carry flag)
	bne		2b									// Not yet, rotate one more bit
	//-------
	// Save the result
	//-------
	strb	r1, [r2]
	lsr		r1, #8
	orrcs	r0, #ARM_CARRY						// Set Carry in r0 if it is set in flags
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0				// Jump back to loop, restoring the flags
	//=======
	// Rotate the value by 1 bit position. Carry and Overflow flags will change.
	//=======
1:	msr		cpsr_f,r0							// Restore flags from r0
rcr_dw_r2_1_RAM:								// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]							// Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r1, r0, r1, lsl #24
	//------
	// Perform the rotation
	//------
	mrs		r0,cpsr					// Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, rrx							// Shift the value 1 bit right, set highest bit from Carry flag, set Carry flag from the lowest bit
	//------
	// Save the result
	//------
	orrcs	r0, #ARM_CARRY
	eors	r3, r1, r1, lsl #1					// r3 highest bit set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

	.global rcr_eax_imm8 
	.global rcr_ecx_imm8 
	.global rcr_edx_imm8 
	.global rcr_ebx_imm8 
	.global rcr_esp_imm8 
	.global rcr_ebp_imm8 
	.global rcr_esi_imm8 
	.global rcr_edi_imm8

.macro rcr_reg32_imm8 reg
	//=======
	// Check for special cases (rotate by 0 or 1 bit positions)
	//=======
	ldrb	r1, [r12], #1						// r1 = rotation count
rcr_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		1f						// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//-------
	// Perform a 33-bit value rotate. Carry goes to the lowest bit, original highest bit will be the new Carry.
	//-------
	movs	r3, r0, lsl #3						// Shift the Carry bit in r0 to the flags (Other result in r3 is irrelevant)
	bic		r0, #ARM_CARRY						// Clear the input carry from r0, it is now in the flags
2:	movs	\reg, \reg, rrx						// Shift the value 1 bit right, set highest bit from Carry flag, set Carry flag from the lowest bit
	sub		r1, #1								// Decrease the loop counter
	tst		r1, #31								// Loop counter zero? (This does not change the Carry flag)
	bne		2b									// Not yet, rotate one more bit
	//-------
	// Save the result
	//-------
	orrcs	r0, #ARM_CARRY						// Set Carry in r0 if it is set in flags
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags
	//=======
	// Rotate by a single bit position.
	//=======	
1:	msr		cpsr_f, r0							// Restore flags from r0
	.global	rcr_reg32_1_\reg
rcr_reg32_1_\reg:								// Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr								// Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER				// Clear the Carry and Overflow bits of the saved CPU flags
	movs	\reg, \reg, rrx						// Shift the value 1 bit right, set highest bit from Carry flag, set Carry flag from the lowest bit
	orrcs	r0, #ARM_CARRY
	eors	r3, \reg, \reg, lsl #1				// r3 highest bit (and flags sign bit) set if we need to set the Overflow flag
	orrmi	r0, #ARM_OVER
	b		restore_flags_from_r0				// Jump back to loop, restoring the flags
.endm

rcr_eax_imm8:
	rcr_reg32_imm8 eax
rcr_ecx_imm8:
	rcr_reg32_imm8 ecx
rcr_edx_imm8:
	rcr_reg32_imm8 edx
rcr_ebx_imm8:
	rcr_reg32_imm8 ebx
rcr_esp_imm8:
	rcr_reg32_imm8 esp
rcr_ebp_imm8:
	rcr_reg32_imm8 ebp
rcr_esi_imm8:
	rcr_reg32_imm8 esi
rcr_edi_imm8:
	rcr_reg32_imm8 edi

// ----- SHL -----

	opc1common shl
	beq		shl_dw_r2_1_RAM						// Yes, go handle that case.
	//=======
	// Shift left by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r0, r3, lsl #24
	movs	r0, r0, lsl r1			// Perform the shift left, setting the flags
	//-------
	// Save the result
	//-------
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
	b		loop
	//=======
	// Shift left by 1 bit position, so handle overflow flag as well.
	//=======
shl_dw_r2_1_RAM:					// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	adds	r0, r0					// Perform the shift left, setting all flags
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
	b		loop

	.global shl_eax_imm8 
	.global shl_ecx_imm8 
	.global shl_edx_imm8 
	.global shl_ebx_imm8 
	.global shl_esp_imm8 
	.global shl_ebp_imm8 
	.global shl_esi_imm8 
	.global shl_edi_imm8

.macro shl_reg32_imm8 reg
	ldrb	r1, [r12], #1						// r1 = rotation count
shl_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		add_\reg\()_\reg		// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Shift left by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	movs	\reg, \reg, lsl r1		// Perform the shift left, setting the flags
	b		loop
.endm

shl_eax_imm8:
	shl_reg32_imm8 eax
shl_ecx_imm8:
	shl_reg32_imm8 ecx
shl_edx_imm8:
	shl_reg32_imm8 edx
shl_ebx_imm8:
	shl_reg32_imm8 ebx
shl_esp_imm8:
	shl_reg32_imm8 esp
shl_ebp_imm8:
	shl_reg32_imm8 ebp
shl_esi_imm8:
	shl_reg32_imm8 esi
shl_edi_imm8:
	shl_reg32_imm8 edi

// ----- SHR -----

	opc1common shr
	beq		shr_dw_r2_1_RAM			// Yes, go handle that case.
	//=======
	// Shift left by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r0, r3, lsl #24
	movs	r0, r0, lsr r1			// Perform the shift, setting the flags
	//-------
	// Save the result
	//-------
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
	b		loop
	//=======
	// Shift right by 1 bit position, so handle overflow flag as well.
	//=======
shr_dw_r2_1_RAM:					// Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	movs	r1, r0, lsr #1			// Perform the shift, setting the flags
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	// ----- Setup the Overflow flag
	mrs		r0,cpsr					// Save flags to r0
	tst		r1, #0x40				// This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0

	.global shr_eax_imm8 
	.global shr_ecx_imm8 
	.global shr_edx_imm8 
	.global shr_ebx_imm8 
	.global shr_esp_imm8 
	.global shr_ebp_imm8 
	.global shr_esi_imm8 
	.global shr_edi_imm8

.macro shr_reg32_imm8 reg
	ldrb	r1, [r12], #1						// r1 = rotation count
shr_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shr_reg32_1_\reg		// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Shift left by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	movs	\reg, \reg, lsr r1		// Perform the shift, setting the flags
	b		loop
	//=======
	// Shift right by 1 bit position, so handle overflow flag as well.
	//=======
	.global	shr_reg32_1_\reg
shr_reg32_1_\reg:					// Directly jump here for single-bit-rotate opcodes
	movs	\reg, \reg, lsr #1		// Perform the shift right, setting the flags
	// ----- Setup the Overflow flag
	mrs		r0,cpsr					// Save flags to r0
	tst		\reg, #0x40000000		// This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0
.endm

shr_eax_imm8:
	shr_reg32_imm8 eax
shr_ecx_imm8:
	shr_reg32_imm8 ecx
shr_edx_imm8:
	shr_reg32_imm8 edx
shr_ebx_imm8:
	shr_reg32_imm8 ebx
shr_esp_imm8:
	shr_reg32_imm8 esp
shr_ebp_imm8:
	shr_reg32_imm8 ebp
shr_esi_imm8:
	shr_reg32_imm8 esi
shr_edi_imm8:
	shr_reg32_imm8 edi

// ----- SAR -----

	opc1common sar
	beq		sar_dw_r2_1_RAM			// Yes, go handle that case.
	//=======
	// Shift left by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	orr		r0, r3, lsl #24
	movs	r0, r0, asr r1			// Perform the shift, setting the flags
	//-------
	// Save the result
	//-------
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
	b		loop
	//=======
	// Arithmetic shift right by 1 bit position, so overflow flag must be cleared.
	//=======
sar_dw_r2_1_RAM:
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	msr		cpsr_f, #0				// Clear all flags (especially Overflow)
	movs	r1, r0, asr #1			// Perform the shift, setting the flags
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		loop

	.global sar_eax_imm8 
	.global sar_ecx_imm8 
	.global sar_edx_imm8 
	.global sar_ebx_imm8 
	.global sar_esp_imm8 
	.global sar_ebp_imm8 
	.global sar_esi_imm8 
	.global sar_edi_imm8

.macro sar_reg32_imm8 reg
	ldrb	r1, [r12], #1						// r1 = rotation count
sar_d3_cont_\reg:								// Opcode d3 jumps here after setting r1 from CL
	mrs		r0,cpsr					// Save flags to r0
	ands	r1, #31					// Mask the rotation count
	beq		restore_flags_from_r0	// If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		sar_reg32_1_\reg		// Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	//=======
	// Shift right by 2..31 bit positions.
	//=======
	msr		cpsr_f,r0				// Restore flags (for Overflow flag, which should not change)
	movs	\reg, \reg, asr r1		// Perform the shift, setting the flags
	b		loop
	//=======
	// Shift right by 1 bit position, so handle overflow flag as well.
	//=======
	.global	sar_reg32_1_\reg
sar_reg32_1_\reg:					// Directly jump here for single-bit-rotate opcodes
	msr		cpsr_f, #0				// Clear all flags (especially Overflow)
	movs	\reg, \reg, asr #1		// Perform the shift right, setting the flags
	b		loop
.endm

sar_eax_imm8:
	sar_reg32_imm8 eax
sar_ecx_imm8:
	sar_reg32_imm8 ecx
sar_edx_imm8:
	sar_reg32_imm8 edx
sar_ebx_imm8:
	sar_reg32_imm8 ebx
sar_esp_imm8:
	sar_reg32_imm8 esp
sar_ebp_imm8:
	sar_reg32_imm8 ebp
sar_esi_imm8:
	sar_reg32_imm8 esi
sar_edi_imm8:
	sar_reg32_imm8 edi

// ------------------- C2 = RETN imm16 ---------------------------------
// Note! This code should be re-executable, as paging may cause a page fault,
// after which this opcode gets re-run from the start. Adjust stack pointer
// only after all page init code has been run!
op_c2_USE32:
	ldrb	r3, [r12], #1
	ldrb	r1, [r12], #1
#if USE_SEIBUSPI
	//-------
	// Since the TLBTable[0] is flagged to use C callbacks, if SP_CS_BASE is zero
	// we need to determine the proper base from 
	//	- TLBTable[0x00200000>>11] (if address >= 0x00200000)
	//	- TLBTable[0x800>>11] (if address < 0x00200000)
	//-------
	mrs		r0,cpsr								// Get original flags to r0.
	orr		r3, r1, lsl #8						// r3 = extra bytes to pop from stack
	pop_dword r12, r1, r2
	ldr		r2, [sp, #SP_TLBTABLE]				// r0 = pointer to the external TLB table
	cmp		r12, #0x00200000
	addge	r2, #0x00200000>>(TLB_GRAN_BITS-2)	// SEIBU SPI hack!
	addlt	r2, #0x800>>(TLB_GRAN_BITS-2)		// SEIBU SPI hack!
	ldr		r2, [r2] 
	add		esp, r3								// Add the extra bytes to ESP
	add		r12, r2								// New physical IP = physical CS:0000 + new logical IP
	str		r2, [sp, #SP_PHYS_CS]				// Save new physical CS to stack
	b		restore_flags_from_r0				// Jump back to loop, setting the flags from r0
#else
	ldr		r2,[sp, #SP_PHYS_CS]				// get the physical CS:0000 into r2
	orr		r3, r1, lsl #8						// r3 = extra bytes to pop from stack
	pop_dword r12, r0, r1
	add		r12, r2								// r12 = new IP + physical CS
	add		esp, r3								// Add the extra bytes to ESP
	b		loop
#endif

// ------------------- C3 = RETN ---------------------------------------
// Note! This code should be re-executable, as paging may cause a page fault,
// after which this opcode gets re-run from the start. Adjust stack pointer
// only after all page init code has been run!
op_c3_USE32:
#if USE_SEIBUSPI
	//-------
	// Since the TLBTable[0] is flagged to use C callbacks, if SP_CS_BASE is zero
	// we need to determine the proper base from 
	//	- TLBTable[0x00200000>>11] (if address >= 0x00200000)
	//	- TLBTable[0x800>>11] (if address < 0x00200000)
	//-------
	ldr		r2, [sp, #SP_TLBTABLE]				// r0 = pointer to the external TLB table
	pop_dword r12, r1, r3
	mov		r0, r12, lsr #TLB_GRAN_BITS			// SEIBU SPI hack!
	ldr		r2, [r2, r0, lsl #2]
	add		r12, r2								// New physical IP = physical CS:0000 + new logical IP
	str		r2, [sp, #SP_PHYS_CS]				// Save new physical CS to stack
#else
	ldr		r2,[sp, #SP_PHYS_CS]				// get the physical CS:0000 into r2
	pop_dword r12, r0, r1
	add		r12, r2								// r12 = new IP + physical CS
#endif
	b		loop

// ------------------- C4 = LES r32,m32:16 -----------------------------
//
	.global	op_c4_USE32
op_c4_USE32:
	modrm_jump_32
// 0
	modrm_help_3_0 les eax
	modrm_help_3_0 les ecx
	modrm_help_3_0 les edx
	modrm_help_3_0 les ebx
	modrm_help_3_0 les esp
	modrm_help_3_0 les ebp
	modrm_help_3_0 les esi
	modrm_help_3_0 les edi
// 0x40
	modrm_help_3_40 les eax
	modrm_help_3_40 les ecx
	modrm_help_3_40 les edx
	modrm_help_3_40 les ebx
	modrm_help_3_40 les esp
	modrm_help_3_40 les ebp
	modrm_help_3_40 les esi
	modrm_help_3_40 les edi
// 0x80
	modrm_help_3_80 les eax
	modrm_help_3_80 les ecx
	modrm_help_3_80 les edx
	modrm_help_3_80 les ebx
	modrm_help_3_80 les esp
	modrm_help_3_80 les ebp
	modrm_help_3_80 les esi
	modrm_help_3_80 les edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

.macro les_reg32_t0 reg
	.global	les_r32_t0_bp_\reg
les_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unles_rs a segment override is in effect.
	//------
	mem_handler_bp
	.global	les_r32_t0_\reg
les_r32_t0_\reg:
	//-------
	// Indexing by the current effective segment.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	\reg, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		\reg, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		\reg, r3, lsl #16
	orr		\reg, r1, lsl #24
	// ----- Load segment
	ldrb	r0, [r2, #4]						// Load low byte of ES from [r2+4]
	ldrb	r2, [r2, #5]						// Load high byte of ES from [r2+5]
	orr		r2, r0, r2, lsl #8					// r2 = low byte | (high byte << 8) = new ES value
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		mov_es_r0r2_prot					// Yes we are, go handle protected mode version!
	//-------
	// We are in real mode, so use the simple handling.
	//-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2,[sp, #SP_ES_VALUE]
	str		r1,[sp, #SP_ES_BASE]
	b		restore_flags_from_r0
.endm

	les_reg32_t0 eax
	les_reg32_t0 ecx
	les_reg32_t0 edx
	les_reg32_t0 ebx
	les_reg32_t0 esp
	les_reg32_t0 ebp
	les_reg32_t0 esi
	les_reg32_t0 edi

.macro les_reg32_reg32 rl rr
	// Empty macro
.endm
	
	modrm_3_genall les les_r32_t0

// ------------------- C5 = LDS r32,m32:16 -----------------------------
//
	.global	op_c5_USE32
op_c5_USE32:
	modrm_jump_32
// 0
	modrm_help_3_0 lds eax
	modrm_help_3_0 lds ecx
	modrm_help_3_0 lds edx
	modrm_help_3_0 lds ebx
	modrm_help_3_0 lds esp
	modrm_help_3_0 lds ebp
	modrm_help_3_0 lds esi
	modrm_help_3_0 lds edi
// 0x40
	modrm_help_3_40 lds eax
	modrm_help_3_40 lds ecx
	modrm_help_3_40 lds edx
	modrm_help_3_40 lds ebx
	modrm_help_3_40 lds esp
	modrm_help_3_40 lds ebp
	modrm_help_3_40 lds esi
	modrm_help_3_40 lds edi
// 0x80
	modrm_help_3_80 lds eax
	modrm_help_3_80 lds ecx
	modrm_help_3_80 lds edx
	modrm_help_3_80 lds ebx
	modrm_help_3_80 lds esp
	modrm_help_3_80 lds ebp
	modrm_help_3_80 lds esi
	modrm_help_3_80 lds edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

.macro lds_reg32_t0 reg
	.global	lds_r32_t0_bp_\reg
lds_r32_t0_bp_\reg:
	//------
	// Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	lds_r32_t0_\reg
lds_r32_t0_\reg:
	//-------
	// Indexing by the current effective segment.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	\reg, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		\reg, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		\reg, r3, lsl #16
	orr		\reg, r1, lsl #24
	// ----- Load segment
	ldrb	r0, [r2, #4]						// Load low byte of DS from [r2+4]
	ldrb	r2, [r2, #5]						// Load high byte of DS from [r2+5]
	orr		r2, r0, r2, lsl #8					// r2 = low byte | (high byte << 8) = new DS value
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		mov_ds_r0r2_prot					// Yes we are, go handle protected mode version!
	//-------
	// We are in real mode, so use the simple handling.
	//-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2,[sp, #SP_DS_VALUE]
	str		r1,[sp, #SP_DS_BASE]
	b		restore_flags_from_r0
.endm

	lds_reg32_t0 eax
	lds_reg32_t0 ecx
	lds_reg32_t0 edx
	lds_reg32_t0 ebx
	lds_reg32_t0 esp
	lds_reg32_t0 ebp
	lds_reg32_t0 esi
	lds_reg32_t0 edi

.macro lds_reg32_reg32 rl rr
	// Empty macro
.endm
	
	modrm_3_genall lds lds_r32_t0

// ------------------- C6 = MOV r/m8, imm8 -----------------------------
	.global	op_c6_USE32
op_c6_USE32:
	modrm_jump_32
// 0
	modrm_help_1_0 mov imm8
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
#if USE_SEIBUSPI
	.word	mov_eaxd8_imm8
	.word	mov_ecxd8_imm8
	.word	mov_edxd8_imm8
	.word	mov_ebxd8_imm8_seibuhack			// Special hack for "mov [ebx+disp8],imm8"
	.word	mov_SIBd8_imm8
	.word	mov_ebpd8_imm8
	.word	mov_esid8_imm8
	.word	mov_edid8_imm8
#else
	modrm_help_1_40 mov imm8
#endif	
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 mov imm8
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0
	.word op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_1_help mov mov_r0 imm8

	.ltorg

#if USE_SEIBUSPI
	//------
	// "Raiden Fighters Jet" has a bug where it attempts to write to ROM area.
	// 0023B1D3	C6437600	mov	[ebx+76],00		(ebx=0023AF04)
	// This hack checks for this situation and ignores the write to ROM.
	//------
mov_ebxd8_imm8_seibuhack:
	r0_from_idx_disp8 ebx						// r0 = target logical address
	mrs		r1,cpsr								// Save CPU flags to r1
	tst		r0, #0x00200000						// Are we trying to access ROM?
	bne		1f									// Yep, so skip the actual handling!
	msr		cpsr_f,r1							// Restore flags from r1
	b		mov_r0_imm8							// Go do the write
1:	msr		cpsr_f,r1							// Restore flags from r1
	add		r12, #1								// Skip over the imm8 byte
	b		loop								// Back to loop ignoring the write.
#endif
	
// ------------------- C7 = MOV r/m32, imm32 -----------------------------
// 662667C7430810000000 = mov dword es:[ebx+08],00000010
// 662667C704F73A090800 = mov dword es:[edi+esi*8], 0008093A
// 662667C744F704008E0000 = mov dword es:[edi+esi*8+04], 00008E00
//
op_c7_USE32:
	modrm_jump_32
// 0
	modrm_help_1_0 mov imm32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 mov imm32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 mov imm32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0
	.word op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	.global	mov_t0_bp_imm32
mov_t0_bp_imm32:
	//------
	// Called from cpu_66: Indexing by BP register, so use SS unless a segment override is in effect.
	//------
	mem_handler_bp
	.global	mov_t0_imm32
mov_t0_imm32:
	//-------
	// Indexing by the current effective segment.
	//-------
	mem_handler_jump_r0r3 1f op_c7_EGA_USE32_r2 op_c7_MODEX_USE32
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	r0, [r12], #1
	ldrb	r1, [r12], #1
	strb	r0, [r2]
	ldrb	r0, [r12], #1
	strb	r1, [r2, #1]
	ldrb	r1, [r12], #1
	strb	r0, [r2, #2]
	strb	r1, [r2, #3]
	b		loop

	modrm_1_help mov mov_t0 imm32

	.ltorg
	
// ------------------- C8 = ENTER imm16,imm8 ---------------------------
// If imm8 = 00, this is a combination of PUSH EBP, MOV EBP,ESP, SUB ESP, imm16
	.global	op_c8_USE32
op_c8_USE32:
	//------
	// PUSH EBP
	//------
	push_dword ebp r0 r1
	//------
	// MOV EBP, ESP
	//------
	mov		ebp, esp
	//------
	// SUB ESP, imm16
	//------
	r0_from_disp16
	sub		esp, r0
	//------
	// Get the additional imm8 value. We only support imm8 = 0 values!
	//------
	ldrb	r0, [r12], #1
	b		loop
	
// ------------------- C9 = LEAVE --------------------------------------
// This is a combination of MOV ESP,EBP and POP EBP
op_c9_USE32:
	//------
	// MOV ESP, EBP
	//------
	mov		esp, ebp
	//------
	// POP EBP
	//------
	pop_dword ebp r0 r1
	b		loop

// ------------------- CA = RETF imm16 ---------------------------------
//
	.global	op_ca_USE32
op_ca_USE32:
	ldrb	r3,[r12],#1				// Load byte to r3, increment r12 by 1
	ldrb	r0,[r12],#1				// Load byte to r0, increment r12 by 1
	orr		r3, r0, lsl #8			// r3 = low byte | (high byte << 8)
	orr		r3, #0x10000			// Tell this is a USE32 RETF
	b		retf_common_r3			// Continue with the common handler

// ------------------- CB = RETF ---------------------------------------
//
	.global	op_cb_USE32
op_cb_USE32:
	mov		r3, #0x10000			// Tell this is a USE32 RETF
	b		retf_common_r3			// Continue with the common handler

// ------------------- CF = IRET ---------------------------------------
//
	.global	op_cf_USE32
op_cf_USE32:
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]	// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				// Save current flags to r0
	tst		r3, #1					// Are we in protected mode (or in VM mode)?
	orr		r0, #0x10000			// Mark that this is a 32-bit IRET
	bne		cpu_iret_prot_r0		// Yes we are, go handle protected mode INT!
	b		.unknown				// Else handle 32-bit real mode IRET (rare!)

// ------------------- D0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,1 ---
// - RCL/RCR/ROL/ROR change the carry and overflow flags only, must not change the Sign and Zero flags.
// - SHL/SHR/SHL/SAR change carry, sign, overflow and zero flags
op_d0_USE32:
	modrm_jump_32
	modrm_tbl_0_oper rol_b ror_b rcl_b rcr_b shl_b shr_b shl_b sar_b "1"
//0xc0 = mod = 11b => two register operands
// ROL
	.word rol_reg8l_1_r4, rol_reg8l_1_r5, rol_reg8l_1_r6, rol_reg8l_1_r7, rol_reg8h_1_r4, rol_reg8h_1_r5, rol_reg8h_1_r6, rol_reg8h_1_r7
// ROR	
	.word ror_reg8l_1_r4, ror_reg8l_1_r5, ror_reg8l_1_r6, ror_reg8l_1_r7, ror_reg8h_1_r4, ror_reg8h_1_r5, ror_reg8h_1_r6, ror_reg8h_1_r7
// RCL
	.word rcl_reg8l_1_r4, rcl_reg8l_1_r5, rcl_reg8l_1_r6, rcl_reg8l_1_r7, rcl_reg8h_1_r4, rcl_reg8h_1_r5, rcl_reg8h_1_r6, rcl_reg8h_1_r7
// RCR
	.word rcr_reg8l_1_r4, rcr_reg8l_1_r5, rcr_reg8l_1_r6, rcr_reg8l_1_r7, rcr_reg8h_1_r4, rcr_reg8h_1_r5, rcr_reg8h_1_r6, rcr_reg8h_1_r7
// SHL
	.word shl_reg8l_1_r4, shl_reg8l_1_r5, shl_reg8l_1_r6, shl_reg8l_1_r7, shl_reg8h_1_r4, shl_reg8h_1_r5, shl_reg8h_1_r6, shl_reg8h_1_r7
// SHR
	.word shr_reg8l_1_r4, shr_reg8l_1_r5, shr_reg8l_1_r6, shr_reg8l_1_r7, shr_reg8h_1_r4, shr_reg8h_1_r5, shr_reg8h_1_r6, shr_reg8h_1_r7
// SHL	
	.word shl_reg8l_1_r4, shl_reg8l_1_r5, shl_reg8l_1_r6, shl_reg8l_1_r7, shl_reg8h_1_r4, shl_reg8h_1_r5, shl_reg8h_1_r6, shl_reg8h_1_r7
// SAR	
	.word sar_reg8l_1_r4, sar_reg8l_1_r5, sar_reg8l_1_r6, sar_reg8l_1_r7, sar_reg8h_1_r4, sar_reg8h_1_r5, sar_reg8h_1_r6, sar_reg8h_1_r7

	modrm_genall_0_oper rol_b ror_b rcl_b rcr_b shl_b shr_b "skip" sar_b yte_r0 "1"

// ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,1 ---
// - RCL/RCR/ROL/ROR change only the carry and overflow flags
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_d1_USE32:
	modrm_jump_32
	modrm_tbl_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw "1"
//0xc0 = mod = 11b => two register operands
	.word rol_reg32_1_r4, rol_reg32_1_r5, rol_reg32_1_r6, rol_reg32_1_r7, rol_reg32_1_r8, rol_reg32_1_r9, rol_reg32_1_r10, rol_reg32_1_r11
	.word ror_reg32_1_r4, ror_reg32_1_r5, ror_reg32_1_r6, ror_reg32_1_r7, ror_reg32_1_r8, ror_reg32_1_r9, ror_reg32_1_r10, ror_reg32_1_r11
	.word rcl_reg32_1_r4, rcl_reg32_1_r5, rcl_reg32_1_r6, rcl_reg32_1_r7, rcl_reg32_1_r8, rcl_reg32_1_r9, rcl_reg32_1_r10, rcl_reg32_1_r11
	.word rcr_reg32_1_r4, rcr_reg32_1_r5, rcr_reg32_1_r6, rcr_reg32_1_r7, rcr_reg32_1_r8, rcr_reg32_1_r9, rcr_reg32_1_r10, rcr_reg32_1_r11
// SHL reg32,1 == add reg32, reg32
	modrm_reg_reg_word add eax eax
	modrm_reg_reg_word add ecx ecx
	modrm_reg_reg_word add edx edx
	modrm_reg_reg_word add ebx ebx
	modrm_reg_reg_word add esp esp
	modrm_reg_reg_word add ebp ebp
	modrm_reg_reg_word add esi esi
	modrm_reg_reg_word add edi edi
	.word shr_reg32_1_r4, shr_reg32_1_r5, shr_reg32_1_r6, shr_reg32_1_r7, shr_reg32_1_r8, shr_reg32_1_r9, shr_reg32_1_r10, shr_reg32_1_r11
// SAL (never used)	
	modrm_reg_reg_word add eax eax
	modrm_reg_reg_word add ecx ecx
	modrm_reg_reg_word add edx edx
	modrm_reg_reg_word add ebx ebx
	modrm_reg_reg_word add esp esp
	modrm_reg_reg_word add ebp ebp
	modrm_reg_reg_word add esi esi
	modrm_reg_reg_word add edi edi
	.word sar_reg32_1_r4, sar_reg32_1_r5, sar_reg32_1_r6, sar_reg32_1_r7, sar_reg32_1_r8, sar_reg32_1_r9, sar_reg32_1_r10, sar_reg32_1_r11

	modrm_genall_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw "skip" sar_dw _t0 "1"

.macro opd1common oper
	.global	\oper\()_dw_t0_bp_1
\oper\()_dw_t0_bp_1:
	mem_handler_bp
	.global	\oper\()_dw_t0_1
\oper\()_dw_t0_1:
	mem_handler_jump_r0r3 \oper\()_dw_r2_1_RAM bad_EGA_opcode bad_EGA_opcode
.endm

	opd1common rol
	opd1common ror
	opd1common rcl
	opd1common rcr
	opd1common shl
	opd1common shr
	opd1common sar

// ------------------- D2 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,CL ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_d2_USE32:
	modrm_jump_32
	modrm_tbl_0_oper rol ror rcl rcr shl shr shl sar 8_CL
	.word rol_al_CL, rol_cl_CL, rol_dl_CL, rol_bl_CL, rol_ah_CL, rol_ch_CL, rol_dh_CL, rol_bh_CL
	.word ror_al_CL, ror_cl_CL, ror_dl_CL, ror_bl_CL, ror_ah_CL, ror_ch_CL, ror_dh_CL, ror_bh_CL
	.word rcl_al_CL, rcl_cl_CL, rcl_dl_CL, rcl_bl_CL, rcl_ah_CL, rcl_ch_CL, rcl_dh_CL, rcl_bh_CL
	.word rcr_al_CL, rcr_cl_CL, rcr_dl_CL, rcr_bl_CL, rcr_ah_CL, rcr_ch_CL, rcr_dh_CL, rcr_bh_CL
	.word shl_al_CL, shl_cl_CL, shl_dl_CL, shl_bl_CL, shl_ah_CL, shl_ch_CL, shl_dh_CL, shl_bh_CL
	.word shr_al_CL, shr_cl_CL, shr_dl_CL, shr_bl_CL, shr_ah_CL, shr_ch_CL, shr_dh_CL, shr_bh_CL
	.word shl_al_CL, shl_cl_CL, shl_dl_CL, shl_bl_CL, shl_ah_CL, shl_ch_CL, shl_dh_CL, shl_bh_CL
	.word sar_al_CL, sar_cl_CL, sar_dl_CL, sar_bl_CL, sar_ah_CL, sar_ch_CL, sar_dh_CL, sar_bh_CL

	modrm_genall_0_oper rol ror rcl rcr shl shr "skip" sar _t0 8_CL

// ------------------- d3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,CL --
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_d3_USE32:
	modrm_jump_32
	modrm_tbl_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw CL
	//0xc0 = mod = 11b => two register operands
	.word rol_eax_CL, rol_ecx_CL, rol_edx_CL, rol_ebx_CL, rol_esp_CL, rol_ebp_CL, rol_esi_CL, rol_edi_CL
	.word ror_eax_CL, ror_ecx_CL, ror_edx_CL, ror_ebx_CL, ror_esp_CL, ror_ebp_CL, ror_esi_CL, ror_edi_CL
	.word rcl_eax_CL, rcl_ecx_CL, rcl_edx_CL, rcl_ebx_CL, rcl_esp_CL, rcl_ebp_CL, rcl_esi_CL, rcl_edi_CL
	.word rcr_eax_CL, rcr_ecx_CL, rcr_edx_CL, rcr_ebx_CL, rcr_esp_CL, rcr_ebp_CL, rcr_esi_CL, rcr_edi_CL
	.word shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	.word shr_eax_CL, shr_ecx_CL, shr_edx_CL, shr_ebx_CL, shr_esp_CL, shr_ebp_CL, shr_esi_CL, shr_edi_CL
	.word shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	.word sar_eax_CL, sar_ecx_CL, sar_edx_CL, sar_ebx_CL, sar_esp_CL, sar_ebp_CL, sar_esi_CL, sar_edi_CL

	modrm_genall_1_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw "skip" sar_dw _t0 CL

.macro opd3common oper
	.global	\oper\()_dw_t0_bp_CL
\oper\()_dw_t0_bp_CL:
	mem_handler_bp
	.global	\oper\()_dw_t0_CL
\oper\()_dw_t0_CL:
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	and		r1, ecx, #31
	b		\oper\()_d3_cont					// Continue with similar code to opcode c1 handler.
	
	.global	\oper\()_eax_CL
\oper\()_eax_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r4					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_ecx_CL
\oper\()_ecx_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r5					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_edx_CL
\oper\()_edx_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r6					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_ebx_CL
\oper\()_ebx_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r7					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_esp_CL
\oper\()_esp_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r8					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_ebp_CL
\oper\()_ebp_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r9					// Continue with similar code to opcode c1 handler.
	.global	\oper\()_esi_CL
\oper\()_esi_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r10				// Continue with similar code to opcode c1 handler.
	.global	\oper\()_edi_CL
\oper\()_edi_CL:
	and		r1, ecx, #31
	b		\oper\()_d3_cont_r11				// Continue with similar code to opcode c1 handler.
.endm

	opd3common rol
	opd3common ror
	opd3common rcl
	opd3common rcr
	opd3common shl
	opd3common shr
	opd3common sar


// ------------------- D7 = XLAT ---------------------------------------
// AL = [EBX + unsigned AL]
	.global	op_d7_USE32
op_d7_USE32:
	mvn		r3, #0				// Use 32-bit memory address masking
	and		r0, eax, #0xFF						// r0 = unsigned AL
	add		r0, ebx								// r0 = EBX + (unsigned)AL
	mem_handler_jump_r0r3 op_d7_RAM .unknown .unknown

// ------------------- D8 .. DF = FPU ESC opcodes ---------------------
// Math coprocessor opcode, unsupported!
//
#if 0

op_db_USE32:
//	lbu		t1, 0(cseip)						// Get the modrm byte
//	subu	t1, 0xE3							// Is it 0xE3 == finit?
//	beqz	t1, op_dd_USE32						// Yep, skip the warning message
op_d8_USE32:
op_da_USE32:
op_dc_USE32:
op_de_USE32:
op_df_USE32:
	//------
	// Show a warning message, if this is the first time we encounter FPU opcodes in this game.
	//------
//	sw		sp, NeedFPUWarn
op_d9_USE32:
op_dd_USE32:
	modrm_jump_32
	modrm_tbl_0_oper fpu fpu fpu fpu fpu fpu fpu fpu
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	.word loop, loop, loop, loop, loop, loop, loop, loop
	
	modrm_0_help fpu fpu_r0

	.ltorg
	
#endif
	
// ------------------- E0 = LOOPNE/LOOPNZ ------------------------------
// Take the jump if ECX != 0 and lf_res != 0
//
	.global	op_e0_USE32
op_e0_USE32:
	mrs		r0, cpsr				// Save current flags to r0
	ldrsb	r1,[r12],#1				// Load sign-extended byte to r1, increment r12 by 1
	sub		ecx, #1					// Decrement ECX
	tst		r0, #0x40000000			// Is the Zero flag set?
	bne		restore_flags_from_r0	// If Zero flag is set, do not take the jump.
	cmp		ecx, #0					// Is ECX zero?
	addne	r12, r12, r1			// Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

// ------------------- E1 = LOOPE/LOOPZ --------------------------------
// Take the jump if ECX != 0 and lf_res == 0
//
	.global	op_e1_USE32
op_e1_USE32:
	mrs		r0, cpsr				// Save current flags to r0
	ldrsb	r1,[r12],#1				// Load sign-extended byte to r1, increment r12 by 1
	sub		ecx, #1					// Decrement ECX
	tst		r0, #0x40000000			// Is the Zero flag set?
	beq		restore_flags_from_r0	// If Zero flag is not set, do not take the jump.
	cmp		ecx, #0					// Is ECX zero?
	addne	r12, r12, r1			// Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

// ------------------- E2 = LOOP ---------------------------------------
	.global	op_e2_USE32
op_e2_USE32:
	mrs		r0, cpsr				// Save current flags to r0
	ldrsb	r1,[r12],#1				// Load sign-extended byte to r1, increment r12 by 1
	subs	ecx, #1					// Decrement ECX
	addne	r12, r12, r1			// Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

// ------------------- E3 = JCXZ ---------------------------------------
	.global	op_e3_USE32
op_e3_USE32:
	mrs		r0, cpsr				// Save current flags to r0
	ldrsb	r1,[r12],#1				// Load sign-extended byte to r1, increment r12 by 1
	cmp		ecx, #0					// Is ECX zero?
	addeq	r12, r12, r1			// Adjust program counter by the jump amount, if CX == 0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags

// ------------------- E8 = CALL near ----------------------------------
// Note! This code should be re-executable, as paging may cause a page fault,
// after which this opcode gets re-run from the start. Adjust stack pointer
// only after all page init code has been run!
//
op_e8_rel32:
	//------
	// Get the new IP from imm32 value, and calculate new physical CS:EIP
	//------
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	ldrb	r2,[r12],#1
	ldrb	r3,[r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	ldr		r2,[sp, #SP_PHYS_CS]				// Get current physical CS from stack
	orr		r0, r3, lsl #24						// r0 = imm32 value
	sub		r1, r12, r2							// r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_dword r1 r3 lr
	add		r1, r0								// r1 = current IP + jump amount
	add		r12, r2, r1							// Adjust program counter by the jump amount
	b		loop

// ------------------- E9 = JMP near -----------------------------------
//
op_e9_rel32:
	//------
	// Get the new IP from imm32 value, and calculate new physical CS:EIP
	//------
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	ldrb	r2,[r12],#1
	ldrb	r3,[r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	ldr		r2,[sp, #SP_PHYS_CS]				// Get current physical CS from stack
	orr		r0, r3, lsl #24
	sub		r1, r12, r2							// r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	add		r1, r0								// r1 = current IP + jump amount
	add		r12, r2, r1							// Adjust program counter by the jump amount
	b		loop


// ------------------- EA = JMP FAR ------------------------------------
//
	.global	op_ea_USE32
op_ea_USE32:
	//-------
	// First get the jump target :
	//	r1 = new IP (offset)
	//	r2 = new CS (segment)
	//-------
	ldrb	r0,[r12]
	ldrb	r1,[r12, #1]
	ldrb	r2,[r12, #2]
	ldrb	r3,[r12, #3]
	orr		r1, r0, r1, lsl #8
	orr		r1, r2, lsl #16
	ldrb	r2,[r12, #4]
	ldrb	r0,[r12, #5]
	orr		r1, r3, lsl #24						// r1 = new logical IP
	orr		r2, r0, lsl #8						// r2 = new CS value
	b		cpu_jmp_far_r1r2

// ------------------- F6 = ??? r/m8 -----------------------------------
op_f6_USE32:
	modrm_jump_32
	modrm_tbl_0_oper test back1 not neg mul imul div idiv b
//0xc0 = mod = 11b => register operand
// TEST	imm8
	.word test_al_imm8, test_cl_imm8, test_dl_imm8, test_bl_imm8, test_ah_imm8, test_ch_imm8, test_dh_imm8, test_bh_imm8
// (invalid opcode)	
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// NOT	
	.word not_al, not_cl, not_dl, not_bl, not_ah, not_ch, not_dh, not_bh
// NEG	
	.word neg_al, neg_cl, neg_dl, neg_bl, neg_ah, neg_ch, neg_dh, neg_bh
// MUL
	.word mul_al, mul_cl, mul_dl, mul_bl, mul_ah, mul_ch, mul_dh, mul_bh
// IMUL
	.word imul_al, imul_cl, imul_dl, imul_bl, imul_ah, imul_ch, imul_dh, imul_bh
// DIV r8
	.word div_al, div_cl, div_dl, div_bl, div_ah, div_ch, div_dh, div_bh
// IDIV r8
	.word idiv_al, idiv_cl, idiv_dl, idiv_bl, idiv_ah, idiv_ch, idiv_dh, idiv_bh

	modrm_genall_0_oper test "skip" not neg mul imul div idiv _r0 b

// ------------------- F7 = ??? r/m32 ----------------------------------
op_f7_USE32:
	modrm_jump_32
	modrm_tbl_1_oper test back1 not neg mul imul div idiv dword
//0xc0 = mod = 11b => register operand
// TEST r32,imm32
	modrm_help_3_C0 test_imm32 ""
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// NOT r32
	modrm_help_3_C0 not ""
// NEG r32
	modrm_help_3_C0 neg ""
// MUL r32
	modrm_help_3_C0 mul ""
// IMUL r32
	modrm_help_3_C0 imul ""
// DIV r32
	modrm_help_3_C0 div ""
// IDIV r32
	modrm_help_3_C0 idiv ""

	modrm_genall_1_oper test skip not neg mul imul div idiv _t0 dword
	
// ============= TEST ==============

	.global	test_t0_bp_dword
test_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	test_t0_dword
test_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
#if USE_SEIBUSPI
	//-------
	// SEIBU SPI special handling for speedup: If we are waiting for the next frame, exit the core emulation.
	//-------
	str		r0, [sp, #SP_FREE1]					// Save the logical memory address
#endif
	mem_handler_jump_r0r3 1f test_EGA_r2_imm32 bad_EGA_opcode
	// ----- Get the word from RAM into r0 -----
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	// ----- Get the imm32 value into r1 -----
	ldrb	r1, [r12], #1
	ldrb	r2, [r12], #1
	ldrb	r3, [r12], #1
	orr		r1, r2, lsl #8
	ldrb	r2, [r12], #1
	orr		r1, r3, lsl #16
	orr		r1, r2, lsl #24
	// ----- Finally test the values -----
#if USE_SEIBUSPI
	//-------
	// SEIBU SPI special handling for speedup: 
	// - rdft: If the memory address is 000298d0, exit the core emulation.
	// - rdft2: If the memory address is 000282ac, exit the core emulation.
	// - rfjet: If the memory address is 0002894c, exit the core emulation.
	//-------
	ldr		r2, [sp, #SP_FREE1]					// Get the logical memory address
	ldr		r3, =0x000298d0						// Raiden Fighters 1 speedup address
	cmp		r2, r3
	beq		1f
	ldr		r3, =0x000282ac						// Raiden Fighters 2 speedup address
	cmp		r2, r3
	beq		1f
	ldr		r3, =0x0002894c						// Raiden Fighters Jet speedup address
	cmp		r2, r3
	beq		1f
#endif
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	tst		r0, r1
	b		loop
#if USE_SEIBUSPI
1:	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	tst		r0, r1
	b		debug_trap_false					// Go exit the emulation!
#endif

.macro test_reg32_imm32 reg
	.global	test_imm32_\reg
test_imm32_\reg:
	ldrb	r0, [r12], #1
	ldrb	r1, [r12], #1
	ldrb	r2, [r12], #1
	ldrb	r3, [r12], #1
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	tst		\reg, r0
	b		loop						// Back to loop
.endm

	test_reg32_imm32 eax
	test_reg32_imm32 ecx
	test_reg32_imm32 edx
	test_reg32_imm32 ebx
	test_reg32_imm32 esp
	test_reg32_imm32 ebp
	test_reg32_imm32 esi
	test_reg32_imm32 edi

// ============= NOT ==============

	.global	not_t0_bp_dword
not_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	not_t0_dword
not_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	mvn		r0, r0
	mvn		r1, r1
	strb	r0, [r2]
	strb	r1, [r2, #1]
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	mvn		r0, r0
	mvn		r1, r1
	strb	r0, [r2, #2]
	strb	r1, [r2, #3]
	b		loop

.macro not_reg32 reg
	.global	not_\reg
not_\reg:
	mvn		\reg, \reg
	b		loop
.endm

	not_reg32 eax
	not_reg32 ecx
	not_reg32 edx
	not_reg32 ebx
	not_reg32 esp
	not_reg32 ebp
	not_reg32 esi
	not_reg32 edi

// ============= NEG ==============

	.global	neg_t0_bp_dword
neg_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	neg_t0_dword
neg_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r1, lsl #24
	rsbs	r0, #0
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	lsr		r0, #8
	strb	r0, [r2, #2]
	lsr		r0, #8
	strb	r0, [r2, #3]
	b		complement_carry

.macro neg_reg32 reg
	.global	neg_\reg
neg_\reg:
	rsbs	\reg, #0
	b		complement_carry
.endm

	neg_reg32 eax
	neg_reg32 ecx
	neg_reg32 edx
	neg_reg32 ebx
	neg_reg32 esp
	neg_reg32 ebp
	neg_reg32 esi
	neg_reg32 edi

// ============= MUL ==============

	.global	mul_t0_bp_dword
mul_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	mul_t0_dword
mul_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
.mul_32_r0:
	msr		cpsr_f,#0				// Clear all flags
#if 1
	mov		r1, eax
	umull	eax, edx, r0, r1
	tst		edx, #0
#else	
	//------
	// 32 X 32 bit multiply.
	// Source operands in r0, eax
	// result in eax, edx
	// r1 and r2 are working registers
	// Algorithm from "http://www.peter-cockerell.net/aalp/html/ch-6.html"
	//------
	mov		r1, r0, lsr #16  					// Get top 16 bits of r0
	mov		edx, eax, lsr #16					// Get top 16 bits of eax
	bic  	r0, r0, r1, lsl #16					// Clear top 16 bits of r0
	bic		eax, eax, edx, lsl #16				// Clear top 16 bits of eax
	mul  	r2, r0, eax   						// Bits 0-15 and 16-31
	mul  	eax, r1, eax   					// Bits 16-47, part 1
	mul		r0, edx, r0							// Bits 16-47, part 2
	mul		edx, r1, edx						// Bits 32-63
	adds	r0, eax, r0   						// Add the two bits 16-47
	addcs	edx, edx, #0x10000					// Add in carry from above
	adds	eax, r2, r0, lsl #16				// Final bottom 32 bits
	adcs	edx, edx, r0, lsr #16				// Final top 32 bits
#endif	
	msrne	cpsr_f, #0x30000000					// Set Carry and Overflow flags if EDX != 0
	b		loop

.macro mul_reg32 reg
	.global	mul_\reg
mul_\reg:
	mov		r0, \reg					// t0 = signed register value
	b		.mul_32_r0
.endm

	mul_reg32 eax
	mul_reg32 ecx
	mul_reg32 edx
	mul_reg32 ebx
	mul_reg32 esp
	mul_reg32 ebp
	mul_reg32 esi
	mul_reg32 edi

// ============= IMUL ==============

	.global	imul_t0_bp_dword
imul_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	imul_t0_dword
imul_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
.imul_32_r0:
#if 1
	mov		r1, eax
	smull	eax, edx, r0, r1
	mov		r0, eax, asr #31
	teq		r0, edx
	msrne	cpsr_f, #0x30000000					// Set Carry and Overflow flags if EDX != 0
	b		loop
#else	
	//------
	// First make both the operands positive
	//------
	mov		r3, #0
	cmp		eax, #0
	rsbmi	eax, #0
	eormi	r3, #1
	cmp		r0, #0
	rsbmi	r0, #0
	eormi	r3, #1
	//------
	// 32 X 32 bit multiply.
	// Source operands in r0, eax
	// result in eax, edx
	// r1 and r2 are working registers
	// Algorithm from "http://www.peter-cockerell.net/aalp/html/ch-6.html"
	//------
	mov		r1, r0, lsr #16  					// Get top 16 bits of r0
	mov		edx, eax, lsr #16					// Get top 16 bits of eax
	bic  	r0, r0, r1, lsl #16					// Clear top 16 bits of r0
	bic		eax, eax, edx, lsl #16				// Clear top 16 bits of eax
	mul  	r2, r0, eax   						// Bits 0-15 and 16-31
	mul  	eax, r1, eax   					// Bits 16-47, part 1
	mul		r0, edx, r0							// Bits 16-47, part 2
	mul		edx, r1, edx						// Bits 32-63
	adds	r0, eax, r0   						// Add the two bits 16-47
	addcs	edx, edx, #0x10000					// Add in carry from above
	adds	eax, r2, r0, lsl #16				// Final bottom 32 bits
	adcs	edx, edx, r0, lsr #16				// Final top 32 bits
	movne	r0, #0x30000000						// Set Carry and Overflow flags if EDX != 0
	moveq	r0, #0
	tst		r3, #1								// Should the result be negative?
	beq		restore_flags_from_r0				// Nope, all OK!
	//------
	// Negate the result if only one of the operands was negative.
	//------
	rsb		edx, #0
	rsbs	eax, #0
	sbc		edx, #0
	b		restore_flags_from_r0
#endif

.macro imul_reg32 reg
	.global	imul_\reg
imul_\reg:
	mov		r0, \reg					// t0 = signed register value
	b		.imul_32_r0
.endm

	imul_reg32 eax
	imul_reg32 ecx
	imul_reg32 edx
	imul_reg32 ebx
	imul_reg32 esp
	imul_reg32 ebp
	imul_reg32 esi
	imul_reg32 edi

// ============= DIV ==============

	.global	div_t0_bp_dword
div_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	div_t0_dword
div_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r2, r0, r2, lsl #24
	.global	div_dw_by_r2
div_dw_by_r2:
#if SOFTWARE_DIV
	//-------
	// Software division routine. Algorithm taken from
	// "http://www.peter-cockerell.net/aalp/html/ch-6.html"
	// 	r0 = div (result)
	// 	r1 = mod (result)
	// 	r2 = rhs
	// 	r3 = count
	//	eax = lhs (low word)
	//	edx = lhs (high word)
	//-------
	teq  	r2, #0    				// trap div by zero
	beq		div32_by_zero			// division by zero!!
	push	{eax, edx}				// save eax and edx in case of an overflow
	cmp		edx, #0
	mov  	r1, #0    				// init remainder
	mov  	r0, #0    				// and result
	mov  	r3, #64    				// set up count
	bmi		2f						// if highest bit of edx set, skip the first loop
1:	subs 	r3, r3, #1  			// get first 1 bit of lhs
	beq		3f    					// into bit 31. return if 0
	movs 	eax, eax, asl #1
	adcs	edx, edx, edx
	bpl  	1b
2:	movs 	eax, eax, asl #1  		// get next bit into...
	adcs	edx, edx, edx
	adcs  	r1, r1, r1   			// r1 for trial subtract, carry set if r1 > 0xffffffff
	cmpcc  	r1, r2    				// can we subtract? we always can if the above shift overflows
	subcs 	r1, r1, r2   			// yes, so do
	adcs  	r0, r0, r0   			// shift carry into result, set carry if overflow
	bcs		4f						// break out if the result overflows
	subs 	r3, r3, #1  			// next loop
	bne  	2b
3:	add		sp, #(2*4)				// skip popping eax and edx
	mov		eax, r0
	mov		edx, r1
	b		loop					// go back to opcode loop
4:	pop		{eax, edx}				// restore original eax and edx
	b		div32_by_zero
#else
	mov		r0, #0x280
	orr		r0, #0x04000000			// #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		div32_by_zero			// Division by zero!!
	mov		r1, #2					// Use 64bit/64bit version, so that we get unsigned results.
	strh	r1, [r0]				// REG_DIVCNT = DIV_64_64 = 2
	str		eax, [r0, #0x10]		// #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		edx, [r0, #0x14]		// #define REG_DIV_NUMER_H		(*(vs32*) (0x04000294)),	REG_DIV_NUMER_H = 0
	mov		r1, #0
	str		r2, [r0, #0x18]			// #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
	str		r1, [r0, #0x1C]			// #define REG_DIV_DENOM_H		(*(vs32*) (0x0400029C)),	REG_DIV_DENOM_H = 0
1:	ldrh	r1, [r0]				// while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		1b
	ldr		r2, [r0, #0x24]			// #define REG_DIV_RESULT_H		(*(vs32*) (0x040002A4))
	ldr		r1, [r0, #0x20]			// #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0))
	cmp		r2, #0					// Does the result fit into 32 bits?
	bne		div32_by_zero			// Division by zero!!
	mov		eax, r1					// eax is the result of the division
	ldr		edx,[r0, #0x28]			// #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	edx is the remainder of the division
	b		loop					// Go back to loop
#endif

.macro div_reg32 reg
	.global	div_\reg
div_\reg:
	mov		r2, \reg
	b		div_dw_by_r2
.endm

	div_reg32 eax
	div_reg32 ecx
	div_reg32 edx
	div_reg32 ebx
	div_reg32 esp
	div_reg32 ebp
	div_reg32 esi
	div_reg32 edi

// ============= IDIV ==============

	.global	idiv_t0_bp_dword
idiv_t0_bp_dword:						// Called from cpu_66.S
	mem_handler_bp	
	.global	idiv_t0_dword
idiv_t0_dword:
	//-------
	// Calculate the memory address.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_EGA_opcode
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r2, r0, r2, lsl #24
	.global	idiv_dw_by_r2
idiv_dw_by_r2:
#if SOFTWARE_DIV
	//-------
	// Software division routine. Algorithm taken from
	// "http://www.peter-cockerell.net/aalp/html/ch-6.html"
	// 	r0 = div (result)
	// 	r1 = mod (result)
	// 	r2 = rhs
	// 	r3 = sign bit save
	// 	r5 = count
	//	eax = lhs (low word)
	//	edx = lhs (high word)
	//-------
	teq  	r2, #0    				// trap div by zero
	beq		div32_by_zero			// division by zero!!
	push	{eax, r5, edx}			// save eax and edx in case of an overflow
	teq		edx, #0
	eor		r3, edx, r2
	lsr		r3, #31					// r3 lowest bit tells the sign of the result
	bpl		1f						// skip edx:eax negation if it was positive
	orr		r3, #2					// mod sign should be negative if lhs is negative
	rsb		edx, #0
	rsbs	eax, #0					// negate edx:eax
	sbc		edx, #0
1:	teq		r2, #0					// is rhs negative?
	rsbmi	r2, #0					// if yes, make it positive
	mov  	r1, #0    				// init remainder
	mov  	r0, #0    				// and result
	mov  	r5, #64    				// set up count
1:	subs 	r5, r5, #1  			// get first 1 bit of lhs
	beq		3f    					// into bit 31. return if 0
	movs 	eax, eax, asl #1
	adcs	edx, edx, edx
	bpl  	1b
2:	movs 	eax, eax, asl #1  		// get next bit into...
	adcs	edx, edx, edx
	adc  	r1, r1, r1   			// r1 for trial subtract
	cmp  	r1, r2    				// can we subtract?
	subcs 	r1, r1, r2   			// yes, so do
	adcs  	r0, r0, r0   			// shift carry into result, set carry if overflow
	bcs		4f						// break out if the result overflows
	subs 	r5, r5, #1  			// next loop
	bne  	2b
3:	pop		{eax, r5, edx}
	and		r2, r3, #1
	add		r2, #0x80000000
	cmp		r0, r2					// did the result overflow?
	bhs		div32_by_zero
	tst		r3, #1					// should we negate the result?
	rsbne	r0, #0					// yep
	tst		r3, #2					// should we negate the remainder?
	rsbne	r1, #0					// yep
	mov		eax, r0
	mov		edx, r1
	b		loop					// go back to opcode loop
4:	pop		{eax, r5, edx}			// restore original eax and edx
	b		div32_by_zero
#else
	mov		r0, #0x280
	orr		r0, #0x04000000			// #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		div32_by_zero			// Division by zero!!
	mov		r1, #1
	strh	r1, [r0]				// REG_DIVCNT = DIV_64_32 = 1
	str		eax, [r0, #0x10]		// #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		edx, [r0, #0x14]		// #define REG_DIV_NUMER_H		(*(vs32*) (0x04000294)),	REG_DIV_NUMER_H = 0
	str		r2, [r0, #0x18]			// #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1:	ldrh	r1, [r0]				// while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		1b
	ldr		r2, [r0, #0x24]			// #define REG_DIV_RESULT_H		(*(vs32*) (0x040002A4))
	ldr		r1, [r0, #0x20]			// #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0))
	cmp		r2, r1, asr #31			// Does the result fit into 32 bits?
	bne		div32_by_zero			// Division by zero!!
	mov		eax, r1					// eax is the result of the division
	ldr		edx,[r0, #0x28]			// #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	edx is the remainder of the division
	b		loop					// Go back to loop
#endif
	
.macro idiv_reg32 reg
	.global	idiv_\reg
idiv_\reg:
	mov		r2, \reg
	b		idiv_dw_by_r2
.endm

	idiv_reg32 eax
	idiv_reg32 ecx
	idiv_reg32 edx
	idiv_reg32 ebx
	idiv_reg32 esp
	idiv_reg32 ebp
	idiv_reg32 esi
	idiv_reg32 edi

// ------------------- FE = INC/DEC r/m8 -------------------------------
op_fe_USE32:
	modrm_jump_32
// 0
	modrm_help_0_0 inc_byte
	modrm_help_0_0 dec_byte
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word op_fe_callback, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_0_40 inc_byte
	modrm_help_0_40 dec_byte
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_0_80 inc_byte
	modrm_help_0_80 dec_byte
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xc0 = mod = 11b => register operand
// INC reg8
	.word inc_al, inc_cl, inc_dl, inc_bl, inc_ah, inc_ch, inc_dh, inc_bh
// DEC reg8
	.word dec_al, dec_cl, dec_dl, dec_bl, dec_ah, dec_ch, dec_dh, dec_bh
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_0_help inc_byte, inc_byte,,"r0"
	modrm_0_help dec_byte, dec_byte,,"r0"

	.ltorg
	
// ------------------- FE callback handling ----------------------
	.global	op_fe_callback
op_fe_callback:
	mrs		r0, cpsr				// Save flags to r0
	ldrb	r1,[r12],#1				// Load low byte to r1, increment r12 by 1
	ldrb	r2,[r12],#1				// Load high byte to r2, increment r12 by 1
	orr		r1, r2, lsl #8			// r1 = low byte | (high byte << 8)
	cmp		r1, #0
	beq		.callback_0_4F05
	cmp		r1, #3
	beq		.callback_3_XMS
	b		.unknown

	//------
	// Callback 0 = Set VESA_Bank (same as INT10h AX=4F05)
	// BH = subfunction
	//	00h select video memory window
	//		DX = window address in video memory (in granularity units)
	//	01h get video memory window
	//		Return:
	//		DX = window address in video memory (in gran. units).
	//		BL = window number
	//			00h window A
	//			01h window B.
	//------
.callback_0_4F05:
	ldr		r2, =VESA_Bank
	ands	r1, ebx, #0xFF00				// r1 = BH value			
	bne		1f
	//------
	// Select video memory window
	//------
	and		r1, edx, #7						// We support 8 banks of 64KB
	lsl		r1, #16							// r0 = (0..7)*64K
	str		r1, [r2]						// Save the new VESA bank value
#ifdef IOS
	push	{r0, r9, r12, lr}
	bl		_SetupEGAVGA_A000				// Go setting up the SVGA addressing
	pop		{r0, r9, r12, lr}
#else
	push	{r0, r12, lr}
	bl		Setup_EGAVGA_A000				// Go setting up the SVGA addressing
	pop		{r0, r12, lr}
#endif
	b		restore_flags_from_r0			// Return to loop from the subroutine
	//------
	// Get video memory window
	//------	
1:	bic		ebx, #0xFF						// BL = 0
	ldr		r1, [r2]						// Get the current VESA bank value
	orr		edx, r1, edx, lsr #16
	ror		edx, #16
	b		restore_flags_from_r0			// Return to loop from the subroutine

	//------
	// Callback 3 = handle XMS request (used to be INT B0)
	//------
.callback_3_XMS:
	//------
	// Fix the new flags
	//------
	msr		cpsr_f,r0							// Restore ARM flags from r0
	ldr		r1,[sp, #SP_FLAGS]					// Get the "Trap", "Interrupts Enabled" and "Direction" flags
	join_ARM_to_X86_flags r1
	mov		r0, eax, lsr #8
	and		r0, #0xFF							// r0 = AH value
	//-------
	// Save registers to memory
	//-------
	ldr		r2,=registers
	stmia	r2!,{r4-r12}						// Save emulation registers to global memory
	str		r1, [r2], #4						// Save flags to global memory
	//-------
	// Save the logical segment registers (ES, CS, SS, DS, FS, GS)
	//-------
	add		r1, sp, #SP_ES_VALUE
	ldmia	r1, {r4-r9}							// ES, CS, SS, DS, FS, GS values
	stmia	r2, {r4-r9}							// Save to REG_ES .. REG_GS to global memory
	//-------
	// Call the XMS handler, a0 = AH value
	//-------
#ifdef IOS
	bl		_XMS									// Call the external XMS handler function
#else
	bl		XMS									// Call the external XMS handler function
#endif
	//-------
	// Load the (possibly changed) registers
	//-------
	ldr		r1,=registers
	cmp		r0, #0								// Did the XMS Handler return 0?
	ldmia	r1!,{r4-r12}						// Load emulation registers from global memory
	//-------
	// Restore flags and continue running
	//-------
	ldr		r0, [r1]							// Get the flags from global memory
	bne		1f									// Jump if XMS() returned != 0
	popped_flags_to_ARM r0						// ... set the processor flags, ...
	b		loop								// XMS Handler returned 0, we can continue running.
	//-------
	// Unsupported INT call, break into debugger
	//-------
1:	ldr		r2, =BreakReason					// Check if the INT handler has already set a break reason...
	ldr		r1, [r2]
	cmp		r1, #0
	ldreq	r1, =BRUnsXMS						// ... tell "Unsupported XMS call" if not, ...
	streq	r1, [r2]
	popped_flags_to_ARM r0						// ... set the processor flags, ...
	msr		cpsr_f,r0
	b		debug_trap_false					// ... stop running and go to the debugger.

	
#if 0
	
	get_cseip_hword t0							// t0 = the callback number
	.set noreorder
	beqz	t0, .callback_0_4F05
	subu	t0, 2
	beqz	t0, .callback_2_4F09
	subu	t0, 1
	beqz	t0, .callback_3_XMS
	nop
	j		.unknown
	nop
	.set reorder
	
	//------
	// Callback 2 = Set Primary Palette (same as INT10h AX=4F09)
	//	CX = number of entries (Warcraft 2: ecx=00000100)
	//	DX = starting index (Warcraft 2: edx=00000000)
	//	ES:EDI = buffer of red, green, blue, alpha bytes. (Warcraft 2: 270:100F63E8)
	//------
.callback_2_4F09_noram:	
	jump_to_noram_handler .unknown .unknown 1f

.callback_2_4F09:
	//------
	// Calculate ES:EDI address
	//------
	move	t0, edi							// t0 = EDI
	li		t3, -1							// t3 mask = 32-bit memory addressing
	calc_eff_seg_ES_from_t0t3_RAM .callback_2_4F09_noram
	//------
	// Write the palette buffer values to BG_PALETTE
	//------
1:	la		t3, BG_PALETTE
	andi	t2, ecx, 0xFFFF					// t2 = number of palette entries to write
	andi	t0, edx, 0xFFFF
	sll		t0, 1							// 16-bit addressing into BG_PALETTE
	addu	t3, t0							// t3 = &BG_PALETTE[dx]
	//------
	// Loop to write the palette values.
	//	t0 = temp reg
	//	t1 = palette halfword to build
	//	t2 = loop index
	//	t3 = BG_PALETTE pointer
	//	eff_seg = input buffer pointer
	//------
1:	lbu		t0, 0(eff_seg)					// Red value
	srl		t1, t0, 2
	lbu		t0, 1(eff_seg)					// Green value
	srl		t0, 2
	sll		t0, 5
	or		t1, t0	
	lbu		t0, 2(eff_seg)					// Blue value
	srl		t0, 2
	sll		t0, 10
	or		t1, t0	
	addu	eff_seg, 4
	sh		t1, 0(t3)						// Store the palette value
	subu	t2, 1
	addu	t3, 2
	bgtz	t2, 1b
	j		loop
		
	//------
	// Callback 3 = handle XMS request (used to be INT B0)
	//------
.callback_3_XMS:
	//------
	// Fix the new flags
	//------
	prepare_lazy_flags
	//-------
	// Save the registers to memory
	//-------
	la		t0, registers
	sw		eax, (4*0)(t0)						// Get EAX
	sw		ecx, (4*1)(t0)						// Get ECX
	sw		edx, (4*2)(t0)						// Get EDX
	sw		ebx, (4*3)(t0)						// Get EBX
	sw		esp, (4*4)(t0)						// Get ESP
	sw		ebp, (4*5)(t0)						// Get EBP
	sw		esi, (4*6)(t0)						// Get ESI
	sw		edi, (4*7)(t0)						// Get EDI
	sw		cseip, (4*8)(t0)					// Save physical CS:EIP
	sw		flags, (4*9)(t0)					// Save CPU flags
	//------
	// Fix effective segments, REAL MODE!
	//------
	srl		eff_es, 4
	srl		eff_cs, 4
	srl		eff_ss, 4
	srl		eff_ds, 4
	sw		eff_es, (4*10)(t0)					// Save ES
	sw		eff_cs, (4*11)(t0)					// Save CS
	sw		eff_ss, (4*12)(t0)					// Save SS
	sw		eff_ds, (4*13)(t0)					// Save DS
	sw		stack_mask, (4*16)(t0)				// Save the stack_mask register
	lw		t0, GP_eff_fs(gp)
	lw		t1, GP_eff_gs(gp)
	srl		t0, 4
	srl		t1, 4								// Fixed in version 0.33, 2012-02-04!
	sw		t0, "[sp, #SP_FS_BASE]"					// Save FS
	sw		t1, "[sp, #SP_GS_BASE]"					// Save GS
	//-------
	// Call the XMS handler, a0 = AH value
	//-------
	srl		a0, eax, 8
	andi	a0, 0xFF							// a0 = AH value
	jal		XMS									// Call the external XMS handler function
	move	t3, v0								// t3 = XMS return value
	//------
	// Load the emulation registers
	//------
	la		t0, registers
	lw		eax, (4*0)(t0)						// Get EAX
	lw		ecx, (4*1)(t0)						// Get ECX
	lw		edx, (4*2)(t0)						// Get EDX
	lw		ebx, (4*3)(t0)						// Get EBX
	lw		esp, (4*4)(t0)						// Get ESP
	lw		ebp, (4*5)(t0)						// Get EBP
	lw		esi, (4*6)(t0)						// Get ESI
	lw		edi, (4*7)(t0)						// Get EDI
	lw		cseip, (4*8)(t0)					// Get physical CS:EIP
	lw		flags, (4*9)(t0)					// Get CPU flags
	lw		eff_es, (4*10)(t0)					// Get ES
	lw		eff_cs, (4*11)(t0)					// Get CS
	lw		eff_ss, (4*12)(t0)					// Get SS
	lw		eff_ds, (4*13)(t0)					// Get DS
	lw		stack_mask, (4*16)(t0)				// Restore the stack_mask register
	//------
	// Fix effective segments, REAL MODE!
	//------
	sll		eff_es, 4
	sll		eff_cs, 4
	sll		eff_ss, 4
	sll		eff_ds, 4
	lw		t0, "[sp, #SP_FS_BASE]"					// Get FS
	lw		t1, "[sp, #SP_GS_BASE]"					// Get GS
	sll		t0, 4
	sll		t1, 4
	sw		t0, GP_eff_fs(gp)
	sw		t1, GP_eff_gs(gp)
	//------
	// Prepare the proper lazy flags initial state, and go back to loop
	//------
	prepare_lazy_flags
	sw		flags, GP_FLAGS(gp)					// Let the host know of the current IRQ flag.
	//-------
	// Check the return value and continue running
	//-------
	bnez	t3, 1f								// XMS handler returned != 0, we need to break!
	j		loop
	//-------
	// Unsupported XMS call, break into debugger
	//-------
1:	la		t1, BRUnsXMS						// ... tell "Unsupported XMS call", ...
	sw		t1, BreakReason
	j		debug_trap_false					// ... stop running and go to the debugger.

#endif

	.ltorg
	
// ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
op_ff_USE32:
	modrm_jump_32
	modrm_tbl_1_oper inc_dword dec_dword call_near_32 call_far_32 jmp_near_32 jmp_far_32 push_dword back1
	.word op_40_USE32, op_41_USE32, op_42_USE32, op_43_USE32, op_44_USE32, op_45_USE32, op_46_USE32, op_47_USE32		// INC EAX ... INC EDI
	.word op_48_USE32, op_49_USE32, op_4a_USE32, op_4b_USE32, op_4c_USE32, op_4d_USE32, op_4e_USE32, op_4f_USE32		// DEC EAX ... DEC EDI
	.word call_near_eax, call_near_ecx, call_near_edx, call_near_ebx, call_near_esp, call_near_ebp, call_near_esi, call_near_edi
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word jmp_near_eax, jmp_near_ecx, jmp_near_edx, jmp_near_ebx, jmp_near_esp, jmp_near_ebp, jmp_near_esi, jmp_near_edi
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_1_oper inc_dword dec_dword call_near_32 call_far_32 jmp_near_32 jmp_far_32 push_dword skip _r0
	
// ============= INC DWORD ==============

	.global	inc_dword_r0_bp
inc_dword_r0_bp:
	mem_handler_bp	
	.global	inc_dword_r0
inc_dword_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
	//-------
	// Memory location in normal RAM.
	//-------
#if USE_SEIBUSPI
	//-------
	// Raiden Fighters Jet, statistics after running two demo games:
	// Aligned RAM access: 525063 times, unaligned RAM access: 0 times.
	// It is worth to test for aligned access and use faster code in that situation.
	//-------
1:	tst		r2, #3
	bne		1f
	ldr		r1, [r2]
	// ----- Perform the increment
	mrs		r0,cpsr					// Get original flags to r0.
	adds	r1, #1					// Add 1 to the value
	// ----- Save the value
	str		r1, [r2]
	// ----- Fix the Carry flag
	mrs		r1,cpsr					// r1 = Flags after the increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
#endif	
1:	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #2]
	orr		r1, r0, r1, lsl #8
	ldrb	r0,[r2, #3]
	orr		r1, r3, lsl #16
	orr		r1, r0, lsl #24
	// ----- Perform the increment
	mrs		r0,cpsr					// Get original flags to r0.
	adds	r1, #1					// Add 1 to the value
	// ----- Save the value
	strb	r1,[r2]
	lsr		r1, #8
	strb	r1,[r2, #1]
	lsr		r1, #8
	strb	r1,[r2, #2]
	lsr		r1, #8
	strb	r1,[r2, #3]
	// ----- Fix the Carry flag
	mrs		r1,cpsr					// r1 = Flags after the increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0

// ============= DEC DWORD ==============

	.global	dec_dword_r0_bp
dec_dword_r0_bp:
	mem_handler_bp	
	.global	dec_dword_r0
dec_dword_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
#if USE_SEIBUSPI
	//-------
	// Raiden Fighters Jet, statistics after running two demo games:
	// Aligned RAM access: 450149 times, unaligned RAM access: 0 times.
	// It is worth to test for aligned access and use faster code in that situation.
	//-------
1:	tst		r2, #3
	bne		1f
	ldr		r1, [r2]
	// ----- Perform the decrement
	mrs		r0,cpsr					// Get original flags to r0.
	subs	r1, #1					// Sub 1 from the value
	// ----- Save the value
	str		r1, [r2]
	// ----- Fix the Carry flag
	mrs		r1,cpsr					// r1 = Flags after the increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
#endif	
1:	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #2]
	orr		r1, r0, r1, lsl #8
	ldrb	r0,[r2, #3]
	orr		r1, r3, lsl #16
	orr		r1, r0, lsl #24
	// ----- Perform the decrement
	mrs		r0,cpsr					// Get original flags to r0.
	subs	r1, #1					// Sub 1 from the value
	// ----- Save the value
	strb	r1,[r2]
	lsr		r1, #8
	strb	r1,[r2, #1]
	lsr		r1, #8
	strb	r1,[r2, #2]
	lsr		r1, #8
	strb	r1,[r2, #3]
	// ----- Fix the Carry flag
	mrs		r1,cpsr					// r1 = Flags after the increment
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0

// ============= CALL NEAR ==============
// Note! This code should be re-executable, as paging may cause a page fault,
// after which this opcode gets re-run from the start. Adjust stack pointer
// only after all page init code has been run!

	.global	call_near_32_r0_bp
call_near_32_r0_bp:
	mem_handler_bp	
	.global	call_near_32_r0
call_near_32_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r0, r1, lsl #8
	orr		r1, r3, lsl #16
	orr		r1, r2, lsl #24
.call_near_t1:
	ldr		r2,[sp, #SP_PHYS_CS]				// Get current physical CS from stack
	sub		r0, r12, r2							// r2 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_dword r0 r3 lr
#if USE_SEIBUSPI
	//-------
	// Since the TLBTable[0] is flagged to use C callbacks, if SP_CS_BASE is zero
	// we need to determine the proper base from 
	//	- TLBTable[0x00200000>>11] (if address >= 0x00200000)
	//	- TLBTable[0x800>>11] (if address < 0x00200000)
	//-------
	ldr		r2, [sp, #SP_TLBTABLE]				// r0 = pointer to the external TLB table
	mov		r0, r1, lsr #TLB_GRAN_BITS			// SEIBU SPI hack!
	ldr		r2, [r2, r0, lsl #2]
	add		r12, r2, r1							// New physical IP = physical CS:0000 + new logical IP
	str		r2, [sp, #SP_PHYS_CS]				// Save new physical CS to stack
#if S2TEST
	mrs		r0,cpsr					// Get original flags to r0.
	cmp		r12, #1
	bne		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
	b		.unknown
#endif	
#else
	add		r12, r2, r1							// New physical IP = physical CS:0000 + new logical IP
#endif	
	b		loop

.macro call_near_reg32 reg
	mov		r1, \reg
	b		.call_near_t1
.endm

call_near_eax:
	call_near_reg32 eax
call_near_ecx:
	call_near_reg32 ecx
call_near_edx:
	call_near_reg32 edx
call_near_ebx:
	call_near_reg32 ebx
call_near_esp:
	call_near_reg32 esp
call_near_ebp:
	call_near_reg32 ebp
call_near_esi:
	call_near_reg32 esi
call_near_edi:
	call_near_reg32 edi

// ============= CALL FAR ==============

	.global	call_far_32_r0_bp
call_far_32_r0_bp:
	mem_handler_bp	
	.global	call_far_32_r0
call_far_32_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
1:	mov		r0, #32
	str		r0, [sp, #SP_FREE3]		// Save the flag telling this is a USE32 call far
	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #2]
	orr		r1, r0, r1, lsl #8
	ldrb	r0,[r2, #3]
	orr		r1, r3, lsl #16
	ldrb	r3,[r2, #4]
	ldrb	r2,[r2, #5]
	orr		r1, r0, lsl #24			// r1 = new IP value
	orr		r2, r3, r2, lsl #8		// r2 = new CS value
	b		cpu_call_far_r1r2		// Jump to common handling for both real and protected mode.

// ============= JMP NEAR ==============

	.global	jmp_near_32_r0_bp
jmp_near_32_r0_bp:
	mem_handler_bp	
	.global	jmp_near_32_r0
jmp_near_32_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r0, r1, lsl #8
	orr		r1, r3, lsl #16
	orr		r1, r2, lsl #24
.jmp_near_t1:
	ldr		r2, [sp, #SP_PHYS_CS]				// r2 = physical CS
	add		r12, r2, r1							// r12 = new physical CS:IP
	b		loop

.macro jmp_near_reg32 reg
	mov		r1, \reg
	b		.jmp_near_t1
.endm

jmp_near_eax:
	jmp_near_reg32 eax
jmp_near_ecx:
	jmp_near_reg32 ecx
jmp_near_edx:
	jmp_near_reg32 edx
jmp_near_ebx:
	jmp_near_reg32 ebx
jmp_near_esp:
	jmp_near_reg32 esp
jmp_near_ebp:
	jmp_near_reg32 ebp
jmp_near_esi:
	jmp_near_reg32 esi
jmp_near_edi:
	jmp_near_reg32 edi

// ============= JMP FAR ==============

	.global	jmp_far_32_r0_bp
jmp_far_32_r0_bp:
	mem_handler_bp	
	.global	jmp_far_32_r0
jmp_far_32_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode bad_MODEX_opcode
	//-------
	// Memory location in normal RAM.
	// On input:
	//	r2 = physical memory address
	// First get the jump target :
	//	r1 = new IP (offset)
	//	r2 = new CS (segment)
	//-------
1:	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #2]
	orr		r1, r0, r1, lsl #8
	ldrb	r0,[r2, #3]
	orr		r1, r3, lsl #16
	ldrb	r3,[r2, #4]
	ldrb	r2,[r2, #5]
	orr		r1, r0, lsl #24			// r1 = new IP value
	orr		r2, r3, r2, lsl #8		// r2 = new CS value
	b		cpu_jmp_far_r1r2		// Jump to common handling for both real and protected mode.

// ============= PUSH ==============

	.global	push_dword_r0_bp
push_dword_r0_bp:
	mem_handler_bp	
	.global	push_dword_r0
push_dword_r0:
	//-------
	// Calculate and jump to correct memory access handler.
	//-------
	mem_handler_jump_r0r3 1f bad_EGA_opcode push_MODEX_32_r2
	//-------
	// Memory location in normal RAM.
	//-------
1:	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #2]
	ldrb	r2,[r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	push_dword r0 r1 r2
	b		loop


	//---------------------------------------------
	// Real Mode, Operand Size = prefix, Address Size = prefix
	//---------------------------------------------
// 0x00 (prefix 6667)
	.global op_00_USE32, op_01_USE32, op_02_USE32, op_03_USE32, op_04, op_05_USE32
	.global op_08_USE32, op_09_USE32, op_0a_USE32, op_0b_USE32, op_0c, op_0d_USE32, op_0e_USE32, op_0f_USE32_r
	.global    op_13_USE32, op_14, op_15_USE32  
	.global  op_19_USE32,  op_1b_USE32,  op_1d_USE32  
	.global  op_21_USE32, op_22_USE32, op_23_USE32, op_24, op_25_USE32, op_26_USE32_r 
	.global op_28_USE32, op_29_USE32, op_2a_USE32, op_2b_USE32,  op_2d_USE32, op_2e_USE32_r 
	.global op_30_USE32, op_31_USE32, op_32_USE32, op_33_USE32, op_34, op_35_USE32, op_36_USE32_r 
	.global op_38_USE32, op_39_USE32, op_3a_USE32, op_3b_USE32, op_3c, op_3d_USE32, op_3e_USE32_r
// 0x40 (prefix 6667)
	.global op_60_USE32, op_61_USE32,    op_65_USE32_r
	.global op_68_USE32, op_69_USE32, op_6a_USE32, op_6b_USE32
// 0x80 (prefix 6667)
	.global op_80_USE32, op_81_USE32, op_82_USE32, op_83_USE32, op_84_USE32, op_85_USE32, op_86_USE32, op_87_USE32
	.global op_88_USE32, op_89_USE32, op_8a_USE32, op_8b_USE32,  op_8d_USE32,  op_8f_USE32
	.global op_98_USE32, op_99_USE32, op_9a_USE32
	.global op_a0_USE32, op_a1_USE32, op_a2_USE32, op_a3_USE32
	.global op_a8, op_a9_USE32
	.global op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	.global op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
// 0xC0 (prefix 6667)
	.global op_c0_USE32, op_c1_USE32, op_c2_USE32, op_c3_USE32,   op_c6_USE32, op_c7_USE32
	.global  op_c9_USE32
	.global op_d0_USE32, op_d1_USE32, op_d2_USE32, op_d3_USE32
	.global   op_e2_USE32
	.global   op_f2_USE32, op_f3_USE32,  op_f5, op_f6_USE32, op_f7_USE32
	.global op_fe_USE32, op_ff_USE32
	//---------------------------------------------
	// USE16: Protected Mode, Operand Size = prefix (32), Address Size = prefix (32)
	// USE32: Protected Mode, Operand Size = default (32), Address Size = default (32)
	//---------------------------------------------
// 0x00 (prefix 6667 or USE32 segment)
	.global op_00_USE32, op_01_USE32, op_02_USE32, op_03_USE32, op_05_USE32
	.global op_08_USE32, op_09_USE32, op_0a_USE32, op_0b_USE32, op_0d_USE32
	.global    op_13_USE32, op_15_USE32,  
	.global  op_19_USE32,  op_1b_USE32,  op_1d_USE32
	.global  op_21_USE32, op_22_USE32, op_23_USE32, op_24, op_25_USE32, op_26_USE32_p
	.global op_28_USE32, op_29_USE32, op_2a_USE32, op_2b_USE32,  op_2d_USE32, op_2e_USE32_p
	.global op_30_USE32, op_31_USE32, op_32_USE32, op_33_USE32, op_34, op_35_USE32, op_36_USE32_p
	.global op_38_USE32, op_39_USE32, op_3a_USE32, op_3b_USE32, op_3c, op_3d_USE32, op_3e_USE32_p
// 0x40 (prefix 6667 or USE32 segment)
	.global op_60_USE32, op_61_USE32,    op_65_USE32_p, op_66, op_67
// 0x80 (prefix 6667 or USE32 segment)
	.global op_88_USE32, op_89_USE32, op_8a_USE32, op_8b_USE32, op_8c_USE32, op_8d_USE32, op_8f_USE32
	.global op_98_USE32, op_99_USE32, op_9c_USE32
// 0xC0 (prefix 6667 or USE32 segment)
	.global op_c0_USE32, op_c1_USE32, op_c2_USE32, op_c3_USE32, op_c6_USE32, op_c7_USE32
	.global  op_c9_USE32,    op_cd_p,  op_cf_USE32_p
	.global	op_d8_USE32, op_d9_USE32, op_da_USE32, op_db_USE32, op_dc_USE32, op_dd_USE32, op_de_USE32, op_df_USE32
	.global op_e8_rel32, op_e9_rel32, op_ea_USE32_p
	.global op_fe_USE32, op_ff_USE32_p

