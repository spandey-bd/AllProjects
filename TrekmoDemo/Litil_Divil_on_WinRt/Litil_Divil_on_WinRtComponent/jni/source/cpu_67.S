//=============================================================================
// cpu_67.s
//
// This file contains normal opcode handlers for when running in a USE16 segment
// and a prefix 0x67 (32-bit addressing) is active. This is similar to a situation
// when running in a USE32 segment with a prefix 0x66 (32-bit operands) active.
// In principle this file only contains the memory address parsing for word-size
// opcodes (since 8-bit opcodes are similar to non-prefixed versions in "cpu_386.s")
// with the actual handlers being in "cpu.s".
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

	.arch armv5te
	.file	"cpu_67.s"

#include "defines.inc"
#include "macros.inc"
#include "gen_67.inc"

	.text
	.align	2

// ------------------- 01 = ADD r/m16, r16 -----------------------------
//
op_01_67:
	modrm_jump_32
	modrm_tbl_1 add
// 0xC0 = two register operands
	.word add_ax_ax, add_cx_ax, add_dx_ax, add_bx_ax, add_sp_ax, add_bp_ax, add_si_ax, add_di_ax
	.word add_ax_cx, add_cx_cx, add_dx_cx, add_bx_cx, add_sp_cx, add_bp_cx, add_si_cx, add_di_cx
	.word add_ax_dx, add_cx_dx, add_dx_dx, add_bx_dx, add_sp_dx, add_bp_dx, add_si_dx, add_di_dx
	.word add_ax_bx, add_cx_bx, add_dx_bx, add_bx_bx, add_sp_bx, add_bp_bx, add_si_bx, add_di_bx
	.word add_ax_sp, add_cx_sp, add_dx_sp, add_bx_sp, add_sp_sp, add_bp_sp, add_si_sp, add_di_sp
	.word add_ax_bp, add_cx_bp, add_dx_bp, add_bx_bp, add_sp_bp, add_bp_bp, add_si_bp, add_di_bp
	.word add_ax_si, add_cx_si, add_dx_si, add_bx_si, add_sp_si, add_bp_si, add_si_si, add_di_si
	.word add_ax_di, add_cx_di, add_dx_di, add_bx_di, add_sp_di, add_bp_di, add_si_di, add_di_di

	modrm_1_genall add add_r0_r16

// ------------------- 03 = ADD r16, r/m16 -----------------------------
//
op_03_67:
	modrm_jump_32
	modrm_tbl_3 add
// 0xC0 = two register operands
	.word add_ax_ax, add_ax_cx, add_ax_dx, add_ax_bx, add_ax_sp, add_ax_bp, add_ax_si, add_ax_di
	.word add_cx_ax, add_cx_cx, add_cx_dx, add_cx_bx, add_cx_sp, add_cx_bp, add_cx_si, add_cx_di
	.word add_dx_ax, add_dx_cx, add_dx_dx, add_dx_bx, add_dx_sp, add_dx_bp, add_dx_si, add_dx_di
	.word add_bx_ax, add_bx_cx, add_bx_dx, add_bx_bx, add_bx_sp, add_bx_bp, add_bx_si, add_bx_di
	.word add_sp_ax, add_sp_cx, add_sp_dx, add_sp_bx, add_sp_sp, add_sp_bp, add_sp_si, add_sp_di
	.word add_bp_ax, add_bp_cx, add_bp_dx, add_bp_bx, add_bp_sp, add_bp_bp, add_bp_si, add_bp_di
	.word add_si_ax, add_si_cx, add_si_dx, add_si_bx, add_si_sp, add_si_bp, add_si_si, add_si_di
	.word add_di_ax, add_di_cx, add_di_dx, add_di_bx, add_di_sp, add_di_bp, add_di_si, add_di_di

	modrm_3_genall add add_r16_r0

// ------------------- 09 = OR r/m16, r16 -----------------------------
//
op_09_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 or
// 0xC0 = two register operands
	.word or_ax_ax, or_cx_ax, or_dx_ax, or_bx_ax, or_sp_ax, or_bp_ax, or_si_ax, or_di_ax
	.word or_ax_cx, or_cx_cx, or_dx_cx, or_bx_cx, or_sp_cx, or_bp_cx, or_si_cx, or_di_cx
	.word or_ax_dx, or_cx_dx, or_dx_dx, or_bx_dx, or_sp_dx, or_bp_dx, or_si_dx, or_di_dx
	.word or_ax_bx, or_cx_bx, or_dx_bx, or_bx_bx, or_sp_bx, or_bp_bx, or_si_bx, or_di_bx
	.word or_ax_sp, or_cx_sp, or_dx_sp, or_bx_sp, or_sp_sp, or_bp_sp, or_si_sp, or_di_sp
	.word or_ax_bp, or_cx_bp, or_dx_bp, or_bx_bp, or_sp_bp, or_bp_bp, or_si_bp, or_di_bp
	.word or_ax_si, or_cx_si, or_dx_si, or_bx_si, or_sp_si, or_bp_si, or_si_si, or_di_si
	.word or_ax_di, or_cx_di, or_dx_di, or_bx_di, or_sp_di, or_bp_di, or_si_di, or_di_di

	modrm_1_genall or or_r0_r16

// ------------------- 0B = OR r16, r/m16 ------------------------------
//
op_0b_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 or
	.word or_ax_ax, or_ax_cx, or_ax_dx, or_ax_bx, or_ax_sp, or_ax_bp, or_ax_si, or_ax_di
	.word or_cx_ax, or_cx_cx, or_cx_dx, or_cx_bx, or_cx_sp, or_cx_bp, or_cx_si, or_cx_di
	.word or_dx_ax, or_dx_cx, or_dx_dx, or_dx_bx, or_dx_sp, or_dx_bp, or_dx_si, or_dx_di
	.word or_bx_ax, or_bx_cx, or_bx_dx, or_bx_bx, or_bx_sp, or_bx_bp, or_bx_si, or_bx_di
	.word or_sp_ax, or_sp_cx, or_sp_dx, or_sp_bx, or_sp_sp, or_sp_bp, or_sp_si, or_sp_di
	.word or_bp_ax, or_bp_cx, or_bp_dx, or_bp_bx, or_bp_sp, or_bp_bp, or_bp_si, or_bp_di
	.word or_si_ax, or_si_cx, or_si_dx, or_si_bx, or_si_sp, or_si_bp, or_si_si, or_si_di
	.word or_di_ax, or_di_cx, or_di_dx, or_di_bx, or_di_sp, or_di_bp, or_di_si, or_di_di

	modrm_3_genall or or_r16_r0

// ------------------- 11 = ADC r/m16, r16 ------------------------------
//
	.global	op_11_67
op_11_67:
	modrm_jump_32
	modrm_tbl_1 adc
	.word adc_ax_ax, adc_cx_ax, adc_dx_ax, adc_bx_ax, adc_sp_ax, adc_bp_ax, adc_si_ax, adc_di_ax
	.word adc_ax_cx, adc_cx_cx, adc_dx_cx, adc_bx_cx, adc_sp_cx, adc_bp_cx, adc_si_cx, adc_di_cx
	.word adc_ax_dx, adc_cx_dx, adc_dx_dx, adc_bx_dx, adc_sp_dx, adc_bp_dx, adc_si_dx, adc_di_dx
	.word adc_ax_bx, adc_cx_bx, adc_dx_bx, adc_bx_bx, adc_sp_bx, adc_bp_bx, adc_si_bx, adc_di_bx
	.word adc_ax_sp, adc_cx_sp, adc_dx_sp, adc_bx_sp, adc_sp_sp, adc_bp_sp, adc_si_sp, adc_di_sp
	.word adc_ax_bp, adc_cx_bp, adc_dx_bp, adc_bx_bp, adc_sp_bp, adc_bp_bp, adc_si_bp, adc_di_bp
	.word adc_ax_si, adc_cx_si, adc_dx_si, adc_bx_si, adc_sp_si, adc_bp_si, adc_si_si, adc_di_si
	.word adc_ax_di, adc_cx_di, adc_dx_di, adc_bx_di, adc_sp_di, adc_bp_di, adc_si_di, adc_di_di

	modrm_1_genall adc adc_r0_r16

// ------------------- 13 = ADC r16, r/m16 ------------------------------
//
	.global	op_13_67
op_13_67:
	modrm_jump_32
	modrm_tbl_3 adc
	.word adc_ax_ax, adc_ax_cx, adc_ax_dx, adc_ax_bx, adc_ax_sp, adc_ax_bp, adc_ax_si, adc_ax_di
	.word adc_cx_ax, adc_cx_cx, adc_cx_dx, adc_cx_bx, adc_cx_sp, adc_cx_bp, adc_cx_si, adc_cx_di
	.word adc_dx_ax, adc_dx_cx, adc_dx_dx, adc_dx_bx, adc_dx_sp, adc_dx_bp, adc_dx_si, adc_dx_di
	.word adc_bx_ax, adc_bx_cx, adc_bx_dx, adc_bx_bx, adc_bx_sp, adc_bx_bp, adc_bx_si, adc_bx_di
	.word adc_sp_ax, adc_sp_cx, adc_sp_dx, adc_sp_bx, adc_sp_sp, adc_sp_bp, adc_sp_si, adc_sp_di
	.word adc_bp_ax, adc_bp_cx, adc_bp_dx, adc_bp_bx, adc_bp_sp, adc_bp_bp, adc_bp_si, adc_bp_di
	.word adc_si_ax, adc_si_cx, adc_si_dx, adc_si_bx, adc_si_sp, adc_si_bp, adc_si_si, adc_si_di
	.word adc_di_ax, adc_di_cx, adc_di_dx, adc_di_bx, adc_di_sp, adc_di_bp, adc_di_si, adc_di_di

	modrm_3_genall adc adc_r16_r0

// ------------------- 19 = SBB r/m16, r16 ------------------------------
//
	.global	op_19_67
op_19_67:
	modrm_jump_32
	modrm_tbl_1 sbb
	.word sbb_ax_ax, sbb_cx_ax, sbb_dx_ax, sbb_bx_ax, sbb_sp_ax, sbb_bp_ax, sbb_si_ax, sbb_di_ax
	.word sbb_ax_cx, sbb_cx_cx, sbb_dx_cx, sbb_bx_cx, sbb_sp_cx, sbb_bp_cx, sbb_si_cx, sbb_di_cx
	.word sbb_ax_dx, sbb_cx_dx, sbb_dx_dx, sbb_bx_dx, sbb_sp_dx, sbb_bp_dx, sbb_si_dx, sbb_di_dx
	.word sbb_ax_bx, sbb_cx_bx, sbb_dx_bx, sbb_bx_bx, sbb_sp_bx, sbb_bp_bx, sbb_si_bx, sbb_di_bx
	.word sbb_ax_sp, sbb_cx_sp, sbb_dx_sp, sbb_bx_sp, sbb_sp_sp, sbb_bp_sp, sbb_si_sp, sbb_di_sp
	.word sbb_ax_bp, sbb_cx_bp, sbb_dx_bp, sbb_bx_bp, sbb_sp_bp, sbb_bp_bp, sbb_si_bp, sbb_di_bp
	.word sbb_ax_si, sbb_cx_si, sbb_dx_si, sbb_bx_si, sbb_sp_si, sbb_bp_si, sbb_si_si, sbb_di_si
	.word sbb_ax_di, sbb_cx_di, sbb_dx_di, sbb_bx_di, sbb_sp_di, sbb_bp_di, sbb_si_di, sbb_di_di

	modrm_1_genall sbb sbb_r0_r16

// ------------------- 1B = SBB r16, r/m16 ------------------------------
//
	.global	op_1b_67
op_1b_67:
	modrm_jump_32
	modrm_tbl_3 sbb
	.word sbb_ax_ax, sbb_ax_cx, sbb_ax_dx, sbb_ax_bx, sbb_ax_sp, sbb_ax_bp, sbb_ax_si, sbb_ax_di
	.word sbb_cx_ax, sbb_cx_cx, sbb_cx_dx, sbb_cx_bx, sbb_cx_sp, sbb_cx_bp, sbb_cx_si, sbb_cx_di
	.word sbb_dx_ax, sbb_dx_cx, sbb_dx_dx, sbb_dx_bx, sbb_dx_sp, sbb_dx_bp, sbb_dx_si, sbb_dx_di
	.word sbb_bx_ax, sbb_bx_cx, sbb_bx_dx, sbb_bx_bx, sbb_bx_sp, sbb_bx_bp, sbb_bx_si, sbb_bx_di
	.word sbb_sp_ax, sbb_sp_cx, sbb_sp_dx, sbb_sp_bx, sbb_sp_sp, sbb_sp_bp, sbb_sp_si, sbb_sp_di
	.word sbb_bp_ax, sbb_bp_cx, sbb_bp_dx, sbb_bp_bx, sbb_bp_sp, sbb_bp_bp, sbb_bp_si, sbb_bp_di
	.word sbb_si_ax, sbb_si_cx, sbb_si_dx, sbb_si_bx, sbb_si_sp, sbb_si_bp, sbb_si_si, sbb_si_di
	.word sbb_di_ax, sbb_di_cx, sbb_di_dx, sbb_di_bx, sbb_di_sp, sbb_di_bp, sbb_di_si, sbb_di_di

	modrm_3_genall sbb sbb_r16_r0

// ------------------- 21 = AND r/m16, r16 ------------------------------
//
	.global	op_21_67
op_21_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 and
	.word and_ax_ax, and_cx_ax, and_dx_ax, and_bx_ax, and_sp_ax, and_bp_ax, and_si_ax, and_di_ax
	.word and_ax_cx, and_cx_cx, and_dx_cx, and_bx_cx, and_sp_cx, and_bp_cx, and_si_cx, and_di_cx
	.word and_ax_dx, and_cx_dx, and_dx_dx, and_bx_dx, and_sp_dx, and_bp_dx, and_si_dx, and_di_dx
	.word and_ax_bx, and_cx_bx, and_dx_bx, and_bx_bx, and_sp_bx, and_bp_bx, and_si_bx, and_di_bx
	.word and_ax_sp, and_cx_sp, and_dx_sp, and_bx_sp, and_sp_sp, and_bp_sp, and_si_sp, and_di_sp
	.word and_ax_bp, and_cx_bp, and_dx_bp, and_bx_bp, and_sp_bp, and_bp_bp, and_si_bp, and_di_bp
	.word and_ax_si, and_cx_si, and_dx_si, and_bx_si, and_sp_si, and_bp_si, and_si_si, and_di_si
	.word and_ax_di, and_cx_di, and_dx_di, and_bx_di, and_sp_di, and_bp_di, and_si_di, and_di_di

	modrm_1_genall and and_r0_r16

// ------------------- 23 = AND r16, r/m16 ------------------------------
//
op_23_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 and
	.word and_ax_ax, and_ax_cx, and_ax_dx, and_ax_bx, and_ax_sp, and_ax_bp, and_ax_si, and_ax_di
	.word and_cx_ax, and_cx_cx, and_cx_dx, and_cx_bx, and_cx_sp, and_cx_bp, and_cx_si, and_cx_di
	.word and_dx_ax, and_dx_cx, and_dx_dx, and_dx_bx, and_dx_sp, and_dx_bp, and_dx_si, and_dx_di
	.word and_bx_ax, and_bx_cx, and_bx_dx, and_bx_bx, and_bx_sp, and_bx_bp, and_bx_si, and_bx_di
	.word and_sp_ax, and_sp_cx, and_sp_dx, and_sp_bx, and_sp_sp, and_sp_bp, and_sp_si, and_sp_di
	.word and_bp_ax, and_bp_cx, and_bp_dx, and_bp_bx, and_bp_sp, and_bp_bp, and_bp_si, and_bp_di
	.word and_si_ax, and_si_cx, and_si_dx, and_si_bx, and_si_sp, and_si_bp, and_si_si, and_si_di
	.word and_di_ax, and_di_cx, and_di_dx, and_di_bx, and_di_sp, and_di_bp, and_di_si, and_di_di

	modrm_3_genall and and_r16_r0

// ------------------- 29 = SUB r/m16, r16 -----------------------------
op_29_67:
	modrm_jump_32
	modrm_tbl_1 sub
	.word sub_ax_ax, sub_cx_ax, sub_dx_ax, sub_bx_ax, sub_sp_ax, sub_bp_ax, sub_si_ax, sub_di_ax
	.word sub_ax_cx, sub_cx_cx, sub_dx_cx, sub_bx_cx, sub_sp_cx, sub_bp_cx, sub_si_cx, sub_di_cx
	.word sub_ax_dx, sub_cx_dx, sub_dx_dx, sub_bx_dx, sub_sp_dx, sub_bp_dx, sub_si_dx, sub_di_dx
	.word sub_ax_bx, sub_cx_bx, sub_dx_bx, sub_bx_bx, sub_sp_bx, sub_bp_bx, sub_si_bx, sub_di_bx
	.word sub_ax_sp, sub_cx_sp, sub_dx_sp, sub_bx_sp, sub_sp_sp, sub_bp_sp, sub_si_sp, sub_di_sp
	.word sub_ax_bp, sub_cx_bp, sub_dx_bp, sub_bx_bp, sub_sp_bp, sub_bp_bp, sub_si_bp, sub_di_bp
	.word sub_ax_si, sub_cx_si, sub_dx_si, sub_bx_si, sub_sp_si, sub_bp_si, sub_si_si, sub_di_si
	.word sub_ax_di, sub_cx_di, sub_dx_di, sub_bx_di, sub_sp_di, sub_bp_di, sub_si_di, sub_di_di

	modrm_1_genall sub sub_r0_r16

// ------------------- 2B = SUB r16, r/m16 ------------------------------
//
op_2b_67:
	modrm_jump_32
	modrm_tbl_3 sub
	.word sub_ax_ax, sub_ax_cx, sub_ax_dx, sub_ax_bx, sub_ax_sp, sub_ax_bp, sub_ax_si, sub_ax_di
	.word sub_cx_ax, sub_cx_cx, sub_cx_dx, sub_cx_bx, sub_cx_sp, sub_cx_bp, sub_cx_si, sub_cx_di
	.word sub_dx_ax, sub_dx_cx, sub_dx_dx, sub_dx_bx, sub_dx_sp, sub_dx_bp, sub_dx_si, sub_dx_di
	.word sub_bx_ax, sub_bx_cx, sub_bx_dx, sub_bx_bx, sub_bx_sp, sub_bx_bp, sub_bx_si, sub_bx_di
	.word sub_sp_ax, sub_sp_cx, sub_sp_dx, sub_sp_bx, sub_sp_sp, sub_sp_bp, sub_sp_si, sub_sp_di
	.word sub_bp_ax, sub_bp_cx, sub_bp_dx, sub_bp_bx, sub_bp_sp, sub_bp_bp, sub_bp_si, sub_bp_di
	.word sub_si_ax, sub_si_cx, sub_si_dx, sub_si_bx, sub_si_sp, sub_si_bp, sub_si_si, sub_si_di
	.word sub_di_ax, sub_di_cx, sub_di_dx, sub_di_bx, sub_di_sp, sub_di_bp, sub_di_si, sub_di_di

	modrm_3_genall sub sub_r16_r0

// ------------------- 31 = XOR r/m16, r16 ----------------------------
//
op_31_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 xor
// 0xC0 = two register operands
	.word xor_ax_ax, xor_cx_ax, xor_dx_ax, xor_bx_ax, xor_sp_ax, xor_bp_ax, xor_si_ax, xor_di_ax
	.word xor_ax_cx, xor_cx_cx, xor_dx_cx, xor_bx_cx, xor_sp_cx, xor_bp_cx, xor_si_cx, xor_di_cx
	.word xor_ax_dx, xor_cx_dx, xor_dx_dx, xor_bx_dx, xor_sp_dx, xor_bp_dx, xor_si_dx, xor_di_dx
	.word xor_ax_bx, xor_cx_bx, xor_dx_bx, xor_bx_bx, xor_sp_bx, xor_bp_bx, xor_si_bx, xor_di_bx
	.word xor_ax_sp, xor_cx_sp, xor_dx_sp, xor_bx_sp, xor_sp_sp, xor_bp_sp, xor_si_sp, xor_di_sp
	.word xor_ax_bp, xor_cx_bp, xor_dx_bp, xor_bx_bp, xor_sp_bp, xor_bp_bp, xor_si_bp, xor_di_bp
	.word xor_ax_si, xor_cx_si, xor_dx_si, xor_bx_si, xor_sp_si, xor_bp_si, xor_si_si, xor_di_si
	.word xor_ax_di, xor_cx_di, xor_dx_di, xor_bx_di, xor_sp_di, xor_bp_di, xor_si_di, xor_di_di

	modrm_1_genall xor xor_r0_r16

// ------------------- 33 = XOR r16, r/m16 ----------------------------
//
	.global	op_33_67
op_33_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_3 xor
// 0xC0 = two register operands
	.word xor_ax_ax, xor_ax_cx, xor_ax_dx, xor_ax_bx, xor_ax_sp, xor_ax_bp, xor_ax_si, xor_ax_di
	.word xor_cx_ax, xor_cx_cx, xor_cx_dx, xor_cx_bx, xor_cx_sp, xor_cx_bp, xor_cx_si, xor_cx_di
	.word xor_dx_ax, xor_dx_cx, xor_dx_dx, xor_dx_bx, xor_dx_sp, xor_dx_bp, xor_dx_si, xor_dx_di
	.word xor_bx_ax, xor_bx_cx, xor_bx_dx, xor_bx_bx, xor_bx_sp, xor_bx_bp, xor_bx_si, xor_bx_di
	.word xor_sp_ax, xor_sp_cx, xor_sp_dx, xor_sp_bx, xor_sp_sp, xor_sp_bp, xor_sp_si, xor_sp_di
	.word xor_bp_ax, xor_bp_cx, xor_bp_dx, xor_bp_bx, xor_bp_sp, xor_bp_bp, xor_bp_si, xor_bp_di
	.word xor_si_ax, xor_si_cx, xor_si_dx, xor_si_bx, xor_si_sp, xor_si_bp, xor_si_si, xor_si_di
	.word xor_di_ax, xor_di_cx, xor_di_dx, xor_di_bx, xor_di_sp, xor_di_bp, xor_di_si, xor_di_di

	modrm_3_genall xor xor_r16_r0


// ------------------- 39 = CMP r/m16, r16 ----------------------------
//
op_39_67:
	modrm_jump_32
	modrm_tbl_1 cmp
	// 0xC0 = two register operands
	.word cmp_ax_ax, cmp_cx_ax, cmp_dx_ax, cmp_bx_ax, cmp_sp_ax, cmp_bp_ax, cmp_si_ax, cmp_di_ax
	.word cmp_ax_cx, cmp_cx_cx, cmp_dx_cx, cmp_bx_cx, cmp_sp_cx, cmp_bp_cx, cmp_si_cx, cmp_di_cx
	.word cmp_ax_dx, cmp_cx_dx, cmp_dx_dx, cmp_bx_dx, cmp_sp_dx, cmp_bp_dx, cmp_si_dx, cmp_di_dx
	.word cmp_ax_bx, cmp_cx_bx, cmp_dx_bx, cmp_bx_bx, cmp_sp_bx, cmp_bp_bx, cmp_si_bx, cmp_di_bx
	.word cmp_ax_sp, cmp_cx_sp, cmp_dx_sp, cmp_bx_sp, cmp_sp_sp, cmp_bp_sp, cmp_si_sp, cmp_di_sp
	.word cmp_ax_bp, cmp_cx_bp, cmp_dx_bp, cmp_bx_bp, cmp_sp_bp, cmp_bp_bp, cmp_si_bp, cmp_di_bp
	.word cmp_ax_si, cmp_cx_si, cmp_dx_si, cmp_bx_si, cmp_sp_si, cmp_bp_si, cmp_si_si, cmp_di_si
	.word cmp_ax_di, cmp_cx_di, cmp_dx_di, cmp_bx_di, cmp_sp_di, cmp_bp_di, cmp_si_di, cmp_di_di
	
	modrm_1_genall cmp cmp_r0_r16

// ------------------- 3B = CMP r16, r/m16 ----------------------------
//
op_3b_67:
	modrm_jump_32
	modrm_tbl_3 cmp
// 0xC0 = two register operands
	.word cmp_ax_ax, cmp_ax_cx, cmp_ax_dx, cmp_ax_bx, cmp_ax_sp, cmp_ax_bp, cmp_ax_si, cmp_ax_di
	.word cmp_cx_ax, cmp_cx_cx, cmp_cx_dx, cmp_cx_bx, cmp_cx_sp, cmp_cx_bp, cmp_cx_si, cmp_cx_di
	.word cmp_dx_ax, cmp_dx_cx, cmp_dx_dx, cmp_dx_bx, cmp_dx_sp, cmp_dx_bp, cmp_dx_si, cmp_dx_di
	.word cmp_bx_ax, cmp_bx_cx, cmp_bx_dx, cmp_bx_bx, cmp_bx_sp, cmp_bx_bp, cmp_bx_si, cmp_bx_di
	.word cmp_sp_ax, cmp_sp_cx, cmp_sp_dx, cmp_sp_bx, cmp_sp_sp, cmp_sp_bp, cmp_sp_si, cmp_sp_di
	.word cmp_bp_ax, cmp_bp_cx, cmp_bp_dx, cmp_bp_bx, cmp_bp_sp, cmp_bp_bp, cmp_bp_si, cmp_bp_di
	.word cmp_si_ax, cmp_si_cx, cmp_si_dx, cmp_si_bx, cmp_si_sp, cmp_si_bp, cmp_si_si, cmp_si_di
	.word cmp_di_ax, cmp_di_cx, cmp_di_dx, cmp_di_bx, cmp_di_sp, cmp_di_bp, cmp_di_si, cmp_di_di

	modrm_3_genall cmp cmp_r16_r0

// ------------------- Segment Overrides using "opcodetable_16_32" -----------------------

op_26_67:
	ldr		r2, [sp, #SP_ES_BASE]				// r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+5*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_2e_67:
	ldr		r2, [sp, #SP_CS_BASE]				// r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+4*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_36_67:
	ldr		r2, [sp, #SP_SS_BASE]				// r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+3*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_3e_67:
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				// r2 = logical DS segment in high halfword
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+2*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_64_67:
	ldr		r2, [sp, #SP_FS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+1*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_65_67:
	ldr		r2, [sp, #SP_GS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+0*5)*4					// (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler


// ------------------- 67 = Address-size Prefix ------------------------
	.global	op_67_USE16
	.global	op_66_USE32
op_67_USE16:
op_66_USE32:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
	//---------------------------------------------
	// Operand Size = 16bit, Address Size = 32bit
	//---------------------------------------------
opcodetable_16_32:
// 0x00 (prefix 67)
	.word op_00_USE32, op_01_67, op_02_USE32, op_03_67, op_04, op_05, op_06, op_07
	.word op_08_USE32, op_09_67, op_0a_USE32, op_0b_67, op_0c, op_0d, op_0e, op_0f_67
	.word op_10_USE32, op_11_67, op_12_USE32, op_13_67, op_14, op_15, op_16, op_17
	.word op_18_USE32, op_19_67, op_1a_USE32, op_1b_67, op_1c, op_1d, op_1e, op_1f
	.word op_20_USE32, op_21_67, op_22_USE32, op_23_67, op_24, op_25, op_26_67, op_27
	.word op_28_USE32, op_29_67, op_2a_USE32, op_2b_67, op_2c, op_2d, op_2e_67, op_2f
	.word op_30_USE32, op_31_67, op_32_USE32, op_33_67, op_34, op_35, op_36_67, op_37
	.word op_38_USE32, op_39_67, op_3a_USE32, op_3b_67, op_3c, op_3d, op_3e_67, op_3f
// 0x40 (prefix 67)
	.word op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47
	.word op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f
	.word op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57
	.word op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
	.word op_60, op_61, .unknown, .unknown, op_64_67, op_65_67, op_66_67_USE16, op_66_67_USE32
	.word op_68, op_69_67, op_6a, op_6b_67, .unknown, .unknown, .unknown, .unknown
	.word op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	.word op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
// 0x80 (prefix 67)
	.word op_80_USE32, op_81_67, op_82_USE32, op_83_67, op_84_USE32, op_85_67, op_86_USE32, op_87_67
	.word op_88_USE32, op_89_67, op_8a_USE32, op_8b_67, op_8c_67, op_8d_67, op_8e_USE32, op_8f_67
	.word loop, xchg_ax_cx, xchg_ax_dx, xchg_ax_bx, xchg_ax_sp, xchg_ax_bp, xchg_ax_si, xchg_ax_di
	.word op_98, op_99, .unknown, loop, op_9c, op_9d, op_9e, op_9f
	.word op_a0_USE32, op_a1_67, op_a2_USE32, op_a3_67, op_a4_movsb_USE32, op_a5_movsw_USE32, op_a6_cmpsb_USE32, op_a7_cmpsw_USE32
	.word op_a8, op_a9, op_aa_stosb_USE32, op_ab_stosw_USE32, op_ac_lodsb_USE32, op_ad_lodsw_USE32, op_ae_scasb_USE32, op_af_scasw_USE32
	.word op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	.word op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
// 0xC0 (prefix 67)
	.word op_c0_USE32, op_c1_67, op_c2, op_c3, op_c4_67, op_c5_67, op_c6_USE32, op_c7_67
	.word .unknown, .unknown, op_ca, op_cb, .unknown, .unknown, .unknown, op_cf
	.word op_d0_USE32, op_d1_67, op_d2_USE32, op_d3_67, op_d4, .unknown, .unknown, op_d7_USE32
#if USE_FPU_OPCODES
	.word op_d8_USE32, op_d9_USE32, op_da_USE32, op_db_USE32, op_dc_USE32, op_dd_USE32, op_de_USE32, op_df_USE32
#else
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
#endif
	.word op_e0_USE32, op_e1_USE32, op_e2_USE32, op_e3_USE32, op_e4_in_al_imm8, op_e5_in_ax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_ax
	.word .unknown, .unknown, .unknown, .unknown, op_ec_in_al_dx, op_ed_in_ax_dx, op_ee_out_dx_al, op_ef_out_dx_ax
	.word .unknown, .unknown, op_f2_67, op_f3_67, op_f4, op_f5, op_f6_USE32, op_f7_67
	.word op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe_USE32, op_ff_67


#if 0

op_67_67:
	lw		t1, GP_CODE_BIG(gp)			// Are we in a USE32 segment?
	get_cseip_byte r0					// Get the opcode byte into r0 register
	beqz	t1, 1b						// USE16 segment, so use normal op_67
	lw		t1, GP_OP_66_67(gp)			// USE32 segment, so op_67_67 is actually op_66_67 (which is USE16 opcode)
	sll		r0, 2						// r0 = 4*opcode
	addu	t1, r0
	lw		t1, 0(t1)					// t1 = jump address (from the 67-prefix table)
	jr		t1							// Jump to the opcode handler

#endif

// ------------------- 69 = IMUL r16,r/m16,imm16 ----------------------
op_69_67:
	modrm_jump_32
	modrm_tbl_3 imul_imm16
	.word imul_ax_ax_imm16, imul_ax_cx_imm16, imul_ax_dx_imm16, imul_ax_bx_imm16, imul_ax_sp_imm16, imul_ax_bp_imm16, imul_ax_si_imm16, imul_ax_di_imm16
	.word imul_cx_ax_imm16, imul_cx_cx_imm16, imul_cx_dx_imm16, imul_cx_bx_imm16, imul_cx_sp_imm16, imul_cx_bp_imm16, imul_cx_si_imm16, imul_cx_di_imm16
	.word imul_dx_ax_imm16, imul_dx_cx_imm16, imul_dx_dx_imm16, imul_dx_bx_imm16, imul_dx_sp_imm16, imul_dx_bp_imm16, imul_dx_si_imm16, imul_dx_di_imm16
	.word imul_bx_ax_imm16, imul_bx_cx_imm16, imul_bx_dx_imm16, imul_bx_bx_imm16, imul_bx_sp_imm16, imul_bx_bp_imm16, imul_bx_si_imm16, imul_bx_di_imm16
	.word imul_sp_ax_imm16, imul_sp_cx_imm16, imul_sp_dx_imm16, imul_sp_bx_imm16, imul_sp_sp_imm16, imul_sp_bp_imm16, imul_sp_si_imm16, imul_sp_di_imm16
	.word imul_bp_ax_imm16, imul_bp_cx_imm16, imul_bp_dx_imm16, imul_bp_bx_imm16, imul_bp_sp_imm16, imul_bp_bp_imm16, imul_bp_si_imm16, imul_bp_di_imm16
	.word imul_si_ax_imm16, imul_si_cx_imm16, imul_si_dx_imm16, imul_si_bx_imm16, imul_si_sp_imm16, imul_si_bp_imm16, imul_si_si_imm16, imul_si_di_imm16
	.word imul_di_ax_imm16, imul_di_cx_imm16, imul_di_dx_imm16, imul_di_bx_imm16, imul_di_sp_imm16, imul_di_bp_imm16, imul_di_si_imm16, imul_di_di_imm16

	modrm_3_genall imul_imm16 imul_imm16_reg16_r0

// ------------------- 6B = IMUL r16,r/m16,imm8 -----------------------
	.global	op_6b_67
op_6b_67:
	modrm_jump_32
	modrm_tbl_3 imul_imm8
	.word imul_ax_ax_imm8, imul_ax_cx_imm8, imul_ax_dx_imm8, imul_ax_bx_imm8, imul_ax_sp_imm8, imul_ax_bp_imm8, imul_ax_si_imm8, imul_ax_di_imm8
	.word imul_cx_ax_imm8, imul_cx_cx_imm8, imul_cx_dx_imm8, imul_cx_bx_imm8, imul_cx_sp_imm8, imul_cx_bp_imm8, imul_cx_si_imm8, imul_cx_di_imm8
	.word imul_dx_ax_imm8, imul_dx_cx_imm8, imul_dx_dx_imm8, imul_dx_bx_imm8, imul_dx_sp_imm8, imul_dx_bp_imm8, imul_dx_si_imm8, imul_dx_di_imm8
	.word imul_bx_ax_imm8, imul_bx_cx_imm8, imul_bx_dx_imm8, imul_bx_bx_imm8, imul_bx_sp_imm8, imul_bx_bp_imm8, imul_bx_si_imm8, imul_bx_di_imm8
	.word imul_sp_ax_imm8, imul_sp_cx_imm8, imul_sp_dx_imm8, imul_sp_bx_imm8, imul_sp_sp_imm8, imul_sp_bp_imm8, imul_sp_si_imm8, imul_sp_di_imm8
	.word imul_bp_ax_imm8, imul_bp_cx_imm8, imul_bp_dx_imm8, imul_bp_bx_imm8, imul_bp_sp_imm8, imul_bp_bp_imm8, imul_bp_si_imm8, imul_bp_di_imm8
	.word imul_si_ax_imm8, imul_si_cx_imm8, imul_si_dx_imm8, imul_si_bx_imm8, imul_si_sp_imm8, imul_si_bp_imm8, imul_si_si_imm8, imul_si_di_imm8
	.word imul_di_ax_imm8, imul_di_cx_imm8, imul_di_dx_imm8, imul_di_bx_imm8, imul_di_sp_imm8, imul_di_bp_imm8, imul_di_si_imm8, imul_di_di_imm8

	modrm_3_genall imul_imm8 imul_imm8_reg16_r0
	
// ------------------- 81 = ??? r/m16,imm16 ----------------------------
op_81_67:
	modrm_jump_32
	modrm_tbl_oper add or adc sbb and sub xor cmp imm16
	.word add_ax_imm16, add_cx_imm16, add_dx_imm16, add_bx_imm16, add_sp_imm16, add_bp_imm16, add_si_imm16, add_di_imm16
	.word or_ax_imm16, or_cx_imm16, or_dx_imm16, or_bx_imm16, or_sp_imm16, or_bp_imm16, or_si_imm16, or_di_imm16
	.word adc_ax_imm16, adc_cx_imm16, adc_dx_imm16, adc_bx_imm16, adc_sp_imm16, adc_bp_imm16, adc_si_imm16, adc_di_imm16
	.word sbb_ax_imm16, sbb_cx_imm16, sbb_dx_imm16, sbb_bx_imm16, sbb_sp_imm16, sbb_bp_imm16, sbb_si_imm16, sbb_di_imm16
	.word and_ax_imm16, and_cx_imm16, and_dx_imm16, and_bx_imm16, and_sp_imm16, and_bp_imm16, and_si_imm16, and_di_imm16
	.word sub_ax_imm16, sub_cx_imm16, sub_dx_imm16, sub_bx_imm16, sub_sp_imm16, sub_bp_imm16, sub_si_imm16, sub_di_imm16
	.word xor_ax_imm16, xor_cx_imm16, xor_dx_imm16, xor_bx_imm16, xor_sp_imm16, xor_bp_imm16, xor_si_imm16, xor_di_imm16
	.word cmp_ax_imm16, cmp_cx_imm16, cmp_dx_imm16, cmp_bx_imm16, cmp_sp_imm16, cmp_bp_imm16, cmp_si_imm16, cmp_di_imm16

	modrm_genall_oper add or adc sbb and sub xor cmp "r0" imm16

// ------------------- 83 = ??? r/m16,+imm8 ----------------------------
op_83_67:
	modrm_jump_32
	modrm_tbl_oper add or adc sbb and sub xor cmp simm8
// 0xc0 = mod = 11b => two register operands
	.word add_ax_simm8, add_cx_simm8, add_dx_simm8, add_bx_simm8, add_sp_simm8, add_bp_simm8, add_si_simm8, add_di_simm8
	.word or_ax_simm8, or_cx_simm8, or_dx_simm8, or_bx_simm8, or_sp_simm8, or_bp_simm8, or_si_simm8, or_di_simm8
	.word adc_ax_simm8, adc_cx_simm8, adc_dx_simm8, adc_bx_simm8, adc_sp_simm8, adc_bp_simm8, adc_si_simm8, adc_di_simm8
	.word sbb_ax_simm8, sbb_cx_simm8, sbb_dx_simm8, sbb_bx_simm8, sbb_sp_simm8, sbb_bp_simm8, sbb_si_simm8, sbb_di_simm8
	.word and_ax_simm8, and_cx_simm8, and_dx_simm8, and_bx_simm8, and_sp_simm8, and_bp_simm8, and_si_simm8, and_di_simm8
	.word sub_ax_simm8, sub_cx_simm8, sub_dx_simm8, sub_bx_simm8, sub_sp_simm8, sub_bp_simm8, sub_si_simm8, sub_di_simm8
	.word xor_ax_simm8, xor_cx_simm8, xor_dx_simm8, xor_bx_simm8, xor_sp_simm8, xor_bp_simm8, xor_si_simm8, xor_di_simm8
	.word cmp_ax_simm8, cmp_cx_simm8, cmp_dx_simm8, cmp_bx_simm8, cmp_sp_simm8, cmp_bp_simm8, cmp_si_simm8, cmp_di_simm8

	modrm_genall_oper add or adc sbb and sub xor cmp "r0" simm8

// ------------------- 85 = TEST r16,r/m16 -----------------------------
op_85_67:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_32
	modrm_tbl_1 test
// 0xC0 = two register operands
	.word test_ax_ax, test_cx_ax, test_dx_ax, test_bx_ax, test_sp_ax, test_bp_ax, test_si_ax, test_di_ax
	.word test_ax_cx, test_cx_cx, test_dx_cx, test_bx_cx, test_sp_cx, test_bp_cx, test_si_cx, test_di_cx
	.word test_ax_dx, test_cx_dx, test_dx_dx, test_bx_dx, test_sp_dx, test_bp_dx, test_si_dx, test_di_dx
	.word test_ax_bx, test_cx_bx, test_dx_bx, test_bx_bx, test_sp_bx, test_bp_bx, test_si_bx, test_di_bx
	.word test_ax_sp, test_cx_sp, test_dx_sp, test_bx_sp, test_sp_sp, test_bp_sp, test_si_sp, test_di_sp
	.word test_ax_bp, test_cx_bp, test_dx_bp, test_bx_bp, test_sp_bp, test_bp_bp, test_si_bp, test_di_bp
	.word test_ax_si, test_cx_si, test_dx_si, test_bx_si, test_sp_si, test_bp_si, test_si_si, test_di_si
	.word test_ax_di, test_cx_di, test_dx_di, test_bx_di, test_sp_di, test_bp_di, test_si_di, test_di_di

	modrm_1_genall test test_r0_r16

// ------------------- 87 = XCHG r/m16,r16 ----------------------------
op_87_67:
	modrm_jump_32
	modrm_tbl_1 xchg
// 0xC0
	.word loop, xchg_cx_ax, xchg_dx_ax, xchg_bx_ax, xchg_sp_ax, xchg_bp_ax, xchg_si_ax, xchg_di_ax
	.word xchg_ax_cx, loop, xchg_dx_cx, xchg_bx_cx, xchg_sp_cx, xchg_bp_cx, xchg_si_cx, xchg_di_cx
	.word xchg_ax_dx, xchg_cx_dx, loop, xchg_bx_dx, xchg_sp_dx, xchg_bp_dx, xchg_si_dx, xchg_di_dx
	.word xchg_ax_bx, xchg_cx_bx, xchg_dx_bx, loop, xchg_sp_bx, xchg_bp_bx, xchg_si_bx, xchg_di_bx
	.word xchg_ax_sp, xchg_cx_sp, xchg_dx_sp, xchg_bx_sp, loop, xchg_bp_sp, xchg_si_sp, xchg_di_sp
	.word xchg_ax_bp, xchg_cx_bp, xchg_dx_bp, xchg_bx_bp, xchg_sp_bp, loop, xchg_si_bp, xchg_di_bp
	.word xchg_ax_si, xchg_cx_si, xchg_dx_si, xchg_bx_si, xchg_sp_si, xchg_bp_si, loop, xchg_di_si
	.word xchg_ax_di, xchg_cx_di, xchg_dx_di, xchg_bx_di, xchg_sp_di, xchg_bp_di, xchg_si_di, loop

	modrm_1_genall xchg xchg_r0_r16

// ------------------- 88 = MOV r/m8,r8 -------------------------------
// 6788254C060000 = mov [0000064C],ah (Zone66)
// See op_88_USE32 at "cpu_386.S"

// ------------------- 89 = MOV r/m16,r16 ------------------------------
// 2667897B66 = mov es:[ebx+66],di
//
	.global	op_89_67
op_89_67:
	modrm_jump_32
	modrm_tbl_1 mov	
// 0xC0
	.word loop, mov_cx_ax, mov_dx_ax, mov_bx_ax, mov_sp_ax, mov_bp_ax, mov_si_ax, mov_di_ax
	.word mov_ax_cx, loop, mov_dx_cx, mov_bx_cx, mov_sp_cx, mov_bp_cx, mov_si_cx, mov_di_cx
	.word mov_ax_dx, mov_cx_dx, loop, mov_bx_dx, mov_sp_dx, mov_bp_dx, mov_si_dx, mov_di_dx
	.word mov_ax_bx, mov_cx_bx, mov_dx_bx, loop, mov_sp_bx, mov_bp_bx, mov_si_bx, mov_di_bx
	.word mov_ax_sp, mov_cx_sp, mov_dx_sp, mov_bx_sp, loop, mov_bp_sp, mov_si_sp, mov_di_sp
	.word mov_ax_bp, mov_cx_bp, mov_dx_bp, mov_bx_bp, mov_sp_bp, loop, mov_si_bp, mov_di_bp
	.word mov_ax_si, mov_cx_si, mov_dx_si, mov_bx_si, mov_sp_si, mov_bp_si, loop, mov_di_si
	.word mov_ax_di, mov_cx_di, mov_dx_di, mov_bx_di, mov_sp_di, mov_bp_di, mov_si_di, loop

	modrm_1_genall mov mov_r0_r16

	.global	op_a3_67
op_a3_67:
	mvn		r3, #0				// Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_r0_r16_r4

// ------------------- 8A = MOV r8, r/m8 ------------------------------
// 678A4514 = mov al,[ebp+0014] (DOS4GW)
// See op_8a_USE32 at "cpu_386.S"

// ------------------- 8B = MOV r16,r/m16 -------------------------------
op_8b_67:
	modrm_jump_32
	modrm_tbl_3 mov	
// 0xC0 = two register operands
	.word loop, mov_ax_cx, mov_ax_dx, mov_ax_bx, mov_ax_sp, mov_ax_bp, mov_ax_si, mov_ax_di
	.word mov_cx_ax, loop, mov_cx_dx, mov_cx_bx, mov_cx_sp, mov_cx_bp, mov_cx_si, mov_cx_di
	.word mov_dx_ax, mov_dx_cx, loop, mov_dx_bx, mov_dx_sp, mov_dx_bp, mov_dx_si, mov_dx_di
	.word mov_bx_ax, mov_bx_cx, mov_bx_dx, loop, mov_bx_sp, mov_bx_bp, mov_bx_si, mov_bx_di
	.word mov_sp_ax, mov_sp_cx, mov_sp_dx, mov_sp_bx, loop, mov_sp_bp, mov_sp_si, mov_sp_di
	.word mov_bp_ax, mov_bp_cx, mov_bp_dx, mov_bp_bx, mov_bp_sp, loop, mov_bp_si, mov_bp_di
	.word mov_si_ax, mov_si_cx, mov_si_dx, mov_si_bx, mov_si_sp, mov_si_bp, loop, mov_si_di
	.word mov_di_ax, mov_di_cx, mov_di_dx, mov_di_bx, mov_di_sp, mov_di_bp, mov_di_si, loop

	modrm_3_genall mov mov_r16_r0

	.global	op_a1_67
op_a1_67:
	mvn		r3, #0				// Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_r16_r0_r4
	
// ------------------- 8C = MOV r/m16,Sreg -----------------------------
op_8c_67:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0				// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0x0
	modrm_help_1_0 mov es es
	modrm_help_1_0 mov cs cs
	modrm_help_1_0 mov ss ss
	modrm_help_1_0 mov ds ds
	modrm_help_1_0 mov fs fs
	modrm_help_1_0 mov gs gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 mov es es
	modrm_help_1_40 mov cs cs
	modrm_help_1_40 mov ss ss
	modrm_help_1_40 mov ds ds
	modrm_help_1_40 mov fs fs
	modrm_help_1_40 mov gs gs
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
// 0xC0
	.word mov_ax_es, mov_cx_es, mov_dx_es, mov_bx_es, mov_sp_es, mov_bp_es, mov_si_es, mov_di_es
	.word mov_ax_cs, mov_cx_cs, mov_dx_cs, mov_bx_cs, mov_sp_cs, mov_bp_cs, mov_si_cs, mov_di_cs
	.word mov_ax_ss, mov_cx_ss, mov_dx_ss, mov_bx_ss, mov_sp_ss, mov_bp_ss, mov_si_ss, mov_di_ss
	.word mov_ax_ds, mov_cx_ds, mov_dx_ds, mov_bx_ds, mov_sp_ds, mov_bp_ds, mov_si_ds, mov_di_ds
	.word mov_ax_fs, mov_cx_fs, mov_dx_fs, mov_bx_fs, mov_sp_fs, mov_bp_fs, mov_si_fs, mov_di_fs
	.word mov_ax_gs, mov_cx_gs, mov_dx_gs, mov_bx_gs, mov_sp_gs, mov_bp_gs, mov_si_gs, mov_di_gs
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	// The memory modrms use handlers in "cpu_386.s", the register modrms use handlers in "cpu.s"
	
// ------------------- 8D = LEA r16,m ----------------------------------
// 678D1500000000 = lea  dx,[00000000] (RACE, REAL, USE16)
//
	.global	op_8d_67
op_8d_67:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0				// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	modrm_help_3_0 lea eax eax
	modrm_help_3_0 lea ecx ecx
	modrm_help_3_0 lea edx edx
	modrm_help_3_0 lea ebx ebx
	modrm_help_3_0 lea esp esp
	modrm_help_3_0 lea ebp ebp
	modrm_help_3_0 lea esi esi
	modrm_help_3_0 lea edi edi
// 0x40
	modrm_help_3_40 lea eax eax
	modrm_help_3_40 lea ecx ecx
	modrm_help_3_40 lea edx edx
	modrm_help_3_40 lea ebx ebx
	modrm_help_3_40 lea esp esp
	modrm_help_3_40 lea ebp ebp
	modrm_help_3_40 lea esi esi
	modrm_help_3_40 lea edi edi
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
	// Need to have the same code 4 bytes before the actual handler,
	// as the SIB code returns to 4 bytes before the handler when using
	// ESP or EBP as the base register!
	//------ 
	nop
lea_SIB_return_\reg:
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
// ----- 0x00 -----
lea_\reg\()_eax:
	mov		r0, eax, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
lea_\reg\()_ecx:
	mov		r0, ecx, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
lea_\reg\()_edx:
	mov		r0, edx, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
lea_\reg\()_bxidx:
	mov		r0, ebx, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
lea_\reg\()_SIB:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_disp32:
	get_cseip_dword r0
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_siidx:
	mov		r0, esi, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
lea_\reg\()_diidx:
	mov		r0, edi, lsl #16
	orr		\reg, r0, \reg, lsr #16
	ror		\reg, #16
	b		loop
// ----- 0x40 = disp8 -----
lea_\reg\()_eaxd8:
	ldrsb	r0, [r12], #1
	add		r0, eax
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_ecxd8:
	ldrsb	r0, [r12], #1
	add		r0, ecx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_edxd8:
	ldrsb	r0, [r12], #1
	add		r0, edx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_bxdisp8:
	ldrsb	r0, [r12], #1
	add		r0, ebx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_SIBd8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_bpdisp8:
	ldrsb	r0, [r12], #1
	add		r0, ebp
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_sidisp8:
	ldrsb	r0, [r12], #1
	add		r0, esi
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_didisp8:
	ldrsb	r0, [r12], #1
	add		r0, edi
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
// ----- 0x80 = disp32 -----
lea_\reg\()_eaxd32:
	get_cseip_dword r0
	add		r0, eax
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_ecxd32:
	get_cseip_dword r0
	add		r0, ecx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_edxd32:
	get_cseip_dword r0
	add		r0, edx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_ebxd32:
	get_cseip_dword r0
	add		r0, ebx
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_SIBd32:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_\reg			// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
lea_\reg\()_ebpd32:
	get_cseip_dword r0
	add		r0, ebp
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_esid32:
	get_cseip_dword r0
	add		r0, esi
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
lea_\reg\()_edid32:
	get_cseip_dword r0
	add		r0, edi
	lsr		\reg ,#16
	orr		\reg, r0, lsl #16
	ror		\reg, #16
	b		loop
.endm

	lea_help eax
	lea_help ecx
	lea_help edx
	lea_help ebx

	.ltorg
	
	lea_help esp
	lea_help ebp
	lea_help esi
	lea_help edi

	.ltorg
	

// ------------------- 8F = POP m16 ------------------------------------
op_8f_67:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0				// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	modrm_help_1_0 pop
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 pop
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 pop
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0
	.word op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_1_help pop pop_r0

	.ltorg
	
// ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,imm8 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_c1_67:
	modrm_jump_32
	modrm_tbl_oper rol ror rcl rcr shl shr shl sar 16_imm8
	.word rol_ax_imm8, rol_cx_imm8, rol_dx_imm8, rol_bx_imm8, rol_sp_imm8, rol_bp_imm8, rol_si_imm8, rol_di_imm8
	.word ror_ax_imm8, ror_cx_imm8, ror_dx_imm8, ror_bx_imm8, ror_sp_imm8, ror_bp_imm8, ror_si_imm8, ror_di_imm8
	.word rcl_ax_imm8, rcl_cx_imm8, rcl_dx_imm8, rcl_bx_imm8, rcl_sp_imm8, rcl_bp_imm8, rcl_si_imm8, rcl_di_imm8
	.word rcr_ax_imm8, rcr_cx_imm8, rcr_dx_imm8, rcr_bx_imm8, rcr_sp_imm8, rcr_bp_imm8, rcr_si_imm8, rcr_di_imm8
	.word shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	.word shr_ax_imm8, shr_cx_imm8, shr_dx_imm8, shr_bx_imm8, shr_sp_imm8, shr_bp_imm8, shr_si_imm8, shr_di_imm8
	.word shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	.word sar_ax_imm8, sar_cx_imm8, sar_dx_imm8, sar_bx_imm8, sar_sp_imm8, sar_bp_imm8, sar_si_imm8, sar_di_imm8

	modrm_genall_oper rol ror rcl rcr shl shr "skip" sar "r0" 16_imm8

// ------------------- C4 = LES r16,m16:16 -----------------------------
//
	.global	op_c4_67
op_c4_67:
	modrm_jump_32
	modrm_tbl_3 les
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_3_genall les les_r16_r0					// Call handlers in cpu.S

// ------------------- C5 = LDS r16,m16:16 -----------------------------
//
	.global	op_c5_67
op_c5_67:
	modrm_jump_32
	modrm_tbl_3 lds
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_3_genall lds lds_r16_r0					// Call handlers in cpu.S

// ------------------- C7 = MOV r/m16, imm16 -----------------------------
// 2667C743640000 = mov word es:[ebx+64],0000 
//
op_c7_67:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	mvn		r3, #0				// Use 32-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	modrm_help_1_0 mov imm16 imm16
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	modrm_help_1_40 mov imm16 imm16
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	modrm_help_1_80 mov imm16
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0xC0
	.word op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_1_help mov mov_r0 imm16

	.ltorg
	
// ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,1 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
	.global	op_d1_67
op_d1_67:
	modrm_jump_32
	modrm_tbl_oper rol ror rcl rcr shl shr shl sar 16_1
	.word rol_reg16_1_r4, rol_reg16_1_r5, rol_reg16_1_r6, rol_reg16_1_r7, rol_reg16_1_r8, rol_reg16_1_r9, rol_reg16_1_r10, rol_reg16_1_r11
	.word ror_reg16_1_r4, ror_reg16_1_r5, ror_reg16_1_r6, ror_reg16_1_r7, ror_reg16_1_r8, ror_reg16_1_r9, ror_reg16_1_r10, ror_reg16_1_r11
	.word rcl_reg16_1_r4, rcl_reg16_1_r5, rcl_reg16_1_r6, rcl_reg16_1_r7, rcl_reg16_1_r8, rcl_reg16_1_r9, rcl_reg16_1_r10, rcl_reg16_1_r11
	.word rcr_reg16_1_r4, rcr_reg16_1_r5, rcr_reg16_1_r6, rcr_reg16_1_r7, rcr_reg16_1_r8, rcr_reg16_1_r9, rcr_reg16_1_r10, rcr_reg16_1_r11
	.word shl_reg16_1_r4, shl_reg16_1_r5, shl_reg16_1_r6, shl_reg16_1_r7, shl_reg16_1_r8, shl_reg16_1_r9, shl_reg16_1_r10, shl_reg16_1_r11
	.word shr_reg16_1_r4, shr_reg16_1_r5, shr_reg16_1_r6, shr_reg16_1_r7, shr_reg16_1_r8, shr_reg16_1_r9, shr_reg16_1_r10, shr_reg16_1_r11
	.word shl_reg16_1_r4, shl_reg16_1_r5, shl_reg16_1_r6, shl_reg16_1_r7, shl_reg16_1_r8, shl_reg16_1_r9, shl_reg16_1_r10, shl_reg16_1_r11
	.word sar_reg16_1_r4, sar_reg16_1_r5, sar_reg16_1_r6, sar_reg16_1_r7, sar_reg16_1_r8, sar_reg16_1_r9, sar_reg16_1_r10, sar_reg16_1_r11

	modrm_genall_oper rol ror rcl rcr shl shr "skip" sar "r0" 16_1

// ------------------- D3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,CL ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
	.global	op_d3_67
op_d3_67:
	modrm_jump_32
	modrm_tbl_oper rol ror rcl rcr shl shr shl sar 16_CL
	.word rol_ax_CL, rol_cx_CL, rol_dx_CL, rol_bx_CL, rol_sp_CL, rol_bp_CL, rol_si_CL, rol_di_CL
	.word ror_ax_CL, ror_cx_CL, ror_dx_CL, ror_bx_CL, ror_sp_CL, ror_bp_CL, ror_si_CL, ror_di_CL
	.word rcl_ax_CL, rcl_cx_CL, rcl_dx_CL, rcl_bx_CL, rcl_sp_CL, rcl_bp_CL, rcl_si_CL, rcl_di_CL
	.word rcr_ax_CL, rcr_cx_CL, rcr_dx_CL, rcr_bx_CL, rcr_sp_CL, rcr_bp_CL, rcr_si_CL, rcr_di_CL
	.word shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	.word shr_ax_CL, shr_cx_CL, shr_dx_CL, shr_bx_CL, shr_sp_CL, shr_bp_CL, shr_si_CL, shr_di_CL
	.word shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	.word sar_ax_CL, sar_cx_CL, sar_dx_CL, sar_bx_CL, sar_sp_CL, sar_bp_CL, sar_si_CL, sar_di_CL

	modrm_genall_oper rol ror rcl rcr shl shr "skip" sar "r0" 16_CL

// ------------------- F7 = ??? r/m16 ----------------------------------
op_f7_67:
	modrm_jump_32
	modrm_tbl_oper test back2 not neg mul imul div idiv w
//0xc0 = mod = 11b => register operand
// TEST r16,imm16
	.word test_ax_imm16, test_cx_imm16, test_dx_imm16, test_bx_imm16, test_sp_imm16, test_bp_imm16, test_si_imm16, test_di_imm16
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// NOT r16
	.word not_ax, not_cx, not_dx, not_bx, not_sp, not_bp, not_si, not_di
// NEG r16
	.word neg_ax, neg_cx, neg_dx, neg_bx, neg_sp, neg_bp, neg_si, neg_di
// MUL r16
	.word mul_ax, mul_cx, mul_dx, mul_bx, mul_sp, mul_bp, mul_si, mul_di
// IMUL r16
	.word imul_ax, imul_cx, imul_dx, imul_bx, imul_sp, imul_bp, imul_si, imul_di
// DIV r16
	.word div_ax, div_cx, div_dx, div_bx, div_sp, div_bp, div_si, div_di
// IDIV r16
	.word idiv_ax, idiv_cx, idiv_dx, idiv_bx, idiv_sp, idiv_bp, idiv_si, idiv_di

	modrm_genall_oper test "skip" not neg mul imul div idiv "r0" w

// ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
op_ff_67:
	modrm_jump_32
	modrm_tbl_oper inc_word dec_word call_near call_far jmp_near back2 push_word back2
	// 0xc0 = mod = 11b => register operand
	.word op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47		// INC AX ... INC DI
	.word op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f		// DEC AX ... DEC DI
	.word call_near_ax, call_near_cx, call_near_dx, call_near_bx, call_near_sp, call_near_bp, call_near_si, call_near_di
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word jmp_near_ax, jmp_near_cx, jmp_near_dx, jmp_near_bx, jmp_near_sp, jmp_near_bp, jmp_near_si, jmp_near_di
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57		// PUSH AX ... PUSH DI
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_oper inc_word dec_word call_near call_far jmp_near skip push_word skip "r0"

	//---------------------------------------------
	// Real Mode, Operand Size = 16bit, Address Size = 32bit
	//---------------------------------------------
// 0x00 (prefix 67)
	.global op_00_67, op_01_67, op_03_67
	.global op_09_67, op_0b_67, op_0f_67
	.global op_23_67, op_26_67_r 
	.global op_29_67, op_2b_67
	.global op_31_67, op_36_67_r
	.global op_39_67, op_3b_67
// 0x40 (prefix 67)
	.global op_65_67_r, op_66_67, op_67
	.global op_69_67,
// 0x80 (prefix 67)
	.global op_81_67, op_83_67, op_85_67, 
	.global op_89_67, op_8b_67, op_8c_67, op_8f_67
	.global op_a1_67, op_a3_67
// 0xC0 (prefix 67)
	.global op_c1_67, op_c7_67
	.global op_f6_67, op_f7_67
	.global op_ff_67
	//---------------------------------------------
	// Protected Mode, Operand Size = 16bit, Address Size = 32bit
	//---------------------------------------------
// 0x00 (prefix 67)
	.global op_23_67, op_26_67_p 
	.global op_31_67, op_36_67_p 
// 0x40 (prefix 67)
	.global op_65_67_p, op_66_67, op_67_67
	.global op_69_67
// 0x80 (prefix 67)
	.global op_81_67, op_83_67, op_85_67, op_87_67
	.global op_89_67, op_8b_67, op_8f_67
// 0xC0 (prefix 67)

