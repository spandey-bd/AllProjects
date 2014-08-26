//=============================================================================
// cpu_66.s
//
// This file contains normal opcode handlers for when running in a USE16 segment
// and a prefix 0x66 (32-bit operands) is active. This is similar to a situation
// when running in a USE32 segment with a prefix 0x67 (16-bit addressing) active.
// In principle this file only contains the memory address parsing for word-size
// opcodes (since 8-bit opcodes are similar to non-prefixed versions in "cpu.s")
// with the actual handlers being in "cpu_386.s".
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
	.file	"cpu_66.s"

#include "defines.inc"
#include "macros.inc"
#include "gen_66.inc"

	.text
	.align	2

op_c2_USE32:
op_c3_USE32:
op_f4:
	b		.unknown
	
// ------------------- 01 = ADD r/m32, r32 -----------------------------
// Use 16-bit memory address with 32-bit values
// 
	.global	op_01_66
op_01_66:
	modrm_jump_16
	modrm_tbl_1 add
	modrm_1_genall add add_t0_r32				// Call handlers in cpu_386.S

// ------------------- 03 = ADD r32, r/m32 ----------------------------
// Use 16-bit memory address with 32-bit values
//
	.global	op_03_66
op_03_66:
	modrm_jump_16
	modrm_tbl_3 add
	modrm_3_genall add add_r32_t0				// Call handlers in cpu_386.S

// ------------------- 09 = OR r/m32, r32 -----------------------------
//
	.global	op_09_66
op_09_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_1 or
	modrm_1_genall or or_t0_r32					// Call handlers in cpu_386.S

// ------------------- 0B = OR r32, r/m32 -----------------------------
//
op_0b_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_3 or
	modrm_3_genall or or_r32_t0					// Call handlers in cpu_386.S

// ------------------- 0F = various opcodes ----------------------------
// op_0f = see "cpu_0F.S"

// ------------------- 11 = ADC r/m32, r32 -----------------------------
//
	.global	op_11_66
op_11_66:
	modrm_jump_16
	modrm_tbl_1 adc
	modrm_1_genall adc adc_t0_r32				// Call handlers in cpu_386.S

// ------------------- 13 = ADC r32, r/m32 -----------------------------
//
	.global	op_13_66
op_13_66:
	modrm_jump_16
	modrm_tbl_3 adc
	modrm_3_genall adc adc_r32_t0				// Call handlers in cpu_386.S

// ------------------- 19 = SBB r/m32, r32 -----------------------------
//
	.global	op_19_66
op_19_66:
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_16
	modrm_tbl_1 sbb
	modrm_1_genall sbb sbb_t0_r32				// Call handlers in cpu_386.S

// ------------------- 1B = SBB r32, r/m32 ------------------------------
//
	.global	op_1b_66
op_1b_66:
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					// Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_16
	modrm_tbl_3 sbb
	modrm_3_genall sbb sbb_r32_t0				// Call handlers in cpu_386.S

// ------------------- 21 = AND r/m32, r32 -----------------------------
//
	.global	op_21_66
op_21_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_1 and
	modrm_1_genall and and_t0_r32				// Call handlers in cpu_386.S

// ------------------- 23 = AND r32, r/m32 ----------------------------
//
	.global	op_23_66
op_23_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_3 and
	modrm_3_genall and and_r32_t0				// Call handlers in cpu_386.S

// ------------------- 29 = SUB r/m32, r32 ------------------------------
//
	.global	op_29_66
op_29_66:
	modrm_jump_16
	modrm_tbl_1 sub
	modrm_1_genall sub sub_t0_r32				// Call handlers in cpu_386.S

// ------------------- 2B = SUB r32, r/m32 ------------------------------
//
	.global	op_2b_66
op_2b_66:
	modrm_jump_16
	modrm_tbl_3 sub
	modrm_3_genall sub sub_r32_t0				// Call handlers in cpu_386.S

// ------------------- 31 = XOR r/m32, r32 ----------------------------
//
	.global	op_31_66
op_31_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_1 xor
	modrm_1_genall xor xor_t0_r32				// Call handlers in cpu_386.S

// ------------------- 33 = XOR r32, r/m32 -----------------------------
//
	.global	op_33_66
op_33_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_3 xor
	modrm_3_genall xor xor_r32_t0		// Call handlers in cpu_386.S

// ------------------- 39 = CMP r/m32, r32 -----------------------------
//
op_39_66:
	modrm_jump_16
	modrm_tbl_1 cmp
	modrm_1_genall cmp cmp_t0_r32				// Call handlers in cpu_386.S

// ------------------- 3B = CMP r32, r/m32 ----------------------------
//
	.global	op_3b_66
op_3b_66:
	modrm_jump_16
	modrm_tbl_3 cmp
	modrm_3_genall cmp cmp_r32_t0					// Call handlers in cpu_386.S


// ------------------- Segment Overrides using "opcodetable_32_16" -----------------------

op_26_66:
	ldr		r2, [sp, #SP_ES_BASE]				// r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+5*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_2e_66:
	ldr		r2, [sp, #SP_CS_BASE]				// r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+4*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_36_66:
	ldr		r2, [sp, #SP_SS_BASE]				// r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+3*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_3e_66:
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				// r2 = logical DS segment in high halfword
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+2*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_64_66:
	ldr		r2, [sp, #SP_FS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+1*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler

op_65_66:
	ldr		r2, [sp, #SP_GS_BASE]				// r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								// BP-relative segment base equals DS-relative segment base
	add		r1, pc, #(3+0*5)*4					// (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					// Jump to the handler


// ------------------- 66 = Operand-size Prefix ------------------------
	.global	op_66_USE16
	.global	op_67_USE32
op_66_USE16:
op_67_USE32:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
	//----------------------------------------------
	// Operand Size = 32bit, address size = 16bit
	//----------------------------------------------
opcodetable_32_16:
// 0x00
	.word op_00, op_01_66, op_02, op_03_66, op_04, op_05_USE32, op_06_USE32, op_07_USE32
	.word op_08, op_09_66, op_0a, op_0b_66, op_0c, op_0d_USE32, op_0e_USE32, op_0f_66
	.word op_10, op_11_66, op_12, op_13_66, op_14, op_15_USE32, op_16_USE32, op_17_USE32
	.word op_18, op_19_66, op_1a, op_1b_66, op_1c, op_1d_USE32, op_1e_USE32, op_1f_USE32
	.word op_20, op_21_66, op_22, op_23_66, op_24, op_25_USE32, op_26_66, op_27
	.word op_28, op_29_66, op_2a, op_2b_66, op_2c, op_2d_USE32, op_2e_66, op_2f
	.word op_30, op_31_66, op_32, op_33_66, op_34, op_35_USE32, op_36_66, op_37
	.word op_38, op_39_66, op_3a, op_3b_66, op_3c, op_3d_USE32, op_3e_66, op_3f
// 0x40
	.word op_40_USE32, op_41_USE32, op_42_USE32, op_43_USE32, op_44_USE32, op_45_USE32, op_46_USE32, op_47_USE32
	.word op_48_USE32, op_49_USE32, op_4a_USE32, op_4b_USE32, op_4c_USE32, op_4d_USE32, op_4e_USE32, op_4f_USE32
	.word op_50_USE32, op_51_USE32, op_52_USE32, op_53_USE32, op_54_USE32, op_55_USE32, op_56_USE32, op_57_USE32
	.word op_58_USE32, op_59_USE32, op_5a_USE32, op_5b_USE32, op_5c_USE32, op_5d_USE32, op_5e_USE32, op_5f_USE32
	.word op_60_USE32, op_61_USE32, .unknown, .unknown, op_64_66, op_65_66, op_66_67_USE32, op_66_67_USE16
	.word op_68_USE32, op_69_66, op_6a_USE32, op_6b_66, .unknown, .unknown, .unknown, .unknown
	.word op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	.word op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
// 0x80
	.word op_80, op_81_66, op_82, op_83_66, op_84, op_85_66, op_86, op_87_66
	.word op_88, op_89_66, op_8a, op_8b_66, op_8c_66, op_8d_66, op_8e, op_8f_66
	.word loop
	modrm_reg_reg_word xchg ecx eax
	modrm_reg_reg_word xchg edx eax
	modrm_reg_reg_word xchg ebx eax
	modrm_reg_reg_word xchg esp eax
	modrm_reg_reg_word xchg ebp eax
	modrm_reg_reg_word xchg esi eax
	modrm_reg_reg_word xchg edi eax
	.word op_98_USE32, op_99_USE32, .unknown, loop, op_9c_USE32, op_9d_USE32, op_9e, op_9f
	.word op_a0, op_a1_66, op_a2, op_a3_66, op_a4_movsb, op_a5_movsd, op_a6_cmpsb, op_a7_cmpsd
	.word .unknown, op_a9_USE32, op_aa_stosb, op_ab_stosd, op_ac_lodsb, op_ad_lodsd, op_ae_scasb, op_af_scasd
	.word op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	.word op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
// 0xC0
	.word op_c0, op_c1_66, op_c2_USE32, op_c3_USE32, op_c4_66, op_c5_66, op_c6, op_c7_66
	.word op_c8_USE32, op_c9_USE32, op_ca_USE32, op_cb_USE32, .unknown, .unknown, .unknown, op_cf_USE32
	.word op_d0, op_d1_66, op_d2, op_d3_66, op_d4, .unknown, .unknown, op_d7
#if USE_FPU_OPCODES
	.word op_d8, op_d9, op_da, op_db, op_dc, op_dd, op_de, op_df
#else
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
#endif
	.word op_e0, op_e1, op_e2, op_e3, op_e4_in_al_imm8, op_e5_in_eax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_eax
	.word .unknown, .unknown, op_ea_USE32, .unknown, op_ec_in_al_dx, op_ed_in_eax_dx, op_ee_out_dx_al, op_ef_out_dx_eax
	.word .unknown, .unknown, op_f2_66, op_f3_66, op_f4, op_f5, op_f6, op_f7_66
	.word op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe, op_ff_66


#if 0

op_66_66:
	lw		t1, GP_CODE_BIG(gp)			// Are we in a USE32 segment?
	get_cseip_byte t0					// Get the opcode byte into t0 register
	beqz	t1, 1b						// USE16 segment, so use normal op_66
	lw		t1, GP_OP_66_67(gp)			// USE32 segment, so op_66_66 is actually op_66_67 (which is USE16 opcode)
	sll		t0, 2						// t0 = 4*opcode
	addu	t1, t0
	lw		t1, 0(t1)					// t1 = jump address (from the 67-prefix table)
	jr		t1							// Jump to the opcode handler

#endif

// ------------------- 69 = IMUL r32,r/m32,imm32 ------------------------
	.global	op_69_66
op_69_66:
	modrm_jump_16
	modrm_tbl_3 imul_imm32
	modrm_3_genall imul_imm32 imul_imm32_r32_t0					// Call handlers in cpu_386.S

// ------------------- 6B = IMUL r32,r/m32,imm8 ------------------------
	.global	op_6b_66
op_6b_66:
	modrm_jump_16
	modrm_tbl_3 imul_imm8
	modrm_3_genall imul_imm8 imul_imm8_r32_t0					// Call handlers in cpu_386.S

// ------------------- 81 = ??? r/m32,imm32 ----------------------------
	.global	op_81_66
op_81_66:
	modrm_jump_16
	modrm_tbl_oper add or adc sbb and sub xor cmp imm32
	.word add_eax_imm32, add_ecx_imm32, add_edx_imm32, add_ebx_imm32, add_esp_imm32, add_ebp_imm32, add_esi_imm32, add_edi_imm32
	.word or_eax_imm32, or_ecx_imm32, or_edx_imm32, or_ebx_imm32, or_esp_imm32, or_ebp_imm32, or_esi_imm32, or_edi_imm32
	.word adc_eax_imm32, adc_ecx_imm32, adc_edx_imm32, adc_ebx_imm32, adc_esp_imm32, adc_ebp_imm32, adc_esi_imm32, adc_edi_imm32
	.word sbb_eax_imm32, sbb_ecx_imm32, sbb_edx_imm32, sbb_ebx_imm32, sbb_esp_imm32, sbb_ebp_imm32, sbb_esi_imm32, sbb_edi_imm32
	.word and_eax_imm32, and_ecx_imm32, and_edx_imm32, and_ebx_imm32, and_esp_imm32, and_ebp_imm32, and_esi_imm32, and_edi_imm32
	.word sub_eax_imm32, sub_ecx_imm32, sub_edx_imm32, sub_ebx_imm32, sub_esp_imm32, sub_ebp_imm32, sub_esi_imm32, sub_edi_imm32
	.word xor_eax_imm32, xor_ecx_imm32, xor_edx_imm32, xor_ebx_imm32, xor_esp_imm32, xor_ebp_imm32, xor_esi_imm32, xor_edi_imm32
	.word cmp_eax_imm32, cmp_ecx_imm32, cmp_edx_imm32, cmp_ebx_imm32, cmp_esp_imm32, cmp_ebp_imm32, cmp_esi_imm32, cmp_edi_imm32

	modrm_genall_oper add or adc sbb and sub xor cmp dw_t0 imm32
	

// ------------------- 83 = ??? r/m32,+imm8 ----------------------------
	.global	op_83_66
op_83_66:
	modrm_jump_16
	modrm_tbl_oper add or adc sbb and sub xor cmp simm8
	.word add_eax_simm8, add_ecx_simm8, add_edx_simm8, add_ebx_simm8, add_esp_simm8, add_ebp_simm8, add_esi_simm8, add_edi_simm8
	.word or_eax_simm8, or_ecx_simm8, or_edx_simm8, or_ebx_simm8, or_esp_simm8, or_ebp_simm8, or_esi_simm8, or_edi_simm8
	.word adc_eax_simm8, adc_ecx_simm8, adc_edx_simm8, adc_ebx_simm8, adc_esp_simm8, adc_ebp_simm8, adc_esi_simm8, adc_edi_simm8
	.word sbb_eax_simm8, sbb_ecx_simm8, sbb_edx_simm8, sbb_ebx_simm8, sbb_esp_simm8, sbb_ebp_simm8, sbb_esi_simm8, sbb_edi_simm8
	.word and_eax_simm8, and_ecx_simm8, and_edx_simm8, and_ebx_simm8, and_esp_simm8, and_ebp_simm8, and_esi_simm8, and_edi_simm8
	.word sub_eax_simm8, sub_ecx_simm8, sub_edx_simm8, sub_ebx_simm8, sub_esp_simm8, sub_ebp_simm8, sub_esi_simm8, sub_edi_simm8
	.word xor_eax_simm8, xor_ecx_simm8, xor_edx_simm8, xor_ebx_simm8, xor_esp_simm8, xor_ebp_simm8, xor_esi_simm8, xor_edi_simm8
	.word cmp_eax_simm8, cmp_ecx_simm8, cmp_edx_simm8, cmp_ebx_simm8, cmp_esp_simm8, cmp_ebp_simm8, cmp_esi_simm8, cmp_edi_simm8

	modrm_genall_oper add or adc sbb and sub xor cmp dw_t0 simm8

// ------------------- 85 = TEST r32,r/m32 -----------------------------
op_85_66:
	msr		cpsr_f,#0							// Clear all flags (especially C and O)
	modrm_jump_16
	modrm_tbl_1 test
	modrm_1_genall test test_t0_r32				// Call handlers in cpu_386.S

// ------------------- 87 = XCHG r/m32,r32 ----------------------------
op_87_66:
	modrm_jump_16
	modrm_tbl_1 xchg
	modrm_1_genall xchg xchg_t0_r32				// Call handlers in cpu_386.S

// ------------------- 89 = MOV r/m32,r32 ------------------------------
	.global	op_89_66
op_89_66:
	modrm_jump_16
	modrm_tbl_1 mov
	modrm_1_genall mov mov_t0_r32				// Call handlers in cpu_386.S

// ------------------- 8B = MOV r32,r/m32 -------------------------------
	.global	op_8b_66
op_8b_66:
	modrm_jump_16
	modrm_tbl_3 mov
	modrm_3_genall mov mov_r32_t0				// Call handlers in cpu_386.S

// ------------------- 8C = MOV r/m16,Sreg -----------------------------
//

op_8c_66:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0

.macro tmp sreg
	.word mov_bxsi_\sreg, mov_bxdi_\sreg, mov_bpsi_\sreg, mov_bpdi_\sreg, mov_siidx_\sreg, mov_diidx_\sreg, mov_disp16_\sreg, mov_bxidx_\sreg
.endm
// 0
	tmp es
	tmp cs
	tmp ss
	tmp ds
	tmp fs
	tmp gs
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
.purgem tmp
.macro tmp sreg
	.word mov_bxsid8_\sreg, mov_bxdid8_\sreg, mov_bpsid8_\sreg, mov_bpdid8_\sreg, mov_sidisp8_\sreg, mov_didisp8_\sreg, mov_bpdisp8_\sreg, mov_bxdisp8_\sreg
.endm
//0x40
	tmp es
	tmp cs
	tmp ss
	tmp ds
	tmp fs
	tmp gs
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
.purgem tmp
.macro tmp sreg
	.word mov_bxsid16_\sreg, mov_bxdid16_\sreg, mov_bpsid16_\sreg, mov_bpdid16_\sreg, mov_sidisp16_\sreg, mov_didisp16_\sreg, mov_bpdisp16_\sreg, mov_bxdisp16_\sreg
.endm
//0x80
	tmp es
	tmp cs
	tmp ss
	tmp ds
	tmp fs
	tmp gs
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
	.word .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1, .unknown_back1
.purgem tmp
// 0xC0 = mod = 11b = register (in "cpu_386.S")
	modrm_help_1_C0 mov es_r
	modrm_help_1_C0 mov cs_r
	modrm_help_1_C0 mov ss_r
	modrm_help_1_C0 mov ds_r
	modrm_help_1_C0 mov fs_r
	modrm_help_1_C0 mov gs_r
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
 
// ------------------- 8D = LEA r32,m ----------------------------------
op_8d_66:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
// 0
	.word lea_eax_bxsi, lea_eax_bxdi, lea_eax_bpsi, lea_eax_bpdi, lea_eax_si, lea_eax_di, lea_eax_disp16, lea_eax_bx
	.word lea_ecx_bxsi, lea_ecx_bxdi, lea_ecx_bpsi, lea_ecx_bpdi, lea_ecx_si, lea_ecx_di, lea_ecx_disp16, lea_ecx_bx
	.word lea_edx_bxsi, lea_edx_bxdi, lea_edx_bpsi, lea_edx_bpdi, lea_edx_si, lea_edx_di, lea_edx_disp16, lea_edx_bx
	.word lea_ebx_bxsi, lea_ebx_bxdi, lea_ebx_bpsi, lea_ebx_bpdi, lea_ebx_si, lea_ebx_di, lea_ebx_disp16, lea_ebx_bx
	.word lea_esp_bxsi, lea_esp_bxdi, lea_esp_bpsi, lea_esp_bpdi, lea_esp_si, lea_esp_di, lea_esp_disp16, lea_esp_bx
	.word lea_ebp_bxsi, lea_ebp_bxdi, lea_ebp_bpsi, lea_ebp_bpdi, lea_ebp_si, lea_ebp_di, lea_ebp_disp16, lea_ebp_bx
	.word lea_esi_bxsi, lea_esi_bxdi, lea_esi_bpsi, lea_esi_bpdi, lea_esi_si, lea_esi_di, lea_esi_disp16, lea_esi_bx
	.word lea_edi_bxsi, lea_edi_bxdi, lea_edi_bpsi, lea_edi_bpdi, lea_edi_si, lea_edi_di, lea_edi_disp16, lea_edi_bx
// 0x40
	.word lea_eax_bxsidisp8, lea_eax_bxdidisp8, lea_eax_bpsidisp8, lea_eax_bpdidisp8, lea_eax_sidisp8, lea_eax_didisp8, lea_eax_bpdisp8, lea_eax_bxdisp8
	.word lea_ecx_bxsidisp8, lea_ecx_bxdidisp8, lea_ecx_bpsidisp8, lea_ecx_bpdidisp8, lea_ecx_sidisp8, lea_ecx_didisp8, lea_ecx_bpdisp8, lea_ecx_bxdisp8
	.word lea_edx_bxsidisp8, lea_edx_bxdidisp8, lea_edx_bpsidisp8, lea_edx_bpdidisp8, lea_edx_sidisp8, lea_edx_didisp8, lea_edx_bpdisp8, lea_edx_bxdisp8
	.word lea_ebx_bxsidisp8, lea_ebx_bxdidisp8, lea_ebx_bpsidisp8, lea_ebx_bpdidisp8, lea_ebx_sidisp8, lea_ebx_didisp8, lea_ebx_bpdisp8, lea_ebx_bxdisp8
	.word lea_esp_bxsidisp8, lea_esp_bxdidisp8, lea_esp_bpsidisp8, lea_esp_bpdidisp8, lea_esp_sidisp8, lea_esp_didisp8, lea_esp_bpdisp8, lea_esp_bxdisp8
	.word lea_ebp_bxsidisp8, lea_ebp_bxdidisp8, lea_ebp_bpsidisp8, lea_ebp_bpdidisp8, lea_ebp_sidisp8, lea_ebp_didisp8, lea_ebp_bpdisp8, lea_ebp_bxdisp8
	.word lea_esi_bxsidisp8, lea_esi_bxdidisp8, lea_esi_bpsidisp8, lea_esi_bpdidisp8, lea_esi_sidisp8, lea_esi_didisp8, lea_esi_bpdisp8, lea_esi_bxdisp8
	.word lea_edi_bxsidisp8, lea_edi_bxdidisp8, lea_edi_bpsidisp8, lea_edi_bpdidisp8, lea_edi_sidisp8, lea_edi_didisp8, lea_edi_bpdisp8, lea_edi_bxdisp8
// 0x80
	.word lea_eax_bxsidisp16, lea_eax_bxdidisp16, lea_eax_bpsidisp16, lea_eax_bpdidisp16, lea_eax_sidisp16, lea_eax_didisp16, lea_eax_bpdisp16, lea_eax_bxdisp16
	.word lea_ecx_bxsidisp16, lea_ecx_bxdidisp16, lea_ecx_bpsidisp16, lea_ecx_bpdidisp16, lea_ecx_sidisp16, lea_ecx_didisp16, lea_ecx_bpdisp16, lea_ecx_bxdisp16
	.word lea_edx_bxsidisp16, lea_edx_bxdidisp16, lea_edx_bpsidisp16, lea_edx_bpdidisp16, lea_edx_sidisp16, lea_edx_didisp16, lea_edx_bpdisp16, lea_edx_bxdisp16
	.word lea_ebx_bxsidisp16, lea_ebx_bxdidisp16, lea_ebx_bpsidisp16, lea_ebx_bpdidisp16, lea_ebx_sidisp16, lea_ebx_didisp16, lea_ebx_bpdisp16, lea_ebx_bxdisp16
	.word lea_esp_bxsidisp16, lea_esp_bxdidisp16, lea_esp_bpsidisp16, lea_esp_bpdidisp16, lea_esp_sidisp16, lea_esp_didisp16, lea_esp_bpdisp16, lea_esp_bxdisp16
	.word lea_ebp_bxsidisp16, lea_ebp_bxdidisp16, lea_ebp_bpsidisp16, lea_ebp_bpdidisp16, lea_ebp_sidisp16, lea_ebp_didisp16, lea_ebp_bpdisp16, lea_ebp_bxdisp16
	.word lea_esi_bxsidisp16, lea_esi_bxdidisp16, lea_esi_bpsidisp16, lea_esi_bpdidisp16, lea_esi_sidisp16, lea_esi_didisp16, lea_esi_bpdisp16, lea_esi_bxdisp16
	.word lea_edi_bxsidisp16, lea_edi_bxdidisp16, lea_edi_bpsidisp16, lea_edi_bpdidisp16, lea_edi_sidisp16, lea_edi_didisp16, lea_edi_bpdisp16, lea_edi_bxdisp16
// 0xC0 = two register operands
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

// --- LEA reg32,[idx] ---
//
// Here segment override does not matter, we are only interested in the offset.
//

.macro lea_reg_idxidx reg idx1 idx2
	add		r0, \idx1, \idx2
	and		\reg, r0, r3
	b		loop
.endm

lea_eax_bxsi:
	lea_reg_idxidx eax ebx esi
lea_ecx_bxsi:
	lea_reg_idxidx ecx ebx esi
lea_edx_bxsi:
	lea_reg_idxidx edx ebx esi
lea_ebx_bxsi:
	lea_reg_idxidx ebx ebx esi
lea_esp_bxsi:
	lea_reg_idxidx esp ebx esi
lea_ebp_bxsi:
	lea_reg_idxidx ebp ebx esi
lea_esi_bxsi:
	lea_reg_idxidx esi ebx esi
lea_edi_bxsi:
	lea_reg_idxidx edi ebx esi

lea_eax_bxdi:
	lea_reg_idxidx eax ebx edi
lea_ecx_bxdi:
	lea_reg_idxidx ecx ebx edi
lea_edx_bxdi:
	lea_reg_idxidx edx ebx edi
lea_ebx_bxdi:
	lea_reg_idxidx ebx ebx edi
lea_esp_bxdi:
	lea_reg_idxidx esp ebx edi
lea_ebp_bxdi:
	lea_reg_idxidx ebp ebx edi
lea_esi_bxdi:
	lea_reg_idxidx esi ebx edi
lea_edi_bxdi:
	lea_reg_idxidx edi ebx edi

lea_eax_bpsi:
	lea_reg_idxidx eax ebp esi
lea_ecx_bpsi:
	lea_reg_idxidx ecx ebp esi
lea_edx_bpsi:
	lea_reg_idxidx edx ebp esi
lea_ebx_bpsi:
	lea_reg_idxidx ebx ebp esi
lea_esp_bpsi:
	lea_reg_idxidx esp ebp esi
lea_ebp_bpsi:
	lea_reg_idxidx ebp ebp esi
lea_esi_bpsi:
	lea_reg_idxidx esi ebp esi
lea_edi_bpsi:
	lea_reg_idxidx edi ebp esi

lea_eax_bpdi:
	lea_reg_idxidx eax ebp edi
lea_ecx_bpdi:
	lea_reg_idxidx ecx ebp edi
lea_edx_bpdi:
	lea_reg_idxidx edx ebp edi
lea_ebx_bpdi:
	lea_reg_idxidx ebx ebp edi
lea_esp_bpdi:
	lea_reg_idxidx esp ebp edi
lea_ebp_bpdi:
	lea_reg_idxidx ebp ebp edi
lea_esi_bpdi:
	lea_reg_idxidx esi ebp edi
lea_edi_bpdi:
	lea_reg_idxidx edi ebp edi

.macro lea_reg_disp16 reg
	ldrb	\reg, [r12], #1
	ldrb	r0, [r12], #1
	orr		\reg, r0, lsl #8
	b		loop
.endm

lea_eax_disp16:
	lea_reg_disp16 eax
lea_ecx_disp16:
	lea_reg_disp16 ecx
lea_edx_disp16:
	lea_reg_disp16 edx
lea_ebx_disp16:
	lea_reg_disp16 ebx
lea_esp_disp16:
	lea_reg_disp16 esp
lea_ebp_disp16:
	lea_reg_disp16 ebp
lea_esi_disp16:
	lea_reg_disp16 esi
lea_edi_disp16:
	lea_reg_disp16 edi

.macro lea_reg_idx reg idx
	and		\reg, \idx, r3
	b		loop
.endm

lea_eax_si:
	lea_reg_idx eax esi
lea_ecx_si:
	lea_reg_idx ecx esi
lea_edx_si:
	lea_reg_idx edx esi
lea_ebx_si:
	lea_reg_idx ebx esi
lea_esp_si:
	lea_reg_idx esp esi
lea_ebp_si:
	lea_reg_idx ebp esi
lea_esi_si:
	lea_reg_idx esi esi
lea_edi_si:
	lea_reg_idx edi esi

lea_eax_di:
	lea_reg_idx eax edi
lea_ecx_di:
	lea_reg_idx ecx edi
lea_edx_di:
	lea_reg_idx edx edi
lea_ebx_di:
	lea_reg_idx ebx edi
lea_esp_di:
	lea_reg_idx esp edi
lea_ebp_di:
	lea_reg_idx ebp edi
lea_esi_di:
	lea_reg_idx esi edi
lea_edi_di:
	lea_reg_idx edi edi

lea_eax_bx:
	lea_reg_idx eax ebx
lea_ecx_bx:
	lea_reg_idx ecx ebx
lea_edx_bx:
	lea_reg_idx edx ebx
lea_ebx_bx:
	lea_reg_idx ebx ebx
lea_esp_bx:
	lea_reg_idx esp ebx
lea_ebp_bx:
	lea_reg_idx ebp ebx
lea_esi_bx:
	lea_reg_idx esi ebx
lea_edi_bx:
	lea_reg_idx edi ebx

// --- LEA reg16,[idx+disp8] ---
//
// Here segment override does not matter, we are only interested in the offset.
//

.macro lea_reg_idxidxdisp8 reg idx1 idx2
	ldrsb	r0, [r12], #1
	add		r1, \idx1, \idx2
	add		r0, r1
	and		\reg, r0, r3
	b		loop
.endm

lea_eax_bxsidisp8:
	lea_reg_idxidxdisp8 eax ebx esi
lea_ecx_bxsidisp8:
	lea_reg_idxidxdisp8 ecx ebx esi
lea_edx_bxsidisp8:
	lea_reg_idxidxdisp8 edx ebx esi
lea_ebx_bxsidisp8:
	lea_reg_idxidxdisp8 ebx ebx esi
lea_esp_bxsidisp8:
	lea_reg_idxidxdisp8 esp ebx esi
lea_ebp_bxsidisp8:
	lea_reg_idxidxdisp8 ebp ebx esi
lea_esi_bxsidisp8:
	lea_reg_idxidxdisp8 esi ebx esi
lea_edi_bxsidisp8:
	lea_reg_idxidxdisp8 edi ebx esi

lea_eax_bxdidisp8:
	lea_reg_idxidxdisp8 eax ebx edi
lea_ecx_bxdidisp8:
	lea_reg_idxidxdisp8 ecx ebx edi
lea_edx_bxdidisp8:
	lea_reg_idxidxdisp8 edx ebx edi
lea_ebx_bxdidisp8:
	lea_reg_idxidxdisp8 ebx ebx edi
lea_esp_bxdidisp8:
	lea_reg_idxidxdisp8 esp ebx edi
lea_ebp_bxdidisp8:
	lea_reg_idxidxdisp8 ebp ebx edi
lea_esi_bxdidisp8:
	lea_reg_idxidxdisp8 esi ebx edi
lea_edi_bxdidisp8:
	lea_reg_idxidxdisp8 edi ebx edi

lea_eax_bpsidisp8:
	lea_reg_idxidxdisp8 eax ebp esi
lea_ecx_bpsidisp8:
	lea_reg_idxidxdisp8 ecx ebp esi
lea_edx_bpsidisp8:
	lea_reg_idxidxdisp8 edx ebp esi
lea_ebx_bpsidisp8:
	lea_reg_idxidxdisp8 ebx ebp esi
lea_esp_bpsidisp8:
	lea_reg_idxidxdisp8 esp ebp esi
lea_ebp_bpsidisp8:
	lea_reg_idxidxdisp8 ebp ebp esi
lea_esi_bpsidisp8:
	lea_reg_idxidxdisp8 esi ebp esi
lea_edi_bpsidisp8:
	lea_reg_idxidxdisp8 edi ebp esi

lea_eax_bpdidisp8:
	lea_reg_idxidxdisp8 eax ebp edi
lea_ecx_bpdidisp8:
	lea_reg_idxidxdisp8 ecx ebp edi
lea_edx_bpdidisp8:
	lea_reg_idxidxdisp8 edx ebp edi
lea_ebx_bpdidisp8:
	lea_reg_idxidxdisp8 ebx ebp edi
lea_esp_bpdidisp8:
	lea_reg_idxidxdisp8 esp ebp edi
lea_ebp_bpdidisp8:
	lea_reg_idxidxdisp8 ebp ebp edi
lea_esi_bpdidisp8:
	lea_reg_idxidxdisp8 esi ebp edi
lea_edi_bpdidisp8:
	lea_reg_idxidxdisp8 edi ebp edi

.macro lea_reg_idxdisp8 reg idx
	ldrsb	r0, [r12], #1
	add		r0, \idx
	and		\reg, r0, r3
	b		loop
.endm

lea_eax_sidisp8:
	lea_reg_idxdisp8 eax esi
lea_ecx_sidisp8:
	lea_reg_idxdisp8 ecx esi
lea_edx_sidisp8:
	lea_reg_idxdisp8 edx esi
lea_ebx_sidisp8:
	lea_reg_idxdisp8 ebx esi
lea_esp_sidisp8:
	lea_reg_idxdisp8 esp esi
lea_ebp_sidisp8:
	lea_reg_idxdisp8 ebp esi
lea_esi_sidisp8:
	lea_reg_idxdisp8 esi esi
lea_edi_sidisp8:
	lea_reg_idxdisp8 edi esi

lea_eax_didisp8:
	lea_reg_idxdisp8 eax edi
lea_ecx_didisp8:
	lea_reg_idxdisp8 ecx edi
lea_edx_didisp8:
	lea_reg_idxdisp8 edx edi
lea_ebx_didisp8:
	lea_reg_idxdisp8 ebx edi
lea_esp_didisp8:
	lea_reg_idxdisp8 esp edi
lea_ebp_didisp8:
	lea_reg_idxdisp8 ebp edi
lea_esi_didisp8:
	lea_reg_idxdisp8 esi edi
lea_edi_didisp8:
	lea_reg_idxdisp8 edi edi

lea_eax_bpdisp8:
	lea_reg_idxdisp8 eax ebp
lea_ecx_bpdisp8:
	lea_reg_idxdisp8 ecx ebp
lea_edx_bpdisp8:
	lea_reg_idxdisp8 edx ebp
lea_ebx_bpdisp8:
	lea_reg_idxdisp8 ebx ebp
lea_esp_bpdisp8:
	lea_reg_idxdisp8 esp ebp
lea_ebp_bpdisp8:
	lea_reg_idxdisp8 ebp ebp
lea_esi_bpdisp8:
	lea_reg_idxdisp8 esi ebp
lea_edi_bpdisp8:
	lea_reg_idxdisp8 edi ebp

lea_eax_bxdisp8:
	lea_reg_idxdisp8 eax ebx
lea_ecx_bxdisp8:
	lea_reg_idxdisp8 ecx ebx
lea_edx_bxdisp8:
	lea_reg_idxdisp8 edx ebx
lea_ebx_bxdisp8:
	lea_reg_idxdisp8 ebx ebx
lea_esp_bxdisp8:
	lea_reg_idxdisp8 esp ebx
lea_ebp_bxdisp8:
	lea_reg_idxdisp8 ebp ebx
lea_esi_bxdisp8:
	lea_reg_idxdisp8 esi ebx
lea_edi_bxdisp8:
	lea_reg_idxdisp8 edi ebx

// --- LEA reg16,[idx+disp16] ---
//
// Here segment override does not matter, we are only interested in the offset.
//
.macro lea_reg_idxidxdisp16 reg idx1 idx2
	r0_from_disp16
	add		r1, \idx1, \idx2
	add		r0, r1
	and		\reg, r0, r3
	b		loop
.endm

lea_eax_bxsidisp16:
	lea_reg_idxidxdisp16 eax ebx esi
lea_ecx_bxsidisp16:
	lea_reg_idxidxdisp16 ecx ebx esi
lea_edx_bxsidisp16:
	lea_reg_idxidxdisp16 edx ebx esi
lea_ebx_bxsidisp16:
	lea_reg_idxidxdisp16 ebx ebx esi
lea_esp_bxsidisp16:
	lea_reg_idxidxdisp16 esp ebx esi
lea_ebp_bxsidisp16:
	lea_reg_idxidxdisp16 ebp ebx esi
lea_esi_bxsidisp16:
	lea_reg_idxidxdisp16 esi ebx esi
lea_edi_bxsidisp16:
	lea_reg_idxidxdisp16 edi ebx esi

lea_eax_bxdidisp16:
	lea_reg_idxidxdisp16 eax ebx edi
lea_ecx_bxdidisp16:
	lea_reg_idxidxdisp16 ecx ebx edi
lea_edx_bxdidisp16:
	lea_reg_idxidxdisp16 edx ebx edi
lea_ebx_bxdidisp16:
	lea_reg_idxidxdisp16 ebx ebx edi
lea_esp_bxdidisp16:
	lea_reg_idxidxdisp16 esp ebx edi
lea_ebp_bxdidisp16:
	lea_reg_idxidxdisp16 ebp ebx edi
lea_esi_bxdidisp16:
	lea_reg_idxidxdisp16 esi ebx edi
lea_edi_bxdidisp16:
	lea_reg_idxidxdisp16 edi ebx edi

lea_eax_bpsidisp16:
	lea_reg_idxidxdisp16 eax ebp esi
lea_ecx_bpsidisp16:
	lea_reg_idxidxdisp16 ecx ebp esi
lea_edx_bpsidisp16:
	lea_reg_idxidxdisp16 edx ebp esi
lea_ebx_bpsidisp16:
	lea_reg_idxidxdisp16 ebx ebp esi
lea_esp_bpsidisp16:
	lea_reg_idxidxdisp16 esp ebp esi
lea_ebp_bpsidisp16:
	lea_reg_idxidxdisp16 ebp ebp esi
lea_esi_bpsidisp16:
	lea_reg_idxidxdisp16 esi ebp esi
lea_edi_bpsidisp16:
	lea_reg_idxidxdisp16 edi ebp esi

lea_eax_bpdidisp16:
	lea_reg_idxidxdisp16 eax ebp edi
lea_ecx_bpdidisp16:
	lea_reg_idxidxdisp16 ecx ebp edi
lea_edx_bpdidisp16:
	lea_reg_idxidxdisp16 edx ebp edi
lea_ebx_bpdidisp16:
	lea_reg_idxidxdisp16 ebx ebp edi
lea_esp_bpdidisp16:
	lea_reg_idxidxdisp16 esp ebp edi
lea_ebp_bpdidisp16:
	lea_reg_idxidxdisp16 ebp ebp edi
lea_esi_bpdidisp16:
	lea_reg_idxidxdisp16 esi ebp edi
lea_edi_bpdidisp16:
	lea_reg_idxidxdisp16 edi ebp edi

.macro lea_reg_idxdisp16 reg idx
	r0_from_disp16
	add		r0, \idx
	and		\reg, r0, r3
	b		loop
.endm

lea_eax_sidisp16:
	lea_reg_idxdisp16 eax esi
lea_ecx_sidisp16:
	lea_reg_idxdisp16 ecx esi
lea_edx_sidisp16:
	lea_reg_idxdisp16 edx esi
lea_ebx_sidisp16:
	lea_reg_idxdisp16 ebx esi
lea_esp_sidisp16:
	lea_reg_idxdisp16 esp esi
lea_ebp_sidisp16:
	lea_reg_idxdisp16 ebp esi
lea_esi_sidisp16:
	lea_reg_idxdisp16 esi esi
lea_edi_sidisp16:
	lea_reg_idxdisp16 edi esi

lea_eax_didisp16:
	lea_reg_idxdisp16 eax edi
lea_ecx_didisp16:
	lea_reg_idxdisp16 ecx edi
lea_edx_didisp16:
	lea_reg_idxdisp16 edx edi
lea_ebx_didisp16:
	lea_reg_idxdisp16 ebx edi
lea_esp_didisp16:
	lea_reg_idxdisp16 esp edi
lea_ebp_didisp16:
	lea_reg_idxdisp16 ebp edi
lea_esi_didisp16:
	lea_reg_idxdisp16 esi edi
lea_edi_didisp16:
	lea_reg_idxdisp16 edi edi

lea_eax_bpdisp16:
	lea_reg_idxdisp16 eax ebp
lea_ecx_bpdisp16:
	lea_reg_idxdisp16 ecx ebp
lea_edx_bpdisp16:
	lea_reg_idxdisp16 edx ebp
lea_ebx_bpdisp16:
	lea_reg_idxdisp16 ebx ebp
lea_esp_bpdisp16:
	lea_reg_idxdisp16 esp ebp
lea_ebp_bpdisp16:
	lea_reg_idxdisp16 ebp ebp
lea_esi_bpdisp16:
	lea_reg_idxdisp16 esi ebp
lea_edi_bpdisp16:
	lea_reg_idxdisp16 edi ebp

lea_eax_bxdisp16:
	lea_reg_idxdisp16 eax ebx
lea_ecx_bxdisp16:
	lea_reg_idxdisp16 ecx ebx
lea_edx_bxdisp16:
	lea_reg_idxdisp16 edx ebx
lea_ebx_bxdisp16:
	lea_reg_idxdisp16 ebx ebx
lea_esp_bxdisp16:
	lea_reg_idxdisp16 esp ebx
lea_ebp_bxdisp16:
	lea_reg_idxdisp16 ebp ebx
lea_esi_bxdisp16:
	lea_reg_idxdisp16 esi ebx
lea_edi_bxdisp16:
	lea_reg_idxdisp16 edi ebx

// ------------------- 8F = POP m32 ------------------------------------
op_8f_66:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
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

	modrm_1_help pop pop_t0 m32

// ------------------- A1 = MOV eax,disp16 ----------------------------
//
op_a1_66:
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r32_t0_r4

// ------------------- A3 = MOV disp16,eax ----------------------------
//
op_a3_66:
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
	r0_from_disp16
	b		mov_t0_r32_r4

// ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,imm8 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
	.global	op_c1_66
op_c1_66:
	modrm_jump_16
	modrm_tbl_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw imm8
	//0xc0 = mod = 11b => two register operands
	.word rol_eax_imm8, rol_ecx_imm8, rol_edx_imm8, rol_ebx_imm8, rol_esp_imm8, rol_ebp_imm8, rol_esi_imm8, rol_edi_imm8
	.word ror_eax_imm8, ror_ecx_imm8, ror_edx_imm8, ror_ebx_imm8, ror_esp_imm8, ror_ebp_imm8, ror_esi_imm8, ror_edi_imm8
	.word rcl_eax_imm8, rcl_ecx_imm8, rcl_edx_imm8, rcl_ebx_imm8, rcl_esp_imm8, rcl_ebp_imm8, rcl_esi_imm8, rcl_edi_imm8
	.word rcr_eax_imm8, rcr_ecx_imm8, rcr_edx_imm8, rcr_ebx_imm8, rcr_esp_imm8, rcr_ebp_imm8, rcr_esi_imm8, rcr_edi_imm8
	.word shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	.word shr_eax_imm8, shr_ecx_imm8, shr_edx_imm8, shr_ebx_imm8, shr_esp_imm8, shr_ebp_imm8, shr_esi_imm8, shr_edi_imm8
	.word shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	.word sar_eax_imm8, sar_ecx_imm8, sar_edx_imm8, sar_ebx_imm8, sar_esp_imm8, sar_ebp_imm8, sar_esi_imm8, sar_edi_imm8

	modrm_genall_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw "skip" sar_dw "t0" imm8

// ------------------- C4 = LES r32,m32:16 -----------------------------
//
	.global	op_c4_66
op_c4_66:
	modrm_jump_16
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

	modrm_3_genall les les_r32_t0				// Call handlers in cpu_386.S

// ------------------- C5 = LDS r32,m32:16 -----------------------------
//
	.global	op_c5_66
op_c5_66:
	modrm_jump_16
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

	modrm_3_genall lds lds_r32_t0				// Call handlers in cpu_386.S

// ------------------- C7 = MOV r/m16, imm16 -----------------------------
// USE32: 6567C7066C00E500E002 = mov dword gs:[006C],02E000E5 
//
	.global	op_c7_66
op_c7_66:
	ldrb	r0,[r12],#1							// Load the second opcode byte to r0, increment r12 by 1
	ldr		r3, [sp, #SP_MASK_16]				// Use 16-bit memory address masking
	ldr		pc,[pc, r0, lsl #2]					// Jump to the handler
	.word	0
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

	modrm_1_help mov mov_t0 imm32

// ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,1 ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
	.global	op_d1_66
op_d1_66:
	modrm_jump_16
	modrm_tbl_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw "1"
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

	modrm_genall_oper rol_dw,ror_dw,rcl_dw,rcr_dw,shl_dw,shr_dw,"skip",sar_dw,"t0","1"

// ------------------- D3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,CL ---
// - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
// - SHL/SHR/SHL/SAR change carry, sign and zero flags
	.global	op_d3_66
op_d3_66:
	modrm_jump_16
	modrm_tbl_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw shl_dw sar_dw CL
	//0xc0 = mod = 11b => two register operands
	.word rol_eax_CL, rol_ecx_CL, rol_edx_CL, rol_ebx_CL, rol_esp_CL, rol_ebp_CL, rol_esi_CL, rol_edi_CL
	.word ror_eax_CL, ror_ecx_CL, ror_edx_CL, ror_ebx_CL, ror_esp_CL, ror_ebp_CL, ror_esi_CL, ror_edi_CL
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	.word shr_eax_CL, shr_ecx_CL, shr_edx_CL, shr_ebx_CL, shr_esp_CL, shr_ebp_CL, shr_esi_CL, shr_edi_CL
	.word shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	.word sar_eax_CL, sar_ecx_CL, sar_edx_CL, sar_ebx_CL, sar_esp_CL, sar_ebp_CL, sar_esi_CL, sar_edi_CL

	modrm_genall_oper rol_dw ror_dw rcl_dw rcr_dw shl_dw shr_dw "skip" sar_dw "t0" CL

// ------------------- F7 = ??? r/m32 ----------------------------------
	.global op_f7_66
op_f7_66:
	modrm_jump_16
	modrm_tbl_oper test back2 not neg mul imul div idiv dword
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

	modrm_genall_oper test skip not neg mul imul div idiv "t0" dword

// ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
	.global op_ff_66
op_ff_66:
	modrm_jump_16
	modrm_tbl_oper inc_dword dec_dword call_near_32 call_far_32 jmp_near_32 jmp_far_32 push_dword back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_oper inc_dword dec_dword call_near_32 call_far_32 jmp_near_32 jmp_far_32 push_dword skip "r0"

	//----------------------------------------------
	// Operand Size = prefix, Address Size = default
	//----------------------------------------------
// 0x00
	.global op_01_66, op_03_66
	.global op_0b_66, op_0f_66
	.global op_26_66_r, 
	.global op_2b_66, op_2e_66_r, 
	.global op_33_66, op_36_66_r, 
	.global op_39_66, op_3b_66, op_3e_66_r, 
// 0x40
	.global op_64_66, op_65_66_r, op_66, op_66_67
// 0x80
	.global op_81_66, op_83_66, op_85_66, op_87_66
	.global op_89_66, op_8b_66, op_8d_66, op_8f_66
	.global op_a1_66, op_a3_66
	.global op_b8_66, op_b9_66, op_ba_66, op_bb_66, op_bc_66, op_bd_66, op_be_66, op_bf_66
	//----------------------------------------------
	// Operand Size = prefix, Address Size = default
	//----------------------------------------------
// 0x00
	.global op_0b_66, op_0f_66_p
	.global op_26_66_p, 
	.global op_2b_66, op_2e_66_p, 
	.global op_33_66, op_36_66_p, 
	.global op_39_66, op_3b_66, op_3e_66_p, 
// 0x40
	.global op_64_66, op_65_66_p, op_66_66, op_66_67
// 0x80
	.global op_89_66, op_8b_66, op_8c_66, op_8d_66, op_8f_66
