;=============================================================================
; cpu_66.s
;
; This file contains normal opcode handlers for when running in a USE16 segment
; and a prefix 0x66 (32-bit operands) is active. This is similar to a situation
; when running in a USE32 segment with a prefix 0x67 (16-bit addressing) active.
; In principle this file only contains the memory address parsing for word-size
; opcodes (since 8-bit opcodes are similar to non-prefixed versions in "cpu.s")
; with the actual handlers being in "cpu_386.s".
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http//dsx86.patrickaalto.com
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

	AREA cpu_66, CODE, READONLY

	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_66.inc

	EXTERN	loop
	EXTERN	unknown
	
	EXTERN	registers

op_c2_USE32
op_c3_USE32
op_f4
	b		unknown
	
; ------------------- 01 = ADD r/m32, r32 -----------------------------
; Use 16-bit memory address with 32-bit values
; 
	GLOBAL	op_01_66
op_01_66
	modrm_jump_16_tbl op_01_66_jump
	modrm_tbl_1 add
	
	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall add, add_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 03 = ADD r32, r/m32 ----------------------------
; Use 16-bit memory address with 32-bit values
;
	GLOBAL	op_03_66
op_03_66
	modrm_jump_16_tbl op_03_66_jump
	modrm_tbl_3 add
	
	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall add, add_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 09 = OR r/m32, r32 -----------------------------
;
	GLOBAL	op_09_66
op_09_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_09_66_jump
	modrm_tbl_1 or

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall or, or_t0_r32					; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 0B = OR r32, r/m32 -----------------------------
;
op_0b_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_0b_66_jump
	modrm_tbl_3 or

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall or, or_r32_t0					; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 0F = various opcodes ----------------------------
; op_0f = see "cpu_0F.S"

; ------------------- 11 = ADC r/m32, r32 -----------------------------
;
	GLOBAL	op_11_66
op_11_66
	modrm_jump_16_tbl op_11_66_jump
	modrm_tbl_1 adc

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall adc, adc_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 13 = ADC r32, r/m32 -----------------------------
;
	GLOBAL	op_13_66
op_13_66
	modrm_jump_16_tbl op_13_66_jump
	modrm_tbl_3 adc

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall adc, adc_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 19 = SBB r/m32, r32 -----------------------------
;
	GLOBAL	op_19_66
op_19_66
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					; Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_16_tbl op_19_66_jump
	modrm_tbl_1 sbb

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall sbb, sbb_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 1B = SBB r32, r/m32 ------------------------------
;
	GLOBAL	op_1b_66
op_1b_66
	mrs		r1,cpsr
	eor		r1, r1, #ARM_CARRY					; Reverse the Carry flag
	msr		cpsr_f,r1
	modrm_jump_16_tbl op_1b_66_jump
	modrm_tbl_3 sbb

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall sbb, sbb_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 21 = AND r/m32, r32 -----------------------------
;
	GLOBAL	op_21_66
op_21_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_21_66_jump
	modrm_tbl_1 and

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall and, and_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 23 = AND r32, r/m32 ----------------------------
;
	GLOBAL	op_23_66
op_23_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_23_66_jump
	modrm_tbl_3 and

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall and, and_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 29 = SUB r/m32, r32 ------------------------------
;
	GLOBAL	op_29_66
op_29_66
	modrm_jump_16_tbl op_29_66_jump
	modrm_tbl_1 sub

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall sub, sub_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 2B = SUB r32, r/m32 ------------------------------
;
	GLOBAL	op_2b_66
op_2b_66
	modrm_jump_16_tbl op_2b_66_jump
	modrm_tbl_3 sub

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall sub, sub_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 31 = XOR r/m32, r32 ----------------------------
;
	GLOBAL	op_31_66
op_31_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_31_66_jump
	modrm_tbl_1 xor

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall xor, xor_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 33 = XOR r32, r/m32 -----------------------------
;
	GLOBAL	op_33_66
op_33_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_33_66_jump
	modrm_tbl_3 xor

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall xor, xor_r32_t0		; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 39 = CMP r/m32, r32 -----------------------------
;
op_39_66
	modrm_jump_16_tbl op_39_66_jump
	modrm_tbl_1 cmp

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall cmp, cmp_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 3B = CMP r32, r/m32 ----------------------------
;
	GLOBAL	op_3b_66
op_3b_66
	modrm_jump_16_tbl op_3b_66_jump
	modrm_tbl_3 cmp

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall cmp, cmp_r32_t0					; Call handlers in cpu_386.S

	LTORG
	
; ------------------- Segment Overrides using "opcodetable_32_16" -----------------------

op_26_66
	ldr		r2, [sp, #SP_ES_BASE]				; r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_2e_66
	ldr		r2, [sp, #SP_CS_BASE]				; r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_36_66
	ldr		r2, [sp, #SP_SS_BASE]				; r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_3e_66
	ldr		r2, [sp, #SP_DS_BASE]				; r2 = logical DS segment in high halfword
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_64_66
	ldr		r2, [sp, #SP_FS_BASE]				; r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_65_66
	ldr		r2, [sp, #SP_GS_BASE]				; r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler


; ------------------- 66 = Operand-size Prefix ------------------------
	GLOBAL	op_66_USE16
	GLOBAL	op_67_USE32
op_66_USE16
op_67_USE32
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_32_16				; (opcodetable_32_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler


	IF 0 = 1

op_66_66
	lw		t1, GP_CODE_BIG(gp)			; Are we in a USE32 segment?
	get_cseip_byte t0					; Get the opcode byte into t0 register
	beqz	t1, 1b						; USE16 segment, so use normal op_66
	lw		t1, GP_OP_66_67(gp)			; USE32 segment, so op_66_66 is actually op_66_67 (which is USE16 opcode)
	sll		t0, 2						; t0 = 4*opcode
	addu	t1, t0
	lw		t1, 0(t1)					; t1 = jump address (from the 67-prefix table)
	jr		t1							; Jump to the opcode handler

	ENDIF

	LTORG

	EXTERN  op_00
	GLOBAL	op_01_66
	EXTERN  op_02
	GLOBAL	op_03_66
	EXTERN  op_04
	EXTERN  op_05_USE32
	EXTERN  op_06_USE32
	EXTERN  op_07_USE32
	EXTERN  op_08
	EXTERN  op_0a
	GLOBAL	op_0b_66
	EXTERN  op_0c
	EXTERN  op_0d_USE32
	EXTERN  op_0e_USE32
	EXTERN  op_0f_66
	EXTERN  op_10
	EXTERN  op_12
	EXTERN  op_14
	EXTERN  op_15_USE32
	EXTERN  op_16_USE32
	EXTERN  op_17_USE32
	EXTERN  op_18
	EXTERN  op_1a
	EXTERN  op_1c
	EXTERN  op_1d_USE32
	EXTERN  op_1e_USE32
	EXTERN  op_1f_USE32
	EXTERN  op_20
	EXTERN  op_22
	EXTERN  op_24
	EXTERN  op_25_USE32
	EXTERN  op_27
	EXTERN  op_28
	EXTERN  op_2a
	GLOBAL	op_2b_66
	EXTERN  op_2c
	EXTERN  op_2d_USE32
	EXTERN  op_2f
	EXTERN  op_30
	EXTERN  op_32
	GLOBAL	op_33_66
	EXTERN  op_34
	EXTERN  op_35_USE32
	EXTERN  op_37
	EXTERN  op_38
	GLOBAL	op_39_66
	EXTERN  op_3a
	GLOBAL	op_3b_66
	EXTERN  op_3c
	EXTERN  op_3d_USE32
	EXTERN  op_3f
; 0x40
	EXTERN  op_40_USE32
	EXTERN  op_41_USE32
	EXTERN  op_42_USE32
	EXTERN  op_43_USE32
	EXTERN  op_44_USE32
	EXTERN  op_45_USE32
	EXTERN  op_46_USE32
	EXTERN  op_47_USE32
	EXTERN  op_48_USE32
	EXTERN  op_49_USE32
	EXTERN  op_4a_USE32
	EXTERN  op_4b_USE32
	EXTERN  op_4c_USE32
	EXTERN  op_4d_USE32
	EXTERN  op_4e_USE32
	EXTERN  op_4f_USE32
	EXTERN  op_50_USE32
	EXTERN  op_51_USE32
	EXTERN  op_52_USE32
	EXTERN  op_53_USE32
	EXTERN  op_54_USE32
	EXTERN  op_55_USE32
	EXTERN  op_56_USE32
	EXTERN  op_57_USE32
	EXTERN  op_58_USE32
	EXTERN  op_59_USE32
	EXTERN  op_5a_USE32
	EXTERN  op_5b_USE32
	EXTERN  op_5c_USE32
	EXTERN  op_5d_USE32
	EXTERN  op_5e_USE32
	EXTERN  op_5f_USE32
	EXTERN  op_60_USE32
	EXTERN  op_61_USE32
	GLOBAL	op_64_66
	EXTERN  op_66_67_USE32
	EXTERN  op_66_67_USE16
	EXTERN  op_68_USE32
	EXTERN  op_6a_USE32
	EXTERN  op_70
	EXTERN  op_71
	EXTERN  op_72
	EXTERN  op_73
	EXTERN  op_74
	EXTERN  op_75
	EXTERN  op_76
	EXTERN  op_77
	EXTERN  op_78
	EXTERN  op_79
	EXTERN  op_7a
	EXTERN  op_7b
	EXTERN  op_7c
	EXTERN  op_7d
	EXTERN  op_7e
	EXTERN  op_7f
; 0x80
	EXTERN  op_80
	GLOBAL	op_81_66
	EXTERN  op_82
	GLOBAL	op_83_66
	EXTERN  op_84
	GLOBAL	op_85_66
	EXTERN  op_86
	GLOBAL	op_87_66
	EXTERN  op_88
	GLOBAL	op_89_66
	EXTERN  op_8a
	GLOBAL	op_8b_66
	GLOBAL	op_8c_66
	GLOBAL	op_8d_66
	EXTERN  op_8e
	GLOBAL	op_8f_66
	EXTERN  op_98_USE32
	EXTERN  op_99_USE32
	EXTERN  op_9c_USE32
	EXTERN  op_9d_USE32
	EXTERN  op_9e
	EXTERN  op_9f
	EXTERN  op_a0
	GLOBAL	op_a1_66
	EXTERN  op_a2
	GLOBAL	op_a3_66
	EXTERN  op_a4_movsb
	EXTERN  op_a5_movsd
	EXTERN  op_a6_cmpsb
	EXTERN  op_a7_cmpsd
	EXTERN  op_a9_USE32
	EXTERN  op_aa_stosb
	EXTERN  op_ab_stosd
	EXTERN  op_ac_lodsb
	EXTERN  op_ad_lodsd
	EXTERN  op_ae_scasb
	EXTERN  op_af_scasd
	EXTERN  op_b0
	EXTERN  op_b1
	EXTERN  op_b2
	EXTERN  op_b3
	EXTERN  op_b4
	EXTERN  op_b5
	EXTERN  op_b6
	EXTERN  op_b7
	EXTERN  op_b8_USE32
	EXTERN  op_b9_USE32
	EXTERN  op_ba_USE32
	EXTERN  op_bb_USE32
	EXTERN  op_bc_USE32
	EXTERN  op_bd_USE32
	EXTERN  op_be_USE32
	EXTERN  op_bf_USE32
; 0xC0
	EXTERN  op_c0
	EXTERN  op_c6
	EXTERN  op_c8_USE32
	EXTERN  op_c9_USE32
	EXTERN  op_ca_USE32
	EXTERN  op_cb_USE32
	EXTERN  op_cf_USE32
	EXTERN  op_d0
	EXTERN  op_d2
	EXTERN  op_d4
	EXTERN  op_d7
	IF USE_FPU_OPCODES = 1
	EXTERN  op_d8
	EXTERN  op_d9
	EXTERN  op_da
	EXTERN  op_db
	EXTERN  op_dc
	EXTERN  op_dd
	EXTERN  op_de
	EXTERN  op_df
	ENDIF
	EXTERN  op_e0
	EXTERN  op_e1
	EXTERN  op_e2
	EXTERN  op_e3
	EXTERN  op_e4_in_al_imm8
	EXTERN  op_e5_in_eax_imm8
	EXTERN  op_e6_out_imm8_al
	EXTERN  op_e7_out_imm8_eax
	EXTERN  op_ea_USE32
	EXTERN  op_ec_in_al_dx
	EXTERN  op_ed_in_eax_dx
	EXTERN  op_ee_out_dx_al
	EXTERN  op_ef_out_dx_eax
	EXTERN  op_f2_66
	EXTERN  op_f3_66
	EXTERN  op_f5
	EXTERN  op_f6
	EXTERN  op_f8
	EXTERN  op_f9
	EXTERN  op_fa_CLI
	EXTERN  op_fb_STI
	EXTERN  op_fc
	EXTERN  op_fd
	EXTERN  op_fe

; ------------------- 69 = IMUL r32,r/m32,imm32 ------------------------
	GLOBAL	op_69_66
op_69_66
	modrm_jump_16_tbl op_69_66_jump
	modrm_tbl_3 imul_imm32

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall imul_imm32, imul_imm32_r32_t0					; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 6B = IMUL r32,r/m32,imm8 ------------------------
	GLOBAL	op_6b_66
op_6b_66
	modrm_jump_16_tbl op_6b_66_jump
	modrm_tbl_3 imul_imm8

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall imul_imm8, imul_imm8_r32_t0					; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 81 = ??? r/m32,imm32 ----------------------------
	GLOBAL	op_81_66
op_81_66
	modrm_jump_16_tbl op_81_66_jump
	modrm_tbl_oper add, or, adc, sbb, and, sub, xor, cmp, imm32
	DCD add_eax_imm32, add_ecx_imm32, add_edx_imm32, add_ebx_imm32, add_esp_imm32, add_ebp_imm32, add_esi_imm32, add_edi_imm32
	DCD or_eax_imm32, or_ecx_imm32, or_edx_imm32, or_ebx_imm32, or_esp_imm32, or_ebp_imm32, or_esi_imm32, or_edi_imm32
	DCD adc_eax_imm32, adc_ecx_imm32, adc_edx_imm32, adc_ebx_imm32, adc_esp_imm32, adc_ebp_imm32, adc_esi_imm32, adc_edi_imm32
	DCD sbb_eax_imm32, sbb_ecx_imm32, sbb_edx_imm32, sbb_ebx_imm32, sbb_esp_imm32, sbb_ebp_imm32, sbb_esi_imm32, sbb_edi_imm32
	DCD and_eax_imm32, and_ecx_imm32, and_edx_imm32, and_ebx_imm32, and_esp_imm32, and_ebp_imm32, and_esi_imm32, and_edi_imm32
	DCD sub_eax_imm32, sub_ecx_imm32, sub_edx_imm32, sub_ebx_imm32, sub_esp_imm32, sub_ebp_imm32, sub_esi_imm32, sub_edi_imm32
	DCD xor_eax_imm32, xor_ecx_imm32, xor_edx_imm32, xor_ebx_imm32, xor_esp_imm32, xor_ebp_imm32, xor_esi_imm32, xor_edi_imm32
	DCD cmp_eax_imm32, cmp_ecx_imm32, cmp_edx_imm32, cmp_ebx_imm32, cmp_esp_imm32, cmp_ebp_imm32, cmp_esi_imm32, cmp_edi_imm32

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper add, or, adc, sbb, and, sub, xor, cmp, dw_t0, imm32
	
	LTORG
	
	EXTERN  add_eax_imm32
	EXTERN  add_ecx_imm32
	EXTERN  add_edx_imm32
	EXTERN  add_ebx_imm32
	EXTERN  add_esp_imm32
	EXTERN  add_ebp_imm32
	EXTERN  add_esi_imm32
	EXTERN  add_edi_imm32
	EXTERN  or_eax_imm32
	EXTERN  or_ecx_imm32
	EXTERN  or_edx_imm32
	EXTERN  or_ebx_imm32
	EXTERN  or_esp_imm32
	EXTERN  or_ebp_imm32
	EXTERN  or_esi_imm32
	EXTERN  or_edi_imm32
	EXTERN  adc_eax_imm32
	EXTERN  adc_ecx_imm32
	EXTERN  adc_edx_imm32
	EXTERN  adc_ebx_imm32
	EXTERN  adc_esp_imm32
	EXTERN  adc_ebp_imm32
	EXTERN  adc_esi_imm32
	EXTERN  adc_edi_imm32
	EXTERN  sbb_eax_imm32
	EXTERN  sbb_ecx_imm32
	EXTERN  sbb_edx_imm32
	EXTERN  sbb_ebx_imm32
	EXTERN  sbb_esp_imm32
	EXTERN  sbb_ebp_imm32
	EXTERN  sbb_esi_imm32
	EXTERN  sbb_edi_imm32
	EXTERN  and_eax_imm32
	EXTERN  and_ecx_imm32
	EXTERN  and_edx_imm32
	EXTERN  and_ebx_imm32
	EXTERN  and_esp_imm32
	EXTERN  and_ebp_imm32
	EXTERN  and_esi_imm32
	EXTERN  and_edi_imm32
	EXTERN  sub_eax_imm32
	EXTERN  sub_ecx_imm32
	EXTERN  sub_edx_imm32
	EXTERN  sub_ebx_imm32
	EXTERN  sub_esp_imm32
	EXTERN  sub_ebp_imm32
	EXTERN  sub_esi_imm32
	EXTERN  sub_edi_imm32
	EXTERN  xor_eax_imm32
	EXTERN  xor_ecx_imm32
	EXTERN  xor_edx_imm32
	EXTERN  xor_ebx_imm32
	EXTERN  xor_esp_imm32
	EXTERN  xor_ebp_imm32
	EXTERN  xor_esi_imm32
	EXTERN  xor_edi_imm32
	EXTERN  cmp_eax_imm32
	EXTERN  cmp_ecx_imm32
	EXTERN  cmp_edx_imm32
	EXTERN  cmp_ebx_imm32
	EXTERN  cmp_esp_imm32
	EXTERN  cmp_ebp_imm32
	EXTERN  cmp_esi_imm32
	EXTERN  cmp_edi_imm32

; ------------------- 83 = ??? r/m32,+imm8 ----------------------------
	GLOBAL	op_83_66
op_83_66
	modrm_jump_16_tbl op_83_66_jump
	modrm_tbl_oper add, or, adc, sbb, and, sub, xor, cmp, simm8
	DCD add_eax_simm8, add_ecx_simm8, add_edx_simm8, add_ebx_simm8, add_esp_simm8, add_ebp_simm8, add_esi_simm8, add_edi_simm8
	DCD or_eax_simm8, or_ecx_simm8, or_edx_simm8, or_ebx_simm8, or_esp_simm8, or_ebp_simm8, or_esi_simm8, or_edi_simm8
	DCD adc_eax_simm8, adc_ecx_simm8, adc_edx_simm8, adc_ebx_simm8, adc_esp_simm8, adc_ebp_simm8, adc_esi_simm8, adc_edi_simm8
	DCD sbb_eax_simm8, sbb_ecx_simm8, sbb_edx_simm8, sbb_ebx_simm8, sbb_esp_simm8, sbb_ebp_simm8, sbb_esi_simm8, sbb_edi_simm8
	DCD and_eax_simm8, and_ecx_simm8, and_edx_simm8, and_ebx_simm8, and_esp_simm8, and_ebp_simm8, and_esi_simm8, and_edi_simm8
	DCD sub_eax_simm8, sub_ecx_simm8, sub_edx_simm8, sub_ebx_simm8, sub_esp_simm8, sub_ebp_simm8, sub_esi_simm8, sub_edi_simm8
	DCD xor_eax_simm8, xor_ecx_simm8, xor_edx_simm8, xor_ebx_simm8, xor_esp_simm8, xor_ebp_simm8, xor_esi_simm8, xor_edi_simm8
	DCD cmp_eax_simm8, cmp_ecx_simm8, cmp_edx_simm8, cmp_ebx_simm8, cmp_esp_simm8, cmp_ebp_simm8, cmp_esi_simm8, cmp_edi_simm8

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper add, or, adc, sbb, and, sub, xor, cmp, dw_t0, simm8

	LTORG
	
	EXTERN  add_eax_simm8
	EXTERN  add_ecx_simm8
	EXTERN  add_edx_simm8
	EXTERN  add_ebx_simm8
	EXTERN  add_esp_simm8
	EXTERN  add_ebp_simm8
	EXTERN  add_esi_simm8
	EXTERN  add_edi_simm8
	EXTERN  or_eax_simm8
	EXTERN  or_ecx_simm8
	EXTERN  or_edx_simm8
	EXTERN  or_ebx_simm8
	EXTERN  or_esp_simm8
	EXTERN  or_ebp_simm8
	EXTERN  or_esi_simm8
	EXTERN  or_edi_simm8
	EXTERN  adc_eax_simm8
	EXTERN  adc_ecx_simm8
	EXTERN  adc_edx_simm8
	EXTERN  adc_ebx_simm8
	EXTERN  adc_esp_simm8
	EXTERN  adc_ebp_simm8
	EXTERN  adc_esi_simm8
	EXTERN  adc_edi_simm8
	EXTERN  sbb_eax_simm8
	EXTERN  sbb_ecx_simm8
	EXTERN  sbb_edx_simm8
	EXTERN  sbb_ebx_simm8
	EXTERN  sbb_esp_simm8
	EXTERN  sbb_ebp_simm8
	EXTERN  sbb_esi_simm8
	EXTERN  sbb_edi_simm8
	EXTERN  and_eax_simm8
	EXTERN  and_ecx_simm8
	EXTERN  and_edx_simm8
	EXTERN  and_ebx_simm8
	EXTERN  and_esp_simm8
	EXTERN  and_ebp_simm8
	EXTERN  and_esi_simm8
	EXTERN  and_edi_simm8
	EXTERN  sub_eax_simm8
	EXTERN  sub_ecx_simm8
	EXTERN  sub_edx_simm8
	EXTERN  sub_ebx_simm8
	EXTERN  sub_esp_simm8
	EXTERN  sub_ebp_simm8
	EXTERN  sub_esi_simm8
	EXTERN  sub_edi_simm8
	EXTERN  xor_eax_simm8
	EXTERN  xor_ecx_simm8
	EXTERN  xor_edx_simm8
	EXTERN  xor_ebx_simm8
	EXTERN  xor_esp_simm8
	EXTERN  xor_ebp_simm8
	EXTERN  xor_esi_simm8
	EXTERN  xor_edi_simm8
	EXTERN  cmp_eax_simm8
	EXTERN  cmp_ecx_simm8
	EXTERN  cmp_edx_simm8
	EXTERN  cmp_ebx_simm8
	EXTERN  cmp_esp_simm8
	EXTERN  cmp_ebp_simm8
	EXTERN  cmp_esi_simm8
	EXTERN  cmp_edi_simm8

; ------------------- 85 = TEST r32,r/m32 -----------------------------
op_85_66
	mov		r0, #0
	msr		cpsr_f,r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_85_66_jump
	modrm_tbl_1 test

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall test, test_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 87 = XCHG r/m32,r32 ----------------------------
op_87_66
	modrm_jump_16_tbl op_87_66_jump
	modrm_tbl_1 xchg

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall xchg, xchg_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 89 = MOV r/m32,r32 ------------------------------
	GLOBAL	op_89_66
op_89_66
	modrm_jump_16_tbl op_89_66_jump
	modrm_tbl_1 mov

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_genall mov, mov_t0_r32				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 8B = MOV r32,r/m32 -------------------------------
	GLOBAL	op_8b_66
op_8b_66
	modrm_jump_16_tbl op_8b_66_jump
	modrm_tbl_3 mov

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall mov, mov_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- 8C = MOV r/m16,Sreg -----------------------------
;

op_8c_66
	modrm_jump_16_tbl op_8c_66_jump

	MACRO
	tmp $sreg
	DCD mov_bxsi_$sreg, mov_bxdi_$sreg, mov_bpsi_$sreg, mov_bpdi_$sreg, mov_siidx_$sreg, mov_diidx_$sreg, mov_disp16_$sreg, mov_bxidx_$sreg
	EXTERN	mov_bxsi_$sreg
	EXTERN  mov_bxdi_$sreg
	EXTERN  mov_bpsi_$sreg
	EXTERN  mov_bpdi_$sreg
	EXTERN  mov_siidx_$sreg
	EXTERN  mov_diidx_$sreg
	EXTERN  mov_disp16_$sreg
	EXTERN  mov_bxidx_$sreg
	MEND
; 0
	tmp es
	tmp cs
	tmp ss
	tmp ds
	tmp fs
	tmp gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	MACRO 
	tmp1 $sreg
	DCD mov_bxsid8_$sreg, mov_bxdid8_$sreg, mov_bpsid8_$sreg, mov_bpdid8_$sreg, mov_sidisp8_$sreg, mov_didisp8_$sreg, mov_bpdisp8_$sreg, mov_bxdisp8_$sreg
	EXTERN	mov_bxsid8_$sreg
	EXTERN  mov_bxdid8_$sreg
	EXTERN  mov_bpsid8_$sreg
	EXTERN  mov_bpdid8_$sreg
	EXTERN  mov_sidisp8_$sreg
	EXTERN  mov_didisp8_$sreg
	EXTERN  mov_bpdisp8_$sreg
	EXTERN  mov_bxdisp8_$sreg
	MEND
;0x40
	tmp1 es
	tmp1 cs
	tmp1 ss
	tmp1 ds
	tmp1 fs
	tmp1 gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	MACRO
	tmp2 $sreg
	DCD mov_bxsid16_$sreg, mov_bxdid16_$sreg, mov_bpsid16_$sreg, mov_bpdid16_$sreg, mov_sidisp16_$sreg, mov_didisp16_$sreg, mov_bpdisp16_$sreg, mov_bxdisp16_$sreg
	EXTERN	mov_bxsid16_$sreg
	EXTERN  mov_bxdid16_$sreg
	EXTERN  mov_bpsid16_$sreg
	EXTERN  mov_bpdid16_$sreg
	EXTERN  mov_sidisp16_$sreg
	EXTERN  mov_didisp16_$sreg
	EXTERN  mov_bpdisp16_$sreg
	EXTERN  mov_bxdisp16_$sreg
	MEND
;0x80
	tmp2 es
	tmp2 cs
	tmp2 ss
	tmp2 ds
	tmp2 fs
	tmp2 gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0 = mod = 11b = register (in "cpu_386.S")
	modrm_help_1_C0 mov, es_r
	modrm_help_1_C0 mov, cs_r
	modrm_help_1_C0 mov, ss_r
	modrm_help_1_C0 mov, ds_r
	modrm_help_1_C0 mov, fs_r
	modrm_help_1_C0 mov, gs_r
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
 
; ------------------- 8D = LEA r32,m ----------------------------------
op_8d_66
	modrm_jump_16_tbl op_8d_66_jump
; 0
	DCD lea_eax_bxsi, lea_eax_bxdi, lea_eax_bpsi, lea_eax_bpdi, lea_eax_si, lea_eax_di, lea_eax_disp16, lea_eax_bx
	DCD lea_ecx_bxsi, lea_ecx_bxdi, lea_ecx_bpsi, lea_ecx_bpdi, lea_ecx_si, lea_ecx_di, lea_ecx_disp16, lea_ecx_bx
	DCD lea_edx_bxsi, lea_edx_bxdi, lea_edx_bpsi, lea_edx_bpdi, lea_edx_si, lea_edx_di, lea_edx_disp16, lea_edx_bx
	DCD lea_ebx_bxsi, lea_ebx_bxdi, lea_ebx_bpsi, lea_ebx_bpdi, lea_ebx_si, lea_ebx_di, lea_ebx_disp16, lea_ebx_bx
	DCD lea_esp_bxsi, lea_esp_bxdi, lea_esp_bpsi, lea_esp_bpdi, lea_esp_si, lea_esp_di, lea_esp_disp16, lea_esp_bx
	DCD lea_ebp_bxsi, lea_ebp_bxdi, lea_ebp_bpsi, lea_ebp_bpdi, lea_ebp_si, lea_ebp_di, lea_ebp_disp16, lea_ebp_bx
	DCD lea_esi_bxsi, lea_esi_bxdi, lea_esi_bpsi, lea_esi_bpdi, lea_esi_si, lea_esi_di, lea_esi_disp16, lea_esi_bx
	DCD lea_edi_bxsi, lea_edi_bxdi, lea_edi_bpsi, lea_edi_bpdi, lea_edi_si, lea_edi_di, lea_edi_disp16, lea_edi_bx
; 0x40
	DCD lea_eax_bxsidisp8, lea_eax_bxdidisp8, lea_eax_bpsidisp8, lea_eax_bpdidisp8, lea_eax_sidisp8, lea_eax_didisp8, lea_eax_bpdisp8, lea_eax_bxdisp8
	DCD lea_ecx_bxsidisp8, lea_ecx_bxdidisp8, lea_ecx_bpsidisp8, lea_ecx_bpdidisp8, lea_ecx_sidisp8, lea_ecx_didisp8, lea_ecx_bpdisp8, lea_ecx_bxdisp8
	DCD lea_edx_bxsidisp8, lea_edx_bxdidisp8, lea_edx_bpsidisp8, lea_edx_bpdidisp8, lea_edx_sidisp8, lea_edx_didisp8, lea_edx_bpdisp8, lea_edx_bxdisp8
	DCD lea_ebx_bxsidisp8, lea_ebx_bxdidisp8, lea_ebx_bpsidisp8, lea_ebx_bpdidisp8, lea_ebx_sidisp8, lea_ebx_didisp8, lea_ebx_bpdisp8, lea_ebx_bxdisp8
	DCD lea_esp_bxsidisp8, lea_esp_bxdidisp8, lea_esp_bpsidisp8, lea_esp_bpdidisp8, lea_esp_sidisp8, lea_esp_didisp8, lea_esp_bpdisp8, lea_esp_bxdisp8
	DCD lea_ebp_bxsidisp8, lea_ebp_bxdidisp8, lea_ebp_bpsidisp8, lea_ebp_bpdidisp8, lea_ebp_sidisp8, lea_ebp_didisp8, lea_ebp_bpdisp8, lea_ebp_bxdisp8
	DCD lea_esi_bxsidisp8, lea_esi_bxdidisp8, lea_esi_bpsidisp8, lea_esi_bpdidisp8, lea_esi_sidisp8, lea_esi_didisp8, lea_esi_bpdisp8, lea_esi_bxdisp8
	DCD lea_edi_bxsidisp8, lea_edi_bxdidisp8, lea_edi_bpsidisp8, lea_edi_bpdidisp8, lea_edi_sidisp8, lea_edi_didisp8, lea_edi_bpdisp8, lea_edi_bxdisp8
; 0x80
	DCD lea_eax_bxsidisp16, lea_eax_bxdidisp16, lea_eax_bpsidisp16, lea_eax_bpdidisp16, lea_eax_sidisp16, lea_eax_didisp16, lea_eax_bpdisp16, lea_eax_bxdisp16
	DCD lea_ecx_bxsidisp16, lea_ecx_bxdidisp16, lea_ecx_bpsidisp16, lea_ecx_bpdidisp16, lea_ecx_sidisp16, lea_ecx_didisp16, lea_ecx_bpdisp16, lea_ecx_bxdisp16
	DCD lea_edx_bxsidisp16, lea_edx_bxdidisp16, lea_edx_bpsidisp16, lea_edx_bpdidisp16, lea_edx_sidisp16, lea_edx_didisp16, lea_edx_bpdisp16, lea_edx_bxdisp16
	DCD lea_ebx_bxsidisp16, lea_ebx_bxdidisp16, lea_ebx_bpsidisp16, lea_ebx_bpdidisp16, lea_ebx_sidisp16, lea_ebx_didisp16, lea_ebx_bpdisp16, lea_ebx_bxdisp16
	DCD lea_esp_bxsidisp16, lea_esp_bxdidisp16, lea_esp_bpsidisp16, lea_esp_bpdidisp16, lea_esp_sidisp16, lea_esp_didisp16, lea_esp_bpdisp16, lea_esp_bxdisp16
	DCD lea_ebp_bxsidisp16, lea_ebp_bxdidisp16, lea_ebp_bpsidisp16, lea_ebp_bpdidisp16, lea_ebp_sidisp16, lea_ebp_didisp16, lea_ebp_bpdisp16, lea_ebp_bxdisp16
	DCD lea_esi_bxsidisp16, lea_esi_bxdidisp16, lea_esi_bpsidisp16, lea_esi_bpdidisp16, lea_esi_sidisp16, lea_esi_didisp16, lea_esi_bpdisp16, lea_esi_bxdisp16
	DCD lea_edi_bxsidisp16, lea_edi_bxdidisp16, lea_edi_bpsidisp16, lea_edi_bpdidisp16, lea_edi_sidisp16, lea_edi_didisp16, lea_edi_bpdisp16, lea_edi_bxdisp16
; 0xC0 = two register operands
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4

	LTORG
	
; --- LEA reg32,[idx] ---
;
; Here segment override does not matter, we are only interested in the offset.
;

	MACRO
	lea_reg_idxidx $reg, $idx1, $idx2
	add		r0, $idx1, $idx2
	and		$reg, r0, r3
	b		loop
	MEND

lea_eax_bxsi
	lea_reg_idxidx eax, ebx, esi
lea_ecx_bxsi
	lea_reg_idxidx ecx, ebx, esi
lea_edx_bxsi
	lea_reg_idxidx edx, ebx, esi
lea_ebx_bxsi
	lea_reg_idxidx ebx, ebx, esi
lea_esp_bxsi
	lea_reg_idxidx esp, ebx, esi
lea_ebp_bxsi
	lea_reg_idxidx ebp, ebx, esi
lea_esi_bxsi
	lea_reg_idxidx esi, ebx, esi
lea_edi_bxsi
	lea_reg_idxidx edi, ebx, esi

lea_eax_bxdi
	lea_reg_idxidx eax, ebx, edi
lea_ecx_bxdi
	lea_reg_idxidx ecx, ebx, edi
lea_edx_bxdi
	lea_reg_idxidx edx, ebx, edi
lea_ebx_bxdi
	lea_reg_idxidx ebx, ebx, edi
lea_esp_bxdi
	lea_reg_idxidx esp, ebx, edi
lea_ebp_bxdi
	lea_reg_idxidx ebp, ebx, edi
lea_esi_bxdi
	lea_reg_idxidx esi, ebx, edi
lea_edi_bxdi
	lea_reg_idxidx edi, ebx, edi

lea_eax_bpsi
	lea_reg_idxidx eax, ebp, esi
lea_ecx_bpsi
	lea_reg_idxidx ecx, ebp, esi
lea_edx_bpsi
	lea_reg_idxidx edx, ebp, esi
lea_ebx_bpsi
	lea_reg_idxidx ebx, ebp, esi
lea_esp_bpsi
	lea_reg_idxidx esp, ebp, esi
lea_ebp_bpsi
	lea_reg_idxidx ebp, ebp, esi
lea_esi_bpsi
	lea_reg_idxidx esi, ebp, esi
lea_edi_bpsi
	lea_reg_idxidx edi, ebp, esi

lea_eax_bpdi
	lea_reg_idxidx eax, ebp, edi
lea_ecx_bpdi
	lea_reg_idxidx ecx, ebp, edi
lea_edx_bpdi
	lea_reg_idxidx edx, ebp, edi
lea_ebx_bpdi
	lea_reg_idxidx ebx, ebp, edi
lea_esp_bpdi
	lea_reg_idxidx esp, ebp, edi
lea_ebp_bpdi
	lea_reg_idxidx ebp, ebp, edi
lea_esi_bpdi
	lea_reg_idxidx esi, ebp, edi
lea_edi_bpdi
	lea_reg_idxidx edi, ebp, edi

	MACRO
	lea_reg_disp16 $reg
	ldrb	$reg, [r12], #1
	ldrb	r0, [r12], #1
	orr		$reg, r0, lsl #8
	b		loop
	MEND

lea_eax_disp16
	lea_reg_disp16 eax
lea_ecx_disp16
	lea_reg_disp16 ecx
lea_edx_disp16
	lea_reg_disp16 edx
lea_ebx_disp16
	lea_reg_disp16 ebx
lea_esp_disp16
	lea_reg_disp16 esp
lea_ebp_disp16
	lea_reg_disp16 ebp
lea_esi_disp16
	lea_reg_disp16 esi
lea_edi_disp16
	lea_reg_disp16 edi

	MACRO
	lea_reg_idx $reg, $idx
	and		$reg, $idx, r3
	b		loop
	MEND

lea_eax_si
	lea_reg_idx eax, esi
lea_ecx_si
	lea_reg_idx ecx, esi
lea_edx_si
	lea_reg_idx edx, esi
lea_ebx_si
	lea_reg_idx ebx, esi
lea_esp_si
	lea_reg_idx esp, esi
lea_ebp_si
	lea_reg_idx ebp, esi
lea_esi_si
	lea_reg_idx esi, esi
lea_edi_si
	lea_reg_idx edi, esi

lea_eax_di
	lea_reg_idx eax, edi
lea_ecx_di
	lea_reg_idx ecx, edi
lea_edx_di
	lea_reg_idx edx, edi
lea_ebx_di
	lea_reg_idx ebx, edi
lea_esp_di
	lea_reg_idx esp, edi
lea_ebp_di
	lea_reg_idx ebp, edi
lea_esi_di
	lea_reg_idx esi, edi
lea_edi_di
	lea_reg_idx edi, edi

lea_eax_bx
	lea_reg_idx eax, ebx
lea_ecx_bx
	lea_reg_idx ecx, ebx
lea_edx_bx
	lea_reg_idx edx, ebx
lea_ebx_bx
	lea_reg_idx ebx, ebx
lea_esp_bx
	lea_reg_idx esp, ebx
lea_ebp_bx
	lea_reg_idx ebp, ebx
lea_esi_bx
	lea_reg_idx esi, ebx
lea_edi_bx
	lea_reg_idx edi, ebx

; --- LEA reg16,[idx+disp8] ---
;
; Here segment override does not matter, we are only interested in the offset.
;

	MACRO
	lea_reg_idxidxdisp8 $reg, $idx1, $idx2
	ldrsb	r0, [r12], #1
	add		r1, $idx1, $idx2
	add		r0, r1
	and		$reg, r0, r3
	b		loop
	MEND

lea_eax_bxsidisp8
	lea_reg_idxidxdisp8 eax, ebx, esi
lea_ecx_bxsidisp8
	lea_reg_idxidxdisp8 ecx, ebx, esi
lea_edx_bxsidisp8
	lea_reg_idxidxdisp8 edx, ebx, esi
lea_ebx_bxsidisp8
	lea_reg_idxidxdisp8 ebx, ebx, esi
lea_esp_bxsidisp8
	lea_reg_idxidxdisp8 esp, ebx, esi
lea_ebp_bxsidisp8
	lea_reg_idxidxdisp8 ebp, ebx, esi
lea_esi_bxsidisp8
	lea_reg_idxidxdisp8 esi, ebx, esi
lea_edi_bxsidisp8
	lea_reg_idxidxdisp8 edi, ebx, esi

lea_eax_bxdidisp8
	lea_reg_idxidxdisp8 eax, ebx, edi
lea_ecx_bxdidisp8
	lea_reg_idxidxdisp8 ecx, ebx, edi
lea_edx_bxdidisp8
	lea_reg_idxidxdisp8 edx, ebx, edi
lea_ebx_bxdidisp8
	lea_reg_idxidxdisp8 ebx, ebx, edi
lea_esp_bxdidisp8
	lea_reg_idxidxdisp8 esp, ebx, edi
lea_ebp_bxdidisp8
	lea_reg_idxidxdisp8 ebp, ebx, edi
lea_esi_bxdidisp8
	lea_reg_idxidxdisp8 esi, ebx, edi
lea_edi_bxdidisp8
	lea_reg_idxidxdisp8 edi, ebx, edi

lea_eax_bpsidisp8
	lea_reg_idxidxdisp8 eax, ebp, esi
lea_ecx_bpsidisp8
	lea_reg_idxidxdisp8 ecx, ebp, esi
lea_edx_bpsidisp8
	lea_reg_idxidxdisp8 edx, ebp, esi
lea_ebx_bpsidisp8
	lea_reg_idxidxdisp8 ebx, ebp, esi
lea_esp_bpsidisp8
	lea_reg_idxidxdisp8 esp, ebp, esi
lea_ebp_bpsidisp8
	lea_reg_idxidxdisp8 ebp, ebp, esi
lea_esi_bpsidisp8
	lea_reg_idxidxdisp8 esi, ebp, esi
lea_edi_bpsidisp8
	lea_reg_idxidxdisp8 edi, ebp, esi

lea_eax_bpdidisp8
	lea_reg_idxidxdisp8 eax, ebp, edi
lea_ecx_bpdidisp8
	lea_reg_idxidxdisp8 ecx, ebp, edi
lea_edx_bpdidisp8
	lea_reg_idxidxdisp8 edx, ebp, edi
lea_ebx_bpdidisp8
	lea_reg_idxidxdisp8 ebx, ebp, edi
lea_esp_bpdidisp8
	lea_reg_idxidxdisp8 esp, ebp, edi
lea_ebp_bpdidisp8
	lea_reg_idxidxdisp8 ebp, ebp, edi
lea_esi_bpdidisp8
	lea_reg_idxidxdisp8 esi, ebp, edi
lea_edi_bpdidisp8
	lea_reg_idxidxdisp8 edi, ebp, edi

	MACRO
	lea_reg_idxdisp8 $reg, $idx
	ldrsb	r0, [r12], #1
	add		r0, $idx
	and		$reg, r0, r3
	b		loop
	MEND

lea_eax_sidisp8
	lea_reg_idxdisp8 eax, esi
lea_ecx_sidisp8
	lea_reg_idxdisp8 ecx, esi
lea_edx_sidisp8
	lea_reg_idxdisp8 edx, esi
lea_ebx_sidisp8
	lea_reg_idxdisp8 ebx, esi
lea_esp_sidisp8
	lea_reg_idxdisp8 esp, esi
lea_ebp_sidisp8
	lea_reg_idxdisp8 ebp, esi
lea_esi_sidisp8
	lea_reg_idxdisp8 esi, esi
lea_edi_sidisp8
	lea_reg_idxdisp8 edi, esi

lea_eax_didisp8
	lea_reg_idxdisp8 eax, edi
lea_ecx_didisp8
	lea_reg_idxdisp8 ecx, edi
lea_edx_didisp8
	lea_reg_idxdisp8 edx, edi
lea_ebx_didisp8
	lea_reg_idxdisp8 ebx, edi
lea_esp_didisp8
	lea_reg_idxdisp8 esp, edi
lea_ebp_didisp8
	lea_reg_idxdisp8 ebp, edi
lea_esi_didisp8
	lea_reg_idxdisp8 esi, edi
lea_edi_didisp8
	lea_reg_idxdisp8 edi, edi

lea_eax_bpdisp8
	lea_reg_idxdisp8 eax, ebp
lea_ecx_bpdisp8
	lea_reg_idxdisp8 ecx, ebp
lea_edx_bpdisp8
	lea_reg_idxdisp8 edx, ebp
lea_ebx_bpdisp8
	lea_reg_idxdisp8 ebx, ebp
lea_esp_bpdisp8
	lea_reg_idxdisp8 esp, ebp
lea_ebp_bpdisp8
	lea_reg_idxdisp8 ebp, ebp
lea_esi_bpdisp8
	lea_reg_idxdisp8 esi, ebp
lea_edi_bpdisp8
	lea_reg_idxdisp8 edi, ebp

lea_eax_bxdisp8
	lea_reg_idxdisp8 eax, ebx
lea_ecx_bxdisp8
	lea_reg_idxdisp8 ecx, ebx
lea_edx_bxdisp8
	lea_reg_idxdisp8 edx, ebx
lea_ebx_bxdisp8
	lea_reg_idxdisp8 ebx, ebx
lea_esp_bxdisp8
	lea_reg_idxdisp8 esp, ebx
lea_ebp_bxdisp8
	lea_reg_idxdisp8 ebp, ebx
lea_esi_bxdisp8
	lea_reg_idxdisp8 esi, ebx
lea_edi_bxdisp8
	lea_reg_idxdisp8 edi, ebx

; --- LEA reg16,[idx+disp16] ---
;
; Here segment override does not matter, we are only interested in the offset.
;
	MACRO
	lea_reg_idxidxdisp16 $reg, $idx1, $idx2
	r0_from_disp16
	add		r1, $idx1, $idx2
	add		r0, r1
	and		$reg, r0, r3
	b		loop
	MEND

lea_eax_bxsidisp16
	lea_reg_idxidxdisp16 eax, ebx, esi
lea_ecx_bxsidisp16
	lea_reg_idxidxdisp16 ecx, ebx, esi
lea_edx_bxsidisp16
	lea_reg_idxidxdisp16 edx, ebx, esi
lea_ebx_bxsidisp16
	lea_reg_idxidxdisp16 ebx, ebx, esi
lea_esp_bxsidisp16
	lea_reg_idxidxdisp16 esp, ebx, esi
lea_ebp_bxsidisp16
	lea_reg_idxidxdisp16 ebp, ebx, esi
lea_esi_bxsidisp16
	lea_reg_idxidxdisp16 esi, ebx, esi
lea_edi_bxsidisp16
	lea_reg_idxidxdisp16 edi, ebx, esi

lea_eax_bxdidisp16
	lea_reg_idxidxdisp16 eax, ebx, edi
lea_ecx_bxdidisp16
	lea_reg_idxidxdisp16 ecx, ebx, edi
lea_edx_bxdidisp16
	lea_reg_idxidxdisp16 edx, ebx, edi
lea_ebx_bxdidisp16
	lea_reg_idxidxdisp16 ebx, ebx, edi
lea_esp_bxdidisp16
	lea_reg_idxidxdisp16 esp, ebx, edi
lea_ebp_bxdidisp16
	lea_reg_idxidxdisp16 ebp, ebx, edi
lea_esi_bxdidisp16
	lea_reg_idxidxdisp16 esi, ebx, edi
lea_edi_bxdidisp16
	lea_reg_idxidxdisp16 edi, ebx, edi

lea_eax_bpsidisp16
	lea_reg_idxidxdisp16 eax, ebp, esi
lea_ecx_bpsidisp16
	lea_reg_idxidxdisp16 ecx, ebp, esi
lea_edx_bpsidisp16
	lea_reg_idxidxdisp16 edx, ebp, esi
lea_ebx_bpsidisp16
	lea_reg_idxidxdisp16 ebx, ebp, esi
lea_esp_bpsidisp16
	lea_reg_idxidxdisp16 esp, ebp, esi
lea_ebp_bpsidisp16
	lea_reg_idxidxdisp16 ebp, ebp, esi
lea_esi_bpsidisp16
	lea_reg_idxidxdisp16 esi, ebp, esi
lea_edi_bpsidisp16
	lea_reg_idxidxdisp16 edi, ebp, esi

lea_eax_bpdidisp16
	lea_reg_idxidxdisp16 eax, ebp, edi
lea_ecx_bpdidisp16
	lea_reg_idxidxdisp16 ecx, ebp, edi
lea_edx_bpdidisp16
	lea_reg_idxidxdisp16 edx, ebp, edi
lea_ebx_bpdidisp16
	lea_reg_idxidxdisp16 ebx, ebp, edi
lea_esp_bpdidisp16
	lea_reg_idxidxdisp16 esp, ebp, edi
lea_ebp_bpdidisp16
	lea_reg_idxidxdisp16 ebp, ebp, edi
lea_esi_bpdidisp16
	lea_reg_idxidxdisp16 esi, ebp, edi
lea_edi_bpdidisp16
	lea_reg_idxidxdisp16 edi, ebp, edi

	MACRO
	lea_reg_idxdisp16 $reg, $idx
	r0_from_disp16
	add		r0, $idx
	and		$reg, r0, r3
	b		loop
	MEND

lea_eax_sidisp16
	lea_reg_idxdisp16 eax, esi
lea_ecx_sidisp16
	lea_reg_idxdisp16 ecx, esi
lea_edx_sidisp16
	lea_reg_idxdisp16 edx, esi
lea_ebx_sidisp16
	lea_reg_idxdisp16 ebx, esi
lea_esp_sidisp16
	lea_reg_idxdisp16 esp, esi
lea_ebp_sidisp16
	lea_reg_idxdisp16 ebp, esi
lea_esi_sidisp16
	lea_reg_idxdisp16 esi, esi
lea_edi_sidisp16
	lea_reg_idxdisp16 edi, esi

lea_eax_didisp16
	lea_reg_idxdisp16 eax, edi
lea_ecx_didisp16
	lea_reg_idxdisp16 ecx, edi
lea_edx_didisp16
	lea_reg_idxdisp16 edx, edi
lea_ebx_didisp16
	lea_reg_idxdisp16 ebx, edi
lea_esp_didisp16
	lea_reg_idxdisp16 esp, edi
lea_ebp_didisp16
	lea_reg_idxdisp16 ebp, edi
lea_esi_didisp16
	lea_reg_idxdisp16 esi, edi
lea_edi_didisp16
	lea_reg_idxdisp16 edi, edi

lea_eax_bpdisp16
	lea_reg_idxdisp16 eax, ebp
lea_ecx_bpdisp16
	lea_reg_idxdisp16 ecx, ebp
lea_edx_bpdisp16
	lea_reg_idxdisp16 edx, ebp
lea_ebx_bpdisp16
	lea_reg_idxdisp16 ebx, ebp
lea_esp_bpdisp16
	lea_reg_idxdisp16 esp, ebp
lea_ebp_bpdisp16
	lea_reg_idxdisp16 ebp, ebp
lea_esi_bpdisp16
	lea_reg_idxdisp16 esi, ebp
lea_edi_bpdisp16
	lea_reg_idxdisp16 edi, ebp

lea_eax_bxdisp16
	lea_reg_idxdisp16 eax, ebx
lea_ecx_bxdisp16
	lea_reg_idxdisp16 ecx, ebx
lea_edx_bxdisp16
	lea_reg_idxdisp16 edx, ebx
lea_ebx_bxdisp16
	lea_reg_idxdisp16 ebx, ebx
lea_esp_bxdisp16
	lea_reg_idxdisp16 esp, ebx
lea_ebp_bxdisp16
	lea_reg_idxdisp16 ebp, ebx
lea_esi_bxdisp16
	lea_reg_idxdisp16 esi, ebx
lea_edi_bxdisp16
	lea_reg_idxdisp16 edi, ebx

	LTORG
	
; ------------------- 8F = POP m32 ------------------------------------
op_8f_66
	modrm_jump_16_tbl op_8f_66_jump
; 0
	modrm_help_1_0 pop, m32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 pop, m32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	modrm_help_1_80 pop, m32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0
	DCD op_58_USE32, op_59_USE32, op_5a_USE32, op_5b_USE32, op_5c_USE32, op_5d_USE32, op_5e_USE32, op_5f_USE32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_help pop, pop_t0, m32

	LTORG
	
	EXTERN	pop_t0_m32
	EXTERN	pop_t0_bp_m32
; ------------------- A1 = MOV eax,disp16 ----------------------------
;
op_a1_66
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r32_t0_eax
	EXTERN	mov_r32_t0_eax

; ------------------- A3 = MOV disp16,eax ----------------------------
;
op_a3_66
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_t0_r32_eax
	EXTERN	mov_t0_r32_eax

; ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,imm8 ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags
	GLOBAL	op_c1_66
op_c1_66
	modrm_jump_16_tbl op_c1_66_jump
	modrm_tbl_oper rol_dw, ror_dw, rcl_dw, rcr_dw, shl_dw, shr_dw, shl_dw, sar_dw, imm8
	;0xc0 = mod = 11b => two register operands
	DCD rol_eax_imm8, rol_ecx_imm8, rol_edx_imm8, rol_ebx_imm8, rol_esp_imm8, rol_ebp_imm8, rol_esi_imm8, rol_edi_imm8
	DCD ror_eax_imm8, ror_ecx_imm8, ror_edx_imm8, ror_ebx_imm8, ror_esp_imm8, ror_ebp_imm8, ror_esi_imm8, ror_edi_imm8
	DCD rcl_eax_imm8, rcl_ecx_imm8, rcl_edx_imm8, rcl_ebx_imm8, rcl_esp_imm8, rcl_ebp_imm8, rcl_esi_imm8, rcl_edi_imm8
	DCD rcr_eax_imm8, rcr_ecx_imm8, rcr_edx_imm8, rcr_ebx_imm8, rcr_esp_imm8, rcr_ebp_imm8, rcr_esi_imm8, rcr_edi_imm8
	DCD shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	DCD shr_eax_imm8, shr_ecx_imm8, shr_edx_imm8, shr_ebx_imm8, shr_esp_imm8, shr_ebp_imm8, shr_esi_imm8, shr_edi_imm8
	DCD shl_eax_imm8, shl_ecx_imm8, shl_edx_imm8, shl_ebx_imm8, shl_esp_imm8, shl_ebp_imm8, shl_esi_imm8, shl_edi_imm8
	DCD sar_eax_imm8, sar_ecx_imm8, sar_edx_imm8, sar_ebx_imm8, sar_esp_imm8, sar_ebp_imm8, sar_esi_imm8, sar_edi_imm8

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper rol_dw, ror_dw, rcl_dw, rcr_dw, shl_dw, shr_dw, "skip", sar_dw, "t0", imm8

	LTORG
	
	EXTERN  rol_eax_imm8
	EXTERN  rol_ecx_imm8
	EXTERN  rol_edx_imm8
	EXTERN  rol_ebx_imm8
	EXTERN  rol_esp_imm8
	EXTERN  rol_ebp_imm8
	EXTERN  rol_esi_imm8
	EXTERN  rol_edi_imm8
	EXTERN  ror_eax_imm8
	EXTERN  ror_ecx_imm8
	EXTERN  ror_edx_imm8
	EXTERN  ror_ebx_imm8
	EXTERN  ror_esp_imm8
	EXTERN  ror_ebp_imm8
	EXTERN  ror_esi_imm8
	EXTERN  ror_edi_imm8
	EXTERN  rcl_eax_imm8
	EXTERN  rcl_ecx_imm8
	EXTERN  rcl_edx_imm8
	EXTERN  rcl_ebx_imm8
	EXTERN  rcl_esp_imm8
	EXTERN  rcl_ebp_imm8
	EXTERN  rcl_esi_imm8
	EXTERN  rcl_edi_imm8
	EXTERN  rcr_eax_imm8
	EXTERN  rcr_ecx_imm8
	EXTERN  rcr_edx_imm8
	EXTERN  rcr_ebx_imm8
	EXTERN  rcr_esp_imm8
	EXTERN  rcr_ebp_imm8
	EXTERN  rcr_esi_imm8
	EXTERN  rcr_edi_imm8
	EXTERN  shl_eax_imm8
	EXTERN  shl_ecx_imm8
	EXTERN  shl_edx_imm8
	EXTERN  shl_ebx_imm8
	EXTERN  shl_esp_imm8
	EXTERN  shl_ebp_imm8
	EXTERN  shl_esi_imm8
	EXTERN  shl_edi_imm8
	EXTERN  shr_eax_imm8
	EXTERN  shr_ecx_imm8
	EXTERN  shr_edx_imm8
	EXTERN  shr_ebx_imm8
	EXTERN  shr_esp_imm8
	EXTERN  shr_ebp_imm8
	EXTERN  shr_esi_imm8
	EXTERN  shr_edi_imm8
	EXTERN  sar_eax_imm8
	EXTERN  sar_ecx_imm8
	EXTERN  sar_edx_imm8
	EXTERN  sar_ebx_imm8
	EXTERN  sar_esp_imm8
	EXTERN  sar_ebp_imm8
	EXTERN  sar_esi_imm8
	EXTERN  sar_edi_imm8

; ------------------- C4 = LES r32,m3216 -----------------------------
;
	GLOBAL	op_c4_66
op_c4_66
	modrm_jump_16_tbl op_c4_66_jump
; 0
	modrm_help_3_0 les, eax
	modrm_help_3_0 les, ecx
	modrm_help_3_0 les, edx
	modrm_help_3_0 les, ebx
	modrm_help_3_0 les, esp
	modrm_help_3_0 les, ebp
	modrm_help_3_0 les, esi
	modrm_help_3_0 les, edi
; 0x40
	modrm_help_3_40 les, eax
	modrm_help_3_40 les, ecx
	modrm_help_3_40 les, edx
	modrm_help_3_40 les, ebx
	modrm_help_3_40 les, esp
	modrm_help_3_40 les, ebp
	modrm_help_3_40 les, esi
	modrm_help_3_40 les, edi
; 0x80
	modrm_help_3_80 les, eax
	modrm_help_3_80 les, ecx
	modrm_help_3_80 les, edx
	modrm_help_3_80 les, ebx
	modrm_help_3_80 les, esp
	modrm_help_3_80 les, ebp
	modrm_help_3_80 les, esi
	modrm_help_3_80 les, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall les, les_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- C5 = LDS r32,m3216 -----------------------------
;
	GLOBAL	op_c5_66
op_c5_66
	modrm_jump_16_tbl op_c5_66_jump
; 0
	modrm_help_3_0 lds, eax
	modrm_help_3_0 lds, ecx
	modrm_help_3_0 lds, edx
	modrm_help_3_0 lds, ebx
	modrm_help_3_0 lds, esp
	modrm_help_3_0 lds, ebp
	modrm_help_3_0 lds, esi
	modrm_help_3_0 lds, edi
; 0x40
	modrm_help_3_40 lds, eax
	modrm_help_3_40 lds, ecx
	modrm_help_3_40 lds, edx
	modrm_help_3_40 lds, ebx
	modrm_help_3_40 lds, esp
	modrm_help_3_40 lds, ebp
	modrm_help_3_40 lds, esi
	modrm_help_3_40 lds, edi
; 0x80
	modrm_help_3_80 lds, eax
	modrm_help_3_80 lds, ecx
	modrm_help_3_80 lds, edx
	modrm_help_3_80 lds, ebx
	modrm_help_3_80 lds, esp
	modrm_help_3_80 lds, ebp
	modrm_help_3_80 lds, esi
	modrm_help_3_80 lds, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_3_genall lds, lds_r32_t0				; Call handlers in cpu_386.S

	LTORG
	
; ------------------- C7 = MOV r/m16, imm16 -----------------------------
; USE32 6567C7066C00E500E002 = mov dword gs[006C],02E000E5 
;
	GLOBAL	op_c7_66
op_c7_66
	modrm_jump_16_tbl op_c7_66_jump
; 0
	modrm_help_1_0 mov, imm32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 mov, imm32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	modrm_help_1_80 mov, imm32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0
	DCD op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_1_help mov, mov_t0, imm32

	LTORG
	
	EXTERN	mov_t0_imm32
	EXTERN	mov_t0_bp_imm32

; ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,1 ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags
	GLOBAL	op_d1_66
op_d1_66
	modrm_jump_16_tbl op_d1_66_jump
	modrm_tbl_oper rol_dw, ror_dw, rcl_dw, rcr_dw, shl_dw, shr_dw, shl_dw, sar_dw, "1"
;0xc0 = mod = 11b => two register operands
	DCD rol_reg32_1_eax, rol_reg32_1_ecx, rol_reg32_1_edx, rol_reg32_1_ebx, rol_reg32_1_esp, rol_reg32_1_ebp, rol_reg32_1_esi, rol_reg32_1_edi
	DCD ror_reg32_1_eax, ror_reg32_1_ecx, ror_reg32_1_edx, ror_reg32_1_ebx, ror_reg32_1_esp, ror_reg32_1_ebp, ror_reg32_1_esi, ror_reg32_1_edi
	DCD rcl_reg32_1_eax, rcl_reg32_1_ecx, rcl_reg32_1_edx, rcl_reg32_1_ebx, rcl_reg32_1_esp, rcl_reg32_1_ebp, rcl_reg32_1_esi, rcl_reg32_1_edi
	DCD rcr_reg32_1_eax, rcr_reg32_1_ecx, rcr_reg32_1_edx, rcr_reg32_1_ebx, rcr_reg32_1_esp, rcr_reg32_1_ebp, rcr_reg32_1_esi, rcr_reg32_1_edi
; SHL reg32,1 == add reg32, reg32
	modrm_reg_reg_word add, eax, eax
	modrm_reg_reg_word add, ecx, ecx
	modrm_reg_reg_word add, edx, edx
	modrm_reg_reg_word add, ebx, ebx
	modrm_reg_reg_word add, esp, esp
	modrm_reg_reg_word add, ebp, ebp
	modrm_reg_reg_word add, esi, esi
	modrm_reg_reg_word add, edi, edi
	DCD shr_reg32_1_eax, shr_reg32_1_ecx, shr_reg32_1_edx, shr_reg32_1_ebx, shr_reg32_1_esp, shr_reg32_1_ebp, shr_reg32_1_esi, shr_reg32_1_edi
; SAL (never used)	
	modrm_reg_reg_word add, eax, eax
	modrm_reg_reg_word add, ecx, ecx
	modrm_reg_reg_word add, edx, edx
	modrm_reg_reg_word add, ebx, ebx
	modrm_reg_reg_word add, esp, esp
	modrm_reg_reg_word add, ebp, ebp
	modrm_reg_reg_word add, esi, esi
	modrm_reg_reg_word add, edi, edi
	DCD sar_reg32_1_eax, sar_reg32_1_ecx, sar_reg32_1_edx, sar_reg32_1_ebx, sar_reg32_1_esp, sar_reg32_1_ebp, sar_reg32_1_esi, sar_reg32_1_edi

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper rol_dw,ror_dw,rcl_dw,rcr_dw,shl_dw,shr_dw,"skip",sar_dw,"t0","1"

	LTORG
	
	EXTERN  rol_reg32_1_eax
	EXTERN  rol_reg32_1_ecx
	EXTERN  rol_reg32_1_edx
	EXTERN  rol_reg32_1_ebx
	EXTERN  rol_reg32_1_esp
	EXTERN  rol_reg32_1_ebp
	EXTERN  rol_reg32_1_esi
	EXTERN  rol_reg32_1_edi
	EXTERN  ror_reg32_1_eax
	EXTERN  ror_reg32_1_ecx
	EXTERN  ror_reg32_1_edx
	EXTERN  ror_reg32_1_ebx
	EXTERN  ror_reg32_1_esp
	EXTERN  ror_reg32_1_ebp
	EXTERN  ror_reg32_1_esi
	EXTERN  ror_reg32_1_edi
	EXTERN  rcl_reg32_1_eax
	EXTERN  rcl_reg32_1_ecx
	EXTERN  rcl_reg32_1_edx
	EXTERN  rcl_reg32_1_ebx
	EXTERN  rcl_reg32_1_esp
	EXTERN  rcl_reg32_1_ebp
	EXTERN  rcl_reg32_1_esi
	EXTERN  rcl_reg32_1_edi
	EXTERN  rcr_reg32_1_eax
	EXTERN  rcr_reg32_1_ecx
	EXTERN  rcr_reg32_1_edx
	EXTERN  rcr_reg32_1_ebx
	EXTERN  rcr_reg32_1_esp
	EXTERN  rcr_reg32_1_ebp
	EXTERN  rcr_reg32_1_esi
	EXTERN  rcr_reg32_1_edi
	EXTERN  shr_reg32_1_eax
	EXTERN  shr_reg32_1_ecx
	EXTERN  shr_reg32_1_edx
	EXTERN  shr_reg32_1_ebx
	EXTERN  shr_reg32_1_esp
	EXTERN  shr_reg32_1_ebp
	EXTERN  shr_reg32_1_esi
	EXTERN  shr_reg32_1_edi
	EXTERN  sar_reg32_1_eax
	EXTERN  sar_reg32_1_ecx
	EXTERN  sar_reg32_1_edx
	EXTERN  sar_reg32_1_ebx
	EXTERN  sar_reg32_1_esp
	EXTERN  sar_reg32_1_ebp
	EXTERN  sar_reg32_1_esi
	EXTERN  sar_reg32_1_edi


; ------------------- D3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m32,CL ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags
	GLOBAL	op_d3_66
op_d3_66
	modrm_jump_16_tbl op_d3_66_jump
	modrm_tbl_oper rol_dw, ror_dw, rcl_dw, rcr_dw, shl_dw, shr_dw, shl_dw, sar_dw, CL
	;0xc0 = mod = 11b => two register operands
	DCD rol_eax_CL, rol_ecx_CL, rol_edx_CL, rol_ebx_CL, rol_esp_CL, rol_ebp_CL, rol_esi_CL, rol_edi_CL
	DCD ror_eax_CL, ror_ecx_CL, ror_edx_CL, ror_ebx_CL, ror_esp_CL, ror_ebp_CL, ror_esi_CL, ror_edi_CL
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	DCD shr_eax_CL, shr_ecx_CL, shr_edx_CL, shr_ebx_CL, shr_esp_CL, shr_ebp_CL, shr_esi_CL, shr_edi_CL
	DCD shl_eax_CL, shl_ecx_CL, shl_edx_CL, shl_ebx_CL, shl_esp_CL, shl_ebp_CL, shl_esi_CL, shl_edi_CL
	DCD sar_eax_CL, sar_ecx_CL, sar_edx_CL, sar_ebx_CL, sar_esp_CL, sar_ebp_CL, sar_esi_CL, sar_edi_CL

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper rol_dw, ror_dw, rcl_dw, rcr_dw, shl_dw, shr_dw, "skip", sar_dw, "t0", CL

	LTORG
	
	EXTERN	rol_eax_CL
	EXTERN	rol_ecx_CL
	EXTERN  rol_edx_CL
	EXTERN  rol_ebx_CL
	EXTERN  rol_esp_CL
	EXTERN  rol_ebp_CL
	EXTERN  rol_esi_CL
	EXTERN  rol_edi_CL
	EXTERN	ror_eax_CL
	EXTERN  ror_ecx_CL
	EXTERN  ror_edx_CL
	EXTERN  ror_ebx_CL
	EXTERN  ror_esp_CL
	EXTERN  ror_ebp_CL
	EXTERN  ror_esi_CL
	EXTERN  ror_edi_CL
	EXTERN	shl_eax_CL
	EXTERN  shl_ecx_CL
	EXTERN  shl_edx_CL
	EXTERN  shl_ebx_CL
	EXTERN  shl_esp_CL
	EXTERN  shl_ebp_CL
	EXTERN  shl_esi_CL
	EXTERN  shl_edi_CL
	EXTERN	shr_eax_CL
	EXTERN  shr_ecx_CL
	EXTERN  shr_edx_CL
	EXTERN  shr_ebx_CL
	EXTERN  shr_esp_CL
	EXTERN  shr_ebp_CL
	EXTERN  shr_esi_CL
	EXTERN  shr_edi_CL
	EXTERN	sar_eax_CL
	EXTERN  sar_ecx_CL
	EXTERN  sar_edx_CL
	EXTERN  sar_ebx_CL
	EXTERN  sar_esp_CL
	EXTERN  sar_ebp_CL
	EXTERN  sar_esi_CL
	EXTERN  sar_edi_CL

; ------------------- F7 = ??? r/m32 ----------------------------------
	GLOBAL op_f7_66
op_f7_66
	modrm_jump_16_tbl op_f7_66_jump
	modrm_tbl_oper test, back2, not, neg, mul, imul, div, idiv, dword
;0xc0 = mod = 11b => register operand
	modrm_help_3_C0 test_imm32
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_3_C0 not
	modrm_help_3_C0 neg
	modrm_help_3_C0 mul
	modrm_help_3_C0 imul
	modrm_help_3_C0 div
	modrm_help_3_C0 idiv

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper test, skip, not, neg, mul, imul, div, idiv, "t0", dword

	LTORG
	
; ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
	GLOBAL op_ff_66
op_ff_66
	modrm_jump_16_tbl op_ff_66_jump
	modrm_tbl_oper inc_dword, dec_dword, call_near_32, call_far_32, jmp_near_32, jmp_far_32, push_dword, back2
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_66, CODE, READONLY
	ALIGN	4
	
	modrm_genall_oper inc_dword, dec_dword, call_near_32, call_far_32, jmp_near_32, jmp_far_32, push_dword, skip, "r0"

	LTORG
	
	AREA	jumptables, DATA, READONLY
	ALIGN	4

	;----------------------------------------------
	; Operand Size = 32bit, address size = 16bit
	;----------------------------------------------
	ALIGN	4
opcodetable_32_16
; 0x00
	DCD op_00, op_01_66, op_02, op_03_66, op_04, op_05_USE32, op_06_USE32, op_07_USE32
	DCD op_08, op_09_66, op_0a, op_0b_66, op_0c, op_0d_USE32, op_0e_USE32, op_0f_66
	DCD op_10, op_11_66, op_12, op_13_66, op_14, op_15_USE32, op_16_USE32, op_17_USE32
	DCD op_18, op_19_66, op_1a, op_1b_66, op_1c, op_1d_USE32, op_1e_USE32, op_1f_USE32
	DCD op_20, op_21_66, op_22, op_23_66, op_24, op_25_USE32, op_26_66, op_27
	DCD op_28, op_29_66, op_2a, op_2b_66, op_2c, op_2d_USE32, op_2e_66, op_2f
	DCD op_30, op_31_66, op_32, op_33_66, op_34, op_35_USE32, op_36_66, op_37
	DCD op_38, op_39_66, op_3a, op_3b_66, op_3c, op_3d_USE32, op_3e_66, op_3f
; 0x40
	DCD op_40_USE32, op_41_USE32, op_42_USE32, op_43_USE32, op_44_USE32, op_45_USE32, op_46_USE32, op_47_USE32
	DCD op_48_USE32, op_49_USE32, op_4a_USE32, op_4b_USE32, op_4c_USE32, op_4d_USE32, op_4e_USE32, op_4f_USE32
	DCD op_50_USE32, op_51_USE32, op_52_USE32, op_53_USE32, op_54_USE32, op_55_USE32, op_56_USE32, op_57_USE32
	DCD op_58_USE32, op_59_USE32, op_5a_USE32, op_5b_USE32, op_5c_USE32, op_5d_USE32, op_5e_USE32, op_5f_USE32
	DCD op_60_USE32, op_61_USE32, unknown, unknown, op_64_66, op_65_66, op_66_67_USE32, op_66_67_USE16
	DCD op_68_USE32, op_69_66, op_6a_USE32, op_6b_66, unknown, unknown, unknown, unknown
	DCD op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	DCD op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
; 0x80
	DCD op_80, op_81_66, op_82, op_83_66, op_84, op_85_66, op_86, op_87_66
	DCD op_88, op_89_66, op_8a, op_8b_66, op_8c_66, op_8d_66, op_8e, op_8f_66
	DCD loop
	modrm_reg_reg_word xchg, ecx, eax
	modrm_reg_reg_word xchg, edx, eax
	modrm_reg_reg_word xchg, ebx, eax
	modrm_reg_reg_word xchg, esp, eax
	modrm_reg_reg_word xchg, ebp, eax
	modrm_reg_reg_word xchg, esi, eax
	modrm_reg_reg_word xchg, edi, eax
	DCD op_98_USE32, op_99_USE32, unknown, loop, op_9c_USE32, op_9d_USE32, op_9e, op_9f
	DCD op_a0, op_a1_66, op_a2, op_a3_66, op_a4_movsb, op_a5_movsd, op_a6_cmpsb, op_a7_cmpsd
	DCD unknown, op_a9_USE32, op_aa_stosb, op_ab_stosd, op_ac_lodsb, op_ad_lodsd, op_ae_scasb, op_af_scasd
	DCD op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	DCD op_b8_USE32, op_b9_USE32, op_ba_USE32, op_bb_USE32, op_bc_USE32, op_bd_USE32, op_be_USE32, op_bf_USE32
; 0xC0
	DCD op_c0, op_c1_66, op_c2_USE32, op_c3_USE32, op_c4_66, op_c5_66, op_c6, op_c7_66
	DCD op_c8_USE32, op_c9_USE32, op_ca_USE32, op_cb_USE32, unknown, unknown, unknown, op_cf_USE32
	DCD op_d0, op_d1_66, op_d2, op_d3_66, op_d4, unknown, unknown, op_d7
	IF USE_FPU_OPCODES = 1
	DCD op_d8, op_d9, op_da, op_db, op_dc, op_dd, op_de, op_df
	ELSE
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ENDIF
	DCD op_e0, op_e1, op_e2, op_e3, op_e4_in_al_imm8, op_e5_in_eax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_eax
	DCD unknown, unknown, op_ea_USE32, unknown, op_ec_in_al_dx, op_ed_in_eax_dx, op_ee_out_dx_al, op_ef_out_dx_eax
	DCD unknown, unknown, op_f2_66, op_f3_66, op_f4, op_f5, op_f6, op_f7_66
	DCD op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe, op_ff_66


	END
