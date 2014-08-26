;=============================================================================
; cpu_0F_67.s
;
; This file contains opcode handlers for the 0x0F-prefixed opcodes, for 32-bit
; address size and 16-bit operand size.
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

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_67.inc

	EXTERN unknown

	EXTERN op_0f_06
	EXTERN op_0f_20
	EXTERN op_0f_21
	EXTERN op_0f_22
	EXTERN op_0f_23
	EXTERN jo_imm16
	EXTERN jno_imm16
	EXTERN jc_imm16 
	EXTERN jnc_imm16
	EXTERN jz_imm16
	EXTERN jnz_imm16
	EXTERN jbe_imm16
	EXTERN ja_imm16
	EXTERN js_imm16
	EXTERN jns_imm16
	EXTERN jpo_imm16
	EXTERN jpe_imm16
	EXTERN jl_imm16
	EXTERN jge_imm16
	EXTERN jle_imm16
	EXTERN jg_imm16
	EXTERN op_0f_90_USE32
	EXTERN op_0f_91_USE32
	EXTERN op_0f_92_USE32
	EXTERN op_0f_93_USE32
	EXTERN op_0f_94_USE32
	EXTERN op_0f_95_USE32
	EXTERN op_0f_96_USE32
	EXTERN op_0f_97_USE32
	EXTERN op_0f_98_USE32
	EXTERN op_0f_99_USE32
	EXTERN op_0f_9a_USE32
	EXTERN op_0f_9b_USE32
	EXTERN op_0f_9C_USE32
	EXTERN op_0f_9D_USE32
	EXTERN op_0f_9E_USE32
	EXTERN op_0f_9F_USE32
	EXTERN push_fs
	EXTERN pop_fs
	EXTERN push_gs
	EXTERN pop_gs
	EXTERN op_8b_67

	EXTERN	registers
	
	EXTERN bswap_eax
	EXTERN bswap_ecx
	EXTERN bswap_edx
	EXTERN bswap_ebx
	EXTERN bswap_esp
	EXTERN bswap_ebp
	EXTERN bswap_esi
	EXTERN bswap_edi

	MACRO
	modrm_reg_reg_word $oper, $lr, $rr
		IF "$rr" = ""
			EXTERN $oper_$lr
			DCD	$oper_$lr
		ELSE
			EXTERN $oper_$lr_$rr
			DCD	$oper_$lr_$rr
		ENDIF
	MEND

	MACRO
	modrm_help_1_C0 $oper, $reg
		modrm_reg_reg_word $oper, eax, $reg
		modrm_reg_reg_word $oper, ecx, $reg
		modrm_reg_reg_word $oper, edx, $reg
		modrm_reg_reg_word $oper, ebx, $reg
		modrm_reg_reg_word $oper, esp, $reg
		modrm_reg_reg_word $oper, ebp, $reg
		modrm_reg_reg_word $oper, esi, $reg
		modrm_reg_reg_word $oper, edi, $reg
	MEND

	MACRO
	modrm_help_3_C0 $oper, $reg
		modrm_reg_reg_word $oper, $reg, eax
		modrm_reg_reg_word $oper, $reg, ecx
		modrm_reg_reg_word $oper, $reg, edx
		modrm_reg_reg_word $oper, $reg, ebx
		modrm_reg_reg_word $oper, $reg, esp
		modrm_reg_reg_word $oper, $reg, ebp
		modrm_reg_reg_word $oper, $reg, esi
		modrm_reg_reg_word $oper, $reg, edi
	MEND


	MACRO
	modrm_3_67 $oper, $jump, $reg
$oper_$reg_bxidx
	mov		r0, ebx
	b		$jump_$reg
$oper_$reg_siidx
	mov		r0, esi
	b		$jump_$reg
$oper_$reg_diidx
	mov		r0, edi
	b		$jump_$reg
$oper_$reg_bxdisp8
	r0_from_idx_disp8 ebx
	b		$jump_$reg
$oper_$reg_bpdisp8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
$oper_$reg_sidisp8
	r0_from_idx_disp8 esi
	b		$jump_$reg
$oper_$reg_didisp8
	r0_from_idx_disp8 edi
	b		$jump_$reg
	MEND

	MACRO
	modrm_3_gen67 $oper, $jump
		modrm_3_67 $oper, $jump, eax
		modrm_3_67 $oper, $jump, ecx
		modrm_3_67 $oper, $jump, edx
		modrm_3_67 $oper, $jump, ebx
		modrm_3_67 $oper, $jump, esp
		modrm_3_67 $oper, $jump, ebp
		modrm_3_67 $oper, $jump, esi
		modrm_3_67 $oper, $jump, edi
	MEND

; ------------------- 0F 00 = SLDT/STR/LLDT/LTR/VERR/VERW r/m16 ------
; 0F00D0 = lldt ax (DOS4GW, AX=0068)
;
	GLOBAL	op_0f_00_67
op_0f_00_67
	;-------
	; TODO! Only available in protected mode!
	;-------
	modrm_jump_32_tbl op_0f_00_67_jump
; 0
	modrm_help_1_0 sldt
	modrm_help_1_0 str
	modrm_help_1_0 lldt
	modrm_help_1_0 ltr
	modrm_help_1_0 verr
	modrm_help_1_0 verw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x40
	modrm_help_1_40 sldt
	modrm_help_1_40 str
	modrm_help_1_40 lldt
	modrm_help_1_40 ltr
	modrm_help_1_40 verr
	modrm_help_1_40 verw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x80
	modrm_help_1_80 sldt
	modrm_help_1_80 str
	modrm_help_1_80 lldt
	modrm_help_1_80 ltr
	modrm_help_1_80 verr
	modrm_help_1_80 verw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0xc0 = mod = 11b => register operand
	modrm_help_1_C0 sldt
	modrm_help_1_C0 str
	modrm_help_1_C0 lldt
	modrm_help_1_C0 ltr
	modrm_help_1_C0 verr
	modrm_help_1_C0 verw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

; Note! Actual handlers are in "cpu_0F.s"

	modrm_genext_oper sldt, str, lldt, ltr, verr, verw, skip, skip, ""

	LTORG
	
; ------------------- 0F 01 = SGDT/SIDT/LGDT/LIDT/SMSW r/m16 ---------
; 670F011500080000 = lgdt dword [00000800] (Zone66)		(Prot mode entry at 02DA:04A2)
;
op_0f_01_67
	modrm_jump_32_tbl op_0f_01_67_jump
	modrm_tbl_oper sgdt, sidt, lgdt_word, lidt_word, smsw, back2, lmsw, back2
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 smsw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 lmsw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	
	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper sgdt, sidt, lgdt_word, lidt_word, smsw, skip, lmsw, skip, "r0"

	LTORG
	
; ------------------- 0F 02 = LAR r16, r/m16 -------------------------
; These are only available while in protected mode!
;
op_0f_02_67
	modrm_jump_32_tbl op_0f_02_67_jump
	modrm_tbl_3_new lar

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new lar, lar_r0

	LTORG
	
; ------------------- 0F 03 = LSL r16, r/m16 -------------------------
; These are only available while in protected mode!
;
op_0f_03_67
	modrm_jump_32_tbl op_0f_03_67_jump
	modrm_tbl_3_new lsl

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new lsl, lsl_r0

	LTORG
	
; ------------------- 0F A3 = BT r/m16, r16 --------------------------
;
op_0f_a3_67
	modrm_jump_32_tbl op_0f_a3_67_jump
	modrm_tbl_1_new bt_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new bt_r16, bt_r0_r16

	LTORG
	
; ------------------- 0F A4 = SHLD r/m16, r16, imm8 ------------------
; Overflow flag is undefined
op_0f_a4_67
	modrm_jump_32_tbl op_0f_a4_67_jump
	modrm_tbl_1_new shld_r16_imm8

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new shld_r16_imm8, shld_r0_r16_imm8

	LTORG
	
; ------------------- 0F A5 = SHLD r/m16, r16, CL --------------------
; Overflow flag is undefined
op_0f_a5_67
	modrm_jump_32_tbl op_0f_a5_67_jump
	modrm_tbl_1_new shld_r16_CL

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new shld_r16_CL, shld_r0_r16_CL

	LTORG
	
; ------------------- 0F AB = BTS r/m16, r16 -------------------------
;
op_0f_ab_67
	modrm_jump_32_tbl op_0f_ab_67_jump
	modrm_tbl_1_new bts_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new bts_r16, bts_r0_r16

	LTORG
	
; ------------------- 0F AC = SHRD r/m16, r16, imm8 ------------------
; Overflow flag is undefined
op_0f_ac_67
	modrm_jump_32_tbl op_0f_ac_67_jump
	modrm_tbl_1_new shrd_r16_imm8

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new shrd_r16_imm8, shrd_r0_r16_imm8

	LTORG
	
; ------------------- 0F AD = SHRD r/m16, r16, CL ------------------
; Overflow flag is undefined
op_0f_ad_67
	modrm_jump_32_tbl op_0f_ad_67_jump
	modrm_tbl_1_new shrd_r16_CL

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new shrd_r16_CL, shrd_r0_r16_CL

	LTORG
	
; ------------------- 0F AF = IMUL r16, r/m16 ------------------------
op_0f_af_67
	modrm_jump_32_tbl op_0f_af_67_jump
	modrm_tbl_3_new imul_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new imul_r16, imul_r16_r0

	LTORG
	
; ------------------- 0F B2 = LSS r16, r/m16 -------------------------
; 
;
op_0f_b2_67
	modrm_jump_32_tbl op_0f_b2_67_jump
; 0
	modrm_help_3_0 lss_r16, eax, eax
	modrm_help_3_0 lss_r16, ecx, ecx
	modrm_help_3_0 lss_r16, edx, edx
	modrm_help_3_0 lss_r16, ebx, ebx
	modrm_help_3_0 lss_r16, esp, esp
	modrm_help_3_0 lss_r16, ebp, ebp
	modrm_help_3_0 lss_r16, esi, esi
	modrm_help_3_0 lss_r16, edi, edi
; 0x40
	modrm_help_3_40 lss_r16, eax, eax
	modrm_help_3_40 lss_r16, ecx, ecx
	modrm_help_3_40 lss_r16, edx, edx
	modrm_help_3_40 lss_r16, ebx, ebx
	modrm_help_3_40 lss_r16, esp, esp
	modrm_help_3_40 lss_r16, ebp, ebp
	modrm_help_3_40 lss_r16, esi, esi
	modrm_help_3_40 lss_r16, edi, edi
; 0x80
	modrm_help_3_80 lss_r16, eax
	modrm_help_3_80 lss_r16, ecx
	modrm_help_3_80 lss_r16, edx
	modrm_help_3_80 lss_r16, ebx
	modrm_help_3_80 lss_r16, esp
	modrm_help_3_80 lss_r16, ebp
	modrm_help_3_80 lss_r16, esi
	modrm_help_3_80 lss_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new lss_r16, lss_r16_r0

	LTORG
	
; ------------------- 0F B3 = BTR r/m16, r16 -------------------------
;
op_0f_b3_67
	modrm_jump_32_tbl op_0f_b3_67_jump
	modrm_tbl_1_new btr_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new btr_r16, btr_r0_r16

	LTORG
	
; ------------------- 0F B4 = LFS r16, r/m16 -------------------------
; 
;
op_0f_b4_67
	modrm_jump_32_tbl op_0f_b4_67_jump
; 0
	modrm_help_3_0 lfs_r16, eax, eax
	modrm_help_3_0 lfs_r16, ecx, ecx
	modrm_help_3_0 lfs_r16, edx, edx
	modrm_help_3_0 lfs_r16, ebx, ebx
	modrm_help_3_0 lfs_r16, esp, esp
	modrm_help_3_0 lfs_r16, ebp, ebp
	modrm_help_3_0 lfs_r16, esi, esi
	modrm_help_3_0 lfs_r16, edi, edi
; 0x40
	modrm_help_3_40 lfs_r16, eax, eax
	modrm_help_3_40 lfs_r16, ecx, ecx
	modrm_help_3_40 lfs_r16, edx, edx
	modrm_help_3_40 lfs_r16, ebx, ebx
	modrm_help_3_40 lfs_r16, esp, esp
	modrm_help_3_40 lfs_r16, ebp, ebp
	modrm_help_3_40 lfs_r16, esi, esi
	modrm_help_3_40 lfs_r16, edi, edi
; 0x80
	modrm_help_3_80 lfs_r16, eax
	modrm_help_3_80 lfs_r16, ecx
	modrm_help_3_80 lfs_r16, edx
	modrm_help_3_80 lfs_r16, ebx
	modrm_help_3_80 lfs_r16, esp
	modrm_help_3_80 lfs_r16, ebp
	modrm_help_3_80 lfs_r16, esi
	modrm_help_3_80 lfs_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new lfs_r16, lfs_r16_r0

	LTORG
	
; ------------------- 0F B5 = LGS r16, r/m16 -------------------------
; 
;
op_0f_b5_67
	modrm_jump_32_tbl op_0f_b5_67_jump
; 0
	modrm_help_3_0 lgs_r16, eax, eax
	modrm_help_3_0 lgs_r16, ecx, ecx
	modrm_help_3_0 lgs_r16, edx, edx
	modrm_help_3_0 lgs_r16, ebx, ebx
	modrm_help_3_0 lgs_r16, esp, esp
	modrm_help_3_0 lgs_r16, ebp, ebp
	modrm_help_3_0 lgs_r16, esi, esi
	modrm_help_3_0 lgs_r16, edi, edi
; 0x40
	modrm_help_3_40 lgs_r16, eax, eax
	modrm_help_3_40 lgs_r16, ecx, ecx
	modrm_help_3_40 lgs_r16, edx, edx
	modrm_help_3_40 lgs_r16, ebx, ebx
	modrm_help_3_40 lgs_r16, esp, esp
	modrm_help_3_40 lgs_r16, ebp, ebp
	modrm_help_3_40 lgs_r16, esi, esi
	modrm_help_3_40 lgs_r16, edi, edi
; 0x80
	modrm_help_3_80 lgs_r16, eax
	modrm_help_3_80 lgs_r16, ecx
	modrm_help_3_80 lgs_r16, edx
	modrm_help_3_80 lgs_r16, ebx
	modrm_help_3_80 lgs_r16, esp
	modrm_help_3_80 lgs_r16, ebp
	modrm_help_3_80 lgs_r16, esi
	modrm_help_3_80 lgs_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new lgs_r16, lgs_r16_r0

	LTORG
	
; ------------------- 0F B6 = MOVZX r16, r/m8 ------------------------
; 660FB630 = movzx si,[eax] (DOOM, USE32)
;
op_0f_b6_67
	modrm_jump_32_tbl op_0f_b6_67_jump
	modrm_tbl_3_new movzx_r16_b

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new movzx_r16_b, movzx_r16_b_r0

	LTORG
	
; ------------------- 0F BA = BT/BTS/BTR/BTC r/m16, imm8 -------------
; 660FBAE800 = bts  ax,00 (Settlers, PROT, USE32)
;
op_0f_ba_67
	modrm_jump_32_tbl op_0f_ba_67_jump
	modrm_tbl_oper back2, back2, back2, back2, bt_16, bts_16, btr_16, btc_16, imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 bt_16, imm8
	modrm_help_1_C0 bts_16, imm8
	modrm_help_1_C0 btr_16, imm8
	modrm_help_1_C0 btc_16, imm8

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper skip, skip, skip, skip, bt_16, bts_16, btr_16, btc_16, "r0", imm8

	LTORG
	
; ------------------- 0F BB = BTC r/m16, r16 -------------------------
;
op_0f_bb_67
	modrm_jump_32_tbl op_0f_bb_67_jump
	modrm_tbl_1_new btc_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_new btc_r16, btc_r0_r16

	LTORG
	
; ------------------- 0F BC = BSF r16, r/m16 -------------------------
;
op_0f_bc_67
	modrm_jump_32_tbl op_0f_bc_67_jump
	modrm_tbl_3_new bsf_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new bsf_r16, bsf_r16_r0

	LTORG
	
; ------------------- 0F BD = BSR r16, r/m16 -------------------------
;
op_0f_bd_67
	modrm_jump_32_tbl op_0f_bd_67_jump
	modrm_tbl_3_new bsr_r16

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new bsr_r16, bsr_r16_r0

	LTORG
	
; ------------------- 0F BE = MOVSX r16, r/m8 ------------------------
; 660FBE8094E71A00 = movsx ax,[eax+001AE794] (DOOM, USE32)
;
op_0f_be_67
	modrm_jump_32_tbl op_0f_be_67_jump
	modrm_tbl_3_new movsx_r16_b

	AREA cpu_0F_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_new movsx_r16_b, movsx_r16_b_r0

	LTORG
	
; ------------------- 670F = various opcodes --------------------------
;
	GLOBAL	op_0f_67
op_0f_67
	modrm_jump_32_tbl op_0f_67_jump
; 0
	DCD op_0f_00_67, op_0f_01_67, op_0f_02_67, op_0f_03_67, unknown, unknown, op_0f_06, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_0f_20, op_0f_21, op_0f_22, op_0f_23, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	DCD jo_imm16, jno_imm16, jc_imm16, jnc_imm16, jz_imm16, jnz_imm16, jbe_imm16, ja_imm16
	DCD js_imm16, jns_imm16, jpe_imm16, jpo_imm16, jl_imm16, jge_imm16, jle_imm16, jg_imm16
	DCD op_0f_90_USE32, op_0f_91_USE32, op_0f_92_USE32, op_0f_93_USE32, op_0f_94_USE32, op_0f_95_USE32, op_0f_96_USE32, op_0f_97_USE32
	DCD op_0f_98_USE32, op_0f_99_USE32, op_0f_9a_USE32, op_0f_9b_USE32, op_0f_9C_USE32, op_0f_9D_USE32, op_0f_9E_USE32, op_0f_9F_USE32
	DCD push_fs, pop_fs, unknown, op_0f_a3_67, op_0f_a4_67, op_0f_a5_67, unknown, unknown
	DCD push_gs, pop_gs, unknown, op_0f_ab_67, op_0f_ac_67, op_0f_ad_67, unknown, op_0f_af_67
	DCD unknown, unknown, op_0f_b2_67, op_0f_b3_67, op_0f_b4_67, op_0f_b5_67, op_0f_b6_67, op_8b_67
	DCD unknown, unknown, op_0f_ba_67, op_0f_bb_67, op_0f_bc_67, op_0f_bd_67, op_0f_be_67, op_8b_67
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD bswap_eax, bswap_ecx, bswap_edx, bswap_ebx, bswap_esp, bswap_ebp, bswap_esi, bswap_edi
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	END
