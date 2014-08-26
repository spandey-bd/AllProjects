//=============================================================================
// cpu_0F_66.s
//
// This file contains opcode handlers for the 0x0F-prefixed opcodes, for 16-bit
// address size and 32-bit operand size.
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
	.file	"cpu_0F_66.s"

#include "defines.inc"
#include "macros.inc"
#include "gen_66.inc"

    .text
    .align  2
    .code   32
	.arm

// ------------------- 0F 00 = SLDT/STR/LLDT/LTR/VERR/VERW r/m16 ------
// 0F00D0 = lldt ax (DOS4GW, AX=0068)
//
	.global	op_0f_00_66
op_0f_00_66:
	//-------
	// TODO! Only available in protected mode!
	//-------
	modrm_jump_16
// 0
	modrm_help_1_0 sldt
	modrm_help_1_0 str
	modrm_help_1_0 lldt
	modrm_help_1_0 ltr
	modrm_help_1_0 verr
	modrm_help_1_0 verw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
//0x40
	modrm_help_1_40 sldt
	modrm_help_1_40 str
	modrm_help_1_40 lldt
	modrm_help_1_40 ltr
	modrm_help_1_40 verr
	modrm_help_1_40 verw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
//0x80
	modrm_help_1_80 sldt
	modrm_help_1_80 str
	modrm_help_1_80 lldt
	modrm_help_1_80 ltr
	modrm_help_1_80 verr
	modrm_help_1_80 verw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
//0xc0 = mod = 11b => register operand
	modrm_help_1_C0 sldt
	modrm_help_1_C0 str
	modrm_help_1_C0 lldt
	modrm_help_1_C0 ltr
	modrm_help_1_C0 verr
	modrm_help_1_C0 verw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

// Note! Actual handlers are in "cpu_0F.s" (for memory) and in "cpu_0F_USE32.s" (sldt and str register versions)

// ------------------- 0F 01 = SGDT/SIDT/LGDT/LIDT/SMSW r/m16 ---------
// 660F01167409 = lgdt dword [0974] (DOS4GW)			(Prot mode entry at 0511:031F)
//
	.global	op_0f_01_66
op_0f_01_66:
	modrm_jump_16
	modrm_tbl_oper sgdt sidt lgdt_dword lidt_dword smsw back2 lmsw back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_1_C0 smsw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_1_C0 lmsw
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	
	modrm_genall_oper sgdt sidt lgdt_dword lidt_dword smsw skip lmsw skip "r0"

// ------------------- 0F 02 = LAR r32, r/m32 -------------------------
// These are only available while in protected mode!
//
op_0f_02_66:
	modrm_jump_16
	modrm_tbl_3 lar_r32
	
	modrm_3_genall lar_r32 lar_r32_r0

// ------------------- 0F 03 = LSL r32, r/m32 -------------------------
// These are only available while in protected mode!
//
op_0f_03_66:
	modrm_jump_16
	modrm_tbl_3 lsl_r32
	
	modrm_3_genall lsl_r32 lsl_r32_r0

// ------------------- 0F A3 = BT r/m32, r32 --------------------------
// 660FA3F0 = bt eax,esi (REAL, USE16)
	.global	op_0f_a3_66
op_0f_a3_66:
	modrm_jump_16
	modrm_tbl_1 bt_r32
	modrm_1_genall bt_r32 bt_r0_r32

// ------------------- 0F A4 = SHLD r/m32, r32, imm8 ------------------
// 660FA4C210 = shld edx,eax,10 (RIPTIDE, REAL, USE16)
op_0f_a4_66:
	modrm_jump_16
	modrm_tbl_1 shld_r32_imm8
	modrm_1_genall shld_r32_imm8 shld_r0_r32_imm8

// ------------------- 0F A5 = SHLD r/m32, r32, CL --------------------
//
op_0f_a5_66:
	modrm_jump_16
	modrm_tbl_1 shld_r32_CL
	modrm_1_genall shld_r32_CL shld_r0_r32_CL

// ------------------- 0F AB = BTS r/m32, r32 -------------------------
// 66260FAB84A200 = bts  es:[si+00A2],eax (FRONTIER, REAL, USE16)
op_0f_ab_66:
	modrm_jump_16
	modrm_tbl_1 bts_r32
	modrm_1_genall bts_r32 bts_r0_r32

// ------------------- 0F AC = SHRD r/m32, r32, imm8 ------------------
//
op_0f_ac_66:
	modrm_jump_16
	modrm_tbl_1 shrd_r32_imm8
	modrm_1_genall shrd_r32_imm8 shrd_r0_r32_imm8

// ------------------- 0F AD = SHRD r/m32, r32, CL ------------------
//
op_0f_ad_66:
	modrm_jump_16
	modrm_tbl_1 shrd_r32_CL
	modrm_1_genall shrd_r32_CL shrd_r0_r32_CL

// ------------------- 0F AF = IMUL r32, r/m32 ------------------------
// 660FAFD1 = imul edx,ecx (hexxtrnr)
op_0f_af_66:
	modrm_jump_16
	modrm_tbl_3 imul_r32
	modrm_3_genall imul_r32 imul_r32_r0

// ------------------- 0F B2 = LSS r32, r/m32 -------------------------
// 662E0FB226824E  lss  dword cs:[4E82] (Loadlin, REAL)
//
op_0f_b2_66:
	modrm_jump_16
// 0
	modrm_help_3_0 lss_r32 eax
	modrm_help_3_0 lss_r32 ecx
	modrm_help_3_0 lss_r32 edx
	modrm_help_3_0 lss_r32 ebx
	modrm_help_3_0 lss_r32 esp
	modrm_help_3_0 lss_r32 ebp
	modrm_help_3_0 lss_r32 esi
	modrm_help_3_0 lss_r32 edi
// 0x40
	modrm_help_3_40 lss_r32 eax
	modrm_help_3_40 lss_r32 ecx
	modrm_help_3_40 lss_r32 edx
	modrm_help_3_40 lss_r32 ebx
	modrm_help_3_40 lss_r32 esp
	modrm_help_3_40 lss_r32 ebp
	modrm_help_3_40 lss_r32 esi
	modrm_help_3_40 lss_r32 edi
// 0x80
	modrm_help_3_80 lss_r32 eax
	modrm_help_3_80 lss_r32 ecx
	modrm_help_3_80 lss_r32 edx
	modrm_help_3_80 lss_r32 ebx
	modrm_help_3_80 lss_r32 esp
	modrm_help_3_80 lss_r32 ebp
	modrm_help_3_80 lss_r32 esi
	modrm_help_3_80 lss_r32 edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_3_genall lss_r32 lss_r32_r0

// ------------------- 0F B3 = BTR r/m32, r32 -------------------------
//
op_0f_b3_66:
	modrm_jump_16
	modrm_tbl_1 btr_r32
	modrm_1_genall btr_r32 btr_r0_r32

// ------------------- 0F B4 = LFS r32, r/m32 -------------------------
//
op_0f_b4_66:
	modrm_jump_16
// 0
	modrm_help_3_0 lfs_r32 eax
	modrm_help_3_0 lfs_r32 ecx
	modrm_help_3_0 lfs_r32 edx
	modrm_help_3_0 lfs_r32 ebx
	modrm_help_3_0 lfs_r32 esp
	modrm_help_3_0 lfs_r32 ebp
	modrm_help_3_0 lfs_r32 esi
	modrm_help_3_0 lfs_r32 edi
// 0x40
	modrm_help_3_40 lfs_r32 eax
	modrm_help_3_40 lfs_r32 ecx
	modrm_help_3_40 lfs_r32 edx
	modrm_help_3_40 lfs_r32 ebx
	modrm_help_3_40 lfs_r32 esp
	modrm_help_3_40 lfs_r32 ebp
	modrm_help_3_40 lfs_r32 esi
	modrm_help_3_40 lfs_r32 edi
// 0x80
	modrm_help_3_80 lfs_r32 eax
	modrm_help_3_80 lfs_r32 ecx
	modrm_help_3_80 lfs_r32 edx
	modrm_help_3_80 lfs_r32 ebx
	modrm_help_3_80 lfs_r32 esp
	modrm_help_3_80 lfs_r32 ebp
	modrm_help_3_80 lfs_r32 esi
	modrm_help_3_80 lfs_r32 edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_3_genall lfs_r32 lfs_r32_r0

// ------------------- 0F B5 = LGSS r32, r/m32 -------------------------
//
op_0f_b5_66:
	modrm_jump_16
// 0
	modrm_help_3_0 lgs_r32 eax
	modrm_help_3_0 lgs_r32 ecx
	modrm_help_3_0 lgs_r32 edx
	modrm_help_3_0 lgs_r32 ebx
	modrm_help_3_0 lgs_r32 esp
	modrm_help_3_0 lgs_r32 ebp
	modrm_help_3_0 lgs_r32 esi
	modrm_help_3_0 lgs_r32 edi
// 0x40
	modrm_help_3_40 lgs_r32 eax
	modrm_help_3_40 lgs_r32 ecx
	modrm_help_3_40 lgs_r32 edx
	modrm_help_3_40 lgs_r32 ebx
	modrm_help_3_40 lgs_r32 esp
	modrm_help_3_40 lgs_r32 ebp
	modrm_help_3_40 lgs_r32 esi
	modrm_help_3_40 lgs_r32 edi
// 0x80
	modrm_help_3_80 lgs_r32 eax
	modrm_help_3_80 lgs_r32 ecx
	modrm_help_3_80 lgs_r32 edx
	modrm_help_3_80 lgs_r32 ebx
	modrm_help_3_80 lgs_r32 esp
	modrm_help_3_80 lgs_r32 ebp
	modrm_help_3_80 lgs_r32 esi
	modrm_help_3_80 lgs_r32 edi
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_3_genall lgs_r32 lgs_r32_r0

// ------------------- 0F B6 = MOVZX r32, r/m8 ------------------------
// 660FB636 = movzx esi, byte ptr [1242]
//
op_0f_b6_66:
	modrm_jump_16
	modrm_tbl_3 movzx_r32_b
	modrm_3_genall movzx_r32_b movzx_r32_b_r0

// ------------------- 0F B7 = MOVZX r32, r/m16 -----------------------
// 66260FB7060200 = movzx eax, word ptr es:[0002]
//
op_0f_b7_66:
	modrm_jump_16
	modrm_tbl_3 movzx_r32_w
	modrm_3_genall movzx_r32_w movzx_r32_w_r0

// ------------------- 0F BA = BT/BTS/BTR/BTC r/m32, imm8 -------------
// 660FBAFA15 = btc edx,15 (DOS4GW)
//
op_0f_ba_66:
	modrm_jump_16
	modrm_tbl_oper back2 back2 back2 back2 bt_32 bts_32 btr_32 btc_32 imm8
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	modrm_help_1_C0 bt_32 imm8
	modrm_help_1_C0 bts_32 imm8
	modrm_help_1_C0 btr_32 imm8
	modrm_help_1_C0 btc_32 imm8

	modrm_genall_oper skip skip skip skip bt_32 bts_32 btr_32 btc_32 "r0" imm8

// ------------------- 0F BB = BTC r/m32, r32 -------------------------
//
op_0f_bb_66:
	modrm_jump_16
	modrm_tbl_1 btc_r32
	modrm_1_genall btc_r32 btc_r0_r32

// ------------------- 0F BC = BSF r32, r/m32 -------------------------
//
op_0f_bc_66:
	modrm_jump_16
	modrm_tbl_3 bsf_r32
	modrm_3_genall bsf_r32 bsf_r32_r0

// ------------------- 0F BD = BSR r32, r/m32 -------------------------
//
op_0f_bd_66:
	modrm_jump_16
	modrm_tbl_3 bsr_r32
	modrm_3_genall bsr_r32 bsr_r32_r0

// ------------------- 0F BE = MOVSX r32, r/m8 ------------------------
op_0f_be_66:
	modrm_jump_16
	modrm_tbl_3 movsx_r32_b
	modrm_3_genall movsx_r32_b movsx_r32_b_r0

// ------------------- 0F BF = MOVSX r32, r/m16 -----------------------
// 660FBF56FC = movsx edx,[bp-04] (RIPTIDE, REAL, USE16)
op_0f_bf_66:
	modrm_jump_16
	modrm_tbl_3 movsx_r32_w
	modrm_3_genall movsx_r32_w movsx_r32_w_r0

// ------------------- 660F = various opcodes -------------------------
//
	.global	op_0f_66
op_0f_66:
	modrm_jump_16
// 0
	.word op_0f_00_66, op_0f_01_66, op_0f_02_66, op_0f_03_66, .unknown, .unknown, op_0f_06, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word op_0f_20, op_0f_21, op_0f_22, op_0f_23, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x40
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
// 0x80
	.word jo_imm32, jno_imm32, jc_imm32, jnc_imm32, jz_imm32, jnz_imm32, jbe_imm32, ja_imm32
	.word js_imm32, jns_imm32, jp_imm32, jnp_imm32, jl_imm32, jge_imm32, jle_imm32, jg_imm32
	.word op_0f_90, op_0f_91, op_0f_92, op_0f_93, op_0f_94, op_0f_95, op_0f_96, op_0f_97
	.word op_0f_98, op_0f_99, op_0f_9a, op_0f_9b, op_0f_9C, op_0f_9D, op_0f_9E, op_0f_9F
	.word push_fs_USE32, pop_fs_USE32, .unknown, op_0f_a3_66, op_0f_a4_66, op_0f_a5_66, .unknown, .unknown
	.word push_gs_USE32, pop_gs_USE32, .unknown, op_0f_ab_66, op_0f_ac_66, op_0f_ad_66, .unknown, op_0f_af_66
	.word .unknown, .unknown, op_0f_b2_66, op_0f_b3_66, op_0f_b4_66, op_0f_b5_66, op_0f_b6_66, op_0f_b7_66
	.word .unknown, .unknown, op_0f_ba_66, op_0f_bb_66, op_0f_bc_66, op_0f_bd_66, op_0f_be_66, op_0f_bf_66
// 0xC0
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word bswap_eax, bswap_ecx, bswap_edx, bswap_ebx, bswap_esp, bswap_ebp, bswap_esi, bswap_edi
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

