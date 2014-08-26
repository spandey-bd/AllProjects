//=============================================================================
// gen_16.inc
//
// This is an include file for "cpu_0f.s". This file contains macros for
// creating the MODRM jump tables and effectice address calculation routines
// for when the address and code sizes are both 16 bits (USE16 code segment).
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

// ==================== Jump table generators =========================

.macro modrm_help_1_0 oper reg
	.ifc \reg,
		.word	\oper\()_bxsi
		.word	\oper\()_bxdi
		.word	\oper\()_bpsi
		.word	\oper\()_bpdi
		.word	\oper\()_siidx
		.word	\oper\()_diidx
		.word	\oper\()_disp16
		.word	\oper\()_bxidx
	.else
		.word	\oper\()_bxsi_\reg
		.word	\oper\()_bxdi_\reg
		.word	\oper\()_bpsi_\reg
		.word	\oper\()_bpdi_\reg
		.word	\oper\()_siidx_\reg
		.word	\oper\()_diidx_\reg
		.word	\oper\()_disp16_\reg
		.word	\oper\()_bxidx_\reg
	.endif
.endm
.macro modrm_help_1_40 oper reg
	.ifc \reg,
		.word	\oper\()_bxsid8
		.word	\oper\()_bxdid8
		.word	\oper\()_bpsid8
		.word	\oper\()_bpdid8
		.word	\oper\()_sidisp8
		.word	\oper\()_didisp8
		.word	\oper\()_bpdisp8
		.word	\oper\()_bxdisp8
	.else
		.word	\oper\()_bxsid8_\reg
		.word	\oper\()_bxdid8_\reg
		.word	\oper\()_bpsid8_\reg
		.word	\oper\()_bpdid8_\reg
		.word	\oper\()_sidisp8_\reg
		.word	\oper\()_didisp8_\reg
		.word	\oper\()_bpdisp8_\reg
		.word	\oper\()_bxdisp8_\reg
	.endif
.endm
.macro modrm_help_1_80 oper reg
	.ifc \reg,
		.word	\oper\()_bxsid16
		.word	\oper\()_bxdid16
		.word	\oper\()_bpsid16
		.word	\oper\()_bpdid16
		.word	\oper\()_sidisp16
		.word	\oper\()_didisp16
		.word	\oper\()_bpdisp16
		.word	\oper\()_bxdisp16
	.else
		.word	\oper\()_bxsid16_\reg
		.word	\oper\()_bxdid16_\reg
		.word	\oper\()_bpsid16_\reg
		.word	\oper\()_bpdid16_\reg
		.word	\oper\()_sidisp16_\reg
		.word	\oper\()_didisp16_\reg
		.word	\oper\()_bpdisp16_\reg
		.word	\oper\()_bxdisp16_\reg
	.endif
.endm
.macro modrm_help_1_C0 oper reg
	.ifc \reg,
		.word	\oper\()_ax
		.word	\oper\()_cx
		.word	\oper\()_dx
		.word	\oper\()_bx
		.word	\oper\()_sp
		.word	\oper\()_bp
		.word	\oper\()_si
		.word	\oper\()_di
	.else
		.word	\oper\()_ax_\reg
		.word	\oper\()_cx_\reg
		.word	\oper\()_dx_\reg
		.word	\oper\()_bx_\reg
		.word	\oper\()_sp_\reg
		.word	\oper\()_bp_\reg
		.word	\oper\()_si_\reg
		.word	\oper\()_di_\reg
	.endif
.endm

.macro modrm_tbl_1 oper
// 0
	modrm_help_1_0 \oper eax
	modrm_help_1_0 \oper ecx
	modrm_help_1_0 \oper edx
	modrm_help_1_0 \oper ebx
	modrm_help_1_0 \oper esp
	modrm_help_1_0 \oper ebp
	modrm_help_1_0 \oper esi
	modrm_help_1_0 \oper edi
// 0x40
	modrm_help_1_40 \oper eax
	modrm_help_1_40 \oper ecx
	modrm_help_1_40 \oper edx
	modrm_help_1_40 \oper ebx
	modrm_help_1_40 \oper esp
	modrm_help_1_40 \oper ebp
	modrm_help_1_40 \oper esi
	modrm_help_1_40 \oper edi
// 0x80
	modrm_help_1_80 \oper eax
	modrm_help_1_80 \oper ecx
	modrm_help_1_80 \oper edx
	modrm_help_1_80 \oper ebx
	modrm_help_1_80 \oper esp
	modrm_help_1_80 \oper ebp
	modrm_help_1_80 \oper esi
	modrm_help_1_80 \oper edi
// 0xC0
	modrm_help_1_C0 \oper eax
	modrm_help_1_C0 \oper ecx
	modrm_help_1_C0 \oper edx
	modrm_help_1_C0 \oper ebx
	modrm_help_1_C0 \oper esp
	modrm_help_1_C0 \oper ebp
	modrm_help_1_C0 \oper esi
	modrm_help_1_C0 \oper edi
.endm

.macro modrm_help_3_0 oper reg
	.word	\oper\()_\reg\()_bxsi
	.word	\oper\()_\reg\()_bxdi
	.word	\oper\()_\reg\()_bpsi
	.word	\oper\()_\reg\()_bpdi
	.word	\oper\()_\reg\()_siidx
	.word	\oper\()_\reg\()_diidx
	.word	\oper\()_\reg\()_disp16
	.word	\oper\()_\reg\()_bxidx
.endm
.macro modrm_help_3_40 oper reg
	.word	\oper\()_\reg\()_bxsid8
	.word	\oper\()_\reg\()_bxdid8
	.word	\oper\()_\reg\()_bpsid8
	.word	\oper\()_\reg\()_bpdid8
	.word	\oper\()_\reg\()_sidisp8
	.word	\oper\()_\reg\()_didisp8
	.word	\oper\()_\reg\()_bpdisp8
	.word	\oper\()_\reg\()_bxdisp8
.endm
.macro modrm_help_3_80 oper reg
	.word	\oper\()_\reg\()_bxsid16
	.word	\oper\()_\reg\()_bxdid16
	.word	\oper\()_\reg\()_bpsid16
	.word	\oper\()_\reg\()_bpdid16
	.word	\oper\()_\reg\()_sidisp16
	.word	\oper\()_\reg\()_didisp16
	.word	\oper\()_\reg\()_bpdisp16
	.word	\oper\()_\reg\()_bxdisp16
.endm
.macro modrm_reg_reg_word oper lr rr
	.word	\oper\()_\lr\()_\rr
.endm
.macro modrm_help_3_C0 oper reg
	modrm_reg_reg_word \oper \reg r4
	modrm_reg_reg_word \oper \reg r5
	modrm_reg_reg_word \oper \reg r6
	modrm_reg_reg_word \oper \reg r7
	modrm_reg_reg_word \oper \reg r8
	modrm_reg_reg_word \oper \reg r9
	modrm_reg_reg_word \oper \reg r10
	modrm_reg_reg_word \oper \reg r11
.endm

.macro modrm_tbl_3 oper
// 0
	modrm_help_3_0 \oper r4
	modrm_help_3_0 \oper r5
	modrm_help_3_0 \oper r6
	modrm_help_3_0 \oper r7
	modrm_help_3_0 \oper r8
	modrm_help_3_0 \oper r9
	modrm_help_3_0 \oper r10
	modrm_help_3_0 \oper r11
// 0x40
	modrm_help_3_40 \oper r4
	modrm_help_3_40 \oper r5
	modrm_help_3_40 \oper r6
	modrm_help_3_40 \oper r7
	modrm_help_3_40 \oper r8
	modrm_help_3_40 \oper r9
	modrm_help_3_40 \oper r10
	modrm_help_3_40 \oper r11
// 0x80
	modrm_help_3_80 \oper r4
	modrm_help_3_80 \oper r5
	modrm_help_3_80 \oper r6
	modrm_help_3_80 \oper r7
	modrm_help_3_80 \oper r8
	modrm_help_3_80 \oper r9
	modrm_help_3_80 \oper r10
	modrm_help_3_80 \oper r11
// 0xC0
	modrm_help_3_C0 \oper r4
	modrm_help_3_C0 \oper r5
	modrm_help_3_C0 \oper r6
	modrm_help_3_C0 \oper r7
	modrm_help_3_C0 \oper r8
	modrm_help_3_C0 \oper r9
	modrm_help_3_C0 \oper r10
	modrm_help_3_C0 \oper r11
.endm

// ==================== Code generators ===============================

.macro modrm_1_help oper jump reg
.ifc \oper, skip
.else
	.ifc \reg,
		// ----- 0x00 -----
			.global	\oper\()_bxsi
		\oper\()_bxsi:
			add		r0, r7, r10
			b		\jump
			.global	\oper\()_bxdi
		\oper\()_bxdi:
			add		r0, r7, r11
			b		\jump
			.global	\oper\()_bpsi
		\oper\()_bpsi:
			add		r0, r9, r10
			b		\jump\()_bp
			.global	\oper\()_bpdi
		\oper\()_bpdi:
			add		r0, r9, r11
			b		\jump\()_bp
			.global	\oper\()_siidx
		\oper\()_siidx:
			mov		r0, r10
			b		\jump
			.global	\oper\()_diidx
		\oper\()_diidx:
			mov		r0, r11
			b		\jump
			.global	\oper\()_disp16
		\oper\()_disp16:
			r0_from_disp16
			b		\jump
			.global	\oper\()_bxidx
		\oper\()_bxidx:
			mov		r0, r7
			b		\jump
		// ----- 0x40 = disp8 -----
			.global	\oper\()_bxsid8
		\oper\()_bxsid8:
			r0_from_bxidxdisp8 r10
			b		\jump
			.global	\oper\()_bxdid8
		\oper\()_bxdid8:
			r0_from_bxidxdisp8 r11
			b		\jump
			.global	\oper\()_bpsid8
		\oper\()_bpsid8:
			r0_from_bpidxdisp8 r10
			b		\jump\()_bp
			.global	\oper\()_bpdid8
		\oper\()_bpdid8:
			r0_from_bpidxdisp8 r11
			b		\jump\()_bp
			.global	\oper\()_sidisp8
		\oper\()_sidisp8:
			r0_from_idx_disp8 r10
			b		\jump
			.global	\oper\()_didisp8
		\oper\()_didisp8:
			r0_from_idx_disp8 r11
			b		\jump
			.global	\oper\()_bpdisp8
		\oper\()_bpdisp8:
			r0_from_idx_disp8 r9
			b		\jump\()_bp
			.global	\oper\()_bxdisp8
		\oper\()_bxdisp8:
			r0_from_idx_disp8 r7
			b		\jump
		// ----- 0x80 = disp16 -----
			.global	\oper\()_bxsid16
		\oper\()_bxsid16:
			r0_from_bxidxdisp16 r10
			b		\jump
			.global	\oper\()_bxdid16
		\oper\()_bxdid16:
			r0_from_bxidxdisp16 r11
			b		\jump
			.global	\oper\()_bpsid16
		\oper\()_bpsid16:
			r0_from_bpidxdisp16 r10
			b		\jump\()_bp
			.global	\oper\()_bpdid16
		\oper\()_bpdid16:
			r0_from_bpidxdisp16 r11
			b		\jump\()_bp
			.global	\oper\()_sidisp16
		\oper\()_sidisp16:
			r0_from_idx_disp16 r10
			b		\jump
			.global	\oper\()_didisp16
		\oper\()_didisp16:
			r0_from_idx_disp16 r11
			b		\jump
			.global	\oper\()_bpdisp16
		\oper\()_bpdisp16:
			r0_from_idx_disp16 r9
			b		\jump\()_bp
			.global	\oper\()_bxdisp16
		\oper\()_bxdisp16:
			r0_from_idx_disp16 r7
			b		\jump
	.else
		// ----- 0x00 -----
		\oper\()_bxsi_\reg:
			add		r0, r7, r10
			b		\jump\()_\reg
		\oper\()_bxdi_\reg:
			add		r0, r7, r11
			b		\jump\()_\reg
		\oper\()_bpsi_\reg:
			add		r0, r9, r10
			b		\jump\()_bp_\reg
		\oper\()_bpdi_\reg:
			add		r0, r9, r11
			b		\jump\()_bp_\reg
			.global	\oper\()_siidx_\reg
		\oper\()_siidx_\reg:
			mov		r0, r10
			b		\jump\()_\reg
			.global	\oper\()_diidx_\reg
		\oper\()_diidx_\reg:
			mov		r0, r11
			b		\jump\()_\reg
		\oper\()_disp16_\reg:
			r0_from_disp16
			b		\jump\()_\reg
			.global	\oper\()_bxidx_\reg
		\oper\()_bxidx_\reg:
			mov		r0, r7
			b		\jump\()_\reg
		// ----- 0x40 = disp8 -----
		\oper\()_bxsid8_\reg:
			r0_from_bxidxdisp8 r10
			b		\jump\()_\reg
		\oper\()_bxdid8_\reg:
			r0_from_bxidxdisp8 r11
			b		\jump\()_\reg
		\oper\()_bpsid8_\reg:
			r0_from_bpidxdisp8 r10
			b		\jump\()_bp_\reg
		\oper\()_bpdid8_\reg:
			r0_from_bpidxdisp8 r11
			b		\jump\()_bp_\reg
			.global	\oper\()_sidisp8_\reg
		\oper\()_sidisp8_\reg:
			r0_from_idx_disp8 r10
			b		\jump\()_\reg
			.global	\oper\()_didisp8_\reg
		\oper\()_didisp8_\reg:
			r0_from_idx_disp8 r11
			b		\jump\()_\reg
			.global	\oper\()_bpdisp8_\reg
		\oper\()_bpdisp8_\reg:
			r0_from_idx_disp8 r9
			b		\jump\()_bp_\reg
			.global	\oper\()_bxdisp8_\reg
		\oper\()_bxdisp8_\reg:
			r0_from_idx_disp8 r7
			b		\jump\()_\reg
		// ----- 0x80 = disp16 -----
		\oper\()_bxsid16_\reg:
			r0_from_bxidxdisp16 r10
			b		\jump\()_\reg
		\oper\()_bxdid16_\reg:
			r0_from_bxidxdisp16 r11
			b		\jump\()_\reg
		\oper\()_bpsid16_\reg:
			r0_from_bpidxdisp16 r10
			b		\jump\()_bp_\reg
		\oper\()_bpdid16_\reg:
			r0_from_bpidxdisp16 r11
			b		\jump\()_bp_\reg
		\oper\()_sidisp16_\reg:
			r0_from_idx_disp16 r10
			b		\jump\()_\reg
		\oper\()_didisp16_\reg:
			r0_from_idx_disp16 r11
			b		\jump\()_\reg
		\oper\()_bpdisp16_\reg:
			r0_from_idx_disp16 r9
			b		\jump\()_bp_\reg
		\oper\()_bxdisp16_\reg:
			r0_from_idx_disp16 r7
			b		\jump\()_\reg
	.endif
.endif	
.endm

.macro modrm_reg_reg_gen oper lr rr
	.global	\oper\()_\lr\()_\rr
\oper\()_\lr\()_\rr:
	\oper\()_reg16_reg16 \lr \rr
.endm

.macro modrm_gen_reg_oper oper reg
	modrm_reg_reg_gen \oper \reg r4
	modrm_reg_reg_gen \oper \reg r5
	modrm_reg_reg_gen \oper \reg r6
	modrm_reg_reg_gen \oper \reg r7
	modrm_reg_reg_gen \oper \reg r8
	modrm_reg_reg_gen \oper \reg r9
	modrm_reg_reg_gen \oper \reg r10
	modrm_reg_reg_gen \oper \reg r11
.endm

.macro modrm_1_genall oper jump
	modrm_1_help \oper \jump eax
	modrm_1_help \oper \jump ecx
	modrm_1_help \oper \jump edx
	modrm_1_help \oper \jump ebx
	modrm_1_help \oper \jump esp
	modrm_1_help \oper \jump ebp
	modrm_1_help \oper \jump esi
	modrm_1_help \oper \jump edi
.endm

.macro modrm_3_help oper jump reg
// ----- 0x00 -----
\oper\()_\reg\()_bxsi:
	add		r0, r7, r10
	b		\jump\()_\reg
\oper\()_\reg\()_bxdi:
	add		r0, r7, r11
	b		\jump\()_\reg
\oper\()_\reg\()_bpsi:
	add		r0, r9, r10
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdi:
	add		r0, r9, r11
	b		\jump\()_bp_\reg
	.global	\oper\()_\reg\()_siidx
\oper\()_\reg\()_siidx:
	mov		r0, r10
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_diidx
\oper\()_\reg\()_diidx:
	mov		r0, r11
	b		\jump\()_\reg
\oper\()_\reg\()_disp16:
	r0_from_disp16
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_bxidx
\oper\()_\reg\()_bxidx:
	mov		r0, r7
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_\reg\()_bxsid8:
	r0_from_bxidxdisp8 r10
	b		\jump\()_\reg
\oper\()_\reg\()_bxdid8:
	r0_from_bxidxdisp8 r11
	b		\jump\()_\reg
\oper\()_\reg\()_bpsid8:
	r0_from_bpidxdisp8 r10
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdid8:
	r0_from_bpidxdisp8 r11
	b		\jump\()_bp_\reg
	.global	\oper\()_\reg\()_sidisp8
\oper\()_\reg\()_sidisp8:
	r0_from_idx_disp8 r10
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_didisp8
\oper\()_\reg\()_didisp8:
	r0_from_idx_disp8 r11
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_bpdisp8
\oper\()_\reg\()_bpdisp8:
	r0_from_idx_disp8 r9
	b		\jump\()_bp_\reg
	.global	\oper\()_\reg\()_bxdisp8
\oper\()_\reg\()_bxdisp8:
	r0_from_idx_disp8 r7
	b		\jump\()_\reg
// ----- 0x80 = disp16 -----
\oper\()_\reg\()_bxsid16:
	r0_from_bxidxdisp16 r10
	b		\jump\()_\reg
\oper\()_\reg\()_bxdid16:
	r0_from_bxidxdisp16 r11
	b		\jump\()_\reg
\oper\()_\reg\()_bpsid16:
	r0_from_bpidxdisp16 r10
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdid16:
	r0_from_bpidxdisp16 r11
	b		\jump\()_bp_\reg
\oper\()_\reg\()_sidisp16:
	r0_from_idx_disp16 r10
	b		\jump\()_\reg
\oper\()_\reg\()_didisp16:
	r0_from_idx_disp16 r11
	b		\jump\()_\reg
\oper\()_\reg\()_bpdisp16:
	r0_from_idx_disp16 r9
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bxdisp16:
	r0_from_idx_disp16 r7
	b		\jump\()_\reg
// ----- 0xC0 = registers -----
	modrm_gen_reg_oper \oper \reg
.endm

.macro modrm_3_genall oper jump
	modrm_3_help \oper \jump r4
	modrm_3_help \oper \jump r5
	modrm_3_help \oper \jump r6
	modrm_3_help \oper \jump r7
	modrm_3_help \oper \jump r8
	modrm_3_help \oper \jump r9
	modrm_3_help \oper \jump r10
	modrm_3_help \oper \jump r11
.endm

