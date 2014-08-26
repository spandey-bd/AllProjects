//=============================================================================
// gen_66.inc
//
// This is an include file for "cpu_66.s" and "cpu_0f_66.s". This file contains
// macros for creating the MODRM jump tables and effective address calculation
// routines for when the address size is 16 bits (USE16 code segment) but
// operand size is 32 bits.
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

.macro modrm_reg_reg_word oper lr rr
	.ifc \rr,
		.word	\oper\()_\lr
	.else
		.word	\oper\()_\lr\()_\rr
	.endif
.endm

.macro modrm_help_1_0 oper reg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_bxsi
		.word	\oper\()_bxdi
		.word	\oper\()_bpsi
		.word	\oper\()_bpdi
		.word	\oper\()_esiidx
		.word	\oper\()_ediidx
		.word	\oper\()_disp16
		.word	\oper\()_ebxidx
	.else
		.word	\oper\()_bxsi_\reg
		.word	\oper\()_bxdi_\reg
		.word	\oper\()_bpsi_\reg
		.word	\oper\()_bpdi_\reg
		.word	\oper\()_esiidx_\reg
		.word	\oper\()_ediidx_\reg
		.word	\oper\()_disp16_\reg
		.word	\oper\()_ebxidx_\reg
	.endif
.endif	
.endm
.macro modrm_help_1_40 oper reg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_bxsid8
		.word	\oper\()_bxdid8
		.word	\oper\()_bpsid8
		.word	\oper\()_bpdid8
		.word	\oper\()_esid8
		.word	\oper\()_edid8
		.word	\oper\()_bpdisp8
		.word	\oper\()_ebxd8
	.else
		.word	\oper\()_bxsid8_\reg
		.word	\oper\()_bxdid8_\reg
		.word	\oper\()_bpsid8_\reg
		.word	\oper\()_bpdid8_\reg
		.word	\oper\()_esid8_\reg
		.word	\oper\()_edid8_\reg
		.word	\oper\()_bpdisp8_\reg
		.word	\oper\()_ebxd8_\reg
	.endif
.endif	
.endm
.macro modrm_help_1_80 oper reg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
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
.endif
.endm
.macro modrm_help_1_C0 oper reg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	modrm_reg_reg_word \oper eax \reg
	modrm_reg_reg_word \oper ecx \reg
	modrm_reg_reg_word \oper edx \reg
	modrm_reg_reg_word \oper ebx \reg
	modrm_reg_reg_word \oper esp \reg
	modrm_reg_reg_word \oper ebp \reg
	modrm_reg_reg_word \oper esi \reg
	modrm_reg_reg_word \oper edi \reg
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

.macro modrm_tbl_oper o0 o1 o2 o3 o4 o5 o6 o7 param
	modrm_help_1_0 \o0 \param
	modrm_help_1_0 \o1 \param
	modrm_help_1_0 \o2 \param
	modrm_help_1_0 \o3 \param
	modrm_help_1_0 \o4 \param
	modrm_help_1_0 \o5 \param
	modrm_help_1_0 \o6 \param
	modrm_help_1_0 \o7 \param
	modrm_help_1_40 \o0 \param
	modrm_help_1_40 \o1 \param
	modrm_help_1_40 \o2 \param
	modrm_help_1_40 \o3 \param
	modrm_help_1_40 \o4 \param
	modrm_help_1_40 \o5 \param
	modrm_help_1_40 \o6 \param
	modrm_help_1_40 \o7 \param
	modrm_help_1_80 \o0 \param
	modrm_help_1_80 \o1 \param
	modrm_help_1_80 \o2 \param
	modrm_help_1_80 \o3 \param
	modrm_help_1_80 \o4 \param
	modrm_help_1_80 \o5 \param
	modrm_help_1_80 \o6 \param
	modrm_help_1_80 \o7 \param
.endm

.macro modrm_help_3_0 oper reg
	.word	\oper\()_\reg\()_bxsi
	.word	\oper\()_\reg\()_bxdi
	.word	\oper\()_\reg\()_bpsi
	.word	\oper\()_\reg\()_bpdi
	.word	\oper\()_\reg\()_esiidx		// reg,[esi]	(in cpu_386.S)
	.word	\oper\()_\reg\()_ediidx		// reg,[edi]	(in cpu_386.S)
	.word	\oper\()_\reg\()_disp16
	.word	\oper\()_\reg\()_ebxidx		// reg,[ebx]	(in cpu_386.S)
.endm
.macro modrm_help_3_40 oper reg
	.word	\oper\()_\reg\()_bxsid8
	.word	\oper\()_\reg\()_bxdid8
	.word	\oper\()_\reg\()_bpsid8
	.word	\oper\()_\reg\()_bpdid8
	.word	\oper\()_\reg\()_esid8		// reg,[esi+disp8]
	.word	\oper\()_\reg\()_edid8		// reg,[edi+disp8]
	.word	\oper\()_\reg\()_bpdisp8
	.word	\oper\()_\reg\()_ebxd8		// reg,[ebx+disp8]
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
.macro modrm_help_3_C0 oper reg
	modrm_reg_reg_word \oper \reg eax
	modrm_reg_reg_word \oper \reg ecx
	modrm_reg_reg_word \oper \reg edx
	modrm_reg_reg_word \oper \reg ebx
	modrm_reg_reg_word \oper \reg esp
	modrm_reg_reg_word \oper \reg ebp
	modrm_reg_reg_word \oper \reg esi
	modrm_reg_reg_word \oper \reg edi
.endm

.macro modrm_tbl_3 oper
// 0
	modrm_help_3_0 \oper eax
	modrm_help_3_0 \oper ecx
	modrm_help_3_0 \oper edx
	modrm_help_3_0 \oper ebx
	modrm_help_3_0 \oper esp
	modrm_help_3_0 \oper ebp
	modrm_help_3_0 \oper esi
	modrm_help_3_0 \oper edi
// 0x40
	modrm_help_3_40 \oper eax
	modrm_help_3_40 \oper ecx
	modrm_help_3_40 \oper edx
	modrm_help_3_40 \oper ebx
	modrm_help_3_40 \oper esp
	modrm_help_3_40 \oper ebp
	modrm_help_3_40 \oper esi
	modrm_help_3_40 \oper edi
// 0x80
	modrm_help_3_80 \oper eax
	modrm_help_3_80 \oper ecx
	modrm_help_3_80 \oper edx
	modrm_help_3_80 \oper ebx
	modrm_help_3_80 \oper esp
	modrm_help_3_80 \oper ebp
	modrm_help_3_80 \oper esi
	modrm_help_3_80 \oper edi
// 0xC0
	modrm_help_3_C0 \oper eax
	modrm_help_3_C0 \oper ecx
	modrm_help_3_C0 \oper edx
	modrm_help_3_C0 \oper ebx
	modrm_help_3_C0 \oper esp
	modrm_help_3_C0 \oper ebp
	modrm_help_3_C0 \oper esi
	modrm_help_3_C0 \oper edi
.endm

// ==================== Code generators ===============================

.macro modrm_1_help oper jump reg
.ifc \oper, skip
.else
// ----- 0x00 -----
.ifc \reg,
\oper\()_bxsi:
	add		r0, ebx, esi
	b		\jump
\oper\()_bxdi:
	add		r0, ebx, edi
	b		\jump
\oper\()_bpsi:
	add		r0, ebp, esi
	b		\jump\()_bp
\oper\()_bpdi:
	add		r0, ebp, edi
	b		\jump\()_bp
\oper\()_disp16:
	r0_from_disp16
	b		\jump
// ----- 0x40 = disp8 -----
\oper\()_bxsid8:
	r0_from_bxidxdisp8 esi
	b		\jump
\oper\()_bxdid8:
	r0_from_bxidxdisp8 edi
	b		\jump
\oper\()_bpsid8:
	r0_from_bpidxdisp8 esi
	b		\jump\()_bp
\oper\()_bpdid8:
	r0_from_bpidxdisp8 edi
	b		\jump\()_bp
\oper\()_bpdisp8:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp
// ----- 0x80 = disp16 -----
\oper\()_bxsid16:
	r0_from_bxidxdisp16 esi
	b		\jump
\oper\()_bxdid16:
	r0_from_bxidxdisp16 edi
	b		\jump
\oper\()_bpsid16:
	r0_from_bpidxdisp16 esi
	b		\jump\()_bp
\oper\()_bpdid16:
	r0_from_bpidxdisp16 edi
	b		\jump\()_bp
\oper\()_sidisp16:
	r0_from_idx_disp16 esi
	b		\jump
\oper\()_didisp16:
	r0_from_idx_disp16 edi
	b		\jump
\oper\()_bpdisp16:
	r0_from_idx_disp16 ebp
	b		\jump\()_bp
\oper\()_bxdisp16:
	r0_from_idx_disp16 ebx
	b		\jump
.else
\oper\()_bxsi_\reg:
	add		r0, ebx, esi
	b		\jump\()_\reg
\oper\()_bxdi_\reg:
	add		r0, ebx, edi
	b		\jump\()_\reg
\oper\()_bpsi_\reg:
	add		r0, ebp, esi
	b		\jump\()_bp_\reg
\oper\()_bpdi_\reg:
	add		r0, ebp, edi
	b		\jump\()_bp_\reg
\oper\()_disp16_\reg:
	r0_from_disp16
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_bxsid8_\reg:
	r0_from_bxidxdisp8 esi
	b		\jump\()_\reg
\oper\()_bxdid8_\reg:
	r0_from_bxidxdisp8 edi
	b		\jump\()_\reg
\oper\()_bpsid8_\reg:
	r0_from_bpidxdisp8 esi
	b		\jump\()_bp_\reg
\oper\()_bpdid8_\reg:
	r0_from_bpidxdisp8 edi
	b		\jump\()_bp_\reg
\oper\()_bpdisp8_\reg:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp_\reg
// ----- 0x80 = disp16 -----
\oper\()_bxsid16_\reg:
	r0_from_bxidxdisp16 esi
	b		\jump\()_\reg
\oper\()_bxdid16_\reg:
	r0_from_bxidxdisp16 edi
	b		\jump\()_\reg
\oper\()_bpsid16_\reg:
	r0_from_bpidxdisp16 esi
	b		\jump\()_bp_\reg
\oper\()_bpdid16_\reg:
	r0_from_bpidxdisp16 edi
	b		\jump\()_bp_\reg
\oper\()_sidisp16_\reg:
	r0_from_idx_disp16 esi
	b		\jump\()_\reg
\oper\()_didisp16_\reg:
	r0_from_idx_disp16 edi
	b		\jump\()_\reg
\oper\()_bpdisp16_\reg:
	r0_from_idx_disp16 ebp
	b		\jump\()_bp_\reg
\oper\()_bxdisp16_\reg:
	r0_from_idx_disp16 ebx
	b		\jump\()_\reg
.endif
.endif	
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

.macro modrm_genall_oper o0 o1 o2 o3 o4 o5 o6 o7 jump param
	modrm_1_help \o0 \o0\()_\jump \param
	modrm_1_help \o1 \o1\()_\jump \param
	modrm_1_help \o2 \o2\()_\jump \param
	modrm_1_help \o3 \o3\()_\jump \param
	modrm_1_help \o4 \o4\()_\jump \param
	modrm_1_help \o5 \o5\()_\jump \param
	modrm_1_help \o6 \o6\()_\jump \param
	modrm_1_help \o7 \o7\()_\jump \param
.endm

.macro modrm_3_help oper jump reg
// ----- 0x00 -----
\oper\()_\reg\()_bxsi:
	add		r0, ebx, esi
	b		\jump\()_\reg
\oper\()_\reg\()_bxdi:
	add		r0, ebx, edi
	b		\jump\()_\reg
\oper\()_\reg\()_bpsi:
	add		r0, ebp, esi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdi:
	add		r0, ebp, edi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_disp16:
	r0_from_disp16
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_\reg\()_bxsid8:
	r0_from_bxidxdisp8 esi
	b		\jump\()_\reg
\oper\()_\reg\()_bxdid8:
	r0_from_bxidxdisp8 edi
	b		\jump\()_\reg
\oper\()_\reg\()_bpsid8:
	r0_from_bpidxdisp8 esi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdid8:
	r0_from_bpidxdisp8 edi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdisp8:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp_\reg
// ----- 0x80 = disp16 -----
\oper\()_\reg\()_bxsid16:
	r0_from_bxidxdisp16 esi
	b		\jump\()_\reg
\oper\()_\reg\()_bxdid16:
	r0_from_bxidxdisp16 edi
	b		\jump\()_\reg
\oper\()_\reg\()_bpsid16:
	r0_from_bpidxdisp16 esi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bpdid16:
	r0_from_bpidxdisp16 edi
	b		\jump\()_bp_\reg
\oper\()_\reg\()_sidisp16:
	r0_from_idx_disp16 esi
	b		\jump\()_\reg
\oper\()_\reg\()_didisp16:
	r0_from_idx_disp16 edi
	b		\jump\()_\reg
\oper\()_\reg\()_bpdisp16:
	r0_from_idx_disp16 ebp
	b		\jump\()_bp_\reg
\oper\()_\reg\()_bxdisp16:
	r0_from_idx_disp16 ebx
	b		\jump\()_\reg
.endm

.macro modrm_3_genall oper jump
	modrm_3_help \oper \jump eax
	modrm_3_help \oper \jump ecx
	modrm_3_help \oper \jump edx
	modrm_3_help \oper \jump ebx
	modrm_3_help \oper \jump esp
	modrm_3_help \oper \jump ebp
	modrm_3_help \oper \jump esi
	modrm_3_help \oper \jump edi
.endm

