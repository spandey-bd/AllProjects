//=============================================================================
// gen_32.inc
//
// This is an include file for "cpu_386.s" and "cpu_0f_USE32.s". This file
// contains macros for creating the MODRM jump tables and effectice address
// calculation routines for when the address and code sizes are both 32 bits
// (USE32 code segment).
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

// --------------------------- r/m8, r8 -------------------------------

.macro modrm_help_0_0 oper reg8
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.word	\oper\()_eax_\reg8
	.word	\oper\()_ecx_\reg8
	.word	\oper\()_edx_\reg8
	.word	\oper\()_bxidx_\reg8		// [ebx],reg	(= oper_bxidx_reg in cpu.S)
	.word	\oper\()_SIB_\reg8
	.word	\oper\()_disp32_\reg8
	.word	\oper\()_siidx_\reg8		// [esi],reg	(= oper_siidx_reg in cpu.S)
	.word	\oper\()_diidx_\reg8		// [edi],reg	(= oper_diidx_reg in cpu.S)
.endif	
.endm
.macro modrm_help_0_40 oper reg8
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.word	\oper\()_eaxd8_\reg8
	.word	\oper\()_ecxd8_\reg8
	.word	\oper\()_edxd8_\reg8
	.word	\oper\()_bxdisp8_\reg8		// [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
	.word	\oper\()_SIBd8_\reg8
	.word	\oper\()_bpdisp8_\reg8		// [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
	.word	\oper\()_sidisp8_\reg8		// [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
	.word	\oper\()_didisp8_\reg8		// [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
.endif	
.endm
.macro modrm_help_0_80 oper reg8
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.word	\oper\()_eaxd32_\reg8
	.word	\oper\()_ecxd32_\reg8
	.word	\oper\()_edxd32_\reg8
	.word	\oper\()_ebxd32_\reg8
	.word	\oper\()_SIBd32_\reg8
	.word	\oper\()_ebpd32_\reg8
	.word	\oper\()_esid32_\reg8
	.word	\oper\()_edid32_\reg8
.endif	
.endm

.macro modrm_tbl_0 oper
// 0
	modrm_help_0_0 \oper al
	modrm_help_0_0 \oper cl
	modrm_help_0_0 \oper dl
	modrm_help_0_0 \oper bl
	modrm_help_0_0 \oper ah
	modrm_help_0_0 \oper ch
	modrm_help_0_0 \oper dh
	modrm_help_0_0 \oper bh
// 0x40
	modrm_help_0_40 \oper al
	modrm_help_0_40 \oper cl
	modrm_help_0_40 \oper dl
	modrm_help_0_40 \oper bl
	modrm_help_0_40 \oper ah
	modrm_help_0_40 \oper ch
	modrm_help_0_40 \oper dh
	modrm_help_0_40 \oper bh
// 0x80
	modrm_help_0_80 \oper al
	modrm_help_0_80 \oper cl
	modrm_help_0_80 \oper dl
	modrm_help_0_80 \oper bl
	modrm_help_0_80 \oper ah
	modrm_help_0_80 \oper ch
	modrm_help_0_80 \oper dh
	modrm_help_0_80 \oper bh
.endm

.macro modrm_tbl_0_oper o0 o1 o2 o3 o4 o5 o6 o7 param
	modrm_help_0_0 \o0 \param
	modrm_help_0_0 \o1 \param
	modrm_help_0_0 \o2 \param
	modrm_help_0_0 \o3 \param
	modrm_help_0_0 \o4 \param
	modrm_help_0_0 \o5 \param
	modrm_help_0_0 \o6 \param
	modrm_help_0_0 \o7 \param
	modrm_help_0_40 \o0 \param
	modrm_help_0_40 \o1 \param
	modrm_help_0_40 \o2 \param
	modrm_help_0_40 \o3 \param
	modrm_help_0_40 \o4 \param
	modrm_help_0_40 \o5 \param
	modrm_help_0_40 \o6 \param
	modrm_help_0_40 \o7 \param
	modrm_help_0_80 \o0 \param
	modrm_help_0_80 \o1 \param
	modrm_help_0_80 \o2 \param
	modrm_help_0_80 \o3 \param
	modrm_help_0_80 \o4 \param
	modrm_help_0_80 \o5 \param
	modrm_help_0_80 \o6 \param
	modrm_help_0_80 \o7 \param
.endm

// -------------------------- r/m32, r32 ------------------------------

.macro modrm_reg_reg_word oper lr rr
	.ifc \rr,
		.word	\oper\()_\lr
	.else
		.word	\oper\()_\lr\()_\rr
	.endif
.endm
.macro modrm_help_1_0 oper reg
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_eaxidx
		.word	\oper\()_ecxidx
		.word	\oper\()_edxidx
		.word	\oper\()_ebxidx
		.word	\oper\()_SIB
		.word	\oper\()_disp32
		.word	\oper\()_esiidx
		.word	\oper\()_ediidx
	.else
		.word	\oper\()_eaxidx_\reg
		.word	\oper\()_ecxidx_\reg
		.word	\oper\()_edxidx_\reg
		.word	\oper\()_ebxidx_\reg
		.word	\oper\()_SIB_\reg
		.word	\oper\()_disp32_\reg
		.word	\oper\()_esiidx_\reg
		.word	\oper\()_ediidx_\reg
	.endif
.endif	
.endm
.macro modrm_help_1_40 oper reg
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_eaxd8
		.word	\oper\()_ecxd8
		.word	\oper\()_edxd8
		.word	\oper\()_ebxd8
		.word	\oper\()_SIBd8
		.word	\oper\()_ebpd8
		.word	\oper\()_esid8
		.word	\oper\()_edid8
	.else
		.word	\oper\()_eaxd8_\reg
		.word	\oper\()_ecxd8_\reg
		.word	\oper\()_edxd8_\reg
		.word	\oper\()_ebxd8_\reg
		.word	\oper\()_SIBd8_\reg
		.word	\oper\()_ebpd8_\reg
		.word	\oper\()_esid8_\reg
		.word	\oper\()_edid8_\reg
	.endif
.endif	
.endm
.macro modrm_help_1_80 oper reg
.ifc \oper, back1
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_eaxd32
		.word	\oper\()_ecxd32
		.word	\oper\()_edxd32
		.word	\oper\()_ebxd32
		.word	\oper\()_SIBd32
		.word	\oper\()_ebpd32
		.word	\oper\()_esid32
		.word	\oper\()_edid32
	.else
		.word	\oper\()_eaxd32_\reg
		.word	\oper\()_ecxd32_\reg
		.word	\oper\()_edxd32_\reg
		.word	\oper\()_ebxd32_\reg
		.word	\oper\()_SIBd32_\reg
		.word	\oper\()_ebpd32_\reg
		.word	\oper\()_esid32_\reg
		.word	\oper\()_edid32_\reg
	.endif
.endif	
.endm
.macro modrm_help_1_C0 oper reg
	modrm_reg_reg_word \oper eax \reg
	modrm_reg_reg_word \oper ecx \reg
	modrm_reg_reg_word \oper edx \reg
	modrm_reg_reg_word \oper ebx \reg
	modrm_reg_reg_word \oper esp \reg
	modrm_reg_reg_word \oper ebp \reg
	modrm_reg_reg_word \oper esi \reg
	modrm_reg_reg_word \oper edi \reg
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

.macro modrm_tbl_1_oper o0 o1 o2 o3 o4 o5 o6 o7 param
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

// --------------------------- r8, r/m8 -------------------------------

.macro modrm_help_2_0 oper reg8
	.word	\oper\()_\reg8\()_eax
	.word	\oper\()_\reg8\()_ecx
	.word	\oper\()_\reg8\()_edx
	.word	\oper\()_\reg8\()_bxidx		// [ebx],reg	(= oper_bxidx_reg in cpu.S)
	.word	\oper\()_\reg8\()_SIB
	.word	\oper\()_\reg8\()_disp32
	.word	\oper\()_\reg8\()_siidx		// [esi],reg	(= oper_siidx_reg in cpu.S)
	.word	\oper\()_\reg8\()_diidx		// [edi],reg	(= oper_diidx_reg in cpu.S)
.endm
.macro modrm_help_2_40 oper reg8
	.word	\oper\()_\reg8\()_eaxd8
	.word	\oper\()_\reg8\()_ecxd8
	.word	\oper\()_\reg8\()_edxd8
	.word	\oper\()_\reg8\()_bxdisp8		// [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
	.word	\oper\()_\reg8\()_SIBd8
	.word	\oper\()_\reg8\()_bpdisp8		// [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
	.word	\oper\()_\reg8\()_sidisp8		// [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
	.word	\oper\()_\reg8\()_didisp8		// [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
.endm
.macro modrm_help_2_80 oper reg8
	.word	\oper\()_\reg8\()_eaxd32
	.word	\oper\()_\reg8\()_ecxd32
	.word	\oper\()_\reg8\()_edxd32
	.word	\oper\()_\reg8\()_ebxd32
	.word	\oper\()_\reg8\()_SIBd32
	.word	\oper\()_\reg8\()_ebpd32
	.word	\oper\()_\reg8\()_esid32
	.word	\oper\()_\reg8\()_edid32
.endm

.macro modrm_tbl_2 oper
// 0
	modrm_help_2_0 \oper al
	modrm_help_2_0 \oper cl
	modrm_help_2_0 \oper dl
	modrm_help_2_0 \oper bl
	modrm_help_2_0 \oper ah
	modrm_help_2_0 \oper ch
	modrm_help_2_0 \oper dh
	modrm_help_2_0 \oper bh
// 0x40
	modrm_help_2_40 \oper al
	modrm_help_2_40 \oper cl
	modrm_help_2_40 \oper dl
	modrm_help_2_40 \oper bl
	modrm_help_2_40 \oper ah
	modrm_help_2_40 \oper ch
	modrm_help_2_40 \oper dh
	modrm_help_2_40 \oper bh
// 0x80
	modrm_help_2_80 \oper al
	modrm_help_2_80 \oper cl
	modrm_help_2_80 \oper dl
	modrm_help_2_80 \oper bl
	modrm_help_2_80 \oper ah
	modrm_help_2_80 \oper ch
	modrm_help_2_80 \oper dh
	modrm_help_2_80 \oper bh
.endm

// -------------------------- r32, r/m32 ------------------------------

.macro modrm_help_3_0 oper reg
	.word	\oper\()_\reg\()_eaxidx
	.word	\oper\()_\reg\()_ecxidx
	.word	\oper\()_\reg\()_edxidx
	.word	\oper\()_\reg\()_ebxidx
	.word	\oper\()_\reg\()_SIB
	.word	\oper\()_\reg\()_disp32
	.word	\oper\()_\reg\()_esiidx
	.word	\oper\()_\reg\()_ediidx
.endm
.macro modrm_help_3_40 oper reg
	.word	\oper\()_\reg\()_eaxd8
	.word	\oper\()_\reg\()_ecxd8
	.word	\oper\()_\reg\()_edxd8
	.word	\oper\()_\reg\()_ebxd8
	.word	\oper\()_\reg\()_SIBd8
	.word	\oper\()_\reg\()_ebpd8
	.word	\oper\()_\reg\()_esid8
	.word	\oper\()_\reg\()_edid8
.endm
.macro modrm_help_3_80 oper reg
	.word	\oper\()_\reg\()_eaxd32
	.word	\oper\()_\reg\()_ecxd32
	.word	\oper\()_\reg\()_edxd32
	.word	\oper\()_\reg\()_ebxd32
	.word	\oper\()_\reg\()_SIBd32
	.word	\oper\()_\reg\()_ebpd32
	.word	\oper\()_\reg\()_esid32
	.word	\oper\()_\reg\()_edid32
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

// --------------------------- r/m8, r8 -------------------------------

.macro modrm_0_help oper jump reg8 reg
.ifc \oper, skip
.else
// ----- 0x00 -----
\oper\()_eax_\reg8:
	mov		r0, eax
	b		\jump\()_\reg
\oper\()_ecx_\reg8:
	mov		r0, ecx
	b		\jump\()_\reg
\oper\()_edx_\reg8:
	mov		r0, edx
	b		\jump\()_\reg
\oper\()_SIB_\reg8:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_disp32_\reg8:
	get_cseip_dword r0
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_eaxd8_\reg8:
	r0_from_idx_disp8 eax
	b		\jump\()_\reg
\oper\()_ecxd8_\reg8:
	r0_from_idx_disp8 ecx
	b		\jump\()_\reg
\oper\()_edxd8_\reg8:
	r0_from_idx_disp8 edx
	b		\jump\()_\reg
\oper\()_SIBd8_\reg8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
// ----- 0x80 = disp32 -----
\oper\()_eaxd32_\reg8:
	r0_from_idx_disp32 eax
	b		\jump\()_\reg
\oper\()_ecxd32_\reg8:
	r0_from_idx_disp32 ecx
	b		\jump\()_\reg
\oper\()_edxd32_\reg8:
	r0_from_idx_disp32 edx
	b		\jump\()_\reg
\oper\()_ebxd32_\reg8:
	r0_from_idx_disp32 ebx
	b		\jump\()_\reg
\oper\()_SIBd32_\reg8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_ebpd32_\reg8:
	r0_from_idx_disp32 ebp
	b		\jump\()_bp_\reg
\oper\()_esid32_\reg8:
	r0_from_idx_disp32 esi
	b		\jump\()_\reg
\oper\()_edid32_\reg8:
	r0_from_idx_disp32 edi
	b		\jump\()_\reg
.endif	
.endm

.macro modrm_0_genall oper
	modrm_0_help \oper \oper\()_r0_r8l al eax
	modrm_0_help \oper \oper\()_r0_r8l cl ecx
	modrm_0_help \oper \oper\()_r0_r8l dl edx
	modrm_0_help \oper \oper\()_r0_r8l bl ebx
	modrm_0_help \oper \oper\()_r0_r8h ah eax
	modrm_0_help \oper \oper\()_r0_r8h ch ecx
	modrm_0_help \oper \oper\()_r0_r8h dh edx
	modrm_0_help \oper \oper\()_r0_r8h bh ebx
	.ltorg
.endm

.macro modrm_genall_0_oper o0 o1 o2 o3 o4 o5 o6 o7 jump param
	modrm_0_help \o0 \o0\jump \param \param
	modrm_0_help \o1 \o1\jump \param \param
	modrm_0_help \o2 \o2\jump \param \param
	modrm_0_help \o3 \o3\jump \param \param
	modrm_0_help \o4 \o4\jump \param \param
	modrm_0_help \o5 \o5\jump \param \param
	modrm_0_help \o6 \o6\jump \param \param
	modrm_0_help \o7 \o7\jump \param \param
	.ltorg
.endm

// -------------------------- r/m32, r32 ------------------------------

.macro modrm_reg_reg_gen oper lr rr
.ifc \rr,
	.global	\oper\()_\lr
\oper\()_\lr:
	\oper\()_reg32_reg32 \lr
.else
	.global	\oper\()_\lr\()_\rr
\oper\()_\lr\()_\rr:
	\oper\()_reg32_reg32 \lr \rr
.endif	
.endm

.macro modrm_1_help oper jump reg
.ifc \oper, skip
.else
.ifc \reg,
// ----- 0x00 -----
	.global	\oper\()_eaxidx
\oper\()_eaxidx:
	mov		r0, eax
	b		\jump
	.global	\oper\()_ecxidx
\oper\()_ecxidx:
	mov		r0, ecx
	b		\jump
	.global	\oper\()_edxidx
\oper\()_edxidx:
	mov		r0, edx
	b		\jump
	.global	\oper\()_ebxidx						// Called from cpu_66.S
\oper\()_ebxidx:
	mov		r0, ebx
	b		\jump
	.global	\oper\()_SIB
\oper\()_SIB:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump							// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
	.global	\oper\()_disp32
\oper\()_disp32:
	get_cseip_dword r0
	b		\jump
	.global	\oper\()_esiidx						// Called from cpu_66.S
\oper\()_esiidx:
	mov		r0, esi
	b		\jump
	.global	\oper\()_ediidx						// Called from cpu_66.S
\oper\()_ediidx:
	mov		r0, edi
	b		\jump
// ----- 0x40 = disp8 -----
	.global	\oper\()_eaxd8
\oper\()_eaxd8:
	r0_from_idx_disp8 eax
	b		\jump
	.global	\oper\()_ecxd8
\oper\()_ecxd8:
	r0_from_idx_disp8 ecx
	b		\jump
	.global	\oper\()_edxd8
\oper\()_edxd8:
	r0_from_idx_disp8 edx
	b		\jump
	.global	\oper\()_ebxd8						// Called from cpu_66.S
\oper\()_ebxd8:
	r0_from_idx_disp8 ebx
	b		\jump
	.global	\oper\()_SIBd8
\oper\()_SIBd8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump							// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
	.global	\oper\()_esid8						// Called from cpu_66.S
\oper\()_ebpd8:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp
\oper\()_esid8:
	r0_from_idx_disp8 esi
	b		\jump
	.global	\oper\()_edid8						// Called from cpu_66.S
\oper\()_edid8:
	r0_from_idx_disp8 edi
	b		\jump
// ----- 0x80 = disp32 -----
	.global	\oper\()_eaxd32
\oper\()_eaxd32:
	r0_from_idx_disp32 eax
	b		\jump
	.global	\oper\()_ecxd32
\oper\()_ecxd32:
	r0_from_idx_disp32 ecx
	b		\jump
	.global	\oper\()_edxd32
\oper\()_edxd32:
	r0_from_idx_disp32 edx
	b		\jump
	.global	\oper\()_ebxd32
\oper\()_ebxd32:
	r0_from_idx_disp32 ebx
	b		\jump
	.global	\oper\()_SIBd32
\oper\()_SIBd32:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump							// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
	.global	\oper\()_ebpd32
\oper\()_ebpd32:
	r0_from_idx_disp32 ebp
	b		\jump\()_bp
	.global	\oper\()_esid32
\oper\()_esid32:
	r0_from_idx_disp32 esi
	b		\jump
	.global	\oper\()_edid32
\oper\()_edid32:
	r0_from_idx_disp32 edi
	b		\jump
.else
// ----- 0x00 -----
\oper\()_eaxidx_\reg:
	mov		r0, eax
	b		\jump\()_\reg
\oper\()_ecxidx_\reg:
	mov		r0, ecx
	b		\jump\()_\reg
\oper\()_edxidx_\reg:
	mov		r0, edx
	b		\jump\()_\reg
	.global	\oper\()_ebxidx_\reg				// Called from cpu_66.S
\oper\()_ebxidx_\reg:
	mov		r0, ebx
	b		\jump\()_\reg
\oper\()_SIB_\reg:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_disp32_\reg:
	get_cseip_dword r0
	b		\jump\()_\reg
	.global	\oper\()_esiidx_\reg				// Called from cpu_66.S
\oper\()_esiidx_\reg:
	mov		r0, esi
	b		\jump\()_\reg
	.global	\oper\()_ediidx_\reg				// Called from cpu_66.S
\oper\()_ediidx_\reg:
	mov		r0, edi
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_eaxd8_\reg:
	r0_from_idx_disp8 eax
	b		\jump\()_\reg
\oper\()_ecxd8_\reg:
	r0_from_idx_disp8 ecx
	b		\jump\()_\reg
\oper\()_edxd8_\reg:
	r0_from_idx_disp8 edx
	b		\jump\()_\reg
	.global	\oper\()_ebxd8_\reg					// Called from cpu_66.S
\oper\()_ebxd8_\reg:
	r0_from_idx_disp8 ebx
	b		\jump\()_\reg
\oper\()_SIBd8_\reg:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
	.global	\oper\()_esid8_\reg					// Called from cpu_66.S
\oper\()_ebpd8_\reg:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp_\reg
\oper\()_esid8_\reg:
	r0_from_idx_disp8 esi
	b		\jump\()_\reg
	.global	\oper\()_edid8_\reg					// Called from cpu_66.S
\oper\()_edid8_\reg:
	r0_from_idx_disp8 edi
	b		\jump\()_\reg
// ----- 0x80 = disp32 -----
\oper\()_eaxd32_\reg:
	r0_from_idx_disp32 eax
	b		\jump\()_\reg
\oper\()_ecxd32_\reg:
	r0_from_idx_disp32 ecx
	b		\jump\()_\reg
\oper\()_edxd32_\reg:
	r0_from_idx_disp32 edx
	b		\jump\()_\reg
\oper\()_ebxd32_\reg:
	r0_from_idx_disp32 ebx
	b		\jump\()_\reg
\oper\()_SIBd32_\reg:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_ebpd32_\reg:
	r0_from_idx_disp32 ebp
	b		\jump\()_bp_\reg
\oper\()_esid32_\reg:
	r0_from_idx_disp32 esi
	b		\jump\()_\reg
\oper\()_edid32_\reg:
	r0_from_idx_disp32 edi
	b		\jump\()_\reg
.endif	
// ----- 0xC0 = registers handled in modrm_3_help -----
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
	.ltorg
.endm

.macro modrm_gen_reg_oper oper reg
	modrm_reg_reg_gen \oper \reg eax
	modrm_reg_reg_gen \oper \reg ecx
	modrm_reg_reg_gen \oper \reg edx
	modrm_reg_reg_gen \oper \reg ebx
	modrm_reg_reg_gen \oper \reg esp
	modrm_reg_reg_gen \oper \reg ebp
	modrm_reg_reg_gen \oper \reg esi
	modrm_reg_reg_gen \oper \reg edi
	.ltorg
.endm

.macro modrm_genall_1_oper o0 o1 o2 o3 o4 o5 o6 o7 jump param
	modrm_1_help \o0 \o0\jump \param
	modrm_1_help \o1 \o1\jump \param
	modrm_1_help \o2 \o2\jump \param
	modrm_1_help \o3 \o3\jump \param
	modrm_1_help \o4 \o4\jump \param
	modrm_1_help \o5 \o5\jump \param
	modrm_1_help \o6 \o6\jump \param
	modrm_1_help \o7 \o7\jump \param
	.ltorg
.endm

// --------------------------- r8, r/m8 -------------------------------

.macro modrm_2_help oper jump reg8 reg
// ----- 0x00 -----
\oper\()_\reg8\()_eax:
	mov		r0, eax
	b		\jump\()_\reg
\oper\()_\reg8\()_ecx:
	mov		r0, ecx
	b		\jump\()_\reg
\oper\()_\reg8\()_edx:
	mov		r0, edx
	b		\jump\()_\reg
#if 0
\oper\()_\reg8\()_ebx:
	mov		r0, ebx
	b		\jump\()_\reg
#endif	
\oper\()_\reg8\()_SIB:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_\reg8\()_disp32:
	get_cseip_dword r0
	b		\jump\()_\reg
#if 0	
\oper\()_\reg8\()_esi:
	mov		r0, esi
	b		\jump\()_\reg
\oper\()_\reg8\()_edi:
	mov		r0, edi
	b		\jump\()_\reg
#endif	
// ----- 0x40 = disp8 -----
\oper\()_\reg8\()_eaxd8:
	r0_from_idx_disp8 eax
	b		\jump\()_\reg
\oper\()_\reg8\()_ecxd8:
	r0_from_idx_disp8 ecx
	b		\jump\()_\reg
\oper\()_\reg8\()_edxd8:
	r0_from_idx_disp8 edx
	b		\jump\()_\reg
\oper\()_\reg8\()_SIBd8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
// ----- 0x80 = disp32 -----
\oper\()_\reg8\()_eaxd32:
	r0_from_idx_disp32 eax
	b		\jump\()_\reg
\oper\()_\reg8\()_ecxd32:
	r0_from_idx_disp32 ecx
	b		\jump\()_\reg
\oper\()_\reg8\()_edxd32:
	r0_from_idx_disp32 edx
	b		\jump\()_\reg
\oper\()_\reg8\()_ebxd32:
	r0_from_idx_disp32 ebx
	b		\jump\()_\reg
\oper\()_\reg8\()_SIBd32:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_\reg8\()_ebpd32:
	r0_from_idx_disp32 ebp
	b		\jump\()_bp_\reg
\oper\()_\reg8\()_esid32:
	r0_from_idx_disp32 esi
	b		\jump\()_\reg
\oper\()_\reg8\()_edid32:
	r0_from_idx_disp32 edi
	b		\jump\()_\reg
.endm

.macro modrm_2_genall oper
	modrm_2_help \oper \oper\()_r8l_r0 al eax
	modrm_2_help \oper \oper\()_r8l_r0 cl ecx
	modrm_2_help \oper \oper\()_r8l_r0 dl edx
	modrm_2_help \oper \oper\()_r8l_r0 bl ebx
	modrm_2_help \oper \oper\()_r8h_r0 ah eax
	modrm_2_help \oper \oper\()_r8h_r0 ch ecx
	modrm_2_help \oper \oper\()_r8h_r0 dh edx
	modrm_2_help \oper \oper\()_r8h_r0 bh ebx
	.ltorg
.endm

// -------------------------- r32, r/m32 ------------------------------

.macro modrm_3_help oper jump reg
// ----- 0x00 -----
\oper\()_\reg\()_eaxidx:
	mov		r0, eax
	b		\jump\()_\reg
\oper\()_\reg\()_ecxidx:
	mov		r0, ecx
	b		\jump\()_\reg
\oper\()_\reg\()_edxidx:
	mov		r0, edx
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_ebxidx				// Called from cpu_66
\oper\()_\reg\()_ebxidx:
	mov		r0, ebx
	b		\jump\()_\reg
\oper\()_\reg\()_SIB:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_\reg\()_disp32:
	get_cseip_dword r0
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_esiidx				// Called from cpu_66
\oper\()_\reg\()_esiidx:
	mov		r0, esi
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_ediidx				// Called from cpu_66
\oper\()_\reg\()_ediidx:
	mov		r0, edi
	b		\jump\()_\reg
// ----- 0x40 = disp8 -----
\oper\()_\reg\()_eaxd8:
	r0_from_idx_disp8 eax
	b		\jump\()_\reg
\oper\()_\reg\()_ecxd8:
	r0_from_idx_disp8 ecx
	b		\jump\()_\reg
\oper\()_\reg\()_edxd8:
	r0_from_idx_disp8 edx
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_ebxd8				// Called from cpu_66
\oper\()_\reg\()_ebxd8:
	r0_from_idx_disp8 ebx
	b		\jump\()_\reg
\oper\()_\reg\()_SIBd8:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_\reg\()_ebpd8:
	r0_from_idx_disp8 ebp
	b		\jump\()_bp_\reg
	.global	\oper\()_\reg\()_esid8				// Called from cpu_66
\oper\()_\reg\()_esid8:
	r0_from_idx_disp8 esi
	b		\jump\()_\reg
	.global	\oper\()_\reg\()_edid8				// Called from cpu_66
\oper\()_\reg\()_edid8:
	r0_from_idx_disp8 edi
	b		\jump\()_\reg
// ----- 0x80 = disp32 -----
\oper\()_\reg\()_eaxd32:
	r0_from_idx_disp32 eax
	b		\jump\()_\reg
\oper\()_\reg\()_ecxd32:
	r0_from_idx_disp32 ecx
	b		\jump\()_\reg
\oper\()_\reg\()_edxd32:
	r0_from_idx_disp32 edx
	b		\jump\()_\reg
\oper\()_\reg\()_ebxd32:
	r0_from_idx_disp32 ebx
	b		\jump\()_\reg
\oper\()_\reg\()_SIBd32:
	ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
\oper\()_\reg\()_ebpd32:
	r0_from_idx_disp32 ebp
	b		\jump\()_bp_\reg
\oper\()_\reg\()_esid32:
	r0_from_idx_disp32 esi
	b		\jump\()_\reg
\oper\()_\reg\()_edid32:
	r0_from_idx_disp32 edi
	b		\jump\()_\reg
// ----- 0xC0 = registers -----
	modrm_gen_reg_oper \oper \reg
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
	.ltorg
.endm
