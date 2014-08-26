//=============================================================================
// gen_67.inc
//
// This is an include file for "cpu_67.s" and "cpu_0f_67.s". This file contains
// macros for creating the MODRM jump tables and effective address calculation
// routines for when the address size is 32 bits (USE32 code segment) but
// operand size is 16 bits.
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

// -------------------------- r/m32, r32 ------------------------------

.macro modrm_help_1_0 oper reg sreg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_eaxidx
		.word	\oper\()_ecxidx
		.word	\oper\()_edxidx
		.word	\oper\()_bxidx		// [ebx],reg	(= oper_bxidx_reg in cpu.S)
		.word	\oper\()_SIB
		.word	\oper\()_disp32
		.word	\oper\()_siidx		// [esi],reg	(= oper_siidx_reg in cpu.S)
		.word	\oper\()_diidx		// [edi],reg	(= oper_diidx_reg in cpu.S)
	.else
		.word	\oper\()_eaxidx_\reg
		.word	\oper\()_ecxidx_\reg
		.word	\oper\()_edxidx_\reg
		.word	\oper\()_bxidx_\sreg		// [ebx],reg	(= oper_bxidx_reg in cpu.S)
		.word	\oper\()_SIB_\reg
		.word	\oper\()_disp32_\reg
		.word	\oper\()_siidx_\sreg		// [esi],reg	(= oper_siidx_reg in cpu.S)
		.word	\oper\()_diidx_\sreg		// [edi],reg	(= oper_diidx_reg in cpu.S)
	.endif	
.endif	
.endm
.macro modrm_help_1_40 oper reg sreg
.ifc \oper, back2
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
.else
	.ifc \reg,
		.word	\oper\()_eaxd8
		.word	\oper\()_ecxd8
		.word	\oper\()_edxd8
		.word	\oper\()_bxdisp8		// [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
		.word	\oper\()_SIBd8
		.word	\oper\()_bpdisp8		// [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
		.word	\oper\()_sidisp8		// [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
		.word	\oper\()_didisp8		// [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
	.else
		.word	\oper\()_eaxd8_\reg
		.word	\oper\()_ecxd8_\reg
		.word	\oper\()_edxd8_\reg
		.word	\oper\()_bxdisp8_\sreg		// [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
		.word	\oper\()_SIBd8_\reg
		.word	\oper\()_bpdisp8_\sreg		// [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
		.word	\oper\()_sidisp8_\sreg		// [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
		.word	\oper\()_didisp8_\sreg		// [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
	.endif
.endif	
.endm
.macro modrm_help_1_80 oper reg
.ifc \oper, back2
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

.macro modrm_tbl_1 oper
// 0
	modrm_help_1_0 \oper eax ax
	modrm_help_1_0 \oper ecx cx
	modrm_help_1_0 \oper edx dx
	modrm_help_1_0 \oper ebx bx
	modrm_help_1_0 \oper esp "sp"
	modrm_help_1_0 \oper ebp bp
	modrm_help_1_0 \oper esi si
	modrm_help_1_0 \oper edi di
// 0x40
	modrm_help_1_40 \oper eax ax
	modrm_help_1_40 \oper ecx cx
	modrm_help_1_40 \oper edx dx
	modrm_help_1_40 \oper ebx bx
	modrm_help_1_40 \oper esp "sp"
	modrm_help_1_40 \oper ebp bp
	modrm_help_1_40 \oper esi si
	modrm_help_1_40 \oper edi di
// 0x80
	modrm_help_1_80 \oper eax
	modrm_help_1_80 \oper ecx
	modrm_help_1_80 \oper edx
	modrm_help_1_80 \oper ebx
	modrm_help_1_80 \oper esp
	modrm_help_1_80 \oper ebp
	modrm_help_1_80 \oper esi
	modrm_help_1_80 \oper edi
.endm

.macro modrm_tbl_oper o0 o1 o2 o3 o4 o5 o6 o7 param
	modrm_help_1_0 \o0 \param \param
	modrm_help_1_0 \o1 \param \param
	modrm_help_1_0 \o2 \param \param
	modrm_help_1_0 \o3 \param \param
	modrm_help_1_0 \o4 \param \param
	modrm_help_1_0 \o5 \param \param
	modrm_help_1_0 \o6 \param \param
	modrm_help_1_0 \o7 \param \param
	modrm_help_1_40 \o0 \param \param
	modrm_help_1_40 \o1 \param \param
	modrm_help_1_40 \o2 \param \param
	modrm_help_1_40 \o3 \param \param
	modrm_help_1_40 \o4 \param \param
	modrm_help_1_40 \o5 \param \param
	modrm_help_1_40 \o6 \param \param
	modrm_help_1_40 \o7 \param \param
	modrm_help_1_80 \o0 \param
	modrm_help_1_80 \o1 \param
	modrm_help_1_80 \o2 \param
	modrm_help_1_80 \o3 \param
	modrm_help_1_80 \o4 \param
	modrm_help_1_80 \o5 \param
	modrm_help_1_80 \o6 \param
	modrm_help_1_80 \o7 \param
.endm

.macro modrm_help_3_0 oper reg sreg
	.word	\oper\()_\reg\()_eax
	.word	\oper\()_\reg\()_ecx
	.word	\oper\()_\reg\()_edx
	.word	\oper\()_\sreg\()_bxidx		// reg,[ebx]	(= oper_reg_bxidx in cpu.S)
	.word	\oper\()_\reg\()_SIB		// reg,SIB
	.word	\oper\()_\reg\()_disp32		// reg,disp32
	.word	\oper\()_\sreg\()_siidx		// reg,[esi]	(in cpu.S)
	.word	\oper\()_\sreg\()_diidx		// reg,[edi]	(in cpu.S)
.endm
.macro modrm_help_3_40 oper reg sreg
	.word	\oper\()_\reg\()_eaxd8
	.word	\oper\()_\reg\()_ecxd8
	.word	\oper\()_\reg\()_edxd8
	.word	\oper\()_\sreg\()_bxdisp8	// reg,[ebx+disp8]	(= oper_reg_bxdisp8 in cpu.S)
	.word	\oper\()_\reg\()_SIBd8
	.word	\oper\()_\sreg\()_bpdisp8	// reg,[ebp+disp8]	(= oper_reg_bpdisp8 in cpu.S)
	.word	\oper\()_\sreg\()_sidisp8	// reg,[esi+disp8]	(= oper_reg_sidisp8 in cpu.S)
	.word	\oper\()_\sreg\()_didisp8	// reg,[edi+disp8]	(= oper_reg_didisp8 in cpu.S)
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

.macro modrm_tbl_3 oper
// 0
	modrm_help_3_0 \oper eax ax
	modrm_help_3_0 \oper ecx cx
	modrm_help_3_0 \oper edx dx
	modrm_help_3_0 \oper ebx bx
	modrm_help_3_0 \oper esp "sp"
	modrm_help_3_0 \oper ebp bp
	modrm_help_3_0 \oper esi si
	modrm_help_3_0 \oper edi di
// 0x40
	modrm_help_3_40 \oper eax ax
	modrm_help_3_40 \oper ecx cx
	modrm_help_3_40 \oper edx dx
	modrm_help_3_40 \oper ebx bx
	modrm_help_3_40 \oper esp "sp"
	modrm_help_3_40 \oper ebp bp
	modrm_help_3_40 \oper esi si
	modrm_help_3_40 \oper edi di
// 0x80
	modrm_help_3_80 \oper eax
	modrm_help_3_80 \oper ecx
	modrm_help_3_80 \oper edx
	modrm_help_3_80 \oper ebx
	modrm_help_3_80 \oper esp
	modrm_help_3_80 \oper ebp
	modrm_help_3_80 \oper esi
	modrm_help_3_80 \oper edi
.endm

// ==================== Code generators ===============================

// -------------------------- r/m32, r32 ------------------------------

.macro modrm_1_help oper jump reg
.ifc \oper, skip
.else
	.ifc \reg,
		// ----- 0x00 -----
		\oper\()_eaxidx:
			mov		r0, eax
			b		\jump\()
		\oper\()_ecxidx:
			mov		r0, ecx
			b		\jump\()
		\oper\()_edxidx:
			mov		r0, edx
			b		\jump\()
		\oper\()_SIB:
			ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
			ldr		r1,=sib_table
			add		r0, r1, r0, lsl #2
			ldr		r1, =\jump\()						// r1 = return address
			ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
		\oper\()_disp32:
			get_cseip_dword r0
			b		\jump\()
		// ----- 0x40 = disp8 -----
		\oper\()_eaxd8:
			r0_from_idx_disp8 eax
			b		\jump\()
		\oper\()_ecxd8:
			r0_from_idx_disp8 ecx
			b		\jump\()
		\oper\()_edxd8:
			r0_from_idx_disp8 edx
			b		\jump\()
		\oper\()_SIBd8:
			ldrb	r0,[r12],#1
			ldr		r1,=sib_disp8_table
			add		r0, r1, r0, lsl #2
			ldr		r1, =\jump\()						// r1 = return address
			ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
		// ----- 0x80 = disp32 -----
		\oper\()_eaxd32:
			r0_from_idx_disp32 eax
			b		\jump\()
		\oper\()_ecxd32:
			r0_from_idx_disp32 ecx
			b		\jump\()
		\oper\()_edxd32:
			r0_from_idx_disp32 edx
			b		\jump\()
		\oper\()_ebxd32:
			r0_from_idx_disp32 ebx
			b		\jump\()
		\oper\()_SIBd32:
			ldrb	r0,[r12],#1
			ldr		r1,=sib_disp32_table
			add		r0, r1, r0, lsl #2
			ldr		r1, =\jump\()						// r1 = return address
			ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
		\oper\()_ebpd32:
			r0_from_idx_disp32 ebp
			b		\jump\()_bp
		\oper\()_esid32:
			r0_from_idx_disp32 esi
			b		\jump\()
		\oper\()_edid32:
			r0_from_idx_disp32 edi
			b		\jump\()
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
		\oper\()_SIB_\reg:
			ldrb	r0,[r12],#1							// Get the SIB byte into r0 register
			ldr		r1,=sib_table
			add		r0, r1, r0, lsl #2
			ldr		r1, =\jump\()_\reg					// r1 = return address
			ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
		\oper\()_disp32_\reg:
			get_cseip_dword r0
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
		\oper\()_SIBd8_\reg:
			ldrb	r0,[r12],#1
			ldr		r1,=sib_disp8_table
			add		r0, r1, r0, lsl #2
			ldr		r1, =\jump\()_\reg					// r1 = return address
			ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
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

.macro modrm_genall_oper o0 o1 o2 o3 o4 o5 o6 o7 jump param
	modrm_1_help \o0 \o0\()_\jump \param
	modrm_1_help \o1 \o1\()_\jump \param
	modrm_1_help \o2 \o2\()_\jump \param
	modrm_1_help \o3 \o3\()_\jump \param
	modrm_1_help \o4 \o4\()_\jump \param
	modrm_1_help \o5 \o5\()_\jump \param
	modrm_1_help \o6 \o6\()_\jump \param
	modrm_1_help \o7 \o7\()_\jump \param
	.ltorg
.endm

.macro modrm_3_help oper jump reg
// ----- 0x00 -----
\oper\()_\reg\()_eax:
	mov		r0, eax
	b		\jump\()_\reg
\oper\()_\reg\()_ecx:
	mov		r0, ecx
	b		\jump\()_\reg
\oper\()_\reg\()_edx:
	mov		r0, edx
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
\oper\()_\reg\()_SIBd8:
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =\jump\()_\reg					// r1 = return address
	ldr		pc, [r0]							// Call the SIB calculation routine, return to r1
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
	ldrb	r0,[r12],#1
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

