;=============================================================================
; gen_32.inc
;
; This is an include file for "cpu_386.s" and "cpu_0f_USE32.s". This file
; contains macros for creating the MODRM jump tables and effectice address
; calculation routines for when the address and code sizes are both 32 bits
; (USE32 code segment).
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http:;dsx86.patrickaalto.com
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

	EXTERN	sib_table
	EXTERN	sib_disp8_table
	EXTERN	sib_disp32_table

; ==================== Jump table generators =========================

; --------------------------- r/m8, r8 -------------------------------

	MACRO
	modrm_help_0_0 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELIF "$reg8" = ""
		DCD	$oper_eax
		DCD	$oper_ecx
		DCD	$oper_edx
		DCD	$oper_bxidx
		DCD	$oper_SIB
		DCD	$oper_disp32
		DCD	$oper_siidx
		DCD	$oper_diidx
	ELSE
		DCD	$oper_eax_$reg8
		DCD	$oper_ecx_$reg8
		DCD	$oper_edx_$reg8
		DCD	$oper_bxidx_$reg8		; [ebx],reg	(= oper_bxidx_reg in cpu.S)
		DCD	$oper_SIB_$reg8
		DCD	$oper_disp32_$reg8
		DCD	$oper_siidx_$reg8		; [esi],reg	(= oper_siidx_reg in cpu.S)
		DCD	$oper_diidx_$reg8		; [edi],reg	(= oper_diidx_reg in cpu.S)
	ENDIF	
	MEND
	MACRO
	modrm_help_0_40 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELIF "$reg8" = ""
		DCD	$oper_eaxd8
		DCD	$oper_ecxd8
		DCD	$oper_edxd8
		DCD	$oper_bxdisp8
		DCD	$oper_SIBd8
		DCD	$oper_bpdisp8
		DCD	$oper_sidisp8
		DCD	$oper_didisp8
	ELSE
		DCD	$oper_eaxd8_$reg8
		DCD	$oper_ecxd8_$reg8
		DCD	$oper_edxd8_$reg8
		DCD	$oper_bxdisp8_$reg8		; [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
		DCD	$oper_SIBd8_$reg8
		DCD	$oper_bpdisp8_$reg8		; [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
		DCD	$oper_sidisp8_$reg8		; [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
		DCD	$oper_didisp8_$reg8		; [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
	ENDIF	
	MEND
	MACRO
	modrm_help_0_80 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELIF "$reg8" = ""
		DCD	$oper_eaxd32
		DCD	$oper_ecxd32
		DCD	$oper_edxd32
		DCD	$oper_ebxd32
		DCD	$oper_SIBd32
		DCD	$oper_ebpd32
		DCD	$oper_esid32
		DCD	$oper_edid32
	ELSE
		DCD	$oper_eaxd32_$reg8
		DCD	$oper_ecxd32_$reg8
		DCD	$oper_edxd32_$reg8
		DCD	$oper_ebxd32_$reg8
		DCD	$oper_SIBd32_$reg8
		DCD	$oper_ebpd32_$reg8
		DCD	$oper_esid32_$reg8
		DCD	$oper_edid32_$reg8
	ENDIF	
	MEND

	MACRO
	modrm_tbl_0 $oper
; 0
	modrm_help_0_0 $oper, al
	modrm_help_0_0 $oper, cl
	modrm_help_0_0 $oper, dl
	modrm_help_0_0 $oper, bl
	modrm_help_0_0 $oper, ah
	modrm_help_0_0 $oper, ch
	modrm_help_0_0 $oper, dh
	modrm_help_0_0 $oper, bh
; 0x40
	modrm_help_0_40 $oper, al
	modrm_help_0_40 $oper, cl
	modrm_help_0_40 $oper, dl
	modrm_help_0_40 $oper, bl
	modrm_help_0_40 $oper, ah
	modrm_help_0_40 $oper, ch
	modrm_help_0_40 $oper, dh
	modrm_help_0_40 $oper, bh
; 0x80
	modrm_help_0_80 $oper, al
	modrm_help_0_80 $oper, cl
	modrm_help_0_80 $oper, dl
	modrm_help_0_80 $oper, bl
	modrm_help_0_80 $oper, ah
	modrm_help_0_80 $oper, ch
	modrm_help_0_80 $oper, dh
	modrm_help_0_80 $oper, bh
	MEND

	MACRO
	modrm_tbl_0_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $param
	modrm_help_0_0 $o0, $param
	modrm_help_0_0 $o1, $param
	modrm_help_0_0 $o2, $param
	modrm_help_0_0 $o3, $param
	modrm_help_0_0 $o4, $param
	modrm_help_0_0 $o5, $param
	modrm_help_0_0 $o6, $param
	modrm_help_0_0 $o7, $param
	modrm_help_0_40 $o0, $param
	modrm_help_0_40 $o1, $param
	modrm_help_0_40 $o2, $param
	modrm_help_0_40 $o3, $param
	modrm_help_0_40 $o4, $param
	modrm_help_0_40 $o5, $param
	modrm_help_0_40 $o6, $param
	modrm_help_0_40 $o7, $param
	modrm_help_0_80 $o0, $param
	modrm_help_0_80 $o1, $param
	modrm_help_0_80 $o2, $param
	modrm_help_0_80 $o3, $param
	modrm_help_0_80 $o4, $param
	modrm_help_0_80 $o5, $param
	modrm_help_0_80 $o6, $param
	modrm_help_0_80 $o7, $param
	MEND

; -------------------------- r/m32, r32 ------------------------------

	MACRO
	modrm_reg_reg_word $oper, $lr, $rr
	IF "$rr" = ""
		DCD	$oper_$lr
	ELIF "$lr" = ""
		DCD	$oper_$rr
	ELSE
		DCD	$oper_$lr_$rr
	ENDIF
	MEND
	MACRO
	modrm_help_1_0 $oper, $reg
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		IF "$reg" = ""
			DCD	$oper_eaxidx
			DCD	$oper_ecxidx
			DCD	$oper_edxidx
			DCD	$oper_ebxidx
			DCD	$oper_SIB
			DCD	$oper_disp32
			DCD	$oper_esiidx
			DCD	$oper_ediidx
		ELSE
			DCD	$oper_eaxidx_$reg
			DCD	$oper_ecxidx_$reg
			DCD	$oper_edxidx_$reg
			DCD	$oper_ebxidx_$reg
			DCD	$oper_SIB_$reg
			DCD	$oper_disp32_$reg
			DCD	$oper_esiidx_$reg
			DCD	$oper_ediidx_$reg
		ENDIF
	ENDIF	
	MEND
	MACRO
	modrm_help_1_40 $oper, $reg
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		IF "$reg" = ""
			DCD	$oper_eaxd8
			DCD	$oper_ecxd8
			DCD	$oper_edxd8
			DCD	$oper_ebxd8
			DCD	$oper_SIBd8
			DCD	$oper_ebpd8
			DCD	$oper_esid8
			DCD	$oper_edid8
		ELSE
			DCD	$oper_eaxd8_$reg
			DCD	$oper_ecxd8_$reg
			DCD	$oper_edxd8_$reg
			DCD	$oper_ebxd8_$reg
			DCD	$oper_SIBd8_$reg
			DCD	$oper_ebpd8_$reg
			DCD	$oper_esid8_$reg
			DCD	$oper_edid8_$reg
		ENDIF
	ENDIF	
	MEND
	MACRO
	modrm_help_1_80 $oper, $reg
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		IF "$reg" = ""
			DCD	$oper_eaxd32
			DCD	$oper_ecxd32
			DCD	$oper_edxd32
			DCD	$oper_ebxd32
			DCD	$oper_SIBd32
			DCD	$oper_ebpd32
			DCD	$oper_esid32
			DCD	$oper_edid32
		ELSE
			DCD	$oper_eaxd32_$reg
			DCD	$oper_ecxd32_$reg
			DCD	$oper_edxd32_$reg
			DCD	$oper_ebxd32_$reg
			DCD	$oper_SIBd32_$reg
			DCD	$oper_ebpd32_$reg
			DCD	$oper_esid32_$reg
			DCD	$oper_edid32_$reg
		ENDIF
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
	modrm_tbl_1 $oper
; 0
	modrm_help_1_0 $oper, eax
	modrm_help_1_0 $oper, ecx
	modrm_help_1_0 $oper, edx
	modrm_help_1_0 $oper, ebx
	modrm_help_1_0 $oper, esp
	modrm_help_1_0 $oper, ebp
	modrm_help_1_0 $oper, esi
	modrm_help_1_0 $oper, edi
; 0x40
	modrm_help_1_40 $oper, eax
	modrm_help_1_40 $oper, ecx
	modrm_help_1_40 $oper, edx
	modrm_help_1_40 $oper, ebx
	modrm_help_1_40 $oper, esp
	modrm_help_1_40 $oper, ebp
	modrm_help_1_40 $oper, esi
	modrm_help_1_40 $oper, edi
; 0x80
	modrm_help_1_80 $oper, eax
	modrm_help_1_80 $oper, ecx
	modrm_help_1_80 $oper, edx
	modrm_help_1_80 $oper, ebx
	modrm_help_1_80 $oper, esp
	modrm_help_1_80 $oper, ebp
	modrm_help_1_80 $oper, esi
	modrm_help_1_80 $oper, edi
; 0xC0
	modrm_help_1_C0 $oper, eax
	modrm_help_1_C0 $oper, ecx
	modrm_help_1_C0 $oper, edx
	modrm_help_1_C0 $oper, ebx
	modrm_help_1_C0 $oper, esp
	modrm_help_1_C0 $oper, ebp
	modrm_help_1_C0 $oper, esi
	modrm_help_1_C0 $oper, edi
	MEND

	MACRO
	modrm_tbl_1_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $param
	modrm_help_1_0 $o0, $param
	modrm_help_1_0 $o1, $param
	modrm_help_1_0 $o2, $param
	modrm_help_1_0 $o3, $param
	modrm_help_1_0 $o4, $param
	modrm_help_1_0 $o5, $param
	modrm_help_1_0 $o6, $param
	modrm_help_1_0 $o7, $param
	modrm_help_1_40 $o0, $param
	modrm_help_1_40 $o1, $param
	modrm_help_1_40 $o2, $param
	modrm_help_1_40 $o3, $param
	modrm_help_1_40 $o4, $param
	modrm_help_1_40 $o5, $param
	modrm_help_1_40 $o6, $param
	modrm_help_1_40 $o7, $param
	modrm_help_1_80 $o0, $param
	modrm_help_1_80 $o1, $param
	modrm_help_1_80 $o2, $param
	modrm_help_1_80 $o3, $param
	modrm_help_1_80 $o4, $param
	modrm_help_1_80 $o5, $param
	modrm_help_1_80 $o6, $param
	modrm_help_1_80 $o7, $param
	MEND

; --------------------------- r8, r/m8 -------------------------------

	MACRO
	modrm_help_2_0 $oper, $reg8
	DCD	$oper_$reg8_eax
	DCD	$oper_$reg8_ecx
	DCD	$oper_$reg8_edx
	DCD	$oper_$reg8_bxidx		; [ebx],reg	(= oper_bxidx_reg in cpu.S)
	DCD	$oper_$reg8_SIB
	DCD	$oper_$reg8_disp32
	DCD	$oper_$reg8_siidx		; [esi],reg	(= oper_siidx_reg in cpu.S)
	DCD	$oper_$reg8_diidx		; [edi],reg	(= oper_diidx_reg in cpu.S)
	MEND
	MACRO
	modrm_help_2_40 $oper, $reg8
	DCD	$oper_$reg8_eaxd8
	DCD	$oper_$reg8_ecxd8
	DCD	$oper_$reg8_edxd8
	DCD	$oper_$reg8_bxdisp8		; [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
	DCD	$oper_$reg8_SIBd8
	DCD	$oper_$reg8_bpdisp8		; [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
	DCD	$oper_$reg8_sidisp8		; [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
	DCD	$oper_$reg8_didisp8		; [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
	MEND
	MACRO
	modrm_help_2_80 $oper, $reg8
	DCD	$oper_$reg8_eaxd32
	DCD	$oper_$reg8_ecxd32
	DCD	$oper_$reg8_edxd32
	DCD	$oper_$reg8_ebxd32
	DCD	$oper_$reg8_SIBd32
	DCD	$oper_$reg8_ebpd32
	DCD	$oper_$reg8_esid32
	DCD	$oper_$reg8_edid32
	MEND

	MACRO
	modrm_tbl_2 $oper
; 0
	modrm_help_2_0 $oper, al
	modrm_help_2_0 $oper, cl
	modrm_help_2_0 $oper, dl
	modrm_help_2_0 $oper, bl
	modrm_help_2_0 $oper, ah
	modrm_help_2_0 $oper, ch
	modrm_help_2_0 $oper, dh
	modrm_help_2_0 $oper, bh
; 0x40
	modrm_help_2_40 $oper, al
	modrm_help_2_40 $oper, cl
	modrm_help_2_40 $oper, dl
	modrm_help_2_40 $oper, bl
	modrm_help_2_40 $oper, ah
	modrm_help_2_40 $oper, ch
	modrm_help_2_40 $oper, dh
	modrm_help_2_40 $oper, bh
; 0x80
	modrm_help_2_80 $oper, al
	modrm_help_2_80 $oper, cl
	modrm_help_2_80 $oper, dl
	modrm_help_2_80 $oper, bl
	modrm_help_2_80 $oper, ah
	modrm_help_2_80 $oper, ch
	modrm_help_2_80 $oper, dh
	modrm_help_2_80 $oper, bh
	MEND

; -------------------------- r32, r/m32 ------------------------------

	MACRO
	modrm_help_3_0 $oper, $reg
	DCD	$oper_$reg_eaxidx
	DCD	$oper_$reg_ecxidx
	DCD	$oper_$reg_edxidx
	DCD	$oper_$reg_ebxidx
	DCD	$oper_$reg_SIB
	DCD	$oper_$reg_disp32
	DCD	$oper_$reg_esiidx
	DCD	$oper_$reg_ediidx
	MEND
	MACRO
	modrm_help_3_40 $oper, $reg
	DCD	$oper_$reg_eaxd8
	DCD	$oper_$reg_ecxd8
	DCD	$oper_$reg_edxd8
	DCD	$oper_$reg_ebxd8
	DCD	$oper_$reg_SIBd8
	DCD	$oper_$reg_ebpd8
	DCD	$oper_$reg_esid8
	DCD	$oper_$reg_edid8
	MEND
	MACRO
	modrm_help_3_80 $oper, $reg
	DCD	$oper_$reg_eaxd32
	DCD	$oper_$reg_ecxd32
	DCD	$oper_$reg_edxd32
	DCD	$oper_$reg_ebxd32
	DCD	$oper_$reg_SIBd32
	DCD	$oper_$reg_ebpd32
	DCD	$oper_$reg_esid32
	DCD	$oper_$reg_edid32
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
	modrm_tbl_3 $oper
; 0
	modrm_help_3_0 $oper, eax
	modrm_help_3_0 $oper, ecx
	modrm_help_3_0 $oper, edx
	modrm_help_3_0 $oper, ebx
	modrm_help_3_0 $oper, esp
	modrm_help_3_0 $oper, ebp
	modrm_help_3_0 $oper, esi
	modrm_help_3_0 $oper, edi
; 0x40
	modrm_help_3_40 $oper, eax
	modrm_help_3_40 $oper, ecx
	modrm_help_3_40 $oper, edx
	modrm_help_3_40 $oper, ebx
	modrm_help_3_40 $oper, esp
	modrm_help_3_40 $oper, ebp
	modrm_help_3_40 $oper, esi
	modrm_help_3_40 $oper, edi
; 0x80
	modrm_help_3_80 $oper, eax
	modrm_help_3_80 $oper, ecx
	modrm_help_3_80 $oper, edx
	modrm_help_3_80 $oper, ebx
	modrm_help_3_80 $oper, esp
	modrm_help_3_80 $oper, ebp
	modrm_help_3_80 $oper, esi
	modrm_help_3_80 $oper, edi
; 0xC0
	modrm_help_3_C0 $oper, eax
	modrm_help_3_C0 $oper, ecx
	modrm_help_3_C0 $oper, edx
	modrm_help_3_C0 $oper, ebx
	modrm_help_3_C0 $oper, esp
	modrm_help_3_C0 $oper, ebp
	modrm_help_3_C0 $oper, esi
	modrm_help_3_C0 $oper, edi
	MEND

; ==================== Code generators ===============================

; --------------------------- r/m8, r8 -------------------------------

	MACRO
	modrm_0_help $oper, $jump, $reg8, $reg
	IF "$oper" = "skip"
	ELIF "$reg" = ""							; ... If reg parameter is empty ...
	EXTERN	$jump
	EXTERN	$jump_bp
; ----- 0x00 -----
$oper_eax
	mov		r0, eax
	b		$jump
$oper_ecx
	mov		r0, ecx
	b		$jump
$oper_edx
	mov		r0, edx
	b		$jump
	EXTERN	$oper_bxidx
$oper_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_disp32
	get_cseip_dword r0
	b		$jump
	EXTERN	$oper_siidx
	EXTERN	$oper_diidx
; ----- 0x40 = disp8 -----
$oper_eaxd8
	r0_from_idx_disp8 eax
	b		$jump
$oper_ecxd8
	r0_from_idx_disp8 ecx
	b		$jump
$oper_edxd8
	r0_from_idx_disp8 edx
	b		$jump
	EXTERN	$oper_bxdisp8
$oper_SIBd8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	EXTERN	$oper_bpdisp8
	EXTERN	$oper_sidisp8
	EXTERN	$oper_didisp8
; ----- 0x80 = disp32 -----
$oper_eaxd32
	r0_from_idx_disp32 eax
	b		$jump
$oper_ecxd32
	r0_from_idx_disp32 ecx
	b		$jump
$oper_edxd32
	r0_from_idx_disp32 edx
	b		$jump
$oper_ebxd32
	r0_from_idx_disp32 ebx
	b		$jump
$oper_SIBd32
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_ebpd32
	r0_from_idx_disp32 ebp
	b		$jump_bp
$oper_esid32
	r0_from_idx_disp32 esi
	b		$jump
$oper_edid32
	r0_from_idx_disp32 edi
	b		$jump
	ELSE									; ... else if reg parameter has value ...
	EXTERN	$jump_$reg
	EXTERN	$jump_bp_$reg
	IF "$reg8" = ""							; ... If reg8 parameter is empty ...
; ----- 0x00 -----
$oper_eax
	mov		r0, eax
	b		$jump_$reg
$oper_ecx
	mov		r0, ecx
	b		$jump_$reg
$oper_edx
	mov		r0, edx
	b		$jump_$reg
	EXTERN	$oper_bxidx
$oper_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_disp32
	get_cseip_dword r0
	b		$jump_$reg
	EXTERN	$oper_siidx
	EXTERN	$oper_diidx
; ----- 0x40 = disp8 -----
$oper_eaxd8
	r0_from_idx_disp8 eax
	b		$jump_$reg
$oper_ecxd8
	r0_from_idx_disp8 ecx
	b		$jump_$reg
$oper_edxd8
	r0_from_idx_disp8 edx
	b		$jump_$reg
	EXTERN	$oper_bxdisp8
$oper_SIBd8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	EXTERN	$oper_bpdisp8
	EXTERN	$oper_sidisp8
	EXTERN	$oper_didisp8
; ----- 0x80 = disp32 -----
$oper_eaxd32
	r0_from_idx_disp32 eax
	b		$jump_$reg
$oper_ecxd32
	r0_from_idx_disp32 ecx
	b		$jump_$reg
$oper_edxd32
	r0_from_idx_disp32 edx
	b		$jump_$reg
$oper_ebxd32
	r0_from_idx_disp32 ebx
	b		$jump_$reg
$oper_SIBd32
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_ebpd32
	r0_from_idx_disp32 ebp
	b		$jump_bp_$reg
$oper_esid32
	r0_from_idx_disp32 esi
	b		$jump_$reg
$oper_edid32
	r0_from_idx_disp32 edi
	b		$jump_$reg
	ELSE									; ... else if we have reg8 paramater ...
; ----- 0x00 -----
$oper_eax_$reg8
	mov		r0, eax
	b		$jump_$reg
$oper_ecx_$reg8
	mov		r0, ecx
	b		$jump_$reg
$oper_edx_$reg8
	mov		r0, edx
	b		$jump_$reg
	EXTERN	$oper_bxidx_$reg8
$oper_SIB_$reg8
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_disp32_$reg8
	get_cseip_dword r0
	b		$jump_$reg
	EXTERN	$oper_siidx_$reg8
	EXTERN	$oper_diidx_$reg8
; ----- 0x40 = disp8 -----
$oper_eaxd8_$reg8
	r0_from_idx_disp8 eax
	b		$jump_$reg
$oper_ecxd8_$reg8
	r0_from_idx_disp8 ecx
	b		$jump_$reg
$oper_edxd8_$reg8
	r0_from_idx_disp8 edx
	b		$jump_$reg
	EXTERN	$oper_bxdisp8_$reg8
$oper_SIBd8_$reg8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	EXTERN	$oper_bpdisp8_$reg8
	EXTERN	$oper_sidisp8_$reg8
	EXTERN	$oper_didisp8_$reg8
; ----- 0x80 = disp32 -----
$oper_eaxd32_$reg8
	r0_from_idx_disp32 eax
	b		$jump_$reg
$oper_ecxd32_$reg8
	r0_from_idx_disp32 ecx
	b		$jump_$reg
$oper_edxd32_$reg8
	r0_from_idx_disp32 edx
	b		$jump_$reg
$oper_ebxd32_$reg8
	r0_from_idx_disp32 ebx
	b		$jump_$reg
$oper_SIBd32_$reg8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_ebpd32_$reg8
	r0_from_idx_disp32 ebp
	b		$jump_bp_$reg
$oper_esid32_$reg8
	r0_from_idx_disp32 esi
	b		$jump_$reg
$oper_edid32_$reg8
	r0_from_idx_disp32 edi
	b		$jump_$reg
	ENDIF
	ENDIF	
	MEND

	MACRO
	modrm_0_genall $oper
	modrm_0_help $oper, $oper_r0_r8l, al, eax
	modrm_0_help $oper, $oper_r0_r8l, cl, ecx
	modrm_0_help $oper, $oper_r0_r8l, dl, edx
	modrm_0_help $oper, $oper_r0_r8l, bl, ebx
	modrm_0_help $oper, $oper_r0_r8h, ah, eax
	modrm_0_help $oper, $oper_r0_r8h, ch, ecx
	modrm_0_help $oper, $oper_r0_r8h, dh, edx
	modrm_0_help $oper, $oper_r0_r8h, bh, ebx
	LTORG
	MEND

	MACRO
	modrm_genall_0_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $jump, $param
	modrm_0_help $o0, $o0$jump, $param, $param
	modrm_0_help $o1, $o1$jump, $param, $param
	modrm_0_help $o2, $o2$jump, $param, $param
	modrm_0_help $o3, $o3$jump, $param, $param
	modrm_0_help $o4, $o4$jump, $param, $param
	modrm_0_help $o5, $o5$jump, $param, $param
	modrm_0_help $o6, $o6$jump, $param, $param
	modrm_0_help $o7, $o7$jump, $param, $param
	LTORG
	MEND

; -------------------------- r/m32, r32 ------------------------------

	MACRO
	modrm_reg_reg_gen $oper, $lr, $rr
	IF "$rr" = ""
	GLOBAL	$oper_$lr
$oper_$lr
	$oper_reg32_reg32 $lr
	ELSE
	GLOBAL	$oper_$lr_$rr
$oper_$lr_$rr
	$oper_reg32_reg32 $lr, $rr
	ENDIF	
	MEND

	MACRO
	modrm_1_help $oper, $jump, $reg
	IF "$oper" = "skip"
	ELSE
	IF "$reg" = ""
; ----- 0x00 -----
	GLOBAL	$oper_eaxidx
$oper_eaxidx
	mov		r0, eax
	b		$jump
	GLOBAL	$oper_ecxidx
$oper_ecxidx
	mov		r0, ecx
	b		$jump
	GLOBAL	$oper_edxidx
$oper_edxidx
	mov		r0, edx
	b		$jump
	GLOBAL	$oper_ebxidx						; Called from cpu_66.S
$oper_ebxidx
	mov		r0, ebx
	b		$jump
	GLOBAL	$oper_SIB
$oper_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	GLOBAL	$oper_disp32
$oper_disp32
	get_cseip_dword r0
	b		$jump
	GLOBAL	$oper_esiidx						; Called from cpu_66.S
$oper_esiidx
	mov		r0, esi
	b		$jump
	GLOBAL	$oper_ediidx						; Called from cpu_66.S
$oper_ediidx
	mov		r0, edi
	b		$jump
; ----- 0x40 = disp8 -----
	GLOBAL	$oper_eaxd8
$oper_eaxd8
	r0_from_idx_disp8 eax
	b		$jump
	GLOBAL	$oper_ecxd8
$oper_ecxd8
	r0_from_idx_disp8 ecx
	b		$jump
	GLOBAL	$oper_edxd8
$oper_edxd8
	r0_from_idx_disp8 edx
	b		$jump
	GLOBAL	$oper_ebxd8						; Called from cpu_66.S
$oper_ebxd8
	r0_from_idx_disp8 ebx
	b		$jump
	GLOBAL	$oper_SIBd8
$oper_SIBd8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	GLOBAL	$oper_esid8						; Called from cpu_66.S
$oper_ebpd8
	r0_from_idx_disp8 ebp
	b		$jump_bp
$oper_esid8
	r0_from_idx_disp8 esi
	b		$jump
	GLOBAL	$oper_edid8						; Called from cpu_66.S
$oper_edid8
	r0_from_idx_disp8 edi
	b		$jump
; ----- 0x80 = disp32 -----
	GLOBAL	$oper_eaxd32
$oper_eaxd32
	r0_from_idx_disp32 eax
	b		$jump
	GLOBAL	$oper_ecxd32
$oper_ecxd32
	r0_from_idx_disp32 ecx
	b		$jump
	GLOBAL	$oper_edxd32
$oper_edxd32
	r0_from_idx_disp32 edx
	b		$jump
	GLOBAL	$oper_ebxd32
$oper_ebxd32
	r0_from_idx_disp32 ebx
	b		$jump
	GLOBAL	$oper_SIBd32
$oper_SIBd32
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump							; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	GLOBAL	$oper_ebpd32
$oper_ebpd32
	r0_from_idx_disp32 ebp
	b		$jump_bp
	GLOBAL	$oper_esid32
$oper_esid32
	r0_from_idx_disp32 esi
	b		$jump
	GLOBAL	$oper_edid32
$oper_edid32
	r0_from_idx_disp32 edi
	b		$jump
	ELSE
; ----- 0x00 -----
$oper_eaxidx_$reg
	mov		r0, eax
	b		$jump_$reg
$oper_ecxidx_$reg
	mov		r0, ecx
	b		$jump_$reg
$oper_edxidx_$reg
	mov		r0, edx
	b		$jump_$reg
	GLOBAL	$oper_ebxidx_$reg				; Called from cpu_66.S
$oper_ebxidx_$reg
	mov		r0, ebx
	b		$jump_$reg
$oper_SIB_$reg
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_disp32_$reg
	get_cseip_dword r0
	b		$jump_$reg
	GLOBAL	$oper_esiidx_$reg				; Called from cpu_66.S
$oper_esiidx_$reg
	mov		r0, esi
	b		$jump_$reg
	GLOBAL	$oper_ediidx_$reg				; Called from cpu_66.S
$oper_ediidx_$reg
	mov		r0, edi
	b		$jump_$reg
; ----- 0x40 = disp8 -----
$oper_eaxd8_$reg
	r0_from_idx_disp8 eax
	b		$jump_$reg
$oper_ecxd8_$reg
	r0_from_idx_disp8 ecx
	b		$jump_$reg
$oper_edxd8_$reg
	r0_from_idx_disp8 edx
	b		$jump_$reg
	GLOBAL	$oper_ebxd8_$reg					; Called from cpu_66.S
$oper_ebxd8_$reg
	r0_from_idx_disp8 ebx
	b		$jump_$reg
$oper_SIBd8_$reg
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	GLOBAL	$oper_esid8_$reg					; Called from cpu_66.S
$oper_ebpd8_$reg
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
$oper_esid8_$reg
	r0_from_idx_disp8 esi
	b		$jump_$reg
	GLOBAL	$oper_edid8_$reg					; Called from cpu_66.S
$oper_edid8_$reg
	r0_from_idx_disp8 edi
	b		$jump_$reg
; ----- 0x80 = disp32 -----
$oper_eaxd32_$reg
	r0_from_idx_disp32 eax
	b		$jump_$reg
$oper_ecxd32_$reg
	r0_from_idx_disp32 ecx
	b		$jump_$reg
$oper_edxd32_$reg
	r0_from_idx_disp32 edx
	b		$jump_$reg
$oper_ebxd32_$reg
	r0_from_idx_disp32 ebx
	b		$jump_$reg
$oper_SIBd32_$reg
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_ebpd32_$reg
	r0_from_idx_disp32 ebp
	b		$jump_bp_$reg
$oper_esid32_$reg
	r0_from_idx_disp32 esi
	b		$jump_$reg
$oper_edid32_$reg
	r0_from_idx_disp32 edi
	b		$jump_$reg
	ENDIF	
; ----- 0xC0 = registers handled in modrm_3_help -----
	ENDIF
	MEND

	MACRO
	modrm_1_genall $oper, $jump
	modrm_1_help $oper, $jump, eax
	modrm_1_help $oper, $jump, ecx
	modrm_1_help $oper, $jump, edx
	modrm_1_help $oper, $jump, ebx
	modrm_1_help $oper, $jump, esp
	modrm_1_help $oper, $jump, ebp
	modrm_1_help $oper, $jump, esi
	modrm_1_help $oper, $jump, edi
	LTORG
	MEND

	MACRO
	modrm_gen_reg_oper $oper, $reg
	modrm_reg_reg_gen $oper, $reg, eax
	modrm_reg_reg_gen $oper, $reg, ecx
	modrm_reg_reg_gen $oper, $reg, edx
	modrm_reg_reg_gen $oper, $reg, ebx
	modrm_reg_reg_gen $oper, $reg, esp
	modrm_reg_reg_gen $oper, $reg, ebp
	modrm_reg_reg_gen $oper, $reg, esi
	modrm_reg_reg_gen $oper, $reg, edi
	LTORG
	MEND

	MACRO
	modrm_genall_1_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $jump, $param
	modrm_1_help $o0, $o0$jump, $param
	modrm_1_help $o1, $o1$jump, $param
	modrm_1_help $o2, $o2$jump, $param
	modrm_1_help $o3, $o3$jump, $param
	modrm_1_help $o4, $o4$jump, $param
	modrm_1_help $o5, $o5$jump, $param
	modrm_1_help $o6, $o6$jump, $param
	modrm_1_help $o7, $o7$jump, $param
	LTORG
	MEND

; --------------------------- r8, r/m8 -------------------------------

	MACRO
	modrm_2_help $oper, $jump, $reg8, $reg
	EXTERN	$jump_$reg
	EXTERN	$jump_bp_$reg
; ----- 0x00 -----
$oper_$reg8_eax
	mov		r0, eax
	b		$jump_$reg
$oper_$reg8_ecx
	mov		r0, ecx
	b		$jump_$reg
$oper_$reg8_edx
	mov		r0, edx
	b		$jump_$reg
	EXTERN	$oper_$reg8_bxidx
$oper_$reg8_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg8_disp32
	get_cseip_dword r0
	b		$jump_$reg
	EXTERN	$oper_$reg8_siidx
	EXTERN	$oper_$reg8_diidx
; ----- 0x40 = disp8 -----
$oper_$reg8_eaxd8
	r0_from_idx_disp8 eax
	b		$jump_$reg
$oper_$reg8_ecxd8
	r0_from_idx_disp8 ecx
	b		$jump_$reg
$oper_$reg8_edxd8
	r0_from_idx_disp8 edx
	b		$jump_$reg
	EXTERN $oper_$reg8_bxdisp8
$oper_$reg8_SIBd8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
	EXTERN $oper_$reg8_bpdisp8
	EXTERN $oper_$reg8_sidisp8
	EXTERN $oper_$reg8_didisp8
; ----- 0x80 = disp32 -----
$oper_$reg8_eaxd32
	r0_from_idx_disp32 eax
	b		$jump_$reg
$oper_$reg8_ecxd32
	r0_from_idx_disp32 ecx
	b		$jump_$reg
$oper_$reg8_edxd32
	r0_from_idx_disp32 edx
	b		$jump_$reg
$oper_$reg8_ebxd32
	r0_from_idx_disp32 ebx
	b		$jump_$reg
$oper_$reg8_SIBd32
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg8_ebpd32
	r0_from_idx_disp32 ebp
	b		$jump_bp_$reg
$oper_$reg8_esid32
	r0_from_idx_disp32 esi
	b		$jump_$reg
$oper_$reg8_edid32
	r0_from_idx_disp32 edi
	b		$jump_$reg
	MEND

	MACRO
	modrm_2_genall $oper
	modrm_2_help $oper, $oper_r8l_r0, al, eax
	modrm_2_help $oper, $oper_r8l_r0, cl, ecx
	modrm_2_help $oper, $oper_r8l_r0, dl, edx
	modrm_2_help $oper, $oper_r8l_r0, bl, ebx
	modrm_2_help $oper, $oper_r8h_r0, ah, eax
	modrm_2_help $oper, $oper_r8h_r0, ch, ecx
	modrm_2_help $oper, $oper_r8h_r0, dh, edx
	modrm_2_help $oper, $oper_r8h_r0, bh, ebx
	LTORG
	MEND

; -------------------------- r32, r/m32 ------------------------------

	MACRO
	modrm_3_help $oper, $jump, $reg
; ----- 0x00 -----
$oper_$reg_eaxidx
	mov		r0, eax
	b		$jump_$reg
$oper_$reg_ecxidx
	mov		r0, ecx
	b		$jump_$reg
$oper_$reg_edxidx
	mov		r0, edx
	b		$jump_$reg
	GLOBAL	$oper_$reg_ebxidx				; Called from cpu_66
$oper_$reg_ebxidx
	mov		r0, ebx
	b		$jump_$reg
$oper_$reg_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg_disp32
	get_cseip_dword r0
	b		$jump_$reg
	GLOBAL	$oper_$reg_esiidx				; Called from cpu_66
$oper_$reg_esiidx
	mov		r0, esi
	b		$jump_$reg
	GLOBAL	$oper_$reg_ediidx				; Called from cpu_66
$oper_$reg_ediidx
	mov		r0, edi
	b		$jump_$reg
; ----- 0x40 = disp8 -----
$oper_$reg_eaxd8
	r0_from_idx_disp8 eax
	b		$jump_$reg
$oper_$reg_ecxd8
	r0_from_idx_disp8 ecx
	b		$jump_$reg
$oper_$reg_edxd8
	r0_from_idx_disp8 edx
	b		$jump_$reg
	GLOBAL	$oper_$reg_ebxd8				; Called from cpu_66
$oper_$reg_ebxd8
	r0_from_idx_disp8 ebx
	b		$jump_$reg
$oper_$reg_SIBd8
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg_ebpd8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	GLOBAL	$oper_$reg_esid8				; Called from cpu_66
$oper_$reg_esid8
	r0_from_idx_disp8 esi
	b		$jump_$reg
	GLOBAL	$oper_$reg_edid8				; Called from cpu_66
$oper_$reg_edid8
	r0_from_idx_disp8 edi
	b		$jump_$reg
; ----- 0x80 = disp32 -----
$oper_$reg_eaxd32
	r0_from_idx_disp32 eax
	b		$jump_$reg
$oper_$reg_ecxd32
	r0_from_idx_disp32 ecx
	b		$jump_$reg
$oper_$reg_edxd32
	r0_from_idx_disp32 edx
	b		$jump_$reg
$oper_$reg_ebxd32
	r0_from_idx_disp32 ebx
	b		$jump_$reg
$oper_$reg_SIBd32
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =$jump_$reg					; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg_ebpd32
	r0_from_idx_disp32 ebp
	b		$jump_bp_$reg
$oper_$reg_esid32
	r0_from_idx_disp32 esi
	b		$jump_$reg
$oper_$reg_edid32
	r0_from_idx_disp32 edi
	b		$jump_$reg
; ----- 0xC0 = registers -----
	modrm_gen_reg_oper $oper, $reg
	MEND

	MACRO
	modrm_3_genall $oper, $jump
	modrm_3_help $oper, $jump, eax
	modrm_3_help $oper, $jump, ecx
	modrm_3_help $oper, $jump, edx
	modrm_3_help $oper, $jump, ebx
	modrm_3_help $oper, $jump, esp
	modrm_3_help $oper, $jump, ebp
	modrm_3_help $oper, $jump, esi
	modrm_3_help $oper, $jump, edi
	LTORG
	MEND

	END
