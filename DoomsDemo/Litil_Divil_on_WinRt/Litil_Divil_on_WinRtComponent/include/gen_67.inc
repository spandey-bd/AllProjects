;=============================================================================
; gen_67.inc
;
; This is an include file for "cpu_67.s" and "cpu_0f_67.s". This file contains
; macros for creating the MODRM jump tables and effective address calculation
; routines for when the address size is 32 bits (USE32 code segment) but
; operand size is 16 bits.
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


	EXTERN	sib_table
	EXTERN	sib_disp8_table
	EXTERN	sib_disp32_table

; ==================== Jump table generators =========================

; -------------------------- r/m32, r32 ------------------------------

	MACRO 
	modrm_help_1_0 $oper, $reg, $sreg
	IF "$oper" = "back2"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		IF "$reg" = ""
			DCD	$oper_eaxidx
			DCD	$oper_ecxidx
			DCD	$oper_edxidx
			DCD	$oper_bxidx		; [ebx],reg	(= oper_bxidx_reg in cpu.S)
			DCD	$oper_SIB
			DCD	$oper_disp32
			DCD	$oper_siidx		; [esi],reg	(= oper_siidx_reg in cpu.S)
			DCD	$oper_diidx		; [edi],reg	(= oper_diidx_reg in cpu.S)
		ELSE
			DCD	$oper_eaxidx_$reg
			DCD	$oper_ecxidx_$reg
			DCD	$oper_edxidx_$reg
			DCD	$oper_bxidx_$sreg		; [ebx],reg	(= oper_bxidx_reg in cpu.S)
			DCD	$oper_SIB_$reg
			DCD	$oper_disp32_$reg
			DCD	$oper_siidx_$sreg		; [esi],reg	(= oper_siidx_reg in cpu.S)
			DCD	$oper_diidx_$sreg		; [edi],reg	(= oper_diidx_reg in cpu.S)
		ENDIF	
	ENDIF	
	MEND
	MACRO
	modrm_help_1_40 $oper, $reg, $sreg
	IF "$oper" = "back2"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		IF "$reg" = ""
			DCD	$oper_eaxd8
			DCD	$oper_ecxd8
			DCD	$oper_edxd8
			DCD	$oper_bxdisp8		; [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
			DCD	$oper_SIBd8
			DCD	$oper_bpdisp8		; [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
			DCD	$oper_sidisp8		; [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
			DCD	$oper_didisp8		; [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
		ELSE
			DCD	$oper_eaxd8_$reg
			DCD	$oper_ecxd8_$reg
			DCD	$oper_edxd8_$reg
			DCD	$oper_bxdisp8_$sreg		; [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
			DCD	$oper_SIBd8_$reg
			DCD	$oper_bpdisp8_$sreg		; [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
			DCD	$oper_sidisp8_$sreg		; [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
			DCD	$oper_didisp8_$sreg		; [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
		ENDIF
	ENDIF	
	MEND
	MACRO
	modrm_help_1_80 $oper, $reg
	IF "$oper" = "back2"
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
	modrm_tbl_1_old $oper
		modrm_help_1_0 $oper, eax, eax
		modrm_help_1_0 $oper, ecx, ecx
		modrm_help_1_0 $oper, edx, edx
		modrm_help_1_0 $oper, ebx, ebx
		modrm_help_1_0 $oper, esp, "esp"
		modrm_help_1_0 $oper, ebp, ebp
		modrm_help_1_0 $oper, esi, esi
		modrm_help_1_0 $oper, edi, edi
		modrm_help_1_40 $oper, eax, eax
		modrm_help_1_40 $oper, ecx, ecx
		modrm_help_1_40 $oper, edx, edx
		modrm_help_1_40 $oper, ebx, ebx
		modrm_help_1_40 $oper, esp, "esp"
		modrm_help_1_40 $oper, ebp, ebp
		modrm_help_1_40 $oper, esi, esi
		modrm_help_1_40 $oper, edi, edi
		modrm_help_1_80 $oper, eax
		modrm_help_1_80 $oper, ecx
		modrm_help_1_80 $oper, edx
		modrm_help_1_80 $oper, ebx
		modrm_help_1_80 $oper, esp
		modrm_help_1_80 $oper, ebp
		modrm_help_1_80 $oper, esi
		modrm_help_1_80 $oper, edi
	MEND
	MACRO
	modrm_tbl_1_new $oper
		modrm_help_1_0 $oper, eax, eax
		modrm_help_1_0 $oper, ecx, ecx
		modrm_help_1_0 $oper, edx, edx
		modrm_help_1_0 $oper, ebx, ebx
		modrm_help_1_0 $oper, esp, esp
		modrm_help_1_0 $oper, ebp, ebp
		modrm_help_1_0 $oper, esi, esi
		modrm_help_1_0 $oper, edi, edi
		modrm_help_1_40 $oper, eax, eax
		modrm_help_1_40 $oper, ecx, ecx
		modrm_help_1_40 $oper, edx, edx
		modrm_help_1_40 $oper, ebx, ebx
		modrm_help_1_40 $oper, esp, esp
		modrm_help_1_40 $oper, ebp, ebp
		modrm_help_1_40 $oper, esi, esi
		modrm_help_1_40 $oper, edi, edi
		modrm_help_1_80 $oper, eax
		modrm_help_1_80 $oper, ecx
		modrm_help_1_80 $oper, edx
		modrm_help_1_80 $oper, ebx
		modrm_help_1_80 $oper, esp
		modrm_help_1_80 $oper, ebp
		modrm_help_1_80 $oper, esi
		modrm_help_1_80 $oper, edi
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
	modrm_tbl_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $param
		modrm_help_1_0 $o0, $param, $param
		modrm_help_1_0 $o1, $param, $param
		modrm_help_1_0 $o2, $param, $param
		modrm_help_1_0 $o3, $param, $param
		modrm_help_1_0 $o4, $param, $param
		modrm_help_1_0 $o5, $param, $param
		modrm_help_1_0 $o6, $param, $param
		modrm_help_1_0 $o7, $param, $param
		modrm_help_1_40 $o0, $param, $param
		modrm_help_1_40 $o1, $param, $param
		modrm_help_1_40 $o2, $param, $param
		modrm_help_1_40 $o3, $param, $param
		modrm_help_1_40 $o4, $param, $param
		modrm_help_1_40 $o5, $param, $param
		modrm_help_1_40 $o6, $param, $param
		modrm_help_1_40 $o7, $param, $param
		modrm_help_1_80 $o0, $param
		modrm_help_1_80 $o1, $param
		modrm_help_1_80 $o2, $param
		modrm_help_1_80 $o3, $param
		modrm_help_1_80 $o4, $param
		modrm_help_1_80 $o5, $param
		modrm_help_1_80 $o6, $param
		modrm_help_1_80 $o7, $param
	MEND

	MACRO 
	modrm_help_3_0 $oper, $reg, $sreg
		DCD	$oper_$reg_eaxidx
		DCD	$oper_$reg_ecxidx
		DCD	$oper_$reg_edxidx
		DCD	$oper_$sreg_bxidx		; reg,[ebx]	(= oper_reg_bxidx in cpu.S)
		DCD	$oper_$reg_SIB		; reg,SIB
		DCD	$oper_$reg_disp32		; reg,disp32
		DCD	$oper_$sreg_siidx		; reg,[esi]	(in cpu.S)
		DCD	$oper_$sreg_diidx		; reg,[edi]	(in cpu.S)
	MEND
	MACRO 
	modrm_help_3_40 $oper, $reg, $sreg
		DCD	$oper_$reg_eaxd8
		DCD	$oper_$reg_ecxd8
		DCD	$oper_$reg_edxd8
		DCD	$oper_$sreg_bxdisp8	; reg,[ebx+disp8]	(= oper_reg_bxdisp8 in cpu.S)
		DCD	$oper_$reg_SIBd8
		DCD	$oper_$sreg_bpdisp8	; reg,[ebp+disp8]	(= oper_reg_bpdisp8 in cpu.S)
		DCD	$oper_$sreg_sidisp8	; reg,[esi+disp8]	(= oper_reg_sidisp8 in cpu.S)
		DCD	$oper_$sreg_didisp8	; reg,[edi+disp8]	(= oper_reg_didisp8 in cpu.S)
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
	modrm_tbl_3_old $oper
		modrm_help_3_0 $oper, eax, eax
		modrm_help_3_0 $oper, ecx, ecx
		modrm_help_3_0 $oper, edx, edx
		modrm_help_3_0 $oper, ebx, ebx
		modrm_help_3_0 $oper, esp, "esp"
		modrm_help_3_0 $oper, ebp, ebp
		modrm_help_3_0 $oper, esi, esi
		modrm_help_3_0 $oper, edi, edi
		modrm_help_3_40 $oper, eax, eax
		modrm_help_3_40 $oper, ecx, ecx
		modrm_help_3_40 $oper, edx, edx
		modrm_help_3_40 $oper, ebx, ebx
		modrm_help_3_40 $oper, esp, "esp"
		modrm_help_3_40 $oper, ebp, ebp
		modrm_help_3_40 $oper, esi, esi
		modrm_help_3_40 $oper, edi, edi
		modrm_help_3_80 $oper, eax
		modrm_help_3_80 $oper, ecx
		modrm_help_3_80 $oper, edx
		modrm_help_3_80 $oper, ebx
		modrm_help_3_80 $oper, esp
		modrm_help_3_80 $oper, ebp
		modrm_help_3_80 $oper, esi
		modrm_help_3_80 $oper, edi
	MEND
	MACRO
	modrm_tbl_3_new $oper
		modrm_help_3_0 $oper, eax, eax
		modrm_help_3_0 $oper, ecx, ecx
		modrm_help_3_0 $oper, edx, edx
		modrm_help_3_0 $oper, ebx, ebx
		modrm_help_3_0 $oper, esp, esp
		modrm_help_3_0 $oper, ebp, ebp
		modrm_help_3_0 $oper, esi, esi
		modrm_help_3_0 $oper, edi, edi
		modrm_help_3_40 $oper, eax, eax
		modrm_help_3_40 $oper, ecx, ecx
		modrm_help_3_40 $oper, edx, edx
		modrm_help_3_40 $oper, ebx, ebx
		modrm_help_3_40 $oper, esp, esp
		modrm_help_3_40 $oper, ebp, ebp
		modrm_help_3_40 $oper, esi, esi
		modrm_help_3_40 $oper, edi, edi
		modrm_help_3_80 $oper, eax
		modrm_help_3_80 $oper, ecx
		modrm_help_3_80 $oper, edx
		modrm_help_3_80 $oper, ebx
		modrm_help_3_80 $oper, esp
		modrm_help_3_80 $oper, ebp
		modrm_help_3_80 $oper, esi
		modrm_help_3_80 $oper, edi
		modrm_help_3_C0 $oper, eax
		modrm_help_3_C0 $oper, ecx
		modrm_help_3_C0 $oper, edx
		modrm_help_3_C0 $oper, ebx
		modrm_help_3_C0 $oper, esp
		modrm_help_3_C0 $oper, ebp
		modrm_help_3_C0 $oper, esi
		modrm_help_3_C0 $oper, edi
	MEND

	; ==================== EXTERN generators ===============================

	MACRO
	modrm_1_ext_C0 $oper, $reg
		IF "$reg" = ""
			EXTERN $oper_eax
			EXTERN $oper_ecx
			EXTERN $oper_edx
			EXTERN $oper_ebx
			EXTERN $oper_esp
			EXTERN $oper_ebp
			EXTERN $oper_esi
			EXTERN $oper_edi
		ELSE
		ENDIF
	MEND

	MACRO
	modrm_1_externs $oper, $jump, $reg
	IF "$oper" = "skip"
	ELSE
; ----- 0x00 -----
		IF "$reg" = ""
			EXTERN	$oper_eaxidx
			EXTERN	$oper_ecxidx
			EXTERN	$oper_edxidx
			EXTERN	$oper_bxidx		; [ebx],reg	(= oper_bxidx_reg in cpu.S)
			EXTERN	$oper_SIB
			EXTERN	$oper_disp32
			EXTERN	$oper_siidx		; [esi],reg	(= oper_siidx_reg in cpu.S)
			EXTERN	$oper_diidx		; [edi],reg	(= oper_diidx_reg in cpu.S)
; ----- 0x40 = disp8 -----
			EXTERN	$oper_eaxd8
			EXTERN	$oper_ecxd8
			EXTERN	$oper_edxd8
			EXTERN	$oper_bxdisp8		; [ebx+disp8],reg	(= oper_bxdisp8_reg in cpu.S)
			EXTERN	$oper_SIBd8
			EXTERN	$oper_bpdisp8		; [ebp+disp8],reg	(= oper_bpdisp8_reg in cpu.S)
			EXTERN	$oper_sidisp8		; [esi+disp8],reg	(= oper_sidisp8_reg in cpu.S)
			EXTERN	$oper_didisp8		; [edi+disp8],reg	(= oper_didisp8_reg in cpu.S)
; ----- 0x80 = disp32 -----
			EXTERN	$oper_eaxd32
			EXTERN	$oper_ecxd32
			EXTERN	$oper_edxd32
			EXTERN	$oper_ebxd32
			EXTERN	$oper_SIBd32
			EXTERN	$oper_ebpd32
			EXTERN	$oper_esid32
			EXTERN	$oper_edid32
; ----- 0xC0 = reg -----
			modrm_1_ext_C0 $oper
		ELSE
			EXTERN	$oper_eaxidx_$reg
			EXTERN	$oper_ecxidx_$reg
			EXTERN	$oper_edxidx_$reg
			EXTERN	$oper_bxidx_$reg
			EXTERN	$oper_SIB_$reg
			EXTERN	$oper_disp32_$reg
			EXTERN	$oper_siidx_$reg
			EXTERN	$oper_diidx_$reg
; ----- 0x40 = disp8 -----
			EXTERN	$oper_eaxd8_$reg
			EXTERN	$oper_ecxd8_$reg
			EXTERN	$oper_edxd8_$reg
			EXTERN 	$oper_bxdisp8_$reg
			EXTERN 	$oper_SIBd8_$reg
			EXTERN 	$oper_bpdisp8_$reg
			EXTERN 	$oper_sidisp8_$reg
			EXTERN 	$oper_didisp8_$reg
; ----- 0x80 = disp32 -----
			EXTERN	$oper_eaxd32_$reg
			EXTERN	$oper_ecxd32_$reg
			EXTERN	$oper_edxd32_$reg
			EXTERN	$oper_ebxd32_$reg
			EXTERN	$oper_SIBd32_$reg
			EXTERN	$oper_ebpd32_$reg
			EXTERN	$oper_esid32_$reg
			EXTERN	$oper_edid32_$reg
		ENDIF
	ENDIF	
	MEND

	MACRO
	modrm_genext_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $jump, $param
		modrm_1_externs $o0, $o0_$jump, $param
		modrm_1_externs $o0, $o0_$jump, $param
		modrm_1_externs $o1, $o1_$jump, $param
		modrm_1_externs $o2, $o2_$jump, $param
		modrm_1_externs $o3, $o3_$jump, $param
		modrm_1_externs $o4, $o4_$jump, $param
		modrm_1_externs $o5, $o5_$jump, $param
		modrm_1_externs $o6, $o6_$jump, $param
		modrm_1_externs $o7, $o7_$jump, $param
	MEND

	; ==================== Code generators ===============================

	; -------------------------- r/m32, r32 ------------------------------

	MACRO 
	modrm_1_help $oper, $jump, $reg, $sreg
	IF "$oper" = "skip"
	ELSE
		IF "$reg" = ""
			; ----- 0x00 -----
$oper_eaxidx
				mov		r0, eax
				b		$jump
$oper_ecxidx
				mov		r0, ecx
				b		$jump
$oper_edxidx
				mov		r0, edx
				b		$jump
			EXTERN	$oper_bxidx
$oper_SIB
				ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
				ldr		r1,=sib_table
				add		r0, r1, r0, lsl #2
				ldr		r1, =$jump						; r1 = return address
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
				ldr		r1, =$jump						; r1 = return address
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
				ldr		r1, =$jump						; r1 = return address
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
			EXTERN	$oper_bxidx_$sreg
$oper_SIB_$reg
				ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
				ldr		r1,=sib_table
				add		r0, r1, r0, lsl #2
				ldr		r1, =$jump_$reg					; r1 = return address
				ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_disp32_$reg
				get_cseip_dword r0
				b		$jump_$reg
			EXTERN	$oper_siidx_$sreg
			EXTERN	$oper_diidx_$sreg
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
			EXTERN	$oper_bxdisp8_$sreg
$oper_SIBd8_$reg
				ldrb	r0,[r12],#1
				ldr		r1,=sib_disp8_table
				add		r0, r1, r0, lsl #2
				ldr		r1, =$jump_$reg					; r1 = return address
				ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
			EXTERN	$oper_bpdisp8_$sreg
			EXTERN	$oper_sidisp8_$sreg
			EXTERN	$oper_didisp8_$sreg
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
	ENDIF	
	MEND

	MACRO 
	modrm_1_genall_old $oper, $jump
		modrm_1_help $oper, $jump, eax, eax
		modrm_1_help $oper, $jump, ecx, ecx
		modrm_1_help $oper, $jump, edx, edx
		modrm_1_help $oper, $jump, ebx, ebx
		modrm_1_help $oper, $jump, esp, "esp"
		modrm_1_help $oper, $jump, ebp, ebp
		modrm_1_help $oper, $jump, esi, esi
		modrm_1_help $oper, $jump, edi, edi
		LTORG
		EXTERN $jump_eax
		EXTERN $jump_bp_eax
		EXTERN $jump_ecx
		EXTERN $jump_bp_ecx
		EXTERN $jump_edx
		EXTERN $jump_bp_edx
		EXTERN $jump_ebx
		EXTERN $jump_bp_ebx
		EXTERN $jump_esp
		EXTERN $jump_bp_esp
		EXTERN $jump_ebp
		EXTERN $jump_bp_ebp
		EXTERN $jump_esi
		EXTERN $jump_bp_esi
		EXTERN $jump_edi
		EXTERN $jump_bp_edi
	MEND
	MACRO 
	modrm_1_genall_new $oper, $jump
		modrm_1_help $oper, $jump, eax, eax
		modrm_1_help $oper, $jump, ecx, ecx
		modrm_1_help $oper, $jump, edx, edx
		modrm_1_help $oper, $jump, ebx, ebx
		modrm_1_help $oper, $jump, esp, esp
		modrm_1_help $oper, $jump, ebp, ebp
		modrm_1_help $oper, $jump, esi, esi
		modrm_1_help $oper, $jump, edi, edi
		LTORG
		EXTERN $jump_eax
		EXTERN $jump_bp_eax
		EXTERN $jump_ecx
		EXTERN $jump_bp_ecx
		EXTERN $jump_edx
		EXTERN $jump_bp_edx
		EXTERN $jump_ebx
		EXTERN $jump_bp_ebx
		EXTERN $jump_esp
		EXTERN $jump_bp_esp
		EXTERN $jump_ebp
		EXTERN $jump_bp_ebp
		EXTERN $jump_esi
		EXTERN $jump_bp_esi
		EXTERN $jump_edi
		EXTERN $jump_bp_edi
	MEND

	MACRO 
	modrm_genall_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $jump, $param
		modrm_1_help $o0, $o0_$jump, $param, $param
		modrm_1_help $o1, $o1_$jump, $param, $param
		modrm_1_help $o2, $o2_$jump, $param, $param
		modrm_1_help $o3, $o3_$jump, $param, $param
		modrm_1_help $o4, $o4_$jump, $param, $param
		modrm_1_help $o5, $o5_$jump, $param, $param
		modrm_1_help $o6, $o6_$jump, $param, $param
		modrm_1_help $o7, $o7_$jump, $param, $param
		LTORG
		IF "$param" = ""
			EXTERN $o0_$jump
			EXTERN $o0_$jump_bp
			EXTERN $o1_$jump
			EXTERN $o1_$jump_bp
			EXTERN $o2_$jump
			EXTERN $o2_$jump_bp
			EXTERN $o3_$jump
			EXTERN $o3_$jump_bp
			EXTERN $o4_$jump
			EXTERN $o4_$jump_bp
			EXTERN $o5_$jump
			EXTERN $o5_$jump_bp
			EXTERN $o6_$jump
			EXTERN $o6_$jump_bp
			EXTERN $o7_$jump
			EXTERN $o7_$jump_bp
		ELSE
			EXTERN $o0_$jump_$param
			EXTERN $o0_$jump_bp_$param
			EXTERN $o1_$jump_$param
			EXTERN $o1_$jump_bp_$param
			EXTERN $o2_$jump_$param
			EXTERN $o2_$jump_bp_$param
			EXTERN $o3_$jump_$param
			EXTERN $o3_$jump_bp_$param
			EXTERN $o4_$jump_$param
			EXTERN $o4_$jump_bp_$param
			EXTERN $o5_$jump_$param
			EXTERN $o5_$jump_bp_$param
			EXTERN $o6_$jump_$param
			EXTERN $o6_$jump_bp_$param
			EXTERN $o7_$jump_$param
			EXTERN $o7_$jump_bp_$param
		ENDIF
	MEND

	MACRO 
	modrm_3_help $oper, $jump, $reg, $sreg
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
		EXTERN	$oper_$sreg_bxidx
$oper_$reg_SIB
		ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
		ldr		r1,=sib_table
		add		r0, r1, r0, lsl #2
		ldr		r1, =$jump_$reg					; r1 = return address
		ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
$oper_$reg_disp32
		get_cseip_dword r0
		b		$jump_$reg
		EXTERN	$oper_$sreg_siidx
		EXTERN	$oper_$sreg_diidx
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
		EXTERN	$oper_$sreg_bxdisp8
$oper_$reg_SIBd8
		ldrb	r0,[r12],#1
		ldr		r1,=sib_disp8_table
		add		r0, r1, r0, lsl #2
		ldr		r1, =$jump_$reg					; r1 = return address
		ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
		EXTERN	$oper_$sreg_bpdisp8
		EXTERN	$oper_$sreg_sidisp8
		EXTERN	$oper_$sreg_didisp8
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
		ldrb	r0,[r12],#1
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
	MEND

	MACRO 
	modrm_3_genall_old $oper, $jump
		modrm_3_help $oper, $jump, eax, eax
		modrm_3_help $oper, $jump, ecx, ecx
		modrm_3_help $oper, $jump, edx, edx
		modrm_3_help $oper, $jump, ebx, ebx
		modrm_3_help $oper, $jump, esp, "esp"
		modrm_3_help $oper, $jump, ebp, ebp
		modrm_3_help $oper, $jump, esi, esi
		modrm_3_help $oper, $jump, edi, edi
		LTORG
		EXTERN $jump_eax
		EXTERN $jump_bp_eax
		EXTERN $jump_ecx
		EXTERN $jump_bp_ecx
		EXTERN $jump_edx
		EXTERN $jump_bp_edx
		EXTERN $jump_ebx
		EXTERN $jump_bp_ebx
		EXTERN $jump_esp
		EXTERN $jump_bp_esp
		EXTERN $jump_ebp
		EXTERN $jump_bp_ebp
		EXTERN $jump_esi
		EXTERN $jump_bp_esi
		EXTERN $jump_edi
		EXTERN $jump_bp_edi
	MEND
	MACRO 
	modrm_3_genall_new $oper, $jump
		modrm_3_help $oper, $jump, eax, eax
		modrm_3_help $oper, $jump, ecx, ecx
		modrm_3_help $oper, $jump, edx, edx
		modrm_3_help $oper, $jump, ebx, ebx
		modrm_3_help $oper, $jump, esp, esp
		modrm_3_help $oper, $jump, ebp, ebp
		modrm_3_help $oper, $jump, esi, esi
		modrm_3_help $oper, $jump, edi, edi
		LTORG
		EXTERN $jump_eax
		EXTERN $jump_bp_eax
		EXTERN $jump_ecx
		EXTERN $jump_bp_ecx
		EXTERN $jump_edx
		EXTERN $jump_bp_edx
		EXTERN $jump_ebx
		EXTERN $jump_bp_ebx
		EXTERN $jump_esp
		EXTERN $jump_bp_esp
		EXTERN $jump_ebp
		EXTERN $jump_bp_ebp
		EXTERN $jump_esi
		EXTERN $jump_bp_esi
		EXTERN $jump_edi
		EXTERN $jump_bp_edi
	MEND

	END
