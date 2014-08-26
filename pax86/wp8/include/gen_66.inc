;=============================================================================
; gen_66.inc
;
; This is an include file for "cpu_66.s" and "cpu_0f_66.s". This file contains
; macros for creating the MODRM jump tables and effective address calculation
; routines for when the address size is 16 bits (USE16 code segment) but
; operand size is 32 bits.
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

; ==================== Jump table generators =========================

	MACRO
	modrm_reg_reg_word $oper, $lr, $rr
		IF "$rr" = ""
			EXTERN $oper_$lr
			DCD	$oper_$lr
		ELIF "$lr" = ""
			EXTERN $oper_$rr
			DCD	$oper_$rr
		ELSE
			EXTERN $oper_$lr_$rr
			DCD	$oper_$lr_$rr
		ENDIF
	MEND

	MACRO
	modrm_help_1_0 $oper, $reg
		IF "$oper" = "back2"
			DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
		ELSE
			IF "$reg" = ""
				DCD	$oper_bxsi
				DCD	$oper_bxdi
				DCD	$oper_bpsi
				DCD	$oper_bpdi
				DCD	$oper_esiidx
				DCD	$oper_ediidx
				DCD	$oper_disp16
				DCD	$oper_ebxidx
			ELSE
				DCD	$oper_bxsi_$reg
				DCD	$oper_bxdi_$reg
				DCD	$oper_bpsi_$reg
				DCD	$oper_bpdi_$reg
				DCD	$oper_esiidx_$reg
				DCD	$oper_ediidx_$reg
				DCD	$oper_disp16_$reg
				DCD	$oper_ebxidx_$reg
			ENDIF
		ENDIF	
	MEND
	MACRO
	modrm_help_1_40 $oper, $reg
		IF "$oper" = "back2"
			DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
		ELSE
			IF "$reg" = ""
				DCD	$oper_bxsid8
				DCD	$oper_bxdid8
				DCD	$oper_bpsid8
				DCD	$oper_bpdid8
				DCD	$oper_esid8
				DCD	$oper_edid8
				DCD	$oper_bpdisp8
				DCD	$oper_ebxd8
			ELSE
				DCD	$oper_bxsid8_$reg
				DCD	$oper_bxdid8_$reg
				DCD	$oper_bpsid8_$reg
				DCD	$oper_bpdid8_$reg
				DCD	$oper_esid8_$reg
				DCD	$oper_edid8_$reg
				DCD	$oper_bpdisp8_$reg
				DCD	$oper_ebxd8_$reg
			ENDIF
		ENDIF	
	MEND
	MACRO
	modrm_help_1_80 $oper, $reg
		IF "$oper" = "back2"
			DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
		ELSE
			IF "$reg" = ""
				DCD	$oper_bxsid16
				DCD	$oper_bxdid16
				DCD	$oper_bpsid16
				DCD	$oper_bpdid16
				DCD	$oper_sidisp16
				DCD	$oper_didisp16
				DCD	$oper_bpdisp16
				DCD	$oper_bxdisp16
			ELSE
				DCD	$oper_bxsid16_$reg
				DCD	$oper_bxdid16_$reg
				DCD	$oper_bpsid16_$reg
				DCD	$oper_bpdid16_$reg
				DCD	$oper_sidisp16_$reg
				DCD	$oper_didisp16_$reg
				DCD	$oper_bpdisp16_$reg
				DCD	$oper_bxdisp16_$reg
			ENDIF
		ENDIF
	MEND
	MACRO
	modrm_help_1_C0 $oper, $reg
		IF "$oper" = "back2"
			DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
		ELSE
			modrm_reg_reg_word $oper, eax, $reg
			modrm_reg_reg_word $oper, ecx, $reg
			modrm_reg_reg_word $oper, edx, $reg
			modrm_reg_reg_word $oper, ebx, $reg
			modrm_reg_reg_word $oper, esp, $reg
			modrm_reg_reg_word $oper, ebp, $reg
			modrm_reg_reg_word $oper, esi, $reg
			modrm_reg_reg_word $oper, edi, $reg
		ENDIF	
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
	modrm_tbl_oper $o0, $o1, $o2, $o3, $o4, $o5, $o6, $o7, $param
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

	MACRO
	modrm_help_3_0 $oper, $reg
		DCD	$oper_$reg_bxsi
		DCD	$oper_$reg_bxdi
		DCD	$oper_$reg_bpsi
		DCD	$oper_$reg_bpdi
		DCD	$oper_$reg_esiidx		; reg,[esi]	(in cpu_386.S)
		DCD	$oper_$reg_ediidx		; reg,[edi]	(in cpu_386.S)
		DCD	$oper_$reg_disp16
		DCD	$oper_$reg_ebxidx		; reg,[ebx]	(in cpu_386.S)
	MEND
	MACRO
	modrm_help_3_40 $oper, $reg
		DCD	$oper_$reg_bxsid8
		DCD	$oper_$reg_bxdid8
		DCD	$oper_$reg_bpsid8
		DCD	$oper_$reg_bpdid8
		DCD	$oper_$reg_esid8		; reg,[esi+disp8]
		DCD	$oper_$reg_edid8		; reg,[edi+disp8]
		DCD	$oper_$reg_bpdisp8
		DCD	$oper_$reg_ebxd8		; reg,[ebx+disp8]
	MEND
	MACRO
	modrm_help_3_80 $oper, $reg
		DCD	$oper_$reg_bxsid16
		DCD	$oper_$reg_bxdid16
		DCD	$oper_$reg_bpsid16
		DCD	$oper_$reg_bpdid16
		DCD	$oper_$reg_sidisp16
		DCD	$oper_$reg_didisp16
		DCD	$oper_$reg_bpdisp16
		DCD	$oper_$reg_bxdisp16
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
			EXTERN $oper_bxsi
			EXTERN $oper_bxdi
			EXTERN $oper_bpsi
			EXTERN $oper_bpdi
			EXTERN $oper_esiidx
			EXTERN $oper_ediidx
			EXTERN $oper_disp16
			EXTERN $oper_ebxidx
; ----- 0x40 = disp8 -----
			EXTERN $oper_bxsid8
			EXTERN $oper_bxdid8
			EXTERN $oper_bpsid8
			EXTERN $oper_bpdid8
			EXTERN $oper_esid8
			EXTERN $oper_edid8
			EXTERN $oper_bpdisp8
			EXTERN $oper_ebxd8
; ----- 0x80 = disp16 -----
			EXTERN $oper_bxsid16
			EXTERN $oper_bxdid16
			EXTERN $oper_bpsid16
			EXTERN $oper_bpdid16
			EXTERN $oper_sidisp16
			EXTERN $oper_didisp16
			EXTERN $oper_bpdisp16
			EXTERN $oper_bxdisp16
; ----- 0xC0 = reg -----
			modrm_1_ext_C0 $oper
		ELSE
			EXTERN $oper_bxsi_$reg
			EXTERN $oper_bxdi_$reg
			EXTERN $oper_bpsi_$reg
			EXTERN $oper_bpdi_$reg
			EXTERN $oper_disp16_$reg
; ----- 0x40 = disp8 -----
			EXTERN $oper_bxsid8_$reg
			EXTERN $oper_bxdid8_$reg
			EXTERN $oper_bpsid8_$reg
			EXTERN $oper_bpdid8_$reg
			EXTERN $oper_bpdisp8_$reg
; ----- 0x80 = disp16 -----
			EXTERN $oper_bxsid16_$reg
			EXTERN $oper_bxdid16_$reg
			EXTERN $oper_bpsid16_$reg
			EXTERN $oper_bpdid16_$reg
			EXTERN $oper_sidisp16_$reg
			EXTERN $oper_didisp16_$reg
			EXTERN $oper_bpdisp16_$reg
			EXTERN $oper_bxdisp16_$reg
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

	MACRO
	modrm_1_help $oper, $jump, $reg
	IF "$oper" = "skip"
	ELSE
; ----- 0x00 -----
		IF "$reg" = ""
$oper_bxsi
	add		r0, ebx, esi
	b		$jump
$oper_bxdi
	add		r0, ebx, edi
	b		$jump
$oper_bpsi
	add		r0, ebp, esi
	b		$jump_bp
$oper_bpdi
	add		r0, ebp, edi
	b		$jump_bp
	EXTERN $oper_esiidx
	EXTERN $oper_ediidx
$oper_disp16
	r0_from_disp16
	b		$jump
	EXTERN $oper_ebxidx
; ----- 0x40 = disp8 -----
$oper_bxsid8
	r0_from_bxidxdisp8 esi
	b		$jump
$oper_bxdid8
	r0_from_bxidxdisp8 edi
	b		$jump
$oper_bpsid8
	r0_from_bpidxdisp8 esi
	b		$jump_bp
$oper_bpdid8
	r0_from_bpidxdisp8 edi
	b		$jump_bp
	EXTERN $oper_esid8
	EXTERN $oper_edid8
$oper_bpdisp8
	r0_from_idx_disp8 ebp
	b		$jump_bp
	EXTERN $oper_ebxd8
; ----- 0x80 = disp16 -----
$oper_bxsid16
	r0_from_bxidxdisp16 esi
	b		$jump
$oper_bxdid16
	r0_from_bxidxdisp16 edi
	b		$jump
$oper_bpsid16
	r0_from_bpidxdisp16 esi
	b		$jump_bp
$oper_bpdid16
	r0_from_bpidxdisp16 edi
	b		$jump_bp
$oper_sidisp16
	r0_from_idx_disp16 esi
	b		$jump
$oper_didisp16
	r0_from_idx_disp16 edi
	b		$jump
$oper_bpdisp16
	r0_from_idx_disp16 ebp
	b		$jump_bp
$oper_bxdisp16
	r0_from_idx_disp16 ebx
	b		$jump
		ELSE
$oper_bxsi_$reg
	add		r0, ebx, esi
	b		$jump_$reg
$oper_bxdi_$reg
	add		r0, ebx, edi
	b		$jump_$reg
$oper_bpsi_$reg
	add		r0, ebp, esi
	b		$jump_bp_$reg
$oper_bpdi_$reg
	add		r0, ebp, edi
	b		$jump_bp_$reg
	EXTERN $oper_esiidx_$reg
	EXTERN $oper_ediidx_$reg
$oper_disp16_$reg
	r0_from_disp16
	b		$jump_$reg
	EXTERN $oper_ebxidx_$reg
; ----- 0x40 = disp8 -----
$oper_bxsid8_$reg
	r0_from_bxidxdisp8 esi
	b		$jump_$reg
$oper_bxdid8_$reg
	r0_from_bxidxdisp8 edi
	b		$jump_$reg
$oper_bpsid8_$reg
	r0_from_bpidxdisp8 esi
	b		$jump_bp_$reg
$oper_bpdid8_$reg
	r0_from_bpidxdisp8 edi
	b		$jump_bp_$reg
	EXTERN $oper_esid8_$reg
	EXTERN $oper_edid8_$reg
$oper_bpdisp8_$reg
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	EXTERN $oper_ebxd8_$reg
; ----- 0x80 = disp16 -----
$oper_bxsid16_$reg
	r0_from_bxidxdisp16 esi
	b		$jump_$reg
$oper_bxdid16_$reg
	r0_from_bxidxdisp16 edi
	b		$jump_$reg
$oper_bpsid16_$reg
	r0_from_bpidxdisp16 esi
	b		$jump_bp_$reg
$oper_bpdid16_$reg
	r0_from_bpidxdisp16 edi
	b		$jump_bp_$reg
$oper_sidisp16_$reg
	r0_from_idx_disp16 esi
	b		$jump_$reg
$oper_didisp16_$reg
	r0_from_idx_disp16 edi
	b		$jump_$reg
$oper_bpdisp16_$reg
	r0_from_idx_disp16 ebp
	b		$jump_bp_$reg
$oper_bxdisp16_$reg
	r0_from_idx_disp16 ebx
	b		$jump_$reg
; ----- 0xC0 = reg -----
	EXTERN $oper_$reg_eax
	EXTERN $oper_$reg_ecx
	EXTERN $oper_$reg_edx
	EXTERN $oper_$reg_ebx
	EXTERN $oper_$reg_esp
	EXTERN $oper_$reg_ebp
	EXTERN $oper_$reg_esi
	EXTERN $oper_$reg_edi
		ENDIF
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
		modrm_1_help $o0, $o0_$jump, $param
		modrm_1_help $o1, $o1_$jump, $param
		modrm_1_help $o2, $o2_$jump, $param
		modrm_1_help $o3, $o3_$jump, $param
		modrm_1_help $o4, $o4_$jump, $param
		modrm_1_help $o5, $o5_$jump, $param
		modrm_1_help $o6, $o6_$jump, $param
		modrm_1_help $o7, $o7_$jump, $param
		modrm_1_ext_C0 $o0, $param
		modrm_1_ext_C0 $o1, $param
		modrm_1_ext_C0 $o2, $param
		modrm_1_ext_C0 $o3, $param
		modrm_1_ext_C0 $o4, $param
		modrm_1_ext_C0 $o5, $param
		modrm_1_ext_C0 $o6, $param
		modrm_1_ext_C0 $o7, $param
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
	modrm_3_help $oper, $jump, $reg
; ----- 0x00 -----
$oper_$reg_bxsi
	add		r0, ebx, esi
	b		$jump_$reg
$oper_$reg_bxdi
	add		r0, ebx, edi
	b		$jump_$reg
$oper_$reg_bpsi
	add		r0, ebp, esi
	b		$jump_bp_$reg
$oper_$reg_bpdi
	add		r0, ebp, edi
	b		$jump_bp_$reg
	EXTERN $oper_$reg_esiidx
	EXTERN $oper_$reg_ediidx
$oper_$reg_disp16
	r0_from_disp16
	b		$jump_$reg
	EXTERN $oper_$reg_ebxidx
; ----- 0x40 = disp8 -----
$oper_$reg_bxsid8
	r0_from_bxidxdisp8 esi
	b		$jump_$reg
$oper_$reg_bxdid8
	r0_from_bxidxdisp8 edi
	b		$jump_$reg
$oper_$reg_bpsid8
	r0_from_bpidxdisp8 esi
	b		$jump_bp_$reg
$oper_$reg_bpdid8
	r0_from_bpidxdisp8 edi
	b		$jump_bp_$reg
	EXTERN $oper_$reg_esid8
	EXTERN $oper_$reg_edid8
$oper_$reg_bpdisp8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	EXTERN $oper_$reg_ebxd8
; ----- 0x80 = disp16 -----
$oper_$reg_bxsid16
	r0_from_bxidxdisp16 esi
	b		$jump_$reg
$oper_$reg_bxdid16
	r0_from_bxidxdisp16 edi
	b		$jump_$reg
$oper_$reg_bpsid16
	r0_from_bpidxdisp16 esi
	b		$jump_bp_$reg
$oper_$reg_bpdid16
	r0_from_bpidxdisp16 edi
	b		$jump_bp_$reg
$oper_$reg_sidisp16
	r0_from_idx_disp16 esi
	b		$jump_$reg
$oper_$reg_didisp16
	r0_from_idx_disp16 edi
	b		$jump_$reg
$oper_$reg_bpdisp16
	r0_from_idx_disp16 ebp
	b		$jump_bp_$reg
$oper_$reg_bxdisp16
	r0_from_idx_disp16 ebx
	b		$jump_$reg
; ----- 0xC0 = reg -----
	EXTERN $oper_$reg_eax
	EXTERN $oper_$reg_ecx
	EXTERN $oper_$reg_edx
	EXTERN $oper_$reg_ebx
	EXTERN $oper_$reg_esp
	EXTERN $oper_$reg_ebp
	EXTERN $oper_$reg_esi
	EXTERN $oper_$reg_edi
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
