;=============================================================================
; gen_16.inc
;
; This is an include file for "cpu_0f.s". This file contains macros for
; creating the MODRM jump tables and effectice address calculation routines
; for when the address and code sizes are both 16 bits (USE16 code segment).
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

; --------------------------- r/m8, r8 -------------------------------

	MACRO
	modrm_help_0_0 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		DCD	$oper_bxsi_$reg8
		DCD	$oper_bxdi_$reg8
		DCD	$oper_bpsi_$reg8
		DCD	$oper_bpdi_$reg8
		DCD	$oper_siidx_$reg8
		DCD	$oper_diidx_$reg8
		DCD	$oper_disp16_$reg8
		DCD	$oper_bxidx_$reg8
	ENDIF	
	MEND
	MACRO
	modrm_help_0_40 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		DCD	$oper_bxsid8_$reg8
		DCD	$oper_bxdid8_$reg8
		DCD	$oper_bpsid8_$reg8
		DCD	$oper_bpdid8_$reg8
		DCD	$oper_sidisp8_$reg8
		DCD	$oper_didisp8_$reg8
		DCD	$oper_bpdisp8_$reg8
		DCD	$oper_bxdisp8_$reg8
	ENDIF	
	MEND
	MACRO
	modrm_help_0_80 $oper, $reg8
	IF "$oper" = "back1"
		DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ELSE
		DCD	$oper_bxsid16_$reg8
		DCD	$oper_bxdid16_$reg8
		DCD	$oper_bpsid16_$reg8
		DCD	$oper_bpdid16_$reg8
		DCD	$oper_sidisp16_$reg8
		DCD	$oper_didisp16_$reg8
		DCD	$oper_bpdisp16_$reg8
		DCD	$oper_bxdisp16_$reg8
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

; -------------------------- r/m16, r16 ------------------------------

	MACRO
	modrm_help_1_0 $oper, $reg
	IF "$reg" = ""
		DCD	$oper_bxsi
		DCD	$oper_bxdi
		DCD	$oper_bpsi
		DCD	$oper_bpdi
		DCD	$oper_siidx
		DCD	$oper_diidx
		DCD	$oper_disp16
		DCD	$oper_bxidx
	ELSE
		DCD	$oper_bxsi_$reg
		DCD	$oper_bxdi_$reg
		DCD	$oper_bpsi_$reg
		DCD	$oper_bpdi_$reg
		DCD	$oper_siidx_$reg
		DCD	$oper_diidx_$reg
		DCD	$oper_disp16_$reg
		DCD	$oper_bxidx_$reg
	ENDIF
	MEND
	MACRO
	modrm_help_1_40 $oper, $reg
	IF "$reg" = ""
		DCD	$oper_bxsid8
		DCD	$oper_bxdid8
		DCD	$oper_bpsid8
		DCD	$oper_bpdid8
		DCD	$oper_sidisp8
		DCD	$oper_didisp8
		DCD	$oper_bpdisp8
		DCD	$oper_bxdisp8
	ELSE
		DCD	$oper_bxsid8_$reg
		DCD	$oper_bxdid8_$reg
		DCD	$oper_bpsid8_$reg
		DCD	$oper_bpdid8_$reg
		DCD	$oper_sidisp8_$reg
		DCD	$oper_didisp8_$reg
		DCD	$oper_bpdisp8_$reg
		DCD	$oper_bxdisp8_$reg
	ENDIF
	MEND
	MACRO
	modrm_help_1_80 $oper, $reg
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
	MEND
	MACRO
	modrm_help_1_C0_old $oper, $reg
	IF "$reg" = ""
		DCD	$oper_ax
		DCD	$oper_cx
		DCD	$oper_dx
		DCD	$oper_bx
		DCD	$oper_sp
		DCD	$oper_bp
		DCD	$oper_si
		DCD	$oper_di
	ELSE
		DCD	$oper_ax_$reg
		DCD	$oper_cx_$reg
		DCD	$oper_dx_$reg
		DCD	$oper_bx_$reg
		DCD	$oper_sp_$reg
		DCD	$oper_bp_$reg
		DCD	$oper_si_$reg
		DCD	$oper_di_$reg
	ENDIF
	MEND
	MACRO
	modrm_help_1_C0_new $oper, $reg
	IF "$reg" = ""
		DCD	$oper_eax
		DCD	$oper_ecx
		DCD	$oper_edx
		DCD	$oper_ebx
		DCD	$oper_esp
		DCD	$oper_ebp
		DCD	$oper_esi
		DCD	$oper_edi
	ELSE
		DCD	$oper_eax_$reg
		DCD	$oper_ecx_$reg
		DCD	$oper_edx_$reg
		DCD	$oper_ebx_$reg
		DCD	$oper_esp_$reg
		DCD	$oper_ebp_$reg
		DCD	$oper_esi_$reg
		DCD	$oper_edi_$reg
	ENDIF
	MEND

	MACRO
	modrm_tbl_1_old $oper
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
	modrm_help_1_C0_new $oper, eax
	modrm_help_1_C0_new $oper, ecx
	modrm_help_1_C0_new $oper, edx
	modrm_help_1_C0_new $oper, ebx
	modrm_help_1_C0_new $oper, esp
	modrm_help_1_C0_new $oper, ebp
	modrm_help_1_C0_new $oper, esi
	modrm_help_1_C0_new $oper, edi
	MEND

; --------------------------- r8, r/m8 -------------------------------

	MACRO
	modrm_help_2_0 $oper, $reg8
	DCD	$oper_$reg8_bxsi
	DCD	$oper_$reg8_bxdi
	DCD	$oper_$reg8_bpsi
	DCD	$oper_$reg8_bpdi
	DCD	$oper_$reg8_siidx
	DCD	$oper_$reg8_diidx
	DCD	$oper_$reg8_disp16
	DCD	$oper_$reg8_bxidx
	MEND
	MACRO
	modrm_help_2_40 $oper, $reg8
	DCD	$oper_$reg8_bxsid8
	DCD	$oper_$reg8_bxdid8
	DCD	$oper_$reg8_bpsid8
	DCD	$oper_$reg8_bpdid8
	DCD	$oper_$reg8_sidisp8
	DCD	$oper_$reg8_didisp8
	DCD	$oper_$reg8_bpdisp8
	DCD	$oper_$reg8_bxdisp8
	MEND
	MACRO
	modrm_help_2_80 $oper, $reg8
	DCD	$oper_$reg8_bxsid16
	DCD	$oper_$reg8_bxdid16
	DCD	$oper_$reg8_bpsid16
	DCD	$oper_$reg8_bpdid16
	DCD	$oper_$reg8_sidisp16
	DCD	$oper_$reg8_didisp16
	DCD	$oper_$reg8_bpdisp16
	DCD	$oper_$reg8_bxdisp16
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

; --------------------------- r16, r/m16 -----------------------------


	MACRO
	modrm_help_3_0 $oper, $reg
	DCD	$oper_$reg_bxsi
	DCD	$oper_$reg_bxdi
	DCD	$oper_$reg_bpsi
	DCD	$oper_$reg_bpdi
	DCD	$oper_$reg_siidx
	DCD	$oper_$reg_diidx
	DCD	$oper_$reg_disp16
	DCD	$oper_$reg_bxidx
	MEND
	MACRO
	modrm_help_3_40 $oper, $reg
	DCD	$oper_$reg_bxsid8
	DCD	$oper_$reg_bxdid8
	DCD	$oper_$reg_bpsid8
	DCD	$oper_$reg_bpdid8
	DCD	$oper_$reg_sidisp8
	DCD	$oper_$reg_didisp8
	DCD	$oper_$reg_bpdisp8
	DCD	$oper_$reg_bxdisp8
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
	modrm_reg_reg_word $oper, $lr, $rr
	DCD	$oper_$lr_$rr
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
	modrm_tbl_3_old $oper
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
	ELSE
; ----- 0x00 -----
$oper_bxsi_$reg8
	add		r0, ebx, esi
	b		$jump_$reg
$oper_bxdi_$reg8
	add		r0, ebx, edi
	b		$jump_$reg
$oper_bpsi_$reg8
	add		r0, ebp, esi
	b		$jump_bp_$reg
$oper_bpdi_$reg8
	add		r0, ebp, edi
	b		$jump_bp_$reg
	GLOBAL	$oper_siidx_$reg8
$oper_siidx_$reg8
	mov		r0, esi
	b		$jump_$reg
	GLOBAL	$oper_diidx_$reg8
$oper_diidx_$reg8
	mov		r0, edi
	b		$jump_$reg
$oper_disp16_$reg8
	r0_from_disp16
	b		$jump_$reg
	GLOBAL	$oper_bxidx_$reg8
$oper_bxidx_$reg8
	mov		r0, ebx
	b		$jump_$reg
; ----- 0x40 = disp8 -----
$oper_bxsid8_$reg8
	r0_from_bxidxdisp8 esi
	b		$jump_$reg
$oper_bxdid8_$reg8
	r0_from_bxidxdisp8 edi
	b		$jump_$reg
$oper_bpsid8_$reg8
	r0_from_bpidxdisp8 esi
	b		$jump_bp_$reg
$oper_bpdid8_$reg8
	r0_from_bpidxdisp8 edi
	b		$jump_bp_$reg
	GLOBAL	$oper_sidisp8_$reg8
$oper_sidisp8_$reg8
	r0_from_idx_disp8 esi
	b		$jump_$reg
	GLOBAL	$oper_didisp8_$reg8
$oper_didisp8_$reg8
	r0_from_idx_disp8 edi
	b		$jump_$reg
	GLOBAL	$oper_bpdisp8_$reg8
$oper_bpdisp8_$reg8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	GLOBAL	$oper_bxdisp8_$reg8
$oper_bxdisp8_$reg8
	r0_from_idx_disp8 ebx
	b		$jump_$reg
; ----- 0x80 = disp16 -----
$oper_bxsid16_$reg8
	r0_from_bxidxdisp16 esi
	b		$jump_$reg
$oper_bxdid16_$reg8
	r0_from_bxidxdisp16 edi
	b		$jump_$reg
$oper_bpsid16_$reg8
	r0_from_bpidxdisp16 esi
	b		$jump_bp_$reg
$oper_bpdid16_$reg8
	r0_from_bpidxdisp16 edi
	b		$jump_bp_$reg
$oper_sidisp16_$reg8
	r0_from_idx_disp16 esi
	b		$jump_$reg
$oper_didisp16_$reg8
	r0_from_idx_disp16 edi
	b		$jump_$reg
$oper_bpdisp16_$reg8
	r0_from_idx_disp16 ebp
	b		$jump_bp_$reg
$oper_bxdisp16_$reg8
	r0_from_idx_disp16 ebx
	b		$jump_$reg
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

; -------------------------- r/m16, r16 ------------------------------

	MACRO
	modrm_1_help $oper, $jump, $reg
	IF "$oper"= "skip"
	ELIF "$reg" = ""
		; ----- 0x00 -----
			GLOBAL	$oper_bxsi
$oper_bxsi
			add		r0, ebx, esi
			b		$jump
			GLOBAL	$oper_bxdi
$oper_bxdi
			add		r0, ebx, edi
			b		$jump
			GLOBAL	$oper_bpsi
$oper_bpsi
			add		r0, ebp, esi
			b		$jump_bp
			GLOBAL	$oper_bpdi
$oper_bpdi
			add		r0, ebp, edi
			b		$jump_bp
			GLOBAL	$oper_siidx
$oper_siidx
			mov		r0, esi
			b		$jump
			GLOBAL	$oper_diidx
$oper_diidx
			mov		r0, edi
			b		$jump
			GLOBAL	$oper_disp16
$oper_disp16
			r0_from_disp16
			b		$jump
			GLOBAL	$oper_bxidx
$oper_bxidx
			mov		r0, ebx
			b		$jump
		; ----- 0x40 = disp8 -----
			GLOBAL	$oper_bxsid8
$oper_bxsid8
			r0_from_bxidxdisp8 esi
			b		$jump
			GLOBAL	$oper_bxdid8
$oper_bxdid8
			r0_from_bxidxdisp8 edi
			b		$jump
			GLOBAL	$oper_bpsid8
$oper_bpsid8
			r0_from_bpidxdisp8 esi
			b		$jump_bp
			GLOBAL	$oper_bpdid8
$oper_bpdid8
			r0_from_bpidxdisp8 edi
			b		$jump_bp
			GLOBAL	$oper_sidisp8
$oper_sidisp8
			r0_from_idx_disp8 esi
			b		$jump
			GLOBAL	$oper_didisp8
$oper_didisp8
			r0_from_idx_disp8 edi
			b		$jump
			GLOBAL	$oper_bpdisp8
$oper_bpdisp8
			r0_from_idx_disp8 ebp
			b		$jump_bp
			GLOBAL	$oper_bxdisp8
$oper_bxdisp8
			r0_from_idx_disp8 ebx
			b		$jump
		; ----- 0x80 = disp16 -----
			GLOBAL	$oper_bxsid16
$oper_bxsid16
			r0_from_bxidxdisp16 esi
			b		$jump
			GLOBAL	$oper_bxdid16
$oper_bxdid16
			r0_from_bxidxdisp16 edi
			b		$jump
			GLOBAL	$oper_bpsid16
$oper_bpsid16
			r0_from_bpidxdisp16 esi
			b		$jump_bp
			GLOBAL	$oper_bpdid16
$oper_bpdid16
			r0_from_bpidxdisp16 edi
			b		$jump_bp
			GLOBAL	$oper_sidisp16
$oper_sidisp16
			r0_from_idx_disp16 esi
			b		$jump
			GLOBAL	$oper_didisp16
$oper_didisp16
			r0_from_idx_disp16 edi
			b		$jump
			GLOBAL	$oper_bpdisp16
$oper_bpdisp16
			r0_from_idx_disp16 ebp
			b		$jump_bp
			GLOBAL	$oper_bxdisp16
$oper_bxdisp16
			r0_from_idx_disp16 ebx
			b		$jump
	ELSE
		; ----- 0x00 -----
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
			GLOBAL	$oper_siidx_$reg
$oper_siidx_$reg
			mov		r0, esi
			b		$jump_$reg
			GLOBAL	$oper_diidx_$reg
$oper_diidx_$reg
			mov		r0, edi
			b		$jump_$reg
$oper_disp16_$reg
			r0_from_disp16
			b		$jump_$reg
			GLOBAL	$oper_bxidx_$reg
$oper_bxidx_$reg
			mov		r0, ebx
			b		$jump_$reg
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
			GLOBAL	$oper_sidisp8_$reg
$oper_sidisp8_$reg
			r0_from_idx_disp8 esi
			b		$jump_$reg
			GLOBAL	$oper_didisp8_$reg
$oper_didisp8_$reg
			r0_from_idx_disp8 edi
			b		$jump_$reg
			GLOBAL	$oper_bpdisp8_$reg
$oper_bpdisp8_$reg
			r0_from_idx_disp8 ebp
			b		$jump_bp_$reg
			GLOBAL	$oper_bxdisp8_$reg
$oper_bxdisp8_$reg
			r0_from_idx_disp8 ebx
			b		$jump_$reg
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
	ENDIF
	MEND

	MACRO
	modrm_reg_reg_gen $oper, $lr, $rr
	GLOBAL	$oper_$lr_$rr
$oper_$lr_$rr
	$oper_reg16_reg16 $lr, $rr
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
	MEND

	MACRO
	modrm_1_genall_old $oper, $jump
	modrm_1_help $oper, $jump, ax
	modrm_1_help $oper, $jump, cx
	modrm_1_help $oper, $jump, dx
	modrm_1_help $oper, $jump, bx
	modrm_1_help $oper, $jump, sp
	modrm_1_help $oper, $jump, bp
	modrm_1_help $oper, $jump, si
	modrm_1_help $oper, $jump, di
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
	MEND

; --------------------------- r8, r/m8 -------------------------------

	MACRO
	modrm_2_help $oper, $jump, $reg8, $reg
	IF "$oper" = "skip"
	ELSE
; ----- 0x00 -----
$oper_$reg8_bxsi
	add		r0, ebx, esi
	b		$jump_$reg
$oper_$reg8_bxdi
	add		r0, ebx, edi
	b		$jump_$reg
$oper_$reg8_bpsi
	add		r0, ebp, esi
	b		$jump_bp_$reg
$oper_$reg8_bpdi
	add		r0, ebp, edi
	b		$jump_bp_$reg
	GLOBAL	$oper_$reg8_siidx
$oper_$reg8_siidx
	mov		r0, esi
	b		$jump_$reg
	GLOBAL	$oper_$reg8_diidx
$oper_$reg8_diidx
	mov		r0, edi
	b		$jump_$reg
$oper_$reg8_disp16
	r0_from_disp16
	b		$jump_$reg
	GLOBAL	$oper_$reg8_bxidx
$oper_$reg8_bxidx
	mov		r0, ebx
	b		$jump_$reg
; ----- 0x40 = disp8 -----
$oper_$reg8_bxsid8
	r0_from_bxidxdisp8 esi
	b		$jump_$reg
$oper_$reg8_bxdid8
	r0_from_bxidxdisp8 edi
	b		$jump_$reg
$oper_$reg8_bpsid8
	r0_from_bpidxdisp8 esi
	b		$jump_bp_$reg
$oper_$reg8_bpdid8
	r0_from_bpidxdisp8 edi
	b		$jump_bp_$reg
	GLOBAL	$oper_$reg8_sidisp8
$oper_$reg8_sidisp8
	r0_from_idx_disp8 esi
	b		$jump_$reg
	GLOBAL	$oper_$reg8_didisp8
$oper_$reg8_didisp8
	r0_from_idx_disp8 edi
	b		$jump_$reg
	GLOBAL	$oper_$reg8_bpdisp8
$oper_$reg8_bpdisp8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	GLOBAL	$oper_$reg8_bxdisp8
$oper_$reg8_bxdisp8
	r0_from_idx_disp8 ebx
	b		$jump_$reg
; ----- 0x80 = disp16 -----
$oper_$reg8_bxsid16
	r0_from_bxidxdisp16 esi
	b		$jump_$reg
$oper_$reg8_bxdid16
	r0_from_bxidxdisp16 edi
	b		$jump_$reg
$oper_$reg8_bpsid16
	r0_from_bpidxdisp16 esi
	b		$jump_bp_$reg
$oper_$reg8_bpdid16
	r0_from_bpidxdisp16 edi
	b		$jump_bp_$reg
$oper_$reg8_sidisp16
	r0_from_idx_disp16 esi
	b		$jump_$reg
$oper_$reg8_didisp16
	r0_from_idx_disp16 edi
	b		$jump_$reg
$oper_$reg8_bpdisp16
	r0_from_idx_disp16 ebp
	b		$jump_bp_$reg
$oper_$reg8_bxdisp16
	r0_from_idx_disp16 ebx
	b		$jump_$reg
	ENDIF	
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

; --------------------------- r16, r/m16 -----------------------------

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
	GLOBAL	$oper_$reg_siidx
$oper_$reg_siidx
	mov		r0, esi
	b		$jump_$reg
	GLOBAL	$oper_$reg_diidx
$oper_$reg_diidx
	mov		r0, edi
	b		$jump_$reg
$oper_$reg_disp16
	r0_from_disp16
	b		$jump_$reg
	GLOBAL	$oper_$reg_bxidx
$oper_$reg_bxidx
	mov		r0, ebx
	b		$jump_$reg
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
	GLOBAL	$oper_$reg_sidisp8
$oper_$reg_sidisp8
	r0_from_idx_disp8 esi
	b		$jump_$reg
	GLOBAL	$oper_$reg_didisp8
$oper_$reg_didisp8
	r0_from_idx_disp8 edi
	b		$jump_$reg
	GLOBAL	$oper_$reg_bpdisp8
$oper_$reg_bpdisp8
	r0_from_idx_disp8 ebp
	b		$jump_bp_$reg
	GLOBAL	$oper_$reg_bxdisp8
$oper_$reg_bxdisp8
	r0_from_idx_disp8 ebx
	b		$jump_$reg
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
	MEND

	MACRO
	modrm_3_genall_old $oper, $jump
	modrm_3_help $oper, $jump, eax
	modrm_3_help $oper, $jump, ecx
	modrm_3_help $oper, $jump, edx
	modrm_3_help $oper, $jump, ebx
	modrm_3_help $oper, $jump, esp
	modrm_3_help $oper, $jump, ebp
	modrm_3_help $oper, $jump, esi
	modrm_3_help $oper, $jump, edi
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
	modrm_gen_reg_oper $oper, eax
	modrm_gen_reg_oper $oper, ecx
	modrm_gen_reg_oper $oper, edx
	modrm_gen_reg_oper $oper, ebx
	modrm_gen_reg_oper $oper, esp
	modrm_gen_reg_oper $oper, ebp
	modrm_gen_reg_oper $oper, esi
	modrm_gen_reg_oper $oper, edi
	MEND

	END
