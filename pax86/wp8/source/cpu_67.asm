;=============================================================================
; cpu_67.s
;
; This file contains normal opcode handlers for when running in a USE16 segment
; and a prefix 0x67 (32-bit addressing) is active. This is similar to a situation
; when running in a USE32 segment with a prefix 0x66 (32-bit operands) active.
; In principle this file only contains the memory address parsing for word-size
; opcodes (since 8-bit opcodes are similar to non-prefixed versions in "cpu_386.s")
; with the actual handlers being in "cpu.s".
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

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_67.inc

	EXTERN	loop
	EXTERN	unknown
	
	EXTERN	registers

; ------------------- 01 = ADD r/m16, r16 -----------------------------
;
op_01_67
	modrm_jump_32_tbl op_01_67_jump
	modrm_tbl_1_old add
; 0xC0 = two register operands
	DCD add_ax_ax, add_cx_ax, add_dx_ax, add_bx_ax, add_sp_ax, add_bp_ax, add_si_ax, add_di_ax
	DCD add_ax_cx, add_cx_cx, add_dx_cx, add_bx_cx, add_sp_cx, add_bp_cx, add_si_cx, add_di_cx
	DCD add_ax_dx, add_cx_dx, add_dx_dx, add_bx_dx, add_sp_dx, add_bp_dx, add_si_dx, add_di_dx
	DCD add_ax_bx, add_cx_bx, add_dx_bx, add_bx_bx, add_sp_bx, add_bp_bx, add_si_bx, add_di_bx
	DCD add_ax_sp, add_cx_sp, add_dx_sp, add_bx_sp, add_sp_sp, add_bp_sp, add_si_sp, add_di_sp
	DCD add_ax_bp, add_cx_bp, add_dx_bp, add_bx_bp, add_sp_bp, add_bp_bp, add_si_bp, add_di_bp
	DCD add_ax_si, add_cx_si, add_dx_si, add_bx_si, add_sp_si, add_bp_si, add_si_si, add_di_si
	DCD add_ax_di, add_cx_di, add_dx_di, add_bx_di, add_sp_di, add_bp_di, add_si_di, add_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old add, add_r0_r16

	LTORG
	
	EXTERN  add_ax_ax
	EXTERN  add_cx_ax
	EXTERN  add_dx_ax
	EXTERN  add_bx_ax
	EXTERN  add_sp_ax
	EXTERN  add_bp_ax
	EXTERN  add_si_ax
	EXTERN  add_di_ax
	EXTERN  add_ax_cx
	EXTERN  add_cx_cx 
	EXTERN  add_dx_cx 
	EXTERN  add_bx_cx 
	EXTERN  add_sp_cx 
	EXTERN  add_bp_cx 
	EXTERN  add_si_cx 
	EXTERN  add_di_cx
	EXTERN  add_ax_dx 
	EXTERN  add_cx_dx 
	EXTERN  add_dx_dx 
	EXTERN  add_bx_dx 
	EXTERN  add_sp_dx 
	EXTERN  add_bp_dx 
	EXTERN  add_si_dx 
	EXTERN  add_di_dx
	EXTERN  add_ax_bx 
	EXTERN  add_cx_bx 
	EXTERN  add_dx_bx 
	EXTERN  add_bx_bx 
	EXTERN  add_sp_bx 
	EXTERN  add_bp_bx 
	EXTERN  add_si_bx 
	EXTERN  add_di_bx
	EXTERN  add_ax_sp 
	EXTERN  add_cx_sp 
	EXTERN  add_dx_sp 
	EXTERN  add_bx_sp 
	EXTERN  add_sp_sp 
	EXTERN  add_bp_sp 
	EXTERN  add_si_sp 
	EXTERN  add_di_sp
	EXTERN  add_ax_bp 
	EXTERN  add_cx_bp 
	EXTERN  add_dx_bp 
	EXTERN  add_bx_bp 
	EXTERN  add_sp_bp 
	EXTERN  add_bp_bp 
	EXTERN  add_si_bp 
	EXTERN  add_di_bp
	EXTERN  add_ax_si 
	EXTERN  add_cx_si 
	EXTERN  add_dx_si 
	EXTERN  add_bx_si 
	EXTERN  add_sp_si 
	EXTERN  add_bp_si 
	EXTERN  add_si_si 
	EXTERN  add_di_si
	EXTERN  add_ax_di 
	EXTERN  add_cx_di 
	EXTERN  add_dx_di 
	EXTERN  add_bx_di 
	EXTERN  add_sp_di 
	EXTERN  add_bp_di 
	EXTERN  add_si_di 
	EXTERN  add_di_di

; ------------------- 03 = ADD r16, r/m16 -----------------------------
;
op_03_67
	modrm_jump_32_tbl op_03_67_jump
	modrm_tbl_3_old add
; 0xC0 = two register operands
	DCD add_ax_ax, add_ax_cx, add_ax_dx, add_ax_bx, add_ax_sp, add_ax_bp, add_ax_si, add_ax_di
	DCD add_cx_ax, add_cx_cx, add_cx_dx, add_cx_bx, add_cx_sp, add_cx_bp, add_cx_si, add_cx_di
	DCD add_dx_ax, add_dx_cx, add_dx_dx, add_dx_bx, add_dx_sp, add_dx_bp, add_dx_si, add_dx_di
	DCD add_bx_ax, add_bx_cx, add_bx_dx, add_bx_bx, add_bx_sp, add_bx_bp, add_bx_si, add_bx_di
	DCD add_sp_ax, add_sp_cx, add_sp_dx, add_sp_bx, add_sp_sp, add_sp_bp, add_sp_si, add_sp_di
	DCD add_bp_ax, add_bp_cx, add_bp_dx, add_bp_bx, add_bp_sp, add_bp_bp, add_bp_si, add_bp_di
	DCD add_si_ax, add_si_cx, add_si_dx, add_si_bx, add_si_sp, add_si_bp, add_si_si, add_si_di
	DCD add_di_ax, add_di_cx, add_di_dx, add_di_bx, add_di_sp, add_di_bp, add_di_si, add_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old add, add_r16_r0

	LTORG
	
; ------------------- 09 = OR r/m16, r16 -----------------------------
;
op_09_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_09_67_jump
	modrm_tbl_1_old or
; 0xC0 = two register operands
	DCD or_ax_ax, or_cx_ax, or_dx_ax, or_bx_ax, or_sp_ax, or_bp_ax, or_si_ax, or_di_ax
	DCD or_ax_cx, or_cx_cx, or_dx_cx, or_bx_cx, or_sp_cx, or_bp_cx, or_si_cx, or_di_cx
	DCD or_ax_dx, or_cx_dx, or_dx_dx, or_bx_dx, or_sp_dx, or_bp_dx, or_si_dx, or_di_dx
	DCD or_ax_bx, or_cx_bx, or_dx_bx, or_bx_bx, or_sp_bx, or_bp_bx, or_si_bx, or_di_bx
	DCD or_ax_sp, or_cx_sp, or_dx_sp, or_bx_sp, or_sp_sp, or_bp_sp, or_si_sp, or_di_sp
	DCD or_ax_bp, or_cx_bp, or_dx_bp, or_bx_bp, or_sp_bp, or_bp_bp, or_si_bp, or_di_bp
	DCD or_ax_si, or_cx_si, or_dx_si, or_bx_si, or_sp_si, or_bp_si, or_si_si, or_di_si
	DCD or_ax_di, or_cx_di, or_dx_di, or_bx_di, or_sp_di, or_bp_di, or_si_di, or_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old or, or_r0_r16

	LTORG
	
	EXTERN  or_ax_ax
	EXTERN  or_cx_ax
	EXTERN  or_dx_ax
	EXTERN  or_bx_ax
	EXTERN  or_sp_ax
	EXTERN  or_bp_ax
	EXTERN  or_si_ax
	EXTERN  or_di_ax
	EXTERN  or_ax_cx
	EXTERN  or_cx_cx 
	EXTERN  or_dx_cx 
	EXTERN  or_bx_cx 
	EXTERN  or_sp_cx 
	EXTERN  or_bp_cx 
	EXTERN  or_si_cx 
	EXTERN  or_di_cx
	EXTERN  or_ax_dx 
	EXTERN  or_cx_dx 
	EXTERN  or_dx_dx 
	EXTERN  or_bx_dx 
	EXTERN  or_sp_dx 
	EXTERN  or_bp_dx 
	EXTERN  or_si_dx 
	EXTERN  or_di_dx
	EXTERN  or_ax_bx 
	EXTERN  or_cx_bx 
	EXTERN  or_dx_bx 
	EXTERN  or_bx_bx 
	EXTERN  or_sp_bx 
	EXTERN  or_bp_bx 
	EXTERN  or_si_bx 
	EXTERN  or_di_bx
	EXTERN  or_ax_sp 
	EXTERN  or_cx_sp 
	EXTERN  or_dx_sp 
	EXTERN  or_bx_sp 
	EXTERN  or_sp_sp 
	EXTERN  or_bp_sp 
	EXTERN  or_si_sp 
	EXTERN  or_di_sp
	EXTERN  or_ax_bp 
	EXTERN  or_cx_bp 
	EXTERN  or_dx_bp 
	EXTERN  or_bx_bp 
	EXTERN  or_sp_bp 
	EXTERN  or_bp_bp 
	EXTERN  or_si_bp 
	EXTERN  or_di_bp
	EXTERN  or_ax_si 
	EXTERN  or_cx_si 
	EXTERN  or_dx_si 
	EXTERN  or_bx_si 
	EXTERN  or_sp_si 
	EXTERN  or_bp_si 
	EXTERN  or_si_si 
	EXTERN  or_di_si
	EXTERN  or_ax_di 
	EXTERN  or_cx_di 
	EXTERN  or_dx_di 
	EXTERN  or_bx_di 
	EXTERN  or_sp_di 
	EXTERN  or_bp_di 
	EXTERN  or_si_di 
	EXTERN  or_di_di

; ------------------- 0B = OR r16, r/m16 ------------------------------
;
op_0b_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_0b_67_jump
	modrm_tbl_3_old or
	DCD or_ax_ax, or_ax_cx, or_ax_dx, or_ax_bx, or_ax_sp, or_ax_bp, or_ax_si, or_ax_di
	DCD or_cx_ax, or_cx_cx, or_cx_dx, or_cx_bx, or_cx_sp, or_cx_bp, or_cx_si, or_cx_di
	DCD or_dx_ax, or_dx_cx, or_dx_dx, or_dx_bx, or_dx_sp, or_dx_bp, or_dx_si, or_dx_di
	DCD or_bx_ax, or_bx_cx, or_bx_dx, or_bx_bx, or_bx_sp, or_bx_bp, or_bx_si, or_bx_di
	DCD or_sp_ax, or_sp_cx, or_sp_dx, or_sp_bx, or_sp_sp, or_sp_bp, or_sp_si, or_sp_di
	DCD or_bp_ax, or_bp_cx, or_bp_dx, or_bp_bx, or_bp_sp, or_bp_bp, or_bp_si, or_bp_di
	DCD or_si_ax, or_si_cx, or_si_dx, or_si_bx, or_si_sp, or_si_bp, or_si_si, or_si_di
	DCD or_di_ax, or_di_cx, or_di_dx, or_di_bx, or_di_sp, or_di_bp, or_di_si, or_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old or, or_r16_r0

	LTORG
	
; ------------------- 11 = ADC r/m16, r16 ------------------------------
;
	GLOBAL	op_11_67
op_11_67
	modrm_jump_32_tbl op_11_67_jump
	modrm_tbl_1_old adc
	DCD adc_ax_ax, adc_cx_ax, adc_dx_ax, adc_bx_ax, adc_sp_ax, adc_bp_ax, adc_si_ax, adc_di_ax
	DCD adc_ax_cx, adc_cx_cx, adc_dx_cx, adc_bx_cx, adc_sp_cx, adc_bp_cx, adc_si_cx, adc_di_cx
	DCD adc_ax_dx, adc_cx_dx, adc_dx_dx, adc_bx_dx, adc_sp_dx, adc_bp_dx, adc_si_dx, adc_di_dx
	DCD adc_ax_bx, adc_cx_bx, adc_dx_bx, adc_bx_bx, adc_sp_bx, adc_bp_bx, adc_si_bx, adc_di_bx
	DCD adc_ax_sp, adc_cx_sp, adc_dx_sp, adc_bx_sp, adc_sp_sp, adc_bp_sp, adc_si_sp, adc_di_sp
	DCD adc_ax_bp, adc_cx_bp, adc_dx_bp, adc_bx_bp, adc_sp_bp, adc_bp_bp, adc_si_bp, adc_di_bp
	DCD adc_ax_si, adc_cx_si, adc_dx_si, adc_bx_si, adc_sp_si, adc_bp_si, adc_si_si, adc_di_si
	DCD adc_ax_di, adc_cx_di, adc_dx_di, adc_bx_di, adc_sp_di, adc_bp_di, adc_si_di, adc_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old adc, adc_r0_r16

	LTORG
	
	EXTERN  adc_ax_ax
	EXTERN  adc_cx_ax
	EXTERN  adc_dx_ax
	EXTERN  adc_bx_ax
	EXTERN  adc_sp_ax
	EXTERN  adc_bp_ax
	EXTERN  adc_si_ax
	EXTERN  adc_di_ax
	EXTERN  adc_ax_cx
	EXTERN  adc_cx_cx 
	EXTERN  adc_dx_cx 
	EXTERN  adc_bx_cx 
	EXTERN  adc_sp_cx 
	EXTERN  adc_bp_cx 
	EXTERN  adc_si_cx 
	EXTERN  adc_di_cx
	EXTERN  adc_ax_dx 
	EXTERN  adc_cx_dx 
	EXTERN  adc_dx_dx 
	EXTERN  adc_bx_dx 
	EXTERN  adc_sp_dx 
	EXTERN  adc_bp_dx 
	EXTERN  adc_si_dx 
	EXTERN  adc_di_dx
	EXTERN  adc_ax_bx 
	EXTERN  adc_cx_bx 
	EXTERN  adc_dx_bx 
	EXTERN  adc_bx_bx 
	EXTERN  adc_sp_bx 
	EXTERN  adc_bp_bx 
	EXTERN  adc_si_bx 
	EXTERN  adc_di_bx
	EXTERN  adc_ax_sp 
	EXTERN  adc_cx_sp 
	EXTERN  adc_dx_sp 
	EXTERN  adc_bx_sp 
	EXTERN  adc_sp_sp 
	EXTERN  adc_bp_sp 
	EXTERN  adc_si_sp 
	EXTERN  adc_di_sp
	EXTERN  adc_ax_bp 
	EXTERN  adc_cx_bp 
	EXTERN  adc_dx_bp 
	EXTERN  adc_bx_bp 
	EXTERN  adc_sp_bp 
	EXTERN  adc_bp_bp 
	EXTERN  adc_si_bp 
	EXTERN  adc_di_bp
	EXTERN  adc_ax_si 
	EXTERN  adc_cx_si 
	EXTERN  adc_dx_si 
	EXTERN  adc_bx_si 
	EXTERN  adc_sp_si 
	EXTERN  adc_bp_si 
	EXTERN  adc_si_si 
	EXTERN  adc_di_si
	EXTERN  adc_ax_di 
	EXTERN  adc_cx_di 
	EXTERN  adc_dx_di 
	EXTERN  adc_bx_di 
	EXTERN  adc_sp_di 
	EXTERN  adc_bp_di 
	EXTERN  adc_si_di 
	EXTERN  adc_di_di

; ------------------- 13 = ADC r16, r/m16 ------------------------------
;
	GLOBAL	op_13_67
op_13_67
	modrm_jump_32_tbl op_13_67_jump
	modrm_tbl_3_old adc
	DCD adc_ax_ax, adc_ax_cx, adc_ax_dx, adc_ax_bx, adc_ax_sp, adc_ax_bp, adc_ax_si, adc_ax_di
	DCD adc_cx_ax, adc_cx_cx, adc_cx_dx, adc_cx_bx, adc_cx_sp, adc_cx_bp, adc_cx_si, adc_cx_di
	DCD adc_dx_ax, adc_dx_cx, adc_dx_dx, adc_dx_bx, adc_dx_sp, adc_dx_bp, adc_dx_si, adc_dx_di
	DCD adc_bx_ax, adc_bx_cx, adc_bx_dx, adc_bx_bx, adc_bx_sp, adc_bx_bp, adc_bx_si, adc_bx_di
	DCD adc_sp_ax, adc_sp_cx, adc_sp_dx, adc_sp_bx, adc_sp_sp, adc_sp_bp, adc_sp_si, adc_sp_di
	DCD adc_bp_ax, adc_bp_cx, adc_bp_dx, adc_bp_bx, adc_bp_sp, adc_bp_bp, adc_bp_si, adc_bp_di
	DCD adc_si_ax, adc_si_cx, adc_si_dx, adc_si_bx, adc_si_sp, adc_si_bp, adc_si_si, adc_si_di
	DCD adc_di_ax, adc_di_cx, adc_di_dx, adc_di_bx, adc_di_sp, adc_di_bp, adc_di_si, adc_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old adc, adc_r16_r0

	LTORG
	
; ------------------- 19 = SBB r/m16, r16 ------------------------------
;
	GLOBAL	op_19_67
op_19_67
	modrm_jump_32_tbl op_19_67_jump
	modrm_tbl_1_old sbb
	DCD sbb_ax_ax, sbb_cx_ax, sbb_dx_ax, sbb_bx_ax, sbb_sp_ax, sbb_bp_ax, sbb_si_ax, sbb_di_ax
	DCD sbb_ax_cx, sbb_cx_cx, sbb_dx_cx, sbb_bx_cx, sbb_sp_cx, sbb_bp_cx, sbb_si_cx, sbb_di_cx
	DCD sbb_ax_dx, sbb_cx_dx, sbb_dx_dx, sbb_bx_dx, sbb_sp_dx, sbb_bp_dx, sbb_si_dx, sbb_di_dx
	DCD sbb_ax_bx, sbb_cx_bx, sbb_dx_bx, sbb_bx_bx, sbb_sp_bx, sbb_bp_bx, sbb_si_bx, sbb_di_bx
	DCD sbb_ax_sp, sbb_cx_sp, sbb_dx_sp, sbb_bx_sp, sbb_sp_sp, sbb_bp_sp, sbb_si_sp, sbb_di_sp
	DCD sbb_ax_bp, sbb_cx_bp, sbb_dx_bp, sbb_bx_bp, sbb_sp_bp, sbb_bp_bp, sbb_si_bp, sbb_di_bp
	DCD sbb_ax_si, sbb_cx_si, sbb_dx_si, sbb_bx_si, sbb_sp_si, sbb_bp_si, sbb_si_si, sbb_di_si
	DCD sbb_ax_di, sbb_cx_di, sbb_dx_di, sbb_bx_di, sbb_sp_di, sbb_bp_di, sbb_si_di, sbb_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old sbb, sbb_r0_r16

	LTORG
	
	EXTERN  sbb_ax_ax
	EXTERN  sbb_cx_ax
	EXTERN  sbb_dx_ax
	EXTERN  sbb_bx_ax
	EXTERN  sbb_sp_ax
	EXTERN  sbb_bp_ax
	EXTERN  sbb_si_ax
	EXTERN  sbb_di_ax
	EXTERN  sbb_ax_cx
	EXTERN  sbb_cx_cx 
	EXTERN  sbb_dx_cx 
	EXTERN  sbb_bx_cx 
	EXTERN  sbb_sp_cx 
	EXTERN  sbb_bp_cx 
	EXTERN  sbb_si_cx 
	EXTERN  sbb_di_cx
	EXTERN  sbb_ax_dx 
	EXTERN  sbb_cx_dx 
	EXTERN  sbb_dx_dx 
	EXTERN  sbb_bx_dx 
	EXTERN  sbb_sp_dx 
	EXTERN  sbb_bp_dx 
	EXTERN  sbb_si_dx 
	EXTERN  sbb_di_dx
	EXTERN  sbb_ax_bx 
	EXTERN  sbb_cx_bx 
	EXTERN  sbb_dx_bx 
	EXTERN  sbb_bx_bx 
	EXTERN  sbb_sp_bx 
	EXTERN  sbb_bp_bx 
	EXTERN  sbb_si_bx 
	EXTERN  sbb_di_bx
	EXTERN  sbb_ax_sp 
	EXTERN  sbb_cx_sp 
	EXTERN  sbb_dx_sp 
	EXTERN  sbb_bx_sp 
	EXTERN  sbb_sp_sp 
	EXTERN  sbb_bp_sp 
	EXTERN  sbb_si_sp 
	EXTERN  sbb_di_sp
	EXTERN  sbb_ax_bp 
	EXTERN  sbb_cx_bp 
	EXTERN  sbb_dx_bp 
	EXTERN  sbb_bx_bp 
	EXTERN  sbb_sp_bp 
	EXTERN  sbb_bp_bp 
	EXTERN  sbb_si_bp 
	EXTERN  sbb_di_bp
	EXTERN  sbb_ax_si 
	EXTERN  sbb_cx_si 
	EXTERN  sbb_dx_si 
	EXTERN  sbb_bx_si 
	EXTERN  sbb_sp_si 
	EXTERN  sbb_bp_si 
	EXTERN  sbb_si_si 
	EXTERN  sbb_di_si
	EXTERN  sbb_ax_di 
	EXTERN  sbb_cx_di 
	EXTERN  sbb_dx_di 
	EXTERN  sbb_bx_di 
	EXTERN  sbb_sp_di 
	EXTERN  sbb_bp_di 
	EXTERN  sbb_si_di 
	EXTERN  sbb_di_di

; ------------------- 1B = SBB r16, r/m16 ------------------------------
;
	GLOBAL	op_1b_67
op_1b_67
	modrm_jump_32_tbl op_1b_67_jump
	modrm_tbl_3_old sbb
	DCD sbb_ax_ax, sbb_ax_cx, sbb_ax_dx, sbb_ax_bx, sbb_ax_sp, sbb_ax_bp, sbb_ax_si, sbb_ax_di
	DCD sbb_cx_ax, sbb_cx_cx, sbb_cx_dx, sbb_cx_bx, sbb_cx_sp, sbb_cx_bp, sbb_cx_si, sbb_cx_di
	DCD sbb_dx_ax, sbb_dx_cx, sbb_dx_dx, sbb_dx_bx, sbb_dx_sp, sbb_dx_bp, sbb_dx_si, sbb_dx_di
	DCD sbb_bx_ax, sbb_bx_cx, sbb_bx_dx, sbb_bx_bx, sbb_bx_sp, sbb_bx_bp, sbb_bx_si, sbb_bx_di
	DCD sbb_sp_ax, sbb_sp_cx, sbb_sp_dx, sbb_sp_bx, sbb_sp_sp, sbb_sp_bp, sbb_sp_si, sbb_sp_di
	DCD sbb_bp_ax, sbb_bp_cx, sbb_bp_dx, sbb_bp_bx, sbb_bp_sp, sbb_bp_bp, sbb_bp_si, sbb_bp_di
	DCD sbb_si_ax, sbb_si_cx, sbb_si_dx, sbb_si_bx, sbb_si_sp, sbb_si_bp, sbb_si_si, sbb_si_di
	DCD sbb_di_ax, sbb_di_cx, sbb_di_dx, sbb_di_bx, sbb_di_sp, sbb_di_bp, sbb_di_si, sbb_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old sbb, sbb_r16_r0

	LTORG
	
; ------------------- 21 = AND r/m16, r16 ------------------------------
;
	GLOBAL	op_21_67
op_21_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_21_67_jump
	modrm_tbl_1_old and
	DCD and_ax_ax, and_cx_ax, and_dx_ax, and_bx_ax, and_sp_ax, and_bp_ax, and_si_ax, and_di_ax
	DCD and_ax_cx, and_cx_cx, and_dx_cx, and_bx_cx, and_sp_cx, and_bp_cx, and_si_cx, and_di_cx
	DCD and_ax_dx, and_cx_dx, and_dx_dx, and_bx_dx, and_sp_dx, and_bp_dx, and_si_dx, and_di_dx
	DCD and_ax_bx, and_cx_bx, and_dx_bx, and_bx_bx, and_sp_bx, and_bp_bx, and_si_bx, and_di_bx
	DCD and_ax_sp, and_cx_sp, and_dx_sp, and_bx_sp, and_sp_sp, and_bp_sp, and_si_sp, and_di_sp
	DCD and_ax_bp, and_cx_bp, and_dx_bp, and_bx_bp, and_sp_bp, and_bp_bp, and_si_bp, and_di_bp
	DCD and_ax_si, and_cx_si, and_dx_si, and_bx_si, and_sp_si, and_bp_si, and_si_si, and_di_si
	DCD and_ax_di, and_cx_di, and_dx_di, and_bx_di, and_sp_di, and_bp_di, and_si_di, and_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old and, and_r0_r16

	LTORG
	
	EXTERN  and_ax_ax
	EXTERN  and_cx_ax
	EXTERN  and_dx_ax
	EXTERN  and_bx_ax
	EXTERN  and_sp_ax
	EXTERN  and_bp_ax
	EXTERN  and_si_ax
	EXTERN  and_di_ax
	EXTERN  and_ax_cx
	EXTERN  and_cx_cx 
	EXTERN  and_dx_cx 
	EXTERN  and_bx_cx 
	EXTERN  and_sp_cx 
	EXTERN  and_bp_cx 
	EXTERN  and_si_cx 
	EXTERN  and_di_cx
	EXTERN  and_ax_dx 
	EXTERN  and_cx_dx 
	EXTERN  and_dx_dx 
	EXTERN  and_bx_dx 
	EXTERN  and_sp_dx 
	EXTERN  and_bp_dx 
	EXTERN  and_si_dx 
	EXTERN  and_di_dx
	EXTERN  and_ax_bx 
	EXTERN  and_cx_bx 
	EXTERN  and_dx_bx 
	EXTERN  and_bx_bx 
	EXTERN  and_sp_bx 
	EXTERN  and_bp_bx 
	EXTERN  and_si_bx 
	EXTERN  and_di_bx
	EXTERN  and_ax_sp 
	EXTERN  and_cx_sp 
	EXTERN  and_dx_sp 
	EXTERN  and_bx_sp 
	EXTERN  and_sp_sp 
	EXTERN  and_bp_sp 
	EXTERN  and_si_sp 
	EXTERN  and_di_sp
	EXTERN  and_ax_bp 
	EXTERN  and_cx_bp 
	EXTERN  and_dx_bp 
	EXTERN  and_bx_bp 
	EXTERN  and_sp_bp 
	EXTERN  and_bp_bp 
	EXTERN  and_si_bp 
	EXTERN  and_di_bp
	EXTERN  and_ax_si 
	EXTERN  and_cx_si 
	EXTERN  and_dx_si 
	EXTERN  and_bx_si 
	EXTERN  and_sp_si 
	EXTERN  and_bp_si 
	EXTERN  and_si_si 
	EXTERN  and_di_si
	EXTERN  and_ax_di 
	EXTERN  and_cx_di 
	EXTERN  and_dx_di 
	EXTERN  and_bx_di 
	EXTERN  and_sp_di 
	EXTERN  and_bp_di 
	EXTERN  and_si_di 
	EXTERN  and_di_di

; ------------------- 23 = AND r16, r/m16 ------------------------------
;
op_23_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_23_67_jump
	modrm_tbl_3_old and
	DCD and_ax_ax, and_ax_cx, and_ax_dx, and_ax_bx, and_ax_sp, and_ax_bp, and_ax_si, and_ax_di
	DCD and_cx_ax, and_cx_cx, and_cx_dx, and_cx_bx, and_cx_sp, and_cx_bp, and_cx_si, and_cx_di
	DCD and_dx_ax, and_dx_cx, and_dx_dx, and_dx_bx, and_dx_sp, and_dx_bp, and_dx_si, and_dx_di
	DCD and_bx_ax, and_bx_cx, and_bx_dx, and_bx_bx, and_bx_sp, and_bx_bp, and_bx_si, and_bx_di
	DCD and_sp_ax, and_sp_cx, and_sp_dx, and_sp_bx, and_sp_sp, and_sp_bp, and_sp_si, and_sp_di
	DCD and_bp_ax, and_bp_cx, and_bp_dx, and_bp_bx, and_bp_sp, and_bp_bp, and_bp_si, and_bp_di
	DCD and_si_ax, and_si_cx, and_si_dx, and_si_bx, and_si_sp, and_si_bp, and_si_si, and_si_di
	DCD and_di_ax, and_di_cx, and_di_dx, and_di_bx, and_di_sp, and_di_bp, and_di_si, and_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old and, and_r16_r0

	LTORG
	
; ------------------- 29 = SUB r/m16, r16 -----------------------------
op_29_67
	modrm_jump_32_tbl op_29_67_jump
	modrm_tbl_1_old sub
	DCD sub_ax_ax, sub_cx_ax, sub_dx_ax, sub_bx_ax, sub_sp_ax, sub_bp_ax, sub_si_ax, sub_di_ax
	DCD sub_ax_cx, sub_cx_cx, sub_dx_cx, sub_bx_cx, sub_sp_cx, sub_bp_cx, sub_si_cx, sub_di_cx
	DCD sub_ax_dx, sub_cx_dx, sub_dx_dx, sub_bx_dx, sub_sp_dx, sub_bp_dx, sub_si_dx, sub_di_dx
	DCD sub_ax_bx, sub_cx_bx, sub_dx_bx, sub_bx_bx, sub_sp_bx, sub_bp_bx, sub_si_bx, sub_di_bx
	DCD sub_ax_sp, sub_cx_sp, sub_dx_sp, sub_bx_sp, sub_sp_sp, sub_bp_sp, sub_si_sp, sub_di_sp
	DCD sub_ax_bp, sub_cx_bp, sub_dx_bp, sub_bx_bp, sub_sp_bp, sub_bp_bp, sub_si_bp, sub_di_bp
	DCD sub_ax_si, sub_cx_si, sub_dx_si, sub_bx_si, sub_sp_si, sub_bp_si, sub_si_si, sub_di_si
	DCD sub_ax_di, sub_cx_di, sub_dx_di, sub_bx_di, sub_sp_di, sub_bp_di, sub_si_di, sub_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old sub, sub_r0_r16

	LTORG
	
	EXTERN  sub_ax_ax
	EXTERN  sub_cx_ax
	EXTERN  sub_dx_ax
	EXTERN  sub_bx_ax
	EXTERN  sub_sp_ax
	EXTERN  sub_bp_ax
	EXTERN  sub_si_ax
	EXTERN  sub_di_ax
	EXTERN  sub_ax_cx
	EXTERN  sub_cx_cx 
	EXTERN  sub_dx_cx 
	EXTERN  sub_bx_cx 
	EXTERN  sub_sp_cx 
	EXTERN  sub_bp_cx 
	EXTERN  sub_si_cx 
	EXTERN  sub_di_cx
	EXTERN  sub_ax_dx 
	EXTERN  sub_cx_dx 
	EXTERN  sub_dx_dx 
	EXTERN  sub_bx_dx 
	EXTERN  sub_sp_dx 
	EXTERN  sub_bp_dx 
	EXTERN  sub_si_dx 
	EXTERN  sub_di_dx
	EXTERN  sub_ax_bx 
	EXTERN  sub_cx_bx 
	EXTERN  sub_dx_bx 
	EXTERN  sub_bx_bx 
	EXTERN  sub_sp_bx 
	EXTERN  sub_bp_bx 
	EXTERN  sub_si_bx 
	EXTERN  sub_di_bx
	EXTERN  sub_ax_sp 
	EXTERN  sub_cx_sp 
	EXTERN  sub_dx_sp 
	EXTERN  sub_bx_sp 
	EXTERN  sub_sp_sp 
	EXTERN  sub_bp_sp 
	EXTERN  sub_si_sp 
	EXTERN  sub_di_sp
	EXTERN  sub_ax_bp 
	EXTERN  sub_cx_bp 
	EXTERN  sub_dx_bp 
	EXTERN  sub_bx_bp 
	EXTERN  sub_sp_bp 
	EXTERN  sub_bp_bp 
	EXTERN  sub_si_bp 
	EXTERN  sub_di_bp
	EXTERN  sub_ax_si 
	EXTERN  sub_cx_si 
	EXTERN  sub_dx_si 
	EXTERN  sub_bx_si 
	EXTERN  sub_sp_si 
	EXTERN  sub_bp_si 
	EXTERN  sub_si_si 
	EXTERN  sub_di_si
	EXTERN  sub_ax_di 
	EXTERN  sub_cx_di 
	EXTERN  sub_dx_di 
	EXTERN  sub_bx_di 
	EXTERN  sub_sp_di 
	EXTERN  sub_bp_di 
	EXTERN  sub_si_di 
	EXTERN  sub_di_di

; ------------------- 2B = SUB r16, r/m16 ------------------------------
;
op_2b_67
	modrm_jump_32_tbl op_2b_67_jump
	modrm_tbl_3_old sub
	DCD sub_ax_ax, sub_ax_cx, sub_ax_dx, sub_ax_bx, sub_ax_sp, sub_ax_bp, sub_ax_si, sub_ax_di
	DCD sub_cx_ax, sub_cx_cx, sub_cx_dx, sub_cx_bx, sub_cx_sp, sub_cx_bp, sub_cx_si, sub_cx_di
	DCD sub_dx_ax, sub_dx_cx, sub_dx_dx, sub_dx_bx, sub_dx_sp, sub_dx_bp, sub_dx_si, sub_dx_di
	DCD sub_bx_ax, sub_bx_cx, sub_bx_dx, sub_bx_bx, sub_bx_sp, sub_bx_bp, sub_bx_si, sub_bx_di
	DCD sub_sp_ax, sub_sp_cx, sub_sp_dx, sub_sp_bx, sub_sp_sp, sub_sp_bp, sub_sp_si, sub_sp_di
	DCD sub_bp_ax, sub_bp_cx, sub_bp_dx, sub_bp_bx, sub_bp_sp, sub_bp_bp, sub_bp_si, sub_bp_di
	DCD sub_si_ax, sub_si_cx, sub_si_dx, sub_si_bx, sub_si_sp, sub_si_bp, sub_si_si, sub_si_di
	DCD sub_di_ax, sub_di_cx, sub_di_dx, sub_di_bx, sub_di_sp, sub_di_bp, sub_di_si, sub_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old sub, sub_r16_r0

	LTORG
	
; ------------------- 31 = XOR r/m16, r16 ----------------------------
;
op_31_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_31_67_jump
	modrm_tbl_1_old xor
; 0xC0 = two register operands
	DCD xor_ax_ax, xor_cx_ax, xor_dx_ax, xor_bx_ax, xor_sp_ax, xor_bp_ax, xor_si_ax, xor_di_ax
	DCD xor_ax_cx, xor_cx_cx, xor_dx_cx, xor_bx_cx, xor_sp_cx, xor_bp_cx, xor_si_cx, xor_di_cx
	DCD xor_ax_dx, xor_cx_dx, xor_dx_dx, xor_bx_dx, xor_sp_dx, xor_bp_dx, xor_si_dx, xor_di_dx
	DCD xor_ax_bx, xor_cx_bx, xor_dx_bx, xor_bx_bx, xor_sp_bx, xor_bp_bx, xor_si_bx, xor_di_bx
	DCD xor_ax_sp, xor_cx_sp, xor_dx_sp, xor_bx_sp, xor_sp_sp, xor_bp_sp, xor_si_sp, xor_di_sp
	DCD xor_ax_bp, xor_cx_bp, xor_dx_bp, xor_bx_bp, xor_sp_bp, xor_bp_bp, xor_si_bp, xor_di_bp
	DCD xor_ax_si, xor_cx_si, xor_dx_si, xor_bx_si, xor_sp_si, xor_bp_si, xor_si_si, xor_di_si
	DCD xor_ax_di, xor_cx_di, xor_dx_di, xor_bx_di, xor_sp_di, xor_bp_di, xor_si_di, xor_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old xor, xor_r0_r16

	LTORG
	
	EXTERN  xor_ax_ax
	EXTERN  xor_cx_ax
	EXTERN  xor_dx_ax
	EXTERN  xor_bx_ax
	EXTERN  xor_sp_ax
	EXTERN  xor_bp_ax
	EXTERN  xor_si_ax
	EXTERN  xor_di_ax
	EXTERN  xor_ax_cx
	EXTERN  xor_cx_cx 
	EXTERN  xor_dx_cx 
	EXTERN  xor_bx_cx 
	EXTERN  xor_sp_cx 
	EXTERN  xor_bp_cx 
	EXTERN  xor_si_cx 
	EXTERN  xor_di_cx
	EXTERN  xor_ax_dx 
	EXTERN  xor_cx_dx 
	EXTERN  xor_dx_dx 
	EXTERN  xor_bx_dx 
	EXTERN  xor_sp_dx 
	EXTERN  xor_bp_dx 
	EXTERN  xor_si_dx 
	EXTERN  xor_di_dx
	EXTERN  xor_ax_bx 
	EXTERN  xor_cx_bx 
	EXTERN  xor_dx_bx 
	EXTERN  xor_bx_bx 
	EXTERN  xor_sp_bx 
	EXTERN  xor_bp_bx 
	EXTERN  xor_si_bx 
	EXTERN  xor_di_bx
	EXTERN  xor_ax_sp 
	EXTERN  xor_cx_sp 
	EXTERN  xor_dx_sp 
	EXTERN  xor_bx_sp 
	EXTERN  xor_sp_sp 
	EXTERN  xor_bp_sp 
	EXTERN  xor_si_sp 
	EXTERN  xor_di_sp
	EXTERN  xor_ax_bp 
	EXTERN  xor_cx_bp 
	EXTERN  xor_dx_bp 
	EXTERN  xor_bx_bp 
	EXTERN  xor_sp_bp 
	EXTERN  xor_bp_bp 
	EXTERN  xor_si_bp 
	EXTERN  xor_di_bp
	EXTERN  xor_ax_si 
	EXTERN  xor_cx_si 
	EXTERN  xor_dx_si 
	EXTERN  xor_bx_si 
	EXTERN  xor_sp_si 
	EXTERN  xor_bp_si 
	EXTERN  xor_si_si 
	EXTERN  xor_di_si
	EXTERN  xor_ax_di 
	EXTERN  xor_cx_di 
	EXTERN  xor_dx_di 
	EXTERN  xor_bx_di 
	EXTERN  xor_sp_di 
	EXTERN  xor_bp_di 
	EXTERN  xor_si_di 
	EXTERN  xor_di_di

; ------------------- 33 = XOR r16, r/m16 ----------------------------
;
	GLOBAL	op_33_67
op_33_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_33_67_jump
	modrm_tbl_3_old xor
; 0xC0 = two register operands
	DCD xor_ax_ax, xor_ax_cx, xor_ax_dx, xor_ax_bx, xor_ax_sp, xor_ax_bp, xor_ax_si, xor_ax_di
	DCD xor_cx_ax, xor_cx_cx, xor_cx_dx, xor_cx_bx, xor_cx_sp, xor_cx_bp, xor_cx_si, xor_cx_di
	DCD xor_dx_ax, xor_dx_cx, xor_dx_dx, xor_dx_bx, xor_dx_sp, xor_dx_bp, xor_dx_si, xor_dx_di
	DCD xor_bx_ax, xor_bx_cx, xor_bx_dx, xor_bx_bx, xor_bx_sp, xor_bx_bp, xor_bx_si, xor_bx_di
	DCD xor_sp_ax, xor_sp_cx, xor_sp_dx, xor_sp_bx, xor_sp_sp, xor_sp_bp, xor_sp_si, xor_sp_di
	DCD xor_bp_ax, xor_bp_cx, xor_bp_dx, xor_bp_bx, xor_bp_sp, xor_bp_bp, xor_bp_si, xor_bp_di
	DCD xor_si_ax, xor_si_cx, xor_si_dx, xor_si_bx, xor_si_sp, xor_si_bp, xor_si_si, xor_si_di
	DCD xor_di_ax, xor_di_cx, xor_di_dx, xor_di_bx, xor_di_sp, xor_di_bp, xor_di_si, xor_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old xor, xor_r16_r0

	LTORG
	
; ------------------- 39 = CMP r/m16, r16 ----------------------------
;
op_39_67
	modrm_jump_32_tbl op_39_67_jump
	modrm_tbl_1_old cmp
	; 0xC0 = two register operands
	DCD cmp_ax_ax, cmp_cx_ax, cmp_dx_ax, cmp_bx_ax, cmp_sp_ax, cmp_bp_ax, cmp_si_ax, cmp_di_ax
	DCD cmp_ax_cx, cmp_cx_cx, cmp_dx_cx, cmp_bx_cx, cmp_sp_cx, cmp_bp_cx, cmp_si_cx, cmp_di_cx
	DCD cmp_ax_dx, cmp_cx_dx, cmp_dx_dx, cmp_bx_dx, cmp_sp_dx, cmp_bp_dx, cmp_si_dx, cmp_di_dx
	DCD cmp_ax_bx, cmp_cx_bx, cmp_dx_bx, cmp_bx_bx, cmp_sp_bx, cmp_bp_bx, cmp_si_bx, cmp_di_bx
	DCD cmp_ax_sp, cmp_cx_sp, cmp_dx_sp, cmp_bx_sp, cmp_sp_sp, cmp_bp_sp, cmp_si_sp, cmp_di_sp
	DCD cmp_ax_bp, cmp_cx_bp, cmp_dx_bp, cmp_bx_bp, cmp_sp_bp, cmp_bp_bp, cmp_si_bp, cmp_di_bp
	DCD cmp_ax_si, cmp_cx_si, cmp_dx_si, cmp_bx_si, cmp_sp_si, cmp_bp_si, cmp_si_si, cmp_di_si
	DCD cmp_ax_di, cmp_cx_di, cmp_dx_di, cmp_bx_di, cmp_sp_di, cmp_bp_di, cmp_si_di, cmp_di_di
	
	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old cmp, cmp_r0_r16

	LTORG
	
	EXTERN  cmp_ax_ax
	EXTERN  cmp_cx_ax
	EXTERN  cmp_dx_ax
	EXTERN  cmp_bx_ax
	EXTERN  cmp_sp_ax
	EXTERN  cmp_bp_ax
	EXTERN  cmp_si_ax
	EXTERN  cmp_di_ax
	EXTERN  cmp_ax_cx
	EXTERN  cmp_cx_cx 
	EXTERN  cmp_dx_cx 
	EXTERN  cmp_bx_cx 
	EXTERN  cmp_sp_cx 
	EXTERN  cmp_bp_cx 
	EXTERN  cmp_si_cx 
	EXTERN  cmp_di_cx
	EXTERN  cmp_ax_dx 
	EXTERN  cmp_cx_dx 
	EXTERN  cmp_dx_dx 
	EXTERN  cmp_bx_dx 
	EXTERN  cmp_sp_dx 
	EXTERN  cmp_bp_dx 
	EXTERN  cmp_si_dx 
	EXTERN  cmp_di_dx
	EXTERN  cmp_ax_bx 
	EXTERN  cmp_cx_bx 
	EXTERN  cmp_dx_bx 
	EXTERN  cmp_bx_bx 
	EXTERN  cmp_sp_bx 
	EXTERN  cmp_bp_bx 
	EXTERN  cmp_si_bx 
	EXTERN  cmp_di_bx
	EXTERN  cmp_ax_sp 
	EXTERN  cmp_cx_sp 
	EXTERN  cmp_dx_sp 
	EXTERN  cmp_bx_sp 
	EXTERN  cmp_sp_sp 
	EXTERN  cmp_bp_sp 
	EXTERN  cmp_si_sp 
	EXTERN  cmp_di_sp
	EXTERN  cmp_ax_bp 
	EXTERN  cmp_cx_bp 
	EXTERN  cmp_dx_bp 
	EXTERN  cmp_bx_bp 
	EXTERN  cmp_sp_bp 
	EXTERN  cmp_bp_bp 
	EXTERN  cmp_si_bp 
	EXTERN  cmp_di_bp
	EXTERN  cmp_ax_si 
	EXTERN  cmp_cx_si 
	EXTERN  cmp_dx_si 
	EXTERN  cmp_bx_si 
	EXTERN  cmp_sp_si 
	EXTERN  cmp_bp_si 
	EXTERN  cmp_si_si 
	EXTERN  cmp_di_si
	EXTERN  cmp_ax_di 
	EXTERN  cmp_cx_di 
	EXTERN  cmp_dx_di 
	EXTERN  cmp_bx_di 
	EXTERN  cmp_sp_di 
	EXTERN  cmp_bp_di 
	EXTERN  cmp_si_di 
	EXTERN  cmp_di_di

; ------------------- 3B = CMP r16, r/m16 ----------------------------
;
op_3b_67
	modrm_jump_32_tbl op_3b_67_jump
	modrm_tbl_3_old cmp
; 0xC0 = two register operands
	DCD cmp_ax_ax, cmp_ax_cx, cmp_ax_dx, cmp_ax_bx, cmp_ax_sp, cmp_ax_bp, cmp_ax_si, cmp_ax_di
	DCD cmp_cx_ax, cmp_cx_cx, cmp_cx_dx, cmp_cx_bx, cmp_cx_sp, cmp_cx_bp, cmp_cx_si, cmp_cx_di
	DCD cmp_dx_ax, cmp_dx_cx, cmp_dx_dx, cmp_dx_bx, cmp_dx_sp, cmp_dx_bp, cmp_dx_si, cmp_dx_di
	DCD cmp_bx_ax, cmp_bx_cx, cmp_bx_dx, cmp_bx_bx, cmp_bx_sp, cmp_bx_bp, cmp_bx_si, cmp_bx_di
	DCD cmp_sp_ax, cmp_sp_cx, cmp_sp_dx, cmp_sp_bx, cmp_sp_sp, cmp_sp_bp, cmp_sp_si, cmp_sp_di
	DCD cmp_bp_ax, cmp_bp_cx, cmp_bp_dx, cmp_bp_bx, cmp_bp_sp, cmp_bp_bp, cmp_bp_si, cmp_bp_di
	DCD cmp_si_ax, cmp_si_cx, cmp_si_dx, cmp_si_bx, cmp_si_sp, cmp_si_bp, cmp_si_si, cmp_si_di
	DCD cmp_di_ax, cmp_di_cx, cmp_di_dx, cmp_di_bx, cmp_di_sp, cmp_di_bp, cmp_di_si, cmp_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old cmp, cmp_r16_r0

	LTORG
	
; ------------------- Segment Overrides using "opcodetable_16_32" -----------------------

op_26_67
	ldr		r2, [sp, #SP_ES_BASE]				; r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_2e_67
	ldr		r2, [sp, #SP_CS_BASE]				; r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_36_67
	ldr		r2, [sp, #SP_SS_BASE]				; r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_3e_67
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; r2 = logical DS segment in high halfword
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_64_67
	ldr		r2, [sp, #SP_FS_BASE]				; r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_65_67
	ldr		r2, [sp, #SP_GS_BASE]				; r2 = current effective logical GS segment
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	mov		lr, r2								; BP-relative segment base equals DS-relative segment base
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler


; ------------------- 67 = Address-size Prefix ------------------------
	GLOBAL	op_67_USE16
	GLOBAL	op_66_USE32
op_67_USE16
op_66_USE32
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_32				; (opcodetable_16_32 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler


	IF 0 = 1

op_67_67
	lw		t1, GP_CODE_BIG(gp)			; Are we in a USE32 segment?
	get_cseip_byte r0					; Get the opcode byte into r0 register
	beqz	t1, 1b						; USE16 segment, so use normal op_67
	lw		t1, GP_OP_66_67(gp)			; USE32 segment, so op_67_67 is actually op_66_67 (which is USE16 opcode)
	sll		r0, 2						; r0 = 4*opcode
	addu	t1, r0
	lw		t1, 0(t1)					; t1 = jump address (from the 67-prefix table)
	jr		t1							; Jump to the opcode handler

	ENDIF

	LTORG
	
	EXTERN op_00_USE32
	GLOBAL	op_01_67
	EXTERN op_02_USE32
	GLOBAL	op_03_67
	EXTERN op_04
	EXTERN op_05
	EXTERN op_06
	EXTERN op_07
	EXTERN op_08_USE32
	GLOBAL 	op_09_67
	EXTERN op_0a_USE32
	GLOBAL	op_0b_67
	EXTERN op_0c
	EXTERN op_0d
	EXTERN op_0e
	EXTERN op_0f_67
	EXTERN op_10_USE32
	EXTERN op_12_USE32
	EXTERN op_14
	EXTERN op_15
	EXTERN op_16
	EXTERN op_17
	EXTERN op_18_USE32
	EXTERN op_1a_USE32
	EXTERN op_1c
	EXTERN op_1d
	EXTERN op_1e
	EXTERN op_1f
	EXTERN op_20_USE32
	EXTERN op_22_USE32
	GLOBAL 	op_23_67
	EXTERN op_24
	EXTERN op_25
	GLOBAL	op_26_67
	EXTERN op_27
	EXTERN op_28_USE32
	GLOBAL 	op_29_67
	EXTERN op_2a_USE32
	GLOBAL	op_2b_67
	EXTERN op_2c
	EXTERN op_2d
	EXTERN op_2f
	EXTERN op_30_USE32
	GLOBAL 	op_31_67
	EXTERN op_32_USE32
	EXTERN op_34
	EXTERN op_35
	GLOBAL	op_36_67
	EXTERN op_37
	EXTERN op_38_USE32
	GLOBAL 	op_39_67
	EXTERN op_3a_USE32
	GLOBAL	op_3b_67
	EXTERN op_3c
	EXTERN op_3d
	EXTERN op_3f
	EXTERN  op_40
	EXTERN  op_41
	EXTERN  op_42
	EXTERN  op_43
	EXTERN  op_44
	EXTERN  op_45
	EXTERN  op_46
	EXTERN  op_47
	EXTERN  op_48
	EXTERN  op_49
	EXTERN  op_4a
	EXTERN  op_4b
	EXTERN  op_4c
	EXTERN  op_4d
	EXTERN  op_4e
	EXTERN  op_4f
	EXTERN  op_50
	EXTERN  op_51
	EXTERN  op_52
	EXTERN  op_53
	EXTERN  op_54
	EXTERN  op_55
	EXTERN  op_56
	EXTERN  op_57
	EXTERN  op_58
	EXTERN  op_59
	EXTERN  op_5a
	EXTERN  op_5b
	EXTERN  op_5c
	EXTERN  op_5d
	EXTERN  op_5e
	EXTERN  op_5f
	EXTERN  op_60
	EXTERN  op_61
	EXTERN	op_66_67_USE16
	EXTERN	op_66_67_USE32
	EXTERN  op_68
	EXTERN  op_6a
	EXTERN  op_70
	EXTERN  op_71
	EXTERN  op_72
	EXTERN  op_73
	EXTERN  op_74
	EXTERN  op_75
	EXTERN  op_76
	EXTERN  op_77
	EXTERN  op_78
	EXTERN  op_79
	EXTERN  op_7a
	EXTERN  op_7b
	EXTERN  op_7c
	EXTERN  op_7d
	EXTERN  op_7e
	EXTERN  op_7f
	EXTERN op_80_USE32
	EXTERN op_82_USE32
	EXTERN op_84_USE32
	EXTERN op_86_USE32
	EXTERN op_88_USE32
	GLOBAL 	op_89_67
	EXTERN op_8a_USE32
	GLOBAL	op_8b_67
	GLOBAL	op_8c_67
	EXTERN op_8e_USE32
	GLOBAL	op_8f_67
	EXTERN	op_98
	EXTERN	op_99
	EXTERN	op_9c
	EXTERN	op_9d
	EXTERN	op_9e
	EXTERN	op_9f
	EXTERN	op_a0_USE32
	GLOBAL 	op_a1_67
	EXTERN	op_a2_USE32
	GLOBAL	op_a3_67
	EXTERN	op_a4_movsb_USE32
	EXTERN	op_a5_movsw_USE32
	EXTERN	op_a6_cmpsb_USE32
	EXTERN	op_a7_cmpsw_USE32
	EXTERN	op_a8
	EXTERN	op_a9
	EXTERN	op_aa_stosb_USE32
	EXTERN	op_ab_stosw_USE32
	EXTERN	op_ac_lodsb_USE32
	EXTERN	op_ad_lodsw_USE32
	EXTERN	op_ae_scasb_USE32
	EXTERN	op_af_scasw_USE32
	EXTERN  op_b0
	EXTERN  op_b1
	EXTERN  op_b2
	EXTERN  op_b3
	EXTERN  op_b4
	EXTERN  op_b5
	EXTERN  op_b6
	EXTERN  op_b7
	EXTERN  op_b8
	EXTERN  op_b9
	EXTERN  op_ba
	EXTERN  op_bb
	EXTERN  op_bc
	EXTERN  op_bd
	EXTERN  op_be
	EXTERN  op_bf
	EXTERN  op_c0_USE32
	GLOBAL 	op_c1_67
	EXTERN  op_c2
	EXTERN  op_c3
	EXTERN  op_c6_USE32
	GLOBAL	op_c7_67
	EXTERN  op_ca
	EXTERN  op_cb
	EXTERN  op_cf
	EXTERN  op_d0_USE32
	EXTERN  op_d2_USE32
	EXTERN  op_d4
	EXTERN  op_d7_USE32
	IF USE_FPU_OPCODES = 1
	EXTERN  op_d8_USE32
	EXTERN  op_d9_USE32
	EXTERN  op_da_USE32
	EXTERN  op_db_USE32
	EXTERN  op_dc_USE32
	EXTERN  op_dd_USE32
	EXTERN  op_de_USE32
	EXTERN  op_df_USE32
	ENDIF
	EXTERN  op_e0_USE32
	EXTERN  op_e1_USE32
	EXTERN  op_e2_USE32
	EXTERN  op_e3_USE32
	EXTERN  op_e4_in_al_imm8
	EXTERN  op_e5_in_ax_imm8
	EXTERN  op_e6_out_imm8_al
	EXTERN  op_e7_out_imm8_ax
	EXTERN  op_ec_in_al_dx
	EXTERN  op_ed_in_ax_dx
	EXTERN  op_ee_out_dx_al
	EXTERN  op_ef_out_dx_ax
	EXTERN  op_f2_67
	EXTERN  op_f3_67
	EXTERN  op_f4
	EXTERN  op_f5
	EXTERN  op_f6_USE32
	GLOBAL 	op_f7_67
	EXTERN  op_f8
	EXTERN  op_f9
	EXTERN  op_fa_CLI
	EXTERN  op_fb_STI
	EXTERN  op_fc
	EXTERN  op_fd
	EXTERN  op_fe_USE32
	GLOBAL 	op_ff_67

; ------------------- 69 = IMUL r16,r/m16,imm16 ----------------------
op_69_67
	modrm_jump_32_tbl op_69_67_jump
	modrm_tbl_3_old imul_imm16
	DCD imul_ax_ax_imm16, imul_ax_cx_imm16, imul_ax_dx_imm16, imul_ax_bx_imm16, imul_ax_sp_imm16, imul_ax_bp_imm16, imul_ax_si_imm16, imul_ax_di_imm16
	DCD imul_cx_ax_imm16, imul_cx_cx_imm16, imul_cx_dx_imm16, imul_cx_bx_imm16, imul_cx_sp_imm16, imul_cx_bp_imm16, imul_cx_si_imm16, imul_cx_di_imm16
	DCD imul_dx_ax_imm16, imul_dx_cx_imm16, imul_dx_dx_imm16, imul_dx_bx_imm16, imul_dx_sp_imm16, imul_dx_bp_imm16, imul_dx_si_imm16, imul_dx_di_imm16
	DCD imul_bx_ax_imm16, imul_bx_cx_imm16, imul_bx_dx_imm16, imul_bx_bx_imm16, imul_bx_sp_imm16, imul_bx_bp_imm16, imul_bx_si_imm16, imul_bx_di_imm16
	DCD imul_sp_ax_imm16, imul_sp_cx_imm16, imul_sp_dx_imm16, imul_sp_bx_imm16, imul_sp_sp_imm16, imul_sp_bp_imm16, imul_sp_si_imm16, imul_sp_di_imm16
	DCD imul_bp_ax_imm16, imul_bp_cx_imm16, imul_bp_dx_imm16, imul_bp_bx_imm16, imul_bp_sp_imm16, imul_bp_bp_imm16, imul_bp_si_imm16, imul_bp_di_imm16
	DCD imul_si_ax_imm16, imul_si_cx_imm16, imul_si_dx_imm16, imul_si_bx_imm16, imul_si_sp_imm16, imul_si_bp_imm16, imul_si_si_imm16, imul_si_di_imm16
	DCD imul_di_ax_imm16, imul_di_cx_imm16, imul_di_dx_imm16, imul_di_bx_imm16, imul_di_sp_imm16, imul_di_bp_imm16, imul_di_si_imm16, imul_di_di_imm16

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old imul_imm16, imul_imm16_reg16_r0

	LTORG
	
	EXTERN	imul_ax_ax_imm16 
	EXTERN  imul_ax_cx_imm16 
	EXTERN  imul_ax_dx_imm16 
	EXTERN  imul_ax_bx_imm16 
	EXTERN  imul_ax_sp_imm16 
	EXTERN  imul_ax_bp_imm16 
	EXTERN  imul_ax_si_imm16 
	EXTERN  imul_ax_di_imm16
	EXTERN 	imul_cx_ax_imm16 
	EXTERN  imul_cx_cx_imm16 
	EXTERN  imul_cx_dx_imm16 
	EXTERN  imul_cx_bx_imm16 
	EXTERN  imul_cx_sp_imm16 
	EXTERN  imul_cx_bp_imm16 
	EXTERN  imul_cx_si_imm16 
	EXTERN  imul_cx_di_imm16
	EXTERN 	imul_dx_ax_imm16 
	EXTERN  imul_dx_cx_imm16 
	EXTERN  imul_dx_dx_imm16 
	EXTERN  imul_dx_bx_imm16 
	EXTERN  imul_dx_sp_imm16 
	EXTERN  imul_dx_bp_imm16 
	EXTERN  imul_dx_si_imm16 
	EXTERN  imul_dx_di_imm16
	EXTERN 	imul_bx_ax_imm16 
	EXTERN  imul_bx_cx_imm16 
	EXTERN  imul_bx_dx_imm16 
	EXTERN  imul_bx_bx_imm16 
	EXTERN  imul_bx_sp_imm16 
	EXTERN  imul_bx_bp_imm16 
	EXTERN  imul_bx_si_imm16 
	EXTERN  imul_bx_di_imm16
	EXTERN 	imul_sp_ax_imm16 
	EXTERN  imul_sp_cx_imm16 
	EXTERN  imul_sp_dx_imm16 
	EXTERN  imul_sp_bx_imm16 
	EXTERN  imul_sp_sp_imm16 
	EXTERN  imul_sp_bp_imm16 
	EXTERN  imul_sp_si_imm16 
	EXTERN  imul_sp_di_imm16
	EXTERN 	imul_bp_ax_imm16 
	EXTERN  imul_bp_cx_imm16 
	EXTERN  imul_bp_dx_imm16 
	EXTERN  imul_bp_bx_imm16 
	EXTERN  imul_bp_sp_imm16 
	EXTERN  imul_bp_bp_imm16 
	EXTERN  imul_bp_si_imm16 
	EXTERN  imul_bp_di_imm16
	EXTERN 	imul_si_ax_imm16 
	EXTERN  imul_si_cx_imm16 
	EXTERN  imul_si_dx_imm16 
	EXTERN  imul_si_bx_imm16 
	EXTERN  imul_si_sp_imm16 
	EXTERN  imul_si_bp_imm16 
	EXTERN  imul_si_si_imm16 
	EXTERN  imul_si_di_imm16
	EXTERN 	imul_di_ax_imm16 
	EXTERN  imul_di_cx_imm16 
	EXTERN  imul_di_dx_imm16 
	EXTERN  imul_di_bx_imm16 
	EXTERN  imul_di_sp_imm16 
	EXTERN  imul_di_bp_imm16 
	EXTERN  imul_di_si_imm16 
	EXTERN  imul_di_di_imm16

; ------------------- 6B = IMUL r16,r/m16,imm8 -----------------------
	GLOBAL	op_6b_67
op_6b_67
	modrm_jump_32_tbl op_6b_67_jump
	modrm_tbl_3_old imul_imm8
	DCD imul_ax_ax_imm8, imul_ax_cx_imm8, imul_ax_dx_imm8, imul_ax_bx_imm8, imul_ax_sp_imm8, imul_ax_bp_imm8, imul_ax_si_imm8, imul_ax_di_imm8
	DCD imul_cx_ax_imm8, imul_cx_cx_imm8, imul_cx_dx_imm8, imul_cx_bx_imm8, imul_cx_sp_imm8, imul_cx_bp_imm8, imul_cx_si_imm8, imul_cx_di_imm8
	DCD imul_dx_ax_imm8, imul_dx_cx_imm8, imul_dx_dx_imm8, imul_dx_bx_imm8, imul_dx_sp_imm8, imul_dx_bp_imm8, imul_dx_si_imm8, imul_dx_di_imm8
	DCD imul_bx_ax_imm8, imul_bx_cx_imm8, imul_bx_dx_imm8, imul_bx_bx_imm8, imul_bx_sp_imm8, imul_bx_bp_imm8, imul_bx_si_imm8, imul_bx_di_imm8
	DCD imul_sp_ax_imm8, imul_sp_cx_imm8, imul_sp_dx_imm8, imul_sp_bx_imm8, imul_sp_sp_imm8, imul_sp_bp_imm8, imul_sp_si_imm8, imul_sp_di_imm8
	DCD imul_bp_ax_imm8, imul_bp_cx_imm8, imul_bp_dx_imm8, imul_bp_bx_imm8, imul_bp_sp_imm8, imul_bp_bp_imm8, imul_bp_si_imm8, imul_bp_di_imm8
	DCD imul_si_ax_imm8, imul_si_cx_imm8, imul_si_dx_imm8, imul_si_bx_imm8, imul_si_sp_imm8, imul_si_bp_imm8, imul_si_si_imm8, imul_si_di_imm8
	DCD imul_di_ax_imm8, imul_di_cx_imm8, imul_di_dx_imm8, imul_di_bx_imm8, imul_di_sp_imm8, imul_di_bp_imm8, imul_di_si_imm8, imul_di_di_imm8

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old imul_imm8, imul_imm8_reg16_r0

	LTORG
	
	EXTERN	imul_ax_ax_imm8 
	EXTERN  imul_ax_cx_imm8 
	EXTERN  imul_ax_dx_imm8 
	EXTERN  imul_ax_bx_imm8 
	EXTERN  imul_ax_sp_imm8 
	EXTERN  imul_ax_bp_imm8 
	EXTERN  imul_ax_si_imm8 
	EXTERN  imul_ax_di_imm8
	EXTERN 	imul_cx_ax_imm8 
	EXTERN  imul_cx_cx_imm8 
	EXTERN  imul_cx_dx_imm8 
	EXTERN  imul_cx_bx_imm8 
	EXTERN  imul_cx_sp_imm8 
	EXTERN  imul_cx_bp_imm8 
	EXTERN  imul_cx_si_imm8 
	EXTERN  imul_cx_di_imm8
	EXTERN 	imul_dx_ax_imm8 
	EXTERN  imul_dx_cx_imm8 
	EXTERN  imul_dx_dx_imm8 
	EXTERN  imul_dx_bx_imm8 
	EXTERN  imul_dx_sp_imm8 
	EXTERN  imul_dx_bp_imm8 
	EXTERN  imul_dx_si_imm8 
	EXTERN  imul_dx_di_imm8
	EXTERN 	imul_bx_ax_imm8 
	EXTERN  imul_bx_cx_imm8 
	EXTERN  imul_bx_dx_imm8 
	EXTERN  imul_bx_bx_imm8 
	EXTERN  imul_bx_sp_imm8 
	EXTERN  imul_bx_bp_imm8 
	EXTERN  imul_bx_si_imm8 
	EXTERN  imul_bx_di_imm8
	EXTERN 	imul_sp_ax_imm8 
	EXTERN  imul_sp_cx_imm8 
	EXTERN  imul_sp_dx_imm8 
	EXTERN  imul_sp_bx_imm8 
	EXTERN  imul_sp_sp_imm8 
	EXTERN  imul_sp_bp_imm8 
	EXTERN  imul_sp_si_imm8 
	EXTERN  imul_sp_di_imm8
	EXTERN 	imul_bp_ax_imm8 
	EXTERN  imul_bp_cx_imm8 
	EXTERN  imul_bp_dx_imm8 
	EXTERN  imul_bp_bx_imm8 
	EXTERN  imul_bp_sp_imm8 
	EXTERN  imul_bp_bp_imm8 
	EXTERN  imul_bp_si_imm8 
	EXTERN  imul_bp_di_imm8
	EXTERN 	imul_si_ax_imm8 
	EXTERN  imul_si_cx_imm8 
	EXTERN  imul_si_dx_imm8 
	EXTERN  imul_si_bx_imm8 
	EXTERN  imul_si_sp_imm8 
	EXTERN  imul_si_bp_imm8 
	EXTERN  imul_si_si_imm8 
	EXTERN  imul_si_di_imm8
	EXTERN 	imul_di_ax_imm8 
	EXTERN  imul_di_cx_imm8 
	EXTERN  imul_di_dx_imm8 
	EXTERN  imul_di_bx_imm8 
	EXTERN  imul_di_sp_imm8 
	EXTERN  imul_di_bp_imm8 
	EXTERN  imul_di_si_imm8 
	EXTERN  imul_di_di_imm8
	
; ------------------- 81 = ??? r/m16,imm16 ----------------------------
op_81_67
	modrm_jump_32_tbl op_81_67_jump
	modrm_tbl_oper add, or, adc, sbb, and, sub, xor, cmp, imm16
	DCD add_ax_imm16, add_cx_imm16, add_dx_imm16, add_bx_imm16, add_sp_imm16, add_bp_imm16, add_si_imm16, add_di_imm16
	DCD or_ax_imm16, or_cx_imm16, or_dx_imm16, or_bx_imm16, or_sp_imm16, or_bp_imm16, or_si_imm16, or_di_imm16
	DCD adc_ax_imm16, adc_cx_imm16, adc_dx_imm16, adc_bx_imm16, adc_sp_imm16, adc_bp_imm16, adc_si_imm16, adc_di_imm16
	DCD sbb_ax_imm16, sbb_cx_imm16, sbb_dx_imm16, sbb_bx_imm16, sbb_sp_imm16, sbb_bp_imm16, sbb_si_imm16, sbb_di_imm16
	DCD and_ax_imm16, and_cx_imm16, and_dx_imm16, and_bx_imm16, and_sp_imm16, and_bp_imm16, and_si_imm16, and_di_imm16
	DCD sub_ax_imm16, sub_cx_imm16, sub_dx_imm16, sub_bx_imm16, sub_sp_imm16, sub_bp_imm16, sub_si_imm16, sub_di_imm16
	DCD xor_ax_imm16, xor_cx_imm16, xor_dx_imm16, xor_bx_imm16, xor_sp_imm16, xor_bp_imm16, xor_si_imm16, xor_di_imm16
	DCD cmp_ax_imm16, cmp_cx_imm16, cmp_dx_imm16, cmp_bx_imm16, cmp_sp_imm16, cmp_bp_imm16, cmp_si_imm16, cmp_di_imm16

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper add, or, adc, sbb, and, sub, xor, cmp, "r0", imm16

	LTORG
	
	EXTERN  add_ax_imm16
	EXTERN  add_cx_imm16
	EXTERN  add_dx_imm16
	EXTERN  add_bx_imm16
	EXTERN  add_sp_imm16
	EXTERN  add_bp_imm16
	EXTERN  add_si_imm16
	EXTERN  add_di_imm16
	EXTERN  or_ax_imm16
	EXTERN  or_cx_imm16
	EXTERN  or_dx_imm16
	EXTERN  or_bx_imm16
	EXTERN  or_sp_imm16
	EXTERN  or_bp_imm16
	EXTERN  or_si_imm16
	EXTERN  or_di_imm16
	EXTERN  adc_ax_imm16
	EXTERN  adc_cx_imm16
	EXTERN  adc_dx_imm16
	EXTERN  adc_bx_imm16
	EXTERN  adc_sp_imm16
	EXTERN  adc_bp_imm16
	EXTERN  adc_si_imm16
	EXTERN  adc_di_imm16
	EXTERN  sbb_ax_imm16
	EXTERN  sbb_cx_imm16
	EXTERN  sbb_dx_imm16
	EXTERN  sbb_bx_imm16
	EXTERN  sbb_sp_imm16
	EXTERN  sbb_bp_imm16
	EXTERN  sbb_si_imm16
	EXTERN  sbb_di_imm16
	EXTERN  and_ax_imm16
	EXTERN  and_cx_imm16
	EXTERN  and_dx_imm16
	EXTERN  and_bx_imm16
	EXTERN  and_sp_imm16
	EXTERN  and_bp_imm16
	EXTERN  and_si_imm16
	EXTERN  and_di_imm16
	EXTERN  sub_ax_imm16
	EXTERN  sub_cx_imm16
	EXTERN  sub_dx_imm16
	EXTERN  sub_bx_imm16
	EXTERN  sub_sp_imm16
	EXTERN  sub_bp_imm16
	EXTERN  sub_si_imm16
	EXTERN  sub_di_imm16
	EXTERN  xor_ax_imm16
	EXTERN  xor_cx_imm16
	EXTERN  xor_dx_imm16
	EXTERN  xor_bx_imm16
	EXTERN  xor_sp_imm16
	EXTERN  xor_bp_imm16
	EXTERN  xor_si_imm16
	EXTERN  xor_di_imm16
	EXTERN  cmp_ax_imm16
	EXTERN  cmp_cx_imm16
	EXTERN  cmp_dx_imm16
	EXTERN  cmp_bx_imm16
	EXTERN  cmp_sp_imm16
	EXTERN  cmp_bp_imm16
	EXTERN  cmp_si_imm16
	EXTERN  cmp_di_imm16

; ------------------- 83 = ??? r/m16,+imm8 ----------------------------
op_83_67
	modrm_jump_32_tbl op_83_67_jump
	modrm_tbl_oper add, or, adc, sbb, and, sub, xor, cmp, simm8
; 0xc0 = mod = 11b => two register operands
	DCD add_ax_simm8, add_cx_simm8, add_dx_simm8, add_bx_simm8, add_sp_simm8, add_bp_simm8, add_si_simm8, add_di_simm8
	DCD or_ax_simm8, or_cx_simm8, or_dx_simm8, or_bx_simm8, or_sp_simm8, or_bp_simm8, or_si_simm8, or_di_simm8
	DCD adc_ax_simm8, adc_cx_simm8, adc_dx_simm8, adc_bx_simm8, adc_sp_simm8, adc_bp_simm8, adc_si_simm8, adc_di_simm8
	DCD sbb_ax_simm8, sbb_cx_simm8, sbb_dx_simm8, sbb_bx_simm8, sbb_sp_simm8, sbb_bp_simm8, sbb_si_simm8, sbb_di_simm8
	DCD and_ax_simm8, and_cx_simm8, and_dx_simm8, and_bx_simm8, and_sp_simm8, and_bp_simm8, and_si_simm8, and_di_simm8
	DCD sub_ax_simm8, sub_cx_simm8, sub_dx_simm8, sub_bx_simm8, sub_sp_simm8, sub_bp_simm8, sub_si_simm8, sub_di_simm8
	DCD xor_ax_simm8, xor_cx_simm8, xor_dx_simm8, xor_bx_simm8, xor_sp_simm8, xor_bp_simm8, xor_si_simm8, xor_di_simm8
	DCD cmp_ax_simm8, cmp_cx_simm8, cmp_dx_simm8, cmp_bx_simm8, cmp_sp_simm8, cmp_bp_simm8, cmp_si_simm8, cmp_di_simm8

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper add, or, adc, sbb, and, sub, xor, cmp, "r0", simm8

	LTORG
	
	EXTERN  add_ax_simm8
	EXTERN  add_cx_simm8
	EXTERN  add_dx_simm8
	EXTERN  add_bx_simm8
	EXTERN  add_sp_simm8
	EXTERN  add_bp_simm8
	EXTERN  add_si_simm8
	EXTERN  add_di_simm8
	EXTERN  or_ax_simm8
	EXTERN  or_cx_simm8
	EXTERN  or_dx_simm8
	EXTERN  or_bx_simm8
	EXTERN  or_sp_simm8
	EXTERN  or_bp_simm8
	EXTERN  or_si_simm8
	EXTERN  or_di_simm8
	EXTERN  adc_ax_simm8
	EXTERN  adc_cx_simm8
	EXTERN  adc_dx_simm8
	EXTERN  adc_bx_simm8
	EXTERN  adc_sp_simm8
	EXTERN  adc_bp_simm8
	EXTERN  adc_si_simm8
	EXTERN  adc_di_simm8
	EXTERN  sbb_ax_simm8
	EXTERN  sbb_cx_simm8
	EXTERN  sbb_dx_simm8
	EXTERN  sbb_bx_simm8
	EXTERN  sbb_sp_simm8
	EXTERN  sbb_bp_simm8
	EXTERN  sbb_si_simm8
	EXTERN  sbb_di_simm8
	EXTERN  and_ax_simm8
	EXTERN  and_cx_simm8
	EXTERN  and_dx_simm8
	EXTERN  and_bx_simm8
	EXTERN  and_sp_simm8
	EXTERN  and_bp_simm8
	EXTERN  and_si_simm8
	EXTERN  and_di_simm8
	EXTERN  sub_ax_simm8
	EXTERN  sub_cx_simm8
	EXTERN  sub_dx_simm8
	EXTERN  sub_bx_simm8
	EXTERN  sub_sp_simm8
	EXTERN  sub_bp_simm8
	EXTERN  sub_si_simm8
	EXTERN  sub_di_simm8
	EXTERN  xor_ax_simm8
	EXTERN  xor_cx_simm8
	EXTERN  xor_dx_simm8
	EXTERN  xor_bx_simm8
	EXTERN  xor_sp_simm8
	EXTERN  xor_bp_simm8
	EXTERN  xor_si_simm8
	EXTERN  xor_di_simm8
	EXTERN  cmp_ax_simm8
	EXTERN  cmp_cx_simm8
	EXTERN  cmp_dx_simm8
	EXTERN  cmp_bx_simm8
	EXTERN  cmp_sp_simm8
	EXTERN  cmp_bp_simm8
	EXTERN  cmp_si_simm8
	EXTERN  cmp_di_simm8

; ------------------- 85 = TEST r16,r/m16 -----------------------------
op_85_67
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_32_tbl op_85_67_jump
	modrm_tbl_1_old test
; 0xC0 = two register operands
	DCD test_ax_ax, test_cx_ax, test_dx_ax, test_bx_ax, test_sp_ax, test_bp_ax, test_si_ax, test_di_ax
	DCD test_ax_cx, test_cx_cx, test_dx_cx, test_bx_cx, test_sp_cx, test_bp_cx, test_si_cx, test_di_cx
	DCD test_ax_dx, test_cx_dx, test_dx_dx, test_bx_dx, test_sp_dx, test_bp_dx, test_si_dx, test_di_dx
	DCD test_ax_bx, test_cx_bx, test_dx_bx, test_bx_bx, test_sp_bx, test_bp_bx, test_si_bx, test_di_bx
	DCD test_ax_sp, test_cx_sp, test_dx_sp, test_bx_sp, test_sp_sp, test_bp_sp, test_si_sp, test_di_sp
	DCD test_ax_bp, test_cx_bp, test_dx_bp, test_bx_bp, test_sp_bp, test_bp_bp, test_si_bp, test_di_bp
	DCD test_ax_si, test_cx_si, test_dx_si, test_bx_si, test_sp_si, test_bp_si, test_si_si, test_di_si
	DCD test_ax_di, test_cx_di, test_dx_di, test_bx_di, test_sp_di, test_bp_di, test_si_di, test_di_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old test, test_r0_r16

	LTORG
	
	EXTERN  test_ax_ax
	EXTERN  test_cx_ax
	EXTERN  test_dx_ax
	EXTERN  test_bx_ax
	EXTERN  test_sp_ax
	EXTERN  test_bp_ax
	EXTERN  test_si_ax
	EXTERN  test_di_ax
	EXTERN  test_ax_cx
	EXTERN  test_cx_cx 
	EXTERN  test_dx_cx 
	EXTERN  test_bx_cx 
	EXTERN  test_sp_cx 
	EXTERN  test_bp_cx 
	EXTERN  test_si_cx 
	EXTERN  test_di_cx
	EXTERN  test_ax_dx 
	EXTERN  test_cx_dx 
	EXTERN  test_dx_dx 
	EXTERN  test_bx_dx 
	EXTERN  test_sp_dx 
	EXTERN  test_bp_dx 
	EXTERN  test_si_dx 
	EXTERN  test_di_dx
	EXTERN  test_ax_bx 
	EXTERN  test_cx_bx 
	EXTERN  test_dx_bx 
	EXTERN  test_bx_bx 
	EXTERN  test_sp_bx 
	EXTERN  test_bp_bx 
	EXTERN  test_si_bx 
	EXTERN  test_di_bx
	EXTERN  test_ax_sp 
	EXTERN  test_cx_sp 
	EXTERN  test_dx_sp 
	EXTERN  test_bx_sp 
	EXTERN  test_sp_sp 
	EXTERN  test_bp_sp 
	EXTERN  test_si_sp 
	EXTERN  test_di_sp
	EXTERN  test_ax_bp 
	EXTERN  test_cx_bp 
	EXTERN  test_dx_bp 
	EXTERN  test_bx_bp 
	EXTERN  test_sp_bp 
	EXTERN  test_bp_bp 
	EXTERN  test_si_bp 
	EXTERN  test_di_bp
	EXTERN  test_ax_si 
	EXTERN  test_cx_si 
	EXTERN  test_dx_si 
	EXTERN  test_bx_si 
	EXTERN  test_sp_si 
	EXTERN  test_bp_si 
	EXTERN  test_si_si 
	EXTERN  test_di_si
	EXTERN  test_ax_di 
	EXTERN  test_cx_di 
	EXTERN  test_dx_di 
	EXTERN  test_bx_di 
	EXTERN  test_sp_di 
	EXTERN  test_bp_di 
	EXTERN  test_si_di 
	EXTERN  test_di_di

; ------------------- 87 = XCHG r/m16,r16 ----------------------------
op_87_67
	modrm_jump_32_tbl op_87_67_jump
	modrm_tbl_1_old xchg
; 0xC0
	DCD loop, xchg_cx_ax, xchg_dx_ax, xchg_bx_ax, xchg_sp_ax, xchg_bp_ax, xchg_si_ax, xchg_di_ax
	DCD xchg_ax_cx, loop, xchg_dx_cx, xchg_bx_cx, xchg_sp_cx, xchg_bp_cx, xchg_si_cx, xchg_di_cx
	DCD xchg_ax_dx, xchg_cx_dx, loop, xchg_bx_dx, xchg_sp_dx, xchg_bp_dx, xchg_si_dx, xchg_di_dx
	DCD xchg_ax_bx, xchg_cx_bx, xchg_dx_bx, loop, xchg_sp_bx, xchg_bp_bx, xchg_si_bx, xchg_di_bx
	DCD xchg_ax_sp, xchg_cx_sp, xchg_dx_sp, xchg_bx_sp, loop, xchg_bp_sp, xchg_si_sp, xchg_di_sp
	DCD xchg_ax_bp, xchg_cx_bp, xchg_dx_bp, xchg_bx_bp, xchg_sp_bp, loop, xchg_si_bp, xchg_di_bp
	DCD xchg_ax_si, xchg_cx_si, xchg_dx_si, xchg_bx_si, xchg_sp_si, xchg_bp_si, loop, xchg_di_si
	DCD xchg_ax_di, xchg_cx_di, xchg_dx_di, xchg_bx_di, xchg_sp_di, xchg_bp_di, xchg_si_di, loop

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old xchg, xchg_r0_r16

	LTORG
	
	EXTERN  xchg_ax_ax
	EXTERN  xchg_cx_ax
	EXTERN  xchg_dx_ax
	EXTERN  xchg_bx_ax
	EXTERN  xchg_sp_ax
	EXTERN  xchg_bp_ax
	EXTERN  xchg_si_ax
	EXTERN  xchg_di_ax
	EXTERN  xchg_ax_cx
	EXTERN  xchg_cx_cx 
	EXTERN  xchg_dx_cx 
	EXTERN  xchg_bx_cx 
	EXTERN  xchg_sp_cx 
	EXTERN  xchg_bp_cx 
	EXTERN  xchg_si_cx 
	EXTERN  xchg_di_cx
	EXTERN  xchg_ax_dx 
	EXTERN  xchg_cx_dx 
	EXTERN  xchg_dx_dx 
	EXTERN  xchg_bx_dx 
	EXTERN  xchg_sp_dx 
	EXTERN  xchg_bp_dx 
	EXTERN  xchg_si_dx 
	EXTERN  xchg_di_dx
	EXTERN  xchg_ax_bx 
	EXTERN  xchg_cx_bx 
	EXTERN  xchg_dx_bx 
	EXTERN  xchg_bx_bx 
	EXTERN  xchg_sp_bx 
	EXTERN  xchg_bp_bx 
	EXTERN  xchg_si_bx 
	EXTERN  xchg_di_bx
	EXTERN  xchg_ax_sp 
	EXTERN  xchg_cx_sp 
	EXTERN  xchg_dx_sp 
	EXTERN  xchg_bx_sp 
	EXTERN  xchg_sp_sp 
	EXTERN  xchg_bp_sp 
	EXTERN  xchg_si_sp 
	EXTERN  xchg_di_sp
	EXTERN  xchg_ax_bp 
	EXTERN  xchg_cx_bp 
	EXTERN  xchg_dx_bp 
	EXTERN  xchg_bx_bp 
	EXTERN  xchg_sp_bp 
	EXTERN  xchg_bp_bp 
	EXTERN  xchg_si_bp 
	EXTERN  xchg_di_bp
	EXTERN  xchg_ax_si 
	EXTERN  xchg_cx_si 
	EXTERN  xchg_dx_si 
	EXTERN  xchg_bx_si 
	EXTERN  xchg_sp_si 
	EXTERN  xchg_bp_si 
	EXTERN  xchg_si_si 
	EXTERN  xchg_di_si
	EXTERN  xchg_ax_di 
	EXTERN  xchg_cx_di 
	EXTERN  xchg_dx_di 
	EXTERN  xchg_bx_di 
	EXTERN  xchg_sp_di 
	EXTERN  xchg_bp_di 
	EXTERN  xchg_si_di 
	EXTERN  xchg_di_di

; ------------------- 88 = MOV r/m8,r8 -------------------------------
; 6788254C060000 = mov [0000064C],ah (Zone66)
; See op_88_USE32 at "cpu_386.S"

; ------------------- 89 = MOV r/m16,r16 ------------------------------
; 2667897B66 = mov es:[ebx+66],di
;
	GLOBAL	op_89_67
op_89_67
	modrm_jump_32_tbl op_89_67_jump
	modrm_tbl_1_old mov	
; 0xC0
	DCD loop, mov_cx_ax, mov_dx_ax, mov_bx_ax, mov_sp_ax, mov_bp_ax, mov_si_ax, mov_di_ax
	DCD mov_ax_cx, loop, mov_dx_cx, mov_bx_cx, mov_sp_cx, mov_bp_cx, mov_si_cx, mov_di_cx
	DCD mov_ax_dx, mov_cx_dx, loop, mov_bx_dx, mov_sp_dx, mov_bp_dx, mov_si_dx, mov_di_dx
	DCD mov_ax_bx, mov_cx_bx, mov_dx_bx, loop, mov_sp_bx, mov_bp_bx, mov_si_bx, mov_di_bx
	DCD mov_ax_sp, mov_cx_sp, mov_dx_sp, mov_bx_sp, loop, mov_bp_sp, mov_si_sp, mov_di_sp
	DCD mov_ax_bp, mov_cx_bp, mov_dx_bp, mov_bx_bp, mov_sp_bp, loop, mov_si_bp, mov_di_bp
	DCD mov_ax_si, mov_cx_si, mov_dx_si, mov_bx_si, mov_sp_si, mov_bp_si, loop, mov_di_si
	DCD mov_ax_di, mov_cx_di, mov_dx_di, mov_bx_di, mov_sp_di, mov_bp_di, mov_si_di, loop

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_genall_old mov, mov_r0_r16

	LTORG
	
	GLOBAL	op_a3_67
op_a3_67
	mvn		r3, #0				; Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_r0_r16_eax

	EXTERN	mov_r0_r16_eax
	
	EXTERN  mov_ax_ax
	EXTERN  mov_cx_ax
	EXTERN  mov_dx_ax
	EXTERN  mov_bx_ax
	EXTERN  mov_sp_ax
	EXTERN  mov_bp_ax
	EXTERN  mov_si_ax
	EXTERN  mov_di_ax
	EXTERN  mov_ax_cx
	EXTERN  mov_cx_cx 
	EXTERN  mov_dx_cx 
	EXTERN  mov_bx_cx 
	EXTERN  mov_sp_cx 
	EXTERN  mov_bp_cx 
	EXTERN  mov_si_cx 
	EXTERN  mov_di_cx
	EXTERN  mov_ax_dx 
	EXTERN  mov_cx_dx 
	EXTERN  mov_dx_dx 
	EXTERN  mov_bx_dx 
	EXTERN  mov_sp_dx 
	EXTERN  mov_bp_dx 
	EXTERN  mov_si_dx 
	EXTERN  mov_di_dx
	EXTERN  mov_ax_bx 
	EXTERN  mov_cx_bx 
	EXTERN  mov_dx_bx 
	EXTERN  mov_bx_bx 
	EXTERN  mov_sp_bx 
	EXTERN  mov_bp_bx 
	EXTERN  mov_si_bx 
	EXTERN  mov_di_bx
	EXTERN  mov_ax_sp 
	EXTERN  mov_cx_sp 
	EXTERN  mov_dx_sp 
	EXTERN  mov_bx_sp 
	EXTERN  mov_sp_sp 
	EXTERN  mov_bp_sp 
	EXTERN  mov_si_sp 
	EXTERN  mov_di_sp
	EXTERN  mov_ax_bp 
	EXTERN  mov_cx_bp 
	EXTERN  mov_dx_bp 
	EXTERN  mov_bx_bp 
	EXTERN  mov_sp_bp 
	EXTERN  mov_bp_bp 
	EXTERN  mov_si_bp 
	EXTERN  mov_di_bp
	EXTERN  mov_ax_si 
	EXTERN  mov_cx_si 
	EXTERN  mov_dx_si 
	EXTERN  mov_bx_si 
	EXTERN  mov_sp_si 
	EXTERN  mov_bp_si 
	EXTERN  mov_si_si 
	EXTERN  mov_di_si
	EXTERN  mov_ax_di 
	EXTERN  mov_cx_di 
	EXTERN  mov_dx_di 
	EXTERN  mov_bx_di 
	EXTERN  mov_sp_di 
	EXTERN  mov_bp_di 
	EXTERN  mov_si_di 
	EXTERN  mov_di_di

; ------------------- 8A = MOV r8, r/m8 ------------------------------
; 678A4514 = mov al,[ebp+0014] (DOS4GW)
; See op_8a_USE32 at "cpu_386.S"

; ------------------- 8B = MOV r16,r/m16 -------------------------------
op_8b_67
	modrm_jump_32_tbl op_8b_67_jump
	modrm_tbl_3_old mov	
; 0xC0 = two register operands
	DCD loop, mov_ax_cx, mov_ax_dx, mov_ax_bx, mov_ax_sp, mov_ax_bp, mov_ax_si, mov_ax_di
	DCD mov_cx_ax, loop, mov_cx_dx, mov_cx_bx, mov_cx_sp, mov_cx_bp, mov_cx_si, mov_cx_di
	DCD mov_dx_ax, mov_dx_cx, loop, mov_dx_bx, mov_dx_sp, mov_dx_bp, mov_dx_si, mov_dx_di
	DCD mov_bx_ax, mov_bx_cx, mov_bx_dx, loop, mov_bx_sp, mov_bx_bp, mov_bx_si, mov_bx_di
	DCD mov_sp_ax, mov_sp_cx, mov_sp_dx, mov_sp_bx, loop, mov_sp_bp, mov_sp_si, mov_sp_di
	DCD mov_bp_ax, mov_bp_cx, mov_bp_dx, mov_bp_bx, mov_bp_sp, loop, mov_bp_si, mov_bp_di
	DCD mov_si_ax, mov_si_cx, mov_si_dx, mov_si_bx, mov_si_sp, mov_si_bp, loop, mov_si_di
	DCD mov_di_ax, mov_di_cx, mov_di_dx, mov_di_bx, mov_di_sp, mov_di_bp, mov_di_si, loop

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old mov, mov_r16_r0

	LTORG
	
	GLOBAL	op_a1_67
op_a1_67
	mvn		r3, #0				; Use 32-bit memory address masking
	get_cseip_dword r0
	b		mov_r16_r0_eax
	
	EXTERN	mov_r16_r0_eax
	
; ------------------- 8C = MOV r/m16,Sreg -----------------------------
op_8c_67
	modrm_jump_32_tbl op_8c_67_jump
; 0x0
	modrm_help_1_0 mov, es, es
	modrm_help_1_0 mov, cs, cs
	modrm_help_1_0 mov, ss, ss
	modrm_help_1_0 mov, ds, ds
	modrm_help_1_0 mov, fs, fs
	modrm_help_1_0 mov, gs, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 mov, es, es
	modrm_help_1_40 mov, cs, cs
	modrm_help_1_40 mov, ss, ss
	modrm_help_1_40 mov, ds, ds
	modrm_help_1_40 mov, fs, fs
	modrm_help_1_40 mov, gs, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	modrm_help_1_80 mov, es
	modrm_help_1_80 mov, cs
	modrm_help_1_80 mov, ss
	modrm_help_1_80 mov, ds
	modrm_help_1_80 mov, fs
	modrm_help_1_80 mov, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0
	DCD mov_ax_es, mov_cx_es, mov_dx_es, mov_bx_es, mov_sp_es, mov_bp_es, mov_si_es, mov_di_es
	DCD mov_ax_cs, mov_cx_cs, mov_dx_cs, mov_bx_cs, mov_sp_cs, mov_bp_cs, mov_si_cs, mov_di_cs
	DCD mov_ax_ss, mov_cx_ss, mov_dx_ss, mov_bx_ss, mov_sp_ss, mov_bp_ss, mov_si_ss, mov_di_ss
	DCD mov_ax_ds, mov_cx_ds, mov_dx_ds, mov_bx_ds, mov_sp_ds, mov_bp_ds, mov_si_ds, mov_di_ds
	DCD mov_ax_fs, mov_cx_fs, mov_dx_fs, mov_bx_fs, mov_sp_fs, mov_bp_fs, mov_si_fs, mov_di_fs
	DCD mov_ax_gs, mov_cx_gs, mov_dx_gs, mov_bx_gs, mov_sp_gs, mov_bp_gs, mov_si_gs, mov_di_gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	; The memory modrms use handlers in "cpu_386.s", the register modrms use handlers in "cpu.s"

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_externs mov, "", es
	modrm_1_externs mov, "", cs
	modrm_1_externs mov, "", ss
	modrm_1_externs mov, "", ds
	modrm_1_externs mov, "", fs
	modrm_1_externs mov, "", gs

	LTORG
	
	EXTERN  mov_ax_es 
	EXTERN  mov_cx_es 
	EXTERN  mov_dx_es 
	EXTERN  mov_bx_es 
	EXTERN  mov_sp_es 
	EXTERN  mov_bp_es 
	EXTERN  mov_si_es 
	EXTERN  mov_di_es
	EXTERN  mov_ax_cs 
	EXTERN  mov_cx_cs 
	EXTERN  mov_dx_cs 
	EXTERN  mov_bx_cs 
	EXTERN  mov_sp_cs 
	EXTERN  mov_bp_cs 
	EXTERN  mov_si_cs 
	EXTERN  mov_di_cs
	EXTERN  mov_ax_ss 
	EXTERN  mov_cx_ss 
	EXTERN  mov_dx_ss 
	EXTERN  mov_bx_ss 
	EXTERN  mov_sp_ss 
	EXTERN  mov_bp_ss 
	EXTERN  mov_si_ss 
	EXTERN  mov_di_ss
	EXTERN  mov_ax_ds 
	EXTERN  mov_cx_ds 
	EXTERN  mov_dx_ds 
	EXTERN  mov_bx_ds 
	EXTERN  mov_sp_ds 
	EXTERN  mov_bp_ds 
	EXTERN  mov_si_ds 
	EXTERN  mov_di_ds
	EXTERN  mov_ax_fs 
	EXTERN  mov_cx_fs 
	EXTERN  mov_dx_fs 
	EXTERN  mov_bx_fs 
	EXTERN  mov_sp_fs 
	EXTERN  mov_bp_fs 
	EXTERN  mov_si_fs 
	EXTERN  mov_di_fs
	EXTERN  mov_ax_gs 
	EXTERN  mov_cx_gs 
	EXTERN  mov_dx_gs 
	EXTERN  mov_bx_gs 
	EXTERN  mov_sp_gs 
	EXTERN  mov_bp_gs 
	EXTERN  mov_si_gs 
	EXTERN  mov_di_gs
	
; ------------------- 8D = LEA r16,m ----------------------------------
; 678D1500000000 = lea  dx,[00000000] (RACE, REAL, USE16)
;
	GLOBAL	op_8d_67
op_8d_67
	modrm_jump_32_tbl op_8d_67_jump
; 0
	modrm_help_3_0 lea, eax, eax
	modrm_help_3_0 lea, ecx, ecx
	modrm_help_3_0 lea, edx, edx
	modrm_help_3_0 lea, ebx, ebx
	modrm_help_3_0 lea, esp, esp
	modrm_help_3_0 lea, ebp, ebp
	modrm_help_3_0 lea, esi, esi
	modrm_help_3_0 lea, edi, edi
; 0x40
	modrm_help_3_40 lea, eax, eax
	modrm_help_3_40 lea, ecx, ecx
	modrm_help_3_40 lea, edx, edx
	modrm_help_3_40 lea, ebx, ebx
	modrm_help_3_40 lea, esp, esp
	modrm_help_3_40 lea, ebp, ebp
	modrm_help_3_40 lea, esi, esi
	modrm_help_3_40 lea, edi, edi
; 0x80
	modrm_help_3_80 lea, eax
	modrm_help_3_80 lea, ecx
	modrm_help_3_80 lea, edx
	modrm_help_3_80 lea, ebx
	modrm_help_3_80 lea, esp
	modrm_help_3_80 lea, ebp
	modrm_help_3_80 lea, esi
	modrm_help_3_80 lea, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	MACRO
	lea_help $reg
	;------
	; Need to have the same code 4 bytes before the actual handler,
	; as the SIB code returns to 4 bytes before the handler when using
	; ESP or EBP as the base register!
	;------ 
	nop
lea_SIB_return_$reg
	bfi		$reg, r0, #0, #16
	b		loop
; ----- 0x00 -----
lea_$reg_eaxidx
	bfi		$reg, eax, #0, #16
	b		loop
lea_$reg_ecxidx
	bfi		$reg, ecx, #0, #16
	b		loop
lea_$reg_edxidx
	bfi		$reg, edx, #0, #16
	b		loop
lea_$reg_bxidx
	bfi		$reg, ebx, #0, #16
	b		loop
lea_$reg_SIB
	ldrb	r0,[r12],#1							; Get the SIB byte into r0 register
	ldr		r1,=sib_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_$reg			; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
lea_$reg_disp32
	get_cseip_dword r0
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_siidx
	bfi		$reg, esi, #0, #16
	b		loop
lea_$reg_diidx
	bfi		$reg, edi, #0, #16
	b		loop
; ----- 0x40 = disp8 -----
lea_$reg_eaxd8
	ldrsb	r0, [r12], #1
	add		r0, eax
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_ecxd8
	ldrsb	r0, [r12], #1
	add		r0, ecx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_edxd8
	ldrsb	r0, [r12], #1
	add		r0, edx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_bxdisp8
	ldrsb	r0, [r12], #1
	add		r0, ebx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_SIBd8
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp8_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_$reg			; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
lea_$reg_bpdisp8
	ldrsb	r0, [r12], #1
	add		r0, ebp
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_sidisp8
	ldrsb	r0, [r12], #1
	add		r0, esi
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_didisp8
	ldrsb	r0, [r12], #1
	add		r0, edi
	bfi		$reg, r0, #0, #16
	b		loop
; ----- 0x80 = disp32 -----
lea_$reg_eaxd32
	get_cseip_dword r0
	add		r0, eax
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_ecxd32
	get_cseip_dword r0
	add		r0, ecx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_edxd32
	get_cseip_dword r0
	add		r0, edx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_ebxd32
	get_cseip_dword r0
	add		r0, ebx
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_SIBd32
	ldrb	r0,[r12],#1
	ldr		r1,=sib_disp32_table
	add		r0, r1, r0, lsl #2
	ldr		r1, =lea_SIB_return_$reg			; r1 = return address
	ldr		pc, [r0]							; Call the SIB calculation routine, return to r1
lea_$reg_ebpd32
	get_cseip_dword r0
	add		r0, ebp
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_esid32
	get_cseip_dword r0
	add		r0, esi
	bfi		$reg, r0, #0, #16
	b		loop
lea_$reg_edid32
	get_cseip_dword r0
	add		r0, edi
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

	lea_help eax
	lea_help ecx
	lea_help edx
	lea_help ebx

	LTORG
	
	lea_help esp
	lea_help ebp
	lea_help esi
	lea_help edi

	LTORG
	

; ------------------- 8F = POP m16 ------------------------------------
op_8f_67
	modrm_jump_32_tbl op_8f_67_jump
; 0
	modrm_help_1_0 pop
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 pop
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	modrm_help_1_80 pop
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0
	DCD op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_help pop, pop_r0

	LTORG
	
	EXTERN	pop_r0
	EXTERN	pop_r0_bp
	
	LTORG
	
; ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,imm8 ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags
op_c1_67
	modrm_jump_32_tbl op_c1_67_jump
	modrm_tbl_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, shl_16, sar_16, imm8
	DCD rol_ax_imm8, rol_cx_imm8, rol_dx_imm8, rol_bx_imm8, rol_sp_imm8, rol_bp_imm8, rol_si_imm8, rol_di_imm8
	DCD ror_ax_imm8, ror_cx_imm8, ror_dx_imm8, ror_bx_imm8, ror_sp_imm8, ror_bp_imm8, ror_si_imm8, ror_di_imm8
	DCD rcl_ax_imm8, rcl_cx_imm8, rcl_dx_imm8, rcl_bx_imm8, rcl_sp_imm8, rcl_bp_imm8, rcl_si_imm8, rcl_di_imm8
	DCD rcr_ax_imm8, rcr_cx_imm8, rcr_dx_imm8, rcr_bx_imm8, rcr_sp_imm8, rcr_bp_imm8, rcr_si_imm8, rcr_di_imm8
	DCD shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	DCD shr_ax_imm8, shr_cx_imm8, shr_dx_imm8, shr_bx_imm8, shr_sp_imm8, shr_bp_imm8, shr_si_imm8, shr_di_imm8
	DCD shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	DCD sar_ax_imm8, sar_cx_imm8, sar_dx_imm8, sar_bx_imm8, sar_sp_imm8, sar_bp_imm8, sar_si_imm8, sar_di_imm8

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, "skip", sar_16, "r0", imm8

	LTORG
	
	EXTERN  rol_ax_imm8
	EXTERN  rol_cx_imm8
	EXTERN  rol_dx_imm8
	EXTERN  rol_bx_imm8
	EXTERN  rol_sp_imm8
	EXTERN  rol_bp_imm8
	EXTERN  rol_si_imm8
	EXTERN  rol_di_imm8
	EXTERN  ror_ax_imm8
	EXTERN  ror_cx_imm8
	EXTERN  ror_dx_imm8
	EXTERN  ror_bx_imm8
	EXTERN  ror_sp_imm8
	EXTERN  ror_bp_imm8
	EXTERN  ror_si_imm8
	EXTERN  ror_di_imm8
	EXTERN  rcl_ax_imm8
	EXTERN  rcl_cx_imm8
	EXTERN  rcl_dx_imm8
	EXTERN  rcl_bx_imm8
	EXTERN  rcl_sp_imm8
	EXTERN  rcl_bp_imm8
	EXTERN  rcl_si_imm8
	EXTERN  rcl_di_imm8
	EXTERN  rcr_ax_imm8
	EXTERN  rcr_cx_imm8
	EXTERN  rcr_dx_imm8
	EXTERN  rcr_bx_imm8
	EXTERN  rcr_sp_imm8
	EXTERN  rcr_bp_imm8
	EXTERN  rcr_si_imm8
	EXTERN  rcr_di_imm8
	EXTERN  shl_ax_imm8
	EXTERN  shl_cx_imm8
	EXTERN  shl_dx_imm8
	EXTERN  shl_bx_imm8
	EXTERN  shl_sp_imm8
	EXTERN  shl_bp_imm8
	EXTERN  shl_si_imm8
	EXTERN  shl_di_imm8
	EXTERN  shr_ax_imm8
	EXTERN  shr_cx_imm8
	EXTERN  shr_dx_imm8
	EXTERN  shr_bx_imm8
	EXTERN  shr_sp_imm8
	EXTERN  shr_bp_imm8
	EXTERN  shr_si_imm8
	EXTERN  shr_di_imm8
	EXTERN  sar_ax_imm8
	EXTERN  sar_cx_imm8
	EXTERN  sar_dx_imm8
	EXTERN  sar_bx_imm8
	EXTERN  sar_sp_imm8
	EXTERN  sar_bp_imm8
	EXTERN  sar_si_imm8
	EXTERN  sar_di_imm8

; ------------------- C4 = LES r16,m16:16 -----------------------------
;
	GLOBAL	op_c4_67
op_c4_67
	modrm_jump_32_tbl op_c4_67_jump
	modrm_tbl_3_old les
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old les, les_r16_r0					; Call handlers in cpu.S

	LTORG
	
; ------------------- C5 = LDS r16,m16:16 -----------------------------
;
	GLOBAL	op_c5_67
op_c5_67
	modrm_jump_32_tbl op_c5_67_jump
	modrm_tbl_3_old lds
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_3_genall_old lds, lds_r16_r0					; Call handlers in cpu.S

	LTORG
	
; ------------------- C7 = MOV r/m16, imm16 -----------------------------
; 2667C743640000 = mov word es:[ebx+64],0000 
;
op_c7_67
	modrm_jump_32_tbl op_c7_67_jump
; 0
	modrm_help_1_0 mov, imm16, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 mov, imm16, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	modrm_help_1_80 mov, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0
	DCD op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_help mov, mov_r0, imm16, imm16

	EXTERN	mov_r0_imm16
	EXTERN	mov_r0_bp_imm16

	LTORG
	
; ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,1 ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
	GLOBAL	op_d1_67
op_d1_67
	modrm_jump_32_tbl op_d1_67_jump
	modrm_tbl_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, shl_16, sar_16, 1
	DCD rol_reg16_1_eax, rol_reg16_1_ecx, rol_reg16_1_edx, rol_reg16_1_ebx, rol_reg16_1_esp, rol_reg16_1_ebp, rol_reg16_1_esi, rol_reg16_1_edi
	DCD ror_reg16_1_eax, ror_reg16_1_ecx, ror_reg16_1_edx, ror_reg16_1_ebx, ror_reg16_1_esp, ror_reg16_1_ebp, ror_reg16_1_esi, ror_reg16_1_edi
	DCD rcl_reg16_1_eax, rcl_reg16_1_ecx, rcl_reg16_1_edx, rcl_reg16_1_ebx, rcl_reg16_1_esp, rcl_reg16_1_ebp, rcl_reg16_1_esi, rcl_reg16_1_edi
	DCD rcr_reg16_1_eax, rcr_reg16_1_ecx, rcr_reg16_1_edx, rcr_reg16_1_ebx, rcr_reg16_1_esp, rcr_reg16_1_ebp, rcr_reg16_1_esi, rcr_reg16_1_edi
	DCD shl_reg16_1_eax, shl_reg16_1_ecx, shl_reg16_1_edx, shl_reg16_1_ebx, shl_reg16_1_esp, shl_reg16_1_ebp, shl_reg16_1_esi, shl_reg16_1_edi
	DCD shr_reg16_1_eax, shr_reg16_1_ecx, shr_reg16_1_edx, shr_reg16_1_ebx, shr_reg16_1_esp, shr_reg16_1_ebp, shr_reg16_1_esi, shr_reg16_1_edi
	DCD shl_reg16_1_eax, shl_reg16_1_ecx, shl_reg16_1_edx, shl_reg16_1_ebx, shl_reg16_1_esp, shl_reg16_1_ebp, shl_reg16_1_esi, shl_reg16_1_edi
	DCD sar_reg16_1_eax, sar_reg16_1_ecx, sar_reg16_1_edx, sar_reg16_1_ebx, sar_reg16_1_esp, sar_reg16_1_ebp, sar_reg16_1_esi, sar_reg16_1_edi

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, "skip", sar_16, "r0", 1

	LTORG
	
	EXTERN  rol_reg16_1_eax
	EXTERN  rol_reg16_1_ecx
	EXTERN  rol_reg16_1_edx
	EXTERN  rol_reg16_1_ebx
	EXTERN  rol_reg16_1_esp
	EXTERN  rol_reg16_1_ebp
	EXTERN  rol_reg16_1_esi
	EXTERN  rol_reg16_1_edi
	EXTERN  ror_reg16_1_eax
	EXTERN  ror_reg16_1_ecx
	EXTERN  ror_reg16_1_edx
	EXTERN  ror_reg16_1_ebx
	EXTERN  ror_reg16_1_esp
	EXTERN  ror_reg16_1_ebp
	EXTERN  ror_reg16_1_esi
	EXTERN  ror_reg16_1_edi
	EXTERN  rcl_reg16_1_eax
	EXTERN  rcl_reg16_1_ecx
	EXTERN  rcl_reg16_1_edx
	EXTERN  rcl_reg16_1_ebx
	EXTERN  rcl_reg16_1_esp
	EXTERN  rcl_reg16_1_ebp
	EXTERN  rcl_reg16_1_esi
	EXTERN  rcl_reg16_1_edi
	EXTERN  rcr_reg16_1_eax
	EXTERN  rcr_reg16_1_ecx
	EXTERN  rcr_reg16_1_edx
	EXTERN  rcr_reg16_1_ebx
	EXTERN  rcr_reg16_1_esp
	EXTERN  rcr_reg16_1_ebp
	EXTERN  rcr_reg16_1_esi
	EXTERN  rcr_reg16_1_edi
	EXTERN  shl_reg16_1_eax
	EXTERN  shl_reg16_1_ecx
	EXTERN  shl_reg16_1_edx
	EXTERN  shl_reg16_1_ebx
	EXTERN  shl_reg16_1_esp
	EXTERN  shl_reg16_1_ebp
	EXTERN  shl_reg16_1_esi
	EXTERN  shl_reg16_1_edi
	EXTERN  shr_reg16_1_eax
	EXTERN  shr_reg16_1_ecx
	EXTERN  shr_reg16_1_edx
	EXTERN  shr_reg16_1_ebx
	EXTERN  shr_reg16_1_esp
	EXTERN  shr_reg16_1_ebp
	EXTERN  shr_reg16_1_esi
	EXTERN  shr_reg16_1_edi
	EXTERN  sar_reg16_1_eax
	EXTERN  sar_reg16_1_ecx
	EXTERN  sar_reg16_1_edx
	EXTERN  sar_reg16_1_ebx
	EXTERN  sar_reg16_1_esp
	EXTERN  sar_reg16_1_ebp
	EXTERN  sar_reg16_1_esi
	EXTERN  sar_reg16_1_edi

; ------------------- D3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,CL ---
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags
	GLOBAL	op_d3_67
op_d3_67
	modrm_jump_32_tbl op_d3_67_jump
	modrm_tbl_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, shl_16, sar_16, CL
	DCD rol_ax_CL, rol_cx_CL, rol_dx_CL, rol_bx_CL, rol_sp_CL, rol_bp_CL, rol_si_CL, rol_di_CL
	DCD ror_ax_CL, ror_cx_CL, ror_dx_CL, ror_bx_CL, ror_sp_CL, ror_bp_CL, ror_si_CL, ror_di_CL
	DCD rcl_ax_CL, rcl_cx_CL, rcl_dx_CL, rcl_bx_CL, rcl_sp_CL, rcl_bp_CL, rcl_si_CL, rcl_di_CL
	DCD rcr_ax_CL, rcr_cx_CL, rcr_dx_CL, rcr_bx_CL, rcr_sp_CL, rcr_bp_CL, rcr_si_CL, rcr_di_CL
	DCD shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	DCD shr_ax_CL, shr_cx_CL, shr_dx_CL, shr_bx_CL, shr_sp_CL, shr_bp_CL, shr_si_CL, shr_di_CL
	DCD shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	DCD sar_ax_CL, sar_cx_CL, sar_dx_CL, sar_bx_CL, sar_sp_CL, sar_bp_CL, sar_si_CL, sar_di_CL

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper rol_16, ror_16, rcl_16, rcr_16, shl_16, shr_16, "skip", sar_16, "r0", CL

	LTORG
	
	EXTERN	rol_ax_CL
	EXTERN	rol_cx_CL
	EXTERN  rol_dx_CL
	EXTERN  rol_bx_CL
	EXTERN  rol_sp_CL
	EXTERN  rol_bp_CL
	EXTERN  rol_si_CL
	EXTERN  rol_di_CL
	EXTERN	ror_ax_CL
	EXTERN  ror_cx_CL
	EXTERN  ror_dx_CL
	EXTERN  ror_bx_CL
	EXTERN  ror_sp_CL
	EXTERN  ror_bp_CL
	EXTERN  ror_si_CL
	EXTERN  ror_di_CL
	EXTERN	rcl_ax_CL
	EXTERN	rcl_cx_CL
	EXTERN  rcl_dx_CL
	EXTERN  rcl_bx_CL
	EXTERN  rcl_sp_CL
	EXTERN  rcl_bp_CL
	EXTERN  rcl_si_CL
	EXTERN  rcl_di_CL
	EXTERN	rcr_ax_CL
	EXTERN  rcr_cx_CL
	EXTERN  rcr_dx_CL
	EXTERN  rcr_bx_CL
	EXTERN  rcr_sp_CL
	EXTERN  rcr_bp_CL
	EXTERN  rcr_si_CL
	EXTERN  rcr_di_CL
	EXTERN	shl_ax_CL
	EXTERN  shl_cx_CL
	EXTERN  shl_dx_CL
	EXTERN  shl_bx_CL
	EXTERN  shl_sp_CL
	EXTERN  shl_bp_CL
	EXTERN  shl_si_CL
	EXTERN  shl_di_CL
	EXTERN	shr_ax_CL
	EXTERN  shr_cx_CL
	EXTERN  shr_dx_CL
	EXTERN  shr_bx_CL
	EXTERN  shr_sp_CL
	EXTERN  shr_bp_CL
	EXTERN  shr_si_CL
	EXTERN  shr_di_CL
	EXTERN	sar_ax_CL
	EXTERN  sar_cx_CL
	EXTERN  sar_dx_CL
	EXTERN  sar_bx_CL
	EXTERN  sar_sp_CL
	EXTERN  sar_bp_CL
	EXTERN  sar_si_CL
	EXTERN  sar_di_CL

; ------------------- F7 = ??? r/m16 ----------------------------------
op_f7_67
	modrm_jump_32_tbl op_f7_67_jump
; 0
	modrm_help_1_0 test, imm16, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_0 not_w
	modrm_help_1_0 neg_w
	modrm_help_1_0 mul_w
	modrm_help_1_0 imul_w
	modrm_help_1_0 div_w
	modrm_help_1_0 idiv_w
;0x40
	modrm_help_1_40 test, imm16, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_40 not_w
	modrm_help_1_40 neg_w
	modrm_help_1_40 mul_w
	modrm_help_1_40 imul_w
	modrm_help_1_40 div_w
	modrm_help_1_40 idiv_w
;0x80
	modrm_help_1_80 test, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_80 not_w
	modrm_help_1_80 neg_w
	modrm_help_1_80 mul_w
	modrm_help_1_80 imul_w
	modrm_help_1_80 div_w
	modrm_help_1_80 idiv_w
;0xc0 = mod = 11b => register operand
	DCD test_ax_imm16, test_cx_imm16, test_dx_imm16, test_bx_imm16, test_sp_imm16, test_bp_imm16, test_si_imm16, test_di_imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD not_ax, not_cx, not_dx, not_bx, not_sp, not_bp, not_si, not_di
	DCD neg_ax, neg_cx, neg_dx, neg_bx, neg_sp, neg_bp, neg_si, neg_di
	DCD mul_ax, mul_cx, mul_dx, mul_bx, mul_sp, mul_bp, mul_si, mul_di
	DCD imul_ax, imul_cx, imul_dx, imul_bx, imul_sp, imul_bp, imul_si, imul_di
	DCD div_ax, div_cx, div_dx, div_bx, div_sp, div_bp, div_si, div_di
	DCD idiv_ax, idiv_cx, idiv_dx, idiv_bx, idiv_sp, idiv_bp, idiv_si, idiv_di

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_1_help test, test_r0, imm16, imm16
	modrm_1_help not_w, not_w_r0
	modrm_1_help neg_w, neg_w_r0
	modrm_1_help mul_w, mul_w_r0
	modrm_1_help imul_w, imul_w_r0
	modrm_1_help div_w, div_w_r0
	modrm_1_help idiv_w, idiv_w_r0

	LTORG

	EXTERN	test_r0_imm16
	EXTERN	test_r0_bp_imm16
	EXTERN	not_w_r0
	EXTERN	not_w_r0_bp
	EXTERN	neg_w_r0
	EXTERN	neg_w_r0_bp
	EXTERN	mul_w_r0
	EXTERN	mul_w_r0_bp
	EXTERN	imul_w_r0
	EXTERN	imul_w_r0_bp
	EXTERN	div_w_r0
	EXTERN	div_w_r0_bp
	EXTERN	idiv_w_r0
	EXTERN	idiv_w_r0_bp

	EXTERN  test_ax_imm16 
	EXTERN  test_cx_imm16 
	EXTERN  test_dx_imm16 
	EXTERN  test_bx_imm16 
	EXTERN  test_sp_imm16 
	EXTERN  test_bp_imm16 
	EXTERN  test_si_imm16 
	EXTERN  test_di_imm16
	EXTERN  not_ax 
	EXTERN  not_cx 
	EXTERN  not_dx 
	EXTERN  not_bx 
	EXTERN  not_sp 
	EXTERN  not_bp 
	EXTERN  not_si 
	EXTERN  not_di
	EXTERN  neg_ax 
	EXTERN  neg_cx 
	EXTERN  neg_dx 
	EXTERN  neg_bx 
	EXTERN  neg_sp 
	EXTERN  neg_bp 
	EXTERN  neg_si 
	EXTERN  neg_di
	EXTERN  mul_ax 
	EXTERN  mul_cx 
	EXTERN  mul_dx 
	EXTERN  mul_bx 
	EXTERN  mul_sp 
	EXTERN  mul_bp 
	EXTERN  mul_si 
	EXTERN  mul_di
	EXTERN  imul_ax 
	EXTERN  imul_cx 
	EXTERN  imul_dx 
	EXTERN  imul_bx 
	EXTERN  imul_sp 
	EXTERN  imul_bp 
	EXTERN  imul_si 
	EXTERN  imul_di
	EXTERN  div_ax 
	EXTERN  div_cx 
	EXTERN  div_dx 
	EXTERN  div_bx 
	EXTERN  div_sp 
	EXTERN  div_bp 
	EXTERN  div_si 
	EXTERN  div_di
	EXTERN  idiv_ax 
	EXTERN  idiv_cx 
	EXTERN  idiv_dx 
	EXTERN  idiv_bx 
	EXTERN  idiv_sp 
	EXTERN  idiv_bp 
	EXTERN  idiv_si 
	EXTERN  idiv_di


; ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
op_ff_67
	modrm_jump_32_tbl op_ff_67_jump
	modrm_tbl_oper inc_word, dec_word, call_near, call_far, jmp_near, back2, push_word, back2
	; 0xc0 = mod = 11b => register operand
	DCD op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47		; INC AX ... INC DI
	DCD op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f		; DEC AX ... DEC DI
	DCD call_near_ax, call_near_cx, call_near_dx, call_near_bx, call_near_sp, call_near_bp, call_near_si, call_near_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD jmp_near_ax, jmp_near_cx, jmp_near_dx, jmp_near_bx, jmp_near_sp, jmp_near_bp, jmp_near_si, jmp_near_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57		; PUSH AX ... PUSH DI
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_67, CODE, READONLY
	ALIGN	4

	modrm_genall_oper inc_word, dec_word, call_near, call_far, jmp_near, skip, push_word, skip, "r0"

	LTORG
	
	EXTERN	call_near_ax 
	EXTERN  call_near_cx 
	EXTERN  call_near_dx 
	EXTERN  call_near_bx 
	EXTERN  call_near_sp 
	EXTERN  call_near_bp 
	EXTERN  call_near_si 
	EXTERN  call_near_di
	EXTERN	jmp_near_ax 
	EXTERN  jmp_near_cx 
	EXTERN  jmp_near_dx 
	EXTERN  jmp_near_bx 
	EXTERN  jmp_near_sp 
	EXTERN  jmp_near_bp 
	EXTERN  jmp_near_si 
	EXTERN  jmp_near_di

	AREA	jumptables, DATA, READONLY
	ALIGN	4

	;---------------------------------------------
	; Operand Size = 16bit, Address Size = 32bit
	;---------------------------------------------
opcodetable_16_32
; 0x00 (prefix 67)
	DCD op_00_USE32, op_01_67, op_02_USE32, op_03_67, op_04, op_05, op_06, op_07
	DCD op_08_USE32, op_09_67, op_0a_USE32, op_0b_67, op_0c, op_0d, op_0e, op_0f_67
	DCD op_10_USE32, op_11_67, op_12_USE32, op_13_67, op_14, op_15, op_16, op_17
	DCD op_18_USE32, op_19_67, op_1a_USE32, op_1b_67, op_1c, op_1d, op_1e, op_1f
	DCD op_20_USE32, op_21_67, op_22_USE32, op_23_67, op_24, op_25, op_26_67, op_27
	DCD op_28_USE32, op_29_67, op_2a_USE32, op_2b_67, op_2c, op_2d, op_2e_67, op_2f
	DCD op_30_USE32, op_31_67, op_32_USE32, op_33_67, op_34, op_35, op_36_67, op_37
	DCD op_38_USE32, op_39_67, op_3a_USE32, op_3b_67, op_3c, op_3d, op_3e_67, op_3f
; 0x40 (prefix 67)
	DCD op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47
	DCD op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f
	DCD op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57
	DCD op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
	DCD op_60, op_61, unknown, unknown, op_64_67, op_65_67, op_66_67_USE16, op_66_67_USE32
	DCD op_68, op_69_67, op_6a, op_6b_67, unknown, unknown, unknown, unknown
	DCD op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	DCD op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
; 0x80 (prefix 67)
	DCD op_80_USE32, op_81_67, op_82_USE32, op_83_67, op_84_USE32, op_85_67, op_86_USE32, op_87_67
	DCD op_88_USE32, op_89_67, op_8a_USE32, op_8b_67, op_8c_67, op_8d_67, op_8e_USE32, op_8f_67
	DCD loop, xchg_ax_cx, xchg_ax_dx, xchg_ax_bx, xchg_ax_sp, xchg_ax_bp, xchg_ax_si, xchg_ax_di
	DCD op_98, op_99, unknown, loop, op_9c, op_9d, op_9e, op_9f
	DCD op_a0_USE32, op_a1_67, op_a2_USE32, op_a3_67, op_a4_movsb_USE32, op_a5_movsw_USE32, op_a6_cmpsb_USE32, op_a7_cmpsw_USE32
	DCD op_a8, op_a9, op_aa_stosb_USE32, op_ab_stosw_USE32, op_ac_lodsb_USE32, op_ad_lodsw_USE32, op_ae_scasb_USE32, op_af_scasw_USE32
	DCD op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	DCD op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
; 0xC0 (prefix 67)
	DCD op_c0_USE32, op_c1_67, op_c2, op_c3, op_c4_67, op_c5_67, op_c6_USE32, op_c7_67
	DCD unknown, unknown, op_ca, op_cb, unknown, unknown, unknown, op_cf
	DCD op_d0_USE32, op_d1_67, op_d2_USE32, op_d3_67, op_d4, unknown, unknown, op_d7_USE32
	IF USE_FPU_OPCODES = 1
	DCD op_d8_USE32, op_d9_USE32, op_da_USE32, op_db_USE32, op_dc_USE32, op_dd_USE32, op_de_USE32, op_df_USE32
	ELSE
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	ENDIF
	DCD op_e0_USE32, op_e1_USE32, op_e2_USE32, op_e3_USE32, op_e4_in_al_imm8, op_e5_in_ax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_ax
	DCD unknown, unknown, unknown, unknown, op_ec_in_al_dx, op_ed_in_ax_dx, op_ee_out_dx_al, op_ef_out_dx_ax
	DCD unknown, unknown, op_f2_67, op_f3_67, op_f4, op_f5, op_f6_USE32, op_f7_67
	DCD op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe_USE32, op_ff_67


	END
