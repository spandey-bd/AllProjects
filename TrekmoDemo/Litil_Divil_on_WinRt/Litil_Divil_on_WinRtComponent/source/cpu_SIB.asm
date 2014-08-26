;=============================================================================
; cpu_SIB.s
;
; This file contains the SIB table generator macros and SIB byte effective
; address calculation routines needed in 32-bit address decoding.
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

	AREA cpu_SIB, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

	EXTERN	unknown

	EXTERN	registers
	
	MACRO
	sib_item $base, $idx, $scale, $disp
	DCD	sib_$base_$idx_$scale_$disp
	MEND
	MACRO
	sib_row $reg, $d 
	sib_item eax, $reg, $d, ""
	sib_item ecx, $reg, $d, ""
	sib_item edx, $reg, $d, ""
	sib_item ebx, $reg, $d, ""
	sib_item esp, $reg, $d, ""
	sib_item disp32, $reg, $d, ""
	sib_item esi, $reg, $d, ""
	sib_item edi, $reg, $d, ""
	MEND
	MACRO
	sib_block $d
	sib_row eax, $d
	sib_row ecx, $d
	sib_row edx, $d
	sib_row ebx, $d
	sib_row "_", $d
	sib_row ebp, $d
	sib_row esi, $d
	sib_row edi, $d
	MEND

	MACRO
	handle_item $base, $idx, $scale
sib_$base_$idx_$scale_
	IF "$base" = "ebp"
		mem_handler_bp
	ELIF "$base" = "esp"
		mem_handler_bp
	ENDIF
	IF $scale = 0
		;------
		; SIB, no scale
		;------
		IF "$base" = "disp32"	
			;------
			; This is disp32 with no base if MOD = 00
			;------
			mov		lr, r1
			r0_from_disp32
			IF "$idx" = "_"
			ELSE	
				add		r0, $idx
			ENDIF	
			bx		lr
		ELSE
			IF "$idx" = "_"
				mov		r0, $base
			ELSE	
				add		r0, $base, $idx
			ENDIF	
		ENDIF	
	ELSE
		;------
		; SIB, scale
		;------
		IF "$base" = "disp32"	
			;------
			; This is disp32 with no base if MOD = 00
			;------
			mov		lr, r1
			r0_from_disp32
			IF "$idx" = "_"
			ELSE	
				add		r0, $idx, lsl #$scale
			ENDIF	
			bx		lr			; Jump to the original r1 address
		ELSE
			IF "$idx" = "_"
				mov		r0, $base
			ELSE	
				add		r0, $base, $idx, lsl #$scale
			ENDIF	
		ENDIF	
	ENDIF
	bx		r1
	MEND
	MACRO
	handle_row $reg, $d
	handle_item eax, $reg, $d
	handle_item ecx, $reg, $d
	handle_item edx, $reg, $d
	handle_item ebx, $reg, $d
	handle_item esp, $reg, $d
	handle_item "disp32", $reg, $d
	handle_item esi, $reg, $d
	handle_item edi, $reg, $d
	MEND
	MACRO
	handle_block $d
	handle_row eax, $d
	handle_row ecx, $d
	handle_row edx, $d
	handle_row ebx, $d
	handle_row "_", $d
	handle_row ebp, $d
	handle_row esi, $d
	handle_row edi, $d
	MEND

	AREA	jumptables, DATA, READONLY
	ALIGN	4

	GLOBAL	sib_table
sib_table
	sib_block 0
	sib_block 1
	sib_block 2
	sib_block 3

	AREA cpu_SIB, CODE, READONLY
	ALIGN	4

	handle_block 0
	handle_block 1
	handle_block 2
	handle_block 3

	MACRO
	sib_row_d $reg, $d, $x
	sib_item eax, $reg, $d, $x
	sib_item ecx, $reg, $d, $x
	sib_item edx, $reg, $d, $x
	sib_item ebx, $reg, $d, $x
	sib_item esp, $reg, $d, $x
	sib_item ebp, $reg, $d, $x
	sib_item esi, $reg, $d, $x
	sib_item edi, $reg, $d, $x
	MEND
	MACRO
	sib_block_d $d, $x
	sib_row_d eax, $d, $x
	sib_row_d ecx, $d, $x
	sib_row_d edx, $d, $x
	sib_row_d ebx, $d, $x
	sib_row_d "_", $d, $x
	sib_row_d ebp, $d, $x
	sib_row_d esi, $d, $x
	sib_row_d edi, $d, $x
	MEND

	MACRO
	handle_item_d8 $base, $idx, $scale
sib_$base_$idx_$scale_d8
	IF "$base" = "ebp"
		mem_handler_bp
	ELIF "$base" = "esp"
		mem_handler_bp
	ENDIF
	IF $scale = 0
		;------
		; SIB, no scale
		;------
		IF "$idx" = "_"
			r0_from_idx_disp8 $base
		ELSE	
			r0_from_idx_disp8 $base
			add		r0, $idx
		ENDIF	
	ELSE
		;------
		; SIB, scale
		;------
		IF "$idx" = "_"
			r0_from_idx_disp8 $base
		ELSE	
			r0_from_idx_disp8 $base
			add		r0, $idx, lsl #$scale
		ENDIF	
	ENDIF
	bx		r1
	MEND
	MACRO
	handle_row_d8 $reg, $d
	handle_item_d8 eax, $reg, $d
	handle_item_d8 ecx, $reg, $d
	handle_item_d8 edx, $reg, $d
	handle_item_d8 ebx, $reg, $d
	handle_item_d8 esp, $reg, $d
	handle_item_d8 ebp, $reg, $d
	handle_item_d8 esi, $reg, $d
	handle_item_d8 edi, $reg, $d
	MEND
	MACRO
	handle_block_d8 $d
	handle_row_d8 eax, $d
	handle_row_d8 ecx, $d
	handle_row_d8 edx, $d
	handle_row_d8 ebx, $d
	handle_row_d8 "_", $d
	handle_row_d8 ebp, $d
	handle_row_d8 esi, $d
	handle_row_d8 edi, $d
	MEND

	AREA	jumptables, DATA, READONLY
	ALIGN	4

	GLOBAL	sib_disp8_table
sib_disp8_table
	sib_block_d 0, d8
	sib_block_d 1, d8
	sib_block_d 2, d8
	sib_block_d 3, d8

	AREA cpu_SIB, CODE, READONLY
	ALIGN	4

	handle_block_d8 0
	handle_block_d8 1
	handle_block_d8 2
	handle_block_d8 3

	MACRO
	handle_item_d32 $base, $idx, $scale
sib_$base_$idx_$scale_d32
	IF "$base" = "ebp"
		mem_handler_bp
	ELIF "$base" = "esp"
		mem_handler_bp
	ENDIF
	mov		lr, r1
	IF $scale = 0
		;------
		; SIB, no scale
		;------
		IF "$idx" = "_"
			r0_from_idx_disp32 $base
		ELSE	
			r0_from_idx_disp32 $base
			add		r0, $idx
		ENDIF	
	ELSE
		;------
		; SIB, scale
		;------
		IF "$idx" = "_"
			r0_from_idx_disp32 $base
		ELSE	
			r0_from_idx_disp32 $base
			add		r0, $idx, lsl #$scale
		ENDIF	
	ENDIF
	bx		lr			; Jump to original r1 value
	MEND
	MACRO 
	handle_row_d32 $reg, $d
	handle_item_d32 eax, $reg, $d
	handle_item_d32 ecx, $reg, $d
	handle_item_d32 edx, $reg, $d
	handle_item_d32 ebx, $reg, $d
	handle_item_d32 esp, $reg, $d
	handle_item_d32 ebp, $reg, $d
	handle_item_d32 esi, $reg, $d
	handle_item_d32 edi, $reg, $d
	MEND
	MACRO 
	handle_block_d32 $d
	handle_row_d32 eax, $d
	handle_row_d32 ecx, $d
	handle_row_d32 edx, $d
	handle_row_d32 ebx, $d
	handle_row_d32 "_", $d
	handle_row_d32 ebp, $d
	handle_row_d32 esi, $d
	handle_row_d32 edi, $d
	MEND


	AREA	jumptables, DATA, READONLY
	ALIGN	4

	GLOBAL	sib_disp32_table
sib_disp32_table
	sib_block_d 0, d32
	sib_block_d 1, d32
	sib_block_d 2, d32
	sib_block_d 3, d32

	AREA cpu_SIB, CODE, READONLY
	ALIGN	4

	handle_block_d32 0
	handle_block_d32 1
	handle_block_d32 2
	handle_block_d32 3

	END
