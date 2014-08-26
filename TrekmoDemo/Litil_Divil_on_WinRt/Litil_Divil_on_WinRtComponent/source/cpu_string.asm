;=============================================================================
; cpu_string.s
;
; This file contains string opcode handlers, both for the single operation versions
; and for the REP-prefixed multi-byte string operations.
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

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

	GLOBAL	bad_string_op_seg_back1
	GLOBAL	bad_string_op_seg

	GLOBAL op_af_scasw
	
	GLOBAL	op_f3

	GLOBAL rep_stosw_cld
	
	GLOBAL repe_cmpsb_cld

	EXTERN	loop
	EXTERN	restore_flags_from_r0
	EXTERN	complement_carry
	EXTERN	unknown

	EXTERN	registers
	
	EXTERN	BreakReason
	EXTERN	BRUns386Op

	EXTERN	rep_insb
	EXTERN	rep_outsb
	EXTERN	rep_insb_cld_USE32
	EXTERN	rep_outsb_cld_USE32

; ------------------- ERROR JUMP --------------------------------------
;
bad_string_op_seg_back1
	movs	r5, r5, lsl #16			; Restore CX value
	moveq	r5, #0x00010000			; CX = 0 if CX was 1, so restore the value
	sub		r12,r12,#1				; Rewind the PC to point to the REP opcode
bad_string_op_seg
	ldr		r0, =BRUnsString
	ldr		r1, =BreakReason
	str		r0, [r1]				; ... tell we break because of an unsupported string opcode segment
	b		unknown

bad_386_string_op_seg_back1
	movs	r5, r5, lsl #16			; Restore CX value
	moveq	r5, #0x00010000			; CX = 0 if CX was 1, so restore the value
	sub		r12,r12,#1				; Rewind the PC to point to the REP opcode
bad_386_string_op_seg
	ldr		r0, =BRUns386Op
	ldr		r1, =BreakReason
	str		r0, [r1]				; ... tell we break because of an unsupported string opcode segment
	b		unknown

	LTORG
	
; ------------------- A4 = MOVSB --------------------------------------
;
; Move a byte from DS:SI to ES:DI
;
	GLOBAL op_a4_movsb
op_a4_movsb
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #7					; Put it into (1<<17) position
	rsb		r1, #0x00010000			; Now r1 = (-1 or 1)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 op_a4_from_RAM, op_a4_from_EGA, op_a4_from_MODEX	

	EXTERN	op_a4_from_EGA
	EXTERN	op_a4_from_MODEX

	;-------
	; On input
	;	r1 = DI increment/decrement value
	;	r2 = DS:SI physical address
	;	r3 = Address size mask
	;-------
op_a4_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	ldrb	r1, [r2]				; r1 = the byte from DS:SI
	mem_handler_jump_r0r3_ES op_a4_to_RAM, op_a4_to_EGA, op_a4_to_MODEX

	EXTERN	op_a4_to_EGA
	EXTERN	op_a4_to_MODEX

	;-------
	; On input
	;	r1 = byte read from DS:SI
	;	r2 = ES:DI physical address
	;-------
	GLOBAL	op_a4_to_RAM
op_a4_to_RAM
	strb	r1, [r2]				; Write the byte to ES:DI
	b		loop

	GLOBAL	op_a4_movsb_USE32
op_a4_movsb_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #9					; Put it into (1<<1) position
	rsb		r1, #1					; Now r1 = (-1 or 1)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 op_a4_from_RAM_USE32, op_a4_from_EGA_USE32, op_a4_from_MODEX_USE32	

	EXTERN	op_a4_from_EGA_USE32
	EXTERN	op_a4_from_MODEX_USE32
	
	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:ESI physical address
	;	r3 = Address size mask
	;-------
op_a4_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]				; r1 = the byte from DS:SI
	mem_handler_jump_r0r3_ES_far op_a4_to_RAM, op_a4_to_EGA, op_a4_to_MODEX

	EXTERN	op_a4_to_EGA
	EXTERN	op_a4_to_MODEX

; ------------------- A5 = MOVSW --------------------------------------
;
; Move halfword from DS:SI to ES:DI
;
	GLOBAL	op_a5_movsw
op_a5_movsw
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #8					; Put it into (1<<18) position
	rsb		r1, #0x00020000			; Now r1 = (-2 or 2)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 op_a5_from_RAM, op_a5_from_EGA, op_a5_from_MODEX	
	
	EXTERN	op_a5_from_EGA
	EXTERN	op_a5_from_MODEX	

op_a5_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ldrb	r1, [r2]				; Get the low byte from DS:SI
	ldrb	r2, [r2, #1]			; Get the high byte from DS:SI+1
	ror		edi, #16
	orr		r1, r2, lsl #8			; r1 = halfword read from DS:SI
	mem_handler_jump_r0r3_ES op_a5_to_RAM, op_a5_to_EGA, op_a5_to_MODEX

	EXTERN	op_a5_to_EGA
	EXTERN	op_a5_to_MODEX
	
	;-------
	; On input
	;	r1 = halfword read from DS:SI
	;	r2 = ES:DI physical address
	;-------
	GLOBAL	op_a5_to_RAM
op_a5_to_RAM
	strb	r1, [r2]				; Write the byte to ES:DI
	lsr		r1, #8
	strb	r1, [r2, #1]			; Write the byte to ES:DI+1
	b		loop

	GLOBAL	op_a5_movsw_USE32
op_a5_movsw_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #8					; Put it into (1<<2) position
	rsb		r1, #2					; Now r1 = (-2 or 2)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 op_a5_from_RAM_USE32, op_a5_from_EGA_USE32, op_a5_from_MODEX_USE32

	EXTERN	op_a5_from_EGA_USE32
	EXTERN	op_a5_from_MODEX_USE32
	
op_a5_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]				; Get the low byte from DS:SI
	ldrb	r2, [r2, #1]			; Get the high byte from DS:SI+1
	orr		r1, r2, lsl #8			; r1 = halfword read from DS:SI
	mem_handler_jump_r0r3_ES_far op_a5_to_RAM, op_a5_to_EGA, op_a5_to_MODEX

	EXTERN	op_a5_to_EGA
	EXTERN	op_a5_to_MODEX
	
; ------------------- A5 = MOVSD --------------------------------------
;
; Move word from DS:ESI to ES:EDI
;
	GLOBAL	op_a5_movsd
op_a5_movsd
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #9					; Put it into (1<<19) position
	rsb		r1, #0x00040000			; Now r1 = (-4 or 4)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 movsd_from_RAM, movsd_from_EGA, movsd_from_MODEX	

	EXTERN	movsd_from_EGA
	EXTERN	movsd_from_MODEX	

movsd_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ldrb	r1, [r2]
	ldrb	lr, [r2, #1]
	ror		edi, #16
	orr		r1, lr, lsl #8
	ldrb	lr, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, lr, lsl #16
	orr		r1, r2, lsl #24
	mem_handler_jump_r0r3_ES movsd_to_RAM, movsd_to_EGA, movsd_to_MODEX

	EXTERN	movsd_to_EGA
	EXTERN	movsd_to_MODEX

	;-------
	; On input
	;	r1 = word read from DS:SI
	;	r2 = ES:DI physical address
	;-------
	GLOBAL	movsd_to_RAM
movsd_to_RAM
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	lsr		r1, #8
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	b		loop

	GLOBAL	op_a5_movsd_USE32
op_a5_movsd_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #7					; Put it into (1<<3) position
	rsb		r1, #4					; Now r1 = (-4 or 4)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 movsd_from_RAM_USE32, movsd_from_EGA_USE32, movsd_from_MODEX_USE32

	EXTERN	movsd_from_EGA_USE32
	EXTERN	movsd_from_MODEX_USE32

movsd_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]
	ldrb	r3, [r2, #1]
	orr		r1, r3, lsl #8
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r3, lsl #16
	mvn		r3, #0					; Use 32-bit memory addressing
	orr		r1, r2, lsl #24
	mem_handler_jump_r0r3_ES_far movsd_to_RAM, movsd_to_EGA, movsd_to_MODEX
	
	EXTERN	movsd_to_EGA
	EXTERN	movsd_to_MODEX

; ------------------- A6 = CMPSB --------------------------------------
;
	GLOBAL	op_a6_cmpsb
op_a6_cmpsb
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #7					; Put it into (1<<17) position
	rsb		r1, #0x00010000			; Now r1 = (-1 or 1)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 op_a6_from_RAM, bad_string_op_seg, bad_string_op_seg

op_a6_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ldrb	r1, [r2]				; r1 = the byte from DS:SI
	ror		edi, #16
	mem_handler_jump_r0r3_ES op_a6_to_RAM, bad_string_op_seg, bad_string_op_seg

	;-------
	; On input
	;	r0 = free
	;	r1 = byte read from DS:SI
	;	r2 = ES:DI physical address
	;-------
	GLOBAL	op_a6_to_RAM
op_a6_to_RAM
	ldrb	r0, [r2]				; r0 = byte from ES:DI
	lsl		r1, #24					; r1 = byte from DS:SI in highest byte
	subs	r1, r0, lsl #24			; Flags set by [DS:SI] - [ES:DI]
	b		complement_carry

	GLOBAL	op_a6_cmpsb_USE32
op_a6_cmpsb_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #9					; Put it into (1<<1) position
	rsb		r1, #1					; Now r1 = (-1 or 1)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 op_a6_from_RAM_USE32, bad_string_op_seg, bad_string_op_seg

	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:ESI physical address
	;	r3 = Address size mask
	;-------
op_a6_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]				; r1 = the byte from DS:SI
	mem_handler_jump_r0r3_ES_far op_a6_to_RAM, bad_string_op_seg, bad_string_op_seg

; ------------------- A7 = CMPSW --------------------------------------
;
	GLOBAL	op_a7_cmpsw
op_a7_cmpsw
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #8					; Put it into (1<<18) position
	rsb		r1, #0x00020000			; Now r1 = (-2 or 2)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 op_a7_from_RAM, bad_string_op_seg, bad_string_op_seg

op_a7_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ldrb	r1, [r2]				; Get the low byte from DS:SI
	ldrb	r2, [r2, #1]			; Get the high byte from DS:SI+1
	ror		edi, #16
	orr		r1, r2, lsl #8			; r1 = halfword read from DS:SI
	mem_handler_jump_r0r3_ES op_a7_to_RAM, bad_string_op_seg, bad_string_op_seg

	;-------
	; On input
	;	r0 = free
	;	r1 = halfword read from DS:SI
	;	r2 = ES:DI physical address
	;-------
op_a7_to_RAM
	ldrb	r0, [r2]				; Get the low byte from ES:DI
	ldrb	r2, [r2, #1]			; Get the high byte from ES:DI+1
	lsl		r1, #16
	orr		r0, r2, lsl #8			; r0 = halfword read from ES:DI
	subs	r1, r0, lsl #16			; Flags set by [DS:SI] - [ES:DI]
	b		complement_carry

	GLOBAL	op_a7_cmpsw_USE32
op_a7_cmpsw_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #8					; Put it into (1<<2) position
	rsb		r1, #2					; Now r1 = (-2 or 2)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 op_a7_from_RAM_USE32, bad_string_op_seg, bad_string_op_seg

op_a7_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]				; Get the low byte from DS:SI
	ldrb	r2, [r2, #1]			; Get the high byte from DS:SI+1
	orr		r1, r2, lsl #8			; r1 = halfword read from DS:SI
	mem_handler_jump_r0r3_ES_far op_a7_to_RAM, bad_string_op_seg, bad_string_op_seg

; ------------------- A7 = CMPSD --------------------------------------
;
	GLOBAL	op_a7_cmpsd
op_a7_cmpsd
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #9					; Put it into (1<<19) position
	rsb		r1, #0x00040000			; Now r1 = (-4 or 4)<<16
	ror		esi, #16
	add		esi, r1					; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 cmpsd_from_RAM, bad_string_op_seg, bad_string_op_seg

cmpsd_from_RAM
	mov		r0, edi					; r0 = DI
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ldrb	r1, [r2]
	ldrb	r3, [r2, #1]
	ror		edi, #16
	orr		r1, r3, lsl #8
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r3, lsl #16
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	orr		r1, r2, lsl #24
	mem_handler_jump_r0r3_ES cmpsd_to_RAM, bad_string_op_seg, bad_string_op_seg

	;-------
	; On input
	;	r0 = free
	;	r1 = halfword read from DS:SI
	;	r2 = ES:DI physical address
	;-------
cmpsd_to_RAM
	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	orr		r0, r3, lsl #8
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	subs	r1, r0					; Flags set by [DS:SI] - [ES:DI]
	b		complement_carry

	GLOBAL	op_a7_cmpsd_USE32
op_a7_cmpsd_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #7					; Put it into (1<<3) position
	rsb		r1, #4					; Now r1 = (-4 or 4)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 cmpsd_from_RAM_USE32, bad_string_op_seg, bad_string_op_seg

cmpsd_from_RAM_USE32
	mov		r0, edi					; r0 = EDI
	add		edi, r1					; Fix logical DI.
	ldrb	r1, [r2]
	ldrb	r3, [r2, #1]
	orr		r1, r3, lsl #8
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r3, lsl #16
	mvn		r3, #0					; Use 32-bit memory addressing
	orr		r1, r2, lsl #24
	mem_handler_jump_r0r3_ES_far cmpsd_to_RAM, bad_string_op_seg, bad_string_op_seg

; ------------------- AA = STOSB --------------------------------------
; We may not change the flags!
; Segment override does not affect STOSB, it always uses ES:DI
;
	GLOBAL op_aa_stosb
op_aa_stosb
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #7					; Put it into (1<<17) position
	rsb		r1, #0x00010000			; Now r1 = (-1 or 1)<<16
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES stosb_RAM, stosb_EGA, stosb_MODEX

	GLOBAL	op_aa_stosb_USE32
op_aa_stosb_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #9					; Put it into (1<<1) position
	rsb		r1, #1					; Now r1 = (-1 or 1)
	add		edi, r1					; Fix logical EDI.
	mem_handler_jump_r0r3_ES stosb_RAM, stosb_EGA, stosb_MODEX

	EXTERN	stosb_EGA
	EXTERN	stosb_MODEX
	
stosb_RAM
	strb	eax, [r2]				; Save the byte into ES:DI = physical ES:0000 + (unsigned)DI
	b		loop

; ------------------- AB = STOSW --------------------------------------
; We may not change the flags!
; Segment override does not affect STOSW, it always uses ES:DI
;
	GLOBAL	op_ab_stosw
op_ab_stosw
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #8					; Put it into (1<<18) position
	rsb		r1, #0x00020000			; Now r1 = (-2 or 2)<<16
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES stosw_RAM, stosw_EGA, stosw_MODEX
	
	GLOBAL	op_ab_stosw_USE32
op_ab_stosw_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #8					; Put it into (1<<2) position
	rsb		r1, #2					; Now r1 = (-2 or 2)
	add		edi, r1					; Fix logical EDI.
	mem_handler_jump_r0r3_ES stosw_RAM, stosw_EGA, stosw_MODEX

	EXTERN	stosw_EGA
	EXTERN	stosw_MODEX

stosw_RAM
	strb	eax, [r2]				; Save the byte into ES:DI = physical ES:0000 + (unsigned)DI
	mov		r0, eax, lsr #8			; r0 = AH
	strb	r0, [r2, #1]			; Save the byte into ES:DI = physical ES:0000 + (unsigned)DI + 1
	b		loop

; ------------------- AB = STOSD --------------------------------------
; We may not change the flags!
; Segment override does not affect STOSW, it always uses ES:DI
;
	GLOBAL	op_ab_stosd
op_ab_stosd
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #9					; Put it into (1<<19) position
	rsb		r1, #0x00040000			; Now r1 = (-4 or 4)<<16
	add		edi, r1, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES stosd_RAM, stosd_EGA, stosd_MODEX
	
	GLOBAL	op_ab_stosd_USE32
op_ab_stosd_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #7					; Put it into (1<<3) position
	rsb		r1, #4					; Now r1 = (-4 or 4)<<16
	add		edi, r1					; Fix logical DI.
	mem_handler_jump_r0r3_ES stosd_RAM, stosd_EGA, stosd_MODEX

	EXTERN	stosd_EGA
	EXTERN	stosd_MODEX

stosd_RAM
	strb	eax, [r2]
	mov		r0, eax, lsr #8
	strb	r0, [r2, #1]
	mov		r0, eax, lsr #16
	strb	r0, [r2, #2]
	mov		r0, eax, lsr #24
	strb	r0, [r2, #3]
	b		loop

; ------------------- AC = LODSB --------------------------------------
; We may not change the flags!
;
	GLOBAL	op_ac_lodsb
op_ac_lodsb
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #7					; Put it into (1<<17) position
	rsb		r1, #0x00010000			; Now r1 = (-1 or 1)<<16
	add		esi, r1, esi, ror #16	; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 lodsb_RAM, lodsb_EGA, lodsb_MODEX

	GLOBAL	op_ac_lodsb_USE32
op_ac_lodsb_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = ESI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #9					; Put it into (1<<1) position
	rsb		r1, #1					; Now r1 = (-1 or 1)
	add		esi, r1					; Fix logical ESI.
	mem_handler_jump_r0r3 lodsb_RAM, lodsb_EGA, lodsb_MODEX

	EXTERN	lodsb_EGA
	EXTERN	lodsb_MODEX
	
lodsb_RAM
	ldrb	r0,[r2]					; Load byte to r0 from DS:SI = physical Effective Segment + (unsigned)SI
	bic		eax, #0xFF
	orr		eax, r0					; Put the byte to AL
	b		loop

; ------------------- AD = LODSW --------------------------------------
; We may not change the flags!
;
	GLOBAL	op_ad_lodsw
op_ad_lodsw
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #8					; Put it into (1<<18) position
	rsb		r1, #0x00020000			; Now r1 = (-2 or 2)<<16
	add		esi, r1, esi, ror #16	; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 lodsw_RAM, lodsw_EGA, lodsw_MODEX

	GLOBAL	op_ad_lodsw_USE32
op_ad_lodsw_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #8					; Put it into (1<<2) position
	rsb		r1, #2					; Now r1 = (-2 or 2)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 lodsw_RAM, lodsw_EGA, lodsw_MODEX

	EXTERN	lodsw_EGA
	EXTERN	lodsw_MODEX

lodsw_RAM
	ldrb	r0,[r2]					; Load low byte to r0
	ldrb	r1,[r2, #1]				; Load high byte to r1
	lsr		eax, #16
	orr		eax, r0, eax, lsl #16
	orr		eax, r1, lsl #8
	b		loop

; ------------------- AD = LODSD --------------------------------------
; We may not change the flags!
;
	GLOBAL	op_ad_lodsd
op_ad_lodsd
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsl		r1, #9					; Put it into (1<<19) position
	rsb		r1, #0x00040000			; Now r1 = (-4 or 4)<<16
	add		esi, r1, esi, ror #16	; Fix logical SI.
	ror		esi, #16
	mem_handler_jump_r0r3 lodsd_RAM, lodsd_EGA, lodsd_MODEX

	GLOBAL	op_ad_lodsd_USE32
op_ad_lodsd_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, esi					; r0 = SI
	and		r1, #FLAG_DF			; Get the direction flag (1<<10)
	lsr		r1, #7					; Put it into (1<<3) position
	rsb		r1, #4					; Now r1 = (-4 or 4)
	add		esi, r1					; Fix logical SI.
	mem_handler_jump_r0r3 lodsd_RAM, lodsd_EGA, lodsd_MODEX

	EXTERN	lodsd_EGA
	EXTERN	lodsd_MODEX

lodsd_RAM
	ldrb	eax,[r2]
	ldrb	r0,[r2, #1]
	ldrb	r1,[r2, #2]
	ldrb	r2,[r2, #3]
	orr		eax, r0, lsl #8
	orr		eax, r1, lsl #16
	orr		eax, r2, lsl #24
	b		loop

; ------------------- AE = SCASB --------------------------------------
; Segment override does not affect SCASB, it always uses ES:DI
;
	GLOBAL	op_ae_scasb
op_ae_scasb
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	tst		r1, #FLAG_DF			; Get the direction flag (1<<10)
	ror		edi, #16
	addeq	edi, #0x00010000		; Fix logical DI.
	subne	edi, #0x00010000		; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES scasb_RAM, scasb_EGA, bad_string_op_seg

	GLOBAL	op_ae_scasb_USE32
op_ae_scasb_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get flags (for direction flag checking)
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = DI
	tst		r1, #FLAG_DF			; Get the direction flag (1<<10)
	addeq	edi, #1					; Fix logical EDI.
	subne	edi, #1					; Fix logical EDI.
	mem_handler_jump_r0r3_ES scasb_RAM, scasb_EGA, bad_string_op_seg

	EXTERN	scasb_EGA

scasb_RAM
	ldrb  	r1, [r2]				; Get byte from ES:DI
	mov		r0, eax, lsl #24
	cmp		r0, r1, lsl #24			; Compare AL with byte
	b		complement_carry		; Go back to the opcode loop

; ------------------- AF = SCASW --------------------------------------
; Segment override does not affect SCASW, it always uses ES:DI
;
	GLOBAL	op_af_scasw
op_af_scasw
	ldr		r1, [sp, #SP_FLAGS]		; Get the FLAGS value
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	ror		edi, #16
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	addeq	edi, #0x00020000		; Increment DI
	subne	edi, #0x00020000		; Decrement DI
	ror		edi, #16
	mem_handler_jump_r0r3_ES scasw_RAM, bad_string_op_seg, bad_string_op_seg

	GLOBAL	op_af_scasw_USE32
op_af_scasw_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get the FLAGS value
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	addeq	edi, #2					; Increment EDI
	subne	edi, #2					; Decrement EDI
	mem_handler_jump_r0r3_ES scasw_RAM, bad_string_op_seg, bad_string_op_seg

scasw_RAM
	ldrb  	r0, [r2]				; Get byte from physical ES:DI
	ldrb	r1, [r2, #1]			; Get byte from ES:DI+1
	mov		r2, eax, lsl #16
	orr		r0, r1, lsl #8			; r0 = halfword from ES:DI
	cmp		r2, r0, lsl #16			; Compare AX with halfword from memory
	b		complement_carry		; Go back to the opcode loop

; ------------------- AF = SCASD --------------------------------------
; Segment override does not affect SCASD, it always uses ES:DI
;
	GLOBAL	op_af_scasd
op_af_scasd
	ldr		r1, [sp, #SP_FLAGS]		; Get the FLAGS value
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	ror		edi, #16
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	addeq	edi, #0x00040000		; Increment DI
	subne	edi, #0x00040000		; Decrement DI
	ror		edi, #16
	mem_handler_jump_r0r3_ES scasd_RAM, bad_string_op_seg, bad_string_op_seg

	GLOBAL	op_af_scasd_USE32
op_af_scasd_USE32
	ldr		r1, [sp, #SP_FLAGS]		; Get the FLAGS value
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	addeq	edi, #4					; Increment EDI
	subne	edi, #4					; Decrement EDI
	mem_handler_jump_r0r3_ES scasd_RAM, bad_string_op_seg, bad_string_op_seg

scasd_RAM
	ldrb  	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	cmp		eax, r0					; Compare EAX with word from memory
	b		complement_carry		; Go back to the opcode loop

; ------------------- F2 = REPNE/REPNZ ------------------------------
; Next opcode should be A6, A7, AE, AF, other opcodes work like F3
;
	MACRO
	segovr_test $jump
	cmp		r1, #0x36				; SS: segment override?
	ldreq	r2, [sp, #SP_SS_BASE]	; r2 = current effective logical SS segment
	beq		$jump
	cmp		r1, #0x26				; ES: segment override?
	ldreq	r2, [sp, #SP_ES_BASE]	; r2 = current effective logical ES segment
	beq		$jump
	cmp		r1, #0x2E				; CS: segment override?
	ldreq	r2, [sp, #SP_CS_BASE]	; r2 = current effective logical CS segment
	beq		$jump
	cmp		r1, #0x3E				; DS: segment override?
	ldreq	r2, [sp, #SP_DS_BASE]	; r2 = current effective logical DS segment
	beq		$jump
	cmp		r1, #0x64				; FS: segment override?
	ldreq	r2, [sp, #SP_FS_BASE]	; r2 = current effective logical FS segment
	beq		$jump
	cmp		r1, #0x65				; GS: segment override?
	ldreq	r2, [sp, #SP_GS_BASE]	; r2 = current effective logical GS segment
	beq		$jump
	MEND

	GLOBAL	op_f2_retry
op_f2_retry
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f2
op_f2
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f2_segovr						; Check for a segment override
	movs	r3, ecx, lsl #16
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f2_jump						; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]							; Jump to the handler

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f2_jump
	DCD		rep_movsb_cld, rep_movsw_cld, repne_cmpsb_cld, repne_cmpsw_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosw_cld
	DCD		rep_lodsb_cld, rep_lodsw_cld, repne_scasb_cld, repne_scasw_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsw_std, repne_cmpsb_std, repne_cmpsw_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosw_std
	DCD		rep_lodsb_std, rep_lodsw_std, repne_scasb_std, repne_scasw_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f2_segovr
	add		r1, #0xA4
	segovr_test op_f2_retry
	;-------
	; It was not a segment override, check for REP OUTSB or 386 operations
	;-------
	cmp		r1, #0x66						; 32-bit size override?	
	beq		iop_f2_66
	cmp		r1, #0x67						; 32-bit addressing override?	
	beq		iop_f2_67
	cmp		r1, #0x6E				; REP OUTSB opcode?
	beq		rep_outsb				; in "ports.s"
	cmp		r1, #0x6C				; REP INSB opcode?
	beq		rep_insb				; in "ports.s"
	b		bad_string_op_seg_back1	; Nope, unsupported opcode


; ------------------- F266 = REPNE/REPNZ -----------------------------
; Use 16-bit memory addresses but 32-bit transfers.
;
iop_f2_66
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f2_66
op_f2_66									; Called from "cpu_66.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f2_66_segovr					; Check for a segment override
	movs	r3, ecx, lsl #16
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f2_66_jump					; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f2_66_jump
	DCD		rep_movsb_cld, rep_movsd_cld, repne_cmpsb_cld, repne_cmpsd_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosd_cld
	DCD		rep_lodsb_cld, rep_lodsd_cld, repne_scasb_cld, repne_scasd_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsd_std, repne_cmpsb_std, repne_cmpsd_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosd_std
	DCD		rep_lodsb_std, rep_lodsd_std, repne_scasb_std, repne_scasd_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f2_66_segovr
	add		r1, #0xA4
	segovr_test iop_f2_66
	cmp		r1, #0x67							; 32-bit address override?	
	mvn		r3, #0								; t3 mask = 32-bit memory addressing
	beq		iop_f2_USE32
	b		unknown							; Unknown opcode!


; ------------------- F267 = REPNE/REPNZ ---------------------------
; Use 32-bit memory addresses but 16-bit transfers.
;
iop_f2_67
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f2_67
op_f2_67									; Called from "cpu_66.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f2_67_segovr					; Check for a segment override
	tst		ecx, ecx
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f2_67_jump					; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mvn		r3, #0								; Use 32-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f2_67_jump
	DCD	rep_movsb_cld, rep_movsw_cld, repne_cmpsb_cld, repne_cmpsw_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosw_cld
	DCD	rep_lodsb_cld, rep_lodsw_cld, repne_scasb_cld, repne_scasw_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD	rep_movsb_std, rep_movsw_std, repne_cmpsb_std, repne_cmpsw_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosw_std
	DCD	rep_lodsb_std, rep_lodsw_std, repne_scasb_std, repne_scasw_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f2_67_segovr
	add		r1, #0xA4
	segovr_test iop_f2_67
	cmp		r1, #0x66						; 32-bit operand override?	
	beq		iop_f2_USE32
	b		unknown					; Unknown opcode!

; ------------------- F26667 = REPNE/REPNZ -------------------------
; Use 32-bit memory addresses and 32-bit instead of 16-bit transfers.
;
iop_f2_USE32
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f2_USE32
op_f2_USE32										; Called from "cpu_386.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f2_USE32_segovr					; Check for a segment override
	tst		ecx, ecx
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f2_USE32_jump				; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mvn		r3, #0								; Use 32-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f2_USE32_jump
	DCD		rep_movsb_cld, rep_movsd_cld, repne_cmpsb_cld, repne_cmpsd_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosd_cld
	DCD		rep_lodsb_cld, rep_lodsd_cld, repne_scasb_cld, repne_scasd_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsd_std, repne_cmpsb_std, repne_cmpsd_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosd_std
	DCD		rep_lodsb_std, rep_lodsd_std, repne_scasb_std, repne_scasd_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f2_USE32_segovr
	add		r1, #0xA4
	segovr_test iop_f2_USE32
	cmp		r1, #0x66						; 16-bit operand override?	
	beq		iop_f2_67
	cmp		r1, #0x67						; 16-bit address override?	
	beq		iop_f2_66
	cmp		r1, #0x6e						; rep outsb?
	beq		rep_outsb_cld_USE32
	cmp		r1, #0x6c						; rep insb?
	beq		rep_insb_cld_USE32
	b		unknown					; Unknown opcode!

op_f3_noram
op_f3a4_cld_noRAM
op_f3a4_std_noRAM
op_f3a5_std_noRAM
op_f2ae_cld_noRAM
op_f2ae_std_noRAM
op_f2af_cld_noRAM
op_f2af_std_noRAM
	b		unknown					; Unknown opcode!

; ------------------- F3 = REP/REPE/REPZ ------------------------------
; 6C..6F = 01101100..01101111 = 011011XX, A4..AF = 10100100..10101111 = 1010XXXX
;
	GLOBAL	op_f3_retry
op_f3_retry
	msr		cpsr_f,r0							; Restore flags
op_f3
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f3_segovr						; Check for a segment override
	movs	r3, ecx, lsl #16
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f3_jump						; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f3_jump
	DCD		rep_movsb_cld, rep_movsw_cld, repe_cmpsb_cld, repe_cmpsw_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosw_cld
	DCD		rep_lodsb_cld, rep_lodsw_cld, repe_scasb_cld, repe_scasw_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsw_std, repe_cmpsb_std, repe_cmpsw_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosw_std
	DCD		rep_lodsb_std, rep_lodsw_std, repe_scasb_std, repe_scasw_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f3_segovr
	add		r1, #0xA4
	segovr_test op_f3_retry
	;-------
	; It was not a segment override, check for REP OUTSB or 386 operations
	;-------
	cmp		r1, #0x66						; 32-bit size override?	
	beq		iop_f3_66
	cmp		r1, #0x67						; 32-bit addressing override?	
	beq		iop_f3_67
	cmp		r1, #0x6E				; REP OUTSB opcode?
	beq		rep_outsb				; in "ports.s"
	cmp		r1, #0x6C				; REP INSB opcode?
	beq		rep_insb				; in "ports.s"
	b		bad_string_op_seg_back1	; Nope, unsupported opcode

	LTORG
	
; ------------------- F366 = REP/REPE/REPZ ---------------------------
; Use 32-bit instead of 16-bit transfers.
;
iop_f3_66
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f3_66
op_f3_66										; Called from "cpu_66.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f3_66_segovr						; Check for a segment override
	movs	r3, ecx, lsl #16
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f3_66_jump					; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f3_66_jump
	DCD		rep_movsb_cld, rep_movsd_cld, repe_cmpsb_cld, repe_cmpsd_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosd_cld
	DCD		rep_lodsb_cld, rep_lodsd_cld, repe_scasb_cld, repe_scasd_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsd_std, repe_cmpsb_std, repe_cmpsd_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosd_std
	DCD		rep_lodsb_std, rep_lodsd_std, repe_scasb_std, repe_scasd_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f3_66_segovr
	add		r1, #0xA4
	segovr_test iop_f3_66
	cmp		r1, #0x67							; 32-bit address override?	
	mvn		r3, #0								; t3 mask = 32-bit memory addressing
	beq		iop_f3_USE32
	b		unknown							; Unknown opcode!


; ------------------- F367 = REP/REPE/REPZ ---------------------------
; Use 32-bit memory addresses but 16-bit transfers.
;
iop_f3_67
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f3_67
op_f3_67									; Called from "cpu_66.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f3_67_segovr					; Check for a segment override
	tst		ecx, ecx
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f3_67_jump					; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mvn		r3, #0								; Use 32-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f3_67_jump
	DCD		rep_movsb_cld, rep_movsw_cld, repe_cmpsb_cld, repe_cmpsw_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosw_cld
	DCD		rep_lodsb_cld, rep_lodsw_cld, repe_scasb_cld, repe_scasw_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsw_std, repe_cmpsb_std, repe_cmpsw_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosw_std
	DCD		rep_lodsb_std, rep_lodsw_std, repe_scasb_std, repe_scasw_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4

	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f3_67_segovr
	add		r1, #0xA4
	segovr_test iop_f3_67
	cmp		r1, #0x66						; 32-bit operand override?	
	beq		iop_f3_USE32
	b		unknown					; Unknown opcode!

; ------------------- F36667 = REP/REPE/REPZ -------------------------
; Use 32-bit memory addresses and 32-bit instead of 16-bit transfers.
;
iop_f3_USE32
	msr		cpsr_f,r0							; Restore flags
	GLOBAL	op_f3_USE32
op_f3_USE32										; Called from "cpu_386.S"
	ldrb	r1,[r12],#1							; Load next opcode byte to r1, increment r12 by 1
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	subs	r1, #0xA4							; Use only 0xA4..0xAF
	blt		op_f3_USE32_segovr					; Check for a segment override
	tst		ecx, ecx
	beq		restore_flags_from_r0				; nothing to do if CX == 0, go back to loop
	ldr		r3, [sp, #SP_FLAGS]					; Get current flags (for FLAG_DF)
	msr		cpsr_f,r0							; Restore flags
	ldr		r0, =op_f3_USE32_jump				; r0 = address of the jump table
	and		r3, #FLAG_DF						; Leave only the "Direction" bit
	add		r1, r3, lsr #(10-4)					; Add 16 to the jump table index if direction flag is set.
	mvn		r3, #0								; Use 32-bit memory address masking
	add		r1, r0, r1, lsl #2					; r1 = actual jump address
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	ldr		pc, [r1]

	AREA jumptables, DATA, READONLY
	ALIGN	4
op_f3_USE32_jump
	DCD		rep_movsb_cld, rep_movsd_cld, repe_cmpsb_cld, repe_cmpsd_cld
	DCD 	unknown, unknown, rep_stosb_cld, rep_stosd_cld
	DCD		rep_lodsb_cld, rep_lodsd_cld, repe_scasb_cld, repe_scasd_cld
	DCD 	unknown, unknown, unknown, unknown
	DCD		rep_movsb_std, rep_movsd_std, repe_cmpsb_std, repe_cmpsd_std
	DCD 	unknown, unknown, rep_stosb_std, rep_stosd_std
	DCD		rep_lodsb_std, rep_lodsd_std, repe_scasb_std, repe_scasd_std

	AREA cpu_string, CODE, READONLY
	ALIGN	4


	;-------
	; Second byte not directly supported, check if it is a segment override.
	;------- 
op_f3_USE32_segovr
	add		r1, #0xA4
	segovr_test iop_f3_USE32
	cmp		r1, #0x66						; 16-bit operand override?	
	beq		iop_f3_67
	cmp		r1, #0x67						; 16-bit address override?	
	beq		iop_f3_66
	cmp		r1, #0x6e						; rep outsb?
	beq		rep_outsb_cld_USE32
	cmp		r1, #0x6c						; rep insb?
	beq		rep_insb_cld_USE32
	b		unknown					; Unknown opcode!

	LTORG
	
; ------------------- F3A4 REP MOVSB (CLD) ----------------------------
;
	GLOBAL	rep_movsb_cld_next
rep_movsb_cld_next
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	; On input r0 = saved flags
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
rep_movsb_cld
	;-------
	; Begin a byte move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	mov		r0, esi					; r0 = SI
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsb_cld_from_RAM, rep_movsb_cld_from_EGA, rep_movsb_cld_from_MODEX
	
	EXTERN	rep_movsb_cld_from_EGA
	EXTERN	rep_movsb_cld_from_MODEX
	
rep_movsb_cld_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsb_cld_RAM_RAM, rep_movsb_cld_RAM_EGA, rep_movsb_cld_RAM_MODEX
	
	EXTERN	rep_movsb_cld_RAM_EGA
	EXTERN	rep_movsb_cld_RAM_MODEX
	
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
rep_movsb_cld_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, lr}
	tst		r3, #0x80000000
	bne		movsb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2, pops r0 and r3 and return address
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		; ... else handle the next 16K page.

movsb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	mov		r3, ecx					; Start with r3 = ECX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2, pops r0 and r3 and return address
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		; ... else handle the next 16K page.

	;-------
	; Subroutine to move r3 bytes from r1 to r2.
	;-------
movs_cld_sub	
	;-------
	; Check for alignments and the type of move we can do
	;-------
	eor		r0, r1, r2
	tst		r0, #3					; Do the addresses have similar alignments?
	bne		movs_up_slow			; Nope, move the data slowly byte-by-byte
	;-------
	; If source < target and target < source+length, use a slow byte move.
	; (This is a bug in the program, should use STD version of the move!)
	;-------
	cmp		r1, r2
	addlt	r0, r1, r3
	cmplt	r2, r0
	blt		movs_up_slow			; Yep, move it byte-by-byte
	;-------
	; Move leading byte, if any
	;-------
	tst		r2, #1					; Byte or halfword aligned?
	ldrbne	r0, [r1], #1
	subne	r3, #1					; CX -= 1, first byte handled
	strbne	r0, [r2], #1
	;-------
	; More bytes to move?
	;-------
	cmp		r3, #1
	beq		movs_last_byte			; Only one more byte to do
	bxlt	lr						; If no more bytes to do, return to caller
	;-------
	; Move leading halfword, if any
	;-------
	tst		r2, #2
	ldrhne	r0, [r1], #2
	subne	r3, #2					; CX -= 2
	strhne	r0, [r2], #2
	;-------
	; Now the input address is word-aligned.
	;-------
movs_up_word_align
	cmp		r3, #8*4				; More than 8 words to move?
	bgt		movs_up_block			; More than 8 words to move, use the block code
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 0: Move 8 words
	cmp		r3, #7*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 1: Move 7 words
	cmp		r3, #6*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 2: Move 6 words
	cmp		r3, #5*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 3: Move 5 words
	cmp		r3, #4*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 4: Move 4 words
	cmp		r3, #3*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 5: Move 3 words
	cmp		r3, #2*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 6: Move 2 words
	cmp		r3, #1*4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 7: Move 1 words
	;-------
	; Move an extra halfword, if needed
	;-------
	tst		r3, #2					; Still a halfword to move?
	ldrhne	r0, [r1], #2
	strhne	r0, [r2], #2			; Yep, move the last halfword
	;-------
	; Move the last byte, if needed
	;-------
movs_last_byte
	tst		r3, #1					; Still a byte to move?
	ldrbne	r0, [r1]				; Get the last byte
	strbne	r0, [r2]				; Save the last byte
	bx		lr						; Return to caller
	;-------
	; Use block move when more than 8 words to move
	;-------
movs_up_block
	push	{r4-r11}
1	mov		r0, r3, lsr #5			; r1 = number of 32-byte blocks to move
	sub		r3, #(8*32)				; This many bytes handled
	cmp		r0, #8					; More than 8 blocks to move?
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 0 = 8
	cmp		r0, #7
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 1 = 7
	cmp		r0, #6
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 2 = 6
	cmp		r0, #5
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 3 = 5
	cmp		r0, #4
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 4 = 4
	cmp		r0, #3
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 5 = 3
	cmp		r0, #2
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 6 = 2
	cmp		r0, #1
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 7 = 1
	cmp		r3, #32					; Still more than 32 bytes to move?
	bge		%b1						; Yep, go back to loop
	pop		{r4-r11}
	ands	r3, #31					; Still bytes to move?
	bne		movs_up_word_align
	bx		lr						; Return to caller
	;-------
	; Move slowly one byte at a time. Addresses are misaligned or overlapping.
	;-------
movs_up_slow
	ldrb	r0, [r1], #1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #1
	bne		movs_up_slow			; Back to loop until r3 = 0
	bx		lr						; Return to caller

; ------------------- F3A4 REP MOVSB (STD) ----------------------------
;
	GLOBAL	rep_movsb_std_next
rep_movsb_std_next
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags from r0
rep_movsb_std
	;-------
	; Begin a byte move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	mov		r0, esi					; r0 = SI
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsb_std_from_RAM, rep_movsb_std_from_EGA, rep_movsb_std_from_MODEX
	
	EXTERN	rep_movsb_std_from_EGA
	EXTERN	rep_movsb_std_from_MODEX
	
rep_movsb_std_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsb_std_RAM_RAM, rep_movsb_std_RAM_EGA, rep_movsb_std_RAM_MODEX
	
	EXTERN	rep_movsb_std_RAM_EGA
	EXTERN	rep_movsb_std_RAM_MODEX
	
rep_movsb_std_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	subs	r3, #1					; Start with r3 = CX, decrease it by one for the page checks.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
1	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Move slowly one byte at a time.
	;-------
2	ldrb	r0, [r1], #-1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #-1
	bne		%b2						; Back to loop until r3 = 0
	pop		{r0, r3}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if CX == 0 ...
	b		rep_movsb_std_next		; Else continue with the next 16K block
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	subs	r3, #1					; Start with r3 = CX, decrease it by one for the page checks.
	beq		%f1
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		r3, #1
	sub		esi, r3					; Increment ESI by the number of bytes we shall move.
	sub		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2

; ------------------- F3A5 REP MOVSW (CLD) ----------------------------
;
	GLOBAL	rep_movsw_cld_next
rep_movsw_cld_next
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
rep_movsw_cld
	;-------
	; Begin a halfword move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	mov		r0, esi					; r0 = SI
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsw_cld_from_RAM, rep_movsw_cld_from_EGA, rep_movsw_cld_from_MODEX
	
	EXTERN	rep_movsw_cld_from_EGA
	EXTERN	rep_movsw_cld_from_MODEX
	
rep_movsw_cld_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_cld_RAM_RAM, rep_movsw_cld_RAM_EGA, rep_movsw_cld_RAM_MODEX
	
	EXTERN	rep_movsw_cld_RAM_EGA
	EXTERN	rep_movsw_cld_RAM_MODEX
	
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of halfwords to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX*2
	;	edi = DI register, needs to be incremented by CX*2
	;-------
rep_movsw_cld_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, lr}
	tst		r3, #0x80000000
	bne		movsw_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #17		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
1	ldrb	r0, [r1], #1
	subs	r3, #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	bne		%b1
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		esi, #16
	ror		edi, #16
	add		esi, #0x20000
	add		edi, #0x20000
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.


movsw_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	mov		r3, ecx, lsl #1			; Start with r3 = ECX*2
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #1			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
1	ldrb	r0, [r1], #1
	subs	r3, #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	bne		%b1
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Move the remaining byte to a possibly different page.
	;------
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.
	

; ------------------- F3A5 REP MOVSW (STD) ----------------------------
;
	GLOBAL	rep_movsw_std_next
rep_movsw_std_next
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; First setup the correct segment override flags and effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
rep_movsw_std
	;-------
	; Begin a word move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	add		r0, esi, #1				; r0 = SI+1
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsw_std_from_RAM, rep_movsw_std_from_EGA, bad_string_op_seg_back1
	
	EXTERN	rep_movsw_std_from_EGA
	
rep_movsw_std_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES rep_movsw_std_RAM_RAM, rep_movsw_std_RAM_EGA, bad_string_op_seg_back1
	
	EXTERN	rep_movsw_std_RAM_EGA
	
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI+1 in RAM
	;	r2 = physical start address of ES:DI+1 in RAM
	;	r5 = number of bytes to move, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX*2
	;	r11 = DI register, needs to be decremented by CX*2
	;-------
rep_movsw_std_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		movsw_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	sub		r3, #1					; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
	add		r3, #2					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Move slowly one byte at a time.
	;-------
1	ldrb	r0, [r1], #-1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #-1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0, r3}
	tst		ecx, r3					; Set zero flag if CX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	sub		esi, r3, lsl #17		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #17		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
1	ldrb	r0, [r1], #-1
	subs	r3, #1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	bne		%b1
	;------
	; Then move the extra high byte that will still fit to the same page.
	;------
5	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	mov		r0, esi					; r0 = SI (always points to the low byte of the halfword)
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	mov		r0, edi					; r0 = DI (always points to the low byte of the halfword)
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		esi, #16
	ror		edi, #16
	sub		esi, #0x20000
	sub		edi, #0x20000
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.

movsw_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	mov		r3, ecx, lsl #1			; Start with r3 = ECX*2
	sub		r3, #1					; Start with r3 = 2*ECX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		esi, r3					; Increment ESI by the number of bytes we shall move.
	sub		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	;-------
	; Move slowly one byte at a time.
	;-------
1	ldrb	r0, [r1], #-1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #-1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0, r3}
	tst		ecx, r3					; Set zero flag if CX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	sub		esi, r3, lsl #1			; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3, lsl #1			; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
1	ldrb	r0, [r1], #-1
	subs	r3, #1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	bne		%b1
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Move the remaining byte to a possibly different page.
	;------
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	mov		r0, esi					; r0 = ESI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	mov		r0, edi					; r0 = EDI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	sub		esi, #2
	sub		edi, #2
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.

; ------------------- F3A5 REP MOVSD (CLD) ----------------------------
;
	GLOBAL	rep_movsd_cld_next
rep_movsd_cld_next
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
rep_movsd_cld
	;-------
	; Begin a byte move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	mov		r0, esi					; r0 = SI
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsd_cld_from_RAM, rep_movsd_cld_from_EGA, rep_movsd_cld_from_MODEX
	
	EXTERN	rep_movsd_cld_from_EGA
	EXTERN	rep_movsd_cld_from_MODEX
	
rep_movsd_cld_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_cld_RAM_RAM, rep_movsd_cld_RAM_EGA, rep_movsd_cld_RAM_MODEX
	
	EXTERN	rep_movsd_cld_RAM_EGA
	EXTERN	rep_movsd_cld_RAM_MODEX
	
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of halfwords to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX*2
	;	edi = DI register, needs to be incremented by CX*2
	;-------
rep_movsd_cld_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, lr}
	tst		r3, #0x80000000
	bne		movsd_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #2					; Number of bytes to move = 4*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3					; Are we to move an uneven number of words?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of words remaining
	ror		esi, #16
	ror		edi, #16
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full dwords to move
	beq		%f5						; Jump if no full dwords fit here
	add		esi, r3, lsl #18		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #18		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full dwords we shall move.
	;------
	; First move the full dwords we can move
	;------
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	subs	r3, #1
	strb	r0, [r2], #1
	bne		%b1
	;------
	; Then move the dword that is split between two pages.
	; First move the byte that is in this page.
	;------
5	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	;------
	; Then move the second byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Then move the third byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Finally move the fourth byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #3				; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #3				; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Adjust the registers, one more dword moved.
	;------
	sub		ecx, #1
	ror		esi, #16
	ror		edi, #16
	add		esi, #0x40000
	add		edi, #0x40000
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.


movsd_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	mov		r3, ecx, lsl #2			; Start with r3 = ECX*4
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3					; Are we to move an uneven number of words?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of bytes remaining
	bl		movs_cld_sub			; Go move r3 bytes from r1 to r2
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full dwords to move
	beq		%f5						; Jump if no full dwords fit here
	add		esi, r3, lsl #2			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #2			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full dwords we shall move.
	;------
	; First move the full dwords we can move
	;------
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	ldrb	r0, [r1], #1
	subs	r3, #1
	strb	r0, [r2], #1
	bne		%b1
	;------
	; Then move the dword that is split between two pages.
	; First move the byte that is in this page.
	;------
5	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	pop		{r0, r3, lr}			; pop flags and 16-bit/32-bit mask
	;------
	; Then move the second byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Then move the third byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Finally move the fourth byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #3				; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #3				; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #1
	strb	r0, [r2], #1
	;------
	; Adjust the registers, one more dword moved.
	;------
	sub		ecx, #1
	add		esi, #4
	add		edi, #4
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.

; ------------------- F3A5 REP movsd (STD) ----------------------------
;
	GLOBAL	rep_movsd_std_next
rep_movsd_std_next	
	;-------
	; Continue a move that needs to span several 16K memory blocks.
	; First setup the correct segment override flags and effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
rep_movsd_std
	;-------
	; Begin a word move. We need to save the possible segment override flags so that
	; we can setup the correct effective segment in case we need to redo the move
	; for the next 16K memory block.
	;-------
	add		r0, esi, #3				; r0 = SI+3 (poinst to the highest byte of the first word to move)
	str		r2, [sp, #SP_STR_SEG]	; Save the current effective segment to stack
	mem_handler_jump_r0r3 rep_movsd_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
rep_movsd_std_from_RAM
	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #3				; r0 = DI+3 (poinst to the highest byte of the first word to move)
	mem_handler_jump_r0r3_ES rep_movsd_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI+1 in RAM
	;	r2 = physical start address of ES:DI+1 in RAM
	;	r5 = number of bytes to move, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX*2
	;	r11 = DI register, needs to be decremented by CX*2
	;-------
rep_movsd_std_RAM_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		movsd_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #2					; Number of bytes to move = 4*CX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	sub		r3, #3					; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
	add		r3, #4					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #3					; Are we to move an uneven number of words?
	bne		%f5						; Yes, so handle this situation specially.
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Move slowly one byte at a time.
	;-------
1	ldrb	r0, [r1], #-1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #-1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0, r3}
	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if CX == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_std_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full dwords to move
	beq		%f5						; Jump if no full dwords fit here
	sub		esi, r3, lsl #18		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #18		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full dwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
1	ldrb	r0, [r1], #-1
	subs	r3, #1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	bne		%b1
	;------
	; Then move the dword that is split between two pages.
	; First move the highest byte that is in this page.
	;------
5	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	;------
	; Then move the third byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Then move the second byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Finally move the lowest byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	mov		r0, esi					; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Adjust the registers, one more dword moved.
	;------
	sub		ecx, #1
	ror		esi, #16
	ror		edi, #16
	sub		esi, #0x40000
	sub		edi, #0x40000
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_std_next		; ... else handle the next 16K page.

movsd_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	mov		r3, ecx, lsl #2			; Start with r3 = ECX*4
	sub		r3, #1					; Start with r3 = 4*ECX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		esi, r3					; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of bytes remaining
	;-------
	; Move slowly one byte at a time.
	;-------
1	ldrb	r0, [r1], #-1
	subs	r3, #1					; CX -= 1
	strb	r0, [r2], #-1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0, r3}
	tst		ecx, r3					; Set zero flag if CX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_std_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	sub		esi, r3, lsl #2			; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3, lsl #2			; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full dwords we shall move.
	;------
	; First move the full dwords we can move
	;------
1	ldrb	r0, [r1], #-1
	subs	r3, #1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	bne		%b1
	;------
	; Then move the dword that is split between two pages.
	; First move the highest byte that is in this page.
	;------
5	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	;------
	; Then move the third byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Then move the second byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Finally move the lowest byte, which might be in different pages.
	;------
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	mov		r0, esi					; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1], #-1
	strb	r0, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	sub		esi, #4
	sub		edi, #4
	mrs		r0, cpsr				; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_std_next		; ... else handle the next 16K page.

; ------------------- F3AA REP STOSB ----------------------------------
;
	GLOBAL	rep_stosb_cld
rep_stosb_cld
	;-------
	; r2 = physical address where to store
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES rep_stosb_cld_RAM, rep_stosb_cld_EGA, rep_stosb_cld_MODEX
	
	EXTERN	rep_stosb_cld_EGA
	EXTERN	rep_stosb_cld_MODEX
	
rep_stosb_cld_RAM
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	push	{r0, r3}
	;-------
	; r1 = word value to store
	;-------
	and		r1, eax, #0xFF
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16						; r1 = value to store (in all 4 bytes)
	;-------
	; r3 = number of bytes to store it
	;-------
	cmp		r3, #0
	and		r3, ecx								; r3 = number of bytes to store
	blt		%f32
	;-------
	; 16-bit addressing
	;-------
	ror		edi, #16
	min_cnt_next_page 	r3, r2
	min_cnt_idx_wrap_16 r3, edi
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	add		edi, r3, lsl #16					; Increment DI by the number of bytes we stored.
	ror		edi, #16
	b		%f16
	;-------
	; 32-bit addressing
	;-------
32	min_cnt_next_page 	r3, r2
	add		edi, r3
	;-------
	; Common to both 16-bit and 32-bit addressing
	;-------
16	sub		ecx, r3								; CX = number of bytes remaining
	bl		rep_stos_sub						; Call the common subroutine for REP STOSB/REP STOSW/REP STOSD
	pop		{r0, r3}
	tst		ecx, r3								; Set zero flag if we stored everything we should.
	beq		restore_flags_from_r0				; Go back to the opcode loop if CX == 0 ...
	msr		cpsr_f,r0							; Restore flags
	b		rep_stosb_cld						; ... else handle the next 16K page.

	LTORG								; Dump the current literal pool here

	GLOBAL	rep_stosb_std
rep_stosb_std
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES rep_stosb_std_RAM, rep_stosb_std_EGA, rep_stosb_std_MODEX
	
	EXTERN	rep_stosb_std_EGA
	EXTERN	rep_stosb_std_MODEX
	
rep_stosb_std_RAM
	;-------
	; Registers
	;	r0 = Saved flags (in highest byte), byte to store (in lowest byte)
	;	r1 = loop counter
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = 16-bit / 32-bit mask
	;	r5 = number of bytes to store, needs to be cleared
	;	r11 = DI register, needs to be incremented by CX
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0}
	;-------
	; Calculate the number of safe bytes we can store
	; r1 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	and		r1, ecx, r3
	sub		r1, #1					; Start with r1 = CX, decrease it by one for the page checks.
	min_cnt_prev_page 	r1, r2		; ----- Check with ES:DI bytes in this page
	cmp		r3, #0					; Set the flags based on whether we are using 16-bit or 32-bit addressing.
	rorgt	edi, #16				; Rotate EDI, if 16-bit addressing.
	min_cnt_idx_zero_cond r1, edi	; ----- Check with DI, but only if 16-bit addressing.
	add		r1, #1
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	cmp		r3, #0					; Set the flags based on whether we are using 16-bit or 32-bit addressing.
	subgt	edi, r1, lsl #16		; Decrement DI by the number of bytes we shall move (16-bit addressing)
	sublt	edi, r1					; Decrement DI by the number of bytes we shall move (32-bit addressing)
	rorgt	edi, #16				; Rotate EDI, if 16-bit addressing.
	sub		ecx, r1					; r5 = number of bytes remaining
	;-------
	; Store slowly one byte at a time.
	;-------
1	strb	eax, [r2], #-1
	subs	r1, #1					; CX -= 1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0}					; Restore flags in r0
	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosb_std			; ... else handle the next 16K page.

; ------------------- F3AB REP STOSW ----------------------------------
;
rep_stosw_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES rep_stosw_cld_RAM, rep_stosw_cld_EGA, rep_stosw_cld_MODEX
	
	EXTERN	rep_stosw_cld_EGA
	EXTERN	rep_stosw_cld_MODEX
	
rep_stosw_cld_RAM
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		stosw_cld_USE32
	;-------
	; r1 = word value to store
	;-------
	mov		r1, eax, lsl #16
	orr		r1, r1, lsr #16						; r1 = value to store (in all 4 bytes)
	;-------
	; r3 = number of bytes to store it
	;-------
	and		r3, ecx								; r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #1								; Number of bytes to store = 2*CX
	ror		edi, #16
	min_cnt_next_page 	r3, r2
	min_cnt_idx_wrap_16 r3, edi
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1								; Are we to store an uneven number of bytes?
	bne		%f5									; Yes, so handle this situation specially.
	add		edi, r3, lsl #16					; Increment DI by the number of bytes we stored.
	ror		edi, #16
	sub		ecx, r3, lsr #1						; CX = number of bytes remaining
	bl		rep_stos_sub						; Call the common subroutine for REP STOSB and REP STOSW
	pop		{r0, r3}
2	tst		ecx, r3								; Set zero flag if we stored everything we should.
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0							; Restore flags
	b		rep_stosw_cld						; ... else handle the next 16K page.
	;-------
	; Store an uneven number of bytes, meaning the start address was not aligned.
	;-------
5	movs	r3, r3, lsr #1						; r3 = number of full halfwords to store
	mov		r1, eax, lsr #8						; r1 = AH value
	beq		%f5									; Jump if no full halfwords fit here
	add		edi, r3, lsl #17					; Increment DI by the number of bytes we shall move..
	sub		ecx, r3								; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full halfwords we can move
	;------
1	strb	eax, [r2], #1
	strb	r1, [r2], #1
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AL value that will still fit to the same page.
	;------
5	strb	eax, [r2], #1
	;------
	; Store the AH value to a possibly different page.
	;------
	ror		edi, #16
	pop		{r0, r3}							; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0							; Restore flags
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	r1, [r2], #1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		edi, #16
	add		edi, #0x20000
	ror		edi, #16
	mrs		r0, cpsr							; r0 = flags
	b		%b2

stosw_cld_USE32
	;-------
	; r1 = word value to store
	;-------
	mov		r1, eax, lsl #16
	orr		r1, r1, lsr #16						; r1 = value to store (in all 4 bytes)
	;-------
	; r3 = number of bytes to store it
	;-------
	and		r3, ecx								; r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #1								; Number of bytes to store = 2*CX
	min_cnt_next_page 	r3, r2
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1								; Are we to store an uneven number of bytes?
	bne		%f5									; Yes, so handle this situation specially.
	add		edi, r3
	sub		ecx, r3, lsr #1						; CX = number of bytes remaining
	bl		rep_stos_sub						; Call the common subroutine for REP STOSB and REP STOSW
	pop		{r0, r3}
2	tst		ecx, r3								; Set zero flag if we stored everything we should.
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0							; Restore flags
	b		rep_stosw_cld						; ... else handle the next 16K page.
	;-------
	; Store an uneven number of bytes, meaning the start address was not aligned.
	;-------
5	movs	r3, r3, lsr #1						; r3 = number of full halfwords to store
	mov		r1, eax, lsr #8						; r1 = AH value
	beq		%f5									; Jump if no full halfwords fit here
	add		edi, r3, lsl #1						; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3								; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full halfwords we can move
	;------
1	strb	eax, [r2], #1
	strb	r1, [r2], #1
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AL value that will still fit to the same page.
	;------
5	strb	eax, [r2], #1
	;------
	; Store the AH value to a possibly different page.
	;------
	pop		{r0, r3}							; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0							; Restore flags
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	r1, [r2], #1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		edi, #2
	mrs		r0, cpsr							; r0 = flags
	b		%b2

	LTORG								; Dump the current literal pool here

	GLOBAL	rep_stosw_std
rep_stosw_std
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES rep_stosw_std_RAM, rep_stosw_std_EGA, rep_stosw_std_MODEX
	
	EXTERN	rep_stosw_std_EGA
	EXTERN	rep_stosw_std_MODEX
	
rep_stosw_std_RAM
	;-------
	; On input
	;	r0 = free
	;	r1 = free
	;	r2 = physical start address of ES:DI+1 in RAM
	;	r4 = halfword to store (r4>>16)
	;	r5 = number of BYTES to store, needs to be cleared
	;	r11 = DI register, needs to be incremented by CX
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		stosw_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		edi, #16
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	sub		r3, #1					; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
	add		r3, #2					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move.
	sub		ecx, r3, lsr #1			; CX = number of bytes remaining
	ror		edi, #16
	;-------
	; r1 = word value to store
	;-------
	mov		r1, eax, lsr #8			; r1 = AH value
	;-------
	; Store slowly two bytes by loop, first AH value, then AL value
	;-------
1	strb	r1, [r2], #-1			; Store the AH value
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1					; counter -= 1
	bne		%b1
	;-------
	; All done for this block, return!
	;-------
	pop		{r0, r3}
3	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosw_std			; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	mov		r1, eax, lsr #8			; r1 = AH value
	beq		%f5						; Jump if no full halfwords fit here
	sub		edi, r3, lsl #17		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full halfwords we can store
	;------
1	strb	r1, [r2], #-1
	strb	eax, [r2], #-1
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AH value that will still fit to the same page.
	;------
5	strb	r1, [r2], #-1
	;-------
	; Put AL value to the previous page!
	;-------
	ror		edi, #16
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0				; Restore flags
	mov		r0, edi					; r0 = EDI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		edi, #16
	sub		edi, #0x20000
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	b		%b3

stosw_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move.
	sub		ecx, r3, lsr #1			; CX = number of bytes remaining
	;-------
	; r1 = word value to store
	;-------
	mov		r1, eax, lsr #8			; r1 = AH value
	;-------
	; Store slowly two bytes by loop, first AH value, then AL value
	;-------
1	strb	r1, [r2], #-1			; Store the AH value
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1					; counter -= 1
	bne		%b1
	;-------
	; All done for this block, return!
	;-------
	pop		{r0, r3}
3	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosw_std			; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	mov		r1, eax, lsr #8			; r1 = AH value
	beq		%f5						; Jump if no full halfwords fit here
	sub		edi, r3, lsl #1			; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full halfwords we can store
	;------
1	strb	r1, [r2], #-1
	strb	eax, [r2], #-1
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AH value that will still fit to the same page.
	;------
5	strb	r1, [r2], #-1
	;-------
	; Put AL value to the previous page!
	;-------
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0				; Restore flags
	mov		r0, edi					; r0 = EDI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	sub		edi, #2
	mrs		r0, cpsr				; r0 = flags
	b		%b3

; ------------------- F3AB REP STOSD ----------------------------------
;
rep_stosd_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES rep_stosd_cld_RAM, rep_stosd_cld_EGA, rep_stosd_cld_MODEX
	
	EXTERN	rep_stosd_cld_EGA
	EXTERN	rep_stosd_cld_MODEX
	
rep_stosd_cld_RAM
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		stosd_cld_USE32
	;-------
	; r3 = number of bytes to store it
	;-------
	and		r3, ecx								; r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #2								; Number of bytes to store = 4*CX
	;-------
	; 16-bit addressing
	;-------
	ror		edi, #16
	min_cnt_next_page 	r3, r2
	min_cnt_idx_wrap_16 r3, edi
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3								; Are we to store an uneven number of dwords?
	bne		%f5									; Yes, so handle this situation specially.
	add		edi, r3, lsl #16					; Increment DI by the number of bytes we stored.
	ror		edi, #16
	b		%f16
	;-------
	; 32-bit addressing
	;-------
32	min_cnt_next_page 	r3, r2
	add		edi, r3
	;-------
	; Common to both 16-bit and 32-bit addressing
	;-------
16	sub		ecx, r3, lsr #2						; CX = number of bytes remaining
	mov		r1, eax								; r1 = value to store (in all 4 bytes)
	bl		rep_stos_sub						; Call the common subroutine for REP STOSB and REP stosd
	pop		{r0, r3}
2	tst		ecx, r3								; Set zero flag if we stored everything we should.
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0							; Restore flags
	b		rep_stosd_cld						; ... else handle the next 16K page.
	;-------
	; Store an uneven number of bytes, meaning the start address was not aligned.
	;-------
5	movs	r3, r3, lsr #2						; r3 = number of full dwords to store
	beq		%f5									; Jump if no full halfwords fit here
	add		edi, r3, lsl #18					; Increment DI by the number of bytes we shall move..
	sub		ecx, r3								; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full dwords we can move
	;------
1	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AL value that will still fit to the same page.
	;------
5	strb	eax, [r2], #1
	ror		eax, #8
	ror		edi, #16
	pop		{r0, r3}							; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0							; Restore flags
	;------
	; Store the remaining bytes  to a possibly different page.
	;------
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		edi, #16
	add		edi, #0x40000
	ror		edi, #16
	mrs		r0, cpsr							; r0 = flags
	b		%b2

stosd_cld_USE32
	;-------
	; r3 = number of bytes to store it
	;-------
	and		r3, ecx								; r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #2								; Number of bytes to store = 4*CX
	min_cnt_next_page 	r3, r2
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3								; Are we to store an uneven number of dwords?
	bne		%f5									; Yes, so handle this situation specially.
	add		edi, r3								; Increment DI by the number of bytes we stored.
	sub		ecx, r3, lsr #2						; CX = number of bytes remaining
	mov		r1, eax								; r1 = value to store (in all 4 bytes)
	bl		rep_stos_sub						; Call the common subroutine for REP STOSB and REP stosd
	pop		{r0, r3}
2	tst		ecx, r3								; Set zero flag if we stored everything we should.
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0							; Restore flags
	b		rep_stosd_cld						; ... else handle the next 16K page.
	;-------
	; Store an uneven number of bytes, meaning the start address was not aligned.
	;-------
5	movs	r3, r3, lsr #2						; r3 = number of full dwords to store
	beq		%f5									; Jump if no full halfwords fit here
	add		edi, r3, lsl #2						; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3								; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full dwords we can move
	;------
1	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	strb	eax, [r2], #1
	ror		eax, #8
	subs	r3, #1
	bne		%b1
	;------
	; Then store the AL value that will still fit to the same page.
	;------
5	strb	eax, [r2], #1
	ror		eax, #8
	pop		{r0, r3}							; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0							; Restore flags
	;------
	; Store the remaining bytes  to a possibly different page.
	;------
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	strb	eax, [r2], #1
	ror		eax, #8
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		edi, #4
	mrs		r0, cpsr							; r0 = flags
	b		%b2

	LTORG								; Dump the current literal pool here

rep_stosd_std
	add		r0, edi, #3				; r0 = DI+3
	mem_handler_jump_r0r3_ES rep_stosd_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
rep_stosd_std_RAM
	;-------
	; On input
	;	r0 = free
	;	r1 = free
	;	r2 = physical start address of ES:DI+1 in RAM
	;	r4 = halfword to store (r4>>16)
	;	r5 = number of BYTES to store, needs to be cleared
	;	r11 = DI register, needs to be incremented by CX
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	tst		r3, #0x80000000
	bne		stosd_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #2					; Number of bytes to move = 4*CX
	ror		edi, #16
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	sub		r3, #3					; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
	add		r3, #4					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #3					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move.
	sub		ecx, r3, lsr #2			; CX = number of bytes remaining
	ror		edi, #16
	;-------
	; Store slowly the EAX value
	;-------
1	ror		eax, #24
	strb	eax, [r2], #-1			; Store the highest byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the third byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AH value
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1					; counter -= 1
	bne		%b1
	;-------
	; All done for this block, return!
	;-------
	pop		{r0, r3}
3	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosd_std			; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	sub		edi, r3, lsl #18		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full dwords we can store
	;------
1	ror		eax, #24
	strb	eax, [r2], #-1			; Store the highest byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the third byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AH value
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1
	bne		%b1
	;------
	; Then store the highest byte that will still fit to the same page.
	;------
5	ror		eax, #24
	strb	eax, [r2], #-1
	ror		edi, #16
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0				; Restore flags
	;-------
	; Put the other bytes possibly to the previous page!
	;-------
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	ror		edi, #16
	sub		edi, #0x40000
	ror		edi, #16
	mrs		r0, cpsr				; r0 = flags
	b		%b3

stosd_std_USE32
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #2					; Number of bytes to move = 4*CX
	sub		r3, #1					; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes in this page
	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	tst		r3, #3					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move.
	sub		ecx, r3, lsr #2			; CX = number of bytes remaining
	;-------
	; Store slowly the EAX value
	;-------
1	ror		eax, #24
	strb	eax, [r2], #-1			; Store the highest byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the third byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AH value
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1					; counter -= 1
	bne		%b1
	;-------
	; All done for this block, return!
	;-------
	pop		{r0, r3}
3	tst		ecx, r3
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosd_std			; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	sub		edi, r3, lsl #2			; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First store the full dwords we can store
	;------
1	ror		eax, #24
	strb	eax, [r2], #-1			; Store the highest byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the third byte
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AH value
	ror		eax, #24
	strb	eax, [r2], #-1			; Store the AL value
	subs	r3, #1
	bne		%b1
	;------
	; Then store the highest byte that will still fit to the same page.
	;------
5	ror		eax, #24
	strb	eax, [r2], #-1
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	msr		cpsr_f,r0				; Restore flags
	;-------
	; Put the other bytes possibly to the previous page!
	;-------
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ror		eax, #24
	strb	eax, [r2], #-1
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	sub		edi, #4
	mrs		r0, cpsr				; r0 = flags
	b		%b3


; ------------------- REP STOSB / REP STOSW subroutine ----------------
;

	;-------
	; Common subroutine for REP STOSB and REP STOSW
	; On input
	;	r0  = free
	;	r1  = word value to store
	;	r2  = starting physical address for the store
	;	r3  = number of bytes before the end of the 16K page
	;	r5  = number of total bytes to store
	;	r11 = DI value
	;	lr 	= return address
	;	stack = {r0, r3}
	;-------
rep_stos_sub
	;-------
	; Store leading byte, if any
	;-------
	tst		r2, #1					; Byte or halfword aligned?
	strbne	r1, [r2], #1			; Store the first byte
	subne	r3, #1					; CX -= 1, first byte handled
	rorne	r1, #8					; Rotate the AX value, now AH = low and AL = high byte
	;-------
	; Store leading halfword, if any
	;-------
	cmp		r3, #1
	beq		stos_last_byte			; Only one more byte to do
	bxlt	lr						; We just did the last byte, all done!
	tst		r2, #2
	strhne	r1, [r2], #2
	subne	r3, #2					; CX -= 2
	rorne	r1, #16					; Rotate the EAX value
	;-------
	; Now the input address is word-aligned.
	; Registers here
	;	r0 = free
	;	r1 = word value to store
	;	r2 = physical ES:DI address to start storing to (word aligned!)
	;	r3 = number of BYTEs to store
	;	lr = return address
	;	stack = {r0, r3, lr}
	;-------
	GLOBAL	stos_word_align			; Called also from "MODEX.s"!
stos_word_align
	mov		r0, r3, lsr #2			; r0 = number of words to store
	cmp		r0, #8					; More than 8 words to store?
	bgt		stos_block				; More than 8 words to store, use the block code
	strge	r1, [r2], #4			; r0 = 0: Store 8 words
	cmp		r0, #7
	strge	r1, [r2], #4			; r0 = 1: Store 7 words
	cmp		r0, #6
	strge	r1, [r2], #4			; r0 = 2: Store 6 words
	cmp		r0, #5
	strge	r1, [r2], #4			; r0 = 3: Store 5 words
	cmp		r0, #4
	strge	r1, [r2], #4			; r0 = 4: Store 4 words
	cmp		r0, #3
	strge	r1, [r2], #4			; r0 = 5: Store 3 words
	cmp		r0, #2
	strge	r1, [r2], #4			; r0 = 6: Store 2 words
	cmp		r0, #1
	strge	r1, [r2], #4			; r0 = 7: Store 1 words
	;-------
	; Store an extra halfword, if needed
	;-------
	tst		r3, #2					; Still a halfword to store?
	strhne	r1, [r2], #2			; Yep, store the last halfword
	rorne	r1, #16					; Rotate the EAX value
stos_last_byte
	;-------
	; Store the last byte, if needed
	;-------
	tst		r3, #1					; Still a byte to store?
	strbne	r1, [r2]				; Store the last byte (AH value)
	bx		lr
	;-------
	; Use a block fill
	;-------
stos_block
	push	{r4-r11}
	mov		r4, r1
	mov		r5, r1
	mov		r6, r1
	mov		r7, r1
	mov		r8, r1
	mov		r9, r1
	mov		r10, r1
	mov		r11, r1
1	mov		r0, r3, lsr #5			; r0 = number of 32-byte blocks to store
	sub		r3, #(8*32)
	cmp		r0, #8					; More than 8 words to store?
    stmiage r2!, {r4-r11}			; 0 = 8
	cmp		r0, #7
    stmiage r2!, {r4-r11}			; 1 = 7
	cmp		r0, #6
    stmiage r2!, {r4-r11}			; 2 = 6
	cmp		r0, #5
    stmiage r2!, {r4-r11}			; 3 = 5
	cmp		r0, #4
    stmiage r2!, {r4-r11}			; 4 = 4
	cmp		r0, #3
    stmiage r2!, {r4-r11}			; 5 = 3
	cmp		r0, #2
    stmiage r2!, {r4-r11}			; 6 = 2
	cmp		r0, #1
    stmiage r2!, {r4-r11}			; 7 = 1
	cmp		r3, #32
	bge		%b1
	pop		{r4-r11}
	ands	r3, #31
	bne		stos_word_align
	bx		lr

	LTORG								; Dump the current literal pool here

; ------------------- F3 A6 = REPE CMPSB ------------------------------
; On input
;	r0 = free
;	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repe_cmpsb_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsb_cld
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsb_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsb_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_cmpsb_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r5 = number of bytes to compare, needs to be cleared
	;	r10 = SI register, needs to be incremented by CX
	;	r11 = DI register, needs to be incremented by CX
	;-------
repe_cmpsb_cld_RAM_RAM
	push	{r3, r4}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsb_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI
	add		esi, #0x00010000					; Increment SI
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
	pop		{r3, r4}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4}							; Restore r3 (16-bit/32-bit mask) and r4 (EAX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_cmpsb_cld_next					; ... and go handle the next 16K page.

pop_r3r4_restore_flags_r0
	pop		{r3, r4}
	b		restore_flags_from_r0

repe_cmpsb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:ESI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:EDI
	add		esi, #1								; Increment ESI
	add		edi, #1								; Increment EDI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
	pop		{r3, r4}
	b		complement_carry					; We need to swap the Carry flag!


repe_cmpsb_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsb_std
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsb_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsb_std_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_cmpsb_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r5 = number of bytes to compare, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX
	;	r11 = DI register, needs to be decremented by CX
	;-------
repe_cmpsb_std_RAM_RAM
	push	{r3, r4}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsb_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	subs	r3, #1								; Start with r3 = CX, decrease it by one for the page checks.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
1	add		r3, #1								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%f5									; Go check whether we need to exit the iteration or handle previous 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI
	sub		esi, #0x00010000					; Decrement SI
	sub		edi, #0x00010000					; Decrement DI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
	pop		{r3, r4}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4}							; Restore r3 (16-bit/32-bit mask) and r4 (EAX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_cmpsb_std_next					; ... and go handle the next 16K page.

repe_cmpsb_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:ESI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:EDI
	sub		esi, #1								; Decrement ESI
	sub		edi, #1								; Decrement EDI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
	pop		{r3, r4}
	b		complement_carry					; We need to swap the Carry flag!

; ------------------- F2 A6 = REPNE CMPSB -----------------------------
; Input
; 	r0 = saved flags
;	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repne_cmpsb_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsb_cld
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsb_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsb_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_cmpsb_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r5 = number of bytes to compare, needs to be cleared
	;	r10 = SI register, needs to be incremented by CX
	;	r11 = DI register, needs to be incremented by CX
	;-------
repne_cmpsb_cld_RAM_RAM
	push	{r3, r4}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsb_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI
	add		esi, #0x00010000					; Increment SI
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	pop		{r3, r4}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4}							; Restore r3 (16-bit/32-bit mask) and r4 (EAX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0 ...
	bne		repne_cmpsb_cld_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.

repne_cmpsb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:ESI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:EDI
	add		esi, #1								; Increment ESI
	add		edi, #1								; Increment EDI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	pop		{r3, r4}
	b		complement_carry					; We need to swap the Carry flag!


repne_cmpsb_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsb_std
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsb_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsb_std_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_cmpsb_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in RAM
	;	r5 = number of bytes to compare, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX
	;	r11 = DI register, needs to be decremented by CX
	;-------
repne_cmpsb_std_RAM_RAM
	push	{r3, r4}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsb_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	subs	r3, #1								; Start with r3 = CX, decrease it by one for the page checks.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
1	add		r3, #1								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%f5									; Go check whether we need to exit the iteration or handle previous 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI
	sub		esi, #0x00010000					; Decrement SI
	sub		edi, #0x00010000					; Decrement DI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	pop		{r3, r4}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4}							; Restore r3 (16-bit/32-bit mask) and r4 (EAX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0 ...
	bne		repne_cmpsb_std_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.

repne_cmpsb_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4_restore_flags_r0			; If CX == 0 at the beginning, exit immediately.
	cmp		r3, #1								; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r3, r3								; Zero flag set if r3 == 0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:ESI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:EDI
	sub		esi, #1								; Decrement ESI
	sub		edi, #1								; Decrement EDI
	lsl		r0, #24
	cmp		r0, r4, lsl #24						; Compare the bytes
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #1								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	pop		{r3, r4}
	b		complement_carry					; We need to swap the Carry flag!

; ------------------- F3 A7 = REPE CMPSW ------------------------------
; On input
;	r0 = free
;	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repe_cmpsw_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsw_cld
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsw_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsw_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_cmpsw_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsw_cld_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsw_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #0x00020000					; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags (not actually needed) ...
	b		repe_cmpsw_cld_next					; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI low byte
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #0x00020000					; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:DI low byte
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repe_cmpsw_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #2								; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #2								; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:ESI low byte
	ldrb  	r4, [r1], #1						; Get byte from physical DS:ESI+1
	add		esi, #2								; Increment ESI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:ESI
	ldrb  	r6, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:EDI low byte
	ldrb  	r4, [r2], #1						; Get byte from physical ES:EDI+1
	add		edi, #2								; Increment EDI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:EDI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

	
pop_r3r4r6_restore_flags_r0
	pop		{r3, r4, r6}
	b		restore_flags_from_r0


repe_cmpsw_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsw_std
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	add		r0, esi, #1							; r0 = SI+1
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsw_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsw_std_from_RAM
	mov		r1, r2								; r1 = DS:SI+1 linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES repe_cmpsw_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsw_std_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsw_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	sub		r3, #1								; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
	add		r3, #2								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+1
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	sub		esi, #0x00020000					; Decrement SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Decrement DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags (not actually needed) ...
	b		repe_cmpsw_std_next					; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI+1 to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI+1 low byte
	ldrb  	r0, [r1]							; Get byte from physical DS:SI
	sub		esi, #0x00020000					; Decrement SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:DI+1 low byte
	ldrb  	r6, [r2]							; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Decrement DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repe_cmpsw_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	add		r3, #1								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+1
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	sub		esi, #2								; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #2								; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI+1 to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:ESI+1 low byte
	ldrb  	r0, [r1]							; Get byte from physical DS:ESI
	sub		esi, #2								; Decrement ESI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:ESI
	ldrb  	r4, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:EDI+1 low byte
	ldrb  	r6, [r2]							; Get byte from physical ES:EDI
	sub		edi, #2								; Decrement EDI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:EDI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F2 A7 = REPNE CMPSW -----------------------------
; On input
; Input
; 	r0 = saved flags
; 	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repne_cmpsw_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsw_cld
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsw_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsw_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_cmpsw_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsw_cld_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsw_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #0x00020000					; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	bne		repne_cmpsw_cld_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI low byte
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #0x00020000					; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:DI low byte
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repne_cmpsw_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	add		esi, #2								; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #2								; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:ESI low byte
	ldrb  	r4, [r1], #1						; Get byte from physical DS:ESI+1
	add		esi, #2								; Increment ESI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:ESI
	ldrb  	r6, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:EDI low byte
	ldrb  	r4, [r2], #1						; Get byte from physical ES:EDI+1
	add		edi, #2								; Increment EDI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:EDI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

repne_cmpsw_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsw_std
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	add		r0, esi, #1							; r0 = SI+1
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsw_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsw_std_from_RAM
	mov		r1, r2								; r1 = DS:SI+1 linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES repne_cmpsw_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsw_std_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsw_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	sub		r3, #1								; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
	add		r3, #2								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+1
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	sub		esi, #0x00020000					; Decrement SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Decrement DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	bne		repne_cmpsw_std_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI+1 to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI+1 low byte
	ldrb  	r0, [r1]							; Get byte from physical DS:SI
	sub		esi, #0x00020000					; Decrement SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:DI+1 low byte
	ldrb  	r6, [r2]							; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Decrement DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repne_cmpsw_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #1								; Check with 2*ECX
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	add		r3, #1								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	beq		%f7									; r3 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+1
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	sub		esi, #2								; Increment SI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #2								; Increment DI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:DI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #2								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from ES:DI+1 to stack
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r4, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:ESI+1 low byte
	ldrb  	r0, [r1]							; Get byte from physical DS:ESI
	sub		esi, #2								; Decrement ESI
	orr		r0, r4, lsl #8						; r0 = halfword from DS:ESI
	ldrb  	r4, [sp, #(3*4)+2+SP_FREE2]			; Get the saved ES:EDI+1 low byte
	ldrb  	r6, [r2]							; Get byte from physical ES:EDI
	sub		edi, #2								; Decrement EDI
	lsl		r0, #16
	orr		r6, r4, lsl #8						; r3 = halfword from ES:EDI
	cmp		r0, r6, lsl #16						; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F3 A7 = REPE CMPSD ------------------------------
; On input
;	r0 = free
;	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repe_cmpsd_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsd_cld
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsd_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsd_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_cmpsd_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsd_cld_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsd_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3
	beq		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	ldrb  	r6, [r1], #1						; Get byte from physical DS:SI+2
	orr		r0, r4, lsl #8
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+3
	orr		r0, r6, lsl #16
	orr		r0, r4, lsl #24						; r0 = dword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		esi, #0x00040000					; Increment SI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+2
	add		edi, #0x00040000					; Increment DI
	orr		r6, r4, lsl #16
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+3
	orr		r6, r4, lsl #24						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags (not actually needed) ...
	b		repe_cmpsd_cld_next					; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+2 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+2 to stack
	add		r0, esi, #3							; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+3 linear address
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI+3 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI+3 to stack
	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	add		esi, #0x00040000					; Increment SI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	add		edi, #0x00040000					; Increment DI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repe_cmpsd_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	tst		r3, r3
	beq		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	ldrb  	r6, [r1], #1						; Get byte from physical DS:SI+2
	orr		r0, r4, lsl #8
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+3
	orr		r0, r6, lsl #16
	orr		r0, r4, lsl #24						; r0 = dword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		esi, #4								; Increment ESI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+2
	add		edi, #4								; Increment EDI
	orr		r6, r4, lsl #16
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+3
	orr		r6, r4, lsl #24						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+2 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+2 to stack
	add		r0, esi, #3							; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+3 linear address
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI+3 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI+3 to stack
	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	add		esi, #4								; Increment ESI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	add		edi, #4								; Increment EDI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

repe_cmpsd_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repe_cmpsd_std
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	add		r0, esi, #3							; r0 = SI+3
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repe_cmpsd_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsd_std_from_RAM
	mov		r1, r2								; r1 = DS:SI+3 linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES repe_cmpsd_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_cmpsd_std_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_cmpsd_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	sub		r3, #3								; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
	add		r3, #4								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3
	beq		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI+3
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+2
	ldrb  	r6, [r1], #-1						; Get byte from physical DS:SI+1
	orr		r4, r0, lsl #8
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	orr		r6, r4, lsl #8
	orr		r0, r6, lsl #8						; r0 = dword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI+2
	sub		esi, #0x00040000					; Decrement SI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	sub		edi, #0x00040000					; Decrement DI
	orr		r4, r6, lsl #8
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	orr		r6, r4, lsl #8						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags (not actually needed) ...
	b		repe_cmpsd_std_next					; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	;-------
	; Get and save bytes from DS:SI[3] and ES:DI[3]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI[3] to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI[3] to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	sub		esi, #0x00040000					; Increment SI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	sub		edi, #0x00040000					; Increment DI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repe_cmpsd_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	add		r3, #1								; Restore the count
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	tst		r3, r3
	beq		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI+3
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+2
	ldrb  	r6, [r1], #-1						; Get byte from physical DS:SI+1
	orr		r4, r0, lsl #8
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	orr		r6, r4, lsl #8
	orr		r0, r6, lsl #8						; r0 = dword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI+2
	sub		esi, #4								; Decrement ESI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	sub		edi, #4								; Decrement EDI
	orr		r4, r6, lsl #8
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	orr		r6, r4, lsl #8						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	;-------
	; Get and save bytes from DS:SI[3] and ES:DI[3]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI[3] to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI[3] to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	sub		esi, #4								; Decrement ESI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	sub		edi, #4								; Decrement EDI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was not equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F3 A7 = REPNE CMPSD -----------------------------
; On input
;	r0 = free
;	r1 = free
;	r2 = current effective segment
;	r3 = 16-bit/32-bit mask
;
repne_cmpsd_cld_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsd_cld
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	mov		r0, esi								; r0 = SI
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsd_cld_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsd_cld_from_RAM
	mov		r1, r2								; r1 = DS:SI linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_cmpsd_cld_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsd_cld_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsd_cld_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi					; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3
	beq		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	ldrb  	r6, [r1], #1						; Get byte from physical DS:SI+2
	orr		r0, r4, lsl #8
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+3
	orr		r0, r6, lsl #16
	orr		r0, r4, lsl #24						; r0 = dword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		esi, #0x00040000					; Increment SI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+2
	add		edi, #0x00040000					; Increment DI
	orr		r6, r4, lsl #16
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+3
	orr		r6, r4, lsl #24						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	bne		repne_cmpsd_cld_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+2 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+2 to stack
	add		r0, esi, #3							; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+3 linear address
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI+3 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI+3 to stack
	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	add		esi, #0x00040000					; Increment SI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	add		edi, #0x00040000					; Increment DI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repne_cmpsd_cld_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	min_cnt_next_page 	r3, r1					; ----- Check with DS:SI bytes before page end
	min_cnt_next_page 	r3, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	tst		r3, r3
	beq		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #1						; Get byte from physical DS:SI
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+1
	ldrb  	r6, [r1], #1						; Get byte from physical DS:SI+2
	orr		r0, r4, lsl #8
	ldrb  	r4, [r1], #1						; Get byte from physical DS:SI+3
	orr		r0, r6, lsl #16
	orr		r0, r4, lsl #24						; r0 = dword from DS:SI
	ldrb  	r6, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+1
	add		esi, #4								; Increment ESI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+2
	add		edi, #4								; Increment EDI
	orr		r6, r4, lsl #16
	ldrb  	r4, [r2], #1						; Get byte from physical ES:DI+3
	orr		r6, r4, lsl #24						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+2 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+2 to stack
	add		r0, esi, #3							; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+3 linear address
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI+3 to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI+3 to stack
	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	add		esi, #4								; Increment ESI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	add		edi, #4								; Increment EDI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

repne_cmpsd_std_next
	;-------
	; Continue an operation that needs to span several 16K memory blocks.
	; Setup the correct effective segment.
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
repne_cmpsd_std
	msr		cpsr_f,r0							; Restore flags from r0
	;-------
	; Determine the DS:SI memory access mode.
	;-------
	add		r0, esi, #3							; r0 = SI+3
	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 repne_cmpsd_std_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsd_std_from_RAM
	mov		r1, r2								; r1 = DS:SI+3 linear address
	;-------
	; Determine the ES:DI memory access mode.
	;-------
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES repne_cmpsd_std_RAM_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_cmpsd_std_RAM_RAM
	push	{r3, r4, r6}
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_cmpsd_std_USE32				; Yep, go handle it.
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	ror		esi, #16
	ror		edi, #16
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	sub		r3, #3								; Decrease count by one halfword for the index register checks.
	min_cnt_idx_zero_16 r3, esi					; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi					; ----- Check with DI
	add		r3, #4								; Restore the counter
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		r3, r3
	beq		%f5									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI+3
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+2
	ldrb  	r6, [r1], #-1						; Get byte from physical DS:SI+1
	orr		r4, r0, lsl #8
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	orr		r6, r4, lsl #8
	orr		r0, r6, lsl #8						; r0 = dword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI+2
	sub		esi, #0x00040000					; Decrement SI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	sub		edi, #0x00040000					; Decrement DI
	orr		r4, r6, lsl #8
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	orr		r6, r4, lsl #8						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r3 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		esi, #16							; Restore ESI
	ror		edi, #16							; Restore EDI
6	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	bne		repne_cmpsd_std_next				; ... else go handle the next 16K page.
	eor		r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	b		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	ror		esi, #16
	ror		edi, #16
	;-------
	; Get and save bytes from DS:SI[3] and ES:DI[3]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI[3] to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI[3] to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	push	{r3, r4, r6}
	ror		esi, #16
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	sub		esi, #0x00040000					; Increment SI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	sub		edi, #0x00040000					; Increment DI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.

repne_cmpsd_std_USE32
	;-------
	; Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	ands	r3, ecx								; Start with r3 = CX
	beq		pop_r3r4r6_restore_flags_r0		; If CX == 0 at the beginning, exit immediately.
	lsl		r3, #2								; Check with 4*ECX
	sub		r3, #1								; Start with r3 = CX, decrease it by one byte for the page checks.
	min_cnt_prev_page 	r3, r1					; ----- Check with DS:SI bytes in this page
	min_cnt_prev_page 	r3, r2					; ----- Check with ES:DI bytes in this page
	add		r3, #1								; Restore the count
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	cmp		r3, #1
	tst		r3, r3
	beq		%b6									; r3 == 0 => Go check whether we need to exit the iteration or handle next 16K block
	cmp		r3, #4
	blt		%f7									; r3 == 1..3 => Less than a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI+3
	ldrb  	r4, [r1], #-1						; Get byte from physical DS:SI+2
	ldrb  	r6, [r1], #-1						; Get byte from physical DS:SI+1
	orr		r4, r0, lsl #8
	ldrb  	r0, [r1], #-1						; Get byte from physical DS:SI
	orr		r6, r4, lsl #8
	orr		r0, r6, lsl #8						; r0 = dword from DS:SI
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI+2
	sub		esi, #4								; Decrement ESI
	orr		r6, r4, lsl #8
	ldrb  	r4, [r2], #-1						; Get byte from physical ES:DI+1
	sub		edi, #4								; Decrement EDI
	orr		r4, r6, lsl #8
	ldrb  	r6, [r2], #-1						; Get byte from physical ES:DI
	orr		r6, r4, lsl #8						; r6 = dword from ES:DI
	cmp		r0, r6								; Compare the dwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r3, #4								; Decrement 16K block internal counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3, r4, r6}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3, r4, r6}						; Restore r3 (16-bit/32-bit mask), r4 (EAX) and r6 (EDX)
	;-------
	; Get and save bytes from DS:SI[3] and ES:DI[3]
	;-------
	ldrb	r0, [r1]
	strb	r0, [sp, #3+SP_FREE2]				; Save the byte from DS:SI[3] to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[2] and ES:DI[2]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #3+SP_FREE3]				; Save the byte from ES:DI[3] to stack
	add		r0, esi, #2							; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+2 linear address
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #2+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[1] and ES:DI[1]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #2+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	add		r0, esi, #1							; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI+1 linear address
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #1+SP_FREE2]				; Save the byte from DS:SI+1 to stack
	ldrb	r0, [r2]
	;-------
	; Get and save bytes from DS:SI[0] and ES:DI[0]
	;-------
	ldr		r2, [sp, #SP_STR_SEG]				; Get the current effective segment from stack
	strb	r0, [sp, #1+SP_FREE3]				; Save the byte from ES:DI+1 to stack
	mov		r0, esi								; r0 = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2								; r1 = DS:SI linear address
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r1]
	strb	r0, [sp, #SP_FREE2]					; Save the byte from DS:SI to stack
	ldrb	r0, [r2]
	strb	r0, [sp, #SP_FREE3]					; Save the byte from ES:DI to stack
	push	{r3, r4, r6}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldr  	r0, [sp, #(3*4)+SP_FREE2]			; Get the saved DS:SI dword
	sub		esi, #4								; Decrement ESI
	ldr  	r6, [sp, #(3*4)+SP_FREE3]			; Get the saved ES:DI dword
	sub		edi, #4								; Decrement EDI
	cmp		r0, r6								; Compare the halfwords
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F3 AC = REP LODSB -----------------------------
;	Works with both 16-bit and 32-bit memory addressing!
;  Used in "MISC_256" and "PCDIZZY"
;
rep_lodsb_cld
rep_lodsb_std
	;-------
	; Save flags, and test if CX == 0 (meaning we have nothing to do).
	;-------
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		ecx, r3
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	ldr		r1, [sp, #SP_FLAGS]					; Get flags (for direction flag checking)
	and		r1, #FLAG_DF						; Get the direction flag (1<<10)
	;-------
	; First, loop spending some time here so that we find the final ESI value where we load the AL byte.
	;-------
	cmp		r3, #0
	blt		%f2
	;-------
	; 16-bit loop
	;-------
	lsl		r1, #7								; Put FLAG_DF into (1<<17) position
	rsb		r1, #0x00010000						; Now r1 = (-1 or 1)<<16
	ror		esi, #16
16	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b16
	msr		cpsr_f,r0							; Restore flags
	sub		r0, esi, r1
	ror		esi, #16
	ror		r0, #16								; r0 = SI-1 or SI+1
	b		%f1
	;-------
	; 32-bit loop
	;-------
2	lsr		r1, #9								; Put FLAG_DF into (1<<1) position
	rsb		r1, #1								; Now r1 = (-1 or 1)
32	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b32
	msr		cpsr_f,r0							; Restore flags
	sub		r0, esi, r1							; r0 = SI-1 or SI+1
	;-------
	; Determine the DS:SI memory access mode.
	;-------
1	mem_handler_jump_r0r3 rep_lodsb_from_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
rep_lodsb_from_RAM
	ldrb	r0, [r2]							; r0 = the last byte we supposedly loaded
	bic		eax, #0xFF
	orr		eax, r0								; AL = most recent byte loaded
	b		loop

; ------------------- F3 AD = REP LODSW -----------------------------
;	Works with both 16-bit and 32-bit memory addressing!
rep_lodsw_cld
rep_lodsw_std
	;-------
	; Save flags, and test if CX == 0 (meaning we have nothing to do).
	;-------
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	tst		ecx, r3
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	ldr		r1, [sp, #SP_FLAGS]					; Get flags (for direction flag checking)
	and		r1, #FLAG_DF						; Get the direction flag (1<<10)
	;-------
	; First, loop spending some time here so that we find the final ESI value where we load the AX value.
	;-------
	cmp		r3, #0
	blt		%f2
	;-------
	; 16-bit loop
	;-------
	lsl		r1, #8								; Put FLAG_DF into (1<<18) position
	rsb		r1, #0x00020000						; Now r1 = (-2 or 2)<<16
	ror		esi, #16
16	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b16
	msr		cpsr_f,r0							; Restore flags
	sub		r0, esi, r1
	add		r1, r0, #0x00010000					; r1 = high byte address
	ror		esi, #16
	ror		r0, #16								; r0 = SI-2 or SI+2
	ror		r1, #16								; r1 = SI-1 or SI+3
	b		%f1
	;-------
	; 32-bit loop
	;-------
2	lsr		r1, #8								; Put FLAG_DF into (1<<2) position
	rsb		r1, #2								; Now r1 = (-2 or 2)
32	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b32
	msr		cpsr_f,r0							; Restore flags
	sub		r0, esi, r1							; r0 = SI-2 or SI+2
	add		r1, r0, #1							; r1 = SI-1 or SI+3
	;-------
	; Determine the DS:SI memory access mode.
	;-------
1	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r0, r1								; r0 = offset of the AH value
	ldrb	r1, [r2]							; r1 = AL value
	ldr		r2, [sp, #SP_STR_SEG]				; r2 = current effective segment
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r2]							; r0 = AH value
	bic		eax, #0xFF
	bic		eax, #0xFF00
	orr		eax, r1								; AL = low byte loaded
	orr		eax, r0, lsl #8						; AH = high byte loaded
	b		loop

; ------------------- F3 AD = REP LODSD -----------------------------
;	Works with both 16-bit and 32-bit memory addressing!
rep_lodsd_cld
rep_lodsd_std
	;-------
	; Save flags, and test if CX == 0 (meaning we have nothing to do).
	;-------
	mrs		r0,cpsr								; Save flags
	tst		ecx, r3
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	ldr		r1, [sp, #SP_FLAGS]					; Get flags (for direction flag checking)
	and		r1, #FLAG_DF						; Get the direction flag (1<<10)
	;-------
	; First, loop spending some time here so that we find the final ESI value where we load the AX value.
	;-------
	cmp		r3, #0
	blt		%f2
	;-------
	; 16-bit loop
	;-------
	lsl		r1, #9								; Put FLAG_DF into (1<<19) position
	rsb		r1, #0x00040000						; Now r1 = (-4 or 4)<<16
	ror		esi, #16
16	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b16
	msr		cpsr_f,r0							; Restore flags
	sub		r1, esi, r1
	ror		esi, #16
	ror		r1, #16								; r1 = SI-2 or SI+2
	b		%f1
	;-------
	; 32-bit loop
	;-------
2	lsr		r1, #7								; Put FLAG_DF into (1<<3) position
	rsb		r1, #4								; Now r1 = (-4 or 4)
32	add		esi, r1
	sub		ecx, #1
	tst		ecx, r3
	bne		%b32
	msr		cpsr_f,r0							; Restore flags
	sub		r1, esi, r1							; r1 = SI-2 or SI+2
	;-------
	; Determine the DS:SI memory access mode.
	;-------
1	str		r2, [sp, #SP_STR_SEG]				; Save the current effective segment to stack
	mov		r0, r1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	eax, [r2]							; AL value
	mrs		r2,cpsr								; Save flags
	cmp		r3, #0
	addlt	r1, #1
	rorgt	r1, #16
	addgt	r1, #0x00010000
	rorgt	r1, #16
	msr		cpsr_f,r2							; Restore flags
	mov		r0, r1								; r0 = offset of the AH value
	ldr		r2, [sp, #SP_STR_SEG]				; r2 = current effective segment
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r2]							; r0 = AH value
	mrs		r2,cpsr								; Save flags
	orr		eax, r0, lsl #8
	cmp		r3, #0
	addlt	r1, #1
	rorgt	r1, #16
	addgt	r1, #0x00010000
	rorgt	r1, #16
	msr		cpsr_f,r2							; Restore flags
	mov		r0, r1								; r0 = offset of the AH value
	ldr		r2, [sp, #SP_STR_SEG]				; r2 = current effective segment
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r2]							; r0 = third byte value
	mrs		r2,cpsr								; Save flags
	orr		eax, r0, lsl #16
	cmp		r3, #0
	addlt	r1, #1
	rorgt	r1, #16
	addgt	r1, #0x00010000
	rorgt	r1, #16
	msr		cpsr_f,r2							; Restore flags
	mov		r0, r1								; r0 = offset of the AH value
	ldr		r2, [sp, #SP_STR_SEG]				; r2 = current effective segment
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb	r0, [r2]							; r0 = third byte value
	orr		eax, r0, lsl #24
	b		loop
	
; ------------------- F3 AE = REPE SCASB ------------------------------
; On input CX == r5, r0 = saved flags
;
	GLOBAL	repe_scasb_cld
repe_scasb_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_scasb_cld_RAM, repe_scasb_cld_EGA, repe_scasb_cld_MODEX
	
	EXTERN	repe_scasb_cld_EGA
	EXTERN	repe_scasb_cld_MODEX
	
repe_scasb_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasb_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasb_cld						; ... and go handle the next 16K page.

repe_scasb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	add		edi, #1								; Increment EDI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	b		complement_carry					; We need to swap the Carry flag!

repe_scasb_std
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES repe_scasb_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_scasb_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasb_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00010000					; Decrement DI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasb_std						; ... and go handle the next 16K page.

repe_scasb_std_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #1								; Decrement EDI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	b		complement_carry					; We need to swap the Carry flag!
	
; ------------------- F3 AF = REPE SCASW ------------------------------
; On input CX == r5, r0 = saved flags
;
repe_scasw_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_scasw_cld_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_scasw_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #1
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasw_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00020000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasw_cld						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	orr		r0, r1, r0, lsl #8					; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repe_scasw_cld_USE32
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #2								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #2								; Increment DI
	orr		r0, r1, r0, lsl #8					; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.


repe_scasw_std
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES repe_scasw_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_scasw_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #1
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasw_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasw_std						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI+1
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	sub		edi, #0x00020000					; Increment DI
	orr		r0, r1, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repe_scasw_std_USE32
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #2								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI+1
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	sub		edi, #2								; Increment DI
	orr		r0, r1, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.


; ------------------- F3 AF = REPE SCASD ------------------------------
;
repe_scasd_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repe_scasd_cld_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_scasd_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #2
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasd_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00040000					; Increment DI
	orr		r0, r3, lsl #8
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #16
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #24						; r0 = dword from ES:DI
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasd_cld						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	orr		r1, r0, lsl #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+2
	orr		r1, r0, lsl #16
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+3
	push	{r3}
	orr		r1, r0, lsl #24
	;-------
	; 4) Perform the string operation once.
	;-------
	ror		edi, #16
	add		edi, #0x00040000					; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repe_scasd_cld_USE32
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #4								; Increment DI
	orr		r0, r3, lsl #8
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #16
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #24						; r0 = dword from ES:DI
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	orr		r1, r0, lsl #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+2
	orr		r1, r0, lsl #16
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+3
	push	{r3}
	orr		r1, r0, lsl #24
	;-------
	; 4) Perform the string operation once.
	;-------
	add		edi, #4								; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

repe_scasd_std
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES repe_scasd_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repe_scasd_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #2
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasd_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI+2
	sub		edi, #0x00040000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	orr		r0, r3, r0, lsl #8
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI
	orr		r0, r3, r0, lsl #8
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasd_std						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI+3
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+2
	orr		r1, r0, r1, lsl #8
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+1
	orr		r1, r0, r1, lsl #8
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI
	push	{r3}
	orr		r1, r0, r1, lsl #8
	;-------
	; 4) Perform the string operation once.
	;-------
	ror		edi, #16
	sub		edi, #0x00040000					; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repe_scasd_std_USE32
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI+2
	sub		edi, #4								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	orr		r0, r3, r0, lsl #8
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI
	orr		r0, r3, r0, lsl #8
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	beq		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI+3
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+2
	orr		r1, r0, r1, lsl #8
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+1
	orr		r1, r0, r1, lsl #8
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI
	push	{r3}
	orr		r1, r0, r1, lsl #8
	;-------
	; 4) Perform the string operation once.
	;-------
	sub		edi, #4								; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was not equal.
	;-------
	bne		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F2 AE = REPNE SCASB -----------------------------
; Input
; r0 = saved flags
; r1 = free
; r2 = free
; r4 = AL<<16 = byte to compare
; r5 = CX<<16
; r11 = DI<<16
;
	GLOBAL	repne_scasb_cld
repne_scasb_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_scasb_cld_RAM, repne_scasb_cld_EGA, repne_scasb_cld_MODEX
	
	EXTERN	repne_scasb_cld_EGA
	EXTERN	repne_scasb_cld_MODEX
	
repne_scasb_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasb_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasb_cld						; ... and go handle the next 16K page.

repne_scasb_cld_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	add		edi, #1								; Increment EDI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	b		complement_carry					; We need to swap the Carry flag!

repne_scasb_std
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES repne_scasb_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_scasb_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasb_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00010000					; Decrement DI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasb_std						; ... and go handle the next 16K page.

repne_scasb_std_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #1								; Decrement EDI
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	b		complement_carry					; We need to swap the Carry flag!

; ------------------- F2 AF = REPNE SCASW -----------------------------
; Input
; r0 = saved flags (in high byte), temp variable (low 16 bits)
; r1 = free
; r2 = free
; r4 = AX<<16 = halfword to compare
; r5 = CX<<16
; r11 = DI<<16
;
repne_scasw_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_scasw_cld_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_scasw_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #1
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasw_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00020000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasw_cld						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #0x00020000					; Increment DI
	orr		r0, r1, r0, lsl #8					; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repne_scasw_cld_USE32
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #2								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	add		edi, #2								; Increment DI
	orr		r0, r1, r0, lsl #8					; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.


repne_scasw_std
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES repne_scasw_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_scasw_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #1
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasw_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #0x00020000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasw_std						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI+1
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	ror		edi, #16
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	sub		edi, #0x00020000					; Increment DI
	orr		r0, r1, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repne_scasw_std_USE32
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #1
	beq		%f7									; r1 == 1 => Only one byte left in this page, use special code!
	blt		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI
	sub		edi, #2								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #2								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI+1
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	push	{r3}
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	sub		edi, #2								; Increment DI
	orr		r0, r1, lsl #8						; r0 = halfword from ES:DI
	lsl		r0, #16								; Put it to high halfword of r0
	rsbs	r0, eax, lsl #16					; Subtract the byte from AX
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

; ------------------- F2 AF = REPNE SCASD ------------------------------
;
repne_scasd_cld
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES repne_scasd_cld_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_scasd_cld_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #2
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasd_cld_USE32				; Yep, go handle it.
	ror		edi, #16
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #0x00040000					; Increment DI
	orr		r0, r3, lsl #8
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #16
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #24						; r0 = dword from ES:DI
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasd_cld						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	orr		r1, r0, lsl #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+2
	orr		r1, r0, lsl #16
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+3
	push	{r3}
	orr		r1, r0, lsl #24
	;-------
	; 4) Perform the string operation once.
	;-------
	ror		edi, #16
	add		edi, #0x00040000					; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repne_scasd_cld_USE32
	min_cnt_next_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	add		edi, #4								; Increment DI
	orr		r0, r3, lsl #8
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #16
	ldrb  	r3, [r2], #1						; Get byte from physical ES:DI
	orr		r0, r3, lsl #24						; r0 = dword from ES:DI
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+1
	orr		r1, r0, lsl #8
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+2
	orr		r1, r0, lsl #16
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2], #1						; Get byte from physical ES:DI+3
	push	{r3}
	orr		r1, r0, lsl #24
	;-------
	; 4) Perform the string operation once.
	;-------
	add		edi, #4								; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.

repne_scasd_std
	add		r0, edi, #3							; r0 = DI+3
	mem_handler_jump_r0r3_ES repne_scasd_std_RAM, bad_string_op_seg_back1, bad_string_op_seg_back1
repne_scasd_std_RAM
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	push	{r3}
	lsl		r1, #2
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasd_std_USE32				; Yep, go handle it.
	ror		edi, #16
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r1, edi					; ----- Check with DI
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI+2
	sub		edi, #0x00040000					; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	orr		r0, r3, r0, lsl #8
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI
	orr		r0, r3, r0, lsl #8
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	ror		edi, #16
	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	pop		{r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasd_std						; ... and go handle the next 16K page.
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ror		edi, #16
	ldrb	r1, [r2]							; r1 = byte from ES:DI+3
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+2
	orr		r1, r0, r1, lsl #8
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+1
	orr		r1, r0, r1, lsl #8
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI
	push	{r3}
	orr		r1, r0, r1, lsl #8
	;-------
	; 4) Perform the string operation once.
	;-------
	ror		edi, #16
	sub		edi, #0x00040000					; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b5									; ... go calculate proper addresses for next 16K block.
repne_scasd_std_USE32
	sub		r1, #1
	min_cnt_prev_page 	r1, r2					; ----- Check with ES:DI bytes before page end
	add		r1, #1
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	cmp		r1, #0
	beq		%b6									; Go check whether we need to exit the iteration or handle next page
	cmp		r1, #4
	blt		%f7									; r1 < 4 => Not a full dword left in this page, use special code!
	;-------
	; 4) Perform the string operation once.
	;-------
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+3
	ldrb  	r0, [r2], #-1						; Get byte from physical ES:DI+2
	sub		edi, #4								; Increment DI
	orr		r0, r3, lsl #8						; r0 = halfword from ES:DI
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI+1
	orr		r0, r3, r0, lsl #8
	ldrb  	r3, [r2], #-1						; Get byte from physical ES:DI
	orr		r0, r3, r0, lsl #8
	cmp		eax, r0
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #4								; Decrement page counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
4	pop		{r3}
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; The value to test is split into two pages!
	; We need to get the low bytes, save them temporarily,
	; then calculate the new page addresses and get the high bytes,
	; then compare them, and then continue with the normal loop.
	;-------
7	pop		{r3}								; Restore r3 (16-bit/32-bit mask)
	ldrb	r1, [r2]							; r1 = byte from ES:DI+3
	add		r0, edi, #2							; r0 = DI+2
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+2
	orr		r1, r0, r1, lsl #8
	add		r0, edi, #1							; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI+1
	orr		r1, r0, r1, lsl #8
	mov		r0, edi								; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	ldrb  	r0, [r2]							; Get byte from physical ES:DI
	push	{r3}
	orr		r1, r0, r1, lsl #8
	;-------
	; 4) Perform the string operation once.
	;-------
	sub		edi, #4								; Increment DI
	cmp		eax, r1
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	beq		%b4									; Comparison was equal, so go exit the iteration.
	mrs		r0, cpsr							; Else save flags to r0, and ...
	b		%b6									; ... go calculate proper addresses for next 16K block.


; ---------------------------------------------------------------------

	AREA cpu_string_data, DATA, READWRITE
	ALIGN	4

BRUnsString
	DCB	"Unsupported string op seg/dir!"
	DCB	0x0a, 0

	END
