;=============================================================================
; ports.s
;
; This file contains opcode handlers for I/O port opcodes.
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

	AREA ports, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

	EXTERN	loop
	EXTERN	restore_flags_from_r0
	EXTERN	debug_trap_false
	EXTERN	unknown
	EXTERN	bad_string_op_seg

	EXTERN	out_20_al
	EXTERN	out_21_al
	EXTERN	in_al_20
	EXTERN	in_al_21
	
	EXTERN	registers
	
	EXTERN	INTVectors
	EXTERN	BreakReason
	
	GLOBAL	bad_out_port
	GLOBAL	bad_in_port
	GLOBAL op_6f_outsw
	GLOBAL	op_e4_in_al_imm8
	GLOBAL	op_e5_in_ax_imm8
	GLOBAL	op_e6_out_imm8_al
	GLOBAL op_ec_in_al_dx
	GLOBAL	op_ee_out_dx_al
	GLOBAL	rep_outsb
	GLOBAL	rep_insb

	GLOBAL restore_flags_goto_debugger

;-------------------- PC PORT OVERVIEW ---------------------------------
;
; 000-00F	DMA Controller 8237 A-5
; 010-01F	RESERVED
; 020-02F	Interrupt Controller 1 8259A
; 030-03F	Interrupt Controller 1
; 040-04F	Timer 8254
; 050-05F	Timer
; 060-06F	Keyboard 8042
; 070-07F	RTC, NMI Mask
; 080-08F	DMA Page Registers
; 090-09F	DMA Page Registers
; 0A0-0AF	Interrupt Controller 2
; 0B0-0BF	Interrupt Controller 2
; 0C0-0CF	DMA Controller 2
; 0D0-0DF	DMA Controller 2
; 0E0-0EF	RESERVED
; 0F0-0FF	Math Coprocessor (80287)
; 100-1EF	I/O expansion card
; 1F0-1FF	Fixed disk
; 200-20F	Game adapter
;

IGNORE_BAD_PORTS	EQU		1

; ------------------- Handling of unsupported port number -------------
;
bad_out_port
	IF IGNORE_BAD_PORTS = 1
	bx		lr
	ELSE
	pop		{r0,lr}
	ENDIF
restore_flags_goto_debugger
	msr		cpsr_f,r0				; Set the processor flags
	ldr		r0, =BRUnsPORT
	ldr		r1, =BreakReason
	str		r0, [r1]				; ... tell we break because of an unsupported port I/O, ...
	b		debug_trap_false		; Stop and go to the debugger

bad_in_port
	IF IGNORE_BAD_PORTS = 1
	orr		eax, #0xFF				; Set AL = 0xFF
	bx		lr
	ELSE
	pop		{r0,lr}
	msr		cpsr_f,r0				; Set the processor flags
	ldr		r0, =BRUnsPORT
	ldr		r1, =BreakReason
	orr		eax, #0xFF				; Set AL = 0xFF
	str		r0, [r1]				; ... tell we break because of an unsupported port I/O, ...
	b		debug_trap_false		; Stop and go to the debugger
	ENDIF


; =================== PORT OUTPUT =====================================


; ------------------- 6E = OUTSB --------------------------------------
;
; Output a byte from r2:r10 to port r6, increment r10 by 1.
; Uses new LOGSEG_MEM_ACCESS!
;
	GLOBAL op_6e_outsb
op_6e_outsb
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_6e_prot_r0r2			; Yes we are, go handle protected mode OUTSB!
	GLOBAL	op_6e_real_r0r2
op_6e_real_r0r2
	ldr		r1, [sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	mov		r1, r0					; Save flags to r1
	mov		r0, esi					; r0 = SI
	ror		esi, #16
	addeq	esi, #0x00010000		; Fix logical DI.
	subne	esi, #0x00010000		; Fix logical DI.
	ror		esi, #16
	mem_handler_jump_r0r3 outsb_RAM, unknown, unknown

	EXTERN	op_6e_prot_r0r2
	
	GLOBAL	outsb_RAM

outsb_RAM
	mov		r0, r1					; Save current flags to r0
	ldrb	r1, [r2]				; r1 = byte to output (from effective_segment:SI)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	GLOBAL	op_6e_outsb_USE32
op_6e_outsb_USE32
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_6e_USE32_prot_r0r2	; Yes we are, go handle protected mode OUTSB!
	GLOBAL	op_6e_USE32_real_r0r2
op_6e_USE32_real_r0r2
	ldr		r1, [sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	mvn		r3, #0					; Use 32-bit memory addressing
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	mov		r1, r0					; Save flags to r1
	mov		r0, esi					; r0 = ESI
	addeq	esi, #1					; Fix logical ESI
	subne	esi, #1					; Fix logical ESI
	mem_handler_jump_r0r3 goto_outsb_RAM, unknown, unknown

goto_outsb_RAM
	b		outsb_RAM
	
	EXTERN	op_6e_USE32_prot_r0r2
	
; ------------------- F3 6E = REP OUTSB -------------------------------
;
; Output CX bytes from r2:r10 to port r6, increment r10 by CX.
;
; On input
;	r0 = saved flags
;	r2 = effective segment
rep_outsb
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	msr		cpsr_f,r0				; Restore flags
	mov		r0, esi					; r0high = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg, bad_string_op_seg
1	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	movs	r1, ecx, lsl #16
	beq		restore_flags_from_r0	; Nothing to do if CX == 0
	push	{r0, r2}
1	ldrb	r1,[r2]					; r1 = byte to output (from effective_segment:SI)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		out_byte_r1r2			; Call the OUT handler (expects r0 and lr to be pushed to stack!)
	ldr		r2, [sp, #4]			; Restore r2
	add		esi, #1					; Fix logical SI, direction = UP.
	add		r2, #1
	str		r2, [sp, #4]			; Save r2
	sub		ecx, #1					; CX--
	movs	r1, ecx, lsl #16		; CX == 0 ?
	bne		%b1
	pop		{r0, r2}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	GLOBAL	rep_outsb_cld_USE32
rep_outsb_cld_USE32
	mvn		r3, #0								; Use 32-bit memory address masking
	msr		cpsr_f,r0				; Restore flags
	mov		r0, esi					; r0high = SI
	mem_handler_jump_r0r3 %f1, bad_string_op_seg, bad_string_op_seg
1	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	cmp		ecx, #0
	beq		restore_flags_from_r0	; Nothing to do if CX == 0
	push	{r0, r2}
1	ldrb	r1,[r2]					; r1 = byte to output (from effective_segment:SI)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		out_byte_r1r2			; Call the OUT handler (expects r0 and lr to be pushed to stack!)
	ldr		r2, [sp, #4]			; Restore r2
	add		esi, #1					; Fix logical SI, direction = UP.
	add		r2, #1
	str		r2, [sp, #4]			; Save r2
	subs	ecx, #1					; CX--
	bne		%b1
	pop		{r0, r2}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

; ------------------- 6F = OUTSW --------------------------------------
;
; Output a word from r2:r10 to port r6, increment r10 by 2.
; Uses new LOGSEG_MEM_ACCESS!
;
op_6f_outsw
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, esi					; r0high = SI
	mem_handler_jump_r0r3 outsw_RAM, bad_string_op_seg, bad_string_op_seg
outsw_RAM
	mrs		r0, cpsr				; Save current flags to r0
	;-------
	; Save the registers (we must have exactly 2 registers in the stack!)
	;-------
	push	{r0, r2}
	;-------
	; Output the low byte to port DX
	;-------
	ldrb	r1,[r2]					; r1 = byte to output (from effective_segment:SI)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Restore the effective segment, adjust SI for high byte
	;-------
	ldr		r2, [sp, #4]			; Get r2 from stack, but also keep it there
	;-------
	; Output the high byte to port DX+1
	;-------
	ldrb	r1,[r2, #1]				; r1 = byte to output (from effective_segment:SI+1)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	add		r2, #1
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Restore the registers
	;-------
	pop		{r0, r2}
	;-------
	; Adjust SI by the direction flag
	;-------
	ldr		r1,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	addeq	esi, #2					; Fix logical SI, direction = UP.
	subne	esi, #2					; Fix logical SI, direction = DOWN.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

; ------------------- E6 = OUT imm8,AL --------------------------------
;
op_e6_out_imm8_al
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e6_prot_r0			; Yes we are, go handle protected mode OUT imm8, AL!
	GLOBAL	op_e6_real_r0
op_e6_real_r0
	and		r1, eax, #0xFF			; r1 = AL value
	ldrb	r2,[r12],#1				; r2 = port number
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e6_prot_r0
	
; ------------------- E7 = OUT imm8,AX --------------------------------
;
	GLOBAL	op_e7_out_imm8_ax
op_e7_out_imm8_ax
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e7_prot_r0			; Yes we are, go handle protected mode IN AL,DX!
	GLOBAL	op_e7_real_r0
op_e7_real_r0
	;-------
	; First output AL to port imm8
	;-------
	and		r1, eax, #0xFF			; r1 = AL value
	ldrb	r2,[r12],#1				; r2 = port number
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output AH to port imm8+1
	;-------
	ldrb	r2,[r12, #-1]			; r2 = port number
	mov		r1, eax, lsr #8			; r1 = AH value
	and		r1, #0xFF
	add		r2, #1					; r2 = port+1
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e7_prot_r0
	
; ------------------- E7 = OUT imm8,EAX --------------------------------
;
	GLOBAL	op_e7_out_imm8_eax
op_e7_out_imm8_eax
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e7_USE32_prot_r0			; Yes we are, go handle protected mode IN AL,DX!
	GLOBAL	op_e7_USE32_real_r0
op_e7_USE32_real_r0
	;-------
	; First output AL to port imm8
	;-------
	and		r1, eax, #0xFF			; r1 = AL value
	ldrb	r2,[r12]				; r2 = port number
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output AH to port imm8+1
	;-------
	ldrb	r2,[r12]				; r2 = port number
	mov		r1, eax, lsr #8			; r1 = AH value
	and		r1, #0xFF
	add		r2, #1					; r2 = port+1
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output 3rd byte to port imm8+2
	;-------
	ldrb	r2,[r12]				; r2 = port number
	mov		r1, eax, lsr #16		; r1 = value
	and		r1, #0xFF
	add		r2, #2					; r2 = port+2
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output fourth byte to port imm8+3
	;-------
	ldrb	r2,[r12], #1			; r2 = port number
	mov		r1, eax, lsr #24		; r1 = value
	add		r2, #3					; r2 = port+1
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e7_USE32_prot_r0
	
; ------------------- EE = OUT dx,al ----------------------------------
;
	GLOBAL	op_ee_out_dx_al
op_ee_out_dx_al
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ee_prot_r0			; Yes we are, go handle protected mode OUT DX, AL!
	GLOBAL	op_ee_real_r0
op_ee_real_r0
	and		r1, eax, #0xFF			; r1 = AL value
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ee_prot_r0
	
; ------------------- EF = OUT dx,ax ----------------------------------
	GLOBAL	op_ef_out_dx_ax
op_ef_out_dx_ax
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ef_prot_r0			; Yes we are, go handle protected mode OUT DX, AX!
	GLOBAL	op_ef_real_r0
op_ef_real_r0
	;-------
	; First output AL to port DX
	;-------
	and		r1, eax, #0xFF			; r1 = AL value
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output AH to port DX+1
	;-------
	ubfx	r1, eax, #8, #8			; r1 = AH value
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	add		r2, #1					; r2 = DX+1
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ef_prot_r0
	
; ------------------- EF = OUT dx,eax ----------------------------------
	GLOBAL	op_ef_out_dx_eax
op_ef_out_dx_eax
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ef_USE32_prot_r0			; Yes we are, go handle protected mode OUT DX, AX!
	GLOBAL	op_ef_USE32_real_r0
op_ef_USE32_real_r0
	;-------
	; First output AL to port DX
	;-------
	and		r1, eax, #0xFF			; r1 = AL value
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	push	{r0, lr}
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output AH to port DX+1
	;-------
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	mov		r1, eax, lsr #8			; r1 = AH value
	and		r1, #0xFF
	add		r2, #1					; r2 = port+1
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output 3rd byte to port DX+2
	;-------
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	mov		r1, eax, lsr #16		; r1 = value
	and		r1, #0xFF
	add		r2, #2					; r2 = port+2
	bl		out_byte_r1r2			; Call the OUT handler
	;-------
	; Then output fourth byte to port DX+3
	;-------
	ubfx	r2, edx, #0, #16		; r2 = DX value = port number
	mov		r1, eax, lsr #24		; r1 = value
	add		r2, #3					; r2 = port+1
	bl		out_byte_r1r2			; Call the OUT handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ef_USE32_prot_r0
	
; ------------------- OUT BYTE Handler --------------------------------
; On input
;	r0 = free
;	r1 = byte to output
;	r2 = port address (DX value)
;	lr = return address
;
out_byte_r1r2
	cmp		r2, #0x400
	bge		out_ignore				; No port above 0x400 is supported!
	ldr		r0, =out_byte_jump		; r0 = address of the jump table
	bfi		r1, r2, #16, #16		; Put the port address to the r1 high 16 bits
	bic		r1, #0x000F0000			; Clean the low 4 bits of the port number
	add		r0, r1, lsr #(16+4-2)	; Add the port address / 16 to the jump table address
	and		r1, #0xFF				; r1 = byte value
	and		r2, #0x0F				; r2 = port address lowest 4 bits
	ldr		pc,[r0]					; Jump to the handler
	
	AREA	jumptables, DATA, READONLY
	ALIGN	4
out_byte_jump
	;-------
	; Each handler handles a 16-port range. Register r2 tells which of the 16 ports in the block should we output to.
	;-------
; 0
	DCD out_000_00F, bad_out_port, out_020_02F, bad_out_port, out_040_04F, bad_out_port, out_060_06F, out_070_07F
; 0x80
	DCD out_80_8F, out_090_09F, out_ignore, bad_out_port, out_0C0_0CF, bad_out_port, out_ignore, out_ignore
; 0x100
	DCD out_100_10F, out_110_11F, bad_out_port, bad_out_port, bad_out_port, bad_out_port, bad_out_port, bad_out_port
; 0x180
	DCD bad_out_port, out_ignore, bad_out_port, bad_out_port, bad_out_port, bad_out_port, bad_out_port, bad_out_port
; 0x200
	DCD out_200_20F, out_ignore, out_220_22F, out_ignore, out_ignore, out_ignore, out_ignore, out_270_27F
; 0x280
	DCD out_ignore, out_ignore, out_ignore, out_ignore, out_ignore, out_ignore, out_ignore, out_ignore
; 0x300
	DCD out_ignore, out_ignore, out_ignore, out_330_33F, out_ignore, out_ignore, out_ignore, out_370_37F
; 0x380
	DCD out_380_38F, bad_out_port, bad_out_port, out_ignore, out_3C0_3CF, out_3D0_3DF, out_ignore, out_ignore

	AREA ports, CODE, READONLY
	ALIGN	4

	;=======
	; DMA Controller ports. All DMA commands are sent to ARM7 (for SB DMA handling).
	;=======
out_000_00F
	ldr		r0, =DMAAddress
	cmp		r2, #2					; Port 0x02?
	beq		set_DMA_addr_byte
	cmp		r2, #3					; Port 0x03?
	beq		set_DMA_length_byte
	cmp		r2, #0x0C				; Port 0x0C?
	moveq	r2, #0
	strbeq	r2, [r0, #(DMAFlipFlop-DMAAddress)]	; Clear flipflop. We do not check the actual written AL value! TODO!
	bx		lr						; Return to caller
set_DMA_addr_byte
	ldrb	r2, [r0, #(DMAFlipFlop-DMAAddress)]
	strb	r1, [r0, r2]			; Set the lowest or second-lowest byte of DMAAddress, depending on flipflop value.
	eors	r2, #1					; Reverse flipflop value
	strb	r2, [r0, #(DMAFlipFlop-DMAAddress)]
	mov		r2, #2					; Restore r2 = port number
	bxne	lr
	;-------
	; Initialize also DMACurrent = DMAAddress+INTVectors when both bytes of the address have been given
	;-------
	push	{r3}
	ldr		r3, =INTVectors
	ldr		r2, [r0]				; r2 = DMAAddress
	ldr		r3, [r3]
	add		r2, r3					; r2 = DMAAddress+INTVectors
	str		r2, [r0, #(DMACurrent-DMAAddress)]
	pop		{r3}
	mov		r2, #2					; Restore r2 = port number
	bx		lr						; Return to caller
set_DMA_length_byte
	ldrb	r2, [r0, #(DMAFlipFlop-DMAAddress)]
	add		r2, #4
	strb	r1, [r0, r2]			; Set the lowest or second-lowest byte of DMALength, depending on flipflop value.
	sub		r2, #4
	eor		r2, #1					; Reverse flipflop value
	strb	r2, [r0, #(DMAFlipFlop-DMAAddress)]
	mov		r2, #3					; Restore r2 = port number
	bx		lr						; Return to caller

	;=======
	; Interrupt Controller 1 Ports (handled in "pic.s")
	;=======
out_020_02F
	cmp		r2, #1					; Port 0x21?
	beq		out_21_al
	blt		out_20_al
	cmp		r2, #8					; Port 0x28?
	bne		bad_out_port			; Nope
	bx		lr						; Yep, probably SIMDEMO bug, ignore it

	
	;=======
	; Timer Ports (handled in "timer.s")
	;=======
out_040_04F
	cmp		r2, #1					; Port 0x41?
	blt		out_40_al_j
	beq		out_ignore
	cmp		r2, #3					; Port 0x43?
	beq		out_43_al_j
	blt		out_42_al_j
	b		out_ignore
out_40_al_j
	push	{r12, lr}
	mov		r0, r1
	bl		out_40_al				; Call the C routine
	pop		{r12, pc}
out_42_al_j
	push	{r12, lr}
	mov		r0, r1
	bl		out_42_al				; Call the C routine
	pop		{r12, pc}
out_43_al_j
	push	{r12, lr}
	mov		r0, r1
	bl		out_43_al				; Call the C routine
	pop		{r12, pc}

	EXTERN	out_40_al
	EXTERN	out_42_al
	EXTERN	out_43_al

	;=======
	; Keyboard controller ports
	;=======
out_060_06F
	cmp		r2, #1					; Port 0x61?
	blt		out_60					; Jump if port 0x60
	bxne	lr						; Ignore ports other than 0x60 and 0x61
	;-------
	; Port 0x61 = Keyboard/Speaker control
	; Save the new data byte
	;-------
	ldr		r2,=Port61Data
	ldrb	r0, [r2]				; Get current Port61 data value
	eor		r0, r1
	ands	r0, #3					; Has the speaker status (on/off) changed?
	strb	r1, [r2]				; Save the new Port61 value
	bx		lr						; No change in speaker status, return
	;-------
	; Port 0x60 = Keyboard Microcontroller Commands (Port 60h) Value (hex) Description
	;	D0 Copy microcontroller output port value to port 60h (see definition below). 
	;	D1 Write the next data byte written to port 60h to the microcontroller output port. This port has the following definition
	;		bit 7: Keyboard data.
	;		bit 6: Keyboard clock.
	;		bit 5: Input buffer empty flag.
	;		bit 4: Output buffer full flag.
	;		bit 3: Undefined.
	;		bit 2: Undefined.
	;		bit 1: Gate A20 line.
	;		bit 0: System reset (if zero).
	;		Note: writing a zero to bit zero will reset the machine.
	;		Writing a one to bit one combines address lines 19 and 20 on the PCs address bus. 
 	;	ED 	Send LED bits. The next byte written to port 60h updates the LEDs on the keyboard. The parameter (next) byte contains
	;		bits 3-7: Must be zero.
	;		bit 2: Capslock LED (1 = on, 0 = off).
	;		bit 1: Numlock LED (1 = on, 0 = off).
	;		bit 0: Scroll lock LED (1 = on, 0 = off). 
	;	EE 	Echo commands. Returns 0EEh in port 60h as a diagnostic aid. 
	;	F0 	Select alternate scan code set (PS/2 only). The next byte written to port 60h selects one of the following options
	;		00: Report current scan code set in use (next value read from port 60h).
	;		01: Select scan code set #1 (standard PC/AT scan code set).
	;		02: Select scan code set #2.
	;		03: Select scan code set #3. 
	;	F2 	Send two-byte keyboard ID code as the next two bytes read from port 60h (PS/2 only). 
	;	F3 	Set Autorepeat delay and repeat rate. Next byte written to port 60h determines rate
	;		bit 7: must be zero
	;		bits 5,6: Delay. 00- 1/4 sec, 01- 1/2 sec, 10- 3/4 sec, 11- 1 sec.
	;		bits 0-4: Repeat rate. 0- approx 30 chars/sec to 1Fh- approx 2 chars/sec. 
	;	F4 	Enable keyboard. 
	;	F5 	Reset to power on condition and wait for enable command. 
	;	F6 	Reset to power on condition and begin scanning keyboard. 
	;	F7	Make all keys autorepeat (PS/2 only). 
	;	F8 	Set all keys to generate an up code and a down code (PS/2 only). 
	;	F9 	Set all keys to generate an up code only (PS/2 only). 
	;	FA 	Set all keys to autorepeat and generate up and down codes (PS/2 only). 
	;	FB 	Set an individual key to autorepeat. Next byte contains the scan code of the desired key. (PS/2 only). 
	;	FC 	Set an individual key to generate up and down codes. Next byte contains the scan code of the desired key. (PS/2 only). 
	;	FD 	Set an individual key to generate only down codes. Next byte contains the scan code of the desired key. (PS/2 only). 
	;	FE 	Resend last result. Use this command if there is an error receiving data. 
	;	FF 	Reset keyboard to power on state and start the self-test. 
	;-------
CMD_NONE		EQU		0
CMD_SETLEDS		EQU		0xED
CMD_SETTYPERATE	EQU		0xF3
CMD_SETOUTPORT	EQU		0xD1
	
out_60
	ldr		r2,=Port60Command
	ldrb	r0, [r2]				; Get current Port60 command value
	cmp		r0, #CMD_SETTYPERATE	; Was the previous command "Set Autorepeat Delay and Rate"?
	beq		%f2
	cmp		r0, #CMD_SETOUTPORT		; Was the previous command "Set Output Port"?
	beq		%f3
	strb	r1, [r2]				; Store the new command byte
	bx		lr						; Not an autorepeat command, ignore it
	;-------
	; Autorepeat Delay and Rate command value
	;-------
2	strb	r1, [r2, #(RepeatRate-Port60Command)]	; Store the new autorepeat value
	mov		r1, #0
	strb	r1, [r2]				; Reset the new command byte
	bx		lr
	;-------
	; Out Port command (A20 gate change)
	;-------
3	mov		r0, #0
	strb	r0, [r2]				; Reset the new command byte
	mov		r2, #2					; Point to port 0x92
	b		out_090_09F				; and go setup the A20 line

	;-------
	; On-Board Keyboard Controller Commands (Port 64h) Value (hex) Description 
	;	20 Transmit keyboard controllers command byte to system as a scan code at port 60h. 
	;	60 The next byte written to port 60h will be stored in the keyboard controllers command byte. 
	;	A4 Test if a password is installed (PS/2 only). Result comes back in port 60h. 0FAh means a password is installed, 0F1h means no password. 
	;	A5 Transmit password (PS/2 only). Starts receipt of password. The next sequence of scan codes written to port 60h, ending with a zero byte, are the new password. 
	;	A6 Password match. Characters from the keyboard are compared to password until a match occurs. 
	;	A7 Disable mouse device (PS/2 only). Identical to setting bit five of the command byte.  
	;	A8 Enable mouse device (PS/2 only). Identical to clearing bit five of the command byte. 
	;	A9 Test mouse device. Returns 0 if okay, 1 or 2 if there is a stuck clock, 3 or 4 if there is a stuck data line. Results come back in port 60h. 
	;	AA Initiates self-test. Returns 55h in port 60h if successful. 
	;	AB Keyboard interface test. Tests the keyboard interface. Returns 0 if okay, 1 or 2 if there is a stuck clock, 3 or 4 if there is a stuck data line. Results come back in port 60h. 
	;	AC Diagnostic. Returns 16 bytes from the keyboards microcontroller chip. Not available on PS/2 systems. 
	;	AD Disable keyboard. Same operation as setting bit four of the command register. 
	;	AE Enable keyboard. Same operation as clearing bit four of the command register. 
	;	C0 Read keyboard input port to port 60h. This input port contains the following values
	;		bit 7: Keyboard inhibit keyswitch (0 = inhibit, 1 = enabled).
	;		bit 6: Display switch (0=color, 1=mono).
	;		bit 5: Manufacturing jumper.
	;		bit 4: System board RAM (always 1).
	;		bits 0-3: undefined. 
	;	C1 Copy input port (above) bits 0-3 to status bits 4-7. (PS/2 only) 
	;	C2 Copy input pot (above) bits 4-7 to status port bits 4-7. (PS/2 only). 
	;	D0 Copy microcontroller output port value to port 60h (see definition below). 
	;	D1 Write the next data byte written to port 60h to the microcontroller output port. This port has the following definition
	;		bit 7: Keyboard data.
	;		bit 6: Keyboard clock.
	;		bit 5: Input buffer empty flag.
	;		bit 4: Output buffer full flag.
	;		bit 3: Undefined.
	;		bit 2: Undefined.
	;		bit 1: Gate A20 line.
	;		bit 0: System reset (if zero).
	;		Note: writing a zero to bit zero will reset the machine.
	;		Writing a one to bit one combines address lines 19 and 20 on the PCs address bus. 
	;	D2 Write keyboard buffer. The keyboard controller returns the next value sent to port 60h as though a keypress produced that value. (PS/2 only). 
	;	D3 Write mouse buffer. The keyboard controller returns the next value sent to port 60h as though a mouse operation produced that value. (PS/2 only). 
	;	D4 Writes the next data byte (60h) to the mouse (auxiliary) device. (PS/2 only). 
	;	E0 Read test inputs. Returns in port 60h the status of the keyboard serial lines. Bit zero contains the keyboard clock input, bit one contains the keyboard data input. 
	;	Fx Pulse output port (see definition for D1). Bits 0-3 of the keyboard controller command byte are pulsed onto the output port. Resets the system if bit zero is a zero. 
	;-------

	;=======
	; RTC, NMI Mask
	;=======
out_070_07F
	cmp		r2, #1					; Port 0x71?
	beq		%f1
	bgt		bad_out_port
	;-------
	; Port 0x70 = CMOS register select
	;-------
	ldr		r2,=cmos_reg
	and		r0, r1, #0x80
	and		r1, #0x3F
	strb	r1, [r2]				; Save the CMOS address
	strb	r0, [r2, #-1]			; Save the CMOS NMI byte
	bx		lr
	;-------
	; Port 0x71 = CMOS value
	;-------
1	ldr		r2,=cmos_regs
	ldrb	r0, [r2, #-1]			; r0 = cmos_reg value
	and		r1, #0x7F
	and		r0, #0x3F
	strb	r1, [r2, r0]			; Store the register value
	bx		lr
	
	;=======
	; DMA Page Register Ports
	;=======
out_80_8F
	ldr		r0, =DMAAddress
	cmp		r2, #3					; Port 0x83?
	bxne	lr						; Nope, ignore other DMA channels besides 1
	strb	r1, [r0, #2]			; Set the page number of the DMAAddress
	bx		lr
	
out_090_09F
	cmp		r2, #2					; Port 0x92 ?
	bne		bad_out_port
	;-------
	; OUT 0x92,AL = System Control Port A
	;-------
	ldr		r2, =Port92Data
	bic		r0, r1, #2				; Clear the A20 bit from the value we save to Port92Data
	strb	r1, [r2]
	and		r0, r1, #2				; r0 = 0 if we must disable A20 gate, r0 = 2 if we must enable it.
	push	{r12, lr}
	bl		SetA20Gate				; Call the C routine
	pop		{r12, pc}

	EXTERN	SetA20Gate
	
	;=======
	; DMA Controller 2
	;=======
out_0C0_0CF
	cmp		r2, #0					; Port 0xC0?
	bxeq	lr
	b		bad_out_port

	;=======
	; Raspberry Pi GPIO pins, using wiringPi
	;=======
out_110_11F
	add		r2, #0x10				; Ports 0x110..0x11F
out_100_10F
	bx		lr
	
	;=======
	; Joystick port
	;=======
out_200_20F
	ldr		r2, =joy_counters
	ldr		r1, [r2, #(JoyValues - joy_counters)]
	str		r1, [r2]				; Init the counters by the current joystick axis values.
out_ignore
	bx		lr						; Ignore all writes to ports 0x200..0x20F
	
	;=======
	; SoundBlaster ports (handled in "ports_SB.s")
	;=======
out_220_22F
	cmp		r2, #9					; Port 0x229?
	beq		out_229_389_FM_data
	cmp		r2, #8					; Port 0x228?
	beq		out_228_388_FM_regsel
	cmp		r2, #0xC				; Port 0x22C?
	beq		out_22C_SB_DSP
	cmp		r2, #6					; Port 0x226?
	beq		out_226_SB_reset
	cmp		r2, #1					; Port 0x221? (SB Pro left AdLib channel)
	beq		out_229_389_FM_data
	cmp		r2, #0					; Port 0x220? (SB Pro left AdLib channel)
	beq		out_228_388_FM_regsel
	bx		lr						; Ignore output to other SB ports

	EXTERN	out_229_389_FM_data
	EXTERN	out_228_388_FM_regsel
	EXTERN	out_22C_SB_DSP
	EXTERN	out_226_SB_reset
	EXTERN	out_229_389_FM_data
	EXTERN	out_228_388_FM_regsel
	
	;=======
	; Roland MPU 401 ports
	;=======
out_330_33F
	bx		lr
	
	;=======
	; Printer ports
	;=======
out_270_27F
out_370_37F
	bx		lr

	;=======
	; AdLib ports (handled in "ports_SB.s")
	;=======
out_380_38F
	cmp		r2, #9					; Port 0x389?
	beq		out_229_389_FM_data
	cmp		r2, #8					; Port 0x388?
	beq		out_228_388_FM_regsel
	bx		lr						; Ignore output to other AdLib ports

	EXTERN	out_229_389_FM_data
	EXTERN	out_228_388_FM_regsel
	
	;=======
	; VGA ports (handled in "EGA.s")
	;=======
out_3C0_3CF
	ldr		r0, =out_VGA_jump
	ldr		pc,[r0, r2, lsl #2]		; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
out_VGA_jump
	DCD	out_3C0_VGA_attr, out_ignore, out_3C2_VGA_misc, bad_out_port
	DCD	out_3C4_VGA_sequencer, out_3C5_VGA_sequencer, out_ignore, out_3C7_VGA_pel
	DCD	out_3C8_VGA_pel
	DCD	%f1, bad_out_port, bad_out_port
	DCD	bad_out_port, out_ignore, out_3CE_VGA_graphics, out_3CF_VGA_graphics

	AREA ports, CODE, READONLY
	ALIGN	4

1	ldr		r2, =VGA_out_3C9_addr
	ldr		pc, [r2]				; Jump to the function

	EXTERN	out_3C0_VGA_attr
	EXTERN	out_3C2_VGA_misc
	EXTERN	out_3C4_VGA_sequencer
	EXTERN	out_3C5_VGA_sequencer
	EXTERN	out_3C7_VGA_pel
	EXTERN	out_3C8_VGA_pel
	EXTERN	out_3CE_VGA_graphics
	EXTERN	out_3CF_VGA_graphics

	;=======
	; CGA/VGA ports (in "EGA.s"/"CGA.s")
	;=======
out_3D0_3DF
	ldr		r0, =out_CGA_jump
	ldr		pc,[r0, r2, lsl #2]		; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
out_CGA_jump
	DCD	out_3D4_VGA_CRTC_addr, out_3D5_VGA_CRTC_data, out_3D4_VGA_CRTC_addr, out_3D5_VGA_CRTC_data
	DCD	out_3D4_VGA_CRTC_addr, out_3D5_VGA_CRTC_data, out_3D4_VGA_CRTC_addr, out_3D5_VGA_CRTC_data
	DCD	out_3D8_CGA_Mode, out_3D9_CGA_Color, out_ignore, out_ignore
	DCD	out_ignore, out_ignore, out_ignore, out_ignore

	AREA ports, CODE, READONLY
	ALIGN	4


	EXTERN	out_3D4_VGA_CRTC_addr
	EXTERN	out_3D5_VGA_CRTC_data
	EXTERN	out_3D4_VGA_CRTC_addr
	EXTERN	out_3D5_VGA_CRTC_data
	EXTERN	out_3D4_VGA_CRTC_addr
	EXTERN	out_3D5_VGA_CRTC_data
	EXTERN	out_3D4_VGA_CRTC_addr
	EXTERN	out_3D5_VGA_CRTC_data
	EXTERN	out_3D8_CGA_Mode
	EXTERN	out_3D9_CGA_Color


	LTORG
	
; =================== PORT INPUT ======================================

; ------------------- 6C = INSB ---------------------------------------
;
	GLOBAL	op_6c_insb
op_6c_insb
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_6c_prot_r0r2			; Yes we are, go handle protected mode INSB!
	GLOBAL	op_6c_real_r0r2
op_6c_real_r0r2
	ldr		r1, [sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	mov		r1, r0					; Save flags to r1
	mov		r0, edi					; r0 = DI
	ror		edi, #16
	addeq	edi, #0x00010000		; Fix logical DI.
	subne	edi, #0x00010000		; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES insb_RAM, unknown, unknown

	GLOBAL	insb_RAM
	;-------
	; On input
	;	r0 = free
	;	r1 = saved flags
	;	r2 = physical ES:DI address
	;-------
insb_RAM
	;-------
	; Save the registers we must save.
	; We must push exactly two words to stack!
	;-------
	str		r1, [sp, #SP_STR_SEG]	; Save the current flags
	mov		r3, eax					; Save EAX to r3
	push	{r2, lr}				; Save physical ES:DI address and physical SS:0000 address
	;-------
	; Read the byte from port DX
	;-------
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		in_byte_r2				; Call the IN handler, returns byte read in AL (r4)
	;-------
	; Store the byte to ES:DI
	;-------
	pop		{r2, lr}				; Restore physical ES:DI address and physical SS:0000 address
	strb	eax, [r2]				; Store byte to ES:DI
	;-------
	; Restore saved registers
	;-------
	ldr		r0, [sp, #SP_STR_SEG]	; r0 = saved CPU flags
	mov		eax, r3					; Restore eax from r3
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	GLOBAL	op_6c_insb_USE32
op_6c_insb_USE32
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_6c_USE32_prot_r0r2	; Yes we are, go handle protected mode INSB!
	GLOBAL	op_6c_USE32_real_r0r2
op_6c_USE32_real_r0r2
	ldr		r1, [sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	mvn		r3, #0					; Use 32-bit memory addressing
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	mov		r1, r0					; Save flags to r1
	mov		r0, edi					; r0 = EDI
	addeq	edi, #1					; Fix logical EDI
	subne	edi, #1					; Fix logical EDI
	mem_handler_jump_r0r3_ES goto_insb_RAM, unknown, unknown

goto_insb_RAM
	b		insb_RAM
	
	EXTERN	op_6c_prot_r0r2
	EXTERN	op_6c_USE32_prot_r0r2
	
; ------------------- F3 6C = REP INSB -------------------------------
;
; Input CX bytes from port r6 to ES:r11, increment r11 by CX.
;
; On input
;	r0 = saved flags
;	r2 = effective segment
rep_insb
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	msr		cpsr_f,r0				; Restore flags
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg, bad_string_op_seg
1	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	movs	r1, ecx, lsl #16
	beq		restore_flags_from_r0	; Nothing to do if CX == 0
	ldr		r1,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	and		r0, #0xF0000000			; r0 highest 4 bits == saved flags
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	orrne	r0, #0x08000000			; Set the bit if direction flag is set
	mov		r3, eax					; Save EAX to r3
	ror		edi, #16
	; ----- Loop here
1	push	{r0, r2}
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		in_byte_r2				; Call the IN handler
	pop		{r0, r2}
	; ----- Fix the RAM address and save the byte
	strb	eax, [r2]				; Store byte to ES:DI
	; ----- Increment/decrement RAM address and DI register
	tst		r0, #0x08000000			; Check the "Direction" bit
	addeq	edi, #0x00010000		; Fix logical DI, direction = UP.
	addeq	r2, #1
	subne	edi, #0x00010000		; Fix logical DI, direction = DOWN.
	subne	r2, #1
	; ----- Back to loop if not yet all done
	sub		ecx, #1					; CX--
	movs	r1, ecx, lsl #16
	bne		%b1
	ror		edi, #16
	mov		eax, r3					; Restore EAX value
	and		r0, #0xF0000000			; r0 highest 4 bits == saved flags
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	GLOBAL rep_insb_cld_USE32
rep_insb_cld_USE32
	mvn		r3, #0					; Use 32-bit memory address masking
	msr		cpsr_f,r0				; Restore flags
	mov		r0, edi					; r0 = DI
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg, bad_string_op_seg
1	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	cmp		ecx, #0
	beq		restore_flags_from_r0	; Nothing to do if CX == 0
	ldr		r1,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	and		r0, #0xF0000000			; r0 highest 4 bits == saved flags
	tst		r1, #FLAG_DF			; Check the "Direction" bit
	orrne	r0, #0x08000000			; Set the bit if direction flag is set
	mov		r3, eax					; Save EAX to r3
	; ----- Loop here
1	push	{r0, r2}
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	bl		in_byte_r2				; Call the IN handler
	pop		{r0, r2}
	; ----- Fix the RAM address and save the byte
	strb	eax, [r2]				; Store byte to ES:DI
	; ----- Increment/decrement RAM address and DI register
	tst		r0, #0x08000000			; Check the "Direction" bit
	addeq	edi, #1					; Fix logical DI, direction = UP.
	addeq	r2, #1
	subne	edi, #1					; Fix logical DI, direction = DOWN.
	subne	r2, #1
	; ----- Back to loop if not yet all done
	subs	ecx, #1					; CX--
	bne		%b1
	mov		eax, r3					; Restore EAX value
	and		r0, #0xF0000000			; r0 highest 4 bits == saved flags
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

; ------------------- E4 = IN AL,imm8 ---------------------------------
;
op_e4_in_al_imm8
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e4_prot_r0			; Yes we are, go handle protected mode IN AL,imm8!
	GLOBAL	op_e4_real_r0
op_e4_real_r0
	ldrb	r2,[r12],#1				; r2 = port number
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e4_prot_r0
	
; ------------------- E5 = IN AX,imm8 ---------------------------------
;
op_e5_in_ax_imm8
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e5_prot_r0			; Yes we are, go handle protected mode IN AL,DX!
	GLOBAL	op_e5_real_r0
op_e5_real_r0
	ldrb	r2,[r12]				; r2 = port number
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler (we must have exactly two words pushed to stack!)
	ldrb	r2,[r12], #1			; r2 = port number
	ror		eax, #8					; Save the read AL value into highest byte, prepare for reading AH value
	add		r2, #1
	bl		in_byte_r2				; Call the IN handler for port+1 (we must have exactly two words pushed to stack!)
	ror		eax, #24				; Rotate the AL and AH to their proper places.
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e5_prot_r0
	
; ------------------- E5 = IN EAX,imm8 ---------------------------------
;
	GLOBAL	op_e5_in_eax_imm8
op_e5_in_eax_imm8
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_e5_USE32_prot_r0		; Yes we are, go handle protected mode port input!
	GLOBAL	op_e5_USE32_real_r0
op_e5_USE32_real_r0
	ldrb	r2,[r12]				; r2 = port number
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler (we must have exactly two words pushed to stack!)
	ldrb	r2,[r12]				; r2 = port number
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #1
	bl		in_byte_r2				; Call the IN handler for port+1
	ldrb	r2,[r12]				; r2 = port number
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #2
	bl		in_byte_r2				; Call the IN handler for port+2
	ldrb	r2,[r12], #1			; r2 = port number, r12 incremented for next opcode
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #3
	bl		in_byte_r2				; Call the IN handler for port+3
	ror		eax, #8					; eax = final result value
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_e5_USE32_prot_r0
	
; ------------------- EC = IN AL,DX -----------------------------------
	GLOBAL	op_ec_in_al_dx
op_ec_in_al_dx
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ec_prot_r0			; Yes we are, go handle protected mode IN AL,DX!
	GLOBAL	op_ec_real_r0
op_ec_real_r0
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ec_prot_r0
	
; ------------------- ED = IN AX,DX -----------------------------------
	GLOBAL	op_ed_in_ax_dx
op_ed_in_ax_dx
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ed_prot_r0			; Yes we are, go handle protected mode IN AX,DX!
	GLOBAL	op_ed_real_r0
op_ed_real_r0
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler (we must have exactly two words pushed to stack!)
	ror		eax, #8					; Save the read AL value into highest byte, prepare for reading AH value
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	add		r2, #1
	bl		in_byte_r2				; Call the IN handler for port+1
	ror		eax, #24				; Rotate the AL and AH to their proper places.
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ed_prot_r0
	
; ------------------- ED = IN EAX,DX -----------------------------------
	GLOBAL	op_ed_in_eax_dx
op_ed_in_eax_dx
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_ed_USE32_prot_r0		; Yes we are, go handle protected mode IN AX,DX!
	GLOBAL	op_ed_USE32_real_r0
op_ed_USE32_real_r0
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	push	{r0, lr}
	bl		in_byte_r2				; Call the IN handler (we must have exactly two words pushed to stack!)
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #1
	bl		in_byte_r2				; Call the IN handler for port+1
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #2
	bl		in_byte_r2				; Call the IN handler for port+2
	mov		r2, edx, lsl #16		; r2 = DX value = port number
	lsr		r2, #16
	ror		eax, #8					; Prepare for reading the next byte
	add		r2, #3
	bl		in_byte_r2				; Call the IN handler for port+3
	ror		eax, #8					; eax = final result value
	pop		{r0, lr}
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags.

	EXTERN	op_ed_USE32_prot_r0

; ------------------- IN BYTE Handler ---------------------------------
; On input
;	r0 = saved flags (= free, flags already pushed to stack)
;	r1 = free
;	r2 = port address (DX value)
;	lr = return address
; NOTE! We must have exactly two words pushed into stack!
;
in_byte_r2
	cmp		r2, #0x400
	bge		in_ignore				; No port above 0x400 is supported!
	ldr		r0, =in_byte_jump
	mov		r1, r2, lsr #4
	and		r2, #0x0F
	ldr		pc,[r0, r1, lsl #2]		; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
in_byte_jump
	;-------
	; Each handler handles a 16-port range. Register r2 tells which of the 16 ports in the block should we input from.
	;-------
; 0
	DCD in_000_00F, bad_in_port, in_020_02F, bad_in_port, in_040_04F, bad_in_port, in_060_06F, in_070_07F
; 0x80
	DCD in_080_08F, in_090_09F, in_ignore, bad_in_port, bad_in_port, bad_in_port, in_ignore, bad_in_port
; 0x100
	DCD bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port
; 0x180
	DCD in_ignore, in_ignore, bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port, bad_in_port
; 0x200
	DCD in_200_20F, in_ignore, in_220_22F, in_ignore, in_ignore, in_ignore, in_ignore, in_ignore
; 0x280
	DCD in_ignore, in_ignore, in_ignore, in_ignore, in_ignore, in_ignore, in_ignore, in_ignore
; 0x300
	DCD in_ignore, in_ignore, in_ignore, in_330_33F, in_ignore, in_ignore, in_ignore, in_ignore
; 0x380
	DCD in_380_38F, in_ignore, in_ignore, in_ignore, in_3C0_3CF, in_3D0_3DF, in_ignore, in_ignore

	AREA ports, CODE, READONLY
	ALIGN	4

	;=======
	; DMA Controller ports. All DMA commands are sent to ARM7 (for SB DMA handling).
	;=======
in_000_00F
	cmp		r2, #2					; Port 0x02?
	beq		in_002
	biclt	eax, #0xFF				; Port 0x01, clear current AL value ...
	bxlt	lr						; ... and return
	cmp		r2, #3
	bgt		in_DMA_port
	;-------
	; IN AL,03 = Get current DMA transfer remaining count.
	; This is calculated as: DMALength-((DMACurrent-(INTVectors+DMAAddress))&0xFFFF)
	;-------
	ldr		r2, =DMAAddress
	ldr		r1, =INTVectors
	ldr		r0, [r2]
	ldr		r1, [r1]
	add		r0, r1
	ldr		r1, [r2, #(DMACurrent-DMAAddress)]
	sub		r1, r0					; r1 = (DMACurrent-(INTVectors+DMAAddress))
	ldr		r0, [r2, #(DMALength-DMAAddress)]
	sub		r0, r1					; r0 = DMALength-(DMACurrent-(INTVectors+DMAAddress))
	ldrb	r1, [r2, #(DMAFlipFlop-DMAAddress)]
	eors	r1, #1					; Reverse flipflop value
	strb	r1, [r2, #(DMAFlipFlop-DMAAddress)]
	lsreq	r0, #8
	bfi		eax, r0, #0, #8			; Put the value into AL
	bx		lr
	;-------
	; IN AL,02 = Get current DMA transfer address.
	; This is calculated as: (DMACurrent-INTVectors)&0xFFFF
	;-------
in_002
	ldr		r2, =DMACurrent
	ldr		r1, =INTVectors
	ldr		r0, [r2]
	ldr		r1, [r1]
	sub		r0, r1					; r0 = DMACurrent-INTVectors
	ldrb	r1, [r2, #(DMAFlipFlop-DMACurrent)]
	eors	r1, #1					; Reverse flipflop value
	strb	r1, [r2, #(DMAFlipFlop-DMACurrent)]
	lsreq	r0, #8
	bfi		eax, r0, #0, #8			; Put the value into AL
	bx		lr
in_DMA_port
	cmp		r2, #0x08
	bne		in_ignore
	;-------
	; IN AL,08 = Status register
	;-------
	ldr		r2, =DMAAddress
	ldr		r1, =INTVectors
	ldr		r0, [r2]
	ldr		r1, [r1]
	add		r0, r1
	ldr		r1, [r2, #(DMACurrent-DMAAddress)]
	sub		r1, r0					; r1 = (DMACurrent-(INTVectors+DMAAddress))
	ldr		r0, [r2, #(DMALength-DMAAddress)]
	subs	r0, r1					; r0 = DMALength-(DMACurrent-(INTVectors+DMAAddress))
	bic		eax, #0xFF				; Clear current AL value
	orreq	eax, #0x02				; Set bit for DMA channel 1 tcount if remaining count = 0
	bx		lr
	
	;=======
	; Interrupt Controller 1 Ports (handled in "pic.s")
	;=======
in_020_02F
	cmp		r2, #1					; Port 0x21?
	beq		in_al_21
	blt		in_al_20
	b		bad_in_port
	
	;=======
	; Timer Ports (handled in "timer.c")
	;=======
in_040_04F
	cmp		r2, #0
	beq		in_al_40_j
	cmp		r2, #2
	beq		in_al_42_j
	b		in_ignore				; Ignore other timer port input
in_al_40_j
	push	{r12, lr}
	bl		in_al_40				; Call the C routine
	pop		{r12, lr}
	bfi		eax, r0, #0, #8			; Put the value into AL
	bx		lr
in_al_42_j
	push	{r12, lr}
	bl		in_al_42				; Call the C routine
	pop		{r12, lr}
	bfi		eax, r0, #0, #8			; Put the value into AL
	bx		lr

	EXTERN	in_al_40
	EXTERN	in_al_42

	;=======
	; Keyboard controller ports
	;=======
in_060_06F
	cmp		r2, #1
	blt		in_al_60
	beq		in_al_61
	cmp		r2, #4
	bne		in_ignore				; Ignore other keyboard ports (like 0x62)
	;-------
	; IN AL,64 = Keyboard Status Byte
	;	bit 0 = Output Buffer Status (1=full,0=empty)
	;	bit 1 = Input Buffer Status (1=full, 0=empty)
	;	bit 2 = System Flag (1=self test passed, 0=failed)
	;	bit 3 = Command/Data available (0=data available at port 60h, 1=command available at port 64h)
	;	bit 4 = Keyboard Active (1=enabled, 0=disabled)
	;	bit 5 = Error detected (1=error in transmission, 0=no error)
	;	bit 6 = Timeout Error (1=keyboard timed out, 0=no error)
	;	bit 7 = Parity error (1=parity error in transmission, 0=no error)
	;-------
	ldr		r2,=KeyboardStatusByte
	ldrb	r1, [r2]
	bic		eax, #0xFF				; Clear current AL value
	orr		eax, r1					; Put keyboard status byte into AL
	bx		lr
	;-------
	; IN AL,60 = Keyboard to System Transmissions
	;	00 Data overrun. System sends a zero byte as the last value when the keyboard controllers internal buffer overflows. 
	;	1..58
	;	81..D8 Scan codes for key presses. The positive values are down codes, the negative values (H.O. bit set) are up codes. 
	;	83AB Keyboard ID code returned in response to the F2 command (PS/2 only). 
	;	AA Returned during basic assurance test after reset. Also the up code for the left shift key. 
	;	EE Returned by the ECHO command. 
	;	F0 Prefix to certain up codes (N/A on PS/2). 
	;	FA Keyboard acknowledge to keyboard commands other than resend or ECHO. 
	;	FC Basic assurance test failed (PS/2 only). 
	;	FD Diagnostic failure (not available on PS/2). 
	;	FE Resend. Keyboard requests the system to resend the last command. 
	;	FF Key error (PS/2 only). 
	;-------
in_al_60							; Keyboard data port
	ldr		r2,=KeyboardDataByte
	ldrb	r1, [r2]
	bic		eax, #0xFF				; Clear current AL value
	orr		eax, r1					; Put keyboard status byte into AL
	ldrb	r1, [r2, #(KeyboardStatusByte-KeyboardDataByte)]	; Get the current status byte
	and		r1, #0xFE				; Clear "Buffer Full" bit when the data is read (Fixes Little Big Adventure)
	strb	r1, [r2, #(KeyboardStatusByte-KeyboardDataByte)]	; Set the new status byte
	bx		lr
	
	;-------
	; IN AL,61 = Keyboard/Interrupt stuff
	;-------
in_al_61							; Keyboard/Interrupt handshake port
	ldr		r2,=Port61Data
	ldrb	r1, [r2]
	bic		eax, #0xFF				; Clear current AL value
	eor		r1, #0x30				; port_61_data^=0x20; port_61_data^=0x10;
	orr		eax, r1					; Put keyboard status byte into AL
	strb	r1, [r2]				; Save the new Port61Data
	bx		lr

	;=======
	; CMOS RTC, NMI Mask
	;=======
in_070_07F
	cmp		r2, #1
	bne		bad_in_port
	;-------
	; IN AL,71 = CMOS Data
	;-------
	ldr		r2,=cmos_regs
	ldrb	r0, [r2, #-1]			; r0 = cmos_reg value
	cmp		r0, #0x3F
	bgt		%f1
	ldrb	r1, [r2, r0]			; Load the register value
	bic		eax, #0xFF				; Clear current AL value
	orr		eax, r1
	bx		lr
1	orr		eax, #0xFF				; Invalid port!
	bx		lr

	;=======
	; DMA Page Register Ports
	;=======
in_080_08F
	ldr		r0, =DMAAddress
	bic		eax, #0xFF				; Clear current AL value
	ldrb	r1, [r0, #2]			; Get the page number of the DMAAddress
	cmp		r2, #3					; Port 0x83?
	orreq	eax, r1					; Yes, set the page number to AL
	bx		lr

in_090_09F
	cmp		r2, #2
	bne		bad_in_port
	;-------
	; IN AL,92 = System Control Port A
	;-------
	ldr		r2, =Port92Data
	bic		eax, #0xFF
	ldrb	r0, [r2]
	orr		eax, r0					; Set the other bits besides A20 gate bit
	push	{r12, lr}
	bl		QueryA20Gate			; Returns r0 = 0 or 1
	orr		eax, r0, lsl #1			; Set the A20 gate bit
	pop		{r12, pc}

	EXTERN	QueryA20Gate
	
	;=======
	; Joystick port, read values.
	; Bit mapped output: 4321PZYX
	; Bit
	;=======
in_200_20F
	ldr		r2, =joy_counters
	bic		eax, #0xFF
	ldrb	r0, [r2, #(JoyButtons-joy_counters)]	; r0 = button values
	ldr		r1, [r2]				; r1 = joystick counter values
	tst		r0, #1					; Is r0 lowest bit set?
	beq		in_ignore				; Jump if no joystick
	;-------
	; We have a joystick, set buttons and decrement axis counters until all zero.
	;-------
	orr		eax, r0					; Now button values are correct, all axis bits set (not timed out yet)
	tst		r1, #0xFF				; X axis timed out?
	biceq	eax, #1					; Yes, so clear the bit
	subne	r1, #1					; Nope, so decrement counter
	tst		r1, #0xFF00				; Y axis timed out?
	biceq	eax, #2					; Yes, so clear the bit
	subne	r1, #0x100				; Nope, so decrement counter
	tst		r1, #0xFF0000			; Z axis timed out?
	biceq	eax, #4					; Yes, so clear the bit
	subne	r1, #0x10000			; Nope, so decrement counter
	tst		r1, #0xFF000000			; Pedal axis timed out?
	biceq	eax, #8					; Yes, so clear the bit
	subne	r1, #0x1000000			; Nope, so decrement counter
	str		r1, [r2]				; Save the new counter values
	bx		lr						; Return

in_ignore
	orr		eax, #0xFF				; AL = 0xFF
	bx		lr

	;=======
	; SoundBlaster ports (handled in "ports_SB.s")
	;=======
in_220_22F
	cmp		r2, #0xA				; SoundBlaster READ DATA port?
	beq		in_22A_SB_read_data
	cmp		r2, #0xE				; SoundBlaster DATA AVAILABLE port?
	beq		in_22E_SB_data_avail
	cmp		r2, #0x8				; SoundBlaster FM Audio port?
	beq		in_228_388_FM_status
	cmp		r2, #0xC				; SoundBlaster WRITE BUFFER STATUS port?
	beq		in_22C_SB_DSP_status
	orr		eax, #0xFF				; AL = 0xFF
	bx		lr						; Ignore input from other SB ports

	EXTERN	in_22A_SB_read_data
	EXTERN	in_22E_SB_data_avail
	EXTERN	in_228_388_FM_status
	EXTERN	in_22C_SB_DSP_status
	
	;=======
	; Roland MPU 401 ports
	;=======
in_330_33F
	orr		eax, #0xFF
	bx		lr

	;=======
	; AdLib ports (handled in "ports_SB.s")
	;=======
in_380_38F
	cmp		r2, #8					; Port 0x388?
	beq		in_228_388_FM_status
	orr		eax, #0xFF				; AL = 0xFF
	bx		lr						; Ignore input from other AdLib ports

	EXTERN	in_228_388_FM_status
	
	;=======
	; Color EGA/VGA ports (handled in "EGA.s")
	;=======
in_3C0_3CF
	ldr		r0, =in_3C0_jump
	ldr		pc,[r0, r2, lsl #2]		; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
in_3C0_jump
	DCD	in_3C0_VGA_attr, in_3C1_VGA_attr, in_3CC_VGA_misc, bad_in_port
	DCD	in_3C4_VGA_sequencer, in_3C5_VGA_sequencer, in_ignore, in_3C7_VGA_DAC
	DCD	in_3C8_VGA_pel, in_3C9_VGA_pel, bad_in_port, bad_in_port
	DCD	in_3CC_VGA_misc, in_ignore, in_3CE_VGA_graphics, in_3CF_VGA_graphics

	AREA ports, CODE, READONLY
	ALIGN	4

	EXTERN	in_3C0_VGA_attr
	EXTERN	in_3C1_VGA_attr
	EXTERN	in_3CC_VGA_misc
	EXTERN	in_3C4_VGA_sequencer
	EXTERN	in_3C5_VGA_sequencer
	EXTERN	in_3C7_VGA_DAC
	EXTERN	in_3C8_VGA_pel
	EXTERN	in_3C9_VGA_pel
	EXTERN	in_3CC_VGA_misc
	EXTERN	in_3CE_VGA_graphics
	EXTERN	in_3CF_VGA_graphics

	;=======
	; Color EGA/VGA ports (handled in "EGA.s")
	;=======
in_3D0_3DF
	ldr		r0, =in_3D0_jump
	ldr		pc,[r0, r2, lsl #2]		; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
in_3D0_jump
	DCD	bad_in_port, bad_in_port, bad_in_port, bad_in_port
	DCD	in_3D4_VGA_CRTC_addr, in_3D5_VGA_CRTC_data, bad_in_port, bad_in_port
	DCD	bad_in_port, in_3D9_CGA_Color, in_3DA, in_ignore			; "Silpheed" reads from port 3DB!
	DCD	bad_in_port, bad_in_port, in_ignore, in_ignore

	AREA ports, CODE, READONLY
	ALIGN	4

in_3DA
	push	{r12, lr}
	mov		r0, r12					; First parameter = pointer to next opcode
	bl		in_3DA_status			; Call the C routine
	pop		{r12, lr}
	bic		eax, #0xFF				; Clear current AL value
	and		r0, #0xFF				; r0 == low byte of value
	orr		eax, r0					; Put low byte of value into AL
	bx		lr

	EXTERN	in_3D4_VGA_CRTC_addr
	EXTERN	in_3D5_VGA_CRTC_data
	EXTERN	in_3D9_CGA_Color
	EXTERN	in_3DA_status

	
	AREA ports_data, DATA, READWRITE

	ALIGN	4

	GLOBAL	VGA_out_3C9_addr
VGA_out_3C9_addr					; This can not be in text segment in Android!
	DCD	out_3C9_VGA_pel	

	EXTERN	out_3C9_VGA_pel
	
	GLOBAL	DMAAddress				; Used also by ports_SB.s
DMAAddress
	DCD	0
	GLOBAL	DMALength
DMALength							; Must immediately follow DMAAddress!
	DCD	0
	GLOBAL	DMACurrent
DMACurrent							; Must follow DMALength! Updated by FIFO messages from ARM7!
	DCD	0
	GLOBAL	DMAFlipFlop				; For serialization
DMAFlipFlop
	DCB	0

	;-------
	; bit 	 7: must be zero
	; bits 5,6: Delay. 00- 1/4 sec, 01- 1/2 sec, 10- 3/4 sec, 11- 1 sec.
	; bits 0-4: Repeat rate. 0- approx 30 chars/sec to 1Fh- approx 2 chars/sec. 
	;-------
	GLOBAL	RepeatRate
RepeatRate							; Key repeat rate programmed to port 0x60
	DCB	0x21

Port60Command						; Commands written to port 0x60
	DCB	0
	
	GLOBAL	Port61Data
Port61Data
	DCB	0
	
	GLOBAL KeyboardStatusByte
KeyboardStatusByte					; Status port, read from port 64h
	DCB	0
	
KeyboardCommandByte					; Command port
	DCB	0
	
	GLOBAL KeyboardDataByte
KeyboardDataByte					; keyboard data, read from port 60h
	DCB	0

	;------
	; This port has a number of functions, and the details depend on the manufacturer.
	; Bits 0,1,3,6,7 seem to have the same meaning everywhere this port is implemented. 
	; Bit 0 (w): writing 1 to this bit causes a fast reset (used to switch back to real mode; for MCA this took 13.4 ms). 
	; Bit 1 (rw): 0: disable A20, 1: enable A20. 
	; Bit 3 (rw?): 0/1: power-on password bytes (stored in CMOS bytes 0x38-0x3f or 0x36-0x3f) accessible/inaccessible.
	;		 This bit can be written to only when it is 0. 
	; Bits 6-7 (rw): 00: hard disk activity LED off, 01,10,11: hard disk activity LED on. 
	; Bits 2,4,5 are unused or have varying meanings. (On MCA bit 4 (r): 1: watchdog timeout occurred.) 
	;------
	GLOBAL	Port92Data
Port92Data
	DCB	0xC0

	ALIGN	4
	
joy_counters						; Counters decremented in port 201 read, set in port 201 write
	SPACE	4
	GLOBAL	JoyValues
JoyValues							; Actual values of the joystick axels (0..255), set in read_keyboard() routine
	SPACE	4
	GLOBAL	JoyButtons
JoyButtons							; Button values (in high 4 bits), set in read_keyboard() routine.
	DCB	0						; Lowest bit set if we have a joystick.

cmos_nmi							; Must be at cmos_reg-1
	DCB 	0
cmos_reg
	DCB	0
	GLOBAL	cmos_regs
cmos_regs
	SPACE	0x40
	

BRUnsPORT
	DCB	"Unsupported port I/O!"
	DCB	0x0a, 0

	END
