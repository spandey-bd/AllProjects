;=============================================================================
; macros.inc
;
; This is an include file containing various general-purpose macros used by all
; the assembler source modules.
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

	MACRO
	modrm_jump_16_tbl $tbl 
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =$tbl
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
$tbl	
	MEND

	MACRO
	modrm_jump_32_tbl $tbl
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =$tbl
	mvn		r3, #0								; Use 32-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
$tbl	
	MEND

; ---- Common effective address calculation macros ----

	MACRO
	r0_from_disp16
	IF USE_UNALIGNED = 1
	ldrh	r0, [r12], #2
	ELSE
	ldrb	r0,[r12],#1				; Load low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r1, increment r12 by 1
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	ENDIF
	MEND

	MACRO
	r0_from_disp32
	IF USE_UNALIGNED = 1
	ldr		r0, [r12], #4
	ELSE
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	orr		r0, r1, lsl #8
	ldrb	r1,[r12],#1
	orr		r0, r1, lsl #16
	ldrb	r1,[r12],#1
	orr		r0, r1, lsl #24
	ENDIF
	MEND
	MACRO
	get_cseip_dword $reg
	IF USE_UNALIGNED = 1
	ldr		$reg, [r12], #4
	ELSE
	ldrb	$reg,[r12],#1
	ldrb	r1,[r12],#1
	orr		$reg, r1, lsl #8
	ldrb	r1,[r12],#1
	orr		$reg, r1, lsl #16
	ldrb	r1,[r12],#1
	orr		$reg, r1, lsl #24
	ENDIF
	MEND

	MACRO
	r0_from_idx_disp8 $idx
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	add		r0, $idx				; r0 = (idx register + signed offset) << 16
	MEND

	MACRO
	r0_from_idx_disp16 $idx
	IF USE_UNALIGNED = 1
	ldrh	r0, [r12], #2
	add		r0, $idx
	ELSE
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	add		r0, $idx
	add		r0, r1, lsl #8
	ENDIF
	MEND

	MACRO
	r0_from_idx_disp32 $idx
	IF USE_UNALIGNED = 1
	ldr		r0, [r12], #4
	add		r0, $idx
	ELSE
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	add		r0, $idx
	add		r0, r1, lsl #8
	ldrb	r1,[r12],#1
	add		r0, r1, lsl #16
	ldrb	r1,[r12],#1
	add		r0, r1, lsl #24
	ENDIF
	MEND

	MACRO
	r0_from_bxidxdisp8 $idx
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	add		r1, r7, $idx			; r1 = BX+idx
	add		r0, r1					; r0 = (BX+idx+signed offset)
	MEND

	MACRO
	r0_from_bxidxdisp16 $idx
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	add		r0, $idx
	add		r0, r1, lsl #8
	add		r0, r7
	MEND

	MACRO
	r0_from_bpidxdisp8 $idx
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	add		r1, r9, $idx			; r1 = BP+idx
	add		r0, r1					; r0 = (BP+idx+signed offset)
	MEND

	MACRO
	r0_from_bpidxdisp16 $idx
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	add		r0, $idx
	add		r0, r1, lsl #8
	add		r0, r9
	MEND

;------------------------------------------------------------------------
; PUSH/POP macros
;------------------------------------------------------------------------

	MACRO
	push_hword $reg, $tmp, $tmp2
	ldr		$tmp, [sp, #SP_SP_MASK]
	ldr		$tmp2, [sp, #SP_PHYS_SS]
	sub		esp, #2								; Note! Stack overflow not handled properly!
	IF USE_UNALIGNED = 1
	and		$tmp, esp
	strh	$reg, [$tmp2, $tmp]
	ELSE
	ror		$reg, #8
	and		$tmp, esp
	add		$tmp, $tmp2							; Now $tmp = physical new stack address
	strb	$reg, [$tmp, #1]
	ror		$reg, #24
	strb	$reg, [$tmp]
	ENDIF
	MEND

	MACRO
	push_dword $reg, $tmp, $tmp2
	ldr		$tmp, [sp, #SP_SP_MASK]
	ldr		$tmp2, [sp, #SP_PHYS_SS]
	sub		esp, #4								; Note! Stack overflow not handled properly!
	IF USE_UNALIGNED = 1
	and		$tmp, esp
	str		$reg, [$tmp2, $tmp]
	ELSE
	ror		$reg, #24
	and		$tmp, esp
	add		$tmp, $tmp2							; Now $tmp = physical new stack address
	strb	$reg, [$tmp, #3]
	ror		$reg, #24
	strb	$reg, [$tmp, #2]
	ror		$reg, #24
	strb	$reg, [$tmp, #1]
	ror		$reg, #24
	strb	$reg, [$tmp]
	ENDIF
	MEND

	MACRO
	push_low_hi $low, $hi, $tmp, $tmp2
	ldr		$tmp, [sp, #SP_SP_MASK]
	ldr		$tmp2, [sp, #SP_PHYS_SS]
	sub		esp, #2								; Note! Stack overflow not handled properly!
	and		$tmp, esp
	add		$tmp, $tmp2							; Now $tmp = physical new stack address
	strb	$low, [$tmp]
	strb	$hi, [$tmp, #1]
	MEND

	MACRO
	pop_reg_low_tmp $reg, $tmp
	ldr		$tmp, [sp, #SP_SP_MASK]
	ldr		$reg, [sp, #SP_PHYS_SS]
	and		$tmp, esp
	IF USE_UNALIGNED = 1
	add		esp, #2								; Note! Stack overflow not handled properly!
	ldrh	$reg, [$reg, $tmp]
	ELSE
	add		$tmp, $reg							; Now $tmp = physical new stack address
	ldrb	$reg, [$tmp]						; Pop the low byte
	ldrb	$tmp, [$tmp, #1]					; Pop the high byte
	add		esp, #2								; Note! Stack overflow not handled properly!
	orr		$reg, $tmp, lsl #8
	ENDIF
	MEND

	MACRO
	pop_dword $reg, $tmp, $tmp2
	ldr		$tmp, [sp, #SP_SP_MASK]
	ldr		$tmp2, [sp, #SP_PHYS_SS]
	and		$tmp, esp
	IF USE_UNALIGNED = 1
	add		esp, #4								; Note! Stack overflow not handled properly!
	ldr		$reg, [$tmp2, $tmp]
	ELSE
	add		$tmp, $tmp2							; Now $tmp = physical new stack address
	ldrb	$reg, [$tmp]
	ldrb	$tmp2, [$tmp, #1]
	orr		$reg, $tmp2, lsl #8
	ldrb	$tmp2, [$tmp, #2]
	orr		$reg, $tmp2, lsl #16
	ldrb	$tmp, [$tmp, #3]
	add		esp, #4								; Note! Stack overflow not handled properly!
	orr		$reg, $tmp, lsl #24
	ENDIF
	MEND

	MACRO
	pop_reg_low_hi $lo, $hi
	ldr		$hi, [sp, #SP_SP_MASK]
	ldr		$lo, [sp, #SP_PHYS_SS]
	and		$hi, esp
	add		$hi, $lo							; Now $hi = physical new stack address
	ldrb	$lo, [$hi]							; Pop the low byte
	ldrb	$hi, [$hi, #1]						; Pop the high byte
	add		esp, #2								; Adjust the SP. Note! Stack overflow not handled properly! 
	MEND

;------------------------------------------------------------------------
; Convert flags from ARM to x86 bit positions, and push them to x86 stack
; ARM Flags bits:
; 	bit 0x80000000 = Negative Flag
;	bit 0x40000000 = Zero Flag
;	bit 0x20000000 = Carry flag
;	bit	0x10000000 = Overflow Flag
;
;	FLAG_TF			= (1<<8)
;	FLAG_IF			= (1<<9)
;	FLAG_DF			= (1<<10)
;
; Intel x86 FLAGS Register
;	Bit #	Abbreviation	Description						Category[1]
;	0		CF				Carry flag						S
;	1		1				Reserved	 
;	2		PF				Parity flag						S
;	3		0				Reserved	 
;	4		AF				Adjust flag						S
;	5		0				Reserved	 
;	6		ZF				Zero flag						S
;	7		SF				Sign flag						S
;
;	8		TF				Trap flag (single step)			X
;	9		IF				Interrupt enable flag			X
;	10		DF				Direction flag					C
;	11		OF				Overflow flag					S
;	12, 13	IOPL			I/O privilege level (286+)		X
;	14		NT				Nested task flag (286+ only)	X
;	15		0				Reserved	 
;------------------------------------------------------------------------
	MACRO
	join_ARM_to_X86_flags $reg
	bic		$reg, #(FLAG_CF+FLAG_ZF+FLAG_SF+(1<<3)+(1<<5))
	bic		$reg, #(FLAG_OF+(1<<15))
	orr		$reg, #(1<<1)
	orrcs	$reg, #FLAG_CF
	orreq	$reg, #FLAG_ZF
	orrmi	$reg, #FLAG_SF
	orrvs	$reg, #FLAG_OF
	MEND

	MACRO
	push_flags_16 $reg, $tmp, $tmp2
	ldr		$reg,[sp, #SP_FLAGS]	; Get the "Trap", "Interrupts Enabled" and "Direction" flags
	join_ARM_to_X86_flags $reg
	push_hword $reg, $tmp, $tmp2
	MEND

	MACRO
	popped_flags_to_ARM $reg
	orr		$reg, $reg, lsl #24		; Now Sign and Zero are OK, other ARM flags are undefined.
	bic		$reg, #(ARM_CARRY:OR:ARM_OVER:OR:0x08000000)	; Win 3.1 destroys Carry and Overflow flags!
	tst		$reg, #FLAG_CF			; Carry set?
	orrne	$reg, #ARM_CARRY
	tst		$reg, #FLAG_OF			; Overflow set?
	orrne	$reg, #ARM_OVER
	MEND

;------------------------------------------------------------------------
; Memory address calculation macros
;------------------------------------------------------------------------

; Note! By default BP accesses the SS segment, unless segment override is in effect.
; Thus, we need to determine if segment override is in effect, and use
;   r2 = physical segment override:0000 if it is,
;   r2 = physical SS:0000 if it is not.
;
	MACRO
	mem_handler_bp
	mov		r2, lr								; Use BP-relative segment base
	MEND

	MACRO
	calc_linear_address_r2
	mov		r0, r2, lsr #TLB_GRAN_BITS			; r0 = 16K page number
	add		r0, #(SP_TLBTABLE>>2)				; r0 = index into TLB table in stack
	ldr		r0,[sp, r0, lsl #2]					; r0 = physical start address of the 16K page
	add		r2, r0, lsl #4						; r2 = physical linear address
	MEND
	MACRO
	calc_linear_address_r2_from_r0r3
	and		r0, r3
	add		r2, r0								; r2 = full logical linear memory address
	calc_linear_address_r2	
	MEND

	MACRO
	jump_to_r2_handler $ram, $ega, $modex
	;-------
	; On input:
	;	r2 = physical memory address (with EGA/MODEX flags if applicable)
	; Destroys:
	;	r0
	;-------
	lsr		r0, #30								; Leave only the two highest bits to r0
	cbz		r0, $ram							; Jump to $ram if r0 == 0 ...
	IF "$ega" = "$modex"
	;-------
	; We only have two options, RAM or not RAM. Use the cbnz to jump to the RAM handler,
	; otherwise jump to the non-RAM handler.
	;-------
	b		$ega								; ... else jump to the non-RAM handler.
	ELSE
	sub		r0, #1								; Is it EGA (1) or MODEX (2)?
	cbz		r0, %f77							; It is EGA
	b		$modex								; Jump to MODEX handler
77	b		$ega								; Jump to EGA handler
	ENDIF
	MEND

	MACRO
	jump_to_r2_handler_far $ram, $ega, $modex
	;-------
	; On input:
	;	r2 = physical memory address (with EGA/MODEX flags if applicable)
	; Destroys:
	;	r0
	;-------
	lsr		r0, #30								; Leave only the two highest bits to r0
	cbnz	r0, %f76
	b		$ram								; Jump to $ram if r0 == 0 ...
76
	IF "$ega" = "$modex"
	;-------
	; We only have two options, RAM or not RAM. Use the cbnz to jump to the RAM handler,
	; otherwise jump to the non-RAM handler.
	;-------
	b		$ega								; ... else jump to the non-RAM handler.
	ELSE
	sub		r0, #1								; Is it EGA (1) or MODEX (2)?
	cbz		r0, %f77							; It is EGA
	b		$modex								; Jump to MODEX handler
77	b		$ega								; Jump to EGA handler
	ENDIF
	MEND

	MACRO
	mem_handler_jump_r0r3 $ram, $ega, $modex
	;-------
	; On input:
	; 	r0 = offset within the segment
	;	r2 = segment base physical address
	;	r3 = address masking mode, either 0x0000FFFF (16-bit addressing) or 0xFFFFFFFF (32-bit addressing)
	;	NOTE! Nothing may have been pushed into stack before this!
	; Output:
	;	r2 = physical memory address (with EGA/MODEX flags if applicable)
	; Destroys:
	;	r0
	;-------
	calc_linear_address_r2_from_r0r3
	jump_to_r2_handler $ram, $ega, $modex
	MEND

	MACRO
	mem_handler_jump_r0r3_far $ram, $ega, $modex
	calc_linear_address_r2_from_r0r3
	jump_to_r2_handler_far $ram, $ega, $modex
	MEND

	MACRO
	mem_handler_jump_r0r3_ES $ram, $ega, $modex
	ldr		r2, [sp, #SP_ES_BASE]	; r2 = current effective logical ES segment
	mem_handler_jump_r0r3 $ram, $ega, $modex
	MEND

	MACRO
	mem_handler_jump_r0r3_ES_far $ram, $ega, $modex
	ldr		r2, [sp, #SP_ES_BASE]	; r2 = current effective logical ES segment
	mem_handler_jump_r0r3_far $ram, $ega, $modex
	MEND

;============================
; Prot Mode segment handling 
;============================

	MACRO
	GetDescriptor_r2 $reg, $overlimit
	push	{r0, r3}
	mov		r3, $reg							; Put the input register (segment selector) to r3, just in case...
	ldr		r2, =cpu_gdt_base
	and		r0, r3, #4
	add		r2, r0								; r2 = Pointer to GDT/LDT base variable
	ldrh	r0, [r2, #8]						; r0 = GDT/LDT limit value
	cmp		r0, r3								; Is the limit less than the selector?
	poplt	{r0, r3}
	blt		$overlimit							; GP fault if it is!
	ldr		r2, [r2, #16]						; r2 = GDT/LDT physical base address
	bic		r0, r3, #0x7						; and r0, r3, #0xFFF8
	add		r2, r0								; r2 = physical pointer to the descriptor
	pop		{r0, r3}
	MEND

	MACRO
	GetDescriptor_r3_from_r2_destroy_r0 $overlimit
	ldr		r3, =cpu_gdt_base
	and		r0, r2, #4
	add		r3, r0								; r3 = Pointer to GDT/LDT base variable
	ldrh	r0, [r3, #8]						; r0 = GDT/LDT limit value
	cmp		r0, r2								; Is the limit less than the selector?
	blt		$overlimit							; GP fault if it is!
	ldr		r3, [r3, #16]						; r3 = GDT/LDT physical base address
	bic		r0, r2, #0x7						; and r0, r3, #0xFFF8
	add		r3, r0								; r3 = physical pointer to the descriptor
	MEND

	MACRO
	fix_opcode_table_destroy_flags
	push	{r1-r11}
	ldrb	r1, [sp, #SP_CPU_BIG+(11*4)]
	ldr		r2, =cpu_big
	tst		r1, #0x40							; Are we in cpu_code_big mode?
	strb	r1, [r2]							; Save the changed cpu_code_big to global memory
	;-------
	; Copy the proper (no-prefix) opcode table to stack
	;-------
	mov		r1, #(256/8)
	ldreq	r2, =opcodetable_16_16				; Use 16-bit opcode table as default
	ldrne	r2, =opcodetable_32_32				; Use 32-bit opcode table as default
	add		r3, sp, #(11*4)						; r3 points to the opcode table in stack
1	ldmia	r2!,{r4-r11}						; 8 registers = 8 words at a time
	subs	r1, #1
	stmia	r3!,{r4-r11}
	bne		%b1
	pop		{r1-r11}
	MEND

;============================
; Miscellaneous macros
;============================


	MACRO
	debugreg $reg
	push	{r0-r12, lr}
	mov		r0, $reg
	mov		r1, #0
	mrs		r5,cpsr					; Save flags to r5
	bl		printreg
	msr		cpsr_f,r5				; Restore flags from r5
	pop		{r0-r12, lr}
	MEND

	MACRO
	debugreg2 $reg, $info
	push	{r0-r12, lr}
	mov		r0, $reg
	mov		r1, #$info
	bl		printreg
	pop		{r0-r12, lr}
	MEND

	MACRO
	debugpos $offs
	push	{r0-r12, lr}
	add		r0, r12, $offs			; r0 = current physical instruction pointer
	ldrb	r1, [r0]				; r1 = opcode at current instruction pointer
	mrs		r5,cpsr					; Save flags to r5
	bl		printpos
	msr		cpsr_f,r5				; Restore flags from r5
	pop		{r0-r12, lr}
	MEND

	MACRO
	irq_lock
	push	{r0,r1,r2,r3,r4,r12,lr}
	mrs		r4,cpsr					; Put flags to r4
	bl		lock_irqflag
	msr		cpsr_f,r4				; Restore flags from r4
	pop		{r0,r1,r2,r3,r4,r12,lr}
	MEND

	MACRO
	irq_unlock
	push	{r0,r1,r2,r3,r4,r12,lr}
	mrs		r4,cpsr					; Put flags to r4
	bl		unlock_irqflag
	msr		cpsr_f,r4				; Restore flags from r4
	pop		{r0,r1,r2,r3,r4,r12,lr}
	MEND

	MACRO
	calc_modex_r2
	lsl		r2, #2					; r2 = linear address into EGAVGA_A000 (from byte to word addressing)
	MEND
	MACRO
	calc_modex_r1
	mov		r1, r2, lsl #2			; r1 = linear address into EGAVGA_A000 (from byte to word addressing)
	MEND

	MACRO
	calc_ega_r2
	lsl		r2, #2					; r2 = linear address into EGAVGA_A000 (from byte to word addressing)
	MEND
	MACRO
	calc_ega_r1
	mov		r1, r2, lsl #2			; r1 = linear address into EGAVGA_A000 (from byte to word addressing)
	MEND

	MACRO
	min_cnt_next_page $cnt, $offs
	add		r0, $offs, #(1<<TLB_GRAN_BITS)
	lsr		r0, #TLB_GRAN_BITS					; Clean up the offset bits within the page
	rsb		r0, $offs, r0, lsl #TLB_GRAN_BITS	; r0 = number of bytes remaining on this page
	cmp		r0, $cnt
	movlt	$cnt, r0
	MEND
	MACRO
	min_cnt_prev_page $cnt, $offs
	mov		r0, $offs, lsl #(32-TLB_GRAN_BITS)
	cmp		$cnt, r0, lsr #(32-TLB_GRAN_BITS)
	movgt	$cnt, r0, lsr #(32-TLB_GRAN_BITS)
	MEND

	MACRO
	min_cnt_idx_wrap_16 $cnt, $idx
	mov		r0, #0x10000
	sub		r0, $idx, lsr #16					; r0 = 0x10000-SI
	cmp		r0, $cnt
	movlt	$cnt, r0							; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	MEND
	MACRO
	min_cnt_idx_zero_16 $cnt, $idx
	cmp		$cnt, $idx, lsr #16
	movgt	$cnt, $idx, lsr #16					; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	MEND
	MACRO
	min_cnt_idx_zero_cond $cnt, $idx
	cmpgt	$cnt, $idx, lsr #16					; Only compare if mask was not 0xFFFFFFFF (32-bit addressing)
	movgt	$cnt, $idx, lsr #16					; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	MEND

	MACRO 
	min_cnt_VGA_end $cnt, $offs
	ldr		r0, =EGAVGA_A000			; t0 = end of EGAVGA  VRAM
	ldr		r0, [r0]
	add		r0, #0x40000
	sub		r0, $offs					; t0 = number of bytes left in this page for ES:DI
	cmp		$cnt, r0, lsr #2			; t0 = number of words (EGA bytes) left in this page
	movgt	$cnt, r0, lsr #2
	MEND	
	MACRO
	min_cnt_VGA_beg $cnt, $offs
	ldr		r0, =EGAVGA_A000			; t0 = start of EGAVGA  VRAM
	ldr		r0, [r0]
	sub		r0, $offs, r0				; t0 = number of bytes left in this page for ES:DI
	cmp		$cnt, r0, lsr #2			; t0 = number of words (EGA bytes) left in this page
	movgt	$cnt, r0, lsr #2
	MEND	

	END



