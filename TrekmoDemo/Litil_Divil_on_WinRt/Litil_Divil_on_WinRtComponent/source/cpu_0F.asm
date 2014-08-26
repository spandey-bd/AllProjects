;=============================================================================
; cpu_0F.s
;
; This file contains opcode handlers for the 0x0F-prefixed opcodes, for 16-bit
; address size and 16-bit operand size (USE16 code segment).
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

	AREA cpu_0F, CODE, READONLY
	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_16.inc

	EXTERN	loop
	EXTERN	unknown
	EXTERN	bad_386_opcode_back1
	EXTERN	bad_EGA_opcode
	EXTERN	bad_MODEX_opcode
	EXTERN	restore_flags_from_r0
	EXTERN	complement_carry
	EXTERN	unsjpe
	EXTERN	unsjpo
	EXTERN	jmp_GP_fault
	
	EXTERN	registers
	EXTERN	cpu_cpl
	EXTERN	cpu_gdt_base
	EXTERN	cpu_idt_base
	EXTERN	cpu_ldt_value
	EXTERN	cpu_tss_selector
	EXTERN	cpu_cr0
	EXTERN	cpu_DRx
	EXTERN	BreakReason

; ------------------- 0F 00 = SLDT/STR/LLDT/LTR/VERR/VERW r/m16 ------
; 0F00D0 = lldt ax (DOS4GW, AX=0068)
;
	GLOBAL	op_0f_00
op_0f_00
	;-------
	; TODO! Only available in protected mode!
	;-------
	modrm_jump_16_tbl op_0f_00_jump
; 0
	modrm_help_1_0 sldt
	modrm_help_1_0 str
	modrm_help_1_0 lldt
	modrm_help_1_0 ltr
	modrm_help_1_0 verr
	modrm_help_1_0 verw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0x40
	modrm_help_1_40 sldt
	modrm_help_1_40 str
	modrm_help_1_40 lldt
	modrm_help_1_40 ltr
	modrm_help_1_40 verr
	modrm_help_1_40 verw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0x80
	modrm_help_1_80 sldt
	modrm_help_1_80 str
	modrm_help_1_80 lldt
	modrm_help_1_80 ltr
	modrm_help_1_80 verr
	modrm_help_1_80 verw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0xc0 = mod = 11b => register operand
	modrm_help_1_C0_new sldt
	modrm_help_1_C0_new str
	modrm_help_1_C0_new lldt
	modrm_help_1_C0_new ltr
	modrm_help_1_C0_new verr
	modrm_help_1_C0_new verw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	GLOBAL	sldt_r0_bp		; Called from "cpu_0F_USE32.s"
sldt_r0_bp
	mem_handler_bp
	GLOBAL	sldt_r0			; Called from "cpu_0F_USE32.s"
sldt_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r0, =cpu_ldt_value
	ldrh	r0, [r0]
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	b		loop

	MACRO
	sldt $reg
sldt_$reg
	ldr		r0, =cpu_ldt_value
	ldrh	r0, [r0]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

	sldt eax
	sldt ecx
	sldt edx
	sldt ebx
	sldt esp
	sldt ebp
	sldt esi
	sldt edi

	modrm_1_help sldt, sldt_r0

	GLOBAL	str_r0_bp		; Called from "cpu_0F_USE32.s"
str_r0_bp
	mem_handler_bp
	GLOBAL	str_r0			; Called from "cpu_0F_USE32.s"
str_r0	
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r0, =cpu_tss_selector
	ldrh	r0, [r0]
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	b		loop

	MACRO
	str_r16 $reg
str_$reg
	ldr		r0, =cpu_tss_selector
	ldrh	r0, [r0]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

	str_r16 eax
	str_r16 ecx
	str_r16 edx
	str_r16 ebx
	str_r16 esp
	str_r16 ebp
	str_r16 esi
	str_r16 edi

	modrm_1_help str, str_r0

	LTORG

	GLOBAL	lldt_r0_bp		; Called from "cpu_0F_USE32.s"
lldt_r0_bp
	mem_handler_bp
	GLOBAL	lldt_r0			; Called from "cpu_0F_USE32.s"
lldt_r0	
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;------
	;	Descriptor desc;
	;	if (!GetDescriptor(value,desc)) return !CPU_PrepareException(EXCEPTION_GP,value);
	;	if (desc.Type()!=DESC_LDT) return !CPU_PrepareException(EXCEPTION_GP,value);
	;	if (!desc.saved.seg.p) return !CPU_PrepareException(EXCEPTION_NP,value);
	;	ldt_base=desc.GetBase();
	;	ldt_limit=desc.GetLimit();
	;	ldt_value=value;
	;	return true;
	;------
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
lldt_r1_value
	mrs		r0, cpsr				; Save flags to r0
	str		r0, [sp, #SP_STR_SEG]	; Save the flags temporarily to stack
	;------
	;	if ((value&0xfffc)==0) {
	;		ldt_value=0;
	;		ldt_base=0;
	;		ldt_limit=0;
	;		return true;
	;	}
	;------
	bics	r0, r1, #3
	beq		%f8
	;------
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r2 r1, jmp_GP_fault
	ldrb	r0, [r2, #5]						; r0 = Access byte of the descriptor
	;------
	;	if (!desc.saved.seg.p) return !CPU_PrepareException(EXCEPTION_NP,value);
	;------
	tst		r0, #0x80
	beq		jmp_GP_fault						; TODO!
	;------
	;	if (desc.Type()!=DESC_LDT) return !CPU_PrepareException(EXCEPTION_GP,value);
	;------
	and		r0, #0x1F
	cmp		r0, #2
	bne		jmp_GP_fault						; if desc.Type() != 2 => fault
	;------
	;	ldt_base=desc.GetBase();
	;	ldt_limit=desc.GetLimit();
	;	ldt_value=value;
	;	return true;
	;------
	ldr		r0, =cpu_ldt_value
	str		r1, [r0]
	; --- limit ---
	ldrb	r1, [r2, #0]
	strb	r1, [r0, #16]						; cpu_ldt_limit
	ldrb	r1, [r2, #1]
	strb	r1, [r0, #17]						; cpu_ldt_limit<<8
	; --- base ---
	ldrb	r1, [r2, #2]
	strb	r1, [r0, #8]						; cpu_ldt_base
	ldrb	r1, [r2, #3]
	strb	r1, [r0, #9]						; cpu_ldt_base<<8
	ldrb	r1, [r2, #4]
	strb	r1, [r0, #10]						; cpu_ldt_base<<16
	ldrb	r1, [r2, #7]
	strb	r1, [r0, #11]						; cpu_ldt_base<<24
	; --- phys ---
	ldr		r2, [r0, #8]						; r2 = cpu_ldt_base
	mov		r1, r0
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #24]						; Save the physical address to cpu_ldt_phys
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
	;-------
	; Zero selector, so clear the values
	;-------
8	ldr		r0, =cpu_ldt_value
	mov		r1, #0
	str		r1, [r0]
	str		r1, [r0, #8]
	str		r1, [r0, #16]
	str		r1, [r0, #24]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags


	MACRO
	lldt $sreg, $rreg
	GLOBAL	lldt_$rreg
lldt_$rreg										; Called from "cpu_0F_USE32.s"
lldt_$sreg
	ubfx	r1, $rreg, #0, #16
	b		lldt_r1_value
	MEND

	lldt ax, eax
	lldt cx, ecx
	lldt dx, edx
	lldt bx, ebx
	lldt sp, esp
	lldt bp, ebp
	lldt si, esi
	lldt di, edi

	modrm_1_help lldt, lldt_r0


	GLOBAL	ltr_r0_bp		; Called from "cpu_0F_USE32.s"
ltr_r0_bp
	mem_handler_bp
	GLOBAL	ltr_r0			; Called from "cpu_0F_USE32.s"
ltr_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
	;-----
	;	if (cpu.cpl) EXCEPTION(EXCEPTION_GP);
	;-----
	GLOBAL	ltr_from_r1
ltr_from_r1
	mrs		r0, cpsr				; Save flags to r0
	str		r0, [sp, #SP_STR_SEG]	; Save the flags temporarily to stack
	;------
	;	bool CPU_LTR(Bitu selector) {
	;		if ((selector & 0xfffc)==0) {
	;			cpu_tss.SetSelector(selector);
	;			return false;
	;		}
	;------
	bics	r0, r1, #3
	beq		%f8
	;------
	;		TSS_Descriptor desc;
	;		if ((selector & 4) || (!cpu.gdt.GetDescriptor(selector,desc))) {
	;			LOG(LOG_CPU,LOG_ERROR)("LTR failed, selector=%X",selector);
	;			return CPU_PrepareException(EXCEPTION_GP,selector);
	;		}
	;------
	tst		r1, #4
	bne		jmp_GP_fault
	GetDescriptor_r2 r1, jmp_GP_fault
	ldrb	r0, [r2, #5]						; r0 = Access byte of the descriptor
	;------
	;			if (!desc.saved.seg.p) {
	;				LOG(LOG_CPU,LOG_ERROR)("LTR failed, selector=%X (not present)",selector);
	;				return CPU_PrepareException(EXCEPTION_NP,selector);
	;			}
	;------
	tst		r0, #0x80
	beq		jmp_GP_fault						; TODO!
	;------
	;		if ((desc.Type()==DESC_286_TSS_A) || (desc.Type()==DESC_386_TSS_A)) {		0x01, 0x09
	;------
	tst		r0, #0x1
	beq		jmp_GP_fault
	tst		r0, #0x16
	bne		jmp_GP_fault
	;------
	;			cpu_tss.desc.SetBusy(true);
	;			cpu_tss.SaveSelector();
	;------
	orr		r0, #2				; Set the Busy bit on
	strb	r0, [r2, #5]		; Save the type byte of the TSS into the GDT
	;------
	;			if (!cpu_tss.SetSelector(selector)) E_Exit("LTR failed, selector=%X",selector);
	;						bool SetSelector(Bitu new_sel) {
	;							selector=new_sel;
	;							valid=true;
	;							base=desc.GetBase();
	;							limit=desc.GetLimit();
	;							return true;
	;						}
	;------
	and		r3, r0, #8						; t0 = is386 bit of desc.Type()
	ldr		r0, =cpu_tss_selector
	str		r1, [r0]						; cpu_tss_selector
	; --- limit ---
	ldrb	r1, [r2, #0]					
	strb	r1, [r0, #8]					; cpu_tss_limit
	ldrb	r1, [r2, #1]					
	strb	r1, [r0, #9]					; cpu_tss_limit<<8
	; --- base ---
	ldrb	r1, [r2, #2]					
	strb	r1, [r0, #4]					; cpu_tss_base
	ldrb	r1, [r2, #3]					
	strb	r1, [r0, #5]					; cpu_tss_base<<8
	ldrb	r1, [r2, #4]					
	strb	r1, [r0, #6]					; cpu_tss_base<<16
	ldrb	r1, [r2, #7]					
	strb	r1, [r0, #7]					; cpu_tss_base<<24
	; --- valid ---
	mov		r1, #1
	strb	r1, [r0, #12]					; cpu_tss_valid
	; --- is386 ---
	strb	r3, [r0, #20]					; cpu_tss_is386
	; --- phys ---
	ldr		r2, [r0, #4]						; r2 = cpu_tss_base
	mov		r1, r0
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #16]						; Save the physical address to cpu_tss_phys
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
	;------
	;							valid=false;
	;							if ((new_sel & 0xfffc)==0) {
	;								selector=0;
	;								base=0;
	;								limit=0;
	;								is386=1;
	;								return true;
	;							}
	;------
8	ldr		r0, =cpu_tss_selector
	mov		r1, #0
	str		r1, [r0]						; cpu_tss_selector
	str		r1, [r0, #4]					; cpu_tss_base
	str		r1, [r0, #8]					; cpu_tss_limit
	str		r1, [r0, #12]					; cpu_tss_valid
	str		r1, [r0, #16]					; cpu_tss_phys
	ldr		r0, [sp, #SP_STR_SEG]			; Restore the flags from stack
	b		restore_flags_from_r0			; Go back to the opcode loop, restoring flags

	MACRO
	ltr $sreg, $rreg
	GLOBAL	ltr_$rreg
ltr_$rreg									; Called from "cpu_0F_USE32.s"
ltr_$sreg
	ubfx	r1, $rreg, #0, #16
	b		ltr_from_r1
	MEND

	ltr ax, eax
	ltr cx, ecx
	ltr dx, edx
	ltr bx, ebx
	ltr sp, esp
	ltr bp, ebp
	ltr si, esi
	ltr di, edi


	modrm_1_help ltr, ltr_r0


	GLOBAL	verr_r0_bp		; Called from "cpu_0F_USE32.s"
verr_r0_bp
	mem_handler_bp
	GLOBAL	verr_r0			; Called from "cpu_0F_USE32.s"
verr_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
	GLOBAL	verr_from_r1
verr_from_r1
	;------
	; void CPU_VERR(Bitu selector) {
	;	FillFlags();
	;------
	mrs		r0, cpsr				; Save flags to r0
	str		r0, [sp, #SP_STR_SEG]	; Save the flags temporarily to stack
	;------
	;	if (selector == 0) {
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r1, #0
	beq		verr_ZF_false
	;------
	;	Descriptor desc;Bitu rpl=selector & 3;
	;	if (!cpu.gdt.GetDescriptor(selector,desc)){
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	GetDescriptor_r2 r1, verr_ZF_false
	ldrb	r0, [r2, #5]						; r0 = Access byte of the descriptor
	and		r1, #3								; r1 = rpl
	mov		r2, r0, lsr #5
	and		r2, #3								; r2 = desc.DPL()
	and		r0, #0x1F							; r0 = desc.Type()
	;------
	;	switch (desc.Type()){
	;	case DESC_CODE_R_C_A:		case DESC_CODE_R_C_NA:			0x1E 0x1F
	;		;Conforming readable code segments can be always read 
	;		break;
	;------
	cmp		r0, #0x1E
	bge		verr_ZF_true
	;------
	;	case DESC_DATA_EU_RO_NA:	case DESC_DATA_EU_RO_A:			0x10..0x17
	;	case DESC_DATA_EU_RW_NA:	case DESC_DATA_EU_RW_A
	;	case DESC_DATA_ED_RO_NA:	case DESC_DATA_ED_RO_A
	;	case DESC_DATA_ED_RW_NA:	case DESC_DATA_ED_RW_A
	;
	;	case DESC_CODE_R_NC_A:		case DESC_CODE_R_NC_NA:			0x1A 0x1B
	;		if (desc.DPL()<cpu.cpl || desc.DPL() < rpl) {
	;			SETFLAGBIT(ZF,false);
	;			return;
	;		}
	;		break;
	;	default
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r0, #0x10
	blt		verr_ZF_false
	cmp		r0, #0x1A
	beq		%f1
	cmp		r0, #0x1B
	beq		%f1
	cmp		r0, #0x17
	bgt		verr_ZF_false
1	cmp		r2, r1								; desc.DPL() < rpl ?
	blt		verr_ZF_false
	ldr		r0, =cpu_cpl
	ldrb	r0, [r0]
	cmp		r2, r0								; desc.DPL() < cpu.cpl ?
	blt		verr_ZF_false
verr_ZF_true
	;------
	;	SETFLAGBIT(ZF,true);
	;}
	;------
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
verr_ZF_false
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	bic		r0, #ARM_ZERO
	b		restore_flags_from_r0

	MACRO
	verr $sreg, $rreg
	GLOBAL	verr_$rreg
verr_$rreg										; Called from "cpu_0F_USE32.s"
verr_$sreg
	ubfx	r1, $rreg, #0, #16
	b		verr_from_r1
	MEND

	verr ax, eax
	verr cx, ecx
	verr dx, edx
	verr bx, ebx
	verr sp, esp
	verr bp, ebp
	verr si, esi
	verr di, edi

	modrm_1_help verr, verr_r0

	GLOBAL	verw_r0_bp		; Called from "cpu_0F_USE32.s"
verw_r0_bp
	mem_handler_bp
	GLOBAL	verw_r0			; Called from "cpu_0F_USE32.s"
verw_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
	GLOBAL	verw_from_r1
verw_from_r1
	;------
	; void CPU_VERW(Bitu selector) {
	;	FillFlags();
	;------
	mrs		r0, cpsr				; Save flags to r0
	str		r0, [sp, #SP_STR_SEG]	; Save the flags temporarily to stack
	;------
	;	if (selector == 0) {
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r1, #0
	beq		verw_ZF_false
	;------
	;	Descriptor desc;Bitu rpl=selector & 3;
	;	if (!cpu.gdt.GetDescriptor(selector,desc)){
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	GetDescriptor_r2 r1, verw_ZF_false
	ldrb	r0, [r2, #5]						; r0 = Access byte of the descriptor
	and		r1, #3								; r1 = rpl
	mov		r2, r0, lsr #5
	and		r2, #3								; r2 = desc.DPL()
	and		r0, #0x1A							; t0 = desc.Type() & 0b11010
	;------
	;	case DESC_DATA_EU_RW_NA:	case DESC_DATA_EU_RW_A:			0x12 0x13 = 0b10010 0b10011
	;	case DESC_DATA_ED_RW_NA:	case DESC_DATA_ED_RW_A:			0x16 0x17 = 0b10110 0b10111
	;		if (desc.DPL()<cpu.cpl || desc.DPL() < rpl) {
	;			SETFLAGBIT(ZF,false);
	;			return;
	;		}
	;		break;
	;	default
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r0, #0x12
	bne		verw_ZF_false
	cmp		r2, r1								; desc.DPL() < rpl ?
	blt		verw_ZF_false
	ldr		r0, =cpu_cpl
	ldrb	r0, [r0]
	cmp		r2, r0								; desc.DPL() < cpu.cpl ?
	blt		verw_ZF_false
verw_ZF_true
	;------
	;	SETFLAGBIT(ZF,true);
	;}
	;------
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
verw_ZF_false
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	bic		r0, #ARM_ZERO
	b		restore_flags_from_r0

	MACRO
	verw $sreg, $rreg
	GLOBAL	verw_$rreg
verw_$rreg										; Called from "cpu_0F_USE32.s"
verw_$sreg
	ubfx	r1, $rreg, #0, #16
	b		verw_from_r1
	MEND

	verw ax, eax
	verw cx, ecx
	verw dx, edx
	verw bx, ebx
	verw sp, esp
	verw bp, ebp
	verw si, esi
	verw di, edi

	modrm_1_help verw, verw_r0


	LTORG
	
	
; ------------------- 0F 01 = SGDT/SIDT/LGDT/LIDT/SMSW/LMSW r/m16 -----
;  0F0146FC 	sgdt word [BP-04]	(Windows 3.00a WIN.COM)
;				sgdt word [0074]	(Windows 3.00a DOSX.EXE)
;  0F010E1A00	sidt word [001A]	(Windows 3.00a DOSX.EXE)
;
	GLOBAL	op_0f_01
op_0f_01
	modrm_jump_16_tbl op_0f_01_jump
; 0
	modrm_help_1_0 sgdt
	modrm_help_1_0 sidt
	modrm_help_1_0 lgdt_word
	modrm_help_1_0 lidt_word
	modrm_help_1_0 smsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	modrm_help_1_0 lmsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0x40
	modrm_help_1_40 sgdt
	modrm_help_1_40 sidt
	modrm_help_1_40 lgdt_word
	modrm_help_1_40 lidt_word
	modrm_help_1_40 smsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	modrm_help_1_40 lmsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0x80
	modrm_help_1_80 sgdt
	modrm_help_1_80 sidt
	modrm_help_1_80 lgdt_word
	modrm_help_1_80 lidt_word
	modrm_help_1_80 smsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	modrm_help_1_80 lmsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
;0xc0 = mod = 11b => register operand
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	modrm_help_1_C0_new smsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1
	modrm_help_1_C0_new lmsw
	DCD bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1, bad_386_opcode_back1

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	GLOBAL	sgdt_r0_bp
sgdt_r0_bp
	mem_handler_bp
	GLOBAL	sgdt_r0
sgdt_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r0, =cpu_gdt_base
	ldr		r1, [r0]				; cpu_gdt_base
	ldrh	r0, [r0, #8]			; cpu_gdt_limit
	;-------
	; Save limit
	;-------
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	;-------
	; Save base
	;-------
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	lsr		r1, #8
	strb	r1, [r2, #4]
	lsr		r1, #8
	strb	r1, [r2, #5]
	b		loop

	modrm_1_help sgdt, sgdt_r0

	GLOBAL	sidt_r0_bp
sidt_r0_bp
	mem_handler_bp
	GLOBAL	sidt_r0
sidt_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r0, =cpu_idt_base
	ldr		r1, [r0]				; cpu_idt_base
	ldrh	r0, [r0, #4]			; cpu_idt_limit
	;-------
	; Save limit
	;-------
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	;-------
	; Save base
	;-------
	strb	r1, [r2, #2]
	lsr		r1, #8
	strb	r1, [r2, #3]
	lsr		r1, #8
	strb	r1, [r2, #4]
	lsr		r1, #8
	strb	r1, [r2, #5]
	b		loop

	modrm_1_help sidt, sidt_r0

	GLOBAL	lgdt_word_r0_bp
lgdt_word_r0_bp
	mem_handler_bp
	GLOBAL	lgdt_word_r0
lgdt_word_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r1, =cpu_gdt_base
	;-------
	; Get limit
	;-------
	ldrb	r0, [r2, #0]
	strb	r0, [r1, #8]						; cpu_gdt_limit
	ldrb	r0, [r2, #1]
	strb	r0, [r1, #9]						; cpu_gdt_limit<<8
	;-------
	; Get base
	;-------
	ldrb	r0, [r2, #2]
	strb	r0, [r1, #0]						; cpu_gdt_base
	ldrb	r0, [r2, #3]
	strb	r0, [r1, #1]						; cpu_gdt_base<<8
	ldrb	r0, [r2, #4]
	strb	r0, [r1, #2]						; cpu_gdt_base<<16
	mov		r0, #0
	strb	r0, [r1, #3]						; cpu_gdt_base<<24
	;-------
	; Calculate new physical base
	;-------
	ldr		r2, [r1]							; r2 = cpu_gdt_base
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #16]						; Save the physical address to cpu_gdt_phys
	b		loop

	modrm_1_help lgdt_word, lgdt_word_r0

	GLOBAL	lidt_word_r0_bp
lidt_word_r0_bp
	mem_handler_bp
	GLOBAL	lidt_word_r0
lidt_word_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r1, =cpu_idt_base
	;-------
	; Get limit
	;-------
	ldrb	r0, [r2, #0]
	strb	r0, [r1, #4]						; cpu_idt_limit
	ldrb	r0, [r2, #1]
	strb	r0, [r1, #5]						; cpu_idt_limit<<8
	;-------
	; Get base
	;-------
	ldrb	r0, [r2, #2]
	strb	r0, [r1, #0]						; cpu_idt_base
	ldrb	r0, [r2, #3]
	strb	r0, [r1, #1]						; cpu_idt_base<<8
	ldrb	r0, [r2, #4]
	strb	r0, [r1, #2]						; cpu_idt_base<<16
	mov		r0, #0
	strb	r0, [r1, #3]						; cpu_idt_base<<24
	;-------
	; Calculate new physical base
	;-------
	ldr		r2, [r1]							; r2 = cpu_idt_base
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #8]						; Save the physical address to cpu_idt_phys
	b		loop

	modrm_1_help lidt_word, lidt_word_r0


	GLOBAL	smsw_r0_bp
smsw_r0_bp
	mem_handler_bp
	GLOBAL	smsw_r0
smsw_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldr		r0, =cpu_cr0
	ldrh	r0, [r0]
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	b		loop

	MACRO
	smsw $sreg, $rreg
	GLOBAL	smsw_$rreg
smsw_$rreg
smsw_$sreg
	ldr		r0, =cpu_cr0
	ldrh	r0, [r0]
	bfi		$rreg, r0, #0, #16
	b		loop
	MEND

	smsw ax, eax
	smsw cx, ecx
	smsw dx, edx
	smsw bx, ebx
	smsw sp, esp
	smsw bp, ebp
	smsw si, esi
	smsw di, edi

	modrm_1_help smsw, smsw_r0

	GLOBAL	lmsw_r0_bp
lmsw_r0_bp
	mem_handler_bp
	GLOBAL	lmsw_r0
lmsw_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	orr		r2, r0, r1, lsl #8
lmsw_r2
	;------
	; if (cpu.pmode && (cpu.cpl>0)) return CPU_PrepareException(EXCEPTION_GP,0);
	; word&=0xf;
	; if (cpu.cr0 & 1) word|=1; 
	; word|=(cpu.cr0&0xfffffff0);
	; CPU_SET_CRX(0,word);
	;------
	ldr		r1, =cpu_cr0
	ldr		r0, [r1]
	and		r2, #0x0F
	bic		r0, #0x0E
	orr		r2, r0
	strh	r2, [r1]
	strb	r2, [sp, #SP_CPU_CR0]
	eor		r3, r2, r0				; Set the bits that changed
	mrs		r0, cpsr				; Save flags to r0
	tst		r3, #1					; Did we just go into protected mode?
	beq		restore_flags_from_r0	; Nope, just go back to the opcode loop
	msr		cpsr_f, r0				; Restore flags
	mov		r0, r2					; r0 = new cpu_cr0 value
	b		prot_mode_entry_exit_r0	; Go enter protected mode!

	MACRO
	lmsw $sreg, $rreg
	GLOBAL	lmsw_$rreg
lmsw_$rreg
lmsw_$sreg
	ubfx	r2, $rreg, #0, #16
	b		lmsw_r2
	MEND

	lmsw ax, eax
	lmsw cx, ecx
	lmsw dx, edx
	lmsw bx, ebx
	lmsw sp, esp
	lmsw bp, ebp
	lmsw si, esi
	lmsw di, edi

	modrm_1_help lmsw, lmsw_r0

	LTORG

; ------------------- 0F 02 = LAR r16, r/m16 -------------------------
; These are only available while in protected mode!
;
	GLOBAL	op_0f_02
op_0f_02
	modrm_jump_16_tbl op_0f_02_jump
	modrm_tbl_3 lar

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	MACRO
	lar_r0 $reg
	GLOBAL	lar_r0_bp_$reg
lar_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lar_r0_$reg
lar_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
lar_r1_$reg
	;------
	;void CPU_LAR(Bitu selector,Bitu & ar) {
	;	FillFlags();
	;------
	mrs		r0, cpsr				; Save flags to r1
	str		r0, [sp, #SP_STR_SEG]	; Save the flags temporarily to stack
	;------
	;	if (selector == 0) {
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r1, #0
	beq		lar_ZF_false
	;------
	;	Descriptor desc;Bitu rpl=selector & 3;
	;	if (!cpu.gdt.GetDescriptor(selector,desc)){
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	GetDescriptor_r2 r1, lar_ZF_false
	;------
	;	switch (desc.Type()){
	;------
	ldrb	r2, [r2, #5]
	and		r0, r2, #0x1F			; r0 = desc.Type()
	;------
	;	case DESC_CODE_N_C_A:	case DESC_CODE_N_C_NA:					0x1c, 0x1d, 0x1e, 0x1f
	;	case DESC_CODE_R_C_A:	case DESC_CODE_R_C_NA
	;		break;
	;------
	cmp		r0, #0x1C
	bge		%f1
	;------
	;	case DESC_286_INT_GATE:		case DESC_286_TRAP_GATE:	{		0x06, 0x07
	;	case DESC_386_INT_GATE:		case DESC_386_TRAP_GATE:			0x0E, 0x0F
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r0, #0x06
	beq		lar_ZF_false
	cmp		r0, #0x07
	beq		lar_ZF_false
	cmp		r0, #0x0E
	beq		lar_ZF_false
	cmp		r0, #0x0F
	beq		lar_ZF_false
	;------
	;	default:														0x00, 0x08, 0x0a, 0x0d
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r0, #0
	beq		lar_ZF_false
	cmp		r0, #0x08
	beq		lar_ZF_false
	cmp		r0, #0x0a
	beq		lar_ZF_false
	cmp		r0, #0x0d
	beq		lar_ZF_false
	;------
	;	case DESC_LDT:													0x02
	;	case DESC_TASK_GATE:											0x05
	;
	;	case DESC_286_TSS_A:		case DESC_286_TSS_B:				0x01, 0x03
	;	case DESC_286_CALL_GATE:										0x04
	;
	;	case DESC_386_TSS_A:		case DESC_386_TSS_B:				0x09, 0x0b
	;	case DESC_386_CALL_GATE:										0x0c
	;	
	;
	;	case DESC_DATA_EU_RO_NA:	case DESC_DATA_EU_RO_A:				0x10..0x1b
	;	case DESC_DATA_EU_RW_NA:	case DESC_DATA_EU_RW_A
	;	case DESC_DATA_ED_RO_NA:	case DESC_DATA_ED_RO_A
	;	case DESC_DATA_ED_RW_NA:	case DESC_DATA_ED_RW_A
	;	case DESC_CODE_N_NC_A:		case DESC_CODE_N_NC_NA
	;	case DESC_CODE_R_NC_A:		case DESC_CODE_R_NC_NA
	;		if (desc.DPL()<cpu.cpl || desc.DPL() < rpl) {				0x01..
	;			SETFLAGBIT(ZF,false);
	;			return;
	;		}
	;		break;
	;------
	and		r0, r1, #3					; r0 = rpl
	mov		r1, r2, lsr #5
	and		r1, #3						; r1 = desc.DPL()
	cmp		r1, r0
	blt		lar_ZF_false
	ldr		r0, =cpu_cpl
	ldrb	r0, [r0]					; r0 = cpu.cpl
	cmp		r1, r0
	blt		lar_ZF_false
	;------
	;	/* Valid descriptor */
	;	ar=desc.saved.fill[1] & 0x00ffff00;
	;	SETFLAGBIT(ZF,true);
	;}
	;------
1	ldr		r0, [sp, #SP_STR_SEG]	; Get the flags from stack
	lsl		r2, #8
	bfi		$reg, r2, #0, #16
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0
	MEND

	GLOBAL	lar_ZF_false
	GLOBAL	lsl_ZF_false
lar_ZF_false
lsl_ZF_false
	ldr		r0, [sp, #SP_STR_SEG]	; Get the flags from stack
	bic		r0, #ARM_ZERO
	b		restore_flags_from_r0

	lar_r0 eax
	lar_r0 ecx
	lar_r0 edx
	lar_r0 ebx
	lar_r0 esp
	lar_r0 ebp
	lar_r0 esi
	lar_r0 edi

	LTORG
	
	MACRO
	lar_reg16_reg16 $rl, $rr
	ubfx	r1, $rr, #0, #16
	b		lar_r1_$rl
	MEND

	modrm_3_genall lar, lar_r0

; ------------------- 0F 03 = LSL r16, r/m16 -------------------------
; These are only available while in protected mode!
;
	GLOBAL	op_0f_03
op_0f_03
	modrm_jump_16_tbl op_0f_03_jump
	modrm_tbl_3 lsl

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	MACRO
	lsl_r0 $reg
	GLOBAL	lsl_r0_bp_$reg
lsl_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lsl_r0_$reg
lsl_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
lsl_r1_$reg
	;------
	;void CPU_LSL(Bitu selector,Bitu & limit) {
	;	FillFlags();
	;------
	mrs		r0, cpsr				; Save flags to r1
	str		r0, [sp, #SP_STR_SEG]	; Save the flags to stack
	;------
	;	if (selector == 0) {
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	cmp		r1, #0
	beq		lsl_ZF_false
	;------
	;	Descriptor desc;Bitu rpl=selector & 3;
	;	if (!cpu.gdt.GetDescriptor(selector,desc)){
	;		SETFLAGBIT(ZF,false);
	;		return;
	;	}
	;------
	GetDescriptor_r2 r1, lsl_ZF_false
	;------
	;	switch (desc.Type()){
	;------
	ldrb	r0, [r2, #5]			; r0 = flags byte
	and		r1, #3					; r1 = rpl
	mov		r3, r0, lsr #5
	and		r3, #3					; r3 = desc.DPL()
	ands	r0, #0x1F				; r0 = desc.Type()
	beq		lsl_ZF_false
	;------
	;	case DESC_CODE_N_C_A:	case DESC_CODE_N_C_NA:					0x1c, 0x1d, 0x1e, 0x1f
	;	case DESC_CODE_R_C_A:	case DESC_CODE_R_C_NA
	;		break;
	;------
	cmp		r0, #0x1C
	bge		%f1
	;------
	;	default:														0x00, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
	;		SETFLAGBIT(ZF,false);										0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f
	;		return;
	;	}
	;------
	cmp		r0, #0x10
	bge		%f2
	cmp		r0, #0x04
	bge		lsl_ZF_false
	;------
	;	case DESC_LDT:													0x02
	;
	;	case DESC_286_TSS_A:		case DESC_286_TSS_B:				0x01, 0x03
	;
	;	case DESC_DATA_EU_RO_NA:	case DESC_DATA_EU_RO_A:				0x10..0x1b
	;	case DESC_DATA_EU_RW_NA:	case DESC_DATA_EU_RW_A
	;	case DESC_DATA_ED_RO_NA:	case DESC_DATA_ED_RO_A
	;	case DESC_DATA_ED_RW_NA:	case DESC_DATA_ED_RW_A
	;	case DESC_CODE_N_NC_A:		case DESC_CODE_N_NC_NA
	;	case DESC_CODE_R_NC_A:		case DESC_CODE_R_NC_NA
	;		if (desc.DPL()<cpu.cpl || desc.DPL() < rpl) {				0x01..
	;			SETFLAGBIT(ZF,false);
	;			return;
	;		}
	;		break;
	;------
2	cmp		r3, r1						; desc.DPL() < rpl ?
	blt		lsl_ZF_false
	ldr		r0, =cpu_cpl
	ldrb	r0, [r0]					; r0 = cpu.cpl
	cmp		r3, r0						; desc.DPL() < cpu.cpl?
	blt		lsl_ZF_false
	;------
	;	/* Valid descriptor */
	; limit=desc.GetLimit();
	;	Bitu GetLimit (void) {
	;		Bitu limit = (saved.seg.limit_16_19<<16) | saved.seg.limit_0_15;
	;		if (saved.seg.g)	return (limit<<12) | 0xFFF;
	;		return limit;
	;	}
	; SETFLAGBIT(ZF,true);
	;}
	;------
1	ldrb	r1, [r2, #6]				; Get Flags and limit_16_19
	ldrb	r0, [r2, #0]				; Low byte of limit
	ldrb	r2, [r2, #1]				; High byte of limit
	tst		r1, #0x80					; saved.seg.g ?
	orr		r0, r2, lsl #8				; r0 = limit_0_15
	lslne	r0, #12
	orrne	r0, #0xFF
	orrne	r0, #0x0F00					; if (saved.seg.g) return (limit<<12) | 0xFFF;
	bfi		$reg, r0, #0, #16
	ldr		r0, [sp, #SP_STR_SEG]	; Get the flags from stack
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0
	MEND

	lsl_r0 eax
	lsl_r0 ecx
	lsl_r0 edx
	lsl_r0 ebx
	lsl_r0 esp
	lsl_r0 ebp
	lsl_r0 esi
	lsl_r0 edi

	LTORG
	
	MACRO
	lsl_reg16_reg16 $rl, $rr
	ubfx	r1, $rr, #0, #16
	b		lsl_r1_$rl
	MEND

	modrm_3_genall lsl, lsl_r0
	

	;------
	; Fault! Not a proper code segment selector!
	;------	
jmp_GP_fault:	
	b		unknown

; ------------------- 0F 06 = CLTS ---------------------
; (Comanche)
;
	GLOBAL	op_0f_06
op_0f_06
	;------
	;	if (cpu.pmode && cpu.cpl) EXCEPTION(EXCEPTION_GP);
	;	cpu.cr0&=(~CR0_TASKSWITCH);
	;------
	ldr		r1, =cpu_cr0
	ldr		r0, [r1]
	bic		r0, #CR0_TASKSWITCH
	str		r0, [r1]
	b		loop

; ------------------- 0F 20 = MOV Rd,CRx -----------------------------
;
	GLOBAL	op_0f_20
op_0f_20
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =cpu_cr0						; r1 = address of the cpu_cr0 variable
	ldr		r2, =op_0f_20_jump
	and		r0, #0x3F							; No memory access used.
	ldr		pc,[r2, r0, lsl #2]					; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
op_0f_20_jump
; cr0
	DCD mov_eax_CR0, mov_ecx_CR0, mov_edx_CR0, mov_ebx_CR0, mov_esp_CR0, mov_ebp_CR0, mov_esi_CR0, mov_edi_CR0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; cr2
	DCD mov_eax_CR2, mov_ecx_CR2, mov_edx_CR2, mov_ebx_CR2, mov_esp_CR2, mov_ebp_CR2, mov_esi_CR2, mov_edi_CR2
; cr3
	DCD mov_eax_CR3, mov_ecx_CR3, mov_edx_CR3, mov_ebx_CR3, mov_esp_CR3, mov_ebp_CR3, mov_esi_CR3, mov_edi_CR3
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F, CODE, READONLY
	ALIGN	4
	
	GLOBAL	mov_eax_CR0
mov_eax_CR0
	ldr		eax, [r1, #0]
	b		loop
	GLOBAL	mov_ecx_CR0
mov_ecx_CR0
	ldr		ecx, [r1, #0]
	b		loop
	GLOBAL	mov_edx_CR0
mov_edx_CR0
	ldr		edx, [r1, #0]
	b		loop
	GLOBAL	mov_ebx_CR0
mov_ebx_CR0
	ldr		ebx, [r1, #0]
	b		loop
	GLOBAL	mov_esp_CR0
mov_esp_CR0
	ldr		esp, [r1, #0]
	b		loop
	GLOBAL	mov_ebp_CR0
mov_ebp_CR0
	ldr		ebp, [r1, #0]
	b		loop
	GLOBAL	mov_esi_CR0
mov_esi_CR0
	ldr		esi, [r1, #0]
	b		loop
	GLOBAL	mov_edi_CR0
mov_edi_CR0
	ldr		edi, [r1, #0]
	b		loop

	GLOBAL	mov_eax_CR2
mov_eax_CR2
	ldr		eax, [r1, #4]
	b		loop
	GLOBAL	mov_ecx_CR2
mov_ecx_CR2
	ldr		ecx, [r1, #4]
	b		loop
	GLOBAL	mov_edx_CR2
mov_edx_CR2
	ldr		edx, [r1, #4]
	b		loop
	GLOBAL	mov_ebx_CR2
mov_ebx_CR2
	ldr		ebx, [r1, #4]
	b		loop
	GLOBAL	mov_esp_CR2
mov_esp_CR2
	ldr		esp, [r1, #4]
	b		loop
	GLOBAL	mov_ebp_CR2
mov_ebp_CR2
	ldr		ebp, [r1, #4]
	b		loop
	GLOBAL	mov_esi_CR2
mov_esi_CR2
	ldr		esi, [r1, #4]
	b		loop
	GLOBAL	mov_edi_CR2
mov_edi_CR2
	ldr		edi, [r1, #4]
	b		loop

	GLOBAL	mov_eax_CR3
mov_eax_CR3
	ldr		eax, [r1, #8]
	b		loop
	GLOBAL	mov_ecx_CR3
mov_ecx_CR3
	ldr		ecx, [r1, #8]
	b		loop
	GLOBAL	mov_edx_CR3
mov_edx_CR3
	ldr		edx, [r1, #8]
	b		loop
	GLOBAL	mov_ebx_CR3
mov_ebx_CR3
	ldr		ebx, [r1, #8]
	b		loop
	GLOBAL	mov_esp_CR3
mov_esp_CR3
	ldr		esp, [r1, #8]
	b		loop
	GLOBAL	mov_ebp_CR3
mov_ebp_CR3
	ldr		ebp, [r1, #8]
	b		loop
	GLOBAL	mov_esi_CR3
mov_esi_CR3
	ldr		esi, [r1, #8]
	b		loop
	GLOBAL	mov_edi_CR3
mov_edi_CR3
	ldr		edi, [r1, #8]
	b		loop
	
; ------------------- 0F 21 = MOV Rd,DRx -----------------------------
;
	GLOBAL	op_0f_21
op_0f_21
	ldrb	r1,[r12],#1							; Load the second opcode byte to r1, increment r12 by 1
	mrs		r0, cpsr							; Save flags to r0
	;------
	; Calculate r2 = the cpu_DRx variable value
	;------
	ldr		r2, =cpu_DRx						; r2 = address of the cpu_DRx table
	mov		r3, r1, lsr #3
	and		r3, #7								; r3=which=(rm >> 3) & 7;
	cmp		r3, #4
	orrge	r3, #2								; Make 4 => 6 and 5 => 7 
	ldr		r2, [r2, r3, lsl #2]				; r2 = debug register value to put into general register
	;------
	; Handle the correct output register
	;------
	and		r1, #7
	;------
	; Register handlers, each must be 8 bytes in size!
	;------	
	cmp		r1, #0
	moveq	eax, r2	
	beq		restore_flags_from_r0
	cmp		r1, #1
	moveq	ecx, r2	
	beq		restore_flags_from_r0
	cmp		r1, #2
	moveq	edx, r2	
	beq		restore_flags_from_r0
	cmp		r1, #3
	moveq	ebx, r2	
	beq		restore_flags_from_r0
	cmp		r1, #4
	moveq	esp,  r2
	beq		restore_flags_from_r0
	cmp		r1, #5
	moveq	ebp, r2	
	beq		restore_flags_from_r0
	cmp		r1, #6
	moveq	esi, r2	
	beq		restore_flags_from_r0
	mov		edi, r2	
	b		restore_flags_from_r0


; ------------------- 0F 22 = MOV CRx,Rd -----------------------------
;
	GLOBAL	op_0f_22
op_0f_22
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =cpu_cr0						; r1 = address of the cpu_cr0 variable
	ldr		r2, =op_0f_22_jump
	and		r0, #0x3F							; No memory access used.
	ldr		pc,[r2, r0, lsl #2]					; Jump to the handler

	AREA	jumptables, DATA, READONLY
	ALIGN	4
op_0f_22_jump
	DCD mov_CR0_eax, mov_CR0_ecx, mov_CR0_edx, mov_CR0_ebx, mov_CR0_esp, mov_CR0_ebp, mov_CR0_esi, mov_CR0_edi
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD mov_CR2_eax, mov_CR2_ecx, mov_CR2_edx, mov_CR2_ebx, mov_CR2_esp, mov_CR2_ebp, mov_CR2_esi, mov_CR2_edi
	DCD mov_CR3_eax, mov_CR3_ecx, mov_CR3_edx, mov_CR3_ebx, mov_CR3_esp, mov_CR3_ebp, mov_CR3_esi, mov_CR3_edi
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F, CODE, READONLY
	ALIGN	4

mov_CR0_eax
	mov		r0, eax
	;------
	; Fix the opcode tables depending on whether we entered or exited protected mode.
	;------
prot_mode_entry_exit_r0
	str		r0, [r1, #0]						; Save the general register value to CR0
	strb	r0, [sp, #SP_CPU_CR0]
	IF 1 = 1
	mov		r1, r0
	mrs		r0, cpsr							; Save flags to r0
	tst		r1, #0x80000000						; Paging on?
	beq		restore_flags_from_r0				; Nope, all OK.
	ldr		r0, =BRUnsVM
	ldr		r1, =BreakReason
	str		r0, [r1]
	b		unknown							; TODO! Paging support!
	ELSE
	lw		t1, GP_CPU_CR0(gp)
	sw		t0, GP_CPU_CR0(gp)
	xor		t1, t0
	bgez	t1, 1f								; Jump if paging mode did not change
	;------
	; Paging mode has changed!
	;------
	srl		t0, 31
	la		t1, PagingEnabled
	la		t2, PagingDisabled
	movz	t1, t2, t0
	jalr	t1									; Call the paging helper subroutine
	;------
	; Protected mode might have changed!
	;------
1:	fix_opcode_table_pointers_t0t2
	j		loop
	ENDIF

mov_CR0_ecx
	mov		r0, ecx
	b		prot_mode_entry_exit_r0
mov_CR0_edx
	mov		r0, edx
	b		prot_mode_entry_exit_r0
mov_CR0_ebx
	mov		r0, ebx
	b		prot_mode_entry_exit_r0
mov_CR0_esp
	mov		r0, esp
	b		prot_mode_entry_exit_r0
mov_CR0_ebp
	mov		r0, ebp
	b		prot_mode_entry_exit_r0
mov_CR0_esi
	mov		r0, esi
	b		prot_mode_entry_exit_r0
mov_CR0_edi
	mov		r0, edi
	b		prot_mode_entry_exit_r0

mov_CR2_eax
	str		eax, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_ecx
	str		ecx, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_edx
	str		edx, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_ebx
	str		ebx, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_esp
	str		esp, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_ebp
	str		ebp, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_esi
	str		esi, [r1, #4]						; Save the general register value to CR2
	b		loop
mov_CR2_edi
	str		edi, [r1, #4]						; Save the general register value to CR2
	b		loop

mov_CR3_eax
	mov		r0, eax
mov_CR3_t1
	str		r0, [r1, #8]						; Save the general register value to CR3
	IF 1 = 1
	ldr		r0, =BRUnsVM
	ldr		r1, =BreakReason
	str		r0, [r1]
	b		unknown							; TODO! Paging support!
	ELSE
	la		t0, INTVectors
	sw		t1, GP_Paging_CR3(gp)
	lw		t2, GP_CPU_CR0(gp)
	addu	t0, t1
	sw		t0, GP_Paging_CR3_Phys(gp)
	bltz	t2, 1f								; Jump if paging is currently enabled.
	j		loop								; Paging not enabled, just return to loop.
1:	la		ra, loop
	j		ClearTLB							; Clear the EMSPages TLB when CR3 register gets set and paging is enabled.
	ENDIF

mov_CR3_ecx
	mov		r0, ecx
	b		mov_CR3_t1
mov_CR3_edx
	mov		r0, edx
	b		mov_CR3_t1
mov_CR3_ebx
	mov		r0, ebx
	b		mov_CR3_t1
mov_CR3_esp
	mov		r0, esp
	b		mov_CR3_t1
mov_CR3_ebp
	mov		r0, ebp
	b		mov_CR3_t1
mov_CR3_esi
	mov		r0, esi
	b		mov_CR3_t1
mov_CR3_edi
	mov		r0, edi
	b		mov_CR3_t1

; ------------------- 0F 23 = MOV DRx,Rd -----------------------------
;
	GLOBAL	op_0f_23
op_0f_23
	ldrb	r1,[r12],#1							; Load the second opcode byte to r1, increment r12 by 1
	mrs		r0, cpsr							; Save flags to r0
	;------
	; Calculate r2 = the cpu_DRx variable address
	;------
	ldr		r2, =cpu_DRx						; r2 = address of the cpu_DRx table
	mov		r3, r1, lsr #3
	and		r3, #7								; r3=which=(rm >> 3) & 7;
	cmp		r3, #4
	orrge	r3, #2								; Make 4 => 6 and 5 => 7 
	add		r2, r3, lsl #2
	;------
	; Handle the correct output register
	;------
	and		r1, #7
	;------
	; Register handlers, each must be 8 bytes in size!
	;------	
	cmp		r1, #0
	streq	eax, [r2]	
	beq		restore_flags_from_r0
	cmp		r1, #1
	streq	ecx, [r2]	
	beq		restore_flags_from_r0
	cmp		r1, #2
	streq	edx, [r2]	
	beq		restore_flags_from_r0
	cmp		r1, #3
	streq	ebx, [r2]	
	beq		restore_flags_from_r0
	cmp		r1, #4
	streq	esp, [r2]
	beq		restore_flags_from_r0
	cmp		r1, #5
	streq	ebp, [r2]	
	beq		restore_flags_from_r0
	cmp		r1, #6
	streq	esi, [r2]	
	beq		restore_flags_from_r0
	str		edi, [r2]	
	b		restore_flags_from_r0

	LTORG
	
; ------------------- 0F 80 .. 0F 8F = conditional jumps -------------

	MACRO
	cond_jump $cnd
	ldrb	r1,[r12],#1
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	orr		r0, r1, r0, lsl #8		; r0 = signed halfword
	add$cnd	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	b		loop
	MEND

jo_imm16								; JO cw
	cond_jump vs
jno_imm16								; JNO cw
	cond_jump vc
jc_imm16								; JC/JB/JNAE cw
	cond_jump cs
jnc_imm16								; JNC/JNB/JAE cw
	cond_jump cc
jz_imm16								; JZ/JE cw
	cond_jump eq
jnz_imm16								; JNZ/JNE cw
	cond_jump ne
jbe_imm16								; JBE/JNA cw (CF=1 or ZF=1)
	ldrb	r1,[r12],#1
	ldrsb	r0,[r12],#1					; Load sign-extended byte to r0, increment r12 by 1
	orr		r0, r1, r0, lsl #8			; r0 = signed halfword
	addcs	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	bcs		loop
	addeq	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	b		loop
ja_imm16								; JA/JNBE cw (CF=0 and ZF=0)
	ldrb	r1,[r12],#1
	ldrsb	r0,[r12],#1					; Load sign-extended byte to r0, increment r12 by 1
	orr		r0, r1, r0, lsl #8			; r0 = signed halfword
	bcs		loop
	addne	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	b		loop
js_imm16								; JS cw
	cond_jump mi
jns_imm16								; JNS cw
	cond_jump pl
jl_imm16								; JL/JNGE cw (SF != OF)
	cond_jump lt
jge_imm16								; JNL/JGE cw (SF == OF)
	cond_jump ge
jle_imm16								; JLE/JNG cw (ZF == 1 or SF != OF)
	cond_jump le
jg_imm16								; JG/JNLE cw (ZF == 0 and SF == OF)
	cond_jump gt

jpe_imm16								; JP/JPE cb
	b		unsjpe

	;-------
	; JNP/JPO cb
	; Special hack for "Chess Genius 3"
	;	1B14:A07C F6C582          test ch,82
	;	1B14:A07F 0F896BFF        jns  00009FEE ($-95)
	;	1B14:A083 0F8B67FF        jnp  00009FEE ($-99)
	;-------
jpo_imm16							; JNP/JPO cb
	mrs		r0,cpsr					; r0 = Current flags
	ldrb	r1,[r12, #-4]
	cmp		r1, #0x6B
	beq		%f1						; Could be "Chess genius 3"...
	b		unsjpo
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:A07C F6C582          test ch,82
	;	1B14:A07F 0F896BFF        jns  00009FEE ($-95)
	;	1B14:A083 0F8B67FF        jnp  00009FEE ($-99)
	;-------
1	ldrb	r1,[r12, #-3]
	cmp		r1, #0xFF
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x89
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0x0F
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x82
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xC5
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test ch,82" result into r1
	;-------
	mov		r1, ecx, lsr #8
	and		r1, #0x82
	b		op_0f8b_r1

op_0f8b_r1
	and		r1, #0xFF
	eor 	r1,r1,r1,lsl#16
	eor 	r1,r1,r1,lsl#8
	eor 	r1,r1,r1,lsl#4
	eor 	r1,r1,r1,lsl#2
	eors 	r1,r1,r1,lsl#1
	ldrb	r1,[r12],#1
	ldrsb	r2,[r12],#1					; Load sign-extended byte to r2, increment r12 by 1
	orr		r1, r2, lsl #8				; r1 = signed halfword
	addmi	r12, r12, r1				; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0		; Back to loop, restoring flags
	

	GLOBAL jo_imm16
	GLOBAL jno_imm16
	GLOBAL jc_imm16
	GLOBAL jnc_imm16
	GLOBAL jz_imm16
	GLOBAL jnz_imm16
	GLOBAL jbe_imm16
	GLOBAL ja_imm16
	GLOBAL js_imm16
	GLOBAL jns_imm16
	GLOBAL jpe_imm16
	GLOBAL jpo_imm16
	GLOBAL jl_imm16
	GLOBAL jge_imm16
	GLOBAL jle_imm16
	GLOBAL jg_imm16

; ------------------- 0F 90 .. 0F 9F = SETcc r/m8 --------------------
;
	MACRO
	setccm $cond
	mov		r1, #0
	mov$cond r1, #1								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	MEND

op_0f_90										; SETO
	setccm	vs
	
op_0f_91										; SETNO
	setccm	vc

op_0f_92										; SETC
	setccm	cs

op_0f_93										; SETNC
	setccm	cc

op_0f_94										; SETZ
	setccm	eq
	
op_0f_95										; SETNZ
	setccm	ne

op_0f_96										; SETBE (CF=1 or ZF=1)
	mov		r1, #0
	movcs 	r1, #1								
	moveq	r1, #1								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_0f_97										; SETA (CF=0 and ZF=0)
	mov		r1, #1
	movcs 	r1, #0								
	moveq	r1, #0								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_0f_98										; SETS
	setccm	mi

op_0f_99										; SETNS
	setccm	pl

op_0f_9a										; SETP
	b		unsjpe

op_0f_9b										; SETNP
	b		unsjpo

op_0f_9C										; SETL
	setccm	lt

op_0f_9D										; SETGE
	setccm	ge

op_0f_9E										; SETLE
	setccm	le

op_0f_9F										; SETG
	setccm	gt

	LTORG
	
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
setcc_table
; 0x00	
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
	modrm_help_1_0 setcc
; 0x40
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
	modrm_help_1_40 setcc
; 0x80
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
	modrm_help_1_80 setcc
; 0xC0
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh
	DCD setcc_al, setcc_cl, setcc_dl, setcc_bl, setcc_ah, setcc_ch, setcc_dh, setcc_bh

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	modrm_1_help setcc, setcc_r0

	GLOBAL	setcc_r0_bp
setcc_r0_bp
	mem_handler_bp
	GLOBAL	setcc_r0
setcc_r0
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	strb	r1, [r2]							; Save it to the result byte in memory
	b		loop								; All done, back to loop
	
setcc_al
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		eax, r1, #0, #8
	b		loop
setcc_cl
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		ecx, r1, #0, #8
	b		loop
setcc_dl
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		edx, r1, #0, #8
	b		loop
setcc_bl
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		ebx, r1, #0, #8
	b		loop
setcc_ah
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		eax, r1, #8, #8
	b		loop
setcc_ch
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		ecx, r1, #8, #8
	b		loop
setcc_dh
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		edx, r1, #8, #8
	b		loop
setcc_bh
	ldrb	r1, [sp, #SP_STR_SEG]				; r1 = condition test result, 0 / 1
	bfi		ebx, r1, #8, #8
	b		loop

	GLOBAL setcc_al
	GLOBAL setcc_cl
	GLOBAL setcc_dl
	GLOBAL setcc_bl
	GLOBAL setcc_ah
	GLOBAL setcc_ch
	GLOBAL setcc_dh
	GLOBAL setcc_bh
	GLOBAL op_0f_90
	GLOBAL op_0f_91
	GLOBAL op_0f_92
	GLOBAL op_0f_93
	GLOBAL op_0f_94
	GLOBAL op_0f_95
	GLOBAL op_0f_96
	GLOBAL op_0f_97
	GLOBAL op_0f_98
	GLOBAL op_0f_99
	GLOBAL op_0f_9a
	GLOBAL op_0f_9b
	GLOBAL op_0f_9C
	GLOBAL op_0f_9D
	GLOBAL op_0f_9E
	GLOBAL op_0f_9F

; ------------------- 0F A0 / 0F A1 = PUSH/POP FS --------------------

	GLOBAL	push_fs
push_fs
	ldr		r1, [sp, #SP_FS_VALUE]	
	push_hword r1, r0, r2
	b		loop

	GLOBAL	pop_fs
pop_fs
	pop_reg_low_tmp r2, r1
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		mov_fs_r0r2_prot		; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_FS_VALUE]
	str		r1, [sp, #SP_FS_BASE]
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

; ------------------- 0F A3 = BT r/m16, r16 --------------------------
;
op_0f_a3
	modrm_jump_16_tbl op_0f_a3_jump
	modrm_tbl_1 bt_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	bt_r0_reg $reg
	GLOBAL	bt_r0_r16_bp_$reg
bt_r0_r16_bp_$reg
	mem_handler_bp
	GLOBAL	bt_r0_r16_$reg
bt_r0_r16_$reg
	;------
	;	CASE_0F_W(0xa3)												/* BT Ew,Gw */
	;		{
	;			FillFlags();GetRMrw;
	;			Bit16u mask=1 << (*rmrw & 15);
	;			if (rm >= 0xc0 ) {
	;				GetEArw;
	;				SETFLAGBIT(CF,(*earw & mask));
	;			} else {
	;				GetEAa;eaa+=(((Bit16s)*rmrw)>>4)*2;
	;				Bit16u old=LoadMw(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, lsl #16
	lsr		r1, #16+4
	add		r0, r1, lsl #1						;	eaa+=(((Bit16s)*rmrw)>>4)*2;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r0, [r2]
	ldrb	r2, [r2, #1]
	and		r1, $reg, #0x0F
	orr		r2, r0, r2, lsl #8					; r2 = halfword from RAM
	mrs		r0, cpsr							; Save flags to r0
	mov		r1, r2, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	bt_r0_reg eax
	bt_r0_reg ecx
	bt_r0_reg edx
	bt_r0_reg ebx
	bt_r0_reg esp
	bt_r0_reg ebp
	bt_r0_reg esi
	bt_r0_reg edi

	MACRO
	bt_r16_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x0F
	mov		r1, $reg1, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper bt_r16, eax
	modrm_gen_reg_oper bt_r16, ecx
	modrm_gen_reg_oper bt_r16, edx
	modrm_gen_reg_oper bt_r16, ebx
	modrm_gen_reg_oper bt_r16, esp
	modrm_gen_reg_oper bt_r16, ebp
	modrm_gen_reg_oper bt_r16, esi
	modrm_gen_reg_oper bt_r16, edi

	modrm_1_genall bt_r16, bt_r0_r16

; ------------------- 0F A4 = SHLD r/m16, r16, imm8 ------------------
; Overflow flag is undefined
op_0f_a4
	modrm_jump_16_tbl op_0f_a4_jump
	modrm_tbl_1 shld_r16_imm8

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	shld_r0_r16_imm8 $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shld_r0_r16_imm8_bp_$reg
shld_r0_r16_imm8_bp_$reg
	mem_handler_bp
	GLOBAL	shld_r0_r16_imm8_$reg
shld_r0_r16_imm8_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r3, [r2]
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	ldrb	r0, [r2, #1]
	orr		r3, $reg, lsl #16		; 
	orr		r3, r0, lsl #8			; r3 = op1 | op2<<16
	ror		r3, #16					; r3 = (op1<<16)|op2
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16
	lsl		r3, #16
	orr		r3, r3, lsr #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsr r0
	mov		r3, r3, lsl r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsl r1			; Shift the value left, the flags are set according to the result.
	lsr		r3, #16					; High 16 bits contain the result
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		loop
	MEND

	shld_r0_r16_imm8 eax
	shld_r0_r16_imm8 ecx
	shld_r0_r16_imm8 edx
	shld_r0_r16_imm8 ebx
	shld_r0_r16_imm8 esp
	shld_r0_r16_imm8 ebp
	shld_r0_r16_imm8 esi
	shld_r0_r16_imm8 edi
	
	MACRO
	shld_r16_imm8_reg16_reg16 $reg1, $reg2
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	mov		r3, $reg1, lsl #16
	mov		r2, $reg2, lsl #16
	orr		r3, r2, lsr #16			; r3 = (op1<<16)|op2
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16					; r1 = rotation count - 16
	lsl		r3, #16
	orr		r3, r3, lsr #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsr r0
	mov		r3, r3, lsl r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsl r1			; Shift the value left, the flags are set according to the result.
	orr		$reg1, r3, $reg1, lsr #16
	ror		$reg1, #16
	b		loop
	MEND

	modrm_gen_reg_oper shld_r16_imm8, eax
	modrm_gen_reg_oper shld_r16_imm8, ecx
	modrm_gen_reg_oper shld_r16_imm8, edx
	modrm_gen_reg_oper shld_r16_imm8, ebx
	modrm_gen_reg_oper shld_r16_imm8, esp
	modrm_gen_reg_oper shld_r16_imm8, ebp
	modrm_gen_reg_oper shld_r16_imm8, esi
	modrm_gen_reg_oper shld_r16_imm8, edi

	modrm_1_genall shld_r16_imm8, shld_r0_r16_imm8

; ------------------- 0F A5 = SHLD r/m16, r16, CL ------------------
; Overflow flag is undefined
op_0f_a5
	modrm_jump_16_tbl op_0f_a5_jump
	modrm_tbl_1 shld_r16_CL

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	shld_r0_r16_CL $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shld_r0_r16_CL_bp_$reg
shld_r0_r16_CL_bp_$reg
	mem_handler_bp
	GLOBAL	shld_r0_r16_CL_$reg
shld_r0_r16_CL_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r3, [r2]
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	ldrb	r0, [r2, #1]
	orr		r3, $reg, lsl #16		; 
	orr		r3, r0, lsl #8			; r3 = op1 | op2<<16
	ror		r3, #16					; r3 = (op1<<16)|op2
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16
	lsl		r3, #16
	orr		r3, r3, lsr #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsr r0
	mov		r3, r3, lsl r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsl r1			; Shift the value left, the flags are set according to the result.
	lsr		r3, #16					; High 16 bits contain the result
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		loop
	MEND

	shld_r0_r16_CL eax
	shld_r0_r16_CL ecx
	shld_r0_r16_CL edx
	shld_r0_r16_CL ebx
	shld_r0_r16_CL esp
	shld_r0_r16_CL ebp
	shld_r0_r16_CL esi
	shld_r0_r16_CL edi
	
	MACRO
	shld_r16_CL_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	mov		r3, $reg1, lsl #16
	mov		r2, $reg2, lsl #16
	orr		r3, r2, lsr #16			; r3 = (op1<<16)|op2
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16					; r1 = rotation count - 16
	lsl		r3, #16
	orr		r3, r3, lsr #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsr r0
	mov		r3, r3, lsl r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsl r1			; Shift the value left, the flags are set according to the result.
	orr		$reg1, r3, $reg1, lsr #16
	ror		$reg1, #16
	b		loop
	MEND

	modrm_gen_reg_oper shld_r16_CL, eax
	modrm_gen_reg_oper shld_r16_CL, ecx
	modrm_gen_reg_oper shld_r16_CL, edx
	modrm_gen_reg_oper shld_r16_CL, ebx
	modrm_gen_reg_oper shld_r16_CL, esp
	modrm_gen_reg_oper shld_r16_CL, ebp
	modrm_gen_reg_oper shld_r16_CL, esi
	modrm_gen_reg_oper shld_r16_CL, edi

	modrm_1_genall shld_r16_CL, shld_r0_r16_CL

; ------------------- 0F A8 / 0F A9 = PUSH/POP GS --------------------

	GLOBAL	push_gs
push_gs
	ldr		r1, [sp, #SP_GS_VALUE]	
	push_hword r1, r0, r2
	b		loop

	GLOBAL	pop_gs
pop_gs
	pop_reg_low_tmp r2, r1
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_gs_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_GS_VALUE]
	str		r1, [sp, #SP_GS_BASE]
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

; ------------------- 0F AB = BTS r/m16, r16 --------------------------
;
op_0f_ab
	modrm_jump_16_tbl op_0f_ab_jump
	modrm_tbl_1 bts_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	bts_r0_reg $reg
	GLOBAL	bts_r0_r16_bp_$reg
bts_r0_r16_bp_$reg
	mem_handler_bp
	GLOBAL	bts_r0_r16_$reg
bts_r0_r16_$reg
	;------
	;	CASE_0F_W(0xa3)												/* BT Ew,Gw */
	;		{
	;			FillFlags();GetRMrw;
	;			Bit16u mask=1 << (*rmrw & 15);
	;			if (rm >= 0xc0 ) {
	;				GetEArw;
	;				SETFLAGBIT(CF,(*earw & mask));
	;				*earw|=mask;
	;			} else {
	;				GetEAa;eaa+=(((Bit16s)*rmrw)>>4)*2;
	;				Bit16u old=LoadMw(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;				SaveMw(eaa,old | mask);
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, lsl #16
	lsr		r1, #16+4
	add		r0, r1, lsl #1						;	eaa+=(((Bit16s)*rmrw)>>4)*2;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	and		r1, $reg, #0x0F
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	orr		r3, r1								; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop
	MEND

	bts_r0_reg eax
	bts_r0_reg ecx
	bts_r0_reg edx
	bts_r0_reg ebx
	bts_r0_reg esp
	bts_r0_reg ebp
	bts_r0_reg esi
	bts_r0_reg edi

	MACRO
	bts_r16_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x0F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg1, r1							; Is the bit set?
	orr		$reg1, r1							; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper bts_r16, eax
	modrm_gen_reg_oper bts_r16, ecx
	modrm_gen_reg_oper bts_r16, edx
	modrm_gen_reg_oper bts_r16, ebx
	modrm_gen_reg_oper bts_r16, esp
	modrm_gen_reg_oper bts_r16, ebp
	modrm_gen_reg_oper bts_r16, esi
	modrm_gen_reg_oper bts_r16, edi

	modrm_1_genall bts_r16, bts_r0_r16

; ------------------- 0F AC = SHRD r/m16, r16, imm8 ------------------
; Overflow flag is undefined
op_0f_ac
	modrm_jump_16_tbl op_0f_ac_jump
	modrm_tbl_1 shrd_r16_imm8

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	shrd_r0_r16_imm8 $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shrd_r0_r16_imm8_bp_$reg
shrd_r0_r16_imm8_bp_$reg
	mem_handler_bp
	GLOBAL	shrd_r0_r16_imm8_$reg
shrd_r0_r16_imm8_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r3, [r2]
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	ldrb	r0, [r2, #1]
	orr		r3, $reg, lsl #16		; 
	orr		r3, r0, lsl #8			; r3 = (op2<<16)|op1
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16
	lsr		r3, #16
	orr		r3, r3, lsl #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsl r0
	mov		r3, r3, lsr r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsr r1			; Shift the value right, the flags are set according to the result.
	mrs		r0, cpsr				; Save current flags
	tst		r3, #0x8000				; Should we set the sign flag on?
	orrne	r0, #ARM_NEG
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0
	MEND

	shrd_r0_r16_imm8 eax
	shrd_r0_r16_imm8 ecx
	shrd_r0_r16_imm8 edx
	shrd_r0_r16_imm8 ebx
	shrd_r0_r16_imm8 esp
	shrd_r0_r16_imm8 ebp
	shrd_r0_r16_imm8 esi
	shrd_r0_r16_imm8 edi
	
	MACRO
	shrd_r16_imm8_reg16_reg16 $reg1, $reg2
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	mov		r3, $reg1, lsl #16
	mov		r2, $reg2, lsl #16
	orr		r3, r2, r3, lsr #16		; r3 = (op2<<16)|op1
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16					; r1 = rotation count - 16
	lsr		r3, #16
	orr		r3, r3, lsl #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsl r0
	mov		r3, r3, lsr r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsr r1			; Shift the value right, the flags are set according to the result.
	mrs		r0, cpsr				; Save current flags
	tst		r3, #0x8000				; Should we set the sign flag on?
	orrne	r0, #ARM_NEG
	lsr		$reg1, #16
	orr		$reg1, r3, $reg1, lsl #16
	b		restore_flags_from_r0
	MEND

	modrm_gen_reg_oper shrd_r16_imm8, eax
	modrm_gen_reg_oper shrd_r16_imm8, ecx
	modrm_gen_reg_oper shrd_r16_imm8, edx
	modrm_gen_reg_oper shrd_r16_imm8, ebx
	modrm_gen_reg_oper shrd_r16_imm8, esp
	modrm_gen_reg_oper shrd_r16_imm8, ebp
	modrm_gen_reg_oper shrd_r16_imm8, esi
	modrm_gen_reg_oper shrd_r16_imm8, edi

	modrm_1_genall shrd_r16_imm8, shrd_r0_r16_imm8

; ------------------- 0F AD = SHRD r/m16, r16, CL ------------------
; Overflow flag is undefined
op_0f_ad
	modrm_jump_16_tbl op_0f_ad_jump
	modrm_tbl_1 shrd_r16_CL

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	shrd_r0_r16_CL $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shrd_r0_r16_CL_bp_$reg
shrd_r0_r16_CL_bp_$reg
	mem_handler_bp
	GLOBAL	shrd_r0_r16_CL_$reg
shrd_r0_r16_CL_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r3, [r2]
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	ldrb	r0, [r2, #1]
	orr		r3, $reg, lsl #16		; 
	orr		r3, r0, lsl #8			; r3 = (op2<<16)|op1
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16
	lsr		r3, #16
	orr		r3, r3, lsl #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsl r0
	mov		r3, r3, lsr r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsr r1			; Shift the value right, the flags are set according to the result.
	mrs		r0, cpsr				; Save current flags
	tst		r3, #0x8000				; Should we set the sign flag on?
	orrne	r0, #ARM_NEG
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0
	MEND

	shrd_r0_r16_CL eax
	shrd_r0_r16_CL ecx
	shrd_r0_r16_CL edx
	shrd_r0_r16_CL ebx
	shrd_r0_r16_CL esp
	shrd_r0_r16_CL ebp
	shrd_r0_r16_CL esi
	shrd_r0_r16_CL edi
	
	MACRO
	shrd_r16_CL_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	mov		r3, $reg1, lsl #16
	mov		r2, $reg2, lsl #16
	orr		r3, r2, r3, lsr #16		; r3 = (op2<<16)|op1
	rsbs	r0, r1, #16				; r0 = 16 - r1 (if negative, rotation count was > 16!)
	bge		%f1
	;------
	; Rotation count > 16
	;------
	sub		r1, #16					; r1 = rotation count - 16
	lsr		r3, #16
	orr		r3, r3, lsl #16			; r3 = (op2<<16)|op2, since all bits come from the second operand
	rsb		r0, r1, #16				; r0 = 16 - r1
	;------
	; Rotation count <= 16
	;------
1	mov		r3, r3, lsl r0
	mov		r3, r3, lsr r0			; Clean up the bits we will not shift into the result
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	movs	r3, r3, lsr r1			; Shift the value right, the flags are set according to the result.
	mrs		r0, cpsr				; Save current flags
	tst		r3, #0x8000				; Should we set the sign flag on?
	orrne	r0, #ARM_NEG
	lsr		$reg1, #16
	orr		$reg1, r3, $reg1, lsl #16
	b		restore_flags_from_r0
	MEND

	modrm_gen_reg_oper shrd_r16_CL, eax
	modrm_gen_reg_oper shrd_r16_CL, ecx
	modrm_gen_reg_oper shrd_r16_CL, edx
	modrm_gen_reg_oper shrd_r16_CL, ebx
	modrm_gen_reg_oper shrd_r16_CL, esp
	modrm_gen_reg_oper shrd_r16_CL, ebp
	modrm_gen_reg_oper shrd_r16_CL, esi
	modrm_gen_reg_oper shrd_r16_CL, edi

	modrm_1_genall shrd_r16_CL, shrd_r0_r16_CL

; ------------------- 0F AF = IMUL r16, r/m16 ------------------------
op_0f_af
	modrm_jump_16_tbl op_0f_af_jump
	modrm_tbl_3 imul_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	imul_reg16_r0 $reg
	GLOBAL	imul_r16_r0_bp_$reg
imul_r16_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	imul_r16_r0_$reg
imul_r16_r0_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r0, [r2]
	ldrsb	r1, [r2, #1]
	orr		r0, r1, lsl #8
imul_reg16_r0_$reg
	sbfx	r1, $reg, #0, #16
	mul		r2, r0, r1					; Signed multiply
	mov		r0, #0						; Clear all flags
	bfi		$reg, r2, #0, #16
	sbfx	r1, r2, #15, #1				; Fill r1 with the sign bit of the result
	cmp		r1, r2, asr #16				; Is the r2 high halfword equal to the sign of the result?
	orrne	r0, #(ARM_OVER|ARM_CARRY)	; If not, we got an overflow.
	b		restore_flags_from_r0
	MEND

	imul_reg16_r0 eax
	imul_reg16_r0 ecx
	imul_reg16_r0 edx
	imul_reg16_r0 ebx
	imul_reg16_r0 esp
	imul_reg16_r0 ebp
	imul_reg16_r0 esi
	imul_reg16_r0 edi

	MACRO
	imul_r16_reg16_reg16 $rl, $rr
	sbfx	r0, $rr, #0, #16
	b		imul_reg16_r0_$rl
	MEND

	modrm_3_genall imul_r16, imul_r16_r0

; ------------------- 0F B2 = LSS r16, r/m16 -------------------------
; 0FB26704 = lss reg,word [bx+04] (ZOOL2, REAL, USE16)
op_0f_b2
	modrm_jump_16_tbl op_0f_b2_jump
; 0
	modrm_help_3_0 lss_r16, eax
	modrm_help_3_0 lss_r16, ecx
	modrm_help_3_0 lss_r16, edx
	modrm_help_3_0 lss_r16, ebx
	modrm_help_3_0 lss_r16, esp
	modrm_help_3_0 lss_r16, ebp
	modrm_help_3_0 lss_r16, esi
	modrm_help_3_0 lss_r16, edi
; 0x40
	modrm_help_3_40 lss_r16, eax
	modrm_help_3_40 lss_r16, ecx
	modrm_help_3_40 lss_r16, edx
	modrm_help_3_40 lss_r16, ebx
	modrm_help_3_40 lss_r16, esp
	modrm_help_3_40 lss_r16, ebp
	modrm_help_3_40 lss_r16, esi
	modrm_help_3_40 lss_r16, edi
; 0x80
	modrm_help_3_80 lss_r16, eax
	modrm_help_3_80 lss_r16, ecx
	modrm_help_3_80 lss_r16, edx
	modrm_help_3_80 lss_r16, ebx
	modrm_help_3_80 lss_r16, esp
	modrm_help_3_80 lss_r16, ebp
	modrm_help_3_80 lss_r16, esi
	modrm_help_3_80 lss_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	lss_r16_r0 $reg
	GLOBAL	lss_r16_r0_bp_$reg
lss_r16_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lss_r16_r0_$reg
lss_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	; Load offset and segment
	;-------
1	ldrb	r0, [r2] 							; Load low byte of offset
	ldrb	r1, [r2, #1]						; Load high byte of offset
	ldrb	r3, [r2, #2]						; Load low byte of SS from [r2+2]
	ldrb	r2, [r2, #3]						; Load high byte of SS from [r2+3]
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	orr		r2, r3, r2, lsl #8					; r2 = low byte | (high byte << 8) = new SS value
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_ss_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so setup the segment base.
	;-------
	msr		cpsr_f, r0							; Restore flags
	str		r2,[sp, #SP_SS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2,[sp, #SP_SS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]
	b		loop
	MEND

	EXTERN	mov_ss_r0r2_prot
	
	lss_r16_r0 eax
	lss_r16_r0 ecx
	lss_r16_r0 edx
	lss_r16_r0 ebx
	lss_r16_r0 esp
	lss_r16_r0 ebp
	lss_r16_r0 esi
	lss_r16_r0 edi

	MACRO
	lss_r16_reg16_reg16 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lss_r16, lss_r16_r0

; ------------------- 0F B3 = BTR r/m16, r16 --------------------------
;
op_0f_b3
	modrm_jump_16_tbl op_0f_b3_jump
	modrm_tbl_1 btr_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	btr_r0_reg $reg
	GLOBAL	btr_r0_r16_bp_$reg
btr_r0_r16_bp_$reg
	mem_handler_bp
	GLOBAL	btr_r0_r16_$reg
btr_r0_r16_$reg
	;------
	;	CASE_0F_W(0xa3)												/* BT Ew,Gw */
	;		{
	;			FillFlags();GetRMrw;
	;			Bit16u mask=1 << (*rmrw & 15);
	;			if (rm >= 0xc0 ) {
	;				GetEArw;
	;				SETFLAGBIT(CF,(*earw & mask));
	;				*earw&=~mask;
	;			} else {
	;				GetEAa;eaa+=(((Bit16s)*rmrw)>>4)*2;
	;				Bit16u old=LoadMw(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;				SaveMw(eaa,old&~mask);
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, lsl #16
	lsr		r1, #16+4
	add		r0, r1, lsl #1						;	eaa+=(((Bit16s)*rmrw)>>4)*2;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	and		r1, $reg, #0x0F
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	bic		r3, r1								; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop
	MEND

	btr_r0_reg eax
	btr_r0_reg ecx
	btr_r0_reg edx
	btr_r0_reg ebx
	btr_r0_reg esp
	btr_r0_reg ebp
	btr_r0_reg esi
	btr_r0_reg edi

	MACRO
	btr_r16_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x0F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg1, r1							; Is the bit set?
	bic		$reg1, r1							; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper btr_r16, eax
	modrm_gen_reg_oper btr_r16, ecx
	modrm_gen_reg_oper btr_r16, edx
	modrm_gen_reg_oper btr_r16, ebx
	modrm_gen_reg_oper btr_r16, esp
	modrm_gen_reg_oper btr_r16, ebp
	modrm_gen_reg_oper btr_r16, esi
	modrm_gen_reg_oper btr_r16, edi

	modrm_1_genall btr_r16, btr_r0_r16

; ------------------- 0F B4 = LFS r16, r/m16 -------------------------
;
op_0f_b4
	modrm_jump_16_tbl op_0f_b4_jump
; 0
	modrm_help_3_0 lfs_r16, eax
	modrm_help_3_0 lfs_r16, ecx
	modrm_help_3_0 lfs_r16, edx
	modrm_help_3_0 lfs_r16, ebx
	modrm_help_3_0 lfs_r16, esp
	modrm_help_3_0 lfs_r16, ebp
	modrm_help_3_0 lfs_r16, esi
	modrm_help_3_0 lfs_r16, edi
; 0x40
	modrm_help_3_40 lfs_r16, eax
	modrm_help_3_40 lfs_r16, ecx
	modrm_help_3_40 lfs_r16, edx
	modrm_help_3_40 lfs_r16, ebx
	modrm_help_3_40 lfs_r16, esp
	modrm_help_3_40 lfs_r16, ebp
	modrm_help_3_40 lfs_r16, esi
	modrm_help_3_40 lfs_r16, edi
; 0x80
	modrm_help_3_80 lfs_r16, eax
	modrm_help_3_80 lfs_r16, ecx
	modrm_help_3_80 lfs_r16, edx
	modrm_help_3_80 lfs_r16, ebx
	modrm_help_3_80 lfs_r16, esp
	modrm_help_3_80 lfs_r16, ebp
	modrm_help_3_80 lfs_r16, esi
	modrm_help_3_80 lfs_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	lfs_r16_r0 $reg
	GLOBAL	lfs_r16_r0_bp_$reg
lfs_r16_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lfs_r16_r0_$reg
lfs_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	; ----- Load offset
1	ldrb	r0, [r2] 				; Load low byte of offset
	ldrb	r1, [r2, #1]			; Load high byte of offset
	bfi		$reg, r0, #0, #8
	; ----- Load segment
	ldrb	r0,[r2, #2]				; Load low byte of FS from [r2+2]
	ldrb	r2,[r2, #3]				; Load high byte of FS from [r2+3]
	bfi		$reg, r1, #8, #8		; Put offset to result register
	orr		r2, r0, r2, lsl #8		; r2 = low byte | (high byte << 8) = new FS value
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		mov_fs_r0r2_prot		; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_FS_VALUE]	
	str		r1, [sp, #SP_FS_BASE]	
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
	MEND

	EXTERN	mov_fs_r0r2_prot
	
	lfs_r16_r0 eax
	lfs_r16_r0 ecx
	lfs_r16_r0 edx
	lfs_r16_r0 ebx
	lfs_r16_r0 esp
	lfs_r16_r0 ebp
	lfs_r16_r0 esi
	lfs_r16_r0 edi

	MACRO
	lfs_r16_reg16_reg16 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lfs_r16, lfs_r16_r0

; ------------------- 0F B5 = LGS r16, r/m16 -------------------------
;
op_0f_b5
	modrm_jump_16_tbl op_0f_b5_jump
; 0
	modrm_help_3_0 lgs_r16, eax
	modrm_help_3_0 lgs_r16, ecx
	modrm_help_3_0 lgs_r16, edx
	modrm_help_3_0 lgs_r16, ebx
	modrm_help_3_0 lgs_r16, esp
	modrm_help_3_0 lgs_r16, ebp
	modrm_help_3_0 lgs_r16, esi
	modrm_help_3_0 lgs_r16, edi
; 0x40
	modrm_help_3_40 lgs_r16, eax
	modrm_help_3_40 lgs_r16, ecx
	modrm_help_3_40 lgs_r16, edx
	modrm_help_3_40 lgs_r16, ebx
	modrm_help_3_40 lgs_r16, esp
	modrm_help_3_40 lgs_r16, ebp
	modrm_help_3_40 lgs_r16, esi
	modrm_help_3_40 lgs_r16, edi
; 0x80
	modrm_help_3_80 lgs_r16, eax
	modrm_help_3_80 lgs_r16, ecx
	modrm_help_3_80 lgs_r16, edx
	modrm_help_3_80 lgs_r16, ebx
	modrm_help_3_80 lgs_r16, esp
	modrm_help_3_80 lgs_r16, ebp
	modrm_help_3_80 lgs_r16, esi
	modrm_help_3_80 lgs_r16, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	lgs_r16_r0 $reg
	GLOBAL	lgs_r16_r0_bp_$reg
lgs_r16_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lgs_r16_r0_$reg
lgs_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	; ----- Load offset
1	ldrb	r0, [r2] 				; Load low byte of offset
	ldrb	r1, [r2, #1]			; Load high byte of offset
	bfi		$reg, r0, #0, #8
	; ----- Load segment
	ldrb	r0,[r2, #2]				; Load low byte of GS from [r2+2]
	ldrb	r2,[r2, #3]				; Load high byte of GS from [r2+3]
	bfi		$reg, r1, #8, #8		; Put offset to result register
	orr		r2, r0, r2, lsl #8		; r2 = low byte | (high byte << 8) = new GS value
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_gs_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_GS_VALUE]
	str		r1, [sp, #SP_GS_BASE]
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
	MEND

	EXTERN	mov_gs_r0r2_prot
	
	lgs_r16_r0 eax
	lgs_r16_r0 ecx
	lgs_r16_r0 edx
	lgs_r16_r0 ebx
	lgs_r16_r0 esp
	lgs_r16_r0 ebp
	lgs_r16_r0 esi
	lgs_r16_r0 edi

	MACRO
	lgs_r16_reg16_reg16 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lgs_r16, lgs_r16_r0

; ------------------- 0F B6 = MOVZX r16, r/m8 ------------------------
; 2E0FB61EB302 = movzx bx,cs:[02B3] (hexxtrnr)
;
op_0f_b6
	modrm_jump_16_tbl op_0f_b6_jump
	modrm_tbl_3 movzx_r16_b

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	movzx_r16_b_r0 $reg
	GLOBAL	movzx_r16_b_r0_bp_$reg
movzx_r16_b_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movzx_r16_b_r0_$reg
movzx_r16_b_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrb	r0, [r2]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

	movzx_r16_b_r0 eax
	movzx_r16_b_r0 ecx
	movzx_r16_b_r0 edx
	movzx_r16_b_r0 ebx
	movzx_r16_b_r0 esp
	movzx_r16_b_r0 ebp
	movzx_r16_b_r0 esi
	movzx_r16_b_r0 edi

	MACRO
	movzx_r16_b_reg16_reg16 $rl, $rr
	IF "$rr" = "esp"
		ubfx	r0, eax, #8, #8
	ELIF "$rr" = "ebp"
		ubfx	r0, ecx, #8, #8
	ELIF "$rr" = "esi"
		ubfx	r0, edx, #8, #8
	ELIF "$rr" = "edi"
		ubfx	r0, ebx, #8, #8
	ELSE
		ubfx	r0, $rr, #0, #8
	ENDIF	
	bfi		$rl, r0, #0, #16
	b		loop
	MEND

	modrm_3_genall movzx_r16_b, movzx_r16_b_r0

; ------------------- 0F BA = BT/BTS/BTR/BTC r/m16, imm8 -------------
; 0FBAE800      bts  ax,00
;
op_0f_ba
	modrm_jump_16_tbl op_0f_ba_jump
	; 0x00
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_0 bt_16, imm8
	modrm_help_1_0 bts_16, imm8
	modrm_help_1_0 btr_16, imm8
	modrm_help_1_0 btc_16, imm8
	; 0x40
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_40 bt_16, imm8
	modrm_help_1_40 bts_16, imm8
	modrm_help_1_40 btr_16, imm8
	modrm_help_1_40 btc_16, imm8
	; 0x80
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_80 bt_16, imm8
	modrm_help_1_80 bts_16, imm8
	modrm_help_1_80 btr_16, imm8
	modrm_help_1_80 btc_16, imm8
	; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0_new bt_16, imm8
	modrm_help_1_C0_new bts_16, imm8
	modrm_help_1_C0_new btr_16, imm8
	modrm_help_1_C0_new btc_16, imm8

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	EXTERN	bt_16_EGA_r2
	EXTERN	bts_16_EGA_r2
	EXTERN	btr_16_EGA_r2
	EXTERN	btc_16_EGA_r2
	
	GLOBAL	bt_16_r0_bp_imm8
bt_16_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bt_16_r0_imm8
bt_16_r0_imm8
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bt_16_EGA_r2, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r0, [r2]
	ldrb	r2, [r2, #1]
	ldrb	r1, [r12], #1						; Get the imm8 value
	orr		r2, r0, r2, lsl #8					; r2 = halfword from RAM
	and		r1, #15
	mrs		r0, cpsr							; Save flags to r0
	mov		r1, r2, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop

	MACRO
	bt_reg16_imm8 $reg
	GLOBAL	bt_16_$reg_imm8
bt_16_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #15
	mov		r1, $reg, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	bt_reg16_imm8 eax
	bt_reg16_imm8 ecx
	bt_reg16_imm8 edx
	bt_reg16_imm8 ebx
	bt_reg16_imm8 esp
	bt_reg16_imm8 ebp
	bt_reg16_imm8 esi
	bt_reg16_imm8 edi

	GLOBAL	bts_16_r0_bp_imm8
bts_16_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bts_16_r0_imm8
bts_16_r0_imm8
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bts_16_EGA_r2, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	ldrb	r1, [r12], #1						; Get the imm8 value
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	and		r1, #15
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	orr		r3, r1								; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop

	MACRO
	bts_reg16_imm8 $reg
	GLOBAL	bts_16_$reg_imm8
bts_16_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #15
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg, r1							; Is the bit set?
	orr		$reg, r1							; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	bts_reg16_imm8 eax
	bts_reg16_imm8 ecx
	bts_reg16_imm8 edx
	bts_reg16_imm8 ebx
	bts_reg16_imm8 esp
	bts_reg16_imm8 ebp
	bts_reg16_imm8 esi
	bts_reg16_imm8 edi

	GLOBAL	btr_16_r0_bp_imm8
btr_16_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	btr_16_r0_imm8
btr_16_r0_imm8
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, btr_16_EGA_r2, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	ldrb	r1, [r12], #1						; Get the imm8 value
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	and		r1, #15
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	bic		r3, r1								; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop

	MACRO
	btr_reg16_imm8 $reg
	GLOBAL	btr_16_$reg_imm8
btr_16_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #15
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg, r1							; Is the bit set?
	bic		$reg, r1							; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	btr_reg16_imm8 eax
	btr_reg16_imm8 ecx
	btr_reg16_imm8 edx
	btr_reg16_imm8 ebx
	btr_reg16_imm8 esp
	btr_reg16_imm8 ebp
	btr_reg16_imm8 esi
	btr_reg16_imm8 edi

	GLOBAL	btc_16_r0_bp_imm8
btc_16_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	btc_16_r0_imm8
btc_16_r0_imm8
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, btc_16_EGA_r2, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	ldrb	r1, [r12], #1						; Get the imm8 value
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	and		r1, #15
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	eor		r3, r1								; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop

	MACRO
	btc_reg16_imm8 $reg
	GLOBAL	btc_16_$reg_imm8
btc_16_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #15
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg, r1							; Is the bit set?
	eor		$reg, r1							; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	btc_reg16_imm8 eax
	btc_reg16_imm8 ecx
	btc_reg16_imm8 edx
	btc_reg16_imm8 ebx
	btc_reg16_imm8 esp
	btc_reg16_imm8 ebp
	btc_reg16_imm8 esi
	btc_reg16_imm8 edi

	modrm_1_help bt_16, bt_16_r0, imm8
	modrm_1_help bts_16, bts_16_r0, imm8
	modrm_1_help btr_16, btr_16_r0, imm8
	modrm_1_help btc_16, btc_16_r0, imm8

; ------------------- 0F BB = BTC r/m16, r16 --------------------------
;
op_0f_bb
	modrm_jump_16_tbl op_0f_bb_jump
	modrm_tbl_1 btc_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	btc_r0_reg $reg
	GLOBAL	btc_r0_r16_bp_$reg
btc_r0_r16_bp_$reg
	mem_handler_bp
	GLOBAL	btc_r0_r16_$reg
btc_r0_r16_$reg
	;------
	;	CASE_0F_W(0xa3)												/* BT Ew,Gw */
	;		{
	;			FillFlags();GetRMrw;
	;			Bit16u mask=1 << (*rmrw & 15);
	;			if (rm >= 0xc0 ) {
	;				GetEArw;
	;				SETFLAGBIT(CF,(*earw & mask));
	;				*earw&=~mask;
	;			} else {
	;				GetEAa;eaa+=(((Bit16s)*rmrw)>>4)*2;
	;				Bit16u old=LoadMw(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;				SaveMw(eaa,old&~mask);
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, lsl #16
	lsr		r1, #16+4
	add		r0, r1, lsl #1						;	eaa+=(((Bit16s)*rmrw)>>4)*2;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r0, [r2]
	ldrb	r3, [r2, #1]
	and		r1, $reg, #0x0F
	orr		r3, r0, r3, lsl #8					; r3 = halfword from RAM
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	eor		r3, r1								; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	b		restore_flags_from_r0				; Back to loop
	MEND

	btc_r0_reg eax
	btc_r0_reg ecx
	btc_r0_reg edx
	btc_r0_reg ebx
	btc_r0_reg esp
	btc_r0_reg ebp
	btc_r0_reg esi
	btc_r0_reg edi

	MACRO
	btc_r16_reg16_reg16 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x0F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	tst		$reg1, r1							; Is the bit set?
	eor		$reg1, r1							; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper btc_r16, eax
	modrm_gen_reg_oper btc_r16, ecx
	modrm_gen_reg_oper btc_r16, edx
	modrm_gen_reg_oper btc_r16, ebx
	modrm_gen_reg_oper btc_r16, esp
	modrm_gen_reg_oper btc_r16, ebp
	modrm_gen_reg_oper btc_r16, esi
	modrm_gen_reg_oper btc_r16, edi

	modrm_1_genall btc_r16, btc_r0_r16

; ------------------- 0F BC = BSF r16, r/m16 -------------------------
;
op_0f_bc
	modrm_jump_16_tbl op_0f_bc_jump
	modrm_tbl_3 bsf_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	bsf_r16_r0 $reg
	GLOBAL	bsf_r16_r0_bp_$reg
bsf_r16_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bsf_r16_r0_$reg
bsf_r16_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;			if (value==0) {
	;				SETFLAGBIT(ZF,true);
	;			} else {
	;				result = 0;
	;				while ((value & 0x01)==0) { result++; value>>=1; }
	;				SETFLAGBIT(ZF,false);
	;				*rmrw = result;
	;			}
	;-------
1	ldrb	r1, [r2]
	ldrb	r2, [r2, #1]
	mrs		r0, cpsr							; Save flags to r0
	orrs	r2, r1, r2, lsl #8
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
bsf_r16_r2_$reg
	mov		r1, #0
1	movs	r2, r2, lsr #1						; Put lowest bit to Carry, shift the value right
	bcs		%f1									; Loop done if Carry set
	add		r1, #1								; result++
	b		%b1									; Back to loop
1	lsr		$reg, #16
	orr		$reg, r1, $reg, lsl #16
	bic		r0, #ARM_ZERO						; Clear the FLAG_ZF
	b		restore_flags_from_r0
	MEND

	bsf_r16_r0 eax
	bsf_r16_r0 ecx
	bsf_r16_r0 edx
	bsf_r16_r0 ebx
	bsf_r16_r0 esp
	bsf_r16_r0 ebp
	bsf_r16_r0 esi
	bsf_r16_r0 edi

	MACRO
	bsf_r16_reg16_reg16 $rl, $rr
	mrs		r0, cpsr							; Save flags to r0
	movs	r2, $rr, lsl #16
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
	lsr		r2, #16
	b		bsf_r16_r2_$rl
	MEND

	modrm_3_genall bsf_r16, bsf_r16_r0

bsf_ZF_true
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0

; ------------------- 0F BD = BSR r16, r/m16 -------------------------
;
op_0f_bd
	modrm_jump_16_tbl op_0f_bd_jump
	modrm_tbl_3 bsr_r16

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	bsr_r16_r0 $reg
	GLOBAL	bsr_r16_r0_bp_$reg
bsr_r16_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bsr_r16_r0_$reg
bsr_r16_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;			if (value==0) {
	;				SETFLAGBIT(ZF,true);
	;			} else {
	;				result = 0;
	;				while ((value & 0x01)==0) { result++; value>>=1; }
	;				SETFLAGBIT(ZF,false);
	;				*rmrw = result;
	;			}
	;-------
1	ldrb	r1, [r2]
	ldrb	r2, [r2, #1]
	mrs		r0, cpsr							; Save flags to r0
	lsl		r1, #16
	orrs	r2, r1, r2, lsl #24
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
bsr_r16_r2_$reg
	mov		r1, #15
1	movs	r2, r2, lsl #1						; Put highest bit to Carry, shift the value left
	bcs		%f1									; Loop done if Carry set
	sub		r1, #1								; result--
	b		%b1									; Back to loop
1	lsr		$reg, #16
	orr		$reg, r1, $reg, lsl #16
	bic		r0, #ARM_ZERO						; Clear the FLAG_ZF
	b		restore_flags_from_r0
	MEND

	bsr_r16_r0 eax
	bsr_r16_r0 ecx
	bsr_r16_r0 edx
	bsr_r16_r0 ebx
	bsr_r16_r0 esp
	bsr_r16_r0 ebp
	bsr_r16_r0 esi
	bsr_r16_r0 edi

	MACRO
	bsr_r16_reg16_reg16 $rl, $rr
	mrs		r0, cpsr							; Save flags to r0
	movs	r2, $rr, lsl #16
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
	b		bsr_r16_r2_$rl
	MEND

	modrm_3_genall bsr_r16, bsr_r16_r0

; ------------------- 0F BE = MOVSX r16, r/m8 ------------------------
;
op_0f_be
	modrm_jump_16_tbl op_0f_be_jump
	modrm_tbl_3 movsx_r16_b

	AREA cpu_0f, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO
	movsx_r16_b_r0 $reg
	GLOBAL	movsx_r16_b_r0_bp_$reg
movsx_r16_b_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movsx_r16_b_r0_$reg
movsx_r16_b_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrsb	r0, [r2]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

	movsx_r16_b_r0 eax
	movsx_r16_b_r0 ecx
	movsx_r16_b_r0 edx
	movsx_r16_b_r0 ebx
	movsx_r16_b_r0 esp
	movsx_r16_b_r0 ebp
	movsx_r16_b_r0 esi
	movsx_r16_b_r0 edi

	MACRO
	movsx_r16_b_reg16_reg16 $rl, $rr
	IF "$rr" = "esp"
		sbfx	r0, eax, #8, #8
	ELIF "$rr" = "ebp"
		sbfx	r0, ecx, #8, #8
	ELIF "$rr" = "esi"
		sbfx	r0, edx, #8, #8
	ELIF "$rr" = "edi"
		sbfx	r0, ebx, #8, #8
	ELSE
		sbfx	r0, $rr, #0, #8
	ENDIF
	bfi		$rl, r0, #0, #16
	b		loop
	MEND

	modrm_3_genall movsx_r16_b, movsx_r16_b_r0

; ------------------- 0F = protected mode opcodes ---------------------
; 
	GLOBAL	op_0f
op_0f
	modrm_jump_16_tbl op_0f_jump
; 0
	DCD op_0f_00, op_0f_01, op_0f_02, op_0f_03, unknown, unknown, op_0f_06, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_0f_20, op_0f_21, op_0f_22, op_0f_23, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x40
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x80
	DCD jo_imm16, jno_imm16, jc_imm16, jnc_imm16, jz_imm16, jnz_imm16, jbe_imm16, ja_imm16
	DCD js_imm16, jns_imm16, jpe_imm16, jpo_imm16, jl_imm16, jge_imm16, jle_imm16, jg_imm16
	DCD op_0f_90, op_0f_91, op_0f_92, op_0f_93, op_0f_94, op_0f_95, op_0f_96, op_0f_97
	DCD op_0f_98, op_0f_99, op_0f_9a, op_0f_9b, op_0f_9C, op_0f_9D, op_0f_9E, op_0f_9F
	DCD push_fs, pop_fs, unknown, op_0f_a3, op_0f_a4, op_0f_a5, unknown, unknown
	DCD push_gs, pop_gs, unknown, op_0f_ab, op_0f_ac, op_0f_ad, unknown, op_0f_af
	DCD unknown, unknown, op_0f_b2, op_0f_b3, op_0f_b4, op_0f_b5, op_0f_b6, op_8b
	DCD unknown, unknown, op_0f_ba, op_0f_bb, op_0f_bc, op_0f_bd, op_0f_be, op_8b
;0xc0 = mod = 11b => register operand
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	EXTERN	op_8b

	AREA cpu_0F_data, DATA, READWRITE
	ALIGN	4
	
	GLOBAL	BRUnsVM
BRUnsVM
	DCB	"Virtual memory not supported!"
	DCB	0x0a, 0

	END
