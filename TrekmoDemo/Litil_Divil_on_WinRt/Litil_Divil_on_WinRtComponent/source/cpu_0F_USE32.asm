;=============================================================================
; cpu_0F_USE32.s
;
; This file contains opcode handlers for the 0x0F-prefixed opcodes, for 32-bit
; address size and 32-bit operand size (USE32 code segment).
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

	AREA cpu_0F_USE32, CODE, READONLY

	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_32.inc

	EXTERN	loop
	EXTERN	restore_flags_from_r0
	EXTERN	complement_carry
	EXTERN	unknown
	EXTERN	bad_386_opcode_back1
	EXTERN	bad_EGA_opcode
	EXTERN	bad_MODEX_opcode

	EXTERN	registers
	EXTERN	cpu_cpl
	EXTERN	cpu_gdt_base
	EXTERN	cpu_idt_base
	EXTERN	cpu_ldt_value
	EXTERN	cpu_tss_selector
	
; ------------------- 0F 00 = SLDT/STR/LLDT/LTR/VERR/VERW r/m16 ------
; These are only available while in protected mode!
; See the actual handlers in "cpu_0F.S"!
;
	GLOBAL	op_0f_00_USE32
op_0f_00_USE32
	modrm_jump_32_tbl op_0f_00_32_jump
	modrm_tbl_1_oper sldt, str, lldt, ltr, verr, verw, back1, back1
	modrm_help_1_C0 sldt
	modrm_help_1_C0 str
	modrm_help_1_C0 lldt
	modrm_help_1_C0 ltr
	modrm_help_1_C0 verr
	modrm_help_1_C0 verw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	
	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	modrm_genall_1_oper sldt, str, lldt, ltr, verr, verw, skip, skip, "_r0"

	EXTERN	sldt_r0
	EXTERN	str_r0
	EXTERN	lldt_r0
	EXTERN	ltr_r0
	EXTERN	verr_r0
	EXTERN	verw_r0

	EXTERN	sldt_r0_bp
	EXTERN	str_r0_bp
	EXTERN	lldt_r0_bp
	EXTERN	ltr_r0_bp
	EXTERN	verr_r0_bp
	EXTERN	verw_r0_bp
	
	MACRO
	sldt $reg
	GLOBAL	sldt_$reg						; Called from "cpu_0F_66.s"
sldt_$reg
	ldr		r0, =cpu_ldt_value
	ldrh	$reg, [r0]
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

	MACRO
	str_r32 $reg
	GLOBAL	str_$reg						; Called from "cpu_0F_66.s"
str_$reg
	ldr		r0, =cpu_tss_selector
	ldrh	$reg, [r0]
	b		loop
	MEND

	str_r32 eax
	str_r32 ecx
	str_r32 edx
	str_r32 ebx
	str_r32 esp
	str_r32 ebp
	str_r32 esi
	str_r32 edi

	EXTERN	lldt_eax
	EXTERN	lldt_ecx
	EXTERN	lldt_edx
	EXTERN	lldt_ebx
	EXTERN	lldt_esp
	EXTERN	lldt_ebp
	EXTERN	lldt_esi
	EXTERN	lldt_edi

	EXTERN	ltr_eax
	EXTERN	ltr_ecx
	EXTERN	ltr_edx
	EXTERN	ltr_ebx
	EXTERN	ltr_esp
	EXTERN	ltr_ebp
	EXTERN	ltr_esi
	EXTERN	ltr_edi

	EXTERN	verr_eax
	EXTERN	verr_ecx
	EXTERN	verr_edx
	EXTERN	verr_ebx
	EXTERN	verr_esp
	EXTERN	verr_ebp
	EXTERN	verr_esi
	EXTERN	verr_edi

	EXTERN	verw_eax
	EXTERN	verw_ecx
	EXTERN	verw_edx
	EXTERN	verw_ebx
	EXTERN	verw_esp
	EXTERN	verw_ebp
	EXTERN	verw_esi
	EXTERN	verw_edi

	LTORG

; ------------------- 0F 01 = SGDT/SIDT/LGDT/LIDT/SMSW r/m16 ---------
; 660F01167409 = lgdt dword [0974] (DOS4GW)			(Prot mode entry at 0511:031F)
; See the actual handlers in "cpu_0F.S"!
;
op_0f_01_USE32
	modrm_jump_32_tbl op_0f_01_32_jump
	modrm_tbl_1_oper sgdt, sidt, lgdt_dword, lidt_dword, smsw, back1, lmsw, back1
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 smsw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 lmsw
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	
	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	modrm_genall_1_oper sgdt, sidt, lgdt_dword, lidt_dword, smsw, skip, lmsw, skip, "_r0"

	EXTERN	sgdt_r0
	EXTERN	sidt_r0
	EXTERN	smsw_r0
	EXTERN	lmsw_r0

	EXTERN	sgdt_r0_bp
	EXTERN	sidt_r0_bp
	EXTERN	smsw_r0_bp
	EXTERN	lmsw_r0_bp

	GLOBAL	lgdt_dword_r0_bp
lgdt_dword_r0_bp
	mem_handler_bp
	GLOBAL	lgdt_dword_r0
lgdt_dword_r0
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
	ldrb	r0, [r2, #5]
	strb	r0, [r1, #3]						; cpu_gdt_base<<24
	;-------
	; Calculate new physical base
	;-------
	ldr		r2, [r1]							; r2 = cpu_gdt_base
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #16]						; Save the physical address to cpu_gdt_phys
	b		loop


	GLOBAL	lidt_dword_r0_bp
lidt_dword_r0_bp
	mem_handler_bp
	GLOBAL	lidt_dword_r0
lidt_dword_r0
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
	ldrb	r0, [r2, #5]
	strb	r0, [r1, #3]						; cpu_idt_base<<24
	;-------
	; Calculate new physical base
	;-------
	ldr		r2, [r1]							; r2 = cpu_idt_base
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [r1, #8]						; Save the physical address to cpu_idt_phys
	b		loop

	EXTERN	smsw_eax
	EXTERN	smsw_ecx
	EXTERN	smsw_edx
	EXTERN	smsw_ebx
	EXTERN	smsw_esp
	EXTERN	smsw_ebp
	EXTERN	smsw_esi
	EXTERN	smsw_edi

	EXTERN	lmsw_eax
	EXTERN	lmsw_ecx
	EXTERN	lmsw_edx
	EXTERN	lmsw_ebx
	EXTERN	lmsw_esp
	EXTERN	lmsw_ebp
	EXTERN	lmsw_esi
	EXTERN	lmsw_edi

	LTORG

; ------------------- 0F 02 = LAR r32, r/m32 -------------------------
; These are only available while in protected mode!
;
op_0f_02_USE32
	modrm_jump_32_tbl op_0f_02_32_jump
	modrm_tbl_3 lar_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	lar_r32_r0 $reg
	GLOBAL	lar_r32_r0_bp_$reg
lar_r32_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lar_r32_r0_$reg
lar_r32_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
lar_r32_r1_$reg
	;------
	;void CPU_LAR(Bitu selector,Bitu & ar) {
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
	ldrb	r0, [r2, #6]
	ldrb	r2, [r2, #5]
	orr		r2, r0, lsl #8			; r2 = 2 bytes,  from offset 5 and 6
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
	mov		$reg, r2, lsl #8		; Same as (desc.saved.fill[1] & 0x00FFFF00), since r2 contains bytes 5 and 6.
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0
	MEND

	lar_r32_r0 eax
	lar_r32_r0 ecx
	lar_r32_r0 edx
	lar_r32_r0 ebx
	lar_r32_r0 esp
	lar_r32_r0 ebp
	lar_r32_r0 esi
	lar_r32_r0 edi

	MACRO
	lar_r32_reg32_reg32 $rl, $rr
	ubfx	r1, $rr, #0, #16
	b		lar_r32_r1_$rl
	MEND

	modrm_3_genall lar_r32, lar_r32_r0

	EXTERN	lar_ZF_false

; ------------------- 0F 03 = LSL r16, r/m16 -------------------------
; These are only available while in protected mode!
;
	GLOBAL	op_0f_03_USE32
op_0f_03_USE32
	modrm_jump_32_tbl op_0f_03_32_jump
	modrm_tbl_3 lsl_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	lsl_r32_r0 $reg
	GLOBAL	lsl_r32_r0_bp_$reg
lsl_r32_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lsl_r32_r0_$reg
lsl_r32_r0_$reg
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
1	ldrb	r1, [r2]
	ldrb	r0, [r2, #1]
	orr		r1, r0, lsl #8
lsl_r32_r1_$reg
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
	ldrb	$reg, [r2, #0]				; Low byte of limit
	ldrb	r2, [r2, #1]				; High byte of limit
	tst		r1, #0x80					; saved.seg.g ?
	orr		$reg, r2, lsl #8			; reg = limit_0_15
	and		r1, #0x0F
	orr		$reg, r1, lsl #16			; reg = (saved.seg.limit_16_19<<16) | saved.seg.limit_0_15;
	lslne	$reg, #12
	orrne	$reg, #0xFF
	orrne	$reg, #0x0F00				; if (saved.seg.g) return (limit<<12) | 0xFFF;
	ldr		r0, [sp, #SP_STR_SEG]	; Get the flags from stack
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0
	MEND

	lsl_r32_r0 eax
	lsl_r32_r0 ecx
	lsl_r32_r0 edx
	lsl_r32_r0 ebx
	lsl_r32_r0 esp
	lsl_r32_r0 ebp
	lsl_r32_r0 esi
	lsl_r32_r0 edi

	LTORG
	
	MACRO
	lsl_r32_reg32_reg32 $rl, $rr
	ubfx	r1, $rr, #0, #16
	b		lsl_r32_r1_$rl
	MEND

	modrm_3_genall lsl_r32, lsl_r32_r0

	EXTERN	lsl_ZF_false
	
; ------------------- 0F 80 .. 0F 8F = conditional jumps -------------

	MACRO
	cond_jump $cnd
	IF USE_UNALIGNED = 1
	ldr		r0, [r12], #4
	ELSE
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	ldrb	r2,[r12],#1
	ldrb	r3,[r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	ENDIF
	add$cnd	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	b		loop
	MEND

jo_imm32								; JO cw
	cond_jump vs
jno_imm32								; JNO cw
	cond_jump vc
jc_imm32								; JC/JB/JNAE cw
	cond_jump cs
jnc_imm32								; JNC/JNB/JAE cw
	cond_jump cc
jz_imm32								; JZ/JE cw
	cond_jump eq
jnz_imm32								; JNZ/JNE cw
	cond_jump ne
jbe_imm32								; JBE/JNA cw (CF=1 or ZF=1)
	IF USE_UNALIGNED = 1
	ldr		r0, [r12], #4
	ELSE
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	ldrb	r2,[r12],#1
	ldrb	r3,[r12],#1
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	ENDIF
	addcs	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	bcs		loop
	addeq	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	b		loop
ja_imm32								; JA/JNBE cw (CF=0 and ZF=0)
	IF USE_UNALIGNED = 1
	ldr		r0, [r12], #4
	bcs		loop
	ELSE
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	ldrb	r2,[r12],#1
	ldrb	r3,[r12],#1
	bcs		loop
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	ENDIF
	addne	r12, r12, r0				; Adjust program counter by the jump amount, if the condition is true
	b		loop
js_imm32								; JS cw
	cond_jump mi
jns_imm32								; JNS cw
	cond_jump pl
jl_imm32								; JL/JNGE cw (SF != OF)
	cond_jump lt
jge_imm32								; JNL/JGE cw (SF == OF)
	cond_jump ge
jle_imm32								; JLE/JNG cw (ZF == 1 or SF != OF)
	cond_jump le
jg_imm32								; JG/JNLE cw (ZF == 0 and SF == OF)
	cond_jump gt

jp_imm32								; JP/JPE cb
	b		unsjpe
jnp_imm32								; JNP/JPO cb
	b		unsjpo

	GLOBAL jo_imm32 
	GLOBAL jno_imm32 
	GLOBAL jc_imm32 
	GLOBAL jnc_imm32 
	GLOBAL jz_imm32 
	GLOBAL jnz_imm32 
	GLOBAL jbe_imm32 
	GLOBAL ja_imm32
	GLOBAL js_imm32 
	GLOBAL jns_imm32 
	GLOBAL jp_imm32 
	GLOBAL jnp_imm32 
	GLOBAL jl_imm32 
	GLOBAL jge_imm32 
	GLOBAL jle_imm32 
	GLOBAL jg_imm32

	EXTERN	unsjpe
	EXTERN	unsjpo
	
; ------------------- 0F 90 .. 0F 9F = SETcc r/m8 --------------------
;
	MACRO
	setccm $cond
	mov		r1, #0
	mov$cond r1, #1								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mvn		r3, #0								; Use 32-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	MEND

op_0f_90_USE32									; SETO
	setccm	vs
	
op_0f_91_USE32									; SETNO
	setccm	vc

op_0f_92_USE32									; SETC
	setccm	cs

op_0f_93_USE32									; SETNC
	setccm	cc

op_0f_94_USE32									; SETZ
	setccm	eq
	
op_0f_95_USE32									; SETNZ
	setccm	ne

op_0f_96_USE32									; SETBE (CF=1 or ZF=1)
	mov		r1, #0
	movcs 	r1, #1								
	moveq	r1, #1								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mvn		r3, #0								; Use 32-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_0f_97_USE32										; SETA (CF=0 and ZF=0)
	mov		r1, #1
	movcs 	r1, #0								
	moveq	r1, #0								; r1 = 0 / 1 depending on the condition
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	strb	r1, [sp, #SP_STR_SEG]				; Save the result byte into stack
	ldr		r1, =setcc_table
	mvn		r3, #0								; Use 32-bit memory address masking
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_0f_98_USE32									; SETS
	setccm	mi

op_0f_99_USE32									; SETNS
	setccm	pl

op_0f_9a_USE32									; SETP
	b		unknown

op_0f_9b_USE32									; SETNP
	b		unknown

op_0f_9C_USE32									; SETL
	setccm	lt

op_0f_9D_USE32									; SETGE
	setccm	ge

op_0f_9E_USE32									; SETLE
	setccm	le

op_0f_9F_USE32									; SETG
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

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	modrm_1_help setcc, setcc_r0

	GLOBAL op_0f_90_USE32 
	GLOBAL op_0f_91_USE32 
	GLOBAL op_0f_92_USE32 
	GLOBAL op_0f_93_USE32 
	GLOBAL op_0f_94_USE32 
	GLOBAL op_0f_95_USE32 
	GLOBAL op_0f_96_USE32 
	GLOBAL op_0f_97_USE32
	GLOBAL op_0f_98_USE32 
	GLOBAL op_0f_99_USE32 
	GLOBAL op_0f_9a_USE32 
	GLOBAL op_0f_9b_USE32 
	GLOBAL op_0f_9C_USE32 
	GLOBAL op_0f_9D_USE32 
	GLOBAL op_0f_9E_USE32 
	GLOBAL op_0f_9F_USE32

	EXTERN	setcc_r0
	EXTERN	setcc_r0_bp

	EXTERN	setcc_al
	EXTERN	setcc_cl
	EXTERN	setcc_dl
	EXTERN	setcc_bl
	EXTERN	setcc_ah
	EXTERN	setcc_ch
	EXTERN	setcc_dh
	EXTERN	setcc_bh

; ------------------- 0F A0 / 0F A1 = PUSH/POP FS --------------------

	GLOBAL	push_fs_USE32
push_fs_USE32
	ldr		r1, [sp, #SP_FS_VALUE]	
	push_dword r1, r0, r2
	b		loop

	GLOBAL	pop_fs_USE32
pop_fs_USE32
	pop_dword r2, r0, r1
	ubfx	r2, r2, #0, #16					; Only use the low 16 bits
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

	EXTERN	mov_fs_r0r2_prot
	
; ------------------- 0F A3 = BT r/m32, r32 --------------------------
;
op_0f_a3_USE32
	modrm_jump_32_tbl op_0f_a3_32_jump
	modrm_tbl_1 bt_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	bt_r0_reg $reg
	GLOBAL	bt_r0_r32_bp_$reg
bt_r0_r32_bp_$reg
	mem_handler_bp
	GLOBAL	bt_r0_r32_$reg
bt_r0_r32_$reg
	;------
	;	CASE_0F_D(0xa3)												/* BT Ed,Gd */
	;		{
	;			FillFlags();GetRMrd;
	;			Bit32u mask=1 << (*rmrd & 31);
	;			if (rm >= 0xc0 ) {
	;				GetEArd;
	;				SETFLAGBIT(CF,(*eard & mask));
	;			} else {
	;				GetEAa;eaa+=(((Bit32s)*rmrd)>>5)*4;
	;				Bit32u old=LoadMd(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, asr #5
	add		r0, r1, lsl #2						;	eaa+=(((Bit32s)*rmrd)>>5)*4;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldr		r2, [r2]
	and		r1, $reg, #0x1F
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	and		r1, $reg, #0x1F
	orr		r0, r3, lsl #16
	orr		r2, r0, r2, lsl #24					; r2 = dword from RAM
	ENDIF
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
	bt_r32_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x1F
	mov		r1, $reg1, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper bt_r32, eax
	modrm_gen_reg_oper bt_r32, ecx
	modrm_gen_reg_oper bt_r32, edx
	modrm_gen_reg_oper bt_r32, ebx
	modrm_gen_reg_oper bt_r32, esp
	modrm_gen_reg_oper bt_r32, ebp
	modrm_gen_reg_oper bt_r32, esi
	modrm_gen_reg_oper bt_r32, edi

	modrm_1_genall bt_r32, bt_r0_r32

; ------------------- 0F A4 = SHLD r/m32, r32, imm8 ------------------
; Overflow flag is undefined
op_0f_a4_USE32
	modrm_jump_32_tbl op_0f_a4_32_jump
	modrm_tbl_1 shld_r32_imm8

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	shld_r0_r32_imm8 $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shld_r0_r32_imm8_bp_$reg
shld_r0_r32_imm8_bp_$reg
	mem_handler_bp
	GLOBAL	shld_r0_r32_imm8_$reg
shld_r0_r32_imm8_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	IF USE_UNALIGNED = 1
	ldr		r3, [r2]
	ELSE
	ldrb	r3, [r2]
	ENDIF
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	IF USE_UNALIGNED = 0
	ldrb	r0, [r2, #1]
	orr		r3, r0, lsl #8
	ldrb	r0, [r2, #2]
	orr		r3, r0, lsl #16
	ldrb	r0, [r2, #3]
	orr		r3, r0, lsl #24
	ENDIF
	;------
	; lf_resd=(lf_var1d << lf_var2b) | (op2 >> (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using adc
	rsb		r0, r1, #32
	mov		r3, r3, lsl r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg, lsr r0		; Setup the Carry flag (last bit shifted right out of the register)
	orr		r3, r1		
	adcs	r3, r3, r3				; Shift the final bit position, set flags, get lowest bit from Carry
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	b		loop
	MEND

	shld_r0_r32_imm8 eax
	shld_r0_r32_imm8 ecx
	shld_r0_r32_imm8 edx
	shld_r0_r32_imm8 ebx
	shld_r0_r32_imm8 esp
	shld_r0_r32_imm8 ebp
	shld_r0_r32_imm8 esi
	shld_r0_r32_imm8 edi
	
	MACRO
	shld_r32_imm8_reg32_reg32 $reg1, $reg2
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	;------
	; lf_resd=(lf_var1d << lf_var2b) | (op2 >> (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using adc
	rsb		r0, r1, #32
	mov		r2, $reg1, lsl r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg2, lsr r0		; Setup the Carry flag
	orr		r2, r1
	adcs	$reg1, r2, r2			; Shift the final bit position, set flags, get lowest bit from Carry
	b		loop
	MEND

	modrm_gen_reg_oper shld_r32_imm8, eax
	modrm_gen_reg_oper shld_r32_imm8, ecx
	modrm_gen_reg_oper shld_r32_imm8, edx
	modrm_gen_reg_oper shld_r32_imm8, ebx
	modrm_gen_reg_oper shld_r32_imm8, esp
	modrm_gen_reg_oper shld_r32_imm8, ebp
	modrm_gen_reg_oper shld_r32_imm8, esi
	modrm_gen_reg_oper shld_r32_imm8, edi

	modrm_1_genall shld_r32_imm8, shld_r0_r32_imm8

; ------------------- 0F A5 = SHLD r/m32, r32, CL ------------------
; Overflow flag is undefined
op_0f_a5_USE32
	modrm_jump_32_tbl op_0f_a5_32_jump
	modrm_tbl_1 shld_r32_CL

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	shld_r0_r32_CL $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shld_r0_r32_CL_bp_$reg
shld_r0_r32_CL_bp_$reg
	mem_handler_bp
	GLOBAL	shld_r0_r32_CL_$reg
shld_r0_r32_CL_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
	IF USE_UNALIGNED = 1
1	ldr		r3, [r2]
	ELSE
1	ldrb	r3, [r2]
	ENDIF
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	IF USE_UNALIGNED = 0
	ldrb	r0, [r2, #1]
	orr		r3, r0, lsl #8
	ldrb	r0, [r2, #2]
	orr		r3, r0, lsl #16
	ldrb	r0, [r2, #3]
	orr		r3, r0, lsl #24
	ENDIF
	;------
	; lf_resd=(lf_var1d << lf_var2b) | (op2 >> (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using adc
	rsb		r0, r1, #32
	mov		r3, r3, lsl r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg, lsr r0		; Setup the Carry flag
	orr		r3, r1
	adcs	r3, r3, r3				; Shift the final bit position, set flags, get lowest bit from Carry
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	b		loop
	MEND

	shld_r0_r32_CL eax
	shld_r0_r32_CL ecx
	shld_r0_r32_CL edx
	shld_r0_r32_CL ebx
	shld_r0_r32_CL esp
	shld_r0_r32_CL ebp
	shld_r0_r32_CL esi
	shld_r0_r32_CL edi
	
	MACRO
	shld_r32_CL_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	;------
	; lf_resd=(lf_var1d << lf_var2b) | (op2 >> (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using adc
	rsb		r0, r1, #32
	mov		r2, $reg1, lsl r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg2, lsr r0		; Setup the Carry flag
	orr		r2, r1
	adcs	$reg1, r2, r2			; Shift the final bit position, set flags, get lowest bit from Carry
	b		loop
	MEND

	modrm_gen_reg_oper shld_r32_CL, eax
	modrm_gen_reg_oper shld_r32_CL, ecx
	modrm_gen_reg_oper shld_r32_CL, edx
	modrm_gen_reg_oper shld_r32_CL, ebx
	modrm_gen_reg_oper shld_r32_CL, esp
	modrm_gen_reg_oper shld_r32_CL, ebp
	modrm_gen_reg_oper shld_r32_CL, esi
	modrm_gen_reg_oper shld_r32_CL, edi

	modrm_1_genall shld_r32_CL, shld_r0_r32_CL

; ------------------- 0F A8 / 0F A9 = PUSH/POP GS --------------------

	GLOBAL	push_gs_USE32
push_gs_USE32
	ldr		r1, [sp, #SP_GS_VALUE]	
	push_dword r1, r0, r2
	b		loop

	GLOBAL	pop_gs_USE32
pop_gs_USE32
	pop_dword r2, r0, r1
	ubfx	r2, r2, #0, #16						; Only use the low 16 bits
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

	EXTERN	mov_gs_r0r2_prot
	
; ------------------- 0F AB = BTS r/m32, r32 --------------------------
;
op_0f_ab_USE32
	modrm_jump_32_tbl op_0f_ab_32_jump
	modrm_tbl_1 bts_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	bts_r0_reg $reg
	GLOBAL	bts_r0_r32_bp_$reg
bts_r0_r32_bp_$reg
	mem_handler_bp
	GLOBAL	bts_r0_r32_$reg
bts_r0_r32_$reg
	;------
	;	CASE_0F_D(0xa3)												/* BT Ed,Gd */
	;		{
	;			FillFlags();GetRMrd;
	;			Bit32u mask=1 << (*rmrd & 31);
	;			if (rm >= 0xc0 ) {
	;				GetEArd;
	;				SETFLAGBIT(CF,(*eard & mask));
	;			} else {
	;				GetEAa;eaa+=(((Bit32s)*rmrd)>>5)*4;
	;				Bit32u old=LoadMd(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, asr #5
	add		r0, r1, lsl #2						;	eaa+=(((Bit32s)*rmrd)>>5)*4;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldr		r3, [r2]
	and		r1, $reg, #0x1F
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	and		r1, $reg, #0x1F
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	orr		r3, r1								; Set the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
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
	bts_r32_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x1F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg1, r1							; Is the bit set?
	orr		$reg1, r1							; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper bts_r32, eax
	modrm_gen_reg_oper bts_r32, ecx
	modrm_gen_reg_oper bts_r32, edx
	modrm_gen_reg_oper bts_r32, ebx
	modrm_gen_reg_oper bts_r32, esp
	modrm_gen_reg_oper bts_r32, ebp
	modrm_gen_reg_oper bts_r32, esi
	modrm_gen_reg_oper bts_r32, edi

	modrm_1_genall bts_r32, bts_r0_r32

; ------------------- 0F AC = SHRD r/m32, r32, imm8 ------------------
; Overflow flag is undefined
op_0f_ac_USE32
	modrm_jump_32_tbl op_0f_ac_32_jump
	modrm_tbl_1 shrd_r32_imm8

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	shrd_r0_r32_imm8 $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shrd_r0_r32_imm8_bp_$reg
shrd_r0_r32_imm8_bp_$reg
	mem_handler_bp
	GLOBAL	shrd_r0_r32_imm8_$reg
shrd_r0_r32_imm8_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
1	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	IF USE_UNALIGNED = 1
	ldr		r3, [r2]
	ELSE
	ldrb	r3, [r2]
	ENDIF
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	IF USE_UNALIGNED = 0
	ldrb	r0, [r2, #1]
	orr		r3, r0, lsl #8
	ldrb	r0, [r2, #2]
	orr		r3, r0, lsl #16
	ldrb	r0, [r2, #3]
	orr		r3, r0, lsl #24
	ENDIF
	;------
	; lf_resd=(lf_var1d >> lf_var2b) | (op2 << (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using rrx
	rsb		r0, r1, #32
	mov		r3, r3, lsr r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg, lsl r0		; Setup the Carry flag
	orr		r3, r1
	movs	r3, r3, rrx				; Shift the final bit position, set flags, get highest bit from Carry
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	b		loop
	MEND

	shrd_r0_r32_imm8 eax
	shrd_r0_r32_imm8 ecx
	shrd_r0_r32_imm8 edx
	shrd_r0_r32_imm8 ebx
	shrd_r0_r32_imm8 esp
	shrd_r0_r32_imm8 ebp
	shrd_r0_r32_imm8 esi
	shrd_r0_r32_imm8 edi
	
	MACRO
	shrd_r32_imm8_reg32_reg32 $reg1, $reg2
	ldrb	r1, [r12], #1
	mrs		r0, cpsr				; Save current flags
	ands	r1, #0x1F				; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	;------
	; lf_resd=(lf_var1d >> lf_var2b) | (op2 << (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using rrx
	rsb		r0, r1, #32
	mov		r2, $reg1, lsr r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg2, lsl r0		; Setup the Carry flag
	orr		r2, r1
	movs	$reg1, r2, rrx			; Shift the final bit position, set flags, get highest bit from Carry
	b		loop
	MEND

	modrm_gen_reg_oper shrd_r32_imm8, eax
	modrm_gen_reg_oper shrd_r32_imm8, ecx
	modrm_gen_reg_oper shrd_r32_imm8, edx
	modrm_gen_reg_oper shrd_r32_imm8, ebx
	modrm_gen_reg_oper shrd_r32_imm8, esp
	modrm_gen_reg_oper shrd_r32_imm8, ebp
	modrm_gen_reg_oper shrd_r32_imm8, esi
	modrm_gen_reg_oper shrd_r32_imm8, edi

	modrm_1_genall shrd_r32_imm8, shrd_r0_r32_imm8

; ------------------- 0F AD = SHRD r/m32, r32, CL ------------------
; Overflow flag is undefined
op_0f_ad_USE32
	modrm_jump_32_tbl op_0f_ad_32_jump
	modrm_tbl_1 shrd_r32_CL

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	shrd_r0_r32_CL $reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	GLOBAL	shrd_r0_r32_CL_bp_$reg
shrd_r0_r32_CL_bp_$reg
	mem_handler_bp
	GLOBAL	shrd_r0_r32_CL_$reg
shrd_r0_r32_CL_$reg
	;-------
	; Calculate the memory address.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
	IF USE_UNALIGNED = 1
1	ldr		r3, [r2]
	ELSE
1	ldrb	r3, [r2]
	ENDIF
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	IF USE_UNALIGNED = 0
	ldrb	r0, [r2, #1]
	orr		r3, r0, lsl #8
	ldrb	r0, [r2, #2]
	orr		r3, r0, lsl #16
	ldrb	r0, [r2, #3]
	orr		r3, r0, lsl #24
	ENDIF
	;------
	; lf_resd=(lf_var1d >> lf_var2b) | (op2 << (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using rrx
	rsb		r0, r1, #32
	mov		r3, r3, lsr r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg, lsl r0		; Setup the Carry flag
	orr		r3, r1
	movs	r3, r3, rrx				; Shift the final bit position, set flags, get highest bit from Carry
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	b		loop
	MEND

	shrd_r0_r32_CL eax
	shrd_r0_r32_CL ecx
	shrd_r0_r32_CL edx
	shrd_r0_r32_CL ebx
	shrd_r0_r32_CL esp
	shrd_r0_r32_CL ebp
	shrd_r0_r32_CL esi
	shrd_r0_r32_CL edi
	
	MACRO
	shrd_r32_CL_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr				; Save current flags
	ands	r1, ecx, #0x1F			; r1 = masked rotation count
	beq		restore_flags_from_r0	; Do nothing if rotation count = 0
	;------
	; lf_resd=(lf_var1d >> lf_var2b) | (op2 << (32-lf_var2b));
	;------
	sub		r1, #1					; Shift one less bits than we should, so that we can get the proper flags using rrx
	rsb		r0, r1, #32
	mov		r2, $reg1, lsr r1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	movs	r1, $reg2, lsl r0		; Setup the Carry flag
	orr		r2, r1
	movs	$reg1, r2, rrx			; Shift the final bit position, set flags, get highest bit from Carry
	b		loop
	MEND

	modrm_gen_reg_oper shrd_r32_CL, eax
	modrm_gen_reg_oper shrd_r32_CL, ecx
	modrm_gen_reg_oper shrd_r32_CL, edx
	modrm_gen_reg_oper shrd_r32_CL, ebx
	modrm_gen_reg_oper shrd_r32_CL, esp
	modrm_gen_reg_oper shrd_r32_CL, ebp
	modrm_gen_reg_oper shrd_r32_CL, esi
	modrm_gen_reg_oper shrd_r32_CL, edi

	modrm_1_genall shrd_r32_CL, shrd_r0_r32_CL

; ------------------- 0F AF = IMUL r32, r/m32 ------------------------
op_0f_af_USE32
	modrm_jump_32_tbl op_0f_af_32_jump
	modrm_tbl_3 imul_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	imul_reg32_r0 $reg
	GLOBAL	imul_r32_r0_bp_$reg
imul_r32_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	imul_r32_r0_$reg
imul_r32_r0_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	;-------
	; Memory location in normal RAM.
	;-------
	IF USE_UNALIGNED = 1
1	ldr		r0, [r2]
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	ENDIF
	mul		$reg, r0, $reg
	b		loop
	MEND

	imul_reg32_r0 eax
	imul_reg32_r0 ecx
	imul_reg32_r0 edx
	imul_reg32_r0 ebx
	imul_reg32_r0 esp
	imul_reg32_r0 ebp
	imul_reg32_r0 esi
	imul_reg32_r0 edi

	MACRO
	imul_r32_reg32_reg32 $rl, $rr
	IF "$rl" = "$rr"
	mov		r0, $rr
	mul		$rl, r0, $rl						; imul eax,eax and such, we can not use the same register three times in ARM.
	ELSE
	mul		$rl, $rr, $rl
	ENDIF	
	b		loop
	MEND

	modrm_3_genall imul_r32, imul_r32_r0

; ------------------- 0F B2 = LSS r32, r/m32 -------------------------
;
op_0f_b2_USE32
	modrm_jump_32_tbl op_0f_b2_32_jump
; 0
	modrm_help_3_0 lss_r32, eax
	modrm_help_3_0 lss_r32, ecx
	modrm_help_3_0 lss_r32, edx
	modrm_help_3_0 lss_r32, ebx
	modrm_help_3_0 lss_r32, esp
	modrm_help_3_0 lss_r32, ebp
	modrm_help_3_0 lss_r32, esi
	modrm_help_3_0 lss_r32, edi
; 0x40
	modrm_help_3_40 lss_r32, eax
	modrm_help_3_40 lss_r32, ecx
	modrm_help_3_40 lss_r32, edx
	modrm_help_3_40 lss_r32, ebx
	modrm_help_3_40 lss_r32, esp
	modrm_help_3_40 lss_r32, ebp
	modrm_help_3_40 lss_r32, esi
	modrm_help_3_40 lss_r32, edi
; 0x80
	modrm_help_3_80 lss_r32, eax
	modrm_help_3_80 lss_r32, ecx
	modrm_help_3_80 lss_r32, edx
	modrm_help_3_80 lss_r32, ebx
	modrm_help_3_80 lss_r32, esp
	modrm_help_3_80 lss_r32, ebp
	modrm_help_3_80 lss_r32, esi
	modrm_help_3_80 lss_r32, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	lss_r32_r0 $reg
	GLOBAL	lss_r32_r0_bp_$reg
lss_r32_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lss_r32_r0_$reg
lss_r32_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	IF USE_UNALIGNED = 1
1	ldr		$reg, [r2]
	ldrh	r2, [r2, #4]
	ELSE
1	ldrb	$reg, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		$reg, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		$reg, r3, lsl #16
	ldrb	r0,[r2, #4]							; Load low byte of SS from [r2+4]
	ldrb	r2,[r2, #5]							; Load high byte of SS from [r2+5]
	orr		$reg, r1, lsl #24
	orr		r2, r0, r2, lsl #8					; r2 = low byte | (high byte << 8) = new SS value
	ENDIF
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

	lss_r32_r0 eax
	lss_r32_r0 ecx
	lss_r32_r0 edx
	lss_r32_r0 ebx
	lss_r32_r0 esp
	lss_r32_r0 ebp
	lss_r32_r0 esi
	lss_r32_r0 edi

	MACRO
	lss_r32_reg32_reg32 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lss_r32, lss_r32_r0

	EXTERN	mov_ss_r0r2_prot
	
; ------------------- 0F B3 = BTR r/m32, r32 --------------------------
;
op_0f_b3_USE32
	modrm_jump_32_tbl op_0f_b3_32_jump
	modrm_tbl_1 btr_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	btr_r0_reg $reg
	GLOBAL	btr_r0_r32_bp_$reg
btr_r0_r32_bp_$reg
	mem_handler_bp
	GLOBAL	btr_r0_r32_$reg
btr_r0_r32_$reg
	;------
	;	CASE_0F_D(0xa3)												/* BT Ed,Gd */
	;		{
	;			FillFlags();GetRMrd;
	;			Bit32u mask=1 << (*rmrd & 31);
	;			if (rm >= 0xc0 ) {
	;				GetEArd;
	;				SETFLAGBIT(CF,(*eard & mask));
	;			} else {
	;				GetEAa;eaa+=(((Bit32s)*rmrd)>>5)*4;
	;				Bit32u old=LoadMd(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, asr #5
	add		r0, r1, lsl #2						;	eaa+=(((Bit32s)*rmrd)>>5)*4;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldr		r3, [r2]
	and		r1, $reg, #0x1F
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	and		r1, $reg, #0x1F
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	bic		r3, r1								; Clear the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
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
	btr_r32_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x1F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg1, r1							; Is the bit set?
	bic		$reg1, r1							; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper btr_r32, eax
	modrm_gen_reg_oper btr_r32, ecx
	modrm_gen_reg_oper btr_r32, edx
	modrm_gen_reg_oper btr_r32, ebx
	modrm_gen_reg_oper btr_r32, esp
	modrm_gen_reg_oper btr_r32, ebp
	modrm_gen_reg_oper btr_r32, esi
	modrm_gen_reg_oper btr_r32, edi

	modrm_1_genall btr_r32, btr_r0_r32

; ------------------- 0F B4 = LFS r32, r/m32 -------------------------
;
op_0f_b4_USE32
	modrm_jump_32_tbl op_0f_b4_32_jump
; 0
	modrm_help_3_0 lfs_r32, eax
	modrm_help_3_0 lfs_r32, ecx
	modrm_help_3_0 lfs_r32, edx
	modrm_help_3_0 lfs_r32, ebx
	modrm_help_3_0 lfs_r32, esp
	modrm_help_3_0 lfs_r32, ebp
	modrm_help_3_0 lfs_r32, esi
	modrm_help_3_0 lfs_r32, edi
; 0x40
	modrm_help_3_40 lfs_r32, eax
	modrm_help_3_40 lfs_r32, ecx
	modrm_help_3_40 lfs_r32, edx
	modrm_help_3_40 lfs_r32, ebx
	modrm_help_3_40 lfs_r32, esp
	modrm_help_3_40 lfs_r32, ebp
	modrm_help_3_40 lfs_r32, esi
	modrm_help_3_40 lfs_r32, edi
; 0x80
	modrm_help_3_80 lfs_r32, eax
	modrm_help_3_80 lfs_r32, ecx
	modrm_help_3_80 lfs_r32, edx
	modrm_help_3_80 lfs_r32, ebx
	modrm_help_3_80 lfs_r32, esp
	modrm_help_3_80 lfs_r32, ebp
	modrm_help_3_80 lfs_r32, esi
	modrm_help_3_80 lfs_r32, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	lfs_r32_r0 $reg
	GLOBAL	lfs_r32_r0_bp_$reg
lfs_r32_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lfs_r32_r0_$reg
lfs_r32_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	IF USE_UNALIGNED = 1
1	ldr		$reg, [r2]
	ldrh	r2, [r2, #4]
	ELSE
	; ----- Load offset
1	ldrb	$reg, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		$reg, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		$reg, r3, lsl #16
	; ----- Load segment
	ldrb	r0,[r2, #4]				; Load low byte of FS from [r2+4]
	ldrb	r2,[r2, #5]				; Load high byte of FS from [r2+5]
	orr		$reg, r1, lsl #24
	orr		r2, r0, r2, lsl #8		; r2 = low byte | (high byte << 8) = new FS value
	ENDIF
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

	lfs_r32_r0 eax
	lfs_r32_r0 ecx
	lfs_r32_r0 edx
	lfs_r32_r0 ebx
	lfs_r32_r0 esp
	lfs_r32_r0 ebp
	lfs_r32_r0 esi
	lfs_r32_r0 edi

	MACRO
	lfs_r32_reg32_reg32 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lfs_r32, lfs_r32_r0

	EXTERN	mov_fs_r0r2_prot
	
; ------------------- 0F B5 = LGS r32, r/m32 -------------------------
;
op_0f_b5_USE32
	modrm_jump_32_tbl op_0f_b5_32_jump
; 0
	modrm_help_3_0 lgs_r32, eax
	modrm_help_3_0 lgs_r32, ecx
	modrm_help_3_0 lgs_r32, edx
	modrm_help_3_0 lgs_r32, ebx
	modrm_help_3_0 lgs_r32, esp
	modrm_help_3_0 lgs_r32, ebp
	modrm_help_3_0 lgs_r32, esi
	modrm_help_3_0 lgs_r32, edi
; 0x40
	modrm_help_3_40 lgs_r32, eax
	modrm_help_3_40 lgs_r32, ecx
	modrm_help_3_40 lgs_r32, edx
	modrm_help_3_40 lgs_r32, ebx
	modrm_help_3_40 lgs_r32, esp
	modrm_help_3_40 lgs_r32, ebp
	modrm_help_3_40 lgs_r32, esi
	modrm_help_3_40 lgs_r32, edi
; 0x80
	modrm_help_3_80 lgs_r32, eax
	modrm_help_3_80 lgs_r32, ecx
	modrm_help_3_80 lgs_r32, edx
	modrm_help_3_80 lgs_r32, ebx
	modrm_help_3_80 lgs_r32, esp
	modrm_help_3_80 lgs_r32, ebp
	modrm_help_3_80 lgs_r32, esi
	modrm_help_3_80 lgs_r32, edi
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	lgs_r32_r0 $reg
	GLOBAL	lgs_r32_r0_bp_$reg
lgs_r32_r0_bp_$reg
	mem_handler_bp
	GLOBAL	lgs_r32_r0_$reg
lgs_r32_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	IF USE_UNALIGNED = 1
1	ldr		$reg, [r2]
	ldrh	r2, [r2, #4]
	ELSE
	; ----- Load offset
1	ldrb	$reg, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		$reg, r1, lsl #8
	ldrb	r1, [r2, #3]
	orr		$reg, r3, lsl #16
	; ----- Load segment
	ldrb	r0,[r2, #4]				; Load low byte of GS from [r2+4]
	ldrb	r2,[r2, #5]				; Load high byte of GS from [r2+5]
	orr		$reg, r1, lsl #24
	orr		r2, r0, r2, lsl #8		; r2 = low byte | (high byte << 8) = new GS value
	ENDIF
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

	lgs_r32_r0 eax
	lgs_r32_r0 ecx
	lgs_r32_r0 edx
	lgs_r32_r0 ebx
	lgs_r32_r0 esp
	lgs_r32_r0 ebp
	lgs_r32_r0 esi
	lgs_r32_r0 edi

	MACRO
	lgs_r32_reg32_reg32 $rl, $rr
	; Empty macro
	MEND

	modrm_3_genall lgs_r32, lgs_r32_r0

	EXTERN	mov_gs_r0r2_prot
	
; ------------------- 0F B6 = MOVZX r32, r/m8 ------------------------
; 66670FB6AE90150000 = movzx ebp,[esi+00001590]
;
op_0f_b6_USE32
	modrm_jump_32_tbl op_0f_b6_32_jump
	modrm_tbl_3 movzx_r32_b

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	movzx_r32_b_r0 $reg
	GLOBAL	movzx_r32_b_r0_bp_$reg
movzx_r32_b_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movzx_r32_b_r0_$reg
movzx_r32_b_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrb	$reg, [r2]
	b		loop
	MEND

	movzx_r32_b_r0 eax
	movzx_r32_b_r0 ecx
	movzx_r32_b_r0 edx
	movzx_r32_b_r0 ebx
	movzx_r32_b_r0 esp
	movzx_r32_b_r0 ebp
	movzx_r32_b_r0 esi
	movzx_r32_b_r0 edi

	MACRO
	movzx_r32_b_reg32_reg32 $rl, $rr
	IF "$rr" = "esp"
		ubfx	$rl, eax, #8, #8
	ELIF "$rr" = "ebp"
		ubfx	$rl, ecx, #8, #8
	ELIF "$rr" = "esi"
		ubfx	$rl, edx, #8, #8
	ELIF "$rr" = "edi"
		ubfx	$rl, ebx, #8, #8
	ELSE
		ubfx	$rl, $rr, #0, #8
	ENDIF
	b		loop
	MEND

	modrm_3_genall movzx_r32_b, movzx_r32_b_r0

; ------------------- 0F B7 = MOVZX r32, r/m16 -----------------------
; 650FB7762C = movzx esi, gs:[esi+2C]
;
op_0f_b7_USE32
	modrm_jump_32_tbl op_0f_b7_32_jump
	modrm_tbl_3 movzx_r32_w

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	movzx_reg32_r0 $reg
	GLOBAL	movzx_r32_w_r0_bp_$reg
movzx_r32_w_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movzx_r32_w_r0_$reg
movzx_r32_w_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	IF USE_UNALIGNED = 1
1	ldrh	$reg, [r2]
	ELSE
1	ldrb	$reg, [r2]
	ldrb	r0, [r2, #1]
	orr		$reg, r0, lsl #8
	ENDIF
	b		loop
	MEND

	movzx_reg32_r0 eax
	movzx_reg32_r0 ecx
	movzx_reg32_r0 edx
	movzx_reg32_r0 ebx
	movzx_reg32_r0 esp
	movzx_reg32_r0 ebp
	movzx_reg32_r0 esi
	movzx_reg32_r0 edi

	MACRO
	movzx_r32_w_reg32_reg32 $rl, $rr
	ubfx	$rl, $rr, #0, #16
	b		loop
	MEND

	modrm_3_genall movzx_r32_w, movzx_r32_w_r0

; ------------------- 0F BA = BT/BTS/BTR/BTC r/m32, imm8 -------------
; 0FBAFA15 = btc edx,15
;
op_0f_ba_USE32
	modrm_jump_32_tbl op_0f_ba_32_jump
	modrm_tbl_1_oper back1, back1, back1, back1, bt_32, bts_32, btr_32, btc_32, imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_C0 bt_32, imm8
	modrm_help_1_C0 bts_32, imm8
	modrm_help_1_C0 btr_32, imm8
	modrm_help_1_C0 btc_32, imm8

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	GLOBAL	bt_32_r0_bp_imm8
bt_32_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bt_32_r0_imm8
bt_32_r0_imm8
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldrb	r1, [r12], #1						; Get the imm8 value
	ldr		r2, [r2]							; r2 = dword from RAM
	and		r1, #31
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r0, r1, lsl #8
	ldrb	r1, [r12], #1						; Get the imm8 value
	orr		r0, r3, lsl #16
	and		r1, #31
	orr		r2, r0, r2, lsl #24					; r2 = dword from RAM
	ENDIF
	mrs		r0, cpsr							; Save flags to r0
	mov		r1, r2, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	

	MACRO
	bt_reg32_imm8 $reg
	GLOBAL	bt_32_$reg_imm8
bt_32_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #31
	mov		r1, $reg, lsr r1
	tst		r1, #1								; Is the bit set?
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	bt_reg32_imm8 eax
	bt_reg32_imm8 ecx
	bt_reg32_imm8 edx
	bt_reg32_imm8 ebx
	bt_reg32_imm8 esp
	bt_reg32_imm8 ebp
	bt_reg32_imm8 esi
	bt_reg32_imm8 edi

	GLOBAL	bts_32_r0_bp_imm8
bts_32_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bts_32_r0_imm8
bts_32_r0_imm8
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldrb	r1, [r12], #1						; Get the imm8 value
	ldr		r3, [r2]							; r3 = dword from RAM
	and		r1, #31
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r1, [r12], #1						; Get the imm8 value
	ldrb	r3, [r2, #3]
	and		r1, #31
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	orr		r3, r1								; Set the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop

	MACRO
	bts_reg32_imm8 $reg
	GLOBAL	bts_32_$reg_imm8
bts_32_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #31
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg, r1							; Is the bit set?
	orr		$reg, r1							; Set the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	bts_reg32_imm8 eax
	bts_reg32_imm8 ecx
	bts_reg32_imm8 edx
	bts_reg32_imm8 ebx
	bts_reg32_imm8 esp
	bts_reg32_imm8 ebp
	bts_reg32_imm8 esi
	bts_reg32_imm8 edi

	GLOBAL	btr_32_r0_bp_imm8
btr_32_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	btr_32_r0_imm8
btr_32_r0_imm8
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldrb	r1, [r12], #1						; Get the imm8 value
	ldr		r3, [r2]							; r3 = dword from RAM
	and		r1, #31
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r1, [r12], #1						; Get the imm8 value
	ldrb	r3, [r2, #3]
	and		r1, #31
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	bic		r3, r1								; Clear the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop

	MACRO
	btr_reg32_imm8 $reg
	GLOBAL	btr_32_$reg_imm8
btr_32_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #31
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg, r1							; Is the bit set?
	bic		$reg, r1							; Clear the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	btr_reg32_imm8 eax
	btr_reg32_imm8 ecx
	btr_reg32_imm8 edx
	btr_reg32_imm8 ebx
	btr_reg32_imm8 esp
	btr_reg32_imm8 ebp
	btr_reg32_imm8 esi
	btr_reg32_imm8 edi

	GLOBAL	btc_32_r0_bp_imm8
btc_32_r0_bp_imm8
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	btc_32_r0_imm8
btc_32_r0_imm8
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldrb	r1, [r12], #1						; Get the imm8 value
	ldr		r3, [r2]							; r3 = dword from RAM
	and		r1, #31
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r1, [r12], #1						; Get the imm8 value
	ldrb	r3, [r2, #3]
	and		r1, #31
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	eor		r3, r1								; Complement the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop

	MACRO
	btc_reg32_imm8 $reg
	GLOBAL	btc_32_$reg_imm8
btc_32_$reg_imm8
	ldrb	r1, [r12], #1						; Get the imm8 value
	mrs		r0, cpsr							; Save flags to r0
	and		r1, #31
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg, r1							; Is the bit set?
	eor		$reg, r1							; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	btc_reg32_imm8 eax
	btc_reg32_imm8 ecx
	btc_reg32_imm8 edx
	btc_reg32_imm8 ebx
	btc_reg32_imm8 esp
	btc_reg32_imm8 ebp
	btc_reg32_imm8 esi
	btc_reg32_imm8 edi

	modrm_genall_1_oper skip, skip, skip, skip, bt_32, bts_32, btr_32, btc_32, _r0, imm8

; ------------------- 0F BB = BTC r/m32, r32 --------------------------
;
op_0f_bb_USE32
	modrm_jump_32_tbl op_0f_bb_32_jump
	modrm_tbl_1 btc_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	btc_r0_reg $reg
	GLOBAL	btc_r0_r32_bp_$reg
btc_r0_r32_bp_$reg
	mem_handler_bp
	GLOBAL	btc_r0_r32_$reg
btc_r0_r32_$reg
	;------
	;	CASE_0F_D(0xa3)												/* BT Ed,Gd */
	;		{
	;			FillFlags();GetRMrd;
	;			Bit32u mask=1 << (*rmrd & 31);
	;			if (rm >= 0xc0 ) {
	;				GetEArd;
	;				SETFLAGBIT(CF,(*eard & mask));
	;			} else {
	;				GetEAa;eaa+=(((Bit32s)*rmrd)>>5)*4;
	;				Bit32u old=LoadMd(eaa);
	;				SETFLAGBIT(CF,(old & mask));
	;			}
	;			break;
	;		}
	;------
	mov		r1, $reg, asr #5
	add		r0, r1, lsl #2						;	eaa+=(((Bit32s)*rmrd)>>5)*4;
	mem_handler_jump_r0r3 %f1, bad_386_opcode_back1, bad_386_opcode_back1
	IF USE_UNALIGNED = 1
1	ldr		r3, [r2]							; r3 = dword from RAM
	and		r1, $reg, #0x1F
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	ldrb	r3, [r2, #3]
	and		r1, $reg, #0x1F
	orr		r3, r0, r3, lsl #24					; r3 = dword from RAM
	ENDIF
	mov		r0, #1
	mov		r1, r0, lsl r1						; r1 = mask = (1<<(*rmrw & 15))
	mrs		r0, cpsr							; Save flags to r0
	tst		r3, r1								; Is the bit set?
	eor		r3, r1								; Complement the bit in any case
	IF USE_UNALIGNED = 1
	str		r3, [r2]
	ELSE
	strb	r3, [r2]
	lsr		r3, #8
	strb	r3, [r2, #1]
	lsr		r3, #8
	strb	r3, [r2, #2]
	lsr		r3, #8
	strb	r3, [r2, #3]
	ENDIF
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
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
	btc_r32_reg32_reg32 $reg1, $reg2
	mrs		r0, cpsr							; Save flags to r0
	and		r1, $reg2, #0x1F
	mov		r2, #1
	mov		r1, r2, lsl r1						; r1 = mask = (1<<(*rmrw & 31))
	tst		$reg1, r1							; Is the bit set?
	eor		$reg1, r1							; Complement the bit in any case
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0				; Back to loop
	MEND

	modrm_gen_reg_oper btc_r32, eax
	modrm_gen_reg_oper btc_r32, ecx
	modrm_gen_reg_oper btc_r32, edx
	modrm_gen_reg_oper btc_r32, ebx
	modrm_gen_reg_oper btc_r32, esp
	modrm_gen_reg_oper btc_r32, ebp
	modrm_gen_reg_oper btc_r32, esi
	modrm_gen_reg_oper btc_r32, edi

	modrm_1_genall btc_r32, btc_r0_r32

; ------------------- 0F BC = BSF r32, r/m32 -------------------------
;
op_0f_bc_USE32
	modrm_jump_32_tbl op_0f_bc_32_jump
	modrm_tbl_3 bsf_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	bsf_r32_r0 $reg
	GLOBAL	bsf_r32_r0_bp_$reg
bsf_r32_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bsf_r32_r0_$reg
bsf_r32_r0_$reg
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
	IF USE_UNALIGNED = 1
1	ldr		r2, [r2]
	mrs		r0, cpsr							; Save flags to r0
	cmp		r2, #0
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r0, r1, lsl #8
	mrs		r0, cpsr							; Save flags to r0
	orr		r1, r3, lsl #16
	orrs	r2, r1, r2, lsl #24
	ENDIF
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
bsf_r32_r2_$reg
	bic		r0, #ARM_ZERO						; Clear the FLAG_ZF
	mov		$reg, #0
1	movs	r2, r2, lsr #1						; Put lowest bit to Carry, shift the value right
	bcs		restore_flags_from_r0				; Loop done if Carry set
	add		$reg, #1							; result++
	b		%b1									; Back to loop
	MEND

	bsf_r32_r0 eax
	bsf_r32_r0 ecx
	bsf_r32_r0 edx
	bsf_r32_r0 ebx
	bsf_r32_r0 esp
	bsf_r32_r0 ebp
	bsf_r32_r0 esi
	bsf_r32_r0 edi

	MACRO
	bsf_r32_reg32_reg32 $rl, $rr
	mrs		r0, cpsr							; Save flags to r0
	movs	r2, $rr
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
	b		bsf_r32_r2_$rl
	MEND

	modrm_3_genall bsf_r32, bsf_r32_r0

bsf_ZF_true
	orr		r0, #ARM_ZERO
	b		restore_flags_from_r0

; ------------------- 0F BD = BSR r32, r/m32 -------------------------
;
op_0f_bd_USE32
	modrm_jump_32_tbl op_0f_bd_32_jump
	modrm_tbl_3 bsr_r32

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	bsr_r32_r0 $reg
	GLOBAL	bsr_r32_r0_bp_$reg
bsr_r32_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	bsr_r32_r0_$reg
bsr_r32_r0_$reg
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
	IF USE_UNALIGNED = 1
1	ldr		r2, [r2]
	mrs		r0, cpsr							; Save flags to r0
	cmp		r2, #0
	ELSE
1	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r3, [r2, #2]
	ldrb	r2, [r2, #3]
	orr		r1, r0, r1, lsl #8
	mrs		r0, cpsr							; Save flags to r0
	orr		r1, r3, lsl #16
	orrs	r2, r1, r2, lsl #24
	ENDIF
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
bsr_r32_r2_$reg
	bic		r0, #ARM_ZERO						; Clear the FLAG_ZF
	mov		$reg, #31
1	movs	r2, r2, lsl #1						; Put lowest bit to Carry, shift the value right
	bcs		restore_flags_from_r0				; Loop done if Carry set
	sub		$reg, #1							; result--
	b		%b1									; Back to loop
	MEND

	bsr_r32_r0 eax
	bsr_r32_r0 ecx
	bsr_r32_r0 edx
	bsr_r32_r0 ebx
	bsr_r32_r0 esp
	bsr_r32_r0 ebp
	bsr_r32_r0 esi
	bsr_r32_r0 edi

	MACRO
	bsr_r32_reg32_reg32 $rl, $rr
	mrs		r0, cpsr							; Save flags to r0
	movs	r2, $rr
	beq		bsf_ZF_true							; Go set ZF flag if the value was zero
	b		bsr_r32_r2_$rl
	MEND

	modrm_3_genall bsr_r32, bsr_r32_r0

; ------------------- 0F BE = MOVSX r32, r/m8 ------------------------
; 
op_0f_be_USE32
	modrm_jump_32_tbl op_0f_be_32_jump
	modrm_tbl_3 movsx_r32_b

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	movsx_r32_b_r0 $reg
	GLOBAL	movsx_r32_b_r0_bp_$reg
movsx_r32_b_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movsx_r32_b_r0_$reg
movsx_r32_b_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrsb	$reg, [r2]
	b		loop
	MEND

	movsx_r32_b_r0 eax
	movsx_r32_b_r0 ecx
	movsx_r32_b_r0 edx
	movsx_r32_b_r0 ebx
	movsx_r32_b_r0 esp
	movsx_r32_b_r0 ebp
	movsx_r32_b_r0 esi
	movsx_r32_b_r0 edi

	MACRO
	movsx_r32_b_reg32_reg32 $rl, $rr
	IF "$rr" = "esp"
		sbfx	$rl, eax, #8, #8
	ELIF "$rr" = "ebp"
		sbfx	$rl, ecx, #8, #8
	ELIF "$rr" = "esi"
		sbfx	$rl, edx, #8, #8
	ELIF "$rr" = "edi"
		sbfx	$rl, ebx, #8, #8
	ELSE
		sbfx	$rl, $rr, #0, #8
	ENDIF
	b		loop
	MEND

	modrm_3_genall movsx_r32_b, movsx_r32_b_r0

; ------------------- 0F B7 = MOVSX r32, r/m16 -----------------------
; 
op_0f_bf_USE32
	modrm_jump_32_tbl op_0f_bf_32_jump
	modrm_tbl_3 movsx_r32_w

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	MACRO
	movsx_reg32_r0 $reg
	GLOBAL	movsx_r32_w_r0_bp_$reg
movsx_r32_w_r0_bp_$reg
	;------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;------
	mem_handler_bp
	GLOBAL	movsx_r32_w_r0_$reg
movsx_r32_w_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	IF USE_UNALIGNED = 1
1	ldrsh	$reg, [r2]
	ELSE
1	ldrb	$reg, [r2]
	ldrsb	r0, [r2, #1]
	orr		$reg, r0, lsl #8
	ENDIF
	b		loop
	MEND

	movsx_reg32_r0 eax
	movsx_reg32_r0 ecx
	movsx_reg32_r0 edx
	movsx_reg32_r0 ebx
	movsx_reg32_r0 esp
	movsx_reg32_r0 ebp
	movsx_reg32_r0 esi
	movsx_reg32_r0 edi

	MACRO
	movsx_r32_w_reg32_reg32 $rl, $rr
	sbfx	$rl, $rr, #0, #16
	b		loop
	MEND

	modrm_3_genall movsx_r32_w, movsx_r32_w_r0

	GLOBAL	bswap_eax
bswap_eax
	rev		eax, eax
	b		loop
	GLOBAL	bswap_ecx
bswap_ecx
	rev		ecx, ecx
	b		loop
	GLOBAL	bswap_edx
bswap_edx
	rev		edx, edx
	b		loop
	GLOBAL	bswap_ebx
bswap_ebx
	rev		ebx, ebx
	b		loop
	GLOBAL	bswap_esp
bswap_esp
	rev		esp, esp
	b		loop
	GLOBAL	bswap_ebp
bswap_ebp
	rev		ebp, ebp
	b		loop
	GLOBAL	bswap_esi
bswap_esi
	rev		esi, esi
	b		loop
	GLOBAL	bswap_edi
bswap_edi
	rev		edi, edi
	b		loop

; ------------------- 0F = various opcodes ----------------------------
; 66670FB6AE90150000 = movzx ebp,[esi+00001590]
; 650FB7762C = movzx esi, gs:[esi+2C]
;
	GLOBAL	op_0f_USE32
op_0f_USE32
	modrm_jump_32_tbl op_0f_32_jump
; 0
	DCD op_0f_00_USE32, op_0f_01_USE32, op_0f_02_USE32, op_0f_03_USE32, unknown, unknown, op_0f_06, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_0f_20, op_0f_21, op_0f_22, op_0f_23, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x80
	GLOBAL	op_0f_USE32_cond_jumps
op_0f_USE32_cond_jumps
	DCD jo_imm32, jno_imm32, jc_imm32, jnc_imm32, jz_imm32, jnz_imm32, jbe_imm32, ja_imm32
	DCD js_imm32, jns_imm32, jp_imm32, jnp_imm32, jl_imm32, jge_imm32, jle_imm32, jg_imm32
	DCD op_0f_90_USE32, op_0f_91_USE32, op_0f_92_USE32, op_0f_93_USE32, op_0f_94_USE32, op_0f_95_USE32, op_0f_96_USE32, op_0f_97_USE32
	DCD op_0f_98_USE32, op_0f_99_USE32, op_0f_9a_USE32, op_0f_9b_USE32, op_0f_9C_USE32, op_0f_9D_USE32, op_0f_9E_USE32, op_0f_9F_USE32
	DCD push_fs_USE32, pop_fs_USE32, unknown, op_0f_a3_USE32, op_0f_a4_USE32, op_0f_a5_USE32, unknown, unknown
	DCD push_gs_USE32, pop_gs_USE32, unknown, op_0f_ab_USE32, op_0f_ac_USE32, op_0f_ad_USE32, unknown, op_0f_af_USE32
	DCD unknown, unknown, op_0f_b2_USE32, op_0f_b3_USE32, op_0f_b4_USE32, op_0f_b5_USE32, op_0f_b6_USE32, op_0f_b7_USE32
	DCD unknown, unknown, op_0f_ba_USE32, op_0f_bb_USE32, op_0f_bc_USE32, op_0f_bd_USE32, op_0f_be_USE32, op_0f_bf_USE32
; 0xC0
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD bswap_eax, bswap_ecx, bswap_edx, bswap_ebx, bswap_esp, bswap_ebp, bswap_esi, bswap_edi
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_0F_USE32, CODE, READONLY
	ALIGN	4

	EXTERN	op_0f_06
	EXTERN	op_0f_20
	EXTERN	op_0f_21
	EXTERN	op_0f_22
	EXTERN	op_0f_23

	END
