;=============================================================================
; cpu_prot.s
;
; This file contains protected mode -specific routines and opcode handlers.
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

	AREA cpu_prot, CODE, READONLY

	ALIGN	4

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_16.inc

CPU_INT_SOFTWARE		EQU		0x1
CPU_INT_EXCEPTION		EQU		0x2
CPU_INT_HAS_ERROR		EQU		0x4
CPU_INT_NOIOPLCHECK		EQU		0x8

	EXTERN	loop
	EXTERN	restore_flags_from_r0
	EXTERN	debug_trap_false
	EXTERN	unknown
	EXTERN	prot_start_cont
	
	EXTERN	opcodetable_16_16
	EXTERN	opcodetable_32_32
	EXTERN	registers

	EXTERN	mov_es_r0r2_real
	EXTERN	mov_ss_r0r2_real
	EXTERN	mov_ds_r0r2_real
	EXTERN	mov_fs_r0r2_real
	EXTERN	mov_gs_r0r2_real
	
	
	MACRO
	CheckSegment_r0123
	;------
	;	bool needs_invalidation=false;
	;	Descriptor desc;
	;	if (!cpu.gdt.GetDescriptor(SegValue(es),desc)) needs_invalidation=true;
	;	else switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA:	case DESC_DATA_EU_RO_A:	case DESC_DATA_EU_RW_NA:	case DESC_DATA_EU_RW_A
	;		case DESC_DATA_ED_RO_NA:	case DESC_DATA_ED_RO_A:	case DESC_DATA_ED_RW_NA:	case DESC_DATA_ED_RW_A
	;		case DESC_CODE_N_NC_A:	case DESC_CODE_N_NC_NA:	case DESC_CODE_R_NC_A:	case DESC_CODE_R_NC_NA
	;		== case 0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b
	;			if (cpu.cpl>desc.DPL()) needs_invalidation=true; break;
	;		default: break;	}
	;	if (needs_invalidation) CPU_SetSegGeneral(es,0);
	;------
	ldr		r1, [r0]
	GetDescriptor_r2 r1, %f2
	ldrb	r2, [r2, #5]
	and		r1, r2, #0x1F						; r1 = descriptor type
	and		r2, #3<<5							; r0 = desc.DPL()
	cmp		r1, #0x10
	blt		%f1
	cmp		r1, #0x1b
	bgt		%f1
	cmp		r3, r2, lsr #5						; cpu.cpl > desc.DPL() ?
	ble		%f1
2	mov		r1, #0
	str		r1, [r0]
1
	MEND

	MACRO
	seg_sel_to_eff_seg_r1_r2 $seg, $fault, $npfault
	;------
	;	if ((value & 0xfffc)==0) {
	;		Segs.val[seg]=value;
	;		Segs.phys[seg]=0;	; ??
	;		return false;
	;	}
	;------
	bics	r0, r1, #3
	IF "$seg" = "es"
	streq	r0, [sp, #SP_ES_VALUE]
	streq	r0, [sp, #SP_ES_BASE]
	ELIF "$seg" = "fs"
	streq	r0, [sp, #SP_FS_VALUE]
	streq	r0, [sp, #SP_FS_BASE]
	ELIF "$seg" = "gs"
	streq	r0, [sp, #SP_GS_VALUE]
	streq	r0, [sp, #SP_GS_BASE]
	ELIF "$seg" = "ds"
	streq	r0, [sp, #SP_DS_VALUE]
	streq	r0, [sp, #SP_DS_BASE]
	ENDIF
	beq		%f9
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r2 r1, $fault
	;------
	;	if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;		; extreme pinball
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	push	{r3, r4}
	ldrb	r0, [r2, #5]						; Get the descriptor Access byte
	mov		r3, r0, lsr #5
	and		r3, #3								; r3 = desc.DPL()
	and		r4, r1, #3							; r4 = (value & 3)
	cmp		r4, r3
	popgt	{r3, r4}
	bgt		$fault								; (value & 3) > desc.DPL()
	ldr		r4, =cpu_cpl
	ldrb	r4, [r4]							; r4 = cpu_cpl
	cmp		r4, r3
	pop		{r3, r4}
	bgt		$fault								; cpu.cpl > desc.DPL()
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA:		case DESC_DATA_EU_RO_A
	;		case DESC_DATA_EU_RW_NA:		case DESC_DATA_EU_RW_A
	;		case DESC_DATA_ED_RO_NA:		case DESC_DATA_ED_RO_A
	;		case DESC_DATA_ED_RW_NA:		case DESC_DATA_ED_RW_A
	;		case DESC_CODE_R_NC_A:			case DESC_CODE_R_NC_NA
	;			if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;				; extreme pinball
	;				return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;			}
	;			break;
	;		case DESC_CODE_R_C_A:			case DESC_CODE_R_C_NA
	;			break;
	;		default
	;			; gabriel knight
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		return CPU_PrepareException(EXCEPTION_NP,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	IF "$npfault" = ""
	beq		$fault
	ELSE
	beq		$npfault
	ENDIF	
	;------
	;	Segs.val[seg]=value;
	;------
	IF "$seg" = "es"
	str		r1, [sp, #SP_ES_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	str		r0, [sp, #SP_ES_BASE]
	ELIF "$seg" = "fs"
	str		r1, [sp, #SP_FS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	str		r0, [sp, #SP_FS_BASE]
	ELIF "$seg" = "gs"
	str		r1, [sp, #SP_GS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	str		r0, [sp, #SP_GS_BASE]
	ELIF "$seg" = "ds"
	str		r1, [sp, #SP_DS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r0, r2, lsl #24
	str		r0, [sp, #SP_DS_BASE]
	ENDIF
9
	MEND

	MACRO
	cs_sel_to_eff_cs_r1_r2 $fault, $npfault
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r2 r1, $fault
	ldrb	r0, [r2, #5]						; Get the descriptor Access byte
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;		return;
	;	}
	;------
	tst		r0, #0x80
	IF "$npfault" = ""
	beq		$fault
	ELSE
	beq		$npfault
	ENDIF	
	;------
	;	Bitu rpl=selector & 3;
	;
	;	switch (desc.Type()) {
	;	case DESC_CODE_N_NC_A:		case DESC_CODE_N_NC_NA
	;	case DESC_CODE_R_NC_A:		case DESC_CODE_R_NC_NA
	;		CPU_CHECK_COND(rpl>cpu.cpl,
	;			"JMP:NC:RPL>CPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		CPU_CHECK_COND(cpu.cpl!=desc.DPL(),
	;			"JMP:NC:RPL != DPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:Code:NC to %X:%X big %d",selector,offset,desc.Big());
	;		goto CODE_jmp;
	;	case DESC_386_TSS_A
	;		CPU_CHECK_COND(desc.DPL()<cpu.cpl,
	;			"JMP:TSS:dpl<cpl",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		CPU_CHECK_COND(desc.DPL()<rpl,
	;			"JMP:TSS:dpl<rpl",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:TSS to %X",selector);
	;		CPU_SwitchTask(selector,TSwitch_JMP,oldeip);
	;		break;
	;	case DESC_CODE_N_C_A:		case DESC_CODE_N_C_NA
	;	case DESC_CODE_R_C_A:		case DESC_CODE_R_C_NA
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:Code:C to %X:%X big %d",selector,offset,desc.Big());
	;		CPU_CHECK_COND(cpu.cpl<desc.DPL(),
	;			"JMP:C:CPL < DPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;------
	and		r0, #0x1F
	cmp		r0, #0x18							; Segment needs to be a Code segment!
	blt		$fault
	;------
	; CODE_jmp
	;	if (!desc.saved.seg.p) {
	;		; win
	;		CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;		return;
	;	}
	;------
	;------
	;	/* Normal jump to another selector:offset */
	;	Segs.val[cs]=(selector & 0xfffc) | cpu.cpl;
	;------
	ldr		r0, =cpu_cpl
	ldrb	r0, [r0]
	bic		r1, #3
	orr		r1, r0
	str		r1, [sp, #SP_CS_VALUE]
	;------
	;	Segs.phys[cs]=desc.GetBase();
	;	reg_ip=offset;
	;	return;
	;------
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r2, r0, r2, lsl #24
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	MEND

	MACRO
	ss_sel_to_eff_ss_r1_r2 $fault
	;------
	;	Stack needs to be non-zero
	;	if ((value & 0xfffc)==0) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment zero");
	;	}
	;------
	bics	r0, r1, #3
	beq		$fault
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r2 r1, $fault
	;------
	;	if (((value & 3)!=cpu.cpl) || (desc.DPL()!=cpu.cpl)) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment with invalid privileges");
    ;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	push	{r3}
	ldr		r3, =cpu_cpl
	ldrb	r3, [r3]							; r3 = cpu_cpl
	and		r0, r1, #3							; r0 = value & 3
	cmp		r0, r3
	popne	{r3}
	bne		$fault
	ldrb	r0, [r2, #5]						; Get the descriptor Access byte
	eor		r3, r0, lsr #5
	ands	r3, #3
	pop		{r3}
	bne		$fault
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RW_NA:		case DESC_DATA_EU_RW_A
	;		case DESC_DATA_ED_RW_NA:		case DESC_DATA_ED_RW_A
	;			break;
	;		default
	;			;Earth Siege 1
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;	if (!desc.saved.seg.p) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment not present");	; or #SS(sel)
	;		return CPU_PrepareException(EXCEPTION_SS,value & 0xfffc);
	;	}
	;------
	and		r0, #0x9A							; Present, Descriptor, Executable, Writeable bits tested
	cmp		r0, #0x92							; Must be Present, Descriptor, Data, Writeable
	bne		$fault
	;------
	;	Segs.val[seg]=value;
	;------
	str		r1, [sp, #SP_SS_VALUE]
	;------
	;	if (desc.Big()) {
	;		cpu.stack.big=true;
	;		cpu.stack.mask=0xffffffff;
	;		cpu.stack.notmask=0;
	;	} else {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;------
	ldrb	r0, [r2, #6]						; Get the descriptor Flags byte
	mvn		r1, #0
	tst		r0, #0x40
	lsreq	r1, #16
	ldr		r3, =stack_mask
	str		r1, [r3]							; Save the stack mask, 0x0000FFFF or 0xFFFFFFFF
	str		r1, [sp, #SP_SP_MASK]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	ldrb	r3, [r2, #4]
	ldrb	r2, [r2, #7]
	orr		r0, r1, lsl #8
	orr		r0, r3, lsl #16
	orr		r2, r0, r2, lsl #24
	str		r2, [sp, #SP_SS_BASE]
	mov		lr, r2
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_SS]
	MEND

	EXTERN	reg_es
	EXTERN	reg_cs
	EXTERN	reg_ss
	EXTERN	reg_ds
	EXTERN	reg_fs
	EXTERN	reg_gs

	GLOBAL init_prot_segments
init_prot_segments
	;-------
	; Init CS
	;-------
	ldr		r0, =reg_cs
	ldr		r1, [r0]
	cs_sel_to_eff_cs_r1_r2 debug_trap_false
	;-------
	; Init ES
	;-------
	ldr		r0, =reg_es
	ldr		r1, [r0]
	seg_sel_to_eff_seg_r1_r2 es, debug_trap_false
	;-------
	; Init DS
	;-------
	ldr		r0, =reg_ds
	ldr		r1, [r0]
	seg_sel_to_eff_seg_r1_r2 ds, debug_trap_false
	;-------
	; Init SS
	;-------
	ldr		r0, =reg_ss
	ldr		r1, [r0]
	ss_sel_to_eff_ss_r1_r2 debug_trap_false
	;-------
	; Init FS
	;-------
	ldr		r0, =reg_fs
	ldr		r1, [r0]
	seg_sel_to_eff_seg_r1_r2 fs, debug_trap_false
	;-------
	; Init GS
	;-------
	ldr		r0, =reg_gs
	ldr		r1, [r0]
	seg_sel_to_eff_seg_r1_r2 gs, debug_trap_false
	b		prot_start_cont

; ------------------- 8E = MOV Sreg,r/m16 -----------------------------
; On input
;	r0 = saved flags
;	r2 = segment selector value
;
	GLOBAL	mov_es_r0r2_prot
mov_es_r0r2_prot
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	Segs.val[seg]=value;
	;	Segs.phys[seg]=value << 4;
	;	if (seg==ss) {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;	return false;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		mov_es_r0r2_real					; Yes, so use the real mode handler.
	;------
	;	if ((value & 0xfffc)==0) {
	;		Segs.val[seg]=value;
	;		Segs.phys[seg]=0;	; ??
	;		return false;
	;	}
	;------
	bics	r1, r2, #3
	moveq	r2, #0
	beq		mov_es_r0r2_real					; Behaviour is similar to that of real mode, so go there.
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	and		r1, r2, #3							; r1 = (value & 3)
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		return CPU_PrepareException(EXCEPTION_NP,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA: 0x10	case DESC_DATA_EU_RO_A: 0x11
	;		case DESC_DATA_EU_RW_NA: 0x12	case DESC_DATA_EU_RW_A: 0x13
	;		case DESC_DATA_ED_RO_NA: 0x14	case DESC_DATA_ED_RO_A: 0x15
	;		case DESC_DATA_ED_RW_NA: 0x16	case DESC_DATA_ED_RW_A: 0x17
	;		case DESC_CODE_R_NC_A:	 0x1A	case DESC_CODE_R_NC_NA: 0x1B
	;			if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;				; extreme pinball
	;				return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;			}
	;			break;
	;		case DESC_CODE_R_C_A:	0x1E	case DESC_CODE_R_C_NA: 0x1F
	;			break;
	;		default: 0..0F, 0x18, 0x19, 0x1C, 0x1D = 0b11000, 0b11001, 0b11100, 0b11101
	;			; gabriel knight
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT			; Data or Code descriptor?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_BIT					; Code descriptor?
	beq		%f1									; Nope, so go check the protection level
	tst		r0, #DESC_CODE_READABLE_BIT			; Readable code segment?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_CONFORMING_BIT		; Conforming code segment?
	bne		%f2									; Yep, so skip protection check
	;------
	;	if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;		; extreme pinball
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
1	lsr		r0, #5
	and		r0, #3								; r0 = desc.DPL()
	cmp		r1, r0								; (value & 3) > desc.DPL()?
	ldrb	r1, [sp, #SP_CPU_CPL]
	bgt		cpu_jmp_GP_fault
	cmp		r1, r0								; cpu.cpl > desc.DPL()?
	bgt		cpu_jmp_GP_fault
	;------
	;	Segs.val[seg]=value;
	;------
2	str		r2, [sp, #SP_ES_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	str		r0, [sp, #SP_ES_BASE]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

ss_fault
	str		r0, [sp, #SP_FREE5]
	b		unknown

	GLOBAL	mov_ss_r0r2_prot
mov_ss_r0r2_prot
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	Segs.val[seg]=value;
	;	Segs.phys[seg]=value << 4;
	;	if (seg==ss) {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;	return false;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		mov_ss_r0r2_real					; Yes, so use the real mode handler.
	;------
	;	; Stack needs to be non-zero
	;	if ((value & 0xfffc)==0) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment zero");
	;	}
	;------
	bics	r1, r2, #3
	moveq	r0, #1
	beq		ss_fault
	beq		unknown
	;------
	;	Descriptor desc;
	;	if (!cpu.gdt.GetDescriptor(value,desc)) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment beyond limits");
	;	}
	;------
	GetDescriptor_r3_from_r2_destroy_r0 unknown
	;------
	;	if (((value & 3)!=cpu.cpl) || (desc.DPL()!=cpu.cpl)) {
	;		E_Exit("CPU_SetSegGeneral: Stack segment with invalid privileges");
	;	}
	;------
	ldrb	r1, [sp, #SP_CPU_CPL]				; r1 = cpu.cpl
	and		r0, r2, #3							; r0 = (value & 3)
	cmp		r0, r1
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	movne	r0, #2
	bne		ss_fault
	bne		unknown
	eor		r1, r0, lsr #5
	ands	r1, #3
	movne	r0, #3
	bne		ss_fault
	bne		unknown
	;------
	;	switch (desc.Type()) {
	;	case DESC_DATA_EU_RW_NA:		case DESC_DATA_EU_RW_A
	;	case DESC_DATA_ED_RW_NA:		case DESC_DATA_ED_RW_A
	;		break;
	;	default
	;		;Earth Siege 1
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT			; Data or Code descriptor?
	moveq	r0, #4
	beq		ss_fault
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_BIT					; Code descriptor?
	movne	r0, #5
	bne		ss_fault
	bne		cpu_jmp_GP_fault					; Yep, so GP fault!
	tst		r0, #DESC_DATA_READWRITE_BIT		; Writeable data segment?
	moveq	r0, #6
	beq		ss_fault
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	;------
	;	if (!desc.saved.seg.p) {
	;		return CPU_PrepareException(EXCEPTION_SS,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	moveq	r0, #7
	beq		ss_fault
	beq		cpu_jmp_SS_fault
	;------
	;	Segs.val[seg]=value;
	;------
	str		r2, [sp, #SP_SS_VALUE]
	;------
	;	if (desc.Big()) {
	;		cpu.stack.big=true;
	;		cpu.stack.mask=0xffffffff;
	;		cpu.stack.notmask=0;
	;	} else {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;------
	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	mvn		r1, #0
	tst		r0, #0x40
	lsreq	r1, #16
	ldr		r2, =stack_mask
	str		r1, [r2]							; Save the stack mask, 0x0000FFFF or 0xFFFFFFFF
	str		r1, [sp, #SP_SP_MASK]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r2, r0, r3, lsl #24
	str		r2, [sp, #SP_SS_BASE]
	mov		lr, r2								; Setup lr = BP-relative indexing base for the opcode jump below.
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_SS]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	msr		cpsr_f,r0							; Restore flags
	;-------
	; NOTE! x86 disables interrupts until the next instruction has been executed.
	; Thus we must handle the next opcode immediately!
	;-------
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; r2 high halfword = logical DS segment, clear segment override flags
	str		r12, [sp, #SP_EX_CSIP]				; Remember where this opcode started, for division-by-zero and exception handling
	ldr		pc,[sp, r0, lsl #2]					; Jump to the opcode handler

	GLOBAL	mov_ds_r0r2_prot
mov_ds_r0r2_prot
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	Segs.val[seg]=value;
	;	Segs.phys[seg]=value << 4;
	;	if (seg==ss) {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;	return false;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		mov_ds_r0r2_real					; Yes, so use the real mode handler.
	;------
	;	if ((value & 0xfffc)==0) {
	;		Segs.val[seg]=value;
	;		Segs.phys[seg]=0;	; ??
	;		return false;
	;	}
	;------
	bics	r1, r2, #3
	moveq	r2, #0
	beq		mov_ds_r0r2_real					; Behaviour is similar to that of real mode, so go there.
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	and		r1, r2, #3							; r1 = (value & 3)
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		return CPU_PrepareException(EXCEPTION_NP,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA: 0x10	case DESC_DATA_EU_RO_A: 0x11
	;		case DESC_DATA_EU_RW_NA: 0x12	case DESC_DATA_EU_RW_A: 0x13
	;		case DESC_DATA_ED_RO_NA: 0x14	case DESC_DATA_ED_RO_A: 0x15
	;		case DESC_DATA_ED_RW_NA: 0x16	case DESC_DATA_ED_RW_A: 0x17
	;		case DESC_CODE_R_NC_A:	 0x1A	case DESC_CODE_R_NC_NA: 0x1B
	;			if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;				; extreme pinball
	;				return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;			}
	;			break;
	;		case DESC_CODE_R_C_A:	0x1E	case DESC_CODE_R_C_NA: 0x1F
	;			break;
	;		default: 0..0F, 0x18, 0x19, 0x1C, 0x1D = 0b11000, 0b11001, 0b11100, 0b11101
	;			; gabriel knight
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT			; Data or Code descriptor?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_BIT					; Code descriptor?
	beq		%f1									; Nope, so go check the protection level
	tst		r0, #DESC_CODE_READABLE_BIT			; Readable code segment?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_CONFORMING_BIT		; Conforming code segment?
	bne		%f2									; Yep, so skip protection check
	;------
	;	if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;		; extreme pinball
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
1	lsr		r0, #5
	and		r0, #3								; r0 = desc.DPL()
	cmp		r1, r0								; (value & 3) > desc.DPL()?
	ldrb	r1, [sp, #SP_CPU_CPL]
	bgt		cpu_jmp_GP_fault
	cmp		r1, r0								; cpu.cpl > desc.DPL()?
	bgt		cpu_jmp_GP_fault
	;------
	;	Segs.val[seg]=value;
	;------
2	str		r2, [sp, #SP_DS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	str		r0, [sp, #SP_DS_BASE]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	GLOBAL	mov_fs_r0r2_prot
mov_fs_r0r2_prot
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	Segs.val[seg]=value;
	;	Segs.phys[seg]=value << 4;
	;	if (seg==ss) {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;	return false;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		mov_fs_r0r2_real					; Yes, so use the real mode handler.
	;------
	;	if ((value & 0xfffc)==0) {
	;		Segs.val[seg]=value;
	;		Segs.phys[seg]=0;	; ??
	;		return false;
	;	}
	;------
	bics	r1, r2, #3
	moveq	r2, #0
	beq		mov_fs_r0r2_real					; Behaviour is similar to that of real mode, so go there.
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	and		r1, r2, #3							; r1 = (value & 3)
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		return CPU_PrepareException(EXCEPTION_NP,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA: 0x10	case DESC_DATA_EU_RO_A: 0x11
	;		case DESC_DATA_EU_RW_NA: 0x12	case DESC_DATA_EU_RW_A: 0x13
	;		case DESC_DATA_ED_RO_NA: 0x14	case DESC_DATA_ED_RO_A: 0x15
	;		case DESC_DATA_ED_RW_NA: 0x16	case DESC_DATA_ED_RW_A: 0x17
	;		case DESC_CODE_R_NC_A:	 0x1A	case DESC_CODE_R_NC_NA: 0x1B
	;			if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;				; extreme pinball
	;				return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;			}
	;			break;
	;		case DESC_CODE_R_C_A:	0x1E	case DESC_CODE_R_C_NA: 0x1F
	;			break;
	;		default: 0..0F, 0x18, 0x19, 0x1C, 0x1D = 0b11000, 0b11001, 0b11100, 0b11101
	;			; gabriel knight
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT			; Data or Code descriptor?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_BIT					; Code descriptor?
	beq		%f1									; Nope, so go check the protection level
	tst		r0, #DESC_CODE_READABLE_BIT			; Readable code segment?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_CONFORMING_BIT		; Conforming code segment?
	bne		%f2									; Yep, so skip protection check
	;------
	;	if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;		; extreme pinball
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
1	lsr		r0, #5
	and		r0, #3								; r0 = desc.DPL()
	cmp		r1, r0								; (value & 3) > desc.DPL()?
	ldrb	r1, [sp, #SP_CPU_CPL]
	bgt		cpu_jmp_GP_fault
	cmp		r1, r0								; cpu.cpl > desc.DPL()?
	bgt		cpu_jmp_GP_fault
	;------
	;	Segs.val[seg]=value;
	;------
2	str		r2, [sp, #SP_FS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	str		r0, [sp, #SP_FS_BASE]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	GLOBAL	mov_gs_r0r2_prot
mov_gs_r0r2_prot
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	Segs.val[seg]=value;
	;	Segs.phys[seg]=value << 4;
	;	if (seg==ss) {
	;		cpu.stack.big=false;
	;		cpu.stack.mask=0xffff;
	;		cpu.stack.notmask=0xffff0000;
	;	}
	;	return false;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		mov_gs_r0r2_real					; Yes, so use the real mode handler.
	;------
	;	if ((value & 0xfffc)==0) {
	;		Segs.val[seg]=value;
	;		Segs.phys[seg]=0;	; ??
	;		return false;
	;	}
	;------
	bics	r1, r2, #3
	moveq	r2, #0
	beq		mov_gs_r0r2_real					; Behaviour is similar to that of real mode, so go there.
	;------
	;	Descriptor desc;
	;	bool GetDescriptor(Bitu selector, Descriptor& desc) {
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	and		r1, r2, #3							; r1 = (value & 3)
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		return CPU_PrepareException(EXCEPTION_NP,value & 0xfffc);
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;	switch (desc.Type()) {
	;		case DESC_DATA_EU_RO_NA: 0x10	case DESC_DATA_EU_RO_A: 0x11
	;		case DESC_DATA_EU_RW_NA: 0x12	case DESC_DATA_EU_RW_A: 0x13
	;		case DESC_DATA_ED_RO_NA: 0x14	case DESC_DATA_ED_RO_A: 0x15
	;		case DESC_DATA_ED_RW_NA: 0x16	case DESC_DATA_ED_RW_A: 0x17
	;		case DESC_CODE_R_NC_A:	 0x1A	case DESC_CODE_R_NC_NA: 0x1B
	;			if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;				; extreme pinball
	;				return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;			}
	;			break;
	;		case DESC_CODE_R_C_A:	0x1E	case DESC_CODE_R_C_NA: 0x1F
	;			break;
	;		default: 0..0F, 0x18, 0x19, 0x1C, 0x1D = 0b11000, 0b11001, 0b11100, 0b11101
	;			; gabriel knight
	;			return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT			; Data or Code descriptor?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_BIT					; Code descriptor?
	beq		%f1									; Nope, so go check the protection level
	tst		r0, #DESC_CODE_READABLE_BIT			; Readable code segment?
	beq		cpu_jmp_GP_fault					; Nope, so GP fault!
	tst		r0, #DESC_CODE_CONFORMING_BIT		; Conforming code segment?
	bne		%f2									; Yep, so skip protection check
	;------
	;	if (((value & 3)>desc.DPL()) || (cpu.cpl>desc.DPL())) {
	;		; extreme pinball
	;		return CPU_PrepareException(EXCEPTION_GP,value & 0xfffc);
	;	}
	;------
1	lsr		r0, #5
	and		r0, #3								; r0 = desc.DPL()
	cmp		r1, r0								; (value & 3) > desc.DPL()?
	ldrb	r1, [sp, #SP_CPU_CPL]
	bgt		cpu_jmp_GP_fault
	cmp		r1, r0								; cpu.cpl > desc.DPL()?
	bgt		cpu_jmp_GP_fault
	;------
	;	Segs.val[seg]=value;
	;------
2	str		r2, [sp, #SP_GS_VALUE]
	;------
	;	Segs.phys[seg]=desc.GetBase();
	;------
	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	str		r0, [sp, #SP_GS_BASE]
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	LTORG


; ------------------- 6C = INSB ---------------------------------------
;
	GLOBAL	op_6c_prot_r0r2
op_6c_prot_r0r2
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r3, [sp, #SP_FLAGS]			; r3 = SP_FLAGS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, #FLAG_IOPL				; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_6c_real_r0r2				; Continue with the common INSB handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_6c_real_r0r2
	
	GLOBAL	op_6c_USE32_prot_r0r2
op_6c_USE32_prot_r0r2
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r3, [sp, #SP_FLAGS]			; r3 = SP_FLAGS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, #FLAG_IOPL				; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_6c_USE32_real_r0r2		; Continue with the common INSB handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_6c_USE32_real_r0r2
	
; ------------------- 6E = OUTSB --------------------------------------
;
	GLOBAL	op_6e_prot_r0r2
op_6e_prot_r0r2
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r3, [sp, #SP_FLAGS]			; r3 = SP_FLAGS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, #FLAG_IOPL				; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_6e_real_r0r2				; Continue with the common INSB handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_6e_real_r0r2
	
	GLOBAL	op_6e_USE32_prot_r0r2
op_6e_USE32_prot_r0r2
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r3, [sp, #SP_FLAGS]			; r3 = SP_FLAGS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, #FLAG_IOPL				; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_6e_USE32_real_r0r2		; Continue with the common INSB handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_6e_USE32_real_r0r2
	
	LTORG

; ------------------- 9A = CALL FAR PROT -------------------------------
; 0080:00292CE8 9AFF018800      call 0088:01FF (DOOM)
;
; On input
;	r0 = saved flags
;	r1 = new logical IP (offset)
;	r2 = new CS selector (segment)
;
	GLOBAL	cpu_call_prot_r0r1r2
cpu_call_prot_r0r1r2
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	if (!use32) {
	;		reg_eip=offset&0xffff;
	;	} else {
	;		reg_eip=offset;
	;	}
	;	SegSet16(cs,selector);
	;	cpu.code.big=false;
	;	return;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]				; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]			; Save the flags temporarily to stack
	tst		r3, #FLAG_VM					; Are we in VM mode?
	bne		cpu_call_real_r0r1r2			; Yes, so use the real mode handler. TODO! Set cpu.code.big=false;
	;------
	;	} else {
	;		CPU_CHECK_COND((selector & 0xfffc)==0,
	;			"CALL:CS selector zero",
	;			EXCEPTION_GP,0)
	;------
	;------
	;		Bitu rpl=selector & 3;
	;		Descriptor call;
	;		CPU_CHECK_COND(!cpu.gdt.GetDescriptor(selector,call),
	;			"CALL:CS beyond limits",
	;			EXCEPTION_GP,selector & 0xfffc)
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	;------
	;	if (!desc.saved.seg.p) {
	;		; Borland RTM
	;		CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;		return;
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;		/* Check for type of far call */
	;		switch (call.Type()) {
	;		case DESC_CODE_N_NC_A:case DESC_CODE_N_NC_NA:		0x18, 0x19, 0x1a, 0x1b
	;		case DESC_CODE_R_NC_A:case DESC_CODE_R_NC_NA
	;------
	and		r0, #0x1F
	cmp		r0, #0x18							; Segment needs to be a Code segment! TODO! CPU_SwitchTask() handling!
	blt		cpu_jmp_GP_fault
;	cmp		r0, #0x1C							; Non-conforming (< 0x1C) or conforming (>= 0x1C) code?
	ldrb	r0, [sp, #SP_CPU_CPL]				; r0 = CPU_CPL
;	bge		.op_9a_conforming_code
	;------
	;			CPU_CHECK_COND(rpl>cpu.cpl,
	;				"CALL:CODE:NC:RPL>CPL",
	;				EXCEPTION_GP,selector & 0xfffc)
	;			CPU_CHECK_COND(call.DPL()!=cpu.cpl,
	;				"CALL:CODE:NC:DPL!=CPL",
	;				EXCEPTION_GP,selector & 0xfffc)
	;			LOG(LOG_CPU,LOG_NORMAL)("CALL:CODE:NC to %X:%X",selector,offset);
	;			goto call_code;	
	;------
	; TODO!
;	b		.op_9a_call_code
	;------
	;		case DESC_CODE_N_C_A:case DESC_CODE_N_C_NA:			0x1c, 0x1d, 0x1e, 0x1f
	;		case DESC_CODE_R_C_A:case DESC_CODE_R_C_NA
	;------
;.op_9a_conforming_code
	;------
	;			CPU_CHECK_COND(call.DPL()>cpu.cpl,
	;				"CALL:CODE:C:DPL>CPL",
	;				EXCEPTION_GP,selector & 0xfffc)
	;			LOG(LOG_CPU,LOG_NORMAL)("CALL:CODE:C to %X:%X",selector,offset);
	;------
	; TODO!
; .op_9a_call_code
	;------
	;			; commit point
	;------
	;------
	;			if (!use32) {
	;				CPU_Push16(SegValue(cs));
	;				CPU_Push16(oldeip);
	;				reg_eip=offset & 0xffff;
	;			} else {
	;				CPU_Push32(SegValue(cs));
	;				CPU_Push32(oldeip);
	;				reg_eip=offset;
	;			}
	;------
	ldr		r0, [sp, #SP_FREE3]					; Get the call type from stack
	cmp		r0, #32
	ldr		lr, [sp, #SP_PHYS_CS]
	ldr		r0, [sp, #SP_CS_VALUE]				; r0 = SegValue(cs)
	sub		lr, r12, lr							; lr = oldeip
	mov		r12, r1								; r12 = reg_eip = offset
	beq		%f32
	;------
	; USE16 push
	;------
	orr		lr, r0, lsl #16						; lr = oldeip | (SegValue(cs) << 16
	push_dword lr, r0, r1
	b		%f16
	;------
	; USE32 push
	;------
32	str		r4, [sp, #SP_R4SAVE]
	push_dword r0, r1, r4
	push_dword lr, r0, r1
	ldr		r4, [sp, #SP_R4SAVE]
	;------
	; Register here
	;	r0 = free
	;	r1 = free
	;	r2 = selector
	;	r3 = pointer to cs_desc
	;	r12 = reg_eip = offset
	;			Segs.val[cs]=(selector & 0xfffc) | cpu.cpl;
	;------
16	ldrb	r0, [sp, #SP_CPU_CPL]
	bic		r2, #3
	orr		r2, r0
	str		r2, [sp, #SP_CS_VALUE]
	;------
	;	cpu.code.big=desc.Big()>0;
	;------
	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	ldrb	r1, [sp, #SP_CPU_BIG]
	and		r0, #0x40
	cmp		r0, r1								; Did the cpu_code_big value change?
	strb	r0, [sp, #SP_CPU_BIG]
	beq		%f2									; No change, so skip changing the opcode table
	fix_opcode_table_destroy_flags				; cpu_code_big changed, so change the main opcode table in stack.
	;------
	;	Segs.phys[cs]=desc.GetBase();
	;	return;
	;------
2	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r2, r0, r2, lsl #16
	orr		r2, r3, lsl #24						; r2 = new logical CS base address
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	add		r12, r2								; r12 = new physical CS:IP
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	EXTERN	cpu_call_real_r0r1r2
	

; ------------------- CA = RETF imm16 / CB = RETF ---------------------
;
; On input
;	r0 = saved flags
;	r3 = (use32<<16) | (extra_words_to_pop)
;
	GLOBAL	cpu_retf_prot_r0r3
cpu_retf_prot_r0r3
	;------
	; void CPU_RET(bool use32,Bitu bytes,Bitu oldeip) {
	;------
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	SegSet16(cs,selector);
	;	cpu.code.big=false;
	;	return;
	; }
	;------
	ldr		r1, [sp, #SP_FLAGS]					; r1 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	tst		r1, #FLAG_VM						; Are we in VM mode?
	bne		cpu_retf_real_r0r3					; Yes, so use the real mode handler. TODO! Set cpu.code.big=false;
	;------
	;	Bitu offset,selector;
	;	if (!use32) selector	= mem_readw(SegPhys(ss) + (reg_esp & cpu.stack.mask) + 2);
	;	else 		selector	= mem_readd(SegPhys(ss) + (reg_esp & cpu.stack.mask) + 4) & 0xffff;
	;------
	ldr		r0, [sp, #SP_SP_MASK]				; Much like in 'pop_reg_low_hi' macro!
	ldr		r2, [sp, #SP_PHYS_SS]
	and		r0, esp
	add		r0, r2								; Now r0 = physical stack address
	tst		r3, #0x10000						; USE32?
	addne	r0, #2
	ldrb	r2, [r0, #2]						; Pop the low byte
	ldrb	r1, [r0, #3]						; Pop the high byte
	str		r3, [sp, #SP_FREE2]					; Save the USE32 flag and extra words to pop to stack.
	orr		r2, r1, lsl #8						; r2 = selector
	;------
	;		CPU_CHECK_COND(!cpu.gdt.GetDescriptor(selector,desc),
	;			"RET:CS beyond limits",
	;			EXCEPTION_GP,selector & 0xfffc)
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	;------
	;	Descriptor desc;
	;	Bitu rpl=selector & 3;
	;	if(rpl < cpu.cpl) {
	;		; win setup
	;		CPU_Exception(EXCEPTION_GP,selector & 0xfffc);
	;		return;
	;	}
	;------
	ldrb	r0, [sp, #SP_CPU_CPL]
	and		r1, r2, #3							; r1 = rpl
	cmp		r1, r0
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	blt		cpu_jmp_GP_fault
	;------
	;		if (cpu.cpl==rpl) {	
	;------
	bne		retf_outer_level
	;======
	;			/* Return to same level */
	;			switch (desc.Type()) {
	;======
	and		r1, r0, #0x1F
	;------
	;			default
	;				E_Exit("RET from illegal descriptor type %X",desc.Type());
	;			}
	;------
	cmp		r1, #(DESC_DATA_OR_CODE_BIT|DESC_CODE_BIT)	; must have Descriptor and Code bits on
	blt		cpu_jmp_GP_fault
	;------
	;			if (!desc.saved.seg.p) {
	;				; borland extender (RTM)
	;				CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;				return;
	;			}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;			; commit point
	;			if (!use32) {
	;				offset=CPU_Pop16();
	;				selector=CPU_Pop16();
	;			} else {
	;				offset=CPU_Pop32();
	;				selector=CPU_Pop32() & 0xffff;
	;			}
	;			Segs.val[cs]=selector;
	;------
	ldr		r0, [sp, #SP_SP_MASK]				; Much like in 'pop_reg_low_hi' macro!
	ldr		r1, [sp, #SP_PHYS_SS]
	and		r0, esp
	add		r0, r1								; Now r0 = physical stack address
	ldr		r1, [sp, #SP_FREE2]					; Get the USE32 flag and extra words to pop from stack.
	str		r2, [sp, #SP_CS_VALUE]				; Segs.val[cs]=selector;
	tst		r1, #0x10000						; USE32?
	bne		%f32
	;------
	; USE16
	;			offset=CPU_Pop16();
	;			reg_sp+=bytes;
	;------
	add		esp, #(2*2)
	add		esp, r1								; ESP is now correct
	ldrb	r12, [r0]							; Pop the low byte
	ldrb	r0, [r0, #1]						; Pop the high byte
	orr		r12, r0, lsl #8						; r12 = offset
	b		%f16
	;------
	; USE32
	;			offset=CPU_Pop32();
	;			reg_esp+=bytes;
	;------
32	bic		r1, #0x10000						; r1 has the extra bytes to pop
	add		esp, #(2*4)
	add		esp, r1								; ESP is now correct
	ldrb	r12, [r0]
	ldrb	r1, [r0, #1]
	ldrb	r2, [r0, #2]
	ldrb	r0, [r0, #3]
	orr		r12, r1, lsl #8
	orr		r12, r2, lsl #16
	orr		r12, r0, lsl #24					; r12 = offset
	;------
	;	cpu.code.big=desc.Big()>0;
	;------
16	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	ldrb	r1, [sp, #SP_CPU_BIG]
	and		r0, #0x40
	cmp		r0, r1								; Did the cpu_code_big value change?
	strb	r0, [sp, #SP_CPU_BIG]
	beq		%f2									; No change, so skip changing the opcode table
	fix_opcode_table_destroy_flags				; cpu_code_big changed, so change the main opcode table in stack.
	;------
	;	Segs.phys[cs]=desc.GetBase();
	;	return;
	;------
2	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r2, r0, r2, lsl #16
	orr		r2, r3, lsl #24						; r2 = new logical CS base address
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	add		r12, r2								; r12 = new physical CS:IP
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags
	
	EXTERN	cpu_retf_real_r0r3


	;------
	;		} else {
	;			/* Return to outer level */
	;------
	GLOBAL	retf_outer_level
retf_outer_level
	b		unknown


retf_GP_fault
	b		unknown

	LTORG

	;-------
	; Handle a Page Fault (0x0B) (Windows 3.0)
	; Registers here
	;	r0 = free
	;	r1 = selector causing the fault
	;	r2 = free
	;	[sp, #SP_STR_SEG] = saved CPU flags
	;	[sp, #SP_EX_CSIP] = original CS:IP
	;	CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;		void CPU_Exception(Bitu which,Bitu error ) {
	;			cpu.exception.error=error;
	;			CPU_Interrupt(which,CPU_INT_EXCEPTION | ((which>=8) ? CPU_INT_HAS_ERROR : 0),reg_eip);
	;		}
	;-------
	GLOBAL	PageFault_r1
PageFault_r1
	ldr		r0, [sp, #SP_STR_SEG]			; Restore the flags from stack
	lsr		r0, #24
	orr		r4, r0							; Put the flags to r4 low byte
PageFault_r1_r4flags						; Jumped to from RETF NP fault
	ldr		r12, [sp, #SP_EX_CSIP]			; Restore the original CS:IP
	mov		r0, #EXCEPTION_NP				; r0 = Exception value = EXCEPTION_NP
	bic		r1, #0x03						; r1 = error value = selector & 0xFFFC
	sub		r12, #1
;	b		cpu_exception					; Go start a CPU exception

; ------------------- CPU Exception code ------------------------------
; Generate an exception.
; On input
;	r0 = Exception number
;	r1 = Error value
;	r4 = low byte = saved CPU flags
;
	GLOBAL	cpu_exception
cpu_exception
	mov		r2, r0
	lsl		r1, #16
	orr		r1, #CPU_INT_EXCEPTION				; r1 = We are handling an exception
	b		unknown

; ------------------- IRQStart code -----------------------------------
; Start actually handling an IRQ. What we need to do here
; On input
;	r0 = INT number<<2 to call
;	r2 = saved CPU flags
;
	GLOBAL	irq_prot
irq_prot
	;-------
	; Adjust the interrupt number, and setup int/exception type.
	; Need to swap r0 and r2
	;-------
	mov		r1, r2
	mov		r2, r0, lsr #2
	mov		r0, r1
	mov		r1, #0								; Mark that this is a hardware IRQ
	b		exception_cont_r0r1r2


; ------------------- CD = INT imm8 -----------------------------------
;
; On input
;	r0 = saved flags
;	r2 = interrupt number
;
	GLOBAL	cpu_int_prot_r0r2
cpu_int_prot_r0r2
	;-------
	;	/* Protected Mode Interrupt */
	;	if ((reg_flags & FLAG_VM) && (type&CPU_INT_SOFTWARE) && !(type&CPU_INT_NOIOPLCHECK)) {
	;		if ((reg_flags & FLAG_IOPL)!=FLAG_IOPL) {
	;			CPU_Exception(EXCEPTION_GP,0);
	;			return;
	;		}
	;	} 
	; CPU_INT_NOIOPLCHECK only used by 0xCC (INT 3, breakpoint) and 0xF1 (INT 1, ICEBP)
	;-------
	ldr		r3, [sp, #SP_FLAGS]					; r3 = Current x86 flags
	mov		r1, #CPU_INT_SOFTWARE				; r1 = We are handling a software interrupt
	tst		r3, #FLAG_VM						; Are we in VM mode?
	bne		op_cd_VM_IOPL						; Yes, go check the IOPL level
	;-------
	; Protected mode exception / IRQ continues here
	; registers on input
	;	r0 = saved flags
	;	r1 = interrupt/exception type
	;	r2 = interrupt number
	;-------
	GLOBAL	exception_cont_r0r1r2
exception_cont_r0r1r2
	str		r0, [sp, #SP_STR_SEG]				; Save the flags temporarily to stack
	str		r1, [sp, #SP_FREE2]					; Save the interrupt/exception type
	;------
	;		Descriptor gate;
	;		if (!cpu.idt.GetDescriptor(num<<3,gate)) {
	;			; zone66
	;			CPU_Exception(EXCEPTION_GP,num*8+2+(type&CPU_INT_SOFTWARE)?0:1);
	;			return;
	;		}
	;------
	ldr		r0, =cpu_idt_base
	ldr		r3, [r0, #4]						; r1 = cpu_idt_limit
	ldr		r0, [r0, #8]						; r0 = cpu_idt_phys
	cmp		r3, r2, lsl #3
	blt		cpu_int_GP_fault
	add		r3, r0, r2, lsl #3					; r3 = physical address of this selector (gate) in the IDT
	;------
	;		if ((type&CPU_INT_SOFTWARE) && (gate.DPL()<cpu.cpl)) {
	;			; zone66, win3.x e
	;			CPU_Exception(EXCEPTION_GP,num*8+2);
	;			return;
	;		}
	;------
	ldrb	r0, [r3, #5]						; r0 = type byte of the gate
	tst		r1, #CPU_INT_SOFTWARE
	beq		%f1
	ldrb	r1, [sp, #SP_CPU_CPL]
	and		r2, r0, #(3<<5)
	cmp		r1, r2, lsr #5
	bgt		cpu_int_GP_fault
1	tst		r0, #0x80
	beq		cpu_int_GP_fault					; Common "segment not present" handling for all gate types here
	;------
	;		switch (gate.Type()) {
	;------
	and		r0, #0x1F
	;------
	;		case DESC_TASK_GATE:									0x05
	;			CPU_CHECK_COND(!gate.saved.seg.p,
	;				"INT:Gate segment not present",
	;				EXCEPTION_NP,num*8+2+(type&CPU_INT_SOFTWARE)?0:1)
	;------
	cmp		r0, #0x05
	beq		cpu_int_task_gate
	;------
	;		default
	;			E_Exit("Illegal descriptor type %X for int %X",gate.Type(),num);
	;------
	and		r0, #0x16
	cmp		r0, #0x06
	bne		unknown
	;------
	;		case DESC_286_INT_GATE:		case DESC_386_INT_GATE:		0x06, 0x0E = 0b00110, 0b01110
	;		case DESC_286_TRAP_GATE:	case DESC_386_TRAP_GATE:	0x07, 0x0F = 0b00111, 0b01111
	;			{
	;				CPU_CHECK_COND(!gate.saved.seg.p,
	;					"INT:Gate segment not present",
	;					EXCEPTION_NP,num*8+2+(type&CPU_INT_SOFTWARE)?0:1)
	;				Descriptor cs_desc;
	;				Bitu gate_sel=gate.GetSelector();
	;				Bitu gate_off=gate.GetOffset();
	;------
	ldrb	r2, [r3, #2]
	ldrb	r0, [r3, #3]
	str		r3, [sp, #SP_FREE3]					; Save the gate to stack temporarily
	orr		r2, r0, lsl #8						; r2 = gate_sel
	;------
	;				CPU_CHECK_COND(!cpu.gdt.GetDescriptor(gate_sel,cs_desc),
	;					"INT:Gate with CS beyond limit",
	;					EXCEPTION_GP,(gate_sel & 0xfffc)+(type&CPU_INT_SOFTWARE)?0:1)
	;------
	GetDescriptor_r3_from_r2_destroy_r0 cpu_int_GP_fault
	ldrb	r0, [r3, #5]						; Get the type byte of the cs_desc
	tst		r0, #0x80
	beq		cpu_int_GP_fault					; Common "segment not present" handling for all gate types here
	;------
	;				switch (cs_desc.Type()) {
	;				default
	;					E_Exit("INT:Gate Selector points to illegal descriptor with type %x",cs_desc.Type());
	;------
	tst		r0, #DESC_DATA_OR_CODE_BIT
	tstne	r0, #DESC_CODE_BIT
	beq		cpu_int_GP_fault					; Both bits need to be set
	;------
	;				case DESC_CODE_N_NC_A:	case DESC_CODE_N_NC_NA:			0x18, 0x19, 0x1a, 0x1b
	;				case DESC_CODE_R_NC_A:	case DESC_CODE_R_NC_NA
	;------
	tst		r0, #DESC_CODE_CONFORMING_BIT
	bne		int_conforming_code
	;------
	;				Bitu cs_dpl=cs_desc.DPL();
	;				CPU_CHECK_COND(cs_dpl>cpu.cpl,
	;					"Interrupt to higher privilege",
	;					EXCEPTION_GP,(gate_sel & 0xfffc)+(type&CPU_INT_SOFTWARE)?0:1)
	;------
	and		r0, #(3<<5)
	cmp		r1, r0, lsr #5
	blt		cpu_int_GP_fault
	beq		int_conforming_cont					; If the priority levels are equal, use the conforming code handling.
	;------
	;					if (cs_dpl<cpu.cpl) {
	;------
	b	unknown	; TODO!

	;------
	;					} 
	;				case DESC_CODE_N_C_A:	case DESC_CODE_N_C_NA:			0x1c, 0x1d, 0x1e, 0x1f
	;				case DESC_CODE_R_C_A:	case DESC_CODE_R_C_NA
	;------
int_conforming_code
	;------
	;				Bitu cs_dpl=cs_desc.DPL();
	;				CPU_CHECK_COND(cs_dpl>cpu.cpl,
	;					"Interrupt to higher privilege",
	;					EXCEPTION_GP,(gate_sel & 0xfffc)+(type&CPU_INT_SOFTWARE)?0:1)
	;------
	and		r0, #(3<<5)
	cmp		r1, r0, lsr #5
	blt		cpu_int_GP_fault
int_conforming_cont
	;------
	;					/* Prepare stack for gate to same priviledge */
	;					CPU_CHECK_COND(!cs_desc.saved.seg.p,
	;							"INT:Same level:CS segment not present",
	;						EXCEPTION_NP,(gate_sel & 0xfffc)+(type&CPU_INT_SOFTWARE)?0:1)
	;------
	; Handled above!
	;------
	;					if ((reg_flags & FLAG_VM) && (cs_dpl<cpu.cpl))
	;						E_Exit("V86 interrupt doesn't change to pl0");	; or #GP(cs_sel)
	;------
	;======
	; Commit Point
	;======
do_interrupt
	;------
	;					if (gate.Type() & 0x8) {	/* 32-bit Gate */
	;						CPU_Push32(reg_flags);
	;						CPU_Push32(SegValue(cs));
	;						CPU_Push32(oldeip);
	;						if (type & CPU_INT_HAS_ERROR) CPU_Push32(cpu.exception.error);
	;					} else {					/* 16-bit gate */
	;						CPU_Push16(reg_flags & 0xffff);
	;						CPU_Push16(SegValue(cs));
	;						CPU_Push16(oldeip);
	;						if (type & CPU_INT_HAS_ERROR) CPU_Push16(cpu.exception.error);
	;					}
	;					break;		
	;------
	ldr		r1, [sp, #SP_FREE3]					; Get the gate from stack
	ldr		r0, [sp, #SP_STR_SEG]				; Get the flags from stack
	ldrb	r1, [r1, #5]						; r1 = type byte of the gate
	tst		r1, #DESC_GATE_32BIT
	bne		%f32
	;------
	; 16-bit gate
	;------
	msr		cpsr_f, r0							; Restore the CPU flags
	push_flags_16 r0, r1, lr
	ldr		r1, [sp, #SP_PHYS_CS]
	ldr		r0, [sp, #SP_CS_VALUE]
	sub		r12, r1								; r12 = logical IP
	push_hword r0, r1, lr
	push_hword r12, r1, lr
	;------
	; TODO! Push cpu_error!
	;------
	b		%f16									; Continue with common code
	;------
	; 32-bit gate
	;------
32	msr		cpsr_f, r0							; Restore the CPU flags
	ldr		r0,[sp, #SP_FLAGS]
	join_ARM_to_X86_flags r0
	push_dword r0, r1, lr
	ldr		r1, [sp, #SP_PHYS_CS]
	ldr		r0, [sp, #SP_CS_VALUE]
	sub		r12, r1								; r12 = logical IP
	push_dword r0, r1, lr
	push_dword r12, r1, lr
	;------
	; TODO! Push cpu_error!
	;------
	;------
	;				Segs.val[cs]=(gate_sel&0xfffc) | cpu.cpl;
	;------
16	ldrb	r0, [sp, #SP_CPU_CPL]
	bic		r2, #3
	orr		r2, r0
	str		r2, [sp, #SP_CS_VALUE]
	;------
	;				cpu.code.big=cs_desc.Big()>0;
	;------
	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	ldrb	r1, [sp, #SP_CPU_BIG]
	and		r0, #0x40
	cmp		r0, r1								; Did the cpu_code_big value change?
	strb	r0, [sp, #SP_CPU_BIG]
	beq		%f2									; No change, so skip changing the opcode table
	fix_opcode_table_destroy_flags				; cpu_code_big changed, so change the main opcode table in stack.
	;------
	;				Segs.phys[cs]=cs_desc.GetBase();
	;------
2	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r2, r0, r2, lsl #16
	orr		r2, r3, lsl #24						; r2 = new logical CS base address
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	;------
	;				reg_eip=gate_off;
	;------
	ldr		r3, [sp, #SP_FREE3]					; Get the gate from stack
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	ldrb	r12, [r3]
	ldrb	r0, [r3, #1]
	ldrb	r1, [r3, #6]
	orr		r12, r0, lsl #8
	ldrb	r0, [r3, #7]
	orr		r12, r1, lsl #16
	orr		r12, r0, lsl #24
	add		r12, r2								; r12 = new physical CS:IP
	;------
	;				if (!(gate.Type()&1)) {
	;					SETFLAGBIT(IF,false);
	;				}
	;				SETFLAGBIT(TF,false);
	;				SETFLAGBIT(NT,false);
	;------
	ldrb	r0, [r3, #5]						; r0 = type byte of the gate
	ldr		r2,[sp, #SP_FLAGS]					; Get the EXTRAFLAGS value
	tst		r0, #1
	biceq	r2, #FLAG_IF
	bic		r2, #(FLAG_TF|FLAG_NT)
	;------
	; If this was a hardware IRQ, clear the IRQFlag/semaphore, OK for further interrupts.
	;------
	ldr		r1, [sp, #SP_FREE2]					; Get the interrupt/exception type
	str		r2, [sp, #SP_FLAGS]					; Save the EXTRAFLAGS value
	tst		r1, #(CPU_INT_SOFTWARE|CPU_INT_EXCEPTION)
	ldreq	r0, [sp, #SP_IRQMAXC]
	streq	r0, [sp, #SP_IRQFLAG]
	;------
	;				LOG(LOG_CPU,LOG_NORMAL)("INT:Gate to %X:%X big %d %s",gate_sel,gate_off,cs_desc.Big(),gate.Type() & 0x8 ? "386" : "286");
	;				return;
	;			}
	;------
	ldr		r0, [sp, #SP_STR_SEG]				; Get the flags from stack
	b		restore_flags_from_r0


op_cd_VM_IOPL
	and		r3, #FLAG_IOPL
	cmp		r3, #FLAG_IOPL
	bne		cpu_jmp_GP_fault
	b		exception_cont_r0r1r2
	

	LTORG

; ------------------- CF = IRET ---------------------------------------
; On input
;	r0 = saved flags and USE32 flag (0 = USE16, 0x10000 = USE32)
;
	GLOBAL	cpu_iret_prot_r0
cpu_iret_prot_r0
	str		r0, [sp, #SP_STR_SEG]				; Save the flags and USE32 flag temporarily to stack
	add		r1, sp, #SP_R4SAVE
	stmia	r1, {r4, r5, r6}
	ldr		r1, [sp, #SP_FLAGS]					; Get the EXTRAFLAGS value
n_eip		RN		4
n_cs_sel	RN		5	
n_flags		RN		6
	;------
	; void CPU_IRET(bool use32,Bitu oldeip) {
	;	if (!cpu.pmode) {
	;	} else {							/* Protected mode IRET */
	;		if (reg_flags & FLAG_VM) {
	;-------
	tst		r1, #FLAG_VM
	bne		iret_VM
	;-------
	;		}
	;		/* Check if this is task IRET */	
	;		if (GETFLAG(NT)) {
	;-------
	tst		r1, #FLAG_NT
	bne		iret_task
	;-------
	;		}
	;		Bitu n_cs_sel,n_eip,n_flags;
	;		Bit32u tempesp;
	;-------
	ldr		r3, [sp, #SP_SP_MASK]
	ldr		r4, [sp, #SP_PHYS_SS]
	and		r3, esp
	add		r3, r4							; Now r3 = physical stack address
	;-------
	;		if (use32) {
	;-------
	tst		r0, #0x10000
	beq		%f16
	;-------
	;			n_eip=mem_readd(SegPhys(ss) + (reg_esp & cpu.stack.mask));
	;			tempesp=(reg_esp&cpu.stack.notmask)|((reg_esp+4)&cpu.stack.mask);
	;			n_cs_sel=mem_readd(SegPhys(ss) + (tempesp & cpu.stack.mask)) & 0xffff;
	;			tempesp=(tempesp&cpu.stack.notmask)|((tempesp+4)&cpu.stack.mask);
	;			n_flags=mem_readd(SegPhys(ss) + (tempesp & cpu.stack.mask));
	;			tempesp=(tempesp&cpu.stack.notmask)|((tempesp+4)&cpu.stack.mask);
	;-------
	ldrb	n_eip, [r3]
	ldrb	r0, [r3, #1]
	ldrb	r1, [r3, #2]
	ldrb	r2, [r3, #3]
	orr		n_eip, r0, lsl #8
	orr		n_eip, r1, lsl #16
	ldrb	n_cs_sel, [r3, #4]
	ldrb	r0, [r3, #5]
	orr		n_eip, r2, lsl #24
	orr		n_cs_sel, r0, lsl #8
	ldrb	n_flags, [r3, #8]
	ldrb	r0, [r3, #9]
	ldrb	r1, [r3, #10]
	ldrb	r2, [r3, #11]
	orr		n_flags, r0, lsl #8
	orr		n_flags, r1, lsl #16
	orr		n_flags, r2, lsl #24
	;-------
	;			if ((n_flags & FLAG_VM) && (cpu.cpl==0)) {
	;-------
	tst		n_flags, #FLAG_VM
	beq		%f32
	ldrb	r0, [sp, #SP_CPU_CPL]
	cmp		r0, #0
	bne		unknown							;	if (n_flags & FLAG_VM) E_Exit("IRET from pmode to v86 with CPL!=0");
	;======
	; IRET into VM mode! (Windows 3.11 at 0028:80006D1A	CF	iret)
	;======
	;				; commit point
	;				reg_esp=tempesp;
	;				SegSet16(cs,n_cs_sel);
	;------
	b unknown	; TODO!	
	;-------
	;		} else { ; use16
	;			n_eip=mem_readw(SegPhys(ss) + (reg_esp & cpu.stack.mask));
	;			tempesp=(reg_esp&cpu.stack.notmask)|((reg_esp+2)&cpu.stack.mask);
	;			n_cs_sel=mem_readw(SegPhys(ss) + (tempesp & cpu.stack.mask));
	;			tempesp=(tempesp&cpu.stack.notmask)|((tempesp+2)&cpu.stack.mask);
	;			n_flags=mem_readw(SegPhys(ss) + (tempesp & cpu.stack.mask));
	;			n_flags|=(reg_flags & 0xffff0000);
	;			tempesp=(tempesp&cpu.stack.notmask)|((tempesp+2)&cpu.stack.mask);
	;-------
16	ldrb	n_eip, [r3]
	ldrb	r0, [r3, #1]
	ldrb	n_cs_sel, [r3, #2]
	ldrb	r2, [r3, #3]
	ldrb	n_flags, [r3, #4]
	ldrb	r3, [r3, #5]
	orr		n_eip, r0, lsl #8
	orr		n_cs_sel, r2, lsl #8
	orr		n_flags, r3, lsl #8
	lsr		r1, #16								; r1 = current reg_flags value
	orr		n_flags, r1, lsl #16				; n_flags|=(reg_flags & 0xffff0000);
	;-------
	; 			; Can not happen, since FLAG_VM comes from the original flags which have already been checked!
	;			if (n_flags & FLAG_VM) E_Exit("VM Flag in 16-bit iret");
	;		}
	;-------
	;------
	;		CPU_CHECK_COND((n_cs_sel & 0xfffc)==0,
	;			"IRET:CS selector zero",
	;			EXCEPTION_GP,0)
	;------
32	bics	r0, n_cs_sel, #3
	beq		iret_GP_fault
	;------
	;		Bitu n_cs_rpl=n_cs_sel & 3;
	;		Descriptor n_cs_desc;
	;		CPU_CHECK_COND(!cpu.gdt.GetDescriptor(n_cs_sel,n_cs_desc),
	;			"IRET:CS selector beyond limits",
	;			EXCEPTION_GP,n_cs_sel & 0xfffc)
	;------
	mov		r2, n_cs_sel
	GetDescriptor_r3_from_r2_destroy_r0 iret_GP_fault
	ldrb	r1, [r3, #5]
	ldrb	r0, [sp, #SP_CPU_CPL]
	and		r2, n_cs_sel, #3					; r2 = n_cs_rpl
	;------
	;		CPU_CHECK_COND(!n_cs_desc.saved.seg.p,
	;			"IRET with nonpresent code segment",
	;			EXCEPTION_NP,n_cs_sel & 0xfffc)
	;------
	tst		r1, #0x80
	beq		iret_NP_fault
	;------
	;		switch (n_cs_desc.Type()) {
	;		case DESC_CODE_N_NC_A:	case DESC_CODE_N_NC_NA
	;		case DESC_CODE_R_NC_A:	case DESC_CODE_R_NC_NA
	;			CPU_CHECK_COND(n_cs_rpl!=n_cs_desc.DPL(),
	;				"IRET:NC:DPL!=RPL",
	;				EXCEPTION_GP,n_cs_sel & 0xfffc)
	;			break;
	;		case DESC_CODE_N_C_A:	case DESC_CODE_N_C_NA
	;		case DESC_CODE_R_C_A:	case DESC_CODE_R_C_NA
	;			CPU_CHECK_COND(n_cs_desc.DPL()>n_cs_rpl,
	;				"IRET:C:DPL>RPL",
	;				EXCEPTION_GP,n_cs_sel & 0xfffc)
	;			break;
	;		default
	;			E_Exit("IRET:Illegal descriptor type %X",n_cs_desc.Type());
	;		}
	;------
	and		r1, #0x1F
	cmp		r1, #0x18							; Segment needs to be a Code segment!
	blt		iret_GP_fault
	;------
	;		CPU_CHECK_COND(n_cs_rpl<cpu.cpl,
	;			"IRET to lower privilege",
	;			EXCEPTION_GP,n_cs_sel & 0xfffc)
	;------
	cmp		r2, r0
	blt		iret_GP_fault
	;------
	;		if (n_cs_rpl==cpu.cpl) {	
	;			/* Return to same level */
	;------
	bgt		iret_outer_level
	;======
	; Commit Point! Registers here
	;	r0			= cpu_cpl
	;	r1			= free
	;	r2			= free (n_cs_rpl)
	;	r3			= pointer to new CS descriptor
	;	r4			= n_eip
	;	r5			= n_cs_sel
	;	r6			= n_flags
	;	r12			= free, needs to be set to new CS:IP
	;	SP_STR_SEG	= flags and USE32 flag
	;	SP_R4SAVE	= original r4 value
	;	SP_R5SAVE	= original r5 value
	;	SP_R6SAVE	= original r6 value
	;======
	;------
	;			; commit point
	;			reg_esp=tempesp;
	;------
	ldr		r1, [sp, #SP_STR_SEG]				; Get the USE32 flag from stack
	tst		r1, #0x10000
	addne	esp, #3*4
	addeq	esp, #3*2
	;------
	;			Bitu mask=cpu.cpl ? (FMASK_NORMAL | FLAG_NT) : FMASK_ALL;
	;			if (GETFLAG_IOPL<cpu.cpl) mask &= (~FLAG_IF);
	;			CPU_SetFlags(n_flags,mask);
	;------
	cmp		r0, #0
	ldr		r2, [sp, #SP_FLAGS]					; Get the EXTRAFLAGS value
	ldreq	r1, =FMASK_ALL
	ldrne	r1, =(FMASK_NORMAL | FLAG_NT)
	and		r12, r2, #FLAG_IOPL
	cmp		r12, r0, lsl #12
	biclt	r1, #FLAG_IF
	and		n_flags, r1
	bic		r2, r1
	orr		r2, n_flags
	str		r2, [sp, #SP_FLAGS]					; Set the EXTRAFLAGS value
	;------
	;			Segs.val[cs]=n_cs_sel;
	;			reg_eip=n_eip;
	;------
	str		n_cs_sel, [sp, #SP_CS_VALUE]
	mov		r12, n_eip
	;------
	;			cpu.code.big=n_cs_desc.Big()>0;
	;------
	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	ldrb	r1, [sp, #SP_CPU_BIG]
	and		r0, #0x40
	cmp		r0, r1								; Did the cpu_code_big value change?
	strb	r0, [sp, #SP_CPU_BIG]
	beq		%f2									; No change, so skip changing the opcode table
	fix_opcode_table_destroy_flags				; cpu_code_big changed, so change the main opcode table in stack.
	;------
	;			Segs.phys[cs]=n_cs_desc.GetBase();
	;------
2	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r2, r0, r2, lsl #16
	orr		r2, r3, lsl #24						; r2 = new logical CS base address
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	add		r12, r2								; r12 = new physical CS:IP
	;------
	;			return;
	;		}
	;------
	add		r4, sp, #SP_R4SAVE
	ldr		r0, [sp, #SP_FLAGS]					; Get the new flags
	ldmia	r4, {r4, r5, r6}
	popped_flags_to_ARM r0						; Set the real CPU flags
	b		restore_flags_from_r0
	
iret_outer_level


iret_VM
iret_task
iret_GP_fault
iret_NP_fault
	b		unknown
	

; ------------------- E4 = IN AL,imm8 ---------------------------------
;
	GLOBAL	op_e4_prot_r0
op_e4_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e4_real_r0				; Continue with the common IN AL, imm8 handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e4_real_r0
	
; ------------------- E5 = IN AX,imm8 --------------------------------
;
	GLOBAL	op_e5_prot_r0
op_e5_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e5_real_r0				; Continue with the common IN AX,imm8 handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e5_real_r0
	
	GLOBAL	op_e5_USE32_prot_r0
op_e5_USE32_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e5_USE32_real_r0			; Continue with the common IN EAX,imm8 handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e5_USE32_real_r0

; ------------------- E6 = OUT imm8,AL --------------------------------
;
	GLOBAL	op_e6_prot_r0
op_e6_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e6_real_r0				; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e6_real_r0
	
; ------------------- E7 = OUT imm8,AX --------------------------------
;
	GLOBAL	op_e7_prot_r0
op_e7_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e7_real_r0				; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e7_real_r0
	
	GLOBAL	op_e7_USE32_prot_r0
op_e7_USE32_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_e7_USE32_real_r0			; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_e7_USE32_real_r0
	
; ------------------- EA = JMP FAR PROT -------------------------------
; 02EE:1389 EA8E132000	jmp 0020:138E	(TREKMO)
; 05BF:09B6 EABB091001	jmp	0110:09BB	(Win 3.0)
;
; On input
;	r0 = saved flags
;	r1 = new logical IP (offset)
;	r2 = new CS selector (segment)
;
	GLOBAL	cpu_jmp_prot_r0r1r2
cpu_jmp_prot_r0r1r2
	;------
	; void CPU_JMP(bool use32,Bitu selector,Bitu offset,Bitu oldeip) {
	;------
	;------
	; First check if we are in Virtual_86 mode, and use the real mode handler if we are.
	; if (!cpu.pmode || (reg_flags & FLAG_VM)) {
	;	if (!use32) {
	;		reg_eip=offset&0xffff;
	;	} else {
	;		reg_eip=offset;
	;	}
	;	SegSet16(cs,selector);
	;	cpu.code.big=false;
	;	return;
	; }
	;------
	ldr		r3, [sp, #SP_FLAGS]				; r3 = Current x86 flags
	str		r0, [sp, #SP_STR_SEG]			; Save the flags temporarily to stack
	tst		r3, #FLAG_VM					; Are we in VM mode?
	bne		cpu_jmp_real_r0r1r2				; Yes, so use the real mode handler. TODO! Set cpu.code.big=false;
	;-----
	;	CPU_CHECK_COND((selector & 0xfffc)==0,
	;		"JMP:CS selector zero",
	;		EXCEPTION_GP,0)
	;-----
	;-----
	;	Descriptor desc;
	;	CPU_CHECK_COND(!cpu.gdt.GetDescriptor(selector,desc),
	;		"JMP:CS beyond limits",
	;		EXCEPTION_GP,selector & 0xfffc)
	;-----
	GetDescriptor_r3_from_r2_destroy_r0 cpu_jmp_GP_fault
	ldrb	r0, [r3, #5]						; Get the descriptor Access byte
	;------
	;	if (!desc.saved.seg.p) {
	;		; win
	;		CPU_Exception(EXCEPTION_NP,selector & 0xfffc);
	;		return;
	;	}
	;------
	tst		r0, #0x80
	beq		cpu_jmp_NP_fault
	;------
	;	Bitu rpl=selector & 3;
	;	switch (desc.Type()) {
	;	case DESC_CODE_N_NC_A:		case DESC_CODE_N_NC_NA
	;	case DESC_CODE_R_NC_A:		case DESC_CODE_R_NC_NA
	;		CPU_CHECK_COND(rpl>cpu.cpl,
	;			"JMP:NC:RPL>CPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		CPU_CHECK_COND(cpu.cpl!=desc.DPL(),
	;			"JMP:NC:RPL != DPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:Code:NC to %X:%X big %d",selector,offset,desc.Big());
	;		goto CODE_jmp;
	;	case DESC_386_TSS_A
	;		CPU_CHECK_COND(desc.DPL()<cpu.cpl,
	;			"JMP:TSS:dpl<cpl",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		CPU_CHECK_COND(desc.DPL()<rpl,
	;			"JMP:TSS:dpl<rpl",
	;			EXCEPTION_GP,selector & 0xfffc)
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:TSS to %X",selector);
	;		CPU_SwitchTask(selector,TSwitch_JMP,oldeip);
	;		break;
	;	case DESC_CODE_N_C_A:		case DESC_CODE_N_C_NA
	;	case DESC_CODE_R_C_A:		case DESC_CODE_R_C_NA
	;		LOG(LOG_CPU,LOG_NORMAL)("JMP:Code:C to %X:%X big %d",selector,offset,desc.Big());
	;		CPU_CHECK_COND(cpu.cpl<desc.DPL(),
	;			"JMP:C:CPL < DPL",
	;			EXCEPTION_GP,selector & 0xfffc)
	;------
	and		r0, #0x1F
	cmp		r0, #0x18							; Segment needs to be a Code segment! TODO! CPU_SwitchTask() handling!
	blt		cpu_jmp_GP_fault
	;------
	;	/* Normal jump to another selector:offset */
	;	Segs.val[cs]=(selector & 0xfffc) | cpu.cpl;
	;------
	ldrb	r0, [sp, #SP_CPU_CPL]
	bic		r2, #3
	orr		r2, r0
	str		r2, [sp, #SP_CS_VALUE]
	;------
	;	reg_eip=offset;
	;	cpu.code.big=desc.Big()>0;
	;------
	ldrb	r0, [r3, #6]						; Get the descriptor Flags byte
	mov		r12, r1
	ldrb	r1, [sp, #SP_CPU_BIG]
	and		r0, #0x40
	cmp		r0, r1								; Did the cpu_code_big value change?
	strb	r0, [sp, #SP_CPU_BIG]
	beq		%f2									; No change, so skip changing the opcode table
	fix_opcode_table_destroy_flags				; cpu_code_big changed, so change the main opcode table in stack.
	;------
	;	Segs.phys[cs]=desc.GetBase();
	;	return;
	;------
2	ldrb	r0, [r3, #2]
	ldrb	r1, [r3, #3]
	ldrb	r2, [r3, #4]
	ldrb	r3, [r3, #7]
	orr		r0, r1, lsl #8
	orr		r2, r0, r2, lsl #16
	orr		r2, r3, lsl #24						; r2 = new logical CS base address
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2						; Calculate the proper physical address
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	add		r12, r2								; r12 = new physical CS:IP
	ldr		r0, [sp, #SP_STR_SEG]				; Restore the flags from stack
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	EXTERN	cpu_jmp_real_r0r1r2
	
cpu_int_GP_fault
cpu_int_task_gate

	;------
	; Fault! Not a proper code segment selector!
	;------	
cpu_jmp_SS_fault
cpu_jmp_GP_fault
cpu_jmp_NP_fault
	GLOBAL	jmp_GP_fault
jmp_GP_fault
	b		unknown

	LTORG
	
; ------------------- EC = IN AL,DX -----------------------------------
	GLOBAL	op_ec_prot_r0
op_ec_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ec_real_r0				; Continue with the common IN AL,DX handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ec_real_r0
	
; ------------------- ED = IN AX,DX -----------------------------------
	GLOBAL	op_ed_prot_r0
op_ed_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ed_real_r0				; Continue with the common IN AX,DX handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ed_real_r0
	
	GLOBAL	op_ed_USE32_prot_r0
op_ed_USE32_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ed_USE32_real_r0			; Continue with the common IN EAX,DX handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ed_USE32_real_r0
	
; ------------------- EE = OUT DX,AL ----------------------------------
	GLOBAL	op_ee_prot_r0
op_ee_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ee_real_r0				; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ee_real_r0
	
; ------------------- EF = OUT DX,AX ----------------------------------
	GLOBAL	op_ef_prot_r0
op_ef_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ef_real_r0				; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ef_real_r0
	
	GLOBAL	op_ef_USE32_prot_r0
op_ef_USE32_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldr		r2, [sp, #SP_FLAGS]			; r2 = SP_FLAGSS
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r2, #FLAG_IOPL				; r2 = FLAGS_IOPL
	cmp		r1, r2, lsr #12
	ble		op_ef_USE32_real_r0			; Continue with the common OUT imm8,AL handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_ef_USE32_real_r0
	
; ------------------- FA = CLI ----------------------------------------
	GLOBAL	op_fa_prot_r0
op_fa_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, r2, #FLAG_IOPL			; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_fa_real_r0				; Continue with the common CLI handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_fa_real_r0
	
; ------------------- FB = STI ----------------------------------------
	GLOBAL	op_fb_prot_r0
op_fb_prot_r0
	;-------
	; if (cpu.pmode && FLAGS_IOPL < cpu.cpl) EXCEPTION_GP;
	;-------
	ldrb	r1, [sp, #SP_CPU_CPL]		; r1 = cpu.cpl
	and		r3, r2, #FLAG_IOPL			; r3 = FLAGS_IOPL
	cmp		r1, r3, lsr #12
	ble		op_fb_real_r0				; Continue with the common STI handling
	b		jmp_GP_fault				; Else a GP fault!

	EXTERN	op_fb_real_r0

	AREA cpu_prot_data, DATA, READWRITE

	ALIGN	4

	GLOBAL	cpu_cr0
cpu_cr0
	DCD	0
	
	GLOBAL	cpu_cr2
cpu_cr2
	DCD	0
	
	GLOBAL	cpu_cr3
cpu_cr3
	DCD	0

	GLOBAL	cpu_tss_selector
cpu_tss_selector							; offset 0
	DCD	0	
	GLOBAL	cpu_tss_base	
cpu_tss_base								; offset 4
	DCD	0	
	GLOBAL	cpu_tss_limit	
cpu_tss_limit								; offset 8
	DCD	0	
	GLOBAL	cpu_tss_valid
cpu_tss_valid								; offset 12
	DCD	0	
	GLOBAL	cpu_tss_phys
cpu_tss_phys								; offset 16
	DCD	0
	GLOBAL	cpu_tss_is386
cpu_tss_is386								; offset 20
	DCD	0

	GLOBAL	cpu_ldt_value
cpu_ldt_value
	DCD	0
	GLOBAL	cpu_gdt_base
cpu_gdt_base
	DCD	0
	GLOBAL	cpu_ldt_base
cpu_ldt_base
	DCD	0
	GLOBAL	cpu_gdt_limit
cpu_gdt_limit
	DCD	0
	GLOBAL	cpu_ldt_limit
cpu_ldt_limit
	DCD	0
	GLOBAL	cpu_gdt_phys
cpu_gdt_phys
	DCD	0
	GLOBAL	cpu_ldt_phys
cpu_ldt_phys
	DCD	0

	GLOBAL	cpu_idt_base
cpu_idt_base
	DCD	0
	GLOBAL	cpu_idt_limit
cpu_idt_limit
	DCD	0
	GLOBAL	cpu_idt_phys
cpu_idt_phys
	DCD	0

	GLOBAL	cpu_cpl
cpu_cpl
	DCD	0

	GLOBAL	cpu_big
cpu_big
	DCD	0

	GLOBAL	stack_mask
stack_mask
	DCD	0x0000FFFF

cpu_exception_error
	DCD	0

	;------
	; DR7 - Debug control
	; http:;en.wikipedia.org/wiki/Debug_register
	; The low-order eight bits of DR7 (0,2,4,6 and 1,3,5,7) selectively enable the four address breakpoint conditions.
	; There are two levels of enabling: the local (0,2,4,6) and global (1,3,5,7) levels. The local enable bits are
	; automatically reset by the processor at every task switch to avoid unwanted breakpoint conditions in the new task.
	; The global enable bits are not reset by a task switch; therefore, they can be used for conditions that are global
	; to all tasks.
	;
	; Bits 16-17 (DR0), 20-21 (DR1), 24-25 (DR2), 28-29 (DR3), define when breakpoints trigger. Each breakpoint has a
	; two-bit entry that specifies whether they break on execution (00b), data write (01b), data read or write (11b).
	; 10b is defined to mean break on IO read or write but no hardware supports it. Bits 18-19 (DR0), 22-23 (DR1),
	; 26-27 (DR2), 30-31 (DR3), define how large area of memory is watched by breakpoints. Again each breakpoint has
	; a two-bit entry that specifies whether they watch one (00b), two (01b), eight (10b) or four (11b) bytes. [1]
	;
	GLOBAL	cpu_DRx
cpu_DRx
	DCD	0
	DCD	0
	DCD	0
	DCD	0
	DCD	0
	DCD	0
	DCD	0
	DCD	0x400

	END
