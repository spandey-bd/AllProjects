;=============================================================================
; cpu.s
;
; This file is the main emulator core source module. It has one public function
; "run(bool run)" which begins executing the x86 program from current CS:IP
; address. If the parameter is true, the function returns only if there was
; an error (unsupported opcode) when running the emulation. If the parameter
; is false, the core executes only one opcode and then returns immediately.
; This is mainly used when tracing the x86 program in the built-in debugger.
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
;
; At the start are various debugging-related defines that can be used when
; hunting for bugs or compatibility problems in the core.
;
;	DISALLOW_ZERO_CODE	If 1, two adjacent "add [bx+si],al" opcodes (0x00, 0x00)
;						are not allowed, as that usually means the core is
;						executing empty data instead of code.
;	DEBUGTRAP			If 1, call external "bool DebugTrap()" C function before
;						executing any opcode. This causes a major performance
;						hit, but the C function can be used for all kinds of
;						debugging purposes. If the C function returns false,
;						the core quits.
;	PROFILER			If 1, uses Nintendo DS -specific timers to calculate the
;						number of ARM cycles each opcode takes, and calls external
;						"Profiler(int opcode, int cycles)" C function to store
;						the cycle data (to memory, file, etc).
;	DEBUGMEMWATCH		If 1, before executing each opcode a code is run that
;						compares the given memory location contents with the
;						previous contents. If they differ, the core quits.
;						The actual checking code can be changed as needed.
;	DEBUGREGWATCH		If 1, before executing each opcode a code is run that
;						checks for a certain value in a certain ARM register.
;						The actual checking code can be changed as needed.
;	CSIPTRACE			If 1, the last 32 physical CS:EIP addresses are stored
;						into a ring buffer "eiptrace". This buffer can be used
;						by the calling code to determine the address where the
;						core encountered an unsupported opcode or crashed.
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

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc
	INCLUDE ../include/gen_16.inc

	EXTERN	BIOS_F000

	EXTERN	cpu_cr0
	EXTERN	cpu_cpl
	EXTERN	cpu_big
	EXTERN	stack_mask
	EXTERN	opcodetable_32_32
	EXTERN	XMS_Total
	EXTERN	XMS_Start_Aligned
	
	EXTERN	BreakSeg
	EXTERN	BreakOffs
	EXTERN	BreakValue
	EXTERN	OrigUSE16RHandler
	
	EXTERN	exception_cont_r0r1r2
	EXTERN	init_prot_segments
	EXTERN	INTHandler

	EXTERN	op_f5								; In pic.asm
	EXTERN 	complement_carry					; In pic.asm
	EXTERN 	restore_flags_from_r0				; In pic.asm
	EXTERN 	loop								; In pic.asm
	EXTERN	test_irq_time						; In pic.asm
	
; ===============================================================

DISALLOW_ZERO_CODE	EQU		0
DEBUGTRAP			EQU		0
PROFILER			EQU		0
DEBUGMEMWATCH		EQU		0
DEBUGREGWATCH		EQU		0
CSIPTRACE			EQU		0
SAVE_HACK			EQU		0
S2TEST				EQU		0
FULLTRACE			EQU		0

; ===============================================================
; Register save, 8 registers AX, CX, DX, BX, SP, BP, SI, DI, PC, Flags, ES, CS, SS, DS, FS, GS, CurF3Tbl, ScreenSEG, VRAMBase
;							 eax, ecx, edx, ebx, esp, ebp, esi, edi,r12,  cpsr
;
	AREA	cpu_data, DATA, READWRITE
	ALIGN	4

	GLOBAL registers
registers
	SPACE	4*8
reg_ip
	SPACE	4
	GLOBAL	reg_flags
reg_flags
	SPACE	4
	GLOBAL	reg_es
reg_es
	SPACE	4
	GLOBAL	reg_cs
reg_cs
	SPACE	4
	GLOBAL	reg_ss
reg_ss
	SPACE	4
	GLOBAL	reg_ds
reg_ds
	SPACE	4
	GLOBAL	reg_fs
reg_fs
	SPACE	4
	GLOBAL	reg_gs
reg_gs
	SPACE	4

	SPACE	4*4

; ===============================================================
; run( bool r0 )
; Input: r0 == run/trace flag, if == 0, trace a single opcode and then return.
; Output: Nothing
;
; Register usage
; 	r0  = On input: Are we Running instead of Tracing, in use: Next opcode / temp register
;	r1  = Jump address / temp register
;	r2  = Segment Override (physical segment start address for memory operations) / temp register
;	r3  = Memory address size mask (0x0000FFFF/0xFFFFFFFF) / temp register
;	eax  = EAX
;	ecx  = ECX
;	edx  = EDX
;	ebx  = EBX
;	esp  = ESP
;	ebp  = EBP
;	esi = ESI
;	edi = EDI
;	r12 = CS:IP (physical address)
;	sp = stack pointer (internal)
;	lr  = SS:0000 (physical address of current stack segment)
;	r15 = program counter (internal)
;

	AREA cpu_code, CODE, READONLY
	ALIGN	4
	
	MACRO
	handle_profiling $cnt			; No content if profiling is not defined on!
		mrc		p15, 0, r0, c15, c12, 1	; r0 = cycle count value
		push	{r2-r12, lr}			; Push used registers
		mrs		ecx,cpsr					; Put flags to ecx
		mov		r1, r12					; r1 = pointer to next instruction to execute
		bl		Profiler				; Call the external profiler function
		msr		cpsr_f,ecx				; Restore flags from ecx
		pop		{r2-r12, lr}			; Pop used registers
		mov		r0, #(1|4)				; Enable all counters, clear cycle counter value
		mcr		p15, 0, r0, c15, c12, 0
	MEND

	;-------
	; x86 emulator enry point
	;-------
	GLOBAL	run_core
run_core
	push	{r4-r12, lr}						; Push used registers
	;-------
	; Clear the BreakReason pointer
	;-------
	ldr		r4,=BreakReason
	mov		r5, #0
	str		r5, [r4]
	IF EXTERNAL_TLB = 1
	push	{r1}								; Save the address of the external TLB table to SP_TLBTABLE
	ELSE
	;-------
	; Push the XMS memory addresses to stack.
	; The address is always ((u32)XMS_Start_Aligned - 0x100000)>>4.
	; We are to push (XMS_Total/16)-4 words.
	;-------
	ldr		r2, =XMS_Start_Aligned
	ldr		r3, =XMS_Total
	ldr		r2, [r2]							; r2 = (u32)XMS_Start_Aligned
	ldr		r3, [r3]
	sub		r2, #0x100000						; r2 = (u32)XMS_Start_Aligned - 0x100000
	lsr		r2, #4								; r2 = ((u32)XMS_Start_Aligned - 0x100000)>>4
	lsr		r3, #4
	sub		r3, #4
1	push	{r2}
	subs	r3, #1
	bne		%b1
	;-------
	; Copy the EMSPageStatic table to stack (to keep it in TCM)
	;-------
	mov		r1, #(64/8)
	ldr		r2,=EMSPageStatic
	add		r2,#(68*4)							; r0 points to the end of the EMSPage table
1	ldmdb	r2!,{r5-r12}						; 8 registers = 8 words at a time
	push	{r5-r12}
	subs	r1, #1
	bne		%b1
	;-------
	; Push the remaining 4 words (for the wrap-around segment).
	;-------
	ldmdb	r2!,{r5-r8}							; 4 registers = 4 words
	push	{r5-r8}
	ldr		r1,=EMSPageTable
	str		sp, [r1]							; Fix the EMSPageTable pointer, we use the table in stack
	ENDIF
	;-------
	; Save the (first) input parameter to ebp
	;-------
	mov		ebp, r0								; ebp = input parameter: 0 = not running, 1 = running
	;-------
	; Store 16-bit mask and protected mode -related stack values
	;-------
	sub		sp, #(4*16)							; Make room for the temp variables (SP_FREE1..SP_R11SAVE) on stack
	ldr		r3, =cpu_cr0
	ldr		r5, =cpu_cpl
	ldr		r2, =cpu_big
	ldrb	r3, [r3]							; r3 = lowest byte of cpu_cr0 value (lowest bits tells whether we are in prot mode)
	ldrb	r5, [r5]
	ldrb	r2, [r2]
	ldr		r4, =stack_mask
	orr		r3, r2, lsl #16						; r3 = third byte tells cpu_big value
	orr		r3, r5, lsl #8						; r3 second byte tells cpu_cpl value
	ldr		r4, [r4]							; r4 = stack_mask value, either 0x0000FFFF or 0xFFFFFFFF
	ldr		r2, =0x0000FFFF						; r2 = SP_MASK_16
	push	{r2-r7}								; r5,r6,r7 = temporary variable locations
	;-------
	; Store the logical segment registers to stack
	; SP_ES_BASE, SP_CS_BASE, SP_SS_BASE, SP_DS_BASE, SP_FS_BASE, SP_GS_BASE
	;-------
	ldr		r6,=reg_es 							; Point ecx to ES register save
	ldmia	r6, {r0-r5}							; Get ES, CS, SS, DS, FS, GS
	push	{r0-r5}								; Save them to SP_ES_VALUE, SP_CS_VALUE, SP_SS_VALUE, SP_DS_VALUE, SP_FS_VALUE, SP_GS_VALUE
	mov		r0, r0, lsl #REAL_SEGMENT_SHIFT
	mov		r1, r1, lsl #REAL_SEGMENT_SHIFT
	mov		r2, r2, lsl #REAL_SEGMENT_SHIFT
	mov		r3, r3, lsl #REAL_SEGMENT_SHIFT
	mov		r4, r4, lsl #REAL_SEGMENT_SHIFT
	mov		r5, r5, lsl #REAL_SEGMENT_SHIFT
	push	{r0-r5}								; Save them to SP_ES_BASE, SP_CS_BASE, SP_SS_BASE, SP_DS_BASE, SP_FS_BASE, SP_GS_BASE
	sub		sp, #(6*4)							; Make room in stack for SP_IRQFLAG .. SP_PHYS_CS
	;-------
	; Tell interrupts are now OK if we are indeed running (r0 != 0)
	;-------
	ldr		r2, =reg_flags
	ldr		r1, [r2]
	str		r1, [sp, #(3*4)]					; SP_FLAGS
	;-------
	; Set the IRQFlag
	;-------
	mov		r1, #IRQ_MAXC
	ldr		r2, =cpu_big
	;-------
	; Store the current IRQ flag value to stack
	;-------
	str		r1, [sp]							; SP_IRQFLAG
	str		r1, [sp, #8]						; SP_IRQMAXC
	ldr		r1,=IRQFlagAddr
	cmp		ebp, #0								; Are we running?
	streq	ebp,[r1]								; Not running, set IRQFlagAddr = NULL
	strne	sp,[r1]								; Running, save the address of the SP_IRQFLAG to IRQFlagAddr
	;-------
	; Copy the main opcode table to stack (to keep it in TCM)
	;-------
	ldrb	r2, [r2]							; r2 = cpu_big value
	mov		r1, #(256/8)
	cmp		r2, #0								; Is cpu_big on?
	ldreq	r2,=opcodetable_16_16				; We are in USE16 segment
	ldrne	r2,=opcodetable_32_32				; We are in USE32 segment
	add		r2,#(256*4)							; r2 points to the end of the opcode table
1	ldmdb	r2!,{r5-r12}						; 8 registers = 8 words at a time
	push	{r5-r12}
	subs	r1, #1
	bne		%b1
	;-------
	; If we are in protected mode, go calculate the proper segment base addresses
	;-------
	ldrb	r2, [sp, #SP_CPU_CR0]
	tst		r2, #1								; Are we in protected mode?
	bne		init_prot_segments					; in "cpu_prot.s"
	;=======
	; REAL-MODE startup code, calculate physical CS and SS addresses
	;=======
	ldr		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]				; Store new physical CS into stack
	ldr		r2, [sp, #SP_SS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]				; Store new physical SS into stack
	;-------
	; Setup the break point opcode handler if BreakOffs has a value.
	;-------
	ldr		r4, =BreakOffs
	ldr		r4, [r4]
	cmp		r4, #0
	beq		prot_start_cont
	ldr		r4, =BreakValue
	ldr		r5, =GoToUSE16ROpcode
	ldrb	r4, [r4]
	str		r5, [sp, r4, lsl #2]				; Change the opcode handler to be the break point handler.
	;-------
	; Load emulation registers
	;-------
	GLOBAL	prot_start_cont
prot_start_cont
	ldr		r4,=IRQFlagAddr
	ldr		r1, =|registers|
	mov		r5, #1								; Default return value = 1 = true
	str		r5, [r1, #4*16]						; Save the default return value to registers[16]
	ldr		r0, [r4]							; r0 == 0 if we are not running
	ldmia	r1!,{eax-r12}						; Load emulation registers from global memory
	IF CSIPTRACE = 1
	mov		r2, #0								
	str		r2, [sp, #SP_FREE4]					; Clear the eiptrace table index
	ENDIF	
	cmp		r0, #0								; Are we not running (r0 == 0 means we are tracing)?
	ldr		r0, [r1]							; Get the flags from global memory
	beq		trace_one_opcode					; Jump if we are not running
	popped_flags_to_ARM r0						; Convert the x86 flags to ARM flags in r0
	msr		cpsr_f,r0							; Set the processor flags
	;-------
	; Go start the code
	;-------
	b		loop
trace_one_opcode
	mov		r1, #IRQ_ON
	str		r1, [sp, #SP_IRQFLAG]				; Make the normal opcode handler jump to IRQStart
	popped_flags_to_ARM r0						; Convert the x86 flags to ARM flags in r0
	msr		cpsr_f,r0							; Set the processor flags
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		lr, [sp, #SP_SS_BASE]				; Get the default segment base for BP-relative addressing
	ldr		r2, [sp, #SP_DS_BASE]				; r2 = logical DS segment
	str		r12, [sp, #SP_EX_CSIP]				; Remember where this opcode started, for division-by-zero and exception handling
	ldr		pc,[sp, r0, lsl #2]					; Jump to the opcode handler

	LTORG										; Dump the current literal pool here

	ALIGN	4

exit_restoring_flags
	msr		cpsr_f,r0							; Restore flags from r0
	;------ End of the opcode decoder loop
	GLOBAL unknown
unknown
	ldr		r12, [sp, #SP_EX_CSIP]				; rewind the CSEIP back to the beginning of this opcode.
	ldr		r1, =registers
	mov		r0, #0
	sub		r12, #1
	str		r0, [r1, #4*16]						; Save return value 0 = false
	IF CSIPTRACE = 1
	ldr		r3, [sp, #SP_FREE4]
	str		r3, [r1, #4*19]
	ENDIF	
	GLOBAL debug_trap_false
debug_trap_false
program_exit
	ldr		r1, =registers
	IF 0 = 1
	ldr		r3, [sp, #SP_FREE5]
	str		r3, [r1, #4*18]
	ldr		r3, [sp, #SP_SS_BASE]
	str		r3, [r1, #4*19]
	ENDIF
	;------ Save emulation registers
	stmia	r1!,{eax-r12}						; Save emulation registers to main memory
	ldr		r2,[sp, #SP_FLAGS]					; Get the current X86 FLAGS from stack
	join_ARM_to_X86_flags r2					; Update the X86 flags with the current ARM flags
	str		r2, [r1]							; Save the complete flags to main memory
	; ----- Tell we are not running any more
	ldr		r2,=IRQFlagAddr
	mov		r1, #0
	str		r1, [r2]							; Set IRQFlagAddr = NULL (= not running)
	;-------
	; Save the logical segment registers
	;-------
	add		r0, sp, #SP_ES_VALUE
	ldr		r1, =reg_es
	ldmia	r0, {r2-r7}							; Get current ES,CS,SS,DS,FS,GS
	stmia	r1!, {r2-r7}						; Save current ES,CS,SS,DS,FS,GS
	;-------
	; Save CPU_BIG, CPU_CPL and CPU_CR0
	;-------
	ldr		r2, =cpu_big
	ldr		r3, =cpu_cpl
	ldr		r4, =cpu_cr0
	ldrb	r5, [sp, #SP_CPU_BIG]
	ldrb	r6, [sp, #SP_CPU_CPL]
	ldrb	r7, [sp, #SP_CPU_CR0]
	strb	r5, [r2]
	strb	r6, [r3]
	strb	r7, [r4]
	;-------
	; Clean up the stack (remove the opcode table etc)
	;-------
	add		sp, #(256*4)						; Pop the opcode table from stack
	add		sp, #(SP_TLBTABLE-SP_IRQFLAG)		; Pop the extra local variables from stack
	;-------
	; Get the return value from registers[16]
	;-------
	ldr		r0, [r1]
	IF EXTERNAL_TLB = 1
	add		sp, #4								; Pop the SP_TLBTABLE pointer
	ELSE	
	;-------
	; Restore the EMSPageStatic table from stack
	;-------
	mov		r1, #(64/8)
	ldr		r2,=EMSPageStatic
1	pop		{r5-r12}							; 8 registers = 8 words at a time
	stmia	r2!, {r5-r12}
	subs	r1, #1
	bne		%b1
	;-------
	; Restore the wrap-around segment
	;-------
	pop		{r5-r8}								; 4 registers = 4 words
	stmia	r2!, {r5-r8}
	;-------
	; Pop the XMS memory addresses from stack
	;-------
	ldr		r3, =XMS_Total
	ldr		r3, [r3]
	lsr		r3, #(4-2)
	sub		r3, #(4*4)
	add		sp, r3
	;-------
	; Fix the EMSPageTable pointer, the table is now in main RAM.
	;-------
	ldr		r1,=EMSPageTable
	ldr		r2,=EMSPageStatic
	str		r2, [r1]
	ENDIF	
	;-------
	; Pop the used registers and return
	;-------
	pop		{r4-r12, pc}						; Pop used registers and return

bad_EGA_opcode_2
	add		r12, #1
bad_EGA_opcode_1
	add		r12, #1
	GLOBAL	bad_EGA_opcode
bad_EGA_opcode
	ldr		r0, =BRUnsEGA
	ldr		r1, =BreakReason
	str		r0, [r1]							; ... tell we break because of an unsupported EGA opcode
	b		debug_trap_false

bad_MODEX_opcode_2
	add		r12, #1
bad_MODEX_opcode_1
	add		r12, #1
	GLOBAL	bad_MODEX_opcode
bad_MODEX_opcode
	ldr		r0, =BRUnsMODEX
	ldr		r1, =BreakReason
	str		r0, [r1]							; ... tell we break because of an unsupported MODEX opcode
	b		debug_trap_false

	;------
	; Subroutines for breaking into the debugger when a certain opcode is
	; encountered at a certain CS:IP address.
	;------
	GLOBAL	GoToUSE16ROpcode
GoToUSE16ROpcode
	;-------
	; Check for the correct IP value
	;-------
	ldr		r2, =BreakOffs
	ldr		r1, [sp, #SP_PHYS_CS]				; Get current physical CS from stack
	ldr		r2, [r2]
	sub		r1, r12, r1							; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	mrs		r0,cpsr								; Save flags to r0
	cmp		r1, r2
	bne		%f1									; Not the correct IP address, call original handler
	;-------
	; Check for the correct segment value
	;-------
	ldr		r2, =BreakSeg
	ldr		r1, [sp, #SP_CS_VALUE]
	ldr		r2, [r2]
	cmp		r1, r2
	beq		exit_restoring_flags				; Correct CS:IP, break into the debugger!
	;-------
	; Call the original handler.
	;-------
1	ldr		r1, =OrigUSE16RHandler
	msr		cpsr_f,r0							; Restore flags from r0
	ldrb	r0, [r12, #-1]						; 2 Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; 2 r2 = logical DS segment
	ldr		pc, [r1]							; 9 Jump to the original opcode handler

	LTORG											; Dump the current literal pool here
	
; ------------------- 00 = ADD r/m8, r8 -------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_00
op_00
	;-------
	; Handle opcode 00.
	;-------
	modrm_jump_16_tbl op_00_jump
	modrm_tbl_0 add
	DCD add_al_al, add_cl_al, add_dl_al, add_bl_al, add_ah_al, add_ch_al, add_dh_al, add_bh_al
	DCD add_al_cl, add_cl_cl, add_dl_cl, add_bl_cl, add_ah_cl, add_ch_cl, add_dh_cl, add_bh_cl
	DCD add_al_dl, add_cl_dl, add_dl_dl, add_bl_dl, add_ah_dl, add_ch_dl, add_dh_dl, add_bh_dl
	DCD add_al_bl, add_cl_bl, add_dl_bl, add_bl_bl, add_ah_bl, add_ch_bl, add_dh_bl, add_bh_bl
	DCD add_al_ah, add_cl_ah, add_dl_ah, add_bl_ah, add_ah_ah, add_ch_ah, add_dh_ah, add_bh_ah
	DCD add_al_ch, add_cl_ch, add_dl_ch, add_bl_ch, add_ah_ch, add_ch_ch, add_dh_ch, add_bh_ch
	DCD add_al_dh, add_cl_dh, add_dl_dh, add_bl_dh, add_ah_dh, add_ch_dh, add_dh_dh, add_bh_dh
	DCD add_al_bh, add_cl_bh, add_dl_bh, add_bl_bh, add_ah_bh, add_ch_bh, add_dh_bh, add_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4
	
	MACRO 
	add_r0_reg8l $reg
	EXTERN	op_00_EGA_l_$reg
	EXTERN	op_00_MODEX_l_$reg
	GLOBAL	add_r0_r8l_bp_$reg
add_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	add_r0_r8l_$reg
add_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_00_RAM_l_$reg, op_00_EGA_l_$reg, op_00_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_00_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	adds	r0, r1, r0, lsl #24		; r0 = [RAM] + reg in highest byte
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to [physical segment + disp16]
	b		loop
	MEND
	MACRO 
	add_r0_reg8h $reg
	EXTERN	op_00_EGA_h_$reg
	EXTERN	op_00_MODEX_h_$reg
	GLOBAL	add_r0_r8h_bp_$reg
add_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	add_r0_r8h_$reg
add_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_00_RAM_h_$reg, op_00_EGA_h_$reg, op_00_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_00_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0x0000FF00
	lsl		r1, #16
	adds	r0, r1, r0, lsl #24		; r0 = [RAM] + reg in highest byte
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to RAM
	b		loop
	MEND

	add_r0_reg8l eax
	add_r0_reg8l ecx
	add_r0_reg8l edx
	add_r0_reg8l ebx
	add_r0_reg8h eax
	add_r0_reg8h ecx
	add_r0_reg8h edx
	add_r0_reg8h ebx

	LTORG
	
	modrm_0_genall add

; ------------------- 01 = ADD r/m16, r16 -----------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_01
op_01
	modrm_jump_16_tbl op_01_jump
	modrm_tbl_1_old add
	DCD add_ax_ax, add_cx_ax, add_dx_ax, add_bx_ax, add_sp_ax, add_bp_ax, add_si_ax, add_di_ax
	DCD add_ax_cx, add_cx_cx, add_dx_cx, add_bx_cx, add_sp_cx, add_bp_cx, add_si_cx, add_di_cx
	DCD add_ax_dx, add_cx_dx, add_dx_dx, add_bx_dx, add_sp_dx, add_bp_dx, add_si_dx, add_di_dx
	DCD add_ax_bx, add_cx_bx, add_dx_bx, add_bx_bx, add_sp_bx, add_bp_bx, add_si_bx, add_di_bx
	DCD add_ax_sp, add_cx_sp, add_dx_sp, add_bx_sp, add_sp_sp, add_bp_sp, add_si_sp, add_di_sp
	DCD add_ax_bp, add_cx_bp, add_dx_bp, add_bx_bp, add_sp_bp, add_bp_bp, add_si_bp, add_di_bp
	DCD add_ax_si, add_cx_si, add_dx_si, add_bx_si, add_sp_si, add_bp_si, add_si_si, add_di_si
	DCD add_ax_di, add_cx_di, add_dx_di, add_bx_di, add_sp_di, add_bp_di, add_si_di, add_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	add_r0_r16_reg $reg
	EXTERN	op_01_EGA_r2_$reg
	GLOBAL	add_r0_r16_bp_$reg
add_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	add_r0_r16_$reg
add_r0_r16_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_01_RAM_$reg, op_01_EGA_r2_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_01_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	lsl		r0, #16
	orr		r0, r1, lsl #24			; r0 = low byte | (high byte << 8)
	adds	r0, $reg, lsl #16		; r0 = [RAM] + reg
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop
	MEND

	add_r0_r16_reg eax
	add_r0_r16_reg ecx
	add_r0_r16_reg edx
	add_r0_r16_reg ebx
	add_r0_r16_reg esp
	add_r0_r16_reg ebp
	add_r0_r16_reg esi
	add_r0_r16_reg edi

	LTORG

	modrm_1_genall add, add_r0_r16


; ------------------- 02 = ADD r8, r/m8 -------------------------------
;
; All modrm variations supported!
;
;
op_02
	modrm_jump_16_tbl op_02_jump
	modrm_tbl_2 add
	DCD add_al_al, add_al_cl, add_al_dl, add_al_bl, add_al_ah, add_al_ch, add_al_dh, add_al_bh
	DCD add_cl_al, add_cl_cl, add_cl_dl, add_cl_bl, add_cl_ah, add_cl_ch, add_cl_dh, add_cl_bh
	DCD add_dl_al, add_dl_cl, add_dl_dl, add_dl_bl, add_dl_ah, add_dl_ch, add_dl_dh, add_dl_bh
	DCD add_bl_al, add_bl_cl, add_bl_dl, add_bl_bl, add_bl_ah, add_bl_ch, add_bl_dh, add_bl_bh
	DCD add_ah_al, add_ah_cl, add_ah_dl, add_ah_bl, add_ah_ah, add_ah_ch, add_ah_dh, add_ah_bh
	DCD add_ch_al, add_ch_cl, add_ch_dl, add_ch_bl, add_ch_ah, add_ch_ch, add_ch_dh, add_ch_bh
	DCD add_dh_al, add_dh_cl, add_dh_dl, add_dh_bl, add_dh_ah, add_dh_ch, add_dh_dh, add_dh_bh
	DCD add_bh_al, add_bh_cl, add_bh_dl, add_bh_bl, add_bh_ah, add_bh_ch, add_bh_dh, add_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4
	
	MACRO 
	add_reg8l_r0high $reg
	GLOBAL	add_r8l_r0_bp_$reg
add_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	add_r8l_r0_$reg
add_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_02_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_02_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1,$reg, lsl #24
	adds	r1, r0, lsl #24			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND
	MACRO 
	add_reg8h_r0high $reg
	GLOBAL	add_r8h_r0_bp_$reg
add_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	add_r8h_r0_$reg
add_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_02_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_02_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	adds	r1, r0, lsl #24			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16
	b		loop
	MEND

	add_reg8l_r0high eax
	add_reg8l_r0high ecx
	add_reg8l_r0high edx
	add_reg8l_r0high ebx
	add_reg8h_r0high eax
	add_reg8h_r0high ecx
	add_reg8h_r0high edx
	add_reg8h_r0high ebx

	LTORG
	
	modrm_2_genall add


; --- Register operands ---

	MACRO 
	add_reg8l_reg8l $rl, $rr
	mov		r0,$rl, lsl #24
	adds	r0, $rr, lsl #24		; Perform the addition using the highest bytes to get the correct flags
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND
	MACRO 
	add_reg8l_reg8h $rl, $rr
	and		r1, $rr, #0xFF00
	lsl		r1, #16
	adds	r0, r1, $rl, lsl #24	; Perform the addition using the highest bytes to get the correct flags
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND
	MACRO 
	add_reg8h_reg8l $rl, $rr
	and		r0, $rl, #0xFF00
	lsl		r0, #16
	adds	r0, $rr, lsl #24		; Perform the addition using the highest bytes to get the correct flags
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16
	b		loop
	MEND
	MACRO 
	add_reg8h_reg8h $rl, $rr
	and		r0, $rl, #0xFF00
	lsl		r0, #16
	and		r1, $rr, #0xFF00
	adds	r0, r1, lsl #16			; Perform the addition using the highest bytes to get the correct flags
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16
	b		loop
	MEND

add_al_al
	add_reg8l_reg8l eax, eax
add_al_cl
	add_reg8l_reg8l eax, ecx
add_al_dl
	add_reg8l_reg8l eax, edx
add_al_bl
	add_reg8l_reg8l eax, ebx
add_al_ah
	add_reg8l_reg8h eax, eax
add_al_ch
	add_reg8l_reg8h eax, ecx
add_al_dh
	add_reg8l_reg8h eax, edx
add_al_bh
	add_reg8l_reg8h eax, ebx
add_cl_al
	add_reg8l_reg8l ecx, eax
add_cl_cl
	add_reg8l_reg8l ecx, ecx
add_cl_dl
	add_reg8l_reg8l ecx, edx
add_cl_bl
	add_reg8l_reg8l ecx, ebx
add_cl_ah
	add_reg8l_reg8h ecx, eax
add_cl_ch
	add_reg8l_reg8h ecx, ecx
add_cl_dh
	add_reg8l_reg8h ecx, edx
add_cl_bh
	add_reg8l_reg8h ecx, ebx
add_dl_al
	add_reg8l_reg8l edx, eax
add_dl_cl
	add_reg8l_reg8l edx, ecx
add_dl_dl
	add_reg8l_reg8l edx, edx
add_dl_bl
	add_reg8l_reg8l edx, ebx
add_dl_ah
	add_reg8l_reg8h edx, eax
add_dl_ch
	add_reg8l_reg8h edx, ecx
add_dl_dh
	add_reg8l_reg8h edx, edx
add_dl_bh
	add_reg8l_reg8h edx, ebx
add_bl_al
	add_reg8l_reg8l ebx, eax
add_bl_cl
	add_reg8l_reg8l ebx, ecx
add_bl_dl
	add_reg8l_reg8l ebx, edx
add_bl_bl
	add_reg8l_reg8l ebx, ebx
add_bl_ah
	add_reg8l_reg8h ebx, eax
add_bl_ch
	add_reg8l_reg8h ebx, ecx
add_bl_dh
	add_reg8l_reg8h ebx, edx
add_bl_bh
	add_reg8l_reg8h ebx, ebx

add_ah_al
	add_reg8h_reg8l eax, eax
add_ah_cl
	add_reg8h_reg8l eax, ecx
add_ah_dl
	add_reg8h_reg8l eax, edx
add_ah_bl
	add_reg8h_reg8l eax, ebx
add_ah_ah
	add_reg8h_reg8h eax, eax
add_ah_ch
	add_reg8h_reg8h eax, ecx
add_ah_dh
	add_reg8h_reg8h eax, edx
add_ah_bh
	add_reg8h_reg8h eax, ebx
add_ch_al
	add_reg8h_reg8l ecx, eax
add_ch_cl
	add_reg8h_reg8l ecx, ecx
add_ch_dl
	add_reg8h_reg8l ecx, edx
add_ch_bl
	add_reg8h_reg8l ecx, ebx
add_ch_ah
	add_reg8h_reg8h ecx, eax
add_ch_ch
	add_reg8h_reg8h ecx, ecx
add_ch_dh
	add_reg8h_reg8h ecx, edx
add_ch_bh
	add_reg8h_reg8h ecx, ebx
add_dh_al
	add_reg8h_reg8l edx, eax
add_dh_cl
	add_reg8h_reg8l edx, ecx
add_dh_dl
	add_reg8h_reg8l edx, edx
add_dh_bl
	add_reg8h_reg8l edx, ebx
add_dh_ah
	add_reg8h_reg8h edx, eax
add_dh_ch
	add_reg8h_reg8h edx, ecx
add_dh_dh
	add_reg8h_reg8h edx, edx
add_dh_bh
	add_reg8h_reg8h edx, ebx
add_bh_al
	add_reg8h_reg8l ebx, eax
add_bh_cl
	add_reg8h_reg8l ebx, ecx
add_bh_dl
	add_reg8h_reg8l ebx, edx
add_bh_bl
	add_reg8h_reg8l ebx, ebx
add_bh_ah
	add_reg8h_reg8h ebx, eax
add_bh_ch
	add_reg8h_reg8h ebx, ecx
add_bh_dh
	add_reg8h_reg8h ebx, edx
add_bh_bh
	add_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  add_al_al
	GLOBAL  add_al_cl
	GLOBAL  add_al_dl
	GLOBAL  add_al_bl
	GLOBAL  add_al_ah
	GLOBAL  add_al_ch
	GLOBAL  add_al_dh
	GLOBAL  add_al_bh
	GLOBAL  add_cl_al
	GLOBAL  add_cl_cl
	GLOBAL  add_cl_dl
	GLOBAL  add_cl_bl
	GLOBAL  add_cl_ah
	GLOBAL  add_cl_ch
	GLOBAL  add_cl_dh
	GLOBAL  add_cl_bh
	GLOBAL  add_dl_al
	GLOBAL  add_dl_cl
	GLOBAL  add_dl_dl
	GLOBAL  add_dl_bl
	GLOBAL  add_dl_ah
	GLOBAL  add_dl_ch
	GLOBAL  add_dl_dh
	GLOBAL  add_dl_bh
	GLOBAL  add_bl_al
	GLOBAL  add_bl_cl
	GLOBAL  add_bl_dl
	GLOBAL  add_bl_bl
	GLOBAL  add_bl_ah
	GLOBAL  add_bl_ch
	GLOBAL  add_bl_dh
	GLOBAL  add_bl_bh
	GLOBAL  add_ah_al
	GLOBAL  add_ah_cl
	GLOBAL  add_ah_dl
	GLOBAL  add_ah_bl
	GLOBAL  add_ah_ah
	GLOBAL  add_ah_ch
	GLOBAL  add_ah_dh
	GLOBAL  add_ah_bh
	GLOBAL  add_ch_al
	GLOBAL  add_ch_cl
	GLOBAL  add_ch_dl
	GLOBAL  add_ch_bl
	GLOBAL  add_ch_ah
	GLOBAL  add_ch_ch
	GLOBAL  add_ch_dh
	GLOBAL  add_ch_bh
	GLOBAL  add_dh_al
	GLOBAL  add_dh_cl
	GLOBAL  add_dh_dl
	GLOBAL  add_dh_bl
	GLOBAL  add_dh_ah
	GLOBAL  add_dh_ch
	GLOBAL  add_dh_dh
	GLOBAL  add_dh_bh
	GLOBAL  add_bh_al
	GLOBAL  add_bh_cl
	GLOBAL  add_bh_dl
	GLOBAL  add_bh_bl
	GLOBAL  add_bh_ah
	GLOBAL  add_bh_ch
	GLOBAL  add_bh_dh
	GLOBAL  add_bh_bh
	
; ------------------- 03 = ADD r16, r/m16 ------------------------------
;
; All modrm variations supported!
;
;
op_03
	modrm_jump_16_tbl op_03_jump
	modrm_tbl_3_old add
	DCD add_ax_ax, add_ax_cx, add_ax_dx, add_ax_bx, add_ax_sp, add_ax_bp, add_ax_si, add_ax_di
	DCD add_cx_ax, add_cx_cx, add_cx_dx, add_cx_bx, add_cx_sp, add_cx_bp, add_cx_si, add_cx_di
	DCD add_dx_ax, add_dx_cx, add_dx_dx, add_dx_bx, add_dx_sp, add_dx_bp, add_dx_si, add_dx_di
	DCD add_bx_ax, add_bx_cx, add_bx_dx, add_bx_bx, add_bx_sp, add_bx_bp, add_bx_si, add_bx_di
	DCD add_sp_ax, add_sp_cx, add_sp_dx, add_sp_bx, add_sp_sp, add_sp_bp, add_sp_si, add_sp_di
	DCD add_bp_ax, add_bp_cx, add_bp_dx, add_bp_bx, add_bp_sp, add_bp_bp, add_bp_si, add_bp_di
	DCD add_si_ax, add_si_cx, add_si_dx, add_si_bx, add_si_sp, add_si_bp, add_si_si, add_si_di
	DCD add_di_ax, add_di_cx, add_di_dx, add_di_bx, add_di_sp, add_di_bp, add_di_si, add_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	add_reg16_r0high $reg
	EXTERN	op_03_EGA_$reg
	GLOBAL	add_r16_r0_bp_$reg
add_r16_r0_bp_$reg
	;-------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;-------
	mem_handler_bp
	GLOBAL	add_r16_r0_$reg
add_r16_r0_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_03_RAM_$reg, op_03_EGA_$reg, bad_MODEX_opcode
op_03_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	adds	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

	add_reg16_r0high eax
	add_reg16_r0high ecx
	add_reg16_r0high edx
	add_reg16_r0high ebx
	add_reg16_r0high esp
	add_reg16_r0high ebp
	add_reg16_r0high esi
	add_reg16_r0high edi

	LTORG

	modrm_3_genall_old add, add_r16_r0
	

; --- registers ---

	MACRO 
	add_reg16_reg16 $rl, $rr
	mov		r1, $rl, lsl #16					; Put the 16-bit register value to high halfword of r1
	adds	r0, r1, $rr, lsl #16				; Perform the operation, put result to r0
	eor		$rl, r1, lsr #16					; Clear the left 16-bit register value
	orr		$rl, r0, lsr #16					; Put the result into the left 16-bit register.
	b		loop
	MEND

add_ax_ax
shl_reg16_1_eax
	add_reg16_reg16		eax, eax
add_ax_cx
	add_reg16_reg16		eax, ecx
add_ax_dx
	add_reg16_reg16		eax, edx
add_ax_bx
	add_reg16_reg16		eax, ebx
add_ax_sp
	add_reg16_reg16		eax, esp
add_ax_bp
	add_reg16_reg16		eax, ebp
add_ax_si
	add_reg16_reg16		eax, esi
add_ax_di
	add_reg16_reg16		eax, edi
add_cx_ax
	add_reg16_reg16		ecx, eax
add_cx_cx
shl_reg16_1_ecx
	add_reg16_reg16		ecx, ecx
add_cx_dx
	add_reg16_reg16		ecx, edx
add_cx_bx
	add_reg16_reg16		ecx, ebx
add_cx_sp
	add_reg16_reg16		ecx, esp
add_cx_bp
	add_reg16_reg16		ecx, ebp
add_cx_si
	add_reg16_reg16		ecx, esi
add_cx_di
	add_reg16_reg16		ecx, edi
add_dx_ax
	add_reg16_reg16		edx, eax
add_dx_cx
	add_reg16_reg16		edx, ecx
add_dx_dx
shl_reg16_1_edx
	add_reg16_reg16		edx, edx
add_dx_bx
	add_reg16_reg16		edx, ebx
add_dx_sp
	add_reg16_reg16		edx, esp
add_dx_bp
	add_reg16_reg16		edx, ebp
add_dx_si
	add_reg16_reg16		edx, esi
add_dx_di
	add_reg16_reg16		edx, edi
add_bx_ax
	add_reg16_reg16		ebx, eax
add_bx_cx
	add_reg16_reg16		ebx, ecx
add_bx_dx
	add_reg16_reg16		ebx, edx
add_bx_bx
shl_reg16_1_ebx
	add_reg16_reg16		ebx, ebx
add_bx_sp
	add_reg16_reg16		ebx, esp
add_bx_bp
	add_reg16_reg16		ebx, ebp
add_bx_si
	add_reg16_reg16		ebx, esi
add_bx_di
	add_reg16_reg16		ebx, edi
add_sp_ax
	add_reg16_reg16		esp, eax
add_sp_cx
	add_reg16_reg16		esp, ecx
add_sp_dx
	add_reg16_reg16		esp, edx
add_sp_bx
	add_reg16_reg16		esp, ebx
add_sp_sp
shl_reg16_1_esp
	add_reg16_reg16		esp, esp
add_sp_bp
	add_reg16_reg16		esp, ebp
add_sp_si
	add_reg16_reg16		esp, esi
add_sp_di
	add_reg16_reg16		esp, edi
add_bp_ax
	add_reg16_reg16		ebp, eax
add_bp_cx
	add_reg16_reg16		ebp, ecx
add_bp_dx
	add_reg16_reg16		ebp, edx
add_bp_bx
	add_reg16_reg16		ebp, ebx
add_bp_sp
	add_reg16_reg16		ebp, esp
add_bp_bp
shl_reg16_1_ebp
	add_reg16_reg16		ebp, ebp
add_bp_si
	add_reg16_reg16		ebp, esi
add_bp_di
	add_reg16_reg16		ebp, edi
add_si_ax
	add_reg16_reg16		esi, eax
add_si_cx
	add_reg16_reg16		esi, ecx
add_si_dx
	add_reg16_reg16		esi, edx
add_si_bx
	add_reg16_reg16		esi, ebx
add_si_sp
	add_reg16_reg16		esi, esp
add_si_bp
	add_reg16_reg16		esi, ebp
add_si_si
shl_reg16_1_esi
	add_reg16_reg16		esi, esi
add_si_di
	add_reg16_reg16		esi, edi
add_di_ax
	add_reg16_reg16		edi, eax
add_di_cx
	add_reg16_reg16		edi, ecx
add_di_dx
	add_reg16_reg16		edi, edx
add_di_bx
	add_reg16_reg16		edi, ebx
add_di_sp
	add_reg16_reg16		edi, esp
add_di_bp
	add_reg16_reg16		edi, ebp
add_di_si
	add_reg16_reg16		edi, esi
add_di_di
shl_reg16_1_edi
	add_reg16_reg16		edi, edi

; These are called from "cpu_67.s"

	GLOBAL  add_ax_ax
	GLOBAL  shl_reg16_1_eax
	GLOBAL  add_ax_cx
	GLOBAL  add_ax_dx
	GLOBAL  add_ax_bx
	GLOBAL  add_ax_sp
	GLOBAL  add_ax_bp
	GLOBAL  add_ax_si
	GLOBAL  add_ax_di
	GLOBAL  add_cx_ax
	GLOBAL  add_cx_cx
	GLOBAL  shl_reg16_1_ecx
	GLOBAL  add_cx_dx
	GLOBAL  add_cx_bx
	GLOBAL  add_cx_sp
	GLOBAL  add_cx_bp
	GLOBAL  add_cx_si
	GLOBAL  add_cx_di
	GLOBAL  add_dx_ax
	GLOBAL  add_dx_cx
	GLOBAL  add_dx_dx
	GLOBAL  shl_reg16_1_edx
	GLOBAL  add_dx_bx
	GLOBAL  add_dx_sp
	GLOBAL  add_dx_bp
	GLOBAL  add_dx_si
	GLOBAL  add_dx_di
	GLOBAL  add_bx_ax
	GLOBAL  add_bx_cx
	GLOBAL  add_bx_dx
	GLOBAL  add_bx_bx
	GLOBAL  shl_reg16_1_ebx
	GLOBAL  add_bx_sp
	GLOBAL  add_bx_bp
	GLOBAL  add_bx_si
	GLOBAL  add_bx_di
	GLOBAL  add_sp_ax
	GLOBAL  add_sp_cx
	GLOBAL  add_sp_dx
	GLOBAL  add_sp_bx
	GLOBAL  add_sp_sp
	GLOBAL  shl_reg16_1_esp
	GLOBAL  add_sp_bp
	GLOBAL  add_sp_si
	GLOBAL  add_sp_di
	GLOBAL  add_bp_ax
	GLOBAL  add_bp_cx
	GLOBAL  add_bp_dx
	GLOBAL  add_bp_bx
	GLOBAL  add_bp_sp
	GLOBAL  add_bp_bp
	GLOBAL  shl_reg16_1_ebp
	GLOBAL  add_bp_si
	GLOBAL  add_bp_di
	GLOBAL  add_si_ax
	GLOBAL  add_si_cx
	GLOBAL  add_si_dx
	GLOBAL  add_si_bx
	GLOBAL  add_si_sp
	GLOBAL  add_si_bp
	GLOBAL  add_si_si
	GLOBAL  shl_reg16_1_esi
	GLOBAL  add_si_di
	GLOBAL  add_di_ax
	GLOBAL  add_di_cx
	GLOBAL  add_di_dx
	GLOBAL  add_di_bx
	GLOBAL  add_di_sp
	GLOBAL  add_di_bp
	GLOBAL  add_di_si
	GLOBAL  add_di_di
	GLOBAL  shl_reg16_1_edi

; ------------------- 04 = ADD AL,imm8 --------------------------------
op_04
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r1, eax, lsl #24
	adds	r1, r0, lsl #24
	bic		eax, #0xFF
	orr		eax, r1, lsr #24
	b		loop
	
; ------------------- 05 = ADD AX,imm16 -------------------------------
op_05
	ldrb	r1,[r12],#1				; Load byte to r1, increment r12 by 1
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, eax, lsl #16
	eor		eax, r2, lsr #16
	orr		r1, r0, lsl #8			; r1 = low byte | (high byte << 8)
	adds	r0, r2, r1, lsl #16
	orr		eax, r0, lsr #16
	b		loop

; ------------------- 06 = PUSH ES ------------------------------------
op_06
	ldr		r1, [sp, #SP_ES_VALUE]	
	push_hword r1, r0, r2
	b		loop

; ------------------- 07 = POP ES -------------------------------------
; Profiler: 4027, 18, 51.37, 206885, 0.1%
;
op_07
	pop_reg_low_tmp r2, r1
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_es_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_ES_VALUE]
	str		r1, [sp, #SP_ES_BASE]
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	EXTERN	mov_es_r0r2_prot
	
	LTORG								; Dump the current literal pool here

	ALIGN	4

; ------------------- 08 = OR r/m8,esp -------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_08
op_08
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_08_jump
	modrm_tbl_0	or
	DCD or_al_al, or_cl_al, or_dl_al, or_bl_al, or_ah_al, or_ch_al, or_dh_al, or_bh_al
	DCD or_al_cl, or_cl_cl, or_dl_cl, or_bl_cl, or_ah_cl, or_ch_cl, or_dh_cl, or_bh_cl
	DCD or_al_dl, or_cl_dl, or_dl_dl, or_bl_dl, or_ah_dl, or_ch_dl, or_dh_dl, or_bh_dl
	DCD or_al_bl, or_cl_bl, or_dl_bl, or_bl_bl, or_ah_bl, or_ch_bl, or_dh_bl, or_bh_bl
	DCD or_al_ah, or_cl_ah, or_dl_ah, or_bl_ah, or_ah_ah, or_ch_ah, or_dh_ah, or_bh_ah
	DCD or_al_ch, or_cl_ch, or_dl_ch, or_bl_ch, or_ah_ch, or_ch_ch, or_dh_ch, or_bh_ch
	DCD or_al_dh, or_cl_dh, or_dl_dh, or_bl_dh, or_ah_dh, or_ch_dh, or_dh_dh, or_bh_dh
	DCD or_al_bh, or_cl_bh, or_dl_bh, or_bl_bh, or_ah_bh, or_ch_bh, or_dh_bh, or_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	or_r0_reg8l $reg
	EXTERN	op_08_EGA_l_$reg
	EXTERN	op_08_MODEX_l_$reg
	GLOBAL	or_r0_r8l_bp_$reg
or_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	or_r0_r8l_$reg
or_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_08_RAM_l_$reg, op_08_EGA_l_$reg, op_08_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_08_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	orrs	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND
	MACRO 
	or_r0_reg8h $reg
	EXTERN	op_08_EGA_h_$reg
	EXTERN	op_08_MODEX_h_$reg
	GLOBAL	or_r0_r8h_bp_$reg
or_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	or_r0_r8h_$reg
or_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_08_RAM_h_$reg, op_08_EGA_h_$reg, op_08_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_08_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	orrs	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND

	or_r0_reg8l eax
	or_r0_reg8l ecx
	or_r0_reg8l edx
	or_r0_reg8l ebx
	or_r0_reg8h eax
	or_r0_reg8h ecx
	or_r0_reg8h edx
	or_r0_reg8h ebx

	LTORG
	
	modrm_0_genall or


; ------------------- 09 = OR r/m16,r16 -------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_09
op_09
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_09_jump
	modrm_tbl_1_old or
	DCD or_ax_ax, or_cx_ax, or_dx_ax, or_bx_ax, or_sp_ax, or_bp_ax, or_si_ax, or_di_ax
	DCD or_ax_cx, or_cx_cx, or_dx_cx, or_bx_cx, or_sp_cx, or_bp_cx, or_si_cx, or_di_cx
	DCD or_ax_dx, or_cx_dx, or_dx_dx, or_bx_dx, or_sp_dx, or_bp_dx, or_si_dx, or_di_dx
	DCD or_ax_bx, or_cx_bx, or_dx_bx, or_bx_bx, or_sp_bx, or_bp_bx, or_si_bx, or_di_bx
	DCD or_ax_sp, or_cx_sp, or_dx_sp, or_bx_sp, or_sp_sp, or_bp_sp, or_si_sp, or_di_sp
	DCD or_ax_bp, or_cx_bp, or_dx_bp, or_bx_bp, or_sp_bp, or_bp_bp, or_si_bp, or_di_bp
	DCD or_ax_si, or_cx_si, or_dx_si, or_bx_si, or_sp_si, or_bp_si, or_si_si, or_di_si
	DCD or_ax_di, or_cx_di, or_dx_di, or_bx_di, or_sp_di, or_bp_di, or_si_di, or_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	or_r0_r16_reg $reg
	EXTERN	op_09_EGA_r2_$reg
	GLOBAL	or_r0_r16_bp_$reg
or_r0_r16_bp_$reg
	;-------
	; Indexing by BP register, so use SS unless a segment override is in effect.
	;-------
	mem_handler_bp
	GLOBAL	or_r0_r16_$reg
or_r0_r16_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_09_RAM_$reg, op_09_EGA_r2_$reg, bad_MODEX_opcode
op_09_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r3, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	orrs	r0, r3, r0, lsl #16
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop
	MEND

	or_r0_r16_reg eax
	or_r0_r16_reg ecx
	or_r0_r16_reg edx
	or_r0_r16_reg ebx
	or_r0_r16_reg esp
	or_r0_r16_reg ebp
	or_r0_r16_reg esi
	or_r0_r16_reg edi

	LTORG
	
	modrm_1_genall or, or_r0_r16


; ------------------- 0A = OR r8,r/m8 -------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_0a
op_0a
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_0a_jump
	modrm_tbl_2 or
	DCD or_al_al, or_al_cl, or_al_dl, or_al_bl, or_al_ah, or_al_ch, or_al_dh, or_al_bh
	DCD or_cl_al, or_cl_cl, or_cl_dl, or_cl_bl, or_cl_ah, or_cl_ch, or_cl_dh, or_cl_bh
	DCD or_dl_al, or_dl_cl, or_dl_dl, or_dl_bl, or_dl_ah, or_dl_ch, or_dl_dh, or_dl_bh
	DCD or_bl_al, or_bl_cl, or_bl_dl, or_bl_bl, or_bl_ah, or_bl_ch, or_bl_dh, or_bl_bh
	DCD or_ah_al, or_ah_cl, or_ah_dl, or_ah_bl, or_ah_ah, or_ah_ch, or_ah_dh, or_ah_bh
	DCD or_ch_al, or_ch_cl, or_ch_dl, or_ch_bl, or_ch_ah, or_ch_ch, or_ch_dh, or_ch_bh
	DCD or_dh_al, or_dh_cl, or_dh_dl, or_dh_bl, or_dh_ah, or_dh_ch, or_dh_dh, or_dh_bh
	DCD or_bh_al, or_bh_cl, or_bh_dl, or_bh_bl, or_bh_ah, or_bh_ch, or_bh_dh, or_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	or_reg8l_r0high $reg
	EXTERN	op_0a_EGA_l_$reg
	EXTERN	op_0a_MODEX_l_$reg
	GLOBAL	or_r8l_r0_bp_$reg
or_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	or_r8l_r0_$reg
or_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_0a_RAM_l_$reg, op_0a_EGA_l_$reg, op_0a_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_0a_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1,$reg, lsl #24
	orrs	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND
	MACRO 
	or_reg8h_r0high $reg
	EXTERN	op_0a_EGA_h_$reg
	EXTERN	op_0a_MODEX_h_$reg
	GLOBAL	or_r8h_r0_bp_$reg
or_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	or_r8h_r0_$reg
or_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_0a_RAM_h_$reg, op_0a_EGA_h_$reg, op_0a_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_0a_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	orrs	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16
	b		loop
	MEND

	or_reg8l_r0high eax
	or_reg8l_r0high ecx
	or_reg8l_r0high edx
	or_reg8l_r0high ebx
	or_reg8h_r0high eax
	or_reg8h_r0high ecx
	or_reg8h_r0high edx
	or_reg8h_r0high ebx

	LTORG

	modrm_2_genall or


; --- Register operands ---

	MACRO 
	or_reg8l_reg8l $rl, $rr
	mov		r0, $rl, lsl #24
	mov		r1, $rr, lsl #24
	orrs	r0, r1					; Perform the operation using the highest bytes to get the correct flags
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND

	MACRO 
	or_reg8l_reg8h $rl, $rr
	and		r0, $rr, #0xFF00
	lsl		r0, #16
	mov		r1, $rl, lsl #24
	orrs	r0, r1					; Perform the operation using the highest bytes to get the correct flags
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND

	MACRO 
	or_reg8h_reg8l $rl, $rr
	and		r0, $rl, #0xFF00
	lsl		r0, #16
	mov		r1, $rr, lsl #24
	orrs	r0, r1					; Perform the operation using the highest bytes to get the correct flags
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; Put the result to the proper byte of the left register
	b		loop
	MEND

	MACRO 
	or_reg8h_reg8h $rl, $rr
	and		r0, $rl, #0xFF00		; Left operand already uses just the rightmost byte
	and		r1, $rr, #0xFF00		; Right operand already uses just the rightmost byte
	lsl		r0, #16
	lsl		r1, #16
	orrs	r0, r1					; Perform the operation using the highest bytes to get the correct flags
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; Put the result to the proper byte of the left register
	b		loop
	MEND

or_al_al
	or_reg8l_reg8l eax, eax
or_al_cl
	or_reg8l_reg8l eax, ecx
or_al_dl
	or_reg8l_reg8l eax, edx
or_al_bl
	or_reg8l_reg8l eax, ebx
or_al_ah
	or_reg8l_reg8h eax, eax
or_al_ch
	or_reg8l_reg8h eax, ecx
or_al_dh
	or_reg8l_reg8h eax, edx
or_al_bh
	or_reg8l_reg8h eax, ebx
or_cl_al
	or_reg8l_reg8l ecx, eax
or_cl_cl
	or_reg8l_reg8l ecx, ecx
or_cl_dl
	or_reg8l_reg8l ecx, edx
or_cl_bl
	or_reg8l_reg8l ecx, ebx
or_cl_ah
	or_reg8l_reg8h ecx, eax
or_cl_ch
	or_reg8l_reg8h ecx, ecx
or_cl_dh
	or_reg8l_reg8h ecx, edx
or_cl_bh
	or_reg8l_reg8h ecx, ebx
or_dl_al
	or_reg8l_reg8l edx, eax
or_dl_cl
	or_reg8l_reg8l edx, ecx
or_dl_dl
	or_reg8l_reg8l edx, edx
or_dl_bl
	or_reg8l_reg8l edx, ebx
or_dl_ah
	or_reg8l_reg8h edx, eax
or_dl_ch
	or_reg8l_reg8h edx, ecx
or_dl_dh
	or_reg8l_reg8h edx, edx
or_dl_bh
	or_reg8l_reg8h edx, ebx
or_bl_al
	or_reg8l_reg8l ebx, eax
or_bl_cl
	or_reg8l_reg8l ebx, ecx
or_bl_dl
	or_reg8l_reg8l ebx, edx
or_bl_bl
	or_reg8l_reg8l ebx, ebx
or_bl_ah
	or_reg8l_reg8h ebx, eax
or_bl_ch
	or_reg8l_reg8h ebx, ecx
or_bl_dh
	or_reg8l_reg8h ebx, edx
or_bl_bh
	or_reg8l_reg8h ebx, ebx

or_ah_al
	or_reg8h_reg8l eax, eax
or_ah_cl
	or_reg8h_reg8l eax, ecx
or_ah_dl
	or_reg8h_reg8l eax, edx
or_ah_bl
	or_reg8h_reg8l eax, ebx
or_ah_ah
	or_reg8h_reg8h eax, eax
or_ah_ch
	or_reg8h_reg8h eax, ecx
or_ah_dh
	or_reg8h_reg8h eax, edx
or_ah_bh
	or_reg8h_reg8h eax, ebx
or_ch_al
	or_reg8h_reg8l ecx, eax
or_ch_cl
	or_reg8h_reg8l ecx, ecx
or_ch_dl
	or_reg8h_reg8l ecx, edx
or_ch_bl
	or_reg8h_reg8l ecx, ebx
or_ch_ah
	or_reg8h_reg8h ecx, eax
or_ch_ch
	or_reg8h_reg8h ecx, ecx
or_ch_dh
	or_reg8h_reg8h ecx, edx
or_ch_bh
	or_reg8h_reg8h ecx, ebx
or_dh_al
	or_reg8h_reg8l edx, eax
or_dh_cl
	or_reg8h_reg8l edx, ecx
or_dh_dl
	or_reg8h_reg8l edx, edx
or_dh_bl
	or_reg8h_reg8l edx, ebx
or_dh_ah
	or_reg8h_reg8h edx, eax
or_dh_ch
	or_reg8h_reg8h edx, ecx
or_dh_dh
	or_reg8h_reg8h edx, edx
or_dh_bh
	or_reg8h_reg8h edx, ebx
or_bh_al
	or_reg8h_reg8l ebx, eax
or_bh_cl
	or_reg8h_reg8l ebx, ecx
or_bh_dl
	or_reg8h_reg8l ebx, edx
or_bh_bl
	or_reg8h_reg8l ebx, ebx
or_bh_ah
	or_reg8h_reg8h ebx, eax
or_bh_ch
	or_reg8h_reg8h ebx, ecx
or_bh_dh
	or_reg8h_reg8h ebx, edx
or_bh_bh
	or_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  or_al_al
	GLOBAL  or_al_cl
	GLOBAL  or_al_dl
	GLOBAL  or_al_bl
	GLOBAL  or_al_ah
	GLOBAL  or_al_ch
	GLOBAL  or_al_dh
	GLOBAL  or_al_bh
	GLOBAL  or_cl_al
	GLOBAL  or_cl_cl
	GLOBAL  or_cl_dl
	GLOBAL  or_cl_bl
	GLOBAL  or_cl_ah
	GLOBAL  or_cl_ch
	GLOBAL  or_cl_dh
	GLOBAL  or_cl_bh
	GLOBAL  or_dl_al
	GLOBAL  or_dl_cl
	GLOBAL  or_dl_dl
	GLOBAL  or_dl_bl
	GLOBAL  or_dl_ah
	GLOBAL  or_dl_ch
	GLOBAL  or_dl_dh
	GLOBAL  or_dl_bh
	GLOBAL  or_bl_al
	GLOBAL  or_bl_cl
	GLOBAL  or_bl_dl
	GLOBAL  or_bl_bl
	GLOBAL  or_bl_ah
	GLOBAL  or_bl_ch
	GLOBAL  or_bl_dh
	GLOBAL  or_bl_bh
	GLOBAL  or_ah_al
	GLOBAL  or_ah_cl
	GLOBAL  or_ah_dl
	GLOBAL  or_ah_bl
	GLOBAL  or_ah_ah
	GLOBAL  or_ah_ch
	GLOBAL  or_ah_dh
	GLOBAL  or_ah_bh
	GLOBAL  or_ch_al
	GLOBAL  or_ch_cl
	GLOBAL  or_ch_dl
	GLOBAL  or_ch_bl
	GLOBAL  or_ch_ah
	GLOBAL  or_ch_ch
	GLOBAL  or_ch_dh
	GLOBAL  or_ch_bh
	GLOBAL  or_dh_al
	GLOBAL  or_dh_cl
	GLOBAL  or_dh_dl
	GLOBAL  or_dh_bl
	GLOBAL  or_dh_ah
	GLOBAL  or_dh_ch
	GLOBAL  or_dh_dh
	GLOBAL  or_dh_bh
	GLOBAL  or_bh_al
	GLOBAL  or_bh_cl
	GLOBAL  or_bh_dl
	GLOBAL  or_bh_bl
	GLOBAL  or_bh_ah
	GLOBAL  or_bh_ch
	GLOBAL  or_bh_dh
	GLOBAL  or_bh_bh

; ------------------- 0B = OR r16,r/m16 -------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_0b
op_0b
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_0b_jump
	modrm_tbl_3_old or
	DCD or_ax_ax, or_ax_cx, or_ax_dx, or_ax_bx, or_ax_sp, or_ax_bp, or_ax_si, or_ax_di
	DCD or_cx_ax, or_cx_cx, or_cx_dx, or_cx_bx, or_cx_sp, or_cx_bp, or_cx_si, or_cx_di
	DCD or_dx_ax, or_dx_cx, or_dx_dx, or_dx_bx, or_dx_sp, or_dx_bp, or_dx_si, or_dx_di
	DCD or_bx_ax, or_bx_cx, or_bx_dx, or_bx_bx, or_bx_sp, or_bx_bp, or_bx_si, or_bx_di
	DCD or_sp_ax, or_sp_cx, or_sp_dx, or_sp_bx, or_sp_sp, or_sp_bp, or_sp_si, or_sp_di
	DCD or_bp_ax, or_bp_cx, or_bp_dx, or_bp_bx, or_bp_sp, or_bp_bp, or_bp_si, or_bp_di
	DCD or_si_ax, or_si_cx, or_si_dx, or_si_bx, or_si_sp, or_si_bp, or_si_si, or_si_di
	DCD or_di_ax, or_di_cx, or_di_dx, or_di_bx, or_di_sp, or_di_bp, or_di_si, or_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	or_reg16_r0high $reg
	EXTERN	op_0b_EGA_$reg
	GLOBAL	or_r16_r0_bp_$reg
or_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	or_r16_r0_$reg
or_r16_r0_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_0b_RAM_$reg, op_0b_EGA_$reg, bad_MODEX_opcode
op_0b_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r3, $reg, lsl #16
	eor		$reg, r3, lsr #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	orrs	r0, r3, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	or_reg16_r0high eax
	or_reg16_r0high ecx
	or_reg16_r0high edx
	or_reg16_r0high ebx
	or_reg16_r0high esp
	or_reg16_r0high ebp
	or_reg16_r0high esi
	or_reg16_r0high edi

	LTORG
	
	modrm_3_genall_old or, or_r16_r0


; --- OR reg16, reg16 ---

	MACRO 
	or_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	mov		r1, $rr, lsl #16
	eor		$rl, r0, lsr #16
	orrs	r0, r1
	orr		$rl, r0, lsr #16
	b		loop
	MEND

or_ax_ax
	or_reg16_reg16		eax, eax
or_ax_cx
	or_reg16_reg16		eax, ecx
or_ax_dx
	or_reg16_reg16		eax, edx
or_ax_bx
	or_reg16_reg16		eax, ebx
or_ax_sp
	or_reg16_reg16		eax, esp
or_ax_bp
	or_reg16_reg16		eax, ebp
or_ax_si
	or_reg16_reg16		eax, esi
or_ax_di
	or_reg16_reg16		eax, edi
or_cx_ax
	or_reg16_reg16		ecx, eax
or_cx_cx
	or_reg16_reg16		ecx, ecx
or_cx_dx
	or_reg16_reg16		ecx, edx
or_cx_bx
	or_reg16_reg16		ecx, ebx
or_cx_sp
	or_reg16_reg16		ecx, esp
or_cx_bp
	or_reg16_reg16		ecx, ebp
or_cx_si
	or_reg16_reg16		ecx, esi
or_cx_di
	or_reg16_reg16		ecx, edi
or_dx_ax
	or_reg16_reg16		edx, eax
or_dx_cx
	or_reg16_reg16		edx, ecx
or_dx_dx
	or_reg16_reg16		edx, edx
or_dx_bx
	or_reg16_reg16		edx, ebx
or_dx_sp
	or_reg16_reg16		edx, esp
or_dx_bp
	or_reg16_reg16		edx, ebp
or_dx_si
	or_reg16_reg16		edx, esi
or_dx_di
	or_reg16_reg16		edx, edi
or_bx_ax
	or_reg16_reg16		ebx, eax
or_bx_cx
	or_reg16_reg16		ebx, ecx
or_bx_dx
	or_reg16_reg16		ebx, edx
or_bx_bx
	or_reg16_reg16		ebx, ebx
or_bx_sp
	or_reg16_reg16		ebx, esp
or_bx_bp
	or_reg16_reg16		ebx, ebp
or_bx_si
	or_reg16_reg16		ebx, esi
or_bx_di
	or_reg16_reg16		ebx, edi
or_sp_ax
	or_reg16_reg16		esp, eax
or_sp_cx
	or_reg16_reg16		esp, ecx
or_sp_dx
	or_reg16_reg16		esp, edx
or_sp_bx
	or_reg16_reg16		esp, ebx
or_sp_sp
	or_reg16_reg16		esp, esp
or_sp_bp
	or_reg16_reg16		esp, ebp
or_sp_si
	or_reg16_reg16		esp, esi
or_sp_di
	or_reg16_reg16		esp, edi
or_bp_ax
	or_reg16_reg16		ebp, eax
or_bp_cx
	or_reg16_reg16		ebp, ecx
or_bp_dx
	or_reg16_reg16		ebp, edx
or_bp_bx
	or_reg16_reg16		ebp, ebx
or_bp_sp
	or_reg16_reg16		ebp, esp
or_bp_bp
	or_reg16_reg16		ebp, ebp
or_bp_si
	or_reg16_reg16		ebp, esi
or_bp_di
	or_reg16_reg16		ebp, edi
or_si_ax
	or_reg16_reg16		esi, eax
or_si_cx
	or_reg16_reg16		esi, ecx
or_si_dx
	or_reg16_reg16		esi, edx
or_si_bx
	or_reg16_reg16		esi, ebx
or_si_sp
	or_reg16_reg16		esi, esp
or_si_bp
	or_reg16_reg16		esi, ebp
or_si_si
	or_reg16_reg16		esi, esi
or_si_di
	or_reg16_reg16		esi, edi
or_di_ax
	or_reg16_reg16		edi, eax
or_di_cx
	or_reg16_reg16		edi, ecx
or_di_dx
	or_reg16_reg16		edi, edx
or_di_bx
	or_reg16_reg16		edi, ebx
or_di_sp
	or_reg16_reg16		edi, esp
or_di_bp
	or_reg16_reg16		edi, ebp
or_di_si
	or_reg16_reg16		edi, esi
or_di_di
	or_reg16_reg16		edi, edi

	GLOBAL  or_ax_ax
	GLOBAL  or_ax_cx
	GLOBAL  or_ax_dx
	GLOBAL  or_ax_bx
	GLOBAL  or_ax_sp
	GLOBAL  or_ax_bp
	GLOBAL  or_ax_si
	GLOBAL  or_ax_di
	GLOBAL  or_cx_ax
	GLOBAL  or_cx_cx
	GLOBAL  or_cx_dx
	GLOBAL  or_cx_bx
	GLOBAL  or_cx_sp
	GLOBAL  or_cx_bp
	GLOBAL  or_cx_si
	GLOBAL  or_cx_di
	GLOBAL  or_dx_ax
	GLOBAL  or_dx_cx
	GLOBAL  or_dx_dx
	GLOBAL  or_dx_bx
	GLOBAL  or_dx_sp
	GLOBAL  or_dx_bp
	GLOBAL  or_dx_si
	GLOBAL  or_dx_di
	GLOBAL  or_bx_ax
	GLOBAL  or_bx_cx
	GLOBAL  or_bx_dx
	GLOBAL  or_bx_bx
	GLOBAL  or_bx_sp
	GLOBAL  or_bx_bp
	GLOBAL  or_bx_si
	GLOBAL  or_bx_di
	GLOBAL  or_sp_ax
	GLOBAL  or_sp_cx
	GLOBAL  or_sp_dx
	GLOBAL  or_sp_bx
	GLOBAL  or_sp_sp
	GLOBAL  or_sp_bp
	GLOBAL  or_sp_si
	GLOBAL  or_sp_di
	GLOBAL  or_bp_ax
	GLOBAL  or_bp_cx
	GLOBAL  or_bp_dx
	GLOBAL  or_bp_bx
	GLOBAL  or_bp_sp
	GLOBAL  or_bp_bp
	GLOBAL  or_bp_si
	GLOBAL  or_bp_di
	GLOBAL  or_si_ax
	GLOBAL  or_si_cx
	GLOBAL  or_si_dx
	GLOBAL  or_si_bx
	GLOBAL  or_si_sp
	GLOBAL  or_si_bp
	GLOBAL  or_si_si
	GLOBAL  or_si_di
	GLOBAL  or_di_ax
	GLOBAL  or_di_cx
	GLOBAL  or_di_dx
	GLOBAL  or_di_bx
	GLOBAL  or_di_sp
	GLOBAL  or_di_bp
	GLOBAL  or_di_si
	GLOBAL  or_di_di


; ------------------- 0C = OR AL,imm8 --------------------------------
op_0c
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r0,[r12],#1				; Load imm8 to r0, increment r12 by 1
	mov		r1, eax, lsl #24
	orrs	r1, r0, lsl #24
	bic		eax, #0xFF
	orr		eax, r1, lsr #24
	b		loop

; ------------------- 0D = OR AX,imm16 --------------------------------
op_0d
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	lsl		r1, eax, #16
	eor		eax, r1, lsr #16
	orrs	r0, r1, r0, lsl #16
	orr		eax, r0, lsr #16
	b		loop

; ------------------- 0E = PUSH CS ------------------------------------
op_0e
	ldr		r0, [sp, #SP_CS_VALUE]	; r0 = Current logical CS
	push_hword r0, r1, r2
	b		loop

	LTORG							; Dump the current literal pool here

	ALIGN	4

; ------------------- 0F = protected mode opcodes ---------------------
; See "cpu_prot.s"


; ------------------- 10 = ADC r/m8, esp -------------------------------
;
; All modrm variations supported!
;
;
op_10
	modrm_jump_16_tbl op_10_jump
	modrm_tbl_0	adc
	DCD adc_al_al, adc_cl_al, adc_dl_al, adc_bl_al, adc_ah_al, adc_ch_al, adc_dh_al, adc_bh_al
	DCD adc_al_cl, adc_cl_cl, adc_dl_cl, adc_bl_cl, adc_ah_cl, adc_ch_cl, adc_dh_cl, adc_bh_cl
	DCD adc_al_dl, adc_cl_dl, adc_dl_dl, adc_bl_dl, adc_ah_dl, adc_ch_dl, adc_dh_dl, adc_bh_dl
	DCD adc_al_bl, adc_cl_bl, adc_dl_bl, adc_bl_bl, adc_ah_bl, adc_ch_bl, adc_dh_bl, adc_bh_bl
	DCD adc_al_ah, adc_cl_ah, adc_dl_ah, adc_bl_ah, adc_ah_ah, adc_ch_ah, adc_dh_ah, adc_bh_ah
	DCD adc_al_ch, adc_cl_ch, adc_dl_ch, adc_bl_ch, adc_ah_ch, adc_ch_ch, adc_dh_ch, adc_bh_ch
	DCD adc_al_dh, adc_cl_dh, adc_dl_dh, adc_bl_dh, adc_ah_dh, adc_ch_dh, adc_dh_dh, adc_bh_dh
	DCD adc_al_bh, adc_cl_bh, adc_dl_bh, adc_bl_bh, adc_ah_bh, adc_ch_bh, adc_dh_bh, adc_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	adc_r0_reg8l $reg
	GLOBAL	adc_r0_r8l_bp_$reg
adc_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r0_r8l_$reg
adc_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_10_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_10_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	lsl		r0, #24
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, $reg, lsl #24		; Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to [physical segment + disp16]
	b		loop
	MEND
	MACRO 
	adc_r0_reg8h $reg
	GLOBAL	adc_r0_r8h_bp_$reg
adc_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r0_r8h_$reg
adc_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_10_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_10_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r0, #24
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to RAM
	b		loop
	MEND

	adc_r0_reg8l eax
	adc_r0_reg8l ecx
	adc_r0_reg8l edx
	adc_r0_reg8l ebx
	adc_r0_reg8h eax
	adc_r0_reg8h ecx
	adc_r0_reg8h edx
	adc_r0_reg8h ebx

	LTORG

	modrm_0_genall adc


; ------------------- 11 = ADC r/m16, r16 -----------------------------
;
; All modrm variations supported!
;
;
op_11
	modrm_jump_16_tbl op_11_jump
	modrm_tbl_1_old adc
	DCD adc_ax_ax, adc_cx_ax, adc_dx_ax, adc_bx_ax, adc_sp_ax, adc_bp_ax, adc_si_ax, adc_di_ax
	DCD adc_ax_cx, adc_cx_cx, adc_dx_cx, adc_bx_cx, adc_sp_cx, adc_bp_cx, adc_si_cx, adc_di_cx
	DCD adc_ax_dx, adc_cx_dx, adc_dx_dx, adc_bx_dx, adc_sp_dx, adc_bp_dx, adc_si_dx, adc_di_dx
	DCD adc_ax_bx, adc_cx_bx, adc_dx_bx, adc_bx_bx, adc_sp_bx, adc_bp_bx, adc_si_bx, adc_di_bx
	DCD adc_ax_sp, adc_cx_sp, adc_dx_sp, adc_bx_sp, adc_sp_sp, adc_bp_sp, adc_si_sp, adc_di_sp
	DCD adc_ax_bp, adc_cx_bp, adc_dx_bp, adc_bx_bp, adc_sp_bp, adc_bp_bp, adc_si_bp, adc_di_bp
	DCD adc_ax_si, adc_cx_si, adc_dx_si, adc_bx_si, adc_sp_si, adc_bp_si, adc_si_si, adc_di_si
	DCD adc_ax_di, adc_cx_di, adc_dx_di, adc_bx_di, adc_sp_di, adc_bp_di, adc_si_di, adc_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	adc_r0_r16_reg $reg
	GLOBAL	adc_r0_r16_bp_$reg
adc_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r0_r16_$reg
adc_r0_r16_$reg
	mem_handler_jump_r0r3 op_11_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_11_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	lsl		r0, #16
	orr		r0, r1, lsl #24			; r0 = low byte | (high byte << 8)
	addcs	r0, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, $reg, lsl #16		; Perform the actual addition, setting the resulting flags.
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop
	MEND

	adc_r0_r16_reg eax
	adc_r0_r16_reg ecx
	adc_r0_r16_reg edx
	adc_r0_r16_reg ebx
	adc_r0_r16_reg esp
	adc_r0_r16_reg ebp
	adc_r0_r16_reg esi
	adc_r0_r16_reg edi

	LTORG

	modrm_1_genall adc, adc_r0_r16


; ------------------- 12 = ADC esp, r/m8 -------------------------------
;
; All modrm variations supported!
;
;
op_12
	modrm_jump_16_tbl op_12_jump
	modrm_tbl_2 adc
	DCD adc_al_al, adc_al_cl, adc_al_dl, adc_al_bl, adc_al_ah, adc_al_ch, adc_al_dh, adc_al_bh
	DCD adc_cl_al, adc_cl_cl, adc_cl_dl, adc_cl_bl, adc_cl_ah, adc_cl_ch, adc_cl_dh, adc_cl_bh
	DCD adc_dl_al, adc_dl_cl, adc_dl_dl, adc_dl_bl, adc_dl_ah, adc_dl_ch, adc_dl_dh, adc_dl_bh
	DCD adc_bl_al, adc_bl_cl, adc_bl_dl, adc_bl_bl, adc_bl_ah, adc_bl_ch, adc_bl_dh, adc_bl_bh
	DCD adc_ah_al, adc_ah_cl, adc_ah_dl, adc_ah_bl, adc_ah_ah, adc_ah_ch, adc_ah_dh, adc_ah_bh
	DCD adc_ch_al, adc_ch_cl, adc_ch_dl, adc_ch_bl, adc_ch_ah, adc_ch_ch, adc_ch_dh, adc_ch_bh
	DCD adc_dh_al, adc_dh_cl, adc_dh_dl, adc_dh_bl, adc_dh_ah, adc_dh_ch, adc_dh_dh, adc_dh_bh
	DCD adc_bh_al, adc_bh_cl, adc_bh_dl, adc_bh_bl, adc_bh_ah, adc_bh_ch, adc_bh_dh, adc_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	adc_reg8l_r0high $reg
	GLOBAL	adc_r8l_r0_bp_$reg
adc_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r8l_r0_$reg
adc_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_12_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_12_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	lsl		r0, #24
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, $reg, lsl #24		; Perform the actual addition, setting the resulting flags.
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r0, lsr #24		; Put the result to the lowest byte of the left register
	b		loop
	MEND
	MACRO 
	adc_reg8h_r0high $reg
	GLOBAL	adc_r8h_r0_bp_$reg
adc_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r8h_r0_$reg
adc_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_12_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_12_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00		; Left operand already uses just the rightmost byte
	lsl		r0, #24
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	adc_reg8l_r0high eax
	adc_reg8l_r0high ecx
	adc_reg8l_r0high edx
	adc_reg8l_r0high ebx
	adc_reg8h_r0high eax
	adc_reg8h_r0high ecx
	adc_reg8h_r0high edx
	adc_reg8h_r0high ebx

	LTORG
	
	modrm_2_genall adc


; --- Register operands ---

	MACRO 
	adc_reg8l_reg8l $rl, $rr
	mov		r0, $rl, lsl #24		; r0 high byte = the left operand
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, $rr, lsl #24		; Perform the actual addition, setting the resulting flags.
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		loop
	MEND
	MACRO 
	adc_reg8l_reg8h $rl, $rr
	and		r1, $rr, #0xFF00		; r1 high byte = the right operand
	mov		r0, $rl, lsl #24		; r0 high byte = the left operand
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		loop
	MEND
	MACRO 
	adc_reg8h_reg8l $rl, $rr
	mov		r0, $rr, lsl #24		; r1 high byte = the right operand
	and		r1, $rl, #0xFF00		; r0 high byte = the left operand
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; Put the result to the high byte of the high halfword of the left register
	b		loop
	MEND
	MACRO 
	adc_reg8h_reg8h $rl, $rr
	and		r1, $rr, #0xFF00		; r1 high byte = the right operand
	and		r0, $rl, #0xFF00		; r0 high byte = the left operand
	lsl		r0, #16
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; Put the result to the high byte of the high halfword of the left register
	b		loop
	MEND

adc_al_al
	adc_reg8l_reg8l eax, eax
adc_al_cl
	adc_reg8l_reg8l eax, ecx
adc_al_dl
	adc_reg8l_reg8l eax, edx
adc_al_bl
	adc_reg8l_reg8l eax, ebx
adc_al_ah
	adc_reg8l_reg8h eax, eax
adc_al_ch
	adc_reg8l_reg8h eax, ecx
adc_al_dh
	adc_reg8l_reg8h eax, edx
adc_al_bh
	adc_reg8l_reg8h eax, ebx
adc_cl_al
	adc_reg8l_reg8l ecx, eax
adc_cl_cl
	adc_reg8l_reg8l ecx, ecx
adc_cl_dl
	adc_reg8l_reg8l ecx, edx
adc_cl_bl
	adc_reg8l_reg8l ecx, ebx
adc_cl_ah
	adc_reg8l_reg8h ecx, eax
adc_cl_ch
	adc_reg8l_reg8h ecx, ecx
adc_cl_dh
	adc_reg8l_reg8h ecx, edx
adc_cl_bh
	adc_reg8l_reg8h ecx, ebx
adc_dl_al
	adc_reg8l_reg8l edx, eax
adc_dl_cl
	adc_reg8l_reg8l edx, ecx
adc_dl_dl
	adc_reg8l_reg8l edx, edx
adc_dl_bl
	adc_reg8l_reg8l edx, ebx
adc_dl_ah
	adc_reg8l_reg8h edx, eax
adc_dl_ch
	adc_reg8l_reg8h edx, ecx
adc_dl_dh
	adc_reg8l_reg8h edx, edx
adc_dl_bh
	adc_reg8l_reg8h edx, ebx
adc_bl_al
	adc_reg8l_reg8l ebx, eax
adc_bl_cl
	adc_reg8l_reg8l ebx, ecx
adc_bl_dl
	adc_reg8l_reg8l ebx, edx
adc_bl_bl
	adc_reg8l_reg8l ebx, ebx
adc_bl_ah
	adc_reg8l_reg8h ebx, eax
adc_bl_ch
	adc_reg8l_reg8h ebx, ecx
adc_bl_dh
	adc_reg8l_reg8h ebx, edx
adc_bl_bh
	adc_reg8l_reg8h ebx, ebx

adc_ah_al
	adc_reg8h_reg8l eax, eax
adc_ah_cl
	adc_reg8h_reg8l eax, ecx
adc_ah_dl
	adc_reg8h_reg8l eax, edx
adc_ah_bl
	adc_reg8h_reg8l eax, ebx
adc_ah_ah
	adc_reg8h_reg8h eax, eax
adc_ah_ch
	adc_reg8h_reg8h eax, ecx
adc_ah_dh
	adc_reg8h_reg8h eax, edx
adc_ah_bh
	adc_reg8h_reg8h eax, ebx
adc_ch_al
	adc_reg8h_reg8l ecx, eax
adc_ch_cl
	adc_reg8h_reg8l ecx, ecx
adc_ch_dl
	adc_reg8h_reg8l ecx, edx
adc_ch_bl
	adc_reg8h_reg8l ecx, ebx
adc_ch_ah
	adc_reg8h_reg8h ecx, eax
adc_ch_ch
	adc_reg8h_reg8h ecx, ecx
adc_ch_dh
	adc_reg8h_reg8h ecx, edx
adc_ch_bh
	adc_reg8h_reg8h ecx, ebx
adc_dh_al
	adc_reg8h_reg8l edx, eax
adc_dh_cl
	adc_reg8h_reg8l edx, ecx
adc_dh_dl
	adc_reg8h_reg8l edx, edx
adc_dh_bl
	adc_reg8h_reg8l edx, ebx
adc_dh_ah
	adc_reg8h_reg8h edx, eax
adc_dh_ch
	adc_reg8h_reg8h edx, ecx
adc_dh_dh
	adc_reg8h_reg8h edx, edx
adc_dh_bh
	adc_reg8h_reg8h edx, ebx
adc_bh_al
	adc_reg8h_reg8l ebx, eax
adc_bh_cl
	adc_reg8h_reg8l ebx, ecx
adc_bh_dl
	adc_reg8h_reg8l ebx, edx
adc_bh_bl
	adc_reg8h_reg8l ebx, ebx
adc_bh_ah
	adc_reg8h_reg8h ebx, eax
adc_bh_ch
	adc_reg8h_reg8h ebx, ecx
adc_bh_dh
	adc_reg8h_reg8h ebx, edx
adc_bh_bh
	adc_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  adc_al_al
	GLOBAL  adc_al_cl
	GLOBAL  adc_al_dl
	GLOBAL  adc_al_bl
	GLOBAL  adc_al_ah
	GLOBAL  adc_al_ch
	GLOBAL  adc_al_dh
	GLOBAL  adc_al_bh
	GLOBAL  adc_cl_al
	GLOBAL  adc_cl_cl
	GLOBAL  adc_cl_dl
	GLOBAL  adc_cl_bl
	GLOBAL  adc_cl_ah
	GLOBAL  adc_cl_ch
	GLOBAL  adc_cl_dh
	GLOBAL  adc_cl_bh
	GLOBAL  adc_dl_al
	GLOBAL  adc_dl_cl
	GLOBAL  adc_dl_dl
	GLOBAL  adc_dl_bl
	GLOBAL  adc_dl_ah
	GLOBAL  adc_dl_ch
	GLOBAL  adc_dl_dh
	GLOBAL  adc_dl_bh
	GLOBAL  adc_bl_al
	GLOBAL  adc_bl_cl
	GLOBAL  adc_bl_dl
	GLOBAL  adc_bl_bl
	GLOBAL  adc_bl_ah
	GLOBAL  adc_bl_ch
	GLOBAL  adc_bl_dh
	GLOBAL  adc_bl_bh
	GLOBAL  adc_ah_al
	GLOBAL  adc_ah_cl
	GLOBAL  adc_ah_dl
	GLOBAL  adc_ah_bl
	GLOBAL  adc_ah_ah
	GLOBAL  adc_ah_ch
	GLOBAL  adc_ah_dh
	GLOBAL  adc_ah_bh
	GLOBAL  adc_ch_al
	GLOBAL  adc_ch_cl
	GLOBAL  adc_ch_dl
	GLOBAL  adc_ch_bl
	GLOBAL  adc_ch_ah
	GLOBAL  adc_ch_ch
	GLOBAL  adc_ch_dh
	GLOBAL  adc_ch_bh
	GLOBAL  adc_dh_al
	GLOBAL  adc_dh_cl
	GLOBAL  adc_dh_dl
	GLOBAL  adc_dh_bl
	GLOBAL  adc_dh_ah
	GLOBAL  adc_dh_ch
	GLOBAL  adc_dh_dh
	GLOBAL  adc_dh_bh
	GLOBAL  adc_bh_al
	GLOBAL  adc_bh_cl
	GLOBAL  adc_bh_dl
	GLOBAL  adc_bh_bl
	GLOBAL  adc_bh_ah
	GLOBAL  adc_bh_ch
	GLOBAL  adc_bh_dh
	GLOBAL  adc_bh_bh

; ------------------- 13 = adc r16, r/m16 ------------------------------
;
; All modrm variations supported!
;
;
op_13
	modrm_jump_16_tbl op_13_jump
	modrm_tbl_3_old adc
	DCD adc_ax_ax, adc_ax_cx, adc_ax_dx, adc_ax_bx, adc_ax_sp, adc_ax_bp, adc_ax_si, adc_ax_di
	DCD adc_cx_ax, adc_cx_cx, adc_cx_dx, adc_cx_bx, adc_cx_sp, adc_cx_bp, adc_cx_si, adc_cx_di
	DCD adc_dx_ax, adc_dx_cx, adc_dx_dx, adc_dx_bx, adc_dx_sp, adc_dx_bp, adc_dx_si, adc_dx_di
	DCD adc_bx_ax, adc_bx_cx, adc_bx_dx, adc_bx_bx, adc_bx_sp, adc_bx_bp, adc_bx_si, adc_bx_di
	DCD adc_sp_ax, adc_sp_cx, adc_sp_dx, adc_sp_bx, adc_sp_sp, adc_sp_bp, adc_sp_si, adc_sp_di
	DCD adc_bp_ax, adc_bp_cx, adc_bp_dx, adc_bp_bx, adc_bp_sp, adc_bp_bp, adc_bp_si, adc_bp_di
	DCD adc_si_ax, adc_si_cx, adc_si_dx, adc_si_bx, adc_si_sp, adc_si_bp, adc_si_si, adc_si_di
	DCD adc_di_ax, adc_di_cx, adc_di_dx, adc_di_bx, adc_di_sp, adc_di_bp, adc_di_si, adc_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	adc_reg16_r0high $reg
	GLOBAL	adc_r16_r0_bp_$reg
adc_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	adc_r16_r0_$reg
adc_r16_r0_$reg
	mem_handler_jump_r0r3 op_13_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory adcress
	;-------
op_13_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eor		$reg, r2, lsr #16
	addcs	r2, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r2, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r2, r0, lsl #16		; Perform the actual addition, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	adc_reg16_r0high eax
	adc_reg16_r0high ecx
	adc_reg16_r0high edx
	adc_reg16_r0high ebx
	adc_reg16_r0high esp
	adc_reg16_r0high ebp
	adc_reg16_r0high esi
	adc_reg16_r0high edi

	LTORG

	modrm_3_genall_old adc, adc_r16_r0


; --- registers ---

	MACRO 
	adc_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16		; r0 = left operand in high halfword
	mov		r1, $rr, lsl #16		; r1 = right operand in high halfword
	eor		$rl, r0, lsr #16
	addcs	r0, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1					; Perform the actual addition, setting the resulting flags.
	orr		$rl, r0, lsr #16
	b		loop
	MEND

adc_ax_ax
	adc_reg16_reg16		eax, eax
adc_ax_cx
	adc_reg16_reg16		eax, ecx
adc_ax_dx
	adc_reg16_reg16		eax, edx
adc_ax_bx
	adc_reg16_reg16		eax, ebx
adc_ax_sp
	adc_reg16_reg16		eax, esp
adc_ax_bp
	adc_reg16_reg16		eax, ebp
adc_ax_si
	adc_reg16_reg16		eax, esi
adc_ax_di
	adc_reg16_reg16		eax, edi
adc_cx_ax
	adc_reg16_reg16		ecx, eax
adc_cx_cx
	adc_reg16_reg16		ecx, ecx
adc_cx_dx
	adc_reg16_reg16		ecx, edx
adc_cx_bx
	adc_reg16_reg16		ecx, ebx
adc_cx_sp
	adc_reg16_reg16		ecx, esp
adc_cx_bp
	adc_reg16_reg16		ecx, ebp
adc_cx_si
	adc_reg16_reg16		ecx, esi
adc_cx_di
	adc_reg16_reg16		ecx, edi
adc_dx_ax
	adc_reg16_reg16		edx, eax
adc_dx_cx
	adc_reg16_reg16		edx, ecx
adc_dx_dx
	adc_reg16_reg16		edx, edx
adc_dx_bx
	adc_reg16_reg16		edx, ebx
adc_dx_sp
	adc_reg16_reg16		edx, esp
adc_dx_bp
	adc_reg16_reg16		edx, ebp
adc_dx_si
	adc_reg16_reg16		edx, esi
adc_dx_di
	adc_reg16_reg16		edx, edi
adc_bx_ax
	adc_reg16_reg16		ebx, eax
adc_bx_cx
	adc_reg16_reg16		ebx, ecx
adc_bx_dx
	adc_reg16_reg16		ebx, edx
adc_bx_bx
	adc_reg16_reg16		ebx, ebx
adc_bx_sp
	adc_reg16_reg16		ebx, esp
adc_bx_bp
	adc_reg16_reg16		ebx, ebp
adc_bx_si
	adc_reg16_reg16		ebx, esi
adc_bx_di
	adc_reg16_reg16		ebx, edi
adc_sp_ax
	adc_reg16_reg16		esp, eax
adc_sp_cx
	adc_reg16_reg16		esp, ecx
adc_sp_dx
	adc_reg16_reg16		esp, edx
adc_sp_bx
	adc_reg16_reg16		esp, ebx
adc_sp_sp
	adc_reg16_reg16		esp, esp
adc_sp_bp
	adc_reg16_reg16		esp, ebp
adc_sp_si
	adc_reg16_reg16		esp, esi
adc_sp_di
	adc_reg16_reg16		esp, edi
adc_bp_ax
	adc_reg16_reg16		ebp, eax
adc_bp_cx
	adc_reg16_reg16		ebp, ecx
adc_bp_dx
	adc_reg16_reg16		ebp, edx
adc_bp_bx
	adc_reg16_reg16		ebp, ebx
adc_bp_sp
	adc_reg16_reg16		ebp, esp
adc_bp_bp
	adc_reg16_reg16		ebp, ebp
adc_bp_si
	adc_reg16_reg16		ebp, esi
adc_bp_di
	adc_reg16_reg16		ebp, edi
adc_si_ax
	adc_reg16_reg16		esi, eax
adc_si_cx
	adc_reg16_reg16		esi, ecx
adc_si_dx
	adc_reg16_reg16		esi, edx
adc_si_bx
	adc_reg16_reg16		esi, ebx
adc_si_sp
	adc_reg16_reg16		esi, esp
adc_si_bp
	adc_reg16_reg16		esi, ebp
adc_si_si
	adc_reg16_reg16		esi, esi
adc_si_di
	adc_reg16_reg16		esi, edi
adc_di_ax
	adc_reg16_reg16		edi, eax
adc_di_cx
	adc_reg16_reg16		edi, ecx
adc_di_dx
	adc_reg16_reg16		edi, edx
adc_di_bx
	adc_reg16_reg16		edi, ebx
adc_di_sp
	adc_reg16_reg16		edi, esp
adc_di_bp
	adc_reg16_reg16		edi, ebp
adc_di_si
	adc_reg16_reg16		edi, esi
adc_di_di
	adc_reg16_reg16		edi, edi

	GLOBAL  adc_ax_ax
	GLOBAL  adc_ax_cx
	GLOBAL  adc_ax_dx
	GLOBAL  adc_ax_bx
	GLOBAL  adc_ax_sp
	GLOBAL  adc_ax_bp
	GLOBAL  adc_ax_si
	GLOBAL  adc_ax_di
	GLOBAL  adc_cx_ax
	GLOBAL  adc_cx_cx
	GLOBAL  adc_cx_dx
	GLOBAL  adc_cx_bx
	GLOBAL  adc_cx_sp
	GLOBAL  adc_cx_bp
	GLOBAL  adc_cx_si
	GLOBAL  adc_cx_di
	GLOBAL  adc_dx_ax
	GLOBAL  adc_dx_cx
	GLOBAL  adc_dx_dx
	GLOBAL  adc_dx_bx
	GLOBAL  adc_dx_sp
	GLOBAL  adc_dx_bp
	GLOBAL  adc_dx_si
	GLOBAL  adc_dx_di
	GLOBAL  adc_bx_ax
	GLOBAL  adc_bx_cx
	GLOBAL  adc_bx_dx
	GLOBAL  adc_bx_bx
	GLOBAL  adc_bx_sp
	GLOBAL  adc_bx_bp
	GLOBAL  adc_bx_si
	GLOBAL  adc_bx_di
	GLOBAL  adc_sp_ax
	GLOBAL  adc_sp_cx
	GLOBAL  adc_sp_dx
	GLOBAL  adc_sp_bx
	GLOBAL  adc_sp_sp
	GLOBAL  adc_sp_bp
	GLOBAL  adc_sp_si
	GLOBAL  adc_sp_di
	GLOBAL  adc_bp_ax
	GLOBAL  adc_bp_cx
	GLOBAL  adc_bp_dx
	GLOBAL  adc_bp_bx
	GLOBAL  adc_bp_sp
	GLOBAL  adc_bp_bp
	GLOBAL  adc_bp_si
	GLOBAL  adc_bp_di
	GLOBAL  adc_si_ax
	GLOBAL  adc_si_cx
	GLOBAL  adc_si_dx
	GLOBAL  adc_si_bx
	GLOBAL  adc_si_sp
	GLOBAL  adc_si_bp
	GLOBAL  adc_si_si
	GLOBAL  adc_si_di
	GLOBAL  adc_di_ax
	GLOBAL  adc_di_cx
	GLOBAL  adc_di_dx
	GLOBAL  adc_di_bx
	GLOBAL  adc_di_sp
	GLOBAL  adc_di_bp
	GLOBAL  adc_di_si
	GLOBAL  adc_di_di


; ------------------- 14 = ADC AL,imm8 --------------------------------
op_14
	ldrb	r1,[r12],#1				; Load low byte to r1, increment r12 by 1
	lsl		r1, #24
	addcs	r1, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, eax, lsl #24	; Perform the actual addition, setting the resulting flags.
	bic		eax, #0xFF				; Clear the current AL value
	orr		eax, r0, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		loop

; ------------------- 15 = ADC AX,imm16 -------------------------------
;
op_15
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	lsl		r0, #16
	orr		r0, r1, lsl #24			; r0 = low byte | (high byte << 8)
	mov		r1, eax, lsl #16
	eor		eax, r1, lsr #16
	addcs	r0, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1					; Perform the actual addition, setting the resulting flags.
	orr		eax, r0, lsr #16
	b		loop

; ------------------- 16 = PUSH SS ------------------------------------
op_16
	ldr		r0, [sp, #SP_SS_VALUE]
	push_hword r0, r1, r2
	b		loop

; ------------------- 17 = POP SS -------------------------------------
op_17
	pop_reg_low_tmp	r2, r1
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_ss_r0r2_prot					; Yes we are, go handle protected mode version!
	msr		cpsr_f, r0							; Restore flags
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	str		r2, [sp, #SP_SS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_SS_BASE]
	mov		lr, r2
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]
	;-------
	; NOTE! x86 disables interrupts until the next instruction has been executed.
	; Thus we must handle the next opcode immediately!
	;-------
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; r2 high halfword = logical DS segment, clear segment override flags
	ldr		pc,[sp, r0, lsl #2]					; Jump to the opcode handler

	EXTERN	mov_ss_r0r2_prot

	LTORG
	
	ALIGN	4

; ------------------- 18 = SBB r/m8, esp -----------------------------
;
; All modrm variations supported!
;
;
op_18
	modrm_jump_16_tbl op_18_jump
	modrm_tbl_0	sbb
	DCD sbb_al_al, sbb_cl_al, sbb_dl_al, sbb_bl_al, sbb_ah_al, sbb_ch_al, sbb_dh_al, sbb_bh_al
	DCD sbb_al_cl, sbb_cl_cl, sbb_dl_cl, sbb_bl_cl, sbb_ah_cl, sbb_ch_cl, sbb_dh_cl, sbb_bh_cl
	DCD sbb_al_dl, sbb_cl_dl, sbb_dl_dl, sbb_bl_dl, sbb_ah_dl, sbb_ch_dl, sbb_dh_dl, sbb_bh_dl
	DCD sbb_al_bl, sbb_cl_bl, sbb_dl_bl, sbb_bl_bl, sbb_ah_bl, sbb_ch_bl, sbb_dh_bl, sbb_bh_bl
	DCD sbb_al_ah, sbb_cl_ah, sbb_dl_ah, sbb_bl_ah, sbb_ah_ah, sbb_ch_ah, sbb_dh_ah, sbb_bh_ah
	DCD sbb_al_ch, sbb_cl_ch, sbb_dl_ch, sbb_bl_ch, sbb_ah_ch, sbb_ch_ch, sbb_dh_ch, sbb_bh_ch
	DCD sbb_al_dh, sbb_cl_dh, sbb_dl_dh, sbb_bl_dh, sbb_ah_dh, sbb_ch_dh, sbb_dh_dh, sbb_bh_dh
	DCD sbb_al_bh, sbb_cl_bh, sbb_dl_bh, sbb_bl_bh, sbb_ah_bh, sbb_ch_bh, sbb_dh_bh, sbb_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	;-------
	; SBB with input carry set, we need to calculate the proper flags manually.
	; On input
	;	r0 = byte from RAM
	;	r1 = register value
	;	r2 = RAM address
	;	r3 = free
	;-------
op_18_sbb_carry_r1
	sub		r3, r0, r1				; r2 = lf_var1d - lf_var2d
	sub		r3, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r3, r3, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	lsr		r3, #24
	strb	r3,[r2]					; Store byte to RAM
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0
	eor		r3, r0
	and		r1, r3
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0

	MACRO 
	sbb_r0_reg8l $reg
	GLOBAL	sbb_r0_r8l_bp_$reg
sbb_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r0_r8l_$reg
sbb_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_18_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_18_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	bcs		%f1
	lsl		r0, #24
	subs	r0, $reg, lsl #24		; Perform the actual subtraction, setting the resulting flags.
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to [physical segment + disp16]
	b		complement_carry
1	and		r1, $reg, #0xFF
	b		op_18_sbb_carry_r1
	MEND
	MACRO 
	sbb_r0_reg8h $reg
	GLOBAL	sbb_r0_r8h_bp_$reg
sbb_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r0_r8h_$reg
sbb_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_18_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_18_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	bcs		%f1
	lsl		r0, #24
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to RAM
	b		complement_carry
1	lsr		r1, #8
	b		op_18_sbb_carry_r1
	MEND

	sbb_r0_reg8l eax
	sbb_r0_reg8l ecx
	sbb_r0_reg8l edx
	sbb_r0_reg8l ebx
	sbb_r0_reg8h eax
	sbb_r0_reg8h ecx
	sbb_r0_reg8h edx
	sbb_r0_reg8h ebx

	LTORG

	modrm_0_genall sbb


; ------------------- 19 = SBB r/m16, r16 -----------------------------
;
; All modrm variations supported!
;
;
op_19
	modrm_jump_16_tbl op_19_jump
	modrm_tbl_1_old sbb
	DCD sbb_ax_ax, sbb_cx_ax, sbb_dx_ax, sbb_bx_ax, sbb_sp_ax, sbb_bp_ax, sbb_si_ax, sbb_di_ax
	DCD sbb_ax_cx, sbb_cx_cx, sbb_dx_cx, sbb_bx_cx, sbb_sp_cx, sbb_bp_cx, sbb_si_cx, sbb_di_cx
	DCD sbb_ax_dx, sbb_cx_dx, sbb_dx_dx, sbb_bx_dx, sbb_sp_dx, sbb_bp_dx, sbb_si_dx, sbb_di_dx
	DCD sbb_ax_bx, sbb_cx_bx, sbb_dx_bx, sbb_bx_bx, sbb_sp_bx, sbb_bp_bx, sbb_si_bx, sbb_di_bx
	DCD sbb_ax_sp, sbb_cx_sp, sbb_dx_sp, sbb_bx_sp, sbb_sp_sp, sbb_bp_sp, sbb_si_sp, sbb_di_sp
	DCD sbb_ax_bp, sbb_cx_bp, sbb_dx_bp, sbb_bx_bp, sbb_sp_bp, sbb_bp_bp, sbb_si_bp, sbb_di_bp
	DCD sbb_ax_si, sbb_cx_si, sbb_dx_si, sbb_bx_si, sbb_sp_si, sbb_bp_si, sbb_si_si, sbb_di_si
	DCD sbb_ax_di, sbb_cx_di, sbb_dx_di, sbb_bx_di, sbb_sp_di, sbb_bp_di, sbb_si_di, sbb_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	;-------
	; SBB with input carry set, we need to calculate the proper flags manually.
	; On input
	;	r0 = halfword from RAM
	;	r1 = register value in high 16 bits
	;	r2 = RAM address
	;	r3 = free
	;-------
op_19_sbb_carry_r1
	sub		r3, r0, r1, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r3, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r3, r3, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	lsr		r3, #16
	strb	r3,[r2]					; Store byte to RAM
	ror		r3, #8
	strb	r3,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, r1, lsr #16
	eor		r3, r0, r3, ror #24
	and		r1, r3
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0

	MACRO 
	sbb_r0_r16_reg $reg
	GLOBAL	sbb_r0_r16_bp_$reg
sbb_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r0_r16_$reg
sbb_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_19_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_19_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	bcs		%f1						; If input carry is set, we need to calculate the flags manually.
	lsl		r0, #16
	orr		r0, r1, lsl #24			; r0 = low byte | (high byte << 8)
	subs	r0, $reg, lsl #16		; Perform the actual subtraction, setting the resulting flags.
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		complement_carry
1	orr		r0, r1, lsl #8
	mov		r1, $reg, lsl #16
	b		op_19_sbb_carry_r1
	MEND

	sbb_r0_r16_reg eax
	sbb_r0_r16_reg ecx
	sbb_r0_r16_reg edx
	sbb_r0_r16_reg ebx
	sbb_r0_r16_reg esp
	sbb_r0_r16_reg ebp
	sbb_r0_r16_reg esi
	sbb_r0_r16_reg edi

	LTORG

	modrm_1_genall sbb, sbb_r0_r16


; ------------------- 1A = SBB r8, r/m8 -------------------------------
;
; All modrm variations supported!
;
;
op_1a
	modrm_jump_16_tbl op_1a_jump
	modrm_tbl_2 sbb
	DCD sbb_al_al, sbb_al_cl, sbb_al_dl, sbb_al_bl, sbb_al_ah, sbb_al_ch, sbb_al_dh, sbb_al_bh
	DCD sbb_cl_al, sbb_cl_cl, sbb_cl_dl, sbb_cl_bl, sbb_cl_ah, sbb_cl_ch, sbb_cl_dh, sbb_cl_bh
	DCD sbb_dl_al, sbb_dl_cl, sbb_dl_dl, sbb_dl_bl, sbb_dl_ah, sbb_dl_ch, sbb_dl_dh, sbb_dl_bh
	DCD sbb_bl_al, sbb_bl_cl, sbb_bl_dl, sbb_bl_bl, sbb_bl_ah, sbb_bl_ch, sbb_bl_dh, sbb_bl_bh
	DCD sbb_ah_al, sbb_ah_cl, sbb_ah_dl, sbb_ah_bl, sbb_ah_ah, sbb_ah_ch, sbb_ah_dh, sbb_ah_bh
	DCD sbb_ch_al, sbb_ch_cl, sbb_ch_dl, sbb_ch_bl, sbb_ch_ah, sbb_ch_ch, sbb_ch_dh, sbb_ch_bh
	DCD sbb_dh_al, sbb_dh_cl, sbb_dh_dl, sbb_dh_bl, sbb_dh_ah, sbb_dh_ch, sbb_dh_dh, sbb_dh_bh
	DCD sbb_bh_al, sbb_bh_cl, sbb_bh_dl, sbb_bh_bl, sbb_bh_ah, sbb_bh_ch, sbb_bh_dh, sbb_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sbb_reg8l_r0high $reg
	GLOBAL	sbb_r8l_r0_bp_$reg
sbb_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r8l_r0_$reg
sbb_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_1a_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_1a_RAM_l_$reg
	ldrb	r1, [r2] 				; Load byte to r0
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	mov		r0, $reg, lsl #24
	subs	r0, r1, lsl #24			; Perform the actual subtraction, setting the resulting flags.
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r0, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	and		r0, $reg, #0xFF
	sub		r2, r0, r1				; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r2, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0
	eor		r2, r0, r2, lsr #24
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND
	MACRO 
	sbb_reg8h_r0high $reg
	GLOBAL	sbb_r8h_r0_bp_$reg
sbb_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r8h_r0_$reg
sbb_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_1a_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_1a_RAM_h_$reg
	ldrb	r1, [r2] 				; Load byte to r0
	and		r0, $reg, #0xFF00		; Left operand already uses just the rightmost byte
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	lsl		r0, #16
	subs	r0, r1, lsl #24			; Perform the actual subtraction, setting the resulting flags.
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r0, lsr #16
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	rsb		r2, r1, r0, lsr #8		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r2, lsr #16		; Put the result to the correct byte of the register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, lsr #8
	eor		r2, r0, lsl #16
	and		r1, r2, lsr #24
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	sbb_reg8l_r0high eax
	sbb_reg8l_r0high ecx
	sbb_reg8l_r0high edx
	sbb_reg8l_r0high ebx
	sbb_reg8h_r0high eax
	sbb_reg8h_r0high ecx
	sbb_reg8h_r0high edx
	sbb_reg8h_r0high ebx

	LTORG

	modrm_2_genall sbb


; --- registers ---

	MACRO 
	sbb_reg8l_reg8l $rl, $rr
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	mov		r0, $rl, lsl #24		; Put left operand to r0 high byte
	subs	r0, $rr, lsl #24		; Perform the actual subtraction, setting the resulting flags.
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; and put the result there
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	and		r0, $rl, #0xFF	
	and		r1, $rr, #0xFF
	sub		r2, r0, r1				; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r2, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0
	eor		r2, r0, r2, lsr #24
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	MACRO 
	sbb_reg8l_reg8h $rl, $rr
	and		r1, $rr, #0xFF00		; Put the right operand into r1 high byte
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	mov		r0, $rl, lsl #24		; Put left operand to r0 high byte
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r0, lsr #24		; and put the result there
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	and		r0, $rl, #0xFF	
	sub		r2, r0, r1, lsr #8		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$rl, #0xFF				; Clear the current reg8l value
	orr		$rl, r2, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, r1, lsr #8
	eor		r2, r0, r2, lsr #24
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	MACRO 
	sbb_reg8h_reg8l $rl, $rr
	and		r0, $rl, #0xFF00		; Put left operand to r0 high byte
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	lsl		r0, #16					; Put left operand to r0 high byte
	subs	r0, $rr, lsl #24		; Perform the actual subtraction, setting the resulting flags.
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; and put the result there
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	and		r1, $rr, #0xFF	
	rsb		r2, r1, r0, lsr #8		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$rl, #0xFF00			; Clear the current reg8l value
	orr		$rl, r2, lsr #16		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, lsr #8
	eor		r2, r0, lsl #16
	and		r1, r2, lsr #24
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	MACRO 
	sbb_reg8h_reg8h $rl, $rr
	and		r0, $rl, #0xFF00		; Put left operand to r0 high byte
	and		r1, $rr, #0xFF00		; Put the right operand into r1 high byte
	bcs		%f1						; If input carry is set, we need to determine the flags ourselves.
	lsl		r0, #16
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	bic		$rl, #0xFF00			; Clear the current reg8h value
	orr		$rl, r0, lsr #16		; and put the result there
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	sub		r2, r0, r1				; r2 = lf_var1d - lf_var2d
	sub		r2, #0x100				; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$rl, #0xFF00			; Clear the current reg8l value
	orr		$rl, r2, lsr #16		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0
	eor		r2, r0, lsl #16
	and		r1, r2, lsr #16
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

sbb_al_al
	sbb_reg8l_reg8l eax, eax
sbb_al_cl
	sbb_reg8l_reg8l eax, ecx
sbb_al_dl
	sbb_reg8l_reg8l eax, edx
sbb_al_bl
	sbb_reg8l_reg8l eax, ebx
sbb_al_ah
	sbb_reg8l_reg8h eax, eax
sbb_al_ch
	sbb_reg8l_reg8h eax, ecx
sbb_al_dh
	sbb_reg8l_reg8h eax, edx
sbb_al_bh
	sbb_reg8l_reg8h eax, ebx
sbb_cl_al
	sbb_reg8l_reg8l ecx, eax
sbb_cl_cl
	sbb_reg8l_reg8l ecx, ecx
sbb_cl_dl
	sbb_reg8l_reg8l ecx, edx
sbb_cl_bl
	sbb_reg8l_reg8l ecx, ebx
sbb_cl_ah
	sbb_reg8l_reg8h ecx, eax
sbb_cl_ch
	sbb_reg8l_reg8h ecx, ecx
sbb_cl_dh
	sbb_reg8l_reg8h ecx, edx
sbb_cl_bh
	sbb_reg8l_reg8h ecx, ebx
sbb_dl_al
	sbb_reg8l_reg8l edx, eax
sbb_dl_cl
	sbb_reg8l_reg8l edx, ecx
sbb_dl_dl
	sbb_reg8l_reg8l edx, edx
sbb_dl_bl
	sbb_reg8l_reg8l edx, ebx
sbb_dl_ah
	sbb_reg8l_reg8h edx, eax
sbb_dl_ch
	sbb_reg8l_reg8h edx, ecx
sbb_dl_dh
	sbb_reg8l_reg8h edx, edx
sbb_dl_bh
	sbb_reg8l_reg8h edx, ebx
sbb_bl_al
	sbb_reg8l_reg8l ebx, eax
sbb_bl_cl
	sbb_reg8l_reg8l ebx, ecx
sbb_bl_dl
	sbb_reg8l_reg8l ebx, edx
sbb_bl_bl
	sbb_reg8l_reg8l ebx, ebx
sbb_bl_ah
	sbb_reg8l_reg8h ebx, eax
sbb_bl_ch
	sbb_reg8l_reg8h ebx, ecx
sbb_bl_dh
	sbb_reg8l_reg8h ebx, edx
sbb_bl_bh
	sbb_reg8l_reg8h ebx, ebx

sbb_ah_al
	sbb_reg8h_reg8l eax, eax
sbb_ah_cl
	sbb_reg8h_reg8l eax, ecx
sbb_ah_dl
	sbb_reg8h_reg8l eax, edx
sbb_ah_bl
	sbb_reg8h_reg8l eax, ebx
sbb_ah_ah
	sbb_reg8h_reg8h eax, eax
sbb_ah_ch
	sbb_reg8h_reg8h eax, ecx
sbb_ah_dh
	sbb_reg8h_reg8h eax, edx
sbb_ah_bh
	sbb_reg8h_reg8h eax, ebx
sbb_ch_al
	sbb_reg8h_reg8l ecx, eax
sbb_ch_cl
	sbb_reg8h_reg8l ecx, ecx
sbb_ch_dl
	sbb_reg8h_reg8l ecx, edx
sbb_ch_bl
	sbb_reg8h_reg8l ecx, ebx
sbb_ch_ah
	sbb_reg8h_reg8h ecx, eax
sbb_ch_ch
	sbb_reg8h_reg8h ecx, ecx
sbb_ch_dh
	sbb_reg8h_reg8h ecx, edx
sbb_ch_bh
	sbb_reg8h_reg8h ecx, ebx
sbb_dh_al
	sbb_reg8h_reg8l edx, eax
sbb_dh_cl
	sbb_reg8h_reg8l edx, ecx
sbb_dh_dl
	sbb_reg8h_reg8l edx, edx
sbb_dh_bl
	sbb_reg8h_reg8l edx, ebx
sbb_dh_ah
	sbb_reg8h_reg8h edx, eax
sbb_dh_ch
	sbb_reg8h_reg8h edx, ecx
sbb_dh_dh
	sbb_reg8h_reg8h edx, edx
sbb_dh_bh
	sbb_reg8h_reg8h edx, ebx
sbb_bh_al
	sbb_reg8h_reg8l ebx, eax
sbb_bh_cl
	sbb_reg8h_reg8l ebx, ecx
sbb_bh_dl
	sbb_reg8h_reg8l ebx, edx
sbb_bh_bl
	sbb_reg8h_reg8l ebx, ebx
sbb_bh_ah
	sbb_reg8h_reg8h ebx, eax
sbb_bh_ch
	sbb_reg8h_reg8h ebx, ecx
sbb_bh_dh
	sbb_reg8h_reg8h ebx, edx
sbb_bh_bh
	sbb_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  sbb_al_al
	GLOBAL  sbb_al_cl
	GLOBAL  sbb_al_dl
	GLOBAL  sbb_al_bl
	GLOBAL  sbb_al_ah
	GLOBAL  sbb_al_ch
	GLOBAL  sbb_al_dh
	GLOBAL  sbb_al_bh
	GLOBAL  sbb_cl_al
	GLOBAL  sbb_cl_cl
	GLOBAL  sbb_cl_dl
	GLOBAL  sbb_cl_bl
	GLOBAL  sbb_cl_ah
	GLOBAL  sbb_cl_ch
	GLOBAL  sbb_cl_dh
	GLOBAL  sbb_cl_bh
	GLOBAL  sbb_dl_al
	GLOBAL  sbb_dl_cl
	GLOBAL  sbb_dl_dl
	GLOBAL  sbb_dl_bl
	GLOBAL  sbb_dl_ah
	GLOBAL  sbb_dl_ch
	GLOBAL  sbb_dl_dh
	GLOBAL  sbb_dl_bh
	GLOBAL  sbb_bl_al
	GLOBAL  sbb_bl_cl
	GLOBAL  sbb_bl_dl
	GLOBAL  sbb_bl_bl
	GLOBAL  sbb_bl_ah
	GLOBAL  sbb_bl_ch
	GLOBAL  sbb_bl_dh
	GLOBAL  sbb_bl_bh
	GLOBAL  sbb_ah_al
	GLOBAL  sbb_ah_cl
	GLOBAL  sbb_ah_dl
	GLOBAL  sbb_ah_bl
	GLOBAL  sbb_ah_ah
	GLOBAL  sbb_ah_ch
	GLOBAL  sbb_ah_dh
	GLOBAL  sbb_ah_bh
	GLOBAL  sbb_ch_al
	GLOBAL  sbb_ch_cl
	GLOBAL  sbb_ch_dl
	GLOBAL  sbb_ch_bl
	GLOBAL  sbb_ch_ah
	GLOBAL  sbb_ch_ch
	GLOBAL  sbb_ch_dh
	GLOBAL  sbb_ch_bh
	GLOBAL  sbb_dh_al
	GLOBAL  sbb_dh_cl
	GLOBAL  sbb_dh_dl
	GLOBAL  sbb_dh_bl
	GLOBAL  sbb_dh_ah
	GLOBAL  sbb_dh_ch
	GLOBAL  sbb_dh_dh
	GLOBAL  sbb_dh_bh
	GLOBAL  sbb_bh_al
	GLOBAL  sbb_bh_cl
	GLOBAL  sbb_bh_dl
	GLOBAL  sbb_bh_bl
	GLOBAL  sbb_bh_ah
	GLOBAL  sbb_bh_ch
	GLOBAL  sbb_bh_dh
	GLOBAL  sbb_bh_bh
	
; ------------------- %b1 = SBB r16, r/m16 -----------------------------
;
; All modrm variations supported!
;
;
op_1b
	modrm_jump_16_tbl op_1b_jump
	modrm_tbl_3_old sbb
	DCD sbb_ax_ax, sbb_ax_cx, sbb_ax_dx, sbb_ax_bx, sbb_ax_sp, sbb_ax_bp, sbb_ax_si, sbb_ax_di
	DCD sbb_cx_ax, sbb_cx_cx, sbb_cx_dx, sbb_cx_bx, sbb_cx_sp, sbb_cx_bp, sbb_cx_si, sbb_cx_di
	DCD sbb_dx_ax, sbb_dx_cx, sbb_dx_dx, sbb_dx_bx, sbb_dx_sp, sbb_dx_bp, sbb_dx_si, sbb_dx_di
	DCD sbb_bx_ax, sbb_bx_cx, sbb_bx_dx, sbb_bx_bx, sbb_bx_sp, sbb_bx_bp, sbb_bx_si, sbb_bx_di
	DCD sbb_sp_ax, sbb_sp_cx, sbb_sp_dx, sbb_sp_bx, sbb_sp_sp, sbb_sp_bp, sbb_sp_si, sbb_sp_di
	DCD sbb_bp_ax, sbb_bp_cx, sbb_bp_dx, sbb_bp_bx, sbb_bp_sp, sbb_bp_bp, sbb_bp_si, sbb_bp_di
	DCD sbb_si_ax, sbb_si_cx, sbb_si_dx, sbb_si_bx, sbb_si_sp, sbb_si_bp, sbb_si_si, sbb_si_di
	DCD sbb_di_ax, sbb_di_cx, sbb_di_dx, sbb_di_bx, sbb_di_sp, sbb_di_bp, sbb_di_si, sbb_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sbb_reg16_r0high $reg
	GLOBAL	sbb_r16_r0_bp_$reg
sbb_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sbb_r16_r0_$reg
sbb_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_1b_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_1b_RAM_$reg
	ldrb	r1, [r2] 				; Load low byte
	ldrb	r2, [r2, #1]			; Load high byte
	mov		r0, $reg, lsl #16
	orr		r1, r2, lsl #8			; r1 = low byte | (high byte << 8)
	eor		$reg, r0, lsr #16
	bcs		%f1						; Calculate the flags separately if input carry is set.
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	rsb		r2, r1, r0, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	orr		$reg, r2, lsr #16
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, lsr #16
	eor		r2, r0
	and		r1, r2, lsr #16
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	sbb_reg16_r0high eax
	sbb_reg16_r0high ecx
	sbb_reg16_r0high edx
	sbb_reg16_r0high ebx
	sbb_reg16_r0high esp
	sbb_reg16_r0high ebp
	sbb_reg16_r0high esi
	sbb_reg16_r0high edi

	LTORG

	modrm_3_genall_old sbb, sbb_r16_r0


; --- registers ---

	MACRO 
	sbb_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	mov		r1, $rr, lsl #16		; r1 = right operand at high halfword
	eor		$rl, r0, lsr #16
	bcs		%f1
	subs	r0, r1					; Perform the actual subtraction, setting the resulting flags.
	orr		$rl, r0, lsr #16
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	lsr		r0, #16
	sub		r2, r0, r1, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	orr		$rl, r2, lsr #16
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, r1, lsr #16
	eor		r2, r0, r2, lsr #16
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND
	
sbb_ax_ax
	sbb_reg16_reg16		eax, eax
sbb_ax_cx
	sbb_reg16_reg16		eax, ecx
sbb_ax_dx
	sbb_reg16_reg16		eax, edx
sbb_ax_bx
	sbb_reg16_reg16		eax, ebx
sbb_ax_sp
	sbb_reg16_reg16		eax, esp
sbb_ax_bp
	sbb_reg16_reg16		eax, ebp
sbb_ax_si
	sbb_reg16_reg16		eax, esi
sbb_ax_di
	sbb_reg16_reg16		eax, edi
sbb_cx_ax
	sbb_reg16_reg16		ecx, eax
sbb_cx_cx
	sbb_reg16_reg16		ecx, ecx
sbb_cx_dx
	sbb_reg16_reg16		ecx, edx
sbb_cx_bx
	sbb_reg16_reg16		ecx, ebx
sbb_cx_sp
	sbb_reg16_reg16		ecx, esp
sbb_cx_bp
	sbb_reg16_reg16		ecx, ebp
sbb_cx_si
	sbb_reg16_reg16		ecx, esi
sbb_cx_di
	sbb_reg16_reg16		ecx, edi
sbb_dx_ax
	sbb_reg16_reg16		edx, eax
sbb_dx_cx
	sbb_reg16_reg16		edx, ecx
sbb_dx_dx
	sbb_reg16_reg16		edx, edx
sbb_dx_bx
	sbb_reg16_reg16		edx, ebx
sbb_dx_sp
	sbb_reg16_reg16		edx, esp
sbb_dx_bp
	sbb_reg16_reg16		edx, ebp
sbb_dx_si
	sbb_reg16_reg16		edx, esi
sbb_dx_di
	sbb_reg16_reg16		edx, edi
sbb_bx_ax
	sbb_reg16_reg16		ebx, eax
sbb_bx_cx
	sbb_reg16_reg16		ebx, ecx
sbb_bx_dx
	sbb_reg16_reg16		ebx, edx
sbb_bx_bx
	sbb_reg16_reg16		ebx, ebx
sbb_bx_sp
	sbb_reg16_reg16		ebx, esp
sbb_bx_bp
	sbb_reg16_reg16		ebx, ebp
sbb_bx_si
	sbb_reg16_reg16		ebx, esi
sbb_bx_di
	sbb_reg16_reg16		ebx, edi
sbb_sp_ax
	sbb_reg16_reg16		esp, eax
sbb_sp_cx
	sbb_reg16_reg16		esp, ecx
sbb_sp_dx
	sbb_reg16_reg16		esp, edx
sbb_sp_bx
	sbb_reg16_reg16		esp, ebx
sbb_sp_sp
	sbb_reg16_reg16		esp, esp
sbb_sp_bp
	sbb_reg16_reg16		esp, ebp
sbb_sp_si
	sbb_reg16_reg16		esp, esi
sbb_sp_di
	sbb_reg16_reg16		esp, edi
sbb_bp_ax
	sbb_reg16_reg16		ebp, eax
sbb_bp_cx
	sbb_reg16_reg16		ebp, ecx
sbb_bp_dx
	sbb_reg16_reg16		ebp, edx
sbb_bp_bx
	sbb_reg16_reg16		ebp, ebx
sbb_bp_sp
	sbb_reg16_reg16		ebp, esp
sbb_bp_bp
	sbb_reg16_reg16		ebp, ebp
sbb_bp_si
	sbb_reg16_reg16		ebp, esi
sbb_bp_di
	sbb_reg16_reg16		ebp, edi
sbb_si_ax
	sbb_reg16_reg16		esi, eax
sbb_si_cx
	sbb_reg16_reg16		esi, ecx
sbb_si_dx
	sbb_reg16_reg16		esi, edx
sbb_si_bx
	sbb_reg16_reg16		esi, ebx
sbb_si_sp
	sbb_reg16_reg16		esi, esp
sbb_si_bp
	sbb_reg16_reg16		esi, ebp
sbb_si_si
	sbb_reg16_reg16		esi, esi
sbb_si_di
	sbb_reg16_reg16		esi, edi
sbb_di_ax
	sbb_reg16_reg16		edi, eax
sbb_di_cx
	sbb_reg16_reg16		edi, ecx
sbb_di_dx
	sbb_reg16_reg16		edi, edx
sbb_di_bx
	sbb_reg16_reg16		edi, ebx
sbb_di_sp
	sbb_reg16_reg16		edi, esp
sbb_di_bp
	sbb_reg16_reg16		edi, ebp
sbb_di_si
	sbb_reg16_reg16		edi, esi
sbb_di_di
	sbb_reg16_reg16		edi, edi

	GLOBAL  sbb_ax_ax
	GLOBAL  sbb_ax_cx
	GLOBAL  sbb_ax_dx
	GLOBAL  sbb_ax_bx
	GLOBAL  sbb_ax_sp
	GLOBAL  sbb_ax_bp
	GLOBAL  sbb_ax_si
	GLOBAL  sbb_ax_di
	GLOBAL  sbb_cx_ax
	GLOBAL  sbb_cx_cx
	GLOBAL  sbb_cx_dx
	GLOBAL  sbb_cx_bx
	GLOBAL  sbb_cx_sp
	GLOBAL  sbb_cx_bp
	GLOBAL  sbb_cx_si
	GLOBAL  sbb_cx_di
	GLOBAL  sbb_dx_ax
	GLOBAL  sbb_dx_cx
	GLOBAL  sbb_dx_dx
	GLOBAL  sbb_dx_bx
	GLOBAL  sbb_dx_sp
	GLOBAL  sbb_dx_bp
	GLOBAL  sbb_dx_si
	GLOBAL  sbb_dx_di
	GLOBAL  sbb_bx_ax
	GLOBAL  sbb_bx_cx
	GLOBAL  sbb_bx_dx
	GLOBAL  sbb_bx_bx
	GLOBAL  sbb_bx_sp
	GLOBAL  sbb_bx_bp
	GLOBAL  sbb_bx_si
	GLOBAL  sbb_bx_di
	GLOBAL  sbb_sp_ax
	GLOBAL  sbb_sp_cx
	GLOBAL  sbb_sp_dx
	GLOBAL  sbb_sp_bx
	GLOBAL  sbb_sp_sp
	GLOBAL  sbb_sp_bp
	GLOBAL  sbb_sp_si
	GLOBAL  sbb_sp_di
	GLOBAL  sbb_bp_ax
	GLOBAL  sbb_bp_cx
	GLOBAL  sbb_bp_dx
	GLOBAL  sbb_bp_bx
	GLOBAL  sbb_bp_sp
	GLOBAL  sbb_bp_bp
	GLOBAL  sbb_bp_si
	GLOBAL  sbb_bp_di
	GLOBAL  sbb_si_ax
	GLOBAL  sbb_si_cx
	GLOBAL  sbb_si_dx
	GLOBAL  sbb_si_bx
	GLOBAL  sbb_si_sp
	GLOBAL  sbb_si_bp
	GLOBAL  sbb_si_si
	GLOBAL  sbb_si_di
	GLOBAL  sbb_di_ax
	GLOBAL  sbb_di_cx
	GLOBAL  sbb_di_dx
	GLOBAL  sbb_di_bx
	GLOBAL  sbb_di_sp
	GLOBAL  sbb_di_bp
	GLOBAL  sbb_di_si
	GLOBAL  sbb_di_di


; ------------------- 1C = SBB AL,imm8 --------------------------------
;
op_1c
	ldrb	r1,[r12],#1				; Load low byte to r1, increment r12 by 1
	bcs		%f1						; If input Carry is set, we need to perform a more complex operation to get correct flags.
	mov		r0, eax, lsl #24		; r0 high byte = the left operand
	subs	r0, r1, lsl #24			; Perform the actual subtraction, setting the resulting flags.
	bic		eax, #0xFF				; Clear the current AL value
	orr		eax, r0, lsr #24		; Put the result to the lower byte of the left register
	b		complement_carry
	;-------
	; SBB with input carry set, we need to calculate the proper flags manually.
	;-------
1	and		r0, eax, #0xFF
	sub		r2, r0, r1				; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		eax, #0xFF				; Clear the current AL value
	orr		eax, r2, lsr #24		; Put the result to the lower byte of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0
	eor		r2, r0, r2, lsr #24
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	

; ------------------- 1D = SBB AX,imm16 -------------------------------
;
	GLOBAL op_1d
op_1d
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r0, increment r12 by 1
	bcs		%f1
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	mov		r1, eax, lsl #16
	eor		eax, r1, lsr #16
	subs	r1, r0, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	orr		eax, r1, lsr #16
	b		complement_carry
	;-------
	; SBB with input carry set, we need to calculate the proper flags manually.
	;-------
1	orr		r1, r0, r1, lsl #8		; r1 = low byte | (high byte << 8)
	mov		r0, eax, lsl #16
	eor		eax, r0, lsr #16		; Clear the current AX value
	rsb		r2, r1, r0, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	orr		eax, r2, lsr #16		; Put the result to the lower 16 bits of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, lsr #16			; r1 in low 16bit, r0 in high 16bit, result to low 16bit
	eor		r0, r2					; Both in high 16bit, result to high 16bit
	and		r1, r0, lsr #16			; r1 in low 16bit, r0 in high 16bit, result to low 16bit
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	
; ------------------- 1E = PUSH DS ------------------------------------
op_1e
	ldr		r1, [sp, #SP_DS_VALUE]	
	push_hword r1, r0, r2
	b		loop

; ------------------- %1 = POP DS -------------------------------------
; Profiler: 14467, 18, 26.62, 385083, 0.2%
;
op_1f
	pop_reg_low_tmp	r2, r1			; NOTE! When using paging, we can not pop the value before checking for exception!
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		mov_ds_r0r2_prot		; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	lsl		r1, r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_DS_VALUE]
	str		r1, [sp, #SP_DS_BASE]	
	b		restore_flags_from_r0				; Go back to the opcode loop, restoring flags

	LTORG								; Dump the current literal pool here

	EXTERN	mov_ds_r0r2_prot

	ALIGN	4

; ------------------- 20 = AND r/m8,esp --------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_20
op_20
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_20_jump
	modrm_tbl_0	and
	DCD and_al_al, and_cl_al, and_dl_al, and_bl_al, and_ah_al, and_ch_al, and_dh_al, and_bh_al
	DCD and_al_cl, and_cl_cl, and_dl_cl, and_bl_cl, and_ah_cl, and_ch_cl, and_dh_cl, and_bh_cl
	DCD and_al_dl, and_cl_dl, and_dl_dl, and_bl_dl, and_ah_dl, and_ch_dl, and_dh_dl, and_bh_dl
	DCD and_al_bl, and_cl_bl, and_dl_bl, and_bl_bl, and_ah_bl, and_ch_bl, and_dh_bl, and_bh_bl
	DCD and_al_ah, and_cl_ah, and_dl_ah, and_bl_ah, and_ah_ah, and_ch_ah, and_dh_ah, and_bh_ah
	DCD and_al_ch, and_cl_ch, and_dl_ch, and_bl_ch, and_ah_ch, and_ch_ch, and_dh_ch, and_bh_ch
	DCD and_al_dh, and_cl_dh, and_dl_dh, and_bl_dh, and_ah_dh, and_ch_dh, and_dh_dh, and_bh_dh
	DCD and_al_bh, and_cl_bh, and_dl_bh, and_bl_bh, and_ah_bh, and_ch_bh, and_dh_bh, and_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	and_r0_reg8l $reg
	EXTERN	op_20_EGA_l_$reg
	EXTERN	op_20_MODEX_l_$reg
	GLOBAL	and_r0_r8l_bp_$reg
and_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r0_r8l_$reg
and_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_20_RAM_l_$reg, op_20_EGA_l_$reg, op_20_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_20_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	ands	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND
	MACRO 
	and_r0_reg8h $reg
	EXTERN	op_20_EGA_h_$reg
	EXTERN	op_20_MODEX_h_$reg
	GLOBAL	and_r0_r8h_bp_$reg
and_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r0_r8h_$reg
and_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_20_RAM_h_$reg, op_20_EGA_h_$reg, op_20_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_20_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #16		; No need to clear the reg8l value, as the AND will do that for us.
	ands	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND

	and_r0_reg8l eax
	and_r0_reg8l ecx
	and_r0_reg8l edx
	and_r0_reg8l ebx
	and_r0_reg8h eax
	and_r0_reg8h ecx
	and_r0_reg8h edx
	and_r0_reg8h ebx

	LTORG

	modrm_0_genall and


; ------------------- 21 = AND r/m16,r16 ------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL op_21
op_21
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_21_jump
	modrm_tbl_1_old and
	DCD and_ax_ax, and_cx_ax, and_dx_ax, and_bx_ax, and_sp_ax, and_bp_ax, and_si_ax, and_di_ax
	DCD and_ax_cx, and_cx_cx, and_dx_cx, and_bx_cx, and_sp_cx, and_bp_cx, and_si_cx, and_di_cx
	DCD and_ax_dx, and_cx_dx, and_dx_dx, and_bx_dx, and_sp_dx, and_bp_dx, and_si_dx, and_di_dx
	DCD and_ax_bx, and_cx_bx, and_dx_bx, and_bx_bx, and_sp_bx, and_bp_bx, and_si_bx, and_di_bx
	DCD and_ax_sp, and_cx_sp, and_dx_sp, and_bx_sp, and_sp_sp, and_bp_sp, and_si_sp, and_di_sp
	DCD and_ax_bp, and_cx_bp, and_dx_bp, and_bx_bp, and_sp_bp, and_bp_bp, and_si_bp, and_di_bp
	DCD and_ax_si, and_cx_si, and_dx_si, and_bx_si, and_sp_si, and_bp_si, and_si_si, and_di_si
	DCD and_ax_di, and_cx_di, and_dx_di, and_bx_di, and_sp_di, and_bp_di, and_si_di, and_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	and_r0_r16_reg $reg
	EXTERN	op_21_EGA_r2_$reg
	GLOBAL	and_r0_r16_bp_$reg
and_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r0_r16_$reg
and_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_21_RAM_$reg, op_21_EGA_r2_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_21_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r3, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	ands	r0, r3, r0, lsl #16
	lsr		r0, #16
	strb	r0, [r2]				; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0, [r2, #1]			; Store high byte to [physical segment + disp16 + 1]
	b		loop
	MEND

	and_r0_r16_reg eax
	and_r0_r16_reg ecx
	and_r0_r16_reg edx
	and_r0_r16_reg ebx
	and_r0_r16_reg esp
	and_r0_r16_reg ebp
	and_r0_r16_reg esi
	and_r0_r16_reg edi

	LTORG
	
	modrm_1_genall and, and_r0_r16


; ------------------- 22 = AND r8,r/m8 --------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_22
op_22
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_22_jump
	modrm_tbl_2 and
	DCD and_al_al, and_al_cl, and_al_dl, and_al_bl, and_al_ah, and_al_ch, and_al_dh, and_al_bh
	DCD and_cl_al, and_cl_cl, and_cl_dl, and_cl_bl, and_cl_ah, and_cl_ch, and_cl_dh, and_cl_bh
	DCD and_dl_al, and_dl_cl, and_dl_dl, and_dl_bl, and_dl_ah, and_dl_ch, and_dl_dh, and_dl_bh
	DCD and_bl_al, and_bl_cl, and_bl_dl, and_bl_bl, and_bl_ah, and_bl_ch, and_bl_dh, and_bl_bh
	DCD and_ah_al, and_ah_cl, and_ah_dl, and_ah_bl, and_ah_ah, and_ah_ch, and_ah_dh, and_ah_bh
	DCD and_ch_al, and_ch_cl, and_ch_dl, and_ch_bl, and_ch_ah, and_ch_ch, and_ch_dh, and_ch_bh
	DCD and_dh_al, and_dh_cl, and_dh_dl, and_dh_bl, and_dh_ah, and_dh_ch, and_dh_dh, and_dh_bh
	DCD and_bh_al, and_bh_cl, and_bh_dl, and_bh_bl, and_bh_ah, and_bh_ch, and_bh_dh, and_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	and_reg8l_r0high $reg
	EXTERN	op_22_EGA_l_$reg
	EXTERN	op_22_MODEX_l_$reg
	GLOBAL	and_r8l_r0_bp_$reg
and_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r8l_r0_$reg
and_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_22_RAM_l_$reg, op_22_EGA_l_$reg, op_22_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_22_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	ands	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		loop
	MEND
	MACRO 
	and_reg8h_r0high $reg
	EXTERN	op_22_EGA_h_$reg
	EXTERN	op_22_MODEX_h_$reg
	GLOBAL	and_r8h_r0_bp_$reg
and_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r8h_r0_$reg
and_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_22_RAM_h_$reg, op_22_EGA_h_$reg, op_22_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_22_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #16		; No need to clear the reg8l value from r1, as the AND will do that for us.
	ands	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16
	b		loop
	MEND

	and_reg8l_r0high eax
	and_reg8l_r0high ecx
	and_reg8l_r0high edx
	and_reg8l_r0high ebx
	and_reg8h_r0high eax
	and_reg8h_r0high ecx
	and_reg8h_r0high edx
	and_reg8h_r0high ebx

	LTORG

	modrm_2_genall and


; ----- registers -----

	MACRO 
	and_reg8l_reg8l $reg1, $reg2
	mov		r0, $reg1, lsl #24
	mov		r1, $reg2, lsl #24
	ands	r0, r1
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		loop
	MEND

	MACRO 
	and_reg8l_reg8h $reg1, $reg2
	mov		r0, $reg1, lsl #24
	mov		r1, $reg2, lsl #16		; No need to clear the reg8l value from r1, as the AND will do that for us.
	ands	r0, r1
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		loop
	MEND

	MACRO 
	and_reg8h_reg8l $reg1, $reg2
	mov		r0, $reg1, lsl #16		; No need to clear the reg8l value from r1, as the AND will do that for us.
	mov		r1, $reg2, lsl #24
	ands	r0, r1
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		loop
	MEND

	MACRO 
	and_reg8h_reg8h $reg1, $reg2
	and		r0, $reg1, #0xFF00
	lsl		r0, #16
	mov		r1, $reg2, lsl #16
	ands	r0, r1
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		loop
	MEND

and_al_al
	and_reg8l_reg8l eax, eax
and_al_cl
	and_reg8l_reg8l eax, ecx
and_al_dl
	and_reg8l_reg8l eax, edx
and_al_bl
	and_reg8l_reg8l eax, ebx
and_al_ah
	and_reg8l_reg8h eax, eax
and_al_ch
	and_reg8l_reg8h eax, ecx
and_al_dh
	and_reg8l_reg8h eax, edx
and_al_bh
	and_reg8l_reg8h eax, ebx
and_cl_al
	and_reg8l_reg8l ecx, eax
and_cl_cl
	and_reg8l_reg8l ecx, ecx
and_cl_dl
	and_reg8l_reg8l ecx, edx
and_cl_bl
	and_reg8l_reg8l ecx, ebx
and_cl_ah
	and_reg8l_reg8h ecx, eax
and_cl_ch
	and_reg8l_reg8h ecx, ecx
and_cl_dh
	and_reg8l_reg8h ecx, edx
and_cl_bh
	and_reg8l_reg8h ecx, ebx
and_dl_al
	and_reg8l_reg8l edx, eax
and_dl_cl
	and_reg8l_reg8l edx, ecx
and_dl_dl
	and_reg8l_reg8l edx, edx
and_dl_bl
	and_reg8l_reg8l edx, ebx
and_dl_ah
	and_reg8l_reg8h edx, eax
and_dl_ch
	and_reg8l_reg8h edx, ecx
and_dl_dh
	and_reg8l_reg8h edx, edx
and_dl_bh
	and_reg8l_reg8h edx, ebx
and_bl_al
	and_reg8l_reg8l ebx, eax
and_bl_cl
	and_reg8l_reg8l ebx, ecx
and_bl_dl
	and_reg8l_reg8l ebx, edx
and_bl_bl
	and_reg8l_reg8l ebx, ebx
and_bl_ah
	and_reg8l_reg8h ebx, eax
and_bl_ch
	and_reg8l_reg8h ebx, ecx
and_bl_dh
	and_reg8l_reg8h ebx, edx
and_bl_bh
	and_reg8l_reg8h ebx, ebx

and_ah_al
	and_reg8h_reg8l eax, eax
and_ah_cl
	and_reg8h_reg8l eax, ecx
and_ah_dl
	and_reg8h_reg8l eax, edx
and_ah_bl
	and_reg8h_reg8l eax, ebx
and_ah_ah
	and_reg8h_reg8h eax, eax
and_ah_ch
	and_reg8h_reg8h eax, ecx
and_ah_dh
	and_reg8h_reg8h eax, edx
and_ah_bh
	and_reg8h_reg8h eax, ebx
and_ch_al
	and_reg8h_reg8l ecx, eax
and_ch_cl
	and_reg8h_reg8l ecx, ecx
and_ch_dl
	and_reg8h_reg8l ecx, edx
and_ch_bl
	and_reg8h_reg8l ecx, ebx
and_ch_ah
	and_reg8h_reg8h ecx, eax
and_ch_ch
	and_reg8h_reg8h ecx, ecx
and_ch_dh
	and_reg8h_reg8h ecx, edx
and_ch_bh
	and_reg8h_reg8h ecx, ebx
and_dh_al
	and_reg8h_reg8l edx, eax
and_dh_cl
	and_reg8h_reg8l edx, ecx
and_dh_dl
	and_reg8h_reg8l edx, edx
and_dh_bl
	and_reg8h_reg8l edx, ebx
and_dh_ah
	and_reg8h_reg8h edx, eax
and_dh_ch
	and_reg8h_reg8h edx, ecx
and_dh_dh
	and_reg8h_reg8h edx, edx
and_dh_bh
	and_reg8h_reg8h edx, ebx
and_bh_al
	and_reg8h_reg8l ebx, eax
and_bh_cl
	and_reg8h_reg8l ebx, ecx
and_bh_dl
	and_reg8h_reg8l ebx, edx
and_bh_bl
	and_reg8h_reg8l ebx, ebx
and_bh_ah
	and_reg8h_reg8h ebx, eax
and_bh_ch
	and_reg8h_reg8h ebx, ecx
and_bh_dh
	and_reg8h_reg8h ebx, edx
and_bh_bh
	and_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  and_al_al
	GLOBAL  and_al_cl
	GLOBAL  and_al_dl
	GLOBAL  and_al_bl
	GLOBAL  and_al_ah
	GLOBAL  and_al_ch
	GLOBAL  and_al_dh
	GLOBAL  and_al_bh
	GLOBAL  and_cl_al
	GLOBAL  and_cl_cl
	GLOBAL  and_cl_dl
	GLOBAL  and_cl_bl
	GLOBAL  and_cl_ah
	GLOBAL  and_cl_ch
	GLOBAL  and_cl_dh
	GLOBAL  and_cl_bh
	GLOBAL  and_dl_al
	GLOBAL  and_dl_cl
	GLOBAL  and_dl_dl
	GLOBAL  and_dl_bl
	GLOBAL  and_dl_ah
	GLOBAL  and_dl_ch
	GLOBAL  and_dl_dh
	GLOBAL  and_dl_bh
	GLOBAL  and_bl_al
	GLOBAL  and_bl_cl
	GLOBAL  and_bl_dl
	GLOBAL  and_bl_bl
	GLOBAL  and_bl_ah
	GLOBAL  and_bl_ch
	GLOBAL  and_bl_dh
	GLOBAL  and_bl_bh
	GLOBAL  and_ah_al
	GLOBAL  and_ah_cl
	GLOBAL  and_ah_dl
	GLOBAL  and_ah_bl
	GLOBAL  and_ah_ah
	GLOBAL  and_ah_ch
	GLOBAL  and_ah_dh
	GLOBAL  and_ah_bh
	GLOBAL  and_ch_al
	GLOBAL  and_ch_cl
	GLOBAL  and_ch_dl
	GLOBAL  and_ch_bl
	GLOBAL  and_ch_ah
	GLOBAL  and_ch_ch
	GLOBAL  and_ch_dh
	GLOBAL  and_ch_bh
	GLOBAL  and_dh_al
	GLOBAL  and_dh_cl
	GLOBAL  and_dh_dl
	GLOBAL  and_dh_bl
	GLOBAL  and_dh_ah
	GLOBAL  and_dh_ch
	GLOBAL  and_dh_dh
	GLOBAL  and_dh_bh
	GLOBAL  and_bh_al
	GLOBAL  and_bh_cl
	GLOBAL  and_bh_dl
	GLOBAL  and_bh_bl
	GLOBAL  and_bh_ah
	GLOBAL  and_bh_ch
	GLOBAL  and_bh_dh
	GLOBAL  and_bh_bh

; ------------------- 23 = AND r16,r/m16 ------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
;
	GLOBAL	op_23
op_23
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_23_jump
	modrm_tbl_3_old and
	DCD and_ax_ax, and_ax_cx, and_ax_dx, and_ax_bx, and_ax_sp, and_ax_bp, and_ax_si, and_ax_di
	DCD and_cx_ax, and_cx_cx, and_cx_dx, and_cx_bx, and_cx_sp, and_cx_bp, and_cx_si, and_cx_di
	DCD and_dx_ax, and_dx_cx, and_dx_dx, and_dx_bx, and_dx_sp, and_dx_bp, and_dx_si, and_dx_di
	DCD and_bx_ax, and_bx_cx, and_bx_dx, and_bx_bx, and_bx_sp, and_bx_bp, and_bx_si, and_bx_di
	DCD and_sp_ax, and_sp_cx, and_sp_dx, and_sp_bx, and_sp_sp, and_sp_bp, and_sp_si, and_sp_di
	DCD and_bp_ax, and_bp_cx, and_bp_dx, and_bp_bx, and_bp_sp, and_bp_bp, and_bp_si, and_bp_di
	DCD and_si_ax, and_si_cx, and_si_dx, and_si_bx, and_si_sp, and_si_bp, and_si_si, and_si_di
	DCD and_di_ax, and_di_cx, and_di_dx, and_di_bx, and_di_sp, and_di_bp, and_di_si, and_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	and_reg16_r0high $reg
	EXTERN	op_23_EGA_$reg
	EXTERN	op_23_MODEX_$reg
	GLOBAL	and_r16_r0_bp_$reg
and_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	and_r16_r0_$reg
and_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_23_RAM_$reg, op_23_EGA_$reg, op_23_MODEX_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_23_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r3, $reg, lsl #16
	eor		$reg, r3, lsr #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	ands	r0, r3, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	and_reg16_r0high eax
	and_reg16_r0high ecx
	and_reg16_r0high edx
	and_reg16_r0high ebx
	and_reg16_r0high esp
	and_reg16_r0high ebp
	and_reg16_r0high esi
	and_reg16_r0high edi

	LTORG

	modrm_3_genall_old and, and_r16_r0

; --- AND reg16, reg16 ---

	MACRO 
	and_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	mov		r1, $rr, lsl #16
	eor		$rl, r0, lsr #16
	ands	r0, r1
	orr		$rl, r0, lsr #16
	b		loop
	MEND

and_ax_ax
	and_reg16_reg16		eax, eax
and_ax_cx
	and_reg16_reg16		eax, ecx
and_ax_dx
	and_reg16_reg16		eax, edx
and_ax_bx
	and_reg16_reg16		eax, ebx
and_ax_sp
	and_reg16_reg16		eax, esp
and_ax_bp
	and_reg16_reg16		eax, ebp
and_ax_si
	and_reg16_reg16		eax, esi
and_ax_di
	and_reg16_reg16		eax, edi
and_cx_ax
	and_reg16_reg16		ecx, eax
and_cx_cx
	and_reg16_reg16		ecx, ecx
and_cx_dx
	and_reg16_reg16		ecx, edx
and_cx_bx
	and_reg16_reg16		ecx, ebx
and_cx_sp
	and_reg16_reg16		ecx, esp
and_cx_bp
	and_reg16_reg16		ecx, ebp
and_cx_si
	and_reg16_reg16		ecx, esi
and_cx_di
	and_reg16_reg16		ecx, edi
and_dx_ax
	and_reg16_reg16		edx, eax
and_dx_cx
	and_reg16_reg16		edx, ecx
and_dx_dx
	and_reg16_reg16		edx, edx
and_dx_bx
	and_reg16_reg16		edx, ebx
and_dx_sp
	and_reg16_reg16		edx, esp
and_dx_bp
	and_reg16_reg16		edx, ebp
and_dx_si
	and_reg16_reg16		edx, esi
and_dx_di
	and_reg16_reg16		edx, edi
and_bx_ax
	and_reg16_reg16		ebx, eax
and_bx_cx
	and_reg16_reg16		ebx, ecx
and_bx_dx
	and_reg16_reg16		ebx, edx
and_bx_bx
	and_reg16_reg16		ebx, ebx
and_bx_sp
	and_reg16_reg16		ebx, esp
and_bx_bp
	and_reg16_reg16		ebx, ebp
and_bx_si
	and_reg16_reg16		ebx, esi
and_bx_di
	and_reg16_reg16		ebx, edi
and_sp_ax
	and_reg16_reg16		esp, eax
and_sp_cx
	and_reg16_reg16		esp, ecx
and_sp_dx
	and_reg16_reg16		esp, edx
and_sp_bx
	and_reg16_reg16		esp, ebx
and_sp_sp
	and_reg16_reg16		esp, esp
and_sp_bp
	and_reg16_reg16		esp, ebp
and_sp_si
	and_reg16_reg16		esp, esi
and_sp_di
	and_reg16_reg16		esp, edi
and_bp_ax
	and_reg16_reg16		ebp, eax
and_bp_cx
	and_reg16_reg16		ebp, ecx
and_bp_dx
	and_reg16_reg16		ebp, edx
and_bp_bx
	and_reg16_reg16		ebp, ebx
and_bp_sp
	and_reg16_reg16		ebp, esp
and_bp_bp
	and_reg16_reg16		ebp, ebp
and_bp_si
	and_reg16_reg16		ebp, esi
and_bp_di
	and_reg16_reg16		ebp, edi
and_si_ax
	and_reg16_reg16		esi, eax
and_si_cx
	and_reg16_reg16		esi, ecx
and_si_dx
	and_reg16_reg16		esi, edx
and_si_bx
	and_reg16_reg16		esi, ebx
and_si_sp
	and_reg16_reg16		esi, esp
and_si_bp
	and_reg16_reg16		esi, ebp
and_si_si
	and_reg16_reg16		esi, esi
and_si_di
	and_reg16_reg16		esi, edi
and_di_ax
	and_reg16_reg16		edi, eax
and_di_cx
	and_reg16_reg16		edi, ecx
and_di_dx
	and_reg16_reg16		edi, edx
and_di_bx
	and_reg16_reg16		edi, ebx
and_di_sp
	and_reg16_reg16		edi, esp
and_di_bp
	and_reg16_reg16		edi, ebp
and_di_si
	and_reg16_reg16		edi, esi
and_di_di
	and_reg16_reg16		edi, edi

	GLOBAL  and_ax_ax
	GLOBAL  and_ax_cx
	GLOBAL  and_ax_dx
	GLOBAL  and_ax_bx
	GLOBAL  and_ax_sp
	GLOBAL  and_ax_bp
	GLOBAL  and_ax_si
	GLOBAL  and_ax_di
	GLOBAL  and_cx_ax
	GLOBAL  and_cx_cx
	GLOBAL  and_cx_dx
	GLOBAL  and_cx_bx
	GLOBAL  and_cx_sp
	GLOBAL  and_cx_bp
	GLOBAL  and_cx_si
	GLOBAL  and_cx_di
	GLOBAL  and_dx_ax
	GLOBAL  and_dx_cx
	GLOBAL  and_dx_dx
	GLOBAL  and_dx_bx
	GLOBAL  and_dx_sp
	GLOBAL  and_dx_bp
	GLOBAL  and_dx_si
	GLOBAL  and_dx_di
	GLOBAL  and_bx_ax
	GLOBAL  and_bx_cx
	GLOBAL  and_bx_dx
	GLOBAL  and_bx_bx
	GLOBAL  and_bx_sp
	GLOBAL  and_bx_bp
	GLOBAL  and_bx_si
	GLOBAL  and_bx_di
	GLOBAL  and_sp_ax
	GLOBAL  and_sp_cx
	GLOBAL  and_sp_dx
	GLOBAL  and_sp_bx
	GLOBAL  and_sp_sp
	GLOBAL  and_sp_bp
	GLOBAL  and_sp_si
	GLOBAL  and_sp_di
	GLOBAL  and_bp_ax
	GLOBAL  and_bp_cx
	GLOBAL  and_bp_dx
	GLOBAL  and_bp_bx
	GLOBAL  and_bp_sp
	GLOBAL  and_bp_bp
	GLOBAL  and_bp_si
	GLOBAL  and_bp_di
	GLOBAL  and_si_ax
	GLOBAL  and_si_cx
	GLOBAL  and_si_dx
	GLOBAL  and_si_bx
	GLOBAL  and_si_sp
	GLOBAL  and_si_bp
	GLOBAL  and_si_si
	GLOBAL  and_si_di
	GLOBAL  and_di_ax
	GLOBAL  and_di_cx
	GLOBAL  and_di_dx
	GLOBAL  and_di_bx
	GLOBAL  and_di_sp
	GLOBAL  and_di_bp
	GLOBAL  and_di_si
	GLOBAL  and_di_di



; ------------------- 24 = AND AL, imm8 ------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
op_24
	ldrb	r1,[r12],#1				; Load byte to r1, increment r12 by 1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	mov		r0, eax, lsl #24
	ands	r0, r1, lsl #24			; Perform the AND
	bic		eax, #0xFF
	orr		eax, r0, lsr #24		; Put the result to AL
	;-------
	; Save the resulting byte (for possible later parity check)
	;-------
	strb	eax, [sp, #SP_PARITY_BYTE]	; For "Chess Genius 3", save the parity byte to stack.
	b		loop

; ------------------- 25 = AND AX, imm16 ------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual ands operation, so C and O work like in x86.
;
op_25
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r1,[r12],#1				; Load byte to r1, increment r12 by 1
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	orr		r0, r1, r0, lsl #8		; r0 = low byte | (high byte << 8)
	mov		r1, eax, lsl #16
	eor		eax, r1, lsr #16
	ands	r0, r1, r0, lsl #16
	orr		eax, r0, lsr #16
	b		loop

; ------------------- Segment Overrides using "opcodetable_16_16" -----------------------

op_26
	ldr		r2, [sp, #SP_ES_BASE]				; r2 = current effective logical ES segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_2e
	ldr		r2, [sp, #SP_CS_BASE]				; r2 = current effective logical CS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_36
	ldr		r2, [sp, #SP_SS_BASE]				; r2 = current effective logical SS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_3e
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; r2 = logical DS segment in high halfword
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_64
	ldr		r2, [sp, #SP_FS_BASE]				; r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler

op_65
	ldr		r2, [sp, #SP_GS_BASE]				; r2 = current effective logical GS segment
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	mov		lr, r2
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler


; ------------------- 6667 = Operand- and Address-size Prefixes -------
	GLOBAL	op_66_67_USE32
op_66_67_USE32
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =opcodetable_16_16				; (opcodetable_16_16 - 8 - .)
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	;---------------------------------------------
	; Operand Size = 16bit, Address Size = 16bit
	;---------------------------------------------

	ALIGN	4

; ------------------- 27 = DAA ----------------------------------------
; Weird BCD opcode.
;
; if(AL & 0xF > 9 || AF == 1) {
;	CF = OldCF | GetCarry(AL = AL + 6);
;	AF = 1;
; }
; else AF = 0;
;
; if(OldAL > 0x99 || OldCF == 1) {
;	AL = AL + 0x60;
;	CF = 1;
; }
; else CF = 0;
;
	GLOBAL	op_27
op_27
	ldr		r2, [sp, #SP_FLAGS]					; r2 = current x86 flags (for FLAG_AF)
	mrs		r0, cpsr							; r0 = current ARM flags (for other flags)
	and		r1, eax, #0x0F						;	if (((reg_al & 0x0F)>0x09) || get_AF()) {
	cmp		r1, #0x09
	and		r1, eax, #0xFF						; r1 = AL value
	bgt		%f1
	tst		r2, #FLAG_AF
	beq		%f2
1	
	cmp		r1, #0x99							;		if ((reg_al > 0x99) || get_CF()) {
	bgt		%f6
	tst		r0, #ARM_CARRY
	beq		%f7
6	add		r1, #0x60							;			reg_al+=0x60;
	orr		r0, #ARM_CARRY						;			SETFLAGBIT(CF,true);
	b		%f8									;		} else
7	bic		r0, #ARM_CARRY						;			SETFLAGBIT(CF,false);
8	add		r1, #0x06							;		reg_al+=0x06;
	orr		r2, #FLAG_AF						;		SETFLAGBIT(AF,true);
	b		%f9									;	} else {
	
2	cmp		r1, #0x99							;		if ((reg_al > 0x99) || get_CF()) {
	bgt		%f3
	tst		r0, #ARM_CARRY
	beq		%f4
3	add		r1, #0x60							;			reg_al+=0x60;
	orr		r0, #ARM_CARRY						;			SETFLAGBIT(CF,true);
	b		%f5									;		} else
4	bic		r0, #ARM_CARRY						;			SETFLAGBIT(CF,false);
5	bic		r2, #FLAG_AF						;		SETFLAGBIT(AF,false);
9	bic		r0, #(ARM_ZERO|ARM_NEG)
	;------
	; SETFLAGBIT(ZF,(reg_al==0));
	;------
	ands	r1, #0xFF
	orreq	r0, #ARM_ZERO
	;------
	; SETFLAGBIT(SF,(reg_al&0x80));
	;------
	tst		r1, #0x80
	orrne	r0, #ARM_NEG
	;------
	; SETFLAGBIT(PF,parity_lookup[reg_al]);
	;------
	;------
	; Put the result into EAX low byte
	;------
	bic		eax, #0xFF
	orr		eax, r1
	str		r2, [sp, #SP_FLAGS]					; Save new x86 flags (for FLAG_AF)
	b		restore_flags_from_r0

; ------------------- 28 = SUB r/m8, r8 -----------------------------
;
; All modrm variations supported!
;
;
op_28
	modrm_jump_16_tbl op_28_jump
	modrm_tbl_0	sub
	DCD sub_al_al, sub_cl_al, sub_dl_al, sub_bl_al, sub_ah_al, sub_ch_al, sub_dh_al, sub_bh_al
	DCD sub_al_cl, sub_cl_cl, sub_dl_cl, sub_bl_cl, sub_ah_cl, sub_ch_cl, sub_dh_cl, sub_bh_cl
	DCD sub_al_dl, sub_cl_dl, sub_dl_dl, sub_bl_dl, sub_ah_dl, sub_ch_dl, sub_dh_dl, sub_bh_dl
	DCD sub_al_bl, sub_cl_bl, sub_dl_bl, sub_bl_bl, sub_ah_bl, sub_ch_bl, sub_dh_bl, sub_bh_bl
	DCD sub_al_ah, sub_cl_ah, sub_dl_ah, sub_bl_ah, sub_ah_ah, sub_ch_ah, sub_dh_ah, sub_bh_ah
	DCD sub_al_ch, sub_cl_ch, sub_dl_ch, sub_bl_ch, sub_ah_ch, sub_ch_ch, sub_dh_ch, sub_bh_ch
	DCD sub_al_dh, sub_cl_dh, sub_dl_dh, sub_bl_dh, sub_ah_dh, sub_ch_dh, sub_dh_dh, sub_bh_dh
	DCD sub_al_bh, sub_cl_bh, sub_dl_bh, sub_bl_bh, sub_ah_bh, sub_ch_bh, sub_dh_bh, sub_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sub_r0_reg8l $reg
	GLOBAL	sub_r0_r8l_bp_$reg
sub_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sub_r0_r8l_$reg
sub_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_28_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_28_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	rsbs	r0, r1, r0, lsl #24
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to [physical segment + disp16]
	b		complement_carry
	MEND
	MACRO 
	sub_r0_reg8h $reg
	GLOBAL	sub_r0_r8h_bp_$reg
sub_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sub_r0_r8h_$reg
sub_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_28_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_28_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	rsbs	r0, r1, r0, lsl #24
	lsr		r0, #24
	strb	r0,[r2]					; Store byte to RAM
	b		complement_carry
	MEND

	sub_r0_reg8l eax
	sub_r0_reg8l ecx
	sub_r0_reg8l edx
	sub_r0_reg8l ebx
	sub_r0_reg8h eax
	sub_r0_reg8h ecx
	sub_r0_reg8h edx
	sub_r0_reg8h ebx

	LTORG

	modrm_0_genall sub


; ------------------- 29 = SUB r/m16, r16 -----------------------------
;
; All modrm variations supported!
;
;
op_29
	modrm_jump_16_tbl op_29_jump
	modrm_tbl_1_old sub
	DCD sub_ax_ax, sub_cx_ax, sub_dx_ax, sub_bx_ax, sub_sp_ax, sub_bp_ax, sub_si_ax, sub_di_ax
	DCD sub_ax_cx, sub_cx_cx, sub_dx_cx, sub_bx_cx, sub_sp_cx, sub_bp_cx, sub_si_cx, sub_di_cx
	DCD sub_ax_dx, sub_cx_dx, sub_dx_dx, sub_bx_dx, sub_sp_dx, sub_bp_dx, sub_si_dx, sub_di_dx
	DCD sub_ax_bx, sub_cx_bx, sub_dx_bx, sub_bx_bx, sub_sp_bx, sub_bp_bx, sub_si_bx, sub_di_bx
	DCD sub_ax_sp, sub_cx_sp, sub_dx_sp, sub_bx_sp, sub_sp_sp, sub_bp_sp, sub_si_sp, sub_di_sp
	DCD sub_ax_bp, sub_cx_bp, sub_dx_bp, sub_bx_bp, sub_sp_bp, sub_bp_bp, sub_si_bp, sub_di_bp
	DCD sub_ax_si, sub_cx_si, sub_dx_si, sub_bx_si, sub_sp_si, sub_bp_si, sub_si_si, sub_di_si
	DCD sub_ax_di, sub_cx_di, sub_dx_di, sub_bx_di, sub_sp_di, sub_bp_di, sub_si_di, sub_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sub_r0_r16_reg $reg
	GLOBAL	sub_r0_r16_bp_$reg
sub_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sub_r0_r16_$reg
sub_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_29_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_29_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	lsl		r0, #16
	orr		r0, r1, lsl #24			; r0 = low byte | (high byte << 8)
	subs	r0, $reg, lsl #16		; r0 = [disp16] - reg
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		complement_carry
	MEND

	sub_r0_r16_reg eax
	sub_r0_r16_reg ecx
	sub_r0_r16_reg edx
	sub_r0_r16_reg ebx
	sub_r0_r16_reg esp
	sub_r0_r16_reg ebp
	sub_r0_r16_reg esi
	sub_r0_r16_reg edi

	LTORG

	modrm_1_genall sub, sub_r0_r16


; ------------------- 2A = SUB esp, r/m8 -------------------------------
;
; All modrm variations supported!
;
;
op_2a
	modrm_jump_16_tbl op_2a_jump
	modrm_tbl_2 sub
	DCD sub_al_al, sub_al_cl, sub_al_dl, sub_al_bl, sub_al_ah, sub_al_ch, sub_al_dh, sub_al_bh
	DCD sub_cl_al, sub_cl_cl, sub_cl_dl, sub_cl_bl, sub_cl_ah, sub_cl_ch, sub_cl_dh, sub_cl_bh
	DCD sub_dl_al, sub_dl_cl, sub_dl_dl, sub_dl_bl, sub_dl_ah, sub_dl_ch, sub_dl_dh, sub_dl_bh
	DCD sub_bl_al, sub_bl_cl, sub_bl_dl, sub_bl_bl, sub_bl_ah, sub_bl_ch, sub_bl_dh, sub_bl_bh
	DCD sub_ah_al, sub_ah_cl, sub_ah_dl, sub_ah_bl, sub_ah_ah, sub_ah_ch, sub_ah_dh, sub_ah_bh
	DCD sub_ch_al, sub_ch_cl, sub_ch_dl, sub_ch_bl, sub_ch_ah, sub_ch_ch, sub_ch_dh, sub_ch_bh
	DCD sub_dh_al, sub_dh_cl, sub_dh_dl, sub_dh_bl, sub_dh_ah, sub_dh_ch, sub_dh_dh, sub_dh_bh
	DCD sub_bh_al, sub_bh_cl, sub_bh_dl, sub_bh_bl, sub_bh_ah, sub_bh_ch, sub_bh_dh, sub_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sub_reg8l_r0high $reg
	GLOBAL	sub_r8l_r0_bp_$reg
sub_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sub_r8l_r0_$reg
sub_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_2a_RAM_l_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_2a_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	subs	r1, r0, lsl #24			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		complement_carry
	MEND
	MACRO 
	sub_reg8h_r0high $reg
	GLOBAL	sub_r8h_r0_bp_$reg
sub_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL sub_r8h_r0_$reg
sub_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_2a_RAM_h_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_2a_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00		; Left operand already uses just the rightmost byte
	lsl		r1, #16
	subs	r1, r0, lsl #24			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16
	b		complement_carry
	MEND

	sub_reg8l_r0high eax
	sub_reg8l_r0high ecx
	sub_reg8l_r0high edx
	sub_reg8l_r0high ebx
	sub_reg8h_r0high eax
	sub_reg8h_r0high ecx
	sub_reg8h_r0high edx
	sub_reg8h_r0high ebx

	LTORG
	
	modrm_2_genall sub


; --- registers ---

	MACRO 
	sub_reg8l_reg8l $reg1, $reg2
	mov		r0, $reg1, lsl #24
	mov		r1, $reg2, lsl #24
	subs	r0, r1
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		complement_carry
	MEND

	MACRO 
	sub_reg8l_reg8h $reg1, $reg2
	mov		r0, $reg1, lsl #24
	and		r1, $reg2, #0xFF00
	subs	r0, r1, lsl #16
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		complement_carry
	MEND

	MACRO 
	sub_reg8h_reg8l $reg1, $reg2
	and		r0, $reg1, #0xFF00
	mov		r1, $reg2, lsl #24
	rsbs	r0, r1, r0, lsl #16
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		complement_carry
	MEND

	MACRO 
	sub_reg8h_reg8h $reg1, $reg2
	and		r0, $reg1, #0xFF00
	and		r1, $reg2, #0xFF00
	lsl		r0, #16
	subs	r0, r1, lsl #16
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		complement_carry
	MEND

sub_al_al
	sub_reg8l_reg8l eax, eax
sub_al_cl
	sub_reg8l_reg8l eax, ecx
sub_al_dl
	sub_reg8l_reg8l eax, edx
sub_al_bl
	sub_reg8l_reg8l eax, ebx
sub_al_ah
	sub_reg8l_reg8h eax, eax
sub_al_ch
	sub_reg8l_reg8h eax, ecx
sub_al_dh
	sub_reg8l_reg8h eax, edx
sub_al_bh
	sub_reg8l_reg8h eax, ebx
sub_cl_al
	sub_reg8l_reg8l ecx, eax
sub_cl_cl
	sub_reg8l_reg8l ecx, ecx
sub_cl_dl
	sub_reg8l_reg8l ecx, edx
sub_cl_bl
	sub_reg8l_reg8l ecx, ebx
sub_cl_ah
	sub_reg8l_reg8h ecx, eax
sub_cl_ch
	sub_reg8l_reg8h ecx, ecx
sub_cl_dh
	sub_reg8l_reg8h ecx, edx
sub_cl_bh
	sub_reg8l_reg8h ecx, ebx
sub_dl_al
	sub_reg8l_reg8l edx, eax
sub_dl_cl
	sub_reg8l_reg8l edx, ecx
sub_dl_dl
	sub_reg8l_reg8l edx, edx
sub_dl_bl
	sub_reg8l_reg8l edx, ebx
sub_dl_ah
	sub_reg8l_reg8h edx, eax
sub_dl_ch
	sub_reg8l_reg8h edx, ecx
sub_dl_dh
	sub_reg8l_reg8h edx, edx
sub_dl_bh
	sub_reg8l_reg8h edx, ebx
sub_bl_al
	sub_reg8l_reg8l ebx, eax
sub_bl_cl
	sub_reg8l_reg8l ebx, ecx
sub_bl_dl
	sub_reg8l_reg8l ebx, edx
sub_bl_bl
	sub_reg8l_reg8l ebx, ebx
sub_bl_ah
	sub_reg8l_reg8h ebx, eax
sub_bl_ch
	sub_reg8l_reg8h ebx, ecx
sub_bl_dh
	sub_reg8l_reg8h ebx, edx
sub_bl_bh
	sub_reg8l_reg8h ebx, ebx

sub_ah_al
	sub_reg8h_reg8l eax, eax
sub_ah_cl
	sub_reg8h_reg8l eax, ecx
sub_ah_dl
	sub_reg8h_reg8l eax, edx
sub_ah_bl
	sub_reg8h_reg8l eax, ebx
sub_ah_ah
	sub_reg8h_reg8h eax, eax
sub_ah_ch
	sub_reg8h_reg8h eax, ecx
sub_ah_dh
	sub_reg8h_reg8h eax, edx
sub_ah_bh
	sub_reg8h_reg8h eax, ebx
sub_ch_al
	sub_reg8h_reg8l ecx, eax
sub_ch_cl
	sub_reg8h_reg8l ecx, ecx
sub_ch_dl
	sub_reg8h_reg8l ecx, edx
sub_ch_bl
	sub_reg8h_reg8l ecx, ebx
sub_ch_ah
	sub_reg8h_reg8h ecx, eax
sub_ch_ch
	sub_reg8h_reg8h ecx, ecx
sub_ch_dh
	sub_reg8h_reg8h ecx, edx
sub_ch_bh
	sub_reg8h_reg8h ecx, ebx
sub_dh_al
	sub_reg8h_reg8l edx, eax
sub_dh_cl
	sub_reg8h_reg8l edx, ecx
sub_dh_dl
	sub_reg8h_reg8l edx, edx
sub_dh_bl
	sub_reg8h_reg8l edx, ebx
sub_dh_ah
	sub_reg8h_reg8h edx, eax
sub_dh_ch
	sub_reg8h_reg8h edx, ecx
sub_dh_dh
	sub_reg8h_reg8h edx, edx
sub_dh_bh
	sub_reg8h_reg8h edx, ebx
sub_bh_al
	sub_reg8h_reg8l ebx, eax
sub_bh_cl
	sub_reg8h_reg8l ebx, ecx
sub_bh_dl
	sub_reg8h_reg8l ebx, edx
sub_bh_bl
	sub_reg8h_reg8l ebx, ebx
sub_bh_ah
	sub_reg8h_reg8h ebx, eax
sub_bh_ch
	sub_reg8h_reg8h ebx, ecx
sub_bh_dh
	sub_reg8h_reg8h ebx, edx
sub_bh_bh
	sub_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  sub_al_al
	GLOBAL  sub_al_cl
	GLOBAL  sub_al_dl
	GLOBAL  sub_al_bl
	GLOBAL  sub_al_ah
	GLOBAL  sub_al_ch
	GLOBAL  sub_al_dh
	GLOBAL  sub_al_bh
	GLOBAL  sub_cl_al
	GLOBAL  sub_cl_cl
	GLOBAL  sub_cl_dl
	GLOBAL  sub_cl_bl
	GLOBAL  sub_cl_ah
	GLOBAL  sub_cl_ch
	GLOBAL  sub_cl_dh
	GLOBAL  sub_cl_bh
	GLOBAL  sub_dl_al
	GLOBAL  sub_dl_cl
	GLOBAL  sub_dl_dl
	GLOBAL  sub_dl_bl
	GLOBAL  sub_dl_ah
	GLOBAL  sub_dl_ch
	GLOBAL  sub_dl_dh
	GLOBAL  sub_dl_bh
	GLOBAL  sub_bl_al
	GLOBAL  sub_bl_cl
	GLOBAL  sub_bl_dl
	GLOBAL  sub_bl_bl
	GLOBAL  sub_bl_ah
	GLOBAL  sub_bl_ch
	GLOBAL  sub_bl_dh
	GLOBAL  sub_bl_bh
	GLOBAL  sub_ah_al
	GLOBAL  sub_ah_cl
	GLOBAL  sub_ah_dl
	GLOBAL  sub_ah_bl
	GLOBAL  sub_ah_ah
	GLOBAL  sub_ah_ch
	GLOBAL  sub_ah_dh
	GLOBAL  sub_ah_bh
	GLOBAL  sub_ch_al
	GLOBAL  sub_ch_cl
	GLOBAL  sub_ch_dl
	GLOBAL  sub_ch_bl
	GLOBAL  sub_ch_ah
	GLOBAL  sub_ch_ch
	GLOBAL  sub_ch_dh
	GLOBAL  sub_ch_bh
	GLOBAL  sub_dh_al
	GLOBAL  sub_dh_cl
	GLOBAL  sub_dh_dl
	GLOBAL  sub_dh_bl
	GLOBAL  sub_dh_ah
	GLOBAL  sub_dh_ch
	GLOBAL  sub_dh_dh
	GLOBAL  sub_dh_bh
	GLOBAL  sub_bh_al
	GLOBAL  sub_bh_cl
	GLOBAL  sub_bh_dl
	GLOBAL  sub_bh_bl
	GLOBAL  sub_bh_ah
	GLOBAL  sub_bh_ch
	GLOBAL  sub_bh_dh
	GLOBAL  sub_bh_bh

	
	
; ------------------- 2B = SUB r16, r/m16 -----------------------------
;
; All modrm variations supported!
;
;
op_2b
	modrm_jump_16_tbl op_2b_jump
	modrm_tbl_3_old sub
	DCD sub_ax_ax, sub_ax_cx, sub_ax_dx, sub_ax_bx, sub_ax_sp, sub_ax_bp, sub_ax_si, sub_ax_di
	DCD sub_cx_ax, sub_cx_cx, sub_cx_dx, sub_cx_bx, sub_cx_sp, sub_cx_bp, sub_cx_si, sub_cx_di
	DCD sub_dx_ax, sub_dx_cx, sub_dx_dx, sub_dx_bx, sub_dx_sp, sub_dx_bp, sub_dx_si, sub_dx_di
	DCD sub_bx_ax, sub_bx_cx, sub_bx_dx, sub_bx_bx, sub_bx_sp, sub_bx_bp, sub_bx_si, sub_bx_di
	DCD sub_sp_ax, sub_sp_cx, sub_sp_dx, sub_sp_bx, sub_sp_sp, sub_sp_bp, sub_sp_si, sub_sp_di
	DCD sub_bp_ax, sub_bp_cx, sub_bp_dx, sub_bp_bx, sub_bp_sp, sub_bp_bp, sub_bp_si, sub_bp_di
	DCD sub_si_ax, sub_si_cx, sub_si_dx, sub_si_bx, sub_si_sp, sub_si_bp, sub_si_si, sub_si_di
	DCD sub_di_ax, sub_di_cx, sub_di_dx, sub_di_bx, sub_di_sp, sub_di_bp, sub_di_si, sub_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	sub_reg16_r0high $reg
	GLOBAL	sub_r16_r0_bp_$reg
sub_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	sub_r16_r0_$reg
sub_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_2b_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_2b_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eor		$reg, r2, lsr #16
	subs	r0, r2, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		complement_carry
	MEND

	sub_reg16_r0high eax
	sub_reg16_r0high ecx
	sub_reg16_r0high edx
	sub_reg16_r0high ebx
	sub_reg16_r0high esp
	sub_reg16_r0high ebp
	sub_reg16_r0high esi
	sub_reg16_r0high edi

	LTORG

	modrm_3_genall_old sub, sub_r16_r0


; --- registers ---

	MACRO 
	sub_reg16_reg16 $rl, $rr
	mov		r1, $rl, lsl #16
	subs	r0, r1, $rr, lsl #16
	eor		$rl, r1, lsr #16
	orr		$rl, r0, lsr #16
	b		complement_carry
	MEND
	
sub_ax_ax
	sub_reg16_reg16		eax, eax
sub_ax_cx
	sub_reg16_reg16		eax, ecx
sub_ax_dx
	sub_reg16_reg16		eax, edx
sub_ax_bx
	sub_reg16_reg16		eax, ebx
sub_ax_sp
	sub_reg16_reg16		eax, esp
sub_ax_bp
	sub_reg16_reg16		eax, ebp
sub_ax_si
	sub_reg16_reg16		eax, esi
sub_ax_di
	sub_reg16_reg16		eax, edi
sub_cx_ax
	sub_reg16_reg16		ecx, eax
sub_cx_cx
	sub_reg16_reg16		ecx, ecx
sub_cx_dx
	sub_reg16_reg16		ecx, edx
sub_cx_bx
	sub_reg16_reg16		ecx, ebx
sub_cx_sp
	sub_reg16_reg16		ecx, esp
sub_cx_bp
	sub_reg16_reg16		ecx, ebp
sub_cx_si
	sub_reg16_reg16		ecx, esi
sub_cx_di
	sub_reg16_reg16		ecx, edi
sub_dx_ax
	sub_reg16_reg16		edx, eax
sub_dx_cx
	sub_reg16_reg16		edx, ecx
sub_dx_dx
	sub_reg16_reg16		edx, edx
sub_dx_bx
	sub_reg16_reg16		edx, ebx
sub_dx_sp
	sub_reg16_reg16		edx, esp
sub_dx_bp
	sub_reg16_reg16		edx, ebp
sub_dx_si
	sub_reg16_reg16		edx, esi
sub_dx_di
	sub_reg16_reg16		edx, edi
sub_bx_ax
	sub_reg16_reg16		ebx, eax
sub_bx_cx
	sub_reg16_reg16		ebx, ecx
sub_bx_dx
	sub_reg16_reg16		ebx, edx
sub_bx_bx
	sub_reg16_reg16		ebx, ebx
sub_bx_sp
	sub_reg16_reg16		ebx, esp
sub_bx_bp
	sub_reg16_reg16		ebx, ebp
sub_bx_si
	sub_reg16_reg16		ebx, esi
sub_bx_di
	sub_reg16_reg16		ebx, edi
sub_sp_ax
	sub_reg16_reg16		esp, eax
sub_sp_cx
	sub_reg16_reg16		esp, ecx
sub_sp_dx
	sub_reg16_reg16		esp, edx
sub_sp_bx
	sub_reg16_reg16		esp, ebx
sub_sp_sp
	sub_reg16_reg16		esp, esp
sub_sp_bp
	sub_reg16_reg16		esp, ebp
sub_sp_si
	sub_reg16_reg16		esp, esi
sub_sp_di
	sub_reg16_reg16		esp, edi
sub_bp_ax
	sub_reg16_reg16		ebp, eax
sub_bp_cx
	sub_reg16_reg16		ebp, ecx
sub_bp_dx
	sub_reg16_reg16		ebp, edx
sub_bp_bx
	sub_reg16_reg16		ebp, ebx
sub_bp_sp
	sub_reg16_reg16		ebp, esp
sub_bp_bp
	sub_reg16_reg16		ebp, ebp
sub_bp_si
	sub_reg16_reg16		ebp, esi
sub_bp_di
	sub_reg16_reg16		ebp, edi
sub_si_ax
	sub_reg16_reg16		esi, eax
sub_si_cx
	sub_reg16_reg16		esi, ecx
sub_si_dx
	sub_reg16_reg16		esi, edx
sub_si_bx
	sub_reg16_reg16		esi, ebx
sub_si_sp
	sub_reg16_reg16		esi, esp
sub_si_bp
	sub_reg16_reg16		esi, ebp
sub_si_si
	sub_reg16_reg16		esi, esi
sub_si_di
	sub_reg16_reg16		esi, edi
sub_di_ax
	sub_reg16_reg16		edi, eax
sub_di_cx
	sub_reg16_reg16		edi, ecx
sub_di_dx
	sub_reg16_reg16		edi, edx
sub_di_bx
	sub_reg16_reg16		edi, ebx
sub_di_sp
	sub_reg16_reg16		edi, esp
sub_di_bp
	sub_reg16_reg16		edi, ebp
sub_di_si
	sub_reg16_reg16		edi, esi
sub_di_di
	sub_reg16_reg16		edi, edi

	GLOBAL  sub_ax_ax
	GLOBAL  sub_ax_cx
	GLOBAL  sub_ax_dx
	GLOBAL  sub_ax_bx
	GLOBAL  sub_ax_sp
	GLOBAL  sub_ax_bp
	GLOBAL  sub_ax_si
	GLOBAL  sub_ax_di
	GLOBAL  sub_cx_ax
	GLOBAL  sub_cx_cx
	GLOBAL  sub_cx_dx
	GLOBAL  sub_cx_bx
	GLOBAL  sub_cx_sp
	GLOBAL  sub_cx_bp
	GLOBAL  sub_cx_si
	GLOBAL  sub_cx_di
	GLOBAL  sub_dx_ax
	GLOBAL  sub_dx_cx
	GLOBAL  sub_dx_dx
	GLOBAL  sub_dx_bx
	GLOBAL  sub_dx_sp
	GLOBAL  sub_dx_bp
	GLOBAL  sub_dx_si
	GLOBAL  sub_dx_di
	GLOBAL  sub_bx_ax
	GLOBAL  sub_bx_cx
	GLOBAL  sub_bx_dx
	GLOBAL  sub_bx_bx
	GLOBAL  sub_bx_sp
	GLOBAL  sub_bx_bp
	GLOBAL  sub_bx_si
	GLOBAL  sub_bx_di
	GLOBAL  sub_sp_ax
	GLOBAL  sub_sp_cx
	GLOBAL  sub_sp_dx
	GLOBAL  sub_sp_bx
	GLOBAL  sub_sp_sp
	GLOBAL  sub_sp_bp
	GLOBAL  sub_sp_si
	GLOBAL  sub_sp_di
	GLOBAL  sub_bp_ax
	GLOBAL  sub_bp_cx
	GLOBAL  sub_bp_dx
	GLOBAL  sub_bp_bx
	GLOBAL  sub_bp_sp
	GLOBAL  sub_bp_bp
	GLOBAL  sub_bp_si
	GLOBAL  sub_bp_di
	GLOBAL  sub_si_ax
	GLOBAL  sub_si_cx
	GLOBAL  sub_si_dx
	GLOBAL  sub_si_bx
	GLOBAL  sub_si_sp
	GLOBAL  sub_si_bp
	GLOBAL  sub_si_si
	GLOBAL  sub_si_di
	GLOBAL  sub_di_ax
	GLOBAL  sub_di_cx
	GLOBAL  sub_di_dx
	GLOBAL  sub_di_bx
	GLOBAL  sub_di_sp
	GLOBAL  sub_di_bp
	GLOBAL  sub_di_si
	GLOBAL  sub_di_di


; ------------------- 2C = SUB AL, imm8 -------------------------------
op_2c
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r1, eax, lsl #24		; r1 = AL
	subs	r1, r0, lsl #24			; r1 = AL - imm8
	bic		eax, #0xFF
	orr		eax, r1, lsr #24		; Put result into AL
	b		complement_carry
	
; ------------------- 2D = SUB AX, imm16 ------------------------------
op_2d
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r1, increment r12 by 1
	mov		r2, eax, lsl #16
	eor		eax, r2, lsr #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	subs	r0, r2, r0, lsl #16		; Here we can safely use the shifter
	orr		eax, r0, lsr #16
	b		complement_carry

	ALIGN	4

; ------------------- 2F = DAS ----------------------------------------
;
; TODO! AC/PF handling!
;
; OldAL = AL;
; OldCF = CF;
; CF = 0;
;
; if(AL & 0xF > 9 || AF == 1) {
;	CF = OldCF | GetBorrow(AL = AL - 6);
;	AF = 1;
; }
; else AF = 0;
;
; if(OldAL > 0x99 || OldCF == 1) {
;	AL = AL - 0x60;
;	CF = 1;
; }
; else CF = 0;
op_2f
	ldr		r2, [sp, #SP_FLAGS]					; r2 = current x86 flags (for FLAG_AF)
	mrs		r0, cpsr							; r0 = current ARM flags (for other flags)
	and		r1, eax, #0x0F						;	if (((reg_al & 0x0F)>0x09) || get_AF()) {
	cmp		r1, #0x09
	and		r1, eax, #0xFF
	bgt		%f1
	tst		r2, #FLAG_AF
	beq		%f2
1	cmp		r1, #0x99							;		if ((reg_al > 0x99) || get_CF()) {
	bgt		%f6
	tst		r0, #ARM_CARRY
	beq		%f7
6	sub		r1, #0x60							;			reg_al-=0x60;
	orr		r0, #ARM_CARRY						;			SETFLAGBIT(CF,true);
	b		%f8									;		} else
7	cmp		r1, #0x06
	bicge	r0, #ARM_CARRY						;			SETFLAGBIT(CF,(reg_al<0x06));
	orrlt	r0, #ARM_CARRY
8	sub		r1, #0x06							;		reg_al-=0x06;
	orr		r2, #FLAG_AF						;		SETFLAGBIT(AF,true);
	b		%f9									;	} else {
2	cmp		r1, #0x99							;		if ((reg_al > 0x99) || get_CF()) {
	bgt		%f3
	tst		r0, #ARM_CARRY
	beq		%f4
3	sub		r1, #0x60							;			reg_al-=0x60;
	orr		r0, #ARM_CARRY						;			SETFLAGBIT(CF,true);
	b		%f5									;		} else
4	bic		r0, #ARM_CARRY						;			SETFLAGBIT(CF,false);
5	bic		r2, #FLAG_AF						;		SETFLAGBIT(AF,false);
9	bic		r0, #(ARM_OVER:OR:ARM_NEG:OR:ARM_ZERO)
	;------
	; SETFLAGBIT(ZF,(reg_al==0));
	;------
	ands	r1, #0xFF
	orreq	r0, #ARM_ZERO
	;------
	; SETFLAGBIT(SF,(reg_al&0x80));
	;------
	tst		r1, #0x80
	orrne	r0, #ARM_NEG
	;------
	; SETFLAGBIT(OF,osigned && ((reg_al&0x80)==0))
	;------
	tst		eax, #0x80							; osigned == original AL highest bit
	beq		%f1
	tst		r1, #0x80
	orreq	r0, #ARM_OVER
1	
	;------
	; SETFLAGBIT(PF,parity_lookup[reg_al]);
	;------
	;------
	; Put the result into EAX low byte
	;------
	bic		eax, #0xFF
	orr		eax, r1
	str		r2, [sp, #SP_FLAGS]					; Save new x86 flags (for FLAG_AF)
	b		restore_flags_from_r0
	
; ------------------- 30 = XOR r/m8, esp -------------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual eors operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_30
op_30
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_30_jump
	modrm_tbl_0	xor
	DCD xor_al_al, xor_cl_al, xor_dl_al, xor_bl_al, xor_ah_al, xor_ch_al, xor_dh_al, xor_bh_al
	DCD xor_al_cl, xor_cl_cl, xor_dl_cl, xor_bl_cl, xor_ah_cl, xor_ch_cl, xor_dh_cl, xor_bh_cl
	DCD xor_al_dl, xor_cl_dl, xor_dl_dl, xor_bl_dl, xor_ah_dl, xor_ch_dl, xor_dh_dl, xor_bh_dl
	DCD xor_al_bl, xor_cl_bl, xor_dl_bl, xor_bl_bl, xor_ah_bl, xor_ch_bl, xor_dh_bl, xor_bh_bl
	DCD xor_al_ah, xor_cl_ah, xor_dl_ah, xor_bl_ah, xor_ah_ah, xor_ch_ah, xor_dh_ah, xor_bh_ah
	DCD xor_al_ch, xor_cl_ch, xor_dl_ch, xor_bl_ch, xor_ah_ch, xor_ch_ch, xor_dh_ch, xor_bh_ch
	DCD xor_al_dh, xor_cl_dh, xor_dl_dh, xor_bl_dh, xor_ah_dh, xor_ch_dh, xor_dh_dh, xor_bh_dh
	DCD xor_al_bh, xor_cl_bh, xor_dl_bh, xor_bl_bh, xor_ah_bh, xor_ch_bh, xor_dh_bh, xor_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xor_r0_reg8l $reg
	EXTERN	op_30_EGA_l_$reg
	EXTERN	op_30_MODEX_l_$reg
	GLOBAL	xor_r0_r8l_bp_$reg
xor_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r0_r8l_$reg
xor_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_30_RAM_l_$reg, op_30_EGA_l_$reg, op_30_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_30_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	eors	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND
	MACRO 
	xor_r0_reg8h $reg
	EXTERN	op_30_EGA_h_$reg
	EXTERN	op_30_MODEX_h_$reg
	GLOBAL	xor_r0_r8h_bp_$reg
xor_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r0_r8h_$reg
xor_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_30_RAM_h_$reg, op_30_EGA_h_$reg, op_30_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_30_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	eors	r0, r1, r0, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	lsr		r0, #24
	strb	r0,[r2]					; Store the byte back
	b		loop
	MEND

	xor_r0_reg8l eax
	xor_r0_reg8l ecx
	xor_r0_reg8l edx
	xor_r0_reg8l ebx
	xor_r0_reg8h eax
	xor_r0_reg8h ecx
	xor_r0_reg8h edx
	xor_r0_reg8h ebx

	LTORG

	modrm_0_genall xor


; ------------------- 31 = XOR r/m16, r16 -----------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual eors operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_31
op_31
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_31_jump
	modrm_tbl_1_old xor
	DCD xor_ax_ax, xor_cx_ax, xor_dx_ax, xor_bx_ax, xor_sp_ax, xor_bp_ax, xor_si_ax, xor_di_ax
	DCD xor_ax_cx, xor_cx_cx, xor_dx_cx, xor_bx_cx, xor_sp_cx, xor_bp_cx, xor_si_cx, xor_di_cx
	DCD xor_ax_dx, xor_cx_dx, xor_dx_dx, xor_bx_dx, xor_sp_dx, xor_bp_dx, xor_si_dx, xor_di_dx
	DCD xor_ax_bx, xor_cx_bx, xor_dx_bx, xor_bx_bx, xor_sp_bx, xor_bp_bx, xor_si_bx, xor_di_bx
	DCD xor_ax_sp, xor_cx_sp, xor_dx_sp, xor_bx_sp, xor_sp_sp, xor_bp_sp, xor_si_sp, xor_di_sp
	DCD xor_ax_bp, xor_cx_bp, xor_dx_bp, xor_bx_bp, xor_sp_bp, xor_bp_bp, xor_si_bp, xor_di_bp
	DCD xor_ax_si, xor_cx_si, xor_dx_si, xor_bx_si, xor_sp_si, xor_bp_si, xor_si_si, xor_di_si
	DCD xor_ax_di, xor_cx_di, xor_dx_di, xor_bx_di, xor_sp_di, xor_bp_di, xor_si_di, xor_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xor_r0_r16_reg $reg
	EXTERN	op_31_EGA_r2_$reg
	GLOBAL	xor_r0_r16_bp_$reg
xor_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r0_r16_$reg
xor_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_31_RAM_$reg, op_31_EGA_r2_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_31_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r3, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eors	r0, r3, r0, lsl #16
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop
	MEND

	xor_r0_r16_reg eax
	xor_r0_r16_reg ecx
	xor_r0_r16_reg edx
	xor_r0_r16_reg ebx
	xor_r0_r16_reg esp
	xor_r0_r16_reg ebp
	xor_r0_r16_reg esi
	xor_r0_r16_reg edi

	LTORG

	modrm_1_genall xor, xor_r0_r16
	
	
; ------------------- 32 = XOR r8, r/m8 -----------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual eors operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_32
op_32
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_32_jump
	modrm_tbl_2 xor
	DCD xor_al_al, xor_al_cl, xor_al_dl, xor_al_bl, xor_al_ah, xor_al_ch, xor_al_dh, xor_al_bh
	DCD xor_cl_al, xor_cl_cl, xor_cl_dl, xor_cl_bl, xor_cl_ah, xor_cl_ch, xor_cl_dh, xor_cl_bh
	DCD xor_dl_al, xor_dl_cl, xor_dl_dl, xor_dl_bl, xor_dl_ah, xor_dl_ch, xor_dl_dh, xor_dl_bh
	DCD xor_bl_al, xor_bl_cl, xor_bl_dl, xor_bl_bl, xor_bl_ah, xor_bl_ch, xor_bl_dh, xor_bl_bh
	DCD xor_ah_al, xor_ah_cl, xor_ah_dl, xor_ah_bl, xor_ah_ah, xor_ah_ch, xor_ah_dh, xor_ah_bh
	DCD xor_ch_al, xor_ch_cl, xor_ch_dl, xor_ch_bl, xor_ch_ah, xor_ch_ch, xor_ch_dh, xor_ch_bh
	DCD xor_dh_al, xor_dh_cl, xor_dh_dl, xor_dh_bl, xor_dh_ah, xor_dh_ch, xor_dh_dh, xor_dh_bh
	DCD xor_bh_al, xor_bh_cl, xor_bh_dl, xor_bh_bl, xor_bh_ah, xor_bh_ch, xor_bh_dh, xor_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xor_reg8l_r0high $reg
	EXTERN	op_32_EGA_l_$reg
	EXTERN	op_32_MODEX_l_$reg
	GLOBAL	xor_r8l_r0_bp_$reg
xor_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r8l_r0_$reg
xor_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_32_RAM_l_$reg, op_32_EGA_l_$reg, op_32_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_32_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1,$reg, lsl #24
	eors	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	b		loop
	MEND
	MACRO 
	xor_reg8h_r0high $reg
	EXTERN	op_32_EGA_h_$reg
	EXTERN	op_32_MODEX_h_$reg
	GLOBAL	xor_r8h_r0_bp_$reg
xor_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r8h_r0_$reg
xor_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_32_RAM_h_$reg, op_32_EGA_h_$reg, op_32_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_32_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1,$reg,#0xFF00
	lsl		r1, #16
	eors	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16
	b		loop
	MEND

	xor_reg8l_r0high eax
	xor_reg8l_r0high ecx
	xor_reg8l_r0high edx
	xor_reg8l_r0high ebx
	xor_reg8h_r0high eax
	xor_reg8h_r0high ecx
	xor_reg8h_r0high edx
	xor_reg8h_r0high ebx

	LTORG

	modrm_2_genall xor


; --- Register operands ---

	MACRO 
	xor_reg8l_reg8l $reg1, $reg2
	mov		r0, $reg1, lsl #24
	mov		r1, $reg2, lsl #24
	eors	r0, r1
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		loop
	MEND
	
	MACRO 
	xor_reg8l_reg8h $reg1, $reg2
	mov		r0, $reg1, lsl #24
	and		r1, $reg2, #0xFF00
	eors	r0, r1, lsl #16
	bic		$reg1, #0xFF			; Clear the current reg8l value
	orr		$reg1, r0, lsr #24		; and replace it with r0
	b		loop
	MEND
	
	MACRO 
	xor_reg8h_reg8l $reg1, $reg2
	and		r0, $reg1, #0xFF00
	mov		r1, $reg2, lsl #24
	eors	r0, r1, r0, lsl #16
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		loop
	MEND
	
	MACRO 
	xor_reg8h_reg8h $reg1, $reg2
	and		r0, $reg1, #0xFF00
	and		r1, $reg2, #0xFF00
	lsl		r0, #16
	eors	r0, r1, lsl #16
	bic		$reg1, #0xFF00			; Clear the current reg8h value
	orr		$reg1, r0, lsr #16		; and replace it with r0
	b		loop
	MEND

xor_al_al
	xor_reg8l_reg8l eax, eax
xor_al_cl
	xor_reg8l_reg8l eax, ecx
xor_al_dl
	xor_reg8l_reg8l eax, edx
xor_al_bl
	xor_reg8l_reg8l eax, ebx
xor_al_ah
	xor_reg8l_reg8h eax, eax
xor_al_ch
	xor_reg8l_reg8h eax, ecx
xor_al_dh
	xor_reg8l_reg8h eax, edx
xor_al_bh
	xor_reg8l_reg8h eax, ebx
xor_cl_al
	xor_reg8l_reg8l ecx, eax
xor_cl_cl
	xor_reg8l_reg8l ecx, ecx
xor_cl_dl
	xor_reg8l_reg8l ecx, edx
xor_cl_bl
	xor_reg8l_reg8l ecx, ebx
xor_cl_ah
	xor_reg8l_reg8h ecx, eax
xor_cl_ch
	xor_reg8l_reg8h ecx, ecx
xor_cl_dh
	xor_reg8l_reg8h ecx, edx
xor_cl_bh
	xor_reg8l_reg8h ecx, ebx
xor_dl_al
	xor_reg8l_reg8l edx, eax
xor_dl_cl
	xor_reg8l_reg8l edx, ecx
xor_dl_dl
	xor_reg8l_reg8l edx, edx
xor_dl_bl
	xor_reg8l_reg8l edx, ebx
xor_dl_ah
	xor_reg8l_reg8h edx, eax
xor_dl_ch
	xor_reg8l_reg8h edx, ecx
xor_dl_dh
	xor_reg8l_reg8h edx, edx
xor_dl_bh
	xor_reg8l_reg8h edx, ebx
xor_bl_al
	xor_reg8l_reg8l ebx, eax
xor_bl_cl
	xor_reg8l_reg8l ebx, ecx
xor_bl_dl
	xor_reg8l_reg8l ebx, edx
xor_bl_bl
	xor_reg8l_reg8l ebx, ebx
xor_bl_ah
	xor_reg8l_reg8h ebx, eax
xor_bl_ch
	xor_reg8l_reg8h ebx, ecx
xor_bl_dh
	xor_reg8l_reg8h ebx, edx
xor_bl_bh
	xor_reg8l_reg8h ebx, ebx

xor_ah_al
	xor_reg8h_reg8l eax, eax
xor_ah_cl
	xor_reg8h_reg8l eax, ecx
xor_ah_dl
	xor_reg8h_reg8l eax, edx
xor_ah_bl
	xor_reg8h_reg8l eax, ebx
xor_ah_ah
	xor_reg8h_reg8h eax, eax
xor_ah_ch
	xor_reg8h_reg8h eax, ecx
xor_ah_dh
	xor_reg8h_reg8h eax, edx
xor_ah_bh
	xor_reg8h_reg8h eax, ebx
xor_ch_al
	xor_reg8h_reg8l ecx, eax
xor_ch_cl
	xor_reg8h_reg8l ecx, ecx
xor_ch_dl
	xor_reg8h_reg8l ecx, edx
xor_ch_bl
	xor_reg8h_reg8l ecx, ebx
xor_ch_ah
	xor_reg8h_reg8h ecx, eax
xor_ch_ch
	xor_reg8h_reg8h ecx, ecx
xor_ch_dh
	xor_reg8h_reg8h ecx, edx
xor_ch_bh
	xor_reg8h_reg8h ecx, ebx
xor_dh_al
	xor_reg8h_reg8l edx, eax
xor_dh_cl
	xor_reg8h_reg8l edx, ecx
xor_dh_dl
	xor_reg8h_reg8l edx, edx
xor_dh_bl
	xor_reg8h_reg8l edx, ebx
xor_dh_ah
	xor_reg8h_reg8h edx, eax
xor_dh_ch
	xor_reg8h_reg8h edx, ecx
xor_dh_dh
	xor_reg8h_reg8h edx, edx
xor_dh_bh
	xor_reg8h_reg8h edx, ebx
xor_bh_al
	xor_reg8h_reg8l ebx, eax
xor_bh_cl
	xor_reg8h_reg8l ebx, ecx
xor_bh_dl
	xor_reg8h_reg8l ebx, edx
xor_bh_bl
	xor_reg8h_reg8l ebx, ebx
xor_bh_ah
	xor_reg8h_reg8h ebx, eax
xor_bh_ch
	xor_reg8h_reg8h ebx, ecx
xor_bh_dh
	xor_reg8h_reg8h ebx, edx
xor_bh_bh
	xor_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  xor_al_al
	GLOBAL  xor_al_cl
	GLOBAL  xor_al_dl
	GLOBAL  xor_al_bl
	GLOBAL  xor_al_ah
	GLOBAL  xor_al_ch
	GLOBAL  xor_al_dh
	GLOBAL  xor_al_bh
	GLOBAL  xor_cl_al
	GLOBAL  xor_cl_cl
	GLOBAL  xor_cl_dl
	GLOBAL  xor_cl_bl
	GLOBAL  xor_cl_ah
	GLOBAL  xor_cl_ch
	GLOBAL  xor_cl_dh
	GLOBAL  xor_cl_bh
	GLOBAL  xor_dl_al
	GLOBAL  xor_dl_cl
	GLOBAL  xor_dl_dl
	GLOBAL  xor_dl_bl
	GLOBAL  xor_dl_ah
	GLOBAL  xor_dl_ch
	GLOBAL  xor_dl_dh
	GLOBAL  xor_dl_bh
	GLOBAL  xor_bl_al
	GLOBAL  xor_bl_cl
	GLOBAL  xor_bl_dl
	GLOBAL  xor_bl_bl
	GLOBAL  xor_bl_ah
	GLOBAL  xor_bl_ch
	GLOBAL  xor_bl_dh
	GLOBAL  xor_bl_bh
	GLOBAL  xor_ah_al
	GLOBAL  xor_ah_cl
	GLOBAL  xor_ah_dl
	GLOBAL  xor_ah_bl
	GLOBAL  xor_ah_ah
	GLOBAL  xor_ah_ch
	GLOBAL  xor_ah_dh
	GLOBAL  xor_ah_bh
	GLOBAL  xor_ch_al
	GLOBAL  xor_ch_cl
	GLOBAL  xor_ch_dl
	GLOBAL  xor_ch_bl
	GLOBAL  xor_ch_ah
	GLOBAL  xor_ch_ch
	GLOBAL  xor_ch_dh
	GLOBAL  xor_ch_bh
	GLOBAL  xor_dh_al
	GLOBAL  xor_dh_cl
	GLOBAL  xor_dh_dl
	GLOBAL  xor_dh_bl
	GLOBAL  xor_dh_ah
	GLOBAL  xor_dh_ch
	GLOBAL  xor_dh_dh
	GLOBAL  xor_dh_bh
	GLOBAL  xor_bh_al
	GLOBAL  xor_bh_cl
	GLOBAL  xor_bh_dl
	GLOBAL  xor_bh_bl
	GLOBAL  xor_bh_ah
	GLOBAL  xor_bh_ch
	GLOBAL  xor_bh_dh
	GLOBAL  xor_bh_bh

	
; ------------------- 33 = XOR r16, r/m16 -----------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual eors operation, so C and O work like in x86.
;
; All modrm variations supported!
;
;
	GLOBAL	op_33
op_33
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_33_jump
	modrm_tbl_3_old xor
	DCD xor_ax_ax, xor_ax_cx, xor_ax_dx, xor_ax_bx, xor_ax_sp, xor_ax_bp, xor_ax_si, xor_ax_di
	DCD xor_cx_ax, xor_cx_cx, xor_cx_dx, xor_cx_bx, xor_cx_sp, xor_cx_bp, xor_cx_si, xor_cx_di
	DCD xor_dx_ax, xor_dx_cx, xor_dx_dx, xor_dx_bx, xor_dx_sp, xor_dx_bp, xor_dx_si, xor_dx_di
	DCD xor_bx_ax, xor_bx_cx, xor_bx_dx, xor_bx_bx, xor_bx_sp, xor_bx_bp, xor_bx_si, xor_bx_di
	DCD xor_sp_ax, xor_sp_cx, xor_sp_dx, xor_sp_bx, xor_sp_sp, xor_sp_bp, xor_sp_si, xor_sp_di
	DCD xor_bp_ax, xor_bp_cx, xor_bp_dx, xor_bp_bx, xor_bp_sp, xor_bp_bp, xor_bp_si, xor_bp_di
	DCD xor_si_ax, xor_si_cx, xor_si_dx, xor_si_bx, xor_si_sp, xor_si_bp, xor_si_si, xor_si_di
	DCD xor_di_ax, xor_di_cx, xor_di_dx, xor_di_bx, xor_di_sp, xor_di_bp, xor_di_si, xor_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xor_reg16_r0high $reg
	EXTERN	op_33_EGA_$reg
	GLOBAL	xor_r16_r0_bp_$reg
xor_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xor_r16_r0_$reg
xor_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_33_RAM_$reg, op_33_EGA_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_33_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eors	r0, r2, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	xor_reg16_r0high eax
	xor_reg16_r0high ecx
	xor_reg16_r0high edx
	xor_reg16_r0high ebx
	xor_reg16_r0high esp
	xor_reg16_r0high ebp
	xor_reg16_r0high esi
	xor_reg16_r0high edi

	LTORG

	modrm_3_genall_old xor, xor_r16_r0


; --- registers ---
	
	MACRO 
	xor_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	mov		r1, $rr, lsl #16
	eor		$rl, r0, lsr #16
	eors	r0, r1
	orr		$rl, r0, lsr #16
	b		loop
	MEND

xor_ax_ax
	xor_reg16_reg16		eax, eax
xor_ax_cx
	xor_reg16_reg16		eax, ecx
xor_ax_dx
	xor_reg16_reg16		eax, edx
xor_ax_bx
	xor_reg16_reg16		eax, ebx
xor_ax_sp
	xor_reg16_reg16		eax, esp
xor_ax_bp
	xor_reg16_reg16		eax, ebp
xor_ax_si
	xor_reg16_reg16		eax, esi
xor_ax_di
	xor_reg16_reg16		eax, edi
xor_cx_ax
	xor_reg16_reg16		ecx, eax
xor_cx_cx
	xor_reg16_reg16		ecx, ecx
xor_cx_dx
	xor_reg16_reg16		ecx, edx
xor_cx_bx
	xor_reg16_reg16		ecx, ebx
xor_cx_sp
	xor_reg16_reg16		ecx, esp
xor_cx_bp
	xor_reg16_reg16		ecx, ebp
xor_cx_si
	xor_reg16_reg16		ecx, esi
xor_cx_di
	xor_reg16_reg16		ecx, edi
xor_dx_ax
	xor_reg16_reg16		edx, eax
xor_dx_cx
	xor_reg16_reg16		edx, ecx
xor_dx_dx
	xor_reg16_reg16		edx, edx
xor_dx_bx
	xor_reg16_reg16		edx, ebx
xor_dx_sp
	xor_reg16_reg16		edx, esp
xor_dx_bp
	xor_reg16_reg16		edx, ebp
xor_dx_si
	xor_reg16_reg16		edx, esi
xor_dx_di
	xor_reg16_reg16		edx, edi
xor_bx_ax
	xor_reg16_reg16		ebx, eax
xor_bx_cx
	xor_reg16_reg16		ebx, ecx
xor_bx_dx
	xor_reg16_reg16		ebx, edx
xor_bx_bx
	xor_reg16_reg16		ebx, ebx
xor_bx_sp
	xor_reg16_reg16		ebx, esp
xor_bx_bp
	xor_reg16_reg16		ebx, ebp
xor_bx_si
	xor_reg16_reg16		ebx, esi
xor_bx_di
	xor_reg16_reg16		ebx, edi
xor_sp_ax
	xor_reg16_reg16		esp, eax
xor_sp_cx
	xor_reg16_reg16		esp, ecx
xor_sp_dx
	xor_reg16_reg16		esp, edx
xor_sp_bx
	xor_reg16_reg16		esp, ebx
xor_sp_sp
	xor_reg16_reg16		esp, esp
xor_sp_bp
	xor_reg16_reg16		esp, ebp
xor_sp_si
	xor_reg16_reg16		esp, esi
xor_sp_di
	xor_reg16_reg16		esp, edi
xor_bp_ax
	xor_reg16_reg16		ebp, eax
xor_bp_cx
	xor_reg16_reg16		ebp, ecx
xor_bp_dx
	xor_reg16_reg16		ebp, edx
xor_bp_bx
	xor_reg16_reg16		ebp, ebx
xor_bp_sp
	xor_reg16_reg16		ebp, esp
xor_bp_bp
	xor_reg16_reg16		ebp, ebp
xor_bp_si
	xor_reg16_reg16		ebp, esi
xor_bp_di
	xor_reg16_reg16		ebp, edi
xor_si_ax
	xor_reg16_reg16		esi, eax
xor_si_cx
	xor_reg16_reg16		esi, ecx
xor_si_dx
	xor_reg16_reg16		esi, edx
xor_si_bx
	xor_reg16_reg16		esi, ebx
xor_si_sp
	xor_reg16_reg16		esi, esp
xor_si_bp
	xor_reg16_reg16		esi, ebp
xor_si_si
	xor_reg16_reg16		esi, esi
xor_si_di
	xor_reg16_reg16		esi, edi
xor_di_ax
	xor_reg16_reg16		edi, eax
xor_di_cx
	xor_reg16_reg16		edi, ecx
xor_di_dx
	xor_reg16_reg16		edi, edx
xor_di_bx
	xor_reg16_reg16		edi, ebx
xor_di_sp
	xor_reg16_reg16		edi, esp
xor_di_bp
	xor_reg16_reg16		edi, ebp
xor_di_si
	xor_reg16_reg16		edi, esi
xor_di_di
	xor_reg16_reg16		edi, edi

	GLOBAL  xor_ax_ax
	GLOBAL  xor_ax_cx
	GLOBAL  xor_ax_dx
	GLOBAL  xor_ax_bx
	GLOBAL  xor_ax_sp
	GLOBAL  xor_ax_bp
	GLOBAL  xor_ax_si
	GLOBAL  xor_ax_di
	GLOBAL  xor_cx_ax
	GLOBAL  xor_cx_cx
	GLOBAL  xor_cx_dx
	GLOBAL  xor_cx_bx
	GLOBAL  xor_cx_sp
	GLOBAL  xor_cx_bp
	GLOBAL  xor_cx_si
	GLOBAL  xor_cx_di
	GLOBAL  xor_dx_ax
	GLOBAL  xor_dx_cx
	GLOBAL  xor_dx_dx
	GLOBAL  xor_dx_bx
	GLOBAL  xor_dx_sp
	GLOBAL  xor_dx_bp
	GLOBAL  xor_dx_si
	GLOBAL  xor_dx_di
	GLOBAL  xor_bx_ax
	GLOBAL  xor_bx_cx
	GLOBAL  xor_bx_dx
	GLOBAL  xor_bx_bx
	GLOBAL  xor_bx_sp
	GLOBAL  xor_bx_bp
	GLOBAL  xor_bx_si
	GLOBAL  xor_bx_di
	GLOBAL  xor_sp_ax
	GLOBAL  xor_sp_cx
	GLOBAL  xor_sp_dx
	GLOBAL  xor_sp_bx
	GLOBAL  xor_sp_sp
	GLOBAL  xor_sp_bp
	GLOBAL  xor_sp_si
	GLOBAL  xor_sp_di
	GLOBAL  xor_bp_ax
	GLOBAL  xor_bp_cx
	GLOBAL  xor_bp_dx
	GLOBAL  xor_bp_bx
	GLOBAL  xor_bp_sp
	GLOBAL  xor_bp_bp
	GLOBAL  xor_bp_si
	GLOBAL  xor_bp_di
	GLOBAL  xor_si_ax
	GLOBAL  xor_si_cx
	GLOBAL  xor_si_dx
	GLOBAL  xor_si_bx
	GLOBAL  xor_si_sp
	GLOBAL  xor_si_bp
	GLOBAL  xor_si_si
	GLOBAL  xor_si_di
	GLOBAL  xor_di_ax
	GLOBAL  xor_di_cx
	GLOBAL  xor_di_dx
	GLOBAL  xor_di_bx
	GLOBAL  xor_di_sp
	GLOBAL  xor_di_bp
	GLOBAL  xor_di_si
	GLOBAL  xor_di_di


; ------------------- 34 = XOR AL,imm8 --------------------------------
;
op_34
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	ldrb	r1,[r12],#1				; Load the second opcode byte to r1, increment r12 by 1
	mov		r0, eax, lsl #24
	bic		eax, #0xFF				; Clear the current AL value
	eors	r0, r1, lsl #24
	orr		eax, r0, lsr #24		; and replace it with r0
	b		loop

; ------------------- 35 = XOR AX,imm16 --------------------------------
;
op_35
	ldrb	r0,[r12],#1				; Load low byte of imm16
	ldrb	r1,[r12],#1				; Load high byte
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially C and O)
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	mov		r1, eax, lsl #16
	eor		eax, r1, lsr #16
	eors	r0, r1, r0, lsl #16
	orr		eax, r0, lsr #16
	b		loop

	ALIGN	4

; ------------------- 37 = AAA ----------------------------------------
;
; Adjusts the sum of two unpacked BCD values to create an unpacked BCD result.
;
; if((AL & 0xF) > 9 || AF == 1) {
;	AL = AL + 6;
;	AH = AH + 1;
;	AF = 1;
;	CF = 1
; }
; else {
;	AF = 0;
;	CF = 0;
; }
; AL = AL & 0xF;
;
op_37
	IF 1 = 1
	mov		r0, #0								; Default to all ARM flags being clear
	;------
	; SETFLAGBIT(SF,((reg_al>=0x7a) && (reg_al<=0xf9)));
	;------
	and		r1, eax, #0xFF
	cmp		r1, #0xF9
	bgt		%f1
	cmp		r1, #0x7A
	orrge	r0, #ARM_NEG
	;------
	; if ((reg_al & 0xf) > 9)
	;------
1	and		r1, eax, #0x0F
	cmp		r1, #0x09
	ble		%f5									;	{
	and		r1, eax, #0xF0
	cmp		r1, #0x70							;		SETFLAGBIT(OF,(reg_al&0xf0)==0x70);
	orreq	r0, #ARM_OVER
	add		eax, #0x0100
	add		eax, #0x0006						;		reg_ax += 0x106;
	orr		r0, #ARM_CARRY						;		SETFLAGBIT(CF,true);
5	tst		eax, #0xFF
	orreq	r0, #ARM_ZERO						;	} SETFLAGBIT(ZF,(reg_al == 0));
	;------
	; SETFLAGBIT(PF,parity_lookup[reg_al]);
	;------
	;------
	; reg_al &= 0x0F;
	;------
	bic		eax, #0x00F0
	b		restore_flags_from_r0
	ELSE
	ldr		r2, [sp, #SP_FLAGS]					; r2 = current x86 flags (for FLAG_AF)
	mov		r0, #0								; Default to all ARM flags being clear
	;------
	; SETFLAGBIT(SF,((reg_al>=0x7a) && (reg_al<=0xf9)));
	;------
	and		r1, eax, #0xFF
	cmp		r1, #0xF9
	bgt		%f1
	cmp		r1, #0x7A
	orrge	r0, #ARM_NEG
	;------
	; if ((reg_al & 0xf) > 9)
	;------
1	and		r1, eax, #0x0F
	cmp		r1, #9
	ble		%f2									;	{
	and		r1, eax, #0xF0
	cmp		r1, #0x70							;		SETFLAGBIT(OF,(reg_al&0xf0)==0x70);
	orreq	r0, #ARM_OVER
	ror		eax, #16
	add		eax, #0x01000000
	add		eax, #0x00060000					;		reg_ax += 0x106;
	ror		eax, #16
	orr		r0, #ARM_CARRY						;		SETFLAGBIT(CF,true);
	orr		r2, #FLAG_AF						;		SETFLAGBIT(AF,true);
	tst		eax, #0xFF
	orreq	r0, #ARM_ZERO						;		SETFLAGBIT(ZF,(reg_al == 0));
	b		%f4									;	}
	;------
	; else if (get_AF())
	;------
2	tst		r2, #FLAG_AF
	beq		%f5									;	{
	ror		eax, #16
	add		eax, #0x01000000
	add		eax, #0x00060000					;		reg_ax += 0x106;
	ror		eax, #16
	orr		r0, #ARM_CARRY						;		SETFLAGBIT(CF,true);
	orr		r2, #FLAG_AF						;		SETFLAGBIT(AF,true);
	b		%f4
	;------
	; else
	;------
5	tst		eax, #0xFF
	orreq	r0, #ARM_ZERO						;		SETFLAGBIT(ZF,(reg_al == 0)); }
	;------
	; SETFLAGBIT(PF,parity_lookup[reg_al]);
	;------
4	
	;------
	; reg_al &= 0x0F;
	;------
	bic		eax, #0xF0
	str		r2, [sp, #SP_FLAGS]					; Save FLAG_AF
	b		restore_flags_from_r0
	ENDIF
	
; ------------------- 38 = CMP r/m8,esp --------------------------------
;
; All modrm variations supported!
;
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
	GLOBAL	op_38
op_38
	modrm_jump_16_tbl op_38_jump
	modrm_tbl_0	cmp
	DCD cmp_al_al, cmp_cl_al, cmp_dl_al, cmp_bl_al, cmp_ah_al, cmp_ch_al, cmp_dh_al, cmp_bh_al
	DCD cmp_al_cl, cmp_cl_cl, cmp_dl_cl, cmp_bl_cl, cmp_ah_cl, cmp_ch_cl, cmp_dh_cl, cmp_bh_cl
	DCD cmp_al_dl, cmp_cl_dl, cmp_dl_dl, cmp_bl_dl, cmp_ah_dl, cmp_ch_dl, cmp_dh_dl, cmp_bh_dl
	DCD cmp_al_bl, cmp_cl_bl, cmp_dl_bl, cmp_bl_bl, cmp_ah_bl, cmp_ch_bl, cmp_dh_bl, cmp_bh_bl
	DCD cmp_al_ah, cmp_cl_ah, cmp_dl_ah, cmp_bl_ah, cmp_ah_ah, cmp_ch_ah, cmp_dh_ah, cmp_bh_ah
	DCD cmp_al_ch, cmp_cl_ch, cmp_dl_ch, cmp_bl_ch, cmp_ah_ch, cmp_ch_ch, cmp_dh_ch, cmp_bh_ch
	DCD cmp_al_dh, cmp_cl_dh, cmp_dl_dh, cmp_bl_dh, cmp_ah_dh, cmp_ch_dh, cmp_dh_dh, cmp_bh_dh
	DCD cmp_al_bh, cmp_cl_bh, cmp_dl_bh, cmp_bl_bh, cmp_ah_bh, cmp_ch_bh, cmp_dh_bh, cmp_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	cmp_r0_reg8l $reg
	EXTERN	op_38_EGA_l_$reg
	EXTERN	op_38_MODEX_l_$reg
	GLOBAL	cmp_r0_r8l_bp_$reg
cmp_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r0_r8l_$reg
cmp_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_38_RAM_l_$reg, op_38_EGA_l_$reg, op_38_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_38_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	rsbs	r1, r0, lsl #24
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND
	MACRO 
	cmp_r0_reg8h $reg
	EXTERN	op_38_EGA_h_$reg
	EXTERN	op_38_MODEX_h_$reg
	GLOBAL	cmp_r0_r8h_bp_$reg
cmp_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r0_r8h_$reg
cmp_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_38_RAM_h_$reg, op_38_EGA_h_$reg, op_38_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_38_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	rsbs	r1, r0, lsl #24
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	cmp_r0_reg8l eax
	cmp_r0_reg8l ecx
	cmp_r0_reg8l edx
	cmp_r0_reg8l ebx
	cmp_r0_reg8h eax
	cmp_r0_reg8h ecx
	cmp_r0_reg8h edx
	cmp_r0_reg8h ebx

	modrm_0_genall cmp


; ------------------- 39 = CMP r/m16,r16 --------------------------------
;
; All modrm variations supported!
;
;
	GLOBAL	op_39
op_39
	modrm_jump_16_tbl op_39_jump
	modrm_tbl_1_old cmp
	DCD cmp_ax_ax, cmp_cx_ax, cmp_dx_ax, cmp_bx_ax, cmp_sp_ax, cmp_bp_ax, cmp_si_ax, cmp_di_ax
	DCD cmp_ax_cx, cmp_cx_cx, cmp_dx_cx, cmp_bx_cx, cmp_sp_cx, cmp_bp_cx, cmp_si_cx, cmp_di_cx
	DCD cmp_ax_dx, cmp_cx_dx, cmp_dx_dx, cmp_bx_dx, cmp_sp_dx, cmp_bp_dx, cmp_si_dx, cmp_di_dx
	DCD cmp_ax_bx, cmp_cx_bx, cmp_dx_bx, cmp_bx_bx, cmp_sp_bx, cmp_bp_bx, cmp_si_bx, cmp_di_bx
	DCD cmp_ax_sp, cmp_cx_sp, cmp_dx_sp, cmp_bx_sp, cmp_sp_sp, cmp_bp_sp, cmp_si_sp, cmp_di_sp
	DCD cmp_ax_bp, cmp_cx_bp, cmp_dx_bp, cmp_bx_bp, cmp_sp_bp, cmp_bp_bp, cmp_si_bp, cmp_di_bp
	DCD cmp_ax_si, cmp_cx_si, cmp_dx_si, cmp_bx_si, cmp_sp_si, cmp_bp_si, cmp_si_si, cmp_di_si
	DCD cmp_ax_di, cmp_cx_di, cmp_dx_di, cmp_bx_di, cmp_sp_di, cmp_bp_di, cmp_si_di, cmp_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	cmp_r0_r16_reg $reg
	EXTERN	op_39_EGA_r2_$reg
	GLOBAL	cmp_r0_r16_bp_$reg
cmp_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r0_r16_$reg
cmp_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_39_RAM_$reg, op_39_EGA_r2_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_39_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	rsbs	r0, r2, r0, lsl #16		; Compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	cmp_r0_r16_reg eax
	cmp_r0_r16_reg ecx
	cmp_r0_r16_reg edx
	cmp_r0_r16_reg ebx
	cmp_r0_r16_reg esp
	cmp_r0_r16_reg ebp
	cmp_r0_r16_reg esi
	cmp_r0_r16_reg edi

	LTORG

	modrm_1_genall cmp, cmp_r0_r16


; ------------------- 3A = CMP r8,r/m8 ------------------------------
;
; All modrm variations supported!
;
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
	GLOBAL	op_3a
op_3a
	modrm_jump_16_tbl op_3a_jump
	modrm_tbl_2 cmp
	DCD cmp_al_al, cmp_al_cl, cmp_al_dl, cmp_al_bl, cmp_al_ah, cmp_al_ch, cmp_al_dh, cmp_al_bh
	DCD cmp_cl_al, cmp_cl_cl, cmp_cl_dl, cmp_cl_bl, cmp_cl_ah, cmp_cl_ch, cmp_cl_dh, cmp_cl_bh
	DCD cmp_dl_al, cmp_dl_cl, cmp_dl_dl, cmp_dl_bl, cmp_dl_ah, cmp_dl_ch, cmp_dl_dh, cmp_dl_bh
	DCD cmp_bl_al, cmp_bl_cl, cmp_bl_dl, cmp_bl_bl, cmp_bl_ah, cmp_bl_ch, cmp_bl_dh, cmp_bl_bh
	DCD cmp_ah_al, cmp_ah_cl, cmp_ah_dl, cmp_ah_bl, cmp_ah_ah, cmp_ah_ch, cmp_ah_dh, cmp_ah_bh
	DCD cmp_ch_al, cmp_ch_cl, cmp_ch_dl, cmp_ch_bl, cmp_ch_ah, cmp_ch_ch, cmp_ch_dh, cmp_ch_bh
	DCD cmp_dh_al, cmp_dh_cl, cmp_dh_dl, cmp_dh_bl, cmp_dh_ah, cmp_dh_ch, cmp_dh_dh, cmp_dh_bh
	DCD cmp_bh_al, cmp_bh_cl, cmp_bh_dl, cmp_bh_bl, cmp_bh_ah, cmp_bh_ch, cmp_bh_dh, cmp_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	cmp_reg8l_r0high $reg
	EXTERN	op_3a_EGA_l_$reg
	EXTERN	op_3a_MODEX_l_$reg
	GLOBAL	cmp_r8l_r0_bp_$reg
cmp_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r8l_r0_$reg
cmp_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_3a_RAM_l_$reg, op_3a_EGA_l_$reg, op_3a_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_3a_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1,$reg, lsl #24
	cmp		r1, r0, lsl #24
	b		complement_carry
	MEND
	MACRO 
	cmp_reg8h_r0high $reg
	EXTERN	op_3a_EGA_h_$reg
	EXTERN	op_3a_MODEX_h_$reg
	GLOBAL	cmp_r8h_r0_bp_$reg
cmp_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r8h_r0_$reg
cmp_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_3a_RAM_h_$reg, op_3a_EGA_h_$reg, op_3a_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_3a_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00		; Left operand already uses just the rightmost byte
	lsl		r1, #16
	cmp		r1, r0, lsl #24			; Perform the addition using the highest bytes to get the correct flags
	b		complement_carry
	MEND

	cmp_reg8l_r0high eax
	cmp_reg8l_r0high ecx
	cmp_reg8l_r0high edx
	cmp_reg8l_r0high ebx
	cmp_reg8h_r0high eax
	cmp_reg8h_r0high ecx
	cmp_reg8h_r0high edx
	cmp_reg8h_r0high ebx

	LTORG

	modrm_2_genall cmp


; --- registers ---

	MACRO
	cmp_reg8l_reg8l $rl, $rr
	mov		r1, $rl, lsl #24
	cmp		r1, $rr, lsl #24
	b		complement_carry
	MEND

	MACRO 
	cmp_reg8l_reg8h $rl, $rr
	mov		r0, $rl, lsl #24
	and		r1, $rr, #0xFF00
	cmp		r0, r1, lsl #16
	b		complement_carry
	MEND

	MACRO 
	cmp_reg8h_reg8l $rl, $rr
	and		r1, $rl, #0xFF00
	lsl		r1, #16
	cmp		r1, $rr, lsl #24
	b		complement_carry
	MEND

	MACRO 
	cmp_reg8h_reg8h $rl, $rr
	and		r0, $rl, #0xFF00
	lsl		r0, #16
	and		r1, $rr, #0xFF00
	cmp		r0, r1, lsl #16
	b		complement_carry
	MEND

cmp_al_al
	cmp_reg8l_reg8l eax, eax
cmp_al_cl
	cmp_reg8l_reg8l eax, ecx
cmp_al_dl
	cmp_reg8l_reg8l eax, edx
cmp_al_bl
	cmp_reg8l_reg8l eax, ebx
cmp_al_ah
	cmp_reg8l_reg8h eax, eax
cmp_al_ch
	cmp_reg8l_reg8h eax, ecx
cmp_al_dh
	cmp_reg8l_reg8h eax, edx
cmp_al_bh
	cmp_reg8l_reg8h eax, ebx
cmp_cl_al
	cmp_reg8l_reg8l ecx, eax
cmp_cl_cl
	cmp_reg8l_reg8l ecx, ecx
cmp_cl_dl
	cmp_reg8l_reg8l ecx, edx
cmp_cl_bl
	cmp_reg8l_reg8l ecx, ebx
cmp_cl_ah
	cmp_reg8l_reg8h ecx, eax
cmp_cl_ch
	cmp_reg8l_reg8h ecx, ecx
cmp_cl_dh
	cmp_reg8l_reg8h ecx, edx
cmp_cl_bh
	cmp_reg8l_reg8h ecx, ebx
cmp_dl_al
	cmp_reg8l_reg8l edx, eax
cmp_dl_cl
	cmp_reg8l_reg8l edx, ecx
cmp_dl_dl
	cmp_reg8l_reg8l edx, edx
cmp_dl_bl
	cmp_reg8l_reg8l edx, ebx
cmp_dl_ah
	cmp_reg8l_reg8h edx, eax
cmp_dl_ch
	cmp_reg8l_reg8h edx, ecx
cmp_dl_dh
	cmp_reg8l_reg8h edx, edx
cmp_dl_bh
	cmp_reg8l_reg8h edx, ebx
cmp_bl_al
	cmp_reg8l_reg8l ebx, eax
cmp_bl_cl
	cmp_reg8l_reg8l ebx, ecx
cmp_bl_dl
	cmp_reg8l_reg8l ebx, edx
cmp_bl_bl
	cmp_reg8l_reg8l ebx, ebx
cmp_bl_ah
	cmp_reg8l_reg8h ebx, eax
cmp_bl_ch
	cmp_reg8l_reg8h ebx, ecx
cmp_bl_dh
	cmp_reg8l_reg8h ebx, edx
cmp_bl_bh
	cmp_reg8l_reg8h ebx, ebx

cmp_ah_al
	cmp_reg8h_reg8l eax, eax
cmp_ah_cl
	cmp_reg8h_reg8l eax, ecx
cmp_ah_dl
	cmp_reg8h_reg8l eax, edx
cmp_ah_bl
	cmp_reg8h_reg8l eax, ebx
cmp_ah_ah
	cmp_reg8h_reg8h eax, eax
cmp_ah_ch
	cmp_reg8h_reg8h eax, ecx
cmp_ah_dh
	cmp_reg8h_reg8h eax, edx
cmp_ah_bh
	cmp_reg8h_reg8h eax, ebx
cmp_ch_al
	cmp_reg8h_reg8l ecx, eax
cmp_ch_cl
	cmp_reg8h_reg8l ecx, ecx
cmp_ch_dl
	cmp_reg8h_reg8l ecx, edx
cmp_ch_bl
	cmp_reg8h_reg8l ecx, ebx
cmp_ch_ah
	cmp_reg8h_reg8h ecx, eax
cmp_ch_ch
	cmp_reg8h_reg8h ecx, ecx
cmp_ch_dh
	cmp_reg8h_reg8h ecx, edx
cmp_ch_bh
	cmp_reg8h_reg8h ecx, ebx
cmp_dh_al
	cmp_reg8h_reg8l edx, eax
cmp_dh_cl
	cmp_reg8h_reg8l edx, ecx
cmp_dh_dl
	cmp_reg8h_reg8l edx, edx
cmp_dh_bl
	cmp_reg8h_reg8l edx, ebx
cmp_dh_ah
	cmp_reg8h_reg8h edx, eax
cmp_dh_ch
	cmp_reg8h_reg8h edx, ecx
cmp_dh_dh
	cmp_reg8h_reg8h edx, edx
cmp_dh_bh
	cmp_reg8h_reg8h edx, ebx
cmp_bh_al
	cmp_reg8h_reg8l ebx, eax
cmp_bh_cl
	cmp_reg8h_reg8l ebx, ecx
cmp_bh_dl
	cmp_reg8h_reg8l ebx, edx
cmp_bh_bl
	cmp_reg8h_reg8l ebx, ebx
cmp_bh_ah
	cmp_reg8h_reg8h ebx, eax
cmp_bh_ch
	cmp_reg8h_reg8h ebx, ecx
cmp_bh_dh
	cmp_reg8h_reg8h ebx, edx
cmp_bh_bh
	cmp_reg8h_reg8h ebx, ebx

; These are called from "cpu_386.s"

	GLOBAL  cmp_al_al
	GLOBAL  cmp_al_cl
	GLOBAL  cmp_al_dl
	GLOBAL  cmp_al_bl
	GLOBAL  cmp_al_ah
	GLOBAL  cmp_al_ch
	GLOBAL  cmp_al_dh
	GLOBAL  cmp_al_bh
	GLOBAL  cmp_cl_al
	GLOBAL  cmp_cl_cl
	GLOBAL  cmp_cl_dl
	GLOBAL  cmp_cl_bl
	GLOBAL  cmp_cl_ah
	GLOBAL  cmp_cl_ch
	GLOBAL  cmp_cl_dh
	GLOBAL  cmp_cl_bh
	GLOBAL  cmp_dl_al
	GLOBAL  cmp_dl_cl
	GLOBAL  cmp_dl_dl
	GLOBAL  cmp_dl_bl
	GLOBAL  cmp_dl_ah
	GLOBAL  cmp_dl_ch
	GLOBAL  cmp_dl_dh
	GLOBAL  cmp_dl_bh
	GLOBAL  cmp_bl_al
	GLOBAL  cmp_bl_cl
	GLOBAL  cmp_bl_dl
	GLOBAL  cmp_bl_bl
	GLOBAL  cmp_bl_ah
	GLOBAL  cmp_bl_ch
	GLOBAL  cmp_bl_dh
	GLOBAL  cmp_bl_bh
	GLOBAL  cmp_ah_al
	GLOBAL  cmp_ah_cl
	GLOBAL  cmp_ah_dl
	GLOBAL  cmp_ah_bl
	GLOBAL  cmp_ah_ah
	GLOBAL  cmp_ah_ch
	GLOBAL  cmp_ah_dh
	GLOBAL  cmp_ah_bh
	GLOBAL  cmp_ch_al
	GLOBAL  cmp_ch_cl
	GLOBAL  cmp_ch_dl
	GLOBAL  cmp_ch_bl
	GLOBAL  cmp_ch_ah
	GLOBAL  cmp_ch_ch
	GLOBAL  cmp_ch_dh
	GLOBAL  cmp_ch_bh
	GLOBAL  cmp_dh_al
	GLOBAL  cmp_dh_cl
	GLOBAL  cmp_dh_dl
	GLOBAL  cmp_dh_bl
	GLOBAL  cmp_dh_ah
	GLOBAL  cmp_dh_ch
	GLOBAL  cmp_dh_dh
	GLOBAL  cmp_dh_bh
	GLOBAL  cmp_bh_al
	GLOBAL  cmp_bh_cl
	GLOBAL  cmp_bh_dl
	GLOBAL  cmp_bh_bl
	GLOBAL  cmp_bh_ah
	GLOBAL  cmp_bh_ch
	GLOBAL  cmp_bh_dh
	GLOBAL  cmp_bh_bh

	
; ------------------- 3B = CMP r16,r/m16 ------------------------------
;
; All modrm variations supported!
;
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
op_3b
	modrm_jump_16_tbl op_3b_jump
	modrm_tbl_3_old cmp
	DCD cmp_ax_ax, cmp_ax_cx, cmp_ax_dx, cmp_ax_bx, cmp_ax_sp, cmp_ax_bp, cmp_ax_si, cmp_ax_di
	DCD cmp_cx_ax, cmp_cx_cx, cmp_cx_dx, cmp_cx_bx, cmp_cx_sp, cmp_cx_bp, cmp_cx_si, cmp_cx_di
	DCD cmp_dx_ax, cmp_dx_cx, cmp_dx_dx, cmp_dx_bx, cmp_dx_sp, cmp_dx_bp, cmp_dx_si, cmp_dx_di
	DCD cmp_bx_ax, cmp_bx_cx, cmp_bx_dx, cmp_bx_bx, cmp_bx_sp, cmp_bx_bp, cmp_bx_si, cmp_bx_di
	DCD cmp_sp_ax, cmp_sp_cx, cmp_sp_dx, cmp_sp_bx, cmp_sp_sp, cmp_sp_bp, cmp_sp_si, cmp_sp_di
	DCD cmp_bp_ax, cmp_bp_cx, cmp_bp_dx, cmp_bp_bx, cmp_bp_sp, cmp_bp_bp, cmp_bp_si, cmp_bp_di
	DCD cmp_si_ax, cmp_si_cx, cmp_si_dx, cmp_si_bx, cmp_si_sp, cmp_si_bp, cmp_si_si, cmp_si_di
	DCD cmp_di_ax, cmp_di_cx, cmp_di_dx, cmp_di_bx, cmp_di_sp, cmp_di_bp, cmp_di_si, cmp_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	cmp_reg16_r0high $reg
	GLOBAL	cmp_r16_r0_bp_$reg
cmp_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	cmp_r16_r0_$reg
cmp_r16_r0_$reg
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_3b_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
op_3b_RAM_$reg
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r1, [r2, #1]			; Load high byte
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	cmp		r2, r0, lsl #16
	b		complement_carry
	MEND

	cmp_reg16_r0high eax
	cmp_reg16_r0high ecx
	cmp_reg16_r0high edx
	cmp_reg16_r0high ebx
	cmp_reg16_r0high esp
	cmp_reg16_r0high ebp
	cmp_reg16_r0high esi
	cmp_reg16_r0high edi

	LTORG

	modrm_3_genall_old cmp, cmp_r16_r0


; --- registers ---
	
	MACRO 
	cmp_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	cmp		r0, $rr, lsl #16
	b		complement_carry
	MEND
	
cmp_ax_ax
	cmp_reg16_reg16		eax, eax
cmp_ax_cx
	cmp_reg16_reg16		eax, ecx
cmp_ax_dx
	cmp_reg16_reg16		eax, edx
cmp_ax_bx
	cmp_reg16_reg16		eax, ebx
cmp_ax_sp
	cmp_reg16_reg16		eax, esp
cmp_ax_bp
	cmp_reg16_reg16		eax, ebp
cmp_ax_si
	cmp_reg16_reg16		eax, esi
cmp_ax_di
	cmp_reg16_reg16		eax, edi
cmp_cx_ax
	cmp_reg16_reg16		ecx, eax
cmp_cx_cx
	cmp_reg16_reg16		ecx, ecx
cmp_cx_dx
	cmp_reg16_reg16		ecx, edx
cmp_cx_bx
	cmp_reg16_reg16		ecx, ebx
cmp_cx_sp
	cmp_reg16_reg16		ecx, esp
cmp_cx_bp
	cmp_reg16_reg16		ecx, ebp
cmp_cx_si
	cmp_reg16_reg16		ecx, esi
cmp_cx_di
	cmp_reg16_reg16		ecx, edi
cmp_dx_ax
	cmp_reg16_reg16		edx, eax
cmp_dx_cx
	cmp_reg16_reg16		edx, ecx
cmp_dx_dx
	cmp_reg16_reg16		edx, edx
cmp_dx_bx
	cmp_reg16_reg16		edx, ebx
cmp_dx_sp
	cmp_reg16_reg16		edx, esp
cmp_dx_bp
	cmp_reg16_reg16		edx, ebp
cmp_dx_si
	cmp_reg16_reg16		edx, esi
cmp_dx_di
	cmp_reg16_reg16		edx, edi
cmp_bx_ax
	cmp_reg16_reg16		ebx, eax
cmp_bx_cx
	cmp_reg16_reg16		ebx, ecx
cmp_bx_dx
	cmp_reg16_reg16		ebx, edx
cmp_bx_bx
	cmp_reg16_reg16		ebx, ebx
cmp_bx_sp
	cmp_reg16_reg16		ebx, esp
cmp_bx_bp
	cmp_reg16_reg16		ebx, ebp
cmp_bx_si
	cmp_reg16_reg16		ebx, esi
cmp_bx_di
	cmp_reg16_reg16		ebx, edi
cmp_sp_ax
	cmp_reg16_reg16		esp, eax
cmp_sp_cx
	cmp_reg16_reg16		esp, ecx
cmp_sp_dx
	cmp_reg16_reg16		esp, edx
cmp_sp_bx
	cmp_reg16_reg16		esp, ebx
cmp_sp_sp
	cmp_reg16_reg16		esp, esp
cmp_sp_bp
	cmp_reg16_reg16		esp, ebp
cmp_sp_si
	cmp_reg16_reg16		esp, esi
cmp_sp_di
	cmp_reg16_reg16		esp, edi
cmp_bp_ax
	cmp_reg16_reg16		ebp, eax
cmp_bp_cx
	cmp_reg16_reg16		ebp, ecx
cmp_bp_dx
	cmp_reg16_reg16		ebp, edx
cmp_bp_bx
	cmp_reg16_reg16		ebp, ebx
cmp_bp_sp
	cmp_reg16_reg16		ebp, esp
cmp_bp_bp
	cmp_reg16_reg16		ebp, ebp
cmp_bp_si
	cmp_reg16_reg16		ebp, esi
cmp_bp_di
	cmp_reg16_reg16		ebp, edi
cmp_si_ax
	cmp_reg16_reg16		esi, eax
cmp_si_cx
	cmp_reg16_reg16		esi, ecx
cmp_si_dx
	cmp_reg16_reg16		esi, edx
cmp_si_bx
	cmp_reg16_reg16		esi, ebx
cmp_si_sp
	cmp_reg16_reg16		esi, esp
cmp_si_bp
	cmp_reg16_reg16		esi, ebp
cmp_si_si
	cmp_reg16_reg16		esi, esi
cmp_si_di
	cmp_reg16_reg16		esi, edi
cmp_di_ax
	cmp_reg16_reg16		edi, eax
cmp_di_cx
	cmp_reg16_reg16		edi, ecx
cmp_di_dx
	cmp_reg16_reg16		edi, edx
cmp_di_bx
	cmp_reg16_reg16		edi, ebx
cmp_di_sp
	cmp_reg16_reg16		edi, esp
cmp_di_bp
	cmp_reg16_reg16		edi, ebp
cmp_di_si
	cmp_reg16_reg16		edi, esi
cmp_di_di
	cmp_reg16_reg16		edi, edi

	GLOBAL  cmp_ax_ax
	GLOBAL  cmp_ax_cx
	GLOBAL  cmp_ax_dx
	GLOBAL  cmp_ax_bx
	GLOBAL  cmp_ax_sp
	GLOBAL  cmp_ax_bp
	GLOBAL  cmp_ax_si
	GLOBAL  cmp_ax_di
	GLOBAL  cmp_cx_ax
	GLOBAL  cmp_cx_cx
	GLOBAL  cmp_cx_dx
	GLOBAL  cmp_cx_bx
	GLOBAL  cmp_cx_sp
	GLOBAL  cmp_cx_bp
	GLOBAL  cmp_cx_si
	GLOBAL  cmp_cx_di
	GLOBAL  cmp_dx_ax
	GLOBAL  cmp_dx_cx
	GLOBAL  cmp_dx_dx
	GLOBAL  cmp_dx_bx
	GLOBAL  cmp_dx_sp
	GLOBAL  cmp_dx_bp
	GLOBAL  cmp_dx_si
	GLOBAL  cmp_dx_di
	GLOBAL  cmp_bx_ax
	GLOBAL  cmp_bx_cx
	GLOBAL  cmp_bx_dx
	GLOBAL  cmp_bx_bx
	GLOBAL  cmp_bx_sp
	GLOBAL  cmp_bx_bp
	GLOBAL  cmp_bx_si
	GLOBAL  cmp_bx_di
	GLOBAL  cmp_sp_ax
	GLOBAL  cmp_sp_cx
	GLOBAL  cmp_sp_dx
	GLOBAL  cmp_sp_bx
	GLOBAL  cmp_sp_sp
	GLOBAL  cmp_sp_bp
	GLOBAL  cmp_sp_si
	GLOBAL  cmp_sp_di
	GLOBAL  cmp_bp_ax
	GLOBAL  cmp_bp_cx
	GLOBAL  cmp_bp_dx
	GLOBAL  cmp_bp_bx
	GLOBAL  cmp_bp_sp
	GLOBAL  cmp_bp_bp
	GLOBAL  cmp_bp_si
	GLOBAL  cmp_bp_di
	GLOBAL  cmp_si_ax
	GLOBAL  cmp_si_cx
	GLOBAL  cmp_si_dx
	GLOBAL  cmp_si_bx
	GLOBAL  cmp_si_sp
	GLOBAL  cmp_si_bp
	GLOBAL  cmp_si_si
	GLOBAL  cmp_si_di
	GLOBAL  cmp_di_ax
	GLOBAL  cmp_di_cx
	GLOBAL  cmp_di_dx
	GLOBAL  cmp_di_bx
	GLOBAL  cmp_di_sp
	GLOBAL  cmp_di_bp
	GLOBAL  cmp_di_si
	GLOBAL  cmp_di_di


; ------------------- 3C = CMP AL,imm8 --------------------------------
op_3c
	ldrb	r0,[r12],#1				; Load the immediate byte to r0, increment r12 by 1
	mov		r1, eax, lsl #24		; Shift the AL value to the topmost byte of r1
	cmp		r1, r0, lsl #24			; Shift the immediate byte also to the topmost byte, and then compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

; ------------------- 3D = CMP AX,imm16 -------------------------------
op_3d
	ldrb	r0,[r12],#1				; Load the immediate byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load the immediate byte to r0, increment r12 by 1
	mov		r2, eax, lsl #16
	orr		r0, r1, lsl #8
	cmp		r2, r0, lsl #16			; Shift the immediate value also to the topmost halfword, and then compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

; ------------------- 3F = AAS ----------------------------------------
;
; if((AL & 0xF) > 9 || AF == 1) {
;	AL = AL - 6;
;	AH = AH - 1;
;	AF = 1;
;	CF = 1;
; }
; else {
;	CF = 0;
;	AF = 0;
; }
; AL = AL & 0xF;
;
op_3f
	ldr		r2, [sp, #SP_FLAGS]					; r2 = current x86 flags (for FLAG_AF)
	mov		r0, #0								; Clear all ARM flags
	;------
	; if ((reg_al & 0xf) > 9)
	;------
	and		r1, eax, #0x0F
	cmp		r1, #9
	bgt		%f1									;	{
	;------
	; else if (get_AF())
	;------
2	tst		r2, #FLAG_AF
	beq		%f4									;	{
	and		r1, eax, #0xFF						;		SETFLAGBIT(OF,((reg_al>=0x80) && (reg_al<=0x85)));
	cmp		r1, #0x85
	bgt		%f1
	cmp		r1, #0x80
	blt		%f1
	orr		r0, #ARM_OVER
1	ror		eax, #16
	sub		eax, #0x01000000
	sub		eax, #0x00060000					;		reg_ax -= 0x106;
	ror		eax, #16
	orr		r0, #ARM_CARRY						;		SETFLAGBIT(CF,true);
	orr		r2, #FLAG_AF						;		SETFLAGBIT(AF,true); }
	;------
	; SETFLAGBIT(ZF,(reg_al == 0));
	;------
4	tst		eax, #0xFF
	orreq	r0, #ARM_ZERO
	;------
	; SETFLAGBIT(SF,(reg_al&0x80));
	;------
	tst		eax, #0x80
	orrne	r0, #ARM_NEG
	;------
	; SETFLAGBIT(PF,parity_lookup[reg_al]);
	;------
	;------
	; reg_al &= 0x0F;
	;------
	bic		eax, #0xF0
	str		r2, [sp, #SP_FLAGS]					; Save FLAG_AF
	b		restore_flags_from_r0
	
; =================== 40..47 = INC reg16 ==============================
; Opcodes 40..47 for AX, CX, DX, BX, SP, BP, SI, DI
; INC reg16 is like ADD reg16,1, except the Carry flag is not changed.
;
	MACRO 
	inc_reg16 $reg
	mrs		r0,cpsr					; r0 = Current flags
	mov		r1, $reg, lsl #16
	eor		$reg, r1, lsr #16
	adds	r1, #0x00010000			; Add 1 to the high halfword
	orr		$reg, r1, lsr #16
	mrs		r1,cpsr					; r1 = Flags after increment
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags + original Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

op_40								; inc AX
	inc_reg16 eax
op_41								; inc CX
	inc_reg16 ecx
op_42								; inc DX
	inc_reg16 edx
op_43								; inc BX
	inc_reg16 ebx
op_44								; inc SP Note! This will break stack alignment unless it is followed by another INC SP!
	inc_reg16 esp
op_45								; inc BP
	inc_reg16 ebp
op_46								; inc SI
	inc_reg16 esi
op_47								; inc DI
	inc_reg16 edi

; =================== 48..4F = DEC reg16 ==============================
; Opcodes 48..4F for AX, CX, DX, BX, SP, BP, SI, DI
; DEC reg16 is like SUB reg16,1, except the Carry flag is not changed.
;
	MACRO 
	dec_reg16 $reg
	mrs		r0,cpsr					; r0 = Current flags
	mov		r1, $reg, lsl #16
	eor		$reg, r1, lsr #16
	subs	r1, #0x00010000			; Add 1 to the high halfword
	orr		$reg, r1, lsr #16
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

op_48								; DEC AX
	dec_reg16 eax
op_49								; DEC CX
	dec_reg16 ecx
op_4a								; DEC DX
	dec_reg16 edx
op_4b								; DEC BX
	dec_reg16 ebx
	GLOBAL	op_4c
op_4c								; DEC SP Note! This will break stack alignment unless it is followed by another DEC SP!
	dec_reg16 esp
op_4d								; DEC BP
	dec_reg16 ebp
op_4e								; DEC SI
	dec_reg16 esi
op_4f								; DEC DI
	dec_reg16 edi

; =================== 50..57 = PUSH reg16 ==============================
; Opcodes 50..57 for AX, CX, DX, BX, SP, BP, SI, DI

	MACRO 
	push_reg16 $reg
	mov		r1, $reg, lsr #8
	push_low_hi $reg, r1, r2, r3
	b		loop
	MEND

op_50
	push_reg16 eax
op_51
	push_reg16 ecx
op_52
	push_reg16 edx
op_53
	push_reg16 ebx
op_54
	;-------
	; Windows 3.0 uses PUSH SP to determine if it is running on a 80186 processor.
	; If the value pushed is not the original value of the stack pointer, this is a 80186 processor.
	;-------
	mov		r0, esp
	mov		r1, r0, lsr #8
	push_low_hi r0, r1, r2, r3
	b		loop
op_55
	push_reg16 ebp
op_56
	push_reg16 esi
op_57
	push_reg16 edi

; =================== 58..5F = POP reg16 ==============================
; Opcodes 58..5F for AX, CX, DX, BX, SP, BP, SI, DI

	MACRO 
	pop_reg16 $reg
	pop_reg_low_tmp r0, r1
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

op_58
	pop_reg16 eax
op_59
	pop_reg16 ecx
op_5a
	pop_reg16 edx
op_5b
	pop_reg16 ebx
op_5c
	pop_reg16 esp
op_5d
	pop_reg16 ebp
op_5e
	pop_reg16 esi
op_5f
	pop_reg16 edi

; ------------------- 60 = PUSHA ---------------------------------------
; Push AX, CX, DX, BX, orig SP, BP, SI, DI
;
; Note! This is a 80186+ opcode!
;
op_60
	mov		r1, esp
	push_hword eax, r0, r2
	push_hword ecx, r0, r2
	push_hword edx, r0, r2
	push_hword ebx, r0, r2
	push_hword r1, r0, r2						; SP
	push_hword ebp, r0, r2
	push_hword esi, r0, r2
	push_hword edi, r0, r2
	b		loop

; ------------------- 61 = POPA ---------------------------------------
; Pop DI, SI, BP, (nothing), BX, DX, CX, AX
;
; Note! This is a 80186+ opcode!
;
op_61
	pop_reg_low_tmp r0, r1
	bfi		edi, r0, #0, #16
	pop_reg_low_tmp r0, r1
	bfi		esi, r0, #0, #16
	pop_reg_low_tmp r0, r1
	bfi		ebp, r0, #0, #16
	add		esp, #2					; (nothing)
	pop_reg_low_tmp r0, r1
	bfi		ebx, r0, #0, #16
	pop_reg_low_tmp r0, r1
	bfi		edx, r0, #0, #16
	pop_reg_low_tmp r0, r1
	bfi		ecx, r0, #0, #16
	pop_reg_low_tmp r0, r1
	bfi		eax, r0, #0, #16
	b		loop

; ------------------- 68 = PUSH imm16 ---------------------------------
;
; Note! This is a 80186+ opcode!
;
op_68
	ldrb	r0,[r12],#1				; Load the immediate byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load the immediate byte to r0, increment r12 by 1
	push_low_hi r0, r1, r2, r3
	b		loop

	LTORG

	ALIGN	4

; ------------------- 66 = 32-bit override ----------------------------

	GLOBAL bad_386_opcode_back1
bad_386_opcode_back1
	sub		r12, #1
	b		unknown

; ------------------- 69 = IMUL r16,r/m16,imm16 ------------------------
;
; Note! This is a 80186+ opcode!
; TODO! Handle overflow and carry flags! Both should be cleared when the result fits into target.
;
;
; 69048A00 		imul ax,[si],008A
; 69D20005		imul dx,dx,0500
;
op_69
	modrm_jump_16_tbl op_69_jump
	modrm_tbl_3_old imul_imm16
	DCD imul_ax_ax_imm16, imul_ax_cx_imm16, imul_ax_dx_imm16, imul_ax_bx_imm16, imul_ax_sp_imm16, imul_ax_bp_imm16, imul_ax_si_imm16, imul_ax_di_imm16
	DCD imul_cx_ax_imm16, imul_cx_cx_imm16, imul_cx_dx_imm16, imul_cx_bx_imm16, imul_cx_sp_imm16, imul_cx_bp_imm16, imul_cx_si_imm16, imul_cx_di_imm16
	DCD imul_dx_ax_imm16, imul_dx_cx_imm16, imul_dx_dx_imm16, imul_dx_bx_imm16, imul_dx_sp_imm16, imul_dx_bp_imm16, imul_dx_si_imm16, imul_dx_di_imm16
	DCD imul_bx_ax_imm16, imul_bx_cx_imm16, imul_bx_dx_imm16, imul_bx_bx_imm16, imul_bx_sp_imm16, imul_bx_bp_imm16, imul_bx_si_imm16, imul_bx_di_imm16
	DCD imul_sp_ax_imm16, imul_sp_cx_imm16, imul_sp_dx_imm16, imul_sp_bx_imm16, imul_sp_sp_imm16, imul_sp_bp_imm16, imul_sp_si_imm16, imul_sp_di_imm16
	DCD imul_bp_ax_imm16, imul_bp_cx_imm16, imul_bp_dx_imm16, imul_bp_bx_imm16, imul_bp_sp_imm16, imul_bp_bp_imm16, imul_bp_si_imm16, imul_bp_di_imm16
	DCD imul_si_ax_imm16, imul_si_cx_imm16, imul_si_dx_imm16, imul_si_bx_imm16, imul_si_sp_imm16, imul_si_bp_imm16, imul_si_si_imm16, imul_si_di_imm16
	DCD imul_di_ax_imm16, imul_di_cx_imm16, imul_di_dx_imm16, imul_di_bx_imm16, imul_di_sp_imm16, imul_di_bp_imm16, imul_di_si_imm16, imul_di_di_imm16

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	imul_imm16_reg_r0 $reg
	GLOBAL	imul_imm16_reg16_r0_bp_$reg
imul_imm16_reg16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	imul_imm16_reg16_r0_$reg
imul_imm16_reg16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_69_RAM_$reg, bad_EGA_opcode_2, bad_MODEX_opcode_2
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_69_RAM_$reg
	ldrb	r0,[r2] 					; Load byte to r0
	ldrsb	r1,[r2, #1]					; Get signed high byte
	ldrb	r2,[r12], #1
	ldrsb	r3,[r12], #1
	orr		r0, r1, lsl #8				; Now r0 = signed [disp16] value
	orr		r1, r2, r3, lsl #8			; Now r1 = signed imm16 value
	mul		r2, r0, r1
	bfi		$reg, r2, #0, #16
	b		loop	
	MEND

	imul_imm16_reg_r0 eax
	imul_imm16_reg_r0 ecx
	imul_imm16_reg_r0 edx
	imul_imm16_reg_r0 ebx
	imul_imm16_reg_r0 esp
	imul_imm16_reg_r0 ebp
	imul_imm16_reg_r0 esi
	imul_imm16_reg_r0 edi

	modrm_3_genall_old imul_imm16, imul_imm16_reg16_r0


; --- [registers] ---

	MACRO 
	imul_reg1_reg2_imm16 $reg1, $reg2
	ldrb	r0,[r12], #1
	ldrsb	r1,[r12], #1
	sbfx	r2, $reg2, #0, #16			; Now r2 = signed reg2 value
	orr		r0, r1, lsl #8				; Now r0 = signed imm16 value	
	mul		r1, r0, r2
	bfi		$reg1, r1, #0, #16
	b		loop	
	MEND

imul_ax_ax_imm16
	imul_reg1_reg2_imm16 eax, eax
imul_cx_ax_imm16
	imul_reg1_reg2_imm16 ecx, eax
imul_dx_ax_imm16
	imul_reg1_reg2_imm16 edx, eax
imul_bx_ax_imm16
	imul_reg1_reg2_imm16 ebx, eax
imul_sp_ax_imm16
	imul_reg1_reg2_imm16 esp, eax
imul_bp_ax_imm16
	imul_reg1_reg2_imm16 ebp, eax
imul_si_ax_imm16
	imul_reg1_reg2_imm16 esi, eax
imul_di_ax_imm16
	imul_reg1_reg2_imm16 edi, eax

imul_ax_cx_imm16
	imul_reg1_reg2_imm16 eax, ecx
imul_cx_cx_imm16
	imul_reg1_reg2_imm16 ecx, ecx
imul_dx_cx_imm16
	imul_reg1_reg2_imm16 edx, ecx
imul_bx_cx_imm16
	imul_reg1_reg2_imm16 ebx, ecx
imul_sp_cx_imm16
	imul_reg1_reg2_imm16 esp, ecx
imul_bp_cx_imm16
	imul_reg1_reg2_imm16 ebp, ecx
imul_si_cx_imm16
	imul_reg1_reg2_imm16 esi, ecx
imul_di_cx_imm16
	imul_reg1_reg2_imm16 edi, ecx

imul_ax_dx_imm16
	imul_reg1_reg2_imm16 eax, edx
imul_cx_dx_imm16
	imul_reg1_reg2_imm16 ecx, edx
imul_dx_dx_imm16
	imul_reg1_reg2_imm16 edx, edx
imul_bx_dx_imm16
	imul_reg1_reg2_imm16 ebx, edx
imul_sp_dx_imm16
	imul_reg1_reg2_imm16 esp, edx
imul_bp_dx_imm16
	imul_reg1_reg2_imm16 ebp, edx
imul_si_dx_imm16
	imul_reg1_reg2_imm16 esi, edx
imul_di_dx_imm16
	imul_reg1_reg2_imm16 edi, edx

imul_ax_bx_imm16
	imul_reg1_reg2_imm16 eax, ebx
imul_cx_bx_imm16
	imul_reg1_reg2_imm16 ecx, ebx
imul_dx_bx_imm16
	imul_reg1_reg2_imm16 edx, ebx
imul_bx_bx_imm16
	imul_reg1_reg2_imm16 ebx, ebx
imul_sp_bx_imm16
	imul_reg1_reg2_imm16 esp, ebx
imul_bp_bx_imm16
	imul_reg1_reg2_imm16 ebp, ebx
imul_si_bx_imm16
	imul_reg1_reg2_imm16 esi, ebx
imul_di_bx_imm16
	imul_reg1_reg2_imm16 edi, ebx

imul_ax_sp_imm16
	imul_reg1_reg2_imm16 eax, esp
imul_cx_sp_imm16
	imul_reg1_reg2_imm16 ecx, esp
imul_dx_sp_imm16
	imul_reg1_reg2_imm16 edx, esp
imul_bx_sp_imm16
	imul_reg1_reg2_imm16 ebx, esp
imul_sp_sp_imm16
	imul_reg1_reg2_imm16 esp, esp
imul_bp_sp_imm16
	imul_reg1_reg2_imm16 ebp, esp
imul_si_sp_imm16
	imul_reg1_reg2_imm16 esi, esp
imul_di_sp_imm16
	imul_reg1_reg2_imm16 edi, esp

imul_ax_bp_imm16
	imul_reg1_reg2_imm16 eax, ebp
imul_cx_bp_imm16
	imul_reg1_reg2_imm16 ecx, ebp
imul_dx_bp_imm16
	imul_reg1_reg2_imm16 edx, ebp
imul_bx_bp_imm16
	imul_reg1_reg2_imm16 ebx, ebp
imul_sp_bp_imm16
	imul_reg1_reg2_imm16 esp, ebp
imul_bp_bp_imm16
	imul_reg1_reg2_imm16 ebp, ebp
imul_si_bp_imm16
	imul_reg1_reg2_imm16 esi, ebp
imul_di_bp_imm16
	imul_reg1_reg2_imm16 edi, ebp

imul_ax_si_imm16
	imul_reg1_reg2_imm16 eax, esi
imul_cx_si_imm16
	imul_reg1_reg2_imm16 ecx, esi
imul_dx_si_imm16
	imul_reg1_reg2_imm16 edx, esi
imul_bx_si_imm16
	imul_reg1_reg2_imm16 ebx, esi
imul_sp_si_imm16
	imul_reg1_reg2_imm16 esp, esi
imul_bp_si_imm16
	imul_reg1_reg2_imm16 ebp, esi
imul_si_si_imm16
	imul_reg1_reg2_imm16 esi, esi
imul_di_si_imm16
	imul_reg1_reg2_imm16 edi, esi

imul_ax_di_imm16
	imul_reg1_reg2_imm16 eax, edi
imul_cx_di_imm16
	imul_reg1_reg2_imm16 ecx, edi
imul_dx_di_imm16
	imul_reg1_reg2_imm16 edx, edi
imul_bx_di_imm16
	imul_reg1_reg2_imm16 ebx, edi
imul_sp_di_imm16
	imul_reg1_reg2_imm16 esp, edi
imul_bp_di_imm16
	imul_reg1_reg2_imm16 ebp, edi
imul_si_di_imm16
	imul_reg1_reg2_imm16 esi, edi
imul_di_di_imm16
	imul_reg1_reg2_imm16 edi, edi

	GLOBAL	imul_ax_ax_imm16 
	GLOBAL  imul_ax_cx_imm16 
	GLOBAL  imul_ax_dx_imm16 
	GLOBAL  imul_ax_bx_imm16 
	GLOBAL  imul_ax_sp_imm16 
	GLOBAL  imul_ax_bp_imm16 
	GLOBAL  imul_ax_si_imm16 
	GLOBAL  imul_ax_di_imm16
	GLOBAL 	imul_cx_ax_imm16 
	GLOBAL  imul_cx_cx_imm16 
	GLOBAL  imul_cx_dx_imm16 
	GLOBAL  imul_cx_bx_imm16 
	GLOBAL  imul_cx_sp_imm16 
	GLOBAL  imul_cx_bp_imm16 
	GLOBAL  imul_cx_si_imm16 
	GLOBAL  imul_cx_di_imm16
	GLOBAL 	imul_dx_ax_imm16 
	GLOBAL  imul_dx_cx_imm16 
	GLOBAL  imul_dx_dx_imm16 
	GLOBAL  imul_dx_bx_imm16 
	GLOBAL  imul_dx_sp_imm16 
	GLOBAL  imul_dx_bp_imm16 
	GLOBAL  imul_dx_si_imm16 
	GLOBAL  imul_dx_di_imm16
	GLOBAL 	imul_bx_ax_imm16 
	GLOBAL  imul_bx_cx_imm16 
	GLOBAL  imul_bx_dx_imm16 
	GLOBAL  imul_bx_bx_imm16 
	GLOBAL  imul_bx_sp_imm16 
	GLOBAL  imul_bx_bp_imm16 
	GLOBAL  imul_bx_si_imm16 
	GLOBAL  imul_bx_di_imm16
	GLOBAL 	imul_sp_ax_imm16 
	GLOBAL  imul_sp_cx_imm16 
	GLOBAL  imul_sp_dx_imm16 
	GLOBAL  imul_sp_bx_imm16 
	GLOBAL  imul_sp_sp_imm16 
	GLOBAL  imul_sp_bp_imm16 
	GLOBAL  imul_sp_si_imm16 
	GLOBAL  imul_sp_di_imm16
	GLOBAL 	imul_bp_ax_imm16 
	GLOBAL  imul_bp_cx_imm16 
	GLOBAL  imul_bp_dx_imm16 
	GLOBAL  imul_bp_bx_imm16 
	GLOBAL  imul_bp_sp_imm16 
	GLOBAL  imul_bp_bp_imm16 
	GLOBAL  imul_bp_si_imm16 
	GLOBAL  imul_bp_di_imm16
	GLOBAL 	imul_si_ax_imm16 
	GLOBAL  imul_si_cx_imm16 
	GLOBAL  imul_si_dx_imm16 
	GLOBAL  imul_si_bx_imm16 
	GLOBAL  imul_si_sp_imm16 
	GLOBAL  imul_si_bp_imm16 
	GLOBAL  imul_si_si_imm16 
	GLOBAL  imul_si_di_imm16
	GLOBAL 	imul_di_ax_imm16 
	GLOBAL  imul_di_cx_imm16 
	GLOBAL  imul_di_dx_imm16 
	GLOBAL  imul_di_bx_imm16 
	GLOBAL  imul_di_sp_imm16 
	GLOBAL  imul_di_bp_imm16 
	GLOBAL  imul_di_si_imm16 
	GLOBAL  imul_di_di_imm16

; ------------------- 6A = PUSH imm8 ----------------------------------
;
; Note! This is a 80186+ opcode!
;
op_6a
	ldrsb	r0,[r12],#1							; Load the sign-extended imm8 value to r0, increment r12 by 1
	mov		r1, r0, lsr #8
	push_low_hi r0, r1, r2, r3
	b		loop

; ------------------- 6B = IMUL r16,r/m16,imm8 ------------------------
; IMUL SI,BX,1D = 6BF31D => mod = 11, reg = 110 (SI), rm = 011 (BX)
; 6BBDE20015    imul di,[di+00E2],15
;
; Note! This is a 80186+ opcode!
; TODO! Handle overflow and carry flags!
;
;
op_6b
	modrm_jump_16_tbl op_6b_jump
	modrm_tbl_3_old imul_imm8
	DCD imul_ax_ax_imm8, imul_ax_cx_imm8, imul_ax_dx_imm8, imul_ax_bx_imm8, imul_ax_sp_imm8, imul_ax_bp_imm8, imul_ax_si_imm8, imul_ax_di_imm8
	DCD imul_cx_ax_imm8, imul_cx_cx_imm8, imul_cx_dx_imm8, imul_cx_bx_imm8, imul_cx_sp_imm8, imul_cx_bp_imm8, imul_cx_si_imm8, imul_cx_di_imm8
	DCD imul_dx_ax_imm8, imul_dx_cx_imm8, imul_dx_dx_imm8, imul_dx_bx_imm8, imul_dx_sp_imm8, imul_dx_bp_imm8, imul_dx_si_imm8, imul_dx_di_imm8
	DCD imul_bx_ax_imm8, imul_bx_cx_imm8, imul_bx_dx_imm8, imul_bx_bx_imm8, imul_bx_sp_imm8, imul_bx_bp_imm8, imul_bx_si_imm8, imul_bx_di_imm8
	DCD imul_sp_ax_imm8, imul_sp_cx_imm8, imul_sp_dx_imm8, imul_sp_bx_imm8, imul_sp_sp_imm8, imul_sp_bp_imm8, imul_sp_si_imm8, imul_sp_di_imm8
	DCD imul_bp_ax_imm8, imul_bp_cx_imm8, imul_bp_dx_imm8, imul_bp_bx_imm8, imul_bp_sp_imm8, imul_bp_bp_imm8, imul_bp_si_imm8, imul_bp_di_imm8
	DCD imul_si_ax_imm8, imul_si_cx_imm8, imul_si_dx_imm8, imul_si_bx_imm8, imul_si_sp_imm8, imul_si_bp_imm8, imul_si_si_imm8, imul_si_di_imm8
	DCD imul_di_ax_imm8, imul_di_cx_imm8, imul_di_dx_imm8, imul_di_bx_imm8, imul_di_sp_imm8, imul_di_bp_imm8, imul_di_si_imm8, imul_di_di_imm8

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	imul_imm8_reg_r0 $reg
	GLOBAL	imul_imm8_reg16_r0_bp_$reg
imul_imm8_reg16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	imul_imm8_reg16_r0_$reg
imul_imm8_reg16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_6b_RAM_$reg, bad_EGA_opcode_1, bad_MODEX_opcode_1
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_6b_RAM_$reg
	ldrb	r0,[r2] 					; Load byte to r0
	ldrsb	r1,[r2, #1]					; Get signed high byte
	ldrsb	r2,[r12], #1				; r2 = signed imm8 value
	orr		r0, r1, lsl #8				; Now r0 = signed [disp16] value
	mul		r1, r0, r2
	bfi		$reg, r1, #0, #16
	b		loop	
	MEND

	imul_imm8_reg_r0 eax
	imul_imm8_reg_r0 ecx
	imul_imm8_reg_r0 edx
	imul_imm8_reg_r0 ebx
	imul_imm8_reg_r0 esp
	imul_imm8_reg_r0 ebp
	imul_imm8_reg_r0 esi
	imul_imm8_reg_r0 edi

	modrm_3_genall_old imul_imm8, imul_imm8_reg16_r0
	

; --- registers ---

	MACRO 
	imul_reg1_reg2_imm8 $reg1, $reg2
	ldrsb	r0,[r12],#1				; Load the sign-extended imm8 value to r0, increment r12 by 1
	sbfx	r1, $reg2, #0, #16
	mul		r2, r1, r0
	bfi		$reg1, r2, #0, #16
	b		loop	
	MEND

imul_ax_ax_imm8
	imul_reg1_reg2_imm8 eax, eax
imul_cx_ax_imm8
	imul_reg1_reg2_imm8 ecx, eax
imul_dx_ax_imm8
	imul_reg1_reg2_imm8 edx, eax
imul_bx_ax_imm8
	imul_reg1_reg2_imm8 ebx, eax
imul_sp_ax_imm8
	imul_reg1_reg2_imm8 esp, eax
imul_bp_ax_imm8
	imul_reg1_reg2_imm8 ebp, eax
imul_si_ax_imm8
	imul_reg1_reg2_imm8 esi, eax
imul_di_ax_imm8
	imul_reg1_reg2_imm8 edi, eax

imul_ax_cx_imm8
	imul_reg1_reg2_imm8 eax, ecx
imul_cx_cx_imm8
	imul_reg1_reg2_imm8 ecx, ecx
imul_dx_cx_imm8
	imul_reg1_reg2_imm8 edx, ecx
imul_bx_cx_imm8
	imul_reg1_reg2_imm8 ebx, ecx
imul_sp_cx_imm8
	imul_reg1_reg2_imm8 esp, ecx
imul_bp_cx_imm8
	imul_reg1_reg2_imm8 ebp, ecx
imul_si_cx_imm8
	imul_reg1_reg2_imm8 esi, ecx
imul_di_cx_imm8
	imul_reg1_reg2_imm8 edi, ecx

imul_ax_dx_imm8
	imul_reg1_reg2_imm8 eax, edx
imul_cx_dx_imm8
	imul_reg1_reg2_imm8 ecx, edx
imul_dx_dx_imm8
	imul_reg1_reg2_imm8 edx, edx
imul_bx_dx_imm8
	imul_reg1_reg2_imm8 ebx, edx
imul_sp_dx_imm8
	imul_reg1_reg2_imm8 esp, edx
imul_bp_dx_imm8
	imul_reg1_reg2_imm8 ebp, edx
imul_si_dx_imm8
	imul_reg1_reg2_imm8 esi, edx
imul_di_dx_imm8
	imul_reg1_reg2_imm8 edi, edx

imul_ax_bx_imm8
	imul_reg1_reg2_imm8 eax, ebx
imul_cx_bx_imm8
	imul_reg1_reg2_imm8 ecx, ebx
imul_dx_bx_imm8
	imul_reg1_reg2_imm8 edx, ebx
imul_bx_bx_imm8
	imul_reg1_reg2_imm8 ebx, ebx
imul_sp_bx_imm8
	imul_reg1_reg2_imm8 esp, ebx
imul_bp_bx_imm8
	imul_reg1_reg2_imm8 ebp, ebx
imul_si_bx_imm8
	imul_reg1_reg2_imm8 esi, ebx
imul_di_bx_imm8
	imul_reg1_reg2_imm8 edi, ebx

imul_ax_sp_imm8
	imul_reg1_reg2_imm8 eax, esp
imul_cx_sp_imm8
	imul_reg1_reg2_imm8 ecx, esp
imul_dx_sp_imm8
	imul_reg1_reg2_imm8 edx, esp
imul_bx_sp_imm8
	imul_reg1_reg2_imm8 ebx, esp
imul_sp_sp_imm8
	imul_reg1_reg2_imm8 esp, esp
imul_bp_sp_imm8
	imul_reg1_reg2_imm8 ebp, esp
imul_si_sp_imm8
	imul_reg1_reg2_imm8 esi, esp
imul_di_sp_imm8
	imul_reg1_reg2_imm8 edi, esp

imul_ax_bp_imm8
	imul_reg1_reg2_imm8 eax, ebp
imul_cx_bp_imm8
	imul_reg1_reg2_imm8 ecx, ebp
imul_dx_bp_imm8
	imul_reg1_reg2_imm8 edx, ebp
imul_bx_bp_imm8
	imul_reg1_reg2_imm8 ebx, ebp
imul_sp_bp_imm8
	imul_reg1_reg2_imm8 esp, ebp
imul_bp_bp_imm8
	imul_reg1_reg2_imm8 ebp, ebp
imul_si_bp_imm8
	imul_reg1_reg2_imm8 esi, ebp
imul_di_bp_imm8
	imul_reg1_reg2_imm8 edi, ebp

imul_ax_si_imm8
	imul_reg1_reg2_imm8 eax, esi
imul_cx_si_imm8
	imul_reg1_reg2_imm8 ecx, esi
imul_dx_si_imm8
	imul_reg1_reg2_imm8 edx, esi
imul_bx_si_imm8
	imul_reg1_reg2_imm8 ebx, esi
imul_sp_si_imm8
	imul_reg1_reg2_imm8 esp, esi
imul_bp_si_imm8
	imul_reg1_reg2_imm8 ebp, esi
imul_si_si_imm8
	imul_reg1_reg2_imm8 esi, esi
imul_di_si_imm8
	imul_reg1_reg2_imm8 edi, esi

imul_ax_di_imm8
	imul_reg1_reg2_imm8 eax, edi
imul_cx_di_imm8
	imul_reg1_reg2_imm8 ecx, edi
imul_dx_di_imm8
	imul_reg1_reg2_imm8 edx, edi
imul_bx_di_imm8
	imul_reg1_reg2_imm8 ebx, edi
imul_sp_di_imm8
	imul_reg1_reg2_imm8 esp, edi
imul_bp_di_imm8
	imul_reg1_reg2_imm8 ebp, edi
imul_si_di_imm8
	imul_reg1_reg2_imm8 esi, edi
imul_di_di_imm8
	imul_reg1_reg2_imm8 edi, edi

	GLOBAL	imul_ax_ax_imm8 
	GLOBAL  imul_ax_cx_imm8 
	GLOBAL  imul_ax_dx_imm8 
	GLOBAL  imul_ax_bx_imm8 
	GLOBAL  imul_ax_sp_imm8 
	GLOBAL  imul_ax_bp_imm8 
	GLOBAL  imul_ax_si_imm8 
	GLOBAL  imul_ax_di_imm8
	GLOBAL 	imul_cx_ax_imm8 
	GLOBAL  imul_cx_cx_imm8 
	GLOBAL  imul_cx_dx_imm8 
	GLOBAL  imul_cx_bx_imm8 
	GLOBAL  imul_cx_sp_imm8 
	GLOBAL  imul_cx_bp_imm8 
	GLOBAL  imul_cx_si_imm8 
	GLOBAL  imul_cx_di_imm8
	GLOBAL 	imul_dx_ax_imm8 
	GLOBAL  imul_dx_cx_imm8 
	GLOBAL  imul_dx_dx_imm8 
	GLOBAL  imul_dx_bx_imm8 
	GLOBAL  imul_dx_sp_imm8 
	GLOBAL  imul_dx_bp_imm8 
	GLOBAL  imul_dx_si_imm8 
	GLOBAL  imul_dx_di_imm8
	GLOBAL 	imul_bx_ax_imm8 
	GLOBAL  imul_bx_cx_imm8 
	GLOBAL  imul_bx_dx_imm8 
	GLOBAL  imul_bx_bx_imm8 
	GLOBAL  imul_bx_sp_imm8 
	GLOBAL  imul_bx_bp_imm8 
	GLOBAL  imul_bx_si_imm8 
	GLOBAL  imul_bx_di_imm8
	GLOBAL 	imul_sp_ax_imm8 
	GLOBAL  imul_sp_cx_imm8 
	GLOBAL  imul_sp_dx_imm8 
	GLOBAL  imul_sp_bx_imm8 
	GLOBAL  imul_sp_sp_imm8 
	GLOBAL  imul_sp_bp_imm8 
	GLOBAL  imul_sp_si_imm8 
	GLOBAL  imul_sp_di_imm8
	GLOBAL 	imul_bp_ax_imm8 
	GLOBAL  imul_bp_cx_imm8 
	GLOBAL  imul_bp_dx_imm8 
	GLOBAL  imul_bp_bx_imm8 
	GLOBAL  imul_bp_sp_imm8 
	GLOBAL  imul_bp_bp_imm8 
	GLOBAL  imul_bp_si_imm8 
	GLOBAL  imul_bp_di_imm8
	GLOBAL 	imul_si_ax_imm8 
	GLOBAL  imul_si_cx_imm8 
	GLOBAL  imul_si_dx_imm8 
	GLOBAL  imul_si_bx_imm8 
	GLOBAL  imul_si_sp_imm8 
	GLOBAL  imul_si_bp_imm8 
	GLOBAL  imul_si_si_imm8 
	GLOBAL  imul_si_di_imm8
	GLOBAL 	imul_di_ax_imm8 
	GLOBAL  imul_di_cx_imm8 
	GLOBAL  imul_di_dx_imm8 
	GLOBAL  imul_di_bx_imm8 
	GLOBAL  imul_di_sp_imm8 
	GLOBAL  imul_di_bp_imm8 
	GLOBAL  imul_di_si_imm8 
	GLOBAL  imul_di_di_imm8


; =================== 70..7F = Conditional Jumps ======================
;
	MACRO 
	cond_jump $cnd
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	add$cnd	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	b		loop
	MEND

op_70								; JO cb
	cond_jump vs
op_71								; JNO cb
	cond_jump vc
op_72								; JC/JB/JNAE cb
	cond_jump cs
op_73								; JNC/JNB/JAE cb
	cond_jump cc
op_74								; JZ/JE cb (It is more likely that the jump is NOT taken)
	cond_jump eq
op_75								; JNZ/JNE cb (It is more likely that the jump IS taken)
	cond_jump ne
op_76								; JBE/JNA cb (C=1 or Z=1, x86 Carry has opposite sense to ARM Carry)
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	addcs	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	bcs		loop
	addeq	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	b		loop
op_77								; JA/JNBE cb (C=0 and Z=0, x86 Carry has opposite sense to ARM Carry)
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	bcs		loop
	addne	r12, r12, r0			; Adjust program counter by the jump amount, if the condition is true
	b		loop
op_78								; JS cb
	cond_jump mi
op_79								; JNS cb
	cond_jump pl
op_7c								; JL/JNGE cb
	cond_jump lt
op_7d								; JNL/JGE cb
	cond_jump ge
op_7e								; JLE/JNG cb
	cond_jump le
op_7f								; JG/JNLE cb
	cond_jump gt

	ALIGN	4

	;-------
	; JP/JPE cb
	; Special hack for "The Incredible Machine" and "Turbo Science"
	;	2004:6F5D 0BFF            or   di,di
	;	2004:6F5F 7A02            jpe  6F63 ($+2)
	; Special hack for "SOR_LOR"
	;	020B:3CB4 8B5E08          mov  bx,[bp+08]
	;	020B:3CB7 83FB08          cmp  bx,0008
	;	020B:3CBA 7506            jne  3CC2 ($+6)
	;	020B:3CBC 8B1EBA17        mov  bx,[17BA]
	;	020B:3CC0 7A04            jpe  3CC6 ($+4)
	; Special hack for "Wolfenstein 3D"
	;	1FD6:1CAC F646FC03		  test byte [bp-04],03
	;	1FD6:1CB0 7A02			  jpe  1CB4
	; Special hack for "CALGAMES"
	;	120C:5851 80E7B4          and  bh,B4
	;	120C:5854 7A02            jpe  5858 ($+2)
	; Special hack for "MM3"
	;	06C9:0013 1BC3            sbb  ax,bx
	;	06C9:0015 8CD3            mov  bx,ss
	;	06C9:0017 7A1E            jpe  0037 ($+1e)
	; Special hack for "MM3"
	;	06C9:001B 350281          xor  ax,8102
	;	06C9:001E 7A17            jpe  0037 ($+17)
	; Special hack for "STARGATE"
	;	02DE:18D6 0BC0			  or   ax,ax
	;	02DE:18D8 7A22			  jpe  18FC
	; Special hack for "STARGATE"
	;	02DE:16FF 833E4A0000	  cmp  word [004A],0000
	;	02DE:1704 7A11			  jpe  1717
	; Special hack for "Crime City"
	;	1D4A:0C16 7A00            jpe  0C18 ($+0)	
	; Special hack for "Bubble Ghost" (various similar code segments)
	;	02D9:3939 240C			  and  al,0C
	;	02D9:393B 7A33			  jpe  3970 ($+33)
	; Special hack for "F29 Retaliator" (several locations)
	;	02F0:5088 84E1            test cl,ah
	;	02F0:508A 7A0C            jpe  5098 ($+c)
	; Special hack for "Chess Genius 3"
	;	1B14:3812 0BF6            or   si,si
	;	1B14:3814 0F84C102        jz   00003AD9 ($+2c1)
	;	1B14:3818 7AD4            jpe  000037EE ($-2c)
	; Special hack for "Chess Genius 3"
	;	1B14:3B90 0BF6            or   si,si
	;	1B14:3B92 B00A            mov  al,0A
	;	1B14:3B94 7A0A            jpe  00003BA0 ($+a)
	; Special hack for "Chess Genius 3"
	;	1B14:AD4D F6C488          test ah,88
	;	1B14:AD50 740A            je   0000AD5C ($+a)
	;	1B14:AD52 B1C0            mov  cl,C0
	;	1B14:AD54 7A06            jpe  0000AD5C ($+6)
	; Special hack for "Chess Genius 3"
	;	1B14:B231 A860            test al,60
	;	1B14:B233 7A4B            jpe  0000B280 ($+4b)
	; Special hack for "Chess Genius 3"
	;	1B14:7CC5 A806            test al,06
	;	1B14:7CC7 7420            je   00007CE9 ($+20)
	;	1B14:7CC9 7A1E            jpe  00007CE9 ($+1e)
	; Special hack for "Chess Genius 3"
	;	1B14:4AAB 83C308          add  bx,0008
	;	1B14:4AAE 49              dec  cx
	;	1B14:4AAF 7AD3            jpe  00004A84 ($-2d)
	; Special hack for "Chess Genius 3"
	;	1B14:58C6 A809            test al,09
	;	1B14:58C8 B004            mov  al,04
	;	1B14:58CA 7A02            jpe  000058CE ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:0BB4 C684A1E101      mov  byte [si-1E5F],01
	;	1B14:0BB9 7AF3            jpe  00000BAE ($-d)
	; Special hack for "Chess Genius 3"
	;	1B14:6562 F6C4C0          test ah,C0
	;	1B14:6565 B220            mov  dl,20
	;	1B14:6567 740A            je   00006573 ($+a)
	;	1B14:6569 B22D            mov  dl,2D
	;	1B14:656B 7906            jns  00006573 ($+6)
	;	1B14:656D B22E            mov  dl,2E
	;	1B14:656F 7A02            jpe  00006573 ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:B276 80E411          and  ah,11
	;	1B14:B279 7A01            jpe  0000B27C ($+1)
	; Special hack for "Chess Genius 3"
	;	1B14:5387 F6C5C0          test ch,C0
	;	1B14:538A 7411            je   0000539D ($+11)
	;	1B14:538C B4A2            mov  ah,A2
	;	1B14:538E 7A06            jpe  00005396 ($+6)
	; Special hack for "Chess Genius 3"
	;	1B14:54C5 A8C0            test al,C0
	;	1B14:54C7 7411            je   000054DA ($+11)
	;	1B14:54C9 790B            jns  000054D6 ($+b)
	;	1B14:54CB 7A02            jpe  000054CF ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:43BD 240C            and  al,0C
	;	1B14:43BF 7A0C            jpe  000043CD ($+c)
	; Special hack for "Chess Genius 3"
	;	1B14:45C0 A828            test al,28
	;	1B14:45C2 7431            je   000045F5 ($+31)
	;	1B14:45C4 7A32            jpe  000045F8 ($+32)
	; Special hack for "Chess Genius 3"
	;	1B14:45D7 81E38080        and  bx,8080
	;	1B14:45DB 741E            je   000045FB ($+1e)
	;	1B14:45DD 888DF009        mov  [di+09F0],cl
	;	1B14:45E1 B015            mov  al,15
	;	1B14:45E3 7A0A            jpe  000045EF ($+a)
	; Special hack for "Chess Genius 3"
	;	1B14:481F 02E4            add  ah,ah
	;	1B14:4821 7522            jne  00004845 ($+22)
	;	1B14:4823 E8CEFF          call 000047F4 ($-32)
	;	1B14:4826 66C1CD10        ror  ebp,10
	;	1B14:482A 8B3E2827        mov  di,[2728]
	;	1B14:482E 8B85780A        mov  ax,[di+0A78]
	;	1B14:4832 8A957A0A        mov  dl,[di+0A7A]
	;	1B14:4836 A2FE01          mov  [01FE],al
	;	1B14:4839 E9FAF1          jmp  00003A36 ($-e06)
	;	1B14:483C 7838            js   00004876 ($+38)
	;	1B14:483E 7A20            jpe  00004860 ($+20)
	;	1B14:4840 689948          push 4899
	;	1B14:4843 EBAF            jmp  short 000047F4 ($-51)
	;	1B14:4845 73F5            jnc  0000483C ($-b)
	;	1B14:4847 7A14            jpe  0000485D ($+14)
	; Special hack for "Chess Genius 3"
	;	1B14:449D 83C510          add  bp,0010
	;	1B14:44A0 C3              ret
	;	1B14:44A1 7A1E            jpe  000044C1 ($+1e)
	;	...
	;	1B14:44E2 892E2E27		  mov  [272E],bp
	;	1B14:44E6 73B9			  jnc  000044A1
	;	...
	;	1B14:453D 02E4			  add  ah,ah
	;	1B14:453F 75A1			  jne  000044E2
	; Special hack for "Chess Genius 3"
	;	1B14:7DB6 F6			  test ch,A0	
	;	1B14:7DB9 8B			  mov  si,[2728]
	;	1B14:7DBD 0F			  js   00007F46
	;	...
	;	1B14:7F46 EB02            jmp  short 00007F4A ($+2)
	;	1B14:7F48 7AF2            jpe  00007F3C ($-e)
	; Special hack for "Chess Genius 3"
	;	1B14:41C9 A80C            test al,0C
	;	1B14:41CB 7ABF            jpe  0000418C ($-41)
	; Special hack for "Chess Genius 3"
	;	1B14:41CD A814            test al,14
	;	1B14:41CF 7495            je   00004166 ($-6b)
	;	1B14:41D1 7AB9            jpe  0000418C ($-47)
	; Special hack for "Chess Genius 3"
	;	1B14:44E2 892E2E27        mov  [272E],bp
	;	1B14:44E6 73B9            jnc  000044A1 ($-47)
	;	1B14:44E8 7AC6            jpe  000044B0 ($-3a)
	;	...
	;	1B14:453D 02E4			  add  ah,ah
	;	1B14:453F 75A1			  jne  000044E2
	; Special hack for "Chess Genius 3"
	;	1B14:A530 80E181          and  cl,81
	;	1B14:A533 7406            je   0000A53B ($+6)
	;	1B14:A535 7906            jns  0000A53D ($+6)
	;	1B14:A537 6657            push edi
	;	1B14:A539 7A02            jpe  0000A53D ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:A77F 0AD2            or   dl,dl
	;	1B14:A781 8B86AE01        mov  ax,[bp+01AE]
	;	1B14:A785 7A02            jpe  0000A789 ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:834F F6C47E          test ah,7E
	;	1B14:8352 7402            je   00008356 ($+2)
	;	1B14:8354 7AAC            jpe  00008302 ($-54)
	; Special hack for "Chess Genius 3"
	;	1B14:83BD F606000288      test byte [0200],88
	;	1B14:83C2 7AD2            jpe  00008396 ($-2e)
	; Special hack for "Chess Genius 3"
	;	1B14:B45B F6C381          test bl,81
	;	1B14:B45E 7402            je   0000B462 ($+2)
	;	1B14:B460 7A1A            jpe  0000B47C ($+1a)
	; Special hack for "Chess Genius 3"
	;	1B14:4C95 0BFF            or   di,di
	;	1B14:4C97 75DD            jne  00004C76 ($-23)
	;	1B14:4C99 E92BFE          jmp  00004AC7 ($-1d5)
	;	1B14:4C9C 7A33            jpe  00004CD1 ($+33)
	; Special hack for "Chess Genius 3"
	;	1B14:6247 0AE4            or   ah,ah
	;	1B14:6249 7404            je   0000624F ($+4)
	;	1B14:624B 7A01            jpe  0000624E ($+1)
	; Special hack for "Chess Genius 3"
	;	117D:8483 0AC7            or   al,bh
	;	117D:8485 7AF1            jpe  00008478 ($-f)
	; Special hack for "Chess Genius 3"
	;	15E2:6889 80E309        AND	BL,09                              
	;	15E2:688C 7A1C          JPE	68AA                               	
	; Special hack for "Chess Genius 3"
	;	15E2:BAF7 2411          AND	AL,11                              
	;	15E2:BAF9 B000          MOV	AL,00                              
	;	15E2:BAFB 7A01          JPE	BAFE
	; Special hack for "Super Frog"
	;	0158:00178D77 DFE0      fstsw ax
	;	0158:00178D79 9E        sahf
	;	0158:00178D7A 7AF8      jpe  00178D74 ($-8)
	; Special hack for YADRO "DDISP"
	;	13BD:1C7C 8A66FF          mov  ah,[bp+FF]
	;	13BD:1C7F 80CC01          or   ah,01
	;	13BD:1C82 9E              sahf
	;	13BD:1C83 7B1C            jpo  1CA1 ($+1c)
	;	13BD:1C85 CD3C            int  3C
	;	13BD:1C87 9B2E			  (data)
	;	13BD:1C89 7A1B            jpe  1CA6 ($+%b1)
	; Special hack for Black Knight: Marine Strike Fighter
	;	0180:00289266 F7C610000000    test si,00000010
	;	0180:0028926C 7A06            jpe  00289274 ($+6)
	; Special hack for "Stalingrad"
	;	0180:0020F93B DD7DFE      fstsw [ebp+FE]
	;	0180:0020F93E 8A65FF	  mov  ah,[ebp+FF]
	;   0180:0020F941 9E		  sahf
	;   0180:0020F942 7AF4		  jpe  ($-0C)
	;-------
op_7a
	mrs		r0,cpsr					; r0 = Current flags
	ldrb	r1,[r12, #-3]
	cmp		r1, #0x00
	beq		%f7						; Could be "STARGATE"
	cmp		r1, #0x02
	beq		%f6						; Could be "MM3" or "Chess Genius 3"
	cmp		r1, #0x08
	beq		%f15					; Could be "Chess Genius 3"
	cmp		r1, #0x0A
	beq		%f28					; Could be "Chess Genius 3"
	cmp		r1, #0x0B
	beq		%f1						; Could be "The Incredible Machine" / "Turbo Science" / "STARGATE" / "Chess Genius 3"
	cmp		r1, #0x10
	beq		%f23					; Could be "Chess Genius 3"
	cmp		r1, #0x24
	beq		%f8						; Could be "Bubble Ghost" or "CG3"
	cmp		r1, #0x2B
	beq		%f27					; Could be "Chess Genius 3"
	cmp		r1, #0x66
	beq		%f25					; Could be "Chess Genius 3"
	cmp		r1, #0x73
	beq		%f21					; Could be "Chess Genius 3"
	cmp		r1, #0x74
	beq		%f14					; Could be "Chess Genius 3"
	cmp		r1, #0x78
	beq		%f22					; Could be "Chess Genius 3"
	cmp		r1, #0x79
	beq		%f20					; Could be "Chess Genius 3"
	cmp		r1, #0x84
	beq		%f9						; Could be "F29 Retaliator"
	cmp		r1, #0x8C
	beq		%f5						; Could be "MM3"
	cmp		r1, #0x9B
	beq		%f31						; Could be YADRO "DDISP"
	cmp		r1, #0xA8
	beq		%f13						; Could be "Chess Genius 3"
	cmp		r1, #0xAE
	beq		%f26						; Could be "Chess Genius 3"
	cmp		r1, #0xB0
	beq		%f11						; Could be "Chess Genius 3"
	cmp		r1, #0xB1
	beq		%f12						; Could be "Chess Genius 3"
	cmp		r1, #0xB2
	beq		%f17						; Could be "Chess Genius 3"
	cmp		r1, #0xB4
	beq		%f19						; Could be "Chess Genius 3"
	cmp		r1, #0xBA
	beq		%f2						; Could be "SOR_LOR"
	cmp		r1, #0xC1
	beq		%f10						; Could be "Chess Genius 3"
	cmp		r1, #0xE0
	beq		%f30						; Could be "Super Frog"
	cmp		r1, #0xE1
	beq		%f16						; Could be "Chess Genius 3"
	cmp		r1, #0xE3
	beq		%f29						; Could be "Chess Genius 3"
	cmp		r1, #0xE4
	beq		%f18						; Could be "Chess Genius 3"
	cmp		r1, #0xE7
	beq		%f4						; Could be "CALGAMES"
	cmp		r1, #0xEB
	beq		%f24						; Could be "Chess Genius 3"
	cmp		r1, #0xFC
	beq		%f3						; Could be "Wolfenstein 3D"
	;-------
	; Check for a NOP
	;-------
	ldrb	r1, [r12]
	cmp		r1, #0
	addeq	r12, #1
	beq		restore_flags_from_r0	; Back to loop, restoring flags
	b		unsjpe

	;-------
	;	2004:6F5D 0BFF            or   di,di
	;	2004:6F5F 7A02            jpe  6F63 ($+2)
	;
	;	02DE:18D6 0BC0			  or   ax,ax
	;	02DE:18D8 7A22			  jpe  18FC
	;
	;	15E2:7CF4 0BED          OR	BP,BP                              
	;	15E2:7CF6 7A04          JPE	7CFC                               	
	;-------
1	ldrb	r1,[r12, #-2]
	cmp		r1, #0xFF
	moveq	r1, edi					; Put the "or di,di" result to r1
	beq		op_7a_r1
	cmp		r1, #0xC0
	moveq	r1, eax					; Put the "or ax,ax" result to r1
	beq		op_7a_r1
	cmp		r1, #0xED
	moveq	r1, ebp					; Put the "or bp,bp" result to r1
	beq		op_7a_r1
	b		unsjpe
	;-------
	; Confirm it is "mov bx,[bp+08] & cmp bx,0008 & jne 3CC2 & mov bx,[17BA]"
	;-------
2	ldrb	r1,[r12, #-5]
	cmp		r1, #0x8B
	bne		unsjpe
	ldrb	r1,[r12, #-7]
	cmp		r1, #0x75
	bne		unsjpe
	ldrb	r1,[r12, #-0x0D]
	cmp		r1, #0x8B
	bne		unsjpe
	ldrb	r1,[r12, #-0x0C]
	cmp		r1, #0x5E
	bne		unsjpe
	ldrb	r1,[r12, #-0x0B]
	cmp		r1, #0x08
	bne		unsjpe
	ldrb	r1,[r12, #-0x0A]
	cmp		r1, #0x83
	bne		unsjpe
	;-------
	; Put the "mov bx,[bp+08] & cmp bx,0008" result to r1
	;-------
	ldr		lr, [sp, #SP_PHYS_SS]
	add		r1, ebp, #8				; r1 = (BP+08)
	ubfx	r1, r1, #0, #16
	ldrb	r1,[lr, r1]				; "mov bx,[bp+08]"
	sub		r1, #8					; "cmp bx,0008"
	b		op_7a_r1
	;-------
	; Wolfenstein 3D: Confirm it is "test byte [bp-04],03" = F6 46 FC 03
	;-------
3	ldrb	r1,[r12, #-5]
	cmp		r1, #0xF6
	bne		unsjpe
	ldrb	r1,[r12, #-4]
	cmp		r1, #0x46
	bne		unsjpe
	ldrb	r1,[r12, #-2]
	cmp		r1, #0x03
	bne		unsjpe
	;-------
	; Wolfenstein 3D: Put the "test byte [bp-04],03" result to r1
	;-------
	ldr		lr, [sp, #SP_PHYS_SS]
	sub		r1, ebp, #4				; r1 = (BP-04)
	ubfx	r1, r1, #0, #16
	ldrb	r1,[lr, r1]				; r1 = byte at [BP-04]
	and		r1, #3
	b		op_7a_r1
	;-------
	; Confirm it is "and bh,B4" = 80 E7 B4
	;-------
4	ldrb	r1,[r12, #-4]
	cmp		r1, #0x80
	bne		unsjpe
	ldrb	r1,[r12, #-2]
	cmp		r1, #0xB4
	bne		unsjpe
	;-------
	; Put the "and bh,B4" result to r1
	;-------
	mov		r1, ebx, lsr #8
	and		r1, #0xB4
	b		op_7a_r1
	;-------
	; Confirm it is "sbb ax,bx & mov bx,ss" = %b1 C3 8C D3
	;-------
5	ldrb	r1,[r12, #-5]
	cmp		r1, #0x1B
	bne		unsjpe
	ldrb	r1,[r12, #-4]
	cmp		r1, #0xC3
	bne		unsjpe
	ldrb	r1,[r12, #-2]
	cmp		r1, #0xD3
	bne		unsjpe
	;-------
	; Put the "sbb ax,bx" result to r1
	;-------
	mov		r1, eax
	b		op_7a_r1
	;-------
	; Micro Machines 3
	;	06C9:001B 350281          xor  ax,8102
	;	06C9:001E 7A17            jpe  0037 ($+17)
	;
	; Chess Genius 3
	;	1B14:83BD F606000288      test byte [0200],88
	;	1B14:83C2 7AD2            jpe  00008396 ($-2e)
	;-------
6	ldrb	r1,[r12, #-4]
	cmp		r1, #0x35
	beq		%f1						; Make sure it is Micro Machines 3
	cmp		r1, #0x00
	beq		%f2						; Make sure it is Chess Genius 3
	b		unsjpe
	;-------
	; Make sure it is Micro Machines 3
	;-------
1	ldrb	r1,[r12, #-2]
	cmp		r1, #0x81
	bne		unsjpe
	;-------
	; Put the "xor ax,8102" result to r1
	;-------
	mov		r1, eax
	b		op_7a_r1
	;-------
	; Make sure it is Chess Genius 3
	;-------
2	ldrb	r1,[r12, #-2]
	cmp		r1, #0x88
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x06
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put the "test byte [0200],88" result to r1
	;-------
	mov		r1, r0					; Save flags to r1
	mov		r0, #0x0200				; r0 = [0200] address
	calc_linear_address_r2_from_r0r3	
	mov		r0, r1					; Restore flags from r1
	ldrb	r1, [r2]				; Get byte from [0200]
	and		r1, #0x88
	b		op_7a_r1
	;-------
	; Special hack for "STARGATE"
	;	02DE:16FF 833E4A0000	  cmp  word [004A],0000
	;	02DE:1704 7A11			  jpe  1717
	; Special hack for Black Knight: Marine Strike Fighter
	;	0180:00289266 F7C610000000    test si,00000010
	;	0180:0028926C 7A06            jpe  00289274 ($+6)
	;-------
7	ldrb	r1,[r12, #-6]
	cmp		r1, #0x83
	beq		%f1						; Make sure it is STARGATE
	cmp		r1, #0xC6
	beq		%f2						; Make sure it is Black Knight: Marine Strike Fighter
	b		unsjpe
	;-------
	; Confirm it is "STARGATE"
	;-------
1	ldrb	r1,[r12, #-5]
	cmpeq	r1, #0x3E
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x4A
	ldrbeq	r1,[r12, #-2]
	cmpeq	r1, #0x00
	bne		unsjpe
	;-------
	; Put the "cmp  word [004A],0000" result to r1
	;-------
	mov		r1, r0					; Save flags to r1
	mov		r0, #0x004A				; r0 = [004A] address
	mem_handler_jump_r0r3 op_7a_7, bad_EGA_opcode, bad_MODEX_opcode
op_7a_7
	mov		r0, r1					; Restore flags from r1
	ldrb	r1, [r2]				; Get byte from [004A]
	b		op_7a_r1
	;-------
	; Confirm it is Black Knight: Marine Strike Fighter
	;-------
2	ldrb	r1,[r12, #-7]
	cmpeq	r1, #0xF7
	ldrb	r1,[r12, #-5]
	cmpeq	r1, #0x10
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x00
	ldrbeq	r1,[r12, #-2]
	cmpeq	r1, #0x00
	bne		unsjpe
	;-------
	; Put the "test si,00000010" result to r1
	;-------
	and		r1, esi, #0x10
	b		op_7a_r1
	;-------
	; Confirm it is "and  al,imm8" = 24 imm8
	; Anding AL with the reverse imm8 mask should result in zero.
	;-------
8	ldrb	r1,[r12, #-2]
	bic		r1, eax, r1				; r1 = eax with the mask bits cleared
	ands	r1, #0xFF				; If any AL bits are on, this was not "and AL, imm8"!
	bne		unsjpe
	;-------
	; Put the "and  al,imm8" result into r1
	;-------
	mov		r1, eax					; Put the "and al,imm8" result to r1
	b		op_7a_r1
	;-------
	; Confirm it is "test cl,ah" = 84 E1
	;-------
9	ldrb	r1,[r12, #-2]
	cmp		r1, #0xE1
	bne		unsjpe
	;-------
	; Put the "test cl,ah" result into r1
	;-------
	mov		r1, ecx					; r1 = CL
	and		r1, eax, lsr #8			; r1 = CL and AH
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:3812 0BF6            or   si,si
	;	1B14:3814 0F84C102        jz   00003AD9 ($+2c1)
	;	1B14:3818 7AD4            jpe  000037EE ($-2c)
	;-------
10	ldrb	r1,[r12, #-6]
	cmp		r1, #0xF6
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x0B
	bne		unsjpe
	;-------
	; Put the "or si,si" result into r1
	;-------
	mov		r1, esi
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:3B90 0BF6            or   si,si
	;	1B14:3B92 B00A            mov  al,0A
	;	1B14:3B94 7A0A            jpe  00003BA0 ($+a)
	;
	;	1B14:58C6 A809            test al,09
	;	1B14:58C8 B004            mov  al,04
	;	1B14:58CA 7A02            jpe  000058CE ($+2)
	;
	;	1B14:45D7 81E38080        and  bx,8080
	;	1B14:45DB 741E            je   000045FB ($+1e)
	;	1B14:45DD 888DF009        mov  [di+09F0],cl
	;	1B14:45E1 B015            mov  al,15
	;	1B14:45E3 7A0A            jpe  000045EF ($+a)
	;
	;	15E2:18E3 F6C4C0        TEST	AH,C0                              
	;	15E2:18E6 B020          MOV	AL,20                              
	;	15E2:18E8 740A          JZ	18F4                               
	;	15E2:18EA B02E          MOV	AL,2E                              
	;	15E2:18EC 7A06          JPE	18F4                               
	;
	;	15E2:BAF7 2411          AND	AL,11                              
	;	15E2:BAF9 B000          MOV	AL,00                              
	;	15E2:BAFB 7A01          JPE	BAFE
	;-------
11	ldrb	r1,[r12, #-5]
	cmp		r1, #0xA8
	beq		op_7a_parity_byte
	cmp		r1, #0x24
	beq		op_7a_parity_byte
	cmp		r1, #0x0B
	beq		%f1
	cmp		r1, #0xF0
	beq		%f2
	cmp		r1, #0x74
	beq		%f3
	b		unsjpe
1	ldrb	r1,[r12, #-4]			; Is it "or si,si"?
	cmp		r1, #0xF6
	moveq	r1, esi					; Put the "or si,si" result into r1
	beq		op_7a_r1
	b		unsjpe
2	ldrb	r1, [r12, #-10]			; Is it "and bx,8080"?
	cmp		r1, #0x80
	ldrbeq	r1, [r12, #-11]
	cmpeq	r1, #0x80
	ldrbeq	r1, [r12, #-12]
	cmpeq	r1, #0xE3
	ldrbeq	r1, [r12, #-13]
	cmpeq	r1, #0x81
	bne		unsjpe
	and		r1, ebx, #0x80			; Put the result into r1
	b		op_7a_r1
3	ldrb	r1, [r12, #-6]			; Is it "test ah,C0"?
	cmp		r1, #0x20
	ldrbeq	r1, [r12, #-7]
	cmpeq	r1, #0xB0
	ldrbeq	r1, [r12, #-8]
	cmpeq	r1, #0xC0
	ldrbeq	r1, [r12, #-9]
	cmpeq	r1, #0xC4
	ldrbeq	r1, [r12, #-10]
	cmpeq	r1, #0xF6
	bne		unsjpe
	mov		r1, eax, lsr #8			; Put the result into r1
	and		r1, #0xC0
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:AD4D F6C488          test ah,88
	;	1B14:AD50 740A            je   0000AD5C ($+a)
	;	1B14:AD52 B1C0            mov  cl,C0
	;	1B14:AD54 7A06            jpe  0000AD5C ($+6)
	;-------
12	ldrb	r1,[r12, #-4]
	cmp		r1, #0x0A
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x74
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0x88
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xC4
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put the "test ah,88" result into r1
	;-------
	mov		r1, eax, lsr #8
	and		r1, #0x88
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:B231 A860            test al,60
	;	1B14:B233 7A4B            jpe  0000B280 ($+4b)
	;
	;	1B14:41C9 A80C            test al,0C
	;	1B14:41CB 7ABF            jpe  0000418C ($-41)
	;-------
13	ldrb	r1,[r12, #-2]
	cmp		r1, #0x60
	cmpne	r1, #0x0C
	beq		op_7a_parity_byte
	b		unsjpe
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:7CC5 A806            test al,06
	;	1B14:7CC7 7420            je   00007CE9 ($+20)
	;	1B14:7CC9 7A1E            jpe  00007CE9 ($+1e)
	;
	;	1B14:45C0 A828            test al,28
	;	1B14:45C2 7431            je   000045F5 ($+31)
	;	1B14:45C4 7A32            jpe  000045F8 ($+32)
	;
	;	1B14:41CD A814            test al,14
	;	1B14:41CF 7495            je   00004166 ($-6b)
	;	1B14:41D1 7AB9            jpe  0000418C ($-47)
	;
	;	1B14:834F F6C47E          test ah,7E
	;	1B14:8352 7402            je   00008356 ($+2)
	;	1B14:8354 7AAC            jpe  00008302 ($-54)
	;
	;	15E2:142D F6C430        TEST	AH,30                              
	;	15E2:1430 7402          JZ	1434                               
	;	15E2:1432 7A18          JPE	144C                               
	;
	;	1B14:B45B F6C381          test bl,81
	;	1B14:B45E 7402            je   0000B462 ($+2)
	;	1B14:B460 7A1A            jpe  0000B47C ($+1a)
	;
	;	1B14:6247 0AE4            or   ah,ah
	;	1B14:6249 7404            je   0000624F ($+4)
	;	1B14:624B 7A01            jpe  0000624E ($+1)
	;-------
14	ldrb	r1,[r12, #-5]
	cmp		r1, #0xA8
	beq		op_7a_parity_byte
	cmp		r1, #0xC4
	beq		%f1
	cmp		r1, #0xC3
	beq		%f2
	cmp		r1, #0x0A
	beq		%f3
	b		unsjpe
1	ldrb	r1, [r12, #-6]
	cmp		r1, #0xF6
	bne		unsjpe
	;-------
	;	1B14:834F F6C47E          test ah,7E
	;	15E2:142D F6C430        TEST	AH,30                              
	; Put "test ah,imm8" result into r1
	;-------
	ldrb	r2, [r12, #-4]			; r2 = the imm8 value
	mov		r1, eax, lsr #8
	and		r1, r2
	b		op_7a_r1
2	ldrb	r1, [r12, #-4]
	cmp		r1, #0x81
	ldrbeq	r1, [r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put "test bl,81" result into r1
	;-------
	and		r1, ebx, #0x81
	b		op_7a_r1
3	ldrb	r1, [r12, #-4]
	cmp		r1, #0xE4
	bne		unsjpe
	;-------
	; Put "or ah,ah" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:4AAB 83C308          add  bx,0008
	;	1B14:4AAE 49              dec  cx
	;	1B14:4AAF 7AD3            jpe  00004A84 ($-2d)
	;-------
15	ldrb	r1,[r12, #-2]
	cmp		r1, #0x49
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xC3
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x83
	bne		unsjpe
	;-------
	; Put the "dec cx" result into r1
	;-------
	mov		r1, ecx
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3" (jumps to this code after "or al,al")
	;	1B14:0BB4 C684A1E101      mov  byte [si-1E5F],01
	;	1B14:0BB9 7AF3            jpe  00000BAE ($-d)
	;-------
16	ldrb	r1,[r12, #-2]
	cmp		r1, #0x01
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xA1
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x84
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC6
	bne		unsjpe
	;-------
	; Put the "or al,al" result into r1
	;-------
	mov		r1, eax
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:6562 F6C4C0          test ah,C0
	;	1B14:6565 B220            mov  dl,20
	;	1B14:6567 740A            je   00006573 ($+a)
	;	1B14:6569 B22D            mov  dl,2D
	;	1B14:656B 7906            jns  00006573 ($+6)
	;	1B14:656D B22E            mov  dl,2E
	;	1B14:656F 7A02            jpe  00006573 ($+2)
	;-------
17	ldrb	r1,[r12, #-2]
	cmp		r1, #0x2E
	ldrbeq	r1,[r12, #-12]
	cmpeq	r1, #0xC0
	ldrbeq	r1,[r12, #-13]
	cmpeq	r1, #0xC4
	ldrbeq	r1,[r12, #-14]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put the "test ah,C0" result into r1
	;-------
	mov		r1, eax, lsr #8
	and		r1, #0xC0
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:B276 80E411          and  ah,11
	;	1B14:B279 7A01            jpe  0000B27C ($+1)
	;-------
18	ldrb	r1,[r12, #-2]
	cmp		r1, #0x11
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpe
	;-------
	; Put the "and ah,11" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:00005387 F6C5C0          test ch,C0
	;	1B14:0000538A 7411            je   0000539D ($+11)
	;	1B14:0000538C B4A2            mov  ah,A2
	;	1B14:0000538E 7A06            jpe  00005396 ($+6)
	;-------
19	ldrb	r1,[r12, #-2]
	cmp		r1, #0xA2
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC0
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xC5
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put the "test ch,C0" result into r1
	;-------
	mov		r1, ecx, lsr #8
	and		r1, #0xC0
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:54C5 A8C0            test al,C0
	;	1B14:54C7 7411            je   000054DA ($+11)
	;	1B14:54C9 790B            jns  000054D6 ($+b)
	;	1B14:54CB 7A02            jpe  000054CF ($+2)
	;-------
20	ldrb	r1,[r12, #-2]
	cmp		r1, #0x0B
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC0
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xA8
	bne		unsjpe
	;-------
	; Put the "test al,C0" result into r1
	;-------
	and		r1, eax, #0xC0
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:481F 02E4            add  ah,ah
	;	1B14:4821 7522            jne  00004845 ($+22)
	;	1B14:4823 E8CEFF          call 000047F4 ($-32)
	;	1B14:4826 66C1CD10        ror  ebp,10
	;	1B14:482A 8B3E2827        mov  di,[2728]
	;	1B14:482E 8B85780A        mov  ax,[di+0A78]
	;	1B14:4832 8A957A0A        mov  dl,[di+0A7A]
	;	1B14:4836 A2FE01          mov  [01FE],al
	;	1B14:4839 E9FAF1          jmp  00003A36 ($-e06)
	;	1B14:483C 7838            js   00004876 ($+38)
	;	1B14:483E 7A20            jpe  00004860 ($+20)
	;	1B14:4840 689948          push 4899
	;	1B14:4843 EBAF            jmp  short 000047F4 ($-51)
	;	1B14:4845 73F5            jnc  0000483C ($-b)
	;	1B14:4847 7A14            jpe  0000485D ($+14)
	;
	;	1B14:44E2 892E2E27        mov  [272E],bp
	;	1B14:44E6 73B9            jnc  000044A1 ($-47)
	;	1B14:44E8 7AC6            jpe  000044B0 ($-3a)
	;	...
	;	1B14:453D 02E4			  add  ah,ah
	;	1B14:453F 75A1			  jne  000044E2
	;-------
21	ldrb	r1,[r12, #-2]
	cmp		r1, #0xF5
	beq		%f1
	cmp		r1, #0xB9
	beq		%f2
	b		unsjpe
1	ldrb	r1,[r12, #-4]
	cmp		r1, #0xAF
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0xEB
	ldrbeq	r1,[r12, #-40]
	cmpeq	r1, #0xE4
	ldrbeq	r1,[r12, #-41]
	cmpeq	r1, #0x02
	bne		unsjpe
	;-------
	; Put the "add ah,ah" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
2	ldrb	r1,[r12, #-6]
	cmp		r1, #0x2E
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x89
	ldrbeq	r1,[r12, #(0x453D-0x44E9)]
	cmpeq	r1, #0x02
	ldrbeq	r1,[r12, #(0x453E-0x44E9)]
	cmpeq	r1, #0xE4
	bne		unsjpe
	;-------
	; Put the "add ah,ah" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:481F 02E4            add  ah,ah
	;	1B14:4821 7522            jne  00004845 ($+22)
	;	1B14:4823 E8CEFF          call 000047F4 ($-32)
	;	1B14:4826 66C1CD10        ror  ebp,10
	;	1B14:482A 8B3E2827        mov  di,[2728]
	;	1B14:482E 8B85780A        mov  ax,[di+0A78]
	;	1B14:4832 8A957A0A        mov  dl,[di+0A7A]
	;	1B14:4836 A2FE01          mov  [01FE],al
	;	1B14:4839 E9FAF1          jmp  00003A36 ($-e06)
	;	1B14:483C 7838            js   00004876 ($+38)
	;	1B14:483E 7A20            jpe  00004860 ($+20)
	;-------
22	ldrb	r1,[r12, #-2]
	cmp		r1, #0x38
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xF1
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0xFA
	ldrbeq	r1,[r12, #-31]
	cmpeq	r1, #0xE4
	ldrbeq	r1,[r12, #-32]
	cmpeq	r1, #0x02
	bne		unsjpe
	;-------
	; Put the "add ah,ah" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:449D 83C510          add  bp,0010
	;	1B14:44A0 C3              ret
	;	1B14:44A1 7A1E            jpe  000044C1 ($+1e)
	;	...
	;	1B14:44E2 892E2E27		  mov  [272E],bp
	;	1B14:44E6 73B9			  jnc  000044A1
	;	...
	;	1B14:453D 02E4			  add  ah,ah
	;	1B14:453F 75A1			  jne  000044E2
	;-------
23	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC3
	ldrbeq	r1,[r12, #155]
	cmpeq	r1, #0x02
	ldrbeq	r1,[r12, #156]
	cmpeq	r1, #0xE4
	ldrbeq	r1,[r12, #157]
	cmpeq	r1, #0x75
	ldrbeq	r1,[r12, #158]
	cmpeq	r1, #0xA1
	bne		unsjpe
	;-------
	; Put the "add ah,ah" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:7DB6 F6??A0		  test ch,A0	
	;	1B14:7DB9 8B			  mov  si,[2728]
	;	1B14:7DBD 0F			  js   00007F46
	;	...
	;	1B14:7F46 EB02            jmp  short 00007F4A ($+2)
	;	1B14:7F48 7AF2            jpe  00007F3C ($-e)
	;-------
24	ldrb	r1,[r12, #-2]
	mov		r2, #-(0x7F49-0x7DB6)
	add		r2, r12
	cmp		r1, #0x02
	ldrbeq	r1,[r2, #7]						; [r12, #-(0x7F49-0x7DBD)]
	cmpeq	r1, #0x0F
	ldrbeq	r1,[r2, #3]						; [r12, #-(0x7F49-0x7DB9)]
	cmpeq	r1, #0x8B
	ldrbeq	r1,[r2, #2]						; [r12, #-(0x7F49-0x7DB8)]
	cmpeq	r1, #0xA0
	ldrbeq	r1,[r2]							; [r12, #-(0x7F49-0x7DB6)]
	cmpeq	r1, #0xF6
	bne		unsjpe
	;-------
	; Put the "test ch,A0" result into r1
	;-------
	mov		r1, ecx, lsr #8
	and		r1, #0xA0
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:A530 80E181          and  cl,81
	;	1B14:A533 7406            je   0000A53B ($+6)
	;	1B14:A535 7906            jns  0000A53D ($+6)
	;	1B14:A537 6657            push edi
	;	1B14:A539 7A02            jpe  0000A53D ($+2)
	;-------
25	ldrb	r1,[r12, #-2]
	cmp		r1, #0x57
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x79
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x74
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0xE1
	ldrbeq	r1,[r12, #-10]
	cmpeq	r1, #0x80
	bne		unsjpe
	;-------
	; Put the "and cl,81" result into r1
	;-------
	mov		r1, ecx
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:A77F 0AD2            or   dl,dl
	;	1B14:A781 8B86AE01        mov  ax,[bp+01AE]
	;	1B14:A785 7A02            jpe  0000A789 ($+2)
	;-------
26	ldrb	r1,[r12, #-2]
	cmp		r1, #0x01
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x86
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x8B
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xD2
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x0A
	bne		unsjpe
	;-------
	; Put the "or dl,dl" result into r1
	;-------
	mov		r1, edx
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	1B14:4C95 0BFF            or   di,di
	;	1B14:4C97 75DD            jne  00004C76 ($-23)
	;	1B14:4C99 E92BFE          jmp  00004AC7 ($-1d5)
	;	1B14:4C9C 7A33            jpe  00004CD1 ($+33)
	;-------
27	ldrb	r1,[r12, #-2]
	cmp		r1, #0xFE
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xE9
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xFF
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0x0B
	bne		unsjpe
	;-------
	; Put the "or di,di" result into r1
	;-------
	mov		r1, edi
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	117D:8483 0AC7            or   al,bh
	;	117D:8485 7AF1            jpe  00008478 ($-f)
	;-------
28	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC7
	bne		unsjpe
	;-------
	; Put the "or al,bh" result into r1
	;-------
	mov		r1, eax
	b		op_7a_r1
	;-------
	; Confirm it is "Chess Genius 3"
	;	15E2:6889 80E309        AND	BL,09                              
	;	15E2:688C 7A1C          JPE	68AA                       
	;        	
	;	15E2:6AE1 80E309        AND	BL,09                              
	;	15E2:6AE4 7AE8          JPE	6ACE                               	
	;-------
29	ldrb	r1,[r12, #-2]
	cmp		r1, #0x09
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpe
	;-------
	; Put the "and bl,09" result into r1
	;-------
	mov		r1, ebx
	b		op_7a_r1
	;-------
	; Confirm it is "Super Frog"
	;	0158:00178D77 DFE0      fstsw ax
	;	0158:00178D79 9E        sahf
	;	0158:00178D7A 7AF8      jpe  00178D74 ($-8)
	;-------
30	ldrb	r1,[r12, #-2]
	cmp		r1, #0x9E
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xDF
	bne		unsjpe
	;-------
	; Test the AH register FLAG_PF bit
	;-------
1	tst		eax, #(FLAG_PF<<8)
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	addne	r12, r12, r1			; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0	; Back to loop, restoring flags
	;-------
	; Confirm it is YADRO "DDISP"
	;	13BD:1C7C 8A66FF          mov  ah,[bp+FF]
	;	13BD:1C7F 80CC01          or   ah,01
	;	13BD:1C82 9E              sahf
	;	13BD:1C83 7B1C            jpo  1CA1 ($+1c)
	;	13BD:1C85 CD3C            int  3C
	;	13BD:1C87 9B2E			  (data)
	;	13BD:1C89 7A1B            jpe  1CA6 ($+%b1)
	;-------
31	ldrb	r1, [r12, #-4]
	cmp		r1, #0x3C
	ldrbeq	r1, [r12, #-5]
	cmpeq	r1, #0xCD
	bne		unsjpe
	;-------
	; Get the parity flag from SP_FLAGS, as IRET has put it there.
	;-------
	ldrb	r1, [sp, #SP_FLAGS]
	tst		r1, #FLAG_PF			; Test the parity flag
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	addne	r12, r12, r1			; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0	; Back to loop, restoring flags
	;-------
	; We have the result in SP_PARITY_BYTE, use it.
	;-------
op_7a_parity_byte
	ldrb	r1, [sp, #SP_PARITY_BYTE]
	;-------
	; Calculate parity flag value from r1 register low byte.
	; Use special ARM hack by FluBBa ("http:;www.ndsretro.com/armhacks.html")
	; r1 is input/output byte. 1 = odd, 0 = even parity. Sign flag can also be used if actual value not needed.
	;-------
op_7a_r1
	and		r1, #0xFF
	eor 	r1,r1,r1,lsl#16
	eor 	r1,r1,r1,lsl#8
	eor 	r1,r1,r1,lsl#4
	eor 	r1,r1,r1,lsl#2
	eors 	r1,r1,r1,lsl#1
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	addpl	r12, r12, r1			; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0	; Back to loop, restoring flags
	;------
	; Tell "Unhandled JPE opcode" and exit the emulation.
	;------
	GLOBAL	unsjpe
unsjpe
	ldr		r2, =BreakReason
	ldr		r1, =BRUnsJPE						; Tell "Unhandled JPE opcode"
	str		r1, [r2]
	b		unknown


	;-------
	; JNP/JPO cb
	; Special hack for "Amazing Spiderman"
	; 	3EFE:0CC7 22C0            and  al,al
	; 	3EFE:0CC9 7B0B            jpo  0CD6 ($+b)
	; Special hack for "Tapper"
	;	1645:2F51 2598D5          and  ax,D598
	;	1645:2F54 7B01            jpo  2F57 ($+1)
	; Special hack for "TOUTRUN"
	; 	023F:1BC6 F6C4B4          test ah,B4
	;	023F:1BC9 7B02            jpo  1BCD ($+2)
	; Special hack for "GWBASIC"
	;	04D8:8C6F E85BF1          call 7DCD ($-ea5)		(Does "DEC AL" as the last operation before return)
	;	04D8:8C72 7B52            jpo  8CC6 ($+52)
	; Special hack for "BUBBOB"
	;	1AEC:1329 0BC0            or   ax,ax
	;	1AEC:132B 7B01            jpo  132E ($+1)
	; Special hack for "QB"
	;	2BEA:08A0 F6069C1A30      test byte [1A9C],30
	;	2BEA:08A5 741A            je   08C1 ($+1a)
	;	2BEA:08A7 7B18            jpo  08C1 ($+18)
	; Special hack for "Batman Returns"
	;	12F0:B607 A8C0            test al,C0
	;	12F0:B609 7837            js   B642 ($+37)
	;	12F0:B60B 7B1F            jpo  B62C ($+%1)
	; Special hack for "Batman Returns"
	;	12F0:B640 EBBE			  jmp  short B600 ($-42)
	;	12F0:B642 7B03            jpo  B647 ($+3)
	; Special hack for "F29 Retaliator"
	;	5741:0214 0AC0            or   al,al
	;	5741:0216 781C            js   0234 ($+1c)
	;	5741:0218 8ACF            mov  cl,bh
	;	5741:021A 7B10            jpo  022C ($+10)
	; Special hack for "F29 Retaliator"
	;	02F0:50C3 8BC7            mov  ax,di
	;	02F0:50C5 F6C403          test ah,03
	;	02F0:50C8 A1DB4F          mov  ax,[4FDB]
	;	02F0:50CB 7B2A            jpo  50F7 ($+2a)
	; Special hack for "F29 Retaliator"
	;	02F0:4FDF F606855060      test byte [5085],60
	;	02F0:4FE4 7B04            jpo  4FEA ($+4)
	; Special hack for "Super Solvers: Challenge of the Ancient Empires"
	;	02F0:D6BF A90300          test ax,0003
	;	02F0:D6C2 7408            je   D6CC ($+8)
	;	02F0:D6C4 7B06            jpo  D6CC ($+6)
	; Special hack for "Chess Genius 3"
	;	1AF0:3980 A818            test al,18
	;	1AF0:3982 7505            jne  00003989 ($+5)
	;	1AF0:3984 31BFB001        xor  [bx+01B0],di
	;	1AF0:3988 C3              ret
	;	1AF0:3989 7B0D            jpo  00003998 ($+d)
	; Special hack for "Chess Genius 3"
	;	1B14:3801 2458            and  al,58
	;	1B14:3803 8B2E1427        mov  bp,[2714]
	;	1B14:3807 7B1B            jpo  00003824 ($+%b1)
	; Special hack for "Chess Genius 3"
	;	1B14:BA25 80E288          and  dl,88
	;	1B14:BA28 7B02            jpo  0000BA2C ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:B07A 2403            and  al,03
	;	1B14:B07C 7B09            jpo  0000B087 ($+9)
	; Special hack for "Chess Genius 3"
	;	1B14:B196 0AC9            or   cl,cl
	;	1B14:B198 7B02            jpo  0000B19C ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:B217 F6C318          test bl,18
	;	1B14:B21A 7405            je   0000B221 ($+5)
	;	1B14:B21C 7B03            jpo  0000B221 ($+3)
	; Special hack for "Chess Genius 3"
	;	1B14:4B3B F6C703          test bh,03
	;	1B14:4B3E 7B0B            jpo  00004B4B ($+b)
	; Special hack for "Chess Genius 3"
	;	1B14:4B5D 0BD2            or   dx,dx
	;	1B14:4B5F 7B1E            jpo  00004B7F ($+1e)
	; Special hack for "Chess Genius 3"
	;	1B14:6261 0AE4            or   ah,ah
	;	1B14:6263 7B04            jpo  00006269 ($+4)
	; Special hack for "Chess Genius 3"
	;	1B14:11B8 F6C4C0          test ah,C0
	;	1B14:11BB 790E            jns  000011CB ($+e)
	;	1B14:11BD 7B0E            jpo  000011CD ($+e)
	; Special hack for "Chess Genius 3"
	;	1B14:7E86 80E318          and  bl,18
	;	1B14:7E89 7B11            jpo  00007E9C ($+11)
	; Special hack for "Chess Genius 3"
	;	1B14:0F27 A90180          test ax,8001
	;	1B14:0F2A 79A1            jns  00000ECD ($-%5)
	;	1B14:0F2C 7B8D            jpo  00000EBB ($-73)
	; Special hack for "Chess Genius 3"
	;	1B14:631A A8C1            test al,C1
	;	1B14:631C 7404            je   00006322 ($+4)
	;	1B14:631E 7B02            jpo  00006322 ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:632B A850            test al,50
	;	1B14:632D 7424            je   00006353 ($+24)
	;	1B14:632F 7B0B            jpo  0000633C ($+b)
	; Special hack for "Chess Genius 3"
	;	1B14:43C3 84C9            test cl,cl
	;	1B14:43C5 7B02            jpo  000043C9 ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:3889 80E3C1		  and  bl,C1
	;	1B14:388C 89ADA801        mov  [di+01A8],bp
	;	1B14:3890 7B02            jpo  00003894 ($+2)
	; Special hack for "Chess Genius 3"
	;	1B14:4069 0ADB            or   bl,bl
	;	1B14:406B 7B12            jpo  0000407F ($+12)
	; Special hack for "Chess Genius 3"
	;	1B14:6CFF F6C203          test dl,03
	;	1B14:6D02 7408            je   00006D0C ($+8)
	;	1B14:6D04 BE36E2          mov  si,E236
	;	1B14:6D07 7B03            jpo  00006D0C ($+3)
	; Special hack for "Chess Genius 3"
	;	1B14:B2F6 A803            test al,03
	;	1B14:B2F8 742F            je   0000B329 ($+%2)
	;	1B14:B2FA B420            mov  ah,20
	;	1B14:B2FC 7B0E            jpo  0000B30C ($+e)
	; Special hack for "Chess Genius 3"
	;	1B14:0A65 F6C430          test ah,30
	;	1B14:0A68 740B            je   00000A75 ($+b)
	;	1B14:0A6A 7B09            jpo  00000A75 ($+9)
	; Special hack for "Chess Genius 3"
	;	1B14:0C4D F6C430          test ah,30
	;	1B14:0C50 8A4600          mov  al,[bp]
	;	1B14:0C53 7406            je   00000C5B ($+6)
	;	1B14:0C55 7B04            jpo  00000C5B ($+4)
	; Special hack for "Chess Genius 3"
	;	1B14:499F 80E503          and  ch,03
	;	1B14:49A2 7B98            jpo  0000493C ($-68)
	; Special hack for "Chess Genius 3"
	;	1B14:B675 F6C2C3          test dl,C3
	;	1B14:B678 7B05            jpo  0000B67F ($+5)
	; Special hack for "Chess Genius 3"
	;	1B14:B794 F6C488          test ah,88
	;	1B14:B797 7904            jns  0000B79D ($+4)
	;	1B14:B799 7B01            jpo  0000B79C ($+1)
	; Special hack for "Chess Genius 3"
	;	1B14:0B58 0AC0            or   al,al
	;	1B14:0B5A 7444            je   00000BA0 ($+44)
	;	1B14:0B5C 7856            js   00000BB4 ($+56)
	;	1B14:0B5E C684A1E101      mov  byte [si-1E5F],01
	;	1B14:0B63 7B49            jpo  00000BAE ($+49)
	; Special hack for "Chess Genius 3"
	;	1B14:8361 83F740          xor  di,0040
	;	1B14:8364 7B0E            jpo  00008374 ($+e)
	; Special hack for "Chess Genius 3"
	;	15E2:6F76 F6C1C0        TEST	CL,C0                              
	;	15E2:6F79 7B0A          JPO	6F85
	; Special hack for "Chess Genius 3"
	;	15E2:766F 0AC0          OR	AL,AL                              
	;	15E2:7671 7413          JZ	7686                               
	;	15E2:7673 7817          JS	768C                               
	;	15E2:7675 7B40          JPO	76B7
	; Special hack for "Chess Genius 3"
	;	15E2:8502 F6C541        TEST	CH,41                              
	;	15E2:8505 7409          JZ	8510                               
	;	15E2:8507 7B07          JPO	8510
	; Special hack for "Chess Genius 3"
	;	15E2:C1AA A803          TEST	AL,03                              
	;	15E2:C1AC BB3031        MOV	BX,3130                            
	;	15E2:C1AF 7429          JZ	C1DA                               
	;	15E2:C1B1 7B1E          JPO	C1D1
	; Special hack for "QBASIC"
	;	298C:0638 F6069A1630      test byte [169A],30
	;	298C:063D 741A            je   0659 ($+1a)
	;	298C:063F 7B18            jpo  0659 ($+18)
	; Special hack for "NHL '94"
	;	0691:32F8 80E403          and  ah,03
	;	0691:32FB 7B0B            jpo  3308 ($+b)
	; Special hack for YADRO "DDISP"
	;	13BD:1C7C 8A66FF          mov  ah,[bp+FF]
	;	13BD:1C7F 80CC01          or   ah,01
	;	13BD:1C82 9E              sahf
	;	13BD:1C83 7B1C            jpo  1CA1 ($+1c)
	; Special hack for "Stalingrad"
	;	0180:0020F923 DD7DFE      fstsw [ebp+FE]
	;	0180:0020F926 8A65FF	  mov  ah,[ebp+FF]
	;	0180:0020F929 80CC01      or   ah,01
	;   0180:0020F92C 9E		  sahf
	;   0180:0020F92D 7B18		  jpo  ($+18)
	;-------
op_7b								
	mrs		r0,cpsr					; r0 = Current flags
	ldrb	r1,[r12, #-3]
	cmp		r1, #0x01
	beq		%f12						; Could be "CG3" or "Stalingrad"...
	cmp		r1, #0x0A
	beq		%f16						; Could be "CG3"...
	cmp		r1, #0x0B
	beq		%f1						; Could be "BUBBOB" or "CG3" ...
	cmp		r1, #0x14
	beq		%f13						; Could be "CG3"...
	cmp		r1, #0x22
	beq		%f1						; Could be "Amazing Spiderman"...
	cmp		r1, #0x24
	beq		%f15						; Could be "CG3"...
	cmp		r1, #0x36
	beq		%f23						; Could be "CG3"...
	cmp		r1, #0x50
	beq		%f11						; Could be "F29RETAL"...
	cmp		r1, #0x5B
	beq		%f4						; Could be "GWBASIC"...
	cmp		r1, #0x74
	beq		%f5						; Could be "QB" or "AEPROG" or "CG3"...
	cmp		r1, #0x78
	beq		%f6						; Could be "BATMAN" or "CG3"...
	cmp		r1, #0x79
	beq		%f19						; Could be "Chess Genius 3"...
	cmp		r1, #0x84
	beq		%f21						; Could be "Chess Genius 3"...
	cmp		r1, #0x8A
	beq		%f8						; Could be "F29RETAL"...
	cmp		r1, #0x98
	beq		%f2						; Could be "Tapper"...
	cmp		r1, #0xA8
	beq		%f22						; Could be "Chess Genius 3"...
	cmp		r1, #0xB4
	beq		%f24						; Could be "Chess Genius 3"...
	cmp		r1, #0xC1
	beq		%f31						; Could be "CG3"...
	cmp		r1, #0xC2
	beq		%f28						; Could be "CG3"...
	cmp		r1, #0xC4
	beq		%f3						; Could be "TOUTRUN"...
	cmp		r1, #0xC7
	beq		%f18						; Could be "CG3"...
	cmp		r1, #0xDB
	beq		%f10						; Could be "F29RETAL"...
	cmp		r1, #0xE1
	beq		%f29						; Could be "CG3"...
	cmp		r1, #0xE2
	beq		%f14						; Could be "CG3"...
	cmp		r1, #0xE3
	beq		%f20						; Could be "CG3"...
	cmp		r1, #0xE4
	beq		%f32						; Could be "NHL '94"...
	cmp		r1, #0xE5
	beq		%f27						; Could be "CG3"...
	cmp		r1, #0xEB
	beq		%f7						; Could be "BATMAN"...
	cmp		r1, #0xF7
	beq		%f30						; Could be "CG3"...
	b		unsjpo
	;-------
	;	15E2:4B5D 0BD2          OR	DX,DX
	;	15E2:4B5F 7B1E          JPO	4B7F
	;-------
1	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC0				; Is it "and al,al" or "or ax,ax" ?
	moveq	r1, eax					; Put "and al,al" result into r1
	beq		op_7b_r1
	cmp		r1, #0xD2				; Is it "or dx,dx" ?
	moveq	r1, edx					; Put "or dx,dx" result into r1
	beq		op_7b_r1
	b		unsjpo
	;-------
	; Make sure it is "and ax,D598" = 25 98 D5
	;-------
2	ldrb	r1,[r12, #-4]
	cmp		r1, #0x25
	bne		unsjpo
	ldrb	r1,[r12, #-2]
	cmp		r1, #0xD5
	bne		unsjpo
	;-------
	; Put "and ax,D598" result into r1
	;-------
	and		r1, eax, #0x98
	b		op_7b_r1
	;-------
	; Make sure it is "test ah,B4" = F6 C4 B4
	;-------
3	ldrb	r1,[r12, #-4]
	cmp		r1, #0xF6
	bne		unsjpo
	ldrb	r1,[r12, #-2]
	cmp		r1, #0xB4
	bne		unsjpo
	;-------
	; Put "test ah,B4" result into r1
	;-------
	mov		r1, eax, lsr #8
	and		r1, #0xB4
	b		op_7b_r1
	;-------
	; Make sure it is "call $-ea5" = E8 5B F1
	;-------
4	ldrb	r1,[r12, #-4]
	cmp		r1, #0xE8
	bne		unsjpo
	ldrb	r1,[r12, #-2]
	cmp		r1, #0xF1
	bne		unsjpo
	;-------
	; Put "dec al" result into r1
	;-------
	mov		r1, eax
	b		op_7b_r1
	;-------
	; Test whether it could be "QB", "AEPROG" or "CG3"
	;	2BEA:08A0 F6069C1A30      test byte [1A9C],30			QB (handled here)
	;	2BEA:08A5 741A            je   08C1 ($+1a)
	;	2BEA:08A7 7B18            jpo  08C1 ($+18)
	;
	;	02F0:D6BF A90300          test ax,0003					AEPROG (jump to %9)
	;	02F0:D6C2 7408            je   D6CC ($+8)
	;	02F0:D6C4 7B06            jpo  D6CC ($+6)
	;
	;	1B14:B217 F6C318          test bl,18					CG3 (jump to %17)
	;	1B14:B21A 7405            je   0000B221 ($+5)
	;	1B14:B21C 7B03            jpo  0000B221 ($+3)
	;
	;	1B14:631A A8C1            test al,C1					CG3 (jump to op_7b_test_al_imm8)
	;	1B14:631C 7404            je   00006322 ($+4)
	;	1B14:631E 7B02            jpo  00006322 ($+2)
	;
	;	1B14:632B A850            test al,50					CG3 (jump to op_7b_test_al_imm8)
	;	1B14:632D 7424            je   00006353 ($+24)
	;	1B14:632F 7B0B            jpo  0000633C ($+b)
	;
	;	1B14:0A65 F6C430          test ah,30
	;	1B14:0A68 740B            je   00000A75 ($+b)
	;	1B14:0A6A 7B09            jpo  00000A75 ($+9)
	;
	;	1B14:0C4D F6C430          test ah,30
	;	1B14:0C50 8A4600          mov  al,[bp]
	;	1B14:0C53 7406            je   00000C5B ($+6)
	;	1B14:0C55 7B04            jpo  00000C5B ($+4)
	;
	;	15E2:4CC4 A830          TEST	AL,30					CG3 (jump to op_7b_test_al_imm8)
	;	15E2:4CC6 7408          JZ	4CD0
	;	15E2:4CC8 7B04          JPO	4CCE
	;
	;	15E2:54F6 F606D59D03    TEST	BYTE PTR [9DD5],03                 
	;	15E2:54FB 7420          JZ	551D                               
	;	15E2:54FD 7B1E          JPO	551D
	;
	;	15E2:8502 F6C541        TEST	CH,41                              
	;	15E2:8505 7409          JZ	8510                               
	;	15E2:8507 7B07          JPO	8510
	;
	;	15E2:C1AA A803          TEST	AL,03                              
	;	15E2:C1AC BB3031        MOV	BX,3130                            
	;	15E2:C1AF 7429          JZ	C1DA                               
	;	15E2:C1B1 7B1E          JPO	C1D1
	;
	;	298C:0638 F6069A1630      test byte [169A],30
	;	298C:063D 741A            je   00000659 ($+1a)
	;	298C:063F 7B18            jpo  00000659 ($+18)
	;-------
5	ldrb	r1,[r12, #-5]
	cmp		r1, #0x03
	beq		%f9						; Could be "AEPROG", go test it
	cmp		r1, #0x46
	beq		%f26						; Could be "CG3", go test it
	cmp		r1, #0xC3
	beq		%f17						; Could be "CG3", go test it
	cmp		r1, #0xC4
	beq		%f25						; Could be "CG3", go test it
	cmp		r1, #0xA8
	beq		op_7b_test_al_imm8		; Seems to be "test al,imm8", go handle it
	cmp		r1, #0x1A
	beq		%f1						; Could be "QB", go test it
	cmp		r1, #0x16
	beq		%f1						; Could be "QB", go test it
	cmp		r1, #0x9D
	beq		%f2						; Could be "CG3", go test it
	cmp		r1, #0xC5
	beq		%f3						; Could be "CG3", go test it
	cmp		r1, #0x30
	beq		%f4						; Could be "CG3", go test it
	b		unsjpo
	;-------
	; Make sure it is "QB"
	;	2BEA:08A0 F6069C1A30      test byte [1A9C],30
	;	2BEA:08A5 741A            je   08C1 ($+1a)
	;	2BEA:08A7 7B18            jpo  08C1 ($+18)
	;
	;	298C:0638 F6069A1630      test byte [169A],30
	;	298C:063D 741A            je   00000659 ($+1a)
	;	298C:063F 7B18            jpo  00000659 ($+18)
	;-------
1	ldrb	r1,[r12, #-8]
	cmp		r1, #0xF6
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x06
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x30
	bne		unsjpo
	;-------
	; Put the "test byte [1A9C],30" result to r1
	;-------
	mov		r1, r0					; Save flags to r1
	ldrb	r0,[r12, #-6]
	ldrb	r2,[r12, #-5]
	orr		r0, r2, lsl #8			; r0 = 1A9C / 169A
	ldr		r2, [sp, #SP_DS_BASE]	; r2 = logical DS segment
	calc_linear_address_r2_from_r0r3	
	mov		r0, r1					; Restore flags from r1
	ldrb	r1, [r2]				; Get byte from [1A9C]
	and		r1, #0x30
	b		op_7b_r1
	;-------
	; Make sure it is "CG3"
	;	15E2:54F6 F606D59D03    TEST	BYTE PTR [9DD5],03                 
	;	15E2:54FB 7420          JZ	551D                               
	;	15E2:54FD 7B1E          JPO	551D
	;-------
2	ldrb	r1,[r12, #-8]
	cmp		r1, #0xF6
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x06
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xD5
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x03
	bne		unsjpo
	;-------
	; Put the "test byte [9DD5],03" result to r1
	;-------
	mov		r1, r0					; Save flags to r1
	mov		r0, #0xD5
	orr		r0, #0x9D00
	calc_linear_address_r2_from_r0r3	
	mov		r0, r1					; Restore flags from r1
	ldrb	r1, [r2]				; Get byte from [9DD5]
	and		r1, #0x03
	b		op_7b_r1
	;-------
	; Make sure it is "CG3"
	;	15E2:8502 F6C541        TEST	CH,41                              
	;	15E2:8505 7409          JZ	8510                               
	;	15E2:8507 7B07          JPO	8510
	;-------
3	ldrb	r1,[r12, #-4]
	cmp		r1, #0x41
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put the "test ch,41" result to r1
	;-------
	mov		r1, ecx, lsr #8
	and		r1, #0x41
	b		op_7b_r1
	;-------
	; Make sure it is "CG3"
	;	15E2:C1AA A803          TEST	AL,03                              
	;	15E2:C1AC BB3031        MOV	BX,3130                            
	;	15E2:C1AF 7429          JZ	C1DA                               
	;	15E2:C1B1 7B1E          JPO	C1D1
	;-------
4	ldrb	r1,[r12, #-4]
	cmp		r1, #0x31
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xBB
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x03
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xA8
	bne		unsjpo
	;-------
	; Put the "test al,03" result to r1
	;-------
	and		r1, eax, #0x03
	b		op_7b_r1
	;-------
	; Batman
	;	12F0:B607 A8C0            test al,C0
	;	12F0:B609 7837            js   B642 ($+37)
	;	12F0:B60B 7B1F            jpo  B62C ($+%1)
	; Chess Genius 3
	;	15E2:766F 0AC0          OR	AL,AL                              
	;	15E2:7671 7413          JZ	7686                               
	;	15E2:7673 7817          JS	768C                               
	;	15E2:7675 7B40          JPO	76B7
	;-------
6	ldrb	r1,[r12, #-5]
	cmp		r1, #0xA8
	beq		%f1						; Could be BATMAN
	cmp		r1, #0x74
	beq		%f2						; Could be Chess Genius 3
	b		unsjpo
1	ldrb	r1,[r12, #-4]
	cmp		r1, #0xC0
	ldrbeq	r1,[r12, #-2]
	cmpeq	r1, #0x37
	bne		unsjpo
	;-------
	; Put "test al,C0" result into r1
	;-------
	and		r1, eax, #0xC0
	b		op_7b_r1
2	ldrb	r1,[r12, #-6]
	cmp		r1, #0xC0
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x0A
	bne		unsjpo
	;-------
	; Put "or al,al" result into r1
	;-------
	mov		r1, eax
	b		op_7b_r1
	;-------
	; Make sure it is "Batman Returns", with the other jpo before this one.
	;	12F0:B607 A8C0            test al,C0
	;	12F0:B609 7837            js   B642 ($+37)
	;	12F0:B60B 7B1F            jpo  B62C ($+%1)
	;   ...
	;	12F0:B640 EBBE			  jmp  short B600 ($-42)
	;	12F0:B642 7B03            jpo  B647 ($+3)
	;-------
7	ldrb	r1,[r12, #-2]
	cmp		r1, #0xBE
	bne		unsjpo
	ldrb	r1,[r12, #(0xB607-0xB643)]
	cmp		r1, #0xA8
	bne		unsjpo
	ldrb	r1,[r12, #(0xB608-0xB643)]
	cmp		r1, #0xC0
	bne		unsjpo
	;-------
	; Put "test al,C0" result into r1
	;-------
	and		r1, eax, #0xC0
	b		op_7b_r1
	;-------
	; Make sure it is "F29 Retaliator"
	;	5741:0214 0AC0            or   al,al
	;	5741:0216 781C            js   0234 ($+1c)
	;	5741:0218 8ACF            mov  cl,bh
	;	5741:021A 7B10            jpo  022C ($+10)
	;-------
8	ldrb	r1,[r12, #-2]
	cmp		r1, #0xCF
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x1C
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x78
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC0
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x0A
	bne		unsjpo
	;-------
	; Put "or al,al" result into r1
	;-------
	mov		r1, eax
	b		op_7b_r1
	;-------
	; Make sure it is "AEPROG"
	;	02F0:D6BF A90300          test ax,0003
	;	02F0:D6C2 7408            je   D6CC ($+8)
	;	02F0:D6C4 7B06            jpo  D6CC ($+6)
	;-------
9	ldrb	r1,[r12, #-4]
	cmp		r1, #0x00
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xA9
	bne		unsjpo
	;-------
	; Put "test ax,0003" result into r1
	;-------
	and		r1, eax, #3
	b		op_7b_r1
	;-------
	; Make sure it is "F29 Retaliator"
	;	02F0:50C3 8BC7            mov  ax,di
	;	02F0:50C5 F6C403          test ah,03
	;	02F0:50C8 A1DB4F          mov  ax,[4FDB]
	;	02F0:50CB 7B2A            jpo  50F7 ($+2a)
	;-------
10	ldrb	r1,[r12, #-2]
	cmp		r1, #0x4F
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xA1
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x03
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC4
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xF6
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xC7
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0x8B
	bne		unsjpo	
	;-------
	; Put "mov  ax,di" and "test ah,03" result into r1
	;-------
	mov		r1, edi, lsr #8			; r1 = AH
	and		r1, #3
	b		op_7b_r1
	;-------
	; Make sure it is "F29 Retaliator"
	;	02F0:4FDF F606855060      test byte [5085],60
	;	02F0:4FE4 7B04            jpo  4FEA ($+4)
	;-------
11	ldrb	r1,[r12, #-2]
	cmp		r1, #0x60
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x85
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x06
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpo	
	;-------
	; Put the "test byte [5085],60" result to r1
	;-------
	mov		r1, r0					; Save flags to r1
	mov		r0, #0x85
	orr		r0, #0x5000
	mem_handler_jump_r0r3 op_7b_11, bad_EGA_opcode, bad_MODEX_opcode
op_7b_11
	mov		r0, r1					; Restore flags from r1
	ldrb	r1, [r2]
	and		r1, #0x60
	b		op_7b_r1
	;-------
	; Test whether this is "Chess Genius 3", YADRO "DDISP" or "Stalingrad"
	;-------
12	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC3
	beq		%f1
	cmp		r1, #0x9E
	bne		unsjpo			
	;-------
	; Make sure it is YADRO "DDISP"
	;	13BD:1C7C 8A66FF          mov  ah,[bp+FF]
	;	13BD:1C7F 80CC01          or   ah,01
	;	13BD:1C82 9E              sahf
	;	13BD:1C83 7B1C            jpo  1CA1 ($+1c)
	; or "Stalingrad"
	;	0180:0020F926 8A65FF	  mov  ah,[ebp+FF]
	;	0180:0020F929 80CC01      or   ah,01
	;   0180:0020F92C 9E		  sahf
	;   0180:0020F92D 7B18		  jpo  ($+18)
	;-------
	ldrb	r1, [r12, #-4]
	cmp		r1, #0xCC
	ldrbeq	r1, [r12, #-5]
	cmpeq	r1, #0x80
	ldrbeq	r1, [r12, #-6]
	cmpeq	r1, #0xFF
	ldrbeq	r1, [r12, #-8]
	cmpeq	r1, #0x8A
	bne		unsjpo
	;-------
	; Test the AH register FLAG_PF bit
	;-------
	tst		eax, #(FLAG_PF<<8)
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	addeq	r12, r12, r1			; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0	; Back to loop, restoring flags
	;-------
	; Make sure it is "Chess Genius 3"
	;	1AF0:3980 A818            test al,18
	;	1AF0:3982 7505            jne  00003989 ($+5)
	;	1AF0:3984 31BFB001        xor  [bx+01B0],di
	;	1AF0:3988 C3              ret
	;	1AF0:3989 7B0D            jpo  00003998 ($+d)
	;-------
1	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x05
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0x75
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0x18
	bne		unsjpo	
	;-------
	; Put "test al,18" result into r1
	;-------
	and		r1, eax, #0x18
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:3801 2458            and  al,58
	;	1B14:3803 8B2E1427        mov  bp,[2714]
	;	1B14:3807 7B1B            jpo  00003824 ($+%b1)
	;-------
13	ldrb	r1,[r12, #-6]
	cmp		r1, #0x58
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x24
	bne		unsjpo	
	;-------
	; Put "and al,58" result into r1
	;-------
	and		r1, eax, #0x58
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:BA25 80E288          and  dl,88
	;	1B14:BA28 7B02            jpo  0000BA2C ($+2)
	;-------
14	ldrb	r1,[r12, #-2]
	cmp		r1, #0x88
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpo	
	;-------
	; Put "and dl,88" result into r1
	;-------
	and		r1, edx, #0x88
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:B07A 2403            and  al,03
	;	1B14:B07C 7B09            jpo  0000B087 ($+9)
	;-------
15	ldrb	r1,[r12, #-2]
	cmp		r1, #0x03
	bne		unsjpo	
	;-------
	; Put "and al,03" result into r1
	;-------
	and		r1, eax, #3
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:B196 0AC9            or   cl,cl
	;	1B14:B198 7B02            jpo  0000B19C ($+2)
	; Make sure it is "Chess Genius 3"
	;	1B14:6261 0AE4            or   ah,ah
	;	1B14:6263 7B04            jpo  00006269 ($+4)
	; Make sure it is "Chess Genius 3"
	;	1B14:4069 0ADB            or   bl,bl
	;	1B14:406B 7B12            jpo  0000407F ($+12)
	;-------
16	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC9
	moveq	r1, ecx					; Put "or cl,cl" result into r1
	beq		op_7b_r1
	cmp		r1, #0xE4
	moveq	r1, eax, lsr #8			; Put "or ah,ah" result into r1
	beq		op_7b_r1
	cmp		r1, #0xDB
	moveq	r1, ebx					; Put "or bl,bl" result into r1
	beq		op_7b_r1
	b		unsjpo	
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:B217 F6C318          test bl,18
	;	1B14:B21A 7405            je   0000B221 ($+5)
	;	1B14:B21C 7B03            jpo  0000B221 ($+3)
	;-------
17	ldrb	r1,[r12, #-4]
	cmp		r1, #0x18
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0xC3
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpo	
	;-------
	; Put "test bl,18" result into r1
	;-------
	and		r1, ebx, #0x18
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:4B3B F6C703          test bh,03
	;	1B14:4B3E 7B0B            jpo  00004B4B ($+b)
	;
	;	15E2:41F2 F6C703        TEST	BH,03                              
	;	15E2:41F5 7B95          JPO	418C                               - handled, 
	;-------
18	ldrb	r1,[r12, #-2]
	cmp		r1, #0x03
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xF6
	bne		unsjpo	
	;-------
	; Put "test bh,03" result into r1
	;-------
	mov		r1, ebx, lsr #8
	and		r1, #3
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:11B8 F6C4C0          test ah,C0
	;	1B14:11BB 790E            jns  000011CB ($+e)
	;	1B14:11BD 7B0E            jpo  000011CD ($+e)
	;
	;	1B14:0F27 A90180          test ax,8001
	;	1B14:0F2A 79A1            jns  00000ECD ($-%5)
	;	1B14:0F2C 7B8D            jpo  00000EBB ($-73)
	;
	;	1B14:B794 F6C488          test ah,88
	;	1B14:B797 7904            jns  0000B79D ($+4)
	;	1B14:B799 7B01            jpo  0000B79C ($+1)
	;-------
19	ldrb	r1,[r12, #-6]
	cmp		r1, #0xF6
	bne		%f1
	ldrb	r1,[r12, #-5]
	cmp		r1, #0xC4
	bne		unsjpo
	;-------
	; Calculate "test ah,imm8" value
	;-------
	ldrb	r2, [r12, #-4]
	mov		r1, eax, lsr #8
	and		r1, r2
	b		op_7b_r1
	;-------
	; Make sure it is "test ax,8001"
	;-------
1	cmp		r1, #0xA9
	bne		unsjpo	
	ldrb	r1,[r12, #-5]
	cmp		r1, #0x01
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpo	
	;-------
	; Put "test ax,8001" result into r1
	;-------
	and		r1, eax, #1
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:7E86 80E318          and  bl,18
	;	1B14:7E89 7B11            jpo  00007E9C ($+11)
	;-------
20	ldrb	r1,[r12, #-2]
	cmp		r1, #0x18
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpo	
	;-------
	; Put "and bl,18" result into r1
	;-------
	and		r1, ebx, #0x18
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:43C3 84C9            test cl,cl
	;	1B14:43C5 7B02            jpo  000043C9 ($+2)
	;-------
21	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC9
	bne		unsjpo	
	;-------
	; Put "test cl,cl" result into r1
	;-------
	mov		r1, ecx
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:3889 80E3C1		  and  bl,C1
	;	1B14:388C 89ADA801        mov  [di+01A8],bp
	;	1B14:3890 7B02            jpo  00003894 ($+2)
	;-------
22	ldrb	r1,[r12, #-2]
	cmp		r1, #0x01
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xAD
	ldrbeq	r1,[r12, #-5]
	cmpeq	r1, #0x89
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xC1
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xE3
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0x80
	bne		unsjpo	
	;-------
	; Put "and bl,C1" result into r1
	;-------
	and		r1, ebx, #0xC1
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:6CFF F6C203          test dl,03
	;	1B14:6D02 7408            je   00006D0C ($+8)
	;	1B14:6D04 BE36E2          mov  si,E236
	;	1B14:6D07 7B03            jpo  00006D0C ($+3)
	;-------
23	ldrb	r1,[r12, #-2]
	cmp		r1, #0xE2
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0x03
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xC2
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test dl,03" result into r1
	;-------
	and		r1, edx, #0x03
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:B2F6 A803            test al,03
	;	1B14:B2F8 742F            je   0000B329 ($+%2)
	;	1B14:B2FA B420            mov  ah,20
	;	1B14:B2FC 7B0E            jpo  0000B30C ($+e)
	;-------
24	ldrb	r1,[r12, #-5]
	cmp		r1, #0x74
	ldrbeq	r1,[r12, #-7]
	cmpeq	r1, #0xA8
	beq		op_7b_test_al_imm8
	b		unsjpo
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:0A65 F6C430          test ah,30
	;	1B14:0A68 740B            je   00000A75 ($+b)
	;	1B14:0A6A 7B09            jpo  00000A75 ($+9)
	;-------
25	ldrb	r1,[r12, #-4]
	cmp		r1, #0x30
	ldrbeq	r1,[r12, #-6]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test ah,30" result into r1
	;-------
	mov		r1, eax, lsr #8
	and		r1, #0x30
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:0C4D F6C430          test ah,30
	;	1B14:0C50 8A4600          mov  al,[bp]
	;	1B14:0C53 7406            je   00000C5B ($+6)
	;	1B14:0C55 7B04            jpo  00000C5B ($+4)
	;-------
26	ldrb	r1,[r12, #-7]
	cmp		r1, #0x30
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0xC4
	ldrbeq	r1,[r12, #-9]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test ah,30" result into r1
	;-------
	mov		r1, eax, lsr #8
	and		r1, #0x30
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:499F 80E503          and  ch,03
	;	1B14:49A2 7B98            jpo  0000493C ($-68)
	;-------
27	ldrb	r1,[r12, #-2]
	cmp		r1, #0x03
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpo
	;-------
	; Put "and ch,03" result into r1
	;-------
	mov		r1, ecx, lsr #8
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:B675 F6C2C3          test dl,C3
	;	1B14:B678 7B05            jpo  0000B67F ($+5)
	;-------
28	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC3
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test dl,C3" result into r1
	;-------
	and		r1, edx, #0xC3
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:0B58 0AC0            or   al,al
	;	1B14:0B5A 7444            je   00000BA0 ($+44)
	;	1B14:0B5C 7856            js   00000BB4 ($+56)
	;	1B14:0B5E C684A1E101      mov  byte [si-1E5F],01
	;	1B14:0B63 7B49            jpo  00000BAE ($+49)
	;-------
29	ldrb	r1,[r12, #-6]
	cmp		r1, #0xC6
	ldrbeq	r1,[r12, #-8]
	cmpeq	r1, #0x78
	ldrbeq	r1,[r12, #-10]
	cmpeq	r1, #0x74
	ldrbeq	r1,[r12, #-11]
	cmpeq	r1, #0xC0
	ldrbeq	r1,[r12, #-12]
	cmpeq	r1, #0x0A
	bne		unsjpo
	;-------
	; Put "or al,al" result into r1
	;-------
	mov		r1, eax
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	1B14:8361 83F740          xor  di,0040
	;	1B14:8364 7B0E            jpo  00008374 ($+e)
	;-------
30	ldrb	r1,[r12, #-2]
	cmp		r1, #0x40
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x83
	bne		unsjpo
	;-------
	; Put "xor di,0040" result into r1
	;-------
	mov		r1, edi
	b		op_7b_r1
	;-------
	; Make sure it is "Chess Genius 3"
	;	15E2:6F76 F6C1C0        TEST	CL,C0                              
	;	15E2:6F79 7B0A          JPO	6F85
	;-------
31	ldrb	r1,[r12, #-2]
	cmp		r1, #0xC0
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0xF6
	bne		unsjpo
	;-------
	; Put "test cl,C0" result into r1
	;-------
	and		r1, ecx, #0xC0
	b		op_7b_r1
	;-------
	; Make sure it is "NHL '94"
	;	0691:000032F8 80E403          and  ah,03
	;	0691:000032FB 7B0B            jpo  00003308 ($+b)
	;-------
32	ldrb	r1,[r12, #-2]
	cmp		r1, #0x03
	ldrbeq	r1,[r12, #-4]
	cmpeq	r1, #0x80
	bne		unsjpo
	;-------
	; Put "and ah,03" result into r1
	;-------
	mov		r1, eax, lsr #8
	b		op_7b_r1
	;-------
	; Seems to be "test AL,imm8".
	; We have the result in SP_PARITY_BYTE, use it.
	;-------
op_7b_test_al_imm8
	ldrb	r1,[sp, #SP_PARITY_BYTE]
	;-------
	; Calculate parity flag value from r1 register low byte.
	; Use special ARM hack by FluBBa ("http:;www.ndsretro.com/armhacks.html")
	; r1 is input/output byte. 1 = odd, 0 = even parity. Sign flag can also be used if actual value not needed.
	;-------
op_7b_r1
	and		r1, #0xFF
	eor 	r1,r1,r1,lsl#16
	eor 	r1,r1,r1,lsl#8
	eor 	r1,r1,r1,lsl#4
	eor 	r1,r1,r1,lsl#2
	eors 	r1,r1,r1,lsl#1
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	addmi	r12, r12, r1			; Adjust program counter by the jump amount, if the condition is true
	b		restore_flags_from_r0	; Back to loop, restoring flags
	;------
	; Tell "Unhandled JPO opcode" and exit the emulation.
	;------
	GLOBAL	unsjpo
unsjpo
	ldr		r2, =BreakReason
	ldr		r1, =BRUnsJPO						; Tell "Unhandled JPO opcode"
	str		r1, [r2]
	b		unknown
	
; ------------------- 80 = ??? r/m8,imm8 ------------------------------
;
; All modrm bytes supported!
;
;
	GLOBAL	op_80
op_80
op_82
	modrm_jump_16_tbl op_80_jump
; 0 (idx only)
	modrm_help_1_0 add, imm8
	modrm_help_1_0 or, imm8
	modrm_help_1_0 adc, imm8
	modrm_help_1_0 sbb, imm8
	modrm_help_1_0 and, imm8
	modrm_help_1_0 sub, imm8
	modrm_help_1_0 xor, imm8
	modrm_help_1_0 cmp, imm8
;0x40 (idx+disp8)
	modrm_help_1_40 add, imm8
	modrm_help_1_40 or, imm8
	modrm_help_1_40 adc, imm8
	modrm_help_1_40 sbb, imm8
	modrm_help_1_40 and, imm8
	modrm_help_1_40 sub, imm8
	modrm_help_1_40 xor, imm8
	modrm_help_1_40 cmp, imm8
;0x80 (idx+disp16)
	modrm_help_1_80 add, imm8
	modrm_help_1_80 or, imm8
	modrm_help_1_80 adc, imm8
	modrm_help_1_80 sbb, imm8
	modrm_help_1_80 and, imm8
	modrm_help_1_80 sub, imm8
	modrm_help_1_80 xor, imm8
	modrm_help_1_80 cmp, imm8
;0xc0 = mod = 11b => two register operands
	DCD add_al_imm8, add_cl_imm8, add_dl_imm8, add_bl_imm8, add_ah_imm8, add_ch_imm8, add_dh_imm8, add_bh_imm8
	DCD or_al_imm8, or_cl_imm8, or_dl_imm8, or_bl_imm8, or_ah_imm8, or_ch_imm8, or_dh_imm8, or_bh_imm8
	DCD adc_al_imm8, adc_cl_imm8, adc_dl_imm8, adc_bl_imm8, adc_ah_imm8, adc_ch_imm8, adc_dh_imm8, adc_bh_imm8
	DCD sbb_al_imm8, sbb_cl_imm8, sbb_dl_imm8, sbb_bl_imm8, sbb_ah_imm8, sbb_ch_imm8, sbb_dh_imm8, sbb_bh_imm8
	DCD and_al_imm8, and_cl_imm8, and_dl_imm8, and_bl_imm8, and_ah_imm8, and_ch_imm8, and_dh_imm8, and_bh_imm8
	DCD sub_al_imm8, sub_cl_imm8, sub_dl_imm8, sub_bl_imm8, sub_ah_imm8, sub_ch_imm8, sub_dh_imm8, sub_bh_imm8
	DCD xor_al_imm8, xor_cl_imm8, xor_dl_imm8, xor_bl_imm8, xor_ah_imm8, xor_ch_imm8, xor_dh_imm8, xor_bh_imm8
	DCD cmp_al_imm8, cmp_cl_imm8, cmp_dl_imm8, cmp_bl_imm8, cmp_ah_imm8, cmp_ch_imm8, cmp_dh_imm8, cmp_bh_imm8

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG

	MACRO 
	op80common $oper
	EXTERN	op_80_EGA_$oper
	EXTERN	op_80_MODEX_$oper
	GLOBAL	$oper_t0_bp_imm8
$oper_t0_bp_imm8
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	$oper_t0_imm8
$oper_t0_imm8
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_80_RAM_$oper, op_80_EGA_$oper, op_80_MODEX_$oper
	MEND

	;-------
	; Add: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common add
	GLOBAL	op_80_RAM_add
op_80_RAM_add
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	adds	r1, r0, lsl #24
	lsr		r1, #24					; Shift it to the lowest byte
	strb	r1, [r2]				; Save the changed byte
	b		loop

	;-------
	; Or: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common or
op_80_RAM_or
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	orrs	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	lsr		r1, #24					; Shift it to the lowest byte
	strb	r1, [r2]				; Save the changed byte
	b		loop

	;-------
	; Adc: memory location in normal RAM.
	; On input
	;	r1 = immediate byte << 24
	;	r2 = physical memory address
	;-------
	op80common adc
op_80_RAM_adc
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	addcs	r1, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, r0, lsl #24		; Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	strb	r0, [r2]				; Save the result
	b		loop

	;-------
	; Sbb: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common sbb
op_80_RAM_sbb
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	bcs		%f1						; If input carry is set, we need to calculate the flags ourselves.
	rsbs	r0, r1, r0, lsl #24		; Perform the actual subtraction, setting the resulting flags.
	lsr		r0, #24					; Shift it to the lowest byte
	strb	r0,[r2]					; Save the changed byte
	b		complement_carry
	;-------
	; Input carry is set, so calculate the flags here.
	;-------
1	sub		r3, r0, r1, lsr #24		; r2 = lf_var1d - lf_var2d
	sub		r3, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r3, r3, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	lsr		r3, #24
	strb	r3,[r2]					; Store byte to RAM
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, r1, lsr #24
	eor		r3, r0
	and		r1, r3
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0

	;-------
	; And: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common and
op_80_RAM_and
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	ands	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	lsr		r1, #24					; Shift it to the lowest byte
	strb	r1, [r2]				; Save the changed byte
	b		loop

	;-------
	; Sub: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common sub
op_80_RAM_sub
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	lsr		r1, #24					; Shift it to the lowest byte
	strb	r1,[r2]					; Save the changed byte
	b		complement_carry

	;-------
	; Xor: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common xor
op_80_RAM_xor
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	eors	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	lsr		r1, #24					; Shift it to the lowest byte
	strb	r1, [r2]				; Save the changed byte
	b		loop

	;-------
	; Cmp: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op80common cmp
op_80_RAM_cmp
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	ldrb	r0, [r2] 							; Load byte to r0
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, r0, lsl #24			; Perform the operation using the highest bytes to get the correct flags
	b		complement_carry

; ----- ADD -----

	modrm_1_help add, add_t0, imm8

	MACRO 
	add_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	lsl		r1, #24								; imm8 byte to highest byte of r1
	adds	r1, $reg, lsl #24		; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the lowest byte of the register
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the register
	b		loop
	MEND
	MACRO 
	add_reg8h_imm8 $reg
	; ----- On input, r1 = imm8 byte value in high byte
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	and		r0, $reg, #0xFF00		; Put the reg8h value to the r0, clearing reg8l value
	lsl		r1, #24								; imm8 byte to highest byte of r1
	adds	r1, r0, lsl #16			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the reg8h value of the register
	orr		$reg, r1, lsr #16		; Put the result to the highest byte of the register
	b		loop
	MEND

add_al_imm8
	add_reg8l_imm8 eax
add_cl_imm8
	add_reg8l_imm8 ecx
add_dl_imm8
	add_reg8l_imm8 edx
add_bl_imm8
	add_reg8l_imm8 ebx
add_ah_imm8
	add_reg8h_imm8 eax
add_ch_imm8
	add_reg8h_imm8 ecx
add_dh_imm8
	add_reg8h_imm8 edx
add_bh_imm8
	add_reg8h_imm8 ebx

	GLOBAL	add_al_imm8
	GLOBAL	add_cl_imm8
	GLOBAL	add_dl_imm8
	GLOBAL	add_bl_imm8
	GLOBAL	add_ah_imm8
	GLOBAL	add_ch_imm8
	GLOBAL	add_dh_imm8
	GLOBAL	add_bh_imm8

; ----- OR -----

	modrm_1_help or, or_t0, imm8

	MACRO 
	or_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	mov		r0, $reg, lsl #24		; Shift the reg value to the topmost byte of r0
	orrs	r1, r0					; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the lowest byte of the register
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the register
	b		loop
	MEND

	MACRO 
	or_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	and		r0, $reg, #0xFF00		; Put the reg8h value to the r0, clearing reg8l value
	orrs	r1, r0, lsl #16			; Perform the addition using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the reg8h value of the register
	orr		$reg, r1, lsr #16		; Put the result to the highest byte of the register
	b		loop
	MEND

or_al_imm8
	or_reg8l_imm8 eax
or_cl_imm8
	or_reg8l_imm8 ecx
or_dl_imm8
	or_reg8l_imm8 edx
or_bl_imm8
	or_reg8l_imm8 ebx
or_ah_imm8
	or_reg8h_imm8 eax
or_ch_imm8
	or_reg8h_imm8 ecx
or_dh_imm8
	or_reg8h_imm8 edx
or_bh_imm8
	or_reg8h_imm8 ebx

	GLOBAL or_al_imm8
	GLOBAL or_cl_imm8
	GLOBAL or_dl_imm8
	GLOBAL or_bl_imm8
	GLOBAL or_ah_imm8
	GLOBAL or_ch_imm8
	GLOBAL or_dh_imm8
	GLOBAL or_bh_imm8

; ----- ADC -----

	modrm_1_help adc, adc_t0, imm8

	MACRO 
	adc_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	lsl		r1, #24								; imm8 byte to highest byte of r1
	addcs	r1, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r1, $reg, lsl #24		; Perform the actual addition, setting the resulting flags.
	bic		$reg, #0xFF				; Clear the lowest byte of the register
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the register
	b		loop
	MEND

	MACRO 
	adc_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	and		r0, $reg, #0xFF00		; Put the reg8h value to the r1, clearing reg8l value
	lsl		r1, #24								; imm8 byte to highest byte of r1
	addcs	r1, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r1, r0, lsl #16			; Perform the actual addition, setting the resulting flags.
	bic		$reg, #0xFF00			; Clear the reg8h value of the register
	orr		$reg, r1, lsr #16		; Put the result to the highest byte of the register
	b		loop
	MEND

adc_al_imm8
	adc_reg8l_imm8 eax
adc_cl_imm8
	adc_reg8l_imm8 ecx
adc_dl_imm8
	adc_reg8l_imm8 edx
adc_bl_imm8
	adc_reg8l_imm8 ebx
adc_ah_imm8
	adc_reg8h_imm8 eax
adc_ch_imm8
	adc_reg8h_imm8 ecx
adc_dh_imm8
	adc_reg8h_imm8 edx
adc_bh_imm8
	adc_reg8h_imm8 ebx

	GLOBAL adc_al_imm8
	GLOBAL adc_cl_imm8
	GLOBAL adc_dl_imm8
	GLOBAL adc_bl_imm8
	GLOBAL adc_ah_imm8
	GLOBAL adc_ch_imm8
	GLOBAL adc_dh_imm8
	GLOBAL adc_bh_imm8

; ----- SBB -----

	modrm_1_help sbb, sbb_t0, imm8

	MACRO 
	sbb_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	lsl		r1, #24								; imm8 byte to highest byte of r1
	bcs		%f1
	rsbs	r0, r1, $reg, lsl #24	; Perform the actual subtraction, setting the resulting flags.
	bic		$reg, #0xFF				; Clear the lowest byte of the register
	orr		$reg, r0, lsr #24		; Put the result to the lowest byte of the register
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	and		r0, $reg, #0xFF	
	sub		r2, r0, r1, lsr #24		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r2, lsr #24		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, r1, lsr #24
	eor		r2, r0, r2, lsr #24
	and		r1, r2
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

	MACRO 
	sbb_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	and		r0, $reg, #0xFF00		; Put the reg8h value to the r1, clearing reg8l value
	lsl		r1, #24								; imm8 byte to highest byte of r1
	bcs		%f1
	rsbs	r0, r1, r0, lsl #16		; Perform the actual subtraction, setting the resulting flags.
	bic		$reg, #0xFF00			; Clear the reg8h value of the register
	orr		$reg, r0, lsr #16		; Put the result to the highest byte of the register
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	lsr		r1, #24
	rsb		r2, r1, r0, lsr #8		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #24			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF00			; Clear the current reg8l value
	orr		$reg, r2, lsr #16		; Put the result to the lower byte of the high halfword of the left register
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x80
	;-------
	eor		r1, r0, lsr #8
	eor		r2, r0, lsl #16
	and		r1, r2, lsr #24
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x80
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

sbb_al_imm8
	sbb_reg8l_imm8 eax
sbb_cl_imm8
	sbb_reg8l_imm8 ecx
sbb_dl_imm8
	sbb_reg8l_imm8 edx
sbb_bl_imm8
	sbb_reg8l_imm8 ebx
sbb_ah_imm8
	sbb_reg8h_imm8 eax
sbb_ch_imm8
	sbb_reg8h_imm8 ecx
sbb_dh_imm8
	sbb_reg8h_imm8 edx
sbb_bh_imm8
	sbb_reg8h_imm8 ebx

	GLOBAL sbb_al_imm8
	GLOBAL sbb_cl_imm8
	GLOBAL sbb_dl_imm8
	GLOBAL sbb_bl_imm8
	GLOBAL sbb_ah_imm8
	GLOBAL sbb_ch_imm8
	GLOBAL sbb_dh_imm8
	GLOBAL sbb_bh_imm8

; ----- AND -----

	modrm_1_help and, and_t0, imm8

	MACRO 
	and_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	mov		r0, $reg, lsl #24
	ands	r1, r0					; AND the values using the top byte, setting flags
	bic		$reg, #0xFF
	orr		$reg, r1, lsr #24
	b		loop
	MEND

	MACRO 
	and_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	mov		r0, $reg, lsl #16
	ands	r1, r0					; AND the values using the top byte, setting flags
	bic		$reg, #0xFF00
	orr		$reg, r1, lsr #16
	b		loop
	MEND

and_al_imm8
	and_reg8l_imm8 eax
and_cl_imm8
	and_reg8l_imm8 ecx
and_dl_imm8
	and_reg8l_imm8 edx
and_bl_imm8
	and_reg8l_imm8 ebx

and_ah_imm8
	and_reg8h_imm8 eax
and_ch_imm8
	and_reg8h_imm8 ecx
and_dh_imm8
	and_reg8h_imm8 edx
and_bh_imm8
	and_reg8h_imm8 ebx

	GLOBAL and_al_imm8
	GLOBAL and_cl_imm8
	GLOBAL and_dl_imm8
	GLOBAL and_bl_imm8
	GLOBAL and_ah_imm8
	GLOBAL and_ch_imm8
	GLOBAL and_dh_imm8
	GLOBAL and_bh_imm8

; ----- SUB -----

	modrm_1_help sub, sub_t0, imm8

	MACRO 
	sub_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, $reg, lsl #24		; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF				; Clear the lowest byte of the register
	orr		$reg, r1, lsr #24		; Put the result to the lowest byte of the register
	b		complement_carry
	MEND
	MACRO 
	sub_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	and		r0, $reg, #0xFF00		; Put the reg8h value to the r0, clearing reg8l value
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, r0, lsl #16			; Perform the operation using the highest bytes to get the correct flags
	bic		$reg, #0xFF00			; Clear the reg8h value of the register
	orr		$reg, r1, lsr #16		; Put the result to the highest byte of the register
	b		complement_carry
	MEND

sub_al_imm8
	sub_reg8l_imm8 eax
sub_cl_imm8
	sub_reg8l_imm8 ecx
sub_dl_imm8
	sub_reg8l_imm8 edx
sub_bl_imm8
	sub_reg8l_imm8 ebx
sub_ah_imm8
	sub_reg8h_imm8 eax
sub_ch_imm8
	sub_reg8h_imm8 ecx
sub_dh_imm8
	sub_reg8h_imm8 edx
sub_bh_imm8
	sub_reg8h_imm8 ebx

	GLOBAL sub_al_imm8
	GLOBAL sub_cl_imm8
	GLOBAL sub_dl_imm8
	GLOBAL sub_bl_imm8
	GLOBAL sub_ah_imm8
	GLOBAL sub_ch_imm8
	GLOBAL sub_dh_imm8
	GLOBAL sub_bh_imm8

; ----- XOR -----

	modrm_1_help xor, xor_t0, imm8

	MACRO 
	xor_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	mov		r0, $reg, lsl #24
	eors	r1, r0					; XOR the values using the top byte, setting flags
	bic		$reg, #0xFF
	orr		$reg, r1, lsr #24
	b		loop
	MEND

	MACRO 
	xor_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	lsl		r1, #24								; imm8 byte to highest byte of r1
	and		r0, $reg, #0xFF00		; Put reg8h to r1 and clear all but the topmost byte
	eors	r1, r0, lsl #16			; XOR the values using the top byte, setting flags
	bic		$reg, #0xFF00
	orr		$reg, r1, lsr #16
	b		loop
	MEND

xor_al_imm8
	xor_reg8l_imm8 eax
xor_cl_imm8
	xor_reg8l_imm8 ecx
xor_dl_imm8
	xor_reg8l_imm8 edx
xor_bl_imm8
	xor_reg8l_imm8 ebx

xor_ah_imm8
	xor_reg8h_imm8 eax
xor_ch_imm8
	xor_reg8h_imm8 ecx
xor_dh_imm8
	xor_reg8h_imm8 edx
xor_bh_imm8
	xor_reg8h_imm8 ebx

	GLOBAL xor_al_imm8
	GLOBAL xor_cl_imm8
	GLOBAL xor_dl_imm8
	GLOBAL xor_bl_imm8
	GLOBAL xor_ah_imm8
	GLOBAL xor_ch_imm8
	GLOBAL xor_dh_imm8
	GLOBAL xor_bh_imm8

; ----- CMP -----

	modrm_1_help cmp, cmp_t0, imm8
	
	MACRO 
	cmp_reg8l_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, $reg, lsl #24		; Shift the register also to the topmost byte, and then compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	MACRO 
	cmp_reg8h_imm8 $reg
	ldrb	r1,[r12], #1						; Load the imm8 byte to r1
	and		r0, $reg, #0xFF00		; Put reg8h to r1 and clear out the low byte
	lsl		r1, #24								; imm8 byte to highest byte of r1
	rsbs	r1, r0, lsl #16			; Shift the immediate byte also to the topmost byte, and then compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

cmp_al_imm8
	cmp_reg8l_imm8 eax
cmp_cl_imm8
	cmp_reg8l_imm8 ecx
cmp_dl_imm8
	cmp_reg8l_imm8 edx
cmp_bl_imm8
	cmp_reg8l_imm8 ebx
cmp_ah_imm8
	cmp_reg8h_imm8 eax
cmp_ch_imm8
	cmp_reg8h_imm8 ecx
cmp_dh_imm8
	cmp_reg8h_imm8 edx
cmp_bh_imm8
	cmp_reg8h_imm8 ebx

	GLOBAL cmp_al_imm8
	GLOBAL cmp_cl_imm8
	GLOBAL cmp_dl_imm8
	GLOBAL cmp_bl_imm8
	GLOBAL cmp_ah_imm8
	GLOBAL cmp_ch_imm8
	GLOBAL cmp_dh_imm8
	GLOBAL cmp_bh_imm8

; ------------------- 81 = ??? r/m16,imm16 -------------------------------
; 
; All modrm bytes supported!
;
op_81
	modrm_jump_16_tbl op_81_jump
; 0 (idx only)
	modrm_help_1_0 add, imm16
	modrm_help_1_0 or, imm16
	modrm_help_1_0 adc, imm16
	modrm_help_1_0 sbb, imm16
	modrm_help_1_0 and, imm16
	modrm_help_1_0 sub, imm16
	modrm_help_1_0 xor, imm16
	modrm_help_1_0 cmp, imm16
;0x40 (idx+disp8)
	modrm_help_1_40 add, imm16
	modrm_help_1_40 or, imm16
	modrm_help_1_40 adc, imm16
	modrm_help_1_40 sbb, imm16
	modrm_help_1_40 and, imm16
	modrm_help_1_40 sub, imm16
	modrm_help_1_40 xor, imm16
	modrm_help_1_40 cmp, imm16
;0x80 (idx+disp16)
	modrm_help_1_80 add, imm16
	modrm_help_1_80 or, imm16
	modrm_help_1_80 adc, imm16
	modrm_help_1_80 sbb, imm16
	modrm_help_1_80 and, imm16
	modrm_help_1_80 sub, imm16
	modrm_help_1_80 xor, imm16
	modrm_help_1_80 cmp, imm16
;0xc0 = mod = 11b => register operand
	DCD add_ax_imm16, add_cx_imm16, add_dx_imm16, add_bx_imm16, add_sp_imm16, add_bp_imm16, add_si_imm16, add_di_imm16
	DCD or_ax_imm16, or_cx_imm16, or_dx_imm16, or_bx_imm16, or_sp_imm16, or_bp_imm16, or_si_imm16, or_di_imm16
	DCD adc_ax_imm16, adc_cx_imm16, adc_dx_imm16, adc_bx_imm16, adc_sp_imm16, adc_bp_imm16, adc_si_imm16, adc_di_imm16
	DCD sbb_ax_imm16, sbb_cx_imm16, sbb_dx_imm16, sbb_bx_imm16, sbb_sp_imm16, sbb_bp_imm16, sbb_si_imm16, sbb_di_imm16
	DCD and_ax_imm16, and_cx_imm16, and_dx_imm16, and_bx_imm16, and_sp_imm16, and_bp_imm16, and_si_imm16, and_di_imm16
	DCD sub_ax_imm16, sub_cx_imm16, sub_dx_imm16, sub_bx_imm16, sub_sp_imm16, sub_bp_imm16, sub_si_imm16, sub_di_imm16
	DCD xor_ax_imm16, xor_cx_imm16, xor_dx_imm16, xor_bx_imm16, xor_sp_imm16, xor_bp_imm16, xor_si_imm16, xor_di_imm16
	DCD cmp_ax_imm16, cmp_cx_imm16, cmp_dx_imm16, cmp_bx_imm16, cmp_sp_imm16, cmp_bp_imm16, cmp_si_imm16, cmp_di_imm16

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	op81common $oper
	EXTERN	op_81_EGA_$oper
	GLOBAL	$oper_r0_bp_imm16
$oper_r0_bp_imm16
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	$oper_r0_imm16
$oper_r0_imm16
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_81_RAM_$oper, op_81_EGA_$oper, unknown
	MEND

	;-------
	; Add: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common add
op_81_RAM_add
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12, #1]			; r1 = high byte of imm16 value
	orr		r0, r1, lsl #24			; Save it to r0 highest byte
	ldrb	r1, [r12],#2			; r1 = low byte of imm16 value
	orr		r1, r0, lsr #16			; r1 = imm16 value in low halfword
	lsl		r0, #16					; r0 = RAM value in high halfword
	;-------
	; Perform the actual operation
	;-------
	adds	r0, r1, lsl #16			; ADD the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Or: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common or
op_81_RAM_or
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12, #1]			; r1 = high byte of imm16 value
	orr		r0, r1, lsl #24			; Save it to r0 highest byte
	ldrb	r1, [r12],#2			; r1 = low byte of imm16 value
	orr		r1, r0, lsr #16			; r1 = imm16 value in low halfword
	lsl		r0, #16					; r0 = RAM value in high halfword
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	orrs	r0, r1					; OR the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Adc: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common adc
op_81_RAM_adc
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	ldrb	r3, [r12], #1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12], #1			; r1 = high byte of imm16 value
	lsl		r3, #16
	orr		r1, r3, r1, lsl #24		; r1 = imm16 value in high 16bits
	;-------
	; Perform the actual operation
	;-------
	addcs	r1, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, r0, lsl #16		; Perform the actual addition, setting the resulting flags.
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop

	;-------
	; Sbb: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common sbb
op_81_RAM_sbb
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	ldrb	r3, [r12], #1			; r3 = low byte of imm16 value
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12], #1			; r1 = high byte of imm16 value
	bcs		%f1
	;-------
	; Perform the actual operation
	;-------
	lsl		r3, #16
	orr		r1, r3, r1, lsl #24		; r1 = imm16 value in high 16 bits
	rsbs	r0, r1, r0, lsl #16		; Perform the actual subtraction, setting the resulting flags.
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		complement_carry
	;-------
	; Input carry set, calculate the flags separately
	;-------
1	orr		r1, r3, r1, lsl #8		; r1 = imm16 value
	sub		r3, r0, r1				; r3 = lf_var1d - lf_var2d
	sub		r3, #1					; r3 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r3, r3, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	lsr		r3, #16
	strb	r3,[r2]					; Store byte to RAM
	ror		r3, #8
	strb	r3,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0
	eor		r3, r0, r3, ror #24
	and		r1, r3
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0


	;-------
	; And: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common and
op_81_RAM_and
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12, #1]			; r1 = high byte of imm16 value
	orr		r0, r1, lsl #24			; Save it to r0 highest byte
	ldrb	r1, [r12],#2			; r1 = low byte of imm16 value
	orr		r1, r0, lsr #16			; r1 = imm16 value in low halfword
	lsl		r0, #16					; r0 = RAM value in high halfword
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	ands	r0, r1					; AND the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Sub: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common sub
op_81_RAM_sub
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12, #1]			; r1 = high byte of imm16 value
	orr		r0, r1, lsl #24			; Save it to r0 highest byte
	ldrb	r1, [r12],#2			; r1 = low byte of imm16 value
	orr		r1, r0, lsr #16			; r1 = imm16 value in low halfword
	lsl		r0, #16					; r0 = RAM value in high halfword
	;-------
	; Perform the actual operation
	;-------
	subs	r0, r1, lsl #16			; SUB the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		complement_carry

	;-------
	; Xor: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common xor
op_81_RAM_xor
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	orr		r0, r1, lsl #8			; r0 low halfword = value from RAM
	ldrb	r1, [r12, #1]			; r1 = high byte of imm16 value
	orr		r0, r1, lsl #24			; Save it to r0 highest byte
	ldrb	r1, [r12],#2			; r1 = low byte of imm16 value
	orr		r1, r0, lsr #16			; r1 = imm16 value in low halfword
	lsl		r0, #16					; r0 = RAM value in high halfword
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	eors	r0, r1					; XOR the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Cmp: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op81common cmp
op_81_RAM_cmp
	;-------
	; Load the values from RAM and imm16
	;-------
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	lsl		r0, #16					; r0 = RAM value in high halfword
	orr		r0, r1, lsl #24			; r0 high halfword = value from RAM
	ldrb	r1, [r12], #1
	ldrb	r2, [r12], #1
	lsl		r1, #16
	orr		r1, r2, lsl #24			; r1 high halfword = imm16 value
	;-------
	; Perform the actual operation
	;-------
	cmp		r0, r1					; CMP the values, setting flags
	b		complement_carry

	LTORG

; ----- ADD -----

	modrm_1_help add, add_r0, imm16

	MACRO 
	add_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	adds	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

add_ax_imm16
	add_reg16_imm16 eax
add_cx_imm16
	add_reg16_imm16 ecx
add_dx_imm16
	add_reg16_imm16 edx
add_bx_imm16
	add_reg16_imm16 ebx
add_sp_imm16
	add_reg16_imm16 esp
add_bp_imm16
	add_reg16_imm16 ebp
add_si_imm16
	add_reg16_imm16 esi
add_di_imm16
	add_reg16_imm16 edi

	GLOBAL add_ax_imm16
	GLOBAL add_cx_imm16 
	GLOBAL add_dx_imm16 
	GLOBAL add_bx_imm16 
	GLOBAL add_sp_imm16 
	GLOBAL add_bp_imm16 
	GLOBAL add_si_imm16 
	GLOBAL add_di_imm16

; ----- OR -----

	modrm_1_help or, or_r0, imm16

	MACRO 
	or_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	orrs	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

or_ax_imm16
	or_reg16_imm16 eax
or_cx_imm16
	or_reg16_imm16 ecx
or_dx_imm16
	or_reg16_imm16 edx
or_bx_imm16
	or_reg16_imm16 ebx
or_sp_imm16
	or_reg16_imm16 esp
or_bp_imm16
	or_reg16_imm16 ebp
or_si_imm16
	or_reg16_imm16 esi
or_di_imm16
	or_reg16_imm16 edi

	GLOBAL  or_ax_imm16
	GLOBAL  or_cx_imm16
	GLOBAL  or_dx_imm16
	GLOBAL  or_bx_imm16
	GLOBAL  or_sp_imm16
	GLOBAL  or_bp_imm16
	GLOBAL  or_si_imm16
	GLOBAL  or_di_imm16

; ----- ADC -----

	modrm_1_help adc, adc_r0, imm16

	MACRO 
	adc_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eor		$reg, r2, lsr #16
	addcs	r2, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r2, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r2, r0, lsl #16		; Perform the actual addition, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		loop
	MEND

adc_ax_imm16
	adc_reg16_imm16 eax
adc_cx_imm16
	adc_reg16_imm16 ecx
adc_dx_imm16
	adc_reg16_imm16 edx
adc_bx_imm16
	adc_reg16_imm16 ebx
adc_sp_imm16
	adc_reg16_imm16 esp
adc_bp_imm16
	adc_reg16_imm16 ebp
adc_si_imm16
	adc_reg16_imm16 esi
adc_di_imm16
	adc_reg16_imm16 edi

	GLOBAL  adc_ax_imm16
	GLOBAL  adc_cx_imm16
	GLOBAL  adc_dx_imm16
	GLOBAL  adc_bx_imm16
	GLOBAL  adc_sp_imm16
	GLOBAL  adc_bp_imm16
	GLOBAL  adc_si_imm16
	GLOBAL  adc_di_imm16

; ----- SBB -----

	modrm_1_help sbb, sbb_r0, imm16

	MACRO 
	sbb_reg16_imm16 $reg
	ldrb	r1,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r2,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r0, $reg, lsl #16
	orr		r1, r2, lsl #8			; r0 = low byte | (high byte << 8)
	eor		$reg, r0, lsr #16
	bcs		%f1
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	rsb		r2, r1, r0, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	orr		$reg, r2, lsr #16
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, lsr #16
	eor		r2, r0
	and		r1, r2, lsr #16
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

sbb_ax_imm16
	sbb_reg16_imm16 eax
sbb_cx_imm16
	sbb_reg16_imm16 ecx
sbb_dx_imm16
	sbb_reg16_imm16 edx
sbb_bx_imm16
	sbb_reg16_imm16 ebx
sbb_sp_imm16
	sbb_reg16_imm16 esp
sbb_bp_imm16
	sbb_reg16_imm16 ebp
sbb_si_imm16
	sbb_reg16_imm16 esi
sbb_di_imm16
	sbb_reg16_imm16 edi

	GLOBAL  sbb_ax_imm16
	GLOBAL  sbb_cx_imm16
	GLOBAL  sbb_dx_imm16
	GLOBAL  sbb_bx_imm16
	GLOBAL  sbb_sp_imm16
	GLOBAL  sbb_bp_imm16
	GLOBAL  sbb_si_imm16
	GLOBAL  sbb_di_imm16

; ----- AND -----

	modrm_1_help and, and_r0, imm16

	MACRO 
	and_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	ands	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

and_ax_imm16
	and_reg16_imm16 eax
and_cx_imm16
	and_reg16_imm16 ecx
and_dx_imm16
	and_reg16_imm16 edx
and_bx_imm16
	and_reg16_imm16 ebx
and_sp_imm16
	and_reg16_imm16 esp
and_bp_imm16
	and_reg16_imm16 ebp
and_si_imm16
	and_reg16_imm16 esi
and_di_imm16
	and_reg16_imm16 edi

	GLOBAL  and_ax_imm16
	GLOBAL  and_cx_imm16
	GLOBAL  and_dx_imm16
	GLOBAL  and_bx_imm16
	GLOBAL  and_sp_imm16
	GLOBAL  and_bp_imm16
	GLOBAL  and_si_imm16
	GLOBAL  and_di_imm16

; ----- SUB -----

	modrm_1_help sub, sub_r0, imm16

	MACRO 
	sub_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	subs	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

sub_ax_imm16
	sub_reg16_imm16 eax
sub_cx_imm16
	sub_reg16_imm16 ecx
sub_dx_imm16
	sub_reg16_imm16 edx
sub_bx_imm16
	sub_reg16_imm16 ebx
sub_sp_imm16
	sub_reg16_imm16 esp
sub_bp_imm16
	sub_reg16_imm16 ebp
sub_si_imm16
	sub_reg16_imm16 esi
sub_di_imm16
	sub_reg16_imm16 edi

	GLOBAL  sub_ax_imm16
	GLOBAL  sub_cx_imm16
	GLOBAL  sub_dx_imm16
	GLOBAL  sub_bx_imm16
	GLOBAL  sub_sp_imm16
	GLOBAL  sub_bp_imm16
	GLOBAL  sub_si_imm16
	GLOBAL  sub_di_imm16

; ----- XOR -----

	modrm_1_help xor, xor_r0, imm16

	MACRO 
	xor_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	eors	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

xor_ax_imm16
	xor_reg16_imm16 eax
xor_cx_imm16
	xor_reg16_imm16 ecx
xor_dx_imm16
	xor_reg16_imm16 edx
xor_bx_imm16
	xor_reg16_imm16 ebx
xor_sp_imm16
	xor_reg16_imm16 esp
xor_bp_imm16
	xor_reg16_imm16 ebp
xor_si_imm16
	xor_reg16_imm16 esi
xor_di_imm16
	xor_reg16_imm16 edi

	GLOBAL  xor_ax_imm16
	GLOBAL  xor_cx_imm16
	GLOBAL  xor_dx_imm16
	GLOBAL  xor_bx_imm16
	GLOBAL  xor_sp_imm16
	GLOBAL  xor_bp_imm16
	GLOBAL  xor_si_imm16
	GLOBAL  xor_di_imm16

; ----- CMP -----

	modrm_1_help cmp, cmp_r0, imm16

	MACRO 
	cmp_reg16_imm16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r1, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	cmp		r2, r0, lsl #16			; Perform the operation using the high 16 bits
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

cmp_ax_imm16
	cmp_reg16_imm16 eax
cmp_cx_imm16
	cmp_reg16_imm16 ecx
cmp_dx_imm16
	cmp_reg16_imm16 edx
cmp_bx_imm16
	cmp_reg16_imm16 ebx
cmp_sp_imm16
	cmp_reg16_imm16 esp
cmp_bp_imm16
	cmp_reg16_imm16 ebp
cmp_si_imm16
	cmp_reg16_imm16 esi
cmp_di_imm16
	cmp_reg16_imm16 edi

	GLOBAL  cmp_ax_imm16
	GLOBAL  cmp_cx_imm16
	GLOBAL  cmp_dx_imm16
	GLOBAL  cmp_bx_imm16
	GLOBAL  cmp_sp_imm16
	GLOBAL  cmp_bp_imm16
	GLOBAL  cmp_si_imm16
	GLOBAL  cmp_di_imm16
	
; ------------------- 83 = ??? r/m16,+imm8 ----------------------------
; 
; All modrm bytes supported!
;
;
	GLOBAL	op_83
op_83
	modrm_jump_16_tbl op_83_jump
; 0 (idx only)
	modrm_help_1_0 add, simm8
	modrm_help_1_0 or, simm8
	modrm_help_1_0 adc, simm8
	modrm_help_1_0 sbb, simm8
	modrm_help_1_0 and, simm8
	modrm_help_1_0 sub, simm8
	modrm_help_1_0 xor, simm8
	modrm_help_1_0 cmp, simm8
;0x40 (idx+disp8)
	modrm_help_1_40 add, simm8
	modrm_help_1_40 or, simm8
	modrm_help_1_40 adc, simm8
	modrm_help_1_40 sbb, simm8
	modrm_help_1_40 and, simm8
	modrm_help_1_40 sub, simm8
	modrm_help_1_40 xor, simm8
	modrm_help_1_40 cmp, simm8
;0x80 (idx+disp16)
	modrm_help_1_80 add, simm8
	modrm_help_1_80 or, simm8
	modrm_help_1_80 adc, simm8
	modrm_help_1_80 sbb, simm8
	modrm_help_1_80 and, simm8
	modrm_help_1_80 sub, simm8
	modrm_help_1_80 xor, simm8
	modrm_help_1_80 cmp, simm8
;0xc0 = mod = 11b => two register operands
	DCD add_ax_simm8, add_cx_simm8, add_dx_simm8, add_bx_simm8, add_sp_simm8, add_bp_simm8, add_si_simm8, add_di_simm8
	DCD or_ax_simm8, or_cx_simm8, or_dx_simm8, or_bx_simm8, or_sp_simm8, or_bp_simm8, or_si_simm8, or_di_simm8
	DCD adc_ax_simm8, adc_cx_simm8, adc_dx_simm8, adc_bx_simm8, adc_sp_simm8, adc_bp_simm8, adc_si_simm8, adc_di_simm8
	DCD sbb_ax_simm8, sbb_cx_simm8, sbb_dx_simm8, sbb_bx_simm8, sbb_sp_simm8, sbb_bp_simm8, sbb_si_simm8, sbb_di_simm8
	DCD and_ax_simm8, and_cx_simm8, and_dx_simm8, and_bx_simm8, and_sp_simm8, and_bp_simm8, and_si_simm8, and_di_simm8
	DCD sub_ax_simm8, sub_cx_simm8, sub_dx_simm8, sub_bx_simm8, sub_sp_simm8, sub_bp_simm8, sub_si_simm8, sub_di_simm8
	DCD xor_ax_simm8, xor_cx_simm8, xor_dx_simm8, xor_bx_simm8, xor_sp_simm8, xor_bp_simm8, xor_si_simm8, xor_di_simm8
	DCD cmp_ax_simm8, cmp_cx_simm8, cmp_dx_simm8, cmp_bx_simm8, cmp_sp_simm8, cmp_bp_simm8, cmp_si_simm8, cmp_di_simm8

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	op83common $oper
	EXTERN	op_83_EGA_$oper
	GLOBAL	$oper_r0_bp_simm8
$oper_r0_bp_simm8
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	$oper_r0_simm8
$oper_r0_simm8
	;-------
	; Indexing by the current effective segment.
	;-------
	mem_handler_jump_r0r3 op_83_RAM_$oper, op_83_EGA_$oper, bad_MODEX_opcode_1
	;-------
	; The actual RAM handler must immediately follow this macro!
	;-------
	MEND

	;-------
	; Add: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common add
op_83_RAM_add
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	adds	r0, r1, lsl #16			; ADD the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Or: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common or
op_83_RAM_or
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	orrs	r0, r1					; OR the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Adc: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common adc
op_83_RAM_adc
	ldrb	r0, [r2] 				; Load byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	addcs	r0, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #16			; Perform the actual addition, setting the resulting flags.
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		loop

	;-------
	; Sbb: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common sbb
op_83_RAM_sbb
	ldrsb	r1, [r12],#1			; r1 = simm8 value
	ldrb	r0, [r2] 				; Load byte to r0
	ldrb	r3, [r2, #1]			; Load high byte to r1
	lsl		r1, #16
	orr		r0, r3, lsl #8
	bcs		%f1
	;-------
	; Perform the actual operation
	;-------
	rsbs	r0, r1, r0, lsl #16		; Perform the actual subtraction, setting the resulting flags.
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]					; Store low byte to [physical segment + disp16]
	lsr		r0, #8
	strb	r0,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	b		complement_carry
	;-------
	; Input carry set, calculate the flags separately
	;-------
1	sub		r3, r0, r1, lsr #16		; r3 = lf_var1d - lf_var2d
	sub		r3, #1					; r3 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r3, r3, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	lsr		r3, #16
	strb	r3,[r2]					; Store byte to RAM
	ror		r3, #8
	strb	r3,[r2, #1]				; Store high byte to [physical segment + disp16 + 1]
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, r1, lsr #16
	eor		r3, r0, r3, ror #24
	and		r1, r3
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0

	;-------
	; And: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common and
op_83_RAM_and
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	ands	r0, r1					; AND the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Sub: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common sub
op_83_RAM_sub
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	subs	r0, r1, lsl #16			; SUB the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		complement_carry

	;-------
	; Xor: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common xor
op_83_RAM_xor
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	lsl		r1, #16					; Shift it to the high halfword
	eors	r0, r1					; XOR the values, setting flags
	;-------
	; Save the result into [r2]
	;-------
	lsr		r0, #16
	strb	r0,[r2]
	lsr		r0, #8
	strb	r0,[r2, #1]
	b		loop

	;-------
	; Cmp: memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
	op83common cmp
op_83_RAM_cmp
	ldrb	r0, [r2] 				; Load low byte to r0
	ldrb	r1, [r2, #1]			; Load high byte to r1
	lsl		r0, #16
	orr		r0, r1, lsl #24
	;-------
	; Next load the simm8 value into r1.
	;-------
	ldrsb	r1,[r12],#1
	;-------
	; Perform the actual operation
	;-------
	cmp		r0, r1, lsl #16			; CMP the values, setting flags
	b		complement_carry

	ALIGN	4

	LTORG
	

; ----- ADD -----

	modrm_1_help add, add_r0, simm8
	
	MACRO 
	add_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	adds	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

add_ax_simm8
	add_reg16_simm8 eax
add_cx_simm8
	add_reg16_simm8 ecx
add_dx_simm8
	add_reg16_simm8 edx
add_bx_simm8
	add_reg16_simm8 ebx
add_sp_simm8
	add_reg16_simm8 esp
add_bp_simm8
	add_reg16_simm8 ebp
add_si_simm8
	add_reg16_simm8 esi
add_di_simm8
	add_reg16_simm8 edi

	GLOBAL  add_ax_simm8
	GLOBAL  add_cx_simm8
	GLOBAL  add_dx_simm8
	GLOBAL  add_bx_simm8
	GLOBAL  add_sp_simm8
	GLOBAL  add_bp_simm8
	GLOBAL  add_si_simm8
	GLOBAL  add_di_simm8

; ----- OR ------

	modrm_1_help or, or_r0, simm8

	MACRO 
	or_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	lsl		r0, #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	orrs	r0, r2, r0				; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

or_ax_simm8
	or_reg16_simm8 eax
or_cx_simm8
	or_reg16_simm8 ecx
or_dx_simm8
	or_reg16_simm8 edx
or_bx_simm8
	or_reg16_simm8 ebx
or_sp_simm8
	or_reg16_simm8 esp
or_bp_simm8
	or_reg16_simm8 ebp
or_si_simm8
	or_reg16_simm8 esi
or_di_simm8
	or_reg16_simm8 edi

	GLOBAL  or_ax_simm8
	GLOBAL  or_cx_simm8
	GLOBAL  or_dx_simm8
	GLOBAL  or_bx_simm8
	GLOBAL  or_sp_simm8
	GLOBAL  or_bp_simm8
	GLOBAL  or_si_simm8
	GLOBAL  or_di_simm8

; ----- ADC -----

	modrm_1_help adc, adc_r0, simm8

	MACRO 
	adc_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r1, $reg, lsl #16
	eor		$reg, r1, lsr #16
	addcs	r1, #0x00010000			; If input Carry is set, adjust the right operand so that ...
	subcs	r1, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, r0, lsl #16		; Perform the actual addition, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		loop
	MEND

adc_ax_simm8
	adc_reg16_simm8 eax
adc_cx_simm8
	adc_reg16_simm8 ecx
adc_dx_simm8
	adc_reg16_simm8 edx
adc_bx_simm8
	adc_reg16_simm8 ebx
adc_sp_simm8
	adc_reg16_simm8 esp
adc_bp_simm8
	adc_reg16_simm8 ebp
adc_si_simm8
	adc_reg16_simm8 esi
adc_di_simm8
	adc_reg16_simm8 edi

	GLOBAL  adc_ax_simm8
	GLOBAL  adc_cx_simm8
	GLOBAL  adc_dx_simm8
	GLOBAL  adc_bx_simm8
	GLOBAL  adc_sp_simm8
	GLOBAL  adc_bp_simm8
	GLOBAL  adc_si_simm8
	GLOBAL  adc_di_simm8

; ----- SBB -----

	modrm_1_help sbb, sbb_r0, simm8

	MACRO 
	sbb_reg16_simm8 $reg
	ldrsb	r1,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r0, $reg, lsl #16
	eor		$reg, r0, lsr #16
	bcs		%f1
	subs	r0, r1, lsl #16			; Perform the actual subtraction, setting the resulting flags.
	orr		$reg, r0, lsr #16
	b		complement_carry
	;-------
	; Input carry is set, so calculate flags separately.
	;-------
1	lsl		r1, #16					; Make r1 be a 16-bit value
	lsr		r1, #16
	rsb		r2, r1, r0, lsr #16		; r2 = lf_var1d - lf_var2d
	sub		r2, #1					; r2 = lf_var1d - lf_var2d - Carry
	;-------
	; Calculate Carry, Zero and Sign flags
	;-------
	movs	r2, r2, lsl #16			; Determine Carry, Zero and Sign flags
	;-------
	; Save the result
	;-------
	orr		$reg, r2, lsr #16
	;-------
	; Calculate Overflow flag: ((lf_var1d ^ lf_var2d) & (lf_var1d ^ lf_resd)) & 0x8000
	;-------
	eor		r1, r0, lsr #16
	eor		r2, r0
	and		r1, r2, lsr #16
	mrs		r0,cpsr					; Get Carry, Sign and Zero flags to r0
	tst		r1, #0x8000
	orrne	r0, #ARM_OVER
	biceq	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

sbb_ax_simm8
	sbb_reg16_simm8 eax
sbb_cx_simm8
	sbb_reg16_simm8 ecx
sbb_dx_simm8
	sbb_reg16_simm8 edx
sbb_bx_simm8
	sbb_reg16_simm8 ebx
sbb_sp_simm8
	sbb_reg16_simm8 esp
sbb_bp_simm8
	sbb_reg16_simm8 ebp
sbb_si_simm8
	sbb_reg16_simm8 esi
sbb_di_simm8
	sbb_reg16_simm8 edi

	GLOBAL  sbb_ax_simm8
	GLOBAL  sbb_cx_simm8
	GLOBAL  sbb_dx_simm8
	GLOBAL  sbb_bx_simm8
	GLOBAL  sbb_sp_simm8
	GLOBAL  sbb_bp_simm8
	GLOBAL  sbb_si_simm8
	GLOBAL  sbb_di_simm8

; ----- AND -----

	modrm_1_help and, and_r0, simm8
	
	MACRO 
	and_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	lsl		r0, #16					; We can not do the shift using a shifter in 'ands' as it affects the Carry flag
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	ands	r0, r2					; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

and_ax_simm8
	and_reg16_simm8 eax
and_cx_simm8
	and_reg16_simm8 ecx
and_dx_simm8
	and_reg16_simm8 edx
and_bx_simm8
	and_reg16_simm8 ebx
and_sp_simm8
	and_reg16_simm8 esp
and_bp_simm8
	and_reg16_simm8 ebp
and_si_simm8
	and_reg16_simm8 esi
and_di_simm8
	and_reg16_simm8 edi

	GLOBAL  and_ax_simm8
	GLOBAL  and_cx_simm8
	GLOBAL  and_dx_simm8
	GLOBAL  and_bx_simm8
	GLOBAL  and_sp_simm8
	GLOBAL  and_bp_simm8
	GLOBAL  and_si_simm8
	GLOBAL  and_di_simm8

; ----- SUB -----

	modrm_1_help sub, sub_r0, simm8

	MACRO 
	sub_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	subs	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		complement_carry
	MEND

sub_ax_simm8
	sub_reg16_simm8 eax
sub_cx_simm8
	sub_reg16_simm8 ecx
sub_dx_simm8
	sub_reg16_simm8 edx
sub_bx_simm8
	sub_reg16_simm8 ebx
sub_sp_simm8
	sub_reg16_simm8 esp
sub_bp_simm8
	sub_reg16_simm8 ebp
sub_si_simm8
	sub_reg16_simm8 esi
sub_di_simm8
	sub_reg16_simm8 edi

	GLOBAL  sub_ax_simm8
	GLOBAL  sub_cx_simm8
	GLOBAL  sub_dx_simm8
	GLOBAL  sub_bx_simm8
	GLOBAL  sub_sp_simm8
	GLOBAL  sub_bp_simm8
	GLOBAL  sub_si_simm8
	GLOBAL  sub_di_simm8

; ----- XOR -----

	modrm_1_help xor, xor_r0, simm8

	MACRO 
	xor_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially Overflow)
	mov		r2, $reg, lsl #16
	lsl		r0, #16					; We can not do the shift using a shifter in 'ands' as it affects the Carry flag
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	eors	r0, r2					; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

xor_ax_simm8
	xor_reg16_simm8 eax
xor_cx_simm8
	xor_reg16_simm8 ecx
xor_dx_simm8
	xor_reg16_simm8 edx
xor_bx_simm8
	xor_reg16_simm8 ebx
xor_sp_simm8
	xor_reg16_simm8 esp
xor_bp_simm8
	xor_reg16_simm8 ebp
xor_si_simm8
	xor_reg16_simm8 esi
xor_di_simm8
	xor_reg16_simm8 edi

	GLOBAL  xor_ax_simm8
	GLOBAL  xor_cx_simm8
	GLOBAL  xor_dx_simm8
	GLOBAL  xor_bx_simm8
	GLOBAL  xor_sp_simm8
	GLOBAL  xor_bp_simm8
	GLOBAL  xor_si_simm8
	GLOBAL  xor_di_simm8

; ----- CMP -----

	modrm_1_help cmp, cmp_r0, simm8

	MACRO 
	cmp_reg16_simm8 $reg
	ldrsb	r0,[r12],#1				; Load the simm8 byte to r0, increment r12 by 1
	mov		r2, $reg, lsl #16
	cmp		r2, r0, lsl #16			; Compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

cmp_ax_simm8
	cmp_reg16_simm8 eax
cmp_cx_simm8
	cmp_reg16_simm8 ecx
cmp_dx_simm8
	cmp_reg16_simm8 edx
cmp_bx_simm8
	cmp_reg16_simm8 ebx
cmp_sp_simm8
	cmp_reg16_simm8 esp
cmp_bp_simm8
	cmp_reg16_simm8 ebp
cmp_si_simm8
	cmp_reg16_simm8 esi
cmp_di_simm8
	cmp_reg16_simm8 edi

	GLOBAL  cmp_ax_simm8
	GLOBAL  cmp_cx_simm8
	GLOBAL  cmp_dx_simm8
	GLOBAL  cmp_bx_simm8
	GLOBAL  cmp_sp_simm8
	GLOBAL  cmp_bp_simm8
	GLOBAL  cmp_si_simm8
	GLOBAL  cmp_di_simm8

	ALIGN	4

; ------------------- 84 = TEST esp,r/m8 -------------------------------
;
; All modrm bytes supported!
;
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual tst operation, so C and O work like in x86.
;
	GLOBAL	op_84
op_84
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_84_jump
	modrm_tbl_0 test
	DCD test_al_al, test_al_cl, test_al_dl, test_al_bl, test_al_ah, test_al_ch, test_al_dh, test_al_bh
	DCD test_al_cl, test_cl_cl, test_cl_dl, test_cl_bl, test_cl_ah, test_cl_ch, test_cl_dh, test_cl_bh
	DCD test_al_dl, test_cl_dl, test_dl_dl, test_dl_bl, test_dl_ah, test_dl_ch, test_dl_dh, test_dl_bh
	DCD test_al_bl, test_cl_bl, test_dl_bl, test_bl_bl, test_bl_ah, test_bl_ch, test_bl_dh, test_bl_bh
	DCD test_al_ah, test_cl_ah, test_dl_ah, test_bl_ah, test_ah_ah, test_ah_ch, test_ah_dh, test_ah_bh
	DCD test_al_ch, test_cl_ch, test_dl_ch, test_bl_ch, test_ah_ch, test_ch_ch, test_ch_dh, test_ch_bh
	DCD test_al_dh, test_cl_dh, test_dl_dh, test_bl_dh, test_ah_dh, test_ch_dh, test_dh_dh, test_dh_bh
	DCD test_al_bh, test_cl_bh, test_dl_bh, test_bl_bh, test_ah_bh, test_ch_bh, test_dh_bh, test_bh_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	test_r0_reg8l $reg
	EXTERN	op_84_EGA_l_$reg
	EXTERN	op_84_MODEX_l_$reg
	GLOBAL	test_r0_r8l_bp_$reg
test_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	test_r0_r8l_$reg
test_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_84_RAM_l_$reg, op_84_EGA_l_$reg, op_84_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_84_RAM_l_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	mov		r1, $reg, lsl #24
	tst		r1, r0, lsl #24
	b		loop
	MEND
	MACRO 
	test_r0_reg8h $reg
	EXTERN	op_84_EGA_h_$reg
	EXTERN	op_84_MODEX_h_$reg
	GLOBAL	test_r0_r8h_bp_$reg
test_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	test_r0_r8h_$reg
test_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_84_RAM_h_$reg, op_84_EGA_h_$reg, op_84_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_84_RAM_h_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	and		r1, $reg, #0xFF00
	lsl		r0, #24
	tst		r0, r1, lsl #16
	b		loop
	MEND

	test_r0_reg8l eax
	test_r0_reg8l ecx
	test_r0_reg8l edx
	test_r0_reg8l ebx
	test_r0_reg8h eax
	test_r0_reg8h ecx
	test_r0_reg8h edx
	test_r0_reg8h ebx

	LTORG

	modrm_0_genall test


; --- registers ---

	MACRO 
	test_reg8l_reg8l $rl, $rr
	mov		r0, $rl, lsl #24
	mov		r1, $rr, lsl #24
	tst		r0, r1
	b		loop
	MEND
	
	MACRO
	test_reg8l_reg8h $rl, $rr
	mov		r0, $rl, lsl #24
	and		r1, $rr, #0xFF00
	tst		r0, r1, lsl #16
	b		loop
	MEND
	MACRO 
	test_reg8h_reg8h $rl, $rr
	and		r0, $rl, #0xFF00
	and		r1, $rr, #0xFF00
	lsl		r0, #16
	tst		r0, r1, lsl #16
	b		loop
	MEND

	GLOBAL test_al_al 
	GLOBAL test_cl_al 
	GLOBAL test_dl_al 
	GLOBAL test_bl_al 
	GLOBAL test_ah_al 
	GLOBAL test_ch_al 
	GLOBAL test_dh_al 
	GLOBAL test_bh_al
	GLOBAL test_al_cl 
	GLOBAL test_cl_cl 
	GLOBAL test_dl_cl 
	GLOBAL test_bl_cl 
	GLOBAL test_ah_cl 
	GLOBAL test_ch_cl 
	GLOBAL test_dh_cl 
	GLOBAL test_bh_cl
	GLOBAL test_al_dl 
	GLOBAL test_cl_dl 
	GLOBAL test_dl_dl 
	GLOBAL test_bl_dl 
	GLOBAL test_ah_dl 
	GLOBAL test_ch_dl 
	GLOBAL test_dh_dl 
	GLOBAL test_bh_dl
	GLOBAL test_al_bl 
	GLOBAL test_cl_bl 
	GLOBAL test_dl_bl 
	GLOBAL test_bl_bl 
	GLOBAL test_ah_bl 
	GLOBAL test_ch_bl 
	GLOBAL test_dh_bl 
	GLOBAL test_bh_bl
	GLOBAL test_al_ah 
	GLOBAL test_cl_ah 
	GLOBAL test_dl_ah 
	GLOBAL test_bl_ah 
	GLOBAL test_ah_ah 
	GLOBAL test_ch_ah 
	GLOBAL test_dh_ah 
	GLOBAL test_bh_ah
	GLOBAL test_al_ch 
	GLOBAL test_cl_ch 
	GLOBAL test_dl_ch 
	GLOBAL test_bl_ch 
	GLOBAL test_ah_ch 
	GLOBAL test_ch_ch 
	GLOBAL test_dh_ch 
	GLOBAL test_bh_ch
	GLOBAL test_al_dh 
	GLOBAL test_cl_dh 
	GLOBAL test_dl_dh 
	GLOBAL test_bl_dh 
	GLOBAL test_ah_dh 
	GLOBAL test_ch_dh 
	GLOBAL test_dh_dh 
	GLOBAL test_bh_dh
	GLOBAL test_al_bh 
	GLOBAL test_cl_bh 
	GLOBAL test_dl_bh 
	GLOBAL test_bl_bh 
	GLOBAL test_ah_bh 
	GLOBAL test_ch_bh 
	GLOBAL test_dh_bh 
	GLOBAL test_bh_bh

test_al_al
	test_reg8l_reg8l eax, eax
test_al_cl
test_cl_al
	test_reg8l_reg8l eax, ecx
test_al_dl
test_dl_al
	test_reg8l_reg8l eax, edx
test_al_bl
test_bl_al
	test_reg8l_reg8l eax, ebx
test_al_ah
test_ah_al
	test_reg8l_reg8h eax, eax
test_al_ch
test_ch_al
	test_reg8l_reg8h eax, ecx
test_al_dh
test_dh_al
	test_reg8l_reg8h eax, edx
test_al_bh
test_bh_al
	test_reg8l_reg8h eax, ebx

test_cl_cl
	test_reg8l_reg8l ecx, ecx
test_cl_dl
test_dl_cl
	test_reg8l_reg8l ecx, edx
test_cl_bl
test_bl_cl
	test_reg8l_reg8l ecx, ebx
test_cl_ah
test_ah_cl
	test_reg8l_reg8h ecx, eax
test_cl_ch
test_ch_cl
	test_reg8l_reg8h ecx, ecx
test_cl_dh
test_dh_cl
	test_reg8l_reg8h ecx, edx
test_cl_bh
test_bh_cl
	test_reg8l_reg8h ecx, ebx

test_dl_dl
	test_reg8l_reg8l edx, edx
test_dl_bl
test_bl_dl
	test_reg8l_reg8l edx, ebx
test_dl_ah
test_ah_dl
	test_reg8l_reg8h edx, eax
test_dl_ch
test_ch_dl
	test_reg8l_reg8h edx, ecx
test_dl_dh
test_dh_dl
	test_reg8l_reg8h edx, edx
test_dl_bh
test_bh_dl
	test_reg8l_reg8h edx, ebx

test_bl_bl
	test_reg8l_reg8l ebx, ebx
test_bl_ah
test_ah_bl
	test_reg8l_reg8h ebx, eax
test_bl_ch
test_ch_bl
	test_reg8l_reg8h ebx, ecx
test_bl_dh
test_dh_bl
	test_reg8l_reg8h ebx, edx
test_bl_bh
test_bh_bl
	test_reg8l_reg8h ebx, ebx

test_ah_ah
	test_reg8h_reg8h eax, eax
test_ah_ch
test_ch_ah
	test_reg8h_reg8h eax, ecx
test_ah_dh
test_dh_ah
	test_reg8h_reg8h eax, edx
test_ah_bh
test_bh_ah
	test_reg8h_reg8h eax, ebx

test_ch_ch
	test_reg8h_reg8h ecx, ecx
test_ch_dh
test_dh_ch
	test_reg8h_reg8h ecx, edx
test_ch_bh
test_bh_ch
	test_reg8h_reg8h ecx, ebx

test_dh_dh
	test_reg8h_reg8h edx, edx
test_dh_bh
test_bh_dh
	test_reg8h_reg8h edx, ebx

test_bh_bh
	test_reg8h_reg8h ebx, ebx
	
; ------------------- 85 = TEST r16,r/m16 -----------------------------
;
; All modrm variations supported!
;
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual tst operation, so C and O work like in x86.
;
op_85
	mov		r0, #0
	msr		cpsr_f, r0							; Clear all flags (especially C and O)
	modrm_jump_16_tbl op_85_jump
	modrm_tbl_1_old test
	DCD test_ax_ax, test_ax_cx, test_ax_dx, test_ax_bx, test_ax_sp, test_ax_bp, test_ax_si, test_ax_di
	DCD test_ax_cx, test_cx_cx, test_cx_dx, test_cx_bx, test_cx_sp, test_cx_bp, test_cx_si, test_cx_di
	DCD test_ax_dx, test_cx_dx, test_dx_dx, test_dx_bx, test_dx_sp, test_dx_bp, test_dx_si, test_dx_di
	DCD test_ax_bx, test_cx_bx, test_dx_bx, test_bx_bx, test_bx_sp, test_bx_bp, test_bx_si, test_bx_di
	DCD test_ax_sp, test_cx_sp, test_dx_sp, test_bx_sp, test_sp_sp, test_bp_sp, test_si_sp, test_di_sp
	DCD test_ax_bp, test_cx_bp, test_dx_bp, test_bx_bp, test_bp_sp, test_bp_bp, test_bp_si, test_bp_di
	DCD test_ax_si, test_cx_si, test_dx_si, test_bx_si, test_si_sp, test_bp_si, test_si_si, test_si_di
	DCD test_ax_di, test_cx_di, test_dx_di, test_bx_di, test_di_sp, test_bp_di, test_si_di, test_di_di

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	test_r0_reg16 $reg
	GLOBAL	test_r0_r16_bp_$reg
test_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	test_r0_r16_$reg
test_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_85_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_85_RAM_$reg
	ldrb	r0, [r2] 				; Load byte to r0
	ldrb	r1, [r2, #1]
	mov		r2, $reg, lsl #16
	orr		r0, r1, lsl #8
	tst		r2, r0, lsl #16
	b		loop
	MEND

	test_r0_reg16 eax
	test_r0_reg16 ecx
	test_r0_reg16 edx
	test_r0_reg16 ebx
	test_r0_reg16 esp
	test_r0_reg16 ebp
	test_r0_reg16 esi
	test_r0_reg16 edi

	LTORG

	modrm_1_genall test, test_r0_r16

; --- registers ---

	MACRO 
	test_reg16_reg16 $rl, $rr
	mov		r0, $rl, lsl #16
	mov		r1, $rr, lsl #16
	tst		r0, r1
	b		loop
	MEND
	
test_ax_ax
	test_reg16_reg16 eax, eax
test_cx_ax
test_ax_cx
	test_reg16_reg16 eax, ecx
test_dx_ax
test_ax_dx
	test_reg16_reg16 eax, edx
test_bx_ax
test_ax_bx
	test_reg16_reg16 eax, ebx
test_sp_ax
test_ax_sp
	test_reg16_reg16 eax, esp
test_bp_ax
test_ax_bp
	test_reg16_reg16 eax, ebp
test_si_ax
test_ax_si
	test_reg16_reg16 eax, esi
test_di_ax
test_ax_di
	test_reg16_reg16 eax, edi
test_cx_cx
	test_reg16_reg16 ecx, ecx
test_dx_cx
test_cx_dx
	test_reg16_reg16 ecx, edx
test_bx_cx
test_cx_bx
	test_reg16_reg16 ecx, ebx
test_sp_cx
test_cx_sp
	test_reg16_reg16 ecx, esp
test_bp_cx
test_cx_bp
	test_reg16_reg16 ecx, ebp
test_si_cx
test_cx_si
	test_reg16_reg16 ecx, esi
test_di_cx
test_cx_di
	test_reg16_reg16 ecx, edi
test_dx_dx
	test_reg16_reg16 edx, edx
test_bx_dx
test_dx_bx
	test_reg16_reg16 edx, ebx
test_sp_dx
test_dx_sp
	test_reg16_reg16 edx, esp
test_bp_dx
test_dx_bp
	test_reg16_reg16 edx, ebp
test_si_dx
test_dx_si
	test_reg16_reg16 edx, esi
test_di_dx
test_dx_di
	test_reg16_reg16 edx, edi
test_bx_bx
	test_reg16_reg16 ebx, ebx
test_sp_bx
test_bx_sp
	test_reg16_reg16 ebx, esp
test_bp_bx
test_bx_bp
	test_reg16_reg16 ebx, ebp
test_si_bx
test_bx_si
	test_reg16_reg16 ebx, esi
test_di_bx
test_bx_di
	test_reg16_reg16 ebx, edi
test_sp_sp
	test_reg16_reg16 esp, esp
test_sp_bp
test_bp_sp
	test_reg16_reg16 ebp, esp
test_bp_bp
	test_reg16_reg16 ebp, ebp
test_si_bp
test_bp_si
	test_reg16_reg16 ebp, esi
test_di_bp
test_bp_di
	test_reg16_reg16 ebp, edi
test_sp_si
test_si_sp
	test_reg16_reg16 esi, esp
test_si_si
	test_reg16_reg16 esi, esi
test_di_si
test_si_di
	test_reg16_reg16 esi, edi
test_sp_di
test_di_sp
	test_reg16_reg16 edi, esp
test_di_di
	test_reg16_reg16 edi, edi

	GLOBAL  test_ax_ax
	GLOBAL  test_cx_ax
	GLOBAL  test_dx_ax
	GLOBAL  test_bx_ax
	GLOBAL  test_sp_ax
	GLOBAL  test_bp_ax
	GLOBAL  test_si_ax
	GLOBAL  test_di_ax
	GLOBAL  test_ax_cx
	GLOBAL  test_cx_cx 
	GLOBAL  test_dx_cx 
	GLOBAL  test_bx_cx 
	GLOBAL  test_sp_cx 
	GLOBAL  test_bp_cx 
	GLOBAL  test_si_cx 
	GLOBAL  test_di_cx
	GLOBAL  test_ax_dx 
	GLOBAL  test_cx_dx 
	GLOBAL  test_dx_dx 
	GLOBAL  test_bx_dx 
	GLOBAL  test_sp_dx 
	GLOBAL  test_bp_dx 
	GLOBAL  test_si_dx 
	GLOBAL  test_di_dx
	GLOBAL  test_ax_bx 
	GLOBAL  test_cx_bx 
	GLOBAL  test_dx_bx 
	GLOBAL  test_bx_bx 
	GLOBAL  test_sp_bx 
	GLOBAL  test_bp_bx 
	GLOBAL  test_si_bx 
	GLOBAL  test_di_bx
	GLOBAL  test_ax_sp 
	GLOBAL  test_cx_sp 
	GLOBAL  test_dx_sp 
	GLOBAL  test_bx_sp 
	GLOBAL  test_sp_sp 
	GLOBAL  test_bp_sp 
	GLOBAL  test_si_sp 
	GLOBAL  test_di_sp
	GLOBAL  test_ax_bp 
	GLOBAL  test_cx_bp 
	GLOBAL  test_dx_bp 
	GLOBAL  test_bx_bp 
	GLOBAL  test_sp_bp 
	GLOBAL  test_bp_bp 
	GLOBAL  test_si_bp 
	GLOBAL  test_di_bp
	GLOBAL  test_ax_si 
	GLOBAL  test_cx_si 
	GLOBAL  test_dx_si 
	GLOBAL  test_bx_si 
	GLOBAL  test_sp_si 
	GLOBAL  test_bp_si 
	GLOBAL  test_si_si 
	GLOBAL  test_di_si
	GLOBAL  test_ax_di 
	GLOBAL  test_cx_di 
	GLOBAL  test_dx_di 
	GLOBAL  test_bx_di 
	GLOBAL  test_sp_di 
	GLOBAL  test_bp_di 
	GLOBAL  test_si_di 
	GLOBAL  test_di_di

; ------------------- 86 = XCHG r/m8,r8 -----------------------------
;
; All modrm bytes supported!
;
;
	GLOBAL	op_86
op_86
	modrm_jump_16_tbl op_86_jump
	modrm_tbl_0 xchg
	DCD loop, xchg_al_cl, xchg_al_dl, xchg_al_bl, xchg_al_ah, xchg_al_ch, xchg_al_dh, xchg_al_bh
	DCD xchg_al_cl, loop, xchg_cl_dl, xchg_cl_bl, xchg_cl_ah, xchg_cl_ch, xchg_cl_dh, xchg_cl_bh
	DCD xchg_al_dl, xchg_cl_dl, loop, xchg_dl_bl, xchg_dl_ah, xchg_dl_ch, xchg_dl_dh, xchg_dl_bh
	DCD xchg_al_bl, xchg_cl_bl, xchg_dl_bl, loop, xchg_bl_ah, xchg_bl_ch, xchg_bl_dh, xchg_bl_bh
	DCD xchg_al_ah, xchg_cl_ah, xchg_dl_ah, xchg_bl_ah, loop, xchg_ah_ch, xchg_ah_dh, xchg_ah_bh
	DCD xchg_al_ch, xchg_cl_ch, xchg_dl_ch, xchg_bl_ch, xchg_ah_ch, loop, xchg_ch_dh, xchg_ch_bh
	DCD xchg_al_dh, xchg_cl_dh, xchg_dl_dh, xchg_bl_dh, xchg_ah_dh, xchg_ch_dh, loop, xchg_dh_bh
	DCD xchg_al_bh, xchg_cl_bh, xchg_dl_bh, xchg_bl_bh, xchg_ah_bh, xchg_ch_bh, xchg_dh_bh, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xchg_r0_r8l_reg $reg
	EXTERN	op_86_EGA_l_$reg
	EXTERN	op_86_MODEX_l_$reg
	GLOBAL	xchg_r0_r8l_bp_$reg
xchg_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xchg_r0_r8l_$reg
xchg_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_86_RAM_l_$reg, op_86_EGA_l_$reg, op_86_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_86_RAM_l_$reg
	ldrb	r0, [r2]
	strb	$reg, [r2]
	bfi		$reg, r0, #0, #8
	b		loop
	MEND
	MACRO 
	xchg_r0_r8h_reg $reg
	EXTERN	op_86_EGA_h_$reg
	EXTERN	op_86_MODEX_h_$reg
	GLOBAL	xchg_r0_r8h_bp_$reg
xchg_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xchg_r0_r8h_$reg
xchg_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_86_RAM_h_$reg, op_86_EGA_h_$reg, op_86_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_86_RAM_h_$reg
	mov		r1, $reg, lsr #8
	ldrb	r0, [r2]
	strb	r1, [r2]
	bfi		$reg, r0, #8, #8
	b		loop
	MEND

	xchg_r0_r8l_reg eax
	xchg_r0_r8l_reg ecx
	xchg_r0_r8l_reg edx
	xchg_r0_r8l_reg ebx
	xchg_r0_r8h_reg eax
	xchg_r0_r8h_reg ecx
	xchg_r0_r8h_reg edx
	xchg_r0_r8h_reg ebx

	LTORG

	modrm_0_genall xchg

; --- registers ---

	MACRO 
	xchg_reg8l_reg8l $reg1, $reg2
	mov		r0, $reg1
	bfi		$reg1, $reg2, #0, #8
	bfi		$reg2, r0, #0, #8
	b		loop
	MEND

	MACRO 
	xchg_reg8l_reg8h $reg1, $reg2
	and		r0, $reg1, #0xFF
	ubfx	r1, $reg2, #8, #8
	bfi		$reg1, r1, #0, #8
	bfi		$reg2, r0, #8, #8
	b		loop
	MEND

	MACRO 
	xchg_reg8h_reg8h $reg1, $reg2
	ubfx	r0, $reg1, #8, #8
	ubfx	r1, $reg2, #8, #8
	bfi		$reg1, r1, #8, #8
	bfi		$reg2, r0, #8, #8
	b		loop
	MEND

xchg_al_cl
xchg_cl_al
	xchg_reg8l_reg8l eax, ecx
xchg_al_dl
xchg_dl_al
	xchg_reg8l_reg8l eax, edx
xchg_al_bl
xchg_bl_al
	xchg_reg8l_reg8l eax, ebx
xchg_al_ah
xchg_ah_al
	xchg_reg8l_reg8h eax, eax
xchg_al_ch
xchg_ch_al
	xchg_reg8l_reg8h eax, ecx
xchg_al_dh
xchg_dh_al
	xchg_reg8l_reg8h eax, edx
xchg_al_bh
xchg_bh_al
	xchg_reg8l_reg8h eax, ebx

xchg_cl_dl
xchg_dl_cl
	xchg_reg8l_reg8l ecx, edx
xchg_cl_bl
xchg_bl_cl
	xchg_reg8l_reg8l ecx, ebx
xchg_cl_ah
xchg_ah_cl
	xchg_reg8l_reg8h ecx, eax
xchg_cl_ch
xchg_ch_cl
	xchg_reg8l_reg8h ecx, ecx
xchg_cl_dh
xchg_dh_cl
	xchg_reg8l_reg8h ecx, edx
xchg_cl_bh
xchg_bh_cl
	xchg_reg8l_reg8h ecx, ebx

xchg_dl_bl
xchg_bl_dl
	xchg_reg8l_reg8l edx, ebx
xchg_dl_ah
xchg_ah_dl
	xchg_reg8l_reg8h edx, eax
xchg_dl_ch
xchg_ch_dl
	xchg_reg8l_reg8h edx, ecx
xchg_dl_dh
xchg_dh_dl
	xchg_reg8l_reg8h edx, edx
xchg_dl_bh
xchg_bh_dl
	xchg_reg8l_reg8h edx, ebx

xchg_bl_ah
xchg_ah_bl
	xchg_reg8l_reg8h ebx, eax
xchg_bl_ch
xchg_ch_bl
	xchg_reg8l_reg8h ebx, ecx
xchg_bl_dh
xchg_dh_bl
	xchg_reg8l_reg8h ebx, edx
xchg_bl_bh
xchg_bh_bl
	xchg_reg8l_reg8h ebx, ebx

xchg_ah_ch
xchg_ch_ah
	xchg_reg8h_reg8h eax, ecx
xchg_ah_dh
xchg_dh_ah
	xchg_reg8h_reg8h eax, edx
xchg_ah_bh
xchg_bh_ah
	xchg_reg8h_reg8h eax, ebx

xchg_ch_dh
xchg_dh_ch
	xchg_reg8h_reg8h ecx, edx
xchg_ch_bh
xchg_bh_ch
	xchg_reg8h_reg8h ecx, ebx

xchg_dh_bh
xchg_bh_dh
	xchg_reg8h_reg8h edx, ebx

	GLOBAL xchg_cl_al 
	GLOBAL xchg_dl_al 
	GLOBAL xchg_bl_al 
	GLOBAL xchg_ah_al 
	GLOBAL xchg_ch_al 
	GLOBAL xchg_dh_al 
	GLOBAL xchg_bh_al
	GLOBAL xchg_al_cl 
	GLOBAL xchg_dl_cl 
	GLOBAL xchg_bl_cl 
	GLOBAL xchg_ah_cl 
	GLOBAL xchg_ch_cl 
	GLOBAL xchg_dh_cl 
	GLOBAL xchg_bh_cl
	GLOBAL xchg_al_dl 
	GLOBAL xchg_cl_dl 
	GLOBAL xchg_bl_dl 
	GLOBAL xchg_ah_dl 
	GLOBAL xchg_ch_dl 
	GLOBAL xchg_dh_dl 
	GLOBAL xchg_bh_dl
	GLOBAL xchg_al_bl 
	GLOBAL xchg_cl_bl 
	GLOBAL xchg_dl_bl 
	GLOBAL xchg_ah_bl 
	GLOBAL xchg_ch_bl 
	GLOBAL xchg_dh_bl 
	GLOBAL xchg_bh_bl
	GLOBAL xchg_al_ah 
	GLOBAL xchg_cl_ah 
	GLOBAL xchg_dl_ah 
	GLOBAL xchg_bl_ah 
	GLOBAL xchg_ch_ah 
	GLOBAL xchg_dh_ah 
	GLOBAL xchg_bh_ah
	GLOBAL xchg_al_ch 
	GLOBAL xchg_cl_ch 
	GLOBAL xchg_dl_ch 
	GLOBAL xchg_bl_ch 
	GLOBAL xchg_ah_ch 
	GLOBAL xchg_dh_ch 
	GLOBAL xchg_bh_ch
	GLOBAL xchg_al_dh 
	GLOBAL xchg_cl_dh 
	GLOBAL xchg_dl_dh 
	GLOBAL xchg_bl_dh 
	GLOBAL xchg_ah_dh 
	GLOBAL xchg_ch_dh 
	GLOBAL xchg_bh_dh
	GLOBAL xchg_al_bh 
	GLOBAL xchg_cl_bh 
	GLOBAL xchg_dl_bh 
	GLOBAL xchg_bl_bh 
	GLOBAL xchg_ah_bh 
	GLOBAL xchg_ch_bh 
	GLOBAL xchg_dh_bh 


; ------------------- 87 = XCHG r16,r/m16 -----------------------------
;
; All modrm bytes supported!
;
op_87
	modrm_jump_16_tbl op_87_jump
	modrm_tbl_1_old xchg
	DCD loop, xchg_ax_cx, xchg_ax_dx, xchg_ax_bx, xchg_ax_sp, xchg_ax_bp, xchg_ax_si, xchg_ax_di
	DCD xchg_ax_cx, loop, xchg_cx_dx, xchg_cx_bx, xchg_cx_sp, xchg_cx_bp, xchg_cx_si, xchg_cx_di
	DCD xchg_ax_dx, xchg_cx_dx, loop, xchg_dx_bx, xchg_dx_sp, xchg_dx_bp, xchg_dx_si, xchg_dx_di
	DCD xchg_ax_bx, xchg_cx_bx, xchg_dx_bx, loop, xchg_bx_sp, xchg_bx_bp, xchg_bx_si, xchg_bx_di
	DCD xchg_ax_sp, xchg_cx_sp, xchg_dx_sp, xchg_bx_sp, loop, xchg_sp_bp, xchg_sp_si, xchg_sp_di
	DCD xchg_ax_bp, xchg_cx_bp, xchg_dx_bp, xchg_bx_bp, xchg_sp_bp, loop, xchg_bp_si, xchg_bp_di
	DCD xchg_ax_si, xchg_cx_si, xchg_dx_si, xchg_bx_si, xchg_sp_si, xchg_bp_si, loop, xchg_si_di
	DCD xchg_ax_di, xchg_cx_di, xchg_dx_di, xchg_bx_di, xchg_sp_di, xchg_bp_di, xchg_si_di, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	xchg_reg16_r0high $reg
	EXTERN	op_87_EGA_$reg
	GLOBAL	xchg_r0_r16_bp_$reg
xchg_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	xchg_r0_r16_$reg
xchg_r0_r16_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_87_RAM_$reg, op_87_EGA_$reg, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_87_RAM_$reg
	ldrb	r0, [r2]
	strb	$reg, [r2]
	bfi		$reg, r0, #0, #8
	mov		r0, $reg, lsr #8
	ldrb	r1, [r2, #1]
	strb	r0, [r2, #1]
	bfi		$reg, r1, #8, #8
	b		loop
	MEND

	xchg_reg16_r0high eax
	xchg_reg16_r0high ecx
	xchg_reg16_r0high edx
	xchg_reg16_r0high ebx
	xchg_reg16_r0high esp
	xchg_reg16_r0high ebp
	xchg_reg16_r0high esi
	xchg_reg16_r0high edi

	LTORG

	modrm_1_genall xchg, xchg_r0_r16

; --- registers ---
	
	MACRO 
	xchg_reg16_reg16 $rl, $rr
	mov		r0, $rl
	bfi		$rl, $rr, #0, #16
	bfi		$rr, r0, #0, #16
	b		loop
	MEND
	
xchg_ax_cx
xchg_cx_ax
	xchg_reg16_reg16 eax, ecx
xchg_dx_ax
xchg_ax_dx
	xchg_reg16_reg16 eax, edx
xchg_bx_ax
xchg_ax_bx
	xchg_reg16_reg16 eax, ebx
xchg_sp_ax
xchg_ax_sp
	xchg_reg16_reg16 eax, esp
xchg_bp_ax
xchg_ax_bp
	xchg_reg16_reg16 eax, ebp
xchg_si_ax
xchg_ax_si
	xchg_reg16_reg16 eax, esi
xchg_di_ax
xchg_ax_di
	xchg_reg16_reg16 eax, edi
xchg_dx_cx
xchg_cx_dx
	xchg_reg16_reg16 ecx, edx
xchg_bx_cx
xchg_cx_bx
	xchg_reg16_reg16 ecx, ebx
xchg_sp_cx
xchg_cx_sp
	xchg_reg16_reg16 ecx, esp
xchg_bp_cx
xchg_cx_bp
	xchg_reg16_reg16 ecx, ebp
xchg_si_cx
xchg_cx_si
	xchg_reg16_reg16 ecx, esi
xchg_di_cx
xchg_cx_di
	xchg_reg16_reg16 ecx, edi
xchg_bx_dx
xchg_dx_bx
	xchg_reg16_reg16 edx, ebx
xchg_sp_dx
xchg_dx_sp
	xchg_reg16_reg16 edx, esp
xchg_bp_dx
xchg_dx_bp
	xchg_reg16_reg16 edx, ebp
xchg_si_dx
xchg_dx_si
	xchg_reg16_reg16 edx, esi
xchg_di_dx
xchg_dx_di
	xchg_reg16_reg16 edx, edi
xchg_sp_bx
xchg_bx_sp
	xchg_reg16_reg16 ebx, esp
xchg_bp_bx
xchg_bx_bp
	xchg_reg16_reg16 ebx, ebp
xchg_si_bx
xchg_bx_si
	xchg_reg16_reg16 ebx, esi
xchg_di_bx
xchg_bx_di
	xchg_reg16_reg16 ebx, edi
xchg_bp_sp
xchg_sp_bp
	xchg_reg16_reg16 ebp, esp
xchg_si_bp
xchg_bp_si
	xchg_reg16_reg16 ebp, esi
xchg_di_bp
xchg_bp_di
	xchg_reg16_reg16 ebp, edi
xchg_si_sp
xchg_sp_si
	xchg_reg16_reg16 esi, esp
xchg_di_si
xchg_si_di
	xchg_reg16_reg16 esi, edi
xchg_di_sp
xchg_sp_di
	xchg_reg16_reg16 edi, esp

	GLOBAL  xchg_cx_ax
	GLOBAL  xchg_dx_ax
	GLOBAL  xchg_bx_ax
	GLOBAL  xchg_sp_ax
	GLOBAL  xchg_bp_ax
	GLOBAL  xchg_si_ax
	GLOBAL  xchg_di_ax
	GLOBAL  xchg_ax_cx
	GLOBAL  xchg_dx_cx 
	GLOBAL  xchg_bx_cx 
	GLOBAL  xchg_sp_cx 
	GLOBAL  xchg_bp_cx 
	GLOBAL  xchg_si_cx 
	GLOBAL  xchg_di_cx
	GLOBAL  xchg_ax_dx 
	GLOBAL  xchg_cx_dx 
	GLOBAL  xchg_bx_dx 
	GLOBAL  xchg_sp_dx 
	GLOBAL  xchg_bp_dx 
	GLOBAL  xchg_si_dx 
	GLOBAL  xchg_di_dx
	GLOBAL  xchg_ax_bx 
	GLOBAL  xchg_cx_bx 
	GLOBAL  xchg_dx_bx 
	GLOBAL  xchg_sp_bx 
	GLOBAL  xchg_bp_bx 
	GLOBAL  xchg_si_bx 
	GLOBAL  xchg_di_bx
	GLOBAL  xchg_ax_sp 
	GLOBAL  xchg_cx_sp 
	GLOBAL  xchg_dx_sp 
	GLOBAL  xchg_bx_sp 
	GLOBAL  xchg_bp_sp 
	GLOBAL  xchg_si_sp 
	GLOBAL  xchg_di_sp
	GLOBAL  xchg_ax_bp 
	GLOBAL  xchg_cx_bp 
	GLOBAL  xchg_dx_bp 
	GLOBAL  xchg_bx_bp 
	GLOBAL  xchg_sp_bp 
	GLOBAL  xchg_si_bp 
	GLOBAL  xchg_di_bp
	GLOBAL  xchg_ax_si 
	GLOBAL  xchg_cx_si 
	GLOBAL  xchg_dx_si 
	GLOBAL  xchg_bx_si 
	GLOBAL  xchg_sp_si 
	GLOBAL  xchg_bp_si 
	GLOBAL  xchg_di_si
	GLOBAL  xchg_ax_di 
	GLOBAL  xchg_cx_di 
	GLOBAL  xchg_dx_di 
	GLOBAL  xchg_bx_di 
	GLOBAL  xchg_sp_di 
	GLOBAL  xchg_bp_di 
	GLOBAL  xchg_si_di 

; ------------------- 88 = MOV r/m8,r8 -------------------------------
;
; All modrm bytes supported!
;
;
	GLOBAL	op_88
op_88
	modrm_jump_16_tbl op_88_jump
	modrm_tbl_0	mov
	DCD loop, mov_cl_al, mov_dl_al, mov_bl_al, mov_ah_al, mov_ch_al, mov_dh_al, mov_bh_al
	DCD mov_al_cl, loop, mov_dl_cl, mov_bl_cl, mov_ah_cl, mov_ch_cl, mov_dh_cl, mov_bh_cl
	DCD mov_al_dl, mov_cl_dl, loop, mov_bl_dl, mov_ah_dl, mov_ch_dl, mov_dh_dl, mov_bh_dl
	DCD mov_al_bl, mov_cl_bl, mov_dl_bl, loop, mov_ah_bl, mov_ch_bl, mov_dh_bl, mov_bh_bl
	DCD mov_al_ah, mov_cl_ah, mov_dl_ah, mov_bl_ah, loop, mov_ch_ah, mov_dh_ah, mov_bh_ah
	DCD mov_al_ch, mov_cl_ch, mov_dl_ch, mov_bl_ch, mov_ah_ch, loop, mov_dh_ch, mov_bh_ch
	DCD mov_al_dh, mov_cl_dh, mov_dl_dh, mov_bl_dh, mov_ah_dh, mov_ch_dh, loop, mov_bh_dh
	DCD mov_al_bh, mov_cl_bh, mov_dl_bh, mov_bl_bh, mov_ah_bh, mov_ch_bh, mov_dh_bh, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	MACRO 
	mov_r0_r8l_reg $reg
	EXTERN	op_88_EGA_l_$reg
	EXTERN	op_88_MODEX_l_$reg
	GLOBAL	mov_r0_r8l_bp_$reg
mov_r0_r8l_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_r8l_$reg
mov_r0_r8l_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_88_RAM_l_$reg, op_88_EGA_l_$reg, op_88_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_88_RAM_l_$reg
	strb	$reg,[r2]							; Store the value to [physical segment + (unsigned)(idx)]
	b		loop
	MEND
	MACRO 
	mov_r0_r8h_reg $reg
	EXTERN	op_88_EGA_h_$reg
	EXTERN	op_88_MODEX_h_$reg
	GLOBAL	mov_r0_r8h_bp_$reg
mov_r0_r8h_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_r8h_$reg
mov_r0_r8h_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_88_RAM_h_$reg, op_88_EGA_h_$reg, op_88_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_88_RAM_h_$reg
	mov		r1, $reg, lsr #8		; Put high byte of register to low byte of r1
	strb	r1,[r2]					; Store the value to [physical segment + (unsigned)(idx)]
	b		loop
	MEND

	mov_r0_r8l_reg eax
	mov_r0_r8l_reg ecx
	mov_r0_r8l_reg edx
	mov_r0_r8l_reg ebx
	mov_r0_r8h_reg eax
	mov_r0_r8h_reg ecx
	mov_r0_r8h_reg edx
	mov_r0_r8h_reg ebx

	LTORG

	modrm_0_genall mov

	GLOBAL	op_a2
op_a2											; "mov [1234],al" == A2 34 12 == 88 06 34 12
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r0_r8l_eax

; ------------------- 89 = MOV r/m16,r16 ------------------------------
; 
; All modrm variations supported!
;
;
	GLOBAL	op_89
op_89
	modrm_jump_16_tbl op_89_jump
	modrm_tbl_1_old mov
	DCD loop, mov_cx_ax, mov_dx_ax, mov_bx_ax, mov_sp_ax, mov_bp_ax, mov_si_ax, mov_di_ax
	DCD mov_ax_cx, loop, mov_dx_cx, mov_bx_cx, mov_sp_cx, mov_bp_cx, mov_si_cx, mov_di_cx
	DCD mov_ax_dx, mov_cx_dx, loop, mov_bx_dx, mov_sp_dx, mov_bp_dx, mov_si_dx, mov_di_dx
	DCD mov_ax_bx, mov_cx_bx, mov_dx_bx, loop, mov_sp_bx, mov_bp_bx, mov_si_bx, mov_di_bx
	DCD mov_ax_sp, mov_cx_sp, mov_dx_sp, mov_bx_sp, loop, mov_bp_sp, mov_si_sp, mov_di_sp
	DCD mov_ax_bp, mov_cx_bp, mov_dx_bp, mov_bx_bp, mov_sp_bp, loop, mov_si_bp, mov_di_bp
	DCD mov_ax_si, mov_cx_si, mov_dx_si, mov_bx_si, mov_sp_si, mov_bp_si, loop, mov_di_si
	DCD mov_ax_di, mov_cx_di, mov_dx_di, mov_bx_di, mov_sp_di, mov_bp_di, mov_si_di, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	mov_r0_reg16 $reg
	EXTERN	op_89_EGA_$reg
	EXTERN	op_89_MODEX_$reg
	GLOBAL	mov_r0_r16_bp_$reg
mov_r0_r16_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_r16_$reg
mov_r0_r16_$reg
	mem_handler_jump_r0r3 %f1, op_89_EGA_$reg, op_89_MODEX_$reg
1	strb	$reg, [r2]
	mov		r1, $reg, lsr #8
	strb	r1, [r2, #1]
	b		loop
	MEND

	mov_r0_reg16 eax
	mov_r0_reg16 ecx
	mov_r0_reg16 edx
	mov_r0_reg16 ebx
	mov_r0_reg16 esp
	mov_r0_reg16 ebp
	mov_r0_reg16 esi
	mov_r0_reg16 edi

	modrm_1_genall mov, mov_r0_r16

	GLOBAL	op_a3
op_a3											; "mov [1234],ax" == A3 34 12 == 89 06 34 12
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r0_r16_eax


; ------------------- 8A = MOV esp,r/m8 --------------------------------
; 
; All modrm bytes supported!
;
;
; This is the second most common opcode during Wing Commander II space flight!
;
	GLOBAL	op_8a
op_8a
	modrm_jump_16_tbl op_8a_jump
	modrm_tbl_2 mov
	DCD loop, mov_al_cl, mov_al_dl, mov_al_bl, mov_al_ah, mov_al_ch, mov_al_dh, mov_al_bh
	DCD mov_cl_al, loop, mov_cl_dl, mov_cl_bl, mov_cl_ah, mov_cl_ch, mov_cl_dh, mov_cl_bh
	DCD mov_dl_al, mov_dl_cl, loop, mov_dl_bl, mov_dl_ah, mov_dl_ch, mov_dl_dh, mov_dl_bh
	DCD mov_bl_al, mov_bl_cl, mov_bl_dl, loop, mov_bl_ah, mov_bl_ch, mov_bl_dh, mov_bl_bh
	DCD mov_ah_al, mov_ah_cl, mov_ah_dl, mov_ah_bl, loop, mov_ah_ch, mov_ah_dh, mov_ah_bh
	DCD mov_ch_al, mov_ch_cl, mov_ch_dl, mov_ch_bl, mov_ch_ah, loop, mov_ch_dh, mov_ch_bh
	DCD mov_dh_al, mov_dh_cl, mov_dh_dl, mov_dh_bl, mov_dh_ah, mov_dh_ch, loop, mov_dh_bh
	DCD mov_bh_al, mov_bh_cl, mov_bh_dl, mov_bh_bl, mov_bh_ah, mov_bh_ch, mov_bh_dh, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	mov_reg8l_r0high $reg
	EXTERN	op_8a_EGA_l_$reg
	EXTERN	op_8a_MODEX_l_$reg
	GLOBAL	mov_r8l_r0_bp_$reg
mov_r8l_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r8l_r0_$reg
mov_r8l_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8a_RAM_l_$reg, op_8a_EGA_l_$reg, op_8a_MODEX_l_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8a_RAM_l_$reg
	ldrb	r0,[r2]					; Get the byte from RAM
	bic		$reg, #0xFF
	orr		$reg, r0
	b		loop
	MEND
	MACRO 
	mov_reg8h_r0high $reg
	EXTERN	op_8a_EGA_h_$reg
	EXTERN	op_8a_MODEX_h_$reg
	GLOBAL	mov_r8h_r0_bp_$reg
mov_r8h_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r8h_r0_$reg
mov_r8h_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8a_RAM_h_$reg, op_8a_EGA_h_$reg, op_8a_MODEX_h_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8a_RAM_h_$reg
	ldrb	r0,[r2]					; Get the byte from RAM
	bic		$reg, #0xFF00
	orr		$reg, r0, lsl #8
	b		loop
	MEND

	mov_reg8l_r0high eax
	mov_reg8l_r0high ecx
	mov_reg8l_r0high edx
	mov_reg8l_r0high ebx
	mov_reg8h_r0high eax
	mov_reg8h_r0high ecx
	mov_reg8h_r0high edx
	mov_reg8h_r0high ebx

	modrm_2_genall mov

	GLOBAL	op_a0
op_a0									; "mov al,[1234]" == A0 34 12 == 8A 06 34 12
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r8l_r0_eax
	
	ALIGN	4
	

; --- Register operands ---

	MACRO 
	mov_reg8l_reg8l $rto, $rfr
	bfi		$rto, $rfr, #0, #8
	b		loop
	MEND

	MACRO 
	mov_reg8l_reg8h $rto, $rfr
	ubfx	r0, $rfr, #8, #8
	bfi		$rto, r0, #0, #8
	b		loop
	MEND

	MACRO 
	mov_reg8h_reg8l $rto, $rfr
	bfi		$rto, $rfr, #8, #8
	b		loop
	MEND

	MACRO 
	mov_reg8h_reg8h $rto, $rfr
	ubfx	r0, $rfr, #8, #8
	bfi		$rto, r0, #8, #8
	b		loop
	MEND

mov_al_cl
	mov_reg8l_reg8l eax, ecx
mov_al_dl
	mov_reg8l_reg8l eax, edx
mov_al_bl
	mov_reg8l_reg8l eax, ebx
mov_al_ah
	mov_reg8l_reg8h eax, eax
mov_al_ch
	mov_reg8l_reg8h eax, ecx
mov_al_dh
	mov_reg8l_reg8h eax, edx
mov_al_bh
	mov_reg8l_reg8h eax, ebx
	
mov_cl_al
	mov_reg8l_reg8l ecx, eax
mov_cl_dl
	mov_reg8l_reg8l ecx, edx
mov_cl_bl
	mov_reg8l_reg8l ecx, ebx
mov_cl_ah
	mov_reg8l_reg8h ecx, eax
mov_cl_ch
	mov_reg8l_reg8h ecx, ecx
mov_cl_dh
	mov_reg8l_reg8h ecx, edx
mov_cl_bh
	mov_reg8l_reg8h ecx, ebx
	
mov_dl_al
	mov_reg8l_reg8l edx, eax
mov_dl_cl
	mov_reg8l_reg8l edx, ecx
mov_dl_bl
	mov_reg8l_reg8l edx, ebx
mov_dl_ah
	mov_reg8l_reg8h edx, eax
mov_dl_ch
	mov_reg8l_reg8h edx, ecx
mov_dl_dh
	mov_reg8l_reg8h edx, edx
mov_dl_bh
	mov_reg8l_reg8h edx, ebx
	
mov_bl_al
	mov_reg8l_reg8l ebx, eax
mov_bl_cl
	mov_reg8l_reg8l ebx, ecx
mov_bl_dl
	mov_reg8l_reg8l ebx, edx
mov_bl_ah
	mov_reg8l_reg8h ebx, eax
mov_bl_ch
	mov_reg8l_reg8h ebx, ecx
mov_bl_dh
	mov_reg8l_reg8h ebx, edx
mov_bl_bh
	mov_reg8l_reg8h ebx, ebx

mov_ah_al
	mov_reg8h_reg8l eax, eax
mov_ah_cl
	mov_reg8h_reg8l eax, ecx
mov_ah_dl
	mov_reg8h_reg8l eax, edx
mov_ah_bl
	mov_reg8h_reg8l eax, ebx
mov_ah_ch
	mov_reg8h_reg8h eax, ecx
mov_ah_dh
	mov_reg8h_reg8h eax, edx
mov_ah_bh
	mov_reg8h_reg8h eax, ebx

mov_ch_al
	mov_reg8h_reg8l ecx, eax
mov_ch_cl
	mov_reg8h_reg8l ecx, ecx
mov_ch_dl
	mov_reg8h_reg8l ecx, edx
mov_ch_bl
	mov_reg8h_reg8l ecx, ebx
mov_ch_ah
	mov_reg8h_reg8h ecx, eax
mov_ch_dh
	mov_reg8h_reg8h ecx, edx
mov_ch_bh
	mov_reg8h_reg8h ecx, ebx

mov_dh_al
	mov_reg8h_reg8l edx, eax
mov_dh_cl
	mov_reg8h_reg8l edx, ecx
mov_dh_dl
	mov_reg8h_reg8l edx, edx
mov_dh_bl
	mov_reg8h_reg8l edx, ebx
mov_dh_ah
	mov_reg8h_reg8h edx, eax
mov_dh_ch
	mov_reg8h_reg8h edx, ecx
mov_dh_bh
	mov_reg8h_reg8h edx, ebx

mov_bh_al
	mov_reg8h_reg8l ebx, eax
mov_bh_cl
	mov_reg8h_reg8l ebx, ecx
mov_bh_dl
	mov_reg8h_reg8l ebx, edx
mov_bh_bl
	mov_reg8h_reg8l ebx, ebx
mov_bh_ah
	mov_reg8h_reg8h ebx, eax
mov_bh_ch
	mov_reg8h_reg8h ebx, ecx
mov_bh_dh
	mov_reg8h_reg8h ebx, edx

	GLOBAL mov_cl_al
	GLOBAL mov_dl_al
	GLOBAL mov_bl_al
	GLOBAL mov_ah_al
	GLOBAL mov_ch_al
	GLOBAL mov_dh_al
	GLOBAL mov_bh_al
	GLOBAL mov_al_cl
	GLOBAL mov_dl_cl
	GLOBAL mov_bl_cl
	GLOBAL mov_ah_cl
	GLOBAL mov_ch_cl
	GLOBAL mov_dh_cl
	GLOBAL mov_bh_cl
	GLOBAL mov_al_dl
	GLOBAL mov_cl_dl
	GLOBAL mov_bl_dl
	GLOBAL mov_ah_dl
	GLOBAL mov_ch_dl
	GLOBAL mov_dh_dl
	GLOBAL mov_bh_dl
	GLOBAL mov_al_bl
	GLOBAL mov_cl_bl
	GLOBAL mov_dl_bl
	GLOBAL mov_ah_bl
	GLOBAL mov_ch_bl
	GLOBAL mov_dh_bl
	GLOBAL mov_bh_bl
	GLOBAL mov_al_ah
	GLOBAL mov_cl_ah
	GLOBAL mov_dl_ah
	GLOBAL mov_bl_ah
	GLOBAL mov_ch_ah
	GLOBAL mov_dh_ah
	GLOBAL mov_bh_ah
	GLOBAL mov_al_ch
	GLOBAL mov_cl_ch
	GLOBAL mov_dl_ch
	GLOBAL mov_bl_ch
	GLOBAL mov_ah_ch
	GLOBAL mov_dh_ch
	GLOBAL mov_bh_ch
	GLOBAL mov_al_dh
	GLOBAL mov_cl_dh
	GLOBAL mov_dl_dh
	GLOBAL mov_bl_dh
	GLOBAL mov_ah_dh
	GLOBAL mov_ch_dh
	GLOBAL mov_bh_dh
	GLOBAL mov_al_bh
	GLOBAL mov_cl_bh
	GLOBAL mov_dl_bh
	GLOBAL mov_bl_bh
	GLOBAL mov_ah_bh
	GLOBAL mov_ch_bh
	GLOBAL mov_dh_bh


	ALIGN	4
	
; ------------------- 8B = MOV r16,r/m16 -------------------------------
; 
; All modrm bytes supported!
; We must not change the flags here!!!
; This is the most often used opcode in Wing Commander 2!
;
	GLOBAL	op_8b
op_8b
	modrm_jump_16_tbl op_8b_jump
	modrm_tbl_3_old mov
	DCD loop, mov_ax_cx, mov_ax_dx, mov_ax_bx, mov_ax_sp, mov_ax_bp, mov_ax_si, mov_ax_di
	DCD mov_cx_ax, loop, mov_cx_dx, mov_cx_bx, mov_cx_sp, mov_cx_bp, mov_cx_si, mov_cx_di
	DCD mov_dx_ax, mov_dx_cx, loop, mov_dx_bx, mov_dx_sp, mov_dx_bp, mov_dx_si, mov_dx_di
	DCD mov_bx_ax, mov_bx_cx, mov_bx_dx, loop, mov_bx_sp, mov_bx_bp, mov_bx_si, mov_bx_di
	DCD mov_sp_ax, mov_sp_cx, mov_sp_dx, mov_sp_bx, loop, mov_sp_bp, mov_sp_si, mov_sp_di
	DCD mov_bp_ax, mov_bp_cx, mov_bp_dx, mov_bp_bx, mov_bp_sp, loop, mov_bp_si, mov_bp_di
	DCD mov_si_ax, mov_si_cx, mov_si_dx, mov_si_bx, mov_si_sp, mov_si_bp, loop, mov_si_di
	DCD mov_di_ax, mov_di_cx, mov_di_dx, mov_di_bx, mov_di_sp, mov_di_bp, mov_di_si, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	mov_reg16_r0 $reg
	EXTERN	op_8b_EGA_$reg
	EXTERN	op_8b_MODEX_$reg
	GLOBAL	mov_r16_r0_bp_$reg
mov_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r16_r0_$reg
mov_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, op_8b_EGA_$reg, op_8b_MODEX_$reg
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrb	r0,[r2]					; Get the byte from RAM
	ldrb	r1,[r2, #1]				; Get the byte from RAM
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	b		loop
	MEND

	mov_reg16_r0 eax
	mov_reg16_r0 ecx
	mov_reg16_r0 edx
	mov_reg16_r0 ebx
	mov_reg16_r0 esp
	mov_reg16_r0 ebp
	mov_reg16_r0 esi
	mov_reg16_r0 edi

	modrm_3_genall_old mov, mov_r16_r0

	GLOBAL	op_a1
op_a1
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	r0_from_disp16
	b		mov_r16_r0_eax

; --- registers ---

	MACRO 
	mov_reg16_reg16 $rl, $rr
	bfi		$rl, $rr, #0, #16
	b		loop
	MEND

mov_ax_cx
	mov_reg16_reg16		eax, ecx
mov_ax_dx
	mov_reg16_reg16		eax, edx
mov_ax_bx
	mov_reg16_reg16		eax, ebx
mov_ax_sp
	mov_reg16_reg16		eax, esp
mov_ax_bp
	mov_reg16_reg16		eax, ebp
mov_ax_si
	mov_reg16_reg16		eax, esi
mov_ax_di
	mov_reg16_reg16		eax, edi
mov_cx_ax
	mov_reg16_reg16		ecx, eax
mov_cx_dx
	mov_reg16_reg16		ecx, edx
mov_cx_bx
	mov_reg16_reg16		ecx, ebx
mov_cx_sp
	mov_reg16_reg16		ecx, esp
mov_cx_bp
	mov_reg16_reg16		ecx, ebp
mov_cx_si
	mov_reg16_reg16		ecx, esi
mov_cx_di
	mov_reg16_reg16		ecx, edi
mov_dx_ax
	mov_reg16_reg16		edx, eax
mov_dx_cx
	mov_reg16_reg16		edx, ecx
mov_dx_bx
	mov_reg16_reg16		edx, ebx
mov_dx_sp
	mov_reg16_reg16		edx, esp
mov_dx_bp
	mov_reg16_reg16		edx, ebp
mov_dx_si
	mov_reg16_reg16		edx, esi
mov_dx_di
	mov_reg16_reg16		edx, edi
mov_bx_ax
	mov_reg16_reg16		ebx, eax
mov_bx_cx
	mov_reg16_reg16		ebx, ecx
mov_bx_dx
	mov_reg16_reg16		ebx, edx
mov_bx_sp
	mov_reg16_reg16		ebx, esp
mov_bx_bp
	mov_reg16_reg16		ebx, ebp
mov_bx_si
	mov_reg16_reg16		ebx, esi
mov_bx_di
	mov_reg16_reg16		ebx, edi
mov_sp_ax
	mov_reg16_reg16		esp, eax
mov_sp_cx
	mov_reg16_reg16		esp, ecx
mov_sp_dx
	mov_reg16_reg16		esp, edx
mov_sp_bx
	mov_reg16_reg16		esp, ebx
mov_sp_bp
	mov_reg16_reg16		esp, ebp
mov_sp_si
	mov_reg16_reg16		esp, esi
mov_sp_di
	mov_reg16_reg16		esp, edi
mov_bp_ax
	mov_reg16_reg16		ebp, eax
mov_bp_cx
	mov_reg16_reg16		ebp, ecx
mov_bp_dx
	mov_reg16_reg16		ebp, edx
mov_bp_bx
	mov_reg16_reg16		ebp, ebx
mov_bp_sp
	mov_reg16_reg16		ebp, esp
mov_bp_si
	mov_reg16_reg16		ebp, esi
mov_bp_di
	mov_reg16_reg16		ebp, edi
mov_si_ax
	mov_reg16_reg16		esi, eax
mov_si_cx
	mov_reg16_reg16		esi, ecx
mov_si_dx
	mov_reg16_reg16		esi, edx
mov_si_bx
	mov_reg16_reg16		esi, ebx
mov_si_sp
	mov_reg16_reg16		esi, esp
mov_si_bp
	mov_reg16_reg16		esi, ebp
mov_si_di
	mov_reg16_reg16		esi, edi
mov_di_ax
	mov_reg16_reg16		edi, eax
mov_di_cx
	mov_reg16_reg16		edi, ecx
mov_di_dx
	mov_reg16_reg16		edi, edx
mov_di_bx
	mov_reg16_reg16		edi, ebx
mov_di_sp
	mov_reg16_reg16		edi, esp
mov_di_bp
	mov_reg16_reg16		edi, ebp
mov_di_si
	mov_reg16_reg16		edi, esi

	GLOBAL  mov_cx_ax
	GLOBAL  mov_dx_ax
	GLOBAL  mov_bx_ax
	GLOBAL  mov_sp_ax
	GLOBAL  mov_bp_ax
	GLOBAL  mov_si_ax
	GLOBAL  mov_di_ax
	GLOBAL  mov_ax_cx
	GLOBAL  mov_dx_cx 
	GLOBAL  mov_bx_cx 
	GLOBAL  mov_sp_cx 
	GLOBAL  mov_bp_cx 
	GLOBAL  mov_si_cx 
	GLOBAL  mov_di_cx
	GLOBAL  mov_ax_dx 
	GLOBAL  mov_cx_dx 
	GLOBAL  mov_bx_dx 
	GLOBAL  mov_sp_dx 
	GLOBAL  mov_bp_dx 
	GLOBAL  mov_si_dx 
	GLOBAL  mov_di_dx
	GLOBAL  mov_ax_bx 
	GLOBAL  mov_cx_bx 
	GLOBAL  mov_dx_bx 
	GLOBAL  mov_sp_bx 
	GLOBAL  mov_bp_bx 
	GLOBAL  mov_si_bx 
	GLOBAL  mov_di_bx
	GLOBAL  mov_ax_sp 
	GLOBAL  mov_cx_sp 
	GLOBAL  mov_dx_sp 
	GLOBAL  mov_bx_sp 
	GLOBAL  mov_bp_sp 
	GLOBAL  mov_si_sp 
	GLOBAL  mov_di_sp
	GLOBAL  mov_ax_bp 
	GLOBAL  mov_cx_bp 
	GLOBAL  mov_dx_bp 
	GLOBAL  mov_bx_bp 
	GLOBAL  mov_sp_bp 
	GLOBAL  mov_si_bp 
	GLOBAL  mov_di_bp
	GLOBAL  mov_ax_si 
	GLOBAL  mov_cx_si 
	GLOBAL  mov_dx_si 
	GLOBAL  mov_bx_si 
	GLOBAL  mov_sp_si 
	GLOBAL  mov_bp_si 
	GLOBAL  mov_di_si
	GLOBAL  mov_ax_di 
	GLOBAL  mov_cx_di 
	GLOBAL  mov_dx_di 
	GLOBAL  mov_bx_di 
	GLOBAL  mov_sp_di 
	GLOBAL  mov_bp_di 
	GLOBAL  mov_si_di 


; ------------------- 8C = MOV r/m16,Sreg -----------------------------
;
; All modrm variations supported!
;
; We must not change the flags here!!!
;
op_8c
	modrm_jump_16_tbl op_8c_jump
	modrm_help_1_0 mov, es
	modrm_help_1_0 mov, cs
	modrm_help_1_0 mov, ss
	modrm_help_1_0 mov, ds
	modrm_help_1_0 mov, fs
	modrm_help_1_0 mov, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0x40
	modrm_help_1_40 mov, es
	modrm_help_1_40 mov, cs
	modrm_help_1_40 mov, ss
	modrm_help_1_40 mov, ds
	modrm_help_1_40 mov, fs
	modrm_help_1_40 mov, gs
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
; 0xC0 = mod = 11b = register
	DCD mov_ax_es, mov_cx_es, mov_dx_es, mov_bx_es, mov_sp_es, mov_bp_es, mov_si_es, mov_di_es
	DCD mov_ax_cs, mov_cx_cs, mov_dx_cs, mov_bx_cs, mov_sp_cs, mov_bp_cs, mov_si_cs, mov_di_cs
	DCD mov_ax_ss, mov_cx_ss, mov_dx_ss, mov_bx_ss, mov_sp_ss, mov_bp_ss, mov_si_ss, mov_di_ss
	DCD mov_ax_ds, mov_cx_ds, mov_dx_ds, mov_bx_ds, mov_sp_ds, mov_bp_ds, mov_si_ds, mov_di_ds
	DCD mov_ax_fs, mov_cx_fs, mov_dx_fs, mov_bx_fs, mov_sp_fs, mov_bp_fs, mov_si_fs, mov_di_fs
	DCD mov_ax_gs, mov_cx_gs, mov_dx_gs, mov_bx_gs, mov_sp_gs, mov_bp_gs, mov_si_gs, mov_di_gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	GLOBAL	mov_r0_bp_es
mov_r0_bp_es
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_es
mov_r0_es
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_es, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_es
	ldr		r1, [sp, #SP_ES_VALUE]	; r1 = Current logical ES
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop

	GLOBAL	mov_r0_bp_cs
mov_r0_bp_cs
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_cs
mov_r0_cs
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_cs, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_cs
	ldr		r1, [sp, #SP_CS_VALUE]	; r1 = Current logical CS
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop

	GLOBAL	mov_r0_bp_ss
mov_r0_bp_ss
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_ss
mov_r0_ss
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_ss, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_ss
	ldr		r1, [sp, #SP_SS_VALUE]
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop

	GLOBAL	mov_r0_bp_ds
mov_r0_bp_ds
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_ds
mov_r0_ds
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_ds, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_ds
	ldr		r1, [sp, #SP_DS_VALUE]	
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop

	GLOBAL	mov_r0_bp_fs
mov_r0_bp_fs
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_fs
mov_r0_fs
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_fs, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_fs
	ldr		r1, [sp, #SP_FS_VALUE]	
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop

	GLOBAL	mov_r0_bp_gs
mov_r0_bp_gs
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_gs
mov_r0_gs
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8c_RAM_gs, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8c_RAM_gs
	ldr		r1, [sp, #SP_GS_VALUE]	
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop


; --- ES ---

	modrm_1_help mov, mov_r0, es

    GLOBAL  mov_bxsi_es
    GLOBAL  mov_bxdi_es
    GLOBAL  mov_bpsi_es
    GLOBAL  mov_bpdi_es
    GLOBAL  mov_siidx_es
    GLOBAL  mov_diidx_es
    GLOBAL  mov_disp16_es
    GLOBAL  mov_bxidx_es
    GLOBAL  mov_bxsid8_es
    GLOBAL  mov_bxdid8_es
    GLOBAL  mov_bpsid8_es
    GLOBAL  mov_bpdid8_es
    GLOBAL  mov_sidisp8_es
    GLOBAL  mov_didisp8_es
    GLOBAL  mov_bpdisp8_es
    GLOBAL  mov_bxdisp8_es
    GLOBAL  mov_bxsid16_es
    GLOBAL  mov_bxdid16_es
    GLOBAL  mov_bpsid16_es
    GLOBAL  mov_bpdid16_es
    GLOBAL  mov_sidisp16_es
    GLOBAL  mov_didisp16_es
    GLOBAL  mov_bpdisp16_es
    GLOBAL  mov_bxdisp16_es

	MACRO 
	mov_reg_es $reg
	ldr		r0, [sp, #SP_ES_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

mov_ax_es
	mov_reg_es eax
mov_cx_es
	mov_reg_es ecx
mov_dx_es
	mov_reg_es edx
mov_bx_es
	mov_reg_es ebx
mov_sp_es
	mov_reg_es	esp
mov_bp_es
	mov_reg_es ebp
mov_si_es
	mov_reg_es esi
mov_di_es
	mov_reg_es edi

	GLOBAL  mov_ax_es 
	GLOBAL  mov_cx_es 
	GLOBAL  mov_dx_es 
	GLOBAL  mov_bx_es 
	GLOBAL  mov_sp_es 
	GLOBAL  mov_bp_es 
	GLOBAL  mov_si_es 
	GLOBAL  mov_di_es

; --- CS ---

	modrm_1_help mov, mov_r0, cs

    GLOBAL  mov_bxsi_cs
    GLOBAL  mov_bxdi_cs
    GLOBAL  mov_bpsi_cs
    GLOBAL  mov_bpdi_cs
    GLOBAL  mov_siidx_cs
    GLOBAL  mov_diidx_cs
    GLOBAL  mov_disp16_cs
    GLOBAL  mov_bxidx_cs
    GLOBAL  mov_bxsid8_cs
    GLOBAL  mov_bxdid8_cs
    GLOBAL  mov_bpsid8_cs
    GLOBAL  mov_bpdid8_cs
    GLOBAL  mov_sidisp8_cs
    GLOBAL  mov_didisp8_cs
    GLOBAL  mov_bpdisp8_cs
    GLOBAL  mov_bxdisp8_cs
    GLOBAL  mov_bxsid16_cs
    GLOBAL  mov_bxdid16_cs
    GLOBAL  mov_bpsid16_cs
    GLOBAL  mov_bpdid16_cs
    GLOBAL  mov_sidisp16_cs
    GLOBAL  mov_didisp16_cs
    GLOBAL  mov_bpdisp16_cs
    GLOBAL  mov_bxdisp16_cs

	MACRO 
	mov_reg_cs $reg
	ldr		r0, [sp, #SP_CS_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND
	
mov_ax_cs
	mov_reg_cs	eax
mov_cx_cs
	mov_reg_cs	ecx
mov_dx_cs
	mov_reg_cs	edx
mov_bx_cs
	mov_reg_cs	ebx
mov_sp_cs
	mov_reg_cs	esp
mov_bp_cs
	mov_reg_cs	ebp
mov_si_cs
	mov_reg_cs	esi
mov_di_cs
	mov_reg_cs	edi

	GLOBAL  mov_ax_cs 
	GLOBAL  mov_cx_cs 
	GLOBAL  mov_dx_cs 
	GLOBAL  mov_bx_cs 
	GLOBAL  mov_sp_cs 
	GLOBAL  mov_bp_cs 
	GLOBAL  mov_si_cs 
	GLOBAL  mov_di_cs

; --- SS ---

	modrm_1_help mov, mov_r0, ss

    GLOBAL  mov_bxsi_ss
    GLOBAL  mov_bxdi_ss
    GLOBAL  mov_bpsi_ss
    GLOBAL  mov_bpdi_ss
    GLOBAL  mov_siidx_ss
    GLOBAL  mov_diidx_ss
    GLOBAL  mov_disp16_ss
    GLOBAL  mov_bxidx_ss
    GLOBAL  mov_bxsid8_ss
    GLOBAL  mov_bxdid8_ss
    GLOBAL  mov_bpsid8_ss
    GLOBAL  mov_bpdid8_ss
    GLOBAL  mov_sidisp8_ss
    GLOBAL  mov_didisp8_ss
    GLOBAL  mov_bpdisp8_ss
    GLOBAL  mov_bxdisp8_ss
    GLOBAL  mov_bxsid16_ss
    GLOBAL  mov_bxdid16_ss
    GLOBAL  mov_bpsid16_ss
    GLOBAL  mov_bpdid16_ss
    GLOBAL  mov_sidisp16_ss
    GLOBAL  mov_didisp16_ss
    GLOBAL  mov_bpdisp16_ss
    GLOBAL  mov_bxdisp16_ss

	MACRO 
	mov_reg_ss $reg
	ldr		r0, [sp, #SP_SS_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

mov_ax_ss
	mov_reg_ss 	eax
mov_cx_ss
	mov_reg_ss	ecx
mov_dx_ss
	mov_reg_ss	edx
mov_bx_ss
	mov_reg_ss	ebx
mov_sp_ss
	mov_reg_ss	esp
mov_bp_ss
	mov_reg_ss	ebp
mov_si_ss
	mov_reg_ss	esi
mov_di_ss
	mov_reg_ss	edi

	GLOBAL  mov_ax_ss 
	GLOBAL  mov_cx_ss 
	GLOBAL  mov_dx_ss 
	GLOBAL  mov_bx_ss 
	GLOBAL  mov_sp_ss 
	GLOBAL  mov_bp_ss 
	GLOBAL  mov_si_ss 
	GLOBAL  mov_di_ss

; --- DS ---

	modrm_1_help mov, mov_r0, ds

    GLOBAL  mov_bxsi_ds
    GLOBAL  mov_bxdi_ds
    GLOBAL  mov_bpsi_ds
    GLOBAL  mov_bpdi_ds
    GLOBAL  mov_siidx_ds
    GLOBAL  mov_diidx_ds
    GLOBAL  mov_disp16_ds
    GLOBAL  mov_bxidx_ds
    GLOBAL  mov_bxsid8_ds
    GLOBAL  mov_bxdid8_ds
    GLOBAL  mov_bpsid8_ds
    GLOBAL  mov_bpdid8_ds
    GLOBAL  mov_sidisp8_ds
    GLOBAL  mov_didisp8_ds
    GLOBAL  mov_bpdisp8_ds
    GLOBAL  mov_bxdisp8_ds
    GLOBAL  mov_bxsid16_ds
    GLOBAL  mov_bxdid16_ds
    GLOBAL  mov_bpsid16_ds
    GLOBAL  mov_bpdid16_ds
    GLOBAL  mov_sidisp16_ds
    GLOBAL  mov_didisp16_ds
    GLOBAL  mov_bpdisp16_ds
    GLOBAL  mov_bxdisp16_ds

	MACRO 
	mov_reg_ds $reg
	ldr		r0, [sp, #SP_DS_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

mov_ax_ds
	mov_reg_ds	eax
mov_cx_ds
	mov_reg_ds	ecx
mov_dx_ds
	mov_reg_ds	edx
mov_bx_ds
	mov_reg_ds	ebx
mov_sp_ds
	mov_reg_ds	esp
mov_bp_ds
	mov_reg_ds	ebp
mov_si_ds
	mov_reg_ds	esi
mov_di_ds
	mov_reg_ds	edi

	GLOBAL  mov_ax_ds 
	GLOBAL  mov_cx_ds 
	GLOBAL  mov_dx_ds 
	GLOBAL  mov_bx_ds 
	GLOBAL  mov_sp_ds 
	GLOBAL  mov_bp_ds 
	GLOBAL  mov_si_ds 
	GLOBAL  mov_di_ds

; --- FS ---

	modrm_1_help mov, mov_r0, fs

    GLOBAL  mov_bxsi_fs
    GLOBAL  mov_bxdi_fs
    GLOBAL  mov_bpsi_fs
    GLOBAL  mov_bpdi_fs
    GLOBAL  mov_siidx_fs
    GLOBAL  mov_diidx_fs
    GLOBAL  mov_disp16_fs
    GLOBAL  mov_bxidx_fs
    GLOBAL  mov_bxsid8_fs
    GLOBAL  mov_bxdid8_fs
    GLOBAL  mov_bpsid8_fs
    GLOBAL  mov_bpdid8_fs
    GLOBAL  mov_sidisp8_fs
    GLOBAL  mov_didisp8_fs
    GLOBAL  mov_bpdisp8_fs
    GLOBAL  mov_bxdisp8_fs
    GLOBAL  mov_bxsid16_fs
    GLOBAL  mov_bxdid16_fs
    GLOBAL  mov_bpsid16_fs
    GLOBAL  mov_bpdid16_fs
    GLOBAL  mov_sidisp16_fs
    GLOBAL  mov_didisp16_fs
    GLOBAL  mov_bpdisp16_fs
    GLOBAL  mov_bxdisp16_fs

	MACRO 
	mov_reg_fs $reg
	ldr		r0, [sp, #SP_FS_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

mov_ax_fs
	mov_reg_fs	eax
mov_cx_fs
	mov_reg_fs	ecx
mov_dx_fs
	mov_reg_fs	edx
mov_bx_fs
	mov_reg_fs	ebx
mov_sp_fs
	mov_reg_fs	esp
mov_bp_fs
	mov_reg_fs	ebp
mov_si_fs
	mov_reg_fs	esi
mov_di_fs
	mov_reg_fs	edi

	GLOBAL  mov_ax_fs 
	GLOBAL  mov_cx_fs 
	GLOBAL  mov_dx_fs 
	GLOBAL  mov_bx_fs 
	GLOBAL  mov_sp_fs 
	GLOBAL  mov_bp_fs 
	GLOBAL  mov_si_fs 
	GLOBAL  mov_di_fs

; --- GS ---

	modrm_1_help mov, mov_r0, gs

    GLOBAL  mov_bxsi_gs
    GLOBAL  mov_bxdi_gs
    GLOBAL  mov_bpsi_gs
    GLOBAL  mov_bpdi_gs
    GLOBAL  mov_siidx_gs
    GLOBAL  mov_diidx_gs
    GLOBAL  mov_disp16_gs
    GLOBAL  mov_bxidx_gs
    GLOBAL  mov_bxsid8_gs
    GLOBAL  mov_bxdid8_gs
    GLOBAL  mov_bpsid8_gs
    GLOBAL  mov_bpdid8_gs
    GLOBAL  mov_sidisp8_gs
    GLOBAL  mov_didisp8_gs
    GLOBAL  mov_bpdisp8_gs
    GLOBAL  mov_bxdisp8_gs
    GLOBAL  mov_bxsid16_gs
    GLOBAL  mov_bxdid16_gs
    GLOBAL  mov_bpsid16_gs
    GLOBAL  mov_bpdid16_gs
    GLOBAL  mov_sidisp16_gs
    GLOBAL  mov_didisp16_gs
    GLOBAL  mov_bpdisp16_gs
    GLOBAL  mov_bxdisp16_gs

	MACRO 
	mov_reg_gs $reg
	ldr		r0, [sp, #SP_GS_VALUE]
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

mov_ax_gs
	mov_reg_gs	eax
mov_cx_gs
	mov_reg_gs	ecx
mov_dx_gs
	mov_reg_gs	edx
mov_bx_gs
	mov_reg_gs	ebx
mov_sp_gs
	mov_reg_gs	esp
mov_bp_gs
	mov_reg_gs	ebp
mov_si_gs
	mov_reg_gs	esi
mov_di_gs
	mov_reg_gs	edi

	GLOBAL  mov_ax_gs 
	GLOBAL  mov_cx_gs 
	GLOBAL  mov_dx_gs 
	GLOBAL  mov_bx_gs 
	GLOBAL  mov_sp_gs 
	GLOBAL  mov_bp_gs 
	GLOBAL  mov_si_gs 
	GLOBAL  mov_di_gs

	LTORG								; Dump the current literal pool here


; ------------------- 8D = LEA r16,m ----------------------------------
;
; All modrm variations supported!
;
op_8d
	modrm_jump_16_tbl op_8d_jump
; 0
	DCD lea_ax_bxsi, lea_ax_bxdi, lea_ax_bpsi, lea_ax_bpdi, mov_ax_si, mov_ax_di, lea_ax_disp16, mov_ax_bx
	DCD lea_cx_bxsi, lea_cx_bxdi, lea_cx_bpsi, lea_cx_bpdi, mov_cx_si, mov_cx_di, lea_cx_disp16, mov_cx_bx
	DCD lea_dx_bxsi, lea_dx_bxdi, lea_dx_bpsi, lea_dx_bpdi, mov_dx_si, mov_dx_di, lea_dx_disp16, mov_dx_bx
	DCD lea_bx_bxsi, lea_bx_bxdi, lea_bx_bpsi, lea_bx_bpdi, mov_bx_si, mov_bx_di, lea_bx_disp16, loop
	DCD lea_sp_bxsi, lea_sp_bxdi, lea_sp_bpsi, lea_sp_bpdi, mov_sp_si, mov_sp_di, lea_sp_disp16, mov_sp_bx
	DCD lea_bp_bxsi, lea_bp_bxdi, lea_bp_bpsi, lea_bp_bpdi, mov_bp_si, mov_bp_di, lea_bp_disp16, mov_bp_bx
	DCD lea_si_bxsi, lea_si_bxdi, lea_si_bpsi, lea_si_bpdi, loop, mov_si_di, lea_si_disp16, mov_si_bx
	DCD lea_di_bxsi, lea_di_bxdi, lea_di_bpsi, lea_di_bpdi, mov_di_si, loop, lea_di_disp16, mov_di_bx
; 0x40
	modrm_help_3_40 lea, ax
	modrm_help_3_40 lea, cx
	modrm_help_3_40 lea, dx
	modrm_help_3_40 lea, bx
	modrm_help_3_40 lea, "sp"
	modrm_help_3_40 lea, bp
	modrm_help_3_40 lea, si
	modrm_help_3_40 lea, di
; 0x80
	modrm_help_3_80 lea, ax
	modrm_help_3_80 lea, cx
	modrm_help_3_80 lea, dx
	modrm_help_3_80 lea, bx
	modrm_help_3_80 lea, "sp"
	modrm_help_3_80 lea, bp
	modrm_help_3_80 lea, si
	modrm_help_3_80 lea, di
; 0xC0 = mod = 11b = register (ILLEGAL INSTRUCTION!)
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
; --- LEA reg16,[idx] ---
;
; Here segment override does not matter, we are only interested in the offset.
;

	MACRO 
	lea_reg_idxidx $reg, $idx1, $idx2
	add		r0, $idx1, $idx2
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

lea_ax_bxsi
	lea_reg_idxidx eax, ebx, esi
lea_cx_bxsi
	lea_reg_idxidx ecx, ebx, esi
lea_dx_bxsi
	lea_reg_idxidx edx, ebx, esi
lea_bx_bxsi
	lea_reg_idxidx ebx, ebx, esi
lea_sp_bxsi
	lea_reg_idxidx esp, ebx, esi
lea_bp_bxsi
	lea_reg_idxidx ebp, ebx, esi
lea_si_bxsi
	lea_reg_idxidx esi, ebx, esi
lea_di_bxsi
	lea_reg_idxidx edi, ebx, esi

lea_ax_bxdi
	lea_reg_idxidx eax, ebx, edi
lea_cx_bxdi
	lea_reg_idxidx ecx, ebx, edi
lea_dx_bxdi
	lea_reg_idxidx edx, ebx, edi
lea_bx_bxdi
	lea_reg_idxidx ebx, ebx, edi
lea_sp_bxdi
	lea_reg_idxidx esp, ebx, edi
lea_bp_bxdi
	lea_reg_idxidx ebp, ebx, edi
lea_si_bxdi
	lea_reg_idxidx esi, ebx, edi
lea_di_bxdi
	lea_reg_idxidx edi, ebx, edi

lea_ax_bpsi
	lea_reg_idxidx eax, ebp, esi
lea_cx_bpsi
	lea_reg_idxidx ecx, ebp, esi
lea_dx_bpsi
	lea_reg_idxidx edx, ebp, esi
lea_bx_bpsi
	lea_reg_idxidx ebx, ebp, esi
lea_sp_bpsi
	lea_reg_idxidx esp, ebp, esi
lea_bp_bpsi
	lea_reg_idxidx ebp, ebp, esi
lea_si_bpsi
	lea_reg_idxidx esi, ebp, esi
lea_di_bpsi
	lea_reg_idxidx edi, ebp, esi

lea_ax_bpdi
	lea_reg_idxidx eax, ebp, edi
lea_cx_bpdi
	lea_reg_idxidx ecx, ebp, edi
lea_dx_bpdi
	lea_reg_idxidx edx, ebp, edi
lea_bx_bpdi
	lea_reg_idxidx ebx, ebp, edi
lea_sp_bpdi
	lea_reg_idxidx esp, ebp, edi
lea_bp_bpdi
	lea_reg_idxidx ebp, ebp, edi
lea_si_bpdi
	lea_reg_idxidx esi, ebp, edi
lea_di_bpdi
	lea_reg_idxidx edi, ebp, edi

	MACRO 
	lea_reg_disp16 $reg
	ldrb	r0,[r12],#1				; Load low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r1, increment r12 by 1
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	b		loop
	MEND

lea_ax_disp16
	lea_reg_disp16 eax
lea_cx_disp16
	lea_reg_disp16 ecx
lea_dx_disp16
	lea_reg_disp16 edx
lea_bx_disp16
	lea_reg_disp16 ebx
lea_sp_disp16
	lea_reg_disp16 esp
lea_bp_disp16
	lea_reg_disp16 ebp
lea_si_disp16
	lea_reg_disp16 esi
lea_di_disp16
	lea_reg_disp16 edi

; --- LEA reg16,[idx+disp8] ---
;
; Here segment override does not matter, we are only interested in the offset.
;

	MACRO 
	lea_reg_idxidxdisp8 $reg, $idx1, $idx2
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	add		r0, $idx1, $idx2
	add		r0, r1
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

lea_ax_bxsid8
	lea_reg_idxidxdisp8 eax, ebx, esi
lea_cx_bxsid8
	lea_reg_idxidxdisp8 ecx, ebx, esi
lea_dx_bxsid8
	lea_reg_idxidxdisp8 edx, ebx, esi
lea_bx_bxsid8
	lea_reg_idxidxdisp8 ebx, ebx, esi
lea_sp_bxsid8
	lea_reg_idxidxdisp8 esp, ebx, esi
lea_bp_bxsid8
	lea_reg_idxidxdisp8 ebp, ebx, esi
lea_si_bxsid8
	lea_reg_idxidxdisp8 esi, ebx, esi
lea_di_bxsid8
	lea_reg_idxidxdisp8 edi, ebx, esi

lea_ax_bxdid8
	lea_reg_idxidxdisp8 eax, ebx, edi
lea_cx_bxdid8
	lea_reg_idxidxdisp8 ecx, ebx, edi
lea_dx_bxdid8
	lea_reg_idxidxdisp8 edx, ebx, edi
lea_bx_bxdid8
	lea_reg_idxidxdisp8 ebx, ebx, edi
lea_sp_bxdid8
	lea_reg_idxidxdisp8 esp, ebx, edi
lea_bp_bxdid8
	lea_reg_idxidxdisp8 ebp, ebx, edi
lea_si_bxdid8
	lea_reg_idxidxdisp8 esi, ebx, edi
lea_di_bxdid8
	lea_reg_idxidxdisp8 edi, ebx, edi

lea_ax_bpsid8
	lea_reg_idxidxdisp8 eax, ebp, esi
lea_cx_bpsid8
	lea_reg_idxidxdisp8 ecx, ebp, esi
lea_dx_bpsid8
	lea_reg_idxidxdisp8 edx, ebp, esi
lea_bx_bpsid8
	lea_reg_idxidxdisp8 ebx, ebp, esi
lea_sp_bpsid8
	lea_reg_idxidxdisp8 esp, ebp, esi
lea_bp_bpsid8
	lea_reg_idxidxdisp8 ebp, ebp, esi
lea_si_bpsid8
	lea_reg_idxidxdisp8 esi, ebp, esi
lea_di_bpsid8
	lea_reg_idxidxdisp8 edi, ebp, esi

lea_ax_bpdid8
	lea_reg_idxidxdisp8 eax, ebp, edi
lea_cx_bpdid8
	lea_reg_idxidxdisp8 ecx, ebp, edi
lea_dx_bpdid8
	lea_reg_idxidxdisp8 edx, ebp, edi
lea_bx_bpdid8
	lea_reg_idxidxdisp8 ebx, ebp, edi
lea_sp_bpdid8
	lea_reg_idxidxdisp8 esp, ebp, edi
lea_bp_bpdid8
	lea_reg_idxidxdisp8 ebp, ebp, edi
lea_si_bpdid8
	lea_reg_idxidxdisp8 esi, ebp, edi
lea_di_bpdid8
	lea_reg_idxidxdisp8 edi, ebp, edi

	MACRO 
	lea_reg_idxdisp8 $reg, $idx
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	add		r0, $idx
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

lea_ax_sidisp8
	lea_reg_idxdisp8 eax, esi
lea_cx_sidisp8
	lea_reg_idxdisp8 ecx, esi
lea_dx_sidisp8
	lea_reg_idxdisp8 edx, esi
lea_bx_sidisp8
	lea_reg_idxdisp8 ebx, esi
lea_sp_sidisp8
	lea_reg_idxdisp8 esp, esi
lea_bp_sidisp8
	lea_reg_idxdisp8 ebp, esi
lea_si_sidisp8
	lea_reg_idxdisp8 esi, esi
lea_di_sidisp8
	lea_reg_idxdisp8 edi, esi

lea_ax_didisp8
	lea_reg_idxdisp8 eax, edi
lea_cx_didisp8
	lea_reg_idxdisp8 ecx, edi
lea_dx_didisp8
	lea_reg_idxdisp8 edx, edi
lea_bx_didisp8
	lea_reg_idxdisp8 ebx, edi
lea_sp_didisp8
	lea_reg_idxdisp8 esp, edi
lea_bp_didisp8
	lea_reg_idxdisp8 ebp, edi
lea_si_didisp8
	lea_reg_idxdisp8 esi, edi
lea_di_didisp8
	lea_reg_idxdisp8 edi, edi

lea_ax_bpdisp8
	lea_reg_idxdisp8 eax, ebp
lea_cx_bpdisp8
	lea_reg_idxdisp8 ecx, ebp
lea_dx_bpdisp8
	lea_reg_idxdisp8 edx, ebp
lea_bx_bpdisp8
	lea_reg_idxdisp8 ebx, ebp
lea_sp_bpdisp8
	lea_reg_idxdisp8 esp, ebp
lea_bp_bpdisp8
	lea_reg_idxdisp8 ebp, ebp
lea_si_bpdisp8
	lea_reg_idxdisp8 esi, ebp
lea_di_bpdisp8
	lea_reg_idxdisp8 edi, ebp

lea_ax_bxdisp8
	lea_reg_idxdisp8 eax, ebx
lea_cx_bxdisp8
	lea_reg_idxdisp8 ecx, ebx
lea_dx_bxdisp8
	lea_reg_idxdisp8 edx, ebx
lea_bx_bxdisp8
	lea_reg_idxdisp8 ebx, ebx
lea_sp_bxdisp8
	lea_reg_idxdisp8 esp, ebx
lea_bp_bxdisp8
	lea_reg_idxdisp8 ebp, ebx
lea_si_bxdisp8
	lea_reg_idxdisp8 esi, ebx
lea_di_bxdisp8
	lea_reg_idxdisp8 edi, ebx

; --- LEA reg16,[idx+idx+disp16] ---
;
; Here segment override does not matter, we are only interested in the offset.
;
	MACRO 
	lea_reg_idxidxdisp16 $reg, $idx1, $idx2
	ldrb	r0,[r12],#1				; Load low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r1, increment r12 by 1
	add		r2, $idx1, $idx2
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	add		r0, r2
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

lea_ax_bxsid16
	lea_reg_idxidxdisp16 eax, ebx, esi
lea_cx_bxsid16
	lea_reg_idxidxdisp16 ecx, ebx, esi
lea_dx_bxsid16
	lea_reg_idxidxdisp16 edx, ebx, esi
lea_bx_bxsid16
	lea_reg_idxidxdisp16 ebx, ebx, esi
lea_sp_bxsid16
	lea_reg_idxidxdisp16 esp, ebx, esi
lea_bp_bxsid16
	lea_reg_idxidxdisp16 ebp, ebx, esi
lea_si_bxsid16
	lea_reg_idxidxdisp16 esi, ebx, esi
lea_di_bxsid16
	lea_reg_idxidxdisp16 edi, ebx, esi

lea_ax_bxdid16
	lea_reg_idxidxdisp16 eax, ebx, edi
lea_cx_bxdid16
	lea_reg_idxidxdisp16 ecx, ebx, edi
lea_dx_bxdid16
	lea_reg_idxidxdisp16 edx, ebx, edi
lea_bx_bxdid16
	lea_reg_idxidxdisp16 ebx, ebx, edi
lea_sp_bxdid16
	lea_reg_idxidxdisp16 esp, ebx, edi
lea_bp_bxdid16
	lea_reg_idxidxdisp16 ebp, ebx, edi
lea_si_bxdid16
	lea_reg_idxidxdisp16 esi, ebx, edi
lea_di_bxdid16
	lea_reg_idxidxdisp16 edi, ebx, edi

lea_ax_bpsid16
	lea_reg_idxidxdisp16 eax, ebp, esi
lea_cx_bpsid16
	lea_reg_idxidxdisp16 ecx, ebp, esi
lea_dx_bpsid16
	lea_reg_idxidxdisp16 edx, ebp, esi
lea_bx_bpsid16
	lea_reg_idxidxdisp16 ebx, ebp, esi
lea_sp_bpsid16
	lea_reg_idxidxdisp16 esp, ebp, esi
lea_bp_bpsid16
	lea_reg_idxidxdisp16 ebp, ebp, esi
lea_si_bpsid16
	lea_reg_idxidxdisp16 esi, ebp, esi
lea_di_bpsid16
	lea_reg_idxidxdisp16 edi, ebp, esi

lea_ax_bpdid16
	lea_reg_idxidxdisp16 eax, ebp, edi
lea_cx_bpdid16
	lea_reg_idxidxdisp16 ecx, ebp, edi
lea_dx_bpdid16
	lea_reg_idxidxdisp16 edx, ebp, edi
lea_bx_bpdid16
	lea_reg_idxidxdisp16 ebx, ebp, edi
lea_sp_bpdid16
	lea_reg_idxidxdisp16 esp, ebp, edi
lea_bp_bpdid16
	lea_reg_idxidxdisp16 ebp, ebp, edi
lea_si_bpdid16
	lea_reg_idxidxdisp16 esi, ebp, edi
lea_di_bpdid16
	lea_reg_idxidxdisp16 edi, ebp, edi

	MACRO 
	lea_reg_idxdisp16 $reg, $idx
	ldrb	r0,[r12],#1				; Load low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load high byte to r1, increment r12 by 1
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	add		r0, $idx
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

lea_ax_sidisp16
	lea_reg_idxdisp16 eax, esi
lea_cx_sidisp16
	lea_reg_idxdisp16 ecx, esi
lea_dx_sidisp16
	lea_reg_idxdisp16 edx, esi
lea_bx_sidisp16
	lea_reg_idxdisp16 ebx, esi
lea_sp_sidisp16
	lea_reg_idxdisp16 esp, esi
lea_bp_sidisp16
	lea_reg_idxdisp16 ebp, esi
lea_si_sidisp16
	lea_reg_idxdisp16 esi, esi
lea_di_sidisp16
	lea_reg_idxdisp16 edi, esi

lea_ax_didisp16
	lea_reg_idxdisp16 eax, edi
lea_cx_didisp16
	lea_reg_idxdisp16 ecx, edi
lea_dx_didisp16
	lea_reg_idxdisp16 edx, edi
lea_bx_didisp16
	lea_reg_idxdisp16 ebx, edi
lea_sp_didisp16
	lea_reg_idxdisp16 esp, edi
lea_bp_didisp16
	lea_reg_idxdisp16 ebp, edi
lea_si_didisp16
	lea_reg_idxdisp16 esi, edi
lea_di_didisp16
	lea_reg_idxdisp16 edi, edi

lea_ax_bpdisp16
	lea_reg_idxdisp16 eax, ebp
lea_cx_bpdisp16
	lea_reg_idxdisp16 ecx, ebp
lea_dx_bpdisp16
	lea_reg_idxdisp16 edx, ebp
lea_bx_bpdisp16
	lea_reg_idxdisp16 ebx, ebp
lea_sp_bpdisp16
	lea_reg_idxdisp16 esp, ebp
lea_bp_bpdisp16
	lea_reg_idxdisp16 ebp, ebp
lea_si_bpdisp16
	lea_reg_idxdisp16 esi, ebp
lea_di_bpdisp16
	lea_reg_idxdisp16 edi, ebp

lea_ax_bxdisp16
	lea_reg_idxdisp16 eax, ebx
lea_cx_bxdisp16
	lea_reg_idxdisp16 ecx, ebx
lea_dx_bxdisp16
	lea_reg_idxdisp16 edx, ebx
lea_bx_bxdisp16
	lea_reg_idxdisp16 ebx, ebx
lea_sp_bxdisp16
	lea_reg_idxdisp16 esp, ebx
lea_bp_bxdisp16
	lea_reg_idxdisp16 ebp, ebx
lea_si_bxdisp16
	lea_reg_idxdisp16 esi, ebx
lea_di_bxdisp16
	lea_reg_idxdisp16 edi, ebx
	
; ------------------- 8E = MOV Sreg,r/m16 -----------------------------
; Profiler: 5493, 21, 113.66, 624308, 0.3%
;
; MOV ES,DI = 8EC7 => mod = 11, reg = 000 (ES), rm = 111 (DI)
; MOV DS,BX = 8EDB => mod = 11, reg = 011 (DS), rm = 011 (BX)
; MOV ES,[023C] = 8E063C02 => mod = 00, reg = 000 (ES), rm = 110 (disp16)
; MOV ES,[BP+06] = 8E4606 => mod = 01 (+disp8), reg = 000 (ES), rm = 110 ([BP+disp8])
;
; reg field values = ES, CS, SS, DS = 0, 1, 2, 3
;
; We must not change the flags here!!!
;
	GLOBAL	op_8e
op_8e
	modrm_jump_16_tbl op_8e_jump
; 0
	modrm_help_3_0 mov, es
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_3_0 mov, ss
	modrm_help_3_0 mov, ds
	modrm_help_3_0 mov, fs
	modrm_help_3_0 mov, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x40
	modrm_help_3_40 mov, es
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_3_40 mov, ss
	modrm_help_3_40 mov, ds
	modrm_help_3_40 mov, fs
	modrm_help_3_40 mov, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x80
	modrm_help_3_80 mov, es
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_3_80 mov, ss
	modrm_help_3_80 mov, ds
	modrm_help_3_80 mov, fs
	modrm_help_3_80 mov, gs
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
; 0xC0 = mod = 11b = register
	DCD mov_es_ax, mov_es_cx, mov_es_dx, mov_es_bx, mov_es_sp, mov_es_bp, mov_es_si, mov_es_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD mov_ss_ax, mov_ss_cx, mov_ss_dx, mov_ss_bx, mov_ss_sp, mov_ss_bp, mov_ss_si, mov_ss_di
	DCD mov_ds_ax, mov_ds_cx, mov_ds_dx, mov_ds_bx, mov_ds_sp, mov_ds_bp, mov_ds_si, mov_ds_di
	DCD mov_fs_ax, mov_fs_cx, mov_fs_dx, mov_fs_bx, mov_fs_sp, mov_fs_bp, mov_fs_si, mov_fs_di
	DCD mov_gs_ax, mov_gs_cx, mov_gs_dx, mov_gs_bx, mov_gs_sp, mov_gs_bp, mov_gs_si, mov_gs_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	EXTERN	op_8e_EGA_r2_es
	EXTERN	mov_es_r0r2_prot
	GLOBAL	mov_sreg_r0_bp_es
mov_sreg_r0_bp_es
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_sreg_r0_es
mov_sreg_r0_es
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8e_RAM_es, op_8e_EGA_r2_es, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8e_RAM_es
	ldrb	r0, [r2] 							; Load low byte
	ldrb	r2, [r2, #1]						; Load the zero-extended high byte
	orr		r2, r0, r2, lsl #8					; r2 = halfword unsigned value at [disp16]
	GLOBAL	mov_es_r2							; Called also from "EGA.s"!
mov_es_r2
	ldrb	r1, [sp, #SP_CPU_CR0]				; r1 tells whether we are in protected mode
	mrs		r0, cpsr							; Save current flags to r0
	tst		r1, #1								; Are we in protected mode?
	bne		mov_es_r0r2_prot					; Go to the handler in "cpu_prot.s"
	GLOBAL	mov_es_r0r2_real
mov_es_r0r2_real
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_ES_VALUE]
	str		r1, [sp, #SP_ES_BASE]
	b		restore_flags_from_r0

	EXTERN	mov_ss_r0r2_prot
	GLOBAL	mov_sreg_r0_bp_ss
mov_sreg_r0_bp_ss
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_sreg_r0_ss
mov_sreg_r0_ss
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8e_RAM_ss, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8e_RAM_ss
	ldrb	r0, [r2] 							; Load low byte
	ldrb	r2, [r2, #1]						; Load the zero-extended high byte
	orr		r2, r0, r2, lsl #8					; r2 = halfword unsigned value at [disp16]
mov_ss_r2										; Called from the register versions of the opcode
	ldrb	r1, [sp, #SP_CPU_CR0]				; r1 tells whether we are in protected mode
	mrs		r0, cpsr							; Save current flags to r0
	tst		r1, #1								; Are we in protected mode?
	bne		mov_ss_r0r2_prot					; Go to the handler in "cpu_prot.s"
	GLOBAL	mov_ss_r0r2_real
mov_ss_r0r2_real
	msr		cpsr_f,r0							; Restore flags
	str		r2, [sp, #SP_SS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_SS_BASE]
	mov		lr, r2
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]
	;-------
	; NOTE! x86 disables interrupts until the next instruction has been executed.
	; Thus we must handle the next opcode immediately!
	;-------
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1	
	ldr		r2, [sp, #SP_DS_BASE]				; r2 high halfword = logical DS segment, clear segment override flags
	str		r12, [sp, #SP_EX_CSIP]				; Remember where this opcode started, for division-by-zero and exception handling
	ldr		pc,[sp, r0, lsl #2]					; Jump to the opcode handler

	EXTERN	mov_ds_r0r2_prot
	GLOBAL	mov_sreg_r0_bp_ds
mov_sreg_r0_bp_ds
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_sreg_r0_ds
mov_sreg_r0_ds
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8e_RAM_ds, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8e_RAM_ds
	ldrb	r0, [r2] 							; Load low byte
	ldrb	r2, [r2, #1]						; Load the zero-extended high byte
	orr		r2, r0, r2, lsl #8					; r2 = halfword unsigned value at [disp16]
mov_ds_r2										; Called from the register versions of the opcode
	ldrb	r1, [sp, #SP_CPU_CR0]				; r1 tells whether we are in protected mode
	mrs		r0, cpsr							; Save current flags to r0
	tst		r1, #1								; Are we in protected mode?
	bne		mov_ds_r0r2_prot					; Go to the handler in "cpu_prot.s"
	GLOBAL	mov_ds_r0r2_real
mov_ds_r0r2_real
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_DS_VALUE]
	str		r1, [sp, #SP_DS_BASE]
	b		restore_flags_from_r0

	EXTERN	mov_fs_r0r2_prot
	GLOBAL	mov_sreg_r0_bp_fs
mov_sreg_r0_bp_fs
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_sreg_r0_fs
mov_sreg_r0_fs
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8e_RAM_fs, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8e_RAM_fs
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r2, [r2, #1]			; Load the zero-extended high byte
	orr		r2, r0, r2, lsl #8		; r2 = halfword unsigned value at [disp16]
mov_fs_r2							; Called from the register versions of the opcode
	ldrb	r1, [sp, #SP_CPU_CR0]				; r1 tells whether we are in protected mode
	mrs		r0, cpsr							; Save current flags to r0
	tst		r1, #1								; Are we in protected mode?
	bne		mov_fs_r0r2_prot					; Go to the handler in "cpu_prot.s"
	GLOBAL	mov_fs_r0r2_real
mov_fs_r0r2_real
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_FS_VALUE]
	str		r1, [sp, #SP_FS_BASE]
	b		restore_flags_from_r0

	EXTERN	mov_gs_r0r2_prot
	GLOBAL	mov_sreg_r0_bp_gs
mov_sreg_r0_bp_gs
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_sreg_r0_gs
mov_sreg_r0_gs
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8e_RAM_gs, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8e_RAM_gs
	ldrb	r0, [r2] 				; Load low byte
	ldrb	r2, [r2, #1]			; Load the zero-extended high byte
	orr		r2, r0, r2, lsl #8		; r2 = halfword unsigned value at [disp16]
mov_gs_r2							; Called from the register versions of the opcode
	ldrb	r1, [sp, #SP_CPU_CR0]				; r1 tells whether we are in protected mode
	mrs		r0, cpsr							; Save current flags to r0
	tst		r1, #1								; Are we in protected mode?
	bne		mov_gs_r0r2_prot					; Go to the handler in "cpu_prot.s"
	GLOBAL	mov_gs_r0r2_real
mov_gs_r0r2_real
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_GS_VALUE]
	str		r1, [sp, #SP_GS_BASE]
	b		restore_flags_from_r0

	modrm_3_help mov, mov_sreg_r0, es
	modrm_3_help mov, mov_sreg_r0, ss
	modrm_3_help mov, mov_sreg_r0, ds
	modrm_3_help mov, mov_sreg_r0, fs
	modrm_3_help mov, mov_sreg_r0, gs

; --- MOV Sreg,reg16 ---

	MACRO 
	mov_es_reg $reg
	ubfx	r2, $reg, #0, #16		; Segment registers are saved as 16-bit unsigned values
	b		mov_es_r2
	MEND
	MACRO 
	mov_ss_reg $reg
	ubfx	r2, $reg, #0, #16		; Segment registers are saved as 16-bit unsigned values
	b		mov_ss_r2
	MEND
	MACRO 
	mov_ds_reg $reg
	ubfx	r2, $reg, #0, #16		; Segment registers are saved as 16-bit unsigned values
	b		mov_ds_r2
	MEND
	MACRO 
	mov_fs_reg $reg
	ubfx	r2, $reg, #0, #16		; Segment registers are saved as 16-bit unsigned values
	b		mov_fs_r2
	MEND
	MACRO 
	mov_gs_reg $reg
	ubfx	r2, $reg, #0, #16		; Segment registers are saved as 16-bit unsigned values
	b		mov_gs_r2
	MEND

mov_es_ax
	mov_es_reg eax
mov_es_cx
	mov_es_reg ecx
mov_es_dx
	mov_es_reg edx
mov_es_bx
	mov_es_reg ebx
mov_es_sp
	mov_es_reg esp
mov_es_bp
	mov_es_reg ebp
mov_es_si
	mov_es_reg esi
mov_es_di
	mov_es_reg edi

mov_ss_ax
	mov_ss_reg eax
mov_ss_cx
	mov_ss_reg ecx
mov_ss_dx
	mov_ss_reg edx
mov_ss_bx
	mov_ss_reg ebx
mov_ss_sp
	mov_ss_reg esp
mov_ss_bp
	mov_ss_reg ebp
mov_ss_si
	mov_ss_reg esi
mov_ss_di
	mov_ss_reg edi

mov_ds_ax
	mov_ds_reg eax
mov_ds_cx
	mov_ds_reg ecx
mov_ds_dx
	mov_ds_reg edx
mov_ds_bx
	mov_ds_reg ebx
mov_ds_sp
	mov_ds_reg esp
mov_ds_bp
	mov_ds_reg ebp
mov_ds_si
	mov_ds_reg esi
mov_ds_di
	mov_ds_reg edi

mov_fs_ax
	mov_fs_reg eax
mov_fs_cx
	mov_fs_reg ecx
mov_fs_dx
	mov_fs_reg edx
mov_fs_bx
	mov_fs_reg ebx
mov_fs_sp
	mov_fs_reg esp
mov_fs_bp
	mov_fs_reg ebp
mov_fs_si
	mov_fs_reg esi
mov_fs_di
	mov_fs_reg edi

mov_gs_ax
	mov_gs_reg eax
mov_gs_cx
	mov_gs_reg ecx
mov_gs_dx
	mov_gs_reg edx
mov_gs_bx
	mov_gs_reg ebx
mov_gs_sp
	mov_gs_reg esp
mov_gs_bp
	mov_gs_reg ebp
mov_gs_si
	mov_gs_reg esi
mov_gs_di
	mov_gs_reg edi

	GLOBAL	mov_es_ax
	GLOBAL	mov_es_cx
	GLOBAL	mov_es_dx
	GLOBAL	mov_es_bx
	GLOBAL	mov_es_sp
	GLOBAL	mov_es_bp
	GLOBAL	mov_es_si
	GLOBAL	mov_es_di
	GLOBAL	mov_ss_ax
	GLOBAL	mov_ss_cx
	GLOBAL	mov_ss_dx
	GLOBAL	mov_ss_bx
	GLOBAL	mov_ss_sp
	GLOBAL	mov_ss_bp
	GLOBAL	mov_ss_si
	GLOBAL	mov_ss_di
	GLOBAL	mov_ds_ax
	GLOBAL	mov_ds_cx
	GLOBAL	mov_ds_dx
	GLOBAL	mov_ds_bx
	GLOBAL	mov_ds_sp
	GLOBAL	mov_ds_bp
	GLOBAL	mov_ds_si
	GLOBAL	mov_ds_di
	GLOBAL	mov_fs_ax
	GLOBAL	mov_fs_cx
	GLOBAL	mov_fs_dx
	GLOBAL	mov_fs_bx
	GLOBAL	mov_fs_sp
	GLOBAL	mov_fs_bp
	GLOBAL	mov_fs_si
	GLOBAL	mov_fs_di
	GLOBAL	mov_gs_ax
	GLOBAL	mov_gs_cx
	GLOBAL	mov_gs_dx
	GLOBAL	mov_gs_bx
	GLOBAL	mov_gs_sp
	GLOBAL	mov_gs_bp
	GLOBAL	mov_gs_si
	GLOBAL	mov_gs_di


	LTORG								; Dump the current literal pool here

; ------------------- 8F = POP m16 ------------------------------------
;
; This is a rare opcode, modrm = rm000mod
;
op_8f
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =op_8f_jump
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	orr		r0, r0, lsr #3
	and		r0, #0x1F
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
op_8f_jump
; 0
	modrm_help_1_0 pop
; 0x40
	modrm_help_1_40 pop
; 0x80
	modrm_help_1_80 pop
; 0xC0
	DCD op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	GLOBAL	pop_r0_bp
pop_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	pop_r0
pop_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_8f_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_8f_RAM
	pop_reg_low_hi r0, r1
	strb	r0,[r2]					; Store the low byte
	strb	r1,[r2, #1]				; Store the high byte
	b		loop

	modrm_1_help pop, pop_r0


; ------------------- 98 = CBW ----------------------------------------
;
op_98
	sbfx	r0, eax, #0, #8
	bfi		eax, r0, #0, #16
	b		loop

; ------------------- 99 = CWD ----------------------------------------
;
op_99
	sbfx	r0, eax, #0, #16
	asr		r0, #16
	bfi		edx, r0, #0, #16					; r0 high 16 bits = new DX value
	b		loop

; ------------------- 9A = CALL FAR ptr16:16 --------------------------
; Profiler: 7571, 31, 46.84, 354618, 0.18%
;
; CALL 1604:3228 = 9A28320416
;
op_9a
	;-------
	; First get the call target 
	;	r1 = new IP (offset)
	;	r2 = new CS (segment)
	;-------
	mov		r0, #16
	str		r0, [sp, #SP_FREE3]		; Save the flag telling this is a USE16 call far
	ldrb	r0,[r12], #1
	ldrb	r1,[r12], #1
	ldrb	r2,[r12], #1
	ldrb	r3,[r12], #1
	orr		r1, r0, r1, lsl #8		; r1 = new logical IP
	orr		r2, r3, lsl #8			; r2 = new CS value
	GLOBAL	cpu_call_far_r1r2
cpu_call_far_r1r2
	;-------
	; Then determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		cpu_call_prot_r0r1r2	; Yes we are, go handle protected mode CALL FAR!
	;-------
	; Real mode CALL FAR handling
	;-------
	GLOBAL	cpu_call_real_r0r1r2
cpu_call_real_r0r1r2
	; ----- Store current CS:IP to stack
	ldr		r3, [sp, #SP_CS_VALUE]
	msr		cpsr_f, r0				; Restore flags
	push_hword r3, r0, lr
	ldr		r3, [sp, #SP_PHYS_CS]	; get the physical CS:0000 into r3
	sub		r3, r12, r3				; r3 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r3, r0, lr
	; ----- Then get new CS:IP
	mov		r12, r1
	; ----- Then save new logical CS
	str		r2, [sp, #SP_CS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]	; Store new physical CS into stack
	add		r12, r2 				; r12 = new physical CS:IP = new IP + physical base + (new CS << 4)
	b		loop

	EXTERN	cpu_call_prot_r0r1r2
	
	LTORG
	
; ------------------- 9C = PUSHF --------------------------------------
;	if (cpu.pmode && GETFLAG(VM) && (GETFLAG(IOPL)!=FLAG_IOPL)) {
;		/* Not enough privileges to execute PUSHF */
;		return CPU_PrepareException(EXCEPTION_GP,0);
;	}
;
op_9c
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	ldr		r1, [sp, #SP_FLAGS]					; Get the "Trap", "Interrupts Enabled" and "Direction" flags
	mrs		r0,cpsr								; Save flags (we are not allowed to change any)
	tst		r3, #1								; Are we in protected mode?
	bne		op_9c_prot
op_9c_cont
	msr		cpsr_f, r0							; Restore flags
	join_ARM_to_X86_flags r1
	push_hword r1, r0, r2
	b		loop
op_9c_prot
	tst		r1, #FLAG_VM
	beq		op_9c_cont
	and		r2, r1, #FLAG_IOPL
	cmp		r2, #FLAG_IOPL
	beq		op_9c_cont
	b		unknown								; EXCEPTION_GP!

; ------------------- 9D = POPF ---------------------------------------
;	if (cpu.pmode && GETFLAG(VM) && (GETFLAG(IOPL)!=FLAG_IOPL)) {
;		/* Not enough privileges to execute POPF */
;		return CPU_PrepareException(EXCEPTION_GP,0);
;	}
;	Bitu mask=FMASK_ALL;
;	/* IOPL field can only be modified when CPL=0 or in real mode: */
;	if (cpu.pmode && (cpu.cpl>0)) mask &= (~FLAG_IOPL);
;	if (cpu.pmode && !GETFLAG(VM) && (GETFLAG_IOPL<cpu.cpl)) mask &= (~FLAG_IF);
;	if (use32)
;		CPU_SetFlags(CPU_Pop32(),mask);
;	else CPU_SetFlags(CPU_Pop16(),mask & 0xffff);
;	DestroyConditionFlags();
;	return false;
;
;
op_9d
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	ldr		r1, [sp, #SP_FLAGS]					; Get the #SP_FLAGS
	ldr		r2, =FMASK_ALL						; r2 = mask of the flag bits we can change
	tst		r3, #1								; Are we in protected mode?
	bne		op_9d_prot
op_9d_cont
	pop_reg_low_tmp	r0, r3						; r0 = new flags
	bic		r2, #0x00FF0000						; mask = mask & 0xFFFF
	and		r0, r2								; Leave only the bits we can change to r0
	bic		r2, r1, r2							; r1 = flags bits that will not change
	orr		r0, r2
	str		r0, [sp, #SP_FLAGS]					; Store the new #SP_FLAGS
	b		iret_cont_flags_old_r1_new_r0
op_9d_prot
	tst		r1, #FLAG_VM						; Are we in VM mode?
	and		r3, r1, #FLAG_IOPL
	cmpne	r3, #FLAG_IOPL						; If we are in VM mode, check FLAG_IOPL
	ldrb	r0, [sp, #SP_CPU_CPL]
	bne		unknown							; EXCEPTION_GP!
	cmp		r0, #0								; if (cpu.pmode && (cpu.cpl>0)) 
	bicgt	r2, #FLAG_IOPL						;	mask &= (~FLAG_IOPL);
	cmp		r3, r0, lsl #12						; (GETFLAG_IOPL<cpu.cpl)
	biclt	r2, #FLAG_IF						;	mask &= (~FLAG_IF);
	b		op_9d_cont

	LTORG
	
; ------------------- 9E = SAHF ---------------------------------------
;
; ARM Flags bits
; 	bit 0x80000000 = Negative Flag
;	bit 0x40000000 = Zero Flag
;	bit 0x20000000 = Carry flag
;	bit	0x10000000 = Overflow Flag
; x86 Flags bits
;	0 	CF Carry flag
;	1 	1 Reserved   
;	2 	PF Parity flag
;	3 	0 Reserved   
;	4 	AF Adjust flag
;	5 	0 Reserved   
;	6 	ZF Zero flag
;	7 	SF Sign flag
;
op_9e
	mov		r0, eax, lsr #8
	and		r0, #(FLAG_SF:OR:FLAG_ZF:OR:FLAG_AF:OR:FLAG_PF:OR:FLAG_CF)
	orr		r0, #2
	strb	r0, [sp, #SP_FLAGS]		; Save the AF and PF flags to emulated x86 flags
	popped_flags_to_ARM r0			; Move the flags in r0 to the ARM flag bits
	b		restore_flags_from_r0	; Go back to the opcode loop
	
; ------------------- 9F = LAHF ---------------------------------------
;
op_9f
	mrs		r0, cpsr				; Save flags to r0
	ldrb	r1, [sp, #SP_FLAGS]		; Get the AF and PF flags from emulated x86 flags
	;-------
	; First set the flag bits that are common to ARM and x86
	;-------
	bic		eax, #0xFF00			; Clear current AH
	orrcs	eax, #((FLAG_CF)<<8)	; Set Carry flag
	orr		eax, #((1<<1)<<8)		; Set the Reserved flag
	orreq	eax, #((FLAG_ZF)<<8)	; Set Zero flag
	orrmi	eax, #((FLAG_SF)<<8)	; Set Sign flag
	;-------
	; Set the x86 emulated AF and PF flags
	;-------
	and		r1, #(FLAG_AF:OR:FLAG_PF)
	orr		eax, r1, lsl #8
	;-------
	; Check if the following opcode is "TEST AH,10" = F6C410 (testing for Auxiliary Carry, Leisure Suit Larry III)
	;-------
	ldrb	r1, [r12]
	cmp		r1, #0xF6
	ldrbeq	r1, [r12, #1]
	cmpeq	r1, #0xC4
	ldrbeq	r1, [r12, #2]
	cmpeq	r1, #0x10
	bne		restore_flags_from_r0	; Go back to the opcode loop
	;-------
	; The next opcode is a check for the AC flag!
	; Check if the previous opcode is "CMP BL,[DI+7FF0]" = 3A9DF07F	(Leisure Suit Larry III)
	;-------
	ldrb	r1, [r12, #-5]
	cmp		r1, #0x3A
	ldrbeq	r1, [r12, #-4]
	cmpeq	r1, #0x9D
	ldrbeq	r1, [r12, #-3]
	cmpeq	r1, #0xF0
	ldrbeq	r1, [r12, #-2]
	cmpeq	r1, #0x7F
	bne		%f1
	;-------
	; The previous opcode was "CMP BL,[DI+7FF0]". Perform the compare again for the lowest 4 bits of each byte,
	; and put the resulting (complemented) carry to the AC flag bit of AH.
	;-------
	mov		r1, r0					; Save flags in r0 to r1
	ldr		r2, [sp, #SP_DS_BASE]	; r2 = current effective logical DS segment
	mov		r3, #0xFFFF				; Use 16-bit memory addressing
	mov		r0, edi					; r0 = DI
	add		r0, #0x00F0
	add		r0, #0x7F00				; r0 = DI+7FF0
	calc_linear_address_r2_from_r0r3
	mov		r0, r1					; Restore flags from r1 to r0
	ldrb	r1, [r2]				; r1 = byte from [DI+7FF0]
	mov		r2, ebx, lsl #(24+4)	; r2 = low nibble of BL in highest bits
	cmp		r2, r1, lsl #(24+4)		; CMP BL,[DI+7FF0], ARM Carry clear if we need to set FLAG_AF.
	orrcc	eax, #((FLAG_AF)<<8)	; Set FLAG_AF if needed
	b		restore_flags_from_r0	; Go back to the opcode loop
1	msr		cpsr_f,r0
	b		debug_trap_false
	
; ------------------- A0 = MOV AL,moffs8 ------------------------------
;
; See op_8a (mov_al_disp16)

; ------------------- A1 = MOV AX,moffs16 -----------------------------
;
; See op_8b (mov_ax_disp16)

; ------------------- A2 = MOV moffs8,AL ------------------------------
;
; See op_88 (mov_disp16_al)

; ------------------- A3 = MOV moffs16,AX ------------------------------
;
; See op_89 (mov_disp16_ax)

; ------------------- A8 = TEST AL,imm8 -------------------------------
;
op_a8
	ldrb	r0,[r12],#1				; Load the imm8 byte to r0, increment r12 by 1
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, eax, lsl #24		; Move AL into high byte of r1
	ands	r1, r0, lsl #24			; Perform the test
	;-------
	; Save the resulting byte (for possible later parity check)
	;-------
	lsr		r1, #24
	strb	r1, [sp, #SP_PARITY_BYTE]	; For "Chess Genius 3", save the parity byte to stack.
	b		loop

; ------------------- A9 = TEST AX,imm16 ------------------------------
;
op_a9
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	ldrb	r0,[r12],#1				; Load byte to reg, increment r12 by 1
	ldrb	r1,[r12],#1				; Load byte to r0, increment r12 by 1
	mov		r2, eax, lsl #16
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	tst		r2, r0, lsl #16			; Perform the test
	b		loop

; =================== B0..B7 = MOV reg8,imm8 ==========================
; Opcodes B0..B7 for AL, CL, DL, BL, AH, CH, DH, BH
;
	MACRO 
	mov_regl_imm8 $reg
	ldrb	r0,[r12],#1				; Load imm8 to r0, increment r12 by 1
	bfi		$reg, r0, #0, #8
	b		loop
	MEND
	MACRO 
	mov_regh_imm8 $reg
	ldrb	r0,[r12],#1				; Load imm8 to r0, increment r12 by 1
	bfi		$reg, r0, #8, #8
	b		loop
	MEND

op_b0								; MOV AL,imm8
	mov_regl_imm8 eax
op_b1								; MOV CL,imm8
	mov_regl_imm8 ecx
op_b2								; MOV DL,imm8
	mov_regl_imm8 edx
op_b3								; MOV BL,imm8
	mov_regl_imm8 ebx
op_b4								; MOV AH,imm8
	mov_regh_imm8 eax
op_b5								; MOV CH,imm8
	mov_regh_imm8 ecx
op_b6								; MOV DH,imm8
	mov_regh_imm8 edx
op_b7								; MOV BH,imm8
	mov_regh_imm8 ebx

; =================== B8..BF = MOV reg16,imm16 ========================
; Opcodes B8..BF for AX, CX, DX, BX, SP, BP, SI, DI
;
	MACRO 
	mov_reg_imm16 $reg
	ldrb	r0,[r12],#1
	ldrb	r1,[r12],#1
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	b		loop
	MEND

op_b8
	mov_reg_imm16 eax
op_b9
	mov_reg_imm16 ecx
op_ba
	mov_reg_imm16 edx
op_bb
	mov_reg_imm16 ebx
op_bc
	mov_reg_imm16 esp
op_bd
	mov_reg_imm16 ebp
op_be
	mov_reg_imm16 esi
op_bf
	mov_reg_imm16 edi

	ALIGN	4
	
; ------------------- C0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,imm8 ---
; 
; Note: This instruction is only available on 80186 and up.
;
; We must also setup the flags!
; - RCL/RCR/ROL/ROR change only the carry flag, and overflow flag if the rotation count == 1.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags (ARM behaves exactly like 8086)
;
op_c0
	modrm_jump_16_tbl op_c0_jump
; 0 (idx only)
	modrm_help_1_0 rol_b, imm8
	modrm_help_1_0 ror_b, imm8
	modrm_help_1_0 rcl_b, imm8
	modrm_help_1_0 rcr_b, imm8
	modrm_help_1_0 shl_b, imm8
	modrm_help_1_0 shr_b, imm8
	modrm_help_1_0 shl_b, imm8
	modrm_help_1_0 sar_b, imm8
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_b, imm8
	modrm_help_1_40 ror_b, imm8
	modrm_help_1_40 rcl_b, imm8
	modrm_help_1_40 rcr_b, imm8
	modrm_help_1_40 shl_b, imm8
	modrm_help_1_40 shr_b, imm8
	modrm_help_1_40 shl_b, imm8
	modrm_help_1_40 sar_b, imm8
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_b, imm8
	modrm_help_1_80 ror_b, imm8
	modrm_help_1_80 rcl_b, imm8
	modrm_help_1_80 rcr_b, imm8
	modrm_help_1_80 shl_b, imm8
	modrm_help_1_80 shr_b, imm8
	modrm_help_1_80 shl_b, imm8
	modrm_help_1_80 sar_b, imm8
;0xc0 = mod = 11b => two register operands
	DCD rol_al_imm8, rol_cl_imm8, rol_dl_imm8, rol_bl_imm8, rol_ah_imm8, rol_ch_imm8, rol_dh_imm8, rol_bh_imm8
	DCD ror_al_imm8, ror_cl_imm8, ror_dl_imm8, ror_bl_imm8, ror_ah_imm8, ror_ch_imm8, ror_dh_imm8, ror_bh_imm8
	DCD rcl_al_imm8, rcl_cl_imm8, rcl_dl_imm8, rcl_bl_imm8, rcl_ah_imm8, rcl_ch_imm8, rcl_dh_imm8, rcl_bh_imm8
	DCD rcr_al_imm8, rcr_cl_imm8, rcr_dl_imm8, rcr_bl_imm8, rcr_ah_imm8, rcr_ch_imm8, rcr_dh_imm8, rcr_bh_imm8
	DCD shl_al_imm8, shl_cl_imm8, shl_dl_imm8, shl_bl_imm8, shl_ah_imm8, shl_ch_imm8, shl_dh_imm8, shl_bh_imm8
	DCD shr_al_imm8, shr_cl_imm8, shr_dl_imm8, shr_bl_imm8, shr_ah_imm8, shr_ch_imm8, shr_dh_imm8, shr_bh_imm8
	DCD shl_al_imm8, shl_cl_imm8, shl_dl_imm8, shl_bl_imm8, shl_ah_imm8, shl_ch_imm8, shl_dh_imm8, shl_bh_imm8
	DCD sar_al_imm8, sar_cl_imm8, sar_dl_imm8, sar_bl_imm8, sar_ah_imm8, sar_ch_imm8, sar_dh_imm8, sar_bh_imm8

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
; ----- ROL -----

	GLOBAL	rol_b_r0_bp_imm8
rol_b_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rol_b_r0_imm8
rol_b_r0_imm8					; Rotate a byte at offset r0high in effective segment left by r1 value
	mem_handler_jump_r0r3 rol_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rol_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
rol_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	bic		r0, #0xFF				; Clear the low byte of r0 (highest byte contains the saved flags)
	rsb		r1, #8					; r1 = 8-rol_count == ror_count
	orr		r0, r1					; Now r0 low byte is the number of bits to rotate the byte right 
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	mov		r1, r1, ror r0			; Rotate the value
	orr		r1, r1, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	strb	r1, [r2]				; Save the byte back to RAM
	tst		r1, #1					; Is the lowest bit set (means we need to set Carry)?
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
rol_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	mrs		r0,cpsr					; Save flags to r0
1	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsl #(24+1)		; First shift the register left 1 bit position, and set the flags
	orrcs	r1, #0x01000000			; If Carry is set, set the lowest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	lsr		r1, #24
	strb	r1, [r2]				; Save the shifted value
	eor		r1, r0, r1, lsl #(24-2)	; Now r0 ARM_CARRY bit = new Overflow bit value
	and		r1, #ARM_CARRY			; Leave only the ARM_CARRY bit into r0
	orr		r0, r1, lsr #1			; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the low bit of the byte value.
	;=======
8	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY
	tst		r1, #1
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rol_b, rol_b_r0, imm8

	MACRO 
	rol_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rol_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	rsb		r2, r1, #8				; r2 = 8-rol_count == ror_count
	and		r1, $reg, #0xFF
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	mov		r1, r1, ror r2			; Rotate the value
	orr		r1, r1, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	and		r1, #0xFF
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1				; and replace it with r1 shifted to reg8l
	tst		r1, #1					; Is the lowest bit set (means we need to set Carry)?
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
	GLOBAL	rol_reg8l_1_$reg
rol_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	mov		r1, $reg, lsl #24		; r1 = reg8l value in the highest byte
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsl #1			; First shift the register left 1 bit position, and set the flags
	orrcs	r1, #0x01000000			; If Carry is set, set the lowest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1, lsr #24		; and replace it with r1 shifted to reg8l
	eor		r1, r0, r1, lsr #2		; Now r0 ARM_CARRY bit = new Overflow bit value
	and		r1, #ARM_CARRY			; Leave only the ARM_CARRY bit into r0
	orr		r0, r1, lsr #1			; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the low bit of the byte value.
	;=======
8	bic		r0, #ARM_CARRY
	tst		$reg, #0x0001
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND
	MACRO 
	rol_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rol_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	rsb		r2, r1, #8				; r2 = 8-rol_count == ror_count
	mov		r1, $reg, lsr #8		; r1 = reg8h value in the lowest byte
	and		r1, #0xFF
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	mov		r1, r1, ror r2			; Rotate the value
	orr		r1, r1, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	and		r1, #0xFF
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsl #8		; and replace it with r1 shifted to reg8h
	tst		r1, #1					; Is the lowest bit set (means we need to set Carry)?
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======
	GLOBAL	rol_reg8h_1_$reg
rol_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	mov		r1, $reg, lsl #16		; r1 = reg8h value in the highest byte
	and		r1, #0xFF000000
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsl #1			; First shift the register left 1 bit position, and set the flags
	orrcs	r1, #0x01000000			; If Carry is set, set the lowest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsr #16		; and replace it with r1 shifted to reg8h
	eor		r1, r0, r1, lsr #2		; Now r0 ARM_CARRY bit = new Overflow bit value
	and		r1, #ARM_CARRY			; Leave only the ARM_CARRY bit into r0
	orr		r0, r1, lsr #1			; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the low bit of the byte value.
	;=======
8	bic		r0, #ARM_CARRY
	tst		$reg, #0x0100
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rol_al_imm8
	rol_reg8l_imm8 eax
rol_cl_imm8
	rol_reg8l_imm8 ecx
rol_dl_imm8
	rol_reg8l_imm8 edx
rol_bl_imm8
	rol_reg8l_imm8 ebx
rol_ah_imm8
	rol_reg8h_imm8 eax
rol_ch_imm8
	rol_reg8h_imm8 ecx
rol_dh_imm8
	rol_reg8h_imm8 edx
rol_bh_imm8
	rol_reg8h_imm8 ebx

	GLOBAL rol_al_imm8
	GLOBAL rol_cl_imm8
	GLOBAL rol_dl_imm8
	GLOBAL rol_bl_imm8
	GLOBAL rol_ah_imm8
	GLOBAL rol_ch_imm8
	GLOBAL rol_dh_imm8
	GLOBAL rol_bh_imm8

; ----- ROR -----

	GLOBAL	ror_b_r0_bp_imm8
ror_b_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	ror_b_r0_imm8
ror_b_r0_imm8					; Rotate a byte at offset r0high in effective segment right by r1 value
	mem_handler_jump_r0r3 ror_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
ror_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
ror_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	bic		r0, #0xFF				; Clear the low byte of r0 (highest byte contains the saved flags)
	orr		r0, r1					; Now r0 low byte is the number of bits to rotate the byte right 
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	movs	r1, r1, ror r0			; Rotate the value
	orr		r1, r1, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	strb	r1, [r2]				; Save the byte back to RAM
	orrcs	r0, #ARM_CARRY			; If Carry is set, set it in the saved flags as well.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
ror_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	mrs		r0,cpsr					; Save flags to r0
1	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; First shift the register right 1 bit position, and set the flags
	orrcs	r1, #0x80				; If Carry is set, set the highest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	strb	r1, [r2]				; Save the shifted value
	eor		r1, r1, lsl #1			; Now r1 bit 0x80 = new Overflow bit value
	and		r1, #0x80				; Leave only the highest bit into r1
	orr		r0, r1, lsl #(24-3)		; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the high bit of the byte value.
	;=======
8	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	bic		r0, #ARM_CARRY
	tst		r1, #0x80
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help ror_b, ror_b_r0, imm8

	MACRO 
	ror_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
ror_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	and		r2, $reg, #0xFF			; r1 = reg8l value in the lowest byte
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	movs	r2, r2, ror r1			; Rotate the value
	orr		r2, r2, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	and		r2, #0xFF
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r2				; and replace it with r1 shifted to reg8l
	orrcs	r0, #ARM_CARRY			; If Carry is set, set it in the saved flags as well.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
	GLOBAL	ror_reg8l_1_$reg
ror_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	and		r1, $reg, #0xFF			; r1 = reg8l value in the lowest byte
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; First shift the register right 1 bit position, and set the flags
	orrcs	r1, #0x80				; If Carry is set, set the highest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1				; and replace it with r1 shifted to reg8l
	eor		r1, r1, lsl #1			; Now r1 bit 0x80 = new Overflow bit value
	and		r1, #0x80				; Leave only the highest bit into r1
	orr		r0, r1, lsl #(24-3)		; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the high bit of the byte value.
	;=======
8	bic		r0, #ARM_CARRY
	tst		$reg, #0x0080
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND
	MACRO 
	ror_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
ror_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, 8, 16 or 24 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #7					; Are we to shift by a number divisible by 8, yet not zero bits?
	beq		%f8						; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..7 bit positions.
	;=======
	mov		r2, $reg, lsr #8		; r2 = reg8h value in the lowest byte
	and		r2, #0xFF
	bic		r0, #ARM_CARRY			; Clear the Carry bit of the saved CPU flags
	movs	r2, r2, ror r1			; Rotate the value
	orr		r2, r2, lsr #24			; Move the bits we rotated from the bottom back to the lowest byte
	and		r2, #0xFF
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r2, lsl #8		; and replace it with r2 shifted to reg8h
	orrcs	r0, #ARM_CARRY			; If Carry is set, set it in the saved flags as well.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
	GLOBAL	ror_reg8h_1_$reg
ror_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	mov		r1, $reg, lsr #8		; r1 = reg8h value in the lowest byte
	and		r1, #0xFF
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; First shift the register right 1 bit position, and set the flags
	orrcs	r1, #0x80				; If Carry is set, set the highest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	bic		$reg, #0xFF00			; Clear the current reg8l value
	orr		$reg, r1, lsl #8		; and replace it with r1 shifted to reg8l
	eor		r1, r1, lsl #1			; Now r1 bit 0x80 = new Overflow bit value
	and		r1, #0x80				; Leave only the highest bit into r1
	orr		r0, r1, lsl #(24-3)		; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 8, 16 or 24 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the high bit of the byte value.
	;=======
8	bic		r0, #ARM_CARRY
	tst		$reg, #0x8000
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

ror_al_imm8
	ror_reg8l_imm8 eax
ror_cl_imm8
	ror_reg8l_imm8 ecx
ror_dl_imm8
	ror_reg8l_imm8 edx
ror_bl_imm8
	ror_reg8l_imm8 ebx
ror_ah_imm8
	ror_reg8h_imm8 eax
ror_ch_imm8
	ror_reg8h_imm8 ecx
ror_dh_imm8
	ror_reg8h_imm8 edx
ror_bh_imm8
	ror_reg8h_imm8 ebx

	GLOBAL ror_al_imm8
	GLOBAL ror_cl_imm8
	GLOBAL ror_dl_imm8
	GLOBAL ror_bl_imm8
	GLOBAL ror_ah_imm8
	GLOBAL ror_ch_imm8
	GLOBAL ror_dh_imm8
	GLOBAL ror_bh_imm8

; ----- RCL -----

	GLOBAL	rcl_b_r0_bp_imm8
rcl_b_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rcl_b_r0_imm8
rcl_b_r0_imm8					; Rotate a byte at offset r0high in effective segment left by r1 value
	mem_handler_jump_r0r3 rcl_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rcl_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcl_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	bic		r0, #0xFF				; Clear the low byte of r0 (highest byte contains the saved flags)
	orr		r0, r1					; r0 low byte = number of bit positions to rotate
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r1, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the lowest bit, bit 0x100 will be the new Carry.
	;-------
2	lsl		r1, #1					; Perform the rotate once
	tst		r1, #0x200				; Was the 9th bit set before the rotate?
	orrne	r1, #1					; It was, set the lowest bit
	sub		r0, #1					; One bit handled,
	tst		r0, #0xFF				; still bits to do?
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Save the result
	;-------
	strb	r1, [r2]
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r1, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======	
rcl_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	mrs		r0,cpsr					; Save flags to r0
1	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	tst		r0, #ARM_CARRY
	lsl		r1, #1					; Shift left 1 bit
	orrne	r1, #1					; Move Carry into the lowest bit
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	strb	r1, [r2]				; Save the shifted value
	eor		r1, r1, lsr #1			; Now r1 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r1, #0x180				; Leave only the new Carry and Overflow bits to r1
	orr		r0, r1, lsl #(24-3)		; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rcl_b, rcl_b_r0, imm8

	MACRO 
	rcl_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcl_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	and		r2, $reg, #0xFF			; r2 = reg8l value in the lowest byte
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the lowest bit, bit 0x100 will be the new Carry.
	;-------
2	lsl		r2, #1					; Perform the rotate once
	tst		r2, #0x200				; Was the 9th bit set before the rotate?
	orrne	r2, #1					; It was, set the lowest bit
	subs	r1, #1					; One bit handled,
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF				; Clear the current reg8l value
	and		r2, #0xFF				; Use only the low byte for result
	orr		$reg, r2				; and replace it with r1 shifted to reg8l
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======	
	GLOBAL	rcl_reg8l_1_$reg
rcl_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	and		r1, $reg, #0xFF
	tst		r0, #ARM_CARRY
	lsl		r1, #1					; Shift left 1 bit
	orrne	r1, #1					; Move Carry into the lowest bit
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	bic		$reg, #0xFF				; Clear current reg8l value
	and		r2, r1, #0xFF			; Make sure r1 does not have any extra bits on
	orr		$reg, r2				; And put the new reg value into reg8l
	eor		r1, r1, lsr #1			; Now r1 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r1, #0x180				; Leave only the new Carry and Overflow bits to r1
	orr		r0, r1, lsl #(24-3)		; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND
	MACRO 
	rcl_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcl_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	mov		r2, $reg, lsr #8		; r2 = reg8h value in the lowest byte
	and		r2, #0xFF
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the lowest bit, bit 0x100 will be the new Carry.
	;-------
2	lsl		r2, #1					; Perform the rotate once
	tst		r2, #0x200				; Was the 9th bit set before the rotate?
	orrne	r2, #1					; It was, set the lowest bit
	subs	r1, #1					; One bit handled,
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	and		r2, #0xFF
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r2, lsl #8		; and replace it with r2 shifted to reg8h
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======	
	GLOBAL	rcl_reg8h_1_$reg
rcl_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	mov		r1, $reg, lsr #8
	and		r1, #0xFF
	tst		r0, #ARM_CARRY
	lsl		r1, #1					; Shift left 1 bit
	orrne	r1, #1					; Move Carry into the lowest bit
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	bic		$reg, #0xFF00			; Clear current reg8h value
	and		r2, r1, #0xFF
	orr		$reg, r2, lsl #8		; And put the new reg value into reg8h
	eor		r1, r1, lsr #1			; Now r1 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r1, #0x180				; Leave only the new Carry and Overflow bits to r1
	orr		r0, r1, lsl #(24-3)		; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rcl_al_imm8
	rcl_reg8l_imm8 eax
rcl_cl_imm8
	rcl_reg8l_imm8 ecx
rcl_dl_imm8
	rcl_reg8l_imm8 edx
rcl_bl_imm8
	rcl_reg8l_imm8 ebx
rcl_ah_imm8
	rcl_reg8h_imm8 eax
rcl_ch_imm8
	rcl_reg8h_imm8 ecx
rcl_dh_imm8
	rcl_reg8h_imm8 edx
rcl_bh_imm8
	rcl_reg8h_imm8 ebx

	GLOBAL rcl_al_imm8
	GLOBAL rcl_cl_imm8
	GLOBAL rcl_dl_imm8
	GLOBAL rcl_bl_imm8
	GLOBAL rcl_ah_imm8
	GLOBAL rcl_ch_imm8
	GLOBAL rcl_dh_imm8
	GLOBAL rcl_bh_imm8

; ----- RCR -----

	GLOBAL	rcr_b_r0_bp_imm8
rcr_b_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rcr_b_r0_imm8
rcr_b_r0_imm8					; Rotate a byte at offset r0high in effective segment right by r1 value
	mem_handler_jump_r0r3 rcr_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and also overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rcr_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcr_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	bic		r0, #0xFF				; Clear the low byte of r0 (highest byte contains the saved flags)
	orr		r0, r1					; r0 low byte = number of bit positions to rotate
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r1, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the 8th bit, bit 0 will go to Carry.
	;-------
2	movs	r1, r1, lsr #1			; Perform the rotate once, adjusting real Carry flag
	orrcs	r1, #0x100				; Put the carry into the 9th bit of the value
	sub		r0, #1					; One bit handled,
	tst		r0, #0xFF				; still bits to do?
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Save the result
	;-------
	strb	r1, [r2]
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r1, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the highest bit.
	;=======	
rcr_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	mrs		r0,cpsr					; Save flags to r0
1	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	tst		r0, #ARM_CARRY			; If the input Carry was set, ...
	orrne	r1, #0x100				; ... set the 9th bit of the value to rotate.
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	tst		r1, #1					; If the lowest bit was set in the value to RCR, ...
	orrne	r0, #ARM_CARRY			; ... set the resulting Carry flag
	lsr		r1, #1					; Shift the value
	strb	r1, [r2]				; and save it to memory
	eor		r1, r1, lsl #1			; Now bit 0x80 has the new overflow flag value
	tst		r1, #0x80				; If the new Overflow bit is set, ...
	orrne	r0, #ARM_OVER			; ... set the resulting Overflow flag.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rcr_b, rcr_b_r0, imm8

	MACRO 
	rcr_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcr_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	and		r2, $reg, #0xFF			; r2 = reg8l value in the lowest byte
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the 8th bit, bit 0 will go to Carry.
	;-------
2	movs	r2, r2, lsr #1			; Perform the rotate once, adjusting real Carry flag
	orrcs	r2, #0x100				; Put the carry into the 9th bit of the value
	subs	r1, #1					; One bit handled,
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	bic		$reg, #0xFF				; Clear the current reg8l value
	and		r2, #0xFF				; Use only the low byte for result
	orr		$reg, r2				; and replace it with r1 shifted to reg8l
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the highest bit.
	;=======	
	GLOBAL	rcr_reg8l_1_$reg
rcr_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	and		r1, $reg, #0xFF			; r2 = reg8l value in the lowest byte
	tst		r0, #ARM_CARRY			; If the input Carry was set, ...
	orrne	r1, #0x100				; ... set the 9th bit of the value to rotate.
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; Shift the value
	orrcs	r0, #ARM_CARRY			; Set the resulting Carry flag
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1				; and replace it with r1 shifted to reg8l
	eor		r1, r1, lsl #1			; Now bit 0x80 has the new overflow flag value
	tst		r1, #0x80				; If the new Overflow bit is set, ...
	orrne	r0, #ARM_OVER			; ... set the resulting Overflow flag.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND
	MACRO 
	rcr_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
rcr_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	mov		r2, $reg, lsr #8		; r2 = reg8h value in the lowest byte
	and		r2, #0xFF
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x100				; If it was, set the 9th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the 8th bit, bit 0 will go to Carry.
	;-------
2	movs	r2, r2, lsr #1			; Perform the rotate once, adjusting real Carry flag
	orrcs	r2, #0x100				; Put the carry into the 9th bit of the value
	subs	r1, #1					; One bit handled,
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	and		r2, #0xFF
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r2, lsl #8		; and replace it with r2 shifted to reg8h
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the highest bit.
	;=======	
	GLOBAL	rcr_reg8h_1_$reg
rcr_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	mov		r1, $reg, lsr #8		; r2 = reg8l value in the lowest byte
	and		r1, #0xFF
	tst		r0, #ARM_CARRY			; If the input Carry was set, ...
	orrne	r1, #0x100				; ... set the 9th bit of the value to rotate.
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; Shift the value
	orrcs	r0, #ARM_CARRY			; Set the resulting Carry flag
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsl #8		; and replace it with r1 shifted to reg8h
	eor		r1, r1, lsl #1			; Now bit 0x80 has the new overflow flag value
	tst		r1, #0x80				; If the new Overflow bit is set, ...
	orrne	r0, #ARM_OVER			; ... set the resulting Overflow flag.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rcr_al_imm8
	rcr_reg8l_imm8 eax
rcr_cl_imm8
	rcr_reg8l_imm8 ecx
rcr_dl_imm8
	rcr_reg8l_imm8 edx
rcr_bl_imm8
	rcr_reg8l_imm8 ebx
rcr_ah_imm8
	rcr_reg8h_imm8 eax
rcr_ch_imm8
	rcr_reg8h_imm8 ecx
rcr_dh_imm8
	rcr_reg8h_imm8 edx
rcr_bh_imm8
	rcr_reg8h_imm8 ebx

	GLOBAL rcr_al_imm8
	GLOBAL rcr_cl_imm8
	GLOBAL rcr_dl_imm8
	GLOBAL rcr_bl_imm8
	GLOBAL rcr_ah_imm8
	GLOBAL rcr_ch_imm8
	GLOBAL rcr_dh_imm8
	GLOBAL rcr_bh_imm8

; ----- SHL -----

	GLOBAL	shl_b_r0_bp_imm8
shl_b_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	shl_b_r0_imm8
shl_b_r0_imm8					; Shift a byte at offset r0high in effective segment left by r1 value
	mem_handler_jump_r0r3 shl_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
shl_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
shl_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift left by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shl_byte_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Shift left by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which sould not change)
	ldrb	r0, [r2]				; Get the byte to rotate from RAM
	lsl		r0, #24
	movs	r0, r0, lsl r1			; Perform the shift left, setting the flags
	lsr		r0, #24
	strb	r0, [r2]
	b		loop
	;=======
	; Shift left by 1 bit position, so handle overflow flag as well.
	;=======
shl_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the byte to rotate from RAM
	lsl		r0, #24
	adds	r0, r0					; Perform the shift left, setting all flags
	lsr		r0, #24
	strb	r0, [r2]				; Get the byte to rotate from RAM
	b		loop

	modrm_1_help shl_b, shl_b_r0, imm8

	MACRO 
	shl_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
shl_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift left by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shl_reg8l_1_$reg		; Yes, go handle that case.
	;=======
	; Shift left by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which sould not change)
	mov		r0, $reg, lsl #24		; r0 = reg8l value in highest byte
	movs	r0, r0, lsl r1			; Perform the shift left, setting the flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r0, lsr #24		; and replace it with r0 shifted to reg8l
	b		loop
	;=======
	; Shift left by 1 bit position, so handle overflow flag as well.
	;=======
	GLOBAL	shl_reg8l_1_$reg
shl_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r0, $reg, lsl #24		; r0 = reg8l value in highest byte
	adds	r0, r0					; Perform the shift left, setting all flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r0, lsr #24		; and replace it with r0 shifted to reg8l
	b		loop
	MEND
	MACRO 
	shl_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
shl_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift left by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shl_reg8h_1_$reg		; Yes, go handle that case.
	;=======
	; Shift left by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	mov		r0, $reg, lsl #16		; r0 = reg8l value in highest byte
	and		r0, #0xFF000000			; r0 = reg8h value in highest byte
	movs	r0, r0, lsl r1			; Perform the shift left, setting the flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r0, lsr #16		; and replace it with r0 shifted to reg8h
	b		loop
	;=======
	; Shift left by 1 bit position, so handle overflow flag as well.
	;=======
	GLOBAL	shl_reg8h_1_$reg
shl_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r0, $reg, lsl #16		; r0 = reg8h value in highest byte
	and		r0, #0xFF000000			; r0 = reg8h value in highest byte
	adds	r0, r0					; Perform the shift left, setting all flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r0, lsr #16		; and replace it with r0 shifted to reg8h
	b		loop
	MEND

shl_al_imm8
	shl_reg8l_imm8 eax
shl_cl_imm8
	shl_reg8l_imm8 ecx
shl_dl_imm8
	shl_reg8l_imm8 edx
shl_bl_imm8
	shl_reg8l_imm8 ebx
shl_ah_imm8
	shl_reg8h_imm8 eax
shl_ch_imm8
	shl_reg8h_imm8 ecx
shl_dh_imm8
	shl_reg8h_imm8 edx
shl_bh_imm8
	shl_reg8h_imm8 ebx

	GLOBAL shl_al_imm8
	GLOBAL shl_cl_imm8
	GLOBAL shl_dl_imm8
	GLOBAL shl_bl_imm8
	GLOBAL shl_ah_imm8
	GLOBAL shl_ch_imm8
	GLOBAL shl_dh_imm8
	GLOBAL shl_bh_imm8

; ----- SHR -----

	GLOBAL	shr_b_r0_bp_imm8
shr_b_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	shr_b_r0_imm8
shr_b_r0_imm8					; Shift a byte at offset r0high in effective segment right by r1 value
	mem_handler_jump_r0r3 shr_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
shr_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
shr_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		shr_byte_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]				; Get the byte to rotate from RAM
	movs	r0, r0, lsr r1			; Perform the shift right, setting the flags
	strb	r0, [r2]
	b		loop
	;=======
	; Shift right by 1 bit position, so handle overflow flag as well.
	;=======
shr_byte_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	movs	r1, r1, lsr #1			; Perform the shift right, setting all other flags byt Overflow
	strb	r1, [r2]				; Save the byte back to RAM
	; ----- Setup the Overflow flag
	mrs		r0,cpsr					; Save flags to r0
	tst		r1, #0x40				; This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0

	modrm_1_help shr_b, shr_b_r0, imm8

	MACRO 
	shr_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
shr_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		shr_reg8l_1_$reg		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	and		r0, $reg, #0xFF			; r0 = reg8l value in lowest byte
	movs	r0, r0, lsr r1			; Perform the shift right, setting the flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r0				; and replace it with r0 shifted to reg8l
	b		loop
	;=======
	; Shift right by 1 bit position, so handle overflow flag as well.
	;=======
	GLOBAL	shr_reg8l_1_$reg
shr_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	and		r1, $reg, #0xFF			; r1 = reg8l value in lowest byte
	movs	r1, r1, lsr #1			; Perform the shift right, setting all other flags but Overflow
	bic		$reg, #0xFF				; Clear the current reg8l value
	orr		$reg, r1				; and replace it with r1 shifted to reg8l
	; ----- Setup the Overflow flag
	mrs		r0,cpsr					; Save flags to r0
	tst		r1, #0x40				; This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND
	MACRO 
	shr_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
shr_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		shr_reg8h_1_$reg		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	mov		r0, $reg, lsr #8		; r0 = reg8h value in lowest byte
	and		r0, #0xFF
	movs	r0, r0, lsr r1			; Perform the shift right, setting the flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r0, lsl #8		; and replace it with r0 shifted to reg8h
	b		loop
	;=======
	; Shift right by 1 bit position, so handle overflow flag as well.
	;=======
	GLOBAL	shr_reg8h_1_$reg
shr_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r1, $reg, lsr #8		; r1 = reg8h value in lowest byte
	and		r1, #0xFF
	movs	r1, r1, lsr #1			; Perform the shift right, setting all other flags but Overflow
	bic		$reg, #0xFF00			; Clear the current reg8h value
	orr		$reg, r1, lsl #8		; and replace it with r1 shifted to reg8h
	; ----- Setup the Overflow flag
	mrs		r0,cpsr					; Save flags to r0
	tst		r1, #0x40				; This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

shr_al_imm8
	shr_reg8l_imm8 eax
shr_cl_imm8
	shr_reg8l_imm8 ecx
shr_dl_imm8
	shr_reg8l_imm8 edx
shr_bl_imm8
	shr_reg8l_imm8 ebx
shr_ah_imm8
	shr_reg8h_imm8 eax
shr_ch_imm8
	shr_reg8h_imm8 ecx
shr_dh_imm8
	shr_reg8h_imm8 edx
shr_bh_imm8
	shr_reg8h_imm8 ebx

	GLOBAL shr_al_imm8
	GLOBAL shr_cl_imm8
	GLOBAL shr_dl_imm8
	GLOBAL shr_bl_imm8
	GLOBAL shr_ah_imm8
	GLOBAL shr_ch_imm8
	GLOBAL shr_dh_imm8
	GLOBAL shr_bh_imm8

; ----- SAR -----

	GLOBAL	sar_b_r0_bp_imm8
sar_b_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	sar_b_r0_imm8
sar_b_r0_imm8					; Shift a byte at offset r0high in effective segment right by r1 value
	mem_handler_jump_r0r3 sar_byte_r2_imm8_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
sar_byte_r2_imm8_RAM
	ldrb	r1,[r12],#1				; Get the imm8 byte
sar_byte_r2_r1
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		sar_byte_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]				; Get the byte to rotate from RAM
	lsl		r0, #24					; 
	asr		r0, #24					; Fix all the high bits for asr
	movs	r0, r0, asr r1			; Perform the shift right, setting the flags
	strb	r0, [r2]
	b		loop
	;=======
	; Shift right by 1 bit position, so overflow flag must be cleared.
	;=======
sar_byte_r2_1_RAM
	ldrb	r1, [r2]				; Get the byte to rotate from RAM
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	sbfx	r1, r1, #0, #8			; Fix all the high bits for asr
	movs	r1, r1, asr #1			; Perform the shift right, setting all other flags but Overflow
	strb	r1, [r2]				; Save the byte back to RAM
	b		loop

	modrm_1_help sar_b, sar_b_r0, imm8

	MACRO 
	sar_reg8l_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
sar_r8l_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		sar_reg8l_1_$reg		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	mov		r0, $reg, lsl #24		; r0 = reg8l value in highest byte
	asr		r0, #24					; Fix all the high bits for asr
	movs	r0, r0, asr r1			; Perform the shift right, setting the flags
	bic		$reg, #0xFF				; Clear the current reg8l value
	and		r0, #0xFF
	orr		$reg, r0				; and replace it with r0 shifted to reg8l
	b		loop
	;=======
	; Shift right by 1 bit position, so overflow flag must be cleared.
	;=======
	GLOBAL	sar_reg8l_1_$reg
sar_reg8l_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	sbfx	r0, $reg, #0, #8		; r0 = signed reg8l value
	movs	r0, r0, asr #1			; Perform the shift right, setting all other flags but Overflow
	bfi		$reg, r0, #0, #8		; and replace it with r0 shifted to reg8l
	b		loop
	MEND
	MACRO 
	sar_reg8h_imm8 $reg
	ldrb	r1,[r12],#1				; Get the imm8 byte
sar_r8h_r1_$reg
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		sar_reg8h_1_$reg		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	mov		r0, $reg, lsl #16		; Fix all the high bits for asr
	asr		r0, #24
	movs	r0, r0, asr r1			; Perform the shift right, setting the flags
	bic		$reg, #0xFF00			; Clear the current reg8h value
	and		r0, #0xFF
	orr		$reg, r0, lsl #8		; and replace it with r0 shifted to reg8h
	b		loop
	;=======
	; Shift right by 1 bit position, so overflow flag must be cleared.
	;=======
	GLOBAL	sar_reg8h_1_$reg
sar_reg8h_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	mov		r0, $reg, lsl #16		; Fix all the high bits for asr
	asr		r0, #24
	movs	r0, r0, asr #1			; Perform the shift right, setting all other flags but Overflow
	bic		$reg, #0xFF00			; Clear the current reg8h value
	and		r0, #0xFF
	orr		$reg, r0, lsl #8		; and replace it with r0 shifted to reg8h
	b		loop
	MEND

sar_al_imm8
	sar_reg8l_imm8 eax
sar_cl_imm8
	sar_reg8l_imm8 ecx
sar_dl_imm8
	sar_reg8l_imm8 edx
sar_bl_imm8
	sar_reg8l_imm8 ebx
sar_ah_imm8
	sar_reg8h_imm8 eax
sar_ch_imm8
	sar_reg8h_imm8 ecx
sar_dh_imm8
	sar_reg8h_imm8 edx
sar_bh_imm8
	sar_reg8h_imm8 ebx

	GLOBAL sar_al_imm8
	GLOBAL sar_cl_imm8
	GLOBAL sar_dl_imm8
	GLOBAL sar_bl_imm8
	GLOBAL sar_ah_imm8
	GLOBAL sar_ch_imm8
	GLOBAL sar_dh_imm8
	GLOBAL sar_bh_imm8

; ------------------- C1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,imm8 ---
; 
; Note: This instruction is only available on 80186 and up.
;
; SHR BX,4 = C1EB04 => mod = 11, reg = 101 (SHR), rm = 011 (BX)
; rol word [si],08 = C10408
;
; mod = whether we use reg or mem
; reg = the operation (010 = 2 = RCL, 101 = 5 = SHR)
; rm = register to use if mod == 11b, 001 = CX
;
; We must also setup the flags!
; - RCL/RCR/ROL/ROR change only the carry flag.
; - SHL/SHR/SHL/SAR change carry, sign and zero flags (ARM behaves exactly like 8086)
;
	GLOBAL	op_c1
op_c1
	modrm_jump_16_tbl op_c1_jump
; 0 (idx only)
	modrm_help_1_0 rol_16, imm8
	modrm_help_1_0 ror_16, imm8
	modrm_help_1_0 rcl_16, imm8
	modrm_help_1_0 rcr_16, imm8
	modrm_help_1_0 shl_16, imm8
	modrm_help_1_0 shr_16, imm8
	modrm_help_1_0 shl_16, imm8
	modrm_help_1_0 sar_16, imm8
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_16, imm8
	modrm_help_1_40 ror_16, imm8
	modrm_help_1_40 rcl_16, imm8
	modrm_help_1_40 rcr_16, imm8
	modrm_help_1_40 shl_16, imm8
	modrm_help_1_40 shr_16, imm8
	modrm_help_1_40 shl_16, imm8
	modrm_help_1_40 sar_16, imm8
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_16, imm8
	modrm_help_1_80 ror_16, imm8
	modrm_help_1_80 rcl_16, imm8
	modrm_help_1_80 rcr_16, imm8
	modrm_help_1_80 shl_16, imm8
	modrm_help_1_80 shr_16, imm8
	modrm_help_1_80 shl_16, imm8
	modrm_help_1_80 sar_16, imm8
;0xc0 = mod = 11b => two register operands (use the same handlers as opcode d3)
	DCD rol_ax_imm8, rol_cx_imm8, rol_dx_imm8, rol_bx_imm8, rol_sp_imm8, rol_bp_imm8, rol_si_imm8, rol_di_imm8
	DCD ror_ax_imm8, ror_cx_imm8, ror_dx_imm8, ror_bx_imm8, ror_sp_imm8, ror_bp_imm8, ror_si_imm8, ror_di_imm8
	DCD rcl_ax_imm8, rcl_cx_imm8, rcl_dx_imm8, rcl_bx_imm8, rcl_sp_imm8, rcl_bp_imm8, rcl_si_imm8, rcl_di_imm8
	DCD rcr_ax_imm8, rcr_cx_imm8, rcr_dx_imm8, rcr_bx_imm8, rcr_sp_imm8, rcr_bp_imm8, rcr_si_imm8, rcr_di_imm8
	DCD shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	DCD shr_ax_imm8, shr_cx_imm8, shr_dx_imm8, shr_bx_imm8, shr_sp_imm8, shr_bp_imm8, shr_si_imm8, shr_di_imm8
	DCD shl_ax_imm8, shl_cx_imm8, shl_dx_imm8, shl_bx_imm8, shl_sp_imm8, shl_bp_imm8, shl_si_imm8, shl_di_imm8
	DCD sar_ax_imm8, sar_cx_imm8, sar_dx_imm8, sar_bx_imm8, sar_sp_imm8, sar_bp_imm8, sar_si_imm8, sar_di_imm8

	AREA cpu_code, CODE, READONLY
	ALIGN	4
	
	LTORG

; ----- ROL -----

	GLOBAL	rol_16_r0_bp_imm8
rol_16_r0_bp_imm8					; Rotate a word when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rol_16_r0_imm8
rol_16_r0_imm8						; Rotate a word at offset r0high in effective segment left by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 rol_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the word to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rol_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, or 16 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #15					; Are we to shift by 16 bits?
	beq		%f16					; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..15 bit positions.
	;=======
	lsr		r0, #24					; r0 = flags in the lowest byte
	rsb		r1, #16					; r1 = 16-rol_count == ror_count
	orr		r0, r1, r0, ror #24		; Now r0 low byte is the number of bits to rotate the byte right, second lowest byte = flags
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	bic		r0, #(ARM_CARRY>>16)	; Clear the Carry bit of the saved CPU flags
	orr		r0, r1, lsl #24			; Save it to r0 high byte for a while
	ldrb	r1, [r2]				; Get the low byte to rotate from RAM
	orr		r1, r0, lsr #16			; Join the low and high bytes to r1
	mov		r1, r1, ror r0			; Rotate the value
	orr		r1, r1, lsr #16			; Move the bits we rotated from the bottom back to the low halfword
	lsl		r0, #16					; Put the flags back to the top byte of r0
	strb	r1, [r2]				; Save the low byte back to RAM
	tst		r1, #1					; Is the lowest bit set (means we need to set Carry)?
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte back to RAM
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
1	msr		cpsr_f,r0				; Restore flags from r0
rol_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	lsl		r0, #1					; Shift the low byte
	orr		r1, r0, r1, lsl #9		; r1 = the result, with bit 0x00010000 needed to move to bit 0x00000001
	mrs		r0,cpsr					; Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	tst		r1, #0x00010000			; Should the carry and lowest bit be set?
	orrne	r1, #1					; Yes, so set the lowest bit, ...
	orrne	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	strb	r1, [r2]				; Save the low byte of the value
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte of the value
	eor		r1, r0, r1, lsl #(24-2)	; Now r0 ARM_CARRY bit = new Overflow bit value
	and		r1, #ARM_CARRY			; Leave only the ARM_CARRY bit into r0
	orr		r0, r1, lsr #1			; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 16 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the low bit of the value.
	;=======
16	ldrb	r1, [r2]				; Get the low byte of the value to rotate from RAM
	bic		r0, #ARM_CARRY
	tst		r1, #1
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rol_16, rol_16_r0, imm8

	MACRO 
	rol_reg16_imm8 $reg
	ldrb	r1, [r12], #1
rol_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, or 16 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #15					; Are we to shift by a number divisible by 16, yet not zero bits?
	beq		%f16					; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..15 bit positions.
	;=======
	rsb		r1, #16					; r1 = 16-rol_count == ror_count
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	orr		r2, r2, lsr #16			; r2 = register value in both low and high 16 bits
	mov		r2, r2, ror r1			; Rotate the register value
	orr		$reg, r2, lsr #16		; Set the register value
16	tst		$reg, #1				; Is the lowest bit set (means we need to set Carry)?
	biceq	r0, #ARM_CARRY			; No, so clear it.
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
	GLOBAL	rol_reg16_1_$reg
rol_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	movs	r2, r2, lsl #1			; First shift the register left 1 bit position, and set the flags
	orrcs	r2, #0x00010000			; If Carry is set, set the lowest bit of the register, ...
	orr		$reg, r2, lsr #16		; Set the register value
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	eor		r1, r0, r2, lsr #2		; Now r1 ARM_CARRY bit = new Overflow bit value
	and		r1, #ARM_CARRY			; Leave only the ARM_CARRY bit into r1
	orr		r0, r1, lsr #1			; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rol_ax_imm8
	rol_reg16_imm8 eax
rol_cx_imm8
	rol_reg16_imm8 ecx
rol_dx_imm8
	rol_reg16_imm8 edx
rol_bx_imm8
	rol_reg16_imm8 ebx
rol_sp_imm8
	rol_reg16_imm8 esp
rol_bp_imm8
	rol_reg16_imm8 ebp
rol_si_imm8
	rol_reg16_imm8 esi
rol_di_imm8
	rol_reg16_imm8 edi

	GLOBAL  rol_ax_imm8
	GLOBAL  rol_cx_imm8
	GLOBAL  rol_dx_imm8
	GLOBAL  rol_bx_imm8
	GLOBAL  rol_sp_imm8
	GLOBAL  rol_bp_imm8
	GLOBAL  rol_si_imm8
	GLOBAL  rol_di_imm8


; ----- ROR -----

	GLOBAL	ror_16_r0_bp_imm8
ror_16_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	ror_16_r0_imm8
ror_16_r0_imm8					; Rotate a byte at offset r0high in effective segment right by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 ror_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
ror_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, or 16 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #15					; Are we to shift by a number divisible by 16, yet not zero bits?
	beq		%f16					; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..15 bit positions.
	;=======
	lsr		r0, #24					; r0 = flags in the lowest byte
	orr		r0, r1, r0, ror #24		; Now r0 low byte is the number of bits to rotate the byte right, second lowest byte = flags
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	bic		r0, #(ARM_CARRY>>16)	; Clear the Carry bit of the saved CPU flags
	orr		r0, r1, lsl #24			; Save it to r0 high byte for a while
	ldrb	r1, [r2]				; Get the low byte to rotate from RAM
	orr		r1, r0, lsr #16			; Join the low and high bytes to r1
	movs	r1, r1, ror r0			; Rotate the value, setting the carry flag
	orr		r1, r1, lsr #16			; Move the bits we rotated from the bottom back to the low halfword
	lsl		r0, #16					; Put the flags back to the top byte of r0
	strb	r1, [r2]				; Save the low byte back to RAM
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte back to RAM
	orrcs	r0, #ARM_CARRY			; If Carry is set, set it in the saved flags as well.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
1	msr		cpsr_f,r0				; Restore flags from r0
ror_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	orr		r1, r0, r1, lsl #8		; r1 = the value to rotate
	mrs		r0,cpsr					; Save flags to r0
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r1, r1, lsr #1			; First shift the register right 1 bit position, and set the flags
	orrcs	r1, #0x8000				; If Carry is set, set the highest bit of the register, ...
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	strb	r1, [r2]				; Save the low byte of the shifted value
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte of the shifted value
	eor		r1, r1, lsl #1			; Now r1 bit 0x80 = new Overflow bit value
	and		r1, #0x80				; Leave only the highest bit into r1
	orr		r0, r1, lsl #(24-3)		; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by 16 bit positions, so the value will stay unchanged
	; but the carry flag needs to be set to the high bit of the byte value.
	;=======
16	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	bic		r0, #ARM_CARRY
	tst		r1, #0x80
	orrne	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help ror_16, ror_16_r0, imm8
	
	MACRO 
	ror_reg16_imm8 $reg
	ldrb	r1, [r12], #1
ror_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0, 1, or 16 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	ands	r1, #15					; Are we to shift by a number divisible by 16, yet not zero bits?
	beq		%f16					; Yes, go handle that special case.
	;=======
	; Rotate the value by 2..15 bit positions.
	;=======
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16
	orr		r2, r2, lsr #16
	mov		r2, r2, ror r1			; Rotate the register value
	orr		$reg, r2, lsr #16
16	tst		$reg, #0x8000			; Is the highest bit set (means we need to set Carry)?
	biceq	r0, #ARM_CARRY			; No, so clear it.
	orrne	r0, #ARM_CARRY			; Yep, so set it.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position.
	;=======	
	GLOBAL	ror_reg16_1_$reg
ror_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
1	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16
	movs	r2, r2, lsr #17			; First shift the register right 1 bit position, and set the flags
	orrcs	r2, #0x8000				; If Carry is set, set the highest bit of the register, ...
	orr		$reg, r2
	orrcs	r0, #ARM_CARRY			; ... and set the Carry bit of the CPU flags (Carry bit is 0x20000000) in r0
	eor		r1, r2, r2, lsl #1		; Now r1 bit 0x8000 = new Overflow bit value
	and		r1, #0x8000				; Leave only the highest bit into r1
	orr		r0, r1, lsl #(16-3)		; and put it into CPU Overflow flag (Overflow bit is 0x10000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

ror_ax_imm8
	ror_reg16_imm8 eax
ror_cx_imm8
	ror_reg16_imm8 ecx
ror_dx_imm8
	ror_reg16_imm8 edx
ror_bx_imm8
	ror_reg16_imm8 ebx
ror_sp_imm8
	ror_reg16_imm8 esp
ror_bp_imm8
	ror_reg16_imm8 ebp
ror_si_imm8
	ror_reg16_imm8 esi
ror_di_imm8
	ror_reg16_imm8 edi

	GLOBAL  ror_ax_imm8
	GLOBAL  ror_cx_imm8
	GLOBAL  ror_dx_imm8
	GLOBAL  ror_bx_imm8
	GLOBAL  ror_sp_imm8
	GLOBAL  ror_bp_imm8
	GLOBAL  ror_si_imm8
	GLOBAL  ror_di_imm8

; ----- RCL -----

	GLOBAL	rcl_16_r0_bp_imm8
rcl_16_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rcl_16_r0_imm8
rcl_16_r0_imm8					; Rotate a byte at offset r0high in effective segment left by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 rcl_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rcl_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	orr		r0, r1, r0, lsr #16		; Now r0 low byte is the number of bits to rotate the byte right, second lowest byte = flags
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	orr		r0, r1, lsl #24			; Save it to r0 high byte for a while
	ldrb	r1, [r2]				; Get the low byte to rotate from RAM
	orr		r1, r0, lsr #16			; Join the low and high bytes to r1
	lsl		r0, #16					; Put the flags back to the top byte of r0
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r1, #0x10000			; If it was, set the 17th bit of the value
	;-------
	; Perform a 17-bit value rotate. Carry goes to the lowest bit, bit 0x10000 will be the new Carry.
	;-------
2	lsl		r1, #1					; Perform the rotate once
	tst		r1, #0x20000			; Was the 17th bit set before the rotate?
	orrne	r1, #1					; It was, set the lowest bit
	sub		r0, #0x010000			; One bit handled,
	tst		r0, #0xFF0000			; still bits to do?
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Save the result
	;-------
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r1, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======	
1	msr		cpsr_f,r0				; Restore flags from r0
rcl_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	lsl		r0, #1					; Shift the low byte
	orr		r1, r0, r1, lsl #9		; r1 = the result, with bit 0x00010000 telling the new Carry flag
	orrcs	r1, #1					; Lowest bit set from the Carry flag
	mrs		r0,cpsr					; Save flags to r0
	strb	r1, [r2]				; Save the shifted value
	lsr		r1, #8
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	strb	r1, [r2, #1]			; Save the shifted value
	eor		r1, r1, lsr #1			; Now r1 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r1, #0x180				; Leave only the new Carry and Overflow bits to r1
	orr		r0, r1, lsl #(24-3)		; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rcl_16, rcl_16_r0, imm8

	MACRO 
	rcl_reg16_imm8 $reg
	ldrb	r1, [r12], #1
rcl_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x8000				; If it was, set the 17th bit of the value
	;-------
	; Perform a 9-bit value rotate. Carry goes to the lowest bit, bit 0x100 will be the new Carry.
	;-------
2	movs	r2, r2, lsl #1			; Perform the rotate once, setting the Carry flag
	orrcs	r2, #0x8000				; It Carry got set, set the (low) 17th bit
	subs	r1, #1					; One bit handled, more bits to do?
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x8000
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	orr		$reg, r2, lsr #16
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======	
	GLOBAL	rcl_reg16_1_$reg
rcl_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
1	lsr		r2, #16
	tst		r0, #ARM_CARRY
	lsl		r2, #1					; Shift left 1 bit
	orrne	r2, #1					; Move Carry into the lowest bit
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	eor		r1, r2, r2, lsr #1		; Now r1 bit 0x10000 is the new Carry flag, bit 0x8000 is the new Overflow flag
	and		r1, #0x18000			; Leave only the new Carry and Overflow bits to r1
	orr		r0, r1, lsl #(16-3)		; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	lsl		r2, #16
	orr		$reg, r2, lsr #16
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rcl_ax_imm8
	rcl_reg16_imm8 eax
rcl_cx_imm8
	rcl_reg16_imm8 ecx
rcl_dx_imm8
	rcl_reg16_imm8 edx
rcl_bx_imm8
	rcl_reg16_imm8 ebx
rcl_sp_imm8
	rcl_reg16_imm8 esp
rcl_bp_imm8
	rcl_reg16_imm8 ebp
rcl_si_imm8
	rcl_reg16_imm8 esi
rcl_di_imm8
	rcl_reg16_imm8 edi

	GLOBAL  rcl_ax_imm8
	GLOBAL  rcl_cx_imm8
	GLOBAL  rcl_dx_imm8
	GLOBAL  rcl_bx_imm8
	GLOBAL  rcl_sp_imm8
	GLOBAL  rcl_bp_imm8
	GLOBAL  rcl_si_imm8
	GLOBAL  rcl_di_imm8

; ----- RCR -----

	GLOBAL	rcr_16_r0_bp_imm8
rcr_16_r0_bp_imm8				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	rcr_16_r0_imm8
rcr_16_r0_imm8					; Rotate a byte at offset r0high in effective segment right by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 rcr_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; We can only change the Carry flag, and also overflow flag if r1 == 1. All other flags must remain unchanged.
	;-------
rcr_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	orr		r0, r1, r0, lsr #16		; Now r0 low byte is the number of bits to rotate the byte right, second lowest byte = flags
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	orr		r0, r1, lsl #24			; Save it to r0 high byte for a while
	ldrb	r1, [r2]				; Get the low byte to rotate from RAM
	orr		r1, r0, lsr #16			; Join the low and high bytes to r1
	lsl		r0, #16					; Put the flags back to the top byte of r0
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r1, #0x10000			; If it was, set the 17th bit of the value
	;-------
	; Perform a 17-bit value rotate. Carry goes to the lowest bit, bit 0x10000 will be the new Carry.
	;-------
2	movs	r1, r1, lsr #1			; Perform the rotate once, adjusting real Carry flag
	orrcs	r1, #0x10000			; Put the carry into the 17th bit of the value
	sub		r0, #0x010000			; One bit handled,
	tst		r0, #0xFF0000			; still bits to do?
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Save the result
	;-------
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r1, #0x100
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the highest bit.
	;=======	
1	msr		cpsr_f,r0				; Restore flags from r0
rcr_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the low byte to rotate from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to rotate from RAM
	orr		r1, r0, r1, lsl #8		; r1 = the result, with bit 0x00010000 telling the new Carry flag
	mrs		r0,cpsr					; Save flags to r0
	orrcs	r1, #0x10000			; If input Carry was set, set the 17th bit of the value.
	movs	r1, r1, lsr #1			; Rotate the value, setting the Carry flag
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	orrcs	r0, #ARM_CARRY			; Set the resulting Carry flag if the lowest bit was set
	strb	r1, [r2]				; Save the shifted value
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the shifted value
	eor		r1, r1, lsl #1			; Now bit 0x80 has the new overflow flag value
	tst		r1, #0x80				; If the new Overflow bit is set, ...
	orrne	r0, #ARM_OVER			; ... set the resulting Overflow flag.
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	modrm_1_help rcr_16, rcr_16_r0, imm8

	MACRO 
	rcr_reg16_imm8 $reg
	ldrb	r1, [r12], #1
rcr_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (rotate by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	beq		%f1						; Yes, go handle that case.
	;=======
	; Rotate the value by 2..31 bit positions.
	;=======
	lsr		r2, #16
	tst		r0, #ARM_CARRY			; Was the input Carry set?
	orrne	r2, #0x10000			; If it was, set the 17th bit of the value
	;-------
	; Perform a 17-bit value rotate. Carry goes to the 16th bit, bit 0 will go to Carry.
	;-------
2	movs	r2, r2, lsr #1		; Perform the rotate once, adjusting real Carry flag
	orrcs	r2, #0x10000			; Put the carry into the 17th bit of the value
	subs	r1, #1					; One bit handled,
	bne		%b2						; Yep, handle the remaining bits.
	;-------
	; Fix the resulting Carry flag
	;-------
	tst		r2, #0x10000
	orrne	r0, #ARM_CARRY
	biceq	r0, #ARM_CARRY
	;-------
	; Save the result
	;-------
	lsl		r2, #16
	orr		$reg, r2, lsr #16
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	;=======
	; Rotate by a single bit position, putting current Carry flag into the highest bit.
	;=======	
	GLOBAL	rcr_reg16_1_$reg
rcr_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mrs		r0,cpsr					; Save flags to r0
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
1	lsr		r2, #16
	tst		r0, #ARM_CARRY			; If the input Carry was set, ...
	orrne	r2, #0x10000			; ... set the 17th bit of the value to rotate.
	bic		r0, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	movs	r2, r2, lsr #1		; Shift the value
	orrcs	r0, #ARM_CARRY			; Set the resulting Carry flag
	eor		r1, r2, r2, lsl #1	; Now bit 0x8000 has the new overflow flag value
	tst		r1, #0x8000				; If the new Overflow bit is set, ...
	orrne	r0, #ARM_OVER			; ... set the resulting Overflow flag.
	lsl		r2, #16
	orr		$reg, r2, lsr #16
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	MEND

rcr_ax_imm8
	rcr_reg16_imm8 eax
rcr_cx_imm8
	rcr_reg16_imm8 ecx
rcr_dx_imm8
	rcr_reg16_imm8 edx
rcr_bx_imm8
	rcr_reg16_imm8 ebx
rcr_sp_imm8
	rcr_reg16_imm8 esp
rcr_bp_imm8
	rcr_reg16_imm8 ebp
rcr_si_imm8
	rcr_reg16_imm8 esi
rcr_di_imm8
	rcr_reg16_imm8 edi

	GLOBAL  rcr_ax_imm8
	GLOBAL  rcr_cx_imm8
	GLOBAL  rcr_dx_imm8
	GLOBAL  rcr_bx_imm8
	GLOBAL  rcr_sp_imm8
	GLOBAL  rcr_bp_imm8
	GLOBAL  rcr_si_imm8
	GLOBAL  rcr_di_imm8

; ----- SHL -----

	GLOBAL	shl_16_r0_bp_imm8
shl_16_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	shl_16_r0_imm8
shl_16_r0_imm8					; Shift a byte at offset r0high in effective segment left by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 shl_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
shl_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift left by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shl_word_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Shift left by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2]				; Get the low byte to shift from RAM
	orr		r1, r0, r1, ror #8
	ldrb	r0, [r2, #1]			; Get the high byte to shift from RAM
	orr		r1, r0, lsl #8			; Now r1 high byte = the number of bits to shift, r1 low halfword = value to shift
	mov		r0, r1, lsr #24			; r0 = number of bits to shift the value left
	lsl		r1, #16					; r1 = value to shift in high halfword
	movs	r1, r1, lsl r0			; Perform the shift left, setting the flags
	lsr		r1, #16
	strb	r1, [r2]
	lsr		r1, #8
	strb	r1, [r2, #1]
	b		loop
	;=======
	; Shift left by 1 bit position, so handle overflow flag as well.
	;=======
shl_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the byte to rotate from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to shift from RAM
	lsl		r0, #16
	orr		r0, r1, lsl #24
	adds	r0, r0					; Perform the shift left, setting all flags
	lsr		r0, #16
	strb	r0, [r2]				; Save the low byte
	lsr		r0, #8
	strb	r0, [r2, #1]			; Save the high byte
	b		loop

	modrm_1_help shl_16, shl_16_r0, imm8

	MACRO 
	shl_reg16_imm8 $reg
	ldrb	r1, [r12], #1
shl_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift left by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to rotate by a single bit position (so we need to handle overflow flag as well)?
	beq		shl_reg16_1_$reg		; Yes, go handle that case. (See "add_ax_ax" etc)
	;=======
	; Shift left by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	movs	r2, r2, lsl r1			; Perform the shift left, setting the flags
	orr		$reg, r2, lsr #16
	b		loop
	MEND

shl_ax_imm8
	shl_reg16_imm8 eax
shl_cx_imm8
	shl_reg16_imm8 ecx
shl_dx_imm8
	shl_reg16_imm8 edx
shl_bx_imm8
	shl_reg16_imm8 ebx
shl_sp_imm8
	shl_reg16_imm8 esp
shl_bp_imm8
	shl_reg16_imm8 ebp
shl_si_imm8
	shl_reg16_imm8 esi
shl_di_imm8
	shl_reg16_imm8 edi

	GLOBAL  shl_ax_imm8
	GLOBAL  shl_cx_imm8
	GLOBAL  shl_dx_imm8
	GLOBAL  shl_bx_imm8
	GLOBAL  shl_sp_imm8
	GLOBAL  shl_bp_imm8
	GLOBAL  shl_si_imm8
	GLOBAL  shl_di_imm8

; ----- SHR -----

	GLOBAL	shr_16_r0_bp_imm8
shr_16_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	shr_16_r0_imm8
shr_16_r0_imm8					; Shift a byte at offset r0high in effective segment right by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 shr_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
shr_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		shr_word_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2, #1]			; Get the high byte to shift from RAM
	orr		r1, r0, lsl #24			; Now r1 high byte = high byte of value to shift
	ldrb	r0, [r2]				; Get the low byte to shift from RAM
	orr		r0, r1, lsr #16			; r0 = 16-bit value to shift in low halfword
	and		r1, #31					; r1 = number of bits to shift the value
	movs	r0, r0, lsr r1			; Perform the shift, setting the flags
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	b		loop
	;=======
	; Shift right by 1 bit position, so handle overflow flag as well.
	;=======
shr_word_r2_1_RAM					; Directly jump here from single-bit-shift opcodes
	ldrb	r0, [r2]				; Get the low byte to shift from RAM
	ldrb	r1, [r2, #1]			; Get the high byte to shift from RAM
	orr		r1, r0, r1, lsl #8
	movs	r1, r1, lsr #1			; Perform the shift right, setting all other flags but Overflow
	strb	r1, [r2]				; Save the low byte
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte
	; ----- Setup the Overflow flag
	mrs		r0,cpsr					; Save flags to r0
	tst		r1, #0x40				; This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0

	modrm_1_help shr_16, shr_16_r0, imm8

	MACRO 
	shr_reg16_imm8 $reg
	ldrb	r1, [r12], #1
shr_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	beq		%f1						; Yes, go handle that case.
	;=======
	; Shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	lsr		r2, #16
	movs	r2, r2, lsr r1			; Perform the shift right, setting the flags
	orr		$reg, r2
	b		loop
	;=======
	; Shift right by 1 bit position, so handle overflow flag as well.
	;=======
	GLOBAL	shr_reg16_1_$reg
shr_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
1	lsr		r2, #16
	movs	r2, r2, lsr #1			; Perform the shift right, setting the flags
	orr		$reg, r2
	; ----- Setup the Overflow flag
	mrs		r0,cpsr					; Save flags to r0
	tst		$reg, #0x4000			; This is the new Overflow flag
	biceq	r0, #ARM_OVER
	orrne	r0, #ARM_OVER
	b		restore_flags_from_r0
	MEND

shr_ax_imm8
	shr_reg16_imm8 eax
shr_cx_imm8
	shr_reg16_imm8 ecx
shr_dx_imm8
	shr_reg16_imm8 edx
shr_bx_imm8
	shr_reg16_imm8 ebx
shr_sp_imm8
	shr_reg16_imm8 esp
shr_bp_imm8
	shr_reg16_imm8 ebp
shr_si_imm8
	shr_reg16_imm8 esi
shr_di_imm8
	shr_reg16_imm8 edi

	GLOBAL  shr_ax_imm8
	GLOBAL  shr_cx_imm8
	GLOBAL  shr_dx_imm8
	GLOBAL  shr_bx_imm8
	GLOBAL  shr_sp_imm8
	GLOBAL  shr_bp_imm8
	GLOBAL  shr_si_imm8
	GLOBAL  shr_di_imm8

; ----- SAR -----

	GLOBAL	sar_16_r0_bp_imm8
sar_16_r0_bp_imm8				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	sar_16_r0_imm8
sar_16_r0_imm8					; Shift a byte at offset r0high in effective segment right by r1 value
	ldrb	r1, [r12], #1			; r1 = imm8 value to shift/rotate with
	mem_handler_jump_r0r3 sar_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; On input
	;	r0 = free
	;	r1 = number of bits to rotate the value with
	;	r2 = address of the byte to rotate
	; Zero, Sign and Carry flags are set always, Overflow only if r1 == 1.
	;-------
sar_word_r2_r1_RAM
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	beq		sar_word_r2_1_RAM		; Yes, go handle that case.
	;=======
	; Arithmetic shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	ldrb	r0, [r2, #1]			; Get the high byte to shift from RAM
	orr		r1, r0, lsl #24			; Now r1 high byte = high byte of value to shift
	ldrb	r0, [r2]				; Get the low byte to shift from RAM
	orr		r0, r1, asr #16			; r0 = 16-bit value to shift in low halfword
	and		r1, #31					; r1 = number of bits to shift the value
	movs	r0, r0, asr r1			; Perform the shift, setting the flags
	strb	r0, [r2]
	lsr		r0, #8
	strb	r0, [r2, #1]
	b		loop
	;=======
	; Arithmetic shift right by 1 bit position, so overflow flag must be cleared.
	;=======
sar_word_r2_1_RAM
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	ldrb	r1, [r2, #1]			; Get the high byte to shift from RAM
	ldrb	r0, [r2]				; Get the low byte to shift from RAM
	lsl		r1, #24
	orr		r1, r0, r1, asr #16
	movs	r1, r1, asr #1			; Perform the shift right, setting all other flags but Overflow
	strb	r1, [r2]				; Save the low byte
	lsr		r1, #8
	strb	r1, [r2, #1]			; Save the high byte
	b		loop

	modrm_1_help sar_16, sar_16_r0, imm8

	MACRO 
	sar_reg16_imm8 $reg
	ldrb	r1, [r12], #1
sar_reg16_r1_$reg					; Directly jump here for CL-based opcodes
	mrs		r0,cpsr					; Save flags to r0
	;=======
	; Check for special cases (shift by 0 or 1 bit positions)
	;=======
	ands	r1, #31					; Mask the rotation count
	beq		restore_flags_from_r0	; If the masked rotation count == 0, jump back to loop, restoring the flags.
	cmp		r1, #1					; Are we to shift by a single bit position (so we need to handle overflow flag as well)?
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
	beq		%f1						; Yes, go handle that case.
	;=======
	; Arithmetic shift right by 2..31 bit positions.
	;=======
	msr		cpsr_f,r0				; Restore flags (for Overflow flag, which should not change)
	asr		r2, #16
	movs	r2, r2, asr r1			; Perform the shift right, setting the flags
	lsl		r2, #16
	orr		$reg, r2, lsr #16
	b		loop
	;=======
	; Arithmetic shift right by 1 bit position, so overflow flag must be cleared.
	;=======
	GLOBAL	sar_reg16_1_$reg
sar_reg16_1_$reg					; Directly jump here for single-bit-rotate opcodes
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clean the current register value
1	asr		r2, #16
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially Overflow)
	movs	r2, r2, asr #1			; Perform the shift right, setting all other flags but Overflow
	lsl		r2, #16
	orr		$reg, r2, lsr #16
	b		loop
	MEND

sar_ax_imm8
	sar_reg16_imm8 eax
sar_cx_imm8
	sar_reg16_imm8 ecx
sar_dx_imm8
	sar_reg16_imm8 edx
sar_bx_imm8
	sar_reg16_imm8 ebx
sar_sp_imm8
	sar_reg16_imm8 esp
sar_bp_imm8
	sar_reg16_imm8 ebp
sar_si_imm8
	sar_reg16_imm8 esi
sar_di_imm8
	sar_reg16_imm8 edi

	GLOBAL  sar_ax_imm8
	GLOBAL  sar_cx_imm8
	GLOBAL  sar_dx_imm8
	GLOBAL  sar_bx_imm8
	GLOBAL  sar_sp_imm8
	GLOBAL  sar_bp_imm8
	GLOBAL  sar_si_imm8
	GLOBAL  sar_di_imm8
	
; ------------------- C2 = RETN imm16 ---------------------------------
op_c2
	ldrb	r1,[r12],#1				; Load byte to r1, increment r12 by 1
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	ldr		r2,[sp, #SP_PHYS_CS]	; get the physical CS:0000 into r2
	orr		r1, r0, lsl #8			; r1 = low byte | (high byte << 8)
	pop_reg_low_tmp	r12, r0
	add		r1, esp
	bfi		esp, r1, #0, #16
	add		r12, r2					; r12 = new IP + physical CS
	b		loop

; ------------------- C3 = RETN ---------------------------------------
op_c3
	ldr		r2,[sp, #SP_PHYS_CS]	; get the physical CS:0000 into r2
	pop_reg_low_tmp	r12, r0
	add		r12, r2					; r12 = new IP + physical CS
	b		loop


	ALIGN	4
	
; ------------------- C4 = LES r16,m16:16 -----------------------------
;
; All modrm variations supported!
;
op_c4
	modrm_jump_16_tbl op_c4_jump
	modrm_tbl_3_old les
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	les_r16_r0_reg $reg
	GLOBAL	les_r16_r0_bp_$reg
les_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	les_r16_r0_$reg
les_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 les_r2_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
les_r2_RAM_$reg
	; ----- Load offset and segment
	ldrb	r0, [r2] 					; Load low byte of offset
	ldrb	r1, [r2, #1]				; Load high byte of offset
	ldrb	r3, [r2, #2]				; Load low byte of ES from [r2+2]
	ldrb	r2, [r2, #3]				; Load high byte of ES from [r2+3]
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	orr		r2, r3, r2, lsl #8			; r2 = low byte | (high byte << 8) = new ES value
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_es_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_ES_VALUE]
	str		r1, [sp, #SP_ES_BASE]
	b		restore_flags_from_r0
	MEND

	les_r16_r0_reg eax
	les_r16_r0_reg ecx
	les_r16_r0_reg edx
	les_r16_r0_reg ebx
	les_r16_r0_reg esp
	les_r16_r0_reg ebp
	les_r16_r0_reg esi
	les_r16_r0_reg edi

	LTORG
	
	modrm_3_genall_old les, les_r16_r0
	
; ------------------- C5 = LDS r16,m16:16 -----------------------------
;
; All modrm variations (except SP) supported!
;
op_c5
	modrm_jump_16_tbl op_c5_jump
	modrm_tbl_3_old lds
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	lds_r16_r0_reg $reg
	GLOBAL	lds_r16_r0_bp_$reg
lds_r16_r0_bp_$reg
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	lds_r16_r0_$reg
lds_r16_r0_$reg
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 lds_r2_RAM_$reg, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
lds_r2_RAM_$reg
	; ----- Load offset and segment
	ldrb	r0, [r2] 					; Load low byte of offset
	ldrb	r1, [r2, #1]				; Load high byte of offset
	ldrb	r3, [r2, #2]				; Load low byte of DS from [r2+2]
	ldrb	r2, [r2, #3]				; Load high byte of DS from [r2+3]
	bfi		$reg, r0, #0, #8
	bfi		$reg, r1, #8, #8
	orr		r2, r3, r2, lsl #8			; r2 = low byte | (high byte << 8) = new DS value
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		mov_ds_r0r2_prot					; Yes we are, go handle protected mode version!
	;-------
	; We are in real mode, so use the simple handling.
	;-------
	mov		r1, r2, lsl #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_DS_VALUE]
	str		r1, [sp, #SP_DS_BASE]
	b		restore_flags_from_r0
	MEND

	lds_r16_r0_reg eax
	lds_r16_r0_reg ecx
	lds_r16_r0_reg edx
	lds_r16_r0_reg ebx
	lds_r16_r0_reg esp
	lds_r16_r0_reg ebp
	lds_r16_r0_reg esi
	lds_r16_r0_reg edi

	LTORG
	
	modrm_3_genall_old lds, lds_r16_r0

; ------------------- C6 = MOV r/m8, imm8 -----------------------------
;
; All modrm variations supported!
;
	GLOBAL	op_c6
op_c6
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =op_c6_jump
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	orr		r0, r0, lsr #3
	and		r0, #0x1F
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
op_c6_jump
	modrm_help_1_0 mov, imm8
	modrm_help_1_40 mov, imm8
	modrm_help_1_80 mov, imm8
	DCD op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	EXTERN	op_c6_EGA_r2
	EXTERN	op_c6_MODEX_r2
	GLOBAL	mov_r0_bp_imm8
mov_r0_bp_imm8
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_imm8
mov_r0_imm8
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 op_c6_RAM_r2, op_c6_EGA_r2, op_c6_MODEX_r2
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
op_c6_RAM_r2
	ldrb	r0, [r12],#1			; Load the imm8 value to r0
	strb	r0, [r2]				; Store the imm8 byte into RAM
	b		loop

	modrm_1_help mov, mov_r0, imm8

; ------------------- C7 = MOV r/m16, imm16 -----------------------------
;
; All modrm variations supported!
;
	GLOBAL	op_c7
op_c7
	ldrb	r0,[r12],#1							; Load the second opcode byte to r0, increment r12 by 1
	ldr		r1, =op_c7_jump
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	orr		r0, r0, lsr #3
	and		r0, #0x1F
	ldr		pc,[r1, r0, lsl #2]					; Jump to the handler
	AREA	jumptables, DATA, READONLY			; Begin a DATA area for the jump table here
	ALIGN	4
op_c7_jump
	modrm_help_1_0 mov, imm16
	modrm_help_1_40 mov, imm16
	modrm_help_1_80 mov, imm16
	DCD op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	EXTERN	op_c7_EGA_r2
	EXTERN	op_c7_MODEX_r2
	GLOBAL	mov_r0_bp_imm16
mov_r0_bp_imm16
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mov_r0_imm16
mov_r0_imm16
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 %f1, op_c7_EGA_r2, op_c7_MODEX_r2
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
1	ldrb	r0, [r12],#1			; Load the low byte of imm16 value to r0
	ldrb	r1, [r12],#1			; Load the high byte of imm16 value to r0
	strb	r0, [r2]				; Store the low byte into RAM
	strb	r1, [r2, #1]			; Store the high byte into RAM
	b		loop

	modrm_1_help mov, mov_r0, imm16

; ------------------- C8 = ENTER imm16,imm8 ---------------------------
;
; Note! This is a 80186+ opcode!
; If imm8 = 00, this is a combination of PUSH BP, MOV BP,SP, SUB SP, imm16
;
op_c8
	push_hword ebp, r0, r1			; PUSH BP
	bfi		ebp, esp, #0, #16		; MOV BP, SP
	ldrb	r0,[r12],#1				; Load the low byte of imm16 value to r0
	ldrb	r2,[r12],#1				; Load the high byte of imm16 value to r1
	ldrb	r3,[r12],#1				; Load the imm8 value to r2
	orr		r0, r2, lsl #8
	sub		r1, esp, r0
	bfi		esp, r1, #0, #16		; SUB SP,imm16
	;------ The code below is temporary, until the code is completed!
	mrs		r0, cpsr				; Save current flags
	cmp		r3, #0
	beq		restore_flags_from_r0
	sub		r12, #3
	b		unknown
	
; ------------------- C9 = LEAVE --------------------------------------
;
; Note! This is a 80186+ opcode!
; This is a combination of MOV SP,BP and POP BP
;
op_c9
	bfi		esp, ebp, #0, #16		; MOV SP, BP
	pop_reg_low_tmp r0, r1
	bfi		ebp, r0, #0, #16		; POP BP
	b		loop
	
; ------------------- CA = RETF imm16 ---------------------------------
; Profiler: 25767, 22, 25.61, 659836, 0.34%
;
	GLOBAL	op_ca
op_ca
	ldrb	r3,[r12],#1				; Load byte to r1, increment r12 by 1
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	orr		r3, r0, lsl #8			; r3 = low byte | (high byte << 8)
	b		retf_common_r3			; Continue with the common handler

; ------------------- CB = RETF ---------------------------------------
; Profiler: 9596, 20, 27.29, 261900, 0.14%
;
	GLOBAL	op_cb
op_cb
	;-------
	; First prepare the parameter value
	;	r3 = use32<<16 | (extra_words_to_pop)
	;-------
	mov		r3, #0					; USE16 RETF, no extra words to pop.
	GLOBAL	retf_common_r3
retf_common_r3
	;-------
	; Then determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r1, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	tst		r1, #1					; Are we in protected mode (or in VM mode)?
	bne		cpu_retf_prot_r0r3		; Yes we are, go handle protected mode RETF!
	;-------
	; Real mode RETF handling
	;-------
	GLOBAL	cpu_retf_real_r0r3
cpu_retf_real_r0r3
	tst		r3, #0x10000			; USE32 RETF?
	bne		retf_USE32				; Yep, go handle it (rare)
	msr		cpsr_f, r0				; restore flags
	pop_reg_low_tmp r12, r2			; Get new logical IP (zero-extended) to r12
	pop_reg_low_tmp r2, r1			; Get new logical CS (zero-extended) to r2
	add		r3, esp
	bfi		esp, r3, #0, #16		; Pop the extra bytes from the stack.
	str		r2, [sp, #SP_CS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]
	add		r12, r2
	b		loop
retf_USE32
	b		unknown

	EXTERN	cpu_retf_prot_r0r3
	
	ALIGN	4

; ------------------- CC = INT 03 -------------------------------------
; See opcode D6 for generic INT handler call
;
	GLOBAL	op_cc					; Protected mode INT 3 used in Realms of the Haunting (ROTH)
op_cc
	mov		r2, #3					; INT #3
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		cpu_int_prot_r0r2		; Yes we are, go handle protected mode INT!
	msr		cpsr_f,r0				; Restore the real flags again
	;------
	; Push current CPU flags to stack
	;------
	push_flags_16 r0, r1, r3	
	;------
	; Clear the Trap and Interrupt flags
	;------
	ldr		r1, [sp, #SP_FLAGS]		; Get the #SP_FLAGSS
	mrs		r0, cpsr				; Save flags to r0
	tst		r1, #FLAG_TF
	bic		r1, #(FLAG_TF:OR:FLAG_IF) ; Clear interrupts and the TRAP flag
	str		r1, [sp, #SP_FLAGS]		; Save the #SP_FLAGS
	ldrne	r0, [sp, #SP_IRQMAXC]
	strne	r1, [sp, #SP_IRQFLAG]	; Turn off the IRQ flag if trap flag was on
	msr		cpsr_f,r0				; Restore the real flags again
	b		op_cc_cont				; Continue with common code with opcode 0xCD = INT imm8
	
; ------------------- CD = INT imm8 -----------------------------------
; See opcode D6 for generic INT handler call
;
op_cd
	ldrb	r2,[r12],#1				; Load next opcode byte to r2, increment r12 by 1
op_cd_cont
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		cpu_int_prot_r0r2		; Yes we are, go handle protected mode INT!
	GLOBAL	cpu_int_real_r0r2
cpu_int_real_r0r2
	msr		cpsr_f,r0				; Restore the real flags again
	;------
	; Push current CPU flags to stack
	;------
	push_flags_16 r0, r1, r3	
op_cc_cont
	;------
	; Get the interrupt vector address
	;------
	mov		r1, r2					; r1 = INT number
	mov		r2, #0
	calc_linear_address_r2
	ldr		r2, [r2, r1, lsl #2]	; Get interrupt vector address, high halfword = segment, low halfword = offset
	;------
	; Push current CS:IP to stack
	;------
	ldr		r0, [sp, #SP_CS_VALUE]	; r0 = Current logical CS
	push_hword r0, r1, r3
	ldr		r1, [sp, #SP_PHYS_CS]	; r1 = Current physical CS
	sub		r1, r12, r1				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1, r0, r3
	;------
	; Get new logical CS:IP to r0:r12
	;------
	mov		r12, r2, lsl #16
	mov		r0, r2, lsr #16			; Now r0 = new logical CS
	;------
	; Save the new logical CS
	;------
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]
	str		r2, [sp, #SP_CS_BASE]
	;------
	; Finally calculate new physical IP
	;------
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]	; Store new physical CS into stack
	add		r12, r2, r12, lsr #16	; r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	b		loop

	EXTERN	cpu_int_prot_r0r2
	
; ------------------- CE = INTO ---------------------------------------
; See opcode D6 for generic INT handler call
;
op_ce
	bvc		loop					; NOP if Overflow flag is not set
	mov		r2, #4					; INT #4
	b		op_cd_cont				; Else go perform INT 4

; ------------------- CF = IRET ---------------------------------------
; Profiler: 5240, 24, 69.58, 364614, 0.19%
;
	GLOBAL	op_cf					; Called from "cpu_prot.S" when performing a triple fault reset.
op_cf
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bic		r0, #0x10000			; Mark that this is a 16-bit IRET
	bne		cpu_iret_prot_r0		; Yes we are, go handle protected mode IRET!
	;-------
	; We are in real mode, so continue.
	;-------
	msr		cpsr_f, r0				; restore flags
	pop_reg_low_tmp r12, r0			; Get new logical IP (zero-extended) to r12
	pop_reg_low_tmp r2, r0			; Get new logical CS (zero-extended) to r2
	str		r2, [sp, #SP_CS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]
	add		r12, r2
	pop_reg_low_tmp	r0, r1			; r0 = new flags
	ldr		r2, =FMASK_ALL			; r2 = mask of the flag bits we can change
	ldr		r1, [sp, #SP_FLAGS]		; Get the #SP_FLAGS
	bic		r2, #0x00FF0000			; mask = mask & 0xFFFF
	and		r0, r2					; Leave only the bits we can change to r0
	bic		r2, r1, r2				; r2 = flags bits that will not change
	orr		r0, r2
	str		r0, [sp, #SP_FLAGS]		; Store the new #SP_FLAGS
	GLOBAL	iret_cont_flags_old_r1_new_r0
iret_cont_flags_old_r1_new_r0		; Called from "cpu_prot.s" after a protected mode IRET!
	eor		r2, r0, r1				; r2 tells which flags have changed
	popped_flags_to_ARM r0			; Convert the flags to ARM flags
	ands	r2, #(FLAG_TF:OR:FLAG_IF) ; Have the Trap or Interrupt flags changed?
	beq		restore_flags_from_r0	; Nope, so go restore CPU flags and continue running.
	;-------
	; Extra flags have changed! 
	;	r0 = new flags (in x86 and ARM format)
	;	r1 = old flags (in x86 format)
	;	r2 = bit mask telling the changed flags
	;-------
	; If the trap flag is now on, make sure we have a proper handler for INT01!
	; For example "catacomb" sets the trap flag accidentally when pressing a key.
	;-------
	tst		r2, #FLAG_TF
	bne		trap_flag_on
	;-------
	; If the INT flags is now on, go test for pending interrupts. See "pic.s".
	;-------
trap_flag_ignore
	tst		r2, #FLAG_IF
	tstne	r0, #FLAG_IF
	bne		int_flag_on
	;-------
	; Neither INT or TRAP flags on, just continue.
	;-------
	b		restore_flags_from_r0	

	EXTERN	int_flag_on
	EXTERN	cpu_iret_prot_r0
	
	;-------
	; Trap flag turned on, go to single step mode (but only if we have a proper INT01 handler!)
	; On input
	;	r0 = saved CPU flags
	;	r1 = current EXTRAFLAGs
	;	r2 = free
	;-------
trap_flag_on
	mov		r3, r0					; Save the CPU flags to r3
	mov		r2, #0
	calc_linear_address_r2
	ldr		r2, [r2, #4]			; r2 = INT01 vector handler address
	mov		r0, #0xF0000000
	add		r0, #1					; r0 = our INT01 placeholder F000:0001
	cmp		r2, r0					; Is it our own placeholder?
	mov		r0, r3					; Restore CPU flags from r3
	biceq	r0, #FLAG_TF			; If yes, turn the flag back off.
	beq		trap_flag_ignore		; and ignore the trap flag handling.
	mov		r1, #IRQ_ON
	str		r1,[sp, #SP_IRQFLAG]	; Set the IRQFlag
	ldr		r2, =TrapFlag
	mov		r1, #0
	str		r1, [r2]
	b		restore_flags_from_r0

	LTORG								; Dump the current literal pool here

; ------------------- D0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,1 ---
; 
; All modrm variations supported!
; Note: The actual handlers are by op_c0, as rotating by 1 bit is a special case of the full rotate functionality.
;
; - RCL/RCR/ROL/ROR change the carry and overflow flags only, must not change the Sign and Zero flags.
; - SHL/SHR/SHL/SAR change carry, sign, overflow and zero flags
;
op_d0
	modrm_jump_16_tbl op_d0_jump
; 0 (idx only)
	modrm_help_1_0 rol_b, 1
	modrm_help_1_0 ror_b, 1
	modrm_help_1_0 rcl_b, 1
	modrm_help_1_0 rcr_b, 1
	modrm_help_1_0 shl_b, 1
	modrm_help_1_0 shr_b, 1
	modrm_help_1_0 shl_b, 1
	modrm_help_1_0 sar_b, 1
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_b, 1
	modrm_help_1_40 ror_b, 1
	modrm_help_1_40 rcl_b, 1
	modrm_help_1_40 rcr_b, 1
	modrm_help_1_40 shl_b, 1
	modrm_help_1_40 shr_b, 1
	modrm_help_1_40 shl_b, 1
	modrm_help_1_40 sar_b, 1
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_b, 1
	modrm_help_1_80 ror_b, 1
	modrm_help_1_80 rcl_b, 1
	modrm_help_1_80 rcr_b, 1
	modrm_help_1_80 shl_b, 1
	modrm_help_1_80 shr_b, 1
	modrm_help_1_80 shl_b, 1
	modrm_help_1_80 sar_b, 1
;0xc0 = mod = 11b => two register operands
	DCD rol_reg8l_1_eax, rol_reg8l_1_ecx, rol_reg8l_1_edx, rol_reg8l_1_ebx, rol_reg8h_1_eax, rol_reg8h_1_ecx, rol_reg8h_1_edx, rol_reg8h_1_ebx
	DCD ror_reg8l_1_eax, ror_reg8l_1_ecx, ror_reg8l_1_edx, ror_reg8l_1_ebx, ror_reg8h_1_eax, ror_reg8h_1_ecx, ror_reg8h_1_edx, ror_reg8h_1_ebx
	DCD rcl_reg8l_1_eax, rcl_reg8l_1_ecx, rcl_reg8l_1_edx, rcl_reg8l_1_ebx, rcl_reg8h_1_eax, rcl_reg8h_1_ecx, rcl_reg8h_1_edx, rcl_reg8h_1_ebx
	DCD rcr_reg8l_1_eax, rcr_reg8l_1_ecx, rcr_reg8l_1_edx, rcr_reg8l_1_ebx, rcr_reg8h_1_eax, rcr_reg8h_1_ecx, rcr_reg8h_1_edx, rcr_reg8h_1_ebx
	DCD shl_reg8l_1_eax, shl_reg8l_1_ecx, shl_reg8l_1_edx, shl_reg8l_1_ebx, shl_reg8h_1_eax, shl_reg8h_1_ecx, shl_reg8h_1_edx, shl_reg8h_1_ebx
	DCD shr_reg8l_1_eax, shr_reg8l_1_ecx, shr_reg8l_1_edx, shr_reg8l_1_ebx, shr_reg8h_1_eax, shr_reg8h_1_ecx, shr_reg8h_1_edx, shr_reg8h_1_ebx
	DCD shl_reg8l_1_eax, shl_reg8l_1_ecx, shl_reg8l_1_edx, shl_reg8l_1_ebx, shl_reg8h_1_eax, shl_reg8h_1_ecx, shl_reg8h_1_edx, shl_reg8h_1_ebx
	DCD sar_reg8l_1_eax, sar_reg8l_1_ecx, sar_reg8l_1_edx, sar_reg8l_1_ebx, sar_reg8h_1_eax, sar_reg8h_1_ecx, sar_reg8h_1_edx, sar_reg8h_1_ebx

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	opd0common $oper
	GLOBAL	$oper_byte_r0_bp_1
$oper_byte_r0_bp_1				; Rotate a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	$oper_byte_r0_1
$oper_byte_r0_1					; Rotate a byte at offset r0high in effective segment left by r1 value
	mem_handler_jump_r0r3_far $oper_byte_r2_1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	modrm_1_help $oper_b, $oper_byte_r0, 1
	MEND

	opd0common rol
	opd0common ror
	opd0common rcl
	opd0common rcr
	opd0common shl
	opd0common shr
	opd0common sar
	
; ------------------- D1 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,1 ---
; 
; All modrm variations supported!
;
; - RCL/RCR/ROL/ROR change the carry and overflow flags only, must not change the Sign and Zero flags.
; - SHL/SHR/SHL/SAR change carry, sign, overflow and zero flags
;
op_d1
	modrm_jump_16_tbl op_d1_jump
; 0 (idx only)
	modrm_help_1_0 rol_16, 1
	modrm_help_1_0 ror_16, 1
	modrm_help_1_0 rcl_16, 1
	modrm_help_1_0 rcr_16, 1
	modrm_help_1_0 shl_16, 1
	modrm_help_1_0 shr_16, 1
	modrm_help_1_0 shl_16, 1
	modrm_help_1_0 sar_16, 1
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_16, 1
	modrm_help_1_40 ror_16, 1
	modrm_help_1_40 rcl_16, 1
	modrm_help_1_40 rcr_16, 1
	modrm_help_1_40 shl_16, 1
	modrm_help_1_40 shr_16, 1
	modrm_help_1_40 shl_16, 1
	modrm_help_1_40 sar_16, 1
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_16, 1
	modrm_help_1_80 ror_16, 1
	modrm_help_1_80 rcl_16, 1
	modrm_help_1_80 rcr_16, 1
	modrm_help_1_80 shl_16, 1
	modrm_help_1_80 shr_16, 1
	modrm_help_1_80 shl_16, 1
	modrm_help_1_80 sar_16, 1
;0xc0 = mod = 11b => two register operands
	DCD rol_reg16_1_eax, rol_reg16_1_ecx, rol_reg16_1_edx, rol_reg16_1_ebx, rol_reg16_1_esp, rol_reg16_1_ebp, rol_reg16_1_esi, rol_reg16_1_edi
	DCD ror_reg16_1_eax, ror_reg16_1_ecx, ror_reg16_1_edx, ror_reg16_1_ebx, ror_reg16_1_esp, ror_reg16_1_ebp, ror_reg16_1_esi, ror_reg16_1_edi
	DCD rcl_reg16_1_eax, rcl_reg16_1_ecx, rcl_reg16_1_edx, rcl_reg16_1_ebx, rcl_reg16_1_esp, rcl_reg16_1_ebp, rcl_reg16_1_esi, rcl_reg16_1_edi
	DCD rcr_reg16_1_eax, rcr_reg16_1_ecx, rcr_reg16_1_edx, rcr_reg16_1_ebx, rcr_reg16_1_esp, rcr_reg16_1_ebp, rcr_reg16_1_esi, rcr_reg16_1_edi
	DCD shl_reg16_1_eax, shl_reg16_1_ecx, shl_reg16_1_edx, shl_reg16_1_ebx, shl_reg16_1_esp, shl_reg16_1_ebp, shl_reg16_1_esi, shl_reg16_1_edi
	DCD shr_reg16_1_eax, shr_reg16_1_ecx, shr_reg16_1_edx, shr_reg16_1_ebx, shr_reg16_1_esp, shr_reg16_1_ebp, shr_reg16_1_esi, shr_reg16_1_edi
	DCD shl_reg16_1_eax, shl_reg16_1_ecx, shl_reg16_1_edx, shl_reg16_1_ebx, shl_reg16_1_esp, shl_reg16_1_ebp, shl_reg16_1_esi, shl_reg16_1_edi
	DCD sar_reg16_1_eax, sar_reg16_1_ecx, sar_reg16_1_edx, sar_reg16_1_ebx, sar_reg16_1_esp, sar_reg16_1_ebp, sar_reg16_1_esi, sar_reg16_1_edi

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	opd1common $oper
	GLOBAL	$oper_16_r0_bp_1
$oper_16_r0_bp_1
	mem_handler_bp
	GLOBAL	$oper_16_r0_1
$oper_16_r0_1
	mem_handler_jump_r0r3_far $oper_word_r2_1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	modrm_1_help $oper_16, $oper_16_r0, 1
	MEND

	opd1common rol
	opd1common ror
	opd1common rcl
	opd1common rcr
	opd1common shl
	opd1common shr
	opd1common sar

; ------------------- D2 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,CL ---
; 
; All modrm variations supported!
; Note: The actual handlers are by op_c0
;
op_d2
	modrm_jump_16_tbl op_d2_jump
; 0 (idx only)
	modrm_help_1_0 rol_b, CL
	modrm_help_1_0 ror_b, CL
	modrm_help_1_0 rcl_b, CL
	modrm_help_1_0 rcr_b, CL
	modrm_help_1_0 shl_b, CL
	modrm_help_1_0 shr_b, CL
	modrm_help_1_0 shl_b, CL
	modrm_help_1_0 sar_b, CL
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_b, CL
	modrm_help_1_40 ror_b, CL
	modrm_help_1_40 rcl_b, CL
	modrm_help_1_40 rcr_b, CL
	modrm_help_1_40 shl_b, CL
	modrm_help_1_40 shr_b, CL
	modrm_help_1_40 shl_b, CL
	modrm_help_1_40 sar_b, CL
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_b, CL
	modrm_help_1_80 ror_b, CL
	modrm_help_1_80 rcl_b, CL
	modrm_help_1_80 rcr_b, CL
	modrm_help_1_80 shl_b, CL
	modrm_help_1_80 shr_b, CL
	modrm_help_1_80 shl_b, CL
	modrm_help_1_80 sar_b, CL
;0xC0 = mod = 11b => two register operands
	DCD rol_al_CL, rol_cl_CL, rol_dl_CL, rol_bl_CL, rol_ah_CL, rol_ch_CL, rol_dh_CL, rol_bh_CL
	DCD ror_al_CL, ror_cl_CL, ror_dl_CL, ror_bl_CL, ror_ah_CL, ror_ch_CL, ror_dh_CL, ror_bh_CL
	DCD rcl_al_CL, rcl_cl_CL, rcl_dl_CL, rcl_bl_CL, rcl_ah_CL, rcl_ch_CL, rcl_dh_CL, rcl_bh_CL
	DCD rcr_al_CL, rcr_cl_CL, rcr_dl_CL, rcr_bl_CL, rcr_ah_CL, rcr_ch_CL, rcr_dh_CL, rcr_bh_CL
	DCD shl_al_CL, shl_cl_CL, shl_dl_CL, shl_bl_CL, shl_ah_CL, shl_ch_CL, shl_dh_CL, shl_bh_CL
	DCD shr_al_CL, shr_cl_CL, shr_dl_CL, shr_bl_CL, shr_ah_CL, shr_ch_CL, shr_dh_CL, shr_bh_CL
	DCD shl_al_CL, shl_cl_CL, shl_dl_CL, shl_bl_CL, shl_ah_CL, shl_ch_CL, shl_dh_CL, shl_bh_CL
	DCD sar_al_CL, sar_cl_CL, sar_dl_CL, sar_bl_CL, sar_ah_CL, sar_ch_CL, sar_dh_CL, sar_bh_CL

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	opd2common $oper
	GLOBAL	$oper_b_r0_bp_CL
$oper_b_r0_bp_CL
	mem_handler_bp
	GLOBAL	$oper_b_r0_CL
$oper_b_r0_CL
	mem_handler_jump_r0r3 %f2, bad_EGA_opcode, bad_MODEX_opcode
2	and		r1, ecx, #31						; Mask the rotation count
	b		$oper_byte_r2_r1					; Jump to a common handler with "op_c0"
	modrm_1_help $oper_b, $oper_b_r0, CL
	MEND

	opd2common rol
	opd2common ror
	opd2common rcl
	opd2common rcr
	opd2common shl
	opd2common shr
	opd2common sar

	MACRO 
	opd2reg8l_CL $oper, $reg
	and		r1, ecx, #31
	b		$oper_r8l_r1_$reg	
	MEND
	MACRO 
	opd2reg8h_CL $oper, $reg
	and		r1, ecx, #31
	b		$oper_r8h_r1_$reg	
	MEND
	
rol_al_CL
	opd2reg8l_CL rol, eax
rol_cl_CL
	opd2reg8l_CL rol, ecx
rol_dl_CL
	opd2reg8l_CL rol, edx
rol_bl_CL
	opd2reg8l_CL rol, ebx
rol_ah_CL
	opd2reg8h_CL rol, eax
rol_ch_CL
	opd2reg8h_CL rol, ecx
rol_dh_CL
	opd2reg8h_CL rol, edx
rol_bh_CL
	opd2reg8h_CL rol, ebx
	
ror_al_CL
	opd2reg8l_CL ror, eax
ror_cl_CL
	opd2reg8l_CL ror, ecx
ror_dl_CL
	opd2reg8l_CL ror, edx
ror_bl_CL
	opd2reg8l_CL ror, ebx
ror_ah_CL
	opd2reg8h_CL ror, eax
ror_ch_CL
	opd2reg8h_CL ror, ecx
ror_dh_CL
	opd2reg8h_CL ror, edx
ror_bh_CL
	opd2reg8h_CL ror, ebx

rcl_al_CL
	opd2reg8l_CL rcl, eax
rcl_cl_CL
	opd2reg8l_CL rcl, ecx
rcl_dl_CL
	opd2reg8l_CL rcl, edx
rcl_bl_CL
	opd2reg8l_CL rcl, ebx
rcl_ah_CL
	opd2reg8h_CL rcl, eax
rcl_ch_CL
	opd2reg8h_CL rcl, ecx
rcl_dh_CL
	opd2reg8h_CL rcl, edx
rcl_bh_CL
	opd2reg8h_CL rcl, ebx
	
rcr_al_CL
	opd2reg8l_CL rcr, eax
rcr_cl_CL
	opd2reg8l_CL rcr, ecx
rcr_dl_CL
	opd2reg8l_CL rcr, edx
rcr_bl_CL
	opd2reg8l_CL rcr, ebx
rcr_ah_CL
	opd2reg8h_CL rcr, eax
rcr_ch_CL
	opd2reg8h_CL rcr, ecx
rcr_dh_CL
	opd2reg8h_CL rcr, edx
rcr_bh_CL
	opd2reg8h_CL rcr, ebx
	
shl_al_CL
	opd2reg8l_CL shl, eax
shl_cl_CL
	opd2reg8l_CL shl, ecx
shl_dl_CL
	opd2reg8l_CL shl, edx
shl_bl_CL
	opd2reg8l_CL shl, ebx
shl_ah_CL
	opd2reg8h_CL shl, eax
shl_ch_CL
	opd2reg8h_CL shl, ecx
shl_dh_CL
	opd2reg8h_CL shl, edx
shl_bh_CL
	opd2reg8h_CL shl, ebx
	
shr_al_CL
	opd2reg8l_CL shr, eax
shr_cl_CL
	opd2reg8l_CL shr, ecx
shr_dl_CL
	opd2reg8l_CL shr, edx
shr_bl_CL
	opd2reg8l_CL shr, ebx
shr_ah_CL
	opd2reg8h_CL shr, eax
shr_ch_CL
	opd2reg8h_CL shr, ecx
shr_dh_CL
	opd2reg8h_CL shr, edx
shr_bh_CL
	opd2reg8h_CL shr, ebx

sar_al_CL
	opd2reg8l_CL sar, eax
sar_cl_CL
	opd2reg8l_CL sar, ecx
sar_dl_CL
	opd2reg8l_CL sar, edx
sar_bl_CL
	opd2reg8l_CL sar, ebx
sar_ah_CL
	opd2reg8h_CL sar, eax
sar_ch_CL
	opd2reg8h_CL sar, ecx
sar_dh_CL
	opd2reg8h_CL sar, edx
sar_bh_CL
	opd2reg8h_CL sar, ebx
	
	GLOBAL rol_al_CL
	GLOBAL rol_cl_CL
	GLOBAL rol_dl_CL
	GLOBAL rol_bl_CL
	GLOBAL rol_ah_CL
	GLOBAL rol_ch_CL
	GLOBAL rol_dh_CL
	GLOBAL rol_bh_CL
	GLOBAL ror_al_CL
	GLOBAL ror_cl_CL
	GLOBAL ror_dl_CL
	GLOBAL ror_bl_CL
	GLOBAL ror_ah_CL
	GLOBAL ror_ch_CL
	GLOBAL ror_dh_CL
	GLOBAL ror_bh_CL
	GLOBAL rcl_al_CL
	GLOBAL rcl_cl_CL
	GLOBAL rcl_dl_CL
	GLOBAL rcl_bl_CL
	GLOBAL rcl_ah_CL
	GLOBAL rcl_ch_CL
	GLOBAL rcl_dh_CL
	GLOBAL rcl_bh_CL
	GLOBAL rcr_al_CL
	GLOBAL rcr_cl_CL
	GLOBAL rcr_dl_CL
	GLOBAL rcr_bl_CL
	GLOBAL rcr_ah_CL
	GLOBAL rcr_ch_CL
	GLOBAL rcr_dh_CL
	GLOBAL rcr_bh_CL
	GLOBAL shl_al_CL
	GLOBAL shl_cl_CL
	GLOBAL shl_dl_CL
	GLOBAL shl_bl_CL
	GLOBAL shl_ah_CL
	GLOBAL shl_ch_CL
	GLOBAL shl_dh_CL
	GLOBAL shl_bh_CL
	GLOBAL shr_al_CL
	GLOBAL shr_cl_CL
	GLOBAL shr_dl_CL
	GLOBAL shr_bl_CL
	GLOBAL shr_ah_CL
	GLOBAL shr_ch_CL
	GLOBAL shr_dh_CL
	GLOBAL shr_bh_CL
	GLOBAL sar_al_CL
	GLOBAL sar_cl_CL
	GLOBAL sar_dl_CL
	GLOBAL sar_bl_CL
	GLOBAL sar_ah_CL
	GLOBAL sar_ch_CL
	GLOBAL sar_dh_CL
	GLOBAL sar_bh_CL

; ------------------- D3 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m16,CL ---
; 
; All modrm variations supported!
;
; We must also setup the flags!
; - RCL/RCR/ROL/ROR only change the carry flag
; - SHL/SHR/SHL/SAR change carry, sign and zero flags (ARM behaves exactly like 8086)
;
; We can use the same handlers as opcode C1, but only if the handlers do not need to
; load additional immediate bytes!
;
op_d3
	modrm_jump_16_tbl op_d3_jump
; 0 (idx only)
	modrm_help_1_0 rol_16, CL
	modrm_help_1_0 ror_16, CL
	modrm_help_1_0 rcl_16, CL
	modrm_help_1_0 rcr_16, CL
	modrm_help_1_0 shl_16, CL
	modrm_help_1_0 shr_16, CL
	modrm_help_1_0 shl_16, CL
	modrm_help_1_0 sar_16, CL
; 0x40 (idx+disp8)
	modrm_help_1_40 rol_16, CL
	modrm_help_1_40 ror_16, CL
	modrm_help_1_40 rcl_16, CL
	modrm_help_1_40 rcr_16, CL
	modrm_help_1_40 shl_16, CL
	modrm_help_1_40 shr_16, CL
	modrm_help_1_40 shl_16, CL
	modrm_help_1_40 sar_16, CL
; 0x80 (idx+disp16)
	modrm_help_1_80 rol_16, CL
	modrm_help_1_80 ror_16, CL
	modrm_help_1_80 rcl_16, CL
	modrm_help_1_80 rcr_16, CL
	modrm_help_1_80 shl_16, CL
	modrm_help_1_80 shr_16, CL
	modrm_help_1_80 shl_16, CL
	modrm_help_1_80 sar_16, CL
;0xC0 = mod = 11b => two register operands
	DCD rol_ax_CL, rol_cx_CL, rol_dx_CL, rol_bx_CL, rol_sp_CL, rol_bp_CL, rol_si_CL, rol_di_CL
	DCD ror_ax_CL, ror_cx_CL, ror_dx_CL, ror_bx_CL, ror_sp_CL, ror_bp_CL, ror_si_CL, ror_di_CL
	DCD rcl_ax_CL, rcl_cx_CL, rcl_dx_CL, rcl_bx_CL, rcl_sp_CL, rcl_bp_CL, rcl_si_CL, rcl_di_CL
	DCD rcr_ax_CL, rcr_cx_CL, rcr_dx_CL, rcr_bx_CL, rcr_sp_CL, rcr_bp_CL, rcr_si_CL, rcr_di_CL
	DCD shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	DCD shr_ax_CL, shr_cx_CL, shr_dx_CL, shr_bx_CL, shr_sp_CL, shr_bp_CL, shr_si_CL, shr_di_CL
	DCD shl_ax_CL, shl_cx_CL, shl_dx_CL, shl_bx_CL, shl_sp_CL, shl_bp_CL, shl_si_CL, shl_di_CL
	DCD sar_ax_CL, sar_cx_CL, sar_dx_CL, sar_bx_CL, sar_sp_CL, sar_bp_CL, sar_si_CL, sar_di_CL

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	MACRO 
	opd3common $oper
	GLOBAL	$oper_16_r0_bp_CL
$oper_16_r0_bp_CL				; Shift a byte when r0high offset is based on BP index
	mem_handler_bp
	GLOBAL	$oper_16_r0_CL
$oper_16_r0_CL					; Shift a byte at offset r0high in effective segment right by r1 value
	and		r1, ecx, #31		; r1 = CL value to shift/rotate with
	mem_handler_jump_r0r3_far $oper_word_r2_r1_RAM, bad_EGA_opcode, bad_MODEX_opcode
	modrm_1_help $oper_16, $oper_16_r0, CL
	MEND



; ----- ROL -----

	opd3common	rol

rol_ax_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_eax
rol_cx_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_ecx
rol_dx_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_edx
rol_bx_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_ebx
rol_sp_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_esp
rol_bp_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_ebp
rol_si_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_esi
rol_di_CL
	and		r1, ecx, #31
	b		rol_reg16_r1_edi
	
; ----- ROR -----

	opd3common	ror

ror_ax_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_eax
ror_cx_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_ecx
ror_dx_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_edx
ror_bx_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_ebx
ror_sp_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_esp
ror_bp_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_ebp
ror_si_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_esi
ror_di_CL
	and		r1, ecx, #31
	b		ror_reg16_r1_edi

; ----- RCL -----

	opd3common	rcl

rcl_ax_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_eax
rcl_cx_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_ecx
rcl_dx_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_edx
rcl_bx_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_ebx
rcl_sp_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_esp
rcl_bp_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_ebp
rcl_si_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_esi
rcl_di_CL
	and		r1, ecx, #31
	b		rcl_reg16_r1_edi

; ----- RCR -----

	opd3common	rcr

rcr_ax_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_eax
rcr_cx_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_ecx
rcr_dx_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_edx
rcr_bx_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_ebx
rcr_sp_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_esp
rcr_bp_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_ebp
rcr_si_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_esi
rcr_di_CL
	and		r1, ecx, #31
	b		rcr_reg16_r1_edi

; ----- SHL -----

	opd3common	shl

shl_ax_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_eax
shl_cx_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_ecx
shl_dx_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_edx
shl_bx_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_ebx
shl_sp_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_esp
shl_bp_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_ebp
shl_si_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_esi
shl_di_CL
	and		r1, ecx, #31
	b		shl_reg16_r1_edi

; ----- SHR -----

	opd3common	shr

shr_ax_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_eax
shr_cx_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_ecx
shr_dx_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_edx
shr_bx_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_ebx
shr_sp_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_esp
shr_bp_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_ebp
shr_si_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_esi
shr_di_CL
	and		r1, ecx, #31
	b		shr_reg16_r1_edi

; ----- SAR -----

	opd3common	sar

sar_ax_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_eax
sar_cx_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_ecx
sar_dx_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_edx
sar_bx_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_ebx
sar_sp_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_esp
sar_bp_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_ebp
sar_si_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_esi
sar_di_CL
	and		r1, ecx, #31
	b		sar_reg16_r1_edi

	GLOBAL	rol_ax_CL
	GLOBAL	rol_cx_CL
	GLOBAL  rol_dx_CL
	GLOBAL  rol_bx_CL
	GLOBAL  rol_sp_CL
	GLOBAL  rol_bp_CL
	GLOBAL  rol_si_CL
	GLOBAL  rol_di_CL
	GLOBAL	ror_ax_CL
	GLOBAL  ror_cx_CL
	GLOBAL  ror_dx_CL
	GLOBAL  ror_bx_CL
	GLOBAL  ror_sp_CL
	GLOBAL  ror_bp_CL
	GLOBAL  ror_si_CL
	GLOBAL  ror_di_CL
	GLOBAL	rcl_ax_CL
	GLOBAL	rcl_cx_CL
	GLOBAL  rcl_dx_CL
	GLOBAL  rcl_bx_CL
	GLOBAL  rcl_sp_CL
	GLOBAL  rcl_bp_CL
	GLOBAL  rcl_si_CL
	GLOBAL  rcl_di_CL
	GLOBAL	rcr_ax_CL
	GLOBAL  rcr_cx_CL
	GLOBAL  rcr_dx_CL
	GLOBAL  rcr_bx_CL
	GLOBAL  rcr_sp_CL
	GLOBAL  rcr_bp_CL
	GLOBAL  rcr_si_CL
	GLOBAL  rcr_di_CL
	GLOBAL	shl_ax_CL
	GLOBAL  shl_cx_CL
	GLOBAL  shl_dx_CL
	GLOBAL  shl_bx_CL
	GLOBAL  shl_sp_CL
	GLOBAL  shl_bp_CL
	GLOBAL  shl_si_CL
	GLOBAL  shl_di_CL
	GLOBAL	shr_ax_CL
	GLOBAL  shr_cx_CL
	GLOBAL  shr_dx_CL
	GLOBAL  shr_bx_CL
	GLOBAL  shr_sp_CL
	GLOBAL  shr_bp_CL
	GLOBAL  shr_si_CL
	GLOBAL  shr_di_CL
	GLOBAL	sar_ax_CL
	GLOBAL  sar_cx_CL
	GLOBAL  sar_dx_CL
	GLOBAL  sar_bx_CL
	GLOBAL  sar_sp_CL
	GLOBAL  sar_bp_CL
	GLOBAL  sar_si_CL
	GLOBAL  sar_di_CL


; ------------------- D4 = AAM ----------------------------------------
; AAM divides AL by 10 (imm8), leaving the quotient in AH and remainder in AL
; OF, CF and AF flags are cleared, SF and ZF are based on the resulting AL value.
;
op_d4
	ldrb	r2,[r12],#1				; Get the divider (usually 0x0A = 10)
	;-------
	; Same code as in div_reg8_by_r2
	;-------
	IF SOFTWARE_DIV = 1

	;-------
	; Software division routine. Algorithm taken from
	; "http:;www.peter-cockerell.net/aalp/html/ch-6.html"
	; 	r0 = div (result)
	; 	r1 = mod (result)
	; 	r2 = rhs
	; 	r3 = lhs
	;	eax = count
	;-------
	teq  	r2, #0    				; Trap div by zero
	beq		div8_by_zero			; division by zero!!
	push	{eax}
	movs	r3, eax, lsl #24
	mov  	r1, #0    				; Init remainder
	mov  	r0, #0    				; and result
	mov  	eax, #8    				; Set up count
	bmi		%f2						; Skip first loop if highest bit of r3 set
1	subs 	eax, eax, #1  			; Get first 1 bit of lhs
	beq		%f3    					; into bit 31. Return if 0
	movs 	r3, r3, ASL #1
	bpl  	%b1
2	movs 	r3, r3, ASL #1  		; Get next bit into...
	adc  	r1, r1, r1   			; r1 for trial subtract
	cmp  	r1, r2    				; Can we subtract?
	subcs 	r1, r1, r2   			; Yes, so do
	adc  	r0, r0, r0   			; Shift carry into result
	subs 	eax, eax, #1  			; Next loop
	bne  	%b2
3	pop		{eax}
	mov		r2, #0
	msr		cpsr_f, r2				; First clear all flags
	mov		r2, r1, lsl #24			; Put remainder to the highest byte
	tst		r2, r2					; Set the Sign and Zero flags
	bfi		eax, r1, #0, #8			; AL = remainder
	bfi		eax, r0, #8, #8			; AH = result
	b		loop					; Go back to loop

	ELSE

	mov		r0, #0x280
	orr		r0, #0x04000000			; #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		div8_by_zero			; Division by zero!! (See F7 opcode handler)
	mov		r1, #0
	strh	r1, [r0]				; REG_DIVCNT = DIV_32_32 = 0
	and		r1, eax, #0xFF			; r1 = AL value
	str		r1, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1	ldrh	r1, [r0]				; while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		%b1
	ldr		r2,[r0, #0x20]			; #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	eax (AL) is the result of the division
	cmp		r2, #0x100
	bhs		div8_by_zero			; If result is >= 256, divide overflow! (See F7 opcode handler)
	ldr		r1,[r0, #0x28]			; #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	r1 (AH) is the remainder of the division
	mov		r0, eax, lsl #16
	eor		eax, r0, lsr #16
	and		r2, #0xFF
	orr		eax, r2, lsl #8			; AH = quotient
	and		r1, #0xFF
	orr		eax, r1					; AL = remainder
	b		loop

	ENDIF

; ------------------- D5 = AAD ----------------------------------------
; AAD calculates AL = AL + AH * 10 (imm8), AH = 0
; Sign and Zero flags are set from result, FLAG_CF, FLAG_OF and FLAG_AF are cleared.
;
op_d5
	ldrb	r2,[r12],#1				; Get the multiplier (usually 0x0A = 10)
	mov		r0, eax, lsl #16
	mov		r3, eax, lsl #24		; r3 = AL in highest byte
	eor		eax, r0, lsr #16		; Clean AX
	lsr		r0, #24					; r0 = AH
	mul		r1, r0, r2				; r1 = AH * imm8 value
	adds	r3, r1, lsl #24			; r3 = AL + AH * imm8 in highest byte, flags set as per result
	ldr		r0,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	orr		eax, r3, lsr #24		; AL = AL + AH*imm8, AH = 0
	bic		r0, #FLAG_AF
	str		r0,[sp, #SP_FLAGS]		; Save the new extra flags
	mov		r0, #0
	tst		eax, #0xFF
	orreq	r0, #ARM_ZERO
	tst		eax, #0x80
	orrne	r0, #ARM_NEG
	b		restore_flags_from_r0

; ------------------- D6 = SETALC (undocumented) ----------------------
; If Carry Then AL=0xFF Else AL=00
;
; DSx86: Interrupt call if CS:IP between F000:0000 and F000:0200
;
op_d6
	IF EXTERNAL_TLB = 0
	ldr		r3, =BIOS_F000
	ldr		r3, [r3]				; r3 = address of BIOS_F000
	mrs		r0, cpsr				; Save current flags to r0
	subs	r3, r12, r3
	ble		%f1
	;=======
	; Call our generic interrupt handler, r1 = interrupt number + 1
	;=======
	sub		r3, #1					; r3 = interrupt number to call
	;-------
	; Perform an IRET first, so that the IRQ handler will return to the correct place.
	;-------
	pop_reg_low_tmp r12, r0			; Get new logical IP (zero-extended) to r12
	pop_reg_low_tmp r2, r0			; Get new logical CS (zero-extended) to r2
	str		r2, [sp, #SP_CS_VALUE]
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]
	add		r12, r2
	pop_reg_low_tmp	r1, r0						; r1 = new flags
	popped_flags_to_ARM r1
	;-------
	; Fix the extra flags, as starting an interrupt has cleared the interrupt flag.
	;-------
	ldr		r2, =FMASK_ALL						; r2 = mask of the flag bits we can change
	ldr		r0, [sp, #SP_FLAGS]					; Get the #SP_FLAGS
	and		r1, r2								; Leave only the bits we can change to r0
	bic		r0, r2								; r0 = flags bits that will not change
	orr		r1, r0
	;-------
	; Save registers to memory
	;-------
	ldr		r2,=registers
	stmia	r2!,{r4-r12}						; Save emulation registers to global memory
	str		r1, [r2], #4						; Save flags to global memory
	;-------
	; Save the logical segment registers (ES, CS, SS, DS, FS, GS)
	;-------
	add		r1, sp, #SP_ES_VALUE
	ldmia	r1, {r4-r9}							; ES, CS, SS, DS, FS, GS values
	stmia	r2, {r4-r9}							; Save to REG_ES .. REG_GS to global memory
	;-------
	; Call the INT handler, r0 = interrupt number
	;-------
	mov		r0, r3
	bl		INTHandler							; Call the external INT handler function, parameter r0 = interrupt number, return whether we can continue
	mov		r10, r0								; r10 = INTHandler return value
	;-------
	; Setup (possibly changed) segment registers
	;-------
	ldr		r1, =reg_es
	add		r2, sp, #SP_ES_VALUE
	ldmia	r1, {r4-r9}							; ES, CS, SS, DS, FS, GS values
	stmia	r2, {r4-r9}							; Save the segment register values to stack
	add		r2, #(SP_ES_BASE - SP_ES_VALUE)
	mov		r4, r4, lsl #REAL_SEGMENT_SHIFT
	mov		r5, r5, lsl #REAL_SEGMENT_SHIFT
	mov		r6, r6, lsl #REAL_SEGMENT_SHIFT
	mov		r7, r7, lsl #REAL_SEGMENT_SHIFT
	mov		r8, r8, lsl #REAL_SEGMENT_SHIFT
	mov		r9, r9, lsl #REAL_SEGMENT_SHIFT
	stmia	r2, {r4-r9}							; Save the segment register base values to stack
	ldr		r2,[sp, #SP_CS_BASE]
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]				; Store new physical CS into stack
	ldr		r2,[sp, #SP_SS_BASE]
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_SS]
	;-------
	; Load the (possibly changed) registers
	;-------
	ldr		r1,=registers
	mov		r0, r10								; r0 = INTHandler return value
	ldmia	r1!,{r4-r12}						; Load emulation registers from global memory
	;-------
	; Restore flags and continue running
	;-------
	cmp		r0, #0								; Did the INTHandler return 0?
	ldr		r0, [r1]							; Get the flags from global memory
	ldr		r1, [sp, #SP_FLAGS]					; Get the old flags
	str		r0, [sp, #SP_FLAGS]					; Save the new flags
	beq		iret_cont_flags_old_r1_new_r0		; INTHandler returned 0, we can continue running. Go setup proper flags.

	;-------
	; Unsupported INT call, break into debugger
	;-------
	ldr		r2, =BreakReason					; Check if the INT handler has already set a break reason...
	ldr		r1, [r2]
	cmp		r1, #0
	ldreq	r1, =BRUnsINT						; ... tell "Unsupported INT call" if not, ...
	streq	r1, [r2]
	popped_flags_to_ARM r0						; ... set the processor flags, ...
	msr		cpsr_f,r0
	b		debug_trap_false					; ... stop running and go to the debugger.
	ENDIF
	;=======
	; Perform SETALC
	;=======
1	tst		r0, #ARM_CARRY
	biceq	eax, #0xFF
	orrne	eax, #0xFF
	b		restore_flags_from_r0

	LTORG
	
; ------------------- D7 = XLAT ---------------------------------------
; AL = [BX + unsigned AL]
;
op_d7
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	and		r0, eax, #0xFF						; r0 = unsigned AL
	add		r0, ebx								; r0 = BX + (unsigned)AL
	mem_handler_jump_r0r3 op_d7_RAM, unknown, unknown

	GLOBAL	op_d7_RAM
op_d7_RAM
	ldrb	r0,[r2]
	bfi		eax, r0, #0, #8						; AL = byte at [BX + unsigned AL]
	b		loop


; ------------------- D8 .. DF = FPU ESC opcodes ---------------------
; Math coprocessor opcode, unsupported!
; DBE3				finit
; D93F				fstcw [bx] (Comptest, Turbo Pascal 7)
; DD7EFE			fstsw [bp-02] (Warcraft 2, Settlers, Game, Abuse, RAP)
;
op_db
	mrs		r0, cpsr				; Save current flags to r0
	ldrb	r1, [r12]				; Get the modrm byte
	cmp		r1, #0xE3				; Is it 0xE3 == finit?
	bne		%f1
	msr		cpsr_f, r0
	add		r12, #1
	b		loop					; in "fpu.S"
1	msr		cpsr_f, r0

op_d8
op_da
op_dc
op_de
op_df
	;------
	; Show a warning message, if this is the first time we encounter FPU opcodes in this game.
	;------
;	sw		sp, NeedFPUWarn
op_d9											; Skip warning message, could be fstcw
op_dd											; Skip warning message, could be fstsw
	modrm_jump_16_tbl op_dd_jump
; 0
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
	DCD fpu_bxsi, fpu_bxdi, fpu_bpsi, fpu_bpdi, fpu_siidx, fpu_diidx, fpu_disp16, fpu_bxidx
; 0x40
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
	DCD fpu_bxsid8, fpu_bxdid8, fpu_bpsid8, fpu_bpdid8, fpu_sidisp8, fpu_didisp8, fpu_bpdisp8, fpu_bxdisp8
; 0x80
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
	DCD fpu_bxsid16, fpu_bxdid16, fpu_bpsid16, fpu_bpdid16, fpu_sidisp16, fpu_didisp16, fpu_bpdisp16, fpu_bxdisp16
; 0xC0
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop
	DCD loop, loop, loop, loop, loop, loop, loop, loop

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	GLOBAL	fpu_r0_bp
fpu_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	fpu_r0
fpu_r0
	;------
	; Test if we possibly need to give a warning message.
	;------
;	lw		t0, NeedFPUWarn
;	bnez	t0, %f1
	;-------
	; Ignore = go back to loop.
	;-------
	b		loop						; Back to loop
	

; -- fpu [idx] --

fpu_bxsi
	add		r0, ebx, esi
	b		fpu_r0
fpu_bxdi
	add		r0, ebx, edi
	b		fpu_r0
fpu_bpsi
	add		r0, ebp, esi
	b		fpu_r0_bp
fpu_bpdi
	add		r0, ebp, edi
	b		fpu_r0_bp
	GLOBAL	fpu_siidx
fpu_siidx
	mov		r0, esi
	b		fpu_r0
	GLOBAL	fpu_diidx
fpu_diidx
	mov		r0, edi
	b		fpu_r0
fpu_disp16
	r0_from_disp16
	b		fpu_r0
	GLOBAL	fpu_bxidx
fpu_bxidx
	mov		r0, ebx
	b		fpu_r0

; -- fpu [idx+disp8] --

fpu_bxsid8
	r0_from_bxidxdisp8 esi
	b		fpu_r0
fpu_bxdid8
	r0_from_bxidxdisp8 edi
	b		fpu_r0
fpu_bpsid8
	r0_from_bpidxdisp8 esi
	b		fpu_r0_bp
fpu_bpdid8
	r0_from_bpidxdisp8 edi
	b		fpu_r0_bp
	GLOBAL	fpu_sidisp8
fpu_sidisp8
	r0_from_idx_disp8 esi
	b		fpu_r0
	GLOBAL	fpu_didisp8
fpu_didisp8
	r0_from_idx_disp8 edi
	b		fpu_r0
	GLOBAL	fpu_bpdisp8
fpu_bpdisp8
	r0_from_idx_disp8 ebp
	b		fpu_r0_bp
	GLOBAL	fpu_bxdisp8
fpu_bxdisp8
	r0_from_idx_disp8 ebx
	b		fpu_r0

; -- fpu [idx+disp16] --

fpu_bxsid16
	r0_from_bxidxdisp16 esi
	b		fpu_r0
fpu_bxdid16
	r0_from_bxidxdisp16 edi
	b		fpu_r0
fpu_bpsid16
	r0_from_bpidxdisp16  esi
	b		fpu_r0_bp
fpu_bpdid16
	r0_from_bpidxdisp16 edi
	b		fpu_r0_bp
fpu_sidisp16
	r0_from_idx_disp16 esi
	b		fpu_r0
fpu_didisp16
	r0_from_idx_disp16 edi
	b		fpu_r0
fpu_bpdisp16
	r0_from_idx_disp16 ebp
	b		fpu_r0_bp
fpu_bxdisp16
	r0_from_idx_disp16 ebx
	b		fpu_r0

	
; ------------------- E0 = LOOPNE/LOOPNZ ------------------------------
; We are not allowed to change any flags here!
;
op_e0
	mrs		r0, cpsr				; Save current flags to r0
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	mov		r2, ecx, lsl #16
	eor		ecx, r2, lsr #16
	sub		r2, #0x00010000			; Decrement CX
	orr		ecx, r2, lsr #16
	tst		r0, #0x40000000			; Is the Zero flag set?
	bne		restore_flags_from_r0	; If Zero flag is set, do not take the jump.
	cmp		r2, #0					; Is CX zero?
	addne	r12, r12, r1			; Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

; ------------------- E1 = LOOPE/LOOPZ --------------------------------
; We are not allowed to change any flags here!
;
op_e1
	mrs		r0, cpsr				; Save current flags to r0
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	mov		r2, ecx, lsl #16
	eor		ecx, r2, lsr #16
	sub		r2, #0x00010000			; Decrement CX
	orr		ecx, r2, lsr #16
	tst		r0, #0x40000000			; Is the Zero flag set?
	beq		restore_flags_from_r0	; If Zero flag is not set, do not take the jump.
	cmp		r2, #0					; Is CX zero?
	addne	r12, r12, r1			; Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

; ------------------- E2 = LOOP ---------------------------------------
; We are not allowed to change any flags here!
;
op_e2
	mrs		r0, cpsr				; Save current flags to r0
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	mov		r2, ecx, lsl #16
	eor		ecx, r2, lsr #16
	subs	r2, #0x00010000			; Decrement CX
	orr		ecx, r2, lsr #16
	addne	r12, r12, r1			; Adjust program counter by the jump amount, if CX != 0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags
	
; ------------------- E3 = JCXZ ---------------------------------------
; We are not allowed to change any flags here!
;
op_e3
	mrs		r0, cpsr				; Save current flags to r0
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	movs	r2, ecx, lsl #16		; Is CX zero?
	addeq	r12, r12, r1			; Adjust program counter by the jump amount, if CX == 0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

; ------------------- E8 = CALL near ----------------------------------
op_e8
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	ldr		r2,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	sub		r1, r12, r2				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1, r3, lr
	add		r1, r0					; r1 = current IP + jump amount
	lsl		r1, #16					; Make the result unsigned
	add		r12, r2, r1, lsr #16	; Adjust program counter by the jump amount
	b		loop

; ------------------- E9 = JMP near -----------------------------------
op_e9
	ldrb	r0,[r12],#1				; Load byte to r0, increment r12 by 1
	ldrsb	r1,[r12],#1				; Load sign-extended byte to r1, increment r12 by 1
	ldr		r2,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	orr		r0, r1, lsl #8			; r0 = low byte | (high byte << 8)
	sub		r1, r12, r2				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	add		r1, r0					; r1 = current IP + jump amount
	lsl		r1, #16					; Make the result unsigned
	add		r12, r2, r1, lsr #16	; Adjust program counter by the jump amount
	IF 0 = 1
	ldr		r0, =registers
	str		r12, [r0, #4*16]
	ENDIF	
	b		loop

; ------------------- EA = JMP FAR ------------------------------------
; Profiler: 2059, 24, 25.61, 52739, 0.03%
;
op_ea
	;-------
	; First get the jump target 
	;	r1 = new IP (offset)
	;	r2 = new CS (segment)
	;-------
	ldrb	r0,[r12]
	ldrb	r1,[r12, #1]
	ldrb	r2,[r12, #2]
	ldrb	r3,[r12, #3]
	orr		r1, r0, r1, lsl #8		; r1 = new logical IP
	orr		r2, r3, lsl #8			; r2 = new CS value
	GLOBAL	cpu_jmp_far_r1r2
cpu_jmp_far_r1r2
	;-------
	; Then determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		cpu_jmp_prot_r0r1r2		; Yes we are, go handle protected mode JMP FAR!
	;-------
	; Real mode JMP FAR handling
	;-------
	GLOBAL	cpu_jmp_real_r0r1r2
cpu_jmp_real_r0r1r2
	msr		cpsr_f, r0				; Restore flags
	mov		r12, r1					; r12 = r1 = new logical IP
	str		r2, [sp, #SP_CS_VALUE]	; Store new logical CS value
	lsl		r2, #REAL_SEGMENT_SHIFT
	str		r2, [sp, #SP_CS_BASE]	; Store new logical CS base
	calc_linear_address_r2
	str		r2, [sp, #SP_PHYS_CS]	; Store new physical CS into stack
	add		r12, r2					; r12 = new physical CS:IP = new logical IP + new physical CS:0000
	b		loop

	EXTERN	cpu_jmp_prot_r0r1r2
	
; ------------------- EB = JMP short ----------------------------------
op_eb
	ldrsb	r0,[r12],#1				; Load sign-extended byte to r0, increment r12 by 1
	add		r12, r0					; Adjust program counter by the jump amount
	b		loop

	ALIGN	4

; ------------------- F4 = HLT ----------------------------------------
;
	GLOBAL	op_f4					; Prot mode HLT used in Fragile Alliance
op_f4
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
op_f4_loop
	ldr		r2, [sp, #SP_IRQFLAG]
	mov		r1, #IRQ_ON
	cmp		r2, r1
	bne		op_f4_loop				; Nope, continue looping
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags

	LTORG
	
; ------------------- F6 = ??? r/m8 -----------------------------------
;
; All modrm variations supported!
;
op_f6
	modrm_jump_16_tbl op_f6_jump
; 0
	modrm_help_1_0 test, imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_0 not_b
	modrm_help_1_0 neg_b
	modrm_help_1_0 mul_b
	modrm_help_1_0 imul_b
	modrm_help_1_0 div_b
	modrm_help_1_0 idiv_b
;0x40
	modrm_help_1_40 test, imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_40 not_b
	modrm_help_1_40 neg_b
	modrm_help_1_40 mul_b
	modrm_help_1_40 imul_b
	modrm_help_1_40 div_b
	modrm_help_1_40 idiv_b
;0x80
	modrm_help_1_80 test, imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_80 not_b
	modrm_help_1_80 neg_b
	modrm_help_1_80 mul_b
	modrm_help_1_80 imul_b
	modrm_help_1_80 div_b
	modrm_help_1_80 idiv_b
;0xc0 = mod = 11b => register operand
	DCD test_al_imm8, test_cl_imm8, test_dl_imm8, test_bl_imm8, test_ah_imm8, test_ch_imm8, test_dh_imm8, test_bh_imm8
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD not_al, not_cl, not_dl, not_bl, not_ah, not_ch, not_dh, not_bh
	DCD neg_al, neg_cl, neg_dl, neg_bl, neg_ah, neg_ch, neg_dh, neg_bh
	DCD mul_al, mul_cl, mul_dl, mul_bl, mul_ah, mul_ch, mul_dh, mul_bh
	DCD imul_al, imul_cl, imul_dl, imul_bl, imul_ah, imul_ch, imul_dh, imul_bh
	DCD div_al, div_cl, div_dl, div_bl, div_ah, div_ch, div_dh, div_bh
	DCD idiv_al, idiv_cl, idiv_dl, idiv_bl, idiv_ah, idiv_ch, idiv_dh, idiv_bh

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
; ----- TEST -----

	EXTERN	test_EGA_r2_imm8
	EXTERN	test_MODEX_r2_imm8
	GLOBAL	test_r0_bp_imm8
test_r0_bp_imm8
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	test_r0_imm8
test_r0_imm8
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 test_RAM_imm8, test_EGA_r2_imm8, test_MODEX_r2_imm8
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
test_RAM_imm8
	ldrb	r0, [r2]
	ldrb	r1, [r12], #1
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially C and O)
	lsl		r0, #24					; r0 = byte at address [r2], shifted to high byte
	tst		r0, r1, lsl #24
	b		loop

	modrm_1_help test, test_r0, imm8
	
	ALIGN	4
	

; -- TEST register --

	MACRO 
	test_reg8l_imm8 $reg
	ldrb	r1, [r12], #1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	mov		r0, $reg, lsl #24
	tst		r0, r1, lsl #24
	b		loop
	MEND
	MACRO 
	test_reg8h_imm8 $reg
	ldrb	r1, [r12], #1
	mov		r0, #0
	msr		cpsr_f, r0				; Clear all flags (especially C and O)
	mov		r0, $reg, lsl #16
	tst		r0, r1, lsl #24
	b		loop
	MEND

test_al_imm8
	test_reg8l_imm8	eax
test_cl_imm8
	test_reg8l_imm8	ecx
test_dl_imm8
	test_reg8l_imm8	edx
test_bl_imm8
	test_reg8l_imm8	ebx
	
test_ah_imm8
	test_reg8h_imm8	eax
test_ch_imm8
	test_reg8h_imm8	ecx
test_dh_imm8
	test_reg8h_imm8	edx
test_bh_imm8
	test_reg8h_imm8	ebx

; ----- NOT -----

	GLOBAL	not_b_r0_bp
not_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	not_b_r0
not_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 not_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
not_b_RAM
	ldrb	r0, [r2]
	mvn		r0, r0
	strb	r0, [r2]
	b		loop

	modrm_1_help not_b, not_b_r0

	ALIGN	4


; -- NOT register --

	MACRO 
	not_reg8l $reg
	mvn		r0, $reg
	bfi		$reg, r0, #0, #8
	b		loop
	MEND
	MACRO 
	not_reg8h $reg
	mvn		r0, $reg
	lsr		r0, #8
	bfi		$reg, r0, #8, #8
	b		loop
	MEND

not_al
	not_reg8l eax
not_cl
	not_reg8l ecx
not_dl
	not_reg8l edx
not_bl
	not_reg8l ebx
not_ah
	not_reg8h eax
not_ch
	not_reg8h ecx
not_dh
	not_reg8h edx
not_bh
	not_reg8h ebx

; ----- NEG -----

	GLOBAL	neg_b_r0_bp
neg_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	neg_b_r0
neg_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 neg_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
neg_b_RAM
	ldrb	r0, [r2]
	lsl		r0, #24
	rsbs	r0, #0
	lsr		r0, #24
	strb	r0, [r2]
	b		complement_carry

	modrm_1_help neg_b, neg_b_r0

	ALIGN	4


; -- neg register --

	MACRO 
	neg_reg8l $reg
	mov		r0, $reg, lsl #24
	rsbs	r0, #0
	bic		$reg, #0xFF
	orr		$reg, r0, lsr #24
	b		complement_carry
	MEND
	MACRO 
	neg_reg8h $reg
	mov		r0, $reg, lsl #16
	and		r0, #0xFF000000
	rsbs	r0, #0
	bic		$reg, #0xFF00
	orr		$reg, r0, lsr #16
	b		complement_carry
	MEND

neg_al
	neg_reg8l eax
neg_cl
	neg_reg8l ecx
neg_dl
	neg_reg8l edx
neg_bl
	neg_reg8l ebx
neg_ah
	neg_reg8h eax
neg_ch
	neg_reg8h ecx
neg_dh
	neg_reg8h edx
neg_bh
	neg_reg8h ebx
	
; ----- MUL -----

	GLOBAL	mul_b_r0_bp
mul_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mul_b_r0
mul_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 mul_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
mul_b_RAM
	ldrb	r0, [r2]
	and		r1, eax, #0xFF
	mul		r2, r0, r1
	mov		r0, #0
	tst		r2, #0xFF00				; Is the result > 255?
	bfi		eax, r2, #0, #16
	beq		restore_flags_from_r0
	mov		r0, #(ARM_CARRY|ARM_OVER) ; Set Carry and Overflow flags if it is
	b		restore_flags_from_r0

	modrm_1_help mul_b, mul_b_r0

	ALIGN	4


; -- mul register --

	MACRO 
	mul_reg8l $reg
	; One of the operands is always AL and the result goes to AX
	; Carry and Overflow are set if the result is > 255, other flags are undeterminate
	and		r0, $reg, #0xFF
	and		r1, eax, #0xFF
	mul		r2, r0, r1
	mov		r0, #0
	tst		r2, #0xFF00				; Is the result > 255?
	bfi		eax, r2, #0, #16
	beq		restore_flags_from_r0
	mov		r0, #(ARM_CARRY|ARM_OVER) ; Set Carry and Overflow flags if it is
	b		restore_flags_from_r0
	MEND

	MACRO 
	mul_reg8h $reg
	; One of the operands is always AL and the result goes to AX
	; Carry and Overflow are set if the result is > 255, other flags are undeterminate
	ubfx	r0, $reg, #8, #8		; r0 = reg8h
	and		r1, eax, #0xFF
	mul		r2, r0, r1
	mov		r0, #0
	tst		r2, #0xFF00				; Is the result > 255?
	bfi		eax, r2, #0, #16
	beq		restore_flags_from_r0
	mov		r0, #(ARM_CARRY|ARM_OVER) ; Set Carry and Overflow flags if it is
	b		restore_flags_from_r0
	MEND

mul_al
	mul_reg8l eax
mul_cl
	mul_reg8l ecx
mul_dl
	mul_reg8l edx
mul_bl
	mul_reg8l ebx
mul_ah
	mul_reg8h eax
mul_ch
	mul_reg8h ecx
mul_dh
	mul_reg8h edx
mul_bh
	mul_reg8h ebx

; ----- IMUL -----

	GLOBAL	imul_b_r0_bp
imul_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	imul_b_r0
imul_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 imul_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
imul_b_RAM
	ldrsb	r0, [r2]				; Load signed byte
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	mul		r2, r1, r0
	mov		r0, #0
	bfi		eax, r2, #0, #16
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	sbfx	r2, eax, #8, #8			; r2 = signed AH value
	cmp		r2, r1, asr #8
	orrne	r0, #(ARM_CARRY|ARM_OVER)
	b		restore_flags_from_r0

	modrm_1_help imul_b, imul_b_r0

	ALIGN	4


; -- imul register --

	MACRO 
	imul_reg8l $reg
	; One of the operands is always AL and the result goes to AX
	; Carry and Overflow are set if the result is > 255, other flags are undeterminate
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags
	sbfx	r0, $reg, #0, #8		; r0 = signed reg8l value
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	mul		r2, r1, r0
	mov		r0, #0
	bfi		eax, r2, #0, #16
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	sbfx	r2, eax, #8, #8			; r2 = signed AH value
	cmp		r2, r1, asr #8
	orrne	r0, #(ARM_CARRY|ARM_OVER)
	b		restore_flags_from_r0
	MEND

	MACRO 
	imul_reg8h $reg
	; One of the operands is always AL and the result goes to AX
	; Carry and Overflow are set if the result is > 255, other flags are undeterminate
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags
	sbfx	r0, $reg, #8, #8		; r0 = signed reg8h value
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	mul		r2, r1, r0
	mov		r0, #0
	bfi		eax, r2, #0, #16
	sbfx	r1, eax, #0, #8			; r1 = signed AL value
	sbfx	r2, eax, #8, #8			; r2 = signed AH value
	cmp		r2, r1, asr #8
	orrne	r0, #(ARM_CARRY|ARM_OVER)
	b		restore_flags_from_r0
	MEND

imul_al
	imul_reg8l eax
imul_cl
	imul_reg8l ecx
imul_dl
	imul_reg8l edx
imul_bl
	imul_reg8l ebx
imul_ah
	imul_reg8h eax
imul_ch
	imul_reg8h ecx
imul_dh
	imul_reg8h edx
imul_bh
	imul_reg8h ebx


; --- DIV r/m8 ---
; Numerator is always AX, the result goes to AL and the remainder to AH.
; All flags are undeterminate.

	GLOBAL	div_b_r0_bp
div_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	div_b_r0
div_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 div_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
div_b_RAM
	ldrb	r2, [r2]				; Load byte
div_reg8_by_r2_subroutine
	IF SOFTWARE_DIV = 1
	;-------
	; Software division routine. Algorithm taken from
	; "http:;www.peter-cockerell.net/aalp/html/ch-6.html"
	; 	r0 = div (result)
	; 	r1 = mod (result)
	; 	r2 = rhs
	; 	r3 = lhs
	;	eax = count
	;-------
	teq  	r2, #0    				; Trap div by zero
	beq		div8_by_zero			; division by zero!!
	push	{eax}
	movs	r3, eax, lsl #16
	mov  	r1, #0    				; Init remainder
	mov  	r0, #0    				; and result
	mov  	eax, #16    				; Set up count
	bmi		%f2						; Skip first loop if highest bit of r3 set
1	subs 	eax, eax, #1  			; Get first 1 bit of lhs
	beq		%f3    					; into bit 31. Return if 0
	movs 	r3, r3, asl #1
	bpl  	%b1
2	movs 	r3, r3, asl #1  		; Get next bit into...
	adc  	r1, r1, r1   			; r1 for trial subtract
	cmp  	r1, r2    				; Can we subtract?
	subcs 	r1, r1, r2   			; Yes, so do
	adc  	r0, r0, r0   			; Shift carry into result
	subs 	eax, eax, #1  			; Next loop
	bne  	%b2
3	pop		{eax}
	cmp		r0, #0x100				; Is the result >= 256?
	bhs		div8_by_zero			; Yep, overflow!
	bfi		eax, r0, #0, #8			; AL = div (result)
	bfi		eax, r1, #8, #8			; AH = rem (result)
	b		loop					; Go back to loop
	ELSE
	mov		r0, #0x280
	orr		r0, #0x04000000			; #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		div8_by_zero			; Division by zero!! (See F7 opcode handler)
	mov		r1, #0
	strh	r1, [r0]				; REG_DIVCNT = DIV_32_32 = 0
	mov		r1, eax, lsl #16
	lsr		r1, #16
	str		r1, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1	ldrh	r1, [r0]				; while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		%b1
	ldr		r2,[r0, #0x20]			; #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	eax (AL) is the result of the division
	cmp		r2, #0x100
	bhs		.div8_by_zero			; If result is >= 256, divide overflow! (See F7 opcode handler)
	ldr		r1,[r0, #0x28]			; #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	r1 (AH) is the remainder of the division
	lsr		eax, #16
	orr		eax, r2, eax, lsl #16
	orr		eax, r1, lsl #8			; AX = AH<<8|AL
	b		loop
	ENDIF

	modrm_1_help div_b, div_b_r0

; -- div register --

	MACRO 
	div_reg8l $reg
	and		r2, $reg, #0xFF			; r2 = UNSIGNED register value
	b		div_reg8_by_r2_subroutine
	MEND
	MACRO 
	div_reg8h $reg
	ubfx	r2, $reg, #8, #8		; r2 = UNSIGNED register value
	b		div_reg8_by_r2_subroutine
	MEND

div_al
	div_reg8l eax
div_cl
	div_reg8l ecx
div_dl
	div_reg8l edx
div_bl
	div_reg8l ebx

div_ah
	div_reg8h eax
div_ch
	div_reg8h ecx
div_dh
	div_reg8h edx
div_bh
	div_reg8h ebx

; --- IDIV r/m8 ---
; Numerator is always AX, the result goes to AL and the remainder to AH.
; All flags are undeterminate.
;

	GLOBAL	idiv_b_r0_bp
idiv_b_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	idiv_b_r0
idiv_b_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 idiv_b_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
idiv_b_RAM
	ldrsb	r2, [r2]				; Load signed byte
idiv_reg8_by_r2_subroutine
	IF SOFTWARE_DIV = 1
	;-------
	; Software division routine. Algorithm taken from
	; "http:;www.peter-cockerell.net/aalp/html/ch-6.html"
	; 	r0 = div (result)
	; 	r1 = mod (result)
	; 	r2 = rhs
	; 	r3 = lhs
	;	eax = count
	;-------
	teq  	r2, #0    				; Trap div by zero
	beq		div8_by_zero			; division by zero!!
	push	{eax, ecx}
	movs	ecx, eax, lsl #16
	eor		r3, ecx, r2
	lsr		r3, #31					; r3 lowest bit tells the sign of the result
	orrmi	r3, #2					; Mod sign should be negative if lhs is negative
	rsbmi	ecx, #0					; Make lhs positive
	teq		r2, #0					; Is rhs negative?
	rsbmi	r2, #0					; If yes, make it positive
	mov  	r1, #0    				; Init remainder
	mov  	r0, #0    				; and result
	mov  	eax, #16    				; Set up count
1	subs 	eax, eax, #1  			; Get first 1 bit of lhs
	beq		%f3    					; into bit 31. Return if 0
	movs 	ecx, ecx, asl #1
	bpl  	%b1
2	movs 	ecx, ecx, asl #1  		; Get next bit into...
	adc  	r1, r1, r1   			; r1 for trial subtract
	cmp  	r1, r2    				; Can we subtract?
	subcs 	r1, r1, r2   			; Yes, so do
	adc  	r0, r0, r0   			; Shift carry into result
	subs 	eax, eax, #1  			; Next loop
	bne  	%b2
3	pop		{eax, ecx}
	and		r2, r3, #1
	add		r2, #0x80
	cmp		r0, r2					; Is the result > 127 or < -128?
	bhs		div8_by_zero			; Yep, overflow!
	tst		r3, #1					; Should we negate the result?
	rsbne	r0, #0					; Yep
	tst		r3, #2					; Should we negate the remainder?
	rsbne	r1, #0					; Yep
	bfi		eax, r0, #0, #8			; AL = div (result)
	bfi		eax, r1, #8, #8			; AH = rem (result)
	b		loop					; Go back to loop
	ELSE
	mov		r0, #0x280
	orr		r0, #0x04000000			; #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		.div8_by_zero			; Division by zero!!
	mov		r1, #0
	strh	r1, [r0]				; REG_DIVCNT = DIV_32_32 = 0
	mov		r1, eax, lsl #16
	asr		r1, #16					; r1 = SIGNED AX
	str		r1, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1	ldrh	r1, [r0]				; while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		%b1
	ldr		r2,[r0, #0x20]			; #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	eax (AL) is the result of the division
	mov		r1, #0x80000000
	cmp		r2, #0x80
	bge		.div8_by_zero
	cmp		r2, r1, asr #24
	blt		.div8_by_zero
	ldr		r1,[r0, #0x28]			; #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	r1 (AH) is the remainder of the division
	and		r2, #0xFF
	lsr		eax, #16
	orr		eax, r2, eax, lsl #16
	and		r1, #0xFF
	orr		eax, r1, lsl #8			; AX = AH<<8|AL
	b		loop
	ENDIF

	modrm_1_help idiv_b, idiv_b_r0

	MACRO 
	idiv_reg8l $reg
	sbfx	r2, $reg, #0, #8		; r2 = SIGNED register value
	b		idiv_reg8_by_r2_subroutine
	MEND
	MACRO 
	idiv_reg8h $reg
	sbfx	r2, $reg, #8, #8		; r2 = SIGNED register value
	b		idiv_reg8_by_r2_subroutine
	MEND

idiv_al
	idiv_reg8l eax
idiv_cl
	idiv_reg8l ecx
idiv_dl
	idiv_reg8l edx
idiv_bl
	idiv_reg8l ebx

idiv_ah
	idiv_reg8h eax
idiv_ch
	idiv_reg8h ecx
idiv_dh
	idiv_reg8h edx
idiv_bh
	idiv_reg8h ebx
	
	GLOBAL test_al_imm8
	GLOBAL test_cl_imm8
	GLOBAL test_dl_imm8
	GLOBAL test_bl_imm8
	GLOBAL test_ah_imm8
	GLOBAL test_ch_imm8
	GLOBAL test_dh_imm8
	GLOBAL test_bh_imm8
	GLOBAL not_al
	GLOBAL not_cl
	GLOBAL not_dl
	GLOBAL not_bl
	GLOBAL not_ah
	GLOBAL not_ch
	GLOBAL not_dh
	GLOBAL not_bh
	GLOBAL neg_al
	GLOBAL neg_cl
	GLOBAL neg_dl
	GLOBAL neg_bl
	GLOBAL neg_ah
	GLOBAL neg_ch
	GLOBAL neg_dh
	GLOBAL neg_bh
	GLOBAL mul_al
	GLOBAL mul_cl
	GLOBAL mul_dl
	GLOBAL mul_bl
	GLOBAL mul_ah
	GLOBAL mul_ch
	GLOBAL mul_dh
	GLOBAL mul_bh
	GLOBAL imul_al
	GLOBAL imul_cl
	GLOBAL imul_dl
	GLOBAL imul_bl
	GLOBAL imul_ah
	GLOBAL imul_ch
	GLOBAL imul_dh
	GLOBAL imul_bh
	GLOBAL div_al
	GLOBAL div_cl
	GLOBAL div_dl
	GLOBAL div_bl
	GLOBAL div_ah
	GLOBAL div_ch
	GLOBAL div_dh
	GLOBAL div_bh
	GLOBAL idiv_al
	GLOBAL idiv_cl
	GLOBAL idiv_dl
	GLOBAL idiv_bl
	GLOBAL idiv_ah
	GLOBAL idiv_ch
	GLOBAL idiv_dh
	GLOBAL idiv_bh

; ------------------- F7 = ??? r/m16 ----------------------------------
;
; All modrm variations supported!
;
op_f7
	modrm_jump_16_tbl op_f7_jump
; 0
	modrm_help_1_0 test, imm16
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	modrm_help_1_0 not_w
	modrm_help_1_0 neg_w
	modrm_help_1_0 mul_w
	modrm_help_1_0 imul_w
	modrm_help_1_0 div_w
	modrm_help_1_0 idiv_w
;0x40
	modrm_help_1_40 test, imm16
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

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
; --- TEST r/m16, imm16 --

	EXTERN	test_EGA_r2_imm16
	EXTERN	test_MODEX_r2_imm16
	GLOBAL	test_r0_bp_imm16
test_r0_bp_imm16
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	test_r0_imm16
test_r0_imm16
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 test_RAM_imm16, test_EGA_r2_imm16, test_MODEX_r2_imm16
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
test_RAM_imm16
	; ----- Get the word from RAM into r0 -----
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	ldrb	r2, [r12], #1
	orr		r0, r1, lsl #8
	; ----- Get the imm16 value into high halfword of r2 -----
	ldrb	r1, [r12], #1
	lsl		r2, #16
	orr		r2, r1, lsl #24
	; ----- Finally test the values -----
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	tst		r2, r0, lsl #16			; This will clear the C flag!
	b		loop

	modrm_1_help test, test_r0, imm16

	ALIGN	4

; -- TEST reg16, imm16 --

	MACRO 
	test_reg16_imm16 $reg
	ldrb	r0, [r12], #1
	ldrb	r1, [r12], #1
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially C and O)
	orr		r0, r1, lsl #8
	mov		r1, $reg, lsl #16
	tst		r1, r0, lsl #16
	b		loop
	MEND

test_ax_imm16
	test_reg16_imm16 eax
test_cx_imm16
	test_reg16_imm16 ecx
test_dx_imm16
	test_reg16_imm16 edx
test_bx_imm16
	test_reg16_imm16 ebx
test_sp_imm16
	test_reg16_imm16 esp
test_bp_imm16
	test_reg16_imm16 ebp
test_si_imm16
	test_reg16_imm16 esi
test_di_imm16
	test_reg16_imm16 edi

; --- NOT r/m16 ---

	GLOBAL	not_w_r0_bp
not_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	not_w_r0
not_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 not_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
not_w_RAM
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	mvn		r0, r0
	mvn		r1, r1
	strb	r0, [r2]
	strb	r1, [r2, #1]
	b		loop

	modrm_1_help not_w, not_w_r0

	ALIGN	4

	MACRO 
	not_reg16 $reg
	mvn		r0, $reg
	bfi		$reg, r0, #0, #16
	b		loop
	MEND

not_ax
	not_reg16 eax
not_cx
	not_reg16 ecx
not_dx
	not_reg16 edx
not_bx
	not_reg16 ebx
not_sp
	not_reg16 esp
not_bp
	not_reg16 ebp
not_si
	not_reg16 esi
not_di
	not_reg16 edi

; --- NEG r/m16 ---

	GLOBAL	neg_w_r0_bp
neg_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	neg_w_r0
neg_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 neg_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
neg_w_RAM
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	lsl		r0, #16
	orr		r0, r1, lsl #24
	rsbs	r0, #0					; reg = 0 - reg, setting flags
	lsr		r0, #16
	strb	r0,[r2]					; Save low byte
	lsr		r0, #8
	strb	r0,[r2, #1]				; Save high byte
	b		complement_carry

	modrm_1_help neg_w, neg_w_r0

	ALIGN	4

	MACRO 
	neg_reg16 $reg
	lsl		r0, $reg, #16
	eor		$reg, r0, lsr #16
	rsbs	r0, #0				; reg = 0 - reg, setting flags
	orr		$reg, r0, lsr #16
	b		complement_carry
	MEND

neg_ax
	neg_reg16 eax
neg_cx
	neg_reg16 ecx
neg_dx
	neg_reg16 edx
neg_bx
	neg_reg16 ebx
neg_sp
	neg_reg16 esp
neg_bp
	neg_reg16 ebp
neg_si
	neg_reg16 esi
neg_di
	neg_reg16 edi

; --- MUL r/m16 ---
; One of the operands is always AX and the result goes to DX:AX
; Carry and Overflow are set if DX != 0, other flags are undeterminate

	GLOBAL	mul_w_r0_bp
mul_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	mul_w_r0
mul_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 mul_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
mul_w_RAM
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	orr		r0, r1, lsl #8
	ubfx	r1, eax, #0, #16
	mul		r2, r0, r1
	movs	r1, r2, lsr #16			; r1 = new DX value
	bfi		eax, r2, #0, #16
	bfi		edx, r1, #0, #16
	mov		r0, #0
	beq		restore_flags_from_r0
	mov		r0, #0x30000000			; Set Carry and Overflow flags if DX is != 0
	b		restore_flags_from_r0

	modrm_1_help mul_w, mul_w_r0

	ALIGN	4


; -- mul registers --

	MACRO 
	mul_reg16 $reg
	ubfx	r0, $reg, #0, #16
	ubfx	r1, eax, #0, #16
	mul		r2, r0, r1
	movs	r1, r2, lsr #16			; r1 = new DX value
	bfi		eax, r2, #0, #16
	bfi		edx, r1, #0, #16
	mov		r0, #0
	beq		restore_flags_from_r0
	mov		r0, #0x30000000			; Set Carry and Overflow flags if DX is != 0
	b		restore_flags_from_r0
	MEND

mul_ax
	mul_reg16 eax
mul_cx
	mul_reg16 ecx
mul_dx
	mul_reg16 edx
mul_bx
	mul_reg16 ebx
mul_sp
	mul_reg16 esp
mul_bp
	mul_reg16 ebp
mul_si
	mul_reg16 esi
mul_di
	mul_reg16 edi

; --- IMUL r/m16 ---
; One of the operands is always AX and the result goes to DX:AX
; TODO! Overflow and Carry flag handling

	GLOBAL	imul_w_r0_bp
imul_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	imul_w_r0
imul_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 imul_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
imul_w_RAM
	ldrb	r0, [r2]
	ldrsb	r1, [r2, #1]			; Get high byte
	sbfx	r2, eax, #0, #16		; r2 = SIGNED AX value
	orr		r0, r1, lsl #8			; r0 = SIGNED halfword
	mul		r1, r0, r2
	mov		r0, r1, lsr #16
	bfi		eax, r1, #0, #16
	bfi		edx, r0, #0, #16
	b		loop

	modrm_1_help imul_w, imul_w_r0

	ALIGN	4

; -- imul registers --

	MACRO 
	imul_reg16 $reg
	sbfx	r0, $reg, #0, #16		; r0 = SIGNED halfword
	sbfx	r2, eax, #0, #16		; r2 = SIGNED AX value
	mul		r1, r0, r2
	mov		r0, r1, lsr #16
	bfi		eax, r1, #0, #16
	bfi		edx, r0, #0, #16
	b		loop
	MEND

imul_ax
	imul_reg16 eax
imul_cx
	imul_reg16 ecx
imul_dx
	imul_reg16 edx
imul_bx
	imul_reg16 ebx
imul_sp
	imul_reg16 esp
imul_bp
	imul_reg16 ebp
imul_si
	imul_reg16 esi
imul_di
	imul_reg16 edi

; --- DIV r/m16 ---
; Numerator is always DX:AX, the result goes to AX and the remainder to DX.
; All flags are undeterminate.
;
; INT 00 C - CPU-generated - DIVIDE ERROR
;
; Desc: Generated if the divisor of a DIV or IDIV instruction is zero or the quotient overflows the result register
;		DX and AX will be unchanged. 
;
; Notes: On an 8086/8088, the return address points to the following instruction.
;		 On an 80286+, the return address points to the divide instruction.
;		 An 8086/8088 will generate this interrupt if the result of a division is 80h (byte) or 8000h (word) 
;

	GLOBAL	div_w_r0_bp
div_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	div_w_r0
div_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 div_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
div_w_RAM
	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]			; Get high byte
	orr		r2, r0, r1, lsl #8		; r2 = UNSIGNED halfword
div_reg16_by_r2_subroutine
	IF SOFTWARE_DIV = 1
	;-------
	; Software division routine. Algorithm taken from
	; "http:;www.peter-cockerell.net/aalp/html/ch-6.html"
	; 	r0 = div (result)
	; 	r1 = mod (result)
	; 	r2 = rhs
	; 	r3 = lhs
	;	eax = count
	;-------
	teq  	r2, #0    				; Trap div by zero
	beq		div16_by_zero			; division by zero!!
	push	{eax}
	mov		r3, eax, lsl #16
	mov		r1, edx, lsl #16		; r1 = DX in high 16 bits, low 16 bits = 0
	orrs	r3, r1, r3, lsr #16		; r3 = DX:AX = lhs
	mov  	r1, #0    				; Init remainder
	mov  	r0, #0    				; and result
	mov  	eax, #32    				; Set up count
	bmi		%f2						; If highest bit of r3 set, skip the first loop
1	subs 	eax, eax, #1  			; Get first 1 bit of lhs
	beq		%f3    					; into bit 31. Return if 0
	movs 	r3, r3, asl #1
	bpl  	%b1
2	movs 	r3, r3, asl #1  		; Get next bit into...
	adc  	r1, r1, r1   			; r1 for trial subtract
	cmp  	r1, r2    				; Can we subtract?
	subcs 	r1, r1, r2   			; Yes, so do
	adc  	r0, r0, r0   			; Shift carry into result
	subs 	eax, eax, #1  			; Next loop
	bne  	%b2
3	pop		{eax}
	cmp		r0, #0x10000			; Is the result >= 65536?
	bhs		div16_by_zero			; Yep, overflow!
	bfi		eax, r0, #0, #16
	bfi		edx, r1, #0, #16
	b		loop					; Go back to loop
	ELSE
	;-------
	; Nintendo DS division, using the math coprocessor
	;-------
	mov		r0, #0x280
	orr		r0, #0x04000000			; #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		div16_by_zero			; Division by zero!!
	mov		r3, eax, lsl #16
	movs	r1, edx, lsl #16		; r1 = DX in high 16 bits, low 16 bits = 0
	bmi		div16_slow				; If DX >= 0x8000 use the slower 64bit/32bit divide
	strh	r1, [r0]				; REG_DIVCNT = DIV_32_32 = 0
	orr		r1, r3, lsr #16			; r1 = DX:AX
	str		r1, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1	ldrh	r1, [r0]				; while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		%b1
	ldr		r2,[r0, #0x20]			; #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	r2 is the result of the division
	cmp		r2, #0x10000			; Is the result >= 65536?
	bhs		div16_by_zero			; Yep, overflow!
	ldr		r0,[r0, #0x28]			; #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	r0 is the remainder of the division
	lsr		eax, #16
	lsr		edx, #16
	orr		eax, r2, eax, lsl #16
	orr		edx, r0, edx, lsl #16
	b		loop					; Go back to loop
div16_slow
	orr		r3, r1, r3, lsr #16		; r3 = DX:AX
	mov		r1, #1
	strh	r1, [r0]				; REG_DIVCNT = DIV_64_32 = 1
	mov		r1, #0
	str		r3, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r1, [r0, #0x14]			; #define REG_DIV_NUMER_H		(*(vs32*) (0x04000294)),	REG_DIV_NUMER_H = 0
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
	b		%b1
	ENDIF
	
CPU_INT_EXCEPTION		EQU		0x2
	
	;-------
	; Handle divide by zero (= divide overflow)
	;-------
	GLOBAL	div32_by_zero
div32_by_zero
div16_by_zero
div8_by_zero
	ldr		r12, [sp, #SP_EX_CSIP]	; Get the starting address of this opcode
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	sub		r12, #1					; Count the actual opcode itself as well.
	;-------
	; If in prot mode, generate EXCEPTION(0)
	;-------
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	movne	r0, #0
	movne	r1, #CPU_INT_EXCEPTION
	movne	r2, #0
	bne		exception_cont_r0r1r2
	;-------
	; Real mode, call the INT00 vector
	;-------
	push_flags_16 r0, r2, r3
	mov		r2, #0
	calc_linear_address_r2
	ldr		r2,[r2]					; Get interrupt vector address, high halfword = segment, low halfword = offset
	; ----- Then store current CS:IP to stack
	ldr		r0, [sp, #SP_CS_VALUE]	; r0 = Current logical CS
	push_hword r0, r1, r3
	ldr		r1, [sp, #SP_PHYS_CS]	; r1 = Current physical CS
	sub		r1, r12, r1				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1, r0, r3
	; ----- Then get new logical IP (zero-extended) to r12
	mov		r12, r2, lsl #16
	; ----- Then get new logical CS (zero-extended) to r0
	mov		r0, r2, lsr #16			; Now r0 = new logical CS
	; ----- Then save the new logical CS
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]
	str		r2, [sp, #SP_CS_BASE]
	; ----- And finally calculate new physical IP
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]	; Store new physical CS into stack
	add		r12, r2, r12, lsr #16	; r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	b		loop

	modrm_1_help div_w, div_w_r0

; -- div register --
	
	MACRO 
	div_reg16 $reg
	ubfx	r2, $reg, #0, #16		; r2 = UNSIGNED register value
	b		div_reg16_by_r2_subroutine
	MEND

div_ax
	div_reg16 eax
div_cx
	div_reg16 ecx
div_dx
	div_reg16 edx
div_bx
	div_reg16 ebx
div_sp
	div_reg16 esp
div_bp
	div_reg16 ebp
div_si
	div_reg16 esi
div_di
	div_reg16 edi

; --- IDIV r/m16 ---
; Numerator is always DX:AX, the result goes to AX and the remainder to DX.
; All flags are undeterminate.
;
; INT 00 C - CPU-generated - DIVIDE ERROR
;
; Desc: Generated if the divisor of a DIV or IDIV instruction is zero or the quotient overflows the result register
;		DX and AX will be unchanged. 
;
; Notes: On an 8086/8088, the return address points to the following instruction.
;		 On an 80286+, the return address points to the divide instruction.
;		 An 8086/8088 will generate this interrupt if the result of a division is 80h (byte) or 8000h (word) 
;
	GLOBAL	idiv_w_r0_bp
idiv_w_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	idiv_w_r0
idiv_w_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 idiv_w_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
idiv_w_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrsb	r1,[r2, #1]				; Get signed high byte
	orr		r2, r0, r1, lsl #8		; r2 = SIGNED halfword
idiv_reg16_by_r2_subroutine
	IF SOFTWARE_DIV = 1
	;-------
	; Software division routine. Algorithm taken from
	; "http:;www.peter-cockerell.net/aalp/html/ch-6.html"
	; 	r0 = div (result)
	; 	r1 = mod (result)
	; 	r2 = rhs
	;	r3 = signs (bit 0 = div sign, bit 1 = mod sign)
	;	eax = count
	; 	ecx = lhs
	;-------
	teq  	r2, #0    				; Trap div by zero
	beq		div16_by_zero			; division by zero!!
	push	{eax, ecx}
	mov		ecx, eax, lsl #16
	mov		r1, edx, lsl #16		; r1 = DX in high 16 bits, low 16 bits = 0
	orrs	ecx, r1, ecx, lsr #16		; ecx = DX:AX = lhs, sign flag set if negative
	eor		r3, ecx, r2
	lsr		r3, #31					; r3 lowest bit tells the sign of the result
	orrmi	r3, #2					; Mod sign should be negative if lhs is negative
	rsbmi	ecx, #0					; Make lhs positive
	teq		r2, #0					; Is rhs negative?
	rsbmi	r2, #0					; If yes, make it positive
	mov  	r1, #0    				; Init remainder
	mov  	r0, #0    				; and result
	mov  	eax, #32    				; Set up count
1	subs 	eax, eax, #1  			; Get first 1 bit of lhs
	beq		%f3    					; into bit 31. Return if 0
	movs 	ecx, ecx, ASL #1
	bpl  	%b1
2	movs 	ecx, ecx, ASL #1  		; Get next bit into...
	adc  	r1, r1, r1   			; r1 for trial subtract
	cmp  	r1, r2    				; Can we subtract?
	subcs 	r1, r1, r2   			; Yes, so do
	adc  	r0, r0, r0   			; Shift carry into result
	subs 	eax, eax, #1  			; Next loop
	bne  	%b2
3	pop		{eax, ecx}
	and		r2, r3, #1
	add		r2, #0x8000
	cmp		r0, r2					; Is the result > 32767 or < -32768?
	bhs		div16_by_zero			; Yep, overflow!
	tst		r3, #1					; Should we negate the result?
	rsbne	r0, #0					; Yep
	tst		r3, #2					; Should we negate the remainder?
	rsbne	r1, #0					; Yep
	bfi		eax, r0, #0, #16
	bfi		edx, r1, #0, #16
	b		loop					; Go back to loop
	ELSE
	mov		r0, #0x280
	orr		r0, #0x04000000			; #define REG_DIVCNT			(*(vu16*)(0x04000280))
	cmp		r2, #0
	beq		.div16_by_zero			; Division by zero!!
	mov		r3, eax, lsl #16
	mov		r1, edx, lsl #16		; r1 = DX in high 16 bits, low 16 bits = 0
	strh	r1, [r0]				; REG_DIVCNT = DIV_32_32 = 0
	orr		r1, r3, lsr #16			; r1 = DX:AX
	str		r1, [r0, #0x10]			; #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r2, [r0, #0x18]			; #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
1	ldrh	r1, [r0]				; while(REG_DIVCNT & DIV_BUSY)
	tst		r1, #0x8000
	bne		%b1
	ldr		r2,[r0, #0x20]			; #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	r2 is the result of the division
	cmp		r2, #0x8000				; Is the result >= 32768?
	bge		.div16_by_zero			; Yep, overflow!
	mov		r1, #0x80000000
	cmp		r2, r1, asr #16			; Is the result < -32768?
	blt		.div16_by_zero			; Yep, overflow!
	ldr		r0,[r0, #0x28]			; #define REG_DIVREM_RESULT_L	(*(vs32*) (0x040002A8)),	r0 is the remainder of the division
	lsr		eax, #16
	lsr		edx, #16
	orr		eax, r2, lsl #16
	orr		edx, r0, lsl #16
	ror		eax, #16
	ror		edx, #16
	b		loop					; Go back to loop
	ENDIF

	modrm_1_help idiv_w, idiv_w_r0

; -- idiv register --

	MACRO 
	idiv_reg16 $reg
	sbfx	r2, $reg, #0, #16		; r2 = SIGNED register value
	b		idiv_reg16_by_r2_subroutine
	MEND

idiv_ax
	idiv_reg16 eax
idiv_cx
	idiv_reg16 ecx
idiv_dx
	idiv_reg16 edx
idiv_bx
	idiv_reg16 ebx
idiv_sp
	idiv_reg16 esp
idiv_bp
	idiv_reg16 ebp
idiv_si
	idiv_reg16 esi
idiv_di
	idiv_reg16 edi

	LTORG								; Dump the current literal pool here

	GLOBAL  test_ax_imm16 
	GLOBAL  test_cx_imm16 
	GLOBAL  test_dx_imm16 
	GLOBAL  test_bx_imm16 
	GLOBAL  test_sp_imm16 
	GLOBAL  test_bp_imm16 
	GLOBAL  test_si_imm16 
	GLOBAL  test_di_imm16
	GLOBAL  not_ax 
	GLOBAL  not_cx 
	GLOBAL  not_dx 
	GLOBAL  not_bx 
	GLOBAL  not_sp 
	GLOBAL  not_bp 
	GLOBAL  not_si 
	GLOBAL  not_di
	GLOBAL  neg_ax 
	GLOBAL  neg_cx 
	GLOBAL  neg_dx 
	GLOBAL  neg_bx 
	GLOBAL  neg_sp 
	GLOBAL  neg_bp 
	GLOBAL  neg_si 
	GLOBAL  neg_di
	GLOBAL  mul_ax 
	GLOBAL  mul_cx 
	GLOBAL  mul_dx 
	GLOBAL  mul_bx 
	GLOBAL  mul_sp 
	GLOBAL  mul_bp 
	GLOBAL  mul_si 
	GLOBAL  mul_di
	GLOBAL  imul_ax 
	GLOBAL  imul_cx 
	GLOBAL  imul_dx 
	GLOBAL  imul_bx 
	GLOBAL  imul_sp 
	GLOBAL  imul_bp 
	GLOBAL  imul_si 
	GLOBAL  imul_di
	GLOBAL  div_ax 
	GLOBAL  div_cx 
	GLOBAL  div_dx 
	GLOBAL  div_bx 
	GLOBAL  div_sp 
	GLOBAL  div_bp 
	GLOBAL  div_si 
	GLOBAL  div_di
	GLOBAL  idiv_ax 
	GLOBAL  idiv_cx 
	GLOBAL  idiv_dx 
	GLOBAL  idiv_bx 
	GLOBAL  idiv_sp 
	GLOBAL  idiv_bp 
	GLOBAL  idiv_si 
	GLOBAL  idiv_di

; ------------------- F8 = CLC ----------------------------------------
op_f8
	mrs		r0, cpsr				; Save current flags to r0
	bic		r0, #0x20000000			; Clear the Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

; ------------------- F9 = STC ----------------------------------------
op_f9
	mrs		r0, cpsr				; Save current flags to r0
	orr		r0, #0x20000000			; Set the Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	
; ------------------- FA = CLI ----------------------------------------
; op_fa_CLI: see "pic.s"
;
	
; ------------------- FB = STI ----------------------------------------
; op_fb_STI: see "pic.s"
;
	
; ------------------- FC = CLD ----------------------------------------
; Clear Direction Flag. Here we fix all the function pointers for the
; functions that use the direction flag (so that the functions themselves
; do not have to check the flag). CLD/STD are executed much less frequently
; than the functions themselves.

op_fc
	ldr		r1,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	bic		r1,#FLAG_DF				; Clear the "Direction" bit
	str		r1,[sp, #SP_FLAGS]		; Save the EXTRAFLAGS value
	b		loop

; ------------------- FD = STD ----------------------------------------
; Set Direction Flag. Here we fix all the function pointers for the
; functions that use the direction flag (so that the functions themselves
; do not have to check the flag). CLD/STD are executed much less frequently
; than the functions themselves.
;
op_fd
	ldr		r1,[sp, #SP_FLAGS]		; Get the EXTRAFLAGS value
	orr		r1,#FLAG_DF				; Set the "Direction" bit
	str		r1,[sp, #SP_FLAGS]		; Save the EXTRAFLAGS value
	b		loop

	LTORG								; Dump the current literal pool here

	ALIGN	4
	
; ------------------- FE = INC/DEC r/m8 -------------------------------
; INC x is like ADD x,1, except the Carry flag is not changed.
;
; All modrm variations supported!
;
	GLOBAL	op_fe
op_fe
	modrm_jump_16_tbl op_fe_jump
; 0
	modrm_help_1_0 inc_byte
	modrm_help_1_0 dec_byte
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_fe_callback, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x40
	modrm_help_1_40 inc_byte
	modrm_help_1_40 dec_byte
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x80
	modrm_help_1_80 inc_byte
	modrm_help_1_80 dec_byte
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0xc0 = mod = 11b => register operand
	DCD inc_al, inc_cl, inc_dl, inc_bl, inc_ah, inc_ch, inc_dh, inc_bh
	DCD dec_al, dec_cl, dec_dl, dec_bl, dec_ah, dec_ch, dec_dh, dec_bh
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG
	
	EXTERN	op_fe_callback
	
; ----- INC -----

	EXTERN	inc_byte_EGA_r2
	EXTERN	inc_byte_MODEX_r2
	GLOBAL	inc_byte_r0_bp
inc_byte_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	inc_byte_r0
inc_byte_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 inc_byte_RAM, inc_byte_EGA_r2, inc_byte_MODEX_r2
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
inc_byte_RAM
	ldrb	r1,[r2]					; Get byte from RAM
	mrs		r0,cpsr					; Get original flags to r0.
	lsl		r1, #24
	adds	r1, #0x01000000			; Add 1 to the high byte
	lsr		r1, #24
	strb	r1,[r2]					; Save low byte
	; ----- Fix the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags + original Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

	modrm_1_help inc_byte, inc_byte_r0

	ALIGN	4

; --- INC reg8 ---

	MACRO 
	inc_reg8l $reg
	; ----- Save the current flags (namely Carry)
	mrs		r0,cpsr					; r0 = Current flags
	; ----- Perform the INC reg8l operation
	mov		r1, $reg, lsl #24
	adds	r1, #0x01000000			; Perform the INC using the highest byte and setting flags
	bic		$reg, #0xFF
	orr		$reg, r1, lsr #24
	; ----- Restore the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Original Carry flag
	bic		r1, #0x20000000			; r1 = All but the carry flag
	orr		r0, r1					; r0 = new flags
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

	MACRO 
	inc_reg8h $reg
	; ----- Save the current flags (namely Carry)
	mrs		r0,cpsr					; r0 = Current flags
	; ----- Perform the INC reg8h operation
	mov		r1, $reg, lsl #16
	and		r1, #0xFF000000
	adds	r1, #0x01000000			; Perform the INC using the highest byte and setting flags
	bic		$reg, #0xFF00
	orr		$reg, r1, lsr #16
	; ----- Restore the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Original Carry flag
	bic		r1, #0x20000000			; r1 = All but the carry flag
	orr		r0, r1					; r0 = new flags
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

inc_al
	inc_reg8l eax
inc_cl
	inc_reg8l ecx
inc_dl
	inc_reg8l edx
inc_bl
	inc_reg8l ebx
inc_ah
	inc_reg8h eax
inc_ch
	inc_reg8h ecx
inc_dh
	inc_reg8h edx
inc_bh
	inc_reg8h ebx

; ----- DEC -----

	EXTERN	dec_byte_EGA_r2
	EXTERN	dec_byte_MODEX_r2
	GLOBAL 	dec_byte_r0_bp
dec_byte_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	dec_byte_r0
dec_byte_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 dec_byte_RAM, dec_byte_EGA_r2, dec_byte_MODEX_r2
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
dec_byte_RAM
	ldrb	r1,[r2]					; Get byte from RAM
	mrs		r0,cpsr					; Get original flags to r0.
	lsl		r1, #24
	subs	r1, #0x01000000			; Add 1 to the high byte
	lsr		r1, #24
	strb	r1,[r2]					; Save low byte
	; ----- Fix the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags + original Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

	modrm_1_help dec_byte, dec_byte_r0

	ALIGN	4

; --- DEC reg8 ---

	MACRO 
	dec_reg8l $reg
	; ----- Save the current flags (namely Carry)
	mrs		r0,cpsr					; r0 = Current flags
	; ----- Perform the dec reg8l operation
	mov		r1, $reg, lsl #24
	subs	r1, #0x01000000			; Perform the dec using the highest byte and setting flags
	bic		$reg, #0xFF
	orr		$reg, r1, lsr #24
	; ----- Restore the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Original Carry flag
	bic		r1, #0x20000000			; r1 = All but the carry flag
	orr		r0, r1					; r0 = new flags
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

	MACRO 
	dec_reg8h $reg
	; ----- Save the current flags (namely Carry)
	mrs		r0,cpsr					; r0 = Current flags
	; ----- Perform the dec reg8h operation
	mov		r1, $reg, lsl #16
	and		r1, #0xFF000000
	subs	r1, #0x01000000			; Perform the dec using the highest byte and setting flags
	bic		$reg, #0xFF00
	orr		$reg, r1, lsr #16
	; ----- Restore the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Original Carry flag
	bic		r1, #0x20000000			; r1 = All but the carry flag
	orr		r0, r1					; r0 = new flags
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	MEND

dec_al
	dec_reg8l eax
dec_cl
	dec_reg8l ecx
dec_dl
	dec_reg8l edx
dec_bl
	dec_reg8l ebx
dec_ah
	dec_reg8h eax
dec_ch
	dec_reg8h ecx
dec_dh
	dec_reg8h edx
dec_bh
	dec_reg8h ebx

	GLOBAL	inc_al
	GLOBAL	inc_cl
	GLOBAL	inc_dl
	GLOBAL	inc_bl
	GLOBAL	inc_ah
	GLOBAL	inc_ch
	GLOBAL	inc_dh
	GLOBAL	inc_bh
	GLOBAL	dec_al
	GLOBAL	dec_cl
	GLOBAL	dec_dl
	GLOBAL	dec_bl
	GLOBAL	dec_ah
	GLOBAL	dec_ch
	GLOBAL	dec_dh
	GLOBAL	dec_bh

; ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
;
; All modrm variations supported!
;
	GLOBAL	op_ff
op_ff
	modrm_jump_16_tbl op_ff_jump
; 0 (idx only)
	modrm_help_1_0 inc_word
	modrm_help_1_0 dec_word
	modrm_help_1_0 call_near
	modrm_help_1_0 call_far
	modrm_help_1_0 jmp_near
	modrm_help_1_0 jmp_far
	modrm_help_1_0 push_word
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x40 (+disp8)
	modrm_help_1_40 inc_word
	modrm_help_1_40 dec_word
	modrm_help_1_40 call_near
	modrm_help_1_40 call_far
	modrm_help_1_40 jmp_near
	modrm_help_1_40 jmp_far
	modrm_help_1_40 push_word
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0x80
	modrm_help_1_80 inc_word
	modrm_help_1_80 dec_word
	modrm_help_1_80 call_near
	modrm_help_1_80 call_far
	modrm_help_1_80 jmp_near
	modrm_help_1_80 jmp_far
	modrm_help_1_80 push_word
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
;0xC0
	DCD op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47		; INC AX ... INC DI
	DCD op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f		; DEC AX ... DEC DI
	DCD call_near_ax, call_near_cx, call_near_dx, call_near_bx, call_near_sp, call_near_bp, call_near_si, call_near_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD jmp_near_ax, jmp_near_cx, jmp_near_dx, jmp_near_bx, jmp_near_sp, jmp_near_bp, jmp_near_si, jmp_near_di
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown
	DCD op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57		; PUSH AX ... PUSH DI
	DCD unknown, unknown, unknown, unknown, unknown, unknown, unknown, unknown

	AREA cpu_code, CODE, READONLY
	ALIGN	4

	LTORG


; ----- INC -----

	GLOBAL	inc_word_r0_bp
inc_word_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	inc_word_r0
inc_word_r0
	mem_handler_jump_r0r3 inc_word_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
inc_word_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrb	r1,[r2, #1]				; Get high byte
	lsl		r0, #16
	orr		r1, r0, r1, lsl #24		; r1 = low byte | (high byte << 8) = the value to increment
	mrs		r0,cpsr					; Get original flags to r0.
	adds	r1, #0x00010000			; Add 1 to the value
	lsr		r1, #16
	strb	r1,[r2]					; Save low byte
	lsr		r1, #8
	strb	r1,[r2, #1]				; Save high byte
	; ----- Fix the Carry flag
	mrs		r1,cpsr					; r1 = Flags after the increment
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags + original Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

	modrm_1_help inc_word, inc_word_r0

	ALIGN	4
	

; ----- DEC -----

	EXTERN	dec_word_EGA
	GLOBAL	dec_word_r0_bp
dec_word_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	dec_word_r0
dec_word_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 dec_word_RAM, dec_word_EGA, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
dec_word_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrb	r1,[r2, #1]				; Get high byte
	lsl		r0, #16
	orr		r1, r0, r1, lsl #24		; r1 = low byte | (high byte << 8) = the value to decrement
	mrs		r0,cpsr					; Get original flags to r0.
	subs	r1, #0x00010000			; Sub 1 from the value
	lsr		r1, #16
	strb	r1,[r2]					; Save low byte
	lsr		r1, #8
	strb	r1,[r2, #1]				; Save high byte
	; ----- Fix the Carry flag
	mrs		r1,cpsr					; r1 = Flags after subtraction
	and		r0, #0x20000000			; r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	orr		r0, r1					; r0 = new flags + original Carry flag
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

	modrm_1_help dec_word, dec_word_r0
	
	ALIGN	4
	

; --- CALL NEAR [address] ---

	GLOBAL	call_near_r0_bp
call_near_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	call_near_r0
call_near_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 call_near_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
call_near_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrb	r2,[r2, #1]
	ldr		r1,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	orr		r0, r2, lsl #8			; Now r0 = new logical IP
	sub		r2, r12, r1				; r2 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	add		r12, r1, r0				; New physical IP = physical CS:0000 + new logical IP
	push_hword r2, r0, r1
	b		loop

	modrm_1_help call_near, call_near_r0
	
	ALIGN	4


	MACRO 
	call_near_reg16 $reg
	ldr		r1,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	sub		r2, r12, r1				; r2 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	ubfx	r0, $reg, #0, #16		; Before push, in case the opcode is "call near sp" (very unlikely!)
	add		r12, r1, r0				; New PC = physical base + (CS<<4) + new logical IP
	push_hword r2, r0, r1
	b		loop
	MEND

call_near_ax
	call_near_reg16 eax
call_near_cx
	call_near_reg16 ecx
call_near_dx
	call_near_reg16 edx
call_near_bx
	call_near_reg16 ebx
call_near_sp
	call_near_reg16 esp
call_near_bp
	call_near_reg16 ebp
call_near_si
	call_near_reg16 esi
call_near_di
	call_near_reg16 edi

	GLOBAL	call_near_ax
	GLOBAL	call_near_cx
	GLOBAL	call_near_dx
	GLOBAL	call_near_bx
	GLOBAL	call_near_sp
	GLOBAL	call_near_bp
	GLOBAL	call_near_si
	GLOBAL	call_near_di

; --- CALL FAR [address] ---

	GLOBAL	call_far_r0_bp
call_far_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	call_far_r0
call_far_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 call_far_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
call_far_RAM
	mov		r0, #16
	str		r0, [sp, #SP_FREE3]		; Save the flag telling this is a USE16 call far
	;-------
	; First get the call target 
	;	r1 = new IP (offset)
	;	r2 = new CS (segment)
	;-------
	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #3]
	ldrb	r2,[r2, #2]
	orr		r1, r0, r1, lsl #8		; r1 = new logical IP
	orr		r2, r3, lsl #8			; r2 = new CS value
	b		cpu_call_far_r1r2		; Jump to common handling for both real and protected mode.

	modrm_1_help call_far, call_far_r0

; --- JMP NEAR [address] ---

	GLOBAL	jmp_near_r0_bp
jmp_near_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	jmp_near_r0
jmp_near_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 jmp_near_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
jmp_near_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrb	r2,[r2, #1]
	ldr		r1,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	orr		r0, r2, lsl #8			; Now r0 = new logical IP
	; ----- Then calculate the current logical IP
	add		r12, r1, r0				; New PC = physical base + (CS<<4) + new logical IP
	b		loop

	modrm_1_help jmp_near, jmp_near_r0
	
	ALIGN	4

	MACRO 
	jmp_near_reg16 $reg
	ldr		r1,[sp, #SP_PHYS_CS]	; Get current physical CS from stack
	ubfx	r0, $reg, #0, #16
	add		r12, r1, r0				; New PC = physical base + (CS<<4) + new logical IP
	b		loop
	MEND

jmp_near_ax
	jmp_near_reg16 eax
jmp_near_cx
	jmp_near_reg16 ecx
jmp_near_dx
	jmp_near_reg16 edx
jmp_near_bx
	jmp_near_reg16 ebx
jmp_near_sp
	jmp_near_reg16 esp
jmp_near_bp
	jmp_near_reg16 ebp
jmp_near_si
	jmp_near_reg16 esi
jmp_near_di
	jmp_near_reg16 edi

	GLOBAL	jmp_near_ax
	GLOBAL	jmp_near_cx
	GLOBAL	jmp_near_dx
	GLOBAL	jmp_near_bx
	GLOBAL	jmp_near_sp
	GLOBAL	jmp_near_bp
	GLOBAL	jmp_near_si
	GLOBAL	jmp_near_di

; --- JMP FAR [address] ---

	GLOBAL	jmp_far_r0_bp
jmp_far_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	jmp_far_r0
jmp_far_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 jmp_far_RAM, bad_EGA_opcode, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
jmp_far_RAM
	;-------
	; First get the jump target 
	;	r1 = new IP (offset)
	;	r2 = new CS (segment)
	;-------
	ldrb	r0,[r2]
	ldrb	r1,[r2, #1]
	ldrb	r3,[r2, #3]
	ldrb	r2,[r2, #2]
	orr		r1, r0, r1, lsl #8		; r1 = new logical IP
	orr		r2, r3, lsl #8			; r2 = new CS value
	b		cpu_jmp_far_r1r2		; Jump to common handling for both real and protected mode.

	modrm_1_help jmp_far, jmp_far_r0

	ALIGN	4

; --- PUSH ---

	EXTERN	push_EGA_r2
	GLOBAL	push_word_r0_bp
push_word_r0_bp
	;-------
	; Setup the correct segment when indexing by BP register.
	;-------
	mem_handler_bp
	GLOBAL	push_word_r0
push_word_r0
	;-------
	; Calculate and jump to correct memory access handler.
	;-------
	mem_handler_jump_r0r3 push_RAM, push_EGA_r2, bad_MODEX_opcode
	;-------
	; Memory location in normal RAM.
	; On input
	;	r2 = physical memory address
	;-------
push_RAM
	ldrb	r0,[r2]					; Get low byte
	ldrb	r1,[r2, #1]				; Load high byte to r1
	push_low_hi r0, r1, r2, r3
	b		loop

	modrm_1_help push_word, push_word_r0

	ALIGN	4


	GLOBAL	ccnt_enable
ccnt_enable
	mov		r0, #1						; Enable all counters
	mcr		p15, 0, r0, c15, c12, 0
	bx		lr

	GLOBAL	ccnt_read
ccnt_read
	mrc		p15, 0, r0, c15, c12, 1
	bx		lr

; =================== Opcode tables etc ===============================

	AREA	jumptables, DATA, READONLY
	ALIGN	4

	GLOBAL	opcodetable_16_16					; Also used from "cpu_prot.S" when returning to real mode
opcodetable_16_16
; 0
	DCD op_00, op_01, op_02, op_03, op_04, op_05, op_06, op_07
	DCD op_08, op_09, op_0a, op_0b, op_0c, op_0d, op_0e, op_0f
	DCD op_10, op_11, op_12, op_13, op_14, op_15, op_16, op_17
	DCD op_18, op_19, op_1a, op_1b, op_1c, op_1d, op_1e, op_1f
	DCD op_20, op_21, op_22, op_23, op_24, op_25, op_26, op_27
	DCD op_28, op_29, op_2a, op_2b, op_2c, op_2d, op_2e, op_2f
	DCD op_30, op_31, op_32, op_33, op_34, op_35, op_36, op_37
	DCD op_38, op_39, op_3a, op_3b, op_3c, op_3d, op_3e, op_3f
; 0x40
	DCD op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47
	DCD op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f
	DCD op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57
	DCD op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
	DCD op_60, op_61, unknown, unknown, op_64, op_65, op_66_USE16, op_67_USE16
	DCD op_68, op_69, op_6a, op_6b, op_6c_insb, unknown, op_6e_outsb, op_6f_outsw
	DCD op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
	DCD op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f
; 0x80
	DCD op_80, op_81, op_82, op_83, op_84, op_85, op_86, op_87
	DCD op_88, op_89, op_8a, op_8b, op_8c, op_8d, op_8e, op_8f
	DCD loop, xchg_ax_cx, xchg_ax_dx, xchg_ax_bx, xchg_ax_sp, xchg_ax_bp, xchg_ax_si, xchg_ax_di
	DCD op_98, op_99, op_9a, loop, op_9c, op_9d, op_9e, op_9f
	DCD op_a0, op_a1, op_a2, op_a3, op_a4_movsb, op_a5_movsw, op_a6_cmpsb, op_a7_cmpsw
	DCD op_a8, op_a9, op_aa_stosb, op_ab_stosw, op_ac_lodsb, op_ad_lodsw, op_ae_scasb, op_af_scasw
	DCD op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
	DCD op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
; 0xC0
	DCD op_c0, op_c1, op_c2, op_c3, op_c4, op_c5, op_c6, op_c7
	DCD op_c8, op_c9, op_ca, op_cb, op_cc, op_cd, op_ce, op_cf
	DCD op_d0, op_d1, op_d2, op_d3, op_d4, op_d5, op_d6, op_d7
	DCD op_d8, op_d9, op_da, op_db, op_dc, op_dd, op_de, op_df
	DCD op_e0, op_e1, op_e2, op_e3, op_e4_in_al_imm8, op_e5_in_ax_imm8, op_e6_out_imm8_al, op_e7_out_imm8_ax
	DCD op_e8, op_e9, op_ea, op_eb, op_ec_in_al_dx, op_ed_in_ax_dx, op_ee_out_dx_al, op_ef_out_dx_ax
	DCD loop, unknown, op_f2, op_f3, op_f4, op_f5, op_f6, op_f7
	DCD op_f8, op_f9, op_fa_CLI, op_fb_STI, op_fc, op_fd, op_fe, op_ff


	AREA	cpu_data, DATA, READWRITE
	ALIGN	4

	GLOBAL	EMSPageTable
EMSPageTable
	DCD	EMSPageStatic

	GLOBAL IRQFlagAddr
IRQFlagAddr
	DCD	0
FrameCounter
	DCD 	0
TestData
	DCD	0
	GLOBAL	BreakReason
BreakReason						; Reason for breaking into debugger (0 = NULL = unknown opcode)
	DCD	0
	GLOBAL	TrapFlag				; Also used from "pic.s"
TrapFlag
	DCD	0
	
	; ----- Jump table for opcode handler and IRQ handler

	GLOBAL op_00
	GLOBAL op_01 
	GLOBAL op_02 
	GLOBAL op_03 
	GLOBAL op_04 
	GLOBAL op_05 
	GLOBAL op_06 
	GLOBAL op_07
	GLOBAL op_08 
	GLOBAL op_09 
	GLOBAL op_0a 
	GLOBAL op_0b 
	GLOBAL op_0c 
	GLOBAL op_0d 
	GLOBAL op_0e 
	EXTERN	op_0f
	GLOBAL op_10 
	GLOBAL op_11 
	GLOBAL op_12 
	GLOBAL op_13 
	GLOBAL op_14 
	GLOBAL op_15 
	GLOBAL op_16 
	GLOBAL op_17
	GLOBAL op_18 
	GLOBAL op_19 
	GLOBAL op_1a 
	GLOBAL op_1b 
	GLOBAL op_1c 
	GLOBAL op_1d 
	GLOBAL op_1e 
	GLOBAL op_1f
	GLOBAL op_20 
	GLOBAL op_21 
	GLOBAL op_22 
	GLOBAL op_23 
	GLOBAL op_24 
	GLOBAL op_25 
	GLOBAL op_26 
	GLOBAL op_27
	GLOBAL op_28 
	GLOBAL op_29 
	GLOBAL op_2a 
	GLOBAL op_2b 
	GLOBAL op_2c 
	GLOBAL op_2d 
	GLOBAL op_2e 
	GLOBAL op_2f
	GLOBAL op_30 
	GLOBAL op_31 
	GLOBAL op_32 
	GLOBAL op_33 
	GLOBAL op_34 
	GLOBAL op_35 
	GLOBAL op_36 
	GLOBAL op_37
	GLOBAL op_38 
	GLOBAL op_39 
	GLOBAL op_3a 
	GLOBAL op_3b 
	GLOBAL op_3c 
	GLOBAL op_3d 
	GLOBAL op_3e 
	GLOBAL op_3f
; 0x40
	GLOBAL op_40 
	GLOBAL op_41 
	GLOBAL op_42 
	GLOBAL op_43 
	GLOBAL op_44 
	GLOBAL op_45 
	GLOBAL op_46 
	GLOBAL op_47
	GLOBAL op_48 
	GLOBAL op_49 
	GLOBAL op_4a 
	GLOBAL op_4b 
	GLOBAL op_4c 
	GLOBAL op_4d 
	GLOBAL op_4e 
	GLOBAL op_4f
	GLOBAL op_50 
	GLOBAL op_51 
	GLOBAL op_52 
	GLOBAL op_53 
	GLOBAL op_54 
	GLOBAL op_55 
	GLOBAL op_56 
	GLOBAL op_57
	GLOBAL op_58 
	GLOBAL op_59 
	GLOBAL op_5a 
	GLOBAL op_5b 
	GLOBAL op_5c 
	GLOBAL op_5d 
	GLOBAL op_5e 
	GLOBAL op_5f
	GLOBAL op_60 
	GLOBAL op_61 
	GLOBAL op_64 
	GLOBAL op_65
	EXTERN	op_66_USE16
	EXTERN	op_67_USE16
	GLOBAL op_68 
	GLOBAL op_69 
	GLOBAL op_6a 
	GLOBAL op_6b 
	EXTERN	op_6c_insb
	EXTERN	op_6e_outsb
	EXTERN	op_6f_outsw
	GLOBAL op_70 
	GLOBAL op_71 
	GLOBAL op_72 
	GLOBAL op_73 
	GLOBAL op_74 
	GLOBAL op_75 
	GLOBAL op_76 
	GLOBAL op_77
	GLOBAL op_78 
	GLOBAL op_79 
	GLOBAL op_7a 
	GLOBAL op_7b 
	GLOBAL op_7c 
	GLOBAL op_7d 
	GLOBAL op_7e 
	GLOBAL op_7f
; 0x80
	GLOBAL op_80 
	GLOBAL op_81 
	GLOBAL op_82 
	GLOBAL op_83 
	GLOBAL op_84 
	GLOBAL op_85 
	GLOBAL op_86 
	GLOBAL op_87
	GLOBAL op_88 
	GLOBAL op_89 
	GLOBAL op_8a 
	GLOBAL op_8b 
	GLOBAL op_8c
	GLOBAL op_8d 
	GLOBAL op_8f
	GLOBAL loop 
	GLOBAL xchg_ax_cx 
	GLOBAL xchg_ax_dx 
	GLOBAL xchg_ax_bx 
	GLOBAL xchg_ax_sp 
	GLOBAL xchg_ax_bp 
	GLOBAL xchg_ax_si 
	GLOBAL xchg_ax_di
	GLOBAL op_98 
	GLOBAL op_99 
	GLOBAL op_9a 
	GLOBAL op_9c 
	GLOBAL op_9d 
	GLOBAL op_9e 
	GLOBAL op_9f
	GLOBAL op_a0 
	GLOBAL op_a1 
	GLOBAL op_a2 
	GLOBAL op_a3 
	EXTERN	op_a4_movsb
	EXTERN	op_a5_movsw
	EXTERN	op_a6_cmpsb
	EXTERN	op_a7_cmpsw
	GLOBAL op_a8 
	GLOBAL op_a9
	EXTERN	op_aa_stosb
	EXTERN	op_ab_stosw
	EXTERN	op_ac_lodsb
	EXTERN	op_ad_lodsw
	EXTERN	op_ae_scasb
	EXTERN	op_af_scasw
	GLOBAL op_b0 
	GLOBAL op_b1 
	GLOBAL op_b2 
	GLOBAL op_b3 
	GLOBAL op_b4 
	GLOBAL op_b5 
	GLOBAL op_b6 
	GLOBAL op_b7
	GLOBAL op_b8
	GLOBAL op_b9 
	GLOBAL op_ba 
	GLOBAL op_bb 
	GLOBAL op_bc 
	GLOBAL op_bd 
	GLOBAL op_be 
	GLOBAL op_bf
; 0xC0
	GLOBAL op_c0 
	GLOBAL op_c1 
	GLOBAL op_c2 
	GLOBAL op_c3 
	GLOBAL op_c4 
	GLOBAL op_c5 
	GLOBAL op_c6 
	GLOBAL op_c7
	GLOBAL op_c8 
	GLOBAL op_c9 
	GLOBAL op_ca 
	GLOBAL op_cb 
	GLOBAL unknown 
	GLOBAL op_cd 
	GLOBAL op_ce 
	GLOBAL op_cf
	GLOBAL op_d0 
	GLOBAL op_d1 
	GLOBAL op_d2 
	GLOBAL op_d3 
	GLOBAL op_d4 
	GLOBAL op_d5 
	GLOBAL op_d6 
	GLOBAL op_d7
	GLOBAL op_d8 
	GLOBAL op_d9 
	GLOBAL op_da 
	GLOBAL op_db 
	GLOBAL op_dc 
	GLOBAL op_dd 
	GLOBAL op_de 
	GLOBAL op_df
	GLOBAL op_e0 
	GLOBAL op_e1 
	GLOBAL op_e2 
	GLOBAL op_e3 
	EXTERN	op_e4_in_al_imm8
	EXTERN	op_e5_in_ax_imm8
	EXTERN	op_e6_out_imm8_al
	EXTERN	op_e7_out_imm8_ax
	GLOBAL op_e8
	GLOBAL op_e9 
	GLOBAL op_ea 
	GLOBAL op_eb
	EXTERN	op_ec_in_al_dx
	EXTERN	op_ed_in_ax_dx
	EXTERN	op_ee_out_dx_al
	EXTERN	op_ef_out_dx_ax
	EXTERN	op_f2
	EXTERN	op_f3
	GLOBAL op_f4 
	GLOBAL op_f6 
	GLOBAL op_f7
	GLOBAL op_f8 
	GLOBAL op_f9 
	EXTERN	op_fa_CLI
	EXTERN	op_fb_STI
	GLOBAL op_fc 
	GLOBAL op_fd 
	GLOBAL op_fe 
	GLOBAL op_ff


	GLOBAL EMSPageStatic
EMSPageStatic
	SPACE	4*4*(16+1)

	;-------
	; Potential reasons for breaking into the debugger
	;-------
	GLOBAL BRUnsOP
BRUnsOP
	DCB	"Unsupported opcode!"
	DCB	0x0a, 0

	GLOBAL BRUnsJPO
BRUnsJPO
	DCB	"Unhandled JPO opcode!"
	DCB	0x0a, 0

	GLOBAL BRUnsJPE
BRUnsJPE
	DCB	"Unhandled JPE opcode!"
	DCB	0x0a, 0

	GLOBAL BRUnsEGA
BRUnsEGA
	DCB	"Unsupported EGA opcode!"
	DCB	0x0a, 0

	GLOBAL BRUnsMODEX
BRUnsMODEX
	DCB	"Unsupported Mode-X opcode!"
	DCB	0x0a, 0

BRUnsINT
	DCB	"Unsupported INT "
	GLOBAL	BRUnsIntCode
BRUnsIntCode
	DCB	"XX call!"
	DCB	0x0a, 0

BRMemWatch
	DCB	"Memory watch trap"
	DCB	0x0a, 0

	GLOBAL	BRUns386Op
BRUns386Op
	DCB	"386 opcodes not supported!"
	DCB	0x0a, 0

	GLOBAL	BRUnsXMS
BRUnsXMS
	DCB	"Unsupported XMS call!"
	DCB	0x0a, 0

	ALIGN	4
	GLOBAL	eiptrace
eiptrace
	SPACE	4*32

	GLOBAL profcounts
profcounts
	SPACE	4*256
	
	END
