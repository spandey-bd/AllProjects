;=============================================================================
; pic.s
;
; This file contains interrupt handling -related stuff:
;	- IRQStart = Switching the emulated CPU to handling a hardware interrupt.
;	- ???IRQ = Functions called by external C handlers after a host event
;	  (real-life hardware interrupt or keypress) has occurred.
;	- CLI / STI opcode handlers.
;	- I/O port 0x20 and 0x21 (PIC chip) handlers.
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

	AREA pic, ALIGN=4, CODE, READONLY

	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

	GLOBAL	int_flag_on
	
	GLOBAL	op_fa_CLI
	GLOBAL	op_fb_STI
	
	GLOBAL	in_al_20
	GLOBAL	in_al_21
	GLOBAL	out_20_al
	GLOBAL	out_21_al

	EXTERN	registers
	
	EXTERN	BreakReason
	EXTERN	cpu_cr0
	EXTERN	TrapFlag
	EXTERN	IRQFlagAddr
	EXTERN	sb_irq_ack
	EXTERN	debug_trap_false
	EXTERN	op_fa_prot_r0
	EXTERN	op_fb_prot_r0
	EXTERN	bad_out_port
	EXTERN	irq_prot
	EXTERN	lock_irqflag
	EXTERN	unlock_irqflag

	IMPORT	|__imp_QueryPerformanceCounter|
	EXTERN	|__imp_AcquireSRWLockExclusive|
	EXTERN	|__imp_ReleaseSRWLockExclusive|
	EXTERN	|irqflag_mutex|

IRQ_DECR	EQU		0x80000				; IRQ testing frequency, after every 0x100000000 / IRQ_DECR emulated CPU cycles.
EOITRACE	EQU		0
IRQTRACE	EQU		0

	IF IRQTRACE = 1
	EXTERN	irqtrace
	ENDIF
	
	MACRO
	select_IRQ_to_handle_r0r1
		push	{r2}					; Save r2 (flags) to stack
		ldr		r1,=IRQPending
		mov		r2, #0x00010000			; Mark this IRQ as being serviced
		; ----- Test for IRQ0 -----
		ldr		r0, [r1]				; Get the IRQPending[0] value
		cmp		r0, #((8+0)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		; ----- Test for IRQ1 -----
		add		r1, #4
		ldr		r0, [r1]				; Get the IRQPending[1] value
		cmp		r0, #((8+1)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		; ----- Test for IRQ12 -----
		add		r1, #4
		ldr		r0, [r1]				; Get the IRQPending[2] value
		cmp		r0, #((8+2)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		; ----- Test for IRQ3 (COM2) -----
		add		r1, #4
		ldr		r0, [r1]				; Get the IRQPending[3] value
		cmp		r0, #((8+3)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		; ----- Test for IRQ4 (COM1) -----
		add		r1, #4
		ldr		r0, [r1]				; Get the IRQPending[4] value
		cmp		r0, #((8+4)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		; ----- Test for IRQ7 -----
		add		r1, #3*4
		ldr		r0, [r1]				; Get the IRQPending[7] value
		cmp		r0, #((8+7)*4)			; Was it pending but not yet being serviced?
		beq		handle_vector_r0_flags_pushed	; Yep, go handle it
		pop		{r2}					; Restore flags to r2
	MEND
	MACRO
	test_for_pending_IRQ_r1r2
		ldr		r2, =IRQPending
		ldr		r1, [r2]				; IRQ0 (TimerIRQ)
		cmp		r1, #((8+0)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #4]			; IRQ1 (KeyboardIRQ)
		cmpne	r1, #((8+1)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #(4*2)]		; IRQ12
		cmpne	r1, #((8+2)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #(4*3)]		; IRQ3
		cmpne	r1, #((8+3)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #(4*4)]		; IRQ4
		cmpne	r1, #((8+4)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #(4*7)]		; IRQ7 (SBIRQ)
		cmpne	r1, #((8+7)*4)			; Is it pending but not yet being serviced?
		ldrne	r1, [r2, #(4*6)]		; IRQ6 (IRQ_EXIT)
		cmpne	r1, #((8+6)*4)			; Is it pending but not yet being serviced?
		;-------
		; Zero flag set if we have a pending IRQ.
		;-------
	MEND

DISALLOW_ZERO_CODE	EQU		0
DEBUGTRAP			EQU		0
PROFILER			EQU		0
DEBUGMEMWATCH		EQU		0
DEBUGREGWATCH		EQU		0
CSIPTRACE			EQU		0
SAVE_HACK			EQU		0
S2TEST				EQU		0
FULLTRACE			EQU		0

; ------------------- Main CPU opcode loop -----------------------------------
; Main opcode loop functions moved here from cpu.asm, because cpu.asm
; is so slow to compile, and these routines may change frequently.
;
	ALIGN	8

	GLOBAL	op_f5
op_f5											; Opcode F5 = CMC (Complement Carry)
	GLOBAL complement_carry
complement_carry
	mrs		r0,cpsr
	eor		r0, r0, #ARM_CARRY
	GLOBAL restore_flags_from_r0
restore_flags_from_r0							; Many functions jump here to restore flags and continue loop
	msr		cpsr_f,r0
	;------ Start the opcode decoder loop
	GLOBAL loop
loop
	IF FULLTRACE = 1
	add		r0, sp, #SP_ES_VALUE
	ldr		r1,=registers
	stmia	r1,{eax-r12}							; Save emulation registers to global memory
	ldr		eax, [sp, #SP_PHYS_CS]
	mov		esp, r1
	str		eax, [r1, #4*18]						; registers[18] = PHYS_CS
	ldr		r1, =reg_es
	ldmia	r0, {r2-ebx}							; Get current ES,CS,SS,DS,FS,GS
	stmia	r1, {r2-ebx}							; Save current ES,CS,SS,DS,FS,GS
	mrs		eax,cpsr								; Save flags
	bl		RegDebug							; Call the external debug trap function
	msr		cpsr_f, eax							; Restore flags
	ldmia	esp,{eax-r12}							; Restore emulation registers from global memory
	ENDIF
	IF CSIPTRACE = 1
	ldr		r0, =eiptrace
	ldr		r1, [sp, #SP_FREE4]					; Index
	ldr		r2, [sp, #SP_EX_CSIP]
	add		r1, #4
	and		r1, #4*32-1
	str		r1, [sp, #SP_FREE4]					; Index
	str		r2, [r0, r1]						; Save ex csip to table
	ENDIF
	IF DEBUGREGWATCH = 1
	ldr		r2, =IRQPending
	ldr		r1, =0x10000 + ((8+0)*4)
	ldr		r2, [r2]
	mrs		r0,cpsr								; Save flags to r0
	cmp		r2, r1
	beq		debug_trap_false
	msr		cpsr_f,r0							; Restore flags from r0
	ENDIF
	IF DEBUGMEMWATCH = 1
	;-------
	; These operations determine the memory location to watch
	;-------
	IF 1 = 0
	ldr		r1, [sp, #SP_CS_VALUE]
	ldr		r2, [sp, #SP_PHYS_CS]
	sub		r2, r12, r2
	mrs		r0,cpsr								; Save flags to r0
	cmp		r1, #0x80
	bne		debug_trap_continue					; No break, continue running
	ldr		r1, =0x5142
	cmp		r2, r1
	ELSE	
	ldr		r2, =0x07
	calc_linear_address_r2
	ldrb	r1,[r2]
	mrs		r0,cpsr								; Save flags to r0
	cmp		r1, #0
	ENDIF	
	;-------
	; Continue or break depending on the comparison
	;-------
	bne		debug_trap_continue					; No break, continue running
	ldr		r2,=TestData
	str		r1,[r2]
	ldr		r1, =BRMemWatch
	ldr		r2, =BreakReason
	str		r1, [r2]							; Tell the reason for breaking was a user request
	msr		cpsr_f,r0
	b		unknown
debug_trap_continue
	msr		cpsr_f,r0							; Restore flags from r0
	ENDIF	
	IF DEBUGTRAP = 1
	mrs		r1,cpsr								; Put flags to r1
	push	{r1-r12, lr}						; Push used registers
	ldr		r1,=registers
	stmia	r1!,{eax-r12}						; Save emulation registers to global memory
	mrs		r2, cpsr
	str		r2, [r1]							; Save flags to global memory
	bl		DebugTrap							; Call the external debug trap function
	pop		{r1-r12, lr}						; Pop used registers
	cmp		r0, #0								; Are we allowed to continue running?
	beq		debug_trap_false					; Nope, return to UI
	b		debug_trap_continue					; Yep, continue
debug_trap_continue	
	msr		cpsr_f,r1							; Get flags from r1
	ENDIF
	IF PROFILER = 1
	handle_profiling 0x1000000
	ENDIF	
	;-------
	; Use internal IRQ handling.
	;-------
	ldr		r1, [sp, #SP_IRQFLAG]				; Load IRQ_ON / IRQ_OFF mask to r1									cycle 1, dual-issue pipeline 0
	add		lr, sp, #SP_IRQFLAG					; lr = SP_IRQFLAG address											cycle 1, dual-issue pipeline 1
	ldrb	r0, [r12], #1						; Load opcode byte to r0, increment r12 by 1						cycle 2, dual-issue pipeline 0
	sub		r1, #IRQ_DECR						; Decrease the IRQ counter											cycle 2, dual-issue pipeline 1
	strd	r1, r12, [lr]						; Save SP_IRQFLAG from r1 and SP_EX_CSIP from r12					cycle 3, dual-issue pipeline 0
	cbz		r1, test_irq_time					; Jump to test for an IRQ if SP_IRQFLAG == 0						cycle 3, dual-issue pipeline 1
	ldrd	lr, r2, [lr, #(SP_SS_BASE-SP_IRQFLAG)] ; Get lr = BP-relative default segment, r2 = logical DS segment	cycle 4, dual-issue pipeline 0
	ldr		pc, [sp, r0, lsl #2]				; Jump to the opcode handler										cycles 5&6, dual-issue pipeline 0
	
	EXTERN	irqtime
	EXTERN	irqnext
	EXTERN	irqperiod
	EXTERN	irqcnt							; TESTONLY
	EXTERN	skipcnt							; TESTONLY
	EXTERN	sbirqcnt						; TESTONLY

	ALIGN	8
	
no_irq_to_handle
	;-------
	;	#ifdef TESTONLY
	;		skipcnt++;
	;	#endif
	;-------
	ldr		r12, =skipcnt
	ldr		r0, [r12]
	add		r0, #1
	str		r0, [r12]
	;-------
	;-------
	pop		{r0}							; r0 = CPU flags
	ldr		r12, [sp, #SP_EX_CSIP]			; Restore r12 value
	msr		cpsr_f, r0						; Restore the CPU flags from r0
	sub		r12, #1							; Rewind the opcode pointer
	b		loop							; Go to re-run the current opcode.

	ALIGN	8

; ------------------- Test IRQ Time -----------------------------------
; This gets called periodically from the CPU emulation loop.
; What we need to do here:
; - Determine whether the current performance counter value is over the
;	irqnext value. Request IRQ 0 if it is, and calculate new irqnext value.
; - If any IRQ is pending and interrupts are enabled, jump to IRQStart.
; - Else go back to loop
;
; Running a 140Hz IRQ timer on Nokia Lumia 520:
; 	- IRQ_DECR = 0x40000: irqcnt = 23399, skipcnt = 84160 (OK for up to 500Hz timers on Nokia Lumia 520, faster on faster phones)
; 	- IRQ_DECR = 0x80000: irqcnt = 22990, skipcnt = 197836 (OK for up to 1000Hz timers on Nokia Lumia 520)
;
; Doom timedemo on Nokia Lumia 520: 5856 realtics (proper 140Hz timer)
; Doom timedemo on Nokia Lumia 520: 5787 realtics (proper 140Hz timer)
; Doom timedemo on Nokia Lumia 520: 5886 realtics (with "add lr, sp, #SP_SS_BASE")
; Doom timedemo on Nokia Lumia 520: 5655 realtics (with "ldrd lr, r2, [lr]")
; Doom timedemo on Nokia Lumia 520: 5639 realtics (with "strd r1, r12, [lr]", debugging)
; Doom timedemo on Nokia Lumia 520: 5635 realtics (with "strd r1, r12, [lr]", without debugging)
; Doom timedemo on Nokia Lumia 520: 5930 realtics (with "and r1, r0, r1, asr #31")
; Doom timedemo on Nokia Lumia 520: 5924 realtics (with proper audio support)
;
; Doom timedemo on Nokia Lumia 820: 3829 realtics (proper 140Hz timer)
;
	GLOBAL	test_irq_time
test_irq_time
	mrs		r0,cpsr							; Save the CPU flags into r0
	push	{r0}							; Push flags
	;-------
	; QueryPerformanceCounter(&tmp);
	;-------
	ldr		r3, =|__imp_QueryPerformanceCounter|
	sub		sp, #8
	ldr		r3,[r3]
	mov		r0, sp
	blx     r3
	;-------
	; Now [sp+4]:[sp] is the new PerformanceCounter value.
	; Test it against "irqnext" value.
	;-------
	ldr		lr, =irqnext
	ldr		r3, [sp, #4]					; r3 = current performance counter HighPart
	ldr		r1, [lr, #4]					; r1 = irqnext.HighPart
	ldr		r0, [lr]						; r0 = irqnext.LowPart
	;-------
	; Test the HighParts
	;-------
	cmp		r3, r1
	blt		skip_irq_0
	ldr		r2, [sp]						; r2 = current performance counter LowPart
	bgt		request_irq_0
	;-------
	; Test the LowParts
	;-------
	cmp		r2, r0
	bcc     skip_irq_0
request_irq_0
	;-------
	; Prepare irqnext for the next IRQ time.
	; irqnext.QuadPart += irqperiod
	;-------
	ldr		r12, =irqperiod
	ldr		r12, [r12]						; r12 = irqperiod value
1	adds	r0, r12
	adcs	r1, #0
	cmp		r3, r1							; Is the new time in the future (high part)?
	bgt		%b1								; Nope, so add one more irqperiod!
	blt		%f2
	cmp		r2, r0							; High parts are equal, low part is in the past, so redo the add!
	bcs		%b1
2	str		r0, [lr]
	str		r1, [lr, #4]
	;-------
	; Save new (irqnext - irqperiod) as the irqtime value.
	;-------
	subs	r0, r12
	ldr		r12, =irqtime
	sbcs	r1, #0
	str		r0, [r12]
	str		r1, [r12, #4]
	;-------
	;	#ifdef TESTONLY
	;		if (0 == num)
	;			irqcnt++;
	;	#endif
	;-------
	ldr		r12, =irqcnt
	ldr		r0, [r12]
	add		r0, #1
	str		r0, [r12]
	;-------
	; IRQRequest(IRQ0);
	;-------
	ldr		r12, =IRQPending
	ldrb	r0, [r12, #2]
	tst		r0, #1							; Is TimerIRQ already in progress?
	bne		skip_irq_0						; Yep, so skip launching a new timer IRQ
	mov		r0, #8*4
	strb	r0, [r12]						; IRQPending[num] |= ((num+8)*4);
	IF IRQTRACE = 1
	;-------
	; Trace the IRQ0 request time
	;-------
	orr		r1, r0, #0x80000000				; r1 = event = IRQRequest(IRQ_0)
	ldr		r0, [sp, #SP_EX_CSIP+(4*3)]		; r0 = SP_EX_CSIP value
	ldr		r2, [sp]						; r2 = current QueryPerformanceCounter value
	bl		irqtrace
	ENDIF
skip_irq_0
	;-------
	; Are interrupts OK now?
	;-------
	ldr		r2, [sp, #SP_FLAGS+(4*3)]		; Get the FLAGS value
	add		sp, #8
	tst		r2, #FLAG_IF					; Test the "Interrupts Enabled" bit
	beq		no_irq_to_handle
	;-------
	; Test for a pending interrupt in the IRQRequest table. This is safe to do
	; without irq_lock, because the other threads only set the values we are
	; testing for, and we don't change any IRQRequest table values here.
	;-------
	test_for_pending_IRQ_r1r2				; Zero flag set if we have a pending IRQ
	bne		no_irq_to_handle				; Jump if no interrupt pending
	;=======
	; IRQStart
	; We have a pending interrupt, and it is OK to start handling it!
	;=======
	ldr		r12, [sp, #SP_EX_CSIP+4]		; Restore r12 value
	pop		{r2}							; r2 = CPU flags
	ldr		r1,[sp, #SP_FLAGS]				; Get the FLAGS value
	sub		r12, #1							; Rewind the opcode pointer
	tst		r1, #FLAG_TF					; Is the Trap Flag on?
	bne		handle_trap						; Jump to handle single stepping if it is
	;-------
	; Determine the IRQ vector we need to jump to
	;-------
	select_IRQ_to_handle_r0r1				; Jump to handle_vector_r0_flags_pushed if we found a pending IRQ
	;-------
	; No IRQ vector to jump to, so this is a debugger break request. Go to the debugger.
	;-------
	ldr		r0, =BRUserBreak
	ldr		r1, =BreakReason
	str		r0, [r1]						; Tell the reason for breaking was a user request
	;-------
	; Restore the flags
	;-------
	msr		cpsr_f, r2
	b		debug_trap_false				; Go to the debugger
	;-------
	; Start handling the interrupt.
	; Input: r0 = IRQ vector address where we need to jump to.
	;-------
handle_vector_r0_flags_pushed
	str		r2, [r1]						; Set IRQPending in-progress bit, clear pending byte
	;-------
	; Mark the active interrupt number.
	;-------
	ldr		r1, =IRQPending
	str		r0, [r1, #4*8]					; Mark that this is the currently active IRQ
	;-------
	; Test for auto-eoi, and clear the in-service bit if we have auto-eoi on.
	;-------
	ldrb	r2, [r1, #4*9+3]
	cmp		r2, #0
	subne	r1, #(8*4)-2
	movne	r2, #0
	strbne	r2, [r1, r0]					; Clear the in-service bit
	;-------
	; Adjust the IRQ0..IRQ7 start address (for Windows 3.0),
	; and move the IRQ12 vector to INT 74.
	;-------
	cmp		r0, #(8+2)*4					; If it is IRQ2, ...
	ldr		r2,=IRQ0Offset
	ldrb	r2, [r2]
	sub		r0, #8*4
	ldr		r1, =cpu_cr0
	add		r0, r2, lsl #2
	ldrb	r1, [r1]
	moveq	r0, #0x74*4						; ... make it IRQ12
	;-------
	; Test for protected mode
	;-------
	pop		{r2}							; Restore flags
	tst		r1, #1
	bne		irq_prot
	;-------
	; Push the x86 flags. After that we can freely change the ARM flags.
	;-------
	msr		cpsr_f, r2						; Set the real CPU flags from r2
	push_flags_16 r2, r1, r3				; Push the x86 flags
	mov		r2, #0
	mov		r1, r0							; Save the IRQ number*4 to r1
	calc_linear_address_r2					; Get the physical address of the beginning of RAM, destroys r0!
	ldr		r2,[r2, r1]						; Get the IRQ vector address, high halfword = segment, low halfword = offset
	; ----- Then store current CS:IP to stack
	ldr		r0, [sp, #SP_CS_VALUE]			; r0 = Current logical CS
	push_hword r0, r1, r3
	ldr		r1, [sp, #SP_PHYS_CS]			; r1 = Current physical CS
	sub		r1, r12, r1						; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1, r0, r3
	; ----- Then get new logical IP (zero-extended) to r12
	mov		r12, r2, lsl #16				; r12 high 16 bits contain the new logical IP
	; ----- Then get new logical CS (zero-extended) to r0
	mov		r0, r2, lsr #16					; Now r0 = new logical CS
	; ----- Then save new CS into REG_CS
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]			; Save new CS value to stack
	str		r2, [sp, #SP_CS_BASE]
	; ----- And finally calculate new physical IP
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]			; Store new physical CS into stack
	add		r12, r2, r12, lsr #16			; r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	; ----- Clear the Trap and Interrupts flags
	ldr		r0,[sp, #SP_FLAGS]				; Get the EXTRAFLAGS value
	bic		r0, #(FLAG_TF|FLAG_IF)
	str		r0,[sp, #SP_FLAGS]				; Save the new extra flags
	; ----- Clear the IRQFlag/semaphore, OK for further interrupts
	ldr		r0, [sp, #SP_IRQMAXC]
	str		r0, [sp, #SP_IRQFLAG]
	b		loop
	;-------
	; Handle trap (single step) interrupt.
	; INT01 = CPU-generated - SINGLE STEP
	; 	Desc: Generated after each instruction if TF (trap flag) is set
	; 	TF is cleared on invoking the single-step interrupt handler 
	;	Notes: Interrupts are prioritized such that external interrupts are invoked
	;	after the INT 01 pushes CS:IP/FLAGS and clears TF, but before the first instruction of the handler executes.
	;	Used by debuggers for single-instruction execution tracing, such as MS-DOS DEBUGs T command.
	;-------
	GLOBAL	handle_trap
handle_trap
	ldr		r1, =TrapFlag
	ldr		r0, [r1]				; Get the current Trap Flag value
	eors	r0, #1					; Toggle the flag
	str		r0, [r1]				; and save it
	bne		handle_trap_opcode
	;-------
	; We are in a phase where we need to push flags and CS:IP,
	; clear the TF flag, check for a pending real IRQ, and finally jump to INT01 handler.
	;-------
	msr		cpsr_f, r2				; Set the real CPU flags from r2
	push_flags_16 r2, r0, r3		; Push the x86 flags
	; ----- Clear the Trap and Interrupts flags
	ldr		r0,[sp, #SP_FLAGS]	; Get the EXTRAFLAGS value
	bic		r0, #(FLAG_TF|FLAG_IF)	; Clear the Trap and Interrupt flags
	str		r0,[sp, #SP_FLAGS]	; Save the new extra flags
	; ----- Then store current CS:IP to stack
	ldr		r0, [sp, #SP_CS_VALUE]	; r0 = Current logical CS
	ldr		r1, [sp, #SP_PHYS_CS]	; r1 = Current physical CS
	push_hword r0, r2, r3
	sub		r1, r12, r1				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1, r2, r3
	; ----- Then get new logical IP (zero-extended) to r12
	mov		r2, #0
	calc_linear_address_r2			; Get the physical address of the beginning of RAM
	ldr		r2,[r2, #(4*1)]			; Get the INT1 vector address, high halfword = segment, low halfword = offset
	mov		r12, r2, lsl #16		; r12 high 16 bits contain the new logical IP
	; ----- Then get new logical CS (zero-extended) to r0
	mov		r0, r2, lsr #16			; Now r0 = new logical CS
	; ----- Then save new CS into REG_CS
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]	; Save new CS value to stack
	str		r2, [sp, #SP_CS_BASE]
	; ----- And finally calculate new physical IP
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]	; Store new physical CS into stack
	add		r12, r2, r12, lsr #16	; r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	; ----- Then check for a pending real IRQ
	mrs		r2, cpsr				; Save flags to r2
	select_IRQ_to_handle_r0r1		; Jump to handle_vector_r0_flags_pushed if we found a pending IRQ
	msr		cpsr_f, r2				; Restore flags
	; ----- Clear the IRQFlag/semaphore, OK for further interrupts
	ldr		r0, [sp, #SP_IRQMAXC]
	str		r0, [sp, #SP_IRQFLAG]
	b		loop					; Go start the INT1 handler
	;-------
	; We are in a phase where we need to first execute the opcode, and only then jump to INT1
	;-------
handle_trap_opcode
	msr		cpsr_f, r2				; Restore flags
	ldrb	r0,[r12],#1				; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]	; r2 high halfword = logical DS segment, clear segment override flags
	ldr		pc,[sp, r0, lsl #2]		; Jump to the opcode handler
	
; ------------------- SoundBlasterIRQ code ----------------------------
; Called by fifoInternalRecvInterrupt(), mark that a SB interrupt is pending
; and make the next opcode fetch jump into the IRQStart instead.
; Also called from "ports_SB.s" when an interrupt is requested via SB DSP command!
; 
	GLOBAL SBIRQ
SBIRQ
	IF 1 = 1
	ldr		r2, =sb_irq_ack
	mov		r1, #0
	strb	r1, [r2]
	ENDIF
	;-------
	;	#ifdef TESTONLY
	;		sbirqcnt++;
	;	#endif
	;-------
	ldr		r2, =sbirqcnt
	ldr		r0, [r2]
	add		r0, #1
	str		r0, [r2]
	;-------
	; Test whether we handle interrupts at all
	;-------
	ldr		r2,=IRQFlagAddr			; Get address of the address
	ldr		r3,[r2]					; Get the actual address into r3
	cmp		r3, #0					; Is the address = 0 ?
	bxeq	lr						; Not running, ignore SoundBlaster interrupts
	;-------
	; Mark that IRQ7 is pending
	;-------
	ldr		r0,=IRQPending
	mov		r1, #((8+7)*4)			; Use the INT vector address as the pending flag
	add		r0, #(4*7)
	;-------
	; Registers here:
	;	r0 = IRQPending[IRQNumber] address
	;	r1 = INT vector address (0x08*4..0x0F*4)
	;	r2 = free
	;	r3 = SP_IRQFLAG address (pointer to a variable inside our stack!)
	;-------
	GLOBAL	common_irq
common_irq							; Now r2 = #SP_FLAGS value, r3 = SP_IRQFLAG address
	IF IRQTRACE = 1
	;-------
	; Trace the IRQ0 request time
	;-------
	push	{r0, r1, r3, r12, lr}
	orr		r1, #0x80000000						; r1 = event = IRQRequest(IRQ_?)
	ldr		r0, [r3, #(SP_EX_CSIP-SP_IRQFLAG)]	; r0 = SP_EX_CSIP value
	mov		r2, #0								; r2 = current QueryPerformanceCounter value needs to be checked
	bl		irqtrace
	pop		{r0, r1, r3, r12, lr}
	ENDIF
	;-------
	; Make sure we are not already servicing this IRQ
	;-------
	ldrb	r2, [r0, #2]			; r2 = previous IRQ pending value
	tst		r2, #1					; IRQ already being serviced?
	strb	r1, [r0]				; mark it pending.
	bxne	lr						; This IRQ was already being serviced, just return!
	IF 0 = 1	
	;-------
	; Check if this IRQ has been masked, and return if it has.
	;-------
	ldr		r2, =Port21Data
	ldrb	r2, [r2]
	sub		r1, #8*4
	lsr		r1, #2
	mov		r0, #1
	lsl		r0, r0, r1
	tst		r2, r0
	bne		unlock_exit
	ENDIF	
	;-------
	; Set the IRQFlag semaphore, unless interrupts are disabled.
	;-------
1	ldr		r2,[r3, #(SP_FLAGS-SP_IRQFLAG)]	; Get the #SP_FLAGS into r2
	mov		r1, #IRQ_ON
	;-------
	; Check whether interrupt are currently enabled...
	;-------
	tst		r2, #FLAG_IF 			; Are interrupts enabled?
	strne	r1,[r3]					; Yep, set the IRQ flag, start handling an IRQ.
	bx		lr
	
; ------------------- KeyboardIRQ code --------------------------------
; Launch an IRQ1 to handle the keyboard scan code
	GLOBAL KeyboardIRQ
KeyboardIRQ
	;-------
	; Test whether we handle interrupts at all
	;-------
	ldr		r2,=IRQFlagAddr			; Get address of the address
	ldr		r3,[r2]					; Get the actual address into r3
	cmp		r3, #0					; Is the address = 0 ?
	bxeq	lr						; Not running, ignore keyboard interrupts
	;-------
	; Mark that IRQ1 is pending
	;-------
	ldr		r0,=IRQPending
	mov		r1, #((8+1)*4)			; Use the INT vector address as the pending flag
	add		r0, #(4*1)
	b		common_irq				; The rest of the code is common to all interrupt types

; ------------------- MouseIRQ code -----------------------------------
; Launch an IRQ3 to handle the mouse/COM2 user subroutine call
	GLOBAL MouseIRQ
MouseIRQ
	;-------
	; Test whether we handle interrupts at all
	;-------
	ldr		r2,=IRQFlagAddr			; Get address of the address
	ldr		r3,[r2]					; Get the actual address into r3
	cmp		r3, #0					; Is the address = 0 ?
	bxeq	lr						; Not running, ignore keyboard interrupts
	;-------
	; Mark that IRQ3 is pending
	;-------
	ldr		r0,=IRQPending
	mov		r1, #((8+3)*4)			; Use the INT vector address as the pending flag
	add		r0, #(4*3)
	b		common_irq				; The rest of the code is common to all interrupt types

; ------------------- PS/2 Mouse IRQ code -----------------------------
; Launch an IRQ2 to handle the mouse user subroutine call
	GLOBAL PS2MouseIRQ
PS2MouseIRQ
	;-------
	; Test whether we handle interrupts at all
	;-------
	ldr		r2,=IRQFlagAddr			; Get address of the address
	ldr		r3,[r2]					; Get the actual address into r3
	cmp		r3, #0					; Is the address = 0 ?
	bxeq	lr						; Not running, ignore keyboard interrupts
	;-------
	; Mark that IRQ2 == IRQ12 is pending
	;-------
	ldr		r0,=IRQPending
	mov		r1, #((8+2)*4)			; Use the INT vector address as the pending flag
	add		r0, #(4*2)
	b		common_irq				; The rest of the code is common to all interrupt types

; ------------------- COMIRQ code -----------------------------------
; Launch an IRQ4 to handle the COM port user subroutine call
	GLOBAL COM1IRQ
COM1IRQ
	;-------
	; Test whether we handle interrupts at all
	;-------
	ldr		r2,=IRQFlagAddr			; Get address of the address
	ldr		r3,[r2]					; Get the actual address into r3
	cmp		r3, #0					; Is the address = 0 ?
	bxeq	lr						; Not running, ignore keyboard interrupts
	;-------
	; Mark that IRQ4 is pending
	;-------
	ldr		r0,=IRQPending
	mov		r1, #((8+4)*4)			; Use the INT vector address as the pending flag
	add		r0, #(4*4)
	b		common_irq				; The rest of the code is common to all interrupt types
	
	LTORG							; Dump the current literal pool here

; ------------------- 9D = POPF / CF = IRET ---------------------------
; IRQ helper subroutines for POPF and IRET when the INT flag changes.
;
	;-------
	; Interrupts got enabled, check for pending interrupts
	;-------
int_flag_on
	test_for_pending_IRQ_r1r2		; Zero flag set if we have a pending IRQ
	;-------
	; Interrupts enabled and an interrupt is pending, start handling it
	;-------
	moveq	r1, #IRQ_ON
	streq	r1,[sp, #SP_IRQFLAG]	; Set the IRQFlag
	b		restore_flags_from_r0	; Go to restore the cpu flags

; ------------------- FA = CLI ----------------------------------------
op_fa_CLI
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]	; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				; Save current flags to r0
	tst		r3, #1					; Are we in protected mode (or in VM mode)?
	bne		op_fa_prot_r0			; Yes we are, go handle protected mode CLI!
	GLOBAL	op_fa_real_r0
op_fa_real_r0
	ldr		r2, [sp, #SP_FLAGS]		; Get the FLAGS value
	bic		r2, #FLAG_IF			; Clear the "Interrupts Enabled" bit
	str		r2, [sp, #SP_FLAGS]		; Save the FLAGS value
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0
	
; ------------------- FB = STI ----------------------------------------
op_fb_STI
	;-------
	; Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	;-------
	ldrb	r3, [sp, #SP_CPU_CR0]				; Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							; Save current flags to r0
	tst		r3, #1								; Are we in protected mode (or in VM mode)?
	bne		op_fb_prot_r0						; Yes we are, go handle protected mode CLI!
	GLOBAL	op_fb_real_r0
op_fb_real_r0
	; ----- Interrupts are getting enabled, check for pending interrupts
	test_for_pending_IRQ_r1r2					; Zero flag set if we have a pending IRQ
	bne		%f1									; Jump if no interrupt pending
	;-------
	; Interrupts got enabled and an interrupt is pending.
	; Start handling it... 
	;-------
	mov		r1, #IRQ_ON
	str		r1, [sp, #SP_IRQFLAG]
	ldr		r1,[sp, #SP_FLAGS]					; Get the EXTRAFLAGS value
	msr		cpsr_f,r0							; Restore flags
	orr		r1,#FLAG_IF							; Set the "Interrupts Enabled" bit
	str		r1,[sp, #SP_FLAGS]					; Save the EXTRAFLAGS value
	;-------
	; ...but only after we have executed the next opcode! (Star Control 2)
	;-------
	ldrb	r0,[r12],#1							; Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				; r2 high halfword = logical DS segment, clear segment override flags
	ldr		pc,[sp, r0, lsl #2]					; Jump to the opcode handler
	;-------
	; Interrupts got enabled but no interrupt is pending.
	; Just set the interrupt flag and return.
	;-------
1	ldr		r1,[sp, #SP_FLAGS]					; Get the FLAGS value
	orr		r1,#FLAG_IF							; Set the "Interrupts Enabled" bit
	str		r1,[sp, #SP_FLAGS]					; Save the FLAGS value
	b		restore_flags_from_r0				; Jump back to loop, setting the flags from r0


; ------------------- PIC port handling -------------------------------
;
;

	;-------
	; IN AL,20 = Return bitmask of active IRQs.
	; If Port20RequestISSR is on, we need to return the IRQs that are currently being serviced,
	; else return a bitmask of interrupts currently active (== pending).
	;-------
in_al_20	
	ldr		r2, =IRQPending
	ldrb	r0, [r2, #(4*9+1)]					; Get current Port20RequestISSR value
	tst		r0, #1								; Are we to return Pending (EQ) or InService (NE) values?
	orr		r0, #0xFF							; Default to Pending ...
	lslne	r0, #16								; ... but return InService values if ISSR says so.
	ldr		r1, [r2]							; IRQ0
	bic		eax, #0xFF							; Clear current AL value
	tst		r1, r0
	ldr		r1, [r2, #4]						; IRQ1
	orrne	eax, #0x01							; IRQ0 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*2)]					; IRQ2 == IRQ12
	orrne	eax, #0x02							; IRQ1 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*3)]					; IRQ3
	orrne	eax, #0x04							; IRQ2 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*7)]					; IRQ7
	orrne	eax, #0x08							; IRQ3 is pending/being serviced
	tst		r1, r0
	orrne	eax, #0x80							; IRQ7 is pending/being serviced
	bx		lr

	;-------
	; IN AL,21 = Return IRQ mask
	;-------
in_al_21
	ldr		r2,=Port21Data
	ldrb	r1, [r2]
	bic		eax, #0xFF							; Clear current AL value
	orr		eax, r1								; Put IRQ mask into AL
	bx		lr

	;-------
	; Port 0x20. On input r1 == AL value
	;-------
out_20_al
	and		r0, r1, #0x38
	cmp		r0, #0x20							; OCW2 EOI operations?
	bne		out_20_cont
	;=======
	; EOI operations
	;=======
	tst		r1, #0x40							; Specific EOI?
	and		r0, r1, #7							; r0 = the interrupt number
	lsl		r0, #2
	;-------
	; First acknowledge this interrupt (mark it not being serviced)
	;-------
	ldr		r2, =IRQPending
	ldr		r1, [r2, #4*8] 						; r1 = The currently active IRQ number
	sub		r1, #(8*4)
	cmpne	r0, r1								; Is this the correct IRQ for the specific EOI?
	bxne	lr									; Nope, so do nothing!
	add		r1, #2
	mov		r0, #0
	strb	r0, [r2, r1]						; Clear the in-progress bit.
	IF IRQTRACE = 1
	;-------
	; Trace the IRQ0 request time
	;-------
	push	{r3, r12, lr}
	add		r1, #(8*4)-2
	orr		r1, #0x40000000						; r1 = event = EOI(IRQ_?)
	ldr		r0, [sp, #SP_EX_CSIP+4*(2+3)]		; r0 = SP_EX_CSIP value
	mov		r2, #0								; r2 = current QueryPerformanceCounter value needs to be checked
	bl		irqtrace
	pop		{r3, r12, lr}
	ENDIF
	IF EOITRACE = 1
	push	{lr}
	cmp		r1, #2
	bne		%f1
	bl		eoitrace
1	pop		{lr}
	ENDIF	
	;-------
	; Make IRQActive point to the previously activated IRQ, if any.
	;-------
	ldr		r2, =IRQPending
	ldrb	r1, [r2, #2]						; IRQ0
	tst		r1, #1								; Is it being serviced?
	movne	r1, #((8+0)*4)
	bne		%f1
	ldrb	r1, [r2, #4+2]						; IRQ1
	tst		r1, #1								; Is it being serviced?
	movne	r1, #((8+1)*4)
	bne		%f1
	ldrb	r1, [r2, #(4*2)+2]					; IRQ2
	tst		r1, #1								; Is it being serviced?
	movne	r1, #((8+2)*4)						; TODO! Handle IRQ12 properly!
	bne		%f1
	ldrb	r1, [r2, #(4*3)+2]					; IRQ3
	tst		r1, #1								; Is it being serviced?
	movne	r1, #((8+3)*4)
	bne		%f1
	ldrb	r1, [r2, #(4*7)+2]					; IRQ7
	tst		r1, #1								; Is it being serviced?
	movne	r1, #((8+7)*4)
	moveq	r1, #((8+5)*4)						; Point to IRQ5 if no active IRQs
1	str		r1, [r2, #4*8] 						; r1 = The currently active IRQ number
	bx		lr

	ALIGN	8
	
out_20_cont
	tst		r1, #0x10							; ICW1 issued?
	bne		%f1
	tst		r1, #0x08							; OCW3 issued?
	beq		bad_out_port
	;=======
	; OCW3 issued
	;=======
	tst		r1, #2								; Function select?
	beq		bad_out_port
	;-------
	; OCW3: Function Select
	;-------
	ldr		r2, =Port20RequestISSR
	and		r1, #1
	strb	r1, [r2]
	bx		lr
	;=======
	; ICW1 issued
	;=======
1	ldr		r2, =Port20ICWIndex
	mov		r0, #1
	strb	r0, [r2]							; pic->icw_index = 1;
	and		r0, r1, #1
	add		r0, #1								; Changed from 1 to 2 on 1.1.2011! Is this OK?
	strb	r0, [r2, #1]						; pic->icw_words=2 + (val&0x01);
	bx		lr
	
	;-------
	; Port 0x21. On input r1 == AL value
	;-------
out_21_al
	ldr		r2,=Port21Data
	ldrb	r0, [r2, #2]						; Get pic->icw_index
	cmp		r0, #1
	beq		%f1
	bgt		%f2
	;-------
	; pic->icw_index == 0: Interrupt mask register
	;-------
	strb	r1, [r2]							; Save new Port21 data value
	bx		lr
	;-------
	; pic->icw_index == 1: ICW2 (interrupt vector)
	;-------
1	and		r0, r1, #0xF8
	ldr		r1, =IRQ0Offset
	strb	r0, [r1]							; Save the offset of IRQ0
	mov		r0, #2								; pic->icw_index++
	strb	r0, [r2, #2]						; Save pic->icw_index
	bx		lr
	;-------
2	cmp		r0, #3
	beq		%f3
	bgt		bad_out_port
	;-------
	; pic->icw_index == 2: ICW3 (ignored)
	;-------
	ldrb	r0, [r2, #3]
	cmp		r0, #2								; pic->icw_words == 2?
	moveq	r0,	#0								; Yep, so pic->icw_index = 0
	movne	r0, #3								; Nope, so pic->icw_index = 3
	strb	r0, [r2, #2]
	bx		lr
	;-------
	; pic->icw_index == 3: ICW4 (auto-eoi mode)
	;-------
3	and		r0, r1, #2							; r0 = val&2
	strb	r0, [r2, #1]						; pic->auto_eoi=(val & 0x2)>0;
	mov		r0,	#0								; pic->icw_index = 0
	strb	r0, [r2, #2]						; Save pic->icw_index
	bx		lr

	IF EOITRACE = 1
eoitrace
	push	{r0-r2}					; r0 = current pending value, r2 = pointer to IRQPending table
	ldr		r1, [sp, #SP_PHYS_CS+6*4]	; Get current physical CS from stack
	ldr		r0, [sp, #SP_CS_BASE+6*4]	; r0 = Current logical CS
	sub		r1, r12, r1				; r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	lsl		r0, #(16-REAL_SEGMENT_SHIFT)
	add		r0, r1					; r0 = CS:IP
	ldr		r2,=BIOS_F000
	ldr		r2, [r2]
	add		r2, #0xD000
	ldr		r1, [r2]				; Current offset at F000:D000, table starts at F000:D004
	add		r1, #4
	str		r0, [r2, r1]
	mrs		r0,cpsr					; Save flags to r0
	cmp		r1, #(4*64)
	movge	r1, #0
	str		r1, [r2]
	msr		cpsr_f,r0
	pop		{r0-r2}
	bx		lr
	ENDIF


	AREA pic_DATA, DATA, READWRITE
	ALIGN	4

	GLOBAL	pic_serialize_start
pic_serialize_start

	GLOBAL	IRQPending
IRQPending							; offset 0
	DCD	0						; IRQ0 = INT8 = Timer
	DCD	0						; IRQ1 = INT9 = Keyboard
	DCD	0						; IRQ2 = IRQ12 = INT74 = PS/2 Mouse
	DCD	0						; IRQ3 = INTB = Mouse (COM2)
	DCD	0						; IRQ4 = INTC = COM1
	DCD	0
	DCD	0						; IRQ6 = INTE = User exit interrupt
	DCD	0						; IRQ7 = INTF = SoundBlaster
IRQActive							; offset 4*8
	DCD	0

	GLOBAL	IRQ0Offset
IRQ0Offset							; Offset added to the IRQ numbers 0..7 to get the INT number.
	DCB	8
	
Port20RequestISSR					; offset 4*9 + 1
	DCB	0
	
Port21Data							; offset 4*9 + 2
	DCB	0

Port20AutoEOI						; offset 4*9 + 3
	DCB	0
	
Port20ICWIndex						; offset 4*9 + 4
	DCB	0

Port20ICWWords						; offset 4*9 + 5
	DCB	0
	
	GLOBAL	pic_serialize_end
pic_serialize_end

	GLOBAL BRUserBreak
BRUserBreak
	DCB	"User-requested breakpoint"
	DCB	0x0a, 0

	END
