//=============================================================================
// pic.s
//
// This file contains interrupt handling -related stuff:
//	- IRQStart = Switching the emulated CPU to handling a hardware interrupt.
//	- ???IRQ = Functions called by external C handlers after a host event
//	  (real-life hardware interrupt or keypress) has occurred.
//	- CLI / STI opcode handlers.
//	- I/O port 0x20 and 0x21 (PIC chip) handlers.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2013 Patrick "Pate" Aalto
//	
// Redistribution and use in source or binary form, with or without modifications,
// is NOT permitted without specific prior written permission from the author.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//=============================================================================

	.arch armv6
	.file	"pic.s"
	
    .text
    .align  2
    .code   32
	.arm

	.global	IRQStart
	
	.global	int_flag_on
	.global	int_and_cld
	.global	int_and_std
	
	.global	op_fa_CLI
	.global	op_fb_STI
	
	.global	in_al_20
	.global	in_al_21
	.global	out_20_al
	.global	out_21_al

#include "defines.inc"
#include "macros.inc"

#define	EOITRACE	0

.macro select_IRQ_to_handle_r0r1
	push	{r2}					// Save r2 (flags) to stack
	ldr		r1,=IRQPending
	mov		r2, #0x00010000			// Mark this IRQ as being serviced
	// ----- Test for IRQ0 -----
	ldr		r0, [r1]				// Get the IRQPending[0] value
	cmp		r0, #((8+0)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	// ----- Test for IRQ1 -----
	add		r1, #4
	ldr		r0, [r1]				// Get the IRQPending[1] value
	cmp		r0, #((8+1)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	// ----- Test for IRQ12 -----
	add		r1, #4
	ldr		r0, [r1]				// Get the IRQPending[2] value
	cmp		r0, #((8+2)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	// ----- Test for IRQ3 (COM2) -----
	add		r1, #4
	ldr		r0, [r1]				// Get the IRQPending[3] value
	cmp		r0, #((8+3)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	// ----- Test for IRQ4 (COM1) -----
	add		r1, #4
	ldr		r0, [r1]				// Get the IRQPending[4] value
	cmp		r0, #((8+4)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	// ----- Test for IRQ7 -----
	add		r1, #3*4
	ldr		r0, [r1]				// Get the IRQPending[7] value
	cmp		r0, #((8+7)*4)			// Was it pending but not yet being serviced?
	streq	r2, [r1]				// Yep, mark it being serviced
	beq		.handle_vector_r0_flags_pushed	// Yep, go handle it
	pop		{r2}					// Restore flags to r2
.endm
.macro test_for_pending_IRQ_r1r2
	ldr		r2, =IRQPending
	ldr		r1, [r2]				// IRQ0
	cmp		r1, #((8+0)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #4]			// IRQ1
	cmpne	r1, #((8+1)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #(4*2)]		// IRQ12
	cmpne	r1, #((8+2)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #(4*3)]		// IRQ3
	cmpne	r1, #((8+3)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #(4*4)]		// IRQ4
	cmpne	r1, #((8+4)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #(4*7)]		// IRQ7
	cmpne	r1, #((8+7)*4)			// Is it pending but not yet being serviced?
	ldrne	r1, [r2, #(4*6)]		// IRQ6
	cmpne	r1, #((8+6)*4)			// Is it pending but not yet being serviced?
	//-------
	// Zero flag set if we have a pending IRQ.
	//-------
.endm


// ------------------- IRQStart code -----------------------------------
// Start actually handling an IRQ. What we need to do here:
// - Fix the IRQFlag to use the normal jump table again
// - Push Flags, IP and CS to stack
// - Make CS:IP point to the IRQ handler address from INTVectors
// - Go back to loop
// 
	.global	IRQStart_r1
IRQStart_r1:
	msr		cpsr_f, r1				// Restore flags from r1
IRQStart:
	mrs		r2,cpsr					// Save the CPU flags into r2
	sub		r12,#1					// Rewind the opcode pointer
	//-------
	// Tell we are currently handling an interrupt
	//-------
	ldr		r1,[sp, #SP_FLAGS]		// Get the FLAGS value
	tst		r1, #FLAG_TF			// Is the Trap Flag on?
	bne		.handle_trap			// Jump to handle single stepping if it is
	//-------
	// Determine the IRQ vector we need to jump to
	//-------
	irq_lock
	select_IRQ_to_handle_r0r1		// Jump to .handle_vector_r0_flags_pushed if we found a pending IRQ
	//-------
	// No IRQ vector to jump to, so this is a debugger break request. Go to the debugger.
	//-------
	ldr		r0, =BRUserBreak
	ldr		r1, =BreakReason
	str		r0, [r1]				// Tell the reason for breaking was a user request
	//-------
	// Restore the flags
	//-------
	msr		cpsr_f, r2
	irq_unlock
	b		debug_trap_false		// Go to the debugger
	//-------
	// Start handling the interrupt.
	// Input: r0 = IRQ vector address where we need to jump to.
	//		 r2 = Flags we need to push
	//-------
.handle_vector_r0_flags_pushed:
	//-------
	// Mark the active interrupt number.
	//-------
	ldr		r1, =IRQPending
	str		r0, [r1, #4*8]			// Mark that this is the currently active IRQ
	//-------
	// Test for auto-eoi, and clear the in-service bit if we have auto-eoi on.
	//-------
	ldrb	r2, [r1, #4*9+3]
	cmp		r2, #0
	subne	r1, #(8*4)
	ldrne	r2, [r1, r0]
	bicne	r2, #0x00010000			// Clear the in-service bit
	strne	r2, [r1, r0]
	irq_unlock
	//-------
	// Adjust the IRQ0..IRQ7 start address (for Windows 3.0),
	// and move the IRQ12 vector to INT 74.
	//-------
	cmp		r0, #(8+2)*4			// If it is IRQ2, ...
	ldr		r2,=IRQ0Offset
	ldrb	r2, [r2]
	sub		r0, #8*4
	ldr		r1, =cpu_cr0
	add		r0, r2, lsl #2
	ldrb	r1, [r1]
	moveq	r0, #0x74*4				// ... make it IRQ12
	//-------
	// Test for protected mode
	//-------
	pop		{r2}					// Restore flags
	tst		r1, #1
	bne		irq_prot
	//-------
	// Push the x86 flags. After that we can freely change the ARM flags.
	//-------
	msr		cpsr_f, r2				// Set the real CPU flags from r2
	push_flags_16 r2 r1	r3			// Push the x86 flags
	mov		r2, #0
	mov		r1, r0					// Save the IRQ number*4 to r1
	calc_linear_address_r2			// Get the physical address of the beginning of RAM, destroys r0!
	ldr		r2,[r2, r1]				// Get the IRQ vector address, high halfword = segment, low halfword = offset
	// ----- Then store current CS:IP to stack
	ldr		r0, [sp, #SP_CS_VALUE]	// r0 = Current logical CS
	push_hword r0 r1 r3
	ldr		r1, [sp, #SP_PHYS_CS]	// r1 = Current physical CS
	sub		r1, r12, r1				// r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1 r0 r3
	// ----- Then get new logical IP (zero-extended) to r12
	mov		r12, r2, lsl #16		// r12 high 16 bits contain the new logical IP
	// ----- Then get new logical CS (zero-extended) to r0
	mov		r0, r2, lsr #16			// Now r0 = new logical CS
	// ----- Then save new CS into REG_CS
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]	// Save new CS value to stack
	str		r2, [sp, #SP_CS_BASE]
	// ----- And finally calculate new physical IP
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]	// Store new physical CS into stack
	add		r12, r2, r12, lsr #16	// r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	// ----- Clear the Trap and Interrupts flags
	ldr		r0,[sp, #SP_FLAGS]	// Get the EXTRAFLAGS value
	bic		r0, #(FLAG_TF|FLAG_IF)
	str		r0,[sp, #SP_FLAGS]	// Save the new extra flags
	// ----- Clear the IRQFlag/semaphore, OK for further interrupts
	mov		r0, #IRQ_OFF
	str		r0, [sp, #SP_IRQFLAG]
	b		loop
	//-------
	// Handle trap (single step) interrupt.
	// INT01 = CPU-generated - SINGLE STEP
	// 	Desc: Generated after each instruction if TF (trap flag) is set
	// 	TF is cleared on invoking the single-step interrupt handler 
	//	Notes: Interrupts are prioritized such that external interrupts are invoked
	//	after the INT 01 pushes CS:IP/FLAGS and clears TF, but before the first instruction of the handler executes.
	//	Used by debuggers for single-instruction execution tracing, such as MS-DOS DEBUGs T command.
	//-------
	.global	.handle_trap
.handle_trap:
	ldr		r1, =TrapFlag
	ldr		r0, [r1]				// Get the current Trap Flag value
	eors	r0, #1					// Toggle the flag
	str		r0, [r1]				// and save it
	bne		.handle_trap_opcode
	//-------
	// We are in a phase where we need to push flags and CS:IP,
	// clear the TF flag, check for a pending real IRQ, and finally jump to INT01 handler.
	//-------
	msr		cpsr_f, r2				// Set the real CPU flags from r2
	push_flags_16 r2 r0	r3			// Push the x86 flags
	// ----- Clear the Trap and Interrupts flags
	ldr		r0,[sp, #SP_FLAGS]	// Get the EXTRAFLAGS value
	bic		r0, #(FLAG_TF|FLAG_IF)	// Clear the Trap and Interrupt flags
	str		r0,[sp, #SP_FLAGS]	// Save the new extra flags
	// ----- Then store current CS:IP to stack
	ldr		r0, [sp, #SP_CS_VALUE]	// r0 = Current logical CS
	ldr		r1, [sp, #SP_PHYS_CS]	// r1 = Current physical CS
	push_hword r0 r2 r3
	sub		r1, r12, r1				// r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	push_hword r1 r2 r3
	// ----- Then get new logical IP (zero-extended) to r12
	mov		r2, #0
	calc_linear_address_r2			// Get the physical address of the beginning of RAM
	ldr		r2,[r2, #(4*1)]			// Get the INT1 vector address, high halfword = segment, low halfword = offset
	mov		r12, r2, lsl #16		// r12 high 16 bits contain the new logical IP
	// ----- Then get new logical CS (zero-extended) to r0
	mov		r0, r2, lsr #16			// Now r0 = new logical CS
	// ----- Then save new CS into REG_CS
	mov		r2, r0, lsl #REAL_SEGMENT_SHIFT
	str		r0, [sp, #SP_CS_VALUE]	// Save new CS value to stack
	str		r2, [sp, #SP_CS_BASE]
	// ----- And finally calculate new physical IP
	calc_linear_address_r2
	str		r2,[sp, #SP_PHYS_CS]	// Store new physical CS into stack
	add		r12, r2, r12, lsr #16	// r12 = new physical CS:IP = physical base + new IP + (new CS << 4)
	// ----- Then check for a pending real IRQ
	mrs		r2, cpsr				// Save flags to r2
	select_IRQ_to_handle_r0r1		// Jump to .handle_vector_r0_flags_pushed if we found a pending IRQ
	msr		cpsr_f, r2				// Restore flags
	// ----- Clear the IRQFlag/semaphore, OK for further interrupts
	mov		r0, #IRQ_OFF
	str		r0, [sp, #SP_IRQFLAG]
	b		loop					// Go start the INT1 handler
	//-------
	// We are in a phase where we need to first execute the opcode, and only then jump to INT1
	//-------
.handle_trap_opcode:
	msr		cpsr_f, r2				// Restore flags
	ldrb	r0,[r12],#1				// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]	// r2 high halfword = logical DS segment, clear segment override flags
	ldr		pc,[sp, r0, lsl #2]		// Jump to the opcode handler

#if !defined(RPi) && !defined(Roku)

// ------------------- TimerIRQ code -----------------------------------
// Called by timer, mark that an interrupt is pending and make the next
// opcode fetch jump into the IRQStart instead.
// 
	.global TimerIRQ
TimerIRQ:
	//-------
	// First test whether we handle interrupts at all.
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Yep, so we are not running, just ignore this IRQ.
	//-------
	// Mark that IRQ0 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+0)*4)			// Use the INT vector address as the pending flag
	//-------
	// Make sure we are not already servicing this IRQ
	// "Bart vs Space Mutants" hangs if we handle two timer IRQs right after each other!
	//-------
	irq_lock
	ldr		r2, [r0]				// r2 = previous IRQ pending value
	str		r1, [r0]				// mark it pending.
	cmp		r2, #0x00010000			// IRQ already being serviced?
	strhs	r2, [r0]				// Yep, so ignore this new timer interrupt ...
	bhs		.unlock_exit			// ... and just return!
	b		1f						// Else continue
	//-------
	// Registers here:
	//	r0 = IRQPending[IRQNumber] address
	//	r1 = INT vector address (0x08*4..0x0F*4)
	//	r2 = free
	//	r3 = IRQ_FLAG address (in stack!)
	//-------
	.global	common_irq
common_irq:							// Now r2 = #SP_FLAGS value, r3 = SP_IRQFLAG address
	irq_lock
	//-------
	// Make sure we are not already servicing this IRQ
	//-------
	ldr		r2, [r0]				// r2 = previous IRQ pending value
	str		r1, [r0]				// mark it pending.
	cmp		r2, #0x00010000			// IRQ already being serviced?
	orrhs	r2, r1
	strhs	r2, [r0]				// Mark that it is being serviced but also pending.
	bhs		.unlock_exit			// This IRQ was already being serviced, just return!
#if 0	
	//-------
	// Check if this IRQ has been masked, and return if it has.
	//-------
	ldr		r2, =Port21Data
	ldrb	r2, [r2]
	sub		r1, #8*4
	lsr		r1, #2
	mov		r0, #1
	lsl		r0, r0, r1
	tst		r2, r0
	bne		.unlock_exit
#endif	
	//-------
	// Set (and get) the IRQFlag semaphore
	//-------
1:	mov		r1, #IRQ_ON
	mov		r0, r3					// r0 = IRQFlagAddr
	ldr		r2, [r0]				// Get the current value into r2
	cmp		r2, r1					// Make sure an IRQ is not already pending
	ldr		r2,[r3, #(SP_FLAGS-SP_IRQFLAG)]	// Get the #SP_FLAGS into r2
	beq		.unlock_exit			// IRQ already pending, just return
	//-------
	// IRQ flag was not already set, check Extra Flags IRQ bits
	//-------
	tst		r2, #FLAG_IF 			// Are interrupts enabled?
	beq		.unlock_exit			// Nope, skip this IRQ for now!
	//-------
	// OK to handle this interrupt now!
	//-------
	str		r1,[r0]					// Set the IRQ flag, start handling an IRQ.
.unlock_exit:
	irq_unlock
	bx		lr
	
// ------------------- SoundBlasterIRQ code ----------------------------
// Called by fifoInternalRecvInterrupt(), mark that a SB interrupt is pending
// and make the next opcode fetch jump into the IRQStart instead.
// Also called from "ports_SB.s" when an interrupt is requested via SB DSP command!
// 
	.global SBIRQ
SBIRQ:
#if 1
	ldr		r2, =sb_irq_ack
	mov		r1, #0
	strb	r1, [r2]
#endif
	//-------
	// Test whether we handle interrupts at all
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Not running, ignore SoundBlaster interrupts
	//-------
	// Mark that IRQ7 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+7)*4)			// Use the INT vector address as the pending flag
	add		r0, #(4*7)
	b		common_irq				// The rest of the code is common to all interrupt types
	
// ------------------- KeyboardIRQ code --------------------------------
// Launch an IRQ1 to handle the keyboard scan code
	.global KeyboardIRQ
KeyboardIRQ:
	//-------
	// Test whether we handle interrupts at all
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Not running, ignore keyboard interrupts
	//-------
	// Mark that IRQ1 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+1)*4)			// Use the INT vector address as the pending flag
	add		r0, #(4*1)
	b		common_irq				// The rest of the code is common to all interrupt types

// ------------------- MouseIRQ code -----------------------------------
// Launch an IRQ3 to handle the mouse/COM2 user subroutine call
#ifdef USECOM2
	.global COM2IRQ
COM2IRQ:
#else
#ifdef IOS
	.global _MouseIRQ
_MouseIRQ:							// Not fixed in fix_asm_ios.pl because it is not unique (vs PS2MouseIRQ)!
#else
	.global MouseIRQ
MouseIRQ:
#endif
#endif
	//-------
	// Test whether we handle interrupts at all
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Not running, ignore keyboard interrupts
	//-------
	// Mark that IRQ3 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+3)*4)			// Use the INT vector address as the pending flag
	add		r0, #(4*3)
	b		common_irq				// The rest of the code is common to all interrupt types

// ------------------- PS/2 Mouse IRQ code -----------------------------
// Launch an IRQ2 to handle the mouse user subroutine call
	.global PS2MouseIRQ
PS2MouseIRQ:
	//-------
	// Test whether we handle interrupts at all
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Not running, ignore keyboard interrupts
	//-------
	// Mark that IRQ2 == IRQ12 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+2)*4)			// Use the INT vector address as the pending flag
	add		r0, #(4*2)
	b		common_irq				// The rest of the code is common to all interrupt types

// ------------------- COMIRQ code -----------------------------------
// Launch an IRQ4 to handle the COM port user subroutine call
	.global COM1IRQ
COM1IRQ:
	//-------
	// Test whether we handle interrupts at all
	//-------
	ldr		r2,=IRQFlagAddr			// Get address of the address
	ldr		r3,[r2]					// Get the actual address into r3
	cmp		r3, #0					// Is the address = 0 ?
	bxeq	lr						// Not running, ignore keyboard interrupts
	//-------
	// Mark that IRQ4 is pending
	//-------
	ldr		r0,=IRQPending
	mov		r1, #((8+4)*4)			// Use the INT vector address as the pending flag
	add		r0, #(4*4)
	b		common_irq				// The rest of the code is common to all interrupt types

#endif

.ltorg								// Dump the current literal pool here

// ------------------- 9D = POPF / CF = IRET ---------------------------
// IRQ helper subroutines for POPF and IRET when the INT flag changes.
//
	//-------
	// Interrupts got enabled, check for pending interrupts
	//-------
int_flag_on:
	irq_lock
	test_for_pending_IRQ_r1r2		// Zero flag set if we have a pending IRQ
	//-------
	// Interrupts enabled and an interrupt is pending, start handling it
	//-------
	moveq	r1, #IRQ_ON
	streq	r1,[sp, #SP_IRQFLAG]	// Set the IRQFlag
#if 0
	moveq	r1, #1
	movne	r1, #2
	str		r1, [sp, #SP_FREE8]
#endif	
	irq_unlock
	b		restore_flags_from_r0	// Go to restore the cpu flags

// ------------------- FA = CLI ----------------------------------------
op_fa_CLI:
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]	// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr				// Save current flags to r0
	tst		r3, #1					// Are we in protected mode (or in VM mode)?
	bne		op_fa_prot_r0			// Yes we are, go handle protected mode CLI!
	.global	op_fa_real_r0
op_fa_real_r0:
	//-------
	// Set the IRQFlag semaphore on (so we are not interrupted during the operation)
	//-------
	irq_lock
	//-------
	// - If current IRQFlag = IRQ_OFF, we can just turn the interrupts off and all is well.
	//-------
	ldr		r1, [sp, #SP_IRQFLAG]	// Get current value into r1
	ldr		r2, [sp, #SP_FLAGS]		// Get the FLAGS value
	cmp		r1, #IRQ_ON				// Should we already be runnning an IRQ?
	beq		1f						// Yep, so go handle that first.
	bic		r2, #FLAG_IF			// Clear the "Interrupts Enabled" bit
	str		r2, [sp, #SP_FLAGS]		// Save the FLAGS value
	irq_unlock
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0
	//-------
	// - Else we behave like the interrupt had just occurred before this CLI opcode
	//-------
1:	irq_unlock
	msr		cpsr_f,r0				// Restore flags
	b		IRQStart				// Go handle the IRQ immediately
	
// ------------------- FB = STI ----------------------------------------
op_fb_STI:
	//-------
	// Determine if we are in real mode, and jump to a handler in "cpu_prot.s" if not.
	//-------
	ldrb	r3, [sp, #SP_CPU_CR0]				// Get the lowest byte of cpu_cr0
	mrs		r0, cpsr							// Save current flags to r0
	tst		r3, #1								// Are we in protected mode (or in VM mode)?
	bne		op_fb_prot_r0						// Yes we are, go handle protected mode CLI!
	.global	op_fb_real_r0
op_fb_real_r0:
	irq_lock
	// ----- Interrupts are getting enabled, check for pending interrupts
	test_for_pending_IRQ_r1r2					// Zero flag set if we have a pending IRQ
	bne		1f									// Jump if no interrupt pending
	//-------
	// Interrupts got enabled and an interrupt is pending.
	// Start handling it... 
	//-------
	mov		r1, #IRQ_ON
	str		r1, [sp, #SP_IRQFLAG]
	ldr		r1,[sp, #SP_FLAGS]					// Get the EXTRAFLAGS value
	msr		cpsr_f,r0							// Restore flags
	orr		r1,#FLAG_IF							// Set the "Interrupts Enabled" bit
	str		r1,[sp, #SP_FLAGS]					// Save the EXTRAFLAGS value
	irq_unlock
	//-------
	// ...but only after we have executed the next opcode! (Star Control 2)
	//-------
	ldrb	r0,[r12],#1							// Load opcode byte to r0, increment r12 by 1
	ldr		r2, [sp, #SP_DS_BASE]				// r2 high halfword = logical DS segment, clear segment override flags
	ldr		pc,[sp, r0, lsl #2]					// Jump to the opcode handler
	//-------
	// Interrupts got enabled but no interrupt is pending.
	// Just set the interrupt flag and return.
	//-------
1:	ldr		r1,[sp, #SP_FLAGS]					// Get the FLAGS value
	orr		r1,#FLAG_IF							// Set the "Interrupts Enabled" bit
	str		r1,[sp, #SP_FLAGS]					// Save the FLAGS value
	irq_unlock
	b		restore_flags_from_r0				// Jump back to loop, setting the flags from r0


// ------------------- PIC port handling -------------------------------
//
//

	//-------
	// IN AL,20 = Return bitmask of active IRQs.
	// If Port20RequestISSR is on, we need to return the IRQs that are currently being serviced,
	// else return a bitmask of interrupts currently active (== pending).
	//-------
in_al_20:	
	ldr		r2, =IRQPending
	ldrb	r0, [r2, #(4*9+1)]					// Get current Port20RequestISSR value
	tst		r0, #1								// Are we to return Pending (EQ) or InService (NE) values?
	orr		r0, #0xFF							// Default to Pending ...
	lslne	r0, #16								// ... but return InService values if ISSR says so.
	ldr		r1, [r2]							// IRQ0
	bic		eax, #0xFF							// Clear current AL value
	tst		r1, r0
	ldr		r1, [r2, #4]						// IRQ1
	orrne	eax, #0x01							// IRQ0 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*2)]					// IRQ2 == IRQ12
	orrne	eax, #0x02							// IRQ1 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*3)]					// IRQ3
	orrne	eax, #0x04							// IRQ2 is pending/being serviced
	tst		r1, r0
	ldr		r1, [r2, #(4*7)]					// IRQ7
	orrne	eax, #0x08							// IRQ3 is pending/being serviced
	tst		r1, r0
	orrne	eax, #0x80							// IRQ7 is pending/being serviced
	bx		lr

	//-------
	// IN AL,21 = Return IRQ mask
	//-------
in_al_21:
	ldr		r2,=Port21Data
	ldrb	r1, [r2]
	bic		eax, #0xFF							// Clear current AL value
	orr		eax, r1								// Put IRQ mask into AL
	bx		lr

	//-------
	// Port 0x20. On input r1 == AL value
	//-------
out_20_al:
	and		r0, r1, #0x38
	cmp		r0, #0x20							// OCW2 EOI operations?
	bne		.out_20_cont
	//=======
	// EOI operations
	//=======
	tst		r1, #0x40							// Specific EOI?
	and		r0, r1, #7							// r0 = the interrupt number
	lsl		r0, #2
	//-------
	// First acknowledge this interrupt (mark it not being serviced)
	//-------
	ldr		r2, =IRQPending
	ldr		r1, [r2, #4*8] 						// r1 = The currently active IRQ number
	sub		r1, #(8*4)
	cmpne	r0, r1								// Is this the correct IRQ for the specific EOI?
	bxne	lr									// Nope, so do nothing!
#if defined(RPi) || defined(Roku)
	add		r2, r1
1:	ldrex	r0, [r2]
	and		r0, #0xFF							// Clear the in-progress value
	strex	r1, r0, [r2]						// Attempt to save the value. r1 = 1 if not exclusive access.
	cmp		r1, #0								// Did the update succeed?
	bne		1b									// Nope, so try again.
#else
	ldr		r0, [r2, r1]
	and		r0, #0xFF							// Was it pending?
	str		r0, [r2, r1]						// If yes, keep it pending, else clear the value.
#endif
#if EOITRACE
	push	{lr}
	cmp		r1, #0
	bne		1f
	bl		eoitrace
1:	pop		{lr}
#endif	
	//-------
	// Make IRQActive point to the previously activated IRQ, if any.
	//-------
	ldr		r2, =IRQPending
	ldr		r1, [r2]							// IRQ0
	cmp		r1, #0x00010000						// Is it being serviced?
	movhs	r1, #((8+0)*4)
	bhs		1f
	ldr		r1, [r2, #4]						// IRQ1
	cmp		r1, #0x00010000						// Is it being serviced?
	movhs	r1, #((8+1)*4)
	bhs		1f
	ldr		r1, [r2, #(4*2)]					// IRQ2
	cmp		r1, #0x00010000						// Is it being serviced?
	movhs	r1, #((8+2)*4)						// TODO! Handle IRQ12 properly!
	bhs		1f
	ldr		r1, [r2, #(4*3)]					// IRQ3
	cmp		r1, #0x00010000						// Is it being serviced?
	movhs	r1, #((8+3)*4)
	bhs		1f
	ldr		r1, [r2, #(4*7)]					// IRQ7
	cmp		r1, #0x00010000						// Is it being serviced?
	movhs	r1, #((8+7)*4)
	movlo	r1, #((8+5)*4)						// Point to IRQ5 if no active IRQs
1:  str		r1, [r2, #4*8] 						// r1 = The currently active IRQ number
#if 0
	//-------
	// All done if interrupts are not currently enabled
	//-------
	ldr		r1,[sp, #SP_FLAGS+8]	// Get the EXTRAFLAGS value
	tst		r1, #FLAG_IF	// Are interrupts enabled?
	bxeq	lr
	//-------
	// Interrupts are enabled, check if there are pending interrupts.
	// We do this before acknowledging this interrupt to keep from getting new IRQs before we are ready.
	//-------
	test_for_pending_IRQ_r1r2		// Zero flag set if we have a pending IRQ
	bxne	lr
	//-------
	// Interrupts are enabled and there is an interrupt pending, start handling it.
	// Also acknowledge this previous interrupt.
	//-------
	ldr		r2, =SM_IRQFLAG
	ldr		r1, =IRQ_ON
	str		r1, [r2]
#endif
	bx		lr

	.text
	.align	2
	
.out_20_cont:
	tst		r1, #0x10							// ICW1 issued?
	bne		1f
	tst		r1, #0x08							// OCW3 issued?
	beq		bad_out_port
	//=======
	// OCW3 issued
	//=======
	tst		r1, #2								// Function select?
	beq		bad_out_port
	//-------
	// OCW3: Function Select
	//-------
	ldr		r2, =Port20RequestISSR
	and		r1, #1
	strb	r1, [r2]
	bx		lr
	//=======
	// ICW1 issued
	//=======
1:	ldr		r2, =Port20ICWIndex
	mov		r0, #1
	strb	r0, [r2]							// pic->icw_index = 1;
	and		r0, r1, #1
	add		r0, #1								// Changed from 1 to 2 on 1.1.2011! Is this OK?
	strb	r0, [r2, #1]						// pic->icw_words=2 + (val&0x01);
	bx		lr
	
	//-------
	// Port 0x21. On input r1 == AL value
	//-------
out_21_al:
	ldr		r2,=Port21Data
	ldrb	r0, [r2, #2]						// Get pic->icw_index
	cmp		r0, #1
	beq		1f
	bgt		2f
	//-------
	// pic->icw_index == 0: Interrupt mask register
	//-------
	strb	r1, [r2]							// Save new Port21 data value
	bx		lr
	//-------
	// pic->icw_index == 1: ICW2 (interrupt vector)
	//-------
1:	and		r0, r1, #0xF8
	ldr		r1, =IRQ0Offset
	strb	r0, [r1]							// Save the offset of IRQ0
	mov		r0, #2								// pic->icw_index++
	strb	r0, [r2, #2]						// Save pic->icw_index
	bx		lr
	//-------
2:	cmp		r0, #3
	beq		3f
	bgt		bad_out_port
	//-------
	// pic->icw_index == 2: ICW3 (ignored)
	//-------
	ldrb	r0, [r2, #3]
	cmp		r0, #2								// pic->icw_words == 2?
	moveq	r0,	#0								// Yep, so pic->icw_index = 0
	movne	r0, #3								// Nope, so pic->icw_index = 3
	strb	r0, [r2, #2]
	bx		lr
	//-------
	// pic->icw_index == 3: ICW4 (auto-eoi mode)
	//-------
3:	and		r0, r1, #2							// r0 = val&2
	strb	r0, [r2, #1]						// pic->auto_eoi=(val & 0x2)>0;
	mov		r0,	#0								// pic->icw_index = 0
	strb	r0, [r2, #2]						// Save pic->icw_index
	bx		lr

#if EOITRACE
eoitrace:
	push	{r0-r2}					// r0 = current pending value, r2 = pointer to IRQPending table
	ldr		r1, [sp, #SP_PHYS_CS+6*4]	// Get current physical CS from stack
	ldr		r0, [sp, #SP_CS_BASE+6*4]	// r0 = Current logical CS
	sub		r1, r12, r1				// r1 = Current physical IP  - (physical base + (CS << 4)) = Current logical IP
	lsl		r0, #(16-REAL_SEGMENT_SHIFT)
	add		r0, r1					// r0 = CS:IP
	ldr		r2,=BIOS_F000
	ldr		r2, [r2]
	add		r2, #0xD000
	ldr		r1, [r2]				// Current offset at F000:D000, table starts at F000:D004
	add		r1, #4
	str		r0, [r2, r1]
	mrs		r0,cpsr					// Save flags to r0
	cmp		r1, #(4*64)
	movge	r1, #0
	str		r1, [r2]
	msr		cpsr_f,r0
	pop		{r0-r2}
	bx		lr
#endif


	.data
	.align 2

	.global	pic_serialize_start
pic_serialize_start:

	.global	IRQPending
IRQPending:							// offset 0
	.word	0						// IRQ0 = INT8 = Timer
	.word	0						// IRQ1 = INT9 = Keyboard
	.word	0						// IRQ2 = IRQ12 = INT74 = PS/2 Mouse
	.word	0						// IRQ3 = INTB = Mouse (COM2)
	.word	0						// IRQ4 = INTC = COM1
	.word	0
	.word	0						// IRQ6 = INTE = User exit interrupt
	.word	0						// IRQ7 = INTF = SoundBlaster
IRQActive:							// offset 4*8
	.word	0

	.global	IRQ0Offset
IRQ0Offset:							// Offset added to the IRQ numbers 0..7 to get the INT number.
	.byte	8
	
Port20RequestISSR:					// offset 4*9 + 1
	.byte	0
	
Port21Data:							// offset 4*9 + 2
	.byte	0

Port20AutoEOI:						// offset 4*9 + 3
	.byte	0
	
Port20ICWIndex:						// offset 4*9 + 4
	.byte	0

Port20ICWWords:						// offset 4*9 + 5
	.byte	0
	
	.global	pic_serialize_end
pic_serialize_end:

	.global BRUserBreak
BRUserBreak:
	.ascii	"User-requested breakpoint"
	.byte	0x0a, 0

	.global	flagIFfmt
flagIFfmt:
	.ascii	"%08X op_%02X %02X"
	.byte	0x0a, 0
