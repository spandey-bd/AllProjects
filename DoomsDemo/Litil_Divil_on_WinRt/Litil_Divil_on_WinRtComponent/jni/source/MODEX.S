//=============================================================================
// MODEX.S
//
// This file contains routines to handle Mode-X graphics (256-color with
// various screen sizes), when the VGA register Chain-4 is off. In this mode
// the four VGA planes are organized as four adjacent pixels, so this file
// contains separate MODEX VRAM read/write opcode handlers that map between
// the x86 Mode-X memory order and linear VRAM that pax86 uses to speed up
// screen memory blitting. Each written byte may affect various bits on the
// output word (4 adjacent 8-bit pixels), depending on the VGA registers.
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

	.arch armv5te
	.file	"MODEX.s"
	
	.global	op_80_MODEX_r0r2
	
	.global	movsb_cld_RAM_MODEX
	.global	movsb_std_RAM_MODEX
	.global	movsb_cld_MODEX_MODEX
	.global	movsb_cld_MODEX_RAM
	.global	movsw_cld_RAM_MODEX
	

//
// Mode-X memory handling:
// The four planes are interleaved so that each input byte corresponds to a pixel in EGAVGA_A000.
//

	.text
	.align	2
	.code	32
	.arm

#include "defines.inc"
#include "macros.inc"

.macro check_write_mode reg
	ldr		\reg, =VGABitMask
	ldrb	\reg, [\reg]
	cmp		\reg, #0					// Zero flag set if bit mask == 0, which works like Write Mode 1 (Hocus Pocus)
	ldrne	\reg,=VGAModeReg
	ldrneb	\reg, [\reg]
	and		\reg, #3
	cmpne	\reg, #1					// Set flags, LT = write mode 0, EQ = write mode 1, GT = write mode 2 or 3
.endm

bad_MODEX_mode1:
	ldr		r0, =BRUnsMODEXMode1
	ldr		r1, =BreakReason
	str		r0, [r1]							// ... tell we break because of an unsupported MODEX opcode
	b		debug_trap_false

bad_MODEX_mode2:
	ldr		r0, =BRUnsMODEXMode2
	ldr		r1, =BreakReason
	str		r0, [r1]							// ... tell we break because of an unsupported MODEX opcode
	b		debug_trap_false

	//-------
	// This is jumped to by handlers that need to pop flags before returning.
	//-------
pop_back:
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown

pop2_back:
	pop		{r0, r1}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown

pop3_back:
	pop		{r0, r1, r2}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown

pop_back_1:
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown_back1

pop2_back_1:
	pop		{r0, r1}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown_back1

pop_back_2:
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown_back2

// ------------------- 00 = ADD r/m8, r8 -------------------------------
// 260009 		  add es:[bx+di],cl (History Line)
//
.macro op_00_MODEX_reg8l reg
	.global op_00_MODEX_l_\reg
op_00_MODEX_l_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r0, r3, lsr r1			// Put the value of the requested plane into lowest byte of r0
	lsl		r0, #24					// Shift it to the highest byte
	//-------
	// ADD the VGA RAM value with the register value
	//-------
	adds	lr, r0, \reg, lsl #24	// lr highest byte = VGA value + reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm
.macro op_00_MODEX_reg8h reg
	.global op_00_MODEX_h_\reg
op_00_MODEX_h_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r0, r3, lsr r1			// Put the value of the requested plane into lowest byte of r0
	//-------
	// ADD the VGA RAM value with the register value
	//-------
	and		lr, \reg, #0xFF00
	lsl		lr, #16
	adds	lr, r0, lsl #24			// lr highest byte = VGA value + reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm


	op_00_MODEX_reg8l eax
	op_00_MODEX_reg8l ecx
	op_00_MODEX_reg8l edx
	op_00_MODEX_reg8l ebx
	op_00_MODEX_reg8h eax
	op_00_MODEX_reg8h ecx
	op_00_MODEX_reg8h edx
	op_00_MODEX_reg8h ebx

	.ltorg

// ------------------- 08 = OR r/m8, r8 --------------------------------
// 26082D          or  es:[di],ch
//
.macro op_08_MODEX_reg8l reg
	.global op_08_MODEX_l_\reg
op_08_MODEX_l_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r0, r3, lsr r1			// Put the value of the requested plane into lowest byte of r0
	lsl		r0, #24					// Shift it to the highest byte
	//-------
	// OR the VGA RAM value with the register value
	//-------
	mov		lr, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	orrs	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm
.macro op_08_MODEX_reg8h reg
	.global op_08_MODEX_h_\reg
op_08_MODEX_h_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r0, r3, lsr r1			// Put the value of the requested plane into lowest byte of r0
	lsl		r0, #24					// Shift it to the highest byte
	//-------
	// OR the VGA RAM value with the register value
	//-------
	and		lr, \reg, #0xFF00
	lsl		lr, #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	orrs	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm

	op_08_MODEX_reg8l eax
	op_08_MODEX_reg8l ecx
	op_08_MODEX_reg8l edx
	op_08_MODEX_reg8l ebx
	op_08_MODEX_reg8h eax
	op_08_MODEX_reg8h ecx
	op_08_MODEX_reg8h edx
	op_08_MODEX_reg8h ebx

	.ltorg

// ------------------- 0A = OR r8,r/m8 -------------------------------
// 260A4403        or   al,es:[si+03]
// 260A842007      or   al,es:[si+0720]
//
.macro op_0a_MODEX_reg8l reg
	.global op_0a_MODEX_l_\reg
op_0a_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	mov		r1, \reg, lsl #24
	lsl		r2, #24
	orrs	r1, r2					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF				// Clear the current reg8l value
	orr		\reg, r1, lsr #24		// Put the result to the register
	b		loop
.endm
.macro op_0a_MODEX_reg8h reg
	.global op_0a_MODEX_h_\reg
op_0a_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r1, \reg,#0xFF00
	lsl		r1, #16
	lsl		r2, #24
	orrs	r1, r2					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF00			// Clear the current reg8h value
	orr		\reg, r1, lsr #16
	b		loop
.endm

	op_0a_MODEX_reg8l eax
	op_0a_MODEX_reg8l ecx
	op_0a_MODEX_reg8l edx
	op_0a_MODEX_reg8l ebx
	op_0a_MODEX_reg8h eax
	op_0a_MODEX_reg8h ecx
	op_0a_MODEX_reg8h edx
	op_0a_MODEX_reg8h ebx

	.ltorg

// ------------------- 20 = AND r/m8,r8 --------------------------------
//
.macro op_20_MODEX_reg8l reg
	.global op_20_MODEX_l_\reg
op_20_MODEX_l_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24					// r1 = 24, 16, 8 or 0
	mov		r0, r3, lsl r1			// Put the value of the requested plane into highest byte of r0
	and		r0, #0xFF000000
	//-------
	// AND the VGA RAM value with the register value
	//-------
	mov		lr, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	ands	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm
.macro op_20_MODEX_reg8h reg
	.global op_20_MODEX_h_\reg
op_20_MODEX_h_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24					// r1 = 24, 16, 8 or 0
	mov		r0, r3, lsl r1			// Put the value of the requested plane into highest byte of r0
	and		r0, #0xFF000000
	//-------
	// AND the VGA RAM value with the register value
	//-------
	mov		lr, \reg, lsl #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	ands	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm

	op_20_MODEX_reg8l eax
	op_20_MODEX_reg8l ecx
	op_20_MODEX_reg8l edx
	op_20_MODEX_reg8l ebx
	op_20_MODEX_reg8h eax
	op_20_MODEX_reg8h ecx
	op_20_MODEX_reg8h edx
	op_20_MODEX_reg8h ebx

	.ltorg

// ------------------- 22 = AND r8,r/m8 --------------------------------
//
.macro op_22_MODEX_reg8l reg
	.global op_22_MODEX_l_\reg
op_22_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	mov		r1,\reg, lsl #24
	lsl		r2, #24
	ands	r1, r2					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF				// Clear the current reg8l value
	orr		\reg, r1, lsr #24		// Put the result to the left register
	b		loop
.endm
.macro op_22_MODEX_reg8h reg
	.global op_22_MODEX_h_\reg
op_22_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	lsl		r2, #24					// Put the byte to and into the highest byte
	mov		r0, \reg, lsl #16
	ands	r2, r0					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF00			// Clear the current reg8h value
	orr		\reg, r2, lsr #16
	b		loop
.endm

	op_22_MODEX_reg8l eax
	op_22_MODEX_reg8l ecx
	op_22_MODEX_reg8l edx
	op_22_MODEX_reg8l ebx
	op_22_MODEX_reg8h eax
	op_22_MODEX_reg8h ecx
	op_22_MODEX_reg8h edx
	op_22_MODEX_reg8h ebx

	.ltorg

// ------------------- 23 = AND r16,r/m16 ------------------------------
//
// x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
// We clear all the flags before the actual ands operation, so C and O work like in x86.
//
// 262315 	and  dx,es:[di]
//
.macro op_23_MODEX_reg reg
	.global op_23_MODEX_\reg
op_23_MODEX_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r0, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	ldr		r2, [r2, #4]			// Get the value from the VGA VRAM
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r0, r1					// Put the value of the requested plane into low byte
	and		r3, r0, #0xFF			// r3 = value of the lower byte from VGA VRAM
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r1, r2, #0xFF
	mov		r2, \reg, lsl #16
	eor		\reg, r2, lsr #16
	orr		r0, r3, r1, lsl #8		// r0 = low byte | (high byte << 8)
	ands	r0, r2, r0, lsl #16
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_23_MODEX_reg eax
	op_23_MODEX_reg ecx
	op_23_MODEX_reg edx
	op_23_MODEX_reg ebx
	op_23_MODEX_reg esp
	op_23_MODEX_reg ebp
	op_23_MODEX_reg esi
	op_23_MODEX_reg edi

	.ltorg
	
// ------------------- 30 = XOR r/m8, r8 -------------------------------
// 26302D          xor  es:[di],ch (LW2)
//
.macro op_30_MODEX_reg8l reg
	.global op_30_MODEX_l_\reg
op_30_MODEX_l_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24					// r1 = 24, 16, 8 or 0
	mov		r0, r3, lsl r1			// Put the value of the requested plane into highest byte of r0
	and		r0, #0xFF000000
	//-------
	// OR the VGA RAM value with the register value
	//-------
	mov		lr, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	eors	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm
.macro op_30_MODEX_reg8h reg
	.global op_30_MODEX_h_\reg
op_30_MODEX_h_\reg:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r0 low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24					// r1 = 24, 16, 8 or 0
	mov		r0, r3, lsl r1			// Put the value of the requested plane into highest byte of r0
	and		r0, #0xFF000000
	//-------
	// OR the VGA RAM value with the register value
	//-------
	and		lr, \reg, #0xFF00
	lsl		lr, #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	eors	lr, r0					// lr highest byte = VGA value OR reg value, flags set as required
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	b		loop
.endm

	op_30_MODEX_reg8l eax
	op_30_MODEX_reg8l ecx
	op_30_MODEX_reg8l edx
	op_30_MODEX_reg8l ebx
	op_30_MODEX_reg8h eax
	op_30_MODEX_reg8h ecx
	op_30_MODEX_reg8h edx
	op_30_MODEX_reg8h ebx

	.ltorg

// ------------------- 32 = XOR r8, r/m8 -----------------------------
//
.macro op_32_MODEX_reg8l reg
	.global op_32_MODEX_l_\reg
op_32_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	mov		r1, \reg, lsl #24
	lsl		r2, #24
	eors	r1, r2					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF				// Clear the current reg8l value
	orr		\reg, r1, lsr #24		// Put the result to the lower byte of the high halfword of the left register
	b		loop
.endm
.macro op_32_MODEX_reg8h reg
	.global op_32_MODEX_h_\reg
op_32_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Store the value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r1, \reg,#0xFF00
	lsl		r1, #16
	lsl		r2, #24
	eors	r1, r2					// Perform the operation using the highest bytes to get the correct flags
	bic		\reg, #0xFF00			// Clear the current reg8h value
	orr		\reg, r1, lsr #16
	b		loop
.endm

	op_32_MODEX_reg8l eax
	op_32_MODEX_reg8l ecx
	op_32_MODEX_reg8l edx
	op_32_MODEX_reg8l ebx
	op_32_MODEX_reg8h eax
	op_32_MODEX_reg8h ecx
	op_32_MODEX_reg8h edx
	op_32_MODEX_reg8h ebx

	.ltorg


// ------------------- 38 = CMP r/m8,r8 --------------------------------
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
// 263801 	cmp  es:[bx+di],al 	(Pinball Dreams)
//
.macro op_38_MODEX_reg8l reg
	.global op_38_MODEX_l_\reg
op_38_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	//-------
	// Perform the CMP
	//-------
	lsl		r2, #24
	cmp		r2, \reg, lsl #24		// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm
.macro op_38_MODEX_reg8h reg
	.global op_38_MODEX_h_\reg
op_38_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	//-------
	// Perform the CMP
	//-------
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	rsbs	r1, r2, lsl #24
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_38_MODEX_reg8l eax
	op_38_MODEX_reg8l ecx
	op_38_MODEX_reg8l edx
	op_38_MODEX_reg8l ebx
	op_38_MODEX_reg8h eax
	op_38_MODEX_reg8h ecx
	op_38_MODEX_reg8h edx
	op_38_MODEX_reg8h ebx

	.ltorg

// ------------------- 39 = CMP r/m16,r16 ------------------------------
// 26397704		cmp es:[bx+04],si			(Aces Over Europe)
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
.macro op_39_MODEX_r2_reg reg
	.global op_39_MODEX_r2_\reg
op_39_MODEX_r2_\reg:
	calc_modex_r2
	ldr		r3, =VGAReadMap
	ldrd	r0, r1, [r2]			// Get the bytes from the VGA VRAM
	ldrb	r3, [r3]
	str		r1, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r3, #3					// r1 = 0, 8, 16 or 24
	lsr		r0, r3					// Put the value of the requested plane into low byte
	lsr		r1, r3					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	orr		r0, r1, lsl #8			// r0 = low byte | (high byte << 8)
	lsl		r0, #16
	cmp		r0, \reg, lsl #16		// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_39_MODEX_r2_reg r4
	op_39_MODEX_r2_reg r5
	op_39_MODEX_r2_reg r6
	op_39_MODEX_r2_reg r7
	op_39_MODEX_r2_reg r8
	op_39_MODEX_r2_reg r9
	op_39_MODEX_r2_reg r10
	op_39_MODEX_r2_reg r11

	.ltorg
	
// ------------------- 3A = CMP r8,r/m8 --------------------------------
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
.macro op_3a_MODEX_reg8l reg
	.global op_3a_MODEX_l_\reg
op_3a_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	//-------
	// Perform the CMP
	//-------
	mov		r1, \reg, lsl #24		// r1 = AL in high byte
	cmp		r1, r2, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm
.macro op_3a_MODEX_reg8h reg
	.global op_3a_MODEX_h_\reg
op_3a_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	//-------
	// Perform the CMP
	//-------
	and		r1, \reg, #0xFF00		// r1 = AH in high byte
	lsl		r1, #16
	cmp		r1, r2, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_3a_MODEX_reg8l eax
	op_3a_MODEX_reg8l ecx
	op_3a_MODEX_reg8l edx
	op_3a_MODEX_reg8l ebx
	op_3a_MODEX_reg8h eax
	op_3a_MODEX_reg8h ecx
	op_3a_MODEX_reg8h edx
	op_3a_MODEX_reg8h ebx

	.ltorg

// ------------------- 3B = CMP r16,r/m16 --------------------------------
// 263B4704		cmp		ax,es:[bx+02]	(Aces Over Europe)
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
.macro op_3b_MODEX_reg16 reg
	.global op_3b_MODEX_\reg
op_3b_MODEX_\reg:
	calc_modex_r2
	ldr		r3, =VGAReadMap
	ldrd	r0, r1, [r2]			// Get the bytes from the VGA VRAM
	ldrb	r3, [r3]
	str		r1, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r3, #3					// r1 = 0, 8, 16 or 24
	lsr		r0, r3					// Put the value of the requested plane into low byte
	lsr		r1, r3					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	mov		r2, \reg, lsl #16
	orr		r0, r1, lsl #8			// r0 = low byte | (high byte << 8)
	cmp		r2, r0, lsl #16
	b		complement_carry
.endm

	op_3b_MODEX_reg16 r4
	op_3b_MODEX_reg16 r5
	op_3b_MODEX_reg16 r6
	op_3b_MODEX_reg16 r7
	op_3b_MODEX_reg16 r8
	op_3b_MODEX_reg16 r9
	op_3b_MODEX_reg16 r10
	op_3b_MODEX_reg16 r11

	.ltorg

// ------------------- 80 = ??? r/m8,imm8 ------------------------------
// 26807E0000 cmp  byte es:[bp],00
// 26803F00   cmp  byte es:[bx],00
// 803E3F0500 cmp  byte [053F],00
// 
	//-------
	// TODO!
	//-------
	.global	op_80_MODEX_add
op_80_MODEX_add:
	.global	op_80_MODEX_adc
op_80_MODEX_adc:
	.global	op_80_MODEX_sub
op_80_MODEX_sub:
	.global	op_80_MODEX_sbb
op_80_MODEX_sbb:
	.global	op_80_MODEX_or
op_80_MODEX_or:
	.global	op_80_MODEX_and
op_80_MODEX_and:
	.global	op_80_MODEX_xor
op_80_MODEX_xor:
	b		.unknown_back1
	
	//-------
	// Cmp: memory location in VGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_MODEX_cmp
op_80_MODEX_cmp:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	lsl		r2, #24					// VRAM value to high byte
	cmp		r2, r1, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

// ------------------- 83 = ??? r/m32,+imm8 ----------------------------
// 
// 6626833F00 	cmp dword es:[bx],00 	(Aces Over Europe)
//

	//-------
	// TODO!
	//-------
	.global	op_83_MODEX_32_add
op_83_MODEX_32_add:
	.global	op_83_MODEX_32_adc
op_83_MODEX_32_adc:
	.global	op_83_MODEX_32_sub
op_83_MODEX_32_sub:
	.global	op_83_MODEX_32_sbb
op_83_MODEX_32_sbb:
	.global	op_83_MODEX_32_or
op_83_MODEX_32_or:
	.global	op_83_MODEX_32_and
op_83_MODEX_32_and:
	.global	op_83_MODEX_32_xor
op_83_MODEX_32_xor:
	b		bad_MODEX_opcode
	
	//-------
	// Cmp: memory location in VGA VRAM.
	// On input:
	//	r2 = physical memory address (with MODEX flags)
	//-------
	.global	op_83_MODEX_32_cmp
op_83_MODEX_32_cmp:
	ldr		r1, =VGAReadMap
	calc_modex_r2
	ldrb	r3, [r1]
	ldrd	r0, r1, [r2]			// Read first two bytes
	lsl		r3, #3					// r3 = 0, 8, 16 or 24
	lsr		r0, r3					// Put the value of the requested plane into low byte
	lsr		r1, r3					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	and		r1, #0xFF
	orr		r0, r1, lsl #8
	mov		r1, r3
	ldrd	r2, r3, [r2, #8]		// Read the last two bytes
	str		r3, [sp, #SP_VGA_LATCH]	// Save the last read value to VGA_latch
	lsr		r2, r1					// Put the value of the requested plane into low byte
	lsr		r3, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	ldrsb	r1, [r12],#1			// r1 = +imm8 value
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	cmp		r0, r1
	b		complement_carry


// ------------------- 84 = TEST r8,r/m8 -------------------------------
//
.macro op_84_MODEX_reg8l reg
	.global op_84_MODEX_l_\reg
op_84_MODEX_l_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	mov		r1, \reg, lsl #24
	lsl		r2, #24
	tst		r1, r2					// Perform the operation using the highest bytes to get the correct flags
	b		loop
.endm
.macro op_84_MODEX_reg8h reg
	.global op_84_MODEX_h_\reg
op_84_MODEX_h_\reg:
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	lsl		r2, #24
	tst		r1, r2					// Perform the operation using the highest bytes to get the correct flags
	b		loop
.endm

	op_84_MODEX_reg8l eax
	op_84_MODEX_reg8l ecx
	op_84_MODEX_reg8l edx
	op_84_MODEX_reg8l ebx
	op_84_MODEX_reg8h eax
	op_84_MODEX_reg8h ecx
	op_84_MODEX_reg8h edx
	op_84_MODEX_reg8h ebx

	.ltorg


// ------------------- 86 = XCHG r/m8,r8 -----------------------------
// 268605          xchg es:[di],al
//
.macro op_86_MODEX_reg8l reg
	.global op_86_MODEX_l_\reg
op_86_MODEX_l_\reg:
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// First read the VGA VRAM value into r3 (full) and r1 (masked)
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r1, r3, r1				// Put the value of the requested plane into r1 low byte
	and		r1, #0xFF
	//-------
	// Make AL = r1, lr = AL original value
	//-------
	and		lr, \reg, #0xFF
	bic		\reg, #0xFF
	orr		\reg, r1
	//-------
	// Check the current VGA Write Mode
	//	r0 = saved flags
	//	r1 = free
	//	r2 = VGA VRAM address
	//	r3 = full value read from VGA VRAM (to be written if write mode = 1)
	//	lr = original AL value in second byte (to be written if write mode = 0)
	//-------
	check_write_mode r1
	//-------
	// Then write either the r3 or lr value into VRAM
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orrlt	lr, lr, lsl #8				// Write mode 0
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	moveq	lr, r3						// Write mode 1
	orrlt	lr, lr, lsl #16				// Write mode 0
	bic		r3, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	str		r3, [r2]
	b		restore_flags_from_r0
.endm
.macro op_86_MODEX_reg8h reg
	.global op_86_MODEX_h_\reg
op_86_MODEX_h_\reg:
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// First read the VGA VRAM value into r3 (full) and r1 (masked)
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r1, r3, r1				// Put the value of the requested plane into r1 low byte
	and		r1, #0xFF
	//-------
	// Make AH = r1, lr = AH original value
	//-------
	and		lr, \reg, #0xFF00
	bic		\reg, #0xFF00
	orr		\reg, r1, lsl #8
	//-------
	// Check the current VGA Write Mode
	//	r0 = saved flags
	//	r1 = free
	//	r2 = VGA VRAM address
	//	r3 = full value read from VGA VRAM (to be written if write mode = 1)
	//	lr = original AH value in second byte (to be written if write mode = 0)
	//-------
	check_write_mode r1
	//-------
	// Then write either the r3 or lr value into VRAM
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orrlt	lr, lr, lsr #8				// Write mode 0
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	moveq	lr, r3						// Write mode 1
	orrlt	lr, lr, lsl #16				// Write mode 0
	bic		r3, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	str		r3, [r2]
	b		restore_flags_from_r0
.endm

	op_86_MODEX_reg8l eax
	op_86_MODEX_reg8l ecx
	op_86_MODEX_reg8l edx
	op_86_MODEX_reg8l ebx

	op_86_MODEX_reg8h eax
	op_86_MODEX_reg8h ecx
	op_86_MODEX_reg8h edx
	op_86_MODEX_reg8h ebx

	.ltorg

// ------------------- 88 = MOV r/m8,r8 --------------------------------
// 26882D		  MOV ES:[DI],CH (LW2)
//
.macro op_88_MODEX_reg8l reg
	.global op_88_MODEX_l_\reg
op_88_MODEX_l_\reg:
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	bgt		bad_MODEX_mode2
	//-------
	// Write the value in register to VRAM
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	ldr		r3, [r2]					// Get current pixels in VGA RAM
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	andlt	lr, \reg, #0xFF				// Write mode 0
	ldreq	lr, [sp, #SP_VGA_LATCH]		// Write mode 1
	orrlt	lr, lr, lsl #8				// Write mode 0
	orrlt	lr, lr, lsl #16				// Write mode 0
	bic		r3, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	str		r3, [r2]
	b		restore_flags_from_r0
.endm
.macro op_88_MODEX_reg8h reg
	.global op_88_MODEX_h_\reg
op_88_MODEX_h_\reg:
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	bgt		bad_MODEX_mode2
	//-------
	// Write the value in register to VRAM
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	ldr		r3, [r2]					// Get current pixels in VGA RAM
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	andlt	lr, \reg, #0xFF00			// Write mode 0
	ldreq	lr, [sp, #SP_VGA_LATCH]		// Write mode 1
	orrlt	lr, lr, lsr #8				// Write mode 0
	orrlt	lr, lr, lsl #16				// Write mode 0
	bic		r3, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	str		r3, [r2]
	b		restore_flags_from_r0
.endm

	op_88_MODEX_reg8l eax
	op_88_MODEX_reg8l ecx
	op_88_MODEX_reg8l edx
	op_88_MODEX_reg8l ebx

	op_88_MODEX_reg8h eax
	op_88_MODEX_reg8h ecx
	op_88_MODEX_reg8h edx
	op_88_MODEX_reg8h ebx

	.ltorg

// ------------------- 89 = MOV r/m16,r16 ------------------------------
// 
.macro op_89_MODEX_reg16 reg
	.global op_89_MODEX_\reg
op_89_MODEX_\reg:
	calc_modex_r2
	mrs		r3,cpsr						// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	beq		.op_89_mode1				// Jump to handle write mode 1
	//-------
	// Write Mode 0
	//-------
	msr		cpsr_f, r3					// Restore flags
	ldmia	r2, {r0, r3}				// Get current pixels in VGA RAM
	and		lr, \reg, #0xFF				// Write mode 0
	orr		lr, lr, lsl #8				// Write mode 0
	orr		lr, lr, lsl #16				// Write mode 0
	bic		r0, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	and		lr, \reg, #0xFF00			// Write mode 0
	orr		lr, lr, lsr #8				// Write mode 0
	orr		lr, lr, lsl #16				// Write mode 0
	bic		r3, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3}				// Save the new pixels to VGA VRAM
	b		loop
.endm

	//-------
	// Write Mode 1, the host byte has no effect so use this common code
	//-------
.op_89_mode1:
	msr		cpsr_f, r3					// Restore flags
	ldr		lr, [sp, #SP_VGA_LATCH]		// Write mode 1
	ldmia	r2, {r0, r3}				// Get current pixels in VGA RAM
	and		lr, r1						// Leave the pixels we are to update
	bic		r0, r1						// Clear the pixels we are to update
	bic		r3, r1						// Clear the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	orr		r3, lr						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3}				// Save the new pixels to VGA VRAM
	b		loop

	op_89_MODEX_reg16 eax
	op_89_MODEX_reg16 ecx
	op_89_MODEX_reg16 edx
	op_89_MODEX_reg16 ebx
	op_89_MODEX_reg16 esp
	op_89_MODEX_reg16 ebp
	op_89_MODEX_reg16 esi
	op_89_MODEX_reg16 edi

	.ltorg

// ------------------- 89 = MOV r/m32,r32 ------------------------------
// 
.macro op_89_MODEX_reg32 reg
	.global op_89_MODEX_USE32_\reg
op_89_MODEX_USE32_\reg:
	calc_modex_r2
	mrs		r3,cpsr						// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	beq		.op_89_mode1_USE32			// Jump to handle write mode 1
	//-------
	// Write Mode 0
	//-------
	msr		cpsr_f, r3					// Restore flags
	ldr		r0, [r2]					// Get current pixels in VGA RAM
	and		r3, \reg, #0xFF				// Write mode 0
	orr		r3, r3, lsl #8				// Write mode 0
	orr		r3, r3, lsl #16				// Write mode 0
	bic		r0, r1						// Clear the pixels we are to update
	and		r3, r1						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2]
	ldr		r0, [r2, #4]				// Get current pixels in VGA RAM
	and		r3, \reg, #0xFF00			// Write mode 0
	orr		r3, r3, lsr #8				// Write mode 0
	orr		r3, r3, lsl #16				// Write mode 0
	bic		r0, r1						// Clear the pixels we are to update
	and		r3, r1						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2, #4]
	ldr		r0, [r2, #8]				// Get current pixels in VGA RAM
	and		r3, \reg, #0xFF0000			// Write mode 0
	orr		r3, r3, lsl #8				// Write mode 0
	orr		r3, r3, lsr #16				// Write mode 0
	bic		r0, r1						// Clear the pixels we are to update
	and		r3, r1						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2, #8]
	ldr		r0, [r2, #12]				// Get current pixels in VGA RAM
	and		r3, \reg, #0xFF000000		// Write mode 0
	orr		r3, r3, lsr #8				// Write mode 0
	orr		r3, r3, lsr #16				// Write mode 0
	bic		r0, r1						// Clear the pixels we are to update
	and		r3, r1						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2, #12]
	b		loop
.endm

	//-------
	// Write Mode 1, the host byte has no effect so use this common code
	//-------
.op_89_mode1_USE32:
	msr		cpsr_f, r3					// Restore flags
	ldr		lr, [sp, #SP_VGA_LATCH]		// Write mode 1, get VGA latch value into lr
	push	{r4, r5}
	ldmia	r2, {r0, r3, r4, r5}		// Get 4 pixels from VGA VRAM
	and		lr, r1						// Leave the pixels we are to update
	bic		r0, r1						// Clear the pixels we are to update
	bic		r3, r1						// Clear the pixels we are to update
	bic		r4, r1						// Clear the pixels we are to update
	bic		r5, r1						// Clear the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	orr		r3, lr						// Replace the original pixels with the new pixels
	orr		r4, lr						// Replace the original pixels with the new pixels
	orr		r5, lr						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3, r4, r5}		// Write the new pixels to VGA VRAM
	pop		{r4, r5}
	b		loop

	op_89_MODEX_reg32 eax
	op_89_MODEX_reg32 ecx
	op_89_MODEX_reg32 edx
	op_89_MODEX_reg32 ebx
	op_89_MODEX_reg32 esp
	op_89_MODEX_reg32 ebp
	op_89_MODEX_reg32 esi
	op_89_MODEX_reg32 edi

	.ltorg

// ------------------- 8A = MOV r8,r/m8 --------------------------------
// 268A05 = MOV AL,ES:[DI] (LW2)
//
.macro op_8a_MODEX_reg8l reg
	.global op_8a_MODEX_l_\reg
op_8a_MODEX_l_\reg:
	calc_modex_r2
	//-------
	// Get the byte from VRAM
	//-------
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	//-------
	// Move it to the register
	//-------
	bic		\reg, #0xFF
	orr		\reg, r2				// Put the VGA VRAM byte into the register
	b		loop
.endm
.macro op_8a_MODEX_reg8h reg
	.global op_8a_MODEX_h_\reg
op_8a_MODEX_h_\reg:
	calc_modex_r2
	//-------
	// Get the byte from VRAM
	//-------
	ldr		r1, =VGAReadMap
	ldr		r2, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	//-------
	// Move it to the register
	//-------
	and		r2, #0xFF
	bic		\reg, #0xFF00
	orr		\reg, r2, lsl #8		// Put the VGA VRAM byte into the register
	b		loop
.endm

	op_8a_MODEX_reg8l eax
	op_8a_MODEX_reg8l ecx
	op_8a_MODEX_reg8l edx
	op_8a_MODEX_reg8l ebx

	op_8a_MODEX_reg8h eax
	op_8a_MODEX_reg8h ecx
	op_8a_MODEX_reg8h edx
	op_8a_MODEX_reg8h ebx

	.ltorg

// ------------------- 8B = MOV r16,r/m16 ------------------------------
// 
//
.macro op_8b_MODEX_reg16 reg
	.global op_8b_MODEX_\reg
op_8b_MODEX_\reg:
	calc_modex_r2
	//-------
	// Get the halfword from VRAM
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the low byte value from the VGA VRAM
	ldrb	r1, [r1]
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r3, r1					// Put the value of the requested plane into low byte
	ldr		r2, [r2, #4]			// Get the high byte value from the VGA VRAM
	and		r3, #0xFF
	lsr		\reg, #16
	orr		\reg, r3, \reg, lsl #16
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA latch
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	orr		\reg, r2, lsl #8
	b		loop
.endm

	op_8b_MODEX_reg16 eax
	op_8b_MODEX_reg16 ecx
	op_8b_MODEX_reg16 edx
	op_8b_MODEX_reg16 ebx
	op_8b_MODEX_reg16 esp
	op_8b_MODEX_reg16 ebp
	op_8b_MODEX_reg16 esi
	op_8b_MODEX_reg16 edi

	.ltorg

// ------------------- 8B = MOV r32,r/m32 ------------------------------
// 
//
.macro op_8b_MODEX_reg32 reg
	.global op_8b_MODEX_USE32_\reg
op_8b_MODEX_USE32_\reg:
	calc_modex_r2
	//-------
	// Get the word from VRAM
	//-------
	ldr		r1, =VGAReadMap
	ldmia	r2, {r2, r3, \reg, lr}	// Read 4 words = 4 bytes from ModeX VRAM
	ldrb	r1, [r1]
	str		lr, [sp, #SP_VGA_LATCH]	// Save the last read value to VGA latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r2, r1					// Put the value of the requested plane into low byte
	lsr		r3, r1					// Put the value of the requested plane into low byte
	lsr		\reg, r1				// Put the value of the requested plane into low byte
	lsr		lr, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	and		r3, #0xFF
	and		\reg, #0xFF
	and		lr, #0xFF
	orr		\reg, r2, \reg, lsl #16
	orr		\reg, r3, lsl #8
	orr		\reg, lr, lsl #24
	b		loop
.endm

	op_8b_MODEX_reg32 eax
	op_8b_MODEX_reg32 ecx
	op_8b_MODEX_reg32 edx
	op_8b_MODEX_reg32 ebx
	op_8b_MODEX_reg32 esp
	op_8b_MODEX_reg32 ebp
	op_8b_MODEX_reg32 esi
	op_8b_MODEX_reg32 edi

	.ltorg
		
// ------------------- A4 = MOVSB --------------------------------------
//
// Move a byte from DS:SI to ES:DI
//
	.global	op_a4_from_MODEX
op_a4_from_MODEX:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value (-1<<16 or 1<<16)
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0x0000FFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	mov		lr, r1					// Save EDI inc/dec value to lr register
	calc_modex_r2
	//-------
	// Get the byte from DS:[SI]
	//-------
	ldr		r1, =VGAReadMap
	ldr		r0, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r0, [sp, #SP_VGA_LATCH]	// Save it to VGA latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r0, r1					// Put the value of the requested plane into low byte
	and		r1, r0, #0xFF			// r1 = the byte read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	mov		r0, edi					// r0 = DI
	add		edi, lr, edi, ror #16	// Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES op_a4_to_RAM .unknown_back1 op_a4_to_MODEX

	.global	op_a4_from_MODEX_USE32
op_a4_from_MODEX_USE32:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value (-1 or 1)
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0xFFFFFFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	mov		lr, r1					// Save EDI inc/dec value to lr register
	calc_modex_r2
	//-------
	// Get the byte from DS:[SI]
	//-------
	ldr		r1, =VGAReadMap
	ldr		r0, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r0, [sp, #SP_VGA_LATCH]	// Save it to VGA latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	lsr		r0, r1					// Put the value of the requested plane into low byte
	and		r1, r0, #0xFF			// r1 = the byte read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	mov		r0, edi					// r0 = DI
	add		edi, lr					// Fix logical DI.
	mem_handler_jump_r0r3_ES op_a4_to_RAM .unknown_back1 op_a4_to_MODEX

	//-------
	// On input:
	//	r1 = byte read from DS:SI
	//	r2 = ES:DI linear address (with EGA flag)
	//	r0, r3, lr = free
	//-------
	.global	op_a4_to_MODEX
op_a4_to_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r3
	bgt		bad_MODEX_mode2
	//-------
	// Write the byte in r1 low byte into ES:DI
	//-------
	ldr		r3, [r2]				// Get current pixels in VGA RAM
	movlt	lr, r1, lsl #24			// Write mode 0
	ldr		r1, =MODEX_WRITE_MASK32
	ldreq	lr, [sp, #SP_VGA_LATCH]	// Write mode 1
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orrlt	lr, lr, lsr #8			// Write mode 0
	orrlt	lr, lr, lsr #16			// Write mode 0
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	str		r1, [r2]
	b		restore_flags_from_r0

// ------------------- A5 = MOVSW --------------------------------------
//
// Move a word from DS:SI to ES:DI
//
	.global	op_a5_from_MODEX
op_a5_from_MODEX:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0x0000FFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	mov		lr, r1					// Save EDI inc/dec value to lr register
	calc_modex_r2
	//-------
	// Get the values from VGA VRAM (DS:[SI]), and also store them to VGALatch
	// Get the read map shift value
	//-------
	ldr		r0, [r2]				// Get the value from the VGA VRAM
	ldr		r1, =VGAReadMap
	ldr		r2, [r2, #4]			// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r0, [sp, #SP_VGA_LATCH_1]	// Save it to VGA_latch minus 4
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	//-------
	// Make r1 = the halfword read
	//-------
	lsr		r0, r1					// Put the value of the requested plane into low byte
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	and		r1, r0, #0xFF
	orr		r1, r2, lsl #8			// r1 = halfword read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	mov		r0, edi					// r0 = DI
	add		edi, lr, edi, ror #16	// Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES op_a5_to_RAM .unknown_back1 op_a5_to_MODEX

	.global	op_a5_from_MODEX_USE32
op_a5_from_MODEX_USE32:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0xFFFFFFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	mov		lr, r1					// Save EDI inc/dec value to lr register
	calc_modex_r2
	//-------
	// Get the values from VGA VRAM (DS:[SI]), and also store them to VGALatch
	// Get the read map shift value
	//-------
	ldr		r0, [r2]				// Get the value from the VGA VRAM
	ldr		r2, [r2, #4]			// Get the value from the VGA VRAM
	ldr		r1, =VGAReadMap
	str		r0, [sp, #SP_VGA_LATCH_1] // Save it to VGA_latch_minus_4
	ldrb	r1, [r1]
	str		r2, [sp, #SP_VGA_LATCH]	// Save it to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	//-------
	// Make r1 = the halfword read
	//-------
	lsr		r0, r1					// Put the value of the requested plane into low byte
	lsr		r2, r1					// Put the value of the requested plane into low byte
	and		r2, #0xFF
	and		r1, r0, #0xFF
	orr		r1, r2, lsl #8			// r1 = halfword read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	mov		r0, edi					// r0 = DI
	add		edi, lr					// Fix logical EDI.
	mem_handler_jump_r0r3_ES op_a5_to_RAM .unknown_back1 op_a5_to_MODEX

	//-------
	// On input:
	//	r1 = halfword read from DS:SI
	//	r2 = ES:DI linear address (with Mode-X flag)
	//	r0, r3, lr = free
	//-------
	.global	op_a5_to_MODEX
op_a5_to_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r3
	bgt		bad_MODEX_mode2
	beq		.op_a5_mode1
	msr		cpsr_f, r0				// Restore flags
	//-------
	// Write Mode 0
	//	r1 = halfword to write
	//	r2 = VRAM address
	//	r0, r3, lr = free
	//-------
	ldr		lr, =MODEX_WRITE_MASK32
	//-------
	// Write the low byte into ES:DI
	//-------
	ldr		r0, [r2]					// Get current pixels in VGA RAM
	ldr		lr, [lr]					// r1 = mask of the pixels to change
	mov		r3, r1, lsl #24				// Write mode 0
	orr		r3, r3, lsr #8				// Write mode 0
	orr		r3, r3, lsr #16				// Write mode 0
	bic		r0, lr						// Clear the pixels we are to update
	and		r3, lr						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2]
	//-------
	// Write the high byte into ES:DI+1
	//-------
	ldr		r0, [r2, #4]				// Get current pixels in VGA RAM
	and		r3, r1, #0xFF00				// Write mode 0
	orr		r3, r3, lsr #8				// Write mode 0
	orr		r3, r3, lsl #16				// Write mode 0
	bic		r0, lr						// Clear the pixels we are to update
	and		r3, lr						// Leave the pixels we are to update
	orr		r0, r3						// Replace the original pixels with the new pixels
	str		r0, [r2, #4]
	b		loop						// Back to loop
.op_a5_mode1:
	msr		cpsr_f, r0					// Restore flags
	//-------
	// Write Mode 1
	//	r2 = VRAM address
	//	r0, r1, r3, lr = free
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	//-------
	// Write the low byte into ES:DI
	//-------
	ldr		r0, [r2]					// Get current pixels in VGA RAM
	ldr		lr, [sp, #SP_VGA_LATCH_1]	// Write mode 1
	bic		r0, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	str		r0, [r2]
	//-------
	// Write the high byte into ES:DI+1
	//-------
	ldr		r0, [r2, #4]				// Get current pixels in VGA RAM
	ldr		lr, [sp, #SP_VGA_LATCH]		// Write mode 1
	bic		r0, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	str		r0, [r2, #4]
	b		loop						// Back to loop

	.global	movsd_from_MODEX
movsd_from_MODEX:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0x0000FFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	ldr		lr, =SP_VGA_LATCH_3
	add		lr, sp
	push	{r1}					// Save EDI inc/dec value to stack
	calc_modex_r2
	//-------
	// Get the values from VGA VRAM (DS:[SI]), and also store them to VGALatch
	//-------
	ldmia	r2, {r0, r1, r2, r3}	// Read 4 words from the VGA RAM
	stmia	lr, {r0, r1, r2, r3}	// Save the read values to VGA latch
	//-------
	// Get the read map shift value
	//-------
	ldr		lr, =VGAReadMap
	ldrb	lr, [lr]
	lsl		lr, #3					// r1 = 0, 8, 16 or 24
	//-------
	// Make r1 = the halfword read
	//-------
	lsr		r0, lr					// Put the value of the requested plane into low byte
	lsr		r1, lr					// Put the value of the requested plane into low byte
	lsr		r2, lr					// Put the value of the requested plane into low byte
	lsr		r3, lr					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	and		r1, #0xFF
	and		r2, #0xFF
	orr		r1, r0, r1, lsl #8		// r1 = halfword read from DS:SI
	orr		r1, r2, lsl #16
	orr		r1, r3, lsl #24			// r1 = word read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	pop		{lr}					// Restore EDI inc/dec value from stack
	ldr		r3, [sp, #SP_MASK_16]	// Use 16-bit memory addressing
	mov		r0, edi					// r0 = DI
	add		edi, lr, edi, ror #16	// Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES movsd_to_RAM .unknown_back1 movsd_to_MODEX

	.global	movsd_from_MODEX_USE32
movsd_from_MODEX_USE32:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with MODEX flag)
	//	r3 = Address size mask (0xFFFFFFFF)
	// First calculate the physical VRAM input address to r2
	//-------
	ldr		lr, =SP_VGA_LATCH_3
	add		lr, sp
	push	{r1}					// Save EDI inc/dec value to stack
	calc_modex_r2
	//-------
	// Get the values from VGA VRAM (DS:[SI]), and also store them to VGALatch
	//-------
	ldmia	r2, {r0, r1, r2, r3}	// Read 4 words from the VGA RAM
	stmia	lr, {r0, r1, r2, r3}	// Save the read values to VGA latch
	//-------
	// Get the read map shift value
	//-------
	ldr		lr, =VGAReadMap
	ldrb	lr, [lr]
	lsl		lr, #3					// r1 = 0, 8, 16 or 24
	//-------
	// Make r1 = the halfword read
	//-------
	lsr		r0, lr					// Put the value of the requested plane into low byte
	lsr		r1, lr					// Put the value of the requested plane into low byte
	lsr		r2, lr					// Put the value of the requested plane into low byte
	lsr		r3, lr					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	and		r1, #0xFF
	and		r2, #0xFF
	orr		r1, r0, r1, lsl #8		// r1 = halfword read from DS:SI
	orr		r1, r2, lsl #16
	orr		r1, r3, lsl #24			// r1 = word read from DS:SI
	//-------
	// Get the ES:DI address.
	//-------
	pop		{lr}					// Restore EDI inc/dec value from stack
	mvn		r3, #0					// Use 32-bit memory addressing
	mov		r0, edi					// r0 = EDI
	add		edi, lr					// Fix logical EDI.
	mem_handler_jump_r0r3_ES movsd_to_RAM .unknown_back1 movsd_to_MODEX

	//-------
	// On input:
	//	r1 = word read from DS:SI
	//	r2 = ES:DI linear address (with Mode-X flag)
	//	r0, r3, lr = free
	//-------
	.global	movsd_to_MODEX
movsd_to_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r3
	bgt		bad_MODEX_mode2
	beq		.movsd_mode1
	//-------
	// Write Mode 0
	//	r1 = word to write
	//	r2 = VRAM address
	//	r0, r3, lr = free
	//-------
	ldr		lr, =MODEX_WRITE_MASK32
	push	{r0, r4, r5, r6}
	ldr		lr, [lr]					// r4 = mask of the pixels to change
	ldmia	r2, {r0, r3, r4, r5}		// Load 4 words from VRAM
	mov		r6, r1, lsl #24				// Write mode 0
	orr		r6, r6, lsr #8				// Write mode 0
	orr		r6, r6, lsr #16				// Write mode 0
	bic		r0, lr						// Clear the pixels we are to update
	and		r6, lr						// Leave the pixels we are to update
	orr		r0, r6						// Replace the original pixels with the new pixels
	and		r6, r1, #0xFF00				// Write mode 0
	orr		r6, r6, lsr #8				// Write mode 0
	orr		r6, r6, lsl #16				// Write mode 0
	bic		r3, lr						// Clear the pixels we are to update
	and		r6, lr						// Leave the pixels we are to update
	orr		r3, r6						// Replace the original pixels with the new pixels
	and		r6, r1, #0xFF0000			// Write mode 0
	orr		r6, r6, lsl #8				// Write mode 0
	orr		r6, r6, lsr #16				// Write mode 0
	bic		r4, lr						// Clear the pixels we are to update
	and		r6, lr						// Leave the pixels we are to update
	orr		r4, r6						// Replace the original pixels with the new pixels
	mov		r6, r1, lsr #24				// Write mode 0
	orr		r6, r6, lsl #8				// Write mode 0
	orr		r6, r6, lsl #16				// Write mode 0
	bic		r5, lr						// Clear the pixels we are to update
	and		r6, lr						// Leave the pixels we are to update
	orr		r5, r6						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3, r4, r5}		// Write the 4 words back to VRAM
	pop		{r0, r4, r5, r6}
	b		restore_flags_from_r0		// Back to loop
.movsd_mode1:
	//-------
	// Write Mode 1
	//	r0 = saved flags
	//	r2 = VRAM address
	//	r1, r3, lr = free
	//-------
	push	{r0, r4, r5, r6, r7, r8}
	ldr		lr, =MODEX_WRITE_MASK32
	add		r0, sp, #SP_VGA_LATCH_3+(6*4) // Write mode 1, address of the lowest of VGA latch stack values
	ldr		lr, [lr]					// lr = mask of the pixels to change
	ldmia	r0, {r5, r6, r7, r8}		// Load 4 words from VGALatch
	ldmia	r2, {r0, r1, r3, r4}		// Load 4 words from VRAM
	bic		r0, lr						// Clear the pixels we are to update
	and		r5, lr						// Leave the pixels we are to update
	orr		r0, r5						// Replace the original pixels with the new pixels
	bic		r1, lr						// Clear the pixels we are to update
	and		r6, lr						// Leave the pixels we are to update
	orr		r1, r6						// Replace the original pixels with the new pixels
	bic		r3, lr						// Clear the pixels we are to update
	and		r7, lr						// Leave the pixels we are to update
	orr		r3, r7						// Replace the original pixels with the new pixels
	bic		r4, lr						// Clear the pixels we are to update
	and		r8, lr						// Leave the pixels we are to update
	orr		r4, r8						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r1, r3, r4}		// Save 4 words to VRAM
	pop		{r0, r4, r5, r6, r7, r8}
	b		restore_flags_from_r0		// Back to loop

// ------------------- AA = STOSB --------------------------------------
// Segment override does not affect STOSB, it always uses ES:DI
//	r2 = linear ES:DI with MODEX flags
//	r0, r1, r3, lr = free
//
	.global	stosb_MODEX
stosb_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r3
	bgt		bad_MODEX_mode2
	//-------
	// The rest of the code is similar to MOV ES:[DI],AL
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	ldr		r3, [r2]				// Get current pixels in VGA RAM
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	movlt	lr, eax, lsl #24		// Write mode 0
	ldreq	lr, [sp, #SP_VGA_LATCH]	// Write mode 1
	orrlt	lr, lr, lsr #8			// Write mode 0
	orrlt	lr, lr, lsr #16			// Write mode 0
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	str		r1, [r2]
	b		restore_flags_from_r0

// ------------------- AB = STOSW --------------------------------------
// Segment override does not affect STOSW, it always uses ES:DI
//	r2 = linear ES:DI with MODEX flags
//	r0, r1, r3, lr = free
//
	.global	stosw_MODEX
stosw_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	beq		.stosw_mode1
	//-------
	// Mode 0 handling
	//-------
	msr		cpsr_f, r0				// Restore flags
	ldr		r0, [r2]				// Get current pixels in VGA RAM
	ldr		r3, [r2, #4]			// Get current pixels in VGA RAM
	bic		r0, r1					// Clear the pixels we are to update
	bic		r3, r1					// Clear the pixels we are to update
	mov		lr, eax, lsl #24
	orr		lr, lr, lsr #8			// Write mode 0
	orr		lr, lr, lsr #16			// Write mode 0
	and		lr, r1					// Leave the pixels we are to update
	orr		r0, lr					// Replace the original pixels with the new pixels
	mov		lr, eax, lsr #8			// Write mode 0
	and		lr, #0xFF
	orr		lr, lr, lsl #8			// Write mode 0
	orr		lr, lr, lsl #16			// Write mode 0
	and		lr, r1					// Leave the pixels we are to update
	orr		r3, lr					// Replace the original pixels with the new pixels
	str		r0, [r2]
	str		r3, [r2, #4]
	b		loop
	//-------
	// Mode 1 handling
	//-------
.op_c7_mode1:
	add		r12, #2					// Skip the imm16 value, not needed in write mode 1
.stosw_mode1:
	msr		cpsr_f, r0				// Restore flags
	ldr		lr, [sp, #SP_VGA_LATCH]	// Write mode 1
	ldr		r0, [r2]				// Get current pixels in VGA RAM
	ldr		r3, [r2, #4]			// Get current pixels in VGA RAM
	and		lr, r1					// Leave the pixels we are to update
	bic		r0, r1					// Clear the pixels we are to update
	bic		r3, r1					// Clear the pixels we are to update
	orr		r0, lr					// Replace the original pixels with the new pixels
	orr		r3, lr					// Replace the original pixels with the new pixels
	str		r0, [r2]				// Write the output pixels to VGA RAM
	str		r3, [r2, #4]			// Write the output pixels to VGA RAM
	b		loop

	.global	stosd_MODEX
stosd_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	push	{r0, r5, r6}
	beq		.stosd_mode1
	//-------
	// Mode 0 handling
	//-------
	ldmia	r2, {r0, r3, r5, r6}		// Load 4 words from VRAM
	bic		r0, r1						// Clear the pixels we are to update
	bic		r3, r1						// Clear the pixels we are to update
	bic		r5, r1						// Clear the pixels we are to update
	bic		r6, r1						// Clear the pixels we are to update
	//
	and		lr, eax, #0xFF
	orr		lr, lr, lsl #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	//
	and		lr, eax, #0xFF00
	orr		lr, lr, lsr #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	//
	and		lr, eax, #0xFF0000
	orr		lr, lr, lsl #8
	orr		lr, lr, lsr #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r5, lr						// Replace the original pixels with the new pixels
	//
	and		lr, eax, #0xFF000000
	orr		lr, lr, lsr #8
	orr		lr, lr, lsr #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r6, lr						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3, r5, r6}		// Save 4 words to VRAM
	pop		{r0, r5, r6}
	b		restore_flags_from_r0
	//-------
	// Mode 1 handling, registers r0, r4, r5 pushed to stack!
	//-------
.op_c7_mode1_USE32:
	add		r12, #4						// Skip the immediate word, not needed in write mode 1
.stosd_mode1:
	ldmia	r2, {r0, r3, r5, r6}		// Load 4 words from VRAM
	ldr		lr, [sp, #SP_VGA_LATCH+(3*4)] // Write mode 1
	bic		r0, r1						// Clear the pixels we are to update
	bic		r3, r1						// Clear the pixels we are to update
	bic		r5, r1						// Clear the pixels we are to update
	bic		r6, r1						// Clear the pixels we are to update
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	orr		r3, lr						// Replace the original pixels with the new pixels
	orr		r5, lr						// Replace the original pixels with the new pixels
	orr		r6, lr						// Replace the original pixels with the new pixels
	stmia	r2, {r0, r3, r5, r6}		// Save 4 words to VRAM
	pop		{r0, r5, r6}
	b		restore_flags_from_r0

	.ltorg
	
// ------------------- AC = LODSB --------------------------------------
//
// We may not change the flags!
//
	.global	lodsb_MODEX
lodsb_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	//-------
	// Get the byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r0, [r2]					// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r0, [sp, #SP_VGA_LATCH]		// Save the last read value to VGA_latch
	lsl		r1, #3						// r1 = 0, 8, 16 or 24
	lsr		r0, r1						// Put the value of the requested plane into low byte
	and		r0, #0xFF
	//-------
	// Put the result into AL (r4)
	//-------
	bic		eax, #0xFF
	orr		eax, r0
	b		loop

// ------------------- AD = LODSW --------------------------------------
//
// We may not change the flags!
// Result will be in r4 = AX, so we can freely modify it until we load it with the new value.
//
	.global	lodsw_MODEX
lodsw_MODEX:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	//-------
	// Get low byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]					// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	lsl		r1, #3						// r1 = 0, 8, 16 or 24
	lsr		r3, r1						// Put the value of the requested plane into low byte
	and		r3, #0xFF
	//-------
	// Get high byte
	//-------
	ldr		r0, [r2, #4]				// Get the value from the VGA VRAM
	lsr		eax, #16
	str		r0, [sp, #SP_VGA_LATCH]		// Save the last read value to VGA_latch
	lsr		r0, r1						// Put the value of the requested plane into low byte
	and		r0, #0xFF
	//-------
	// Put the result into AX (r4)
	//-------
	orr		r3, r0, lsl #8
	orr		eax, r3, eax, lsl #16
	b		loop

	.global	lodsd_MODEX
lodsd_MODEX:
	//-------
	// Get the bytes from VGA RAM
	//-------
	ldr		r3, =VGAReadMap
	calc_modex_r2
	ldrb	r3, [r3]
	ldmia	r2, {r0, r1, r2, eax}		// Get the values from the VGA VRAM
	lsl		r3, #3						// r3 = 0, 8, 16 or 24
	str		eax, [sp, #SP_VGA_LATCH]	// Save the last read value to VGA_latch
	lsr		r0, r3						// Put the value of the requested plane into low byte
	lsr		r1, r3						// Put the value of the requested plane into low byte
	lsr		r2, r3						// Put the value of the requested plane into low byte
	lsr		eax, r3						// Put the value of the requested plane into low byte
	//-------
	// Build the result into EAX and return.
	//-------
	and		r0, #0xFF
	orr		eax, r0, eax, lsl #24
	and		r1, #0xFF
	and		r2, #0xFF
	orr		eax, r1, lsl #8
	orr		eax, r2, lsl #16
	b		loop

// ------------------- C4 = LES r16,m16:16 -----------------------------
//
// 26C41F	les bx,es:[bx]		(Aces Over Europe)
//
.macro les_MODEX_reg16 reg
	.global les_MODEX_r2_\reg
les_MODEX_r2_\reg:
	ldr		r1, =VGAReadMap
	calc_modex_r2
	ldrb	r3, [r1]
	// ----- Load offset
	ldrd	r0, r1, [r2]
	lsl		r3, #3					// r3 = 0, 8, 16 or 24
	lsr		r0, r3					// Put the value of the requested plane into low byte
	lsr		r1, r3					// Put the value of the requested plane into low byte
	and		r0, #0xFF
	lsr		\reg, #16
	orr		\reg, r1, lsl #24
	// ----- Load segment
	mov		r1, r3
	ldrd	r2, r3, [r2, #8]
	orr		\reg, r0, \reg, ror #16
	str		r3, [sp, #SP_VGA_LATCH]	// Save the last loaded value to VGA latch
	lsr		r2, r1
	lsr		r3, r1
	and		r2, #0xFF
	and		r3, #0xFF
	orr		r2, r3, lsl #8			// r2 = new ES value
	b		les_r2_cont_\reg		// Continue with code in cpu.S
.endm

	les_MODEX_reg16 eax
	les_MODEX_reg16 ecx
	les_MODEX_reg16 edx
	les_MODEX_reg16 ebx
	les_MODEX_reg16 esp
	les_MODEX_reg16 ebp
	les_MODEX_reg16 esi
	les_MODEX_reg16 edi

	.ltorg

	
// ------------------- C6 = MOV r/m8, imm8 -----------------------------
//
	.global	op_c6_MODEX_r2
op_c6_MODEX_r2:
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldrb	lr, [r12],#1			// Write mode 0: Load the imm8 byte to r2, increment r12 by 1
	ldr		r3, [r2]				// Get current pixels in VGA RAM
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orrlt	lr, lr, lsl #8			// Write mode 0
	orrlt	lr, lr, lsl #16			// Write mode 0
	ldreq	lr, [sp, #SP_VGA_LATCH]	// Write mode 1: Use VGA latch value
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r3, lr					// Replace the original pixels with the new pixels
	str		r3, [r2]
	b		restore_flags_from_r0

// ------------------- C7 = MOV r/m16, imm16 ---------------------------
// 
	.global	op_c7_MODEX_r2
op_c7_MODEX_r2:
	calc_modex_r2
	mrs		r0,cpsr						// Save flags (we are not allowed to change any)
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]					// r1 = mask of the pixels to change
	beq		.op_c7_mode1				// Jump if write mode 1
	//-------
	// Write Mode 0 handling
	//-------
	msr		cpsr_f, r0					// Restore flags
	ldr		r0, [r2]					// Get current pixels in VGA RAM
	ldr		r3, [r2, #4]				// Get current pixels in VGA RAM
	ldrb	lr, [r12],#1				// Get low byte of imm16 value
	bic		r0, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8				// Write mode 0
	orr		lr, lr, lsl #16				// Write mode 0
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	ldrb	lr, [r12],#1				// Get low byte of imm16 value
	bic		r3, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8				// Write mode 0
	orr		lr, lr, lsl #16				// Write mode 0
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	str		r0, [r2]
	str		r3, [r2, #4]
	b		loop

	.global	op_c7_MODEX_USE32
op_c7_MODEX_USE32:
	//-------
	// First calculate the physical address to r2
	//-------
	calc_modex_r2
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	//-------
	// Check the current VGA Write Mode
	//-------
	check_write_mode r1
	ldr		r1, =MODEX_WRITE_MASK32
	bgt		bad_MODEX_mode2
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	push	{r0, r5, r6}
	beq		.op_c7_mode1_USE32
	//-------
	// Mode 0 handling
	//-------
	ldmia	r2, {r0, r3, r5, r6}		// Load 4 words from VRAM
	//
	ldrb	lr, [r12],#1				// Get low byte of imm32 value
	bic		r0, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r0, lr						// Replace the original pixels with the new pixels
	//
	ldrb	lr, [r12],#1				// Get byte of imm32 value
	bic		r3, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r3, lr						// Replace the original pixels with the new pixels
	//
	ldrb	lr, [r12],#1				// Get byte of imm32 value
	bic		r5, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r5, lr						// Replace the original pixels with the new pixels
	//
	ldrb	lr, [r12],#1				// Get high byte of imm32 value
	bic		r6, r1						// Clear the pixels we are to update
	orr		lr, lr, lsl #8
	orr		lr, lr, lsl #16
	and		lr, r1						// Leave the pixels we are to update
	orr		r6, lr						// Replace the original pixels with the new pixels
	//
	stmia	r2, {r0, r3, r5, r6}		// Save 4 words to VRAM
	pop		{r0, r5, r6}
	b		restore_flags_from_r0


// ------------------- F6 = ??? r/m8 -----------------------------------
//
	.global	test_MODEX_r2_imm8
test_MODEX_r2_imm8:
	calc_modex_r2
	//-------
	// Get the value from VGA VRAM
	//-------
	ldr		r0, [r2]
	ldr		r2, =VGAReadMap
	str		r0, [sp, #SP_VGA_LATCH]		// Store the last value read to VGA_latch
	ldrb	r2, [r2]
	msr		cpsr_f,#0					// Clear all flags (especially C and O)
	lsl		r2, #3						// r2 = 0, 8, 16 or 24
	lsr		r0, r2						// Put the value of the requested plane into low byte
	//-------
	// Get the imm8 value
	//-------
	ldrb	r1, [r12], #1
	//-------
	// Perform the test
	//-------
	lsl		r0, #24
	lsl		r1, #24
	tst		r0, r1
	b		loop

// ------------------- F7 = ??? r/m16 ----------------------------------
// 26F7074000      test word es:[bx],0040
//
	.global	test_MODEX_r2_imm16
test_MODEX_r2_imm16:
	calc_modex_r2
	//-------
	// Get the value from VGA VRAM
	//-------
	ldrd	r0, r1, [r2]				// r0 = low byte, r1 = high byte
	ldr		r2, =VGAReadMap
	str		r1, [sp, #SP_VGA_LATCH]		// Store the last value read to VGA_latch
	msr		cpsr_f,#0					// Clear all flags (especially C and O)
	ldrb	r2, [r2]
	lsl		r2, #3						// r2 = 0, 8, 16 or 24
	lsr		r0, r2						// Put the value of the requested plane into low byte
	and		r0, #0xFF
	lsr		r1, r2						// Put the value of the requested plane into low byte
	lsl		r1, #24						// r1 highest byte = high byte read from VGA VRAM
	orr		r2, r1, r0, lsl #16			// r2 = 16bit value from VGA VRAM in high halfword
	//-------
	// Get the imm16 value
	//-------
	ldrb	r0, [r12], #1
	ldrb	r1, [r12], #1
	lsl	 	r0, #16
	orr		r0, r1, lsl #24				// r0 = imm16 value in high halfword
	//-------
	// Perform the test
	//-------
	tst		r2, r0
	b		loop

// ------------------- FE = INC/DEC r/m8 -------------------------------
// INC x is like ADD x,1, except the Carry flag is not changed.
//
	.global	inc_byte_MODEX_r2
inc_byte_MODEX_r2:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r1 high byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r1, r3, lsr r1
	lsl		r1, #24					// Put the value of the requested plane into highest byte of r1
	//-------
	// Increment the VGA RAM value
	//-------
	mrs		r0,cpsr					// Get original flags to r0.
	adds	lr, r1, #0x01000000		// Add 1 to the high byte
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	//-------
	// Fix the Carry flag
	//-------
	mrs		r1,cpsr					// Save new flags to r1
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	//-------
	// Return
	//-------
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0

	.global	dec_byte_MODEX_r2
dec_byte_MODEX_r2:
	calc_modex_r2
	//-------
	// First get the current VGA RAM value into r1 high byte
	//-------
	ldr		r1, =VGAReadMap
	ldr		r3, [r2]				// Get the value from the VGA VRAM
	ldrb	r1, [r1]
	str		r3, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	mov		r1, r3, lsr r1
	lsl		r1, #24					// Put the value of the requested plane into highest byte of r1
	//-------
	// Decrement the VGA RAM value
	//-------
	mrs		r0,cpsr					// Get original flags to r0.
	subs	lr, r1, #0x01000000		// Sub 1 from the high byte
	//-------
	// Determine the output bit planes
	//-------
	ldr		r1, =MODEX_WRITE_MASK32
	orr		lr, lr, lsr #8
	ldr		r1, [r1]				// r1 = mask of the pixels to change
	orr		lr, lr, lsr #16
	//-------
	// Mask the bit planes as needed
	//-------
	bic		r3, r1					// Clear the pixels we are to update
	and		lr, r1					// Leave the pixels we are to update
	orr		r1, r3, lr				// Replace the original pixels with the new pixels
	//-------
	// Write the new value
	//-------
	str		r1, [r2]
	//-------
	// Fix the Carry flag
	//-------
	mrs		r1,cpsr					// Save new flags to r1
	and		r0, #0x20000000			// r0 = Only the original Carry flag bit
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	orr		r0, r1					// r0 = new flags + original Carry flag
	//-------
	// Return
	//-------
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0

// ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
//
// 26FF7708 push word es:[bx+08] (Aces Over Europe)
//
	.global	push_MODEX_r2
push_MODEX_r2:
	calc_modex_r2
	ldrd	r0, r1, [r2]				// r0 = low byte, r1 = high byte
	ldr		r2, =VGAReadMap
	str		r1, [sp, #SP_VGA_LATCH]		// Store the last value read to VGA_latch
	ldrb	r2, [r2]
	lsl		r2, #3						// r2 = 0, 8, 16 or 24
	lsr		r0, r2						// Put the value of the requested plane into low byte
	and		r0, #0xFF
	lsr		r1, r2						// Put the value of the requested plane into low byte
	orr		r0, r1, lsl #8				// r0 = 16bit value from VGA VRAM in high halfword
	push_hword r0 r1 r2
	b		loop

	.global	push_MODEX_32_r2
push_MODEX_32_r2:
	ldr		r1, =VGAReadMap
	calc_modex_r2
	ldrb	r3, [r1]
	ldrd	r0, r1, [r2]				// r0 = low byte, r1 = second byte
	lsl		r3, #3						// r3 = 0, 8, 16 or 24
	lsr		r0, r3						// Put the value of the requested plane into low byte
	lsr		r1, r3						// Put the value of the requested plane into low byte
	and		r0, #0xFF
	and		r1, #0xFF
	orr		r0, r1, lsl #8				// r0 = 16bit value from VGA VRAM
	mov		r1, r3
	ldrd	r2, r3, [r2, #8]			// r2 = third byte, r3 = high byte
	lsr		r2, r1						// Put the value of the requested plane into low byte
	str		r3, [sp, #SP_VGA_LATCH]		// Store the last value read to VGA_latch
	lsr		r3, r1						// Put the value of the requested plane into low byte
	and		r2, #0xFF
	orr		r0, r2, lsl #16
	orr		r0, r3, lsl #24
	push_dword r0 r1 r2
	b		loop

	.ltorg

// ------------------- REP MOVSB ---------------------------------------
//

	//-------
	// Direction flag clear
	//-------
	.global	rep_movsb_cld_from_MODEX
rep_movsb_cld_from_MODEX:
	calc_modex_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsb_cld_MODEX_RAM .unknown_back1 rep_movsb_cld_MODEX_MODEX
	
	//-------
	// Direction flag set
	//-------
	.global	rep_movsb_std_from_MODEX
rep_movsb_std_from_MODEX:
	calc_modex_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsb_std_MODEX_RAM .unknown_back1 rep_movsb_std_MODEX_MODEX
	

// ------------------- REP MOVSB from RAM to MODEX ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in RAM
	//	r2 = physical start address of ES:DI in Mode-X VRAM (with MODEX flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
	.global	rep_movsb_cld_RAM_MODEX
rep_movsb_cld_RAM_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Then calculate the mask to use
	//-------
2:	ldr		r4, =MODEX_WRITE_MASK32
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = Map and Bit Masks combined
	//	r5 = total loop counter
	//	r6 = word read/written to VGA VRAM
	//-------
1:	ldrb	r0, [r1], #1			// Get the byte from DS:SI
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	pop		{r0, r3, r4}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	beq		1f
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.

	.global	rep_movsb_std_RAM_MODEX
rep_movsb_std_RAM_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	subs	r3, #1					// Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_prev_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_beg 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_zero_16 r3 esi		// ----- Check with SI
	min_cnt_idx_zero_16 r3 edi		// ----- Check with DI
1:	add		r3, #1					// Restore r3 count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		// Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Then calculate the mask to use
	//-------
2:	ldr		r4, =MODEX_WRITE_MASK32
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = Map and Bit Masks combined
	//	r5 = total loop counter
	//	r6 = word read/written to VGA VRAM
	//-------
1:	ldrb	r0, [r1], #-1			// Get the byte from DS:SI
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #-4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	pop		{r0, r3, r4}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	subs	r3, #1					// Decrease the count by one for page checks
	beq		1f
	min_cnt_prev_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_beg 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
1:	add		r3, #1					// Restore r3 count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3					// Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					// Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.

// ------------------- REP MOVSB from MODEX to RAM ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in Mode-X VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in RAM
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsb_cld_MODEX_RAM:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}				// Push used registers
	//-------
	// Check the current Read Mode and get the Read Mask.
	//-------
	ldr		r0, =VGAModeReg
	ldr		lr, =VGAReadMap
	ldrb	r0, [r0]
	ldrb	lr, [lr]
	tst		r0, #8
	popne	{r0, r3}
	bne		bad_MODEX_mode1			// Read Mode 1 not supported!
	lsl		lr, #3					// lr = 0, 8, 16 or 24
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input VRAM pointer (DS:SI)
	//	r2 = output RAM pointer (ES:DI)
	//	r3 = loop counter
	//	lr = Read Mask shift value
	//-------
2:	ldr		r0, [r1], #4
	subs	r3, #1					// Decrement the loop counter
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	bgt		2b
	//-------
	// Update the VGA Read Latch
	//-------
	rsb		lr, #32
	ror		lr, r0, lr				// lr = The last value read from VRAM
	//-------
	// Return
	//-------
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	beq		1f
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.

rep_movsb_std_MODEX_RAM:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}				// Push used registers
	//-------
	// Check the current Read Mode and get the Read Mask.
	//-------
	ldr		r0, =VGAModeReg
	ldr		lr, =VGAReadMap
	ldrb	r0, [r0]
	ldrb	lr, [lr]
	tst		r0, #8
	popne	{r0, r3}
	bne		bad_MODEX_mode1			// Read Mode 1 not supported!
	lsl		lr, #3					// lr = 0, 8, 16 or 24
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	subs	r3, #1					// Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_VGA_beg 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_prev_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r3 esi		// ----- Check with SI
	min_cnt_idx_zero_16 r3 edi		// ----- Check with DI
1:	add		r3, #1					// Restore the count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		// Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input VRAM pointer (DS:SI)
	//	r2 = output RAM pointer (ES:DI)
	//	r3 = loop counter
	//	lr = Read Mask shift value
	//-------
2:	ldr		r0, [r1], #-4
	subs	r3, #1					// Decrement the loop counter
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #-1			// Store a byte
	bgt		2b
	//-------
	// Update the VGA Read Latch
	//-------
	rsb		lr, #32
	ror		lr, r0, lr				// lr = the original value read from VRAM
	//-------
	// Return
	//-------
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	subs	r3, #1					// Decrease the count by one for page checks
	beq		1f
	min_cnt_VGA_beg 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_prev_page 	r3 r2		// ----- Check with ES:DI bytes before page end
1:	add		r3, #1					// Restore the count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3					// Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					// Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	
// ------------------- REP MOVSB from MODEX to MODEX --------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in Mode-X VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in Mode-X VRAM (with MODEX flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsb_cld_MODEX_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Test for current write mode.
	//-------
2:	check_write_mode r0
	beq		movsb_cld_EGA_EGA_words	// Call the write mode 1 routine in "EGA.s"!
	popgt	{r0, r3}
	bgt		bad_MODEX_mode2			// Nope, write mode 2 or 3, not supported!
	//-------
	// Write Mode 0: Copy the data within the EGA/VGA emulated VRAM
	//-------
	push	{r5, r6, r7}
	//-------
	// Calculate the masks to use
	//-------
	ldr		r6, =VGAReadMap
	ldr		r7, =MODEX_WRITE_MASK32
	ldrb	r6, [r6]
	ldr		r7, [r7]				// lr = mask of the pixels to change
	lsl		r6, #3					// r6 = 0, 8, 16 or 24
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r5 = word read/written to VGA VRAM
	//	r6 = VGA Read Map << 3
	//	r7 = Write Mask
	//	lr = Last data read from VGA VRAM
	//-------
1:	ldr		lr, [r1], #4 			// Get the value from DS:SI (VGA VRAM)
	ldr		r5, [r2]				// Get the current data from ES:DI (VGA VRAM)
	mov		r0, lr, lsr r6			// Put the value of the requested plane into low byte
	and		r0, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r1 = the input word value
	bic		r5, r7					// Clear the bits we are to replace
	and		r0, r7					// r0 = Bits to change in EGA RAM
	orr		r5, r0					// Replace the bits
	str		r5, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bgt		1b
	//-------
	// Return
	//-------
	pop		{r5, r6, r7}
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	cmp		r3, #1					// Shortcut for when we only need to move one byte, that is always possible.
	beq		1f
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.

rep_movsb_std_MODEX_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	subs	r3, #1					// Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		1f
	min_cnt_VGA_beg 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_beg 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_zero_16 r3 esi		// ----- Check with SI
	min_cnt_idx_zero_16 r3 edi		// ----- Check with DI
1:	add		r3, #1					// Restore the count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		// Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Test for current write mode.
	//-------
2:	check_write_mode r0
	beq		.rep_movsb_std_mode1	// Yep
	popgt	{r0, r3}
	bgt		bad_MODEX_mode2			// Nope, write mode 2 or 3, not supported!
	//-------
	// Write Mode 0: Copy the data within the EGA/VGA emulated VRAM
	//-------
	push	{r5, r6, r7}
	//-------
	// Calculate the masks to use
	//-------
	ldr		r6, =VGAReadMap
	ldr		lr, =MODEX_WRITE_MASK32
	ldrb	r6, [r6]
	ldr		lr, [lr]				// lr = mask of the pixels to change
	lsl		r6, #3					// r6 = 0, 8, 16 or 24
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r5 = word read/written to VGA VRAM
	//	r6 = VGA Read Map << 3
	//	r7 = Write Mask
	//	lr = Last data read from VGA VRAM
	//-------
1:	ldr		lr, [r1], #-4 			// Get the value from DS:SI (VGA VRAM)
	ldr		r5, [r2]				// Get the current data from ES:DI (VGA VRAM)
	mov		r0, lr, lsr r6			// Put the value of the requested plane into low byte
	and		r0, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r1 = the input word value
	bic		r5, r7					// Clear the bits we are to replace
	and		r0, r7					// r0 = Bits to change in EGA RAM
	orr		r5, r0					// Replace the bits
	str		r5, [r2], #-4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bgt		1b
	//-------
	// Return
	//-------
	pop		{r5, r6, r7}
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	subs	r3, #1					// Decrease the count by one for page checks
	beq		1f
	min_cnt_VGA_beg 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_beg 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
1:	add		r3, #1					// Restore the count
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		esi, r3					// Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					// Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.

	//-------
	// Write Mode 1: Copy the data within the EGA/VGA emulated VRAM
	//-------
.rep_movsb_std_mode1:
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = free
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	lr = scratch (input byte)
	//-------
1:	ldr		lr, [r1], #-4 			// Get the value from DS:SI (VGA VRAM)
	subs	r3, #1
	str		lr, [r2], #-4			// Write the result to ES:DI (VGA VRAM)
	bgt		1b
	//-------
	// Return
	//-------
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		// ... else handle the next 16K page.

	.ltorg
	
// ------------------- REP MOVSW ---------------------------------------
//

	//-------
	// Direction flag clear
	//-------
	.global	rep_movsw_cld_from_MODEX
rep_movsw_cld_from_MODEX:
	calc_modex_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_cld_MODEX_RAM .unknown_back1 rep_movsw_cld_MODEX_MODEX
	
	//-------
	// Direction flag set
	//-------
	.global	rep_movsw_std_from_MODEX
rep_movsw_std_from_MODEX:
	calc_modex_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_std_MODEX_RAM .unknown_back1 rep_movsw_std_MODEX_MODEX

rep_movsw_std_MODEX_RAM:
rep_movsw_std_MODEX_MODEX:
	b		.unknown

// ------------------- REP MOVSW from RAM to MODEX ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in RAM
	//	r2 = physical start address of ES:DI in Mode-X VRAM (with MODEX flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
	.global	rep_movsw_cld_RAM_MODEX
rep_movsw_cld_RAM_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	//-------
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx
	lsl		r3, #1					// Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = Map and Bit Masks combined
	//	r5 = total loop counter
	//	r6 = word read/written to VGA VRAM
	//	lr = current data at output ES:DI address
	//-------
2:	ldrb	r0, [r1], #1			// Get the byte from DS:SI
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		2b
	pop		{r0, r3, r4}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #17		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4}			// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	lsl		r3, #1					// Number of bytes to move = 2*CX
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #1			// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//------
	// Move the remaining byte to a possibly different page.
	//------
	pop		{r0, r3, r4}			// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

// ------------------- REP MOVSW from MODEX to MODEX --------------------
//

	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in Mode-X VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in Mode-X VRAM (with MODEX flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsw_cld_MODEX_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	lsl		r3, #1					// Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Test for current write mode.
	//-------
2:	check_write_mode r0
	beq		movsw_cld_EGA_EGA_words	// Call the write mode 1 routine in "EGA.s"!
	popgt	{r0, r3}
	bgt		bad_MODEX_mode2			// Nope, write mode 2 or 3, not supported!
	//-------
	// Write Mode 0: Copy the data within the EGA/VGA emulated VRAM
	//-------
	push	{r5, r6, r7}
	//-------
	// Calculate the masks to use
	//-------
	ldr		r6, =VGAReadMap
	ldr		r7, =MODEX_WRITE_MASK32
	ldrb	r6, [r6]
	ldr		r7, [r7]				// lr = mask of the pixels to change
	lsl		r6, #3					// r6 = 0, 8, 16 or 24
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r5 = word read/written to VGA VRAM
	//	r6 = VGA Read Map << 3
	//	r7 = Write Mask
	//	lr = Last data read from VGA VRAM
	//-------
1:	ldr		lr, [r1], #4 			// Get the value from DS:SI (VGA VRAM)
	ldr		r5, [r2]				// Get the current data from ES:DI (VGA VRAM)
	mov		r0, lr, lsr r6			// Put the value of the requested plane into low byte
	and		r0, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r1 = the input word value
	bic		r5, r7					// Clear the bits we are to replace
	and		r0, r7					// r0 = Bits to change in EGA RAM
	orr		r5, r0					// Replace the bits
	str		r5, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bgt		1b
	//-------
	// Return
	//-------
	pop		{r5, r6, r7}
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	check_write_mode r0				// Should we use write mode 1?
	popne	{r0, r3}
	bne		bad_MODEX_mode2			// Nope, write mode 0, 2 or 3, not supported!
	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #17		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldr		r0, [r1], #4
	str		r0, [r2], #4
	ldr		r0, [r1], #4
	subs	r3, #1
	str		r0, [r2], #4
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldr		r0, [r1], #4
	str		r0, [r2], #4
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	ldr		r0, [r1], #4
	str		r0, [r2], #4
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	lsl		r3, #1					// Number of bytes to move = 2*CX
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	check_write_mode r0				// Should we use write mode 1?
	popne	{r0, r3}
	bne		bad_MODEX_mode2			// Nope, write mode 0, 2 or 3, not supported!
	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #1			// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldr		r0, [r1], #4
	str		r0, [r2], #4
	ldr		r0, [r1], #4
	subs	r3, #1
	str		r0, [r2], #4
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldr		r0, [r1], #4
	str		r0, [r2], #4
	//------
	// Move the remaining byte to a possibly different page.
	//------
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	ldr		r0, [r1], #4
	str		r0, [r2], #4
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

// ------------------- REP MOVSW from MODEX to RAM ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in Mode-X VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in RAM
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsw_cld_MODEX_RAM:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}				// Push used registers
	//-------
	// Check the current Read Mode and get the Read Mask.
	//-------
	ldr		r0, =VGAModeReg
	ldr		lr, =VGAReadMap
	ldrb	r0, [r0]
	ldrb	lr, [lr]
	tst		r0, #8
	popne	{r0, r3}
	bne		bad_MODEX_mode1			// Read Mode 1 not supported!
	lsl		lr, #3					// lr = 0, 8, 16 or 24
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx					// Start with r3 = CX
	lsl		r3, #1					// Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input VRAM pointer (DS:SI)
	//	r2 = output RAM pointer (ES:DI)
	//	r3 = loop counter
	//	lr = Read Mask shift value
	//-------
2:	ldr		r0, [r1], #4
	subs	r3, #1					// Decrement the loop counter
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	bgt		2b
	//-------
	// Update the VGA Read Latch
	//-------
	rsb		lr, #32
	ror		lr, r0, lr				// lr = The last value read from VRAM
	//-------
	// Return
	//-------
	pop		{r0, r3}
	str		lr, [sp, #SP_VGA_LATCH]	// Store the last value read to VGA_latch
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #17		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	ldr		r0, [r1], #4
	subs	r3, #1					// Decrement the loop counter
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	bgt		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx					// Start with r3 = ECX
	lsl		r3, #1					// Number of bytes to move = 2*CX
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #1			// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full halfwords we can move
	//------
1:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	ldr		r0, [r1], #4
	subs	r3, #1					// Decrement the loop counter
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	bgt		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	//------
	// Move the remaining byte to a possibly different page.
	//------
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	calc_modex_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	ldr		r0, [r1], #4
	ror		r0, lr					// Rotate the value of the requested plane into low byte
    strb	r0, [r2], #1			// Store a byte
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

	.ltorg
	
// ------------------- REP MOVSD ---------------------------------------
//

	//-------
	// Direction flag clear
	//-------
	.global	rep_movsd_cld_from_MODEX
rep_movsd_cld_from_MODEX:
	calc_modex_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_cld_MODEX_RAM .unknown_back1 rep_movsd_cld_MODEX_MODEX

rep_movsd_cld_MODEX_RAM:
rep_movsd_cld_MODEX_MODEX:
	b		.unknown

// ------------------- REP MOVSD from RAM to MODEX ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in RAM
	//	r2 = physical start address of ES:DI in Mode-X VRAM (with MODEX flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
	.global	rep_movsd_cld_RAM_MODEX
rep_movsd_cld_RAM_MODEX:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	//-------
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx
	lsl		r3, #2					// Number of bytes to move = 4*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3 esi		// ----- Check with SI
	min_cnt_idx_wrap_16 r3 edi		// ----- Check with DI
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #3					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3, lsl #16		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			// ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Loop here for r3 bytes.
	// Registers:
	//	r0 = scratch (input byte)
	//	r1 = input RAM pointer (DS:SI)
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = Map and Bit Masks combined
	//	r5 = total loop counter
	//	r6 = word read/written to VGA VRAM
	//	lr = current data at output ES:DI address
	//-------
2:	ldrb	r0, [r1], #1			// Get the byte from DS:SI
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		2b
	pop		{r0, r3, r4}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsd_cld_next		// ... else handle the next 16K page.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #2			// r3 = number of full words to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #18		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #18		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//------
	// First move the full words we can move
	//------
1:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)

	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//------
	// Move the remaining bytes to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4}			// pop flags and 16-bit/32-bit mask
	
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	add		r0, esi, #2				// r0 = SI+2
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #2				// r0 = DI+2
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	add		r0, esi, #3				// r0 = SI+3
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #3				// r0 = DI+3
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}

	//------
	// Adjust the registers, one more word moved.
	//------
	sub		ecx, #1
	mov		r0, #0x40000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_movsd_cld_next		// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx, lsl #2			// Start with r3 = ECX*4
	min_cnt_next_page 	r3 r1		// ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3 r2		// ----- Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #3					// Are we to move an uneven number of bytes?
	bne		5f						// Yes, so handle this situation specially.
	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	//-------
	// Uneven number of bytes to move, so need special attention.
	//-------
5:	movs	r3, r3, lsr #2			// r3 = number of full words to move
	beq		5f						// Jump if no full words fit here
	add		esi, r3, lsl #2			// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #2			// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full words we shall move.
	//------
	// First move the full words we can move
	//------
1:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	ldrb	r0, [r1], #1
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	//------
	// Move the remaining byte to a possibly different page.
	//------
	pop		{r0, r3, r4}			// pop flags and 16-bit/32-bit mask
	
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	add		r0, esi, #2				// r0 = SI+2
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #2				// r0 = DI+2
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	add		r0, esi, #3				// r0 = SI+3
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #3				// r0 = DI+3
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 bad_string_op_seg_back1 1f
1:	push	{r4}
	calc_modex_r2
	ldr		r4, =MODEX_WRITE_MASK32
	ldrb	r0, [r1], #1
	ldr		r4, [r4]				// r4 = mask of the pixels to change
	ldr		lr, [r2] 				// Get the current data from ES:DI (VGA VRAM)
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 contains 4 pixels of the input color
	bic		lr, r4					// Clear the bits we are to replace
	and		r0, r4					// r0 = Bits to change in EGA RAM
	orr		lr, r0					// Replace the bits
	str		lr, [r2], #4			// Write the result to ES:DI (VGA VRAM)
	pop		{r4}
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #4
	add		edi, #4
	mrs		r0, cpsr				// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_movsd_cld_next		// ... else handle the next 16K page.


// ------------------- REP STOSB (CLD) ---------------------------------
//
	.global	rep_stosb_cld_MODEX
rep_stosb_cld_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of bytes to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
	cmp		r3, #0
	and		r3, ecx								// r3 = number of bytes to store
	blt		32f
	//-------
	// 16-bit addressing
	//-------
	ror		edi, #16
	min_cnt_idx_wrap_16 r3 edi
	min_cnt_VGA_end 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	add		edi, r3, lsl #16					// Increment DI by the number of bytes we stored.
	ror		edi, #16
	b		16f
	//-------
	// 32-bit addressing
	//-------
32:	min_cnt_VGA_end 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	add		edi, r3
	//-------
	// Common to both 16-bit and 32-bit addressing
	//-------
16:	sub		ecx, r3								// CX = number of bytes remaining
.stos_common:						// From this point on this is also used for REP STOSW when AH == AL 
	ldr		r0, =MODEX_WRITE_MASK32
	ldr		r0, [r0]
	adds	r1, r0, #1				// Is the mask 0xFFFFFFFF (zero flag set if it is)
	and		r1, eax, #0xFF
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16			// r1 = word value to write
	bne		2f						// Jump to slower code if we are not writing to all planes
	//-------
	// We are to write to all planes. Prepare r1 = word to store,
	// and call the "cpu_string.s" stos_word_align routine.
	//-------
	lsl		r3, #2					// Each byte is a word in VGA VRAM, so adjust the count.
	bl		stos_word_align			// Needs r1 = word to store, r2 = physical address, r3 = number of bytes
	//-------
	// All done for this block!
	//-------
	pop		{r0, r3}
	tst		ecx, r3
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosb_cld			// ... else handle the next 16K page.
	//-------
	// Write only to some planes.
	//-------
2:	and		r1, r0					// And the value with the write mask
	//-------
	// Do it!
	//-------
1:	ldr		lr, [r2]				// Use lr register for temp value, as it happens to be free.
	subs	r3, #1
	bic		lr, r0
	orr		lr, r1
	str		lr, [r2], #4
	bne		1b
	//-------
	// Return
	//-------
	pop		{r0, r3}
	tst		ecx, r3
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosb_cld			// ... else handle the next 16K page.
	
	.ltorg

// ------------------- REP STOSB (STD) ---------------------------------
// NHL '94 (HOCKEY.EXE)
//
	.global	rep_stosb_std_MODEX
rep_stosb_std_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of bytes to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
	cmp		r3, #0
	and		r3, ecx								// r3 = number of bytes to store
	sub		r3, #1								// Decrease by one for the bage tests
	blt		32f
	//-------
	// 16-bit addressing
	//-------
	ror		edi, #16
	min_cnt_idx_zero_16 r3 edi
	min_cnt_VGA_beg 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	add		r3, #1								// Restore the counter
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		edi, r3, lsl #16					// Decrement DI by the number of bytes we stored.
	ror		edi, #16
	b		16f
	//-------
	// 32-bit addressing
	//-------
32:	min_cnt_VGA_beg 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	add		r3, #1
	sub		edi, r3
	//-------
	// Common to both 16-bit and 32-bit addressing
	//-------
16:	ldr		r0, =MODEX_WRITE_MASK32
	sub		ecx, r3								// CX = number of bytes remaining
	ldr		r0, [r0]
	and		r1, eax, #0xFF
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16			// r1 = word value to write
	and		r1, r0					// And the value with the write mask
	//-------
	// Do it!
	//-------
1:	ldr		lr, [r2]				// Use lr register for temp value, as it happens to be free.
	subs	r3, #1
	bic		lr, r0
	orr		lr, r1
	str		lr, [r2], #-4
	bne		1b
	//-------
	// Return
	//-------
	pop		{r0, r3}
	tst		ecx, r3
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosb_std			// ... else handle the next 16K page.

// ------------------- REP STOSW (CLD) ---------------------------------
//
	.global	rep_stosw_cld_MODEX
rep_stosw_cld_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of BYTES to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
	ldr		r0, =EGAVGA_A000		// r0 = end of the EGAVGA VRAM
	and		r3, ecx					// Start with r3 = CX
	ldr		r0, [r0]
	lsl		r3, #1					// Number of bytes to store = 2*CX
	add		r0, #0x40000
	// ----- Check with ES:DI bytes before page end
	sub		lr, r0, r2				// Use lr register for temp value, as it happens to be free.
	cmp		r3, lr, lsr #2
	movgt	r3, lr, lsr #2
	// ----- Check with DI
	ror		edi, #16
	min_cnt_idx_wrap_16 r3 edi
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we stored.
	ror		edi, #16
	sub		ecx, r3, lsr #1			// CX = number of bytes remaining
	//-------
	// Test if AH == AL, and use the STOSB code if it is.
	//-------
	mov		r1, eax, lsl #16
	orr		r1, r1, lsr #16			// r1 = value to store (in all 4 bytes)
	eors	lr, r1, r1, ror #8		// Zero flag set if AH == AL
	beq		.stos_common			// Go to REP STOSB code if AH == AL
	//=======
	// Now AH != AL, so we need to store the bytes separately.
	//=======
	tst		r5, #1
	rorne	r1, #8					// Swap AL and AH if we have an odd total number of bytes to do.
	//-------
	// Adjust the registers by the number of bytes we will store.
	// Also check the planes we are to write to.
	//-------
	ldr		r0, =MODEX_WRITE_MASK32
	ldr		r0, [r0]
	adds	lr, r0, #1				// Is the mask 0xFFFFFFFF (zero flag set if it is)
	bne		3f						// Jump to slower code if we are not writing to all planes
	//-------
	// We are to write to all planes.
	// Prepare r0 & r1 = the bytes to write
	//-------
	and		r0, r1, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 = AL value
	and		r1, #0xFF00			
	orr		r1, r1, lsr #8
	orr		r1, r1, lsl #16			// r1 = AH value
	//-------
	// Do it!
	//-------
1:	str		r0, [r2], #4			// Store AL
	subs	r3, #1
	beq		2f						// Jump if r3 is zero
	str		r1, [r2], #4			// Store AH
	subs	r3, #1
	bne		1b
	//-------
	// All done for this block!
	//-------
2:	pop		{r0, r3}
	tst		ecx, r3
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosw_cld			// ... else handle the next 16K page.
	//-------
	// We are not writing to all planes, so use slower code.
	//-------
3:	push	{r4}
	mov		lr, r0					// lr = MODEX_WRITE_MASK32
	//-------
	// Prepare r0 & r1 = the bytes to write
	//-------
	and		r0, r1, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 = AL value
	and		r0, lr
	and		r1, #0xFF00			
	orr		r1, r1, lsr #8
	orr		r1, r1, lsl #16			// r1 = AH value
	and		r1, lr
	//-------
	// Do it!
	//-------
1:	ldr		r4, [r2]
	subs	r3, #1
	beq		2f						// Jump if r3 is zero
	bic		r4, lr
	orr		r4, r0
	str		r4, [r2], #4
	ldr		r4, [r2]
	subs	r3, #1
	bic		r4, lr
	orr		r4, r1
	str		r4, [r2], #4
	bne		1b
	//-------
	// Return!
	//-------
2:	pop		{r4}
	pop		{r0, r3}
	cmp		r5, #0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosw_cld		// ... else handle the next 16K page.

// ------------------- REP STOSW (STD) ---------------------------------
// This is a silly and rare operation!
//
	.global	rep_stosw_std_MODEX
rep_stosw_std_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI+1 address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of BYTES to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_modex_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with ES:DI bytes in this page
	ldr		r0, =EGAVGA_A000
	ldr		r0, [r0]
	rsb		r0, r2					// r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	// ----- Check with DI
	sub		r3, #1					// DI points 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					// Restore the counter
	//-------
	// r1 = word value to store
	//-------
	mov		r1, eax, lsl #16
	orr		r1, r1, lsr #16			// r1 = value to store (in all 4 bytes)
	tst		r5, #1
	rorne	r1, #8					// If r5 is odd, we need to swap AH and AL when continuing.
	//-------
	// Adjust the registers by the number of bytes we will store.
	// Also check the planes we are to write to.
	//-------
	ldr		r0, =MODEX_WRITE_MASK32
	sub		r11, r3, lsl #16		// Increment DI by the number of bytes we shall store..
	ldr		r0, [r0]
	sub		r5, r3					// r5 = number of bytes remaining
	adds	lr, r0, #1				// Is the mask 0xFFFFFFFF (zero flag set if it is)
	bne		3f						// Jump to slower code if we are not writing to all planes
	//-------
	// We are to write to all planes.
	// Prepare r0 & r1 = the bytes to write
	//-------
	and		r0, r1, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 = AL value
	and		r1, #0xFF00			
	orr		r1, r1, lsr #8
	orr		r1, r1, lsl #16			// r1 = AH value
	//-------
	// Do it!
	//-------
1:	str		r1, [r2], #-4			// Store AH
	subs	r3, #1
	beq		2f						// Jump if r3 is zero
	str		r0, [r2], #-4			// Store AL
	subs	r3, #1
	bne		1b
	//-------
	// All done for this block!
	//-------
2:	pop		{r0, r3}
	cmp		r5, #0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosw_std			// ... else handle the next 16K page.
	//-------
	// We are not writing to all planes, so use slower code.
	//-------
3:	push	{r4}
	mov		lr, r0					// lr = MODEX_WRITE_MASK32
	//-------
	// Prepare r0 & r1 = the bytes to write
	//-------
	and		r0, r1, #0xFF
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16			// r0 = AL value
	and		r0, lr
	and		r1, #0xFF00			
	orr		r1, r1, lsr #8
	orr		r1, r1, lsl #16			// r1 = AH value
	and		r1, lr
	//-------
	// Do it!
	//-------
1:	ldr		r4, [r2]
	subs	r3, #1
	beq		2f						// Jump if r3 is zero
	bic		r4, lr
	orr		r4, r1
	str		r4, [r2], #-4			// Store AH
	ldr		r4, [r2]
	subs	r3, #1
	bic		r4, lr
	orr		r4, r0
	str		r4, [r2], #-4			// Store AL
	bne		1b
	//-------
	// Return!
	//-------
2:	pop		{r4}
	pop		{r0, r3}
	cmp		r5, #0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosw_std			// ... else handle the next 16K page.

// ------------------- REP STOSD (CLD) ---------------------------------
//
	.global	rep_stosd_cld_MODEX
rep_stosd_cld_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with MODEX flags)
	// r3 = address size mask, must be saved
	// r4 = EAX, must be saved
	// r5 = ECX = number of words to store, must be cleared
	// r6..r10 must be saved
	// r11 = EDI = +/- ECX
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Test whether we are in USE16 or USE32 mode
	//-------
	calc_modex_r2
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16 version:
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
	ldr		r0, =EGAVGA_A000		// r0 = end of the EGAVGA VRAM
	and		r3, ecx					// Start with r3 = CX
	ldr		r0, [r0]
	lsl		r3, #2					// Number of bytes to store = 4*CX
	add		r0, #0x40000
	// ----- Check with ES:DI bytes before page end
	sub		lr, r0, r2				// Use lr register for temp value, as it happens to be free.
	cmp		r3, lr, lsr #2
	movgt	r3, lr, lsr #2
	// ----- Check with DI
	ror		edi, #16
	min_cnt_idx_wrap_16 r3 edi
	add		edi, r3, lsl #16		// Increment DI by the number of bytes we will store.
	ror		edi, #16
	sub		ecx, r3, lsr #2			// CX = number of bytes remaining
	b		1f						// Continue with common code between USE16 and USE32 versions.
	//-------
	// USE32 version:
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg)
	//-------
3:	ldr		r0, =EGAVGA_A000					// r0 = end of the EGAVGA VRAM
	and		r3, ecx								// r3 = number of bytes/halfwords/dwords to store
	ldr		r0, [r0]
	lsl		r3, #2								// Number of bytes to store = 4*CX
	add		r0, #0x40000
	// ----- Check with ES:DI bytes before page end
	sub		lr, r0, r2							// Use lr register for temp value, as it happens to be free.
	cmp		r3, lr, lsr #2
	movgt	r3, lr, lsr #2
	add		edi, r3
	sub		ecx, r3, lsr #2
	//-------
	// Test if EAX all bytes are similar, and use the STOSB code if they are.
	//-------
1:	eors	lr, eax, eax, ror #8				// Zero flag set if all bytes are similar
	beq		.stos_common						// Go to REP STOSB code if AH == AL
	pop		{r0, r3}
	b		.unknown
	


// ------------------- F3 AE = REPE SCASB ------------------------------
//
	.global	repe_scasb_cld_MODEX
repe_scasb_cld_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = logical ES:DI address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of bytes to scan
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	lsl		r5, #16					// Put CX back to the high halfword of r5
	//-------
	// Calculate physical ES:DI start address and the shift value to use
	//-------
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldrb	r1, [r1]
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24
	//-------
	// Perform the repe scasb
	//-------
	mrs		r0,cpsr					// Save flags
	bic		r0, #0xFF
	orr		r0, r1					// r0 lowest byte = shift value
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
1:	cmp		r5, #0
	beq		restore_flags_from_r0
	// 4) Perform the string operation once.
	ldr		r1, [r2], #4 			// Get the value from ES:DI (VGA VRAM)
	mov		r1, r1, lsl r0			// Put the value of the requested plane into highest byte
	and		r1, #0xFF000000			// Clean up the other bytes
	add		r11, #0x00010000		// Increment DI
	cmp		r1, r4, lsl #8			// Compare byte with AL
	orreq	r0,#0x40000000			// Set the Zero flag if the comparison was equal
	bicne	r0,#0x40000000			// Clear Zero flag if the comparison was not equal
	// 5) Decrement CX, no flags are modified.
	sub		r5, #0x00010000			// Decrement CX
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	beq		1b
	b		restore_flags_from_r0

// ------------------- F2 AE = REPNE SCASB -----------------------------
//
	.global	repne_scasb_cld_MODEX
repne_scasb_cld_MODEX:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = logical ES:DI address (with MODEX flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX << 16 = number of bytes to scan
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	//-------
	// Calculate physical ES:DI start address and the shift value to use
	//-------
	calc_modex_r2
	ldr		r1, =VGAReadMap
	ldrb	r1, [r1]
	lsl		r1, #3					// r1 = 0, 8, 16 or 24
	rsb		r1, #24
	//-------
	// Perform the repne scasb
	//-------
	mrs		r0,cpsr					// Save flags
	bic		r0, #0xFF
	orr		r0, r1					// r0 lowest byte = shift value
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
1:	cmp		r5, #0
	beq		restore_flags_from_r0
	// 4) Perform the string operation once.
	ldr		r1, [r2], #4 			// Get the value from ES:DI (VGA VRAM)
	mov		r1, r1, lsl r0			// Put the value of the requested plane into highest byte
	and		r1, #0xFF000000			// Clean up the other bytes
	add		r11, #0x00010000		// Increment DI
	cmp		r1, r4, lsl #8			// Compare byte with AL
	orreq	r0,#0x40000000			// Set the Zero flag if the comparison was equal
	bicne	r0,#0x40000000			// Clear Zero flag if the comparison was not equal
	// 5) Decrement CX, no flags are modified.
	sub		r5, #0x00010000			// Decrement CX
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	bne		1b
	b		restore_flags_from_r0

// ------------------- COPY TO SCREEN ----------------------------------
//	r0 = pixels = pointer to offscreen screen buffer 
//	r1 = &render = pointer to RenderData structure to be filled
//

#define	SCREEN_WIDTH	1024

	//-------
	// Copy Mode-X graphics screen to physical screen.
	// Each pixel consists of a byte in one of the four different bit planes.
	//-------
	.global screen_copy_ModeX
screen_copy_ModeX:
	push    {r4-r12, lr}
#ifdef RPi
	ldr		r12, =VGA_CRTC_data_3D5
	ldr		r5, =EGAVGA_A000			// VGA screen start
	ldr		r6, =VGA_misc_3C2			// Check the current VSync polarity
	//-------
	// Calculate the required RenderData structure values
	//-------
	ldrb	r2,[r12, #0x0D]				// Start Address Low
	ldrb	r3,[r12, #0x0C]				// Start Address High
	ldrb	r4, [r12, #0x13]			// r4 = VGA offset (pitch) value
	ldr		r7, [r5]					// r7 = render.imgbot = VGA VRAM start address
	ldrb	r6, [r6]					// r6 = VGA Misc Register value
	ldrb	r10, [r12, #0x07]			// r10 = VGA Overflow Register value
	ldrb	r11, [r12, #0x18]			// r11 = VGA Line Compare Register value
	ldrb	r9, [r12, #0x09]			// r9 = VGA Maximum Scan Line register value
	lsl		r4, #3						// r4 = render.pitch
	add		r5, r7, r3, lsl #(8+2)
	add		r5, r2, lsl #2				// r5 = render.imgtop = VRAM area start + (VGA start address<<2)
	//------
	// Flashback special handling (256x224 screen size)
	//------
	cmp		r4, #320
	bge		1f
	ldr		r0, =MaxScreenX
	ldrb	r3, [r12, #0x12]			// r3 = Vertical Display End value
	str		r4, [r0]					// Save witdth (less than 320) to MaxScreenX
	cmp		r3, #0xBF					// r3 == 0xBF => VDE = 256 + 191 = 447 => MaxScreenY = 224
	bne		1f
	ldr		r0, =MaxScreenY
	mov		r6, #224
	str		r6, [r0]
	b		2f
	//------
	// Determine VSync polarity (between 200/240 or 400/480 screen height)
	//------
1:	and		r6, #0xC0
	cmp		r6, #0xC0					// Is it 480/240 lines?
	movne	r6, #200					// Nope, copy 200 lines to VRAM
	moveq	r6, #240					// Yep, copy 240 lines to VRAM
2:	tst		r10, #0x10					// Line Compare bit 8 on?
	orrne	r11, #0x100					// Yep, update Line Compare Register result value
	tst		r9, #0x40					// Line Compare bit 9 on?
	orrne	r11, #0x200					// Yep, update Line Compare Register result value
	tst		r9, #1						// Character cell height = 0 (normal) or 1 (double)?
	lsleq	r6, #1						// If normal, copy 400 / 480 lines to VRAM
	lsrne	r11, #1						// If double, adjust the Line Compare value for double-scanning
	cmp		r11, r6						// Is the line compare row visible?
	bge		1f							// Nope, so continue checks
	//------
	// Add the line compare secondary texture block
	//------
	sub		r8, r6, r11					// Yes, so r8 = render.botrows = the remaining rows after line compare
	mov		r6, r11						// Yes, so r6 = render.toprows = the rows before line compare
2:	stmia	r1, {r4-r8}					// Save to renderdata
	pop   	{r4-r12, pc}				// Return to caller
	//-------
	// Test for possible wrap around situation (Chaos Engine)
	//-------
1:	mul		r11, r4, r6					// r11 = pitch * screen rows
	add		r12, r7, #0x40000			// r12 = End address of the logical EGA/VGA VRAM
	add		r11, r5						// r11 = End address of the visible linear area
	cmp		r11, r12					// Is it beyond the screen end?
	movle	r7, #0						// Nope, so we don't need the bottom block
	ble		2b							// Nope, so write the renderdata and return
	//-------
	// Wrap around situation (Chaos Engine)!
	// Registers:
	//	r0  = s_pixels (0x422C8008)
	//	r1  = renderdata pointer (0x001B6FCC)
	//	r2  =
	//	r3  =
	//	r4  = render.pitch (0x160 = 352)
	//	r5  = render.imgtop = EGAVGA_A000 + (VGA start address<<2) (0x43412C40)
	//	r6  = total number of screen rows (0xC8 = 200)
	//	r7  = start address of EGAVGA_A000 (0x433DC000)
	//	r8  = 
	//	r9  =
	//	r10 =
	//	r11 = pitch * screen_rows
	//	r12 = EGAVGA_A000 + 0x40000 = end address of VGA VRAM
	//-------
	sub		r3, r11, r12				// r3 = number of bytes we went over the end (0x7F40 = 32576)
	//-------
	// Divide the wrap bytes by screen pitch, to see how many full rows
	// and how many extra pixels we need to handle. Calculate:
	// Software division routine. Algorithm taken from
	// "http://www.peter-cockerell.net/aalp/html/ch-6.html"
	// 	r8 = div (result)
	// 	r10 = mod (result)
	// 	r4 = rhs = pitch
	// 	r3 = lhs = number of bytes we went over the end
	//	r9 = count
	//-------
	lsl		r3, #14					// Maximum value is 256KB (0x1FD00000)
	mov  	r10, #0    				// Init remainder
	mov  	r8, #0    				// and result
	mov  	r9, #18    				// Set up count
1:	subs 	r9, r9, #1  			// Get first 1 bit of lhs
	beq		3f    					// into bit 31. Return if 0
	movs 	r3, r3, ASL #1
	bpl  	1b
4:	movs 	r3, r3, ASL #1  		// Get next bit into...
	adc  	r10, r10, r10   		// r10 for trial subtract
	cmp  	r10, r4    				// Can we subtract?
	subcs 	r10, r10, r4   			// Yes, so do
	adc  	r8, r8, r8   			// Shift carry into result
	subs 	r9, r9, #1  			// Next loop
	bne  	4b
	//-------
	// Division done, now
	//	r8  = render.botrows = number of rows we went over the end (= overflow_bytes / pitch) (0x5C = 92)
	//	r10 = overflow_bytes % pitch (0xC0 = 192)
	//-------	
3:	add		r7, r10					// r7 = EGAVGA_A000 + overflow bytes % pitch
	sub		r6, r8					// Decrease the render.toprows by the render.botrows
	//-------
	// Copy r10 pixels from the beginning of VRAM to the end
	//-------	
	sub		r2, r7, r10				// r2 = start of the EGAVGA VRAM
4:	ldr		r3, [r2], #4
	cmp		r2, r7
	str		r3, [r12], #4
	blt		4b	
	//-------
	// Write the renderdata and return
	//-------
	b		2b	
#else	
	ldr		r1,=EGAVGA_A000			// VGA screen start
	ldr		r2,=VGAStartAddrHigh
	ldr		r1, [r1]
	//-------
	// Calculate the input start position
	//-------
	add		r12, r1, #0x40000		// r12 = End address of the logical EGA/VGA VRAM
	ldrb	r3,[r2]					// Start Address High
	ldrb	r2,[r2, #1]				// Start Address Low
	add		r1, r3, lsl #(8+2)
	add		r1, r2, lsl #2
	//-------
	// Check the screen resolution we are to copy
	//-------
	ldr		r3, =VGA_misc_3C2		// Check the current VSync polarity
	ldr		r4, =VGAMaxScanLine		// Check whether we do line doubling
	ldrb	r3, [r3]				// r3 = VGA Misc Register value
	ldrb	r5, [r4]				// r5 = VGA Maximum Scan Line register value
	and		r3, #0xC0
	cmp		r3, #0xC0				// Is it 480/240 lines?
	movne	r8, #(SCREEN_WIDTH*200)*2	// Nope, copy 200 lines to VRAM
	moveq	r8, #(SCREEN_WIDTH*240)*2	// Yep, copy 240 lines to VRAM
	tst		r5, #1					// Character cell height = 0 (normal) or 1 (double)?
	lsleq	r8, #1					// Normal, copy 400 / 480 lines to VRAM
	add		r11, r0, r8				// r11 = VRAM output ending address
	//-------
	// Calculate Line Compare value
	//-------
	ldrb	r6, [r4, #(0x07-0x09)]	// r6 = VGA Overflow Register value
	ldrb	r7, [r4, #(0x18-0x09)]	// r7 = VGA Line Compare Register value
	tst		r6, #0x10				// Line Compare bit 8 on?
	orrne	r7, #0x100				// Yep, update Line Compare Register result value
	tst		r5, #0x40				// Line Compare bit 9 on?
	orrne	r7, #0x200				// Yep, update Line Compare Register result value
	mov		lr, r7, lsl #(10+1)		// lr = 1024*2 * Line Compare Value
	//-------
	// Are we to double each row?
	//-------
	tst		r5, #1					// Character cell height = 0 (normal) or 1 (double)?
	lsrne	lr, #1					// Adjust the Line Compare value for double-scanning
	add		lr, r0					// lr = Address where we need to rewind back to the EGAVGA_A000 address
	//-------
	// Calculate the display pitch value (vga.draw.address_add in DOSBox)
	// from CRTC registers index 0x13
	//-------
	ldr		r4, =VGAOffset
	ldrb	r3, [r4]
	lsl		r3, #3
	sub		r3, #320
	push	{r3}					// Save the display pitch value
	//-------
	// Check for 360 pixel wide mode
	//-------
	ldrb	r10, [r4, #-0x13]		// (VGA_CRTC_data_3D5 - VGAOffset)
	cmp		r10, #0x6B				// horizontal total = 360 pixels
	moveq	r2, #45					// 360 = copy 45*8 pixels per row
	movne	r2, #40					// 320 = copy 40*8 pixels per row
	//-------
	// Loop here to do between 320x200 and 360x480 pixels
	// Registers:
	//	r0 = Output VRAM address (s_pixels input parameter)
	//	r1 = Emulated EGA/VGA RAM address (EGAVGA_A000+StartAddress...)
	//	r2 = Number of 8-pixel blocks to copy per row
	//	r3 = BG_PALETTE lookup table address
	//	r4-r9 = Scratch registers
	//	r10 = Loop counter
	//	r11 = End position in physical VRAM
	//	r12 = End address of the EGAVGA_A000 area (EGAVGA_A000+0x40000)
	//	lr = Line compare address
	//-------
1:	ldr		r3, =BG_PALETTE			// Palette lookup table address
	mov		r10, r2					// Copy 40*8 = 320 pixels or 45*8 = 360 pixels per row
	
2:	ldr		r4, [r1], #4			// Get 4 bytes (4 pixels) from input

	and		r5, r4, #0xFF
	and		r7, r4, #0xFF00
	and		r6, r4, #0xFF0000
	and		r8, r4, #0xFF000000
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsr #(8-2)]
	ldr		r6, [r3, r6, lsr #(16-2)]
	ldr		r8, [r3, r8, lsr #(24-2)]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16

	ldr		r4, [r1], #4			// Get 4 bytes (4 pixels) from input
	
	and		r7, r4, #0xFF
	and		r9, r4, #0xFF00
	and		r8, r4, #0xFF0000
	and		r4, r4, #0xFF000000
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(16-2)]
	ldr		r4, [r3, r4, lsr #(24-2)]
	orr		r7, r9, lsl #16
	orr		r8, r4, lsl #16

	stmia	r0!, {r5-r8}			// Write 4 words = 16 bytes = 8 pixels	

	subs	r10, #1
	bgt		2b
	
	//-------
	// We are at an "interesting" location in target VRAM.
	//-------
	//-------
	// Adjust the pointer to the start of the next row.
	//-------
	ldr		r3, [sp]				// Get the display pitch value
	sub		r0, r2, lsl #4			// Rewind r0 back to the start of the row (16 * 40 or 16 * 45 bytes)
	add		r0, #SCREEN_WIDTH*2		// Point r0 to the next row in physical VRAM
	cmp		r0, lr					// Are we at or over Line Compare line?
	bge		9f						// Yep, handle split screen stuff
	add		r1, r3
	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block?
	bge		2f						// Yep, go handle wrapping back to start.
	//-------
	// All done?
	//-------
3:	cmp		r0, r11					// All done?
    blt    	1b						// Not yet, back to loop
	pop   	{r3-r12, pc}			// Return to caller

	//-------
	// We are at Line Compare address, continue copying from the start of the EGA_VGA_A000 area
	//-------
9:	ldr		r1,=EGAVGA_A000			// VGA screen start
	orr		lr, #0x0F000000			// No more Line Compare needed
	ldr		r1, [r1]
	b		3b
	
	//-------
	// Special code if we went over the end of the source VRAM within the scanline.
	// This is very rare, so no need to optimize this further.
	// On input:
	//	r0 = Output hardware VRAM address (start of the next row)
	//	r1 = Input logival VGA VRAM address (over EGAVGA area)
	//	r3 = display pitch value
	//	r4..r10 = free
	//	r11 = end address in physical VRAM
	//	r12 = end address in logical VGA VRAM
	//-------
2:	sub		r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.
	beq		3b						// We were exactly at the end, so nothing else needs to be done.
	sub		r5, r1, r3				// r5 = source position at the end of the previous row
	sub		r10, r12, #0x40000		// r10 = logical A000:0000
	rsbs	r10, r5					// r10 = number bytes we went over the end and need to redo
	ble		3b						// No need to redo anything, the wrap position is not visible.
	sub		r4, r0, #SCREEN_WIDTH*2	// Point r4 to the previous row in physical VRAM
	add		r4, r2, lsl #4			// Point r4 to the end of what we just copied.
	ldr		r3, =BG_PALETTE			// Palette lookup table address
4:	ldr		r6, [r5, #-4]!			// Get 4 bytes (4 pixels) from input
	and		r7, r6, #0xFF
	and		r9, r6, #0xFF00
	and		r8, r6, #0xFF0000
	and		r6, r6, #0xFF000000
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(16-2)]
	ldr		r6, [r3, r6, lsr #(24-2)]
	orr		r7, r9, lsl #16
	orr		r8, r6, lsl #16
    stmdb   r4!, {r7-r8}
	subs	r10, #4					// We copy 4 bytes at a time
	bgt		4b
	b		3b
#endif

// ------------------- DATA VALUES -------------------------------------
//
	.data
	.align	2

BRUnsMODEXMode1:
	.ascii	"Unsupported Mode-X read mode 1 operation!"
	.byte	0x0a, 0

BRUnsMODEXMode2:
	.ascii	"Unsupported Mode-X write mode 2 (or 3) operation!"
	.byte	0x0a, 0

