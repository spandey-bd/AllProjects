//=============================================================================
// CGA.S
//
// This file contains routines to handle CGA graphics (both 320x200 4-color and
// 640x200 2-color) routines.
//
//	screen_copy_04()			= Blit 320x200x4 CGA screen
//	screen_copy_06()			= Blit 640x200x2 CGA screen
//	out_3D8_CGA_Mode			= OUT 3D8,AL handler (does nothing)
//	out_3D9_CGA_Color			= OUT 3D9,AL handler (CGA 4-color palette switch)
//	in_3D9_CGA_Color			= IN AL,3D9 handler (Get current CGA 4-color palette)
//	CharToCGA4Screen			= Draw a character to CGA 4-color screen (mode 4)
//	CharToCGA6Screen			= Draw a character to CGA 2-color screen (mode 6)
//	PixelToCGA4Screen			= int10h helper code for drawing a pixel (mode 4)
//	PixelToCGA6Screen			= int10h helper code for drawing a pixel (mode 6)
//	PixelFromCGA4Screen			= int10h helper code for reading a pixel (mode 4)
//	PixelFromCGA6Screen			= int10h helper code for reading a pixel (mode 6)
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
	.file	"CGA.s"

	.global screen_copy_04
	.global screen_copy_06

	.global	out_3D8_CGA_Mode
	.global	out_3D9_CGA_Color
	.global	in_3D9_CGA_Color
	
	.global CharToCGA4Screen
	.global	CharToCGA6Screen

	.global	PixelToCGA4Screen
	.global	PixelToCGA6Screen

	.global	PixelFromCGA4Screen
	.global	PixelFromCGA6Screen

	.global	CGA_3D9_value
	
	.text
	.align	2
	.code	32
	.arm

#include "defines.inc"
#include "macros.inc"

#define	SCREEN_WIDTH	1024

// ------------------- COPY TO SCREEN ----------------------------------
// Profiler:
//	- 323*256 ticks in No$GBA
// See for possible NEON implementation: https://www.riscosopen.org/forum/forums/5/topics/715
//
screen_copy_04:
#ifdef RPi
	push    {r4-r12, lr}
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #320					// render.pitch
	mov		r5, r0						// render.imgtop
	mov		r6, #200					// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
	//-------
	// Setup the pointers etc
	//-------
	ldr		r1, =CGA_B800			// r1 = B800 segment
	ldr		r2, =CGA4LUT			// r2 = CGA look-up-table for mode 4
	ldr		r1, [r1]
	mov		r5, r0					// Remember the output address
	mov		r6, #100				// Do 100 rows
	//-------
	// Blit the even rows
	//-------
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*4 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input

	and		r8, r4, #0xFF			// r8 = First input byte
	ldr		r9, [r2, r8, lsl #2]
	and		r8, r4, #0xFF00
	ldr		r10, [r2, r8, lsr #(8-2)]
	and		r8, r4, #0xFF0000
	ldr		r11, [r2, r8, lsr #(16-2)]
	and		r8, r4, #0xFF000000
	ldr		r12, [r2, r8, lsr #(24-2)]
	stmia	r0!, {r9-r12}
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next even row
	//-------
	add		r0, #320				// Point r0 to the next even row in VRAM
	subs	r6, #1
	bne		1b
	//-------
	// Blit the odd rows
	//-------
	mov		r0, r5					// r0 = Physical VRAM start
	add		r0, #320
	add		r1, #192				// r1 = B800 segment + 8192
	mov		r6, #100				// Do 100 rows
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*4 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input
	
	and		r8, r4, #0xFF			// r8 = First input byte
	ldr		r9, [r2, r8, lsl #2]
	and		r8, r4, #0xFF00
	ldr		r10, [r2, r8, lsr #(8-2)]
	and		r8, r4, #0xFF0000
	ldr		r11, [r2, r8, lsr #(16-2)]
	and		r8, r4, #0xFF000000
	ldr		r12, [r2, r8, lsr #(24-2)]
	stmia	r0!, {r9-r12}
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next odd row
	//-------
	add		r0, #320				// Point r0 to the next odd row in VRAM
	subs	r6, #1
	bne		1b
	//-------
	// Return
	//-------
	pop    {r4-r12, pc}
#else
	push    {r0, r4-r12, lr}
	ldr		r3, =CGA_3D9_value
	ldr		r1, =CGA_B800			// r1 = B800 segment
	ldrb	r3, [r3]
	ldr		r1, [r1]
	ldr		r2, =CGA4LUT			// r2 = CGA look-up-table for mode 4
	tst		r3, #0x20				// Test the "active color set" bit
	addne	r2, #256*4*2			// Point to the second CGA4LUT table if color set bit set
	//-------
	// Blit the even rows
	//-------
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*4 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes (16 pixels) from input

	and		r5, r4, #0xFF			// r5 = first 4 pixels
	add		r5, r2, r5, lsl #3		// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r6, r7}			// r6..r7 = first 4 pixels
	and		r5, r4, #0xFF00			// r5 = next 4 pixels
	add		r5, r2, r5, lsr #(8-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r8, r9}			// r8..r9 = next 4 pixels
	and		r5, r4, #0xFF0000		// r5 = third 4 pixels
	add		r5, r2, r5, lsr #(16-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r10, r11}			// r6..r9 = first 4 pixels
	and		r5, r4, #0xFF000000		// r5 = last 4 pixels
	add		r5, r2, r5, lsr #(24-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r12, lr}			// r12..lr = last 4 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next even row
	//-------
	ldr		r3, [sp]				// r3 = VRAM start address
	add		r0, #(2*SCREEN_WIDTH-320)*2	// Point r0 to the next even row in VRAM
	sub		r3, r0, r3
	cmp		r3, #(SCREEN_WIDTH*200)*2	// Already at row 200?
	blt		1b						// Not yet, continue loop
	//-------
	// Blit the odd rows
	//-------
	ldr		r0, [sp]				// r3 = VRAM start address
	add		r1, #192				// r1 = B800 segment + 8192
	add		r0, #SCREEN_WIDTH*2		// r0 = Physical VRAM start
 
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*4 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input
	
	and		r5, r4, #0xFF			// r5 = first 4 pixels
	add		r5, r2, r5, lsl #3		// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r6, r7}			// r6..r7 = first 4 pixels
	and		r5, r4, #0xFF00			// r5 = next 4 pixels
	add		r5, r2, r5, lsr #(8-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r8, r9}			// r8..r9 = next 4 pixels
	and		r5, r4, #0xFF0000		// r5 = third 4 pixels
	add		r5, r2, r5, lsr #(16-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r10, r11}			// r6..r9 = first 4 pixels
	and		r5, r4, #0xFF000000		// r5 = last 4 pixels
	add		r5, r2, r5, lsr #(24-3)	// r4 = address into CGA4LUT, where we can load 2 words (4 pixels)
	ldmia	r5, {r12, lr}			// r12..lr = last 4 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next odd row
	//-------
	ldr		r3, [sp]				// r3 = VRAM start address
	add		r0, #(2*SCREEN_WIDTH-320)*2	// Point r0 to the next odd row in VRAM
	sub		r3, r0, r3
	cmp		r3, #(SCREEN_WIDTH*200)*2	// Already at row 200?
	blt		1b						// Not yet, continue loop
	//-------
	// Return
	//-------
	pop     {r0, r4-r12, pc}		// Return to caller
#endif
	
screen_copy_06:
#ifdef RPi
	push    {r4-r12, lr}
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #640					// render.pitch
	mov		r5, r0						// render.imgtop
	mov		r6, #200					// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
	//-------
	// Setup the pointers etc
	//-------
	ldr		r1, =CGA_B800			// r1 = B800 segment
	ldr		r2, =CGA6LUT			// r2 = CGA look-up-table for mode 6
	ldr		r1, [r1]
	mov		r5, r0					// Remember the output address
	mov		r6, #100				// Do 100 rows
	//-------
	// Blit the even rows
	//-------
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*8 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input

	and		r8, r4, #0xFF			// r8 = First input byte
	add		r8, r2, r8, lsl #3
	ldmia	r8, {r9, r10}
	and		r8, r4, #0xFF00
	add		r8, r2, r8, lsr #(8-3)
	ldmia	r8, {r11, r12}
	stmia	r0!, {r9-r12}

	and		r8, r4, #0xFF0000		// r8 = third input byte
	add		r8, r2, r8, lsr #(16-3)
	ldmia	r8, {r9, r10}
	and		r8, r4, #0xFF000000
	add		r8, r2, r8, lsr #(24-3)
	ldmia	r8, {r11, r12}
	stmia	r0!, {r9-r12}
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next even row
	//-------
	add		r0, #640				// Point r0 to the next even row in VRAM
	subs	r6, #1
	bne		1b
	//-------
	// Blit the odd rows
	//-------
	mov		r0, r5					// r0 = Physical VRAM start
	add		r0, #640
	add		r1, #192				// r1 = B800 segment + 8192
	mov		r6, #100				// Do 100 rows
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*8 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input

	and		r8, r4, #0xFF			// r8 = First input byte
	add		r8, r2, r8, lsl #3
	ldmia	r8, {r9, r10}
	and		r8, r4, #0xFF00
	add		r8, r2, r8, lsr #(8-3)
	ldmia	r8, {r11, r12}
	stmia	r0!, {r9-r12}

	and		r8, r4, #0xFF0000		// r8 = third input byte
	add		r8, r2, r8, lsr #(16-3)
	ldmia	r8, {r9, r10}
	and		r8, r4, #0xFF000000
	add		r8, r2, r8, lsr #(24-3)
	ldmia	r8, {r11, r12}
	stmia	r0!, {r9-r12}
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next odd row
	//-------
	add		r0, #640				// Point r0 to the next odd row in VRAM
	subs	r6, #1
	bne		1b
	//-------
	// Return
	//-------
	pop    {r4-r12, pc}
#else
	push    {r0, r4-r12, lr}
	ldr		r1, =CGA_B800			// r1 = B800 segment
	ldr		r2, =CGA6LUT			// r2 = CGA look-up-table for mode 6
	ldr		r1, [r1]
	//-------
	// Blit the even rows
	//-------
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*8 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input

	and		r5, r4, #0xFF			// r5 = first 8 pixels
	add		r5, r2, r5, lsl #4		// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r6, r7, r8, r9}	// r6..r9 = first 8 pixels
	and		r5, r4, #0xFF00			// r5 = next 8 pixels
	add		r5, r2, r5, lsr #(8-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r10, r11, r12, lr}	// r10..lr = next 8 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels

	and		r5, r4, #0xFF0000		// r5 = third 8 pixels
	add		r5, r2, r5, lsr #(16-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r6, r7, r8, r9}	// r6..r9 = first 8 pixels
	and		r5, r4, #0xFF000000		// r5 = last 8 pixels
	add		r5, r2, r5, lsr #(24-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r10, r11, r12, lr}	// r10..lr = next 8 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels
	
	subs	r3, #1
	bne		2b
	//-------
	// One row done, move to next even row
	//-------
	ldr		r3, [sp]				// r3 = VRAM start address
	add		r0, #(2*SCREEN_WIDTH-640)*2	// Point r0 to the next even row in VRAM
	sub		r3, r0, r3
	cmp		r3, #(SCREEN_WIDTH*200)*2	// Already at row 200?
	blt		1b						// Not yet, continue loop
	//-------
	// Blit the odd rows
	//-------
	ldr		r0, [sp]				// r3 = VRAM start address
	add		r1, #192				// r1 = B800 segment + 8192
	add		r0, #SCREEN_WIDTH*2		// r0 = Physical VRAM start
 
1:	mov		r3, #20					// r3 = row counter
	//-------
	// Each row consists of 20 words = 80 bytes = 80*4 pixels
	//-------
2:	ldr		r4, [r1], #4			// Get 4 bytes from input
	
	and		r5, r4, #0xFF			// r5 = first 8 pixels
	add		r5, r2, r5, lsl #4		// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r6, r7, r8, r9}	// r6..r9 = first 8 pixels
	and		r5, r4, #0xFF00			// r5 = next 8 pixels
	add		r5, r2, r5, lsr #(8-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r10, r11, r12, lr}	// r10..lr = next 8 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels

	and		r5, r4, #0xFF0000		// r5 = third 8 pixels
	add		r5, r2, r5, lsr #(16-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r6, r7, r8, r9}	// r6..r9 = first 8 pixels
	and		r5, r4, #0xFF000000		// r5 = last 8 pixels
	add		r5, r2, r5, lsr #(24-4)	// r4 = address into CGA6LUT, where we can load 4 words (8 pixels)
	ldmia	r5, {r10, r11, r12, lr}	// r10..lr = next 8 pixels
	stmia	r0!, {r6-r12, lr}		// Save 8 words = 16 pixels
	
	subs	r3, #1
	bne		2b

	//-------
	// One row done, move to next odd row
	//-------
	ldr		r3, [sp]				// r3 = VRAM start address
	add		r0, #(2*SCREEN_WIDTH-640)*2	// Point r0 to the next odd row in VRAM
	sub		r3, r0, r3
	cmp		r3, #(SCREEN_WIDTH*200)*2	// Already at row 200?
	blt		1b						// Not yet, continue loop
	//-------
	// Return
	//-------
	pop     {r0, r4-r12, pc}		// Return to caller
#endif


//-------------------- Port I/O handling -------------------------------

	//-------
	// 03D8h: Mode control register
	// Bit 5 = 1 to enable blinking, 0 to disable it.
	// Bit 4 = High-resolution graphics
	// Bit 3 = Enable video output
	// Bit 2 = Black and white. On an RGB monitor it has no effect except in the 320x200 graphics mode, when it selects a third palette (black/red/cyan/white).
	// Bit 1 = Graphics mode. If this bit is set, the display RAM will be treated as bitmap graphics rather than as text.
	// Bit 0 = High resolution
	//-------
out_3D8_CGA_Mode:
	bx		lr
	
	//-------
	// CGA Color Select Register
	// bit 0 = Blue border / background
	// bit 1 = Green border / background
	// bit 2 = Red border / background
	// bit 3 = Intense border / background
	// bit 4 = Intense colors
	// bit 5 = Active color set: 0 = red/green/yellow, 1 = cyan/magenta/white
	//
	// Note! This is called also from C code as "out_3D9_CGA_Color(0, value);"
	//-------
out_3D9_CGA_Color:
	ldr		r2, =CGA_3D9_value
	strb	r1, [r2]
	ldr		r2, =BG_PALETTE			// BG_PALETTE address
	tst		r1, #0x20				// Test the "active color set" bit
#ifdef RPi
	ldrh	r1, [r2, #2]			// BG_PALETTE[1] = RGB15(0,31,0) / RGB15(0,31,31)
#else	
	ldr		r1, [r2, #4]			// BG_PALETTE[1] = RGB15(0,31,0) / RGB15(0,31,31)
#endif	
	biceq	r1, #31					// Green
	orrne	r1, #31					// Cyan
#ifdef RPi
	strh	r1, [r2, #2]			// 1 = Cyan / Green
	ldrh	r1, [r2, #4]			// BG_PALETTE[2] = RGB15(31,0,0) / RGB15(31,0,31)
#else	
	str		r1, [r2, #4]			// 1 = Cyan / Green
	ldr		r1, [r2, #8]			// BG_PALETTE[2] = RGB15(31,0,0) / RGB15(31,0,31)
#endif	
	biceq	r1, #31					// Red
	orrne	r1, #31					// Magenta
#ifdef RPi
	strh	r1, [r2, #4]			// 2 = Magenta / Red
	ldrh	r1, [r2, #6]			// BG_PALETTE[2] = RGB15(31,0,0) / RGB15(31,0,31)
#else	
	str		r1, [r2, #8]			// 2 = Magenta / Red
	ldr		r1, [r2, #12]			// BG_PALETTE[3] = RGB15(31,31,0) / RGB15(31,31,31)
#endif
	biceq	r1, #31					// Yellow
	orrne	r1, #31					// White
#ifdef RPi
	strh	r1, [r2, #6]			// 3 = White / Yellow
	add		r2, #(2*256)
	mov		r1, #1
	strh	r1, [r2]				// Flag that the palette has changed
#else	
	str		r1, [r2, #12]			// 3 = White / Yellow
#endif	
	bx		lr

in_3D9_CGA_Color:
	ldr		r2, =CGA_3D9_value
	ldrb	r1, [r2]
	bic		eax,#0xFF				// Clear AL first
	orr		eax, r1					// Set AL value from r1
	bx		lr


//-------------------- Print a character to the screen -----------------
// Called by a C routine, r0 = char, r1 = color (0..3), r2 = number of times.
// Use the cursor position from BIOSData[0x50] and ROM8x8font.
// We can change r0..r3, must save other registers.
//
CharToCGA4Screen:
	push	{r4-r6}
	//-------
	// Save number of times to write character
	//-------
	ldr		r4,=INTVectors
	mov		r6, r2
	//-------
	// Calculate screen address to r4
	//-------
	ldr		r4, [r4]
	mov		r2, #0x450
	ldrh	r4,[r4, r2]				// r4 low byte = column, high byte = row
	and		r2, r4, #0xFF			// r2 = column
	ldr		r3, =CGA_B800
	lsr		r4, #8					// r4 = row (0..25), address = 4*80*row
	ldr		r3, [r3]
	mov		r5, #320
	mul		r4, r5, r4
	add		r3, r4					// r3 = Y address
	add		r4, r3, r2, lsl #1		// r4 = final CGA RAM start address
	//-------
	// Get address to font data
	//-------
	ldr		r3, =ROM8x8Font
	ldr		r5, =BIOS_F000
	ldr		r3, [r3]
	ldr		r5, [r5]
	bic		r3, #0xFF000000
	add		r3, r5
	ldrb	r0, [r3, r0, lsl #3]!	// r3 = font position, r0 = first row of font
2:	mov		r5, #8
	//-------
	// Each font byte is expanded to 2 bytes into the CGA VRAM.
	//-------
1:	mov		r2, #0
	tst		r0, #0x80
	orrne	r2, r1, lsl #6
	tst		r0, #0x40
	orrne	r2, r1, lsl #4
	tst		r0, #0x20
	orrne	r2, r1, lsl #2
	tst		r0, #0x10
	orrne	r2, r1
	tst		r0, #0x08
	orrne	r2, r1, lsl #(6+8)
	tst		r0, #0x04
	orrne	r2, r1, lsl #(4+8)
	tst		r0, #0x02
	orrne	r2, r1, lsl #(2+8)
	tst		r0, #0x01
	orrne	r2, r1, lsl #8
	strh	r2, [r4]
	//-------
	// Next row of font
	//-------
	tst		r5, #1					// Odd/even row in CGA VRAM?
	addeq	r4, #0x2000
	subne	r4, #0x2000
	addne	r4, #80
	ldrb	r0, [r3, #1]!			// r3 = font position, r0 = second row of font
	subs	r5, #1
	bne		1b
	//-------
	// One character done, test if we need to write more.
	//-------
	subs	r6, #1
	beq		1f
	sub		r3, #8					// Back to first row of font
	ldrb	r0, [r3]				// r0 = first row of font
	sub		r4, #(4*80)
	add		r4, #2					// Next column
	b		2b
1:	pop		{r4-r6}
	bx		lr						// Return to caller

CharToCGA6Screen:
	push	{r4-r6}
	//-------
	// Save number of times to write character
	//-------
	ldr		r4,=INTVectors
	mov		r6, r2
	//-------
	// Calculate screen address to r4
	//-------
	ldr		r4, [r4]
	mov		r2, #0x450
	ldrh	r4,[r4, r2]				// r4 low byte = column, high byte = row
	and		r2, r4, #0xFF			// r2 = column
	ldr		r3, =CGA_B800
	lsr		r4, #8					// r4 = row (0..25), address = 4*80*row
	ldr		r3, [r3]
	mov		r5, #(4*80)
	mul		r4, r5, r4
	add		r3, r4					// r3 = Y address
	add		r4, r3, r2				// r4 = final CGA RAM start address
	//-------
	// Get address to font data
	//-------
	ldr		r3, =ROM8x8Font
	ldr		r5, =BIOS_F000
	ldr		r3, [r3]
	ldr		r5, [r5]
	bic		r3, #0xFF000000
	add		r3, r5
	ldrb	r0, [r3, r0, lsl #3]!	// r3 = font position, r0 = first row of font
2:	mov		r5, #8
	//-------
	// Each font byte corresponds to a byte in the CGA VRAM.
	//-------
1:	strb	r0, [r4]
	//-------
	// Next row of font
	//-------
	tst		r5, #1					// Odd/even row in CGA VRAM?
	addeq	r4, #0x2000
	subne	r4, #0x2000
	addne	r4, #80
	ldrb	r0, [r3, #1]!			// r3 = font position, r0 = second row of font
	subs	r5, #1
	bne		1b
	//-------
	// One character done, test if we need to write more.
	//-------
	subs	r6, #1
	beq		1f
	sub		r3, #8					// Back to first row of font
	ldrb	r0, [r3]				// r0 = first row of font
	sub		r4, #(4*80)
	add		r4, #1					// Next column
	b		2b
1:	pop		{r4-r6}
	bx		lr						// Return to caller


//-------------------- Draw a pixel to the screen ----------------------
// Called by a C routine, r0 = pixel color, r1 = X, r2 = Y
// We can change r0..r3, must save other registers.
//
PixelToCGA4Screen:
	ldr		r3, =CGA_B800
	ldr		r3, [r3]
	movs	r2, r2, lsr #1
	addcs	r3, #0x2000				// Odd row, on second video plane
	mov		r12, #80
	mul		r12, r2, r12			// r12 = 80*(Y/2)
	add		r3, r12
	add		r3, r1, lsr #2			// r3 = (X/4)+80*(Y/2) = byte in CGA RAM to change
	and		r1, #3					// r1 = pixel mask
	add		r1, r1					// r1 = 0, 2, 4, 6 = mask shift value
	tst		r0, #0x80				// Should we XOR the pixel?
	and		r0, #3					// r0 = color (0..3)
	lsl		r0, #6
	ldrb	r2, [r3]				// Get byte from CGA VRAM
	mov		r12, #0xC0
	mov		r12, r12, lsr r1
	biceq	r2, r12					// Clear the current pixel value
	orreq	r2, r0, lsr r1			// Replace it with color in r0
	eorne	r2, r0, lsr r1			// XOR it with color in r0
	strb	r2, [r3]				// Save byte to CGA VRAM
	bx		lr

PixelToCGA6Screen:
	ldr		r3, =CGA_B800
	ldr		r3, [r3]
	movs	r2, r2, lsr #1
	addcs	r3, #0x2000				// Odd row, on second video plane
	mov		r12, #80
	mul		r12, r2, r12			// r12 = 80*(Y/2)
	add		r3, r12
	add		r3, r1, lsr #3			// r3 = (X/8)+80*(Y/2) = byte in CGA RAM to change
	and		r1, #7					// r1 = pixel mask shift value
	tst		r0, #0x80				// Should we XOR the pixel?
	and		r0, #1					// r0 = color (0..1)
	lsl		r0, #7
	ldrb	r2, [r3]				// Get byte from CGA VRAM
	mov		r12, #0x80
	mov		r12, r12, lsr r1
	biceq	r2, r12					// Clear the current pixel value
	orreq	r2, r0, lsr r1			// Replace it with color in r0
	eorne	r2, r0, lsr r1			// XOR it with color in r0
	strb	r2, [r3]				// Save byte to CGA VRAM
	bx		lr

//-------------------- Read a pixel from the screen --------------------
// Called by a C routine, r0 = X, r1 = Y
// We can change r0..r3, must save other registers.
//
	
PixelFromCGA4Screen:
	ldr		r3, =CGA_B800
	ldr		r3, [r3]
	movs	r2, r1, lsr #1
	addcs	r3, #0x2000				// Odd row, on second video plane
	mov		r12, #80
	mul		r12, r2, r12			// r12 = 80*(Y/2)
	add		r3, r12
	add		r3, r0, lsr #2			// r3 = (X/4)+80*(Y/2) = byte in CGA RAM to change
	and		r1, r0, #3				// r1 = pixel mask
	add		r1, r1					// r1 = 0, 2, 4, 6 = mask shift value
	ldrb	r2, [r3]				// Get byte from CGA VRAM
	mov		r12, #0xC0
	mov		r12, r12, lsr r1
	and		r2, r12					// Leave only the current pixel value
	rsb		r1, #6
	mov		r0, r2, lsr r1			// Return the pixel color
	bx		lr

PixelFromCGA6Screen:
	ldr		r3, =CGA_B800
	ldr		r3, [r3]
	movs	r2, r1, lsr #1
	addcs	r3, #0x2000				// Odd row, on second video plane
	mov		r12, #80
	mul		r12, r2, r12			// r12 = 80*(Y/2)
	add		r3, r12
	add		r3, r0, lsr #3			// r3 = (X/8)+80*(Y/2) = byte in CGA RAM to change
	and		r1, r0, #7				// r1 = pixel mask shift value
	ldrb	r2, [r3]				// Get byte from CGA VRAM
	mov		r12, #0x80
	mov		r12, r12, lsr r1
	and		r2, r12					// Leave only the current pixel value
	rsb		r1, #7
	mov		r0, r2, lsr r1			// Return the pixel color
	bx		lr

	.data
	.align 2

CGA_3D9_value:
	.byte	0

	.align 2

#ifdef RPi

.macro  buildlut from=0, to=63
	.word   (((\from)&0x03)<<24) + (((\from)&0x0C)<<(16-2)) + (((\from)&0x30)<<(8-4)) + (((\from)&0xC0)>>(6))
	.if     \to-\from
	buildlut "(\from+1)",\to
	.endif
.endm

CGA4LUT:	
	buildlut 0 63
	buildlut 64 127
	buildlut 128 191
	buildlut 192 255

.purgem buildlut
.macro  buildlut from=0, to=63
	.word   (((\from)&0x80)>>7) + (((\from)&0x40)<<(8-6)) + (((\from)&0x20)<<(16-5)) + (((\from)&0x10)<<(24-4))
	.word	(((\from)&0x8)>>3) + (((\from)&0x4)<<(8-2)) + (((\from)&0x2)<<(16-1)) + (((\from)&0x1)<<(24-0))
	.if     \to-\from
	buildlut "(\from+1)",\to
	.endif
.endm

CGA6LUT:	
	buildlut 0 63
	buildlut 64 127
	buildlut 128 191
	buildlut 192 255

#else
	
	//------
	// Build look-up tables so that for each input byte we can create the required output RGB15 pixels.
	// In mode 04 each input byte has 4 pixels, so table has 2 words per input byte.
	// bit mask:
	//	00 = RGB15(0,0,0)
	//	01 = RGB15(0,31,0)
	//	10 = RGB15(31,0,0)
	//	11 = RGB15(31,31,0)
	//------

#define RGB15(r, g, b)   (((r)<<11)|((g)<<6)|(b))
#define	CGACOLOR0(n) (((((n)>>1)&1)*RGB15(31,0,0))|(((n)&1)*RGB15(0,31,0)))

.macro  buildlut from=0, to=63
	.word   CGACOLOR0(((\from)&0xC0)>>6) | (CGACOLOR0(((\from)&0x30)>>4)<<16)
	.word   CGACOLOR0(((\from)&0xC)>>2) | (CGACOLOR0(((\from)&0x3)>>0)<<16)
	.if     \to-\from
	buildlut "(\from+1)",\to
	.endif
.endm

CGA4LUT:	
	buildlut 0 63
	buildlut 64 127
	buildlut 128 191
	buildlut 192 255

#define	CGACOLOR1(n) (((((n)>>1)&1)*RGB15(31,0,31))|(((n)&1)*RGB15(0,31,31)))

.purgem buildlut
.macro  buildlut from=0, to=63
	.word   CGACOLOR1(((\from)&0xC0)>>6) | (CGACOLOR1(((\from)&0x30)>>4)<<16)
	.word   CGACOLOR1(((\from)&0xC)>>2) | (CGACOLOR1(((\from)&0x3)>>0)<<16)
	.if     \to-\from
	buildlut "(\from+1)",\to
	.endif
.endm

	buildlut 0 63
	buildlut 64 127
	buildlut 128 191
	buildlut 192 255

	//------
	// In mode 06 each input byte has 8 pixels, so table has 4 words per input byte.
	//------

.purgem buildlut
.macro  buildlut from=0, to=63
	.word   ((((\from)&0x80)>>7)*0xFFFF) | ((((\from)&0x40)>>6)*0xFFFF0000)
	.word	((((\from)&0x20)>>5)*0xFFFF) | ((((\from)&0x10)>>4)*0xFFFF0000)
	.word	((((\from)&0x8)>>3)*0xFFFF) | ((((\from)&0x4)>>2)*0xFFFF0000)
	.word	((((\from)&0x2)>>1)*0xFFFF) | ((((\from)&0x1)>>0)*0xFFFF0000)
	.if     \to-\from
	buildlut "(\from+1)",\to
	.endif
.endm

CGA6LUT:	
	buildlut 0 63
	buildlut 64 127
	buildlut 128 191
	buildlut 192 255

#endif
