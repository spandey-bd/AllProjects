//=============================================================================
// MCGA.S
//
// This file contains routines to handle MCGA graphics (320x200 256-color)
// and SVGA graphics (640x400 or 640x480 256-color) routines.
//
//	screen_copy_13()			= Blit 320x200x256 MCGA screen
//	screen_copy_SVGA()			= Blit 640x400x256 or 640x480x256 SVGA screen
//	CharToMCGAScreen			= Draw a character to MCGA 256-color screen (mode 13)
//	PixelToMCGAScreen			= int10h helper code for drawing a pixel (mode 13)
//	PixelFromMCGAScreen			= int10h helper code for reading a pixel (mode 13)
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
	.file	"MCGA.s"

	.global	CharToMCGAScreen
	.global	PixelToMCGAScreen
	.global	PixelFromMCGAScreen
	
	.text
	.align	2
	.code	32
	.arm

#include "defines.inc"
#include "macros.inc"

#define	SCREEN_WIDTH	1024

// ------------------- COPY TO SCREEN ----------------------------------
// screen_copy_13(pixels, &render)
//	r0 = pixels = pointer to offscreen screen buffer 
//	r1 = &render = pointer to RenderData structure to be filled
//
	.global screen_copy_13
screen_copy_13:
	push    {r4-r11, lr}
#ifdef RPi
	//-------
	// Setup the RenderData structure
	//-------
	ldr		r2, =VGAOffset
	ldr		r5,=EGAVGA_A000				// VGA screen start
	ldrb	r4, [r2]					// r4 = VGA offset (pitch) value
//	mov		r4, #320					// render.pitch
	ldr		r5, [r5]					// render.imgtop = Direct MCGA emulated VRAM area
	lsl		r4, #3						// r4 = render.pitch
	mov		r6, #200					// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	//------
	// BC Racers special handling
	//------
	cmp		r4, #320
	bge		1f
	ldr		r0, =MaxScreenX
	str		r4, [r0]					// Save witdth (less than 320) to MaxScreenX
	ldrb	r3, [r2, #-1]				// r3 = Vertical Display End value
	cmp		r3, #0xBF					// r3 == 0xBF => VDE = 256 + 191 = 447 => MaxScreenY = 224
	bne		1f
	ldr		r0, =MaxScreenY
	mov		r6, #224
	str		r6, [r0]
1:	stmia	r1, {r4-r8}					// Save to renderdata
#else
	ldr		r1,=EGAVGA_A000			// VGA screen start
	ldr		r3, =BG_PALETTE			// Palette lookup table address
	ldr		r1, [r1]
	//-------
	// Calculate the display pitch value (vga.draw.address_add in DOSBox)
	// from CRTC registers index 0x13
	//-------
	ldr		r4, =VGAOffset
	ldrb	r11, [r4]
	lsl		r11, #3
	sub		r11, #320
	
	mov		r2, #200				// Copy 200 rows worth of data
1:	mov		r12, #40				// Copy 40*8 = 320 pixels per row
	
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
	and		r10, r4, #0xFF000000
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(16-2)]
	ldr		r10, [r3, r10, lsr #(24-2)]
	orr		r7, r9, lsl #16
	orr		r8, r10, lsl #16

	stmia	r0!, {r5-r8}			// Write 4 words = 16 bytes = 8 pixels	

	subs	r12, #1
	bgt		2b

	//-------
	// One screen row handled, prepare for the next row
	//-------		
	add		r0, #(SCREEN_WIDTH-320)*2 // Point r0 to the next row in VRAM
	add		r1, r11					// Adjust input position by the VGA offset value
    subs    r2, #1
    bgt     1b
#endif    
	pop     {r4-r11, pc}			// Return to caller

// ------------------- COPY TO SCREEN ----------------------------------
// screen_copy_13(pixels, &render)
//	r0 = pixels = pointer to offscreen screen buffer 
//	r1 = &render = pointer to RenderData structure to be filled
//
	.global screen_copy_SVGA
screen_copy_SVGA:
	push    {r4-r11, lr}
	//-------
	// Get the current display mode, to select between 640x400 and 640x480 modes.
	//-------
	ldr		r4, =BIOSData
	ldr		r5, =EGAVGA_A000			// VGA screen start
	ldr		r4, [r4]
	ldrb	r11, [r4, #0x49]
	cmp		r11, #0x1D
#ifdef RPi
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #640					// render.pitch
	ldr		r5, [r5]					// render.imgtop = Direct SVGA emulated VRAM area
	moveq	r6, #480					// render.toprows = Copy 480 rows worth of data
	movne	r6, #400					// render.toprows = Copy 400 rows worth of data
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
#else
	moveq	r2, #480				// Copy 480 rows worth of data
	movne	r2, #400				// Copy 400 rows worth of data
	ldr		r3, =BG_PALETTE			// Palette lookup table address
	ldr		r1, [r5]
	
1:	mov		r12, #80				// Copy 80*8 = 640 pixels per row
	
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
	and		r10, r4, #0xFF000000
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(16-2)]
	ldr		r10, [r3, r10, lsr #(24-2)]
	orr		r7, r9, lsl #16
	orr		r8, r10, lsl #16

	stmia	r0!, {r5-r8}			// Write 4 words = 16 bytes = 8 pixels	

	subs	r12, #1
	bgt		2b

	//-------
	// One screen row handled, prepare for the next row
	//-------		
	add		r0, #(SCREEN_WIDTH-640)*2 // Point r0 to the next row in VRAM
    subs    r2, #1
    bgt     1b
#endif    
	pop     {r4-r11, pc}			// Return to caller
	
//-------------------- Print a character to the screen -----------------
// Called by a C routine, r0 = char, r1 = color (0..16)|page<<8, r2=number of times.
// Use the cursor position from BIOSData[0x50] and ROM8x8font.
// We can change r0..r3, must save other registers.
//

.macro put_font_row
	tst		r0, #0x80
	strneb	r1, [r4]
	tst		r0, #0x40
	strneb	r1, [r4, #1]
	tst		r0, #0x20
	strneb	r1, [r4, #2]
	tst		r0, #0x10
	strneb	r1, [r4, #3]
	tst		r0, #0x8
	strneb	r1, [r4, #4]
	tst		r0, #0x4
	strneb	r1, [r4, #5]
	tst		r0, #0x2
	strneb	r1, [r4, #6]
	tst		r0, #0x1
	strneb	r1, [r4, #7]
	add		r4, r2
.endm

CharToMCGAScreen:
	push	{r4-r6}
	//-------
	// Setup r6, r1 and r2
	//-------
	ldr		r4,=INTVectors
	mov		r6, r2					// r6 = counter
	and		r1, #0xFF				// r1 = color
	//-------
	// Calculate screen address to r4
	//-------
	ldr		r4,[r4]
	mov		r3, #0x450
	ldrh	r4,[r4, r3]				// r4 low byte = column, high byte = row
	and		r3, r4, #0xFF			// r3 = column (0..39)
	lsr		r4, #8					// r4 = row (0..24)
	mov		r5, #320*8
	mul		r4, r5, r4				// r4 = 40*8*8*row
	add		r4, r3, lsl #3			// r4 = 40*8*row + 8*column
	ldr		r3, =EGAVGA_A000
	ldr		r3, [r3]
	add		r4, r3
	mov		r2, #320
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
	//-------
	// Write 8x8 pixels
	//	r0 = row of font data
	//	r1 = color
	//	r2 = free
	//	r3 = font pointer
	//	r4 = VRAM pointer
	//	r5 = free
	//	r6 = number of times to write character
	//-------
2:
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	//-------
	// One character done, test if we need to write more.
	//-------
	subs	r6, #1
	popeq	{r4-r6}
	bxeq	lr						// Return to caller
	sub		r3, #7					// Back to first row of font
	sub		r4, r2, lsl #3
	add		r4, #8					// Next column
	ldrb	r0, [r3]				// Get first row of font
	b		2b

//-------------------- Draw a pixel to the screen ----------------------
// Called by a C routine, r0 = pixel color, r1 = X, r2 = Y
// We can change r0..r3 and r12, must save other registers.
//

PixelToMCGAScreen:
	//-------
	// Calculate screen address to r3
	//-------
	mov		r3, #320
	mul		r3, r2, r3
	ldr		r2, =EGAVGA_A000
	ldr		r2, [r2]
	add		r3, r1					// r3 = 320*Y + X
	add		r3, r2
	//-------
	// Write the pixel
	//-------
	strb	r0, [r3]
	bx		lr

//-------------------- Read a pixel from the screen --------------------
// Called by a C routine, r0 = X, r1 = Y
// We can change r0..r3 and r12, must save other registers.
//

PixelFromMCGAScreen:
	//-------
	// Calculate screen address to r3
	//-------
	mov		r3, #320
	mul		r3, r1, r3
	ldr		r2, =EGAVGA_A000
	ldr		r2, [r2]
	add		r3, r0					// r3 = 320*Y + X
	add		r3, r2
	//-------
	// Read the pixel
	//-------
	ldrb	r0, [r3]
	bx		lr

#ifdef RPi

//---- Convert an image from 32-bit to 16-bit depth, swap Y coordinate.
// r0 = pointer to output data
// r1 = pointer to input data
// r2 = width of input
// r3 = height of input
//
// 
	.global	From32To16Bit
From32To16Bit:
	push	{r4-r12,lr}
	//-------
	// Prepare the loop counters etc
	//-------
	mov		lr, r0					// Remember original output start address
	lsl		r3, #1					// 2 bytes per pixel in output
	mla		r0, r2, r3, lr			// r0 = output pointer + width*2*height
	sub		r0, r2, lsl #1			// r0 = output pointer + width**(height-1) = last row start address
2:	mov		r4, r2					// r4 = number of pixels to do per row
	//-------
	// Convert one full row from 32 to 16 bits.
	//-------
1:	ldmia	r1!, {r9-r12}			// Read 4 pixels
	//-------
	//	r0 = src0 & 0xff;
	//	g0 = (src0 >> 8) & 0xff;
	//	b0 = (src0 >> 16) & 0xff;
	//	r1 = src1 & 0xff;
	//	g1 = (src1 >> 8) & 0xff;
	//	b1 = (src1 >> 16) & 0xff;
	//	dest[x] = ((r1 >> 3) << 27) | ((g1 >> 2) << 21) | ((b1 >> 3) << 16)
	//	| ((r0 >> 3) << 11) | ((g0 >> 2) << 5) | ((b0 >> 3) << 0);
	//-------
	and		r8, r9, #(31<<(16+3))
	mov		r7, r8, lsr #(16+3)		// r7 = ((b0 >> 3) << 0)
	and		r8, r9, #(63<<(8+2))
	orr		r7, r8, lsr #(8+2-5)	// r7 = ((g0 >> 2) << 5) | ((b0 >> 3) << 0);
	and		r8, r9, #(31<<3)
	orr		r7, r8, lsl #(11-3)		// r7 = ((r0 >> 3) << 11) | ((g0 >> 2) << 5) | ((b0 >> 3) << 0);
	//
	pld		[r1]
	//
	and		r8, r10, #(31<<(16+3))
	orr		r7, r8, lsr #(16+3-16)
	and		r8, r10, #(63<<(8+2))
	orr		r7, r8, lsl #(21-(8+2))
	and		r8, r10, #(31<<3)
	orr		r7, r8, lsl #(27-3)
	//
	and		r9, r11, #(31<<(16+3))
	mov		r8, r9, lsr #(16+3)		// r8 = ((b0 >> 3) << 0)
	and		r9, r11, #(63<<(8+2))
	orr		r8, r9, lsr #(8+2-5)	// r8 = ((g0 >> 2) << 5) | ((b0 >> 3) << 0);
	and		r9, r11, #(31<<3)
	orr		r8, r9, lsl #(11-3)		// r8 = ((r0 >> 3) << 11) | ((g0 >> 2) << 5) | ((b0 >> 3) << 0);
	//
	and		r9, r12, #(31<<(16+3))
	orr		r8, r9, lsr #(16+3-16)
	and		r9, r12, #(63<<(8+2))
	orr		r8, r9, lsl #(21-(8+2))
	and		r9, r12, #(31<<3)
	orr		r8, r9, lsl #(27-3)
	//-------
	// Write the result
	//-------
	subs	r4, #4					// 4 pixels done
	stmia	r0!, {r7, r8}
	//-------
	// Check for the end of the row
	//-------
	bgt		1b						// Back to loop if not yet whole row done
	//-------
	// Prepare for the next row
	//-------
	sub		r0, r2, lsl #2			// Subtract 2 * width == go to previous row.
	cmp		r0, lr					// Below the start of the output address?
	bge		2b						// Nope, so back to loop.
	//-------
	// Return to caller
	//-------
	pop		{r4-r12, pc}


#endif
