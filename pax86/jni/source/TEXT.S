//=============================================================================
// TEXT.S
//
// This file contains routines to handle text mode graphics blitting
// (generating 640x400 pixel graphics image from 80x25 character text screen).
// This file also has rpoutines for text mode blinking cursor emulation.
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

#ifdef RPi
	.arch	armv6
#else	
	.arch	armv5te
#endif	
	.file	"TEXT.s"

	.global	text_pos_by_page_cursor
	.global	text_pos_by_page_col_row
	.global screen_copy_03
	
	.text
	.align	2
	.code	32
	.arm

#include "defines.inc"
#include "macros.inc"

#define	SCR_MODE	0x49
#define	SCR_COLS	0x4A
#define	SCR_PAGELEN	0x4C
#define	SCR_OFFS_LO	0x4E
#define	SCR_OFFS_HI	0x4F
#define	CURSOR_COL	0x50
#define	CURSOR_ROW	0x51
#define	SCR_PAGE	0x62
#define	SCR_ROWS	0x84

#ifdef RPi
#define	SCREEN_WIDTH	640
#else
#define	SCREEN_WIDTH	1024
#endif

#define RGB565(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))

// ------------- Helper functions for address calculations -------------
//

	//-------
	// Input:
	//	r0 = page
	// Output:
	//	r0 = offset into B800 segment
	// Can change:
	//	r0..r3, r12
	//-------
text_pos_by_page_cursor:
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrh	r1, [r3, #SCR_PAGELEN]			// r1 = length of each page in B800 segment
	add		r3, #CURSOR_COL
	mul		r2, r1, r0						// r2 = Start offset of the requested page
	ldrb	r1,[r3, r0, lsl #1]				// r1 = Cursor column on this page
	add		r3, #1
	ldrb	r0,[r3, r0, lsl #1]				// r0 = Cursor row on this page
	sub		r3, #CURSOR_ROW
	add		r2, r1, lsl #1					// r2 = page offset + 2*column
	ldrh	r1, [r3, #SCR_COLS]
	mul		r3, r1, r0
	add		r0, r2, r3, lsl #1				// r0 = page offset + 2*column + 2*(screen_columns*cursor_row)
	bx		lr

	//-------
	// Input:
	//	r0 = page, r1 = column, r2 = row
	// Output:
	//	r0 = offset into B800 segment
	// Can change:
	//	r0..r3, r12
	//-------
text_pos_by_page_col_row:
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrh	r12, [r3, #SCR_PAGELEN]			// r12 = length of each page in B800 segment
	mul		r0, r12, r0						// r0 = Start offset of the requested page
	add		r0, r1, lsl #1					// r0 = page offset + 2*column
	ldrh	r1, [r3, #SCR_COLS]
	mul		r3, r1, r2
	add		r0, r3, lsl #1					// r0 = page offset + 2*column + 2*(screen_columns*row)
	bx		lr

// ------------------- COPY TO SCREEN ----------------------------------
// Input:
//	r0 = pixels = pointer to offscreen screen buffer 
//	r1 = &render = pointer to RenderData structure to be filled
//
#define	SP_ROWEND	0
#define	SP_NEXTROW	4
#define	SP_SCRCOLS	8
#define	SP_ENDPOS	12

screen_copy_03:
	push    {r4-r12,lr}
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #SCREEN_WIDTH			// render.pitch
	mov		r5, r0						// render.imgtop
	mov		r6, #(25*16)				// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
	//-------
	// Setup pointers and screen dimensions
	//-------
	ldr		r8, =BIOSData
	ldr		r11,=CGA_B800
	ldr		r8, [r8]
	ldr		r11, [r11]
	ldr		r10,=TEXTBuffer
	ldrh	r3, [r8, #SCR_OFFS_LO]	// r3 = offset of current video page
	ldrb	r6, [r8, #SCR_COLS]		// Get number of screen columns
	ldrb	r2, [r8, #SCR_ROWS]		// Get number of screen rows
	add		r11, r3
#ifdef RPi
	add		r8, r0, #(25*SCREEN_WIDTH*16) // r8 = end position in VRAM
#else
	add		r8, r0, #(25*SCREEN_WIDTH*2*16) // r8 = end position in VRAM
#endif
	cmp		r2, #24					// 25 screen rows?
	ldreq	r12, =copy_font_sub_8x16
	ldrne	r12, =copy_font_sub_8x8
	moveq	r5, #SCREEN_WIDTH*16
	movne	r5, #SCREEN_WIDTH*8
	add		r4, r11, r6, lsl #1		// r4 = current screen row end position in input
	push	{r4, r5, r6, r8}		// r4=SP_ROWEND, r5=SP_NEXTROW, r6=SP_SCRCOLS, r8=SP_ENDPOS
	mov		r8, r0					// r8 = VRAM output position
.copy_loop:
	ldmia	r11!, {r0-r3}			// r0..r3 = actual screen data
	ldmia	r10!, {r4-r7}			// r4..r7 = dirty buffer values
	//-------
	// First two characters have stayed the same?
	//-------
	cmp		r0, r4					
#ifdef RPi
	addeq	r8, #(2*8)
#else	
	addeq	r8, #(2*2*8)
#endif	
	beq		1f
	//-------
	// Update these two characters on the physical screen
	//-------
	str		r0, [r10, #-(4*4)]		// First update the dirty buffer
	//-------
	// r0 = char0 code, char0 attribute, char1 code, char1 attribute
	// r1..r3 = char2, char3, char4, char5, char6, char7
	// r4 = free
	// r5..r7 = char2, char3, char4, char5, char6, char7 dirty buffer values
	// r8 = VRAM position
	// r9 = free
	// r10 = dirty buffer position
	// r11 = logical B800 screen position
	// r12 = font_sub call address
	//-------
	push	{r5}
	push	{r0}
	blx		r12
	pop		{r0}
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	lsr		r0, #16
	blx		r12
	pop		{r5}
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	//-------
	// Second two characters have stayed the same?
	//-------
1:	cmp		r1, r5
#ifdef RPi
	addeq	r8, #(2*8)
#else	
	addeq	r8, #(2*2*8)
#endif	
	beq		2f
	//-------
	// Update these two characters on the physical screen
	//-------
	str		r1, [r10, #-(3*4)]		// First update the dirty buffer
	mov		r0, r1
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	mov		r0, r1, lsr #16
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	//-------
	// Third two characters have stayed the same?
	//-------
2:	cmp		r2, r6
#ifdef RPi
	addeq	r8, #(2*8)
#else	
	addeq	r8, #(2*2*8)
#endif	
	beq		3f
	//-------
	// Update these two characters on the physical screen
	//-------
	str		r2, [r10, #-(2*4)]		// First update the dirty buffer
	mov		r0, r2
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	mov		r0, r2, lsr #16
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	//-------
	// Fourth two characters have stayed the same?
	//-------
3:	cmp		r3, r7
#ifdef RPi
	addeq	r8, #(2*8)
#else	
	addeq	r8, #(2*2*8)
#endif	
	beq		4f
	//-------
	// Update these two characters on the physical screen
	//-------
	str		r3, [r10, #-(1*4)]		// First update the dirty buffer
	mov		r0, r3
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	mov		r0, r3, lsr #16
	blx		r12
#ifdef RPi
	add		r8, #8
#else	
	add		r8, #2*8
#endif
	//-------
	// Check if we have handled one full screen row
	//-------
4:	ldr		r4, [sp, #SP_ROWEND]
	cmp		r11, r4
	bne		.copy_loop				// Not a full row yet, continue
	//-------
	// One row handled, adjust for the next screen row
	//-------
	ldmia	sp, {r4, r5, r6, r7}	// r4=SP_ROWEND, r5=SP_NEXTROW, r6=SP_SCRCOLS, r7=SP_ENDPOS
#ifdef RPi
	add		r8, r5					// Next row in VRAM
#else
	add		r8, r5, lsl #1			// Next row in VRAM
#endif	
	cmp		r7, r8					// All 25/50 rows handled?
#ifdef RPi
	sub		r8, r6, lsl #3			// Clear the current column offset
#else
	sub		r8, r6, lsl #4			// Clear the current column offset
#endif	
	add		r4, r6, lsl #1			// Prepare for the next row
	str		r4, [sp, #SP_ROWEND]
	bgt		.copy_loop				// and continue copying, if not yet all 25 rows handled.
	add		sp, #4*4				// Remove pushed r4, r5, r6 and r8 from stack
	pop		{r4-r12,lr}
	bx		lr

.macro put_font_row
	tst		r0, #0x80
#ifdef RPi	
	strneb	r4, [r8]
	streqb	r5, [r8]
#else
	strneh	r4, [r8]
	streqh	r5, [r8]
#endif	
	tst		r0, #0x40
#ifdef RPi	
	strneb	r4, [r8, #1]
	streqb	r5, [r8, #1]
#else
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
#endif	
	tst		r0, #0x20
#ifdef RPi	
	strneb	r4, [r8, #2]
	streqb	r5, [r8, #2]
#else
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
#endif	
	tst		r0, #0x10
#ifdef RPi	
	strneb	r4, [r8, #3]
	streqb	r5, [r8, #3]
#else
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
#endif	
	tst		r0, #0x8
#ifdef RPi	
	strneb	r4, [r8, #4]
	streqb	r5, [r8, #4]
#else
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
#endif	
	tst		r0, #0x4
#ifdef RPi	
	strneb	r4, [r8, #5]
	streqb	r5, [r8, #5]
#else
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
#endif	
	tst		r0, #0x2
#ifdef RPi	
	strneb	r4, [r8, #6]
	streqb	r5, [r8, #6]
#else
	strneh	r4, [r8, #12]
	streqh	r5, [r8, #12]
#endif	
	tst		r0, #0x1
#ifdef RPi	
	strneb	r4, [r8, #7]
	streqb	r5, [r8, #7]
	add		r8, #SCREEN_WIDTH
#else
	strneh	r4, [r8, #14]
	streqh	r5, [r8, #14]
	add		r8, #SCREEN_WIDTH*2
#endif	
.endm

	//-------
	// Input:
	//	r0 low byte = character
	//	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	//	r8 = address to VRAM
	// Destroys:
	//	r0, r4, r5, r9
	//-------
copy_font_sub_8x16:
	//-------
	// Make r4 low word = foreground color and r5 low word = background color
	// Make r9 = pointer to the font data and load r0 = first 4 font rows
	//-------
	ldr		r9, =ROM8x16Font
	ldr		r5, =BIOS_F000
	ldr		r9, [r9]
	ldr		r5, [r5]
	bic		r9, #0xFF000000
	add		r9, r5
#ifdef RPi
	mov		r4, r0, lsr #8		// Foreground color
	mov		r5, r0, lsr #(8+4)	// Background color
	and		r4, #15
	and		r5, #15
#else
	push	{r9}
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	ldr		r4, [r9, r4, lsr #(8-2)]
	ldr		r5, [r9, r5, lsr #(8+4-2)]
	//-------
	// Make r9 = pointer to the font data and load r0 = first 4 font rows
	//-------
	pop		{r9}
#endif	
	and		r0, #0xFF
	cmp		r0, #0x20				// Are we to write a blank (' ')?
	beq		1f						// Yep, perform it separately
	ldr		r0, [r9, r0, lsl #4]!	// r0 = first 4 pixel rows of the font
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	//-------
	// Get the next 4 rows of font data
	//-------
	ldr		r0, [r9, #4]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	//-------
	// Get the next 4 rows of font data
	//-------
	ldr		r0, [r9, #8]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	//-------
	// Get the last 4 rows of font data
	//-------
	ldr		r0, [r9, #12]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
#ifdef RPi
	sub		r8, #(16*SCREEN_WIDTH)
#else
	sub		r8, #(16*SCREEN_WIDTH*2)
#endif
	bx		lr
	//-------
	// Write a 8x16 byte block of background color.
	//-------
1:
	//-------
	// Write 16 rows of 8 bytes of background color
	//-------
#ifdef RPi	
	orr		r5, r5, lsl #8
	orr		r5, r5, lsl #16
#else
	orr		r5, r5, lsl #16
	mov		r0, r5
	mov		r4, r5
	mov		r9, r5
#endif	
.rept 8	
#ifdef RPi
	str		r5, [r8]
	str		r5, [r8, #4]
	str		r5, [r8, #SCREEN_WIDTH]
	str		r5, [r8, #SCREEN_WIDTH+4]
	add		r8, #SCREEN_WIDTH*2
#else
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
#endif
.endr		
#ifdef RPi
	sub		r8, #(16*SCREEN_WIDTH)
#else
	sub		r8, #(16*SCREEN_WIDTH*2)
#endif
	bx		lr

	//-------
	// Input:
	//	r0 low byte = character
	//	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	//	r8 = address to VRAM
	// Destroys:
	//	r0, r4, r5, r9
	//-------
copy_font_sub_8x8:
	//-------
	// Make r4 low word = foreground color and r5 low word = background color
	// Make r9 = pointer to the font data and load r0 = first 4 font rows
	//-------
	ldr		r9, =ROM8x8Font
	ldr		r5, =BIOS_F000
	ldr		r9, [r9]
	ldr		r5, [r5]
	bic		r9, #0xFF000000
	add		r9, r5
#ifdef RPi
	mov		r4, r0, lsr #8		// Foreground color
	mov		r5, r0, lsr #(8+4)	// Background color
	and		r4, #15
	and		r5, #15
#else
	push	{r9}
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	ldr		r4, [r9, r4, lsr #(8-2)]
	ldr		r5, [r9, r5, lsr #(8+4-2)]
	//-------
	// Make r9 = pointer to the font data and load r0 = first 4 font rows
	//-------
	pop		{r9}
#endif	
	and		r0, #0xFF
	cmp		r0, #0x20				// Are we to write a blank (' ')?
	beq		1f						// Yep, perform it separately
	ldr		r0, [r9, r0, lsl #3]!	// r0 = first 4 pixel rows of the font
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	//-------
	// Get the next 4 rows of font data
	//-------
	ldr		r0, [r9, #4]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
#ifdef RPi
	sub		r8, #(8*SCREEN_WIDTH)
#else
	sub		r8, #(8*SCREEN_WIDTH*2)
#endif
	bx		lr
	//-------
	// Write a 8x8 byte block of background color.
	//-------
1:
	//-------
	// Write 8 rows of 8 bytes of background color
	//-------
#ifdef RPi	
	orr		r5, r5, lsl #8
	orr		r5, r5, lsl #16
#else
	orr		r5, r5, lsl #16
	mov		r0, r5
	mov		r4, r5
	mov		r9, r5
#endif	
.rept 4	
#ifdef RPi
	str		r5, [r8]
	str		r5, [r8, #4]
	str		r5, [r8, #SCREEN_WIDTH]
	str		r5, [r8, #SCREEN_WIDTH+4]
	add		r8, #SCREEN_WIDTH*2
#else
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
#endif
.endr		
#ifdef RPi
	sub		r8, #(8*SCREEN_WIDTH)
#else
	sub		r8, #(8*SCREEN_WIDTH*2)
#endif
	bx		lr

#if 0
	//-------
	// Input:
	//	r0 low byte = character
	//	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	//	r8 = address to VRAM
	// Destroys:
	//	r0, r4, r5, r9
	//-------
copy_font_sub_6x8:
	//-------
	// Make r4 low word = foreground color and r5 low word = background color
	//-------
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	ldr		r4, [r9, r4, lsr #(8-2)]
	ldr		r5, [r9, r5, lsr #(8+4-2)]
	//-------
	// Make r9 = pointer to the font data and load r0 = first 4 font rows
	//-------
	ldr		r9, =fontTiles
	and		r0, #0xFF
	cmp		r0, #0x20				// Are we to write a blank (' ')?
	beq		.copy_blank				// Yep, perform it separately
	ldr		r0, [r9, r0, lsl #3]!	// r0 = first 4 pixel rows of the font
	//-------
	// Write topmost row of the font
	//-------
	tst		r0, #1
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #2
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #4
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #8
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #16
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #32
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the second row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<8)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<9)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<10)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<11)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<12)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<13)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the third row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<16)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<17)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<18)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<19)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<20)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<21)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the 4th row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<24)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<25)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<26)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<27)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<28)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<29)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Get the last 4 rows of font data
	//-------
	ldr		r0, [r9, #4]
	//-------
	// Write the 5th row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #1
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #2
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #4
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #8
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #16
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #32
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the 6th row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<8)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<9)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<10)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<11)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<12)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<13)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the 7th row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<16)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<17)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<18)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<19)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<20)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<21)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	//-------
	// Write the 8th row of the font
	//-------
	add		r8, #SCREEN_WIDTH*2
	tst		r0, #(1<<24)
	strneh	r4, [r8]
	streqh	r5, [r8]
	tst		r0, #(1<<25)
	strneh	r4, [r8, #2]
	streqh	r5, [r8, #2]
	tst		r0, #(1<<26)
	strneh	r4, [r8, #4]
	streqh	r5, [r8, #4]
	tst		r0, #(1<<27)
	strneh	r4, [r8, #6]
	streqh	r5, [r8, #6]
	tst		r0, #(1<<28)
	strneh	r4, [r8, #8]
	streqh	r5, [r8, #8]
	tst		r0, #(1<<29)
	strneh	r4, [r8, #10]
	streqh	r5, [r8, #10]
	sub		r8, #(7*SCREEN_WIDTH*2)
	bx		lr
	//-------
	// Write a 6x8 byte block of background color.
	//-------
.copy_blank:
	//-------
	// Write 8 rows of 4+2 bytes of background color
	//-------
	lsl		r5, #16
	orr		r5, r5, lsr #16
	mov		r0, r5
	mov		r9, r5
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r5, r9}
	sub		r8, #(7*SCREEN_WIDTH*2)
	bx		lr
#endif
	
//-------------------- Show hardware cursor -----------------
// Called from BlinkCursor, which is called by a C routine.
// Use the cursor position from BIOSData[0x50+2*page] and font size info.
// We can change r0..r3, must save other registers.
//	r0 contains the VRAM start addresss (s_pixels)
//
ShowCursor:
	//-------
	// First determine if we are in TEXT mode
	//-------
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrb	r1,[r3, #SCR_MODE]		// Get current video mode
	cmp		r1, #3					// Text mode?
	bxgt	lr						// Nope, just return immediately
	//-------
	// Determine cursor position, from 0040:0050+2*page
	//-------
	ldrb	r1,[r3, #SCR_PAGE]		// r1 = current display page
	add		r2, r3, r1, lsl #1
	ldrb	r1,[r2, #CURSOR_ROW]	// r1 = Row
	ldrb	r2,[r2, #CURSOR_COL]	// r2 = Column
	//-------
	// Calculate VRAM address
	//-------
#ifdef RPi
	lsl		r1, #(7+4)				// r1 = 128*16*Row
	add		r1, r1, lsl #2			// r1 = 128*16*Row + 512*16*Row = 640*16*Row
	add		r2, r1, r2, lsl #3		// r2 = SCREEN_WIDTH*16*Row + 8*Col
	cmp		r2, #SCREEN_WIDTH*(512-16)*2
	bxge	lr						// Skip if we would run out of screen area
	add		r2, r0					// r2 = screen address
#else
#if 1
	lsl		r1, #(10+4)				// r1 = SCREEN_WIDTH*16*Row
	add		r2, r1, r2, lsl #3		// r2 = SCREEN_WIDTH*16*Row + 8*Col
#else
	lsl		r1, #(10+3)				// r1 = SCREEN_WIDTH*8*Row
	lsl		r2, #1
	add		r2, r2, lsl #1			// r2 = 6*Column
	add		r2, r1
#endif	
	add		r2, r0, r2, lsl #1		// r2 = screen address
#endif	
	//-------
	// Calculate the cursor size.
	// [0x61] = cursor start and options
	// [0x60] = bottom scan line containing cursor (bits 0-4)
	// Return:Nothing
	//
	// Bitfields for cursor start and options:
	//
	// Bit(s)  Description     (Table 00013)
	// 7       should be zero
	// 6,5     cursor blink.
	// 			(00=normal, 01=invisible, 10=erratic, 11=slow).
	// 			(00=normal, other=invisible on EGA/VGA)
	// 4-0     topmost scan line containing cursor
	//-------
	ldrb	r1,[r3, #0x60]			// r1 = Cursor ending scanline
	ldrb	r0,[r3, #0x61]			// r0 = Cursor starting scanline
	and		r1, #0x1F				// Bottom scanline only uses low 5 bits
	tst		r0, #0xE0				// Invisible cursor?
	bxne	lr						// Yep, so skip drawing it
	add		r1, #1					// Add one to bottom scanline, to actually draw it
#ifdef RPi
	lsl		r1, #7
	add		r1, r1, lsl #2
	lsl		r0, #7
	add		r0, r0, lsl #2
	add		r3, r2, r1				// r3 = ending address in VRAM
	add		r2, r0					// r2 = starting address in VRAM
	cmp		r2, r3
	bxgt	lr						// Skip if start > end (SYSINFO)
	//-------
	// Draw the actual cursor
	//-------
	ldr		r1, =0x0F0F0F0F
1:	str		r1, [r2]
	str		r1, [r2, #4]
	add		r2, #SCREEN_WIDTH
	cmp		r2, r3
	blt		1b
#else
	add		r3, r2, r1, lsl #(10+1)	// r3 = ending address in VRAM
	add		r2, r0, lsl #(10+1)		// r2 = starting address in VRAM
	cmp		r2, r3
	bxgt	lr						// Skip if start > end (SYSINFO)
	//-------
	// Draw the actual cursor
	//-------
	mvn		r1, #0
1:	str		r1, [r2]
	str		r1, [r2, #4]
	str		r1, [r2, #8]
	add		r2, #SCREEN_WIDTH*2
	cmp		r2, r3
	blt		1b
#endif	
	bx		lr

//-------------------- Hide hardware cursor -----------------
// Called by a C routine, whenever the cursor position is about to change.
// Use the cursor position from BIOSData[0x50+2*page] and 6x8 font.
// We can change r0..r3, must save other registers.
//
	.global	HideCursor
HideCursor:
	//-------
	// First determine if we are in TEXT mode
	//-------
	ldr		r0,=BIOSData
	ldr		r0, [r0]
	ldrb	r1,[r0, #SCR_MODE]		// Get current video mode
	cmp		r1, #3					// Text mode?
	bxgt	lr						// Nope, just return immediately
	//-------
	// Clear the CursorCounter
	//-------
	ldr		r1,=CursorCounter
	mov		r3, #0
	ldr		r2, [r1]				// Get old CursorCounter into r2
	str		r3, [r1]				// CursorCounter = 0, 
	cmp		r2, #16					// Is the cursor visible currently?
	bxlt	lr						// Nope, all done.
	//-------
	// Determine cursor position, from 0040:0050+2*page
	//-------
	push	{r4, r5}
	ldrb	r1,[r0, #SCR_PAGE]		// r1 = current display page
	ldrb	r5,[r0, #0x4A]			// r5 = Number of screen columns
	add		r0, r1, lsl #1
	ldrh	r4,[r0, #CURSOR_COL]	// r4 low byte = column, high byte = row
	mov		r2, r5, lsl #1			// r2 = columns*2 = 160 / 80
	mov		r1, r4, lsr #8			// r1 = row number
	and		r4, #0xFF				// r4 = column number
	mul		r3, r2, r1				// r3 = columns*2*row
	add		r5, r3, r4, lsl #1		// r5 = columns*2*row+2*col
	//-------
	// Make sure the cursor is within the visible area,
	// and skip this if it is not.
	//-------
	cmp		r5, #80*50*2
	bge		1f
	//-------
	// Mark the position as dirty
	//-------
	ldr		r2,=TEXTBuffer
	mov		r4, #0
	strh	r4,[r2, r5]
	//-------
	// Return
	//-------
1:	pop		{r4, r5}
	bx		lr

//-------------------- Blink hardware cursor -----------------
// Called by a C routine.
// Increment cursor counter, show or hide cursor depending on counter value.
// This is called from the VBlank interrupt
//	r0 = VRAM start address (s_pixels)
//
	.global	BlinkCursor
BlinkCursor:
	ldr		r1,=CursorCounter
	ldr		r2, [r1]
	add		r2, #1					// Increment the counter
	str		r2, [r1]
	tst		r2, #15					// If the low 4 bits are clear, need to either show or hide the cursor, ...
	bxne	lr						// ... else just return.
	cmp		r2, #32
	bge		HideCursor				// Hide the cursors if the counter is >= 32
	b		ShowCursor				// Show the cursor if the counter = 16


#ifndef RPi
#ifndef IOS

	.ltorg
	
//-------------------- RenderChar -----------------
// Input:
// 	r0 low byte = char
//	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
//	r3 = address to VRAM
//	r8 = VRAM address of the next block (we may not write there!)
// Destroys:
//	r1, r2
// Output:
//	r0 = font data of the character, 0 if blank
RenderChar:
	push	{r3}
	sub		r2, r8, r3
	cmp		r2, #2*3				// Room for <3, =3 or >3 pixels?
	//-------
	// Make:
	//	r0 = font data,
	//	r1 = foreground color,
	//	r2 = background color
	//-------
	mov		r2, #0x05000000			// r2 = BG_PALETTE
	mov		r1, r0, lsr #8			// r1 = foreground palette index
	and		r1, #0xF
	lsl		r1, #1
	ldrh	r1, [r2, r1]			// r1 = foreground color
	orr		r0, r1, lsl #16			// Save it to r0 high halfword
	mov		r1, r0, lsr #12			// r1 = background palette index
	and		r1, #0xF
	lsl		r1, #1
	ldrh	r1, [r2, r1]			// r1 = background color
	lsl		r1, #16
	orr		r1, r0, lsr #16			// r1 = (background<<16) | foreground palette values
	ldr		r2, =font_4x8
	and		r0, #0xFF
	ldr		r0, [r2, r0, lsl #2]	// r0 = font data (4x8 bits)
	mov		r2, r1, lsr #16			// r2 = background palette value
	//-------
	// Determine how many pixels we can write
	//-------
	beq		3f						// Go write 3 pixels per row
	blt		2f						// Only room for 2 pixels per row!
	//-------
	// Room for 4 pixels, so write all of them.
	// Write topmost row of the font. Input = bits 28..31 of t0, output = 4 bytes.
	//-------
	tst		r0, #(1<<28)			// Leftmost pixel of the first row of the font
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<29)			// Second pixel of the first row of the font
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<30)			// Third pixel of the first row of the font
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<31)			// Last pixel of the first row of the font
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write second row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<24)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<25)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<26)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<27)			// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write third row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<20)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<21)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<22)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<23)			// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write fourth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<16)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<17)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<18)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<19)			// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write fifth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<12)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<13)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<14)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<15)			// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write sixth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<8)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<9)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<10)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<11)			// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write seventh row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<4)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<5)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<6)				// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<7)				// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	//-------
	// Write last row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<0)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<1)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<2)				// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	tst		r0, #(1<<3)				// Last pixel
	strneh	r1, [r3, #6]
	streqh	r2, [r3, #6]
	pop		{r3}
	bx		lr
	//-------
	// Room for 3 pixels!
	// Write topmost row of the font. Input = bits 28..31 of t0, output = 4 bytes.
	//-------
3:	tst		r0, #(1<<28)			// Leftmost pixel of the first row of the font
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<29)			// Second pixel of the first row of the font
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<30)			// Third pixel of the first row of the font
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write second row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<24)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<25)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<26)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write third row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<20)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<21)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<22)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write fourth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<16)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<17)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<18)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write fifth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<12)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<13)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<14)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write sixth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<8)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<9)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<10)			// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write seventh row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<4)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<5)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<6)				// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	//-------
	// Write last row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<0)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<1)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	tst		r0, #(1<<2)				// Third pixel
	strneh	r1, [r3, #4]
	streqh	r2, [r3, #4]
	pop		{r3}
	bx		lr
	//-------
	// Room for only 2 pixels!
	// Write topmost row of the font. Input = bits 28..31 of t0, output = 4 bytes.
	//-------
2:	tst		r0, #(1<<28)			// Leftmost pixel of the first row of the font
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<29)			// Second pixel of the first row of the font
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write second row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<24)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<25)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write third row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<20)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<21)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write fourth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<16)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<17)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write fifth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<12)			// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<13)			// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write sixth row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<8)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<9)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write seventh row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<4)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<5)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	//-------
	// Write last row of the font. Input = bits 24..27 of t0, output = 4 bytes.
	//-------
	add		r3, #512
	tst		r0, #(1<<0)				// Leftmost pixel
	strneh	r1, [r3, #0]
	streqh	r2, [r3, #0]
	tst		r0, #(1<<1)				// Second pixel
	strneh	r1, [r3, #2]
	streqh	r2, [r3, #2]
	pop		{r3}
	bx		lr


	.global	screen_copy_03_tiny
screen_copy_03_tiny:
	push    {r4-r12,lr}
	ldr		r8, =BIOSData
	ldr		r11,=CGA_B800
	ldr		r8, [r8]
	ldr		r11, [r11]
	ldr		r10,=TEXTBuffer
	ldrh	r3, [r8, #SCR_OFFS_LO]	// r3 = offset of current video page
	ldrb	r12, [r8, #SCR_COLS]	// Get number of screen columns
	push	{r12}
	add		r11, r3
	add		r12, r11, r12, lsl #1	// r12 = screen row end position
	mov		r8, #0x06000000			// r8 = VRAM output position
	//-------
	// Test each 20-character block, have the dirty buffer values changed
	// compared to current actual B800 segment screen contents? If any
	// character has changed, render the whole 20-character block again.
	//-------
.copy_loop_tiny:
	add		r8, #64*2				// r8 = Next block start position in VRAM
	//-------
	// Have the first 8 characters stayed the same?
	//-------
	ldmia	r11!, {r0-r3}			// r0..r3 = actual screen data
	ldmia	r10, {r4-r7}			// r4..r7 = dirty buffer values
	cmp		r0, r4
	cmpeq	r1, r5
	cmpeq	r2, r6
	cmpeq	r3, r7
	stmneia	r10, {r0-r3}			// Update the dirty buffer, if the values differ.
	add		r10, #2*8
	//-------
	// How about the middle 4 characters?
	//-------
	ldmia	r11!, {r0-r1}			// r0..r1 = actual screen data
	ldmia	r10, {r4-r5}			// r4..r5 = dirty buffer values
	cmpeq	r0, r4
	cmpeq	r1, r5
	stmneia	r10, {r0-r1}			// Update the dirty buffer, if the values differ.
	add		r10, #2*4
	//-------
	// How about the final 8 characters?
	//-------
	ldmia	r11!, {r0-r3}			// r0..r3 = actual screen data
	ldmia	r10, {r4-r7}			// r4..r7 = dirty buffer values
	cmpeq	r0, r4
	cmpeq	r1, r5
	cmpeq	r2, r6
	cmpeq	r3, r7
	stmneia	r10, {r0-r3}			// Update the dirty buffer, if the values differ.
	add		r10, #2*8
	beq		4f						// If all characters in the 20-char block have stayed the same, go to next block.
	//-------
	// Characters in this block have changed!
	// Input:
	//	r8 = output address in VRAM + 2*64
	//	r11 = input block start + 2*20
	// Need to save:
	//	r8 = output address in VRAM
	//	r10 = next dirty buffer start address
	//	r11 = next input block start
	//	r12 = row end address
	//-------
	//-------
	// Calculate:
	//	delta=maxwidth-calculateWidth(first, len);
	//	if (delta<0)
    //		freq=countWideChars(first, len)/(-delta);
	//-------
	mov		r9, #64					// r9 = delta = maxwidth = 64
	mov		r7, #0					// r7 = number of wide chars
	sub		r1, r11, #2*20
	ldr		r6, =char_width
1:	ldrb	r0, [r1], #2			// r0 = character
	ldrb	r0, [r6, r0]			// r0 = width of the character
	cmp		r0, #4					// Is this a wide character?
	addeq	r7, #1					// If yes, increment r7
	sub		r9, r0					// r9 = delta
	cmp		r1, r11					// 20 characters done?
	blt		1b						// Not yet, back to loop
	//-------
	// Calculate:
	//	if (delta<0)
    //		freq=countWideChars(first, len)/(-delta);
	//-------
	cmp		r9, #0
	bge		1f
	mov		r0, #0x280
	orr		r0, #0x04000000			// #define REG_DIVCNT			(*(vu16*)(0x04000280))
	rsb		r1, r9, #0				// r1 = -delta
	mov		r2, #0
	strh	r2, [r0]				// REG_DIVCNT = DIV_32_32 = 0
	str		r7, [r0, #0x10]			// #define REG_DIV_NUMER_L		(*(vs32*) (0x04000290)),	REG_DIV_NUMER_L = num
	str		r1, [r0, #0x18]			// #define REG_DIV_DENOM_L		(*(vs32*) (0x04000298)),	REG_DIV_DENOM_L = den
2:	ldrh	r1, [r0]
	tst		r1, #0x8000
	bne		2b						// Wait until the division is completed
	ldr		r7, [r0, #0x20]			// #define REG_DIV_RESULT_L		(*(vs32*) (0x040002A0)),	r7 is the result of the division
	//-------
	// Subroutine globbers r0..r2, needs r0 and r3 as input.
	// Registers:
	//	r0 = temp, char and attribute, globbered in the subroutine
	//	r1 = temp, globbered in the subroutine
	//	r2 = temp, globbered in the subroutine
	//	r3 = pointer to VRAM output block
	//	r4 = pointer to input characters (B800 segment)
	//	r5 = width of the character we just rendered
	//	r6 = pointer to char_width table
	//	r7 = freq (low byte), pass (next byte)
	//	r8 = VRAM output pointer of the next block of 20 input characters (64 pixels)
	//	r9 = delta
	//	r10 = dirty buffer pointer (used in the outer loop)
	//	r11 = pointer to next block of 20 input characters (B800 segment)
	//	r12 = row end address (used in the outer loop)
	//-------
1:	sub		r4, r11, #2*20			// r4 = pointer to characters to output
	sub		r3, r8, #2*64			// r3 = output address
	//-------
	// Loop here drawing 20 characters into 64 horizontal pixels
	//-------
1:	ldrh	r0, [r4], #2			// r0 = char and attribute
	and		r1, r0, #0xFF
	ldrb	r5, [r6, r1]			// r5 = width of the character
	bl		RenderChar				// Go render it, returns r0 = font data (0 if blank)
	add		r3, r5, lsl #1			// Increment the position by the character width
	cmp		r4, r11					// All 20 chars done?
	bge		4f						// Yep, exit the loop
	//-------
	// Check if we need to shrink or pad the font.
	//-------
	cmp		r5, #4					// If char width == 4, we may need to shrink it, if it is < 4, we may need to pad it.
	beq		2f
	//-------
	// Width < 4, do we need padding?
	//-------
	cmp		r9, #0					// delta > 0?
	ble		1b						// Nope, so skip padding.
	//-------
	// Pad after a blank, or if i<delta (== delta larger than the number of remaining characters).
	//-------
	sub		r1, r11, r4				// r1 = number of chars remaining * 2 == i*2
	rsbs	r1, r9, r1, lsr #1		// i<delta?
	rsblt	r1, r5, #4				// If (i<delta), pad width = 4 - char width (== 1 or 2)
	movge	r1, #1					// If (i>=delta), pad with 1 pixel only
	cmpge	r0, #1					// If (i>=delta), pad only if this is a blank (r0 = font data = 0)
	sublt	r9, r1					// Pad, so delta -= (i<delta)?2:1 and ..
	addlt	r3, r1, lsl #1			// .. pos += (i<delta)?2:1
	b		1b
	//-------
	// Do we need to shrink the font?
	//-------
2:	cmp		r9, #0					// delta < 0?
	bge		1b						// Nope, ready for the next char
	//-------
	// Shrink it, if it is time to do that. if ((pass%freq)==0)
	//-------
	and		r0, r7, #0xFF
	cmp		r0, r7, lsr #8			// pass == freq?
	add		r7, #1<<8				// pass++
	addeq	r9, #1					// Yep, so delta++ and ..
	subeq	r3, #2					// .. pos -= 1 and ..
	biceq	r7, #0xFF00				// .. pass = 0
	b		1b						// Continue with the next character
	//-------
	// Check if we have handled one full screen row
	//-------
4:	cmp		r11, r12
	bne		.copy_loop_tiny			// Not a full row yet, continue
	//-------
	// One row handled, adjust for the next screen row
	//-------
	add		r8, #256*2*7			// Next row in VRAM
	mov		r0, #(25*256*16)<<8
	cmp		r0, r8, lsl #8			// All 25 rows handled?
	ble		5f						// Yep, copying done!
	ldr		r0, [sp]				// Get number of screen columns
	add		r12, r0, lsl #1			// Prepare for the next row
	b		.copy_loop_tiny			// and continue copying
5:	pop		{r12}
	pop		{r4-r12,lr}
	bx		lr


#endif
#endif
	
	.data
	.align 2

	.global	CursorCounter
CursorCounter:
	.word	0
	
#ifndef RPi
#ifndef IOS
font_4x8:
	.word	0
	.word	0b11110001101000001010111000101111
	.word	0b00000111101011111010100001110000
	.word	0b00001001111111111111011001100000
	.word	0b00000110111111111111111101100000
	.word	0b01101111011011111111011001101111
	.word	0b00000110111111111111111101101111
	.word	0b00000000001001110111001000000000

	.word	0b11111111110110001000110111111111
	.word	0b00000000001001010101001000000000
	.word	0b11111111110110101010110111111111
	.word	0b00000000100001110101011100000000
	.word	0b00000000011101010111001001110010
	.word	0b00000000011000100100010001110011
	.word	0b01000110010111011101000100110011
	.word	0b11110110011011111111011001101111

	.word	0b00010011011111111111011100110001
	.word	0b10001100111011111111111011001000
	.word	0b01101111111101100110111111110110
	.word	0b00000101010101010101000001010000
	.word	0b00001110110111011110110011000000
	.word	0b11110001111110011111100011110000
	.word	0b00000000000000000000111111110000
	.word	0b01101111011001100110111101101111

	.word	0b01101111011001100110000000000000
	.word	0b00000000000001100110011011110110
	.word	0b00000000010011111111010000000000
	.word	0b00000000001011111111001000000000
	.word	0b00000000000000010001111100000000
	.word	0b00000000001011111111010000000000
	.word	0b00000000000001100110111111110000
	.word	0b00000000111111110110011000000000
	//-------
	// chars 32..63
	//-------
	.word	0											// blank
	.word	0b00000001000100010001000000010000			// !
	.word	0b00000101010100000000000000000000			// " "
	.word	0b00000101011101010101011101010000			// #
	.word	0b00100010010100100100010100100010			// $
	.word	0b00000101010000100010000101010000			// %
	.word	0b00100101001001010101001101100000			// &
	.word	0b00000001000100000000000000000000			// ' '
	.word	0b00000010000100010001000100100000			// (
	.word	0b00000001001000100010001000010000			// )
	.word	0b00000101001001110010010100000000			// *
	.word	0b00000010001001110010001000000000			// +
	.word	0b00000000000000000000001000100001			// ,
	.word	0b00000000000001110000000000000000			// -
	.word	0b00000000000000000000000100010000			// .
	.word	0b00000100010000100010000100010000			// /
	.word	0b00000010010101110101010100100000			// 0
	.word	0b00000010001100100010001000100000			// 1
	.word	0b00000010010101000010000101110000			// 2
	.word	0b00000111010000100100010100100000			// 3
	.word	0b00000100011001010111010001000000			// 4
	.word	0b00000111000100110100010100100000			// 5
	.word	0b00000110000100110101010100100000			// 6
	.word	0b00000111010001000010001000100000			// 7
	.word	0b00000010010100100101010100100000			// 8
	.word	0b00000010010101010110010000110000			// 9
	.word	0b00000000000100000000000100000000			// :
	.word	0b00000000001000000000001000010000			// ;
	.word	0b00000000010000100001001001000000			// <
	.word	0b00000000011100000111000000000000			// =
	.word	0b00000000000100100100001000010000			// >
	.word	0b00000010010101000010000000100000			// ?
	//-------
	// chars 64..95
	//-------
	.word	0b00000010010101010001000101100000			// //
	.word	0b00000010010101010111010101010000			// A
	.word	0b00000011010100110101010100110000			// B
	.word	0b00000010010100010001010100100000			// C
	.word	0b00000011010101010101010100110000			// D
	.word	0b00000011000100110001000100110000			// E
	.word	0b00000011000100110001000100010000			// F
	.word	0b00000010010100010101010100100000			// G
	.word	0b00000101010101110101010101010000			// H
	.word	0b00000001000100010001000100010000			// I
	.word	0b00000100010001000100010100100000			// J
	.word	0b00000101010100110101010101010000			// K
	.word	0b00000001000100010001000100110000			// L
	.word	0b00000101011101010101010101010000			// M
	.word	0b00000100010101110111010100010000			// N
	.word	0b00000010010101010101010100100000			// O
	.word	0b00000011010101010011000100010000			// P
	.word	0b00000010010101010101001101100000			// Q
	.word	0b00000011010101010011010101010000			// R
	.word	0b00000010010100100100010100100000			// S
	.word	0b00000111001000100010001000100000			// T
	.word	0b00000101010101010101010101110000			// U
	.word	0b00000101010101010101010100100000			// V
	.word	0b00000101010101010101011101010000			// W
	.word	0b00000101010100100101010101010000			// X
	.word	0b00000101010100100010001000100000			// Y
	.word	0b00000111010000100010000101110000			// Z
	.word	0b00000011000100010001000100110000
	.word	0b00000001000100100010010001000000
	.word	0b00000011001000100010001000110000
	.word	0b00000010010100000000000000000000
	.word	0b00000000000000000000000000000111
	//-------
	// chars 96..127
	//-------
	.word	0b00000001001000000000000000000000
	.word	0b00000000001101000110010101100000			// a
	.word	0b00000001000100110101010100110000			// b
	.word	0b00000000001000010001000100100000			// c
	.word	0b00000100010001100101010101100000			// d
	.word	0b00000000001001010111000101100000			// e
	.word	0b00000010000100110001000100010000			// f
	.word	0b00000000011001010101011001000011			// g
	.word	0b00000001000100110101010101010000			// h
	.word	0b00000001000000010001000100010000			// i
	.word	0b00000010000000100010001000100001			// j
	.word	0b00000001000101010011010101010000			// k
	.word	0b00000001000100010001000100010000			// l
	.word	0b00000000010101110101010101010000			// m
	.word	0b00000000001101010101010101010000			// n
	.word	0b00000000001001010101010100100000			// o
	.word	0b00000000001101010101001100010001			// p
	.word	0b00000000011001010101011001000100			// q
	.word	0b00000000001000010001000100010000			// r
	.word	0b00000000011000010010010000110000			// s
	.word	0b00000010001001110010001000100000			// t
	.word	0b00000000010101010101010101110000			// u
	.word	0b00000000010101010101010100100000			// v
	.word	0b00000000010101010101011101010000			// w
	.word	0b00000000010101010010010101010000			// x
	.word	0b00000000010101010101011001000011			// y
	.word	0b00000000011101000010000101110000			// z
	.word	0b01000010001000010010001001000000			// {
	.word	0b00010001000100000001000100010000			// |
	.word	0b00010010001001000010001000010000			// }
	.word	0b00000000001101100000000000000000			// ~
	.word	0b00100101010101010111000000000000			// 127
	//-------
	// chars 128..159
	//-------
	.word	0b00000010010100010001010100100011			// Ç
	.word	0b01010000010101010101010101110000			// ü
	.word	0b00000100001001010111000101100000			// é
	.word	0b01110000001101000110010101100000			// ã
	.word	0b01010000001101000110010101100000			// ä
	.word	0b00110000001101000110010101100000			// à
	.word	0b00100000001101000110010101100000			// å
	.word	0b00000000011000010001000101100011			// ç
	
	.word	0b01110000001001010111000101100000			// ê		136
	.word	0b01010000001001010111000101100000			// ë
	.word	0b00110000001001010111000101100000			// è
	.word	0b01010000000000100010001000100000			// ï
	.word	0b00100101000000100010001000100000			// î
	.word	0b00110000000000100010001000100000			// ì
	.word	0b01010000001001010101011101010000			// Ä
	.word	0b00100000001001010101011101010000			// Å
	
	.word	0b01100000001100010011000100110000			// É		144
	.word	0b00000000011100100111010101110000			// æ
	.word	0b00000111001000100111010101110000			// Æ
	.word	0b01110000001001010101010100100000			// ô
	.word	0b01010000001001010101010100100000			// ö
	.word	0b00110000001001010101010100100000			// ò
	.word	0b01110000010101010101010101110000			// û
	.word	0b00110000010101010101010101110000			// ù
	
	.word	0b01010000010101010101011001000011			// ÿ		152
	.word	0b01010010010101010101010100100000			// Ö
	.word	0b01010000010101010101010101110000			// Ü
	.word	0b00000000001001100001000101100010			// ¢
	.word	0b00000100001001100010001001110000			// £
	.word	0b00000101010100100111001001110010			// ¥
	.word	0b00000111010101110001010101010101			//
	.word	0b00000110001000100111001000100011			// ƒ
	//-------
	// chars 160..191
	//-------
	.word	0b01100000001101000110010101100000			// á		160
	.word	0b00110000000000010001000100010000			// í
	.word	0b01100000001001010101010100100000			// ó
	.word	0b01100000010101010101010101110000			// ú
	.word	0b01110000001101010101010101010000			// ñ
	.word	0b01110100010101110111010100010000			// Ñ
	.word	0b01100101011000000111000000000000			// ª
	.word	0b01110101011100000111000000000000			// º
	
	.word	0b00000010000000100001010100100000			// ¿		168
	.word	0b00000000000000000111000100000000			// ¬
	.word	0b00000000000000000111010000000000			// ¬	
	.word	0b00100010000001110000001100100110			// ½
	.word	0b00100010000001110000000101110010			// ¼
	.word	0b00000001000000010001000100010000			// ¡
	.word	0b00000010000101010101000100100000			// «
	.word	0b00000010010001010101010000100000			// »	
	
	.word	0b01000001010000010100000101000001			// 176
	.word	0b10100101101001011010010110100101			// Checkerboard
	.word	0b01011110101011100101111010101110			//
	.word	0b00100010001000100010001000100010			// |
	.word	0b00100010001000100011001000100010			// -|
	.word	0b00100010001100100011001000100010			// =|
	.word	0b01010101010101010101010101010101			// -||
	.word	0b00000000000000000111010101010101			// -,,

	.word	0b00000000001100100011001000100010			// =,
	.word	0b01010101010101000101010101010101
	.word	0b01010101010101010101010101010101			// ||
	.word	0b00000000011101000101010101010101
	.word	0b01010101010101000111000000000000
	.word	0b01010101010101010111000000000000
	.word	0b01000100011101000111000000000000			// =´
	.word	0b00000000000000000011001000100010			// -,
	//-------
	// chars 192..223
	//-------
	.word	0b00100010001000101110000000000000			// ´-
	.word	0b00100010001000101111000000000000			// -´-
	.word	0b00000000000000001111001000100010			// -,-
	.word	0b00100010001000101110001000100010			// |-
	.word	0b00000000000000001111000000000000			// -
	.word	0b00100010001000101111001000100010			// +
	.word	0b00100010111000101110001000100010			// |=
	.word	0b01010101010101011101010101010101			// ||-
	
	.word	0b01010101110100011111000000000000			// ''=
	.word	0b00000000111100011101010101010101			// ,,=
	.word	0b01010101110100001111000000000000			// =''=
	.word	0b00000000111100001101010101010101			// =,,=
	.word	0b01010101110100011101010101010101			// ||=
	.word	0b00000000111100001111000000000000			// =
	.word	0b01010101110100001101010101010101			// ++
	.word	0b00100010111100001111000000000000			// =´=
	
	.word	0b01010101010101011111000000000000			// -''-
	.word	0b00000000111100001111001000100010			// =,=
	.word	0b00000000000000001111010101010101			// -,,-
	.word	0b01010101010101011111000000000000			// ''-
	.word	0b00100010111000101110000000000000			// ´=
	.word	0b00000000111000101110001000100010			// ,=
	.word	0b00000000000000001111010101010101			// ,,-
	.word	0b01010101010101011111010101010101			// -||-
	
	.word	0b00100010111100001111001000100010			// =|=
	.word	0b00100010001000100011000000000000			// -´
	.word	0b00000000000000001110001000100010			// ,-
	.word	0b11111111111111111111111111111111
	.word	0b00000000000000001111111111111111
	.word	0b00110011001100110011001100110011
	.word	0b11001100110011001100110011001100
	.word	0b11111111111111110000000000000000
	//-------
	// chars 224..255
	//-------
	.word	0b00000000000010100101010110100000
	.word	0b00000000001101010111010100110001
	.word	0b00000000011101010001000100010000
	.word	0b00000000000001110101010101010000
	.word	0b11110001001001000010000111110000
	.word	0b00000000000011100101010100100000
	.word	0b00000000100110011001011000100001
	.word	0b00000000000010100101010001000100
	
	.word	0b00000000011100100101010100100111
	.word	0b00000000011010011111100101100000
	.word	0b00000000011010011001011010010000
	.word	0b00001100010001101001100101100000
	.word	0b00000000011011011111101101100000
	.word	0b10000100011011011111101101100001
	.word	0b00000000011000010111000101100000
	.word	0b00000000001001010101010101010000

	.word	0b00000000011100000111000001110000
	.word	0b00000000001001110010000001110000
	.word	0b00000100001000010010010001110000
	.word	0b00000001001001000010000101110000
	.word	0b00000011001100010001000100010001
	.word	0b00000010001000100010001000110011
	.word	0b00000000001000000111000000100000
	.word	0b00000000101001010000101001010000

	.word	0b00000010010100100000000000000000
	.word	0b00000000000000110011000000000000
	.word	0b00000000000000000011000000000000
	.word	0b11000100010001000100010101100100
	.word	0b00110101010101010101000000000000
	.word	0b00110010001100010011000000000000
	.word	0b00000000001100110011001100000000
	.word	0b00000010000000000000000000000000
	
	
	
char_width:
	.byte	2,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4		// 0
    .byte	2,2,4,4,4,4,4,2, 3,3,4,4,3,4,2,3, 4,3,4,4,4,4,4,4, 4,4,2,3,4,4,4,4		// 32
    .byte	4,4,4,4,4,3,3,4, 4,2,4,4,3,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,3,4,3,4,4		// 64
    .byte	3,4,4,3,4,4,3,4, 4,2,3,4,2,4,4,4, 4,4,3,4,4,4,4,4, 4,4,4,4,2,4,4,2		// 96
	.byte	4,4,4,4,4,4,4,4, 4,4,4,4,4,3,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4		// 128
	.byte	4,3,4,4,4,4,4,4, 4,4,4,4,4,2,4,4, 4,4,4,3,3,3,3,3, 3,3,3,3,3,3,3,3		// 160
	.byte	3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4		// 192
	.byte	4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4		// 224

#endif
#endif

	.align 2
	
	.global	TEXTBuffer
TEXTBuffer:
	.space	80*50*2				// Dirty buffer for text screen blitting
