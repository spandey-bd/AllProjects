;=============================================================================
; MCGA.S
;
; This file contains routines to handle MCGA graphics (320x200 256-color)
; and SVGA graphics (640x400 or 640x480 256-color) routines.
;
;	screen_copy_13()			= Blit 320x200x256 MCGA screen
;	screen_copy_SVGA()			= Blit 640x400x256 or 640x480x256 SVGA screen
;	CharToMCGAScreen			= Draw a character to MCGA 256-color screen (mode 13)
;	PixelToMCGAScreen			= int10h helper code for drawing a pixel (mode 13)
;	PixelFromMCGAScreen			= int10h helper code for reading a pixel (mode 13)
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

	AREA MCGA, CODE, READONLY

	GLOBAL	CharToMCGAScreen
	GLOBAL	PixelToMCGAScreen
	GLOBAL	PixelFromMCGAScreen
	
	ALIGN	4

	INCLUDE ../include/defines.inc

	EXTERN	EGAVGA_A000
	EXTERN	BG_PALETTE
	EXTERN	VGAOffset
	EXTERN	BIOSData
	EXTERN	INTVectors
	EXTERN	ROM8x8Font
	EXTERN	BIOS_F000

;#include "macros.inc"

SCREEN_WIDTH	EQU	1024

; ------------------- COPY TO SCREEN ----------------------------------
; screen_copy_13(pixels, &render)
;	r0 = pixels = pointer to offscreen screen buffer 
;	r1 = &render = pointer to RenderData structure to be filled
;
	GLOBAL screen_copy_13
screen_copy_13
	push    {r4-r11, lr}
	ldr		r1,=EGAVGA_A000			; VGA screen start
	ldr		r3, =BG_PALETTE			; Palette lookup table address
	ldr		r1, [r1]
	;-------
	; Calculate the display pitch value (vga.draw.address_add in DOSBox)
	; from CRTC registers index 0x13
	;-------
	ldr		r4, =VGAOffset
	ldrb	r11, [r4]
	lsl		r11, #3
	sub		r11, #320
	
	mov		r2, #200				; Copy 200 rows worth of data
1	mov		r12, #40				; Copy 40*8 = 320 pixels per row
	
2	ldr		r4, [r1], #4			; Get 4 bytes (4 pixels) from input

	uxtb	r5, r4
	uxtb	r7, r4, ror #8
	uxtb	r6, r4, ror #16
	uxtb	r8, r4, ror #24
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsl #2]
	ldr		r6, [r3, r6, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16

	ldr		r4, [r1], #4			; Get 4 bytes (4 pixels) from input
	
	uxtb	r7, r4
	uxtb	r9, r4, ror #8
	uxtb	r8, r4, ror #16
	uxtb	r10, r4, ror #24
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	ldr		r10, [r3, r10, lsl #2]
	orr		r7, r9, lsl #16
	orr		r8, r10, lsl #16

	stmia	r0!, {r5-r8}			; Write 4 words = 16 bytes = 8 pixels	

	subs	r12, #1
    bgt     %b2

	;-------
	; One screen row handled, prepare for the next row
	;-------		
	add		r0, #(SCREEN_WIDTH-320)*2 ; Point r0 to the next row in VRAM
	add		r1, r11					; Adjust input position by the VGA offset value
    subs    r2, #1
    bgt     %b1
	pop     {r4-r11, pc}			; Return to caller

; ------------------- COPY TO SCREEN ----------------------------------
; screen_copy_13(pixels, &render)
;	r0 = pixels = pointer to offscreen screen buffer 
;	r1 = &render = pointer to RenderData structure to be filled
;
	GLOBAL screen_copy_SVGA
screen_copy_SVGA
	push    {r4-r11, lr}
	;-------
	; Get the current display mode, to select between 640x400 and 640x480 modes.
	;-------
	ldr		r4, =BIOSData
	ldr		r5, =EGAVGA_A000			; VGA screen start
	ldr		r4, [r4]
	ldrb	r11, [r4, #0x49]
	mov		r2, #400				; Copy 400 rows worth of data
	cmp		r11, #0x1D
	bne		%f3
	mov		r2, #480				; Copy 480 rows worth of data
3	ldr		r3, =BG_PALETTE			; Palette lookup table address
	ldr		r1, [r5]

1	mov		r12, #80				; Copy 80*8 = 640 pixels per row
	
2	ldr		r4, [r1], #4			; Get 4 bytes (4 pixels) from input

	uxtb	r5, r4
	uxtb	r7, r4, ror #8
	uxtb	r6, r4, ror #16
	uxtb	r8, r4, ror #24
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsl #2]
	ldr		r6, [r3, r6, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16

	ldr		r4, [r1], #4			; Get 4 bytes (4 pixels) from input
	
	uxtb	r7, r4
	uxtb	r9, r4, ror #8
	uxtb	r8, r4, ror #16
	uxtb	r10, r4, ror #24
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	ldr		r10, [r3, r10, lsl #2]
	orr		r7, r9, lsl #16
	orr		r8, r10, lsl #16

	stmia	r0!, {r5-r8}			; Write 4 words = 16 bytes = 8 pixels	

	subs	r12, #1
	bgt		%b2

	;-------
	; One screen row handled, prepare for the next row
	;-------		
	add		r0, #(SCREEN_WIDTH-640)*2 ; Point r0 to the next row in VRAM
    subs    r2, #1
    bgt     %b1
	pop     {r4-r11, pc}			; Return to caller
	
;-------------------- Print a character to the screen -----------------
; Called by a C routine, r0 = char, r1 = color (0..16)|page<<8, r2=number of times.
; Use the cursor position from BIOSData[0x50] and ROM8x8font.
; We can change r0..r3, must save other registers.
;

	MACRO
	put_font_row
	tst		r0, #0x80
	strbne	r1, [r4]
	tst		r0, #0x40
	strbne	r1, [r4, #1]
	tst		r0, #0x20
	strbne	r1, [r4, #2]
	tst		r0, #0x10
	strbne	r1, [r4, #3]
	tst		r0, #0x8
	strbne	r1, [r4, #4]
	tst		r0, #0x4
	strbne	r1, [r4, #5]
	tst		r0, #0x2
	strbne	r1, [r4, #6]
	tst		r0, #0x1
	strbne	r1, [r4, #7]
	add		r4, r2
	MEND

CharToMCGAScreen ROUT
	push	{r4-r6}
	;-------
	; Setup r6, r1 and r2
	;-------
	ldr		r4,=INTVectors
	mov		r6, r2					; r6 = counter
	and		r1, #0xFF				; r1 = color
	;-------
	; Calculate screen address to r4
	;-------
	ldr		r4,[r4]
	mov		r3, #0x450
	ldrh	r4,[r4, r3]				; r4 low byte = column, high byte = row
	and		r3, r4, #0xFF			; r3 = column (0..39)
	lsr		r4, #8					; r4 = row (0..24)
	mov		r5, #320*8
	mul		r4, r5, r4				; r4 = 40*8*8*row
	add		r4, r3, lsl #3			; r4 = 40*8*row + 8*column
	ldr		r3, =EGAVGA_A000
	ldr		r3, [r3]
	add		r4, r3
	mov		r2, #320
	;-------
	; Get address to font data
	;-------
	ldr		r3, =ROM8x8Font
	ldr		r5, =BIOS_F000
	ldr		r3, [r3]
	ldr		r5, [r5]
	bic		r3, #0xFF000000
	add		r3, r5
	ldrb	r0, [r3, r0, lsl #3]!	; r3 = font position, r0 = first row of font
	;-------
	; Write 8x8 pixels
	;	r0 = row of font data
	;	r1 = color
	;	r2 = free
	;	r3 = font pointer
	;	r4 = VRAM pointer
	;	r5 = free
	;	r6 = number of times to write character
	;-------
2	put_font_row
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
	;-------
	; One character done, test if we need to write more.
	;-------
	subs	r6, #1
	beq		%f3
	sub		r3, #7					; Back to first row of font
	sub		r4, r2, lsl #3
	add		r4, #8					; Next column
	ldrb	r0, [r3]				; Get first row of font
	b		%b2
3	pop		{r4-r6}
	bx		lr						; Return to caller

;-------------------- Draw a pixel to the screen ----------------------
; Called by a C routine, r0 = pixel color, r1 = X, r2 = Y
; We can change r0..r3 and r12, must save other registers.
;

PixelToMCGAScreen
	;-------
	; Calculate screen address to r3
	;-------
	mov		r3, #320
	mul		r3, r2, r3
	ldr		r2, =EGAVGA_A000
	ldr		r2, [r2]
	add		r3, r1					; r3 = 320*Y + X
	add		r3, r2
	;-------
	; Write the pixel
	;-------
	strb	r0, [r3]
	bx		lr

;-------------------- Read a pixel from the screen --------------------
; Called by a C routine, r0 = X, r1 = Y
; We can change r0..r3 and r12, must save other registers.
;

PixelFromMCGAScreen
	;-------
	; Calculate screen address to r3
	;-------
	mov		r3, #320
	mul		r3, r1, r3
	ldr		r2, =EGAVGA_A000
	ldr		r2, [r2]
	add		r3, r0					; r3 = 320*Y + X
	add		r3, r2
	;-------
	; Read the pixel
	;-------
	ldrb	r0, [r3]
	bx		lr

	END
