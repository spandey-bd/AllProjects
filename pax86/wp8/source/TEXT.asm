;=============================================================================
; TEXT.S
;
; This file contains routines to handle text mode graphics blitting
; (generating 640x400 pixel graphics image from 80x25 character text screen).
; This file also has rpoutines for text mode blinking cursor emulation.
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http//dsx86.patrickaalto.com
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

	AREA TEXT, CODE, READONLY

	ALIGN	4

	GLOBAL	text_pos_by_page_cursor
	GLOBAL	text_pos_by_page_col_row
	GLOBAL	screen_copy_03
	
	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

	EXTERN	registers
	
	EXTERN	BIOSData
	EXTERN	BG_PALETTE
	EXTERN	BIOS_F000
	EXTERN	ROM8x8Font
	EXTERN	ROM8x16Font
	EXTERN	CGA_B800

SCR_MODE		EQU		0x49
SCR_COLS		EQU		0x4A
SCR_PAGELEN		EQU		0x4C
SCR_OFFS_LO		EQU		0x4E
SCR_OFFS_HI		EQU		0x4F
CURSOR_COL		EQU		0x50
CURSOR_ROW		EQU		0x51
SCR_PAGE		EQU		0x62
SCR_ROWS		EQU		0x84

SCREEN_WIDTH	EQU		1024

;#define RGB565(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))

; ------------- Helper functions for address calculations -------------
;

	;-------
	; Input
	;	r0 = page
	; Output
	;	r0 = offset into B800 segment
	; Can change
	;	r0..r3, r12
	;-------
text_pos_by_page_cursor
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrh	r1, [r3, #SCR_PAGELEN]			; r1 = length of each page in B800 segment
	add		r3, #CURSOR_COL
	mul		r2, r1, r0						; r2 = Start offset of the requested page
	ldrb	r1,[r3, r0, lsl #1]				; r1 = Cursor column on this page
	add		r3, #1
	ldrb	r0,[r3, r0, lsl #1]				; r0 = Cursor row on this page
	sub		r3, #CURSOR_ROW
	add		r2, r1, lsl #1					; r2 = page offset + 2*column
	ldrh	r1, [r3, #SCR_COLS]
	mul		r3, r1, r0
	add		r0, r2, r3, lsl #1				; r0 = page offset + 2*column + 2*(screen_columns*cursor_row)
	bx		lr

	;-------
	; Input
	;	r0 = page, r1 = column, r2 = row
	; Output
	;	r0 = offset into B800 segment
	; Can change
	;	r0..r3, r12
	;-------
text_pos_by_page_col_row
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrh	r12, [r3, #SCR_PAGELEN]			; r12 = length of each page in B800 segment
	mul		r0, r12, r0						; r0 = Start offset of the requested page
	add		r0, r1, lsl #1					; r0 = page offset + 2*column
	ldrh	r1, [r3, #SCR_COLS]
	mul		r3, r1, r2
	add		r0, r3, lsl #1					; r0 = page offset + 2*column + 2*(screen_columns*row)
	bx		lr

	LTORG

; ------------------- COPY TO SCREEN ----------------------------------
; Input
;	r0 = pixels = pointer to offscreen screen buffer 
;	r1 = &render = pointer to RenderData structure to be filled
;
SP_ROWEND		EQU		0
SP_NEXTROW		EQU		4
SP_SCRCOLS		EQU		8
SP_ENDPOS		EQU		12

screen_copy_03
	push    {r4-r12,lr}
	;-------
	; Setup the RenderData structure
	;-------
	mov		r4, #SCREEN_WIDTH			; render.pitch
	mov		r5, r0						; render.imgtop
	mov		r6, #(25*16)				; render.toprows
	mov		r7, #0						; render.imgbot
	mov		r8, #0						; render.botrows
	stmia	r1, {r4-r8}					; Save to renderdata
	;-------
	; Setup pointers and screen dimensions
	;-------
	ldr		r8, =BIOSData
	ldr		r11,=CGA_B800
	ldr		r8, [r8]
	ldr		r11, [r11]
	ldr		r10,=TEXTBuffer
	ldrh	r3, [r8, #SCR_OFFS_LO]	; r3 = offset of current video page
	ldrb	r6, [r8, #SCR_COLS]		; Get number of screen columns
	ldrb	r2, [r8, #SCR_ROWS]		; Get number of screen rows
	add		r11, r3
	add		r8, r0, #(25*SCREEN_WIDTH*2*16) ; r8 = end position in VRAM
	cmp		r2, #24					; 25 screen rows?
	ldreq	r12, =copy_font_sub_8x16
	ldrne	r12, =copy_font_sub_8x8
	moveq	r5, #SCREEN_WIDTH*16
	movne	r5, #SCREEN_WIDTH*8
	add		r4, r11, r6, lsl #1		; r4 = current screen row end position in input
	push	{r4, r5, r6, r8}		; r4=SP_ROWEND, r5=SP_NEXTROW, r6=SP_SCRCOLS, r8=SP_ENDPOS
	mov		r8, r0					; r8 = VRAM output position
copy_loop
	ldmia	r11!, {r0-r3}			; r0..r3 = actual screen data
	ldmia	r10!, {r4-r7}			; r4..r7 = dirty buffer values
	;-------
	; First two characters have stayed the same?
	;-------
	cmp		r0, r4					
	addeq	r8, #(2*2*8)
	beq		%f1
	;-------
	; Update these two characters on the physical screen
	;-------
	str		r0, [r10, #-(4*4)]		; First update the dirty buffer
	;-------
	; r0 = char0 code, char0 attribute, char1 code, char1 attribute
	; r1..r3 = char2, char3, char4, char5, char6, char7
	; r4 = free
	; r5..r7 = char2, char3, char4, char5, char6, char7 dirty buffer values
	; r8 = VRAM position
	; r9 = free
	; r10 = dirty buffer position
	; r11 = logical B800 screen position
	; r12 = font_sub call address
	;-------
	push	{r5}
	push	{r0}
	blx		r12
	pop		{r0}
	add		r8, #2*8
	lsr		r0, #16
	blx		r12
	pop		{r5}
	add		r8, #2*8
	;-------
	; Second two characters have stayed the same?
	;-------
1	cmp		r1, r5
	addeq	r8, #(2*2*8)
	beq		%f2
	;-------
	; Update these two characters on the physical screen
	;-------
	str		r1, [r10, #-(3*4)]		; First update the dirty buffer
	mov		r0, r1
	blx		r12
	add		r8, #2*8
	mov		r0, r1, lsr #16
	blx		r12
	add		r8, #2*8
	;-------
	; Third two characters have stayed the same?
	;-------
2	cmp		r2, r6
	addeq	r8, #(2*2*8)
	beq		%f3
	;-------
	; Update these two characters on the physical screen
	;-------
	str		r2, [r10, #-(2*4)]		; First update the dirty buffer
	mov		r0, r2
	blx		r12
	add		r8, #2*8
	mov		r0, r2, lsr #16
	blx		r12
	add		r8, #2*8
	;-------
	; Fourth two characters have stayed the same?
	;-------
3	cmp		r3, r7
	addeq	r8, #(2*2*8)
	beq		%f4
	;-------
	; Update these two characters on the physical screen
	;-------
	str		r3, [r10, #-(1*4)]		; First update the dirty buffer
	mov		r0, r3
	blx		r12
	add		r8, #2*8
	mov		r0, r3, lsr #16
	blx		r12
	add		r8, #2*8
	;-------
	; Check if we have handled one full screen row
	;-------
4	ldr		r4, [sp, #SP_ROWEND]
	cmp		r11, r4
	bne		copy_loop				; Not a full row yet, continue
	;-------
	; One row handled, adjust for the next screen row
	;-------
	ldmia	sp, {r4, r5, r6, r7}	; r4=SP_ROWEND, r5=SP_NEXTROW, r6=SP_SCRCOLS, r7=SP_ENDPOS
	add		r8, r5, lsl #1			; Next row in VRAM
	cmp		r7, r8					; All 25/50 rows handled?
	sub		r8, r6, lsl #4			; Clear the current column offset
	add		r4, r6, lsl #1			; Prepare for the next row
	str		r4, [sp, #SP_ROWEND]
	bgt		copy_loop				; and continue copying, if not yet all 25 rows handled.
	add		sp, #4*4				; Remove pushed r4, r5, r6 and r8 from stack
	pop		{r4-r12,pc}

	LTORG
	
	MACRO
	put_font_row
	tst		r0, #0x80
	strhne	r4, [r8]
	strheq	r5, [r8]
	tst		r0, #0x40
	strhne	r4, [r8, #2]
	strheq	r5, [r8, #2]
	tst		r0, #0x20
	strhne	r4, [r8, #4]
	strheq	r5, [r8, #4]
	tst		r0, #0x10
	strhne	r4, [r8, #6]
	strheq	r5, [r8, #6]
	tst		r0, #0x8
	strhne	r4, [r8, #8]
	strheq	r5, [r8, #8]
	tst		r0, #0x4
	strhne	r4, [r8, #10]
	strheq	r5, [r8, #10]
	tst		r0, #0x2
	strhne	r4, [r8, #12]
	strheq	r5, [r8, #12]
	tst		r0, #0x1
	strhne	r4, [r8, #14]
	strheq	r5, [r8, #14]
	add		r8, #SCREEN_WIDTH*2
	MEND

	;-------
	; Input
	;	r0 low byte = character
	;	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	;	r8 = address to VRAM
	; Destroys
	;	r0, r4, r5, r9
	;-------
copy_font_sub_8x16
	;-------
	; Make r4 low word = foreground color and r5 low word = background color
	; Make r9 = pointer to the font data and load r0 = first 4 font rows
	;-------
	ldr		r9, =ROM8x16Font
	ldr		r5, =BIOS_F000
	ldr		r9, [r9]
	ldr		r5, [r5]
	bic		r9, #0xFF000000
	add		r9, r5
	push	{r9}
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	lsr		r4, #(8-2)
	lsr		r5, #(8+4-2)
	ldr		r4, [r9, r4]
	ldr		r5, [r9, r5]
	;-------
	; Make r9 = pointer to the font data and load r0 = first 4 font rows
	;-------
	pop		{r9}
	and		r0, #0xFF
	cmp		r0, #0x20				; Are we to write a blank (' ')?
	beq		%f1						; Yep, perform it separately
	add		r9, r0, lsl #4
	ldr		r0, [r9]				; r0 = first 4 pixel rows of the font
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	;-------
	; Get the next 4 rows of font data
	;-------
	ldr		r0, [r9, #4]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	;-------
	; Get the next 4 rows of font data
	;-------
	ldr		r0, [r9, #8]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	;-------
	; Get the last 4 rows of font data
	;-------
	ldr		r0, [r9, #12]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	sub		r8, #(16*SCREEN_WIDTH*2)
	bx		lr
	;-------
	; Write a 8x16 byte block of background color.
	;-------
1
	;-------
	; Write 16 rows of 8 bytes of background color
	;-------
	orr		r5, r5, lsl #16
	mov		r0, r5
	mov		r4, r5
	mov		r9, r5
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	sub		r8, #(16*SCREEN_WIDTH*2)
	bx		lr

	;-------
	; Input
	;	r0 low byte = character
	;	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	;	r8 = address to VRAM
	; Destroys
	;	r0, r4, r5, r9
	;-------
copy_font_sub_8x8
	;-------
	; Make r4 low word = foreground color and r5 low word = background color
	; Make r9 = pointer to the font data and load r0 = first 4 font rows
	;-------
	ldr		r9, =ROM8x8Font
	ldr		r5, =BIOS_F000
	ldr		r9, [r9]
	ldr		r5, [r5]
	bic		r9, #0xFF000000
	add		r9, r5
	push	{r9}
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	lsr		r4, #(8-2)
	lsr		r5, #(8+4-2)
	ldr		r4, [r9, r4]
	ldr		r5, [r9, r5]
	;-------
	; Make r9 = pointer to the font data and load r0 = first 4 font rows
	;-------
	pop		{r9}
	and		r0, #0xFF
	cmp		r0, #0x20				; Are we to write a blank (' ')?
	beq		%f1						; Yep, perform it separately
	add		r9, r0, lsl #3
	ldr		r0, [r9]				; r0 = first 4 pixel rows of the font
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	;-------
	; Get the next 4 rows of font data
	;-------
	ldr		r0, [r9, #4]
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	lsr		r0, #8
	put_font_row
	sub		r8, #(8*SCREEN_WIDTH*2)
	bx		lr
	;-------
	; Write a 8x8 byte block of background color.
	;-------
1
	;-------
	; Write 8 rows of 8 bytes of background color
	;-------
	orr		r5, r5, lsl #16
	mov		r0, r5
	mov		r4, r5
	mov		r9, r5
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	stmia	r8, {r0, r4, r5, r9}
	add		r8, #SCREEN_WIDTH*2
	sub		r8, #(8*SCREEN_WIDTH*2)
	bx		lr

	IF 0 = 1
	;-------
	; Input
	;	r0 low byte = character
	;	r0 2nd byte = attribute (low 4 bits = foreground, high 4 bits = background)
	;	r8 = address to VRAM
	; Destroys
	;	r0, r4, r5, r9
	;-------
copy_font_sub_6x8
	;-------
	; Make r4 low word = foreground color and r5 low word = background color
	;-------
	ldr		r9, =BG_PALETTE
	and		r4, r0, #(15<<8)
	and		r5, r0, #(15<<(8+4))
	ldr		r4, [r9, r4, lsr #(8-2)]
	ldr		r5, [r9, r5, lsr #(8+4-2)]
	;-------
	; Make r9 = pointer to the font data and load r0 = first 4 font rows
	;-------
	ldr		r9, =fontTiles
	and		r0, #0xFF
	cmp		r0, #0x20				; Are we to write a blank (' ')?
	beq		.copy_blank				; Yep, perform it separately
	ldr		r0, [r9, r0, lsl #3]!	; r0 = first 4 pixel rows of the font
	;-------
	; Write topmost row of the font
	;-------
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
	;-------
	; Write the second row of the font
	;-------
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
	;-------
	; Write the third row of the font
	;-------
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
	;-------
	; Write the 4th row of the font
	;-------
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
	;-------
	; Get the last 4 rows of font data
	;-------
	ldr		r0, [r9, #4]
	;-------
	; Write the 5th row of the font
	;-------
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
	;-------
	; Write the 6th row of the font
	;-------
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
	;-------
	; Write the 7th row of the font
	;-------
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
	;-------
	; Write the 8th row of the font
	;-------
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
	;-------
	; Write a 6x8 byte block of background color.
	;-------
.copy_blank
	;-------
	; Write 8 rows of 4+2 bytes of background color
	;-------
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
	ENDIF
	
;-------------------- Show hardware cursor -----------------
; Called from BlinkCursor, which is called by a C routine.
; Use the cursor position from BIOSData[0x50+2*page] and font size info.
; We can change r0..r3, must save other registers.
;	r0 contains the VRAM start addresss (s_pixels)
;
ShowCursor
	;-------
	; First determine if we are in TEXT mode
	;-------
	ldr		r3,=BIOSData
	ldr		r3, [r3]
	ldrb	r1,[r3, #SCR_MODE]		; Get current video mode
	cmp		r1, #3					; Text mode?
	bxgt	lr						; Nope, just return immediately
	;-------
	; Determine cursor position, from 00400050+2*page
	;-------
	ldrb	r1,[r3, #SCR_PAGE]		; r1 = current display page
	add		r2, r3, r1, lsl #1
	ldrb	r1,[r2, #CURSOR_ROW]	; r1 = Row
	ldrb	r2,[r2, #CURSOR_COL]	; r2 = Column
	;-------
	; Calculate VRAM address
	;-------
	IF 1 = 1
	lsl		r1, #(10+4)				; r1 = SCREEN_WIDTH*16*Row
	add		r2, r1, r2, lsl #3		; r2 = SCREEN_WIDTH*16*Row + 8*Col
	ELSE
	lsl		r1, #(10+3)				; r1 = SCREEN_WIDTH*8*Row
	lsl		r2, #1
	add		r2, r2, lsl #1			; r2 = 6*Column
	add		r2, r1
	ENDIF	
	add		r2, r0, r2, lsl #1		; r2 = screen address
	;-------
	; Calculate the cursor size.
	; [0x61] = cursor start and options
	; [0x60] = bottom scan line containing cursor (bits 0-4)
	; ReturnNothing
	;
	; Bitfields for cursor start and options
	;
	; Bit(s)  Description     (Table 00013)
	; 7       should be zero
	; 6,5     cursor blink.
	; 			(00=normal, 01=invisible, 10=erratic, 11=slow).
	; 			(00=normal, other=invisible on EGA/VGA)
	; 4-0     topmost scan line containing cursor
	;-------
	ldrb	r1,[r3, #0x60]			; r1 = Cursor ending scanline
	ldrb	r0,[r3, #0x61]			; r0 = Cursor starting scanline
	and		r1, #0x1F				; Bottom scanline only uses low 5 bits
	tst		r0, #0xE0				; Invisible cursor?
	bxne	lr						; Yep, so skip drawing it
	add		r1, #1					; Add one to bottom scanline, to actually draw it
	add		r3, r2, r1, lsl #(10+1)	; r3 = ending address in VRAM
	add		r2, r0, lsl #(10+1)		; r2 = starting address in VRAM
	cmp		r2, r3
	bxgt	lr						; Skip if start > end (SYSINFO)
	;-------
	; Draw the actual cursor
	;-------
	mvn		r1, #0
1	str		r1, [r2]
	str		r1, [r2, #4]
	str		r1, [r2, #8]
	add		r2, #SCREEN_WIDTH*2
	cmp		r2, r3
	blt		%b1
	bx		lr

;-------------------- Hide hardware cursor -----------------
; Called by a C routine, whenever the cursor position is about to change.
; Use the cursor position from BIOSData[0x50+2*page] and 6x8 font.
; We can change r0..r3, must save other registers.
;
	GLOBAL	HideCursor
HideCursor
	;-------
	; First determine if we are in TEXT mode
	;-------
	ldr		r0,=BIOSData
	ldr		r0, [r0]
	ldrb	r1,[r0, #SCR_MODE]		; Get current video mode
	cmp		r1, #3					; Text mode?
	bxgt	lr						; Nope, just return immediately
	;-------
	; Clear the CursorCounter
	;-------
	ldr		r1,=CursorCounter
	mov		r3, #0
	ldr		r2, [r1]				; Get old CursorCounter into r2
	str		r3, [r1]				; CursorCounter = 0, 
	cmp		r2, #16					; Is the cursor visible currently?
	bxlt	lr						; Nope, all done.
	;-------
	; Determine cursor position, from 00400050+2*page
	;-------
	push	{r4, r5}
	ldrb	r1,[r0, #SCR_PAGE]		; r1 = current display page
	ldrb	r5,[r0, #0x4A]			; r5 = Number of screen columns
	add		r0, r1, lsl #1
	ldrh	r4,[r0, #CURSOR_COL]	; r4 low byte = column, high byte = row
	mov		r2, r5, lsl #1			; r2 = columns*2 = 160 / 80
	mov		r1, r4, lsr #8			; r1 = row number
	and		r4, #0xFF				; r4 = column number
	mul		r3, r2, r1				; r3 = columns*2*row
	add		r5, r3, r4, lsl #1		; r5 = columns*2*row+2*col
	;-------
	; Make sure the cursor is within the visible area,
	; and skip this if it is not.
	;-------
	cmp		r5, #80*50*2
	bge		%f1
	;-------
	; Mark the position as dirty
	;-------
	ldr		r2,=TEXTBuffer
	mov		r4, #0
	strh	r4,[r2, r5]
	;-------
	; Return
	;-------
1	pop		{r4, r5}
	bx		lr

;-------------------- Blink hardware cursor -----------------
; Called by a C routine.
; Increment cursor counter, show or hide cursor depending on counter value.
; This is called from the VBlank interrupt
;	r0 = VRAM start address (s_pixels)
;
	GLOBAL	BlinkCursor
BlinkCursor
	ldr		r1,=CursorCounter
	ldr		r2, [r1]
	add		r2, #1					; Increment the counter
	str		r2, [r1]
	tst		r2, #15					; If the low 4 bits are clear, need to either show or hide the cursor, ...
	bxne	lr						; ... else just return.
	cmp		r2, #32
	bge		HideCursor				; Hide the cursors if the counter is >= 32
	b		ShowCursor				; Show the cursor if the counter = 16


	AREA	TEXT_DATA, DATA, READWRITE
	ALIGN	4

	GLOBAL	CursorCounter
CursorCounter
	DCD	0
	
	ALIGN	4
	
	GLOBAL	TEXTBuffer
TEXTBuffer
	SPACE	80*50*2				; Dirty buffer for text screen blitting

	END
