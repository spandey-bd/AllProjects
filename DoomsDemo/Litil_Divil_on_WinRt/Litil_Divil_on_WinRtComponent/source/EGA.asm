;=============================================================================
; EGA.S
;
; This file contains routines to handle EGA graphics (all 16-color graphics
; modes, including 640x480x16 VGA mode). Since the EGA memory is not linear
; but consists of four planes, this file contains separate EGA VRAM read/write
; opcode handlers that map between the x86 EGA memory order and linear VRAM
; that pax86 uses to speed up screen memory blitting. Each written byte
; (8 pixels) is expanded to a word (8 * 4 bits), and vice versa.
;
; This file also contains the screen blitting routines for all 16-color modes,
; EGA/VGA I/O port handlers, and video parameter tables.
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

	AREA EGA, CODE, READONLY

	GLOBAL	CharToEGAScreen
	GLOBAL	PixelToEGAScreen
	GLOBAL	PixelFromEGAScreen

	GLOBAL	out_3C0_VGA_attr
	GLOBAL	in_3C0_VGA_attr
	GLOBAL	in_3C1_VGA_attr
	GLOBAL	out_3C2_VGA_misc
	GLOBAL	in_3CC_VGA_misc
	GLOBAL	out_3C4_VGA_sequencer
	GLOBAL	in_3C4_VGA_sequencer
	GLOBAL	out_3C5_VGA_sequencer
	GLOBAL	in_3C5_VGA_sequencer
	
	GLOBAL	out_3C7_VGA_pel
	GLOBAL	in_3C7_VGA_DAC
	GLOBAL	out_3C8_VGA_pel
	GLOBAL	in_3C8_VGA_pel
	GLOBAL	out_3C9_VGA_pel
	GLOBAL	in_3C9_VGA_pel
	GLOBAL	out_3C9_EGA_pel
	GLOBAL	out_3CE_VGA_graphics
	GLOBAL	in_3CE_VGA_graphics
	GLOBAL	out_3CF_VGA_graphics
	GLOBAL	in_3CF_VGA_graphics
	GLOBAL	out_3D4_VGA_CRTC_addr
	GLOBAL	in_3D4_VGA_CRTC_addr
	GLOBAL	out_3D5_VGA_CRTC_data
	GLOBAL	in_3D5_VGA_CRTC_data

	EXTERN	loop
	EXTERN	restore_flags_from_r0
	EXTERN	complement_carry
	EXTERN	unknown
	EXTERN	bad_string_op_seg_back1
	EXTERN	ScreenConfigChanged

	EXTERN	registers
	
	EXTERN	EGAVGA_A000
	EXTERN	INTVectors
	EXTERN	BIOSData
	EXTERN	EMSPageTable

	EXTERN	mov_es_r2
	EXTERN	rep_movsb_cld_next
	EXTERN	rep_movsb_std_next
	EXTERN	rep_movsw_cld_next
	EXTERN	rep_movsw_std_next
	EXTERN	rep_movsd_cld_next
	EXTERN	rep_stosb_cld
	EXTERN	rep_stosb_std
	EXTERN	rep_stosw_cld
	EXTERN	rep_stosw_std
	EXTERN	repe_scasb_cld
	EXTERN	repne_scasb_cld



;
; EGA memory handling
; The four planes are interlaced so that each input byte corresponds to a word in EGAVGA_A000.
; Each bit of the input is a 4-bit nibble in the output word. The Map Mask register (EGA_MASK32)
; contains a mask telling which bit in each of these nibbles the written byte affects.
;
;


	INCLUDE ../include/defines.inc
	INCLUDE ../include/macros.inc

REG_DEBUG	EQU		0

	GLOBAL	bt_16_EGA_r2
	GLOBAL	bts_16_EGA_r2
	GLOBAL	btr_16_EGA_r2
	GLOBAL	btc_16_EGA_r2
	GLOBAL	op_89_EGA_USE32_eax
	GLOBAL	op_89_EGA_USE32_ecx
	GLOBAL	op_89_EGA_USE32_edx
	GLOBAL	op_89_EGA_USE32_ebx
	GLOBAL	op_89_EGA_USE32_esp
	GLOBAL	op_89_EGA_USE32_ebp
	GLOBAL	op_89_EGA_USE32_esi
	GLOBAL	op_89_EGA_USE32_edi
	GLOBAL	op_8b_EGA_USE32_eax
	GLOBAL	op_8b_EGA_USE32_ecx
	GLOBAL	op_8b_EGA_USE32_edx
	GLOBAL	op_8b_EGA_USE32_ebx
	GLOBAL	op_8b_EGA_USE32_esp
	GLOBAL	op_8b_EGA_USE32_ebp
	GLOBAL	op_8b_EGA_USE32_esi
	GLOBAL	op_8b_EGA_USE32_edi
	GLOBAL	op_c7_EGA_USE32_r2
	GLOBAL	test_EGA_r2_imm32
	GLOBAL	dec_word_EGA

bt_16_EGA_r2
bts_16_EGA_r2
btr_16_EGA_r2
btc_16_EGA_r2
op_89_EGA_USE32_eax
op_89_EGA_USE32_ecx
op_89_EGA_USE32_edx
op_89_EGA_USE32_ebx
op_89_EGA_USE32_esp
op_89_EGA_USE32_ebp
op_89_EGA_USE32_esi
op_89_EGA_USE32_edi
op_8b_EGA_USE32_eax
op_8b_EGA_USE32_ecx
op_8b_EGA_USE32_edx
op_8b_EGA_USE32_ebx
op_8b_EGA_USE32_esp
op_8b_EGA_USE32_ebp
op_8b_EGA_USE32_esi
op_8b_EGA_USE32_edi
op_c7_EGA_USE32_r2
test_EGA_r2_imm32
dec_word_EGA
	b		unknown

; ------------------- Common routines ---------------------------------
;

	;-------
	;	Bitu readHandler(PhysPt start) {
	;		vga.latch.d=((Bit32u*)vga.mem.linear)[start];
	;		switch (vga.config.read_mode) {
	;		case 0
	;			return (vga.latch.b[vga.config.read_map_select]);
	;		case 1
	;			VGA_Latch templatch;
	;			templatch.d=(vga.latch.d &	FillTable[vga.config.color_dont_care]) ^ FillTable[vga.config.color_compare & vga.config.color_dont_care];
	;			return (Bit8u)~(templatch.b[0] | templatch.b[1] | templatch.b[2] | templatch.b[3]);
	;		}
	;		return 0;
	;	}
	;-------
EGA_read_byte_r2
	;-------
	; On input
	;	r2 = Segment+Offset address into emulated EGA VRAM
	;	lr = return address
	; Destroys
	;	r1, r2, flags
	; Output
	;	r0 = byte value read from EGA
	;-------
	ldr		r1,=VGA_latch
	ldr		r0, [r2] 							; Get the value from our emulated EGA/VGA RAM
	ldrb	r2, [r1, #(VGAModeReg-VGA_latch)]	; Get Mode Register value
	str		r0, [r1]							; vga.latch.d = r0
	str		r0, [r1, #-4]						; vga.latch.d = r0
	tst		r2, #0x8							; Read Mode 1?
	bne		%f1									; Yep, go handle it
	;=======
	; Read Mode 0, return 8 consecutive pixels from EGA VRAM.
	;=======
	ldr		r2, [r1, #(EGA_READ_MASK32-VGA_latch)]
	and		r1, r0, r2							; Leave only the pixels in r1 that exist in the plane we are interested in.
	;-------
	; Turn on the needed pixels in AL
	;-------
	mov		r0, #0								; Init AL = 0
	tst		r1, #0xF							; Pixel 0 set?
	orrne	r0, #0x80
	tst		r1, #0xF0							; Pixel 1 set?
	orrne	r0, #0x40
	tst		r1, #0xF00							; Pixel 2 set?
	orrne	r0, #0x20
	tst		r1, #0xF000							; Pixel 3 set?
	orrne	r0, #0x10
	tst		r1, #0xF0000						; Pixel 4 set?
	orrne	r0, #0x08
	tst		r1, #0xF00000						; Pixel 5 set?
	orrne	r0, #0x04
	tst		r1, #0xF000000						; Pixel 6 set?
	orrne	r0, #0x02
	tst		r1, #0xF0000000						; Pixel 7 set?
	orrne	r0, #0x01
	;-------
	; Return
	;-------
	bx		lr
	;=======
	; Read Mode 1, return the result of a color comparison.
	;=======
1	ldr		r2, [r1, #(EGA_COLOR_COMPARE32-VGA_latch)]
	ldr		r1, [r1, #(EGA_COLOR_DONT_CARE32-VGA_latch)]
	and		r0, r1								; Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r2, r1								; Clear the "Dont Care" bits from the Color Compare value
	eor		r1, r2, r0							; Now r1 contains bits set in nibbles that fail the comparison
	;-------
	; Turn on the needed pixels in AL
	;-------
	mov		r0, #0								; Init AL = 0
	tst		r1, #0xF							; Pixel 0 set?
	orreq	r0, #0x80
	tst		r1, #0xF0							; Pixel 1 set?
	orreq	r0, #0x40
	tst		r1, #0xF00							; Pixel 2 set?
	orreq	r0, #0x20
	tst		r1, #0xF000							; Pixel 3 set?
	orreq	r0, #0x10
	tst		r1, #0xF0000						; Pixel 4 set?
	orreq	r0, #0x08
	tst		r1, #0xF00000						; Pixel 5 set?
	orreq	r0, #0x04
	tst		r1, #0xF000000						; Pixel 6 set?
	orreq	r0, #0x02
	tst		r1, #0xF0000000						; Pixel 7 set?
	orreq	r0, #0x01
	;-------
	; Return
	;-------
	bx		lr


	;-------
	;;Nice one from DosEmu
	;INLINE static Bit32u RasterOp(Bit32u input,Bit32u mask) {
	;	switch (vga.config.raster_op) {
	;	case 0x00:	/* None */
	;		return (input & mask) | (vga.latch.d & ~mask);
	;	case 0x01:	/* AND */
	;		return (input | ~mask) & vga.latch.d;
	;	case 0x02:	/* OR */
	;		return (input & mask) | vga.latch.d;
	;	case 0x03:	/* XOR */
	;		return (input & mask) ^ vga.latch.d;
	;	};
	;	return 0;
	;}
	;
	;INLINE static Bit32u ModeOperation(Bit8u val) {
	;	Bit32u full;
	;	switch (vga.config.write_mode) {
	;	case 0x00
	;		; Write Mode 0: In this mode, the host data is first rotated as per the Rotate Count field,
	;		; then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	;		; Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;		; Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;		; Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;		full=ExpandTable[val];
	;		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	;		full=RasterOp(full,vga.config.full_bit_mask);
	;		break;
	;	case 0x01
	;		; Write Mode 1: In this mode, data is transferred directly from the 32 bit latch register to display memory,
	;		; affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	;		full=vga.latch.d;
	;		break;
	;	case 0x02
	;		; Write Mode 2: In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	;		; Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;		; Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;		; Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;		full=RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	;		break;
	;	case 0x03
	;		; Write Mode 3: In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	;		; Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	;		; The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	;		; to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	;		; Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	;		full=RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	;		break;
	;	default
	;		LOG(LOG_VGAMISC,LOG_NORMAL)("VGA:Unsupported write mode %d",vga.config.write_mode);
	;		full=0;
	;		break;
	;	}
	;	return full;
	;}
	;
	;		Bit32u data=ModeOperation(val);
	;		/* Update video memory and the pixel buffer */
	;		VGA_Latch pixels;
	;		pixels.d=((Bit32u*)vga.mem.linear)[start];
	;		pixels.d&=vga.config.full_not_map_mask;
	;		pixels.d|=(data & vga.config.full_map_mask);
	;		((Bit32u*)vga.mem.linear)[start]=pixels.d;
	;		Bit8u * write_pixels=&vga.fastmem[start<<3];
	;-------

	;=======
	; On input
	;	r0 = byte to write (higher bits not used)
	;	r1 = free
	;	r2 = Full Segment+Offset address into EGA VRAM
	;	lr = return address
	; Return
	;	r0 = 32-bit value written to [r2]
	; Destroys
	;	r1, flags
	;=======
	GLOBAL	EGA_write_byte_from_C
EGA_write_byte_from_C
	mov		r2, r1
	GLOBAL	EGA_write_byte_r0_to_r2
EGA_write_byte_r0_to_r2
	ldr		r1, =byte2wordtbl
	push	{r3}
	push	{r2}								; Save the r2 register containing the output address
	ldrb	r2, [r1, #(VGAModeReg-byte2wordtbl)]; Get Mode Register value
	ands	r2, #3								; r2 = Write Mode
	bne		not_mode0							; Jump if Write Mode is not zero
	;=======
	; Write Mode 0
	;	In this mode, the host data is first rotated as per the Rotate Count field,
	;	then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	;	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;
	;		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;		full=ExpandTable[val];
	;		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	;		full=RasterOp(full,vga.config.full_bit_mask);
	;		break;
	;=======
	;-------
	; r0 = val = ((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	and		r0, #0xFF
	and		r2, #7
	mov		r3, r0, lsr r2
	rsb		r2, #8
	mov		r0, r0, lsl r2
	orr		r3, r0
	and		r0, r3, #0xFF						; val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;-------
	; r0 = full = ExpandTable[val];
	;-------
	ldr		r0, [r1, r0, lsl #2]				; full=ExpandTable[val];
	;-------
	; r0 = full = (full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset;
	;-------
	ldr		r2, [r1, #(EGA_ENABLE_SET_RESET32-byte2wordtbl)]
	ldr		r3, [r1, #(EGA_SET_RESET32-byte2wordtbl)]
	bic		r0, r2
	and		r3, r2
	orr		r0, r3								; full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	;-------
	; r0 = full = RasterOp(full,vga.config.full_bit_mask);
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	ands	r2, #0x18
	bne		not_raster_op_mov
raster_op_mov
	;-------
	; MOV: return (input & mask) | (vga.latch.d & ~mask);
	;-------
	ldr		r2, [r1, #(VGA_latch-byte2wordtbl)]
	and		r0, r3
	bic		r2, r3
	orr		r0, r2
	;-------
	; VGA_Latch pixels;
	; pixels.d=((Bit32u*)vga.mem.linear)[start];
	; pixels.d&=vga.config.full_not_map_mask;
	; pixels.d|=(data & vga.config.full_map_mask);
	;-------
mode1_cont
	pop		{r2}
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
	ldr		r3, [r2]
	and		r0, r1								; data &= vga.config.full_map_mask;
	bic		r3, r1								; pixels.d&=vga.config.full_not_map_mask;
	orr		r0, r3								; pixels.d|=(data & vga.config.full_map_mask);
	;-------
	; ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	;-------
	str		r0, [r2]
	pop		{r3}
	;-------
	; Return
	;-------
	bx		lr
	
	GLOBAL not_raster_op_mov
not_raster_op_mov
	cmp		r2, #0x10
	;-------
	; AND: return (input | ~mask) & vga.latch.d;	(lt)
	; OR: return (input & mask) | vga.latch.d;		(eq)
	; XOR: return (input & mask) ^ vga.latch.d;		(gt)
	;-------
	ldr		r2, [r1, #(VGA_latch-byte2wordtbl)]
	mvnlt	r3, r3								; AND: (mask = ~mask)
	orrlt	r0, r3								; AND: (input | mask)
	andge	r0, r3								; OR/XOR: (input & mask)
	andlt	r0, r2								; AND
	orreq	r0, r2								; OR
	eorgt	r0, r2								; XOR
	;-------
	; VGA_Latch pixels;
	; pixels.d=((Bit32u*)vga.mem.linear)[start];
	; pixels.d&=vga.config.full_not_map_mask;
	; pixels.d|=(data & vga.config.full_map_mask);
	;-------
	pop		{r2}
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
	ldr		r3, [r2]
	and		r0, r1
	bic		r3, r1
	orr		r0, r3
	;-------
	; ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	;-------
	str		r0, [r2]
	pop		{r3}
	;-------
	; Return
	;-------
	bx		lr

	;=======
	; Write Mode > 0
	;=======
not_mode0
	cmp		r2, #2								; Write Mode <2 == 1, == 2, or >2 == 3?
	;=======
	; Write Mode 1
	;	In this mode, data is transferred directly from the 32 bit latch register to display memory,
	;	affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	; r0 = full = vga.latch.d;
	;=======
	ldrlt	r0, [r1, #(VGA_latch-byte2wordtbl)]
	blt		mode1_cont
	bgt		mode3
	;=======
	; Write Mode 2
	;	In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	;	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;=======
	;-------
	; r0 = full = RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	and		r0, #15
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	ands	r2, #0x18
	bne		not_raster_op_mov
	b		raster_op_mov

	;=======
	; Write Mode 3
	;	In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	;	Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	;	The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	;	to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	;=======
mode3
	;-------
	; r0 = full = RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	;-------
	and		r0, #0xFF
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]		; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	ldr		r0, [r1, r0, lsl #2]
	and		r3, r0										; r3 = ExpandTable[val] & vga.config.full_bit_mask
	ldr		r0, [r1, #(EGA_SET_RESET32-byte2wordtbl)]	; r0 = vga.config.full_set_reset
	ands	r2, #0x18
	bne		not_raster_op_mov
	b		raster_op_mov
	
EGA_read_hword_r2
	;-------
	; On input
	;	r2 = Segment+Offset address into emulated EGA VRAM
	;	lr = return address
	; Destroys
	;	r1, r2
	; Output
	;	r0 = halfword value read from EGA
	;-------
	push	{r3}
	ldr		r1,=VGA_latch_minus_4
	ldmia	r2, {r0, r3}						; Get the value from our emulated EGA/VGA RAM
	ldrb	r2, [r1, #(VGAModeReg-VGA_latch_minus_4)]	; Get Mode Register value
	stmia	r1, {r0, r3}						; vga.latch.d = r3 = the last read "byte"
	tst		r2, #0x8							; Read Mode 1?
	bne		%f1									; Yep, go handle it
	;=======
	; Read Mode 0, return 16 consecutive pixels from EGA VRAM.
	;=======
	ldr		r2, [r1, #(EGA_READ_MASK32-VGA_latch_minus_4)]
	and		r1, r0, r2							; Leave only the pixels in r1 that exist in the plane we are interested in.
	and		r3, r2								; Leave only the pixels in r3 that exist in the plane we are interested in.
	;-------
	; Turn on the needed pixels in AX
	;-------
	mov		r0, #0
	tst		r1, #0xF				; Pixel 0 set?
	orrne	r0, #0x0080
	tst		r1, #0xF0				; Pixel 1 set?
	orrne	r0, #0x0040
	tst		r1, #0xF00				; Pixel 2 set?
	orrne	r0, #0x0020
	tst		r1, #0xF000				; Pixel 3 set?
	orrne	r0, #0x0010
	tst		r1, #0xF0000			; Pixel 4 set?
	orrne	r0, #0x0008
	tst		r1, #0xF00000			; Pixel 5 set?
	orrne	r0, #0x0004
	tst		r1, #0xF000000			; Pixel 6 set?
	orrne	r0, #0x0002
	tst		r1, #0xF0000000			; Pixel 7 set?
	orrne	r0, #0x0001
	tst		r3, #0xF				; Pixel 0 set?
	orrne	r0, #0x8000
	tst		r3, #0xF0				; Pixel 1 set?
	orrne	r0, #0x4000
	tst		r3, #0xF00				; Pixel 2 set?
	orrne	r0, #0x2000
	tst		r3, #0xF000				; Pixel 3 set?
	orrne	r0, #0x1000
	tst		r3, #0xF0000			; Pixel 4 set?
	orrne	r0, #0x0800
	tst		r3, #0xF00000			; Pixel 5 set?
	orrne	r0, #0x0400
	tst		r3, #0xF000000			; Pixel 6 set?
	orrne	r0, #0x0200
	tst		r3, #0xF0000000			; Pixel 7 set?
	orrne	r0, #0x0100
	;-------
	; Return
	;-------
	pop		{r3}
	bx		lr
	;=======
	; Read Mode 1, return the result of a color comparison.
	;=======
1	ldr		r2, [r1, #(EGA_COLOR_COMPARE32-VGA_latch_minus_4)]
	ldr		r1, [r1, #(EGA_COLOR_DONT_CARE32-VGA_latch_minus_4)]
	and		r0, r1								; Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r3, r1								; Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r2, r1								; Clear the "Dont Care" bits from the Color Compare value
	eor		r1, r2, r0							; Now r1 contains bits set in nibbles that fail the comparison
	eor		r3, r2								; Now r3 contains bits set in nibbles that fail the comparison
	;-------
	; Turn on the needed pixels in AX
	;-------
	mov		r0, #0
	tst		r1, #0xF				; Pixel 0 set?
	orreq	r0, #0x0080
	tst		r1, #0xF0				; Pixel 1 set?
	orreq	r0, #0x0040
	tst		r1, #0xF00				; Pixel 2 set?
	orreq	r0, #0x0020
	tst		r1, #0xF000				; Pixel 3 set?
	orreq	r0, #0x0010
	tst		r1, #0xF0000			; Pixel 4 set?
	orreq	r0, #0x0008
	tst		r1, #0xF00000			; Pixel 5 set?
	orreq	r0, #0x0004
	tst		r1, #0xF000000			; Pixel 6 set?
	orreq	r0, #0x0002
	tst		r1, #0xF0000000			; Pixel 7 set?
	orreq	r0, #0x0001
	tst		r3, #0xF				; Pixel 0 set?
	orreq	r0, #0x8000
	tst		r3, #0xF0				; Pixel 1 set?
	orreq	r0, #0x4000
	tst		r3, #0xF00				; Pixel 2 set?
	orreq	r0, #0x2000
	tst		r3, #0xF000				; Pixel 3 set?
	orreq	r0, #0x1000
	tst		r3, #0xF0000			; Pixel 4 set?
	orreq	r0, #0x0800
	tst		r3, #0xF00000			; Pixel 5 set?
	orreq	r0, #0x0400
	tst		r3, #0xF000000			; Pixel 6 set?
	orreq	r0, #0x0200
	tst		r3, #0xF0000000			; Pixel 7 set?
	orreq	r0, #0x0100
	;-------
	; Return
	;-------
	pop		{r3}
	bx		lr


	;=======
	; On input
	;	r0 = hword to write (higher bits not used)
	;	r1 = free
	;	r2 = Full Segment+Offset address into EGA VRAM
	;	lr = return address
	; Destroys
	;	r0, r1
	;=======
	GLOBAL EGA_write_hword_r0_to_r2
EGA_write_hword_r0_to_r2
	ldr		r1, =byte2wordtbl
	push	{r3-r5}
	push	{r2}								; Save the r2 register containing the output address
	ldrb	r2, [r1, #(VGAModeReg-byte2wordtbl)]; Get Mode Register value
	ands	r2, #3								; r2 = Write Mode
	bne		not_mode0_hw						; Jump if Write Mode is not zero
	;=======
	; Write Mode 0
	;	In this mode, the host data is first rotated as per the Rotate Count field,
	;	then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	;	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;
	;		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;		full=ExpandTable[val];
	;		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	;		full=RasterOp(full,vga.config.full_bit_mask);
	;		break;
	;=======
	;-------
	; r0 = val = ((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	and		r2, #7
	mov		r4, r0, lsr #8						; r4 = high byte of val
	and		r0, #0xFF							; r0 = low byte of val
	and		r4, #0xFF
	mov		r3, r0, lsr r2
	mov		r5, r4, lsr r2
	rsb		r2, #8
	mov		r0, r0, lsl r2
	mov		r4, r4, lsl r2
	orr		r3, r0
	orr		r5, r4
	and		r0, r3, #0xFF						; val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	and		r4, r5, #0xFF						; val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	;-------
	; r0 = full = ExpandTable[val];
	;-------
	ldr		r0, [r1, r0, lsl #2]				; full=ExpandTable[val];
	ldr		r4, [r1, r4, lsl #2]				; full=ExpandTable[val];
	;-------
	; r0 = full = (full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset;
	;-------
	ldr		r2, [r1, #(EGA_ENABLE_SET_RESET32-byte2wordtbl)]
	ldr		r3, [r1, #(EGA_SET_RESET32-byte2wordtbl)]
	bic		r0, r2
	bic		r4, r2
	and		r3, r2
	orr		r0, r3								; full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	orr		r4, r3								; full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	;-------
	; r0 = full = RasterOp(full,vga.config.full_bit_mask);
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	ands	r2, #0x18
	bne		not_raster_op_mov_hw
raster_op_mov_hw
	;-------
	; MOV: return (input & mask) | (vga.latch.d & ~mask);
	;-------
	ldr		r2, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldr		r5, [r1, #(VGA_latch-byte2wordtbl)]
	and		r0, r3
	and		r4, r3
	bic		r2, r3
	bic		r5, r3
	orr		r0, r2
	orr		r4, r5
	;-------
	; VGA_Latch pixels;
	; pixels.d=((Bit32u*)vga.mem.linear)[start];
	; pixels.d&=vga.config.full_not_map_mask;
	; pixels.d|=(data & vga.config.full_map_mask);
	;-------
mode1_cont_hw
	pop		{r2}
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
	ldmia	r2, {r3, r5}
	and		r0, r1
	and		r4, r1
	bic		r3, r1
	bic		r5, r1
	orr		r0, r3
	orr		r4, r5
	;-------
	; ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	;-------
	stmia	r2, {r0, r4}
	pop		{r3-r5}
	;-------
	; Return
	;-------
	bx		lr
	
not_raster_op_mov_hw
	cmp		r2, #0x10
	;-------
	; AND: return (input | ~mask) & vga.latch.d;
	; OR: return (input & mask) | vga.latch.d;
	; XOR: return (input & mask) ^ vga.latch.d;
	;-------
	ldr		r2, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldr		r5, [r1, #(VGA_latch-byte2wordtbl)]
	mvnlt	r3, r3								; AND: (mask = ~mask)
	orrlt	r0, r3								; AND: (input | mask)
	orrlt	r4, r3								; AND: (input | mask)
	andge	r0, r3								; OR/XOR: (input & mask)
	andge	r4, r3								; OR/XOR: (input & mask)
	andlt	r0, r2								; AND
	andlt	r4, r5								; AND
	orreq	r0, r2								; OR
	orreq	r4, r5								; OR
	eorgt	r0, r2								; XOR
	eorgt	r4, r5								; XOR
	;-------
	; VGA_Latch pixels;
	; pixels.d=((Bit32u*)vga.mem.linear)[start];
	; pixels.d&=vga.config.full_not_map_mask;
	; pixels.d|=(data & vga.config.full_map_mask);
	;-------
	pop		{r2}
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
	ldmia	r2, {r3, r5}
	and		r0, r1
	and		r4, r1
	bic		r3, r1
	bic		r5, r1
	orr		r0, r3
	orr		r4, r5
	;-------
	; ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	;-------
	stmia	r2, {r0, r4}
	pop		{r3-r5}
	;-------
	; Return
	;-------
	bx		lr

	;=======
	; Write Mode > 0
	;=======
not_mode0_hw
	cmp		r2, #2								; Write Mode <2 == 1, == 2, or >2 == 3?
	;=======
	; Write Mode 1
	;	In this mode, data is transferred directly from the 32 bit latch register to display memory,
	;	affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	; r0 = full = vga.latch.d;
	;=======
	ldrlt	r0, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldrlt	r4, [r1, #(VGA_latch-byte2wordtbl)]
	blt		mode1_cont_hw
	bgt		mode3_hw
	;=======
	; Write Mode 2
	;	In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	;	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	;	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	;
	;	TODO! This does not work properly for 16-bit input value!
	;=======
	;-------
	; r0 = full = RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	;-------
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	and		r0, #15
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	mov		r4, r0
	ands	r2, #0x18
	bne		not_raster_op_mov_hw
	b		raster_op_mov_hw

	;=======
	; Write Mode 3
	;	In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	;	Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	;	The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	;	to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	;	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	;
	;	TODO! This does not work properly for 16-bit input value!
	;=======
mode3_hw
	;-------
	; r0 = full = RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	;-------
	and		r0, #0xFF									; r0 = low byte of val
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]		; Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
	ldr		r0, [r1, r0, lsl #2]
	and		r3, r0										; r3 = ExpandTable[val] & vga.config.full_bit_mask
	ldr		r0, [r1, #(EGA_SET_RESET32-byte2wordtbl)]	; r0 = vga.config.full_set_reset
	mov		r4, r0
	ands	r2, #0x18
	bne		not_raster_op_mov_hw
	b		raster_op_mov_hw

	ALIGN	4

	;-------
	; This is jumped to by handlers that need to pop flags before returning.
	;-------
pop_back_2
	pop		{r0}
	msr		cpsr_f,r0				; Restore flags
	b		unknown
pop2_back_2
	pop		{r0, lr}
	msr		cpsr_f,r0				; Restore flags
	b		unknown


	LTORG

; ------------------- 00 = ADD r/m8, r8 -------------------------------
;

	MACRO 
	op_00_EGA_reg8l $reg
	GLOBAL op_00_EGA_l_$reg
op_00_EGA_l_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, $reg, lsl #24
	adds	r0, r1, r0, lsl #24
	mov		r2, r3					; Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND
	MACRO 
	op_00_EGA_reg8h $reg
	GLOBAL op_00_EGA_h_$reg
op_00_EGA_h_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	adds	r0, r1, r0, lsl #24
	mov		r2, r3					; Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_00_EGA_reg8l eax
	op_00_EGA_reg8l ecx
	op_00_EGA_reg8l edx
	op_00_EGA_reg8l ebx
	op_00_EGA_reg8h eax
	op_00_EGA_reg8h ecx
	op_00_EGA_reg8h edx
	op_00_EGA_reg8h ebx

; ------------------- 01 = ADD r/m16, r16 -----------------------------
;
; 26010E0200 	add es:[0002],cx	(LHX)
;
	MACRO 
	op_01_EGA_r2_reg $reg
	GLOBAL op_01_EGA_r2_$reg
op_01_EGA_r2_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2		; r0 = hword from [r2]
	lsl		r0, #16
	adds	r0, $reg, lsl #16		; r0 = [RAM] + reg
	mov		r2, r3					; restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	lsr		r0, #16
	bl		EGA_write_hword_r0_to_r2 ; r0 to hword at [r2]
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_01_EGA_r2_reg eax
	op_01_EGA_r2_reg ecx
	op_01_EGA_r2_reg edx
	op_01_EGA_r2_reg ebx
	op_01_EGA_r2_reg esp
	op_01_EGA_r2_reg ebp
	op_01_EGA_r2_reg esi
	op_01_EGA_r2_reg edi


; ------------------- 03 = ADD r16, r/m16 ------------------------------
;
; 2603060200 	add ax,es:[0002] 	(LHX)
;
	MACRO 
	op_03_EGA_reg $reg
	GLOBAL op_03_EGA_$reg
op_03_EGA_$reg
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r2, $reg, lsl #16
	eor		$reg, r2, lsr #16		; Clear the 16-bit register value
	adds	r0, r2, r0, lsl #16		; Perform the operation using the high 16 bits
	orr		$reg, r0, lsr #16		; Put the result to the 16-bit register
	b		loop
	MEND

	op_03_EGA_reg eax
	op_03_EGA_reg ecx
	op_03_EGA_reg edx
	op_03_EGA_reg ebx
	op_03_EGA_reg esp
	op_03_EGA_reg ebp
	op_03_EGA_reg esi
	op_03_EGA_reg edi

	LTORG

; ------------------- 08 = OR r/m8,r8 ---------------------------------
; 260885401F 	or es:[di+1F40],al (Ultima)
;
	MACRO 
	op_08_EGA_reg8l $reg
	GLOBAL op_08_EGA_l_$reg
op_08_EGA_l_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	lsl		r0, #24
	orrs	r0, r1
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND
	MACRO 
	op_08_EGA_reg8h $reg
	GLOBAL op_08_EGA_h_$reg
op_08_EGA_h_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	lsl		r0, #24
	orrs	r0, r1
	mov		r2, r3					; Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_08_EGA_reg8l eax
	op_08_EGA_reg8l ecx
	op_08_EGA_reg8l edx
	op_08_EGA_reg8l ebx
	op_08_EGA_reg8h eax
	op_08_EGA_reg8h ecx
	op_08_EGA_reg8h edx
	op_08_EGA_reg8h ebx
	
; ------------------- 09 = OR r/m16,r16 -------------------------------
;
; 260905 	or es:[di],ax
; 26094528  or es:[di+28],ax (Bards Tale)
;
	MACRO 
	op_09_EGA_r2_reg $reg
	GLOBAL op_09_EGA_r2_$reg
op_09_EGA_r2_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	orrs	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_09_EGA_r2_reg eax
	op_09_EGA_r2_reg ecx
	op_09_EGA_r2_reg edx
	op_09_EGA_r2_reg ebx
	op_09_EGA_r2_reg esp
	op_09_EGA_r2_reg ebp
	op_09_EGA_r2_reg esi
	op_09_EGA_r2_reg edi

; ------------------- 0A = OR r8,r/m8 -------------------------------
;
	MACRO 
	op_0a_EGA_reg8l $reg
	GLOBAL op_0a_EGA_l_$reg
op_0a_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	orrs	r0, r1, r0, lsl #24
	bic		$reg, #0xFF
	orr		$reg, r0, lsr #24
	b		loop
	MEND
	MACRO 
	op_0a_EGA_reg8h $reg
	GLOBAL op_0a_EGA_h_$reg
op_0a_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	orrs	r0, r1, r0, lsl #24
	bic		$reg, #0xFF00
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_0a_EGA_reg8l eax
	op_0a_EGA_reg8l ecx
	op_0a_EGA_reg8l edx
	op_0a_EGA_reg8l ebx
	op_0a_EGA_reg8h eax
	op_0a_EGA_reg8h ecx
	op_0a_EGA_reg8h edx
	op_0a_EGA_reg8h ebx

; ------------------- 0B = OR r16,r/m16 -------------------------------
;
	MACRO 
	op_0b_EGA_reg $reg
	GLOBAL op_0b_EGA_$reg
op_0b_EGA_$reg
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r3, $reg, lsl #16
	eor		$reg, r3, lsr #16
	orrs	r0, r3, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_0b_EGA_reg eax
	op_0b_EGA_reg ecx
	op_0b_EGA_reg edx
	op_0b_EGA_reg ebx
	op_0b_EGA_reg esp
	op_0b_EGA_reg ebp
	op_0b_EGA_reg esi
	op_0b_EGA_reg edi

	LTORG
	
; ------------------- 20 = AND r/m8,r8 --------------------------------
;
; 26204600 	and es:[bp],al
; 262027 	and es:[bx],ah
;
	MACRO 
	op_20_EGA_reg8l $reg
	GLOBAL op_20_EGA_l_$reg
op_20_EGA_l_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	ands	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND
	MACRO 
	op_20_EGA_reg8h $reg
	GLOBAL op_20_EGA_h_$reg
op_20_EGA_h_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	ands	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_20_EGA_reg8l eax
	op_20_EGA_reg8l ecx
	op_20_EGA_reg8l edx
	op_20_EGA_reg8l ebx
	op_20_EGA_reg8h eax
	op_20_EGA_reg8h ecx
	op_20_EGA_reg8h edx
	op_20_EGA_reg8h ebx


; ------------------- 21 = AND r/m16,r16 ------------------------------
; 262105 	and  es:[di],ax (BARD,DK)
; 26214528 	and  es:[di+28],ax (BARD,DK)
;
	MACRO 
	op_21_EGA_r2_reg $reg
	GLOBAL op_21_EGA_r2_$reg
op_21_EGA_r2_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	ands	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_21_EGA_r2_reg eax
	op_21_EGA_r2_reg ecx
	op_21_EGA_r2_reg edx
	op_21_EGA_r2_reg ebx
	op_21_EGA_r2_reg esp
	op_21_EGA_r2_reg ebp
	op_21_EGA_r2_reg esi
	op_21_EGA_r2_reg edi


; ------------------- 22 = AND r8,r/m8 --------------------------------
;
	MACRO 
	op_22_EGA_reg8l $reg
	GLOBAL op_22_EGA_l_$reg
op_22_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	ands	r0, r1, r0, lsl #24
	bic		$reg, #0xFF
	orr		$reg, r0, lsr #24
	b		loop
	MEND
	MACRO 
	op_22_EGA_reg8h $reg
	GLOBAL op_22_EGA_h_$reg
op_22_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	ands	r0, r1, r0, lsl #24
	bic		$reg, #0xFF00
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_22_EGA_reg8l eax
	op_22_EGA_reg8l ecx
	op_22_EGA_reg8l edx
	op_22_EGA_reg8l ebx
	op_22_EGA_reg8h eax
	op_22_EGA_reg8h ecx
	op_22_EGA_reg8h edx
	op_22_EGA_reg8h ebx


; ------------------- 23 = AND r16,r/m16 --------------------------------
;
	MACRO 
	op_23_EGA_reg $reg
	GLOBAL op_23_EGA_$reg
op_23_EGA_$reg
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r3, $reg, lsl #16
	eor		$reg, r3, lsr #16
	ands	r0, r3, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_23_EGA_reg eax
	op_23_EGA_reg ecx
	op_23_EGA_reg edx
	op_23_EGA_reg ebx
	op_23_EGA_reg esp
	op_23_EGA_reg ebp
	op_23_EGA_reg esi
	op_23_EGA_reg edi

	LTORG
	
; ------------------- 30 = XOR r/m8, r8 -------------------------------
;
; 26301D          xor  es:[di],bl (Wasteland)
; 304501          xor  [di+01],al (Supaplex)
;
	MACRO 
	op_30_EGA_reg8l $reg
	GLOBAL op_30_EGA_l_$reg
op_30_EGA_l_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	eors	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND
	MACRO 
	op_30_EGA_reg8h $reg
	GLOBAL op_30_EGA_h_$reg
op_30_EGA_h_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	eors	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_30_EGA_reg8l eax
	op_30_EGA_reg8l ecx
	op_30_EGA_reg8l edx
	op_30_EGA_reg8l ebx
	op_30_EGA_reg8h eax
	op_30_EGA_reg8h ecx
	op_30_EGA_reg8h edx
	op_30_EGA_reg8h ebx


; ------------------- 31 = XOR r/m16,r16 ------------------------------
; 3105      xor  [di],ax (BARD, DK)
;
	MACRO 
	op_31_EGA_r2_reg $reg
	GLOBAL op_31_EGA_r2_$reg
op_31_EGA_r2_$reg
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	eors	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op_31_EGA_r2_reg eax
	op_31_EGA_r2_reg ecx
	op_31_EGA_r2_reg edx
	op_31_EGA_r2_reg ebx
	op_31_EGA_r2_reg esp
	op_31_EGA_r2_reg ebp
	op_31_EGA_r2_reg esi
	op_31_EGA_r2_reg edi


; ------------------- 32 = XOR r8, r/m8 -----------------------------
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual eors operation, so C and O work like in x86.
;
; 263205 	xor al,es:[di] 	(bob_fossil) 
;
	MACRO 
	op_32_EGA_reg8l $reg
	GLOBAL op_32_EGA_l_$reg
op_32_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	eors	r0, r1, r0, lsl #24
	bic		$reg, #0xFF
	orr		$reg, r0, lsr #24
	b		loop
	MEND
	MACRO 
	op_32_EGA_reg8h $reg
	GLOBAL op_32_EGA_h_$reg
op_32_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	eors	r0, r1, r0, lsl #24
	bic		$reg, #0xFF00
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_32_EGA_reg8l eax
	op_32_EGA_reg8l ecx
	op_32_EGA_reg8l edx
	op_32_EGA_reg8l ebx
	op_32_EGA_reg8h eax
	op_32_EGA_reg8h ecx
	op_32_EGA_reg8h edx
	op_32_EGA_reg8h ebx

; ------------------- 33 = XOR r16, r/m16 -----------------------------
;
	MACRO 
	op_33_EGA_reg $reg
	GLOBAL op_33_EGA_$reg
op_33_EGA_$reg
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r3, $reg, lsl #16
	eor		$reg, r3, lsr #16
	eors	r0, r3, r0, lsl #16
	orr		$reg, r0, lsr #16
	b		loop
	MEND

	op_33_EGA_reg eax
	op_33_EGA_reg ecx
	op_33_EGA_reg edx
	op_33_EGA_reg ebx
	op_33_EGA_reg esp
	op_33_EGA_reg ebp
	op_33_EGA_reg esi
	op_33_EGA_reg edi

	LTORG
	

; ------------------- 38 = CMP r/m8,r8 --------------------------------
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
; 263804 	cmp es:[si],al 	(lilalurl, OE)
; 263801    cmp  es:[bx+di],al	(X)
;
	MACRO 
	op_38_EGA_reg8l $reg
	GLOBAL op_38_EGA_l_$reg
op_38_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	lsl		r0, #24
	cmp		r0, $reg, lsl #24		; Compare the values using the high bytes
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND
	MACRO 
	op_38_EGA_reg8h $reg
	GLOBAL op_38_EGA_h_$reg
op_38_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, $reg, #0xFF00
	lsl		r1, #16
	rsbs	r1, r0, lsl #24
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	op_38_EGA_reg8l eax
	op_38_EGA_reg8l ecx
	op_38_EGA_reg8l edx
	op_38_EGA_reg8l ebx
	op_38_EGA_reg8h eax
	op_38_EGA_reg8h ecx
	op_38_EGA_reg8h edx
	op_38_EGA_reg8h ebx

	LTORG

; ------------------- 39 = CMP r/m16,r16 ------------------------------
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
; 26390E0200      cmp  es:[0002],cx		(LHX)
;
	MACRO 
	op_39_EGA_r2_reg $reg
	GLOBAL op_39_EGA_r2_$reg
op_39_EGA_r2_$reg
	calc_ega_r2
	bl		EGA_read_hword_r2
	lsl		r0, #16
	cmp		r0, $reg, lsl #16		; Compare the values using the high bytes
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	op_39_EGA_r2_reg eax
	op_39_EGA_r2_reg ecx
	op_39_EGA_r2_reg edx
	op_39_EGA_r2_reg ebx
	op_39_EGA_r2_reg esp
	op_39_EGA_r2_reg ebp
	op_39_EGA_r2_reg esi
	op_39_EGA_r2_reg edi

	
; ------------------- 3A = CMP r8,r/m8 --------------------------------
;
; 263A05 	cmp  al,es:[di] 	(drdim:START)
; 263A6501  cmp  ah,es:[di+01]
;
; ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
;
	MACRO 
	op_3a_EGA_reg8l $reg
	GLOBAL op_3a_EGA_l_$reg
op_3a_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, $reg, lsl #24		; r1 = AL in high byte
	cmp		r1, r0, lsl #24			; Compare the values using the high bytes
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND
	MACRO 
	op_3a_EGA_reg8h $reg
	GLOBAL op_3a_EGA_h_$reg
op_3a_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, $reg, #0xFF00		; r1 = AH in high byte
	lsl		r1, #16
	cmp		r1, r0, lsl #24			; Compare the values using the high bytes
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
	MEND

	op_3a_EGA_reg8l eax
	op_3a_EGA_reg8l ecx
	op_3a_EGA_reg8l edx
	op_3a_EGA_reg8l ebx
	op_3a_EGA_reg8h eax
	op_3a_EGA_reg8h ecx
	op_3a_EGA_reg8h edx
	op_3a_EGA_reg8h ebx


; ------------------- 80 = ??? r/m8,imm8 ------------------------------
;
; 26800FFF 		or   byte es:[bx],FF 		(midwinter)
; 26803C18 		cmp  byte es:[si],18 		(09-feb)
; 26807C2818 	cmp  byte es:[si+28],18 	(09-feb)
; 268037FF 		xor  byte es:[bx],FF
;
	;-------
	; Add: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_add
op_80_EGA_add
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	ldrb	r1,[r12], #1			; Load the imm8 byte to r1
	lsl		r0, #24
	adds	r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop

	;-------
	; Adc: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_adc
op_80_EGA_adc
	calc_ega_r2
	mrs		r0,cpsr					; Save flags to r0 (for input Carry)
	mov		r3, r2					; Save original r2
	str		r0, [sp, #SP_STR_SEG]	; Save flags to stack
	bl		EGA_read_byte_r2
	ldr		r1, [sp, #SP_STR_SEG]	; Get flags from stack
	lsl		r0, #24
	msr		cpsr_f, r1				; Set the CPU flags
	ldrb	r1,[r12], #1			; Load the imm8 byte to r1
	addcs	r0, #0x01000000			; If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			; ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #24			; Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop

	;-------
	; Sub: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_sub
op_80_EGA_sub
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldrb	r1,[r12], #1			; Load the imm8 byte to r1
	lsl		r0, #24
	subs	r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		complement_carry

	;-------
	; Sbb: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_sbb
op_80_EGA_sbb
	calc_ega_r2
	mrs		r0,cpsr					; Save flags to r0 (for input Carry)
	mov		r3, r2					; Save original r2
	str		r0, [sp, #SP_STR_SEG]	; Save flags to stack
	bl		EGA_read_byte_r2
	ldr		r1, [sp, #SP_STR_SEG]	; Get flags from stack
	lsl		r0, #24
	msr		cpsr_f, r1				; Set the CPU flags
	ldrb	r1,[r12], #1			; Load the imm8 byte to r1
	lsl		r1, #24
	addscs	r1, #0x01000000			; If input Carry is set, the right operand = (register value + 1).
	subs	r0, r1					; Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		complement_carry

	;-------
	; Cmp: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_cmp
op_80_EGA_cmp
	calc_ega_r2
	ldrb	r3,[r12], #1			; Load the imm8 byte to r3
	bl		EGA_read_byte_r2
	lsl		r3, #24
	rsbs	r3, r0, lsl #24			; Compare the values using the high bytes
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

	MACRO 
	op80EGAlogical $x86oper, $armoper
	;-------
	; Memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_80_EGA_$x86oper
op_80_EGA_$x86oper
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	ldrb	r1,[r12], #1			; Load the imm8 byte to r1
	lsl		r0, #24
	$armoper r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op80EGAlogical or, orrs
	op80EGAlogical and, ands
	op80EGAlogical xor, eors

; ------------------- 81 = ??? r/m16,imm16 ----------------------------
; 
; 268125FC00      and  word es:[di],00FC	(Spacewrecked)
;
	GLOBAL	op_81_EGA_add
op_81_EGA_add
	GLOBAL	op_81_EGA_sub
op_81_EGA_sub
	GLOBAL	op_81_EGA_adc
op_81_EGA_adc
	GLOBAL	op_81_EGA_sbb
op_81_EGA_sbb
	b		unknown

	MACRO
	op81EGAlogical $x86oper, $armoper
	;-------
	; Memory location in EGA VRAM.
	; On input:
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_81_EGA_$x86oper
op_81_EGA_$x86oper
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2
	ldrb	r1, [r12], #2			; r1 = low byte of imm16 value
	ldrb	r2, [r12, #-1]			; r2 = high byte of imm16 value
	lsl		r0, #16					; r0 = EGA value in high halfword
	lsl		r1, #16
	orr		r1, r2, lsl #24			; r1 = imm16 value in high halfword
	mov		r2, #0
	msr		cpsr_f, r2				; Clear all flags (especially C and O)
	$armoper r0, r1
	lsr		r0, #16
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op81EGAlogical or, orrs
	op81EGAlogical and, ands
	op81EGAlogical xor, eors

	;-------
	; Cmp: memory location in EGA VRAM.
	; On input:
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_81_EGA_cmp
op_81_EGA_cmp
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldrb	r1, [r12], #2			; r1 = low byte of imm16 value
	ldrb	r2, [r12, #-1]			; r2 = high byte of imm16 value
	lsl		r0, #16
	orr		r1, r2, lsl #8			; r1 = imm16 value in low halfword
	cmp		r0, r1, lsl #16			; Compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

; ------------------- 83 = ??? r/m16,+imm8 ----------------------------
; 
; 26833E00000F 	cmp word es:[0000],000F 	(LHX)
;
	GLOBAL	op_83_EGA_add
op_83_EGA_add
	GLOBAL	op_83_EGA_sub
op_83_EGA_sub
	GLOBAL	op_83_EGA_adc
op_83_EGA_adc
	GLOBAL	op_83_EGA_sbb
op_83_EGA_sbb
	b		unknown

	MACRO 
	op83EGAlogical $x86oper, $armoper
	;-------
	; Memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_83_EGA_$x86oper
op_83_EGA_$x86oper
	calc_ega_r2
	mov		r3, r2					; Save original r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	ldrsb	r1,[r12],#1
	lsl		r0, #16
	lsl		r1, #16
	$armoper r0, r1
	lsr		r0, #16
	mov		r2, r3					; Restore address from r3
	mrs		r3,cpsr					; Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				; Restore flags
	b		loop
	MEND

	op83EGAlogical or, orrs
	op83EGAlogical and, ands
	op83EGAlogical xor, eors

	;-------
	; Cmp: memory location in EGA VRAM.
	; On input
	;	r2 = physical memory address (with EGA flags)
	;-------
	GLOBAL	op_83_EGA_cmp
op_83_EGA_cmp
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldrsb	r1,[r12],#1
	lsl		r0, #16
	cmp		r0, r1, lsl #16			; Compare the values
	b		complement_carry		; Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

; ------------------- 84 = TEST r8,r/m8 -------------------------------
;
; 268405    test es:[di],al	(SIERRA)
; 26841D    test es:[di],bl	(XENON2)
; 268425 	test es:[di],ah 	(XENON2)
;
; x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
; We clear all the flags before the actual tst operation, so C and O work like in x86.
;
	MACRO 
	op_84_EGA_reg8l $reg
	GLOBAL op_84_EGA_l_$reg
op_84_EGA_l_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #24
	tst		r1, r0, lsl #24
	b		loop
	MEND
	MACRO 
	op_84_EGA_reg8h $reg
	GLOBAL op_84_EGA_h_$reg
op_84_EGA_h_$reg
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	mov		r1, $reg, lsl #16
	tst		r1, r0, lsl #24
	b		loop
	MEND

	op_84_EGA_reg8l eax
	op_84_EGA_reg8l ecx
	op_84_EGA_reg8l edx
	op_84_EGA_reg8l ebx
	op_84_EGA_reg8h eax
	op_84_EGA_reg8h ecx
	op_84_EGA_reg8h edx
	op_84_EGA_reg8h ebx


; ------------------- 86 = XCHG r/m8,r8 -----------------------------
; This is most likely used in modes that require -read-before-write
; handling to refresh the internal latch register. Probably either
; Write Mode 2, or when using the Function Select register.
;
; 268605        xchg es:[di],al
; 268607        xchg es:[bx],al
; 268625        xchg es:[di],ah
; 2686451E 		xchg es:[di+1E],al (Gauntlet 2)
; 2686859600 	xchg es:[di+0096],al (Gauntlet 2)
;
	MACRO 
	op_86_EGA_reg8l $reg
	GLOBAL op_86_EGA_l_$reg
op_86_EGA_l_$reg
	calc_ega_r2
	mrs		r0,cpsr					; Save flags to r0 (for input Carry)
	mov		r3, r2					; Save original r2
	str		r0, [sp, #SP_STR_SEG]	; Save flags to stack
	bl		EGA_read_byte_r2
	and		r1, $reg, #0xFF
	bic		$reg, #0xFF
	orr		$reg, r0
	mov		r2, r3
	mov		r0, r1
	bl		EGA_write_byte_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	; Restore flags from stack
	b		restore_flags_from_r0
	MEND
	MACRO 
	op_86_EGA_reg8h $reg
	GLOBAL op_86_EGA_h_$reg
op_86_EGA_h_$reg
	calc_ega_r2
	mrs		r0,cpsr					; Save flags to r0 (for input Carry)
	mov		r3, r2					; Save original r2
	str		r0, [sp, #SP_STR_SEG]	; Save flags to stack
	bl		EGA_read_byte_r2
	mov		r1, $reg, lsr #8
	bic		$reg, #0xFF00
	orr		$reg, r0, lsl #8
	mov		r2, r3
	and		r0, r1, #0xFF
	bl		EGA_write_byte_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	; Restore flags from stack
	b		restore_flags_from_r0
	MEND

	op_86_EGA_reg8l eax
	op_86_EGA_reg8l ecx
	op_86_EGA_reg8l edx
	op_86_EGA_reg8l ebx

	op_86_EGA_reg8h eax
	op_86_EGA_reg8h ecx
	op_86_EGA_reg8h edx
	op_86_EGA_reg8h ebx

	LTORG

; ------------------- 87 = XCHG r/m16,r16 -----------------------------
; 8707 xchg [bx],ax (RAMPAGE)
;
	MACRO 
	op_87_EGA $reg
	GLOBAL op_87_EGA_$reg
op_87_EGA_$reg
	calc_ega_r2
	mrs		r0,cpsr					; Save flags to r0 (for input Carry)
	mov		r3, r2					; Save original r2
	str		r0, [sp, #SP_STR_SEG]	; Save flags to stack
	bl		EGA_read_hword_r2
	mov		r1, $reg, lsl #16
	eor		$reg, r1, lsr #16
	orr		$reg, r0
	mov		r2, r3
	mov		r0, r1, lsr #16
	bl		EGA_write_hword_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	; Restore flags from stack
	b		restore_flags_from_r0
	MEND

	op_87_EGA eax
	op_87_EGA ecx
	op_87_EGA edx
	op_87_EGA ebx
	op_87_EGA esp
	op_87_EGA ebp
	op_87_EGA esi
	op_87_EGA edi

	LTORG

	
; ------------------- 88 = MOV r/m8,r8 --------------------------------
; 268801		  MOV ES:[BX+DI],AL
; 26881D		  MOV ES:[DI],BL
; 268880D007      mov es:[bx+si+07D0],al (Nukem2)
; 26884528		  mov es:[di+28],al (Nukem2)
;
	MACRO 
	op_88_EGA_reg8l $reg
	GLOBAL op_88_EGA_l_$reg
op_88_EGA_l_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	and		r0, $reg, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3
	b		loop
	MEND
	MACRO 
	op_88_EGA_reg8h $reg
	GLOBAL op_88_EGA_h_$reg
op_88_EGA_h_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	mov		r0, $reg, lsr #8
	and		r0, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3
	b		loop
	MEND

	op_88_EGA_reg8l eax
	op_88_EGA_reg8l ecx
	op_88_EGA_reg8l edx
	op_88_EGA_reg8l ebx

	op_88_EGA_reg8h eax
	op_88_EGA_reg8h ecx
	op_88_EGA_reg8h edx
	op_88_EGA_reg8h ebx

	LTORG

; ------------------- 89 = MOV r/m16,r16 ------------------------------
; 26891D		mov es:[di],bx (KEEN4E)
; 26891EFABF	mov es:[BFFA],bx (WIN)
;
	MACRO 
	op_89_EGA_reg16 $reg
	GLOBAL op_89_EGA_$reg
op_89_EGA_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	mov		r0, $reg
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3
	b		loop
	MEND

	op_89_EGA_reg16 eax
	op_89_EGA_reg16 ecx
	op_89_EGA_reg16 edx
	op_89_EGA_reg16 ebx
	op_89_EGA_reg16 esp
	op_89_EGA_reg16 ebp
	op_89_EGA_reg16 esi
	op_89_EGA_reg16 edi

	LTORG

; ------------------- 8A = MOV r8,r/m8 --------------------------------
; 268A05 = MOV AL,ES:[DI]
; 268A07 = MOV AL,ES:[BX] (Nukem2)
; 268A00 = MOV AL,ES:[BX+SI] (Wasteland)
; 268A4528 = mov al,es:[di+28] (Nukem2)
;
	MACRO 
	op_8a_EGA_reg8l $reg
	GLOBAL op_8a_EGA_l_$reg
op_8a_EGA_l_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	bl		EGA_read_byte_r2
	bfi		$reg, r0, #0, #8
	msr		cpsr_f, r3
	b		loop
	MEND
	MACRO 
	op_8a_EGA_reg8h $reg
	GLOBAL op_8a_EGA_h_$reg
op_8a_EGA_h_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	bl		EGA_read_byte_r2
	bfi		$reg, r0, #8, #8
	msr		cpsr_f, r3
	b		loop
	MEND

	op_8a_EGA_reg8l eax
	op_8a_EGA_reg8l ecx
	op_8a_EGA_reg8l edx
	op_8a_EGA_reg8l ebx

	op_8a_EGA_reg8h eax
	op_8a_EGA_reg8h ecx
	op_8a_EGA_reg8h edx
	op_8a_EGA_reg8h ebx

	LTORG

; ------------------- 8B = MOV r16,r/m16 ------------------------------
; 268B0D	mov	cx,es:[di]	(KEEN4E)
; 268B2C 	mov bp,es:[si] (WARET)
;
	MACRO 
	op_8b_EGA_reg16 $reg
	GLOBAL op_8b_EGA_$reg
op_8b_EGA_$reg
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	bl		EGA_read_hword_r2
	bfi		$reg, r0, #0, #16
	msr		cpsr_f, r3
	b		loop
	MEND

	op_8b_EGA_reg16 eax
	op_8b_EGA_reg16 ecx
	op_8b_EGA_reg16 edx
	op_8b_EGA_reg16 ebx
	op_8b_EGA_reg16 esp
	op_8b_EGA_reg16 ebp
	op_8b_EGA_reg16 esi
	op_8b_EGA_reg16 edi

	LTORG

; ------------------- 8E = MOV Sreg,r/m16 -----------------------------
;
; 268E060400 	mov es,es:[0004] 	(LHX)
;
	GLOBAL	op_8e_EGA_r2_es
op_8e_EGA_r2_es
	calc_ega_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	bl		EGA_read_hword_r2
	mov		r2, r0					; r2 = hword from EGA VRAM
	msr		cpsr_f,r3				; Restore flags
	b		mov_es_r2				; Jump to the ES segment setting in "cpu.s"

; ------------------- A4 = MOVSB --------------------------------------
;
; Move a byte from DS:SI to ES:DI
;
	GLOBAL	op_a4_from_EGA
op_a4_from_EGA
	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:SI linear address (with EGA flag)
	;	r3 = Address size mask (0x0000FFFF)
	;-------
	str		r1, [sp, #SP_STR_SEG]	; Save EDI inc/dec value to stack
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldr		lr, [sp, #SP_STR_SEG]	; Get EDI inc/dec value from stack
	mov		r1, r0					; r1 = the byte read from DS:SI
	msr		cpsr_f, r3				; Restore flags
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	add		edi, lr, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES_far op_a4_to_RAM, op_a4_to_EGA, unknown

	EXTERN	op_a4_to_RAM

	GLOBAL	op_a4_from_EGA_USE32
op_a4_from_EGA_USE32
	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:SI linear address (with EGA flag)
	;	r3 = Address size mask (0xFFFFFFFF)
	;-------
	str		r1, [sp, #SP_STR_SEG]	; Save EDI inc/dec value to stack
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldr		lr, [sp, #SP_STR_SEG]	; Get EDI inc/dec value from stack
	mov		r1, r0					; r1 = the byte read from DS:SI
	msr		cpsr_f, r3				; Restore flags
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	add		edi, lr					; Fix logical EDI.
	mem_handler_jump_r0r3_ES_far op_a4_to_RAM, op_a4_to_EGA, unknown

	;-------
	; On input
	;	r1 = byte read from DS:SI
	;	r2 = ES:DI linear address (with EGA flag)
	;-------
	GLOBAL	op_a4_to_EGA
op_a4_to_EGA
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, r1					; Get the output byte from r1
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop

	LTORG
	
; ------------------- A5 = MOVSW --------------------------------------
;
; Move a word from DS:SI to ES:DI
;
	GLOBAL	op_a5_from_EGA
op_a5_from_EGA
	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:SI linear address (with EGA flag)
	;	r3 = Address size mask (0x0000FFFF)
	;-------
	str		r1, [sp, #SP_STR_SEG]	; Save EDI inc/dec value to stack
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldr		lr, [sp, #SP_STR_SEG]	; Get EDI inc/dec value from stack
	mov		r1, r0					; r1 = the byte read from DS:SI
	msr		cpsr_f, r3				; Restore flags
	mov		r3, #0xFFFF							; Use 16-bit memory address masking
	mov		r0, edi					; r0 = DI
	add		edi, lr, edi, ror #16	; Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES_far op_a5_to_RAM, op_a5_to_EGA, unknown

	EXTERN	op_a5_to_RAM

	GLOBAL	op_a5_from_EGA_USE32
op_a5_from_EGA_USE32
	;-------
	; On input
	;	r1 = EDI increment/decrement value
	;	r2 = DS:SI linear address (with EGA flag)
	;	r3 = Address size mask (0x0000FFFF)
	;-------
	str		r1, [sp, #SP_STR_SEG]	; Save EDI inc/dec value to stack
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldr		lr, [sp, #SP_STR_SEG]	; Get EDI inc/dec value from stack
	mov		r1, r0					; r1 = the byte read from DS:SI
	msr		cpsr_f, r3				; Restore flags
	mvn		r3, #0					; Use 32-bit memory addressing
	mov		r0, edi					; r0 = EDI
	add		edi, lr					; Fix logical EDI.
	mem_handler_jump_r0r3_ES_far op_a5_to_RAM, op_a5_to_EGA, unknown


	GLOBAL	movsd_from_EGA
movsd_from_EGA
	GLOBAL	movsd_to_EGA
movsd_to_EGA
	GLOBAL	movsd_from_EGA_USE32
movsd_from_EGA_USE32
	GLOBAL	stosd_EGA
stosd_EGA
	GLOBAL	rep_stosd_cld_EGA
rep_stosd_cld_EGA
	b		unknown

	;-------
	; On input
	;	r1 = halfword read from DS:SI
	;	r2 = ES:DI linear address (with EGA flag)
	;-------
	GLOBAL	op_a5_to_EGA
op_a5_to_EGA
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, r1					; r0 = the halfword read from DS:SI
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- AA = STOSB --------------------------------------
; Segment override does not affect STOSB, it always uses ES:DI
; On input r2 = linear ES:DI with EGA flags
;
	GLOBAL	stosb_EGA
stosb_EGA
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	and		r0, eax, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- AB = STOSW --------------------------------------
; Segment override does not affect STOSW, it always uses ES:DI
; On input r2 = linear ES:DI with EGA flags
;
	GLOBAL	stosw_EGA
stosw_EGA
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, eax
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop
	
; ------------------- AC = LODSB --------------------------------------
;
; We may not change the flags!
;
	GLOBAL	lodsb_EGA
lodsb_EGA
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	bic		eax, #0xFF
	orr		eax, r0
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- AD = LODSW --------------------------------------
;
; We may not change the flags!
;
	GLOBAL	lodsw_EGA
lodsw_EGA								; This is called by "cpu_string.s"
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	lsr		eax, #16
	orr		eax, r0, eax, lsl #16
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- AD = LODSD --------------------------------------
;
; We may not change the flags!
;
	GLOBAL	lodsd_EGA
lodsd_EGA								; This is called by "cpu_string.s"
	mrs		r3,cpsr						; Save flags (we are not allowed to change any)
	calc_ega_r2
	str		r2, [sp, #SP_STR_SEG]		; Save original r2 value
	bl		EGA_read_hword_r2			; Read low halfword
	ldr		r2, [sp, #SP_STR_SEG]		; Restore original r2 value
	mov		eax, r0
	add		r2, #2*4
	bl		EGA_read_hword_r2			; Read high halfword
	orr		eax, r0, lsl #16
	msr		cpsr_f, r3					; Restore flags
	b		loop

; ------------------- AE = SCASB --------------------------------------
;
	GLOBAL	scasb_EGA
scasb_EGA
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, eax, lsl #24
	cmp		r1, r0, lsl #24			; Compare AL with byte
	b		complement_carry		; Go back to the opcode loop

	LTORG

; ------------------- C6 = MOV r/m8, imm8 -----------------------------
;
	GLOBAL	op_c6_EGA_r2
op_c6_EGA_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	ldrb	r0,[r12],#1				; Load the imm8 byte to r0, increment r12 by 1
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- C7 = MOV r/m16, imm16 -----------------------------
;
	GLOBAL	op_c7_EGA_r2
op_c7_EGA_r2
	mrs		r3,cpsr					; Save flags (we are not allowed to change any)
	calc_ega_r2
	ldrb	r0,[r12],#1				; Load the imm16 low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				; Load the imm16 high byte to r1, increment r12 by 1
	orr		r0, r1, lsl #8
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				; Restore flags
	b		loop

; ------------------- D0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,1 ---
; 

	;=======
	; Rotate by a single bit position, putting current Carry flag into the lowest bit.
	;=======
	GLOBAL	rcl_byte_r2_1_EGA
rcl_byte_r2_1_EGA					; Directly jump here from single-bit-shift opcodes
	mrs		r3,cpsr					; Save flags to r3
	calc_ega_r2
	str		r2, [sp, #SP_STR_SEG]	; Save original r2 value
	bl		EGA_read_byte_r2
	;-------
	; Perform the RCL operation
	;-------
	lsl		r0, #1					; Shift left 1 bit
	tst		r3, #ARM_CARRY
	orrne	r0, #1					; Move Carry into the lowest bit
	bic		r3, #ARM_CARRY|ARM_OVER	; Clear the Carry and Overflow bits of the saved CPU flags
	;-------
	; Swap the values between registers and stack
	;-------
	ldr		r2, [sp, #SP_STR_SEG]	; Get original r2 value
	str		r0, [sp, #SP_STR_SEG]	; Save the output byte to stack
	;-------
	; Write the resulting byte to EGA VRAM
	;-------
	bl		EGA_write_byte_r0_to_r2
	;-------
	; Calculate the resulting flags and return
	;-------
	ldr		r0, [sp, #SP_STR_SEG]	; Get output value from stack
	eor		r0, r0, lsr #1			; Now r0 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r0, #0x180				; Leave only the new Carry and Overflow bits to r1
	orr		r0, r3, r0, lsl #(24-3)	; and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	; Jump back to loop, restoring the flags


; ------------------- REP MOVSB ---------------------------
;

	GLOBAL	rep_movsb_cld_from_EGA
rep_movsb_cld_from_EGA				; This code is common for both REP MOVSB and REP MOVSW
	calc_ega_r1
	mov		r0, r11					; r0high = DI
	mem_handler_jump_r0r3_ES_far rep_movsb_cld_EGA_RAM, rep_movsb_cld_EGA_EGA, unknown

	GLOBAL	rep_movsb_std_from_EGA
rep_movsb_std_from_EGA
	calc_ega_r1
	mov		r0, r11					; r0high = DI
	mem_handler_jump_r0r3_ES_far rep_movsb_std_EGA_RAM, rep_movsb_std_EGA_EGA, unknown

; ------------------- REP MOVSB from RAM to EGA ------------------------
;
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
	GLOBAL	rep_movsb_cld_RAM_EGA
rep_movsb_cld_RAM_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 input bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r2 = output VRAM pointer (ES:DI)
	;	r3 = loop counter
	;	r4 = input RAM pointer (DS:SI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2	; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.

; ------------------- REP MOVSB from RAM to EGA (STD) -----------------
; This is a silly and rare operation!
;
	GLOBAL	rep_movsb_std_RAM_EGA
rep_movsb_std_RAM_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	subs	r3, #1					; Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_beg 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
1	add		r3, #1					; Restore r3 count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 input bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r2 = output VRAM pointer (ES:DI)
	;	r3 = loop counter
	;	r4 = input RAM pointer (DS:SI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #-1			; r0 = byte from DS:SI, decrement input pointer
	bl		EGA_write_byte_r0_to_r2	; Write it to ES:DI. TODO! This should be speeded up!
	sub		r2, #4					; Decrement output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	subs	r3, #1					; Decrease the count by one for page checks
	beq		%f1
	min_cnt_prev_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_beg 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
1	add		r3, #1					; Restore r3 count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3					; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.

; ------------------- REP MOVS from EGA to RAM ------------------------
;
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
rep_movsb_cld_EGA_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Check the current read mode
	;-------
2	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	tst		r0, #8
	bne		movsb_cld_EGA_RAM_readmode_1	; Go handle Read Mode 1
	;-------
	; Get the read mask
	;-------
	ldr		r0,=EGA_READ_MASK32
	ldr		lr, [r0]
	;-------
	; Get the value from our emulated EGA/VGA RAM
	;-------
1	ldr		r4, [r1], #4
	;-------
	; Read Mode 0, return 8 consecutive pixels from EGA VRAM.
	;-------
	mov		r0, #0
	and		r4, lr					; Leave only the pixels in r1 that exist in the plane we are interested in.
	tst		r4, #0xF				; Pixel 0 set?
	orrne	r0, #0x80
	tst		r4, #0xF0				; Pixel 1 set?
	orrne	r0, #0x40
	tst		r4, #0xF00				; Pixel 2 set?
	orrne	r0, #0x20
	tst		r4, #0xF000				; Pixel 3 set?
	orrne	r0, #0x10
	tst		r4, #0xF0000			; Pixel 4 set?
	orrne	r0, #0x08
	tst		r4, #0xF00000			; Pixel 5 set?
	orrne	r0, #0x04
	tst		r4, #0xF000000			; Pixel 6 set?
	orrne	r0, #0x02
	tst		r4, #0xF0000000			; Pixel 7 set?
	orrne	r0, #0x01
	;-------
	; Save the output byte
	;-------
	strb	r0, [r2], #1
	subs	r3, #1
	bgt		%b1
movsb_cld_EGA_RAM_done
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
	;-------
	; Read Mode 1, return the result of a color comparison.
	;-------
movsb_cld_EGA_RAM_readmode_1	
	ldr		lr, =EGA_COLOR_COMPARE32
	ldr		r4, [lr]
	ldr		lr, [lr, #(EGA_COLOR_DONT_CARE32-EGA_COLOR_COMPARE32)]
	and		lr, r4								; Clear the "Dont Care" bits from the Color Compare value
	;-------
	; Get the value from our emulated EGA/VGA RAM
	; Registers
	;	r0 = value read from EGA VRAM
	;	r1 = input address DS:SI
	;	r2 = output address ES:DI
	;	r3 = loop counter
	;	r4 = "Dont Care" value, not changed in the loop
	;	r5 = total counter CX
	;	lr = Color Compare value, not changed in the loop
	;	r10 = source index SI
	;	r11 = target index DI
	;-------
1	ldr		r0, [r1], #4
	and		r0, r4								; Clear the "Dont Care" bits from the EGA/VGA RAM value
	eor		r0, lr								; Now r1 contains bits set in nibbles that fail the comparison
	;-------
	; Turn on the needed pixels in AL
	;-------
	tst		r0, #0xF0000000						; Pixel 7 set?
	lsl		r0, #4
	orreq	r0, #0x01
	tst		r0, #0xF0							; Pixel 0 set?
	bic		r0, #0xF0
	orreq	r0, #0x80
	tst		r0, #0xF000							; Pixel 1 set?
	orreq	r0, #0x40
	tst		r0, #0xF00000						; Pixel 2 set?
	orreq	r0, #0x20
	tst		r0, #0xF0000000						; Pixel 3 set?
	orreq	r0, #0x10
	tst		r0, #0xF00							; Pixel 4 set?
	orreq	r0, #0x08
	tst		r0, #0xF0000						; Pixel 5 set?
	orreq	r0, #0x04
	tst		r0, #0xF000000						; Pixel 6 set?
	orreq	r0, #0x02
	;-------
	; Save the output byte
	;-------
	strb	r0, [r2], #1
	subs	r3, #1
	bgt		%b1
	b		movsb_cld_EGA_RAM_done

; ------------------- REP MOVSB from EGA to RAM (STD) -----------------
; This is a silly and rare operation!
;
rep_movsb_std_EGA_RAM
	;-------
	; On input
	;	r0 = free
	;	r1 = physical linear start address of DS:SI in EGAVGA_A000
	;	r2 = physical start address of ES:DI in RAM
	;	r5 = number of bytes to store, needs to be cleared
	;	r10 = SI register, needs to be incremented by CX
	;	r11 = DI register, needs to be incremented by CX
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6}
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	subs	r3, #1					; Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_VGA_beg 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
1	add		r3, #1					; Restore the count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 input bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r2 = destroyed
	;	r3 = loop counter
	;	r4 = input VRAM pointer (DS:SI)
	;	r6 = output RAM pointer (ES:DI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
	mov		r6, r2					; r6 = output ES:DI pointer
1	mov		r2, r4
	bl		EGA_read_byte_r2		; Returns byte in r0, destroys r1 and r2!
	sub		r4, #4
	strb	r0, [r6], #-1			; Save byte to ES:DI, decrement output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4, r6}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	subs	r3, #1					; Decrease the count by one for page checks
	beq		%f1
	min_cnt_VGA_beg 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_prev_page 	r3, r2		; ----- Check with ES:DI bytes before page end
1	add		r3, #1					; Restore the count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3					; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.

; ------------------- REP MOVS from EGA to EGA ------------------------
; ALREADY FIXED FOR ax86 USE!
;
rep_movsb_cld_EGA_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2						; Clear the EGA flag bits from the linear address
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Test for current write mode
	;-------
2	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					; r1 = Write Mode
	cmp		r0, #1					; Is it Write Mode 1?
	bne		movsb_cld_EGA_EGA_slow	; Nope, use the slow move
	;-------
	; Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	;-------
	GLOBAL	movsb_cld_EGA_EGA_words	; Also used by "MODEX.s"!!
movsb_cld_EGA_EGA_words
	cmp		r3, #8
	bgt		movsb_cld_EGA_EGA_block	; More than 8 words to move, use the block code
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 0: Move 8 words
	cmp		r3, #7
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 1: Move 7 words
	cmp		r3, #6
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 2: Move 6 words
	cmp		r3, #5
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 3: Move 5 words
	cmp		r3, #4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 4: Move 4 words
	cmp		r3, #3
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 5: Move 3 words
	cmp		r3, #2
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 6: Move 2 words
	cmp		r3, #1
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 7: Move 1 words
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							; vga.latch.d = r0
	str		r0, [r1, #-4]						; vga.latch.d = r0
movsb_cld_EGA_EGA_done
	pop		{r0, r3}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_cld_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	cmp		r3, #1					; Shortcut for when we only need to move one byte, that is always possible.
	beq		%f1
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
1	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
	;-------
	; Use block move when more than 8 words to move
	;-------
movsb_cld_EGA_EGA_block
	push	{r4-r11}
1	mov		r0, r3, lsr #(5-2)		; r0 = number of 32-byte blocks to move
	sub		r3, #(8*8)				; This many bytes (words) handled
	cmp		r0, #8
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 0 = 8
	cmp		r0, #7
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 1 = 7
	cmp		r0, #6
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 2 = 6
	cmp		r0, #5
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 3 = 5
	cmp		r0, #4
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 4 = 4
	cmp		r0, #3
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 5 = 3
	cmp		r0, #2
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 6 = 2
	cmp		r0, #1
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 7 = 1
	cmp		r3, #8					; Still more than 32 bytes (8 words) to move?
	bge		%b1						; Yep, go back to loop
	ands	r3, #7					; Still bytes to move?
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldreq	r1, =VGA_latch
	streq	r11, [r1]							; vga.latch.d = r11
	streq	r11, [r1, #-4]						; vga.latch.d = r11
	pop		{r4-r11}
	bne		movsb_cld_EGA_EGA_words
	b		movsb_cld_EGA_EGA_done
	;-------
	; Write mode != 1: Slow copy using the subroutines.
	;-------
movsb_cld_EGA_EGA_slow
	push	{r4, r6, lr}
	;-------
	; Setup the registers
	;-------
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	;-------
	; First read the EGA VRAM value into r0
	;-------
1	mov		r2, r4
	bl		EGA_read_byte_r2
	add		r4, #4
	;-------
	; Then write the r0 value into EGA VRAM
	;-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	add		r6, #4
	;-------
	; Decrease the loop counter
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Return
	;-------
	pop		{r4, r6, lr}
	b		movsb_cld_EGA_EGA_done

; ------------------- REP MOVSB from EGA to EGA (STD) -----------------
; (Windows 3.00a: Control Panel, Alt-S for Settings)
; 
rep_movsb_std_EGA_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	subs	r3, #1					; Decrease the count by one for page checks
	ror		esi, #16
	ror		edi, #16
	beq		%f1
	min_cnt_VGA_beg 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_beg 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_zero_16 r3, esi		; ----- Check with SI
	min_cnt_idx_zero_16 r3, edi		; ----- Check with DI
1	add		r3, #1					; Restore the count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		edi, r3, lsl #16		; Decrement DI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Adjust the registers by the number of bytes we will store.
	; Test for current write mode
	;-------
2	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					; r0 = Write Mode
	cmp		r0, #1					; Is it Write Mode 1?
	bne		movsb_std_EGA_EGA_slow	; Nope, use the slow move
	;-------
	; Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	;-------
1	ldr		r0, [r1], #-4 			; Get the value from DS:SI (EGA VRAM)
	str		r0, [r2], #-4			; Write the result to ES:DI (EGA VRAM)
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							; vga.latch.d = r0
	str		r0, [r1, #-4]						; vga.latch.d = r0
movsb_std_EGA_EGA_done
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsb_std_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	subs	r3, #1					; Decrease the count by one for page checks
	beq		%f1
	min_cnt_VGA_beg 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_beg 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
1	add		r3, #1					; Restore the count
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		esi, r3					; Decrement ESI by the number of bytes we shall move.
	sub		edi, r3					; Decrement EDI by the number of bytes we shall move..
	sub		ecx, r3					; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
movsb_std_EGA_EGA_slow
	push	{r6}
	;-------
	; Setup the registers
	;-------
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	;-------
	; First read the EGA VRAM value into r0
	;-------
1	mov		r2, r4
	bl		EGA_read_byte_r2
	sub		r4, #4
	;-------
	; Then write the r0 value into EGA VRAM
	;-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	sub		r6, #4
	;-------
	; Decrease the loop counter
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Return
	;-------
	pop		{r6}
	b		movsb_std_EGA_EGA_done

; ------------------- REP MOVSW ---------------------------------------
;

	;-------
	; Direction flag clear
	;-------
	GLOBAL	rep_movsw_cld_from_EGA
rep_movsw_cld_from_EGA
	calc_ega_r1
	mov		r0, r11					; r0 = DI
	mem_handler_jump_r0r3_ES_far rep_movsw_cld_EGA_RAM, rep_movsw_cld_EGA_EGA, unknown

	
	;-------
	; Direction flag set
	;-------
	GLOBAL	rep_movsw_std_from_EGA
rep_movsw_std_from_EGA
	calc_ega_r1
	mov		r0, r11					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_std_EGA_RAM, rep_movsw_std_EGA_EGA, unknown


rep_movsw_std_EGA_RAM
rep_movsw_std_RAM_EGA
rep_movsw_std_EGA_EGA
rep_stosb_std_EGA
rep_stosw_std_EGA
	b		unknown

	GLOBAL	rep_movsw_std_RAM_EGA

; ------------------- REP MOVSW from RAM to EGA ----------------------
;
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in RAM
	;	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
	GLOBAL	rep_movsw_cld_RAM_EGA
rep_movsw_cld_RAM_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			; Push used registers
	;-------
	; Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	;-------
	calc_ega_r2
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r2 = output VRAM pointer (ES:DI)
	;	r3 = loop counter
	;	r4 = input RAM pointer (DS:SI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #17		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r4, r1					; r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4}			; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1 
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			; r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	lsl		r3, #1					; Number of bytes to move = 2*CX
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #1			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r4, r1					; r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Move the remaining byte to a possibly different page.
	;------
	pop		{r0, r3, r4}			; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			; r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Adjust the registers, one more halfword moved.
	;------
	pop		{r0}					; Pop saved flags
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

; ------------------- REP MOVSW from RAM to EGA (STD) -----------------
; (Windows 3.00a: Closing About window)
;

	GLOBAL	rep_movsw_std_RAM_EGA_TODO
rep_movsw_std_RAM_EGA_TODO
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI+1 in RAM
	;	r2 = physical start address of ES:DI+1 (with EGA flags)
	;	r10 = SI register, needs to be decremented by CX*2
	;	r11 = DI register, needs to be decremented by CX*2
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	sub		r3, r5, #1				; Start with r3 = CX, decrease it by one for the page checks.
	; ----- Check with ES:DI bytes in this page
	rsb		r0, r2					; r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	; ----- Check with DS:SI bytes in this page
	mov		r0, r1, lsl #18
	cmp		r3, r0, lsr #18
	movgt	r3, r0, lsr #18
	; ----- Check with SI
	sub		r3, #1					; SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	; ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will move.
	;-------
	sub		r10, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		r11, r3, lsl #16		; Decrement DI by the number of bytes we shall move.
	sub		r5, r3					; r5 = number of bytes remaining
	;-------
	; Move slowly one byte at a time.
	;-------
	mov		r4, r1					; Save r1 = DS:SI into r4
1	ldrb	r0, [r4], #-1			; r0 = byte from DS:SI, decrement input pointer
	bl		EGA_write_byte_r0_to_r2	; Write it to ES:DI. TODO! This should be speeded up!
	sub		r2, #4					; Decrement output pointer
	subs	r3, #1					; CX -= 1
	bne		%b1						; Back to loop until r3 = 0
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_std_next		; ... else handle the next 16K page.

	LTORG

; ------------------- REP MOVSW from EGA to EGA ------------------
; Bard's Tale
;
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	;	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
rep_movsw_cld_EGA_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2						; Clear the EGA flag bits from the linear address
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx					; Start with r3 = CX
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of halfwords remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Test for current write mode, from now on similar code for USE16 and USE32 versions.
	;-------
2	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					; r1 = Write Mode
	cmp		r0, #1					; Is it Write Mode 1?
	bne		movsw_cld_EGA_EGA_slow	; Nope, use the slow move
	;-------
	; Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	;-------
	GLOBAL	movsw_cld_EGA_EGA_words	; Also used by "MODEX.s"!!
movsw_cld_EGA_EGA_words
	cmp		r3, #8
	bgt		movsw_cld_EGA_EGA_block	; More than 8 words to move, use the block code
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 0: Move 8 words
	cmp		r3, #7
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 1: Move 7 words
	cmp		r3, #6
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 2: Move 6 words
	cmp		r3, #5
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 3: Move 5 words
	cmp		r3, #4
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 4: Move 4 words
	cmp		r3, #3
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 5: Move 3 words
	cmp		r3, #2
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 6: Move 2 words
	cmp		r3, #1
	ldrge	r0, [r1], #4
	strge	r0, [r2], #4			; r0 = 7: Move 1 words
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							; vga.latch.d = r0
	str		r0, [r1, #-4]						; vga.latch.d = r0
movsw_cld_EGA_EGA_done
	pop		{r0, r3}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

	;-------
	; USE16 Uneven number of bytes to move, so need special attention.
	;-------
5	push	{r4, r6}
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #17		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;-------
	; First move the full halfwords we can move
	;-------
1	mov		r2, r4
	bl		EGA_read_hword_r2
	add		r4, #8
	mov		r2, r6
	bl		EGA_write_hword_r0_to_r2
	add		r6, #8
	subs	r3, #1
	bne		%b1
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r2, r4
	bl		EGA_read_byte_r2
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16				; Restore ESI
	ror		edi, #16				; Restore EDI
	pop		{r4, r6}				; Pop pushed registers
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, r0					; r1 = EGA byte read from DS:SI
	pop		{r0}
	msr		cpsr_f,r0				; Restore flags
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1 
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	mov		r0, r1					; r0 = EGA value to write
	bl		EGA_write_byte_r0_to_r2
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	lsl		r3, #1					; Number of bytes to move = 2*CX
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of halfwords remaining
	b		%b2						; The rest of the code is similar to USE16 version.

	;-------
	; USE32 Uneven number of bytes to move, so need special attention.
	;-------
5	push	{r4, r6}
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #1			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;-------
	; First move the full halfwords we can move
	;-------
1	mov		r2, r4
	bl		EGA_read_hword_r2
	add		r4, #8
	mov		r2, r6
	bl		EGA_write_hword_r0_to_r2
	add		r6, #8
	subs	r3, #1
	bne		%b1
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r2, r4
	bl		EGA_read_byte_r2
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	;------
	; Move the remaining byte to a possibly different page.
	;------
	pop		{r4, r6}				; Pop pushed registers
	pop		{r0, r3}				; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, r0					; r1 = EGA byte read from DS:SI
	pop		{r0}
	msr		cpsr_f,r0				; Restore flags
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1 
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	mov		r0, r1					; r0 = EGA value to write
	bl		EGA_write_byte_r0_to_r2
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

	;-------
	; Use block move when more than 8 words to move
	;-------
movsw_cld_EGA_EGA_block
	push	{r4-r11}
1	mov		r0, r3, lsr #(5-2)		; r0 = number of 32-byte blocks to move
	sub		r3, #(8*8)				; This many bytes (words) handled
	cmp		r0, #8
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 0 = 8
	cmp		r0, #7
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 1 = 7
	cmp		r0, #6
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 2 = 6
	cmp		r0, #5
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 3 = 5
	cmp		r0, #4
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 4 = 4
	cmp		r0, #3
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 5 = 3
	cmp		r0, #2
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 6 = 2
	cmp		r0, #1
	ldmiage	r1!, {r4-r11}
    stmiage r2!, {r4-r11}			; 7 = 1
	cmp		r3, #8					; Still more than 32 bytes (8 words) to move?
	bge		%b1						; Yep, go back to loop
	ands	r3, #7					; Still bytes to move?
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldreq	r1, =VGA_latch
	streq	r11, [r1]							; vga.latch.d = r11
	streq	r11, [r1, #-4]						; vga.latch.d = r11
	pop		{r4-r11}
	bne		movsw_cld_EGA_EGA_words
	b		movsw_cld_EGA_EGA_done

	;-------
	; Write mode != 1: Slow copy using the subroutines.
	;-------
movsw_cld_EGA_EGA_slow
	push	{r4, r6}
	;-------
	; Setup the registers
	;-------
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	;-------
	; First read the EGA VRAM value into r0
	;-------
1	mov		r2, r4
	bl		EGA_read_byte_r2
	add		r4, #4
	;-------
	; Then write the r0 value into EGA VRAM
	;-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	add		r6, #4
	;-------
	; Decrease the loop counter
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Return
	;-------
	pop		{r4, r6}
	b		movsw_cld_EGA_EGA_done

	LTORG
	
; ------------------- REP MOVSW from EGA to RAM  -----------------
; Commander Keen 4
;	EAX=00000000 EBX=000003E4 ECX=0000314D EDX=000003CE
;	ESP=FFFFFF54 EBP=0000FF5A ESI=00000000 EDI=00000000
;	ES=5BB1 CS=02EE SS=31D5 DS=A000 FS=0000 GS=0000
;	02EE:00001D58 F3A5            repe movsw
;
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	;	r2 = physical start address of ES:DI in RAM
	;	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	;	ecx = number of bytes to store, needs to be cleared
	;	esi = SI register, needs to be incremented by CX
	;	edi = DI register, needs to be incremented by CX
	;-------
rep_movsw_cld_EGA_RAM
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6}
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #1					; Number of bytes to move = 2*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_VGA_end 	r3, r1		; ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3, r2		; ----- Check with ES:DI bytes before page end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r3 = loop counter
	;	r4 = input RAM pointer (DS:SI)
	;	r6 = output VRAM pointer (ES:DI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
	mov		r6, r2					; r6 = output ES:DI pointer
1	mov		r2, r4	
	bl		EGA_read_byte_r2
	add		r4, #4
	strb	r0, [r6], #1
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4, r6}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #17		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
	mov		r6, r2					; r6 = output ES:DI pointer
1	mov		r2, r4	
	bl		EGA_read_hword_r2
	add		r4, #8
	strb	r0, [r6], #1
	lsr		r0, #8
	strb	r0, [r6], #1
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	mov		r2, r6
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r6, r2					; r6 = output ES:DI pointer
	mov		r2, r1
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4, r6}		; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	
	calc_ega_r2
	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r6, r2					; r6 = output ES:DI pointer
	mov		r2, r1
	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.

	;-------
	; USE32: Calculate the number of safe bytes we can move
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx					; Start with r3 = ECX
	lsl		r3, #1					; Number of bytes to move = 2*CX
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #1			; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #1			; r3 = number of full halfwords to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #1			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full halfwords we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
	mov		r6, r2					; r6 = output ES:DI pointer
1	mov		r2, r4	
	bl		EGA_read_hword_r2
	add		r4, #8
	strb	r0, [r6], #1
	lsr		r0, #8
	strb	r0, [r6], #1
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	mov		r2, r6
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r6, r2					; r6 = output ES:DI pointer
	mov		r2, r1
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	;------
	; Move the remaining byte to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4, r6}		; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1
	calc_ega_r2
	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES %f1, bad_string_op_seg_back1, bad_string_op_seg_back1 
1	mov		r6, r2					; r6 = output ES:DI pointer
	mov		r2, r1
	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		; ... else handle the next 16K page.


; ------------------- REP MOVSW from EGA to RAM (STD) -----------------
; This is a silly and rare operation!
;
rep_movsw_std_EGA_RAM_TODO
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI+1 in EGA VRAM
	;	r2 = physical start address of ES:DI+1 in RAM
	;	r5 = number of BYTES to move, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX*2
	;	r11 = DI register, needs to be decremented by CX*2
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6, lr}
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	sub		r3, r5, #1				; Start with r3 = CX, decrease it by one for the page checks.
	; ----- Check with DS:SI bytes in this page
	rsb		r0, r1					; r0 = physical DS:SI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	; ----- Check with ES:DI bytes in this page
	mov		r0, r2, lsl #18
	cmp		r3, r0, lsr #18
	movgt	r3, r0, lsr #18
	; ----- Check with SI
	sub		r3, #1					; SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	; ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		r10, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	sub		r11, r3, lsl #16		; Decrement DI by the number of bytes we shall move.
	sub		r5, r3					; r5 = number of bytes remaining
	mov		r4, r1					; r4 = input DS:SI pointer
	mov		r6, r2					; r6 = output ES:DI pointer
	;-------
	; Loop here for r3 input bytes.
	; Registers
	;	r0 = scratch
	;	r1 = destroyed
	;	r2 = destroyed
	;	r3 = loop counter
	;	r4 = input VRAM pointer (DS:SI)
	;	r6 = output RAM pointer (ES:DI)
	;-------
1	mov		r2, r4
	bl		EGA_read_byte_r2		; Returns byte in r0, destroys r1 and r2!
	sub		r4, #4
	strb	r0, [r6], #-1			; Save byte to ES:DI, decrement output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4, r6, lr}
	cmp		r5, #0
	biceq	r9, #0xFF				; Clear the r9 low halfword (saved effective segment value)
	biceq	r9, #0xFF00				; Clear the r9 low halfword (saved effective segment value)
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.


; ------------------- REP MOVSW from EGA to EGA (STD) -----------------
;
rep_movsw_std_EGA_EGA_TODO
	;-------
	; On input
	;	r0 = free
	;	r1 = physical start address of DS:SI+1 in EGA VRAM
	;	r2 = physical start address of ES:DI+1 with EGA flags
	;	r5 = number of BYTES to move, needs to be cleared
	;	r10 = SI register, needs to be decremented by CX*2
	;	r11 = DI register, needs to be decremented by CX*2
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_address_before_B000_seg, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	sub		r3, r5, #1				; Start with r3 = CX, decrease it by one for the page checks.
	; ----- Check with DS:SI bytes in this page
	sub		r4, r1, r0				; r4 = physical DS:SI - EGAVGA_A000
	cmp		r3, r4, lsr #2
	movgt	r3, r4, lsr #2
	; ----- Check with ES:DI bytes in this page
	sub		r4, r2, r0				; r4 = physical ES:DI - EGAVGA_A000
	cmp		r3, r4, lsr #2
	movgt	r3, r4, lsr #2
	; ----- Check with SI
	sub		r3, #1					; SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	; ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will store.
	; Also test for current write mode
	;-------
	ldr		r0, =VGAModeReg
	sub		r10, r3, lsl #16		; Decrement SI by the number of bytes we shall move.
	ldrb	r0, [r0]
	sub		r11, r3, lsl #16		; Decrement DI by the number of bytes we shall move.
	sub		r5, r3					; r5 = number of bytes remaining
	and		r0, #3					; r0 = Write Mode
	cmp		r0, #1					; Is it Write Mode 1?
	bne		movsw_std_EGA_EGA_slow	; Nope, use the slow move
	;-------
	; Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	;-------
1	ldr		r0, [r1], #-4 			; Get the value from DS:SI (EGA VRAM)
	str		r0, [r2], #-4			; Write the result to ES:DI (EGA VRAM)
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Leave the last copied value to the EGA/VGA latch
	;-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							; vga.latch.d = r0
	str		r0, [r1, #-4]						; vga.latch.d = r0
movsw_std_EGA_EGA_done
	pop		{r0, r3, r4}
	cmp		r5, #0
	biceq	r9, #0xFF				; Clear the r9 low halfword (saved effective segment value)
	biceq	r9, #0xFF00				; Clear the r9 low halfword (saved effective segment value)
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsw_std_next		; ... else handle the next 16K page.
movsw_std_EGA_EGA_slow
	push	{r6, lr}
	;-------
	; Setup the registers
	;-------
	mov		r4, r1					; r4 = physical DS:SI
	mov		r6, r2					; r6 = physical ES:DI
	;-------
	; First read the EGA VRAM value into r0
	;-------
1	mov		r2, r4
	bl		EGA_read_byte_r2
	sub		r4, #4
	;-------
	; Then write the r0 value into EGA VRAM
	;-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	sub		r6, #4
	;-------
	; Decrease the loop counter
	;-------
	subs	r3, #1
	bne		%b1
	;-------
	; Return
	;-------
	pop		{r6, lr}
	b		movsw_std_EGA_EGA_done

	LTORG
	
; ------------------- REP MOVSD ---------------------------------------
;

	;-------
	; Direction flag clear
	;-------
	GLOBAL	rep_movsd_cld_from_EGA
rep_movsd_cld_from_EGA
	calc_ega_r1
	mov		r0, r11					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_cld_EGA_RAM, rep_movsd_cld_EGA_EGA, unknown
	
	;-------
	; Direction flag set
	;-------
	GLOBAL	rep_movsd_std_from_EGA
rep_movsd_std_from_EGA
	calc_ega_r1
	mov		r0, r11					; r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_std_EGA_RAM, rep_movsd_std_EGA_EGA, unknown

rep_movsd_cld_EGA_RAM
rep_movsd_cld_EGA_EGA
rep_movsd_std_EGA_RAM
rep_movsd_std_EGA_EGA
	b		unknown

	GLOBAL	rep_movsd_cld_RAM_EGA
rep_movsd_cld_RAM_EGA
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			; Push used registers
	;-------
	; Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	;-------
	calc_ega_r2
	;-------
	; Check whether we need USE16 or USE32 address handling
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can move:
	; r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx
	lsl		r3, #2					; Number of bytes to move = 4*CX
	ror		esi, #16
	ror		edi, #16
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r3, esi		; ----- Check with SI
	min_cnt_idx_wrap_16 r3, edi		; ----- Check with DI
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3, lsl #16		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #16		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of bytes remaining
	ror		esi, #16
	ror		edi, #16
	;-------
	; Loop here for r3 bytes.
	; Registers:
	;	r0 = scratch (input byte)
	;	r1 = destroyed
	;	r2 = output VRAM pointer (ES:DI)
	;	r3 = loop counter
	;	r4 = input RAM pointer (DS:SI)
	;-------
2	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	;-------
	; Back to loop if more bytes to do
	;-------
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, r4}
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	b		rep_movsd_cld_next		; ... else handle the next 16K page.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full words to move
	beq		%f5						; Jump if no full halfwords fit here
	add		esi, r3, lsl #18		; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #18		; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full halfwords we shall move.
	;------
	; First move the full words we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r4, r1					; r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Move the remaining bytes to a possibly different page.
	;------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4}			; pop flags and 16-bit/32-bit mask
	
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			; r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			; r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #3				; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #3				; r0 = DI+3
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			; r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Adjust the registers, one more word moved.
	;------
	sub		ecx, #1
	mov		r0, #0x40000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can move:
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx, lsl #2			; Start with r3 = ECX*4
	min_cnt_next_page 	r3, r1		; ----- Check with DS:SI bytes before page end
	min_cnt_VGA_end 	r3, r2		; ----- Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #3					; Are we to move an uneven number of bytes?
	bne		%f5						; Yes, so handle this situation specially.
	add		esi, r3					; Increment ESI by the number of bytes we shall move.
	add		edi, r3					; Increment EDI by the number of bytes we shall move..
	sub		ecx, r3, lsr #2			; ecx = number of bytes remaining
	b		%b2						; The rest of the code is similar to USE16 version.
	;-------
	; Uneven number of bytes to move, so need special attention.
	;-------
5	movs	r3, r3, lsr #2			; r3 = number of full words to move
	beq		%f5						; Jump if no full words fit here
	add		esi, r3, lsl #2			; Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #2			; Increment DI by the number of bytes we shall move..
	sub		ecx, r3					; Decrement ECX by the number of full words we shall move.
	;------
	; First move the full words we can move
	;------
	mov		r4, r1					; r4 = input DS:SI pointer
1	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					; Increment output pointer
	subs	r3, #1
	bne		%b1
	mov		r1, r4
	;------
	; Then move the extra byte that will still fit to the same page.
	;------
5	mov		r4, r1					; r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			; r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Move the remaining bytes to a possibly different page.
	;------
	pop		{r0, r3, r4}			; pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #1				; r0 = SI+1
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #1				; r0 = DI+1
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #2				; r0 = SI+2
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #2				; r0 = DI+2
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	; Get the current effective segment from stack
	msr		cpsr_f,r0				; Restore flags
	add		r0, esi, #3				; r0 = SI+3
	mem_handler_jump_r0r3 %f1, bad_string_op_seg_back1, bad_string_op_seg_back1
1	mov		r1, r2					; r1 = DS:SI linear address
	add		r0, edi, #3				; r0 = DI+3
	mem_handler_jump_r0r3_ES_far bad_string_op_seg_back1, %f1, bad_string_op_seg_back1
1	mrs		r0, cpsr				; Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 ; Write it to ES:DI. TODO! This should be speeded up!
	;------
	; Adjust the registers, one more halfword moved.
	;------
	sub		ecx, #1
	add		esi, #4
	add		edi, #4
	pop		{r0}					; r0 = flags
	tst		ecx, r3					; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_movsd_cld_next		; ... else handle the next 16K page.

; ------------------- REP STOSB (CLD) ---------------------------------
;
	GLOBAL	rep_stosb_cld_EGA
rep_stosb_cld_EGA
	;-------
	; Registers here
	; r0 = free
	; r1 = free
	; r2 = physical ES:DI address (with EGA flags)
	; r3 must be saved
	; r4 = AX << 16, must be saved
	; r5 = CX = number of bytes to store, must be cleared
	; r6..r10 must be saved
	; r11 = DI = +/- (CX<<16)
	; r12, lr must be saved
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	cmp		r3, #0
	and		r3, ecx								; r3 = number of bytes to store
	blt		%f32
	;-------
	; 16-bit addressing
	;-------
	ror		edi, #16
	min_cnt_idx_wrap_16 r3, edi
	min_cnt_VGA_end 	r3, r2					; Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	add		edi, r3, lsl #16					; Increment DI by the number of bytes we stored.
	ror		edi, #16
	b		%f16
	;-------
	; 32-bit addressing
	;-------
32	min_cnt_VGA_end 	r3, r2					; Check with ES:DI bytes before VGA VRAM end
	add		edi, r3
	;-------
	; Common to both 16-bit and 32-bit addressing
	;-------
16	sub		ecx, r3								; CX = number of bytes remaining
	;-------
	; Loop here to store the bytes
	;-------
1	and		r0, eax, #0xFF
	bl		EGA_write_byte_r0_to_r2				; Saves r2!
	add		r2, #4
	subs	r3, #1
	bne		%b1
	pop		{r0, r3}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	b		rep_stosb_cld						; ... else handle the next 16K page.

; ------------------- REP STOSB (STD) ---------------------------------
; This is a silly and rare operation!
;
	GLOBAL	rep_stosb_std_EGA
rep_stosb_std_EGA_TODO
	;-------
	; Registers here
	; r0 = free
	; r1 = free
	; r2 = physical ES:DI address (with EGA flags)
	; r3 must be saved
	; r4 = AX << 16, must be saved
	; r5 = CX = number of bytes to store, must be cleared
	; r6..r10 must be saved
	; r11 = DI = +/- (CX<<16)
	; r12, lr must be saved
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, lr}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_address_before_B000_seg, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	sub		r3, r5, #1				; Start with r3 = CX, decrease it by one for the page checks.
	; ----- Check with ES:DI bytes in this page
	rsb		r0, r2					; r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	; ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #1					; Restore the counter
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		r11, r3, lsl #16		; Decrement DI by the number of bytes we shall store..
	sub		r5, r3					; r5 = number of bytes remaining
	;-------
	; Loop here to store the bytes
	;-------
1	mov		r0, r4, lsr #16
	bl		EGA_write_byte_r0_to_r2	; Saves r2!
	sub		r2, #4
	subs	r3, #1
	bne		%b1
	pop		{r0, r3, lr}
	cmp		r5, #0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosb_std			; ... else handle the next 16K page.

; ------------------- REP STOSW (CLD) ---------------------------------
;
	GLOBAL	rep_stosw_cld_EGA
rep_stosw_cld_EGA
	;-------
	; Registers here
	; r0 = free
	; r1 = free
	; r2 = physical ES:DI address (with EGA flags)
	; r3 must be saved
	; r4 = AX << 16, must be saved
	; r5 = CX = number of BYTES to store, must be cleared
	; r6..r10 must be saved
	; r11 = DI = +/- (CX<<16)
	; r12, lr must be saved
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; r4 = halfword value to store
	;-------
	mov		r4, eax, lsl #16
	orr		r4, r4, lsr #16			; r4 = value to store (in all 4 bytes)
	;-------
	; Test for 32bit store
	;-------
	tst		r3, #0x80000000
	bne		%f3
	;-------
	; USE16: Calculate the number of safe bytes we can store
	; r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	;-------
	and		r3, ecx								; r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #1								; Number of bytes to store = 2*CX
	ror		edi, #16
	min_cnt_idx_wrap_16 r3, edi
	min_cnt_VGA_end 	r3, r2					; Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1								; Are we to store an uneven number of bytes?
	bne		unknown							; Yes, so handle this situation specially.
	add		edi, r3, lsl #16					; Increment DI by the number of bytes we stored.
	ror		edi, #16
	sub		ecx, r3, lsr #1						; CX = number of bytes remaining
	;-------
	; Loop here to store the bytes
	;-------
2	mov		r0, eax
	bl		EGA_write_byte_r0_to_r2				; Saves r2!
	add		r2, #4
	subs	r3, #1
	bgt		%b2
	;-------
	; Return!
	;-------
	pop		{r0, r3, r4}
	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0				; Go back to the opcode loop if r5 == 0 ...
	b		rep_stosw_cld						; ... else handle the next 16K page.
	;-------
	; USE32: Calculate the number of safe bytes we can store
	; r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	;-------
3	mov		r3, ecx, lsl #1						; r3 = number of bytes/halfwords/dwords to store
	min_cnt_VGA_end 	r3, r2					; Check with ES:DI bytes before VGA VRAM end
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	tst		r3, #1								; Are we to store an uneven number of bytes?
	bne		unknown							; Yes, so handle this situation specially.
	add		edi, r3								; Increment EDI by the number of bytes we stored.
	sub		ecx, r3, lsr #1						; CX = number of bytes remaining
	b		%b2

; ------------------- REP STOSW (STD) ---------------------------------
; This is a silly and rare operation!
;
	GLOBAL	rep_stosw_std_EGA
rep_stosw_std_EGA_TODO
	;-------
	; Registers here
	; r0 = free
	; r1 = free
	; r2 = physical ES:DI+1 address (with EGA flags)
	; r3 must be saved
	; r4 = AX << 16, must be saved
	; r5 = CX = number of BYTES to store, must be cleared
	; r6..r10 must be saved
	; r11 = DI = +/- (CX<<16)
	; r12, lr must be saved
	;-------
	mrs		r0,cpsr					; Save flags (we are not allowed to change any)
	push	{r0, r3, r4, lr}
	;-------
	; Calculate physical ES:DI start address
	;-------
	calc_ega_r2
	;-------
	; Calculate the number of safe bytes we can store
	; r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	;-------
	sub		r3, r5, #1				; Start with r3 = CX, decrease it by one for the page checks.
	; ----- Check with ES:DI bytes in this page
	rsb		r0, r2					; r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	; ----- Check with DI
	sub		r3, #1					; DI points 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		; r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					; Restore the counter
	;-------
	; r4 = halfword value to store
	;-------
	orr		r4, r4, lsr #16			; r4 = value to store (in all 4 bytes)
	;-------
	; Test for odd number of bytes to store
	;-------
	tst		r5, #1					; If we wrote odd number of bytes the last time...
	rorne	r4, #8					; ... we need to swap AH and AL when continuing.
	;-------
	; Adjust the registers by the number of bytes we will store.
	;-------
	sub		r11, r3, lsl #16		; Increment DI by the number of bytes we shall store..
	sub		r5, r3					; r5 = number of bytes remaining
	;-------
	; Loop here to store the bytes
	;-------
1	ror		r4, #8
	and		r0, r4, #0xFF			; r0 = AH value
	bl		EGA_write_byte_r0_to_r2	; Store AH, saves r2!
	sub		r2, #4
	subs	r3, #1
	beq		%f2						; Jump if r3 == 0
	ror		r4, #8
	and		r0, r4, #0xFF			; r0 = AL value
	bl		EGA_write_byte_r0_to_r2	; Store AL, saves r2!
	sub		r2, #4
	subs	r3, #1
	bne		%b1
	;-------
	; Return!
	;-------
2	pop		{r0, r3, r4, lr}
	cmp		r5, #0
	beq		restore_flags_from_r0	; Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				; Restore flags
	b		rep_stosw_std			; ... else handle the next 16K page.

; ------------------- F3 AE = REPE SCASB ------------------------------
;
	GLOBAL	repe_scasb_cld_EGA
repe_scasb_cld_EGA
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repe_scasb_cld_EGA_USE32			; Yep, go handle it.
	ror		edi, #16
	min_cnt_VGA_end 	r1, r2					; Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	push	{r2}
	bl		EGA_read_byte_r2					; Get byte from physical ES:DI
	pop		{r2}
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24								; Byte to highest byte of r0
	add		r2, #4
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repe_scasb_cld						; ... and go handle the next 16K page.

repe_scasb_cld_EGA_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	min_cnt_VGA_end 	r1, r2					; Check with ES:DI bytes before VGA VRAM end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	push	{r2}
	bl		EGA_read_byte_r2					; Get byte from physical ES:DI
	pop		{r2}
	add		edi, #1								; Increment EDI
	add		r2, #4
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	;-------
	beq		%b1
	b		complement_carry					; We need to swap the Carry flag!


; ------------------- F2 AE = REPNE SCASB -----------------------------
;
	GLOBAL	repne_scasb_cld_EGA
repne_scasb_cld_EGA
	mrs		r0,cpsr								; Save flags (CX comparison must not change flags)
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	ands	r1, r3, ecx							; Start with r1 = CX
	beq		restore_flags_from_r0				; If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						; Is this a USE32 operation?
	bne		repne_scasb_cld_EGA_USE32			; Yep, go handle it.
	ror		edi, #16
	min_cnt_VGA_end 	r1, r2					; Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r1, edi					; ----- Check with DI
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%f5									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	push	{r2}
	bl		EGA_read_byte_r2					; Get byte from physical ES:DI
	pop		{r2}
	add		edi, #0x00010000					; Increment DI
	lsl		r0, #24								; Byte to highest byte of r0
	add		r2, #4
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	ror		edi, #16
	b		complement_carry					; We need to swap the Carry flag!
	;-------
	; r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	;-------
5	ror		edi, #16							; Restore EDI
6	tst		ecx, r3								; Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					; If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				; ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							; CX > 0, so restore flags ...
	b		repne_scasb_cld						; ... and go handle the next 16K page.

repne_scasb_cld_EGA_USE32
	;-------
	; Calculate the number of safe bytes we can scan
	; r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	;-------
	min_cnt_VGA_end 	r1, r2					; Check with ES:DI bytes before VGA VRAM end
	;-------
	; 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	;-------
1	mrs		r0,cpsr								; Save flags
	tst		r1, r1
	beq		%b6									; Go check whether we need to exit the iteration or handle next 16K block
	;-------
	; 4) Perform the string operation once.
	;-------
	push	{r2}
	bl		EGA_read_byte_r2					; Get byte from physical ES:DI
	pop		{r2}
	add		edi, #1								; Increment EDI
	add		r2, #4
	lsl		r0, #24								; Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					; Subtract the byte from AL
	;-------
	; 5) Decrement CX, no flags are modified.
	;-------
	sub		ecx, #1								; Decrement CX
	sub		r1, #1								; Decrement 16K block counter
	;-------
	; 6) Check the Zero flag if the string operation is SCAS or CMPS.
	;	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	;-------
	bne		%b1
	b		complement_carry					; We need to swap the Carry flag!

; ------------------- F6 = ??? r/m8 -----------------------------------
;
	GLOBAL	test_EGA_r2_imm8
test_EGA_r2_imm8
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	ldrb	r1, [r12], #1
	lsl		r0, #24					; r0 = byte at address [r2], shifted to high byte
	lsl		r1, #24					; r1 = imm8 byte, shifted to high byte
	tst		r0, r1
	b		loop

; ------------------- F7 = ??? r/m16 ----------------------------------
;
	GLOBAL	test_EGA_r2_imm16
test_EGA_r2_imm16
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r1, #0
	msr		cpsr_f, r1				; Clear all flags (especially C and O)
	ldrb	r1, [r12], #1
	ldrb	r2, [r12], #1
	lsl		r0, #16
	lsl		r1, #16
	orr		r1, r2, lsl #24
	tst		r0, r1
	b		loop

; ------------------- FE = INC/DEC r/m8 -------------------------------
; INC x is like ADD x,1, except the Carry flag is not changed.
;

	GLOBAL	inc_byte_EGA_r2
inc_byte_EGA_r2
	mrs		r3,cpsr					; Get original flags to r3.
	calc_ega_r2
	push	{r2}					; Save used registers
	bl		EGA_read_byte_r2
	lsl		r0, #24
	adds	r0, #0x01000000			; Add 1 to the high byte
	mrs		r1,cpsr					; Save new flags to r1
	lsr		r0, #24
	ldr		r2, [sp]				; Get the address from stack, ...
	str		r1, [sp]				; ... and svae the new flags there.
	bl		EGA_write_byte_r0_to_r2
	;-------
	; Fix the Carry flag
	;-------
	pop		{r1}					; r3 = original flags, r1 = new flags
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	and		r3, #0x20000000			; r0 = Only the original Carry flag bit
	orr		r0, r3, r1				; r0 = new flags + original Carry flag
	;-------
	; Return
	;-------
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0

	GLOBAL	dec_byte_EGA_r2
dec_byte_EGA_r2
	mrs		r3,cpsr					; Get original flags to r3.
	calc_ega_r2
	push	{r2}					; Save used registers
	bl		EGA_read_byte_r2
	lsl		r0, #24
	subs	r0, #0x01000000			; Subtract 1 from the high byte
	mrs		r1,cpsr					; Save new flags to r1
	lsr		r0, #24
	ldr		r2, [sp]				; Get the address from stack, ...
	str		r1, [sp]				; ... and svae the new flags there.
	bl		EGA_write_byte_r0_to_r2
	;-------
	; Fix the Carry flag
	;-------
	pop		{r1}					; r3 = original flags, r1 = new flags
	bic		r1, #0x20000000			; r1 = New flags with Carry cleared
	and		r3, #0x20000000			; r0 = Only the original Carry flag bit
	orr		r0, r3, r1				; r0 = new flags + original Carry flag
	;-------
	; Return
	;-------
	b		restore_flags_from_r0	; Jump back to loop, setting the flags from r0


; ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
;
; 26FF37 push word es:[bx] (Lords of Doom)
; 26FF7702 push word es:[bx+02] (Lords of Doom)
;
	GLOBAL	push_EGA_r2
push_EGA_r2
	mrs		r3,cpsr					; Get original flags to r3.
	calc_ega_r2
	bl		EGA_read_hword_r2
	msr		cpsr_f, r3				; Restore flags
	push_hword r0, r1, r2
	b		loop

;-------------------- Print a character to the screen -----------------
; Called by a C routine, r0 = char, r1 = color (0..16)|page<<8, r2=number of times.
; Use the cursor position from BIOSData[0x50] and ROM8x8font.
; We can change r0..r3, must save other registers.
;

	MACRO 
	put_font_row
	ldr		r0, [r5, r0, lsl #2]
	and		r0, r1, r0
	str		r0, [r4]
	add		r4, r7, lsl #2
	MEND

CharToEGAScreen
	push	{r4-r8}
	;-------
	; Setup r7, r6, r1 and r2
	;-------
	ldr		r7, =VGAOffset
	mov		r6, r2					; r6 = counter
	mov		r2, r1, lsr #8			; r2 = page
	and		r1, #0xFF				; r1 = color
	ldrb	r7, [r7]				; r7 = 20 (320x) or 40 (640x)
	;-------
	; Calculate screen address to r4
	;-------
	ldr		r8,=BIOSData
	mov		r3, #0x50
	ldr		r8, [r8]
	add		r3, r2, lsl #1			; Use the cursor position of the correct page
	ldrh	r5, [r8, #0x4C]			; r5 = size of the screen page in bytes (0x2000 or 0x4000)
	ldrh	r4, [r8, r3]			; r4 low byte = column, high byte = row
	ldrb	r8, [r8, #0x85]			; r8 = character height of the current font
	mul		r2, r5, r2				; r2 = start offset of this page (in emulated bytes)
	and		r3, r4, #0xFF			; r3 = column (0..39)
	lsr		r4, #8					; r4 = row (0..24)
	lsl		r7, #1					; r7 = 40 or 80
	mul		r5, r7, r8				; r5 = 40*cheight or 80*cheight
	mul		r4, r5, r4				; r4 = 40*cheight*row or 80*cheight*row
	add		r4, r3					; r4 = column+40*cheight*row
	ldr		r3, =EGAVGA_A000
	ldr		r3, [r3]
	add		r4, r3, r4, lsl #2		; Each byte is a word in the EGA RAM
	add		r4, r2, lsl #2			; Each page is 0x2000*4 or 0x4000*4 bytes
	;-------
	; Get address to font data
	; Registers now
	;	r0 = char to output
	;	r1 = color
	;	r2 = free
	;	r3 = free
	;	r4 = output position
	;	r5 = free
	;	r6 = counter
	;	r7 = screen offset value
	;	r8 = character height
	;-------
	ldr		r3, =INTVectors
	ldr		r3, [r3]				; Now r3 = INTVectors address
	ldr		r3, [r3, #(0x43*4)]		; Get the current INT43 (font pointer) vector address
	mov		r2, r3, lsr #16			; r2 = segment of the font pointer
	push	{r0, r1}
	lsl		r3, #16
	lsl		r2, #4
	add		r2, r3, lsr #16			; r2 = logical start address of the font
	;-------
	; calc_linear_address_r2
	;-------
	ldr		r0,=EMSPageTable
	mov		r1, r2, lsr #TLB_GRAN_BITS			; r1 = 16K page number
	ldr		r0, [r0]							; r0 = index into static TLB table
	ldr		r0, [r0, r1, lsl #2]				; r0 = physical start address of the page
	add		r3, r2, r0, lsl #4					; r2 = physical font start address
	; -----
	pop		{r0, r1}
	mul		r0, r8, r0				; r0 = char*cheight
	ldrb	r0, [r3, r0]!			; r3 = font position, r0 = first row of font
	ldr		r5, =byte2wordtbl
	;-------
	; Prepare color value
	;-------
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	;-------
	; Write 8x8 pixels
	;-------
2
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
	cmp		r8, #8
	beq		%f1
	;-------
	; Write font rows 9-14 (EGA)
	;-------
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
	cmp		r8, #14
	beq		%f1
	;-------
	; Write font rows 15-16 (VGA)
	;-------
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	;-------
	; One character done, test if we need to write more.
	;-------
1	subs	r6, #1
	popeq	{r4-r8}
	bxeq	lr						; Return to caller
	sub		r3, r8					; Back to first row of font
	add		r3, #1
	mul		r0, r7, r8				; r0 = 40*cheight or 80*cheight
	sub		r4, r0, lsl #2
	add		r4, #4					; Next column
	ldrb	r0, [r3]				; Get first row of font
	b		%b2

;-------------------- Draw a pixel to the screen ----------------------
; Called by a C routine, r0 = pixel color, r1 = X, r2 = Y
; We can change r0..r3, must save other registers.
;
PixelToEGAScreen
	push	{r4-r8}
	;-------
	; Calculate the byte address of the pixel
	;-------
	ldr		r7, =VGAOffset
	ldr		r3, =EGAVGA_A000
	ldrb	r7, [r7]				; r7 = 20 (320x) or 40 (640x)
	ldr		r3, [r3]
	lsl		r7, #1
	mul		r4, r2, r7				; r4 = 40*Y or 80*Y
	add		r4, r1, lsr #3			; r4 = (X/8)+40*Y = byte in EGA RAM to change
	add		r4, r3, r4, lsl #2		; Each byte is a word in the EGA RAM
	;-------
	; Calculate the mask to use
	;-------
	and		r1, #7
	lsl		r1, #2					; r1 = 4*X
	mov		r12, #0xF
	lsl		r12, r1
	;-------
	; Prepare color value
	;-------
	tst		r0, #0x80				; Should we XOR the pixel?
	and		r0, #15					; r0 = color (0..15)
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	and		r0, r12
	;-------
	; Write the color to EGA VRAM
	;-------
	ldr		r2, [r4]
	biceq	r2, r12
	orreq	r2, r0
	eorne	r2, r0
	str		r2, [r4]
	pop		{r4-r8}
	bx		lr

;-------------------- Read a pixel from the screen --------------------
; Called by a C routine, r0 = X, r1 = Y
; We can change r0..r3, must save other registers.
;
PixelFromEGAScreen
	push	{r4-r8}
	;-------
	; Calculate the byte address of the pixel
	;-------
	ldr		r7, =VGAOffset
	ldr		r3, =EGAVGA_A000
	ldrb	r7, [r7]				; r7 = 20 (320x) or 40 (640x)
	ldr		r3, [r3]
	lsl		r7, #1
	mul		r4, r1, r7				; r4 = 40*Y or 80*Y
	add		r4, r0, lsr #3			; r4 = (X/8)+40*Y = byte in EGA RAM to change
	add		r4, r3, r4, lsl #2		; Each byte is a word in the EGA RAM
	;-------
	; Calculate the mask to use
	;-------
	and		r1, r0, #7
	lsl		r1, #2					; r1 = 4*X
	mov		r12, #0xF
	lsl		r12, r1
	;-------
	; Get the color from EGA VRAM
	;-------
	ldr		r0, [r4]
	and		r0, r12
	lsr		r0, r1
	pop		{r4-r8}
	bx		lr

	LTORG
	
	;=================== EGA/VGA/MCGA PORTS ===========================


	;-------
	; Write to port 3C0 from a C routine
	;	Out3C0(int reg_idx, int value);
	;-------
	GLOBAL	Out3C0
Out3C0
	ldr		r2, =VGA_attr_data_3C0
	strb	r0, [r2, #-1]			; Set the current address/flipflop value
	;-------
	; VGA Attribute Controller Registers.
	;-------
out_3C0_VGA_attr
	ldr		r2, =VGA_attr_data_3C0
	ldrsb	r0, [r2, #-1]			; Get the current address/flipflop value
	cmp		r0, #0					; Are we to write the new register address value?
	blt		out_3C0_address			; Yep, go write the address
	;-------
	; We are to write the register data
	;-------
	IF (REG_DEBUG:AND:1) = 1
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	debugreg r1
	and r1, #0xFF
	ENDIF
	cmp		r0, #0x10				; Is this an EGA palette register write?
	orr		r0, #0x80
	strb	r0, [r2, #-1]			; Save the new address/flipflop value
	bic		r0, #0x80				; Restore r0 value
	beq		out_3C0_mode_control	; Jump if this was a mode control register change
	bgt		out_3C0_other			; Jump if other attribute registers
	strb	r1, [r2, r0]			; Save the new data
	;-------
	; Change an EGA palette register value 
	;-------
	ldr		r2, =BIOSData
	ldr		r2, [r2]
	ldrb	r2, [r2, #0x49]					; Get current screen mode 
	cmp		r2, #0x13						; Mode 13?
	bxge	lr								; Yep, we can ignore the palette change
	;-------
	; VGA_ATTR_SetPalette(Bit8u index,Bit8u val)
	; Input
	; 	r0 = index = palette register number, 0..F
	; 	r1 = val = register value
	;	lr = return address
	; Destroys
	;	r0, r1, r2
	; Palette register change in EGA mode, so we need to copy the
	; palette value from the current EGA palette table (at 0x05000000+512+32).
	; - Fixes Terminator 2 - Judgment Day
	;
	; Handle the palette blocks (16x16 or 4x64)
	; - Fixes SimFarm
	; if (vga.attr.mode_control & 0x80) val = (val&0xf) | (vga.attr.color_select << 4);
	; val &= 63;
	; val |= (vga.attr.color_select & 0xc) << 4;
	;-------
VGA_ATTR_SetPalette
	ldr		r2, =VGA_attr_data_3C0
	orr		r1, r0, lsl #24			; Combine r1 and r0 to r1
	ldrb	r0, [r2, #0x10]			; r0 = vga.attr.mode_control
	tst		r0, #0x80				; if (vga.attr.mode_control & 0x80)
	ldrb	r0, [r2, #0x14]
	bicne	r1, #0xFF0
	orrne	r1, r0, lsl #4			;	val = (val&0xf) | (vga.attr.color_select << 4);
	and		r2, r0, #0xC
	mov		r0, r1, lsr #24
	and		r1, #63					; val &= 63;
	orr		r1, r2, lsl #4			; val |= (vga.attr.color_select & 0xc) << 4;
	;-------
	; VGA_DAC_CombineColor(index,val);
	;-------
	ldr		r2, =VGA_dac_combine
	strb	r1, [r2, r0]
	;-------
	; static void VGA_DAC_SendColor( Bitu index, Bitu src )
	; Input
	;	r0 = index
	;	r1 = src
	;	lr = return address
	; Destroys
	;	r0, r1, r2
	;-------
	GLOBAL VGA_DAC_SendColor
VGA_DAC_SendColor
	ldr		r2, =BG_PALETTE
	add		r0, r2, r0, lsl #2		; r0 = *(BG_PALETTE[index]) = where we need to copy the value to
	add		r2, #256*4				; r2 points to EGA_PALETTE_SAVE
	ldr		r1, [r2, r1, lsl #2]	; r1 = Palette value to use (src)
	str		r1, [r0]				; BG_PALETTE[pal_idx] = pal_color
	bx		lr						; return
	
out_3C0_mode_control
	add		r2, r0
	ldrb	r0, [r2]				; Get the original value to r0
	strb	r1, [r2]				; Save the new value 
	eor		r0, r1
	tst		r0, #0x80				; Did the highest bit change?
	bxeq	lr						; Nope, just return
	;-------
	; Internal Palette Size bit changed!
	; We need to re-send all the 16 colors.
	;-------
	mov		r0, #0
1	ldr		r2, =VGA_attr_data_3C0
	push	{r0, lr}
	ldrb	r1, [r2, r0]			; r1 = vga.attr.palette[i]
	bl		VGA_ATTR_SetPalette
	pop		{r0, lr}
	add		r0, #1
	cmp		r0, #16
	blt		%b1
	bx		lr
out_3C0_other
	cmp		r0, #0x14				; Color Select Register?
	strbne	r1, [r2, r0]			; Nope, just save the new data
	bxne	lr						; Nope, just return
	add		r2, r0
	ldrb	r0, [r2]				; Get the original value to r0
	strb	r1, [r2]				; Save the new value 
	eors	r0, r1					; Did the value change?
	bxeq	lr						; Nope, just return
	;-------
	; Color Select Register changed!
	; We need to re-send all the 16 colors.
	;-------
	mov		r0, #0
	b		%b1
	
	;-------
	; We are to write the register address
	;-------
out_3C0_address
	and		r1, #0x1F				; only use the lower 5 bits
	cmp		r1, #0x14				; We have at most 0x14 registers
	movgt	r1, #0x14
	strb	r1, [r2, #-1]			; Save the new address/flipflop value
	bx		lr
in_3C0_VGA_attr
	bic		eax, #0xFF				; Clear AL (copied from DOSBox)
	bx		lr
in_3C1_VGA_attr
	ldr		r2, =VGA_attr_data_3C0
	ldrb	r0, [r2, #-1]			; Get the current address/flipflop value
	and		r0, #0x1F
	ldrb	r1, [r2, r0]
	bic		eax, #0xFF				; Clear AL first
	orr		eax, r1					; Set AL value from r1
	bx		lr
	
	;-------
	; VGA Miscellaneous Output Register.
	;	bit 7 = Vertical Sync Polarity
	;	bit 6 = Horizontal Sync Polarity
	;	bit 5 = Page Bit for Odd/Even
	;	bit 4 = Disable Video Drivers (EGA)
	;	bit 3-2 = Clock Select
	;	bit 1 = Enable RAM
	;	bit 0 = I/O Address (0=mono=3BX, 1=color=3DX)
	;-------
out_3C2_VGA_misc
	ldr		r2, =VGA_misc_3C2
	strb	r1, [r2]				; Save the VGA Misc register value
	;-------
	; Screen height might have changed, call the C routine.
	;-------
	push	{r3, r12, lr}
	bl		ScreenConfigChanged		; Call the C routine to update the screen configuration
	pop		{r3, r12, pc}
in_3CC_VGA_misc
	ldr		r2, =VGA_misc_3C2
	ldrb	r1, [r2]				; r1 = VGA Misc Register value
	bic		eax, #0xFF				; Clear AL first
	orr		eax, r1					; Set AL value from r1
	bx		lr

	;-------
	; VGA Sequencer Registers.
	; 	- index 0 = Reset
	;	- index 1 = Clocking Mode
	;	- index 2 = Map Mask
	;	- index 3 = Character Map Select
	;	- index 4 = Memory Mode
	;-------
out_3C4_VGA_sequencer
	ldr		r2, =VGA_Sequencer_addr_3C4
	strb	r1, [r2]
	bx		lr
in_3C4_VGA_sequencer
	ldr		r2, =VGA_Sequencer_addr_3C4
	ldrb	r1, [r2]
	bic		eax, #0xFF				; Clear AL first
	orr		eax, r1					; Set AL value from r1
	bx		lr
	
out_3C5_VGA_sequencer
	ldr		r2, =VGA_Sequencer_regs_3C5
	ldrb	r0, [r2, #-1]
	IF (REG_DEBUG:AND:2) = 2
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	orr	r1, #0x0C50000
	debugreg r1
	and r1, #0xFF
	ENDIF
	cmp		r0, #4
	strble	r1, [r2, r0]					; Store the new Sequencer Register value
	beq		out_3C5_MemoryMode				; Jump if it is the MemoryMode register
	cmp		r0, #2							; Is it the Map Mask register?
	bxne	lr								; Nope, just return
	;-------
	; Map Mask register value changed.
	;-------
	;-------
	; Calculate new emulated EGA write mask value
	;-------
	cmp		r1, #15
	andgt	r1, #15
	strbgt	r1, [r2, r0]					; Store again the new Sequencer Register value (anded with 15)
	mov		r0, r1
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	str		r1, [r2, #(EGA_WRITE_MASK32-VGA_Sequencer_regs_3C5)]
	;-------
	; Calculate new emulated Mode-X write mask value
	;-------
	add		r1, r2, #(ModeXMaskTbl-VGA_Sequencer_regs_3C5)
	ldr		r0, [r1, r0, lsl #2]
	str		r0, [r2, #(MODEX_WRITE_MASK32-VGA_Sequencer_regs_3C5)]
	bx		lr
	;-------
	; Memory Mode register value changed, check if this was entry to Mode-X (or exit from it).
	;-------
out_3C5_MemoryMode
	ldr		r2, =BIOSData
	ldr		r2, [r2]
	ldrb	r2, [r2, #0x49]					; Get current screen mode 
	cmp		r2, #0x13						; Mode 13?
	bxne	lr								; Nope, we can ignore the write
	;-------
	; Memory Mode register changed while in mode 0x13, possible Mode-X entry/exit!
	;-------
	push	{r3, r12, lr}
	bl		ScreenConfigChanged				; Call the C routine to update the screen configuration
	pop		{r3, r12, pc}
	;-------
	; VGA Sequencer Registers.
	; 	- index 0 = Reset
	;	- index 1 = Clocking Mode
	;	- index 2 = Map Mask
	;	- index 3 = Character Map Select
	;	- index 4 = Memory Mode
	;-------
in_3C5_VGA_sequencer
	ldr		r2, =VGA_Sequencer_addr_3C4
	ldrb	r1, [r2]						; r1 = Sequencer register index
	add		r1, #1							; Point to VGA_Sequencer_regs_3C5
	ldrb	r1, [r2, r1]					; Get the Sequencer Register value into r1
	and		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr

	;-------
	; VGA PEL Address Read Mode Register
	;	- pal_idx = AL
	;	- pal_rgb = 0
	;	- pal_color = 0
	;-------
out_3C7_VGA_pel
	ldr		r2, =VGA_palette_read
	lsl		r1, #24							; pal_idx = AL, pal_rgb=0, pal_color=0
	str		r1, [r2]						; Save VGA_palette read word
	add		r1, #0x01000000
	str		r1, [r2, #(VGA_palette_write-VGA_palette_read)]	; Save VGA_palette write word (Star Control 2)
	mov		r1, #0
	strb	r1, [r2, #(VGA_palette_3C7-VGA_palette_read)]	; Tell read mode is in effect
	bx		lr

	;-------
	; VGA DAC State Register
	;-------
in_3C7_VGA_DAC
	ldr		r2, =VGA_palette_3C7
	ldrb	r1, [r2]						; r1 = Sequencer register index
	bic		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr

	;-------
	; VGA PEL Address Write Mode Register
	;	- pal_idx = AL
	;	- pal_rgb = 0
	;	- pal_color = 0
	;-------
out_3C8_VGA_pel
	ldr		r2, =VGA_palette_write
	lsl		r1, #24					; pal_idx = AL, pal_rgb=0, pal_color=0
	str		r1, [r2]				; Save VGA_palette data word
	mov		r1, #3
	strb	r1, [r2, #(VGA_palette_3C7-VGA_palette_write)]	; Tell write mode is in effect
	bx		lr

	;-------
	; VGA PEL Address Write Mode Register
	;-------
in_3C8_VGA_pel
	ldr		r2, =VGA_palette_write
	ldr		r1, [r2]							; Get VGA_palette data word
	lsr		r1, #24
	bic		eax, #0xFF							; Clear AL first
	orr		eax, r1								; Set AL value from r1
	bx		lr

	;-------
	; 0x3C9 = VGA PEL Data Register (used to set palette data)
	;		switch( pal_rgb )
	;		{
	;			case 0:	; Write the Red component
	;				pal_color = (AL>>1)<<11;
	;				pal_rgb++;
	;				return true;
	;			case 1:	; Write the Green component
	;				pal_color |= ((AL>>1)<<6);
	;				pal_rgb++;
	;				return true;
	;			case 2:	; Write the Blue component, and increment index
	;				pal_color |= (AL>>1);
	;				BG_PALETTE[pal_idx++] = pal_color;
	;				pal_idx &= 0xFF;
	;				pal_rgb = 0;
	;				pal_color = 0;
	;				return true;
	;		}
	;		break;
	;-------
out_3C9_VGA_pel
	ldr		r2, =VGA_palette_write
	lsr		r1, #1					; r1 = AL>>1
	ldr		r0, [r2]				; Get VGA_palette data word
	and		r1, #0x1F				; r1 = new palette data, 0..31
	and		r3, r0, #0x00030000		; Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r1, r1, lsl #11			; Red channel if r3 == 0
	moveq	r1, r1, lsl #6			; Green channel if r3 == 1
	orr		r0, r1					; pal_color |= (AL >> 1) << pal_rgb
	add		r0, #0x00010000			; pal_rgb++
	strle	r0, [r2]				; All done if not yet Blue component handled
	bxle	lr
	;-------
	; case 2: Store the new palette value
	;-------
	ldr		r3, =BG_PALETTE
	add		r1, r3, r0, lsr #(24-2)	; r1 = BG_PALETTE[pal_idx]
	strh	r0, [r1]				; BG_PALETTE[pal_idx] = pal_color
	add		r0, #0x01000000			; pal_idx++
	and		r0, #0xFF000000			; pal_rgb = 0, pal_color = 0
	str		r0, [r2]
	bx		lr

	;-------
	; Routine for EGA mode, all the higher palette registers need to be
	; copies of the first 16 palette registers.
	;-------
out_3C9_EGA_pel
	ldr		r2, =VGA_palette_write	; Assume these are VGA palette registers
	lsr		r1, #1					; r1 = AL>>1
	ldr		r0, [r2]				; Get VGA_palette data word
	and		r1, #0x1F				; Fix for Prehistorik etc.
	and		r3, r0, #0x00030000		; Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r1, r1, lsl #11			; Red channel if r3 == 0
	moveq	r1, r1, lsl #6			; Green channel if r3 == 1
	orr		r0, r1					; pal_color |= (AL >> 1) << pal_rgb
	add		r0, #0x00010000			; pal_rgb++
	strle	r0, [r2]				; Save VGA_palette data word
	bxle	lr
	;-------
	; case 2: Store the new palette value. We need to store it to the
	; EGA palette save area, and also to the currently
	; active palette index, if the attribute registers have this index
	; mapped to an active EGA color.
	; First store it to the save area.
	;-------
	ldr		r1, =EGA_PALETTE_SAVE
	mov		r3, r0, lsr #24			; r3 = palette register index		
	lsl		r3, #2
	strh	r0, [r1, r3]			; Indexed as words instead of halfwords
	;-------
	; Check for attributes and DAC entry link
	; for (Bitu i=0;i<16;i++) {
	;	if (vga.dac.combine[i]==vga.dac.write_index) {
	;		VGA_DAC_SendColor( i, vga.dac.write_index );
	;	}
	; }
	;-------
	add		r2, #(VGA_dac_combine - VGA_palette_write)
	mov		r3, #15
1	ldrb	r1, [r2, r3]
	cmp		r1, r0, lsr #24			; Is this the correct palette index?
	beq		%f2						; Yep, use r3 as the index
	subs	r3, #1					; Go to next attribute register
	bge		%b1						; Back to loop until all 16 checked
	;-------
	; The palette register we updated is not used by the attribute registers,
	; so skip the hardware palette update.
	;-------
	sub		r2, #(VGA_dac_combine - VGA_palette_write)
	add		r0, #0x01000000			; pal_idx++
	and		r0, #0xFF000000			; pal_rgb = 0, pal_color = 0
	str		r0, [r2]
	bx		lr
	;-------
	; Found the correct hardware palette register index.
	; Call VGA_DAC_SendColor( i, vga.dac.write_index );
	;-------
2	mov		r1, r0, lsr #24			; r1 = vga.dac.write_index
	sub		r2, #(VGA_dac_combine - VGA_palette_write)
	add		r0, #0x01000000			; pal_idx++
	and		r0, #0xFF000000			; pal_rgb = 0, pal_color = 0
	str		r0, [r2]
	mov		r0, r3					; r0 = i
	b		VGA_DAC_SendColor

	;-------
	; 0x3C9 = VGA PEL Data Register (used to get palette data)
	;	BG_PALETTE = ((u16*)0x05000000)
	;		switch( pal_rgb )
	;		{
	;			case 0
	;				pal_rgb++;
	;				return (BG_PALETTE[pal_idx]&0x1F)<<1;
	;			case 1
	;				pal_rgb++;
	;				return ((BG_PALETTE[pal_idx]>>5)&0x1F)<<1;
	;			case 2
	;				{
	;					int tmp = ((BG_PALETTE[pal_idx]>>10)&0x1F)<<1;
	;					pal_idx++;
	;					pal_idx &= 0xFF;
	;					pal_rgb = 0;
	;					return tmp;
	;				}
	;		}
	;		break;
	;-------
in_3C9_VGA_pel
	ldr		r2, =VGA_palette_read	; Get address of pal_idx, pal_rgb, pal_color
	ldr		r3, =BG_PALETTE			; r3 = *(BG_PALETTE[0])
	ldr		r1, [r2]				; r1 = pal_idx, pal_rgb, pal_color		0xFB0A0000
	lsr		r0, r1, #24
	ldr		r0, [r3, r0, lsl #2]	; r0 = BG_PALETTE[pal_idx]
	and		r3, r1, #0x00030000		; Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r0, r0, lsr #11			; Red channel if r3 == 0
	moveq	r0, r0, lsr #6			; Green channel if r3 == 1
	and		r0, #0x1F
	bic		eax, #0xFF				; Clear AL first
	orr		eax, r0, lsl #1			; AL = ((BG_PALETTE[pal_idx]>>pal_rgb)&0x1F)<<1;

	add		r1, #0x00010000			; pal_rgb++
	addgt	r1, #0x01000000			; If already Blue component, pal_idx++
	bicgt	r1, #0x00FF0000			; If already Blue component, pal_rgb = 0
	str		r1, [r2]				; Save VGA_palette data word
	bx		lr


	;-------
	; VGA Graphics Controller Registers.
	; 	- index 0 = Set/Reset
	;	- index 1 = Enable Set/Reset
	;	- index 2 = Color Compare
	;	- index 3 = Data Rotate
	;	- index 4 = Read Map Select
	;	- index 5 = Mode
	;	- index 6 = Miscellaneous
	;	- index 7 = Color Dont Care
	;	- index 8 = Bit Mask
	;-------
out_3CE_VGA_graphics
	ldr		r2, =VGA_Graphics_addr_3CE
	strb	r1, [r2]
	bx		lr
	;-------
	; Write to port 3CF from a C routine
	;	Out3CF(int reg_idx, int value);
	;-------
	GLOBAL	Out3CF
Out3CF
	ldr		r2, =VGA_Graphics_regs_3CF
	strb	r0, [r2, #-1]			; Set the current address/flipflop value
out_3CF_VGA_graphics
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r0, [r2, #-1]
	IF (REG_DEBUG:AND:4) == 4
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	orr	r1, #0x0CF0000
	debugreg r1
	and r1, #0xFF
	ENDIF
	cmp		r0, #8
	strble	r1, [r2, r0]					; Store the new Graphics Register value
	bxgt	lr								; Return if an invalid index

	IF 1 = 1

	adr		r3, out_3CF_jump
	ldr		pc,[r3, r0, lsl #2]				; Jump to the special handler of the register index
	ALIGN	4
out_3CF_jump
	DCD 	set_reset, enable_set_reset, color_compare, bxlr, read_map_select, bxlr, bxlr, color_dont_care, bit_mask
	
	ELSE

	tbb		[pc, r0]						; Table Branch Byte to the correct handler routine
out_3CF_branches
	DCB 	((set_reset - out_3CF_branches)/2)
	DCB 	((enable_set_reset - out_3CF_branches)/2)
	DCB 	((color_compare - out_3CF_branches)/2)
	DCB 	((bxlr - out_3CF_branches)/2)
	DCB 	((read_map_select - out_3CF_branches)/2)
	DCB 	((bxlr - out_3CF_branches)/2)
	DCB 	((bxlr - out_3CF_branches)/2)
	DCB 	((color_dont_care - out_3CF_branches)/2)
	DCB 	((bit_mask - out_3CF_branches)/2)
	DCB		0								; Need to have the adresses even!

	ENDIF

	;-------
	; Set/Reset register value changed
	; Index 0 = Set/Reset, bits 0..3
	;-------
set_reset
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	str		r1, [r2, #(EGA_SET_RESET32-VGA_Graphics_regs_3CF)]
	bx		lr
	;-------
	; Snable Set/Reset register value changed
	; Index 1 = Enable Set/Reset, bits 0..3
	;-------
enable_set_reset
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	str		r1, [r2, #(EGA_ENABLE_SET_RESET32-VGA_Graphics_regs_3CF)]
	bx		lr
	;-------
	; Color Compare register value changed
	; Index 2 = Color Compare, bits 0..3
	;-------
color_compare
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	str		r1, [r2, #(EGA_COLOR_COMPARE32-VGA_Graphics_regs_3CF)]
	bx		lr
	;-------
	; Read Map Select register value changed
	; Index 4 = Read Map Select, bits 0..1
	;-------
read_map_select
	and		r1, #3
	ldr		r0, =0x11111111
	mov		r0, r0, lsl r1
	str		r0, [r2, #(EGA_READ_MASK32-VGA_Graphics_regs_3CF)]
	bx		lr
	
mode
	;debugreg r1
bxlr
	bx		lr
	
	;-------
	; Color Dont Care register value changed
	; Index 7 = Color Dont Care
	;-------
color_dont_care
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	str		r1, [r2, #(EGA_COLOR_DONT_CARE32-VGA_Graphics_regs_3CF)]
	bx		lr
	;-------
	; Bit Mask register value changed
	; Index 8 = Bit Mask Register, bits 0..7 (= which bits are modified during write)
	;-------
bit_mask
	add		r2, #(byte2wordtbl-VGA_Graphics_regs_3CF)
	ldr		r1, [r2, r1, lsl #2]			; r1 = Bit Mask adjusted from Byte to Word
	str		r1, [r2, #(EGA_BIT_MASK32-byte2wordtbl)]
	bx		lr

in_3CE_VGA_graphics
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r1, [r2, #-1]
	bic		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr

in_3CF_VGA_graphics
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r1, [r2, #-1]
	ldrb	r1, [r2, r1]					; Get the CRTC Register value into r1
	bic		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr

	
	;-------
	; 0x3D4 = VGA CRTC Address Register
	;-------
out_3D4_VGA_CRTC_addr
	ldr		r2, =VGA_CRTC_addr_3D4
	strb	r1, [r2]
	bx		lr

in_3D4_VGA_CRTC_addr
	ldr		r2, =VGA_CRTC_addr_3D4
	ldrb	r1, [r2]
	bic		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr

	;-------
	; 0x3D5 = VGA CRTC Data Register
	;-------
out_3D5_VGA_CRTC_data
	ldr		r2, =VGA_CRTC_data_3D5
	ldrb	r0, [r2, #-1]					; Which of the 24 registers we are to change?
	cmp		r0, #0x18
	strble	r1, [r2, r0]					; Store the new CRTC data value
	cmp		r0, #1							; Is it index 1?
	cmpne	r0, #9							; or index 9?
	bxne	lr								; Nope, just return
	;-------
	; Screen config might have changed, call the C routine.
	;-------
	push	{r3, r12, lr}
	bl		ScreenConfigChanged				; Call the C routine to update the screen configuration
	pop		{r3, r12, pc}

in_3D5_VGA_CRTC_data
	ldr		r2, =VGA_CRTC_data_3D5
	ldrb	r1, [r2, #-1]					; Which of the 24 registers we are to change?
	ldrb	r1, [r2, r1]					; Get the CRTC Register value into r1
	bic		eax, #0xFF						; Clear AL first
	orr		eax, r1							; Set AL value from r1
	bx		lr


	LTORG
	
; ------------------- COPY TO SCREEN ----------------------------------
;
SCREEN_WIDTH	EQU		1024

	;-------
	; Copy a 320x200x16 EGA mode graphics screen to physical screen.
	; Each pixel consists of a bit in four different bit planes.
	; Each page occupies 8000 bytes (0x2000) in the A000 memory segment.
	;-------
	GLOBAL screen_copy_0D
screen_copy_0D
	push    {r4-r12, lr}
	ldr		r2,=VGAStartAddrHigh
	ldr		r1,=EGAVGA_A000			; VGA screen start
	mov		r10, r0					; r10 = input parameter value = s_pixels
	ldr		r1, [r1]
	;-------
	; Calculate the display pitch value (vga.draw.address_add in DOSBox)
	; from CRTC registers index 0x13
	;-------
	ldrb	r4, [r2, #(VGAOffset-VGAStartAddrHigh)]
	lsl		r4, #4
	sub		r11, r4, #320
	;-------
	; Calculate Line Compare value
	;-------
	ldrb	r5, [r2, #(VGAMaxScanLine-VGAStartAddrHigh)]			; r5 = VGA Max Scanline Register value
	ldrb	r6, [r2, #(VGA_CRTC_data_3D5+0x07-VGAStartAddrHigh)]	; r6 = VGA Overflow Register value
	ldrb	r7, [r2, #(VGA_CRTC_data_3D5+0x18-VGAStartAddrHigh)]	; r7 = VGA Line Compare Register value
	tst		r6, #0x10				; Line Compare bit 8 on?
	orrne	r7, #0x100				; Yep, update Line Compare Register result value
	tst		r5, #0x40				; Line Compare bit 9 on?
	orrne	r7, #0x200				; Yep, update Line Compare Register result value
	mov		lr, r7, lsl #(10+1)		; lr = 1024*2 * Line Compare Value
	;-------
	; Calculate the input start position
	;-------
	ldrb	r0, [r2]				; VGAStartAddrHigh
	ldrb	r3, [r2, #1]			; VGAStartAddrLo
	add		r12, r1, #0x40000		; r12 = End address of the logical EGA/VGA VRAM
	orr		r0, r3, r0, lsl #8
	add		r1, r0, lsl #2
	ldrb	r3, [r2, #(VGAPixelPanning-VGAStartAddrHigh)]
	mov		r0, r10					; Physical VRAM start
	and		r10, r3, #7				; Crystal Caves sets PixelPanning to 0x10!
	lsl		r10, #2					; Ech pixel panning step shifts the values by 4 pixels
	lsr		lr, #1					; Adjust the Line Compare value for double-scanning
	add		lr, r0					; lr = Address where we need to rewind back to the EGAVGA_A000 address
	add		r2, r0, #(SCREEN_WIDTH*200)*2	; r2 = output ending address
	;-------
	; Loop here to do 320x200 pixels
	; Registers
	;	r0 = Output VRAM address (s_pixels input parameter)
	;	r1 = Emulated EGA/VGA RAM address (EGAVGA_A000+StartAddress...)
	;	r2 = End position in physical s_pixels output area
	;	r3 = BG_PALETTE lookup table address
	;	r4-r9 = Scratch registers
	;	r10 = Loop counter (highest byte) | pixel panning value (lowest byte)
	;	r11 = display pitch value
	;	r12 = End address of the EGAVGA_A000 area (EGAVGA_A000+0x40000)
	;	lr = Line compare address
	;-------
5
	ldr		r3, =BG_PALETTE			; Palette lookup table address
	orr		r10, #39<<24			; Copy 40*8 = 320 pixels per row

51	ldr		r4, [r1], #4			; Get 4 bytes (8 pixels) from input

	tst		r10, #0xFF				; Is pixel panning active?
	beq		%f52
	ldr		r5, [r1]
	mov		r4, r4, lsr r10
	rsb		r6, r10, #32
	mov		r5, r5, lsl r6
	orr		r4, r5
52
	and		r5, r4, #0xF
	ubfx	r7, r4, #4, #4
	ubfx	r6, r4, #8, #4
	ubfx	r8, r4, #12, #4
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsl #2]
	ldr		r6, [r3, r6, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16
	ubfx	r7, r4, #16, #4
	ubfx	r9, r4, #20, #4
	ubfx	r8, r4, #24, #4
	ubfx	r4, r4, #28, #4
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	ldr		r4, [r3, r4, lsl #2]
	orr		r7, r9, lsl #16
	orr		r8, r4, lsl #16

	cmp		r1, r12					; Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			; Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r8}			; Write 4 words = 16 bytes = 8 pixels	

	subs	r10, #1<<24				; Decrement the counter
	bge		%b51
	add		r10, #1<<24				; Make r10 positive again
	
	;-------
	; We are at an "interesting" location in target VRAM.
	; Adjust the pointer to the start of the next row.
	;-------
6	add		r0, #(SCREEN_WIDTH-320)*2	; Point r0 to the next row in physical VRAM
	cmp		r0, lr					; Are we at or over Line Compare line?
	bge		%f9						; Yep, handle split screen stuff
	add 	r1, r11, lsr #1			; Add vga.draw.address_add to go to the start of the next row
	cmp		r1, r12					; Did we go over the EGA/VGA A000 segment block?
	subge	r1, #0x40000			; Wrap back to start of the logical VRAM if we went over the end.
	;-------
	; All done?
	;-------
3	cmp		r0, r2					; All done?
    blt    	%b5						; Not yet, back to loop
	pop   	{r4-r12, pc}			; Return to caller
	;-------
	; We are at Line Compare address, continue copying from the start of the EGA_VGA_A000 area
	;-------
9	ldr		r5, =VGAModeControl
	ldr		r1, =EGAVGA_A000		; VGA screen start
	ldrb	r4, [r5]
	orr		lr, #0x0F000000			; No more Line Compare needed
	ldr		r1, [r1]
	;-------
	; If VGAModeControl register value & 0x20 is on, we must ignore the VGAPixelPanning
	; value after we have jumped back to the start of the VGA VRAM.
	;-------
	tst		r4, #0x20
	bicne 	r10, #0xFF
	b		%b3


	;-------
	; Copy a 640x(200/350/480)x16 VGA mode graphics screen to physical screen.
	; Each pixel consists of a bit in four different bit planes.
	;-------
	GLOBAL screen_copy_640
screen_copy_640
	push    {r4-r12, lr}
	ldr		r2,=VGAStartAddrHigh
	ldr		r1,=EGAVGA_A000			; VGA screen start
	mov		r10, r0					; r10 = input parameter value = s_pixels
	ldr		r1, [r1]
	;-------
	; Calculate the display pitch value (vga.draw.address_add in DOSBox)
	; from CRTC registers index 0x13
	;-------
	ldrb	r11, [r2, #(VGAOffset-VGAStartAddrHigh)]
	lsl		r11, #4
	sub		r11, #640
	;-------
	; Calculate Line Compare value
	;-------
	ldrb	r5, [r2, #(VGAMaxScanLine-VGAStartAddrHigh)]			; r5 = VGA Max Scanline Register value
	ldrb	r6, [r2, #(VGA_CRTC_data_3D5+0x07-VGAStartAddrHigh)]	; r6 = VGA Overflow Register value
	ldrb	r7, [r2, #(VGA_CRTC_data_3D5+0x18-VGAStartAddrHigh)]	; r7 = VGA Line Compare Register value
	tst		r6, #0x10				; Line Compare bit 8 on?
	orrne	r7, #0x100				; Yep, update Line Compare Register result value
	tst		r5, #0x40				; Line Compare bit 9 on?
	orrne	r7, #0x200				; Yep, update Line Compare Register result value
	mov		lr, r7, lsl #(10+1)		; lr = 1024*2 * Line Compare Value
	;-------
	; Calculate the input start position
	;-------
	ldrb	r0, [r2]				; VGAStartAddrHigh
	ldrb	r3, [r2, #1]			; VGAStartAddrLo
	add		r12, r1, #0x40000		; r12 = End address of the logical EGA/VGA VRAM
	orr		r0, r3, r0, lsl #8
	add		r1, r0, lsl #2
	ldrb	r3, [r2, #(VGAPixelPanning-VGAStartAddrHigh)]
	mov		r0, r10					; Physical VRAM start
	and		r10, r3, #7				; Crystal Caves sets PixelPanning to 0x10!
	lsl		r10, #2					; Ech pixel panning step shifts the values by 4 pixels
	;-------
	; Check the screen resolution we are to copy
	;-------
	ldr		r3, =VGA_misc_3C2		; Check the current VSync polarity
	ldr		r4, =VGAMaxScanLine		; Check whether we do line doubling
	ldrb	r3, [r3]				; r3 = VGA Misc Register value
	ldrb	r5, [r4]				; r5 = VGA Maximum Scan Line register value
	and		r3, #0xC0
	cmp		r3, #0xC0				; Is it 480/240 lines?
	movne	r8, #(SCREEN_WIDTH*200)*2	; Nope, copy 200 lines to VRAM
	moveq	r8, #(SCREEN_WIDTH*240)*2	; Yep, copy 240 lines to VRAM
	tst		r5, #1					; Character cell height = 0 (normal) or 1 (double)?
	lsleq	r8, #1					; Normal, copy 400 / 480 lines to VRAM
	add		r2, r0, r8				; r2 = VRAM output ending address
	lsrne	lr, #1					; Adjust the Line Compare value for double-scanning
	add		lr, r0					; lr = Address where we need to rewind back to the EGAVGA_A000 address
	;-------
	; Loop here to do 640x??? pixels
	; Registers
	;	r0 = Output VRAM address (s_pixels input parameter)
	;	r1 = Emulated EGA/VGA RAM address (EGAVGA_A000+StartAddress...)
	;	r2 = End position in physical s_pixels output area
	;	r3 = BG_PALETTE lookup table address
	;	r4-r9 = Scratch registers
	;	r10 = Loop counter (highest byte) | pixel panning value (lowest byte)
	;	r11 = display pitch value
	;	r12 = End address of the EGAVGA_A000 area (EGAVGA_A000+0x40000)
	;	lr = Line compare address
	;-------
5
	ldr		r3, =BG_PALETTE			; Palette lookup table address
	orr		r10, #79<<24			; Copy 80*8 = 640 pixels per row

51	ldr		r4, [r1], #4			; Get 4 bytes (8 pixels) from input

;	tst		r10, #0xFF				; Is pixel panning active?
;	ldrne	r5, [r1]
;	movne	r4, r4, lsr r10
;	rsbne	r6, r10, #32
;	orrne	r4, r5, lsl r6

	and		r5, r4, #0xF
	ubfx	r7, r4, #4, #4
	ubfx	r6, r4, #8, #4
	ubfx	r8, r4, #12, #4
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsl #2]
	ldr		r6, [r3, r6, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16
	ubfx	r7, r4, #16, #4
	ubfx	r9, r4, #20, #4
	ubfx	r8, r4, #24, #4
	ubfx	r4, r4, #28, #4
	ldr		r7, [r3, r7, lsl #2]
	ldr		r9, [r3, r9, lsl #2]
	ldr		r8, [r3, r8, lsl #2]
	ldr		r4, [r3, r4, lsl #2]
	orr		r7, r9, lsl #16
	orr		r8, r4, lsl #16

	cmp		r1, r12					; Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			; Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r8}			; Write 4 words = 16 bytes = 8 pixels	
	subs	r10, #1<<24				; Decrement the counter
	bge		%b51
	add		r10, #1<<24				; Make r10 positive again
	
	;-------
	; We are at an "interesting" location in target VRAM.
	; Adjust the pointer to the start of the next row.
	;-------
6	add		r0, #(SCREEN_WIDTH-640)*2	; Point r0 to the next row in physical VRAM
	cmp		r0, lr					; Are we at or over Line Compare line?
	bge		%f9						; Yep, handle split screen stuff
	add 	r1, r11, lsr #1			; Add vga.draw.address_add to go to the start of the next row
	cmp		r1, r12					; Did we go over the EGA/VGA A000 segment block?
	subge	r1, #0x40000			; Wrap back to start of the logical VRAM if we went over the end.
	;-------
	; All done?
	;-------
3	cmp		r0, r2					; All done?
    blt    	%b5						; Not yet, back to loop
	pop   	{r4-r12, pc}			; Return to caller
	;-------
	; We are at Line Compare address, continue copying from the start of the EGA_VGA_A000 area
	;-------
9	ldr		r5, =VGAModeControl
	ldr		r1, =EGAVGA_A000		; VGA screen start
	ldrb	r4, [r5]
	orr		lr, #0x0F000000			; No more Line Compare needed
	ldr		r1, [r1]
	;-------
	; If VGAModeControl register value & 0x20 is on, we must ignore the VGAPixelPanning
	; value after we have jumped back to the start of the VGA VRAM.
	;-------
	tst		r4, #0x20
	bicne 	r10, #0xFF
	b		%b3

	

	LTORG

; ------------------- DATA VALUES -------------------------------------
;

	AREA EGA_DATA, DATA, READWRITE
	ALIGN	4

	GLOBAL	VGA_serialize_start	; For serialization!
VGA_serialize_start

	GLOBAL	VGA_draw_address
VGA_draw_address
	DCD	0
VGA_debug_addr
	DCD	0
VGA_latch_minus_12				; Used in 4-byte read-modify-write operations
	DCD	0
VGA_latch_minus_8				; Used in 4-byte read-modify-write operations
	DCD	0
VGA_latch_minus_4				; Used in 2-byte read-modify-write operations
	DCD	0
VGA_latch
	DCD	0
VGA_palette_read
	DCD	0
VGA_palette_write
	DCD	0
	GLOBAL	EGA_READ_MASK32
EGA_READ_MASK32
	DCD	0xFFFFFFFF
	GLOBAL	EGA_WRITE_MASK32
EGA_WRITE_MASK32
	DCD	0xFFFFFFFF
EGA_BIT_MASK32
	DCD	0xFFFFFFFF
EGA_COLOR_COMPARE32
	DCD	0
EGA_COLOR_DONT_CARE32
	DCD	0
EGA_SET_RESET32
	DCD	0
EGA_ENABLE_SET_RESET32
	DCD	0
	GLOBAL	MODEX_WRITE_MASK32
MODEX_WRITE_MASK32
	DCD	0xFFFFFFFF

	GLOBAL	VGA_pel_mask_3C6
VGA_pel_mask_3C6				; VGA PEL Mask Register
	DCB	0xFF
	
	GLOBAL	VGA_misc_3C2
VGA_misc_3C2					; VGA Misc Register, 3C2 = write, 3CC = read
	DCB	1
VGA_palette_3C7				; DAC State Register: 0 = read is in effect, 3 = write is in effect.
	DCB	0
	GLOBAL	VGA_attr_addr_3C0
VGA_attr_addr_3C0
	DCB	0					; Address/flipflop, -1 if the next write is to address, else 0..0x14
	GLOBAL	VGA_attr_data_3C0
VGA_attr_data_3C0
	DCB	0					; 0 = EGA palette #0
	DCB	0					; 1 = EGA palette #1
	DCB	0					; 2 = EGA palette #2
	DCB	0					; 3 = EGA palette #3
	DCB	0					; 4 = EGA palette #4
	DCB	0					; 5 = EGA palette #5
	DCB	0					; 6 = EGA palette #6
	DCB	0					; 7 = EGA palette #7
	DCB	0					; 8 = EGA palette #8
	DCB	0					; 9 = EGA palette #9
	DCB	0					; A = EGA palette #A
	DCB	0					; B = EGA palette #B
	DCB	0					; C = EGA palette #C
	DCB	0					; D = EGA palette #D
	DCB	0					; E = EGA palette #E
	DCB	0					; F = EGA palette #F
VGAModeControl	
	DCB	0					; 0x10 = Mode Control
	DCB	0					; 0x11 = Overscan Color
	DCB	0					; 0x12 = Color Plane Enable
	GLOBAL	VGAPixelPanning
VGAPixelPanning
	DCB	0					; 0x13 = Horizontal Pixel Panning
	DCB	0					; 0x14 = Color Select (VGA only)

VGA_dac_combine
	DCB	0					; 0 = EGA palette #0
	DCB	0					; 1 = EGA palette #1
	DCB	0					; 2 = EGA palette #2
	DCB	0					; 3 = EGA palette #3
	DCB	0					; 4 = EGA palette #4
	DCB	0					; 5 = EGA palette #5
	DCB	0					; 6 = EGA palette #6
	DCB	0					; 7 = EGA palette #7
	DCB	0					; 8 = EGA palette #8
	DCB	0					; 9 = EGA palette #9
	DCB	0					; A = EGA palette #A
	DCB	0					; B = EGA palette #B
	DCB	0					; C = EGA palette #C
	DCB	0					; D = EGA palette #D
	DCB	0					; E = EGA palette #E
	DCB	0					; F = EGA palette #F
	
VGA_Sequencer_addr_3C4
	DCB	0
	GLOBAL	VGA_Sequencer_regs_3C5
VGA_Sequencer_regs_3C5
	DCB	3					; 0 = Reset Register, bit 0 = Asynchronous Reset, bit 1 = Synchronous Reset
	DCB	0x0E				; 1 = Clocking Mode Register
	GLOBAL VGAMapMask
VGAMapMask	
	DCB	0xF					; 2 = Map Mask Register
	DCB	0					; 3 = Character Map Select Register
	GLOBAL	VGAMemoryMode
VGAMemoryMode	
	DCB	0x0C				; 4 = Memory Mode Register, bit 3 = Chain4, bit 2 = Odd/Even, bit 1 = Extended memory, bit 0 = Alpha/Graphics
	
VGA_Graphics_addr_3CE
	DCB	0
	GLOBAL	VGA_Graphics_regs_3CF
VGA_Graphics_regs_3CF
	DCB	0					; 0 = Set/Reset Register, bits 0..3
	DCB	0					; 1 = Enable Set/Reset, bits 0..3
	DCB	0					; 2 = Color Compare, bits 0..3
VGAFuncReg
	DCB	0					; 3 = Data Rotate, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	GLOBAL	VGAReadMap
VGAReadMap	
	DCB	0					; 4 = Read Map Select, bits 0..1
	GLOBAL VGAModeReg
VGAModeReg
	DCB	0					; 5 = Mode Register, bits 6..5 = Shift Register, bit 4 = Odd/Even, bit 3 = Read Mode, bit 2 = 0, bits 1..0 = Write Mode
	DCB	0					; 6 = Misc Register, bits 3..2 = Memory Map, bit 1 = Chain Odd/Even, bit 0 = Graphics / Alpha mode
	DCB	0					; 7 = Color Dont Care register, bits 0..3
	GLOBAL VGABitMask
VGABitMask
	DCB	0xFF				; 8 = Bit Mask Register, bits 0..7 (= which bits are modified during write)

VGA_CRTC_addr_3D4
	DCB	0
	GLOBAL	VGA_CRTC_data_3D5
VGA_CRTC_data_3D5
	DCB	0					; 0 = Horizontal Total
	DCB	0					; 1 = Horizontal Display End
	DCB	0					; 2 = Start Horizontal Blank
	DCB	0					; 3 = End Horizontal Blank
	DCB	0					; 4 = Start Horizontal Retrace
	DCB	0					; 5 = End Horizontal Retrace
	DCB	0					; 6 = Vertical Total
	DCB	0					; 7 = Overflow
	DCB	0					; 8 = Preset Row Scan
	GLOBAL	VGAMaxScanLine
VGAMaxScanLine	
	DCB	0					; 9 = Max Scan Line
	DCB	0					; A = Cursor Start
	DCB	0					; B = Cursor End
	GLOBAL VGAStartAddrHigh
VGAStartAddrHigh
	DCB	0					; C = Start Address High
	GLOBAL VGAStartAddrLow
VGAStartAddrLow
	DCB	0					; D = Start Address Low
	DCB	0					; E = Cursor Location High
	DCB	0					; F = Cursor Location Low
	DCB	0					; 10 = Vertical Retrace Start
	DCB	0					; 11 = Vertical Retrace Low
	DCB	0					; 12 = Vertical Display End
	GLOBAL VGAOffset
VGAOffset	
	DCB	0x14				; 13 = Offset
	DCB	0					; 14 = Underline Location
	DCB	0					; 15 = Start Vertical Blank
	DCB	0					; 16 = End Vertical Blank
	DCB	0					; 17 = Mode Control
	DCB	0					; 18 = Line Compare

	ALIGN	4

	GLOBAL	VGA_serialize_end	; Needs to be last VGA data, for serialization!
VGA_serialize_end
	; This is for Mode-X Write Mask calculation
ModeXMaskTbl
	DCD	0x00000000
	DCD	0x000000FF
	DCD	0x0000FF00
	DCD	0x0000FFFF
	DCD	0x00FF0000
	DCD	0x00FF00FF
	DCD	0x00FFFF00
	DCD	0x00FFFFFF
	DCD	0xFF000000
	DCD	0xFF0000FF
	DCD	0xFF00FF00
	DCD	0xFF00FFFF
	DCD	0xFFFF0000
	DCD	0xFFFF00FF
	DCD	0xFFFFFF00
	DCD	0xFFFFFFFF

	MACRO
	b2w1 $val
	DCD	(($val/128)*0xF)+((($val/64):AND:1)*0xF0)+((($val/32):AND:1)*0xF00)+((($val/16):AND:1)*0xF000)+((($val/8):AND:1)*0xF0000)+((($val/4):AND:1)*0xF00000)+((($val/2):AND:1)*0xF000000)+((($val):AND:1)*0xF0000000)
	;.print 	"(($val>>7)*0xF)|((($val>>3):AND:1)*0xF0)|((($val>>6):AND:1)*0xF00)|((($val>>2):AND:1)*0xF000)|((($val>>5):AND:1)*0xF0000)|((($val>>1):AND:1)*0xF00000)|((($val>>4):AND:1)*0xF000000)|((($val):AND:1)*0xF0000000)"
	MEND
	MACRO 
	b2w8 $val
	b2w1 $val
	b2w1 ($val+1)
	b2w1 ($val+2)
	b2w1 ($val+3)
	b2w1 ($val+4)
	b2w1 ($val+5)
	b2w1 ($val+6)
	b2w1 ($val+7)
	MEND

byte2wordtbl
	b2w8	0x00
	b2w8	0x08
	b2w8	0x10
	b2w8	0x18
	b2w8	0x20
	b2w8	0x28
	b2w8	0x30
	b2w8	0x38
	b2w8	0x40
	b2w8	0x48
	b2w8	0x50
	b2w8	0x58
	b2w8	0x60
	b2w8	0x68
	b2w8	0x70
	b2w8	0x78
	b2w8	0x80
	b2w8	0x88
	b2w8	0x90
	b2w8	0x98
	b2w8	0xA0
	b2w8	0xA8
	b2w8	0xB0
	b2w8	0xB8
	b2w8	0xC0
	b2w8	0xC8
	b2w8	0xD0
	b2w8	0xD8
	b2w8	0xE0
	b2w8	0xE8
	b2w8	0xF0
	b2w8	0xF8

cursor2spritetbl
	; For AND mask (screen)
	DCD	0x02020202
	DCD	0x00020202
	DCD	0x02000202
	DCD	0x00000202
	DCD	0x02020002
	DCD	0x00020002
	DCD	0x02000002
	DCD	0x00000002
	DCD	0x02020200
	DCD	0x00020200
	DCD	0x02000200
	DCD	0x00000200
	DCD	0x02020000
	DCD	0x00020000
	DCD	0x02000000
	DCD	0x00000000
	; For XOR mask (cursor)
	DCD	0
	DCD	0x01000000
	DCD	0x00010000
	DCD	0x01010000
	DCD	0x00000100
	DCD	0x01000100
	DCD	0x00010100
	DCD	0x01010100
	DCD	0x00000001
	DCD	0x01000001
	DCD	0x00010001
	DCD	0x01010001
	DCD	0x00000101
	DCD	0x01000101
	DCD	0x00010101
	DCD	0x01010101

	;-------
	; Table for mapping EGA palette register values to real palette values, in RGBI Color mode
	;	bit5 = ?
	;	bit4 = Intensity
	;	bit3 = ?
	;	bit2 = Red
	;	bit1 = Green
	;	bit0 = Blue
	;-------
	MACRO
	RGB15 $r, $g, $b
		DCD	((($r)*2048)+(($g)*64)+($b))
	MEND

	GLOBAL	EGAPaletteTable
EGAPaletteTable																								  ; Brown adjusted!
	DCD	0
	RGB15 0,0,21
	RGB15 0,21,0
	RGB15 0,21,21
	RGB15 21,0,0
	RGB15 21,0,21
	RGB15 21,12,0
	RGB15 21,21,21
	DCD	0
	RGB15 0,0,21
	RGB15 0,21,0
	RGB15 0,21,21
	RGB15 21,0,0
	RGB15 21,0,21
	RGB15 21,12,0
	RGB15 21,21,21
	; 0x10
	RGB15 12,12,12
	RGB15 12,12,31
	RGB15 12,31,12
	RGB15 12,31,31
	RGB15 31,12,12
	RGB15 31,12,31
	RGB15 31,31,12
	RGB15 31,31,31
	RGB15 12,12,12
	RGB15 12,12,31
	RGB15 12,31,12
	RGB15 12,31,31
	RGB15 31,12,12
	RGB15 31,12,31
	RGB15 31,31,12
	RGB15 31,31,31
	; 0x20
	DCD	0
	RGB15 0,0,21
	RGB15 0,21,0
	RGB15 0,21,21
	RGB15 21,0,0
	RGB15 21,0,21
	RGB15 21,21,0
	RGB15 21,21,21
	DCD	0
	RGB15 0,0,21
	RGB15 0,21,0
	RGB15 0,21,21
	RGB15 21,0,0
	RGB15 21,0,21
	RGB15 21,21,0
	RGB15 21,21,21
	; 0x30
	RGB15 12,12,12
	RGB15 12,12,31
	RGB15 12,31,12
	RGB15 12,31,31
	RGB15 31,12,12
	RGB15 31,12,31
	RGB15 31,31,12
	RGB15 31,31,31
	RGB15 12,12,12
	RGB15 12,12,31
	RGB15 12,31,12
	RGB15 12,31,31
	RGB15 31,12,12
	RGB15 31,12,31
	RGB15 31,31,12
	RGB15 31,31,31

	GLOBAL	VideoParameterTable
VideoParameterTable
	; video parameter table for mode 00h = T  40x25  8x8   320x200  16gray    8   B800 CGA,PCjr,Tandy
    DCB   40, 24, 8, 0x00, 0x08 ; bios data (columns, rows, pixels/char, page length)
    DCB   0x08, 0x03, 0x00, 0x07 ; sequencer registers
    DCB   0x67 ; misc output registers
    DCB   0x2d, 0x27, 0x28, 0x90, 0x2b, 0xa0, 0xbf, 0x1f       ; crtc registers 0-7
    DCB   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
    DCB   0x9c, 0x8e, 0x8f, 0x14, 0x1f, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
    DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
    DCB   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
    DCB   0x0c, 0x00, 0x0f, 0x08 ; attr registers 16-19
    DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff ; graphics registers 0-8
	; video parameter table for mode 01h = T  40x25  8x8   320x200   16       8   B800 CGA,PCjr,Tandy
	DCB   40, 24, 8, 0x00, 0x08 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x08, 0x03, 0x00, 0x07 ; sequencer registers
	DCB   0x67 ; misc output registers
	DCB   0x2d, 0x27, 0x28, 0x90, 0x2b, 0xa0, 0xbf, 0x1f       ; crtc registers 0-7
	DCB   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0x9c, 0x8e, 0x8f, 0x14, 0x1f, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
	DCB   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
	DCB   0x0c, 0x00, 0x0f, 0x08 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 02h = T  80x25  8x8   640x200  16gray    4   B800 CGA,PCjr,Tandy
	DCB   80, 24, 8, 0x00, 0x10 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x00, 0x03, 0x00, 0x07 ; sequencer registers
	DCB   0x67 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       ; crtc registers 0-7
	DCB   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0x9c, 0x8e, 0x8f, 0x28, 0x1f, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
	DCB   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
	DCB   0x0c, 0x00, 0x0f, 0x08 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 03h = T  80x25  8x8   640x200   16       4   B800 CGA,PCjr,Tandy
	DCB	80, 24, 8, 0x00, 0x10 ; bios data (columns, rows, pixels/char, page length)
	DCB	0x00, 0x03, 0x00, 0x07 ; sequencer registers
	DCB	0x67 ; misc output registers
	DCB	0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       ; crtc registers 0-7
	DCB	0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB	0x9c, 0x8e, 0x8f, 0x28, 0x1f, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
	DCB	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
	DCB	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
	DCB	0x0c, 0x00, 0x0f, 0x08 ; attr registers 16-19
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 04h = G  40x25  8x8   320x200    4       .   B800 CGA,PCjr,EGA,MCGA,VGA
	DCB	40, 24, 8, 0x00, 0x40 ; bios data (columns, rows, pixels/char, page length)
	DCB	0x09, 0x00, 0x00, 0x02 ; sequencer registers
	DCB	0x63 ; misc output registers
	DCB	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
	DCB	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xa2, 0xff ; crtc registers 16-24
	DCB	0x00, 0x13, 0x15, 0x17, 0x02, 0x04, 0x06, 0x07       ; attr registers 0-7
	DCB	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       ; attr registers 8-15
	DCB	0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x0f, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 05h = G  40x25  8x8   320x200   4gray    .   B800 CGA,PCjr,EGA
	DCB	40, 24, 0x08, 0x00, 0x40 ; bios data
	DCB	0x09, 0x00, 0x00, 0x02 ; sequencer registers
	DCB	0x63 ; misc output registers
	DCB	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
	DCB	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
  	DCB	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xa2, 0xff ; crtc registers 16-24
  	DCB	0x00, 0x13, 0x15, 0x17, 0x02, 0x04, 0x06, 0x07       ; attr registers 0-7
  	DCB	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       ; attr registers 8-15
  	DCB	0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
  	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x0f, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 06h = G  80x25  8x8   640x200    2       .   B800 CGA,PCjr,EGA,MCGA,VGA
  	DCB	80, 24, 0x08, 0x00, 0x40 ; bios data
  	DCB	0x09, 0x0f, 0x00, 0x02 ; sequencer registers
  	DCB	0x63 ; misc output registers
  	DCB	0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
  	DCB	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
  	DCB	0x9c, 0x8e, 0x8f, 0x28, 0x00, 0x96, 0xb9, 0xc2, 0xff ; crtc registers 16-24
  	DCB	0x00, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17       ; attr registers 0-7
  	DCB	0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17       ; attr registers 8-15
  	DCB	0x01, 0x00, 0x01, 0x00 ; attr registers 16-19
  	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0f, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 07h = T  80x25  9x14  720x350  mono     var  B000 MDA,Hercules,EGA
  	DCB	80, 24, 0x10, 0x00, 0x10 ; bios data
  	DCB	0x00, 0x0f, 0x00, 0x07 ; sequencer registers
  	DCB	0x66 ; misc output registers
  	DCB	0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       ; crtc registers 0-7
  	DCB	0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
  	DCB	0x9c, 0x8e, 0x8f, 0x28, 0x0f, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
  	DCB	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
  	DCB	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
  	DCB	0x0c, 0x00, 0x0f, 0x08 ; attr registers 16-19
  	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0a, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 8
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
; video parameter table for mode 9
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
; video parameter table for mode a
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
; video parameter table for mode b
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
; video parameter table for mode c
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
; video parameter table for mode 0Dh = G  40x25  8x8   320x200   16       8   A000 EGA,VGA
  	DCB	40, 24, 8, 0x00, 0x20 ; bios data
  	DCB	0x09, 0x0f, 0x00, 0x02 ; sequencer registers
  	DCB	0x63 ; misc output registers
  	DCB	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
  	DCB	0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
  	DCB	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xe3, 0xff ; crtc registers 16-24
  	DCB	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       ; attr registers 0-7
  	DCB	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       ; attr registers 8-15
  	DCB	0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
  	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 0Eh = G  80x25  8x8   640x200   16       4   A000 EGA,VGA
  	DCB	80, 24, 8, 0x00, 0x40 ; bios data
  	DCB	0x01, 0x0f, 0x00, 0x02 ; sequencer registers
  	DCB	0x63 ; misc output registers
  	DCB	0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
  	DCB	0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
  	DCB	0x9c, 0x8e, 0x8f, 0x28, 0x00, 0x96, 0xb9, 0xe3, 0xff ; crtc registers 16-24
  	DCB	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       ; attr registers 0-7
  	DCB	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       ; attr registers 8-15
  	DCB	0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
  	DCB	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 0Fh = G  80x25  8x14  640x350  mono      2   A000 EGA,VGA
	DCB   80, 24, 14, 0x00, 0x80 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x01, 0x0f, 0x00, 0x02 ; sequencer registers
	DCB   0xa2 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
	DCB   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0x83, 0x85, 0x5d, 0x28, 0x0f, 0x63, 0xba, 0xe3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x08, 0x00, 0x00, 0x18, 0x18, 0x00, 0x00       ; attr registers 0-7
	DCB   0x00, 0x08, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00       ; attr registers 8-15
	DCB   0x0b, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8  	
; video parameter table for mode 10 = G  80x25  8x14  640x350  16color    2   A000 EGA,VGA
	DCB   80, 24, 14, 0x00, 0x80 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x01, 0x0f, 0x00, 0x02 ; sequencer registers
	DCB   0xa3 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
	DCB   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0x83, 0x85, 0x5d, 0x28, 0x0f, 0x63, 0xba, 0xe3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
	DCB   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
	DCB   0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 11h = G  80x30  8x16  640x480  mono      .   A000 VGA,MCGA,ATI EGA,ATI VIP
	DCB   80, 29, 16, 0x00, 0xa0 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x01, 0x0f, 0x00, 0x02 ; sequencer registers
	DCB   0xe3 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0x0b, 0x3e       ; crtc registers 0-7
	DCB   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0xea, 0x8c, 0xdf, 0x28, 0x00, 0xe7, 0x04, 0xc3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f       ; attr registers 0-7
	DCB   0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f       ; attr registers 8-15
	DCB   0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 12h = G  80x30  8x16  640x480   16/256K  .   A000 VGA,ATI VIP
	DCB   80, 29, 16, 0x00, 0xa0 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x01, 0x0f, 0x00, 0x02 ; sequencer registers
	DCB   0xe3 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0x0b, 0x3e       ; crtc registers 0-7
	DCB   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0xea, 0x8c, 0xdf, 0x28, 0x00, 0xe7, 0x04, 0xe3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       ; attr registers 0-7
	DCB   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       ; attr registers 8-15
	DCB   0x01, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff ; graphics registers 0-8
; video parameter table for mode 13h = G  40x25  8x8   320x200  256/256K  .   A000 VGA,MCGA,ATI VIP
	DCB   40, 24, 8, 0x00, 0x20 ; bios data (columns, rows, pixels/char, page length)
	DCB   0x01, 0x0f, 0x00, 0x0e ; sequencer registers
	DCB   0x63 ; misc output registers
	DCB   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       ; crtc registers 0-7
	DCB   0x00, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ; crtc registers 8-15
	DCB   0x9c, 0x8e, 0x8f, 0x28, 0x40, 0x96, 0xb9, 0xa3, 0xff ; crtc registers 16-24
	DCB   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       ; attr registers 0-7
	DCB   0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f       ; attr registers 8-15
	DCB   0x41, 0x00, 0x0f, 0x00 ; attr registers 16-19
	DCB   0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x05, 0x0f, 0xff ; graphics registers 0-8

	ALIGN	4
	
	GLOBAL	BG_PALETTE
BG_PALETTE						; Palette used in all other but EGA modes.
	SPACE	4*256
	GLOBAL	EGA_PALETTE_SAVE
EGA_PALETTE_SAVE				; Palette used when in EGA graphics mode. Values copied to BG_PALETTE in VGA_DAC_SendColor
	SPACE	4*256

	END
