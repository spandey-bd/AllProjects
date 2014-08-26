//=============================================================================
// EGA.S
//
// This file contains routines to handle EGA graphics (all 16-color graphics
// modes, including 640x480x16 VGA mode). Since the EGA memory is not linear
// but consists of four planes, this file contains separate EGA VRAM read/write
// opcode handlers that map between the x86 EGA memory order and linear VRAM
// that pax86 uses to speed up screen memory blitting. Each written byte
// (8 pixels) is expanded to a word (8 * 4 bits), and vice versa.
//
// This file also contains the screen blitting routines for all 16-color modes,
// EGA/VGA I/O port handlers, and video parameter tables.
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
	.file	"EGA.s"

	.global	op_20_EGA_r0r2
	.global	op_21_EGA_r0r2
	.global	op_31_EGA_r0r2
	.global	op_80_EGA_r0r2
	.global	EGA_opcode_jump
	.global	movsb_cld_from_EGA
	.global	movsb_cld_to_EGA
	.global	movsb_cld_RAM_EGA
	.global	movsb_std_RAM_EGA
	.global	movsb_cld_EGA_RAM
	.global	movsb_cld_EGA_EGA
	.global	movsb_std_EGA_EGA
	.global	movsw_cld_RAM_EGA
	.global	movsw_cld_EGA_RAM
	.global	movsw_cld_EGA_EGA
	.global	CharToEGAScreen
	.global	PixelToEGAScreen
	.global	PixelFromEGAScreen

	.global	out_3C0_VGA_attr
	.global	in_3C0_VGA_attr
	.global	in_3C1_VGA_attr
	.global	out_3C2_VGA_misc
	.global	in_3CC_VGA_misc
	.global	out_3C4_VGA_sequencer
	.global	in_3C4_VGA_sequencer
	.global	out_3C5_VGA_sequencer
	.global	in_3C5_VGA_sequencer
	
	.global	out_3C7_VGA_pel
	.global	in_3C7_VGA_DAC
	.global	out_3C8_VGA_pel
	.global	in_3C8_VGA_pel
	.global	out_3C9_VGA_pel
	.global	in_3C9_VGA_pel
	.global	out_3C9_EGA_pel
	.global	out_3CE_VGA_graphics
	.global	in_3CE_VGA_graphics
	.global	out_3CF_VGA_graphics
	.global	in_3CF_VGA_graphics
	.global	out_3D4_VGA_CRTC_addr
	.global	in_3D4_VGA_CRTC_addr
	.global	out_3D5_VGA_CRTC_data
	.global	in_3D5_VGA_CRTC_data

//
// EGA memory handling:
// The four planes are interlaced so that each input byte corresponds to a word in EGAVGA_A000.
// Each bit of the input is a 4-bit nibble in the output word. The Map Mask register (EGA_MASK32)
// contains a mask telling which bit in each of these nibbles the written byte affects.
//
//

	.text
	.align	2
	.code	32
	.arm

#include "defines.inc"
#include "macros.inc"

#define REG_DEBUG	0

	.global	bt_16_EGA_r2
	.global	bts_16_EGA_r2
	.global	btr_16_EGA_r2
	.global	btc_16_EGA_r2
	.global	op_89_EGA_USE32_r4
	.global	op_89_EGA_USE32_r5
	.global	op_89_EGA_USE32_r6
	.global	op_89_EGA_USE32_r7
	.global	op_89_EGA_USE32_r8
	.global	op_89_EGA_USE32_r9
	.global	op_89_EGA_USE32_r10
	.global	op_89_EGA_USE32_r11
	.global	op_8b_EGA_USE32_r4
	.global	op_8b_EGA_USE32_r5
	.global	op_8b_EGA_USE32_r6
	.global	op_8b_EGA_USE32_r7
	.global	op_8b_EGA_USE32_r8
	.global	op_8b_EGA_USE32_r9
	.global	op_8b_EGA_USE32_r10
	.global	op_8b_EGA_USE32_r11
	.global	op_c7_EGA_USE32_r2
	.global	test_EGA_r2_imm32
	.global	dec_word_EGA

bt_16_EGA_r2:
bts_16_EGA_r2:
btr_16_EGA_r2:
btc_16_EGA_r2:
op_89_EGA_USE32_r4:
op_89_EGA_USE32_r5:
op_89_EGA_USE32_r6:
op_89_EGA_USE32_r7:
op_89_EGA_USE32_r8:
op_89_EGA_USE32_r9:
op_89_EGA_USE32_r10:
op_89_EGA_USE32_r11:
op_8b_EGA_USE32_r4:
op_8b_EGA_USE32_r5:
op_8b_EGA_USE32_r6:
op_8b_EGA_USE32_r7:
op_8b_EGA_USE32_r8:
op_8b_EGA_USE32_r9:
op_8b_EGA_USE32_r10:
op_8b_EGA_USE32_r11:
op_c7_EGA_USE32_r2:
test_EGA_r2_imm32:
dec_word_EGA:
	b		.unknown

// ------------------- Common routines ---------------------------------
//

	//-------
	//	Bitu readHandler(PhysPt start) {
	//		vga.latch.d=((Bit32u*)vga.mem.linear)[start];
	//		switch (vga.config.read_mode) {
	//		case 0:
	//			return (vga.latch.b[vga.config.read_map_select]);
	//		case 1:
	//			VGA_Latch templatch;
	//			templatch.d=(vga.latch.d &	FillTable[vga.config.color_dont_care]) ^ FillTable[vga.config.color_compare & vga.config.color_dont_care];
	//			return (Bit8u)~(templatch.b[0] | templatch.b[1] | templatch.b[2] | templatch.b[3]);
	//		}
	//		return 0;
	//	}
	//-------
EGA_read_byte_r2:
	//-------
	// On input:
	//	r2 = Segment+Offset address into emulated EGA VRAM
	//	lr = return address
	// Destroys:
	//	r1, r2, flags
	// Output:
	//	r0 = byte value read from EGA
	//-------
	ldr		r1,=VGA_latch
	ldr		r0, [r2] 							// Get the value from our emulated EGA/VGA RAM
#ifdef IOS
	ldr		r2, =VGAModeReg
	ldrb	r2, [r2]
#else
	ldrb	r2, [r1, #(VGAModeReg-VGA_latch)]	// Get Mode Register value
#endif
	str		r0, [r1]							// vga.latch.d = r0
	str		r0, [r1, #-4]						// vga.latch.d = r0
	tst		r2, #0x8							// Read Mode 1?
	bne		1f									// Yep, go handle it
	//=======
	// Read Mode 0, return 8 consecutive pixels from EGA VRAM.
	//=======
#ifdef IOS
	ldr		r2, =EGA_READ_MASK32
	ldr		r2, [r2]
#else
	ldr		r2, [r1, #(EGA_READ_MASK32-VGA_latch)]
#endif
	and		r1, r0, r2							// Leave only the pixels in r1 that exist in the plane we are interested in.
	//-------
	// Turn on the needed pixels in AL
	//-------
	mov		r0, #0								// Init AL = 0
	tst		r1, #0xF							// Pixel 0 set?
	orrne	r0, #0x80
	tst		r1, #0xF0							// Pixel 1 set?
	orrne	r0, #0x40
	tst		r1, #0xF00							// Pixel 2 set?
	orrne	r0, #0x20
	tst		r1, #0xF000							// Pixel 3 set?
	orrne	r0, #0x10
	tst		r1, #0xF0000						// Pixel 4 set?
	orrne	r0, #0x08
	tst		r1, #0xF00000						// Pixel 5 set?
	orrne	r0, #0x04
	tst		r1, #0xF000000						// Pixel 6 set?
	orrne	r0, #0x02
	tst		r1, #0xF0000000						// Pixel 7 set?
	orrne	r0, #0x01
	//-------
	// Return
	//-------
	bx		lr
	//=======
	// Read Mode 1, return the result of a color comparison.
	//=======
#ifdef IOS
1:	ldr		r2, =EGA_COLOR_COMPARE32
	ldr		r1, =EGA_COLOR_DONT_CARE32
	ldr		r2, [r2]
	ldr		r1, [r1]
#else
1:	ldr		r2, [r1, #(EGA_COLOR_COMPARE32-VGA_latch)]
	ldr		r1, [r1, #(EGA_COLOR_DONT_CARE32-VGA_latch)]
#endif
	and		r0, r1								// Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r2, r1								// Clear the "Dont Care" bits from the Color Compare value
	eor		r1, r2, r0							// Now r1 contains bits set in nibbles that fail the comparison
	//-------
	// Turn on the needed pixels in AL
	//-------
	mov		r0, #0								// Init AL = 0
	tst		r1, #0xF							// Pixel 0 set?
	orreq	r0, #0x80
	tst		r1, #0xF0							// Pixel 1 set?
	orreq	r0, #0x40
	tst		r1, #0xF00							// Pixel 2 set?
	orreq	r0, #0x20
	tst		r1, #0xF000							// Pixel 3 set?
	orreq	r0, #0x10
	tst		r1, #0xF0000						// Pixel 4 set?
	orreq	r0, #0x08
	tst		r1, #0xF00000						// Pixel 5 set?
	orreq	r0, #0x04
	tst		r1, #0xF000000						// Pixel 6 set?
	orreq	r0, #0x02
	tst		r1, #0xF0000000						// Pixel 7 set?
	orreq	r0, #0x01
	//-------
	// Return
	//-------
	bx		lr


	//-------
	////Nice one from DosEmu
	//INLINE static Bit32u RasterOp(Bit32u input,Bit32u mask) {
	//	switch (vga.config.raster_op) {
	//	case 0x00:	/* None */
	//		return (input & mask) | (vga.latch.d & ~mask);
	//	case 0x01:	/* AND */
	//		return (input | ~mask) & vga.latch.d;
	//	case 0x02:	/* OR */
	//		return (input & mask) | vga.latch.d;
	//	case 0x03:	/* XOR */
	//		return (input & mask) ^ vga.latch.d;
	//	};
	//	return 0;
	//}
	//
	//INLINE static Bit32u ModeOperation(Bit8u val) {
	//	Bit32u full;
	//	switch (vga.config.write_mode) {
	//	case 0x00:
	//		// Write Mode 0: In this mode, the host data is first rotated as per the Rotate Count field,
	//		// then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	//		// Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//		// Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//		// Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//		full=ExpandTable[val];
	//		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	//		full=RasterOp(full,vga.config.full_bit_mask);
	//		break;
	//	case 0x01:
	//		// Write Mode 1: In this mode, data is transferred directly from the 32 bit latch register to display memory,
	//		// affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	//		full=vga.latch.d;
	//		break;
	//	case 0x02:
	//		// Write Mode 2: In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	//		// Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//		// Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//		// Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//		full=RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	//		break;
	//	case 0x03:
	//		// Write Mode 3: In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	//		// Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	//		// The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	//		// to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	//		// Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	//		full=RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	//		break;
	//	default:
	//		LOG(LOG_VGAMISC,LOG_NORMAL)("VGA:Unsupported write mode %d",vga.config.write_mode);
	//		full=0;
	//		break;
	//	}
	//	return full;
	//}
	//
	//		Bit32u data=ModeOperation(val);
	//		/* Update video memory and the pixel buffer */
	//		VGA_Latch pixels;
	//		pixels.d=((Bit32u*)vga.mem.linear)[start];
	//		pixels.d&=vga.config.full_not_map_mask;
	//		pixels.d|=(data & vga.config.full_map_mask);
	//		((Bit32u*)vga.mem.linear)[start]=pixels.d;
	//		Bit8u * write_pixels=&vga.fastmem[start<<3];
	//-------

	//=======
	// On input:
	//	r0 = byte to write (higher bits not used)
	//	r1 = free
	//	r2 = Full Segment+Offset address into EGA VRAM
	//	lr = return address
	// Return:
	//	r0 = 32-bit value written to [r2]
	// Destroys:
	//	r1, flags
	//=======
	.global	EGA_write_byte_from_C
EGA_write_byte_from_C:
	mov		r2, r1
	.global	EGA_write_byte_r0_to_r2
EGA_write_byte_r0_to_r2:
	ldr		r1, =byte2wordtbl
	push	{r3}
	push	{r2}								// Save the r2 register containing the output address
#ifdef IOS
	ldr		r2, =VGAModeReg
	ldrb	r2, [r2]
#else
	ldrb	r2, [r1, #(VGAModeReg-byte2wordtbl)]// Get Mode Register value
#endif
	ands	r2, #3								// r2 = Write Mode
	bne		.not_mode0							// Jump if Write Mode is not zero
	//=======
	// Write Mode 0
	//	In this mode, the host data is first rotated as per the Rotate Count field,
	//	then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	//	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//
	//		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//		full=ExpandTable[val];
	//		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	//		full=RasterOp(full,vga.config.full_bit_mask);
	//		break;
	//=======
	//-------
	// r0 = val = ((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldrb	r2, [r2]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
#endif
	and		r0, #0xFF
	and		r2, #7
	mov		r3, r0, lsr r2
	rsb		r2, #8
	orr		r3, r0, lsl r2
	and		r0, r3, #0xFF						// val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//-------
	// r0 = full = ExpandTable[val];
	//-------
	ldr		r0, [r1, r0, lsl #2]				// full=ExpandTable[val];
	//-------
	// r0 = full = (full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset;
	//-------
#ifdef IOS
	ldr		r2, =EGA_ENABLE_SET_RESET32
	ldr		r3, =EGA_SET_RESET32
	ldr		r2, [r2]
	ldr		r3, [r3]
#else
	ldr		r2, [r1, #(EGA_ENABLE_SET_RESET32-byte2wordtbl)]
	ldr		r3, [r1, #(EGA_SET_RESET32-byte2wordtbl)]
#endif
	bic		r0, r2
	and		r3, r2
	orr		r0, r3								// full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	//-------
	// r0 = full = RasterOp(full,vga.config.full_bit_mask);
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	ands	r2, #0x18
	bne		.not_raster_op_mov
.raster_op_mov:
	//-------
	// MOV: return (input & mask) | (vga.latch.d & ~mask);
	//-------
#ifdef IOS
	ldr		r2, =VGA_latch
	ldr		r2, [r2]
#else
	ldr		r2, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	and		r0, r3
	bic		r2, r3
	orr		r0, r2
	//-------
	// VGA_Latch pixels;
	// pixels.d=((Bit32u*)vga.mem.linear)[start];
	// pixels.d&=vga.config.full_not_map_mask;
	// pixels.d|=(data & vga.config.full_map_mask);
	//-------
.mode1_cont:
	pop		{r2}
#ifdef IOS
	ldr		r1, =EGA_WRITE_MASK32
	ldr		r1, [r1]
#else
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
#endif
	ldr		r3, [r2]
	and		r0, r1								// data &= vga.config.full_map_mask;
	bic		r3, r1								// pixels.d&=vga.config.full_not_map_mask;
	orr		r0, r3								// pixels.d|=(data & vga.config.full_map_mask);
	//-------
	// ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	//-------
	str		r0, [r2]
	pop		{r3}
	//-------
	// Return
	//-------
	bx		lr
	
	.global .not_raster_op_mov
.not_raster_op_mov:
	cmp		r2, #0x10
	//-------
	// AND: return (input | ~mask) & vga.latch.d;	(lt)
	// OR: return (input & mask) | vga.latch.d;		(eq)
	// XOR: return (input & mask) ^ vga.latch.d;		(gt)
	//-------
#ifdef IOS
	ldr		r2, =VGA_latch
	ldr		r2, [r2]
#else
	ldr		r2, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	mvnlt	r3, r3								// AND: (mask = ~mask)
	orrlt	r0, r3								// AND: (input | mask)
	andge	r0, r3								// OR/XOR: (input & mask)
	andlt	r0, r2								// AND
	orreq	r0, r2								// OR
	eorgt	r0, r2								// XOR
	//-------
	// VGA_Latch pixels;
	// pixels.d=((Bit32u*)vga.mem.linear)[start];
	// pixels.d&=vga.config.full_not_map_mask;
	// pixels.d|=(data & vga.config.full_map_mask);
	//-------
	pop		{r2}
#ifdef IOS
	ldr		r1, =EGA_WRITE_MASK32
	ldr		r1, [r1]
#else
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
#endif
	ldr		r3, [r2]
	and		r0, r1
	bic		r3, r1
	orr		r0, r3
	//-------
	// ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	//-------
	str		r0, [r2]
	pop		{r3}
	//-------
	// Return
	//-------
	bx		lr

	//=======
	// Write Mode > 0
	//=======
.not_mode0:
	cmp		r2, #2								// Write Mode <2 == 1, == 2, or >2 == 3?
	//=======
	// Write Mode 1
	//	In this mode, data is transferred directly from the 32 bit latch register to display memory,
	//	affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	// r0 = full = vga.latch.d;
	//=======
#ifdef IOS
	ldrlt	r0, =VGA_latch
	ldrlt	r0, [r0]
#else
	ldrlt	r0, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	blt		.mode1_cont
	bgt		.mode3
	//=======
	// Write Mode 2
	//	In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	//	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//=======
	//-------
	// r0 = full = RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	and		r0, #15
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	ands	r2, #0x18
	bne		.not_raster_op_mov
	b		.raster_op_mov

	//=======
	// Write Mode 3
	//	In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	//	Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	//	The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	//	to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	//=======
.mode3:	
	//-------
	// r0 = full = RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	//-------
	and		r0, #0xFF
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]		// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	ldr		r0, [r1, r0, lsl #2]
	and		r3, r0										// r3 = ExpandTable[val] & vga.config.full_bit_mask
#ifdef IOS
	ldr		r0, =EGA_SET_RESET32
	ldr		r0, [r0]
#else
	ldr		r0, [r1, #(EGA_SET_RESET32-byte2wordtbl)]	// r0 = vga.config.full_set_reset
#endif
	ands	r2, #0x18
	bne		.not_raster_op_mov
	b		.raster_op_mov


EGA_read_hword_r2:
	//-------
	// On input:
	//	r2 = Segment+Offset address into emulated EGA VRAM
	//	lr = return address
	// Destroys:
	//	r1, r2
	// Output:
	//	r0 = halfword value read from EGA
	//-------
	push	{r3}
	ldr		r1,=VGA_latch_minus_4
	ldmia	r2, {r0, r3}						// Get the value from our emulated EGA/VGA RAM
#ifdef IOS
	ldr		r2, =VGAModeReg
	ldrb	r2, [r2]							// Get Mode Register value
#else
	ldrb	r2, [r1, #(VGAModeReg-VGA_latch_minus_4)]	// Get Mode Register value
#endif
	stmia	r1, {r0, r3}						// vga.latch.d = r3 = the last read "byte"
	tst		r2, #0x8							// Read Mode 1?
	bne		1f									// Yep, go handle it
	//=======
	// Read Mode 0, return 16 consecutive pixels from EGA VRAM.
	//=======
#ifdef IOS
	ldr		r2, =EGA_READ_MASK32
	ldr		r2, [r2]
#else
	ldr		r2, [r1, #(EGA_READ_MASK32-VGA_latch_minus_4)]
#endif
	and		r1, r0, r2							// Leave only the pixels in r1 that exist in the plane we are interested in.
	and		r3, r2								// Leave only the pixels in r3 that exist in the plane we are interested in.
	//-------
	// Turn on the needed pixels in AX
	//-------
	mov		r0, #0
	tst		r1, #0xF				// Pixel 0 set?
	orrne	r0, #0x0080
	tst		r1, #0xF0				// Pixel 1 set?
	orrne	r0, #0x0040
	tst		r1, #0xF00				// Pixel 2 set?
	orrne	r0, #0x0020
	tst		r1, #0xF000				// Pixel 3 set?
	orrne	r0, #0x0010
	tst		r1, #0xF0000			// Pixel 4 set?
	orrne	r0, #0x0008
	tst		r1, #0xF00000			// Pixel 5 set?
	orrne	r0, #0x0004
	tst		r1, #0xF000000			// Pixel 6 set?
	orrne	r0, #0x0002
	tst		r1, #0xF0000000			// Pixel 7 set?
	orrne	r0, #0x0001
	tst		r3, #0xF				// Pixel 0 set?
	orrne	r0, #0x8000
	tst		r3, #0xF0				// Pixel 1 set?
	orrne	r0, #0x4000
	tst		r3, #0xF00				// Pixel 2 set?
	orrne	r0, #0x2000
	tst		r3, #0xF000				// Pixel 3 set?
	orrne	r0, #0x1000
	tst		r3, #0xF0000			// Pixel 4 set?
	orrne	r0, #0x0800
	tst		r3, #0xF00000			// Pixel 5 set?
	orrne	r0, #0x0400
	tst		r3, #0xF000000			// Pixel 6 set?
	orrne	r0, #0x0200
	tst		r3, #0xF0000000			// Pixel 7 set?
	orrne	r0, #0x0100
	//-------
	// Return
	//-------
	pop		{r3}
	bx		lr
	//=======
	// Read Mode 1, return the result of a color comparison.
	//=======
#ifdef IOS
1:	ldr		r2, =EGA_COLOR_COMPARE32
	ldr		r1, =EGA_COLOR_DONT_CARE32
	ldr		r2, [r2]
	ldr		r1, [r1]
#else
1:	ldr		r2, [r1, #(EGA_COLOR_COMPARE32-VGA_latch_minus_4)]
	ldr		r1, [r1, #(EGA_COLOR_DONT_CARE32-VGA_latch_minus_4)]
#endif
	and		r0, r1								// Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r3, r1								// Clear the "Dont Care" bits from the EGA/VGA RAM value
	and		r2, r1								// Clear the "Dont Care" bits from the Color Compare value
	eor		r1, r2, r0							// Now r1 contains bits set in nibbles that fail the comparison
	eor		r3, r2								// Now r3 contains bits set in nibbles that fail the comparison
	//-------
	// Turn on the needed pixels in AX
	//-------
	mov		r0, #0
	tst		r1, #0xF				// Pixel 0 set?
	orreq	r0, #0x0080
	tst		r1, #0xF0				// Pixel 1 set?
	orreq	r0, #0x0040
	tst		r1, #0xF00				// Pixel 2 set?
	orreq	r0, #0x0020
	tst		r1, #0xF000				// Pixel 3 set?
	orreq	r0, #0x0010
	tst		r1, #0xF0000			// Pixel 4 set?
	orreq	r0, #0x0008
	tst		r1, #0xF00000			// Pixel 5 set?
	orreq	r0, #0x0004
	tst		r1, #0xF000000			// Pixel 6 set?
	orreq	r0, #0x0002
	tst		r1, #0xF0000000			// Pixel 7 set?
	orreq	r0, #0x0001
	tst		r3, #0xF				// Pixel 0 set?
	orreq	r0, #0x8000
	tst		r3, #0xF0				// Pixel 1 set?
	orreq	r0, #0x4000
	tst		r3, #0xF00				// Pixel 2 set?
	orreq	r0, #0x2000
	tst		r3, #0xF000				// Pixel 3 set?
	orreq	r0, #0x1000
	tst		r3, #0xF0000			// Pixel 4 set?
	orreq	r0, #0x0800
	tst		r3, #0xF00000			// Pixel 5 set?
	orreq	r0, #0x0400
	tst		r3, #0xF000000			// Pixel 6 set?
	orreq	r0, #0x0200
	tst		r3, #0xF0000000			// Pixel 7 set?
	orreq	r0, #0x0100
	//-------
	// Return
	//-------
	pop		{r3}
	bx		lr


	//=======
	// On input:
	//	r0 = hword to write (higher bits not used)
	//	r1 = free
	//	r2 = Full Segment+Offset address into EGA VRAM
	//	lr = return address
	// Destroys:
	//	r0, r1
	//=======
	.global EGA_write_hword_r0_to_r2
EGA_write_hword_r0_to_r2:
	ldr		r1, =byte2wordtbl
	push	{r3-r5}
	push	{r2}								// Save the r2 register containing the output address
#ifdef IOS
	ldr		r2, =VGAModeReg
	ldrb	r2, [r2]
#else
	ldrb	r2, [r1, #(VGAModeReg-byte2wordtbl)]// Get Mode Register value
#endif
	ands	r2, #3								// r2 = Write Mode
	bne		.not_mode0_hw						// Jump if Write Mode is not zero
	//=======
	// Write Mode 0
	//	In this mode, the host data is first rotated as per the Rotate Count field,
	//	then the Enable Set/Reset mechanism selects data from this or the Set/Reset field.
	//	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//
	//		val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//		full=ExpandTable[val];
	//		full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	//		full=RasterOp(full,vga.config.full_bit_mask);
	//		break;
	//=======
	//-------
	// r0 = val = ((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldrb	r2, [r2]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
#endif
	and		r2, #7
	mov		r4, r0, lsr #8						// r4 = high byte of val
	and		r0, #0xFF							// r0 = low byte of val
	and		r4, #0xFF
	mov		r3, r0, lsr r2
	mov		r5, r4, lsr r2
	rsb		r2, #8
	orr		r3, r0, lsl r2
	orr		r5, r4, lsl r2
	and		r0, r3, #0xFF						// val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	and		r4, r5, #0xFF						// val=((val >> vga.config.data_rotate) | (val << (8-vga.config.data_rotate)));
	//-------
	// r0 = full = ExpandTable[val];
	//-------
	ldr		r0, [r1, r0, lsl #2]				// full=ExpandTable[val];
	ldr		r4, [r1, r4, lsl #2]				// full=ExpandTable[val];
	//-------
	// r0 = full = (full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset;
	//-------
#ifdef IOS
	ldr		r2, =EGA_ENABLE_SET_RESET32
	ldr		r3, =EGA_SET_RESET32
	ldr		r2, [r2]
	ldr		r3, [r3]
#else
	ldr		r2, [r1, #(EGA_ENABLE_SET_RESET32-byte2wordtbl)]
	ldr		r3, [r1, #(EGA_SET_RESET32-byte2wordtbl)]
#endif
	bic		r0, r2
	bic		r4, r2
	and		r3, r2
	orr		r0, r3								// full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	orr		r4, r3								// full=(full & vga.config.full_not_enable_set_reset) | vga.config.full_enable_and_set_reset; 
	//-------
	// r0 = full = RasterOp(full,vga.config.full_bit_mask);
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	ands	r2, #0x18
	bne		.not_raster_op_mov_hw
.raster_op_mov_hw:
	//-------
	// MOV: return (input & mask) | (vga.latch.d & ~mask);
	//-------
#ifdef IOS
	ldr		r2, =VGA_latch
	ldr		r5, [r2]
	ldr		r2, [r2, #-4]
#else
	ldr		r2, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldr		r5, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	and		r0, r3
	and		r4, r3
	bic		r2, r3
	bic		r5, r3
	orr		r0, r2
	orr		r4, r5
	//-------
	// VGA_Latch pixels;
	// pixels.d=((Bit32u*)vga.mem.linear)[start];
	// pixels.d&=vga.config.full_not_map_mask;
	// pixels.d|=(data & vga.config.full_map_mask);
	//-------
.mode1_cont_hw:
	pop		{r2}
#ifdef IOS
	ldr		r1, =EGA_WRITE_MASK32
	ldr		r1, [r1]
#else
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
#endif
	ldmia	r2, {r3, r5}
	and		r0, r1
	and		r4, r1
	bic		r3, r1
	bic		r5, r1
	orr		r0, r3
	orr		r4, r5
	//-------
	// ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	//-------
	stmia	r2, {r0, r4}
	pop		{r3-r5}
	//-------
	// Return
	//-------
	bx		lr
	
.not_raster_op_mov_hw:
	cmp		r2, #0x10
	//-------
	// AND: return (input | ~mask) & vga.latch.d;
	// OR: return (input & mask) | vga.latch.d;
	// XOR: return (input & mask) ^ vga.latch.d;
	//-------
#ifdef IOS
	ldr		r2, =VGA_latch
	ldr		r5, [r2]
	ldr		r2, [r2, #-4]
#else
	ldr		r2, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldr		r5, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	mvnlt	r3, r3								// AND: (mask = ~mask)
	orrlt	r0, r3								// AND: (input | mask)
	orrlt	r4, r3								// AND: (input | mask)
	andge	r0, r3								// OR/XOR: (input & mask)
	andge	r4, r3								// OR/XOR: (input & mask)
	andlt	r0, r2								// AND
	andlt	r4, r5								// AND
	orreq	r0, r2								// OR
	orreq	r4, r5								// OR
	eorgt	r0, r2								// XOR
	eorgt	r4, r5								// XOR
	//-------
	// VGA_Latch pixels;
	// pixels.d=((Bit32u*)vga.mem.linear)[start];
	// pixels.d&=vga.config.full_not_map_mask;
	// pixels.d|=(data & vga.config.full_map_mask);
	//-------
	pop		{r2}
#ifdef IOS
	ldr		r1, =EGA_WRITE_MASK32
	ldr		r1, [r1]
#else
	ldr		r1, [r1, #(EGA_WRITE_MASK32-byte2wordtbl)]
#endif
	ldmia	r2, {r3, r5}
	and		r0, r1
	and		r4, r1
	bic		r3, r1
	bic		r5, r1
	orr		r0, r3
	orr		r4, r5
	//-------
	// ((Bit32u*)vga.mem.linear)[start]=pixels.d;
	//-------
	stmia	r2, {r0, r4}
	pop		{r3-r5}
	//-------
	// Return
	//-------
	bx		lr

	//=======
	// Write Mode > 0
	//=======
.not_mode0_hw:
	cmp		r2, #2								// Write Mode <2 == 1, == 2, or >2 == 3?
	//=======
	// Write Mode 1
	//	In this mode, data is transferred directly from the 32 bit latch register to display memory,
	//	affected only by the Memory Plane Write Enable field. The host data is not used in this mode. 
	// r0 = full = vga.latch.d;
	//=======
#ifdef IOS
	ldrlt	r0, =VGA_latch
	ldrlt	r4, [r0, #-4]
	ldrlt	r0, [r0]
#else
	ldrlt	r0, [r1, #(VGA_latch_minus_4-byte2wordtbl)]
	ldrlt	r4, [r1, #(VGA_latch-byte2wordtbl)]
#endif
	blt		.mode1_cont_hw
	bgt		.mode3_hw
	//=======
	// Write Mode 2
	//	In this mode, the bits 3-0 of the host data are replicated across all 8 bits of their respective planes.
	//	Then the selected Logical Operation is performed on the resulting data and the data in the latch register.
	//	Then the Bit Mask field is used to select which bits come from the resulting data and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory. 
	//
	//	TODO! This does not work properly for 16-bit input value!
	//=======
	//-------
	// r0 = full = RasterOp(FillTable[val&0xF],vga.config.full_bit_mask);
	//-------
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	and		r0, #15
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	mov		r4, r0
	ands	r2, #0x18
	bne		.not_raster_op_mov_hw
	b		.raster_op_mov_hw

	//=======
	// Write Mode 3
	//	In this mode, the data in the Set/Reset field is used as if the Enable Set/Reset field were set to 1111b.
	//	Then the host data is first rotated as per the Rotate Count field, then logical ANDed with the value of the Bit Mask field.
	//	The resulting value is used on the data obtained from the Set/Reset field in the same way that the Bit Mask field would ordinarily be used,
	//	to select which bits come from the expansion of the Set/Reset field and which come from the latch register.
	//	Finally, only the bit planes enabled by the Memory Plane Write Enable field are written to memory.
	//
	//	TODO! This does not work properly for 16-bit input value!
	//=======
.mode3_hw:	
	//-------
	// r0 = full = RasterOp(vga.config.full_set_reset,ExpandTable[val] & vga.config.full_bit_mask);
	//-------
	and		r0, #0xFF									// r0 = low byte of val
#ifdef IOS
	ldr		r2, =VGAFuncReg
	ldr		r3, =EGA_BIT_MASK32
	ldrb	r2, [r2]
	ldr		r3, [r3]
#else
	ldrb	r2, [r1, #(VGAFuncReg-byte2wordtbl)]		// Get Function Register value, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	ldr		r3, [r1, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	ldr		r0, [r1, r0, lsl #2]
	and		r3, r0										// r3 = ExpandTable[val] & vga.config.full_bit_mask
#ifdef IOS
	ldr		r0, =EGA_SET_RESET32
	ldr		r0, [r0]
#else
	ldr		r0, [r1, #(EGA_SET_RESET32-byte2wordtbl)]	// r0 = vga.config.full_set_reset
#endif
	mov		r4, r0
	ands	r2, #0x18
	bne		.not_raster_op_mov_hw
	b		.raster_op_mov_hw

	.text
	.align	2

	//-------
	// This is jumped to by handlers that need to pop flags before returning.
	//-------
pop_back_2:
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown_back2
pop2_back_2:
	pop		{r0, lr}
	msr		cpsr_f,r0				// Restore flags
	b		.unknown_back2


	.ltorg

// ------------------- 00 = ADD r/m8, r8 -------------------------------
//

.macro op_00_EGA_reg8l reg
	.global op_00_EGA_l_\reg
op_00_EGA_l_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	adds	r0, r1, r0, lsl #24
	mov		r2, r3					// Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm
.macro op_00_EGA_reg8h reg
	.global op_00_EGA_h_\reg
op_00_EGA_h_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	adds	r0, r1, r0, lsl #24
	mov		r2, r3					// Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_00_EGA_reg8l r4
	op_00_EGA_reg8l r5
	op_00_EGA_reg8l r6
	op_00_EGA_reg8l r7
	op_00_EGA_reg8h r4
	op_00_EGA_reg8h r5
	op_00_EGA_reg8h r6
	op_00_EGA_reg8h r7

// ------------------- 01 = ADD r/m16, r16 -----------------------------
//
// 26010E0200 	add es:[0002],cx	(LHX)
//
.macro op_01_EGA_r2_reg reg
	.global op_01_EGA_r2_\reg
op_01_EGA_r2_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2		// r0 = hword from [r2]
	lsl		r0, #16
	adds	r0, \reg, lsl #16		// r0 = [RAM] + reg
	mov		r2, r3					// restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	lsr		r0, #16
	bl		EGA_write_hword_r0_to_r2 // r0 to hword at [r2]
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_01_EGA_r2_reg r4
	op_01_EGA_r2_reg r5
	op_01_EGA_r2_reg r6
	op_01_EGA_r2_reg r7
	op_01_EGA_r2_reg r8
	op_01_EGA_r2_reg r9
	op_01_EGA_r2_reg r10
	op_01_EGA_r2_reg r11


// ------------------- 03 = ADD r16, r/m16 ------------------------------
//
// 2603060200 	add ax,es:[0002] 	(LHX)
//
.macro op_03_EGA_reg reg
	.global op_03_EGA_\reg
op_03_EGA_\reg:
	calc_ega_r2
	bl		EGA_read_hword_r2
	mov		r2, \reg, lsl #16
	eor		\reg, r2, lsr #16		// Clear the 16-bit register value
	adds	r0, r2, r0, lsl #16		// Perform the operation using the high 16 bits
	orr		\reg, r0, lsr #16		// Put the result to the 16-bit register
	b		loop
.endm

	op_03_EGA_reg r4
	op_03_EGA_reg r5
	op_03_EGA_reg r6
	op_03_EGA_reg r7
	op_03_EGA_reg r8
	op_03_EGA_reg r9
	op_03_EGA_reg r10
	op_03_EGA_reg r11

	.ltorg

// ------------------- 08 = OR r/m8,r8 ---------------------------------
// 260885401F 	or es:[di+1F40],al (Ultima)
//
.macro op_08_EGA_reg8l reg
	.global op_08_EGA_l_\reg
op_08_EGA_l_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	lsl		r0, #24
	orrs	r0, r1
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm
.macro op_08_EGA_reg8h reg
	.global op_08_EGA_h_\reg
op_08_EGA_h_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	lsl		r0, #24
	orrs	r0, r1
	mov		r2, r3					// Restore address from r3
	lsr		r0, #24
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_08_EGA_reg8l r4
	op_08_EGA_reg8l r5
	op_08_EGA_reg8l r6
	op_08_EGA_reg8l r7
	op_08_EGA_reg8h r4
	op_08_EGA_reg8h r5
	op_08_EGA_reg8h r6
	op_08_EGA_reg8h r7
	
// ------------------- 09 = OR r/m16,r16 -------------------------------
//
// 260905 	or es:[di],ax
// 26094528  or es:[di+28],ax (Bards Tale)
//
.macro op_09_EGA_r2_reg reg
	.global op_09_EGA_r2_\reg
op_09_EGA_r2_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2
	mov		r1, \reg, lsl #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	orrs	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_09_EGA_r2_reg r4
	op_09_EGA_r2_reg r5
	op_09_EGA_r2_reg r6
	op_09_EGA_r2_reg r7
	op_09_EGA_r2_reg r8
	op_09_EGA_r2_reg r9
	op_09_EGA_r2_reg r10
	op_09_EGA_r2_reg r11

// ------------------- 0A = OR r8,r/m8 -------------------------------
//
.macro op_0a_EGA_reg8l reg
	.global op_0a_EGA_l_\reg
op_0a_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	orrs	r0, r1, r0, lsl #24
	bic		\reg, #0xFF
	orr		\reg, r0, lsr #24
	b		loop
.endm
.macro op_0a_EGA_reg8h reg
	.global op_0a_EGA_h_\reg
op_0a_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	orrs	r0, r1, r0, lsl #24
	bic		\reg, #0xFF00
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_0a_EGA_reg8l r4
	op_0a_EGA_reg8l r5
	op_0a_EGA_reg8l r6
	op_0a_EGA_reg8l r7
	op_0a_EGA_reg8h r4
	op_0a_EGA_reg8h r5
	op_0a_EGA_reg8h r6
	op_0a_EGA_reg8h r7

// ------------------- 0B = OR r16,r/m16 -------------------------------
//
.macro op_0b_EGA_reg reg
	.global op_0b_EGA_\reg
op_0b_EGA_\reg:
	calc_ega_r2
	bl		EGA_read_hword_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r3, \reg, lsl #16
	eor		\reg, r3, lsr #16
	orrs	r0, r3, r0, lsl #16
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_0b_EGA_reg r4
	op_0b_EGA_reg r5
	op_0b_EGA_reg r6
	op_0b_EGA_reg r7
	op_0b_EGA_reg r8
	op_0b_EGA_reg r9
	op_0b_EGA_reg r10
	op_0b_EGA_reg r11

	.ltorg
	
// ------------------- 20 = AND r/m8,r8 --------------------------------
//
// 26204600 	and es:[bp],al
// 262027 	and es:[bx],ah
//
.macro op_20_EGA_reg8l reg
	.global op_20_EGA_l_\reg
op_20_EGA_l_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	ands	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm
.macro op_20_EGA_reg8h reg
	.global op_20_EGA_h_\reg
op_20_EGA_h_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r1, \reg, lsl #16
	ands	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_20_EGA_reg8l r4
	op_20_EGA_reg8l r5
	op_20_EGA_reg8l r6
	op_20_EGA_reg8l r7
	op_20_EGA_reg8h r4
	op_20_EGA_reg8h r5
	op_20_EGA_reg8h r6
	op_20_EGA_reg8h r7


// ------------------- 21 = AND r/m16,r16 ------------------------------
// 262105 	and  es:[di],ax (BARD,DK)
// 26214528 	and  es:[di+28],ax (BARD,DK)
//
.macro op_21_EGA_r2_reg reg
	.global op_21_EGA_r2_\reg
op_21_EGA_r2_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r1, \reg, lsl #16
	ands	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_21_EGA_r2_reg r4
	op_21_EGA_r2_reg r5
	op_21_EGA_r2_reg r6
	op_21_EGA_r2_reg r7
	op_21_EGA_r2_reg r8
	op_21_EGA_r2_reg r9
	op_21_EGA_r2_reg r10
	op_21_EGA_r2_reg r11


// ------------------- 22 = AND r8,r/m8 --------------------------------
//
.macro op_22_EGA_reg8l reg
	.global op_22_EGA_l_\reg
op_22_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	ands	r0, r1, r0, lsl #24
	bic		\reg, #0xFF
	orr		\reg, r0, lsr #24
	b		loop
.endm
.macro op_22_EGA_reg8h reg
	.global op_22_EGA_h_\reg
op_22_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #16
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	ands	r0, r1, r0, lsl #24
	bic		\reg, #0xFF00
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_22_EGA_reg8l r4
	op_22_EGA_reg8l r5
	op_22_EGA_reg8l r6
	op_22_EGA_reg8l r7
	op_22_EGA_reg8h r4
	op_22_EGA_reg8h r5
	op_22_EGA_reg8h r6
	op_22_EGA_reg8h r7


// ------------------- 23 = AND r16,r/m16 --------------------------------
//
.macro op_23_EGA_reg reg
	.global op_23_EGA_\reg
op_23_EGA_\reg:
	calc_ega_r2
	bl		EGA_read_hword_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r3, \reg, lsl #16
	eor		\reg, r3, lsr #16
	ands	r0, r3, r0, lsl #16
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_23_EGA_reg r4
	op_23_EGA_reg r5
	op_23_EGA_reg r6
	op_23_EGA_reg r7
	op_23_EGA_reg r8
	op_23_EGA_reg r9
	op_23_EGA_reg r10
	op_23_EGA_reg r11

	.ltorg
	
// ------------------- 30 = XOR r/m8, r8 -------------------------------
//
// 26301D          xor  es:[di],bl (Wasteland)
// 304501          xor  [di+01],al (Supaplex)
//
.macro op_30_EGA_reg8l reg
	.global op_30_EGA_l_\reg
op_30_EGA_l_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	eors	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm
.macro op_30_EGA_reg8h reg
	.global op_30_EGA_h_\reg
op_30_EGA_h_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	eors	r0, r1, r0, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_30_EGA_reg8l r4
	op_30_EGA_reg8l r5
	op_30_EGA_reg8l r6
	op_30_EGA_reg8l r7
	op_30_EGA_reg8h r4
	op_30_EGA_reg8h r5
	op_30_EGA_reg8h r6
	op_30_EGA_reg8h r7


// ------------------- 31 = XOR r/m16,r16 ------------------------------
// 3105      xor  [di],ax (BARD, DK)
//
.macro op_31_EGA_r2_reg reg
	.global op_31_EGA_r2_\reg
op_31_EGA_r2_\reg:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2
	mov		r1, \reg, lsl #16
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	eors	r0, r1, r0, lsl #16
	lsr		r0, #16
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

	op_31_EGA_r2_reg r4
	op_31_EGA_r2_reg r5
	op_31_EGA_r2_reg r6
	op_31_EGA_r2_reg r7
	op_31_EGA_r2_reg r8
	op_31_EGA_r2_reg r9
	op_31_EGA_r2_reg r10
	op_31_EGA_r2_reg r11


// ------------------- 32 = XOR r8, r/m8 -----------------------------
//
// x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
// We clear all the flags before the actual eors operation, so C and O work like in x86.
//
// 263205 	xor al,es:[di] 	(bob_fossil) 
//
.macro op_32_EGA_reg8l reg
	.global op_32_EGA_l_\reg
op_32_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	eors	r0, r1, r0, lsl #24
	bic		\reg, #0xFF
	orr		\reg, r0, lsr #24
	b		loop
.endm
.macro op_32_EGA_reg8h reg
	.global op_32_EGA_h_\reg
op_32_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	msr		cpsr_f,#0				// Clear all flags (especially C or O)
	eors	r0, r1, r0, lsl #24
	bic		\reg, #0xFF00
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_32_EGA_reg8l r4
	op_32_EGA_reg8l r5
	op_32_EGA_reg8l r6
	op_32_EGA_reg8l r7
	op_32_EGA_reg8h r4
	op_32_EGA_reg8h r5
	op_32_EGA_reg8h r6
	op_32_EGA_reg8h r7

// ------------------- 33 = XOR r16, r/m16 -----------------------------
//
.macro op_33_EGA_reg reg
	.global op_33_EGA_\reg
op_33_EGA_\reg:
	calc_ega_r2
	bl		EGA_read_hword_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r3, \reg, lsl #16
	eor		\reg, r3, lsr #16
	eors	r0, r3, r0, lsl #16
	orr		\reg, r0, lsr #16
	b		loop
.endm

	op_33_EGA_reg r4
	op_33_EGA_reg r5
	op_33_EGA_reg r6
	op_33_EGA_reg r7
	op_33_EGA_reg r8
	op_33_EGA_reg r9
	op_33_EGA_reg r10
	op_33_EGA_reg r11

	.ltorg
	

// ------------------- 38 = CMP r/m8,r8 --------------------------------
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
// 263804 	cmp es:[si],al 	(lilalurl, OE)
// 263801    cmp  es:[bx+di],al	(X)
//
.macro op_38_EGA_reg8l reg
	.global op_38_EGA_l_\reg
op_38_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	lsl		r0, #24
	cmp		r0, \reg, lsl #24		// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm
.macro op_38_EGA_reg8h reg
	.global op_38_EGA_h_\reg
op_38_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00
	lsl		r1, #16
	rsbs	r1, r0, lsl #24
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_38_EGA_reg8l r4
	op_38_EGA_reg8l r5
	op_38_EGA_reg8l r6
	op_38_EGA_reg8l r7
	op_38_EGA_reg8h r4
	op_38_EGA_reg8h r5
	op_38_EGA_reg8h r6
	op_38_EGA_reg8h r7

	.ltorg

// ------------------- 39 = CMP r/m16,r16 ------------------------------
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
// 26390E0200      cmp  es:[0002],cx		(LHX)
//
.macro op_39_EGA_r2_reg reg
	.global op_39_EGA_r2_\reg
op_39_EGA_r2_\reg:
	calc_ega_r2
	bl		EGA_read_hword_r2
	lsl		r0, #16
	cmp		r0, \reg, lsl #16		// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_39_EGA_r2_reg r4
	op_39_EGA_r2_reg r5
	op_39_EGA_r2_reg r6
	op_39_EGA_r2_reg r7
	op_39_EGA_r2_reg r8
	op_39_EGA_r2_reg r9
	op_39_EGA_r2_reg r10
	op_39_EGA_r2_reg r11

	
// ------------------- 3A = CMP r8,r/m8 --------------------------------
//
// 263A05 	cmp  al,es:[di] 	(drdim:START)
// 263A6501  cmp  ah,es:[di+01]
//
// ARM uses the Carry flag in the opposite sense to x86, so we need to swap it.
//
.macro op_3a_EGA_reg8l reg
	.global op_3a_EGA_l_\reg
op_3a_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsl #24		// r1 = AL in high byte
	cmp		r1, r0, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm
.macro op_3a_EGA_reg8h reg
	.global op_3a_EGA_h_\reg
op_3a_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF00		// r1 = AH in high byte
	lsl		r1, #16
	cmp		r1, r0, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)
.endm

	op_3a_EGA_reg8l r4
	op_3a_EGA_reg8l r5
	op_3a_EGA_reg8l r6
	op_3a_EGA_reg8l r7
	op_3a_EGA_reg8h r4
	op_3a_EGA_reg8h r5
	op_3a_EGA_reg8h r6
	op_3a_EGA_reg8h r7


// ------------------- 80 = ??? r/m8,imm8 ------------------------------
//
// 26800FFF 		or   byte es:[bx],FF 		(midwinter)
// 26803C18 		cmp  byte es:[si],18 		(09-feb)
// 26807C2818 	cmp  byte es:[si+28],18 	(09-feb)
// 268037FF 		xor  byte es:[bx],FF
//
	//-------
	// Add: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_add
op_80_EGA_add:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	lsl		r0, #24
	adds	r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop

	//-------
	// Adc: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_adc
op_80_EGA_adc:
	calc_ega_r2
	mrs		r0,cpsr					// Save flags to r0 (for input Carry)
	mov		r3, r2					// Save original r2
	str		r0, [sp, #SP_STR_SEG]	// Save flags to stack
	bl		EGA_read_byte_r2
	ldr		r1, [sp, #SP_STR_SEG]	// Get flags from stack
	lsl		r0, #24
	msr		cpsr_f, r1				// Set the CPU flags
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	addcs	r0, #0x01000000			// If input Carry is set, adjust the right operand so that ...
	subcs	r0, #0x00000001			// ... we can use the ARM ADC opcode for the actual operation.
	adcs	r0, r1, lsl #24			// Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop

	//-------
	// Sub: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_sub
op_80_EGA_sub:
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	lsl		r0, #24
	subs	r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		complement_carry

	//-------
	// Sbb: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_sbb
op_80_EGA_sbb:
	calc_ega_r2
	mrs		r0,cpsr					// Save flags to r0 (for input Carry)
	mov		r3, r2					// Save original r2
	str		r0, [sp, #SP_STR_SEG]	// Save flags to stack
	bl		EGA_read_byte_r2
	ldr		r1, [sp, #SP_STR_SEG]	// Get flags from stack
	lsl		r0, #24
	msr		cpsr_f, r1				// Set the CPU flags
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	lsl		r1, #24
	addcss	r1, #0x01000000			// If input Carry is set, the right operand = (register value + 1).
	subs	r0, r1					// Perform the actual addition, setting the resulting flags.
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		complement_carry

	//-------
	// Cmp: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_cmp
op_80_EGA_cmp:
	calc_ega_r2
	ldrb	r3,[r12], #1			// Load the imm8 byte to r3
	bl		EGA_read_byte_r2
	lsl		r3, #24
	rsbs	r3, r0, lsl #24			// Compare the values using the high bytes
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

.macro op80EGAlogical x86oper armoper
	//-------
	// Memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_80_EGA_\x86oper
op_80_EGA_\x86oper:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_byte_r2
	ldrb	r1,[r12], #1			// Load the imm8 byte to r1
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	lsl		r0, #24
	\armoper r0, r1, lsl #24
	lsr		r0, #24
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

op80EGAlogical or orrs
op80EGAlogical and ands
op80EGAlogical xor eors


// ------------------- 81 = ??? r/m16,imm16 ----------------------------
// 
// 268125FC00      and  word es:[di],00FC	(Spacewrecked)
//
	.global	op_81_EGA_add
op_81_EGA_add:
	.global	op_81_EGA_sub
op_81_EGA_sub:
	.global	op_81_EGA_adc
op_81_EGA_adc:
	.global	op_81_EGA_sbb
op_81_EGA_sbb:
	b		bad_EGA_opcode

.macro op81EGAlogical x86oper armoper
	//-------
	// Memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_81_EGA_\x86oper
op_81_EGA_\x86oper:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2
	ldrb	r1, [r12], #2			// r1 = low byte of imm16 value
	ldrb	r2, [r12, #-1]			// r2 = high byte of imm16 value
	lsl		r0, #16					// r0 = EGA value in high halfword
	lsl		r1, #16
	orr		r1, r2, lsl #24			// r1 = imm16 value in high halfword
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	\armoper r0, r1
	lsr		r0, #16
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

op81EGAlogical or orrs
op81EGAlogical and ands
op81EGAlogical xor eors

	//-------
	// Cmp: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_81_EGA_cmp
op_81_EGA_cmp:
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldrb	r1, [r12], #2			// r1 = low byte of imm16 value
	ldrb	r2, [r12, #-1]			// r2 = high byte of imm16 value
	lsl		r0, #16
	orr		r1, r2, lsl #8			// r1 = imm16 value in low halfword
	cmp		r0, r1, lsl #16			// Compare the values
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

// ------------------- 83 = ??? r/m16,+imm8 ----------------------------
// 
// 26833E00000F 	cmp word es:[0000],000F 	(LHX)
//
	.global	op_83_EGA_add
op_83_EGA_add:
	.global	op_83_EGA_sub
op_83_EGA_sub:
	.global	op_83_EGA_adc
op_83_EGA_adc:
	.global	op_83_EGA_sbb
op_83_EGA_sbb:
	b		bad_EGA_opcode

.macro op83EGAlogical x86oper armoper
	//-------
	// Memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_83_EGA_\x86oper
op_83_EGA_\x86oper:
	calc_ega_r2
	mov		r3, r2					// Save original r2
	bl		EGA_read_hword_r2
	ldrsb	r1,[r12],#1
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	lsl		r0, #16
	lsl		r1, #16
	\armoper r0, r1
	lsr		r0, #16
	mov		r2, r3					// Restore address from r3
	mrs		r3,cpsr					// Save flags to r3
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f,r3				// Restore flags
	b		loop
.endm

op83EGAlogical or orrs
op83EGAlogical and ands
op83EGAlogical xor eors

	//-------
	// Cmp: memory location in EGA VRAM.
	// On input:
	//	r2 = physical memory address (with EGA flags)
	//-------
	.global	op_83_EGA_cmp
op_83_EGA_cmp:
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldrsb	r1,[r12],#1
	lsl		r0, #16
	cmp		r0, r1, lsl #16			// Compare the values
	b		complement_carry		// Jump back to loop, reversing the Carry flag (ARM -> x86 convention)

// ------------------- 84 = TEST r8,r/m8 -------------------------------
//
// 268405    test es:[di],al	(SIERRA)
// 26841D    test es:[di],bl	(XENON2)
// 268425 	test es:[di],ah 	(XENON2)
//
// x86 clears C and O flags, while ARM leaves O untouched and C gets the shifter output.
// We clear all the flags before the actual tst operation, so C and O work like in x86.
//
.macro op_84_EGA_reg8l reg
	.global op_84_EGA_l_\reg
op_84_EGA_l_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r1, \reg, lsl #24
	tst		r1, r0, lsl #24
	b		loop
.endm
.macro op_84_EGA_reg8h reg
	.global op_84_EGA_h_\reg
op_84_EGA_h_\reg:
	calc_ega_r2
	bl		EGA_read_byte_r2
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	mov		r1, \reg, lsl #16
	tst		r1, r0, lsl #24
	b		loop
.endm

	op_84_EGA_reg8l r4
	op_84_EGA_reg8l r5
	op_84_EGA_reg8l r6
	op_84_EGA_reg8l r7
	op_84_EGA_reg8h r4
	op_84_EGA_reg8h r5
	op_84_EGA_reg8h r6
	op_84_EGA_reg8h r7


// ------------------- 86 = XCHG r/m8,r8 -----------------------------
// This is most likely used in modes that require -read-before-write
// handling to refresh the internal latch register. Probably either
// Write Mode 2, or when using the Function Select register.
//
// 268605        xchg es:[di],al
// 268607        xchg es:[bx],al
// 268625        xchg es:[di],ah
// 2686451E 		xchg es:[di+1E],al (Gauntlet 2)
// 2686859600 	xchg es:[di+0096],al (Gauntlet 2)
//
.macro op_86_EGA_reg8l reg
	.global op_86_EGA_l_\reg
op_86_EGA_l_\reg:
	calc_ega_r2
	mrs		r0,cpsr					// Save flags to r0 (for input Carry)
	mov		r3, r2					// Save original r2
	str		r0, [sp, #SP_STR_SEG]	// Save flags to stack
	bl		EGA_read_byte_r2
	and		r1, \reg, #0xFF
	bic		\reg, #0xFF
	orr		\reg, r0
	mov		r2, r3
	mov		r0, r1
	bl		EGA_write_byte_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	// Restore flags from stack
	b		restore_flags_from_r0
.endm
.macro op_86_EGA_reg8h reg
	.global op_86_EGA_h_\reg
op_86_EGA_h_\reg:
	calc_ega_r2
	mrs		r0,cpsr					// Save flags to r0 (for input Carry)
	mov		r3, r2					// Save original r2
	str		r0, [sp, #SP_STR_SEG]	// Save flags to stack
	bl		EGA_read_byte_r2
	mov		r1, \reg, lsr #8
	bic		\reg, #0xFF00
	orr		\reg, r0, lsl #8
	mov		r2, r3
	and		r0, r1, #0xFF
	bl		EGA_write_byte_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	// Restore flags from stack
	b		restore_flags_from_r0
.endm

	op_86_EGA_reg8l r4
	op_86_EGA_reg8l r5
	op_86_EGA_reg8l r6
	op_86_EGA_reg8l r7

	op_86_EGA_reg8h r4
	op_86_EGA_reg8h r5
	op_86_EGA_reg8h r6
	op_86_EGA_reg8h r7

	.ltorg

// ------------------- 87 = XCHG r/m16,r16 -----------------------------
// 8707 xchg [bx],ax (RAMPAGE)
//
.macro op_87_EGA reg
	.global op_87_EGA_\reg
op_87_EGA_\reg:
	calc_ega_r2
	mrs		r0,cpsr					// Save flags to r0 (for input Carry)
	mov		r3, r2					// Save original r2
	str		r0, [sp, #SP_STR_SEG]	// Save flags to stack
	bl		EGA_read_hword_r2
	mov		r1, \reg, lsl #16
	eor		\reg, r1, lsr #16
	orr		\reg, r0
	mov		r2, r3
	mov		r0, r1, lsr #16
	bl		EGA_write_hword_r0_to_r2
	ldr		r0, [sp, #SP_STR_SEG]	// Restore flags from stack
	b		restore_flags_from_r0
.endm

	op_87_EGA r4
	op_87_EGA r5
	op_87_EGA r6
	op_87_EGA r7
	op_87_EGA r8
	op_87_EGA r9
	op_87_EGA r10
	op_87_EGA r11

	.ltorg

	
// ------------------- 88 = MOV r/m8,r8 --------------------------------
// 268801		  MOV ES:[BX+DI],AL
// 26881D		  MOV ES:[DI],BL
// 268880D007      mov es:[bx+si+07D0],al (Nukem2)
// 26884528		  mov es:[di+28],al (Nukem2)
//
.macro op_88_EGA_reg8l reg
	.global op_88_EGA_l_\reg
op_88_EGA_l_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	and		r0, \reg, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3
	b		loop
.endm
.macro op_88_EGA_reg8h reg
	.global op_88_EGA_h_\reg
op_88_EGA_h_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	mov		r0, \reg, lsr #8
	and		r0, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3
	b		loop
.endm

	op_88_EGA_reg8l r4
	op_88_EGA_reg8l r5
	op_88_EGA_reg8l r6
	op_88_EGA_reg8l r7

	op_88_EGA_reg8h r4
	op_88_EGA_reg8h r5
	op_88_EGA_reg8h r6
	op_88_EGA_reg8h r7

	.ltorg

// ------------------- 89 = MOV r/m16,r16 ------------------------------
// 26891D		mov es:[di],bx (KEEN4E)
// 26891EFABF	mov es:[BFFA],bx (WIN)
//
.macro op_89_EGA_reg16 reg
	.global op_89_EGA_\reg
op_89_EGA_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	mov		r0, \reg
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3
	b		loop
.endm

	op_89_EGA_reg16 r4
	op_89_EGA_reg16 r5
	op_89_EGA_reg16 r6
	op_89_EGA_reg16 r7
	op_89_EGA_reg16 r8
	op_89_EGA_reg16 r9
	op_89_EGA_reg16 r10
	op_89_EGA_reg16 r11

	.ltorg

// ------------------- 8A = MOV r8,r/m8 --------------------------------
// 268A05 = MOV AL,ES:[DI]
// 268A07 = MOV AL,ES:[BX] (Nukem2)
// 268A00 = MOV AL,ES:[BX+SI] (Wasteland)
// 268A4528 = mov al,es:[di+28] (Nukem2)
//
.macro op_8a_EGA_reg8l reg
	.global op_8a_EGA_l_\reg
op_8a_EGA_l_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	bl		EGA_read_byte_r2
	bic		\reg, #0xFF
	orr		\reg, r0
	msr		cpsr_f, r3
	b		loop
.endm
.macro op_8a_EGA_reg8h reg
	.global op_8a_EGA_h_\reg
op_8a_EGA_h_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	bl		EGA_read_byte_r2
	bic		\reg, #0xFF00
	orr		\reg, r0, lsl #8
	msr		cpsr_f, r3
	b		loop
.endm

	op_8a_EGA_reg8l r4
	op_8a_EGA_reg8l r5
	op_8a_EGA_reg8l r6
	op_8a_EGA_reg8l r7

	op_8a_EGA_reg8h r4
	op_8a_EGA_reg8h r5
	op_8a_EGA_reg8h r6
	op_8a_EGA_reg8h r7

	.ltorg

// ------------------- 8B = MOV r16,r/m16 ------------------------------
// 268B0D	mov	cx,es:[di]	(KEEN4E)
// 268B2C 	mov bp,es:[si] (WARET)
//
.macro op_8b_EGA_reg16 reg
	.global op_8b_EGA_\reg
op_8b_EGA_\reg:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	bl		EGA_read_hword_r2
	lsr		\reg, #16
	orr		\reg, r0, \reg, lsl #16
	msr		cpsr_f, r3
	b		loop
.endm

	op_8b_EGA_reg16 r4
	op_8b_EGA_reg16 r5
	op_8b_EGA_reg16 r6
	op_8b_EGA_reg16 r7
	op_8b_EGA_reg16 r8
	op_8b_EGA_reg16 r9
	op_8b_EGA_reg16 r10
	op_8b_EGA_reg16 r11

	.ltorg

// ------------------- 8E = MOV Sreg,r/m16 -----------------------------
//
// 268E060400 	mov es,es:[0004] 	(LHX)
//
	.global	op_8e_EGA_r2_es
op_8e_EGA_r2_es:
	calc_ega_r2
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	bl		EGA_read_hword_r2
	mov		r2, r0					// r2 = hword from EGA VRAM
	msr		cpsr_f,r3				// Restore flags
	b		mov_es_r2				// Jump to the ES segment setting in "cpu.s"

// ------------------- A4 = MOVSB --------------------------------------
//
// Move a byte from DS:SI to ES:DI
//
	.global	op_a4_from_EGA
op_a4_from_EGA:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with EGA flag)
	//	r3 = Address size mask (0x0000FFFF)
	//-------
	str		r1, [sp, #SP_STR_SEG]	// Save EDI inc/dec value to stack
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldr		lr, [sp, #SP_STR_SEG]	// Get EDI inc/dec value from stack
	mov		r1, r0					// r1 = the byte read from DS:SI
	msr		cpsr_f, r3				// Restore flags
	ldr		r3, [sp, #SP_MASK_16]	// Use 16-bit memory addressing
	mov		r0, edi					// r0 = DI
	add		edi, lr, edi, ror #16	// Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES op_a4_to_RAM op_a4_to_EGA .unknown_back1

	.global	op_a4_from_EGA_USE32
op_a4_from_EGA_USE32:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with EGA flag)
	//	r3 = Address size mask (0xFFFFFFFF)
	//-------
	str		r1, [sp, #SP_STR_SEG]	// Save EDI inc/dec value to stack
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldr		lr, [sp, #SP_STR_SEG]	// Get EDI inc/dec value from stack
	mov		r1, r0					// r1 = the byte read from DS:SI
	msr		cpsr_f, r3				// Restore flags
	mvn		r3, #0					// Use 32-bit memory addressing
	mov		r0, edi					// r0 = EDI
	add		edi, lr					// Fix logical EDI.
	mem_handler_jump_r0r3_ES op_a4_to_RAM op_a4_to_EGA .unknown_back1

	//-------
	// On input:
	//	r1 = byte read from DS:SI
	//	r2 = ES:DI linear address (with EGA flag)
	//-------
	.global	op_a4_to_EGA
op_a4_to_EGA:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, r1					// Get the output byte from r1
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop

	.ltorg
	
// ------------------- A5 = MOVSW --------------------------------------
//
// Move a word from DS:SI to ES:DI
//
	.global	op_a5_from_EGA
op_a5_from_EGA:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with EGA flag)
	//	r3 = Address size mask (0x0000FFFF)
	//-------
	str		r1, [sp, #SP_STR_SEG]	// Save EDI inc/dec value to stack
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldr		lr, [sp, #SP_STR_SEG]	// Get EDI inc/dec value from stack
	mov		r1, r0					// r1 = the byte read from DS:SI
	msr		cpsr_f, r3				// Restore flags
	ldr		r3, [sp, #SP_MASK_16]	// Use 16-bit memory addressing
	mov		r0, edi					// r0 = DI
	add		edi, lr, edi, ror #16	// Fix logical DI.
	ror		edi, #16
	mem_handler_jump_r0r3_ES op_a5_to_RAM op_a5_to_EGA .unknown_back1

	.global	op_a5_from_EGA_USE32
op_a5_from_EGA_USE32:
	//-------
	// On input:
	//	r1 = EDI increment/decrement value
	//	r2 = DS:SI linear address (with EGA flag)
	//	r3 = Address size mask (0x0000FFFF)
	//-------
	str		r1, [sp, #SP_STR_SEG]	// Save EDI inc/dec value to stack
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldr		lr, [sp, #SP_STR_SEG]	// Get EDI inc/dec value from stack
	mov		r1, r0					// r1 = the byte read from DS:SI
	msr		cpsr_f, r3				// Restore flags
	mvn		r3, #0					// Use 32-bit memory addressing
	mov		r0, edi					// r0 = EDI
	add		edi, lr					// Fix logical EDI.
	mem_handler_jump_r0r3_ES op_a5_to_RAM op_a5_to_EGA .unknown_back1


	.global	movsd_from_EGA
movsd_from_EGA:
	.global	movsd_to_EGA
movsd_to_EGA:
	.global	movsd_from_EGA_USE32
movsd_from_EGA_USE32:
	.global	stosd_EGA
stosd_EGA:
	.global	rep_stosd_cld_EGA
rep_stosd_cld_EGA:
	b		.unknown

	//-------
	// On input:
	//	r1 = halfword read from DS:SI
	//	r2 = ES:DI linear address (with EGA flag)
	//-------
	.global	op_a5_to_EGA
op_a5_to_EGA:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, r1					// r0 = the halfword read from DS:SI
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- AA = STOSB --------------------------------------
// Segment override does not affect STOSB, it always uses ES:DI
// On input r2 = linear ES:DI with EGA flags
//
	.global	stosb_EGA
stosb_EGA:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	and		r0, eax, #0xFF
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- AB = STOSW --------------------------------------
// Segment override does not affect STOSW, it always uses ES:DI
// On input r2 = linear ES:DI with EGA flags
//
	.global	stosw_EGA
stosw_EGA:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	mov		r0, eax
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop
	
// ------------------- AC = LODSB --------------------------------------
//
// We may not change the flags!
//
	.global	lodsb_EGA
lodsb_EGA:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_byte_r2
	bic		eax, #0xFF
	orr		eax, r0
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- AD = LODSW --------------------------------------
//
// We may not change the flags!
//
	.global	lodsw_EGA
lodsw_EGA:								// This is called by "cpu_string.s"
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	bl		EGA_read_hword_r2
	lsr		eax, #16
	orr		eax, r0, eax, lsl #16
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- AD = LODSD --------------------------------------
//
// We may not change the flags!
//
	.global	lodsd_EGA
lodsd_EGA:								// This is called by "cpu_string.s"
	mrs		r3,cpsr						// Save flags (we are not allowed to change any)
	calc_ega_r2
	str		r2, [sp, #SP_STR_SEG]		// Save original r2 value
	bl		EGA_read_hword_r2			// Read low halfword
	ldr		r2, [sp, #SP_STR_SEG]		// Restore original r2 value
	mov		eax, r0
	add		r2, #2*4
	bl		EGA_read_hword_r2			// Read high halfword
	orr		eax, r0, lsl #16
	msr		cpsr_f, r3					// Restore flags
	b		loop

// ------------------- AE = SCASB --------------------------------------
//
	.global	scasb_EGA
scasb_EGA:
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, eax, lsl #24
	cmp		r1, r0, lsl #24			// Compare AL with byte
	b		complement_carry		// Go back to the opcode loop

	.ltorg

// ------------------- C6 = MOV r/m8, imm8 -----------------------------
//
	.global	op_c6_EGA_r2
op_c6_EGA_r2:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	ldrb	r0,[r12],#1				// Load the imm8 byte to r0, increment r12 by 1
	bl		EGA_write_byte_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- C7 = MOV r/m16, imm16 -----------------------------
//
	.global	op_c7_EGA_r2
op_c7_EGA_r2:
	mrs		r3,cpsr					// Save flags (we are not allowed to change any)
	calc_ega_r2
	ldrb	r0,[r12],#1				// Load the imm16 low byte to r0, increment r12 by 1
	ldrb	r1,[r12],#1				// Load the imm16 high byte to r1, increment r12 by 1
	orr		r0, r1, lsl #8
	bl		EGA_write_hword_r0_to_r2
	msr		cpsr_f, r3				// Restore flags
	b		loop

// ------------------- D0 = ROL/ROR/RCL/RCR/SHL/SHR/SHL/SAR r/m8,1 ---
// 

	//=======
	// Rotate by a single bit position, putting current Carry flag into the lowest bit.
	//=======
	.global	rcl_byte_r2_1_EGA
rcl_byte_r2_1_EGA:					// Directly jump here from single-bit-shift opcodes
	mrs		r3,cpsr					// Save flags to r3
	calc_ega_r2
	str		r2, [sp, #SP_STR_SEG]	// Save original r2 value
	bl		EGA_read_byte_r2
	//-------
	// Perform the RCL operation
	//-------
	lsl		r0, #1					// Shift left 1 bit
	tst		r3, #ARM_CARRY
	orrne	r0, #1					// Move Carry into the lowest bit
	bic		r3, #ARM_CARRY|ARM_OVER	// Clear the Carry and Overflow bits of the saved CPU flags
	//-------
	// Swap the values between registers and stack
	//-------
	ldr		r2, [sp, #SP_STR_SEG]	// Get original r2 value
	str		r0, [sp, #SP_STR_SEG]	// Save the output byte to stack
	//-------
	// Write the resulting byte to EGA VRAM
	//-------
	bl		EGA_write_byte_r0_to_r2
	//-------
	// Calculate the resulting flags and return
	//-------
	ldr		r0, [sp, #SP_STR_SEG]	// Get output value from stack
	eor		r0, r0, lsr #1			// Now r0 bit 0x100 is the new Carry flag, bit 0x80 is the new Overflow flag
	and		r0, #0x180				// Leave only the new Carry and Overflow bits to r1
	orr		r0, r3, r0, lsl #(24-3)	// and put them into the ARM_CARRY and ARM_OVER positions (0x30000000) in r0
	b		restore_flags_from_r0	// Jump back to loop, restoring the flags


// ------------------- REP MOVSB ---------------------------
//

	.global	rep_movsb_cld_from_EGA
rep_movsb_cld_from_EGA:				// This code is common for both REP MOVSB and REP MOVSW
	calc_ega_r1
	mov		r0, r11					// r0high = DI
	mem_handler_jump_r0r3_ES rep_movsb_cld_EGA_RAM rep_movsb_cld_EGA_EGA .unknown_back1

	.global	rep_movsb_std_from_EGA
rep_movsb_std_from_EGA:
	calc_ega_r1
	mov		r0, r11					// r0high = DI
	mem_handler_jump_r0r3_ES rep_movsb_std_EGA_RAM rep_movsb_std_EGA_EGA .unknown_back1

// ------------------- REP MOVSB from RAM to EGA ------------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in RAM
	//	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
	.global	rep_movsb_cld_RAM_EGA
rep_movsb_cld_RAM_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
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
	// Loop here for r3 input bytes.
	// Registers:
	//	r0 = scratch
	//	r1 = destroyed
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = input RAM pointer (DS:SI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2	// Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
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

// ------------------- REP MOVSB from RAM to EGA (STD) -----------------
// This is a silly and rare operation!
//
	.global	rep_movsb_std_RAM_EGA
rep_movsb_std_RAM_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
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
	// Loop here for r3 input bytes.
	// Registers:
	//	r0 = scratch
	//	r1 = destroyed
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = input RAM pointer (DS:SI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #-1			// r0 = byte from DS:SI, decrement input pointer
	bl		EGA_write_byte_r0_to_r2	// Write it to ES:DI. TODO! This should be speeded up!
	sub		r2, #4					// Decrement output pointer
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

// ------------------- REP MOVS from EGA to RAM ------------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in RAM
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsb_cld_EGA_RAM:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
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
	// Check the current read mode
	//-------
2:	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	tst		r0, #8
	bne		.movsb_cld_EGA_RAM_readmode_1	// Go handle Read Mode 1
	//-------
	// Get the read mask
	//-------
	ldr		r0,=EGA_READ_MASK32
	ldr		lr, [r0]
	//-------
	// Get the value from our emulated EGA/VGA RAM
	//-------
1:	ldr		r4, [r1], #4
	//-------
	// Read Mode 0, return 8 consecutive pixels from EGA VRAM.
	//-------
	mov		r0, #0
	and		r4, lr					// Leave only the pixels in r1 that exist in the plane we are interested in.
	tst		r4, #0xF				// Pixel 0 set?
	orrne	r0, #0x80
	tst		r4, #0xF0				// Pixel 1 set?
	orrne	r0, #0x40
	tst		r4, #0xF00				// Pixel 2 set?
	orrne	r0, #0x20
	tst		r4, #0xF000				// Pixel 3 set?
	orrne	r0, #0x10
	tst		r4, #0xF0000			// Pixel 4 set?
	orrne	r0, #0x08
	tst		r4, #0xF00000			// Pixel 5 set?
	orrne	r0, #0x04
	tst		r4, #0xF000000			// Pixel 6 set?
	orrne	r0, #0x02
	tst		r4, #0xF0000000			// Pixel 7 set?
	orrne	r0, #0x01
	//-------
	// Save the output byte
	//-------
	strb	r0, [r2], #1
	subs	r3, #1
	bgt		1b
.movsb_cld_EGA_RAM_done:
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
	min_cnt_VGA_end 	r3 r1		// ----- Check with DS:SI bytes before VGA VRAM end
	min_cnt_next_page 	r3 r2		// ----- Check with ES:DI bytes before page end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
1:	add		esi, r3					// Increment ESI by the number of bytes we shall move.
	add		edi, r3					// Increment EDI by the number of bytes we shall move..
	sub		ecx, r3					// ecx = number of bytes remaining
	b		2b						// The rest of the code is similar to USE16 version.
	//-------
	// Read Mode 1, return the result of a color comparison.
	//-------
.movsb_cld_EGA_RAM_readmode_1:	
	ldr		lr, =EGA_COLOR_COMPARE32
	ldr		r4, [lr]
#ifdef IOS
	ldr		lr, =EGA_COLOR_DONT_CARE32
	ldr		lr, [lr]
#else
	ldr		lr, [lr, #(EGA_COLOR_DONT_CARE32-EGA_COLOR_COMPARE32)]
#endif
	and		lr, r4								// Clear the "Dont Care" bits from the Color Compare value
	//-------
	// Get the value from our emulated EGA/VGA RAM
	// Registers:
	//	r0 = value read from EGA VRAM
	//	r1 = input address DS:SI
	//	r2 = output address ES:DI
	//	r3 = loop counter
	//	r4 = "Dont Care" value, not changed in the loop
	//	r5 = total counter CX
	//	lr = Color Compare value, not changed in the loop
	//	r10 = source index SI
	//	r11 = target index DI
	//-------
1:	ldr		r0, [r1], #4
	and		r0, r4								// Clear the "Dont Care" bits from the EGA/VGA RAM value
	eor		r0, lr								// Now r1 contains bits set in nibbles that fail the comparison
	//-------
	// Turn on the needed pixels in AL
	//-------
	tst		r0, #0xF0000000						// Pixel 7 set?
	lsl		r0, #4
	orreq	r0, #0x01
	tst		r0, #0xF0							// Pixel 0 set?
	bic		r0, #0xF0
	orreq	r0, #0x80
	tst		r0, #0xF000							// Pixel 1 set?
	orreq	r0, #0x40
	tst		r0, #0xF00000						// Pixel 2 set?
	orreq	r0, #0x20
	tst		r0, #0xF0000000						// Pixel 3 set?
	orreq	r0, #0x10
	tst		r0, #0xF00							// Pixel 4 set?
	orreq	r0, #0x08
	tst		r0, #0xF0000						// Pixel 5 set?
	orreq	r0, #0x04
	tst		r0, #0xF000000						// Pixel 6 set?
	orreq	r0, #0x02
	//-------
	// Save the output byte
	//-------
	strb	r0, [r2], #1
	subs	r3, #1
	bgt		1b
	b		.movsb_cld_EGA_RAM_done

// ------------------- REP MOVSB from EGA to RAM (STD) -----------------
// This is a silly and rare operation!
//
rep_movsb_std_EGA_RAM:
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical linear start address of DS:SI in EGAVGA_A000
	//	r2 = physical start address of ES:DI in RAM
	//	r5 = number of bytes to store, needs to be cleared
	//	r10 = SI register, needs to be incremented by CX
	//	r11 = DI register, needs to be incremented by CX
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6}
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
	// Loop here for r3 input bytes.
	// Registers:
	//	r0 = scratch
	//	r1 = destroyed
	//	r2 = destroyed
	//	r3 = loop counter
	//	r4 = input VRAM pointer (DS:SI)
	//	r6 = output RAM pointer (ES:DI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
	mov		r6, r2					// r6 = output ES:DI pointer
1:	mov		r2, r4
	bl		EGA_read_byte_r2		// Returns byte in r0, destroys r1 and r2!
	sub		r4, #4
	strb	r0, [r6], #-1			// Save byte to ES:DI, decrement output pointer
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	pop		{r0, r3, r4, r6}
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

// ------------------- REP MOVS from EGA to EGA ------------------------
// ALREADY FIXED FOR ax86 USE!
//
rep_movsb_cld_EGA_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2						// Clear the EGA flag bits from the linear address
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
	// Test for current write mode
	//-------
2:	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					// r1 = Write Mode
	cmp		r0, #1					// Is it Write Mode 1?
	bne		.movsb_cld_EGA_EGA_slow	// Nope, use the slow move
	//-------
	// Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	//-------
	.global	movsb_cld_EGA_EGA_words	// Also used by "MODEX.s"!!
movsb_cld_EGA_EGA_words:
	rsbs	r0, r3, #8				// r0 = 8 - number of words to move
	blt		.movsb_cld_EGA_EGA_block	// More than 8 words to move, use the block code
	add		r0, pc, r0, lsl #3
	mov		pc, r0					// Jump to move the last 0..7 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 0: Move 8 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 1: Move 7 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 2: Move 6 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 3: Move 5 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 4: Move 4 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 5: Move 3 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 6: Move 2 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 7: Move 1 words
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							// vga.latch.d = r0
	str		r0, [r1, #-4]						// vga.latch.d = r0
.movsb_cld_EGA_EGA_done:
	pop		{r0, r3}
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
	//-------
	// Use block move when more than 8 words to move
	//-------
.movsb_cld_EGA_EGA_block:
	push	{r4-r11}
1:	mov		r0, r3, lsr #(5-2)		// r0 = number of 32-byte blocks to move
	rsbs	r0, #8					// r0 = 8 - number of 32-byte blocks to move (max 256 bytes)
	movlt	r0, #0					// If more than 8 block to move, move 8 blocks at a time (8-8 = 0)
	sub		r3, #(8*8)				// This many bytes (words) handled
	add		r0, pc, r0, lsl #3
	mov		pc, r0					// Jump to move 0..8 times 32 bytes
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 0 = 8
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 1 = 7
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 2 = 6
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 3 = 5
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 4 = 4
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 5 = 3
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 6 = 2
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 7 = 1
	cmp		r3, #8					// Still more than 32 bytes (8 words) to move?
	bge		1b						// Yep, go back to loop
	ands	r3, #7					// Still bytes to move?
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldreq	r1, =VGA_latch
	streq	r11, [r1]							// vga.latch.d = r11
	streq	r11, [r1, #-4]						// vga.latch.d = r11
	pop		{r4-r11}
	bne		movsb_cld_EGA_EGA_words
	b		.movsb_cld_EGA_EGA_done
	//-------
	// Write mode != 1: Slow copy using the subroutines.
	//-------
.movsb_cld_EGA_EGA_slow:
	push	{r4, r6, lr}
	//-------
	// Setup the registers
	//-------
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	//-------
	// First read the EGA VRAM value into r0
	//-------
1:	mov		r2, r4
	bl		EGA_read_byte_r2
	add		r4, #4
	//-------
	// Then write the r0 value into EGA VRAM
	//-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	add		r6, #4
	//-------
	// Decrease the loop counter
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Return
	//-------
	pop		{r4, r6, lr}
	b		.movsb_cld_EGA_EGA_done

// ------------------- REP MOVSB from EGA to EGA (STD) -----------------
// (Windows 3.00a: Control Panel, Alt-S for Settings)
// 
rep_movsb_std_EGA_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
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
	// Adjust the registers by the number of bytes we will store.
	// Test for current write mode
	//-------
2:	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					// r0 = Write Mode
	cmp		r0, #1					// Is it Write Mode 1?
	bne		.movsb_std_EGA_EGA_slow	// Nope, use the slow move
	//-------
	// Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	//-------
1:	ldr		r0, [r1], #-4 			// Get the value from DS:SI (EGA VRAM)
	str		r0, [r2], #-4			// Write the result to ES:DI (EGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							// vga.latch.d = r0
	str		r0, [r1, #-4]						// vga.latch.d = r0
.movsb_std_EGA_EGA_done:
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
.movsb_std_EGA_EGA_slow:
	push	{r6}
	//-------
	// Setup the registers
	//-------
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	//-------
	// First read the EGA VRAM value into r0
	//-------
1:	mov		r2, r4
	bl		EGA_read_byte_r2
	sub		r4, #4
	//-------
	// Then write the r0 value into EGA VRAM
	//-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	sub		r6, #4
	//-------
	// Decrease the loop counter
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Return
	//-------
	pop		{r6}
	b		.movsb_std_EGA_EGA_done

// ------------------- REP MOVSW ---------------------------------------
//

	//-------
	// Direction flag clear
	//-------
	.global	rep_movsw_cld_from_EGA
rep_movsw_cld_from_EGA:
	calc_ega_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_cld_EGA_RAM rep_movsw_cld_EGA_EGA .unknown_back1

	
	//-------
	// Direction flag set
	//-------
	.global	rep_movsw_std_from_EGA
rep_movsw_std_from_EGA:
	calc_ega_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsw_std_EGA_RAM rep_movsw_std_EGA_EGA .unknown_back1


rep_movsw_std_EGA_RAM:
rep_movsw_std_RAM_EGA:
rep_movsw_std_EGA_EGA:
rep_stosb_std_EGA:
rep_stosw_std_EGA:
	b		.unknown

// ------------------- REP MOVSW from RAM to EGA ----------------------
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in RAM
	//	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
	.global	rep_movsw_cld_RAM_EGA
rep_movsw_cld_RAM_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	//-------
	calc_ega_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
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
	//	r0 = scratch
	//	r1 = destroyed
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = input RAM pointer (DS:SI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
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
	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	subs	r3, #1
	bne		1b
	mov		r1, r4
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r4, r1					// r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
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
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1 
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			// r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					// r0 = flags
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
	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	subs	r3, #1
	bne		1b
	mov		r1, r4
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r4, r1					// r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
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
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			// r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	//------
	// Adjust the registers, one more halfword moved.
	//------
	pop		{r0}					// Pop saved flags
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

// ------------------- REP MOVSW from RAM to EGA (STD) -----------------
// (Windows 3.00a: Closing About window)
//

	.global	rep_movsw_std_RAM_EGA
rep_movsw_std_RAM_EGA_TODO:
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI+1 in RAM
	//	r2 = physical start address of ES:DI+1 (with EGA flags)
	//	r10 = SI register, needs to be decremented by CX*2
	//	r11 = DI register, needs to be decremented by CX*2
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with ES:DI bytes in this page
	rsb		r0, r2					// r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	// ----- Check with DS:SI bytes in this page
	mov		r0, r1, lsl #18
	cmp		r3, r0, lsr #18
	movgt	r3, r0, lsr #18
	// ----- Check with SI
	sub		r3, #1					// SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	// ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					// Restore the counter
	//-------
	// Adjust the registers by the number of bytes we will move.
	//-------
	sub		r10, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	sub		r11, r3, lsl #16		// Decrement DI by the number of bytes we shall move.
	sub		r5, r3					// r5 = number of bytes remaining
	//-------
	// Move slowly one byte at a time.
	//-------
	mov		r4, r1					// Save r1 = DS:SI into r4
1:	ldrb	r0, [r4], #-1			// r0 = byte from DS:SI, decrement input pointer
	bl		EGA_write_byte_r0_to_r2	// Write it to ES:DI. TODO! This should be speeded up!
	sub		r2, #4					// Decrement output pointer
	subs	r3, #1					// CX -= 1
	bne		1b						// Back to loop until r3 = 0
	pop		{r0, r3, r4}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_std_next		// ... else handle the next 16K page.

	.ltorg

// ------------------- REP MOVSW from EGA to EGA ------------------
// Bard's Tale:
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in EGA VRAM (with EGA flags)
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsw_cld_EGA_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2						// Clear the EGA flag bits from the linear address
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
	sub		ecx, r3, lsr #1			// ecx = number of halfwords remaining
	ror		esi, #16
	ror		edi, #16
	//-------
	// Test for current write mode, from now on similar code for USE16 and USE32 versions.
	//-------
2:	ldr		r0, =VGAModeReg
	ldrb	r0, [r0]
	and		r0, #3					// r1 = Write Mode
	cmp		r0, #1					// Is it Write Mode 1?
	bne		.movsw_cld_EGA_EGA_slow	// Nope, use the slow move
	//-------
	// Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	//-------
	.global	movsw_cld_EGA_EGA_words	// Also used by "MODEX.s"!!
movsw_cld_EGA_EGA_words:
	rsbs	r0, r3, #8				// r0 = 8 - number of words to move
	blt		.movsw_cld_EGA_EGA_block	// More than 8 words to move, use the block code
	add		r0, pc, r0, lsl #3
	mov		pc, r0					// Jump to move the last 0..7 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 0: Move 8 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 1: Move 7 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 2: Move 6 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 3: Move 5 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 4: Move 4 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 5: Move 3 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 6: Move 2 words
	ldr		r0, [r1], #4
	str		r0, [r2], #4			// r0 = 7: Move 1 words
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							// vga.latch.d = r0
	str		r0, [r1, #-4]						// vga.latch.d = r0
.movsw_cld_EGA_EGA_done:
	pop		{r0, r3}
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

	//-------
	// USE16 Uneven number of bytes to move, so need special attention.
	//-------
5:	push	{r4, r6}
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #17		// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #17		// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//-------
	// First move the full halfwords we can move
	//-------
1:	mov		r2, r4
	bl		EGA_read_hword_r2
	add		r4, #8
	mov		r2, r6
	bl		EGA_write_hword_r0_to_r2
	add		r6, #8
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r2, r4
	bl		EGA_read_byte_r2
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16				// Restore ESI
	ror		edi, #16				// Restore EDI
	pop		{r4, r6}				// Pop pushed registers
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, r0					// r1 = EGA byte read from DS:SI
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1 
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	mov		r0, r1					// r0 = EGA value to write
	bl		EGA_write_byte_r0_to_r2
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					// r0 = flags
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
	sub		ecx, r3, lsr #1			// ecx = number of halfwords remaining
	b		2b						// The rest of the code is similar to USE16 version.

	//-------
	// USE32 Uneven number of bytes to move, so need special attention.
	//-------
5:	push	{r4, r6}
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	movs	r3, r3, lsr #1			// r3 = number of full halfwords to move
	beq		5f						// Jump if no full halfwords fit here
	add		esi, r3, lsl #1			// Increment SI by the number of bytes we shall move.
	add		edi, r3, lsl #1			// Increment DI by the number of bytes we shall move..
	sub		ecx, r3					// Decrement ECX by the number of full halfwords we shall move.
	//-------
	// First move the full halfwords we can move
	//-------
1:	mov		r2, r4
	bl		EGA_read_hword_r2
	add		r4, #8
	mov		r2, r6
	bl		EGA_write_hword_r0_to_r2
	add		r6, #8
	subs	r3, #1
	bne		1b
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r2, r4
	bl		EGA_read_byte_r2
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	//------
	// Move the remaining byte to a possibly different page.
	//------
	pop		{r4, r6}				// Pop pushed registers
	pop		{r0, r3}				// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	bl		EGA_read_byte_r2
	mov		r1, r0					// r1 = EGA byte read from DS:SI
	pop		{r0}
	msr		cpsr_f,r0				// Restore flags
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1 
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	calc_ega_r2
	mov		r0, r1					// r0 = EGA value to write
	bl		EGA_write_byte_r0_to_r2
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.

	//-------
	// Use block move when more than 8 words to move
	//-------
.movsw_cld_EGA_EGA_block:
	push	{r4-r11}
1:	mov		r0, r3, lsr #(5-2)		// r0 = number of 32-byte blocks to move
	rsbs	r0, #8					// r0 = 8 - number of 32-byte blocks to move (max 256 bytes)
	movlt	r0, #0					// If more than 8 block to move, move 8 blocks at a time (8-8 = 0)
	sub		r3, #(8*8)				// This many bytes (words) handled
	add		r0, pc, r0, lsl #3
	mov		pc, r0					// Jump to move 0..8 times 32 bytes
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 0 = 8
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 1 = 7
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 2 = 6
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 3 = 5
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 4 = 4
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 5 = 3
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 6 = 2
	ldmia	r1!, {r4-r11}
    stmia   r2!, {r4-r11}			// 7 = 1
	cmp		r3, #8					// Still more than 32 bytes (8 words) to move?
	bge		1b						// Yep, go back to loop
	ands	r3, #7					// Still bytes to move?
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldreq	r1, =VGA_latch
	streq	r11, [r1]							// vga.latch.d = r11
	streq	r11, [r1, #-4]						// vga.latch.d = r11
	pop		{r4-r11}
	bne		movsw_cld_EGA_EGA_words
	b		.movsw_cld_EGA_EGA_done

	//-------
	// Write mode != 1: Slow copy using the subroutines.
	//-------
.movsw_cld_EGA_EGA_slow:
	push	{r4, r6}
	//-------
	// Setup the registers
	//-------
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	//-------
	// First read the EGA VRAM value into r0
	//-------
1:	mov		r2, r4
	bl		EGA_read_byte_r2
	add		r4, #4
	//-------
	// Then write the r0 value into EGA VRAM
	//-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	add		r6, #4
	//-------
	// Decrease the loop counter
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Return
	//-------
	pop		{r4, r6}
	b		.movsw_cld_EGA_EGA_done

	.ltorg
	
// ------------------- REP MOVSW from EGA to RAM  -----------------
// Commander Keen 4:
//	EAX=00000000 EBX=000003E4 ECX=0000314D EDX=000003CE
//	ESP=FFFFFF54 EBP=0000FF5A ESI=00000000 EDI=00000000
//	ES=5BB1 CS=02EE SS=31D5 DS=A000 FS=0000 GS=0000
//	02EE:00001D58 F3A5            repe movsw
//
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI in EGA VRAM (already adjusted to word addressing)
	//	r2 = physical start address of ES:DI in RAM
	//	r3 = address size mask (0x0000FFFF or 0xFFFFFFFF)
	//	ecx = number of bytes to store, needs to be cleared
	//	esi = SI register, needs to be incremented by CX
	//	edi = DI register, needs to be incremented by CX
	//-------
rep_movsw_cld_EGA_RAM:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6}
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can move:
	// r3 = min(CX, source_address_before_page_wrap, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx
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
	//	r0 = scratch
	//	r1 = destroyed
	//	r3 = loop counter
	//	r4 = input RAM pointer (DS:SI)
	//	r6 = output VRAM pointer (ES:DI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
	mov		r6, r2					// r6 = output ES:DI pointer
1:	mov		r2, r4	
	bl		EGA_read_byte_r2
	add		r4, #4
	strb	r0, [r6], #1
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	pop		{r0, r3, r4, r6}
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
	mov		r4, r1					// r4 = input DS:SI pointer
	mov		r6, r2					// r6 = output ES:DI pointer
1:	mov		r2, r4	
	bl		EGA_read_hword_r2
	add		r4, #8
	strb	r0, [r6], #1
	lsr		r0, #8
	strb	r0, [r6], #1
	subs	r3, #1
	bne		1b
	mov		r1, r4
	mov		r2, r6
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r6, r2					// r6 = output ES:DI pointer
	mov		r2, r1
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4, r6}		// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	calc_ega_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES 1f bad_string_op_seg_back1 bad_string_op_seg_back1 
1:	mov		r6, r2					// r6 = output ES:DI pointer
	mov		r2, r1
	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	mov		r0, #0x20000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					// r0 = flags
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
	mov		r4, r1					// r4 = input DS:SI pointer
	mov		r6, r2					// r6 = output ES:DI pointer
1:	mov		r2, r4	
	bl		EGA_read_hword_r2
	add		r4, #8
	strb	r0, [r6], #1
	lsr		r0, #8
	strb	r0, [r6], #1
	subs	r3, #1
	bne		1b
	mov		r1, r4
	mov		r2, r6
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r6, r2					// r6 = output ES:DI pointer
	mov		r2, r1
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	//------
	// Move the remaining byte to a possibly different page.
	//------
	ror		esi, #16
	ror		edi, #16
	pop		{r0, r3, r4, r6}		// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	calc_ega_r2
	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES 1f bad_string_op_seg_back1 bad_string_op_seg_back1 
1:	mov		r6, r2					// r6 = output ES:DI pointer
	mov		r2, r1
	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	bl		EGA_read_byte_r2
	strb	r0, [r6]
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #2
	add		edi, #2
	pop		{r0}					// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	b		rep_movsw_cld_next		// ... else handle the next 16K page.


// ------------------- REP MOVSW from EGA to RAM (STD) -----------------
// This is a silly and rare operation!
//
rep_movsw_std_EGA_RAM_TODO:
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI+1 in EGA VRAM
	//	r2 = physical start address of ES:DI+1 in RAM
	//	r5 = number of BYTES to move, needs to be cleared
	//	r10 = SI register, needs to be decremented by CX*2
	//	r11 = DI register, needs to be decremented by CX*2
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4, r6, lr}
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with DS:SI bytes in this page
	rsb		r0, r1					// r0 = physical DS:SI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	// ----- Check with ES:DI bytes in this page
	mov		r0, r2, lsl #18
	cmp		r3, r0, lsr #18
	movgt	r3, r0, lsr #18
	// ----- Check with SI
	sub		r3, #1					// SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	// ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					// Restore the counter
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		r10, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	sub		r11, r3, lsl #16		// Decrement DI by the number of bytes we shall move.
	sub		r5, r3					// r5 = number of bytes remaining
	mov		r4, r1					// r4 = input DS:SI pointer
	mov		r6, r2					// r6 = output ES:DI pointer
	//-------
	// Loop here for r3 input bytes.
	// Registers:
	//	r0 = scratch
	//	r1 = destroyed
	//	r2 = destroyed
	//	r3 = loop counter
	//	r4 = input VRAM pointer (DS:SI)
	//	r6 = output RAM pointer (ES:DI)
	//-------
1:	mov		r2, r4
	bl		EGA_read_byte_r2		// Returns byte in r0, destroys r1 and r2!
	sub		r4, #4
	strb	r0, [r6], #-1			// Save byte to ES:DI, decrement output pointer
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	pop		{r0, r3, r4, r6, lr}
	cmp		r5, #0
	biceq	r9, #0xFF				// Clear the r9 low halfword (saved effective segment value)
	biceq	r9, #0xFF00				// Clear the r9 low halfword (saved effective segment value)
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_movsw_std_next		// ... else handle the next 16K page.


// ------------------- REP MOVSW from EGA to EGA (STD) -----------------
//
rep_movsw_std_EGA_EGA_TODO:
	//-------
	// On input:
	//	r0 = free
	//	r1 = physical start address of DS:SI+1 in EGA VRAM
	//	r2 = physical start address of ES:DI+1 with EGA flags
	//	r5 = number of BYTES to move, needs to be cleared
	//	r10 = SI register, needs to be decremented by CX*2
	//	r11 = DI register, needs to be decremented by CX*2
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_address_before_B000_seg, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with DS:SI bytes in this page
	sub		r4, r1, r0				// r4 = physical DS:SI - EGAVGA_A000
	cmp		r3, r4, lsr #2
	movgt	r3, r4, lsr #2
	// ----- Check with ES:DI bytes in this page
	sub		r4, r2, r0				// r4 = physical ES:DI - EGAVGA_A000
	cmp		r3, r4, lsr #2
	movgt	r3, r4, lsr #2
	// ----- Check with SI
	sub		r3, #1					// SI/DI point 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r10, lsr #16
	movgt	r3, r10, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero)
	// ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					// Restore the counter
	//-------
	// Adjust the registers by the number of bytes we will store.
	// Also test for current write mode
	//-------
	ldr		r0, =VGAModeReg
	sub		r10, r3, lsl #16		// Decrement SI by the number of bytes we shall move.
	ldrb	r0, [r0]
	sub		r11, r3, lsl #16		// Decrement DI by the number of bytes we shall move.
	sub		r5, r3					// r5 = number of bytes remaining
	and		r0, #3					// r0 = Write Mode
	cmp		r0, #1					// Is it Write Mode 1?
	bne		.movsw_std_EGA_EGA_slow	// Nope, use the slow move
	//-------
	// Write mode 1: Fast copy the data within the EGA/VGA emulated VRAM
	//-------
1:	ldr		r0, [r1], #-4 			// Get the value from DS:SI (EGA VRAM)
	str		r0, [r2], #-4			// Write the result to ES:DI (EGA VRAM)
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Leave the last copied value to the EGA/VGA latch
	//-------
	ldr		r1, =VGA_latch
	str		r0, [r1]							// vga.latch.d = r0
	str		r0, [r1, #-4]						// vga.latch.d = r0
.movsw_std_EGA_EGA_done:
	pop		{r0, r3, r4}
	cmp		r5, #0
	biceq	r9, #0xFF				// Clear the r9 low halfword (saved effective segment value)
	biceq	r9, #0xFF00				// Clear the r9 low halfword (saved effective segment value)
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_movsw_std_next		// ... else handle the next 16K page.
.movsw_std_EGA_EGA_slow:
	push	{r6, lr}
	//-------
	// Setup the registers
	//-------
	mov		r4, r1					// r4 = physical DS:SI
	mov		r6, r2					// r6 = physical ES:DI
	//-------
	// First read the EGA VRAM value into r0
	//-------
1:	mov		r2, r4
	bl		EGA_read_byte_r2
	sub		r4, #4
	//-------
	// Then write the r0 value into EGA VRAM
	//-------
	mov		r2, r6
	bl		EGA_write_byte_r0_to_r2
	sub		r6, #4
	//-------
	// Decrease the loop counter
	//-------
	subs	r3, #1
	bne		1b
	//-------
	// Return
	//-------
	pop		{r6, lr}
	b		.movsw_std_EGA_EGA_done

	.ltorg
	
// ------------------- REP MOVSD ---------------------------------------
//

	//-------
	// Direction flag clear
	//-------
	.global	rep_movsd_cld_from_EGA
rep_movsd_cld_from_EGA:
	calc_ega_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_cld_EGA_RAM rep_movsd_cld_EGA_EGA .unknown_back1
	
	//-------
	// Direction flag set
	//-------
	.global	rep_movsd_std_from_EGA
rep_movsd_std_from_EGA:
	calc_ega_r1
	mov		r0, r11					// r0 = DI
	mem_handler_jump_r0r3_ES rep_movsd_std_EGA_RAM rep_movsd_std_EGA_EGA .unknown_back1

rep_movsd_cld_EGA_RAM:
rep_movsd_cld_EGA_EGA:
rep_movsd_std_EGA_RAM:
rep_movsd_std_EGA_EGA:
	b		bad_EGA_opcode

	.global	rep_movsd_cld_RAM_EGA
rep_movsd_cld_RAM_EGA:
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}			// Push used registers
	//-------
	// Calculate r2 = physical ES:DI start address, and r4 = the mask to use
	//-------
	calc_ega_r2
	//-------
	// Check whether we need USE16 or USE32 address handling
	//-------
	tst		r3, #0x80000000
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
	//	r1 = destroyed
	//	r2 = output VRAM pointer (ES:DI)
	//	r3 = loop counter
	//	r4 = input RAM pointer (DS:SI)
	//-------
2:	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	//-------
	// Back to loop if more bytes to do
	//-------
	subs	r3, #1
	bne		1b
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
	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	subs	r3, #1
	bne		1b
	mov		r1, r4
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r4, r1					// r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
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
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			// r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #2				// r0 = SI+2
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #2				// r0 = DI+2
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			// r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #3				// r0 = SI+3
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #3				// r0 = DI+3
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1			// r0 = byte from DS:SI, increment input pointer
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	//------
	// Adjust the registers, one more word moved.
	//------
	sub		ecx, #1
	mov		r0, #0x40000
	add		esi, r0, esi, ror #16
	add		edi, r0, edi, ror #16
	ror		esi, #16
	ror		edi, #16
	pop		{r0}					// r0 = flags
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
	mov		r4, r1					// r4 = input DS:SI pointer
1:	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	add		r2, #4					// Increment output pointer
	subs	r3, #1
	bne		1b
	mov		r1, r4
	//------
	// Then move the extra byte that will still fit to the same page.
	//------
5:	mov		r4, r1					// r4 = input DS:SI pointer
	ldrb	r0, [r4], #1			// r0 = byte from DS:SI, increment input pointer
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	//------
	// Move the remaining bytes to a possibly different page.
	//------
	pop		{r0, r3, r4}			// pop flags and 16-bit/32-bit mask
	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #1				// r0 = SI+1
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #1				// r0 = DI+1
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #2				// r0 = SI+2
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #2				// r0 = DI+2
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	pop		{r0}

	ldr		r2, [sp, #SP_STR_SEG]	// Get the current effective segment from stack
	msr		cpsr_f,r0				// Restore flags
	add		r0, esi, #3				// r0 = SI+3
	mem_handler_jump_r0r3 1f bad_string_op_seg_back1 bad_string_op_seg_back1
1:	mov		r1, r2					// r1 = DS:SI linear address
	add		r0, edi, #3				// r0 = DI+3
	mem_handler_jump_r0r3_ES bad_string_op_seg_back1 1f bad_string_op_seg_back1
1:	mrs		r0, cpsr				// Save flags (we are not allowed to change any)
	push	{r0}
	ldrb	r0, [r1], #1
	calc_ega_r2
	bl		EGA_write_byte_r0_to_r2 // Write it to ES:DI. TODO! This should be speeded up!
	//------
	// Adjust the registers, one more halfword moved.
	//------
	sub		ecx, #1
	add		esi, #4
	add		edi, #4
	pop		{r0}					// r0 = flags
	tst		ecx, r3					// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_movsd_cld_next		// ... else handle the next 16K page.

// ------------------- REP STOSB (CLD) ---------------------------------
//
	.global	rep_stosb_cld_EGA
rep_stosb_cld_EGA:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with EGA flags)
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
	calc_ega_r2
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
	//-------
	// Loop here to store the bytes
	//-------
1:	and		r0, eax, #0xFF
	bl		EGA_write_byte_r0_to_r2				// Saves r2!
	add		r2, #4
	subs	r3, #1
	bne		1b
	pop		{r0, r3}
	tst		ecx, r3								// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0				// Go back to the opcode loop if r5 == 0 ...
	b		rep_stosb_cld						// ... else handle the next 16K page.

// ------------------- REP STOSB (STD) ---------------------------------
// This is a silly and rare operation!
//
	.global	rep_stosb_std_EGA
rep_stosb_std_EGA_TODO:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with EGA flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of bytes to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, lr}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_address_before_B000_seg, target_address_before_B000_seg, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with ES:DI bytes in this page
	rsb		r0, r2					// r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	// ----- Check with DI
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #1					// Restore the counter
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		r11, r3, lsl #16		// Decrement DI by the number of bytes we shall store..
	sub		r5, r3					// r5 = number of bytes remaining
	//-------
	// Loop here to store the bytes
	//-------
1:	mov		r0, r4, lsr #16
	bl		EGA_write_byte_r0_to_r2	// Saves r2!
	sub		r2, #4
	subs	r3, #1
	bne		1b
	pop		{r0, r3, lr}
	cmp		r5, #0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosb_std			// ... else handle the next 16K page.

// ------------------- REP STOSW (CLD) ---------------------------------
//
	.global	rep_stosw_cld_EGA
rep_stosw_cld_EGA:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI address (with EGA flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of BYTES to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
	//-------
	// r4 = halfword value to store
	//-------
	mov		r4, eax, lsl #16
	orr		r4, r4, lsr #16			// r4 = value to store (in all 4 bytes)
	//-------
	// Test for 32bit store
	//-------
	tst		r3, #0x80000000
	bne		3f
	//-------
	// USE16: Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
	and		r3, ecx								// r3 = number of bytes/halfwords/dwords to store
	lsl		r3, #1								// Number of bytes to store = 2*CX
	ror		edi, #16
	min_cnt_idx_wrap_16 r3 edi
	min_cnt_VGA_end 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1								// Are we to store an uneven number of bytes?
	bne		.unknown							// Yes, so handle this situation specially.
	add		edi, r3, lsl #16					// Increment DI by the number of bytes we stored.
	ror		edi, #16
	sub		ecx, r3, lsr #1						// CX = number of bytes remaining
	//-------
	// Loop here to store the bytes
	//-------
2:	mov		r0, eax
	bl		EGA_write_byte_r0_to_r2				// Saves r2!
	add		r2, #4
	subs	r3, #1
	bgt		2b
	//-------
	// Return!
	//-------
	pop		{r0, r3, r4}
	tst		ecx, r3								// Set the zero flag if CX == 0 or ECX == 0
	beq		restore_flags_from_r0				// Go back to the opcode loop if r5 == 0 ...
	b		rep_stosw_cld						// ... else handle the next 16K page.
	//-------
	// USE32: Calculate the number of safe bytes we can store:
	// r3 = min(CX, target_address_before_B000_seg, bytes_until_DI_is_zero)
	//-------
3:	mov		r3, ecx, lsl #1						// r3 = number of bytes/halfwords/dwords to store
	min_cnt_VGA_end 	r3 r2					// Check with ES:DI bytes before VGA VRAM end
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	tst		r3, #1								// Are we to store an uneven number of bytes?
	bne		.unknown							// Yes, so handle this situation specially.
	add		edi, r3								// Increment EDI by the number of bytes we stored.
	sub		ecx, r3, lsr #1						// CX = number of bytes remaining
	b		2b

// ------------------- REP STOSW (STD) ---------------------------------
// This is a silly and rare operation!
//
	.global	rep_stosw_std_EGA
rep_stosw_std_EGA_TODO:
	//-------
	// Registers here:
	// r0 = free
	// r1 = free
	// r2 = physical ES:DI+1 address (with EGA flags)
	// r3 must be saved
	// r4 = AX << 16, must be saved
	// r5 = CX = number of BYTES to store, must be cleared
	// r6..r10 must be saved
	// r11 = DI = +/- (CX<<16)
	// r12, lr must be saved
	//-------
	mrs		r0,cpsr					// Save flags (we are not allowed to change any)
	push	{r0, r3, r4, lr}
	//-------
	// Calculate physical ES:DI start address
	//-------
	calc_ega_r2
	//-------
	// Calculate the number of safe bytes we can store:
	// r3 = min(CX, source_bytes_before_end_of_page, target_bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	//-------
	sub		r3, r5, #1				// Start with r3 = CX, decrease it by one for the page checks.
	// ----- Check with ES:DI bytes in this page
	rsb		r0, r2					// r0 = physical ES:DI - EGAVGA_A000
	cmp		r3, r0, lsr #2
	movgt	r3, r0, lsr #2
	// ----- Check with DI
	sub		r3, #1					// DI points 2 bytes below the counter, so now r3 = CX-2
	cmp		r3, r11, lsr #16
	movgt	r3, r11, lsr #16		// r3 = min(CX, bytes_before_end_of_page, bytes_until_SI_is_zero, bytes_until_DI_is_zero)
	add		r3, #2					// Restore the counter
	//-------
	// r4 = halfword value to store
	//-------
	orr		r4, r4, lsr #16			// r4 = value to store (in all 4 bytes)
	//-------
	// Test for odd number of bytes to store
	//-------
	tst		r5, #1					// If we wrote odd number of bytes the last time...
	rorne	r4, #8					// ... we need to swap AH and AL when continuing.
	//-------
	// Adjust the registers by the number of bytes we will store.
	//-------
	sub		r11, r3, lsl #16		// Increment DI by the number of bytes we shall store..
	sub		r5, r3					// r5 = number of bytes remaining
	//-------
	// Loop here to store the bytes
	//-------
1:	ror		r4, #8
	and		r0, r4, #0xFF			// r0 = AH value
	bl		EGA_write_byte_r0_to_r2	// Store AH, saves r2!
	sub		r2, #4
	subs	r3, #1
	beq		2f						// Jump if r3 == 0
	ror		r4, #8
	and		r0, r4, #0xFF			// r0 = AL value
	bl		EGA_write_byte_r0_to_r2	// Store AL, saves r2!
	sub		r2, #4
	subs	r3, #1
	bne		1b
	//-------
	// Return!
	//-------
2:	pop		{r0, r3, r4, lr}
	cmp		r5, #0
	beq		restore_flags_from_r0	// Go back to the opcode loop if r5 == 0 ...
	msr		cpsr_f,r0				// Restore flags
	b		rep_stosw_std			// ... else handle the next 16K page.

// ------------------- F3 AE = REPE SCASB ------------------------------
//
	.global	repe_scasb_cld_EGA
repe_scasb_cld_EGA:
	mrs		r0,cpsr								// Save flags (CX comparison must not change flags)
	//-------
	// Calculate the number of safe bytes we can scan:
	// r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	//-------
	ands	r1, r3, ecx							// Start with r1 = CX
	beq		restore_flags_from_r0				// If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						// Is this a USE32 operation?
	bne		repe_scasb_cld_EGA_USE32			// Yep, go handle it.
	ror		edi, #16
	min_cnt_VGA_end 	r1 r2					// Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r1 edi					// ----- Check with DI
	//-------
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	//-------
1:	mrs		r0,cpsr								// Save flags
	tst		r1, r1
	beq		5f									// Go check whether we need to exit the iteration or handle next 16K block
	//-------
	// 4) Perform the string operation once.
	//-------
	push	{r2}
	bl		EGA_read_byte_r2					// Get byte from physical ES:DI
	pop		{r2}
	add		edi, #0x00010000					// Increment DI
	lsl		r0, #24								// Byte to highest byte of r0
	add		r2, #4
	rsbs	r0, eax, lsl #24					// Subtract the byte from AL
	//-------
	// 5) Decrement CX, no flags are modified.
	//-------
	sub		ecx, #1								// Decrement CX
	sub		r1, #1								// Decrement 16K block counter
	//-------
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	//-------
	beq		1b
	ror		edi, #16
	b		complement_carry					// We need to swap the Carry flag!
	//-------
	// r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	//-------
5:	ror		edi, #16							// Restore EDI
6:	tst		ecx, r3								// Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					// If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				// ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							// CX > 0, so restore flags ...
	b		repe_scasb_cld						// ... and go handle the next 16K page.

repe_scasb_cld_EGA_USE32:
	//-------
	// Calculate the number of safe bytes we can scan:
	// r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	//-------
	min_cnt_VGA_end 	r1 r2					// Check with ES:DI bytes before VGA VRAM end
	//-------
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	//-------
1:	mrs		r0,cpsr								// Save flags
	tst		r1, r1
	beq		6b									// Go check whether we need to exit the iteration or handle next 16K block
	//-------
	// 4) Perform the string operation once.
	//-------
	push	{r2}
	bl		EGA_read_byte_r2					// Get byte from physical ES:DI
	pop		{r2}
	add		edi, #1								// Increment EDI
	add		r2, #4
	lsl		r0, #24								// Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					// Subtract the byte from AL
	//-------
	// 5) Decrement CX, no flags are modified.
	//-------
	sub		ecx, #1								// Decrement CX
	sub		r1, #1								// Decrement 16K block counter
	//-------
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPE and the last comparison was NOT equal.
	//-------
	beq		1b
	b		complement_carry					// We need to swap the Carry flag!


// ------------------- F2 AE = REPNE SCASB -----------------------------
//
	.global	repne_scasb_cld_EGA
repne_scasb_cld_EGA:
	mrs		r0,cpsr								// Save flags (CX comparison must not change flags)
	//-------
	// Calculate the number of safe bytes we can scan:
	// r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	//-------
	ands	r1, r3, ecx							// Start with r1 = CX
	beq		restore_flags_from_r0				// If CX == 0 at the beginning, exit immediately.
	tst		r3, #0x80000000						// Is this a USE32 operation?
	bne		repne_scasb_cld_EGA_USE32			// Yep, go handle it.
	ror		edi, #16
	min_cnt_VGA_end 	r1 r2					// Check with ES:DI bytes before VGA VRAM end
	min_cnt_idx_wrap_16 r1 edi					// ----- Check with DI
	//-------
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	//-------
1:	mrs		r0,cpsr								// Save flags
	tst		r1, r1
	beq		5f									// Go check whether we need to exit the iteration or handle next 16K block
	//-------
	// 4) Perform the string operation once.
	//-------
	push	{r2}
	bl		EGA_read_byte_r2					// Get byte from physical ES:DI
	pop		{r2}
	add		edi, #0x00010000					// Increment DI
	lsl		r0, #24								// Byte to highest byte of r0
	add		r2, #4
	rsbs	r0, eax, lsl #24					// Subtract the byte from AL
	//-------
	// 5) Decrement CX, no flags are modified.
	//-------
	sub		ecx, #1								// Decrement CX
	sub		r1, #1								// Decrement 16K block counter
	//-------
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	//-------
	bne		1b
	ror		edi, #16
	b		complement_carry					// We need to swap the Carry flag!
	//-------
	// r1 == 0, so either CX == 0 or we need to recalculate the addresses for the next page.
	//-------
5:	ror		edi, #16							// Restore EDI
6:	tst		ecx, r3								// Set the zero flag if CX == 0 or ECX == 0
	eoreq	r0, r0, #0x20000000					// If CX == 0, swap the Carry flag in r0 ...
	beq		restore_flags_from_r0				// ... and go back to the opcode loop setting flags from r0.
	msr		cpsr_f,r0							// CX > 0, so restore flags ...
	b		repne_scasb_cld						// ... and go handle the next 16K page.

repne_scasb_cld_EGA_USE32:
	//-------
	// Calculate the number of safe bytes we can scan:
	// r3 = min(CX, target_bytes_before_end_of_page, bytes_until_DI_is_zero)
	//-------
	min_cnt_VGA_end 	r1 r2					// Check with ES:DI bytes before VGA VRAM end
	//-------
	// 2) Check CX. If it is zero, exit the iteration and move to next instruction.
	//-------
1:	mrs		r0,cpsr								// Save flags
	tst		r1, r1
	beq		6b									// Go check whether we need to exit the iteration or handle next 16K block
	//-------
	// 4) Perform the string operation once.
	//-------
	push	{r2}
	bl		EGA_read_byte_r2					// Get byte from physical ES:DI
	pop		{r2}
	add		edi, #1								// Increment EDI
	add		r2, #4
	lsl		r0, #24								// Byte to highest byte of r0
	rsbs	r0, eax, lsl #24					// Subtract the byte from AL
	//-------
	// 5) Decrement CX, no flags are modified.
	//-------
	sub		ecx, #1								// Decrement CX
	sub		r1, #1								// Decrement 16K block counter
	//-------
	// 6) Check the Zero flag if the string operation is SCAS or CMPS.
	//	 Exit the iteration if the prefix was REPNE and the last comparison was equal.
	//-------
	bne		1b
	b		complement_carry					// We need to swap the Carry flag!

// ------------------- F6 = ??? r/m8 -----------------------------------
//
	.global	test_EGA_r2_imm8
test_EGA_r2_imm8:
	calc_ega_r2
	bl		EGA_read_byte_r2
	ldrb	r1, [r12], #1
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	lsl		r0, #24					// r0 = byte at address [r2], shifted to high byte
	lsl		r1, #24					// r1 = imm8 byte, shifted to high byte
	tst		r0, r1
	b		loop

// ------------------- F7 = ??? r/m16 ----------------------------------
//
	.global	test_EGA_r2_imm16
test_EGA_r2_imm16:
	calc_ega_r2
	bl		EGA_read_hword_r2
	ldrb	r1, [r12], #1
	ldrb	r2, [r12], #1
	lsl		r0, #16
	lsl		r1, #16
	orr		r1, r2, lsl #24
	msr		cpsr_f,#0				// Clear all flags (especially C and O)
	tst		r0, r1
	b		loop

// ------------------- FE = INC/DEC r/m8 -------------------------------
// INC x is like ADD x,1, except the Carry flag is not changed.
//

	.global	inc_byte_EGA_r2
inc_byte_EGA_r2:
	mrs		r3,cpsr					// Get original flags to r3.
	calc_ega_r2
	push	{r2}					// Save used registers
	bl		EGA_read_byte_r2
	lsl		r0, #24
	adds	r0, #0x01000000			// Add 1 to the high byte
	mrs		r1,cpsr					// Save new flags to r1
	lsr		r0, #24
	ldr		r2, [sp]				// Get the address from stack, ...
	str		r1, [sp]				// ... and svae the new flags there.
	bl		EGA_write_byte_r0_to_r2
	//-------
	// Fix the Carry flag
	//-------
	pop		{r1}					// r3 = original flags, r1 = new flags
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	and		r3, #0x20000000			// r0 = Only the original Carry flag bit
	orr		r0, r3, r1				// r0 = new flags + original Carry flag
	//-------
	// Return
	//-------
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0

	.global	dec_byte_EGA_r2
dec_byte_EGA_r2:
	mrs		r3,cpsr					// Get original flags to r3.
	calc_ega_r2
	push	{r2}					// Save used registers
	bl		EGA_read_byte_r2
	lsl		r0, #24
	subs	r0, #0x01000000			// Subtract 1 from the high byte
	mrs		r1,cpsr					// Save new flags to r1
	lsr		r0, #24
	ldr		r2, [sp]				// Get the address from stack, ...
	str		r1, [sp]				// ... and svae the new flags there.
	bl		EGA_write_byte_r0_to_r2
	//-------
	// Fix the Carry flag
	//-------
	pop		{r1}					// r3 = original flags, r1 = new flags
	bic		r1, #0x20000000			// r1 = New flags with Carry cleared
	and		r3, #0x20000000			// r0 = Only the original Carry flag bit
	orr		r0, r3, r1				// r0 = new flags + original Carry flag
	//-------
	// Return
	//-------
	b		restore_flags_from_r0	// Jump back to loop, setting the flags from r0


// ------------------- FF = INC/DEC/CALL/JMP/PUSH ----------------------
//
// 26FF37 push word es:[bx] (Lords of Doom)
// 26FF7702 push word es:[bx+02] (Lords of Doom)
//
	.global	push_EGA_r2
push_EGA_r2:
	mrs		r3,cpsr					// Get original flags to r3.
	calc_ega_r2
	bl		EGA_read_hword_r2
	msr		cpsr_f, r3				// Restore flags
	push_hword r0 r1 r2
	b		loop

//-------------------- Print a character to the screen -----------------
// Called by a C routine, r0 = char, r1 = color (0..16)|page<<8, r2=number of times.
// Use the cursor position from BIOSData[0x50] and ROM8x8font.
// We can change r0..r3, must save other registers.
//

.macro put_font_row
	ldr		r0, [r5, r0, lsl #2]
	and		r0, r1, r0
	str		r0, [r4]
	add		r4, r7, lsl #2
.endm

CharToEGAScreen:
	push	{r4-r8}
	//-------
	// Setup r7, r6, r1 and r2
	//-------
	ldr		r7, =VGAOffset
	mov		r6, r2					// r6 = counter
	mov		r2, r1, lsr #8			// r2 = page
	and		r1, #0xFF				// r1 = color
	ldrb	r7, [r7]				// r7 = 20 (320x) or 40 (640x)
	//-------
	// Calculate screen address to r4
	//-------
	ldr		r8,=BIOSData
	mov		r3, #0x50
	ldr		r8, [r8]
	add		r3, r2, lsl #1			// Use the cursor position of the correct page
	ldrh	r5, [r8, #0x4C]			// r5 = size of the screen page in bytes (0x2000 or 0x4000)
	ldrh	r4, [r8, r3]			// r4 low byte = column, high byte = row
	ldrb	r8, [r8, #0x85]			// r8 = character height of the current font
	mul		r2, r5, r2				// r2 = start offset of this page (in emulated bytes)
	and		r3, r4, #0xFF			// r3 = column (0..39)
	lsr		r4, #8					// r4 = row (0..24)
	lsl		r7, #1					// r7 = 40 or 80
	mul		r5, r7, r8				// r5 = 40*cheight or 80*cheight
	mul		r4, r5, r4				// r4 = 40*cheight*row or 80*cheight*row
	add		r4, r3					// r4 = column+40*cheight*row
	ldr		r3, =EGAVGA_A000
	ldr		r3, [r3]
	add		r4, r3, r4, lsl #2		// Each byte is a word in the EGA RAM
	add		r4, r2, lsl #2			// Each page is 0x2000*4 or 0x4000*4 bytes
	//-------
	// Get address to font data
	// Registers now:
	//	r0 = char to output
	//	r1 = color
	//	r2 = free
	//	r3 = free
	//	r4 = output position
	//	r5 = free
	//	r6 = counter
	//	r7 = screen offset value
	//	r8 = character height
	//-------
	ldr		r3, =INTVectors
	ldr		r3, [r3]				// Now r3 = INTVectors address
	ldr		r3, [r3, #(0x43*4)]		// Get the current INT43 (font pointer) vector address
	mov		r2, r3, lsr #16			// r2 = segment of the font pointer
	push	{r0, r1}
	lsl		r3, #16
	lsl		r2, #4
	add		r2, r3, lsr #16			// r2 = logical start address of the font
	//-------
	// calc_linear_address_r2
	//-------
	ldr		r0,=EMSPageTable
	mov		r1, r2, lsr #TLB_GRAN_BITS			// r1 = 16K page number
	ldr		r0, [r0]							// r0 = index into static TLB table
	ldr		r0, [r0, r1, lsl #2]				// r0 = physical start address of the page
	add		r3, r2, r0, lsl #4					// r2 = physical font start address
	// -----
	pop		{r0, r1}
	mul		r0, r8, r0				// r0 = char*cheight
	ldrb	r0, [r3, r0]!			// r3 = font position, r0 = first row of font
	ldr		r5, =byte2wordtbl
	//-------
	// Prepare color value
	//-------
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
	//-------
	// Write 8x8 pixels
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
	cmp		r8, #8
	beq		1f
	//-------
	// Write font rows 9-14 (EGA)
	//-------
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
	beq		1f
	//-------
	// Write font rows 15-16 (VGA)
	//-------
	ldrb	r0, [r3, #1]!
	put_font_row
	ldrb	r0, [r3, #1]!
	put_font_row
	//-------
	// One character done, test if we need to write more.
	//-------
1:	subs	r6, #1
	popeq	{r4-r8}
	bxeq	lr						// Return to caller
	sub		r3, r8					// Back to first row of font
	add		r3, #1
	mul		r0, r7, r8				// r0 = 40*cheight or 80*cheight
	sub		r4, r0, lsl #2
	add		r4, #4					// Next column
	ldrb	r0, [r3]				// Get first row of font
	b		2b

//-------------------- Draw a pixel to the screen ----------------------
// Called by a C routine, r0 = pixel color, r1 = X, r2 = Y
// We can change r0..r3, must save other registers.
//
PixelToEGAScreen:
	push	{r4-r8}
	//-------
	// Calculate the byte address of the pixel
	//-------
	ldr		r7, =VGAOffset
	ldr		r3, =EGAVGA_A000
	ldrb	r7, [r7]				// r7 = 20 (320x) or 40 (640x)
	ldr		r3, [r3]
	lsl		r7, #1
	mul		r4, r2, r7				// r4 = 40*Y or 80*Y
	add		r4, r1, lsr #3			// r4 = (X/8)+40*Y = byte in EGA RAM to change
	add		r4, r3, r4, lsl #2		// Each byte is a word in the EGA RAM
	//-------
	// Calculate the mask to use
	//-------
	and		r1, #7
	lsl		r1, #2					// r1 = 4*X
	mov		r12, #0xF
	lsl		r12, r1
	//-------
	// Prepare color value
	//-------
	tst		r0, #0x80				// Should we XOR the pixel?
	and		r0, #15					// r0 = color (0..15)
	orr		r0, r0, lsl #4
	orr		r0, r0, lsl #8
	orr		r0, r0, lsl #16
	and		r0, r12
	//-------
	// Write the color to EGA VRAM
	//-------
	ldr		r2, [r4]
	biceq	r2, r12
	orreq	r2, r0
	eorne	r2, r0
	str		r2, [r4]
	pop		{r4-r8}
	bx		lr

//-------------------- Read a pixel from the screen --------------------
// Called by a C routine, r0 = X, r1 = Y
// We can change r0..r3, must save other registers.
//
PixelFromEGAScreen:
	push	{r4-r8}
	//-------
	// Calculate the byte address of the pixel
	//-------
	ldr		r7, =VGAOffset
	ldr		r3, =EGAVGA_A000
	ldrb	r7, [r7]				// r7 = 20 (320x) or 40 (640x)
	ldr		r3, [r3]
	lsl		r7, #1
	mul		r4, r1, r7				// r4 = 40*Y or 80*Y
	add		r4, r0, lsr #3			// r4 = (X/8)+40*Y = byte in EGA RAM to change
	add		r4, r3, r4, lsl #2		// Each byte is a word in the EGA RAM
	//-------
	// Calculate the mask to use
	//-------
	and		r1, r0, #7
	lsl		r1, #2					// r1 = 4*X
	mov		r12, #0xF
	lsl		r12, r1
	//-------
	// Get the color from EGA VRAM
	//-------
	ldr		r0, [r4]
	and		r0, r12
	lsr		r0, r1
	pop		{r4-r8}
	bx		lr

	.ltorg
	
	//=================== EGA/VGA/MCGA PORTS ===========================


	//-------
	// Write to port 3C0 from a C routine:
	//	Out3C0(int reg_idx, int value);
	//-------
	.global	Out3C0
Out3C0:
	ldr		r2, =VGA_attr_data_3C0
	strb	r0, [r2, #-1]			// Set the current address/flipflop value
	//-------
	// VGA Attribute Controller Registers.
	//-------
out_3C0_VGA_attr:
	ldr		r2, =VGA_attr_data_3C0
	ldrsb	r0, [r2, #-1]			// Get the current address/flipflop value
	cmp		r0, #0					// Are we to write the new register address value?
	blt		.out_3C0_address		// Yep, go write the address
	//-------
	// We are to write the register data
	//-------
#if (REG_DEBUG&1)
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	debugreg r1
	and r1, #0xFF
#endif
	cmp		r0, #0x10				// Is this an EGA palette register write?
	orr		r0, #0x80
	strb	r0, [r2, #-1]			// Save the new address/flipflop value
	bic		r0, #0x80				// Restore r0 value
	beq		.out_3C0_mode_control	// Jump if this was a mode control register change
	bgt		.out_3C0_other			// Jump if other attribute registers
	strb	r1, [r2, r0]			// Save the new data
	//-------
	// Change an EGA palette register value 
	//-------
	ldr		r2, =BIOSData
	ldr		r2, [r2]
	ldrb	r2, [r2, #0x49]					// Get current screen mode 
	cmp		r2, #0x13						// Mode 13?
	bxge	lr								// Yep, we can ignore the palette change
	//-------
	// VGA_ATTR_SetPalette(Bit8u index,Bit8u val)
	// Input:
	// 	r0 = index = palette register number, 0..F
	// 	r1 = val = register value
	//	lr = return address
	// Destroys:
	//	r0, r1, r2
	// Palette register change in EGA mode, so we need to copy the
	// palette value from the current EGA palette table (at 0x05000000+512+32).
	// - Fixes Terminator 2 - Judgment Day
	//
	// Handle the palette blocks (16x16 or 4x64)
	// - Fixes SimFarm
	// if (vga.attr.mode_control & 0x80) val = (val&0xf) | (vga.attr.color_select << 4);
	// val &= 63;
	// val |= (vga.attr.color_select & 0xc) << 4;
	//-------
VGA_ATTR_SetPalette:
	ldr		r2, =VGA_attr_data_3C0
	orr		r1, r0, lsl #24			// Combine r1 and r0 to r1
	ldrb	r0, [r2, #0x10]			// r0 = vga.attr.mode_control
	tst		r0, #0x80				// if (vga.attr.mode_control & 0x80)
	ldrb	r0, [r2, #0x14]
	bicne	r1, #0xFF0
	orrne	r1, r0, lsl #4			//	val = (val&0xf) | (vga.attr.color_select << 4);
	and		r2, r0, #0xC
	mov		r0, r1, lsr #24
	and		r1, #63					// val &= 63;
	orr		r1, r2, lsl #4			// val |= (vga.attr.color_select & 0xc) << 4;
	//-------
	// VGA_DAC_CombineColor(index,val);
	//-------
	ldr		r2, =VGA_dac_combine
	strb	r1, [r2, r0]
	//-------
	// static void VGA_DAC_SendColor( Bitu index, Bitu src )
	// Input:
	//	r0 = index
	//	r1 = src
	//	lr = return address
	// Destroys:
	//	r0, r1, r2
	//-------
	.global VGA_DAC_SendColor
VGA_DAC_SendColor:
	ldr		r2, =BG_PALETTE
#ifdef RPi
	add		r0, r2, r0, lsl #1		// r0 = *(BG_PALETTE[index]) = where we need to copy the value to
#else	
	add		r0, r2, r0, lsl #2		// r0 = *(BG_PALETTE[index]) = where we need to copy the value to
#endif
	add		r2, #256*4				// r2 points to EGA_PALETTE_SAVE
#ifdef RPi
	lsl		r1, #1
	ldrh	r1, [r2, r1]			// r1 = Palette value to use (src)
	sub		r2, #(2*256)
	strh	r1, [r0]				// BG_PALETTE[pal_idx] = pal_color
	mov		r1, #1
	strh	r1, [r2]				// Flag that the palette has changed
#else	
	ldr		r1, [r2, r1, lsl #2]	// r1 = Palette value to use (src)
	str		r1, [r0]				// BG_PALETTE[pal_idx] = pal_color
#endif	
	bx		lr						// return
	
.out_3C0_mode_control:
	add		r2, r0
	ldrb	r0, [r2]				// Get the original value to r0
	strb	r1, [r2]				// Save the new value 
	eor		r0, r1
	tst		r0, #0x80				// Did the highest bit change?
	bxeq	lr						// Nope, just return
	//-------
	// Internal Palette Size bit changed!
	// We need to re-send all the 16 colors.
	//-------
	mov		r0, #0
1:	ldr		r2, =VGA_attr_data_3C0
	push	{r0, lr}
	ldrb	r1, [r2, r0]			// r1 = vga.attr.palette[i]
	bl		VGA_ATTR_SetPalette
	pop		{r0, lr}
	add		r0, #1
	cmp		r0, #16
	blt		1b
	bx		lr
.out_3C0_other:
#if 0
	cmp		r0, #0x13				// VGAPixelPanning ?
	bne		1f
	push	{r0-r3, r9, r12, lr}
	mov		r0, #6					// VSYNC_PPANN		6
	bl		vsynclog
	pop		{r0-r3, r9, r12, lr}
1:
#endif
	cmp		r0, #0x14				// Color Select Register?
	strneb	r1, [r2, r0]			// Nope, just save the new data
	bxne	lr						// Nope, just return
	add		r2, r0
	ldrb	r0, [r2]				// Get the original value to r0
	strb	r1, [r2]				// Save the new value 
	eors	r0, r1					// Did the value change?
	bxeq	lr						// Nope, just return
	//-------
	// Color Select Register changed!
	// We need to re-send all the 16 colors.
	//-------
	mov		r0, #0
	b		1b
	
	//-------
	// We are to write the register address
	//-------
.out_3C0_address:
	and		r1, #0x1F				// only use the lower 5 bits
	cmp		r1, #0x14				// We have at most 0x14 registers
	movgt	r1, #0x14
	strb	r1, [r2, #-1]			// Save the new address/flipflop value
	bx		lr
in_3C0_VGA_attr:
	bic		eax, #0xFF				// Clear AL (copied from DOSBox)
	bx		lr
in_3C1_VGA_attr:
	ldr		r2, =VGA_attr_data_3C0
	ldrb	r0, [r2, #-1]			// Get the current address/flipflop value
	and		r0, #0x1F
	ldrb	r1, [r2, r0]
	bic		eax, #0xFF				// Clear AL first
	orr		eax, r1					// Set AL value from r1
	bx		lr
	
	//-------
	// VGA Miscellaneous Output Register.
	//	bit 7 = Vertical Sync Polarity
	//	bit 6 = Horizontal Sync Polarity
	//	bit 5 = Page Bit for Odd/Even
	//	bit 4 = Disable Video Drivers (EGA)
	//	bit 3-2 = Clock Select
	//	bit 1 = Enable RAM
	//	bit 0 = I/O Address (0=mono=3BX, 1=color=3DX)
	//-------
out_3C2_VGA_misc:
	ldr		r2, =VGA_misc_3C2
	strb	r1, [r2]				// Save the VGA Misc register value
	//-------
	// Screen height might have changed, call the C routine.
	//-------
#ifdef IOS
	push	{r3, r9, r12, lr}
	bl		_ScreenConfigChanged	// Call the C routine to update the screen configuration
	pop		{r3, r9, r12, pc}
#else
	push	{r3, r12, lr}
	bl		ScreenConfigChanged		// Call the C routine to update the screen configuration
	pop		{r3, r12, pc}
#endif	
in_3CC_VGA_misc:
	ldr		r2, =VGA_misc_3C2
	ldrb	r1, [r2]				// r1 = VGA Misc Register value
	bic		eax, #0xFF				// Clear AL first
	orr		eax, r1					// Set AL value from r1
	bx		lr

	//-------
	// VGA Sequencer Registers.
	// 	- index 0 = Reset
	//	- index 1 = Clocking Mode
	//	- index 2 = Map Mask
	//	- index 3 = Character Map Select
	//	- index 4 = Memory Mode
	//-------
out_3C4_VGA_sequencer:
	ldr		r2, =VGA_Sequencer_addr_3C4
	strb	r1, [r2]
	bx		lr
in_3C4_VGA_sequencer:
	ldr		r2, =VGA_Sequencer_addr_3C4
	ldrb	r1, [r2]
	bic		eax, #0xFF				// Clear AL first
	orr		eax, r1					// Set AL value from r1
	bx		lr
	
out_3C5_VGA_sequencer:
	ldr		r2, =VGA_Sequencer_regs_3C5
	ldrb	r0, [r2, #-1]
#if (REG_DEBUG&2)
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	orr	r1, #0x0C50000
	debugreg r1
	and r1, #0xFF
#endif
	cmp		r0, #4
	strleb	r1, [r2, r0]					// Store the new Sequencer Register value
	beq		.out_3C5_MemoryMode				// Jump if it is the MemoryMode register
	cmp		r0, #2							// Is it the Map Mask register?
	bxne	lr								// Nope, just return
	//-------
	// Map Mask register value changed.
	//-------
	//-------
	// Calculate new emulated EGA write mask value
	//-------
	cmp		r1, #15
	andgt	r1, #15
	strgtb	r1, [r2, r0]					// Store again the new Sequencer Register value (anded with 15)
	mov		r0, r1
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
#ifdef IOS
	ldr		r2, =EGA_WRITE_MASK32
	str		r1, [r2]
	//-------
	// Calculate new emulated Mode-X write mask value
	//-------
	ldr		r1, =ModeXMaskTbl
	ldr		r0, [r1, r0, lsl #2]
	ldr		r2, =MODEX_WRITE_MASK32
	str		r0, [r2]
#else
	str		r1, [r2, #(EGA_WRITE_MASK32-VGA_Sequencer_regs_3C5)]
	//-------
	// Calculate new emulated Mode-X write mask value
	//-------
	add		r1, r2, #(ModeXMaskTbl-VGA_Sequencer_regs_3C5)
	ldr		r0, [r1, r0, lsl #2]
	str		r0, [r2, #(MODEX_WRITE_MASK32-VGA_Sequencer_regs_3C5)]
#endif
	bx		lr
	//-------
	// Memory Mode register value changed, check if this was entry to Mode-X (or exit from it).
	//-------
.out_3C5_MemoryMode:
	ldr		r2, =BIOSData
	ldr		r2, [r2]
	ldrb	r2, [r2, #0x49]					// Get current screen mode 
	cmp		r2, #0x13						// Mode 13?
	bxne	lr								// Nope, we can ignore the write
	//-------
	// Memory Mode register changed while in mode 0x13, possible Mode-X entry/exit!
	//-------
#ifdef IOS
	push	{r3, r9, r12, lr}
	bl		_ScreenConfigChanged			// Call the C routine to update the screen configuration
	pop		{r3, r9, r12, pc}
#else
	push	{r3, r12, lr}
	bl		ScreenConfigChanged				// Call the C routine to update the screen configuration
	pop		{r3, r12, pc}
#endif
	//-------
	// VGA Sequencer Registers.
	// 	- index 0 = Reset
	//	- index 1 = Clocking Mode
	//	- index 2 = Map Mask
	//	- index 3 = Character Map Select
	//	- index 4 = Memory Mode
	//-------
in_3C5_VGA_sequencer:
	ldr		r2, =VGA_Sequencer_addr_3C4
	ldrb	r1, [r2]						// r1 = Sequencer register index
	add		r1, #1							// Point to VGA_Sequencer_regs_3C5
	ldrb	r1, [r2, r1]					// Get the Sequencer Register value into r1
	and		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr

	//-------
	// VGA PEL Address Read Mode Register
	//	- pal_idx = AL
	//	- pal_rgb = 0
	//	- pal_color = 0
	//-------
out_3C7_VGA_pel:
	ldr		r2, =VGA_palette_read
	lsl		r1, #24							// pal_idx = AL, pal_rgb=0, pal_color=0
	str		r1, [r2]						// Save VGA_palette read word
	add		r1, #0x01000000
#ifdef IOS
	ldr		r2, =VGA_palette_write
	str		r1, [r2]						// Save VGA_palette write word (Star Control 2)
#else
	str		r1, [r2, #(VGA_palette_write-VGA_palette_read)]	// Save VGA_palette write word (Star Control 2)
#endif
	mov		r1, #0
#ifdef IOS
	ldr		r2, =VGA_palette_3C7
	strb	r1, [r2]				// Tell write mode is in effect
#else
	strb	r1, [r2, #(VGA_palette_3C7-VGA_palette_read)]	// Tell read mode is in effect
#endif
	bx		lr

	//-------
	// VGA DAC State Register
	//-------
in_3C7_VGA_DAC:
	ldr		r2, =VGA_palette_3C7
	ldrb	r1, [r2]						// r1 = Sequencer register index
	bic		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr

	//-------
	// VGA PEL Address Write Mode Register
	//	- pal_idx = AL
	//	- pal_rgb = 0
	//	- pal_color = 0
	//-------
out_3C8_VGA_pel:
	ldr		r2, =VGA_palette_write
	lsl		r1, #24					// pal_idx = AL, pal_rgb=0, pal_color=0
	str		r1, [r2]				// Save VGA_palette data word
	mov		r1, #3
#ifdef IOS
	ldr		r2, =VGA_palette_3C7
	strb	r1, [r2]				// Tell write mode is in effect
#else
	strb	r1, [r2, #(VGA_palette_3C7-VGA_palette_write)]	// Tell write mode is in effect
#endif
	bx		lr

	//-------
	// VGA PEL Address Write Mode Register
	//-------
in_3C8_VGA_pel:
	ldr		r2, =VGA_palette_write
	ldr		r1, [r2]							// Get VGA_palette data word
	lsr		r1, #24
	bic		eax, #0xFF							// Clear AL first
	orr		eax, r1								// Set AL value from r1
	bx		lr

	//-------
	// 0x3C9 = VGA PEL Data Register (used to set palette data)
	//		switch( pal_rgb )
	//		{
	//			case 0:	// Write the Red component
	//				pal_color = (AL>>1)<<11;
	//				pal_rgb++;
	//				return true;
	//			case 1:	// Write the Green component
	//				pal_color |= ((AL>>1)<<6);
	//				pal_rgb++;
	//				return true;
	//			case 2:	// Write the Blue component, and increment index
	//				pal_color |= (AL>>1);
	//				BG_PALETTE[pal_idx++] = pal_color;
	//				pal_idx &= 0xFF;
	//				pal_rgb = 0;
	//				pal_color = 0;
	//				return true;
	//		}
	//		break;
	//-------
out_3C9_VGA_pel:
	ldr		r2, =VGA_palette_write
	lsr		r1, #1					// r1 = AL>>1
	ldr		r0, [r2]				// Get VGA_palette data word
	and		r1, #0x1F				// r1 = new palette data, 0..31
	and		r3, r0, #0x00030000		// Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r1, r1, lsl #11			// Red channel if r3 == 0
	moveq	r1, r1, lsl #6			// Green channel if r3 == 1
	orr		r0, r1					// pal_color |= (AL >> 1) << pal_rgb
	add		r0, #0x00010000			// pal_rgb++
	strle	r0, [r2]				// All done if not yet Blue component handled
	bxle	lr
	//-------
	// case 2: Store the new palette value
	//-------
	ldr		r3, =BG_PALETTE
#ifdef RPi
	add		r1, r3, r0, lsr #(24-1)	// r1 = BG_PALETTE[pal_idx]
#else	
	add		r1, r3, r0, lsr #(24-2)	// r1 = BG_PALETTE[pal_idx]
#endif	
	strh	r0, [r1]				// BG_PALETTE[pal_idx] = pal_color
	add		r0, #0x01000000			// pal_idx++
	and		r0, #0xFF000000			// pal_rgb = 0, pal_color = 0
	str		r0, [r2]
#ifdef RPi
	add		r3, #(2*256)
	mov		r0, #1
	strh	r0, [r3]				// Flag that the palette has changed
#endif	
	bx		lr

	//-------
	// Routine for EGA mode, all the higher palette registers need to be
	// copies of the first 16 palette registers.
	//-------
out_3C9_EGA_pel:
	ldr		r2, =VGA_palette_write	// Assume these are VGA palette registers
	lsr		r1, #1					// r1 = AL>>1
	ldr		r0, [r2]				// Get VGA_palette data word
	and		r1, #0x1F				// Fix for Prehistorik etc.
	and		r3, r0, #0x00030000		// Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r1, r1, lsl #11			// Red channel if r3 == 0
	moveq	r1, r1, lsl #6			// Green channel if r3 == 1
	orr		r0, r1					// pal_color |= (AL >> 1) << pal_rgb
	add		r0, #0x00010000			// pal_rgb++
	strle	r0, [r2]				// Save VGA_palette data word
	bxle	lr
	//-------
	// case 2: Store the new palette value. We need to store it to the
	// EGA palette save area, and also to the currently
	// active palette index, if the attribute registers have this index
	// mapped to an active EGA color.
	// First store it to the save area.
	//-------
	ldr		r1, =EGA_PALETTE_SAVE
	mov		r3, r0, lsr #24			// r3 = palette register index		
#ifdef RPi
	lsl		r3, #1
#else
	lsl		r3, #2
#endif	
	strh	r0, [r1, r3]			// Indexed as words instead of halfwords
	//-------
	// Check for attributes and DAC entry link
	// for (Bitu i=0;i<16;i++) {
	//	if (vga.dac.combine[i]==vga.dac.write_index) {
	//		VGA_DAC_SendColor( i, vga.dac.write_index );
	//	}
	// }
	//-------
#ifdef IOS
	ldr		r2, =VGA_dac_combine
#else
	add		r2, #(VGA_dac_combine - VGA_palette_write)
#endif
	mov		r3, #15
1:	ldrb	r1, [r2, r3]
	cmp		r1, r0, lsr #24			// Is this the correct palette index?
	beq		2f						// Yep, use r3 as the index
	subs	r3, #1					// Go to next attribute register
	bge		1b						// Back to loop until all 16 checked
	//-------
	// The palette register we updated is not used by the attribute registers,
	// so skip the hardware palette update.
	//-------
#ifdef IOS
	ldr		r2, =VGA_palette_write
#else
	sub		r2, #(VGA_dac_combine - VGA_palette_write)
#endif
	add		r0, #0x01000000			// pal_idx++
	and		r0, #0xFF000000			// pal_rgb = 0, pal_color = 0
	str		r0, [r2]
	bx		lr
	//-------
	// Found the correct hardware palette register index.
	// Call VGA_DAC_SendColor( i, vga.dac.write_index );
	//-------
2:	mov		r1, r0, lsr #24			// r1 = vga.dac.write_index
#ifdef IOS
	ldr		r2, =VGA_palette_write
#else
	sub		r2, #(VGA_dac_combine - VGA_palette_write)
#endif
	add		r0, #0x01000000			// pal_idx++
	and		r0, #0xFF000000			// pal_rgb = 0, pal_color = 0
	str		r0, [r2]
	mov		r0, r3					// r0 = i
	b		VGA_DAC_SendColor

	//-------
	// 0x3C9 = VGA PEL Data Register (used to get palette data)
	//	BG_PALETTE = ((u16*)0x05000000)
	//		switch( pal_rgb )
	//		{
	//			case 0:
	//				pal_rgb++;
	//				return (BG_PALETTE[pal_idx]&0x1F)<<1;
	//			case 1:
	//				pal_rgb++;
	//				return ((BG_PALETTE[pal_idx]>>5)&0x1F)<<1;
	//			case 2:
	//				{
	//					int tmp = ((BG_PALETTE[pal_idx]>>10)&0x1F)<<1;
	//					pal_idx++;
	//					pal_idx &= 0xFF;
	//					pal_rgb = 0;
	//					return tmp;
	//				}
	//		}
	//		break;
	//-------
in_3C9_VGA_pel:	
	ldr		r2, =VGA_palette_read	// Get address of pal_idx, pal_rgb, pal_color
	ldr		r3, =BG_PALETTE			// r3 = *(BG_PALETTE[0])
	ldr		r1, [r2]				// r1 = pal_idx, pal_rgb, pal_color		0xFB0A0000
#ifdef RPi
	add		r3, r1, lsr #(24-1)
	ldrh	r0, [r3]				// r0 = BG_PALETTE[pal_idx]
#else	
	ldr		r0, [r3, r1, lsr #(24-2)] // r0 = BG_PALETTE[pal_idx]
#endif
	and		r3, r1, #0x00030000		// Which of the color channels we should access?
	cmp		r3, #0x00010000
	movlt	r0, r0, lsr #11			// Red channel if r3 == 0
	moveq	r0, r0, lsr #6			// Green channel if r3 == 1
	and		r0, #0x1F
	bic		eax, #0xFF				// Clear AL first
	orr		eax, r0, lsl #1			// AL = ((BG_PALETTE[pal_idx]>>pal_rgb)&0x1F)<<1;

	add		r1, #0x00010000			// pal_rgb++
	addgt	r1, #0x01000000			// If already Blue component, pal_idx++
	bicgt	r1, #0x00FF0000			// If already Blue component, pal_rgb = 0
	str		r1, [r2]				// Save VGA_palette data word
	bx		lr


	//-------
	// VGA Graphics Controller Registers.
	// 	- index 0 = Set/Reset
	//	- index 1 = Enable Set/Reset
	//	- index 2 = Color Compare
	//	- index 3 = Data Rotate
	//	- index 4 = Read Map Select
	//	- index 5 = Mode
	//	- index 6 = Miscellaneous
	//	- index 7 = Color Dont Care
	//	- index 8 = Bit Mask
	//-------
out_3CE_VGA_graphics:
	ldr		r2, =VGA_Graphics_addr_3CE
	strb	r1, [r2]
.bxlr:
	bx		lr
	//-------
	// Write to port 3CF from a C routine:
	//	Out3CF(int reg_idx, int value);
	//-------
	.global	Out3CF
Out3CF:
	ldr		r2, =VGA_Graphics_regs_3CF
	strb	r0, [r2, #-1]			// Set the current address/flipflop value
out_3CF_VGA_graphics:
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r0, [r2, #-1]
#if (REG_DEBUG&4)
	orr r1, r0, lsl #8
	orr	r1, #0x3C00000
	orr	r1, #0x0CF0000
	debugreg r1
	and r1, #0xFF
#endif
	cmp		r0, #8
	strleb	r1, [r2, r0]					// Store the new Graphics Register value
	bxgt	lr								// Return if an invalid index
	ldr		pc,[pc, r0, lsl #2]				// Jump to the special handler of the register index
	.word	0								// Dummy word to align the table to PC+8
	.word 	.set_reset, .enable_set_reset, .color_compare, .bxlr, .read_map_select, .bxlr, .bxlr, .color_dont_care, .bit_mask
	//-------
	// Set/Reset register value changed
	// Index 0 = Set/Reset, bits 0..3
	//-------
.set_reset:
#ifdef IOS
	ldr		r2, =EGA_SET_RESET32
#endif
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
#ifdef IOS
	str		r1, [r2]
#else
	str		r1, [r2, #(EGA_SET_RESET32-VGA_Graphics_regs_3CF)]
#endif
	bx		lr
	//-------
	// Snable Set/Reset register value changed
	// Index 1 = Enable Set/Reset, bits 0..3
	//-------
.enable_set_reset:
#ifdef IOS
	ldr		r2, =EGA_ENABLE_SET_RESET32
#endif
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
#ifdef IOS
	str		r1, [r2]
#else
	str		r1, [r2, #(EGA_ENABLE_SET_RESET32-VGA_Graphics_regs_3CF)]
#endif
	bx		lr
	//-------
	// Color Compare register value changed
	// Index 2 = Color Compare, bits 0..3
	//-------
.color_compare:
#ifdef IOS
	ldr		r2, =EGA_COLOR_COMPARE32
#endif
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
#ifdef IOS
	str		r1, [r2]
#else
	str		r1, [r2, #(EGA_COLOR_COMPARE32-VGA_Graphics_regs_3CF)]
#endif
	bx		lr
	//-------
	// Read Map Select register value changed
	// Index 4 = Read Map Select, bits 0..1
	//-------
.read_map_select:
#ifdef IOS
	ldr		r2, =EGA_READ_MASK32
#endif
	and		r1, #3
	ldr		r0, =0x11111111
	mov		r0, r0, lsl r1
#ifdef IOS
	str		r0, [r2]
#else
	str		r0, [r2, #(EGA_READ_MASK32-VGA_Graphics_regs_3CF)]
#endif
	bx		lr
	
.mode:
	//debugreg r1
	bx		lr
	
	//-------
	// Color Dont Care register value changed
	// Index 7 = Color Dont Care
	//-------
.color_dont_care:
#ifdef IOS
	ldr		r2, =EGA_COLOR_DONT_CARE32
#endif
	and		r1, #15
	orr		r1, r1, lsl #4
	orr		r1, r1, lsl #8
	orr		r1, r1, lsl #16
#ifdef IOS
	str		r1, [r2]
#else
	str		r1, [r2, #(EGA_COLOR_DONT_CARE32-VGA_Graphics_regs_3CF)]
#endif
	bx		lr
	//-------
	// Bit Mask register value changed
	// Index 8 = Bit Mask Register, bits 0..7 (= which bits are modified during write)
	//-------
.bit_mask:
#ifdef IOS
	ldr		r2, =byte2wordtbl
	ldr		r1, [r2, r1, lsl #2]			// r1 = Bit Mask adjusted from Byte to Word
	ldr		r2, =EGA_BIT_MASK32
	str		r1, [r2]
#else
	add		r2, #(byte2wordtbl-VGA_Graphics_regs_3CF)
	ldr		r1, [r2, r1, lsl #2]			// r1 = Bit Mask adjusted from Byte to Word
	str		r1, [r2, #(EGA_BIT_MASK32-byte2wordtbl)]
#endif
	bx		lr

in_3CE_VGA_graphics:
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r1, [r2, #-1]
	bic		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr

in_3CF_VGA_graphics:
	ldr		r2, =VGA_Graphics_regs_3CF
	ldrb	r1, [r2, #-1]
	ldrb	r1, [r2, r1]					// Get the CRTC Register value into r1
	bic		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr

	
	//-------
	// 0x3D4 = VGA CRTC Address Register
	//-------
out_3D4_VGA_CRTC_addr:
	ldr		r2, =VGA_CRTC_addr_3D4
	strb	r1, [r2]
	bx		lr

in_3D4_VGA_CRTC_addr:
	ldr		r2, =VGA_CRTC_addr_3D4
	ldrb	r1, [r2]
	bic		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr

	//-------
	// 0x3D5 = VGA CRTC Data Register
	//-------
out_3D5_VGA_CRTC_data:
	ldr		r2, =VGA_CRTC_data_3D5
	ldrb	r0, [r2, #-1]					// Which of the 24 registers we are to change?
	cmp		r0, #0x18
	strleb	r1, [r2, r0]					// Store the new CRTC data value
#if 0
	cmp		r0, #0x0C						// VGAStartAddrLow?
	cmpne	r0, #0x0D						// VGAStartAddrHigh?
	bne		1f
	push	{r0-r3, r9, r12, lr}
	sub		r0, #(0x0C-0x04)				// VSYNC_ADDRL		4
	bl		vsynclog
	pop		{r0-r3, r9, r12, lr}
1:
#endif
	cmp		r0, #1							// Is it index 1?
	cmpne	r0, #9							// or index 9?
	bxne	lr								// Nope, just return
	//-------
	// Screen config might have changed, call the C routine.
	//-------
#ifdef IOS
	push	{r3, r9, r12, lr}
	bl		_ScreenConfigChanged			// Call the C routine to update the screen configuration
	pop		{r3, r9, r12, pc}
#else
	push	{r3, r12, lr}
	bl		ScreenConfigChanged				// Call the C routine to update the screen configuration
	pop		{r3, r12, pc}
#endif

in_3D5_VGA_CRTC_data:
	ldr		r2, =VGA_CRTC_data_3D5
	ldrb	r1, [r2, #-1]					// Which of the 24 registers we are to change?
	ldrb	r1, [r2, r1]					// Get the CRTC Register value into r1
	bic		eax, #0xFF						// Clear AL first
	orr		eax, r1							// Set AL value from r1
	bx		lr


	.ltorg
	
// ------------------- COPY TO SCREEN ----------------------------------
//
#define	SCREEN_WIDTH	1024

	//-------
	// Copy a 320x200x16 EGA mode graphics screen to physical screen.
	// Each pixel consists of a bit in four different bit planes.
	// Each page occupies 8000 bytes (0x2000) in the A000 memory segment.
	//-------
	.global screen_copy_0D
screen_copy_0D:
	push    {r4-r12, lr}
#ifdef RPi
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #SCREEN_WIDTH			// render.pitch
	mov		r5, r0						// render.imgtop
	mov		r6, #200					// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
#endif	
	ldr		r2,=VGAStartAddrHigh
	ldr		r1,=EGAVGA_A000			// VGA screen start
	mov		r10, r0					// r10 = input parameter value = s_pixels
	ldr		r1, [r1]
	//-------
	// Calculate the display pitch value (vga.draw.address_add in DOSBox)
	// from CRTC registers index 0x13
	//-------
#ifdef IOS
	ldr		r4, =VGAOffset
	ldrb	r4, [r4]
#else
	ldrb	r4, [r2, #(VGAOffset-VGAStartAddrHigh)]
#endif
	lsl		r4, #4
	sub		r11, r4, #320
	//-------
	// Calculate Line Compare value
	//-------
#ifdef IOS
	ldr		r5, =VGAMaxScanLine
	ldr		r7, =VGA_CRTC_data_3D5
	ldrb	r5, [r5]
	ldrb	r6, [r7, #0x07]			// r6 = VGA Overflow Register value
	ldrb	r7, [r7, #0x18]			// r7 = VGA Line Compare Register value
#else
	ldrb	r5, [r2, #(VGAMaxScanLine-VGAStartAddrHigh)]			// r5 = VGA Max Scanline Register value
	ldrb	r6, [r2, #(VGA_CRTC_data_3D5+0x07-VGAStartAddrHigh)]	// r6 = VGA Overflow Register value
	ldrb	r7, [r2, #(VGA_CRTC_data_3D5+0x18-VGAStartAddrHigh)]	// r7 = VGA Line Compare Register value
#endif
	tst		r6, #0x10				// Line Compare bit 8 on?
	orrne	r7, #0x100				// Yep, update Line Compare Register result value
	tst		r5, #0x40				// Line Compare bit 9 on?
	orrne	r7, #0x200				// Yep, update Line Compare Register result value
#ifdef RPi
	mov		lr, r7, lsl #10			// lr = 1024 * Line Compare Value
#else
	mov		lr, r7, lsl #(10+1)		// lr = 1024*2 * Line Compare Value
#endif	
	//-------
	// Calculate the input start position
	//-------
	ldrb	r0, [r2]				// VGAStartAddrHigh
	ldrb	r3, [r2, #1]			// VGAStartAddrLo
	add		r12, r1, #0x40000		// r12 = End address of the logical EGA/VGA VRAM
	orr		r0, r3, r0, lsl #8
	add		r1, r0, lsl #2
#ifdef IOS
	ldr		r3, =VGAPixelPanning
	ldrb	r3, [r3]
#else
	ldrb	r3, [r2, #(VGAPixelPanning-VGAStartAddrHigh)]
#endif
	mov		r0, r10					// Physical VRAM start
	and		r10, r3, #7				// Crystal Caves sets PixelPanning to 0x10!
	lsl		r10, #2					// Ech pixel panning step shifts the values by 4 pixels
	lsr		lr, #1					// Adjust the Line Compare value for double-scanning
	add		lr, r0					// lr = Address where we need to rewind back to the EGAVGA_A000 address
#ifdef RPi
	add		r2, r0, #(SCREEN_WIDTH*200)	// r2 = output ending address
#else
	add		r2, r0, #(SCREEN_WIDTH*200)*2	// r2 = output ending address
#endif	
	//-------
	// Loop here to do 320x200 pixels
	// Registers:
	//	r0 = Output VRAM address (s_pixels input parameter)
	//	r1 = Emulated EGA/VGA RAM address (EGAVGA_A000+StartAddress...)
	//	r2 = End position in physical s_pixels output area
	//	r3 = BG_PALETTE lookup table address
	//	r4-r9 = Scratch registers
	//	r10 = Loop counter (highest byte) | pixel panning value (lowest byte)
	//	r11 = display pitch value
	//	r12 = End address of the EGAVGA_A000 area (EGAVGA_A000+0x40000)
	//	lr = Line compare address
	//-------
5:
#ifndef RPi
	ldr		r3, =BG_PALETTE			// Palette lookup table address
#endif	
	orr		r10, #39<<24			// Copy 40*8 = 320 pixels per row

51:	ldr		r4, [r1], #4			// Get 4 bytes (8 pixels) from input

	tst		r10, #0xFF				// Is pixel panning active?
	ldrne	r5, [r1]
	movne	r4, r4, lsr r10
	rsbne	r6, r10, #32
	orrne	r4, r5, lsl r6

	and		r5, r4, #0xF
	and		r7, r4, #0xF0
	and		r6, r4, #0xF00
	and		r8, r4, #0xF000
#ifdef RPi
	orr		r5, r7, lsl #4
	orr		r5, r6, lsl #8
	orr		r5, r8, lsl #12
	and		r7, r4, #0xF0000
	and		r9, r4, #0xF00000
	and		r8, r4, #0xF000000
	and		r4, r4, #0xF0000000
	mov		r6, r7, lsr #16
	orr		r6, r9, lsr #12
	orr		r6, r8, lsr #8
	orr		r6, r4, lsr #4

	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r6}			// Write 2 words = 8 bytes = 8 pixels	
#else	
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsr #(4-2)]
	ldr		r6, [r3, r6, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(12-2)]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16
	and		r7, r4, #0xF0000
	and		r9, r4, #0xF00000
	and		r8, r4, #0xF000000
	and		r4, r4, #0xF0000000
	ldr		r7, [r3, r7, lsr #(16-2)]
	ldr		r9, [r3, r9, lsr #(20-2)]
	ldr		r8, [r3, r8, lsr #(24-2)]
	ldr		r4, [r3, r4, lsr #(28-2)]
	orr		r7, r9, lsl #16
	orr		r8, r4, lsl #16

	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r8}			// Write 4 words = 16 bytes = 8 pixels	
#endif

	subs	r10, #1<<24				// Decrement the counter
	bge		51b
	add		r10, #1<<24				// Make r10 positive again
	
	//-------
	// We are at an "interesting" location in target VRAM.
	// Adjust the pointer to the start of the next row.
	//-------
#ifdef RPi
6:	add		r0, #(SCREEN_WIDTH-320)	// Point r0 to the next row in physical VRAM
#else	
6:	add		r0, #(SCREEN_WIDTH-320)*2	// Point r0 to the next row in physical VRAM
#endif
	cmp		r0, lr					// Are we at or over Line Compare line?
	bge		9f						// Yep, handle split screen stuff
	add 	r1, r11, lsr #1			// Add vga.draw.address_add to go to the start of the next row
	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block?
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.
	//-------
	// All done?
	//-------
3:	cmp		r0, r2					// All done?
    blt    	5b						// Not yet, back to loop
	pop   	{r4-r12, pc}			// Return to caller
	//-------
	// We are at Line Compare address, continue copying from the start of the EGA_VGA_A000 area
	//-------
9:	ldr		r5, =VGAModeControl
	ldr		r1, =EGAVGA_A000		// VGA screen start
	ldrb	r4, [r5]
	orr		lr, #0x0F000000			// No more Line Compare needed
	ldr		r1, [r1]
	//-------
	// If VGAModeControl register value & 0x20 is on, we must ignore the VGAPixelPanning
	// value after we have jumped back to the start of the VGA VRAM.
	//-------
	tst		r4, #0x20
	bicne 	r10, #0xFF
	b		3b


	//-------
	// Copy a 640x(200/350/480)x16 VGA mode graphics screen to physical screen.
	// Each pixel consists of a bit in four different bit planes.
	//-------
	.global screen_copy_640
screen_copy_640:
	push    {r4-r12, lr}
#ifdef RPi
	//-------
	// Setup the RenderData structure
	//-------
	mov		r4, #SCREEN_WIDTH			// render.pitch
	mov		r5, r0						// render.imgtop
	mov		r6, #200					// render.toprows
	mov		r7, #0						// render.imgbot
	mov		r8, #0						// render.botrows
	stmia	r1, {r4-r8}					// Save to renderdata
	mov		r9, r1						// Save renderdata pointer to r9
#endif	
	ldr		r2,=VGAStartAddrHigh
	ldr		r1,=EGAVGA_A000			// VGA screen start
	mov		r10, r0					// r10 = input parameter value = s_pixels
	ldr		r1, [r1]
	//-------
	// Calculate the display pitch value (vga.draw.address_add in DOSBox)
	// from CRTC registers index 0x13
	//-------
#ifdef IOS
	ldr		r11, =VGAOffset
	ldrb	r11, [r11]
#else
	ldrb	r11, [r2, #(VGAOffset-VGAStartAddrHigh)]
#endif
	lsl		r11, #4
	sub		r11, #640
	//-------
	// Calculate Line Compare value
	//-------
#ifdef IOS
	ldr		r5, =VGAMaxScanLine
	ldr		r7, =VGA_CRTC_data_3D5
	ldrb	r5, [r5]
	ldrb	r6, [r7, #0x07]			// r6 = VGA Overflow Register value
	ldrb	r7, [r7, #0x18]			// r7 = VGA Line Compare Register value
#else
	ldrb	r5, [r2, #(VGAMaxScanLine-VGAStartAddrHigh)]			// r5 = VGA Max Scanline Register value
	ldrb	r6, [r2, #(VGA_CRTC_data_3D5+0x07-VGAStartAddrHigh)]	// r6 = VGA Overflow Register value
	ldrb	r7, [r2, #(VGA_CRTC_data_3D5+0x18-VGAStartAddrHigh)]	// r7 = VGA Line Compare Register value
#endif
	tst		r6, #0x10				// Line Compare bit 8 on?
	orrne	r7, #0x100				// Yep, update Line Compare Register result value
	tst		r5, #0x40				// Line Compare bit 9 on?
	orrne	r7, #0x200				// Yep, update Line Compare Register result value
#ifdef RPi
	mov		lr, r7, lsl #10			// lr = 1024 * Line Compare Value
#else
	mov		lr, r7, lsl #(10+1)		// lr = 1024*2 * Line Compare Value
#endif	
	//-------
	// Calculate the input start position
	//-------
	ldrb	r0, [r2]				// VGAStartAddrHigh
	ldrb	r3, [r2, #1]			// VGAStartAddrLo
	add		r12, r1, #0x40000		// r12 = End address of the logical EGA/VGA VRAM
	orr		r0, r3, r0, lsl #8
	add		r1, r0, lsl #2
#ifdef IOS
	ldr		r3, =VGAPixelPanning
	ldrb	r3, [r3]
#else
	ldrb	r3, [r2, #(VGAPixelPanning-VGAStartAddrHigh)]
#endif
	mov		r0, r10					// Physical VRAM start
	and		r10, r3, #7				// Crystal Caves sets PixelPanning to 0x10!
	lsl		r10, #2					// Ech pixel panning step shifts the values by 4 pixels
	//-------
	// Check the screen resolution we are to copy
	//-------
	ldr		r3, =VGA_misc_3C2		// Check the current VSync polarity
	ldr		r4, =VGAMaxScanLine		// Check whether we do line doubling
	ldrb	r3, [r3]				// r3 = VGA Misc Register value
	ldrb	r5, [r4]				// r5 = VGA Maximum Scan Line register value
	and		r3, #0xC0
	cmp		r3, #0xC0				// Is it 480/240 lines?
#ifdef RPi
	movne	r8, #(SCREEN_WIDTH*200)	// Nope, copy 200 lines to VRAM
	movne	r3, #200
	moveq	r8, #(SCREEN_WIDTH*240)	// Yep, copy 240 lines to VRAM
	moveq	r3, #240
#else
	movne	r8, #(SCREEN_WIDTH*200)*2	// Nope, copy 200 lines to VRAM
	moveq	r8, #(SCREEN_WIDTH*240)*2	// Yep, copy 240 lines to VRAM
#endif	
	tst		r5, #1					// Character cell height = 0 (normal) or 1 (double)?
	lsleq	r8, #1					// Normal, copy 400 / 480 lines to VRAM
#ifdef RPi
	lsleq	r3, #1
	str		r3, [r9, #8]			// Save the screen height to renderdata
#endif	
	add		r2, r0, r8				// r2 = VRAM output ending address
	lsrne	lr, #1					// Adjust the Line Compare value for double-scanning
	add		lr, r0					// lr = Address where we need to rewind back to the EGAVGA_A000 address
	//-------
	// Loop here to do 640x??? pixels
	// Registers:
	//	r0 = Output VRAM address (s_pixels input parameter)
	//	r1 = Emulated EGA/VGA RAM address (EGAVGA_A000+StartAddress...)
	//	r2 = End position in physical s_pixels output area
	//	r3 = BG_PALETTE lookup table address
	//	r4-r9 = Scratch registers
	//	r10 = Loop counter (highest byte) | pixel panning value (lowest byte)
	//	r11 = display pitch value
	//	r12 = End address of the EGAVGA_A000 area (EGAVGA_A000+0x40000)
	//	lr = Line compare address
	//-------
5:
#ifndef RPi
	ldr		r3, =BG_PALETTE			// Palette lookup table address
#endif	
	orr		r10, #79<<24			// Copy 80*8 = 640 pixels per row

51:	ldr		r4, [r1], #4			// Get 4 bytes (8 pixels) from input

//	tst		r10, #0xFF				// Is pixel panning active?
//	ldrne	r5, [r1]
//	movne	r4, r4, lsr r10
//	rsbne	r6, r10, #32
//	orrne	r4, r5, lsl r6

	and		r5, r4, #0xF
	and		r7, r4, #0xF0
	and		r6, r4, #0xF00
	and		r8, r4, #0xF000
#ifdef RPi
	orr		r5, r7, lsl #4
	orr		r5, r6, lsl #8
	orr		r5, r8, lsl #12
	and		r7, r4, #0xF0000
	and		r9, r4, #0xF00000
	and		r8, r4, #0xF000000
	and		r4, r4, #0xF0000000
	mov		r6, r7, lsr #16
	orr		r6, r9, lsr #12
	orr		r6, r8, lsr #8
	orr		r6, r4, lsr #4

	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r6}			// Write 2 words = 8 bytes = 8 pixels	
#else	
	ldr		r5, [r3, r5, lsl #2]
	ldr		r7, [r3, r7, lsr #(4-2)]
	ldr		r6, [r3, r6, lsr #(8-2)]
	ldr		r8, [r3, r8, lsr #(12-2)]
	orr		r5, r7, lsl #16
	orr		r6, r8, lsl #16
	and		r7, r4, #0xF0000
	and		r9, r4, #0xF00000
	and		r8, r4, #0xF000000
	and		r4, r4, #0xF0000000
	ldr		r7, [r3, r7, lsr #(16-2)]
	ldr		r9, [r3, r9, lsr #(20-2)]
	ldr		r8, [r3, r8, lsr #(24-2)]
	ldr		r4, [r3, r4, lsr #(28-2)]
	orr		r7, r9, lsl #16
	orr		r8, r4, lsl #16

	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block? (KEEN4E)
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.

	stmia	r0!, {r5-r8}			// Write 4 words = 16 bytes = 8 pixels	
#endif
	subs	r10, #1<<24				// Decrement the counter
	bge		51b
	add		r10, #1<<24				// Make r10 positive again
	
	//-------
	// We are at an "interesting" location in target VRAM.
	// Adjust the pointer to the start of the next row.
	//-------
#ifdef RPi
6:	add		r0, #(SCREEN_WIDTH-640)		// Point r0 to the next row in physical VRAM
#else
6:	add		r0, #(SCREEN_WIDTH-640)*2	// Point r0 to the next row in physical VRAM
#endif
	cmp		r0, lr					// Are we at or over Line Compare line?
	bge		9f						// Yep, handle split screen stuff
	add 	r1, r11, lsr #1			// Add vga.draw.address_add to go to the start of the next row
	cmp		r1, r12					// Did we go over the EGA/VGA A000 segment block?
	subge	r1, #0x40000			// Wrap back to start of the logical VRAM if we went over the end.
	//-------
	// All done?
	//-------
3:	cmp		r0, r2					// All done?
    blt    	5b						// Not yet, back to loop
	pop   	{r4-r12, pc}			// Return to caller
	//-------
	// We are at Line Compare address, continue copying from the start of the EGA_VGA_A000 area
	//-------
9:	ldr		r5, =VGAModeControl
	ldr		r1, =EGAVGA_A000		// VGA screen start
	ldrb	r4, [r5]
	orr		lr, #0x0F000000			// No more Line Compare needed
	ldr		r1, [r1]
	//-------
	// If VGAModeControl register value & 0x20 is on, we must ignore the VGAPixelPanning
	// value after we have jumped back to the start of the VGA VRAM.
	//-------
	tst		r4, #0x20
	bicne 	r10, #0xFF
	b		3b

	

	.ltorg

// ------------------- DATA VALUES -------------------------------------
//

	.data
	.align	2

	.global	VGA_serialize_start	// For serialization!
VGA_serialize_start:

	.global	VGA_draw_address
VGA_draw_address:
	.word	0
VGA_debug_addr:
	.word	0
VGA_latch_minus_12:				// Used in 4-byte read-modify-write operations
	.word	0
VGA_latch_minus_8:				// Used in 4-byte read-modify-write operations
	.word	0
VGA_latch_minus_4:				// Used in 2-byte read-modify-write operations
	.word	0
VGA_latch:
	.word	0
VGA_palette_read:
	.word	0
VGA_palette_write:
	.word	0
	.global	EGA_READ_MASK32
EGA_READ_MASK32:
	.word	0xFFFFFFFF
	.global	EGA_WRITE_MASK32
EGA_WRITE_MASK32:
	.word	0xFFFFFFFF
EGA_BIT_MASK32:
	.word	0xFFFFFFFF
EGA_COLOR_COMPARE32:
	.word	0
EGA_COLOR_DONT_CARE32:
	.word	0
EGA_SET_RESET32:
	.word	0
EGA_ENABLE_SET_RESET32:
	.word	0
	.global	MODEX_WRITE_MASK32
MODEX_WRITE_MASK32:
	.word	0xFFFFFFFF

	.global	VGA_pel_mask_3C6
VGA_pel_mask_3C6:				// VGA PEL Mask Register
	.byte	0xFF
	
	.global	VGA_misc_3C2
VGA_misc_3C2:					// VGA Misc Register, 3C2 = write, 3CC = read
	.byte	1
VGA_palette_3C7:				// DAC State Register: 0 = read is in effect, 3 = write is in effect.
	.byte	0
	.global	VGA_attr_addr_3C0
VGA_attr_addr_3C0:
	.byte	0					// Address/flipflop, -1 if the next write is to address, else 0..0x14
	.global	VGA_attr_data_3C0
VGA_attr_data_3C0:
	.byte	0					// 0 = EGA palette #0
	.byte	0					// 1 = EGA palette #1
	.byte	0					// 2 = EGA palette #2
	.byte	0					// 3 = EGA palette #3
	.byte	0					// 4 = EGA palette #4
	.byte	0					// 5 = EGA palette #5
	.byte	0					// 6 = EGA palette #6
	.byte	0					// 7 = EGA palette #7
	.byte	0					// 8 = EGA palette #8
	.byte	0					// 9 = EGA palette #9
	.byte	0					// A = EGA palette #A
	.byte	0					// B = EGA palette #B
	.byte	0					// C = EGA palette #C
	.byte	0					// D = EGA palette #D
	.byte	0					// E = EGA palette #E
	.byte	0					// F = EGA palette #F
VGAModeControl:	
	.byte	0					// 0x10 = Mode Control
	.byte	0					// 0x11 = Overscan Color
	.byte	0					// 0x12 = Color Plane Enable
	.global	VGAPixelPanning
VGAPixelPanning:
	.byte	0					// 0x13 = Horizontal Pixel Panning
	.byte	0					// 0x14 = Color Select (VGA only)

VGA_dac_combine:
	.byte	0					// 0 = EGA palette #0
	.byte	0					// 1 = EGA palette #1
	.byte	0					// 2 = EGA palette #2
	.byte	0					// 3 = EGA palette #3
	.byte	0					// 4 = EGA palette #4
	.byte	0					// 5 = EGA palette #5
	.byte	0					// 6 = EGA palette #6
	.byte	0					// 7 = EGA palette #7
	.byte	0					// 8 = EGA palette #8
	.byte	0					// 9 = EGA palette #9
	.byte	0					// A = EGA palette #A
	.byte	0					// B = EGA palette #B
	.byte	0					// C = EGA palette #C
	.byte	0					// D = EGA palette #D
	.byte	0					// E = EGA palette #E
	.byte	0					// F = EGA palette #F
	
VGA_Sequencer_addr_3C4:
	.byte	0
	.global	VGA_Sequencer_regs_3C5
VGA_Sequencer_regs_3C5:
	.byte	3					// 0 = Reset Register, bit 0 = Asynchronous Reset, bit 1 = Synchronous Reset
	.byte	0b00001110			// 1 = Clocking Mode Register
	.global VGAMapMask
VGAMapMask:	
	.byte	0xF					// 2 = Map Mask Register
	.byte	0					// 3 = Character Map Select Register
	.global	VGAMemoryMode
VGAMemoryMode:	
	.byte	0b00001100			// 4 = Memory Mode Register, bit 3 = Chain4, bit 2 = Odd/Even, bit 1 = Extended memory, bit 0 = Alpha/Graphics
	
VGA_Graphics_addr_3CE:
	.byte	0
	.global	VGA_Graphics_regs_3CF
VGA_Graphics_regs_3CF:
	.byte	0					// 0 = Set/Reset Register, bits 0..3
	.byte	0					// 1 = Enable Set/Reset, bits 0..3
	.byte	0					// 2 = Color Compare, bits 0..3
VGAFuncReg:
	.byte	0					// 3 = Data Rotate, bits 0..2 = Rotate Count, bits 3..4 = MOV,AND,OR,XOR
	.global	VGAReadMap
VGAReadMap:	
	.byte	0					// 4 = Read Map Select, bits 0..1
	.global VGAModeReg
VGAModeReg:
	.byte	0					// 5 = Mode Register, bits 6..5 = Shift Register, bit 4 = Odd/Even, bit 3 = Read Mode, bit 2 = 0, bits 1..0 = Write Mode
	.byte	0					// 6 = Misc Register, bits 3..2 = Memory Map, bit 1 = Chain Odd/Even, bit 0 = Graphics / Alpha mode
	.byte	0					// 7 = Color Dont Care register, bits 0..3
	.global VGABitMask
VGABitMask:
	.byte	0xFF				// 8 = Bit Mask Register, bits 0..7 (= which bits are modified during write)

VGA_CRTC_addr_3D4:
	.byte	0
	.global	VGA_CRTC_data_3D5
VGA_CRTC_data_3D5:
	.byte	0					// 0 = Horizontal Total
	.byte	0					// 1 = Horizontal Display End
	.byte	0					// 2 = Start Horizontal Blank
	.byte	0					// 3 = End Horizontal Blank
	.byte	0					// 4 = Start Horizontal Retrace
	.byte	0					// 5 = End Horizontal Retrace
	.byte	0					// 6 = Vertical Total
	.byte	0					// 7 = Overflow
	.byte	0					// 8 = Preset Row Scan
	.global	VGAMaxScanLine
VGAMaxScanLine:	
	.byte	0					// 9 = Max Scan Line
	.byte	0					// A = Cursor Start
	.byte	0					// B = Cursor End
	.global VGAStartAddrHigh
VGAStartAddrHigh:
	.byte	0					// C = Start Address High
	.global VGAStartAddrLow
VGAStartAddrLow:
	.byte	0					// D = Start Address Low
	.byte	0					// E = Cursor Location High
	.byte	0					// F = Cursor Location Low
	.byte	0					// 10 = Vertical Retrace Start
	.byte	0					// 11 = Vertical Retrace Low
	.byte	0					// 12 = Vertical Display End
	.global VGAOffset
VGAOffset:	
	.byte	0x14				// 13 = Offset
	.byte	0					// 14 = Underline Location
	.byte	0					// 15 = Start Vertical Blank
	.byte	0					// 16 = End Vertical Blank
	.byte	0					// 17 = Mode Control
	.byte	0					// 18 = Line Compare

	.align	2

	.global	VGA_serialize_end	// Needs to be last VGA data, for serialization!
VGA_serialize_end:
	// This is for Mode-X Write Mask calculation
ModeXMaskTbl:
	.word	0x00000000
	.word	0x000000FF
	.word	0x0000FF00
	.word	0x0000FFFF
	.word	0x00FF0000
	.word	0x00FF00FF
	.word	0x00FFFF00
	.word	0x00FFFFFF
	.word	0xFF000000
	.word	0xFF0000FF
	.word	0xFF00FF00
	.word	0xFF00FFFF
	.word	0xFFFF0000
	.word	0xFFFF00FF
	.word	0xFFFFFF00
	.word	0xFFFFFFFF

.macro b2w1 val
	.word	((\val>>7)*0xF)|(((\val>>6)&1)*0xF0)|(((\val>>5)&1)*0xF00)|(((\val>>4)&1)*0xF000)|(((\val>>3)&1)*0xF0000)|(((\val>>2)&1)*0xF00000)|(((\val>>1)&1)*0xF000000)|(((\val)&1)*0xF0000000)
	//.print 	"((\val>>7)*0xF)|(((\val>>3)&1)*0xF0)|(((\val>>6)&1)*0xF00)|(((\val>>2)&1)*0xF000)|(((\val>>5)&1)*0xF0000)|(((\val>>1)&1)*0xF00000)|(((\val>>4)&1)*0xF000000)|(((\val)&1)*0xF0000000)"
.endm
.macro b2w8 val
	b2w1 \val
	b2w1 (\val+1)
	b2w1 (\val+2)
	b2w1 (\val+3)
	b2w1 (\val+4)
	b2w1 (\val+5)
	b2w1 (\val+6)
	b2w1 (\val+7)
.endm

byte2wordtbl:
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

cursor2spritetbl:
	// For AND mask (screen)
	.word	0x02020202
	.word	0x00020202
	.word	0x02000202
	.word	0x00000202
	.word	0x02020002
	.word	0x00020002
	.word	0x02000002
	.word	0x00000002
	.word	0x02020200
	.word	0x00020200
	.word	0x02000200
	.word	0x00000200
	.word	0x02020000
	.word	0x00020000
	.word	0x02000000
	.word	0x00000000
	// For XOR mask (cursor)
	.word	0
	.word	0x01000000
	.word	0x00010000
	.word	0x01010000
	.word	0x00000100
	.word	0x01000100
	.word	0x00010100
	.word	0x01010100
	.word	0x00000001
	.word	0x01000001
	.word	0x00010001
	.word	0x01010001
	.word	0x00000101
	.word	0x01000101
	.word	0x00010101
	.word	0x01010101

	//-------
	// Table for mapping EGA palette register values to real palette values, in RGBI Color mode
	//	bit5 = ?
	//	bit4 = Intensity
	//	bit3 = ?
	//	bit2 = Red
	//	bit1 = Green
	//	bit0 = Blue
	//-------
#define  RGB15(r, g, b)   (((r)<<11)|((g)<<6)|(b))

	.global	EGAPaletteTable
EGAPaletteTable:																								  // Brown adjusted!
#ifdef RPi
	.short	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,12,0),  RGB15(21,21,21)
	.short	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,12,0),  RGB15(21,21,21)
	// 0x10
	.short	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	.short	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	// 0x20
	.short	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,21,0),  RGB15(21,21,21)
	.short	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,21,0),  RGB15(21,21,21)
	// 0x30
	.short	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	.short	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
#else	
	.word	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,12,0),  RGB15(21,21,21)
	.word	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,12,0),  RGB15(21,21,21)
	// 0x10
	.word	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	.word	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	// 0x20
	.word	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,21,0),  RGB15(21,21,21)
	.word	0,               RGB15(0,0,21),   RGB15(0,21,0),   RGB15(0,21,21),  RGB15(21,0,0),   RGB15(21,0,21),  RGB15(21,21,0),  RGB15(21,21,21)
	// 0x30
	.word	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
	.word	RGB15(12,12,12), RGB15(12,12,31),   RGB15(12,31,12),   RGB15(12,31,31),  RGB15(31,12,12),   RGB15(31,12,31),  RGB15(31,31,12),  RGB15(31,31,31)
#endif

	.global	VideoParameterTable
VideoParameterTable:
	// video parameter table for mode 00h = T  40x25  8x8   320x200  16gray    8   B800 CGA,PCjr,Tandy
    .byte   40, 24, 8, 0x00, 0x08 // bios data (columns, rows, pixels/char, page length)
    .byte   0x08, 0x03, 0x00, 0x07 // sequencer registers
    .byte   0x67 // misc output registers
    .byte   0x2d, 0x27, 0x28, 0x90, 0x2b, 0xa0, 0xbf, 0x1f       // crtc registers 0-7
    .byte   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
    .byte   0x9c, 0x8e, 0x8f, 0x14, 0x1f, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
    .byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
    .byte   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
    .byte   0x0c, 0x00, 0x0f, 0x08 // attr registers 16-19
    .byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff // graphics registers 0-8
	// video parameter table for mode 01h = T  40x25  8x8   320x200   16       8   B800 CGA,PCjr,Tandy
	.byte   40, 24, 8, 0x00, 0x08 // bios data (columns, rows, pixels/char, page length)
	.byte   0x08, 0x03, 0x00, 0x07 // sequencer registers
	.byte   0x67 // misc output registers
	.byte   0x2d, 0x27, 0x28, 0x90, 0x2b, 0xa0, 0xbf, 0x1f       // crtc registers 0-7
	.byte   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0x9c, 0x8e, 0x8f, 0x14, 0x1f, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
	.byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
	.byte   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
	.byte   0x0c, 0x00, 0x0f, 0x08 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 02h = T  80x25  8x8   640x200  16gray    4   B800 CGA,PCjr,Tandy
	.byte   80, 24, 8, 0x00, 0x10 // bios data (columns, rows, pixels/char, page length)
	.byte   0x00, 0x03, 0x00, 0x07 // sequencer registers
	.byte   0x67 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       // crtc registers 0-7
	.byte   0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0x9c, 0x8e, 0x8f, 0x28, 0x1f, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
	.byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
	.byte   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
	.byte   0x0c, 0x00, 0x0f, 0x08 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 03h = T  80x25  8x8   640x200   16       4   B800 CGA,PCjr,Tandy
	.byte	80, 24, 8, 0x00, 0x10 // bios data (columns, rows, pixels/char, page length)
	.byte	0x00, 0x03, 0x00, 0x07 // sequencer registers
	.byte	0x67 // misc output registers
	.byte	0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       // crtc registers 0-7
	.byte	0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte	0x9c, 0x8e, 0x8f, 0x28, 0x1f, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
	.byte	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
	.byte	0x0c, 0x00, 0x0f, 0x08 // attr registers 16-19
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 04h = G  40x25  8x8   320x200    4       .   B800 CGA,PCjr,EGA,MCGA,VGA
	.byte	40, 24, 8, 0x00, 0x40 // bios data (columns, rows, pixels/char, page length)
	.byte	0x09, 0x00, 0x00, 0x02 // sequencer registers
	.byte	0x63 // misc output registers
	.byte	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       // crtc registers 0-7
	.byte	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xa2, 0xff // crtc registers 16-24
	.byte	0x00, 0x13, 0x15, 0x17, 0x02, 0x04, 0x06, 0x07       // attr registers 0-7
	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       // attr registers 8-15
	.byte	0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x0f, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 05h = G  40x25  8x8   320x200   4gray    .   B800 CGA,PCjr,EGA
	.byte	40, 24, 0x08, 0x00, 0x40 // bios data
	.byte	0x09, 0x00, 0x00, 0x02 // sequencer registers
	.byte	0x63 // misc output registers
	.byte	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       // crtc registers 0-7
	.byte	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
  	.byte	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xa2, 0xff // crtc registers 16-24
  	.byte	0x00, 0x13, 0x15, 0x17, 0x02, 0x04, 0x06, 0x07       // attr registers 0-7
  	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       // attr registers 8-15
  	.byte	0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
  	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x0f, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 06h = G  80x25  8x8   640x200    2       .   B800 CGA,PCjr,EGA,MCGA,VGA
  	.byte	80, 24, 0x08, 0x00, 0x40 // bios data
  	.byte	0x09, 0x0f, 0x00, 0x02 // sequencer registers
  	.byte	0x63 // misc output registers
  	.byte	0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       // crtc registers 0-7
  	.byte	0x00, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
  	.byte	0x9c, 0x8e, 0x8f, 0x28, 0x00, 0x96, 0xb9, 0xc2, 0xff // crtc registers 16-24
  	.byte	0x00, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17       // attr registers 0-7
  	.byte	0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17       // attr registers 8-15
  	.byte	0x01, 0x00, 0x01, 0x00 // attr registers 16-19
  	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0f, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 07h = T  80x25  9x14  720x350  mono     var  B000 MDA,Hercules,EGA
  	.byte	80, 24, 0x10, 0x00, 0x10 // bios data
  	.byte	0x00, 0x0f, 0x00, 0x07 // sequencer registers
  	.byte	0x66 // misc output registers
  	.byte	0x5f, 0x4f, 0x50, 0x82, 0x55, 0x81, 0xbf, 0x1f       // crtc registers 0-7
  	.byte	0x00, 0x4f, 0x0d, 0x0e, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
  	.byte	0x9c, 0x8e, 0x8f, 0x28, 0x0f, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
  	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
  	.byte	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
  	.byte	0x0c, 0x00, 0x0f, 0x08 // attr registers 16-19
  	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0a, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 8
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
// video parameter table for mode 9
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
// video parameter table for mode a
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
// video parameter table for mode b
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
// video parameter table for mode c
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
// video parameter table for mode 0Dh = G  40x25  8x8   320x200   16       8   A000 EGA,VGA
  	.byte	40, 24, 8, 0x00, 0x20 // bios data
  	.byte	0x09, 0x0f, 0x00, 0x02 // sequencer registers
  	.byte	0x63 // misc output registers
  	.byte	0x2d, 0x27, 0x28, 0x90, 0x2b, 0x80, 0xbf, 0x1f       // crtc registers 0-7
  	.byte	0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
  	.byte	0x9c, 0x8e, 0x8f, 0x14, 0x00, 0x96, 0xb9, 0xe3, 0xff // crtc registers 16-24
  	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       // attr registers 0-7
  	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       // attr registers 8-15
  	.byte	0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
  	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 0Eh = G  80x25  8x8   640x200   16       4   A000 EGA,VGA
  	.byte	80, 24, 8, 0x00, 0x40 // bios data
  	.byte	0x01, 0x0f, 0x00, 0x02 // sequencer registers
  	.byte	0x63 // misc output registers
  	.byte	0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       // crtc registers 0-7
  	.byte	0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
  	.byte	0x9c, 0x8e, 0x8f, 0x28, 0x00, 0x96, 0xb9, 0xe3, 0xff // crtc registers 16-24
  	.byte	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       // attr registers 0-7
  	.byte	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17       // attr registers 8-15
  	.byte	0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
  	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 0Fh = G  80x25  8x14  640x350  mono      2   A000 EGA,VGA
	.byte   80, 24, 14, 0x00, 0x80 // bios data (columns, rows, pixels/char, page length)
	.byte   0x01, 0x0f, 0x00, 0x02 // sequencer registers
	.byte   0xa2 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       // crtc registers 0-7
	.byte   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0x83, 0x85, 0x5d, 0x28, 0x0f, 0x63, 0xba, 0xe3, 0xff // crtc registers 16-24
	.byte   0x00, 0x08, 0x00, 0x00, 0x18, 0x18, 0x00, 0x00       // attr registers 0-7
	.byte   0x00, 0x08, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00       // attr registers 8-15
	.byte   0x0b, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8  	
// video parameter table for mode 10 = G  80x25  8x14  640x350  16color    2   A000 EGA,VGA
	.byte   80, 24, 14, 0x00, 0x80 // bios data (columns, rows, pixels/char, page length)
	.byte   0x01, 0x0f, 0x00, 0x02 // sequencer registers
	.byte   0xa3 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       // crtc registers 0-7
	.byte   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0x83, 0x85, 0x5d, 0x28, 0x0f, 0x63, 0xba, 0xe3, 0xff // crtc registers 16-24
	.byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
	.byte   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
	.byte   0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 11h = G  80x30  8x16  640x480  mono      .   A000 VGA,MCGA,ATI EGA,ATI VIP
	.byte   80, 29, 16, 0x00, 0xa0 // bios data (columns, rows, pixels/char, page length)
	.byte   0x01, 0x0f, 0x00, 0x02 // sequencer registers
	.byte   0xe3 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0x0b, 0x3e       // crtc registers 0-7
	.byte   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0xea, 0x8c, 0xdf, 0x28, 0x00, 0xe7, 0x04, 0xc3, 0xff // crtc registers 16-24
	.byte   0x00, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f       // attr registers 0-7
	.byte   0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f, 0x3f       // attr registers 8-15
	.byte   0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 12h = G  80x30  8x16  640x480   16/256K  .   A000 VGA,ATI VIP
	.byte   80, 29, 16, 0x00, 0xa0 // bios data (columns, rows, pixels/char, page length)
	.byte   0x01, 0x0f, 0x00, 0x02 // sequencer registers
	.byte   0xe3 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0x0b, 0x3e       // crtc registers 0-7
	.byte   0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0xea, 0x8c, 0xdf, 0x28, 0x00, 0xe7, 0x04, 0xe3, 0xff // crtc registers 16-24
	.byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07       // attr registers 0-7
	.byte   0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f       // attr registers 8-15
	.byte   0x01, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff // graphics registers 0-8
// video parameter table for mode 13h = G  40x25  8x8   320x200  256/256K  .   A000 VGA,MCGA,ATI VIP
	.byte   40, 24, 8, 0x00, 0x20 // bios data (columns, rows, pixels/char, page length)
	.byte   0x01, 0x0f, 0x00, 0x0e // sequencer registers
	.byte   0x63 // misc output registers
	.byte   0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f       // crtc registers 0-7
	.byte   0x00, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       // crtc registers 8-15
	.byte   0x9c, 0x8e, 0x8f, 0x28, 0x40, 0x96, 0xb9, 0xa3, 0xff // crtc registers 16-24
	.byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07       // attr registers 0-7
	.byte   0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f       // attr registers 8-15
	.byte   0x41, 0x00, 0x0f, 0x00 // attr registers 16-19
	.byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x05, 0x0f, 0xff // graphics registers 0-8

	.align	2
	
	.global	BG_PALETTE
BG_PALETTE:						// Palette used in all other but EGA modes.
	.space	4*256
	.global	EGA_PALETTE_SAVE
EGA_PALETTE_SAVE:				// Palette used when in EGA graphics mode. Values copied to BG_PALETTE in VGA_DAC_SendColor
	.space	4*256
