//=============================================================================
// int10h.c
//
// This file contains the BIOS video interrupt handlers.
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

#include <stdio.h>
#include <string.h>

#include "pax86.h"
#include "pacpu.h"
//#include "pados.h"
//#include "config.h"

#define	USE_VESA_BIOS	1

extern u8 *BIOS_C000;

// We need to have have pragma pack active here!
#pragma pack(1)

typedef struct MODE_INFO {
	u16 ModeAttributes;
	u8 WinAAttributes;
	u8 WinBAttributes;
	u16 WinGranularity;
	u16 WinSize;
	u16 WinASegment;
	u16 WinBSegment;
	u32 WinFuncPtr;
	u16 BytesPerScanLine;
	u16 XResolution;
	u16 YResolution;
	u8 XCharSize;
	u8 YCharSize;
	u8 NumberOfPlanes;
	u8 BitsPerPixel;
	u8 NumberOfBanks;
	u8 MemoryModel;
	u8 BankSize;
	u8 NumberOfImagePages;
	u8 Reserved_page;
	u8 RedMaskSize;
	u8 RedMaskPos;
	u8 GreenMaskSize;
	u8 GreenMaskPos;
	u8 BlueMaskSize;
	u8 BlueMaskPos;
	u8 ReservedMaskSize;
	u8 ReservedMaskPos;
	u8 DirectColorModeInfo;
	u32 PhysBasePtr;
	u32 OffScreenMemOffset;
	u16 OffScreenMemSize;
	u8 Reserved[206];
} MODE_INFO;

#pragma pack()

extern void CharToCGA4Screen(int, int, int);
extern void CharToCGA6Screen(int, int, int);
extern void CharToEGAScreen(int, int, int);
extern void CharToMCGAScreen(int, int, int);

extern void PixelToCGA4Screen(int color, int X, int Y);
extern void PixelToCGA6Screen(int color, int X, int Y);
extern void PixelToEGAScreen(int color, int X, int Y);
extern void PixelToMCGAScreen(int color, int X, int Y);

extern int PixelFromCGA4Screen(int X, int Y);
extern int PixelFromCGA6Screen(int X, int Y);
extern int PixelFromEGAScreen(int X, int Y);
extern int PixelFromMCGAScreen(int X, int Y);

extern void ScreenConfigChanged();
extern void BuildEGAPaletteLUT(int idx, int src);
extern void HideCursor();

extern void Out3C0(int reg_idx, int value);				// In EGA.s, to set an EGA palette register value
extern void out_3D9_CGA_Color(int dummy, int value);	// In CGA.s, to select palette

extern void out_3C4_VGA_sequencer(int dummy, int reg);
extern void out_3C5_VGA_sequencer(int dummy, int value);
	
extern void out_3C8_VGA_pel(int dummy, int addr);
extern void out_3C9_VGA_pel(int dummy, int color);		// Normal routine
extern void out_3C9_EGA_pel(int dummy, int color);		// 16-color routine

extern void Out3CF(int reg_idx, int value);				// In EGA.s

extern int text_pos_by_page_cursor(int page);						// In TEXT.s
extern int text_pos_by_page_col_row(int page, int col, int row);	// In TEXT.s

extern void Setup_EGAVGA_A000();

extern void *VGA_out_3C9_addr;	// Address of the routine that handles output for port 0x3C9
//extern int out_3C9_VGA_pel;		// Normal routine
//extern int out_3C9_EGA_pel;		// 16-color routine

extern char TEXTBuffer[];
extern u8 EGAPaletteRegisters;
extern int EGAPaletteTable[];
//extern u16 EGA_PALETTE_LUT[];
extern u8 VideoParameterTable;
extern u8 VGA_attr_data_3C0[];
extern u8 VGA_pel_mask_3C6;
extern u8 VGA_misc_3C2;
extern u8 VGA_Sequencer_regs_3C5;
extern u8 VGA_Graphics_regs_3CF[];
extern u8 VGA_CRTC_data_3D5[];
extern u8 VGAMemoryMode;
extern u8 CGA_3D9_value;

extern int MaxScreenX;
extern int MaxScreenY;

int VESA_Bank = 0;
int VESA_Start = 0;

#define	SCR_MODE	0x49
#define	SCR_COLS	0x4A
#define	SCR_OFFS_LO	0x4E
#define	SCR_OFFS_HI	0x4F
#define	CURSOR_COL	0x50
#define	CURSOR_ROW	0x51
#define	SCR_PAGE	0x62
#define	SCR_ROWS	0x84
#define	CHAR_HEIGHT	0x85

#define RC2COL(row, col) (((BIOSData[SCR_COLS]*row)+(col)))

#ifdef RPi
extern short	BG_PALETTE[257];
#else
extern int		BG_PALETTE[256];
#endif
extern int		EGA_PALETTE_SAVE[256];

#ifdef RPi
static const short DefaultPalette[] = {
#else
static const int DefaultPalette[] = {
#endif
	RGB15(0,0,0),					// High Black
	RGB15(0,0,21),					// High Blue
	RGB15(0,21,0),					// High Green
	RGB15(0,21,21),					// High Blue + High Green

	RGB15(21,0,0),					// High Red
	RGB15(21,0,21),					// High Red + High Blue
	RGB15(21,12,0),					// Low Green + High Red
	RGB15(21,21,21),				// High Red + Green + Blue

	RGB15(12,12,12),				// Low Red + Green + Blue
	RGB15(12,12,31),				// High Blue + Low (Red + Green + Blue)
	RGB15(12,31,12),
	RGB15(12,31,31),

	RGB15(31,12,12),
	RGB15(31,12,31),
	RGB15(31,31,12),
	RGB15(31,31,31),

	// MCGA colors 16-31 = grey scale
	
	RGB15(0,0,0),
	RGB15(2,2,2),
	RGB15(4,4,4),
	RGB15(6,6,6),
	RGB15(7,7,7),
	RGB15(9,9,9),
	RGB15(10,10,10),
	RGB15(12,12,12),
	RGB15(14,14,14),
	RGB15(16,16,16),
	RGB15(18,18,18),
	RGB15(20,20,20),
	RGB15(23,23,23),
	RGB15(25,25,25),
	RGB15(27,27,27),
	RGB15(31,31,31)
};

const u8 VESAProtMode[] = {0x08, 0, 0x0D, 0, 0x12, 0, 0, 0, 
						   0xFE, 0x38, 0x00, 0x00, 0xC3,			// callback 0, RET	(Function 4F05 = Set Bank)
						   0xFE, 0x38, 0x01, 0x00, 0xC3,			// callback 1, RET
						   0xFE, 0x38, 0x02, 0x00, 0xC3 }; 			// callback 2, RET	(Function 4F09 = Set Primary Palette)

static void ClearTextWindow(int attr, int start, int end) {
	int x1, y1, rows, cols, i, j, pos;
	u16 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = (end >> 8)-y1+1;
	ptr = (u16 *)CGA_B800;
	start = RC2COL(y1, x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>1);		// index counted in halfwords
	attr = (attr<<8)|' ';
	pos = start;
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = attr;
		}
		pos += BIOSData[SCR_COLS];
	}
}

static void ClearCGA4Window(int attr, int start, int end) {
	int x1, y1, rows, cols, i, j, pos;
	u16 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = ((end >> 8)-y1+1)<<2;
	ptr = (u16 *)CGA_B800;
	start = RC2COL(y1<<2, x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>1);		// index counted in halfwords
	attr &= 3;
	attr |= (attr<<2);
	attr |= (attr<<4);
	attr |= (attr<<8);
	pos = start;
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = attr;
			ptr[pos+j+0x1000] = attr;
		}
		pos += BIOSData[SCR_COLS];
	}
}

static void ClearCGA6Window(int attr, int start, int end) {
	int x1, y1, rows, cols, i, j, pos;
	u8 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = ((end >> 8)-y1+1)<<2;
	ptr = (u8 *)CGA_B800;
	start = RC2COL(y1<<2, x1)+(BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8));				// index counted in bytes
	attr &= 1;
	attr |= (attr<<1);
	attr |= (attr<<2);
	attr |= (attr<<4);
	pos = start;
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = attr;
			ptr[pos+j+0x2000] = attr;
		}
		pos += BIOSData[SCR_COLS];
	}
}

static void ClearEGAWindow(int attr, int start, int end) {
	int x1, y1, rows, cols, i, j, pos;
	u32 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = ((end >> 8)-y1+1)*BIOSData[CHAR_HEIGHT];
	ptr = (u32 *)EGAVGA_A000;
	start = RC2COL(y1*BIOSData[CHAR_HEIGHT], x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>2);		// index counted in words
	attr &= 15;
	attr |= (attr<<4);
	attr |= (attr<<8);
	attr |= (attr<<16);
	pos = start;
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = attr;
		}
		pos += BIOSData[SCR_COLS];
	}
}

static void ClearMCGAWindow(int attr, int start, int end) {
	int x1, y1, rows, cols, i, j, pos;
	u32 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = ((end & 0xFF)-x1+1)<<1;
	rows = ((end >> 8)-y1+1)<<3;
	ptr = (u32 *)EGAVGA_A000;
	start = RC2COL(y1<<(3+1), x1<<1);		// index counted in words
	attr |= (attr<<8);
	attr |= (attr<<16);
	pos = start;
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<cols; j++ )
		{
			ptr[pos+j] = attr;
		}
		pos += (BIOSData[SCR_COLS]<<1);
	}
}

static void ScrollTextWindow(int attr, int start, int end, bool up) {
	int x1, y1, rows, cols, pos, rowlen, i, j;
	u16 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = (end >> 8)-y1;
	ptr = (u16 *)CGA_B800;
	start = RC2COL(y1, x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>1);		// index counted in halfwords
	attr = (attr<<8)|' ';
	if (up)
	{
		pos = start;
		rowlen = BIOSData[SCR_COLS];
	}
	else
	{
		rowlen = -(int)(BIOSData[SCR_COLS]);
		pos = start - rows*rowlen;
	}
	// Move the rows that are scrolled
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = ptr[pos+j+rowlen];
		}
		pos += rowlen;
	}
	// Clear the bottom row using attribute
	for ( j=0; j<=cols; j++ )
	{
		ptr[pos+j] = attr;
	}
}

static void ScrollCGA4Window(int attr, int start, int end, bool up) {
	int x1, y1, rows, cols, pos, rowlen, i, j;
	u16 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = ((end >> 8)-y1)<<2;
	ptr = (u16 *)CGA_B800;
	start = RC2COL(y1<<2, x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>2);		// index counted in words
	attr &= 3;
	attr |= (attr<<2);
	attr |= (attr<<4);
	attr |= (attr<<8);
	if (up)
	{
		pos = start;
		rowlen = BIOSData[SCR_COLS];
	}
	else
	{
		rowlen = -(int)(BIOSData[SCR_COLS]);
		pos = start - rows*rowlen;
	}
	// Move the rows that are scrolled
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = ptr[pos+j+(rowlen<<2)];
			ptr[pos+j+0x1000] = ptr[pos+j+0x1000+(rowlen<<2)];
		}
		pos += rowlen;
	}
	// Clear the bottom row using attribute
	if ( rowlen < 0 ) rowlen = -rowlen;
	for ( j=0; j<=cols; j++ )
	{
		ptr[pos+j] = attr;
		ptr[pos+j+rowlen] = attr;
		ptr[pos+j+2*rowlen] = attr;
		ptr[pos+j+3*rowlen] = attr;
		ptr[pos+j+0x1000] = attr;
		ptr[pos+j+0x1000+rowlen] = attr;
		ptr[pos+j+0x1000+2*rowlen] = attr;
		ptr[pos+j+0x1000+3*rowlen] = attr;
	}
}

static void ScrollCGA6Window(int attr, int start, int end, bool up) {
	int x1, y1, rows, cols, pos, rowlen, i, j;
	u8 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	rows = ((end >> 8)-y1)<<2;
	ptr = (u8 *)CGA_B800;
	start = RC2COL(y1<<2, x1)+(BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8));				// index counted in bytes
	attr &= 1;
	attr |= (attr<<1);
	attr |= (attr<<2);
	attr |= (attr<<4);
	if (up)
	{
		pos = start;
		rowlen = BIOSData[SCR_COLS];
	}
	else
	{
		rowlen = -(int)(BIOSData[SCR_COLS]);
		pos = start - rows*rowlen;
	}
	// Move the rows that are scrolled
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = ptr[pos+j+(rowlen<<2)];
			ptr[pos+j+0x2000] = ptr[pos+j+0x2000+(rowlen<<2)];
		}
		pos += rowlen;
	}
	// Clear the bottom row using attribute
	if ( rowlen < 0 ) rowlen = -rowlen;
	for ( j=0; j<=cols; j++ )
	{
		ptr[pos+j] = attr;
		ptr[pos+j+rowlen] = attr;
		ptr[pos+j+2*rowlen] = attr;
		ptr[pos+j+3*rowlen] = attr;
		ptr[pos+j+0x2000] = attr;
		ptr[pos+j+0x2000+rowlen] = attr;
		ptr[pos+j+0x2000+2*rowlen] = attr;
		ptr[pos+j+0x2000+3*rowlen] = attr;
	}
}

static void ScrollEGAWindow(int attr, int start, int end, bool up) {
	int x1, y1, rows, cols, pos, rowlen, i, j, cheight;
	u32 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = (end & 0xFF)-x1;
	cheight = BIOSData[CHAR_HEIGHT];
	rows = ((end >> 8)-y1)*cheight;
	ptr = (u32 *)EGAVGA_A000;
	start = RC2COL(y1*cheight, x1)+((BIOSData[SCR_OFFS_LO]|(BIOSData[SCR_OFFS_HI]<<8))>>2);		// index counted in words
	attr &= 15;
	attr |= (attr<<4);
	attr |= (attr<<8);
	attr |= (attr<<16);
	if (up)
	{
		pos = start;
		rowlen = BIOSData[SCR_COLS];
	}
	else
	{
		rowlen = -(int)(BIOSData[SCR_COLS]);
		pos = start - rows*rowlen;
	}
	// Move the rows that are scrolled
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<=cols; j++ )
		{
			ptr[pos+j] = ptr[pos+j+(rowlen*cheight)];
		}
		pos += rowlen;
	}
	// Clear the bottom row using attribute
	if ( rowlen < 0 ) rowlen = -rowlen;
	for ( j=0; j<=cols; j++ )
	{
		for ( i = 0; i<cheight; i++ )
			ptr[pos+j+rowlen*i] = attr;
	}
}

static void ScrollMCGAWindow(int attr, int start, int end, bool up) {
	int x1, y1, rows, cols, pos, rowlen, i, j;
	u32 *ptr;
	x1 = start & 0xFF;
	y1 = start >> 8;
	cols = ((end & 0xFF)-x1+1)<<1;
	rows = ((end >> 8)-y1)<<3;
	ptr = (u32 *)EGAVGA_A000;
	start = RC2COL(y1<<(3+1), x1<<1);		// index counted in words
	attr |= (attr<<8);
	attr |= (attr<<16);
	if (up)
	{
		pos = start;
		rowlen = 40*2;
	}
	else
	{
		rowlen = -40*2;
		pos = start - rows*rowlen;
	}
	// Move the rows that are scrolled
	for ( i = 0; i<rows; i++ )
	{
		for ( j=0; j<cols; j++ )
		{
			ptr[pos+j] = ptr[pos+j+rowlen*8];
		}
		pos += rowlen;
	}
	// Clear the bottom row using attribute
	for ( j=0; j<cols; j++ )
	{
		ptr[pos+j] = attr;
		ptr[pos+j+40*2] = attr;
		ptr[pos+j+40*4] = attr;
		ptr[pos+j+40*6] = attr;
		ptr[pos+j+40*8] = attr;
		ptr[pos+j+40*10] = attr;
		ptr[pos+j+40*12] = attr;
		ptr[pos+j+40*14] = attr;
	}
}

static void PerformGrayScaleSumming(int start, int count) {
	int ct;
	if (count>0x100) count=0x100;
	for (ct=0; ct<count; ct++) {
		int dac = BG_PALETTE[start+ct];
		int red = dac&0x1F;
		int green = (dac>>6)&0x1F;
		int blue = (dac>>11)&0x1F;
		/* calculate clamped intensity, taken from VGABIOS */
		int i=(( 77*red + 151*green + 28*blue ) + 0x80) >> 8;
		if ( i > 0x1F ) i = 0x1F;
		BG_PALETTE[start+ct] = RGB15(i, i, i);
	}
#ifdef RPi
	BG_PALETTE[256] = 1;	// Flag that the palette has changed
#endif
}

void ScreenConfigChanged()
{
	int	vheight = 200;
	// Check for special screen modes tricks
	// - GODS: Mode = 0x10, VGA_CRTC_data_3D5[1] == 0x27 => Mode = 0x0D
	if ( 0x10 == BIOSData[SCR_MODE] && 0x27 == VGA_CRTC_data_3D5[1] )
		BIOSData[SCR_MODE] = 0x0D;
	// - Comanche: Mode = 0x12, VGA_GFX_MODE[5] = 0x40 => Mode = 0x13
	if ( 0x12 == BIOSData[SCR_MODE] && 0x40 == (VGA_Graphics_regs_3CF[5]&0x40) )
		BIOSData[SCR_MODE] = 0x13;
	switch ( BIOSData[SCR_MODE] )	// Depending on mode...
	{
		case 0x00:					// 40x25 mono text mode
		case 0x01:					// 40x25 color text mode
			MaxScreenX = 40*8;
			MaxScreenY = 25*16;
			break;
		case 0x02:					// 80x25 mono text mode
		case 0x03:					// 80x25 color text mode
			MaxScreenX = 80*8;
			MaxScreenY = 25*16;		// Handle both 80x25 and 80x50 screen modes
			break;
		case 0x06:					// CGA 640x200
			MaxScreenX = 640;
			MaxScreenY = 200;
			BG_PALETTE[1] = RGB15(31,31,31);			// 1 = White
#ifdef RPi
			BG_PALETTE[256] = 1;	// Flag that the palette has changed
#endif
			break;
		case 0x13:					// MCGA 320x200
			if ( 0xC0 == (VGA_misc_3C2 & 0xC0) )		// 480/240 lines, for Mode-X!
				vheight = 240;
			if (0 == (VGA_CRTC_data_3D5[9] & 1))		// 400/480 lines, for Mode-X!
				vheight <<= 1;
		case 0x04:					// CGA 320x200
		case 0x05:					// CGA 320x200
		case 0x0D:					// EGA 320x200
			if (0x6B == VGA_CRTC_data_3D5[0])		// 360-pixel wide version!
				MaxScreenX = 360;
			else
				MaxScreenX = 320;
			MaxScreenY = vheight;
			break;
		case 0x0E:					// EGA 640x200 (or 640x400)
			// - 98mj2: Mode = 0x0E, VGA_CRTC_data_3D5[9] & 0x9F == 0 => Resolution = 640x400
			MaxScreenX = 640;
			if ( 0 == (VGA_CRTC_data_3D5[9] & 0x9F) )
				MaxScreenY = 400;
			else
				MaxScreenY = 200;
			break;
		case 0x10:					// EGA 640x350
			MaxScreenX = 640;
			MaxScreenY = 350;
			break;
		case 0x12:					// VGA 640x480
			MaxScreenX = 640;
			MaxScreenY = 480;
			break;
		case 0x1C:					// SVGA 640x400
			MaxScreenX = 640;
			MaxScreenY = 400;
			break;
		case 0x1D:					// SVGA 640x480
			MaxScreenX = 640;
			MaxScreenY = 480;
			break;
	}
	Setup_EGAVGA_A000();
}

extern u32	ROM8x8Font;
extern u32	ROM8x8Upper;
extern u32 	ROM8x14Font;
extern u32 	ROM8x16Font;

extern void MouseNewVideoMode();

void SetupVideoParameters(int mode)
{
	u8 *table;
	int i;
	mode &= 0x7F;											// Ignore "don't clear video RAM" bit of mode

	table = ((u8*)(&VideoParameterTable))+mode*64;

	BIOSData[SCR_MODE] = mode;								//	40:49	byte	Current video mode
	BIOSData[SCR_COLS] = table[0];							//	40:4A	word	Number of screen columns
	BIOSData[SCR_COLS+1] = 0;
	BIOSData[SCR_ROWS] = table[1];							//	40:84   byte    Rows on the screen (less 1, EGA+)
	BIOSData[SCR_PAGE] = 0;									// Current page = 0
	BIOSData[SCR_OFFS_LO] = 0;
	BIOSData[SCR_OFFS_HI] = 0;
	BIOSData[CURSOR_COL] = 0;								// Cursor position on page 0 = 0
	BIOSData[CURSOR_ROW] = 0;								// Cursor position on page 0 = 0
	BIOSData[0x4C] = table[3];								// Size of a page in video regen buffer
	BIOSData[0x4D] = table[4];								// Size of a page in video regen buffer

	switch ( mode )
	{
		case 0x0F:
		case 0x10:
			BIOSData[CHAR_HEIGHT] = 14;
			INTVectors[0x43] = (int)ROM8x14Font;
			break;
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
		case 0x11:
		case 0x12:
			BIOSData[CHAR_HEIGHT] = 16;
			INTVectors[0x43] = (int)ROM8x16Font;
			BIOSData[0x60] = 15; // Cursor bottom scanline
			BIOSData[0x61] = 14; // Cursor top scanline
			break;
		default:
			BIOSData[CHAR_HEIGHT] = 8;
			INTVectors[0x43] = (int)ROM8x8Font;
			break;
	}

	// Reset the EGA palette save area
	memcpy( EGA_PALETTE_SAVE, EGAPaletteTable, 64*4);

	// Setup the sequencer registers
	memcpy( &VGA_Sequencer_regs_3C5, &(table[4]), 5);
	
	// Setup the Misc Register
	VGA_misc_3C2 = table[9];
	
	// Setup the CRTC registers
	memcpy( VGA_CRTC_data_3D5, &(table[10]), 25 );

	// Setup the attribute (palette) registers (mainly used in EGA mode)
	for ( i=0; i < 20; i++ )
	{
		Out3C0(i, table[35+i]);
	}

	// Setup the Graphics registers
	for ( i=0; i < 9; i++ )
	{
		Out3CF(i, table[55+i]);
	}

	MouseNewVideoMode();
}

extern char BreakReasonText[];
char ModeStr[20];

extern u8 VGAStartAddrLow;
extern u8 VGAStartAddrHigh;

static int WriteCharAtCursor(int ch, int page, int attr, int count, bool useattr)
{
	int i;
	switch ( BIOSData[SCR_MODE] )
	{
		case 0x00:	// TEXT 40x25 mono mode
		case 0x01:	// TEXT 40x25 color mode
		case 0x02:	// TEXT 80x25 mono mode
		case 0x03:	// TEXT 80x25 color mode
			if ( useattr )
			{
				int pos = text_pos_by_page_cursor(page)>>1;
				for ( i=0; i < count; i++ )
					((u16 *)CGA_B800)[pos+i] = ch|(attr<<8);
			}
			else
			{
				int pos = text_pos_by_page_cursor(page);
				for ( i=0; i < count; i++ )
					((u8 *)CGA_B800)[pos+i*2] = ch;
				
			}
			return 0;
		case 0x04:	// CGA 320x200x4 color mode
		case 0x05:	// CGA 320x200x4 mono mode
			CharToCGA4Screen( ch, attr&3, count );
			return 0;
		case 0x06:	// CGA 640x200x1 mono mode
			CharToCGA6Screen( ch, attr, count );
			return 0;
		case 0x0D:	// EGA 320x200x16 mode
		case 0x0E:	// EGA 640x200x16 mode
		case 0x10:	// EGA 640x350x16 mode
		case 0x12:	// EGA 640x480x16 mode
			CharToEGAScreen( ch, (page<<8)|attr, count );
			return 0;
		case 0x13:	// MCGA 320x200x256 mode
			CharToMCGAScreen( ch, (page<<8)|attr, count );
			return 0;
		default:
			break;
	}
	return -1;
}

int Teletype(int ch, int page, int attr, bool useattr)
{
	int col = BIOSData[CURSOR_COL+(page<<1)];
	int row = BIOSData[CURSOR_ROW+(page<<1)];
	//iprintf("p=%d r=%d c=%d ch=%c\n", page, row, col, ch );
	int	retval = 0;
	HideCursor();
	switch ( ch )
	{
		case 0x07:
			break;
		case 0x08:
			if ( col > 0 )
				col--;
			break;
		case 0x0a:
			if ( row++ == BIOSData[SCR_ROWS] )
			{
				row--;
				ScrollTextWindow(((u16 *)CGA_B800)[col + (int)BIOSData[SCR_COLS]*row]>>8, 0, (BIOSData[SCR_ROWS]<<8)|BIOSData[SCR_COLS], true);
			}
			break;
		case 0x0d:
			col = 0;
			break;
		default:
			retval = WriteCharAtCursor(ch, page, attr, 1, useattr);
			if ( ++col == BIOSData[SCR_COLS] )
			{
				col = 0;
				if ( row++ == BIOSData[SCR_ROWS] )
				{
					row--;
					ScrollTextWindow(((u16 *)CGA_B800)[col + (int)BIOSData[SCR_COLS]*row]>>8, 0, (BIOSData[SCR_ROWS]<<8)|BIOSData[SCR_COLS], true);
				}
			}
			break;
	}
	BIOSData[CURSOR_COL+(page<<1)] = col;
	BIOSData[CURSOR_ROW+(page<<1)] = row;
	return retval;
}

//--------------------------
// Video BIOS 10h interrupt.
//--------------------------
int int10h() {
	int i;
	switch ( AH )
	{
		case 0x00:
			// VIDEO - SET VIDEO MODE
			// AH = 00h
			// AL = desired video mode (see #00010)
			// Return:AL = video mode flag (Phoenix, AMI BIOS)
			// 20h mode > 7
			// 30h modes 0-5 and 7
			// 3Fh mode 6
			// AL = CRT controller mode byte (Phoenix 386 BIOS v1.10)
			// 
			// Desc: Specify the display mode for the currently active display adapter 
			// 
			// InstallCheck:For Ahead adapters, the signature "AHEAD" at C000h:0025h.
			// For Paradise adapters, the signature "VGA=" at C000h:007Dh.
			// For Oak Tech OTI-037/057/067/077 chipsets, the signature "OAK VGA" at
			// C000h:0008h.
			// For ATI adapters, the signature "761295520" at C000h:0031h; the byte
			// at C000h:0043h indicates the chipset revision:
			// 31h for 18800
			// 32h for 18800-1
			// 33h for 18800-2
			// 34h for 18800-4
			// 35h for 18800-5
			// 62h for 68800AX (Mach32) (see also #00732)
			// the two bytes at C000h:0040h indicate the adapter type
			// "22" EGA Wonder
			// "31" VGA Wonder
			// "32" EGA Wonder800+
			// the byte at C000h:0042h contains feature flags
			// bit 1:Mouse port present
			// bit 4:Programmable video clock
			// the byte at C000h:0044h contains additional feature flags if chipset
			// byte > 30h (see #00009).
			// For Genoa video adapters, the signature 77h XXh 99h 66h at C000h:NNNNh,
			// where NNNNh is stored at C000h:0037h and XXh is
			// 00h for Genoa 6200/6300
			// 11h for Genoa 6400/6600
			// 22h for Genoa 6100
			// 33h for Genoa 5100/5200
			// 55h for Genoa 5300/5400
			// for SuperEGA BIOS v2.41+, C000h:0057h contains the product level
			// for Genoa SuperEGA BIOS v3.0+, C000h:0070h contains the signature
			// "EXTMODE", indicating support for extended modes
			// 
			// Notes: IBM standard modes do not clear the screen if the high bit of AL is set (EGA or higher only).
			// The Tseng ET4000 chipset is used by the Orchid Prodesigner II, Diamond SpeedSTAR VGA,
			// Groundhog Graphics Shadow VGA, Boca Super X VGA, Everex EV-673, etc..
			// Intercepted by GRAFTABL from Novell DOS 7 and Caldera OpenDOS 7.01. 
			// 
			// See Also: AX=0070h - AX=007Eh - AX=10E0h - AX=10F0h - 
			// 
			// See Also: INT 33/AX=0028h - INT 5F/AH=00h - INT 62/AX=0001h - MEM 0040h:0049h 
			// 
			// Index:Installation check;Tseng ET4000|installation check;Ahead video cards
			// Index:Installation check;Oak Technologies|installation check;ATI video cards
			// Index:Installation check;Paradise video|installation check;Genoa video cards
			// 
			// Bitfields for ATI additional feature flags:
			// 
			// Bit(s)  Description     (Table 00009)
			// 0      70 Hz non-interlaced display
			// 1      Korean (double-byte) characters
			// 2      45 MHz memory clock rather than 40 MHz
			// 3      zero wait states
			// 4      paged ROMs
			// 6      no 8514/A monitor support
			// 7      HiColor DAC
			// 
			// 
			// (Table 00010)
			// Values for video mode:
			// text/ text pixel   pixel   colors disply scrn  system
			// grph resol  box  resolution       pages  addr
			// 00h = T  40x25  8x8   320x200  16gray    8   B800 CGA,PCjr,Tandy
			// = T  40x25  8x14  320x350  16gray    8   B800 EGA
			// = T  40x25  8x16  320x400   16       8   B800 MCGA
			// = T  40x25  9x16  360x400   16       8   B800 VGA
			// 01h = T  40x25  8x8   320x200   16       8   B800 CGA,PCjr,Tandy
			// = T  40x25  8x14  320x350   16       8   B800 EGA
			// = T  40x25  8x16  320x400   16       8   B800 MCGA
			// = T  40x25  9x16  360x400   16       8   B800 VGA
			// 02h = T  80x25  8x8   640x200  16gray    4   B800 CGA,PCjr,Tandy
			// = T  80x25  8x14  640x350  16gray    8   B800 EGA
			// = T  80x25  8x16  640x400   16       8   B800 MCGA
			// = T  80x25  9x16  720x400   16       8   B800 VGA
			// 03h = T  80x25  8x8   640x200   16       4   B800 CGA,PCjr,Tandy
			// = T  80x25  8x14  640x350   16/64    8   B800 EGA
			// = T  80x25  8x16  640x400   16       8   B800 MCGA
			// = T  80x25  9x16  720x400   16       8   B800 VGA
			// = T  80x43  8x8   640x350   16       4   B800 EGA,VGA [17]
			// = T  80x50  8x8   640x400   16       4   B800 VGA [17]
			// 04h = G  40x25  8x8   320x200    4       .   B800 CGA,PCjr,EGA,MCGA,VGA
			// 05h = G  40x25  8x8   320x200   4gray    .   B800 CGA,PCjr,EGA
			// = G  40x25  8x8   320x200    4       .   B800 MCGA,VGA
			// 06h = G  80x25  8x8   640x200    2       .   B800 CGA,PCjr,EGA,MCGA,VGA
			// = G  80x25   .       .     mono      .   B000 HERCULES.COM on HGC [14]
			// 07h = T  80x25  9x14  720x350  mono     var  B000 MDA,Hercules,EGA
			// = T  80x25  9x16  720x400  mono      .   B000 VGA
			// 08h = T 132x25  8x8  1056x200   16       .   B800 ATI EGA/VGA Wonder [2]
			// = T 132x25  8x8  1056x200  mono      .   B000 ATI EGA/VGA Wonder [2]
			// = G  20x25  8x8   160x200   16       .     .  PCjr, Tandy 1000
			// = G  80x25  8x16  640x400  color     .     .  Tandy 2000
			// = G  90x43  8x8   720x348  mono      .   B000 Hercules + MSHERC.COM
			// = G  90x45  8x8   720x360  mono      .   B000 Hercules + HERKULES [11]
			// = G  90x29  8x12  720x348  mono      .     .  Hercules + HERCBIOS [15]
			// 09h = G  40x25  8x8   320x200   16       .     .  PCjr, Tandy 1000
			// = G  80x25  8x16  640x400  mono      .     .  Tandy 2000
			// = G  90x43  8x8   720x348  mono      .     .  Hercules + HERCBIOS [15]
			// 0Ah = G  80x25  8x8   640x200    4       .     .  PCjr, Tandy 1000
			// 0Bh =   reserved                                  (EGA BIOS internal use)
			// = G  80x25  8x8   640x200   16       .     .  Tandy 1000 SL/TL [13]
			// 0Ch =   reserved                                  (EGA BIOS internal use)
			// 0Fh = G  80x25  8x14  640x350  mono      2   A000 EGA,VGA
			// 10h = G  80x25  8x14  640x350    4       2   A000 64k EGA
			// 	   = G    .     .    640x350   16       .   A000 256k EGA,VGA
			// 11h = G  80x30  8x16  640x480  mono      .   A000 VGA,MCGA,ATI EGA,ATI VIP
			// 12h = G  80x30  8x16  640x480   16/256K  .   A000 VGA,ATI VIP
			// = G  80x30  8x16  640x480   16/64    .   A000 ATI EGA Wonder
			// = G    .     .    640x480   16       .     .  UltraVision+256K EGA
			// 14h = T 132x25  Nx16     .      16       .   B800 XGA, IBM Enhanced VGA [3]
			// = T 132x25  8x16 1056x400   16/256K  .     .  Cirrus CL-GD5420/5422/5426
			// = G  80x25  8x8   640x200    .       .     .  Lava Chrome II EGA
			// = G    .     .    640x400   16       .     .  Tecmar VGA/AD
			// 15h = G  80x25  8x14  640x350    .       .     .  Lava Chrome II EGA
			// 16h = G  80x25  8x14  640x350    .       .     .  Lava Chrome II EGA
			// = G    .     .    800x600   16       .     .  Tecmar VGA/AD
			// 17h = T 132x25   .       .       .       .     .  Tecmar VGA/AD
			// = T  80x43  8x8   640x348   16       4   B800 Tseng ET4000 BIOS [10]
			// = G  80x34  8x14  640x480    .       .     .  Lava Chrome II EGA
			// 18h = T  80x30  9x16  720x480   16       1   A000 Realtek RTVGA [12]
			// = T 132x25   .       .     mono      .   B000 Cirrus 5320 chipset
			// = T 132x44  8x8  1056x352  mono      .   B000 Tseng Labs EVA
			// = T 132x44  9x8  1188x352   4gray    2   B000 Tseng ET3000 chipset
			// = T 132x44  8x8  1056x352   16/256   2   B000 Tseng ET4000 chipset
			// = G  80x34  8x14  640x480    .       .     .  Lava Chrome II EGA
			// = G              1024x768   16       .     .  Tecmar VGA/AD
			// 19h = T  80x43  9x11  720x473   16       1   A000 Realtek RTVGA [12]
			// = T 132x25  8x14 1056x350  mono      .   B000 Tseng Labs EVA
			// = T 132x25  9x14 1188x350   4gray    4   B000 Tseng ET3000 chipset
			// = T 132x25  8x14 1056x350   16/256   4   B000 Tseng ET4000 chipset
			// = T 132x34   .       .     mono      .   B000 Cirrus 5320 chipset
			// 1Ah = T  80x60  9x8   720x480   16       1   A000 Realtek RTVGA [12]
			// = T 132x28  8x13 1056x364  mono      .   B000 Tseng Labs EVA
			// = T 132x28  9x13 1188x364   4gray    4   B000 Tseng ET3000 chipset
			// = T 132x28  8x13 1056x364   16/256   4   B000 Tseng ET4000 chipset
			// = T 132x44   .       .     mono      .   B000 Cirrus 5320 chipset
			// = G    .     .    640x350  256       .     .  Tecmar VGA/AD
			// 1Bh = T 132x25  9x14 1188x350   16       1   A000 Realtek RTVGA [12]
			// = G    .     .    640x400  256       .     .  Tecmar VGA/AD
			// 1Ch = T 132x25   .       .       .       .     .  Cirrus 5320 chipset
			// = T 132x30  9x16 1188x480   16       1   A000 Realtek RTVGA [12]
			// = G    .     .    640x480  256       .     .  Tecmar VGA/AD
			// 1Dh = T 132x43   .       .       .       .     .  Cirrus 5320 chipset
			// = T 132x43  9x11 1188x473   16       1   A000 Realtek RTVGA [12]
			// = G    .     .    800x600  256       .     .  Tecmar VGA/AD
			// 1Eh = T 132x44   .       .       .       .     .  Cirrus 5320 chipset
			// = T 132x60  9x8  1188x480   16       1   A000 Realtek RTVGA [12]
			// 1Fh = G 100x75  8x8   800x600   16       1   A000 Realtek RTVGA
			// Notes:
			// [1] interlaced only
			// [2] for ATI EGA Wonder, mode 08h is only valid if SMS.COM is loaded resident.
			// SMS maps mode 08h to mode 27h if the byte at location 0040:0063 is 0B4h,
			// otherwise to mode 23h, thus selecting the appropriate (monochrome or
			// color) 132x25 character mode.
			// for ATI VGA Wonder, mode 08h is the same, and only valid if VCONFIG loaded
			// resident
			// [3] early XGA boards support 132-column text but do not have this BIOS mode
			// [4] DESQview intercepts calls to change into these two modes (21h is page 0,
			// 22h is page 1) even if there is no Hercules graphics board installed
			// [5] ATI BIOS v4-1.00 has a text-scrolling bug in this mode
			// [6] for AT&T VDC overlay modes, BL contains the DEB mode, which may be 06h,
			// 40h, or 44h
			// [7] BIOS text support is broken in this undocumented mode; scrolling moves
			// only about 1/3 of the screen (and does even that portion incorrectly),
			// while screen clears only clear about 3/4.
			// [8] The Oak OTI-037/067/077 modes are present in the Oak VGA BIOS, which OEMs
			// may choose to use only partially or not at all; thus, not all Oak boards
			// support all "Oak" modes listed here
			// [9] this card uses the full 128K A000h-BFFFh range for the video buffer,
			// precluding the use of a monochrome adapter in the same system
			// [10] mode 17h supported by Tseng ET4000 BIOS 8.01X dated 1990/09/14, but not
			// v8.01X dated 1992/02/28; mode 21h supported by 1992/02/28 version but not
			// 1990/09/14 version
			// [11] HERKULES simulates a 90x45 text mode in Hercules graphics mode; the
			// installation check for HERKULES.COM is the signature "Herkules" two
			// bytes beyond the INT 10 handler
			// [12] The Realtek RTVGA BIOS v3.C10 crashes when attempting to switch into
			// modes 21h or 27h; this version of the BIOS also sets the BIOS data area
			// incorrectly for extended text modes, resulting in scrolling after only
			// 24 lines (the VMODE.EXE   utility does set the data area correctly)
			// [13] The Tandy 1000SL/TL BIOS does not actually support this mode
			// [14] HERCULES.COM is a graphics-mode BIOS extension for Hercules-compatible
			// graphics cards by Soft Warehouse, Inc.  Its installation check is to
			// test whether the word preceding the INT 10 handler is 4137h.
			// [15] The Hercules-graphics video modes for HERCBIOS (shareware by Dave
			// Tutelman) may be changed by a command-line switch; the 90x43
			// character-cell mode's number is always one higher than the 90x29 mode
			// (whose default is mode 08h)
			// [16] Stealth64 Video 2001-series BIOS v1.03 reports 76 lines for mode 7Ch,
			// resulting in incorrect scrolling for TTY output (scrolling occurs only
			// after the end of the 76th line, which is not displayed)
			// [17] For 43-line text on EGA or 43/50-line text on VGA, you must load an 8x8
			// font using AX=1102h after switching to mode 3; VGA may also require
			// using INT 10/AH=12h/BL=30h
			// 
			// See Also: #00011 - #00083 - #00191 
			// 
			// Index:Video modes
			// Index:Installation check;HERKULES|installation check;HERCULES.COM
			// 
			// Category: Video - Int 10h - V 
			// 
			VESA_Bank = 0;
			VESA_Start = 0;
			SetupVideoParameters( AL );
			switch( AL )
			{
				case 0x00:
				case 0x01:
					ClearTextWindow(0x07, 0, (24<<8)|39);
					memset(TEXTBuffer, 0, 80*25*2);
				case 0x80:
				case 0x81:
					//
					// 00h = T  40x25  8x8   320x200  16gray    8   B800 CGA,PCjr,Tandy
					//
					//iprintf("Set mode 40x25\n");
#ifdef RPi
					memcpy(BG_PALETTE, DefaultPalette, 16*2);
					BG_PALETTE[256] = 1;	// Flag that the palette has changed
#else
					memcpy(BG_PALETTE, DefaultPalette, 16*4);
#endif
					VGA_out_3C9_addr = out_3C9_VGA_pel;	// Address of the routine that handles output for port 0x3C9
					ScreenConfigChanged();
					return 0;
				case 0x02:
				case 0x03:
					ClearTextWindow(0x07, 0, (24<<8)|79);
					memset(TEXTBuffer, 0, 80*25*2);
				case 0x82:
				case 0x83:
					//
					// 02h = T  80x25  8x8   640x200  16gray    4   B800 CGA,PCjr,Tandy
					// 	   = T  80x25  8x14  640x350  16gray    8   B800 EGA
					// 	   = T  80x25  8x16  640x400   16       8   B800 MCGA
					// 	   = T  80x25  9x16  720x400   16       8   B800 VGA
					// 03h = T  80x25  8x8   640x200   16       4   B800 CGA,PCjr,Tandy
					// 	   = T  80x25  8x14  640x350   16/64    8   B800 EGA
					// 	   = T  80x25  8x16  640x400   16       8   B800 MCGA
					// 	   = T  80x25  9x16  720x400   16       8   B800 VGA
					// 	   = T  80x43  8x8   640x350   16       4   B800 EGA,VGA [17]
					// 	   = T  80x50  8x8   640x400   16       4   B800 VGA [17]
					//
#ifdef RPi
					memcpy(BG_PALETTE, DefaultPalette, 16*2);
					BG_PALETTE[256] = 1;	// Flag that the palette has changed
#else
					memcpy(BG_PALETTE, DefaultPalette, 16*4);
#endif
					VGA_out_3C9_addr = out_3C9_VGA_pel;	// Address of the routine that handles output for port 0x3C9
					ScreenConfigChanged();
					return 0;
				case 0x04:
				case 0x05:
					memset((void*)CGA_B800, 0, 32768);
				case 0x84:
				case 0x85:
					//
					// 04h = G  40x25  8x8   320x200    4       .   B800 CGA,PCjr,EGA,MCGA,VGA
					// 05h = G  40x25  8x8   320x200   4gray    .   B800 CGA,PCjr,EGA
					// 	   = G  40x25  8x8   320x200    4       .   B800 MCGA,VGA
					//

					BG_PALETTE[0] = RGB15(0,0,0);					// 0 = Background = Black
					BG_PALETTE[1] = RGB15(0,31,31);					// 1 = Green/Cyan
					BG_PALETTE[2] = RGB15(31,0,31);					// 2 = Red/Magenta
					BG_PALETTE[3] = RGB15(31,31,31);				// 3 = Brown/White
					out_3D9_CGA_Color(0, 0x20);						// Cyan/Magenta/White
					VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;			// Address of the routine that handles output for port 0x3C9

					ScreenConfigChanged();
					return 0;
				case 0x06:
					memset((void*)CGA_B800, 0, 32768);
				case 0x86:
					//
					// 06h = G  80x25  8x8   640x200    2       .   B800 CGA,PCjr,EGA,MCGA,VGA
					//
					BG_PALETTE[0] = RGB15(0,0,0);				// 0 = Background = Black
					BG_PALETTE[1] = RGB15(31,31,31);			// 1 = White
#ifdef RPi
					BG_PALETTE[256] = 1;	// Flag that the palette has changed
#endif
					VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;	// Address of the routine that handles output for port 0x3C9

					ScreenConfigChanged();
					return 0;
				case 0x0D:
				case 0x0E:
				case 0x10:
				case 0x12:
					memset(EGAVGA_A000, 0, 4*65536);
				case 0x8D:
				case 0x8E:
				case 0x90:
				case 0x92:
					//
					// 0Dh = G  40x25  8x8   320x200   16       8   A000 EGA,VGA
					// 0Eh = G  80x25  8x8   640x200   16       4   A000 EGA,VGA
					// 10h = G  80x25  8x14  640x350   16       .   A000 256k EGA,VGA
					// 12h = G  80x30  8x16  640x480   16/256K  .   A000 VGA,ATI VIP
					//
					VGA_out_3C9_addr = (void *)&out_3C9_EGA_pel;	// Address of the routine that handles output for port 0x3C9
					
					//CurrentConfig.screen_mode = SM_BLIT30FPS;
					ScreenConfigChanged();
					return 0;
				case 0x13:
					memset(EGAVGA_A000, 0, 65536);
				case 0x93:
					//
					// 13h = G  40x25  8x8   320x200  256/256K  .   A000 VGA,MCGA,ATI VIP
					//
#ifdef RPi
					memcpy(BG_PALETTE, DefaultPalette, 32*2);
					BG_PALETTE[256] = 1;	// Flag that the palette has changed
#else
					memcpy(BG_PALETTE, DefaultPalette, 32*4);
#endif
					VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;	// Address of the routine that handles output for port 0x3C9
					ScreenConfigChanged();
					return 0;
				default:
					switch ( AL )
					{
						case 0x0F:	// 0Fh = G  80x25  8x14  640x350  mono      2   A000 EGA,VGA
							strcpy( ModeStr, "640x350x2 (EGA)" );
							break;
						case 0x11:	// 11h = G  80x30  8x16  640x480  mono      .   A000 VGA,MCGA,ATI EGA,ATI VIP
							strcpy( ModeStr, "640x480x2 (VGA)" );
							break;
						default:
							sprintf( ModeStr, "%02X", AL );
							break;
					}
					sprintf( BreakReasonText, "Unsupported graphics mode\n%s requested!\n", ModeStr );
					BreakReason = BreakReasonText;
					break;
			}
			break;
		case 0x01:	// VIDEO - SET TEXT-MODE CURSOR SHAPE
			// CH = cursor start and options (see #00013)
			// CL = bottom scan line containing cursor (bits 0-4)
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
			/*
			CRT_1:  MOV     CX,[BP+6]                       ; Set cursor type, from CX
					MOV     DS:60h,CX                       ;  ...save it
					MOV     AH,0Ah                          ; CRT index register 0Ah
					CALL    OT6845                          ;  ...send CH,CL to CRT reg
					RET
			*/			
			BIOSData[0x60] = CL;
			BIOSData[0x61] = CH;
			return 0;
		case 0x02:	// VIDEO - SET CURSOR POSITION
			// BH = page number
			// 		0-3 in modes 2&3
			// 		0-7 in modes 0&1
			// 		0 in graphics modes
			// DH = row (00h is top)
			// DL = column (00h is left)
			// Return:Nothing
			//
			HideCursor();
			BIOSData[CURSOR_COL+((BH<<1)&0x0E)] = DL;
			BIOSData[CURSOR_ROW+((BH<<1)&0x0E)] = DH;
			return 0;	
		case 0x03:	// VIDEO - GET CURSOR POSITION AND SIZE
			// BH = page number
			// 		0-3 in modes 2&3
			// 		0-7 in modes 0&1
			// 		0 in graphics modes
			// Return:	AX = 0000h (Phoenix BIOS)
			// 			CH = start scan line
			// 			CL = end scan line
			// 			DH = row (00h is top)
			// 			DL = column (00h is left)
			//
			// Notes: A separate cursor is maintained for each of up to 8 display pages.
			// Many ROM BIOSes incorrectly return the default size for a color display (start 06h, end 07h) when a monochrome display is attached.
			// With PhysTechSoft's PTS ROM-DOS the BH value is ignored on entry. 
			/*
			CRT_3:  MOV     BL,[BP+5]                       ; Get cursor position, page BH
					SHL     BL,1
					MOV     BH,0
					MOV     AX,[BX+50h]
					MOV     [BP+8],AX                       ;  ...return position in user DX
					MOV     AX,DS:60h                       ; Get cursor mode
					MOV     [BP+6],AX                       ;  ...return in user CX
					RET
			*/
			SET_CX((BIOSData[0x61]<<8)|BIOSData[0x60]);
			SET_DX((BIOSData[CURSOR_ROW+((BH<<1)&0x0E)]<<8)|(BIOSData[CURSOR_COL+((BH<<1)&0x0E)]));
			return 0;
		case 0x05:	// VIDEO - SELECT ACTIVE DISPLAY PAGE
			// AL = new page number (00h to number of pages - 1) (see #00010)
			// Return:Nothing
			//
			// Note: To determine whether the requested page actually exists, use AH=0Fh to query the current page after making this call 
			//
			{
				int	tmp = BIOSData[0x4C]|(BIOSData[0x4D]<<8);	// Size of a page in video buffer
				BIOSData[SCR_PAGE] = AL;							// Current page = AL
				tmp *= AL;
				BIOSData[SCR_OFFS_LO] = VGAStartAddrLow = tmp & 0xFF;
				BIOSData[SCR_OFFS_HI] = VGAStartAddrHigh = tmp >> 8;
			}
			return 0;
		case 0x06:	// VIDEO - SCROLL UP WINDOW
		case 0x07:	// VIDEO - SCROLL DOWN WINDOW
			// AL = number of lines by which to scroll up (00h = clear entire window)
			// BH = attribute used to write blank lines at bottom of window
			// CH,CL = row,column of window's upper left corner
			// DH,DL = row,column of window's lower right corner
			// Return:Nothing
			//
			// Note: Affects only the currently active page (see AH=05h) 
			//
			// BUGS: Some implementations (including the original IBM PC) have a bug which destroys BP.
			// The Trident TVGA8900CL (BIOS dated 1992/9/8) clears DS to 0000h when scrolling in an SVGA mode (800x600 or higher) 
			//
			// SYSINFO.EXE: AX=0600 BX=1F00 CX=0000 DX=184F
			if ( 0 == AL )
			{
				switch ( BIOSData[SCR_MODE] )
				{
					case 0:
					case 1:
					case 2:
					case 3:		// If in text mode
						ClearTextWindow(BH, CX, DX);
						return 0;
					case 4:
					case 5:
						ClearCGA4Window(BH, CX, DX);
						return 0;
					case 6:
						ClearCGA6Window(BH, CX, DX);
						return 0;
					case 0x0D:	// EGA 320x200x16 mode
					case 0x0E:	// EGA 640x200x16 mode
					case 0x10:	// EGA 640x350x16 mode
					case 0x12:	// EGA 640x480x16 mode
						ClearEGAWindow(BH, CX, DX);
						return 0;
					case 0x13:
						ClearMCGAWindow(BH, CX, DX);
						return 0;
				}
			}
			else if ( 0x06 == AH )		// Scroll Up
			{
				int cnt;
				switch ( BIOSData[SCR_MODE] )
				{
					case 0:
					case 1:
					case 2:
					case 3:	// If in text mode
						// Scroll the window up AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollTextWindow(BH, CX, DX, true);
						return 0;
					case 4:
					case 5:
						// Scroll the window up AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollCGA4Window(BH, CX, DX, true);
						return 0;
					case 6:
						// Scroll the window up AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollCGA6Window(BH, CX, DX, true);
						return 0;
					case 0x0D:	// EGA 320x200x16 mode
					case 0x0E:	// EGA 640x200x16 mode
					case 0x10:	// EGA 640x350x16 mode
					case 0x12:	// EGA 640x480x16 mode
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollEGAWindow(BH, CX, DX, true);
						return 0;
					case 0x13:
						ScrollMCGAWindow(BH, CX, DX, true);
						return 0;
				}
			}
			else						// Scroll down
			{
				int cnt;
				switch ( BIOSData[SCR_MODE] )
				{
					case 0:
					case 1:
					case 2:
					case 3:	// If in text mode
						// Scroll the window down AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollTextWindow(BH, CX, DX, false);
						return 0;
					case 4:
					case 5:
						// Scroll the window down AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollCGA4Window(BH, CX, DX, false);
						return 0;
					case 6:
						// Scroll the window down AL rows
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollCGA6Window(BH, CX, DX, false);
						return 0;
					case 0x0D:	// EGA 320x200x16 mode
					case 0x0E:	// EGA 640x200x16 mode
					case 0x10:	// EGA 640x350x16 mode
					case 0x12:	// EGA 640x480x16 mode
						for ( cnt = 0; cnt < AL; cnt++ )
							ScrollEGAWindow(BH, CX, DX, false);
						return 0;
					case 0x13:
						ScrollMCGAWindow(BH, CX, DX, false);
						return 0;
				}
			}
			break;
		case 0x08:	// VIDEO - READ CHARACTER AND ATTRIBUTE AT CURSOR POSITION
			// BH = page number (00h to number of pages - 1) (see #00010)
			// Return:	AH = character's attribute (text mode only) (see #00014)
			// 			AL = character
			// TODO! Page handling
			SET_AX(((u16 *)CGA_B800)[text_pos_by_page_cursor(BH)>>1]);
			return 0;
		case 0x09:	// VIDEO - WRITE CHARACTER AND ATTRIBUTE AT CURSOR POSITION
			// AL = character to display
			// BH = page number (00h to number of pages - 1) (see #00010)
			// 			background color in 256-color graphics modes (ET4000)
			// BL = attribute (text mode) or color (graphics mode)
			// 			if bit 7 set in <256-color graphics mode, character is XOR'ed onto screen
			// CX = number of times to write character
			return WriteCharAtCursor(AL, BH, BL, CX, true);
		case 0x0A:	// VIDEO - WRITE CHARACTER ONLY AT CURSOR POSITION
			// AL = character to display
			// BH = page number (00h to number of pages - 1) (see #00010)
			// 			background color in 256-color graphics modes (ET4000)
			// BL = attribute (text mode) or color (graphics mode)
			// 			if bit 7 set in <256-color graphics mode, character is XOR'ed onto screen
			// CX = number of times to write character
			return WriteCharAtCursor(AL, BH, BL, CX, false);
		case 0x0B:
			// VIDEO - SET BACKGROUND/BORDER COLOR
			// AH = 0Bh
			// BH = 00h
			// BL = background/border color (border only in text modes)
			//
			// VIDEO - SET PALETTE
			// AH = 0Bh
			// BH = 01h
			// BL = palette ID
			// 00h background, green, red, and brown/yellow
			// 01h background, cyan, magenta, and white
			// Return:Nothing
			//
			// Note: This call was only valid in 320x200 graphics on the CGA, but newer cards support it in many or all graphics modes 
			if ( 1 == BH )
			{
				if ( 1 == BL )
					out_3D9_CGA_Color(0, CGA_3D9_value | 0x20);		// Cyan/Magenta/White
				else
					out_3D9_CGA_Color(0, CGA_3D9_value & 0xDF);		// Green/Red/Yellow
			}
			return 0;
		case 0x0C:	// VIDEO - WRITE GRAPHICS PIXEL
			// BH = page number
			// AL = pixel color
			// 		if bit 7 set, value is XOR'ed onto screen except in 256-color modes
			// CX = column
			// DX = row
			// Return:Nothing
			//
			// Desc: Set a single pixel on the display in graphics modes 
			//
			// Notes: Valid only in graphics modes. BH is ignored if the current video mode supports only one page 
			//
			switch ( BIOSData[SCR_MODE] )
			{
				case 0x04:
				case 0x05:
					// CGA mode
					PixelToCGA4Screen( AL, CX, DX);
					return 0;
				case 0x06:
					// CGA mode
					PixelToCGA6Screen( AL, CX, DX);
					return 0;
				case 0x0D:
				case 0x0E:
				case 0x10:
				case 0x12:
					// EGA mode
					PixelToEGAScreen( AL, CX, DX);
					return 0;
				case 0x13:
					// MCGA mode
					PixelToMCGAScreen( AL, CX, DX);
					return 0;
			}
			break;
		case 0x0D:	// VIDEO - READ GRAPHICS PIXEL
			// BH = page number
			// CX = column
			// DX = row
			// Return:AL = pixel color
			//
			// Desc: Determine the current color of the specified pixel in grahics modes 
			//
			// Notes: Valid only in graphics modes. BH is ignored if the current video mode supports only one page 
			switch ( BIOSData[SCR_MODE] )
			{
				case 0x04:
				case 0x05:
					// CGA mode
					SET_AX(PixelFromCGA4Screen(CX, DX));
					return 0;
				case 0x06:
					// CGA mode
					SET_AX(PixelFromCGA6Screen(CX, DX));
					return 0;
				case 0x0D:
				case 0x0E:
				case 0x10:
				case 0x12:
					// EGA mode
					SET_AX(PixelFromEGAScreen(CX, DX));
					return 0;
				case 0x13:
					// MCGA mode
					SET_AX(PixelFromMCGAScreen( CX, DX));
					return 0;
			}
			break;
		case 0x0E:	// VIDEO - TELETYPE OUTPUT
			// AH = 0Eh
			// AL = character to write
			// BH = page number (ignored, current page used in DOSBox!)
			// BL = foreground color (graphics modes only)
			// Return:Nothing
			//
			// Desc: Display a character on the screen, advancing the cursor and scrolling the screen as necessary 
			return Teletype(AL, BIOSData[SCR_PAGE], BL, false);
		case 0x0F:	// VIDEO - GET CURRENT VIDEO MODE
			// Return:	AH = number of character columns
			// 			AL = display mode (see #00010 at AH=00h)
			// 			BH = active page (see AH=05h)
			SET_AX(BIOSData[SCR_MODE]|(BIOSData[SCR_COLS]<<8));	// mode and number of columns
			SET_BH(BIOSData[SCR_PAGE]);		// page
			return 0;
		case 0x10:
			switch ( AX )
			{
				case 0x1000:	// VIDEO - SET SINGLE PALETTE REGISTER (PCjr,Tandy,EGA,MCGA,VGA)
					// BL = palette register number (00h-0Fh)
					//	  = attribute register number (undocumented) (see #00017)
					// BH = color or attribute register value
					// Return:Nothing
					if (BL <= 0x14)
						Out3C0(BL, BH);
					return 0;
				case 0x1001:	// VIDEO - SET BORDER (OVERSCAN) COLOR (PCjr,Tandy,EGA,VGA)
					// BH = border color (00h-3Fh)
					// Return:Nothing
					Out3C0(17, BH);	// Border color
					return 0;
				case 0x1002:	// VIDEO - SET ALL PALETTE REGISTERS (PCjr,Tandy,EGA,VGA)
					// ES:DX -> palette register list (see #00018)
					// Return:Nothing
					//
					// Format of palette register list:
					//
					// Offset  Size    Description     (Table 00018)
					// 00h 16 BYTEs   colors for palette registers 00h through 0Fh
					// 10h    BYTE    border color
					for ( i=0; i < 16; i++ )
					{
						Out3C0(i, ES_DX_PTR[i] );
					}
					Out3C0(17, ES_DX_PTR[16] );	// Border color
					return 0;
				case 0x1003:	// VIDEO - TOGGLE INTENSITY/BLINKING BIT (Jr, PS, TANDY 1000, EGA, VGA)
					// BL = new state
					//	00h background intensity enabled
					//	01h blink enabled
					// Return:Nothing
					return 0;
				case 0x1007:	// VIDEO - GET INDIVIDUAL PALETTE REGISTER (VGA,UltraVision v2+)
					// BL = palette or attribute (undoc) register number (see #00017)
					// Return:
					//	BH = palette or attribute register value
					SET_BH(VGA_attr_data_3C0[BL]);
					return 0;
				case 0x1008:	// VIDEO - READ OVERSCAN (BORDER COLOR) REGISTER (VGA,UltraVision v2+)	(Dominus install)
					SET_BH(VGA_attr_data_3C0[17]);
					return 0;
				case 0x1009:	// VIDEO - READ ALL PALETTE REGISTERS AND OVERSCAN REGISTER (VGA)
					// ES:DX -> 17-byte buffer for palette register list (see #00018)
					memcpy( ES_DX_PTR, VGA_attr_data_3C0, 16 );
					ES_DX_PTR[16] = VGA_attr_data_3C0[17];			// Border color
					return 0;
				case 0x1010:	// VIDEO - SET INDIVIDUAL DAC REGISTER (VGA/MCGA)
					// BX = register number
					// CH = new value for green (0-63)
					// CL = new value for blue (0-63)
					// DH = new value for red (0-63)
					out_3C8_VGA_pel(0, BX);	// Set the starting register
					if ( BIOSData[SCR_MODE] >= 0x13 )
					{
						out_3C9_VGA_pel( 0, DH );
						out_3C9_VGA_pel( 0, CH );
						out_3C9_VGA_pel( 0, CL );
					}
					else
					{
						out_3C9_EGA_pel( 0, DH );
						out_3C9_EGA_pel( 0, CH );
						out_3C9_EGA_pel( 0, CL );
					}
					return 0;
				case 0x1012:	// VIDEO - SET BLOCK OF DAC REGISTERS (VGA/MCGA)
					// BX = starting color register
					// CX = number of registers to set
					// ES:DX -> table of 3*CX bytes where each 3 byte group represents one
					// byte each of red, green and blue (0-63)
					// Return:Nothing
					{
						u8 *tbl = (u8 *)ES_DX_PTR;
						out_3C8_VGA_pel(0, BX);	// Set the starting register
						if ( BIOSData[SCR_MODE] >= 0x13 )
							for ( i = 0; i < CX*3; i++ )
								out_3C9_VGA_pel( 0, tbl[i] );
						else
							for ( i = 0; i < CX*3; i++ )
								out_3C9_EGA_pel( 0, tbl[i] );
					}
					return 0;
				case 0x1013:	// VIDEO - SELECT VIDEO DAC COLOR PAGE (VGA)
					// AX = 1013h
					// BL = subfunction
					//	00h select paging mode
					// 		BH = 00h select 4 blocks of 64
					// 		BH = 01h select 16 blocks of 16
					//	01h select page
					//		BH = page number (00h to 03h) or (00h to 0Fh)
					//
					// Return:Nothing
					// Note: This function is not valid in mode 13h
					if ( 0 == BL )
					{
						u8 old10 = VGA_attr_data_3C0[0x10];
						if ( BH )
							Out3C0(0x10, old10|0x80);
						else
							Out3C0(0x10, old10&0x7F);
					}
					return 0;
				case 0x1015:	// VIDEO - READ INDIVIDUAL DAC REGISTER (VGA/MCGA)
					// BL = palette register number
					// Return:	DH = red value		(pal>>11)&31
					// 			CH = green value	(pal>>6)&31
					// 			CL = blue value		pal&31
					{
						int pal = BG_PALETTE[BL];
						SET_DH((pal>>(11-1))&(31<<1));
						SET_CX((((pal>>6)&31)<<9) | (pal&31));
					}
					return 0;
				case 0x1017:	// VIDEO - READ BLOCK OF DAC REGISTERS (VGA/MCGA)
					// BX = starting palette register
					// CX = number of palette registers to read
					// ES:DX -> buffer (3 * CX bytes in size) (see also AX=1012h)
					// Return:Buffer filled with CX red, green and blue triples
					{
						int i;
						u8 *tbl = (u8 *)ES_DX_PTR;
						int start = BX;
						for ( i = 0; i < CX; i++ )
						{
							int pal = BG_PALETTE[i+start];
							tbl[i*3] = (pal>>(11-1))&(31<<1);	// Red
							tbl[i*3+1] = ((pal>>6)&31)<<1;		// Green
							tbl[i*3+2] = (pal&31)<<1;			// Blue
						}
					}
					return 0;
				case 0x1018:	// VIDEO - SET PEL MASK (VGA/MCGA)
					// BL = new PEL value
					// Return:Nothing
					VGA_pel_mask_3C6 = BL;
					return 0;
				case 0x1019:	// VIDEO - READ PEL MASK (VGA/MCGA)
					// Return:BL = value read
					SET_BL(VGA_pel_mask_3C6);
					return 0;
				case 0x101A:	// VIDEO - GET VIDEO DAC COLOR-PAGE STATE (VGA)
					// Return:BL = paging mode
					//	00h four pages of 64
					//	01h sixteen pages of 16
					// BH = current page
					SET_BX(0);		// TODO!
					return 0;
				case 0x101B:	// VIDEO - PERFORM GRAY-SCALE SUMMING (VGA/MCGA)
					// BX = starting palette register
					// CX = number of registers to convert
					// Return:Nothing
					PerformGrayScaleSumming(BX, CX);
					return 0;
			}
			break;
		case 0x11:
			if ( 0x1100 == AX )			// VIDEO - TEXT-MODE CHARGEN - LOAD USER-SPECIFIED PATTERNS (PS,EGA,VGA)
			{
				// ES:BP -> user table
				// CX = count of patterns to store
				// DX = character offset into map 2 block
				// BL = block to load in map 2
				// BH = number of bytes per character pattern
				// Return:Nothing
				return 0;
			}
			else if ( 0x1103 == AX )	// VIDEO - TEXT-MODE CHARGEN - SET BLOCK SPECIFIER (PS,EGA,VGA)
			{
				// BL = block specifier (see #00020)
				// Return:Nothing
				// 
				// Notes: (see also AX=1110h). This function allows dual character sets to appear on screen simultaneously 
				//
				// Bitfields for block specifier:
				//
				// Bit(s)  Description     (Table 00020)
				// ---EGA/MCGA---
				// 0,1    block selected by characters with attribute bit 3 clear
				// 2,3    block selected by characters with attribute bit 3 set
				// ---VGA---
				// 0,1,4  block selected by characters with attribute bit 3 clear
				// 2,3,5  block selected by characters with attribute bit 3 set
				return 0;
			}
			else if ( 0x1110 == AX )	// VIDEO - TEXT-MODE CHARGEN - LOAD USER-SPECIFIED PATTERNS (PS,EGA,VGA)
			{
				// TODO! Used in Fire & Ice
				// Return:Nothing
				return 0;
			}
			else if ( 0x1112 == AX )	// VIDEO - TEXT-MODE CHARGEN - LOAD ROM 8x8 DBL-DOT PATTERNS (PS,EGA,VGA)
			{
				// BL = block to load
				// Return:Nothing
				BIOSData[SCR_ROWS] = 49;	// 80x50 text mode
				BIOSData[CHAR_HEIGHT] = 8;
				MaxScreenY = 50*8;
				return 0;
			}
			else if ( 0x1114 == AX )	// VIDEO - TEXT-MODE CHARGEN - LOAD ROM 8x16 CHARACTER SET (VGA)
			{
				// BL = block to load
				// Return:Nothing
				BIOSData[SCR_ROWS] = 23;	// 80x24 text mode
				BIOSData[CHAR_HEIGHT] = 16;
				MaxScreenY = 25*16;
				return 0;
			}
			else if ( 0x1130 == AX )	// VIDEO - GET FONT INFORMATION (EGA, MCGA, VGA)
			{
				// BH = pointer specifier
				// 00h INT 1Fh pointer
				// 01h INT 43h pointer
				// 02h ROM 8x14 character font pointer
				// 03h ROM 8x8 double dot font pointer (C000:D412)
				// 04h ROM 8x8 double dot font (high 128 characters)
				// 05h ROM alpha alternate (9 by 14) pointer (EGA,VGA)
				// 06h ROM 8x16 font (MCGA, VGA)
				// 07h ROM alternate 9x16 font (VGA only) (see #00021)
				// 11h (UltraVision v2+) 8x20 font (VGA) or 8x19 font (autosync EGA)
				// 12h (UltraVision v2+) 8x10 font (VGA) or 8x11 font (autosync EGA)
				//
				// Return:ES:BP = specified pointer
				// CX    = bytes/character of on-screen font (not the requested font!)
				// DL    = highest character row on screen
				SET_CX(BIOSData[CHAR_HEIGHT]);
				SET_DL(BIOSData[SCR_ROWS]);
				switch ( BH )
				{
					case 0x00:	// 00h INT 1Fh pointer 
						SET_BP(INTVectors[0x1F]);
						REG_ES = (INTVectors[0x1F]>>16)&0xFFFF;
						return 0;
					case 0x01:
						SET_BP(INTVectors[0x43]);
						REG_ES = (INTVectors[0x43]>>16)&0xFFFF;
						return 0;
					case 0x02:	// 02h ROM 8x14 font pointer
					case 0x05:	// 05h ROM alpha alternate (9 by 14) pointer (EGA,VGA)
						SET_BP(ROM8x14Font);	// Point to font data in BIOS
						REG_ES = 0xF000;
						return 0;
					case 0x03:	// 03h ROM 8x8 double dot font pointer
						SET_BP(ROM8x8Font);	// Point to font data in BIOS
						REG_ES = 0xF000;
						return 0;
					case 0x04:	// 04h ROM 8x8 double dot font (high 128 characters)
						SET_BP(ROM8x8Upper);	// Point to font data in BIOS
						REG_ES = 0xF000;
						return 0;
					case 0x06:	// 06h ROM 8x16 font (MCGA, VGA)
					case 0x07:	// 07h ROM alternate 9x16 font (VGA only) (see #00021)
						SET_BP(ROM8x16Font);	// Point to font data in BIOS
						REG_ES = 0xF000;
						return 0;
					default:
						break;
				}
			}
			break;
		case 0x12:
			switch ( BL )
			{
				case 0x00:	// Used by Prince of Persia, not a real VGA BIOS call!
				case 0x08:	// Used by Blues Brothers, not a real VGA BIOS call!
				case 0x20:	// VIDEO - ALTERNATE FUNCTION SELECT (PS,EGA,VGA,MCGA) - ALTERNATE PRTSC
				case 0x80:	// Heretic, Warcraft Setup: Cirrus Logic BIOS - INQUIRE VGA TYPE
					return 0;
				case 0x10:
					// VIDEO - ALTERNATE FUNCTION SELECT (PS, EGA, VGA, MCGA) - GET EGA INFO
					// AH = 12h
					// BL = 10h
					// Return:BH = video state
					// 		00h color mode in effect (I/O port 3Dxh)
					// 		01h mono mode in effect (I/O port 3Bxh)
					// BL = installed memory (00h = 64K, 01h = 128K, 02h = 192K, 03h = 256K)
					// CH = feature connector bits (see #00022)
					// CL = switch settings (see #00023,#00024)
					// AH destroyed (at least by Tseng ET4000 BIOS v8.00n)
					//
					// Note: One possible check for the presence of an EGA or later display card is to call this function with BH=FFh;
					// if not present, BH will be unchanged on return. Another installation check is used by Athena Digital's HGCIBM,
					// which sets CX to FFFFh on calling and checks whether it has been changed on return 
					//
					// Index:Installation check;EGA
					//
					// Bitfields for feature connector bits:
					// 
					// Bit(s)  Description     (Table 00022)
					// 0      FEAT 1 line, state 2
					// 1      FEAT 0 line, state 2
					// 2      FEAT 1 line, state 1
					// 3      FEAT 0 line, state 1
					// 4-7    unused (0)
					//
					// Bitfields for switch settings:
					//
					// Bit(s)  Description     (Table 00023)
					// 0      switch 1 OFF
					// 1      switch 2 OFF
					// 2      switch 3 OFF
					// 3      switch 4 OFF
					// 4-7    unused
					//
					//
					// (Table 00024)
					// Values for switch settings on original EGA/VGA:
					// 00h            primary MDA/HGC,        secondary EGA+ 40x25
					// 01h-03h        primary MDA/HGC,        secondary EGA+ 80x25
					// 04h            primary CGA 40x25,      secondary EGA+ 80x25 mono
					// 05h            primary CGA 80x25,      secondary EGA+ 80x25 mono
					// 06h            primary EGA+ 40x25,     secondary MDA/HGC (optional)
					// 07h-09h        primary EGA+ 80x25,     secondary MDA/HGC (optional)
					// 0Ah            primary EGA+ 80x25 mono,secondary CGA 40x25 (optional)
					// 0Bh            primary EGA+ 80x25 mono,secondary CGA 80x25 (optional)
					//
					// Category: Video - Int 10h - V 
					SET_BX(0x0003);	// Color mode, 256K installed memory
					SET_CX(0x0009);	// primary EGA+ 80x25, secondary MDA/HGC (optional)
					return 0;
				case 0x30:	//VIDEO - ALTERNATE FUNCTION SELECT (VGA) - SELECT VERTICAL RESOLUTION
					//AL = vertical resolution
					//00h 200 scan lines
					//01h 350 scan lines
					//02h 400 scan lines
					//Return:AL = 12h if function supported
					//
					//Desc: Specify the number of scan lines used to display text modes 
					//
					//Note: The specified resolution will take effect on the next mode set 
					//
					if (AL > 2)
						SET_AL(0);
					else
						SET_AL(0x12);	// Fake success, for Windows 3.11
					return 0;
				case 0x31:	// VIDEO - ALTERNATE FUNCTION SELECT (VGA, MCGA) - PALETTE LOADING
					// AL = new state
					// 00h enable default palette loading
					// 01h disable default palette loading
					// Return:AL = 12h if function supported
					// Desc: Specify whether a default palette should be loaded when the display mode is set 
					return 0;
				case 0x32:	// VIDEO - ALTERNATE FUNCTION SELECT (VGA, MCGA) - VIDEO ADDRESSING
					// AL = new state
					// 00h enable video addressing
					// 01h disable video addressing
					// Return:AL = 12h if function supported
					SET_AX(0x1212);
					return 0;
				case 0x33:	// VIDEO - ALTERNATE FUNCTION SELECT (VGA, MCGA) - GRAY-SCALE SUMMING
					// AL = new state
					//	00h enable gray scale summing
					//	01h disable gray scale summing
					// Return:
					//	AL = 12h if function supported
					return 0;
				case 0x34:	// VIDEO - ALTERNATE FUNCTION SELECT (VGA) - CURSOR EMULATION
					// AL = new state
					//	00h enable alphanumeric cursor emulation
					//	01h disable alphanumeric cursor emulation
					// Return:
					//	AL = 12h if function supported
					return 0;
				case 0x36:	// VIDEO - ALTERNATE FUNCTION SELECT (PS, VGA) - VIDEO REFRESH CONTROL
					// AL = new state
					// 00h enable refresh
					// 01h disable refresh
					// Return:AL = 12h if function supported
					// Desc: Specify whether the contents of video memory should be displayed on the screen;
					// disabling refresh effectively blanks the screen 
					// Note: When display refresh is disabled, the entire screen displays the color specified by the DAC color register 00h;
					// thus to actually blank the screen, the application must first ensure that that register has been set to black
					return 0;
				default:
					if ( 0x55 == BH )	// VIDEO - ALTERNATE FUNC SELECT (ATI,Tatung,Taxan) - ENHANCED FEATURES
						return 0;
			}
			break;
		case 0x13:	// VIDEO - WRITE STRING (AT and later,EGA)
			// AL = write mode
			// 	bit 0:Update cursor after writing
			// 	bit 1:String contains alternating characters and attributes
			// 	bits 2-7:Reserved (0).
			// BH = page number.
			// BL = attribute if string contains only characters.
			// CX = number of characters in string.
			// DH,DL = row,column at which to start writing.
			// ES:BP -> string to write
			{
				unsigned char *str = (unsigned char *)ES_BP_PTR;
				int page = BH & 7;
				int col = BIOSData[CURSOR_COL+(page<<1)];		// Save current cursor column
				int row = BIOSData[CURSOR_ROW+(page<<1)];		// Save current cursor row
				BIOSData[CURSOR_COL+(page<<1)] = DL;
				BIOSData[CURSOR_ROW+(page<<1)] = DH;
				if ( AL & 2 )
				{
					// Character/attribute pairs
					for ( i=0; i < CX; i++ )
						Teletype(str[i<<1], page, str[1+(i<<1)], true );
				}
				else
				{
					// Characters only
					for ( i=0; i < CX; i++ )
						Teletype(str[i], page, BL, true );
				}
				if ( 0 == (AL&1) )
				{
					// Restore cursor position
					BIOSData[CURSOR_COL+(page<<1)] = col;
					BIOSData[CURSOR_ROW+(page<<1)] = row;
				}
			}
			return 0;
		case 0x1A:
			if ( 0 == AL )
			{
				// VIDEO - GET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
				// AX = 1A00h
				// Return:AL = 1Ah if function was supported
				// BL = active display code (see #00039)
				// BH = alternate display code (see #00039)
				//
				// Notes: This function is commonly used to check for the presence of a VGA. 
				//
				// (Table 00039)
				// Values for display combination code:
				// 00h    no display
				// 01h    monochrome adapter w/ monochrome display
				// 02h    CGA w/ color display
				// 03h    reserved
				// 04h    EGA w/ color display
				// 05h    EGA w/ monochrome display
				// 06h    PGA w/ color display
				// 07h    VGA w/ monochrome analog display
				// 08h    VGA w/ color analog display
				// 09h    reserved
				// 0Ah    MCGA w/ digital color display
				// 0Bh    MCGA w/ monochrome analog display
				// 0Ch    MCGA w/ color analog display
				// FFh    unknown display type
				SET_AL(0x1A);
				SET_BX(0x0008);
			}
			else
			{
				// VIDEO - SET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
				// AX = 1A01h
				// BL = active display code (see #00039)
				// BH = alternate display code
				// Return:AL = 1Ah if function was supported
			}
			return 0;
		case 0x1B:	// VIDEO - FUNCTIONALITY/STATE INFORMATION (PS,VGA/MCGA)
			// BX = implementation type
			// 0000h return funtionality/state information
			// ES:DI -> 64-byte buffer for state information (see #00040)
			// Return:AL = 1Bh if function supported
			// ES:DI buffer filled with state information
			return 0;
		case 0x1C:	// VIDEO - SAVE/RESTORE VIDEO STATE (PS50+,VGA)
			// AL = function
			// 00h return state buffer size
			// Return:BX = number of 64-byte blocks needed
			// 01h save video state
			// ES:BX -> buffer
			// 02h restore video state
			// ES:BX -> buffer containing previously saved state.
			// CX = requested states (see #00048)
			// Return:AL = 1Ch if function supported
			return 0;
		case 0x4F:	// VESA SuperVGA BIOS (VBE) - GET SuperVGA INFORMATION
#if USE_VESA_BIOS
#define	PUT_REAL_PTR(pos, offs) {buf[pos] = DI+offs; buf[pos+1] = (DI+offs)>>8; buf[pos+2] = REG_ES; buf[pos+3] = REG_ES>>8;}
			switch(AL)
			{
				case 0x00:	// VESA SuperVGA BIOS (VBE) - GET SuperVGA INFORMATION
					// AX = 4F00h
					// ES:DI -> buffer for SuperVGA information (see #00077)
					//
					// Return:
					// AL = 4Fh if function supported
					// AH = status
					//	00h successful
					//	ES:DI buffer filled
					// 01h failed
					{
						char *buf = (char *)ES_DI_PTR;
						// 00h  4 BYTEs   (call) VESA 2.0 request signature ("VBE2"), required to receive version 2.0 info
						bool vbe2 = (0 == memcmp(buf, "VBE2", 4));
						char *scratch = buf + 256;
						if (vbe2)
						{
							memset(buf, 0, 512);
							// 06h    DWORD   pointer to OEM name
							PUT_REAL_PTR(0x6, scratch-buf);
							scratch += sprintf( scratch, "%s", BIOS_C000+0x100 ) + 1;
							// 14h    WORD    OEM software version (BCD, high byte = major, low byte = minor)
							buf[0x15] = 2;	// Version 2.0
							// 16h    DWORD   pointer to vendor name
							PUT_REAL_PTR(0x16, scratch-buf);
							scratch += sprintf( scratch, "Patrick Aalto" ) + 1;
							// 1Ah    DWORD   pointer to product name
							PUT_REAL_PTR(0x1A, scratch-buf);
							scratch += sprintf( scratch, "DS2x86 DOS emulator for DSTwo" ) + 1;
							// 1Eh    DWORD   pointer to product revision string
							PUT_REAL_PTR(0x1E, scratch-buf);
							sprintf( scratch, "v 1.0" );
						}
						else
						{
							memset(buf, 0, 256);
							// 06h    DWORD   pointer to OEM name (DOSBox: C000:2F01 = "S3 Incorporated. Trio64"
							buf[7] = 1;
							buf[9] = 0xC0;		// DWORD at buf[6] = C000:0100
						}
						// 00h  4 BYTEs   (ret) signature ("VESA")
						memcpy(buf, "VESA", 4);
						// 04h    WORD    VESA version number (one-digit minor version -- 0102h = v1.2)
						buf[5] = 2;		// Version 2.0 (WORD at 0x04 = 0x0200)
						// 0Ah    DWORD   capabilities flags (see #00078) (DOSBox:0)
						// 0Eh    DWORD   pointer to list of supported VESA and OEM video modes (list of words terminated with FFFFh) (DOSBox:C000:2EAB)
						buf[0x0F] = 2;
						buf[0x11] = 0xC0;		// DWORD at buf[0x0E] = C000:0200
						// 12h    WORD    total amount of video memory in 64K blocks (DOSBox:0x20 = 2048KB)
						buf[0x12] = 8;	// 8*64 = 512 KB
					}
					SET_AX(0x004F);		// Supported function, OK return.
					return 0;
				case 0x01:	// VESA SuperVGA BIOS - GET SuperVGA MODE INFORMATION
					// AX = 4F01h
					// CX = SuperVGA video mode (see #04082 for bitfields)
					// ES:DI -> 256-byte buffer for mode information (see #00079)
					//
					// Return:
					// AL = 4Fh if function supported
					// AH = status
					//	00h successful
					//	ES:DI buffer filled
					// 01h failed
					//
					// Bitfields for VESA/VBE video mode number:
					//
					// Bit(s)  Description     (Table 04082)
					// 15     preserve display memory on mode change
					// 14     (VBE v2.0+) use linear (flat) frame buffer
					// 13     (VBE/AF 1.0P) VBE/AF initializes accelerator hardware
					// 12     reserved for VBE/AF
					// 11     (VBE v3.0) user user-specified CRTC refresh rate values
					// 10-9   reserved for future expansion
					// 8-0    video mode number (0xxh are non-VESA modes, 1xxh are VESA-defined)
					if (CX != 0x100 && CX != 0x101)
					{
						// Unsupported mode, so the call failed!
						SET_AX(0x014F);
						return 0;
					}
					else
					{
						// Supported mode, either 0x100 (640x400x256) or 0x101 (640x480x256)
						MODE_INFO minfo;
						u8 *buf = (unsigned char *)ES_DI_PTR;
						memset(&minfo,0,sizeof(minfo));
						minfo.BytesPerScanLine = 640;
						minfo.NumberOfPlanes = 1;
						minfo.BitsPerPixel = 8;
						minfo.MemoryModel = 4;			// Packed pixel
						minfo.WinAAttributes = 7;		// Exists/Readable/Writable
						minfo.ModeAttributes = 0x1B;	// Graphics, Color, VBE 1.2+ info available, supported
						minfo.NumberOfImagePages = (0x100 == CX) ? 1 : 0;
						minfo.WinGranularity = 64;
						minfo.WinSize = 64;
						minfo.WinASegment = 0xA000;
						minfo.XResolution = 640;
						minfo.YResolution = (0x100 == CX) ? 400 : 480;
						minfo.WinFuncPtr = 0xC0000220;	// C000:0220 = FAR routine for SVGA bank switching.
						minfo.NumberOfBanks = 1;
						minfo.Reserved_page = 1;	// ??
						minfo.XCharSize = 8;
						minfo.YCharSize = 8;
						memcpy( buf, &minfo, sizeof(minfo) );
					}
					SET_AX(0x004F);		// Supported function, OK return.
					return 0;
				case 0x02:	// VESA SuperVGA BIOS - SET SuperVGA VIDEO MODE
					// AX = 4F02h
					// BX = new video mode (see #04082,#00083,#00084)
					switch(BX)
					{
						case 0x100:
							SetupVideoParameters( 0x13 );
							BIOSData[SCR_MODE] = 0x1C;
							break;
						case 0x101:
							SetupVideoParameters( 0x13 );
							BIOSData[SCR_MODE] = 0x1D;
							break;
						default:
							SET_AX(0x014F);		// Failed!
							return 0;
					}
					VESA_Bank = 0;
					VESA_Start = 0;
					memset(EGAVGA_A000, 0, 512*1024);
#ifdef RPi
					memcpy(BG_PALETTE, DefaultPalette, 32*2);
					BG_PALETTE[256] = 1;	// Flag that the palette has changed
#else
					memcpy(BG_PALETTE, DefaultPalette, 32*4);
#endif
					VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;	// Address of the routine that handles output for port 0x3C9
					ScreenConfigChanged();
					SET_AX(0x004F);	// Success!
					return 0;
				case 0x03:	// VESA SuperVGA BIOS - GET CURRENT VIDEO MODE
					// Return:
					// AL = 4Fh if function supported
					// AH = status
					//	00h successful
					//	BX = video mode (see #00083,#00084)
					switch(BIOSData[SCR_MODE])
					{
						case 0x1C:
							SET_BX(0x100);	// 640x400x256 VESA mode
							break;
						case 0x1D:
							SET_BX(0x101);	// 640x480x256 VESA mode
							break;
						default:
							SET_BX(BIOSData[SCR_MODE]);
							break;
					}
					SET_AX(0x004F);		// Supported function, OK return.
					return 0;
				case 0x05:	// VESA SuperVGA BIOS - CPU VIDEO MEMORY CONTROL
					// BH = subfunction
					//	00h select video memory window
					//		DX = window address in video memory (in granularity units)
					//	01h get video memory window
					//		Return:
					//		DX = window address in video memory (in gran. units).
					//		BL = window number
					//			00h window A
					//			01h window B.
					//		ES = selector for memory-mapped registers (VBE 2.0+, when called from 32-bit protected mode)
					//
					// Return:
					//	AL = 4Fh if function supported
					//	AH = status
					//		00h successful
					//		01h failed
					if (0 == BH)
					{
						if (0 == BL)
						{
							VESA_Bank = DX<<16;		// Set the VRAM start address
							Setup_EGAVGA_A000();	// Go adjust the EMSPages[] table values
							SET_AX(0x004F);			// Supported function, OK return.
						}
						else
							SET_AX(0x014F);			// Window B not supported!
						return 0;
					}
					else if (1 == BH)
					{
						SET_DX(VESA_Bank>>16);
						SET_BL(0);
						SET_AX(0x004F);			// Supported function, OK return.
						return 0;
					}
					SET_AX(0x014F);				// Unknown function!
					return -1;
				case 0x07:	// VESA SuperVGA BIOS v1.1+ - GET/SET DISPLAY START
					if (1 == BL)
					{
						// Get display start
						SET_CX(0);	// Leftmost displayed pixel
						SET_DX(0);	// First displayed scanline
						SET_BH(0);	// Reserved
					}
					SET_AX(0x004F);			// Supported function, OK return.
					return 0;
				case 0x0A:	// VESA SuperVGA BIOS v2.0+ - GET PROTECTED-MODE INTERFACE
					switch(BL)
					{
						case 0:
							// Return:
							// AL = 4Fh if function supported
							// AH = status
							//	00h successful
							//		ES:DI -> protected-mode table (see #00087)
							//		CX = length of table in bytes, included protected-mode code
							//	01h failed
							REG_ES = 0xC000;
							SET_DI(0x0230);
							SET_CX(sizeof(VESAProtMode));
							SET_AX(0x004F);			// Supported function, OK return.
							return 0;
						case 1:
							break;
						case 2:
							break;
						case 3:
							break;
					}
					SET_AX(0x014F);			// Supported function, error return.
					return -1;
			}
			return -1;
#endif
		case 0x30:	// VIDEO - LOCATE 3270PC CONFIGURATION TABLE (INSTALLATION CHECK)
		case 0x50:	// VIDEO - AX PC - GET SCREEN COUNTRY CODE
		case 0x5F:	// Realtek RTVGA - RETURN CHIP VERSION
		case 0x6F:	// Video7 VGA,VEGA VGA - INSTALLATION CHECK
		case 0x70:	// VIDEO - TANDY 2000 only - GET ADDRESS OF VIDEO RAM		
		case 0x8F:	// SILCITY - ??
		case 0xBF:	// VIDEO - Compaq Extensions  (B.A.T. 2)
		case 0xCB:	// UNCHAIN - SAVE CURRENT VGA REGISTERS
		case 0xCC:	// UltraVision - GET STATUS (INSTALLATION CHECK)
		case 0xEF:	// MSHERC.COM - GET VIDEO ADAPTER TYPE AND MODE
			return 0;
		case 0xF1:	// EGA Register Interface Library - WRITE ONE REGISTER	(Needed by A-TRAIN)
			// DX = group index (see #00223)
			// if single register:
			//	BL = value to write
			// otherwise
			//	BL = register number
			//	BH = value to write
			//	Return:BL = data
			//
			// 	00h    CRT Controller (25 reg) 3B4h mono modes, 3D4h color modes
			// 	08h    Sequencer (5 registers) 3C4h
			// 	10h    Graphics Controller (9 registers) 3CEh
			// 	18h    Attribute Controller (20 registers) 3C0h
			// Single registers
			// 	20h    Miscellaneous Output register 3C2h
			// 	28h    Feature Control register (3BAh mono modes, 3DAh color modes)
			// 	30h    Graphics 1 Position register 3CCh
			// 	38h    Graphics 2 Position register 3CAh
			switch ( DX )
			{
				case 0x0008:	// 	08h    Sequencer (5 registers) 3C4h
					out_3C4_VGA_sequencer(0, BL);
					out_3C5_VGA_sequencer(0, BH);
					return 0;
				case 0x0010:	// 	10h    Graphics Controller (9 registers) 3CEh
					Out3CF(BL, BH);
					return 0;
				case 0x020:		//	20h    Miscellaneous Output register 3C2h	(Dragon in Windows 3)
					VGA_misc_3C2 = BL;
					ScreenConfigChanged();
					return 0;
				case 0x028:		//	28h    Feature Control register (3DAh color modes)	(Dragon in Windows 3)
					// Writing to port 0x3DA is ignored.
					return 0;
			}
			return -1;
		case 0xFA:	// EGA Register Interface Library - INTERROGATE DRIVER
		case 0xFE:	// TopView - GET SHADOW BUFFER
		case 0xFF:	// TopView - UPDATE SCREEN FROM SHADOW BUFFER
			return 0;
	}
	LOGI("Unsupported int 10h AH=%02X\n", AH );
	return -1;
}

void Setup_EGAVGA_A000()
{
	int	i, value = ((u32)EGAVGA_A000-0xA0000)>>4;
	//------
	// Setup the EGAVGA_A000 area into EMSPages.
	//------
	switch (BIOSData[0x49])
	{
		case 0x0D:
		case 0x0E:
		case 0x0F:
		case 0x10:
		case 0x11:
		case 0x12:
			//------
			// Graphics mode is EGA! Set the EGA flag bit on.
			//------
			value = (((u32)EGAVGA_A000>>6)-(0xA0000>>4)) | PAGE_FLAG_EGA;
			break;
		case 0x13:
			if ( 0 == (VGAMemoryMode & 0x08) )				// Chain-Four is OFF, this is Mode-X!
			{
				//------
				// Graphics mode is Mode-X! Set the Mode-X flag bit on.
				//------
				value = (((u32)EGAVGA_A000>>6)-(0xA0000>>4)) | PAGE_FLAG_MODEX;
			}
			break;
		case 0x1C:
		case 0x1D:
			//------
			// Use the VESA_Bank to determine the actual memory address of the A000 segment
			//------
			value += (VESA_Bank>>4);
			break;
	}
	for (i=0xA0000>>EMSPAGESHIFT; i < (0xB0000>>EMSPAGESHIFT); i++)
		EMSPages[i] = value;
	//LOGI("Setup_EGAVGA_A000 value=%08X, (%dx%d)\n", value, MaxScreenX, MaxScreenY);
}

const u8 VESAModeList[] = {0x00, 0x01, 0x01, 0x01, 0xFF, 0xFF};		// Supported modes = 0x100, 0x101

const u8 SVGABankSwitch[] = {0xFE, 0x38, 0x00, 0x00, 0xCB};			// callback 0, RETF

void InitVGABIOS(const char *bootString)
{
	BIOS_C000[0] = 0x55;
	BIOS_C000[1] = 0xAA;
	BIOS_C000[2] = 0x40;	// Size of ROM: 64 512-blocks = 32KB
	memcpy( BIOS_C000+0x1E, "IBM", 3 );
	memcpy( BIOS_C000+0x100, bootString, strlen(bootString));	// OEM string
	memcpy( BIOS_C000+0x200, VESAModeList, sizeof(VESAModeList));
	memcpy( BIOS_C000+0x220, SVGABankSwitch, sizeof(SVGABankSwitch));
	memcpy( BIOS_C000+0x230, VESAProtMode, sizeof(VESAProtMode));
	memcpy( BIOS_C000+0x1000, &VideoParameterTable, 64*0x14);
}

#ifdef RII_EX

extern u8 VGA_serialize_start;
extern u8 VGA_serialize_end;

// Return the worst-case size of the VGA serialization data.
int vga_serialize_size()
{
	return (512*1024) + sizeof(BG_PALETTE) + sizeof(EGA_PALETTE_SAVE) +
		(&VGA_serialize_end - &VGA_serialize_start) + 4 + 4 + 4 + 4;
}

// Serialize the EMS memory into data + offset.
int vga_serialize(u8 *data, int offset)
{
	memcpy(data + offset, BG_PALETTE, sizeof(BG_PALETTE));
	offset += sizeof(BG_PALETTE);
	memcpy(data + offset, EGA_PALETTE_SAVE, sizeof(EGA_PALETTE_SAVE));
	offset += sizeof(EGA_PALETTE_SAVE);
	memcpy(data + offset, &VGA_serialize_start, &VGA_serialize_end - &VGA_serialize_start);
	offset += (&VGA_serialize_end - &VGA_serialize_start);
	memcpy(data + offset, &VESA_Start, 4); offset += 4;
	memcpy(data + offset, &VESA_Bank, 4); offset += 4;
	memcpy(data + offset, &MaxScreenX, 4); offset += 4;
	memcpy(data + offset, &MaxScreenY, 4); offset += 4;
	// Serialize the EGA/VGA graphics memory
	switch (BIOSData[0x49])
	{
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
		case 0x04:
		case 0x05:
		case 0x06:
			//------
			// We are in text or CGA mode.
			//------
			memcpy(data + offset, CGA_B800, 0x8000);
			offset += 0x8000;
			break;
		case 0x0D:
		case 0x0E:
		case 0x0F:
		case 0x10:
		case 0x11:
		case 0x12:
			//------
			// Graphics mode is EGA!
			//------
			memcpy(data + offset, EGAVGA_A000, 4*65536);
			offset += 4*65536;
			break;
		case 0x13:
			if ( 0 == (VGAMemoryMode & 0x08) )				// Chain-Four is OFF, this is Mode-X!
			{
				//------
				// Graphics mode is Mode-X.
				//------
				memcpy(data + offset, EGAVGA_A000, 4*65536);
				offset += 4*65536;
			}
			else
			{
				//------
				// Graphics mode is MCGA.
				//------
				memcpy(data + offset, EGAVGA_A000, 65536);
				offset += 65536;
			}
			break;
		case 0x1C:
		case 0x1D:
			//------
			// Graphics mode is SVGA.
			//------
			memcpy(data + offset, EGAVGA_A000, 512*1024);
			offset += 512*1024;
			break;
	}
	return offset;
}

// Unserialize the VGA memory from data + offset
// NOTE! Main memory must have been unserialized before this!
int vga_unserialize(u8 *data, int offset)
{
	memcpy(BG_PALETTE, data + offset, sizeof(BG_PALETTE));
	offset += sizeof(BG_PALETTE);
	memcpy(EGA_PALETTE_SAVE, data + offset, sizeof(EGA_PALETTE_SAVE));
	offset += sizeof(EGA_PALETTE_SAVE);
	memcpy(&VGA_serialize_start, data + offset, &VGA_serialize_end - &VGA_serialize_start);
	offset += (&VGA_serialize_end - &VGA_serialize_start);
	memcpy(&VESA_Start, data + offset, 4); offset += 4;
	memcpy(&VESA_Bank, data + offset, 4); offset += 4;
	memcpy(&MaxScreenX, data + offset, 4); offset += 4;
	memcpy(&MaxScreenY, data + offset, 4); offset += 4;
	// Unserialize the EGA/VGA graphics memory
	switch (BIOSData[0x49])
	{
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
			//------
			// We are in text mode.
			//------
			memset(TEXTBuffer, 0, 80*25*2);
			// fall-thru
		case 0x04:
		case 0x05:
		case 0x06:
			//------
			// We are in CGA mode.
			//------
			VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;			// Address of the routine that handles output for port 0x3C9
			memcpy(CGA_B800, data + offset, 0x8000);
			offset += 0x8000;
			break;
		case 0x0D:
		case 0x0E:
		case 0x0F:
		case 0x10:
		case 0x11:
		case 0x12:
			//------
			// Graphics mode is EGA!
			//------
			VGA_out_3C9_addr = (void *)&out_3C9_EGA_pel;	// Address of the routine that handles output for port 0x3C9
			memcpy(EGAVGA_A000, data + offset, 4*65536);
			offset += 4*65536;
			break;
		case 0x13:
			VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;			// Address of the routine that handles output for port 0x3C9
			if ( 0 == (VGAMemoryMode & 0x08) )				// Chain-Four is OFF, this is Mode-X!
			{
				//------
				// Graphics mode is Mode-X.
				//------
				memcpy(EGAVGA_A000, data + offset, 4*65536);
				offset += 4*65536;
			}
			else
			{
				//------
				// Graphics mode is MCGA.
				//------
				memcpy(EGAVGA_A000, data + offset, 65536);
				offset += 65536;
			}
			break;
		case 0x1C:
		case 0x1D:
			//------
			// Graphics mode is SVGA.
			//------
			VGA_out_3C9_addr = (void *)&out_3C9_VGA_pel;			// Address of the routine that handles output for port 0x3C9
			memcpy(EGAVGA_A000, data + offset, 512*1024);
			offset += 512*1024;
			break;
	}
	// Setup the EMSPages for the correct graphics mode.
	Setup_EGAVGA_A000();
	return offset;
}

#endif
