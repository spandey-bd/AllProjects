//=============================================================================
// BIOS_init.c
//
// This file contains InitBIOS() routine, which should be called when starting
// up the emulation, after a call to InitMemory() but before a call to
// InitKernel(). This routine performs the following initialization steps:
//
//	- Setup the INT vectors at 0000:0000 to point to BIOS area.
//	- Create the default INT vector C callback table at 0xF000:0000.
//	- Copy all x86 ASM INT vector handler binaries into emulated 0xF000 segment.
//	- Setup various other BIOS 0xF000 segment data areas.
//	- Setup the BIOS RAM data area at 0x0040:0000.
//	- Setup the VGA BIOS at 0xC000:0000.
//	- Initialize the EMSPages[] memory address translation table.
//	- Initialize the A20 gate state to disabled.
//	- Setup graphics mode 0x03 (80x25 text).
//	- Show the BIOS boot banner on the text screen.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2014 Patrick "Pate" Aalto
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

#include <string.h>
#include "pax86.h"

#include "pacpu.h"
//#include "ints.h"
//#include "config.h"

#include "int08_bin.h"
#include "int09_bin.h"
#include "int0b_bin.h"
#include "int0f_bin.h"
//#include "int10_bin.h"
#include "int11_bin.h"
#include "int12_bin.h"
#include "int16_bin.h"
#include "int21_bin.h"
#include "int67_bin.h"
#include "int74_bin.h"
#include "ScanXlat_bin.h"
#include "ROM8x8_bin.h"
#include "ROM8x14_bin.h"
#include "ROM8x16_bin.h"
#include "BiosDataInit_bin.h"

//------------- BIOS Configuration Table ---------------
// 00h    WORD    number of bytes following
// 02h    BYTE    model (see #00515)
// 03h    BYTE    submodel (see #00515)
// 05h    BYTE    feature byte 1 (see #00510)
// Bitfields for feature byte 1:
// Bit(s)  Description     (Table 00510)
// 7      DMA channel 3 used by hard disk BIOS
// 6      2nd interrupt controller (8259) installed
// 5      Real-Time Clock installed
// 4      INT 15/AH=4Fh called upon INT 09h
// 3      wait for external event (INT 15/AH=41h) supported
// 2      extended BIOS area allocated (usually at top of RAM)
// 1      bus is Micro Channel instead of ISA
// 0      system has dual bus (Micro Channel + ISA)
const u8 ConfTbl[] = {0x08, 0x00, 0xFC, 0x01, 0x00, 0x70};

//------------- BIOS default IRQ1 vector address -------
const u8 DefIRQ1[] = {0xEA, 0x00, 0x01, 0x00, 0xF0};		// JMP F000:0100

//------------- XMS handler -------
// JMP $+3, NOP, NOP, NOP, callback 3, RETF
const u8 XMSHandler[] = {0xEB, 0x03, 0x90, 0x90, 0x90, 0xFE, 0x38, 0x03, 0x00, 0xCB};		

#ifdef RPi
const char BootString[] = "****** rpix86 (Raspberry Pi x86 emulator) v 0.15 ****** by Patrick Aalto ******\r\n\r\n"
						  "BIOS date: "
						  __DATE__
						  "\r\n";
#else
const char BootString[] = "      Powered by pax86 (x86 emulation technology by Patrick Aalto)      ";
#endif

u32	ROM8x8Font;
u32	ROM8x8Upper;
u32 ROM8x14Font;
u32 ROM8x16Font;

extern int INTHandler(int num);

extern int cpu_idt_phys;
extern void InitVGABIOS(const char * str);		// In int10h.cpp
extern void PagingDisabled();
extern void SetA20Gate(int enabled);

//------------------------------------------------------
// Print a string, using x86 INT10h emulation.
//------------------------------------------------------
void printstr(const char *str)
{
	int	i;
	
	for ( i=0; str[i] != 0; i++ )
	{
		SET_AX(0x0E00 | str[i]);
		SET_BX(0);
		INTHandler(0x10);
	}
}

//------------------------------------------------------
// Initialize the BIOS Data area at F000:0000
//------------------------------------------------------
void InitBIOS()
{
	int	curpos = 0, i;
	//--------------------------------------------------
	// Initialize the interrupt vectors
	//--------------------------------------------------
	cpu_idt_phys = (int)INTVectors;
	for (i=0; i < 256; i++)
		INTVectors[i] = 0xF0000000+i;
	INTVectors[7] = 0;
	//INTVectors[0x5C] = 0;		// NetBIOS int handler, checked by Windows 3.0
	INTVectors[0x62] = 0;
	// Syndicate needs these to not hang at startup!
	INTVectors[0x2F4/4] = 0;
	INTVectors[0x2F8/4] = 0;

	BIOS_F000[0x2DE] = 0xCF;	// IRET

	// Emulated INT handlers at F000:0000
	memset( BIOS_F000, 0xD6, 256 );

	// INT09 is at F000:0100 (IRQ1 = keyboard IRQ handler)
	memcpy(BIOS_F000+0x100, int09_bin, int09_bin_size);
	INTVectors[9] = 0xF0000100;

	// BIOS FAT type byte at F000:02DC
	BIOS_F000[0x2DC] = 0xF8;

	// INT1C default handler = IRET
	INTVectors[0x1C] = 0xF00002DE;

	// INT11 is at F000:02E0
	memcpy(BIOS_F000+0x2E0, int11_bin, int11_bin_size);
	INTVectors[0x11] = 0xF00002E0;

	// INT12 is at F000:02F0
	memcpy(BIOS_F000+0x2F0, int12_bin, int12_bin_size);
	INTVectors[0x12] = 0xF00002F0;
	
	// ScanXlat table is at F000:0300 - F000:0890
	memcpy(BIOS_F000+0x300, ScanXlat_bin, ScanXlat_bin_size);
	
	// XMS Handler is at F000:09F0
	memcpy(BIOS_F000+0x09F0, XMSHandler, sizeof(XMSHandler));

	// INT08 is at F000:0A00 (IRQ0 = timer IRQ handler)
	memcpy(BIOS_F000+0xA00, int08_bin, int08_bin_size);
	INTVectors[8] = 0xF0000A00;

	// INT67 is at F000:0A40 == F0A4:0000
	memcpy(BIOS_F000+0xA40, int67_bin, int67_bin_size);
	INTVectors[0x67] = 0xF0A40000;

	// INT21 is at F000:0A60
	curpos = 0xA60;
	memcpy(BIOS_F000+curpos, int21_bin, int21_bin_size);
	INTVectors[0x21] = 0xF0000A60;

	// INT16 is after int21 vector
	curpos += int21_bin_size;
	memcpy(BIOS_F000+curpos, int16_bin, int16_bin_size);
	INTVectors[0x16] = 0xF0000000+curpos;

	// INT10 is after int16 vector
	curpos += int16_bin_size;
//	memcpy(BIOS_F000+curpos, int10_bin, int10_bin_size);
//	INTVectors[0x10] = 0xF0000000+curpos;

	// INT0F is at F000:1000 (IRQ7 = SB default IRQ handler)
	memcpy(BIOS_F000+0x1000, int0f_bin, int0f_bin_size);
	INTVectors[0x0F] = 0xF0001000;

	// Case map routine at F000:1008
	BIOS_F000[0x1008] = 0xCB;	// RETF

	// INT0B is at F000:1020 (IRQ3 = Mouse default IRQ handler)
	memcpy(BIOS_F000+0x1020, int0b_bin, int0b_bin_size);
	// INT74 is at F000:1060 (IRQ12 = PS/2 Mouse IRQ handler)
	memcpy(BIOS_F000+0x1060, int74_bin, int74_bin_size);

	INTVectors[0x0B] = 0xF0001020;
#ifdef USECOM2
	INTVectors[0x74] = 0xF0001020;	// Default IRQ12 points to normal mouse handler!
#else
	INTVectors[0x74] = 0xF0001060;
#endif

	// Copy the ROM fonts to the ROM bios.
	curpos = (0x1060+int74_bin_size+3)&0xFFFC;
	memcpy( BIOS_F000+curpos, ROM8x8_bin, ROM8x8_bin_size);
	ROM8x8Font = 0xF0000000 + curpos;
	ROM8x8Upper = 0xF0000000 + curpos + (ROM8x8_bin_size>>1);
	curpos += ROM8x8_bin_size;
	memcpy( BIOS_F000+curpos, ROM8x14_bin, ROM8x14_bin_size);
	ROM8x14Font = 0xF0000000 + curpos;
	curpos += ROM8x14_bin_size;
	memcpy( BIOS_F000+curpos, ROM8x16_bin, ROM8x16_bin_size);
	ROM8x16Font = 0xF0000000 + curpos;

	// BIOS Copyright is at F000:E000
	strcpy( BIOS_F000+0xE000, "Copyright 2013 Patrick Aalto" );
	
	// Configuration table is at F000h:E6F5h in 100% compatible BIOSes.
	memcpy(BIOS_F000+0xE6F5, ConfTbl, sizeof(ConfTbl));

	// BIOS default IRQ1 vector at F000:E987 (needed by STARGATE)
	memcpy(BIOS_F000+0xE987, DefIRQ1, sizeof(DefIRQ1));

	// INT1F is at F000:FA6E (ROM 8x8 font lower 128 characters)
	memcpy(BIOS_F000+0xFA6E, ROM8x8_bin, ROM8x8_bin_size>>1);
	INTVectors[0x1F] = 0xF000FA6E;

	// BIOS date at F000:FFF5
	strcpy( BIOS_F000+0xFFF5, "02/28/11" );		// MM/DD/YY
	
	// PC type at F000:FFFE
	BIOS_F000[0xFFFE] = 0xFC;	// IBM AT

	//------------------------------------------------------
	// Initialize the BIOS Data area at 0040:0000
	//------------------------------------------------------
	memcpy( BIOSData, BiosDataInit_bin, BiosDataInit_bin_size);

	//------------------------------------------------------
	// Initialize the VGA BIOS area at segment 0xC000
	//------------------------------------------------------
	InitVGABIOS(BootString);

	//------------------------------------------------------
	// Initialize the EMSPages table
	//------------------------------------------------------
#if 0
	PagingDisabled();
#else
	// Normal RAM
	for (i=0>>EMSPAGESHIFT; i < (0xF0000>>EMSPAGESHIFT); i++)
		EMSPages[i] = ((u32)INTVectors)>>4;
	// EGA/VGA area
	for (i=0xA0000>>EMSPAGESHIFT; i < (0xB0000>>EMSPAGESHIFT); i++)
		EMSPages[i] = ((u32)EGAVGA_A000-0xA0000)>>4;
	// BIOS area
	for (i=0xF0000>>EMSPAGESHIFT; i < (0x100000>>EMSPAGESHIFT); i++)
		EMSPages[i] = ((u32)BIOS_F000-0xF0000)>>4;
#endif

	// First 64KB of the second megabyte = wrap-around segment
	SetA20Gate(0);

	//------------------------------------------------------
	// Initialize the main screen (go to text mode 80x25).
	//------------------------------------------------------
	SET_AX(0x0003);
	INTHandler(0x10);

	//------------------------------------------------------
	// Show the BIOS boot string.
	//------------------------------------------------------
#ifdef RPi
	printstr(BootString);
#else
	printstr("\r\n\r\n\r\n\r\n\r\n\r\n");
	for (i = 0; i < 6*80; i++)
		((u16 *)CGA_B800)[i] = 0x19B1;	// Light blue on dark blue checkerboard
	for (i = 1*80 + 4; i < 1*80 + 76; i++)
		((u16 *)CGA_B800)[i] = 0x19CD;	// Light blue on dark blue double line
	for (i = 3*80 + 4; i < 3*80 + 76; i++)
		((u16 *)CGA_B800)[i] = 0x19CD;	// Light blue on dark blue double line
	for (i = 4*80 + 5; i < 4*80 + 79; i++)
		((u16 *)CGA_B800)[i] = 0x10B1;	// Black on dark blue checkerboard
	// Draw corners
	((u16 *)CGA_B800)[1*80+3] = 0x19C9;
	((u16 *)CGA_B800)[1*80+76] = 0x19BB;
	((u16 *)CGA_B800)[3*80+3] = 0x19C8;
	((u16 *)CGA_B800)[3*80+76] = 0x19BC;
	((u16 *)CGA_B800)[2*80+3] = 0x19BA;
	((u16 *)CGA_B800)[2*80+76] = 0x19BA;
	// Draw right shadow area
	((u16 *)CGA_B800)[2*80+77] = 0x10B1;	// Black on dark blue checkerboard
	((u16 *)CGA_B800)[2*80+78] = 0x10B1;	// Black on dark blue checkerboard
	((u16 *)CGA_B800)[3*80+77] = 0x10B1;	// Black on dark blue checkerboard
	((u16 *)CGA_B800)[3*80+78] = 0x10B1;	// Black on dark blue checkerboard
	// And finally the banner text itself
	for (i = 0; i < strlen(BootString); i++)
		((u16 *)CGA_B800)[2*80+4+i] = 0x1F00 + BootString[i];
#endif

}