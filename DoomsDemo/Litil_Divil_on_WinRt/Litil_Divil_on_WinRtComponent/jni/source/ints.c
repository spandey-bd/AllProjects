//=============================================================================
// ints.c
//
// This file is the main software interrupt multiplexer. If the DOS interrupt
// vector points to an area between 0xF000:0000 and 0xF000:00FF, this file
// will get control of the interrupt handling.
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
#include <time.h>

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

#define	TODO	1

extern int int10h();
extern int int21h();
extern int MouseInt15h(int al);
extern int int33h(int ax);
extern int int67h(int ah);
extern int cdrom_2F();
extern void wait_vsync();
#ifdef RPi
extern void exit_program();
#endif

//extern int RAM_END[];			// End of the emulated RAM, for INT15 AH=88 call.

char IntDebug = false;

static void ShowDOSVer(char *line)
{
	sprintf( line+2, "echo DOS %d.%02d\r", dos_sda.DOSVerMajor, dos_sda.DOSVerMinor );
	line[1] = strlen(line+3);
}

// Try to parse a new DOS version number from the string.
// It can be something like "set 4", "4", "SET 4.12", "3.2".
static void ParseDOSVer(const char *str)
{
	int i;
	// Scan for the first number
	for (i = 0; true ; i++ )
	{
		if ( str[i] >= '0' && str[i] <= '9' )
			break;
		if ( 0 == str[i] || 0x0D == str[i] )
			return;
	}
	dos_sda.DOSVerMajor = str[i] - '0';
	dos_sda.DOSVerMinor = 0;
	if ( '.' == str[i+1] )
	{
		i += 2;
		for ( ; str[i] >= '0' && str[i] <= '9'; i++ )
		{
			dos_sda.DOSVerMinor = dos_sda.DOSVerMinor*10 + (str[i] - '0');
		}
	}
}

char BreakReasonText[100];
extern char BRUnsIntCode[];
extern char *XMS_Start_Aligned;

void SetA20Gate(int enabled)
{
	int i;
	int val = enabled ? ((u32)XMS_Start_Aligned - 0x100000)>>4 : ((u32)INTVectors - 0x100000)>>4;
	for (i=0x100000>>EMSPAGESHIFT; i < (0x110000>>EMSPAGESHIFT); i++)
		EMSPages[i] = val;
}

int QueryA20Gate()
{
	return (((u32)XMS_Start_Aligned - 0x100000)>>4) == EMSPages[0x100000>>EMSPAGESHIFT] ? 1 : 0;
}

int INTHandler(int num)
{
	//int i;
	IntDebug = false;

	// Prepare for unsupported INT error message.
	BRUnsIntCode[0] = (num>>4) > 9 ? (num>>4)-10+'A' : (num>>4) + '0';
	BRUnsIntCode[1] = (num&0x0F) > 9 ? (num&0x0F)-10+'A' : (num&0x0F) + '0';

	if ( 0x03 == num )
		return 0;
	else if ( 0x10 == num )
		return int10h();
	else if ( 0x13 == num )
	{
		// DISK - GET DRIVE PARAMETERS (PC,XT286,CONV,PS,ESDI,SCSI)
		// AH = 08h
		// DL = drive (bit 7 set for hard disk)
		// ES:DI = 0000h:0000h to guard against BIOS bugs
		// Return:CF set on error
		// AH = status (07h) (see #00234)
		// CF clear if successful
		// AH = 00h
		// AL = 00h on at least some BIOSes
		// BL = drive type (AT/PS2 floppies only) (see #00242)
		// CH = low eight bits of maximum cylinder number
		// CL = maximum sector number (bits 5-0)
		// high two bits of maximum cylinder number (bits 7-6)
		// DH = maximum head number
		// DL = number of drives
		// ES:DI -> drive parameter table (floppies only)
		SET_AX(0x0700);
		SET_CF;
		return 0;
	}
	else if ( 0x14 == num )		// COM PORT interrupt, ignore all.
		return 0;
	else if ( 0x15 == num )
	{
		switch ( AH )
		{
			case 0x00:	// Amstrad PC1512 - GET AND RESET MOUSE COUNTS
			case 0x01:	// Amstrad PC1512 - WRITE DATA TO NON-VOLATILE RAM
			case 0x06:	// Amstrad PC1512 - GET ROS VERSION NUMBER
			case 0x10:	// TopView / DESQView
			case 0x11:	// TopView / DESQView
			case 0x12:	// TopView / DESQView
				return 0;
			case 0x24:	// SYSTEM - later PS/2s - A20 Gate handling
				switch(AL)
				{
					case 0:
						// DISABLE A20 GATE
						// Return:
						//	CF clear if successful
						//	AH = 00h
						// First 64KB of the second megabyte = wrap-around segment
						SetA20Gate(0);
						CLR_CF;	// Clear the Carry flag
						SET_AH(0);
						return 0;
					case 1:
						// ENABLE A20 GATE
						// Return:
						//	CF clear if successful
						//	AH = 00h
						SetA20Gate(1);
						CLR_CF;	// Clear the Carry flag
						SET_AH(0);
						return 0;
					case 2:
						// GET A20 GATE STATUS
						// Return:
						//	CF clear if successful
						//	AH = 00h
						//	AL = current state (00h disabled, 01h enabled)
						//	CX = ??? (set to 0000h-000Bh or FFFFh by AMI BIOS v1.00.03.AV0M)
						SET_AX(QueryA20Gate());
						CLR_CF;	// Clear the Carry flag
						return 0;
					case 3:
						// QUERY A20 GATE SUPPORT
						// Return:CF clear if successful
						// AH = 00h
						// BX = status of A20 gate support (see #00462)
						// Bitfields for A20 gate support status:
						// Bit(s)  Description     (Table 00462)
						// 0      supported on keyboard controller
						// 1      supported with bit 1 of I/O port 92h
						// 14-2   reserved
						// 15     additional data is available (location not yet defined)
						CLR_CF;	// Clear the Carry flag
						SET_AH(0);
						SET_BX(3);
						return 0;
				}
				return 0;
			case 0x25:	// ?? (StarControl2, StarControl2 MELEE)
			case 0x2B:	// ??
			case 0x35:	// ?? (START)
			case 0x4C:	// ??
			case 0x50:	// DOS/V - FONT SUBSYSTEM ACCESS
			case 0x64:	// ?? (WordPerfect)
			case 0xBF:	// DESQview/X 1.02+ - DVDOS4GX.DVR 
				return 0;
			case 0xE8:	// Newer BIOSes - GET SYSTEM MEMORY MAP
			case 0x4F:	// KEYBOARD - KEYBOARD INTERCEPT (AT model 3x9,XT2,XT286,CONV,PS)
				// AL = hardware scan code (see #00006)
				// CF set
				// Return:CF set to continue processing scan code
				// AL = possibly-altered hardware scan code (see #00006)
				// CF clear
				// scan code should be ignored
				SET_CF;
				return 0;
			case 0x84:	// BIOS - JOYSTICK SUPPORT (XT after 1982/11/8,AT,XT286,PS)
				// DX = subfunction
				// 	0000h read joystick switches
				// 		Return:AL bits 7-4 = switch settings
				// 	0001h read positions of joysticks
				// 		Return:	AX = X position of joystick A
				// 				BX = Y position of joystick A
				// 				CX = X position of joystick B
				// 				DX = Y position of joystick B
				// 	Return:CF set on error
				// 	AH = status (see #00496)
				// 	CF clear if successful
				//
				// Notes: If no game port is installed, subfunction 0000h returns AL=00h (all switches open) 
				// and subfunction 0001h returns AX=BX=CX=DX=0000h. A 250kOhm joystick typically returns 0000h-01A0h 
				switch ( DX )
				{
					case 0:		// 0000h read joystick switches
						SET_AL(0);
						CLR_CF;	// Clear the Carry flag
						return 0;
					case 1:		// 0001h read positions of joysticks
						SET_AX(0);
						SET_BX(0);
						SET_CX(0);
						SET_DX(0);
						CLR_CF;	// Clear the Carry flag
						return 0;
				}
			
			case 0x86:	// BIOS - WAIT (AT,PS)
				// CX:DX = interval in microseconds (0000:03D0 = 976 in Monkey)
				// Return:
				//	CF clear if successful (wait interval elapsed)
				//	CF set on error or AH=83h wait already in progress
				//	AH = status (see #00496)
				//
				// Note: The resolution of the wait period is 977 microseconds on many systems
				// because many BIOSes use the 1/1024 second fast interrupt from the AT real-time clock chip
				// which is available on INT 70; because newer BIOSes may have much more precise timers available,
				// it is not possible to use this function accurately for very short delays unless the precise
				// behavior of the BIOS is known (or found through testing)
				//
				// 1 microsecond (1 µs) – cycle time for frequency 1 x 10^6 hertz (1 MHz)
				// 
				// swiDelay() Execution Time: NDS9: R0*2 (cache on), or R0*8 (cache off), plus overhead.
				// Note: Both NDS7 and NDS9 timings are counted in 33.51MHz units.
#if !TODO
				swiDelay((REG_CX|((REG_DX>>16)&0xFFFF))<<3);
#endif
				CLR_CF;	// Clear the Carry flag
				SET_AH(0);
				return 0;
			case 0x87:	// SYSTEM - COPY EXTENDED MEMORY
				// CX = number of words to copy (max 8000h)
				// ES:SI -> global descriptor table (see #00499)
				// Return:CF set on error
				// CF clear if successful
				// AH = status (see #00498)
				// 80h invalid command (PC,PCjr)
				// 86h unsupported function (XT,PS30)
				SET_AH(0x86);
				SET_CF;
				return 0;
			case 0x88:	// SYSTEM - GET EXTENDED MEMORY SIZE (286+)
				// Return:CF clear if successful
				// AX = number of contiguous KB starting at absolute address 100000h
				// CF set on error
				// AH = status
				// 80h invalid command (PC,PCjr)
				// 86h unsupported function (XT,PS30)
#if 1
				SET_AX(0);		// Jazz Jackrabbit fails if both HIMEM.SYS exists and this returns > 0!
#else
				SET_AX(((int)RAM_END-((int)INTVectors+0x100000))>>10);
#endif
				CLR_CF;
				return 0;
			case 0xC0:	// SYSTEM - GET CONFIGURATION (XT >1986/1/10,AT mdl 3x9,CONV,XT286,PS)
				// Return:	CF set if BIOS doesn't support call
				// 			CF clear on success
				// 			ES:BX -> ROM table (see #00509)
				// 			AH = status
				// 				00h successful
				// 					The PC XT (since 1986/01/10), PC AT (since 1985/06/10), the
				// 					PC XT Model 286, the PC Convertible and most PS/2 machines
				// 					will clear the CF flag and return the table in ES:BX.
				// 				80h unsupported function
				// 					The PC and PCjr return AH=80h/CF set
				// 				86h unsupported function
				// 					The PC XT (1982/11/08), PC Portable, PC AT (1984/01/10),
				// 					or PS/2 prior to Model 30 return AH=86h/CF set
				//
				// Notes: The 1986/1/10 XT BIOS returns an incorrect value for the feature byte.
				// The configuration table is at F000h:E6F5h in 100% compatible BIOSes.
				// Dell machines contain the signature "DELL" or "Dell" at absolute FE076h and a model byte at absolute address FE845h (see #00516).
				// Hewlett-Packard machines contain the signature "HP" at F000h:00F8h and a product identifier at F000h:00FAh (see #00519).
				// Compaq machines can be identified by the signature string "COMPAQ" at F000h:FFEAh, and is preceded by additional information (see #00517).
				// Tandy 1000 machines contain 21h in the byte at F000h:C000h and FFh in the byte at FFFFh:000Eh;
				// Tandy 1000SL/TL machines only provide the first three data bytes (model/submodel/revision) in the returned table.
				// The ID at F000h:C000h is used by some Microsoft software before trusting the floppy flags bits 1 and 0 at 0040h:00B5h..
				// The Wang PC contains the signature "WANG" at FC00h:0000h. This is used by Peter Reilley's portable binary editor and viewer BEAV to detect a Wang PC..
				// Toshiba laptops contain the signature "TOSHIBA" at FE010h as part of a laptop information record at F000h:E000h (see #00520).
				// Some AST machines contain the string "COPYRIGHT AST RESEARCH" one byte past the end of the configuration table.
				// The Phoenix 386 BIOS contains a second version and date string (presumably the last modification for that OEM version) beginning at F000h:FFD8h,
				// with each byte doubled (so that both ROM chips contain the complete information) 
				//
				// See Also: AH=C7h - AH=C9h - AX=D100h - AX=D103h
				//				
				// F000:E6F5 = 08 00 FC 01 00 70 00 00 00 00
				REG_ES = 0xF000;
				SET_BX(0xE6F5);
				SET_AH(0);			// AH = 00
				CLR_CF;				// Clear the Carry flag
				return 0;
			case 0xC2:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS)
				return MouseInt15h(AL);	// Call the handler in mouse.cpp
			case 0xC3:	// SYSTEM - ENABLE/DISABLE WATCHDOG TIMEOUT (PS50+)
			case 0xC4:	// SYSTEM - PROGRAMMABLE OPTION SELECT (PS50+)
				// AL = subfunction
				//	00h return base POS register address
				//	01h enable selected slot for setup
				// BL = slot number (1 to 8)
				//	02h disable setup for all slots (enable adapter)
				// Return:CF set on error
				// DX = base POS register address (if subfunction 00h)
				SET_CF;
				return 0;
			case 0xC9:	// Newer PS/2; various BIOSes - GET CPU TYPE AND MASK REVISION
				// AL = 10h (may be required on some non-PS BIOSes)
				// Return:CF clear if successful
				// AH = 00h
				// CH = CPU type (see #00528)
				// CL = mask revision (stepping level) (see #00529)
				//
				// (Table 00528)
				// Values for CPU type:
				// 03h    80386DX or clone
				// 04h    80486
				// 05h    Pentium
				// 23h    80386SX or clone
				// 33h    Intel i376
				// 43h    80386SL or clone
				// A3h    IBM 386SLC
				// A4h    IBM 486SLC
				//
				// (Table 00529)
				// Values for stepping level:
				// ---80386/80386DX (type code 03h)---
				// 03h    Intel B1 to B10, Am386DX/DXL step A
				// 05h    Intel D0
				// 08h    Intel D1/D2/E1, Am386DX/DXL step B
				// ---80486DX (type code 04h)---
				// 00h    Intel A0/A1
				// 01h    Intel B2 to B6
				// 02h    Intel C0
				// 03h    Intel C1
				// 04h    Intel D0
				// 10h    Intel cA2/cA3, Cx486SLC step A
				// 11h    Intel cB0/cB1
				SET_CX(0x0303);
				SET_AH(0);			// AH = 00
				CLR_CF;				// Clear the Carry flag
				return 0;
			case 0xD8:	// EISA SYSTEM ROM - READ SLOT CONFIGURATION INFORMATION
				// AX = D800h
				// CL = slot number (including embedded and virtual)
				// Return:CF clear if successful
				// AH = 00h
				// CF set on error
				SET_CF;
				return 0;
		}
	}
	else if ( 0x17 == num )
	{
		switch ( AH )
		{
			case 0:	// PRINTER - WRITE CHARACTER
			case 1:	// PRINTER - INITIALIZE PORT
			case 2:	// PRINTER - GET STATUS
				SET_AH(0x60);	// If both, bit 5 "out of paper" and 4 "selected" are set, no printer is attached.
				return 0;
		}
	}
	else if ( 0x18 == num )	// DISKLESS BOOT HOOK (START CASSETTE BASIC)
		return 0;
	else if ( 0x1A == num )
	{
		switch ( AH )
		{
			case 0x00:	// TIME - GET SYSTEM TIME
				// Return:CX:DX = number of clock ticks since midnight
				// AL = midnight flag, nonzero if midnight passed since time last read
				//
				SET_DX(((u16 *)BIOSData)[0x6C>>1]);
				SET_CX(((u16 *)BIOSData)[0x6E>>1]);
				SET_AX(BIOSData[0x70]);
				return 0;
			case 0x01:	// TIME - SET SYSTEM TIME
				// CX:DX = number of clock ticks since midnight
				// Return:Nothing
				((u16 *)BIOSData)[0x6C>>1] = DX;
				((u16 *)BIOSData)[0x6E>>1] = CX;
				BIOSData[0x70] = 0;
				return 0;
			case 0x02:	// TIME - GET REAL-TIME CLOCK TIME (AT,XT286,PS)
				// Return:CF clear if successful
				// CH = hour (BCD)
				// CL = minutes (BCD)
				// DH = seconds (BCD)
				// DL = daylight savings flag (00h standard time, 01h daylight time)
				// CF set on error (i.e. clock not running or in middle of update)
				{
					time_t unixTime = time(NULL);
					struct tm* timeStruct = gmtime((const time_t *)&unixTime);
					SET_CX((((timeStruct->tm_hour/10)%10)<<(8+4))|((timeStruct->tm_hour%10)<<8)|
							 (((timeStruct->tm_min/10)%10)<<(4))|((timeStruct->tm_min%10)));
					SET_DX((((timeStruct->tm_sec/10)%10)<<(8+4))|((timeStruct->tm_sec%10)<<8));
				}
				CLR_CF;
				return 0;
			case 0x04:	// TIME - GET REAL-TIME CLOCK DATE (AT,XT286,PS)
				// Return:CF clear if successful
				// CH = century (BCD)
				// CL = year (BCD)		CX=0x2010
				// DH = month (BCD)
				// DL = day (BCD)		DX=0x0306 (== March 6th)
				// CF set on error
				{
					time_t unixTime = time(NULL);
					struct tm* timeStruct = gmtime((const time_t *)&unixTime);
					SET_CX(0x2000 | (((timeStruct->tm_year/10)%10)<<(4))|((timeStruct->tm_year%10)));
					SET_DX(((((1+timeStruct->tm_mon)/10)%10)<<(12))|(((1+timeStruct->tm_mon)%10)<<(8))|
							 (((timeStruct->tm_mday/10)%10)<<(4))|((timeStruct->tm_mday%10)));
				}
				CLR_CF;
				return 0;
			case 0x35:	// WordPerfect?
			case 0x36:	// WORD PERFECT v5.0 Third Party Interface - INSTALLATION CHECK
			case 0x80:	// PCjr, Tandy 2500???, Tandy 1000SL/TL - SET UP SOUND MULTIPLEXOR
			case 0x81:	// Tandy 2500, Tandy 1000L series - DIGITAL SOUND - INSTALLATION CHECK
				return 0;
		}
	}
	else if ( 0x20 == num )		// DOS 1+ - TERMINATE PROGRAM
	{
		SET_AX(0);
		return int21h();
	}
	else if ( 0x21 == num )
	{
		return int21h();
	}
	else if ( 0x25 == num || 0x26 == num )		// DOS 1+ - ABSOLUTE DISK READ/WRITE (except partitions > 32M)
	{
		// AL = drive number (00h = A:, 01h = B:, etc)
		// CX = number of sectors to read (not FFFFh)
		// DX = starting logical sector number (0000h - highest sector on drive)
		// DS:BX -> buffer for data
		// Return:CF clear if successful
		// CF set on error
		// AH = status (see #02547)
		// AL = error code (same as passed to INT 24 in DI)
		// AX = 0207h if more than 64K sectors on drive -- use new-style call
		SET_AX(0x0207);
		SET_CF;
		SET_SP(SP-2);		// These calls leave flags to stack!
		return 0;
	}
	else if ( 0x27 == num )		// DOS 1+ - TERMINATE AND STAY RESIDENT
	{
		// DX = number of bytes to keep resident (max FFF0h)
		// CS = segment of PSP
		// Return: Never
		// First convert DX to paragraphs
		SET_DX(((DX+15)>>4)&0x0FFF);
		// Then call the DOS interrupt
		SET_AX(0x3100);
		return int21h();
	}
	else if ( 0x28 == num )		// DOS 2+ - DOS IDLE INTERRUPT
	{
		// This interrupt is invoked each time one of the DOS character input functions loops while waiting for input.
		wait_vsync();
		return 0;
	}
	else if ( 0x29 == num )		// DOS 2+ - FAST CONSOLE OUTPUT
	{
		// AL = character to display
		// Return:Nothing
		// Notes: Automatically called when writing to a device with bit 4 of its device driver header set (see also INT 21/AH=52h).
		// COMMAND.COM v3.2 and v3.3 compare the INT 29 vector against the INT 20 vector and assume that ANSI.SYS is installed if the segment is larger.
		// The default handler under DOS 2.x and 3.x simply calls INT 10/AH=0Eh. 
		int ax = REG_AX;
		int bx = REG_BX;
		REG_AX -= (0x2900-0x0E00); 				// AH = 0Eh, AL = character to write
		REG_BX = (BIOSData[0x62]<<8);			// BH = page number = currently active page
		int10h();								// VIDEO - TELETYPE OUTPUT
		REG_AX = ax;
		REG_BX = bx;
		return 0;
	}
	else if ( 0x2A == num )
	{
		switch ( AH )
		{
			case 0x00:	// NETWORK - INSTALLATION CHECK
				// Return:AH <> 00h if installed
				// CF set if NetWare v2.15 NetBIOS emulator installed
				return 0;
		}
	}
	else if ( 0x2D == num )	// Screen Thief multiplex interrupt
		return 0;
	else if ( 0x2F == num )
	{
		if (0x15 == AH)
			return cdrom_2F();	// Call the CD-ROM MSCDEX 2F dispatcher handler
		switch ( AX )
		{
			case 0x0600:	// DOS 3.0+ ASSIGN - INSTALLATION CHECK
				return 0;
			case 0x1203:	// DOS 3.0+ internal - GET DOS DATA SEGMENT
				// Return:DS = data segment of IBMDOS.COM/MSDOS.SYS (segment of NUL device)
				REG_DS = 0x0080;
				return 0;
			case 0x1607:	// MS Windows - VIRTUAL DEVICE CALL OUT API (Win386)
				if ( BX == 0x0015)
				{
					switch ( CX )
					{
						case 0:					// Query instace
							SET_CX(0x0001);		// state = instanced
							SET_DX(0x0050);		// DOS driver segment
							REG_ES = 0x0050;
							SET_BX(0x0060);
							break;
						case 1:					// Set patches
							SET_AX(0xb97c);		// Supported
							SET_BX(DX & 0x16);	// Bit mask of patches applied
							SET_DX(0xa2ab);		// Supported
							break;
						case 3:					// Get size of data structures
							if (DX == 1)		// CDS size
							{
								SET_AX(0xb97c);	// Supported
								SET_DX(0xa2ab);	// Supported
								SET_CX(0x000e);	// size
							}
							break;
						case 4:					// Instanced data
							SET_DX(0);			// none
							break;
						case 5:					// Get device driver size
							SET_AX(0);
							SET_DX(0);
							break;
					}
				}
				return 0;
			case 0x1680:	// MS Windows, DPMI, various - RELEASE CURRENT VIRTUAL MACHINE TIME-SLICE
			case 0x1689:	// MS Windows 3.0+ - KERNEL IDLE CALL
				SET_AX(0x1600);
				wait_vsync();
				return 0;
			case 0x1684:	// MS Windows - GET DEVICE API ENTRY POINT
				if ( 0x0027 == BX )	// MS Windows95 - VXDLDR - GET API ENTRY POINT (Ultima 7)
				{
					// Return:ES:DI -> VxD API entry point (see #02666)
					// 0000h:0000h if the VxD does not support API in current mode
					REG_ES = 0;
					SET_DI(0);
				}
				return 0;
			case 0x4300:	// EXTENDED MEMORY SPECIFICATION (XMS) v2+ - INSTALLATION CHECK
				SET_AX(0x4380);
				return 0;
			case 0x4310:	// EXTENDED MEMORY SPECIFICATION (XMS) v2+ - GET DRIVER ADDRESS
				// Return:
				//	ES:BX -> driver entry point (see #02749,#02750,#02753,#02760,#02769,#02774)
				REG_ES = 0xF000;
				SET_BX(0x09F0);
				return 0;
			case 0x4A01:	// DOS 5+ - QUERY FREE HMA SPACE
				// Return:BX = number of bytes available in HMA (0000h if DOS not using HMA)
				// ES:DI -> start of available HMA area (FFFFh:FFFFh if not using HMA)
				// Notes: Called by Windows 3.1 DOSX.EXE.
				SET_BX(0);
				REG_ES = 0xFFFF;
				SET_DI(0xFFFF);
				return 0;
			case 0xD44E:	// 4DOS v3.0+ - AWAITING USER INPUT
				wait_vsync();
				return 0;
			case 0xB706:	// DOS 4.0+ APPEND - GET APPEND FUNCTION STATE
				SET_BX(0);	// Return:BX = APPEND state (see #02980)
				return 0;
			case 0xAE00:	// DOS 3.3+ internal - INSTALLABLE COMMAND - INSTALLATION CHECK
				// DX = magic value FFFFh
				// CH = FFh
				// CL = length of command line tail (4DOS v4.0)
				// DS:BX -> command line buffer (see #02977)
				// DS:SI -> command name buffer (see #02978)
				// DI = 0000h (4DOS v4.0)
				// Return:	AL = FFh if this command is a TSR extension to COMMAND.COM
				// 			AL = 00h if the command should be executed as usual
				// "debug test.com" =>
				//		CL = 09
				//		DS:BX = 7C, 0E, "debug test.com", 0D, 00
				//		DS:SI = 05, "DEBUG      ", 0
				// "test.com" =>
				//		CL = 04
				//		DS:BX = 7C, 08, "test.com", 0D, 00
				//		DS:SI = 04, "TEST       ", 0
				if ( (int)0xFFFF == DX )
				{
					char *cmd = DS_SI_PTR;
					char *line = DS_BX_PTR;
					int	len = cmd[0];
					// Always perform the "VER" command ourselves
					if ( 3 == len && 0 == memcmp( cmd+1, "VER", 3 ) )
						SET_AL(0xFF);
					if ( 4 == len && 0 == memcmp( cmd+1, "EXIT", 4 ) )
					{
						// Determine if we are in the primary shell,
						// and exit the program if we are.
						PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
						if ((p->ps_prevpsp>>16) == DOS_PSP)
						{
							// Exit from the primary DOS shell!
#ifdef RPi
							exit_program();
#else
							IRQRequest(IRQ_EXIT);
#endif
						}
					}
					if ( line[1] > len+1 )		// Command has parameters?
					{
						switch ( len )			// Length of the command
						{
							case 5:	// "DEBUG"?
								if ( 0 == memcmp( cmd+1, "DEBUG", len ) )
									SET_AL(0xFF);
								break;
							case 6:	// "SETVER"?
								if ( 0 == memcmp( cmd+1, "SETVER", len ) )
									SET_AL(0xFF);
								break;
							case 7:	// "LOADFIX"?
								if ( 0 == memcmp( cmd+1, "LOADFIX", len ) )
									SET_AL(0xFF);
								break;
						}
					}
				}
				return 0;
			case 0xAE01:	// DOS 3.3+ internal - INSTALLABLE COMMAND - EXECUTE
				// DX = magic value FFFFh
				// CH = 00h
				// CL = length of command name (4DOS v4.0)
				// DS:BX -> command line buffer (see #02977)
				// DS:SI -> command name buffer (see #02978)
				// Return:DS:SI buffer updated
				// if length byte is nonzero, the following bytes contain the uppercase
				// internal command to execute and the command line buffer contains the
				// command's parameters (the first DS:[SI] bytes are ignored)
				if ( (int)0xFFFF == DX )
				{
					char *cmd = DS_SI_PTR;
					char *line = DS_BX_PTR;
					int	len = cmd[0];
					switch ( len )			// Length of the command
					{
						case 3:	// "VER"?
							if ( 0 == memcmp( cmd+1, "VER", len ) )
							{
								if ( line[1] > len+1 )
									ParseDOSVer(line+6);
								ShowDOSVer(line);
							}
							break;
						case 5:	// "DEBUG"
							if ( 0 == memcmp( cmd+1, "DEBUG", len ) )
							{
								memmove( line+2, line+8, line[1]-4 );
								line[1] -= (len+1);
								dos_sda.ExecFlags |= EXEC_DEBUG;
							}
							break;
						case 6:	// "SETVER"?
							if ( 0 == memcmp( cmd+1, "SETVER", len ) )
							{
								ParseDOSVer(line+9);
								ShowDOSVer(line);
								//cmd[0] = 3;
								//memcpy( cmd+1, "VER   ", 6);
							}
							break;
						case 7:	// "LOADFIX"
							if ( 0 == memcmp( cmd+1, "LOADFIX", len ) )
							{
								memmove( line+2, line+9, line[1]-5 );
								line[1] -= (len+1);
								dos_sda.ExecFlags |= EXEC_LOADFIX;
							}
							break;
					}
				}
				return 0;
			case 0x1100:	// NETWORK REDIRECTOR - INSTALLATION CHECK
			case 0x1130:	// IFSFUNC.EXE (DOS 4.x only) - GET IFSFUNC SEGMENT
			case 0x1180:	// LAN Manager Enhanced DOS Services
			case 0x122E:	// DOS 4.0+ internal - GET OR SET ERROR TABLE ADDRESSES (Win 95)
			case 0x1231:	// Windows95 - SET/CLEAR REPORT WINDOWS TO DOS PROGRAMS FLAG
			case 0x1302:	// ?? (Win386)
			case 0x1600:	// MS Windows - WINDOWS ENHANCED MODE INSTALLATION CHECK
			case 0x1605:	// MS Windows - WINDOWS ENHANCED MODE & 286 DOSX INIT BROADCAST
			case 0x1606:	// MS Windows - WINDOWS ENHANCED MODE & 286 DOSX EXIT BROADCAST
			case 0x1608:	// MS Windows - WINDOWS ENHANCED MODE INIT COMPLETE BROADCAST
			case 0x160A:	// MS Windows 3.1 - IDENTIFY WINDOWS VERSION AND TYPE, Return:AX = 0000h if call supported
			case 0x160B:	// MS Windows 3.1 - IDENTIFY TSRs (Win386)
			case 0x160E:	// MS-DOS 7 kernel - BOOT LOGO SUPPORT???
			case 0x1611:	// MS-DOS 7 kernel - GET SHELL PARAMETERS
			case 0x1613:	// MS-DOS 7 kernel - GET SYSTEM.DAT (REGISTRY FILE) PATHNAME
			case 0x1686:	// DOS Protected-Mode Interface - DETECT MODE
			case 0x1687:	// DOS Protected-Mode Interface - INSTALLATION CHECK
			case 0x168F:	// Windows95 - CLOSE-AWARENESS - ENABLE/DISABLE CLOSE COMMAND
			case 0x1700:	// MS Windows WINOLDAP - IDENTIFY WinOldAp VERSION
			case 0x1800:	// MS-Manager
			case 0x1A00:	// DOS 4.0+ ANSI.SYS - INSTALLATION CHECK
			case 0x4000:	// Windows 3+ (OS/2 2.x???) - GET VIRTUAL DEVICE DRIVER (VDD) CAPABILTIES
			case 0x4007:	// Windows 3.x - ENABLE VDD TRAPPING OF VIDEO REGISTERS
			case 0x4309:	// HIMEM.SYS v3.09+ - GET XMS HANDLE TABLE
			case 0x4601:	// MS Windows WINOLDAP - SWITCHING ???
			case 0x4602:	// MS Windows WINOLDAP - SWITCHING ???
			case 0x4680:	// MS Windows v3.0 - INSTALLATION CHECK
			case 0x4A10:	// SMARTDRV v4.00+ - INSTALLATION CHECK AND HIT RATIOS
			case 0x4A11:	// DBLSPACE.BIN - GetVersion - INSTALLATION CHECK
			case 0x4A16:	// Windows95 - OPEN BOOT LOG
			case 0x4A17:	// Windows95 - WRITE TO BOOT LOG
			case 0x4A18:	// Windows95 - CLOSE BOOT LOG
			case 0x4A33:	// Windows95 - CHECK MS-DOS VERSION 7
			case 0x4B01:	// DOS 5+ TASK SWITCHER - BUILD CALLOUT CHAIN
			case 0x4B02:	// DOS 5+ TASK SWITCHER - INSTALLATION CHECK
			case 0x4B03:	// DOS 5+ TASK SWITCHER - ALLOCATE SWITCHER ID
			case 0x4B05:	// DOS 5+ TASK SWITCHER - IDENTIFY INSTANCE DATA
			case 0x4B21:	// Windows95 - WIN.COM - GET NESTING LEVEL
			case 0x5500:	// DOS 5+ - COMMAND.COM INTERFACE
			case 0x7A00:	// Novell NetWare - LOW-LEVEL API (IPX) INSTALLATION CHECK
			case 0x9900:	// DOS Navigator II - INSTALLATION CHECK			
			case 0xA1FE:	// Ergo DOS extenders - INSTALLATION CHECK
			case 0xA1FF:	// Ergo DOS extenders - INSTALLATION CHECK
			case 0xAD00:	// DR DOS 3.41-5.0, Novell DOS 7 KEYB - INSTALLATION CHECK
			case 0xAD80:	// KEYB.COM internal - INSTALLATION CHECK
			case 0xB700:	// APPEND - INSTALLATION CHECK
			case 0xB800:	// NETWORK - INSTALLATION CHECK
			case 0xBC00:	// MediaVision MVSOUND.SYS - INSTALLATION CHECK
			case 0xBC01:	// MediaVision MVSOUND.SYS - GET VERSION
			case 0xD200:	// Quarterdeck RPCI - INSTALLATION CHECK
			case 0xD201:	// Quarterdeck RPCI - QEMM v5.0+ - INSTALLATION CHECK
			case 0xD44D:	// 4DOS.COM v2.1+ - API (installation check)
			case 0xD701:	// Banyan VINES v4+ - GET BANV INTERRUPT NUMBER
			case 0xD800:	// Novell NetWare Lite - CLIENT.EXE - INSTALLATION CHECK
			case 0xD880:	// Novell NetWare Lite v1.0+ - SERVER - INSTALLATION CHECK
			case 0xD8C0:	// Novell NLCACHE,NWCACHE - INSTALLATION CHECK
			case 0xDE00:	// DESQview v2.26+ External Device Interface - INSTALLATION CHECK
			case 0xDE01:	// Quarterdeck QDPMI.SYS v1.0 - INSTALLATION CHECK
			case 0xE000:	// Various - INSTALLATION CHECK
			case 0xED00:	// Phar Lap DOS EXTENDERS - INSTALLATION CHECK
			case 0xF100:	// DOS EXTENDER INSTALLATION CHECK
			case 0xF400:	// FINDIRQ.COM - INSTALLATION CHECK
			case 0xFB42:	// Borland RTM.EXE 1.0 - INSTALLATION CHECK???
			case 0xFBA1:	// TKERNEL (Borland DOS extender) - INSTALLATION CHECK
			case 0xFE00:	// NORTON UTILITIES 5.0+ TSRs - INSTALLATION CHECK/STATUS REPORT
			case 0xFE01:	// NORTON UTILITIES 5.0+ TSRs - ENABLE
				return 0;	// All the above are unsupported, so change no registers.
		}
	}
	else if ( 0x33 == num )
		return int33h( AX );
	else if ( 0x4B == num )
		return 0;			// Virtual DMA Specification (VDS) - GET VERSION
	else if ( 0x5C == num )
		return 0;			// TI Professional PC - KEYBOARD PAUSE KEY VECTOR
	else if ( 0x61 == num )
		return 0;			// Banyan Vines functions (used by Mahjong Fantasy)
	else if ( 0x66 == num )
		return 0;			// DIGPAK functions
	else if ( 0x67 == num )
		return int67h( AH );
	else if ( 0x68 == num )
	{
		switch (AH)
		{
			case 0x43:		// Int 68/AX=4300h - ??? - INSTALLATION CHECK??? (Windows 3.00 DOSX.EXE)
				return 0;
		}
		return -1;
	}
	else if ( 0x7F == num )
		return 0;
	else if ( 0x80 == num )
		return 0;
	else if ( 0xFC == num )	// Unclassified, used in SKULL
		return 0;
	sprintf( BreakReasonText, "int %02X AH=%02X\n", num, AH );
	BreakReason = BreakReasonText;
	return -1;
}
