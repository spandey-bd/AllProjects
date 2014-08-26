//=============================================================================
// int21h.c
//
// This file contains the DOS interrupt handler.
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
#ifdef IOS
#include <stdlib.h>
#else
#include <malloc.h> 
#endif

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

extern int int10h();
extern int EGA_write_byte_from_C(u8 value, int addr);

int FreeMemSegment = 0xA000;					// Lowest free memory segment
int ParentRegSave[18];
int ParentMemSegment = 0;
extern int VSyncCounter;
extern char IntDebug;
extern char	ShowHDDLed;
int OrigKeyboardRefresh = 0;
extern char BreakReasonText[];
int	dbghandle = -1;

#define LOG_UNTIL_EXIT	0

#define	ROWLEN	42

#define HDDLedOn() { ShowHDDLed = 1; }

#define	HDDLedOff() { ShowHDDLed = 0; }

const char* dbgformat = "%04X:%04X %02X %s\n";

const char CountryInfo[] = {0,0,'$',0,0,0,0,' ',0,'.',0,'/',0,':',0,0,2,1,0x08,0x10,0x00,0xF0,';',0 };

//-----------------------
// DOS INT 21h interrupt.
//-----------------------
int int21h() {
	int	rc;
	u16 ax, bx;

#if LOG_UNTIL_EXIT
	IntDebug = true;
#endif

	if ((AH >= 0x38 && AH <= 0x4F) || (AH >= 0x56 && AH <= 0x5c) ||
		(AH >= 0x5e && AH <= 0x60) || (AH >= 0x65 && AH <= 0x6a) ||
		0x6C == AH)
	{
		CLR_CF;		// Clear the Carry flag
		if (AH != 0x59)
			dos_sda.CritErrCode = NO_ERROR;
	}

	switch ( AH )
	{
		case 0x00:	// DOS 1+ - TERMINATE PROGRAM
			// CS = PSP segment
			return_user(false);
			SET_AL(0);
			CLR_CF;			// Clear the Carry flag
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0, "Terminate" );
			return 0;
			
		//cases 0x01, 0x06, 0x07, 0x08, 0x0A, 0x0C handled in "int21.asm"
		
		case 0x09:	// DOS 1+ - WRITE STRING TO STANDARD OUTPUT
			// DS:DX -> '$'-terminated string
			// Return:AL = 24h (the '$' terminating the string, despite official docs which
			// state that nothing is returned) (at least DOS 2.1-7.0 and NWDOS)
			//
			// Notes: ^C/^Break are checked, and INT 23 is called if either pressed.
			// Standard output is always the screen under DOS 1.x, but may be redirected under DOS 2+.
			// Under the FlashTek X-32 DOS extender, the pointer is in DS:EDX 
			//
			// See Also: AH=02h - AH=06h"OUTPUT" 
			//
			// Category: DOS Kernel - Int 21h - D
			{
				unsigned char *str = (unsigned char *)DS_DX_PTR;
				int ax = REG_AX;
				int bx = REG_BX;
				SET_BH(BIOSData[0x62]);					// BH = page number = currently active page
				switch ( BIOSData[0x49] )
				{
					case 4:
					case 5:
						SET_BL(3);			// Use attribute 3 for text
						break;
					case 6:
						SET_BL(1);			// Use attribute 1 for text
						break;
					case 0x0D:
					case 0x0E:
					case 0x10:
					case 0x12:
					case 0x13:
						SET_BL(0x0F);			// Use attribute F for text
						break;
				}
				if (IntDebug)
					LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 9, "Write str$" );
				rc = 0;
				while ( *str != '$' && 0 == rc)
				{
					SET_AX(0x0E00 + (*str)); 				// AH = 0Eh, AL = character to write
					rc = int10h();							// VIDEO - TELETYPE OUTPUT
					str++;
				}
				REG_AX = ax;
				REG_BX = bx;
			}
			if ( rc < 0 )
				break;
			else
				return 0;
		case 0x0B:	// DOS 1+ - GET STDIN STATUS
			// Return: AL = status
			// 			00h if no character available
			//			FFh if character is available
			if ( BIOSData[0x1A] == BIOSData[0x1C] && 0 == BIOSData[0x19])	// No keys in keyboard buffer, no previous scancode waiting to be read
				SET_AL(0);		// AL = 0
			else
				SET_AL(0xFF);	// AL = 0xFF
			//if (IntDebug)
			//	LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x0B, "STDIN status" );
			return 0;
		case 0x0D:	// DOS 1+ - DISK RESET
			// Notes: This function writes all modified disk buffers to disk,
			// but does not update the directory information (that is only done when files are closed or a SYNC call is issued) 
			// TODO! We should flush all open files here.
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x0D, "DISK reset" );
			return 0;
		case 0x0E:	// DOS 1+ - SELECT DEFAULT DRIVE
			// DL = new default drive (00h = A:, 01h = B:, etc)
			// Return:AL = number of potentially valid drive letters
			// Notes: Under DOS 3.0+, the return value is the greatest of 5, the value of LASTDRIVE= in CONFIG.SYS, and the number of drives actually present. 
			DosSetCuDrive(DL);
			SET_AX(0x0E05);
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x0E, "Set def drv" );
			return 0;
		case 0x0F:	// DOS 1+ - OPEN FILE USING FCB
			// DS:DX -> unopened File Control Block (see #01345,#01346)
			// Return:AL = status
			// 00h successful
			// FFh file not found or access denied
			//
			// Notes: FAT32 does not support FCBs for file I/O 
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbOpen((XFCB *)DS_DX_PTR, 2));		// O_FCB | O_LEGACY | O_OPEN | O_RDWR
			HDDLedOff();
			return 0;
		case 0x10:	// DOS 1+ - CLOSE FILE USING FCB
			// DS:DX -> File Control Block (see #01345)
			// Return:AL = status
			// 	00h successful
			//	FFh failed
			//
			// Notes: A successful close forces all disk buffers used by the file to be written and the directory entry to be updated.
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbClose((XFCB *)DS_DX_PTR));
			HDDLedOff();
			return 0;
		case 0x11:	// DOS 1+ - FIND FIRST MATCHING FILE USING FCB
			// DS:DX -> unopened FCB (see #01345), may contain '?' wildcards
			// Return:
			//	AL = status
			//		00h successful
			//			[DTA] unopened FCB for first matching file
			//		FFh no matching filename, or bad FCB
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbFind((XFCB *)DS_DX_PTR, true));
			HDDLedOff();
			return 0;
		case 0x12:	// DOS 1+ - FIND NEXT MATCHING FILE USING FCB
			// DS:DX -> unopened FCB (see #01345), may contain '?' wildcards
			// Return:
			//	AL = status
			//		00h successful
			//			[DTA] unopened FCB for first matching file
			//		FFh no matching filename, or bad FCB
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbFind((XFCB *)DS_DX_PTR, false));
			HDDLedOff();
			return 0;
		case 0x14:	// DOS 1+ - SEQUENTIAL READ FROM FCB FILE
			// DS:DX -> opened FCB (see #01345)
			// Return:
			//	AL = status
			// 		00h successful
			// 		01h end of file (no data)
			// 		02h segment wrap in DTA
			// 		03h end of file, partial record read
			// 	Disk Tranfer Area filled with record read from file
			//
			// Notes: Reads a record of the size specified in the FCB beginning at the current file position,
			//	then updates the current block and current record fields in the FCB. If a partial record was read,
			//	it is zero-padded to the full size. 
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbReadWrite((XFCB *)DS_DX_PTR, 1, FCB_READ));
			HDDLedOff();
			return 0;
		case 0x15:	// DOS 1+ - SEQUENTIAL WRITE TO FCB FILE
			// DS:DX -> opened FCB (see #01345)
			// Disk Tranfer Area contains record to be written
			// Return:
			//	AL = status
			// 		00h successful
			// 		01h disk full
			// 		02h segment wrap in DTA
			// 
			// Notes: Writes a record of the size specified in the FCB beginning at the current file position,
			// then updates the current block and current record fields in the FCB. If less than a full sector is written,
			// the data is placed in a DOS buffer to be written out at a later time. 
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbReadWrite((XFCB *)DS_DX_PTR, 1, FCB_WRITE));
			HDDLedOff();
			return 0;
		case 0x19:	// DOS 1+ - GET CURRENT DEFAULT DRIVE
			// Return:AL = drive (00h = A:, 01h = B:, etc)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get def drv" );
			SET_AX(0x1900 + DosGetCuDrive());
			return 0;
		case 0x1A:	// DOS 1+ - SET DISK TRANSFER AREA ADDRESS
			// DS:DX -> Disk Transfer Area (DTA)
			dos_sda.dta = (REG_DS<<16)|(DX&0xFFFF);
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Set DTA" );
			return 0;
		case 0x1B:	// DOS 1+ - GET ALLOCATION INFORMATION FOR DEFAULT DRIVE
			SET_DL(0);
			// fall-thru
		case 0x1C:	// DOS 1+ - GET ALLOCATION INFORMATION FOR SPECIFIC DRIVE
			// DL = drive (00h = default, 01h = A:, etc)
			// Return:
			// AL = sectors per cluster (allocation unit), or FFh if invalid drive
			// CX = bytes per sector
			// DX = total number of clusters
			// DS:BX -> media ID byte (see #01356)			
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get drv info" );
			if ( 0 == DL || 3 == DL )
			{
				int al, cx, dx;
				// First get the disk geometry
				HDDLedOn();
				DosGetFreeDisk(&al, NULL, &cx, &dx);
				SET_AL(al);
				SET_CX(cx);
				SET_DX(dx);
				// Then set DS:BX to point to a 0xF8 at F000:02DC
				REG_DS = 0xF000;
				SET_BX(0x02DC);
				HDDLedOff();
			}
			else
				SET_AL(0xFF);
			return 0;
		case 0x21:	// DOS 1+ - READ RANDOM RECORD FROM FCB FILE
			// DS:DX -> opened FCB (see #01345)
			// Return:AL = status
			//	00h successful
			//	01h end of file, no data read
			//	02h segment wrap in DTA, no data read
			//	03h end of file, partial record read
			// Disk Tranfer Area filled with record read from file
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbRandomIO((XFCB *)DS_DX_PTR, FCB_READ));
			HDDLedOff();
			return 0;
		case 0x22:	// DOS 1+ - WRITE RANDOM RECORD TO FCB FILE
			// DS:DX -> opened FCB (see #01345)
			// Disk Transfer Area contains data to be written
			// Return:AL = status
			//	00h successful
			//	01h end of file, no data read
			//	02h segment wrap in DTA, no data read
			//	03h end of file, partial record read
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			SET_AL(FcbRandomIO((XFCB *)DS_DX_PTR, FCB_WRITE));
			HDDLedOff();
			return 0;
		case 0x25:	// DOS 1+ - SET INTERRUPT VECTOR
			// AL = interrupt number
			// DS:DX -> new interrupt handler
			INTVectors[AL] = (REG_DS<<16) | DX;
			//if (IntDebug)
			//	LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Set int vect" );
			return 0;
		case 0x26:	// DOS 1+ - CREATE NEW PROGRAM SEGMENT PREFIX
			// DX = segment at which to create PSP (see #01378)
			// Return:AL destroyed
			//
			// Notes: New PSP is updated with memory size information;
			//	INTs 22h, 23h, 24h taken from interrupt vector table;
			// 	the parent PSP field is set to 0.
			//	(DOS 2+) DOS assumes that the caller's CS is the segment of the PSP to copy 
			new_psp(DX, REG_CS);
			return 0;
		case 0x27:	// DOS 1+ - RANDOM BLOCK READ FROM FCB FILE
			// CX = number of records to read
			// DS:DX -> opened FCB (see #01345)
			// Return:AL = status
			// 00h successful, all records read
			// 01h end of file, no data read
			// 02h segment wrap in DTA, no data read
			// 03h end of file, partial read
			// Disk Transfer Area filled with records read from file
			// CX = number of records read (return AL = 00h or 03h)
			//
			// Notes: Read begins at current file position as specified in FCB;
			// the file position is updated after reading.
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			rc = CX;
			SET_AL(FcbRandomBlockIO((XFCB *)DS_DX_PTR, &rc, FCB_READ|FCB_RANDOM));
			SET_CX(rc);
			HDDLedOff();
			return 0;
		case 0x28:	// DOS 1+ - RANDOM BLOCK WRITE TO FCB FILE
			// CX = number of records to write
			// DS:DX -> opened FCB (see #01345)
			// Disk Transfer Area contains records to be written
			// Return:AL = status
			// 00h successful
			// 01h disk full or file read-only
			// 02h segment wrap in DTA
			// CX = number of records written
			//
			// Notes: Write begins at current file position as specified in FCB;
			// the file position is updated after writing.
			// If CX = 0000h on entry, no data is written;
			// instead the file size is adjusted to be the same as the file position specified by
			// the random record and record size fields of the FCB.
			// If the data to be written is less than a disk sector, it is copied into a DOS disk buffer,
			// to be written out to disk at a later time.
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR+1 );
			rc = CX;
			SET_AL(FcbRandomBlockIO((XFCB *)DS_DX_PTR, &rc, FCB_WRITE|FCB_RANDOM));
			SET_CX(rc);
			HDDLedOff();
			return 0;
		case 0x29:	// DOS 1+ - PARSE FILENAME INTO FCB
			// AL = parsing options (see #01380)
			// DS:SI -> filename string (both '*' and '?' wildcards OK)
			// ES:DI -> buffer for unopened FCB
			// Return:	AL = result code
			// 				00h successful parse, no wildcards encountered
			// 				01h successful parse, wildcards present
			// 				FFh failed (invalid drive specifier)
			// 			DS:SI -> first unparsed character
			// 			ES:DI buffer filled with unopened FCB
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_SI_PTR );
			FcbParseFname(AL, DS_SI_PTR, (FCB *)ES_DI_PTR);
			return 0;
		case 0x2A:
			// DOS 1+ - GET SYSTEM DATE
			// AH = 2Ah
			// Return:	CX = year (1980-2099)
			// 			DH = month
			// 			DL = day
			// ---DOS 1.10+---
			// 			AL = day of week (00h=Sunday)
			{
				time_t unixTime = time(NULL);
				struct tm* timeStruct = gmtime((const time_t *)&unixTime);
				SET_CX(1900+timeStruct->tm_year);
				SET_DX(((1+timeStruct->tm_mon)<<8)|(timeStruct->tm_mday));
				SET_AL(timeStruct->tm_wday);
				if (IntDebug)
					LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get date" );
			}
			return 0;
		case 0x2B:
			// DOS 1+ - SET SYSTEM DATE
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Set date" );
			if ( CX == 0x4445 && DX == 0x5351 )
			{
				// DESQview - INSTALLATION CHECK
				// AH = 2Bh
				// CX = 4445h ('DE')
				// DX = 5351h ('SQ')
				// AL = subfunction (DV v2.00+)
				// Return:AL = FFh if DESQview not installed
				SET_AL(0xFF);
				return 0;
			}
			SET_AL(0);		// Return Succesfull, though we don't actually change the date. TODO!
			return 0;
		case 0x2C:	// DOS 1+ - GET SYSTEM TIME
			// Return:	CH = hour
			// 			CL = minute
			// 			DH = second
			// 			DL = 1/100 seconds
			//
			// Note: On most systems, the resolution of the system clock is about 5/100sec,
			// so returned times generally do not increment by 1. On some systems, DL may always return 00h
			//
			// See Also: AH=2Ah - AH=2Dh - AH=E7h"Novell" - INT 1A/AH=00 
			//
			// See Also: INT 2F/AX=120Dh 
			//
			// Category: DOS Kernel - Int 21h - D 
			{
				time_t unixTime = time(NULL);
				struct tm* timeStruct = gmtime((const time_t *)&unixTime);
				SET_CX((timeStruct->tm_hour<<8)|timeStruct->tm_min);
				SET_DX((timeStruct->tm_sec<<8)|((((VSyncCounter%60)*109226)>>16)&0xFF));	// Solar Winds needs DL values to change
			}
			return 0;
		case 0x2D:	// DOS 1+ - SET SYSTEM TIME
			// CH = hour
			// CL = minute
			// DH = second
			// DL = 1/100 seconds
			// Return:AL = result
			// 00h successful
			// FFh invalid time, system time unchanged
			//
			// Note: DOS 3.3+ also sets CMOS clock; due to the limitations of the CLOCK$ driver interface, the CMOS date is also updated to the current DOS date 
			SET_AL(0);		// Return Succesfull, though we don't actually change the time. TODO!
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x2D, "Set time" );
			return 0;
		case 0x2E:	// DOS 1+ - SET VERIFY FLAG
			// AL = new state of verify flag
			// 00h off
			// 01h on
			// 
			// Notes: Default state at system boot is OFF.
			// When ON, all disk writes are verified provided the device driver supports read-after-write verification 
			dos_sda.verify_ena = AL & 1;
			return 0;
		case 0x2F:
			// DOS 2+ - GET DISK TRANSFER AREA ADDRESS
			// AH = 2Fh
			// Return:ES:BX -> current DTA
			//
			// Note: Under the FlashTek X-32 DOS extender, the pointer is in ES:EBX 
			//
			// See Also: AH=1Ah 
			//
			// Category: DOS Kernel - Int 21h - D 
			SET_BX(dos_sda.dta&0xFFFF);
			REG_ES = (dos_sda.dta>>16)&0xFFFF;
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x2F, "Get DTA" );
			return 0;
		case 0x30:
			// INT 21 - DOS 2+ - GET DOS VERSION
			//         AH = 30h
			// ---DOS 5.0---
			//         AL = what to return in BH
			//             00h OEM number (as for DOS 2.0-4.0x)
			//             01h version flag
			// Return: AL = major version number (00h if DOS 1.x)
			//         AH = minor version number
			//         BL:CX = 24-bit user serial number
			//                 (most versions do not use this)
			// ---if DOS <5 or AL=00h---
			//         BH = OEM number
			//             00h IBM
			//             05h Zenith
			//             16h DEC
			//             23h Olivetti
			//             29h Toshiba
			//             4Dh Hewlett-Packard
			//             99h STARLITE architecture (OEM DOS, NETWORK DOS, SMP DOS)
			//             FFh Microsoft, Phoenix
			// ---if DOS 5.0 and AL=01h---
			//         BH = version flag
			//             08h DOS is in ROM
			//             10h DOS is in HMA
			// Notes:  the OS/2 v1.x Compatibility Box returns major version 0Ah (10)
			//         the OS/2 v2.x Compatibility Box returns major version 14h (20)
			//         the Windows/NT DOS box returns major version 1Eh (30)
			//         DOS 4.01 and 4.02 identify themselves as version 4.00; use
			//           INT 21/AH=87h to distinguish between the original
			//           European MSDOS 4.00 and the later PCDOS 4.0x and MSDOS 4.0x
			//         generic MSDOS 3.30, Compaq MSDOS 3.31, and others identify
			//           themselves as PC-DOS by returning OEM number 00h
			//         the version returned under DOS 4.0x may be modified by
			//           entries in the special program list (see AH=52h)
			//         the version returned under DOS 5.0 may be modified by SETVER;
			//           use AX=3306h to get the true version number
			// SeeAlso: AX=3000h/BX=3000h,AX=3306h,AX=4452h,AH=87h,INT 15/AX=4900h
			// SeeAlso: INT 2F/AX=122Fh,INT 2F/AX=E002h
			if ( 0 == AL )
				SET_BX(0xFF00);
			else if ( 1 == AL )
				SET_BX(0x1000);
			else
				SET_BL(0);
			SET_AX(dos_sda.DOSVerMajor|(dos_sda.DOSVerMinor<<8));
			SET_CX(0);
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x30, "Get DOS vers" );
			return 0;
		case 0x31:	// DOS 2+ - TERMINATE AND STAY RESIDENT
			// AL = return code
			// DX = number of paragraphs to keep resident
			// Return: Never
			bx = DX;
			DosMemChange(dos_sda.cu_psp, bx < 6 ? 6 : bx, NULL);
			dos_sda.return_code = AL;
			dos_sda.return_mode = 3;
			return_user(true);
			SET_AL(dos_sda.return_code);
			CLR_CF;	// Clear the Carry flag
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x31, "TSR" );
			return 0;
		case 0x32:	// DOS 2+ - GET DOS DRIVE PARAMETER BLOCK FOR SPECIFIC DRIVE
			if ( DL != 0 && DL != 3 )
			{
				SET_AL(0xFF);
				dos_sda.CritErrCode = -ERROR_INV_DRIVE;
				return 0;
			}
			// Flow-thru
		case 0x1F:	// DOS 1+ - GET DRIVE PARAMETER BLOCK FOR DEFAULT DRIVE
			// DL = drive number (00h = default, 01h = A:, etc)
			// Return:
			//	AL = status
			// 		00h successful
			// 		FFh invalid or network drive
			// 	DS:BX -> Drive Parameter Block (DPB) (see #01395) for specified drive
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get DPB" );
			BuildDPB(&(dos_sda.dpb));	// Refresh the DPB in DOS area.
			REG_DS = DOS_SDA_SEG;
			SET_BX(((u8 *)(&dos_sda.dpb)) - &dos_sda.ErrorMode);
			SET_AL(0);
			return 0;
		case 0x33:
			// DOS 2+ - EXTENDED BREAK CHECKING
			// AH = 33h
			// AL = subfunction
			//
			// Note: Under DOS 3.1+ and DR DOS, this function does not use any of the DOS-internal stacks and may thus be called at any time 
			//
			// See Also: AX=3302h 
			//
			// Category: DOS Kernel - Int 21h - D 
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Break checking" );
			switch ( AL )
			{
				case 1:
					// 01h set state of extended ^C/^Break checking
					// DL = new state
					dos_sda.break_ena = DL & 1;
					// fall-thru
				case 0:	
					// 00h get current extended break state
					// Return:DL = current state, 00h = off, 01h = on
					SET_DL(dos_sda.break_ena);
					return 0;
				case 5:	// DOS 4.0+ - GET BOOT DRIVE
					// Return:DL = boot drive (1=A:,...)
					SET_DL(3);		// DL = 3 = C:
					return 0;
				case 6:	// DOS 5+ - GET TRUE VERSION NUMBER
					// Return:	BL = major version
					// 			BH = minor version
					//			DL = revision (bits 2-0, all others 0)
					//			DH = version flags
					//					bit 3:DOS is in ROM
					//					bit 4:DOS is in HMA.
					SET_DX(0);
					SET_BX(5);
					return 0;
				case 7:	// Windows95 - SET/CLEAR DOS_FLAG
					return 0;
				default:
					LOGI("Break State subfunction %d. ", AL );
					break;
			}
			break;
		case 0x34:	// DOS 2+ - GET ADDRESS OF INDOS FLAG
			REG_ES = DOS_SDA_SEG;
			SET_BX(1);
			return 0;
		case 0x35:	// DOS 2+ - GET INTERRUPT VECTOR
			// AL = interrupt number
			// Return:ES:BX -> current interrupt handler
			rc = INTVectors[AL];
			REG_ES = (rc>>16)&0xFFFF;
			SET_BX(rc&0xFFFF);
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x35, "Get int vect" );
			return 0;
		case 0x36:	// DOS 2+ - GET FREE DISK SPACE
			// DL = drive number (00h = default, 01h = A:, etc)
			// Return:	AX = FFFFh if invalid drive
			// 	else
			// 			AX = sectors per cluster
			// 			BX = number of free clusters
			// 			CX = bytes per sector
			// 			DX = total clusters on drive
			// Free space on drive in bytes is AX * BX * CX. Total space on drive in bytes is AX * CX * DX.
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), 0x36, "Get disk free" );
			if ( (0 == DL && 2 == DosGetCuDrive()) || DL == 3 )
			{
				int ax, bx, cx, dx;
				HDDLedOn();
				DosGetFreeDisk(&ax, &bx, &cx, &dx);
				HDDLedOff();
				SET_AX(ax);
				SET_BX(bx);
				SET_CX(cx);
				SET_DX(dx);
			}
			else if ( (0 == DL && 3 == DosGetCuDrive()) || (HaveCD && 4 == DL))
			{
				// D: == CD-ROM
				SET_AX(1);
				SET_BX(0);
				SET_CX(2048);
				SET_DX(65535);
			}
			else
				SET_AX(0xFFFF);
			return 0;
		case 0x37:	// DOS 2+ - SWITCHAR - GET/SET SWITCH CHARACTER
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get '/' char" );
			switch ( AL )
			{
				case 0x00:	// Get switch char
					SET_DL(dos_sda.switchar);
					SET_AL(0);
					return 0;
				case 0x01:	// Set switch char
					dos_sda.switchar = DL;
					SET_AL(0);
					return 0;
			}
			break;
		case 0x38:	//DOS 2+ - GET COUNTRY-SPECIFIC INFORMATION
			// Input: 	BX = 16-bit country code (see #01400), DS:DX -> buffer for returned info (see #01399)
			// Return:	BX = country code, DS:DX buffer filled
			memcpy( DS_DX_PTR, CountryInfo, 0x18 );
			SET_BX(1);	// USA
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get country nfo" );
			return 0;
		case 0x39:	// DOS 2+ - MKDIR - CREATE SUBDIRECTORY
			// DS:DX -> ASCIZ pathname
			// Return:CF clear if successful
			// 			AX destroyed
			// 		  CF set on error
			// 			AX = error code (03h,05h) (see #01680 at AH=59h/BX=0000h)
			// --- flow-thru intentional ---
		case 0x3A:	// DOS 2+ - RMDIR - REMOVE SUBDIRECTORY
			// DS:DX -> ASCIZ pathname of directory to be removed
			// Return:CF clear if successful
			// 			AX destroyed
			// 		  CF set on error
			// 			AX = error code (03h,05h,06h,10h) (see #01680 at AH=59h/BX=0000h)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosMkRmdir(DS_DX_PTR, AH);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			return 0;
		case 0x3B:	// DOS 2+ - CHDIR - SET CURRENT DIRECTORY
			// DS:DX -> ASCIZ pathname to become current directory (max 64 bytes)
			// Return:	CF clear if successful
			// 				AX destroyed
			// 			CF set on error
			// 				AX = error code (03h) (see #01680 at AH=59h/BX=0000h)
			// Changing the current directory also changes the directory in which FCB file calls operate.
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosChangeDir(DS_DX_PTR);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			return 0;
		case 0x3C:	// DOS 2+ - CREAT - CREATE OR TRUNCATE FILE
			// 	CX = file attributes (see #01401)
			// 	DS:DX -> ASCIZ filename
			// Return:	CF clear if successful
			// 				AX = file handle
			// 			CF set on error
			// 				AX = error code (03h,04h,05h) (see #01680 at AH=59h/BX=0000h)
			// Bitfields for file attributes:
			//	Bit(s) Description (Table 01401)
			//	0 read-only
			//	1 hidden
			//	2 system
			//	3 volume label (ignored)
			//	4 reserved, must be zero (directory)
			//	5 archive bit
			//	7 if set, file is shareable under Novell NetWare 
			//
			if ( CL & 0x1F )	// Only Archive attribute is allowed
			{
				rc = ERROR_ACCESS;
				goto error_exit;
			}
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosOpen(DS_DX_PTR, O_RDWR | O_CREAT | O_TRUNC, CL);	// Create or Truncate
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x3D:	// DOS 2+ - OPEN - OPEN EXISTING FILE
			// 	AL = access and sharing modes (see #01402)
			// 	DS:DX -> ASCIZ filename
			// 	CL = attribute mask of files to look for (server call only)
			// Return:	CF clear if successful
			// 				AX = file handle
			// 			CF set on error
			// 				AX = error code (01h,02h,03h,04h,05h,0Ch,56h) (see #01680 at AH=59h)
			//
			// Bitfields for access and sharing modes:
			//	Bit(s) Description (Table 01402)
			//	2-0 access mode.
			// 			000 read only.
			//			001 write only.
			//			010 read/write.
			//			011 (DOS 5+ internal) passed to redirector on EXEC to allow case-sensitive filenames
			//	3 reserved (0)
			//	6-4 sharing mode (DOS 3.0+) (see #01403).
			//			000 compatibility mode.
			//			001 "DENYALL" prohibit both read and write access by others.
			//			010 "DENYWRITE" prohibit write access by others.
			//			011 "DENYREAD" prohibit read access by others.
			//			100 "DENYNONE" allow full access by others.
			//			111 network FCB (only available during server call)
			//	7 inheritance. If set, file is private to current process and will not be inherited by child processes 
			//
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosOpen(DS_DX_PTR, AL&3, CL);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x3E:	// DOS 2+ - CLOSE - CLOSE FILE
			// 	BX = file handle
			// Return:	CF clear if successful
			// 				AX destroyed
			// 			CF set on error
			// 				AX = error code (06h) (see #01680 at AH=59h/BX=0000h)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Close file" );
			DosClose(BX);
			HDDLedOff();
			return 0;
		case 0x3F:	// DOS 2+ - READ - READ FROM FILE OR DEVICE
			// 	BX = file handle
			// 	CX = number of bytes to read
			// 	DS:DX -> buffer for data
			// Return:	CF clear if successful
			// 				AX = number of bytes actually read (0 if at EOF before call)
			// 			CF set on error
			// 				AX = error code (05h,06h) (see #01680 at AH=59h/BX=0000h)
			//
			if (IntDebug)
				LOGI("%04X:%04X Rd %04X:%04X %04X\n", registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)),
					REG_DS, DX, CX );
			HDDLedOn();
#if 1
			rc = EMSPages[(((REG_DS)<<4)+((DX)&0xFFFF))>>EMSPAGESHIFT] & (PAGE_FLAG_MODEX|PAGE_FLAG_EGA);
			if (rc)
			{
				// Address not in RAM!
				// - "Heimdall" reads data directly to EGA VRAM.
				//	 A000:4030, DS_DX_PTR=0x10DAE030, EGAVGA_A000=0x436A8000, addr=0x436B80C0
				// - "Jazz Jackrabbit" reads data directly to Mode-X VRAM.
				int pagetype = rc;
				char *data = (char *)calloc(CX, 1);
				if (!data)
					return -1;
				rc = DosRead(BX, CX, data);
				if (rc > 0)
				{
					int i = 0;
					int addr = ((int)DS_DX_PTR)<<2;
					if (PAGE_FLAG_EGA == pagetype)
					{
						for (i=0; i < rc; i++)
							EGA_write_byte_from_C(data[i], addr+(i<<2));
					}
					/*
					else if (PAGE_FLAG_MODEX == pagetype)
					{
						for (i=0; i < rc; i++)
							MODEX_write_byte_from_C(data[i], addr+(i<<2));
					}
					*/
				}
				free(data);
			}
			else
#endif
				rc = DosRead(BX, CX, DS_DX_PTR);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x40:	// DOS 2+ - WRITE - WRITE TO FILE OR DEVICE
			// 	BX = file handle
			// 	CX = number of bytes to write
			// 	DS:DX -> data to write
			// Return:	CF clear if successful
			// 				AX = number of bytes actually written
			// 			CF set on error
			// 				AX = error code (05h,06h) (see #01680 at AH=59h/BX=0000h)
			//
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Write file" );
			rc = DosWrite(BX, CX, DS_DX_PTR);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x41:	// DOS 2+ - UNLINK - DELETE FILE
			// DS:DX -> ASCIZ filename (no wildcards, but see notes)
			// CL = attribute mask for deletion (server call only, see notes)
			// Return:	CF clear if successful
			// 				AX destroyed (DOS 3.3) AL seems to be drive of deleted file
			// 			CF set on error
			// 				AX = error code (02h,03h,05h) (see #01680 at AH=59h/BX=0000h)
			//
			// Notes: (DOS 3.1+) wildcards are allowed if invoked via AX=5D00h, in which case the filespec must be canonical
			// (as returned by AH=60h), and only files matching the attribute mask in CL are deleted.
			// DR DOS 5.0-6.0 returns error code 03h if invoked via AX=5D00h;
			// DR DOS 3.41 crashes if called via AX=5D00h with wildcards.
			// DOS does not erase the file's data; it merely becomes inaccessible because the FAT chain for the file is cleared.
			// Deleting a file which is currently open may lead to filesystem corruption.
			// Unless SHARE is loaded, DOS does not close the handles referencing the deleted file,
			// thus allowing writes to a nonexistant file..
			// Under DR DOS and DR Multiuser DOS, this function will fail if the file is currently open.
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosDelete(DS_DX_PTR);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x42:	// DOS 2+ - LSEEK - SET CURRENT FILE POSITION
			// 	AL = origin of move
			// 			00h start of file
			// 			01h current file position
			// 			02h end of file
			// 	BX = file handle
			// 	CX:DX = (signed) offset from origin of new file position
			// Return:	CF clear if successful
			// 				DX:AX = new file position in bytes from start of file
			// 			CF set on error
			// 				AX = error code (01h,06h) (see #01680 at AH=59h/BX=0000h)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Seek file" );
			rc = DosSeek(BX, (CX<<16)|DX, AL);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			SET_DX(rc>>16);
			return 0;
		case 0x43:	// DOS 2+ - GET/SET FILE ATTRIBUTES
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			switch ( AL )
			{
				case 0:	// DOS 2+ - GET FILE ATTRIBUTES
					// 	DS:DX -> ASCIZ filename
					// Return:	CF clear if successful
					// 				CX = file attributes (see #01420)
					// 				AX = CX (DR DOS 5.0)
					// 			CF set on error
					// 				AX = error code (01h,02h,03h,05h) (see #01680 at AH=59h)
					//
					// Bit(s)  Description     (Table 01420)
					// 7      shareable (Novell NetWare)
					// 7      pending deleted files (Novell DOS, OpenDOS)
					// 6      unused
					// 5      archive
					// 4      directory
					// 3      volume label.
					// Execute-only (Novell NetWare)
					// 2      system
					// 1      hidden
					// 0      read-only
					HDDLedOn();
					rc = DosGetFAttr(DS_DX_PTR);
					HDDLedOff();
					if ( rc < 0 )
						goto error_exit;
					SET_CX(rc&0xFFFF);
					return 0;
				case 1:	// DOS 2+ - CHMOD - SET FILE ATTRIBUTES
					// CX = new file attributes (see #01420)
					// DS:DX -> ASCIZ filename
					// Return:CF clear if successful
					// AX destroyed
					// CF set on error
					// AX = error code (01h,02h,03h,05h) (see #01680 at AH=59h)
					// TODO!
					return 0;
			}
			rc = ERROR_INVFUNCTION;
			goto error_exit;
		case 0x44:	// INT 21 - DOS 2+ - IOCTL
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "IOCTL" );
			rc = DosDevIOctl(AL);
			if (rc < 0)
			{
				if (rc != ERROR_DEVICE && rc != ERROR_ACCESS)
					dos_sda.CritErrCode = -rc;
				goto error_exit;
			}
			else if ( rc > 0 )
				return -1;
			return 0;
		case 0x45:	// DOS 2+ - DUP - DUPLICATE FILE HANDLE
			// BX = file handle
			// Return:
			//	CF clear if successful
			// 		AX = new handle
			// 	CF set on error
			// 		AX = error code (04h,06h) (see #01680 at AH=59h/BX=0000h)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "DUP" );
			rc = DosDup(BX);
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x46:	// DOS 2+ - DUP2, FORCEDUP - FORCE DUPLICATE FILE HANDLE
			// BX = file handle
			// CX = file handle to become duplicate of first handle
			// Return:CF clear if successful
			// CF set on error
			// AX = error code (04h,06h) (see #01680 at AH=59h/BX=0000h)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "ForceDUP" );
			rc = DosForceDup(BX, CX);
			if ( rc < 0 )
				goto error_exit;
			return 0;
		case 0x47:	// DOS 2+ - CWD - GET CURRENT DIRECTORY
			// DL = drive number (00h = default, 01h = A:, etc)
			// DS:SI -> 64-byte buffer for ASCIZ pathname
			// Return:	CF clear if successful
			// 				AX = 0100h (undocumented)
			// 			CF set on error
			// 				AX = error code (0Fh) (see #01680 at AH=59h/BX=0000h)
			//
			// Notes: The returned path does not include a drive or the initial backslash.
			// Many Microsoft products for Windows rely on AX being 0100h on success.
			HDDLedOn();
			rc = DosGetCuDir(DL, DS_SI_PTR);
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_SI_PTR );
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(0x0100);
			return 0;
		case 0x48:	// DOS 2+ - ALLOCATE MEMORY
			// Input: 	BX = number of paragraphs to allocate
			// Return:	AX = segment of allocated block, BX = size of largest available block
			ax = AX;
			bx = BX;
			if ((rc = DosMemAlloc(bx, dos_sda.mem_access_mode, &ax, &bx)) != NO_ERROR)
			{
				DosMemLargest(&bx);
				SET_BX(bx);
				//if (DosMemCheck() != SUCCESS)
				//	panic("MCB chain corrupted");
				goto error_exit;
			}
			ax++;   /* DosMemAlloc() returns seg of MCB rather than data */
			SET_AX(ax);
			SET_BX(bx);
			if (IntDebug)
				LOGI("%04X:%04X Alloc %04X %05X\n", registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), ax, bx<<4 );
			return 0;
		case 0x49:	// DOS 2+ - FREE MEMORY
			// Input:	ES = segment of block to free
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Free mem" );
			if ((rc = DosMemFree(REG_ES - 1)) != NO_ERROR)
			{
				//if (DosMemCheck() != SUCCESS)
				//	panic("MCB chain corrupted");
				goto error_exit;
			}
			return 0;
		case 0x4A:	// DOS 2+ - RESIZE MEMORY BLOCK
			// Input:	BX = new size in paragraphs, ES = segment of block to resize
			// Return:	BX = maximum paragraphs available for specified memory block
			bx = BX;
			if (IntDebug)
				LOGI("%04X:%04X Resiz %04X %05X\n", registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), REG_ES, bx<<4 );
			if ((rc = DosMemChange(REG_ES, bx, &bx)) != NO_ERROR)
			{
				SET_BX(bx);
				//if (DosMemCheck() != SUCCESS)
				//	panic("after 4a: MCB chain corrupted");
				goto error_exit;
			}
			SET_AX(REG_ES); 	/* Undocumented MS-DOS behaviour expected by BRUN45! */
			return 0;
		case 0x4B:	// DOS 2+ - EXEC - LOAD AND/OR EXECUTE PROGRAM
			// AL = type of load
			// 		00h load and execute
			// 		01h load but do not execute
			// 		03h load overlay (see #01591)
			// 		04h load and execute in background (European MS-DOS 4.0 only)
			// 		"Exec & Go" (see also AH=80h)
			// DS:DX -> ASCIZ program name (must include extension)
			// ES:BX -> parameter block (see #01590,#01591,#01592)
			// CX = mode (subfunction 04h only)
			// 		0000h child placed in zombie mode after termination
			// 		0001h child's return code discarded on termination
			// Return:	CF clear if successful
			// 			BX,DX destroyed
			// 			if subfunction 01h, process ID set to new program's PSP; get with INT 21/AH=62h
			// 			CF set on error
			// 			AX = error code (01h,02h,05h,08h,0Ah,0Bh) (see #01680 at AH=59h)
			// 
			// Notes: DOS 2.x destroys all registers, including SS:SP.
			// Under ROM-based DOS, if no disk path characters (colons or slashes) are included in the program name,
			// the name is searched for in the ROM module headers (see #01595) before searching on disk.
			// For functions 00h and 01h, the calling process must ensure that there is enough unallocated memory available;
			// if necessary, by releasing memory with AH=49h or AH=4Ah. For function 01h, the AX value to be passed to the
			// child program is put on top of the child's stack. For function 03h, DOS assumes that the overlay is being
			// loaded into memory allocated by the caller. Function 01h was undocumented prior to the release of DOS 5.0.
			// Some versions (such as DR DOS 6.0) check the parameters and parameter block and return an error if an invalid
			// value (such as an offset of FFFFh) is found. Background programs under European MS-DOS 4.0 must use the new executable format.
			// This function ignores the filename extension, instead checking the first two bytes of the file to determine whether
			// there is a valid .EXE header (see #01594); if not, the file is assumed to be in .COM format. If present, the file
			// may be in any of several formats which are extensions of the original .EXE format (see #01593).
			// .COM-format executables begin running with the following register values:
			//		AL = 00h if first FCB has valid drive letter, FFh if not
			//		AH = 00h if second FCB has valid drive letter, FFh if not
			//		CS,DS,ES,SS = PSP segment
			//		SP = offset of last word available in first 64K segment 
			// 		(note:AX is always 0000h under DESQview).
			// Old-format executables begin running with the following register values:
			// 		AL = 00h if first FCB has valid drive letter, FFh if not
			// 		AH = 00h if second FCB has valid drive letter, FFh if not
			// 		DS,ES = PSP segment
			// 		SS:SP as defined in .EXE header
			// 		(note:AX is always 0000h under DESQview).
			//
			// BUGS: DOS 2.00 assumes that DS points at the current program's PSP.
			// Load Overlay (subfunction 03h) loads up to 512 bytes too many if the file contains additional
			// data after the actual overlay. Load but Do Not Execute (subfunction 01h) is reported to corrupt
			// the top word of the caller's stack if the loaded module terminates with INT 21/AH=4Ch in some
			// versions of MS-DOS, including v5.00. 
			// 
			// See Also: AX=4B05h - AH=4Ch - AH=4Dh - AH=64h/BX=0025h 
			// 
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosExec(AL, (EXEC_BLK *)ES_BX_PTR, DS_DX_PTR);
			if ( rc < 0 )
				goto error_exit;
			if ( dos_sda.ExecFlags & EXEC_DEBUG )	// Are we to start in the debugger?
			{
				dos_sda.ExecFlags &= ~EXEC_DEBUG;
				return -1;					// Break into the debugger
			}
			return 0;	// Now we should be in the new process, so just start running it
		case 0x4C:	// DOS Exit Program, AL = errorlevel
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Exit program" );
			dos_sda.return_code = AL;
			dos_sda.return_mode = 0;
			return_user(false);
			SET_AL(dos_sda.return_code);
#if LOG_UNTIL_EXIT
			fflush(stdout);
			return -1;
#endif
			return 0;
		case 0x4D:
			// DOS 2+ - GET RETURN CODE (ERRORLEVEL)
			// AH = 4Dh
			// Return:AH = termination type
			// 00h normal (INT 20,INT 21/AH=00h, or INT 21/AH=4Ch)
			// 01h control-C abort
			// 02h critical error abort
			// 03h terminate and stay resident (INT 21/AH=31h or INT 27)
			// AL = return code
			// CF clear
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get retval" );
			SET_AL(dos_sda.return_code);
			SET_AH(dos_sda.return_mode);	// Needed in Jazz Jackrabbit!
			return 0;
		case 0x4E:	// DOS 2+ - FINDFIRST - FIND FIRST MATCHING FILE
			// AL = special flag for use by APPEND (refer to note below)
			// CX = file attribute mask (see #01420 at AX=4301h) (bits 0 and 5 ignored)
			// DS:DX -> ASCIZ file specification (may include path and wildcards)
			// Return:	CF clear if successful
			// 			Disk Transfer Area filled with FindFirst data block (see #01626)
			// 			CF set on error
			// 			AX = error code (02h,03h,12h) (see #01680 at AH=59h/BX=0000h)
			//
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosFindFirst(DS_DX_PTR, CX);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			else
				SET_AX(0);		// Undocumented behaviour, needed by "Uninvited".
			return 0;
		case 0x4F:
			// DOS 2+ - FINDNEXT - FIND NEXT MATCHING FILE
			// Disk Transfer Area contains data block from previous FindFirst or FindNext call
			// Return:	CF clear if successful
			// 			Disk Transfer Area updated
			// 			CF set on error
			// 			AX = error code (12h) (see #01680 at AH=59h/BX=0000h)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "FindNext" );
			rc = DosFindNext();
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			else
				SET_AX(0);		// Undocumented behaviour, needed by "Uninvited".
			return 0;
		case 0x50:	// DOS 2+ internal - SET CURRENT PROCESS ID (SET PSP ADDRESS)
			// BX = segment of PSP for new process
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get PSP" );
			dos_sda.cu_psp = BX;
			return 0;
		case 0x52:	// DOS 2+ internal - SYSVARS - GET LIST OF LISTS
			// Return:
			//	ES:BX -> DOS list of lists (see #01627)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get SYSVARS" );
			REG_ES = 0x0080;
			SET_BX(0x0026);
			return 0;
		case 0x53:	// DOS 2+ internal - TRANSLATE BIOS PARAMETER BLOCK TO DRIVE PARAM BLOCK
			// DS:SI -> BIOS Parameter Block (see #01663)
			// ES:BP -> buffer for Drive Parameter Block (see #01395 at AH=32h)
			// DBP drive byte must be set to valid drive (Windows95-OSR2)
			// ---Windows95---
			// CX = signature 4558h ('EX') for FAT32 extended BPB/DPB (see #01664)
			// DX = signature 4152h ('AR') for FAT32 extended BPB/DPB
			// Return:ES:BP buffer filled
			bpb_to_dpb((BPB *)DS_SI_PTR, (DPB *)ES_BP_PTR);
			return 0;
		case 0x54:	// DOS 2+ - GET VERIFY FLAG
			// Return:AL = verify flag
			// 00h off
			// 01h on (all disk writes verified after writing)
			SET_AL(dos_sda.verify_ena);
			return 0;
		case 0x55:	// DOS 2+ internal - CREATE CHILD PSP
			// DX = segment at which to create new PSP
			// SI = (DOS 3.0+) value to place in memory size field at DX:[0002h]
			// Return:AL destroyed
			//
			// Notes: Creates a "child" PSP rather than making an exact copy of the current PSP;
			// the new PSP's parent pointer is set to the current PSP and the reference count for each inherited file is incremented.
			// (DOS 2.0+) sets current PSP to DX.
			// (DOS 3.0+) marks "no inherit" file handles as closed in child PSP.
			// This function is implemented using the same code as AH=26h, so unlike other DOS 2+ functions,
			// it does not return status in CF, instead returning status in AL as DOS 1.x functions do
			// (but it never puts an explicit return value in AL) 
			child_psp(DX, dos_sda.cu_psp, SI);
			dos_sda.cu_psp = DX;
			return 0;
		case 0x56:	// DOS 2+ - RENAME - RENAME FILE
			// DS:DX -> ASCIZ filename of existing file (no wildcards, but see below)
			// ES:DI -> ASCIZ new filename (no wildcards)
			// CL = attribute mask (server call only, see below)
			// Return:	CF clear if successful
			// 			CF set on error
			// 				AX = error code (02h,03h,05h,11h) (see #01680)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosRename(DS_DX_PTR, ES_DI_PTR);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			return 0;
		case 0x57:	// DOS 2+ - GET/SET FILE'S LAST-WRITTEN DATE AND TIME
			switch ( AL )
			{
				case 0x00:
					// BX = file handle
					// Return:
					//	CF clear if successful
					// 		CX = file's time (see #01665)
					// 		DX = file's date (see #01666)
					// 	CF set on error
					// 		AX = error code (01h,06h) (see #01680)
					HDDLedOn();
					if (IntDebug)
						LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get FTime" );
					rc = DosGetFtime(BX, &ax, &bx);
					HDDLedOff();
					if ( rc < 0 )
						goto error_exit;
					SET_DX(ax);	// File's Date
					SET_CX(bx);	// File's Time
					return 0;
				case 0x01:
					return 0;
				default:
					rc = -1;
					goto error_exit;
			}
			break;
		case 0x58:	// DOS 2.11+ - GET OR SET MEMORY ALLOCATION STRATEGY / DOS 5+ - GET OR SET UMB LINK STATE
			// AL = subfunction
			//		00h get allocation strategy
			//			Return:AX = current strategy (see #01679)
			// 				Values for DOS memory allocation strategy: 
			// 				00h low memory first fit
			// 				01h low memory best fit
			// 				02h low memory last fit
			// 				40h high memory first fit
			// 				41h high memory best fit
			// 				42h high memory last fit
			// 				80h first fit, try high then low memory
			// 				81h best fit, try high then low memory
			// 				82h last fit, try high then low memory			
			// 		01h set allocation strategy
			// 			BL = new allocation strategy (see #01679)
			// 			BH = 00h (DOS 5+)
			// 		02h get UMB link state
			// 			Return:AL = current link state
			// 				00h UMBs not part of DOS memory chain
			// 				01h UMBs in DOS memory chain
			// 		03h set UMB link state
			// 			BX = new link state
			// 				0000h remove UMBs from DOS memory chain
			// 				0001h add UMBs to DOS memory chain
			// Return:	CF clear if successful
			// 			CF set on error
			// 			AX = error code (01h) (see #01680)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Alloc strategy" );
			switch ( AL )
			{
				case 0x00:
					SET_AX(dos_sda.mem_access_mode);
					return 0;
				case 0x02:
					SET_AL(0);					// UMBs not part of DOS memory chain
					return 0;
				case 0x01:
					if ( BX >= 0 && BX <= 2)
					{
						dos_sda.mem_access_mode = BX;
						return 0;
					}
					else
					{
						rc = -1;
						goto error_exit;
					}
				case 0x03:
					if ( 0 == BX)
						return 0;
					else
					{
						rc = -1;
						goto error_exit;
					}
			}
			break;
		case 0x59:	// DOS 3.0+ - GET EXTENDED ERROR INFORMATION
			// BX = 0000h
			// Return:AX = extended error code (see #01680)
			// BH = error class (see #01682)
			// BL = recommended action (see #01683)
			// CH = error locus (see #01684)
			// ES:DI may be pointer (see #01681, #01680)
			// CL, DX, SI, BP, and DS destroyed
			SET_AX(dos_sda.CritErrCode);
			SET_CX(dos_sda.CritErrLocus);
			SET_BX((dos_sda.CritErrClass<<8)|(dos_sda.CritErrAction));
			SET_DI(dos_sda.CritErrDev);
			REG_ES = (dos_sda.CritErrDev>>16&0xFFFF);
			return 0;
		case 0x5A:	// DOS 3.0+ - CREATE TEMPORARY FILE
			// CX = file attribute (see #01420 at AX=4301h)
			// DS:DX -> ASCIZ path ending with a '\' + 13 zero bytes to receive the generated filename
			// Return:
			//	CF clear if successful
			//		AX = file handle opened for read/write in compatibility mode
			//		DS:DX pathname extended with generated name for temporary file
			//	CF set on error
			//		AX = error code (03h,04h,05h) (see #01680)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosMkTmp(DS_DX_PTR, CL);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x5B:	// DOS 3.0+ - CREATE NEW FILE
			// CX = file attribute (see #01420 at AX=4301h)
			// DS:DX -> ASCIZ filename
			// Return:CF clear if successful
			// AX = file handle opened for read/write in compatibility mode
			// CF set on error
			// AX = error code (03h,04h,05h,50h) (see #01680)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_DX_PTR );
			rc = DosOpen(DS_DX_PTR, O_RDWR | O_CREAT, CL);
			HDDLedOff();
			if ( rc < 0 )
				goto error_exit;
			SET_AX(rc);
			return 0;
		case 0x5C:	// DOS 3.0+ - FLOCK - RECORD LOCKING
			// Notes: Error returned unless SHARE or network installed.
			SET_CF;
			SET_AX(1);
			return 0;
		case 0x5D:
			switch ( AL )
			{
				case 0x06:	// DOS 3.0+ internal - GET ADDRESS OF DOS SWAPPABLE DATA AREA
					// Return:
					//	CF set on error
					// 		AX = error code (see #01680)
					// 	CF clear if successful
					// 		DS:SI -> nonreentrant data area (includes all three DOS stacks)
					// 			(critical error flag is first byte) (see #01687)
					// 		CX = size in bytes of area which must be swapped while in DOS
					// 		DX = size in bytes of area which must always be swapped
					CLR_CF;	// Clear the Carry flag
					REG_DS = DOS_SDA_SEG;
					SET_SI(0);
					SET_CX(0x80);
					SET_DX(0x1A);
					return 0;
			}
			break;
		case 0x5F:	//  DOS 3.1+ network
			rc = ERROR_INVFUNCTION;
			goto error_exit;
		case 0x60:	// DOS 3.0+ - TRUENAME - CANONICALIZE FILENAME OR PATH
			// DS:SI -> ASCIZ filename or path
			// ES:DI -> 128-byte buffer for canonicalized name
			// Return:CF set on error
			// AX = error code
			// 02h invalid component in directory path or drive letter only
			// 03h malformed path or invalid drive letter
			// ES:DI buffer unchanged
			// CF clear if successful
			// AH = 00h or 3Ah (DOS 6.1/6.2 for character device)
			// AL = destroyed (00h or 2Fh or 5Ch or last character of current directory on drive)
			// buffer filled with qualified name of form D:\PATH\FILE.EXT or \\MACHINE\PATH\FILE.EXT
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, DS_SI_PTR );
			strcpy( ES_DI_PTR, DS_SI_PTR );
			return 0;
		case 0x51:	// Undocumented!
		case 0x62:	// DOS 3.0+ - GET CURRENT PSP ADDRESS
			// Return:BX = segment of PSP for current process
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get PSP" );
			SET_BX(dos_sda.cu_psp);
			return 0;
		case 0x63:	// DOS 3.2+ - GET DOUBLE BYTE CHARACTER SET LEAD-BYTE TABLE
			// Return: AL = error code
			//			00h successful
			//				DS:SI -> DBCS table (see #01746)
			//			FFh not supported
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get DBLCHR tbl" );
			SET_AL(0xFF);			// Return not supported
			SET_CF;
			return 0;
		case 0x64:	// DOS 3.2+ internal - SET DEVICE DRIVER LOOKAHEAD FLAG
			// AL = flag
			//	00h (default) call device driver function 5 (non-dest read) before INT 21/AH=01h,08h,0Ah
			// 	nonzero don't call driver function 5
			// Return: Nothing (MS-DOS)
			// 	CF set, AX=error code??? (DR DOS 5.0, which does not support this call)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Set lookahead" );
			SET_AL(0xFF);			// Return not supported
			SET_CF;
			return 0;
		case 0x65:	// DOS 3.3+ - GET EXTENDED COUNTRY INFORMATION
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get ext country" );
			SET_CF;
			return 0;
		case 0x66:	// DOS 3.3+ - GET GLOBAL CODE PAGE TABLE
			// AX = 6601h
			// Return:CF set on error
			// AX = error code (see #01680 at AH=59h/BX=0000h)
			// CF clear if successful
			// BX = active code page (see #01757)
			// DX = system code page (see #01757)
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Get codepage tbl" );
			SET_BX(0);		// 0 = Reduced 7-bit ASCII
			SET_DX(0);		// 0 = Reduced 7-bit ASCII
			return 0;
		case 0x67:	// DOS 3.3+ - SET HANDLE COUNT
			// BX = size of new file handle table for process
			// Return:CF clear if successful
			// CF set on error
			// AX = error code (see #01680 at AH=59h/BX=0000h)
			//
			// Desc: Adjust the size of the per-process open file table,
			// thus raising or lowering the limit on the number of files the caller can open simultaneously
			//
			// Notes: If BX <= 20, no action is taken if the handle limit has not yet been increased,
			// and the table is copied back into the PSP if the limit is currently > 20 handles.
			// Only the first 20 handles are copied to child processes in DOS 3.3-6.0.
			// Increasing the file handles here will not, in general, increase the number of files
			// that can be opened using the runtime library of a high-level language such as C.
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Set handlecnt" );
			rc = SetPSPMaxFiles(BX);	// Used in BLOOD to increase max files to 28
			if (rc != 0)
				goto error_exit;
			return 0;
		case 0x68:	// DOS 3.3+ - FFLUSH - COMMIT FILE
		case 0x6A:	// DOS 4.0+ - COMMIT FILE
			// BX = file handle
			// Return:
			//	CF clear if successful
			//		all data still in DOS disk buffers is written to disk immediately,
			//		and the file's directory entry is updated
			//	CF set on error
			//		AX = error code (see #01680 at AH=59h/BX=0000h)
			HDDLedOn();
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Flush file" );
			DosFlush(BX);
			HDDLedOff();
			return 0;
		case 0x69:	// DOS 4.0+ internal - GET/SET DISK SERIAL NUMBER
			// AL = subfunction
			//		00h get serial number
			//		01h set serial number
			// BL = drive (0=default, 1=A, 2=B, etc)
			// BH = info level (00h only for DOS; OS/2 allows other levels)
			// DS:DX -> disk info (see #01766)
			// Return:	CF set on error (Error 0005h given if no extended BPB on disk)
			//			AX = error code (see #01680 at AH=59h/BX=0000h)
			//			CF clear if successful
			//			AX destroyed
			//			(AL = 00h) buffer filled with appropriate values from extended BPB
			//			(AL = 01h) extended BPB on disk set to values from buffer
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Disk serial#" );
			rc = ERROR_INV_DRIVE;
			goto error_exit;
		case 0x71:	// Windows95 - LONG FILENAME FUNCTIONS
			if (IntDebug)
				LOGI(dbgformat, registers[11], registers[8]-(int)(PHYS_PTR(registers[11], 0)), AH, "Long name fct" );
			SET_AX(0x7100);
			SET_CF;
			return 0;
		case 0x73:	// MS-DOS 7 - DRIVE LOCKING AND FLUSHING
			SET_CF;
			return 0;
		case 0xDC:	// Novell NetWare - CONNECTION SERVICES - GET CONNECTION NUMBER
			// AH = DCh
			// Return:AL = logical connection number
			// 00h if NetWare not loaded or this machine is a non-dedicated server
			// CX = station number in ASCII (CL = first digit)
			SET_AX(0);
			return 0;
		case 0x87:	// European MS-DOS 4.0 - GETPID - GET PROCESS IDENTIFIER (Used in "Moria")
		case 0xE3:	// Novell NetWare - CONNECTION CONTROL
		case 0xEF:	// Novell NetWare - WORKSTATION
		case 0xF4:	// DoubleDOS - INSTALLATION CHECK/PROGRAM STATUS
			return 0;
		case 0xFF:
			rc = ERROR_INVFUNCTION;
			goto error_exit;
		default:
			//SET_AL(0xFF);
			break;
	}
	sprintf( BreakReasonText, "int 21 AH=%02X\n", AH );
	BreakReason = BreakReasonText;
	return -1;		// -1 = unsupported INT21 call, break into debugger
	
error_exit:
	if (IntDebug)
		LOGI("  int21 ax=%04X fail %d\n", AX, -rc);
/*
	if ( 0x3D == AH )
	{
		SET_AX(-rc);
		if (NO_ERROR == dos_sda.CritErrCode)
			dos_sda.CritErrCode = (-rc);
		SET_CF;	// Set the Carry flag
		return -1;
	}
*/	
	SET_AX(-rc);
	if (NO_ERROR == dos_sda.CritErrCode)
		dos_sda.CritErrCode = (-rc);
	SET_CF;	// Set the Carry flag
	return 0;
}
