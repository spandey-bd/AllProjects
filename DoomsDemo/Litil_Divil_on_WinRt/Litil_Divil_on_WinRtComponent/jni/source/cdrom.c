//=============================================================================
// cdrom.c
//
// This file handles the cdrom emulation.
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

#include <stdio.h>
#include <string.h>

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

bool 	HaveCD = false;
char	HostCD[128];

static int ioctl_input(u8 *buf)
{
	switch(buf[0])
	{
		case 0x07:	// Get Sector Size, Fatal Racing
#ifdef RPi
			printf("IOCTL input, get sector size, buf[1] = %d\n", buf[1]);
#endif
			if (0 == buf[1])
			{
				buf[2] = 2048 & 0xFF;
				buf[3] = 2048>>8;
				buf[4] = 0;
				buf[5] = 0;
				return 0;
			}
			else if (1 == buf[1])
			{
				buf[2] = 2352 & 0xFF;
				buf[3] = 2352>>8;
				buf[4] = 0;
				buf[5] = 0;
				return 0;
			}
			return 0x03;	// Invalid function
		case 0x0A:	// Get Audio Disk Info, Fatal Racing
			//		DiskInfo  DB   10        ; Control block code
			//				  DB   ?         ; Lowest track number
			//				  DB   ?         ; Highest track number
			//				  DD   ?         ; Starting point of the lead-out track (frames, seconds, minutes, hours)
#ifdef RPi
			printf("IOCTL input, Get Audio Disk Info\n");
#endif
			buf[1] = 1; 	// Starting track
			buf[2] = 10;	// Ending track
			buf[3] = 0;		// lead-out frames
			buf[4] = 0;		// lead-out seconds
			buf[5] = 60;	// lead-out minutes
			buf[6] = 0;		// lead-out hours
			return 0;
		case 0x0B:	// Get Audio Track Info, Fatal Racing
			//		TnoInfo   DB   11        ; Control block code
			//				  DB   ?         ; Track number
			//				  DD   ?         ; Starting point of the track
			//				  DB   ?         ; Track control information
#ifdef RPi
			printf("IOCTL input, Get Audio Track Info, track=%d\n", buf[1]);
#endif
			buf[2] = 0;			// starting point frames
			buf[3] = 0;			// starting point seconds
			buf[4] = 6*buf[1];	// starting point minutes
			buf[5] = 0;			// starting point hours
			buf[6] = 0;			// attributes: 2 audio channels without pre-emphasis, digital copy prohibited
			return 0;
		default:
#ifdef RPi
			printf("IOCTL input, ioctl_fct=%02X\n", buf[0]);
#endif
			break;
	}
	return 0x03;	// Invalid function
}

static void ioctl_output(u8 *buf, int len)
{
	int i;
	switch (buf[0])
	{
		case 0x02:	// Reset drive
#ifdef RPi
			printf("Reset drive.\n");
#endif						
			break;
		case 0x03: // Audio channel control, set volume for channels 0..3 (0 = off, 1..255 = on)
			break;
		default:
#ifdef RPi
			printf("IOCTL output, ioctl_fct=%02X, len=%d\n", buf[0], len);
			for (i = 0; i < len; i++)
				printf("%02X,", buf[i]);
			printf("\n");
#endif
			break;
	}
}

int cdrom_2F()
{
	switch ( AX )
	{
		case 0x1500:	// CD-ROM - INSTALLATION CHECK
			//	Return:
			//	BX = number of CD-ROM drive letters used
			//	CX = starting drive letter (0=A:)
			//	AX = 15FFh (Novell DOS 7 NWCDEX only!)
			if (HaveCD)
			{
				SET_AX(0x15FF);
				SET_BX(1);			// 1 CD-ROM drive
				SET_CX(3);			// D: drive
			}
			else
			{
				SET_CF;
			}
			return 0;
		case 0x1501:	// CD-ROM - GET DRIVE DEVICE LIST (X-Wing INSTALL.EXE)
		case 0x1505:	// CD-ROM - READ VTOC
		case 0x150C:	// CD-ROM v2.00+ - GET MSCDEX.EXE VERSION (GET VERSION)
		case 0x150D:	// CD-ROM v2.00+ - GET CD-ROM DRIVE LETTERS
		case 0x150F:	// CD-ROM v2.00+ - GET DIRECTORY ENTRY (Little Big Adventure)
#ifdef RPi
			printf("cdrom_2F: AX=%04X\n", AX);
#endif
			SET_CF;
			return 0;
		case 0x150B:
			// CD-ROM v2.00+ - DRIVE CHECK
			// CX = drive number (0=A:)
			// Return:BX = ADADh if MSCDEX.EXE installed
			// AX = support status
			// 0000h if drive not supported
			// nonzero if supported
			if (3 == CX && HaveCD)
			{
				SET_AX(1);
				SET_BX(0xADAD);
			}
			else
				SET_AX(0);
			return 0;
		case 0x1510:
			// CD-ROM v2.10+ - SEND DEVICE DRIVER REQUEST
			//	CX = CD-ROM drive letter (0 = A, 1 = B, etc)
			//	ES:BX -> CD-ROM device driver request header (see #02597 at AX=0802h)
			//
			//	Return:
			//	CF clear if device driver has been called (check the request header's
			//	status word to determine whether an error has occurred)
			//	ES:BX request header updated
			//	CF set if device driver has not been called
			//	AX = error code (000Fh = invalid drive, 0001h = invalid function)
			//	ES:BX request header unchanged
			//
			//	Format of device driver request header:
			//
			//	Offset  Size    Description     (Table 02597)
			//	00h    BYTE    length of request header
			//	01h    BYTE    subunit within device driver
			//	02h    BYTE    command code (see #02595)
			//	03h    WORD    status (filled in by device driver) (see #02596)
			//
			if (3 == CX && HaveCD)
			{
				u8 *ptr = (u8 *)ES_BX_PTR;
				ptr[1] = CL;	// Set subunit byte in the request header
				switch (ptr[2])
				{
					case 0x03:	
						// IOCTL INPUT, Fatal Racing, 4 bytes, ioctl_fct = 0x07 = Get sector size
						//	0Dh 	BYTE 	media descriptor (block devices only) 
						//	0Eh 	DWORD 	transfer address 
						//	12h 	WORD 	(call) number of bytes to read/write
						//					(ret) actual number of bytes read or written
						ioctl_input(phys_ptr(ptr[0x10]+256*ptr[0x11], ptr[0x0E]+256*ptr[0x0F]));
						break;
					case 0x0C:	
						// IOCTL OUTPUT, Fatal Racing, 1 byte, ioctl_fct = 0x02 = Reset Drive
						//	0Dh 	BYTE 	media descriptor (block devices only) 
						//	0Eh 	DWORD 	transfer address 
						//	12h 	WORD 	(call) number of bytes to read/write
						//					(ret) actual number of bytes read or written					
						ioctl_output(phys_ptr(ptr[0x10]+256*ptr[0x11], ptr[0x0E]+256*ptr[0x0F]), ptr[0x12]+256*ptr[0x13]);
						break;
					case 0x84:	// Play Audio Sectors
#ifdef RPi
						printf("Play CDDA audio, type=%d, start=%d.\n", ptr[0x0D],
							ptr[0x0E]+(ptr[0x0F]<<8)+(ptr[0x10]<<16)+(ptr[0x11]<<24));
#endif						
						break;
					case 0x85:	// Stop Audio, Fatal Racing
#ifdef RPi
						printf("Stop CDDA audio.\n");
#endif						
						break;
					default:
#ifdef RPi
						printf("ES:BX = %04X:%04X (len=%d, sub=%d, cmnd=%02X, status=%02X%02X)\n", 
							REG_ES, REG_BX, ptr[0], ptr[1], ptr[2], ptr[4], ptr[3]);
#endif
						break;
				}
				// Set status = OK = 0x100
				ptr[3] = 0;
				ptr[4] = 1;
				CLR_CF;
			}
			else
			{
				SET_AX(0x000F);		// Invalid drive
				SET_CF;
			}
			return 0;
	}
	return -1;
}

void MountCD(char *path)
{
	strcpy(HostCD, path);
	while (strlen(HostCD) > 1 && '/' == HostCD[strlen(HostCD)-1])
		HostCD[strlen(HostCD)-1] = 0;
	HaveCD = true;
}

