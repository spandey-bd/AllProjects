//=============================================================================
// DOS_device.c
//
// This file contains the DOS device I/O (mainly console) emulation routines.
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
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

/* initialize SFT fields (for open/creat) for character devices */
//static int DeviceOpenSft(struct dhdr FAR *dhp, sft FAR *sftp)
int DeviceOpenSft(const char *fname, SFT *sftp)
{
	int i;

	sftp->sft_count += 1;
	strcpy(sftp->sft_name, fname);
	if ( 0 == strcmp( fname, "EMMXXXX0" ) )
		sftp->sft_flags = 0xC080;
	else
		sftp->sft_flags = 0x8000 | SFT_FDEVICE | SFT_FEOF | SFT_FSPECIAL | SFT_FCONOUT | SFT_FCONIN;	// 0xD3

	/* pad with spaces */
	for (i = FNAME_SIZE + FEXT_SIZE - 1; sftp->sft_name[i] == '\0'; i--)
		sftp->sft_name[i] = ' ';
	/* and uppercase */
	//DosUpFMem(sftp->sft_name, FNAME_SIZE + FEXT_SIZE);

	sftp->sft_dcb_or_dev = 1;	// dhp;
	//sftp->sft_date = dos_getdate();
	//sftp->sft_time = dos_gettime();
	sftp->sft_attrib = ATTRIB_DEVICE;
	sftp->sft_mode = O_RDWR;

	//if (sftp->sft_dev->dh_attr & SFT_FOCRM)
	//{
    /* if Open/Close/RM bit in driver's attribute is set
     * then issue an Open request to the driver
     */
		//struct dhdr FAR *dev = sftp->sft_dev;
		//if (BinaryCharIO(&dev, 0, MK_FP(0x0000, 0x0000), C_OPEN) != SUCCESS)
		//	return DE_ACCESS;
	//}
	return NO_ERROR;
}

extern int int10h();

int DoTeleType(int ch)
{
	int ax = REG_AX;
	int bx = REG_BX;
	int	rc;
	SET_AX(0x0E00 + ch); 						// AH = 0Eh, AL = character to write
	SET_BX((BIOSData[0x62]<<8) | 0x0F);		// BH = page number = currently active page, BL = color
	rc = int10h();								// VIDEO - DoTeleType OUTPUT
	REG_AX = ax;
	REG_BX = bx;
	return rc;
}

int DeviceWriteSft(SFT *s, int n, u8 *bp)
{
	int rc = 0, i;
	//iprintf("DeviceWriteSft: %d '%s'\n", n, bp);
	for ( i=0; i < n && 0 == rc; i++ )
	{
		rc = DoTeleType(*bp);
		bp++;
	}
	return n;
}

extern volatile char KeyboardDataByte;			// In ports.s
extern volatile char KeyboardStatusByte;		// In ports.s

int ReadCon(int size, char *data)
{
	int	count = 0;

	while (size > count)
	{
		if (KeyboardStatusByte & 1)		// Data in keyboard data byte
		{
			u8 key = KeyboardDataByte;
			KeyboardStatusByte &= 0xFE;	// Clear the status byte buffer full bit
			if ((key & 0x80) == 0)
			{
				// Key down event, translate the key
				char ch = ((u16*)(BIOS_F000+0x300))[((int)key)<<3];
				switch(ch)
				{
					case 0:
						break;
					case 13:
						data[count++]=0x0D;
						if (size>count) data[count++]=0x0A;
						DoTeleType(0x0D);
						DoTeleType(0x0A);
						return count;
					case 8:
						if (size>1 && count)
						{
							data[count--]=0;
							DoTeleType(8);
							DoTeleType(' ');
						}
						break;
					default:
						data[count++]=ch;
						DoTeleType(ch);
						break;
				}
			}
		}
	}
	return count;
}

extern int EMMDeviceControlChannel(u8 *ptr);

int DosDevIOctl(int al)
{
	if ( al > 0x11 )
		return ERROR_INVFUNCTION;

	switch ( al )
	{
		case 0x00:	// INT 21 - DOS 2+ - IOCTL - GET DEVICE INFORMATION
			// BX = handle
			// Return:CF clear if successful
			// DX = device information word (see #01423)
			// AX destroyed
			// CF set on error
			// AX = error code (01h,05h,06h) (see #01680 at AH=59h/BX=0000h)
			//
			// Notes: Value in DH corresponds to high byte of device driver's attribute word if handle refers to a character device.
			// Novell NetWare reportedly does not return a drive number in bits 5-0 for a disk file.
			// This function was not supported by Digital Research's DOS Plus 
			//
			// Bitfields for device information word:
			//
			// Bit(s)  Description     (Table 01423)
			// character device
			// 14    device driver can process IOCTL requests (see AX=4402h"DOS 2+")
			// 13    output until busy supported
			// 11    driver supports OPEN/CLOSE calls
			// 8    ??? (set by MS-DOS 6.2x KEYB)
			// 7    set (indicates device)
			// 6    EOF on input
			// 5    raw (binary) mode
			// 4    device is special (uses INT 29)
			// 3    clock device
			// 2    NUL device
			// 1    standard output
			// 0    standard input
			// disk file
			// 15    file is remote (DOS 3.0+)
			// 14    don't set file date/time on closing (DOS 3.0+)
			// 11    media not removable
			// 8    (DOS 4 only) generate INT 24 if no disk space on write or read past end of file
			// 7    clear (indicates file)
			// 6    file has not been written
			// 5-0   drive number (0 = A:)
			
			// See Also: AX=4401h - INT 2F/AX=122Bh 
			{
				SFT *sftp = idx_to_sft(get_sft_idx(BX));
				if ( NULL == sftp )
					return ERROR_INV_HANDLE;
				SET_DX((int)sftp->sft_flags);
				SET_AX((int)sftp->sft_flags);
				CLR_CF;			// Clear the Carry flag
			}
			return 0;
		case 0x01:	// DOS 2+ - IOCTL - SET DEVICE INFORMATION
			// BX = handle (must refer to character device)
			// DX = device information word (see #01423)
			// (DH must be zero for DOS version prior to 6.x)
			// Return:CF clear if successful
			// CF set on error
			// AX = error code (01h,05h,06h,0Dh) (see #01680 at AH=59h/BX=0000h)
			return ERROR_INVFUNCTION;
		case 0x02:	// Memory Managers - GET API ENTRY POINT	(Windows 3.00a in Standard Mode)
			// AX = 4402h subfn 00h
			// BX = file handle for device "EMMXXXX0"
			// CX = 0006h (size of buffer in bytes)
			// DS:DX -> buffer for API entry point record (see #01512)
			// first byte must be 00h on entry
			// Return:CF clear if successful
			// buffer filled (see #03603 at INT 67/AH=3Fh)
			// CF set on error
			// AX = error code (01h,05h,06h,0Dh) (see #01680 at AH=59h/BX=0000h)
			{
				SFT *sftp = idx_to_sft(get_sft_idx(BX));
				if ( NULL == sftp )
					return ERROR_INV_HANDLE;
				if (sftp->sft_flags == 0xC080)		// EMM device
				{
					int retval = EMMDeviceControlChannel((u8*)DS_DX_PTR);
					if (0 == retval)
						CLR_CF;
					return retval;
				}
			}
			return ERROR_INVFUNCTION;
		case 0x07:	// DOS 2+ - IOCTL - GET OUTPUT STATUS
			// BX = file handle
			// Return:	CF clear if successful
			// 			AL = input status
			// 				00h not ready
			// 				FFh ready
			// 			CF set on error
			// 			AX = error code (01h,05h,06h,0Dh) (see #01680 at AH=59h/BX=0000h)
			SET_AL(0xFF);
			CLR_CF;						// Clear the Carry flag
			return 0;
		case 0x08:	// DOS 3.0+ - IOCTL - CHECK IF BLOCK DEVICE REMOVABLE
			// BL = drive number (00h = default, 01h = A:, etc)
			// Return:	CF clear if successful
			// 				AX = media type (0000h removable, 0001h fixed)
			// 			CF set on error
			// 				AX = error code (01h,0Fh) (see #01680 at AH=59h/BX=0000h)
			if ( HaveCD && 4 == BL )
			{
				SET_AX(0);					// Removable
				CLR_CF;						// Clear the Carry flag
			}
			if ( BL != 0 && BL != 3)
				return ERROR_INV_DRIVE;	// Bad drive number
			else
			{
				SET_AX(1);					// Fixed
				CLR_CF;						// Clear the Carry flag
			}
			return 0;
		case 0x09:	// DOS 3.1+ - IOCTL - CHECK IF BLOCK DEVICE REMOTE
			// BL = drive number (00h = default, 01h = A:, etc)
			// Return:	CF clear if successful
			// 			DX = device attribute word
			// 				bit 15:Drive is SUBSTituted
			// 				bit 13:(DR DOS 3.41/5.0 local drives only) always set media ID needed
			// 				bit 12:Drive is remote
			// 				bit  9:Direct I/O not allowed.
			// 			CF set on error
			// 			AX = error code (01h,0Fh,15h) (see #01680 at AH=59h/BX=0000h)
			if ( BL != 0 && BL != 3 && !(HaveCD && 4 == BL))
				return ERROR_INV_DRIVE;	// Bad drive number
			else
			{
				SET_DX(0);
				CLR_CF;						// Clear the Carry flag
			}
			return 0;
		case 0x0A:	// DOS 3.1+ - IOCTL - CHECK IF HANDLE IS REMOTE
			// BX = handle
			// Return:CF clear if successful
			// DX = attribute word (as stored in SFT)
			// bit 15:Set if remote
			// bit 14:Date/time not set on close.
			// CF set on error
			// AX = error code (01h,06h) (see #01680 at AH=59h/BX=0000h)
			return ERROR_INVFUNCTION;
		case 0x0D:
			return -(0x16);
		case 0x0E:	// DOS 3.2+ - IOCTL - GET LOGICAL DRIVE MAP
			// BL = drive number (00h=default,01h=A:,etc)
			// Return:	CF set on error
			// 				AX = error code (01h,0Fh) (see #01680 at AH=59h/BX=0000h)
			// 			CF clear if successful
			// 				AL = 00h block device has only one logical drive assigned
			//					 1..26 the last letter used to reference the drive (1=A:,etc)
			if ( BL != 0 && BL != 3 && !(HaveCD && 4 == BL))
			{
				return ERROR_INV_DRIVE;	// Bad drive number
			}
			else
			{
				SET_AX(0x4400);
				CLR_CF;						// Clear the Carry flag
			}
			return 0;
		case 0x0F:	// DOS 3.2+ - IOCTL - SET LOGICAL DRIVE MAP
			// BL = physical drive number (00h=default,01h=A:,etc))
			// Return:
			//	CF set on error
			// 		AX = error code (01h,0Fh) (see #01680 at AH=59h/BX=0000h)
			// 	CF clear if successful
			return ERROR_INVFUNCTION;
		case 0x52:	// DR DOS 3.41+ - DETERMINE DOS TYPE/GET DR DOS VERSION
			return 0;
			
	}
	return 1;	// Unsupported!
}

