//=============================================================================
// XMS.c
//
// This file handles extended memory, and also allocates the full x86 RAM
// area.
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
#include <stdlib.h>
#include <time.h>

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

#if 1

#define	VERBOSE	0

#define XMS_HANDLES							50		/* 50 XMS Memory Blocks */ 
#define XMS_VERSION    						0x0300	/* version 3.00 */
#define XMS_DRIVER_VERSION					0x0301	/* my driver version 3.01 */

#define	XMS_FUNCTION_NOT_IMPLEMENTED		0x80
#define	HIGH_MEMORY_NOT_EXIST				0x90
#define XMS_OUT_OF_SPACE					0xa0
#define XMS_OUT_OF_HANDLES					0xa1
#define XMS_INVALID_HANDLE					0xa2
#define XMS_INVALID_SOURCE_HANDLE			0xa3
#define XMS_INVALID_SOURCE_OFFSET			0xa4
#define XMS_INVALID_DEST_HANDLE				0xa5
#define XMS_INVALID_DEST_OFFSET				0xa6
#define XMS_INVALID_LENGTH					0xa7
#define XMS_BLOCK_NOT_LOCKED				0xaa
#define XMS_BLOCK_LOCKED					0xab

extern void SetA20Gate(int enabled);
extern int QueryA20Gate();

static char *MemAlloced = NULL;
char *XMS_Start_Aligned = NULL;
int	XMS_Total = (0x100000*15/1024);
static int	XMS_Used = 0;

typedef struct XMS_Block {
	int		size;
	char 	*mem;
	u8		locked;
	bool	used;
} XMS_Block;

static XMS_Block xms_handles[XMS_HANDLES];

static int XMS_AllocateMemory(int size)
{	// size = kb
	/* Find free handle */
	int index=1;
	while (xms_handles[index].used)
	{
		if (++index>=XMS_HANDLES)
			return XMS_OUT_OF_HANDLES;
	}
	if (XMS_Total-XMS_Used < size)		// Note! Requested size can be == 0
		return XMS_OUT_OF_SPACE;
	xms_handles[index].used=true;
	xms_handles[index].mem=XMS_Start_Aligned+(XMS_Used*1024);
	xms_handles[index].locked=0;
	xms_handles[index].size=size;
	// 	DX = handle for memory block
	SET_DX(index);
	XMS_Used += size;
	return 0;
}

#define	InvalidHandle(handle) ((!handle || (handle>=XMS_HANDLES) || !xms_handles[handle].used))

static int XMS_FreeMemory(int handle)
{
	if (InvalidHandle(handle))
		return XMS_INVALID_HANDLE;
	if (xms_handles[handle].mem == XMS_Start_Aligned+((XMS_Used-xms_handles[handle].size)*1024))
	{
		// This is the last allocated block, so we can free it properly.
		XMS_Used -= xms_handles[handle].size;
	}
	xms_handles[handle].mem=(char *)-1;
	xms_handles[handle].size=0;
	xms_handles[handle].used=false;
	return 0;
}

#define	MEM_MOVE_LENGTH ((*ptr)|((*(ptr+1))<<8)|((*(ptr+2))<<16)|((*(ptr+3))<<24))
#define	MEM_MOVE_SRC_HANDLE ((*(ptr+4))|((*(ptr+5))<<8))
#define	MEM_MOVE_SRC ((*(ptr+6))|((*(ptr+7))<<8)|((*(ptr+8))<<16)|((*(ptr+9))<<24))
#define	MEM_MOVE_DEST_HANDLE ((*(ptr+10))|((*(ptr+11))<<8))
#define	MEM_MOVE_DEST ((*(ptr+12))|((*(ptr+13))<<8)|((*(ptr+14))<<16)|((*(ptr+15))<<24))

// Win 3.0: DS:SI=0323:0074, values=(00003F20,0000,07FA:0000,0003,00000000)
//
static int XMS_MoveMemory(u8* ptr)
{
	char *srcptr, *destptr;

	if (MEM_MOVE_SRC_HANDLE)
	{
		if (InvalidHandle(MEM_MOVE_SRC_HANDLE))
			return XMS_INVALID_SOURCE_HANDLE;
		if (MEM_MOVE_SRC >= (xms_handles[MEM_MOVE_SRC_HANDLE].size*1024U))
			return XMS_INVALID_SOURCE_OFFSET;
		if (MEM_MOVE_LENGTH > xms_handles[MEM_MOVE_SRC_HANDLE].size*1024 - MEM_MOVE_SRC)
			return XMS_INVALID_LENGTH;
		srcptr = xms_handles[MEM_MOVE_SRC_HANDLE].mem + MEM_MOVE_SRC;
	}
	else
		srcptr = (char *)PHYS_PTR((MEM_MOVE_SRC>>16)&0xFFFF,MEM_MOVE_SRC&0xFFFF);

	if (MEM_MOVE_DEST_HANDLE)
	{
		if (InvalidHandle(MEM_MOVE_DEST_HANDLE))
			return XMS_INVALID_DEST_HANDLE;
		if (MEM_MOVE_DEST >= (xms_handles[MEM_MOVE_DEST_HANDLE].size*1024U))
			return XMS_INVALID_DEST_OFFSET;
		if (MEM_MOVE_LENGTH > xms_handles[MEM_MOVE_DEST_HANDLE].size*1024 - MEM_MOVE_DEST)
			return XMS_INVALID_LENGTH;
		destptr = xms_handles[MEM_MOVE_DEST_HANDLE].mem + MEM_MOVE_DEST;
	}
	else
		destptr = (char *)PHYS_PTR((MEM_MOVE_DEST>>16)&0xFFFF,MEM_MOVE_DEST&0xFFFF);
	
	memcpy( destptr, srcptr, MEM_MOVE_LENGTH);
	return 0;
}

static int XMS_LockMemory(int handle, u32 *address)
{
	if (InvalidHandle(handle))
		return XMS_INVALID_HANDLE;
	if (xms_handles[handle].locked<255)
		xms_handles[handle].locked++;
	*address = (u32)xms_handles[handle].mem - (u32)XMS_Start_Aligned + 0x100000;
	return 0;
}

static int XMS_UnlockMemory(int handle)
{
 	if (InvalidHandle(handle))
		return XMS_INVALID_HANDLE;
	if (xms_handles[handle].locked)
	{
		xms_handles[handle].locked--;
		return 0;
	}
	return XMS_BLOCK_NOT_LOCKED;
}

static int XMS_ResizeMemory(int handle, int newsize)
{
 	if (InvalidHandle(handle))
		return XMS_INVALID_HANDLE;
	// Block has to be unlocked
	if (xms_handles[handle].locked)
		return XMS_BLOCK_LOCKED;
	if (xms_handles[handle].mem == XMS_Start_Aligned+((XMS_Used-xms_handles[handle].size)*1024))
	{
		// This is the last allocated block, so we can resize it easily.
		if (xms_handles[handle].size >= newsize)
		{
			// Shrink the memory block
			XMS_Used -= (xms_handles[handle].size - newsize);
			xms_handles[handle].size = newsize;
			return 0;
		}
		else
		{
			// Enlarge the memory block
			if ((newsize - xms_handles[handle].size) + XMS_Used > XMS_Total)
				return XMS_OUT_OF_SPACE;
			XMS_Used += (newsize - xms_handles[handle].size);
			xms_handles[handle].size = newsize;
			return 0;
		}
	}
	else if (xms_handles[handle].size >= newsize)
	{
		// Shrink the middle block.
		xms_handles[handle].size = newsize;
		return 0;
	}
	else
		// No room to grow a middle block.
		return XMS_OUT_OF_SPACE;
}

#endif

int XMS( int ah ) {
#if VERBOSE
	LOGI("XMS AH=%02X\n", ah );
#endif	
#if 1
	switch ( ah )
	{
		case 0:	// Call the XMS driver "Get XMS version number" function with:.
			// Return:
			//	AX = XMS version (in BCD, AH=major, AL=minor)
			// 	BX = internal revision number (in BCD for HIMEM.SYS)
			// 	DX = High Memory Area (HMA) state
			// 		0001h HMA (1M to 1M + 64K) exists
			// 		0000h HMA does not exist
			SET_AX(XMS_VERSION);
			SET_BX(XMS_DRIVER_VERSION);
			SET_DX(0);
			return 0;
		case 1:	// Call the XMS driver "Request High Memory Area" function with:.
			// DX = memory in bytes (for TSR or device drivers)
			// FFFFh if application program
			// Return:
			//	AX = status
			// 		0001h success
			// 		0000h failure
			// 	BL = error code (80h,81h,90h,91h,92h) (see #02775)
			SET_AX(0);
			SET_BL(HIGH_MEMORY_NOT_EXIST);
			return 0;
		case 3: // Call the XMS driver "Global enable A20, for using the HMA" function with:.
		case 5: // Call the XMS driver "Local enable A20" function with:.
			SetA20Gate(1);
			SET_AX(1);		// Success
			SET_BL(0);
			return 0;
		case 4:	// Call the XMS driver "Global disable A20" function with:.
		case 6:	// Call the XMS driver "Local disable A20" function with:.
			SetA20Gate(0);
			SET_AX(1);		// Success
			SET_BL(0);
			return 0;
		case 7:	// Call the XMS driver "Query A20 state" function with:.
			// Return:AX = status
			// 0001h enabled
			// 0000h disabled
			// BL = error code (00h,80h,81h)
			SET_AX(QueryA20Gate());
			SET_BL(0);
			return 0;
		case 0x88:	// Call the XMS v3.0 driver "Query free extended memory" function with:
			// Return:EAX = largest block of extended memory, in KB
			// BL = status (00h,80h,81h,A0h) (see #02775)
			// ECX = physical address of highest byte of memory (valid even on error codes 81h and A0h)
			// EDX = total Kbytes of extended memory (0 if status A0h)
			REG_AX = 0;				// Clear high halfword
			REG_DX = 0;				// Clear high halfword
			REG_CX = 0x00FFFFFF;	// Highest known physical address
		case 8:	// Call the XMS driver "Query free extended memory" function with:.
			// BL = 00h (some implementations leave BL unchanged on success)
			// Return:
			//	AX = size of largest extended memory block in KB
			// 	DX = total extended memory in KB
			// 	BL = error code (00h,80h,81h,A0h) (see #02775)
			SET_AX(XMS_Total-XMS_Used);
			SET_DX(XMS_Total);
			if (0 == AX)
				SET_BL(XMS_OUT_OF_SPACE);
			else
				SET_BL(0);
			return 0;
		case 0x89:	// Call the XMS driver "Allocate extended memory block" function with:.
			// EDX = Kbytes needed
			REG_DX &= 0xFFFF;
		case 9:	// Call the XMS driver "Allocate extended memory block" function with:.
			// DX = Kbytes needed
			// Return:
			//	AX = status
			// 		0001h success
			// 		0000h failure
			// 	DX = handle for memory block
			// 	BL = error code (80h,81h,A0h)
			SET_BL(XMS_AllocateMemory(DX));
			if (BL)
				SET_AX(0);		// Error!
			else
				SET_AX(1);		// Success
			return 0;
		case 0xA:	// Call the XMS driver "Free extended memory block" function with:.
			// DX = handle of block to free
			// Return:AX = status
			// 0001h success
			// 0000h failure
			// BL = error code (80h,81h,A2h,ABh)
			SET_BL(XMS_FreeMemory(DX));
			if (BL)
				SET_AX(0);		// Error!
			else
				SET_AX(1);		// Success
			return 0;
		case 0xB:	// Call the XMS driver "Move extended memory block" function with:.
			// DS:SI -> EMM structure (see #02776)
			// Return:AX = status
			// 0001h success
			// 0000h failure
			// BL = error code (80h-82h,A3h-A9h) (see #02775)
			//
			// Note: If either handle in the EMM structure is 0000h, the corresponding offset is considered to be an absolute segment:offset address in directly addressable memory 
			{
				int retval = XMS_MoveMemory((u8 *)DS_SI_PTR);
				if (retval)
				{
					SET_BL(retval);
					SET_AX(0);
				}
				else
					SET_AX(1);		// Success
			}
			return 0;
		case 0xC:	// Call the XMS driver "Lock extended memory block" function with:.
			// DX = handle of block to lock
			// Return:AX = status
			// 0001h success
			// DX:BX = 32-bit physical address of locked block
			// 0000h failure
			// BL = error code (80h,81h,A2h,ACh,ADh)
			{
				u32 address = 0;
				SET_BL(XMS_LockMemory(DX, &address));
				if (BL)
					SET_AX(0);				// Error!
				else
				{
					SET_DX(address>>16);
					SET_BX(address&0xFFFF);
					SET_AX(1);				// Success
				}
				return 0;
			}
		case 0xD:	// Call the XMS driver "Unlock extended memory block" function with:.
			// DX = handle of block to unlock
			// Return:AX = status
			// 0001h success
			// 0000h failure
			// BL = error code (80h,81h,A2h,AAh)
			SET_BL(XMS_UnlockMemory(DX));
			if (BL)
				SET_AX(0);				// Error!
			else
				SET_AX(1);				// Success
			return 0;
		case 0x0F:	// Call the XMS driver "Resize extended memory block" function with:.
			// DX = handle of block to resize
			// BX = new size of block in KB
			SET_BL(XMS_ResizeMemory(DX, BX));
			if (BL)
				SET_AX(0);				// Error!
			else
				SET_AX(1);				// Success
			return 0;
		case 0x10:	// UMB functions, not supported (used in Commander Keen 4)
		case 0x11:
		case 0x12:
			SET_BL(XMS_FUNCTION_NOT_IMPLEMENTED);
			SET_AX(0);
			return 0;

	}
#endif
	return -1;
}

char	*BIOS_F000;
int		*INTVectors;
char	*BIOSData;
char	*CGA_B800;
char	*EGAVGA_A000;
u8		*BIOS_C000;

int InitMemory()
{
	char *MemStart = 0;
	if (!MemAlloced)
	{
		MemAlloced = (char *)calloc(64+1024+XMS_Total+16+512, 1024);
		if (!MemAlloced)
		{
			LOGI("XMS Memory allocation failed!\n");
			return -1;
		}
		// Get a properly aligned allocated memory starting address
		if ( 0 == (((int)MemAlloced) & 0x3FFF) )
			MemStart = MemAlloced;
		else
			MemStart = (char *)(((int)MemAlloced+0x4000)&0xFFFFC000);
		// Setup the memory access variables
		BIOS_F000 = MemStart;	// Our allocated memory begins with the BIOS F000 segment (for easier wrapping)
		INTVectors = (int *)(MemStart + 0x10000);	// Next is the RAM area
		BIOSData = (char *)INTVectors + 0x400;		// Then the BIOS RAM area
		CGA_B800 = (char *)INTVectors + 0xB8000;	// Then the CGA display RAM
		BIOS_C000 = (u8 *)INTVectors + 0xC0000;		// Then the VGA BIOS
		XMS_Start_Aligned = (char *)INTVectors + 0x100000;	// Finally the XMS memory
		EGAVGA_A000 = (char *)XMS_Start_Aligned + (XMS_Total*1024);	// Last the EGA/VGA screen buffer area
	}
	return 0;
}

int ReleaseMemory()
{
	if (MemAlloced)
	{
		free(MemAlloced);
		MemAlloced = NULL;
	}
	return 0;
}

#ifdef RII_EX

// Return the worst-case size of the memory serialization data.
int mem_serialize_size()
{
	return 640*1024 + XMS_Total*1024 + sizeof(xms_handles) + 4 + 4;
}

// Serialize the memory into data + offset
int mem_serialize(u8 *data, int offset)
{
	int i;
	XMS_Block ser_handles[XMS_HANDLES];

	// First the low memory
	memcpy(data + offset, INTVectors, 640*1024);
	offset += 640*1024;
	// Then the XMS variables
	memcpy(data + offset, &XMS_Used, 4); offset += 4;
	i = QueryA20Gate();
	memcpy(data + offset, &i, 4); offset += 4;
	// Then the xms_handles, fixing the direct memory addresses
	memcpy(ser_handles, xms_handles, sizeof(xms_handles));
	for (i = 0; i < XMS_HANDLES; i++)
	{
		if (ser_handles[i].used)
			ser_handles[i].mem -= (int)XMS_Start_Aligned;
	}
	memcpy(data + offset, ser_handles, sizeof(xms_handles));
	offset += sizeof(xms_handles);
	// Finally the actual XMS memory allocated.
	if (XMS_Used)
	{
		memcpy(data + offset, XMS_Start_Aligned, XMS_Used*1024);
		offset += XMS_Used*1024;
	}
	return offset;
}

// Unserialize the memory from data + offset
int mem_unserialize(u8 *data, int offset)
{
	int i;
	XMS_Block ser_handles[XMS_HANDLES];

	// First allocate the memory, and setup the ROM areas.
	ReleaseMemory();
	if(InitMemory())
		return -1;
	InitBIOS(); // This initializes EMSPages and VGA to sane values.

	// Then copy the low memory.
	memcpy(INTVectors, data + offset, 640*1024);
	offset += 640*1024;
	// Then the XMS variables
	memcpy(&XMS_Used, data + offset, 4); offset += 4;
	memcpy(&i, data + offset, 4); offset += 4;
	SetA20Gate(i);
	// Then the xms_handles, fixing the direct memory addresses
	memcpy(ser_handles, data + offset, sizeof(xms_handles));
	offset += sizeof(xms_handles);
	for (i = 0; i < XMS_HANDLES; i++)
	{
		if (ser_handles[i].used)
			ser_handles[i].mem += (int)XMS_Start_Aligned;
	}
	memcpy(xms_handles, ser_handles, sizeof(xms_handles));
	// Finally the actual XMS memory allocated.
	if (XMS_Used)
	{
		memcpy(XMS_Start_Aligned, data + offset, XMS_Used*1024);
		offset += XMS_Used*1024;
	}
	return offset;
}

#endif
