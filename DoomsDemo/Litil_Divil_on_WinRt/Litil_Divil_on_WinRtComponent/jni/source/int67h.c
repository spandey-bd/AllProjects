//=============================================================================
// int67h.c
//
// This file contains the Expanded Memory Manager routines.
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

#define	VERBOSE	0

#define EMM_PAGEFRAME	0xE000
#define	EMM_MAX_HANDLES	32				/* 255 Max */
#define EMM_PAGE_SIZE	(16*1024U)
#define EMM_MAX_PAGES	256				/* 256 = 4MB of EMS */
#define EMM_MAX_PHYS	4				/* 4 16kb pages in pageframe */

#define EMM_VERSION		0x40

#define NULL_HANDLE	0xffff
#define	NULL_PAGE	0xffff

/* EMM errors */
#define EMM_NO_ERROR			0x00
#define EMM_SOFT_MAL			0x80
#define EMM_HARD_MAL			0x81
#define EMM_INVALID_HANDLE		0x83
#define EMM_FUNC_NOSUP			0x84
#define EMM_OUT_OF_HANDLES		0x85
#define EMM_SAVEMAP_ERROR		0x86
#define EMM_OUT_OF_PHYS			0x87
#define EMM_OUT_OF_LOG			0x88
#define EMM_ZERO_PAGES			0x89
#define EMM_LOG_OUT_RANGE		0x8a
#define EMM_ILL_PHYS			0x8b
#define EMM_PAGE_MAP_SAVED		0x8d
#define EMM_NO_SAVED_PAGE_MAP	0x8e
#define EMM_INVALID_SUB			0x8f
#define EMM_FEAT_NOSUP			0x91
#define EMM_MOVE_OVLAP			0x92
#define EMM_MOVE_OVLAPI			0x97
#define	EMM_NO_ALT_SET			0x9C
#define EMM_NOT_FOUND			0xa0

#define	EMM_Mapping				int
#define	emm_handle(P1)			(P1>>16)
#define emm_page(P1)			(P1&0xFFFF)

static int 	EMM_Total;
static char *EMM_Start_Alloced = NULL;
char *EMM_Start_Aligned = NULL;

typedef struct EMM_Handle {
	int			pages;
	u8 			page_arr[EMM_MAX_PAGES];
	char 		name[8];
	//bool 		saved_page_map;
	EMM_Mapping page_map[EMM_MAX_PHYS];
} EMM_Handle;

static EMM_Handle emm_handles[EMM_MAX_HANDLES];
static EMM_Mapping emm_mappings[EMM_MAX_PHYS] = {(NULL_HANDLE<<16)|NULL_PAGE,(NULL_HANDLE<<16)|NULL_PAGE,(NULL_HANDLE<<16)|NULL_PAGE,(NULL_HANDLE<<16)|NULL_PAGE};
static u8 emm_used_map[EMM_MAX_PAGES];
static int emm_save_area_ptr = 0;	// Note! Low halfword = segment, high halfword = offset

#define	VALIDHANDLE	( handle < EMM_MAX_HANDLES && NULL_PAGE != emm_handles[handle].pages )

extern u8 *BIOS_C000;

typedef struct PACKED {
	u8		ftype;
	u8		owner;
	u16		logpage;
	u8		physpage;
	u8		flags;
} GEMMIS_FRAME;

typedef struct PACKED {
	u16		flags;
	u16		size;
	u16		version;
	u8		reserved[4];
	GEMMIS_FRAME frames[64];
	u8		unknown;
	u8		UMBframes;
	u8		EMSframes;
	u8		EMShandle[2];
	u8		EMSname[8];
	u8		EMStype[2];
	u8		EMSphysaddr[4];
} GEMMIS;

int EMMDeviceControlChannel(u8 *ptr)
{
	u8 subf = ptr[0];
	switch ( subf )
	{
		case 0:	// Memory Managers - GET API ENTRY POINT
			if (CX != 6)
				return ERROR_INVFUNCTION;
			memset(ptr, 0, 6);
			ptr[0] = 0x23;
			SET_AX(6);
			return 0;
		case 1:	// Memory Managers - GET EMM IMPORT STRUCTURE ADDRESS
			if (CX != 6)
				return ERROR_INVFUNCTION;
			// Address of the structure = C000:2000
			{
				int i;
				GEMMIS *gem = (GEMMIS *)(BIOS_C000 + 0x2000);
				memset(ptr, 0, 6);
				ptr[1] = 0x20; ptr[2] = 0x0C;		// Physical address = 000C2000
				ptr[4] = 1;							// Version 1.0
				memset(BIOS_C000 + 0x2000, 0, sizeof(GEMMIS));
				gem->flags = 4;
				gem->size = sizeof(GEMMIS);
				gem->version = 1;
				for (i = 0; i < 64; i++)
				{
					gem->frames[i].ftype = (i>=56 && i<60) ? 3 : 0;
					gem->frames[i].owner = 0xFF;
					gem->frames[i].logpage = (i>=56 && i<60) ? 0x7FFF : 0xFFFF;
					gem->frames[i].physpage = (i>=56 && i<60) ? i-56 : 0xFF;
					gem->frames[i].flags = (i>=56 && i<60) ? 0 : 0xAA;
				}
				gem->unknown = 0x74;
				gem->UMBframes = 0;
				gem->EMSframes = 1;
				gem->EMStype[0] = 0x10;			// 0x0010 = System handle
				gem->EMSphysaddr[2] = 0x10;		// 0x00100000
				SET_AX(6);
			}
			return 0;
		case 2:
			return ERROR_INVFUNCTION;
	}
	return ERROR_INVFUNCTION;
}

static int EMM_FreePages()
{
	int	retval = 0, i;
	for ( i=0; i < EMM_Total; i++ )
		if ( 0 == emm_used_map[i] )
				retval++;
	return retval;
}

static bool EMM_AllocPages(int handle, int oldpg, int newpg)
{
	int	idx = oldpg, i;
	if ( newpg < oldpg )
	{
		// Decrease allocation
		for ( i=newpg; i < oldpg; i++ )
			emm_used_map[emm_handles[handle].page_arr[i]] = 0;
		return true;
	}
	// Increase allocation
	for ( i=0; i < EMM_Total && idx < newpg; i++ )
	{
		if ( 0 == emm_used_map[i] )
		{
			emm_handles[handle].page_arr[idx++] = i;
			emm_used_map[i] = 1;
		}
	}
	return ( idx == newpg );	// Return false if not enough free pages
}

// Try to allocate pages, put handle in DX if successfull.
// Return status (to be put into AH register).
static int EMM_AllocateMemory( int pages, bool zerook )
{
	int	handle = 1;
//printf("Allocate %d pages\n", pages );
	if ( 0 == pages && !zerook )
		return EMM_ZERO_PAGES;

	if ( EMM_FreePages() < pages )
		return EMM_OUT_OF_LOG;
		

	while ( emm_handles[handle].pages != NULL_HANDLE)
	{
		if (++handle >= EMM_MAX_HANDLES)
		{
//printf("Out of handles!\n");
			return EMM_OUT_OF_HANDLES;
		}
	}

	if ( ! EMM_AllocPages(handle, 0, pages) )
		return EMM_OUT_OF_LOG;	// Should never happen, checked above
		
	emm_handles[handle].pages = pages;
	SET_DX(handle);
	return EMM_NO_ERROR;
}

static int EMM_ReallocatePages(int handle, int pages)
{
//printf("Reallocate %d, %d\n", handle, pages );
	if ( !VALIDHANDLE )
		return EMM_INVALID_HANDLE;

	if ( ! EMM_AllocPages(handle, emm_handles[handle].pages, pages) )
		return EMM_OUT_OF_LOG;
	emm_handles[handle].pages = pages;
	return EMM_NO_ERROR;
}

static int EMM_ReleaseMemory(int handle)
{
	if ( !VALIDHANDLE )
		return EMM_INVALID_HANDLE;

	// should check for saved_page_map flag here, returning an error if it's true
	// as apps are required to restore the pagemap beforehand; to be checked
//	if (emm_handles[handle].saved_page_map) return EMM_SAVEMAP_ERROR;

//printf("Release %d (%d pages)\n", handle, emm_handles[handle].pages );

	EMM_AllocPages(handle, emm_handles[handle].pages, 0);

	if ( 0 == handle ) 
		emm_handles[handle].pages = 0;	// OS handle is NEVER deallocated
	else
		emm_handles[handle].pages = NULL_HANDLE;
	//emm_handles[handle].saved_page_map=false;
	memset(&emm_handles[handle].name,0,8);
	return EMM_NO_ERROR;
}

static void domap(int handle, int phys_page, int log_page)
{
	emm_mappings[phys_page] = (handle<<16)|log_page;
	if ( ((NULL_HANDLE<<16)|NULL_PAGE) == emm_mappings[phys_page] )
#if EMSPAGESHIFT == 12
		EMSPages[0xE0+(phys_page<<2)] = EMSPages[0xE1+(phys_page<<2)] = 
		EMSPages[0xE2+(phys_page<<2)] = EMSPages[0xE3+(phys_page<<2)] = ((u32)INTVectors)>>4;
#else
		EMSPages[(0xE0>>2)+phys_page] = ((u32)INTVectors)>>4;
#endif		
	else
	{
#if EMSPAGESHIFT == 12	
		EMSPages[0xE0+(phys_page<<2)] = EMSPages[0xE1+(phys_page<<2)] = 
		EMSPages[0xE2+(phys_page<<2)] = EMSPages[0xE3+(phys_page<<2)] =	(int)(EMM_Start_Aligned + 0x4000*(int)(emm_handles[handle].page_arr[log_page]))-(0xE0000+0x4000*phys_page);
#else
		EMSPages[(0xE0>>2)+phys_page] = ((u32)(EMM_Start_Aligned + 0x4000*(u32)(emm_handles[handle].page_arr[log_page]))-(0xE0000+0x4000*phys_page))>>4;
#endif		
#if 0
		if ( 0 == phys_page )
			EMSPages[(0xE0>>2)-1] = EMSPages[(0xE0>>2)] - 0x4000;	// Alone in the Dark uses segment DFFF to access EMS memory!
#endif			
	}
}

static int EMM_MapPage(int phys_page, int handle, int log_page)
{
	if ( phys_page >= EMM_MAX_PHYS || phys_page < 0 )
		return EMM_ILL_PHYS;
				
	if ( NULL_PAGE == log_page )
	{
		// unmap a page
		domap(NULL_HANDLE, phys_page, NULL_PAGE);
		return EMM_NO_ERROR;
	}

	if ( !VALIDHANDLE )
		return EMM_INVALID_HANDLE;

	if ( log_page < emm_handles[handle].pages)
	{
		// Map a page
		domap(handle, phys_page, log_page);
		return EMM_NO_ERROR;
	}
	else
	{
//		if ( 0 == phys_page )
//			printf("handle=%d, pages=%d, log=%d\n", handle, emm_handles[handle].pages, log_page );
		return EMM_LOG_OUT_RANGE;
	}
}

//Format of EMS copy data:
//
//Offset  Size    Description     (Table 03653)
// 00h    DWORD   region length in bytes
// 04h    BYTE    source memory type
// 					00h conventional
// 					01h expanded
// 05h    WORD    source handle (0000h if conventional memory)
// 07h    WORD    source initial offset (within page if EMS, segment if convent)
// 09h    WORD    source initial segment (conv mem) or logical page (EMS)
// 0Bh    BYTE    destination memory type
// 					00h conventional
// 					01h expanded
// 0Ch    WORD    destination handle
// 0Eh    WORD    destination initial offset
// 10h    WORD    destination initial segment or page

typedef struct PACKED {
	u8 		bytes0;
	u8 		bytes1;
	u8 		bytes2;
	u8 		bytes3;
	u8		src_type;
	u16 	src_handle;
	u16 	src_offset;
	u16		src_page_seg;
	u8		dest_type;
	u16		dest_handle;
	u16		dest_offset;
	u16		dest_page_seg;
} MOVEREGION;

static int EMM_Move( int mode, MOVEREGION *info )
{
	char	*from, *to;

	if ( mode > 1 )
		return EMM_FUNC_NOSUP;

	if ( 0 == info->src_type )
		from = (char *)PHYS_PTR(info->src_page_seg, info->src_offset);
	else
	{
		if ( ! (info->src_handle < EMM_MAX_HANDLES && NULL_PAGE != emm_handles[info->src_handle].pages) )
			return EMM_INVALID_HANDLE;
		from = EMM_Start_Aligned + 0x4000*(int)(emm_handles[info->src_handle].page_arr[info->src_page_seg]) + info->src_offset;
	}
	if ( 0 == info->dest_type )
		to = (char *)PHYS_PTR(info->dest_page_seg, info->dest_offset);
	else
	{
		if ( ! (info->dest_handle < EMM_MAX_HANDLES && NULL_PAGE != emm_handles[info->dest_handle].pages) )
			return EMM_INVALID_HANDLE;
		to = EMM_Start_Aligned + 0x4000*(int)(emm_handles[info->dest_handle].page_arr[info->dest_page_seg]) + info->dest_offset;
	}	
	memmove( to, from, info->bytes0 | (info->bytes1<<8) | (info->bytes2<<16) | (info->bytes3<<24));	// Fix Aladdin unaligned crash
	return EMM_NO_ERROR;
}

//-----------------------------
// LIM EMS Handler
//-----------------------------
int int67h( int ah ) {
	int i;
#if VERBOSE
	iprintf("int 67h AH=%02X\n", AH );
#endif	
	switch ( ah )
	{
		case 0x40:	// LIM EMS - GET MANAGER STATUS
			// Return:
			//	AH = status (00h,80h,81h,84h) (see #03648)
			SET_AH(0);		// Return AH = 00 == Succesfull
			return 0;
		case 0x41:	// LIM EMS - GET PAGE FRAME SEGMENT
			// Return:
			//	AH = status (see also AH=40h)
			// 		00h function successful
			// 	BX = segment of page frame
			SET_AH(0);		// Return AH = 00 == Succesfull
			SET_BX(EMM_PAGEFRAME);		// Page frame segment = 0xE000
			return 0;
		case 0x42:	// LIM EMS - GET NUMBER OF PAGES
			// Return:
			//	AH = status (see also AH=40h)
			// 		00h function successful
			// 	BX = number of unallocated pages
			// 	DX = total number of pages
			//
			// BUG: DOS 6.0 EMM386.EXE causes a system lock-up or reboot if in AUTO mode when this call is made;
			// use AH=46h to ensure that EMM386 is ON before making this call 
			SET_AH(0);		// Return AH = 00 == Succesfull
			SET_BX(EMM_FreePages());
			SET_DX(EMM_Total);
			return 0;
		case 0x43:	// LIM EMS - GET HANDLE AND ALLOCATE MEMORY
			// BX = number of logical pages to allocate
			// Return:
			//	AH = status (00h,80h,81h,84h,85h,87h,88h,89h) (see #03648)
			// 	DX = handle if AH=00h
			SET_AH(EMM_AllocateMemory(BX, false));
//iprintf("handle=%d, pages=%d\n", DX, emm_handles[DX].pages );
			return 0;
		case 0x44:	// LIM EMS - MAP MEMORY
			// AL = physical page number (0-3)
			// BX = logical page number
			// 		or FFFFh to unmap (QEMM)
			// DX = handle
			// Return:AH = status (00h,80h,81h,83h,84h,8Ah,8Bh) (see #03648)
			SET_AH(EMM_MapPage( AL, DX, BX ));
			return 0;
		case 0x45:	// LIM EMS - RELEASE HANDLE AND MEMORY
			// AH = 45h
			// DX = EMM handle
			// Return:
			//	AH = status (00h,80h,81h,83h,84h,86h) (see #03648)
			SET_AH(EMM_ReleaseMemory( DX ));
			return 0;
		case 0x46:	// LIM EMS - GET EMM VERSION
			// Return:
			//	AH = status (00h,80h,81h,84h) (see #03648)
			// 	AL = EMM version number if AH=00h
			SET_AX(EMM_VERSION);		// Return AL = 40 = Version 4.0 (BCD format)
			return 0;
		case 0x47:
			// LIM EMS - SAVE MAPPING CONTEXT
			// AH = 47h
			// DX = handle
			// Return:AH = status (00h,80h,81h,83h,84h,8Ch-8Eh) (see #03648)
			{
				int	handle = DX;

				if ( handle >= EMM_MAX_HANDLES || NULL_PAGE == emm_handles[handle].pages )
				{
					SET_AH(EMM_INVALID_HANDLE);
					return 0;
				}

				/* Check for previous save */
				//if (emm_handles[handle].saved_page_map) return EMM_PAGE_MAP_SAVED;
				
				for (i=0;i<EMM_MAX_PHYS;i++)
					emm_handles[handle].page_map[i] = emm_mappings[i];
				//emm_handles[handle].saved_page_map=true;
				SET_AH(0);		// Return AH = 00 == Succesfull
				return 0;
			}
		case 0x48:
			// LIM EMS - RESTORE MAPPING CONTEXT
			// AH = 48h
			// DX = handle
			// Return:AH = status (00h,80h,81h,83h,84h,8Eh) (see #03648)
			{
				int	handle = DX;

				if ( !VALIDHANDLE )
				{
					SET_AH(EMM_INVALID_HANDLE);
					return 0;
				}

				/* Check for previous save */
				//if (!emm_handles[handle].saved_page_map) return EMM_NO_SAVED_PAGE_MAP;
				
				for (i=0;i<EMM_MAX_PHYS;i++)
				{
					domap(emm_handles[handle].page_map[i]>>16, i, emm_handles[handle].page_map[i]&0xFFFF);
				}

				SET_AH(0);		// Return AH = 00 == Succesfull
				return 0;
			}
		case 0x4B:	// LIM EMS - GET NUMBER OF EMM HANDLES
			// Return:
			//	AH = status (00h,80h,81h,83h,84h) (see #03648)
			// 	BX = number of EMM handles if AH=00h
			SET_BX(EMM_MAX_HANDLES);
			SET_AH(0);		// Return AH = 00 == Succesfull
			return 0;
		case 0x4C:	// LIM EMS - GET PAGES OWNED BY HANDLE
			// DX = EMM handle
			// Return:
			//	AH = status (see #02785)
			// 	BX = number of logical pages if AH=00h
			{
				int	handle = DX;
				if ( handle >= EMM_MAX_HANDLES || NULL_PAGE == emm_handles[handle].pages )
				{
					SET_AH(EMM_INVALID_HANDLE);
					return 0;
				}
				SET_BX(emm_handles[handle].pages);
				SET_AH(0);		// Return AH = 00 == Succesfull
				return 0;
			}
		case 0x4D:	// LIM EMS - GET PAGES FOR ALL HANDLES
			// ES:DI -> array to receive information
			// Return:
			//	AH = status (00h,80h,81h,84h) (see #03648)
			// 	BX = number of active EMM handles
			// 	array filled with 2-word entries, consisting of a handle and the
			// 	number of pages allocated to that handle
			{
				u8 *ptr = (u8 *)ES_DI_PTR;
				SET_BX(0);
				for (i=0;i<EMM_MAX_HANDLES;i++)
				{
					if (emm_handles[i].pages!=NULL_HANDLE)
					{
						SET_BX(BX+1);
						*ptr++ = i;
						*ptr++ = 0;
						*ptr++ = emm_handles[i].pages;
						*ptr++ = emm_handles[i].pages>>8;
					}
				}
				SET_AH(0);		// Return AH = 00 == Succesfull
				return 0;
			}
		case 0x4E:	// LIM EMS - GET OR SET PAGE MAP
			// AL = subfunction
			// 	00h get mapping registers
			// 	01h set mapping registers
			// 	02h get and set mapping registers at once
			// 	03h get size of page-mapping array
			// DS:SI -> array holding information (AL=01h/02h)
			// ES:DI -> array to receive information (AL=00h/02h)
			// Return:
			//	AH = status (00h,80h,81h,84h,8Fh,A3h) (see also AH=40h)
			// 		00h successful
			// 	AL = bytes in page-mapping array (AL=03h only)
			// 	array pointed to by ES:DI receives mapping info (AL=00h/02h)
			//
			// Notes: This function was designed to be used by multitasking operating systems
			//	and should not ordinarily be used by appplication software..
			//	MD386 returns the size of the page-mapping array in AX instead of AL 
			switch ( AL )
			{
				case 0x00:	// 	00h get mapping registers
					memcpy( ES_DI_PTR, emm_mappings, sizeof(emm_mappings));
					SET_AH(0);		// Return AH = 00 == Succesfull
					return 0;
				case 0x02:	// 	02h get and set mapping registers
					memcpy( ES_DI_PTR, emm_mappings, sizeof(emm_mappings));
				case 0x01:	// 	01h/02h set mapping registers
					memcpy( emm_mappings, DS_SI_PTR, sizeof(emm_mappings));
					for (i=0;i<EMM_MAX_PHYS;i++)
					{
						domap((emm_mappings[i]>>16)&0xFFFF, i, emm_mappings[i]&0xFFFF);
					}
					SET_AH(0);		// Return AH = 00 == Succesfull
					return 0;
				case 0x03:	// 	03h get size of page-mapping array
					SET_AX(sizeof(emm_mappings));
					return 0;
			}
			break;
		case 0x50:	// LIM EMS 4.0 - MAP/UNMAP MULTIPLE HANDLE PAGES
			// AL = subfunction
			// 	00h use physical page numbers
			// 	01h use segment addresses
			// DX = handle
			// CX = number of entries in array
			// DS:SI -> mapping array (see #03649)
			// Return:
			//	AH = status (00h,80h,81h,83h,84h,8Ah,8Bh,8Fh) (see #03648)
			//
			// Format of EMS mapping array entry:
			//
			// Offset  Size    Description     (Table 03649)
			// 00h    WORD    logical page number or FFFFh to unmap physical page
			// 02h    WORD    physical page number or segment address
			//
			SET_AH(0);	// Default to no error
			switch ( AL )
			{
				case 0:	// use physical page numbers
					{
						u8 *data = (u8 *)DS_SI_PTR;
						for (i=0; i<CX; i++, data+=4)
						{
							SET_AH(EMM_MapPage( data[2]|(data[3]<<8), DX, data[0]|(data[1]<<8) ));
							if (AH)
								break;
						}
					}
					break;
				case 1: // use segment address 
					{
						u8 *data = (u8 *)DS_SI_PTR;
						for (i=0; i<CX; i++, data+=4)
						{
							SET_AH(EMM_MapPage( ((int)(data[2]|(data[3]<<8))-EMM_PAGEFRAME)>>10, DX, data[0]|(data[1]<<8) ));
							if (AH)
								break;
						}
					}
					break;
				default:
					SET_AH(EMM_INVALID_HANDLE);
					break;
			}
			return 0;
		case 0x51:	// LIM EMS 4.0 - REALLOCATE PAGES
			// DX = handle
			// BX = number of pages to be allocated to handle
			// Return:
			//	AH = status (00h,80h,81h,83h,84h,87h,88h) (see #03650)
			//	BX = actual number of pages allocated to handle
			SET_AH(EMM_ReallocatePages( DX, BX ));
			return 0;
		case 0x53:
			// LIM EMS 4.0 - GET/SET HANDLE NAME
			// AH = 53h
			// AL = subfunction
			// DX = handle
			// Return:AH = status (00h,80h,81h,83h,84h,8Fh,A1h) (see #03648)
			if ( AL )
			{
				// 	01h set handle name
				//		DS:SI -> 8-byte handle name
				int	handle = DX;

				if ( handle >= EMM_MAX_HANDLES )
				{
					SET_AH(EMM_INVALID_HANDLE);
					return 0;
				}
				memcpy( emm_handles[handle].name, DS_SI_PTR, 8 );
#if VERBOSE				
				iprintf("Handle name='%s'\n", emm_handles[handle].name );
#endif				
			}
			else
			{
				//	00h get handle name
				//		ES:DI -> 8-byte buffer for handle name
				int	handle = DX;

				if ( handle >= EMM_MAX_HANDLES )
				{
					SET_AH(EMM_INVALID_HANDLE);
					return 0;
				}
				memcpy( ES_DI_PTR, emm_handles[handle].name, 8 );
			}
			SET_AH(0);		// Return AH = 00 == Succesfull
			return 0;
		case 0x54:	// LIM EMS 4.0 - GET HANDLE DIRECTORY
			// AL = subfunction
			// 	00h get handle directory
			// 		ES:DI -> buffer for handle directory (see #03651)
			// 	01h search for named handle
			// 		DS:SI -> 8-byte name
			// 	02h get total number of handles
			// 	Return:
			//		AL = number of entries in handle directory (subfunction 00h)
			// 		DX = value of named handle (subfunction 01h)
			// 		BX = total number of handles (subfunction 02h)
			// 		AH = status (00h,80h,81h,84h,8Fh,A0h,A1h) (see also #03650)
			// 			A1h a handle found had no name
			//
			// Format of EMS handle directory entry:
			//
			// Offset  Size    Description     (Table 03651)
			// 00h    WORD    handle
			// 02h  8 BYTEs   handle's name
			switch ( AL )
			{
				case 0x01:	//	01h search for named handle
					for (i=0; i < EMM_MAX_HANDLES; i++)
					{
						if (emm_handles[i].pages != NULL_HANDLE && 0 == memcmp(DS_SI_PTR, emm_handles[i].name, 8))
						{
							SET_DX(i);
							SET_AH(0);		// Return AH = 00 == Succesfull
							return 0;
						}
					}
					SET_AH(EMM_NOT_FOUND);
					return 0;
				case 0x02:	// 	02h get total number of handles
					SET_BX(EMM_MAX_HANDLES);
					SET_AH(0);		// Return AH = 00 == Succesfull
					return 0;
			}
			break;
		case 0x57:	// LIM EMS 4.0 - MOVE/EXCHANGE MEMORY REGION
			// AL = subfunction
			// 	00h move memory region
			// 	01h exchange memory region
			// DS:SI -> structure describing source and destination (see #03653)
			// Return:AH = status (see #03652)
			if ( 1 == AL )
				return -1;
			SET_AH(EMM_Move( AL, (MOVEREGION *)DS_SI_PTR ));
			return 0;
		case 0x58:	// LIM EMS 4.0 - GET MAPPABLE PHYSICAL ADDRESS ARRAY
			// AL = subfunction
			// 	00h get mappable physical address array
			// 		ES:DI -> buffer to be filled with array (see #03654)
			// 	01h get number of entries in m.p.a. array
			// Return:
			//	CX = number of entries in array
			// 	AH = status (00h,80h,81h,84h,8Fh) (see #03652)
			//
			// Note: The returned array for subfunction 00h is filled in physical segment address order.
			// Format of EMS mappable physical address entry:
			//
			// Offset  Size    Description     (Table 03654)
			// 00h    WORD    physical page segment
			// 02h    WORD    physical page number
			if ( 0 == AL )
			{
				u8 *ptr = (u8 *)ES_DI_PTR;
				ptr[0] = 0; ptr[1] = EMM_PAGEFRAME>>8;
				ptr[2] = 0; ptr[3] = 0;
				ptr[4] = 0; ptr[5] = (EMM_PAGEFRAME+0x400)>>8;
				ptr[6] = 1; ptr[7] = 0;
				ptr[8] = 0; ptr[9] = (EMM_PAGEFRAME+0x800)>>8;
				ptr[10] = 2; ptr[11] = 0;
				ptr[12] = 0; ptr[13] = (EMM_PAGEFRAME+0xC00)>>8;
				ptr[14] = 3; ptr[15] = 0;
			}
			SET_CX(EMM_MAX_PHYS);
			SET_AH(0);		// Return AH = 00 == Succesfull
			return 0;
		case 0x59:	// LIM EMS 4.0 - GET EXPANDED MEMORY HARDWARE INFORMATION
			// AL = subfunction
			//	00h get hardware configuration array
			//		ES:DI -> buffer to be filled with array (see #03655)
			//	01h get unallocated raw page count
			//		Return:	BX = unallocated raw pages
			//				DX = total raw pages
			// Return:AH = status (see also AH=58h"EMS 4.0")
			// A4h access denied by operating system
			//
			// Note: Subfunction 00h is for use by operating systems only, and can be enabled or disabled at any time by the operating system 
			//
			// Format of EMS hardware configuration array:
			//
			// Offset  Size    Description     (Table 03655)
			// 00h    WORD    size of raw EMM pages in paragraphs
			// 02h    WORD    number of alternate register sets
			// 04h    WORD    size of mapping-context save area in bytes
			// 06h    WORD    number of register sets assignable to DMA
			// 08h    WORD    DMA operation type
			//					0000h DMA with alternate register sets
			//					0001h only one DMA register set
/*
			if ( 0 == AL )			//	00h get hardware configuration array (Windows 3.00a)
			{
				u16 *ptr = (u16 *)ES_DI_PTR;
				ptr[0] = 0x400;		// size of raw EMM pages in paragraphs
				ptr[1] = 0;			// number of alternate register sets
				ptr[2] = sizeof(EMM_Mapping)*EMM_MAX_PHYS;
				ptr[3] = 0;			// number of register sets assignable to DMA
				ptr[4] = 0;			// DMA operation type
				SET_AH(0);
				return 0;
			}
			else if ( 1 == AL )		//	01h get unallocated raw page count
			{
				SET_BX(EMM_FreePages());
				SET_DX(EMM_Total);
				SET_AH(0);
				return 0;
			}
*/			
			SET_AH(EMM_FUNC_NOSUP);
			return 0;
		case 0x5A:	// LIM EMS 4.0 - ALLOCATE STANDARD/RAW PAGES
			// AL = subfunction
			// 	00h allocate standard pages
			// 	01h allocate raw pages
			// BX = number of pages to allocate
			// Return:
			//	DX = handle
			// 	AH = status (00h,80h,81h,84h,85h,87h,88h,8Fh) (see #03648)
			SET_AH(EMM_AllocateMemory( BX, true ));
			return 0;
		case 0x5B:	// LIM EMS 4.0 - ALTERNATE MAP REGISTER SET	(Windows 3.00a)
			// AL = subfunction
			// 	00h get alternate map register set
			// 		Return:
			//			BL = current active alternate map register set number
			// 			ES:DI -> map register context save area if BL=00h
			// 	01h set alternate map register set
			// 		BL = new alternate map register set number
			// 		ES:DI -> map register context save area if BL=0
			// 	02h get alternate map save array size
			// 		Return:
			//			DX = array size in bytes
			// 	03h allocate alternate map register set
			// 		Return:
			//			BL = number of map register set; 00h = not supported
			// 	04h deallocate alternate map register set
			// 			BL = number of alternate map register set
			// Return:AH = status (00h,80h,81h,84h,8Fh,9Ah-9Dh,A3h,A4h) (see #03656)
			//
			// Note: This function is for use by operating systems only, and can be enabled or disabled at any time by the operating system 
			//
			switch ( AL )
			{
				case 0:		// 00h get alternate map register set
					SET_BL(0);	// BL = 0, we don't support alternate register sets
					REG_ES = emm_save_area_ptr&0xFFFF;
					SET_DI(emm_save_area_ptr>>16);
					if ( REG_ES != 0 )
						memcpy( ES_DI_PTR, emm_mappings, EMM_MAX_PHYS*sizeof(EMM_Mapping));
					SET_AH(0);
					return 0;
				case 1:		// 	01h set alternate map register set
					if ( 0 == BL )
					{
						// If BL==0, use the pointer in ES:DI
						emm_save_area_ptr = REG_ES|(DI<<16);
						if ( emm_save_area_ptr != 0 )
						{
							memcpy( emm_mappings, ES_DI_PTR, EMM_MAX_PHYS*sizeof(EMM_Mapping));
							for (i=0;i<EMM_MAX_PHYS;i++)
							{
								domap((emm_mappings[i]>>16)&0xFFFF, i, emm_mappings[i]&0xFFFF);
							}
						}
						SET_AH(0);		// Return AH = 00 == Succesfull
						return 0;
					}
					else
					{
						// Report that we don't support alternate register sets
						SET_AH(EMM_NO_ALT_SET);
						return -1;
					}
				case 2:		// 	02h get alternate map save array size	(Windows 3.00a)
					SET_DX(EMM_MAX_PHYS*sizeof(EMM_Mapping));
					SET_AH(0);
					return 0;
				case 3:		// 	03h allocate alternate map register set
					SET_BL(0);	// BL = 0, we don't support alternate register sets
				case 4:		// 	04h deallocate alternate map register set
					SET_AH(0);
					return 0;
			}
			SET_AH(EMM_FUNC_NOSUP);
			break;
		case 0xDE:	// Virtual Control Program Interface - INSTALLATION CHECK
		case 0xFF:	// Microsoft EMM386.EXE v4.20+ - INSTALLATION CHECK
			return 0;
	}
	//iprintf("Unsupported int 67h AH=%02X\n", AH );
	return -1;
}

void InitEMS()
{
	int i;
	if ( EMM_Start_Alloced )	// In case of a restart...
		free( EMM_Start_Alloced );
	memset( emm_handles, 0, sizeof(emm_handles) );
	memset( emm_used_map, 0, sizeof(emm_used_map));
	for (i=0; i < EMM_MAX_HANDLES; i++ )
		emm_handles[i].pages = NULL_PAGE;
	for (i=0; i < EMM_MAX_PHYS; i++ )
		emm_mappings[i] = (NULL_HANDLE<<16)|NULL_PAGE;
	EMM_Total = EMM_MAX_PAGES+1;	// Try to allocate full 4MB of EMS
	do {
		EMM_Total--;
		EMM_Start_Alloced = (char *)calloc( EMM_Total+1, 0x4000 );
	} while (NULL == EMM_Start_Alloced);
	if ( 0 == (((int)EMM_Start_Alloced) & 0x3FFF) )
		EMM_Start_Aligned = EMM_Start_Alloced;
	else
		EMM_Start_Aligned = (char *)(((int)EMM_Start_Alloced+0x4000)&0xFFFFC000);
	emm_save_area_ptr = 0;		
	emm_handles[0].pages = 0;	// Allocate system handle, for Windows 3.00a
}

#ifdef RII_EX

// Return the worst-case size of the EMS memory serialization data.
int ems_serialize_size()
{
	return sizeof(emm_handles) + sizeof(emm_mappings) + sizeof(emm_used_map) + 
		   sizeof(emm_save_area_ptr) + (EMM_MAX_PAGES * 0x4000);
}

// Serialize the EMS memory into data + offset
int ems_serialize(u8 *data, int offset)
{
	int i;
	// First copy the EMS data structures.
	memcpy(data + offset, emm_handles, sizeof(emm_handles));
	offset += sizeof(emm_handles);
	memcpy(data + offset, emm_mappings, sizeof(emm_mappings));
	offset += sizeof(emm_mappings);
	memcpy(data + offset, emm_used_map, sizeof(emm_used_map));
	offset += sizeof(emm_used_map);
	memcpy(data + offset, &emm_save_area_ptr, sizeof(emm_save_area_ptr));
	offset += sizeof(emm_save_area_ptr);
	// Then copy the contents of the EMS pages that have been allocated.
	for (i = 0; i < EMM_MAX_PAGES; i++)
	{
		if (emm_used_map[i])
		{
			memcpy(data + offset, EMM_Start_Aligned + 0x4000*i, 0x4000);
			offset += 0x4000;
		}
	}
	return offset;
}

// Unserialize the EMS memory from data + offset
int ems_unserialize(u8 *data, int offset)
{
	int i;
	// Allocate the EMS memory if it has not been allocated yet.
	if ( ! EMM_Start_Alloced )
		InitEMS();
	// First copy the EMS data structures.
	memcpy(emm_handles, data + offset, sizeof(emm_handles));
	offset += sizeof(emm_handles);
	memcpy(emm_mappings, data + offset, sizeof(emm_mappings));
	offset += sizeof(emm_mappings);
	memcpy(emm_used_map, data + offset, sizeof(emm_used_map));
	offset += sizeof(emm_used_map);
	memcpy(&emm_save_area_ptr, data + offset, sizeof(emm_save_area_ptr));
	offset += sizeof(emm_save_area_ptr);
	// Then copy the contents of the EMS pages that have been allocated.
	for (i = 0; i < EMM_MAX_PAGES; i++)
	{
		if (emm_used_map[i])
		{
			memcpy(EMM_Start_Aligned + 0x4000*i, data + offset, 0x4000);
			offset += 0x4000;
		}
	}
	// Then map the pages currently mapped to the EMS page frame.
	// This assumes that EMSPages[] has already been unserialized!
	for (i=0;i<EMM_MAX_PHYS;i++)
	{
		domap((emm_mappings[i]>>16)&0xFFFF, i, emm_mappings[i]&0xFFFF);
	}
	return offset;
}

#endif

