//=============================================================================
// DOS_mem.c
//
// This file contains the DOS memory allocation routines.
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
#include "pados.h"

#define	DEBUG	0

#define LARGEST         0xFFFF
#define FIRST_FIT       0
#define BEST_FIT        1
#define LAST_FIT        2
#define FIRST_FIT_UO    0x40
#define BEST_FIT_UO     0x41
#define LAST_FIT_UO     0x42
#define FIRST_FIT_U     0x80
#define BEST_FIT_U      0x81
#define LAST_FIT_U      0x82
#define FIT_U_MASK      0xc0
#define FIT_MASK        0x3f

#define MCB_NORMAL      0x4d
#define MCB_LAST        0x5a

#define FREE_PSP        0

#define	FIRST_MCB (MCB *)(PHYS_PTR(*((u16 *)PHYS_PTR(0x0080,0x0024)),0))
#define	NEW_MCB(p, size) (MCB *)(PHYS_PTR(LOG_SEG(p)+size+1, 0))
#define	NEXT_MCB(p) NEW_MCB(p, p->m_size)

#define	VALID_MCB(p) (p->m_size != 0xFFFF && (MCB_NORMAL == p->m_type || MCB_LAST == p->m_type))
#define	FREE_MCB(p) (FREE_PSP == p->m_psp)

/*
 * Join any following unused MCBs to MCB 'p'.
 *  Return:
 *  SUCCESS: on success
 *  else: error number <<currently DE_MCBDESTRY only>>
 */
static int joinMCBs(u16 para)
{
	MCB *p = (MCB *)PHYS_PTR(para, 0);
	MCB *q;

	/* loop as long as the current MCB is not the last one in the chain and the next MCB is unused */
	while (MCB_NORMAL == p->m_type)
	{
		q = NEXT_MCB(p);
		if ( !FREE_MCB(q) )
			break;
		if ( ! VALID_MCB(q) )
			return ERROR_BAD_MCB;
		/* join both MCBs */
		p->m_type = q->m_type;      /* possibly the next MCB is the last one */
		p->m_size += q->m_size + 1; /* one for q's MCB itself */
		q->m_size = 0xffff;		/* mark the now unlinked MCB as "fake" */
	}
	return NO_ERROR;
}

int DosMemAlloc(u16 size, u16 mode, u16 *para, u16 *asize)
{
	MCB	*p, *biggest, *found;

	p = FIRST_MCB;
	biggest = found = NULL;

	while(1)
	{
		if ( ! VALID_MCB(p) )
		{
			return ERROR_BAD_MCB;
		}
		if ( FREE_MCB(p) )
		{
			if ( joinMCBs(LOG_SEG((int)p)) != NO_ERROR )
				return ERROR_BAD_MCB;
			
			if ( NULL == biggest || biggest->m_size < p->m_size )
				biggest = p;
				
			if ( p->m_size >= size )
			{
				// Found a block that is big enough.
				// Check for allocation strategy.
				switch (mode)
				{
					case LAST_FIT:       /* search for last possible */
					case LAST_FIT_U:
					case LAST_FIT_UO:
					default:
						found = p;
						break;
					case LARGEST:        /* grab the biggest block */
						/* it is calculated when the MCB chain is completely checked */
						break;
					case BEST_FIT:       /* first, but smallest block */
					case BEST_FIT_U:
					case BEST_FIT_UO:
						if (!found || found->m_size > p->m_size)
							/* better match found */
							found = p;
						break;
					case FIRST_FIT:      /* first possible */
					case FIRST_FIT_U:
					case FIRST_FIT_UO:
						found = p;
						goto found_mcb;	/* OK, rest of chain can be ignored */
				}
			}
		}
		
	    if (MCB_LAST == p->m_type)
			break;

		p = NEXT_MCB(p);
	}

	if (LARGEST == mode && biggest && biggest->m_size >= size)
		*asize = (found = biggest)->m_size;

	if (NULL == found || 0 == found->m_size)
	{
		// No suitable block found!
		if (asize)
			*asize = (biggest != NULL) ? biggest->m_size : 0;
		return ERROR_OUT_OF_MEM;
	}
	
found_mcb:

	if ( mode != LARGEST && size != found->m_size )
	{
		if ( (LAST_FIT == mode) || (LAST_FIT_UO == mode) || (LAST_FIT_U == mode))
		{
			/* allocate the block from the end of the found block */
			p = found;
			p->m_size -= size + 1;    /* size+1 paragraphes are allocated by
                                   the new segment (+1 for MCB itself) */
			found = NEXT_MCB(p);

			/* initialize stuff because found > p */
			found->m_type = p->m_type;
			p->m_type = MCB_NORMAL;
		}
		else
		{
			/* all other modes allocate from the beginning */
			p = NEW_MCB(found, size);
			/* initialize stuff because p > found  */
			p->m_type = found->m_type;
			p->m_size = found->m_size - size - 1;
			found->m_type = MCB_NORMAL;
		}
		p->m_psp = FREE_PSP;
		found->m_size = size;
	}
	found->m_psp = dos_sda.cu_psp;
	found->m_name[0] = 0;
	*para = LOG_SEG(found);
	return NO_ERROR;
}

int DosMemLargest(u16 *size)
{
	u16 dummy;
	*size = 0;
	DosMemAlloc(LARGEST, LARGEST, &dummy, size);
	return *size ? NO_ERROR : ERROR_OUT_OF_MEM;
}

int DosMemFree(u16 seg)
{
	MCB *p;

	if (!seg)
		return ERROR_BAD_MCB;

	p = (MCB *)PHYS_PTR(seg, 0);

	if ( !(MCB_NORMAL == p->m_type || MCB_LAST == p->m_type) )
		return ERROR_BAD_MCB;
  
	p->m_psp = FREE_PSP;
	memset( p->m_name, 0, 8);

	return NO_ERROR;
}

int DosMemChange(u16 para, u16 size, u16 *maxSize)
{
	MCB	*p, *q;
	
	p = (MCB *)PHYS_PTR(para-1, 0);
	
	if ( ! VALID_MCB(p) )
		return ERROR_BAD_MCB;

	if ( size > p->m_size )
	{
		// Try to grow the block.

		if (joinMCBs(LOG_SEG((int)p)) != NO_ERROR)
			return ERROR_BAD_MCB;
		
		if (size > p->m_size)
		{
			if ( maxSize )
				*maxSize = p->m_size;
			return ERROR_OUT_OF_MEM;
		}
	}
	
	if ( size < p->m_size )
	{
		q = NEW_MCB(p, size);

		q->m_size = p->m_size - size - 1;
		q->m_type = p->m_type;

		p->m_size = size;
		p->m_type = MCB_NORMAL;

		q->m_psp = FREE_PSP;
		//memset(q->m_name, 0, 8);	Will overwrite INT22 return vector for Alone in the Dark!

		if (joinMCBs(LOG_SEG((int)q)) != NO_ERROR)
			return ERROR_BAD_MCB;
	}

	p->m_psp = dos_sda.cu_psp;

	return NO_ERROR;
}

const PSP PSP_Init = {
	0x20CD,		// u16	ps_exit;                /* 00 CP/M-like exit point: int 20 */
	0x9FFF,		// u16	ps_size;                /* 02 segment of first byte beyond memory allocated to program  */
	0,			// u8	ps_fill1;                /* 04 single char fill=0           */
	0x9A,		// u8	ps_farcall;             /* 05  far call opcode             */
	0xF01DFEF0,	// u32	*ps_reentry;			/* 06  re-entry point          */
	0xF0000000,	// u32	ps_isv22,              	/* 0a terminate address            */
    0xF0000000,	//    	ps_isv23,              	/* 0e ctrl-break address           */
    0xF0000000,	//    	ps_isv24;              	/* 12 critical error address       */
	0x0060,		// u16	ps_parent;              /* 16 parent psp segment           */
	{1,1,1,0xFF,	// u8	ps_files[20];           /* 18 file table - 0xff is unused  */
	0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF},
	0x0068,		// u16	ps_environ;             /* 2c environment paragraph        */
	0x100749F1,	// u32	ps_stack;           	/* 2e user stack pointer - int 21  */
	20,			// u16	ps_maxfiles;            /* 32 maximum open files           */
	0x00600018,	// u32	ps_filetab;        		/* 34 open file table pointer      */
	0xFFFFFFFF,	// u32	ps_prevpsp;         	/* 38 previous psp pointer         */
	0,			// u8	ps_fill2;               /* 3c unused                       */
	0,			// u8	ps_truename;            /* 3d [unused] append truename flag int2f/B711h */
	{0, 0},		// u8	ps_netx_taskid[2];      /* 3e [Novell only field] task id  */
	0,			// u16	ps_retdosver;           /* 40 [unused] version to return on int21/30h */
	0,			// u16	pdb_next;               /* 42 [Win only field] PSP chain   */
	{0,0,0,0},	// u8	ps_fill2b[4];           /* 44 unused, 4 bytes              */
	0,			// u8	ps_olddos;              /* 48 [Win only field] DOS/Win program */
	{0,0,0,0,0,0,0},	// u8	ps_fill2c[7];           /* 49 unused, 7 bytes              */
	{0xcd,0x21,0xcb}	// u8	ps_unix[3];             /* 50 unix style call - 0xcd 0x21 0xcb */
};



void Dos_InitMem()
{
	MCB	*p;
	u16 *tmp = (u16 *)PHYS_PTR(0x0080,0x0024);
	*tmp = 0x100;		// Put the first MCB into 0100:0000
	
	p = FIRST_MCB;
	
	p->m_type = MCB_LAST;
	p->m_psp = FREE_PSP;
	p->m_size = 0x9FFF-0x100-1;
	
	DosMemAlloc((sizeof(SFTTBL)>>4)+1, 0, tmp, NULL);		// Allocate memory for DOS SystemFileTable
	
	p->m_psp = 0x0008;					// Mark the first memory block to be owned by DOS internals
	memcpy( p->m_name, "DOS", 3);
	
	memcpy( PHYS_PTR(DOS_PSP, 0), &PSP_Init, sizeof(PSP) );
	*(u8 *)PHYS_PTR(DOS_PSP, 0x81) = 0x0D;
}

int FreeProcessMem(u16 ps)
{
	MCB *p;
	MCB	*lfmcb = NULL;	// Possible LOADFIX mcb
	
	//BYTE oldumbstate = uppermem_link & 1;

	//DosUmbLink(1);
//iprintf("FreeProcessMem, ps=%04X\n", ps );

	for (p = FIRST_MCB; ; p = NEXT_MCB(p))
	{

		if ( ! VALID_MCB(p) )
			return ERROR_BAD_MCB;

		if ( 0x40 == p->m_psp )		// Remember the LOADFIX mcb
			lfmcb = p;
		else if (p->m_psp == ps)		// If the block is owned by the process, free it
		{
			// If there is a LOADFIX mcb right before the process MCB, free it as well.
			if ( lfmcb != NULL && NEXT_MCB(lfmcb) == p )
			{
				DosMemFree(LOG_SEG(lfmcb));
				lfmcb = NULL;
			}
			DosMemFree(LOG_SEG(p));
		}

		if (p->m_type == MCB_LAST)
			break;
	}

	//DosUmbLink(oldumbstate);

	return NO_ERROR;
}
