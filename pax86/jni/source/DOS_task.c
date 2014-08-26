//=============================================================================
// DOS_task.c
//
// This file contains the DOS task routines (loading programs and returning
// to the caller programs and eventually to the command shell.
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
//#include "config.h"

#define	DEBUG	0

#ifdef WP8

#define printf retappend
extern void retappend(const char *format, ...);

#endif

//u16		cu_psp = DOS_PSP;								// Current process PSP

extern char PSP_Init[];									// Initial values for the PSP table
extern char CurrentEXE[];
extern int UseConfig( const char *name );
#ifdef RPi
extern void exit_program();
#endif


u32 getvec(unsigned char intno)
{
	u32 iv;
	iv = *(u32 *)PHYS_PTR(0,4 * (intno));
	return iv;
}

void setvec(unsigned char intno, u32 vector)
{
	*(u32 *)PHYS_PTR(0,4 * intno) = vector;
}

void new_psp(u16 para, u16 cur_psp)
{
	PSP *p = (PSP *)PHYS_PTR(para, 0);

#if DEBUG
	LOGI("new_psp, PSP %04X=>%04X\n", cur_psp, para);
#endif

	memcpy(p, PHYS_PTR(cur_psp, 0), sizeof(PSP));

	/* terminate address                                    */
	p->ps_isv22 = getvec(0x22);
	/* break address                                        */
	p->ps_isv23 = getvec(0x23);
	/* critical error address                               */
	p->ps_isv24 = getvec(0x24);
	/* parent psp segment set to 0 (see RBIL int21/ah=26)   */
	p->ps_parent = 0;
}

void child_psp(u16 para, u16 cur_psp, int psize)
{
	int i;
	PSP *p = (PSP *)PHYS_PTR(para, 0);
	PSP *q = (PSP *)PHYS_PTR(cur_psp, 0);
	u8 	*filetab = (u8 *)PHYS_PTR((q->ps_filetab>>16),(q->ps_filetab&0xFFFF));

	new_psp(para, cur_psp);

	/* Now for parent-child relationships                   */
	/* parent psp segment                                   */
	p->ps_parent = dos_sda.cu_psp;
	/* previous psp pointer                                 */
	p->ps_prevpsp = cur_psp<<16;

#if DEBUG
	LOGI("child_psp: p->ps_parent=%04X\n", p->ps_parent);
#endif
	/* Environment and memory useage parameters             */
	/* memory size in paragraphs                            */
	p->ps_size = psize;

	/* File System parameters                               */
	/* maximum open files                                   */
	p->ps_maxfiles = 20;
	memset(p->ps_files, 0xff, 20);

	/* open file table pointer, points to ps_files          */
	p->ps_filetab = para<<16 | 0x18;

	/* clone the file table -- 0xff is unused               */
	for (i = 0; i < 20; i++)
		if (CloneHandle(i) >= 0)
			p->ps_files[i] = filetab[i];

	/* first command line argument                          */
	p->ps_fcb1.fcb_drive = 0;
	memset(p->ps_fcb1.fcb_fname, ' ', FNAME_SIZE + FEXT_SIZE);
	/* second command line argument                         */
	p->ps_fcb2.fcb_drive = 0;
	memset(p->ps_fcb2.fcb_fname, ' ', FNAME_SIZE + FEXT_SIZE);

	/* local command line                                   */
	p->ps_cmd.ctCount = 0;
	p->ps_cmd.ctBuffer[0] = 0xd; /* command tail            */
}

static u16 patchPSP(u16 pspseg, u16 envseg, EXEC_BLK *exb, char *fnam)
{
	PSP 	*psp;
	MCB		*pspmcb;
	int 	i;
	char	*np;

	pspmcb = (MCB *)PHYS_PTR(pspseg, 0);
	++pspseg;
	psp = (PSP *)PHYS_PTR(pspseg, 0);

#if DEBUG
	LOGI("patchPSP, PSP=%04X (%08X)\n", pspseg, (int)psp);
	LOGI("cmd_line = %08X\n", (int)(exb->exec.cmd_line));
	LOGI("envseg=%04X\n", envseg );
#endif

	/* complete the psp by adding the command line and FCBs     */
	memcpy(&psp->ps_cmd, PHYS_PTR(exb->exec.cmd_line>>16, exb->exec.cmd_line&0xFFFF), sizeof(COMMANDTAIL));

	if ((exb->exec.fcb_1 & 0xFFFF) != 0xFFFF)
	{
		memcpy(&psp->ps_fcb1, PHYS_PTR(exb->exec.fcb_1>>16, exb->exec.fcb_1&0xFFFF), 16);
		memcpy(&psp->ps_fcb2, PHYS_PTR(exb->exec.fcb_2>>16, exb->exec.fcb_2&0xFFFF), 16);
	}

	/* identify the mcb as this functions'                  */
	pspmcb->m_psp = pspseg;
	/* Patch in environment segment, if present, also adjust its MCB */
	if (envseg)
	{
		((MCB *)PHYS_PTR(envseg, 0))->m_psp = pspseg;
		envseg++;
	}
	psp->ps_environ = envseg;
#if DEBUG
	LOGI("psp->ps_environ=%04X\n", psp->ps_environ );
#endif

	/* use the file name less extension - left adjusted */
	np = fnam;
	for (;;)
	{
		switch (*fnam++)
		{
			case '\0':
				goto set_name;
			case ':':
			case '/':
			case '\\':
				np = fnam;
		}
	}
set_name:
	for (i = 0; i < 8 && np[i] != '.' && np[i] != '\0'; i++)
	{
		pspmcb->m_name[i] = (np[i] >= 'a' && np[i] <= 'z') ? np[i] - 32 : np[i];
	}
	for (;i < 8; i++)
		pspmcb->m_name[i] = 0;
	memcpy( CurrentEXE, pspmcb->m_name, 8 );
	/* return value: AX value to be passed based on FCB values */
	//return (get_cds1(psp->ps_fcb1.fcb_drive) ? 0 : 0xff) | (get_cds1(psp->ps_fcb2.fcb_drive) ? 0 : 0xff00);
	return 0;
}


#define MAXENV 32768
#define ENV_KEEPFREE 83         /* keep unallocated by environment variables */

static int ChildEnv(EXEC_BLK *exp, u16 *pChildEnvSeg, char *pathname)
{
	u8		*pSrc;
	char	*pDest;
	u16 	nEnvSize;
	int		RetCode;

	PSP *ppsp = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);

	/* copy parent's environment if exec.env_seg == 0       */

	pSrc = exp->exec.env_seg ? PHYS_PTR(exp->exec.env_seg, 0) : PHYS_PTR(ppsp->ps_environ, 0);

#if DEBUG
	LOGI("env_seg=%04X, pSrc='%s'\n", exp->exec.env_seg, pSrc);
#endif

	nEnvSize = 1;
	
	if (pSrc)
	{
		for (nEnvSize = 0;; nEnvSize++)
		{
			//if (nEnvSize >= MAXENV - ENV_KEEPFREE)
			//	return DE_INVLDENV;

			if (0 == pSrc[nEnvSize] && 0 == pSrc[nEnvSize+1])
				break;
		}
		nEnvSize += 2;              /* space for the trailing 0,0 */
	}

	if ((RetCode = DosMemAlloc((nEnvSize + ENV_KEEPFREE + 15)>>4, dos_sda.mem_access_mode, pChildEnvSeg, NULL )) < 0)
		return RetCode;
		
	pDest = (char *)PHYS_PTR(*pChildEnvSeg + 1, 0);

	if (pSrc)
	{
		memcpy(pDest, pSrc, nEnvSize);
		pDest += nEnvSize;
	}
	else
		*pDest++ = '\0';            /* create an empty environment */

	/* initialize 'extra strings' count */
	*pDest++ = 1;
	*pDest++ = 0;

	/* copy complete pathname */
	if ((RetCode = truename(pathname, PriPathName, 0)) < NO_ERROR)
		return RetCode;
	strcpy(pDest, PriPathName);

	return NO_ERROR;
}

static int ExecMemLargest(u16 *asize, u16 threshold)
{
  int rc;
  rc = DosMemLargest(asize);
  return (*asize < threshold ? ERROR_OUT_OF_MEM : rc);
}

static int ExecMemAlloc(u16 size, u16 *para, u16 *asize)
{
	/* We can still get an error on first fit if the above  */
	/* returned size was a best fit case                    */
	/* ModeLoadHigh = 80 = try high, then low               */
	int rc = DosMemAlloc(size, 0, para, asize);

	if (rc != NO_ERROR)
	{
/*  
    if (rc == DE_NOMEM)
    {
      rc = DosMemAlloc(0, LARGEST, para, asize);
      if ((mem_access_mode & 0x80) && (rc != SUCCESS))
      {
        mem_access_mode &= ~0x80;
        rc = DosMemAlloc(0, LARGEST, para, asize);
        mem_access_mode |= 0x80;
      }
    }
*/	
	}
	else
	{
		/* with no error, we got exactly what we asked for      */
		*asize = size;
	}

	/* This should never happen, but ... */
	if (rc == NO_ERROR && *asize < size)
	{
		DosMemFree(*para);
		return ERROR_OUT_OF_MEM;
	}
	return rc;
}

static int load_transfer(u16 ds, EXEC_BLK *exp, u16 fcbcode, u16 mode)
{
	PSP *p = (PSP *)PHYS_PTR(ds, 0);
	PSP *q = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	u8 *regsave;
	
	/* Transfer control to the executable                   */
	p->ps_parent = dos_sda.cu_psp;
	p->ps_prevpsp = dos_sda.cu_psp<<16;
#if DEBUG
	LOGI("load_transfer: p->ps_parent=%04X\n", p->ps_parent);
#endif
	
#if 1
	// DOSBox-style register saving!

	if ((REG_SS<<16)|SP)	// == 0 if we are starting the root shell process
	{
		// Push user registers
		REG_SP -= 0x0018;
		q->ps_stack = (REG_SS<<16)|SP;
		regsave = (u8 *)PHYS_PTR(REG_SS, SP);
		regsave[0] = REG_AX; regsave[1] = REG_AX>>8;
		regsave[2] = REG_CX; regsave[3] = REG_CX>>8;
		regsave[4] = REG_DX; regsave[5] = REG_DX>>8;
		regsave[6] = REG_BX; regsave[7] = REG_BX>>8;
		regsave[8] = REG_BP; regsave[9] = REG_BP>>8;
		regsave[10] = REG_SI; regsave[11] = REG_SI>>8;
		regsave[12] = REG_DI; regsave[13] = REG_DI>>8;
		regsave[14] = REG_DS; regsave[15] = REG_DS>>8;
		regsave[16] = REG_ES; regsave[17] = REG_ES>>8;
		// Emulate the INT 21 SP storage
		regsave[18] = (REG_EIP - (int)PHYS_PTR(REG_CS, 0)); regsave[19] = (REG_EIP - (int)PHYS_PTR(REG_CS, 0))>>8;
		regsave[20] = REG_CS; regsave[21] = REG_CS>>8;
		regsave[22] = REG_FLAGS; regsave[23] = REG_FLAGS>>8;
	}
#else
	// FreeDOS-style register saving!

	q->ps_stack = (REG_SS<<16)|SP;

	if ( q->ps_stack )	// == 0 if we are starting the root shell process
	{
		// Push user registers. NOTE! SP might not be halfword-aligned!
		regsave = (u8 *)PHYS_PTR(REG_SS, SP);
		regsave[-1] = AX>>8; regsave[-2] = AX;
		regsave[-3] = CX>>8; regsave[-4] = CX;
		regsave[-5] = DX>>8; regsave[-6] = DX;
		regsave[-7] = BX>>8; regsave[-8] = BX;
		regsave[-9] = BP>>8; regsave[-10] = BP;
		regsave[-11] = SI>>8; regsave[-12] = SI;
		regsave[-13] = DI>>8; regsave[-14] = DI;
		regsave[-15] = REG_DS>>8; regsave[-16] = REG_DS;
		regsave[-17] = REG_ES>>8; regsave[-18] = REG_ES;
	}
#endif

	dos_sda.cu_psp = ds;
	dos_sda.dta = (ds<<16) + 0x80;
  
#if DEBUG
	LOGI("stack=%08X, start=%08X\n", exp->exec.stack, exp->exec.start_addr);
#endif  
	if (mode == EXECUTE)
	{
		// Registers and flags at the start of a COM program execution
		//AX=0000  CX=28C0  DX=0000  BX=0000  SP=FFFE  BP=0000  SI=0000  DI=0000
		//DS=15F4  ES=15F4  SS=15F4  CS=15F4  IP=0100   NV UP EI PL NZ NA PO NC
		REG_SP = exp->exec.stack&0xFFFF;								// SP initial value
		REG_SS = (exp->exec.stack>>16)&0xFFFF;
		REG_DS = REG_ES = ds;											// Points to PSP
		REG_DX = REG_DS;
		REG_CS = (exp->exec.start_addr>>16)&0xFFFF;
		REG_EIP = (int)PHYS_PTR(REG_CS, exp->exec.start_addr&0xFFFF);
		REG_DI = exp->exec.stack&0xFFFF;
		REG_BP = 0x91E;
		REG_AX = REG_BX = fcbcode&0xFFFF;
		REG_CX = 0xFF;													// CX initial value
		// Windows 3.11: Do not modify VM and IOPL flags!
		// Interrupts enabled at the beginning
		REG_FLAGS=((REG_FLAGS&(~(FLAG_CF|FLAG_PF|FLAG_AF|FLAG_ZF|FLAG_SF|FLAG_OF)))|FLAG_IF);

		UseConfig((char *)p-8);	// Switch to the configuration of this just starting program

		// if (InDOS)
		// --InDOS;
	}
	else
	{
		/* mode == LOAD */
		//exp->exec.stack -= 2;
		//*((UWORD FAR *)(exp->exec.stack)) = fcbcode;
	}
	REG_FLAGS &= ~FLAG_CF;												// Clear the Carry flag
	return NO_ERROR;
}

int DosComLoader(char *namep, EXEC_BLK * exp, u16 mode, u16 fd)
{
	u16 mem;
	u16 env, asize = 0;
    u16 com_size;
    u16 fcbcode;
    u8 *sp;

    {
		int com_size_long = SftGetFSize(fd);
		if ( com_size_long < 0 )
			return com_size_long;
		/* maximally 64k - 256 bytes stack - 256 bytes psp */
		com_size = ((u16)(com_size_long > 0xFE00 ? 0xFE00 : com_size_long) >> 4) + 0x10;
    }

    if ((mode & 0x7f) != OVERLAY)
    {
		int rc;
      
		if ( dos_sda.ExecFlags & EXEC_LOADFIX )
		{
			u16 loadfix;
			MCB	*lfmcb;
			DosMemAlloc(0x1000, 0, &loadfix, NULL);
			lfmcb = (MCB *)PHYS_PTR(loadfix, 0);
			strcpy( lfmcb->m_name, "LOADFIX" );
			lfmcb->m_psp = 0x40;	// Marker for LOADFIX
			dos_sda.ExecFlags &= ~EXEC_LOADFIX;
		}

		rc = ChildEnv(exp, &env, namep);
      
		if (rc == NO_ERROR)
			rc = ExecMemLargest(&asize, com_size);
      
		if (rc == NO_ERROR)
			rc = ExecMemAlloc(asize, &mem, &asize);
		
		if (rc != NO_ERROR)
			DosMemFree(env);

		if (rc != NO_ERROR)
			return rc;

		++mem;
    }
    else
		mem = exp->load.load_seg;

#if DEBUG
	LOGI("DosComLoader. Loading '%s' at %04x\n", namep, mem);
#endif

    if (mode == OVERLAY)
		sp = PHYS_PTR(mem, 0);
    else
		sp = PHYS_PTR(mem, sizeof(PSP));

    /* rewind to start */
    SftSeek(fd, 0, 0);
    /* read everything, but at most 64K - sizeof(PSP)             */
    DosReadSft(fd, 0xff00, sp);
    DosCloseSft(fd, 0);

	if (mode == OVERLAY)
		return NO_ERROR;
  
    //PSP *p;

	setvec(0x22, (REG_CS<<16) | ((int)REG_EIP - (int)PHYS_PTR(REG_CS, 0)));
    child_psp(mem, dos_sda.cu_psp, mem + asize);

    fcbcode = patchPSP(mem - 1, env, exp, namep);
    /* set asize to end of segment */
#if DEBUG
	LOGI("asize=%d\n", asize);
#endif
    if (asize > 0x1000)
      asize = 0x1000;
    if (asize < 0x11)
      return ERROR_OUT_OF_MEM;
    asize -= 0x11;
    //p = (PSP *)PHYS_PTR(mem, 0);
    //p->ps_reentry = MK_FP(0xc - asize, asize << 4);		TODO!
    asize <<= 4;
    asize += 0x10e;
    exp->exec.stack = (mem<<16)|asize;
    exp->exec.start_addr = (mem<<16)|0x100;
    //*((UWORD FAR *) MK_FP(mem, asize)) = (UWORD) 0;
#if DEBUG
	LOGI("exp stack=%08X, start=%08X\n", exp->exec.stack, exp->exec.start_addr);
#endif
    load_transfer(mem, exp, fcbcode, mode);
	
	return NO_ERROR;
}

static EXE_HEADER	ExeHeader;
static EXEC_BLK		TempExeBlock;

int DosExeLoader(char *namep, EXEC_BLK *exp, u16 mode, u16 fd)
{
	u16 mem, env, start_seg, asize = 0;
	u16 RelocTbl[32768];
	u16 exe_size;
    u16 fcbcode;
	int i, bytes;
	u8 *phys;
	int	val;
	
	{
		u16 image_size;

		image_size = (ExeHeader.exPages << 5) - ExeHeader.exHeaderSize;

		if ((mode & 0x7f) != OVERLAY)
		{
			int rc;
		  
			image_size += (sizeof(PSP) >> 4);
			exe_size = image_size + ExeHeader.exMinAlloc;
		  
			// Emulate LOADFIX behaviour if so requested.
			if ( (dos_sda.ExecFlags & EXEC_LOADFIX) )
			{
				u16 loadfix;
				MCB	*lfmcb;
				DosMemAlloc(0x1000, 0, &loadfix, NULL);
				lfmcb = (MCB *)PHYS_PTR(loadfix, 0);
				strcpy( lfmcb->m_name, "LOADFIX" );
				lfmcb->m_psp = 0x40;	// Marker for LOADFIX
				dos_sda.ExecFlags &= ~EXEC_LOADFIX;
			}

			rc = ChildEnv(exp, &env, namep);
		  
			if (rc == NO_ERROR)
				rc = ExecMemLargest(&asize, exe_size);
		  
			exe_size = image_size + ExeHeader.exMaxAlloc;
			if (exe_size > asize || exe_size < image_size)
				exe_size = asize;
		  
			if ((ExeHeader.exMinAlloc | ExeHeader.exMaxAlloc) == 0)
				exe_size = asize;
		  
			if (rc == NO_ERROR)
				rc = ExecMemAlloc(exe_size, &mem, &asize);
			
			if (rc != NO_ERROR)
				DosMemFree(env);
		  
			if (rc != NO_ERROR)
				return rc;
		  
			mode &= 0x7f;
		  
#if DEBUG
			LOGI("DosExeLoader. Loading '%s' at %04x\n", namep, mem);
#endif
		  
			/* memory found large enough - continue processing      */
			++mem;
		}
		else /* OVERLAY */
		{
		  mem = exp->load.load_seg;
		}

		if (SftSeek(fd, ((int)ExeHeader.exHeaderSize)<<4, 0) < NO_ERROR)
		{
			if (mode != OVERLAY)
			{
				DosMemFree(--mem);
				DosMemFree(env);
			}
#if DEBUG
			LOGI("Seek failed!\n");
#endif
			return ERROR_INV_DATA;
		}
		
		start_seg = mem;
		exe_size = image_size;
		if (mode != OVERLAY)
		{
			exe_size -= (sizeof(PSP)>>4);
			start_seg += (sizeof(PSP)>>4);
			if (exe_size > 0 && (ExeHeader.exMinAlloc | ExeHeader.exMaxAlloc) == 0)
			{
				MCB *mp = (MCB *)PHYS_PTR(mem - 1, 0);
				start_seg += (mp->m_size - image_size);		// Load the exe to the end of the allocated block
			}
		}
	}
#if 0
	u8* loadaddress = PHYS_PTR(start_seg, 0);
	int imagesize = ((int)exe_size)<<4;
	int bytes = 1;
	LOGI("Load %08X (%08X) %04X\n", imagesize, loadaddress, start_seg);
	while (imagesize>0x7FFF && bytes > 0) {
		bytes = DosReadSft(fd, 0x8000, loadaddress);
		loadaddress+=0x8000;imagesize-=0x8000;
	}
	if (imagesize > 0 && bytes > 0)
		bytes = DosReadSft(fd, imagesize, loadaddress);
	LOGI("- %08X (%08X) %04X\n", imagesize, loadaddress, bytes);
#else
	bytes = DosReadSft(fd, exe_size<<4, PHYS_PTR(start_seg, 0));
#endif

	if ( bytes <= 0 )
	{
		// We should check whether we managed to read everything! However, FreeDOS does not check this either.
		if (mode != OVERLAY)
		{
			DosMemFree(--mem);
			DosMemFree(env);
		}
#if DEBUG
		LOGI("Read failed! Read %d of %d bytes.\n", bytes, exe_size<<4 );
#endif
		return ERROR_INV_DATA;
	}

	if ( 4*ExeHeader.exRelocItems > sizeof(RelocTbl) )
		return ERROR_OUT_OF_MEM;
	
    SftSeek(fd, ExeHeader.exRelocTable, 0);
#if DEBUG
	LOGI("Reading RelocTable, exRelocItems = %d...\n", ExeHeader.exRelocItems);
#endif
	if (DosReadSft(fd, 4*ExeHeader.exRelocItems, RelocTbl) != 4*ExeHeader.exRelocItems)
	{
		if (mode != OVERLAY)
		{
			DosMemFree(--mem);
			DosMemFree(env);
		}
#if DEBUG
		LOGI("RelocTable read failed!\n" );
#endif
		return ERROR_INV_DATA;
	}
#if DEBUG
	LOGI("Performing relocation, %d items...\n", ExeHeader.exRelocItems);
#endif
	for (i=0; i < ExeHeader.exRelocItems; i++ )
	{
		// Can't use u16* access as the addresses are not halfword-aligned!
		u16 offs = RelocTbl[i*2];
		u16 seg = RelocTbl[i*2+1];
		if ( OVERLAY == mode )
		{
			phys = PHYS_PTR(seg + mem, offs);
			val = (*phys) | ((*(phys+1))<<8);
			val += exp->load.reloc;
		}
		else
		{
			phys = PHYS_PTR(seg + start_seg, offs);
			val = (*phys) | ((*(phys+1))<<8);
			val += start_seg;
		}
		*phys = val & 0xFF;
		*(phys+1) = val >> 8;
	}

#if DEBUG
	LOGI("Closing file...\n");
#endif
	DosCloseSft(fd, 0);

	if (OVERLAY == mode)
		return NO_ERROR;

	setvec(0x22, (REG_CS<<16) | ((int)REG_EIP - (int)PHYS_PTR(REG_CS, 0)));
#if DEBUG
	LOGI("Calling child_psp...\n");
#endif
    child_psp(mem, dos_sda.cu_psp, mem + asize);

    fcbcode = patchPSP(mem - 1, env, exp, namep);
	
    exp->exec.stack = ((ExeHeader.exInitSS + start_seg)<<16) | ExeHeader.exInitSP;
    exp->exec.start_addr = ((ExeHeader.exInitCS + start_seg)<<16) | ExeHeader.exInitIP;

#if DEBUG
	LOGI("Calling load_transfer...\n");
#endif
    load_transfer(mem, exp, fcbcode, mode);
	
#if DEBUG
	LOGI("DosExeLoader done!\n");
#endif
	return NO_ERROR;
}

int DosExec(u16 mode, EXEC_BLK *ep, char *lp)
{
	int rc;
	int fd;

	if ((mode & 0x7f) > 3 || 2 == (mode & 0x7f))
		return ERROR_INV_FORMAT; 

	memcpy(&TempExeBlock, ep, sizeof(EXEC_BLK));

	if (/*IsDevice(lp) ||*/ (fd = DosOpenSft(lp, 0 /*O_LEGACY | O_OPEN | O_RDONLY*/, 0)) < 0)
	{
#if DEBUG	
		LOGI("File '%s' not found (%d)!\n", lp, fd );
#endif		
		return ERROR_FILENOTFOUND;
	}
  
	rc = DosReadSft(fd, sizeof(EXE_HEADER), (u8 *)&ExeHeader);
#if DEBUG	
	LOGI("DosReadSft rc=%d, fd=%d\n", rc, fd );
#endif		

	if (rc == sizeof(EXE_HEADER) && (ExeHeader.exSignature == MAGIC || ExeHeader.exSignature == OLD_MAGIC))
	{
		rc = DosExeLoader(lp, &TempExeBlock, mode, fd);
	}
	else if (rc > 0)
	{
		rc = DosComLoader(lp, &TempExeBlock, mode, fd);
	}

	DosCloseSft(fd, 0);

	if (LOAD == mode && NO_ERROR == rc)
		memcpy(ep, &TempExeBlock, sizeof(EXEC_BLK));

	return rc;
}

void return_user(int tsr)
{
	PSP *p, *q;
	int i;

	/* restore parent                                       */
	p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);

	// If we are attempting to return from the root program, exit.
	if (DOS_PSP == p->ps_parent)
#ifdef RPi
		exit_program();
#else
		IRQRequest(IRQ_EXIT);
#endif

	/* When process returns - restore the isv               */
	setvec(0x22, p->ps_isv22);
	setvec(0x23, p->ps_isv23);
	setvec(0x24, p->ps_isv24);

	/* And free all process memory if not a TSR return      */
	if (!tsr && p->ps_parent != dos_sda.cu_psp)
	{
		for (i = 0; i < p->ps_maxfiles; i++)
		{
			DosClose(i);
		}
		//FcbCloseAll();
		FreeProcessMem(dos_sda.cu_psp);
	}

	dos_sda.cu_psp = p->ps_parent;
	q = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);

	// Ignore config changes when in Windows 3.0 Standard Mode
	if ( memcmp(CurrentEXE, "DOSX", 4 ) || 0 == memcmp((char *)q-8, "4DOS", 4))
	{
		memcpy( CurrentEXE, (char *)q-8, 8 );

		if ( memcmp(CurrentEXE, "KERNEL", 6 ) )	// Do not change the confing when a Windows 3.0 program exits
			UseConfig(CurrentEXE);									// Switch to the config of the parent
	}

	SET_SP(q->ps_stack);									// SP initial value
	REG_SS = (q->ps_stack>>16)&0xFFFF;

#if 1
	// DOSBox-style register restore!
	if ( q->ps_stack )
	{
		// Restore pushed user registers. NOTE! SP might not be halfword-aligned! (for example Micro Machines)
		u8 *regsave = (u8 *)PHYS_PTR(REG_SS, SP);
		SET_AX((regsave[1]<<8)|regsave[0]);
		SET_CX((regsave[3]<<8)|regsave[2]);
		SET_DX((regsave[5]<<8)|regsave[4]);
		SET_BX((regsave[7]<<8)|regsave[6]);
		SET_BP((regsave[9]<<8)|regsave[8]);
		SET_SI((regsave[11]<<8)|regsave[10]);
		SET_DI((regsave[13]<<8)|regsave[12]);
		REG_DS = (regsave[15]<<8)|regsave[14];
		REG_ES = (regsave[17]<<8)|regsave[16];
		// Emulate the original INT 21 input values
		regsave[18] = p->ps_isv22;		// IP
		regsave[19] = p->ps_isv22>>8;	// IP
		regsave[20] = p->ps_isv22>>16;	// CS
		regsave[21] = p->ps_isv22>>24;	// CS
		REG_SP += 0x0018;
#else
	// FreeDOS-style register restore!
	if ( q->ps_stack )
	{
		// Restore pushed user registers. NOTE! SP might not be halfword-aligned! (for example Micro Machines)
		u8 *regsave = (u8 *)PHYS_PTR(REG_SS, SP);
		SET_AX((regsave[-1]<<8)|regsave[-2]);
		SET_CX((regsave[-3]<<8)|regsave[-4]);
		SET_DX((regsave[-5]<<8)|regsave[-6]);
		SET_BX((regsave[-7]<<8)|regsave[-8]);
		SET_BP((regsave[-9]<<8)|regsave[-10]);
		SET_SI((regsave[-11]<<8)|regsave[-12]);
		SET_DI((regsave[-13]<<8)|regsave[-14]);
		REG_DS = (regsave[-15]<<8)|regsave[-16];
		REG_ES = (regsave[-17]<<8)|regsave[-18];
#endif
		REG_CS = (p->ps_isv22>>16)&0xFFFF;
		REG_EIP = (int)PHYS_PTR(REG_CS, p->ps_isv22&0xFFFF);
	}
	else
	{
		// Root shell exited!
		LOGI("Root shell exit!\n");
	}
	//if (InDOS)
	//	--InDOS;
	//exec_user((iregs FAR *) q->ps_stack, 0);
}
