//=============================================================================
// DOS_fcb.c
//
// This file contains the DOS FileControlBlock file routines.
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

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

#define PARSE_SEP_STOP          0x01
#define PARSE_DFLT_DRIVE        0x02
#define PARSE_BLNK_FNAME        0x04
#define PARSE_BLNK_FEXT         0x08

#define PARSE_AL_NOWILD        0x00
#define PARSE_AL_WILD          0x01
#define PARSE_AL_BADDRIVE      0xFF

#define TestCmnSeps(lpFileName) (*lpFileName && strchr(":<|>+=,", *lpFileName) != NULL)
#define TestFieldSeps(lpFileName) ((unsigned char)*lpFileName <= ' ' || strchr("/\"[]<>|.", *lpFileName) != NULL)

static FCB *ExtFcbToFcb(XFCB *lpExtFcb)
{
	if (*((u8 *)lpExtFcb) == 0xff)
		return &lpExtFcb->xfcb_fcb;
	else
		return (FCB *)lpExtFcb;
}

static void FcbNameInit(FCB *lpFcb, char *szBuffer, int *pCurDrive)
{
	/* Build a traditional DOS file name                            */
	*pCurDrive = dos_sda.default_drive + 1;
	if (lpFcb->fcb_drive != 0)
	{
		*pCurDrive = lpFcb->fcb_drive;
		szBuffer[0] = 'A' + lpFcb->fcb_drive - 1;
		szBuffer[1] = ':';
		szBuffer += 2;
	}
	Name83ToSz(szBuffer, lpFcb->fcb_fname);
}

static FCB *CommonFcbInit(XFCB *lpExtFcb, char *pszBuffer, int *pCurDrive)
{
	/* convert to fcb if needed first                               */
	FCB *lpFcb = ExtFcbToFcb(lpExtFcb);
	/* Build a traditional DOS file name                            */
	FcbNameInit(lpFcb, pszBuffer, pCurDrive);
	/* and return the fcb pointer                                   */
	return lpFcb;
}


static const char *ParseSkipWh(const char *lpFileName)
{
	while (*lpFileName == ' ' || *lpFileName == '\t')
		++lpFileName;
	return lpFileName;
}

static const char *GetNameField(const char *lpFileName, char *lpDestField, int nFieldSize, bool *pbWildCard)
{
	int 	nIndex = 0;
	char 	cFill = ' ';

	while (*lpFileName != 0 && !TestFieldSeps(lpFileName) && nIndex < nFieldSize)
	{
		if (' ' == *lpFileName)
			break;
		if ('*' == *lpFileName)
		{
			*pbWildCard = true;
			cFill = '?';
			++lpFileName;
			break;
		}
		if ('?' == *lpFileName)
			*pbWildCard = true;
		*lpDestField++ = (*lpFileName >= 'a' && *lpFileName <= 'z') ? *lpFileName++ - 32 : *lpFileName++;
		++nIndex;
	}

	memset(lpDestField, cFill, nFieldSize - nIndex);
	return lpFileName;
}

//----------------------------------------------------------------------------
// On input:
// 	AL has *wTestMode,
// 	lpFileName == DS_SI_PTR,
// 	lpFcb == ES_DI_PTR.
// We need to return the value in SI.
//
void FcbParseFname(int al, const char *lpFileName, FCB *lpFcb)
{
	bool wRetCodeName = false, wRetCodeExt = false;

	if (!(al & PARSE_SEP_STOP))
	{
		lpFileName = ParseSkipWh(lpFileName);
		if (TestCmnSeps(lpFileName))
			++lpFileName;
	}

	/* Undocumented "feature," we skip white space anyway           */
	lpFileName = ParseSkipWh(lpFileName);

	/* Now check for drive specification                            */
	if (':' == *(lpFileName + 1))
	{
		if ( *lpFileName != 'C' && *lpFileName != 'c' )
		{
			SET_AL(PARSE_AL_BADDRIVE);
			SET_SI(SI + ((int)lpFileName - (int)DS_SI_PTR));
			return;
		}

		lpFcb->fcb_drive = 3;	// 'C' drive
		lpFileName += 2;
	}
	else if (!(al & PARSE_DFLT_DRIVE))
	{
		lpFcb->fcb_drive = 0;	// Default drive
	}

	/* Undocumented behavior, set record number & record size to 0  */
	lpFcb->fcb_cublock = lpFcb->fcb_recsiz = 0;

	if (!(al & PARSE_BLNK_FNAME))
		memset(lpFcb->fcb_fname, ' ', FNAME_SIZE);

	if (!(al & PARSE_BLNK_FEXT))
		memset(lpFcb->fcb_fext, ' ', FEXT_SIZE);

	/* special cases: '.' and '..' */
	if (*lpFileName == '.')
	{
		lpFcb->fcb_fname[0] = '.';
		++lpFileName;
		if (*lpFileName == '.')
		{
			lpFcb->fcb_fname[1] = '.';
			++lpFileName;
		}
		SET_AL(PARSE_AL_NOWILD);
		SET_SI(SI + ((int)lpFileName - (int)DS_SI_PTR));
		return;
	}

	/* Now to format the file name into the string                  */
	lpFileName = GetNameField(lpFileName, lpFcb->fcb_fname, FNAME_SIZE, &wRetCodeName);

	/* Do we have an extension? If do, format it else return        */
	if (*lpFileName == '.')
		lpFileName = GetNameField(++lpFileName, lpFcb->fcb_fext, FEXT_SIZE, &wRetCodeExt);

	SET_AL((wRetCodeName | wRetCodeExt) ? PARSE_AL_WILD : PARSE_AL_NOWILD);
	SET_SI(SI + ((int)lpFileName - (int)DS_SI_PTR));
}

static void FcbNextRecord(FCB *lpFcb)
{
	if (++lpFcb->fcb_curec >= 128)
	{
		lpFcb->fcb_curec = 0;
		++lpFcb->fcb_cublock;
	}
}

static u32 FcbRec(FCB *lpFcb)
{
	return ((u32) lpFcb->fcb_cublock * 128) + lpFcb->fcb_curec;
}

static void FcbCalcRec(XFCB *lpXfcb)
{
	/* Convert to fcb if necessary                                  */
	FCB *lpFcb = ExtFcbToFcb(lpXfcb);

	/* Now update the fcb and compute where we need to position     */
	/* to.                                                          */
	lpFcb->fcb_cublock = (u16)(lpFcb->fcb_rndm / 128);
	lpFcb->fcb_curec = (u8)lpFcb->fcb_rndm & 127;
}

int FcbReadWrite(XFCB *lpXfcb, int recno, int mode)
{
	u32 lPosit;
	int nTransfer;
	FCB *lpFcb;
	unsigned size;
	unsigned recsiz;

	/* Convert to fcb if necessary                                  */
	lpFcb = ExtFcbToFcb(lpXfcb);

	recsiz = lpFcb->fcb_recsiz;
	size = recsiz * recno;

	if ((dos_sda.dta&0xFFFF) + size > 0xFFFF)
		return FCB_ERR_SEGMENT_WRAP;

	/* Now update the fcb and compute where we need to position to. */
	lPosit = FcbRec(lpFcb) * recsiz;
	nTransfer = SftSeek(lpFcb->fcb_sftno, lPosit, 0);
	if (nTransfer < 0 )
	{
		dos_sda.CritErrCode = -nTransfer;
		return FCB_ERR_NODATA;
	}

	/* Do the read / write                                           */
	if ( FCB_READ & mode )
		nTransfer = DosReadSft(lpFcb->fcb_sftno, size, PHYS_PTR((dos_sda.dta>>16)&0xFFFF,dos_sda.dta&0xFFFF));
	else
		nTransfer = DosWriteSft(lpFcb->fcb_sftno, size, PHYS_PTR((dos_sda.dta>>16)&0xFFFF,dos_sda.dta&0xFFFF));
	
	if (nTransfer < 0)
		dos_sda.CritErrCode = -(int)nTransfer;

	/* Now find out how we will return and do it.                   */
	if (mode & FCB_WRITE)
		lpFcb->fcb_fsize = SftGetFSize(lpFcb->fcb_sftno);

	/* if end-of-file, then partial read should count last record */
	if ((mode & FCB_RANDOM) && recsiz > 0)
		lpFcb->fcb_rndm += ((unsigned)nTransfer + recsiz - 1) / recsiz;
	size -= (unsigned)nTransfer;
	if (size == 0)
	{
		FcbNextRecord(lpFcb);
		return FCB_SUCCESS;
	}
	size %= lpFcb->fcb_recsiz;
	if ((FCB_READ & mode) && size > 0)
	{
		memset(PHYS_PTR((dos_sda.dta>>16)&0xFFFF,dos_sda.dta&0xFFFF) + (unsigned)nTransfer, 0, size);
		FcbNextRecord(lpFcb);
		return FCB_ERR_EOF;
	}
	return FCB_ERR_NODATA;
}

int FcbRandomBlockIO(XFCB *lpXfcb, int *nRecords, int mode)
{
	int nErrorCode;
	FCB *lpFcb;
	int	old;

	FcbCalcRec(lpXfcb);

	/* Convert to fcb if necessary                                  */
	lpFcb = ExtFcbToFcb(lpXfcb);

	old = lpFcb->fcb_rndm;
	nErrorCode = FcbReadWrite(lpXfcb, *nRecords, mode);
	*nRecords = (lpFcb->fcb_rndm - old);

	/* Now update the fcb                                           */
	FcbCalcRec(lpXfcb);

	return nErrorCode;
}

int FcbRandomIO(XFCB *lpXfcb, int mode)
{
  u16 uwCurrentBlock;
  u8 ucCurrentRecord;
  int nErrorCode;
  FCB *lpFcb;

  FcbCalcRec(lpXfcb);

  /* Convert to fcb if necessary                                  */
  lpFcb = ExtFcbToFcb(lpXfcb);

  uwCurrentBlock = lpFcb->fcb_cublock;
  ucCurrentRecord = lpFcb->fcb_curec;

  nErrorCode = FcbReadWrite(lpXfcb, 1, mode);

  lpFcb->fcb_cublock = uwCurrentBlock;
  lpFcb->fcb_curec = ucCurrentRecord;
  return nErrorCode;
}

/* FcbOpen and FcbCreate
   Expects lpXfcb to point to a valid, unopened FCB, containing file name to open (create)
   Create will attempt to find the file name in the current directory, if found truncates
     setting file size to 0, otherwise if does not exist will create the new file; the
     FCB is filled in same as the open call.
   On any error returns FCB_ERROR
   On success returns FCB_SUCCESS, and sets the following fields (other non-system reserved ones left unchanged)
     drive identifier (fcb_drive) set to actual drive (1=A, 2=B, ...; always >0 if not device)
     current block number (fcb_cublock) to 0
     file size (fcb_fsize) value from directory entry (0 if create)
     record size (fcb_recsiz) to 128; set to 0 for devices
     time & date (fcb_time & fcb_date) values from directory entry
     fcb_sftno, fcb_attrib_hi/_lo, fcb_strtclst, fcb_dirclst/off_unused are for internal use (system reserved)
*/
int FcbOpen(XFCB *lpXfcb, unsigned flags)
{
	SFT *sftp;
	int sft_idx, FcbDrive;
	unsigned attr = 0;

	/* Build a traditional DOS file name                            */
	FCB *lpFcb = CommonFcbInit(lpXfcb, dos_sda.SecPathName, &FcbDrive);
	if ((flags & 4 /*O_CREAT*/) && lpXfcb->xfcb_flag == 0xff)
		/* pass attribute without constraints (dangerous for directories) */
		attr = lpXfcb->xfcb_attrib;

	sft_idx = DosOpenSft(dos_sda.SecPathName, flags, attr);
	if (sft_idx < 0)
	{
		dos_sda.CritErrCode = -sft_idx;
		return FCB_ERROR;
	}

	sftp = idx_to_sft(sft_idx);
	sftp->sft_mode |= O_FCB;

	lpFcb->fcb_sftno = sft_idx;
	lpFcb->fcb_cublock = 0;

	lpFcb->fcb_curec = 0;		// freedos does not clear this??
	
	lpFcb->fcb_recsiz = 0;      /* true for devices   */
	if (!(sftp->sft_flags & SFT_FDEVICE)) /* check for a device */
	{
		lpFcb->fcb_drive = FcbDrive;
		lpFcb->fcb_recsiz = 128;
	}
	
	// Use dos_sda.SearchDir generated by DosGetFAttr for file size and date
	if ( NO_ERROR == DosGetFAttr(dos_sda.SecPathName) )
	{
		lpFcb->fcb_fsize = dos_sda.SearchDir.dir_size;
		lpFcb->fcb_date = dos_sda.SearchDir.dir_date;
		lpFcb->fcb_time = dos_sda.SearchDir.dir_time;
	}
	return FCB_SUCCESS;
}

int FcbClose(XFCB *lpXfcb)
{
	SFT *s;

	/* Convert to fcb if necessary                                  */
	FCB *lpFcb = ExtFcbToFcb(lpXfcb);

	/* An already closed FCB can be closed again without error */
	if (lpFcb->fcb_sftno == (u8)0xff)
		return FCB_SUCCESS;

	/* Get the SFT block that contains the SFT      */
	if (NULL == (s = idx_to_sft(lpFcb->fcb_sftno)))
		return FCB_ERROR;

	/* change time and set file size                */
	s->sft_size = lpFcb->fcb_fsize;
	//if (!(s->sft_flags & SFT_FSHARED))
		//dos_merge_file_changes(lpFcb->fcb_sftno);
	//DosSetFtimeSft(lpFcb->fcb_sftno, lpFcb->fcb_date, lpFcb->fcb_time);
	DosCloseSft(lpFcb->fcb_sftno, false);
	
    lpFcb->fcb_sftno = (u8) 0xff;
    return FCB_SUCCESS;
}

int FcbFind(XFCB *lpXfcb, int First)
{
	u32	orig_dta = dos_sda.dta;
	FCB *lpFcb;
	u8 *lpDir = PHYS_PTR((dos_sda.dta>>16)&0xFFFF,dos_sda.dta&0xFFFF);	// Physical pointer to caller's FCB-style DTA
	int FcbDrive;

	// Use a temporary DTA buffer in our SDA area
	dos_sda.dta = (DOS_SDA_SEG<<16) | ((u8*)&(dos_sda.Dmatch)-&(dos_sda.ErrorMode));

	/* Next initialize local variables by moving them from the fcb   */
	lpFcb = CommonFcbInit(lpXfcb, dos_sda.SecPathName, &FcbDrive);
	
	/* Reconstruct the dirmatch structure from the fcb - doesn't hurt for first */
	dos_sda.Dmatch.dm_drive = lpFcb->fcb_sftno;

	memcpy(dos_sda.Dmatch.dm_name_pat, lpFcb->fcb_fname, FNAME_SIZE + FEXT_SIZE);
	//DosUpFMem((BYTE FAR *) Dmatch.dm_name_pat, FNAME_SIZE + FEXT_SIZE);
  
	dos_sda.Dmatch.dm_attr_srch = dos_sda.wAttr;
	dos_sda.Dmatch.dm_entry = lpFcb->fcb_strtclst;
	dos_sda.Dmatch.dm_dircluster = lpFcb->fcb_dirclst;

	dos_sda.wAttr = ATTRIB_ALL;
  
	if ((XFCB *)lpFcb != lpXfcb)
	{
		dos_sda.wAttr = lpXfcb->xfcb_attrib;
		memcpy(lpDir, lpXfcb, 7);
		lpDir += 7;
	}

	dos_sda.CritErrCode = -(First ? DosFindFirst(dos_sda.SecPathName, dos_sda.wAttr) : DosFindNext());
	if (dos_sda.CritErrCode != NO_ERROR)
	{
		dos_sda.dta = orig_dta;
		return FCB_ERROR;
	}

	*lpDir++ = FcbDrive;
	memcpy(lpDir, &(dos_sda.SearchDir), 32);
  
	lpFcb->fcb_dirclst = (u16) dos_sda.Dmatch.dm_dircluster;
	lpFcb->fcb_strtclst = dos_sda.Dmatch.dm_entry;
  
	lpFcb->fcb_sftno = dos_sda.Dmatch.dm_drive;   /* MSD seems to save this @ fcb_date. */
	dos_sda.dta = orig_dta;
	return FCB_SUCCESS;
}
