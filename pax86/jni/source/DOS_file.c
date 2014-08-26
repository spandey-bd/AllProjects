//=============================================================================
// DOS_file.c
//
// This file contains the DOS file and directory routines.
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
#ifdef WP8
#include <windows.h>
#include <io.h>		// For _findfirst
#undef NO_ERROR
#undef ERROR_SEEK
#else
#include <dirent.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>
#ifndef WP8
#ifndef IOS
#include <sys/vfs.h>
#endif
#include <sys/time.h>
#include <unistd.h>
#endif
#include <errno.h>
#include <ctype.h>

#define	TODO	1

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

#define	DEBUG		0
#define	INTERNALEXE	0

char PriPathName[256];				// Internal work variable for file/path name handling
char cwd[MAXPATHLEN] = "C:\\";		// Current working directory, in DOS format
char ccwd[MAXPATHLEN] = "C:\\";		// Current C: drive working directory
char dcwd[MAXPATHLEN] = "D:\\";		// Current D: drive working directory (if HaveCD)
char finddir[MAXPATHLEN];			// Used as a current directory for FindFirst/FindNext, in UNIX format
char findname[8+1+3+1];			// Used as a name pattern for FindFirst/FindNext, in Sz format
DIRMATCH *dta = NULL;
char WriteFileName[256];			// File name we might need to reopen when actually writing

static char aDataDir[128] = "";		// Internal data directory in Android
static char aExtDir[128] = "";		// External directory in Android

#ifdef RII_EX
char ser_filenames[DOS_SFT_SIZE][256];	// Remember the actual filenames for serialization.
#endif

#ifdef WP8

#define printf retappend
extern void retappend(const char *format, ...);

#endif

// Convert the DOS-format filename "C:\path\filename" to Unix-format "/path/filename".
// mode:
//	0 = do not perform any checks
//	1 = determine that the path leading to the file has correct upper/lower case
//	2 = determine that the whole path + filename has correct upper/lower case
void UnixName(char *out, char *in, int mode)
{
#ifndef WP8
	DIR 	*pdir;
	struct	dirent *paent;
#endif
	struct	stat statbuf;
	char	*pfend, *ppend;
	char	found;

	char *ptr = out + sprintf(out, "%s", ('D' == in[0]) ? HostCD : aExtDir);	// First put the Android root directory
//LOGI("UnixName('%s')\n", in);
	if ( ':' == in[1] )
		in += 2;
	ppend = ptr; // To avoid compiler warning
	while ( *in )
	{
		if ( '\\' == *in )
		{
			ppend = ptr;	// Remember the last path end
#ifdef WP8
			*ptr++ = *in++;
#else
			*ptr++ = '/';
			in++;
#endif
		}
		else
			*ptr++ = *in++;
	}
	*ptr = 0;
#ifdef WP8
	return;
#else
	// Skip existence and name case checks if so requested
	if (0 == mode)
		return;
	// Determine that we have a correct case for the path leading to the file.
//LOGI("- Path/file '%s' needs testing...\n", out);
	if (stat(out, &statbuf) == 0)
	{
//LOGI("- Path/file '%s' found and returned!\n", out);
		return;
	}
	pfend = ptr;	// Remember the actual filename end.
	do {
		while (ptr > out && *ptr != '/')
			ptr--;
		if (ptr > out)
		{
			*ptr = 0;
			if (stat(out, &statbuf) == 0)
			{
//LOGI("- Path '%s' found!\n", out);
				break;	// OK, found a path that exists.
			}
		}
	} while (ptr > out);
	// Now we have an existing path in out.
	if (ptr == ppend && 1 == mode)
	{
		// If we were only requested to check path existence and it does exist, return.
		*ptr = '/';
		return;
	}
	// The rest of the path does not exist in upper case, so check if it exists in mixed case.
	do {
		found = 0;
		pdir = opendir(out);
		if ( pdir )
		{
			while ( (paent = readdir( pdir )) )
			{
				char *p1 = ptr + 1;
				char *p2 = paent->d_name;
				while (*p1 && *p2 && (*p1 == *p2 || (*p2 >= 'a' && *p2 <= 'z' && *p1 == *p2 - 32)))
				{
					p1++;
					p2++;
				}
				if (0 == *p1 && 0 == *p2)
				{
					// Found a directory but possibly with a different case!
					*ptr = '/';
					strcpy( ptr+1, paent->d_name);
					ptr = p1;
					found = 1;
//LOGI("- Path '%s' found!\n", out);
					break;
				}
			}
			closedir( pdir );
		}
	} while (found && ptr < pfend);
	// Fix the name if we did not find all parts, let the DOS routines report an error if needed.
	while (ptr < pfend)
	{
		if (0 == *ptr)
			*ptr = '/';
		ptr++;
	}
//LOGI("- Path '%s' returned.\n", out);
#endif
}

static int check_host_path(char *PathName)
{
	char UnixPathName[MAXPATHLEN];
#ifdef WP8
	struct	stat statbuf;

	UnixName( UnixPathName, PathName, 2 );	// From "C:\DIR1\DIR2" to "/DIR1/DIR2"
	if (stat(UnixPathName, &statbuf) != 0)
		return ERROR_PATHNOTFOUND;
#else
	DIR *pdir;
	UnixName( UnixPathName, PathName, 2 );	// From "C:\DIR1\DIR2" to "/DIR1/DIR2"
	pdir=opendir(UnixPathName);
	if ( NULL == pdir )
		return ERROR_PATHNOTFOUND;
	closedir(pdir);
#endif
	return NO_ERROR;
}

#define PNE_WILDCARD 1
#define PNE_DOT 2

static const char _DirChars[] = "\"[]:|<>+=;,";

#define DirChar(c)  (((unsigned char)(c)) >= ' ' && !strchr(_DirChars, (c)))

#define addChar(c) \
{ \
  if (p >= dest + MAXPATHLEN) goto errRet; 	\
  *p++ = c; \
}

int truename(const char *src, char *dest, int Wildcards)
{
	int 	i;
	int		state;
	char 	*p = dest;	  /* dynamic pointer into dest */
	char 	*rootPos;
	char 	src0;

	//printf("truename('%s'\n", src);
	/* In opposite of the TRUENAME shell command, an empty string is rejected by MS DOS 6 */
	src0 = src[0];
	if ( 0 == src0)
		return ERROR_FILENOTFOUND;

	if (src0 == '\\' && src[1] == '\\')	// UNC path name, not supported
		return ERROR_PATHNOTFOUND;
		
	dest[0] = cwd[0];
	dest[1] = ':';
	dest[2] = '\\';

	if (':' == src[1] )
	{
		if ('c' == src0 || 'd' == src0 )
			src0 = src0 - 32;
		if ( !('C' == src0 || (HaveCD && 'D' == src0)) )
			return ERROR_PATHNOTFOUND;
		dest[0] = src0;
	}
	
	//cdsEntry = get_cds(result);
	
	//dhp = IsDevice(src);
	if ( 0 == strcmp( src, "NUL" ) || 0 == strcmp( src, "EMMXXXX0" ) /*|| 0 == strcmp( src, "CON" )*/ )
	{
		strcpy( dest, src );
		return IS_DEVICE;
	}

	if (src[1] == ':')
		src += 2;

	/* Make fully-qualified logical path */
	/* register these two used characters and the \0 terminator byte */
	/* we always append the current dir to stat the drive;
	   the only exceptions are devices without paths */
	rootPos = p = dest + 2;
	if (*p != '/') /* i.e., it's a backslash! */
	{
		char *cp;

		cp = ('D' == dest[0]) ? dcwd : ccwd;
		/* ensure termination of strcpy */
		cp[MAXPATHLEN - 1] = 0;

		//if (media_check(TempCDS.cdsDpb) < 0)
        //return DE_PATHNOTFND;

		/* check_host_path ensures that the path exists; if not, we need to change to the root directory */
		if (check_host_path(cp) != NO_ERROR)
		{
			cp[3] = 0;
			if ('D' == dest[0])
				dcwd[3] = 0;
			else
				ccwd[3] = 0;
			check_host_path(cp);
		}

		cp += 2;
		strcpy(p, cp);

		if (p[0] == '\0')
			p[1] = p[0];
		p[0] = '\\'; /* force backslash! */

		if (*src != '\\' && *src != '/')
			p += strlen(p);
		else /* skip the absolute path marker */
			src++;
		/* remove trailing separator */
		if (p[-1] == '\\') p--;
    }

	/* append the path specified in src */
	state = 0;
	while (*src)
	{
		/* New segment.  If any wildcards in previous segment(s), this is an invalid path. */
		if (state & PNE_WILDCARD)
			return ERROR_PATHNOTFOUND;

		/* append backslash if not already there.
		   MS DOS preserves a trailing '\\', so an access to "C:\\DOS\\"
		   or "CDS.C\\" fails; in that case the last new segment consists of just the \ */
		if (p[-1] != *rootPos)
			addChar(*rootPos);

		/* skip multiple separators (duplicated slashes) */
		while ('/' == *src || '\\' == *src)
			src++;

		if ('.' == *src)
		{
			/* special directory component */
			++src;
			if ('.' == *src) /* skip the second dot */
				++src;
			if ('/' == *src || '\\' == *src || 0 == *src)
			{
				--p; /* backup the backslash */
				if (src[-2] == '.')
				{
					/* ".." entry */
					/* remove last path component */
					while (*--p != '\\')
						if (p <= rootPos) /* already on root */
							return ERROR_PATHNOTFOUND;
				}
				continue;	/* next char */
			}

			/* ill-formed .* or ..* entries => return error */
errRet:
			/* The error is either PATHNOTFND or FILENOTFND depending on if it is not the last component */
			return strchr(src, '/') == 0 && strchr(src, '\\') == 0 ? ERROR_FILENOTFOUND : ERROR_PATHNOTFOUND;
		}
		
		/* normal component */
		/* append component in 8.3 convention */

		/* *** parse name and extension *** */
		i = 8;
		state &= ~PNE_DOT;
		while (*src != '/' && *src  != '\\' && *src != 0)
		{
			char c = *src++;
			if (c == '*')
			{
				/* register the wildcard, even if no '?' is appended */
				c = '?';
				while (i)
				{
					--i;
					addChar(c);
				}
			}
			if (c == '.')
			{
				if (state & PNE_DOT) /* multiple dots are ill-formed */
					goto errRet;
				/* strip trailing dot */
				if ('/' == *src || '\\' == *src || 0 == *src)
					break;
				/* we arrive here only when an extension-dot has been found */
				state |= PNE_DOT;
				i = 3 + 1;
			}
			else if (c == '?')
				state |= PNE_WILDCARD;
			if (i)
			{	/* name length in limits */
				--i;
				if (!DirChar(c))
					goto errRet;
				if ( c != ' ' )		// Ignore blanks (like in "WIZ1    .DSK")
					addChar(c);
			}
		}
		/* *** end of parse name and extension *** */
	}

	if ((state & PNE_WILDCARD) && !Wildcards)
		return ERROR_PATHNOTFOUND;
	if (dest+2 == p)
	{
		/* we must always add a separator if dest = "c:" */
		addChar('\\');
	}

	*p = '\0';				/* add the string terminator */
	
	/* Uppercase the string (we only handle ASCII chars here) */
	for ( p = rootPos; *p; p++ )
		if ( *p >= 'a' && *p <= 'z')
			*p = *p - 32;
	
	return NO_ERROR;
}

SFT *lpCurSft = NULL;

int idx_to_sft_(int sftIndex)
{
	/*called from below and int2f/ax=1216*/

	lpCurSft = NULL;
	if (sftIndex < 0)
		return -1;

    if (sftIndex < dos_sft.sftt_count)
    {
		/* finally, point to the right entry            */
		lpCurSft = (SFT *)&(dos_sft.sftt_table[sftIndex]);
		return sftIndex;
    }

	/* If not found, return an error                */
	return -1;
}

SFT *idx_to_sft(int sftIndex)
{
	/* called internally only */
	sftIndex = idx_to_sft_(sftIndex);
	/* if not opened, the SFT is useless            */
	if (-1 == sftIndex || 0 == lpCurSft->sft_count)
		return NULL;
	return lpCurSft;
}

int SetPSPMaxFiles(int cnt)
{
	int rc;
	u16 para;
	u8 *newtab, *psptab;
	PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	if (cnt > dos_sft.sftt_count - 3)
		return ERROR_OUT_OF_MEM;
	psptab = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));
	if (p->ps_maxfiles <= 20 && cnt > 20)
	{
		// Copy the table from PSP to allocated memory
		rc = DosMemAlloc((cnt+3+15)>>4, 0, &para, NULL);
		if (rc != NO_ERROR)
			return rc;
		++para; // Point to the data instead of MCB
		newtab = (u8 *)PHYS_PTR(para,0);
		memcpy( newtab, psptab, 20);
		memset( newtab+20, 0xFF, cnt-20);
		p->ps_maxfiles = cnt;
		p->ps_filetab = ((u32)para)<<16;
	}
	else if (p->ps_maxfiles > 20 && cnt <= 20)
	{
		// Copy table back to PSP
		memcpy( ((u8 *)p) + 0x18, psptab, 20);
		p->ps_maxfiles = cnt;
		p->ps_filetab = (dos_sda.cu_psp<<16) | 0x18;
	}
	return 0;
}

int get_sft_idx(int hndl)
{
	PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	u8 	*filetab = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));
	int idx;

	if (hndl >= p->ps_maxfiles || hndl < 0)
		return ERROR_INV_HANDLE;

	idx = filetab[hndl];
	return idx == 0xff ? ERROR_INV_HANDLE : idx;
}

int get_free_hndl(void)
{
	int i;
	PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	u8 	*q = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));
	for ( i=0; i < p->ps_maxfiles; i++ )
		if ( 0xFF == q[i] )
			return i;
	return ERROR_TOO_MANY;
}

static SFT *get_free_sft(int *sft_idx)
{
	int sys_idx = 0;
	SFT *sfti = (SFT *)&(dos_sft.sftt_table[0]);
	int	i = dos_sft.sftt_count;

    for (; --i >= 0; sys_idx++, sfti++)
    {
		if (0 == sfti->sft_count)
		{
			*sft_idx = sys_idx;
			return sfti;
		}
    }
	/* If not found, return an error                */
	return NULL;
}

/* Internal file open code. */
int DosOpenSft(char *fname, u16 flags, u16 attrib)
{
	char	realname[256];
	int sft_idx;
	SFT	*sftp;
	int result;
#if DEBUG
	LOGI("DosOpenSft(%s, %d, %d)\n", fname, flags, attrib);
#endif
	
	result = truename( fname, PriPathName, 0);
	if ( result < 0 )
		return result;
#if DEBUG
	LOGI("truename='%s'\n", PriPathName);
#endif
		
	// Then convert the input name to Unix format.
	UnixName( realname, PriPathName, 1 );
#if DEBUG
	LOGI("realname='%s'\n", realname);
#endif

	//struct dhdr FAR *dhp;

	/* now get a free system file table entry       */
	if (NULL == (sftp = get_free_sft(&sft_idx)))
		return ERROR_TOO_MANY;
#if DEBUG
	LOGI("sftp=%p\n", sftp);
#endif

	memset(sftp, 0, sizeof(SFT));

	sftp->sft_psp = dos_sda.cu_psp;
	sftp->sft_mode = flags & 0xf0f3;
	//OpenMode = (BYTE) flags;

	sftp->sft_attrib = attrib = attrib | ATTRIB_ARCH;

	/* check for a device   */
	if ((result & IS_DEVICE) /* && (dhp = IsDevice(fname)) != NULL*/)
	{
		int rc = DeviceOpenSft(realname, sftp);
		/* check the status code returned by the
		* driver when we tried to open it
		*/
		if (rc < NO_ERROR)
			return rc;
		return sft_idx;
	}

	sftp->sft_count++;
	sftp->sft_flags = PriPathName[0] - 'A';

	// Then try to open the file.
	if ( fatOK )
	{
		const char *mode;
		if ( O_CREAT & flags )					// Create or Truncate file
		{
#ifdef WP8
			mode = "wb+";
#else
			mode = "w+";
#endif
			if ( ! (O_TRUNC & flags) )			// Create, do not truncate!
			{
				// We are to create a new file that must not exist already!
				sftp->sft_file = fopen( realname, "r" );
				if ( sftp->sft_file != NULL )
				{
					// File exists already!
					fclose(sftp->sft_file);
					sftp->sft_count--;
					return ERROR_FILEEXISTS;
				}
			}
		}
		else if ( O_RDONLY == (flags&3) )		// Open for Read only
#ifdef WP8
			mode = "rb";
#else
			mode = "r";
#endif
		else if ( O_WRONLY == (flags&3) )		// Open for Write only
		{
			// This is difficult! DUNE2 opens the existing file for write only and expects it to be truncated,
			// while WC2 opens the existing file for write only and expects it NOT to get truncated!
			// DUNE2 begins by writing 0 bytes to the file, WC2 starts by seeking to the beginning of the file.
			// First check if the file exists. If not, we can simply create it for writing.
			sftp->sft_file = fopen( realname, "r+" );
			if ( NULL == sftp->sft_file )
#ifdef WP8
				mode = "wb";
#else
				mode = "w";
#endif
			else
			{
				// Okay, the file existed before, mark that the next operation will determine the
				// actual open mode we will use.
				strcpy( WriteFileName, realname );
				sftp->sft_mode |= O_TESTOPEN;
				sftp->sft_posit = 0;
#ifdef RII_EX
				// Remember the actual filename for serialization.
				strcpy(ser_filenames[sft_idx], realname ); 
#endif
				return sft_idx;
			}
		}
		else									// Open for Read and Write, do not truncate
#ifdef WP8
			mode = "rb+";
#else
			mode = "r+";
#endif
		sftp->sft_file = fopen( realname, mode );
#if DEBUG
		LOGI("fopen(%s, %s) = %p (%d))\n", realname, mode, sftp->sft_file, errno);
#endif
		if ( NULL == sftp->sft_file )
		{
			sftp->sft_count--;
			return ERROR_FILENOTFOUND;
		}
		sftp->sft_posit = 0;
		//return sft_idx | ((long)result << 16);
#ifdef RII_EX
		// Remember the actual filename for serialization.
		strcpy(ser_filenames[sft_idx], realname ); 
#endif
		return sft_idx;
	}
	else	// Bundled files
	{
		return ERROR_FILENOTFOUND;
	}
}

int DosOpen(char *fname, u16 mode, u16 attrib)
{
	int result;
	int	hndl;
	PSP *p;
	u8 	*filetab;
  
	/* test if mode is in range                     */
	//if ((mode & ~O_VALIDMASK) != 0)
	//	return DE_INVLDACC;

	/* get a free handle  */
	if ((result = get_free_hndl()) < 0)
		return result;
	hndl = result;

	result = DosOpenSft(fname, mode, attrib);
	if (result < NO_ERROR)
		return result;

	p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	filetab = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));
	filetab[hndl] = (u8)result;
	//return hndl | (result & 0xffff0000l);
	return hndl;
}

int DosMkTmp(char * pathname, u16 attr)
{
	/* create filename from current date and time */
	char *ptmp;
	u32 randvar;
	int rc;
	int loop;
#ifndef WP8
	struct timeval tv;
#endif

	ptmp = pathname + strlen(pathname);
	if (5 == dos_sda.DOSVerMajor) { /* clone some bad habit of MS DOS 5.0 only */
		if (ptmp == pathname || (ptmp[-1] != '\\' && ptmp[-1] != '/'))
			*ptmp++ = '\\';
	}
	ptmp[8] = '\0';

#ifdef WP8
	randvar = rand();
#else
	gettimeofday(&tv, NULL);
	randvar = ((u32)(tv.tv_sec) << 16) | tv.tv_usec;
#endif

	loop = 0;
	do {
		u32 tmp = randvar++;
		int i;
		for(i = 7; i >= 0; tmp >>= 4, i--)
			ptmp[i] = ((char)tmp & 0xf) + 'A';

		/* DOS versions: > 5: characters A - P < 5: hex digits */
		if (dos_sda.DOSVerMajor < 5)
			for (i = 0; i < 8; i++)
				ptmp[i] -= (ptmp[i] < 'A' + 10) ? '0' - 'A' : 10;

		/* only create new file -- 2001/09/22 ska*/
		rc = DosOpen(pathname, O_LEGACY | O_CREAT | O_RDWR, attr);
	} while (rc == ERROR_FILEEXISTS && loop++ < 0xfff);

	return rc;
}

int CloneHandle(int hndl)
{
	/* now get the system file table entry                          */
	SFT *sftp = idx_to_sft(get_sft_idx(hndl));

	if (NULL == sftp || (sftp->sft_mode & O_NOINHERIT))
		return ERROR_INV_HANDLE;
  
	/* now that we have the system file table entry, get the fnode  */
	/* index, and increment the count, so that we've effectively    */
	/* cloned the file.                                             */
	sftp->sft_count += 1;
	return NO_ERROR;
}

int DosDup(int Handle)
{
	int NewHandle;

	if ((NewHandle = get_free_hndl()) < 0)
		return NewHandle;

	if (DosForceDup(Handle, NewHandle) < 0)
		return ERROR_INV_HANDLE;
	else
		return NewHandle;
}

int DosForceDup(int OldHandle, int NewHandle)
{
	SFT *sftp;
	PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
	u8 	*filetab = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));

	/* Get the SFT block that contains the SFT                      */
	if (NULL == (sftp = idx_to_sft(get_sft_idx(OldHandle))))
		return ERROR_INV_HANDLE;

	/* now close the new handle if it's open                        */
	if (filetab[NewHandle] != 0xff)
	{
		int ret;

		if ((ret = DosClose(NewHandle)) != NO_ERROR)
			return ret;
	}

	/* If everything looks ok, bump it up.                          */
	filetab[NewHandle] = filetab[OldHandle];
	sftp->sft_count += 1;
	return NO_ERROR;
}

int DosCloseSft(int sft_idx, bool commitonly)
{
#if DEBUG
	LOGI("DosCloseSft(%d, %d)\n", sft_idx, commitonly);
#endif
	SFT *sftp = idx_to_sft(sft_idx);

	if (NULL == sftp)
		return ERROR_INV_HANDLE;

	lpCurSft = sftp;
	//if (sftp->sft_flags & SFT_FSHARED)
	//{
		/* LOGI("closing SFT %d = %p\n",sft_idx,sftp); */
	//	return network_redirector_fp(commitonly ? REM_FLUSH: REM_CLOSE, sftp);
	//}

	if ( (sftp->sft_mode & O_TESTOPEN) && WriteFileName[0] )
	{
		// Special flag handled, we are not to truncate the file.
		WriteFileName[0] = 0;
		sftp->sft_mode &= ~O_TESTOPEN;
	}

	if (sftp->sft_flags & SFT_FDEVICE)
	{
		//if (sftp->sft_dev->dh_attr & SFT_FOCRM)
		//{
			/* if Open/Close/RM bit in driver's attribute is set
			* then issue a Close request to the driver
			*/
		//	struct dhdr FAR *dev = sftp->sft_dev;
		//	if (BinaryCharIO(&dev, 0, MK_FP(0x0000, 0x0000), C_CLOSE) != SUCCESS)
		//		return DE_ACCESS;
		//}
		/* now just drop the count if a device */
		if (!commitonly)
			sftp->sft_count -= 1;
		return NO_ERROR;
	}

	/* else call file system handler                     */
	if ( commitonly )
	{
		if ( fatOK && sftp->sft_file != NULL)
			fflush( sftp->sft_file );
		return NO_ERROR;
	}
	sftp->sft_count -= 1;

	if ( 0 == sftp->sft_count && sftp->sft_file != NULL )
	{
		if ( fatOK )
			fclose( sftp->sft_file );
		sftp->sft_file = NULL;
	}
	return NO_ERROR;
}

int DosClose(int hndl)
{
	int ret;

	/* Get the SFT block that contains the SFT      */
	ret = DosCloseSft(get_sft_idx(hndl), 0);
	if (ret != ERROR_INV_HANDLE && ret != ERROR_ACCESS)
	{
		PSP *p = (PSP *)PHYS_PTR(dos_sda.cu_psp, 0);
		u8 	*filetab = (u8 *)PHYS_PTR((p->ps_filetab>>16),(p->ps_filetab&0xFFFF));
		filetab[hndl] = 0xff;
	}
	return ret;
}

int DosFlush(int hndl)
{
	return DosCloseSft(get_sft_idx(hndl), 1);
}

int SftSeek(int sft_idx, int new_pos, u16 mode)
{
	SFT *s = idx_to_sft(sft_idx);
#if DEBUG
	LOGI("SftSeek(%d, %d, %d)\n", sft_idx, new_pos, mode);
#endif
	if ( NULL == s )
		return ERROR_INV_HANDLE;
		
	/* Test for invalid mode                        */
	if (mode > 2)
		return ERROR_INVFUNCTION;

	lpCurSft = s;

	if ( (s->sft_mode & O_TESTOPEN) && WriteFileName[0] )
	{
		// Special flag handled, we are not to truncate the file.
		WriteFileName[0] = 0;
		s->sft_mode &= ~O_TESTOPEN;
	}
	
	/* Do special return for character devices      */
	if (s->sft_flags & SFT_FDEVICE)
	{
		s->sft_posit = 0;
	}
	else
	{
		if ( fatOK )
		{
			if ( s->sft_file != NULL )
			{
				int rc = fseek( s->sft_file, new_pos, mode );
				if ( rc >= 0 )	// Bug in fs_api fseek()!
				{
					s->sft_posit = ftell( s->sft_file );
					return s->sft_posit;
				}
				else
				{
#if DEBUG
					LOGI("fseek returned %d!\n", rc);
#endif
					return ERROR_SEEK;
				}
			}
			else
			{
#if DEBUG
				LOGI("fseek invalid handle!\n");
#endif
				return ERROR_INV_HANDLE;
			}
		}
		else
#if INTERNALEXE
		{
			switch ( mode )
			{
				case 0: s->sft_posit = new_pos; break;					// From start
				case 1: s->sft_posit += new_pos; break;					// From current
				case 2:	s->sft_posit = intfiles[(int)s->sft_file].size + new_pos;	// From end
			}
			if ( (int)s->sft_posit > intfiles[(int)s->sft_file].size )
				s->sft_posit = intfiles[(int)s->sft_file].size;
			return s->sft_posit;
		}
#else	
		return ERROR_INV_HANDLE;
#endif	
	}
	return NO_ERROR;
}

int DosSeek(int hndl, int new_pos, u16 mode)
{
	return SftSeek(get_sft_idx(hndl), new_pos, mode);
}

extern int ReadCon(int size, char *data);

int DosReadSft(int sft_idx, int n, void *bp)
{
	/* Get the SFT block that contains the SFT      */
	SFT *s = idx_to_sft(sft_idx);
#if DEBUG
	if ( (int)DS_DX_PTR == (int)bp )
		LOGI("DosReadSft(%d, %d, %04X:%04X)\n", sft_idx, n, REG_DS, REG_DX&0xFFFF);
	else
		LOGI("DosReadSft(%d, %d, %p)\n", sft_idx, n, bp);
#endif
	if ( NULL == s )
		return ERROR_INV_HANDLE;

	/* If for read and write-only or for write and read-only then exit */
	if ( s->sft_mode & O_WRONLY )
		return ERROR_ACCESS;
		
	/* Do a device transfer if device                   */
	if (s->sft_flags & SFT_FDEVICE)
	{
		// Read from CON device (Alien Legacy / YADRO hack)
		if (0 == sft_idx || 1 == sft_idx)
			return ReadCon(n, (char *)bp);
		// TODO!
		return NO_ERROR;
	}

	if ( s->sft_file != NULL )
	{
		if ( fatOK )
		{
			n = fread( bp, 1, n, s->sft_file );
			s->sft_posit += n;
			return n;
		}
		else
			return ERROR_INV_HANDLE;
	}
	else
		return ERROR_INV_HANDLE;
}

int DosRead(int hndl, int n, void *bp)
{
	return DosReadSft(get_sft_idx(hndl), n, bp );
}

extern char	ShowHDDLed;

int DosWriteSft(int sft_idx, int n, void *bp)
{
	/* Get the SFT block that contains the SFT      */

	SFT *s = idx_to_sft(sft_idx);
	if ( NULL == s )
		return ERROR_INV_HANDLE;

	/* If for read and write-only or for write and read-only then exit */
	if ( O_RDONLY == (s->sft_mode&3) )
		return ERROR_ACCESS;
		
	/* Do a device transfer if device                   */
	if (s->sft_flags & SFT_FDEVICE)
	{
		return DeviceWriteSft(s, n, (u8 *)bp);
	}
	ShowHDDLed = 1;
	if ( s->sft_file != NULL )
	{
		if ( fatOK )
		{
			if ( (s->sft_mode & O_TESTOPEN) && WriteFileName[0] )
			{
				// Special flag is on, so the first operation on this write-only file is actual write.
				// We need to truncate the file before writing to it.
				fclose( s->sft_file );
				s->sft_file = fopen( WriteFileName, "w" );
				if ( NULL == s->sft_file )
					return 0;
				s->sft_mode &= ~O_TESTOPEN;	// Special flag handled, file truncation done.
				WriteFileName[0] = 0;
			}
			n = fwrite( bp, 1, n, s->sft_file );
			s->sft_posit += n;
			return n;
		}
		else
#if INTERNALEXE
		{
			if ( n > intfiles[(int)s->sft_file].size - (int)s->sft_posit )
				n = intfiles[(int)s->sft_file].size - (int)s->sft_posit;
			//memcpy( bp, intfiles[(int)s->sft_file].data+s->sft_posit, n );
			s->sft_posit += n;
			return n;
		}
#else	
		return ERROR_INV_HANDLE;
#endif	
	}
	else
		return ERROR_INV_HANDLE;
}

int DosWrite(int hndl, int n, void *bp)
{
	return DosWriteSft(get_sft_idx(hndl), n, bp );
}

int SftGetFSize(int sft_idx)
{
#if DEBUG
	LOGI("SftGetFSize(%d)\n", sft_idx);
#endif
	SFT *s = idx_to_sft(sft_idx);
	/* Get the SFT block that contains the SFT      */
	if ( NULL == s )
		return ERROR_INV_HANDLE;
	if ( fatOK )
	{
		if ( s->sft_file != NULL )
		{
			s->sft_posit = ftell( s->sft_file );
			if ( 0 == fseek( s->sft_file, 0, SEEK_END ) )
			{
				s->sft_size = ftell( s->sft_file );
				fseek( s->sft_file, s->sft_posit, SEEK_SET );
			}
			return s->sft_size;
		}
		return ERROR_INV_HANDLE;
	}
	else
	{
#if INTERNALEXE
		return s->sft_size = intfiles[(int)s->sft_file].size;
#else
		return 0;
#endif
	}
}

int DosGetFtime(int sft_idx, u16 *date, u16 *time)
{
	*date = 0;	// TODO!
	*time = 0;	// TODO!
	return NO_ERROR;
}

#define DELETED         '\x5'    	/* if first char, delete file   */

//-------------------------------------------------------------------------
// Clean up the file name from "4DOS    COM" format to "4DOS.COM",0 format.
// TODO! Allow blanks inside name & ext
//-------------------------------------------------------------------------
void Name83ToSz(char *to, const char *from)
{
	int i = 0;
	int	o = 0;
	
	while ( i < 8 && from[i] != ' ' )
		to[o++] = from[i++];
	if ( from[8] != ' ' )
	{
		// File name has an extension
		to[o++] = '.';
		for ( i = 8; i < 11; i++ )
			if ( from[i] != ' ' )
				to[o++] = from[i];
	}
	to[o] = 0;
}

static void NameSzTo83(char *to, const char *from)
{
	int i = 0;
	int	o = 0;
	
	/* Base part */
	while ( from[i] && o < 8 )
	{
		if ( '*' == from[i] )
			to[o++] = '?';
		else if ( '.' == from[i] )
			break;
		else
			to[o++] = (from[i] >= 'a' && from[i] <= 'z') ? from[i++] - 32 : from[i++];
	}
	while ( o < 8 )
		to[o++] = ' ';
	while ( '*' == from[i] )
		i++;
	if ( '.' == from[i] )
		i++;
	/* Extension */
	while ( from[i] && o < 11 )
	{
		if ( '*' == from[i] )
			to[o++] = '?';
		else if ( '.' == from[i] )
			break;
		else
			to[o++] = (from[i] >= 'a' && from[i] <= 'z') ? from[i++] - 32 : from[i++];
	}
	while ( o < 11 )
		to[o++] = ' ';
	//LOGI("SzTo83: '%s' => '%11.11s'\n", from, to );
}

// Compare s1 (DOS.SDA filename pattern) with s2 (dirent.d_name), with wildcards.
// 
bool fcmp_wild(const char * s1, const char * s2, unsigned n)
{
	char	name83[8+3+1];
	NameSzTo83(name83, s2);
	s2 = name83;

	for ( ; n--; ++s1, ++s2)
		if (*s1 != '?' && *s1 != *s2)
		{
			if ( *s2 < 'a' || *s2 > 'z' || *s1 != *s2 - 32 )
				return 0;
		}
	return 1;
}

int dir_open(const char *dirname, bool split)
{
	int 	i;
#ifdef WP8
	struct	stat statbuf;
#else
	DIR		*pdir;
	struct	dirent *paent;
#endif
	char	found;

	strcpy( finddir, ('D' == dirname[0]) ? HostCD : aExtDir );
#ifdef WP8
	strcat( finddir, "\\" );	// Start search/split from the root directory
#else
	strcat( finddir, "/" );		// Start search/split from the root directory
#endif
	if ( dta )
		dta->dm_entry = 0;

#if DEBUG
	LOGI("dir_open(%s, %d), finddir='%s'\n", dirname, !!split, finddir);
#endif

	dirname += 2;               /* Skip "C:" */
	//fcbname = fnp->f_dmp->dm_name_pat;
	while (*dirname)
	{
		/* skip the path seperator                              */
		++dirname;

		/* don't continue if we're at the end: this check is    */
		/* for root directories, the only fully-qualified path  */
		/* names that end in a \                                */
		if (0 == *dirname)
			break;

		/* Convert the name into an absolute name for           */
		/* comparison...                                        */
		for ( i=0; i < 8+3+1; i++, dirname++ )
		{
			char c = *dirname;
			if ( 0 == c || '\\' == c )
			{
				findname[i] = 0;
				break;
			}
			findname[i] = c;
		}

		/* do not continue if we split the filename off and are */
		/* at the end                                           */
		if (split && 0 == *dirname)
			break;

		/* Now search through the directory to  */
		/* find the entry...                    */
		strcat( finddir, findname );
#ifdef WP8
		if (stat(finddir, &statbuf) != 0)
			return ERROR_PATHNOTFOUND;
		strcat( finddir, "\\" );
#else
		strcat( finddir, "/" );
		pdir = opendir( finddir );
		if ( NULL == pdir )
		{
			// Did not find this part of the path, so determine if there is a case problem.
			finddir[strlen(finddir)-strlen(findname)-1] = 0;	// Remove the findname
			pdir = opendir(finddir);
			if ( NULL == pdir )
			{
				// This should not happen!
				strcat( finddir, findname );
				strcat( finddir, "/" );
				return ERROR_PATHNOTFOUND;
			}
			found = 0;
			while ( (paent = readdir( pdir )) )
			{
				char *p1 = findname;
				char *p2 = paent->d_name;
				while (*p1 && *p2 && (*p1 == *p2 || (*p2 >= 'a' && *p2 <= 'z' && *p1 == *p2 - 32)))
				{
					p1++;
					p2++;
				}
				if (0 == *p1 && 0 == *p2)
				{
					// Found a directory but with a different case!
					strcat( finddir, paent->d_name );
					strcat( finddir, "/" );
					found = 1;
					break;
				}
			}
			if (!found)
			{
				closedir( pdir );
				return ERROR_PATHNOTFOUND;
			}
		}
		closedir(pdir);
#endif
	}
	// Now finddir contains the directory portion and findname the filename portion of input path.
	if ( dta )
	{
		NameSzTo83( dta->dm_name_pat, findname);
#if DEBUG
		LOGI("dir_open dta->dm_name_pat='%s', findname='%s'\n", dta->dm_name_pat, findname);
#endif
	}
	return NO_ERROR;
}

/*                                                                      */
/* split a path into it's component directory and file name             */
/*                                                                      */
int split_path(const char *path)
{
	/* check if the path ends in a backslash */
	if ('\\' == path[strlen(path) - 1])
		return ERROR_PATHNOTFOUND;

	/* Generate the path into 'finddir' */
	return dir_open(path, 1);
}

/* checks whether directory part of path exists */
bool dir_exists(char * path)
{
	return split_path(path) == NO_ERROR;
}

int dos_findnext(void)
{
	int i;
#ifdef WP8
	struct _finddata_t c_file;
	struct tm newtime;
	intptr_t hFile;
	char name[256];
#if DEBUG
	LOGI("dos_findnext(%s)\n", finddir);
#endif
	strcpy(name, finddir);
	//strcat(name, "\\");
	Name83ToSz(name+strlen(name), dta->dm_name_pat);
#if DEBUG
	LOGI("_findfirst('%s')\n", name);
#endif
	hFile = _findfirst(name, &c_file);
#if DEBUG
	LOGI("_findfirst retval=%p\n", hFile);
#endif
	if (hFile == -1L)
		return ERROR_NO_MORE;
	// Loop through the directory
#if DEBUG
	LOGI("dm_entry=%d\n", (int)dta->dm_entry);
#endif
	for ( i = 0; i < (int)dta->dm_entry; i++ )
	{
		if (_findnext( hFile, &c_file ) != 0)
		{
			_findclose( hFile );
			return ERROR_NO_MORE;
		}
	}
	// This is the correct item in the directory we want to return.
	++(dta->dm_entry);
	dta->dm_size = c_file.size;
	dta->dm_attr_fnd = c_file.attrib;
	//--------
	//	Bitfields for file time:
	//	Bit(s)  Description     (Table 01665)
	//	15-11  hours (0-23)
	//	10-5   minutes
	//	4-0    seconds/2
	//
	//	Bitfields for file date:
	//	Bit(s)  Description     (Table 01666)
	//	15-9   year - 1980
	//	8-5    month
	//	4-0    day
	//--------
	localtime_s(&newtime, &c_file.time_write);
	dta->dm_time = (newtime.tm_hour<<11)|(newtime.tm_min<<5)|(newtime.tm_sec>>1);
	dta->dm_date = ((newtime.tm_year-80)<<9)|((newtime.tm_mon+1)<<5)|newtime.tm_mday;
	for (i=0; i < FNAME_SIZE + 1 + FEXT_SIZE; i++)
	{
		dta->dm_name[i] = c_file.name[i];
	}
#if DEBUG
	LOGI("dm_name=%s, dm_size=%d, time_write=%d\n", dta->dm_name, dta->dm_size, c_file.time_write );
#endif
	_findclose( hFile );
	return NO_ERROR;
#else
	DIR 	*pdir;
	//DIRENT	*pent = &(dos_sda.SearchDir);	// Directory entry in DOS format (output)
	struct	dirent *paent;	// Directory entry in Android (Linux) format (input)
	struct	stat statbuf;
	struct	tm *tm;
	char	realname[256];

#if DEBUG
	LOGI("dos_findnext(%s)\n", finddir);
#endif
	pdir = opendir(finddir);
	if ( NULL == pdir )
	{
#if DEBUG
		LOGI("ERROR_NO_MORE 1\n");
#endif
		return ERROR_NO_MORE;
	}

	/* Search through the directory to find the entry, but do a     */
	/* seek first.                                                  */
	/* Loop through the directory                                   */
#if DEBUG
	LOGI("dm_entry=%d\n", (int)dta->dm_entry);
#endif
	for ( i = 0; i < (int)dta->dm_entry; i++ )
	{
		if ( NULL == readdir( pdir ) )
		{
			closedir( pdir );
#if DEBUG
			LOGI("ERROR_NO_MORE 2\n");
#endif
			return ERROR_NO_MORE;
		}
	}

	while ( (paent = readdir( pdir )) )
	{
		++(dta->dm_entry);

		// Remember the directory entry data. Used in FCB functions.
//		memcpy( pent, pdir->currentEntry.entryData, sizeof(DIRENT) );
		
		u8 attr = paent->d_type == DT_DIR ? ATTRIB_DIR : 0;
		if (paent->d_ino != 0)	// inode == 0 if file is deleted
		{
#if DEBUG
			//LOGI("Look for %s in %s (%d)\n", dta->dm_name_pat, paent->d_name, paent->d_type );
#endif
			if (fcmp_wild(dta->dm_name_pat, paent->d_name, FNAME_SIZE + FEXT_SIZE))
			{
				/*
					MSD Command.com uses FCB FN 11 & 12 with attrib set to 0x16.
					Bits 0x21 seem to get set some where in MSD so Rd and Arc
					files are returned. 
					RdOnly + Archive bits are ignored
				*/
				strcpy( realname, finddir );
				strcat( realname, "/" );
				strcat( realname, paent->d_name );
				if (stat( realname, &statbuf) == -1)
					continue;
				if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
					attr = ATTRIB_DIR;
				/* Test the attribute as the final step */
				/* It's either a special volume label search or an                 */
				/* attribute inclusive search. The attribute inclusive search      */
				/* can also find volume labels if you set e.g. D_DIR|D_VOLUME      */
				u8 attr_srch = dta->dm_attr_srch & ~(ATTRIB_RO | ATTRIB_ARCH);
				if (attr_srch == ATTRIB_VOL)
				{
					if (!(attr & ATTRIB_VOL))
						continue;
				}
				else if (~attr_srch & (ATTRIB_DIR | ATTRIB_SYS | ATTRIB_HID | ATTRIB_VOL) & attr)
					continue;
				dta->dm_attr_fnd = attr;
				//--------
				//	Bitfields for file time:
				//	Bit(s)  Description     (Table 01665)
				//	15-11  hours (0-23)
				//	10-5   minutes
				//	4-0    seconds/2
				//
				//	Bitfields for file date:
				//	Bit(s)  Description     (Table 01666)
				//	15-9   year - 1980
				//	8-5    month
				//	4-0    day
				//--------
				tm = localtime(&statbuf.st_mtime);
				dta->dm_time = (tm->tm_hour<<11)|(tm->tm_min<<5)|(tm->tm_sec>>1);
				dta->dm_date = ((tm->tm_year-80)<<9)|((tm->tm_mon+1)<<5)|tm->tm_mday;
				dta->dm_size = statbuf.st_size;
				for (i=0; i < FNAME_SIZE + 1 + FEXT_SIZE; i++)
				{
					dta->dm_name[i] = toupper((u8)(paent->d_name[i]));
				}
#if DEBUG
				//LOGI("dm_name=%s, dm_size=%d\n", dta->dm_name, dta->dm_size );
#endif
				closedir(pdir);
				return NO_ERROR;
			}
		}
	}
	closedir( pdir );
	return ERROR_NO_MORE;
#endif
}

int dos_findfirst(int attr, char *name)
{

	/* The findfirst/findnext calls are probably the worst of the   */
	/* DOS calls. They must work somewhat on the fly (i.e. - open   */
	/* but never close). The near fnodes now work this way. Every   */
	/* time a directory is searched, we will initialize the DOS     */
	/* dirmatch structure and then for every find, we will open the */
	/* current directory, do a seek and read.                       */

#if DEBUG
	LOGI("dos_findfirst(%s)\n", name);
#endif
	/* first: findfirst("D:\\") returns DE_NFILES */
	if (0 == name[3])
		return ERROR_NO_MORE;
	
	/* Now open this directory so that we can read the      */
	/* fnode entry and do a match on it.                    */
	if (split_path(name) < 0)
		return ERROR_PATHNOTFOUND;

	/* Now we have finddir = the directory in which to search, and	*/
	/* findname = the file in SZ format to search for.				*/
	
	/* Special handling - the volume id is only in the root         */
	/* directory and only searched for once.  So we need to open    */
	/* the root and return only the first entry that contains the   */
	/* volume id bit set (while ignoring LFN entries).              */
	/* RBIL: ignore ReaDONLY and ARCHIVE bits but DEVICE ignored too*/
	/* For compatibility with bad search requests, only treat as    */
	/*   volume search if only volume bit set, else ignore it.      */
	if ((attr & ~(ATTRIB_RO | ATTRIB_ARCH)) == ATTRIB_VOL)
		/* if ONLY label wanted return that we don't have a volume label. */
		return ERROR_NO_MORE;

	/* Now further initialize the dirmatch structure.       */
	dta->dm_drive = name[0] - 'A';
	dta->dm_attr_srch = attr;

	return dos_findnext();
}

int DosFindFirst(const char *fname, u16 attr)
{
	// Format of FindFirst data block:
	//
	// Offset  Size    Description     (Table 01626)
	// ---PC-DOS 3.10, PC-DOS 4.01, MS-DOS 3.2/3.3/5.0---
	// 00h    BYTE    drive letter (bits 0-6), remote if bit 7 set
	// 01h 11 BYTEs   search template
	// 0Ch    BYTE    search attributes
	// 0Dh    WORD    entry count within directory
	// 0Fh    WORD    cluster number of start of parent directory
	// 11h  4 BYTEs   reserved
	// 15h    BYTE    attribute of file found
	// 16h    WORD    file time (see #01665 at AX=5700h)
	// 18h    WORD    file date (see #01666 at AX=5700h)
	// 1Ah    DWORD   file size
	// 1Eh 13 BYTEs   ASCIZ filename+extension
	//
	int		rc;
	
	rc = truename(fname, PriPathName, 1);
	if (rc < 0)
		return rc;

#if DEBUG
	LOGI("dos_sda.dat=%08X\n", dos_sda.dta);
#endif
	dta = ((DIRMATCH *)((dos_sda.dta&0xFFFF)+(((dos_sda.dta>>16)&0xFFFF)<<4)+(int)INTVectors));
	memset( dta, 0, sizeof(DIRMATCH) );
	return dos_findfirst( attr, PriPathName );
}

int DosFindNext()
{
	dta = ((DIRMATCH *)((dos_sda.dta&0xFFFF)+(((dos_sda.dta>>16)&0xFFFF)<<4)+(int)INTVectors));

	/* findnext will always fail on a volume id search or device name */
	if ((dta->dm_attr_srch & ~(ATTRIB_RO | ATTRIB_ARCH)) == ATTRIB_VOL || dta->dm_entry == 0xffff)
		return ERROR_NO_MORE;
	return dos_findnext();
}

int DosGetFAttr(char *fname)
{
	DIRMATCH	tmpDTA;
	int		rc;
	
	rc = truename(fname, PriPathName, 1);
	if (rc < 0)
		return rc;

	dta = &tmpDTA;
	memset( dta, 0, sizeof(DIRMATCH) );
	
	if ( fatOK )
	{
		rc = dos_findfirst( ATTRIB_RO|ATTRIB_HID|ATTRIB_SYS|ATTRIB_DIR|ATTRIB_ARCH, PriPathName );
		if ( NO_ERROR == rc )
			rc = dta->dm_attr_fnd & (~ATTRIB_ARCH);	// Elite save games fail if archive bit returned!!
		else if ( ERROR_NO_MORE == rc )
			rc = ERROR_FILENOTFOUND;
	}
	else
		rc = 0;
	dta = NULL;
	return rc;
}

void DosGetFreeDisk(int *sectorsPerCluster, int *freeClusters, int *bytesPerSector, int *totalClusters)
{
	//u32 total, used, free;
#if !(defined(WP8) || defined(IOS))
	struct statfs buf;
#endif
	*sectorsPerCluster = 64;
	*bytesPerSector = 512;
#if !(defined(WP8) || defined(IOS))
	if (0 == statfs(aExtDir, &buf))
	{
		// Nexus 7, Android 4.1, Java StatFs(): avail=7070377 (0x6BE2A9), blocksize=4096 (0x1000), total=7228639 (0x6E4CDF)
		// buf: [0] = f_type = 0xEF53 = EXT3_SUPER_MAGIC
		//		[4] = f_bsize = 0x1000 = 4096
		//		[8] = f_blocks = 0x00000000006E4CDF
		//		[16] = f_bfree = 0x00000000006BE2A9
		//		[24] = f_bavail = 0x00000000006BE2A9
		if (freeClusters)
		{
			*freeClusters = (int)(buf.f_bavail * (u64)buf.f_bsize / (u64)(64*512));
			if (*freeClusters > 65535)
				*freeClusters = 65535;	// Only show at most 2 GB free
#if DEBUG
			LOGI("free=%d clusters (%d MB)\n", *freeClusters, *freeClusters / 32);
#endif
		}
		*totalClusters = (int)(buf.f_blocks * (u64)buf.f_bsize / (u64)(64*512));
		if (*totalClusters > 65535)
			*totalClusters = 65535;	// Only show at most 2 GB total
#if DEBUG
		LOGI("total=%d clusters (%d MB)\n", *totalClusters, *totalClusters / 32);
#endif
	}
	else
#endif
	{
#if DEBUG
		LOGI("statfs failed!\n");
#endif
		if ( freeClusters )
			*freeClusters = 1000;
		*totalClusters = 2000;
	}
}

// Create the DOS Disk Parameter Block for C: drive into 
void BuildDPB(DPB *dpb)
{
	dpb->dpb_secsize = 512;
	dpb->dpb_clsmask = 0x3F;
	dpb->dpb_shftcnt = 0;
	dpb->dpb_fatstrt = 0x8A;
	dpb->dpb_fats = 1;
	dpb->dpb_data = 0x280;
	dpb->dpb_size = 0xEA77;
	dpb->dpb_dirstrt = 0;
	dpb->dpb_dirents = (dpb->dpb_data-dpb->dpb_dirstrt)*(dpb->dpb_secsize>>6);	// For SYSINFO sanity check
	dpb->dpb_fatsize = (dpb->dpb_data-dpb->dpb_fatstrt)/dpb->dpb_fats;	// For SYSINFO sanity check
	dpb->dpb_device = 0xFFFFFFFF;	/* pointer to device header     */
	dpb->dpb_mdb = 0xF8;	/* media descr. byte            */
	dpb->dpb_flags = 0; /* -1 = force MEDIA CHK         */
	dpb->dpb_next = 0xFFFFFFFF;	/* next dpb in chain            */
	dpb->dpb_cluster = 2;
	dpb->dpb_nfreeclst = 0xD925;
}

void bpb_to_dpb(BPB *bpbp, DPB *dpbp)
{
  u32	size;
  u16	shftcnt;
  BPB	sbpb;

  memcpy(&sbpb, bpbp, sizeof(sbpb));
  for (shftcnt = 0; (sbpb.bpb_nsector >> shftcnt) > 1; shftcnt++)
    ;
  dpbp->dpb_shftcnt = shftcnt;

  dpbp->dpb_mdb = sbpb.bpb_mdesc;
  dpbp->dpb_secsize = sbpb.bpb_nbyte;
  dpbp->dpb_clsmask = sbpb.bpb_nsector - 1;
  dpbp->dpb_fatstrt = sbpb.bpb_nreserved;
  dpbp->dpb_fats = sbpb.bpb_nfat;
  dpbp->dpb_dirents = sbpb.bpb_ndirent;
  size = sbpb.bpb_nsize == 0 ? sbpb.bpb_huge : (u32) sbpb.bpb_nsize;
  dpbp->dpb_fatsize = sbpb.bpb_nfsect;
  dpbp->dpb_dirstrt = dpbp->dpb_fatstrt + dpbp->dpb_fats * dpbp->dpb_fatsize;
  dpbp->dpb_data = dpbp->dpb_dirstrt
      + (dpbp->dpb_dirents + dpbp->dpb_secsize/DIRENT_SIZE - 1) /
          (dpbp->dpb_secsize/DIRENT_SIZE);
  dpbp->dpb_size = (u16)((size - dpbp->dpb_data) >> shftcnt) + 1;
  { /* Make sure the number of FAT sectors is actually enough to hold that */
    /* many clusters. Otherwise back the number of clusters down (LG & AB) */
    unsigned fatsiz;
    u32 tmp = dpbp->dpb_fatsize * (u32)(dpbp->dpb_secsize / 2);/* entries/2 */
    if (tmp >= 0x10000UL)
      goto ckok;
    fatsiz = (unsigned) tmp;
    if (dpbp->dpb_size > FAT_MAGIC) {/* FAT16 */
      if (fatsiz <= FAT_MAGIC)       /* FAT12 - let it pass through rather */
        goto ckok;                   /* than lose data correcting FAT type */
    } else {                         /* FAT12 */
      if (fatsiz >= 0x4000)
        goto ckok;
      fatsiz = fatsiz * 4 / 3;
    }
    if (dpbp->dpb_size >= fatsiz)    /* FAT too short */
      dpbp->dpb_size = fatsiz - 1;   /* - 2 reserved entries + 1 */
ckok:;
  }
  dpbp->dpb_flags = 0;
  dpbp->dpb_cluster = UNKNCLUSTER;
  /* number of free clusters */
  dpbp->dpb_nfreeclst = UNKNCLSTFREE;
}

int DosChangeDir(char *dir)
{
	int result;

	if ( truename(dir, PriPathName, 0) < 0 )
		return ERROR_PATHNOTFOUND;

	if ( strlen(PriPathName) >= sizeof(cwd) )
		return ERROR_PATHNOTFOUND;

	result = check_host_path(PriPathName);
	if ( result < 0 )
		return result;
	
	strcpy( cwd, PriPathName );

    if (PriPathName[7] == 0)
		cwd[8] = 0;
	if ('D' == PriPathName[0])
		strcpy( dcwd, cwd);
	else
		strcpy( ccwd, cwd);

	return NO_ERROR;
}

int DosGetCuDir(int drive, char *path)
{
	if ( ! (0 == drive || 3 == drive || (HaveCD && 4 == drive)) )
		return ERROR_INV_DRIVE;

	if (truename((4 == drive) ? "D:" : "C:", PriPathName, 0) < 0)
		return ERROR_INV_DRIVE;

	strcpy(path, PriPathName + 3);	// Skip the initial "C:\"
	return NO_ERROR;
}

int DosGetCuDrive()
{
	if ('D' == cwd[0])
		return 3;		// 3 == 'D:'
	return 2;			// 2 == 'C:'
}

// drive = new default drive (00h = A:, 01h = B:, etc)
void DosSetCuDrive(int drive)
{
	if (HaveCD && 3 == drive)
		strcpy( cwd, dcwd );
	else
		strcpy( cwd, ccwd );
}

int DosDelete(const char *fname)
{
	int result;
	char	realname[256];
	
	result = truename( fname, PriPathName, 0);
	if ( result < 0 )
		return result;
		
	// Then convert the input name to Unix format.
	UnixName( realname, PriPathName, 2 );

	if ( remove(realname) )
		return ERROR_FILENOTFOUND;
	return NO_ERROR;
}

int DosRename(const char *fname, const char *tname)
{
	int result;
	char FromName[128];
	char ToName[128];

	result = truename( fname, PriPathName, 0);
	if ( result < 0 )
		return result;
		
	UnixName( FromName, PriPathName, 2);
	
	result = truename( tname, PriPathName, 0);
	if ( result < 0 )
		return result;

	UnixName( ToName, PriPathName, 1);

	if ( rename(FromName, ToName) )
		return ERROR_FILENOTFOUND;
	return NO_ERROR;
}

int DosMkRmdir(const char *fname, int ah)
{
	int result;
	char	realname[256];

	result = truename(fname, PriPathName, 0);
	if (result < 0)
		return result;

	// Then convert the input name to Unix format.
	UnixName( realname, PriPathName, 2 );

	if ( 0x39 == ah )
#ifdef WP8
		result = mkdir(realname);
#else
		result = mkdir(realname, S_IRWXU|S_IRWXG|S_IRWXO);
#endif
	else
		result = remove(realname);
	if ( 0 == result )
		return NO_ERROR;
	return ERROR_PATHNOTFOUND;
}

void InitFiles(const char *fdir, const char *edir)
{
	aDataDir[0] = 0;
	strncat(aDataDir, fdir, 126);

	aExtDir[0] = 0;
	strncat(aExtDir, edir, 126);

	dta = NULL;
	strcpy( cwd, "C:\\" );
	if ( idx_to_sft_(0) >= 0 )
		DeviceOpenSft("CON", lpCurSft);		// STDIN
	if ( idx_to_sft_(1) >= 0 )
		DeviceOpenSft("CON", lpCurSft);		// STDOUT
	if ( idx_to_sft_(2) >= 0 )
		DeviceOpenSft("CON", lpCurSft);		// STDERR
		
/*
	for ( int i = 2; i < (int)(sizeof(files)/sizeof(DOSFILE)); i++ )
	{
		if ( (int)(files[i].file) > 0 )
			fclose(files[i].file);
		files[i].file = NULL;
		files[i].handle = 0;
	}
*/
}

// Convert an unix-format directory/file to DOS C:\dir\file.
// Used when launching a com/exe file as an input parameter.
// The input 'in' may contain a CD-ROM drive emulation directory separated by ';' character.
//
int DosName(char *out, char *in)
{
	char *ptr;
	char tmp[MAXPATHLEN];
	char intmp[MAXPATHLEN];		// Temporary save for 'in' parameter if we need to split it
	int i;
#ifdef WP8
	const char sep = '\\';
#else
	const char sep = '/';
#endif

	// Check if the input contains the CD-ROM directory at the end, separated by ';'.
	ptr = strchr(in, ';');
	if (ptr)
	{
		// Split the CD-ROM directory away from the input game name,
		// and copy the plain game name to 'intmp'.
		intmp[0] = 0;
		strncat(intmp, in, ptr - in);
		// Skip over possible leading blanks
		do {
			ptr++;
		} while (*ptr == ' ');
		// Save the CD-ROM host directory to HostCD
		if (*ptr)
		{
			HaveCD = true;
			strcpy( HostCD, ptr );
			// Strip away the trailing separator, if any
			for (i = strlen(HostCD); i > 0 && HostCD[i-1] == sep; i--)
				HostCD[i] = 0;
		}
	}
	else
	{
		// Copy the input string to our local variable, since we may need to modify it.
		intmp[0] = 0;
		strncat(intmp, in, MAXPATHLEN-1);
	}
	in = intmp;
	
	//printf("DosName('%s')\n", in);
	if (memcmp(in, aDataDir, strlen(aDataDir)))
	{
		// If the input path does not begin with our data dir,
		// try to strip two last parts away from the input
		// path and use that as the C:\ root directory.
		for (ptr = in; *ptr; ptr++);
		while (ptr > in && *ptr != sep)
			ptr--;
#if 0
		ptr--;	// Skip over the last separator between filename and path
		while (ptr > in && *ptr != sep)
			ptr--;
#endif
		if (ptr > in)
		{
			*ptr = 0;
			strcpy( aDataDir, in );
			strcpy( aExtDir, in );
			*ptr = sep;
		}
		//printf("aDataDir='%s'!\n", aDataDir);
	}
	in += strlen(aDataDir);
#if 0
	// Find the last / and use everything before that as a current working dir.
	for (ptr = in; *ptr; ptr++);
	while (ptr > in && *ptr != sep)
		ptr--;
	if (ptr > in)
	{
		for (i=0; in < ptr; i++)
		{
			tmp[i] = *in == sep ? '\\' : *in; 
			in++;
		}
		tmp[i] = 0;
	}
	else
		strcpy(tmp, "\\");	// Go to root dir
	//printf("cd '%s'\n", tmp);
	if (DosChangeDir(tmp) != NO_ERROR)
		return ERROR_PATHNOTFOUND;
#endif
	in++;	// Skip over the path separator
	// Copy the actual executable name to the output parameter.
	while ( *in )
		*out++ = *in++;
	*out = 0;
	return NO_ERROR;
}

#ifdef RII_EX

// Return the worst-case size of the file serialization data.
int files_serialize_size()
{
	return sizeof(PriPathName) + sizeof(cwd) + sizeof(ccwd) + sizeof(dcwd) + sizeof(finddir) + sizeof(findname) +
		sizeof(WriteFileName) + sizeof(aDataDir) + sizeof(aExtDir) + sizeof(ser_filenames);
}

// Serialize the files into data + offset
int files_serialize(u8 *data, int offset)
{
	memcpy(data + offset, PriPathName, sizeof(PriPathName));
	offset += sizeof(PriPathName);
	memcpy(data + offset, cwd, sizeof(cwd));
	offset += sizeof(cwd);
	memcpy(data + offset, ccwd, sizeof(ccwd));
	offset += sizeof(ccwd);
	memcpy(data + offset, dcwd, sizeof(dcwd));
	offset += sizeof(dcwd);
	memcpy(data + offset, finddir, sizeof(finddir));
	offset += sizeof(finddir);
	memcpy(data + offset, findname, sizeof(findname));
	offset += sizeof(findname);
	memcpy(data + offset, WriteFileName, sizeof(WriteFileName));
	offset += sizeof(WriteFileName);
	memcpy(data + offset, aDataDir, sizeof(aDataDir));
	offset += sizeof(aDataDir);
	memcpy(data + offset, aExtDir, sizeof(aExtDir));
	offset += sizeof(aExtDir);
	memcpy(data + offset, ser_filenames, sizeof(ser_filenames));
	offset += sizeof(ser_filenames);

	return offset;
}

// Unserialize the files from data + offset.
// Note! Main memory must have been unserialized before this call!
int files_unserialize(u8 *data, int offset)
{
	int i;

	// First copy the memory variables.
	memcpy(PriPathName, data + offset, sizeof(PriPathName));
	offset += sizeof(PriPathName);
	memcpy(cwd, data + offset, sizeof(cwd));
	offset += sizeof(cwd);
	memcpy(ccwd, data + offset, sizeof(ccwd));
	offset += sizeof(ccwd);
	memcpy(dcwd, data + offset, sizeof(dcwd));
	offset += sizeof(dcwd);
	memcpy(finddir, data + offset, sizeof(finddir));
	offset += sizeof(finddir);
	memcpy(findname, data + offset, sizeof(findname));
	offset += sizeof(findname);
	memcpy(WriteFileName, data + offset, sizeof(WriteFileName));
	offset += sizeof(WriteFileName);
	memcpy(aDataDir, data + offset, sizeof(aDataDir));
	offset += sizeof(aDataDir);
	memcpy(aExtDir, data + offset, sizeof(aExtDir));
	offset += sizeof(aExtDir);
	memcpy(ser_filenames, data + offset, sizeof(ser_filenames));
	offset += sizeof(ser_filenames);
	// Then attempt to re-open all the files that were open when the serialization was done.
    for (i = 0; i < dos_sft.sftt_count; i++)
    {
		SFT *s = idx_to_sft(i);
		if (s && 0 == (s->sft_flags & SFT_FDEVICE) && ser_filenames[i][0])
		{
			// This file is supposed to be open.
			if (O_RDONLY == (s->sft_mode&3))
				s->sft_file = fopen( ser_filenames[i], "r" );	// Open for Read only
			else
				s->sft_file = fopen( ser_filenames[i], "r+" );	// Open for Read/Write
			// If we fail to open the file, mark that the file is not open.
			if (NULL == s->sft_file)
				s->sft_count = 0;
			// Else seek the file to the correct position.
			else if (s->sft_posit)
				fseek( s->sft_file, s->sft_posit, 0 );
		}
    }

	return offset;
}

// Since unserialize re-opens the files, we need to close the currently open DOS
// files before we start unserializing!
void files_close_before_unserialize()
{
	int i;
    for (i = 0; i < dos_sft.sftt_count; i++)
    {
		SFT *s = idx_to_sft(i);
		if (s && 0 == (s->sft_flags & SFT_FDEVICE) && s->sft_file)
		{
			fclose(s->sft_file);
			s->sft_file = NULL;
		}
	}
}

#endif
