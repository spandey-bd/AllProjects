//=============================================================================
// DOS_kernel.c
//
// This file contains the following routines:
//	ShartShell()	Loads the initial executable from disk to memory and
//					prepares it for running. By default the loaded executable
//					is 4DOS.COM command shell, but an alternate program to run
//					can be given as a parameter to the routine. Returns 0
//					for success and -1 for error. If succesfull, all x86
//					registers are set to proper values for run_core() to
//					start running the loaded executable.
//	InitKernel()	Initializes the DOS kernel memory structures.
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
//#include <fat.h>

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

//#define FIFO_SB 9

char	Shell[256];
bool	fatOK = true;

const char *shells[] = { "C:\\4DOS\\4DOS.COM", "C:\\4DOS.COM" };
#define	SHELLCNT	2

extern void UnixName(char *out, char *in, int mode);	// In DOS_File.c
extern void SendError(char *fmt, char *param);			// in Pax86retro.c

// Attempt to parse the input BAT file, replacing the input contents with the
// actual executable we need to launch. A quick hack for Litl Divil.
// Example DIVIL.BAT contents:
//
// D:
// CD \
// DIVIL C:\  %1 
// %c:
//
int ParseBAT(char *shell)
{
	int 	i;
	char 	batname[256];
	char 	line[256];
	char 	*ptr;
	FILE 	*batfile;
	char	*params = (char *)PHYS_PTR(DOS_PSP+16, 0);
	
	strcpy(line, "C:\\");				// The batch file is assumed to be at the C:\ root!
	strcat(line, shell);
	UnixName(batname, line, 0);			// Get the proper host name for the input batch file.
	
	batfile = fopen(batname, "r");		// Open the batch file.
	if (NULL == batfile)
	{
		SendError("Open of batch file '%s' failed!", batname);
		return ERROR_FILENOTFOUND;
	}
	
	while (fgets(line, 256, batfile) != NULL)
	{
		// Make everything upper case (not strictly correct, but should work for our purposes)
		for (i=0; i < strlen(line); i++)
		{
			if (line[i] >= 'a' && line[i] <= 'z')
				line[i] -= 32;
		}
		// Remove possible comments
		if ((ptr = strchr(line, ';')))
			*ptr = 0;
		// Remove parameters (we don't support them!)
		if ((ptr = strchr(line, '%')))
			*ptr = 0;
		// Remove the whitespace at the end of the line.
		for (i=strlen(line)-1; i >= 0 && strchr("\r\n\t ", line[i]); i--)
			line[i] = 0;
		// Is it a change current drive command?
		if (('C' == line[0] || 'D' == line[0]) && ':' == line[1] && strchr("\r\n\t ;", line[2]))
		{
			DosSetCuDrive(line[0] - 'A');
		}
		// Is it a CD command?
		else if ('C' == line[0] && 'D' == line[1] && ' ' == line[2])
		{
			DosChangeDir(line+3);
		}
		else
		{
			// We assume this is the name of the executable to launch, plus possible parameters.
			// This probably needs much enhancement work in the future!
			i = 0;
			for (ptr = line; *ptr && *ptr != ' '; ptr++)
			{
				shell[i++] = *ptr;
			}
			shell[i] = 0;
			if (!strstr(shell, ".EXE") && !strstr(shell, ".COM"))
				strcat(shell, ".EXE");		// TODO!
			// Build the command line parameters into DOS_PSP+16 (segment 0070:0000)
			while (' ' == *ptr)
				ptr++;
			strcpy(params+1, ptr);
			strcat(params+1, "\r");
			params[0] = strlen(params+1)-1;
			fclose(batfile);
			return NO_ERROR;
		}
	}
	fclose(batfile);
	SendError("Parsing of batch file '%s' failed!", batname);
	return ERROR_FILENOTFOUND;
}

int StartShell(char *comexe)
{
	EXEC_BLK 	exb;
	u8 			mode = EXECUTE;		// Config->cfgP_0_startmode;
	int			i;
	char		*params = (char *)PHYS_PTR(DOS_PSP+16, 0);

	if (comexe && DosName(Shell, comexe) == NO_ERROR)
	{
		// Got a com/exe/bat file as a parameter, so attempt to launch it.
		params[0] = params[1] = 0;
		if (strstr(Shell, ".bat") || strstr(Shell, ".BAT"))
		{
			// If we got a BAT file, attempt to parse it and replace the Shell
			// contents with the actual executable found from the BAT file.
			if (ParseBAT(Shell) != NO_ERROR)
				return -1;
		}
		exb.exec.fcb_1 = exb.exec.fcb_2 = 0xFFFFFFFF;
		exb.exec.env_seg = DOS_PSP + 8;
		exb.exec.cmd_line = ((DOS_PSP+16)<<16);
		i = DosExec(mode, &exb, Shell);
		if ( i != NO_ERROR )
		{
			switch(i)
			{
				case ERROR_FILENOTFOUND:
					SendError("File not found starting program '%s'!", Shell);
					break;
				case ERROR_INV_DATA:
					SendError("Executable file '%s' load failed!", Shell);
					break;
				case ERROR_OUT_OF_MEM:
					SendError("Too big executable file '%s'!", Shell);
					break;
				default:
					SendError("Unspecified error starting program '%s'!", Shell);
					break;
			}
			return -1;
		}
	}
	else
	{
		for ( i = 0; i < SHELLCNT; i++ )
		{
			/* build exec block and save all parameters here as init part will vanish! */
			exb.exec.fcb_1 = exb.exec.fcb_2 = 0xFFFFFFFF;
			exb.exec.env_seg = DOS_PSP + 8;
			strcpy( Shell, shells[i] );

			// Build the command line parameters into DOS_PSP+16 (segment 0070:0000)
			params[1] = 0;
			memcpy( params+1, Shell, strlen(Shell)-9 );
			strcpy( params+strlen(Shell)-8, " /P\r" );		// This is the primary shell
			params[0] = strlen(params+1)-1;
			exb.exec.cmd_line = ((DOS_PSP+16)<<16);
			
			if ( NO_ERROR == DosExec(mode, &exb, Shell) )
				break;
		}
		if ( SHELLCNT == i )
		{
			return -1;
		}
	}
	return 0;
}

#define	INT8OFFS		0xB00
#define	INT9OFFS		0x100
#define	INT16OFFS		0xA00
#define	INT67OFFS		0xB40000
#define	SCANXLATOFFS 	0x300
#define ROM8x8FONTOFFS	0xF66E
#define	VSYNCCOPY	0

extern void Dos_InitMem();
extern void BIOS_Init();
extern int INTVectors_init[];
extern char BIOSData_init[];
extern char BIOS_F000_Init[];
extern char ENV_init[];

const DOS_LOL LOL_init = {
	0,			// u16	lol_start;					// -26	  WORD
	0,			// u16	lol_CX;						// -24    WORD    (DOS 3.1+) contents of CX from INT 21/AX=5E01h
	1,			// u16	lol_LRU_cache;				// -22    WORD    (DOS ???+) LRU counter for FCB caching
	0,			// u16	lol_LRU_opens;				// -20    WORD    (DOS ???+) LRU counter for FCB opens
	0xFFFFFFFF,	// u32	lol_OEM;					// -18    DWORD   (DOS ???+) address of OEM function handler (see INT 21/AH=F8h) FFFFh:FFFFh if not installed or not available
	0xFFFF,		// u16 	lol_IRET;					// -14    WORD    (DOS ???+) offset in DOS CS of code to return from INT 21 call
	0,			// u16	lol_retry_count;			// -12    WORD    (DOS 3.1+) sharing retry count (see AX=440Bh)
	0,			// u16 	lol_retry_delay;			// -10    WORD    (DOS 3.1+) sharing retry delay (see AX=440Bh)
	0xFFFFFFFF,	// u32	lol_disk_buffer;			// -8     DWORD   (DOS 3.0+) pointer to current disk buffer
	0,			// u16 	lol_CON_input;				// -4     WORD    (DOS 3.0+) pointer in DOS data segment of unread CON input when CON is read via a handle. 0000h indicates no unread input
	0x100,		// u16 	lol_first_MCB;				// -2     WORD    segment of first memory control block (see #01628)
	0x0090027A,	// u32	lol_first_DPB;				// 00h    DWORD   pointer to first Drive Parameter Block (see #01395 at AH=32h)
	0x01010000,	// u32	lol_first_SFT;				// 04h    DWORD   -> first System File Table (see #01639,#01640,#01641,#01642)
	0xFFFFFFFF,	// u32 	lol_CLOCK_hdr;				// 08h    DWORD   pointer to active CLOCK$ devices header (most recently loaded driver with CLOCK bit set)
	0xFFFFFFFF,	// u32 	lol_CON_hdr;				// 0Ch    DWORD   pointer to active CON devices header (most recently loaded driver with STDIN bit set)
	512,		// u16	lol_max_bytes_per_sector;	// 10h    WORD    maximum bytes per sector of any block device
	0xFFFFFFFF,	// u32 	lol_disk_buffer_info;		// 12h    DWORD   pointer to disk buffer info record (see #01652,#01653)
	0x00BA0000,	// u32 	lol_dir_array;				// 16h    DWORD pointer to array of current directory structures (see #01643,#01644)
	0x00C00000,	// u32 	lol_FCB_tables;				// 1Ah    DWORD pointer to system FCB tables (see #01640,#01641,#01642)
	0,			// u16 	lol_prot_FCBs;				// 1Eh    WORD number of protected FCBs (the y in the CONFIG.SYS FCBS=x,y) (always 00h for DOS 5.0)
	0,			// u8  	lol_block_devices;			// 20h    BYTE number of block devices installed
	1,			// u8  	lol_lastdrive;				// 21h    BYTE number of available drive letters; also size of current directory structure array.
	//------- Actual NUL device header
	{0x00, 0x00, 0xFE, 0x00,	// 00h    DWORD   pointer to next driver, offset=FFFFh if last driver
	0x04, 0x80,	// 04h    WORD    device attributes (see #01647,#01648)
	0, 0,		// 06h    WORD    device strategy entry point call with ES:BX -> request header (see #02597 at INT 2F/AX=0802h)
	0, 0,		// 08h    WORD    device interrupt entry point
	'N','U','L',' ',' ',' ',' ',' '},	// 0Ah  8 BYTEs   blank-padded character device name
	//-------
	0,			// u8	lol_JOIN_num;				// 34h    BYTE    number of JOIN'ed drives
	0,			// u16	lol_spec_progs;				// 35h    WORD    pointer within IBMDOS code segment to list of special program names (see #01662) (always 0000h for DOS 5.0)
	0,			// u32	lol_SETVER_list;			// 37h    DWORD   pointer to SETVER program list or 0000h:0000h
	0,			// u16 	lol_A20_fix;				// 3Bh    WORD    (DOS=HIGH) offset in DOS CS of function to fix A20 control when executing special .COM format
	0,			// u16 	lol_high_PSP;				// 3Dh    WORD    PSP of most-recently EXECed program if DOS in HMA, 0000h if low used for maintaining count of INT 21 calls which disable A20 on return
	0,			// u16 	lol_BUFFERS_x;				// 3Fh    WORD    the x in BUFFERS x,y (rounded up to multiple of 30 if in EMS)
	0,			// u16 	lol_BUFFERS_y;				// 41h    WORD    number of lookahead buffers (the y in BUFFERS x,y)
	3,			// u8	lol_boot_drive;				// 43h    BYTE    boot drive (1=A:)
	0,			// u8	lol_386_flag;				// 44h    BYTE    flag:01h to use DWORD moves (80386+), 00h otherwise
	0			// u16 	lol_ext_mem_size;			// 45h    WORD    extended memory size in KB
};

const u8 CON_Device_Init[] = {
	0xFF, 0xFF, 0xFF, 0xFF,		// Pointer to next device 
	0x13, 0x80,					// Attributes
	0xFF, 0xFF, 0xFF, 0xFF,		// Strategy routine
	'C', 'O', 'N', ' ', ' ', ' ', ' ', ' '	// Name
};

//==========================================================
// Boot the virtual PC machine and init the DOS kernel
//==========================================================
void InitKernel()
{
	//------------------------------------------------------
	// Start the hardware timers and IRQs.
	//------------------------------------------------------
#if 0
	REG_FLAGS = 0;						// Prohibit the IRQHandler from actually doing anything yet
	
	irqSet(IRQ_VBLANK, VSyncKeyboard);
	irqSet(IRQ_TIMER1, TimerIRQ);
	
	// PC timer runs at 1.193.182 Hz, while ARM timer runs at 33.513.982 Hz,
	// so using ARM timer cascade mode, the lower timer should overflow every 28.0879 ticks.
	TIMER0_CR = 0;					// Stop the timer in case it is running
	TIMER0_DATA = 0x10000 - 28;		// 28
	TIMER0_CR = TIMER_ENABLE;

	TIMER1_CR = 0;
	TIMER1_DATA = 0;
	TIMER1_CR = TIMER_ENABLE|TIMER_IRQ_REQ|TIMER_CASCADE;

	TIMER2_CR = 0;					// Stop the timer in case it is running
	TIMER2_DATA = 0x10000 - 28;		// 28
	TIMER2_CR = TIMER_ENABLE;
	
	irqEnable(IRQ_VBLANK|IRQ_TIMER1);
#endif
	//------------------------------------------------------
	// Initialize the DOS memory structures:
	//	PSP 				(0060:0000)
	// 	Master Environment 	(0068:0000)
	//	List-of-lists 		(0080:0000) (reported as 0080:0026)
	//	DOS SDA				(0090:0000)
	//	First DPB			(0090:027A)
	//	Directory array		(00BA:0000)
	//	Fake FCB SFT		(00C0:0000)
	//	'CON' device		(00FE:0000)
	//  First DOS MCB		(0100:0000)
	//	First SFT			(0101:0000)
	//------------------------------------------------------
	memcpy( PHYS_PTR(0x80, 0x000C), &LOL_init, sizeof(DOS_LOL) );
	Dos_InitMem();
	memcpy( PHYS_PTR(DOS_PSP+8, 0), "BLASTER=A220 I7 D1 T3\0\0", 23 );
	*(PHYS_PTR(DOS_PSP+16, 1)) = 0x0D;
	// Fake the directory array contents
	memcpy( PHYS_PTR(0xBA, 0), "C:\\", 3);	// Fake current directory
	// Fake the FCB SFT
	*((u32*)PHYS_PTR(0xC0, 0)) = 0xFFFFFFFF;	// Last entry in the chain
	*((u16*)PHYS_PTR(0xC0, 4)) = 8;				// Room for 8 files
	// Create a CON device driver
	memcpy( PHYS_PTR(0xFE, 0), CON_Device_Init, sizeof(CON_Device_Init));

	dos_sda.cu_psp = DOS_PSP;
	dos_sda.switchar = '/';
	dos_sda.break_ena = 1;
	dos_sda.default_drive = 2;			// 0 = 'A', 1='B', 2='C'
	dos_sda.DOSVerMajor	= 5;			// DOS 5.00
	dos_sda.DOSVerMinor = 0;
	dos_sft.sftt_next = 0x00C00000;		// Next SFT is the same as FCB SFT (Windows 3.11 needs it to exist)
	dos_sft.sftt_count = DOS_SFT_SIZE;
	
	BuildDPB( &(dos_sda.dpb) );

//	if ( ReadConfig( "/DATA/DSX86/DSX86.INI" ) < 0 )
//		ReadConfig( "/DSX86.INI" );
		
}

