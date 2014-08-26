//=============================================================================
// dos.h
//
// This is the include file for all C code that handle the DOS kernel routines.
// The contents of this file are mainly definitions for the various DOS internal
// structures, and at the end are the extern declarations for the DOS kernel
// routines.
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

#pragma pack(1)
#define	PACKED	

#define	NO_ERROR			0
#define ERROR_INVFUNCTION   -1
#define ERROR_FILENOTFOUND	-2
#define ERROR_PATHNOTFOUND  -3
#define ERROR_TOO_MANY      -4
#define ERROR_ACCESS       	-5
#define ERROR_INV_HANDLE    -6
#define	ERROR_BAD_MCB		-7
#define ERROR_OUT_OF_MEM	-8
//#define DE_INVLDMCB     -9      /* Invalid memory control block */
//#define DE_INVLDENV     -10     /* Invalid enviornement         */
#define ERROR_INV_FORMAT	-11
//#define DE_INVLDACC     -12     /* Invalid access               */
#define ERROR_INV_DATA    	-13
#define ERROR_INV_DRIVE    	-15
//#define DE_RMVCUDIR     -16     /* Attempt remove current dir   */
#define ERROR_DEVICE       	-17   /* Not same device              */
#define ERROR_NO_MORE       -18
//#define DE_WRTPRTCT     -19     /* No more files                */
//#define DE_BLKINVLD     -20     /* invalid block                */
//#define DE_INVLDBUF     -24     /* invalid buffer size, ext fnc */
#define ERROR_SEEK         -25
//#define DE_HNDLDSKFULL  -28     /* handle disk full (?)         */
#define	ERROR_FILEEXISTS	-80

extern u8* phys_ptr(int seg, int offs);

#define PHYS_PTR(seg,offs) (phys_ptr(seg, offs))
#define	PHYS_DOS_PTR(seg)  ((u8 *)((int)INTVectors + ((seg)<<4)))
#define	LOG_SEG(phys) (((int)phys-(int)INTVectors)>>4)

#define DS_DX_PTR ((char *)PHYS_PTR(REG_DS, DX))
#define DS_SI_PTR ((char *)PHYS_PTR(REG_DS, SI))
#define DS_BX_PTR ((char *)PHYS_PTR(REG_DS, BX))
#define ES_DI_PTR ((char *)PHYS_PTR(REG_ES, DI))
#define ES_BX_PTR ((char *)PHYS_PTR(REG_ES, BX))
#define ES_BP_PTR ((char *)PHYS_PTR(REG_ES, BP))
#define ES_DX_PTR ((char *)PHYS_PTR(REG_ES, DX))

#undef MAXPATHLEN
#define	MAXPATHLEN	254

// DOS List-Of-Lists structure at 0080:000C

typedef struct PACKED {
	u16	lol_start;					// -26	  WORD	  ??
	u16	lol_CX;						// -24    WORD    (DOS 3.1+) contents of CX from INT 21/AX=5E01h
	u16	lol_LRU_cache;				// -22    WORD    (DOS ???+) LRU counter for FCB caching
	u16	lol_LRU_opens;				// -20    WORD    (DOS ???+) LRU counter for FCB opens
	u32	lol_OEM;					// -18    DWORD   (DOS ???+) address of OEM function handler (see INT 21/AH=F8h) FFFFh:FFFFh if not installed or not available
	u16 lol_IRET;					// -14    WORD    (DOS ???+) offset in DOS CS of code to return from INT 21 call
	u16	lol_retry_count;			// -12    WORD    (DOS 3.1+) sharing retry count (see AX=440Bh)
	u16 lol_retry_delay;			// -10    WORD    (DOS 3.1+) sharing retry delay (see AX=440Bh)
	u32	lol_disk_buffer;			// -8     DWORD   (DOS 3.0+) pointer to current disk buffer
	u16 lol_CON_input;				// -4     WORD    (DOS 3.0+) pointer in DOS data segment of unread CON input when CON is read via a handle. 0000h indicates no unread input
	u16 lol_first_MCB;				// -2     WORD    segment of first memory control block (see #01628)
	u32	lol_first_DPB;				// 00h    DWORD   pointer to first Drive Parameter Block (see #01395 at AH=32h)
	u32	lol_first_SFT;				// 04h    DWORD   -> first System File Table (see #01639,#01640,#01641,#01642)
	u32 lol_CLOCK_hdr;				// 08h    DWORD   pointer to active CLOCK$ devices header (most recently loaded driver with CLOCK bit set)
	u32 lol_CON_hdr;				// 0Ch    DWORD   pointer to active CON devices header (most recently loaded driver with STDIN bit set)
	u16	lol_max_bytes_per_sector;	// 10h    WORD    maximum bytes per sector of any block device
	u32 lol_disk_buffer_info;		// 12h    DWORD   pointer to disk buffer info record (see #01652,#01653)
	u32 lol_dir_array;				// 16h    DWORD pointer to array of current directory structures (see #01643,#01644)
	u32 lol_FCB_tables;				// 1Ah    DWORD pointer to system FCB tables (see #01640,#01641,#01642)
	u16 lol_prot_FCBs;				// 1Eh    WORD number of protected FCBs (the y in the CONFIG.SYS FCBS=x,y) (always 00h for DOS 5.0)
	u8  lol_block_devices;			// 20h    BYTE number of block devices installed
	u8  lol_lastdrive;				// 21h    BYTE number of available drive letters; also size of current directory structure array.
	u8	lol_NUL_device[18];			// 22h    18 BYTEs   actual NUL device driver header (not a pointer!) NUL is always the first device on DOS's linked list of device drivers. (see #01646)
	u8	lol_JOIN_num;				// 34h    BYTE    number of JOIN'ed drives
	u16	lol_spec_progs;				// 35h    WORD    pointer within IBMDOS code segment to list of special program names (see #01662) (always 0000h for DOS 5.0)
	u32	lol_SETVER_list;			// 37h    DWORD   pointer to SETVER program list or 0000h:0000h
	u16 lol_A20_fix;				// 3Bh    WORD    (DOS=HIGH) offset in DOS CS of function to fix A20 control when executing special .COM format
	u16 lol_high_PSP;				// 3Dh    WORD    PSP of most-recently EXECed program if DOS in HMA, 0000h if low used for maintaining count of INT 21 calls which disable A20 on return
	u16 lol_BUFFERS_x;				// 3Fh    WORD    the x in BUFFERS x,y (rounded up to multiple of 30 if in EMS)
	u16 lol_BUFFERS_y;				// 41h    WORD    number of lookahead buffers (the y in BUFFERS x,y)
	u8	lol_boot_drive;				// 43h    BYTE    boot drive (1=A:)
	u8	lol_386_flag;				// 44h    BYTE    flag:01h to use DWORD moves (80386+), 00h otherwise
	u16 lol_ext_mem_size;			// 45h    WORD    extended memory size in KB
} DOS_LOL;

typedef struct PACKED {
  u8 	m_type;                  	/* mcb type - chain or end              */
  u16 	m_psp;                  	/* owner id via psp segment             */
  u16 	m_size;                 	/* size of segment in paragraphs        */
  u8	m_fill[3];
  char	m_name[8];              	/* owner name limited to 8 bytes        */
} MCB;

/* FAT file name constants                                              */
#define FNAME_SIZE              8
#define FEXT_SIZE               3

// DOS File Control Block (FCB)

typedef struct PACKED {
  u8	fcb_drive;              	// 0x00 = Drive number 0=default, 1=A, etc
  char	fcb_fname[FNAME_SIZE];   	// 0x01 = File name
  char	fcb_fext[FEXT_SIZE];     	// 0x09 = File name Extension
  u16	fcb_cublock;            	// 0x0C = Current block number of 128 records/block, for seq. r/w
  u16	fcb_recsiz;             	// 0x0E = Logical record size in bytes, default = 128
  u32	fcb_fsize;              	// 0x10 = File size in bytes
  u16	fcb_date;                	// 0x14 = Date file created
  u16	fcb_time;                	// 0x16 = Time of last write
  /* the following are reserved by system                         */
  u8	fcb_sftno;               	// 0x18 = Device ID
  u8	fcb_attrib_hi;           	// 0x19 = share info, dev attrib word hi
  u8	fcb_attrib_lo;           	// 0x1A = dev attrib word lo, open mode
  u16	fcb_strtclst;           	// 0x1B = file starting cluster
  u16	fcb_dirclst;            	// 0x1D = cluster of the dir entry
  u8	fcb_diroff_unused;      	// 0x1F = offset of the dir entry
  /* end reserved                                                 */
  u8	fcb_curec;              	// 0x20 = Current rec number
  u32	fcb_rndm;               	// 0x21 = Current relative record number
} FCB;

/* FAT extended fcb                                                     */
typedef struct PACKED {
  u8 xfcb_flag;              /* 0xff indicates Extended FCB  */
  u8 xfcb_resvrd[5];          /* Reserved                     */
  u8 xfcb_attrib;            /* Attribute                    */
  FCB xfcb_fcb;
} XFCB;

#define FCB_READ  	1
#define FCB_WRITE 	2
#define FCB_RANDOM  4

#define FCB_SUCCESS				0
#define FCB_ERR_NODATA  		1
#define FCB_ERR_SEGMENT_WRAP 	2
#define FCB_ERR_EOF     		3
#define	FCB_ERROR				0xFF

/* FAT file system directory entry                                      */
typedef struct PACKED {
  char dir_name[FNAME_SIZE + FEXT_SIZE];   /* Filename + extension in FCB format */
  u8 dir_attrib;             /* File Attribute               */
  u8 dir_case;               /* File case                    */
  u8 dir_crtimems;           /* Milliseconds                 */
  u16 dir_crtime;             /* Creation time                */
  u16 dir_crdate;             /* Creation date                */
  u16 dir_accdate;            /* Last access date             */
  u16 dir_start_high;         /* High word of the cluster     */
  u16 dir_time;                /* Time file created/updated    */
  u16 dir_date;                /* Date file created/updated    */
  u16 dir_start;              /* Starting cluster, 1st available = 2  */
  u32 dir_size;               /* File size in bytes           */
} DIRENT;


#define CTBUFFERSIZE       127

typedef struct PACKED {
  u8	ctCount;                /* number of bytes returned             */
  char 	ctBuffer[CTBUFFERSIZE];  /* the buffer itself            */
} COMMANDTAIL;


// DOS Exec Block

typedef struct PACKED {
  union {
    struct PACKED {
      u16	load_seg;
      u16	reloc;
    } _load;
    struct PACKED {
      u16 	env_seg;
      u32	cmd_line;			// Far pointer in seg<<16|offs notation
      u32	fcb_1;				// Far pointer in seg<<16|offs notation
      u32	fcb_2;				// Far pointer in seg<<16|offs notation
      u32	stack;				// Far pointer in seg<<16|offs notation
      u32	start_addr;			// Far pointer in seg<<16|offs notation
    } _exec;
  } ldata;
} EXEC_BLK;

#define exec    ldata._exec
#define load    ldata._load

#define EXECUTE 0
#define LOAD    1
#define OVERLAY 3

// DOS PSP structure

typedef struct PACKED {
  u16	ps_exit;                /* 00 CP/M-like exit point: int 20 */
  u16	ps_size;                /* 02 segment of first byte beyond */
                                /*    memory allocated to program  */
  u8	ps_fill1;                /* 04 single char fill=0           */

  /* CP/M-like entry point                                         */
  /* offsets 5-9 are a far call to absolute address 0:00C0h
     encoded using 1MB wrap form of address (e.g. 0F01D:FEF0h)
     for compatiblity with CP/M apps that do a near call to psp:5
     and expect size (KB) of allocated segment in word at offset 6 */
  u8	ps_farcall;             /* 05  far call opcode             */
  u32	ps_reentry;				/* 06  re-entry point          */
  u32	ps_isv22,              	/* 0a terminate address            */
        ps_isv23,              	/* 0e ctrl-break address           */
        ps_isv24;              	/* 12 critical error address       */
  u16	ps_parent;              /* 16 parent psp segment           */
  u8	ps_files[20];           /* 18 file table - 0xff is unused  */
  u16	ps_environ;             /* 2c environment paragraph        */
  u32	ps_stack;           	/* 2e user stack pointer - int 21  */
  u16	ps_maxfiles;            /* 32 maximum open files           */
  u32	ps_filetab;        		/* 34 open file table pointer      */
  u32	ps_prevpsp;         	/* 38 previous psp pointer         */
  u8	ps_fill2;               /* 3c unused                       */
  u8	ps_truename;            /* 3d [unused] append truename flag int2f/B711h */
  u8	ps_netx_taskid[2];      /* 3e [Novell only field] task id  */
  u16	ps_retdosver;           /* 40 [unused] version to return on int21/30h */
  u16	pdb_next;               /* 42 [Win only field] PSP chain   */
  u8	ps_fill2b[4];           /* 44 unused, 4 bytes              */
  u8	ps_olddos;              /* 48 [Win only field] DOS/Win program */
  u8	ps_fill2c[7];           /* 49 unused, 7 bytes              */
  u8	ps_unix[3];             /* 50 unix style call - 0xcd 0x21 0xcb */
  u8	ps_fill3[9];             /* 53 */
  union {
    struct PACKED {
      FCB 	_ps_fcb1;             /* 5c first command line argument */
    } _u1;
    struct PACKED {
      u8	fill4[16];
      FCB	_ps_fcb2;             /* second command line argument */
    } _u2;
    struct PACKED {
      u8	fill5[36];
      COMMANDTAIL	_ps_cmd;
    } _u3;
  } _u;
} PSP;

#define ps_fcb1 _u._u1._ps_fcb1
#define ps_fcb2 _u._u2._ps_fcb2
#define ps_cmd  _u._u3._ps_cmd

#define DOS_PSP         0x0060

typedef struct {
  u16 exSignature;
  u16 exExtraBytes;
  u16 exPages;
  u16 exRelocItems;
  u16 exHeaderSize;
  u16 exMinAlloc;
  u16 exMaxAlloc;
  u16 exInitSS;
  u16 exInitSP;
  u16 exCheckSum;
  u16 exInitIP;
  u16 exInitCS;
  u16 exRelocTable;
  u16 exOverlay;
} EXE_HEADER;

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

typedef struct PACKED {
  u8 	dm_drive;
  char 	dm_name_pat[FNAME_SIZE + FEXT_SIZE];
  u8 	dm_attr_srch;
  u16 	dm_entry;
  u16	dm_dircluster;
  u32	reserved;
  u8 	dm_attr_fnd;            	/* found file attribute         */
  u16 	dm_time;                 /* file time                    */
  u16 	dm_date;                 /* file date                    */
  u32 	dm_size;                	/* file size                    */
  char 	dm_name[FNAME_SIZE + FEXT_SIZE + 2];     /* file name    */
} DIRMATCH;


#define MAGIC 0x5a4d
#define OLD_MAGIC 0x4d5a

// File attributes
#define ATTRIB_DEVICE 0x40    		// device bit
#define ATTRIB_ARCH	0x20			// Archive
#define ATTRIB_DIR	0x10			// Directory
#define ATTRIB_LFN	0x0F			// Long file name
#define ATTRIB_VOL	0x08			// Volume
#define ATTRIB_SYS	0x04			// System
#define ATTRIB_HID	0x02			// Hidden
#define ATTRIB_RO	0x01			// Read only
#define	ATTRIB_ALL	(ATTRIB_RO|ATTRIB_HID|ATTRIB_SYS|ATTRIB_VOL|ATTRIB_DIR|ATTRIB_ARCH)

extern bool fatOK;
extern DIRMATCH *dta;			// real pointer to current DTA
extern char PriPathName[];		// Internal work variable for file/path name handling

typedef struct PACKED {
	u8	dpb_unit;                /* unit for error reporting     */
	u8	dpb_subunit;             /* the sub-unit for driver      */
	u16	dpb_secsize;            /* sector size                  */
	u8	dpb_clsmask;            /* mask (sectors/cluster-1)     */
	u8	dpb_shftcnt;            /* log base 2 of cluster size   */
	u16	dpb_fatstrt;            /* FAT start sector             */
	u8	dpb_fats;               /* # of FAT copies              */
	u16	dpb_dirents;            /* # of dir entries             */
	u16	dpb_data;               /* start of data area           */
	u16	dpb_size;               /* # of clusters+1 on media     */
	u16	dpb_fatsize;            /* # of sectors / FAT           */
	u16	dpb_dirstrt;            /* start sec. of root dir       */
	u32	dpb_device;           	/* pointer to device header     */
	u8	dpb_mdb;                /* media descr. byte            */
	u8	dpb_flags;               /* -1 = force MEDIA CHK         */
	u32	dpb_next;              /* next dpb in chain            */
	u16	dpb_cluster;            /* cluster # of first free      */
	u16	dpb_nfreeclst;          /* number of free clusters      */
} DPB;

#define UNKNCLUSTER      0x0000 /* see RBIL INT 21/AH=52 entry */
#define XUNKNCLSTFREE    0xffffffffl    /* unknown for DOS */
#define UNKNCLSTFREE     0xffff /* unknown for DOS */

/*                                                                      */
/* Bios Parameter Block structure                                       */
/*                                                                      */
#define FAT_NO_MIRRORING 0x80
#define BPB_SIZEOF 31           /* size of the standard BPB */
#define DIRENT_SIZE     32

#define FAT_MAGIC       4085
#define FAT_MAGIC16     65525U
#define FAT_MAGIC32     268435455UL

typedef struct PACKED {
	u16		bpb_nbyte;              /* Bytes per Sector             */
	u8		bpb_nsector;            /* Sectors per Allocation Unit  */
	u16		bpb_nreserved;          /* # Reserved Sectors           */
	u8		bpb_nfat;               /* # FATs                       */
	u16		bpb_ndirent;            /* # Root Directory entries     */
	u16		bpb_nsize;              /* Size in sectors              */
	u8		bpb_mdesc;              /* MEDIA Descriptor Byte        */
	u16		bpb_nfsect;             /* FAT size in sectors          */
	u16		bpb_nsecs;              /* Sectors per track            */
	u16		bpb_nheads;             /* Number of heads              */
	u32		bpb_hidden;             /* Hidden sectors               */
	u32		bpb_huge;               /* Size in sectors if           */
} BPB;

// DOS Swappable Data Area at 0080:00DE

typedef struct PACKED {
	u8	PrinterEcho;				// -34 -  0 = no printer echo, ~0 echo
	u8	verify_ena;					// ~0, write with verify
	u8	scr_pos;       				// Current Cursor Column
	u8	switchar;      				// -31 - switch char
	u8	mem_access_mode; 			// -30 -  memory allocation strategy
	u8	sharing_flag;    			// 00 = sharing module not loaded
	u8	net_set_count;				// -28 -  count the name below was set
	char net_name[16];				// -27 - 15 Character Network Name
	u16 CritPatch[5];				// -11 zero list of patched critical section variables
    u8	fill1;            			// -01 - unknown
	//  <-- Address returned by INT21/5D06 = 0090:0000
	u8	ErrorMode;     				// 00 - Critical Error Flag
	u8	InDOS;         				// 01 - Indos Flag
	u8	CritErrDrive;				// 02 - Drive on write protect error
	u8	CritErrLocus;				// 03 - Error Locus
	u16	CritErrCode;				// 04 - DOS format error Code
	u8	CritErrAction;				// 06 - Error Action Code
	u8	CritErrClass;				// 07 - Error Class
	u32	CritErrDev;					// 08 - Failing Device Address
	u32	dta;						// 0C - current DTA
	u16	cu_psp;        				// 10 - Current PSP
	u16	break_sp;        			// 12 - used in int 23
	u8	return_code;				// 14 - return code from process
	u8  return_mode;				//
	u8	default_drive;				// 16 - Current Drive
	u8	break_ena;					// 17 - Break Flag (default TRUE)
	u8	fill2;						// 18 - flag, code page switching
    u8	fill3;						// 19 - flag, copy of 18 on int 24h abort
	u16	Int21AX;					// 1A - AX from last Int 21
	u16	owning_psp;					// 1C - owning psp
	u16	MachineId;     				// 1E - remote machine ID
	u16	fill20[5];					// 20 - First usable mcb
	u8	ExecFlags;					// 2A - unknown (DSx86: Execute flags for the next command)
	u8	fill2B[2];					// 2B - unknown
	u8	break_flg;					// 2D - Program aborted by ^C
    u8	DOSVerMajor;				// 2E - unknown (DSx86: Active DOS version)
    u8	DOSVerMinor;				// 2F - not referenced (DSx86: Active DOS version)
	u8	DayOfMonth;					// 30 - day of month
	u8	Month;						// 31 - month
	u16	YearsSince1980;				// 32 - year since 1980
	u16	daysSince1980;				// 34 - number of days since epoch, force rebuild on first clock read
	u8	DayOfWeek;					// 36 - day of week
	u8	console_swap;				// 37 console swapped during read from dev
	u8	dosidle_flag;				// 38 - safe to call int28 if nonzero
	u8	abort_progress;				// 39 - abort in progress
	u8	ClkReqHdr[30];				// 3A - Device driver request header
    u32	fill58;						// 58 - pointer to driver entry
	u8	MediaReqHdr[22];			// 5C - Device driver request header
	u8	IoReqHdr[30];				// 72 - Device driver request header
    u8	fill90[6];					// 90 - unknown
	u8	ClkRecord[6];				// 96 - CLOCK$ transfer record
    u16	fill9C;						// 9C - unknown
	u8	PriPathBuffer[0x80];		// 9E - buffer for file name
	char SecPathName[0x80];			// 11E - buffer for file name
	u8	sda_tmp_dm[21];				// 19E - 21 byte srch state
	DIRENT SearchDir;				// 1B3 - 32 byte dir entry
	u8	TempCDS[88];				// 1D3 - TemporaryCDS buffer
	u8	DirEntBuffer[32];			// 22B - space enough for 1 dir entry
	u16	wAttr;						// 24B - extended FCB file attribute
	u8	SAttr;          			// 24D - Attribute Mask for Dir Search
	u8	OpenMode;					// 24E - File Open Attribute
	DIRMATCH Dmatch;
	DPB	dpb;
} DOS_SDA;

#define	DOS_SDA_SEG	0x0090
#define dos_sda (*(DOS_SDA*)(PHYS_PTR(DOS_SDA_SEG-0x10, 0x00DE)))

#define	EXEC_DEBUG		1
#define	EXEC_LOADFIX	2

typedef struct PACKED {
	u16	sft_count;               	/* 00 - reference count                      */
	u16	sft_mode;                	/* 02 - open mode - see below                */
	u8	sft_attrib;              	/* 04 - file attribute - dir style           */
	u16	sft_flags;					/* 05 */
	u32	sft_dcb_or_dev;				/* 07 */
	u16	sft_stclust;          		/* 0b - Starting cluster                     */
	u16	sft_time;                	/* 0d - File time                            */
	u16	sft_date;                	/* 0f - File date                            */
	u32 sft_size;               	/* 11 - File size                            */
	u32	sft_posit;              	/* 15 - Current file position                */
	u16	sft_relclust;           	/* 19 - File relative cluster (low part)     */
	u32 sft_dirsector;          	/* 1b - Sector containing cluster            */
	u8	sft_diridx;             	/* 1f - directory index                      */
	char sft_name[11];            	/* 20 - dir style file name                  */
	FILE *sft_file;         		/* 2b - backward link of file sharing sft    */
	u16	sft_mach;                	/* 2f - machine number - network apps        */
	u16	sft_psp;                 	/* 31 - owner psp                            */
	u16	sft_shroff;              	/* 33 - Sharing offset                       */
	u16	sft_cuclust;          		/* 35 - File current cluster                 */
	u32	sft_ifsptr;         		/* 37 - pointer to IFS driver for file, 0000000h if native DOS */
} SFT;

#define	DOS_SFT_SIZE 64

/* System File Definition List                                          */
typedef struct PACKED {
	u32	sftt_next;					/* link to next table in list   */
	u16	sftt_count;              	/* # of handle definition       */
	/* entries, this table          */
	SFT sftt_table[DOS_SFT_SIZE];	/* The array of sft for block   */
} SFTTBL;

#define	DOS_SFT_SEG	0x0101
#define dos_sft (*(SFTTBL*)(PHYS_DOS_PTR(DOS_SFT_SEG)))

#define SFT_FDEVICE     0x0080  	/* device entry                 */

/* the following bits are file (block) unique                           */
#define SFT_FDATE       0x4000  /* File date set                */
#define SFT_FCLEAN      0x0040  /* File has not been written to */
#define SFT_FDMASK      0x003f  /* File mask for drive no       */

/* the following bits are device (char) unique                          */
#define SFT_FIOCTL      0x4000  /* IOCTL support - device       */
#define SFT_FOCRM       0x0800  /* Open/Close/RM bit in device attribute*/
#define SFT_FEOF        0x0040  /* device eof                   */
#define SFT_FBINARY     0x0020  /* device binary mode           */
#define SFT_FSPECIAL    0x0010  /* int 29 support               */
#define SFT_FCLOCK      0x0008  /* device is clock              */
#define SFT_FNUL        0x0004  /* device is nul                */
#define SFT_FCONOUT     0x0002  /* device is console output     */
#define SFT_FCONIN      0x0001  /* device is console input      */

#define IS_DEVICE 0x20

/* mode bits                                                    */
#define O_VALIDMASK     0xfff3  /* valid open mask              */

#define O_RDONLY        0x0000
#define O_WRONLY        0x0001
#define O_RDWR          0x0002
#define O_ACCMODE       0x0003

/* bits 2, 3 reserved */
#define	O_TESTOPEN		0x0004	/* Need to test the real open mode for write */

/* bits 4, 5, 6 sharing modes */
#define O_SHAREMASK     0x0070  /* mask to isolate shared bits  */

#define O_DENYALL       0x0010  /* sharing bits                 */
#define O_DENYWRITE     0x0020  /*     "      "                 */
#define O_DENYREAD      0x0030  /*     "      "                 */
#define O_DENYNONE      0x0040  /*     "      "                 */
#define O_NETFCB        0x0070  /* networked fcb                */

#define O_NOINHERIT     0x0080
#define O_OPEN          0x0100 /* not     */
#define O_TRUNC         0x0200 /*    both */
#define O_CREAT         0x0400
#define O_LEGACY        0x0800
#define O_LARGEFILE     0x1000
#define O_NOCRIT        0x2000
#define O_SYNC          0x4000
#define O_FCB           0x8000

// ----- in DOS_device.cpp -----

extern int DeviceOpenSft(const char *fname, SFT *sftp);
extern int DeviceWriteSft(SFT *s, int n, u8 *bp);
extern int DosDevIOctl(int al);

// ----- in DOS_kernel.cpp -----

extern int StartShell(char *comexe);

// ----- in DOS_mem.cpp -----

extern int DosMemAlloc(u16 size, u16 mode, u16 *para, u16 *asize);
extern int DosMemFree(u16 seg);
extern int DosMemAlloc(u16 size, u16 mode, u16 *para, u16 *asize);
extern int DosMemLargest(u16 *size);
extern int DosMemChange(u16 para, u16 size, u16 *maxSize);
extern int FreeProcessMem(u16 ps);

// ----- in DOS_task.cpp -----

extern int DosExec(u16 mode, EXEC_BLK *ep, char *lp);
extern void new_psp(u16 para, u16 cur_psp);
extern void child_psp(u16 para, u16 cur_psp, int psize);
extern void return_user(int tsr);

// ----- in DOS_file.cpp -----

extern void Name83ToSz(char *to, const char *from);
extern int truename(const char *src, char *dest, int Wildcards);
extern int SetPSPMaxFiles(int cnt);
extern SFT *idx_to_sft(int sftIndex);
extern int get_sft_idx(int hndl);
extern int DosOpenSft(char *fname, u16 flags, u16 attrib);
extern int DosMkTmp(char * pathname, u16 attr);
extern int DosOpen(char *fname, u16 flags, u16 attrib);
extern int CloneHandle(int hndl);
extern int DosDup(int Handle);
extern int DosForceDup(int OldHandle, int NewHandle);
extern int DosCloseSft(int sft_idx, bool commitonly);
extern int DosClose(int hndl);
extern int DosFlush(int hndl);
extern int SftSeek(int sft_idx, int new_pos, u16 mode);
extern int DosSeek(int hndl, int new_pos, u16 mode);
extern int DosReadSft(int sft_idx, int n, void *bp);
extern int DosRead(int hndl, int n, void *bp);
extern int DosWriteSft(int sft_idx, int n, void *bp);
extern int DosWrite(int hndl, int n, void *bp);
extern int SftGetFSize(int sft_idx);
extern int DosGetFtime(int sft_idx, u16 *date, u16 *time);
extern int DosGetFAttr(char *fname);
extern int DosDevInfo(int sft_idx);
extern int DosFindFirst(const char *fname, u16 attr);
extern int DosFindNext(void);
extern void DosGetFreeDisk(int *sectorsPerCluster, int *freeClusters, int *bytesPerSector, int *totalClusters);

extern int DosChangeDir(char *dir);
extern int DosGetCuDir(int drive, char *path);
extern int DosDelete(const char *fname);
extern int DosRename(const char *fname, const char *tname);
extern int DosMkRmdir(const char *fname, int ah);
extern void InitFiles(const char *fdir, const char *edir);
extern void BuildDPB(DPB *dpb);
extern void bpb_to_dpb(BPB *bpbp, DPB *dpbp);
extern int DosName(char *out, char *in);

// ----- in DOS_fcb.cpp -----

extern void FcbParseFname(int al, const char *lpFileName, FCB *lpFcb);
extern int FcbFind(XFCB *lpXfcb, int First);
extern int FcbOpen(XFCB *lpXfcb, unsigned flags);
extern int FcbReadWrite(XFCB *lpXfcb, int recno, int mode);
extern int FcbRandomBlockIO(XFCB *lpXfcb, int *nRecords, int mode);
extern int FcbRandomIO(XFCB *lpXfcb, int mode);
extern int FcbClose(XFCB *lpXfcb);

typedef struct DOSFILE {
	int			handle;
	FILE		*file;
	int			sft_count;	// Reference count
} DOSFILE;

#define	EMMFILE		-6

extern bool HaveCD;			// In cdrom.c
extern char HostCD[];		// In cdrom.c

//#pragma pack()
