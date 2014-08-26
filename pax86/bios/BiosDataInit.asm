
	dw	0x03F8									;	40:00	word	COM1 port address
	dw	0x02F8									;	40:02	word	COM2 port address
	dw	0										;	40:04	word	COM3 port address
	dw	0										;	40:06	word	COM4 port address
	dw	0x0378									;	40:08	word	LPT1 port address
	dw	0x0278									;	40:0A	word	LPT2 port address
	dw	0x0378									;	40:0C	word	LPT3 port address
	dw	0										;	40:0E	word	LPT4 port address (except PS/2)
	db 	0x24									;	40:10  2 bytes	Equipment list flags (see INT 11)
												;		|7|6|5|4|3|2|1|0| 40:10 (value in INT 11 register AL)
												;		 | | | | | | | `- IPL diskette installed
												;		 | | | | | | `-- math coprocessor
												;		 | | | | |-+--- old PC system board RAM < 256K
												;		 | | | | | `-- pointing device installed (PS/2)
												;		 | | | | `--- not used on PS/2
												;		 | | `------ initial video mode
												;		 `--------- # of diskette drives, less 1
	db 	0xC8									;		|7|6|5|4|3|2|1|0| 40:11  (value in INT 11 register AH)
												;		 | | | | | | | `- 0 if DMA installed
												;		 | | | | `------ number of serial ports
												;		 | | | `------- game adapter
												;		 | | `-------- not used, internal modem (PS/2)
												;		 `----------- number of printer ports
	db	0
	dw	640										;	40:13	word	Memory size in Kbytes  (see INT 12)
	dw	0
	db	0										;	40:17	byte	Keyboard flag byte 0 (see KB FLAGS)
													;		|7|6|5|4|3|2|1|0| keyboard flag byte 0
													;		 | | | | | | | `--- right shift key depressed
													;		 | | | | | | `---- left shift key depressed
													;		 | | | | | `----- CTRL key depressed
													;		 | | | | `------ ALT key depressed
													;		 | | | `------- scroll-lock is active
													;		 | | `-------- num-lock is active
													;		 | `--------- caps-lock is active
													;		 `---------- insert is active

	db 	0										;	40:18	byte	Keyboard flag byte 1 (see KB FLAGS)
													;
													;		|7|6|5|4|3|2|1|0| keyboard flag byte
													;		 | | | | | | | `--- left CTRL key depressed
													;		 | | | | | | `---- left ALT key depressed
													;		 | | | | | `----- system key depressed and held
													;		 | | | | `------ suspend key has been toggled
													;		 | | | `------- scroll lock key is depressed
													;		 | | `-------- num-lock key is depressed
													;		 | `--------- caps-lock key is depressed
													;		 `---------- insert key is depressed

	db	0										;	40:19	byte	Storage for alternate keypad entry
	dw	0x1E									;	40:1A	word	Offset from 40:00 to keyboard buffer head
	dw	0x1E									;	40:1C	word	Offset from 40:00 to keyboard buffer tail
	resb 32										;	40:1E  32bytes	Keyboard buffer (circular queue buffer)
	
	db	0										;	40:3E	byte	Drive recalibration status
	db	0										;	40:3F	byte	Diskette motor status
	db	0										;	40:40	byte	Motor shutoff counter (decremented by INT 8)
	db	0										;	40:41	byte	Status of last diskette operation (see INT 13,1)
	resb 7										;	40:42  7 bytes	NEC diskette controller status (see FDC)
											
	db	3										;	40:49	byte	Current video mode  (see VIDEO MODE)
	dw	80										;	40:4A	word	Number of screen columns
	dw	0x1000									;	40:4C	word	Size of current video regen buffer in bytes
	dw	0										;	40:4E	word	Offset of current video page in video regen buffer
	resb 16										;	40:50  8 words	Cursor position of pages 1-8, high order byte=row
													;					low order byte=column, changing this data isnt
													;					reflected immediately on the display
	db	0x07									;	40:60	byte	Cursor ending (bottom) scan line (dont modify)
	db	0x06									;	40:61	byte	Cursor starting (top) scan line (dont modify)
	db	0										;	40:62	byte	Active display page number
	dw	0x3D4									;	40:63	word	Base port address for active 6845 CRT controller
													;					3B4h = mono, 3D4h = color
	db	0x29									;	40:65	byte	6845 CRT mode control register value (port 3x8h)
													;					EGA/VGA values emulate those of the MDA/CGA
	db	0x30									;	40:66	byte	CGA current color palette mask setting (port 3d9h)
													;					EGA and VGA values emulate the CGA
	resb 5
	dd	0										;	40:6C	dword	Daily timer counter, equal to zero at midnight;
													;					incremented by INT 8, read/set by INT 1A
	db	0										;	40:70	byte	Clock rollover flag, set when 40:6C exceeds 24hrs
	db	0										;	40:71	byte	BIOS break flag, bit 7 is set if Ctrl-Break was
													;					*ever* hit, set by INT 9
	dw	0										;       40:72   word    Soft reset flag via Ctl-Alt-Del or JMP FFFF:0
													;
													;                       1234h  Bypass memory tests & CRT initialization
													;                       4321h  Preserve memory
													;                       5678h  System suspend
													;                       9ABCh  Manufacturer test
													;                       ABCDh  Convertible POST loop
													;                       ????h  many other values are used during POST
													;
	db	0										;       40:74   byte    Status of last hard disk operation (see INT 13,1)
	db	1										;       40:75   byte    Number of hard disks attached
	db	1										;       40:76   byte    XT fixed disk drive control byte
	db	0										;       40:77   byte    Port offset to current fixed disk adapter
	dd	0										;       40:78  4 bytes  Time-Out value for LPT1,LPT2,LPT3(,LPT4 except PS/2)
	dd	0										;       40:7C  4 bytes  Time-Out value for COM1,COM2,COM3,COM4
	dw	0x001E									;       40:80   word    Keyboard buffer start offset (seg=40h,BIOS 10-27-82)
	dw	0x003E									;       40:82   word    Keyboard buffer end offset (seg=40h,BIOS 10-27-82)
	db	23										;       40:84   byte    Rows on the screen (less 1, EGA+)
	dw	8										;       40:85   word    Point height of character matrix (EGA+)
													;               byte    PCjr: character to be repeated if the typematic
													;                       repeat key takes effect
													;       40:86   byte    PCjr: initial delay before repeat key action begins
													;       40:87   byte    PCjr: current Fn function key number
	db	0x60									;               byte    Video mode options (EGA+)
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ Video mode options (EGA+)
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +-- 1=alphanumeric cursor emulation enabled
													;                ¦ ¦ ¦ ¦ ¦ ¦ +--- 1=video subsystem attached to monochrome
													;                ¦ ¦ ¦ ¦ ¦ +---- reserved
													;                ¦ ¦ ¦ ¦ +----- 1=video subsystem is inactive
													;                ¦ ¦ ¦ +------ reserved
													;                ¦ +--------- video RAM  00-64K  10-192K  01-128K  11-256K
													;                +---------- video mode number passed to INT 10, function 0
													;
	db	0x09									;       40:88   byte    PCjr: third keyboard status byte
													;                       EGA feature bit switches, emulated on VGA
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ EGA feature bit switches (EGA+)
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +-- EGA SW1 config (1=off)
													;                ¦ ¦ ¦ ¦ ¦ ¦ +--- EGA SW2 config (1=off)
													;                ¦ ¦ ¦ ¦ ¦ +---- EGA SW3 config (1=off)
													;                ¦ ¦ ¦ ¦ +----- EGA SW4 config (1=off)
													;                ¦ ¦ ¦ +------ Input FEAT0 (ISR0 bit 5) after output on FCR0
													;                ¦ ¦ +------- Input FEAT0 (ISR0 bit 6) after output on FCR0
													;                ¦ +-------- Input FEAT1 (ISR0 bit 5) after output on FCR1
													;                +--------- Input FEAT1 (ISR0 bit 6) after output on FCR1
													;
	db	0x51									;       40:89   byte    Video display data area (MCGA and VGA)
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ Video display data area (MCGA and VGA)
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +-- 1=VGA is active
													;                ¦ ¦ ¦ ¦ ¦ ¦ +--- 1=gray scale is enabled
													;                ¦ ¦ ¦ ¦ ¦ +---- 1=using monochrome monitor
													;                ¦ ¦ ¦ ¦ +----- 1=default palette loading is disabled
													;                ¦ ¦ ¦ +------ see table below
													;                ¦ ¦ +------- reserved
													;                ¦ +--------  1=display switching enabled
													;                +--------- alphanumeric scan lines (see table below)
													;
													;             Bit7    Bit4      Scan Lines
													;               0       0       350 line mode
													;               0       1       400 line mode
													;               1       0       200 line mode
													;               1       1       reserved
													;
	db	0x0B									;       40:8A   byte    Display Combination Code (DCC) table index (EGA+)
	db	0										;       40:8B   byte    Last diskette data rate selected
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ last diskette data rate selected
													;                ¦ ¦ ¦ ¦ +--------- reserved
													;                ¦ ¦ +------------ last floppy drive step rate selected
													;                +-------------- last floppy data rate selected
													;
													;               Data Rate                       Step Rate
													;               00  500K bps            00  step rate time of 0C
													;               01  300K bps            01  step rate time of 0D
													;               10  250K bps            10  step rate time of 0A
													;               11  reserved            11  reserved
													;
	db	0										;       40:8C   byte    Hard disk status returned by controller
	db	0										;       40:8D   byte    Hard disk error returned by controller
	db	0										;       40:8E   byte    Hard disk interrupt control flag(bit 7=working int)
	db	0										;       40:8F   byte    Combination hard/floppy disk card when bit 0 set
	dd	0										;       40:90  4 bytes  Drive 0,1,2,3 media state
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ drive media state (4 copies)
													;                ¦ ¦ ¦ ¦ ¦ +------- drive/media state (see below)
													;                ¦ ¦ ¦ ¦ +------- reserved
													;                ¦ ¦ ¦ +------- 1=media/drive established
													;                ¦ ¦ +------- double stepping required
													;                +--------- data rate:  00=500K bps    01=300K bps
													;                                       10=250K bps    11=reserved
													;               Bits
													;               210  Drive Media State
													;               000  360Kb diskette/360Kb drive not established
													;               001  360Kb diskette/1.2Mb drive not established
													;               010  1.2Mb diskette/1.2Mb drive not established
													;               011  360Kb diskette/360Kb drive established
													;               100  360Kb diskette/1.2Mb drive established
													;               101  1.2Mb diskette/1.2Mb drive established
													;               110  Reserved
													;               111  None of the above
													;
	db	0										;       40:94   byte    Track currently seeked to on drive 0
	db	0										;       40:95   byte    Track currently seeked to on drive 1
	db	0										;       40:96   byte    Keyboard mode/type
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ Keyboard mode/type
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +--- last code was the E1 hidden code
													;                ¦ ¦ ¦ ¦ ¦ ¦ +---- last code was the E0 hidden code
													;                ¦ ¦ ¦ ¦ ¦ +----- right CTRL key depressed
													;                ¦ ¦ ¦ ¦ +------ right ALT key depressed
													;                ¦ ¦ ¦ +------- 101/102 enhanced keyboard installed
													;                ¦ ¦ +-------- force num-lock if Rd ID & KBX
													;                ¦ +--------- last char was first ID char
													;                +---------- read ID in process
													;
	db	0										;       40:97   byte    Keyboard LED flags
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ Keyboard LED flags
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +--- scroll lock indicator
													;                ¦ ¦ ¦ ¦ ¦ ¦ +---- num-lock indicator
													;                ¦ ¦ ¦ ¦ ¦ +----- caps-lock indicator
													;                ¦ ¦ ¦ ¦ +------ circus system indicator
													;                ¦ ¦ ¦ +------- ACK received
													;                ¦ ¦ +-------- re-send received flag
													;                ¦ +--------- mode indicator update
													;                +---------- keyboard transmit error flag
													;
													;       40:98   dword   Pointer to user wait complete flag
													;       40:9C   dword   User wait Time-Out value in microseconds
													;       40:A0   byte    RTC wait function flag
													;
													;               ¦7¦6¦5¦4¦3¦2¦1¦0¦ INT 15,86 RTC wait function flag
													;                ¦ ¦ ¦ ¦ ¦ ¦ ¦ +--- 1= wait pending
													;                ¦ +-------------- not used
													;                +--------------- 1=INT 15,86 wait time elapsed
													;
													;       40:A1   byte    LANA DMA channel flags
													;       40:A2  2 bytes  Status of LANA 0,1
													;       40:A4   dword   Saved hard disk interrupt vector
													;       40:A8   dword   BIOS Video Save/Override Pointer Table address
													;                       (see VIDEO TABLES)
													;       40:AC  8 bytes  Reserved
													;       40:B4   byte    Keyboard NMI control flags (convertible)
													;       40:B5   dword   Keyboard break pending flags (convertible)
													;       40:B9   byte    Port 60 single byte queue (convertible)
													;       40:BA   byte    Scan code of last key (convertible)
													;       40:BB   byte    NMI buffer head pointer (convertible)
													;       40:BC   byte    NMI buffer tail pointer (convertible)
													;       40:BD  16bytes  NMI scan code buffer (convertible)
													;       40:CE   word    Day counter (convertible and after)
													;       40:F0  16bytes  Intra-Applications Communications Area (IBM Technical
													;                       Reference incorrectly locates this at 50:F0-50:FF)
													;
													;
													;       Address Size       Description   (BIOS/DOS Data Area)
													;
													;       50:00   byte    Print screen status byte
													;                        00 = PrtSc not active,
													;                        01 = PrtSc in progress
													;                        FF = error
													;       50:01  3 bytes  Used by BASIC
													;       50:04   byte    DOS single diskette mode flag, 0=A:, 1=B:
													;       50:05  10bytes  POST work area
													;       50:0F   byte    BASIC shell flag; set to 2 if current shell
													;       50:10   word    BASICs default DS value (DEF SEG)
													;       50:12   dword   Pointer to BASIC INT 1C interrupt handler
													;       50:16   dword   Pointer to BASIC INT 23 interrupt handler
													;       50:1A   dword   Pointer to BASIC INT 24 disk error handler
													;       50:20   word    DOS dynamic storage
													;       50:22  14bytes  DOS diskette initialization table (INT 1E)
													;       50:30   4bytes  MODE command
													;       70:00           I/O drivers from IO.SYS/IBMBIO.COM
													;

