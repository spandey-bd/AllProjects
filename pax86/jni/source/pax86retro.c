//=============================================================================
// pax86retro.c
//
// This is the libretro (http://www.libretro.com) API for PA x86 emulation core.
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

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "pax86.h"
#include "pacpu.h"
#include "libretro.h"

extern void AdlibInit();							// In adlib.S
extern int InitMouse();
extern int InitMemory();
extern int ReleaseMemory();
extern int InitEMS();
extern void InitBIOS();
extern int InitKernel();
extern int InitFiles(const char *fpath, const char *epath);
extern int StartShell(char *comexe);
extern void InitTimers();
extern void ExitTimers();
extern void MouseButtonPressed(int button);
extern void MouseButtonReleased(int button);
extern void MouseMoved(int x, int y);
extern void MouseTouchDown(int button, int x, int y);
extern void MouseTouchUp(int button, int x, int y);
extern void MouseTouchMove(int x, int y);


//===========================================================
// Internal variables
//===========================================================

#define TEXTURE_WIDTH 1024
#define TEXTURE_HEIGHT 512
#define S_PIXELS_SIZE (sizeof(s_pixels[0]) * TEXTURE_WIDTH * TEXTURE_HEIGHT)
#define RGB565(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))

static uint16_t *s_pixels = 0;

/*static*/ pthread_t cpu_tid = 0;					// Currently active cpu thread ID

static pthread_mutex_t s_adlib_buf_mutex;
static pthread_t atid = 0;						// Currently active adlib thread ID

static struct timespec vsynctime;				// Previous render start time

typedef struct RenderData {
	int		pitch;
	u8		*imgtop;
	int		toprows;
	u8		*imgbot;
	int		botrows;
} RenderData;
static RenderData renderdata;

#define ADLIB_BUFFER_SAMPLES	2048
#define	ADLIB_BUFFER_SIZE		(2*ADLIB_BUFFER_SAMPLES)
#define	ADLIB_RUN_SAMPLES		128
#define	ADLIB_TICK_INTERVAL		(ADLIB_RUN_SAMPLES/32)

static short	adlib_buf[ADLIB_BUFFER_SAMPLES*16];		// 16 slots ring buffer
static int		adlib_buf_read = 0;
static int		adlib_buf_write = 0;
static bool		emulation_paused = false;
static char		adlib_quit = 0;
static char		logmsg[1024];

extern char BRUserBreak;

void LogDebug(char *msg);	// Forward declaration

//===========================================================
// libretro variables
//===========================================================

static retro_video_refresh_t video_cb;
static retro_audio_sample_t audio_cb;
static retro_audio_sample_batch_t audio_batch_cb;
static retro_environment_t environ_cb;
static retro_input_poll_t input_poll_cb;
static retro_input_state_t input_state_cb;

// Does the frontend support key event callback?
static bool use_key_cb = false;				
// Does the frontend support audio callback?
static bool use_audio_cb = false;				

//------------------------------------------------------------
// Mapping for keys between libretro keycodes (received from
// the frontend) and PC scan codes used in pax86 core.
//------------------------------------------------------------
static int keymap[RETROK_LAST] = {
	0,    0,    0,    0,    0,    0,    0,    0,	// 0..7 = unknown
/*
   RETROK_BACKSPACE      = 8,
   RETROK_TAB            = 9,
   RETROK_CLEAR          = 12,
   RETROK_RETURN         = 13,
*/
	0x0E, 0x0F, 0x00, 0x00, 0x00, 0x1C, 0x00, 0x00, // 8..15 = BackSpace, Tab, Enter
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 19  = RETROK_PAUSE
	0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, // 27  = RETROK_ESCAPE
/*
   RETROK_SPACE          = 32,
   RETROK_EXCLAIM        = 33,
   RETROK_QUOTEDBL       = 34,
   RETROK_HASH           = 35,
   RETROK_DOLLAR         = 36,
   RETROK_AMPERSAND      = 38,
   RETROK_QUOTE          = 39,
*/
	0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x28, // 32  = RETROK_SPACE, ...
/*
   RETROK_LEFTPAREN      = 40,
   RETROK_RIGHTPAREN     = 41,
   RETROK_ASTERISK       = 42,
   RETROK_PLUS           = 43,
   RETROK_COMMA          = 44,
   RETROK_MINUS          = 45,
   RETROK_PERIOD         = 46,
   RETROK_SLASH          = 47,
*/
	0x00, 0x00, 0x00, 0x00, 0x33, 0x0C, 0x34, 0x35, // 40  = ( ) * + , - . /
	0x0B, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, // 48  = 0 1 2 3 4 5 6 7
	0x09, 0x0A, 0x00, 0x27, 0x00, 0x0D, 0x00, 0x00, // 56  = 8 9 : ; < = > ?

	0x00, 0x1E, 0x30, 0x2E, 0x20, 0x12, 0x21, 0x22, // 64  = @ A B C D E F G
	0x23, 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, // 72  = H I J K L M N O
	0x19, 0x10, 0x13, 0x1F, 0x14, 0x16, 0x2F, 0x11, // 80  = P Q R S T U V W
	0x2D, 0x15, 0x2C, 0x1A, 0x2B, 0x1B, 0x00, 0x00, // 88  = X Y Z [ \ ] ^ _
	0x29, 0x1E, 0x30, 0x2E, 0x20, 0x12, 0x21, 0x22, // 96  = ` a b c d e f g
	0x23, 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, // 104 = h i j k l m n o
	0x19, 0x10, 0x13, 0x1F, 0x14, 0x16, 0x2F, 0x11, // 112 = p q r s t u v w
	0x2D, 0x15, 0x2C, 0x00, 0x00, 0x00, 0x00, 0x53, // 120 = x y z         Delete

	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 

	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
	0,    0,    0,    0,    0,    0,    0,    0,	// 
/*
   RETROK_KP0            = 256,
   RETROK_KP1            = 257,
   RETROK_KP2            = 258,
   RETROK_KP3            = 259,
   RETROK_KP4            = 260,
   RETROK_KP5            = 261,
   RETROK_KP6            = 262,
   RETROK_KP7            = 263,
*/
	0x52, 0x4F, 0x50, 0x51, 0x4B, 0x4C, 0x4D, 0x47, // 256  = 0 1 2 3 4 5 6 7
/*
   RETROK_KP8            = 264,
   RETROK_KP9            = 265,
   RETROK_KP_PERIOD      = 266,
   RETROK_KP_DIVIDE      = 267,
   RETROK_KP_MULTIPLY    = 268,
   RETROK_KP_MINUS       = 269,
   RETROK_KP_PLUS        = 270,
   RETROK_KP_ENTER       = 271,
*/
	0x48, 0x49, 0x53, 0xE035, 0xE037, 0x4A, 0x4E, 0xE01C, // 264  = 8 9 . / * - + Enter
/*
   RETROK_KP_EQUALS      = 272,
   RETROK_UP             = 273,
   RETROK_DOWN           = 274,
   RETROK_RIGHT          = 275,
   RETROK_LEFT           = 276,
   RETROK_INSERT         = 277,
   RETROK_HOME           = 278,
   RETROK_END            = 279,
*/
	0x00, 0xE048, 0xE050, 0xE04D, 0xE04B, 0xE052, 0xE047, 0xE04F,	// 272 .. 279
/*
   RETROK_PAGEUP         = 280,
   RETROK_PAGEDOWN       = 281,
   RETROK_F1             = 282,
   RETROK_F2             = 283,
   RETROK_F3             = 284,
   RETROK_F4             = 285,
   RETROK_F5             = 286,
   RETROK_F6             = 287,
*/
	0xE049, 0xE051, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40,	// 280 .. 287
/*
   RETROK_F7             = 288,
   RETROK_F8             = 289,
   RETROK_F9             = 290,
   RETROK_F10            = 291,
   RETROK_F11            = 292,
   RETROK_F12            = 293,
   RETROK_F13            = 294,
   RETROK_F14            = 295,
*/
	0x41, 0x42, 0x43, 0x44, 0x57, 0x58, 0x00, 0x00,	// 288 .. 295
/*
   RETROK_F15            = 296,

   RETROK_NUMLOCK        = 300,
   RETROK_CAPSLOCK       = 301,
   RETROK_SCROLLOCK      = 302,
   RETROK_RSHIFT         = 303,
*/
	0x00, 0x00, 0x00, 0x00, 0x45, 0x3A, 0x46, 0x36,	// 296 .. 303
/*
   RETROK_LSHIFT         = 304,
   RETROK_RCTRL          = 305,
   RETROK_LCTRL          = 306,
   RETROK_RALT           = 307,
   RETROK_LALT           = 308,
   RETROK_RMETA          = 309,
   RETROK_LMETA          = 310,
   RETROK_LSUPER         = 311,
*/
	0x2A, 0xE01D, 0x1D, 0xE038, 0x38, 0x00, 0x00, 0x00,	// 304 .. 311
/*
   RETROK_RSUPER         = 312,
   RETROK_MODE           = 313,
   RETROK_COMPOSE        = 314,
   RETROK_HELP           = 315,
   RETROK_PRINT          = 316,
   RETROK_SYSREQ         = 317,
   RETROK_BREAK          = 318,
   RETROK_MENU           = 319,
*/
	0x00, 0x00, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00,	// 312 .. 319

};

//===========================================================
// Public pax86 helper variables
//===========================================================

int	VSyncCounter = 0;
int paging_cr3_phys = 0;
char ShowHDDLed = 0;
int MaxScreenX = 640;
int MaxScreenY = 400;

char CurrentEXE[9] = "?";

// Internal debugger API

int BreakSeg = 0;
int BreakOffs = 0;
int BreakValue = 0;
int OrigUSE16RHandler;

//===========================================================
// Internal pax86 functions
//===========================================================

extern void BlinkCursor(uint16_t *pixels);								// In TEXT.S
extern void screen_copy_03(uint16_t *pixels, RenderData *render);		// TEXT 40x25 .. 80x50
extern void screen_copy_04(uint16_t *pixels, RenderData *render);		// CGA 320x200
extern void screen_copy_06(uint16_t *pixels, RenderData *render);		// CGA 640x200
extern void screen_copy_0D(uint16_t *pixels, RenderData *render);		// EGA 320x200
extern void screen_copy_640(uint16_t *pixels, RenderData *render);		// EGA 640x???
extern void screen_copy_13(uint16_t *pixels, RenderData *render);		// MCGA 320x200
extern void screen_copy_ModeX(uint16_t *pixels, RenderData *render);	// Mode-X 320x200 .. 360x480
extern void screen_copy_SVGA(uint16_t *pixels, RenderData *render);		// SVGA 640x400 or 640x480

extern void AdlibRun(short *buf, int tick);			// In adlib.S
extern int SBEmulation(short *buf, int samples);	// In sb.S

#if defined(RPi) || defined(Roku)
extern void SendExitIRQ();
extern void SendKeyboardIRQ();
#endif

extern char *BIOSData;

extern u8 VGAMemoryMode;

#ifdef IOS

#define	CLOCK_MONOTONIC		0
#define	TIMER_ABSTIME		1

extern int clock_gettime(int clk_id, struct timespec *tp);
extern int clock_nanosleep(int clock_id, int flags, const struct timespec *request, struct timespec *remain);

#endif

//------------------------------------------------------------
// Send an error message to the front end.
//------------------------------------------------------------
void SendError(char *fmt, char *param)
{
	struct retro_message msg;
	logmsg[0] = 0;
	if (param != NULL && strchr(fmt, '%'))
		sprintf(logmsg, fmt, param);
	else
		strncat(logmsg, fmt, 1023);
	msg.msg = logmsg;
	msg.frames = 5*60;
	environ_cb(RETRO_ENVIRONMENT_SET_MESSAGE, &msg);
}

//------------------------------------------------------------
//------------------------------------------------------------
static void render_pixels(uint16_t *pixels, RenderData *render)
{
	switch ( BIOSData[0x49] )
	{
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
			screen_copy_03(pixels, render);
			BlinkCursor(pixels);
			break;
		case 0x04:
		case 0x05:
			screen_copy_04(pixels, render);
			break;
		case 0x06:
			screen_copy_06(pixels, render);
			break;
		case 0x0D:
			screen_copy_0D(pixels, render);
			break;
		case 0x0E:
		case 0x0F:
		case 0x10:
		case 0x11:
		case 0x12:
			screen_copy_640(pixels, render);
			break;
		case 0x13:
			if ( 0 == (VGAMemoryMode & 0x08) )				// Chain-Four is OFF, this is Mode-X!
			{
				screen_copy_ModeX(pixels, render);
				//printf("render={%d, %p, %d, %p, %d}, EGAVGA=%p\n",
				//	render->pitch, render->imgtop, render->toprows, render->imgbot, render->botrows, EGAVGA_A000);
			}
			else
				screen_copy_13(pixels, render);
			break;
		case 0x1C:
		case 0x1D:
			screen_copy_SVGA(pixels, render);
			break;
	}
}

#define	KEYBUFSIZE	32

static unsigned char KeyboardBuf[KEYBUFSIZE];
static int KeyBufHead = 0;
static int KeyBufTail = 0;
static int LastKeyDown = 0;
static int RepeatCounter = 0;

extern u8 KeyboardDataByte;
extern u8 KeyboardStatusByte;
extern u8 JoyButtons;
extern u8 JoyValues[4];

#define SHF	0x2A	// Shift
#define	CTL	0x1d	// Ctrl
#define ALT	0x38	// Alt

//------------------------------------------------------------
// Add a key to the high-level keyboard input buffer. Key
// events in this buffer are sent to the core one event by
// every VSync, to allow the core sufficient time to handle
// each key event interrupt.
//------------------------------------------------------------
static void AddKeyToBuf(int k)
{
	if ( k > 256 )		// Extended key?
	{
		KeyboardBuf[KeyBufHead] = k>>8;
		KeyBufHead = (KeyBufHead+1)&(KEYBUFSIZE-1);
	}
	KeyboardBuf[KeyBufHead] = k;
	KeyBufHead = (KeyBufHead+1)&(KEYBUFSIZE-1);
	if ( 0 == (k&0x80) )
	{
		// Key down event, prepare for key repeat
		if ( k != SHF && k != CTL && k != ALT )	// Ignore Shift, Ctrl, Alt keyrepeat
			LastKeyDown = k;
		RepeatCounter = VSyncCounter;
	}
	else if ( LastKeyDown == (k&0xFF7F) )
	{
		// Stop key repeat, key went up
		LastKeyDown = 0;
	}
}

//------------------------------------------------------------
// Thread that runs the CPU core emulation.
//------------------------------------------------------------
void *cpu_thread(void *arg)
{
	//------------------------------------
	// Run the emulation!
	//------------------------------------
	run_core(1);
	//------------------------------------
	// Clean up the CPU thread exit request.
	//------------------------------------
	IRQPending[IRQ_EXIT] = 0;
	//------------------------------------
	// If we are only pausing, no more cleanup needed.
	//------------------------------------
	if (emulation_paused)
		return 0;
	//------------------------------------
	// Stop the timer thread if it is running.
	//------------------------------------
	ExitTimers();
	//------------------------------------
	// Stop the audio thread.
	//------------------------------------
	adlib_quit = 1;
	//------------------------------------
	// Let the front end know of the reason why the CPU core quit,
	// if there was a specific reason.
	//------------------------------------
	if (BreakReason != &BRUserBreak)
	{
		struct retro_message msg;
		LogDebug(logmsg);
		msg.msg = logmsg;
		msg.frames = 5*60;
		environ_cb(RETRO_ENVIRONMENT_SET_MESSAGE, &msg);
	}
	//------------------------------------
	// Request the front end to shut down.
	//------------------------------------
	environ_cb(RETRO_ENVIRONMENT_SHUTDOWN, NULL);
	return 0;
}

//------------------------------------------------------------
// Thread that runs the AdLib emulator. We are playing at 32.000 Hz
// with two 2048-sample buffers, so we are to fill the buffers at 15.625
// times per second (every 64 ms).
// We are handling the AdLib audio using 128-sample buffers, so we
// run it 16 times during the 2048-sample buffer filling.
// Each sample takes 31.25 microseconds, so each 128-sample run
// should take 4000 microseconds.
// Doom plays digital audio at 11.111 Hz using 128-sample buffers, so
// it needs IRQs at 86.80 times per second (every 11.5 ms).
//------------------------------------------------------------
void* adlib_thread(void *arg)
{
	int i, offs, tick;
	int	swap;
	struct timespec time;
#ifdef Roku
	struct timespec newtime;
#endif
	//LOGI("adlib_thread started\n");
	clock_gettime(CLOCK_MONOTONIC, &time);	// Get the current (starting) time.
	while (1)
	{
		swap = adlib_buf_write;
		for (i = 0; i < ADLIB_BUFFER_SAMPLES; i+=ADLIB_RUN_SAMPLES)
		{
			// Call the actual adlib buffer generation routine.
			AdlibRun(adlib_buf + swap + i, 0);
			// Handle SoundBlaster (and PC Speaker) emulation
			offs = SBEmulation(adlib_buf + swap + i, ADLIB_RUN_SAMPLES);
			// Wait until 4 ms has passed
			time.tv_nsec += 4000000;
			if (time.tv_nsec >= 1000000000)
			{
				time.tv_sec += 1;
				time.tv_nsec -= 1000000000;
			}
#ifdef Roku
			do {
				usleep(1000);	// Sleep for 1000 us = 1 ms
				clock_gettime(CLOCK_MONOTONIC, &newtime);
				if (adlib_quit)
					break;
			} while (newtime.tv_nsec < time.tv_nsec 
					? newtime.tv_sec == time.tv_sec
					: newtime.tv_sec < time.tv_sec);
#else
			clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &time, NULL);
#endif
			// Quit if we were requested to quit
			if (adlib_quit)
				break;
			if (offs > 0)	// If the previous buffer has room for more SB samples..
				SBEmulation(adlib_buf + swap + i + (ADLIB_RUN_SAMPLES - offs), offs);
		}
		// Quit if we were requested to quit
		if (adlib_quit)
			break;
		// Go to the next slot in the ring buffer
		pthread_mutex_lock(&s_adlib_buf_mutex);
		adlib_buf_write = (adlib_buf_write + ADLIB_BUFFER_SAMPLES) & 32767;
		pthread_mutex_unlock(&s_adlib_buf_mutex);
	}
	return NULL;
}

void pause_emulation()
{
	emulation_paused = true;
	//------------------------------------
	// Stop the audio thread, if it is running.
	//------------------------------------
	if (atid)
	{
		// Stop the audio thread.
		adlib_quit = 1;
		// Wait for the audio thread to finish
		pthread_join(atid, NULL);	
	}
	atid = 0;
	//------------------------------------
	// Stop the CPU thread if it is running.
	//------------------------------------
	if (cpu_tid && IRQFlagAddr)
	{
		// Stop the CPU thread.
#if defined(RPi) || defined(Roku)
		SendExitIRQ();
#else
		IRQRequest(IRQ_EXIT);
#endif
		pthread_join(cpu_tid, NULL);	
		IRQPending[IRQ_EXIT] = 0;
	}
	cpu_tid = 0;
	//------------------------------------
	// Stop the timer thread if it is running.
	// This needs to be last, as it also destroys
	// the IRQ mutexes needed in CPU core exit!
	//------------------------------------
	ExitTimers();
}

extern int cpu_gdt_phys;
extern int cpu_ldt_phys;
extern int cpu_cr0;
extern int cpu_big;
extern int stack_mask;

#define	RII_EX	1
#ifdef RII_EX

//------------------------------------------------------------
// CPU and audio serialization stuff
//------------------------------------------------------------

// From cpu_prot.S:
extern int cpu_cr2;
extern int cpu_cr3;
extern int cpu_tss_selector;
extern int cpu_tss_base;
extern int cpu_tss_limit;
extern int cpu_tss_valid;
extern int cpu_tss_phys;
extern int cpu_tss_is386;
extern int cpu_ldt_value;
extern int cpu_gdt_base;
extern int cpu_ldt_base;
extern int cpu_gdt_limit;
extern int cpu_ldt_limit;
extern int cpu_idt_base;
extern int cpu_idt_limit;
extern int cpu_idt_phys;
extern int cpu_cpl;

// From pic.S:
extern u8 pic_serialize_start;
extern u8 pic_serialize_end;

// From ports.S:
extern int DMAAddress;
extern int DMALength;
extern int DMACurrent;
extern u8 DMAFlipFlop;
extern u8 Port61Data;
extern u8 Port92Data;

// Return the worst-case size of the cpu serialization data.
int cpu_serialize_size()
{
	return 4*20 + 4*22 + (&pic_serialize_end - &pic_serialize_start) +
		3*4 + 3*1;

}

// Serialize the audio variables into data + offset.
int cpu_serialize(u8 *data, int offset)
{
	int i, tmp;
	int tmpreg[20];
	u8 *ptr;

	// First the CPU registers
	memcpy(tmpreg, registers, 4*20);
	tmpreg[8] -= (int)INTVectors;		// CS:EIP from physical to logical address
	memcpy(data + offset, tmpreg, 4*20);
	offset += 4*20;

	// Then the protected mode stuff
	memcpy(data + offset, &cpu_cr0, 4); offset += 4;
	memcpy(data + offset, &cpu_cr2, 4); offset += 4;
	memcpy(data + offset, &cpu_cr3, 4); offset += 4;

	memcpy(data + offset, &cpu_tss_selector, 4); offset += 4;
	memcpy(data + offset, &cpu_tss_base, 4); offset += 4;
	memcpy(data + offset, &cpu_tss_limit, 4); offset += 4;
	memcpy(data + offset, &cpu_tss_valid, 4); offset += 4;
	tmp = cpu_tss_phys - (int)INTVectors;
	memcpy(data + offset, &tmp, 4); offset += 4;
	memcpy(data + offset, &cpu_tss_is386, 4); offset += 4;

	memcpy(data + offset, &cpu_ldt_value, 4); offset += 4;
	memcpy(data + offset, &cpu_gdt_base, 4); offset += 4;
	memcpy(data + offset, &cpu_ldt_base, 4); offset += 4;
	memcpy(data + offset, &cpu_gdt_limit, 4); offset += 4;
	memcpy(data + offset, &cpu_ldt_limit, 4); offset += 4;
	tmp = cpu_gdt_phys - (int)INTVectors;
	memcpy(data + offset, &tmp, 4); offset += 4;
	tmp = cpu_ldt_phys - (int)INTVectors;
	memcpy(data + offset, &tmp, 4); offset += 4;
	memcpy(data + offset, &cpu_idt_base, 4); offset += 4;
	memcpy(data + offset, &cpu_idt_limit, 4); offset += 4;
	tmp = cpu_idt_phys - (int)INTVectors;
	memcpy(data + offset, &tmp, 4); offset += 4;

	memcpy(data + offset, &cpu_cpl, 4); offset += 4;
	memcpy(data + offset, &cpu_big, 4); offset += 4;
	memcpy(data + offset, &stack_mask, 4); offset += 4;

	// Then the PIC variables.
	memcpy(data + offset, &pic_serialize_start, &pic_serialize_end - &pic_serialize_start);
	offset += (&pic_serialize_end - &pic_serialize_start);

	// Then the port variables
	memcpy(data + offset, &DMAAddress, 4); offset += 4;
	tmp = DMACurrent - (int)INTVectors;
	memcpy(data + offset, &tmp, 4); offset += 4;
	memcpy(data + offset, &DMALength, 4); offset += 4;
	memcpy(data + offset, &DMAFlipFlop, 1); offset += 1;
	memcpy(data + offset, &Port61Data, 1); offset += 1;
	memcpy(data + offset, &Port92Data, 1); offset += 1;

	return offset;
}

// Unserialize the audio variables from data + offset
int cpu_unserialize(u8 *data, int offset)
{
	int i, tmp;
	int tmpreg[20];
	u8 *ptr;

	// First the CPU registers
	memcpy(tmpreg, data + offset, 4*20);
	offset += 4*20;
	memcpy(registers, tmpreg, 4*20);
	registers[8] += (int)INTVectors;		// CS:EIP from logical to physical address

	// Then the protected mode stuff
	memcpy(&cpu_cr0, data + offset, 4); offset += 4;
	memcpy(&cpu_cr2, data + offset, 4); offset += 4;
	memcpy(&cpu_cr3, data + offset, 4); offset += 4;

	memcpy(&cpu_tss_selector, data + offset, 4); offset += 4;
	memcpy(&cpu_tss_base, data + offset, 4); offset += 4;
	memcpy(&cpu_tss_limit, data + offset, 4); offset += 4;
	memcpy(&cpu_tss_valid, data + offset, 4); offset += 4;
	memcpy(&tmp, data + offset, 4); offset += 4;
	cpu_tss_phys = tmp + (int)INTVectors;
	memcpy(&cpu_tss_is386, data + offset, 4); offset += 4;

	memcpy(&cpu_ldt_value, data + offset, 4); offset += 4;
	memcpy(&cpu_gdt_base, data + offset, 4); offset += 4;
	memcpy(&cpu_ldt_base, data + offset, 4); offset += 4;
	memcpy(&cpu_gdt_limit, data + offset, 4); offset += 4;
	memcpy(&cpu_ldt_limit, data + offset, 4); offset += 4;
	memcpy(&tmp, data + offset, 4); offset += 4;
	cpu_gdt_phys = tmp + (int)INTVectors;
	memcpy(&tmp, data + offset, 4); offset += 4;
	cpu_ldt_phys = tmp + (int)INTVectors;
	memcpy(&cpu_idt_base, data + offset, 4); offset += 4;
	memcpy(&cpu_idt_limit, data + offset, 4); offset += 4;
	memcpy(&tmp, data + offset, 4); offset += 4;
	cpu_idt_phys = tmp + (int)INTVectors;

	memcpy(&cpu_cpl, data + offset, 4); offset += 4;
	memcpy(&cpu_big, data + offset, 4); offset += 4;
	memcpy(&stack_mask, data + offset, 4); offset += 4;

	// Then the PIC variables.
	memcpy(&pic_serialize_start, data + offset, &pic_serialize_end - &pic_serialize_start);
	offset += (&pic_serialize_end - &pic_serialize_start);

	// Then the port variables
	memcpy(&DMAAddress, data + offset, 4); offset += 4;
	memcpy(&tmp, data + offset, 4); offset += 4;
	DMACurrent = tmp + (int)INTVectors;
	memcpy(&DMALength, data + offset, 4); offset += 4;
	memcpy(&DMAFlipFlop, data + offset, 1); offset += 1;
	memcpy(&Port61Data, data + offset, 1); offset += 1;
	memcpy(&Port92Data, data + offset, 1); offset += 1;

	return offset;
}

extern u8 adlib_serialize_start;
extern u8 adlib_serialize_end;
extern u8 SLOT1;
extern u8 SLOT2;
extern u8 ch0_slot1_wavetable;
extern u8 sin_tab;
extern u8 sb_cmd_serialize_start;
extern u8 sb_cmd_serialize_end;
extern u8 sb_play_serialize_start;
extern u8 sb_play_serialize_end;
extern int sb_request_pointer;
extern int sb_buffer_start;
extern int sb_buffer_end;

// Return the worst-case size of the audio serialization data.
int audio_serialize_size()
{
	return (&adlib_serialize_end - &adlib_serialize_start) + 
		   sizeof(adlib_buf) + 4 + 4 +
		   (&sb_cmd_serialize_end - &sb_cmd_serialize_start) +
		   (&sb_play_serialize_end - &sb_play_serialize_start);
}

// Serialize the audio variables into data + offset.
int audio_serialize(u8 *data, int offset)
{
	int i, tmp;
	u8 *ptr;

	// First serialize the AdLib data structures.
	// Fix the SLOT.wavetable physical addresses to be logical addresses.
	memcpy(data + offset, &adlib_serialize_start, &adlib_serialize_end - &adlib_serialize_start);
	ptr = (data + offset + (&ch0_slot1_wavetable - &adlib_serialize_start));
	for (i = 0; i < 2*9; i++)
	{
		memcpy(&tmp, ptr, 4);
		tmp -= (int)sin_tab;
		memcpy(ptr, &tmp, 4);
		ptr += (&SLOT2 - &SLOT1);
	}
	offset += (&adlib_serialize_end - &adlib_serialize_start);
	memcpy(data + offset, adlib_buf, sizeof(adlib_buf));
	offset += sizeof(adlib_buf);
	memcpy(data + offset, &adlib_buf_read, 4);
	offset += 4;
	memcpy(data + offset, &adlib_buf_write, 4);
	offset += 4;
	// Then the SB playing data structures. Fix the physical pointers.
	memcpy(data + offset, &sb_play_serialize_start, &sb_play_serialize_end - &sb_play_serialize_start);
	ptr = (data + offset + (((u8 *)&sb_request_pointer) - &sb_play_serialize_start));
	tmp = sb_request_pointer - (int)INTVectors;
	memcpy(ptr, &tmp, 4);
	ptr = (data + offset + (((u8 *)&sb_buffer_start) - &sb_play_serialize_start));
	tmp = sb_buffer_start - (int)INTVectors;
	memcpy(ptr, &tmp, 4);
	ptr = (data + offset + (((u8 *)&sb_buffer_end) - &sb_play_serialize_start));
	tmp = sb_buffer_end - (int)INTVectors;
	memcpy(ptr, &tmp, 4);
	offset += (&sb_play_serialize_end - &sb_play_serialize_start);
	// Then the SB command data structures.
	memcpy(data + offset, &sb_cmd_serialize_start, &sb_cmd_serialize_end - &sb_cmd_serialize_start);
	offset += (&sb_cmd_serialize_end - &sb_cmd_serialize_start);

	return offset;
}

// Unserialize the audio variables from data + offset
int audio_unserialize(u8 *data, int offset)
{
	int i, *ptr;

	// First unserialize the AdLib data structures.
	// Fix the SLOT.wavetable logical addresses to be physical addresses.
	memcpy(&adlib_serialize_start, data + offset, &adlib_serialize_end - &adlib_serialize_start);
	ptr = (int *)&ch0_slot1_wavetable;
	for (i = 0; i < 2*9; i++)
	{
		*ptr = *ptr + (int)sin_tab;
		ptr += ((&SLOT2 - &SLOT1)>>2);
	}
	offset += (&adlib_serialize_end - &adlib_serialize_start);
	memcpy(adlib_buf, data + offset, sizeof(adlib_buf));
	offset += sizeof(adlib_buf);
	memcpy(&adlib_buf_read, data + offset, 4);
	offset += 4;
	memcpy(&adlib_buf_write, data + offset, 4);
	offset += 4;
	// Then the SB playing data structures. Fix the physical pointers.
	memcpy(&sb_play_serialize_start, data + offset, &sb_play_serialize_end - &sb_play_serialize_start);
	offset += (&sb_play_serialize_end - &sb_play_serialize_start);
	sb_request_pointer += (int)INTVectors;
	sb_buffer_start += (int)INTVectors;
	sb_buffer_end += (int)INTVectors;
	// Then the SB command data structures.
	memcpy(&sb_cmd_serialize_start, data + offset, &sb_cmd_serialize_end - &sb_cmd_serialize_start);
	offset += (&sb_cmd_serialize_end - &sb_cmd_serialize_start);

	return offset;
}

#endif

//===========================================================
// libretro API callbacks.
//===========================================================

//------------------------------------------------------------
// The libretro frontend calls this callback for every key
// event (key down, key up).
//------------------------------------------------------------
void retro_keyboard_event(bool down, unsigned keycode, uint32_t character, uint16_t key_modifiers)
{
	int k = keymap[keycode];
	if (k)
		AddKeyToBuf(k|(down?0:0x80));
}

//------------------------------------------------------------
// The libretro frontend calls this callback whenever the
// audio buffer is ready to receive more data.
//------------------------------------------------------------
void retro_audio_event()
{
	if (audio_batch_cb)
	{
		int tmp_write;
		// Try to write 2048 samples in one go.
		size_t done = audio_batch_cb(adlib_buf + adlib_buf_read, ADLIB_BUFFER_SAMPLES);
		// Switch the next read to happen from the next buffer, but only if it is ready.
		pthread_mutex_lock(&s_adlib_buf_mutex);
		tmp_write = adlib_buf_write;
		pthread_mutex_unlock(&s_adlib_buf_mutex);
		if (tmp_write < adlib_buf_read)
			tmp_write += 32768;
		if (adlib_buf_read + done < tmp_write)
			adlib_buf_read = (adlib_buf_read + done) & 32767;
	}
}

//===========================================================
// libretro API function handlers.
//===========================================================

unsigned retro_api_version(void)
{
	return RETRO_API_VERSION;
}

void retro_init(void)
{
	enum retro_pixel_format fmt;
	pthread_mutex_init(&s_adlib_buf_mutex, NULL);
	VSyncCounter = 0;
	AdlibInit();			// Initialize the AdLib slots and variables.
	fmt = RETRO_PIXEL_FORMAT_RGB565;
	environ_cb(RETRO_ENVIRONMENT_SET_PIXEL_FORMAT, &fmt);	// TODO! Check return value..
}

void retro_deinit(void)
{
	pthread_mutex_destroy(&s_adlib_buf_mutex);
}

void retro_set_controller_port_device(unsigned port, unsigned device)
{
}

void retro_get_system_info(struct retro_system_info *info)
{
	info->library_name = "pax86 x86 emulator core for ARM targets by Patrick Aalto";
	info->library_version = "0.01";
	info->valid_extensions = "exe|com";
	info->need_fullpath = true;
	info->block_extract = false;
}

void retro_get_system_av_info(struct retro_system_av_info *info)
{
   info->timing.fps = 60.0;
   info->timing.sample_rate = 32000.0;
   info->geometry.base_width = 640;
   info->geometry.base_height = 480;
   info->geometry.max_width = 640;
   info->geometry.max_height = 480;
   info->geometry.aspect_ratio = 4.0 / 3.0;
}

void retro_set_environment(retro_environment_t cb)
{
	struct retro_keyboard_callback key_cb;
	struct retro_audio_callback audio_cb;

	environ_cb = cb;

	// Try to use key event callback if the frontend supports it.
	key_cb.callback = retro_keyboard_event;
	use_key_cb = environ_cb(RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK, &key_cb);
	// Try to use the audio callback if the frontend supports it.
	audio_cb.callback = retro_audio_event;
	use_audio_cb = environ_cb(RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK, &audio_cb);
}

void retro_set_audio_sample(retro_audio_sample_t cb)
{
   audio_cb = cb;
}

void retro_set_audio_sample_batch(retro_audio_sample_batch_t cb)
{
   audio_batch_cb = cb;
}

void retro_set_input_poll(retro_input_poll_t cb)
{
   input_poll_cb = cb;
}

void retro_set_input_state(retro_input_state_t cb)
{
   input_state_cb = cb;
}

void retro_set_video_refresh(retro_video_refresh_t cb)
{
   video_cb = cb;
}

bool retro_load_game(const struct retro_game_info *info)
{
	// Allocate the memory we need.
	s_pixels = malloc(S_PIXELS_SIZE);
	if (s_pixels == NULL)
		return false;	// Unable to allocate pixel buffer!

	InitMouse();
	if (InitMemory())
	{
		free(s_pixels);
		s_pixels = NULL;
		return false;	// Unable to allocate 16MB of RAM!
	}

	// Initialize all additional stuff.
	InitEMS();
	InitBIOS();

	InitKernel();
	// Init file paths with anything illegal.
	// They will get overwritten by the StartShell parameter below.
	InitFiles("***", "***");	

	emulation_paused = false;

	// Load the game from disk to memory.
	return StartShell((char *)(info->path)) != -1;
}

void retro_unload_game(void)
{
	pause_emulation();
	//------------------------------------
	// Free all allocated memory
	//------------------------------------
	ReleaseMemory();
	free(s_pixels);
	s_pixels = NULL;
	VSyncCounter = 0;
}

void retro_run(void)
{
	int touchX, touchY, touchDown, mouseX, mouseY, mouseLB, mouseRB;
	static int oldTouchX = 0;
	static int oldTouchY = 0;
	static int oldTouchDown = 0;
	static int oldMouseLB = 0;
	static int oldMouseRB = 0;
	
	if (!emulation_paused)
	{
		if (VSyncCounter == 0)
		{
			adlib_quit = 0;
			//------------------------------------
			// Start up the timer thread.
			//------------------------------------
			InitTimers();
			//------------------------------------
			// Start up the CPU core thread.
			//------------------------------------
			pthread_create(&cpu_tid, NULL, cpu_thread, NULL);
			//------------------------------------
			// Start up the audio emulation thread.
			//------------------------------------
			pthread_create(&atid, NULL, adlib_thread, NULL);
		}
		VSyncCounter++;

		render_pixels(s_pixels, &renderdata);
		
		input_poll_cb();	// Let the frontend prepare the mouse positions
		
		// Mouse event, relative to previous callback
		mouseX = input_state_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_X);
		mouseY = input_state_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_Y);
		mouseLB = input_state_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_LEFT);
		mouseRB = input_state_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_RIGHT);
		// Touch event, -0x7FFF = far top left, 0x7FFF = far bottom right
		touchX = input_state_cb(1, RETRO_DEVICE_POINTER, 0, RETRO_DEVICE_ID_POINTER_X);
		touchY = input_state_cb(1, RETRO_DEVICE_POINTER, 0, RETRO_DEVICE_ID_POINTER_Y);
		touchDown = input_state_cb(1, RETRO_DEVICE_POINTER, 0, RETRO_DEVICE_ID_POINTER_PRESSED);
		
		if (mouseLB != oldMouseLB || mouseRB != oldMouseRB || mouseX != 0 || mouseY != 0)
		{
			// Mouse moved or clicked.
			if (mouseX != 0 || mouseY != 0)
				MouseMoved(mouseX, mouseY);
			if (mouseLB != oldMouseLB)
			{
				if (mouseLB)
					MouseButtonPressed(0);
				else
					MouseButtonReleased(0);
			}
			if (mouseRB != oldMouseRB)
			{
				if (mouseRB)
					MouseButtonPressed(1);
				else
					MouseButtonReleased(1);
			}
			oldMouseLB = mouseLB;
			oldMouseRB = mouseRB;
		}
		else if (touchDown != oldTouchDown || touchX != oldTouchX || touchY != oldTouchY)
		{
			// The correct scaling here depends on the game!
			// Grand Prix 2: 640x480
			// Stalingrad and MOO2: 1280x960.
			// X-COM2: 320x200
			int sx = 1280;
			int sy = 960;
			sx = (touchX + 0x8000) * sx / 65536;
			sy = (touchY + 0x8000) * sy / 65536;
			if (touchDown && !oldTouchDown)
				MouseTouchDown(0, sx, sy);
			else if (!touchDown && oldTouchDown)
				MouseTouchUp(0, sx, sy);
			else
				MouseTouchMove(sx, sy);
			oldTouchDown = touchDown;
			oldTouchX = touchX;
			oldTouchY = touchY;
		}
	}

	video_cb(s_pixels, MaxScreenX, MaxScreenY, TEXTURE_WIDTH);

	if ( KeyBufTail != KeyBufHead /*&& (KeyboardStatusByte&1) == 0*/)
	{
		KeyboardDataByte = KeyboardBuf[KeyBufTail];
		KeyBufTail = (KeyBufTail+1)&(KEYBUFSIZE-1);
		KeyboardStatusByte = 16+4+1;	// OutputBuffer Full, Data Available, Keyboard Active
#if defined(RPi) || defined(Roku)
		SendKeyboardIRQ();
#else
		IRQRequest(IRQ_KEYB);
#endif
	}

	clock_gettime(CLOCK_MONOTONIC, &vsynctime);	// Get current time
}

unsigned retro_get_region(void)
{
	return RETRO_REGION_NTSC;	// Not relevant...
}

bool retro_load_game_special(unsigned type, const struct retro_game_info *info, size_t num)
{
	return false;
}

size_t retro_serialize_size(void)
{
#ifdef RII_EX
	return mem_serialize_size() + cpu_serialize_size() +
		files_serialize_size() + ems_serialize_size() + 
		audio_serialize_size() + vga_serialize_size() + timer_serialize_size();
#else
	return 0;
#endif
}

bool retro_serialize(void *data_, size_t size)
{
#ifdef RII_EX
	int offs = 0;
	u8 *data = (u8 *)data_;
	if (size < retro_serialize_size())
		return false;
	// Pause the emulation before we take a snapshot of the state.
	pause_emulation();
	offs = mem_serialize(data, offs);		// Serialize the main memory
	offs = cpu_serialize(data, offs);		// Serialize the CPU data
	offs = files_serialize(data, offs);		// Serialize the DOS files
	offs = ems_serialize(data, offs);		// Serialize the EMS memory
	offs = audio_serialize(data, offs);		// Serialize the audio variables
	offs = vga_serialize(data, offs);		// Serialize the VGA memory
	offs = timer_serialize(data, offs);		// Serialize the timers
	//LOGI("serialize done, offs=%d!\n", offs);
	// The next call to retro_run() will restart the emulation.
	VSyncCounter = 0;
	emulation_paused = false;
	return true;
#else
	return false;
#endif
}

bool retro_unserialize(const void *data_, size_t size)
{
#ifdef RII_EX
	int offs = 0;
	u8 *data = (u8 *)data_;
	if (size < retro_serialize_size())
		return false;
	// Pause the emulation before we take a snapshot of the state.
	pause_emulation();
	// If we already have initialized the memory, we need to close currently
	// open DOS files before we can start with the unserialization!
	if (s_pixels)
		files_close_before_unserialize();
	offs = mem_unserialize(data, offs);		// Unserialize the main memory
	if (offs < 0)
		return false;
	offs = cpu_unserialize(data, offs);		// Unserialize the CPU data
	offs = files_unserialize(data, offs);	// Unserialize the DOS files
	offs = ems_unserialize(data, offs);		// Unserialize the EMS memory
	offs = audio_unserialize(data, offs);	// Unserialize the audio variables
	offs = vga_unserialize(data, offs);		// Unserialize the VGA memory
	offs = timer_unserialize(data, offs);	// Unserialize the timers
	// Next retro_run() will restart the threads.
	VSyncCounter = 0;
	emulation_paused = false;
	return true;
#else
	return false;
#endif
}

void *retro_get_memory_data(unsigned id)
{
	return NULL;
}

size_t retro_get_memory_size(unsigned id)
{
	return 0;
}

void retro_cheat_reset(void)
{
}

void retro_cheat_set(unsigned index, bool enabled, const char *code)
{
}

//===========================================================
// Public helper functions
//===========================================================

/* wait for the screen to redraw */
void wait_vsync()
{
}

extern u8 VGA_attr_addr_3C0;

// Get the display status bits (is display in VSync or HSync)
int in_3DA_status(int csip)
{
	struct timespec curtime = {0, 0};			// Previous render start time
	//-------
	// Clear the attribute register flipflop value.
	//-------
	VGA_attr_addr_3C0 = 0xFF;
	//-------
	// Determine how much time has passed since the vsynctime.
	// The vsynctime gets set whenever we have copied the screen data and are just
	// waiting for the real hardware VSync to happen, so we can fake that we are in
	// VSync period when this has just happened.
	//-------
	clock_gettime(CLOCK_MONOTONIC, &curtime);	// Get current time
	int nsecdiff = curtime.tv_nsec - vsynctime.tv_nsec;
	if (nsecdiff < 0)
		nsecdiff += 1000000000;
	//-------
	// Since the real VSync will happen with a period of 16.666.666 nanoseconds (60Hz),
	// we fake the VBlank interval to happen when less than 5.000.000 nanoseconds
	// have passed since we reset the vsynctimer.
	// HSync will happen 400 times faster (every 41.666 nanoseconds)
	//-------
	return (nsecdiff > 8000000) ? (8+1) : ((((nsecdiff>>10) & 31) < 20) ? 0 : 1);
}

int UseConfig( const char *name ) { return 0; }
void CursorToSprite(u16 *mask) { }

#define GDT(idx) ((u8)(((char *)cpu_gdt_phys)[idx]))
#define LDT(idx) ((u8)(((char *)cpu_ldt_phys)[idx]))

u8* phys_ptr(int seg, int offs)
{
	if (0xF000 == seg)
		return (u8*)(BIOS_F000 + offs);
	else if (REG_FLAGS & FLAG_VM)
	{
		// V8086 mode, so EMSPages[] might not be initialized for the requested page.
		offs = (seg<<4)+(offs&0xFFFF);
		int base = EMSPages[offs>>EMSPAGESHIFT]<<4;
		if (0 == base)
		{
			// Uninitialized page, so use the paging tables.
			int page = *(int *)(paging_cr3_phys + (offs>>(32-10))*4);						// Directory entry
			page = *(int *)((int)INTVectors + (page&0xFFFFF000) + ((offs>>12)&0x3FF)*4);	// Page table entry
			return (u8 *)((int)INTVectors + (page&0xFFFFF000) + (offs&0xFFF));				// Physical address
		}
		else
			return ((u8 *)(base+offs));
	}
	else if (cpu_cr0&1)
	{
		// Protected mode, get the segment base address by the segment selector
		int s = seg&0xFFF8;
		if (cpu_cr0 < 0)
		{
			// Paging active, use the EMSPages table
			if (s > 0)
			{
				if (seg&4)
					offs += (LDT(s+2)|(LDT(s+3)<<8)|(LDT(s+4)<<16)|(LDT(s+7)<<24));
				else
					offs += (GDT(s+2)|(GDT(s+3)<<8)|(GDT(s+4)<<16)|(GDT(s+7)<<24));
			}
			seg = EMSPages[((u32)offs)>>EMSPAGESHIFT]<<4;
			if (0 == seg)
			{
				// Uninitialized page, so use the paging tables.
				int page = *(int *)(paging_cr3_phys + (offs>>(32-10))*4);						// Directory entry
				page = *(int *)((int)INTVectors + (page&0xFFFFF000) + ((offs>>12)&0x3FF)*4);	// Page table entry
				return (u8 *)((int)INTVectors + (page&0xFFFFF000) + (offs&0xFFF));				// Physical address
			}
			else
				return (u8 *)((seg&0xFFFFF000) + offs);
		}
		else
		{
			// Paging inactive, use the LDT or GDT directly.
			if (s > 0)
			{
				if (seg&4)
					offs += (LDT(s+2)|(LDT(s+3)<<8)|(LDT(s+4)<<16)|(LDT(s+7)<<24));
				else
					offs += (GDT(s+2)|(GDT(s+3)<<8)|(GDT(s+4)<<16)|(GDT(s+7)<<24));
			}
			if (offs >= 0x110000)
				return ((u8 *)((EMSPages[0]<<4)+offs));
			else
				return ((u8 *)((EMSPages[offs>>EMSPAGESHIFT]<<4)+offs));
		}
	}
	else		// Real mode
	{
		offs = (seg<<4)+(offs&0xFFFF);
		return ((u8 *)((EMSPages[offs>>EMSPAGESHIFT]<<4)+offs));
	}
}

void LogDebug(char *msg)
{
	// pa_core_run() encountered an unsupported opcode!
	int i, offs, len, pos;
	u8 *ptr = (u8 *)registers[8];
	if (BreakReason)
		pos = sprintf(msg, "%s", BreakReason);
	else
		pos = sprintf(msg, "Unsupported opcode!\n");
	pos += sprintf(msg + pos, "CPU: %s, %s, %08X\n", (cpu_cr0&1) ? "PROT" : "REAL", cpu_big ? "32" : "16", stack_mask);
	pos += sprintf(msg + pos, "EAX=%08X EBX=%08X ECX=%08X EDX=%08X\n", registers[0], registers[3], registers[1], registers[2] );
	pos += sprintf(msg + pos, "ESP=%08X EBP=%08X ESI=%08X EDI=%08X\n", registers[4], registers[5], registers[6], registers[7] );
	pos += sprintf(msg + pos, "ES=%04X CS=%04X SS=%04X DS=%04X FS=%04X GS=%04X\n", registers[10], registers[11], registers[12], registers[13], registers[14], registers[15] );
	offs = registers[8] - (int)(PHYS_PTR(registers[11], 0));
	pos += sprintf(msg + pos, "Code around the CS:EIP location %04X:%08X\n", registers[11], offs);
	for ( i=-0x10; i<0; i++ )
		pos += sprintf(msg + pos, "%02X", *(ptr+i));
	pos += sprintf(msg + pos, "\n");
	for ( i=0; i<0x10; i++ )
		pos += sprintf(msg + pos, "%02X", *(ptr+i));
	pos += sprintf(msg + pos, "\n");
}

#ifndef IOS
extern char GP_fpu_top;
extern short GP_fpu_sw;
extern unsigned int GP_fpu_regs[];

void log_fpu_c(unsigned char *csip)
{
	LOGI("%p %02X%02X TOP=%d, sw=%04X, 0=%08X%08X, 1=%08X%08X, 2=%08X%08X, 3=%08X%08X", csip, *csip, *(csip+1), GP_fpu_top, GP_fpu_sw,
		GP_fpu_regs[(GP_fpu_top&7)*2+1], GP_fpu_regs[(GP_fpu_top&7)*2],
		GP_fpu_regs[((GP_fpu_top+1)&7)*2+1], GP_fpu_regs[((GP_fpu_top+1)&7)*2],
		GP_fpu_regs[((GP_fpu_top+2)&7)*2+1], GP_fpu_regs[((GP_fpu_top+2)&7)*2],
		GP_fpu_regs[((GP_fpu_top+3)&7)*2+1], GP_fpu_regs[((GP_fpu_top+3)&7)*2]);
}
#endif
