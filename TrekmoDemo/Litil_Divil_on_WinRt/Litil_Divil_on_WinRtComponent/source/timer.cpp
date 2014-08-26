//=============================================================================
// timer.cpp
//
// This file handles the x86 timers and timer I/O ports, for Windows Phone 8.
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
//
// Compile with ASM source listing:
// cl -c .\source\timer.cpp /Fo".\obj\timer.obj" /GS /I..\jni\include /wd"4103" /analyze- /W1 /Zc:wchar_t /Zi /Od  /fp:precise /D "_LIB" /D "WINAPI_FAMILY=WINAPI_FAMILY_PHONE_APP"  /D "WP8" /errorReport:prompt /WX- /Zc:forScope /RTC1 /Gd /Oy-  /MDd /EHsc /nologo /ZW /Fa
//=============================================================================

//#include "pch.h"
#include <wrl/client.h>

#include "pax86.h"

#include "pacpu.h"

using namespace Windows::Foundation;
using namespace Windows::System::Threading;

struct timespec {
    long   	 tv_sec;        /* seconds */
    long     tv_nsec;       /* nanoseconds */
};

extern "C" {

	extern volatile int *IRQFlagAddr;
	extern int IRQPending[];

	extern void retappend(const char *format, ...);

	void lock_irqflag();
	void unlock_irqflag();
	void IRQRequest(int num);
	int in_al_40();
	int in_al_42();
	void out_40_al(int val);
	void out_42_al(int val);
	void out_43_al(int val);
	void StartAdlibTimers(int value);
	int CheckAdlibTimers();
	void InitTimers();
	void ExitTimers();
	void irqtrace(unsigned int r0, unsigned int r1, unsigned int r2);
	
	extern int VSyncCounter;
	extern u8 AdLibTimer1;
	extern u8 AdLibTimer2;
	extern u8 AdLibStatus;
	extern u8 AdLibMasked;
	
	int SpeakerIncr = 0;			// Speaker incrementer (for playing PC speaker audio)
	int DirectDACIncr = 0;			// Direct DAC incrementer (for playing SB Direct DAC audio)
	LARGE_INTEGER irqtime;			// Previous IRQ time
	LARGE_INTEGER irqnext;			// Next IRQ time ( = irqtime + irqperiod)
	int	irqperiod;					// Current IRQ period, in PerformanceCounter units.
	
	SRWLOCK irqflag_mutex = SRWLOCK_INIT;
	
	int irqcnt = 0;					// For debugging
	int skipcnt = 0;				// For debugging
	int prevsecs = 0;				// For debugging
	int sbirqcnt = 0;				// For debugging
}

#define printf  retappend

static LARGE_INTEGER T2inittime;		// Timer 2 initial time value


void lock_irqflag()
{
	AcquireSRWLockExclusive(&irqflag_mutex);
}
void unlock_irqflag()
{
	ReleaseSRWLockExclusive(&irqflag_mutex);
}

void IRQRequest(int num)
{
	// Return immediately if we are not running at the moment.
	if (IRQFlagAddr == 0)
		return;
	// Lock the mutex
	lock_irqflag();
	if (num || 0 == (IRQPending[num] & 0x10000))
	{
		// Mark the IRQ pending in IRQPending table (if not TimerIRQ already in process)
		IRQPending[num] |= ((num+8)*4);
		// Test for FLAG_IF being set
		if (((*(IRQFlagAddr+3) & FLAG_IF) && 0 == (IRQPending[num] & 0x10000)) || IRQ_EXIT == num)
		{
			// Set *IRQFlagAddr (SP_IRQFLAG) to IRQ_ON
			*IRQFlagAddr = 0x100000; // IRQ_ON;
		}
	}
	// Unlock irqmutex and exit
	unlock_irqflag();
}

static LARGE_INTEGER freq;
static int Port40Data = 0;		// First PC timer, used for Timer IRQ
//static int Port42Data = 0;		// Third PC timer, used for PC speaker sounds
static int T0Value = 0x10000;	// Timer 0 counter x86 start value, 0 == 0x10000
static int T2Value = 0x10000;	// Timer 2 counter x86 start value, 0 == 0x10000
static u16 T2ReadLatch = 0;		// Timer 2 counter_latch value
static u16 T2WriteLatch = 0;	// Timer 2 latch written value (port 42 values)
static u8 T2Mode = 0;			// Current Timer 2 mode (port 43 value)
static u8 T2NeedRead = 0;

static int Getx86TicksSince(LARGE_INTEGER *starttime)
{
	// The x86 has decrementing timers, so for example, if the PC timer start_value =
	// 0x0200, the timer counts at 1.193.182 Hz downwards 0x1FF, 0x1FE, 0x1FD, ..., 0,
	// and then back to 0x1FF. We need to determine how much time has passed since the
	// last IRQ (counter reset), and decrement that much from the timer start value.
	// Each x86 timer tick lasts 838 nanoseconds.
	LARGE_INTEGER nowtime;
	long long cdiff;
	//------------------------------------
	//	add         r0,sp,#8
	//	ldr         r3,|$LN13@Getx86Tick|		; =|__imp_QueryPerformanceCounter|
	//	ldr         r3,[r3]
	//	blx         r3
	//------------------------------------
	QueryPerformanceCounter(&nowtime);	// Get the current time as a preformance counter value.
	//------------------------------------
	//	ldr         r0,[sp,#8]					; r0 = nowtime.LowPart
	//	ldr         r1,[sp,#0xC]				; r1 = nowtime.HighPart
	//	ldr         r3,|$LN12@Getx86Tick|		; =|irqtime|
	//	ldr         r2,[r3]						; r2 = irqitme.LowPart
	//	ldr         r3,|$LN12@Getx86Tick|		; =|irqtime|
	//	ldr         r3,[r3,#4]					; r3 = irqtime.HighPart
	//	subs        r2,r0,r2					; r2 = nowtime.LowPart - irqtime.LowPart
	//	sbcs        r3,r1,r3					; r3 = nowtime.HighPart - irqtime.HighPart - Carry
	//	str         r2,[sp,#0x18]				; store cdiff.LowPart
	//	str         r3,[sp,#0x1C]				; store cdiff.HighPart
	//------------------------------------
	cdiff = nowtime.QuadPart - irqtime.QuadPart;
	//------------------------------------
	//	ldr         r3,[sp,#0x18]				; r3 = cdiff.LowPart
	//	ldr         r0,[sp,#0x1C]				; r0 = cdiff.HighPart
	//	str         r3,[sp,#0x24]				; store cdiff.LowPart
	//	ldr         r1,[sp,#0x24]				; r1 = cdiff.LowPart
	//	movs        r3,#0						; r3 = 0
	//	mul         r2,r1,r3					; r2 = 0 * cdiff.LowPart = 0
	//	ldr         r3,|$LN11@Getx86Tick|		; =0x1234de
	//	mul         r3,r0,r3					; r3 = 1193182 * cdiff.HighPart
	//	adds        r0,r2,r3					; r0 = 0 + 1193182 * cdiff.HighPart
	//	ldr         r1,[sp,#0x24]
	//	ldr         r3,|$LN11@Getx86Tick|		; =0x1234de
	//	umull       r2,r3,r1,r3					; r3:r2 = 1193182 * cdiff.LowPart
	//	adds        r4,r0,r3					; r4:r2 = 1193182 * cdiff
	//	ldr         r3,|$LN10@Getx86Tick|		; =|freq|
	//	ldr         r0,[r3]						; r0 = freq
	//	movs        r1,#0						; r1:r0 = freq
	//	mov         r3,r4						; r3:r2 = 1193182 * cdiff
	//	cmp         r0,#0						; Check for division by zero
	//	bne         |$LN3@Getx86Tick|
	//	cmp         r1,#0
	//	bne         |$LN3@Getx86Tick|
	//	__brkdiv0
	//|$LN3@Getx86Tick|
	//	bl          __rt_sdiv64
	//	str         r3,[sp,#0x2C]
	//	str         r2,[sp,#0x28]
	//	mov         r3,r0
	//	str         r3,[sp,#0x20]
	//------------------------------------
	return (int)(cdiff * 1193182L / freq.LowPart);
}

static int TicksToPerfCounter(int ticks)
{
	// Convert the x86 ticks value (0x10000 = 1193182/65536 = 18.2 Hz = 55 ms)
	// to performance counter interval.
	return (int)(ticks * freq.QuadPart / 1193182);
}

int GetnsSince(LARGE_INTEGER *starttime)
{
	// Calculate the number of nanoseconds since the start time. Called from pax86retro.cpp
	return 838*Getx86TicksSince(starttime);
}

int in_al_40()
{ 
	int tmp = Port40Data;
	if (tmp & 0x10000)
	{
		// Return high byte of Port40Data value
		Port40Data &= 0xFF00;
		return tmp >> 8;
	}
	else
	{
		LARGE_INTEGER nowtime;
		long long countdiff;
		// Read the current timer value and save it to Port40Data, set flag for next read.
		tmp = Getx86TicksSince(&irqtime);
		if (tmp >= T0Value)
		{
			// Timer IRQ should already have happened!
			Port40Data = 0x00010001;
		}
		else
			Port40Data = 0x10000 | (T0Value - tmp);
		return Port40Data;
	}
}

int in_al_42()
{ 
	//-------
	// IN AL,42 = Timer Latch input
	// Do we need to read the hardware timer value?
	//-------
	if (T2NeedRead)
	{
		//-------
		// Counter Latch for timer 2 (x86 has decrementing and ARM incrementing timers)
		//-------
		int tmp = Getx86TicksSince(&T2inittime);
		if ((T2Mode&0x0E) == 0 || (T2Mode&0x0E) == (4<<1))
		{
			// Mode 0 or 4, counter keeps on counting after passing terminal count
			T2ReadLatch = (0x10000 - tmp) & 0xFFFF;
		}
		else
		{
			// Other modes, read_latch = counter - current ticks
			T2ReadLatch = T2Value - (tmp%T2Value);
		}
		//LOGI("T2ReadLatch=%04X, T2Value=%04X\n", T2ReadLatch, T2Value);
		T2NeedRead = 0;					// Tell no need to read the counter again
	}
	//-------
	// What is the current timer RW Mode?
	//-------
	switch( T2Mode&0x30 )
	{
		case 0x00:
			//-------
			// Timer 2 read_mode = 0: Go to mode 3, read MSB, return
			//-------
			T2NeedRead = 1;					// Tell we need to re-read counter latch
			T2Mode |= 0x30;					// Go to mode 3 (read LSB) next
			//-------
			// Read MSB, return
			//-------
			return (T2ReadLatch>>8)&0xFF;
		case 0x10:
			//-------
			// read_mode = 1: Tell we need to re-read counter_latch, read LSB, return
			//-------
			T2NeedRead = 1;					// Tell we need to re-read counter latch
			return T2ReadLatch&0xFF;
		case 0x20:
			//-------
			// Timer 2 read_mode = 2: Tell next call needs to re-read counter_latch, Read MSB, return
			//-------
			T2NeedRead = 1;					// Tell we need to re-read counter latch
			return (T2ReadLatch>>8)&0xFF;
		case 0x30:
			//-------
			// read_mode = 3: Go to mode 0, read LSB, return
			//-------
			T2Mode &= 0xCF;					// Go to mode 0 (read MSB) next
			return T2ReadLatch&0xFF;
	}
	return 0;
}

void out_40_al(int val)
{
	int tmp = Port40Data;
	//LOGI("out 40,%02X\n", val&0xFF);
	if (tmp & 0x10000)
	{
		Port40Data = (Port40Data & 0xFF) | ((val & 0xFF) << 8);
		if (T0Value != ((0 == Port40Data) ? 0x10000 : Port40Data))
		{
			T0Value = (0 == Port40Data) ? 0x10000 : Port40Data;
			DirectDACIncr = 2443632 / T0Value;			// For possible SB Direct DAC
			irqperiod = TicksToPerfCounter(T0Value);	// Calculate the new IRQ period.
			QueryPerformanceCounter(&irqtime);			// Get the current time as the timer initial value.
			irqnext.QuadPart = irqtime.QuadPart + irqperiod;	// Setup the next IRQ time.
		}
	}
	else
	{
		Port40Data = (Port40Data & 0xFF00) | 0x10000 | (val & 0xFF);
	}
}

void out_42_al(int val)
{
	//-------
	// 0x42 = Timer 2 Data Port
	// Determine how we are to handle the port 42 values (what is current Read/Write mode)
	//-------
	//LOGI("out 42,%02X\n", val&0xFF);
	switch( T2Mode&0x30 )
	{
		case 0x00:
			//-------
			// write_mode = 0: Write MSB, go to mode 3, set the timer, send to ARM7, return
			//-------
			T2WriteLatch = (T2WriteLatch&0xFF) | ((val&0xFF)<<8);	// Write MSB of timer latch value to T2WriteLatch+1
			T2Mode |= 0x30;					// Go to mode 3 (write LSB) next
			//-------
			// Set the hardware timer. In timer modes 0 and 4 the timer counts the full 0x10000 steps,
			// in other modes the timer counts write_latch steps.
			//-------
out_42_start_timer:
			if ((T2Mode&0x0E) == 0 || (T2Mode&0x0E) == (4<<1))
				T2Value = 0x10000;
			else
			{
				T2Value = T2WriteLatch;
				if (0 == T2Value)
					T2Value = 0x10000;
			}
			QueryPerformanceCounter(&T2inittime);	// Get the current time as a preformance counter value.
			//-------
			// Update the speaker frequency shared memory address for ARM7 speaker handling.
			// frequency = SOUND_FREQ((1193180/T2WriteLatch)<<3);
			// For 32000Hz frequency, we want incrementer 0x00010000 (65536)
			// For  1000Hz frequency, we want incrementer 0x00000800 (2048)
			// For  18.2Hz frequency, we want incrementer 0x00000025 (37)
			// => incrementer = 65536 * (1193180/T2WriteLatch) / 32000
			// => incrementer = 2443632 / T2WriteLatch
			//-------
			if (T2WriteLatch > 36)	// PTrooper uses latch value 2 for silence!
				SpeakerIncr = 2443632 / T2WriteLatch;
			else
				SpeakerIncr = 0;
			//LOGI("T2WriteLatch=%04X, T2Value=%04X, SpeakerIncr=%d\n", T2WriteLatch, T2Value, SpeakerIncr);
			return;
		case 0x10:
			//-------
			// write_mode = 1: Write LSB, set the timer, send to ARM7, return (this is rare)
			//-------
			T2WriteLatch = val&0xFF;		// Write LSB of timer latch value to T2WriteLatch
			goto	out_42_start_timer;
		case 0x20:
			//-------
			// write_mode = 2: Write MSB, set the timer, send to ARM7, return (this is rare)
			//-------
			T2WriteLatch = (val&0xFF)<<8;	// Write MSB of timer latch value to T2WriteLatch
			goto	out_42_start_timer;
		case 0x30:
			//-------
			// write_mode = 3: Write LSB, go to mode 0, return
			//-------
			T2WriteLatch = (T2WriteLatch&0xFF00) | (val&0xFF);	// Write LSB of timer latch value to T2WriteLatch
			T2Mode &= 0xCF;	// Go to mode 0 (write MSB) next
			return;
	}
}

void out_43_al(int val)
{
	//=======
	// Timer Control Word Register
	//=======
	// Bit#   D7  D6     D5  D4     D3 D2 D1      D0
	// Name  SC1 SC0    RW1 RW0     M2 M1 M0      BCD
	// ----  --------   ----------  -----------   ------------------------
	// Func. Select     Read/Write  Select        =0, 16-b binary counter
	//       Counter                Mode          =1, 4-decade BCD counter
	//
	// RW1   RW0   Description
	// ---   ---   ------------------------------------------------
	//  0     0    Counter Latch Command
	//  0     1    Read/Write the least significant byte (LSB) only
	//  1     0    Read/Write the most significant byte (MSB) only
	//  1     1    Read/Write LSB first, followed by MSB 
	//-------
	//LOGI("out 43,%02X\n", val);
	if ((val&0xFF) >= 0xC0)
	{
		//-------
		// Latch multiple timers
		//-------
		return;
	}
	if (0 == (val &0x80))
	{
		//-------
		// Counter 0 (IRQ0) programming (or possibly, but very unlikely, counter 1 = RAM refresh counter)
		//-------
		return;
	}
	//-------
	// Counter 2 programming
	//-------
	if (val & 0x30)
	{
		//-------
		// Program timer 2
		//-------
		T2Mode = val & 0xFF;
		return;
	}
	//-------
	// Counter Latch for timer 2 (x86 has decrementing and ARM incrementing timers)
	//-------
	int tmp = Getx86TicksSince(&T2inittime);
	if ((T2Mode&0x0E) == 0 || (T2Mode&0x0E) == (4<<1))
	{
		// Mode 0 or 4, counter keeps on counting after passing terminal count
		T2ReadLatch = (0x10000 - tmp) & 0xFFFF;
	}
	else
	{
		// Other modes, read_latch = counter - current ticks
		T2ReadLatch = T2Value - (tmp%T2Value);
	}
	T2NeedRead = 0;					// Tell no need to read the counter again
}

static LARGE_INTEGER adlib1;				// Adlib timer 1
static LARGE_INTEGER adlib2;				// Adlib timer 2

// Called from "ports_sb.S"
void StartAdlibTimers(int value)
{
	if (0 == (value & 0x40))		// Timer 1 bit unmasked, start/stop timer 1
	{
		AdLibMasked &= 0xBF;		// Unmask timer 1
		if (value & 1)
		{
			AdLibStatus |= 1;		// Mark that timer 1 is now running
			QueryPerformanceCounter(&adlib1); // Remember start time
		}
		else
			AdLibStatus &= 0xFE;	// Stop timer 1 from running
	}
	else
	{
		AdLibMasked |= 0x40;		// Mask timer 1
		AdLibStatus &= 0xBF;		// Clear timer 1 overflow flag
		if ((AdLibStatus&0xE0) == 0x80)
			AdLibStatus &= 0x0F;
	}
	if (0 == (value & 0x20))		// Timer 1 bit unmasked, start/stop timer 1
	{
		AdLibMasked &= 0xDF;
		if (value & 2)
		{
			AdLibStatus |= 2;		// Mark that timer 2 is now running
			QueryPerformanceCounter(&adlib2); // Remember start time
		}
		else
			AdLibStatus &= 0xFD;	// Stop timer 2 from running
	}
	else
	{
		AdLibMasked |= 0x20;		// Mask timer 2
		AdLibStatus &= 0xDF;		// Clear timer 2 overflow flag
		if ((AdLibStatus&0xE0) == 0x80)
			AdLibStatus &= 0x0F;
	}
}

// Called from "ports_sb.S"
int CheckAdlibTimers()
{
	LARGE_INTEGER curtime;
	int udiff;
	if ((AdLibMasked & 0x40) == 0 && (AdLibStatus & 0x41) == 1)
	{
		// AdLib timer 1 running, not masked, not yet expired
		QueryPerformanceCounter(&curtime);	// Get current time
		udiff = (int)((curtime.QuadPart - adlib1.QuadPart)*1000000L/freq.LowPart);
		if (udiff >= (256 - AdLibTimer1)*80)	// Timer runs at 80 microseconds
			AdLibStatus |= (0x80 | 0x40);		// Mark that timer1 has expired
	}
	if ((AdLibMasked & 0x20) == 0 && (AdLibStatus & 0x22) == 2)
	{
		// AdLib timer 2 running, not masked, not yet expired
		QueryPerformanceCounter(&curtime);	// Get current time
		udiff = (int)((curtime.QuadPart - adlib2.QuadPart)*1000000L/freq.LowPart);
		if (udiff >= (256 - AdLibTimer2)*320)	// Timer runs at 320 microseconds
			AdLibStatus |= (0x80 | 0x20);		// Mark that timer2 has expired
	}
	return AdLibStatus;
}

void InitTimers()
{
	//--------------------------------------
	// Determine the performance counter frequency
	// (On Nokia 520: 67500000 = 67.5 MHz)
	//-------------------------------------
	QueryPerformanceFrequency(&freq);
	//LOGI("freq=%d\n", freq.LowPart);
	irqperiod = TicksToPerfCounter(T0Value);
	QueryPerformanceCounter(&irqtime);		// Get the current time as the timer initial value.
	irqnext.QuadPart = irqtime.QuadPart + irqperiod;	// Setup the next IRQ time.
}

void ExitTimers()
{
}

typedef struct IRQDATA {
	unsigned int		counter;
	unsigned int		pcaddr;
	unsigned int		event;
} IRQDATA;

static int noirq0 = 0;
static int irqidx = 0;
static IRQDATA irqdata[128];

void irqtrace(unsigned int r0, unsigned int r1, unsigned int r2)
{
	LARGE_INTEGER curtime;
	if (r1 != 0x80000000 + (8*4))
	{
		if (++noirq0 > 16)
			return;
	}
	else
		noirq0 = 0;
	if (r2 == 0)
	{
		QueryPerformanceCounter(&curtime);	// Get current time
		r2 = curtime.LowPart;
	}
	lock_irqflag();
	irqdata[irqidx].pcaddr = r0;
	irqdata[irqidx].event = r1;
	irqdata[irqidx].counter = r2;
	irqidx = (irqidx + 1) & 127;
	unlock_irqflag();
}

void DebugTimers()
{
/*
	printf("irqperiod=%d, seconds=%d, irqcnt=%d (%d Hz), skipcnt=%d, sbcnt=%d\n", 
		irqperiod, (int)(irqtime.QuadPart/freq.LowPart), irqcnt, 
		(int)(irqtime.QuadPart/freq.LowPart) - prevsecs > 0 ? irqcnt/((int)(irqtime.QuadPart/freq.LowPart) - prevsecs) : 0,
		skipcnt, sbirqcnt);
*/
	for (int i=0; i < 8; i++)
		printf("%X ", IRQPending[i]);
	printf("\n");
	for (int i=12; i < 64; i++)
	{
		printf("%X %X %02X%02X %X\n", irqdata[(irqidx-i)&127].counter, irqdata[(irqidx-i)&127].pcaddr, 
			*((unsigned char *)irqdata[(irqidx-i)&127].pcaddr-1), *((unsigned char *)irqdata[(irqidx-i)&127].pcaddr),
			irqdata[(irqidx-i)&127].event);
	}
	irqcnt = 0;
	sbirqcnt = 0;
	prevsecs = (int)(irqtime.QuadPart/freq.LowPart);
}

#ifdef RII_EX

// Return the worst-case size of the timer serialization data.
int timer_serialize_size()
{
	return 3*sizeof(struct timespec) + sizeof(Port40Data) + 
		sizeof(T0Value) + sizeof(T2Value) + sizeof(T2ReadLatch) + 
		sizeof(T2WriteLatch) + sizeof(T2Mode) + sizeof(T2NeedRead) + 
		sizeof(SpeakerIncr) + sizeof(DirectDACIncr);
}

static void to_timespec(struct timespec *out, LARGE_INTEGER *now, LARGE_INTEGER *init)
{
	double tmp = (double)(now->QuadPart - init->QuadPart)/(double)freq.LowPart;
	out->tv_sec = (int)tmp;
	out->tv_nsec = (tmp - (double)out->tv_sec)*(double)1000000000;
}

// Serialize the timer data into data + offset.
int timer_serialize(u8 *data, int offset)
{
	struct timespec tmptime;
	LARGE_INTEGER nowtime;
	
	QueryPerformanceCounter(&nowtime);	// Get current time

	// First the timespec structures, save them as differences from the current time.
	to_timespec(&tmptime, &nowtime, &T2inittime);
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	to_timespec(&tmptime, &nowtime, &adlib1);
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	to_timespec(&tmptime, &nowtime, &adlib2);
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);

	// Then the other variables
	memcpy(data + offset, &Port40Data, sizeof(Port40Data));
	offset += sizeof(Port40Data);
	memcpy(data + offset, &T0Value, sizeof(T0Value));
	offset += sizeof(T0Value);
	memcpy(data + offset, &T2Value, sizeof(T2Value));
	offset += sizeof(T2Value);
	memcpy(data + offset, &T2ReadLatch, sizeof(T2ReadLatch));
	offset += sizeof(T2ReadLatch);
	memcpy(data + offset, &T2WriteLatch, sizeof(T2WriteLatch));
	offset += sizeof(T2WriteLatch);
	memcpy(data + offset, &T2Mode, sizeof(T2Mode));
	offset += sizeof(T2Mode);
	memcpy(data + offset, &T2NeedRead, sizeof(T2NeedRead));
	offset += sizeof(T2NeedRead);
	memcpy(data + offset, &SpeakerIncr, sizeof(SpeakerIncr));
	offset += sizeof(SpeakerIncr);
	memcpy(data + offset, &DirectDACIncr, sizeof(DirectDACIncr));
	offset += sizeof(DirectDACIncr);
	return offset;
}

static void from_timespec(LARGE_INTEGER *out, struct timespec *in, LARGE_INTEGER *now)
{
	double tmp = ((double)in->tv_sec + ((double)in->tv_nsec / (double)1000000000)) * (double)freq.LowPart;
	out->QuadPart = now->QuadPart - (long long)tmp;
}

// Unserialize the timer data from data + offset.
int timer_unserialize(u8 *data, int offset)
{
	struct timespec tmptime;
	LARGE_INTEGER nowtime;
	
	QueryPerformanceCounter(&nowtime);	// Get current time

	// First the timespec structures, save them as differences from the current time.
	memcpy(&tmptime, data + offset, sizeof(tmptime));
	offset += sizeof(tmptime);
	from_timespec(&T2inittime, &tmptime, &nowtime);
	memcpy(&tmptime, data + offset, sizeof(tmptime));
	offset += sizeof(tmptime);
	from_timespec(&adlib1, &tmptime, &nowtime);
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	from_timespec(&adlib2, &tmptime, &nowtime);

	// Then the other variables
	memcpy(&Port40Data, data + offset, sizeof(Port40Data));
	offset += sizeof(Port40Data);
	memcpy(&T0Value, data + offset, sizeof(T0Value));
	offset += sizeof(T0Value);
	memcpy(&T2Value, data + offset, sizeof(T2Value));
	offset += sizeof(T2Value);
	memcpy(&T2ReadLatch, data + offset, sizeof(T2ReadLatch));
	offset += sizeof(T2ReadLatch);
	memcpy(&T2WriteLatch, data + offset, sizeof(T2WriteLatch));
	offset += sizeof(T2WriteLatch);
	memcpy(&T2Mode, data + offset, sizeof(T2Mode));
	offset += sizeof(T2Mode);
	memcpy(&T2NeedRead, data + offset, sizeof(T2NeedRead));
	offset += sizeof(T2NeedRead);
	memcpy(&SpeakerIncr, data + offset, sizeof(SpeakerIncr));
	offset += sizeof(SpeakerIncr);
	memcpy(&DirectDACIncr, data + offset, sizeof(DirectDACIncr));
	offset += sizeof(DirectDACIncr);
	return offset;
}

#endif
