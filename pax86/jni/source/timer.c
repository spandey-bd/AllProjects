//=============================================================================
// timer.c
//
// This file handles the x86 timers and timer I/O ports.
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
#include <pthread.h>

#include "pax86.h"
#include "pacpu.h"

#define	RELATIVE	0

#if defined(RPi) || defined(Roku)

#include <signal.h>
#include <sys/time.h>
static pthread_t maintid = 0;
static sigset_t set;
extern void exit_program();

#else

pthread_mutex_t irqflag_mutex;			// Mutex locking access to SP_IRQFLAG
static pthread_mutex_t irqtime_mutex;	// Mutex locking access to irqtime
static pthread_t tid;					// Currently active timer thread ID
static struct timespec irqtime;			// Previous IRQ rollover time
static struct timespec irqint;			// Timer IRQ interval
void lock_irqflag()
{
	pthread_mutex_lock(&irqflag_mutex);
}
void unlock_irqflag()
{
	pthread_mutex_unlock(&irqflag_mutex);
}

#endif

static struct timespec T2inittime;		// Timer 2 initial time value


void IRQRequest(int num)
{
	// Return immediately if we are not running at the moment.
	if (IRQFlagAddr == 0)
		return;
	// Lock the mutex
#if !defined(RPi) && !defined(Roku)
	lock_irqflag();
#endif
	if (num || 0 == (IRQPending[num] & 0x10000))
	{
		// Mark the IRQ pending in IRQPending table (if not TimerIRQ already in process)
		IRQPending[num] |= ((num+8)*4);
		// Test for FLAG_IF being set
		if (((*(IRQFlagAddr+3) & FLAG_IF) && 0 == (IRQPending[num] & 0x10000)) || IRQ_EXIT == num)
		{
			// Set *IRQFlagAddr (SP_IRQFLAG) to IRQ_ON
			*IRQFlagAddr = IRQ_ON;
		}
	}
	// Unlock irqmutex and exit
#if !defined(RPi) && !defined(Roku)
	unlock_irqflag();
#endif
}

#if defined(RPi) || defined(Roku)

static void TimerIRQ()		{ IRQRequest(IRQ_TIMER); }
static void KeyboardIRQ()	{ IRQRequest(IRQ_KEYB); }
static void PS2IRQ()		{ IRQRequest(IRQ_PS2); }
static void COM2IRQ()		{ IRQRequest(IRQ_MOUSE); }
static void COM1IRQ()		{ IRQRequest(IRQ_COM1); }
static void SBIRQ()			{ IRQRequest(IRQ_SB); }
static void ExitIRQ()		{ IRQRequest(IRQ_EXIT); }

void SendKeyboardIRQ()		{ pthread_kill(maintid, SIGRTMIN+IRQ_KEYB); }
void SendSBIRQ()			{ pthread_kill(maintid, SIGRTMIN+IRQ_SB); }
void SendCOM1IRQ()			{ pthread_kill(maintid, SIGRTMIN+IRQ_COM1); }
void SendCOM2IRQ()			{ pthread_kill(maintid, SIGRTMIN+IRQ_MOUSE); }
void SendPS2IRQ()			{ pthread_kill(maintid, SIGRTMIN+IRQ_PS2); }
void SendExitIRQ()			{ pthread_kill(maintid, SIGRTMIN+IRQ_EXIT); }
//void SendSIGTERM()			{ pthread_kill(maintid, SIGTERM); }

#endif

#define COUNT_CYCLES	0
extern int VSyncCounter;

#ifdef IOS

#include <mach/mach.h>
#include <mach/mach_time.h>

#define	CLOCK_MONOTONIC		0
#define	TIMER_ABSTIME		1

int clock_gettime(int clk_id, struct timespec *tp)
{
	uint64_t        mtime;
    static mach_timebase_info_data_t    sTimebaseInfo;

    mtime = mach_absolute_time();

    if ( sTimebaseInfo.denom == 0 ) {
        (void) mach_timebase_info(&sTimebaseInfo);
    }

	mtime = mtime * sTimebaseInfo.numer / sTimebaseInfo.denom;

	tp->tv_sec = mtime / 1000000000L;
	tp->tv_nsec = mtime % 1000000000L;

	return 0;
}

int clock_nanosleep(int clock_id, int flags, const struct timespec *request, struct timespec *remain)
{
	if (flags & TIMER_ABSTIME)
	{
		struct timespec tmp;
		clock_gettime(clock_id, &tmp);
		tmp.tv_nsec = request->tv_nsec - tmp.tv_nsec;
		tmp.tv_sec = request->tv_sec - tmp.tv_sec;
		while (tmp.tv_nsec < 0)
		{
			tmp.tv_sec -= 1;
			tmp.tv_nsec += 1000000000;
		}
		if (tmp.tv_sec < 0)
			return 0;
		return nanosleep(&tmp, remain);
	}
	else
		return nanosleep(request, remain);
}

#endif

#if !defined(RPi) && !defined(Roku)

void* timer_thread(void *arg)
{
	pthread_t mytid = pthread_self();
	pthread_mutex_lock(&irqtime_mutex);
	clock_gettime(CLOCK_MONOTONIC, &irqtime);	// Get current time as the PC timer rollover time
	pthread_mutex_unlock(&irqtime_mutex);
	while(1)
	{
		clock_nanosleep(CLOCK_MONOTONIC, RELATIVE, &irqint, NULL);
		if (mytid != tid) // Oops, we were made obsolete by a different thread, so quit.
		{
			//LOGI("Timer thread %d quitting for %d!\n", mytid, tid);
			break;
		}
		pthread_mutex_lock(&irqtime_mutex);
		clock_gettime(CLOCK_MONOTONIC, &irqtime);	// Get current time as the PC timer rollover time
		pthread_mutex_unlock(&irqtime_mutex);
		IRQRequest(IRQ_TIMER);
	}
	return NULL;
}
#endif

static int Port40Data = 0;		// First PC timer, used for Timer IRQ
//static int Port42Data = 0;		// Third PC timer, used for PC speaker sounds
static int T0Value = 0x10000;	// Timer 0 counter x86 start value, 0 == 0x10000
static int T2Value = 0x10000;	// Timer 2 counter x86 start value, 0 == 0x10000
static u16 T2ReadLatch = 0;		// Timer 2 counter_latch value
static u16 T2WriteLatch = 0;	// Timer 2 latch written value (port 42 values)
static u8 T2Mode = 0;			// Current Timer 2 mode (port 43 value)
static u8 T2NeedRead = 0;

int SpeakerIncr = 0;			// Speaker incrementer (for playing PC speaker audio)
int DirectDACIncr = 0;			// Direct DAC incrementer (for playing SB Direct DAC audio)

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
		// Read the current timer value and save it to Port40Data, set flag for next read.
		// The x86 has decrementing timers, so for example, if the PC timer start_value =
		// 0x0200, the timer counts at 1.193.182 Hz downwards 0x1FF, 0x1FE, 0x1FD, ..., 0,
		// and then back to 0x1FF. We need to determine how much time has passed since the
		// last IRQ (counter reset), and decrement that much from the timer start value.
		// Each x86 timer tick lasts 838 nanoseconds.
#if defined(RPi) || defined(Roku)
		struct itimerval it;
		getitimer(ITIMER_REAL, &it);
		Port40Data = 0x10000 | (it.it_value.tv_usec * 1000 / 838);
#else
		int nsecdiff;
		struct timespec nowtime;
		pthread_mutex_lock(&irqtime_mutex);
		clock_gettime(CLOCK_MONOTONIC, &nowtime);	// Get current time
		nsecdiff = nowtime.tv_nsec - irqtime.tv_nsec;
		pthread_mutex_unlock(&irqtime_mutex);
		if (nsecdiff < 0)
			nsecdiff += 1000000000;
		tmp = (int)(nsecdiff / 838);
		if (tmp >= T0Value)
			Port40Data = 0x00010001;
		else
			Port40Data = 0x10000 | (T0Value - tmp);
#endif
		return Port40Data;
	}
}

int in_al_42()
{ 
	struct timespec T2curtime;
	int nsecdiff;
	//-------
	// IN AL,42 = Timer Latch input
	// Do we need to read the hardware timer value?
	//-------
	if (T2NeedRead)
	{
		//-------
		// Counter Latch for timer 2 (x86 has decrementing and ARM incrementing timers)
		//-------
		clock_gettime(CLOCK_MONOTONIC, &T2curtime);	// Get current time
		nsecdiff = T2curtime.tv_nsec - T2inittime.tv_nsec;
		nsecdiff += (int)(T2curtime.tv_sec - T2inittime.tv_sec) * 1000000000;
		if ((T2Mode&0x0E) == 0 || (T2Mode&0x0E) == (4<<1))
		{
			// Mode 0 or 4, counter keeps on counting after passing terminal count
			T2ReadLatch = (0x10000 - (nsecdiff/838)) & 0xFFFF;
		}
		else
		{
			// Other modes, read_latch = counter - current ticks
			T2ReadLatch = T2Value - ((nsecdiff/838)%T2Value);
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
#if defined(RPi) || defined(Roku)
	struct itimerval it, old;
#endif
	int tmp = Port40Data;
	//LOGI("out 40,%02X\n", val&0xFF);
	if (tmp & 0x10000)
	{
		Port40Data = (Port40Data & 0xFF) | ((val & 0xFF) << 8);
		if (T0Value != ((0 == Port40Data) ? 0x10000 : Port40Data))
		{
			//LOGI("T0Value=%08X, Port40Data=%08X!\n", T0Value, Port40Data);
			T0Value = (0 == Port40Data) ? 0x10000 : Port40Data;
			if (T0Value < 3000)	// If the speed is at least 400Hz
				DirectDACIncr = 2443632 / T0Value;	// For possible SB Direct DAC
#if defined(RPi) || defined(Roku)
			// Setup the timer interval
			it.it_interval.tv_usec = (838*T0Value)/1000;
			it.it_value.tv_usec = it.it_interval.tv_usec;
			it.it_interval.tv_sec = 0;
			it.it_value.tv_sec = 0;
			setitimer(ITIMER_REAL, &it, &old);
			//LOGI("Adjusted itimer to %d usec interval = %dHz!\n", (int)it.it_interval.tv_usec, 1191563/T0Value );
#else
			irqint.tv_nsec = 838*T0Value;	// Setup the timer interval
			// Launch a new timer thread, the old one will die soon.
			pthread_create(&tid, NULL, timer_thread, NULL);
			//LOGI("New timer thread started for %d ns interval = %dHz!\n", (int)irqint.tv_nsec, 1191563/T0Value );
#endif
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
			clock_gettime(CLOCK_MONOTONIC, &T2inittime);	// Get current time
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
	struct timespec T2curtime;
	int nsecdiff;
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
	clock_gettime(CLOCK_MONOTONIC, &T2curtime);	// Get current time
	nsecdiff = T2curtime.tv_nsec - T2inittime.tv_nsec;
	nsecdiff += (int)(T2curtime.tv_sec - T2inittime.tv_sec) * 1000000000;
	if ((T2Mode&0x0E) == 0 || (T2Mode&0x0E) == (4<<1))
	{
		// Mode 0 or 4, counter keeps on counting after passing terminal count
		T2ReadLatch = (0x10000 - (nsecdiff/838)) & 0xFFFF;
	}
	else
	{
		// Other modes, read_latch = counter - current ticks
		T2ReadLatch = T2Value - ((nsecdiff/838)%T2Value);
	}
	T2NeedRead = 0;					// Tell no need to read the counter again
}

static struct timespec adlib1;				// Adlib timer 1
static struct timespec adlib2;				// Adlib timer 2
extern u8 AdLibTimer1;
extern u8 AdLibTimer2;
extern u8 AdLibStatus;
extern u8 AdLibMasked;

// Called from "ports_sb.S"
void StartAdlibTimers(int value)
{
	if (0 == (value & 0x40))		// Timer 1 bit unmasked, start/stop timer 1
	{
		AdLibMasked &= 0xBF;		// Unmask timer 1
		if (value & 1)
		{
			AdLibStatus |= 1;		// Mark that timer 1 is now running
			clock_gettime(CLOCK_MONOTONIC, &adlib1); // Remember start time
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
			clock_gettime(CLOCK_MONOTONIC, &adlib2); // Remember start time
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

extern char CurrentEXE[];

// Called from "ports_sb.S"
int CheckAdlibTimers()
{
	struct timespec curtime;
	int udiff;
	if ((AdLibMasked & 0x40) == 0 && (AdLibStatus & 0x41) == 1)
	{
		// AdLib timer 1 running, not masked, not yet expired
		clock_gettime(CLOCK_MONOTONIC, &curtime);	// Get current time
		udiff = (curtime.tv_nsec - adlib1.tv_nsec)/1000;
		udiff += (curtime.tv_sec - adlib1.tv_sec)*1000000;
		if (udiff >= (256 - AdLibTimer1)*80)	// Timer runs at 80 microseconds
			AdLibStatus |= (0x80 | 0x40);		// Mark that timer1 has expired
		if (0xFF == AdLibTimer1 && 0 == memcmp(CurrentEXE, "VGALEMMI", 8))
			AdLibStatus |= (0x80 | 0x40);		// Mark that timer1 has expired (Lemmings hack!)
	}
	if ((AdLibMasked & 0x20) == 0 && (AdLibStatus & 0x22) == 2)
	{
		// AdLib timer 2 running, not masked, not yet expired
		clock_gettime(CLOCK_MONOTONIC, &curtime);	// Get current time
		udiff = (curtime.tv_nsec - adlib2.tv_nsec)/1000;
		udiff += (curtime.tv_sec - adlib2.tv_sec)*1000000;
		if (udiff >= (256 - AdLibTimer2)*320)	// Timer runs at 320 microseconds
			AdLibStatus |= (0x80 | 0x20);		// Mark that timer2 has expired
	}
	return AdLibStatus;
}

void InitTimers()
{
#if defined(RPi) || defined(Roku)
	struct timespec res;
	struct itimerval it, old;
	struct sigaction sa;

	// Setup the set of signals we want to block/unblock
	sigemptyset(&set);
	sigaddset(&set, SIGALRM);
	sigaddset(&set, SIGRTMIN+IRQ_KEYB);
	sigaddset(&set, SIGRTMIN+IRQ_PS2);
	sigaddset(&set, SIGRTMIN+IRQ_MOUSE);
	sigaddset(&set, SIGRTMIN+IRQ_SB);

	// Determine the resolution of the clocks.
	if (0 == clock_getres( CLOCK_MONOTONIC, &res ) && (int)res.tv_nsec != 1)
	{
		LOGI("clock_getres(CLOCK_MONOTONIC, &res) = %d nsec!\n", (int)res.tv_nsec);
	}
	T0Value = 0x10000;				// Default timer speed is 55ms = 18.2 times per second

	// Remember the main thread ID
	maintid = pthread_self();

	// Setup the alarm system
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &TimerIRQ;
	sigaction (SIGALRM, &sa, NULL);
	// Setup the timer interval
	it.it_interval.tv_usec = (838*T0Value)/1000;
	it.it_value.tv_usec = it.it_interval.tv_usec;
	it.it_interval.tv_sec = 0;
	it.it_value.tv_sec = 0;
	setitimer(ITIMER_REAL, &it, &old);

	// Setup the other signal handlers
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &KeyboardIRQ;
	sigaction (SIGRTMIN+IRQ_KEYB, &sa, NULL);
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &SBIRQ;
	sigaction (SIGRTMIN+IRQ_SB, &sa, NULL);
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &PS2IRQ;
	sigaction (SIGRTMIN+IRQ_PS2, &sa, NULL);
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &COM2IRQ;
	sigaction (SIGRTMIN+IRQ_MOUSE, &sa, NULL);
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &ExitIRQ;
	sigaction (SIGRTMIN+IRQ_EXIT, &sa, NULL);
#ifdef RPi
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &COM1IRQ;
	sigaction (SIGRTMIN+IRQ_COM1, &sa, NULL);
	// Make the SIGTERM signal jump to exit_program
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &exit_program;
	sigaction (SIGTERM, &sa, NULL);
#endif
#else
	//--------------------------------------
	// Create the timer mutexes.
	//--------------------------------------
	pthread_mutex_init(&irqtime_mutex, NULL);
	pthread_mutex_init(&irqflag_mutex, NULL);

	//--------------------------------------
	// Launch the default 18.2Hz timer thread.
	//--------------------------------------
	irqint.tv_sec = 0;
	irqint.tv_nsec = 838*T0Value;	// Default timer speed is 55ms = 18.2 times per second
	pthread_create(&tid, NULL, timer_thread, NULL);
#endif
}

void ExitTimers()
{
#if !defined(RPi) && !defined(Roku)
	pthread_t oldtid = tid;
	//--------------------------------------
	// Exit if the timer thread is not running.
	//--------------------------------------
	if (0 == oldtid)
		return;
	//--------------------------------------
	// Make the current timer thread exit.
	// Any tid value different to the current will exit the timer thread.
	//--------------------------------------
	tid = 0;
	pthread_join(oldtid, NULL);	
	//--------------------------------------
	// Destroy the timer mutexes.
	//--------------------------------------
	pthread_mutex_destroy(&irqtime_mutex);
	pthread_mutex_destroy(&irqflag_mutex);
#endif
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

// Serialize the timer data into data + offset.
int timer_serialize(u8 *data, int offset)
{
	struct timespec nowtime, tmptime;
	clock_gettime(CLOCK_MONOTONIC, &nowtime);	// Get current time

	// First the timespec structures, save them as differences from the current time.
	tmptime.tv_nsec = nowtime.tv_nsec - T2inittime.tv_nsec;
	tmptime.tv_sec = nowtime.tv_sec - T2inittime.tv_sec;
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	tmptime.tv_nsec = nowtime.tv_nsec - adlib1.tv_nsec;
	tmptime.tv_sec = nowtime.tv_sec - adlib1.tv_sec;
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	tmptime.tv_nsec = nowtime.tv_nsec - adlib2.tv_nsec;
	tmptime.tv_sec = nowtime.tv_sec - adlib2.tv_sec;
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

// Unserialize the timer data from data + offset.
int timer_unserialize(u8 *data, int offset)
{
	struct timespec nowtime, tmptime;
	clock_gettime(CLOCK_MONOTONIC, &nowtime);	// Get current time

	// First the timespec structures, save them as differences from the current time.
	memcpy(&tmptime, data + offset, sizeof(tmptime));
	offset += sizeof(tmptime);
	T2inittime.tv_nsec = nowtime.tv_nsec - tmptime.tv_nsec;
	T2inittime.tv_sec = nowtime.tv_sec - tmptime.tv_sec;
	while (T2inittime.tv_nsec < 0)
	{
		T2inittime.tv_nsec += 1000000000;
		T2inittime.tv_sec--;
	}
	while (T2inittime.tv_nsec > 1000000000)
	{
		T2inittime.tv_nsec -= 1000000000;
		T2inittime.tv_sec++;
	}
	memcpy(&tmptime, data + offset, sizeof(tmptime));
	offset += sizeof(tmptime);
	adlib1.tv_nsec = nowtime.tv_nsec - tmptime.tv_nsec;
	adlib1.tv_sec = nowtime.tv_sec - tmptime.tv_sec;
	while (adlib1.tv_nsec < 0)
	{
		adlib1.tv_nsec += 1000000000;
		adlib1.tv_sec--;
	}
	while (adlib1.tv_nsec > 1000000000)
	{
		adlib1.tv_nsec -= 1000000000;
		adlib1.tv_sec++;
	}
	memcpy(data + offset, &tmptime, sizeof(tmptime));
	offset += sizeof(tmptime);
	adlib2.tv_nsec = nowtime.tv_nsec - tmptime.tv_nsec;
	adlib2.tv_sec = nowtime.tv_sec - tmptime.tv_sec;
	while (adlib2.tv_nsec < 0)
	{
		adlib2.tv_nsec += 1000000000;
		adlib2.tv_sec--;
	}
	while (adlib2.tv_nsec > 1000000000)
	{
		adlib2.tv_nsec -= 1000000000;
		adlib2.tv_sec++;
	}

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
