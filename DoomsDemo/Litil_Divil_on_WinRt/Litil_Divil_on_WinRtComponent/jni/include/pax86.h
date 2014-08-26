//=============================================================================
// pax86.h
//
// This is the common include file for all C code.
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

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
#if defined(__cplusplus)
#else
#if defined(_MSC_VER) && !defined(SN_TARGET_PS3) && !defined(__cplusplus)
#define bool unsigned char
#define true 1
#define false 0
#else
#include <stdbool.h>
#endif
#endif

#define RGB15(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))
#define	true 1
#define	false 0

#define LOG_TAG __FILE__
#ifdef RPi
#define	USECOM2	1
#endif
#if defined(RPi) || defined(WP8) || defined(IOS)
#define LOGI(...)  printf(__VA_ARGS__)
#else
#ifdef Roku
#define LOGI(...)  fprintf(stderr,__VA_ARGS__)
#else
#include <android/log.h>
#define LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#endif
#endif

// EMSPAGESHIFT: 14 if 16KB pages, 12 if 4KB pages
#define	EMSPAGESHIFT	14

// Retro Infinity exclusive features enabled
#define	RII_EX			1

#ifdef RII_EX

#if defined(__cplusplus)
	extern "C" {
#endif

extern int mem_serialize_size();							// XMS.c
extern int mem_serialize(u8 * data, int offset);			// XMS.c
extern int mem_unserialize(u8 *data, int offset);			// XMS.c
extern int files_serialize_size();							// DOS_file.c
extern int files_serialize(u8 * data, int offset);			// DOS_file.c
extern int files_unserialize(u8 *data, int offset);			// DOS_file.c
extern void files_close_before_unserialize();				// DOS_file.c
extern int ems_serialize_size();							// int67h.c
extern int ems_serialize(u8 * data, int offset);			// int67h.c
extern int ems_unserialize(u8 *data, int offset);			// int67h.c
extern int vga_serialize_size();							// int10h.c
extern int vga_serialize(u8 * data, int offset);			// int10h.c
extern int vga_unserialize(u8 *data, int offset);			// int10h.c
extern int timer_serialize_size();							// timer.c
extern int timer_serialize(u8 * data, int offset);			// timer.c
extern int timer_unserialize(u8 *data, int offset);			// timer.c

#if defined(__cplusplus)
	}
#endif

#endif
