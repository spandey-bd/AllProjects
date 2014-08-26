/**
*    Copyright (C) 2013-2014 Amiga Games, Inc. / Retro Infinity
*
*    This is CONFIDENTIAL INFORMATION and a trade secret of
*    Amiga Games, Inc. / Retro Infinity. It is not to be disclosed,
*    and not to be utilized in any way, other than as expressly
*    agreed in writing by Amiga Games, Inc. / Retro Infinity.
*
*    All rights reserved.
*
**/



#ifndef __LIBRETRO_WRAPPER_H__
#define __LIBRETRO_WRAPPER_H__

#include "libretro.h"
#include "rii-llsaudio.h"

#ifdef __cplusplus
extern "C" {
#endif

bool retroStartup(const char *gamefile);
void retroShutdown(void);
void retroRun(void);
void retroPause(void);
void retroResume(void);
bool retroSerialize(const char *filename);
bool retroUnserialize(const char *filename);
retro_keyboard_event_t retroKeyEvent;
retro_audio_callback_t retroAudioGetData;

#ifdef __cplusplus
}
#endif

#endif // __LIBRETRO_WRAPPER_H__
