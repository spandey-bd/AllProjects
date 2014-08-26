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

#ifndef __LIBRETRO_CALLBACKS_H__
#define __LIBRETRO_CALLBACKS_H__

#include "libretro.h"

#ifdef __cplusplus
extern "C" {
#endif

bool retroEnvironmentCallback(unsigned cmd, void *data);
void retroVideoRefreshCallback(const void *data, unsigned width, unsigned height, size_t pitch);
void retroInputPollCallback(void);
int16_t retroInputStateCallback(unsigned port, unsigned device, unsigned index, unsigned id);

size_t retroAudioSampleBatchCallback(const int16_t *data, size_t frames);

#ifdef __cplusplus
}
#endif

#endif // __LIBRETRO_CALLBACKS_H__
