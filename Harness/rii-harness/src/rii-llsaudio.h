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

#ifndef __RIILLSAUDIO_H__
#define __RIILLSAUDIO_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Structures */
typedef struct {
	
	uint8_t channels; // see defines
	int8_t  bpsFlags; // see defines
	uint16_t frequency;
	uint32_t bucketSize;

} rlsa_audioFormatParams;


typedef struct {

	uint32_t flags; // 32 status flags

	void *impData; // pointer for implementation's data, if desired.
	
	rlsa_audioFormatParams platformPreferedFormat;
	rlsa_audioFormatParams emulatorSuppliedFormat;
	rlsa_audioFormatParams platformActualFormat;
	
} rlsa_control;

/* Defines */

/*
 * Channels byte bits: 76543210
 * 
 * 0 = Left Channel
 * 1 = Right Channel
 * 2 = Center Front Channel (or Mono)
 * 3 = Sub channel
 * 4 = rear-left
 * 5 = rear-right
 * 6 = read-center
*/
//Defines for convenience
#define RLSA_CHANNELS_STEREO 0x03
#define RLSA_CHANNELS_MONO 0x04

/*
 * BPS Flags bits: 76543XXX
 * 
 * XXX:
 * 		1 = 8bps int
 *		2 = 16bps int
 * 		3 = 32bps int
 * 		4 = 32bps float
 * 	
 * 3 = 0/1 : Unsigned / Signed
 * 4 = 0/1 : Little Endian / Big Endian
*/
//Defines for convenience
#define RLSA_BPS_S16LE 0x0A

/* Macros */

/* Protos */

/*
 * The implementation should fill out the pnginePreferredFormat (audioFormatParams)
 * struct before returning. If the emulator has the ability to change
 * the format of audio it delivers, it will use the values. If there
 * are multiple native preferred formats.
 * 
 * Libretro's preferred format is s16le, 2-channel {left, right}, 22050 or 44100.
 * 
 */
int rlsa_Startup(rlsa_control *ctrl);

/*
 * When Init() is called, the emulator and harness will have filled out
 * the EmulatorSuppliedFormat (AudioFormatParams) struct and the
 * implementation should initialize the OS audio system and put in place
 * any filters in the filter chain to convert and/or enhance audio.
 * 
 * Init() may be called after Deinit() if the harness needs to change
 * audio formats.
 */
int rlsa_Init(rlsa_control *ctrl);

/*
 * The application is going to suspend
 */
int rlsa_Suspend(rlsa_control *cltr);

/*
 * The application is resuming from a suspend. It is possible, after a
 * suspend, that Deinit() then Shutdown() may be called if the app is
 * being closed by the OS.
 */
int rlsa_Resume(rlsa_control *ctrl);

/*
 * The opposite of Init(), this routine will be called if the emulator
 * is going to shut down/exit or if the parameters of
 * EmulatorSuppliedFormat have changed.
 */
int rlsa_Deinit(rlsa_control *ctrl);

/*
 * The application is going to close.
 */
int rlsa_Shutdown(rlsa_control *ctrl);



/*
 * Control various aspect of the implementation. If an implementation
 * doesn't support a control directive, it can be ignored.
 * 
 *   controlId : the numeric ID of a control, such as volume, pan, EQ,
 * 				 etc
 * 
 *   value1    : parameters for the control. Either or both can be used 
 *   value2    : by the control for convenience.
 * 
 */
int rlsa_Control(rlsa_control *ctrl, unsigned int controlId, int value1, void *value2);

/*
 * Dispatch the buffer to the OS for playback.
 * 
 *   flags         : Currently unused
 *   buffer        : Where the PCM data is held
 *   framesInBuffer: The number of audio frames in the buffer to play
 * 
 */ 
int rlsa_PlayBuffer(rlsa_control *ctrl, int flags, void *buffer, int framesInBuffer);

#ifdef __cplusplus
extern "C" {
#endif

#endif // __RIILLSAUDIOH__
