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



#include <stdio.h>
#include <stdlib.h>
#include "libretro.h"
#include "LibretroWrapper.h"



// Local Prototypes.
static bool retroEnvironmentCallback(unsigned cmd, void *data);
static void retroVideoRefreshCallback(const void *data, unsigned width, unsigned height, size_t pitch);
static void retroAudioSampleCallback(int16_t left, int16_t right);
static size_t retroAudioSampleBatchCallback(const int16_t *data, size_t frames);
static void retroInputPollCallback(void);
static int16_t retroInputStateCallback(unsigned port, unsigned device, unsigned index, unsigned id);



// Use this function to report keyboard events to the emulator.  (If it is supported by the emulator.)
// if (retroKeyEvent)
// {
//    retroKeyEvent(bool down, unsigned keycode, uint32_t character, uint16_t key_modifiers);
// }
//    down is set if the key is being pressed, or false if it is being released.
//    keycode is the RETROK value of the char.
//    character is the text character of the pressed key. (UTF-32).
//    key_modifiers is a set of RETROKMOD values or'ed together.
//       RETROKMOD_NONE | RETROKMOD_SHIFT | RETROKMOD_CTRL | RETROKMOD_ALT | RETROKMOD_META | RETROKMOD_NUMLOCK | RETROKMOD_CAPSLOCK | RETROKMOD_SCROLLOCK
retro_keyboard_event_t retroKeyEvent = NULL;



// Use this funciton to get audio data.  (If it is supported by the emulator.)
//    This should be called when the audio buffer needs more data.
// if (retroAudioGetData)
// {
//    retroAudioGetData();
// }
retro_audio_callback_t retroAudioGetData = NULL;



// Local globals.
static bool isRetroGamePause = false;



// Startup emulator.
//    'gamefile' is the full path to the game hardfile.
//    Returns true if successful or false if error.
bool retroStartup(const char *gamefile)
{
   // Check that the libretro version used by the emulator is compatible.
   if (retro_api_version() != RETRO_API_VERSION)
   {
      return false;
   }

   // Set environment callback.
   retro_set_environment(retroEnvironmentCallback);

   // Init libretro.
   retro_init();

   // Init libretro callback functions.
   retro_set_video_refresh(retroVideoRefreshCallback);
   retro_set_audio_sample(retroAudioSampleCallback);
   retro_set_audio_sample_batch(retroAudioSampleBatchCallback);
   retro_set_input_poll(retroInputPollCallback);
   retro_set_input_state(retroInputStateCallback);

   // Set controller port device.
   //    Not sure how to use this with these emulators.
   //    We may not need this?
//void retro_set_controller_port_device(unsigned port, unsigned device);

   // Get libretro system information.
   {
      struct retro_system_info info;
      retro_get_system_info(&info);
      // At the moment the emulators should load from a file.  Not a "ROM" held in RAM.
      if (!info.need_fullpath)
      {
         return false;
      }
   }

   // Load game.
   {
      struct retro_game_info info;
      info.path = gamefile;   // full path to game file (hardfile).
      info.data = NULL;
      info.size = 0;
      info.meta = NULL;
      if (!retro_load_game(&info));
      {
         // Error loading game.
         return false;
      }
   }

   // Get game system information.
   {
      struct retro_system_av_info info;
      retro_get_system_av_info(&info);
// TODO:
// Get game system information.
// Some useful stuff:
//      unsigned width = info.geometry.max_width;
//      unsigned height = info.geometry.max_height;
//      double fps = info.timing.fps;
//      double sample_rate = info.timing.sample_rate;
   }

   return true;
}



// Shutdown emulator.
void retroShutdown(void)
{
   retro_unload_game();
   retro_deinit();
}



// Play one frame.
void retroRun(void)
{
   // Play one frame (if not paused).
   if (!isRetroGamePause)
   {
      retro_run();
   }
}



// Pause emulator.
void retroPause(void)
{
   // Just set the 'isRetroGamePause' flag to keep retro_run() from being called.
   isRetroGamePause = true;
}



// Resume (paused) emulator.
void retroResume(void)
{
   // Resume retro_run().
   isRetroGamePause = false;
}



// Serialize.
//    Store the state of the emulator.
//    'filename' is the save file.
//    Returns 'true' if success or 'false' if state could not be saved.
bool retroSerialize(const char *filename)
{
   FILE *savefile;
   void *data;
   size_t size;
   // Get the size of the save state.
   //    This function returns '0' if serialization is not supported.
   size = retro_serialize_size();
   if (size == 0)
   {
      return false;
   }
   // Allocate the required memory.
   data = malloc(size);
   if (!data)
   {
      return false;
   }
   // Get the state of the emulator.
   if (!retro_serialize(data, size))
   {
      free(data);
      return false;
   }
   // Save the data.
   savefile = fopen(filename, "wb");
   if (!savefile)
   {
      free(data);
      return false;
   }
   if (fwrite(data, 1, size, savefile) != size)
   {
      free(data);
      fclose(savefile);
      remove(filename);
      return false;
   }
   if (fclose(savefile) != 0)
   {
      free(data);
      fclose(savefile);
      remove(filename);
      return false;
   }
   // Cleanup.
   free(data);
   return true;
}



// Unserialize.
//    Load save state into the emulator.
//    'filename' is the load file.
//    Returns 'true' if success or 'false' if state could not be loaded.
bool retroUnserialize(const char *filename)
{
   FILE *loadfile;
   void *data;
   size_t size;
   // Get size of file.
   loadfile = fopen(filename, "rb");
   if (!loadfile)
   {
      return false;
   }
   fseek(loadfile, 0, SEEK_END);
   size = ftell(loadfile);
   fseek(loadfile, 0, SEEK_SET);
   // Allocate required memory.
   data = malloc(size);
   if (!data)
   {
      fclose(loadfile);
      return false;
   }
   // Load the data.
   if (fread(data, 1, size, loadfile) != size)
   {
      free(data);
      fclose(loadfile);
      return false;
   }
   fclose(loadfile);
   // Set the state of the emulator.
   if (!retro_unserialize(data, size))
   {
      free(data);
      return false;
   }
   // Cleanup.
   free(data);
   return true;
}



//---------------------------------------------------------------------------
// Callbacks.
//---------------------------------------------------------------------------



// Retro environment.
bool retroEnvironmentCallback(unsigned cmd, void *data)
{
   switch (cmd)
   {
      case RETRO_ENVIRONMENT_SET_MESSAGE:
      {
         // Display a message from the emulator.
         // Is there any need for this?
         return false;
      }
      case RETRO_ENVIRONMENT_SHUTDOWN:
      {
         // The game/emulator has sent a shutdown request.
// TODO:
//    Windows Store apps are not supposed to shutdown.
//    Should we just reset the game here?
//         retro_reset();
//    Reset may not work if the emulator has shutdown.
//    Do we need to reload the game?
//    If we need to reset or reload the game we may not be able to do it within this callback?
         return false;
      }
      case RETRO_ENVIRONMENT_GET_SYSTEM_DIRECTORY:
      {
         // Return the "system" directory of the frontend.
         // Do we need this?
         return false;
      }
      case RETRO_ENVIRONMENT_SET_PIXEL_FORMAT:
      {
         // The pixel format used by the game.
         switch (*(enum retro_pixel_format *)data)
         {
// TODO:
//    What pixel formats do we support?
//    pax86 appears to use RGB565.
            case RETRO_PIXEL_FORMAT_0RGB1555:
            {
            }
            case RETRO_PIXEL_FORMAT_XRGB8888:
            {
            }
            case RETRO_PIXEL_FORMAT_RGB565:
            {
            }
         }
         return false;
      }
      case RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK:
      {
         // A function used to notify emulator about keyboard events.
         retroKeyEvent = ((struct retro_keyboard_callback *)data)->callback;
         return true;
      }
      case RETRO_ENVIRONMENT_SET_SUPPORT_NO_GAME:
      {
         // If true, the libretro implementation supports calls to retro_load_game() with NULL as argument.
         // We may need this depending on how the emulator/game is launched.
         return false;
      }
      case RETRO_ENVIRONMENT_GET_LIBRETRO_PATH:
      {
         // Give emulator absolute path from where this libretro implementation was loaded.
         // Do we need this?
         return false;
      }
      case RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK:
      {
         // Sets an interface which is used to notify a libretro core about audio being available for writing.
// TODO:
//    It looks like the pax86 emulator uses this.
//    Test this! (We may not actually need this.)
         retroAudioGetData = ((struct retro_audio_callback *)data)->callback;
         return true;
      }
      case RETRO_ENVIRONMENT_SET_FRAME_TIME_CALLBACK:
      {
         // Lets the core know how much time has passed since last invocation of retro_run().
         // This allows us to modify the amount of time between each frame.
         // Do we have any use for this?
         return false;
      }
   }
   return false;
}



// Draw video frame.
void retroVideoRefreshCallback(const void *data, unsigned width, unsigned height, size_t pitch)
{
   if (data)
   {
// TODO:
      // Copy pixels here.
      // We will need our own copy if we pause the game to do things like desplaying menus with the game screen in the background.
      // Get pixel format from RETRO_ENVIRONMENT_SET_PIXEL_FORMAT above.
   }
}



// Play one audio sample.
void retroAudioSampleCallback(int16_t left, int16_t right)
{
// TODO:
   // Play one sample of audio.
   // Samples are signed 16-bits.
   // Get sample rate from retro_get_system_av_info() above.
}



// Play a batch of audio samples.
//    Return number of frames played.
size_t retroAudioSampleBatchCallback(const int16_t *data, size_t frames)
{
   size_t playedFrames = 0;
   if (data)
   {
	   rlsa_control *ctrl = NULL;
		playedFrames = rlsa_PlayBuffer(ctrl, 0, (void *) data, frames);
   }
   return playedFrames;
}



// Poll input.
void retroInputPollCallback(void)
{
// TODO:
   // Poll input.
}



// Queries for input for player 'port'. device will be masked with RETRO_DEVICE_MASK.
// Specialization of devices such as RETRO_DEVICE_JOYPAD_MULTITAP that have been set with retro_set_controller_port_device()
// will still use the higher level RETRO_DEVICE_JOYPAD to request input.
int16_t retroInputStateCallback(unsigned port, unsigned device, unsigned index, unsigned id)
{
   int16_t state = 0;
// TODO:
   return state;
}

