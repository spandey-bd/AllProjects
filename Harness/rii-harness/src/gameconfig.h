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



#ifndef __GAMECONFIG_H__
#define __GAMECONFIG_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "libretro.h"   // This defines bool/true/false.  VS2012 doesn't have stdbool.h!

// Mouse mode options.
#define  MOUSE_MODE_NONE         0     // No mouse pointer movement.
#define  MOUSE_MODE_JOYMOUSE     1     // Mouse pointer is controlled by the joypad.
#define  MOUSE_MODE_ABSOLUTE     2     // Mouse pointer is controlled by absolute screen coordinates (touch screen positioning).
#define  MOUSE_MODE_TRACKPAD     3     // Mouse pointer is controlled like a laptop trackpad (using the touch screen).

// Mouse pointer options.  (All options may not be available on all platforms/emulators.)
#define  MOUSE_POINTER_NONE      0     // No mouse pointer displayed.
#define  MOUSE_POINTER_HOST      1     // Show host mouse pointer.
#define  MOUSE_POINTER_NATIVE    2     // Show in game mouse pointer.
#define  MOUSE_POINTER_BOTH      3     // Both HOST and NATIVE mouse pointers are displayed.


// Game config structure.
typedef struct
{

   char     *name;                  // required - unique game name
   char     *title;                 // required - readable version of the games name
   char     *short_name;            // optional - short title for the icon
   char     *version;               // required - game project version
   bool     saves;                  // optional - game has it's own saves option ('false' is default)
   char     *system;                // required - platform ("Amiga" / "DOS" / "Atari ST")
   char     *model;                 // required - general configuration of the selected system
                                    //             ("A500" / "A600" / "A1200")
                                    //             ("SVGA" / "VGA")
   int32_t  window_x;               // required - cropped x position of game view
   int32_t  window_y;               // required - cropped y position of game view
   int32_t  window_width;           // required - cropped width of game view
   int32_t  window_height;          // required - cropped heigth of game view
   bool     button_red;             // optional - display red button ('false' is default)
   uint32_t button_red_keycode;     // optional - keycode for red button ('0' is default = no keycode)
   bool     button_blue;            // optional - display blue button ('false' is default)
   uint32_t button_blue_keycode;    // optional - keycode for red button ('0' is default = no keycode)
   bool     button_green;           // optional - display green button ('false' is default)
   uint32_t button_green_keycode;   // optional - keycode for red button ('0' is default = no keycode)
   bool     button_yellow;          // optional - display yellow button ('false' is default)
   uint32_t button_yellow_keycode;  // optional - keycode for red button ('0' is default = no keycode)
   bool     button_left;            // optional - display left shoulder button ('false' is default)
   uint32_t button_left_keycode;    // optional - keycode for left shoulder button ('0' is default = no keycode)
   bool     button_right;           // optional - display right shoulder button ('false' is default)
   uint32_t button_right_keycode;   // optional - keycode for right shoulder button ('0' is default = no keycode)
   bool     button_start;           // optional - display "start" button ('false' is default)
   uint32_t button_start_keycode;   // optional - keycode for "start" button ('0' is default = no keycode)
   bool     button_select;          // optional - display "select" button ('false' is default)
   uint32_t button_select_keycode;  // optional - keycode for "select" button ('0' is default = no keycode)
   bool     dpad;                   // optional - display dpad ('true' is default)
   uint32_t dpad_up_keycode;        // optional - set keycode for dpad up ('0' is default = no keycode)
   uint32_t dpad_down_keycode;      // optional - set keycode for dpad down ('0' is default = no keycode)
   uint32_t dpad_left_keycode;      // optional - set keycode for dpad left ('0' is default = no keycode)
   uint32_t dpad_right_keycode;     // optional - set keycode for dpad right ('0' is default = no keycode)
   uint32_t mouse_movement;         // optional - see "mouse mode options" above. (MOUSE_MODE_NONE is default)
   int32_t  mouse_speed;            // optional - speed of the mouse pointer for joymouse and trackpad options (4 is default)
   uint32_t mouse_pointer;          // optional - see "mouse pointer options" above (MOUSE_POINTER_HOST is default)
   bool     sleep_display;          // optional - if 'false' the display will not enter sleep mode ('true' is default)

/*
cfg_licences = "Team17 WEPL UAEGPL"                      -- required - all licences related to the game
cfg_panel_1 = "0x006d"                                   -- optional - RIG panal buttons ('m' key to display map)
optional Amiga specific options
optional DOS specific options
*/



} riM_GameConfig;


// Prototypes.
bool riM_getGameConfig(riM_GameConfig *config);


#ifdef __cplusplus
}
#endif

#endif // __GAMECONFIG_H__
