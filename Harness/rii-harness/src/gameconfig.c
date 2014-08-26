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


#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <string.h>
#include "gameconfig.h"


// Load game configuration.
//    Returns 'true' if success or 'false' if error.
bool riM_getGameConfig(riM_GameConfig *config)
{
   lua_State *luaState;

   // Init config data defaults.
   memset(config, 0, sizeof(riM_GameConfig));
   config->saves = false;
   config->button_red = false;
   config->button_red_keycode = 0;
   config->button_blue = false;
   config->button_blue_keycode = 0;
   config->button_green = false;
   config->button_green_keycode = 0;
   config->button_yellow = false;
   config->button_yellow_keycode = 0;
   config->button_left = false;
   config->button_left_keycode = 0;
   config->button_right = false;
   config->button_right_keycode = 0;
   config->button_start = false;
   config->button_start_keycode = 0;
   config->button_select = false;
   config->button_select_keycode = 0;
   config->dpad = true;
   config->dpad_up_keycode = 0;
   config->dpad_down_keycode = 0;
   config->dpad_left_keycode = 0;
   config->dpad_right_keycode = 0;
   config->mouse_movement = MOUSE_MODE_NONE;
   config->mouse_speed = 4;
   config->mouse_pointer = MOUSE_POINTER_HOST;
   config->sleep_display = true;

// TODO:  Continue adding config items as needed.


   // Init lua.
   luaState = luaL_newstate();
   if (!luaState)
   {
      return false;
   }
   luaopen_base(luaState);
   luaopen_io(luaState);
   luaopen_string(luaState);
   luaopen_math(luaState);

   // Load config file.
   if (luaL_loadfile(luaState, "project.cfg") != 0)
   {
      lua_close(luaState);
      return false;
   }
   if (lua_pcall(luaState, 0, 0, 0) != 0)
   {
      lua_close(luaState);
      return false;
   }


   // Read config data.

   // Required config items.
   lua_getglobal(luaState, "cfg_name");
   if (lua_isstring(luaState, -1))
   {
      config->name = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_title");
   if (lua_isstring(luaState, -1))
   {
      config->title = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_version");
   if (lua_isstring(luaState, -1))
   {
      config->version = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_system");
   if (lua_isstring(luaState, -1))
   {
      config->system = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_model");
   if (lua_isstring(luaState, -1))
   {
      config->model = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_window_x");
   if (lua_isnumber(luaState, -1))
   {
      config->window_x = (int32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_window_y");
   if (lua_isnumber(luaState, -1))
   {
      config->window_y = (int32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_window_width");
   if (lua_isnumber(luaState, -1))
   {
      config->window_width = (int32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_window_height");
   if (lua_isnumber(luaState, -1))
   {
      config->window_height = (int32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);

   // Any other required items?

   // Fail if required config item is missing.
   if (!config->name || !config->title || !config->version || !config->system || !config->model)
   {
      lua_close(luaState);
      return false;
   }

   // Optional config items.
   lua_getglobal(luaState, "cfg_short");
   if (lua_isstring(luaState, -1))
   {
      config->short_name = _strdup(lua_tostring(luaState, -1));
   }
   lua_pop(luaState, 1);
   if (!config->short_name)
   {
      config->short_name = _strdup(config->name);   // Or should we use config->title here?  We may not need this here?
   }
   lua_getglobal(luaState, "cfg_saves");
   if (lua_isboolean(luaState, -1))
   {
      config->saves = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_red");
   if (lua_isboolean(luaState, -1))
   {
      config->button_red = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_red_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_red_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_blue");
   if (lua_isboolean(luaState, -1))
   {
      config->button_blue = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_blue_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_blue_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_green");
   if (lua_isboolean(luaState, -1))
   {
      config->button_green = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_green_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_green_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_yellow");
   if (lua_isboolean(luaState, -1))
   {
      config->button_yellow = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_yellow_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_yellow_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_left");
   if (lua_isboolean(luaState, -1))
   {
      config->button_left = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_left_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_left_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_right");
   if (lua_isboolean(luaState, -1))
   {
      config->button_right = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_right_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_right_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_start");
   if (lua_isboolean(luaState, -1))
   {
      config->button_start = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_start_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_start_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_select");
   if (lua_isboolean(luaState, -1))
   {
      config->button_select = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_button_select_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->button_select_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_dpad");
   if (lua_isboolean(luaState, -1))
   {
      config->dpad = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_dpad_up_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->dpad_up_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_dpad_down_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->dpad_down_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_dpad_left_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->dpad_left_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_dpad_right_keycode");
   if (lua_isnumber(luaState, -1))
   {
      config->dpad_right_keycode = (uint32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_mouse_movement");
   if (lua_isstring(luaState, -1))
   {
      const char *cfgString = lua_tostring(luaState, -1);
      if (strcmp(cfgString, "none") == 0)
      {
         config->mouse_movement = MOUSE_MODE_NONE;
      }
      else if (strcmp(cfgString, "joymouse") == 0)
      {
         config->mouse_movement = MOUSE_MODE_JOYMOUSE;
      }
      else if (strcmp(cfgString, "absolute") == 0)
      {
         config->mouse_movement = MOUSE_MODE_ABSOLUTE;
      }
      else if (strcmp(cfgString, "trackpad") == 0)
      {
         config->mouse_movement = MOUSE_MODE_TRACKPAD;
      }
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_mouse_speed");
   if (lua_isnumber(luaState, -1))
   {
      config->mouse_speed = (int32_t)lua_tonumber(luaState, -1);
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_mouse_pointer");
   if (lua_isstring(luaState, -1))
   {
      const char *cfgString = lua_tostring(luaState, -1);
      if (strcmp(cfgString, "none") == 0)
      {
         config->mouse_pointer = MOUSE_POINTER_NONE;
      }
      else if (strcmp(cfgString, "host") == 0)
      {
         config->mouse_pointer = MOUSE_POINTER_HOST;
      }
      else if (strcmp(cfgString, "native") == 0)
      {
         config->mouse_pointer = MOUSE_POINTER_NATIVE;
      }
      else if (strcmp(cfgString, "both") == 0)
      {
         config->mouse_pointer = MOUSE_POINTER_BOTH;
      }
   }
   lua_pop(luaState, 1);
   lua_getglobal(luaState, "cfg_sleep_display");
   if (lua_isboolean(luaState, -1))
   {
      config->sleep_display = lua_toboolean(luaState, -1);
   }
   lua_pop(luaState, 1);

// TODO:  continue adding optional config items as needed.




   // Cleanup.
   lua_close(luaState);

   return true;
}
