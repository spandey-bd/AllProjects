//=============================================================================
// native.c
//
// This file contains the Android libretro frontend for pax86.
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

#include <GLES/gl.h>
#include <GLES/glext.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "pax86.h"
#include "pacpu.h"
#include "libretro.h"

#define TEXTURE_WIDTH 1024
#define TEXTURE_HEIGHT 512
#define RGB565(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))

#define UNUSED  __attribute__((unused))

extern unsigned ccnt_read();
extern void ccnt_enable();

static GLuint s_texture = 0;

static void check_gl_error(const char* op)
{
	GLint error;
	for (error = glGetError(); error; error = glGetError())
		LOGI("after %s() glError (0x%x)\n", op, error);
}

int s_w = 0;
int s_h = 0;
static int CurScreenX = 0;
static int CurScreenY = 0;
extern int MaxScreenX;
extern int MaxScreenY;

/* disable these capabilities. */
static GLuint s_disable_caps[] = {
	GL_FOG,
	GL_LIGHTING,
	GL_CULL_FACE,
	GL_ALPHA_TEST,
	GL_BLEND,
	GL_COLOR_LOGIC_OP,
	GL_DITHER,
	GL_STENCIL_TEST,
	GL_DEPTH_TEST,
	GL_COLOR_MATERIAL,
	0
};

//------------------------------------------------------------
// Mapping for keys between PC scan codes (received from the
// Java side) and libretro keycodes (sent to libretro backend).
//------------------------------------------------------------
static int keymap[] = {
	0,
	RETROK_ESCAPE,			//  1 = Esc

	RETROK_1,				//  2 = 1
	RETROK_2,				//  3 = 2
	RETROK_3,				//  4 = 3
	RETROK_4,				//  5 = 4
	RETROK_5,				//  6 = 5
	RETROK_6,				//  7 = 6
	RETROK_7,				//  8 = 7
	RETROK_8,				//  9 = 8
	RETROK_9,				//  A = 9
	RETROK_0,				//  B = 0
	RETROK_MINUS,			//  C = -
	RETROK_EQUALS,			//  D = =
	RETROK_BACKSPACE,		//  E = BackSpace

	RETROK_TAB,				//  F = Tab
	RETROK_q,				// 10 = q
	RETROK_w,				// 11 = w
	RETROK_e,				// 12 = e
	RETROK_r,				// 13 = r
	RETROK_t,				// 14 = t
	RETROK_y,				// 15 = y
	RETROK_u,				// 16 = u
	RETROK_i,				// 17 = i
	RETROK_o,				// 18 = o
	RETROK_p,				// 19 = p
	RETROK_LEFTBRACKET,		// 1A = [
	RETROK_RIGHTBRACKET,	// 1B = ]

	RETROK_RETURN,			// 1C = Enter

	RETROK_LCTRL,			// 1D = Ctrl

	RETROK_a,				// 1E = a
	RETROK_s,				// 1F = s
	RETROK_d,				// 20 = d
	RETROK_f,				// 21 = f
	RETROK_g,				// 22 = g
	RETROK_h,				// 23 = h
	RETROK_j,				// 24 = j
	RETROK_k,				// 25 = k
	RETROK_l,				// 26 = l
	RETROK_SEMICOLON,		// 27 = ;
	RETROK_QUOTE,			// 28 = '

	RETROK_BACKQUOTE,		// 29 = `

	RETROK_LSHIFT,			// 2A = Shift

	RETROK_BACKSLASH,		// 2B = BackSlash

	RETROK_z,				// 2C = z
	RETROK_x,				// 2D = x
	RETROK_c,				// 2E = c
	RETROK_v,				// 2F = v
	RETROK_b,				// 30 = b
	RETROK_n,				// 31 = n
	RETROK_m,				// 32 = m
	RETROK_COMMA,			// 33 = ,
	RETROK_PERIOD,			// 34 = .
	RETROK_SLASH,			// 35 = /

	RETROK_RSHIFT,			// 36 = Right Shift
	RETROK_PRINT,			// 37 = Print Screen

	RETROK_LALT,			// 38 = Alt
	RETROK_SPACE,			// 39 = Space

	RETROK_CAPSLOCK,		// 3A = Caps

	RETROK_F1,				// 3B = F1
	RETROK_F2,				// 3C = F2
	RETROK_F3,				// 3D = F3
	RETROK_F4,				// 3E = F4
	RETROK_F5,				// 3F = F5
	RETROK_F6,				// 40 = F6
	RETROK_F7,				// 41 = F7
	RETROK_F8,				// 42 = F8
	RETROK_F9,				// 43 = F9
	RETROK_F10,				// 44 = F10

	RETROK_NUMLOCK,			// 45 = NumLock
	RETROK_SCROLLOCK,		// 46 = Scroll Lock

	RETROK_KP7,				// 47 = Home = KeyPad 7
	RETROK_KP8,				// 48 = Cursor Up = KeyPad 8
	RETROK_KP9,				// 49 = PgUp = KeyPad 9
	RETROK_KP_MINUS,		// 4A = KeyPad Minus
	RETROK_KP4,				// 4B = Cursor Left = KeyPad 4
	RETROK_KP5,				// 4C = KeyPad 5
	RETROK_KP6,				// 4D = Cursor Right = KeyPad 6
	RETROK_KP_PLUS,			// 4E = KeyPad Plus
	RETROK_KP1,				// 4F = End = KeyPad 1
	RETROK_KP2,				// 50 = Cursor Down = KeyPad 2
	RETROK_KP3,				// 51 = PgDn = KeyPad 3
	RETROK_KP0,				// 52 = Ins = KeyPad 0
	RETROK_KP_PERIOD,		// 53 = Del = KeyPad ,
	RETROK_KP_ENTER,		// 54 = E01C = Keypad Enter

	RETROK_RCTRL,			// 55 = E01D = Right Ctrl
	0,						// 56 = 1DE045E0D59D = Pause = Ctrl-NumLock

	RETROK_F11,				// 57 = F11
	RETROK_F12,				// 58 = F12
	// Extended keys
	RETROK_KP_DIVIDE,		// 59 = E035 = Keypad /
	0,						// 5A
	RETROK_KP_MULTIPLY,		// 5B = E037 = Keypad *
	RETROK_RALT,			// 5C = E038 = Right Alt

	RETROK_HOME,			// 5D = E047 = Home
	RETROK_UP,				// 5E = E048 = Cursor Up
	RETROK_PAGEUP,			// 5F = E049 = Page Up
	0,						// 60 = E04A
	RETROK_LEFT,			// 61 = E04B = Cursor Left
	0,						// 62 = E04C
	RETROK_RIGHT,			// 63 = E04D = Cursor Right
	0,						// 64 = E04E
	RETROK_END,				// 65 = E04F = End
	RETROK_DOWN,			// 66 = E050 = Cursor Down
	RETROK_PAGEDOWN,		// 67 = E051 = Page Down
	RETROK_INSERT,			// 68 = E052 = Insert
	RETROK_DELETE,			// 69 = E053 = Delete
};

//===========================================================
// libretro API callbacks.
//===========================================================

volatile bool running = false;
static retro_keyboard_event_t key_cb = 0;
static retro_audio_callback_t audio_cb = 0;
static const char *message = NULL;
static int16_t touchX = 0;
static int16_t touchY = 0;
static int16_t touchDown = 0;

//------------------------------------------------------------
// retro_environment() is called by the libretro backend to
// setup some libretro environment values.
//------------------------------------------------------------
bool retro_environment(unsigned cmd, void *data)
{
	switch (cmd)
	{
		case RETRO_ENVIRONMENT_SET_PIXEL_FORMAT:
			return (RETRO_PIXEL_FORMAT_RGB565 == *((enum retro_pixel_format *)data));
		case RETRO_ENVIRONMENT_SHUTDOWN:
			exit(0);	// Exit the program when the backend shuts down.
			return true;
		case RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK:
			key_cb = ((struct retro_keyboard_callback *)data)->callback;
			return true;
		case RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK:
			audio_cb = ((struct retro_audio_callback *)data)->callback;
			return true;
		case RETRO_ENVIRONMENT_SET_MESSAGE:
			message = ((struct retro_message *)data)->msg;
			LOGI("retro_message: '%s'\n", message);
			return true;
	}
	return false;
}

//------------------------------------------------------------
// video_refresh() is called once per frame by the libretro
// backend to actually blit the screen image on the screen.
//------------------------------------------------------------
void video_refresh(const void *data, unsigned width, unsigned height, size_t pitch)
{
	if (CurScreenX != width || CurScreenY != height)
	{
		// Resize the texture area if/when the graphics mode has changed.
		int rect[4] = {0, height, width, -height};
		glTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_CROP_RECT_OES, rect);
		CurScreenX = width;
		CurScreenY = height;
	}
	glClear(GL_COLOR_BUFFER_BIT);
	glTexImage2D(GL_TEXTURE_2D,			/* target */
			0,							/* level */
			GL_RGB,						/* internal format */
			TEXTURE_WIDTH,				/* width */
			TEXTURE_HEIGHT,				/* height */
			0,							/* border */
			GL_RGB,						/* format */
			GL_UNSIGNED_SHORT_5_6_5,	/* type */
			data);						/* pixels */
	check_gl_error("glTexImage2D");
	glDrawTexiOES(0, 0, 0, s_w, s_h);
	check_gl_error("glDrawTexiOES");
}

//------------------------------------------------------------
// input_poll() is called once per frame by the libretro
// backend to make us refresh the keyboard/mouse state.
//------------------------------------------------------------
void input_poll()
{
}

//------------------------------------------------------------
// input_state() is called by the libretro backend to query
// input device (keybpard/mouse/joystick) status.
//------------------------------------------------------------
int16_t input_state(unsigned port, unsigned device, unsigned index, unsigned id)
{
	if (RETRO_DEVICE_POINTER == device)
	{
		switch(id)
		{
			case RETRO_DEVICE_ID_POINTER_X:
				return touchX;
			case RETRO_DEVICE_ID_POINTER_Y:
				return touchY;
			case RETRO_DEVICE_ID_POINTER_PRESSED:
				return touchDown;
		}
	}
	return 0;
}

//------------------------------------------------------------
// audio_sample_batch() is called by the libretro backend to
// write the next batch of audio samples to the audio output.
// The actual handler is in audio.c source module.
//------------------------------------------------------------
extern size_t write_audio(const int16_t *data, size_t frames);
size_t audio_sample_batch(const int16_t *data, size_t frames)
{
	return write_audio(data, frames);
}

//------------------------------------------------------------
// Callback from Android audio system whenever the audio
// output queue is empty. Send the event to the libretro
// backend, requesting more audio data.
//------------------------------------------------------------
extern void silent_audio();
void audio_buffer_empty()
{
	if (running && audio_cb)
		audio_cb();		// Calls pax86retro.retro_audio_event()
	else
	{
		// The game is not running yet, so we need to send
		// some dummy data to the Android audio system to
		// keep it happy.
		silent_audio();
	}
}


//===========================================================
// Android native API routines, called from the Java code.
//===========================================================

//------------------------------------------------------------
// native_gl_resize()
//------------------------------------------------------------
void native_gl_resize(JNIEnv *env UNUSED, jclass clazz UNUSED, jint w, jint h)
{
	LOGI("native_gl_resize %d %d", w, h);
	//memset(TEXTBuffer, 0, 80*25*2);
	glDeleteTextures(1, &s_texture);
	GLuint *start = s_disable_caps;
	while (*start)
		glDisable(*start++);
	glEnable(GL_TEXTURE_2D);
	glGenTextures(1, &s_texture);
	glBindTexture(GL_TEXTURE_2D, s_texture);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glShadeModel(GL_FLAT);
	check_gl_error("glShadeModel");
	glColor4x(0x10000, 0x10000, 0x10000, 0x10000);
	check_gl_error("glColor4x");
	//int rect[4] = {0, TEXTURE_HEIGHT/2, TEXTURE_WIDTH/2, -TEXTURE_HEIGHT/2};
	int rect[4] = {0, MaxScreenY, MaxScreenX, -MaxScreenY};
	glTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_CROP_RECT_OES, rect);
	CurScreenX = MaxScreenX;
	CurScreenY = MaxScreenY;
	check_gl_error("glTexParameteriv");
	s_w = w;
	s_h = h;
}

static int i = 0;
static char *serptr = 0;
//------------------------------------------------------------
// native_key_click() is called asynchronously whenever a key
// event (key down, key up) has happened.
//------------------------------------------------------------
void native_key_click(JNIEnv *env UNUSED, jclass clazz UNUSED, jint key)
{
	if (key == 127 && i == 0)
	{
		i = retro_serialize_size();
		LOGI("retro_serialize_size() = %d\n", i);
		serptr = calloc(i, 1);
		if (serptr)
		{
			retro_serialize(serptr, i);
			LOGI("retro_serialize() done!\n");
		}
		return;
	}
	if (key == 126 && i > 0 && serptr)
	{
		retro_unserialize(serptr, i);
		LOGI("retro_unserialize() done!\n");
		return;
	}
	if (key_cb)
	{
		// The backend uses the key event callback, so send the key event directly.
		int k = key&0xE07F;
		if (0xE01C == k || 0xE01D == k)
			k = k - 0xE01C + 0x54;
		else if (k >= 0xE035 && k <= 0xE038)
			k = k - 0xE035 + 0x59;
		else if (k >= 0xE047 && k <= 0xE053)
			k = k - 0xE047 + 0x5D;
		if (k >= 0 && k <= 0x69)
		{
			//LOGI("key_cb(%d, %04X)", !(k&0x80), keymap[k]);
			key_cb(!(key&0x80), keymap[k], 0, 0);
		}
	}
}

//------------------------------------------------------------
// native_mouse_event() is called asynchronously whenever a touch
// event (stylys down, stylus up, stulus moved) has happened.
//------------------------------------------------------------
void native_mouse_event(JNIEnv *env, jclass clazz, jint x, jint y, jint mode)
{
	// We will get X coordinates between 0 and 800, and Y coordinates between 0 and 600.
	int tmpx = (x - 400) * 0x7FFF / 400;
	if (tmpx < -0x7FFF)
		touchX = -0x7FFF;
	else if (tmpx > 0x7FFF)
		touchX = 0x7FFF;
	else
		touchX = tmpx;
	touchY = (y - 300) * 0x7FFF / 300;
	touchDown = 1 == mode;
}

//------------------------------------------------------------
// native_gl_render() is called once for every frame (VSync).
//------------------------------------------------------------
void native_gl_render(JNIEnv *env UNUSED, jclass clazz UNUSED)
{
	if (running)
		retro_run();
}

//------------------------------------------------------------
// native_start() is called when the program starts, and
// whenever the screen orientation changes.
//------------------------------------------------------------
void native_start(JNIEnv *env, jclass clazz UNUSED, jstring fdir, jstring edir)
{
	struct retro_game_info game;
	char path[256];

	//---------------------------------------
	// If we are already running, do nothing here.
	//---------------------------------------
	if (running)
		return;

	//---------------------------------------
	// Setup the game path
	//---------------------------------------
	const char					*fpath = (*env)->GetStringUTFChars(env, fdir, 0);
	const char					*epath = (*env)->GetStringUTFChars(env, edir, 0);
	sprintf(path, "%s/4DOS.COM", fpath);
	game.path = path;
	LOGI("native_start, fdir='%s', game.path='%s'\n", fpath, game.path);
	(*env)->ReleaseStringUTFChars(env, fdir, fpath);
	(*env)->ReleaseStringUTFChars(env, edir, epath);

	//---------------------------------------
	// Initialize the libretro stuff.
	//---------------------------------------
	retro_set_environment(retro_environment);
	retro_init();
	retro_set_video_refresh(video_refresh);
	retro_set_audio_sample_batch(audio_sample_batch);
	retro_set_input_poll(input_poll);
	retro_set_input_state(input_state);

	//---------------------------------------
	// Prepare the game for running.
	//---------------------------------------
	running = retro_load_game(&game);

}


