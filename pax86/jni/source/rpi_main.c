//=============================================================================
// rpi_main.c
//
// This file contains the Raspberry Pi libretro test frontend for pax86.
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

#ifdef RPi

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ncurses.h>
#include "libretro.h"

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

volatile bool running = false;

static int keymap[300];

static const char *message = NULL;

//---------------------------------------------------------
// libretro API callbacks.
//---------------------------------------------------------
static retro_keyboard_event_t key_cb;

bool retro_environment(unsigned cmd, void *data)
{
	switch (cmd)
	{
		case RETRO_ENVIRONMENT_SHUTDOWN:
			running = false;
			return true;
		case RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK:
			key_cb = ((struct retro_keyboard_callback *)data)->callback;
			return true;
		case RETRO_ENVIRONMENT_SET_MESSAGE:
			message = ((struct retro_message *)data)->msg;
			return true;
	}
	return false;
}

extern char *CGA_B800;
extern char TEXTBuffer[];
extern char *BIOSData;

bool usecolor = false;

static int colortable[8] = { COLOR_BLACK, COLOR_BLUE,
							 COLOR_GREEN, COLOR_CYAN,
							 COLOR_RED, COLOR_MAGENTA,
							 COLOR_YELLOW, COLOR_WHITE };
static int chartable[256];

void video_refresh(const void *data, unsigned width, unsigned height, size_t pitch)
{
	int x, y, pos;
	u16 *ptr = (u16 *)CGA_B800;
	u16 *buf = (u16 *)TEXTBuffer;

	for (y = 0; y < 25; y++)
	{
		for (x = 0; x < 80; x++)
		{
			pos = y*80 + x;
			if (ptr[pos] != buf[pos])
			{
				buf[pos] = ptr[pos];
				if (usecolor)
					attron(COLOR_PAIR(((ptr[pos]>>8)&7)+COLORS*((ptr[pos]>>12)&7)));
				mvaddch(y, x, chartable[ptr[pos]&255]);
				//if (usecolor)
				//	attroff(COLOR_PAIR(((ptr[pos]>>8)&7)+COLORS*((ptr[pos]>>12)&7)));
			}
		}
	}
	if (BIOSData[0x50] < 80 && BIOSData[0x51] < 25)
	{
		curs_set(1);
		move(BIOSData[0x51], BIOSData[0x50]);
	}
	else
		curs_set(0);

	refresh();
}

void retro_input_poll(void)
{

}

//---------------------------------------------------------
// Main routine
//---------------------------------------------------------


int main(int argc, char *argv[])
{
	int i, j;
	struct retro_system_info info;
	struct retro_game_info game;
	bool load_ok = false;

	// F1 = 27, 91, 49, 49, 126
	// F2 = 27, 91, 49, 50, 126
	// F3 = 27, 91, 49, 51, 126
	for (i=0; i < 300; i++)
		keymap[i] = 0;
	keymap[9] = RETROK_TAB;
	keymap[10] = RETROK_RETURN;
	for (i = 27; i < 127; i++)
		keymap[i] = i;
	keymap[258] = RETROK_DOWN;
	keymap[259] = RETROK_UP;
	keymap[260] = RETROK_LEFT;
	keymap[261] = RETROK_RIGHT;
	keymap[263] = RETROK_BACKSPACE;

	initscr();
	raw();
	keypad(stdscr, TRUE);
	nodelay(stdscr, TRUE);
	noecho();

	usecolor = has_colors();
	if (usecolor)
	{
		start_color();
		for (i = 0; i < COLORS; i++)
			for (j = 0; j < COLORS; j++)
				if (i > 0 || j > 0)
					init_pair(i + COLORS*j, colortable[i], colortable[j]);
	}
	printw("COLOR_PAIRS=%d, COLORS=%d\n", COLOR_PAIRS, COLORS);

	printw("ttyname() == '%s'\n", ttyname(STDIN_FILENO));

	for (i = 0; i < 256; i++)
		chartable[i] = i&0x7F;
	chartable[176] = chartable[177] = ACS_CKBOARD;
	chartable[179] = chartable[186] = ACS_VLINE;
	chartable[196] = chartable[205] = ACS_HLINE;
	chartable[183] = chartable[184] = chartable[187] = chartable[191] = ACS_URCORNER;
	chartable[192] = chartable[200] = chartable[212] = ACS_LLCORNER;
	chartable[218] = chartable[201] = ACS_ULCORNER;
	chartable[188] = chartable[217] = chartable[190] = ACS_LRCORNER;
	chartable[193] = chartable[202] = ACS_BTEE;
	chartable[194] = chartable[203] = ACS_TTEE;
	chartable[180] = chartable[181] = chartable[182] = chartable[185] = ACS_LTEE;
	chartable[197] = chartable[206] = ACS_PLUS;


	printw("retro_api_version = %d\n", retro_api_version());

	printw("retro_get_system_info() ...\n");
	retro_get_system_info(&info);
	printw("retro_get_system_info() done.\n");
	printw("\tinfo.library_name = %s\n", info.library_name);
	printw("\tinfo.library_version = %s\n", info.library_version);
	printw("\tinfo.valid_extensions = %s\n", info.valid_extensions);
	printw("\tinfo.need_fullpath = %d\n", info.need_fullpath);
	printw("\tinfo.block_extract = %d\n", info.block_extract);

	printw("retro_set_environment() ...\n");
	retro_set_environment(retro_environment);
	printw("retro_set_environment() done.\n");

	printw("retro_init() ...\n");
	retro_init();
	printw("retro_init() done.\n");

	printw("retro_set_video_refresh() ...\n");
	retro_set_video_refresh(video_refresh);
	printw("retro_set_video_refresh() done.\n");

	printw("retro_set_input_poll() ...\n");
	retro_set_input_poll(retro_input_poll);
	printw("retro_set_input_poll() done.\n");

	if (argc > 1 && argv[1] != NULL)
	{
		game.path = argv[1];
		printw("retro_load_game(%s) ...\n", game.path);
		load_ok = retro_load_game(&game);
		printw("retro_load_game(%s) = %d.\n", game.path, load_ok);

		if (load_ok)
		{
			running = true;
			do {
				retro_run();
				usleep(1000/60);
				// Read keyboard hits
				while ((i = getch()) != -1)
				{
					if (3 == i)		// Ctrl-C pressed
					{
						running = false;
						break;
					}
					if (key_cb)
					{
						j = keymap[i];
						if (j)
						{
							key_cb(true, j, i, 0);
							key_cb(false, j, i, 0);
						}
					}
				}
			} while (running);
		}

		printw("retro_unload_game() ...\n");
		retro_unload_game();
		printw("retro_unload_game() done.\n");
	}

	printw("retro_deinit() ...\n");
	retro_deinit();
	printw("retro_deinit() done.\n");

//	while ((i = getch()) == -1);
//	printw("Key = %02X", i);
	refresh();
	endwin();

//	printf("Key = %02X\n", i);
	if (message)
		printf("pax86 core exited: '%s'\n", message);
	else
		printf("Exiting ...\n");
	return 0;
}

#endif
