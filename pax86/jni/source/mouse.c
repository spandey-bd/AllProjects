//=============================================================================
// mouse.c
//
// This file handles the mouse emulation. Both serial mouse (int 33h) and
// PS/2 mouse (int 15h) routines are in this source file.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2014 Patrick "Pate" Aalto
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

#include "pax86.h"
#include "pacpu.h"
#include "pados.h"

//#define printf  retappend

extern char BreakReasonText[];

#define	SCR_MODE	0x49
#define	SCR_PAGE	0x62
#define	SCR_ROWS	0x84

static int	oldmouseX = 0;
static int	oldmouseY = 0;
static bool useps2callback = false;
static bool ps2callbackinit = false;

#define QUEUE_SIZE 32
#define MOUSE_BUTTONS 2
#define MOUSE_IRQ 12
#define CURSORX 16
#define CURSORY 16
#define HIGHESTBIT (1<<(CURSORX-1))

static u16 defaultTextAndMask = 0x77FF;
static u16 defaultTextXorMask = 0x7700;

static u16 defaultScreenMask[CURSORY*2] = {
		// Screen mask:
		0x3FFF, 0x1FFF, 0x0FFF, 0x07FF,
		0x03FF, 0x01FF, 0x00FF, 0x007F,
		0x003F, 0x001F, 0x01FF, 0x00FF,
		0x30FF, 0xF87F, 0xF87F, 0xFCFF,
		// Cursor mask:
		0x0000, 0x4000, 0x6000, 0x7000,
		0x7800, 0x7C00, 0x7E00, 0x7F00,
		0x7F80, 0x7C00, 0x6C00, 0x4600,
		0x0600, 0x0300, 0x0300, 0x0000
};

static u16 userdefScreenMask[CURSORY*2];

typedef struct button_event {
	u8 type;
	u8 buttons;
} button_event;

typedef struct PACKED {
	u16	ax;
	u16	bx;
	u16	cx;
	u16	dx;
	u16	si;
	u16	di;
	u16	sub_offs;
	u16	sub_seg;
} MOUSESTORAGE;

typedef struct PACKED {
	u16 	fill1;
	u16		status;
	u8 		fill2;
	u8 		x;
	u16		fill3;
	u8		y;
	u8		fill4[5];
	u16		calloffs;
	u16		callseg;
} PS2CALLBACK;

typedef struct MOUSE {
	u8 		buttons;
	u16 	times_pressed[MOUSE_BUTTONS];
	u16 	times_released[MOUSE_BUTTONS];
	u16 	last_released_x[MOUSE_BUTTONS];
	u16 	last_released_y[MOUSE_BUTTONS];
	u16 	last_pressed_x[MOUSE_BUTTONS];
	u16 	last_pressed_y[MOUSE_BUTTONS];
	u16 	hidden;
	int 	add_x,add_y;
	int 	min_x,max_x,min_y,max_y;
	int 	mickey_x,mickey_y;				// 24.8 fixed point values
	int 	x, y;							// 16.16 fixed point values
	button_event event_queue[QUEUE_SIZE];
	u8		events;
	u16 	sub_mask;

	bool	background;
	int		backposx, backposy;
	u16		backData;		// Only for textmode cursor
	u16*	screenMask;
	int		clipx,clipy;
	int  	hotx,hoty;
	u16  	textAndMask, textXorMask;

	int		mickeysPerPixel_x;
	int		mickeysPerPixel_y;
	int		pixelPerMickey_x;
	int		pixelPerMickey_y;
	u16		senv_x_val;
	u16		senv_y_val;
	u16		dspeed_val;
	int		senv_x;
	int		senv_y;
	u16  	updateRegion_x[2];
	u16  	updateRegion_y[2];
	u16  	doubleSpeedThreshold;
	u16  	language;
	u16  	cursorType;
	u16		oldhidden;
	u8  	page;
	bool 	enabled;
	bool 	inhibit_draw;
	bool 	timer_in_progress;
	bool 	in_UIR;
	bool	drawSprite;
	u8 		mode;
} MOUSE;

static MOUSE mouse;

#define X_MICKEY 8
#define Y_MICKEY 8

#define MOUSE_HAS_MOVED 1
#define MOUSE_LEFT_PRESSED 2
#define MOUSE_LEFT_RELEASED 4
#define MOUSE_RIGHT_PRESSED 8
#define MOUSE_RIGHT_RELEASED 16
#define MOUSE_MIDDLE_PRESSED 32
#define MOUSE_MIDDLE_RELEASED 64
#define MOUSE_DELAY 5.0

extern int text_pos_by_page_col_row(int page, int col, int row);	// In TEXT.s
extern void CursorToSprite(u16 *mask);								// In gles_video.c
#if defined(RPi) || defined(Roku)
extern void SendMouseIRQ();											// In timer.c
extern void SendPS2IRQ();											// In timer.c
#else
extern void MouseIRQ();												// In pic.s
extern void PS2MouseIRQ();											// In pic.s
#endif

static void MouseEvent(u16 type) {
	MOUSESTORAGE* ms = (MOUSESTORAGE*)(BIOS_F000 + 0x1010);
	PS2CALLBACK* ps2 = (PS2CALLBACK*)(BIOS_F000 + 0x1060);
	if ( (mouse.sub_mask & type) && ms->sub_seg != 0 )
	{
		// Values interrupt routine is called with:.
		// AX = condition mask (same bit assignments as call mask).
		// BX = button state.
		// CX = cursor column.
		// DX = cursor row.
		// SI = horizontal mickey count.
		// DI = vertical mickey count
		ms->ax = type;
		ms->bx = mouse.buttons;
		ms->cx = (mouse.x>>16);
		ms->dx = (mouse.y>>16);
		ms->si = ((mouse.mickey_x*mouse.mickeysPerPixel_x)>>16);
		ms->di = ((mouse.mickey_y*mouse.mickeysPerPixel_y)>>16);
		//iprintf("But=%d, X=%d, Y=%d\n", MouseDriverStorage.bx, MouseDriverStorage.cx, MouseDriverStorage.dx );
#ifdef USECOM2
		SendPS2IRQ();	// Launch IRQ12 = INT74 (handler at F000:1060)
#else
#ifdef Roku
		SendCOM2IRQ();
#else
		MouseIRQ();		// Interrupt handler is at F000:1020
#endif
#endif
	}
	else if ( useps2callback )
	{
		// Handle PS/2 mouse callback
		// Setup status bits:
		// 7      Y data overflowed
		// 6      X data overflowed
		// 5      Y data is negative
		// 4      X data is negative
		// 3      reserved (1)
		// 2      reserved (0)
		// 1      right button pressed
		// 0      left button pressed
		u16 status = (mouse.buttons&3) | 8;
		int xdiff = (mouse.x-oldmouseX)>>16;
		int ydiff = (oldmouseY-mouse.y)>>16;
		if ( (xdiff > 2 || xdiff < -2) /*&& MOUSE_DPAD == CurrentConfig.mouse_mode*/ ) // Double-speed threshold in Windows 3.0
		{
			mouse.x += (mouse.x-oldmouseX);	// Add the difference to the mouse position
			oldmouseX += (xdiff<<16);
		}
		oldmouseX += (xdiff<<16);
		if ( (ydiff > 2 || ydiff < -2) /*&& MOUSE_DPAD == CurrentConfig.mouse_mode*/ ) // Double-speed threshold in Windows 3.0
		{
			mouse.y += (mouse.y-oldmouseY);	// Add the difference to the mouse position
			oldmouseY -= (ydiff<<16);
		}
		oldmouseY -= (ydiff<<16);
		
		if ((xdiff>0xff) || (xdiff<-0xff)) status |= 0x40;		// x overflow
		if ((ydiff>0xff) || (ydiff<-0xff)) status |= 0x80;		// y overflow
		if (xdiff<0) {
			xdiff += 0x100;
			status |= 0x10;
		}
		if (ydiff<0) {
			ydiff += 0x100;
			status |= 0x20;
		}
		xdiff &= 255;
		ydiff &= 255;
		ps2->status = status;
		ps2->x = xdiff;
		ps2->y = ydiff;
#if defined(RPi) || defined(Roku)
		SendPS2IRQ();	// Launch IRQ12 = INT74 (handler at F000:1060)
#else
		PS2MouseIRQ();	// Launch IRQ12 = INT74 (handler at F000:1060)
#endif
	}
}

// ***************************************************************************
// Mouse cursor - text mode
// ***************************************************************************

void RestoreCursorBackgroundText() {
	if (mouse.hidden || mouse.inhibit_draw)
		return;

	if (mouse.background)
	{
		((u16 *)CGA_B800)[text_pos_by_page_col_row(BIOSData[SCR_PAGE], mouse.backposx, mouse.backposy)>>1] = mouse.backData;
		mouse.background = false;
	}
}

void DrawCursorText() {	
	int pos;
	// Restore Background
	RestoreCursorBackgroundText();

	// Save Background
	mouse.backposx	= mouse.x >> (16+3);
	mouse.backposy	= mouse.y >> (16+3);

	pos = text_pos_by_page_col_row(BIOSData[SCR_PAGE], mouse.backposx, mouse.backposy)>>1;
	mouse.backData = ((u16 *)CGA_B800)[pos];
	mouse.background	= true;
	
	// Write Cursor
	((u16 *)CGA_B800)[pos] = (mouse.backData & mouse.textAndMask) ^ mouse.textXorMask;
	
	// Hide the graphics cursor, just in case...
	mouse.drawSprite = false;
	//OAM[0] = ATTR0_DISABLED;

}

// ***************************************************************************
// Mouse cursor - graphic mode
// ***************************************************************************

void DrawCursor() {
	if ( mouse.hidden || mouse.inhibit_draw )
	{
		mouse.drawSprite = false;
		return;
	}
		
	// In Textmode ?
	if (BIOSData[SCR_MODE] <= 0x03)
	{
		DrawCursorText();
		return;
	}

	// Check video page. Seems to be ignored for text mode. 
	// hence the text mode handled above this
	if (BIOSData[SCR_PAGE] != mouse.page)
	{
		mouse.drawSprite = false;
		return;
	}

	mouse.drawSprite = true;
}

// ***************************************************************************
// Global functions
// ***************************************************************************

void MouseNewVideoMode() {
	u8 mode = BIOSData[SCR_MODE];
	mouse.inhibit_draw=false;
	/* Get the correct resolution from the current video mode */
	switch (mode) {
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
			{
				u8 rows = BIOSData[SCR_ROWS];
				if ( 0==rows )
					rows = 25-1;
				mouse.max_y = 8*(rows+1)-1;
				break;
			}
		case 0x04:
		case 0x05:
		case 0x06:
		case 0x07:
		case 0x08:
		case 0x09:
		case 0x0a:
		case 0x0d:
		case 0x0e:
		case 0x13:
			mouse.max_y=199;
			break;
		case 0x0f:
		case 0x10:
			mouse.max_y=349;
			break;
		case 0x11:
		case 0x12:
		case 0x1D:
			mouse.max_y=479;
			break;
		case 0x1C:
			mouse.max_y=399;
			break;
		default:
			mouse.inhibit_draw=true;
			return;
	}
	//mouse.mode = mode;
	mouse.hidden = 1;
	mouse.drawSprite = false;
	mouse.max_x = 639;
	mouse.min_x = 0;
	mouse.min_y = 0;

	mouse.events = 0;
	mouse.timer_in_progress = false;
	//PIC_RemoveEvents(MOUSE_Limit_Events);

	mouse.hotx		 = 0;
	mouse.hoty		 = 0;
	mouse.background = false;
	mouse.screenMask = defaultScreenMask;
	mouse.textAndMask= defaultTextAndMask;
	mouse.textXorMask= defaultTextXorMask;
	mouse.language   = 0;
	mouse.page       = 0;
	mouse.doubleSpeedThreshold = 64;
	mouse.updateRegion_x[0] = 1;
	mouse.updateRegion_y[0] = 1;
	mouse.updateRegion_x[1] = 1;
	mouse.updateRegion_y[1] = 1;
	mouse.cursorType = 0;
	mouse.enabled=true;
	mouse.oldhidden=1;

	oldmouseX = mouse.x;
	oldmouseY = mouse.y;
	
	// Prepare the hardware sprite for mouse cursor emulation
	CursorToSprite(mouse.screenMask);
}

bool CalcMouseScreenCoords(int *x, int *y, int *hx, int *hy) {
	int mx = mouse.x;
	int	my = mouse.y;
	// In 320-pixel wide modes we have doubled the X coordinate.
	switch ( BIOSData[SCR_MODE] )		// Depending on mode...
	{
		case 0x04:		// CGA 320x200
		case 0x05:		// CGA 320x200
		case 0x0D:		// EGA 320x200
		case 0x13:		// MCGA 320x200
			mx >>= 1;
			break;
	}
	*x = mx>>16;
	*y = my>>16;
	*hx = mouse.hotx;
	*hy = mouse.hoty;
	return mouse.drawSprite && !(*x >= 1024-16 || *x < -16 || *y >= 512-16 || *y < -16);
}

void MouseButtonPressed(int button) {
	if ( 0 == button )
		mouse.buttons |= 1;
	else
		mouse.buttons |= 2;
	mouse.times_pressed[button]++;
	mouse.last_pressed_x[button] = mouse.x>>16;
	mouse.last_pressed_y[button] = mouse.y>>16;
	MouseEvent(0 == button ? MOUSE_LEFT_PRESSED : MOUSE_RIGHT_PRESSED );
}

void MouseButtonReleased(int button) {
	if ( 0 == button )
		mouse.buttons &= ~1;
	else
		mouse.buttons &= ~2;
	mouse.times_released[button]++;	
	mouse.last_released_x[button] = mouse.x>>16;
	mouse.last_released_y[button] = mouse.y>>16;
	MouseEvent(0 == button ? MOUSE_LEFT_RELEASED : MOUSE_RIGHT_RELEASED );
}

extern u8 VGA_CRTC_data_3D5[];

void MouseMoved(int x, int y) {
	// This is used by the D-Pad mouse handling.
	if ( x != 0 )
	{
		int dx = x*mouse.pixelPerMickey_x;
		if ( useps2callback )
			dx >>= 1;
		mouse.x += dx;
		mouse.mickey_x += (dx>>8);
		if ( mouse.x < (mouse.min_x<<16) )
			mouse.x = (mouse.min_x<<16);
		if ( mouse.x > (mouse.max_x<<16) )
			mouse.x = (mouse.max_x<<16);
	}
	if ( y != 0 )
	{
		int dy = y*mouse.pixelPerMickey_y;
		mouse.y += dy;
		mouse.mickey_y += (dy>>8);
		if ( mouse.y < (mouse.min_y<<16) )
			mouse.y = (mouse.min_y<<16);
		if ( mouse.y > (mouse.max_y<<16) )
			mouse.y = (mouse.max_y<<16);
	}
	MouseEvent(MOUSE_HAS_MOVED);
	DrawCursor();
}

void CalcMouseTouchPos(int x, int y) {
	x -= mouse.hotx;
	y -= mouse.hoty;

	// In 320-pixel wide modes we need to double the X coordinates.
	switch ( BIOSData[SCR_MODE] )		// Depending on mode...
	{
		case 0x00:
		case 0x01:
		case 0x02:
		case 0x03:
			{
				u8 rows = BIOSData[SCR_ROWS];
				if ( 0==rows )
					rows = 25-1;
				if (rows <= 25)
					y >>= 1;
				break;
			}
			break;
		case 0x04:		// CGA 320x200
		case 0x05:		// CGA 320x200
		case 0x0D:		// EGA 320x200
		case 0x13:		// MCGA 320x200
			x <<= 1;
			break;
		case 0x1C:		// SVGA 640x400
		case 0x1D:		// SVGA 640x480 (for Ascendancy)
			y >>= 1;
			break;
	}
	mouse.x = (x*mouse.pixelPerMickey_x)<<2;
	mouse.y = (y*mouse.pixelPerMickey_y)<<3;
	mouse.mickey_x = x*mouse.mickeysPerPixel_x;
	mouse.mickey_y = y*mouse.mickeysPerPixel_y;
	if ( mouse.x < (mouse.min_x<<16) )
		mouse.x = (mouse.min_x<<16);
	if ( mouse.x > (mouse.max_x<<16) )
		mouse.x = (mouse.max_x<<16);
	if ( mouse.y < (mouse.min_y<<16) )
		mouse.y = (mouse.min_y<<16);
	if ( mouse.y > (mouse.max_y<<16) )
		mouse.y = (mouse.max_y<<16);
}

void MouseTouchDown(int button, int x, int y) {
	CalcMouseTouchPos(x, y);
	if ( 0 == button )
		mouse.buttons |= 1;
	else
		mouse.buttons |= 2;
	mouse.times_pressed[button]++;
	mouse.last_pressed_x[button] = mouse.x>>16;
	mouse.last_pressed_y[button] = mouse.y>>16;
	MouseEvent((0 == button ? MOUSE_LEFT_PRESSED : MOUSE_RIGHT_PRESSED));
	DrawCursor();
}

void MouseTouchUp(int button, int x, int y) {
	// Ignore touchscreen mouse up if the mouse button was not down.
	// This avoids the shadow mouse up right after switching to touchscreen mouse mode.
	//if ( 0 == (mouse.buttons & 1) )
	//	return;
	CalcMouseTouchPos(x, y);
	if ( 0 == button )
		mouse.buttons &= ~1;
	else
		mouse.buttons &= ~2;
	mouse.times_released[button]++;	
	mouse.last_released_x[button] = mouse.x>>16;
	mouse.last_released_y[button] = mouse.y>>16;
	MouseEvent((0 == button ? MOUSE_LEFT_RELEASED : MOUSE_RIGHT_RELEASED));
	DrawCursor();
}

void MouseTouchMove(int x, int y) {
	CalcMouseTouchPos(x, y);
	MouseEvent(MOUSE_HAS_MOVED);
	DrawCursor();
}

// ***************************************************************************
// Internal functions
// ***************************************************************************

static void MouseSetMickeyPixelRate(int px, int py) {
	if ( px!=0 && py!=0 )
	{
		mouse.mickeysPerPixel_x	 = (px<<8)/X_MICKEY;	// Used when returning mickey values
		mouse.mickeysPerPixel_y  = (py<<8)/Y_MICKEY;	// Used when returning mickey values
		mouse.pixelPerMickey_x	 = (X_MICKEY<<14)/px;	// Used when calculating mouse cursor position
		mouse.pixelPerMickey_y 	 = (Y_MICKEY<<14)/py;	// Used when calculating mouse cursor position
	}
}

static void MouseSetSensitivity(int px, int py, int dspeed) {
	if ( px>100 )
		px=100;
	if ( py>100 )
		py=100;
	if ( dspeed>100 )
		dspeed=100;
	// save values
	mouse.senv_x_val = px;
	mouse.senv_y_val = py;
	mouse.dspeed_val = dspeed;
	if ( px!=0 && py!=0 )
	{
		px--;  //Inspired by cutemouse 
		py--;  //Although their cursor update routine is far more complex then ours
		mouse.senv_x=((px*px)<<16)/3600 + (1<<16)/3;
		mouse.senv_y=((py*py)<<16)/3600 + (1<<16)/3;
     }
}

static void MouseResetHardware() {
	//PIC_SetIRQMask(MOUSE_IRQ,false);
}

static void MouseReset() {
	if (BIOSData[SCR_MODE] <= 0x03)
		RestoreCursorBackgroundText();
	else
		mouse.drawSprite = false;

	mouse.hidden = 1;

	MouseNewVideoMode();
	MouseSetMickeyPixelRate(8,16);

	mouse.mickey_x = 0;
	mouse.mickey_y = 0;

	mouse.x = (((mouse.max_x + 1)<<16)/ 2);
	mouse.y = (((mouse.max_y + 1)<<16)/ 2);
	mouse.sub_mask = 0;
	mouse.in_UIR = false;
}

//--------------------------
// PS/2 pointing device 15h interrupt.
//--------------------------
int MouseInt15h(int al) {
	switch (al)
	{
		case 0x00:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - ENABLE/DISABLE
			// BH = new state
			// 	00h disabled
			// 	01h enabled
			// Return:CF set on error
			// AH = status (see #00522)
			//
			// Note: IBM classifies this function as required 
			//
			// (Table 00522)
			// Values for pointing device function status:
			// 00h    successful
			// 01h    invalid function
			// 02h    invalid input
			// 03h    interface error
			// 04h    need to resend
			// 05h    no device handler installed
			if ( 0 == BH )
			{
				useps2callback = false;
#ifdef USECOM2
				// Default IRQ12 points to normal mouse handler!
				if (0xF0001060 == INTVectors[0x74])
					INTVectors[0x74] = 0xF0001020;	
#endif
				CLR_CF;
			}
			else if ( 1 == BH )
			{
				if ( !ps2callbackinit)
				{
					useps2callback = false;
#ifdef USECOM2
					// Default IRQ12 points to normal mouse handler!
					if (0xF0001060 == INTVectors[0x74])
						INTVectors[0x74] = 0xF0001020;
#endif
					SET_AX(0x0500);				// No device handler installed
					SET_CF;
					return 0;
				}
				useps2callback = true;
#ifdef USECOM2
				// Default IRQ12 points to PS/2 mouse handler!
				if (0xF0001020 == INTVectors[0x74])
					INTVectors[0x74] = 0xF0001060;
#endif
				CLR_CF;
			}
			else
			{
				SET_CF;
				SET_AX(0x0100);				// Invalid function
			}
			return 0;
		case 0x01:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - RESET
			// Return:CF set on error
			// AH = status (see #00522)
			// CF clear if successful
			// BH = device ID
			// BL = value returned by attached device after reset
			// AAh if device is a mouse
			//
			// Notes: After successful completion of this call, the pointing device is set as follows:
			//	Disabled, sample rate 100 Hz, resolution 4 counts/mm, scaling 1:1, unchanged data package size.
			SET_BX(0x00AA);
			// Fall-through
		case 0x05:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - INITIALIZE
			// BH = data package size (1 - 8 bytes)
			// Return:CF set on error
			// AH = status (see #00522)
			//
			// Note: The pointing device is set as follows: disabled, 100 Hz sample rate, resolution 4 counts/mm, scaling 1:1 
			MouseReset();
			CLR_CF;
			SET_AH(0);								// AH = 0 == OK
			return 0;
		case 0x04:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - GET TYPE
			SET_BH(0);	// Device-ID = 0
			// Fall-through
		case 0x02:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - SET SAMPLING RATE
			// BH = sampling rate
			// 00h 10/second
			// 01h 20/second
			// 02h 40/second (Windows 3.00a)
			// 03h 60/second
			// 04h 80/second
			// 05h 100/second
			// 06h 200/second
			// Return:CF set on error
			// AH = status (see #00522)
			// Flow-through
		case 0x03:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - SET RESOLUTION
			// BH = resolution (see #00523)
			// Return:CF set on error
			// AH = status (see #00522)
			//
			// (Table 00523)
			// Values for pointing device resolution:
			// 00h    one count per mm
			// 01h    two counts per mm
			// 02h    four counts per mm
			// 03h    eight counts per mm (Windows 3.00a)
			CLR_CF;
			SET_AH(0);								// AH = 0 == OK
			return 0;
		case 0x06:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - EXTENDED COMMANDS
			// BH = subfunction
			// 	00h return device status
			// 		Return:
			//		BL = pointing device status (see #00524)
			// 		CL = resolution (see #00523)
			// 		DL = sample rate, reports per second
			// 	01h set scaling at 1:1
			// 	02h set scaling at 2:1
			// Return:CF set on error
			// AH = status (see #00522)
			//
			// Bitfields for pointing device status:
			//
			// Bit(s)  Description     (Table 00524)
			// 0      right button pressed
			// 1      reserved
			// 2      left button pressed
			// 3      reserved
			// 4      0 if 1:1 scaling, 1 if 2:1 scaling
			// 5      device enabled
			// 6      0 if stream mode, 1 if remote mode
			// 7      reserved
			switch (BH)
			{
				case 0x01:	// 	01h set scaling at 1:1
					CLR_CF;
					SET_AH(0);								// AH = 0 == OK
					return 0;
			}
			return -1;
		case 0x07:	// SYSTEM - POINTING DEVICE BIOS INTERFACE (PS) - SET DEVICE HANDLER ADDR
			// ES:BX -> FAR user device handler or 0000h:0000h to cancel (Windows 3.00a: 1107:0094)
			// Return:CF set on error
			// AH = status (see #00522)
			//
			// Note: When the subroutine is called, it is passed the following values on the stack;
			//	the handler should return with a FAR return without popping the stack: 
			//
			// WORD 1:Status (see #00525)		[BP+0C]
			// WORD 2:X data (high byte = 00h)	[BP+0A]
			// WORD 3:Y data (high byte = 00h)	[BP+08]
			// WORD 4:0000h
			//
			// See Also: INT 33/AX=000Ch 
			//
			// Bitfields for pointing device status:
			//
			// Bit(s)  Description     (Table 00525)
			// 15-8   reserved (0)
			// 7      Y data overflowed
			// 6      X data overflowed
			// 5      Y data is negative
			// 4      X data is negative
			// 3      reserved (1)
			// 2      reserved (0)
			// 1      right button pressed
			// 0      left button pressed
			{
#if 0
				PS2CALLBACK* ps2 = (PS2CALLBACK*)PHYS_PTR(0xF000, 0x1060);
#else
				PS2CALLBACK* ps2 = (PS2CALLBACK*)(BIOS_F000 + 0x1060);
#endif
				ps2->calloffs = BX;
				ps2->callseg = REG_ES;
				ps2callbackinit = (ps2->callseg != 0 || ps2->calloffs != 0);
				if ( !ps2callbackinit )
				{
					useps2callback = false;
#ifdef USECOM2
					// Default IRQ12 points to normal mouse handler!
					if (0xF0001060 == INTVectors[0x74])
						INTVectors[0x74] = 0xF0001020;
#endif
				}
				CLR_CF;
				SET_AH(0);								// AH = 0 == OK
			}
			return 0;
	}
	return -1;
}

//--------------------------
// Mouse 33h interrupt.
//--------------------------
int int33h(int ax) {
	//-----------------------------
	// MS MOUSE, uses AX for switch
	//-----------------------------
	switch ( ax )
	{
		case 0x0000:	// MS MOUSE - RESET DRIVER AND READ STATUS
			MouseResetHardware();
			// Fall-thru
		case 0x0021: 	// This call is identical to funtion 00h, but does not reset the mouse
			// Return:
			//	AX = status
			// 		0000h hardware/driver not installed
			// 		FFFFh hardware/driver installed
			// 	BX = number of buttons
			// 		0000h other than two
			// 		0002h two buttons (many drivers)
			// 		0003h Mouse Systems/Logitech three-button mouse
			// 		FFFFh two buttons
			//
			// Notes: Since INT 33 might be uninitialized on old machines, the caller should first check that INT 33 is neither 0000h:0000h
			// nor points at an IRET instruction (BYTE CFh) before calling this API.
			// To use mouse on a Hercules-compatible monographics card in graphics mode,
			// you must first set 0040h:0049h to 6 for page 0 or 5 for page 1, and then call this function.
			// Logitech drivers v5.01 and v6.00 reportedly do not correctly use Hercules graphics in dual-monitor systems,
			// while version 4.10 does.. The Logitech mouse driver contains the signature string "LOGITECH" three bytes past the interrupt handler;
			// many of the Logitech mouse utilities check for this signature..
			// Logitech MouseWare v6.30 reportedly does not support CGA video modes if no CGA is present
			// when it is started and the video board is later switched into CGA emulation 
			//
			// See Also: AX=0011h - AX=0021h - AX=002Fh - INT 62/AX=007A 
			//
			// Category: Mouse/Pointing Device - Int 33h - M 
			//if ( DPAD_MOUSE == CurrentConfig.dpad_mode )
			SET_AX(0xFFFF);				// Hardware installed
			SET_BX(MOUSE_BUTTONS);		// 2 buttons
			MouseReset();
			return 0;
		case 0x0001:	// MS MOUSE v1.0+ - SHOW MOUSE CURSOR
			if ( mouse.hidden )
				mouse.hidden--;
			DrawCursor();
			return 0;
		case 0x0002:	// MS MOUSE v1.0+ - HIDE MOUSE CURSOR
			if (BIOSData[SCR_MODE] <= 0x03)
				RestoreCursorBackgroundText();
			else
				mouse.drawSprite = false;
			mouse.hidden++;
			return 0;
		case 0x0003:	// MS MOUSE v1.0+ - RETURN POSITION AND BUTTON STATUS
			// Return:BX = button status (see #03168)
			// CX = column
			// DX = row
			//
			// Note: In text modes, all coordinates are specified as multiples of the cell size, typically 8x8 pixels 
			//
			// See Also: AX=0004h - AX=000Bh - INT 2F/AX=D000h"ZWmous" 
			//
			// Bitfields for mouse button status:
			//
			// Bit(s)  Description     (Table 03168)
			// 0      left button pressed if 1
			// 1      right button pressed if 1
			// 2      middle button pressed if 1 (Mouse Systems/Logitech/Genius)
			//
			// Category: Mouse/Pointing Device - Int 33h - M 
			SET_BX((int)mouse.buttons);
			SET_CX(mouse.x>>16);
			SET_DX(mouse.y>>16);
			return 0;
		case 0x0004:	// MS MOUSE v1.0+ - POSITION MOUSE CURSOR
			// CX = column
			// DX = row
			//
			// Note: The row and column are truncated to the next lower multiple of the cell size (typically 8x8 in text modes);
			// however, some versions of the Microsoft documentation incorrectly state that the coordinates are rounded 
			//
			// See Also: AX=0003h - INT 62/AX=0081h - INT 6F/AH=10h"F_PUT_SPRITE" 
			//
			// Category: Mouse/Pointing Device - Int 33h - M 
			/* If position isn't different from current position
			 * don't change it then. (as position is rounded so numbers get
			 * lost when the rounded number is set) (arena/simulation Wolf) */
			if (CX >= mouse.max_x)
				mouse.x = mouse.max_x<<16;
			else if (mouse.min_x >= CX)
				mouse.x = mouse.min_x<<16; 
			else if ( CX<<16 != (int)(mouse.x&0xFFFF0000) )
				mouse.x = CX<<16;

			if (DX >= mouse.max_y)
				mouse.y = mouse.max_y<<16;
			else if (mouse.min_y >= DX)
				mouse.y = mouse.min_y<<16; 
			else if ( DX<<16 != (int)(mouse.y&0xFFFF0000) )
				mouse.y = DX<<16;
			DrawCursor();
			return 0;
		case 0x0005:	// MS MOUSE v1.0+ - RETURN BUTTON PRESS DATA
			// BX = button number (see #03169)
			// Return:
			//	AX = button states (see #03168)
			// 	BX = number of times specified button has been pressed since last call
			// 	CX = column at time specified button was last pressed
			// 	DX = row at time specified button was last pressed
			{
				int but = BX&1;
				SET_AX((int)mouse.buttons);
				SET_BX(mouse.times_pressed[but]);
				SET_CX(mouse.last_pressed_x[but]);
				SET_DX(mouse.last_pressed_y[but]);
				mouse.times_pressed[but]=0;
			}
			return 0;
		case 0x0006:	// MS MOUSE v1.0+ - RETURN BUTTON RELEASE DATA
			// BX = button number (see #03169)
			// Return:
			// 	AX = button states (see #03168)
			// 	BX = number of times specified button has been released since last call
			// 	CX = column at time specified button was last released
			// 	DX = row at time specified button was last released
			{
				int but = BX&1;
				SET_AX((int)mouse.buttons);
				SET_CX(mouse.last_released_x[but]);
				SET_DX(mouse.last_released_y[but]);
				SET_BX(mouse.times_released[but]);
				mouse.times_released[but]=0;
			}
			return 0;
		case 0x0007:	// MS MOUSE v1.0+ - DEFINE HORIZONTAL CURSOR RANGE
			// CX = minimum column
			// DX = maximum column
			//
			// Note: In text modes, the minimum and maximum columns are truncated to the next lower multiple of the cell size, typically 8x8 pixels 
			{	//lemmings set 1-640 and wants that. iron seeds set 0-640 but doesn't like 640
				//Iron seed works if newvideo mode with mode 13 sets 0-639
				//Larry 6 actually wants newvideo mode with mode 13 to set it to 0-319
				int max,min;
				if ( CX < DX)
				{ 
					min=CX;
					max=DX;
				}
				else
				{
					min=DX;
					max=CX;
				}
				mouse.min_x=min;
				mouse.max_x=max;
				/* Battlechess wants this */
				if (mouse.x > (mouse.max_x<<16))
					mouse.x = mouse.max_x<<16;
				if (mouse.x < (mouse.min_x<<16))
					mouse.x = mouse.min_x<<16;
				/* Or alternatively this: 
				mouse.x = (mouse.max_x - mouse.min_x + 1)/2;*/
			}
			return 0;
		case 0x0008:	// MS MOUSE v1.0+ - DEFINE VERTICAL CURSOR RANGE
			// CX = minimum row
			// DX = maximum row
			//
			// Note: In text modes, the minimum and maximum rows are truncated to the next lower multiple of the cell size, typically 8x8 pixels 
			{
				int max,min;
				if ( CX < DX)
				{ 
					min=CX;
					max=DX;
				}
				else
				{
					min=DX;
					max=CX;
				}
				mouse.min_y=min;
				mouse.max_y=max;
				/* Battlechess wants this */
				if (mouse.y > (mouse.max_y<<16))
					mouse.y = mouse.max_y<<16;
				if (mouse.y < (mouse.min_y<<16))
					mouse.y = mouse.min_y<<16;
				/* Or alternatively this: 
				mouse.y = (mouse.max_y - mouse.min_y + 1)/2;*/
			}
			return 0;
		case 0x0009:	// MS MOUSE v3.0+ - DEFINE GRAPHICS CURSOR			
			// BX = column of cursor hot spot in bitmap (-16 to 16)
			// CX = row of cursor hot spot (-16 to 16)
			// ES:DX -> mask bitmap (see #03170)
			//
			// Notes: In graphics modes, the screen contents around the current mouse cursor position
			// are ANDed with the screen mask and then XORed with the cursor mask.
			// The Microsoft mouse driver v7.04 and v8.20 uses only BL and CL, so the hot spot row/column should be limited to -128..127.
			// Microsoft KnowledgeBase article Q19850 states that the high bit is right-most,
			// but that statement is contradicted by all other available documentation 
			//
			// Format of mouse mask bitmap:
			//
			// Offset  Size    Description     (Table 03170)
			// 00h 16 WORDs   screen mask
			// 10h 16 WORDs   cursor mask
			// 
			// Note: Each word defines the sixteen pixels of a row, low bit rightmost 
			{
				u16 *src = ((u16 *)PHYS_PTR(REG_ES, DX));
				memcpy( userdefScreenMask, src, CURSORY*2*2);
				mouse.screenMask = userdefScreenMask;
				mouse.hotx		 = (short)BX;
				mouse.hoty		 = (short)CX;
				mouse.cursorType = 2;
				CursorToSprite(mouse.screenMask);
				DrawCursor();
			}
			return 0;
		case 0x000A:	// MS MOUSE v3.0+ - DEFINE TEXT CURSOR
			// BX = hardware/software text cursor
			// 	0000h software
			// 		CX = screen mask
			// 		DX = cursor mask
			// 	0001h hardware
			// 		CX = start scan line
			// 		DX = end scan line
			//
			// Note: When the software cursor is selected, the character/attribute data at the current screen position
			// is ANDed with the screen mask and then XORed with the cursor mask 
			mouse.cursorType = BX;
			mouse.textAndMask = CX;
			mouse.textXorMask = DX;
			return 0;
		case 0x000B:	// MS MOUSE v1.0+ - READ MOTION COUNTERS
			// Return:
			//	CX = number of mickeys mouse moved horizontally since last call
			// 	DX = number of mickeys mouse moved vertically
			//
			// Notes: A mickey is the smallest increment the mouse can sense. Positive values indicate down/right 
			SET_CX((mouse.mickey_x*mouse.mickeysPerPixel_x)>>16);
			SET_DX((mouse.mickey_y*mouse.mickeysPerPixel_y)>>16);
			mouse.mickey_x=0;
			mouse.mickey_y=0;
			return 0;
		case 0x000C:	// MS MOUSE v1.0+ - DEFINE INTERRUPT SUBROUTINE PARAMETERS
			// AX = 000Ch
			// CX = call mask (see #03171)
			// ES:DX -> FAR routine (see #03172)
			//
			// See Also: AX=0018h 
			//
			// Bitfields for mouse call mask:
			//
			// Bit(s)  Description     (Table 03171)
			// 0      call if mouse moves
			// 1      call if left button pressed
			// 2      call if left button released
			// 3      call if right button pressed
			// 4      call if right button released
			// 5      call if middle button pressed (Mouse Systems/Logitech/Genius mouse)
			// 6      call if middle button released (Mouse Systems/Logitech/Genius mouse)
			// 7-15   unused
			//
			// Note: Some versions of the Microsoft documentation incorrectly state that CX bit 0 means call if mouse cursor moves 
			//
			// (Table 03172)
			// Values interrupt routine is called with:.
			// AX = condition mask (same bit assignments as call mask).
			// BX = button state.
			// CX = cursor column.
			// DX = cursor row.
			// SI = horizontal mickey count.
			// DI = vertical mickey count
			//
			// Notes: Some versions of the Microsoft documentation erroneously swap the meanings of SI and DI.
			// In text modes, the row and column will be reported as a multiple of the character cell size, typically 8x8 pixels 
			{
#if 0
				MOUSESTORAGE* ms = (MOUSESTORAGE*)PHYS_PTR(0xF000, 0x1010);
#else
				MOUSESTORAGE* ms = (MOUSESTORAGE*)(BIOS_F000 + 0x1010);
#endif
				ms->sub_seg = REG_ES;
				ms->sub_offs = DX;
				mouse.sub_mask = CX;
			}
		case 0x000E:	// MS MOUSE v1.0+ - LIGHT PEN EMULATION OFF
			return 0;
		case 0x000F:	// MS MOUSE v1.0+ - DEFINE MICKEY/PIXEL RATIO
			// CX = number of mickeys per 8 pixels horizontally (default 8)
			// DX = number of mickeys per 8 pixels vertically (default 16)
			MouseSetMickeyPixelRate(CX, DX);
			return 0;
		case 0x0013:	// MS MOUSE v5.0+ - DEFINE DOUBLE-SPEED THRESHOLD
			// DX = threshold speed in mickeys/second, 0000h = default of 64/second
			// Note: If speed exceeds threshold, the cursor's on-screen motion is doubled 
			return 0;
		case 0x0014:	// MS MOUSE v3.0+ - EXCHANGE INTERRUPT SUBROUTINES
			// CX = call mask (see #03171)
			// ES:DX -> FAR routine
			// Return:
			//	CX = call mask of previous interrupt routine
			// 	ES:DX = FAR address of previous interrupt routine
			{
#if 0
				MOUSESTORAGE* ms = (MOUSESTORAGE*)PHYS_PTR(0xF000, 0x1010);
#else
				MOUSESTORAGE* ms = (MOUSESTORAGE*)(BIOS_F000 + 0x1010);
#endif
				ax = ms->sub_seg;
				ms->sub_seg = REG_ES;
				REG_ES = ax;
				ax = ms->sub_offs;
				ms->sub_offs = DX;
				SET_DX(ax);
				ax = mouse.sub_mask;
				mouse.sub_mask = CX;
				SET_CX(ax);
			}
			return 0;
		case 0x0015:	// MS MOUSE v6.0+ - RETURN DRIVER STORAGE REQUIREMENTS
			// Return:BX = size of buffer needed to store driver state
			SET_BX(sizeof(mouse));
			return 0;
		case 0x0016:	// MS MOUSE v6.0+ - SAVE DRIVER STATE
			// BX = size of buffer (see AX=0015h)
			// ES:DX -> buffer for driver state
			{
				u16 *tgt = ((u16 *)PHYS_PTR(REG_ES, DX));
				memcpy( tgt, &mouse, sizeof(mouse));
			}
			return 0;
		case 0x0017:	// MS MOUSE v6.0+ - RESTORE DRIVER STATE
			// BX = size of buffer (see AX=0015h)
			// ES:DX -> buffer containing saved state
			{
				u16 *src = ((u16 *)PHYS_PTR(REG_ES, DX));
				memcpy( &mouse, src, sizeof(mouse));
			}
			return 0;
		case 0x001A:	// MS MOUSE v6.0+ - SET MOUSE SENSITIVITY
			// BX = horizontal speed 
			// CX = vertical speed
			// DX = double speed threshold (see AX=0013h)
			MouseSetSensitivity(BX, CX, DX);
			return 0;
		case 0x001B:	// MS MOUSE v6.0+ - RETURN MOUSE SENSITIVITY
			// Return:
			//	BX = horizontal speed
			// 	CX = vertical speed
			// 	DX = double speed threshold
			SET_BX(mouse.senv_x_val);
			SET_CX(mouse.senv_y_val);
			SET_DX(mouse.dspeed_val);
			return 0;
		case 0x001C:	// MS MOUSE v6.0+ - SET INTERRUPT RATE
			// BX = rate (see #03176)
			// Notes: Only available on InPort mouse. Values greater than 4 may cause unpredictable driver behavior 
			return 0;
		case 0x001D:	// MS MOUSE v6.0+ - DEFINE DISPLAY PAGE NUMBER
			// BX = display page number
			// Note: The cursor will be displayed on the specified page 
			mouse.page = BX;
			return 0;
		case 0x001E:	// MS MOUSE v6.0+ - RETURN DISPLAY PAGE NUMBER
			// Return:BX = display page number
			SET_BX(mouse.page);
			return 0;
		case 0x001F:	// MS MOUSE v6.0+ - DISABLE MOUSE DRIVER
			// Return:
			//	AX = status
			// 		001Fh successful
			// 		FFFFh unsuccessful
			// 	ES:BX = INT 33 vector before mouse driver was first installed
			REG_ES = 0;
			SET_BX(0);
			return 0;
		case 0x0020:	// MS MOUSE v6.0+ - ENABLE MOUSE DRIVER
			// Return:
			//	AX = status
			// 		0020h successful
			// 		FFFFh unsuccessful
			//
			// Notes: Restores vectors for INT 10h and INT 71h (8086) or INT 74h (286/386)
			// which were removed by function 1Fh. Microsoft's documentation states that no value is returned.
			return 0;
		case 0x0024:	// MS MOUSE v6.26+ - GET SOFTWARE VERSION, MOUSE TYPE, AND IRQ NUMBER
			// BX = 0000h to check for function's existence
			// Return:
			//	AX = FFFFh on error
			// 	otherwise,
			// 	BH = major version
			// 	BL = minor version
			// 	CH = type (1=bus, 2=serial, 3=InPort, 4=PS/2, 5=HP)
			// 	CL = interrupt (0=PS/2, 2=IRQ2, 3=IRQ3,...,7=IRQ7,...,0Fh=IRQ15)
			SET_BX(0x0626);		// Version 6.26
#if 1
			SET_CX(0x0400);		// PS/2 mouse, for Windows support
#else			
			SET_CX(0x0203);		// Serial mouse on IRQ3
#endif			
			return 0;
		case 0x0010:	// MS MOUSE v1.0+ - DEFINE SCREEN REGION FOR UPDATING
		case 0x002D:	// MS MOUSE v7.0+ - SELECT ACCELERATION PROFILE (Virtual Pool)
		case 0x002F:	// MS MOUSE v7.02+ - MOUSE HARDWARE RESET
		case 0x00A0:	// TRUEDOX Mouse driver - SET HARDWARE PC MODE
		case 0x00A1:	// TRUEDOX Mouse driver - SET HARDWARE MS MODE (Knights of Xentar)
		case 0xC00F:	// ?? FX_NEIL
		case 0x53C1:	// DOOM?
			return 0;
	}
	sprintf( BreakReasonText, "int 33 AX=%04X\n", AX );
	BreakReason = BreakReasonText;
	return -1;
}


void InitMouse()
{
	// First hide all sprites
	//oamInit(&oamMain, SpriteMapping_1D_32, false);
	useps2callback = false;
	ps2callbackinit = false;
	CursorToSprite(defaultScreenMask);
	MouseSetSensitivity(50, 50, 50);	// Fixes Fragile Allegiance
}

void MouseDebug()
{
#if 0
	MOUSESTORAGE* ms = (MOUSESTORAGE*)PHYS_PTR(0xF000, 0x1010);
#else
	MOUSESTORAGE* ms = (MOUSESTORAGE*)(BIOS_F000 + 0x1010);
#endif
	LOGI("type=%s, but=%d, X=%d, Y=%d\n", ((mouse.sub_mask & MOUSE_HAS_MOVED) && ms->sub_seg != 0) ? "INT33" : (useps2callback ? "PS2" : "NONE"),
		mouse.buttons, mouse.x>>16, mouse.y>>16);
}

