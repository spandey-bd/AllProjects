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

#include "LibretroCallbacks.h"
#include "EmulatorLayer.h"
#include "InputModule.h"

#ifndef lroundf
#define lroundf(fp) (long int)((fp) >= 0.0 ? (fp) + 0.5 : (fp) - 0.5)
#endif

typedef struct {
    int16_t joypad_0[16];
    int16_t mouse[4];
    int16_t pointer[3];
} input_states;

input_states currentInputStates;


// LibRetro callbacks
bool retroEnvironmentCallback(unsigned cmd, void *data) {
    EmulatorLayer *harnessLayer = EmulatorLayer::sharedLayer();
    switch(cmd) {
        case RETRO_ENVIRONMENT_GET_SYSTEM_DIRECTORY:
        {
            // for storing system specific ROMs such as BIOSes, config data, etc
            const char *dir = *(const char **)data;
            dir = NULL; // no system directory defined right now
            break;
        }

        case RETRO_ENVIRONMENT_SET_PIXEL_FORMAT:
        {
            enum retro_pixel_format pixelFormat = *(enum retro_pixel_format *)data;
            harnessLayer->setPixelFormat(pixelFormat);
            break;
        }

        case RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK:
        {
            const struct retro_keyboard_callback *callback = (const struct retro_keyboard_callback *)data;
            // TODO pass the callback into the harness
            break;
        }

        case RETRO_ENVIRONMENT_SET_SUPPORT_NO_GAME:
        {
            bool supportsNoGame = *(const bool *)data;
            // TODO pass to harness
            break;
        }
    }
    return true;
}

void retroVideoRefreshCallback(const void *data, unsigned width, unsigned height, size_t pitch) {
    EmulatorLayer *harnessLayer = EmulatorLayer::sharedLayer();
    harnessLayer->getEmulatorNode()->drawPixels(data, width, height, pitch);
}

void retroInputPollCallback(void) {
    InputModule *input = InputModule::sharedInputModule();

    // takes a snapshot of the state of all inputs so that retroInputStateCallback can query without worrying about
    // other threads modifying states

    // make sure state is reset
    memset(&currentInputStates, 0, sizeof(input_states));

    // pointer (touch, not mouse) state
    if (input->isTouched()) {
        currentInputStates.pointer[RETRO_DEVICE_ID_POINTER_PRESSED] = true;
        CCPoint location = input->getTouchLocation();
        currentInputStates.pointer[RETRO_DEVICE_ID_POINTER_X] = (int16_t) lroundf(location.x);
        currentInputStates.pointer[RETRO_DEVICE_ID_POINTER_Y] = (int16_t) lroundf(location.y);
    }

    // joypad in port 0
    for (unsigned int button = 0; button <= 15; button++) {
        currentInputStates.joypad_0[button] = input->getGameControllerState(0, button);
    }
}

int16_t retroInputStateCallback(unsigned port, unsigned device, unsigned index, unsigned id) {
    switch(device) {
        case RETRO_DEVICE_POINTER:
            if (id < 3) {
                return currentInputStates.pointer[id];
            }
            CCLog("Illegal pointer state id: %d", id);
            return 0;

        case RETRO_DEVICE_JOYPAD:
            if (port == 0 && id < 16) {
                return currentInputStates.joypad_0[id];
            }
            CCLog("Illegal joypad state id: %d", id);
            return 0;

        default:
//            CCLog("Unsupported device type: %d port: %d index: %d id: %d", device, port, index, id);
            return 0;
    }
}
