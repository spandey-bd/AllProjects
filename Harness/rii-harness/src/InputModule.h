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


#ifndef __INPUT_MODULE_H__
#define __INPUT_MODULE_H__

#include "cocos2d.h"
#include "VirtualController.h"
#include "gameconfig.h"


USING_NS_CC;


class InputModule {

public:
    InputModule();
    bool init(riM_GameConfig *config);
    virtual ~InputModule();

    /**
    @brief Get the singleton instance
    */
    static InputModule *sharedInputModule();

    CCNode *getVirtualController();

    void touchedAt(float x, float y);
    CCPoint getTouchLocation();

    void setTouched(bool isTouched);
    bool isTouched();

    // JOYPAD / GAME CONTROLLER SUPPORT

    void virtualControllerButtonPressed(uint32_t buttonId);
    void virtualControllerButtonReleased(uint32_t buttonId);
    /**
    * Get the current button press state (0 = not pressed / not available) for the given button on
    * a specific game controller.
    *
    * @param port the joypad index e.g. player 1 is port 0, player 2 is port 1, etc.
    * @param buttonId id of the button to query, using RETRO_DEVICE_ID_JOYPAD_xxx where xxx is e.g. A, B, UP, DOWN, etc.
    */
    int16_t getGameControllerState(unsigned port, unsigned buttonId);


private:
    static InputModule *s_inputModule;

    riM_GameConfig *_gameConfig;

    uint16_t _gameControllerButtonStates[16]; // currently only 1 game controller supported
    VirtualController *_virtualController;

    bool _isTouched;
    float _touchX, _touchY;
};

#endif /* defined(__INPUT_MODULE_H__) */
