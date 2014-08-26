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

#ifndef __VIRTUAL_CONTROLLER_H__
#define __VIRTUAL_CONTROLLER_H__

#include "cocos2d.h"
#include "libretro.h"
#include "gameconfig.h"


USING_NS_CC;

/**
 * Manages display and events for a virtual (on-screen) controller
 *
 * @author Gabriel Hauber
 */
class VirtualController : public CCLayerRGBA {
    CCNode *_dpad, *_buttonA, *_buttonB, *_buttonX, *_buttonY;
    CCAction *_fadeIn, *_fadeOut;

    uint16_t _touchCount;
    bool _isActive;

    void didBecomeInactive();

    void buttonPressed(CCObject *button, uint32_t keycode, uint32_t joypadId);
    void buttonReleased(CCObject *button, uint32_t keycode, uint32_t joypadId);

public:

    static VirtualController *create(riM_GameConfig *config);
    virtual bool init(riM_GameConfig *config);

    virtual void ccTouchesBegan(CCSet *pTouches, CCEvent *pEvent);
    virtual void ccTouchesEnded(CCSet *pTouches, CCEvent *pEvent);
    virtual void ccTouchesCancelled(CCSet *pTouches, CCEvent *pEvent);
};

#endif /* defined(__VIRTUAL_CONTROLLER_H__) */
