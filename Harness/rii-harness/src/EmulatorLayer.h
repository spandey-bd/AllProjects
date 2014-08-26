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

#ifndef __EMULATOR_LAYER_H__
#define __EMULATOR_LAYER_H__

#include "cocos2d.h"
#include "libretro.h"
#include "EmulatorPixmapNode.h"


USING_NS_CC;

class EmulatorLayer : public CCLayer
{
    enum retro_pixel_format _pixelFormat;
    EmulatorPixmapNode *_emulatorPixmapNode;

public:

    // SETUP

    static EmulatorLayer *sharedLayer(void);
    virtual bool init();
    CREATE_FUNC(EmulatorLayer);

    void initializeLibretro();
    void setPixelFormat(enum retro_pixel_format);

    // OPERATION

    virtual void onEnter();
    void update(float dt);

    EmulatorPixmapNode *getEmulatorNode();

    // EVENT HANDLING
    
    virtual void ccTouchesBegan(CCSet *pTouches, CCEvent *pEvent);
    virtual void ccTouchesMoved(CCSet *pTouches, CCEvent *pEvent);
    virtual void ccTouchesEnded(CCSet *pTouches, CCEvent *pEvent);
    virtual void ccTouchesCancelled(CCSet *pTouches, CCEvent *pEvent);
};

#endif // __EMULATOR_LAYER_H__
