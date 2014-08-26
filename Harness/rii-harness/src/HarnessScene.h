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

#ifndef __HARNESS_SCENE_H__
#define __HARNESS_SCENE_H__

#include "cocos2d.h"
#include "InputModule.h"
#include "gameconfig.h"

USING_NS_CC;

class HarnessScene : public CCScene {
    InputModule *_inputModule;
    riM_GameConfig _gameConfig;

public:
    virtual bool init();
    CREATE_FUNC(HarnessScene);
};

#endif /* defined(__HARNESS_SCENE_H__) */
