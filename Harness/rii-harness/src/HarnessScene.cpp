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

#include "HarnessScene.h"
#include "EmulatorLayer.h"


bool HarnessScene::init() {
    // super init first
    if (!CCScene::init()) {
        return false;
    }

    // read game configuration or set defaults
    if (!riM_getGameConfig(&_gameConfig)) {
        _gameConfig.button_blue = true;
        _gameConfig.button_green = true;
        _gameConfig.button_red = true;
        _gameConfig.button_yellow = true;
    }

    // todo set up with splash screen first, then transition to emulator once loading/delay is complete

    this->addChild(EmulatorLayer::sharedLayer());

    _inputModule = new InputModule();
    _inputModule->init(&_gameConfig);

    this->addChild(_inputModule->getVirtualController());

    return true;
}
