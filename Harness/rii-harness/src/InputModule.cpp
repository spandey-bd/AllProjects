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


#include "InputModule.h"


InputModule *InputModule::s_inputModule = NULL;

InputModule::InputModule() {
    CC_ASSERT(!s_inputModule);
    s_inputModule = this;
}

bool InputModule::init(riM_GameConfig *config) {
    _virtualController = NULL;
    _gameConfig = config;

    // initialise game controller state
    memset(_gameControllerButtonStates, 0, sizeof(_gameControllerButtonStates));
    return true;
}

InputModule::~InputModule() {
    CC_ASSERT(this == s_inputModule);
    s_inputModule = NULL;
    _virtualController->release();
    _virtualController = NULL;
}

InputModule *InputModule::sharedInputModule() {
    CC_ASSERT(s_inputModule);
    return s_inputModule;
}

CCNode *InputModule::getVirtualController() {
    if (!_virtualController) {
        _virtualController = VirtualController::create(_gameConfig);
        _virtualController->retain();
    }
    return _virtualController;
}

// ==================== TOUCH / POINTER

void InputModule::touchedAt(float x, float y) {
    _touchX = x;
    _touchY = y;
}

CCPoint InputModule::getTouchLocation() {
    return ccp(_touchX, _touchY);
}

void InputModule::setTouched(bool isTouched) {
    _isTouched = isTouched;
}

bool InputModule::isTouched() {
    return _isTouched;
}


// ============================== JOYPAD / GAME CONTROLLER SUPPORT



// At the wrapper level game controller input may be provided via a virtual (on-screen) controller
// if no physical controller is present

void InputModule::virtualControllerButtonPressed(uint32_t buttonId) {
    _gameControllerButtonStates[buttonId] = 1;
}

void InputModule::virtualControllerButtonReleased(uint32_t buttonId) {
    _gameControllerButtonStates[buttonId] = 0;
}

int16_t InputModule::getGameControllerState(unsigned port, unsigned buttonId) {
    // currently only a single port is supported
    if (port != 0) {
        CCLog("Only single game controller supported; TODO: support multiple controllers");
        return 0;
    }

    return _gameControllerButtonStates[buttonId];
}
