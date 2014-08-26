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

#include "VirtualController.h"
#include "VCButton.h"
#include "InputModule.h"

#define MIN_OPACITY 64

// 255 would be completely solid, but we want our buttons to be slightly transparent always?
#define MAX_OPACITY 223

// tags to identify various actions
#define ACTION_TAG_FADE_IN 1
#define ACTION_TAG_FADE_OUT 2


VirtualController *VirtualController::create(riM_GameConfig *config) {
    VirtualController *virtualController = new VirtualController();
    if (virtualController && virtualController->init(config)) {
        virtualController->autorelease();
        return virtualController;
    }
    CC_SAFE_DELETE(virtualController);
    return NULL;
}

bool VirtualController::init(riM_GameConfig *config) {
    if (!CCLayer::init()) {
        return false;
    }

    setTouchPriority(kCCMenuHandlerPriority);
    setTouchMode(kCCTouchesAllAtOnce);
    setTouchEnabled(true);

    setCascadeColorEnabled(true);
    setCascadeOpacityEnabled(true);

    CCSize layerSize = this->getContentSize();

    _dpad = CCSprite::create("DPad.png");
    CCSize size = _dpad->getContentSize();
    _dpad->setPosition(ccp(size.width * .75, size.height * .75));
    this->addChild(_dpad);

    CCPoint buttonsCentre = ccp(layerSize.width - size.width * .75, size.height * .75);

    if (config->button_red) {
        _buttonA = VCButton::create(config->button_red_keycode, RETRO_DEVICE_ID_JOYPAD_A, "A.png", "A_pressed.png", this, button_handler(VirtualController::buttonPressed), button_handler(VirtualController::buttonReleased));
        _buttonA->setPosition(ccp(buttonsCentre.x, buttonsCentre.y - _buttonA->getContentSize().height));
        this->addChild(_buttonA);
    }

    if (config->button_green) {
        _buttonB = VCButton::create(config->button_green_keycode, RETRO_DEVICE_ID_JOYPAD_B, "B.png", "B_pressed.png", this, button_handler(VirtualController::buttonPressed), button_handler(VirtualController::buttonReleased));
        _buttonB->setPosition(ccp(buttonsCentre.x + _buttonB->getContentSize().width, buttonsCentre.y));
        this->addChild(_buttonB);
    }

    if (config->button_yellow) {
        _buttonX = VCButton::create(config->button_yellow_keycode, RETRO_DEVICE_ID_JOYPAD_X, "X.png", "X_pressed.png", this, button_handler(VirtualController::buttonPressed), button_handler(VirtualController::buttonReleased));
        _buttonX->setPosition(ccp(buttonsCentre.x - _buttonX->getContentSize().width, buttonsCentre.y));
        this->addChild(_buttonX);
    }

    if (config->button_blue) {
        _buttonY = VCButton::create(config->button_blue_keycode, RETRO_DEVICE_ID_JOYPAD_Y, "Y.png", "Y_pressed.png", this, button_handler(VirtualController::buttonPressed), button_handler(VirtualController::buttonReleased));
        _buttonY->setPosition(ccp(buttonsCentre.x, buttonsCentre.y + _buttonY->getContentSize().height));
        this->addChild(_buttonY);
    }

    _fadeIn = CCFadeTo::create(0.1f, MAX_OPACITY);
    _fadeIn->setTag(ACTION_TAG_FADE_IN);
    _fadeIn->retain();

    _fadeOut = CCSequence::create(
            CCDelayTime::create(5.0),
            CCCallFunc::create(this, callfunc_selector(VirtualController::didBecomeInactive)),
            CCEaseInOut::create(CCFadeTo::create(2.0, MIN_OPACITY), 2),
            NULL
    );
    _fadeOut->setTag(ACTION_TAG_FADE_OUT);
    _fadeOut->retain();

    this->setOpacity(MIN_OPACITY); // start out in faded state
    return true;
}

void VirtualController::didBecomeInactive() {
    _isActive = false;
}


void VirtualController::buttonPressed(CCObject *button, uint32_t keycode, uint32_t joypadId) {
    CC_UNUSED_PARAM(button);
    if (keycode == 0) {
        InputModule::sharedInputModule()->virtualControllerButtonPressed(joypadId);
    } else {
        // TODO implement me
    }
}

void VirtualController::buttonReleased(CCObject *button, uint32_t keycode, uint32_t joypadId) {
    CC_UNUSED_PARAM(button);
    if (keycode == 0) {
        InputModule::sharedInputModule()->virtualControllerButtonReleased(joypadId);
    } else {
        // TODO implement me
    }
}

void VirtualController::ccTouchesBegan(CCSet *pTouches, CCEvent *pEvent) {
    if (_touchCount == 0) {
        this->stopActionByTag(_fadeOut->getTag());
        if (!_isActive) {
            this->runAction(_fadeIn);
            _isActive = true;
        }
    }
    _touchCount += pTouches->count();
}

void VirtualController::ccTouchesEnded(CCSet *pTouches, CCEvent *pEvent) {
    _touchCount -= pTouches->count();
    if (_touchCount == 0) {
        this->runAction(_fadeOut);
    }
}

void VirtualController::ccTouchesCancelled(CCSet *pTouches, CCEvent *pEvent) {
    this->ccTouchesEnded(pTouches, pEvent);
}
