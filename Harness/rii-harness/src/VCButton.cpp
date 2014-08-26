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

#include "VCButton.h"

const unsigned int    kNormalTag = 0x1;
const unsigned int    kSelectedTag = 0x2;


VCButton::~VCButton() {

}


VCButton *VCButton::create(uint32_t keycode, uint32_t joypadId, const char *normalImage, const char *pressedImage, CCObject *target, SEL_ButtonHandler pressedHandler, SEL_ButtonHandler releasedHandler) {
    VCButton *button = new VCButton();
    if (button && button->init(keycode, joypadId, normalImage, pressedImage, target, pressedHandler, releasedHandler)) {
        button->autorelease();
        return button;
    }
    CC_SAFE_DELETE(button);
    return NULL;
}


bool VCButton::init(uint32_t keycode, uint32_t joypadId, const char *normalImage, const char *pressedImage, CCObject *target, SEL_ButtonHandler pressedHandler, SEL_ButtonHandler releasedHandler) {
    CCNodeRGBA::init();

    setAnchorPoint(ccp(0.5f, 0.5f));

    _keycode = keycode;
    _joypadId = joypadId;
    _target = target;
    _pressedHandler = pressedHandler;
    _releasedHandler = releasedHandler;

    _isPressed = false;

    if (normalImage) {
        setNormalImage(CCSprite::create(normalImage));
    }

    if (pressedImage) {
        setPressedImage(CCSprite::create(pressedImage));
    }

    if (_normalImage) {
        this->setContentSize(_normalImage->getContentSize());

        setCascadeColorEnabled(true);
        setCascadeOpacityEnabled(true);
    }

    return true;
}


void VCButton::setNormalImage(CCNode *normalImage) {
    if (_normalImage != normalImage) {
        if (normalImage) {
            addChild(normalImage, 0, kNormalTag);
            normalImage->setAnchorPoint(ccp(0, 0));
        }

        if (_normalImage) {
            removeChild(_normalImage, true);
        }

        _normalImage = normalImage;
        this->updateImagesVisibility();
    }
}

CCNode *VCButton::getNormalImage() {
    return _normalImage;
}

void VCButton::setPressedImage(CCNode *pressedImage) {
    if (_pressedImage != pressedImage) {
        if (pressedImage) {
            addChild(pressedImage, 0, kNormalTag);
            pressedImage->setAnchorPoint(ccp(0, 0));
        }

        if (_pressedImage) {
            removeChild(_pressedImage, true);
        }

        _pressedImage = pressedImage;
        this->updateImagesVisibility();
    }
}

CCNode *VCButton::getPressedImage() {
    return _pressedImage;
}


void VCButton::updateImagesVisibility() {
    if (_normalImage) _normalImage->setVisible(!_isPressed);
    if (_pressedImage) _pressedImage->setVisible(_isPressed);
}

void VCButton::pressed() {
    _isPressed = true;
    updateImagesVisibility();
    if (_target && _pressedHandler) {
        (_target->*_pressedHandler)(this, _keycode, _joypadId);
    }
}

void VCButton::released() {
    _isPressed = false;
    updateImagesVisibility();
    if (_target && _releasedHandler) {
        (_target->*_releasedHandler)(this, _keycode, _joypadId);
    }
}

bool VCButton::isTouchInsideBounds(CCTouch *pTouch) {
    CCPoint touchPoint = convertTouchToNodeSpace(pTouch);
    CCSize nodeSize = getContentSize();
    return (touchPoint.x >= 0 && touchPoint.y >= 0 && touchPoint.x <= nodeSize.width && touchPoint.y <= nodeSize.height);
}

bool VCButton::ccTouchBegan(CCTouch *pTouch, CCEvent *pEvent) {
    CC_UNUSED_PARAM(pEvent);
    if (isTouchInsideBounds(pTouch)) {
        pressed();
    }
    return true;
}

void VCButton::ccTouchMoved(CCTouch *pTouch, CCEvent *pEvent) {
    CC_UNUSED_PARAM(pEvent);
    bool isTouchInside = isTouchInsideBounds(pTouch);
    if (_isPressed && !isTouchInside) {
        released();
    } else if (!_isPressed && isTouchInside) {
        pressed();
    }
}

void VCButton::ccTouchEnded(CCTouch *pTouch, CCEvent *pEvent) {
    CC_UNUSED_PARAM(pTouch); CC_UNUSED_PARAM(pEvent);
    if (_isPressed) {
        released();
    }
}

void VCButton::ccTouchCancelled(CCTouch *pTouch, CCEvent *pEvent) {
    CC_UNUSED_PARAM(pTouch); CC_UNUSED_PARAM(pEvent);
    _isPressed = false;
    updateImagesVisibility();
}

void VCButton::onEnter() {
    CCNode::onEnter();
    CCDirector::sharedDirector()->getTouchDispatcher()->addTargetedDelegate(this, 0, false);
}

void VCButton::onExit() {
    CCNode::onExit();
    CCDirector::sharedDirector()->getTouchDispatcher()->removeDelegate(this);
}
