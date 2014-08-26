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

#ifndef __VC_BUTTON_H__
#define __VC_BUTTON_H__

#include "cocos2d.h"

USING_NS_CC;

// button callback has 2 paramaters - first is the source button, 2nd is the keycode (if any) to map the button to, otherwise 0
typedef void (CCObject::*SEL_ButtonHandler)(CCObject*,uint32_t,uint32_t);
#define button_handler(_SELECTOR)(SEL_ButtonHandler)(&_SELECTOR)

class VCButton : public CCNodeRGBA, public CCStandardTouchDelegate {
    /** when 0, use standard button mapping (given in _joypadId), otherwise this button press triggers the given keycode */
    uint32_t _keycode;
    uint32_t _joypadId;

    /** whether or not this button is pressed */
    bool _isPressed;

    /** the image used when the item is not selected */
    CC_PROPERTY(CCNode*, _normalImage, NormalImage);
    /** the image used when the item is selected */
    CC_PROPERTY(CCNode*, _pressedImage, PressedImage);

    CCObject *_target;
    SEL_ButtonHandler _pressedHandler, _releasedHandler;

protected:
    void updateImagesVisibility();
    void pressed();
    void released();

public:

    VCButton() : _isPressed(false) , _normalImage(NULL), _pressedImage(NULL) {}
    virtual ~VCButton();

    static VCButton *create(uint32_t keycode, uint32_t joypadId, const char *normalImage, const char *pressedImage, CCObject *target, SEL_ButtonHandler pressedHandler, SEL_ButtonHandler releasedHandler);
    bool init(uint32_t keycode, uint32_t joypadId, const char *normalImage, const char *pressedImage, CCObject *target, SEL_ButtonHandler pressedHandler, SEL_ButtonHandler releasedHandler);

    virtual void onEnter() override;
    virtual void onExit() override;

    virtual bool ccTouchBegan(CCTouch *pTouch, CCEvent *pEvent);
    virtual void ccTouchMoved(CCTouch *pTouch, CCEvent *pEvent);
    virtual void ccTouchEnded(CCTouch *pTouch, CCEvent *pEvent);
    virtual void ccTouchCancelled(CCTouch *pTouch, CCEvent *pEvent);

    bool isTouchInsideBounds(CCTouch *pTouch);
};

#endif /* defined(__VC_BUTTON_H__) */
