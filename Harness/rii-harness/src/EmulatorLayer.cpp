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

#include "EmulatorLayer.h"
#include "support/ccUtils.h"
#include "LibretroCallbacks.h"


// ============================== SETUP

// singleton stuff
static EmulatorLayer *s_sharedEmulatorLayer = NULL;

EmulatorLayer *EmulatorLayer::sharedLayer(void) {
    if (!s_sharedEmulatorLayer) {
        s_sharedEmulatorLayer = EmulatorLayer::create();
        
        // set up our libretro support
        s_sharedEmulatorLayer->initializeLibretro();
    }
    return s_sharedEmulatorLayer;
}

// on "init" you need to initialize your instance
bool EmulatorLayer::init() {
    //////////////////////////////
    // 1. super init first
    if ( !CCLayer::init() ) {
        return false;
    }

    CCLOG("Initializing emulator layer");

    // we want to listen for touches
    this->setTouchEnabled(true);

    return true;
}

void EmulatorLayer::initializeLibretro() {
    struct retro_system_info info;
    retro_get_system_info(&info);
    CCLog("Initializing libretro core: %s version: %s", info.library_name, info.library_version);

    // setup the callbacks, etc
    retro_set_environment(&retroEnvironmentCallback);

    // further libretro setup
    retro_set_video_refresh(&retroVideoRefreshCallback);
    retro_set_audio_sample(NULL);
    retro_set_audio_sample_batch(NULL);
    retro_set_input_poll(&retroInputPollCallback);
    retro_set_input_state(&retroInputStateCallback);

    // ready to call init on the libretro core
    retro_init();

    if (retro_load_game(NULL)) {
        struct retro_system_av_info avInfo;
        retro_get_system_av_info(&avInfo);

        _emulatorPixmapNode = EmulatorPixmapNode::create(&avInfo, _pixelFormat);
        if (_emulatorPixmapNode) {
            // we want to centre the libretro view on the screen and scale it (honoring aspect ratio) to fill the screen
            // note the anchor is the centre of the view
            CCSize parentSize = this->getContentSize();
            CCLog("Size of parent viewport is %f x %f", parentSize.width, parentSize.height);
            _emulatorPixmapNode->setPosition(ccp(parentSize.width / 2, parentSize.height / 2));
            float scaleX = parentSize.width / _emulatorPixmapNode->getContentSize().width;
            float scaleY = parentSize.height / _emulatorPixmapNode->getContentSize().height;
            _emulatorPixmapNode->setScale(MIN(scaleX, scaleY));

            this->addChild(_emulatorPixmapNode);
        }
    }
}

void EmulatorLayer::setPixelFormat(enum retro_pixel_format pixelFormat) {
    _pixelFormat = pixelFormat;
}


// ============================== OPERATION

// event callback when this layer enters the "stage"
void EmulatorLayer::onEnter() {
    CCLayer::onEnter();
    this->schedule(schedule_selector(EmulatorLayer::update));
}

void EmulatorLayer::update(float dt) {
    // now that everything is set up, can run the next frame of the emulator core
    retro_run();
}

EmulatorPixmapNode *EmulatorLayer::getEmulatorNode() {
    return _emulatorPixmapNode;
}


// ============================== EVENT HANDLING

// in testing on iOS, it seems 5 is the maximum touches tracked by Cocos2d, even though iOS is capable of tracking more
// (up to 11) however, 4-finger touches are reserved for multitasking gestures on iOS so we should avoid defining any
// functionality using more than 3 fingers

void EmulatorLayer::ccTouchesBegan(CCSet *pTouches, CCEvent *pEvent) {
    for (CCSetIterator iterator = pTouches->begin(); iterator != pTouches->end(); iterator++) {
        CCTouch *touch = (CCTouch *)*iterator;
        if (touch->getID() == 0) { // TODO check within bounds of emulator pixmap
            // track first touch for emulator
            _emulatorPixmapNode->touchBegan(touch);
        }
    }
}

void EmulatorLayer::ccTouchesMoved(CCSet *pTouches, CCEvent *pEvent) {
    for (CCSetIterator iterator = pTouches->begin(); iterator != pTouches->end(); iterator++) {
        CCTouch *touch = (CCTouch *)*iterator;
        if (touch->getID() == 0) {
            _emulatorPixmapNode->touchMoved(touch);
        }
    }
}

void EmulatorLayer::ccTouchesEnded(CCSet *pTouches, CCEvent *pEvent) {
    for (CCSetIterator iterator = pTouches->begin(); iterator != pTouches->end(); iterator++) {
        CCTouch *touch = (CCTouch *)*iterator;
        if (touch->getID() == 0) {
            _emulatorPixmapNode->touchEnded(touch);
        }
    }
}

void EmulatorLayer::ccTouchesCancelled(CCSet *pTouches, CCEvent *pEvent) {
    // treat the same as touches ended
    this->ccTouchesEnded(pTouches, pEvent);
}


