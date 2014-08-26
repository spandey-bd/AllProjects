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

#include "EmulatorPixmapNode.h"
#include "InputModule.h"

#ifndef roundf
#define roundf(fp) (int32_t)((fp) >= 0.0 ? (fp) + 0.5 : (fp) - 0.5)
#endif

EmulatorPixmapNode::EmulatorPixmapNode() : _ccPixelFormat(kCCTexture2DPixelFormat_RGB565) {

}

EmulatorPixmapNode::~EmulatorPixmapNode() {

}

EmulatorPixmapNode *EmulatorPixmapNode::create(struct retro_system_av_info *avInfo, retro_pixel_format pixelFormat) {
    EmulatorPixmapNode *node = new EmulatorPixmapNode();
    if (node && node->initWithRetroSystemAvInfo(avInfo, pixelFormat)) {
        node->autorelease();
        return node;
    }
    CC_SAFE_DELETE(node);
    return NULL;
}

bool EmulatorPixmapNode::initWithRetroSystemAvInfo(struct retro_system_av_info *avInfo, retro_pixel_format pixelFormat) {
    CCLog("Creating emulator pixmap node of size %d x %d with pixel format %d", avInfo->geometry.max_width, avInfo->geometry.max_height, pixelFormat);

    switch(pixelFormat) {
        case RETRO_PIXEL_FORMAT_0RGB1555:
            _ccPixelFormat = kCCTexture2DPixelFormat_RGB5A1;
            break;
        case RETRO_PIXEL_FORMAT_XRGB8888:
            _ccPixelFormat = kCCTexture2DPixelFormat_RGBA8888;
            break;
        case RETRO_PIXEL_FORMAT_RGB565:
            _ccPixelFormat = kCCTexture2DPixelFormat_RGB565;
            break;
        default:
            CCLog("Unknown / unsupported libretro pixel format: %d", pixelFormat);
            return false;
    }

    void *_pixelData = malloc(avInfo->geometry.max_width * avInfo->geometry.max_height * 2);
    if (!_pixelData) {
        CCLog("Could not create memory buffer for pixel data");
        return false;
    }
    memset(_pixelData, 0, avInfo->geometry.max_width * avInfo->geometry.max_height * 2);

    CCTexture2D *texture2D = new CCTexture2D();
    if (texture2D && texture2D->initWithData(_pixelData, _ccPixelFormat, avInfo->geometry.max_width, avInfo->geometry.max_height, CCSizeMake(avInfo->geometry.max_width, avInfo->geometry.max_height))) {

        bool success = CCSprite::initWithTexture(texture2D);
        texture2D->release();
        return success;

    } else {
        CCLog("Could not create pixel buffer for emulator");
        CC_SAFE_DELETE(texture2D);
        return false;
    }
}


void EmulatorPixmapNode::drawPixels(const void *data, unsigned int width, unsigned int height, size_t pitch) {
    CCTexture2D *texture2D = new CCTexture2D();
    if (texture2D &&
        texture2D->initWithData(data, _ccPixelFormat, width, height, CCSizeMake(width, height))) {
        this->setTexture(texture2D);
        texture2D->release();
    } else {
        CC_SAFE_DELETE(texture2D);
    }
}

int16_t scaleNodeSpaceCoord(float coord, float maxCoord, bool invert) {
    coord = invert ? maxCoord - coord : coord;
    float scaled = coord / maxCoord * 65534.0f ; // 65534 is 0x7fff << 1
    int16_t val = (int16_t)roundf(scaled);
    val -= 0x7fff; // translate to be between -0x7fff to 0x7fff
    return val;
}

void EmulatorPixmapNode::touchBegan(CCTouch *touch) {
    this->touchMoved(touch);
}

void EmulatorPixmapNode::touchMoved(CCTouch *touch) {
    CCPoint location = this->convertTouchToNodeSpace(touch);
    if (location.x >= 0 && location.y >= 0 && location.x <= this->getContentSize().width && location.y <= this->getContentSize().height) {
        float pointerX = scaleNodeSpaceCoord(location.x, this->getContentSize().width, false);
        float pointerY = scaleNodeSpaceCoord(location.y, this->getContentSize().height, true); // y coordinates are inverted
        InputModule::sharedInputModule()->touchedAt(pointerX, pointerY);
//        CCLog("Location of touch: %f, %f scaled: %d, %d", location.x, this->getContentSize().height - location.y, pointerX, pointerY);
    } else {
        InputModule::sharedInputModule()->setTouched(false);
    }
}

void EmulatorPixmapNode::touchEnded(CCTouch *touch) {
    InputModule::sharedInputModule()->setTouched(false);
}
