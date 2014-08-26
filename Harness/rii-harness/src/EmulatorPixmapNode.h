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

#ifndef __EMULATOR_PIXMAP_NODE_H__
#define __EMULATOR_PIXMAP_NODE_H__

#include "cocos2d.h"
#include "libretro.h"


USING_NS_CC;

/**
 * A node for rendering the emulator pixmap and associated functions.
 *
 * @author Gabriel Hauber
 */
class EmulatorPixmapNode : public CCSprite {
    CCTexture2DPixelFormat _ccPixelFormat;

public:

    static EmulatorPixmapNode *create(struct retro_system_av_info *avInfo, retro_pixel_format pixelFormat);

    // default constructor
    EmulatorPixmapNode(void);

    // default destructor
    virtual ~EmulatorPixmapNode(void);

    virtual bool initWithRetroSystemAvInfo(struct retro_system_av_info *avInfo, retro_pixel_format pixelFormat);

    void drawPixels(const void *data, unsigned int width, unsigned int height, size_t pitch);

    void touchBegan(CCTouch *touch);
    void touchMoved(CCTouch *touch);
    void touchEnded(CCTouch *touch);
};

#endif /* defined(__EMULATOR_PIXMAP_NODE_H__) */
