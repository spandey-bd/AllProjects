#ifndef __RII_DEMO_H__
#define __RII_DEMO_H__

#include "cocos2d.h"

class RetroInfinity : public cocos2d::CCLayer
{
public:
	RetroInfinity();
	~RetroInfinity();
    // Here's a difference. Method 'init' in cocos2d-x returns bool, instead of returning 'id' in cocos2d-iphone
    virtual bool init();  

    // there's no 'id' in cpp, so we recommend returning the class instance pointer
    static cocos2d::CCScene* scene();
    
    // implement the "static node()" method manually
    CREATE_FUNC(RetroInfinity);

	
	void spriteMoveFinished(cocos2d::CCNode* sender);
	virtual void keyDown(int key);
	virtual void keyUp(int key);

protected:
	cocos2d::CCSprite *_object;
	void ccTouchesEnded(cocos2d::CCSet* touches, cocos2d::CCEvent* event);
};

#endif // __RII_DEMO_H__
