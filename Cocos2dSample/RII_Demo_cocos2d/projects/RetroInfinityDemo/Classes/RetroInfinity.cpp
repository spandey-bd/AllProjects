#include "RetroInfinity.h"

USING_NS_CC;

RetroInfinity::~RetroInfinity()
{
	if(_object)
	{
		_object->release();
		_object = NULL;
	}
	// cpp don't need to call super dealloc
	// virtual destructor will do this
}

RetroInfinity::RetroInfinity()
:_object(NULL)
{
}

CCScene* RetroInfinity::scene()
{
    // 'scene' is an autorelease object
    CCScene *scene = CCScene::create();
    
    // 'layer' is an autorelease object
    RetroInfinity *layer = RetroInfinity::create();

    // add layer as a child to scene
    scene->addChild(layer);

    // return the scene
    return scene;
}

// on "init" you need to initialize your instance
bool RetroInfinity::init()
{
    //////////////////////////////
    // 1. super init first
    if ( !CCLayer::init() )
    {
        return false;
    }
    
    CCSize visibleSize = CCDirector::sharedDirector()->getVisibleSize();
    CCPoint origin = CCDirector::sharedDirector()->getVisibleOrigin();

    /////////////////////////////
    // 2. add your codes below...

    _object = CCSprite::create("ball.png", CCRectMake(0, 0, 40, 40) );
        
	_object->setPosition( ccp(origin.x + visibleSize.width/2,
                                origin.y + visibleSize.height/2) );	
	this->addChild(_object);

	this->setTouchEnabled(true);
	this->setKeypadEnabled(true);
	CCDirector::sharedDirector()->getKeypadDispatcher()->addDelegate(this);
    
    return true;
}

void RetroInfinity::ccTouchesEnded(CCSet* touches, CCEvent* event)
{
	// Choose one of the touches to work with
	CCTouch* touch = (CCTouch*)( touches->anyObject() );
	CCPoint location = touch->getLocation();

	// Determinie offset of location to object
	float offX = location.x - _object->getPosition().x;
	float offY = location.y - _object->getPosition().y;

	// Determine the duration of how movement
	float length = sqrtf((offX * offX) + (offY*offY));
	float velocity = 600/1; // 480pixels/1sec
	float realMoveDuration = length/velocity;
	
	//Determinte the size of object
	//_object->getContentSize();

	CCPoint realDest = ccp(offX, offY);

	_object->runAction( CCSequence::create(
		CCMoveBy::create(realMoveDuration, realDest),
		CCCallFuncN::create(this, 
                            callfuncN_selector(RetroInfinity::spriteMoveFinished)), 
        NULL) );	
}


void RetroInfinity::spriteMoveFinished(CCNode* sender)
{
	
}

void RetroInfinity::keyDown(int key) 
{
	int x;
	x=0;
}

void RetroInfinity::keyUp(int key) 
{
	int x;
	x=0;
}
