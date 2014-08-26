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

#include "cocos2d.h"
#import "IOSHarnessAppDelegate.h"
#import "AppDelegate.h"
#import "IOSHarnessViewController.h"
#import "EAGLView.h"


@implementation IOSHarnessAppDelegate


#pragma mark - Application lifecycle

// cocos2d application instance
static AppDelegate s_sharedApplication;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    
    // Add the view controller's view to the window and display.
    self.window = [[UIWindow alloc] initWithFrame: [[UIScreen mainScreen] bounds]];

    // Init the EAGLView
    EAGLView *glView = [EAGLView viewWithFrame: self.window.bounds
                                   pixelFormat: kEAGLColorFormatRGB565
                                   depthFormat: GL_DEPTH24_STENCIL8_OES
                            preserveBackbuffer: NO
                                    sharegroup: nil
                                 multiSampling: NO
                               numberOfSamples: 0];
    glView.multipleTouchEnabled = YES;

    // Use IOSHarnessViewController manage EAGLView
    IOSHarnessViewController *viewController = [[IOSHarnessViewController alloc] initWithNibName:nil bundle:nil];
    viewController.view = glView;

    // set up graphics resources search paths; this is heavily dependent on device type
    // which is why it's here and not in the cross-platform AppDelegate.cpp
    std::vector<std::string> searchPath;
    if ([UIScreen mainScreen].scale == 1.0) {
        // standard-def iPad
        searchPath.push_back("media/sd");
    } else {
        // retina iPhone, iPad
        searchPath.push_back("media/hd");
    }
    cocos2d::CCFileUtils::sharedFileUtils()->setSearchPaths(searchPath);

    self.window.rootViewController = viewController;
    [self.window makeKeyAndVisible];
    
    cocos2d::CCApplication::sharedApplication()->run();

    return YES;
}


- (void)applicationWillResignActive:(UIApplication *)application {
    /*
     Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
     Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
     */
    cocos2d::CCDirector::sharedDirector()->pause();
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
    /*
     Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
     */
    cocos2d::CCDirector::sharedDirector()->resume();
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
    /*
     Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
     If your application supports background execution, called instead of applicationWillTerminate: when the user quits.
     */
    cocos2d::CCApplication::sharedApplication()->applicationDidEnterBackground();
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
    /*
     Called as part of  transition from the background to the inactive state: here you can undo many of the changes made on entering the background.
     */
    cocos2d::CCApplication::sharedApplication()->applicationWillEnterForeground();
}

- (void)applicationWillTerminate:(UIApplication *)application {
    /*
     Called when the application is about to terminate.
     See also applicationDidEnterBackground:.
     */
}


#pragma mark -
#pragma mark Memory management

- (void)applicationDidReceiveMemoryWarning:(UIApplication *)application {
    /*
     Free up as much memory as possible by purging cached data objects that can be recreated (or reloaded from disk) later.
     */
}


@end
