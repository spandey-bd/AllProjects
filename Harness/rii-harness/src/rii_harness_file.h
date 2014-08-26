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

#ifndef rii_harness_file_h
#define rii_harness_file_h

/**
 Returns the full path for a given data file, relative to the default document directory.

 @param fileName name of the file

 @return Upon success, a pointer to the full path is returned, otherwise NULL

 @warning Caller is responsible for freeing memory on any non-NULL pointer returned.
 */
char *riF_fullPathForFile(const char *fileName);


/**
 Returns the full path for a given data file, relative to the specified group directory.

 @param fileName name of the file
 @param groupName name of the group the file belongs to

 @return Upon success, a pointer to the full path is returned, otherwise NULL

 @warning Caller is responsible for freeing memory on any non-NULL pointer returned.
 */
char *riF_fullPathForFileInGroup(const char *fileName, const char *groupName)

#endif // rii_harness_file_h
