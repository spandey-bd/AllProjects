//=============================================================================
// register_native.c
//
// This file registers the Java Native Interface C functions to be called from
// the Android Java classes in ax86 project.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2013 Patrick "Pate" Aalto
//	
// Redistribution and use in source or binary form, with or without modifications,
// is NOT permitted without specific prior written permission from the author.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//=============================================================================

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <jni.h>
//#include "glbuffer.h"

#define UNUSED  __attribute__((unused))

#ifndef NELEM
#define NELEM(x) ((int)(sizeof(x) / sizeof((x)[0])))
#endif

void native_start(JNIEnv *env, jclass clazz, jstring fdir, jstring edir);
void native_gl_resize(JNIEnv *env, jclass clazz, jint w, jint h);
void native_gl_render(JNIEnv *env, jclass clazz);

void native_key_click(JNIEnv *env, jclass clazz, jint key);	// In native.c
void native_mouse_event(JNIEnv *env, jclass clazz, jint x, jint y, jint mode);	// In native.c

void native_audio_createEngine(JNIEnv* env, jclass clazz); // In audio.c
void native_audio_createPlayer(JNIEnv* env, jclass clazz); // In audio.c
void native_audio_shutdown(JNIEnv* env, jclass clazz); // In audio.c

static const char *gl_class_path_name = "com/patrickaalto/ax86/GlBufferView";
static JNINativeMethod gl_methods[] = {
	{"native_start", "(Ljava/lang/String;Ljava/lang/String;)V", (void*) native_start},
	{"native_mouse_event", "(III)V", (void*) native_mouse_event},
	{"native_gl_resize", "(II)V", (void*) native_gl_resize},
	{"native_gl_render", "()V", (void*) native_gl_render},
};

static const char *main_class_path_name = "com/patrickaalto/ax86/MainActivity";
static JNINativeMethod main_methods[] = {
	{"native_key_click", "(I)V", (void*) native_key_click},
	{"native_audio_createEngine", "()V", (void *) native_audio_createEngine},
	{"native_audio_createPlayer", "()V", (void *) native_audio_createPlayer},
	{"native_audio_shutdown", "()V", (void*) native_audio_shutdown},
};

static int register_native_methods(JNIEnv* env,
		const char* class_name,
		JNINativeMethod* methods,
		int num_methods)
{
	jclass clazz;

	clazz = (*env)->FindClass(env, class_name);
	if (clazz == NULL) {
		fprintf(stderr, "Native registration unable to find class '%s'\n",
				class_name);
		return JNI_FALSE;
	}
	if ((*env)->RegisterNatives(env, clazz, methods, num_methods) < 0) {
		fprintf(stderr, "RegisterNatives failed for '%s'\n", class_name);
		return JNI_FALSE;
	}

	return JNI_TRUE;
}

static int register_natives(JNIEnv *env)
{
	if (register_native_methods(env, gl_class_path_name, gl_methods, NELEM(gl_methods)) == JNI_FALSE)
		return JNI_FALSE;
	return register_native_methods(env, main_class_path_name, main_methods, NELEM(main_methods));
}

jint JNICALL JNI_OnLoad(JavaVM* vm, void* reserved UNUSED)
{
	JNIEnv* env = NULL;
	jint result = -1;

	if ((*vm)->GetEnv(vm, (void**) &env, JNI_VERSION_1_4) != JNI_OK) {
		fprintf(stderr, "ERROR: GetEnv failed\n");
		goto bail;
	}
	assert(env != NULL);

	if (register_natives(env) < 0) {
		fprintf(stderr, "ERROR: Exif native registration failed\n");
		goto bail;
	}

	/* success -- return valid version number */
	result = JNI_VERSION_1_4;
bail:
	return result;
}
