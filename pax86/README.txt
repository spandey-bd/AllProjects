
This is the readme file for pax86 ARM x86 emulation core by Patrick Aalto.
Here is the directory structure of the pax86 emulation core files:

pax86                 = Location for this README.txt file.
    bios              = x86 ASM sources for the BIOS data, with some utils.
    jni               = Location for the "Android.mk" make file.
        android       = Android-specific sources (libretro test frontend).
        data          = x86 binary data files for pax86. BIOS routines etc.
        include       = Include files for all the sources.
        pax86_VS_proj = Visual C++ 2008 Express project (for source editing only)
        source        = Actual source code files for pax86.
    wp8               = Windows Phone 8 port of pax86
        include       = Include files for all the sources.
        source        = Actual source code files for pax86.


The main directory within "pax86" is called "jni" to make it easy to
use the Android NDK build tools. They expect the native directory to
be called "jni" under the Android project directory. To build the Android
version of pax86:

1) Open Cygwin Terminal, and go to the pax86 directory. On my Windows PC
   this directory is at /cygdrive/c/Projects/pax86
2) Run "ndk-build TARGET_PLATFORM=android-9" in this directory.

If you need to build the x86 BIOS binaries again, this is done as follows:

1) Make sure you have NASM (netwide assembler, www.nasm.us) installed into
   Program Files\NASM. If not, you need to edit the "makebins.bat" to have
   it find the x86 assembler.
2) Open a command prompt and go to pax86/bios directory.
3) Run "makebins.bat". This will compile the .asm files and generate the
   required ARM sources and header files into jni/data and jni/include
   directories.


	
