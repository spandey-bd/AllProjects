@echo off
REM -----------------------------------------------------------------
REM This Windows batch file will create the x86 code binaries to be
REM included in the pax86 emulation core. This needs NASM (netwide
REM assembler, http://www.nasm.us/) software to have been installed
REM into \Program Files\NASM. Any other x86 assembler should work
REM fine as well. Run it on the command prompt on a Windows PC.
REM -----------------------------------------------------------------
echo Compiling .asm files...
"\Program Files\NASM\nasm.exe" int08.asm -o ..\jni\data\int08.bin
"\Program Files\NASM\nasm.exe" int09.asm -o ..\jni\data\int09.bin
"\Program Files\NASM\nasm.exe" int0b.asm -o ..\jni\data\int0b.bin
"\Program Files\NASM\nasm.exe" int0f.asm -o ..\jni\data\int0f.bin
"\Program Files\NASM\nasm.exe" int11.asm -o ..\jni\data\int11.bin
"\Program Files\NASM\nasm.exe" int12.asm -o ..\jni\data\int12.bin
"\Program Files\NASM\nasm.exe" int16.asm -o ..\jni\data\int16.bin
"\Program Files\NASM\nasm.exe" int21.asm -o ..\jni\data\int21.bin
"\Program Files\NASM\nasm.exe" int67.asm -o ..\jni\data\int67.bin
"\Program Files\NASM\nasm.exe" int74.asm -o ..\jni\data\int74.bin
"\Program Files\NASM\nasm.exe" ScanXlat.asm -o ..\jni\data\ScanXlat.bin
"\Program Files\NASM\nasm.exe" ROM8x8.asm -o ..\jni\data\ROM8x8.bin
"\Program Files\NASM\nasm.exe" ROM8x14.asm -o ..\jni\data\ROM8x14.bin
"\Program Files\NASM\nasm.exe" ROM8x16.asm -o ..\jni\data\ROM8x16.bin
"\Program Files\NASM\nasm.exe" BiosDataInit.asm -o ..\jni\data\BiosDataInit.bin
REM -----------------------------------------------------------------
REM Generate assembler .S files from the binaries, to be linked into
REM pax86 core by the Android tools.
REM -----------------------------------------------------------------
echo Generating .S files...
bin2s ..\jni\data\int08.bin > ..\jni\data\int08.bin.S
bin2s ..\jni\data\int09.bin > ..\jni\data\int09.bin.S
bin2s ..\jni\data\int0b.bin > ..\jni\data\int0b.bin.S
bin2s ..\jni\data\int0f.bin > ..\jni\data\int0f.bin.S
bin2s ..\jni\data\int11.bin > ..\jni\data\int11.bin.S
bin2s ..\jni\data\int12.bin > ..\jni\data\int12.bin.S
bin2s ..\jni\data\int16.bin > ..\jni\data\int16.bin.S
bin2s ..\jni\data\int21.bin > ..\jni\data\int21.bin.S
bin2s ..\jni\data\int67.bin > ..\jni\data\int67.bin.S
bin2s ..\jni\data\int74.bin > ..\jni\data\int74.bin.S
bin2s ..\jni\data\ScanXlat.bin > ..\jni\data\ScanXlat.bin.S
bin2s ..\jni\data\ROM8x8.bin > ..\jni\data\ROM8x8.bin.S
bin2s ..\jni\data\ROM8x14.bin > ..\jni\data\ROM8x14.bin.S
bin2s ..\jni\data\ROM8x16.bin > ..\jni\data\ROM8x16.bin.S
bin2s ..\jni\data\BiosDataInit.bin > ..\jni\data\BiosDataInit.bin.S
REM -----------------------------------------------------------------
REM Generate the .h files for the binaries, to be linked into
REM pax86 core by the Android tools.
REM -----------------------------------------------------------------
echo Generating .h files...
echo extern const u8 int08_bin[]; > ..\jni\include\int08_bin.h
echo extern const u32 int08_bin_size; >> ..\jni\include\int08_bin.h
echo extern const u8 int09_bin[]; > ..\jni\include\int09_bin.h
echo extern const u32 int09_bin_size; >> ..\jni\include\int09_bin.h
echo extern const u8 int0b_bin[]; > ..\jni\include\int0b_bin.h
echo extern const u32 int0b_bin_size; >> ..\jni\include\int0b_bin.h
echo extern const u8 int0f_bin[]; > ..\jni\include\int0f_bin.h
echo extern const u32 int0f_bin_size; >> ..\jni\include\int0f_bin.h
echo extern const u8 int11_bin[]; > ..\jni\include\int11_bin.h
echo extern const u32 int11_bin_size; >> ..\jni\include\int11_bin.h
echo extern const u8 int12_bin[]; > ..\jni\include\int12_bin.h
echo extern const u32 int12_bin_size; >> ..\jni\include\int12_bin.h
echo extern const u8 int16_bin[]; > ..\jni\include\int16_bin.h
echo extern const u32 int16_bin_size; >> ..\jni\include\int16_bin.h
echo extern const u8 int21_bin[]; > ..\jni\include\int21_bin.h
echo extern const u32 int21_bin_size; >> ..\jni\include\int21_bin.h
echo extern const u8 int67_bin[]; > ..\jni\include\int67_bin.h
echo extern const u32 int67_bin_size; >> ..\jni\include\int67_bin.h
echo extern const u8 int74_bin[]; > ..\jni\include\int74_bin.h
echo extern const u32 int74_bin_size; >> ..\jni\include\int74_bin.h
echo extern const u8 ScanXlat_bin[]; > ..\jni\include\ScanXlat_bin.h
echo extern const u32 ScanXlat_bin_size; >> ..\jni\include\ScanXlat_bin.h
echo extern const u8 ROM8x8_bin[]; > ..\jni\include\ROM8x8_bin.h
echo extern const u32 ROM8x8_bin_size; >> ..\jni\include\ROM8x8_bin.h
echo extern const u8 ROM8x14_bin[]; > ..\jni\include\ROM8x14_bin.h
echo extern const u32 ROM8x14_bin_size; >> ..\jni\include\ROM8x14_bin.h
echo extern const u8 ROM8x16_bin[]; > ..\jni\include\ROM8x16_bin.h
echo extern const u32 ROM8x16_bin_size; >> ..\jni\include\ROM8x16_bin.h
echo extern const u8 BiosDataInit_bin[]; > ..\jni\include\BiosDataInit_bin.h
echo extern const u32 BiosDataInit_bin_size; >> ..\jni\include\BiosDataInit_bin.h
