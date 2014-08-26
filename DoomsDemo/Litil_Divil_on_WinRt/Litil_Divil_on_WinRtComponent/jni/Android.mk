LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_CFLAGS += -mfpu=vfpv3-d16
LOCAL_LDLIBS 	:= -llog -lGLESv1_CM -lOpenSLES
LOCAL_MODULE    := pax86

LOCAL_C_INCLUDES     := $(LOCAL_PATH)/include

LOCAL_SRC_FILES := source/pax86retro.c

LOCAL_SRC_FILES += android/register_natives.c android/audio.c android/native.c

LOCAL_SRC_FILES += source/adlib.S.arm
LOCAL_SRC_FILES += source/sb.S.arm
LOCAL_SRC_FILES += source/fn_tab.S
LOCAL_SRC_FILES += source/sin_tab.S
LOCAL_SRC_FILES += source/tl_tab.S

LOCAL_SRC_FILES += source/cpu_0F.S.arm
LOCAL_SRC_FILES += source/cpu_0F_USE32.S.arm
LOCAL_SRC_FILES += source/cpu_0F_66.S.arm
LOCAL_SRC_FILES += source/cpu_0F_67.S.arm
LOCAL_SRC_FILES += source/cpu_66.S.arm
LOCAL_SRC_FILES += source/cpu_67.S.arm
LOCAL_SRC_FILES += source/cpu_386.S.arm
LOCAL_SRC_FILES += source/cpu_prot.S.arm
LOCAL_SRC_FILES += source/cpu_SIB.S.arm
LOCAL_SRC_FILES += source/cpu_string.S.arm
LOCAL_SRC_FILES += source/cpu.S.arm
LOCAL_SRC_FILES += source/fpu.S.arm
LOCAL_SRC_FILES += source/pic.S.arm

LOCAL_SRC_FILES += source/ports.S.arm
LOCAL_SRC_FILES += source/ports_SB.S.arm
LOCAL_SRC_FILES += source/mouse.c
LOCAL_SRC_FILES += source/timer.c.arm

LOCAL_SRC_FILES += source/CGA.S.arm
LOCAL_SRC_FILES += source/EGA.S.arm
LOCAL_SRC_FILES += source/MCGA.S.arm
LOCAL_SRC_FILES += source/MODEX.S.arm
LOCAL_SRC_FILES += source/TEXT.S.arm
LOCAL_SRC_FILES += source/int10h.c

LOCAL_SRC_FILES += source/ints.c
LOCAL_SRC_FILES += source/BIOS_init.c
LOCAL_SRC_FILES += source/XMS.c
LOCAL_SRC_FILES += source/cdrom.c

LOCAL_SRC_FILES += source/DOS_mem.c
LOCAL_SRC_FILES += source/DOS_device.c
LOCAL_SRC_FILES += source/DOS_file.c
LOCAL_SRC_FILES += source/DOS_task.c
LOCAL_SRC_FILES += source/DOS_kernel.c
LOCAL_SRC_FILES += source/DOS_fcb.c
LOCAL_SRC_FILES += source/int21h.c
LOCAL_SRC_FILES += source/int67h.c

LOCAL_SRC_FILES += data/int08.bin.S
LOCAL_SRC_FILES += data/int09.bin.S
LOCAL_SRC_FILES += data/int0b.bin.S
LOCAL_SRC_FILES += data/int0f.bin.S
LOCAL_SRC_FILES += data/int11.bin.S
LOCAL_SRC_FILES += data/int12.bin.S
LOCAL_SRC_FILES += data/int16.bin.S
LOCAL_SRC_FILES += data/int21.bin.S
LOCAL_SRC_FILES += data/int67.bin.S
LOCAL_SRC_FILES += data/int74.bin.S
LOCAL_SRC_FILES += data/ScanXlat.bin.S
LOCAL_SRC_FILES += data/ROM8x8.bin.S
LOCAL_SRC_FILES += data/ROM8x14.bin.S
LOCAL_SRC_FILES += data/ROM8x16.bin.S
LOCAL_SRC_FILES += data/BiosDataInit.bin.S


include $(BUILD_SHARED_LIBRARY)