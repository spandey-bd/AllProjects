BASE_DEV=/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.3.sdk
AR = /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/ar 
#CC = /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-gcc-4.2.1 
CC = /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-llvm-gcc-4.2 
AS = /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-gcc-4.2.1 
LD = /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-llvm-gcc-4.2 

SC = ../jni/source
IC = ../jni/include

BD = ../jni/data

OA = ./obj
OB = ./data

AOBJS = $(OA)/adlib.o $(OA)/CGA.o $(OA)/cpu.o $(OA)/cpu_0F.o \
        $(OA)/cpu_0F_66.o $(OA)/cpu_0F_67.o $(OA)/cpu_0F_USE32.o \
        $(OA)/cpu_66.o $(OA)/cpu_67.o $(OA)/cpu_386.o $(OA)/cpu_prot.o \
        $(OA)/cpu_SIB.o $(OA)/cpu_string.o $(OA)/EGA.o $(OA)/fn_tab.o \
        $(OA)/MCGA.o $(OA)/MODEX.o $(OA)/pic.o $(OA)/ports.o $(OA)/ports_SB.o \
        $(OA)/sb.o $(OA)/sin_tab.o $(OA)/TEXT.o $(OA)/tl_tab.o $(OA)/fpu.o
DOBJS = $(OB)/BiosDataInit.o $(OB)/int0b.o $(OB)/int0f.o $(OB)/int08.o \
        $(OB)/int09.o $(OB)/int11.o $(OB)/int12.o $(OB)/int16.o $(OB)/int21.o \
        $(OB)/int67.o $(OB)/int74.o $(OB)/ROM8x8.o $(OB)/ROM8x14.o \
        $(OB)/ROM8x16.o $(OB)/ScanXlat.o
COBJS = $(OA)/BIOS_init.o $(OA)/DOS_device.o $(OA)/DOS_fcb.o $(OA)/DOS_file.o \
        $(OA)/DOS_kernel.o $(OA)/DOS_mem.o $(OA)/DOS_task.o $(OA)/int10h.o \
        $(OA)/int21h.o $(OA)/int67h.o $(OA)/ints.o $(OA)/mouse.o $(OA)/XMS.o \
        $(OA)/timer.o $(OA)/cdrom.o

# This is for the retro-API:
COBJS += $(OA)/pax86retro.o

# The following line is for running the pax86 unit tests:
#COBJS += ./debug_disasm.o ./tester.o

CCOPTS = -I$(IC) -DIOS -fomit-frame-pointer \
	-I/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.3.sdk/usr/include 

LDFLAGS = -lpthread -lm -lc -L$(BASE_DEV)/usr/lib -isysroot $(BASE_DEV)

libpax86.a : $(AOBJS) $(DOBJS) $(COBJS)
	$(AR) -r libpax86.a $(AOBJS) $(DOBJS) $(COBJS)

pax86test : $(AOBJS) $(DOBJS) $(COBJS)
	$(LD) $(LDFLAGS) $(AOBJS) $(DOBJS) $(COBJS) -o $@

$(OA)/%.o: $(SC)/%.c
	@echo Compiling $<...
	@$(CC) $(CCOPTS) -c $< -o $@

$(OA)/%.o: $(SC)/%.S
	@echo Preprocessing $<...
	@perl fix_asm_ios.pl $(CC) $(CCOPTS) -E $< > $@.S
	@echo Compiling $<...
	@$(CC) $(CCOPTS) -march=armv7-a -c $@.S -o $@

$(OB)/%.o: $(BD)/%.bin.S
	@echo Preprocessing $<...
	@perl fix_asm_ios.pl $(CC) $(CCOPTS) -E $< > $@.S
	@echo Compiling $<...
	@$(CC) $(CCOPTS) -march=armv7-a -c $@.S -o $@

