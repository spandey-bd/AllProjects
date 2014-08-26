
SA = .\source
IA = .\include

SC = .\jni\source
IC = .\jni\include

BD = .\jni\data

OA = .\obj
OB = .\data

AOBJS = $(OA)\adlib.obj $(OA)\CGA.obj $(OA)\cpu.obj $(OA)\cpu_0F.obj \
	$(OA)\cpu_0F_66.obj $(OA)\cpu_0F_67.obj $(OA)\cpu_0F_USE32.obj \
	$(OA)\cpu_66.obj $(OA)\cpu_67.obj $(OA)\cpu_386.obj $(OA)\cpu_prot.obj \
	$(OA)\cpu_SIB.obj $(OA)\cpu_string.obj $(OA)\EGA.obj $(OA)\fn_tab.obj \
	$(OA)\MCGA.obj $(OA)\MODEX.obj $(OA)\pic.obj $(OA)\ports.obj $(OA)\ports_SB.obj \
	$(OA)\sb.obj $(OA)\sin_tab.obj $(OA)\TEXT.obj $(OA)\tl_tab.obj $(OA)\fpu.obj
DOBJS = $(OB)\BiosDataInit.obj $(OB)\int0b.obj $(OB)\int0f.obj $(OB)\int08.obj \
	$(OB)\int09.obj $(OB)\int11.obj $(OB)\int12.obj $(OB)\int16.obj $(OB)\int21.obj \
	$(OB)\int67.obj $(OB)\int74.obj $(OB)\ROM8x8.obj $(OB)\ROM8x14.obj \
	$(OB)\ROM8x16.obj $(OB)\ScanXlat.obj
COBJS = $(OA)\BIOS_init.obj $(OA)\DOS_device.obj $(OA)\DOS_fcb.obj $(OA)\DOS_file.obj \
	$(OA)\DOS_kernel.obj $(OA)\DOS_mem.obj $(OA)\DOS_task.obj $(OA)\int10h.obj \
	$(OA)\int21h.obj $(OA)\int67h.obj $(OA)\ints.obj $(OA)\mouse.obj $(OA)\XMS.obj \
	$(OA)\timer.obj $(OA)\cdrom.obj

# This is for the retro-API:
COBJS = $(COBJS) $(OA)\pax86retro.obj $(OA)\CubeRenderer.obj $(OA)\Direct3DBase.obj $(OA)\Direct3DContentProvider.obj $(OA)\Litil_Divil_on_WinRtComponent.obj

# The following line is for running the pax86 unit tests:
#COBJS = $(COBJS) .\debug_disasm.obj .\tester.obj

CCOPTS = /GS /I$(IC) /wd"4103" /analyze- /W1 /Zc:wchar_t /Zi \
	/fp:precise /D "_LIB" /D "WINAPI_FAMILY=WINAPI_FAMILY_PHONE_APP" \
	/D "WP8" /errorReport:prompt /WX- /Zc:forScope /O2 /Gd /Oy- \
	/MDd /EHsc /nologo

.SUFFIXES : .bin

pax86.lib : $(AOBJS) $(DOBJS) $(COBJS)
	LIB /OUT:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.lib" /MANIFEST:NO /NXCOMPAT /PDB:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.pdb" /DYNAMICBASE "d3d11.lib" "WindowsPhoneCore.lib" "RuntimeObject.lib" "PhoneAppModelHost.lib" "pax86.lib" /DEBUG /DLL /MACHINE:ARM /NODEFAULTLIB:"ole32.lib" /WINMD /INCREMENTAL /PGD:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.pgd" /WINMDFILE:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\PhoneDirect3DXamlAppComponent.winmd" /SUBSYSTEM:CONSOLE /MANIFESTUAC:NO /ManifestFile:"Debug\Litil_Divil_on_WinRtComponent.dll.intermediate.manifest" /ERRORREPORT:PROMPT /NOLOGO /LIBPATH:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\pax86" /TLBID:1 $(AOBJS) $(DOBJS) $(COBJS)

pax86.dll : $(AOBJS) $(DOBJS) $(COBJS)
	LINK /DLL /OUT:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.dll" /MANIFEST:NO /NXCOMPAT /PDB:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.pdb" /DYNAMICBASE "d3d11.lib" "WindowsPhoneCore.lib" "RuntimeObject.lib" "PhoneAppModelHost.lib" "pax86.lib" /DEBUG /DLL /MACHINE:ARM /NODEFAULTLIB:"ole32.lib" /WINMD /PGD:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\Litil_Divil_on_WinRtComponent.pgd" /WINMDFILE:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\Debug\Litil_Divil_on_WinRtComponent\PhoneDirect3DXamlAppComponent.winmd" /SUBSYSTEM:CONSOLE /MANIFESTUAC:NO /ManifestFile:"Debug\Litil_Divil_on_WinRtComponent.dll.intermediate.manifest" /ERRORREPORT:PROMPT /NOLOGO /LIBPATH:"C:\Users\Sudheesh.Nair\Desktop\Litil_Divil_on_WinRt\pax86" /TLBID:1 $(AOBJS) $(DOBJS) $(COBJS)


{$(SC)\}.c{$(OA)\}.obj:
	cl -c $(SC)\$(*B).c /Fo"$@" $(CCOPTS)

{$(SA)\}.asm{$(OA)\}.obj: 
	armasm $(SA)\$(*B).asm -o $@ -ignore 4509

{$(BD)\}.bin{$(OB)\}.obj: 
	.\bin2s.exe $(BD)\$(*B).bin > $(OB)\$(*B).asm
	armasm $(OB)\$(*B).asm -o $@
	del $(OB)\$(*B).asm

$(OA)\cpu.obj: $(SA)\cpu.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_16.inc

$(OA)\cpu_0F.obj: $(SA)\cpu_0F.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_16.inc

$(OA)\cpu_0F_66.obj: $(SA)\cpu_0F_66.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_66.inc

$(OA)\cpu_0F_67.obj: $(SA)\cpu_0F_67.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_67.inc

$(OA)\cpu_0F_USE32.obj: $(SA)\cpu_0F_USE32.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_32.inc

$(OA)\cpu_66.obj: $(SA)\cpu_66.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_66.inc

$(OA)\cpu_67.obj: $(SA)\cpu_67.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_67.inc

$(OA)\cpu_386.obj: $(SA)\cpu_386.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_32.inc

$(OA)\cpu_prot.obj: $(SA)\cpu_prot.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_16.inc

$(OA)\cpu_SIB.obj: $(SA)\cpu_SIB.asm $(IA)\defines.inc $(IA)\macros.inc

$(OA)\cpu_string.obj: $(SA)\cpu_string.asm $(IA)\defines.inc $(IA)\macros.inc

$(OA)\fpu.obj: $(SA)\fpu.asm $(IA)\defines.inc $(IA)\macros.inc $(IA)\gen_32.inc

$(OA)\pic.obj: $(SA)\pic.asm $(IA)\defines.inc $(IA)\macros.inc

$(OA)\EGA.obj: $(SA)\EGA.asm $(IA)\defines.inc $(IA)\macros.inc

$(OA)\MODEX.obj: $(SA)\MODEX.asm $(IA)\defines.inc $(IA)\macros.inc

$(OA)\MCGA.obj: $(SA)\MCGA.asm $(IA)\defines.inc

$(OA)\timer.obj : .\source\timer.cpp
	cl -c .\source\timer.cpp /Fo"$@" $(CCOPTS) /ZW

$(OA)\pax86retro.obj : .\source\pax86retro.cpp
	cl -c .\source\pax86retro.cpp /Fo"$@" $(CCOPTS) /ZW

$(OA)\CubeRenderer.obj : .\CubeRenderer.cpp
	cl -c .\CubeRenderer.cpp /Fo"$@" $(CCOPTS) /ZW

$(OA)\Direct3DBase.obj : .\Direct3DBase.cpp
	cl -c .\Direct3DBase.cpp /Fo"$@" $(CCOPTS) /ZW

$(OA)\Direct3DContentProvider.obj : .\Direct3DContentProvider.cpp
	cl -c .\Direct3DContentProvider.cpp /Fo"$@" $(CCOPTS) /ZW

$(OA)\Litil_Divil_on_WinRtComponent.obj : .\Litil_Divil_on_WinRtComponent.cpp
	cl -c .\Litil_Divil_on_WinRtComponent.cpp /Fo"$@" $(CCOPTS) /ZW

.\tester.obj : .\tester.cpp
	cl -c .\$(*B).cpp /Fo"$@" $(CCOPTS)

.\debug_disasm.obj : .\debug_disasm.c
	cl -c .\$(*B).c /Fo"$@" $(CCOPTS)

