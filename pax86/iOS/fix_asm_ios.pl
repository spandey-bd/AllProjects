#!/usr/bin/env perl
# by David Conrad
# This code is licensed under GPLv2 or later; go to gnu.org to read it
#  (not that it much matters for an asm preprocessor)
# usage: set your assembler to be something like "perl gas-preprocessor.pl gcc"
use strict;

# Apple's gas is ancient and doesn't support modern preprocessing features like
# .rept and has ugly macro syntax, among other things. Thus, this script
# implements the subset of the gas preprocessor used by x264 and ffmpeg
# that isn't supported by Apple's gas.

my @gcc_cmd = @ARGV;
my @preprocess_c_cmd;

my $fix_unreq = $^O eq "darwin";

if ($gcc_cmd[0] eq "-fix-unreq") {
    $fix_unreq = 1;
    shift @gcc_cmd;
} elsif ($gcc_cmd[0] eq "-no-fix-unreq") {
    $fix_unreq = 0;
    shift @gcc_cmd;
}

if (grep /\.c$/, @gcc_cmd) {
    # C file (inline asm?) - compile
    @preprocess_c_cmd = (@gcc_cmd, "-S");
} elsif (grep /\.[sS]$/, @gcc_cmd) {
    # asm file, just do C preprocessor
    @preprocess_c_cmd = (@gcc_cmd, "-E");
} else {
    die "Unrecognized input filetype";
}

@preprocess_c_cmd = map { /\.o$/ ? "-" : $_ } @preprocess_c_cmd;

my $comm;

# detect architecture from gcc binary name
if      ($gcc_cmd[0] =~ /arm/) {
    $comm = '@';
} elsif ($gcc_cmd[0] =~ /powerpc|ppc/) {
    $comm = '#';
}

# look for -arch flag
foreach my $i (1 .. $#gcc_cmd-1) {
    if ($gcc_cmd[$i] eq "-arch") {
        if ($gcc_cmd[$i+1] =~ /arm/) {
            $comm = '@';
        } elsif ($gcc_cmd[$i+1] =~ /powerpc|ppc/) {
            $comm = '#';
        }
    }
}

# assume we're not cross-compiling if no -arch or the binary doesn't have the arch name
if (!$comm) {
    my $native_arch = qx/arch/;
    if ($native_arch =~ /arm/) {
        $comm = '@';
    } elsif ($native_arch =~ /powerpc|ppc/) {
        $comm = '#';
    }
}

if (!$comm) {
    die "Unable to identify target architecture";
}

my %ppc_spr = (ctr    => 9,
               vrsave => 256);

open(ASMFILE, "-|", @preprocess_c_cmd) || die "Error running preprocessor";

my $current_macro = '';
my $macro_level = 0;
my %macro_lines;
my %macro_args;
my %macro_args_default;

my @pass1_lines;
my @ifstack;

my %seen_dup = ();

# pass 1: parse .macro
# note that the handling of arguments is probably overly permissive vs. gas
# but it should be the same for valid cases
while (<ASMFILE>) {
    # remove all comments (to avoid interfering with evaluating directives)
    s/$comm.*//x;

    # comment out unsupported directives
    s/\.type/$comm.type/x;
    s/\.func/$comm.func/x;
    s/\.endfunc/$comm.endfunc/x;
    s/\.ltorg/$comm.ltorg/x;
    s/\.size/$comm.size/x;
    s/\.fpu/$comm.fpu/x;
    s/\.arch/$comm.arch/x;
    s/\.object_arch/$comm.object_arch/x;
	s/\.eabi/@.eabi/x;

    # the syntax for these is a little different
    s/\.global/.globl/x;
    s/\.bss/.zerofill/x;
    # also catch .section .rodata since the equivalent to .const_data is .section __DATA,__const
    s/(.*)\.rodata/.const_data/x;
    s/\.int/.long/x;
    s/\.float/.single/x;
    s/\.section\s\.itcm.*//x;
    s/\.hword/.short/x;
    #s/\.value/.short/x;
    
    #pax86 specific
    s/run_core/_run_core/x;
    s/registers/_registers/x;
    s/CGA_B800/_CGA_B800/x;
    s/BIOS_F000/_BIOS_F000/x;
    s/INTVectors/_INTVectors/x;
    s/INTHandler/_INTHandler/x;
    s/ROM8x8Font/_ROM8x8Font/x;
    s/ROM8x16Font/_ROM8x16Font/x;
    s/EGAVGA_A000/_EGAVGA_A000/x;
    s/VESA_Bank/_VESA_Bank/x;
    s/BIOSData/_BIOSData/x;
    s/XMS_Total/_XMS_Total/x;
    s/XMS_Start_Aligned/_XMS_Start_Aligned/x;
    s/SpeakerIncr/_SpeakerIncr/x;
    s/DirectDACIncr/_DirectDACIncr/x;
    s/AdlibRun/_AdlibRun/x;
    s/AdlibInit/_AdlibInit/x;
    s/SBEmulation/_SBEmulation/x;
    s/EMSPageTable/_EMSPageTable/x;
    s/BreakReason/_BreakReason/x;
    s/BRUserBreak/_BRUserBreak/x;
    s/BRUnsIntCode/_BRUnsIntCode/x;
    s/BreakValue/_BreakValue/x;
    s/BreakOffs/_BreakOffs/x;
    s/BreakSeg/_BreakSeg/x;
    s/OrigUSE16RHandler/_OrigUSE16RHandler/x;
    s/cpu_cr0/_cpu_cr0/x;
    s/cpu_big/_cpu_big/x;
    s/stack_mask/_stack_mask/x;
    s/irqflag_mutex/_irqflag_mutex/x;
    s/IRQPending/_IRQPending/x;
    s/IRQFlagAddr/_IRQFlagAddr/x;
    s/PS2MouseIRQ/_PS2MouseIRQ/x;
    s/KeyboardDataByte/_KeyboardDataByte/x;
    s/KeyboardStatusByte/_KeyboardStatusByte/x;
    s/BlinkCursor/_BlinkCursor/x;
    s/HideCursor/_HideCursor/x;
    s/Out3C0/_Out3C0/x;
    s/Out3CF/_Out3CF/x;
    s/TEXTBuffer/_TEXTBuffer/x;
    s/VideoParameterTable/_VideoParameterTable/x;
    s/EGAPaletteTable/_EGAPaletteTable/x;
    s/EGA_PALETTE_SAVE/_EGA_PALETTE_SAVE/x;
    s/BG_PALETTE/_BG_PALETTE/x;
    s/text_pos_by_page_cursor/_text_pos_by_page_cursor/x;
    s/text_pos_by_page_col_row/_text_pos_by_page_col_row/x;
    s/EGA_write_byte_from_C/_EGA_write_byte_from_C/x;
    s/VGA_Graphics_regs_3CF/_VGA_Graphics_regs_3CF/x;
    s/VGA_Sequencer_regs_3C5/_VGA_Sequencer_regs_3C5/x;
    s/VGA_attr_data_3C0/_VGA_attr_data_3C0/x;
    s/VGA_misc_3C2/_VGA_misc_3C2/x;
    s/VGA_pel_mask_3C6/_VGA_pel_mask_3C6/x;
    s/VGA_out_3C9_addr/_VGA_out_3C9_addr/x;
    s/VGA_CRTC_data_3D5/_VGA_CRTC_data_3D5/x;
    s/VGAStartAddrHigh/_VGAStartAddrHigh/x;
    s/VGAStartAddrLow/_VGAStartAddrLow/x;
    s/VGAMemoryMode/_VGAMemoryMode/x;
    s/out_3C4_VGA_sequencer/_out_3C4_VGA_sequencer/x;
    s/out_3C5_VGA_sequencer/_out_3C5_VGA_sequencer/x;
    s/out_3C8_VGA_pel/_out_3C8_VGA_pel/x;
    s/out_3C9_VGA_pel/_out_3C9_VGA_pel/x;
    s/out_3C9_EGA_pel/_out_3C9_EGA_pel/x;
    s/out_3D9_CGA_Color/_out_3D9_CGA_Color/x;
    s/CGA_3D9_value/_CGA_3D9_value/x;
    s/CharToMCGAScreen/_CharToMCGAScreen/x;
    s/PixelToMCGAScreen/_PixelToMCGAScreen/x;
    s/PixelFromMCGAScreen/_PixelFromMCGAScreen/x;
    s/CharToEGAScreen/_CharToEGAScreen/x;
    s/PixelToEGAScreen/_PixelToEGAScreen/x;
    s/PixelFromEGAScreen/_PixelFromEGAScreen/x;
    s/CharToCGA4Screen/_CharToCGA4Screen/x;
    s/PixelToCGA4Screen/_PixelToCGA4Screen/x;
    s/PixelFromCGA4Screen/_PixelFromCGA4Screen/x;
    s/CharToCGA6Screen/_CharToCGA6Screen/x;
    s/PixelToCGA6Screen/_PixelToCGA6Screen/x;
    s/PixelFromCGA6Screen/_PixelFromCGA6Screen/x;
    s/screen_copy_03/_screen_copy_03/x;
    s/screen_copy_04/_screen_copy_04/x;
    s/screen_copy_06/_screen_copy_06/x;
    s/screen_copy_0D/_screen_copy_0D/x;
    s/screen_copy_13/_screen_copy_13/x;
    s/screen_copy_640/_screen_copy_640/x;
    s/screen_copy_ModeX/_screen_copy_ModeX/x;
    s/screen_copy_SVGA/_screen_copy_SVGA/x;
    s/AdLibMasked/_AdLibMasked/x;
    s/AdLibStatus/_AdLibStatus/x;
    s/AdLibTimer1/_AdLibTimer1/x;
    s/AdLibTimer2/_AdLibTimer2/x;

    #pax86 binaries
    s/BiosDataInit_bin/_BiosDataInit_bin/x;
    s/int08_bin/_int08_bin/x;
    s/int09_bin/_int09_bin/x;
    s/int0b_bin/_int0b_bin/x;
    s/int0f_bin/_int0f_bin/x;
    s/int11_bin/_int11_bin/x;
    s/int12_bin/_int12_bin/x;
    s/int16_bin/_int16_bin/x;
    s/int21_bin/_int21_bin/x;
    s/int67_bin/_int67_bin/x;
    s/int74_bin/_int74_bin/x;
    s/ROM8x8_bin/_ROM8x8_bin/x;
    s/ROM8x14_bin/_ROM8x14_bin/x;
    s/ROM8x16_bin/_ROM8x16_bin/x;
    s/ScanXlat_bin/_ScanXlat_bin/x;

    #pax86 serialization global references
    s/DMAAddress/_DMAAddress/x;
    s/DMACurrent/_DMACurrent/x;
    s/DMAFlipFlop/_DMAFlipFlop/x;
    s/DMALength/_DMALength/x;
    s/Port61Data/_Port61Data/x;
    s/Port92Data/_Port92Data/x;
    s/SLOT1/_SLOT1/x;
    s/SLOT2/_SLOT2/x;
    s/VGA_attr_addr_3C0/_VGA_attr_addr_3C0/x;
    s/VGA_serialize_start/_VGA_serialize_start/x;
    s/VGA_serialize_end/_VGA_serialize_end/x;
    s/adlib_serialize_start/_adlib_serialize_start/x;
    s/adlib_serialize_end/_adlib_serialize_end/x;
    s/ch0_slot1_wavetable/_ch0_slot1_wavetable/x;
    s/cpu_cpl/_cpu_cpl/x;
    s/cpu_cr2/_cpu_cr2/x;
    s/cpu_cr3/_cpu_cr3/x;
    s/cpu_gdt_base/_cpu_gdt_base/x;
    s/cpu_gdt_limit/_cpu_gdt_limit/x;
    s/cpu_gdt_phys/_cpu_gdt_phys/x;
    s/cpu_idt_base/_cpu_idt_base/x;
    s/cpu_idt_limit/_cpu_idt_limit/x;
    s/cpu_idt_phys/_cpu_idt_phys/x;
    s/cpu_ldt_base/_cpu_ldt_base/x;
    s/cpu_ldt_limit/_cpu_ldt_limit/x;
    s/cpu_ldt_phys/_cpu_ldt_phys/x;
    s/cpu_ldt_value/_cpu_ldt_value/x;
    s/cpu_tss_base/_cpu_tss_base/x;
    s/cpu_tss_limit/_cpu_tss_limit/x;
    s/cpu_tss_phys/_cpu_tss_phys/x;
    s/cpu_tss_is386/_cpu_tss_is386/x;
    s/cpu_tss_selector/_cpu_tss_selector/x;
    s/cpu_tss_valid/_cpu_tss_valid/x;
    s/sin_tab/_sin_tab/x;
    s/sb_request_pointer/_sb_request_pointer/x;
    s/sb_play_serialize_start/_sb_play_serialize_start/x;
    s/sb_play_serialize_end/_sb_play_serialize_end/x;
    s/sb_cmd_serialize_start/_sb_cmd_serialize_start/x;
    s/sb_cmd_serialize_end/_sb_cmd_serialize_end/x;
    s/sb_buffer_start/_sb_buffer_start/x;
    s/sb_buffer_end/_sb_buffer_end/x;
    s/pic_serialize_start/_pic_serialize_start/x;
    s/pic_serialize_end/_pic_serialize_end/x;

    
    # catch unknown section names that aren't mach-o style (with a comma)
    if (/.section ([^,]*)$/) {
        die ".section $1 unsupported; figure out the mach-o section name and add it";
    }

	if(/[a-zA-Z_0-9]{2,}:$/ || /\.globl.*/)
	{
		$seen_dup{$_}++;
		next if $seen_dup{$_} > 1;
	}
	
    parse_line($_);
}

sub handle_if {
    my $line = $_[0];
    # handle .if directives; apple's assembler doesn't support important non-basic ones
    # evaluating them is also needed to handle recursive macros
    if ($line =~ /\.if(n?)([a-z]*)\s+(.*)/) {
        my $result = $1 eq "n";
        my $type   = $2;
        my $expr   = $3;

        if ($type eq "b") {
            $expr =~ s/\s//g;
            $result ^= $expr eq "";
        } elsif ($type eq "c") {
            if ($expr =~ /(.*)\s*,\s*(.*)/) {
                $result ^= $1 eq $2;
            } else {
                die "argument to .ifc not recognized";
            }
        } elsif ($type eq "") {
            $result ^= eval($expr) != 0;
        } elsif ($type eq "eq") {
            $result = eval($expr) == 0;
        } elsif ($type eq "lt") {
            $result = eval($expr) < 0;
        } else {
	    chomp($line);
            die "unhandled .if varient. \"$line\"";
        }
        push (@ifstack, $result);
        return 1;
    } else {
        return 0;
    }
}

sub parse_line {
    my $line = @_[0];

    # evaluate .if blocks
    if (scalar(@ifstack)) {
        if (/\.endif/) {
            pop(@ifstack);
            return;
        } elsif ($line =~ /\.elseif\s+(.*)/) {
            if ($ifstack[-1] == 0) {
                $ifstack[-1] = !!eval($1);
            } elsif ($ifstack[-1] > 0) {
                $ifstack[-1] = -$ifstack[-1];
            }
            return;
        } elsif (/\.else/) {
            $ifstack[-1] = !$ifstack[-1];
            return;
        } elsif (handle_if($line)) {
            return;
        }

        # discard lines in false .if blocks
        foreach my $i (0 .. $#ifstack) {
            if ($ifstack[$i] <= 0) {
                return;
            }
        }
    }

    if (/\.macro/) {
        $macro_level++;
        if ($macro_level > 1 && !$current_macro) {
            die "nested macros but we don't have master macro";
        }
    } elsif (/\.endm/) {
        $macro_level--;
        if ($macro_level < 0) {
            die "unmatched .endm";
        } elsif ($macro_level == 0) {
            $current_macro = '';
            return;
        }
    }

    if ($macro_level > 1) {
        push(@{$macro_lines{$current_macro}}, $line);
    } elsif ($macro_level == 0) {
        expand_macros($line);
    } else {
        if (/\.macro\s+([\d\w\.]+)\s*(.*)/) {
            $current_macro = $1;

            # commas in the argument list are optional, so only use whitespace as the separator
            my $arglist = $2;
            $arglist =~ s/,/ /g;

            my @args = split(/\s+/, $arglist);
            foreach my $i (0 .. $#args) {
                my @argpair = split(/=/, $args[$i]);
                $macro_args{$current_macro}[$i] = $argpair[0];
                $argpair[0] =~ s/:vararg$//;
                $macro_args_default{$current_macro}{$argpair[0]} = $argpair[1];
            }
            # ensure %macro_lines has the macro name added as a key
            $macro_lines{$current_macro} = [];

        } elsif ($current_macro) {
            push(@{$macro_lines{$current_macro}}, $line);
        } else {
            die "macro level without a macro name";
        }
    }
}

sub expand_macros {
    my $line = @_[0];

    # handle .if directives; apple's assembler doesn't support important non-basic ones
    # evaluating them is also needed to handle recursive macros
    if (handle_if($line)) {
        return;
    }

    if (/\.purgem\s+([\d\w\.]+)/) {
        delete $macro_lines{$1};
        delete $macro_args{$1};
        delete $macro_args_default{$1};
        return;
    }

    if ($line =~ /(\S+:|)\s*([\w\d\.]+)\s*(.*)/ && exists $macro_lines{$2}) {
        push(@pass1_lines, $1);
        my $macro = $2;

        # commas are optional here too, but are syntactically important because
        # parameters can be blank
		my $argListStr = $3;
		$argListStr =~ s/"//g;
        my @arglist = split(/,/, $argListStr);
        my @args;
        my @args_seperator;

        my $comma_sep_required = 0;
        foreach (@arglist) {
            # allow for + and - in macro arguments
            $_ =~ s/\s*\+\s*/+/;
            $_ =~ s/\s*\-\s*/-/;

            my @whitespace_split = split(/\s+/, $_);
            if (!@whitespace_split) {
                push(@args, '');
                push(@args_seperator, '');
            } else {
                foreach (@whitespace_split) {
                        #print ("arglist = \"$_\"\n");
                    if (length($_)) {
                        push(@args, $_);
                        my $sep = $comma_sep_required ? "," : " ";
                        push(@args_seperator, $sep);
                        #print ("sep = \"$sep\", arg = \"$_\"\n");
                        $comma_sep_required = 0;
                    }
                }
            }

            $comma_sep_required = 1;
        }

        my %replacements;
        if ($macro_args_default{$macro}){
            %replacements = %{$macro_args_default{$macro}};
        }

        # construct hashtable of text to replace
        foreach my $i (0 .. $#args) {
            my $argname = $macro_args{$macro}[$i];
            my @macro_args = @{ $macro_args{$macro} };
            if ($args[$i] =~ m/=/) {
                # arg=val references the argument name
                # XXX: I'm not sure what the expected behaviour if a lot of
                # these are mixed with unnamed args
                my @named_arg = split(/=/, $args[$i]);
                $replacements{$named_arg[0]} = $named_arg[1];
            } elsif ($i > $#{$macro_args{$macro}}) {
                # more args given than the macro has named args
                # XXX: is vararg allowed on arguments before the last?
                $argname = $macro_args{$macro}[-1];
                if ($argname =~ s/:vararg$//) {
                    #print "macro = $macro, args[$i] = $args[$i], args_seperator=@args_seperator, argname = $argname, arglist[$i] = $arglist[$i], arglist = @arglist, args=@args, macro_args=@macro_args\n";
                    #$replacements{$argname} .= ", $args[$i]";
                    $replacements{$argname} .= "$args_seperator[$i] $args[$i]";
                } else {
                    die "Too many arguments to macro $macro";
                }
            } else {
                $argname =~ s/:vararg$//;
                $replacements{$argname} = $args[$i];
            }
        }

        # apply replacements as regex
        foreach (@{$macro_lines{$macro}}) {
            my $macro_line = $_;
            # do replacements by longest first, this avoids wrong replacement
            # when argument names are subsets of each other
            foreach (reverse sort {length $a <=> length $b} keys %replacements) {
                $macro_line =~ s/\\$_/$replacements{$_}/g;
            }
            $macro_line =~ s/\\\(\)//g;     # remove \()
            parse_line($macro_line);
        }
    } else {
        push(@pass1_lines, $line);
    }
}

close(ASMFILE) or exit 1;

my @sections;
my $num_repts;
my $rept_lines;

my %literal_labels;     # for ldr <reg>, =<expr>
my $literal_num = 0;

my $in_irp = 0;
my @irp_args;
my $irp_param;

# pass 2: parse .rept and .if variants
# NOTE: since we don't implement a proper parser, using .rept with a
# variable assigned from .set is not supported
foreach my $line (@pass1_lines) {
    # handle .previous (only with regard to .section not .subsection)
    if ($line =~ /\.(section|text|const_data)/) {
        push(@sections, $line);
    } elsif ($line =~ /\.previous/) {
        if (!$sections[-2]) {
            die ".previous without a previous section";
        }
        $line = $sections[-2];
        push(@sections, $line);
    }

    # handle ldr <reg>, =<expr>
    if ($line =~ /(.*)\s*ldr([\w\s\d]+)\s*,\s*=(.*)/) {
        my $label = $literal_labels{$3};
        if (!$label) {
            $label = ".Literal_$literal_num";
            $literal_num++;
            $literal_labels{$3} = $label;
        }
        $line = "$1 ldr$2, $label\n";
    } elsif ($line =~ /\.ltorg/) {
        foreach my $literal (keys %literal_labels) {
            $line .= "$literal_labels{$literal}:\n .word $literal\n";
        }
        %literal_labels = ();
    }

    # @l -> lo16()  @ha -> ha16()
    $line =~ s/,\s+([^,]+)\@l\b/, lo16($1)/g;
    $line =~ s/,\s+([^,]+)\@ha\b/, ha16($1)/g;

    # move to/from SPR
    if ($line =~ /(\s+)(m[ft])([a-z]+)\s+(\w+)/ and exists $ppc_spr{$3}) {
        if ($2 eq 'mt') {
            $line = "$1${2}spr $ppc_spr{$3}, $4\n";
        } else {
            $line = "$1${2}spr $4, $ppc_spr{$3}\n";
        }
    }

    # old gas versions store upper and lower case names on .req,
    # but they remove only one on .unreq
    if ($fix_unreq) {
        if ($line =~ /\.unreq\s+(.*)/) {
            $line = ".unreq " . lc($1) . "\n";
            print ".unreq " . uc($1) . "\n";
        }
    }

    if ($line =~ /\.rept\s+(.*)/) {
        $num_repts = $1;
        $rept_lines = "\n";

        # handle the possibility of repeating another directive on the same line
        # .endr on the same line is not valid, I don't know if a non-directive is
        if ($num_repts =~ s/(\.\w+.*)//) {
            $rept_lines .= "$1\n";
        }
        $num_repts = eval($num_repts);
    } elsif ($line =~ /\.irp\s+([\d\w\.]+)\s*(.*)/) {
        $in_irp = 1;
        $num_repts = 1;
        $rept_lines = "\n";
        $irp_param = $1;

        # only use whitespace as the separator
        my $irp_arglist = $2;
        $irp_arglist =~ s/,/ /g;
        $irp_arglist =~ s/^\s+//;
        @irp_args = split(/\s+/, $irp_arglist);
    } elsif ($line =~ /\.irpc\s+([\d\w\.]+)\s*(.*)/) {
        $in_irp = 1;
        $num_repts = 1;
        $rept_lines = "\n";
        $irp_param = $1;

        my $irp_arglist = $2;
        $irp_arglist =~ s/,/ /g;
        $irp_arglist =~ s/^\s+//;
        @irp_args = split(//, $irp_arglist);
    } elsif ($line =~ /\.endr/) {
        if ($in_irp != 0) {
            foreach my $i (@irp_args) {
                my $line = $rept_lines;
                $line =~ s/\\$irp_param/$i/g;
                $line =~ s/\\\(\)//g;     # remove \()
                print $line;
            }
        } else {
            for (1 .. $num_repts) {
                print $rept_lines;
            }
        }
        $rept_lines = '';
        $in_irp = 0;
        @irp_args = '';
    } elsif ($rept_lines) {
        $rept_lines .= $line;
    } else {
        print $line;
    }
}

print ".text\n";
foreach my $literal (keys %literal_labels) {
    print "$literal_labels{$literal}:\n .word $literal\n";
}
