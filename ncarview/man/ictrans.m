.\"
.\"	$Id: ictrans.m,v 1.31 2008-12-23 00:04:15 haley Exp $
.\"
.\" ictrans 3.01 90/06/22 
.TH ICTRANS 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
ictrans \- interactive NCAR CGM translator
.B ctrans
.SH SYNOPSIS
.B ictrans
[
.BI \-d " device"
] [
.BI \-font " font"
] [
.BI \-e " script"
]* [
.B \-soft
] [
.B \-bell
] [
.BI \-lmin " min"
] [
.BI \-lmax " max"
] [
.BI \-lscale " scale"
] [
.BI \-pal " pal_fname"
] [
.B \-Version
] [
.BI \-wid " window_id"
] [
.I device\-specific options
] 
.IR metafile .\|.\|.
.SH DESCRIPTION
.B ictrans
is the user interface to the Computer Graphics Metafile (\s-1CGM\s0)
translator
.B ctrans.
.B ictrans 
will enter command interpreter mode upon invocation and await instructions
from the user. When waiting for commands
from the user,
.B ictrans
displays the prompt
.RB ` ictrans> '.
.LP
Upon invocation
.B ictrans 
performs a configuration of its spooled device table. The table is configured
by processing several sources.
.B ictrans 
first searches for the file
.B ncarv_spool
in the $NCARG_ROOT/lib/ncarg directory.
If the file exists 
.B ictrans
will load it into the spooled device table. Next, ictrans searches for the
file
.B .ncarv_spool
in the user's home directory. If found, its contents are merged into the 
spooler table. Finally, the NCARV_SPOOL environment variable may contain
the definition for a single spooler. If this variable is set, its contents
also are merged into the table. Each entry in the above set of sources is 
identified by a name. If a conflict in names exists then the last 
entry encountered takes precedence. i.e. the previous entry of the same name is 
overridden.
.SH OPTIONS
.TP
.B \-bell
Ring the bell at the end of each frame. The default is to run in silent mode.
This option is not supported by all devices.
.TP
.BI \-d " device"
Device name.
.B ictrans
will use the 
.I Graphcap
(if it exists) or the appropriate graphics library indicated by 
.I device;
.IP
If 
.I device
is preceded by a UNIX directory path then 
.B ictrans
will look in that directory for the specified graphcap. Otherwise 
.B ictrans
searches the directory $NCARG_ROOT/lib/ncarg/graphcaps for the graphcap.
.IP
For all device specifications
except
.B X11
output is directed to standard out. In the case of
.B X11
translation results in appropriate calls to the X11 libraries.
See 
.BR graphcap(5NCARG)
for a description of supported devices. 
See
.BR gcaps(1NCARG)
for a list of devices supported by 
.I your 
particular configuration of 
.BR ctrans .
.IP
This option overrides the 
.B GRAPHCAP
environment variable.
.TP
.BI \-e " script"
.I script
is a single 
.B ictrans
command. The valid 
.B ictrans
commands are discussed in the 
.B COMMANDS
section below.
Multiple 
.B -e
options may appear on a single command line. Be careful to use quotes if
your command contains spaces or metacharacters that might be interpreted
by the shell. When this option is used
.B ictrans
does not enter interactive mode. It simply performs the given commands
and then exits.
.TP
.BI \-font " fontcap"
Fontcap file to be used for stroking text.
When interpreting CGM
.B TEXT
command elements use
.I fontcap
as the default font for textual translation. Note: CGMs may contain textual
descriptions which are not embedded in CGM
.B TEXT
elements. Hence they are not influenced by
.I fontcap
specifications.
Note also that a CGM may explicitly specify a named font which may override a
font provided on the command line. The environment variable FONTCAP
may also be used to specify a default fontcap.
.IP
If 
.I fontcap
is preceded by a UNIX directory path then 
.B ictrans
will look in that directory for the specified fontcap. Otherwise 
.B ictrans
searches the directory $NCARG_ROOT/lib/ncarg/fontcaps for the fontcap.
.IP
See 
.BR fontcap(5NCARG)
for a description of the available fontcaps. See
.BR fcap(1NCARG)
for a list of the fontcaps installed on your
system.
.IP
This option overrides the 
.B FONTCAP
environment variable.
.TP
.BI \-lmin " min"
On devices which support line width scaling all lines are guaranteed to be
scaled at least
.I min
times the default line width for that device. This option effectively 
insures that the minimum value for the CGM element "LINE WIDTH" is 
.IR min . 
.TP
.BI \-lmax " max"
On devices which support line width scaling all lines are guaranteed to be
scaled at most
.I max
times the default line width for that device. This option effectively 
insures that the maximum value for the CGM element "LINE WIDTH" is 
.IR max . 
The results of setting 
.I max
less then 
.I min
are undefined.
.TP
.BI \-lscale " scale"
On devices which support line width scaling all line width specifications
within the metafile will be scaled by 
.BR scale .
will be scaled
.I scale
This option is subject to modification by the 
.BR -lmin " and " -lmax 
options.
.TP
.BI \-pal " pal_fname"
Use the color palette defined in the file
.I pal_fname
for subsequent translation of the metafile. This palette will override any 
color map defined by the CGM being translated. For a description of 
the format of 
.I pal_fname
see ras_palette(5NCARG).
.TP 
.B \-soft
Unconditionally perform software filling of all filled polygons. This 
option may be useful for devices which have limits on the number of
vertices describing a polygon. On some devices this number is known and
software filling is performed, as appropriate, without user specification.
.TP
.BI \-Version
Print the version number and then exit.
.SH DEVICE SPECIFIC OPTIONS
.B ictrans
accepts an identical set of device-specific options to that of
.BR ctrans.
For a description of the device-specific options see 
.BR ctrans(1NCARG) .
.SH COMMANDS
.SS Command Structure
.LP 
.B ictrans 
commands have a simple and regular structure. Commands which operate directly
on the metafile being processed may be preceded by a 
.I frame_list
which designate the frames to which the command applies. Some commands
accept arguments. Anything following a command name is regarded
as an argument:
.RS
.HP
(
.IR "frame " [
.BI " ," frame
])*
.IR "command  " ( " argument" 
)*
.br
.RE
.LP
A comma-separated pair of frames implies the inclusive list of frames. 
If a frame list is omitted and a command requires a frame then the current
frame is used as the default. If no argument list is specified and one is 
required then a default argument is used whenever possible.
.LP
For example, the command
.B 1,5 8 save
.I /tmp/foo
would write the first through fifth and the eighth frame of the metafile
to the file 
.IR /tmp/foo .
.LP
Commands will ignore any unexpected arguments.
Command names may be abbreviated up to the point that they are unique.
.SS "Frame Lists"
.LP
Frames can be addressed in several ways:
.TP
.I nnn
By frame number. 
Frames are assigned a relative number from first to last in the metafile.
The first frame is numbered 
.IR 1 .
.TP
.B $
The last frame in the file.
.TP
.B \&.
The current frame. 
.B ictrans
keeps track of the last frame upon which an operation was performed. This 
frame is called the "current frame". 
.TP
.IB frame "+-" n
A frame number followed by a plus sign
.RB (\^ + \^)
or a minus sign
.RB ( \- ),
followed by a decimal number, specifies
the frame plus or minus the indicated number of frames. 
.I frame
may be omitted in which case the current frame is assumed. For example,
.RB ` 10\^ +2 ' 
addresses frame 12 in the metafile.
.SS Commands
.HP
.B \&!
.RI " command "
.br
Run 
.I command 
as a shell command on the local machine. 
.HP
.B alias
[ 
.I  name
[
.I def
] ]
.br
Assign
.I def
to the alias
.IR name .
If
.I def
is omitted, the alias
.I name
is displayed along with its current definition.  If both
.I name
and
.I def
are omitted, all aliases are displayed.
.I def 
is of the form:
.sp
.in +0.5i
.BI :
.BI [ " ctrans_args " ]
.B :
.BI [ " filter_chain " ]
.in -0.5i
.sp
where 
.I ctrans_args
is list of command line arguments for the metafile translator 
.B ctrans 
and 
.I filter_chain
is a set of simple commands separated by 
.BR | .
.I filter_chain 
may be terminated by 
.B > 
or 
.B >>
.IR filename .
For example:
.sp
.in +0.5i
.ft B
ictrans> alias \fIname1\fP : -d xwd : | cat > \fIoutfile\fR
.ft R
.sp
.in -0.5i
or
.sp
.in +0.5i
.ft B
ictrans> alias \fIname2\fP : -d ps.mono : | \fIfilter1\fP | lpr 
.ft R
.sp
.in -0.5i
.HP
.B dup
.BI [ number ]
.br
This command is used to set the number of times each frame is displayed during
subsequent plotting. The default is one. If
.B dup
is invoked without any arguments the current value of dup is returned.
.HP
.B count
.br
Reports number of frames contained in the file.
.HP
.B current
.br
Reports the current frame.
.HP
.B file
.BI [ " metafile "]
.br
The file 
.I metafile
will be used for subsequent translation. 
.B ictrans 
uses the shell defined by the environment variable
.B SHELL
(/bin/sh by default) to perform filename substitution on 
.IR metafile . 
The rules governing filename substitution are as defined by the 
working shell. If no argument is given the current metafile name is 
reported.
.HP
.B font
.BI [ " font " ]
.br
Set the fontcap to 
.I font
for future translation. This function is identical to that of the
.B -font
option.  If 
.I font
is omitted the current fontcap name is reported.
.HP
.B help
.BI [ " command " ]
.br
Print a usage statement for 
.IR command .
If 
.I command
is omitted a brief description of all commands is given.
.HP
.BI [ " frames " ] 
.B list
.br
Provide brief information about each metafile frame in 
.IR frames .
If 
.I frames
is omitted then the current frame is used. If 
.I frames 
is omitted and the current frame is not
the last frame then the current frame is incremented to the next frame 
in the metafile.
.HP
.BI < " frame1 " >  
.BI < " frame2 " >  
.B merge
.br
Plot frame number
.I frame1
and then plot frame number
.I frame2
over the first frame without clearing the device. The result is a "merge" of
the two plots. The current frame is not changed. There are no defaults for
.IR frame1 " or " frame2 .
The resulting plot might not be what was expected. Attributes from the first
frame, such as color,  may override attributes in the second frame.
.HP
.B loop
.br
Toggle loop mode on or off. When loop mode is on subsequent
.B plot
commands will cause the requested frames to be plotted and then 
.B ictrans 
will proceed to 
either the first frame in the defined segment or the last and repeatedly
display the first through last (last through first) frames. Looping continues
until an interrupt 
signal is received. The determination of which order to loop, forwards or
backwards, is made as follows: If the last group of frames plotted was in 
ascending order loop forward. If the last group of frames plotted was in
descending order loop backwards. If the order cannot be determined don't 
loop. For example; if loop mode is set "1 3,4 plot" will result in forward
looping, "1 4,3 plot" will result in backward looping, and "3,4 1 plot"
will result in no looping because the last group of frames plotted, "1",
is a single frame. 
.HP
.B movie
.BI [ " time "]
.br
Display each frame for  
.I time
seconds before proceding to the next frame during subsequent plots. If
.I time
is omitted then movie mode is toggled off or on. In the case the movie mode
is toggled on the default time is zero seconds. If movie mode is toggled to 
off a newline must be received before advancing to the next frame
during plotting.
.HP
.B next
.br
Multiple files may be specified on the 
.B ictrans
command line. To edit the next file in the argument list use the 
.B next 
command. 
.HP
.BI [ " frames " ] 
.B plot
.br
Plot the addressed frames. If 
.I frames
is omitted then the current frame is plotted and if possible, the current 
frame number is incremented. If "movie" mode is set 
.B ictrans 
will wait 
.I time 
seconds after displaying each plot before continuing.
.I time
is set with the 
.B movie 
command.
If "movie" mode is not set 
.B ictrans
will wait for a newline character before advancing to the next frame.
.B plot 
will report the number of frames and the last frame in
.IR frames .
Plotting will be terminated and 
.B ictrans 
will reenter command mode after
the last frame is plotted or upon receiving a interrupt signal, SIGINT.
.HP 
.BI [ " frames " ] 
.B Print
.br
The addressed frames are translated and sent to the current spooling device.
Translation is performed by a spawned translator. The 
.B spooler
command may be used to select a spooling device.
See
.BR ncarv_spool(5NCARG).
.HP 
.B quit
.br
Terminate the session.
.HP 
.BI [ " frames " ] 
.B save
.BI [ " metafile " ]
.br
Save the addressed frames to 
.IR metafile .
If 
.I metafile
does not exist then it is created. Filename substitution is performed on
.IR metafile .
If
.I metafile
is omitted than the last file saved to is used. If 
.I frames
is omitted than the current frame is used.
.HP 
.BI [ " frames " ] 
.B Save
.BI [ " metafile " ]
.br
Same as the 
.B save 
command
except
.B Save
does not confirm its actions with the user in the case that the file
exists. If the file exists  but is not a valid NCGM it is overwritten.
If the file exists and is a valid NCGM it is appended to.
.HP
.B skip
.BI [ " number " ]
.br
Set number of frames to skip over during subsequent plotting. For example,
if "skip" is set to 1 and a request is made to plot frames 1 through 10
frames 1, 3, 5, 7, and 9 will be displayed. With no arguments 
.B skip
reports its current value. The default is zero.
.HP
.B spooler
.BI [ " spooler_alias " ]
.br
With no arguments the current spooler alias name is reported. If 
.I "spooler_alias"
is a valid alias either defined by the 
.B alias
command, or in a 
.B ncarv_spool
configuration file, or by the NCARV_SPOOL environment variable, then 
.I "spooler alias" 
becomes the current spooler. Subsequent 
.B Print
commands will use the spooler definition defined by the current spooler. 
See
.BR ncarv_spool(5NCARG).
.HP 
.BI [ " start frame " ] 
.B start
.br
This command defines the first frame in a segment of frames. 
.B start,
together with the
.B stop
command, define the boundaries of a segment of metafile frames. When 
.B ictrans
is in loop mode the contents of this segment are repeatedly displayed.
The default
.I start frame
is the first frame in the metafile, 1. If no arguments are given
.B start
reports the first frame in the current segment.
.HP 
.BI [ " stop frame " ] 
.B stop
.br
This command defines the last frame in a segment of frames. 
The default
.I stop frame
is the last frame in the metafile, $.  If no arguments are given
.B stop 
reports the last frame in the current segment.
.HP
.B zoom
[ \fIllx\fP [ \fIlly\fP [ \fIurx\fP [ \fIury\fP ]]]]
.br
The 
.B zoom 
command allows for specification of a workstation window (in the GKS
sense). Four coordinates are specified which define a rectangular window
which is a subset of the normalized VDC rectangle with corner points
(0.0, 0.0) and (1.0, 1,0). The specified window is then mapped onto the
entire viewport. For example
.sp
.in +0.5i
.ft B
ictrans> zoom 0.0 0.0 0.5 0.5
.ft R
.sp
.in -0.5i
would result in the lower left quarter of subsequent plots being blown up
to fill the entire display. Specification of such a window may be used 
for zooming and panning.
.IP
The range with which one may zoom in on a plot may be limited by the
integer addressing precision of the device.
.SH EXAMPLES
The following example shows how 
.B ictrans 
might be used in a batch mode to translate a metafile called
.B gmeta
and send the translated results of the entire file
to a spooled device called "imagen" which might be defined in the system
.B ncarv_spool
file:
.sp
.IP
.B "% ictrans -e 'spooler imagen' -e '1,$Print' gmeta
.sp
.SH ENVIRONMENT
.TP
.B FONTCAP
Default fontcap specifier.
.TP
.B GRAPHCAP
Default output device specifier.
.TP
.B NCARG_ROOT
Path to root of NCAR Graphics installation.
.TP
.B NCARG_LIB
If set this variable contains the path to the installed NCAR Graphics 
libraries. 
.B NCARG_LIB
overrides 
.BR NCARG_ROOT .
.TP
.B NCARG_TMP
If set, this environment variable contains a directory path to be used for
temporary files. On most systems the default is 
.BR /tmp .
On some systems the default is 
.BR /usr/tmp .
.TP
.B TMPDIR
If 
.B NCARG_TMP
is not set this environment variable specifies the directory path for
scratch disk space. If neither 
.B NCARG_TMP 
.B TMPDIR
is set a site-dependent, hard-coded default is used. 
.SH FILES
.PD 0
.TP 28
.B $NCARG_ROOT/lib/ncarg/ncarv_spool
local
.B ictrans
spooler config file
.TP 
.B ~/.\|ncarv_spool
user's
.B ictrans
spooler config file
.PD
.SH SEE ALSO
.BR ctrans(1NCARG),
.BR fcaps(1NCARG),
.BR gcaps(1NCARG),
.BR idt(1NCARG),
.BR med(1NCARG),
.BR ncarv_spool(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH CAVEATS
Metafile frames written to an existing file via the 
.B save
command will be subject to the effects of any global "attribute elements"
contained within the file.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
