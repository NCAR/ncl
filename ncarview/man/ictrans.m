.\" ictrans 3.01 90/06/22 
.TH ICTRANS 1NCARG "22 June 1990" NCAR "NCAR View 3.01"
.SH NAME
ictrans \- user interface to the CGM translator 
.B ctrans
.SH SYNOPSIS
.B ictrans
[
.BI \-d " device"
] [
.BI \-f " font"
] [
.B \-soft
] [
.BI \-lmin " min"
] [
.BI \-lmax " max"
] [
.BI \-lscale " scale"
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
will enter command interpreter mode upon invocation and awaits instructions
from the user. When awaiting commands
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
in the installation site of local NCAR View libraries (usually /usr/local/lib).
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
.BI \-d " device"
Designate the target device for metafile translation where device is one
of: 
.I valid_graphcap_name, 
.B "CTXT , sunraster , sunview , X11"
or 
.B xbfr. 
For all device specifications
except
.B X11 
and 
.BR sunview ,
output is directed to standard out. In the case of 
.B X11 
and 
.B sunview,
translation results in appropriate calls to the X11 and Sunview
libraries, respectively. The environment variable 
GRAPHCAP
may be used to specify a default device should 
the device option be omitted.
Not all devices may be supported by your version
of 
.B ictrans.
For a list of supported devices make sure 
GRAPHCAP 
is not set and invoke 
.B ictrans 
without any arguments.
.TP
.BI \-f " fontcap"
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
Note that a CGM may explicitly specify a named font which may override a 
font provided on the command line. The environment variable FONTCAP 
may be used to specify a default fontcap.
.TP 
.B \-soft
Unconditionally perform sofware filling of all filled polygons. This 
option may be useful for devices which have limits on the number of
vertices describing a polygon. On some devices this number is known and
software filling is performed, as appropriate, without user specification.
.SH DEVICE SPECIFIC OPTIONS
Some ictrans options are only available for a subset of the supported 
devices. The following is a list of such options.
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
On devices which support line width scaling all lines will be scaled
.I scale
times the default line width for that device. This option is subject to 
modification by the 
.BR -lmin " and " -lmax 
options.
.TP
.BI \-geom " geom"
.I geom 
is a string in the standard X11 format for describing the initial
size and location of a window. This options is only available with the
.B X11
and 
.B xbfr
device specifications. 
For example:
.sp
.in +0.5i
% 
.BI "ictrans -d X11 -geom 800x800+0-0 " metafile
.sp
or
.sp
%
.BI "ictrans -d xbfr -geom 800x800" " metafile" " >" " xwdfile"
.sp
.in -0.5i
The first example would open a window 800 by 800 pixels in dimension 
in the lower left
corner of the screen where 
.I metafile 
would be displayed. The second example would generate a X11 raster file
at a resolution of 800 by 800 pixels and store it in the file 
.IR xwdfile .
.TP
.BI \-Ws " width height"
.br
.I width 
and 
.I height 
are the dimension in pixels of a window created with
the 
.B sunview
device or the resolution of a raster file created with the 
.B sunraster 
device.
.TP
.BI \-Wp " x y"
.B x
and 
.B y
specify the x and y coordinates of the window created with the 
.B sunview
device.
.TP
.B \-Data
Suppress display of  CGM  output  primitive  data when performing a 
clear text translation using the 
.B CTXT
device.  All
other  CGM element data is displayed. This may substantially reduce 
the verbosity of the clear text driver.
.TP
.B \-Para
Suppress display of CGM element data when performing a clear text
translation using the 
.B CTXT 
device except for  output
primitives.  The  
.B -Data  
combined with the 
.B -Para 
option permit the display of only the CGM element names.
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
For example,
.B 1,5 8 save
.I /tmp/foo
means write the first through fifth and the eighth frame of the metafile
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
.I name
is not allowed to be
.BR alias.
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
ictrans> alias \fIname1\fP : -d xbfr : | cat > \fIoutfile\fR
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
.B device
.BI [ " device name " ]
.br
Set the translation device to 
.IR "device name" .
With out an argument 
.B device
reports the name of the current device for metafile translation.
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
for future translation. If 
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
.B loop
.br
Toggle loop mode on or off. When loop mode is set a
.B plot
command will cause the requested frames to be plotted and then ictrans 
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
seconds during subsequent plots. If
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
Plotting will be terminated and ictrans will reenter command mode after
the last frame is plotted or upon receiving a interrupt signal, SIGINT.
.HP 
.BI [ " frames " ] 
.B Print
.br
The addressed frames are translated and sent to the current spooling device.
Translation is performed by a spawned translator. The 
.B spooler
command may be used to select a spooling device.
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
.B skip
.BI [ " number " ]
.br
Set number of frames to skip over during subsequent plotting. For example,
if "skip" is set to 2 and a request is made to plot frames 1 through 10
frames 1, 3, 5, 7, and 9 will be displayed. With no arguments 
.B skip
reports its current value. The default is one.
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
.SH FILES
.PD 0
.TP 28
.B /usr/local/lib/ncarv_spool
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
.BR ctrans(l),
.BR fcaps(l),
.BR gcaps(l),
.BR ncarv_spool(l)
.SH CAVEATS
Metafile frames written to an existing file via the 
.B save
command will be subject to the effects of any global "attribute elements"
contained within the file.
