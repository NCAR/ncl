.\"
.\"	$Id: ctrans.m,v 1.3 1991-01-09 10:47:08 clyne Exp $
.\"
.\" ctrans 3.01 90/06/22
.TH CTRANS 1NCARV "22 June 1990" NCAR "NCAR View 3.01"
.SH NAME
ctrans \- a Computer Graphics Metafile ( \fICGM\fR ) translator
.SH SYNOPSIS
.B ctrans
[
.BI \-d " device"
] [
.BI \-f " font"
] [
.BI \-movie " time" 
] [
.BI \-r " record_num ..."
] [
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
.I device\-specific options
] 
[
.BI metafile " ..."
]
.SH DESCRIPTION
.B ctrans
is a metafile translator, taking 
.I metafile(s),
a metafile stored in the
.B NCAR Computer Graphics Metafile (CGM)
standard, and interpreting its instructions
on the device defined by the GRAPHCAP environment
variable. Fonts are stroked according to specifications in the Fontcap
file defined by the FONTCAP environment variable.
.B ctrans
utilizes 
.I Graphcaps
by default, while providing optional processing
by user
provided libraries, if that is required by the device or desired by
the user.
Thus, 
.B ctrans
is capable of driving
any device for which a 
.I Graphcap
is available; with  programming
modifications,
.B ctrans
can accommodate any device for which an external library
of plotting routines is available. 
Currently, the following
.I Graphcap
independent devices are supported:
.B sunview,
under Sun's
.I Sunview; 
.B sunraster,
a Sun rasterizer under Sun's 
.I Sunview;
.B X11, 
under release 3 and 4, version 11 of 
.I X;
and
.B xbfr, 
a X11 rasterizer under X.
Additionally, a clear text driver,
[\ \fB\-d\ CTXT\fR\ ],
is available on any terminal. 
Not all of the aforementioned devices, 
.B (sunview, sunraster, X11, xbfr, 
and 
.BR CTXT) ,
may be supported by your version of 
.BR ctrans .
For a list of supported devices make sure
GRAPHCAP
is not set and invoke
.B ctrans
without any arguments.
.LP
Consult the
.I Ctrans Reference Manual
(May 1988)
for details regarding customizing you own driver.
.PP
.SH OPTIONS
.TP
.BI \-d " device"
Device name.
.B ctrans
will use the 
.I Graphcap
(if it exists) or appropriate graphics library indicated by 
.I device;
else, it
will default to the device defined by the GRAPHCAP environment variable.
For all device specifications
except
.B X11
and
.BR sunview ,
output is directed to standard out. In the case of
.B X11
and
.B sunview
translation results in appropriate calls to the X11 and Sunview
libraries respectively.
If no GRAPHCAP environment variable is defined, and the 
.I device
option is not used,
.B ctrans
will terminate processing with a report of the supported devices.
.TP
.BI \-f " fontcap"
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
may be used to specify a default fontcap.
.TP
.BI \-movie " time"
Set pause to 
.I time
seconds. In normal operation mode the translator
requires user interaction after the display of each plot. 
.B ctrans 
will not proceed until the user responds.  If 
.B movie
mode is set 
.B ctrans
will wait
.I time
seconds after the display of each frame and then proceed automatically.
.TP
.B -r 
< 
.I "record_number... " 
>
.br
If processing only single frames of the metafile is desired,
this option specifies the 
.I record number
containing
the start of that frame.
.B ctrans
assumes the processing is to start at the
first
.B BEGIN PICTURE
element in that record.  The user must
perform bookkeeping to determine the record that contains
the desired frame.  Normally, a metafile editor (e.g.,
.B ictrans)
may be used as the actual user interface to perform this
bookkeeping. Without a specified 
.I record number,
.B ctrans
processes the entire metafile.
.TP 
.B \-soft
Unconditionally perform software filling of all filled polygons. This
option may be useful for devices which have limits on the number of
vertices describing a polygon. On some devices this number is known and
software filling is performed, as appropriate, without user specification.
.B \-bell
Turn off bell. The default is to bell between plotting of frames.
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
.PP
.SH DEVICE-SPECIFIC OPTIONS:
.PP
The following options are available when 
.I device
is 
.B CTXT:
.TP 
.B \-Data
Suppress display of 
.I CGM output primitive
data. All other 
.I CGM
element data is displayed. This may substantially reduce the verbosity of 
the clear text driver. 
.TP 
.B \-Para
Suppress display of 
.I CGM 
element data except for 
.I output primitives.
The 
.B \-Data 
combined with the 
.B \-Para
option permit the 
display of only the CGM element names.
.PP
The following options are available when 
.I device
is 
.B X11 
or
.BR xbfr :
.TP 
.BI \-geometry " geometry"
Specify the size and/or position of the graphics window in the format
of an 
.I X11 Window System 
geometry string.
.PP
The following options are available when 
.I device 
is 
.B sunview 
or 
.BR sunraster :
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
device. This option is meaningless with the
.B sunraster
device.
.PP
Unknown options are ignored.
.PP
At
.B ctrans'
current level of implementation, the subset
of CGM elements 
supported is identical to that listed in
.B NCAR's
.I Graphics Installer's Guide,
Version 2.00 (August 1987).
Consult this publication also for a discussion of Graphcaps and Fontcaps.
.B ctrans
is written in C; a Fortran
.I Graphcap
version also exists.
.SH EXAMPLES
.PP
To process a metafile named
.B gmeta
and display its contents on the
TEKTRONIX 4107 terminal, use the following call:
.sp
.ti +0.5i
% ctrans -d t4107 gmeta
.br
.PP
If this device is already defined by the GRAPHCAP environment variable,
simply call:
.sp
.ti +0.5i
% 
.B "ctrans gmeta"
.br
.PP
If you wish to display only the first 
frame starting in the third record, call:
.sp
.ti +0.5i
% 
.B "ctrans -r 3 -d t4107 gmeta"
.br
.PP
To examine the metafile
.B gmeta's
contents without 
.B CGM
element data being displayed:
.sp
.ti +0.5i
% 
.B "ctrans -d CTXT -Data -Para gmeta"
.PP
To render the metafile 
.B gmeta 
(under X Windows) in a window that is 512x512 pixels in
dimension in the lower right corner of your screen
.sp
.ti +0.5i
%
.B "ctrans -d X11 -geometry 512x512-0-0 gmeta"
.PP
To rasterize the contents of the metafile  
.B gmeta
at a resolution of 1024x1024 pixels, call:
.sp
.ti +0.5i
% 
.B "ctrans -d xbfr -geom 1024x1024 > xwdfile"
.sp
.br
The raster output is in X11 "xwd" format and is sent to the file
.BR xwdfile .
.SH FILES
.nf
/usr/local/lib/graphcaps/*           The binary NCAR Graphcap files
/usr/local/lib/fontcaps/*            The binary NCAR Fontcap files
.fi
.SH SEE ALSO
.BR cgmtrans(1NCARG), 
.BR fcaps(1NCARG), 
.BR fontcap(1NCARG), 
.BR gcaps(1NCARG), 
.BR graphcap(1NCARG), 
.BR med(1NCARG), 
.BR ictrans(1NCARG),
.BR plt(1NCARG)
.SH CAVEATS
Running in "movie" mode may give surprising results on slower devices, 
such as dumb terminals. If too short a time interval is specified slow devices
may not have finished rendering before the movie timer expires. This results
in no pause between frames.  
.PP
Metafiles which reference color table indices that were not previously 
defined may have varying results from one device to the next.
