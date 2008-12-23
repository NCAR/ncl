.\"
.\"	$Id: ctrans.m,v 1.36 2008-12-23 00:04:15 haley Exp $
.\"
.\" ctrans 3.2 
.TH CTRANS 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
ctrans \- a Computer Graphics Metafile ( \fICGM\fR ) translator
.SH SYNOPSIS
.B ctrans
[
.B \-bell
] [
.BI \-d " device"
] [
.BI \-f " font"
] [
.BI \-lmin " min" 
] [
.BI \-lmax " max" 
] [
.BI \-lscale " scale" 
] [
.BI \-movie " time" 
] [
.BI \-outfile " file" 
] [
.BI \-pal " pal_fname" 
] [
.B \-pause
] [
.B \-quiet
] [
.BI \-record " record_num ..."
] [
.B \-soft
] [
.B \-verbose
] [
.B \-Version
] [
.BI \-viewport " llx:lly:urx:ury"
] [
.BI \-wid " window_id"
] [
.BI \-window " llx:lly:urx:ury"
] [
.I device\-specific options
] 
[
.B - 
| 
.I metafile ...
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
by default, 
see
.BR graphcap(5NCARG),
while providing optional processing
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
.B X11 
under release 4 and 5, version 11 of 
.I X.
.LP
.B ctrans
can also translate metacode into the following raster formats: 
.B a60, avs, hdf, hppcl, nrif, sun
and
.BR xwd .
The device specifier for these raster
formats is the name of the format. For example
"-d xwd" specifies translation to an xwd formatted raster file.
Additionally, a clear text driver, "-d CTXT",
is available on any terminal. 
Not all of the aforementioned devices
may be supported by your particular configuration of 
.BR ctrans .
For a list of supported devices see the
.BR gcaps(1NCARG)
command.
.PP
.B ctrans
will read from the standard input if no metafile name is specified or the
the name specified is `-'.
.PP
.SH OPTIONS
.TP
.B \-bell
Ring the bell at the end of each frame. The default is to run in silent mode.
This option is not supported by all devices.
.TP
.BI \-d " device"
Device name.
.B ctrans
will use the 
.I Graphcap
(if it exists) or the appropriate graphics library indicated by 
.I device;
.IP
If 
.I device
is preceded by a UNIX directory path then 
.B ctrans
will look in that directory for the specified graphcap. Otherwise 
.B ctrans
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
may also be used to specify a default fontcap.
.IP
If 
.I fontcap
is preceded by a UNIX directory path then 
.B ctrans
will look in that directory for the specified fontcap. Otherwise 
.B ctrans
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
This option is subject to modification by the 
.BR -lmin " and " -lmax 
options.
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
This option and the
.B \-pause 
option are mutually exclusive.
.LP
This option may not behave as expected on slower devices.
.TP
.BI \-outfile " file"
Direct translator output to 
.IR file .
By default translator output is written to the standard output. This option
has no effect for devices of which 
.B ctrans
has a function-callable interface. e.g. 
.B X11 .
.TP
.BI \-pal " pal_fname"
Use the color palette defined in the file
.I pal_fname
for subsequent translation of the metafile. This palette will override any 
color map defined by the CGM being translated. For a description of 
the format of 
.I pal_fname
see 
.BR ras_palette(5NCARG) .
.TP
.B \-pause
Pause after each frame in the metafile is displayed and wait for the
user to type a newline before proceding. This option is probably only
useful when used in conjunction with the 
.B \-wid 
option as this is the normal behaviour for 
.B ctrans
in most instances.
This option and the
.B \-movie 
option are mutually exclusive.
.TP
.B \-quiet
Suppress reporting of non-fatal (warning) error 
messages; only fatal error messages are
reported.
.TP
.B -record 
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
.BR ictrans(1NCARG) .
may be used as the actual user interface to perform this
bookkeeping. Without a specified 
.I record number,
.B ctrans
processes the entire metafile.
.TP 
.B \-soft
Unconditionally perform software filling of all filled polygons. This
option may be useful for devices which do not support the filled 
polygon drawing 
primitive or have limits on the number of
vertices describing a polygon. On some devices this number is known and
software filling is performed, as appropriate, without user specification.
.TP
.BI \-verbose
Operate in verbose mode.
.TP
.BI \-Version
Print the version number and then exit.
.TP
.BI \-viewport " llx:lly:urx:ury"
Set the viewport of the output device. The viewport is the rectangular
region of the output device of which the virtual device coordinate
system of the metafile is mapped onto. Normally this region is the largest
device-addressable square which fits in the center of the device address
space. The 
.B \-viewport 
option may be used to change the default mapping. 
.IR llx " and " lly
specify the lower left corner of the device in normalized coordinates.
.IR urx " and " ury
specify the upper right corner of the device in normalized coordinates.
For example, \fB-viewport 0.0 0.0 0.5 0.5\fR, specifies the lower left 
corner of the device. 
.TP
.BI \-window " llx:lly:urx:ury"
Specify the workstation window (in the GKS sense). Four
coordinates are specified
which define a rectangular window which is a subset of the normalized VDC
rectangle with corner points (0,0) and (1.0,1.0). 
.I llx
and
.I lly
specify the lower left corner.
.I urx
and
.I .ury 
specify the upper right corner.
The specified window
is mapped onto the entire display viewport. For example, 
if the workstation
window is defined by the corner points (0,0) and (0.5 0.5) then the lower
left quarter of a plot would be blown up to fill the entire viewport.
Specification of such a window can be used for zooming and panning.
.IP
The range with which one may zoom in on a plot may be limited by the
integer addressing precision of the device.
.IP
.PP
.SH DEVICE-SPECIFIC OPTIONS:
.PP
The following options are available when the device is graphcap-driven (See
the 
.BR gcaps(1NCARG)
command for a list of graphcap-driven devices):
.TP
.B -simulatebg
Simulate CGM background color requests by drawing a large filled rectangle 
of the appropriate color. This option is useful for devices such as
color PostScript printers which have no concept of background color.
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
.BR X11 :
.TP 
.BI \-background " color"
Specifies the default window background color for color devices. If the 
metafile explicitly sets color index 0 then this option is overridden.
.TP
.BI \-foreground " color"
Specifies the default foreground color for color devices. If the metafile 
explicitly sets color index 1 then this option is overridden.
.TP 
.BI \-geometry " geometry"
Specify the size and/or position of the graphics window in the format
of an 
.I X11 Window System 
geometry string.
.TP 
.B \-ignorebg
Ignore requests to change the background color. This option may be useful
when 
.B ctrans
renders into a X window created by an application other than 
.BR ctrans .
As a side effect of this option the rendering window 
is not cleared between frames.
.TP
.B \-reverse
On monochrome devices reverse video is simulated by swapping the foreground
and background colors.
.TP
.BI \-wid " window_id"
Render into the previously created X window specified by
.IR window_id .
Normally 
.B ctrans
creates its own window for plotting. The window specified by 
.I window_id
must be of type 
.BR InputOutput .
The window must also have inherited its color map, depth and visual class from
the root window. 
.IP
Note also that when this option is used 
.B ctrans
cannot receive X events from the drawing window. Hence, 
.B ctrans 
cannot use
"mouse clicks" as a signal to advance frames. For this reason the
.B -pause
option is useful to prevent 
.B ctrans
from processing the entire metafile without pausing between frames.
.IP
.I window_id 
may be specified as a decimal or hexidecimal integer.
.PP
The following options apply to the X11 color map management of
.B ctrans
when
.I device
is X11:
.IP
.B ctrans
supports three different methods of X11 color map management.
.IP
If the
user specifies a shared color map (using the
.B \-scmap
option), then
.B ctrans
will use the default X color map for the screen, that is shared by all
applications.  If the metafile contains more colors than there are
available in the default X color map, then a color matching algorithm
is employed.  The idea of the algorithm is that the color in the
current color table that is
.I closest
to the requested color will be selected.
.I Closest
is defined in terms of the normal distance metric on the RGB cube.  If
the closest color is equal to or farther away than the percentage error
allowed (
.B \-colerr
), then a warning message will be printed.  The closest color is still
used.
.IP
If the
user specifies a private color map (using the
.B \-pcmap
option), then
.B ctrans
will create a private color map for the graphics window.  This will guarantee
that 256 distinct colors are available to the window.  This means that
the  X window will have a different color map than all the other windows on
the screen. Therefore, you usually have to have the mouse pointer in the
window for the correct color table to be installed.  One disadvantage to
this option is that there is usually a color flashing effect on the
screen since the wrong color table will be installed for the other windows
on the screen.
.IP
The default color map management scheme attempts to take the best of
the two previous models.  It starts out
behaving like the shared model, in that it uses the default color map
for the screen.  It differs in that, once it can't allocate any more colors
from the default color map, in allocates its own private color table and
starts using it.  This way, the color flashing is only present if
it absolutely needs to be so that
.B ctrans
can display the correct color.
.TP 
.B \-scmap
Ask
.B ctrans
to use the shared default X color map only.
.IP
This is the option used if
.B \-wid
is specified.
.TP
.BI \-colerr " n"
Specifies the percentage color error that is acceptable if the
.B \-scmap
option is being used.  If the color being used is
.I n
percentage or more different from the color requested, a warning will be
reported by
.B ctrans.
.TP 
.B \-pcmap
Ask 
.B ctrans 
to create its own X color map and use it exclusively.
.IP
This option is ignored if the 
.B \-wid 
option is present.
.PP
The following options are available when 
.I device 
is 
.B a60, avs, hdf, hppcl, nrif, sun, 
or 
.BR xwd :
.TP
.BI \-dpi " dpi"
Specify the number of dots per inch. This option is only meaningful
for the HP LaserJet,
.BR hppcl ,
which ignores the
.B \-resolution 
option. 
.I dpi
may be one of 75, 100, 150, or 300.
The default is 150.
.TP
.B \-direct
By default
.B ctrans
outputs raster imagery with 8-bit-indexed encoding. When this option
is used, if the raster file format supports it, raster imagery is output
in a 24-bit-direct encoding scheme. Be warned: the resultant file is
three times the size of its 8-bit-indexed counterpart.
.TP
.B \-landscape
Generate the image in landscape mode. This option is ignored by all 
raster devices except the HP LaserJet,
.BR hppcl .
By default the LaserJet uses portrait mode.
.TP
.BI \-resolution " width" "x" "height"
.I width
and
.I height
specify the spatial resolution in pixels of the raster file to be created.
The default is 512x512.
.SH EXAMPLES
.PP
To process a metafile named
.B gmeta
and display its contents on the
TEKTRONIX 4107 terminal, use the following call:
.sp
.ti +0.5i
% 
.B ctrans -d t4107 gmeta
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
.B "ctrans -record 3 -d t4107 gmeta"
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
.B "ctrans -d xwd -res 1024x1024 > raster.xwd"
.sp
.br
The raster output is in X11 "xwd" format and is sent to the file
.BR raster.xwd .
.PP
To zoom in on the upper right quarter of the metafile 
.B gmeta
and display it in an X window, call:
.sp
.ti +0.5i
% 
.B "ctrans -d X11 -window 0.5:0.5:1.0:1.0"
.sp
.br
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
.SH FILES
.IP $NCARG_ROOT/lib/ncarg/graphcaps/* 30
The binary NCAR Graphcap files
.IP $NCARG_ROOT/lib/ncarg/fontcaps/* 30
The binary NCAR Fontcap files
.SH SEE ALSO
.BR fcaps(1NCARG), 
.BR fontcap(5NCARG), 
.BR gcaps(1NCARG), 
.BR graphcap(5NCARG), 
.BR idt(1NCARG), 
.BR ras_palette(5NCARG), 
.BR med(1NCARG), 
.BR ictrans(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH CAVEATS
Running in "movie" mode may give surprising results on slower devices, 
such as dumb terminals. If too short a time interval is specified slow devices
may not have finished rendering before the movie timer expires. This results
in no pause between frames.  
.PP
Metafiles which reference color table indices that were not previously 
defined may have varying results from one device to the next.
.PP
Using the 
.B \-wid 
option to have 
.B ctrans 
display its output in a window created by another X application may
produce unexpected results, particularly with regard to color.
.PP
At
.B ctrans'
current level of implementation, the subset
of CGM elements 
supported is closely approximated by the list provided in
.B NCAR's
.I Graphics Installer's Guide,
Version 2.00 (August 1987).
However, the best way to determine whether a particular CGM element
is supported by the translator is feed a metafile containing the element
in question to 
.BR ctrans .
Consult the aforementioned publication for a discussion of 
Graphcaps and Fontcaps as well.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
