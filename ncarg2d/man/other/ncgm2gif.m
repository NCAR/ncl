.\"
.\"	$Id: ncgm2gif.m,v 1.4 2000-08-22 04:16:27 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCGM2GIF 1NCARG "February 1997" NCAR "NCAR GRAPHICS"
.SH NAME
ncgm2gif \- Converts NCGMs to GIFs
.SH SYNOPSIS
\fBncgm2gif\fP 
[\fB\-i\fR]
[\fB\-res WxH\fR]
[\fB\-loop N\fR]
[\fB\-delay N\fR]
\fBNCGM\fR
.SH DESCRIPTION
.I ncgm2gif
converts a single/multi-frame NCGM to a single-frame/animation GIF file.
\fIncgm2gif\fP first converts the NCGM to a raster file, and then
uses public domain software to convert it to GIF.  The public
domain software required for this script include \fIImage Tools\fP
(file://ftp.sdsc.edu/pub/sdsc/graphics/imtools/) and \fIgifmerge\fP
(http://www.iis.ee.ethz.ch/~kiwi/GIFMerge/).
.sp
In order to run \fIncgm2gif\fP, you must have your NCARG_ROOT
environment variable set to the parent directory where the NCAR
Graphics libraries, binaries, and include files were installed, and
have $NCARG_ROOT/bin on your search path.  You must also be sure that
the commands \fIimconv\fP and \fIgifmerge\fP can be found on your
search path.
.sp
.SH OPTIONS
.sp
.IP "\-res WxH"
Indicates the desired resolution in pixels. The default is 500x500.
.sp
.IP "\-i "
Prompts user whenever a file might be overwritten. The default is to
overwrite files.
.sp
.IP "\-loop N"
For the creation of an animation GIF file, N indicates how many
times you want the GIF animation to be run beyond the initial run.
The default is no looping.
.sp
.IP "\-delay N"
If looping is turned on, then N indicates the delay between frames in
units of 1/100th of a second.  The default is N=50 (0.5 seconds
between frames).
.sp
.SH EXAMPLE USAGES
.sp
To convert a single-frame NCGM called "gmeta" to a GIF file:
.sp
ncgm2gif gmeta
.sp
To convert a multi-frame NCGM called "example.ncgm" to an animation
GIF file that is 200x200 pixels:
.sp
ncgm2gif -res 200x200 example.ncgm
.sp
To convert a multi-frame NCGM called "example.ncgm" to a multi-
framed GIF file, and have the animation run 4 times with a delay
of 20/100 seconds between each frame:
.sp
ncgm2gif -loop 3 -delay 20 example.ncgm
.sp
.fi
.SH SEE ALSO
.BR ncgm2mpeg(1NCARG),
.BR ctrans(1NCARG)
.sp
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
