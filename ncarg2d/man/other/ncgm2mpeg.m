.\"
.\"	$Id: ncgm2mpeg.m,v 1.4 2000-08-22 04:16:27 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCGM2MPEG 1NCARG "February 1997" NCAR "NCAR GRAPHICS"
.SH NAME
ncgm2mpeg \- Converts NCGMs to MPEGs
.SH SYNOPSIS
\fBncgm2mpeg\fP 
[\fB\-i\fR]
[\fB\-res WxH\fR]
\fBNCGM\fR
.SH DESCRIPTION
.I ncgm2mpeg
converts a multi-frame NCGM to an MPEG file.  \fIncgm2mpeg\fP first
converts the NCGM to a raster file, then to a PPM file, then to YUV
files, and finally to an MPEG. \fIncgm2mpeg\fP uses public domain
software to do the various conversions.  The public domain software required
includes \fIrasttopnm\fP
(http://www.arc.umn.edu/GVL/Software/pbmplus-ftp.html)),
\fIppm2cyuv\fP (ftp://havefun.stanford.edu/pub/cv/)), and \fImpeg\fP
(ftp://ftp.arc.umn.edu/pub/GVL)).
.sp
In order to run \fIncgm2mpeg\fP, you must have your NCARG_ROOT
environment variable set to the parent directory where the NCAR
Graphics libraries, binaries, and include files were installed, and
have $NCARG_ROOT/bin on your search path.  You must also be sure that
the commands \fIrasttopnm\fP, \fIppm2cyuv\fP, and \fImpeg\fP can be
found on your search path.
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
.fi
.SH SEE ALSO
.BR ncgm2gif(1NCARG),
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
