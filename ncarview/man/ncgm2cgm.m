.\"
.\"	$Id: ncgm2cgm.m,v 1.16 2000-08-22 15:00:54 haley Exp $
.\"
.\"	ncgm2cgm.l 3.00 10/5/89 NCAR View
.TH NCGM2CGM 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
cgm2ncgm ncgm2cgm \- filter \fBNCAR CGM\fR to/from vanilla \fBCGM\fR
.SH SYNOPSIS
.B ncgm2cgm
[\ \fB\-s\ \fIoutput block size\fR\ ]
[\ \fB\-V\ ]
.LP
.B cgm2ncgm
[\ \fB\-s\ \fIinput block size\fR\ ]
[\ \fB\-V\ ]
.PP
.SH DESCRIPTION
\fBncgm2cgm\fR and \fBcgm2ncgm\fR are filters for converting back and forth
between \fBNCAR Computer Graphics Metafile\fR (CGM) and vanilla \fRCGM\fR.
\fBncgm2cgm\fR strips the wrapper from NCAR CGM records while \fRcgm2ncgm\fR
restores it. I/O is done from standard in/out respectively.
.PP
.SH OPTIONS
.IP \fB\-s\fP\fI\ size\fP
Set block size in bytes for reads/writes of vanilla CGM. The default blocking
factor is 1024.
.IP \fB\-V\fR
Print the version number and then exit.
.SH SEE ALSO
.nf
\fIISO/DIS 8632 CGM Functional Specification\fR (Nov. 1985)
.fi
.sp
.SH BUGS
Some 
.I vanilla
metafile interpretors take advantage of shortcuts provided by their CGM
generators. In effect, they are not
.I true
CGM interpretors. Although it will still be possible to convert these metafiles
to NCAR format that may be interpreted by NCAR Graphics translators. It may
not be the case that the aforementioned interpreters will be able to translate
.I true
vanilla metafiles, such as produced by 
.B ncgm2cgm.
.SH COPYRIGHT
Copyright (C) 1987-2000
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

