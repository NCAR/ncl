.\"
.\"	$Id: ncgm2cgm.m,v 1.22 2008-12-23 00:04:16 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
