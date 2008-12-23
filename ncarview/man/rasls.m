.\"
.\"	$Id: rasls.m,v 1.20 2008-12-23 00:04:16 haley Exp $
.\"
.TH RASLS 1NCARG "January 1993" NCARG "NCAR VIEW"
.SH NAME
rasls \- list information on rasterfiles
.SH SYNOPSIS
\fBrasls\fP 
[\fB\-type\fR]
[\fB\-count\fR]
[\fB\-resolution\fR]
[\fB\-verbose\fR]
[\fB\-Version\fR]
[\fB\-ifmt \fIfile_format\fR]
[files]
.SH DESCRIPTION
.sp
For each file \fBrasls\fP will generate a one line
description of the rasterfile. Information provided includes
the format type (Sun, HDF, etc.), the encoding (indexed, direct, etc.),
resolution, and filename. Files that cannot be read as rasterfiles,
are empty, are directories, or are otherwise unrecognizable
are simply listed as such.
.sp
.SH OPTIONS
.TP
.BI \-type
Print the encoding type of the rasterfile. Currently this will be
either \fIindexed\fP or \fIdirect\fP.
.TP
.BI \-count
Print the number of frames in the raster file.
.TP
.BI \-resolution
Print the spatial resolution of a file in pixels.
.TP
.BI \-help
Print help information.
.TP
.BI \-verbose
Print as much information as possible about each rasterfile. Information
provided is a combination of format-independent and format-dependent
parameters.
.TP
.BI \-Version
Print the version number.
.TP
.BI \-ifmt " format"
Specify the input file format.
.I format
is one of the file name extensions discussed
in
.B ras_formats(5NCARG)
(without the ".", e.g.
.BR xwd).
When this option is
specified file name extensions are not necessary and are ignored if present.
All input files must have the same format.
.sp
.SH "SEE ALSO"
.LP
\fBrasview\fP(1NCARG), \fBrascat\fP(1NCARG), \fBrasgetpal\fP(1NCARG),
\fBrasls\fP(1NCARG), \fBrassplit\fP(1NCARG),
\fBras_formats\fP(5NCARG), \fBras_palette\fP(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
