.TH RASLS 1NCAR "January 1992" NCAR "NCAR VIEW"
.SH NAME
rasls \- list information on rasterfiles
.SH SYNOPSIS
\fBrasls\fP 
[\fb\-type\fR]
[\fb\-count\fR]
[\fb\-debug\fR]
[\fB\-verbose\fR]
[\fB\-Version\fR]
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
.sp
.SH "SEE ALSO"
.LP
ras_formats(5), rasview(1), rascat(1)
