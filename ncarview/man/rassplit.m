.\"
.\"	$Id: rassplit.m,v 1.1 1993-01-16 03:54:39 don Exp $
.\"
.TH RASSPLIT 1NCARG "January 1993" NCARG "NCAR VIEW"
.SH NAME
rassplit \- split a multi-frame rasterfile into single-frame rasterfiles.
.SH SYNOPSIS
\fBrassplit\fP 
[\fb\-f f1 f2 f3 ...\fR]
[\fb\-ofmt output-format\fR]
[\fB\-verbose\fR]
[\fB\-Version\fR]
\fIrasterfile\fP
.SH DESCRIPTION
.sp
The program \fBrassplit\fP will open \fIrasterfile\fP and copy each frame
to it's own separate output file. By default, all frames are
extracted but the user can optionally request a specific
set of frames using the \fB-f\fP option. The output format
defaults to be the same as the input format, but the user can
also optionally select a new output format using the \fB-ofmt\fP option.
.sp
The names of the output rasterfiles are derived from the name
of the source rasterfile, the output format, and
the frame numbers. The examples given below will demonstrate
this.
.SH OPTIONS
.TP
.BI \-f " f1 f2 f3 ..."
This option allows the user to specify a list of frames
to be extracted from the input rasterfile.
.TP
.BI \-ofmt " output-format"
The user can specify \fIoutput-format\fP to be the desired format e.g.
\fBhdf\fP, \fBrgb\fP, etc.
.TP
.BI \-help
Print help information.
.TP
.BI \-verbose
Print a message each time an output rasterfile is written.
.TP
.BI \-Version
Print the version number.
.sp
.SH "CAVEATS"
If you're splitting an HDF file, ask for a different output
format. Otherwise you'll just get the first frame over and
over again.
.sp
.SH "SEE ALSO"
.LP
ras_formats(5NCARG), rasview(1NCARG), rascat(1NCARG), rasls(1NCARG),
rasdiff(1NCARG), rasgetpal(1NCARG)
