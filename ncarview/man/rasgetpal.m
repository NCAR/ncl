.\"
.\"	$Id: rasgetpal.m,v 1.1 1993-01-16 03:54:38 don Exp $
.\"
.TH RASGETPAL 1NCARG "January 1993" NCARG "NCAR VIEW"
.SH NAME
rasgetpal \- extract the color palette of a rasterfile and write it
to standard output
.SH SYNOPSIS
\fBrasgetpal\fP 
[\fB\-verbose\fR]
[\fB\-Version\fR]
\fIrasterfile\fP
.SH DESCRIPTION
.sp
The \fBrasgetpal\fP program will extract the color palette from
\fIrasterfile\fP and print it in textual form to standard output.
You can direct the output to a file, edit it in an editor, and then
feed it back to \fIctrans\fP or \fIrasview\fP in order to get a
modified color palette. It's also useful when you just want to know
something about the color palette.
.SH OPTIONS
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
.SH "EXAMPLE"
.LP
Let's suppose you have an X Window Dump rasterfile called \fIwindow.xwd\fP
and you'd like to get a copy of the color palette.
.sp
.in +3.0
.nf
rasgetpal window.xwd >window.txt
vi window.txt /* edit the color table */
rasview -pal window.txt window.xwd
.fi
.in -3.0
.sp
.SH "CAVEATS"
A color map can be extracted from indexed rasterfiles but not
from direct-color rasterfiles.
.sp
.SH "SEE ALSO"
.LP
ras_formats(5NCARG), rasview(1NCARG), rascat(1NCARG), rasls(1NCARG),
rassplit(1NCARG)
