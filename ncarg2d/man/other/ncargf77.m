.\"
.\"	$Id: ncargf77.m,v 1.1 1993-03-11 15:23:03 haley Exp $
.\"
.TH NCARGF77 1NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargf77 \- Command for compiling f77 code that uses NCAR Graphics
.SH SYNOPSIS
\fBncargf77\fP 
[\fB\-smooth\fR]
[\fB\-quick\fR]
[\fB\-super\fR]
[\fB\-agupwrtx\fR]
[\fB\-dashchar\fR]
[\fB\-dashline\fR]
[\fB\-dashsmooth\fR]
[\fB\-dashsuper\fR]
[\fB\-conrecsmooth\fR]
[\fB\-conrecsuper\fR]
[\fB\-conrecquick\fR]
[\fB\-conran\fR]
[\fB\-conransmooth\fR]
[\fB\-conranquick\fR]
[\fB\-ictrans\fR]
[\fB\-noX11\fR] ...
.SH DESCRIPTION
\fIncargf77\fP is a script that invokes the FORTRAN 77 
compiler/linker (f77) with the proper NCAR Graphics libraries.  
Arguments presented above are associated with NCAR Graphics.  
All other arguments and options are identical to the f77 command 
on your particular machine;
arguments that include quoted strings may
have to be enclosed in single quotes.
.sp
Note that, on some systems, if you supply your own binary libraries in
addition to the ones automatically referenced by \fIncargf77\fR, all the
libraries must have been created in a similar fashion.  For example,
on a "Sun3", problems may arise if the "-fswitch" option was used for
some of the libraries and not for others.
.sp
When \fIncargf77\fR is invoked with the \fB\-ictrans\fR option the resulting
executable will, upon invocation, send its metafile output to the translator 
\fBictrans\fR  . The environment variable GRAPHCAP must be set to a valid
graphics output device whenever the executable is executed.
.sp
By default, \fIncargf77\fR will load the X library when linking
your Fortran program.  If you try running \fIncargf77\fR and the
compiler complains that it cannot find the library for -lX11, then try
running \fIncargf77 -L/xxx/yyy/zzz program.f\fP where \fI/xxx/yyy/zzz\fP 
is the path leading to your X library.  If you do not have the X library,
or else you just don't want it to be loaded, you can use the \fB\-noX11\fR
option.
.SH SEE ALSO
Online:
ictrans(1NCARG), gcaps(1NCARG), ncargcc(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
