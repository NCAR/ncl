.\"
.\"	$Id: ncargcc.m,v 1.2 1993-03-02 00:03:59 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCC 1NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcc \- Command for compiling C code that uses NCAR Graphics
.SH SYNOPSIS
\fBncargcc\fP 
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
[\fB\-noX11\fR]
.SH DESCRIPTION
.sp
\fIncargcc\fP is a script that invokes the C compiler/linker (cc) with the
proper NCAR Graphics libraries.  Arguments presented above are
associated with NCAR Graphics.  All other arguments and options
are identical to the cc command on your particular machine;
arguments that include quoted strings may
have to be enclosed in single quotes.
.sp
When \fBncargcc\fR is invoked with the \fB\-ictrans\fR option the resulting
executable will, upon invocation, send its metafile output to the translator
\fBictrans\fR . The environment variable GRAPHCAP must be set to a valid
graphics output device whenever the executable is executed.
.sp
By default, \fIncargcc\fR will load in the X library when linking your C
program.  If you do not have the X library, or else you just don't want
to link it in, you can use the \fB\-noX11\fR option.
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
