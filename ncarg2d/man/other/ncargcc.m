.\"
.\"	$Id: ncargcc.m,v 1.10 1999-03-23 16:53:37 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCC 1NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcc \- Command for compiling C code that uses the NCAR Graphics low-level
utilities
.SH SYNOPSIS
\fBncargcc\fP 
[\fB\-ngmath\fR]
[\fB\-smooth\fR]
[\fB\-quick\fR]
[\fB\-super\fR]
[\fB\-agupwrtx\fR]
[\fB\-noX11\fR] ...
.SH DESCRIPTION
.sp
\fIncargcc\fP is a script that invokes the C compiler/linker with the
proper NCAR Graphics LLU (low-level utility) libraries.  Arguments
presented above are associated with NCAR Graphics.  All other
arguments and options are identical to the cc command on your
particular machine; arguments that include quoted strings may have to
be enclosed in single quotes.
.sp
NOTE: \fIncargcc\fP cannot be used to compile NCAR Graphics C programs
that call the HLUs (high-level utilities).  You must use \fInhlcc\fP
instead.  See the \fInhlcc\fP man page for more information.
.sp
If you don't want to use \fIncargcc\fP, you can just type it on the
command line to see what gets included in the link line, and then you
can add this information to your own Makefile or script.  It is
important to note that you must define the macro \fINeedFuncProto\fP
in order for function prototyping to work correctly.
.sp
In order to run \fIncargcc\fP, you must have your NCARG_ROOT
environment variable set to the directory pathname where the NCAR
Graphics libraries, binaries, and include files were installed.  If
you are not sure what NCARG_ROOT should be set to, please check with
your system administrator or the site representative for NCAR
Graphics.  If the NCAR Graphics libraries, binaries, and include files
were not installed under one root directory, then you will need to set
the environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE
instead.  Please see "man ncargintro" for more information.
.sp
When \fBncargcc\fR is invoked with the \fB\-ictrans\fR option the
resulting executable will, upon invocation, send its metafile output
to the translator \fBictrans\fR. The environment variable GRAPHCAP
must be set to a valid graphics output device whenever the executable
is executed.
.sp
By default, \fIncargcc\fR will load the X11 library when linking your
C program.  If you try running \fIncargcc\fR and the compiler
complains that it cannot find the library for X11, then try running
\fIncargcc -L/xxx/yyy/zzz program.c\fP where \fI/xxx/yyy/zzz\fP is the
path leading to your X11 library.  If you do not have the X11 library,
or else you just don't want to link it in, you can use the
\fB\-noX11\fR option.
.sp
.I OPTIONS
.IP "\-ngmath"
Links in the NCAR Graphics ngmath library.
.sp
.IP "\-smooth"
Link in the "smooth" objects.
.sp
.IP "\-quick"
Link in the "quick" objects.
.sp
.IP "\-super"
Link in the "super" objects.
.sp
.IP "\-agupwrtx"
Link in the "agupwrtx" library.
.sp
.IP "\-noX11"
Do not link in the X library when linking the code.
.sp
By default, \fIncargf77\fR will load the X11 library when linking
your Fortran program.  This is because the GKS library now has an
X11 driver.  If you try running \fIncargf77\fR and the
compiler complains that it cannot find the library for X11, then try
running \fIncargf77 -L/xxx/yyy/zzz program.f\fP where \fI/xxx/yyy/zzz\fP 
is the path leading to your X11 library.  If you do not have the X11 library,
or else you just don't want it to be loaded, you can use the \fB\-noX11\fR
option.
.sp
.SH SEE ALSO
Online:
.BR
ictrans(1NCARG),
.BR gcaps(1NCARG),
.BR ncargf77(1NCARG),
.BR nhlcc(1NCARG),
.BR nhlf77(1NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy: NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-1999
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
