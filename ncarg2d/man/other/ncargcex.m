.\"
.\"	$Id: ncargcex.m,v 1.6 1994-05-24 13:46:06 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCEX 1NCARG "January 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcex \- NCAR Graphics C Examples
.SH SYNOPSIS
\fBncargcex\fP 
[\fB\-all, -A\fR]
[\f\B\-autograph\fR]
[\f\B\-bivar\fR]
[\f\B\-conpack\fR]
[\f\B\-ezmap\fR]
[\f\B\-gks\fR]
[\f\B\-labelbar\fR]
[\f\B\-plotchar\fR]
[\f\B\-scrolled_title\fR]
[\f\B\-softfill\fR]
[\f\B\-inter\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-noX11\fR]
[\fB\-onebyone\fR]
\fBname ...\fR
.SH DESCRIPTION
.I ncargcex
provides the user with access to C example source code that
uses the NCAR Graphics C-bindings.  Some of these examples correspond
with the Fortran examples that you can get with the \fIncargex\fP
command.  Not every Fortran example is included here, however.
These examples should be enough to get one started in learning
how to use the NCAR Graphics C-bindings.
.sp
In order to run \fIncargcex\fP, you must have your NCARG_ROOT
environment variable set to the directory pathname where the NCAR
Graphics libraries, binaries, and include files were installed.  If
you are not sure what NCARG_ROOT should be set to, please check with 
your system administrator or the site representative for NCAR Graphics.
If the NCAR Graphics libraries, binaries, and include files were not
installed under one root directory, then you will need to set the 
environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE instead.
Please see "man ncargintro" for more information.
.sp
\fIncargcex\fP copies the source code for the specified example(s)
into the current directory and then compiles, links,
and executes the example, leaving an NCGM file
with the same name as the example, suffixed with
".ncgm". An option allows you to request that
only the source code be copied to your directory,
without compilation, linking, or execution.
Another option allows you to request that only the
NCGM file be left in your directory and that all other files
created by \fIncargcex\fP be deleted.
The argument \fIname\fP may be
selected from the lists that appear below.
.sp
.I OPTIONS
.sp
.IP "\-all, \-A"
Generate all available examples.
.sp
.IP "\-autograph\fR"
Generate all autograph examples.
.sp
.IP "\-bivar\fR"
Generate all bivar examples.
.sp
.IP "\-conpack\fR"
Generate all conpack examples.
.sp
.IP "\-ezmap\fR"
Generate all ezmap examples.
.sp
.IP "\-gks\fR"
Generate all gks examples.
.sp
.IP "\-labelbar\fR"
Generate all labelbar examples.
.sp
.IP "\-plotchar\fR"
Generate all plotchar examples.
.sp
.IP "\-scrolled_title\fR"
Generate all scrolled examples.
.sp
.IP "\-softfill\fR"
Generate all softfill examples.
.sp
.IP \-clean " " ""
Remove everything but the ".ncgm" file.
.sp
.IP \-n " " ""
Specifies that the example should not be compiled, linked, or run.
.sp
.IP \-noX11 " " ""
Do not link in the X library when linking the selected examples.
.sp
.IP \-onebyone " " ""
Specifies that the selected examples should be generated one
at a time and viewed as they are generated.  This is intended for use during
testing of new releases at NCAR.
.sp
.I "EXAMPLES AVAILABLE"
.sp
.I "AUTOGRAPH Examples:"
	c_agex07
.sp
.I "BIVAR Examples:"
	c_cbex01
.sp
.I "CONPACK Examples:"
	c_colcon
.sp
.I "EZMAP Examples:"
	c_eezmpa c_mpex05
.sp
.I "GKS Examples:"
	c_gtxpac
.sp
.I "LABELBAR Examples:"
	c_elblba
.sp
.I "PLOTCHAR Examples:"
	c_epltch
.sp
.I "SCROLLED TITLE Examples:"
	c_slex01
.sp
.I "SOFTFILL Examples:"
	c_sfex02
.sp
.I "Interactive Examples:"
	c_xwndws
.sp
.fi
.SH SEE ALSO
Online:
.BR ncargex(1NCARG),
.BR ncargcc(1NCARG),
.BR ncarg_cbind(3NCARG),
.BR ncargfile(1NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993, 1994 University Corporation
for Atmospheric Research
.br
All Rights Reserved
