.\"
.\"	$Id: ncargfile.m,v 1.1 1993-05-08 17:00:22 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGFILE 1NCARG "MARCH 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargfile \- NCAR Graphics Files
.SH SYNOPSIS
\fBncargfile\fP 
[\fB\-all\fR]
\fBfile ...\fR
.SH DESCRIPTION
.I ncargfile
provides the user with access to special NCAR Graphics files or
tables.  \fIncargfile\fP copies the specified file(s) into the
current directory.
.sp
In order to run \fIncargfile\fP, you must have your NCARG_ROOT
environment variable set to the directory pathname where the NCAR
Graphics libraries, binaries, and include files were installed.  If
you are not sure what NCARG_ROOT should be set to, please check with
your system administrator or the site representative for NCAR
Graphics.  If the NCAR Graphics libraries, binaries, and include files
were not installed under one root directory, then you will need to set
the environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE
instead.  Please see "man ncargintro" for more information.
.sp
Currently, only one table is available with \fIncargfile\fP, and that
is the table of Ezmap Area identifiers, called "ezmap_area_ids".
.sp
.fi
.SH SEE ALSO
Online:
.BR ncargex(1NCARG),
.BR ncargcex(1NCARG),
.BR ncargintro(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
