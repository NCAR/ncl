.\"
.\"	$Id: ncarg_gks_cbind.m,v 1.1 1993-03-11 15:22:53 haley Exp $
.\"
.TH NCARG_GKS_CBIND 3NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
NCAR Graphics GKS C-binding - Description of how to use the NCAR Graphics
GKS C-binding.
.SH SYNOPSIS
This section only briefly describes how to use the NCAR Graphics GKS
C-binding.  If you intend to use the GKS C-bindings heavily, you may
want to get a copy of the American National Standard's "Computer
Graphics - Graphical Kernel System (GKS) - Functional Description".
If you want information on how to use the C-bindings for the NCAR
Graphics utilities, please see \fBncarg_cbind(3NCARG)\fP or the man
page for any of the NCAR Graphics routines.
.sp
The NCAR Graphics GKS C-binding adheres to the ISO/IEC standard.  Only
the functions at level 0A have been implemented.  In the future, as
higher level GKS Fortran functions are implemented, there may be
C-bindings created for them as well.
.sp
Since the NCAR Graphics GKS C-binding adheres to the standard, the
names for the C-bindings are not like the Fortran names.  Instead, the
functions names are more descriptive, like \fBgset_fill_colr_ind\fP
for \fBGSFACI\fP.  To get the name and usage of the C-binding, please
see the man page for the corresponding Fortran routine.
.SH ARGUMENT LISTS
The argument list of each GKS C-binding closely corresponds with the
argument list of the Fortran routine, but in many cases, the arguments
are represented in the form of a C structure.  These structures are defined
in the include file <ncarg/gks.h>.
.SH FUNCTION PROTOTYPES
The GKS C-bindings are intended to be ANSI C compliant.  To get the
correct function prototypes you can include <ncarg/gks.h>.
.SH COMPILING YOUR PROGRAM
To compile your NCAR Graphics C program with the GKS C-bindings, use the
application \fBncargcc\fP.  \fBncargcc\fP will take care of loading in
the necessary C/Fortran interface libraries as well as the NCAR
Graphics C and Fortran libraries.  If you do not wish to use
\fBncargcc\fP, then you can just run it with no arguments to see what
the necessary libraries are, and then put this in your Makefile or
whatever else you are using to compile your program.
.SH EXAMPLES
An example of a C program that calls some of the NCAR Graphics GKS
C-bindings has been provided.  To copy this C program into your 
directory, and then compile, link, and run it, type \fBncargcex c_gtxpac\fP.
.SH SEE ALSO
Online:
ncarg_cbind (3NCARG), ncargcex (1NCARG), man pages for any of the GKS 0A 
routines and/or NCAR Graphics user entry points.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

