.\"
.\"	$Id: fcaps.m,v 1.10 1993-05-04 22:03:41 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH FCAPS 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
fcaps \- 
report available fontcaps
.SH SYNOPSIS
.B fcaps
[
.B \-V
]
.SH DESCRIPTION
.B fcaps
is a simple script that reports all the available fontcaps for use in 
metafile translation by the metafile translator 
.BR ctrans(1NCARG) .
.SH OPTIONS
.TP
.B \-V
Print the version number and then exit.
.SH ENVIRONMENT
.TP
.B NCARG_ROOT
Path to root of NCAR Graphics installation.
.TP
.B NCARG_LIB
If set this variable contains the path to the installed NCAR Graphics 
libraries. 
.B NCARG_LIB
overrides 
.BR NCARG_ROOT .
.SH FILES
.TP 40
$NCARG_ROOT/lib/ncarg/fontcaps/*
The fontcaps.
.TP 40
$NCARG_LIB/ncarg/fontcaps/*
The fontcaps.
.SH "SEE ALSO"
.BR gcaps(5NCARG),
.BR ctrans(1NCARG),
.BR ictrans(1NCARG),
.BR fontcap(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

