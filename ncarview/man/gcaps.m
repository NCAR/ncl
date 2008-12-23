.\"
.\"	$Id: gcaps.m,v 1.24 2008-12-23 00:04:15 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH GCAPS 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
gcaps \- 
report available graphcaps
.SH SYNOPSIS
.B gcaps
[
.B \-V
]
.SH DESCRIPTION
.B gcaps
is a simple script that reports all the available graphcaps for use in 
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
$NCARG_ROOT/lib/ncarg/graphcaps/*
The graphcaps.
.TP 40
$NCARG_LIB/ncarg/graphcaps/*
The graphcaps.
.SH "SEE ALSO"
.BR fcaps(1NCARG),
.BR ctrans(1NCARG),
.BR ictrans(1NCARG),
.BR graphcap(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
