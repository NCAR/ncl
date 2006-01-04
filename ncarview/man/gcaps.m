.\"
.\"	$Id: gcaps.m,v 1.21 2006-01-04 00:15:51 haley Exp $
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
Copyright (C) 1987-2006
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

