.\"
.\"	$Id: gopks.m,v 1.9 2000-07-11 23:03:09 haley Exp $
.\"
.TH GOPKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GOPKS (Open GKS) - opens the GKS package
.SH SYNOPSIS
CALL GOPKS (ERRFIL, BUFA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gopen_gks(const char *err_file, size_t mem_unit);
.SH DESCRIPTION
.IP ERRFIL 12
(Input, Integer) - The Fortran unit number to which 
error messages are to be written. 
Typically this should be unit 6.
.IP BUFA 12
(Input, Integer) - The dimension of an internal buffer. 
Currently in NCAR GKS-0A, BUFA is ignored.
.SH USAGE
To get output to any workstation,
GOPKS, GOPWK and GACWK must be called in that order.
For jobs producing only CGM output, the three calls to GOPKS, GOPWK and GACWK 
can be replaced with a call to the SPPS function OPNGKS.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopwk, gacwk, gdawk, gclwk, gclks, guwk, opngks, clsgks, gopen_gks
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

