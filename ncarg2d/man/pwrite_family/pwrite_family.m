.TH Pwrite_family 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Pwrite_family - A utility for drawing text and other characters using
polylines.
.SH STATUS
Pwrite_family is obsolete.  It has been replaced by the Plotchar utility.
.sp
Entries PWRITX and PWRITY
continue to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use Plotchar.
.SH SYNOPSIS
PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR) - draws characters as stroked
polylines.  It is the higher quality routine of the Pwrite_family.
.br
PWRITY (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR) - also draws characters as
stroked polylines, but is quicker, of lower quality, and with less
options than PWRITX.
.SH ACCESS 
To use PWRITX or PWRITY, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
pwritx, pwrity,
plotchar, plchhq, plchmq, plchlq
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2003
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
