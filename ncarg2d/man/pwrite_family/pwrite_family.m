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
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
pwritx, pwrity,
plotchar, plchhq, plchmq, plchlq
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
