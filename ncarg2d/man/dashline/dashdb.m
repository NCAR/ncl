.TH DASHDB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DASHDB - Chooses a dash pattern without labels.
.SH SYNOPSIS
CALL DASHDB (IPAT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dashdb (int *ipat)
.SH DESCRIPTION 
.IP IPAT 12
(an input parameter of type INTEGER) the value of which is
a 16-bit dash pattern (1=solid, 0=blank); e.g., the binary
number 1111000011110000, which is a decimal 61680,
will give dashes of medium length.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
carline,
colcon,
cmpfil,
cmpitm,
cmplab,
cmpmsk,
cmptit,
cmpusr,
mpex08,
tdashc,
tdashl,
tdashp,
tdashs,
fdldashd.
.SH ACCESS
To use DASHDB, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_dashdb, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
dashline, dashline_params,
curved, dashdc, frstd, lastd, lined, reset, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
