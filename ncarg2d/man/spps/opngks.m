.TH OPNGKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
OPNGKS - Opens GKS, opens and activates an NCAR GCM
workstation (workstation of type 1) with workstation ID
of 1 and connection ID of 2.
.SH STATUS
OPNGKS is somewhat dated.  It was primarily used with NCAR GKS-0A
when this package only allowed for a single metacode workstation.
Now that the NCAR GKS package provides for multiple workstations
to be open, it is recommended that standard GKS calls to
GOPKS, GOPWK, and GACWK be used instead.
.SH SYNOPSIS
CALL OPNGKS
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_opngks()
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
agex01.
.SH ACCESS
To use OPNGKS or c_opngks, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
spps, gopks, gopwk, gacwk, clsgks, setusv, getusv, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
