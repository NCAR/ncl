.\"
.\"	$Id: gqops.m,v 1.3 1993-05-03 17:27:20 haley Exp $
.\"
.TH GQOPS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQOPS (Inquire operating state value) - returns the operating state
of GKS. 
.SH SYNOPSIS
CALL GQOPS (OPSTA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_op_st(Gop_st *op_st);
.SH DESCRIPTION
.IP OPSTA 12
(Integer, Output) - Returns the GKS operating state:
.RS
.IP 0 
GKS is closed
.IP 1 
GKS is open
.IP 2 
Additionally, a workstation is open
.IP 3 
Additionally, a workstation is active
.IP 4 
Additionally, a segment is open
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gacwk, gdawk, gclwk, gclks, opngks, clsgks, ginq_op_st
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
