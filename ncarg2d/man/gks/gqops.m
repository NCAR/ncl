.\"
.\"	$Id: gqops.m,v 1.16 2008-12-23 00:03:03 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
