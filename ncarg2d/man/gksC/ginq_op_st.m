.\"
.\"	$Id: ginq_op_st.m,v 1.2 1993-05-03 17:30:27 haley Exp $
.\"
.TH GINQ_OP_ST 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_op_st (Inquire operating state value) - returns the operating state
of GKS. 
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_op_st(Gop_st *op_st);
.SH DESCRIPTION
.IP op_st 12
(Output) - Returns the GKS operating state:
.RS
.IP GST_GKCL
GKS is closed
.IP GST_GKOP
GKS is open
.IP GST_WSOP
Additionally, a workstation is open
.IP GST_WSAC
Additionally, a workstation is active
.IP GST_SGOP
Additionally, a segment is open
.RE
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gclose_gks(3NCARG),
.BR opngks(3NCARG),
.BR clsgks(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
