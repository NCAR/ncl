.\"
.\"	$Id: ginq_op_st.m,v 1.16 2008-12-23 00:03:04 haley Exp $
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
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
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
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
