.\"
.\"	$Id: gopen_gks.m,v 1.1 1993-03-21 01:30:49 haley Exp $
.\"
.TH GOPEN_GKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gopen_gks (Open GKS) - opens the GKS package
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gopen_gks(const char *err_file, size_t mem_unit);
.SH DESCRIPTION
.IP err_file 12
(Input) - The place to which error messages are to be written. 
Typically this should be "stdout".
.IP mem_unit 12
(Input) - The dimension of an internal buffer. 
Currently in NCAR GKS-0A, mem_unit is ignored.
.SH USAGE
To get output to any workstation,
gopen_gks, gopen_ws and gactivate_ws must be called in that order.
For jobs producing only CGM output, the three calls to gopen_gks, gopen_ws,
and gactivate_ws 
can be replaced with a call to the SPPS function c_opngks().
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gclose_gks(3NCARG),
.BR gupd_ws(3NCARG),
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
