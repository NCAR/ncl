.\"
.\"	$Id: gclose_gks.m,v 1.2 1993-03-29 22:41:45 haley Exp $
.\"
.TH GCLOSE_GKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gclose_gks (Close GSK) - closes the GKS package.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_gks( void );
.SH USAGE
A call to gclose_gks insures that the graphics package is properly
terminated.  No graphics calls depending on GKS should be made
during the time that GKS is closed.  If output to a CGM has been
the exclusive requirement of the graphics job, and the SPPS entry c_opngks
has been used to open GKS and to open and activate a CGM workstation,
then c_clsgks can be used to deactivate and close the CGM workstation,
and to close GKS, making a direct call to gclose_gks unnecessary in that
case.
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
.BR gupwk(3NCARG),
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
