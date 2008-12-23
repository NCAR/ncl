.TH LASTD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LASTD -
Terminates a sequence of calls to draw a curve (a call to FRSTD followed by
one or more calls to VECTD).
.SH SYNOPSIS
CALL LASTD 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lastd()
.SH USAGE
A sequence of calls to draw a curve (a call to FRSTD followed by one or
more calls to VECTD) may be followed by a call to LASTD.  It flushes any
portions of smoothed curves that are defined by coordinates saved in
internal buffers of FRSTD and VECTD and that have not yet been drawn.  Calls
to LASTD are not always required - for example, when a non-smoothing version
of Dashline is used (no buffering) or when the next call to an NCAR Graphics
routine will be to FRSTD (which flushes the buffers) - but unnecessary calls
do no harm.  If you judge that one of the smoothing versions of Dashline may
be used, it is best to put in the calls to LASTD.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashp, tdashs.
.SH ACCESS
To use LASTD or c_lastd, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashline, dashline_params, curved,
dashdb, dashdc, frstd, lastd, lined, reset, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
