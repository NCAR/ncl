.\"
.\"	$Id: threed.m,v 1.1 1993-03-11 16:34:54 haley Exp $
.\"
.TH THREED 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
THREED - 3-d line drawing package
.SH SYNOPSIS
CALL SET3 (XA,XB,YA,YB,UC,UD,VC,VD,WC,WD,EYE)
.br
CALL CURVE3 (U,V,W,N)
.br
CALL LINE3 (UA,VA,WA,UB,VB,WB)
.br
CALL FRST3 (U,V,W)
.br
CALL VECT3 (U,V,W)
.br
CALL POINT3 (U,V,W)
.br
CALL PERIM3 (MAGR1,MINR2,MAGR2,MINR2,IWHICH,VAR)
.br
CALL TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
.br
CALL FENCE3 (U,V,W,N,IOREN,BOT)
.br
.SH SYNOPSIS
CALL @RTNNAM(@ARGS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_set3
.br
c_curve3
.br
c_line3
.br
c_frst3
.br
c_vect3
.br
c_point3
.br
c_perim3
.br
c_tick43
.br
c_fence3
.br
.SH ACCESS 
To use THREED routines load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the
THREED C-binding routines load the NCAR Graphics libraries ncargC,
ncarg_gksC, ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curve3 fence3 frst3 line3 perim3 point3 set3 threed
tick43 vect3 ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
