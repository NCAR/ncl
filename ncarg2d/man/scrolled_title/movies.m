.\"
.\"	$Id: movies.m,v 1.1 1993-03-11 16:30:04 haley Exp $
.\"
.TH STITLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
STITLE - Plot stationary or scrolling titles for movies
.SH SYNOPSIS
STITLE - Produces movie titles that may be scrolled and/or
faded in and out.
.br
FTITLE - Produces titling frames with the text in each frame
being in the same position.
.br
SLSETI - Sets integer parameter values.
.br
SLSETR - Sets real parameter values.
.br
SLGETI - Retrieves current integer parameter values.
.br
SLGETR - Retrieves current real parameter values.
.br
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_stitle
.br
c_ftitle
.br
c_slseti
.br
c_slsetr
.br
c_slgeti
.br
c_slgetr
.br
.SH ACCESS 
To use STITLE routines load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the STITLE 
C-bindings, load the NCAR Graphics libraries ncargC, ncarg_gksC,
ncarg_gks, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
ftitle movies slgeti slgetr slseti slsetr stitle ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

