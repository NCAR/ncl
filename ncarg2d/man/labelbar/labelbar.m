.TH Labelbar 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Labelbar - Creates a labeled, filled, rectangular bar to serve
as a key for a filled plot.
.SH SYNOPSIS
LBLBAR - Produces a complete label bar.
.sp
LBGETI - Retrieves current integer parameter values.
.sp
LBGETR - Retrieves current real parameter values.
.sp
LBSETI - Sets integer parameter values.
.sp
LBSETR - Sets real parameter values.
.SH C-BINDING SYNOPSIS
c_lblbar
.br
c_lbgeti
.br
c_lbgetr
.br
c_lbfill
.br
c_lbseti
.br
c_lbsetr
.SH USER-MODIFIABLE INTERNAL ROUTINES
LBFILL - Fills label bars.
.SH ACCESS 
To use Labelbar routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the
Labelbar C-binding routines, load the NCAR Graphics libraries 
ncargC, ncarg_gksC, ncarg, ncarg_gks, and ncarg_loc, preferably 
in that order.
.SH MESSAGES
When error conditions are detected, the support routine SETER 
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates 
execution. The possible error messages are as follows:
.sp
LBLBAR - ERROR EXIT FROM GQFACI 
.br
LBLBAR - ERROR EXIT FROM GQPLCI 
.br
LBLBAR - ERROR EXIT FROM GQTXCI 
.br
LBLBAR - ERROR EXIT FROM GQLWSC 
.br
LBGETI OR LBGETR - PARAMETER NAME TOO SHORT - x 
.br
LBGETI OR LBGETR - PARAMETER NAME NOT KNOWN - x 
.br
LBSETI OR LBSETR - PARAMETER NAME TOO SHORT - x 
.br
LBSETI OR LBSETR - PARAMETER NAME NOT KNOWN - x
.sp
All of these should be more or less self-explanatory. Those 
that complain of an error exit from a GKS routine probably 
imply that GKS has somehow been put in the wrong state. The 
others will result from using an incorrect internal-parameter 
name in a call to one of the parameter-access routines. 
(The "x" will be replaced by the offending name.)
.SH SEE ALSO
Online:
labelbar_params, lbfill, lbgeti, lbgetr, lblbar, lbseti, lbsetr, ncarg_cbind
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
