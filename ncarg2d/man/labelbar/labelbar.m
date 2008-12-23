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
ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
Various error conditions can occur in Labelbar.  Each of these results in
a call to the error-handling routine SETER, with a final argument indicating
that the error is recoverable; by default, an error message is printed and
execution is terminated, but, if you turn on error recovery
(as described in the "man" page for "error_handling"), you
can get control back.
.sp
The error messages are as follows; all should be
more or less self-explanatory.
.sp
.in +5
LBGETI - UNCLEARED PRIOR ERROR
.br
LBGETR - PARAMETER NAME NOT KNOWN - X
.br
LBGETR - PARAMETER NAME TOO SHORT - X
.br
LBGETR - UNCLEARED PRIOR ERROR
.br
LBLBAR - ERROR EXIT FROM GQFACI
.br
LBLBAR - ERROR EXIT FROM GQLWSC
.br
LBLBAR - ERROR EXIT FROM GQPLCI
.br
LBLBAR - ERROR EXIT FROM GQTXCI
.br
LBLBAR - UNCLEARED PRIOR ERROR
.br
LBSETI - UNCLEARED PRIOR ERROR
.br
LBSETR - PARAMETER NAME NOT KNOWN - X
.br
LBSETR - PARAMETER NAME TOO SHORT - X
.br
LBSETR - UNCLEARED PRIOR ERROR
.in -5
.sp
All of these should be more or less self-explanatory. Those 
that complain of an error exit from a GKS routine probably 
imply that GKS has somehow been put in the wrong state.
Others will result from using an incorrect internal-parameter
name in a call to one of the parameter-access routines. 
(The "X" will be replaced by the offending name.)
.SH SEE ALSO
Online:
labelbar_params, lbfill, lbgeti, lbgetr, lblbar, lbseti, lbsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
