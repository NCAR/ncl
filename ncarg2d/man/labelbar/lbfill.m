.TH LBFILL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LBFILL - A routine that is called by LBLBAR (when the argument IFTP is
non-zero) to fill portions of the bar. The default version
of this routine does color fill by calling GSFACI and GFA.
A user-written version may be supplied to do a different kind of fill.
.SH SYNOPSIS
CALL LBFILL (IFTP, XCRA, YCRA, NCRA, INDX) 
.SH DESCRIPTION 
.IP IFTP 12
(an input expression of type INTEGER) is the value of the 
argument IFTP from the user's call to LBLBAR.
.IP "XCRA, YCRA" 12
(input arrays of type REAL) are real arrays containing the 
X and Y coordinates of five points defining a rectangular box which is to 
be filled in some manner. The fifth point defined by these arrays will 
always be a duplicate of the first.
.IP NCRA 12
(an input expression of type INTEGER) is the number of points 
defining the rectangular box. Its value will always be five.
.IP INDX 12
(an input expression of type INTEGER) is one of the indices from the 
array LFIN, in the user's call to LBLBAR. Its value may be used as a 
color index or as a pattern selector.
.SH USAGE
This routine is called by LBLBAR when LBLBAR's argument IFTP is non-zero. 
LBFILL is called once for each sub-box in the label bar, using a 
statement of the form:
.sp
.RS
CALL LBFILL (IFTP,XCRA,YCRA,NCRA,INDX)
.RE
.sp
and is expected to fill the sub-box defined by its arguments. The default 
version of the routine looks like this:
.sp
.RS
SUBROUTINE LBFILL (IFTP,XCRA,YCRA,NCRA,INDX) 
.RS 2
DIMENSION XCRA(*),YCRA(*) 
.br
CALL GSFACI (INDX) 
.br
CALL GFA (NCRA-1,XCRA,YCRA) 
.br
RETURN 
.RE
END
.RE
.sp
This version does color fill of the box, using the last argument as a 
color index. The user may replace this routine with a version that does 
any desired sort of fill; usually, this can be done by simply compiling 
the desired version, so that the default one from the package will not be 
loaded.
.SH ACCESS
To use LBFILL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
labelbar, labelbar_params, lbgeti, lbgetr, lblbar, lbseti, lbsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
