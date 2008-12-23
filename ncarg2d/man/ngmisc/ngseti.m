.TH NGSETI 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGSETI - 
Provides a new integer value for an internal parameter of Ngmisc
of type INTEGER.  See the ngmisc_params man page for a complete list of all the
ngmisc internal parameters.
.SH SYNOPSIS
CALL NGSETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter to be set. Only the first two
characters of the string are examined.
.IP IVAL 12
(an input expression of type INTEGER) is the desired
value of the internal parameter. 
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Ngmisc parameters.  For a complete list of parameters available
in this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGSETI or c_ngseti,  load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
ngmisc_params,
nggeti,
nggetc,
ngsetc,
nggetr,
ngsetr,
ncarg_cbind.
.sp
Online URL:  http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
