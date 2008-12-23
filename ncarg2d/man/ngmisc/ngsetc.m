.TH NGSETC 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGSETC - Used to set the character value of a specified internal parameter.
.SH SYNOPSIS
CALL NGSETC (CNP, CVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngsetc (char *cnp, char *cvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of an internal parameter
of Ngmisc.  Only the first two characters will be examined.
.IP CVP 12
(an input expression of type CHARACTER) is the desired new
value of the parameter specified by CNP. This value will be 
used until the next call resetting it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the 
FORTRAN argument descriptions.
.SH USAGE
This routine allows you to set the current value of Ngmisc
parameters. For a complete list of parameters available in
this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGSETC or c_ngsetc, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
ngmisc_params,
nggetc,
ngseti,
nggeti,
ngsetr,
nggetr,
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
