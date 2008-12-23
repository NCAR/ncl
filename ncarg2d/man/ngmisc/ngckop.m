.TH NGCKOP 3NCARG "October 1996" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGCKOP - a function to check if a specified GKS workstation is open.
.SH SYNOPSIS
INTEGER FUNCTION NGCKOP(WKID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_ngckop(int wkid)
.SH DESCRIPTION 
.IP WKID 12
(an input variable of type INTEGER) specifying a  GKS workstation
identifier.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
The function returns a "1" if the workstation identified by
WKID is open, otherwise it returns a "0".
.SH EXAMPLES
.nf

        IOPEN = NGCKOP(WKID)

.fi
sets IOPEN to "1" if WKID is open and sets IOPEN to "0" otherwise.
.SH ACCESS
To use NGCKOP or c_ngckop, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online URL: http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
