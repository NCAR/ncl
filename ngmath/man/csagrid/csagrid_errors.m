.\"
.\"     $Id: csagrid_errors.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH csagrid_errors 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
csagrid_errors - This document briefly describes all the
errors reported from Csagrid.
.SH DESCRIPTION 
Each entry below includes the error number and its meaning.
.IP " 33"
Internal error in matrix computation, contact a consultant.
.IP " 34"
Failure in matrix computation, usually indicates insufficient
number of input data.
.IP " 35"
Internal error in matrix computation, contact a consultant.
.IP "102"
Number of knots is less than 4.
.IP "103"
Input data range is zero.
.IP "104"
Partial derivative order out of range zero to two.
.IP "105"
Insufficient number of input data.
.IP "106"
Insufficient work space (Fortran entry only).
.IP "107"
Failure in matrix computation, usually indicates insufficient
number of input data.
.IP "202"
Number of knots is less than 4 in some coordinate direction.
.IP "203"
Insufficient workspace.
.SH SEE ALSO
csagrid,
csa1s,
csa1xs,
csa2s,
csa2xs,
csa2ls,
csa2lxs,
csa3s,
csa3xs,
csa3ls,
csa3lxs,
c_csa1s,
c_csa1xs,
c_csa2s,
c_csa2xs,
c_csa2ls,
c_csa2lxs,
c_csa3s,
c_csa3xs,
c_csa3ls,
c_csa3lxs.
.sp
Complete documentation for Csagrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csagrid/csahome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
