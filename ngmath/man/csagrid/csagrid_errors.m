.\"
.\"     $Id: csagrid_errors.m,v 1.3 2000-08-22 15:14:28 haley Exp $
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

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

