.\"
.\"     $Id: shgrid_params.m,v 1.4 2008-07-27 03:35:42 haley Exp $
.\"
.TH shgrid_params 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
shgrid_params - This document briefly describes all the
internal parameters of Shgrid.
.SH DESCRIPTION 
Each entry below includes the name of a parameter, its Fortran type
(use the obvious equivalents for C), 
its default value, and a short description of the parameter.  
.IP "\'NCL\'   -   Integer   -    (N/3)**(1/3)"
Granularity of 3-D cell grid
.IP "\'NFL\'   -   Integer   -   32"
Number of input data values within the sphere of influence.
.IP "\'NLS\'   -   Integer   -   17"
Nouber of data values used in the least squares fit.
.SH SEE ALSO
shgrid,
shseti,
shgeti,
shsetnp,
c_shgrid,
c_shseti,
c_shseti,
c_shgetnp.
.sp
Complete documentation for Shgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/shgrid/shhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
