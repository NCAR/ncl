.\"
.\"     $Id: shgrid_params.m,v 1.2 2000-07-13 03:18:11 haley Exp $
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

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

