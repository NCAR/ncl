.\"
.\"     $Id: fitgrid.m,v 1.3 2000-08-22 15:14:53 haley Exp $
.\"
.TH Fitgrid 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Fitgrid is a package for interpolating one dimensional and two 
dimensional data. The package has the following functionality: 
.br
.sp
1.) Interpolating single-valued functions in one dimension. 
.br
2.) Interpolating parametric curves in the plane. 
.br
3.) Computing an interpolatory surface through a rectangular grid of functional
values. 
.br
4.) Interpolating periodic functions in one dimension. 
.br
5.) Finding integrals and derivatives of interpolated functions. 
.br
.sp

.SH SYNOPSIS
.br
.sp
.sp
FORTRAN single precision
.br
------------------------
.sp
 CURV1   -  does set-up for CURV2. 
.br
 CURV2   -  1D interpolation for non-periodic functions.
.br
 CURVD   -  derivatives for 1D functions.
.br
 CURVI   -  integrals for 1D functions.
.br
 CURVP1  -  does set-up for CURVP2 .
.br
 CURVP2  -  1D interpolation for periodic functions.
.br
 CURVPI  -  integrals for periodic functions.
.br
 CURVS   -  does smoothing and set-up for CURV2.
.br
 CURVPS  -  does smoothing for periodic functions.
.br
 KURV1   -  does set-up for KURV2.
.br
 KURV2   -  interpolation for parametric curves.
.br
 KURVD   -  derivatives for parametric curves.
.br
 KURVP1  -  set-up for KURVP2.
.br
 KURVP2  -  interpolation for closed parametric curves.
.br
 KURVPD  -  derivatives for closed parametric curves.
.br
 SURF1   -  set-up for SURF2.
.br
 SURF2   -  2D interpolation for gridded data.
.br
.sp
C single precision:
.br
------------------
.sp
 c_ftcurv    -  1D interpolation for non-periodic functions.
.br
 c_ftcurvd   -  derivatives for 1D functions.
.br
 c_ftcurvi   -  integrals for 1D functions.
.br
 c_ftcurvp   -  1D interpolation for periodic functions.
.br
 c_ftcurvpi  -  integrals for periodic functions.
.br
 c_ftcurvs   -  does smoothing for non-periodic functions.
.br
 c_ftcurvps  -  does smoothing for periodic functions.
.br
 c_ftkurv    -  interpolation for parametric curves.
.br
 c_ftkurvp   -  interpolation for closed parametric curves.
.br
 c_ftkurvd   -  derivatives for parametric curves.
.br
 c_ftkurvpd  -  interpolation for closed parametric curves.
.br
 c_ftsurf    -  2D interpolation for rectangular data.
.br
 c_ftseti    -  set values for int parameters.
.br
 c_ftgeti    -  get values for int parameters.
.br
 c_ftsetr    -  set values for float parameters.
.br
 c_ftgetr    -  get values for float parameters.
.br
 c_ftsetc    -  set values for string parameters.
.br
 c_ftgetc    -  get values for string parameters.
.br
 c_ftsetfa   -  set values for float arrays.
.br
 c_ftgetfa_size  -  get array size of float array.
.br
 c_ftgetfa_data  -  get float array data.
.br
.sp
.br
.SH ACCESS 
To use fitgrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
Individual entries, fitgrid_params
.sp
Complete documentation for fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
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

