.\"
.\"	$Id: c_ftcurvi.m,v 1.2 2000-07-13 03:17:58 haley Exp $
.\"
.TH c_ftcurvi 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftcurvi - calculate integrals
.SH FUNCTION PROTOTYPE
int c_ftcurvi (float, float, int, float [], float [], float *);
.SH SYNOPSIS
int c_ftcurvi (xl, xr, n, xi, yi, integral);
.SH DESCRIPTION
.IP xl 12
The lower limit of the integration. 
.IP xr 12
The upper limit of the integration. 
.IP n 12
The number of input data points. (N > 1) 
.IP xi 12
An array containing the abscissae for the input function. 
.IP yi 12
An array containing the functional values of the input 
function (y(k) is the functional value at x(k) for k=0,n). 
.IP integral 12
The integral of the function from xl to xr is given by *integral. 
.SH RETURN VALUE
c_ftcurvi returns an error value as per: 
.br
.sp
= 0 -- no error.
.br
= 1 -- if n is less than 2.
.br
= 2 -- if X values are not strictly increasing.
.br
.SH USAGE
c_ftcurvi is called after all of the desired values for control parameters 
have been set using the procedures c_ftseti, c_ftsetr, c_ftsetc. 
Control parameters that apply to c_ftcurvi are: sig, sl1, sln, sf1.
.SH ACCESS
To use c_ftcurvi, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftseti, c_ftsetr, c_ftsetc.
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
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

