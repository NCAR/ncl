.\"
.\"	$Id: c_ftcurvp.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftcurvp 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftcurvp - 1D interpolation for periodic functions
.SH FUNCTION PROTOTYPE
int c_ftcurvp (int, float [], float [], float, int, float [], float []);
.SH SYNOPSIS
int c_ftcurvp (n, xi, yi, p, m, xo, yo);
.SH DESCRIPTION
.IP n 12
The number of input data points. (n > 1) 
.IP xi 12
An array containing the abscissae for the input function. 
.IP yi 12
An array containing the input functional values (y(k) is the 
functional value at x(k) for k=0,n). 
.IP p 12
The period of the function; p must not be less than xi[n-1] - xi[0]. 
.IP m 12
The number of desired interpolated points. 
.IP xo 12
An array containing the abscissae for the interpolated values. 
.IP yo 12
An array containing the interpolated functional values 
(yo(k) is the functional value at xo(k) for k=0,n). 
.SH RETURN VALUE
c_ftcurvp returns an error value as per: 
.br
.sp
= 0 -- no error.
.br
= 1 -- if n is less than 2.
.br
= 2 -- if the period is strictly less than the span of the abscissae. 
.br
.SH USAGE
c_ftcurvp is called after all of the desired values for control parameters 
have been set using the procedures c_ftseti, c_ftsetr, c_ftsetc. 
Control parameters that apply to c_ftcurvp are: sig. 
.SH ACCESS
To use c_ftcurvp, load the NCAR Graphics library ngmath.
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

The use of this Software is governed by a License Agreement.
