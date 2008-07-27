.\"
.\"	$Id: c_ftcurvd.m,v 1.4 2008-07-27 03:35:37 haley Exp $
.\"
.TH c_ftcurvd 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftcurvd - calculate derivatives
.SH FUNCTION PROTOTYPE
int c_ftcurvd (int, float [], float [], int, float [], float []);
.SH SYNOPSIS
int c_ftcurvd (n, xi, yi, m, xo, yo);
.SH DESCRIPTION
.IP n 12
The number of input data points. (n > 1) 
.IP xi 12
An array containing the abscissae for the input function. 
.IP yi 12
An array containing the functional values of the input 
function -- y(k) is the functional value at x(k) for k=0,n. 
.IP m 12
The number of desired derivatives.
.IP xo 12
An array containing the abscissae for the output values.
.IP yo 12
An array containing the interpolated derivative 
values -- yo(k) is the functional derivative at xo(k) for k=0,n. 
.SH RETURN VALUE
c_ftcurvd returns an error value as per: 
.br
.sp
= 0 -- no error.
.br
= 1 -- if n is less than 2.
.br
= 2 -- if X values are not strictly increasing.
.br
.SH USAGE
c_ftcurvd is called after all of the desired values for control parameters 
have been set using the procedures c_ftseti, c_ftsetr, c_ftsetc. 
Control parameters that apply to c_ftcurvd are: sig, sl1, sln, sf1.
.SH ACCESS
To use c_ftcurvd, load the NCAR Graphics library ngmath.
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
