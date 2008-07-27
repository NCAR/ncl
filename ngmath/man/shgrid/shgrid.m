.\"
.\"	$Id: shgrid.m,v 1.4 2008-07-27 03:35:42 haley Exp $
.\"
.TH SHGRID 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.SH NAME
SHGRID - interpolate random data in 3-space using a modified Shepard's algorithm
.SH SYNOPSIS

CALL SHGRID (N, X, Y, Z, F, NXO, NYO, NZO, XO, YO, ZO, 
.br
            FF, IWK, RWK, IER)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data points (N > 9).
.IP X 12
(real, input) An array containing the X coordinates of the 
input data points.  X is dimensioned for N. 
.IP Y 12
(real, input) An array containing the Y coordinates of the 
input data points.  Y is dimensioned for N. 
.IP Z 12
(real, input) An array containing the Z coordinates of the 
input data points.  Z is dimensioned for N. 
.IP F 12
(real, input) Array dimensioned for N containing data values. 
F(I) is a functional value at (X(I),Y(I),Z(I)) for I = 1 to N. 
.IP NXO 12
(integer, input) The number of X coordinate values in the output grid.
.IP NYO 12
(integer, input) The number of Y coordinate values in the output grid.
.IP NZO 12
(integer, input) The number of Z coordinate values in the output grid.
.IP XO 12
(real, input) An array dimensioned for NXO 
containing the X coordinates of the output surface.
.IP YO 12
(real, input) An array dimensioned for NYO
containing the Y coordinates of the output surface.
.IP ZO 12
(real, input) An array dimensioned for NZO
containing the Y coordinates of the output surface.
.IP FF 12
(real, output) An array dimensioned for NXO x NYO x NZO 
containing the calculated function values for the
output function.  FF(I,J,K) is the calculated functional value
at (XO(I), YO(J), ZO(K)) for I=1,NXO and J=1,NYO and K=1,NZO.
.IP IRK 12 
(integer, input) An integer workspace of length at least 2*N. 
.IP RWK 12
(real, input) A work array dimensioned for 11*N+6.
.IP IER 12
(integer, output) An error return value.  If IER is returned as 0, then
no errors were detected. If IER is non-zero, then refer to the man
page for shgrid_errors for details.
.SH USAGE
SHGRID is called to interpolate randomly spaced data in 3-space. 
SHGRID uses a modified Shepard's algorithm to construct an interpolatory 
surface.
.SH ACCESS
To use SHGRID, load the NCAR Graphics library ngmath.
.SH SEE ALSO
shgrid_params, 
shseti, 
shgeti, 
shgetnp.
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
