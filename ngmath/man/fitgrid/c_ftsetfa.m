.\"
.\"	$Id: c_ftsetfa.m,v 1.3 2000-08-22 15:14:50 haley Exp $
.\"
.TH c_ftsetfa 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
Set float valued array parameters
.SH FUNCTION PROTOTYPE
int c_ftsetfa(char *pnam, int n, float *far);
.SH SYNOPSIS
int c_ftsetfa (pnam, n, far);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned a float array value. 
.IP n 12
The number of float values in the array. 
.IP far 12
A pointer to the n-element input array. 
.SH USAGE
c_ftsetra is used to set values for any of the control parameters that 
take floating arrays. The values set by
c_ftsetfa remain in effect until changed by subsequent calls to c_ftsetfa .
.SH ACCESS
To use c_ftsetfa, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetfa, c_ftgetfa_size, c_ftgetfa_data 
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

