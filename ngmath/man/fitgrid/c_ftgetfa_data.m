.\"
.\"	$Id: c_ftgetfa_data.m,v 1.2 2000-07-13 03:17:59 haley Exp $
.\"
.TH c_ftgetfa_data 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftgetfa_data - retrieve array values
.SH FUNCTION PROTOTYPE
float *c_ftgetfa_data(char *);
.SH SYNOPSIS
float *c_ftgetfa_data (pnam)
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter whose value is to be retrieved. 
.SH RETURN VALUE
c_ftgetfa_data returns a pointer to a array of data (or returns a null
pointer if an error occurs). The size of the array can be obtained 
by using c_ftgetfa_size. 
.SH USAGE
c_ftgetfa_data is a called to obtain current values for any of 
the float valued array control parameters. 
.SH ACCESS
To use c_ftgetfa_data, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetfa, c_ftgetfa, c_ftgetfa_size
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

