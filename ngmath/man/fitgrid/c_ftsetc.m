.\"
.\"	$Id: c_ftsetc.m,v 1.3 2000-08-22 15:14:50 haley Exp $
.\"
.TH c_ftsetc 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftsetc - set char valued parameters
.SH FUNCTION PROTOTYPE
void c_ftsetc(char *, char *);
.SH SYNOPSIS
void c_ftsetc (pnam, cval);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned an string value. 
.IP ival 12
The value to be assigned to the control parameter whose name is 
pointed to by pnam. 
.SH USAGE
c_ftsetc is used to set values for any of the control parameters 
that take string values. The values set by c_ftsetc remain in effect 
until changed by subsequent calls to c_ftsetc. 
.SH ACCESS
To use c_ftsetc, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftgetc
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

