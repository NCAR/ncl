.\"
.\"	$Id: c_ftseti.m,v 1.3 2000-08-22 15:14:50 haley Exp $
.\"
.TH c_ftseti 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftseti - set int valued parameters
.SH FUNCTION PROTOTYPE
void c_ftseti(char *, int);
.SH SYNOPSIS
void c_ftseti (pnam, ival);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned an int value. 
.IP ival 12
The value to be assigned to the control parameter whose name is 
pointed to by pnam. 
.SH USAGE
c_ftseti is used to set values for any of the control parameters 
that take int values. The values set by c_ftseti remain in effect 
until changed by subsequent calls to c_ftseti. 
.SH ACCESS
To use c_ftseti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftgeti
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

