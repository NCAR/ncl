.\"
.\"     $Id: c_shseti.m,v 1.3 2000-08-22 15:15:15 haley Exp $
.\"
.TH c_shseti 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_shseti - set int valued parameters
.SH FUNCTION PROTOTYPE
void c_shseti(char *, int);
.SH SYNOPSIS
void c_shseti (pnam, ival);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP ival 12
An int value to be assigned to the internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Shgrid parameters.  For a complete list of parameters available
in this utility, see the shgrid_params man page.
.SH ACCESS
To use c_shseti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_shgrid,
c_shgetnp,
c_shgeti,
shgrid_params.
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

