.\"
.\"     $Id: c_csgetr.m,v 1.3 2000-08-22 15:14:33 haley Exp $
.\"
.TH c_csgetr 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_csgetr - Retrieves the value of an internal parameter of type float.
.SH FUNCTION PROTOTYPE
void c_csgetr(char *, float *);
.SH SYNOPSIS
c_csgetr (pnam,rval)
.SH DESCRIPTION 
.IP pnam 12
A string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP rval 12
*rval will be the value currently assigned to the control parameter
whose name is pointed to by pnam. 
.SH USAGE
This routine allows you to retrieve the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the csgrid_params man page.
.SH ACCESS
To use c_csgetr, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_csgrid,
csgrid_params,
c_cssetr.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csgrid/cshome.html
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

