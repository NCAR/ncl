.\"
.\"     $Id: c_nnpntend.m,v 1.4 2000-07-13 03:18:05 haley Exp $
.\"
.TH c_nnpntend 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpntend - exit single point mode
.SH FUNCTION PROTOTYPE
void c_nnpntend();
.SH SYNOPSIS
void c_nnpntend ();
.SH USAGE
This function terminates single point mode.
.SH ACCESS
To use c_nnpntend, load the NCAR Graphics library ngmath.
It should be called
after entering single point mode using c_nnpntinits and interpolating
at individual points with c_nnpnts.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnpntinits,
c_nnpnts.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
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

