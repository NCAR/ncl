.\"
.\"     $Id: c_dsseti.m,v 1.3 2000-07-13 03:17:54 haley Exp $
.\"
.TH c_dsseti 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_dsseti - set int valued parameters
.SH FUNCTION PROTOTYPE
void c_dsseti(char *, int);
.SH SYNOPSIS
void c_dsseti (pnam, ival);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP ival 12
An int value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Dsgrid parameters.  For a complete list of parameters available
in this utility, see the dsgrid_params man page.
.SH ACCESS
To use c_dsseti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_dsgrid,
c_dsgrid_params,
c_dsgrid2s,
c_dsgrid3s,
c_dsgrid2d,
c_dsgrid3d,
c_dspnt2s,
c_dspnt2d,
c_dspnt3s,
c_dspnt3d,
c_dsgeti.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
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



