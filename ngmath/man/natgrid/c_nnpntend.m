.\"
.\"     $Id: c_nnpntend.m,v 1.6 2008-07-27 03:35:40 haley Exp $
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

The use of this Software is governed by a License Agreement.
