.\"
.\"     $Id: c_nnpntendd.m,v 1.3 1998-02-04 15:31:57 haley Exp $
.\"
.TH c_nnpntendd 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nnpntendd - exit single point mode
.SH FUNCTION PROTOTYPE
void c_nnpntendd();
.SH SYNOPSIS
void c_nnpntendd();
.SH USAGE
This function terminates single point mode.
It should be called
after entering single point mode using c_nnpntinitd and interpolating
at individual points with c_nnpntd.
.SH ACCESS
To use c_nnpntendd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgridd,
c_nnpntinitd,
c_nnpntd.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 1997-1998
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
