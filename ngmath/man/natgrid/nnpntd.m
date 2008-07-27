.\"
.\"     $Id: nnpntd.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNPNTD 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NNPNTD- Interpolate at a single point in double precision
.SH SYNOPSIS
CALL NNPNTD (X, Y, Z)
.SH DESCRIPTION
.IP X 12
(Double precision, Input) - The X coordinate of the 
point where interpolation is desired.
.IP Y 12
(Double precision, Input) - The Y coordinate of the point 
where interpolation is desired. 
.IP Z 12
(Double precision, Output) - The interpolated functional value at 
the point (X,Y).
.SH USAGE
This subroutine is called when you want to interpolate at an individal
point.  NNPNTINITD must be called prior to calling NNPNTD and NNPNTENDD
must be called to terminate single point mode.
.SH ACCESS
To use NNPNTD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgridd,
nnpntinitd,
nnpntendd.
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
