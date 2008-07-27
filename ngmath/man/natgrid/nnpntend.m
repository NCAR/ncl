.\"
.\"     $Id: nnpntend.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNPNTEND 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NNPNTEND - terminate interpolation in single point mode
.SH SYNOPSIS
CALL NNPNTEND ()
.SH USAGE
This subroutine terminates single point mode.  It should be called
after entering single point mode using NNPNTINITS and interpolating
at individual points with NNPNTS.
.SH ACCESS
To use NNPNTEND, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgrids,
nnpntinits,
nnpnts.
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
