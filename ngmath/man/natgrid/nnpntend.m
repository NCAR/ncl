.\"
.\"     $Id: nnpntend.m,v 1.2 1997-05-06 23:38:00 fred Exp $
.\"
.TH NNPNTEND 3NCARG "March 1997" UNIX "NCAR GRAPHICS"
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
Copyright (C) 1997
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
