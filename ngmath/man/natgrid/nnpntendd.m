.\"
.\"     $Id: nnpntendd.m,v 1.1 1997-03-07 22:59:25 fred Exp $
.\"
.TH NNPNTENDD 3NCARG "March 1997" UNIX "NCAR GRAPHICS"
.SH NAME
NNPNTENDD - terminate interpolation in single point mode
.SH SYNOPSIS
CALL NNPNTENDD ()
.SH USAGE
This subroutine terminates single point mode. It should be called
after entering single point mode using NNPNTINITD and interpolating
at individual points with NNPNTD.
.SH ACCESS
To use NNPNTENDD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid_params,
natgridd,
nnpntinitd,
nnpntd.
.sp
Complete documentation for Natgird is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 1997
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
