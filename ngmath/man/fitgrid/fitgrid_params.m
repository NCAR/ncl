.\"
.\"     $Id: fitgrid_params.m,v 1.5 2008-07-27 03:35:39 haley Exp $
.\"
.TH fitgrid_params 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
fitgrid_params - This document briefly describes all the
internal parameters of Fitgrid.
.SH DESCRIPTION 
Each entry below includes the name of a parameter, its Fortran type
(use the obvious equivalents for C), 
its default value, and a short description of the parameter.  
.IP "\'SIG\'   -   Real   -    1."
Value of the tension factor.
.IP "\'SL1\'   -   Real   -    0."
The slope of the curve at the first point (see parameter SF1).
.IP "\'SLN\'   -   Real   -    0."
The slope of the curve at the final point (see parameter SF1).
.IP "\'SF1\'   -   Integer   -    3  "
Controls the use of parameters SL1 and SL2: 0=use both; 1=use SL1 only;
2=use SL2 only; 3=use neither.
.IP "\'SF2\'   -   Integer   -    3  "
Controls the use of parameters SMT and EPS: 0=use neither; 1=use both;
2=use SMT only; 3=use EPS only.
.IP "\'SMT\'   -   Real   -   none"
Smoothing parameter for ftcurvs, ftcurvps, c_ftcurvs, and c_ftcurvps.
.IP "\'EPS\'   -   Real   -   none"
Smoothing parameter for ftcurvs, ftcurvps, c_ftcurvs, and c_ftcurvps.
.IP "\'ZX1\'   -   Real array   -   none"
X partials at left for ftsurf and c_ftsurf.
.IP "\'ZXM\'   -   Real array   -   none"
X partials at right for ftsurf and c_ftsurf.
.IP "\'ZY1\'   -   Real array   -   none"
Y partials at bottom for ftsurf and c_ftsurf.
.IP "\'ZYN\'   -   Real array   -   none"
Y partials at top for ftsurf and c_ftsurf.
.IP "\'Z11\'   -   Real   -   none"
X-Y partial at lower left for ftsurf and c_ftsurf.
.IP "\'ZM1\'   -   Real   -   none"
X-Y partial at lower right for ftsurf and c_ftsurf.
.IP "\'Z1N\'   -   Real   -   none"
X-Y partial at upper left for ftsurf and c_ftsurf.
.IP "\'ZMN\'   -   Real   -   none"
X-Y partial at upper right for ftsurf and c_ftsurf.
.IP "\'DF1\'   -   Integer   -   1"
Flag indicating if ZX1 is user supplied: 0=yes; 1=no.
.IP "\'DF2\'   -   Integer   -   1"
Flag indicating if ZXM is user supplied: 0=yes; 1=no.
.IP "\'DF3\'   -   Integer   -   1"
Flag indicating if ZY1 is user supplied: 0=yes; 1=no.
.IP "\'DF4\'   -   Integer   -   1"
Flag indicating if ZYN is user supplied: 0=yes; 1=no.
.IP "\'DF5\'   -   Integer   -   1"
Flag indicating if Z11 is user supplied: 0=yes; 1=no.
.IP "\'DF6\'   -   Integer   -   1"
Flag indicating if ZM1 is user supplied: 0=yes; 1=no.
.IP "\'DF7\'   -   Integer   -   1"
Flag indicating if Z1M is user supplied: 0=yes; 1=no.
.IP "\'DF8\'   -   Integer   -   1"
Flag indicating if ZMN is user supplied: 0=yes; 1=no.
.SH SEE ALSO
fitgrid,
fitgrid_params,
curv1,
curv2,
curvd, 
curvi,
curvp1,
curvp2,
curvpi, 
curvs, 
curvps, 
kurv1, 
kurv2, 
kurvd, 
kurvp1, 
kurvp2, 
kurvpd,
surf1, 
surf2, 
c_ftcurv, 
c_ftcurvd, 
c_ftcurvi, 
c_ftcurvp, 
c_ftcurvpi,
c_ftcurvs,
c_ftcurvps,
c_ftkurv,
c_ftkurvp, 
c_ftkurvd,
c_ftkurvpd,
c_ftsurf, 
c_ftseti, 
c_ftgeti, 
c_ftsetr, 
c_ftgetr, 
c_ftsetc, 
c_ftgetc,
c_ftsetfa,
c_ftgetfa_size,
c_ftgetfa_data.
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
