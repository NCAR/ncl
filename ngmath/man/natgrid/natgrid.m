.\"
.\"     $Id: natgrid.m,v 1.5 2000-07-13 03:18:06 haley Exp $
.\"
.TH Natgrid 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Natgrid is a package for gridding 2D randomly-spaced data using
a natural neighbor algorithm.  Single precison and double
precision interfaces exist in both Fortran and C.
.SH SYNOPSIS
.sp
FORTRAN single precision
.br
------------------------
.sp
 NATGRIDS     -  primary function for gridding.
.br
 NNSETI       -  set INTEGER parameter values.
.br
 NNGETI       -  retrieve values for INTEGER parameters.
.br
 NNSETR       -  set REAL parameter values.
.br
 NNGETR       -  retrieve values for REAL parameters.
.br
 NNSETC       -  set CHARACTER valued parameters.
.br
 NNGETC       -  retrieve values for CHARACTER parameters.
.br
 NNGETSLOPES  -  get slope values, if calculated.
.br
 NNGETASPECTS -  get aspect values, if calculated.
.br
 NNPNTINITS   -  initiate single point mode.
.br
 NNPNTS       -  interpolate at a single point.
.br
 NNPNTEND     -  terminate single point mode.
.sp
Fortran double precision (not supported on UNICOS):
.br
--------------------------------------------------
.sp
 NATGRIDD     -  primary function for gridding.
.br
 NNSETI       -  set INTEGER parameter values.
.br
 NNGETI       -  retrieve values for INTEGER parameters.
.br
 NNSETRD      -  set DOUBLE PRECISION parameter values.
.br
 NNGETRD      -  retrieve DOUBLE PRECISION parameters.
.br
 NNSETC       -  set CHARACTER valued parameters.
.br
 NNGETC       -  retrieve values for CHARACTER parameters.
.br
 NNGETSLOPED  -  get slope values, if calculated.
.br
 NNGETASPECTD -  get aspect values, if calculated.
.br
 NNPNTINITD   -  initiate single point mode.
.br
 NNPNTD       -  interpolate at a single point.
.br
 NNPNTENDD    -  terminate single point mode.
.sp
C single precision:
.br
------------------
.sp
 c_natgrids     -  primary function for gridding.
.br
 c_nnseti       -  set int parameter values.
.br
 c_nngeti       -  retrieve values for int parameters.
.br
 c_nnsetr       -  set float parameter values.
.br
 c_nngetr       -  retrieve values for float parameters.
.br
 c_nnsetc       -  set char valued parameters.
.br
 c_nngetc       -  retrieve values for char parameters.
.br
 c_nngetslopes  -  get slope values, if calculated.
.br
 c_nngetaspects -  get aspect values, if calculated.
.br
 c_nnpntinits   -  initiate single point mode.
.br
 c_nnpnts       -  interpolate at a single point.
.br
 c_nnpntend     -  terminate single point mode.
.sp
C double precision:
.br
------------------
.sp
 c_natgridd     -  primary function for gridding.
.br
 c_nnseti       -  set int parameter values.
.br
 c_nngeti       -  retrieve values for int parameters.
.br
 c_nnsetrd      -  set double parameter values.
.br
 c_nngetrd      -  retrieve values for double parameters.
.br
 c_nnsetc       -  set char valued parameters.
.br
 c_nngetc       -  retrieve values for char parameters.
.br
 c_nngetsloped  -  get slope values, if calculated.
.br
 c_nngetaspectd -  get aspect values, if calculated.
.br
 c_nnpntinitd   -  initiate single point mode.
.br
 c_nnpntd       -  interpolate at a single point.
.br
 c_nnpntendd    -  terminate single point mode.
.br
.SH ACCESS 
To use Natgrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
Individual entries, natgrid_params, natgrid_errors.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.sp
An NCL (NCAR Command Language) interface also exists for Natgrid procedures.
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

