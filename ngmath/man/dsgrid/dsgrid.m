.\"
.\"     $Id: dsgrid.m,v 1.6 2008-07-27 03:35:36 haley Exp $
.\"
.TH Dsgrid 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dsgrid is a package for gridding 3D and 2D randomly-spaced data using
an inverse distance weighted average algorithm.  Single precision and double
precision interfaces exist in both Fortran and C.  
.SH SYNOPSIS
.br
.sp
.sp
FORTRAN single precision
.br
------------------------
.sp
 DSGRID2S  -  primary function for gridding 2D data.
.br
 DSGRID3S  -  primary function for gridding 3D data.
.br
 DSSETI    -  set INTEGER parameter values.
.br
 DSGETI    -  retrieve values for INTEGER parameters.
.br
 DSSETR    -  set REAL parameter values.
.br
 DSGETR    -  retrieve values for REAL parameters.
.br
 DSSETC    -  set CHARACTER valued parameters.
.br
 DSGETC    -  retrieve values for CHARACTER parameters.
.br
 DSPNT2S   -  interpolate 2D data at specified points.
.br
 DSPNT3S   -  interpolate 3D data at specified points.
.sp
Fortran double precision (not supported on UNICOS):
.br
--------------------------------------------------
.sp
 DSGRID2D  -  primary function for gridding 2D data.
.br
 DSGRID3D  -  primary function for gridding 3D data.
.br
 DSSETI    -  set INTEGER parameter values.
.br
 DSGETI    -  retrieve values for INTEGER parameters.
.br
 DSSETRD   -  set DOUBLE PRECISION parameter values.
.br
 DSGETRD   -  retrieve DOUBLE PRECISION parameters.
.br
 DSSETC    -  set CHARACTER valued parameters.
.br
 DSGETC    -  retrieve values for CHARACTER parameters.
.br
 DSPNT2D   -  interpolate 2D data at specified points.
.br
 DSPNT3D   -  interpolate 3D data at specified points.
.sp
C single precision:
.br
------------------
.sp
 c_dsgrid2s  -  primary function for gridding 2D data.
.br
 c_dsgrid3s  -  primary function for gridding 3D data.
.br
 c_dsseti    -  set int parameter values.
.br
 c_dsgeti    -  retrieve values for int parameters.
.br
 c_dssetr    -  set float parameter values.
.br
 c_dsgetr    -  retrieve values for float parameters.
.br
 c_dssetc    -  set char valued parameters.
.br
 c_dsgetc    -  retrieve values for char parameters.
.br
 c_dspnt2s   -  interpolate 2D data at specified points.
.br
 c_dspnt3s   -  interpolate 3D data at specified points.
.sp
C double precision:
.br
------------------
.sp
 c_dsgrid2d  -  primary function for gridding 2D data.
.br
 c_dsgrid3d  -  primary function for gridding 3D data.
.br
 c_dsseti    -  set int parameter values.
.br
 c_dsgeti    -  retrieve values for int parameters.
.br
 c_dssetrd   -  set double parameter values.
.br
 c_dsgetrd   -  retrieve values for double parameters.
.br
 c_dssetc    -  set char valued parameters.
.br
 c_dsgetc    -  retrieve values for char parameters.
.br
 c_dspnt2d   -  interpolate 2D data at specified points.
.br
 c_dspnt3d   -  interpolate 3D data at specified points.
.br
.SH ACCESS 
To use Dsgrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
Individual entries, dsgrid_params, dsgrid_errors.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
