.\"
.\"     $Id: dssetrd.m,v 1.5 2008-07-27 03:35:37 haley Exp $
.\"
.TH DSSETRD 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DSSETRD - Sets the value of an internal parameter of type DOUBLE PRECISION.
.SH SYNOPSIS
CALL DSSETRD (PNAM, RVAL);
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to be set. 
.IP RVAL 12
A DOUBLE PRECISION value that
is the value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Dsgrid parameters.  For a complete list of parameters available
in this utility, see the dsgrid_params man page.
.SH ACCESS
To use DSSETRD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params,
dsgrid2s,
dsgrid3s,
dsgrid2d,
dsgrid3d,
dspnt2s,
dspnt2d,
dspnt3s,
dspnt3d
dsgetrd.
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
