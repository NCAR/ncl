.\"
.\"     $Id: c_dssetr.m,v 1.5 2008-07-27 03:35:36 haley Exp $
.\"
.TH c_dssetr 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_dssetr - Set float valued parameters
.SH FUNCTION PROTOTYPE
void c_dssetr(char *, float);
.SH SYNOPSIS
void c_dssetr (pnam, fval);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP fval 12
A float value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Dsgrid parameters.  For a complete list of parameters available
in this utility, see the dsgrid_params man page.
.SH ACCESS
To use c_dssetr, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_dsgrid,
c_dsgrid_params,
c_dsgrid2s,
c_dsgrid3s,
c_dsgrid2d,
c_dsgrid3d,
c_dspnt2s,
c_dspnt2d,
c_dspnt3s,
c_dspnt3d
c_dsgetr.
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
