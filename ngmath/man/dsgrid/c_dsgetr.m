.\"
.\"     $Id: c_dsgetr.m,v 1.5 2008-07-27 03:35:36 haley Exp $
.\"
.TH c_dsgetr 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_dsgetr - Retrieves the value of an internal parameter of type float.
.SH FUNCTION PROTOTYPE
void c_dsgetr(char *, float *);
.SH SYNOPSIS
c_dsgetr (char *pnam, float *fval)
.SH DESCRIPTION 
.IP pnam 12
A string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP fval 12
*fval will be the value currently assigned to the control parameter
whose name is pointed to by pnam. 
.SH USAGE
This routine allows you to retrieve the current value of
Dsgrid parameters.  For a complete list of parameters available
in this utility, see the dsgrid_params man page.
.SH ACCESS
To use c_dsgetr, load the NCAR Graphics library ngmath.
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
c_dssetr.
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
