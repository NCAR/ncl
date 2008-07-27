.\"
.\"     $Id: c_nngetrd.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH c_nngetrd 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nngetrd - retrieves the value of an internal parameter of type double.
.SH FUNCTION PROTOTYPE
void c_nngetrd(char *, double *);
.SH SYNOPSIS
c_nngetrd (pnam, dval)
.SH DESCRIPTION 
.IP pnam 12
A string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP dval 12
*dval is the value currently assigned to the control parameter whose
name is pointed to by pnam. 
.SH USAGE
This routine allows you to retrieve the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use c_nngetrd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnsetrd.
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
