.\"
.\"     $Id: c_csgetd.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_csgetd 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_csgetd - Retrieves the value of an internal parameter of type double.
.SH FUNCTION PROTOTYPE
void c_csgetd(char *, double *);
.SH SYNOPSIS
c_csgetd (pnam,dval)
.SH DESCRIPTION 
.IP pnam 12
A string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP dval 12
*dval will be the value currently assigned to the control parameter
whose name is pointed to by pnam. 
.SH USAGE
This routine allows you to retrieve the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the csgrid_params man page.
.SH ACCESS
To use c_csgetd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_csgrid,
csgrid_params,
c_cssetd.
c_cssetr.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csgrid/cshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
