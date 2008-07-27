.\"
.\"     $Id: c_csgeti.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_csgeti 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_csgeti - Retrieves the value of an internal parameter of type int.
.SH FUNCTION PROTOTYPE
void c_csgeti(char *, int *);
.SH SYNOPSIS
c_csgeti (pnam,ival)
.SH DESCRIPTION 
.IP pnam 12
A string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP ival 12
*ival will be the value currently assigned to the control parameter
whose name is pointed to by pnam. 
.SH USAGE
This routine allows you to retrieve the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the csgrid_params man page.
.SH ACCESS
To use c_csgeti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_csgrid,
csgrid_params,
c_csseti.
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
