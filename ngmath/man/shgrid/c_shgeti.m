Sh
.\"     $Id: c_shgeti.m,v 1.4 2008-07-27 03:35:41 haley Exp $
.\"
.TH c_shgeti 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_shgeti - Retrieves the value of an internal parameter of type int.
.SH FUNCTION PROTOTYPE
void c_shgeti(char *, int *);
.SH SYNOPSIS
c_shgeti (pnam,ival)
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
Shgrid parameters.  For a complete list of parameters available
in this utility, see the shgrid_params man page.
.SH ACCESS
To use c_shgeti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_shgrid,
c_shgetnp,
shgrid_params,
c_shgrid,
c_shseti.
.sp
Complete documentation for Shgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/shgrid/shhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
