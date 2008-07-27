.\"
.\"     $Id: c_nngeti.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH c_nngeti 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
c_nngeti - Retrieves the value of an internal parameter of type int.
.SH FUNCTION PROTOTYPE
void c_nngeti(char *, int *);
.SH SYNOPSIS
c_nngeti (pnam,ival)
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
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use c_nngeti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nnseti.
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
