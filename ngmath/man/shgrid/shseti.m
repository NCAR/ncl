.\"
.\"     $Id: shseti.m,v 1.4 2008-07-27 03:35:42 haley Exp $
.\"
.TH SHSETI 3NCARG "September 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SHSETI - Sets the value of an internal parameter of type INTEGER.
.SH SYNOPSIS
CALL SHSETI (PNAM,IVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the parameter to be set. 
.IP IVAL 12
An INTEGER value that is the value to be assigned to the
internal parameter specified by PNAM.
.SH USAGE
This routine allows you to set the current value of
Shgrid parameters.  For a complete list of parameters available
in this utility, see the shgrid_params man page.
.SH ACCESS
To use SHSETI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
shgrid,
shgrid_params, 
shgeti, 
shgetnp.
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
