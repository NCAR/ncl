.\"
.\"     $Id: csseti.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSSETI 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CSSETI - Sets the value of an internal parameter of type INTEGER.
.SH SYNOPSIS
CALL CSSETI (PNAM,IVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the parameter to be set. 
.IP IVAL 12
An INTEGER value that is the value to be assigned to the
internal parameter specified by PNAM.
.SH USAGE
This routine allows you to set the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the cssgrid_params man page.
.SH ACCESS
To use CSSETI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
cssgrid_params, 
cssgeti.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
