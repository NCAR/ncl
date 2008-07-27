.\"
.\"     $Id: csgetd.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH CSGETD 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CSGETD - Retrieves the value of an internal parameter of type
DOUBLE PRECISION.
.SH SYNOPSIS
CALL CSGETD (PNAM,DVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP DVAL 12
A DOUBLE PRECISION variable that is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
.SH USAGE
This routine allows you to retrieve the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the cssgrid_params man page.
.SH ACCESS
To use CSGETD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
cssgrid_params,
cssetd.
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
