.\"
.\"     $Id: cssetr.m,v 1.1 2000-05-15 23:48:42 fred Exp $
.\"
.TH CSSETI 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CSSETR - Sets the value of an internal parameter of type REAL for
the Cssgrid package.
.SH SYNOPSIS
CALL CSSETR (PNAM,RVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the parameter to be set. 
.IP RVAL 12
A REAL value that is the value to be assigned to the
internal parameter specified by PNAM.
.SH USAGE
This routine allows you to set the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the cssgrid_params man page.
.SH ACCESS
To use CSSETR, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
cssgrid_params, 
csgetr.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 1999-2000
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
