.\"
.\"     $Id: nngetr.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNGETR 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NNGETR - Retrieves the value of an internal parameter of type REAL.
.SH SYNOPSIS
CALL NNGETR (PNAM,RVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP RVAL 12
A REAL variable that is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
.SH USAGE
This routine allows you to retrieve the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use NNGETR, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgrids,
nnsetr.
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
