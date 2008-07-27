.\"
.\"     $Id: nnsetr.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNSETR 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NNSETR - Sets the value of an internal parameter of type REAL.
.SH SYNOPSIS
CALL NNSETR (PNAM, RVAL);
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to be set. 
.IP RVAL 12
A REAL value that
is the value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use NNSETR, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params, 
natgrids, 
nngetr.
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
