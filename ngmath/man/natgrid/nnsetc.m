.\"
.\"     $Id: nnsetc.m,v 1.2 1997-05-06 23:38:06 fred Exp $
.\"
.TH NNSETC 3NCARG "March 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NNSETC - Sets the value of an internal parameter of type CHARACTER.
.SH SYNOPSIS
CALL NNSETC (PNAM, CVAL);
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to be set. The name must appear as the first three
characters of the string.
.IP CVAL 12
A character string that 
is the value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This function allows you to set the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use NNSETC, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
natgrids,
nngetc.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 1997
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.

