.\"
.\"     $Id: c_nnsetrd.m,v 1.3 1998-02-04 15:32:00 haley Exp $
.\"
.TH c_nnsetrd 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_nnsetrd - Set double valued parameters
.SH FUNCTION PROTOTYPE
void c_nnsetrd(char *, double);
.SH SYNOPSIS
void c_nnsetrd(pnam, dval);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP dval 12
A double value to be assigned to the internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use c_nnsetrd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgridd,
c_nngetrd.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 1997-1998
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
