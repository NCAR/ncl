.\"
.\"     $Id: c_nnsetr.m,v 1.2 1997-05-06 23:37:42 fred Exp $
.\"
.TH c_nnsetr 3NCARG "March 1997" UNIX "NCAR GRAPHICS"
.SH NAME
c_nnsetr - Set float valued parameters
.SH FUNCTION PROTOTYPE
void c_nnsetr(char *, float);
.SH SYNOPSIS
void c_nnsetr (pnam, fval);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP fval 12
A float value to be assigned to the
internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use c_nnsetr, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params,
c_natgrids,
c_nngetr.
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
