.\"
.\"     $Id: c_cssetd.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_cssetd 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_cssetd - set double valued parameters
.SH FUNCTION PROTOTYPE
void c_cssetd(char *, double);
.SH SYNOPSIS
void c_cssetd(pnam, dval);
.SH DESCRIPTION
.IP pnam 12
A string that specifies the name of the parameter to be set.
.IP dval 12
A double value to be assigned to the internal parameter specified by pnam.
.SH USAGE
This routine allows you to set the current value of
Cssgrid parameters.  For a complete list of parameters available
in this utility, see the csgrid_params man page.
.SH ACCESS
To use c_cssetd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
c_csgrid,
c_cssetr,
c_csgetd,
csgrid_params.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csgrid/cshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
