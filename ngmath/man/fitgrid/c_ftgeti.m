.\"
.\"	$Id: c_ftgeti.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftgeti 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftgeti - retrieve an int valued parameter
.SH FUNCTION PROTOTYPE
void c_ftgeti(char *, int *);
.SH SYNOPSIS
void c_ftgeti (pnam, ival);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter whose value is to be retrieved.
.IP ival 12
*ival will be the value currently assigned to the control parameter whose nam.
.SH USAGE
c_ftgeti is a called to obtain current values for any of the int 
valued control parameters.
.SH ACCESS
To use c_ftgeti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftseti
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
