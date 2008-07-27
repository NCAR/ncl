.\"
.\"	$Id: c_ftseti.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftseti 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftseti - set int valued parameters
.SH FUNCTION PROTOTYPE
void c_ftseti(char *, int);
.SH SYNOPSIS
void c_ftseti (pnam, ival);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned an int value. 
.IP ival 12
The value to be assigned to the control parameter whose name is 
pointed to by pnam. 
.SH USAGE
c_ftseti is used to set values for any of the control parameters 
that take int values. The values set by c_ftseti remain in effect 
until changed by subsequent calls to c_ftseti. 
.SH ACCESS
To use c_ftseti, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftgeti
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
