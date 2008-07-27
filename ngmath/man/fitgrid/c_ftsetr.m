.\"
.\"	$Id: c_ftsetr.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftsetr 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftsetr - set float valued parameters
.SH FUNCTION PROTOTYPE
void c_ftsetr(char *, float);
.SH SYNOPSIS
void c_ftsetr (pnam, fval);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned a float value. 
.IP fval 12
The value to be assigned to the control parameter whose name is 
pointed to by pnam. 
.SH USAGE
c_ftsetr is used to set values for any of the control parameters 
that take float values. The values set by c_ftsetr remain in effect 
until changed by subsequent calls to c_ftsetr. 
.SH ACCESS
To use c_ftsetr, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftgetr
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
