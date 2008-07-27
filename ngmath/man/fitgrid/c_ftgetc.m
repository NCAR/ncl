.\"
.\"	$Id: c_ftgetc.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftgetc 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftgetc - retrieve a string valued parameter
.SH FUNCTION PROTOTYPE
void c_ftgetc(char *, char *);
.SH SYNOPSIS
void c_ftgetc (pnam, cval);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter whose value is to be retrieved. 
.IP cval 12
cval will point to a string containing the returned value. The user 
is required to reserve enough space to store the string. 
.SH USAGE
c_ftgetc is a called to obtain current values for any of the 
string valued control parameters.
.SH ACCESS
To use c_ftgetc, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetc
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
