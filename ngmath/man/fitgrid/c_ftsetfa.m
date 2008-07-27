.\"
.\"	$Id: c_ftsetfa.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftsetfa 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
Set float valued array parameters
.SH FUNCTION PROTOTYPE
int c_ftsetfa(char *pnam, int n, float *far);
.SH SYNOPSIS
int c_ftsetfa (pnam, n, far);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter to be assigned a float array value. 
.IP n 12
The number of float values in the array. 
.IP far 12
A pointer to the n-element input array. 
.SH USAGE
c_ftsetra is used to set values for any of the control parameters that 
take floating arrays. The values set by
c_ftsetfa remain in effect until changed by subsequent calls to c_ftsetfa .
.SH ACCESS
To use c_ftsetfa, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetfa, c_ftgetfa_size, c_ftgetfa_data 
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
