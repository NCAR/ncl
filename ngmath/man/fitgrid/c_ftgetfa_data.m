.\"
.\"	$Id: c_ftgetfa_data.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftgetfa_data 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftgetfa_data - retrieve array values
.SH FUNCTION PROTOTYPE
float *c_ftgetfa_data(char *);
.SH SYNOPSIS
float *c_ftgetfa_data (pnam)
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter whose value is to be retrieved. 
.SH RETURN VALUE
c_ftgetfa_data returns a pointer to a array of data (or returns a null
pointer if an error occurs). The size of the array can be obtained 
by using c_ftgetfa_size. 
.SH USAGE
c_ftgetfa_data is a called to obtain current values for any of 
the float valued array control parameters. 
.SH ACCESS
To use c_ftgetfa_data, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetfa, c_ftgetfa, c_ftgetfa_size
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
