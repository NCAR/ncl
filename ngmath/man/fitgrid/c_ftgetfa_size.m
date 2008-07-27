.\"
.\"	$Id: c_ftgetfa_size.m,v 1.4 2008-07-27 03:35:38 haley Exp $
.\"
.TH c_ftgetfa_size 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
c_ftgetfa_size - retrieve the size of an array
.SH FUNCTION PROTOTYPE
int c_ftgetfa_size(char *);
.SH SYNOPSIS
int c_ftgetfa_size (pnam);
.SH DESCRIPTION
.IP pnam 12
The name of the control parameter whose value is to be retrieved. 
.SH RETURN VALUE
c_ftgetfa_size returns the size of the array (or returns a 
zero if an error occurs). 
.SH USAGE
c_ftgetfa_size is a called to obtain current sizes for any 
of the float valued control parameters. 
.SH ACCESS
To use c_ftgetfa_size, load the NCAR Graphics library ngmath.
.SH SEE ALSO
fitgrid_params, c_ftsetfa, c_ftgetfa, c_ftgetfa_data 
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
