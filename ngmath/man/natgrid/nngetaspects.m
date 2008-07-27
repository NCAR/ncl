.\"
.\"     $Id: nngetaspects.m,v 1.6 2008-07-27 03:35:40 haley Exp $
.\"
.TH NNGETASPECTS 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.SH NAME
NNGETASPECTS - retrieve the aspect at a specified coordinate.
.SH SYNOPSIS
CALL NNGETASPECTS (I,J,ASPECT,IER)
.SH DESCRIPTION
.IP I 12
(Integer, Input) - A subscript indexing the first dimensioned variable 
in the output grid of the most recent call to NATGRIDS. 
.IP J 12
(Integer, Input) - A subscript indexing the second dimensioned variable 
in the output grid of the most recent call to NATGRIDS. 
.IP ASPECT 12
(Real, Output) - The slope at the grid point Z(I,J), where Z is the 
output grid in the most recent call to NATGRIDS. 
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then 
no errors were detected. If IER is non-zero, then refer to the man
page for natgrid_errors for details.
.SH ACCESS
To use NNGETASPECTS, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrids,
natgrid_parameters.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
