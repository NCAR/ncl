.\"
.\"     $Id: nngetslopes.m,v 1.1 1997-03-07 22:59:22 fred Exp $
.\"
.TH NNGETSLOPES 3NCARG "March 1997" UNIX "NCAR GRAPHICS"
.SH NAME
NNGETSLOPES - retrieve the slope at a specified coordinate.
.SH SYNOPSIS
CALL NNGETSLOPES (I,J,SLOPE,IER)
.SH DESCRIPTION
.IP I 12
(Integer, Input) - A subscript indexing the first dimensioned variable 
in the output grid of the most recent call to NATGRIDS. 
.IP J 12
(Integer, Input) - A subscript indexing the second dimensioned variable 
in the output grid of the most recent call to NATGRIDS. 
.IP SLOPE 12
(Real, Output) - The slope at the grid point Z(I,J), where Z is the 
output grid in the most recent call to NATGRIDS. 
.IP IER 12
(Integer, Output) - An error return value. If IER is returned as 0, then 
no errors were detected. If IER is non-zero, then refer to the man
page for natgrid_errors for details.
.SH ACCESS
To use NNGETSLOPES, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrids,
natgrid_params.
.sp
Complete documentation for Natgird is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 1997
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
