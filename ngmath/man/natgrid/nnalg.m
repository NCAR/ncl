.\"
.\"     $Id: nnalg.m,v 1.5 2008-07-27 03:35:40 haley Exp $
.\"
.TH nnalg 1NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
nnalg - a tool for displaying features of the Natgrid algorithm
.SH SYNOPSIS
nnalg algorithmic_data_file
.SH DESCRIPTION 
"nnalg" takes a single argument which is a the name of a
special file created by the Fortran subroutine NATGRIDS or
the C function c_natgrids in the Natgrid package. This file 
contains data appropriate
for displaying various features of the Natgrid algorithm. By
default, this file is not created. In order to create it, you need
to set the parameter adf in the Natgrid package. 
You can additionally override the
default file name of "nnalg.dat" by using the parameter alg.
.sp
To invoke "nnalg", simply type the command: 
.sp
nnalg data_file
.sp
By editing the data file, you have many options as to what is
displayed and what the appearance of the plot looks like. 
.sp
The features that can be displayed are: 
.sp
  1. The input data points 
.br
  2. The natural neighbor circumcircles 
.br
  3. The Delaunay triangulation, derived from the natural
     neighbor circumcircles 
.bo
  4. The Voronoi polygons 
.br
  5. The first order natural neighbors of a given 
     input point (or points) 
.br
  6. The second order natural neighbors of a given input
     point (or points) 
.sp
The first four items in the above list are displayed in the
default case.
.SH USAGE
What gets displayed by nnalg depends on values in the
input data file which can be changed by manual editing.
.SH SEE ALSO
natgrid
.sp
Complete documentation for nnalg is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/algorithm.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
