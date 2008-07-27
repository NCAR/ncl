.\"
.\"     $Id: cssgrid_params.m,v 1.4 2008-07-27 03:35:35 haley Exp $
.\"
.TH cssgrid_params 3NCARG "May 2000" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
cssgrid_params - This document briefly describes all the
internal parameters of Cssgrid.
.SH DESCRIPTION 
Each entry below includes the name of a parameter, its Fortran type
(use the obvious equivalents for C), 
its default value, and a short description of the parameter.  
.IP "\'SIG\'   -   Real   -    computed"
value of the tension factor for the splines 
.IP "\'TOL\'   -   Real   -   0.01"
Tolerance to use in calculating gradient differences to terminate the
iteration sequence when computing global gradients 
.IP "\'TTF\'   -   Real   -   0.01"
Tolerance to use in determining how accurately each tension factor should
be calculated to approximate its optimum value 
.IP "\'NLS\'   -   Integer   -   10"
Number of nodes to use in the least squares fit. 
.IP "\'NSG\'   -   Integer   -   10"
Maximum number of iterations to use in the algorithm for computing
automatic tension factors 
.IP "\'ISG\'   -   Integer   -   0"
Flag to revert to calculating automatic tension factors rather than
use a constant tension. Using a constant tension is effected by setting a
value for the control parameter sig. If isg is 0, then a tension
array is used, otherwise a constant tension is used. 
.IP "\'IGR\'   -   Integer   -   1"
Flags whether global or local gradients are used (1 implies
global, anything else implies local). 
.IP "\'MVL\'   -   Real   -   -8."
Fill value to use in returns for NCL functions. Used only
if mvl has been changed from its default. 
.SH SEE ALSO
cssgrid,
csstri,
csvoro,
css2c,
csc2s,
csseti,
csgeti,
cssetr,
csgetr,
cssetd,
csgetd,
cssgridd,
csstrid,
csvorod,
css2cd,
csc2sd,
c_cssgrid,
c_csstri,
c_csvoro,
c_css2c,
c_csc2s,
c_csseti,
c_csgeti,
c_cssetr,
c_csgetr,
c_cssetd,
c_csgetd,
c_cssgridd,
c_csstrid,
c_csvorod,
c_css2cd,
c_csc2sd.
.sp,
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csgrid/cshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
