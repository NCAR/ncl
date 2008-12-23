.TH Vectors 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Vectors - Utility for creating a vector field plot given two arrays
containing the vector components on a uniform grid. In addition, the
user may pass in another array of scalar data defined over the same
grid space independent of but presumably related in some fashion to
the vector data. The utility will then color each vector based on the
scalar value at the grid point. Alternatively, the user can choose to
color the vectors based on vector magnitude.
.SH SYNOPSIS
The following two calls are the minimum required to create a vector
field plot using the current interface:
.IP \(bu
VVINIT - Performs the initialization required for VVECTR to interpret
the input array data properly. VVINIT stores the data array sizes,
sets up various coordinate system mappings, and processes the input
arrays to find the maximum and minimum values. If vector coloring is
enabled and the user has not explicitly taken control, the routine
establishes a linearly scaled color threshold array.
.IP \(bu
VVECTR - Renders the vectors based on conditions established by VVINIT
and the current values of a set of user-modifiable internal parameters
associated with the Vectors utility.
.PP
All user-modifiable internal parameters have default values; only
those which are to have values different from the default need to be
set. The routines used to set Vectors parameters are as
follows:
.IP \(bu 
VVRSET - Resets all internal parameters to their default values.
.IP \(bu
VVSETC - Assigns a value of type CHARACTER to a parameter.
.IP \(bu
VVSETI - Assigns a value of type INTEGER to a parameter.
.IP \(bu
VVSETR - Assigns a value of type REAL to a parameter.
.PP
In general, once a parameter is given a value by a call to one of
these routines, it retains that value until a similar call resets it.
Retrieve the value of an internal parameter by calling one of the
following routines:
.IP \(bu
VVGETC - Returns a parameter value of type CHARACTER.
.IP \(bu
VVGETI - Returns a parameter value of type INTEGER.
.IP \(bu
VVGETR - Returns a parameter value of type REAL.
.IP \(bu
.PP
Vectors supports three obsolete routines for compatibility with
old NCAR Graphics codes. The compatibility mode parameter, CPM, offers
a number of options that allow considerable flexibility
in making the transition to the new version of the utility. When
writing new code you are encouraged not to use these entry points,
since they provide less capability than the standard Vectors
interface, and may eventually be phased out.
.IP \(bu
VELVCT - The principal entry point prior to Version 3.2, now a
front end to the VVINIT/VVECTR interface.
.IP \(bu
EZVEC - Front end to VELVCT: a simpler interface for
standard situations, when detailed control of the plot is
not required.
.IP \(bu
VELVEC - Older version of VELVCT and now also a front end to it.
It differs from the VELVCT call only in that it contains one less
input argument in its calling sequence.
.SH C-BINDING SYNOPSIS
c_vvinit
.br
c_vvectr
.br
c_vvrset
.br
c_vvsetc
.br
c_vvseti
.br
c_vvsetr
.br
c_vvgetc
.br
c_vvgeti
.br
c_vvgetr
.br
c_velvct
.br
c_ezvec
.br
c_velvec
.SH USER-MODIFIABLE INTERNAL ROUTINES
The following routines are not to be called directly. However, you may
modify them in order to customize the behavior of Vectors. VVUMXY is
provided to support user-defined mapping transformations. VVUDMV
allows you to tailor vector masking to your requirements.
.IP \(bu
VVUMXY - Performs a user-definable mapping of vector position
and direction from data coordinate space to normalized device
coordinate space.
.IP \(bu
VVUDMV - The default name of a routine used to draw vectors masked to
an area map. VVUDMV decides which segments of a vector to draw depending
on the area group and group identifier arrays passed to it.
.SH ACCESS 
To use Vectors, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.
.SH MESSAGES
In this section are listed, in alphabetical order, all the
error messages that may be written by Vectors. Each error
message begins with the name of the Vectors routine in
which an error has been detected, followed by a dash,
followed by the error message itself. These error messages
are written by a call to the error-handling support routine
SETER, with a final argument indicating that the error is
fatal and that execution should be terminated.
.IP "VVECTR - TOO MANY AREA GROUPS"
The area map passed as the argument, IAM, to VVECTR
contains more area groups than allowed by the Vectors
utility, currently 64. This error can occur only if the
Mask to Area Map parameter, MSK, has a value greater than
0, specifying that vectors are to be drawn masked to an
area map.
.IP "VVECTR - INVALID AREA MAP"
The area map passed as the argument, IAM, to VVECTR is
judged to be invalid because it contains a negative value
for the number of area groups. This error can occur only if
the Mask to Area Map parameter, MSK, has a value greater
than 0, specifying that vectors are to be drawn masked to
an area map.
.IP "VVGETC - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Vectors.
.IP "VVGETC - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "VVGETI OR VVGETR - GETTING x - PAI INCORRECT"
An attempt has been made to get an element of the parameter
array named \'x\' and the current value of PAI (the
"parameter array index") is inappropriate for that
parameter array.
.IP "VVGETI OR VVGETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Vectors.
.IP "VVGETI OR VVGETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "VVINIT - U AND/OR V ARRAY DIMENSIONS EXCEEDED"
The VVINIT input argument, M, specifying the number of
array elements to use along the first dimension of the U
and V (and possibly P) arrays, exceeds the actual first
dimension of U and/or V, as specified by the input
arguments LU and LV.
.IP "VVINIT - SCALAR ARRAY TOO SMALL"
The value given to LP, the VVINIT input argument specifying
the actual first dimension of the scalar data array, P, is
less than M, the input argument specifying the number of
array elements to use along the first dimension for all of
the input arrays. This error can only occur when the
absolute value of the Color Threshold Level control
parameter, CTV, is set to 2, indicating that coloring is to
be performed according to values contained in the scalar
data array, P.
.IP "VVSETC - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Vectors.
.IP "VVSETC - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "VVSETI OR VVSETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Vectors.
.IP "VVSETI OR VVSETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "VVSETI OR VVSETR - PARAMETER VALUE OUT OF RANGE - x"
An attempt has been made to set the parameter named \'x\' to
a value outside the range allowed by the Vectors utility.
.IP "VVSETI OR VVSETR - SETTING x - PAI INCORRECT"
An attempt has been made to set an element of the parameter
array named \'x\' and the current value of PAI (the
"parameter array index") is inappropriate for that
parameter array.
.SH SEE ALSO
Online:
vectors_params,
ezvec,
velvec,
velvct,
fx,
fy,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
ncarg_cbind.
.sp
Hardcopy: NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
