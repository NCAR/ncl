.TH Vectors 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Vectors - 
Allows a user to plot vector fields
given two arrays containing the components of the vectors
on a uniform grid in a user-defined coordinate system. In
addition, the user may pass in another array of scalar data
defined over the same grid space independent of but
presumably related in some fashion to the vector data. The
utility will then color each vector based on the scalar
value at the same grid point. Alternatively, the user can
choose to color the vectors based on the vector magnitude
at the grid point.
.sp
The old utility name, VELVCT, originated as a contraction
of \'Velocity Vector\'. Beginning with NCAR Graphics Version
3.2, all new routines, whether user-entry points or not,
are six characters long and begin with the key letters,
\'VV\', again standing for \'Velocity Vector\' though of course
any 2-dimensional vector field may serve as input to the
package. The old entry points are still supported through a
compatibility version of the VELVCT subroutine, and some,
though not all, of the features provided by the new
routines are available to these entry points by
appropriately setting the compatibility mode internal
parameter.
.SH SYNOPSIS
The code to draw a vector field plot (unless using the
obsolete entries points discussed at the end of this
section) must include a call to the routine VVINIT followed
at some point by a call to VVECTR. Before and after the
VVINIT call several internal parameter setting routines may
be used to alter the behavior of the vector plotting code.
Some of these parameters must be set before the call to
VVINIT in order to have any (or the correct) effect. In
other cases, however, the user can query the information
VVINIT has acquired about the target data sets using a
parameter getting routine, and then dynamically adjust the
parameter to best fit the requirements of the data, before
calling VVECTR.
.sp
All the internal parameters have default values; only those
which are to have values different from the default need to
be set. The user may call routines to set the values of
parameters as follows:
.IP \(bu 
VVRSET resets all internal parameters to their default
values.
.IP \(bu
VVSETC assigns a value of type CHARACTER to a parameter.
.IP \(bu
VVSETI assigns a value of type INTEGER to a parameter.
.IP \(bu
VVSETR assigns a value of type REAL to a parameter.
.PP
In general, once a parameter is given a value by a call to
one of these routines, it retains that value until a
similar call resets it. Thus, many of the parameter-setting
calls do not need to be repeated for each new vector field
plot.
.sp
The user may retrieve the value of an internal parameter at
any time by calling one of the parameter getting routines:
.IP \(bu
VVGETC returns a parameter value of type CHARACTER.
.IP \(bu
VVGETI returns a parameter value of type INTEGER.
.IP \(bu
VVGETR returns a parameter value of type REAL.
.PP 
Once you have set required parameters and called VVINIT,
drawing the vector field plot is simply a matter of calling
the main entry point VVECTR.
.sp
VVINIT performs the initialization required for VVECTR to
interpret the data properly. The dimensions of the data
arrays are copied to internal variables. Depending on the
current value of certain internal parameters, the grid
space indices are mapped into the X,Y data coordinate
system. The mapping from the user coordinate space to the
to the normalized device coordinate (NDC, sometimes also
known as fractional coordinate) space is also established
using either the routine, GETSET, if a previously
established mapping is to be used, or the routine, SET, if
Vectors needs to set up the mapping. Note, however, that
the mapping from the data to user coordinate system is left
indeterminate at this point. This is because you may choose
from several pre-defined mappings, such as an Ezmap
projection, or implement a user-defined mapping to
implement this transformation. VVINIT and, for that matter,
VVECTR have (almost) no knowledge of the mapping being
performed.
.sp
VVINIT then processes the vector component arrays to find
the maximum and minimum vector magnitudes. If a scalar
array is to be processed, it then extracts the maximum and
minimum scalar data values. If the user is coloring the
vectors and has not predefined a set of threshold values,
the routine sets up an array of values linearly spaced
between the maximum and minimum (scalar data or vector
magnitude) values. The user is responsible for setting up
the color table and defining the number and progression of
the colors to be used. If a zero field condition is
discovered, a flag is set, causing VVECTR not to attempt to
draw any vectors.
.sp
After the VVINIT call the user can retrieve the maximum and
minimum vector magnitudes and/or scalar data values and
based on their value, alter the appearance of the plot by
enlarging the maximum size vector, constraining the
smallest vector to be rendered at some fraction of the
largest vector, or by choosing to eliminate vectors less
than some magnitude. The color threshold values could also
be examined and possibly modified.
.sp
You are now free to call VVECTR, causing the vectors to be
rendered according to the established set up. If a border
around the plot is desired, call PERIM(1,0,1,0). Finally,
to advance the frame, call the SPPS routine FRAME.
.sp
Three Vectors routines are supplied to provide
compatibility with pre-Version 3.2 codes. You may choose
from a variety of compatibility options by setting the
compatibility mode parameter CPM appropriately; this offers
considerable flexibility in making the transition to the
new version of the utility.
.IP \(bu
VELVCT, the pre-Version 3.2 principal entry point, has been
rewritten to act as a front end to the VVINIT/VVECTR
interface. Depending on the setting of the CPM parameter
the input parameter options and/or the common block
variables initialized in the block data subprogram, VELDAT,
may be used to set the corresponding internal parameters.
The CPM parameter also controls whether the old FX,FY and
MXF,MYF vector mapping routines are used instead of the
current mapping routine, VVMPXY. When CPM is set to its
default value, the results of a Vectors call are quite
close to what they would have been pre-Version 3.2,
.IP \(bu
EZVEC is a front end to VELVCT and is unchanged from its
pre-Version 3.2 state. It is a simple interface for
standard situations, when detailed control of the plot is
not required.
.IP \(bu
VELVEC is an older version of VELVCT, now also a front end
to it, and also unchanged for Version 3.2. It differs from
the VELVCT call only in that it contains one less input
parameter in its calling sequence. 
.SH C-BINDING SYNOPSIS
c_ezvec
.br
c_vvectr
.br
c_vvgetc
.br
c_vvgeti
.br
c_vvgetr
.br
c_vvinit
.br
c_vvrset
.br
c_vvsetc
.br
c_vvseti
.br
c_vvsetr
.SH USER-MODIFIABLE INTERNAL ROUTINES
Vectors contains two user-modifiable routines that are not
invoked directly by the user, but by Vectors itself. The
default versions of these routines contain very simple code
that may handle a basic case, but little more. These
routines are as follows:
.IP \(bu
VVUMXY is called when the internal parameter MAP is set to
a value other than zero, one, or two. These three values
are pre-defined mappings, and are handled within the
private code of Vectors. Briefly, the three mappings are 0,
Identity mapping, 1, Ezmap projection, and 2, polar to
rectangular. If MAP is set to any other value, VVUMXY
(Velocity Vector - User Map XY) is invoked, allowing the
user to define a custom mapping from data coordinates to
NDC space. Note that, unlike similar routines in Conpack,
this mapping routine is required to return the location of
both ends of the vector in normalized device coordinates.
If the mapping is non-linear, like the Ezmap projections,
then a technique that effectively maps the instantaneous
tangent angle must be used to map the vector direction.
.IP \(bu
VVUDMV is the default name of an externally defined
function passed as a parameter to VVECTR. It is only
required when the you want to draw the vectors masked by an
area map generated by the routines in the Areas utility.
VVUDMV is responsible for deciding which pieces of a vector
to draw according to its area identifier. The default
version of the routine draws all vector segments that have
positive valued area identifiers.
.SH ACCESS 
To use Vectors, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c and ncarg_loc, preferably in that order.  To use the C bindings, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
