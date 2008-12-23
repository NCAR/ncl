.TH Streamlines 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Streamlines - Utility for plotting a streamline representation of
field flow data given two arrays containing the vector field
components on a uniform grid.
.SH SYNOPSIS
The following two calls are the minimum required to create a
streamline field flow plot using the current interface:
.IP \(bu
STINIT - Performs initialization tasks required before STREAM may be
called to create a streamline field flow plot. Information about the
input data arrays is copied into internal common block variables and
the coordinate system mappings and boundaries are established.
.IP \(bu
STREAM - Plots a streamline representation of field flow data, based
on conditions established by STINIT and the current values of a set of
user-modifiable internal parameters associated with the Streamlines
utility.
.PP
All user-modifiable internal parameters have default values; only
those which are to have values different from the default need to be
set. The routines used to set Streamlines parameters are as
follows:
.IP \(bu 
STRSET - Resets all parameters to their initial default values.
.IP \(bu 
STSETI - Sets the value of an internal parameter of type INTEGER.
.IP \(bu 
STSETR - Sets the value of an internal parameter of type REAL.
.PP
In general, once a parameter is given a value by a call to one of
these routines, it retains that value until a similar call resets it.
Retrieve the value of an internal parameter by calling one of the
following routines:
.IP \(bu 
STGETI - Gets the current value of an internal parameter of type
INTEGER.
.IP \(bu 
STGETR - Gets the current value of an internal parameter of type REAL.
.PP
Streamlines supports two obsolete routines for compatibility
with old NCAR Graphics codes. The compatibility mode parameter, CPM,
offers a number of options that allow considerable
flexibility in making the transition to the new version of the
utility. When writing new code you are encouraged not to use these
entry points, since they provide less capability than the standard
Streamlines interface, and may eventually be phased out.
.IP \(bu 
STRMLN - The principal entry point prior to Version 3.2, now a
front end to the STINIT/STREAM interface.
.IP \(bu 
EZSTRM - Front end to STRMLN: a simpler interface for
standard situations, when detailed control of the plot is
not required.
.SH C-BINDING SYNOPSIS
c_stinit
.br
c_stream
.br
c_strset
.br
c_stseti
.br
c_stsetr
.br
c_stgeti
.br
c_stgetr
.br
c_strmln
.br
c_ezstrm
.SH USER-MODIFIABLE INTERNAL ROUTINES
The following routines are not to be called directly. However, you may
modify them in order to customize the behavior of Streamlines. Three 
routines (STUMXY, STUIXY, and STUMTA) are provided to support user-defined
mapping transformations. The fourth (STUMSL) allows you to tailor streamline
masking to your requirements.
.IP \(bu 
STUIXY -
User modifiable routine that inversely maps a single
point on the streamline from user to data coordinate space.
.IP \(bu 
STUMSL -
Default name for a user-definable external subroutine used
to draw masked streamlines. The default version of the
routine draws any polyline all of whose area identifiers
are greater than or equal to zero.
.IP \(bu 
STUMTA -
Given the coordinates of a point on the streamline in data,
user, and NDC space, and the interpolated, normalized
components of the vector at the point relative to data
coordinate space, the user-modifiable routine STUMTA finds
the directional angle of the streamline relative to NDC
space at the point.
.IP \(bu 
STUMXY -
User modifiable routine that maps a single point on
the streamline from data to user coordinate space.
.SH ACCESS 
To use Streamlines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH MESSAGES
In this section are listed, in alphabetical order, all the
error messages that may be written by Streamlines. Each
error message begins with the name of the Streamlines
routine in which an error has been detected, followed by a
dash, followed by the error message itself. These error
messages are written by a call to the error-handling
support routine SETER, with a final argument indicating
that the error is fatal and that execution should be
terminated.
.IP "STGETI OR STGETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Streamlines.
.IP "STGETI OR STGETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "STINIT - U AND/OR V ARRAY DIMENSIONS EXCEEDED"
The STINIT input argument, M, specifying the number of
array elements to use along the first dimension of the U
and V arrays, exceeds the actual first dimension of U 
and/or V, as specified by the input arguments LU and LV.
.IP "STREAM - TOO MANY AREA GROUPS"
The area map passed as the argument, IAM, to STREAM
contains more area groups than allowed by the Streamlines
utility, currently 64. This error can occur only if the
mask to area map parameter, MSK, has a value greater than
0, specifying that streamlines are to be drawn masked to an
area map.
.IP "STREAM - INVALID AREA MAP"
The area map passed as the argument, IAM, to STREAM is
judged to be invalid because it contains a negative value
for the number of area groups. This error can occur only if
the mask to area map parameter, MSK, has a value greater
than 0, specifying that streamlines are to be drawn masked
to an area map.
.IP "STSETI OR STSETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name (\'x\') is not a legal parameter
name known to Streamlines.
.IP "STSETI OR STSETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "STSETI OR STSETR - PARAMETER VALUE OUT OF RANGE - x"
An attempt has been made to set the parameter named \'x\' to
a value outside the range allowed by the Streamlines
utility.
.SH SEE ALSO
Online:
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines_params,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.sp
Hardcopy: NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
