.TH STUMSL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STUMSL -
This routine is the user-definable external subroutine used
to draw masked streamlines. The default version of the
routine draws any polyline all of whose area identifiers
are greater than or equal to zero.
.SH SYNOPSIS
CALL STUMSL (XCS,YCS,NCS,IAI,IAG,NAI)
.SH DESCRIPTION 
.IP XCS 12
(REAL array, assumed size NCS, input): Array of X
coordinates of the points defining the polyline with the
given set of area identifiers.
.IP YCS 12
(REAL array, assumed size NCS, input): Array of Y
coordinates of the points defining the polyline with the
given set of area identifiers.
.IP NCS 12
(INTEGER, input): Number of points; assumed size of the
X and Y coordinate arrays, XCS and YCS.
.IP IAI 12
(INTEGER array, assumed size NAI, input): Array of area
identifier values. Each value represents the area
identifier with respect to the area group in the area group
array with the same array index.
.IP IAG 12
(INTEGER array, assumed size NAI, input): Array of area-
identifier groups.
.IP NAI 12
(INTEGER, input): Number of area identifier groups. The
current version of Streamlines supports up to 64 area
groups.
.SH USAGE
\'STUMSL\' is the name given to the default version of the
masked streamline drawing routine, and it is also the name
given to the argument through which the external subroutine
is passed to STREAM. The user, however, can choose any
acceptable FORTRAN identifier as the name of a user-defined
version of the routine. But whatever the name, it must have
an equivalent argument list to the default version of
STUMSL. Also, whether or not the default version is used,
the subroutine that calls STREAM should contain an external
declaration of the routine, such as:
.sp
.RS 10
EXTERNAL STUMSL
.RE
.sp 
If the MSK parameter is greater than 0, signifying that
masking is to be performed, Streamlines sends one or more
sets of X and Y polyline coordinate arrays to the area
masking routine, ARDRLN, for each streamline. ARDRLN
subdivides the polyline into pieces such that each smaller
polyline has a single area identifier with respect to each
area identifier group, then makes a call to STUMSL for each
polyline piece. While the default version of STUMSL only
checks to see that none of the area identifiers are
negative, a user-defined version could perform more
complicated decision processing based on knowledge of the
meaning of specific area identifier groups and/or area
identifier values. Note that, before invoking STUMSL,
ARDRLN converts the coordinates into normalized device
coordinates, and does the following SET call:
.sp
.RS 10
CALL SET(-1.0,1.0,-1.0,1.0,-1.0,1.0,-1.0,1.0,1)
.sp
.RE 
This call temporarily makes the user to NDC mapping an
identity, and allows the user to call any of the routines,
CURVE, CURVED, or the GKS routine GPL, to render the
polygon piece, without worrying about a possible non-
identity mapping between user and world coordinates.
.sp
The current version of Streamlines supports masked drawing
with up to 64 area groups. Streamlines will exit with an
error message if an area map with more than 64 groups is
passed to it.
.SH ACCESS
To use STUMSL, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  
.SH SEE ALSO
Online:
streamlines,
ezstrm,
fx,
fy,
stgeti,
stgetr,
stinit,
stream,
strmln,
strset,
stseti,
stsetr,
stuixy,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
