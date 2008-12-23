.TH VVUDMV 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVUDMV - 
This routine is the user-definable external subroutine used
to draw masked vectors. The default version of the routine
draws any polyline all of whose area identifiers are
greater than or equal to zero.
.SH SYNOPSIS
CALL VVUDMV (XCS,YCS,NCS,IAI,IAG,NAI) 
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
(INTEGER array, assumed size NAI, input): Array of 
area-identifier groups.
.IP NAI 12
(INTEGER, input): Number of area identifier groups. The
current version of Vectors supports up to 64 area groups.
.SH USAGE
\'VVUDMV\' is the name given to the default version of the
masked vector drawing routine, and it is also the name
given to the argument through which the external subroutine
is passed to VVECTR. However, you may choose any
acceptable FORTRAN identifier as the name of a user-defined
version of the routine. The substitute routine must have 
an argument list equivalent to the default version of
VVUDMV. Also, whether or not the default version is used,
the subroutine that calls VVECTR should contain an external
declaration of the routine similar to the following:
.in 15
.sp
EXTERNAL VVUDMV
.in -15
.PP
If the MSK parameter is set to the value 1, specifying high precision
masking, Vectors sends one set of X and Y polyline coordinate arrays
to the area masking routine, ARDRLN, for each vector arrow. ARDRLN
subdivides the polyline into pieces such that each smaller polyline
has a single area identifier with respect to each area identifier
group, then makes a call to VVUDMV for each polyline piece.  While the
default version of VVUDMV only checks to see that none of the area
identifiers are negative, a user-defined version could perform more
complicated decision processing based on knowledge of the meaning of
specific area identifier groups and/or area identifier values.  Note
that, before invoking VVUDMV, ARDRLN modifies the user coordinate space by
making the following calls:
.in 15
.sp
CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,LLG)
CALL SET(VPL,VPR,VPB,VPT,VPL,VPR,VPB,VPT,1)
.in -15
.PP
These calls temporarily turn the user to NDC mapping into an identity,
allowing the user to call any of the routines, CURVE, CURVED, or the
GKS routine GPL, to render the polygon piece , without worrying about
a possible non-identity mapping between user and world coordinates.
.sp
If MSK has a value greater than 1, specifying low precision
masking, Vectors calls the routine ARGTAI to get the area
identifiers with respect to the area identifier groups for
a single point that locates the base position of the
vector. Vectors then calls the VVUDMV routine itself,
passing the coordinate arrays for a complete vector arrow.
Thus, a vector arrow whose base position is within an area
to be masked can be eliminated, but an arrow whose base
position is nearby, but outside, a masked area may intrude
into the area. Also, in this case, since faster rendering
is the goal, Vectors does not convert the coordinate arrays
into normalized device coordinates and do the identity SET
call. Therefore, the user should use only CURVE or CURVED
to render the polyline, unless there is no possibility of a
non-identity user to world coordinate mapping.
.sp
The current version of Vectors supports masked drawing with
up to 64 area groups. Vectors will exit with an error
message if an area map with more than 64 groups is passed
to it.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
ffex05,
vvex01.
.SH ACCESS
To use VVUDMV, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
vectors,
vectors_params,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvumxy,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
