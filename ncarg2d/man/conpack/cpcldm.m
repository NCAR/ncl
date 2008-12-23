.TH CPCLDM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCLDM - 
Draws contour lines masked by an existing area
map. The object of this may be simply to avoid drawing
contour lines through label boxes, but the routine may be
used for more complicated tasks, like limiting the drawing
of contour lines to the ocean areas on an Ezmap background.
.SH SYNOPSIS 
CALL CPCLDM (ZDAT, RWRK, IWRK, IAMA, RTPL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpcldm (float *zdat, float *rwrk, int *iwrk, 
.br
int *iama, int (*rtpl)(float *xcs, float *ycs, int *ncs, 
.br
int *iai, int *iag, int *nai))
.SH DESCRIPTION 
The first three arguments are arrays used in the last call 
to CPRECT, CPSPS1, or CPSPS2, the contents of which must 
not have been changed since that call.
.IP ZDAT 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input) is the data array.
.IP RWRK 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input/output) is the real 
workspace array.
.IP IWRK 12
(INTEGER array, dimensioned as specified in the last 
call to CPRECT, CPSPS1, or CPSPS2, input/output) is the 
integer workspace array.
.IP IAMA 12
(INTEGER array, dimensioned as specified in a call to 
ARINAM, in the package Areas, input/output) is the array 
containing the area map which is to be used to mask the 
contour lines as they are drawn.
.IP RTPL 12
(EXTERNAL subroutine) is the user subroutine which is 
to process the polylines which result from masking the 
generated contour lines and other edges against the area 
map. It must be declared EXTERNAL in the routine which 
calls CPCLDM. It will be called repeatedly and must have 
the following form:
.sp
.nf
	SUBROUTINE RTPL (XCS,YCS,NCS,IAI,IAG,NAI)
.br
  	  DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
.br
  	  ...
.br
  	  (CODE TO PROCESS POLYLINE DEFINED BY ARGUMENTS)
.br
  	  ...
.br
	  RETURN
.br
	END
.sp
.fi
The real arrays XCS and YCS hold the X and Y coordinates of 
NCS points defining a polyline which is to be considered 
for drawing. For each I greater than or equal to 1 and less 
than or equal to NAI, IAI(I) is the area identifier for the 
area in which the polyline lies, relative to the area-identifier
group IAG(I). The X and Y coordinates are all 
normalized device coordinates and it may be assumed that 
the appropriate SET call has been done. If it is decided to 
draw the line, it may be done with a call to the SPPS 
routine CURVE, to the Dashline routine CURVED, or to the 
GKS routine GPL. The value of 'PAI' will have been set to 
the appropriate value (1 through 'NCL', -1, -2, or -3) for 
the line of which the polyline is a part. The color and 
line width will have been set as implied by the values of 
the appropriate elements of the parameter arrays 'CLC' and 
\&'CLL'. The dash pattern will have been defined as implied 
by the value of the appropriate elements of the parameter 
arrays 'CLD' and 'CLU' and the value of the parameter 
\&'DPU'. If a dash pattern is defined, it may be retrieved by 
a "CALL CPGETC ('CTM',CVAL)".
.sp
If the only object of using CPCLDM is to avoid drawing 
contour lines through label boxes, then the Conpack routine 
CPDRPL may be used for RTPL. In the routine that calls 
CPCLDM, insert the declaration
.sp
.nf
	EXTERNAL CPDRPL
.sp
.fi
and then use CPDRPL for the last argument.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The routine CPCLDM may be called at any time after the
initialization call to CPRECT, CPSPS1, or CPSPS2 to draw
contour lines masked by an existing area map.  Actually, CPCLDM
does not draw the lines; it generates them, masks them against
a user-specified area map, and generates calls, one for each
polyline resulting from the masking process, to the user
specified routine RTPL.  Each such polyline lies entirely within
precisely one of the areas defined by the area map.  The routine RTPL may
use the information provided by its arguments, describing the
area the polyline is in, to decide whether or not to draw the
polyline.
.sp
The contour lines generated are those specified by the first
\&'NCL' elements of the parameter arrays 'CLV' and 'CLU'.  If
\&'NCL' is zero, CPPKCL is called to generate these values.
Each element of 'CLV' specifies a contour level
and the corresponding element of 'CLU' specifies whether or not
contour lines are to be generated at that level.  If the
parameter 'T2D' has a non-zero value, the contour lines are
smoothed, using cubic splines under tension.
.sp
If the element of the parameter array 'CLU' corresponding to
\&'PAI' = -1 is non-zero, the edge of the grid is also
generated.  If the element of 'CLU' corresponding to 'PAI' = -2
is non-zero, the edges of special-value areas, if any, are
generated.  If the element of 'CLU' corresponding to 'PAI' = -3
is non-zero, the edges of out-of-range areas, if any, are
generated.  The default values are such that none of these
edges is generated.
.sp
Groups of lines are generated in the following order:
.IP \(bu 3
contour lines for each of the specified levels, in ascending
numeric order,
.IP \(bu 3
the edges of special-value areas, if any,
.IP \(bu 3
the edges of out-of-range areas, if any, and
.IP \(bu 3
the edge of the grid.
.PP
The color, dash pattern, and line width to be used for the
lines drawn may be specified by elements of the parameter
arrays 'CLC', 'CLD', and 'CLL', respectively.  Each of these
contains elements corresponding to values of 'PAI' from 1 to
\&'NCL' and three special elements, corresponding to 'PAI' = -1,
-2, and -3.   Before and after each group of lines is
generated, the routine CPCHCL is called; a user-supplied
version of this routine may override the settings of color,
dash pattern, and line width.  Also, of course, the routine
RTPL, which actually does the drawing, may override the
settings of these quantities.
.sp
You can use the
dash-pattern-usage parameter ('DPU') to affect the pattern used
to draw the lines.  Set the value of 'DPU' as follows:
.IP Value  12 
Description
.IP "< 0 or = 0" 12
Lines are drawn by calling the SPPS routine CURVE.
Lines are all solid and unlabeled; specified dash
patterns are not used.
.IP "> 0" 12
Lines are drawn by calling the Dashline routine
CURVED.  Lines are solid or dashed, depending on the
dash pattern specified by the appropriate element of
\&'CLD'.
.sp
If ABS('LLP') = 1, then the dash pattern for those
lines that are to be labeled is constructed by
replicating, 'DPU' times, the dash pattern specified by
the appropriate element of 'CLD', and then appending to
it the characters specified by the appropriate element
of 'LLT'.
.PP
If, during the last call to CPRECT, CPSPS1, or CPSPS2, the data
being contoured were found to be essentially constant, then no
contour lines are generated; instead, the constant-field label
is written.  Other lines are still generated.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcldm,
ccpllc,
ccpllo,
ccpllp,
ccpllt,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccppole,
ccprc,
ccpscam,
colcon,
cpex01,
cpex02,
cpex03,
cpex04,
cpex06,
cpex08,
cbex01,
vvex01,
fcover,
ffex03,
ffex05.
.SH ACCESS
To use CPCLDM or c_cpcldm, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, 
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
