.TH ARDRLN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARDRLN - Draws a polyline that is masked by a given area
map.
.SH SYNOPSIS
CALL ARDRLN (MAP,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ardrln (int *map, float *xcd, float *ycd, int ncd, float *xcs, 
float *ycs, int mcs, int *iai, int *iag, int mai, int (*lpr)(float *xcs, 
float *ycs, int *ncs, int *iai, int *iag, int *nai))
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(a workspace array, dimensioned LMAP, of type INTEGER) - 
The area-map array.
.IP "XCD(NCD)" 12
(an input array, dimensioned NCD, of type REAL) - 
The X coordinates, in the user 
coordinate system, of the points defining the polyline.
.IP "YCD(NCD)" 12
(an input array, dimensioned NCD, of type REAL) - 
The Y coordinates, in the user 
coordinate system, of the points defining the polyline.
.IP "NCD" 12
(an input expression of type INTEGER) - 
Number of points defining the polyline.
.IP "XCS(MCS)" 12
(a workspace array, dimensioned MCS, of type REAL) - 
Used by ARDRLN in calls to LPR, the user-supplied 
line-processing routine to hold the X coordinates of the 
polyline segment contained in a given area.
.IP "YCS(MCS)" 12
(a workspace array, dimensioned MCS, of type REAL) - 
Used by ARDRLN in calls to the user-supplied line-processing 
routine to hold the Y coordinates of the polyline segment 
contained in a given area.
.IP "MCS" 12
(an output expression of type INTEGER) - 
Dimension of each of the arrays XCS and YCS.
.IP "IAI(MAI)" 12
(an input or output array, dimensioned MAI, of type INTEGER) - 
The area identifier array, used in calls to LPR, the user-supplied 
line-processing routine.
.IP "IAG(MAI)" 12
(an input or output array, dimensioned MAI, of type INTEGER) - 
The group identifier array, used in calls to LPR, the user-supplied 
line-processing routine.
.IP "MAI" 12
(an input expression of type INTEGER) - 
Dimension of each of the arrays IAI and 
IAG. MAI must be greater than or equal to n, where n is the number 
of groups in the area map.
.IP "LPR" 12
(Subroutine) - 
An area map line-processing routine that you supply for ARDRLN 
to call. LPR must be declared EXTERNAL in the routine that calls 
ARDRLN. LPR must have the following structure:
.sp
SUBROUTINE LPR (XCS, YCS, NCS, IAI, IAG, NAI)
.br 
DIMENSION XCS(*), YCS(*), IAI(*), IAG(*)
.sp
(code to process polyline defined by XCS, YCS, IAI, and IAG)
.sp
RETURN
.br
END
.RS 12
.IP "XCS(NCS), YCS(NCS)" 12
(input arrays, dimensioned NCS, of type REAL) - 
Hold the X and Y coordinates, in NDCs, of NCS points defining 
a piece of the original polyline contained in a given area.
.IP "NCS" 12
(an input expression of type INTEGER) - 
Number of X and Y coordinates in the arrays XCS and YCS.
.IP "IAI(NAI), IAG(NAI)" 12
(input arrays, dimensioned NAI, of type INTEGER) - 
Hold NAI pairs of area identifiers 
and group identifiers, respectively, for the area in 
which the piece of the polyline lies. For each value of I 
from 1 to NAI, IAI(I) is the area identifier for the area 
with respect to the group of edges specified by the group 
identifier IAG(I).
.IP "NAI" 12
(an input expression of type INTEGER) - 
Number of values given in IAI and IAG. NAI equals the number 
of groups of edges that you put in the area map.
.RE
.sp
Before executing the first call to LPR, ARDRLN calls GETSET to
retrieve the current user-system mapping parameters and then
executes the following statement:
.sp
.in +5
CALL SET (VPL, VPR, VPB, VPT, VPL, VPR, VPB, VPT, 1)
.in -5
.sp
where VPL, VPR, VPB, and VPT are the viewport left, right, 
bottom, and top coordinates in NDCs.
.sp
This ensures correct results if the NDCs in XCS and YCS are used 
by the area-processing routine APR in calls to such routines as 
GPL and CURVE, and this allows clipping at the edges of the 
viewport. LPR may make its own SET call to achieve some other 
effect. Before returning control to the calling routine, ARDRLN calls 
SET again to restore the original mapping parameters. (APR is 
described in the man page for ARSCAM.)
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
carline,
tareas,
fsppoint.
.SH ACCESS
To use ARDRLN, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_ardrln, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, aredam, argeti, argtai, arinam, 
arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
