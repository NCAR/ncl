.\"
.\"	$Id: ardrln.m,v 1.1 1993-03-11 16:13:30 haley Exp $
.\"
.TH ARDRLN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARDRLN - Draws a polyline that is masked by a given area
map.
.SH SYNOPSIS
CALL ARDRLN (MAP,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ardrln (int *map, float *xcd, float *ycd, int ncd, float *xcs, float *ycs, int mcs, int *iai, int *iag, int mai, int lpr (float *xcs, float *ycs, int *ncs, int *iai, int *iag, int *nai))
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(Integer array, Workspace) - 
The area-map array.
.IP "XCD(NCD)" 12
(Real array, Input) - 
The X coordinates, in the user 
coordinate system, of the points defining the polyline.
.IP "YCD(NCD)" 12
(Real array, Input) - 
The Y coordinates, in the user 
coordinate system, of the points defining the polyline.
.IP "NCD" 12
(Integer, Input) - 
Number of points defining the polyline.
.IP "XCS(MCS)" 12
(Real array, Workspace) - 
Used in calls to LPR to hold the X 
coordinates of the polyline segment contained in a given 
area.
.IP "YCS(MCS)" 12
(Real array, Workspace) - 
Used in calls to LPR to hold the Y 
coordinates of the polyline segment contained in a given 
area.
.IP "MCS" 12
(Integer, Output) - 
Dimension of each of the arrays XCS and 
YCS. MCS \(<= NCD.
.IP "IAI(MAI)" 12
(Integer array, Input/output) - 
The area identifier array, 
used in calls to LPR.
.IP "IAG(MAI)" 12
(Integer Array, Input/output) - 
The group identifier array, 
used in calls to LPR.
.IP "MAI" 12
(Integer, Input) - 
Dimension of each of the arrays IAI and 
IAG. MAI \(>= n, where n is the number of groups in the area 
map.
.IP "LPR" 12
(Subroutine) - 
An area map line-processing routine that is 
called by ARDRLN and supplied by you. LPR must be 
declared EXTERNAL in the routine calling ARDRLN. 
.sp
SUBROUTINE LPR (XCS,YCS,NCS,IAI,IAG,NAI)
.br 
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
.sp
(CODE TO PROCESS POLYLINE DEFINED BY XCS, YCS, IAI, AND IAG)
.sp
RETURN
.br
END
.RS 12
.IP "XCS(NCS), YCS(NCS)" 12
(Real arrays, Workspace) - 
Hold the X and Y coordinates, in 
the fractional coordinate system, of NCS points defining 
a piece of the original polyline contained in a given area.
.IP "NCS" 12
(Integer, Input) - 
Number of X and Y coordinates in the 
arrays XCS and YCS.
.IP "IAI(NAI), IAG(NAI)" 12
(Integer arrays, Input) - 
Hold NAI pairs of area identifiers 
and group identifiers, respectively, for the area within 
which the piece of the polyline lies. For each value of I 
from 1 to NAI, IAI(I) is the area identifier for the area 
with respect to the group of edges specified by the group 
identifier IAG(I).
.IP "NAI" 12
(Integer, Input) - 
Number of values returned in IAI and IAG. 
NAI equals the number of groups of edges that you put in 
the area map.
.RE
.sp
Before executing the first call to LPR, ARDRLN calls GETSET to
retrieve the current user-system mapping parameters and then
executes the following statement:
.sp
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.sp
This ensures correct results if the normalized device 
coordinates in XCS and YCS are used in calls to such 
routines as GPL and CURVE. LPR may make its own SET 
call to achieve some other effect. Before returning 
control to the calling routine, ARDRLN calls SET again to 
restore the original mapping parameters.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use ARDRLN, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ardrln, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping; 
"NCAR Graphics User's Guide," Version 2.00
.sp
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
