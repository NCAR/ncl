.\"
.\"	$Id: arscam.m,v 1.1 1993-03-11 16:13:50 haley Exp $
.\"
.TH ARSCAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARSCAM - Scans an area map from left to right, extracting
definitions of all the areas in the map.
.SH SYNOPSIS
CALL ARSCAM (MAP, XCS, YCS, MCS, IAI, IAG, MAI, APR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arscam (int *map, float *xcs, float *ycs, int mcs, int *iai, int *iag, int mai, int apr(float *xcs, float *ycs, int *ncs, int *iai, int *iag, int *nai))
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(Integer array, Workspace) - 
The area map that has been 
initialized by a call to ARINAM and to which edges have 
been added by calls to AREDAM. If you did not preprocess 
the area map by calling ARPRAM, ARSCAM calls it before 
doing anything else.
.IP "XCS(MCS), YCS(MCS)" 12
(Real arrays, Workspace) - 
The X and Y coordinates defining 
the edge of a given area, for use by ARSCAM in calls to 
the area-processing routine APR1.
.IP "MCS"
(Integer, Input) - 
Dimension of each of the arrays XCS and 
YCS.
.IP "IAI(MAI), IAG(MAI)" 12
(Integer arrays, Workspace) - 
Hold MAI pairs of area identifiers 
and group identifiers, respectively. Used by ARSCAM in 
calls to the area-processing routine APR. 
.IP "MAI" 12
(Integer, Input) - 
Dimension of each of the arrays IAI and 
IAG. MAI=n, where n is the number of groups in the area 
map.
.IP "APR" 12
(Subroutine) - 
An area-processing routine that is called by 
ARSCAM, and must be declared EXTERNAL in the routine 
that calls ARSCAM. You must supply the routine APR, and it
must have the following form: 
.sp
SUBROUTINE APR (XCS,YCS,NCS,IAI,IAG,NAI)
.br
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
.sp
(CODE TO PROCESS THE AREA DEFINED BY XCS, YCS, IAI, AND IAG)
.sp
RETURN
.br
END
.sp
.RS 12
.IP "XCS(NCS), YCS(NCS)" 12
(Real arrays, Workspace) - 
The X and Y coordinates, in the 
fractional coordinate system, of NCS points defining a 
polygonal area. The last of these points is a duplicate of 
the first. Holes in an area are traced in such a way as to 
maximize the probability of hardware fill working 
properly  
by using vertical lines to get to and 
from the holes and tracing them in the proper direction.
.IP "NCS" 12
(Integer, Input) - 
Number of X and Y coordinates in the 
arrays XCS and YCS.
.IP "IAI(NAI), IAG(NAI)" 12
(Integer arrays, Input) - 
Hold NAI pairs of identifiers for 
the area defined by XCS and YCS. For each value of I from 
1 to NAI, IAI(I) is the area identifier for the area with 
respect to the group of edges specified by the group 
identifier IAG(I).
.IP "NAI" 12
(Integer, Input) - 
Number of values returned in IAI and IAG. NAI 
equals the number of groups of edges that you put in the 
area map.
.RE
.sp
Before executing the first call to APR, ARSCAM calls GETSET to
retrieve the current user coordinates and then
executes the statement:
.sp
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.sp
This ensures correct results if the normalized device 
coordinates in XCS and YCS are used in calls to such 
routines as GFA and FILL. APR may make its own SET call 
to achieve some other effect. Before returning control to 
the calling routine, ARSCAM recalls SET to restore the 
original mapping parameters.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use ARSCAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_arscam, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping; 
"NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
