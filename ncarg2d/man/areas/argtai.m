.TH ARGTAI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARGTAI - Returns the area identifiers 
associated with a given point.
.SH SYNOPSIS
CALL ARGTAI (MAP, XCD, YCD, IAI, IAG, MAI, NAI, ICF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argtai (int *map, float xcd, float ycd, int *iai, int *iag, 
int mai, int *nai, int icf)
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(a workspace array, dimensioned LMAP, of type INTEGER) - 
The area-map array.
.IP "XCD" 12
(an input expression of type REAL) - 
The X coordinate, in the current 
user coordinate system, of a point about which 
you want to obtain information.
.IP "YCD" 12
(an input expression of type REAL) - 
The Y coordinate, in the current user 
coordinate system, of a point about which you want to 
obtain information.
.IP "IAI(MAI)" 12
(an output array, dimensioned MAI, of type INTEGER) - 
The array in which area identifiers for areas containing the 
specified point will be returned.
.IP "IAG(MAI)" 12
(an output array, dimensioned MAI, of type INTEGER) - 
The array in which group identifiers for areas containing 
the specified point will be returned.
.IP "MAI" 12
(an input expression of type INTEGER) - 
Dimension of each of the arrays IAI and 
IAG. MAI must be greater than or equal to NAI.
.IP "NAI" 12
(an output expression of type INTEGER) - 
Number of values returned in IAI and 
IAG. NAI equals the number of groups of edges that you 
put in the area map.
.IP "ICF" 12
(an input expression of type INTEGER) - 
Flag set nonzero by you to indicate that 
the definition of the user coordinate system has been 
changed since the last call to ARGTAI; in this case, calls 
to GETSET must be executed by ARGTAI. If you set the 
flag to zero, Areas assumes that the information 
retrieved previously is still correct and skips calls to 
GETSET. This option is valuable if you are 
planning to make thousands of calls to ARGTAI 
with the same area map.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
carfill,
carline,
carmap,
cbex01,
tareas,
fsppoint.
.SH ACCESS
To use ARGTAI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_argtai, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, arinam, 
arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
