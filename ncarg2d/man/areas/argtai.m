.TH ARGTAI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARGTAI - Returns the area identifiers 
associated with a given point.
.SH SYNOPSIS
CALL ARGTAI (MAP,XCD,YCD,IAI,IAG,MAI,NAI,ICF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argtai (int *map, float xcd, float ycd, int *iai, int *iag, 
int mai, int *nai, int icf)
.SH DESCRIPTION 
.IP "MAP" 12
(an input/output array of type INTEGER) - An array containing an area map that
has been initialized by a call to ARINAM and to which edges have been added
by calls to AREDAM.  If you did not preprocess the area map by calling
ARPRAM, ARGTAI calls it before doing anything else.
.sp
Note: As part of initializing the area map, ARINAM stores the dimension of
MAP in MAP(1); therefore, the dimension does not have to be given as an
argument in calls to ARGTAI.)
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
.IP "IAI" 12
(an output array, dimensioned MAI, of type INTEGER) - 
The array in which area identifiers for the area containing the
specified point will be returned.
.IP "IAG" 12
(an output array, dimensioned MAI, of type INTEGER) - 
The array in which group identifiers for the area containing
the specified point will be returned.
.IP "MAI" 12
(an input expression of type INTEGER) - 
Dimension of each of the arrays IAI and 
IAG. MAI must be greater than or equal to NAI.
.IP "NAI" 12
(an output expression of type INTEGER) - 
The number of values returned in IAI and 
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
with the same area map: do the first one with ICF = 1 and the
rest with ICF = 0.
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
To use ARGTAI or c_argtai, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, argetr, arinam,
armvam, arpram, arscam, arseti, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
