.\"
.\"	$Id: argtai.m,v 1.1 1993-03-11 16:13:42 haley Exp $
.\"
.TH ARGTAI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARGTAI - Gets the area identifiers 
associated with a given point.
.SH SYNOPSIS
CALL ARGTAI (MAP, XCD, YCD, IAI, IAG, MAI, NAI, ICF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argtai (int *map, float xcd, float ycd, int *iai, int *iag, int mai, int *nai, int icf)
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(Integer array, Workspace) - 
The area-map array.
.IP "XCD" 12
(Real, Input) - 
The X coordinate, in the current user 
coordinate system, of a point about which you want to 
obtain information.
.IP "YCD" 12
(Real, Input) - 
The Y coordinate, in the current user 
coordinate system, of a point about which you want to 
obtain information.
.IP "IAI(MAI)" 12
(Integer array, Input/output) - 
Integer array of size MAI, to 
which information about the specified point will be 
returned.
.IP "IAG(MAI)" 12
(Integer array, Input/output) - 
Integer array of size MAI, to 
which information about the specified point will be 
returned.
.IP "MAI" 12
(Integer, Input) - 
Dimension of each of the arrays IAI and 
IAG. MAI \(>= NAI.
.IP "NAI" 12
(Integer, Output) - 
Number of values returned in IAI and 
IAG. NAI equals the number of groups of edges that you 
put in the area map.
.IP "ICF" 12
(Integer, Input) - 
Flag set nonzero by you to indicate that 
the definition of the user coordinate system has been 
changed since the last call to ARGTAI; in this case, calls 
to GETSET must be executed by ARGTAI. If you set the 
flag to zero, Areas assumes that the information 
retrieved previously is still correct and skips calls to 
GETSET.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use ARGTAI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_argtai, load 
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
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
