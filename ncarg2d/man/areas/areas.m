.\"
.\"	$Id: areas.m,v 1.1 1993-03-11 16:13:33 haley Exp $
.\"
.TH AREAS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
AREAS - A set of routines to create an area map from a set of
edges.
.SH SYNOPSIS
ARINAM - Initializes areas.
.br
AREDAM - Adds edges to an area map.
.br
ARPRAM - Preprocesses an area map.
.br
ARSCAM - Obtains definitions of areas created by edges inserted
into area map.
.br
ARDRLN - Draws a polyline masked by a given area map.
.br
ARGTAI - Gets area identifiers associated with a given point.
.br
ARGETI - Retrieves an Areas integer parameter value.
.br
ARSETI - Sets an Areas integer parameter value.
.br
ARDBPA - For debugging, produces a picture of that part of
the area map that belongs to a specified group.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_arinam
.br
c_aredam
.br
c_arpram
.br
c_arscam
.br
c_ardrln
.br
c_argtai
.br
c_argeti
.br
c_arseti
.br
c_ardbpa
.SH ACCESS
To use AREAS, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use the AREAS C-bindings, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping; 
"NCAR Graphics Autograph, A Graphing Utility," Version 2.00, 
August 1987; "NCAR Graphics User's Guide," Version 2.00; and
"NCAR Graphics Guide to New Utilities," Version 3.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
