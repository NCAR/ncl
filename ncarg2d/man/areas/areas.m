.TH Areas 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Areas - A set of routines allowing you to create an area map from a set of
edges and then to use the area map for various purposes.
.SH SYNOPSIS
ARINAM - Initializes an area map.
.br
AREDAM - Adds edges to an area map.
.br
ARMVAM - Moves an area map from one integer array to another.
.br
ARPRAM - Preprocesses an area map.
.br
ARSCAM - Obtains definitions of areas created by edges in an area map.
.br
ARDRLN - Draws a polyline masked by a given area map.
.br
ARGTAI - Gets area identifiers associated with a given point.
.br
ARGETI - Retrieves the integer value of an Areas parameter.
.br
ARGETR - Retrieves the real value of an Areas parameter.
.br
ARSETI - Provides a new integer value for an Areas parameter.
.br
ARSETR - Provides a new real value for an Areas parameter.
.br
ARDBPA - For debugging - produces a picture of that part of
the area map that belongs to a specified edge group.
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
To use Areas, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use the Areas 
C-bindings, load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
When error conditions are detected, the support routine SETER
is called. By default, SETER writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution.  It is possible to put SETER into recovery mode and
regain control after a recoverable error (which includes
all of the possible errors).
.sp
The possible error messages are listed below.  All errors are recoverable
in the sense that a user program which has called ENTSR to set recovery
mode will get control back after one of these errors occurs.
.sp
ARDBDA - UNCLEARED PRIOR ERROR
.sp
ARDBPA - BAD POINTERS IN AREA MAP
.sp
ARDBPA - ERROR EXIT FROM GQPLCI
.sp
ARDBPA - ERROR EXIT FROM GQTXCI
.sp
ARDBPA - INITIALIZATION DONE IMPROPERLY
.sp
ARDBPA - UNCLEARED PRIOR ERROR
.sp
ARDRLN - ALGORITHM FAILURE
.sp
ARDRLN - INITIALIZATION DONE IMPROPERLY
.sp
ARDRLN - MAI TOO SMALL
.sp
ARDRLN - UNCLEARED PRIOR ERROR
.sp
AREDAM - AREA-MAP ARRAY OVERFLOW
.sp
AREDAM - INITIALIZATION DONE IMPROPERLY
.sp
AREDAM - UNCLEARED PRIOR ERROR
.sp
ARGETI - PARAMETER NAME NOT KNOWN - X
.sp
ARGETI - PARAMETER NAME TOO SHORT - X
.sp
ARGETI - UNCLEARED PRIOR ERROR
.sp
ARGETR - PARAMETER NAME NOT KNOWN - X
.sp
ARGETR - PARAMETER NAME TOO SHORT - X
.sp
ARGETR - UNCLEARED PRIOR ERROR
.sp
ARGTAI - ALGORITHM FAILURE
.sp
ARGTAI - INITIALIZATION DONE IMPROPERLY
.sp
ARGTAI - MAI TOO SMALL
.sp
ARGTAI - UNCLEARED PRIOR ERROR
.sp
ARINAM - AREA-MAP ARRAY IS TOO SMALL
.sp
ARINAM - UNCLEARED PRIOR ERROR
.sp
ARINIT - VALUE OF 'LC' IS TOO LARGE
.sp
ARMPIA - MULTIPLE-PRECISION QUANTITY IS TOO BIG
.sp
ARMVAM - INITIALIZATION DONE IMPROPERLY
.sp
ARMVAM - NEW AREA-MAP ARRAY IS TOO SMALL
.sp
ARMVAM - UNCLEARED PRIOR ERROR
.sp
ARPRAM - ALGORITHM FAILURE
.sp
ARPRAM - AREA-MAP ARRAY OVERFLOW
.sp
ARPRAM - INITIALIZATION DONE IMPROPERLY
.sp
ARPRAM - NO EDGES IN AREA MAP
.sp
ARPRAM - UNCLEARED PRIOR ERROR
.sp
ARSCAM - ALGORITHM FAILURE
.sp
ARSCAM - AREA-MAP ARRAY OVERFLOW
.sp
ARSCAM - INITIALIZATION DONE IMPROPERLY
.sp
ARSCAM - MAI TOO SMALL
.sp
ARSCAM - MCS TOO SMALL
.sp
ARSCAM - UNCLEARED PRIOR ERROR
.sp
ARSETI - PARAMETER NAME NOT KNOWN - X
.sp
ARSETI - PARAMETER NAME TOO SHORT - X
.sp
ARSETI - UNCLEARED PRIOR ERROR
.sp
ARSETR - PARAMETER NAME NOT KNOWN - X
.sp
ARSETR - PARAMETER NAME TOO SHORT - X
.sp
ARSETR - UNCLEARED PRIOR ERROR
.sp
.SH SEE ALSO
Online:
areas_params, ardbpa, ardrln, aredam, argeti, argetr, argtai, arinam,
armvam, arpram, arscam, arseti, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
