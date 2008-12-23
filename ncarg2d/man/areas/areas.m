.TH Areas 3NCARG "April 1995" UNIX "NCAR GRAPHICS"
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
ARDBPA - Produces a picture of a specified portion of the area map.
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
To use the Areas Fortran or C routines, load the NCAR Graphics
libraries ncarg, ncarg_gks, and ncarg_c, preferably in that order.
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
.in +5
ARDBDA - UNCLEARED PRIOR ERROR
.br
ARDBPA - BAD POINTERS IN AREA MAP
.br
ARDBPA - ERROR EXIT FROM GQPLCI
.br
ARDBPA - ERROR EXIT FROM GQTXCI
.br
ARDBPA - INITIALIZATION DONE IMPROPERLY
.br
ARDBPA - UNCLEARED PRIOR ERROR
.br
ARDRLN - ALGORITHM FAILURE
.br
ARDRLN - INITIALIZATION DONE IMPROPERLY
.br
ARDRLN - MAI TOO SMALL
.br
ARDRLN - UNCLEARED PRIOR ERROR
.br
AREDAM - AREA-MAP ARRAY OVERFLOW
.br
AREDAM - INITIALIZATION DONE IMPROPERLY
.br
AREDAM - UNCLEARED PRIOR ERROR
.br
ARGETI - PARAMETER NAME NOT KNOWN - X
.br
ARGETI - PARAMETER NAME TOO SHORT - X
.br
ARGETI - UNCLEARED PRIOR ERROR
.br
ARGETR - PARAMETER NAME NOT KNOWN - X
.br
ARGETR - PARAMETER NAME TOO SHORT - X
.br
ARGETR - UNCLEARED PRIOR ERROR
.br
ARGTAI - ALGORITHM FAILURE
.br
ARGTAI - INITIALIZATION DONE IMPROPERLY
.br
ARGTAI - MAI TOO SMALL
.br
ARGTAI - UNCLEARED PRIOR ERROR
.br
ARINAM - AREA-MAP ARRAY IS TOO SMALL
.br
ARINAM - UNCLEARED PRIOR ERROR
.br
ARINIT - VALUE OF 'LC' IS TOO LARGE
.br
ARMPIA - MULTIPLE-PRECISION QUANTITY IS TOO BIG
.br
ARMVAM - INITIALIZATION DONE IMPROPERLY
.br
ARMVAM - NEW AREA-MAP ARRAY IS TOO SMALL
.br
ARMVAM - UNCLEARED PRIOR ERROR
.br
ARPRAM - ALGORITHM FAILURE
.br
ARPRAM - AREA-MAP ARRAY OVERFLOW
.br
ARPRAM - INITIALIZATION DONE IMPROPERLY
.br
ARPRAM - NO EDGES IN AREA MAP
.br
ARPRAM - UNCLEARED PRIOR ERROR
.br
ARSCAM - ALGORITHM FAILURE
.br
ARSCAM - AREA-MAP ARRAY OVERFLOW
.br
ARSCAM - INITIALIZATION DONE IMPROPERLY
.br
ARSCAM - MAI TOO SMALL
.br
ARSCAM - MCS TOO SMALL
.br
ARSCAM - UNCLEARED PRIOR ERROR
.br
ARSETI - PARAMETER NAME NOT KNOWN - X
.br
ARSETI - PARAMETER NAME TOO SHORT - X
.br
ARSETI - UNCLEARED PRIOR ERROR
.br
ARSETR - PARAMETER NAME NOT KNOWN - X
.br
ARSETR - PARAMETER NAME TOO SHORT - X
.br
ARSETR - UNCLEARED PRIOR ERROR
.in -5
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
