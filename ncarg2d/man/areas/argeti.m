.TH ARGETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARGETI - Retrieves an integer parameter in Areas.
.SH SYNOPSIS
CALL ARGETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP "PNAM" 12
(an input expression of type CHARACTER) - 
The name of the parameter that you want to retrieve.
.IP "IVAL" 12
(an input expression of type INTEGER) - 
An integer variable to hold the parameter value.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of Areas 
parameters. For a complete list of parameters available in this 
utility, see the areas_params man page.
.SH ACCESS
To use ARGETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_argeti, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argtai, arinam, 
arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
