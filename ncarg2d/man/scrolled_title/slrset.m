.TH SLRSET 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SLRSET - Resets all parameters of Scrolled_title to the initial default values.
.SH SYNOPSIS
CALL SLRSET
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_slrset()
.SH DESCRIPTION 
SLRSET has no arguments.
.SH USAGE
Calling this routine restores all
Scrolled_title parameters to their initial default values.
For a complete list of parameters available
in this utility, see the scrolled_title_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
slex02.
.SH ACCESS
To use SLRSET or c_slrset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
ftitle,
scrolled_title,
scrolled_title_params,
slgeti,
slgetr,
slogap,
slseti,
slsetr,
stitle,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
