.TH RESET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
RESET - Initializes the Model Picture bit map to zeros.
.SH SYNOPSIS
CALL RESET
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_reset()
.SH USAGE
The Super option of the Dashline utility controls the generation
of crowded lines on the output graphic.  The concept of a
"Model Picture" is used in which a picture is assigned a 1024 x 1024
pixel representation.  Each pixel is assigned one bit in an array,
ISCREN.  Initially each bit is set to 0 by a call to entry RESET.
When a line is drawn by the Dashline package, all bits representing
pixels in the Model Picture where the line would be drawn are set
to 1.  Before any new line is drawn the Model Picture is checked to
see whether a line has already been drawn (bits already set to 1s)
through this part of the Model Picture.  If not, the line is drawn
and the appropriate bits are set to 1s.
.sp
Therefore, a call to entry RESET should be made before any new picture
is generated which uses the Dashline "Super" option.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashp, 
fdlsmth.
.SH ACCESS
To use RESET, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_reset, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
dashline, dashline_params, curved, dashdb, dashdc, frstd,
lastd, lined, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
