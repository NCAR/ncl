'\" t
.TH CPPKCL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPPKCL - Picks a set of contour levels.
.SH SYNOPSIS
CALL CPPKCL (ZDAT, RWRK, IWRK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cppkcl (float *zdat, float *rwrk, int *iwrk) 
.SH DESCRIPTION 
All three arguments are arrays used in the last call to 
CPRECT, CPSPS1, or CPSPS2, the contents of which must not 
have been changed since that call.
.IP ZDAT 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input) is the data array.
.IP RWRK 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input/output) is the real 
workspace array.
.IP IWRK 12
(INTEGER array, dimensioned as specified in the last 
call to CPRECT, CPSPS1, or CPSPS2, input/output) is the 
integer workspace array.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE@@@
Notice that you must call CPPKCL after the initialization 
routines CPRECT, CPSPS1, or CPSPS2 because it depends 
on values set by the initialization routines. Also, since 
modifying contour level options chosen by Conpack must 
come after the initialization routines, this is one of the 
few times when you want to set parameters after the 
Conpack initialization routines. After parameters are 
chosen, contour lines and a background are drawn 
normally.
.PP
If the contour level selection parameter CLS is zero, 
CPPKCL does nothing. If the data field is found to be 
essentially constant by the initialization routines, (the 
constant field flag CFF \(!= 0), then CPPKCL does nothing.
.PP
Calling CPPKCL sets the number of contour levels 
parameter NCL to the number of contour levels picked. 
Elements 1 through NCL of the parameter array CLV 
(contour level values) are set to the levels themselves. 
The Ith elements of the associated parameter arrays are 
set as follows:
.TS
tab (/);
l l.
.sp
CLU = 1 or 3/Contour level use flag 
LLT = ' ' (single blank)/Contour line label text
CLD = '$$$$$$$$$$$$$'/Contour line dash pattern
CLC = -1/Contour line color
LLC = -1/Contour line label color
CLL = 0./Contour line line width
.TE
.sp
Thus, after CPPKCL is called, the situation for each 
contour level is as follows:
.IP \(bu
Contour lines are not drawn yet.
.IP \(bu
The lines are labeled if CLU=3, but not labeled if 
CLU=1.
.IP \(bu
The dash pattern for the level is solid, and neither 
the color of the line, the color of the labels on it, nor its 
width are to be set. 
.IP \(bu
The text of the label associated with the level is, 
as yet, unspecified.
.PP
By resetting elements in these parameter arrays, you 
have complete control over each contour line.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpex01,
cpex02,
cpex03,
cpex06.
.SH ACCESS
To use CPPKCL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_cppkcl, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppklb, cprect, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

