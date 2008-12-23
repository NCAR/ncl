.TH CPBACK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPBACK - Draws a background for a contour plot.  
.SH SYNOPSIS
CALL CPBACK (ZDAT, RWRK, IWRK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpback (float *zdat, float *rwrk, int *iwrk)
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
.SH USAGE
You can call CPBACK at any time after the initialization call
to CPRECT, CPSPS1, or CPSPS2.  What CPBACK does depends on the
value of the internal parameter 'MAP':
.IP \(bu 
If 'MAP' is set to zero, CPBACK draws the perimeter of the
current viewport by calling PERIM, in the utility Gridall,
requesting 'ZDM'-1 major ticks on the horizontal sides of the
perimeter and 'ZDN'-1 major ticks on the vertical sides.  No
minor ticks are drawn.
.IP \(bu 
If 'MAP' is set non-zero, CPBACK does nothing.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpback,
ccpcff,
ccpcfx,
ccpcis,
ccpcit,
ccpclc,
ccpcld,
ccpcldm,
ccpcll,
ccpclu,
ccpdflt,
ccpfil,
ccphand,
ccphcf,
ccphl,
ccphlt,
ccpila,
ccpils,
ccpilt,
ccpklb,
ccplbam,
ccplbdr,
ccpline,
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpllw,
ccpncls,
ccpnet,
ccpnof,
ccpnsd,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccppkcl,
ccprc,
ccprect,
ccprwc,
ccprwu,
ccpscam,
ccpset,
ccpsps1,
ccpsps2,
ccpspv,
ccptitle,
ccpvp,
cidsfft,
cpex01,
fsfsgfa.
.SH ACCESS
To use CPBACK or c_cpback, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online: 
conpack, 
cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
