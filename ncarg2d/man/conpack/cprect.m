.TH CPRECT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPRECT - Initializes the contouring of a rectangular array
of data.
.SH SYNOPSIS
CALL CPRECT (ZDAT, KZDT, MZDT, NZDT, RWRK, LRWK, IWRK, LIWK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cprect (float *zdat, int kzdt, int mzdt, int nzdt, 
.br
float *rwrk, int lrwk, int *iwrk, int liwk) 
.SH DESCRIPTION 
.IP ZDAT 12
(REAL array, dimensioned KZDT x n, where "n" is 
greater than or equal to NZDT, input) is the array of data 
to be contoured.
.IP KZDT 12
(INTEGER, input) is the first dimension of the array 
ZDAT.
.IP MZDT 12
(INTEGER, input) is the first dimension of the array 
of data in ZDAT. MZDT must be less than or equal to KZDT.
.IP NZDT 12
(INTEGER, input) is the second dimension of the array 
of data in ZDAT. NZDT must be less than or equal to the 
declared second dimension of the array ZDAT.
.IP RWRK 12
(REAL array, dimensioned LRWK, input/output) is the 
real work array.
.IP LRWK 12
(INTEGER, input) is the length of RWRK.
.IP IWRK 12
(INTEGER array, dimensioned LIWK, input/output) is the 
integer work array.
.IP LIWK 12
(INTEGER, input) is the length of IWRK.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exceptions:
.IP "zdat(l,kzdt)" 12
zdat is dimensioned l by kzdt, where l \(>= nzdt.
.IP "kzdt" 12
The second dimension of the array zdat. 
.IP "mzdt" 12
The second dimension of the array of data in zdat. mzdt \(<= kzdt. 
.IP "nzdt" 12
The first dimension of the array of data in zdat. nzdt \(<= l, 
where l is the declared first dimension of the array zdat. 
.SH USAGE
The routine CPRECT is called to initialize the process of drawing a contour
plot from a rectangular array of data.  The arguments define the data array,
a real workspace array, and an integer workspace array.  The dimensions of
all the arrays are transferred to variables in COMMON, so that, in calls to
other Conpack routines, those dimensions may be omitted.
.sp
CPRECT initializes the internal pointers that manage workspace use; also,
it decides the appropriate ranges of X and Y coordinates for drawing contour
lines and positioning labels.
.sp
If Conpack is to call SET, appropriate arguments are determined and SET is
called; otherwise, GETSET is called to retrieve the arguments from the user's
call to SET.
.sp
The list of label positions (if any) left over from previous calls to the
package is discarded.
.sp
If contour levels are to be chosen by Conpack, 'NCL' is zeroed so that the
levels will be chosen when they are needed.
.sp
The minimum and maximum values in the data array are located and it is
decided whether the data are essentially constant.
.sp
Numeric-label parameters that depend on the range of values in the data
array are initialized.
.sp
A scale factor may be chosen.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
caredg,
ccpback,
ccpcff,
ccpcfx,
ccpcica,
ccpci,
ccpcis,
ccpcit,
ccpclc,
ccpcld,
ccpcldm,
ccpcldr,
ccpcll,
ccpclu,
ccpdflt,
ccpfil,
ccpga,
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
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpmap,
ccpmovi,
ccpmpxy,
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
ccppole,
ccprc,
ccprect,
ccprwc,
ccprwu,
ccpscam,
ccpset,
ccpspv,
ccpt2d,
ccptitle,
ccpvp,
ccpvs,
cidsfft,
colcon,
cpex01,
cpex02,
cpex03,
cpex04,
cpex05,
cpex06,
cpex07,
cpex08,
cbex01,
vvex01,
tconpa,
fcover,
ffex03,
ffex05,
fsfsgfa.
.SH ACCESS
To use CPRECT or c_cprect, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprset, cpscae, cpsetc,
cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
