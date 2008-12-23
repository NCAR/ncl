.TH IDSFFT 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
IDSFFT - Performs smooth surface fitting when the projections
of the data points in the X-Y plane are irregularly distributed
in the plane.
.SH SYNOPSIS
 CALL IDSFFT (MD, NDP, XD, YD, ZD, MREG, NREG, KREG,
.br
+ XREG, YREG, ZREG, IWK, WK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_idsfft (int md, int ndp, float *xd, float *yd,
.br
float *zd, int mreg, int nreg, int kreg, float *xreg,
.br
float *yreg, float *zreg, int *iwk, float *wk)
.SH DESCRIPTION
.IP "MD" 12
(Integer, Input/output) - 
Mode of computation (must be 1, 
2, or 3).
.RS
.IP "1"
If this is the first call to this subroutine, or if the value 
of NDP has been changed from the previous call, or if
the contents of the XD or YD arrays have been
changed from the previous call.
.IP "2"
If the values of NDP and the XD, YD arrays are
unchanged from the previous call, but new values for 
XREG, YREG are being used. If MD=2 and NDP has been
changed since the previous call to IDSFFT, an error return 
occurs.
.IP "3"
If the values of NDP, MREG, NREG, XD, YD, XREG,
YREG are unchanged from the previous call, that is, if the 
only change on input to IDSFFT is in the ZD array. If
MD=3 and NDP, MREG or NREG has been changed since the
previous call to IDSFFT, an error return occurs.
.sp
Between the call with MD=2 or MD=3 and the preceding 
call, the IWK and WK work arrays should not be disturbed.
.RE
.IP "NDP" 12
(Integer, Input) - 
Number of random data points (must be 4 
or greater).
.IP "XD(NDP)" 12
(Real array, Input) - 
Array of dimension NDP containing
the X coordinates of the data points.
.IP "YD(NDP)" 12
(Real array, Input) - 
Array of dimension NDP containing
the Y coordinates of the data points.
.IP "ZD(NDP)" 12
(Real array, Input) - 
Array of dimension NDP containing
the Z coordinates of the data points.
.IP "MREG" 12
(Integer, Input) - 
Number of output grid points in the  
X-direction (must be 1 or greater).
.IP "NREG" 12
(Integer, Input) - 
Number of output grid points in the  
Y-direction (must be 1 or greater).
.IP "KREG" 12
(Integer, Input) - 
First dimension of ZREG 
as declared in the calling program. KREG
must be greater than or equal to MREG,
else an error return occurs.
.IP "XREG(MREG)" 12
(Real array, Input) - 
Array of dimension MREG containing 
the X coordinates of the output grid points.
.IP "YREG(NREG)" 12
(Real array, Input) - 
Array of dimension NREG containing 
the Y coordinates of the output grid points.
.IP "ZREG(KREG,NREG)" 12
(Real array, Output) - 
Real, two-dimensional array of 
dimension (KREG,NREG), storing the interpolated Z values 
at the output grid points.
.IP "IWK(*)" 12 
(Integer array, Workspace) - Integer work array 
of dimension at least 31 * NDP + MREG * NREG.
.IP "WK(*)" 12 
(Real array, Workspace) - 
Real work array of 
dimension at least 6 * NDP.
.SH ""
Inadequate work space IWK and WK may may cause incorrect results.
.sp
The data points must be distinct and their projections in the
X-Y plane must not be collinear; otherwise, an error return
occurs.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
.SH EXAMPLES
To use IDSFFT routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.
.sp
Use the ncargex command to see the following relevant
examples: 
ccpcldm,
ccpfil,
ccplbam,
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpllw,
ccpnet,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccprc,
ccpscam,
cidsfft,
fsfsgfa,
cbex01.
.SH ACCESS 
To use IDSFFT or c_idsfft, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.
.SH MESSAGES
See the bivar man page for a description of all Bivar error
messages and/or informational messages.
.SH SEE ALSO
Online:
bivar, bivar_params, idbvip, idpltr, idgeti, idgetr, idseti, idsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH ACKNOWLEDGMENTS
Bivar was written by Hiroshi Akima in August 1975 and rewritten
by him in late 1976.  In 1989, a new version of Bivar,
incorporating changes described in a Rocky Mountain Journal of
Mathematics was obtained from Dr. Akima, and included in NCAR
Graphics with his permission.  In 1995, Dave Kennison incorporated
the capability of doing linear interpolation and a different kind
of triangulation, put in a parameter access interface, and wrote a
routine to allow the triangulation to be plotted.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
