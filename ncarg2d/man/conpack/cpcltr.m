.TH CPCLTR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCLTR - 
Traces the contour lines at a given level and
retrieves them for some sort of user-defined processing.
.SH SYNOPSIS
CALL CPCLTR (ZDAT,RWRK,IWRK,CLVL,IJMP,IRW1,IRW2,NRWK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpcltr (float *zdat, float *rwrk, int *iwrk, 
.br
float clvl, int *ijmp, int *irw1, int *irw2, int *nrwk)
.SH DESCRIPTION 
The first three arguments are arrays used in the last call 
to CPRECT, CPSPS1, or CPSPS2, the contents of which must 
not have been changed since that call.
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
.IP CLEV 12
(REAL, input) is the desired contour level.
.IP IJMP 12
(INTEGER, input/output) is initially set to zero by 
the user. Upon return from CPCLTR, it will be non-zero if 
coordinates of a contour line segment have been placed in 
RWRK; it will be zero if all contour line segments at the 
given level have been generated.
.IP IRW1 12
(INTEGER, output) is a pointer to the place in RWRK 
where X coordinates have been stored. They will be in 
RWRK(IRW1+I) for I = 1, 2, ..., NRWK.
.IP IRW2 12
(INTEGER, output) is a pointer to the place in RWRK 
where Y coordinates have been stored. They will be in 
RWRK(IRW2+I) for I = 1, 2, ..., NRWK.
.IP NRWK 12
(INTEGER, output) is the number of points on the 
contour line segment. The points comprising the segment 
will be (RWRK(IRW1+I),RWRK(IRW2+I)) for I = 1, 2, ... NRWK.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The routine CPCLTR may be called at any time after the initialization call
to CPRECT, CPSPS1, or CPSPS2.  It is called using code like the following:
.sp
.nf
      IJMP=0
.br
  101 CALL CPCLTR (ZDAT,RWRK,IWRK,CLEV,IJMP,IRW1,IRW2,NRWK)
.br
      IF (IJMP.NE.0) THEN
.br
       (PROCESS SEGMENT OF CONTOUR LINE FROM RWRK)
.br
       GO TO 101
.br
      END IF
.sp
.fi
CPCLTR is called repeatedly.  Initially, IJMP is zeroed.  Upon each return
from CPCLTR, if the value of IJMP is non-zero, X and Y coordinates have been
placed in the real workspace array RWRK and the values of IRW1, IRW2, and
NRWK have been set to indicate where the coordinates were put.  After the
coordinates have been processed, CPCLTR is called again.  If, upon return
from CPCLTR, the value of IJMP is zero, all contour lines at the specified
level have been processed.
.sp
If the parameter 'T2D' has a non-zero value, the contour lines are smoothed,
using cubic splines under tension.
.SH ACCESS
To use CPCLTR or c_cpcltr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
