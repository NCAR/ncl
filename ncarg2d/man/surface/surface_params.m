.TH Surface_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Surface_params - This document briefly describes the Surface
internal parameters.
.SH DESCRIPTION 
There are no parameter access routines available for the Surface utility. 
Therefore,
parameters must be set through the common blocks SRFBLK and SRFIP1.
.sp
In order to access the internal parameters, you must declare the following
common blocks for the Surface parameters in your code.
.sp
.nf
 COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
1                LL         ,FACT       ,IROT       ,NDRZ       ,
2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
 COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
3                CLO        ,CINC       ,ISPVAL
.fi
.sp
Note that not all of
the variables declared above are considered user-modifiable parameters.  
Definitions of only user-modifiable parameters follow:
.sp
.nf

.IP IFR 12
\-1   Call FRAME first.
.br
 0   Do not call FRAME.
.br
\+1   Call FRAME when done (default).


.IP ISTP 12
\-1   Draws alternating frames, slightly offset 
.br
     (For movies, IROTS = 0).
 0   Blank frame between for stereo slide (IROTS = 1)
.br
     (default).
.br
\+1   Both on same frame. (left picture to left 
.br
     side. IROTS = 0).
     

.IP IROTS 12
 0   +Z in vertical plotting direction (CINE mode)
.br
     (default).
.br
\+1   +Z in horizontal plotting direction (COMIC mode).


.IP IDRX 12
\+1   Draw lines of constant X (default).
.br
 0   Do not.

.IP IDRY 12
\+1   Draw lines of constant Y (default).
.br
 0   Do not.

.IP IDRZ 12
\+1   Draw lines of constant Z (contour lines).
.br
 0   Do not (default).

.IP IUPPER 12
\+1   Draw upper side of surface.
.br
 0   Draw both sides (default).
.br
\-1   Draw lower side.

.IP ISKIRT 12
\+1   Draw a skirt around the surface.
.br
     BOTTOM = HSKIRT.
.br
 0   Do not (default).

.IP NCLA 12
     Approximate number of levels of constant Z
.br
     that are drawn if levels are not specified.
.br
     40 is the maximum and 6 is the default.

.IP THETA 12
     The angle in radians between eyes for stereo pairs.
.br
     The default is \&.02.

.IP HSKIRT 12
     Height of skirt (if ISKIRT = 1). The default is 0.

.IP CHI 12
     Highest level of constant Z. The default is 0.

.IP CLO 12
     Lowest level of constant Z. The default is 0.

.IP CINC 12
     Increment between levels. The default is 0.

.IP IOFFP 12
     Flag to control the use of the special value feature. Do
.br
     not have both IOFFP=1 and ISKIRT=1.
.br
 0   Feature not in use (default).
.br
+1   Feature in use.  No lines are drawn to data points
.br
     that are equal to SPVAL.

.IP SPVAL 12
     Special value used to mark unknown data when 
.br
     IOFFP=1.  The default is 0.

.fi
.SH SEE ALSO
Online:
surface,
ezsrfc,
pwrzs,
setr,
srface.
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
