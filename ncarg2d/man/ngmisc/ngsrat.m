.TH NGSRAT 3NCARG "October 1996" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGSRAT - save/restore/set NCAR GKS primitive attribute values in toto.
.SH SYNOPSIS
CALL NGSRAT(IOPT, IAT, RAT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngsrat(int iopt, int *iat, float *rat)
.SH DESCRIPTION 
.IP IOPT 12
(an input variable of type INTEGER) indicating the desired action:
.IP "              = 0" 20
save the current settings of all attributes
internal to the subroutine, i.e. not in the IAT and RAT arrays.
.IP "              = 1" 20
restore the context from the most recently saved state.
.IP "              = 2" 20
return the current context in the output arrays IAT (integer 
attributes) and RAT (floating-point attributes) as follows:
.sp
IAT contains integer GKS attribute settings as per:
.IP " " 22
IAT( 1) = Clip indicator
.IP " " 22
IAT( 2) = Line type
.IP " " 22
IAT( 3) = Polyline color index
.IP " " 22
IAT( 4) = Marker type
.IP " " 22
IAT( 5) = Polymarker color index
.IP " " 22
IAT( 6) = Text font
.IP " " 22
IAT( 7) = Text precision
.IP " " 22
IAT( 8) = Text color index
.IP " " 22
IAT( 9) = Text path
.IP " " 22
IAT(10) = Text horizontal alignment
.IP " " 22
IAT(11) = Text vertical alignment
.IP " " 22
IAT(12) = Fill area interior style
.IP " " 22
IAT(13) = Fill are style index
.IP " " 22
IAT(14) = Fill area color index
.IP " " 20
RAT contains REAL GKS attribute settings as per:
.IP " " 22
RAT( 1) = Linewidth scale factor
.IP " " 22
RAT( 2) = Marker scale factor
.IP " " 22
RAT( 3) = Character expansion factor
.IP " " 22
RAT( 4) = Character spacing
.IP " " 22
RAT( 5) = Character height in world coordinates
.IP " " 22
RAT( 6) = Character up vector, X component in world coordinates
.IP " " 22
RAT( 7) = Character up vector, Y component in world coordinates
.IP "              = 3" 20
set the context to the values specified in the IAT and RAT arrays 
(as described above).
.IP IAT 12
An integer array, of length 14, that has meaning only if IOPT 
equals 2 or 3. 
The elements of the IAT array are described in the
above description of IOPT.
.IP RAT 12
A real array, of length 7, that has meaning only if IOPT 
equals 2 or 3.
The elements of the IAT array are described in the
above description of IOPT.
.SH USAGE
If you want to save the state of the NCAR GKS primitive attribute
settings, call NGSRAT with a first argument of 0.  When the first
argument is 0, the IAT and RAT arrays (second and third arguments)
are ignored and the state values are stored internally to NGSRAT.
.sp
To restore the NCAR GKS primitive attribute values to what they
were as of the last time NGSRAT was called with argument IOPT = 0,
then call NGSRAT with first argument of 1.  IAT and RAT are ignored
in this case.
.sp
If you call NGSRAT with a fist argument of 2, then the current
settings of the NCAR GKS primitive attribute values are returned 
in arrays IAT and RAT as per the description of IAT and RAT above.
.sp
If you call NGSRAT with a fist argument of 3, then arrays IAT and
RAT must be supplied and the values therein will be used to set 
the NCAR GKS primitive attribute values as per the description of
IAT and RAT above.
.sp
A common use of NGSRAT would be in conjunction with NGMFTC and
NGREOP for saving the GKS state after writing to one metafile, writing
to a new metafile, and then restoring the GKS state when reopening
the initial metafile.
.SH EXAMPLES
The following sequence:
.nf

        CALL NGSRAT(0,IAT,RAT)

          ... do some setting and drawing

        CALL NGSRAT(1,IAT,RAT)

.fi
would save the values of all NCAR GKS primitive attributes 
before the code that does setting and drawing and then restore the
attribute values after the setting and drawing.
.sp
Use the ncargex command to see the following relevant example: 
pgkex27.
.SH ACCESS
To use NGSRAT or c_ngsrat, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
NGSRAT issues no messages apart from potential illegal setting of
the attribute values when IOPT = 3.
.SH SEE ALSO
Online:
ngmftc(3NCARG),
ngreop(3NCARG),
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
