.TH AGCURV 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGCURV - 
Draws a curve in a manner specified by the current values
of the control parameters - the primary parameters with
default values or with values supplied by the user, and the
secondary parameters with values computed by AGSTUP.
.SH SYNOPSIS
CALL AGCURV (XVEC,IIEX,YVEC,IIEY,NEXY,KDSH)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agcurv (float *xvec, int iiex, float *yvec, 
.br
int iiey, int nexy, int kdsh)
.SH DESCRIPTION
.IP XVEC 12
(a singly-subscripted input array of type REAL), when
IIEX is non-zero, contains NEXY X-coordinate data - curve
point 1 has X coordinate XVEC(1), curve point 2 has X
coordinate XVEC(1+IIEX), curve point 3 has X coordinate
XVEC(1+IIEX*2), etc. When IIEX is zero, the array XVEC is
ignored - curve point 1 has X coordinate "1.", curve point
2 has X coordinate "2.", etc.
.sp
If the value of any X coordinate matches the current value
of 'NULL/1.' (default - "1.E36"), the corresponding point
is considered to be missing - curve segments on either side
of that point are not drawn.
.IP IIEX 12
(an input expression of type INTEGER), if non-zero, is
the index increment between one X coordinate in XVEC and
the next. If IIEX is zero, the array XVEC is ignored, as
described above.
.IP YVEC 12
(a singly-subscripted input array of type REAL) is
just like XVEC, but provides Y coordinate data.
.IP IIEY 12
(an input expression of type INTEGER) is just like
IIEX, but describes the use (or non-use) of YVEC.
.IP NEXY 12
(an input expression of type INTEGER) is the number of
curve points - the number of X/Y coordinate pairs to be
used.
.sp
Note: If 'INVERT.' is given the value "1." (in place of its
default value "0."), AGCURV will behave as if the arguments
XVEC and IIEX had been interchanged with the arguments YVEC
and IIEY, so that X-coordinate values refer to vertical
distances, and Y-coordinate values to horizontal distances,
on the graph. This parameter affects AGSTUP in a similar
manner, thus allowing one to plot "X as a function of Y".
.IP KDSH 12
(an input expression of type INTEGER) specifies the
dashed-line pattern to be used in drawing the curve. (Since
the routines DASHD, FRSTD, VECTD, and LASTD, in the package
Dashline, are used to draw the curve, it may have its own
particular dashed-line pattern.)
If KDSH is zero, the user is assumed to have done his own
call to DASHD; AGCURV will do no such call.
.RS
.IP \(bu 4
If KDSH is zero, the user is assumed to have done his own
call to DASHD; AGCURV will do no such call.
.IP \(bu 4
If KDSH is non-zero and negative, the function 
MOD(-KDSH-1,26)+1 determines which of 26 "alphabetic" patterns is to
be used; each of these generates a solid line interrupted
by one of the letters of the alphabet. The value 1 implies
that an "A" will be used, the value 2 that a "B" will be
used, . . . the value 27 that an "A" will be used again,
etc.
.IP \(bu 4
If KDSH is non-zero and positive, the function 
MOD(KDSH-1,n)+1 determines which of n "user" patterns is to be used;
these n patterns are defined by the parameters in the group
named 'DASH.' - the default values specify one solid-line
pattern.
.RE
.IP ""
Note: The routines EZY and EZXY, which draw one curve per
call, always call AGCURV with KDSH = 1. The routines EZMY
and EZMXY, which draw one or more curves per call, call
AGCURV with KDSH = ISIGN(p,q), where p is the number of the
curve being drawn (p is between 1 and MANY, inclusive) and
q is the current integral value of 'DASH/SELECTOR.'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
agex13.
.SH ACCESS 
To use AGCURV or c_agcurv, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH MESSAGES
See the autograph man page for a description of all Autograph error
messages and/or informational messages.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agpwrt,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
