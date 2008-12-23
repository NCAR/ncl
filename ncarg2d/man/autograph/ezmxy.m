.TH EZMXY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZMXY - 
Draws, in a manner determined by the current values of the
control parameters, a complete graph of one or more curves,
each defined by a set of points (XDRA(I),YDRA(I,J)) (or
(XDRA(I),YDRA(J,I)) or (XDRA(I,J),YDRA(I,J)) or
(XDRA(J,I),YDRA(J,I)), depending on the current value of
\&'ROW.'), for I from 1 to NPTS. The curve number J runs from
1 to MANY. The argument GLAB may be used to specify a
"graph label", to be placed at the top of the graph.
.SH SYNOPSIS
CALL EZMXY (XDRA,YDRA,IDXY,MANY,NPTS,GLAB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezmxy(float *xdra, float *ydra, int idxy, int many, \\
.br
int npts, char *glab)
.SH DESCRIPTION
.IP XDRA 12
(an input array of type REAL, dimensioned NPTS or IDXY
x MANY or IDXY x NPTS, depending on the current value of
\&'ROW.') contains curve-point X coordinates. The current
value of 'NULL/1.' (default value "1.E36") may be used in
XDRA to signal missing points; curve segments on either
side of a missing point are not drawn. Note:  Because all
non-missing coordinates are used in figuring the minimum
and maximum user values along a given axis, it is safest to
mark both coordinates as "missing".
.RS
.IP \(bu
If 'ROW.' has the absolute value "1." (the default), XDRA
is singly-dimensioned. It is subscripted by point number.
.IP \(bu
If 'ROW.' has the absolute value "2." or greater, XDRA is
doubly-dimensioned. It is subscripted by point number and
curve number, in that order if 'ROW.' is positive (the
default), and in the reverse order if 'ROW.' is negative.
.RE
.IP YDRA 12
(an input array of type REAL, dimensioned IDXY x MANY
or IDXY x NPTS, depending on the current value of 'ROW.')
contains curve-point Y coordinates. The current value of
\&'NULL/1.' (default value "1.E36") may be used in YDRA to
signal missing points; curve segments on either side of a
missing point are not drawn. Note:  Because all non-missing
coordinates are used in figuring the minimum and maximum
user values along a given axis, it is safest to mark both
coordinates as "missing".
.RS
.IP \(bu
If 'ROW.' is positive (the default), YDRA is subscripted by
point number and curve number, in that order.
.IP \(bu
If 'ROW.' is negative, YDRA is subscripted by curve number
and point number, in that order.
.RE
.IP IDXY 12
(an input expression of type INTEGER) is the first
dimension of the arrays XDRA (if it is doubly-dimensioned)
and YDRA (unconditionally), required by EZMXY in order to
index these arrays properly.
.RS
.IP \(bu
If 'ROW.' is positive (the default), IDXY must be greater
than or equal to NPTS.
.IP \(bu
If 'ROW.' is negative, IDXY must be greater than or equal
to MANY.
.RE
.IP MANY 12
(an input expression of type INTEGER) is the number of
curves to be drawn by EZMXY.
.IP NPTS 12
(an input expression of type INTEGER) is the number of
points defining each curve to be drawn by EZMXY.
.IP GLAB 12
(an input expression of type CHARACTER) defines a new
"graph label". (If the first character of this expression
is "CHAR(0)", no new "graph label" is defined; the current
one will continue to be used.)  A character string defining
a new graph label must either be of the exact length
specified by the current value of 'LINE/MAXIMUM.' (default:
40 characters), or shorter; if shorter, it must be
terminated by the character defined by the current value of
\&'LINE/END.' (default: a '$'). The string becomes the new
text of line number 100 of the label named 'T'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex04,
agex05,
agex07,
agex08,
tagupw,
tautog,
fagaxclr,
fagaxlbl,
fagaxmax,
fagcuclr,
fagcudsh,
fagezmxy,
fagilclr,
fagovrvw.
.SH ACCESS 
To use EZMXY or c_ezmxy, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.    To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH SEE ALSO
Online:
autograph,
autograph_params,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
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
