'\" t
.TH DISPLA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DISPLA - 
Changes the values of certain primary control parameters
purportedly having to do with the "display" of a graph.
.SH SYNOPSIS
CALL DISPLA(LFRA,LROW,LTYP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_displa(int lfra, int lrow, int ltyp)
.SH DESCRIPTION 
.IP LFRA 12
(an input expression of type INTEGER), if non-zero,
must have an integer value, the real equivalent of which is
to become the new value of 'FRAME.'. If LFRA is zero, no
change is to be made in the current value of 'FRAME.'.
.RS
.IP \(bu
The default value of 'FRAME.' is "1.", specifying that each
of the routines EZY, EZXY, EZMY, and EZMXY is to do a frame
advance after drawing a graph.
.IP \(bu
The value "2." specifies no frame advance.
.IP \(bu
The value "3." specifies a frame advance before drawing a
graph.
.RE
.IP LROW 12
(an input expression of type INTEGER), if non-zero,
must have an integer value, the real equivalent of which is
to become the new value of 'ROW.'. If LROW is zero, no
change is to be made in the current value of 'ROW.'. This
parameter affects the way in which the routines EZMY and
EZMXY interpret the arguments XDRA and YDRA, as follows:
.RS
.IP \(bu
If 'ROW.' is positive, the first subscript of YDRA is a
point number and the second subscript is a curve number. If
\&'ROW.' is negative, the order of the subscripts is reversed
(row-wise, rather than column-wise, storage).
.IP \(bu
If the absolute value of 'ROW.' is "1.", XDRA is singly-subscripted;
its subscript is a point number. If the
absolute value of 'ROW.' is "2." or greater, XDRA is doubly-subscripted;
the order of the subscripts is the same as for
YDRA.
.RE
.IP ""
The default value of 'ROW.' is "1.", specifying that XDRA
is singly-subscripted by point number and that YDRA is
doubly-subscripted by point number and curve number, in
that order.
.IP LTYP 12
(an input expression of type INTEGER), if non-zero, is
an integer specifying new values for 'X/LOGARITHMIC.' and
\&'Y/LOGARITHMIC.'. If LTYP is zero, no change is to be made
in the current values.
.RS
.IP \(bu
The parameter 'X/LOGARITHMIC.' has the default value "0.",
specifying a linear mapping of user X coordinates onto the
horizontal axis of the grid window; it may be given either
of the two values "-1." or "+1." to specify a logarithmic
mapping. The value "-1." protects it from being reset as a
side effect of setting 'SET.'. DISPLA generates the value
"0." or "-1.".
.IP \(bu
The parameter 'Y/LOGARITHMIC.' is defined similarly and
affects the mapping of user Y coordinates onto the vertical
axis of the grid window.
.RE
.IP ""
A non-zero LTYP resets these values, as follows:
.sp
.in +10
.TS
tab (%);
c l l.
LTYP%'X/LOGARITHMIC.'%'Y/LOGARITHMIC.'
----%----------------%----------------
1%linear%linear
2%linear%logarithmic
3%logarithmic%linear
4%logarithmic%logarithmic
.TE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine sets internal parameter values. 
For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
agex13.
bnchmk,
splogy,
sprevx.
.SH ACCESS
To use DISPLA or c_displa, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
ezmxy,
ezmy,
ezxy,
ezy
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
