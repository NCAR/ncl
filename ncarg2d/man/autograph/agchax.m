.TH AGCHAX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGCHAX - 
Provides a way for the user to change the color, intensity,
line style, etc., of various portions of the axes.
.SH SYNOPSIS
SUBROUTINE AGCHAX (IFLG,IAXS,IPRT,VILS)
.SH DESCRIPTION 
.IP IFLG 12
(an input expression of type INTEGER) is zero if a
particular object is about to be drawn, non-zero if it has
just been drawn.
.IP IAXS 12
(an input expression of type INTEGER) is the number of
the axis being drawn. The values 1, 2, 3, and 4 indicate
the left, right, bottom, and top axes, respectively.
.IP IPRT 12
(an input expression of type INTEGER) indicates the
part of the axis being drawn. Possible values are as
follows:
.RS
.IP \(bu
1 implies the line of the axis.
.IP \(bu
2 implies a major tick
.IP \(bu
3 implies a minor tick.
.IP \(bu
4 implies the mantissa of a numeric label.
.IP \(bu
5 implies the exponent of a numeric label.
.RE
.IP VILS 12
(an input expression of type REAL) is the value in the
label system at the point where the part is being drawn.
For IPRT = 1, VILS is zero.
.SH USAGE
This routine is not called by the user program, but by
Autograph itself, just before and just after each of the
objects making up an axis is drawn. The default version
does nothing. The user may supply his own version to change
the color, intensity, line style, etc. of selected portions
of the axis.
.sp
Note: A user version of AGCHAX should not call any other
Autograph routine.
.SH ACCESS
To use AGCHAX, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
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
