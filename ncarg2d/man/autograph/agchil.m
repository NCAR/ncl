.TH AGCHIL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGCHIL - 
Provides a way for the user to change the color, intensity,
text style, etc., of the informational labels.
.SH SYNOPSIS
SUBROUTINE AGCHIL (IFLG,LBNM,LNNO)
.SH DESCRIPTION 
.IP IFLG 12
(an input expression of type INTEGER) is zero if a
particular object is about to be drawn, non-zero if it has
just been drawn.
.IP LBNM 12
(an input expression of type CHARACTER) is the name of
the label being drawn.
.IP LNNO 12
(an input expression of type INTEGER) is the number of
the line being drawn.
.SH USAGE
This routine is not called by the user program, but by
Autograph itself, just before and just after each
informational-label line is drawn. The default version does
nothing. The user may supply his own version to change the
appearance of selected lines of text.
.sp
Note:  A user version of AGCHIL should not call any other
Autograph routine.
.SH ACCESS
To use AGCHIL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
agchax,
agchcu,
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
