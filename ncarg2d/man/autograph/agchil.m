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
ncarg_c, and ncarg_loc, preferably in that order.
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
