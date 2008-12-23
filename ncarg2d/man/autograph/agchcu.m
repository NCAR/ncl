.TH AGCHCU 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGCHCU - 
Provides a way for the user to change the color, intensity,
line style, etc., of curves drawn by Autograph.
.SH SYNOPSIS
SUBROUTINE AGCHCU (IFLG,KDSH)
.SH DESCRIPTION 
.IP IFLG 12
(an input expression of type INTEGER) is zero if a
particular object is about to be drawn, non-zero if it has
just been drawn.
.IP KDSH 12
(an input expression of type INTEGER) is the value of
AGCURV's last argument, as follows:
.RS 12 
.IP "AGCURV called by" 22
Value of KDSH
.IP EZY 22
1
.IP EZXY 22
1
.IP EZMY 22
"n" or "-n", where "n " is
the curve number
.IP EZMXY 22
"n" or "-n", where "n" is
the curve number
.IP "the user program" 22
a user-specified value
.SH USAGE
This routine is not called by the user program, but by
Autograph itself, just before and just after each curve is
drawn. The default version does nothing. The user may
supply his own version to change the color, intensity, line
style, etc. of selected curves.
.sp
Note: A user version of AGCHCU should not call any other
Autograph routine.
.SH ACCESS
To use AGCHCU, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
agchax,
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
