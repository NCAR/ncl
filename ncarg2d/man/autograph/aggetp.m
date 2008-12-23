.TH AGGETP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGGETP - 
Allows a user program to get the values of a group of
parameters containing one or more elements.
.SH SYNOPSIS
CALL AGGETP (TPGN,FURA,LURA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aggetp (char *tpgn, float *fura, int lura)
.SH DESCRIPTION
.IP TPGN 12
(an input expression of type CHARACTER) is a character
string of the form 'k1/k2/ . . . kn.', where each of the
ki's is a keyword. The keyword k1 specifies a group of
parameters, k2 a subgroup of that group, k3 a subgroup of
that subgroup, etc. The whole string is the name of some
group of parameters the user wishes to set.
.sp
For example, 'AXIS.' is the name of a 92-word group of
parameters describing the four axes, 'AXIS/RIGHT.' is the
name of a 23-word subgroup describing the right Y axis,
.sp
.in +5
\&'AXIS/RIGHT/INTERSECTION.'
.in -5
.sp
is the name of a 2-word further subgroup describing the
intersection of the right Y axis with the bottom of the
grid window, and
.sp
.in +5
\&'AXIS/RIGHT/INTERSECTION/USER.'
.sp
.in -5
is the name of a single parameter specifying the point of
intersection of the right Y axis with the bottom of the
grid window as an X coordinate in the user coordinate
system.
.sp
Obviously, these names can sometimes become rather long.
There are various ways in which they may be shortened.
First, since the fifth and following characters of each
keyword are ignored, they may be omitted; this would shorten
.sp
.in +5
\&'AXIS/RIGHT/INTERSECTION/USER.'
.sp
.in -5
to
.sp
.in +5
\&'AXIS/RIGH/INTE/USER.'
.sp
.in -5
Even fewer characters may be used, as long as no ambiguity
of interpretation arises. To be completely safe, use at
least the first three characters of the group keyword and
at least the first two characters of each subgroup keyword;
this would shorten the example above to 'AXI/RI/IN/US.'.
Moreover, certain group and subgroup keywords may be
omitted entirely; for example, 'AXI/RI/IN/US.' may be
shortened to 'RI/IN/US.'. Keywords which may be entirely
omitted are enclosed in brackets in the headings in the
section "PARAMETERS".
.sp
Names may also be lengthened in various ways in order to
improve their readability. Blanks may be used as desired on
either side of a keyword. Any sequence of characters not
including a slash or a period may be inserted after a
keyword, separated from it by at least one blank. For
example, the name
.sp
.in +5
\&'DASH PATTERN / CHARACTER WIDTH .'
.sp
.in -5
is equivalent to, and considerably more meaningful than,
.sp
.in +5
\&'DAS/CH.' (or even 'DASH/CHARACTER.')
.sp
.in -5
.IP FURA 12
(an input array of type REAL, dimensioned LURA)
contains new values for the parameters in the group
specified by TPGN, in the same order as they appear in the
group. All parameters have real values (because of a
portability problem which arose in implementing the
routines AGSETF and AGSETR). Those which represent
intrinsically integral quantities have a value of the form
"FLOAT(n)", where "n" is the integral quantity being
represented. Some parameters intrinsically take on
character-string values; the real quantity stored as the
value of such a parameter is typically an identifier
allowing for later retrieval of the character string from a
character storage area inside Autograph. The routines
AGSETC and AGGETC may be used to set/get the character-string
values of such parameters.
.IP LURA 12
(an input expression of type INTEGER) is the length of
FURA. Its value may be less than, equal to, or greater
than, the length of the group specified by TPGN. The number
of values transferred into FURA is the minimum of the two
(but not less than one). You may, for example, get the
first two parameters of a 100-parameter group by using LURA
= 2.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine allows you to retrieve the current value of
Autograph parameters.  For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH ACCESS 
To use AGGETP or c_aggetp, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.    To get smoother curves, 
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
