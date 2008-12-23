.TH AGSETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGSETC - 
Allows a user program to (in effect) store a character
string as the value of a specified single parameter.
.SH SYNOPSIS
CALL AGSETC (TPGN,CUSR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agsetc (char *tpgn, char *cusr)
.SH DESCRIPTION
.IP TPGN 12
(an input expression of type CHARACTER) is a parameter
identifier, as described for AGSETP, above. The specified
parameter must be one of those which intrinsically have
values of type character: 'LINE/END.', 'LABEL/NAME.', 'LINE/
TEXT.', or 'DASH/PATTERN/n.'
.IP CUSR 12
(an input expression of type CHARACTER) is the desired
character string.
.RS
.IP \(bu
If 'LINE/END.' is being set, only the first character of
CUSR will be used.
.IP \(bu
If 'LABEL/NAME.' is being set, the length of the string
CUSR will be taken to be "MAX(1,LEN(CUSR))".
.IP \(bu
If the text of a label is being set, CUSR must either be of
the exact length specified by 'LINE/MAXIMUM.' (40
characters, by default) or shorter; if shorter, it must be
terminated by the character defined by 'LINE/END.'
(default: a '$').
.IP \(bu
If a dash pattern is being set, the length of CUSR will be
taken to be the minimum of "LEN(CUSR)" and the value
specified by 'DASH/LENGTH.'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Autograph parameters.  For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex05,
agex06,
agex07,
agex08,
agex09,
agex13,
tagupw,
tautog,
fagaxlbl,
fagovrvw.
.SH ACCESS 
To use AGSETC or c_agsetc, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
