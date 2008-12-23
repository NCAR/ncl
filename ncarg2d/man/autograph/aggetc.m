.TH AGGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGGETC - 
Allows a user program to retrieve (in effect) the character-string
values of certain single parameters.
.SH SYNOPSIS
CALL AGGETC (TPGN,CUSR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aggetc (char *tpgn, char *cusr, int len)
.SH DESCRIPTION
.IP TPGN 12
(an input expression of type CHARACTER) is a parameter
identifier, as described for AGGETP, above. The specified
parameter must be one of those which intrinsically have
values of type character: 'LINE/END.', 'LABEL/NAME.', 'LINE/
TEXT.', or 'DASH/PATTERN/n.'
.IP CUSR 12
(an output variable of type CHARACTER) receives the
desired character string. If CUSR is too short, only the
beginning of the string will be put in it. If CUSR is too
long, it will be padded with blanks on the end.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.IP len 12
The size of cusr as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of
Autograph parameters.  For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH ACCESS 
To use AGGETC or c_aggetc, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
