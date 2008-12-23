.TH AGUTOL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGUTOL - 
Provides a way for the user to change the user-system-to-label-system
mapping for one or more of the four axes.
.SH SYNOPSIS
SUBROUTINE AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP)
.SH DESCRIPTION 
.IP IAXS 12
(an input expression of type INTEGER) is the number of
the axis. The values 1, 2, 3, and 4 imply the left, right,
bottom, and top axes, respectively.
.IP FUNS 12
(an input expression of type REAL) is the value of
\&'AXIS/s/FUNCTION.', which may be used to select the desired
mapping function for the axis IAXS. It is recommended that
the default value (zero) be used to specify the identity
mapping. The value may be integral ("1.", "2.", etc.) and
serve purely to select the code to be executed or it may be
the value of an actual parameter in the equations defining
the mapping.
.IP IDMA 12
(an input expression of type INTEGER) specifies the
direction of the mapping. A value greater than zero
indicates that VINP is a value in the user system and that
VOTP is to be a value in the label system, a value less
than zero the opposite. The mappings in one direction must
be the mathematical inverses of the mappings in the other
direction.
.IP VINP 12
(an input expression of type REAL) is an input value
in one coordinate system.
.IP VOTP 12
(an output variable of type REAL) is an output value
in the other coordinate system.
.SH USAGE
This routine is not called by the user program, but by
Autograph itself. It defines the user-system-to-label-system
mapping for all four axes. The default version makes
the label system match the user system on all four axes.
The user may supply his own version of this routine to
change the mapping on one or more of the axes. Mappings
defined by the subroutine must be continuous and monotonic.
.sp
Note: A user version of AGUTOL should not call any other
Autograph routine.
.SH ACCESS
To use AGUTOL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
autograph,
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
