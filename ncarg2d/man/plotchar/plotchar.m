.TH Plotchar 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Plotchar - Allows a user to draw characters of high, medium,
or low quality.  
.SH SYNOPSIS
PCHIQU (or PLCHHQ) -
Draws high quality characters. By default, it uses
the same database as the old NCAR Graphics routine PWRITX,
but it can also use characters from any of the fontcap-defined
databases, it has an improved interface, and it has
many more capabilities than PWRITX.
.sp
PCMEQU (or PLCHMQ) -
Draws characters of "medium quality". It does this
by drawing lines, just as PCHIQU does, but it does not
produce quite such fancy characters. No function codes may
be used. Using PCMEQU to draw a given string of characters
will create a larger metafile than if PCLOQU were used,
which may be a disadvantage. However, it may also be more
dependable, in that it does not depend on capabilities the
translator may or may not have.
.sp
PCLOQU (or PLCHLQ) -
Draws characters of "low quality" by calling the GKS
character-drawing routines. No function codes may be used.
Using PCLOQU to draw a given string of characters will
create a smaller metafile than if PCHIQU or PCMEQU were
used; the results will depend on capabilities of the
translator.
.sp
PCGETC -
Retrieves the character value of a specified internal parameter.
.sp
PCGETI -
Retrieves the integer value of a specified internal parameter.
.sp
PCGETR -
Retrieves the real value of a specified internal parameter.
.sp
PCSETC -
Gives a new character value to a specified internal parameter.
.sp
PCSETI -
Gives a new integer value to a specified internal parameter.
.sp
PCSETR -
Gives a new real value to a specified internal parameter.
.sp
PCPNWI -
A function of type CHARACTER*16 that, given the name of an internal
parameter array and an index, has as its value the name of the specified
element of the internal parameter array.
.sp
PCDLSC -
Defines the default list of "special colors" used by
PCHIQU in drawing certain characters from the filled fonts.
Such a call will define color indices IFCI, IFCI+1, IFCI+2,
\&. . . , IFCI+15 and it will set all elements of the
internal parameter array \'CC\' corresponding to indices 1
through 16. At the moment, although such a call does define a
set of sixteen colors (ranging from blue to red) and set
the elements of \'CC\', it\'s a bit pointless, since there are
no characters for which the special colors are used. In the
future, there will be a few such (like the state highway
symbol, which is normally drawn in a particular pair of
colors); at that time, the routine will be of more use.
.sp
PLCHHQ - An alternate name for the routine PCHIQU.
.sp
PLCHMQ - An alternate name for the routine PCMEQU.
.sp
PLCHLQ - An alternate name for the routine PCLOQU.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_pchiqu
.br
c_pcmequ
.br
c_pcloqu
.br
c_pcgetc
.br
c_pcgeti
.br
c_pcgetr
.br
c_pcsetc
.br
c_pcseti
.br
c_pcsetr
.br
c_pcpnwi
.br
c_pcdlsc
.br
c_plchhq
.br
c_plchmq
.br
c_plchlq
.SH USER-MODIFIABLE INTERNAL ROUTINES
PCMPXY - This routine is normally not called directly by the user
(though it can be). It is called by PCHIQU and by PCMEQU
when the user has set the mapping flag \'MA\' nonzero to
request mapping of characters from one X/Y coordinate
system to another.
.SH ACCESS 
To use Plotchar routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use
the Plotchar
C-binding routines load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
The following error messages may be written by Plotchar.
They are written by means of a call to the error-handling
support routine SETER; in all cases, the final argument in
the call says that the error is fatal and that execution
should be terminated.
.sp
.in +5
PLCHHQ - DATASET NOT LOADED CORRECTLY
.in +5
.sp
Probably indicates an error in the implementation of
Plotchar. See a consultant.
.sp
.in -5
PLCHHQ - INTERNAL LOGIC ERROR (NDGU = 0) - SEE CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (XCRA/YCRA TOO SMALL) - SEE
CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (NPCS TOO BIG) - SEE
CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (NCRA TOO BIG) - SEE
CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (XCP1 OR XCP2 = OORV - SEE
CONSULTANT
.in +5
.sp
An "impossible" situation has arisen. Indicates a compiler
problem, core clobbering, an error in the implementation or
installation of Plotchar, and/or the presence of unfriendly
gremlins. See a consultant.
.sp
.in -5
PLCHHQ - ERROR EXIT FROM GQPLCI
.br
PLCHHQ - ERROR EXIT FROM GQFACI
.br
PLCHHQ - ERROR EXIT FROM GQTXCI
.br
PLCHHQ - ERROR EXIT FROM GQLWSC
.br
PLCHHQ - ERROR EXIT FROM GQFAIS
.sp
.in +5
One of the GKS "query" routines has returned an error flag.
This may mean that GKS is not in the proper state.
.sp
.in -5
PCDLSC - FIRST COLOR INDEX IS LESS THAN ZERO
.sp
.in +5
PCDLSC has been called with an argument whose value is less
than zero, which is illegal.
.sp
.in -5
PCGETC - UNRECOGNIZED PARAMETER NAME
.br
PCGETR - UNRECOGNIZED PARAMETER NAME
.br
PCSETC - UNRECOGNIZED PARAMETER NAME
.br
PCSETR - UNRECOGNIZED PARAMETER NAME
.sp
.in +5
The parameter name used is not recognized. Since PCGETI is
implemented using a call to PCGETR, the second error
message above may result from a call to PCGETI. Similarly,
since PCSETI is implemented using a call to PCSETR, the
fourth error message may result from a call to PCSETI.
.sp
.in -5
PCGETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCSETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE
.sp
.in +5
The parameter name begins with \'BC\' and contains a
subscript that is outside the range 1 to 3, which is the
legal range for that internal parameter.
.sp
.in -5
PCGETR - COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCSETR - COLOR ARRAY INDEX IS OUT OF RANGE
.sp
.in +5
The parameter name begins with \'CC\' and contains a
subscript that is outside the range from 0 to 16, which is
the legal range for that internal parameter.
.sp
.in -5
PCPNWI - PARAMETER NAME TOO SHORT
.br
PCPNWI - INTERNAL ERROR - SEE CONSULTANT
.sp
.in +5
PCPNWI is an internal routine that examines a parameter
name string for an embedded subscript. Both of these error
messages indicate a problem with such a parameter name.
.sp
.in -5
PCSETC - UNRECOGNIZED FONT NAME
.sp
.in +5
If PCSETC is called with first argument \'FN\', the second
argument must be the name of a font to be used. The name of
the font must be spelled exactly as given in Table 2, "Font
Numbers vs. Font Names", above. In particular, the name
must be given in upper case.
.in -5
.SH SEE ALSO
Online:
plotchar_params,
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
pchiqu,
pcloqu,
pcmequ,
pcmpxy,
pcpnwi,
pcsetc,
pcseti,
pcsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
