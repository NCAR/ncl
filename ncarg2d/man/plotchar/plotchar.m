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
.SP
PCRSET -
Resets the values of all internal parameters to the default state.
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
c_pcrset
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
ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
Various error conditions can occur in Plotchar.  Each of these results in
a call to the error-handling routine SETER, with a final argument indicating
that the error is recoverable; by default, an error message is printed and
execution is terminated, but, if you turn on error recovery
(as described in the "man" page for "error_handling"), you
can get control back.
.sp
The error messages are as follows; all should be
more or less self-explanatory.
.sp
.in +5
PCDLSC - FIRST COLOR INDEX IS LESS THAN ZERO
.br
PCDLSC - UNCLEARED PRIOR ERROR
.br
PCGETC - UNCLEARED PRIOR ERROR
.br
PCGETC - UNRECOGNIZED PARAMETER NAME
.br
PCGETI - UNCLEARED PRIOR ERROR
.br
PCGETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCGETR - COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCGETR - UNCLEARED PRIOR ERROR
.br
PCGETR - UNRECOGNIZED PARAMETER NAME
.br
PCHIQU - UNCLEARED PRIOR ERROR
.br
PCLOQU - UNCLEARED PRIOR ERROR
.br
PCMEQU - UNCLEARED PRIOR ERROR
.br
PCPNWI - INTERNAL ERROR - SEE CONSULTANT
.br
PCPNWI - PARAMETER NAME TOO SHORT
.br
PCSETC - UNCLEARED PRIOR ERROR
.br
PCSETC - UNRECOGNIZED FONT NAME
.br
PCSETC - UNRECOGNIZED PARAMETER NAME
.br
PCSETI - UNCLEARED PRIOR ERROR
.br
PCSETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCSETR - COLOR ARRAY INDEX IS OUT OF RANGE
.br
PCSETR - UNCLEARED PRIOR ERROR
.br
PCSETR - UNRECOGNIZED PARAMETER NAME
.br
PLCHHQ - DATASET NOT LOADED CORRECTLY
.br
PLCHHQ - ERROR EXIT FROM GQCNTN
.br
PLCHHQ - ERROR EXIT FROM GQFACI
.br
PLCHHQ - ERROR EXIT FROM GQFAIS
.br
PLCHHQ - ERROR EXIT FROM GQLWSC
.br
PLCHHQ - ERROR EXIT FROM GQNT
.br
PLCHHQ - ERROR EXIT FROM GQPLCI
.br
PLCHHQ - ERROR EXIT FROM GQTXCI
.br
PLCHHQ - INTERNAL LOGIC ERROR (NCRA TOO BIG) - SEE CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (NDGU = 0) - SEE CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (NPCS TOO BIG) - SEE CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (XCP1 OR XCP2 = OORV) - SEE CONSULTANT
.br
PLCHHQ - INTERNAL LOGIC ERROR (XCRA/YCRA TOO SMALL) - SEE CONSULTANT
.br
PLCHHQ - UNCLEARED PRIOR ERROR
.br
PLCHLQ - ERROR EXIT FROM GQCHH
.br
PLCHLQ - ERROR EXIT FROM GQCHUP
.br
PLCHLQ - ERROR EXIT FROM GQCNTN
.br
PLCHLQ - ERROR EXIT FROM GQNT
.br
PLCHLQ - ERROR EXIT FROM GQTXAL
.br
PLCHLQ - ERROR EXIT FROM GQTXP
.br
PLCHLQ - UNCLEARED PRIOR ERROR
.br
PLCHMQ - UNCLEARED PRIOR ERROR
.in -5
.sp
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
pcrset,
pcsetc,
pcseti,
pcsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
