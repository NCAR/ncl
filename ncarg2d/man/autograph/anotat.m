.TH ANOTAT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ANOTAT - 
Changes the values of certain primary control parameters,
purportedly having to do with "annotation" of a graph.
.SH SYNOPSIS
ANOTAT (XLAB,YLAB,LBAC,LSET,NDSH,DSHC)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_anotat(char *xlab, char *ylab, int lbac, int lset, \\
.br
int ndsh, char *dshc[])
.SH DESCRIPTION 
.IP XLAB 12
(an input expression of type CHARACTER) defines a new
"x-axis label". (If the first character of this expression
is "CHAR(0)", no new "x-axis label" is defined; the current
one will continue to be used.) A character string defining
a new X-axis label must either be of the exact length
specified by the current value of 'LINE/MAXIMUM.' (default:
40 characters), or shorter; if shorter, it must be
terminated by the character defined by the current value of
\&'LINE/END.' (default: a '$'). The string becomes the new
text of line number -100 of the label 'B'.
.IP YLAB 12
(an input expression of type CHARACTER) defines a new
"y-axis label". (If the first character of this expression
is "CHAR(0)", no new "y-axis label" is defined; the current
one will continue to be used.) A character string defining
a new Y-axis label must either be of the exact length
specified by the current value of 'LINE/MAXIMUM.' (default:
40 characters), or shorter; if shorter, it must be
terminated by the character defined by the current value of
\&'LINE/END.' (default: a '$'). The string becomes the new
text of line number 100 of the label 'L'.
.IP LBAC 12
(an input expression of type INTEGER), if non-zero,
must have the integer value 1, 2, 3, or 4, the real
equivalent of which is to become the new value of
\&'BACKGROUND.'. (If LBAC is zero, no change is to be made in
the current value.)  The value "1" specifies a perimeter
background, the value "2" a grid background, the value "3"
a half-axis background, and the value "4" no background at
all.
.sp
See the discussion of 'BACKGROUND.', in the 
autograph_params man page.
.IP LSET 12
(an input expression of type INTEGER), if non-zero,
must have the absolute value 1, 2, 3, or 4, the real
equivalent of which is to be stored (by means of a call to
AGSETI) as the new value of 'SET.'. If LSET is zero, no
change is to be made in the current value of 'SET.'.
.sp
See the discussion of 'SET.', in the 
autograph_params man page.
.IP NDSH 12
(an input expression of type INTEGER), if zero,
specifies that no change is to be made in the parameters
which specify the dashed-line patterns to be used for
curves.
.RS
.IP \(bu
If NDSH is non-zero, it specifies an integer value whose
real equivalent is to be stored as the new value of 'DASH/
SELECTOR.' (which has the default value "1.").
.IP \(bu
If NDSH is negative, 'DASH/SELECTOR.' is set negative,
forcing EZMY and EZMXY to use internally-defined
"alphabetic" patterns for the MANY curves drawn ("A" for
the first, "B" for the second, . . ., "Z" for the 26th, "A"
for the 27th, etc.). The routines EZY and EZXY are
unaffected.
.IP \(bu
If NDSH is greater than zero, it must be less than or equal
to 26, and the next argument, DSHC, must contain NDSH
dashed-line patterns comprising the new "user" set of
patterns. The fact that 'DASH/SELECTOR.' is set positive
forces EZMY and EZMXY to use this set of patterns. (The
routines EZY and EZXY always use the first pattern in this
set.)  The contents of the array DSHC are copied to storage
local to Autograph and pointers to them are installed as
the values of 'DASH/PATTERNS/1.', '.../2.', etc.
.RE
.IP ""
See the discussion of 'DASH.', in the 
autograph_params man page.
.IP DSHC 12
(an input array of type CHARACTER, dimensioned NDSH)
is meaningful only when NDSH is greater than zero. In this
case, it must be an array of NDSH character strings, each
of the length specified by the current value of 'DASH/
LENGTH.'. Each character string represents a dashed-line
pattern; dollar signs mean "pen down", quotes mean "pen
up", and other characters mean "draw me".
.sp
See the discussion of 'DASH.', in the 
autograph_params man page.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine sets internal parameter values. 
For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex08,
agex10,
agex13,
bnchmk,
sprevx.
.SH ACCESS
To use ANOTAT or c_anotat, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
