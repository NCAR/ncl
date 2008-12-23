'\" t
.TH AGSTUP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.sp
.SH NAME
AGSTUP - 
Performs "set-up" tasks required before AGBACK and
AGCURV may be called. Basically, AGSTUP examines the
current values of the primary control parameters for errors
and computes from them and from its arguments the values of
secondary control parameters. The primary and secondary
control parameters together determine how the routines
AGBACK and AGCURV will behave.
.SH SYNOPSIS
 CALL AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,YDRA,NVIY,IIVY,
.br
+ NEVY,IIEY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agstup (float *xdra, int nvix, int iivx, int nevx, \\
.br
int iiex, float *ydra, int nviy, int iivy, int nevy, \\
.br
int iiey)
.SH DESCRIPTION
The first five arguments of AGSTUP are meaningful only if
at least one of 'X/MINIMUM.' and 'X/MAXIMUM.' has the value
"null 1" or "null 2", specifying that Autograph is to
determine for itself the minimum and/or maximum X
coordinate in the user's data. Similarly, the second five
arguments are meaningful only if at least one of 'Y/
MINIMUM.' and 'Y/MAXIMUM.' has the value "null 1" or "null
2".
.IP XDRA 12
(an input array of type REAL, dimensioned as implied
by the next four arguments) contains user X coordinates.
.IP NVIX 12
(an input expression of type INTEGER) is the number of
"vectors" of data from XDRA to be considered in computing
the minimum and/or maximum X values.
.IP IIVX 12
(an input expression of type INTEGER) is the index
increment between two "vectors" in XDRA. The 1st element of
the first vector is XDRA(1), the 1st element of the second
vector is XDRA(1+IIVX), the 1st element of the third vector
is XDRA(1+IIVX*2), etc.
.IP NEVX 12
(an input expression of type INTEGER) is the number of
elements of each vector in XDRA to be considered in
computing the minimum and/or maximum X values.
.IP IIEX 12
(an input expression of type INTEGER) is the index
increment between two consecutive elements of a vector in
XDRA. The second element of the 1st vector is XDRA
(1+IIEX), the third element of the 1st vector is
XDRA(1+IIEX*2), etc. If IIEX has the value 0, the contents
of XDRA are ignored completely; the minimum and maximum X
values are considered to be "1." and FLOAT(NEVX),
respectively.
.IP "YDRA, NVIY, IIVY, NEVY, and IIEY" 12 
are used similarly, but
define the user Y coordinates.
.sp
Some examples:
.SH ""
.TS
tab (/);
l l l l l l l l.
X array/Data to use/XDRA/NVIX/IIVX/NEVX/HEX
-------/-----------/----/----/----/----/---
.sp
X(100)/all data/X(1)/1/-/100/1
 /(X(I),I=1,99,2)/X(1)/1/-/50/2
 /(X(I),I=51,99,2)/X(51)/1/-/25/2
X(10,10)/all data/X(1,1)/10/10/10/1
 / /X(1,1)/1/-/100/1
 /((X(I,J),I=1,10),J=1,6)/X(1,1)/6/10/10/1
 / /X(1,1)/10/1/6/10
 / /X(1,1)/-/60/1
 /((X(I,J),I=3,7),J=3,9)/X(3,3)/7/10/5/1
 / /X(3,3)/5/1/7/10
 /((X(I,J),I=3,7,4),J=3,9,3)/X(3,3)/3/30/2/4
none/1., 2., . . ., 62./-/-/-/62/0
.TE
.sp
Note: The character "-" is used above to indicate an
argument which is ignored and may be given a dummy value.
.sp
Normally, the X and Y coordinate data tendered to AGSTUP
are the same data which will later be used to draw curves.
However, this need not be the case. For example, one could
call AGSTUP with a two-word XDRA (setting NVIX=1, IIVX=1,
NEVX=2, and IIEX=1) containing a desired minimum and
maximum to be used, disregarding the real data.
.sp
If 'INVERT.' is given the value "1." (in place of its
default value "0."), AGSTUP will behave as if its first
five arguments had been interchanged with its last five, so
that X-coordinate values refer to vertical distances, and 
Y-coordinate values to horizontal distances, on the graph.
This parameter affects AGCURV in a similar manner, thus
allowing one to plot "X as a function of Y".
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex13,
cbex01.
.SH ACCESS 
To use AGSTUP or c_agstup, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
