.TH TDSETI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDSETI - Provides a new integer value for a Tdpack parameter.
.SH SYNOPSIS
CALL TDSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdseti (char *pnam, int ival)
.SH DESCRIPTION 
The arguments of TDSETI are as follows:
.IP "PNAM" 8
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to set. The character string
can be of any length, but only the first three characters
of it will be examined.
.IP "IVAL" 8
(an input expression of type INTEGER) - 
The integer value you select for the parameter.  If the parameter is of type
INTEGER, it will be given the value IVAL; if is is of type REAL, it will be
given the value REAL(IVAL).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Tdpack
parameters. For a complete list of parameters available in this 
utility, see the "tdpack_params" man page.
.SH ACCESS
To use TDSETI or c_tdseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the "tdpack" man page for a description of all Tdpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpi, tdprpt, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
