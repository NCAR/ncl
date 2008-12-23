.TH TDGETI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDGETI - Retrieves the integer value of a Tdpack parameter.
.SH SYNOPSIS
CALL TDGETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdgeti (char *pnam, int *ival)
.SH DESCRIPTION 
The arguments of TDGETI are as follows:
.IP "PNAM" 8
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to retrieve.  The character string
can be of any length, but only the first three characters
of it will be examined.
.IP "IVAL" 8
(an output variable of type INTEGER) -
An integer variable to receive the desired parameter value.  If the internal
parameter is of type INTEGER and has the value "i", IVAL will be given the
value "i"; if the internal parameter is of type REAL and has the value
"r", IVAL will be given the value "INT(r)".
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of Tdpack
parameters. For a complete list of parameters available in this 
utility, see the "tdpack_params" man page.
.SH ACCESS
To use TDGETI or c_tdgeti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the "tdpack" man page for a description of all Tdpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
