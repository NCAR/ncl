.\"
.\"	$Id: pcgetc.m,v 1.1 1993-03-11 16:29:00 haley Exp $
.\"
.TH PCGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PCGETC -  Retrieves current character values for a specified
parameter.
.SH SYNOPSIS
CALL PCGETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcgetc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
The parameter name of type character (for example, 'FC').
.IP CVAL 12
A character variable in which the current value
of the parameter is to be returned.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions with the following exception:
.SP
.IP len 12
The size of cval as dimensioned in the calling program.
.SH ACCESS
To use PCGETC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_pcgetc, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
pcgetc, pcgeti, pcgetr, pcsetc, pcseti, pcsetr, plchhq, plchlq,
plchmq, plotchar, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
