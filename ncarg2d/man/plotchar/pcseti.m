.\"
.\"	$Id: pcseti.m,v 1.1 1993-03-11 16:29:23 haley Exp $
.\"
.TH PCSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PCSETI -  Sets integer values for a specified parameter.
.SH SYNOPSIS
CALL PCSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
The parameter name of type character (for example, 'CD').
.IP IVAL 12
The integer value you select for the parameter.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use PCSETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_pcseti, load 
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
