.\"
.\"	$Id: plotchar.m,v 1.1 1993-03-11 16:29:37 haley Exp $
.\"
.TH PLOTCHAR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PLOTCHAR - Plots text using one of three quality levels.
.SH SYNOPSIS
PLCHHQ - Plots characters of high quality -- using Hershey set.
.br
PLCHMQ - Plots characters of medium quality -- Roman font available.
.br
PLCHLQ - Plots characters of low quality -- uses GKS text routines.
.br
PCGETC - Retrieves current parameter character values.
.br
PCGETI - Retrieves current parameter integer values.
.br
PCGETR - Retrieves current parameter real values.
.br
PCSETC - Sets character parameter values.
.br
PCSETI - Sets integer parameter values.
.br
PCSETR - Sets real parameter values.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_plchhq
.br
c_plchmq
.br
c_plchlq
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
.SH ACCESS 
To use PLOTCHAR, routines load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the PLOTCHAR
C-binding routines load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
pcgetc, pcgeti, pcgetr, pcsetc, pcseti, pcsetr, plchhq, plchlq,
plchmq, plotchar, ncarg_cbind
.sp
Hardcopy: "NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
