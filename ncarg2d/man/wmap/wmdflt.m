.\"
.\"	$Id: wmdflt.m,v 1.1 1995-01-06 00:57:49 fred Exp $
.\"
.TH WMDFLT 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMDFLT - returns all values of internal parameters in the Wmap package to their default values.
.SH SYNOPSIS
CALL WMDFLT ()
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmdflt()
.SH USAGE
All values of all internal parameters are returned to their default values.
See the man page for wmap_params for default values.
.SH ACCESS
To use WMDFLT, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  To use c_wmdflt, load the 
NCAR Graphics libraries ncargC, ncarg, ncarg_gksC, ncarg_gks, and ncarg_c,
preferably in that order.
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
(c) Copyright 1995 University Corporation for Atmospheric Research
.br
All Rights Reserved
