.\"
.\"     $Id: ngmisc.m,v 1.3 1993-03-23 23:12:49 haley Exp $
.\"
.TH NGMISC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
NGMISC - is a miscellaneous collection of useful functions that are
task-specific and not complex enough to be packages in their own 
right.
.SH SYNOPSIS
.nf
NGDOTS - Draws filled circular dots at specified positions.
NGPICT - Facilitates GKS workstation control functions.
NGWSYM - Draws symbols from the official WMO/NOAA weather symbol charts.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngdots(float *x, float *y, int num, float size, int icolor);
void c_ngpict(int wkid, int action);
void c_ngwsym(char *ftype, int num, float x, float y, float size, int icolor, int ialt);
.SH ACCESS
To use any of the functions in the NGMISC collection, load the NCAR 
Graphics libraries ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
To use any of the NGMISC C-bindings, load the libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the man pages for the individual functions for details on all
appropriate messages.
.SH SEE ALSO
Online:
ngdots(3NCARG),
ngpict(3NCARG),
ngwsym(3NCARG),
ncarg_cbind(3NCARG)
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
