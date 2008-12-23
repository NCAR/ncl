.TH SLOGAP 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SLOGAP -
Generates TIME seconds worth of blank frames in a manner consistent with FTITLE.
.SH SYNOPSIS
CALL SLOGAP (TIME,MTST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_slogap (float time, int mtst)
.SH DESCRIPTION 
.IP TIME 12
(an input expression of type REAL) specifies the number of seconds worth of
blank frames to be generated.
.IP MTST 12
(an input expression of type INTEGER) is a switch to
indicate whether this is a "real" run or a "practice" run.
.RS
.IP 0
means "real run".
.IP 1 
means "practice run".
.RE
.IP ""
During real runs, the blank frames themselves are created.
.sp
During practice runs, a single frame is created with a legend saying how many
seconds of blank frames it stands for.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to output blank frames in a manner consistent with
FTITLE.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
slex02.
.SH ACCESS
To use SLOGAP or c_slogap, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
ftitle,
scrolled_title,
scrolled_title_params,
slgeti,
slgetr,
slrset,
slseti,
slsetr,
stitle,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
