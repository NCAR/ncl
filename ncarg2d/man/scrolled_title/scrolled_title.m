.TH Scrolled_title 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Scrolled_title - 
Creates movie titles. The
titles can be scrolled, faded in, and/or faded out.
Foreground and background colors can be specified.
.SH SYNOPSIS
The package Scrolled_title has six user entry points, as follows:
FTITLE, STITLE, SLGETI, SLGETR, SLSETI, and SLSETR. 
.IP FTITLE 12
stands for "fixed titles" and is used when the
following conditions are met:
.RS
.IP \(bu
Each group of text lines is to fit on one frame (no
scrolling).
.IP \(bu
Vertical spacing of text lines is to be done automatically.
.IP \(bu
Text lines are to be centered horizontally.
.IP \(bu
There are to be no more than 80 characters per text line
(including Plotchar "function codes").
.IP \(bu
In production mode, blank frames are to be generated before
and after each group of title frames.
.IP \(bu
No more than 120 text lines are to be displayed on a single
frame.
.RE
.IP STITLE 12
stands for "scrolled titles" and is used when the
above conditions are not met.
.IP "SLSETI and SLSETR" 12 
are used to set the values of "internal
parameters" whose values affect the behavior of FTITLE and/or
STITLE.
.IP "SLGETI and SLGETR" 12 
are used to retrieve the current values
of internal parameters.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_ftitle
.br
c_stitle
.br
c_slseti
.br
c_slsetr
.br
c_slgeti
.br
c_slgetr
.SH ACCESS 
To use Scrolled_title routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.  To use the Scrolled_title 
C-bindings, load the NCAR Graphics libraries ncargC, ncarg_gksC,
ncarg_gks, ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
The error messages described can be written to the output
unit by routines in the package Scrolled_title. Unless otherwise
indicated, these errors are fatal and cause the user\'s
program to terminate execution.
.sp
.in +5
FTITLE -- NUMBER OF INPUT CARDS EXCEEDS 120
.in +5
.sp
The meaning should be obvious. Correct the input.
.sp
.in -5
SLBKGD - Error flag returned by GQFAIS - n
.br
SLBKGD - Error flag returned by GQFACI - n
.sp
.in +5
These probably mean that GKS is not in the correct state
(e.g., it hasn\'t been opened).
.in -5
.sp
SLGETI OR SLGETR -- INVALID KEYWORD = xxx, ZERO VALUE
RETURNED
.sp
.in +5
SLGETI or SLGETR has been called with an unrecognized
parameter name. A zero value is returned. Execution
continues.
.sp
.in -5
SLINIT -- No active workstations.
.sp
.in +5
SLINIT is an internal routine that is called by STITLE to
initialize things. It has found that there are no active
workstations. This probably means that GKS has not been
properly initialized.
.sp
.in -5
SLSETI OR SLSETR -- INVALID KEYWORD = xxx, NO ACTION TAKEN
.sp
.in +5
SLSETI or SLSETR has been called with an unrecognized
parameter name. No action is taken. Execution continues.
.in -10
.SH SEE ALSO
Online:
ftitle,
slgeti,
slgetr,
slseti,
slsetr,
stitle,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
