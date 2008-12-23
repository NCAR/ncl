.TH Scrolled_title 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Scrolled_title - 
Creates movie titles. The
titles can be scrolled, faded in, and/or faded out.
Foreground and background colors can be specified.
.SH SYNOPSIS
The package Scrolled_title has eight user entry points, as follows:
FTITLE, STITLE, SLGETI, SLGETR, SLOGAP, SLSETI, SLSETR, and SLRSET.
.IP FTITLE 12
stands for "fixed titles" and is used when the
following conditions are met:
.RS
.IP \(bu
Each group of text lines is to fit on one frame (no
scrolling).
.IP \(bu
All test lines in a group are to be written in the same color.
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
.IP SLOGAP 12
is used to generate blank-frame gaps in a manner consistent with FTITLE.
.IP "SLSETI and SLSETR" 12 
are used to set the values of "internal
parameters" whose values affect the behavior of FTITLE and/or
STITLE.
.IP "SLGETI and SLGETR" 12 
are used to retrieve the current values
of internal parameters.
.IP SLRSET 12
is ued to reset the values of all internal parameters of STITLE to the
original defaults.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_ftitle
.br
c_stitle
.br
c_slogap
.br
c_slseti
.br
c_slsetr
.br
c_slgeti
.br
c_slgetr
.br
c_slrset
.SH ACCESS 
To use Scrolled_title routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, ncarg_c, and ncarg_c, preferably in that order.
.SH MESSAGES
The error messages described can be written to the output
unit by routines in the package Scrolled_title. Unless otherwise
indicated, these errors are fatal and cause the user\'s
program to terminate execution.
.sp
.in +5
FTITLE - PREMATURE EOF ON CARD INPUT UNIT
.sp
.in +5
FTITLE has encountered an end-of-file on the card input in a place where one
should not have been.
.sp
.in -5
FTITLE - READ ERROR ON CARD INPUT UNIT
.sp
.in +5
FTITLE has executed a FORTRAN READ statement and the "ERR=..." exit has been
taken.
.sp
.in -5
FTITLE - TOO MANY INPUT CARDS IN GROUP
.sp
.in +5
The first card of a title group contains a value of NCDS greater than 120.
FTITLE can't handle that many lines.
.sp
.in -5
FTITLE - UNCLEARED PRIOR ERROR
.sp
.in +5
When FTITLE was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLBKGD - ERROR RETURN FROM GQFACI: i
.br
SLBKGD - ERROR RETURN FROM GQFAIS: I
.sp
.in +5
A GKS routine has returned a non-zero error flag with value "i".  This most
likely means that GKS is in the wrong state.
.sp
.in -5
SLGCLR - TOO MANY COLORS DEFINED
.sp
.in +5
More than 256 colors have been made known to STITLE.  It has no room in which
to maintain information about them all.
.sp
.in -5
SLGETI - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLGETI was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLGETR -- INVALID KEYWORD: xxx
.sp
.in +5
The first argument in a call to SLGETR is not the name of one of the known
internal parameters of STITLE.  This error message can occur as the result
of a call to SLGETI.
.sp
.in -5
SLGETR - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLGETR was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLGWID - ERROR RETURN FROM GQACWK: i
.sp
.in +5
A GKS routine has returned a non-zero error flag with value "i".  This most
likely means that GKS is in the wrong state.
.sp
.in -5
SLGWID - NO ACTIVE WORKSTATIONS
.sp
.in +5
STITLE uses the routine SLGWID to determine the ID of the first active
workstation (to use in calls dealing with color) and it has found that
there are currently no active workstations at all.
.sp
.in -5
SLOGAP - ERROR RETURN FROM GQTXCI: i
.sp
.in +5
A GKS routine has returned a non-zero error flag with value "i".  This most
likely means that GKS is in the wrong state.
.sp
.in -5
SLOGAP - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLOGAP was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLRSET - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLOGAP was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLSCLR - TOO MANY COLORS DEFINED
.sp
.in +5
More than 256 colors have been made known to STITLE.  It has no room in which
to maintain information about them all.
.sp
.in -5
SLSETI - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLSETI was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
SLSETR -- INVALID KEYWORD: xxx
.sp
.in +5
The first argument in a call to SLSETR is not the name of one of the known
internal parameters of STITLE.  This error message can occur as the result
of a call to SLSETI.
.sp
.in -5
SLSETR - UNCLEARED PRIOR ERROR
.sp
.in +5
When SLSETR was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.sp
.in -5
STITLE - ERROR RETURN FROM GQCLIP: i
.br
STITLE - ERROR RETURN FROM GQCNTN: i
.br
STITLE - ERROR RETURN FROM GQPLCI: i
.br
STITLE - ERROR RETURN FROM GQPMCI: i
.br
STITLE - ERROR RETURN FROM GQTXCI: i
.sp
.in +5
A GKS routine has returned a non-zero error flag with value "i".  This most
likely means that GKS is in the wrong state.
.sp
.in -5
STITLE - UNCLEARED PRIOR ERROR
.sp
.in +5
When STITLE was called, there was an unrecovered outstanding error.  The
routine does not continue; it forces the error message to be printed and
then substitutes this one for it.
.in -10
.SH SEE ALSO
Online:
ftitle,
scrolled_title_params.m
slgeti,
slgetr,
slogap,
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
