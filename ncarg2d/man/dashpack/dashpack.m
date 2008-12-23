.TH Dashpack 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashpack - a set of routines allowing you to draw curves using dashed-line
patterns that may include gap-portion specifiers, solid-portion specifiers,
and label-string specifiers.  DASHPACK is intended to replace and unify the
old family of NCAR Graphics routines (DASHLINE, DASHCHAR, DASHSMTH, and
DASHSUPR), all of which date from the early 1970s or before.  At the time
of its introduction, DASHPACK will do anything that DASHLINE, DASHCHAR,
and DASHSMTH will (and a number of things that they won't); it does not
yet functionally replace all of DASHSUPR, but it is intended that this
will eventually happen.  For the moment, the older routines will stay in
NCAR Graphics.
.sp
Structurally, DASHPACK is rather different from the routines that it replaces:
Its behavior is controlled by "internal parameters" that are accessed by
calling parameter-access routines, rather than by manipulating the contents
of labelled common blocks. There is only one version of DASHPACK; rather than
load a different version of it in order to, say, turn smoothing on, one just
changes the value of the internal parameter that controls whether or not
smoothing is done. There is no analog of the old routines DASHDB and DASHDC
in DASHPACK; to use a different dash pattern, one just changes the values of
the internal parameters that specify what dash pattern to use.
.sp
As in other NCAR Graphics packages, each of the internal parameters of DASHPACK
has a three-character mnemonic name. The internal parameters are described in
detail in the man page "dashpack_params".
.sp
Dashed-Line Patterns
.sp
A dashed-line pattern (or just "dash pattern") may be specified using either
an integer or a character string (see the parameters 'DPL', 'DPS', and 'DPT').
However the pattern is specified, it is applied in a repetitive fashion as a
curve is drawn. For example, if the dash pattern specifies a solid, a gap,
and the label "A", a curve drawn using it will include a solid, a gap, the
label "A", a solid, a gap, the label "A", and so on, until the end of the
curve is reached. When a new curve is begun, use of the dash pattern starts
over: the new curve starts with a solid, a gap, and the label "A".
.sp
If an integer dash pattern is used, the low-order "n" bits of the integer
(where "n" is the absolute value of the user-specified parameter, 'DPS', and
is between 1 and 32, inclusive) are interpreted; 0's represent gaps and 1's
represent solids.
.sp
In a character-string dash pattern, the character used to represent a gap
(by default, an underscore) and the character used to represent a solid
(by default, a dollar sign) are determined by the values of other internal
parameters ('CRG' and 'CRS'); such characters may be mixed with other strings
of characters that are to be used as label strings. There are parameters
allowing one to specify how much distance along the curve should be devoted
to each gap ('WOG') and solid ('WOS'), what size characters should be used
for writing label strings ('WOC'), whether label strings should be written
at a fixed angle relative to the plotter frame or in the direction of the
curve ('SAF'), and, if they are written in the direction of the curve, whether
or not they should be made to "bend" with the curve ('CRB' and 'SCF') and
whether or not it is allowed to rotate them by 180 degrees to make them more
nearly upright on the plotter frame ('SAF').
.sp
A label string written along a curve may either be placed in a gap left in
the curve or just written on top of it ('LS1', 'LS2', and 'LTL'); the latter
is most effective when the characters are written in a color different from
that of the curve itself.
.sp
Output of label strings is buffered so that, if the end of the curve occurs
prior to the occurrence of the next solid or gap in the dash pattern, the
curve itself may be drawn in lieu of an incomplete label, and so that, when
labels are being made to "bend" with the curve, the entire label can be
inverted, if necessary, so as to have the majority of it right-side up. This
buffering is done by default, but may be turned off, if desired ('SBF').
.sp
All character strings are written using calls to PLOTCHAR routines. There is
an internal parameter ('PCF') that says whether to call PLCHHQ, PLCHMQ, or
PLCHLQ. By default, PLCHHQ is used.
.sp
Curve Smoothing
.sp
Curve smoothing is turned on and off by setting a single internal parameter
('TCS') that also specifies the tension on the cubic splines used to do the
smoothing. Another parameter ('SSL') specifies the distance between points
used to plot the smoothed curve and another ('EPS') specifies the minimum
distance between input points required for them to be considered separate
points.
.SH SYNOPSIS
.sp
DPCURV - draws a complete curve with a single call.
.sp
DPDRAW - called to just draw the curve by connecting a given sequence of points.
.sp
DPFRST - does a "pen-up" move to the first in a sequence of points defining a
curve.
.sp
DPGETC - called with the name of an internal parameter of type CHARACTER and
a character variable in which the value of that parameter is to be returned.
.sp
DPGETI - called with the name of an internal parameter of type INTEGER or REAL
and an integer variable in which the integer equivalent of the value of that
parameter is to be returned.
.sp
DPGETR - called with the name of an internal parameter of type INTEGER or REAL
and a real variable in which the real equivalent of the value of that parameter
is to be returned.
.sp
DPLAST - called to say that the latest call to DPVECT was the last one in the
sequence, that drawing of the curve should be completed, and that buffers
should be flushed.
.sp
DPLINE - draws the straight line joining two points. (No smoothing takes place,
even if smoothing is turned on.)
.sp
DPSETC - called with the name of an internal parameter of type CHARACTER and
a character variable or constant that is to supply the new value of that
parameter.
.sp
DPSETI - called with the name of an internal parameter of type INTEGER or REAL
and an integer expression that is to become the new value of that parameter.
.sp
DPSETR - called with the name of an internal parameter of type INTEGER or REAL
and a real expression that is to become the new value of that parameter.
.sp
DPSMTH - called to create a smooth curve passing through a sequence of points
and then to interpolate points along that smoothed curve and pass them on to
DPDRAW.
.sp
DPVECT - does a "pen-down" move to the next in a sequence of points defining
a curve.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_dpcurv
.br
c_dpdraw
.br
c_dpfrst
.br
c_dpgetc
.br
c_dpgeti
.br
c_dpgetr
.br
c_dplast
.br
c_dpline
.br
c_dpsetc
.br
c_dpseti
.br
c_dpsetr
.br
c_dpsmth
.br
c_dpvect
.SH ACCESS
To use the Dashpack C or Fortran routines, load the NCAR Graphics
libraries ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
When error conditions are detected, the support routine SETER
is called. By default, SETER writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution.  It is possible to put SETER into recovery mode and
regain control after a recoverable error (which includes
all of the possible errors).
.sp
The possible error messages are listed below.  All errors are recoverable
in the sense that a user program which has called ENTSR to set recovery
mode will get control back after one of these errors occurs.
.sp
.in +5
DPCURV - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPCURV was called, there was
an unrecovered outstanding error. In this case, DPCURV cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPDRAW - IMPLEMENTATION ERROR - SEE SPECIALIST
.sp
An internal error has occurred which indicates that DASHPACK has not been
correctly installed. See a specialist.
.sp
DPDRAW - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPDRAW was called, there was
an unrecovered outstanding error. In this case, DPDRAW cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPFRST - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPFRST was called, there was
an unrecovered outstanding error. In this case, DPFRST cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPGETC - PARAMETER NAME NOT KNOWN - X
.sp
The first argument in a call to DPGETC is not one of the legal internal
parameter names of DASHPACK. "X" is the value of the offending first argument.
.sp
DPGETC - PARAMETER NAME TOO SHORT - X
.sp
The given parameter name is only one or two characters long. All DASHPACK
parameter names are at least three characters long, so there is something
wrong. "X" is the value of the offending first argument.
.sp
DPGETC - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPGETC was called, there was
an unrecovered outstanding error. In this case, DPGETC cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPGETI - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPGETI was called, there was
an unrecovered outstanding error. In this case, DPGETI cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPGETR - PARAMETER NAME NOT KNOWN - X
.sp
The first argument in a call to DPGETR is not one of the legal internal
parameter names of DASHPACK. "X" is the value of the offending first argument.
.sp
DPGETR - PARAMETER NAME TOO SHORT - X
.sp
The given parameter name is only one or two characters long.  All DASHPACK
parameter names are at least three characters long, so there is something
wrong. "X" is the value of the offending first argument.
.sp
DPGETR - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPGETR was called, there was
an unrecovered outstanding error. In this case, DPGETR cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPLAST - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPLAST was called, there was
an unrecovered outstanding error. In this case, DPLAST cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPLINE - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPLINE was called, there was
an unrecovered outstanding error. In this case, DPLINE cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPSETC - PARAMETER NAME NOT KNOWN - X
.sp
The first argument in a call to DPSETC is not one of the legal internal
parameter names of DASHPACK. "X" is the value of the offending first argument.
.sp
DPSETC - PARAMETER NAME TOO SHORT - X
.sp
The given parameter name is only one or two characters long. All DASHPACK
parameter names are at least three characters long, so there is something
wrong. "X" is the value of the offending first argument.
.sp
DPSETC - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPSETC was called, there was
an unrecovered outstanding error. In this case, DPSETC cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPSETI - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPSETI was called, there was
an unrecovered outstanding error. In this case, DPSETI cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPSETR - PARAMETER NAME NOT KNOWN - X
.sp
The first argument in a call to DPSETR is not one of the legal internal
parameter names of DASHPACK. "X" is the value of the offending first argument.
.sp
DPSETR - PARAMETER NAME TOO SHORT - X
.sp
The given parameter name is only one or two characters long. All DASHPACK
parameter names are at least three characters long, so there is something
wrong. "X" is the value of the offending first argument.
.sp
DPSETR - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPSETR was called, there was
an unrecovered outstanding error. In this case, DPSETR cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPSMTH - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPSMTH was called, there was
an unrecovered outstanding error. In this case, DPSMTH cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.sp
DPVECT - UNCLEARED PRIOR ERROR
.sp
This error message indicates that, at the time DPVECT was called, there was
an unrecovered outstanding error. In this case, DPVECT cannot continue; it
forces the error message for the outstanding error to be printed and then
substitutes this one for it.
.in -5
.sp
.SH SEE ALSO
Online:
dashpack_params,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dpgetr,
dplast,
dpline,
dpsetc,
dpseti,
dpsetr,
dpsmth,
dpvect,
ncarg_cbind.
.sp
Hardcopy:
None.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
