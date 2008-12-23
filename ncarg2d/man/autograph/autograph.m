.TH Autograph 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Autograph - To draw graphs, each with a labeled background and
each displaying one or more curves.
.SH SYNOPSIS
.sp
Each of the following routines draws a complete graph with a single call:
.sp
EZY -
Draws, in a manner determined by the current values of the
control parameters, a complete graph of a single curve
through the points (I,YDRA(I)), for I from 1 to NPTS. The
argument GLAB may be used to specify a "graph label", to be
placed at the top of the graph.
.sp
EZXY -
Draws, in a manner determined by the current values of the
control parameters, a complete graph of a single curve
through the points (XDRA(I),YDRA(I)), for I from 1 to NPTS.
The argument GLAB may be used to specify a "graph label",
to be placed at the top of the graph.
.sp
EZMY -
Draws, in a manner determined by the current values of the
control parameters, a complete graph of one or more curves,
each defined by a set of points (I,YDRA(I,J)) (or
(I,YDRA(J,I)), depending on the current value of \'ROW.\'),
for I from 1 to NPTS. The curve number J runs from 1 to
MANY. The argument GLAB may be used to specify a "graph
label", to be placed at the top of the graph.
.sp
EZMXY -
Draws, in a manner determined by the current values of the
control parameters, a complete graph of one or more curves,
each defined by a set of points (XDRA(I),YDRA(I,J)) (or
(XDRA(I),YDRA(J,I)) or (XDRA(I,J),YDRA(I,J)) or
(XDRA(J,I),YDRA(J,I)), depending on the current value of
\'ROW.\'), for I from 1 to NPTS. The curve number J runs from
1 to MANY. The argument GLAB may be used to specify a
"graph label", to be placed at the top of the graph.
.sp
The following routines provide access to control parameters:
.sp
ANOTAT -
Changes the values of certain primary control parameters,
purportedly having to do with "annotation" of a graph.
.sp
DISPLA -
Changes the values of certain primary control parameters
purportedly having to do with the "display" of a graph.
.sp
AGSETC -
Allows a user program to (in effect) store a character
string as the value of a specified single parameter.
.sp
AGSETF -
Allows a user program to store a real number as the value
of a single parameter.
.sp
AGSETI -
Allows a user program to store the real equivalent of an
integer as the value of a single parameter.
.sp
AGSETP -
Allows a user program to reset the values of a group of
parameters containing one or more elements.
.sp
AGSETR -
Allows a user program to store a real number as the value
of a single parameter.
.sp
AGGETC -
Allows a user program to retrieve (in effect) the character-string
values of certain single parameters.
.sp
AGGETF -
Allows a user program to retrieve the real value of a
single parameter.
.sp
AGGETI -
Allows a user program to retrieve the integer equivalent of
the real value of a single parameter.
.sp
AGGETP -
Allows a user program to get the values of a group of
parameters containing one or more elements.
.sp
AGGETR -
Allows a user program to retrieve the real value of a
single parameter.
.sp
The following are lower-level user-callable routines:
.sp
AGSTUP -
Performs "set-up" tasks required before AGBACK and
AGCURV may be called. Basically, AGSTUP examines the
current values of the primary control parameters for errors
and computes from them and from its arguments the values of
secondary control parameters. The primary and secondary
control parameters together determine how the routines
AGBACK and AGCURV will behave.
.sp
AGBACK -
Draws the background specified by the current values of the
control parameters - the primary parameters with default
values or with values supplied by the user, and the
secondary parameters with values computed by AGSTUP.
.sp
AGCURV -
Draws a curve in a manner specified by the current values
of the control parameters - the primary parameters with
default values or with values supplied by the user, and the
secondary parameters with values computed by AGSTUP.
.sp
The following are other user-callable routines:
.sp
AGSAVE -
Saves the current state of Autograph for later restoration
by AGRSTR.
.sp
AGRSTR -
Restores a saved state of Autograph.
.sp
The following are non-user-callable, CHARACTER*16 functions:
.sp
AGBNCH -
Provides an easy way to convert binary dash patterns into
character dash patterns.
.sp
AGDSHN -
Provides an easy way to generate the names of parameters in
the group \'DASH/PATTERN.\', for use in calls to AGSETC and
AGGETC.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_ezy
.br
c_ezxy
.br
c_ezmy
.br
c_ezmxy
.br
c_anotat
.br
c_displa
.br
c_agsetc
.br
c_agsetf
.br
c_agseti
.br
c_agsetp
.br
c_agsetr
.br
c_aggetc
.br
c_aggetf
.br
c_aggeti
.br
c_aggetp
.br
c_aggetr
.br
c_agstup
.br
c_agback
.br
c_agcurv
.br
c_agsave
.br
c_agrstr
.br
c_agbnch
.br
c_agdshn
.SH USER-MODIFIABLE INTERNAL ROUTINES
AGCHAX -
Provides a way for the user to change the color, intensity,
line style, etc., of various portions of the axes.
.sp
AGCHCU -
Provides a way for the user to change the color, intensity,
line style, etc., of curves drawn by Autograph.
.sp
AGCHIL -
Provides a way for the user to change the color, intensity,
text style, etc., of the informational labels.
.sp
AGCHNL -
Provides a way for the user to substitute arbitrary
character strings for the numeric labels generated by
Autograph.
.sp
AGPWRT -
Provides a way for the user to change the style of all text
strings drawn by Autograph.
.sp
AGUTOL -
Provides a way for the user to change the user-system-to-label-system
mapping for one or more of the four axes.
.SH ACCESS 
To use the Autograph Fortran or C routines, load the NCAR Graphics libraries
ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.sp
To get smoother curves, drawn using spline interpolation,
also load libdashsmth.o     
.sp
Autograph contains a routine AGPWRT, which it calls to draw
labels.  This routine just passes its arguments on to the
system-plot-package routine PWRIT. To use one of the fancier
character-drawers, like Plotchar, just compile a routine AGPWRT to
replace the default version; it has the same arguments as PWRIT
and may either draw the character string itself, or just pass the
arguments on to a desired character-drawer.  You can also modify
AGPWRT so that Autograph will access character fonts that are
different from the default font.  In  its distributed form, AGPWRT
calls PWRIT. Consult the documentation in the code.  The file
AGUPWRT on the distribution tape contains an implementation of
AGPWRTX that allows Autograph to access the PWRITX character set.
See the documentation in the code for AGUPWRTX.   See your NCAR
Graphics site representative to learn how to access the code.
.SH MESSAGES
Autograph routines detect certain errors and, in response,
call the routine SETER, which is an adapted version of a
PORT error handler. Currently, all such errors are treated
as being fatal and cause termination of the job. An error
message is logged before the job is terminated. Each such
message includes the name of the routine which detected the
error and may be accompanied by supplementary information
aimed at allowing the user to easily identify the call that
caused the error. The possible error messages are as
follows (in alphabetical order):
.sp
AGEXAX (CALLED BY AGSTUP) - USER-SYSTEM-TO-LABEL-SYSTEM
MAPPING IS NOT MONOTONIC
.sp
.in +5
This probably means that you have replaced the default
routine AGUTOL with a version of your own, and you\'ve blown
it.
.in -5
.sp
AGGETC - PARAMETER TO GET IS NOT INTRINSICALLY OF TYPE
CHARACTER
.sp
.in +5
The argument TPGN specifies a parameter which is not
intrinsically of type character. See the description in the 
AGGETC man page.
.in -5
.sp
AGGETP OR AGSETP - ATTEMPT TO ACCESS LABEL ATTRIBUTES
BEFORE SETTING LABEL NAME
.sp
.in +5
The parameter \'LABEL/NAME.\' must be set prior to the call
which gave the error message, specifying which label\'s
attributes are being referenced.
.sp
.in -5
AGGETP OR AGSETP - ATTEMPT TO ACCESS LINE ATTRIBUTES BEFORE
SETTING LINE NUMBER
.sp
.in +5
The parameter \'LINE/NUMBER.\' must be set prior to the call
which gave the error message, specifying which label line\'s
attributes are being referenced.
.in -5
.sp
AGGETP OR AGSETP - ILLEGAL KEYWORD USED IN PARAMETER
IDENTIFIER
.sp
.in +5
The argument TPGN contains an unrecognizable keyword.
.in -5
.sp
AGKURV - NUMBER OF POINTS IS \&.LE. 0
.sp
.in +5
The argument NEXY, in a call to AGCURV, is less than or
equal to zero. The routine AGKURV is called by AGCURV to
draw un-windowed curves.
.in -5
.sp
AGNUMB - MANTISSA TOO LONG
.br
AGNUMB - EXPONENT TOO LARGE
.br
AGNUMB - ZERO-LENGTH MANTISSA
.sp
.in +5
AGNUMB is called by AGAXIS to generate a character string
expressing the value of a real number. You should not be
able to generate any of AGNUMB\'s error messages. If you do,
see the Autograph specialist.
.sp
.in -5
AGQURV - NUMBER OF POINTS IS \&.LE. 0
.sp
.in +5
The argument NEXY, in a call to AGCURV, is less than or
equal to zero. The routine AGQURV is called by AGCURV to
draw windowed curves.
.in -5
.sp
AGRSTR - ERROR ON READ
.br
AGRSTR - END-OF-FILE ON READ
.sp
.in +5
Probably the unit specified by IFNO was not positioned
properly.
.sp
.in -5
AGSAVE - ERROR ON WRITE
.in +5
.sp
A system error has occurred as a result of the attempted
"WRITE".
.sp
.in -5
AGSETC - PARAMETER TO SET IS NOT INTRINSICALLY OF TYPE
CHARACTER
.sp
.in +5
This means that the argument TPGN specifies some parameter
other than one of the acceptable possibilities. See the
description in the AGSETC man page.
.sp
.in -5
AGSETP - ATTEMPT TO DEFINE LINE OF NON-EXISTENT LABEL
.sp
.in +5
The user has attempted to define a line of a label without
first specifying which label; \'LABEL/NAME.\' must be set
prior to the call which gave the error message.
.in -5
.sp
AGSETP - LABEL LIST OVERFLOW - SEE AUTOGRAPH SPECIALIST
.sp
.in +5
The user has attempted to define more labels than Autograph
can handle; a modification of Autograph is required. 
.sp
.in -5
AGSETP - LINE LIST OVERFLOW - SEE AUTOGRAPH SPECIALIST
.sp
.in +5
The user has attempted to define more label lines than
Autograph can handle; a modification of Autograph is
required. 
.sp
.in -5
AGSTCH - CHARACTER-STRING BUFFER OVERFLOW - SEE CONSULTANT
.sp
.in +5
The routine AGSTCH is called by AGSETC to stash the
character string in Autograph\'s character storage space.
The available storage space has been exhausted. 
See the
consultant.
.sp
.in -5
AGSTCH - CHARACTER-STRING INDEX OVERFLOW - SEE CONSULTANT
.sp
.in +5
The routine AGSTCH is called by AGSETC to stash the
character string in Autograph\'s character storage space.
Too many such strings have been stored. See the consultant.
.sp
.in -5
AGSTUP - GRAPH WINDOW IMPROPERLY SPECIFIED
.sp
.in +5
The parameters in the group named \'GRAPH.\' have improper
values.
.sp
.in -5
AGSTUP - GRID WINDOW IMPROPERLY SPECIFIED
.sp
.in +5
The parameters in the group named \'GRID.\' have improper
values. This is most likely to occur when \'SET.\' has the
value "2." or "4.", specifying that the edges of the grid
window are to be as implied by the last call to the plot
package routine SET. Check to make sure that the portion of
the plotter frame specified by the last SET call is within
the current graph window.
.sp
.in -5
AGSTUP - s LABELS IMPROPERLY SPECIFIED
.sp
.in +5
(where "s" = "LEFT", "RIGHT", "BOTTOM", "TOP", or
"INTERIOR"). Re-read the paragraph "THE LABEL BOXES", in
the section "OVERVIEW". You have defined a label with a
basepoint on one edge of the grid window and an offset
vector pointing outward, some part of which extends inside
the grid window (or vice-versa). This is not allowed.
.in -5
.SH SEE ALSO
Online:
autograph,
autograph_params,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agpwrt,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
