.TH S*title_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Scrolled_title_params - This document briefly describes all Scrolled_title
internal parameters.
.SH DESCRIPTION 
Parameter descriptions, in alphabetical order, of all Scrolled_title
internal parameters follow. Each description begins with a line
giving the parameter name and it default value. 
.IP "\'ALN\' - 0" 
Flag to control whether alignment
frames with dots in the corners are
produced in non-test mode. \'ALN\'=0
means "suppress them", and \'ALN\'=1
means "produce them".
.IP "\'BGB\' - Device default\."
A real value, in the range 0. to 1.,
to be used for the blue component of
the background color.
.IP "\'BGG\' - Device default\."
A real value, in the range 0\. to 1\.,
to be used for the green component
of the background color.
.IP "\'BGR\' - Device default\."
A real value, in the range 0\. to 1\.,
to be used for the red component of
the background color.
.IP "\'FGB\' - Device default\."
A real value, in the range 0. to 1.,
to be used for the blue component of
the foreground color (in which the
titles are drawn). The title color
can also be set by calling the GKS
subroutine GSCR to define color
index 1; this must be done prior to
calling any Scrolled_title routine and the
color thus specified will be used
for all titles. The title color can
also be set by calling PCSETI to
change the values of internal
parameters of Plotchar that control
character color; this can be done
prior to the STITLE call that writes
a given title.
.IP "\'FGG\' - Device default\."
A real value, in the range 0\. to 1\.,
to be used for the green component
of the foreground color. The
characters are drawn in the
foreground color. See also the
description of \'FGB\', above.
.IP "\'FGR\' - Device default\."
A real value, in the range 0\. to 1\.,
to be used for the red component of
the foreground color. The characters
are drawn in the foreground color.
See also the description of \'FGB\',
above.
.IP "\'FIN\' - 0\."
Number of seconds to fade in titles.
The background color and foreground
color are faded in independently.
Each color is faded in from black to
its current color by varying the
value parameter in an HSV
representation of the color in a
linear manner over the time period
specified.
.IP "\'FOU\' - 0\."
Number of seconds to fade out
titles. The background color and
foreground color are faded out
independently. Each color is faded
out from its current color to black
by varying the value parameter in an
HSV representation of the color in a
linear manner over the time period
specified.
.IP "\'GSZ\' - 40\."
Value for interline spacing, defined
in terms of a 1024x1024 grid. Used
only by FTITLE.
.IP "\'ICO\' - 1"
Centering option. Set to 0 to get
left edges lined up at X-coordinate
64, and to 2 to get right edges
lined up at X-coordinate 960. Set to
1 for centered text. Used only by
FTITLE.
.IP "\'ICU\' - 5"
Unit number for reading input. Used
only by FTITLE.
.IP "\'INC\' - 300"
Vertical spacing between practice
frames. Used only by STITLE.
.IP "\'LOG\' - 4"
FORTRAN logical unit number for
opening WISS (Workstation-Independent
Segment Storage).
.IP "\'LX1\' - 0"
Integer, in the range 0 to 32767,
specifying the X coordinate of the
lower left corner of the viewport.
.IP "\'LX2\' - 32767"
Integer, in the range 0 to 32767,
specifying the X coordinate of the
upper right corner of the viewport.
.IP "\'LY1\' - 0"
Integer, in the range 0 to 32767,
specifying the Y coordinate of the
lower left corner of the viewport.
.IP "\'LY2\' - 32767"
Integer, in the range 0 to 32767,
specifying the Y coordinate of the
upper right corner of the viewport.
.IP "\'MAP\' - 100"
If greater than zero, the value to
which Plotchar\'s internal parameter
\'MA\' is to be set; otherwise, the
user will be assumed to have set it.
The default values of \'MAP\' and
\'ORV\' are such as to create
scrolled titles in the normal way;
other values may be used to create
titles that scroll upwards on the
surface of the globe or that scroll
off into space (a la Star Wars); the
appropriate transformations need to
be embedded in a user-written
version of the Plotchar routine
PCMPXY. (This is still rather new;
eventually, there will be an example
showing how to do this.)
.IP "\'NXE\' - 512"
Analogous to the Scrolled_title argument
NYFIN, to allow for limited
scrolling in the X direction. \'NXE\'
must be within the current Scrolled_title
window.
.IP "\'NXS\' - 512"
Analogous to the Scrolled_title argument
NYST, to allow for limited scrolling
in the X direction. \'NXS\' must be
within the current Scrolled_title window.
.IP "\'ORV\' - 1\.E12"
If non-zero, the value to which
Plotchar\'s internal parameter
\'OR\' is to be set; otherwise, the
user will be assumed to have set it.
See the discussion of the parameter
\'MAP\', above.
.IP "\'PSZ\' - 21\."
Default character height on a 1024
by 1024 grid.
.IP "\'SBK\' - 0"
Suppresses fade in/out of the
background color when a fade in/out
time has been specified. If \'SBK\'=0,
then the fade in/out will be
honored; otherwise, the background
color will appear at full intensity
during a fade in/out.
.IP "\'SFG\' - 0"
Suppresses fade in/out of the
foreground color when a fade in/out
time has been specified. If \'SFG\'=0,
then the fade in/out will be
honored; otherwise, the foreground
color will appear at full intensity
during a fade in/out.
.IP "\'TM1\' - 1\."
Number of seconds worth of blank
frames generated before any title
frames (at 24 frames/second.) Used
only by FTITLE.
.IP "\'TM2\' - \.5"
Number of seconds worth of blank
frames between sets of title frames,
and after the last set of title
frames. Used only by FTITLE.
.IP "\'WID\' - 9"
Workstation identifier for WISS
(Workstation-Independent Segment
Storage); this is used internally in
the calls to Gflash.
.SH SEE ALSO
Online:
slgeti,
slgetr,
slseti,
slsetr,
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
