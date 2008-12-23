.TH Dashline 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashline - A set of line-drawing routines that can be used to
generate various dashed line patterns (including solid), can add
labels to the lines, can smooth the lines, and can suppress
crowding of lines.
There are four different versions of Dashline: "normal", "quick", "smooth",
and "super".
See the ACCESS Section for details on how to invoke the various versions.
.SH SYNOPSIS
.IP "DASHDB -" 10
Defines a dash pattern without labels.
.IP "DASHDC -" 10
Defines a dash pattern with labels.  If DASHDC is called when
the "quick" version of Dashline is used, an error exit results.
.IP "FRSTD  -" 10
Defines the first of a sequence of points through which a curve is to be
drawn.
.IP "VECTD  -" 10
Defines the second and following of a sequence of points through which a
curve is to be drawn.
.IP "LASTD  -" 10
Terminates a sequence of calls to draw a curve (a call to FRSTD followed by
one or more calls to VECTD).
.IP "LINED  -" 10
Draws a straight line segment between two points.
.IP "CURVED -" 10
Draws a curve through a sequence of points.
.IP "RESET  -" 10
The "super" version of RESET zeroes the internal integer array used to detect
crowded lines; other versions do nothing.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_dashdb
.br
c_dashdc
.br
c_frstd
.br
c_vectd
.br
c_lastd
.br
c_lined
.br
c_curved
.br
c_reset
.SH USAGE
The Dashline utility allows you to draw various kinds of lines.
In all four versions of it (quick, normal, smooth and super),
you can specify an arbitrary dash pattern for the lines (including
a solid line).  In all but the quick version, you can also add
interspersed labels to the lines for purposes such as naming
lines in an XY plot or assigning contour levels to contour lines.
In the smooth and super versions, the lines can also be smoothed using
splines under tension.
.IP "Warning:" 10
You must be careful when using the smoothing option.
Allowing a high degree of smoothing may have
serious side effects, such as XY curves with
multiple valued loops and contours which cross over
other contours.
.PP
The super version additionally allows for the culling of crowded lines.
In order for the culling process to work properly, the routine RESET must
be called at the beginning of each picture; when using other versions of
Dashline, the calls to RESET may be left in, as they then do nothing.
.SH ACCESS 
To use the normal version of the Dashline utility,
load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  Other optional libraries
to create the quick, smooth, and super versions of Dashline are
automatically linked in by the ncargf77 command.  If you need to know
the names of these libraries for a particular version of Dashline, execute
an "ncargf77" command with the appropriate option and a dummy input file
and examine the resulting command line that is echoed and executed.
.sp
To run a code called mycode.f which has one or more calls to
Dashline entries, issue one of the commands:
.sp
.IP NORMAL 10
Command:  "ncargf77 mycode.f"
.sp
The lines will be drawn as unsmoothed dashed or solid lines.
DASHDC may be called to define a dash pattern with labels.
.sp 2
.IP QUICK 10
Command:  "ncargf77 -quick mycode.f"
.sp
The lines will be drawn as unsmoothed dashed or solid lines.
The dash pattern may not include labels.
If DASHDC is called, an error exit will result; DASHDB must be used instead.
.sp 2
.IP SMOOTH 10
Command:  "ncargf77 -smooth mycode.f"
.sp
The lines will be drawn as dashed or solid lines, smoothed using splines
under tension.
DASHDC may be called to define a dash pattern with labels.
.sp 2
.IP SUPER 10
Command:  "ncargf77 -super mycode.f"
.sp
The lines will be drawn as dashed or solid lines, smoothed using splines
under tension.
DASHDC may be called to define a dash pattern with labels.
Crowded lines will be thinned; in order for this to work properly, you
must remember to call RESET at the beginning of each picture.
.SH MESSAGES
When error conditions are detected, the support routine SETER 
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates 
execution. The possible error messages are as follows:
.IP "CFVLD  -- VECTD CALL OCCURS BEFORE A CALL TO FRSTD"
You must call FRSTD before the first call to VECTD.
.IP "DASHDB -- BLOCKDATA DASHBD APPARENTLY NOT LOADED CORRECTLY"
If you have used the ncargf77 command options as discussed in
the ACCESS Section, see your system administrator.
.IP "DASHDC -- BLOCKDATA DASHBD APPARENTLY NOT LOADED CORRECTLY"
You have called DASHDC when using the "quick" version of Dashline.
This is not allowed.
.IP "FDVDLD -- VECTD CALL OCCURS BEFORE A CALL TO FRSTD"
You must call FRSTD before the first call to VECTD.
.SH SEE ALSO
Online:
dashline_params, curved, dashdb, dashdc, frstd,
lastd, lined, reset, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
