.TH Dashline 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashline - A set of line drawing routines which can be used to
generate various dashed line patterns (including solid), can add
labels to the lines, can smooth the lines, and can suppress
crowding of lines.
.SH SYNOPSIS
CALL DASHDC - choose a dash pattern with labels 
.br
CALL DASHDB - chooses a dash pattern without labels
.br
CALL CURVED - draws a curve through points.
.br
CALL FRSTD - put pen down
.br
CALL VECTD - draw a line segment
.br
CALL LINED - draw a straight line between two points.
.br
CALL LASTD - used to flush buffers and finish drawing a line
after calling FRSTD, VECTD, or LINED.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_dashdc
.br
c_dashdb
.br
c_curved
.br
c_frstd
.br
c_vectd
.br
c_lined
.br
c_lastd
.SH USAGE
The Dashline utility allows one to create various kinds of lines.
In all of the quick, normal, smooth and super incantantations of
this utility you can create an arbitrary dash pattern for a line
including a solid line.  In all but the quick option, you can also
add interspersed labels to the lines for purposes such as naming
lines in an XY plot, or assigning contour levels to contour lines.
In the smooth and super options the lines can also be smoothed using
splines under tension.
.sp
.nf
Warning:  You must be careful when using the smoothing option.
	  If you allow a high degree of smoothing it can create
	  some serious side effects such as XY curves with
	  multiple valued loops and contours which cross over
	  other contours.  Crossed contours can produce color
	  fill leakage anomalies.
.fi
.sp
The super option additionally allows for the culling of crowded lines.
See the ACCESS Section for details on how to invoke the various options.
.SH ACCESS 
The Dashline entries can be invoked in four different ways to create
line draws which vary considerably in appearance.  The four variations
include quick, normal, smooth, and super lines.  These variations are
specified through selected command line options of the ncargf77 command.
.sp
To use the normal version of line draws in the Dashline utility,
load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  Other optional libraries
to create the quick, smooth, and super line draws will
automatically be linked by the ncargf77 command.
To run a code called mycode.f which has one or more calls to
Dashline entries, issue one of the commands:
.sp
.IP NORMAL 10
Command:  "ncargf77 mycode.f"
.sp
The lines will be drawn as unsmoothed
dashed or solid lines that can include characters along the lines.
.sp 2
.IP QUICK 10
Command:  "ncargf77 -quick mycode.f"
.sp
The lines will be drawn as unsmoothed
dashed or solid lines without characters along the lines.
.sp 2
.IP SMOOTH 10
Command:  "ncargf77 -smooth mycode.f"
.sp
The lines will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.
.sp 2
.IP SUPER 10
Command:  "ncargf77 -super mycode.f"
.sp
The lines will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.  Crowded lines can be thinned.
.SH MESSAGES
When error conditions are detected, the support routine SETER 
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates 
execution. The possible error messages are as follows:
.IP "CFVLD  -- VECTD CALL OCCURS BEFORE A CALL TO FRSTD"
A FRSTD call must proceed the first VECTD call.
.IP "DASHDB -- BLOCKDATA DASHBD APPARRENTLY NOT LOADED CORRECTLY"
If you have used the ncargf77 command options as discussed in
the ACCESS Section, see your system administrator.
.IP "DASHDC -- BLOCKDATA DASHBD APPARRENTLY NOT LOADED CORRECTLY"
If you have used the ncargf77 command options as discussed in
the ACCESS Section, see your system administrator.
.IP "FDVDLD -- VECTD CALL OCCURS BEFORE A CALL TO FRSTD"
A FRSTD call must proceed the first VECTD call.
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
