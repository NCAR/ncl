.TH SPPS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SPPS - a collection of routines for defining and managing NCAR
Graphics coordinate systems, for drawing lines, markers, and text
in those coordinate systems, and for implementing efficiency
considerations such as polyline segment buffering.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "The Use of X/Y Coordinates in NCAR Graphics" for descriptions
of these coordinate systems.
.sp
The NCAR Graphics user coordinate system allows for axis reversal
(mirror imaging) and non-linear (logarithmic) axes, which are two
important extensions to the GKS world coordinate system.
.sp
Plotter address units (PAUs) were used frequently in various SPPS routine
arguments and in some internal parameters.   PAUs are not being used in
new NCAR Graphics routines.  Wherever they might still be encountered,
simply interpret them according to the formula "1 PAU = 1/1023 NDC units".
.SH SYNOPSIS
OPNGKS - Opens GKS, opens and activates an NCAR GCM
workstation (workstation of type 1) with workstation ID
of 1 and connection ID of 2.
.sp
CLSGKS - Deactivates workstation 1, closes workstation 1, and
closes GKS.
.sp
PLOTIF - A line-drawing routine, described in terms of "pen moves".
Most calls to PLOTIF specify whether the "pen" should be up (not
drawing) or down (drawing) and then move it to a designated position
in the fractional coordinate system.  The polylines resulting from
the pen moves are buffered; some calls to PLOTIF just cause the SPPS
polyline buffer to be flushed.
.sp
FRSTPT - generates a "pen-up" move to a specified point in the user
coordinate system.  FRSTPT is used in conjunction with the routine VECTOR
to draw lines.
.sp
VECTOR - generates a "pen-down" move to a specified point in the user
coordinate system.  VECTOR is used in conjunction with the routine FRSTPT
to draw lines.
.sp
POINT - draws a point at a specified position in the user coordinate system.
.sp
POINTS - draws a marker at each of a series of specified positions in the
user coordinate system.  The markers can also be connected by drawing lines
between them.
.sp
LINE - draws a line from the point (X1,Y1) to the point (X2,Y2).
The "pen" (for subsequent calls to FRSTPT, VECTOR, PLOTIF, and PLOTIT)
is left at (X2,Y2).
.sp
CURVE - draws the curve defined by a specified series of points
in the user coordinate system.  The "pen" (for subsequent calls to
FRSTPT, VECTOR, PLOTIF, and PLOTIT) is left at the location of the
last point in the curve.
.sp
SFLUSH - Flushes polylines, accumulated through calls to the routines
PLOTIF and PLOTIT, from the SPPS polyline buffer shared by those routines;
updates all open workstations; and flushes all system-level I/O buffers.
.sp
FRAME - advances to the next picture in the case of CGM output,
and pauses in the window of most recent creation for X11 output.
A mouse or key click in the window on pause will cause all
active workstations to be cleared.
.sp
SET - defines the mapping between fractional and user coordinates: sets
the values of the SPPS internal parameters 'LS' (axis linear/log scaling)
and 'MI' (axis mirror imaging); defines GKS normalization transformation 1.
.sp
GETSET - returns a set of values which, if used as arguments in a call to
SET, will cause normalization transformation 1, axis linear/log scaling
(internal parameter 'LS'), and axis mirror imaging (internal parameter 'MI')
to be defined in such a way as to duplicate the combined effects of the
current normalization transformation, axis scaling, and axis mirror imaging.
.sp
SETUSV - sets the value of one of the internal parameters of SPPS.
.sp
GETUSV - gets the value of one of the internal parameters of SPPS.
.sp
CFUX, CFUY - Converts from fractional coordinates to user coordinates
.sp
CUFX, CUFY - Converts from user coordinates to fractional coordinates
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_opngks()
.sp
void c_clsgks()
.sp
void c_frstpt (float px, float py)
.sp
void c_vector (float px, float py)
.sp
void c_point (float px, float py)
.sp
void c_points (float *px, float *py, int np, int ic, int il)
.sp
void c_line (float x1, float y1, float x2, float y2)
.sp
void c_curve (float *px, float *py, int np)
.sp
void c_sflush() 
.sp
void c_frame()
.sp
void c_set(float vl, float vr, float vb, float vt, float wl, float wr, float wb, float wt, int lf)
.sp
void c_getset(float *vl, float *vr, float *vb, float *vt, float *wl, float *wr, float *wb, float *wt, int *lf)
.sp
void c_setusv (char *vn, int iv)
.sp
void c_getusv (char *vn, int *iv)
.sp
float c_cfux (float rx)
.sp
float c_cfuy (float ry)
.sp
float c_cufx (float rx)
.sp
float c_cufy (float ry)
.SH SEE ALSO
Online:
clsgks, curve, fl2int, frame, frstpt, getset, getsi, getusv,
line, mxmy, opngks, plotif, plotit, point, points, pwrit, set,
seti, setusv, sflush, vector, wtstr, spps_converters, spps_params,
cfux, cfuy, cufx, cufy
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
