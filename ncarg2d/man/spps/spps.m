.TH SPPS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SPPS - is a collection of routines for defining and managing NCAR
Graphics coordinate systems, for drawing lines, markers, and text
in those coordinate systems, and for implementing efficiency
considerations such as polyline segment buffering.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "The Use of X/Y Coordinates in NCAR Graphics" for a description
of these coordinate systems.
.sp
The NCAR Graphics user coordinate system allows for axis reversal and
non-linear axes which are two important extensions to the GKS
world coordinate system.
.SH SYNOPSIS
OPNGKS - Opens GKS, opens workstation 1 to receive metacode output and
activates workstation 1.
.sp
CLSGKS - Deactivates workstation 1, closes workstation 1, and
closes GKS.
.sp
FRSTPT - generates a pen-up move to the point (PX,PY) in the user
coordinate system.
It is used in conjunction with routine VECTOR to draw lines.
.sp
VECTOR - generates a pen-down move to the point (PX,PY) in the user
coordinate system.
It is used in conjunction with routine FRSTPT to draw lines.
.sp
POINT - draws a point at the location (PX,PY) in the user coordinate system.
.sp
POINTS - draws a series of markers at the locations (PX(I),PY(I),I=1,NP).
The markers can also be connected by drawing lines between them.
.sp
LINE - draws a line from the point (X1,Y1) to the point (X2,Y2).
.sp
CURVE - draws a curve defined by the series of points (PX(I),PY(I), I
= 1,NP), in the user coordinate system.  The pen is left at the location
of the last point in the curve.
.sp
SFLUSH - Flushes the plot buffer accumulated through calls to routines
PLOTIF and PLOTIT.
.sp
FRAME - advances to the next picture in the case of CGM output,
and pauses in the window of most recent creation for X11 output.
A mouse or key click in the window on pause will cause all
active workstations to be cleared.
.sp
SET - sets the internal parameters that define the mapping
back and forth between fractional and user coordinates.
.sp
GETSET - returns the values of the parameters used in the previous call
to routine SET.
.sp
SETUSV - sets the value of one of the spps parameters.
.sp
GETUSV - gets the value of one of the spps parameters.
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
