.TH PLOTIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PLOTIT - Moves the plotter pen to a designated position in metacode
coordinates, or causes a pen-move buffer flush
.SH STATUS
Metacode units are no longer used in NCAR Graphics;
thus, PLOTIT is considered an obsolete routine.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
User Document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
PLOTIT continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use routine
PLOTIF which uses fractional coordinates.
.sp
The following definition of the Metacode Coordinate System is included
for the purpose of interpreting and converting codes:
.sp
The metacode coordinates of a point are integers IMX and IMY between
0 and 32767 inclusive.  The area addressed is a square in a "metacode space"
that is usually mapped into a square subset of the addressable area of
the plotting device.  Metacode coordinates were used in calls to the
routine PLOTIT and are returned in calls to FL2INT.
.SH SYNOPSIS
CALL PLOTIT (IX,IY,IP)
.SH DESCRIPTION 
.IP IX 12
(an input coordinate of type INTEGER) is the X metacode coordinate of the
point to which the plotter pen is to be moved.
.IP IY 12
(an input coordinate of type INTEGER) is the Y metacode coordinate of the
point to which the plotter pen is to be moved.
.IP IP 12
(an input parameter of type INTEGER) which determines whether the
movement of the plotter pen to point IX,IY will occur with the pen up
(IP = 0), or with the pen down (IP = 1).  This parameter (IP = 2) can
also be used to flush the pen-move buffer.
"CALL PLOTIT (0,0,0)" will also flush the buffer.
.sp
The size of the pen-move buffer can be changed by a call to
the parameter setting routine SETUSV, with parameter PB.  The legal
range of parameter PB is between 2 and 50.  For example, to set
PB to 2, CALL SETUSV("PB",2).
.sp
The buffer is provided to increase drawing efficiency.
.SH ACCESS
To use PLOTIT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpl, fl2int, plotif, setusv, getusv, sflush, frstpt, vector, line, curve,
spps, spps_converters
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
