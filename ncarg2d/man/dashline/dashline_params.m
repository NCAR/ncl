.TH Dashline_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashline_params - Includes a brief description of all Dashline
internal parameters.
.SH DESCRIPTION 
No parameter setting functions are available for the Dashline utility.
Parameters are set through common blocks SMFLAG, INTPR, and DASHD1.
.sp
Dashline entries can be invoked in four different ways to create
line draws which vary considerably in appearance.  The four variations
include quick, normal, smooth, and super options.  These variations
are specified through selected command line options of the ncargf77 command.
.sp
The following set of common blocks will access any of the internal
parameters available to the Dashline utility.
.nf

 COMMON /SMFLAG/ IOFFS
 COMMON /INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
1              ICLOSE
 COMMON /DASHD1/  ISL,  L,  ISIZE,  IP(100),  NWDSM1,  IPFLAG(100)
1                ,MNCSTR, IGP
.fi
.sp
The following table shows which Dashline internal parameters are available
with the quick, normal, smooth, and super options.
.sp
.nf
Parameter       QUICK   NORMAL  SMOOTH  SUPER

ADDLR             N       N        N      Y
ADDTB             N       N        N      Y
FPART             N       Y        Y      Y
ICLOSE            Y       Y        Y      Y
IGP               N       Y        Y      Y
IOFFS             N       N        Y      Y
IPAU              Y       Y        Y      Y
L1                N       N        Y      Y
MLLINE            N       N        N      Y
NP                N       N        Y      Y
SMALL             N       N        Y      Y
TENSN             N       N        Y      Y
.fi
.sp
PARAMETER DESCRIPTIONS
.IP ADDLR 12
Number of Normalized Device Coordinates (NDCs) * 1024.
added to each character string, on the left and the right, as
free space.
[Default = 2.]
.IP ADDTB 12
Number of Normalized Device Coordinates (NDCs) * 1024.
added to each character string,
on the top and on the bottom, as
free space.
[Default = 2.]
.IP FPART 12
Multiplication factor for
first solid line segment.
This can be used to offset
labels.  For example, if
FPART = \.5, the first solid
line segment is only
one-half as long as the other
solid line segments.  This
moves all labels on this
line towards the beginning,
which reduces the
probability of the label
being written on top of a
label of a nearby line
drawn with FPART = 1.
[Default = 1.]
.IP ICLOSE 12
An internal or external call to
set the pen (pen-up) to a
specific position is executed
only if this position is
more than ICLOSE units away from the
current pen position (distance=
difference in X-coordinates +
difference in Y-coordinates).
The unit of measurement is 32767 integer units
in each of the X and Y dimensions.
[Default = 6]
.IP IGP 12
Flag to control whether a gap
is left for characters when
plotting.
.nf
  9 - A gap is left.
  0 - No gap is left.
.fi
[Default = 9]
.IP IOFFS 12
Flag to turn on smoothing code.
.nf
   0 - Smoothing.
   1 - No smoothing.
.fi
[Default = 0]
.IP IPAU 12
Number of Normalized Device Coordinates (NDCs) * 1024
per line segment in the dash pattern for
solid lines and gaps. The line segment
pattern is repeated every
FLOAT(IPAU)/64. NDCs.
[Default = 3]
.IP L1 12
The maximum number of points
saved at one time.  If there
are more than L1 points on a
given line, L1 points are
processed, then the next L1,
until the entire line is
processed.  Smoothness between
segments is maintained
automatically.  If L1 is
increased, the dimensions of
XSAVE, YSAVE, XP, YP, TEMP,
and SLEN in SUBROUTINE FDVDLD must be
increased to the new value
of L1.
[Default = 70]
.IP MLLINE 12
The maximum length in each
coordinate of a single line to
be processed by the crowded
line algorithm.  Lines longer
than MLLINE
units in a coordinate are broken
into smaller segments each of
which is processed separately.
This is done to prevent
anomalies in the removal of
long lines since only the
starting point and end point of
each line is checked in that
process.
The unit of measurement is 32767 integer units
in each of the X and Y dimensions.
[Default = 384]
.IP NP 12
Twice the maximum number of
interpolated points on a
horizontal line with length
equal to that of the grid.
More points per unit length are
interpolated for short lines
than for long lines.
[Default = 150]
.IP SMALL 12
When the
points on a line are being
processed, only the first of two
consecutive points is saved if
the points are less than SMALL
units apart.
This procedure is to prevent
cusps.
The unit of measurement is 32767 integer units
in each of the X and Y dimensions.
[Default = 128.]
.IP TENSN 12
Tension factor.  Must be
greater than 0.  A large
tension factor (30.) would
essentially turn off smoothing.
[Default = 2.5]
.SH SEE ALSO
Online:
dashline, curved, dashdb, dashdc, frstd,
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
