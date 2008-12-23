.TH CMUY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
CMUY - Converts from metacode coordinates to user coordinates.
.TH SPPS_converters 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
spps_converters - A set of functions, each of which transforms a coordinate
from one of the NCAR Graphics coordinate systems to another.  The complete
list of functions is as follows:  CFUX, CFUY, CMFX, CMFY, CMUX, CMUY, CPFX,
CPFY, CPUX, CPUY, CUFX, CUFY, KFMX, KFMY, KFPX, KFPY, KMPX, KMPY, KPMX, KPMY,
KUMX, KUMY, KUPX, and KUPY.
.SH STATUS
Plotter Address Units (PAUs) and Metacode Units (MUs) are no longer used
in NCAR Graphics; thus, all functions with either an M or a P as the
second or third letter of the function name are considered obsolete.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "NCAR Graphics Fundamentals, UNIX Version" for descriptions
of these coordinate systems.
.sp
The following definitions of the PAU coordinate system and the MU
coordinate system are provided for the purpose of interpreting and
converting codes which use PAUs or MUs:
.sp
The plotter coordinates of a point are integers IPX and IPY, where
IPX is between 1 and 2**MX and IPY is between 1 and 2**MY.  MX and
MY are internal parameters of SPPS; each has a default value of 10.
Values of MX and MY can be set by calling the routines SETI or SETUSV
and retrieved by calling the routines GETSI or GETUSV.
.sp
The metacode coordinates of a point are integers IMX and IMY between
0 and 32767 inclusive.  The area addressed is a square in a "metacode space"
that is usually mapped into a square subset of the addressable area of
the plotting device.  Metacode coordinates were used in calls to the
routine PLOTIT and are returned in calls to FL2INT.
.SH SYNOPSIS
.nf

Current:
X = CFUX (RX),  Y = CFUY(RY)
X = CUFX (RX),  Y = CUFY(RY)

Obsolete:
X = CMUX (IX),  Y = CMUY(IY)
X = CPUX (IX),  Y = CPUY(IY)
X = CMFX (IX),  Y = CMFY(IY)
X = CPFX (IX),  Y = CPFY(IY)
I = KFMX (RX),  J = KFMY(RY)
I = KUMX (RX),  J = KUMY(RY)
I = KFPX (RX),  J = KFPY(RY)
I = KUPX (RX),  J = KUPY(RY)
I = KPMX (IX),  J = KPMY(IY)
I = KMPX (IX),  J = KMPY(IY)
.fi
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
float c_cfux (float rx)
.sp
float c_cfuy (float ry)
.sp
float c_cufx (float rx)
.sp
float c_cufy (float ry)
.SH DESCRIPTION 
.IP RX 12
(an input expression of type REAL) is an X coordinate in the coordinate
system specified by the second letter of the function name.  In a reference
to CFUX, RX is a fractional X coordinate; in a reference to CUFX, RX is
a user X coordinate.
.IP RY 12
(an input expression of type REAL) is a Y coordinate in the coordinate
system specified by the second letter of the function name.  In a reference
to CFUY, RY is a fractional Y coordinate; in a reference to CUFY, RY is
a user Y coordinate.
.PP
IX and IY occur as arguments only in references to some of the obsolete
functions; they represent input expressions of type INTEGER, specifying
the X and Y coordinates of a point in either PAUs or MUs (depending on
the second letter of the function name).
.sp
The first letter of the function name is a C if the result is of type REAL
and a K if the result is of type INTEGER (the latter only happens for some
of the obsolete ones).  This conforms to the usual FORTRAN convention for
implicit typing.
.sp
The second letter of the function name specifies the coordinate system of
the argument.
.sp
The third letter of the function name specifies the coordinate system of
the functional result.  In references to CFUX and CFUY, the result is in
the user system; in references to CUFX and CUFY, the result is in the
fractional system.
.sp
The fourth letter of the function name is an X or a Y, depending on whether
an X or a Y coordinate is being converted.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN
argument description.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples
which use coordinate converters:
mpex10, cbex10, sfex02, epltch.
.sp
The command "ncargex -n mpex10" will load the driver mpex10.f into
your current working directory where you can examine the file using
a local editor, or the UNIX more command.
.SH ACCESS
There were 24 conversion functions in the original set.  These functions
(12 for the X coordinate of a point, and 12 for the Y coordinate),
allowed one to convert between any combination of PAUs, MUs, fractional
coordinates, and user coordinates.  PAUs and MUs are now obsolete;
thus, only four of the conversion functions are still pertinent:  CFUX
and CFUY allow one to convert the X and Y coordinates of a point from
the fractional system to the user system, while CUFX and CUFY do the
opposite.
.sp
To use any of the conversion routines, load the
load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
cfux, cfuy, cmfx, cmfy, cmux, cmuy, cpfx, cpfy, cpux, cpuy, cufx, cufy, 
kfmx, kfmy, kfpx, kfpy, kmpx, kmpy, kpmx, kpmy, kumx, kumy, kupx, kupy,
spps, ncarg_cbind
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
