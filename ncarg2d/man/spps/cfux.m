.TH CFUX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
CFUX - Converts from fractional coordinates to user coordinates
.TH SPPS_CONVERTERS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CFUX, CFUY, CMFX, CMFY, CMUX, CMUY, CPFX, CPFY, CPUX, CPUY, CUFX, CUFY,
KFMX, KFMY, KFPX, KFPY, KMPX, KMPY, KPMX, KPMY, KUMX, KUMY, KUPX, KUPY
.SH STATUS
Plotter Address Units (PAUs) and Metacode Units (MUs) are no longer used
in NCAR Graphics; thus, all functions with either an m or a p in the
second or third letters of the function name are considered obsolete.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
The following definitions of the PAU Coordinate System and the MU
coordinate system are provided for the purpose of interpreting and
converting PAU and MU codes:
.sp
The plotter coordinates of a point are integers IPX and IPY, where
IPX is between 1 and 2**MX and IPY is between 1 and 2**MY.  MX and
MY are user state variables, both having a default value of 10.
Values of MX and MY can be set by routine SETI and retrieved by
routine GETSI.
.sp
The metacode coordinates of a point are integers IMX and IMY between
0 and 32767 inclusive.  The area addressed is a square in a "metacode space"
that is usually mapped into a square subset of the addressable area of
the plotting device.  Metacode coordinates were used in calls to the
routine PLOTIT and are returned in calls to FL2INT.
.SH SYNOPSIS
.nf
X = CFUX (RX),  Y =CFUY(RY)
X = CUFX (RX),  Y =CUFY(RY)

X = CMUX (IX),  Y =CMUY(IY)
X = CPUX (IX),  Y =CPUY(IY)
X = CMFX (IX),  Y =CMFY(IY)
X = CPFX (IX),  Y =CPFY(IY)

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
(input fractional coordinate of type REAL) is converted to the user X
coordinate for CFUX.
.sp
(input user coordinate of type REAL) is converted to the fractional X
coordinate for CUFX.
.IP RY 12
(input fractional coordinate of type REAL) is converted to the user Y
coordinate for CFUY.
.sp
(input user coordinate of type REAL) is converted to the fractional Y
coordinate for CUFY.
.sp
The second letter of the converter function name designates the
coordinate system of the function argument.  For example CFUX(RX)
has RX as a Fractional X coordinate.  The 3rd letter of the name
designates the coordinate system of the functional result.  In the
above case it would be the X User coordinate of the point.
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
There were 24 converters in the original set.  These converters,
12 for the X coordinate of a point, and 12 for the Y coordinate,
allowed one to switch between any combination of PAUs, MUs, fractional
coordinates, and user coordinates.  PAUs and MUs are now obsolete
coordinates; thus, only four of the converters are still pertinent.
CFUX and CFUY allow one to convert the respective X and Y coordinates
of a point in fractional coordinates to user coordinates.  CUFX and
CUFY go in the opposite direction.
.sp
To use one of CFUX, CFUY, CUFX, or CUFY,
load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_cfux,
c_cfuy, c_cufx, or c_cufy, load the
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
