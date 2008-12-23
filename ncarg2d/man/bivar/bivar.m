.TH Bivar 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Bivar - Provides bivariate interpolation and smooth surface
fitting for values given at irregularly distributed points.
The resulting interpolating function and its first-order
partial derivatives are continuous.  The method employed is
local, i.e. a change in the data in one area of the plane does
not affect the interpolating function except in that local
area.  Also, the method gives exact results when all points lie
in a plane.
.SH SYNOPSIS
.IP IDBVIP 12 
Produces interpolated values at points (XI(I), YI(I)),
I=1,...,NIP.  This is useful for filling in missing
data points on a grid.
.IP IDSFFT 12
Performs smooth surface fitting when the projections
of the data points in the X-Y plane are irregularly
distributed in the plane.
.IP IDPLTR 12
Plots the triangulation of the data points.
.IP IDGETI 12
Retrieves the integer value of a Bivar parameter.
.IP IDGETR 12
Retrieves the real value of a Bivar parameter.
.IP IDSETI 12
Provides a new integer value for a Bivar parameter.
.IP IDSETR 12
Provides a new real value for a Bivar parameter.
.SH C-BINDING SYNOPSIS
c_idbvip, 
.br
c_idsfft
.br
c_idpltr
.br
c_idgeti
.br
c_idgetr
.br
c_idseti
.br
c_idsetr
.SH EXAMPLES
See the example "cbex01".
.SH ACCESS 

To use the Bivar Fortran or C routines, load the NCAR Graphics
libraries ncarg, ncarg_gks, and ncarg_c, preferably in that order.

.SH MESSAGES
When error conditions are detected, the support routine SETER
is called. By default, SETER writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution.  It is possible to put SETER into recovery mode and
regain control after a recoverable error (which includes
all of the possible errors).
.sp
The possible error messages are listed below.  All errors are recoverable
in the sense that a user program which has called ENTSR to set recovery
mode will get control back after one of these errors occurs.
.sp
IDBVIP (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDBVIP (BIVAR) - INPUT VARIABLE MD IS OUT OF RANGE
.sp
IDBVIP (BIVAR) - INPUT VARIABLE NDP IS OUT OF RANGE
.sp
IDBVIP (BIVAR) - INPUT VARIABLE NIP IS OUT OF RANGE
.sp
IDBVIP (BIVAR) - MD = 2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL
.sp
IDBVIP (BIVAR) - MD = 3 BUT ITY WAS CHANGED SINCE LAST CALL
.sp
IDBVIP (BIVAR) - MD = 3 BUT NIP WAS CHANGED SINCE LAST CALL
.sp
IDGETI (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDGETR (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDGETR (BIVAR) - INVALID KEYWORD: xxx
.sp
IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSULTANT
.sp
IDPLTR (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDSETI (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDSETR (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDSETR (BIVAR) - INVALID KEYWORD: xxx
.sp
IDSFFT (BIVAR) - UNCLEARED PRIOR ERROR
.sp
IDSFFT (BIVAR) - INPUT VARIABLE MD IS OUT OF RANGE
.sp
IDSFFT (BIVAR) - INPUT VARIABLE NDP IS OUT OF RANGE
.sp
IDSFFT (BIVAR) - INPUT VARIABLE NXI OR NYI IS OUT OF RANGE
.sp
IDSFFT (BIVAR) - INPUT VARIABLE NZI IS LESS THAN NXI
.sp
IDSFFT (BIVAR) - MD = 2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL
.sp
IDSFFT (BIVAR) - MD = 3 BUT ITY WAS CHANGED SINCE LAST CALL
.sp
IDSFFT (BIVAR) - MD = 3 BUT NXI WAS CHANGED SINCE LAST CALL
.sp
IDSFFT (BIVAR) - MD = 3 BUT NYI WAS CHANGED SINCE LAST CALL
.sp
IDTANG (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE
.sp
IDTANG (BIVAR) - TWO OF THE INPUT DATA POINTS ARE IDENTICAL
.sp
IDTANG (BIVAR) - ALL COLLINEAR DATA POINTS
.SH SEE ALSO
Online:
bivar_params, idbvip, idsfft, idpltr, idgeti, idgetr, idseti, idsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH ACKNOWLEDGMENTS
Bivar was written by Hiroshi Akima in August 1975 and rewritten
by him in late 1976.  In 1989, a new version of Bivar,
incorporating changes described in a Rocky Mountain Journal of
Mathematics was obtained from Dr. Akima, and included in NCAR
Graphics with his permission.  In 1995, Dave Kennison incorporated
the capability of doing linear interpolation and a different kind
of triangulation, put in a parameter access interface, and wrote a
routine to allow the triangulation to be plotted.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
