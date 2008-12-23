.TH STUMTA 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STUMTA - 
Given the coordinates of a point on the streamline in data,
user, and NDC space, and the interpolated, normalized
components of the vector at the point relative to data
coordinate space, the user-modifiable routine STUMTA finds
the directional angle of the streamline relative to NDC
space at the point.
.SH SYNOPSIS
CALL STUMTA (XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
.SH DESCRIPTION 
.IP XDA 12
(REAL, input): The X coordinate of a point on the
streamline in the data coordinate system.
.IP YDA 12
(REAL, input); The Y coordinate of a point on the
streamline in the data coordinate system.
.IP XUS 12
(REAL, input): The X coordinate of the point in the
user coordinate system.
.IP YUS 12
(REAL, input): The Y coordinate of the point in the
user coordinate system.
.IP XND 12
(REAL, input): The X coordinate of the point in NDC
space.
.IP YND 12
(REAL, input): The Y coordinate of the point in NDC
space.
.IP DU 12
(REAL, input): The interpolated value of the normalized
component of the vector at the point, with direction
parallel to the X axis of the data coordinate system.
.IP DV 12
(REAL, input): The interpolated value of the normalized
component of the vector at the point, with direction
parallel to the Y axis of the data coordinate system.
.IP TA 12
(REAL, output): The directional angle of the streamline
at the point relative to NDC space.
.IP IST 12
(REAL, output); Status of the mapping operation: 0
indicates success, negative values indicate that the
mapping failed; positive values are reserved and should not
be used by the implementor of a mapping routine.
.SH USAGE
STUMTA is a user-modifiable routine provided to support custom
mappings of the data coordinate space. The user does not invoke it
directly.  Instead, whenever the parameter MAP specifies a mapping not
handled by Streamlines internally (i.e., when MAP is set to a value
other than 0, 1, or 2), Streamlines calls STUMTA once for each
incremental step in the creation of a streamline. The default version
of STUMTA simply returns the angle implied by the incremental vector
components passed to it: that is, it returns ATAN2(DV,DU). In order to
implement a custom mapping, you must pick a unique mapping code (a
positive integer greater than 2), and then modify each of the three
routines, STUMXY, STUIXY, and STUMTA to recognize and respond
consistently to the chosen code. In the standard distribution of NCAR
Graphics, these three routines reside in a single file, \'stumxy.f\'.
STUMXY maps a point from data to user coordinate space and STUIXY
inversely maps a point from user to data coordinate space. STUMTA,
which is likely to be the most difficult to implement, finds the
tangent angle of the streamline at a point in NDC space.
.sp
STUMTA has access to a common block called STMAP that contains a
number of variables used to record the current transformation state.
In order to accommodate a variety of mapping implementations, STMAP
provides more information than normally required. Consider the values
stored in STMAP as strictly read-only. One essential member of this
common block is IMAP, which contains the value currently assigned to
the MAP parameter.
.sp
When implementing a non-linear mapping, an iterative differential
technique will most likely be required. Look at the routine, STMPTA,
in \'stmpxy.f\', which handles the pre-defined mappings, for examples
of the method. Both the default transformation (MAP set to 0), in
order to account for possible log scaling of the user coordinate axes,
and also the Ezmap projection (MAP set to 1) use such a technique.
Basically the idea is that the vector components must be
proportionally reduced in size enough that an effectively
"instantaneous" angle can be calculated, although they must not become
so small that the calculation is adversely affected by the floating
point precision available for the machine. Additionally, checks must
be put in place to prevent the increment from stepping off the edge of
the coordinate system space. The pre-defined mappings step in the
opposite direction to find the angle whenever an increment in the
original direction would fall off the edge.
.SH ACCESS
To use STUMTA, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines,
streamlines_params,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumxy,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
