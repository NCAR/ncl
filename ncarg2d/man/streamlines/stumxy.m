.TH STUMXY 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STUMXY -
Maps a single point on
the streamline from data to user coordinate space.
.SH SYNOPSIS
CALL STUMXY(XDA,YDA,XUS,YUS,IST) 
.SH DESCRIPTION 
.IP XDA 12
(REAL, input): The X coordinate of a point on the
streamline in data coordinate space.
.IP YDA 12
(REAL, input): The Y coordinate of a point on the
streamline in data coordinate space.
.IP XUS 12
(REAL, output): The X coordinate of the point in user
coordinate space.
.IP YUS 12
(REAL, output): The Y coordinate of the point in user
coordinate space.
.IP IST 12
(REAL, output) Status of the mapping operation: 0
indicates success, negative values indicate that the
mapping failed; positive values are reserved and should not
be used by the implementor of a mapping routine.
.SH USAGE
STUMXY is a user-modifiable routine provided to support custom
mappings of the data coordinate space. The user does not invoke it
directly.  Instead, whenever the parameter MAP specifies a mapping not
handled by Streamlines internally (i.e., when MAP is set to a value
other than 0, 1, or 2), Streamlines calls STUMXY as needed during the
process of creating a streamline. The default version of STUMXY simply
performs an identity mapping. In order to implement a custom mapping,
you must pick a unique mapping code (a positive integer greater than
2), and then modify each of the three routines, STUMXY, STUIXY, and
STUMTA to recognize and respond consistently to the chosen code. In
the standard distribution of NCAR Graphics, these three routines
reside in a single file, \'stumxy.f\'.  STUMXY maps a point from data
to user coordinate space, STUIXY inversely maps a point from user to
data coordinate space, and STUMTA, which is likely to be the most
difficult to implement, finds the tangent angle of the streamline at a
point in NDC space.
.sp
STUMXY has access to a common block called STMAP that
contains a number of variables used to record the current
transformation state. In order to accommodate a variety of
mapping implementations, STMAP provides more information
than normally required. Consider the values stored in STMAP
as strictly read-only. One essential member of this common
block is IMAP, which contains the value currently assigned
to the MAP parameter.
.sp
In STUMXY, the implementor of a custom mapping needs to add
code of the form:
.sp
.RS 5
.nf
IF (IMAP .EQ. <chosen_map_code>) THEN
.sp
    ...map XDA to XUS
.sp
    ...map YDA to YUS
.sp
    ...if error, set IST to a negative value
.sp
    RETURN
.sp
ENDIF
.fi
.RE
.sp
Note that after the return from STUMXY, Streamlines checks
to ensure that XUS and YUS are within the current
boundaries of the user coordinate system. 
.sp
When implementing a custom mapping, you may wish to look at the coding
of the pre-defined mappings (0, identity mapping; 1, Ezmap
projections; and 2, polar coordinate mapping) in the file
\'stmpxy.f\'. For these mappings, the subroutine STMPXY is the
equivalent of STUMXY; it has an identical interface and may serve as
a model for your implementation.
.SH ACCESS
To use STUMXY, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
stumta,
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
