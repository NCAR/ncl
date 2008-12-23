.TH TDINIT 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDINIT - Initialization routine for TDPACK, called to define the position of
the eye, the position of the point looked at, which way is up, and whether or
not a stereo view is to be done.
.SH SYNOPSIS
CALL TDINIT (UMID, VMID, WMID, UORI, VORI, WORI, UTHI, VTHI, WTHI, OTEP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdinit(float umid, float vmid, float wmid, float uori, float vori,
float wori, float uthi, float vthi, float wthi, float otep)
.SH DESCRIPTION
This initialization routine is called to define the position of the eye, the
position of the point looked at, which way is up, and whether or not a
stereo view is to be done. TDINIT precomputes some quantities in TDPACK
common blocks that will subsequently be used in projecting points from 3-space
(U, V, and W coordinates) to 2-space (X and Y coordinates).
.sp
By default (that is to say, if the internal parameter 'SET' has its default
value), TDINIT also calls the SPPS routine SET to define the mapping
from the "user" coordinate system (the X/Y system) to the "fractional"
coordinate system (in GKS terms, NDC space). This is done in such a
way as to show in the projection plane a field of view of 'FOV' degrees.
The viewport to be used in the fractional coordinate system is that
defined by the values of the internal parameters 'VPL', 'VPR', 'VPB',
and 'VPT'. In some situations, it may be desirable, after calling
TDINIT, to call the SPPS routine GETSET to retrieve the arguments
with which TDINIT called SET and then recall SET with a different
set of arguments.
.sp
Normally, the creation of an image with TDPACK starts with a call to
TDINIT (perhaps preceded by calls to TDSETI, TDSETR, and/or
TDSTRS to reset internal parameters of the package) and continues
with calls to draw objects. (Of course, if all of the arguments in a call to
TDINIT have the same values as in the last call and the SET call that
was done as a result is still in effect, then it's not necessary to repeat the
call to TDINIT.)
.sp
For stereo views, one calls TDINIT with a negative OTEP, executes the
object-drawing calls, calls TDINIT again with a positive OTEP, and
then repeats all of the object-drawing calls. The exact way in which
stereo views are drawn is also affected (slightly) by the value of the
internal parameter 'STE'.
.sp
The arguments of TDINIT are as follows:
.IP "UMID, VMID, and WMID" 8
(input expressions of type REAL) -
the coordinates of a point, E, at the eye position (if a single view is being
drawn), or of a point midway between the two eyes (if a stereo view is being
drawn).
.IP "UORI, VORI, and WORI" 8
(input expressions of type REAL) -
the coordinates of a point, O, that the eye is looking at.  That point defines
the origin of the XY projection plane.  The line of sight is the line from E
to O.  The projection plane passes through O and is perpendicular to the line
of sight.
.IP "UTHI, VTHI, and WTHI" 8
(input expressions of type REAL) -
the coordinates of a third point, T, needed to completely specify the
orientation of the X and Y axes in the projection plane.  The Y axis
of the projection plane is its intersection with the plane passing though
the points E, O, and T.  The X axis of the projection plane passes through
the point O and is perpendicular to the Y axis.
.IP "OTEP" 8
(an input expression of type REAL) -
set non-zero if and only if a stereo view is to be drawn.  Make the value
negative to draw a left-eye view, positive to draw a right-eye view.  The
magnitude of OTEP is the distance from either eye to the point midway between
the eyes; if R represents the approximate distance from the eye to the objects
being drawn, then OTEP may be set using a statement like
.sp
.nf
  OTEP = (+ or -) R *
         TAN(.017453292519943*ANGD/2.)
.fi
.sp
where the constant (.01745...) is just pi/180 and ANGD is the desired
difference in the angle between the two views, in degrees; use a value of
about 1 or 2 degrees for ANGD.
.sp
Note that, if the origin is approximately in the middle of the objects
being viewed, then it's probably appropriate to use a value of R
computed as follows:
.sp
.nf
  R = SQRT((UMID-UORI)**2
           (VMID-VORI)**2+
           (WMID-WORI)**2)
.fi
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDINIT or c_tdinit, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tditri, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
