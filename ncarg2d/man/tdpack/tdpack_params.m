.TH Tdpack_params 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Tdpack_params - This document briefly describes all internal parameters of
Tdpack.
.SH DESCRIPTION
Tdpack currently supports some fifteen parameters.  The current values may
be retrieved using one of the routines TDGETI, TDGETR, or TDGTRS.  Parameter
values may be reset using one of the routines TDSETI, TDSETR, or TDSTRS.
.sp
The Tdpack parameter descriptions appear below in alphabetical
order. Each description begins with a line giving the parameter name 
and the intrinsic FORTRAN type of the parameter.
.IP "'CS1' - Real"
Character Size 1.
This is a multiplier for the sizes of all characters drawn by the routine
TDLBLS.
.sp
The default value of 'CS1' is 1.
.IP "'CS2' - Real"
Character Size 2.
This is a multiplier for the sizes of all characters drawn by the routine
TDLBLA.  Exactly how it is set depends on how the current reference
parallelogram has been defined.  If the current reference parallelogram
has sides of length 1, then it is appropriate to set
.sp
  \&'CS2'='CS1'*MIN(UMAX-UMIN,VMAX-VMIN,WMAX-WMIN)
.sp
where UMIN, UMAX, VMIN, VMAX, WMIN, and WMAX are defined like the arguments
of TDLBLS. Note that, if you call TDLBLS, it resets 'CS2' in this fashion,
and it does not restore the original value before return.
.sp
The default value of 'CS2' is 1.
.IP "'FOV' - Real"
When 'SET' has a non-zero value, specifying that TDINIT is to call
the SPPS routine SET, then 'FOV' specifies the desired field of view,
in degrees, to be seen in the projection plane; its value is used to
compute appropriate values for the arguments in the call to SET.
.sp
The default value of 'FOV' is 20.
.IP "'HND' - Integer"
A zero value specifies that the UVW coordinate system is to be right-handed
(the default), while a non-zero value says that it is to be left-handed.
Changing the value of 'HND' mostly affects the behavior of TDINIT - it changes
the values of the projection coefficients precomputed and stored in TDPACK
common blocks - but it also affects the behavior of the routines TDDTRI and
TDLBLS; for this reason, you should not change the value of 'HND' between a
call to TDINIT and an ensuing call to one of the other two routines, as this
would lead to inconsistent behavior.  (Most likely, you will decide what the
handedness of your coordinate system is, change the value of 'HND' accordingly,
and leave that value unchanged thereafter.)
.sp
The default value of 'HND' is 0, specifying a right-handed coordinate system.
.IP "'LSU', 'LSV', and 'LSW' - Real"
If one or more of these three values is non-zero, then the 3-space point
('LSU','LSV','LSW') is the position of the light source assumed by
TDDTRI in rendering the triangles in the triangle list.
.sp
The default values are all 0., which, by convention, specifies a
light source at the position (UMID,VMID,WMID), where UMID, VMID,
and WMID are as specified in the last call to TDINIT.
.IP "'SET' - Integer"
If this flag is equal to 1 (the default), calling TDINIT will result in a
call to the SPPS routine SET to define the mapping from the "user coordinate
system" (the X/Y projection plane) to the "fractional coordinate system" (the
same as the GKS NDC space); that call will be done in such a way as to give
a field of view of 'FOV' degrees and to use the portion of the plotter frame
defined by the internal parameters 'VPL', 'VPR', 'VPB', and 'VPT'.  If 'SET'
is given the value 0, no such call to SET will be done and the user will be
expected to have called it.  In the future, other non-zero values of 'SET'
may be implemented which will result in TDINIT's doing a call to SET according
to some other criterion.
.sp
The default value of 'SET' is 1, specifying that a SET call should be
done by TDINIT.
.IP "'SHD' - Integer"
The value of 'SHD' affects the behavior of TDDTRI when it is picking
the fill color to be used for a triangle and the rendering style for the
triangle implies that the color index is to be chosen by computing the
angle between two vectors - the normal to the triangle and the vector
from its center to the light source - and then mapping that angle into a
specified range of color indices (thus shading the surface).
.sp
If the value of 'SHD' is zero, angles between 0 and 90 are mapped
linearly into the specified range of color indices, from lowest to highest,
while angles between 90 and 180 are mapped linearly into that same
range, from highest to lowest. The effect of this is essentially to use two
light sources, one at the position ('LSU','LSV','LSW') and the other at
an exactly opposite position relative to the center of the triangle.
.sp
If the value of 'SHD' is non-zero, angles between 0 and 180 are
mapped linearly into the specified range of color indices, from lowest to
highest. The effect of this is essentially to use a single light source, at
the position ('LSU','LSV','LSW').
.sp
Note that, in both cases, it is the user's responsibility to define the colors
associated with the color indices in the specified range to achieve the
desired effect. (Normally, the colors associated with the beginning of
the range of color indices should be the lightest and the colors
associated with the end of that range should be the darkest.) For help
in defining colors, see the code of the examples.
.sp
The default value of 'SHD' is zero.
.IP "'STE' - Integer"
When 'STE' is zero (the default), then stereo pairs are drawn using a
single image plane which is perpendicular to the line joining a point
midway between the eyes to the point looked at. If 'STE' is set
non-zero, then the view from each eye is drawn using an image plane
which is perpendicular to the line joining that eye to the point looked at.
(In practice, this probably makes little difference.)
.sp
Changing this parameter changes the values of the projection
coefficients precomputed by TDINIT and stored in TDPACK common
blocks.
.sp
The default value of 'STE' is 0, specifying that stereo pairs should be
drawn using a single image plane perpendicular to the line from the
midpoint of the eyes to the point looked at.
.IP "'VPB', 'VPL', 'VPR', and 'VPT' - Real"
The values of these parameters specify the positions of the bottom, left,
right, and top edges, respectively, of a desired viewport within the
plotter frame. Each is given a value between 0 and 1, inclusive,
specifying the position as a fraction of the distance across the plotter
frame from left to right or from bottom to top.
.sp
These values are used when the SPPS routine SET is called by TDINIT
(which happens when the internal parameter 'SET' has a non-zero
value).
.sp
The default values of 'VPB', 'VPL', 'VPR', and 'VPT' are .05, .05, .95,
and .95, respectively, specifying the use of most of the plotter frame.
.IP "'Rendering-Style arrays' - Integer"
The internal parameter arrays that define rendering styles are
accessed using the routines TDGTRS (to get the complete definition of
a specific rendering style) and TDSTRS (to redefine a specific
rendering style). Sixty-four different rendering styles may be defined,
indexed 1 through 64; each is defined by an array of ten quantities,
seven of which are of type INTEGER (named IFC1, IFC2, IFC3,
IFC4, ILC1, ILC2, and ILTD) and three of which are of type REAL
(named USTP, VSTP, and WSTP). These quantities may be described
as follows:
.RS
.IP
IFC1 and IFC2 are color indices specifying a range of colors to be used
for the "bottom" side of a surface (where function values are less than
the value on the surface). If IFC1 is negative, filling of triangles seen
from the "bottom" is turned off. If IFC1 is zero or greater, but IFC2 is
less than or equal to it, the color with index IFC1 is used. If IFC1 is zero
or greater and IFC2 is greater than IFC1, then a range of color indices
is specified; colors near the beginning of that range are used for
triangles that are nearly perpendicular to the line of sight, while colors
near the end of that range are used for triangles more nearly parallel
to the line of sight. (Normally, one should make triangles
perpendicular to the line of sight lighter than those parallel to the line
of sight.)
.IP
IFC3 and IFC4 are color indices specifying a range of colors to be used
for the "top" side of a surface (where function values are greater than
the value on the surface). If IFC3 is negative, filling of triangles seen
from the "top" is turned off. If IFC3 is zero or greater, but IFC4 is less
than or equal to it, the color with index IFC3 is used. If IFC3 is zero or
greater and IFC4 is greater than IFC3, then a range of color indices is
specified; colors near the beginning of that range are used for triangles
that are nearly perpendicular to the line of sight, while colors near the
end of that range are used for triangles more nearly parallel to the line
of sight. (Normally, one should make triangles perpendicular to the
line of sight lighter than those parallel to the line of sight.)
.IP
ILC1 is the color index specifying a color to be used for lines drawn on
the "bottom" side of a surface. If ILC1 is negative, the drawing of these
lines is turned off.
.IP
ILC2 is the color index specifying a color to be used for lines drawn on
the "top" side of a surface. If ILC2 is negative, the drawing of these lines
is turned off.
.IP
ILTD is a flag, which, if set non-zero, turns on the drawing of the
edges of the individual triangles into which surfaces have been
decomposed.
.IP
USTP, VSTP, and WSTP are the distances between slices in the U, V,
and W directions, respectively. If a given value is zero, the associated
slice lines are not drawn.
.sp
Note: It is the responsibility of the user to call the GKS routine GSCR to
define all of the colors to be used.
.RE
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpara, tdplch,
tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
