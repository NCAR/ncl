.\"
.\"	$Id: gfill_area.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GFILL_AREA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gfill_area (Fill area) - output primitive for filling polygonal areas.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gfill_area(const Gpoint_list *point_list);
.SH DESCRIPTION
.IP point_list.num_points 12
(Gint, Input) - The number of points in the polygon to be filled.  
Must be greater than two.
.IP point_list.points 12
(Gpoint *, Input) - The X and Y world coordinates of the polygon.
.SH USAGE
The area to be filled is delimited by the sequence of
straight line segments connecting the successive 
points  
((point_list.points[0].x, point_list.points[0].y), ...(point_list.points[point_list.num_points-1].x, point_list.points[point_list.num_points-1].y))
The last point in the polygon is connected to the first point with a straight 
line segment in the case that it is not the same as the first point.
.sp
Given a polygon whose edges cross each other, it 
becomes ambiguous as to what constitutes the "inside" 
of the polygon.  The algorithm used in GKS is as 
follows: for a given point, draw a ray
starting at that point and going to infinity in any
direction; if the number of intersections between the ray and 
the polygon is odd, the point is within the polygon,
otherwise it is outside.  If the straight line passes 
through a polygon vertex tangentially, the intersection count 
is not affected.  If a point is within the polygon, it 
is included in the area to be filled.
.sp
Several interior styles (hollow, solid, and various hatch patterns)
can be selected.  See the man pages for gset_fill_int_style and gset_fill_style_ind for these.
.sp
Note well: By default in GKS, the interior fill style is hollow,
or no fill. If you call gfill_area and do not get a filled
interior as you expected, you will probably need to call 
gset_fill_int_style to set the fill style to something other than "hollow".
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gset_fill_int_style(3NCARG),
.BR gset_fill_style_ind(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_fill_colr_ind(3NCARG),
.BR ginq_fill_int_style(3NCARG),
.BR ginq_fill_style_ind(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
