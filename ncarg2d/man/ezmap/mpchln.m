.TH MPCHLN 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPCHLN - This routine is called repeatedly as boundary lines are processed by
MPLNAM, MPLNDM, and MPLNDR.  The default version of the routine does nothing.
A user-supplied version may take action to change various characteristics of
the lines.
.SH SYNOPSIS
CALL MPCHLN (IACT,ILTY,IOAL,IOAR,NPTS,PNTS)
.SH C-BINDING SYNOPSIS
None.
.SH DESCRIPTION 
.IP IACT 12
(an input expression of type INTEGER) specifies which of the EZMAPB routines
has called MPCHLN and what that routine is doing or has done with the boundary
line defined by the arguments NPTS and PNTS.  IACT is positive if the line is
about to be processed, negative if the line was just processed; its absolute
value is 1 if the call comes from MPLNAM (which puts lines into an area map),
2 if the call comes from MPLNDM (which draws lines masked by some area map),
and 3 if the call comes from MPLNDR (which just draws lines).
.IP ILTY 12
(an input expression of type INTEGER) specifies the type of the line: 1 => a
line separating land from water, 2 => a line separating one "continent" from
another (as, for example, Africa from Eurasia, North America from Central
America, or Central America from South America), 3 => a line separating one
country from another, 4 => a line separating one state from another, and 5 =>
a line separating one county from another.
.IP "IOAL and IOAR" 12
(input/output variables of type INTEGER) are the identifiers of the areas to
the left and right, respectively, of the line.  (Left and right are defined
from the standpoint of a viewer standing at point 1 of the line and looking
toward point 2.)  The values of IOAL and IOAR may be changed by a knowledgeable
user.
.IP NPTS 12
(an input/output variable of type INTEGER), on entry, is the number of points
defining the line.  NPTS may be zeroed by MPCHLN to suppress any use of the
line by the calling routine.
.IP PNTS 12
(an input/output array, dimensioned 2*NPTS, of type REAL) is an array of point
coordinates.  PNTS(1) and PNTS(2) are the latitude and longitude of the first
point, PNTS(3) and PNTS(4) the latitude and longitude of the second point, ...
PNTS(2*NPTS-1) and PNTS(2*NPTS) the latitude and longitude of the last point.
All values are in degrees.  Longitudes are all between -180 and +180; no
segment crosses the meridian at -180 (+180) degrees.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
MPCHLN is called by each of the routines MPLNAM, MPLNDM, and MPLNDR just
before and just after the processing of each boundary line read from a
specified map database.  A user-supplied version may take action to change
various characteristics of the lines.  For example, the area identifiers to
be used for the line can be changed, the line can be deleted entirely, and
line styles, colors, and widths can be set.
.SH EXAMPLES
Use the ncargex command to see the following relevant example: mpex11.
.SH ACCESS
To use the default version of MPCHLN, load the NCAR Graphics libraries ncarg,
and ncarg_gks, preferably in that order.  To supply your own version, just
compile it and load it.
.SH SEE ALSO
Online:
ezmap,
ezmap_params,
mapaci,
mapbla,
mapblm,
mapdrw,
mapeod,
mapfst,
mapgci,
mapgrd,
mapgrm,
mapgtc,
mapgti,
mapgtl,
mapgtr,
mapint,
mapiq,
mapiqa,
mapiqd,
mapiqm,
mapit,
mapita,
mapitd,
mapitm,
maplbl,
maplmb,
maplot,
mappos,
maproj,
maprs,
maprst,
mapsav,
mapset,
mapstc,
mapsti,
mapstl,
mapstr,
maptra,
maptri,
maptrn,
mapusr,
mapvec,
mpfnme,
mpgetc,
mpgeti,
mpgetl,
mpgetr,
mpglty,
mpiaty,
mpifnb,
mpilnb,
mpiola,
mpiosa,
mpipai,
mpipan,
mpipar,
mpisci,
mplnam,
mplndm,
mplndr,
mplnri,
mpname,
mprset,
mpsetc,
mpseti,
mpsetl,
mpsetr,
supmap,
supcon,
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
