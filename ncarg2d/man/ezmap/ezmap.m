.TH Ezmap 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZMAP - Allows one to plot maps of the earth according to any of ten different
projections, with parallels, meridians, and continental, international,
and/or U.S. state outlines. 
.sp
EZMAPA is the name of a supplement to the EZMAP utility that allows users to
produce solid-filled maps of the earth.  The EZMAPA routines are discussed as
part of the EZMAP utility.
.sp
EZMAPB is the name of a supplement to the EZMAP utility that provides access
to improved map databases (principally one called "Earth..1", which contains
a unified higher-resolution version of everything that was in the old outline
datasets).  The EZMAPB routines are discussed as part of the EZMAP utility.
.SH SYNOPSIS
.IP "Routines Used to Draw a Simple Map" 
.sp
To draw the simple map defined by the current values of EZMAP's internal
parameters (assuming no area fills and no access to the new, improved, map
database "Earth..1", which was created in 1998), one need only execute the
single FORTRAN statement "CALL MAPDRW":
.RS 
.IP "\(bu" 4
MAPDRW - Draws a complete simple map.
.RE
.IP ""
All that MAPDRW does is call four lower-level routines. In some
situations, the user may wish to call these routines directly; they are
as follows (in the order in which they are called by MAPDRW):
.RS
.IP "\(bu" 4
MAPINT - Initializes. MAPINT must be called at least once before calling
any routine that depends on mapping lat/lon coordinates into u/v
coordinates. After one or more of MAPPOS, MAPROJ, and MAPSET has been
called, MAPINT must be called again. MAPINT does the call to the SPPS
routine SET that defines the mapping from the u/v (projection) plane
to the x/y (plotter) plane.
.IP "\(bu" 4
MAPGRD - Draws selected parallels and meridians.
.IP "\(bu" 4
MAPLBL - Labels the international date line, the equator, the Greenwich
meridian, and the poles, and draws the perimeter.
.IP "\(bu" 4
MAPLOT - Draws selected geographic outlines.  Note that this routine uses
whichever old outline dataset is selected by the value of the internal
parameter 'OU'; to access the new map database "Earth..1", which was created
in 1998, one must call instead the EZMAPB routine MPLNDR.
.RE
.IP "Routines Used to Change the Values of Internal Parameters"
.sp
The following routines are called to change the values of internal
parameters of EZMAP, and thus change the behavior of other EZMAP
routines:
.RS
.IP "\(bu" 4
MAPPOS - Determines what portion of the plotter frame is to be used.
.IP "\(bu" 4
MAPROJ - Determines the projection to be used.
.IP "\(bu" 4
MAPSET - Determines what portion of the u/v plane is to be viewed.
.IP "\(bu" 4
MAPSTC - Sets a parameter value of type CHARACTER.
.IP "\(bu" 4
MAPSTI - Sets a parameter value of type INTEGER.
.IP "\(bu" 4
MAPSTL - Sets a parameter value of type LOGICAL.
.IP "\(bu" 4
MAPSTR - Sets a parameter value of type REAL.
.RE
.IP "Routines Used to Retrieve the Values of Internal Parameters"
.sp
The following routines are used to retrieve the current values of EZMAP
parameters:
.RS
.IP "\(bu" 4
MAPGTC - Gets a parameter value of type CHARACTER.
.IP "\(bu" 4
MAPGTI - Gets a parameter value of type INTEGER.
.IP "\(bu" 4
MAPGTL - Gets a parameter value of type LOGICAL.
.IP "\(bu" 4
MAPGTR - Gets a parameter value of type REAL.
.RE
.IP "Routines Used to Save and Restore Internal Parameters"
.sp
To save/restore the current values of the internal parameters of EZMAP,
use the following:
.RS
.IP "\(bu" 4
MAPSAV - Saves the values (by writing a record on a user-specified unit).
.IP "\(bu" 4
MAPRST - Restores saved values (by reading a record from a user-specified
unit).
.RE
.IP "Routines Used to Draw Objects on a Map"
.sp
To draw objects on the map, use the following routines:
.RS
.IP "\(bu" 4
MAPTRA - Computes the u/v coordinates of a point from its latitude and
longitude. If the point is unprojectable or its projection lies outside
the current perimeter, a special value is returned to signal this.
.IP "\(bu" 4
MAPTRN - Computes the u/v coordinates of a point from its latitude and
longitude. If the point is unprojectable, a special value is returned to
signal this, but no check is made for the projected value being outside
the perimeter.
.IP "\(bu" 4
MAPFST - Does a "pen-up" move defining the start of a line to be
projected and drawn. The line is defined by a series of lat/lon
coordinates.
.IP "\(bu" 4
MAPVEC - Does a "pen-down" move defining the continuation of a line to
be projected and drawn. The line is defined by a series of lat/lon
coordinates.
.IP "\(bu" 4
MAPIT - Does "pen-up" or "pen-down" moves. This routine is called by
MAPFST and MAPVEC.
.IP "\(bu" 4
MAPIQ - Signals the end of a string of calls to MAPIT and causes its
buffers to be flushed.
.IP "\(bu" 4
MAPGCI - Given the latitudes and longitudes of two points on the surface
of the globe, this routine returns the latitudes and longitudes of a
specified number of points along the great circle route joining them.
.RE
.IP "Routines Used to Do Inverse Transformations"
.sp
The following routine was added to EZMAP early in 1992:
.RS
.IP "\(bu" 4
MAPTRI - Computes the latitude and longitude of a point from its u/v
coordinates. If the point is outside the boundary of the map, a special
value is returned.
.RE
.IP ""
The example named "mpex10" shows one of the ways in which this routine
may be used; it draws what is essentially a colored contour plot of a
data field defined on the surface of the globe, using an orthographic
projection.
.IP "Routines Used to Draw Solid-Filled Maps (EZMAPA)"
.sp
In late 1986 or early 1987, a package of routines was written allowing a
user to draw solid-filled maps of the globe. This package was named
EZMAPA and was first advertised in the NCAR Graphics User's Guide
(Version 2.00), published in August, 1987. Conceptually, the routines in
this package are really part of EZMAP; they use the same common blocks
and many of the same underlying low-level routines and they are affected
by the same set of internal parameters as the routines in EZMAP proper.
The routines of EZMAPA will be described in this document; to use them
effectively, it will be necessary to understand also the package AREAS,
which is described in a separate document. The EZMAPA routines are as
follows:
.RS
.IP "\(bu" 4
MAPBLA - Adds boundary lines to an existing area map. Routines in the
package AREAS may then be used to process that area map in various ways.
(Example: drawing a map of Europe with each country in a different
color.)  Note that this routine uses whichever old outline dataset is
selected by the value of the internal parameter 'OU'; to access the new
map database "Earth..1", which was created in 1998, one must call instead
the EZMAPB routine MPLNAM.
.IP "\(bu" 4
MAPBLM - Draws boundary lines "masked" by an existing area map.  (Example:
drawing these lines only where they do not overlay CONPACK labels.)   Note
that this routine uses whichever old outline dataset is selected by the value
of the internal parameter 'OU'; to access the new map database "Earth..1",
which was created in 1998, one must call instead the EZMAPB routine MPLNDM.
.IP "\(bu" 4
MAPGRM - Draws lines of latitude and longitude "masked" by an existing
area map. (Example: drawing these lines over water, but not over land.)
.IP "\(bu" 4
MAPITA and MAPIQA - Adds to an area map the projections of arbitrary
lines defined by lat/lon coordinates of points on the surface of the
globe. MAPBLA uses these routines to add boundary lines to an area map;
they may be called directly by the user to add his/her own set of
boundary lines to the area map.
.IP "\(bu" 4
MAPITM and MAPIQM - Draws, masked by an area map, the projections of
arbitrary lines defined by lat/lon coordinates of points on the surface
of the globe. MAPGRM uses these routines to draw masked lines of latitude
and longitude; they may be called directly by the user to draw other
masked lines.
.IP "\(bu" 4
MAPACI - A function which, given the "area identifier" for a particular
area defined by the boundaries in one of the old EZMAP outline datasets,
returns a suggested color index for that area; it is guaranteed that, if
the suggested color indices are used, no two areas having a boundary in
common will have the same color.  Note that this function should not be
used to select color indices for areas defined by the new map
database "Earth..1", which was created in 1998; for that purpose, use
EZMAPB functions instead (in particular, MPISCI).
.RE
.IP "Routines Used to Access New Datasets (EZMAPB)"
.sp
In early 1998, a new world map database, called "Earth..1", was created for
use with EZMAP; this database has higher-resolution coastlines, it has been
updated to reflect many of the political changes that have taken place over
the years since EZMAP came into existence, and it is structured differently,
allowing for greater flexibility and ease of use and providing for easier
changes and extensions in the future.
.sp
Each area defined by the database has 1) a "area identifier" (an integer
uniquely identifying it), 2) an "area type" specifying its level in a
hierarchy of areas, 3) a suggested color index, 4) an area identifier
specifying its "parent" area (the area of which it is a part), and 5) a
name.  For example, there is an area named "Madeline Island" which is of
type 4 (used for a state or a portion thereof) and has suggested color
index 6.  Its parent is an area named "Wisconsin", which is also of type
4 and has suggested color index 6.  The parent of "Wisconsin"
is "Conterminous US", which is of type 3 (used for a country or a portion
thereof) and has suggested color index 3.  The parent of "Conterminous
US" is "United States", which is also of type 3 and has suggested color
index 3.  The parent of "United States" is "North America", which is of
type 2 and has suggested color index 5.  The parent of "North America"
is "Land", which is of type 1 and has suggested color index 2.  The area
named "Land" is at the top of the hierarchy and therefore has no parent
(when you ask for the area identifier of its parent, you get a zero).
.sp
One may use the database at any of five specified hierarchical levels:
1 => land/water, 2 => continents, 3 => countries, 4 => states, and 5 =>
counties (so far, no counties are included).  When the database is used
at a particular level, entities that exist only at lower levels (larger
level numbers) effectively disappear.
.sp
The new database was created from data available on the World Wide Web,
using a new interactive editor based on NCAR Graphics.  There are plans
to make this editor available, so that a knowledgeable user can create
a database tailored to his or her own needs: for example (assuming that
one can obtain the necessary outline data), it should now be relatively
easy to create and use a Pangaea database with EZMAP.
.sp
A new package of routines is used to access "Earth..1" and other databases
in the same format; this package is called EZMAPB.  Conceptually, the
EZMAPB routines are just part of EZMAP; they use the same common blocks
and many of the same underlying low-level routines and they are affected
by the same set of internal parameters as the routines in EZMAP proper.
.sp
The principal EZMAPB routines are as follows:
.RS
.IP "\(bu" 4
MPLNAM (MaP LiNes, to Area Map) - A routine to extract boundary lines from
a specified database and send them to an area map.
.IP "\(bu" 4
MPLNDM (MaP LiNes, Draw Masked) - A routine to extract boundary lines from a
specified database and draw them, masked by the contents of an area map.
.IP "\(bu" 4
MPLNDR (MaP LiNes, Draw) - A routine to extract boundary lines from a specified
database and draw them.
.IP "\(bu" 4
MPLNRI - A routine to force the reading of certain information from a database
into labelled COMMON blocks inside EZMAP, so that subsequent references to some
of the functions described below will have that information to work with.
(Each of the routines MPLNAM, MPLNDM, and MPLNDR reads this data as a side
effect; MPLNRI is provided for use in cases in which none of the other three
routines has yet been called.)
.RE
.IP ""
As each of the EZMAPB routines MPLNAM, MPLNDM, and MPLNDR processes
boundary lines from a specified database, it calls an EZMAPB routine
named MPCHLN (the default version of which does nothing):
.RS
.IP "\(bu" 4
MPCHLN - A user-replaceable routine that can be made to change line style,
color, line width, and so on as the boundary lines from a database are being
drawn; it can also be made to delete particular lines or to change the area
identifiers associated with them.  The arguments of MPCHLN tell it which of
the EZMAPB routines is calling it and whether it's being called before or
after the line is processed; they also supply the "line type" of the line
being drawn, the area identifiers of the areas on either side of it, and the
actual coordinates defining the line.  Line types are similar to area types
(1 => land/water, 2 => continents, 3 => countries, 4 => states, and 5 =>
counties).
.RE
.IP ""
Another EZMAPB routine, named MPGLTY, may be called by the user from within
the line-processing routine specified by the final argument in a call to
MPLNDM:
.RS
.IP "\(bu" 4
MPGLTY - Retrieves the line type of the line being drawn.
.RE
.IP ""
There is a group of EZMAPB functions providing access to information about
the areas defined by a database being used; these may be referenced at any
time the appropriate information has been loaded into EZMAPB's common
blocks (that is, after calling one of MPLNAM, MPLNDM, MPLNDR, or MPLNRI),
but they are normally to be referenced from within the area-processing
routine specified as the final argument in a call to the AREAS routine
ARSCAM, in which they may be used to obtain information determining the
manner in which the areas are to be rendered:
.RS
.IP "\(bu" 4
MPIPAI - A function whose value is non-zero if and only if the area with a
specified area identifier is part of the area having a second specified area
identifier.
.IP "\(bu" 4
MPIPAN - A function whose value is non-zero if and only if the area with a
specified area identifier is part of the area having a specified name.
.IP "\(bu" 4
MPIOAR - A function whose value is the area identifier of the smallest
area that is defined at or above a specified level in the area hierarchy and
of which the area having a specified area identifier is a part.
.IP "\(bu" 4
MPIATY - A function whose value is the type of the area having a specified
area identifier.
.IP "\(bu" 4
MPIPAR - A function whose value is the area identifier of the parent of the
area having a specified area identifier.
.IP "\(bu" 4
MPISCI - A function whose value is the suggested color index for an area
having a specified area identifier.
.IP "\(bu" 4
MPNAME - A function whose value is the name of the area having a particular
area identifier.
.IP "\(bu" 4
MPFNME - A function whose value is the full name of the area having
a specified area identifier, up to a specified level in the hierarchy of areas;
the full name of an area consists of its own name, preceded by the name of its
parent, preceded by the name of its parent's parent, and so on; the various
components of the name are separated by the 3-character string ' - ' (a blank,
a dash, and another blank).
.RE
.IP ""
Two additional EZMAPB functions are provided; these have nothing to do with
mapping, really, but can be useful in dealing with character strings:
.RS
.IP "\(bu" 4
MPIFNB - A function whose value is the index of the first non-blank character
in a character string.
.IP "\(bu" 4
MPILNB - A function whose value is the index of the last non-blank character
in a character string.
.RE
.IP "Miscellaneous Other Routines"
.sp
The following EZMAP routines are used for the purposes stated:
.RS
.IP "\(bu" 4
MAPRS - Re-executes the "CALL SET" done during the last call to MAPINT.
This is useful when there has been an intervening call to a utility that
calls SET. It is quite common for a background drawn by EZMAP to be
placed in a flash buffer (as created by the package "GFLASH"). When the
contents of the flash buffer are copied to the metafile being created, if
it is desired to draw something on the EZMAP background, MAPRS may first
have to be called to ensure that the correct SET call is in effect.
.IP "\(bu" 4
MPRST - Resets the internal state of EZMAP/EZMAPA to the default.
.IP "\(bu" 4
SUPMAP - Draws a map with a single call. An implementation of the routine
from which EZMAP grew.
.RE
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_mapaci
.br
c_mapbla
.br
c_mapdrw
.br
c_mapfst
.br
c_mapgrd
.br
c_mapgrm
.br
c_mapgtc
.br
c_mapgtc
.br
c_mapgti
.br
c_mapgtl
.br
c_mapgtr
.br
c_mapint
.br
c_mapiq
.br
c_mapiqa
.br
c_mapiqm
.br
c_mapit
.br
c_mapita
.br
c_mapitm
.br
c_maplbl
.br
c_maplot
.br
c_mappos
.br
c_maproj
.br
c_maprs
.br
c_maprst
.br
c_mapsav
.br
c_mapset
.br
c_mapstc
.br
c_mapsti
.br
c_mapstl
.br
c_mapstr
.br
c_maptra
.br
c_maptri
.br
c_maptrn
.br
c_mapvec
.br
c_mpfnme
.br
c_mpglty
.br
c_mpiaty
.br
c_mpifnb
.br
c_mpilnb
.br
c_mpiola
.br
c_mpiosa
.br
c_mpipai
.br
c_mpipan
.br
c_mpipar
.br
c_mpisci
.br
c_mplnam
.br
c_mplndm
.br
c_mplndr
.br
c_mplnri
.br
c_mpname
.br
c_mprset
.br
c_supmap
.SH USER-MODIFIABLE INTERNAL ROUTINES
The following EZMAP routines are used for the purposes stated:
.RS
.IP "\(bu" 4
MAPUSR - This routine is called by various EZMAP routines just before and
just after drawing parts of the map. By default, grid lines are drawn
using software dashed lines and geographical outlines are drawn using
either solid lines or dotted lines. The dash pattern used for the grid
lines, the flag which says whether outlines are solid or dotted, and the
color indices of various parts of the map are all user-settable
parameters, but more complete control of color indices, spot size, dash
pattern, etc., may be achieved by supplying one's own version of MAPUSR;
a user version may be as complicated as is required to achieve a desired
effect.  Note that this routine is not called by any of the EZMAPB routines;
they call MPCHLN instead.
.IP "\(bu" 4
MAPEOD - This routine is called by the EZMAP routine MAPLOT and by the EZMAPA
routines MAPBLA and MAPBLM; in each case, it is called once for each segment
in the outline dataset. The user may supply a version which examines the
segment to see if it ought to be plotted and, if not, to delete it.  This
can be used (for example) to reduce the clutter in northern Canada.  Note
that this routine is not called by any of the EZMAPB routines; they call
MPCHLN instead.
.IP "\(bu" 4
MPCHLN - This routine is called by the EZMAPB routines MPLNAM, MPLNDM, and
MPLNDR; in each case, it is called just before and just after the processing
of each segment in the map database.  The default version does nothing; a
user-supplied version can do for the new databases what MAPUSR and MAPEOD
did for the old ones.
.RE
.SH ACCESS 
To use EZMAP Fortran or C routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.
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
MAPBLA - UNCLEARED PRIOR ERROR
.br
MAPCHI - ERROR EXIT FROM GQPLCI
.br
MAPCHI - ERROR EXIT FROM GQPMCI
.br
MAPCHI - ERROR EXIT FROM GQTXCI
.br
MAPDRW - UNCLEARED PRIOR ERROR
.br
MAPFST - UNCLEARED PRIOR ERROR
.br
MAPGCI - UNCLEARED PRIOR ERROR
.br
MAPGRD - UNCLEARED PRIOR ERROR
.br
MAPGRM - UNCLEARED PRIOR ERROR
.br
MAPGTC - UNCLEARED PRIOR ERROR
.br
MAPGTC - UNKNOWN PARAMETER NAME
.br
MAPGTI - UNCLEARED PRIOR ERROR
.br
MAPGTI - UNKNOWN PARAMETER NAME
.br
MAPGTL - UNCLEARED PRIOR ERROR
.br
MAPGTL - UNKNOWN PARAMETER NAME
.br
MAPGTR - UNCLEARED PRIOR ERROR
.br
MAPGTR - UNKNOWN PARAMETER NAME
.br
MAPINT - ANGULAR LIMITS TOO GREAT
.br
MAPINT - ATTEMPT TO USE NON-EXISTENT PROJECTION
.br
MAPINT - MAP HAS ZERO AREA
.br
MAPINT - MAP LIMITS INAPPROPRIATE
.br
MAPINT - UNCLEARED PRIOR ERROR
.br
MAPIO - EOF ENCOUNTERED IN OUTLINE DATASET
.br
MAPIO - ERROR ON READ OF OUTLINE DATASET
.br
MAPIQ - UNCLEARED PRIOR ERROR
.br
MAPIQA - UNCLEARED PRIOR ERROR
.br
MAPIQM - UNCLEARED PRIOR ERROR
.br
MAPIT - UNCLEARED PRIOR ERROR
.br
MAPITA - UNCLEARED PRIOR ERROR
.br
MAPITM - UNCLEARED PRIOR ERROR
.br
MAPLBL - UNCLEARED PRIOR ERROR
.br
MAPLMB - UNCLEARED PRIOR ERROR
.br
MAPLOT - UNCLEARED PRIOR ERROR
.br
MAPPOS - ARGUMENTS ARE INCORRECT
.br
MAPPOS - UNCLEARED PRIOR ERROR
.br
MAPROJ - UNCLEARED PRIOR ERROR
.br
MAPROJ - UNKNOWN PROJECTION NAME
.br
MAPRS - UNCLEARED PRIOR ERROR
.br
MAPRST - EOF ON READ
.br
MAPRST - ERROR ON READ
.br
MAPRST - UNCLEARED PRIOR ERROR
.br
MAPSAV - ERROR ON WRITE
.br
MAPSAV - UNCLEARED PRIOR ERROR
.br
MAPSET - UNCLEARED PRIOR ERROR
.br
MAPSET - UNKNOWN MAP AREA SPECIFIER
.br
MAPSTC - UNCLEARED PRIOR ERROR
.br
MAPSTC - UNKNOWN OUTLINE NAME
.br
MAPSTC - UNKNOWN PARAMETER NAME
.br
MAPSTI - UNCLEARED PRIOR ERROR
.br
MAPSTI - UNKNOWN PARAMETER NAME
.br
MAPSTL - UNCLEARED PRIOR ERROR
.br
MAPSTL - UNKNOWN PARAMETER NAME
.br
MAPSTR - UNCLEARED PRIOR ERROR
.br
MAPSTR - UNKNOWN PARAMETER NAME
.br
MAPTRA - UNCLEARED PRIOR ERROR
.br
MAPTRI - UNCLEARED PRIOR ERROR
.br
MAPTRN - ATTEMPT TO USE NON-EXISTENT PROJECTION
.br
MAPTRN - UNCLEARED PRIOR ERROR
.br
MAPVEC - UNCLEARED PRIOR ERROR
.br
MPGETC - UNCLEARED PRIOR ERROR
.br
MPGETI - UNCLEARED PRIOR ERROR
.br
MPGETL - UNCLEARED PRIOR ERROR
.br
MPGETR - UNCLEARED PRIOR ERROR
.br
MPLNAM - Can't form name of ".names" file
.br
MPLNAM - Can't open the ".lines" file
.br
MPLNAM - Can't open the ".names" file
.br
MPLNAM - Read bad index from ".names" file
.br
MPLNAM - Read error on ".lines" file
.br
MPLNAM - Read error on ".names" file
.br
MPLNAM - UNCLEARED PRIOR ERROR
.br
MPLNDM - Can't form name of ".names" file
.br
MPLNDM - Can't open the ".lines" file
.br
MPLNDM - Can't open the ".names" file
.br
MPLNDM - Read bad index from ".names" file
.br
MPLNDM - Read error on ".lines" file
.br
MPLNDM - Read error on ".names" file
.br
MPLNDM - UNCLEARED PRIOR ERROR
.br
MPLNDR - Can't form name of ".names" file
.br
MPLNDR - Can't open the ".lines" file
.br
MPLNDR - Can't open the ".names" file
.br
MPLNDR - Read bad index from ".names" file
.br
MPLNDR - Read error on ".lines" file
.br
MPLNDR - Read error on ".names" file
.br
MPLNDR - UNCLEARED PRIOR ERROR
.br
MPLNRI - Can't form name of ".names" file
.br
MPLNRI - Can't open the ".names" file
.br
MPLNRI - Read bad index from ".names" file
.br
MPLNRI - Read error on ".names" file
.br
MPLNRI - UNCLEARED PRIOR ERROR
.br
MPRSET - UNCLEARED PRIOR ERROR
.br
MPSETC - UNCLEARED PRIOR ERROR
.br
MPSETI - UNCLEARED PRIOR ERROR
.br
MPSETL - UNCLEARED PRIOR ERROR
.br
MPSETR - UNCLEARED PRIOR ERROR
.br
SUPCON - UNCLEARED PRIOR ERROR
.br
SUPMAP - UNCLEARED PRIOR ERROR
.sp
.SH SEE ALSO
Online:
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
mpchln,
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
.sp
Hardcopy: 
NCAR Graphics Contouring and Mapping Tutorial; 
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
