.\"
.\"	$Id: ezmap.m,v 1.1.1.1 1992-04-17 22:30:22 ncargd Exp $
.\"
.TH EZMAP 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " EZMAP - Plots world/regional border outlines
.dsS1 " CALL MAPDRW Draws a complete map  
.dsS2 " CALL MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS) \ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ Examine each outline-dataset segment
.dsS3 " CALL MAPFST (RLAT,RLON) Draw lines
.dsS4 " CALL MAPGRD Draw grid
.dsS5 " CALL MAPGTx (WHCH,xVAL) get parameters
.dsS6 " CALL MAPINT Initialization
.dsS7 " CALL MAPIQ Terminate string call
.dsS8 " CALL MAPIT (RLAT,RLON,IFST) Draw lines
.dsS9 " CALL MAPLBL Labels map
.dss1 " CALL MAPLOT Draw geographical outlines
.dss2 " CALL MAPPOS (XLOW,XROW,YBOW,YTOW) Position map
.dss3 " CALL MAPROJ (JPRJ,PLAT,PLON,ROTA) Set projection
.dss4 " CALL MAPRS Recalls SET
.dss5 " CALL MAPRST (IFNO) Restores MAPSAVed state
.dss6 " CALL MAPSAV (IFNO) Saves current state
.dss7 " CALL MAPSET (JLTS,PLM1,PLM2,PLM3,PLM4) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ specify rectangular portion of u/v plane to draw
.dss8 " CALL MAPSTx (WHCH,xVAL) Set parameters
.dss9 " CALL MAPTRN (RLAT,RLON,UVAL,VVAL) Project points
.dsN1 " CALL MAPUSR (IPRT) Change appearance of map
.dsN2 " CALL MAPVEC (RLAT,RLON) Draw lines
.dsN3 " CALL SUPCON (RLAT,RLON,UVAL,VVAL) Replaced by MAPTRN
.dsN4 " CALL SUPMAP (...) Draws map
.nrsN 22
./" USE tsi to PRINT THIS FILE!
.pn 205
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9EZMAP\s11@"
.EF "@\s\fB11%\s9\fR@@August 1987\s11@"
.OF "@\s9August 1987@@\s11\fB%\fR@"
.de hD          \" Heading macro level one
.br
.ne 5
.ti -1.5i
.sp 2
.ps +3
.ft B           \" boldface, 14 pt.
\\$1
.ft R
.ps -3
..
.de >>          \" display for indented lines
.in +.25i       \" usage: .>>
.sp
.nf
..              
.de <<          \" end of display for indented lines
.fi
.in -.25i       \" usage: .<<
.sp
..              
.de sf          \"start fortran (constant spacing)
.ps 10
.vs 12
.nf
.ft L
..
.de ef          \"end fortran (resume variable spacing & prev. size & font)
.ft
.fi
.ps
.vs
..
.br
.bp
.S 14
.S 11
SUBROUTINE EZMAP
.H 2 "\s10INTRODUCTION\s11"
The EZMAP package allows one to plot maps of the earth according to any
of ten different projections, with parallels, meridians, and continental,
international, and/or U.S. state outlines.  The origin and orientation of the
projection are selected by the user.  Points on the earth defined by latitude
and longitude are mapped to points in the plane of projection - the u/v plane.
A rectangular frame whose sides are parallel to the u and v axes is chosen;
material within that frame (or an inscribed elliptical frame) is mapped to
the plane of the plotter - the x/y plane - for plotting.  The u and v axes
are parallel to the x and y axes, respectively.
.P
Two changes were made in EZMAP in July 1987:
MAPEOS was replaced by MAPEOD and the seven intensity parameters 
were replaced by color index
parameters.
Also, the EZMAP example codes were rewritten for
execution in a GKS environment.
.P
To draw a complete map, as directed by the current values of the EZMAP
parameters, the user need only execute the statement "CALL MAPDRW."  Thus,
the principal EZMAP routine is this:
.VL .9i
.LI "\fBMAPDRW\fR"
Draw a complete map.
MAPDRW calls four routines.  In some situations, the user may wish to call
them directly.  These routines are as follows:
.LI "\fBMAPINT\fR"
Initialize.  
MAPINT must be called initially and
again after one or more of MAPPOS, MAPROJ, and
MAPSET has been called.
.LI "\fBMAPGRD\fR"
Draws selected parallels and meridians.
.LI "\fBMAPLBL\fR"
Labels the international date line, the equator,
the Greenwich meridian, and the poles, and draws
the perimeter.
.LI "\fBMAPLOT\fR"
Draws selected geographic outlines.
.LE
.sp
The following routines are used to change the values of EZMAP parameters, and
thus the appearance of the map:
.VL .9i
.LI "\fBMAPPOS\fR"
Determine what portion of the plotter frame is
to be used.
.LI "\fBMAPROJ\fR"
Determine the projection to be used.
.LI "\fBMAPSET\fR"
Determine what portion of the u/v plane is to be
viewed.
.LI "\fBMAPSTC\fR"
Set a parameter of type CHARACTER.
.LI "\fBMAPSTI\fR"
Set a parameter of type INTEGER.
.LI "\fBMAPSTL\fR"
Set a parameter of type LOGICAL.
.LI "\fBMAPSTR\fR"
Set a parameter of type REAL.
.LE
.sp
The following routines are used to retrieve the current values of EZMAP
parameters:
.VL .9i
.LI "\fBMAPGTC\fR"
Get a parameter of type CHARACTER.
.LI "\fBMAPGTI\fR"
Get a parameter of type INTEGER.
.LI "\fBMAPGTL\fR"
Get a parameter of type LOGICAL.
.LI "\fBMAPGTR\fR"
Get a parameter of type REAL.
.LE
.sp
To save/restore the current values of EZMAP parameters, use the following:
.VL .9i
.LI "\fBMAPSAV\fR"
To save the values.
.LI "\fBMAPRST\fR"
To restore saved values.
.LE
.sp
To draw objects on the map, use the following routines:
.VL .9i
.LI "\fBMAPTRN\fR"
To compute the u/v coordinates of a point from its
latitude and longitude.
.LI "\fBMAPIT\fR"
To do "pen-up" or "pen-down" moves.
.LI "\fBMAPFST\fR"
To do "pen-up" moves.
.LI "\fBMAPVEC\fR"
To do "pen-down" moves.
.LI "\fBMAPIQ\fR"
To signal the end of a string of calls to MAPIT and
to cause its buffers to be flushed.
.LE
.sp
To re-execute the "CALL SET" originally executed by MAPINT, use this routine:
.VL .9i
.LI "\fBMAPRS\fR"
To re-execute the "CALL SET".
.LE
.sp
By default, grid lines are drawn using software dashed lines and geographical
outlines are drawn using either solid lines or dotted lines.  The dash pattern
used for the grid lines, the flag which says whether outlines are solid or
dotted, and the color indices of various parts
of the map are all user-settable
parameters.  More complete control of color indices, spot size,
dash pattern, etc., may be achieved by supplying one's own version of the
routine MAPUSR, which is called by EZMAP just before and just after drawing
various parts of the map; a user version of this routine may be as complicated
as is required to achieve a desired effect.
.P
The routine MAPEOD is called by EZMAP once for each segment in the outline
dataset.  The
user may supply a version which examines the segment to see if it ought to be
plotted and, if not, to delete it.  This may be used (for example) to reduce
the clutter in northern Canada.
.P
The routine SUPMAP, from which EZMAP grew, is implemented within it and
allows one to draw a complete map with a single, rather lengthy, call.  The
routine SUPCON, which is the old analogue of MAPTRN, is also implemented.
.H 2 "\s10PROJECTIONS\s11"
EZMAP offers ten different projections, in three groups:  conical, azimuthal,
and cylindrical.
.H 3 "Conical Projections"
Conical projections map the surface of the earth onto a cone which is either
tangent to the earth along a single circle or intersects it along two circles.
The cone is cut along some line passing through its vertex and opened up onto
a flat surface.  The only conical projection offered by EZMAP is the Lambert
conformal with two standard parallels (becoming a simple conic with one
standard parallel if the two parallels are made equal).  The cone intersects
the earth along two user-specified standard parallels (lines of latitude),
which would normally both be in the Northern Hemisphere or in the Southern
Hemisphere; the cone is cut along the line opposite a user-specified central
meridian (line of longitude) and laid flat on the u/v plane with either the
North Pole or the South Pole (as implied by the standard parallels) at the
origin.
.P
If LAT1 and LAT2 are the latitudes of the two standard parallels and LAT1 is
not equal to LAT2, the so-called "cone constant" is given by the formula
.sp
.>>
.ce
LOG(COS(LAT1))-LOG(COS(LAT2))
.br
CONE =~~______________________________________________
.sp
.ce
~~~LOG(TAN(45-S*LAT1/2))-LOG(TAN(45-S*LAT2/2))
.<<
.sp
where "S" is +1 in the Northern Hemisphere and -1 in the Southern Hemisphere.
If LAT1 equals LAT2, then
.DS
.>>
CONE = COS(90-S*LAT1)
.<<
.DE
The value of CONE is between 0 and 1;  CONE*360 is the angular separation
between the edges of the cut after the cone is opened onto the plane, as
measured across the surface of the flattened cone.  If (RLAT,RLON) is a point
to be projected, then the formulas
.DS
.>>
R = (TAN(45-S*RLAT/2))**CONE
U = R*SIN(CONE*(RLON-CLON))
V = -S*R*COS(CONE*(RLON-CLON))
.<<
.DE
where CLON is the longitude of the central meridian, give the coordinates of
the projected point in the u/v plane.
.P
The whole globe projects onto the entire u/v plane minus a wedge with its
apex at the origin.  This projection is normally used to depict mid-latitude
regions of limited extent, for which it is relatively distortion-free.  It
has the property of preserving angles.  See example 1.
.H 3 "Azimuthal Projections"
Azimuthal projections map the surface of the earth (or of a single hemisphere
of it) onto a u/v plane whose origin touches the earth at the user-specified
point (PLAT,PLON).  The image may be rotated by a user-specified
angle ROTA.  The azimuthal projections are generated as follows:
.BL
.LI
Step 1:  Imagine that the earth is placed behind the u/v plane so that the
point at latitude 0 and longitude 0 just touches the plane at the point (0,0),
the North Pole is at the top, and the South Pole is at the bottom.
.LI
Step 2:  Rotate the earth about its polar axis until the v axis is tangent to
the meridian identified by PLON (and that meridian is therefore closest to
you).
.LI
Step 3:  Rotate the earth, tilting one of the poles directly
toward you and the other pole directly away from you, until the point
(PLAT,PLON) is at the origin of the u/v plane.
.LI
Step 4:  Rotate the earth clockwise through the angle ROTA about a line
perpendicular to the u/v plane passing through the point (0,0).
.LI
Step 5:  Using lines of projection emanating from a central point within or
behind the earth (depending on the projection type), project geographical
outlines, parallels, and meridians from the earth's surface onto the u/v
plane.
.LI
Step 6:  Set up linear scales along the u and v axes.  The ranges of u and
v depend on the projection type.
.LI
Step 7:  Draw a rectangular or elliptical portion of the resulting map.
.LE
.P
If A is the angular separation, in degrees, of a point P, to be projected,
from the point (PLAT,PLON), SINA is the sine of A, COSA is the cosine of A,
and R is the linear distance of the projected point P' from the u/v origin,
the various azimuthal projections may be described in terms of the
relationship between A and R, as follows:
.BL
.LI
\fBStereographic:\fR  R=TAN(A/2)=(1-COSA)/SINA.  As A approaches
180 degrees, R approaches infinity.  The entire globe is projected
to the entire u/v plane.  In practice, distortion becomes great
beyond R=2, when A is approximately 127 degrees.  The center of the
projection is the point on the earth's surface
opposite the point of tangency with the projection plane.
See examples 2, 4, 5, and 7.
.LI
\fBOrthographic:\fR  R=SINA.  Points for which A is greater than 90 degrees are
treated as invisible.  Thus, a hemisphere is projected inside a circle of
radius 1.  The center of the projection is at infinity; all projection lines
are parallel to each other and perpendicular to the u/v plane.
See example 5.
.LI
\fBLambert equal-area:\fR  R=2*SINA/SQRT(2*(1+COSA)).  As A approaches 180 degrees,
R approaches 2.  The entire globe is projected into a circle of radius 2.
See example 5.
.LI
\fBGnomonic:\fR  R=TAN(A)=SINA/COSA.  Points for which A is greater than 90 degrees
are invisible.  Thus, a hemisphere is projected to the entire u/v plane.  In
practice, distortion becomes great beyond R=2, when A is approximately 65
degrees.  The center of this projection is the center of the earth.
See example 5.
.LI
\fBAzimuthal Equidistant:\fR  R=A*pi/180.  As A approaches 180 degrees, R
approaches pi (3.1415926. . .).  Thus, the entire globe is projected within a
circle of radius pi.
See example 5.
.LI
\fBBasic satellite-view:\fR  R=SQRT(SA*SA-1)*SINA/(SA-COSA), where 'SA' is the
distance from the center of the earth to a satellite above the point
(PLAT,PLON), in multiples of the
earth's radius.  Points for which COSA is less than 1/SA are invisible.
The portion of the earth's surface which would be visible from the satellite
is projected inside a circle of radius 1.  The center of the projection is
at the satellite's position.  As the satellite moves further and further out,
the basic satellite-view projection approaches the orthographic projection.
See example 5 and the final example.
.P
The basic satellite-view projection gives a view of the earth as seen from
a satellite looking straight down toward the center of the earth.  Two other
user-settable parameters, called 'S1' and 'S2', are available; they affect
the satellite-view projection as follows:  S1 measures the angle between the
line to the center of the earth and the line of sight of the satellite (the
line to which the projection plane is perpendicular).  The default value of
S1 is zero, which gives the basic satellite view.  If S1 is non-zero, S2
specifies the angle from the positive u axis of the basic satellite view
counter-clockwise to the line OP, where O is the origin of the basic view and
P is the projection (a single point), on the basic view, of the desired line
of sight from the satellite.  When S1 and S2 are used, the part of the
earth projected is the same as for the basic satellite-view projection, but
the part of the u/v plane covered by the projection is an ellipse with its
major axis at an angle S2 to the u axis.  See example 6.
.LE
.H 3 "Cylindrical Projections"
Cylindrical projections map the surface of the earth onto a cylinder which is
tangent to the earth along a great circle passing through the user-specified
point (PLAT,PLON) and tilted at a user-specified angle ROTA.  The cylinder is
cut along a line parallel to its axis and unrolled onto the plane.  The
cylindrical projections are generated as follows:
.BL
.LI
Step 1:  Imagine that the earth is placed behind the u/v plane so that the
point at latitude 0 and longitude 0 just touches the plane at the point (0,0),
the North Pole is at the top, and the South Pole is at the bottom.
.LI
Step 2:  Rotate the earth about its polar axis until the v axis is tangent to
the meridian identified by PLON (and that meridian is therefore closest to
you).
.LI
Step 3:  Rotate the earth, tilting one of the poles directly
toward you and the other pole directly away from you, until the point
(PLAT,PLON) is at the origin of the u/v plane.
.LI
Step 4:  Rotate the earth clockwise through the angle ROTA about a line
perpendicular to the u/v plane passing through the point (0,0).
.LI
Step 5:  Wrap the u/v plane around the globe to form a cylinder, with the u
axis touching the earth along a great circle.
.LI
Step 6:  Using a technique dependent on the projection type, project
geographical outlines, parallels, and meridians outward from the earth's
surface onto the cylinder.  See below.
.LI
Step 7:  Cut the cylinder along a line parallel to its axis and opposite the
point (0,0).
.LI
Step 8:  Unwrap the cylinder again.
.LI
Step 9:  Set up linear scales along the u and v axes.  The ranges of u and
v depend on the projection type.
.LI
Step 10:  Draw a rectangular or elliptical portion of the resulting map.
.LE
.P
What happens in step 6 above will be described for each of the three types of
cylindrical projections provided by EZMAP in the simple case where PLAT, PLON,
and ROTA are all zero.  Let RLAT and RLON be the latitude and longitude, in
degrees, of a point to be projected; RLON must lie between -180 and +180.
(If PLAT, PLON, and/or ROTA are non-zero, one must substitute for RLAT and
RLON a pseudo-latitude and a pseudo-longitude computed from the real latitude
and longitude; this is left as an exercise for the devotee of spherical
trigonometry.)  The cylindrical projections may then be described as follows:
.BL
.LI
Cylindrical Equidistant:  U and V are computed using the equations
.DS
.>>
U=RLON
V=RLAT
.<<
.DE
The entire globe is projected into a rectangle in the u/v
plane.  U ranges from -180 to +180, V from -90 to +90.
See example 5.
.LI
Mercator:  U and V are computed using the equations
.DS
.>>
U=RLON*pi/180  (where pi=3.14159. . .)
V=ALOG(COT(45-RLAT/2))
.<<
.DE
The entire globe is projected into an infinite rectangle in
the u/v plane.  U ranges from -pi to +pi, V from -infinity to +infinity.
In practice, distortion becomes unacceptable for latitudes within 5 degrees
of the North or South Pole.
See examples 3, 4, 5, 8, and 9.
.LI
Mollweide-type:  The projection is not a true Mollweide.  U and V are
computed using the equations
.DS
.>>
U=RLON/90
V=COS(90-RLAT)
.<<
.DE
The entire surface of the globe is projected into an ellipse.  U ranges from
-2 to +2, V from -1 to +1.
See example 5.
.LE
.H 2 "\s10ERROR CONDITIONS\s11"
When an error occurs during a call to an EZMAP routine, the
routine SETER is called; by default, it prints an informative
error message and STOPs.  To recover from errors, insert
.DS
.>>
CALL ENTSR (IDUM,1)
.<<
.DE
at the beginning of your program.  This suppresses the STOP in
SETER, so that your program gets control back.  Then, after each
call to an EZMAP routine, insert code like the following:
.DS
.>>
.sf
IF (NERRO(IERR).NE.0) THEN
CALL EPRIN
CALL ERROF
END IF
.ef
.<<
.DE
The value of the function NERRO (and of its argument IERR) is
non-zero if and only if SETER has been called.  EPRIN prints the
error message (which has not yet been done).  ERROF turns off an
error flag in SETER, telling it that the error has been processed
and that execution is to continue.  EZMAP's own error flag ('ER')
is not cleared; it remains set until the next successful call to
MAPINT, to prevent problems in other EZMAP routines.
.P
SETER, ENTSR, NERRO, EPRIN, and ERROF are FORTRAN 77 versions of
the PORT routines SETERR, ENTSRC, NERROR, EPRINT, and ERROFF,
which are described in detail in the PORT manual.
.P
Possible error-flag values are as follows:
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w11
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w12
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w13
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w14
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w15
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w16
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w17
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w18
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w19
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w20
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w21
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w22
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.nr 38 \wMAPGTC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPGTI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPGTL
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPGTR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPINT
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPINT
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPINT
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPINT
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPROJ
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSET
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSTC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSTC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSTI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSTL
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSTR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPTRN
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPIO
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPIO
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPPOS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPRST
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPRST
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPSAV
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wATTEMPT TO USE NON-EXISTENT PROJECTION-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wANGULAR LIMITS TOO GREAT-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wMAP HAS ZERO AREA-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wMAP LIMITS INAPPROPRIATE-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PROJECTION NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN MAP AREA SPECIFIER xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN OUTLINE NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wUNKNOWN PARAMETER NAME xx-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wATTEMPT TO USE NON-EXISTENT PROJECTION-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wOUTLINE DATASET IS UNREADABLE-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wEOF ENCOUNTERED IN OUTLINE DATASET-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wARGUMENTS ARE INCORRECT-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wERROR ON READ-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wEOF ON READ-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wERROR ON WRITE-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'MAPGTC\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'MAPGTI\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'MAPGTL\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'MAPGTR\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'MAPINT\h'|\n(42u'ATTEMPT TO USE NON-EXISTENT PROJECTION
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'MAPINT\h'|\n(42u'ANGULAR LIMITS TOO GREAT
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'MAPINT\h'|\n(42u'MAP HAS ZERO AREA
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'MAPINT\h'|\n(42u'MAP LIMITS INAPPROPRIATE
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'9\h'|\n(41u'MAPROJ\h'|\n(42u'UNKNOWN PROJECTION NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'10\h'|\n(41u'MAPSET\h'|\n(42u'UNKNOWN MAP AREA SPECIFIER xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'11\h'|\n(41u'MAPSTC\h'|\n(42u'UNKNOWN OUTLINE NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'12\h'|\n(41u'MAPSTC\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'13\h'|\n(41u'MAPSTI\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'14\h'|\n(41u'MAPSTL\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'15\h'|\n(41u'MAPSTR\h'|\n(42u'UNKNOWN PARAMETER NAME xx
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'16\h'|\n(41u'MAPTRN\h'|\n(42u'ATTEMPT TO USE NON-EXISTENT PROJECTION
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'17\h'|\n(41u'MAPIO\h'|\n(42u'OUTLINE DATASET IS UNREADABLE
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'18\h'|\n(41u'MAPIO\h'|\n(42u'EOF ENCOUNTERED IN OUTLINE DATASET
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'19\h'|\n(41u'MAPPOS\h'|\n(42u'ARGUMENTS ARE INCORRECT
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'20\h'|\n(41u'MAPRST\h'|\n(42u'ERROR ON READ
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'21\h'|\n(41u'MAPRST\h'|\n(42u'EOF ON READ
.ta \n(60u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'22\h'|\n(41u'MAPSAV\h'|\n(42u'ERROR ON WRITE
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-25
.H 3 "\s10IMPLEMENTATION\s11"
The EZMAP code is written in FORTRAN 77; portability has been a prime concern.
.P
The binary outline dataset may be generated by running the program
.sp
.sf
  PROGRAM CONVRT
  DIMENSION FLIM(4),PNTS(200)
  CHARACTER*80 CHAR
  REWIND 1
  REWIND 2
  DO 1 I=1,10
  READ (1,4) CHAR
1 CONTINUE
2 READ (1,5,END=3) NPTS,IGID,IDLS,IDRS,(FLIM(I),I=1,4)
  IF (NPTS.GT.1) READ (1,6,END=3) (PNTS(I),I=1,NPTS)
  WRITE (2) NPTS,IGID,IDLS,IDRS,(FLIM(I),I=1,4),(PNTS(I),I=1,NPTS)
  GO TO 2
3 STOP
4 FORMAT (A80)
5 FORMAT (4I4,4F8.3)
6 FORMAT (10F8.3)
  END
.ef
.sp
with the PORTLIB file EZMAPCDT assigned to unit 1.  The output file, on unit
2, contains the binary outline data to be used by EZMAP.  The EZMAP routine
MAPIO (which see) must then be modified, in a manner dependent on the local
system, to access and read this file.
.P
The default values of EZMAP parameters, set by the block data routine MAPBD,
should be reviewed.  The package has been tuned for NCAR's DICOMED D48 film
recorders.
.H 3 "\s10HISTORY\s11"
About 1963, R. L. Parker of UCSD wrote the original code, called SUPERMAP,
using outline data generated by Hershey.  This was adapted for use at NCAR by
Lee, in 1968.  Revisions occurred in January, 1969, and in May, 1971.  The
code was put in standard NSSL form in October, 1973.  Further revisions
occurred in July, 1974, in August, 1976, and in July, 1978.  In late 1984 and
early 1985, the code was heavily revised to achieve compatibility with
FORTRAN 77 and GKS, to remove errors, to augment the outline datasets, and
to add enough controls to make user modification of the code unnecessary.
In 1987, certain changes were made as part of preparing to introduce a
solid-color routine called MAPLAM.  The routine MAPEOS was
removed and a routine MAPEOD implemented in its place.
Intensity parameters 'I1', 'I2', etc., were removed and the
color-index parameters 'C1', 'C2', etc. implemented.
Cicely Ridley, Jay Chalmers, and Dave Kennison (the current curator) have all
had a hand in the creation of this package.
.H 3 "\s10I/O\s11"
Graphical output is generated by calls to the routines FRSTD, VECTD, PWRIT,
and POINTS.
.P
Outline data is read from a FORTRAN logical unit.  On the CRAY computers, 
this unit is
automatically assigned and connected to the proper dataset.  It is up to the
implementor of EZMAP to make similar arrangements on other systems.
.H 3 "\s10ACCURACY\s11"
The definition of a map drawn by EZMAP is limited by two factors:  the
resolution of the outline data and the resolution of the graphics device.
.P
Data points in the continental outlines are about one degree apart and the
coordinates are accurate to .01 degree.  Data points in U.S. state outlines
are about .05 degrees apart and the coordinates are accurate to .001 degree.
Both the spacing and the accuracy of the international boundaries falls
somewhere between these two extremes.
.P
NCAR's DICOMED D48, for which the package has been tuned, has 15-bit
coordinate
registers, but an effective resolution of at most 1 in 4096 in both x and y.
.H 3 "\s10TIMING\s11"
The time required to draw a single map is highly variable and depends on how
certain parameters are set.
.P
The March 1985, update made EZMAP run significantly slower.  This was
principally because the default resolution was increased from a value
suitable for the dd80 to a value suitable for the DICOMED.  Users who are
concerned about this may increase the values of the parameters 'MV'
and/or 'DD' (see the description of MAPSTx) to improve the speed (at the
expense of plot quality, of course).
.H 3 "\s10REFERENCES\s11"
.P
Hershey, A.V., "The Plotting of Maps on a CRT Printer."  \fINWL Report\fR no. 1844,
1963.
.P
Lee, Tso-Hwa, "Students' Summary Reports, Work-Study Program in Scientific
Computing."  NCAR, 1968.
.P
Parker, R.L., "2UCSD SUPERMAP:  World Plotting Package."
.P
Steers, J.A., "An Introduction to the Study of Map Projections."  University
of London Press, 1962.
.sp
.hD "ROUTINES"
.sp
.sp -1
This section describes, in alphabetical order, all of the user-callable
routines in the EZMAP package.
.hD "MAPDRW"
.H 3 "Purpose"
Draws the complete map described by the current values of the EZMAP
parameters.
.H 3 "Usage"
Just call it.  MAPDRW calls MAPINT (if required), MAPGRD, MAPLBL, and MAPLOT,
in that order.  The user may wish to call these routines directly.
.H 3 "Arguments"
None.
.bp
.hD "MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)"
.H 3 "Purpose"
To examine each outline-dataset segment.
.H 3 "Usage"
CALL MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
.sp
MAPEOD is called by EZMAP to examine each segment in an outline dataset just
before it is plotted.
The default version does nothing.  A user-supplied version may cause selected
segments to be deleted (to reduce the clutter in northern Canada, for
example).  See examples 3, 5, and 9.
.H 3 "Arguments"
INTEGER NOUT,NSEG,IDLS,IDRS,NPTS; REAL PNTS(NPTS).
.sp
\fBNOUT\fR is the number of the outline dataset from which the segment 
comes, as follows:
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 80 0
.nr 38 \wNOUT
.if \n(80<\n(38 .nr 80 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 38 \wDataset to which segment belongs-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w'CO' - Continental outlines only.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w'US' - U.S. state outlines only.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w'PS' - Continental, U.S state, and international outlines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w'PO' - Continental and international outlines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.B
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NOUT\h'|\n(41u'Dataset to which segment belongs
.R
.sp
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u''CO' - Continental outlines only.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u''US' - U.S. state outlines only.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u''PS' - Continental, U.S state, and international outlines.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u''PO' - Continental and international outlines.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
\fBNSEG\fR is the number of the segment within the outline dataset.
The maps in example 9 show segment numbers for the outline
dataset 'CO'; the program may be modified to produce maps showing
segment numbers for any outline dataset.
.sp
\fBIDLS\fR and \fBIDRS\fR
are identifiers for the areas to the left and right, respectively, of the
segment.  (Left and right are defined from the standpoint of a viewer
standing at point 1 of the segment and looking toward point 2.)~
For a complete list of area identifiers, see the
section entitled "AREA IDENTIFIERS".
.P
.bF NPTS ,
on entry, is the number of points defining the outline segment.  NPTS may be
zeroed by MAPEOD to suppress plotting of the segment on the map.
.P
.bF PNTS
is an array of coordinates.  PNTS(1) and PNTS(2) are the latitude and
longitude of the first point, PNTS(3) and PNTS(4) the latitude and longitude
of the second point, . . . PNTS(NPTS-1) and PNTS(NPTS) the latitude and
longitude of the last point.  All values are in degrees.  Longitudes are all
between -180 and +180; no segment crosses the meridian at -180 (+180)
degrees.
.hD "MAPFST (RLAT,RLON)"
.H 3 "Purpose"
Draws lines on the map produced by a call to MAPDRW - used in conjunction
with MAPVEC.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPFST (RLAT,RLON)
.<<
.DE
is equivalent to the statement
.DS
.>>
CALL MAPIT (RLAT,RLON,0)
.<<
.DE
See the description of MAPIT.
.H 3 "Arguments"
REAL RLAT,RLON.
.P
.bF RLAT
and
.bF RLON
are defined as for MAPIT (see MAPIT).
.hD "MAPGRD"
.H 3 "Purpose"
Draws a grid.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPGRD
.<<
.DE
draws a grid consisting of lines of latitude (parallels) and lines of
longitude (meridians).  If EZMAP needs initialization or if the error
flag 'ER' is non-zero or if the parameter 'GR' is less than or equal to
zero, MAPGRD does nothing.
.H 3 "Arguments"
None.
.hD "MAPGTx (WHCH,xVAL)"
.H 3 "Purpose"
Gets the current value of a specified EZMAP parameter.
.H 3 "Usage"
Use one of the four statements
.DS
.>>
CALL MAPGTC (WHCH,CVAL)
CALL MAPGTI (WHCH,IVAL)
CALL MAPGTL (WHCH,LVAL)
CALL MAPGTR (WHCH,RVAL)
.<<
.DE
depending on whether the value to be retrieved is of type character, integer,
logical, or real.  The values of some parameters may be retrieved in more
than one way.  For example, the value of the initialization flag may be
retrieved as a logical quantity (.TRUE. or .FALSE.) or as an integer
(non-zero or zero).
.H 3 "Arguments"
CHARACTER*2 WHCH,CVAL; INTEGER IVAL; LOGICAL LVAL; REAL RVAL.
.sp
.bF WHCH
is a character string specifying which parameter to get.  Only the first two
characters of the string are examined.  Possible values are listed in the
table below and in the table which is part of the description of MAPSTx.
.P
.bF xVAL
(CVAL, IVAL, LVAL, or RVAL) receives the value of the parameter specified
by WHCH - of type character, integer, logical, or real, respectively.
.P
All of the parameters listed in the discussion of MAPSTx may be retrieved.
The following may also be retrieved:
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 34 \n(.lu
.eo
.am 82
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The value of the map limits specifier JLTS
from the last call to MAPSET.  The default
value is 'MA'.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 82
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The current value of the error flag.  The
default value is zero (no error).
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 82
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Initialization flag.  If true (non-zero), it
says that EZMAP is in need of initialization
(by a CALL MAPINT).  The default value is
true (non-zero).
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 82
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The value of the projection specifier JPRJ
from the last call to MAPROJ.  The default
value is \&'CE'.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 82
.br
.di e+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The value of PLON from the last call to
MAPROJ.  The default value is zero.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 82
.br
.di f+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The value of PLAT from the last call to
MAPROJ.  The default value is zero.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.eo
.am 82
.br
.di g+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
"n" is an integer from 1 to 8, inclusive.
Retrieves values from the call to MAPSET.
P1 through P4 specify PLM1(1), PLM2(1),
PLM3(1), and PLM4(1), while P5 through P8
specify PLM1(2), PLM2(2), PLM3(2), and
PLM4(2).  Default values are all zero.
.br
.di
.nr g| \n(dn
.nr g- \n(dl
..
.ec \
.eo
.am 82
.br
.di h+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The value of ROTA from the last call to
MAPROJ.  The default value is zero.
.br
.di
.nr h| \n(dn
.nr h- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wWHCH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wARea
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wERror
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wINitialize
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPRojection
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPN
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPn
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wROtation
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wXLeft
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wXRight
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wYBottom
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wYTop
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 38 \wMeaning-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wThe parameters XLOW, XROW, YBOW, and YTOW-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wfrom the last call to MAPPOS.  Defaults-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \ware .05, .95, .05, and .95, respectively.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'WHCH\h'|\n(41u'Type\h'|\n(42u'Meaning
.R
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ARea\h'|\n(41u'C\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ERror\h'|\n(41u'I\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'INitialize\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PRojection\h'|\n(41u'C\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PN\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PT\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(g|u+\n(.Vu
.if (\n(g|+\n(#^-1v)>\n(#- .nr #- +(\n(g|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Pn\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.g+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(h|u+\n(.Vu
.if (\n(h|+\n(#^-1v)>\n(#- .nr #- +(\n(h|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ROtation\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.h+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'XLeft\h'|\n(41u'R\h'|\n(42u'The parameters XLOW, XROW, YBOW, and YTOW
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'XRight\h'|\n(41u'R\h'|\n(42u'from the last call to MAPPOS.  Defaults
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'YBottom\h'|\n(41u'R\h'|\n(42u'are .05, .95, .05, and .95, respectively.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'YTop\h'|\n(41u'R\h'|\n(42u'
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.rm g+
.rm h+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-60
.hD "MAPINT"
.H 3 "Purpose"
Initialization.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPINT
.<<
.DE
initializes the EZMAP package.  This is required initially and again after
a call to any of the routines MAPPOS, MAPROJ, or MAPSET.  The flag 'IN',
which may be retrieved by a call to MAPGTI or MAPGTL, indicates whether or not
initialization is required at a given time.  As of now (April, 1987), no
parameter change by means of a call to MAPSTx forces re-initialization;
only calls to MAPPOS, MAPROJ, and MAPSET do.
.H 3 "Arguments"
None.
.hD "MAPIQ"
.H 3 "Purpose"
Terminate a string of calls to MAPIT.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPIQ
.<<
.DE
flushes MAPIT's buffers.  It is particularly important that this be done
before a STOP or a CALL FRAME and before changing the color index,
dash pattern, etc.
.H 3 "Arguments"
None.
.hD "MAPIT (RLAT, RLON, IFST)"
.H 3 "Purpose"
Draws lines on a map.
.H 3 "Usage"
.DS
.>>
CALL MAPIT (RLAT,RLON,IFST)
.sp
.<<
.DE
MAPIT is used to draw lines on the map, both by EZMAP itself and, if desired,
by the user.  MAPIT attempts to omit non-visible portions of lines and to
handle "cross-over" - a jump from one end of the map to the other caused by
the projection's having slit the globe along some half of a great circle and
laid it open with the two sides of the slit at opposite ends of the map.
Cross-over can occur on cylindrical and conical projections; MAPIT handles it
gracefully on the former and not so well on the latter.
.P
The EZMAP parameter 'DL' determines whether MAPIT draws solid lines or dotted
lines.  Dotted lines are drawn using calls to POINTS.  Solid lines are drawn
using calls to DASHD, FRSTD, and VECTD.
.P
The parameters 'DD' and 'MV' also affect MAPIT's behavior.  See the
description of these parameters in the description of the routine MAPSTx.
.P
A sequence of calls to MAPIT should be followed by a call to MAPIQ (see
MAPIQ above) to flush its buffers before a STOP, a CALL FRAME, or a call to change
the color index.
.P
Points in two contiguous pen-down calls to MAPIT should not be far apart on
the globe.
.H 3 "Arguments"
REAL RLAT,RLON.
.P
.bF RLAT
and
.bF RLON
specify the latitude and longitude of a point to which the "pen" is to be
moved.  Both are given in degrees.  RLAT must be between -90. and +90.,
inclusive; RLON must be between -540. and +540., inclusive.
.P
.bF IFST
is 0 to do a "pen-up" move, 1 to do a "pen-down" move only if the distance
from the last point to the new point is greater than 'MV' plotter units, and
2 or greater to do a "pen-down" move regardless of the distance from the last
point to the new one.
.hD "MAPLBL"
.H 3 "Purpose"
Labels the map.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPLBL
.<<
.DE
if the parameter 'LA' is set appropriately,
labels the international date line (ID), the equator (EQ), the Greenwich
Meridian (GM), and the poles (NP and SP), and, if the parameter 'PE' is set
appropriately, draws the perimeter of the map.
If EZMAP needs initialization or if the error flag 'ER' is set, MAPLBL does
nothing.
.H 3 "Arguments"
None.
.hD "MAPLOT"
.H 3 "Purpose"
Draws geographical outlines.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPLOT
.<<
.DE
.bp
draws the continental and/or international and/or U.S. state outlines selected
by the EZMAP parameter 'OU'; the parameter 'DO' determines whether solid lines
or dotted lines are used.  If EZMAP currently needs initialization or if the
error flag 'ER' is set, MAPLOT does nothing.
.H 3 "Arguments"
None.
.hD "MAPPOS (XLOW,XROW,YBOW,YTOW)"
.H 3 "Purpose"
Positions the map on the plotter frame.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPPOS (XLOW,XROW,YBOW,YTOW)
.<<
.DE
sets four EZMAP parameters specifying the position of a window in the plotter
frame within which maps are to be drawn.  Each of the arguments is between
0. and 1., inclusive, and specifies the position of one edge of the window,
as a fraction of the distance from left to right, or from bottom to top,
across the window.  The map is centered within the window and made as large
as possible, but maintains its intrinsic shape (aspect ratio).
.H 3 "Arguments"
REAL XLOW,XROW,YBOW,YTOW.
.P
.bF XLOW
specifies the position of the left edge of the window.  The default is .05.
.P
.bF XROW
specifies the position of the right edge of the window.  The default is .95.
.P
.bF YBOW
specifies the position of the bottom edge of the window.  The default is .05.
.P
.bF YTOW
specifies the position of the top edge of the window.  The default is .95.
.hD "MAPROJ (JPRJ,PLAT,PLON,ROTA)"
.H 3 "Purpose"
Set the projection to be used.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPROJ (JPRJ,PLAT,PLON,ROTA)
.<<
.DE
sets EZMAP parameters specifying the projection to be used for subsequent
maps.
.H 3 "Arguments"
CHARACTER*2 JPRJ; REAL PLAT,PLON,ROTA.
.P
.bF JPRJ
is a character variable defining the desired projection type, as follows:
.DS
~~~The conic projection:
.sp
~~~~~~'LC' - Lambert conformal conic with two standard parallels.
.sp
~~~~~~The azimuthal projections:
.sp
~~~~~~~~~'ST' - Stereographic.
.sp
~~~~~~~~~'OR' - Orthographic.  The EZMAP parameter 'SA' will be
          zeroed.  See the note below.
.sp
~~~~~~~~~'LE' - Lambert equal area.
.sp
~~~~~~~~~'GN' - Gnomonic.
.sp
~~~~~~~~~'AE' - Azimuthal equidistant.
.sp
~~~~~~~~~'SV' - Satellite-view.  If the EZMAP parameter 'SA' is less
          than or equal to 1., it will be reset to 6.631 (the value for a 
	  satellite in a geosynchronous orbit).  See the note below.
.sp
~~~The cylindrical projections:
.sp
~~~~~~'CE' - Cylindrical equidistant.
.sp
~~~~~~'ME' - Mercator.
.sp
~~~~~~'MO' - Mollweide-type.
.DE
Note:  The orthographic and satellite-view projections have the same internal
identifier.  The EZMAP parameter 'SA' determines which will be used.  If a
call to MAPROJ selecting one or the other is followed by a call to MAPSTR
resetting 'SA', it may have the effect of causing the other to be used.  See
the description of 'SA', below, in the description of the routine MAPSTx.
.P
.bF PLAT ,
.bF PLON ,
and
.bF ROTA
are angular quantities, in degrees.  How they are used depends on the value
of JPRJ, as follows:
.BL
.LI
If JPRJ is not equal to 'LC':  PLAT and PLON define the latitude and longitude
of the pole of the projection - the point on the globe which is to project to
the origin of the u/v plane.  PLAT must be between -90. and +90., inclusive,
positive in the northern hemisphere, negative in the southern.  PLON must be
between -180. and +180., inclusive, positive to the east, and negative to the
west, of Greenwich.  ROTA is the angle between the v axis and north at the
origin.  It is taken to be positive if the angular movement from north to the
v axis is counter-clockwise, negative otherwise.  If the origin is at the
north pole, "north" is considered to be in the direction of PLON+180.
If the origin is at the south pole, "north" is considered to be in the
direction of PLON.  For the cylindrical projections, the axis of the
projection is parallel to the v axis.
.LI
If JPRJ is equal to 'LC':  PLON defines the central meridian
of the projection, and PLAT and ROTA define the two standard
parallels.  If PLAT and ROTA are equal, a simpler conic
projection, with one standard parallel, is used.
.LE
.P
For more detailed descriptions of the projections, see the section
called "PROJECTIONS".
.hD "MAPRS"
.H 3 "Purpose"
Recalls SET.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPRS
.<<
.DE
repeats the SET call last done by the routine MAPINT.  This might be used
when user lines are to be plotted over a map generated in a different overlay
(e.g., using a flash buffer), and when the system plot package does not
reside in an outer overlay.
.H 3 "Arguments"
None.
.hD "MAPRST (IFNO)"
.H 3 "Purpose"
Restores the state of EZMAP saved by an earlier call to MAPSAV.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPRST (IFNO)
.<<
.DE
restores EZMAP to a previously saved state (frequently the default state) by
reading, from the unit specified by IFNO, values of all user-settable
parameters, and then executing MAPINT.
.H 3 "Arguments"
INTEGER IFNO.
.P
.bF IFNO
is the number of a unit from which a single unformatted record is to be read.
It is the user's responsibility to position this unit.  MAPRST does not rewind
it, either before or after reading the record.
.bp
.hD "MAPSAV (IFNO)"
.H 3 "Purpose"
Saves the current state of EZMAP for later restoration by MAPRST.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPSAV (IFNO)
.<<
.DE
saves the current state of EZMAP (frequently the default state) by writing,
on the unit specified by IFNO, the current values of all the user-settable
parameters.
.H 3 "Arguments"
INTEGER IFNO.
.P
.bF IFNO
is the number of a unit to which a single unformatted record is to be written.
It is the user's responsibility to position this unit.  MAPSAV does not rewind
it, either before or after writing the record.
.hD "MAPSET (JLTS,PLM1,PLM2,PLM3,PLM4)"
.H 3 "Purpose"
To specify the rectangular portion of the u/v plane to be drawn.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPSET (JLTS,PLM1,PLM2,PLM3,PLM4)
.<<
.DE
specifies what portion of the u/v plane is to be plotted.
.H 3 "Arguments"
CHARACTER*2 JLTS; REAL PLM1(2),PLM2(2),PLM3(2),PLM4(2).
.P
.bF JLTS
is a character string specifying how the limits of the map are to be chosen.
There are five possibilities, as follows:
.BL
.LI
JLTS='MA' (MAXIMUM).  The maximum useful area produced by the projection is
plotted.  PLM1, PLM2, PLM3, and PLM4 are not used.
.LI
JLTS='CO' (CORNERS).  The points (PLM1,PLM2) and (PLM3,PLM4) are to be at
opposite corners of the map.  PLM1 and PLM3 are latitudes, in degrees.  PLM2
and PLM4 are longitudes, in degrees.  If a cylindrical projection is being
used, the first point should be on the left edge of the map and the second
point on the right edge; otherwise, the order makes no difference.
.LI
JLTS='PO' (POINTS).  PLM1, PLM2, PLM3, and PLM4 are two-element arrays giving
the latitudes and longitudes, in degrees, of four points which are to be on
the edges of the rectangular map.  If a cylindrical projection is being
used, the first point should be on the left edge and the second point on the
right edge; otherwise, the order makes no difference.
.LI
JLTS='AN' (ANGLES).  PLM1, PLM2, PLM3, and PLM4 are positive angles, in
degrees, representing angular distances from a point on the map to the
left, right, bottom, and top edges of the map, respectively.  For most
projections, these angles are measured with the center of the earth at
the vertex and represent angular distances from the point which projects
to the origin of the u/v plane; on a satellite-view projection, they are
measured with the satellite at the vertex and represent angular
deviations from the line of sight.  Angular limits are particularly
useful for polar projections and for the satellite-view projection; they
are not appropriate for the Lambert conformal conic and an error will
result if one attempts to use JLTS='AN' with JPRJ='LC'.
.LI
JLTS='LI' (LIMITS).  PLM1, PLM2, PLM3, and PLM4 specify the minimum value of
u, the maximum value of u, the minimum value of v, and the maximum value of v,
respectively.  Knowledge of the projection equations is necessary in order to
use this option correctly.
.LE
.hD "MAPSTx (WHCH,xVAL)"
.H 3 "Purpose"
To set the values of certain EZMAP parameters.
.H 3 "Usage"
Use one of the four statements
.DS
.>>
CALL MAPSTC (WHCH,CVAL)
CALL MAPSTI (WHCH,IVAL)
CALL MAPSTL (WHCH,LVAL)
CALL MAPSTR (WHCH,RVAL)
.<<
.DE
depending on whether the value to be set is of type CHARACTER, INTEGER,
LOGICAL, or REAL.  Some parameters may be set in more than one way.  For
example, the parameter 'GR', which specifies the grid spacing, may be
given the value 10.0 in either of two ways:
.DS
.>>
CALL MAPSTI ('GR',10)
CALL MAPSTR ('GR',10.)
.<<
.DE
The flag which controls dotting of outlines may be turned on using either of
these calls:
.DS
.>>
CALL MAPSTI ('DO',1)
CALL MAPSTL ('DO',.TRUE.)
.<<
.DE
The important point to remember is that the last character of the routine
name implies the type of its second argument.
.H 3 "Arguments"
CHARACTER*2 CVAL; INTEGER IVAL; LOGICAL LVAL; REAL RVAL.
.P
.bF WHCH
is a character string specifying the parameter to be set.  Only the first two
characters of this string are examined.  See the table below.
.P
.bF xVAL
(CVAL, IVAL, LVAL, or RVAL) contains the value to be given to the parameter
specified by WHCH - of type character, integer, logical, or real,
respectively.
.P
Following is a list of all the parameters which may be set in this way.
.sp2
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 34 \n(.lu
.eo
.am 82
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
"n" is an integer between 1 and 7.  Each
"Cn" specifies the color index of some part
of the map.  The user must do the calls
to GKS routines or to SPPS routines to
define the color indices.  All default
values are zero, indicating no change in
color index from one part of the map to
another.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 82
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Dashed-line pattern for the grids.  A 16-bit
quantity.  The default is 21845 (octal 52525
or binary 0101010101010101).
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 82
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Distance between dots along a dotted line
drawn by MAPIT.  The default value is 12
(out of 4096. See 'RE', below).
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 82
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If true (non-zero), user calls to MAPIT draw
dotted lines.  The default is false (zero).
Lines drawn by MAPIT are solid or dashed,
depending on the current state of the
DASHCHAR package.  'DL' may be reset by a
user version of MAPUSR to change the way
in which the perimeter, the grid, the limb
lines, and the outlines are drawn.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 82
.br
.di e+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If true (non-zero), outlines are dotted.
The default is false (zero). Outlines are
solid.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 82
.br
.di f+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If true (non-zero), only that part of the
map which falls inside an ellipse inscribed
within the normal rectangular perimeter is
drawn.  This is particularly appropriate for
use with azimuthal projections and angular
limits specifying a square, in which case
the ellipse becomes a circle, but it will
work for any map.  The default value is
false (zero).
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.eo
.am 82
.br
.di g+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The distance between points used to draw the
grid, in degrees.  The default value is 1.
User values must fall between .001 and 10.
.br
.di
.nr g| \n(dn
.nr g- \n(dl
..
.ec \
.eo
.am 82
.br
.di h+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The desired grid spacing.  A zero suppresses
the grid.  The default is 10 degrees.
.br
.di
.nr h| \n(dn
.nr h- \n(dl
..
.ec \
.eo
.am 82
.br
.di i+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If true (non-zero), label the meridians and
the poles.  The default is true (non-zero).
.br
.di
.nr i| \n(dn
.nr i- \n(dl
..
.ec \
.eo
.am 82
.br
.di j+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Controls label size.  A character width, to
be used in a call to PWRIT.  The default
value is 1, which gives a character width
of 12 plotter units.
.br
.di
.nr j| \n(dn
.nr j- \n(dl
..
.ec \
.eo
.am 82
.br
.di k+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Minimum vector length for MAPIT.  A point
closer to the previous point than this is
omitted.  The default value is 4
(out of 4096. See \&'RE', below).
.br
.di
.nr k| \n(dn
.nr k- \n(dl
..
.ec \
.eo
.am 82
.br
.di l+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Says which set of outline data to use.  The
possible values are 'NO', for no outlines,
\&'CO', for continental outlines, 'US', for
U.S. state outlines, 'PS', for continental
outlines plus international outlines plus
U.S. outlines, and 'PO', for continental
outlines plus international outlines.
Default is 'CO'.
.br
.di
.nr l| \n(dn
.nr l- \n(dl
..
.ec \
.eo
.am 82
.br
.di m+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If true (non-zero), draw the perimeter.  The
default is true (non-zero).
.br
.di
.nr m| \n(dn
.nr m- \n(dl
..
.ec \
.eo
.am 82
.br
.di n+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The width of the target plotter, in plotter
units.  The default value is 4096.
.br
.di
.nr n| \n(dn
.nr n- \n(dl
..
.ec \
.eo
.am 82
.br
.di o+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
If 'SA' is greater than 1., a satellite-view
projection replaces the orthographic.  The
value is the distance of the satellite from
the center of the earth, in multiples of the
earth's radius.  The default value is zero.
See also 'S1' and \&'S2', below.
.br
.di
.nr o| \n(dn
.nr o- \n(dl
..
.ec \
.eo
.am 82
.br
.di p+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Used only when 'SA' is greater than 1.  Both
are angles, in degrees.  'S1' measures the
angle between the line to the center of the
earth and the line of sight (to which the
projection plane is perpendicular).  If 'S1'
is zero, the projection shows the earth as
seen by a satellite looking straight down.
Call this the "basic view".  If \&'S1' is
non-zero, \&'S2' measures the angle from the
positive u axis of the basic view to the
line OP, where O is the origin of the basic
view and P is the projection of the desired
line of sight on the basic view.  \&'S2' is
positive if measured counterclockwise.
.br
.di
.nr p| \n(dn
.nr p- \n(dl
..
.ec \
.eo
.am 82
.br
.di q+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
A search radius, in degrees, used by MAPINT
in finding the latitude/longitude range of a
map.  The default value is 1. User values
must lie between .001 and 10.  Should not be
changed except by advice of a consultant.
.br
.di
.nr q| \n(dn
.nr q- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wWHCH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCn
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDAshpattern
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDD
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDOt
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wWHCH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wELliptical
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGD
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGRid
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLAbel
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMV
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wOUtline
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPErim
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wREsolution
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSAtellite
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wWHCH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wS1 and S2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,L
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wI,R
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 38 \wMeaning-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wn~~~~Use-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w1~~~~perimeter-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w2~~~~grid-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w3~~~~labels-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w4~~~~limbs-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w5~~~~continents-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w6~~~~U.S. states-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w7~~~~countries-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wMeaning-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wMeaning-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'WHCH\h'|\n(41u'Type\h'|\n(42u'Meaning
.R
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Cn\h'|\n(41u'I\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'n~~~~Use
.R
.sp
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'1~~~~perimeter
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'2~~~~grid
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'3~~~~labels
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'4~~~~limbs
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'5~~~~continents
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'6~~~~U.S. states
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'7~~~~countries
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DAshpattern\h'|\n(41u'I\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DD\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DL\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DOt\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.bp
.T&
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'WHCH\h'|\n(41u'Type\h'|\n(42u'Meaning
.R
.sp
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ELliptical\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(g|u+\n(.Vu
.if (\n(g|+\n(#^-1v)>\n(#- .nr #- +(\n(g|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GD\h'|\n(41u'R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.g+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(h|u+\n(.Vu
.if (\n(h|+\n(#^-1v)>\n(#- .nr #- +(\n(h|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GRid\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.h+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(i|u+\n(.Vu
.if (\n(i|+\n(#^-1v)>\n(#- .nr #- +(\n(i|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LAbel\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.i+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(j|u+\n(.Vu
.if (\n(j|+\n(#^-1v)>\n(#- .nr #- +(\n(j|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LS\h'|\n(41u'I\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.j+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(k|u+\n(.Vu
.if (\n(k|+\n(#^-1v)>\n(#- .nr #- +(\n(k|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MV\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.k+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(l|u+\n(.Vu
.if (\n(l|+\n(#^-1v)>\n(#- .nr #- +(\n(l|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'OUtline\h'|\n(41u'C\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.l+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(m|u+\n(.Vu
.if (\n(m|+\n(#^-1v)>\n(#- .nr #- +(\n(m|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PErim\h'|\n(41u'I,L\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.m+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(n|u+\n(.Vu
.if (\n(n|+\n(#^-1v)>\n(#- .nr #- +(\n(n|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'REsolution\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.n+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(o|u+\n(.Vu
.if (\n(o|+\n(#^-1v)>\n(#- .nr #- +(\n(o|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SAtellite\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.o+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.bp
.T&
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'WHCH\h'|\n(41u'Type\h'|\n(42u'Meaning
.R
.sp
.sp
.ne \n(p|u+\n(.Vu
.if (\n(p|+\n(#^-1v)>\n(#- .nr #- +(\n(p|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'S1 and S2\h'|\n(41u'I,R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.p+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(q|u+\n(.Vu
.if (\n(q|+\n(#^-1v)>\n(#- .nr #- +(\n(q|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SR\h'|\n(41u'R\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.q+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.rm g+
.rm h+
.rm i+
.rm j+
.rm k+
.rm l+
.rm m+
.rm n+
.rm o+
.rm p+
.rm q+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-171
.sp
.hD "MAPTRN (RLAT,RLON,UVAL,VVAL)"
.H 3 "Purpose"
To project points.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPTRN (RLAT,RLON,UVAL,VVAL)
.<<
.DE
is used to find the projection in the u/v plane of a point whose latitude and
longitude are known.  MAPTRN may be called at any time after EZMAP has been
initialized (by calling MAPINT or otherwise).
.H 3 "Arguments"
REAL RLAT,RLON,UVAL,VVAL.
.P
.bF RLAT
and
.bF RLON
are the latitude and longitude, respectively, of a point on the globe.  RLAT
must be between -90. and +90., inclusive; RLON must be between -540.
and +540., inclusive.
.P
.bF (UVAL,VVAL)
is the projection in the u/v plane of (RLAT,RLON).  The units of UVAL and VVAL
depend on the projection.
.P
If the point is not projectable, UVAL is returned equal to 1.E12.  Note that,
if the point is projectable, but outside the boundary of the map, as defined
by the last call to MAPSET, its u and v coordinates are still returned by
MAPTRN.  The user must do the test required to determine if the point is
within limits, if that is necessary.
.hD "MAPUSR (IPRT)"
.H 3 "Purpose"
To change the appearance of the various parts of a map.
.H 3 "Usage"
EZMAP executes the statement
.DS
.>>
CALL MAPUSR (IPRT)
.<<
.DE
just before and just after each portion of a map is drawn.  The default
version of MAPUSR does nothing.
.P
A user-supplied version of MAPUSR may set/reset the dotting parameter 'DL',
the DASHCHAR dash pattern, the color index, etc., so as to achieve
a desired effect.
.H 3 "Arguments"
INTEGER IPRT.
.P
.bF IPRT ,
if positive, says that a particular part of the map is about to be drawn, as
follows:
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 80 0
.nr 38 \wIPRT
.if \n(80<\n(38 .nr 80 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 38 \wPart-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wPerimeter.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wGrid.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wLabels.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wLimb lines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wContinental outlines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wU.S. state outlines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wInternational outlines.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.B
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IPRT\h'|\n(41u'Part
.R
.sp
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'Perimeter.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'Grid.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'Labels.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'Limb lines.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'Continental outlines.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'U.S. state outlines.
.ta \n(60u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'International outlines.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.sp
If IPRT is negative, it says that drawing of the last part is complete.  The
absolute value of IPRT will be one of the above values.  Changed quantities
should be restored.
.hD "MAPVEC (RLAT,RLON)"
.H 3 "Purpose"
Draws lines on the map produced by a call to MAPDRW - used in conjunction
with MAPFST.
.H 3 "Usage"
The statement
.DS
.>>
CALL MAPVEC (RLAT,RLON)
.<<
.DE
is equivalent to the statement
.DS
.>>
CALL MAPIT (RLAT,RLON,1)
.<<
.DE
See the description of MAPIT.
.H 3 "Arguments"
REAL RLAT,RLON.
.P
.bF RLAT
and
.bF RLON
are defined as for MAPIT (see MAPIT above).
.hD "SUPCON (RLAT,RLON,UVAL,VVAL)"
.H 3 "Purpose"
An old equivalent of the new routine MAPTRN.  Provided for compatibility with
earlier versions of EZMAP/SUPMAP.  If efficiency is a consideration, bypass
this routine and call MAPTRN directly.
.H 3 "Usage"
The statement
.DS
.>>
CALL SUPCON (RLAT,RLON,UVAL,VVAL)
.<<
.DE
is exactly equivalent to the statement
.DS
.>>
CALL MAPTRN (RLAT,RLON,UVAL,VVAL)
.<<
.DE
.H 3 "Arguments"
REAL RLAT,RLON,UVAL,VVAL.
.P
.bF RLAT ,
.bF RLON ,
.bF UVAL ,
and
.bF VVAL
are defined as for MAPTRN (see MAPTRN above).
.hD "SUPMAP (. . .)"
.H 3 "Purpose"
An implementation of the routine from which EZMAP grew.
.H 3 "Usage"
The statement
.DS
.>>
~CALL SUPMAP (JPRJ,PLAT,PLON,ROTA,PLM1,PLM2,PLM3,PLM4,
+                     JLTS,JGRD,IOUT,IDOT,IERR)
.<<
.DE
creates a map of a desired portion of the globe, according to a desired
projection, with desired outlines drawn in, and with lines of latitude
and longitude at desired intervals.  An appropriate call to the routine SET is
performed, and the routine SUPCON (see SUPSON above) is initialized so that the user
may map points of known latitude and longitude to points in the u/v plane and
use the u/v coordinates to draw objects on the map produced by SUPMAP.
.H 3 "Arguments"
.nf
INTEGER JPRJ; REAL PLAT,PLON,ROTA,PLM1(2),PLM2(2),PLM3(2),PLM4(2);
INTEGER JLTS,JGRD,IOUT,IDOT,IERR.
.fi
.P
.bF IABS(JPRJ)
defines the projection type, as follows (values less than 1 or greater than
10 are treated as 1 or 10, respectively):
.DS
.>>
 1  Stereographic.
 2  Orthographic.
 3  Lambert conformal conic.
 4  Lambert equal area.
 5  Gnomonic.
 6  Azimuthal equidistant.
 7  Satellite view.
 8  Cylindrical equidistant.
 9  Mercator.
10  Mollweide-type.
.<<
.DE
Using the value 2 causes the EZMAP parameter 'SA' to be zeroed.  ('SA', if
greater than 1., says that a satellite-view projection, rather than an
orthographic projection, is to be used, and specifies the distance of the
satellite from the center of the earth, in units of earth radii.)  Using the
value 7 causes 'SA' to be examined.  If it has a non-zero value, the value is
left alone.  If it has a zero value, its value is reset to 6.631, which is
about right for a satellite in a geosynchronous equatorial orbit.
.P
The sign of JPRJ, when IOUT is -1, 0, or +1, indicates whether the continental
outlines are to be plotted or not.  See IOUT, below.
.P
.bF PLAT ,
.bF PLON ,
and
.bF ROTA
define the origin of the projection and its rotation angle and are used in
the same way as they would be in a call to the routine MAPROJ (see
MAPROJ above).
.P
.bF JLTS ,
.bF PLM1 ,
.bF PLM2 ,
.bF PLM3 ,
and
.bF PLM4
specify the rectangular limits of the map.  These arguments are used in the
same way as they would be in a call to MAPSET (see MAPSET
above), except that JLTS
is an integer instead of a character string.  IABS(JLTS) may take on the
values 1 through 5, as follows:
.DS
.>>
1  Like JLTS='MA' in a call to MAPSET.
2  Like JLTS='CO' in a call to MAPSET.
3  Like JLTS='LI' in a call to MAPSET.
4  Like JLTS='AN' in a call to MAPSET.
5  Like JLTS='PO' in a call to MAPSET.
.<<
.DE
At one time, the sign of JLTS specified whether or not a line of text was to
be written at the bottom of the plot produced.  This line may no longer be
written and the sign of JLTS is therefore ignored.
.P
.bF MOD(IABS(JGRD),1000)
is the value, in degrees, of the interval at which lines of latitude and
longitude are to be plotted.  If the given interval is zero, grid lines and
labels are not plotted.  If JGRD is less than zero, the perimeter is not
plotted.  Set JGRD to -1000 to suppress both grid lines and perimeter and to
+1000 to suppress the grid lines, but leave the perimeter.  The value -0 may
have a meaning on ones' complement machines, but should be avoided; use -1000
instead.
.P
.bF IOUT
has the value 0 to suppress U.S. state outlines,
and the value -1 or +1 to plot U.S. state
outlines.  In both of these cases, the sign of JPRJ indicates whether
continental outlines are to be plotted (JPRJ positive) or not (JPRJ negative).
Originally, SUPMAP recognized only these values of IOUT; now, if IOUT is less
than 0 or greater than 1, the sign of JPRJ is ignored, and IOUT selects an
outline group, as follows:
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 80 0
.nr 31 0
.nr 32 0
.nr 38 \w-2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w or less
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w 2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w 3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w 4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w 5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w or greater
.if \n(32<\n(38 .nr 32 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.nr 38 \wNO' (no outlines).
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCO' (continental outlines).
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUS' (U.S. state outlines).
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPS' (continental outlines plus international
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \woutlines plus U.S. state outlines).
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPO' (continental outlines plus international
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \woutlines).
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(60u \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-2 or less\h'|\n(41u'NO' (no outlines).
.ta \n(60u \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u' 2\h'|\n(41u'CO' (continental outlines).
.ta \n(60u \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u' 3\h'|\n(41u'US' (U.S. state outlines).
.ta \n(60u \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u' 4\h'|\n(41u'PS' (continental outlines plus international
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'outlines plus U.S. state outlines).
.ta \n(60u \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u' 5 or greater\h'|\n(41u'PO' (continental outlines plus international
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'outlines).
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.sp
At one time, the sign of IOUT specified whether or not a line of text was to
be written on the print output.  This may no longer be done.
.P
.bF IDOT
is a 0 to get continuous outlines, a 1 to get dotted outlines.
.P
.bF IERR
is the only output parameter.  A non-zero value indicates that an error has
occurred.  The section "ERROR CONDITIONS" lists the possible values.
