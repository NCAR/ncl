.\"
.\"	$Id: ezmapa.m,v 1.1.1.1 1992-04-17 22:30:22 ncargd Exp $
.\"
.TH EZMAPA 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " EZMAPA - Allows EZMAP output redirection to AREAS routines
.dsS1 " CALL MAPINT initialize EZMAP
.dsS2 " ICI=MAPACI (IAI) gives ICI color index of area IAI
.dsS3 " CALL MAPBLA (IAM) adds boundary lines
.dsS4 " CALL MAPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR) \ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Draws grid masked by boundarys from MAPBLA
.dsS5 " CALL MAPITA (RLAT,RLON,IFST,IAM,IGP,IDL,IDR) add lines
.dsS6 " CALL MAPIQA (IAM,IGP,IDL,IDR) add lines
.dsS7 " CALL MAPITM (RLAT,RLON,IFST,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR) add lines
.dsS8 " CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,MAI,LPR) add lines
.nrsN 8
.tr ~
./" USE tsi to PRINT THIS FILE!
.pn 289
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9EZMAPA\s11@"
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
.B
.S 16
.S 11
.H 3 "Latest Revision"
May 1987
.H 3 "Purpose"
EZMAPA allows you to redirect EZMAP output to routines in the package AREAS.
Among other things,
this makes it possible to create solid-colored world maps.
.H 3 "Usage"
To use the routines in the package EZMAPA, you should be familiar with the
packages EZMAP and AREAS.
.sp
User-level routines in EZMAPA are as follows:
.BL
.LI
The subroutine MAPBLA adds boundary lines to an already existing area map.
The boundary lines are 
produced by projecting a selected EZMAP dataset according to a specified
EZMAP projection within a specified area.  The area map may then be used in
various ways (for example,
to produce a colored map).
(MAPBLA stands for \fImap boundary lines to area map\fR.)
.LI
The subroutine MAPGRM draws lines of longitude and latitude 
that are masked by the areas defined by the area map.
MAPGRM was used to draw the longitude and latitude lines over water and
omit them over land masses on the cover of this manual.
(MAPGRM stands for \fImap grid, masked\fR.)
.LI
The subroutines MAPITA and MAPIQA add the projections of 
arbitrary lines, defined by
sets of latitude/longitude pairs, to an area map.
(MAPITA stands for \fImap it, areas\fR.
MAPIQA stands for \fImap it, quit areas\fR.)
.LI
The subroutines MAPITM and MAPIQM draw the projections of
arbitrary lines, defined
by sets of latitude/longitude pairs, 
masked by the areas defined by the area map.
(MAPITM stands for \fImap it, masked\fR.
MAPIQM stands for \fImap it, quit masked\fR.)
.LI
The function MAPACI obtains, for a given area, a color index
appropriate for use in generating a colored map that has adjacent areas of
different colors.
(MAPACI stands for \fImap area color index\fR.)
.LE
.sp
See "Details of Routines" below for complete descriptions of each
routine.
.H 3 "\s10REQUIRED STEPS\s11"
You must perform the four steps listed here
before calling any of the other EZMAPA routines.
.H 3 "Step One"
First, execute the FORTRAN statement
.>>
.sf
CALL ARINAM (IAM,LAM)
.<<
.ef
.VL .6i
.LI "\fBIAM\fR"
An integer array in which an area map is being constructed.
.LI "\fBLAM\fR"
Length of the array IAM.
.LE
.sp
("ARINAM" stands for \fIinitialize area map\fR.)
For more information on ARINAM, see the AREAS documentation.
.H 3 "Step Two"
Next, call various EZMAP routines
(MAPPOS, MAPROJ, MAPSET, MAPSTC, MAPSTI, and MAPSTR) to define the position
of the map on the plotter frame, the projection you want, the EZMAP dataset
you are using,
and the limits of the map, just as if you were going to call
MAPDRW to draw a map.
.sp
For more information on these routines, see the EZMAP documentation.
.H 3 "Step Three"
Execute the statement
.>>
.sf
CALL MAPINT
.<<
.ef
to initialize EZMAP.
.sp
.hw MAPINT
("MAPINT" stands for \fImap initialize\fR.)
For more information on MAPINT, see the EZMAP documentation.
.H 3 "Step Four"
Execute the statement
.>>
.sf
CALL MAPBLA (IAM)
.<<
.ef
to retrieve, project, and add the selected EZMAP boundary
lines to the area map.
.VL .6i
.LI "\fBIAM\fR"
An integer array in which an area map is being constructed.
.sp
("MAPBLA" stands for \fImap boundary lines to area map\fR.)
For more information on MAPBLA, see the "Details of Routines" section below.
.LE
.H 2 "\s10POSSIBLE NEXT STEPS\s11"
After the above steps are completed, you can call routines in 
the package AREAS to do various
things with the area map that has been created.  For example, you can call
ARSCAM to draw a solid-colored world map.
See the "Example" below.
.sp
Also, you can now call the other routines in the package EZMAPA:
.BL
.LI
MAPGRM, to draw latitude and longitude lines that are masked by
the areas defined by the area map.
.LI
MAPITA and MAPIQA, to add arbitrary lines,
defined by sets of latitude/longitude pairs, to an area map.
.LI
MAPITM and MAPIQM, to draw arbitrary lines that are masked by
the areas defined by an area map.
The lines are defined by sets of latitude/longitude pairs.
.LE
.sp
These routines are described under "Details of Routines" below.
.H 3 "Entry Points"
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
.rm 80 81 82 83 84 85
.nr 80 0
.nr 38 \wMAPACI
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMAPITM
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wMAPBLA
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wMAPGRM
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMAPIQA
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wMAPIQM
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.nr 38 \wMAPITA
.if \n(85<\n(38 .nr 85 \n(38
.85
.rm 85
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
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr TW \n(85
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MAPACI\h'|\n(41u'MAPBLA\h'|\n(42u'MAPGRM\h'|\n(43u'MAPIQA\h'|\n(44u'MAPIQM\h'|\n(45u'MAPITA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MAPITM\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'\h'|\n(45u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-5
.H 3 "Common Blocks"
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
.nr 38 \wMAPCM1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMAPCMA
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wMAPCM2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMAPCMB
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wMAPCM3
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wMAPCMC
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMAPCM4
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wMAPSAT
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wMAPCM8
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MAPCM1\h'|\n(41u'MAPCM2\h'|\n(42u'MAPCM3\h'|\n(43u'MAPCM4\h'|\n(44u'MAPCM8
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MAPCMA\h'|\n(41u'MAPCMB\h'|\n(42u'MAPCMC\h'|\n(43u'MAPSAT\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-5
.H 3 "Required Routines"
All routines in the packages AREAS and EZMAP.
.H 3 "Required GKS"
Level 0A.
.H 3 "I/O"
None.
.H 3 "Precision"
Single.
.H 3 "Language"
FORTRAN 77.
.H 3 "History"
EZMAPA was added to the NCAR Graphics package in 1987.
.sp
.hD "DETAILS OF ROUTINES"
.sp
The EZMAPA routines are listed here in alphabetical order.
.hD "FUNCTION MAPACI"
.H 3 "Purpose"
MAPACI obtains, for a given area, a color index appropriate for use in creating
a color map having adjacent areas of different colors.
.H 3 "Usage"
The FORTRAN statement
.>>
.sf
ICI=MAPACI(IAI)
.<<
.ef
gives ICI the value of the color index (1 through 7, inclusive)
corresponding to the area whose area identifier is IAI.
.H 3 "Arguments"
.VL .6i
.LI "\fBIAI\fR"
Area identifier for an area defined by an area map created by
MAPBLA.
.LE
.H 3 "Important Note"
See the "Area Identifiers" section of the EZMAP documentation
for the area identifiers associated with various parts of the
world map, and for suggested color indices.
.hD "SUBROUTINE MAPBLA"
.H 3 "Purpose"
MAPBLA adds to an area map the boundary lines produced by projecting a selected
EZMAP dataset according to a specified EZMAP projection within a specified
area.
.H 3 "Usage"
.>>
.sf
CALL MAPBLA (IAM)
.<<
.ef
One or two groups of boundary lines are added to the area map by a call to
MAPBLA.  The first group has the group identifier 'G1',
with a default value of 1.
The group 'G1' consists
of a perimeter (either rectangular or elliptical, depending on the value of
the EZMAP parameter 'EL') and the set of projected boundary lines implied by
your selection of an EZMAP dataset (some combination of continental,
U.S., and world political outlines).  
For certain projections, a limb line
may also be included. 
(The \fIlimb\fR is the boundary between a projectable region and an
.hw unprojectable
unprojectable one.)
.sp
If the EZMAP parameter 'VS' has a value greater than zero,
a second group, with group identifier 'G2', is added to the area map.
(The default value of 'VS' is 1; the default value of 'G2' is 2.)
The purpose of the group 'G2' is to split areas up and thus reduce the
number of points required to define a typical area.
This is necessary because some hardware devices fail if the number of points 
defining an area is too large.
The group 'G2'
consists of a copy of the
perimeter and the limb line (if any) plus a set of vertical lines that split
the area inside the perimeter into 'VS' vertical strips.  
.sp
The perimeter and the limb in the groups 'G1' and 'G2' have the following
left and right area identifiers:
.BL
.LI
0, for the area inside the perimeter or limb.
.LI
-1, for the area outside the perimeter or limb.
.LE
.sp
The vertical lines in the group 'G2' have left and right area 
identifiers of 0.
.sp
To set the values of 'G1', 'G2', and 'VS',
call the EZMAP routine
MAPSTI.
To get the
current values of 'G1', 'G2', and 'VS', call the EZMAP routine MAPGTI.  
See the EZMAP documentation for descriptions of the routines MAPSTI and
MAPGTI.
.H 3 "Arguments"
.VL .6i
.LI "\fBIAM\fR"
Integer array in which an area map is being constructed.  IAM must
previously have appeared in a call to the routine ARINAM.
.LE
.hD "SUBROUTINE MAPGRM"
.H 3 "Purpose"
MAPGRM draws the same grid of latitude and longitude lines as would
be drawn by the EZMAP routine MAPGRD,
but the lines are masked by an area map created by MAPBLA.
.H 3 "Usage"
.sf
CALL MAPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.ef
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
Integer array in which an area map has been constructed by MAPBLA.
.LI "\fBXCS, YCS\fR"
Real arrays, of dimension MCS, to be used by MAPGRM in calls
to the user-provided routine LPR.
.LI "\fBMCS\fR"
Dimension of the arrays XCS and YCS.  MCS must be greater than or
equal to 2; the value 100 is suggested.
.LI "\fBIAI, IAG\fR"
Integer arrays, of dimension MAI, to be used by MAPGRM in
calls to the user-provided routine LPR.
.LI "\fBMAI\fR"
Dimension of the arrays IAI and IAG.  MAI must be greater than or
equal to 2.  If other groups of boundary lines are added to the area map (by
calling MAPITA and MAPIQA or by calling AREDAM), then MAI must be increased
by the number of such groups.
.LI "\fBLPR\fR"
A line-processing subroutine, which is called once for each piece
of the masked grid.  Each such piece is contained in exactly one area
defined by the area map.  LPR must appear in an EXTERNAL statement in the
routine that calls MAPGRM.  The subroutine LPR must be provided by you
and it must have the following structure:
.sp
.sf
SUBROUTINE LPR (XCS,YCS,NCS,IAI,IAG,NAI)
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)

(CODE TO PROCESS POLYLINE DEFINED BY XCS, YCS,
        IAI, AND IAG)

RETURN
END
.ef
.VL 1i
.LI "\fRXCS, YCS\fR"
Hold the X and Y coordinates, in the fractional coordinate
system, of NCS points defining a piece of the grid.
.LI "\fRNCS\fR"
Number of X and Y coordinates in the arrays XCS and YCS.
.LI "IAI, IAG"
Hold NAI pairs of identifiers for the area within which the
piece of the polyline lies.  For each value of I from 1 to NAI, IAI(I) is
the area identifier for the area with respect to the group of edges
specified by the group identifier IAG(I).
.LI "\fRNAI\fR"
Number of values in IAI and IAG.
NAI will be equal to the number of
groups of edges that you put in the area map.
.LE
.sp
You can assume that MAPGRM has done the following call:
.>>
.sf
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.<<
.ef
This call
ensures that, if the normalized device coordinates in XCS and YCS are
used in calls to such routines as GPL, the results will be correct.  
LPR may do its own SET call to achieve some other effect.
.LE
.hD "SUBROUTINES MAPITA and MAPIQA"
.H 3 "Purpose"
MAPBLA uses the routines MAPITA and MAPIQA to insert 
lines in an
area map.  You may call them directly to add the projection of an arbitrary
line, defined by the latitudes and longitudes of points along it, to the area
map.
.H 3 "Usage"
.sf
CALL MAPITA (RLAT,RLON,IFST,IAM,IGP,IDL,IDR)
.sp
CALL MAPIQA (IAM,IGP,IDL,IDR)
.ef
.sp
You must call MAPITA once for each point along the line.  After your
last call to MAPITA for a given line, 
you must call MAPIQA to signal the end
of the line.
.sp
The projection of the line segment joining two
points on the globe is considered to be the
straight-line segment joining the projections
of the points; no attempt is made to project
it as if it were a portion of a great circle.
.BL
.LI
If both endpoints of a line segment are visible,
the segment is considered to be entirely visible.
.LI
If both endpoints are invisible, the segment is
considered to be entirely invisible.
.LI
If one endpoint is visible and the other is not, 
a new point is interpolated at the boundary between the
visible and invisible portions.
.LE
.sp
Only the visible portions of the line are added to the area map.
.sp
\fBNote:\fR  Because of the way MAPITA and MAPIQA work,
(as described above)
points defining a line should not be too far apart on the globe.
.sp
\fBBoundaries:\fR  There are two types of boundaries between visible 
and invisible regions:
.BL
.LI
The \fIlimb\fR is a boundary between a projectable region and an
unprojectable one.
The limb may be circular, elliptical, or some other shape, depending on the
projection being used.
.hw orthographic
For example, an orthographic
.hw radius
projection has as its limb a circle, centered at (0,0), with a radius of 1.
.LI
The \fIperimeter\fR is a rectangular or elliptical boundary
defined by EZMAP
parameters set by you to specify the region you wish to view.
.LE
.H 3 "Arguments"
.VL .6i
.LI "\fBRLAT\fR"
Latitude of a point along the line.
.LI "\fBRLON\fR"
Longitude of a point along the line.
.LI "\fBIFST\fR"
A flag that is set to zero for the first point of a line and
non-zero for all other points of the line.
.LI "\fBIAM\fR"
An integer array containing an area map.
.LI "\fBIGP\fR"
Group identifier for the line.
.LI "\fBIDL\fR"
Area identifier for the area to the left of the line.
.LI "\fBIDR\fR"
Area identifier for the area to the right of the line.
.LE
.hD "SUBROUTINES MAPITM and MAPIQM"
.H 3 "Purpose"
MAPGRM uses the routines MAPITM and MAPIQM to draw lines of latitude
and longitude.
The lines are masked by the area map created by MAPBLA.  
You may call MAPITM
directly to draw the projection of an arbitrary line, defined by the latitudes
and longitudes of points along it, masked by the area map.  
.H 3 "Usage"
.sf
CALL MAPITM (RLAT,RLON,IFST,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.sp
CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,MAI,LPR)
.sp
.ef
You must call MAPITM once for each point along the line.
After the last call to MAPITM for
a given line, you must call MAPIQM to signal the end of the line.
.sp
The projection of the line segment joining two
points on the globe is considered to be the
straight-line segment joining the projections
of the points; no attempt is made to project
it as if it were a portion of a great circle.
.BL
.LI
If both endpoints of a line segment are visible,
the segment is considered to be entirely visible.
.LI
If both endpoints are invisible, the segment is
considered to be entirely invisible.
.LI
If one endpoint is visible and the other is not, 
a new point is interpolated at the boundary between the
visible and invisible portions.
.LE
.sp
Only the visible portions of the line are drawn in the area map.
.sp
\fBNote:\fR  Because of the way MAPITM and MAPIQM work,
(as described above)
points defining a line should not be too far apart on the globe.
.sp
\fBBoundaries:\fR  There are two types of boundaries between visible 
and invisible regions:
.BL
.LI
The \fIlimb\fR is a boundary between a projectable region and an
unprojectable one.
The limb may be circular, elliptical, or some other 
shape, ~depending ~on ~the ~projection ~being ~used.~  For ~example, ~an 
.br
.ne 2
orthographic projection has as its limb a circle, 
centered at (0,0), with a radius of 1.
.LI
The \fIperimeter\fR is a rectangular or elliptical boundary
defined by EZMAP
parameters set by you to specify the region you wish to view.
.LE
.H 3 "Arguments"
.VL 1i
.LI "\fBRLAT\fR"
Latitude of a point along the line.
.LI "\fBRLON\fR"
Longitude of a point along the line.
.LI "\fBIFST\fR"
A flag that is set to zero for the first point of a line and
non-zero for all other points of the line.
.LI "\fBIAM\fR"
Integer array in which an area map has been constructed by MAPBLA.
.LI "\fBXCS, YCS\fR"
Real arrays, of dimension MCS, to be used by MAPGRM in calls
to the user-provided routine LPR.
.LI "\fBMCS\fR"
Dimension of the arrays XCS and YCS.  MCS must be greater than or
equal to 2; the value 100 is suggested.
.LI "\fBIAI, IAG\fR"
Integer arrays, of dimension MAI, to be used by MAPGRM in
calls to the user-provided routine LPR.
.LI "\fBMAI\fR"
Dimension of the arrays IAI and IAG.  MAI must be greater than or
equal to 2.  If other groups of boundary lines are added to the area map (by
calling MAPITA and MAPIQA or by calling AREDAM), then MAI must be increased
by the number of such groups.
.LI "\fBLPR\fR"
A line-processing routine, which is called once for each piece
of the masked grid.  Each such piece is contained in exactly one area
defined by the area map.  LPR must appear in an EXTERNAL statement in the
routine that calls MAPGRM.  The routine LPR must be provided by you
and it must have the following structure:
.sp
.sf
SUBROUTINE LPR (XCS,YCS,NCS,IAI,IAG,NAI)
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)

(CODE TO PROCESS POLYLINE DEFINED BY XCS, YCS,
       IAI, AND IAG)

RETURN
END
.ef
.VL 1i
.LI "\fRXCS, YCS\fR"
Hold the X and Y coordinates, in the fractional coordinate
system, of NCS points defining a piece of the grid.
.LI "\fRNCS\fR"
Number of X and Y coordinates in the arrays XCS and YCS.
.LI "IAI, IAG"
Hold NAI pairs of identifiers for the area within which the
piece of the polyline lies.  For each value of I from 1 to NAI, IAI(I) is
the ~area identifier for the area with respect 
.br
.ne 2
to the group of edges
specified by the group identifier IAG(I).
.LI "\fRNAI\fR"
Number of values in IAI and IAG.
NAI will be equal to the number of
groups of edges that you put in the area map.
.LE
.sp
You can assume that MAPGRM has done the following call:
.>>
.sf
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.<<
.ef
This call
ensures that, if the normalized device coordinates in XCS and YCS are
used in calls to such routines as GPL, the results will be correct.  
LPR may do its own SET call to achieve some other effect.
.LE
.H 3 "Example"
This example uses the routines in the package EZMAPA to
produce a colored map of a Mercator projection of the globe, showing world
political boundaries, with lines of latitude and longitude drawn only over
water.  It should be noted that this example will work properly only on a
device with a color overlay capability. 
If you attempt to send the output to
a device like the color DICOMED, the attempt to draw black lines will not
work.
.in -1.5i
.sf

      PROGRAM COLRIT
C
C Define the array that holds the area map.
C
	DIMENSION IAM(250000)
C
C Dimension the arrays needed by ARSCAM for edges.
C
	DIMENSION XCS(10000),YCS(10000)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C ids.
C
	DIMENSION IAI(10),IAG(10)
C
C Define the RGB triples needed below.
C
	DIMENSION RGB(3,14)
C
	DATA RGB / 0.70,0.70,0.70 , 0.75,0.50,1.00 , 0.50,0.00,1.00 ,
     +             0.00,0.00,1.00 , 0.00,0.50,1.00 , 0.00,1.00,1.00 ,
     +             0.00,1.00,0.60 , 0.00,1.00,0.00 , 0.70,1.00,0.00 ,
     +             1.00,1.00,0.00 , 1.00,0.75,0.00 , 1.00,0.38,0.38 ,
     +             1.00,0.00,0.38 , 1.00,0.00,0.00 /
C
C Define a set of indices determining the ordering of the colors.
C
	DIMENSION IOC(14)
C
	DATA IOC / 6,2,5,12,10,11,1,3,4,8,9,7,13,14 /
C
C Define an array for aspect source flags.
C
	DIMENSION IF(13)
C
C Declare the routine which will color the areas and the one which
C will draw the lines of latitude and longitude over water.
C
	EXTERNAL COLRAM,COLRLN
C
C Open GKS.
C
	CALL OPNGKS
C
C Re-set certain aspect source flags to "individual".
C
	CALL GQASF (IE,IF)
	IF(11)=1
	IF(12)=1
	CALL GSASF (IF)
C
C Force solid fill.
C
	CALL GSFAIS (1)
C
C Define 15 different color indices.  The first 14 are spaced through
C the color spectrum and the final one is black.
C
	CALL SETUSV ('IM',15)
C
	DO 101 J=1,14
	  I=IOC(J)
	  IF (RGB(1,I).GE.RGB(2,I).AND.RGB(1,I).GE.RGB(3,I)) THEN
	    IND=1
	  ELSE IF (RGB(2,I).GE.RGB(1,I).AND.RGB(2,I).GE.RGB(3,I)) THEN
	    IND=2
	  ELSE IF (RGB(3,I).GE.RGB(1,I).AND.RGB(3,I).GE.RGB(2,I)) THEN
	    IND=3
	  END IF
	  CALL SETUSV ('IR',IFIX(10000.*RGB(1,I)/RGB(IND,I)))
	  CALL SETUSV ('IG',IFIX(10000.*RGB(2,I)/RGB(IND,I)))
	  CALL SETUSV ('IB',IFIX(10000.*RGB(3,I)/RGB(IND,I)))
	  CALL SETUSV ('IN',IFIX(10000.*RGB(IND,I)))
  101   CONTINUE
C
	CALL SETUSV ('IR',1)
	CALL SETUSV ('IG',1)
	CALL SETUSV ('IB',1)
	CALL SETUSV ('IN',0)
C
C Set up EZMAP, but don't draw anything.
C
	CALL MAPSTC ('OU','PO')
	CALL MAPROJ ('ME',0.,0.,0.)
	CALL MAPSET ('MA',0.,0.,0.,0.)
C
C Set the number of vertical strips and the group identifiers to
C be used by MAPBLA.
C
	CALL MAPSTI ('VS',150)
	CALL MAPSTI ('G1',1)
	CALL MAPSTI ('G2',2)
C
C Initialize EZMAP.
C
	CALL MAPINT
C
C Initialize the area map.
C
	CALL ARINAM (IAM,250000)
C
C Add edges to the area map.
C
	CALL MAPBLA (IAM)
C
C Pre-process the area map.
C
	CALL ARPRAM (IAM,0,0,0)
C
C Compute and print the amount of space used in the area map.
C
	ISU=250000-(IAM(6)-IAM(5)-1)
	PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C
C Set the background color.
C
	CALL GSCR (1,0,1.,1.,1.)
C
C Color the map.
C
	CALL ARSCAM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRAM)
C
C In black, draw a perimeter and outline all the countries.  We turn
C off the labels (since they seem to detract from the appearance of
C the plot) and we reduce the minimum vector length so as to include
C all of the points in the boundaries.
C
	CALL SETUSV ('II',15)
	CALL MAPSTI ('LA',0)
	CALL MAPSTI ('MV',1)
	CALL MAPLBL
	CALL MAPLOT
C
C Draw lines of latitude and longitude over water.  They will be in
C black because of the SETUSV call above.
C
	CALL MAPGRM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRLN)
C
C Advance the frame.
C
	CALL FRAME
C
C Close GKS.
C
	CALL CLSGKS
C
C Done.
C
	STOP
C
      END

      SUBROUTINE COLRAM (XCS,YCS,NCS,IAI,IAG,NAI)
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C For each area, get a set of points (using normalized device
C coordinates), two group identifiers and their associated area
C identifiers.  If both of the area identifiers are zero or negative,
C the area need not be color-filled; otherwise, it is filled with
C a color obtained from MAPACI.  If the area is defined by more than
C 150 points, print a comment.   (Assume that the device being used won't
C handle polygons defined by more points than that.)
C
	IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
	  ITM=MAX0(IAI(1),IAI(2))
	  IF (ITM.GT.0) THEN
	    IF (NCS.GT.150) PRINT * , 'COLRAM - NCS TOO BIG - ',NCS
	    CALL SETUSV ('II',MAPACI(ITM))
	    CALL GFA (NCS-1,XCS,YCS)
	  END IF
	END IF
C
C Done.
C
	RETURN
C
      END

      SUBROUTINE COLRLN (XCS,YCS,NCS,IAI,IAG,NAI)
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C For each line segment, get a set of points (using normalized
C device coordinates), two group identifiers and their associated
C area identifiers.  If both of the area identifiers are zero or
C negative, the segment is not drawn; otherwise, use MAPACI to
C see if the segment is over water and, if so, draw the segment.
C If the segment is defined by more than 150 points, print a
C comment.
C
	IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
	  ITM=MAX0(IAI(1),IAI(2))
	  IF (MAPACI(ITM).EQ.1) THEN
	    IF (NCS.GT.150) PRINT * , 'COLRLN - NCS TOO BIG - ',NCS
	    CALL GPL (NCS,XCS,YCS)
	  END IF
	END IF
C
C Done.
C
	RETURN
C
      END
.ef
.H 3 "Important Note"
These routines require a great deal of memory and CPU time.
In this example, the area map was dimensioned 250,000; a
little over 225,000 words were actually required.  The job ran about
22 seconds on the CRAY \%X-MP computer. 
Reducing the number of vertical strips
used would cut both of these numbers significantly.
