.\"
.\"	$Id: areas.m,v 1.1.1.1 1992-04-17 22:30:34 ncargd Exp $
.\"
.TH AREAS 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " AREAS - Creates an area map from a set of edges
.dsS1 " CALL ARINAM (IAM,LAM) initialize Areas
.dsS2 " CALL AREDAM (IAM,XCA,YCA,LCA,IGI,IDL,IDR) for each edge
.dsS3 " CALL ARPRAM (IAM,IF1,IF2,IF3) preprocess area map
.dsS4 " CALL ARSCAM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,APR) \ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ obtain definitions of areas created by edges \ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ inserted into area map
.dsS5 " CALL ARDRLN (IAM,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ Draw a polyline masked by a given area map
.dsS6 " CALL ARGTAI (IAM,XCD,YCD,IAI,IAG,MAI,NAI,ICF) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ Get area identifiers associated with a given point
.nrsN 6
./" USE tsi to PRINT THIS FILE!
.pn 71
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9AREAS\s11@"
.EF "@\s\fB11%\s9\fR@@August 1987\s11@"
.OF "@\s9August 1987@@\s11\fB%\fR@"
.de hD          \" Heading macro level one
.br
.ne 5
.sp 2
.ps +3
.ft B           \" boldface, 14 pt.
\\$1
.ft R
.ps -3
.sp 
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
.S 16
.S 11
.H 3  "\fB\s10BACKGROUND\fR"
.S 11
Several of the NCAR graphics utilities produce lines
that divide the plane into areas of interest.
For example, the contouring packages CONREC and CONRAN draw contour lines
that divide the plane into contour bands and the map-drawing utility EZMAP
draws boundary lines that divide the plane into areas that
represent continents, oceans, islands, lakes, and countries.
With the advent of GKS and of hardware devices with color-fill
capability, it has become important to obtain the definitions of the
areas defined by a set of edges 
so that they may be color-filled, shaded, or used as masks
while drawing other lines.
The package AREAS provides a general way to do this.
.sp
In AREAS, the lines that divide the plane are called edges.
Given a set of edges that divide a two-dimensional plane into areas,
AREAS creates a data structure called an area map.
The area map can then be used to
color-fill or shade areas and mask lines.
.sp
The terms \fIedge\fR, \fIarea\fR, \fIarea map\fR, and \fImask\fR
are defined fully below, along with other key terms you need to understand
to use AREAS effectively.
.H 3 "\s10DOCUMENT ORGANIZATION\s11"
The rest of this document on AREAS contains the following sections, in
this order:
.BL
.LI
\fBTerms and Concepts:\fR  Definitions and illustrations of the
key terms used in AREAS.
.LI
\fBCurrent Status:\fR  Current and future planned uses of AREAS.
.LI
\fBRequired Steps to Prepare Area Maps:\fR  Brief descriptions of the
three routines you must use in preparing an area map.
.LI
\fBUses of an Area Map:\fR  Brief descriptions of the three routines used to
obtain definitions of areas, mask lines, and obtain area identifiers
associated with a given point.
.LI
\fBDetails of Routines:\fR  Complete details of the six routines discussed
in the two previous sections.  Routines are in alphabetical order.
.LE
.H 2 "\fB\s10TERMS AND CONCEPTS\s11\fR"
Basic terms and concepts that you need to understand are
defined here.
.H 3 "\fBUser Coordinate System\fR"
The \fIuser coordinate system\fR is defined by the last call to the SPPS
routine SET or by the equivalent calls to GKS routines.
Such a call specifies the minimum and maximum values of
user-system X's and Y's and how such values are to be mapped into the
\fIfractional coordinate system\fR.
You define edges for AREAS using coordinates
in the user coordinate system.
.H 3 "\fBFractional Coordinate System\fR"
The \fIfractional coordinate system\fR is one in which X and Y coordinates take
on values between 0. and 1., inclusive.  Fractional coordinates are identical
to \fInormalized device coordinates\fR in GKS.
Areas are defined using coordinates in the fractional coordinate system.
.H 3 "\fBEdges\fR"
An \fIedge\fR is a sequence of straight-line segments, joined end-to-end.
Each \fIedge\fR is defined by: 1) an ordered set of 
two or more points that you specify in the user coordinate system,
2) an integer identifying the group to which the edge
belongs, 3) another integer identifying what area is to the left of the edge,
and 4) another integer identifying what area is to the right of the edge.
.sp
Most useful groups of edges will obey the following rules:
.BL
.LI
the line segments that 
make up one edge should not cross:
.sp
.mk
This:
.sp 2.25i
.rt
.in +2.5i
Not this:
.sp 2.25i
.in -2.5i
.LI
There should only be one area to the left and one area
to the right of an edge:
.br
.ne 2.25i
.sp
.mk
This:
.sp 2.25i
.rt
.in +2.5i
Not this (The edge defined by points 1, 2, and 3 has 2 areas to its right.):
.sp 2.25i
.in -2.5i
.LE
.H 3 "\fBGroup of Edges\fR"
A \fIgroup of edges\fR is a set of edges that were generated by 
a common source.
A \fIgroup of edges\fR divides
the plane into areas that are useful to you.
For example,
one group of edges might be the set of all
lines produced by projecting the U.S. state boundary lines. 
The set of all lines produced by contouring a
temperature field might constitute a second group of edges.
\&
.br
.ne 2.25i
.sp
.mk
A group of edges:
.sp 2.25i
.rt
.in +2.5i
Another group of edges:
.sp 2.25i
.in -2.5i
.H 3 "\fBGroup Identifier\fR"
A \fIgroup identifier\fR is a positive, non-zero integer (1, 2, ...) that is
associated with a particular group of edges.
.H 3 "\fBArea\fR"
An \fIarea\fR is one of the polygonal regions of the plane 
created by the edges in one or
more groups of edges.  For example, the edges obtained by projecting the U.S.
state boundary lines produce as areas the states (plus islands that are
parts of states, lakes that are included in states, and the like).  Adding
another group of edges obtained by projecting temperature contours creates
a more complicated set of areas, each of which must be characterized by a
geographical location and a temperature range.  An area created by the AREAS
package
always has associated with it as many \fIarea identifiers\fR (see definition
below) as there are groups
of edges in the area map from which the area came.
.sp
Areas created by two groups of edges:
.sp 2.25i
.H 3 "\fBArea Map\fR"
An \fIarea map\fR is a linked list of information
that is constructed by calls to
routines in the AREAS package, in an array that you provide.
The area map contains
all of the relevant information about one or more groups of edges.
The definitions of areas created by the edges can be obtained
from the area map.
.H 3 "\fBPerimeter\fR"
The \fIperimeter\fR of a group of edges is a subgroup of the group.
Each subgroup member
has either a left or a right area identifier that is negative, specifying
that what is to that side of the edge is outside the region of interest for
the group.  Typically, the other area identifier for the edge has a zero
value. 
.sp
A \fIdefault perimeter\fR is automatically supplied for each
group of edges. The default perimeter
consists of the largest possible square, traced in a
clockwise direction, with a \fIleft area identifier\fR of -1 and a \fIright area
identifier\fR of 0.  (The terms \fIleft area identifier\fR and \fIright area
identifier\fR are defined below.)
.br
.ne 2.25i
.sp
Default perimeter:
.sp 2.25i
You may specify a perimeter for each group of edges; areas
outside the perimeter are considered not to be of interest.  For example, if
land/water boundary lines are projected using an orthographic projection, a
circular perimeter defines the edge of visibility; areas outside the perimeter
are invisible.  The perimeter need not be defined by a simple closed curve;
it may be a more complicated topological object having one or more holes.
.sp
Below is an example of a doughnut-shaped perimeter;
it represents a stereographic projection of that part of the
globe between 10\(de N and 60\(de N.
.br
.ne 2.25i
.sp 2.25i
.H 3 "\fBLeft and Right Area Identifiers\fR"
Left and right are defined from the position 
of the first point of an
edge, looking toward the second point of the edge.
From such a vantage point,
the area to the left of the edge is identified by a
\fIleft area identifier\fR and the area to the right of
the edge is identified by a \fIright area identifier\fR.
.sp
Areas to the right and left of the edge defined by the points
1, 2, and 3 are marked as such here:
.sp 1.75i
.H 3 "\fBArea Identifiers\fR"
An \fIarea identifier\fR is an integer (...,-1, 0, 1, 2, ...) that is 
associated with
an area created by the edges in a single group of edges.
As each edge is defined, you must supply area identifiers 
for the areas to the left and right of the edge.
The following types of values may be used:
.BL
.LI
A negative value
identifies an area that is known to be outside the perimeter of the group.
.LI
A positive value identifies an area that is inside the perimeter of the
group.
.LI
A zero value implies
an unidentified area.
A zero value is used as a left or right area identifier 
when it is unknown or unimportant 
what area is to the left or to the right of the edge.
This may be because the area identifier information will not be
required or because that information will be supplied along with the
definition of some other edge of the same area.
.LE
.H 3 "Mask"
The set of areas defined by an area map can be used as a
\fImask\fR for a line that is being drawn.
The line is broken into pieces, each of which lies entirely
within one and only one area.
Each such piece can be drawn a different way.
.br
.sp
Latitude and longitude lines masked over land masses:
.bp
.H 3 "\fB\s10CURRENT STATUS\s11\fR"
AREAS was first introduced in 1987 
in response to a perceived need for color-fill capabilities.
As of July 1987,
EZMAP is the  only utility that has been extended
to use AREAS.
The new routines form a new utility called EZMAPA.
These routines can be used to create color-filled global maps.
See the EZMAPA documentation for usage details.
.sp
Work is in progress to create a new contouring package that will use AREAS
to create color-filled contour plots.
Other uses of AREAS are expected to evolve in response to user needs.
.sp
Since AREAS is a new utility,
there may be changes in the existing package. 
Consult your NCAR Graphics site representative
to make sure you are using the latest version.
.H 2 "\s10REQUIRED STEPS TO PREPARE AREA MAPS\s11"
Detailed descriptions of each of the routines in the package AREAS are given
in "Details of Routines" later in this section.
The steps listed here indicate the order in
which the routines should be called and the basic function of each.
.sp
Assume that you have one or more groups of edges, each of which divides the
user plane into areas.  Remember that each edge is defined by an ordered set
of points in the user coordinate system, a group identifier, a left area
identifier, and a right area identifier.
.sp
All routines in the package AREAS have names that begin with "AR."
.H 3 "\fBStep One\fR"
To initialize AREAS, execute the FORTRAN statement
.>>
.sf
CALL ARINAM (IAM,LAM)
.<<
.ef
LAM is unchanged by the call.
.VL .6i
.LI "\fBIAM\fR"
An integer array in which an area map is to be constructed.
.LI "\fBLAM\fR"
Length of the IAM array.
.sp
.LE
("ARINAM" stands for \fIinitialize area map\fR.)
.H 3 "\fBStep Two\fR"
For each edge, execute the FORTRAN statement
.>>
.sf
CALL AREDAM (IAM,XCA,YCA,LCA,IGI,IDL,IDR)
.<<
.ef
All arguments except IAM are input arguments and are unchanged by the call.
.VL .6i
.LI "\fBIAM\fR"
The area-map array. 
.LI "\fBXCA\fR"
Real array holding the X coordinates,
in the user coordinate system,
of the points defining the edge. 
.LI "\fBYCA\fR"
Real array holding the Y
coordinates, in the user coordinate system,
of the points defining the edge.
.LI "\fBLCA\fR"
Number of coordinates in XCA and YCA.
.LI "\fBIGI\fR"
Identifier of the group to
which the edge belongs.
.LI "\fBIDL\fR"
The identifier of the area that lies to the
left of the edge.
.LI "\fBIDR\fR"
Identifier of the area that lies to the
right of the edge.
.LE
.sp
("AREDAM" stands for \fIedge to area map\fR.)
.H 3 "\fBStep Three\fR"
Once all of the edges have been inserted in the area map, the area map must
be preprocessed.  You can do this by executing the FORTRAN statement
.>>
.sf
CALL ARPRAM (IAM,IF1,IF2,IF3)
.<<
.ef
\fBNote:\fR  If you do not call ARPRAM,
any of the routines ARSCAM, ARDRLN, or ARGTAI
will call it.
However, the routines
will set the shortcut flags IF1, IF2, and IF3 to not take
any shortcuts,
which may result in the subroutine running longer than is
necessary.
For the details of using the flags, see ARPRAM in 
the "Details of Routines" section below.
.sp
All arguments except IAM are input arguments and are unchanged
by the call. 
.VL 1.1i
.LI "\fBIAM\fR"
The area-map array.
.LI "\fBIF1, IF2, IF3\fR"
Flags that designate
what shortcuts are allowed in the preprocessing step.  
.LE
.sp
("ARPRAM" stands for \fIpreprocess area map.\fR)
.H 2 "\s10USES OF AN AREA MAP\s11"
Once you have completed the preceding three steps,
you can use the resulting area map for three different purposes:
.BL
.LI
to obtain the definitions of areas.
.LI
to mask objects being drawn. 
.LI
to get the area identifiers
associated with a given point.
.LE
.sp
Each of these uses is explained briefly below.
For more information, see the "Details of Routines"
section later in this document.
.H 3 "Obtaining Area Definitions"
To obtain the definitions of the areas created by the edges
inserted in the area map,
(to color the area, for example) execute the following FORTRAN statement:
.>>
.sf
CALL ARSCAM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,APR)
.<<
.ef
All arguments except IAM are input arguments
and are unchanged by the call.
.VL .6i
.LI "\fBIAM\fR"
The area-map array.
.LI "\fBXCS\fR" 
Real array of size MCS.
.LI "\fBYCS\fR"
Real array of size MCS.
.LI "\fBMCS\fR"
Dimension of each of the arrays XCS and YCS.
.LI "\fBIAI\fR" 
Integer array of size MAI.
.LI "\fBIAG\fR"
Integer array of size MAI.
.LI "\fBMAI\fR"
Dimension of each of the arrays IAI and IAG.
.LI "\fBAPR\fR"
An area-processing routine
you supply.
The name of APR must appear in an EXTERNAL statement in
the routine that calls ARSCAM.  ARSCAM scans the area map from left to right,
picking off, one by one, the areas defined by it.
ARSCAM calls
the routine APR to process each defined area.
For details of the structure of the routine APR,
see ARSCAM in the "Details of Routines" section below.
.sp
.LE
("ARSCAM" stands for \fIscan area map\fR.)
.H 3 "\fBMasking\fR"
The routine ARDRLN is used to draw a polyline
that is masked by a given area map.
(A \fIpolyline\fR is a line defined by two or more points.)
Given a polyline and an area map, ARDRLN breaks the polyline into
pieces in such a way that each piece lies within only one area.
For each such
piece of the polyline, a routine specified by you is called.  
That routine may draw the piece using a specified color, intensity, and dash
pattern (or it may not draw the line at all).
For example, ARDRLN can be used to limit the
drawing of latitude and longitude lines to areas over the ocean, omitting
the lines over land.
In a future contouring package, 
ARDRLN will be used to keep contour lines from passing through
contour labels.
.sp
ARDRLN is called using a FORTRAN statement such as
.>>
.sf
CALL ARDRLN (IAM,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.<<
.ef
.VL .6i
.LI "\fBIAM\fR"
The area-map array.
.LI "\fBXCD\fR"
Real array holding the X coordinates, 
in the user coordinate system,
of the points defining the polyline. 
.LI "\fBYCD\fR"
Real array holding the Y
coordinates, in the user coordinate system,
of the points defining the polyline.
.LI "\fBNCD\fR"
Number of points defining the polyline.
.LI "\fBXCS\fR" 
Real array of size MCS; used in calls to LPR.
.LI "\fBYCS\fR"
Real array of size MCS; used in calls to LPR.
.LI "\fBMCS\fR"
Dimension of each of the arrays XCS and YCS.
.LI "\fBIAI\fR" 
Integer array of size MAI; used in calls to LPR.
.LI "\fBIAG\fR"
Integer array of size MAI; used in calls to LPR.
.LI "\fBMAI\fR"
Dimension of each of the arrays IAI and IAG.
.bp
.LI "\fBLPR\fR"
Line-processing routine that is supplied by you,
which must be declared EXTERNAL in the
routine calling ARDRLN.
.hw details
For details of the structure of the routine LPR,
see ARDRLN in the 
.ne 2
"Details of Routines" section below. 
.LE
.sp
("ARDRLN" stands for \fIdraw line\fR.)
.H 3 "\fBObtaining Area Identifiers\fR"
The routine ARGTAI is used to get the area identifiers associated
with a given point.
ARGTAI is called using a FORTRAN statement such as
.>>
.sf
CALL ARGTAI (IAM,XCD,YCD,IAI,IAG,MAI,NAI,ICF)
.<<
.ef
.VL .6i
.LI "\fBIAM\fR"
The area-map array.
.LI "\fBXCD\fR"
X coordinate, in the
current user coordinate system,
of a point about which you want to obtain information.
.LI "\fBYCD\fR"
Y coordinate, in the
current user coordinate system,
of a point about which you want to obtain information.
.LI "\fBIAI\fR" 
Integer array of size MAI, 
to which information about the
specified point will be returned.
.LI "\fBIAG\fR"
Integer array of size MAI, 
to which information about the
specified point will be returned.
.LI "\fBMAI\fR"
Dimension of each of the arrays IAI and IAG.
.LI "\fBNAI\fR"
Number of values returned in IAI and IAG.
NAI will be equal to the
number of groups of edges that you put in the area map.
.LI "\fBICF\fR"
Flag set non-zero by you to indicate that the definition of the
user coordinate system has been changed since the last call to ARGTAI, in
which case calls to GETSET must be executed by ARGTAI.
If you set the flag to zero,
it will be assumed that the information retrieved previously is still correct
and that calls to GETSET may be skipped.
.sp
.LE
("ARGTAI" stands for \fIget area identifiers\fR.)
.sp 3
.in -1.5i
.B
.S 14
DETAILS OF ROUTINES
.S 11
.R
.sp
.in +1.5i
This section contains detailed descriptions of all the routines in
AREAS. The routines are arranged in alphabetical order.
.sp 2
.B
.in -1.5i
.S 14
ROUTINE ARDRLN 
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
ARDRLN is given an existing area map and a polyline.  ARDRLN breaks
the polyline into pieces, each of which lies entirely within a single area
defined by the area map.  It calls a user routine LPR once for each piece.
You can program LPR to do whatever you wish with the pieces.
.hw example
For example,
you can change the color, intensity, or dash pattern
.hw drawing
.ne 2
before drawing a given piece \(em or you
may choose not to draw a given piece at all.
.H 3 "Usage"
.sf
CALL ARDRLN (IAM,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
.ef
.H 3 "Type and Dimension of Arguments"
.mk
.nf
INTEGER IAM(*)
REAL XCD(*)
REAL YCD(*)
INTEGER NCD
.rt
.in +1.6i
REAL XCS(MCS)
REAL YCS(MCS)
INTEGER MCS
INTEGER IAI(MAI)
.rt 
.in +1.6i
INTEGER IAG(MAI)
INTEGER MAI
EXTERNAL LPR
.in -3.2i
.sp
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
An integer array containing an area map.  IAM must have been
initialized by a call to ARINAM and edges must have been put into it by calls
to AREDAM.  
If you did not preprocess the area map by calling ARPRAM,
ARDRLN will call ARPRAM before doing anything else.
.LI "\fBXCD, YCD\fR"
Real arrays.
The first NCD elements are the X and Y coordinates,
in the user coordinate system,
of points defining a polyline in the user coordinate system.
.LI "\fBNCD\fR"
Number of coordinates in XCD and YCD.
.LI "\fBXCS, YCS\fR"
Real arrays, each dimensioned MCS, for use by ARDRLN in calls
to the line processing routine LPR.
.LI "\fBMCS\fR"
Dimension of each of the arrays XCS and YCS.
.LI "\fBIAI, IAG\fR"
Integer arrays, each dimensioned MAI, for use by ARDRLN in
calls to the line processing routine LPR.
.LI "\fBMAI\fR"
Dimension of each of the arrays IAI and IAG.
.LI "\fBLPR\fR"
A line-processing routine, declared EXTERNAL in the routine which
calls ARDRLN.  You must supply the routine LPR and it must have the
following form:
.sp
.sf
SUBROUTINE LPR (XCS,YCS,NCS,IAI,IAG,NAI)
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)

(CODE TO PROCESS POLYLINE DEFINED BY XCS,
     YCS, IAI, AND IAG)

RETURN
END
.ef
.VL 1i
.LI "\fRXCS, YCS\fR"
Hold the X and Y coordinates, in the fractional coordinate
system, of NCS points defining a piece of the original polyline.
.LI "\fRNCS\fR"
Number of X and Y coordinates in the arrays XCS and YCS.
.LI "\fRIAI, IAG\fR"
Hold NAI pairs of identifiers for the area within which the
piece of the polyline lies.  For each value of I from 1 to NAI,
IAI(I) is
the area identifier for the area with respect to the group of edges
specified by the group identifier IAG(I).
.LI "\fRNAI\fR"
Number of values in IAI and IAG.
NAI will be equal to the
number of groups of edges 
.ne 2
that you put in the area map.
.sp
.LE
Before executing the first call to LPR, ARDRLN calls GETSET to retrieve the
current user-system mapping parameters and then executes the statement
.>>
.sf
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.<<
.ef
This ensures that, if the normalized device coordinates in XCS and YCS are
used in calls to such routines as GPL and CURVE, the results will be correct.  
LPR may do its own SET call to achieve some other effect.
Before returning control to the calling routine,
ARDRLN calls SET again to restore the original
mapping parameters.
.LE
.B
.sp 2
.in -1.5i
.S 14
ROUTINE AREDAM 
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
AREDAM is called once for each edge that is to be added to a given area map.
.H 3 "Usage"
.sf
CALL AREDAM (IAM,XCA,YCA,LCA,IGI,IDL,IDR)
.ef
.sp
If an edge is too long to be conveniently handled by a single
call to AREDAM, the edge may be broken into two or more pieces,
but the pieces must fit
together to define the whole edge. 
Each piece following the first must
start with the last point of the preceding edge.  For example, to approximate
a circle with 360 points, you could do a single call defining a 361-point
edge in which the last point was a duplicate of the first, or you could do
two calls, the first using points 1 through 181, and the second using points
181 through 361, of the original edge.
.H 3 "Type and Dimension of Arguments"
.mk
.nf
INTEGER IAM(*)
REAL XCA(*)
REAL YCA(*)
.rt
.in +1.6i
INTEGER LCA
INTEGER IGI
.rt
.in +1.6i
INTEGER IDL
INTEGER IDR
.in -3.2i
.fi
.sp
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
The integer array in which the area map is being constructed.  IAM must
have been initialized previously by a call to the routine ARINAM.
.LI "\fBXCA, YCA\fR"
Real arrays.
The first LCA elements are the X and Y
coordinates of the points that
define the edge to be added to the area map
in the current user coordinate system.
Note that not all edges have to be
defined in the same user coordinate system (generally, though, all the edges
in a particular group will be).
.LI "\fBLCA\fR"
Specifies the number of points defining the edge.  The order in which the
points are specified determines the meaning of "left" and "right."
"Left" and "right" are
defined from the standpoint of an observer at the position of point 1, 
.ne 2
looking
in the direction of point 2.
.LI "\fBIGI\fR"
Group identifier for the group of edges to which the edge belongs.
.LI "\fBIDL\fR"
Area identifier for the area to the left of the edge.
.LI "\fBIDR\fR"
Area identifier for the area to the right of the edge.
.LE
.sp 2
.in -1.5i
.B
.S 14
ROUTINE ARGTAI
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
Given an area map and the user-system coordinates of a point, the routine
ARGTAI returns information about the area containing that point.
.H 3 "Usage"
.sf
CALL ARGTAI (IAM,XCD,YCD,IAI,IAG,MAI,NAI,ICF)
.ef
.H 3 "Type and Dimension of Arguments"
.mk
.nf
INTEGER IAM(*)
REAL XCD
REAL YCD
.rt
.in +1.6i
INTEGER IAI(MAI)
INTEGER IAG(MAI)
INTEGER MAI
.rt
.in +1.6i
INTEGER NAI
INTEGER ICF
.fi
.in -3.2i
.sp
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
An integer array containing an area map that has been initialized by
a call to ARINAM and to which edges have been added by calls to AREDAM.
If you did not preprocess the area map by calling ARPRAM,
ARGTAI will call ARPRAM before doing anything else.
.LI "\fBXCD, YCD\fR"
The X and Y coordinates, in the current user coordinate
system, of a point about which information is desired.  Note that the current
user coordinate system need not be the one that was current while the area
map was being constructed.
.LI "\fBIAI, IAG\fR"
Integer arrays, dimensioned MAI, in which information will
be returned by ARGTAI.  For each value of I from 1 to NAI,
IAI(I) is the area
identifier and IAG(I) is the associated group identifier for the area containing
the point (XCD,YCD).
Note that the entries in IAI and IAG may not be assumed
to be in any particular order;
if group identifiers 1, 2, and 3 were used,
one of the set (IAG(1),IAG(2),IAG(3)) will be a 1, one will be a 2, and one
will be a 3, but they may be in any of six different orders.
.LI "\fBMAI\fR"
Dimension of the arrays IAI and IAG.
.LI "\fBNAI\fR"
Set by ARGTAI to indicate how many entries have been placed in
the arrays IAI and IAG.
NAI will be equal to the
number of groups of edges that you put in the area map.
.LI "\fBICF\fR"
Flag set non-zero by you to indicate that the definition of the
user coordinate system has been changed since the last call to ARGTAI, in
which case calls to GETSET must be executed by ARGTAI.
If you set the flag to zero,
ARGTAI assumes that the information retrieved previously is still correct
and that calls to GETSET may be skipped.
This becomes important when you make many
calls to ARGTAI and you want to avoid the overhead associated
with the calls to GETSET.
.LE
.sp 2
.in -1.5i
.S 14
.B
ROUTINE ARINAM
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
The routine ARINAM is called to initialize an area map.
.H 3 "Usage"
CALL ARINAM (IAM,LAM)
.ef
.H 3 "Type and Dimension of Arguments"
INTEGER IAM(LAM)
.br
INTEGER LAM
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
An integer array in which an area map is to be constructed.  Each
original point requires ten words in the array IAM and additional points will
be added to these; therefore, the array needs to be large.
.LI "\fBLAM\fR"
Length of the array IAM.
.LE
.sp 2
.in -1.5i
.S 14
.B
ROUTINE ARPRAM
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
ARPRAM is called to preprocess an area map that has been initialized by a
call to ARINAM and to which edges have been added by calls to AREDAM. 
.H 3 "Usage"
.sf
CALL ARPRAM (IAM,IF1,IF2,IF3)
.ef
.sp
If you call ARPRAM, you must do it
before you call any of the
routines ARDRLN, ARGTAI, or ARSCAM.
If you did not call ARPRAM, 
each of these routines
will call it before doing anything else.
The
only advantage in calling ARPRAM yourself is that you may be able to set some of
the arguments to make it work more efficiently.
In order to understand how to set the arguments IF1, IF2, and IF3,
you need to understand what
ARPRAM does:
.AL 1
.LI
ARPRAM first shortens edge segments whose projections on the X axis are more
than twice as long as the average.
ARPRAM does this by interpolating points along their lengths.
This leads to greater efficiency in executing other parts of the algorithm.
.LI
Next, ARPRAM finds all intersections of edges with each other and interpolates
points along the edge segments that intersect; this can be a time-consuming
operation.  
.VL .6i
.LI "\fBIF1\fR"
If you set IF1 non-zero, ARPRAM examines
a pair of edge segments only
if one of the pair has a left or right area identifier that is zero or
negative.  This would be appropriate, for example, for contour lines, which
are known not to intersect each other, but only to intersect the perimeter.
.LE
.LI
Next, ARPRAM searches for and removes "dangling" edges (those that do not
contribute to enclosing any area); such edges are removed.
.VL .6i
.LI "\fBIF2\fR"
If you set IF2 
non-zero, ARPRAM skips this process.  Again, this would be appropriate for
contour lines, which are known not to have any such edges.
This would not be
appropriate for EZMAP boundary lines.  (The EZMAP dataset contains, for
example, islands that are formed from simple, unclosed curves.)
.LE
.LI
Next, ARPRAM adjusts area-identifier information in the area map.  It examines
all the edge segments of each area in each group to see what area identifier
should be assigned to the area,
and makes the following types of adjustments:
.BL
.LI
If a negative value is found anywhere, the
area is considered to have area identifier -1.  
.LI
If only zeroes are found, the
area is considered to have area identifier 0.
.LI
If no negatives are found and
at least one value greater than zero is found, the greater-than-zero value
belonging to the edge most recently inserted in the area map is used.  
.LE
.sp
In any
case, all of the area identifiers for a given area are reset to the same
value, so that all the information will be consistent.
.VL .6i
.LI "\fBIF3\fR"
If you set IF3
non-zero, ARPRAM speeds up this process by omitting the consideration of
"holes" in the areas examined.  Again, this is appropriate for contour lines.
.LE
.LI
In every case, if an internal problem is detected that seems to indicate
that a shortcut was taken inappropriately, 
ARPRAM attempts to fix the problem
by re-executing the code with the shortcut turned off.
.LE
.H 3 "Type and Dimension of Arguments"
.mk
.nf
INTEGER IAM(*)
INTEGER IF1
.rt
.in +1.6i
INTEGER IF2
.rt
.in +1.6i
INTEGER IF3
.fi
.sp
.H 3 "Arguments"
.VL 1.2i
.LI "\fBIAM\fR"
An integer array containing an area map that has been initialized by
a call to ARINAM and to which edges have been added by calls to AREDAM.
.LI "\fBIF1, IF2, IF3\fR"
Shortcut flags, as described above.  In every case, the
value 0 indicates that ARPRAM will not take a particular shortcut; any other
value indicates that ARPRAM will take the shortcut.
.LE
.sp 2
.in -1.5i
.S 14
.B
ROUTINE ARSCAM
.S 11
.in +1.5i
.R
.H 3 "Latest Revision"
June 1987
.H 3 "Purpose"
Given an existing area map, the routine ARSCAM scans it from left to right,
extracting the definitions of all the areas formed by the area map.
.H 3 "Usage"
.sf
CALL ARSCAM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,APR)
.ef
.sp
ARSCAM calls a user routine APR 
once for each scanned area; APR may be programmed to do
whatever you want with each area
(color it, fill it with cross-hatch marks, or save it in
a file, for example).
.H 3 "Type and Dimension of Arguments"
.mk
.nf
INTEGER IAM(*)
REAL XCS(MCS)
REAL YCS(MCS)
.rt
.in +1.6i
INTEGER MCS
INTEGER IAI(MAI)
INTEGER IAG(MAI)
.rt
.in +1.6i
INTEGER MAI
EXTERNAL APR
.fi
.in -3.2i
.sp
.H 3 "Arguments"
.VL 1i
.LI "\fBIAM\fR"
An integer array containing an area map that has been initialized by
a call to ARINAM and to which edges have been added by calls to AREDAM.  
If you did not preprocess the area map by calling ARPRAM,
ARSCRAM will call it before doing anything else.
.LI "\fBXCS, YCS\fR"
Real arrays, each dimensioned MCS, for use by ARSCAM in calls
to the area processing routine APR.
.LI "\fBMCS\fR"
Dimension of each of the arrays XCS and YCS.
.LI "\fBIAI, IAG\fR"
Integer arrays, each dimensioned MAI, for use by ARSCAM in
calls to the area processing routine APR.
.LI "\fBMAI\fR"
Dimension of each of the arrays IAI and IAG.
.LI "\fBAPR\fR"
An area processing routine, declared EXTERNAL in the routine that
calls ARSCAM.  You must supply the routine APR and it must have the
following form:
.sp
.sf
SUBROUTINE APR (XCS,YCS,NCS,IAI,IAG,NAI)
DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)

(CODE TO PROCESS THE AREA DEFINED BY XCS, 
    YCS, IAI, AND IAG)

RETURN
END
.ef
.VL 1i
.LI "\fRXCS, YCS\fR"
Hold the X and Y coordinates, in the fractional coordinate
system, of NCS points defining a polygonal area.  The last of these points
is a duplicate of the first.  Holes in an area are traced in such a
way as to maximize the probability of hardware fill working properly by
using vertical lines to get to and from the
holes and tracing them in the proper direction.
.LI "\fRNCS\fR"
Number of X and Y coordinates in the arrays XCS and YCS.
.LI "\fRIAI, IAG\fR"
Hold NAI pairs of identifiers for the area defined by XCS
and YCS.  For each value of I from 1 to NAI,
IAI(I) is the area identifier for
the area with respect to the group of edges specified by the group identifier
IAG(I).
.LI "\fRNAI\fR"
Number of values in IAI and IAG. 
NAI will be equal to the number of groups of edges that you put in the
area map.
.LE
.sp
Before executing the first call to APR, ARSCAM calls GETSET to retrieve the
current user-system mapping parameters and then executes the statement
.>>
.sf
CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
.<<
.ef
This ensures that, if the normalized device coordinates in XCS and YCS are
used in calls to such routines as GFA and FILL, the results will be correct.
APR may do its own SET call to achieve some other effect.  Before
returning control to the calling routine,
ARSCAM re-calls SET to restore the original
mapping parameters.
.LE
.H 3 "Error Messages"
When the package detects errors,
it calls the routine SETER.
As of this writing,
all errors
are treated as recoverable.  The following error messages will be issued to
the standard diagnostic unit. 
You will probably never 
see the diagnostic "ALGORITHM FAILURE," but, with a
new package, such problems are possible.
If you get the "ALGORITHM FAILURE" message, 
contact your NCAR Graphics site representative.
.>>
.sf
    ARDRLN - AREA-MAP ARRAY UNINITIALIZED
    ARDRLN - MAI TOO SMALL
    ARDRLN - ALGORITHM FAILURE
.<<
.ef
.>>
.sf
    AREDAM - AREA-MAP ARRAY UNINITIALIZED
    AREDAM - AREA-MAP ARRAY OVERFLOW
.<<
.ef
.>>
.sf
    ARGTAI - AREA-MAP ARRAY UNINITIALIZED
    ARGTAI - MAI TOO SMALL
    ARDRLN - ALGORITHM FAILURE
.<<
.ef
.>>
.sf
    ARINAM - AREA-MAP ARRAY IS TOO SMALL
.<<
.ef
.>>
.sf
    ARPRAM - AREA-MAP ARRAY UNINITIALIZED
    ARPRAM - NO EDGES IN AREA MAP
    ARPRAM - AREA-MAP ARRAY OVERFLOW
    ARPRAM - ALGORITHM FAILURE
    ARPRAM - AREA-MAP ARRAY OVERFLOW
.<<
.ef
.>>
.sf
    ARSCAM - AREA-MAP ARRAY UNINITIALIZED
    ARSCAM - ALGORITHM FAILURE
    ARSCAM - MCS TOO SMALL
    ARSCAM - MAI TOO SMALL
    ARSCAM - AREA-MAP ARRAY OVERFLOW
.<<
.ef
.H 3 "Special Problems"
On some devices, there is an upper limit on the number of points that can be
used to define a polygon to be color-filled.  If the areas in which you are
interested are intrinsically more complicated than the limit allows,
you may overlay the areas
with a group of edges defining a set of vertical strips.  (Because of the way
the code works, you should not use horizontal strips, which will result in
seriously reduced efficiency.) 
Since the area identifier is not important,
choose any non-negative value
for use as the area identifiers
for all of the strips.  
Using vertical strips reduces the number of
points defining each area. 
However, it also
increases the run time of AREAS and the time required to
draw any resulting plot.
.H 3 "Using AREAS with EZMAPA"
The mapping utility, EZMAP, has been extended to use AREAS to
create solid-colored maps.
See the documentation for EZMAPA for instructions.
