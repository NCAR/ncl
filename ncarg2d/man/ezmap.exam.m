.\"
.\"	$Id: ezmap.exam.m,v 1.1.1.1 1992-04-17 22:30:21 ncargd Exp $
.\"
.TH EZMAP.EXAM 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
./" USE tsi to PRINT THIS FILE!
.pn 231
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
.H 2 "EXAMPLES"
This write-up contains the FORTRAN text and Xerox laser printer output from
ten programs which use EZMAP to produce maps.  These programs illustrate many
of the capabilities of EZMAP.
.P
Example 1 shows the United States on a Lambert conformal conic.
.P
Example 2 shows Africa on a stereographic projection, with an elliptical
perimeter.
.P
Example 3 shows simplified continental outlines on a Mercator projection.
.P
Example 4 shows the world in three parts, using stereographic projections
for the poles and a Mercator projection for the equatorial belt.
.P
Example 5 shows maximal-area plots for all types of EZMAP projections.
.P
Example 6 shows the effect of the rotation angle ROTA on a satellite-view
projection.
.P
Example 7 presents a scheme for labeling the meridians on certain types
of maps.
.P
Example 8 demonstrates how the routine MAPUSR is used.
.P
Example 9 shows the numbering of segments in the outline dataset 'CO' and
presents a program which may be used to obtain such plots for the other
outline datasets.  This example is somewhat out of date, since the segment
numbers are only of use in a user version of the routine called MAPEOD and
it is probably better now to use the left and right area identifiers, rather
than the segment numbers, in deciding which outline segments to keep and
which to omit.
.P
The final example shows how to draw lines, defined by points for which one
has the latitudes and longitudes, on a map produced by EZMAP.
.P
Each of the example programs uses a subroutine called BNDARY to draw a
boundary line indicating where the edge of the plotter frame is.  This
subroutine is not a part of AUTOGRAPH or of the NCAR plot package.  It
is written as follows:

.nf
.sf
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
.fi
.ef
.SK
.H 3 "Example 1"
The United States is mapped using a Lambert conformal conic.
.SK
.in
.nf
.sf
      PROGRAM EXMPL1
C
C The program EXMPL1 produces a map of the U.S., using a Lambert
C conformal conic.
C
C Define the label for the top of the map.
C
      CHARACTER*37 PLBL
C
      DATA PLBL / 'THE U.S. ON A LAMBERT CONFORMAL CONIC' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','US')
C
C Set the projection-type parameters.
C
      CALL MAPROJ ('LC',30.,-100.,45.)
C
C Set the limits parameters.
C
      CALL MAPSET ('CO',22.6,-120.,46.9,-64.2)
C
C Draw the map.
C
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.925,PLBL,37,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
.fi
.ef
.SK
.H 3 "Example 2"
A stereographic view of the international boundaries in Africa, jazzed
up by using an elliptical frame.
.sp 7i
.sf
.in
.nf
      PROGRAM EXMPL2
C
C This program produces a nice view of Africa, with an elliptical
C perimeter.
C
C Define the label for the top of the map.
C
      CHARACTER*26 PLBL
.bp
C
      DATA PLBL / 'CAN YOU NAME THE COUNTRIES' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Use an elliptical perimeter.
C
      CALL MAPSTI ('EL',1)
C
C Dot the outlines, using dots a quarter as far apart as the default.
C
      CALL MAPSTI ('DO',1)
      CALL MAPSTI ('DD',3)
C
C Show continents and international boundaries.
C
      CALL MAPSTC ('OU','PO')
C
C Use a stereographic projection.
C
      CALL MAPROJ ('ST',0.,0.,0.)
C
C Specify where two corners of the map are.
C
      CALL MAPSET ('CO',-38.,-28.,40.,62.)
C
C Draw the map.
C
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,26,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
.ef
.fi
.SK
.H 3 "Example 3"
A Mercator projection of the world, using a version of MAPEOD which
scrubs all but the principal land masses (with a tip of the hat to
Cicely Ridley).  The left and right area identifiers are used to determine
which segments to keep and which to scrub.  Segment-number information,
obtained by running the program shown in example 9, could have been used
instead.
.SK
.nf
.in
.sf
      PROGRAM EXMPL3
C
C Produce a Mercator projection of the whole globe, using
C simplified continental outlines.  See the routine MAPEOD,
C below.
C
C Define the label for the top of the map.
C
      CHARACTER*46 PLBL
C
      DATA PLBL/'SIMPLIFIED CONTINENTS ON A MERCATOR PROJECTION'/
C
C Open GKS.
C
      CALL OPNGKS
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Draw the map.
C
      CALL SUPMAP (9,0.,0.,0.,0.,0.,0.,0.,1,15,2,0,IERR)
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,46,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
C This version of MAPEOD uses area identifiers for the outline
C dataset 'CO' to suppress all but the major global land masses.
C In honor of Cicely Ridley, the British Isles are included.
C
C Cull the segment if there's no ocean on either side of it ...
C
      IF (IDLS.NE.  2.AND.IDRS.NE.  2) NPTS=0
C
C or if it's not an edge of any of the desired areas.
C
      IF (IDLS.NE.  1.AND.IDRS.NE.  1.AND.
     +    IDLS.NE.  3.AND.IDRS.NE.  3.AND.
     +    IDLS.NE. 11.AND.IDRS.NE. 11.AND.
     +    IDLS.NE. 79.AND.IDRS.NE. 79.AND.
     +    IDLS.NE. 99.AND.IDRS.NE. 99.AND.
     +    IDLS.NE.104.AND.IDRS.NE.104.AND.
     +    IDLS.NE.107.AND.IDRS.NE.107.AND.
     +    IDLS.NE.163.AND.IDRS.NE.163     ) NPTS=0
C
C Done.
C
      RETURN
C
      END
.ef
.fi
.SK
.H 3 "Example 4"
The world in three parts.  A possibly useful way of depicting the
entire globe, using stereographic projections for the poles and a
Mercator projection for the equatorial belt.
.SK
.nf
.in
.sf
      PROGRAM EXMPL4
C
C This program produces a single frame, with polar stereographic
C views of the poles and a Mercator projection of the rest.
C
C Define the label for the top of the map.
C
      CHARACTER*26 PLBL
C
      DATA PLBL / 'OMNIA TERRA IN PARTES TRES' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','PO')
C
C Use dotted outlines and move the dots a little closer together
C than normal.
C
      CALL MAPSTI ('DO',1)
      CALL MAPSTI ('DD',3)
C
C Do the Mercator projection of the equatorial belt first.
C
      CALL MAPPOS (.05,.95,.05,.5)
      CALL MAPROJ ('ME',0.,0.,0.)
      CALL MAPSET ('LI',-3.1416,+3.1416,-1.5708,+1.5708)
      CALL MAPDRW
C
C Switch to an elliptical (in this case, circular) boundary.
C
      CALL MAPSTI ('EL',1)
C
C Do a polar stereographic view of the North Pole ...
C
      CALL MAPPOS (.07,.48,.52,.93)
      CALL MAPROJ ('ST',90.,0.,-90.)
      CALL MAPSET ('AN',30.,30.,30.,30.)
      CALL MAPDRW
C
C and then a similar view of the South Pole.
C
      CALL MAPPOS (.52,.93,.52,.93)
      CALL MAPROJ ('ST',-90.,0.,-90.)
      CALL MAPSET ('AN',30.,30.,30.,30.)
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,26,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
.ef
.fi
.SK
.H 3 "Example 5"
Intended as a reference.  Maximal-area plots of all the possible EZMAP
projections.
.SK
.in
.sf
.nf
      PROGRAM EXMPL5
C
C The program EXMPL5 produces a single frame with maximal-area
C views of all the EZMAP projections of the globe.
C
C Define the label for the top of the map.
C
      CHARACTER*37 PLBL
C
      DATA PLBL / 'MAXIMAL-AREA PROJECTIONS OF ALL TYPES' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','CO')
C
C Put meridians and parallels every 15 degrees.
C
      CALL MAPSTI ('GR',15)
C
C Reduce the label size.
C
      CALL MAPSTI ('LS',0)
C
C Lambert conformal conic.
C
      CALL MAPPOS (.025,.24375,.63125,.85)
      CALL MAPROJ ('LC',30.,0.,45.)
      CALL MAPDRW
C
C Stereographic.
C
      CALL MAPPOS (.26875,.4875,.63125,.85)
      CALL MAPROJ ('ST',0.,0.,0.)
      CALL MAPDRW
C
C Orthographic.
C
      CALL MAPPOS (.5125,.73125,.63125,.85)
      CALL MAPROJ ('OR',0.,0.,0.)
      CALL MAPDRW
C
C Lambert equal-area.
C
      CALL MAPPOS (.75625,.975,.63125,.85)
      CALL MAPROJ ('LE',0.,0.,0.)
      CALL MAPDRW
C
C Gnomonic.
C
      CALL MAPPOS (.025,.24375,.3875,.60625)
      CALL MAPROJ ('GN',0.,0.,0.)
      CALL MAPDRW
C
C Azimuthal equidistant.
C
      CALL MAPPOS (.26875,.4875,.3875,.60625)
      CALL MAPROJ ('AE',0.,0.,0.)
      CALL MAPDRW
C
C Satellite-view.
C
      CALL MAPPOS (.5125,.73125,.3875,.60625)
      CALL MAPROJ ('SV',0.,0.,0.)
      CALL MAPSTR ('SA',2.)
      CALL MAPDRW
C
C Mercator.
C
      CALL MAPPOS (.75625,.975,.3875,.60625)
      CALL MAPROJ ('ME',0.,0.,0.)
      CALL MAPDRW
C
C Cylindrical equidistant.
C
      CALL MAPPOS (.025,.4875,.13125,.3625)
      CALL MAPROJ ('CE',0.,0.,0.)
      CALL MAPDRW
C
C Mollweide type.
C
      CALL MAPPOS (.5125,.975,.13125,.3625)
      CALL MAPROJ ('MO',0.,0.,0.)
      CALL MAPDRW
C
C Put the label at the top of the plot ...
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.925,PLBL,37,2,0,0)
C
C and the labels under each sub-plot.
C
      CALL PWRIT (.134375,.61875,'LAMBERT CONFORMAL CONIC',
     +                                                  23,0,0,0)
      CALL PWRIT (.378125,.61875,'STEREOGRAPHIC',13,0,0,0)
      CALL PWRIT (.621875,.61875,'ORTHOGRAPHIC',12,0,0,0)
      CALL PWRIT (.865625,.61875,'LAMBERT EQUAL-AREA',18,0,0,0)
      CALL PWRIT (.134375,.375,'GNOMONIC',8,0,0,0)
      CALL PWRIT (.378125,.375,'AZIMUTHAL EQUIDISTANT',21,0,0,0)
      CALL PWRIT (.621875,.375,'SATELLITE-VIEW',14,0,0,0)
      CALL PWRIT (.865625,.375,'MERCATOR',8,0,0,0)
      CALL PWRIT (.25625,.11875,'CYLINDRICAL EQUIDISTANT',
     +                                                  23,0,0,0)
      CALL PWRIT (.74375,.11875,'MOLLWEIDE TYPE',14,0,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
C This version of MAPEOD uses area identifiers for the outline
C dataset 'CO' to suppress all but the major global land masses.
C
      IF (IDLS.NE.  2.AND.IDRS.NE.  2) NPTS=0
C
      IF (IDLS.NE.  1.AND.IDRS.NE.  1.AND.
     +    IDLS.NE.  3.AND.IDRS.NE.  3.AND.
     +    IDLS.NE. 11.AND.IDRS.NE. 11.AND.
     +    IDLS.NE. 79.AND.IDRS.NE. 79.AND.
     +    IDLS.NE. 99.AND.IDRS.NE. 99.AND.
     +    IDLS.NE.104.AND.IDRS.NE.104.AND.
     +    IDLS.NE.107.AND.IDRS.NE.107.AND.
     +    IDLS.NE.163.AND.IDRS.NE.163     ) NPTS=0
C
C Done.
C
      RETURN
C
      END
.ef
.fi
.SK
.H 3 "Example 6"
Shows the effect of changing the rotation angle ROTA.
Note that the satellite is positioned over Kansas.
.SK
.in
.sf
.nf
      PROGRAM EXMPL6
C
C This program produces a single frame showing satellite views
C of the globe, each rotated by a different angle.
C
C Define the label for the top of the map.
C
      CHARACTER*27 PLBL
C
      DATA PLBL / 'THE EARTH IS SPINNING, TOTO' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','PS')
C
C Use a satellite-view projection.
C
      CALL MAPSTR ('SA',1.25)
C
C Aim the camera 15 degrees away from straight down.
C
      CALL MAPSTI ('S1',15)
C
C Turn off the perimeter and reduce the number of grid lines.
C
      CALL MAPSTI ('PE',0)
      CALL MAPSTI ('GR',15)
C
C Center the first map over Kansas.  Rotate by 0 degrees and look
C to the upper left.
C
      CALL MAPPOS (.05,.475,.525,.95)
      CALL MAPROJ ('SV',38.,-98.,0.)
      CALL MAPSTI ('S2',135)
      CALL MAPDRW
C
C Repeat, but rotate by 90 degrees and look to the upper right.
C
      CALL MAPPOS (.525,.95,.525,.95)
      CALL MAPROJ ('SV',38.,-98.,90.)
      CALL MAPSTI ('S2',45)
      CALL MAPDRW
C
C Repeat, but rotate by 180 degrees and look to the lower left.
C
      CALL MAPPOS (.05,.475,.05,.475)
      CALL MAPROJ ('SV',38.,-98.,180.)
      CALL MAPSTI ('S2',-135)
      CALL MAPDRW
C
C Repeat, but rotate by 270 degrees and look to the lower right.
C
      CALL MAPPOS (.525,.95,.05,.475)
      CALL MAPROJ ('SV',38.,-98.,270.)
      CALL MAPSTI ('S2',-45)
      CALL MAPDRW
C
C Put the label at the top of the plot ...
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,27,2,0,0)
C
C and the ones below each sub-plot.
C
      CALL PWRIT (.2625,.5,'ROTA = 0',8,1,0,0)
      CALL PWRIT (.7375,.5,'ROTA = 90',9,1,0,0)
      CALL PWRIT (.2625,.025,'ROTA = 180',10,1,0,0)
      CALL PWRIT (.7375,.025,'ROTA = 270',10,1,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
.ef
.fi
.SK
.H 3 "Example 7"
It is difficult to write a general algorithm for labeling the meridians and
parallels on any map.  The capability has therefore been omitted from EZMAP
proper and is left to the user.  This example presents a routine which will
label the meridians on certain kinds of maps.  Similar techniques enable
one to label other types of maps.
.SK
.in
.sf
.nf
      PROGRAM EXMPL7
C
C This program produces a stereographic view of the North Pole,
C with labeled meridians.
C
C Define the label for the top of the map.
C
      CHARACTER*32 PLBL
C
      DATA PLBL / 'MERIDIONAL LABELS ON A POLAR MAP' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Move the map a little to provide more room for labels.
C
      CALL MAPPOS (.075,.925,.05,.90)
C
C Use an elliptical (circular, in this case) perimeter.
C
      CALL MAPSTI ('EL',1)
C
C Show continents and international boundaries.
C
      CALL MAPSTC ('OU','PO')
C
C Use a stereographic projection, centered at the North Pole.
C
      CALL MAPROJ ('ST',90.,0.,-100.)
C
C Specify the angular distances to the edges of the map.
C
      CALL MAPSET ('AN',80.,80.,80.,80.)
C
C Draw the map.
C
      CALL MAPDRW
C
C Call a routine to label the meridians.  This routine is not
C a part of EZMAP; the code is given below.
C
      CALL MAPLBM
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,32,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
      SUBROUTINE MAPLBM
C
C This routine labels the meridians if and only if the current
C projection is azimuthal and centered at one of the poles, a
C circular boundary is being used, and the grid increment is an
C integral divisor of 180.  The routine was not thought general
C enough to include in EZMAP itself, but may nevertheless be of
C interest to users.
C
C Necessary local declarations.
C
      CHARACTER*2 PROJ
      CHARACTER*3 CHRS
      CHARACTER*4 CHLB
C
C See if the conditions required for MAPLBM to work are met.
C
C The projection must be azimuthal, ...
C
      CALL MAPGTC ('PR',PROJ)
      IF (PROJ.NE.'ST'.AND.PROJ.NE.'OR'.AND.PROJ.NE.'LE'.AND.
     +    PROJ.NE.'GN'.AND.PROJ.NE.'AE') RETURN
C
C the pole latitude must be +90 degrees or -90 degrees, ...
C
      CALL MAPGTR ('PT',PLAT)
      IF (ABS(PLAT).LT.89.9999) RETURN
C
C the perimeter must be elliptical, ...
C
      CALL MAPGTI ('EL',IELP)
      IF (IELP.EQ.0) RETURN
C
C the values used in the SET call must define a circle, ...
C
      CALL GETSET (FLEW,FREW,FBEW,FTEW,ULEW,UREW,VBEW,VTEW,LNLG)
      ILEW=KFPX(FLEW)
      IREW=KFPX(FREW)
      IBEW=KFPY(FBEW)
      ITEW=KFPY(FTEW)
      IF (ULEW+UREW.GT.0.0001.OR.VBEW+VTEW.GT.0.0001) RETURN
      IF (ULEW+VTEW.GT.0.0001.OR.VBEW+UREW.GT.0.0001) RETURN
C
C and the grid spacing must be an integral divisor of 180.
C
      CALL MAPGTR ('GR',GRID)
      IF (AMOD(GRID,1.).NE.0.OR.
     +    MOD(180,IFIX(GRID)).NE.0) RETURN
C
C All conditions are satisfied.  Label the meridians.
C
C Collect the necessary information.
C
      IGRD=GRID
      CALL MAPGTR ('PN',PLON)
      CALL MAPGTR ('RO',ROTA)
      CALL MAPGTI ('LS',ICSZ)
      IF (ICSZ.EQ.0) THEN
        ICSZ=8
      ELSE IF (ICSZ.EQ.1) THEN
        ICSZ=12
      ELSE IF (ICSZ.EQ.2) THEN
        ICSZ=16
      ELSE IF (ICSZ.EQ.3) THEN
        ICSZ=24
      END IF
      WOCH=(FLOAT(  ICSZ)/FLOAT(IREW-ILEW))*(UREW-ULEW)
      HOCH=(FLOAT(2*ICSZ)/FLOAT(ITEW-IBEW))*(VTEW-VBEW)
      HOLB=HOCH/1.5
C
C Loop on the label values.
C
      DO 101 I=-180,179,IGRD
C
C Express the value of the longitude in a nice form.
C
        WRITE (CHRS,1001) IABS(I)
        NCHS=0
        IF (IABS(I).GE.100) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)=CHRS(1:1)
        END IF
        IF (IABS(I).GE.10) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)=CHRS(2:2)
        END IF
        NCHS=NCHS+1
        CHLB(NCHS:NCHS)=CHRS(3:3)
        IF (I.GT.-180.AND.I.LT.0) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)='W'
        ELSE IF (I.GT.0.AND.I.LT.180) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)='E'
        END IF
C
C Compute the width of the label.
C
       WOLB=FLOAT(NCHS)*WOCH
C
C Find the angle at which the labeled meridian lies on the plot.
C
        IF (PLAT.GT.0.) THEN
          ANGD=FLOAT(I-90)-PLON-ROTA
        ELSE
          ANGD=FLOAT(90-I)+PLON-ROTA
        END IF
C
C Reduce the angle to the range from -180 to +180 and
C find its equivalent in radians.
C
        ANGD=ANGD-SIGN(180.,ANGD+180.)
     +           +SIGN(180.,180.-ANGD)
        ANGR=.017453292519943*ANGD
C
C Figure out where the end of the meridian is.
C
        XEND=UREW*COS(ANGR)
        YEND=VTEW*SIN(ANGR)
C
C Extend the meridian a little to make a tick mark.
C
        CALL LINE (XEND,YEND,1.015*XEND,1.015*YEND)
C
C Compute a center position for the label which puts its nearest
C edge at a fixed distance from the perimeter.  First, compute
C the components (DELX,DELY) of the vector from the center of the
C label box to the edge nearest the perimeter.
C
        IF      (ANGD.LT.-179.9999) THEN
          DELX=+0.5*WOLB
          DELY= 0.
        ELSE IF (ANGD.LT. -90.0001) THEN
          DELX=+0.5*WOLB
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT. -89.9999) THEN
          DELX= 0.0
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT.  -0.0001) THEN
          DELX=-0.5*WOLB
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT.  +0.0001) THEN
          DELX=-0.5*WOLB
          DELY= 0.0
        ELSE IF (ANGD.LT. +89.9999) THEN
          DELX=-0.5*WOLB
          DELY=-0.5*HOLB
        ELSE IF (ANGD.LT. +90.0001) THEN
          DELX= 0.0
          DELY=-0.5*HOLB
        ELSE IF (ANGD.LT.+179.9999) THEN
          DELX=+0.5*WOLB
          DELY=-0.5*HOLB
        ELSE
          DELX=+0.5*WOLB
          DELY= 0.0
        END IF
C
C Then, solve (for FMUL) the following equation:
C
C   SQRT((FMUL*XEND+DELX)**2+(FMUL*YEND+DELY)**2))=1.02*UREW
C
C which expresses the condition that the corner of the box
C nearest the circular perimeter should be at a distance of
C 1.02*(the radius of the perimeter) away from the center of
C the plot.
C
        A=XEND*XEND+YEND*YEND
        B=2.*(XEND*DELX+YEND*DELY)
        C=DELX*DELX+DELY*DELY-1.0404*UREW*UREW
C
        FMUL=(-B+SQRT(B*B-4.*A*C))/(2.*A)
C
C Draw the label.
C
        CALL PWRIT (FMUL*XEND,FMUL*YEND,CHLB,NCHS,ICSZ,0,0)
C
C End of loop.
C
  101 CONTINUE
C
C Done.
C
      RETURN
C
C Format
C
 1001 FORMAT (I3)
C
      END
.ef
.fi
.SK
.H 3 "Example 8"
This example demonstrates how a user-provided routine MAPUSR may be employed
to change the way in which portions of a map are drawn.
.SK
.in
.sf
.nf
      PROGRAM EXMPL8
C
C Produce a Mercator projection of the whole globe, using a
C version of MAPUSR which dots the grid lines and dashes the
C continental outlines.
C
C Define the label for the top of the map.
C
      CHARACTER*30 PLBL
C
      DATA PLBL / 'ILLUSTRATING THE USE OF MAPUSR' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Weird up the projection a little.
C
      CALL SUPMAP (9,0.,0.,90.,0.,0.,0.,0.,1,15,2,0,IERR)
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,30,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
      SUBROUTINE MAPUSR (IPRT)
C
C This version of MAPUSR forces the grid lines to be dotted and
C the outlines to be dashed.
C
C Certain local parameters must be saved from call to call.
C
      SAVE INTN,IDTF,IDBD
C
C If IPRT is positive, a part is about to be drawn.  Save the
C dotted/solid flag and/or the distance between dots and then
C reset them and/or the dash pattern.
C
      IF (IPRT.GT.0) THEN
        IF (IPRT.EQ.2) THEN
          CALL MAPGTI ('DL',IDTF)
          CALL MAPGTI ('DD',IDBD)
          CALL MAPSTI ('DL',1)
	  CALL MAPSTI ('DD',3)
        ELSE IF (IPRT.EQ.5) THEN
          CALL MAPGTI ('DL',IDTF)
          CALL MAPSTI ('DL',0)
	  CALL DASHDB (52525B,0,0,0)
        END IF
C
      ELSE
C
C Otherwise, a part has just been drawn.  Restore saved settings
C and/or select a solid dash pattern.
C
        IF (IPRT.EQ.-2) THEN
          CALL MAPSTI ('DL',IDTF)
          CALL MAPSTI ('DD',IDBD)
        ELSE IF (IPRT.EQ.-5) THEN
          CALL MAPSTI ('DL',IDTF)
	  CALL DASHDB (17777B,0,0,0)
        END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
.ef
.fi
.SK
.H 3 "Example 9"
The four plots produced show the numbering of segments in the outline
dataset 'CO'.  This information may be used to produce simplified
continental outlines; see examples 3 and 5 for another way.
.P
The program can produce segment-number plots for the other outline datasets,
also.
.P
The algorithm used to prevent numbers from landing on top of one another
doesn't prevent them from landing on top of the outlines, which makes some of
them hard to read.  (The original is considerably better than the
reproduction.)
.P
The technique used to break the maps into rectangular pieces and
present each piece individually may be useful in its own right.
.SK 3
.in
.sf
.nf
      PROGRAM EXMPL9
C
C The program EXMPL9 produces a set of plots showing the numbers
C of all the segments in a chosen EZMAP outline dataset.  Certain
C variables must be set in data statements at the beginning of
C the program.  In each of the seven places marked off by rows of
C dashes, un-comment the first card to do outline dataset 'CO',
C the second to do 'US', the third to do 'PO', and the fourth to
C do 'PS'.
C
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Select the outline dataset.
C
      CHARACTER*2 OUTD
C
C-----------------------
      DATA OUTD / 'CO' /
C     DATA OUTD / 'US' /
C     DATA OUTD / 'PO' /
C     DATA OUTD / 'PS' /
C-----------------------
C
C Select the projection type.
C
      CHARACTER*2 PROJ
C
C-----------------------
      DATA PROJ / 'ME' /
C     DATA PROJ / 'LC' /
C     DATA PROJ / 'ME' /
C     DATA PROJ / 'ME' /
C-----------------------
C
C Select the appropriate values of PLAT, PLON, and ROTA.
C
C------------------------------------------
      DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C     DATA PLAT,PLON,ROTA / 30.,-100.,45. /
C     DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C     DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C------------------------------------------
C
C Select the parameter saying how the map limits are chosen.
C
      CHARACTER*2 LMTS
C
C-----------------------
      DATA LMTS / 'MA' /
C     DATA LMTS / 'CO' /
C     DATA LMTS / 'MA' /
C     DATA LMTS / 'MA' /
C-----------------------
C
C Select the values to be put in the limits arrays.
C
      DIMENSION PL1(2),PL2(2),PL3(2),PL4(2)
C
C---------------------------------------------------------------
      DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) / 22.6,-120.,46.9,-64.2 /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C---------------------------------------------------------------
C
C---------------------------------------------------------------
      DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C---------------------------------------------------------------
C
C Select values determining how the whole map is to be carved
C up into little maps.  ILIM is the number of divisions of the
C horizontal axis, JLIM the number of divisions of the vertical
C axis.  (If all the labels are put on a single map, the result
C is confusing.)
C
C---------------------------
      DATA ILIM,JLIM / 2,2 /
C     DATA ILIM,JLIM / 3,2 /
C     DATA ILIM,JLIM / 2,2 /
C     DATA ILIM,JLIM / 6,6 /
C---------------------------
C
C Define a variable to hold the plot label.
C
      CHARACTER*38 PLBL
C
      DATA PLBL / 'SEGMENT NUMBERS FOR OUTLINE DATASET XX' /
C
C Open GKS.
C
      CALL OPNGKS
C
C Finish the plot label.
C
      PLBL(37:38)=OUTD
C
C Set the character size; 6 is about the smallest usable value.
C
      ICSZ=6
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU',OUTD)
C
C Set the projection-type parameters.
C
      CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C Set the limits parameters.
C
      CALL MAPSET (LMTS,PL1,PL2,PL3,PL4)
C
C Initialize EZMAP.
C
      CALL MAPINT
C
C Retrieve the values with which MAPINT called SET.
C
      CALL GETSET (FLEM,FREM,FBEM,FTEM,ULEM,UREM,VBEM,VTEM,LNLG)
      ILEM=KFPX(FLEM)
      IREM=KFPX(FREM)
      IBEM=KFPY(FBEM)
      ITEM=KFPY(FTEM)
C
C Now, plot a set of maps which are subsets of the whole map.
C
      DO 102 I=1,ILIM
        DO 101 J=1,JLIM
          CALL MAPSET ('LI',
     +                 ULEM+(UREM-ULEM)*FLOAT(I-1)/FLOAT(ILIM),
     +                 ULEM+(UREM-ULEM)*FLOAT(I  )/FLOAT(ILIM),
     +                 VBEM+(VTEM-VBEM)*FLOAT(J-1)/FLOAT(JLIM),
     +                 VBEM+(VTEM-VBEM)*FLOAT(J  )/FLOAT(JLIM))
C
C Re-initialize EZMAP with the new limits.
C
          CALL MAPINT
C
C Retrieve the values with which MAPINT called SET.
C
	  CALL GETSET (FLEW,FREW,FBEW,FTEW,
     +                 ULEW,UREW,VBEW,VTEW,LNLG)
      ILEW=KFPX(FLEW)
      IREW=KFPX(FREW)
      IBEW=KFPY(FBEW)
      ITEW=KFPY(FTEW)
C
C Compute quantities required by MAPEOD and MOVEIT to position
C labels.
C
          DELU=3.5*(FLOAT(ICSZ)/FLOAT(IREW-ILEW))*(UREW-ULEW)
          DELV=2.0*(FLOAT(ICSZ)/FLOAT(ITEW-IBEW))*(VTEW-VBEW)
          NSAV=0
C
C Draw the outlines.
C
          CALL MAPLOT
C
C Put a label at the top of the plot.
C
          CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
          CALL PWRIT (.5,.975,PLBL,38,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
          CALL BNDARY
C
C Advance the frame.
C
          CALL FRAME
C
  101   CONTINUE
  102 CONTINUE
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
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
C This version of MAPEOD marks each segment of the map with its
C segment number.  The resulting map may be used to set up other
C versions of MAPEOD which plot selected segments.
C
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Define local variables to hold the character-string form of the
C segment number.
C
      CHARACTER*4 CSG1,CSG2
C
C Find out where on the map the center of the segment is.
C
      MPTS=NPTS/2+1
      CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCEN,VCEN)
C
C If the center is visible, label it.
C
      IF (UCEN.GE.ULEW.AND.UCEN.LE.UREW.AND.
     +    VCEN.GE.VBEW.AND.VCEN.LE.VTEW) THEN
C
C Generate a character string representing the value of the
C segment number.
C
        WRITE (CSG2,1001) NSEG
        NFCH=4-IFIX(ALOG10(FLOAT(NSEG)+.5))
        NCHS=5-NFCH
        CSG1=CSG2(NFCH:4)
C
C Find out where the two points on either side of the center are.
C
        MPTS=MAX0(NPTS/2,1)
        CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCM1,VCM1)
C
        MPTS=MIN0(NPTS/2+2,NPTS)
        CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCP1,VCP1)
C
C Compute the preferred position of the label, with one corner
C of its enclosing box at the center of the segment.
C
        ULAB=UCEN-SIGN(.1428*DELU*FLOAT(NCHS),UCM1+UCP1-2*UCEN)
        VLAB=VCEN-SIGN(.3333*DELV,VCM1+VCP1-2*VCEN)
C
C Move the label as necessary to avoid its being on top of any
C previous label.
C
        CALL MOVEIT (ULAB,VLAB)
C
C Write out the character string and connect it to the segment
C with a straight line.
C
        CALL LINE (UCEN,VCEN,
     +             ULAB-SIGN(.1428*DELU*FLOAT(NCHS),ULAB-UCEN),
     +             VLAB-SIGN(.3333*DELV,VLAB-VCEN))
C
        CALL PWRIT (ULAB,VLAB,CSG1,NCHS,ICSZ,0,0)
C
      END IF
C
C Done.
C
      RETURN
C
C Format.
C
 1001 FORMAT (I4)
C
      END
      SUBROUTINE MOVEIT (ULAB,VLAB)
C
C The object of this routine is to avoid putting segment labels
C on top of each other.  (ULAB,VLAB), on entry, is the most
C desirable position for a segment label.  MOVEIT modifies this
C position as necessary to make sure the label will not be on top
C of any previous label.
C
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
C ULEW, UREW, VBEW, and VTEW specify the u/v limits of the window
C in which the map was drawn.  DELU and DELV are the minimum
C allowable distances between segment-label centers in the u
C and v directions, respectively.  NSAV is the number of label
C positions saved in the arrays USAV and VSAV.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Previous label positions are saved in the arrays USAV and VSAV.
C
      DIMENSION USAV(1000),VSAV(1000)
C
C USAV and VSAV must maintain their values between calls.
C
      SAVE USAV,VSAV
C
C Zero the variables which control the generation of a spiral.
C
      IGN1=0
      IGN2=2
C
C Check for overlap at the current position.
C
  101 DO 102 I=1,NSAV
        IF (ABS(ULAB-USAV(I)).LT.DELU.AND.
     +      ABS(VLAB-VSAV(I)).LT.DELV) GO TO 103
  102 CONTINUE
C
C No overlap.  Save the new label position and return to caller.
C
      NSAV=NSAV+1
      IF (NSAV.GT.1000) STOP 1
      USAV(NSAV)=ULAB
      VSAV(NSAV)=VLAB
C
      RETURN
C
C Overlap.  Try a new point.  The points tried form a spiral.
C
  103 IGN1=IGN1+1
      IF (IGN1.LE.IGN2/2) THEN
        ULAB=ULAB+SIGN(DELU,-.5+FLOAT(MOD(IGN2/2,2)))
      ELSE
        VLAB=VLAB+SIGN(DELV,-.5+FLOAT(MOD(IGN2/2,2)))
      END IF
      IF (IGN1.EQ.IGN2) THEN
        IGN1=0
        IGN2=IGN2+2
      END IF
      IF (ULAB.LT.ULEW.OR.ULAB.GT.UREW.OR.
     +    VLAB.LT.VBEW.OR.VLAB.GT.VTEW) GO TO 103
      GO TO 101
C
      END
.ef
.fi
.SK
.H 3 "A Final Example"
On a satellite-view projection of the North Atlantic appears an
appropriate message.  Note the use of user-drawn dotted lines.
.SK
.in
.sf
.nf
      PROGRAM EXMPLF
C
C Define a data array.
C
      DIMENSION XYCD(224)
C
C Define the centers and the expansion/shrinkage factors for
C various copies of the curve to be drawn.
C
      DIMENSION FLAC(4),FLOC(4),FMUL(4)
C
      DATA FLOC(1),FLAC(1),FMUL(1) / -38.1,32.0,1.7 /
      DATA FLOC(2),FLAC(2),FMUL(2) / -37.9,32.0,1.7 /
      DATA FLOC(3),FLAC(3),FMUL(3) / -38.0,31.9,1.7 /
      DATA FLOC(4),FLAC(4),FMUL(4) / -38.0,32.1,1.7 /
C
C Open GKS.
C
      CALL OPNGKS
C
C Fill the data array.
C
      READ 1001 , XYCD
C
C Define the altitude of the satellite.
C
      CALL MAPSTR ('SA',2.)
C
C Draw a map of the North Atlantic, as seen by a satellite.
C
      CALL SUPMAP (7,32.,-38.,20.,0.,0.,0.,0.,1,-1000,5,0,IERR)
C
C Force MAPIT to draw dotted lines.
C
      CALL MAPSTI ('DL',1)
      CALL MAPSTI ('DD',3)
C
C Draw some curves.
C
      DO 102 I=1,4
C
        IFST=0
C
        DO 101 J=1,112
          IF (XYCD(2*J-1).EQ.0.) THEN
            IFST=0
          ELSE
            FLON=FLOC(I)+FMUL(I)*(XYCD(2*J-1)-15.)
            FLAT=FLAC(I)+FMUL(I)*(XYCD(2*J  )-15.)
            CALL MAPIT (FLAT,FLON,IFST)
            IFST=1
          END IF
  101   CONTINUE
C
  102 CONTINUE
C
C Dump MAPIT's buffers.
C
      CALL MAPIQ
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
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
C Format.
 1001 FORMAT (14E5.0)
C
      END
.fi
.ef
