C
C	$Id: cmpgrp.f,v 1.1 1993-01-13 17:59:51 haley Exp $
C
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

	DATA PLIM1 /0.,0./
	DATA PLIM2 /0.,0./
	DATA PLIM3 /0.,0./
	DATA PLIM4 /0.,0./
C
C Open GKS, Turn Clipping off
C
	CALL OPNGKS 
C
C INVOKE DEMO DRIVER
C
	CALL CMPFIL('SV',40.,-50.,0.,'PO',
     +		'MA',PLIM1,PLIM2,PLIM3,PLIM4,10.)
C
C Advance the frame.
C
	CALL FRAME
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
	CALL CLSGKS

	STOP
	END

	SUBROUTINE CMPFIL(PROJ, PLAT, PLON, ROTA, OUTLN,
     +		JLIM, PLIM1, PLIM2, PLIM3, PLIM4, GRD)

	EXTERNAL MASK
	EXTERNAL FILL

	PARAMETER (LMAP=150000,NWRK=10000,ISIZ=5)
	CHARACTER*2 PROJ, OUTLN, JLIM
	INTEGER MAP(LMAP), IAREA(ISIZ), IGRP(ISIZ)
	REAL XWRK(NWRK), YWRK(NWRK)
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)
C
C CMPLOT demonstrates MAPLOT drawing continental and political outlines
C
C Set up Maps.
C
	CALL COLOR
	CALL DASHDB(65535)
C
C Set up 9 vertical strips
C
	CALL MAPSTI ('VS - VERTICAL STRIPS',9)
C
C Draw Continental, political outlines 
C
	CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR',OUTLN)
C
C Set grid spacing
C
	CALL MAPSTR ('GR - GRID SPACING',GRD)
C
C Set up projection
C
	CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C If it's a satellite projection, choose a satellite distance
C
	IF (PROJ.EQ.'SV') CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
C
C Set limits of map
C
	CALL MAPSET (JLIM,PLIM1,PLIM2,PLIM3,PLIM4)
C
C Initialize Maps and Areas
C
	CALL MAPINT
	CALL ARINAM (MAP,LMAP)
	CALL MAPBLA (MAP)
C
C Color fill each country a different color
C
	CALL GSFAIS (1)
	CALL ARSCAM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, FILL)
C
C Draw Masked Grid Lines
C
	CALL MAPGRM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, MASK)
C
C Draw Continental Outlines
C
	CALL MAPSTI('LA - LABEL FLAG',0)
	CALL MAPSTI('EL - ELLIPTICAL-PERIMETER SELECTOR',1)
	CALL MAPLBL
	CALL MAPLOT
C
C Done.
C
	RETURN
	END

	SUBROUTINE MASK(XC,YC,MCS,AREAID,GRPID,IDSIZE)

	INTEGER AREAID(IDSIZE),GRPID(IDSIZE),ID
	REAL XC(MCS),YC(MCS)

C Retrieve area id for geographical area
	DO 10, I=1,IDSIZE
	   IF (GRPID(I).EQ.1) ID=AREAID(I)
 10	CONTINUE
C If the line is over water, and has 2 or more points draw it.
	IF ((MAPACI(ID).EQ.1).AND.(MCS.GE.2)) THEN
	   CALL CURVED(XC,YC,MCS)
	ENDIF
	
C Otherwise, don't draw the line - mask it.

	RETURN
	END
	SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,IDSIZ)
C
	DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)

C Check if the area is over the map
	IF ((IAREA(1) .GE. 0) .AND. (IAREA(2) .GE. 0)) THEN
	   ITMP=MAX0(IAREA(1),IAREA(2))
	   CALL GSFACI(MAPACI(ITMP)+1)
	   CALL GFA(NWRK,XWRK,YWRK)
	ENDIF
	
C Otherwise, do nothing

	RETURN
	END
      SUBROUTINE COLOR
C
C     BACKGROUND COLOR
C     BLACK
      CALL GSCR(1,0,0.,0.,0.)
C
C     FORGROUND COLORS
C White
      CALL GSCR(1,  1, 1.0, 1.0, 1.0)
C Aqua
      CALL GSCR(1,  2, 0.0, 0.9, 1.0)
C Red
      CALL GSCR(1,  3, 0.9, 0.25, 0.0)
C OrangeRed
      CALL GSCR(1,  4, 1.0, 0.0, 0.2)
C Orange
      CALL GSCR(1,  5, 1.0, 0.65, 0.0)
C Yellow
      CALL GSCR(1,  6, 1.0, 1.0, 0.0)
C GreenYellow
      CALL GSCR(1,  7, 0.7, 1.0, 0.2)
C Chartreuse
      CALL GSCR(1,  8, 0.5, 1.0, 0.0)
C Celeste
      CALL GSCR(1,  9, 0.2, 1.0, 0.5)
C Green
      CALL GSCR(1, 10, 0.2, 0.8, 0.2)
C DeepSkyBlue
      CALL GSCR(1, 11, 0.0, 0.75, 1.0)
C RoyalBlue
      CALL GSCR(1, 12, 0.25, 0.45, 0.95)
C SlateBlue
      CALL GSCR(1, 13, 0.4, 0.35, 0.8)
C DarkViolet
      CALL GSCR(1, 14, 0.6, 0.0, 0.8)
C Orchid
      CALL GSCR(1, 15, 0.85, 0.45, 0.8)
C Lavender
      CALL GSCR(1, 16, 0.8, 0.8, 1.0)
C Gray
      CALL GSCR(1, 17, 0.7, 0.7, 0.7)
C Done.
C
        RETURN
C
      END

