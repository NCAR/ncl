C
C	$Id: cmplab.f,v 1.2 1992-10-01 21:56:20 ncargd Exp $
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
      CALL CMPLAB('SV',40.,-50.,0.,'PO','MA',0., 0.,0.,0.,10.)
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

      SUBROUTINE CMPLAB(PROJ, PLAT, PLON, ROTA, OUTLN,
     +	JLIM, PLIM1, PLIM2, PLIM3, PLIM4, GRD)

	EXTERNAL MASK

	PARAMETER (LMAP=150000,NWRK=1000,ISIZ=5)

	CHARACTER*2 PROJ, OUTLN, JLIM
	INTEGER MAP(LMAP), IAREA(ISIZ), IGRP(ISIZ)
	REAL XWRK(NWRK), YWRK(NWRK)
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)
C
C CMPLOT demonstrates MAPLOT drawing continental and political outlines
C
C Use solid lines for grid.
C
	CALL DASHDB(65535)
C
C Draw Continental, political outlines 
C
        CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR',OUTLN)
C
C Set grid spacing
C
	CALL MAPSTR ('GR - GRID SPACING',GRD)
C 
C Set Label Size
C
	CALL MAPSTI ('LS - LABEL SIZE',20)
C
C Set up projection
C
	CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C If it's a satellite projection, choose a satellite distance
C
	IF (PROJ.EQ.'SV') CALL MAPSTR ('SA - SATELLITE DISTANCE',7.)
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
C Draw Masked Grid Lines
C
	CALL MAPGRM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, MASK)
C
C Draw Continental Outlines, Labels and Elliptical Perimeter
C
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
