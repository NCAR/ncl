
      PROGRAM CMPMSK
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)

      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

      DATA PLIM1 /0.,0./
      DATA PLIM2 /0.,0./
      DATA PLIM3 /0.,0./
      DATA PLIM4 /0.,0./
C
C Open GKS, Turn Clipping off
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver
C
      CALL TCMPMSK('SV',40.,-50.,0.,'PO','MA',
     +     PLIM1,PLIM2,PLIM3,PLIM4,10.)
C
C Advance the frame.
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END

      SUBROUTINE TCMPMSK(PROJ, PLAT, PLON, ROTA, OUTLN,
     +     JLIM, PLIM1, PLIM2, PLIM3, PLIM4, GRD)

      EXTERNAL MASK
      PARAMETER (LMAP=150000,NWRK=1000,ISIZ=5)
      
      CHARACTER*2 PROJ, OUTLN, JLIM
      INTEGER MAP(LMAP), IAREA(ISIZ), IGRP(ISIZ)
      REAL XWRK(NWRK), YWRK(NWRK)
      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)
C
C CMPLOT demonstrates MAPLOT drawing continental and political outlines
C
C Use solid lines for grid
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
C Draw Continental Outlines and Elliptical Perimeter
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
C
C Retrieve area id for geographical area
C
      DO 10, I=1,IDSIZE
         IF (GRPID(I).EQ.1) ID=AREAID(I)
 10   CONTINUE
C
C If the line is over water, and has 2 or more points draw it.
C
      IF ((MAPACI(ID).EQ.1).AND.(MCS.GE.2)) THEN
         CALL CURVED(XC,YC,MCS)
      ENDIF
C   
C Otherwise, don't draw the line - mask it.
C
      RETURN
      END
