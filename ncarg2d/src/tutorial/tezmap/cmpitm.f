
      PROGRAM CMPITM
C
C This program draws a map, then it draws the projection of a circle, 
C which is masked over land.
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
C
C The arrays CLAT and CLON will be used to hold lat/lon pairs defining
C the desired circle.
C
      EXTERNAL MASK1
      PARAMETER (NPTS=361,NWRK=5000,LMAP=150000,NGRPS=2)

      REAL CLAT(NPTS), CLON(NPTS), XWRK(NWRK), YWRK(NWRK)
      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)
      INTEGER MAP(LMAP),IAREA(NGRPS),IGRP(NGRPS)

      DATA PLIM1 /20.,0./
      DATA PLIM2 /-85.,0./
      DATA PLIM3 /30.,0./
      DATA PLIM4 /-75.,0./
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Draw the map, and set up area map.
C
      CALL TCMPMSK('ME',0.,-75.,0.,'PS','CO',
     +     PLIM1,PLIM2,PLIM3,PLIM4,2.,MAP,LMAP)
C
C Define a circle centered at RLAT,RLON
C
      CALL CIRCLE (25.,-80.,3.5,CLAT,CLON,NPTS)
C
C Draw the circle on the map.
C
      CALL MAPITM  (CLAT(1),CLON(1),0,MAP,XWRK,YWRK,NWRK,
     +     IAREA,IGRP,NGRPS,MASK1)
      DO 102 ICIR=2,NPTS
         CALL MAPITM (CLAT(ICIR),CLON(ICIR),1,MAP,XWRK,YWRK,NWRK,
     +        IAREA,IGRP,NGRPS,MASK1)
 102  CONTINUE
      CALL MAPIQM (MAP,XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS,MASK1)
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END

      SUBROUTINE CIRCLE (RLAT,RLON,RADIUS,CLAT,CLON,NPTS)
C
C Points on a circle around the North Pole are rotated to lie around
C the point at latitude RLAT and longitude RLON.
C RADIUS is the radius of the circle, in degrees.
C Return the lat/lon coordinates of the desired circle in CLAT & CLON.
C
      REAL RLAT, RLON, CLAT(NPTS), CLON(NPTS)
C
C DTOR and RTOD are the constants used to get from degrees to radians
C and vice-versa.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
      DO 101 ICIR=1,NPTS
         ALAT=90.-RADIUS
         ALON=REAL(ICIR-1)
         UCRD=COS(DTOR*ALAT)*COS(DTOR*ALON)
         VCRD=COS(DTOR*ALAT)*SIN(DTOR*ALON)
         WCRD=SIN(DTOR*ALAT)
         IF (RLAT.GE.0) THEN
            CALL ROTATE (2,90.-RLAT,UCRD,VCRD,WCRD)
            CALL ROTATE (3,RLON,UCRD,VCRD,WCRD)
         ELSE
            CALL ROTATE (2,90.+ABS(RLAT),UCRD,VCRD,WCRD)
            CALL ROTATE (3,RLON,UCRD,VCRD,WCRD)
         ENDIF 
         CLAT(ICIR)=RTOD*ASIN(WCRD)
         CLON(ICIR)=RTOD*ATAN2(VCRD,UCRD)
 101  CONTINUE
      
      RETURN
      END
      SUBROUTINE ROTATE (IAXS,ANGL,UCRD,VCRD,WCRD)
C
C This routine rotates the point with coordinates (UCRD,VCRD,WCRD)
C by the angle ANGL about the axis specified by IAXS (1 for the
C U axis, 2 for the V axis, 3 for the W axis).  The coordinate
C system is assumed to be a right-handed one.
C
      SINA=SIN(.017453292519943*ANGL)
      COSA=COS(.017453292519943*ANGL)
C
      UTMP=UCRD
      VTMP=VCRD
      WTMP=WCRD
C
      IF (IAXS.EQ.1) THEN
         VCRD=VTMP*COSA-WTMP*SINA
         WCRD=WTMP*COSA+VTMP*SINA
      ELSE IF (IAXS.EQ.2) THEN
         UCRD=UTMP*COSA+WTMP*SINA
         WCRD=WTMP*COSA-UTMP*SINA
      ELSE
         UCRD=UTMP*COSA-VTMP*SINA
         VCRD=VTMP*COSA+UTMP*SINA
      END IF
C
      RETURN
C
      END
      SUBROUTINE TCMPMSK(PROJ, PLAT, PLON, ROTA, OUTLN,
     +     JLIM, PLIM1, PLIM2, PLIM3, PLIM4, GRD, MAP, LMAP)

      EXTERNAL MASK2
      PARAMETER (NWRK=5000,ISIZ=2)

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
C Draw Masked Grid Lines
C
      CALL MAPGRM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, MASK2)
C
C Draw Continental Outlines and Perimeter
C
      CALL MAPLBL
      CALL MAPLOT
C
C Done.
C
      RETURN
      END

      SUBROUTINE MASK1(XC,YC,MCS,AREAID,GRPID,IDSIZE)

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
      SUBROUTINE MASK2(XC,YC,MCS,AREAID,GRPID,IDSIZE)

      INTEGER AREAID(IDSIZE),GRPID(IDSIZE),ID
      REAL XC(MCS),YC(MCS)

      CALL DASHDB (29298)
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

