
      PROGRAM FFEX00
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
C Draws a uniform field over an azimuthal projection of the globe
C Continental boundaries are filled with a grayscale value
C
      EXTERNAL FILL
C
      PARAMETER (LMAP=200000,NWRK=20000,ISIZ=5)
      INTEGER MAP(LMAP), IAREA(ISIZ), IGRP(ISIZ)
      REAL XWRK(NWRK), YWRK(NWRK)
C
      PARAMETER (M=18 , N=18)
      DIMENSION A(M,N),B(M,N),WRK(M*N*2)
      PARAMETER (NCLRS=4)
      DIMENSION ICLR(NCLRS)
C
      DIMENSION RGBV(3,NCLRS)
      COMMON /CBFILL/ IFILIX
C
      DATA ICLR / 2, 3, 64, 196 /
      DATA RGBV / 
     +     0.0,1.0,1.0,
     +     0.75,0.0,1.0,
     +     0.75,0.75,0.75,
     +     0.5,0.5,0.5 /
C
C Give initial value to fill color index stored common block CBFILL
C
      IFILIX = 196
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up colors for fixed table grayscale and color workstations
C
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,1.,1.)
      DO 100 I=1,NCLRS,1
         CALL GSCR(IWKID,ICLR(I),RGBV(1,I),RGBV(2,I),RGBV(3,I))
 100  CONTINUE
C     
C Generate uniform field intended for polar input mode.
C
      DO 2 I=1,M
         DO 1 J=1,N
            A(I,J) = 1.0
            B(I,J) = 45.0
 1       CONTINUE
 2    CONTINUE
C
C Set up the map projection
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','CO')
      CALL MAPROJ ('AE',0.0,0.0,0.0)
      CALL MAPSET ('MA',0.0,0.0,0.0,0.0)
C
C Initialize Maps and Areas
C
      CALL MAPINT
      CALL ARINAM (MAP,LMAP)
      CALL MAPBLA (MAP)
C    
C Color fill land masses using a gray scale value
C
      CALL GSFAIS (1)
      CALL ARSCAM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, FILL)
C
C Draw boundaries, including the limb
C
      CALL GSPLCI(ICLR(3))
      CALL MAPSTI ('C4 - LIMB COLOR',ICLR(3))
      CALL MAPSTI('LA - LABEL FLAG',0)
      CALL MAPSTI('EL - ELLIPTICAL-PERIMETER SELECTOR',1)
      CALL MAPLBL
      CALL MAPLOT
C
C Set up Streamline parameters
C
      CALL STSETI('MAP -- Mapping Flag', 1)
      CALL STSETI('SET -- Do Set Call Flag', 0)
      CALL STSETR('XC1 -- Lower X Bound', -180.0)
      CALL STSETR('XCM -- Upper X Bound', 180.0)
      CALL STSETR('YC1 -- Lower Y Bound', -90.0)
      CALL STSETR('YCN -- Upper Y Bound', 90.0)
      CALL STSETI('PLR -- Streamline Polar Flag', 1)
      CALL STSETI('TRP -- Interpolation Method', 1)
      CALL STSETR('SSP -- Stream Spacing', 0.005)
      CALL STSETR('DFM - Differential Magnitude', 0.005)
C
C Set up Vectors parameters
C
      CALL VVSETI('MAP -- Mapping Flag', 1)
      CALL VVSETI('SET -- Do Set Call Flag', 0)
      CALL VVSETR('XC1 -- Lower X Bound', -180.0)
      CALL VVSETR('XCM -- Upper X Bound', 180.0)
      CALL VVSETR('YC1 -- Lower Y Bound', -90.0)
      CALL VVSETR('YCN -- Upper Y Bound', 90.0)
      CALL VVSETI('PLR -- Vector Polar Flag', 1)
      CALL VVSETR('VFR -- Vector Fractional Minimum', 0.7)
      CALL VVSETC('MNT -- Minimum Vector Text', ' ')
      CALL VVSETC('MXT -- Maximum Vector Text', ' ')
C
C Draw Vectors
C
      IDM=0
      RDM=0.0
      CALL GSPLCI(ICLR(2))
      CALL VVINIT(A,M,B,M,RDM,IDM,M,N,RDM,IDM)
      CALL VVECTR(A,B,RDM,IDM,IDM,RDM)
C
C Draw Streamlines
C
      CALL GSPLCI(ICLR(1))
      CALL STINIT(A,M,B,M,RDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(A,B,RDM,IDM,IDM,WRK)
C
      CALL GSPLCI(1)
C
C Draw a perimeter and eject the frame
C
      CALL PERIM(1,0,1,0)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,IDSIZ)
C
      DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)
C
      COMMON /CBFILL/ IFILIX
C
C Retrieve area id for geographic area
C
      ID = 0
      DO 10, I=1,IDSIZ
         IF (IGRP(I) .EQ. 1) ID = IAREA(I)
 10   CONTINUE
C
C If it's not water, draw it
C
      IF (ID .GE. 1 .AND. MAPACI(ID).NE.1) THEN
         CALL GSFACI(IFILIX)
         CALL GFA(NWRK,XWRK,YWRK)
      ENDIF
C
C Otherwise, do nothing
C
      RETURN
      END



