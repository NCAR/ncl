
      PROGRAM FNGNGDTS
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL CMPTRA('OR',35.,-105.,0.,'PO','CO',22.,-122.,47.,-65.,IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END

      SUBROUTINE CMPTRA(PROJ, PLAT, PLON, ROTA, OUTLN,
     +      JLIM, PLIM1, PLIM2, PLIM3, PLIM4,IWKID)

      CHARACTER*2 PROJ, OUTLN, JLIM
      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)
      DATA NUMSTS/3/
      REAL LAT(3), LON(3)
      REAL XCOOR(3), YCOOR(3)
      CHARACTER CITY(3)*20
C
C LAT, LON coordinates for Atlanta, Boulder, and Seattle
C
      DATA LAT/33.5,40.0,47.5/
      DATA LON/-84.5,-105,-122.0/
      DATA XCOOR/0.,0.,0./
      DATA YCOOR/0.,0.,0./
      DATA CITY/'Atlanta, GA', 'Boulder, CO', 'Seattle, WA'/
C
C CMPTRA demonstrates marking points on a map
C
C Set up a color table
C
      CALL DFCLRS (IWKID)
C
C Draw Continental, political outlines 
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR',OUTLN)
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
C Turn off Grid lines
C
      CALL MAPSTR ('GR',0.)
C
C Draw map
C
      CALL MAPDRW

      DO 100 I=1,NUMSTS
C
C Transform the Coordinates
C
         CALL MAPTRA(LAT(I),LON(I),X,Y)
         XCOOR(I) = X
         YCOOR(I) = Y
C
C Draw a label
C
         CALL PLCHHQ (X+.015,Y,CITY(I), .015, 0., -1.)
C
C Save last indice
C
         INDICE = I
 100  CONTINUE

C
C Draw filled circles at selected sites.
C
      IF (XCOOR(INDICE).NE.1.E12) THEN
         CALL NGDOTS (XCOOR,YCOOR,NUMSTS,.02,15)
      ENDIF
C
C Draw stars over the selected sites and connect them with lines.
C
      CALL POINTS (XCOOR, YCOOR, NUMSTS, -3, 1)
C
C Advance the frame.
C
      CALL FRAME
C
C Done.
C
      RETURN
      END

      SUBROUTINE DFCLRS (IWKID)
C
C Define a set of RGB color triples for colors 1 through 15.
C
      DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
      DATA RGBV / 0.00 , 0.00 , 0.00 ,
     +      0.70 , 0.70 , 0.70 ,
     +      0.75 , 0.50 , 1.00 ,
     +      0.50 , 0.00 , 1.00 ,
     +      0.00 , 0.00 , 1.00 ,
     +      0.00 , 0.50 , 1.00 ,
     +      0.00 , 1.00 , 1.00 ,
     +      0.00 , 1.00 , 0.60 ,
     +      0.00 , 1.00 , 0.00 ,
     +      0.70 , 1.00 , 0.00 ,
     +      1.00 , 1.00 , 0.00 ,
     +      1.00 , 0.75 , 0.00 ,
     +      1.00 , 0.38 , 0.38 ,
     +      1.00 , 0.00 , 0.38 ,
     +      1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
      DO 101 I=1,15
         CALL GSCR (IWKID,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
 101  CONTINUE
C
C Done.
C
      RETURN
C
      END

