
      PROGRAM AGEX05
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
C Define the data arrays.
C
      REAL XDRA(401,6),YDRA(401,6)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Compute required constants.
C
      PI=3.14159265358979
      PID200=PI/200.
      PITTWO=2.*PI
      PIT2D3=2.*PI/3.
      PIT4D3=4.*PI/3.
      RADOSC=SQRT(3.)/3.
      RADOLC=SQRT(3.)/2.
      BSSCLL=ATAN(SQRT(12.)/6.)
      BSSCUL=ATAN(SQRT(143.)/7.)
      BSLCLL=ATAN(SQRT(143.)/17.)
      BSLCUL=ATAN(SQRT(2.0))
C
C Fill the data arrays.
C
      DO 101 I=1,401
        THETA=PID200*REAL(I-1)
        XDRA(I,1)=   -.5+RADOSC*COS(THETA)
        YDRA(I,1)=       RADOSC*SIN(THETA)
        IF (ABS(THETA       ).GE.BSSCLL.AND.
     +      ABS(THETA       ).LE.BSSCUL) XDRA(I,1)=1.E36
        IF (ABS(THETA-PITTWO).GE.BSSCLL.AND.
     +      ABS(THETA-PITTWO).LE.BSSCUL) XDRA(I,1)=1.E36
        XDRA(I,2)=    .5+RADOSC*COS(THETA)
        YDRA(I,2)=       RADOSC*SIN(THETA)
        IF (ABS(THETA-PIT2D3).GE.BSSCLL.AND.
     +      ABS(THETA-PIT2D3).LE.BSSCUL) XDRA(I,2)=1.E36
        XDRA(I,3)=       RADOSC*COS(THETA)
        YDRA(I,3)=RADOLC+RADOSC*SIN(THETA)
        IF (ABS(THETA-PIT4D3).GE.BSSCLL.AND.
     +      ABS(THETA-PIT4D3).LE.BSSCUL) XDRA(I,3)=1.E36
        XDRA(I,4)=   -.5+RADOLC*COS(THETA)
        YDRA(I,4)=       RADOLC*SIN(THETA)
        IF (ABS(THETA       ).GE.BSLCLL.AND.
     +      ABS(THETA       ).LE.BSLCUL) XDRA(I,4)=1.E36
        IF (ABS(THETA-PITTWO).GE.BSLCLL.AND.
     +      ABS(THETA-PITTWO).LE.BSLCUL) XDRA(I,4)=1.E36
        XDRA(I,5)=    .5+RADOLC*COS(THETA)
        YDRA(I,5)=       RADOLC*SIN(THETA)
        IF (ABS(THETA-PIT2D3).GE.BSLCLL.AND.
     +      ABS(THETA-PIT2D3).LE.BSLCUL) XDRA(I,5)=1.E36
        XDRA(I,6)=       RADOLC*COS(THETA)
        YDRA(I,6)=RADOLC+RADOLC*SIN(THETA)
        IF (ABS(THETA-PIT4D3).GE.BSLCLL.AND.
     +      ABS(THETA-PIT4D3).LE.BSLCUL) XDRA(I,6)=1.E36
  101 CONTINUE
C
C Specify subscripting of XDRA and YDRA.
C
      CALL AGSETI ('ROW.',2)
C
C Set up grid shape to make 1 unit in x = 1 unit in y.
C
      CALL AGSETF ('GRID/SHAPE.',2.)
C
C Turn off background, then turn labels back on.
C
      CALL AGSETF ('BACKGROUND.',4.)
      CALL AGSETI ('LABEL/CONTROL.',2)
C
C Turn off left label.
C
      CALL AGSETC ('LABEL/NAME.','L')
      CALL AGSETI ('LABEL/SUPPRESSION FLAG.',1)
C
C Change text of bottom label.
C
      CALL AGSETC ('LABEL/NAME.','B')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','PURITY, BODY, AND FLAVOR$')
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,401,6,401,'EXAMPLE 5 (EZMXY)$')
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END
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
