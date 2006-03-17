
      PROGRAM COEX03
C
C Create color wheels in HSV space.  Each wheel is produced with
C a different value of V (the value parameter in HSV space).  Each
C wheel is composed of 16 wedges.  The value for the hue remains
C the same within each wedge, and the value for the saturation
C varies linearly from the center to the outer rim within each wedge.
C The hues vary from 0. to 360. around the color wheel starting
C at pure red, and returning to pure red.
C
C This program requires PLOTCHAR for drawing its characters.
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
C  Define the number of values, the number of hues, the number
C  of saturations, and the number of points bounding a color box.
C
      PARAMETER(NW=3, NCOL=16, NSAT=4, NPTS = 4)
      DIMENSION X(NPTS),Y(NPTS)
      DIMENSION IASF(13)
      DIMENSION VAL(NW),ST(NSAT)
      CHARACTER*80 TITLE
C
      DATA IASF / 13*1 /
      DATA PI   /  3.14159265/
C
C  Define the values to be used.
C
      DATA VAL  / 0.60, 0.80, 1.00 /
C
C  Y-coordinates for the saturation labels.
C
      DATA ST(1),ST(2),ST(3),ST(4)/-.1, -.375, -.625, -.850/
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP(0)
      CALL GSASF(IASF)
      CALL GSFAIS(1)
C
C  Background and foreground colors.
C
      CALL GSCR(IWKID,0,0.0,0.0,0.0)
      CALL GSCR(IWKID,1,1.0,1.0,1.0)
C
C  Loop on the values.
C
      DO 30 IL = 1,NW
        INDEX = 2
        CALL SET(.1,.9,.1,.9,-1.2,1.2,-1.2,1.2,1)
        RINC = 2 * PI / REAL(NCOL)
        HINC = 360.0  / REAL(NCOL)
        SINC =  1.00  / REAL(NSAT - 1)
C
C  Loop on the hues.
C
        DO 20 IHUE = 0,NCOL - 1
          HUE    =  REAL(IHUE)      * HINC
          THETA1 = (REAL(IHUE) -.5) * RINC
          THETA2 = (REAL(IHUE) +.5) * RINC
          X(1) = 0.0
          X(4) = 0.0
          Y(1) = 0.0
          Y(4) = 0.0
C
C  Loop on the saturations
C
          DO 10 ISAT = 1,NSAT
            SAT = (REAL(ISAT - 1) * SINC)
            CALL HSVRGB(HUE,SAT,VAL(IL),R,G,B)
            CALL GSCR(IWKID,INDEX,R,G,B)
            CALL GSFACI(INDEX)
            RLEN = REAL(ISAT) / REAL(NSAT)
            X(2) = COS(THETA1) * RLEN
            Y(2) = SIN(THETA1) * RLEN
            X(3) = COS(THETA2) * RLEN
            Y(3) = SIN(THETA2) * RLEN
            CALL GFA(4,X,Y)
            X(1) = X(2)
            X(4) = X(3)
            Y(1) = Y(2)
            Y(4) = Y(3)
            INDEX = INDEX+1
 10       CONTINUE
 20     CONTINUE
C
C  Label the plots.
C
        CALL PCSETI('QU  - QUALITY FLAG',0)
        CALL PCSETI('CD  - SELECT DUPLEX DATA SET',1)
        WRITE(TITLE,700) VAL(IL)
        CALL GSPLCI(1)
        CALL PLCHHQ(0.0,1.25,TITLE(1:12),21.0,0.0,0.0)
        DO 40 L=2,NSAT
          SAT = (REAL(L-1)*SINC)
          WRITE(TITLE,710) SAT
          CALL PLCHHQ(0.0,ST(L),TITLE(1:6),15.0,0.0,0.0)
   40   CONTINUE
        CALL PLOTIF(0.,0.,2)
        CALL GSPLCI(1)
        CALL LINE(.98,0.,1.03,0.)
        CALL PLCHHQ(1.08,0.,'Hue=0.',15.0,0.0,-1.0)
        CALL LINE( .700,.700, .750,.740)
        CALL PLCHHQ( .80,.740,'Hue=45.',15.0,0.0,-1.0)
        CALL LINE(-.700,.700,-.750,.740)
        CALL PLCHHQ(-.80,.740,'Hue=135.',15.0,0.0,1.0)
        CALL LINE(-.700,-.700,-.750,-.740)
        CALL PLCHHQ(-.80,-.740,'Hue=225.',15.0,0.0,1.0)
        CALL LINE( .700,-.700, .750,-.740)
        CALL PLCHHQ( .80,-.740,'Hue=315.',15.0,0.0,-1.0)
        CALL FRAME
 30   CONTINUE
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
 700  FORMAT('VALUE = ',F4.2)
 710  FORMAT('S=',F4.2)
C
      END
