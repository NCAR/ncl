
      PROGRAM PGKEX05
C
C  Illustrate clipping.
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

      PARAMETER (ILD=121)
      DIMENSION PLX(ILD),PLY(ILD)
      DIMENSION XC(2),YC(2),CX(2,2),CY(2,2),AX(5),AY(5)
      DATA  RADC/.0174532/
C
C  Center positions for spirals.
C
      DATA XC / .31007, .67052 /
      DATA YC / .62000, .20000 /
C
C  Clipping rectangles.
C
      DATA CX / .20, .77, .20, .77 /
      DATA CY / .50, .76, .08, .34 /
C
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define indices, color index 0 defines the background color.
C
      CALL GSCR(IWKID,0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID,1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID,2, 0.4, 0.0, 0.4)
      CALL GSCR(IWKID,3, 0.0, 0.0, 1.0)
C
C  Set the line width to 2 times the nominal width.  This setting
C  may not be honored by all hardware devices.
C
      CALL GSLWSC(2.0)
      DO 30 K=1,2
C
C  Define the clipping rectangle.
C
        CALL GSWN(1,CX(1,K),CX(2,K),CY(1,K),CY(2,K))
        CALL GSVP(1,CX(1,K),CX(2,K),CY(1,K),CY(2,K))
        CALL GSELNT(1)
C
C  Clipping is on for the second curve only.
C
        CALL GSCLIP(K-1)
C
C  Draw a boundary around the clipping rectangle.
C
        AX(1) = CX(1,K)
        AY(1) = CY(1,K)
        AX(2) = CX(2,K)
        AY(2) = AY(1)
        AX(3) = AX(2)
        AY(3) = CY(2,K)
        AX(4) = AX(1)
        AY(4) = AY(3)
        AX(5) = AX(1)
        AY(5) = AY(1)
        CALL GSPLCI(1)
        CALL GPL(5,AX,AY)
C
C  Draw the spirals.
C
        CALL GSPLCI(3)
        J = 0
        DO 10 I=0,720,6
          SCALE = REAL(I)/4000.
          J = J+1
          PLX(J) = XC(1)+SCALE*COS(REAL(I-1)*RADC)
          PLY(J) = YC(K)+SCALE*SIN(REAL(I-1)*RADC)
   10   CONTINUE
C
        CALL GPL(121,PLX,PLY)
C
        J = 0
        DO 20 I=2,722,6
          SCALE = REAL(I)/4000.
          J = J+1
          PLX(J) = XC(2)-SCALE*COS(REAL(I-1)*RADC)
          PLY(J) = YC(K)-SCALE*SIN(REAL(I-1)*RADC)
   20   CONTINUE
C
        CALL GPL(121,PLX,PLY)
   30 CONTINUE
C
C  Turn clipping back off.
C
      CALL GSCLIP(0)
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.9,'Clipping',.035,0.,0.)
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.2,.80,'Clipping off',.022,0.,-1.)
      CALL PLCHHQ(.2,.38,'Clipping on' ,.022,0.,-1.)
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
