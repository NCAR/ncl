      PROGRAM CLIPNG
C
C  Illustrate clipping.
C
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
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Define the necessary color indices, color index 0 defines the
C  background color.
C
      CALL GSCR(1,0,.0,.0,.7)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSCR(1,2,.6,1.,1.)
      CALL GSCR(1,3,0.,1.,0.)
      CALL GSCR(1,4,1.,1.,0.)
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
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',4)
      CALL PLCHHQ(.5,.9,'Clipping',.035,0.,0.)
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.2,.80,'Clipping off',.022,0.,-1.)
      CALL PLCHHQ(.2,.38,'Clipping on' ,.022,0.,-1.)
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
