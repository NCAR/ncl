      PROGRAM POLYL
C
C  Illustrate polylines with different colors, widths, and types.
C
      CHARACTER LABEL1*8, LABEL2*11
      PARAMETER (ILD=121)
      DIMENSION PLX(ILD),PLY(ILD),DLX(ILD),DLY(ILD)
      DATA  RADC/.0174532/
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
      CALL GSCR(1,0,.0,.0,.6)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSCR(1,2,1.,0.,0.)
      CALL GSCR(1,3,0.,1.,0.)
      CALL GSCR(1,4,1.,1.,0.)
      CALL GSCR(1,5,.65,1.,1.)
C
C  Generate data for a spiral.
C
      J = 0
      DO 10 I=0,720,6
        SCALE = REAL(I)/4500.
        J = J+1
        PLX(J) = SCALE*COS(REAL(I-1)*RADC)
        PLY(J) = SCALE*SIN(REAL(I-1)*RADC)
   10 CONTINUE
C
C  Draw spirals with different linetypes, colors, and widths.
C
      DO 20 I=1,4
        X = .28+.42*REAL(MOD(I,2))
        Y = .25+.41*REAL(INT((I-1)/2))
        CALL GSPLCI(I)
        WDTH = 2.*REAL(I-1)+1.
        CALL GSLWSC(WDTH)
        CALL GSLN(I)
        DO 30 J=1,ILD
          DLX(J) = X+PLX(J)
          DLY(J) = Y+PLY(J)
   30   CONTINUE
        CALL GPL(ILD,DLX,DLY)
C
C  Label the lines.
C
        WRITE(LABEL1,100) I
  100   FORMAT('Type = ',I1)
        WRITE(LABEL2,110) WDTH
  110   FORMAT('Width = ',F3.1)
        CALL GSTXAL(2,3)
        CALL GSCHH(.022)
        CALL GSTXFP(-12,2)
        CALL GSTXCI(5)
        CALL GTX(X,Y+.17,LABEL1)
        CALL GTX(X,Y+.13,LABEL2)
   20 CONTINUE
C
C  Label the plot using Plotchar.
C
      CALL GSLN(1)
C     
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',4)
      CALL PLCHHQ(.5,.93,'Polylines',.035,0.,0.)
C
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
