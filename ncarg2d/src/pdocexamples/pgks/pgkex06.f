
      PROGRAM PGKEX06
C
C  Illustrate polylines with different colors, widths, and types.
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

      CHARACTER LABEL1*8, LABEL2*11
      PARAMETER (ILD=121)
      DIMENSION PLX(ILD),PLY(ILD),DLX(ILD),DLY(ILD)
      DATA  RADC/.0174532/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define color indices, color index 0 defines the background color.
C
      CALL GSCR(IWKID,0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID,1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID,2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID,3, 0.0, 0.0, 1.0)
      CALL GSCR(IWKID,4, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID,5, 0.4, 0.0, 0.4)
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
        CALL GTX(X,Y+.18,LABEL1)
        CALL GTX(X,Y+.14,LABEL2)
   20 CONTINUE
C
C  Label the plot using Plotchar.
C
      CALL GSLN(1)
C     
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',5)
      CALL PLCHHQ(.5,.93,'Polylines',.035,0.,0.)
C
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
