      PROGRAM FONTS
C
C  Illustrate the various fonts.
C
      CHARACTER*27 LABEL
      DATA LABEL /'NCAR Graphics, Release 3.2'/
C
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Define the necessary color indices.
C
      CALL GSCR(1,0,0.,0.,.6)
      CALL GSCR(1,1,1.,1.,0.)
      CALL GSCR(1,2,0.,1.,1.)
C
C  Set up character attributes.
C
      CALL GSCHH(.022)
      CALL GSTXAL(2,3)
      CALL GSTXCI(1)
C
C  Loop through the fonts, the Hershey fonts have negative font
C  indices.
C
      X = .5
      DO 10 I=1,20
        Y = .1+.038*REAL(20-I)
        J = I
        IF (I .GT. 1) J=-I
        CALL GSTXFP(J,2)
        CALL GTX(X,Y,LABEL)
   10 CONTINUE
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.96,'Same string',.033,0.,0.)
      CALL PLCHHQ(.5,.90,'using various fonts',.033,0.,0.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
