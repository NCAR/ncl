      PROGRAM CELLAR
C
C  Demonstration of the GKS CELL ARRAY entry.
C
      PARAMETER   (IDX=2,IDY=3)
      INTEGER     COLIA(IDX,IDY)
      CHARACTER*7 LABEL(IDX,IDY)
C
C  Define the X and Y extents for a rectangle.
C
      DATA XL,XR,YB,YT/0.10, 0.90, 0.25, 0.75/
      DATA LABEL/'  Red  ',' Cyan  ',' Green ','Magenta',
     +           ' Blue  ','Yellow '/
C
C  Open GKS, open and activate a CGM workstation.
C
      CALL GOPKS (6,0)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C  Set up a color table and define the color index array.
C
      CALL GSCR(1,0,1.,1.,1.)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSCR(1,2,1.,0.,0.)
      CALL GSCR(1,3,0.,1.,0.)
      CALL GSCR(1,4,0.,0.,1.)
      CALL GSCR(1,5,0.,1.,1.)
      CALL GSCR(1,6,1.,0.,1.)
      CALL GSCR(1,7,1.,1.,0.)
C
      DO 10 I=1,IDX
        DO 20 J=1,IDY
          COLIA(I,J) = IDY*(I-1)+J+1
   20   CONTINUE
   10 CONTINUE
C
C  Draw the cell array.
C
      CALL GCA(0.10, 0.25, 0.90, 0.75, IDX, IDY, 1, 1, IDX, IDY, COLIA)       
C
C  Label the cells using PLOTCHAR font 26 with the foreground color.
C
      DX = (XR-XL)/IDX
      DY = (YT-YB)/IDY
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',1)
      DO 30 I=1,IDX
        XCENT = XL+0.5*DX+REAL(I-1)*DX
        DO 40 J=1,IDY
          YCENT = YB+0.5*DY+REAL(J-1)*DY
          CALL PLCHHQ(XCENT,YCENT,LABEL(I,J),.033,0.,0.)
   40   CONTINUE
   30 CONTINUE
C
C  Terminate the picture.
C
      CALL FRAME
C
C  Deactivate and close the workstation, close gks.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
