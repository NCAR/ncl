
      PROGRAM AGEX13
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
C Define the data array.
C
      DIMENSION XYCD(226)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data array.
C
      READ 1001 , XYCD
C
      DO 101 I=1,226
        IF (XYCD(I).EQ.1.E36) GO TO 101
        XYCD(I)=2.**((XYCD(I)-15.)/2.5)
  101 CONTINUE
C
C Specify log/log plot.
C
      CALL DISPLA (0,0,4)
C
C Bump the line-maximum parameter past 42.
C
      CALL AGSETI ('LINE/MAXIMUM.',50)
C
C Specify x- and y-axis labels, grid background.
C
      CALL ANOTAT('LOGARITHMIC, BASE 2, EXPONENTIAL LABELS$',
     +            'LOGARITHMIC, BASE 2, NO-EXPONENT LABELS$',
     +            2,0,0,' ')
C
C Specify the graph label.
C
      CALL AGSETC ('LABEL/NAME.','T')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.','FINAL EXAMPLE$')
C
C Specify x-axis ticks and labels.
C
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',3)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',2.)
      CALL AGSETI ('BOTTOM/NUMERIC/TYPE.',2)
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',4)
      CALL AGSETI ('BOTTOM/MINOR/PATTERN.',43690)
C
C Specify y-axis ticks and labels.
C
      CALL AGSETI ('LEFT/MAJOR/TYPE.',3)
      CALL AGSETF ('LEFT/MAJOR/BASE.',2.)
      CALL AGSETI ('LEFT/NUMERIC/TYPE.',3)
      CALL AGSETI ('LEFT/MINOR/SPACING.',4)
      CALL AGSETI ('LEFT/MINOR/PATTERN.',43690)
C
C Compute secondary control parameters.
C
      CALL AGSTUP (XYCD(1),1,0,113,2,XYCD(2),1,0,113,2)
C
C Draw the background.
C
      CALL AGBACK
C
C Draw the curve twice to make it darker.
C
      CALL AGCURV (XYCD(1),2,XYCD(2),2,113,1)
      CALL AGCURV (XYCD(1),2,XYCD(2),2,113,1)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
C Format.
C
 1001 FORMAT (14E5.0)
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
