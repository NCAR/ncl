
      PROGRAM AGEX06
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
      REAL XDRA(501),YDRA(501)
C
C Define the graph-window parameter array.
C
      REAL GWND (4,4)
C
C Define variables used in setting up informational labels.
C
      CHARACTER*35 GLAB
      CHARACTER*23 BACK(4)
      CHARACTER*12 LNLG(4)
C
      DATA (GWND(I,1),I=1,4) / 0.0 , 0.5 , 0.5 , 1.0 /
      DATA (GWND(I,2),I=1,4) / 0.5 , 1.0 , 0.5 , 1.0 /
      DATA (GWND(I,3),I=1,4) / 0.0 , 0.5 , 0.0 , 0.5 /
      DATA (GWND(I,4),I=1,4) / 0.5 , 1.0 , 0.0 , 0.5 /
C
      DATA BACK(1) / '(PERIMETER BACKGROUND)$' /
      DATA BACK(2) / '(GRID BACKGROUND)$     ' /
      DATA BACK(3) / '(HALF-AXIS BACKGROUND)$' /
      DATA BACK(4) / '(NO BACKGROUND)$       ' /
C
      DATA LNLG(1) / 'LINEAR$' /
      DATA LNLG(2) / 'LOGARITHMIC$' /
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,501
        THETA=.031415926535898*REAL(I-1)
        XDRA(I)=500.+.9*REAL(I-1)*COS(THETA)
        YDRA(I)=500.+.9*REAL(I-1)*SIN(THETA)
  101 CONTINUE
C
C Suppress the frame advance.
C
      CALL AGSETI ('FRAME.',2)
C
C Do four graphs on the same frame, using different
C backgrounds.
C
      DO 102 IGRF = 1,4
C
C Position the graph window.
C
        CALL AGSETP ('GRAPH WINDOW.',GWND(1,IGRF),4)
C
C Declare the background type.
C
        CALL AGSETI ('BACKGROUND TYPE.',IGRF)
C
C Setting the background type may have turned informational
C labels off.  In that case, turn them back on.
C
        IF (IGRF.EQ.4) CALL AGSETI ('LABEL/CONTROL.',2)
C
C Set up parameters determining linear/log nature of axes.
C
        ILLX=(IGRF-1)/2
        ILLY=MOD(IGRF-1,2)
C
C Declare the linear/log nature of the graph.
C
        CALL AGSETI ('X/LOGARITHMIC.',ILLX)
        CALL AGSETI ('Y/LOGARITHMIC.',ILLY)
C
C Change the x- and y-axis labels to reflect the linear/log
C nature of the graph.
C
        CALL AGSETC ('LABEL/NAME.','B')
        CALL AGSETI ('LINE/NUMBER.',-100)
        CALL AGSETC ('LINE/TEXT.',LNLG(ILLX+1))
C
        CALL AGSETC ('LABEL/NAME.','L')
        CALL AGSETI ('LINE/NUMBER.',100)
        CALL AGSETC ('LINE/TEXT.',LNLG(ILLY+1))
C
C Set up the label for the top of the graph.
C
        WRITE (GLAB,1001) IGRF,BACK(IGRF)
C
C Draw the graph, using EZXY.
C
        CALL EZXY (XDRA,YDRA,501,GLAB)
C
  102 CONTINUE
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
C Format for encode.
C
 1001 FORMAT ('EXAMPLE 6-',I1,' ',A23)
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
