
      PROGRAM SFEX01
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
C Declare required dimensioned arrays.
C
      DIMENSION XRA(200),YRA(200),DST(220),IND(240)
      DIMENSION ID1(8,8),ID2(8,8),ID3(8,8),ID4(8,8)
C
C Define four different dot patterns.
C
      DATA ID1 / 1,1,0,0,0,0,1,1,
     +           1,1,0,1,1,0,1,1,
     +           0,0,0,1,1,0,0,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,0,0,1,1,0,0,0,
     +           1,1,0,1,1,0,1,1,
     +           1,1,0,0,0,0,1,1/
      DATA ID2 / 0,0,0,0,0,0,0,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,0,0,0,0,0,0,0/
      DATA ID3 / 0,0,0,0,0,0,0,0,
     +           0,0,0,0,1,0,0,0,
     +           0,0,0,1,1,1,0,0,
     +           0,1,0,0,1,0,0,1,
     +           0,0,1,1,1,1,1,0,
     +           0,0,0,0,1,0,0,0,
     +           0,0,0,1,0,1,0,0,
     +           0,1,1,0,0,0,1,1/
      DATA ID4 / 0,0,0,0,0,0,0,0,
     +           0,1,1,0,0,1,1,1,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,1,1,0,0,
     +           0,1,1,1,1,0,0,0,
     +           0,1,1,0,1,1,0,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,0,1,1,1/
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Double the size of the GKS dot.
C
      CALL GSMKSC (2.)
C
C This code creates a single frame showing nine circles filled in
C various ways.  The DO-loop variable I says which row of circles
C we're working on (1 => top, 2 => middle, 3 => bottom).  The
C DO-loop variable J says which column of circles we're working
C on (1 => left, 2 => center, 3 => right).  The variable K gives
C the number of the circle currently being drawn and is used in
C a computed GO TO to determine which block of code is executed.
C
      DO 111 I=1,3
        YCN=REAL(4-I)
        DO 110 J=1,3
          XCN=REAL(J)
          K=3*(I-1)+J
          DO 100 L=1,101
            XRA(L)=XCN+.48*SIN(.062831853071796*REAL(L))
            YRA(L)=YCN+.48*COS(.062831853071796*REAL(L))
  100     CONTINUE
C
C Draw the circle.
C
          CALL SET (0.,1.,0.,1.,0.,4.,0.,4.,1)
          CALL CURVE (XRA,YRA,101)
C
C Jump to the proper piece of code to fill the circle.
C
          GO TO (101,102,103,104,105,106,107,108,109) , K
C
C Fill the first circle with horizontal lines.
C
  101     CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the second circle in the same way, but add a diamond-shaped
C hole.
C
  102     XRA(101)=2.00
          YRA(101)=3.24
          XRA(102)=1.76
          YRA(102)=3.00
          XRA(103)=2.00
          YRA(103)=2.76
          XRA(104)=2.24
          YRA(104)=3.00
          XRA(105)=XRA(101)
          YRA(105)=YRA(101)
          XRA(106)=XRA(100)
          YRA(106)=YRA(100)
          CALL SFWRLD (XRA,YRA,106,DST,110,IND,114)
          GO TO 110
C
C Fill the third circle with lines in two different directions to
C create a cross-hatched effect and create a more complicated hole.
C
  103     XRA(101)=XRA( 40)
          YRA(101)=YRA( 40)
          XRA(102)=XRA( 80)
          YRA(102)=YRA( 80)
          XRA(103)=XRA( 20)
          YRA(103)=YRA( 20)
          XRA(104)=XRA( 60)
          YRA(104)=YRA( 60)
          XRA(105)=XRA(100)
          YRA(105)=YRA(100)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.009)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
          CALL SFWRLD (XRA,YRA,105,DST,111,IND,117)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',135)
          CALL SFNORM (XRA,YRA,105,DST,111,IND,117)
          GO TO 110
C
C Fill the fourth circle with the default dot pattern, increasing the
C inter-dot spacing considerably.
C
  104     CALL SFSETR ('SP - SPACING OF FILL LINES',.005)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
          CALL SFSETI ('DO - DOT-FILL FLAG',1)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the fifth circle with a combination of lines and dots.
C
  105     CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
          CALL SFSETI ('DO - DOT-FILL FLAG',0)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.006)
          CALL SFSETI ('DO - DOT-FILL FLAG',1)
          CALL SFNORM (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the sixth circle with a specified dot pattern.
C
  106     CALL SFSETR ('SP - SPACING OF FILL LINES',.004)
          CALL SFSETP (ID1)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the seventh circle with a different dot pattern, tilted at an
C angle.
C
  107     CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
          CALL SFSETP (ID2)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the eighth circle with a different dot pattern, using characters.
C
  108     CALL GSCHH  (.004)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.006)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
          CALL SFSETC ('CH - CHARACTER SPECIFIER','O')
          CALL SFSETP (ID3)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the last circle with K's, both large and small.
C
  109     CALL GSCHH  (.008)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
          CALL SFSETC ('CH - CHARACTER SPECIFIER','K')
          CALL SFSETP (ID4)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
  110   CONTINUE
C
  111 CONTINUE
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
C Done.
C
      STOP
C
      END
