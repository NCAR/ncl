C
C     $Id: ps.bkg_black.f,v 1.5 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM BKGBLK
C
C  This Fortran code is a filter (reads from standard input, writes
C  to standard output) for PostScript files produced from the NCAR
C  Graphics package.  It forces a black background and bumps all B&W
C  intensity values (values with R=G=B) larger than .79 to 1.0 .
C
      INTEGER      CTFLG,CRFLG
      CHARACTER*80 LIN,LOUT
C
      DATA EPS /0.00001/
   10 CONTINUE
C
C  Read an input line
C
      LIN = ' '
      READ(5,100,END=80) LIN
  100 FORMAT(A80)
C
C  Force background color to black as the first plotting instruction in
C  each frame.
C
      IF (LIN(1:3).EQ.'1 j') THEN
          WRITE(6,525)
  525     FORMAT(' ct 000 [0.0000 0.0000 0.0000] put')
          WRITE(6,530)
  530     FORMAT(' 000 o'/'n'/' 0404 1124 m'/' 4482 1124 l'/
     +           ' 4482 5202 l'/' 0404 5202 l'/' 0404 1124 l'/'f')
      ENDIF
C
C  Determine if this is a color table setting.
C
      CTFLG = 0
      DO 20 I=1,80
      IF (LIN(I:I) .EQ. ' ') THEN
        GO TO 20
      ELSE
        IF (LIN(I:I).EQ.'c' .AND. LIN(I+1:I+1).EQ.'t') THEN
          CTFLG = I
          GO TO 30
        ENDIF
      ENDIF
   20 CONTINUE
   30 CONTINUE
      IF (CTFLG .GT. 0) THEN
C
C  Determine if the color values represent a gray value.
C
        CALL CKGRY(LIN,EPS,CTFLG,CRFLG,IDX,R,G,B)
C
C  Encode the modified color values in LOUT.
C
        IF (CRFLG.NE.0) THEN
          LOUT = ' '
          WRITE(LOUT,500) IDX, R,G,B
  500     FORMAT(' ct ',I3,' [',F5.3,' ',F5.3,' ',F5.3,'] put')
        ELSE
          LOUT = LIN
        ENDIF
      ELSE
          LOUT = LIN
      ENDIF
C
C  Trim blanks and Write to output.
C
      KPOS = 1
      DO 110 I=80,1,-1
        IF (LOUT(I:I) .NE. ' ') THEN
          KPOS = I
          GO TO 120
        ENDIF
  110 CONTINUE
  120 CONTINUE
      WRITE(6,130) (LOUT(LL:LL),LL=1,KPOS)
  130 FORMAT(80A1)
C
      GO TO 10
C
   80 CONTINUE
      STOP
      END
      SUBROUTINE CKGRY(LIN,EPS,JFLG,IFLG,IDX,R,G,B)
C
C  Check if the input line in LIN contains a gray intensity value
C  (within the limits of EPS).  IFLG is 1 if a gray value is found;
C  IFLG is 0 otherwise.  If IFLG is 1, then IDX is the color index,
C  and R, G, and B are the complemented color vlaues.
C
      CHARACTER*80 LIN,CTMP
      REAL         EPS,COLORS(3)
      INTEGER      IFLG
C
      IFLG = 0
C
C  Determine the location of the left bracket.
C
      ICOL = 0
      DO 10 I=1,80
        IF (LIN(I:I) .EQ. '[') THEN
          ICOL = I
          GO TO 20
        ELSE
          IF (I .EQ. 80) GO TO 40
          GO TO 10
        ENDIF
   10 CONTINUE
   20 CONTINUE
C
C  Get the color index.
C
      READ(LIN(JFLG+2:ICOL-2),520) IDX
  520 FORMAT(I4)
C
C  Get the three color values.
C
      JSTRT = ICOL+1
      DO 50 I=1,3
        CTMP = ' '
        K = 0
        DO 60 J=JSTRT,80
          K = K+1
          IF (LIN(J:J).EQ.' ' .OR. LIN(J:J).EQ.']') THEN
            IF (K .GT. 1) THEN
              CTMP(1:K-1) = LIN(JSTRT:J-1)
            ELSE
              GO TO 40
            ENDIF
            GO TO 70
          ELSE
            GO TO 60
          ENDIF
   60   CONTINUE
   70   CONTINUE
        READ(CTMP(1:K-1),500) COLORS(I)
  500   FORMAT(F10.3)
        JSTRT = J+1
   50 CONTINUE
C
C  Determine if we have a gray value.
C
      IFLG = 0
      IF (ABS(COLORS(1)-COLORS(2)).LE.EPS .AND.
     -    ABS(COLORS(2)-COLORS(3)).LE.EPS) IFLG = 1
C
C  Adjust the intensities.
C
      IF (IFLG .EQ. 1) THEN
        DO 90 I =1,3
          IF (COLORS(I) .GT. .79) THEN
            COLORS(I) = 1.
          ENDIF
   90   CONTINUE
        R = COLORS(1)
        G = COLORS(2)
        B = COLORS(3)
      ELSE
        GO TO 40
      ENDIF
C
   40 CONTINUE
      RETURN
      END
