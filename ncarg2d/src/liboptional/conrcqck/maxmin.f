C
C	$Id: maxmin.f,v 1.5 2008-07-27 00:23:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAXMIN (Z,L,MM,NN,JSIZEM,AASH,JOFFDT)
      SAVE
C
C THIS ROUTINE FINDS RELATIVE MINIMUMS AND MAXIMUMS.  A RELATIVE MINIMUM
C (OR MAXIMUM) IS DEFINED TO BE THE LOWEST (OR HIGHEST) POINT WITHIN
C A CERTAIN NEIGHBORHOOD OF THE POINT.  THE NEIGHBORHOOD USED HERE
C IS + OR - MN IN THE X DIRECTION AND + OR - NM IN THE Y DIRECTION.
C
C
      CHARACTER*7     IA
      DIMENSION       IW(4)
      DIMENSION       Z(L,NN)
C
C
      COMMON /CONRE1/ IOFFP      ,SPVAL
      COMMON /CONRE5/ SCLY
      DATA IW(1),IW(2),IW(3),IW(4)/4,6,8,12/
C
C     FX(X,Y) = X
C     FY(X,Y) = Y
C
      M = MM
      N = NN
      ISIZEM = JSIZEM
      ASH = ABS(AASH)
      IOFFDT = JOFFDT
C
      IF (AASH .LT. 0.0) GO TO 128
C
      MN = MIN(15,MAX(2,INT(REAL(M)/8.)))
      NM = MIN(15,MAX(2,INT(REAL(N)/8.)))
      NM1 = N-1
      MM1 = M-1
      IH1 = 4*IW(ISIZEM+1)
C
C SCALE IH1 TO COMPENSATE FOR USER CHANGE IN RESOLUTION
C
      IH1 = 32*SCLY*IH1
C
C LINE LOOP FOLLOWS - THE COMPLETE TWO-DIMENSIONAL TEST FOR A MINIMUM OR
C MAXIMUM OF THE FIELD IS ONLY PERFORMED FOR POINTS WHICH ARE MINIMA OR
C MAXIMA ALONG SOME LINE - FINDING THESE CANDIDATES IS MADE EFFICIENT BY
C USING A COUNT OF CONSECUTIVE INCREASES OR DECREASES OF THE FUNCTION
C ALONG THE LINE
C
      DO 127 JP=2,NM1
C
         IM = MN-1
         IP = -1
         GO TO 126
C
C CONTROL RETURNS TO STATEMENT 10 AS LONG AS THE FUNCTION IS INCREASING
C ALONG THE LINE - WE SEEK A POSSIBLE MAXIMUM
C
  101    IP = IP+1
         AA = AN
         IF (IP .EQ. MM1) GO TO 104
         AN = Z(IP+1,JP)
         IF (IOFFP.NE.0 .AND. AN.EQ.SPVAL) GO TO 125
         IF (AA-AN) 102,103,104
  102    IM = IM+1
         GO TO 101
  103    IM = 0
         GO TO 101
C
C FUNCTION DECREASED - TEST FOR MAXIMUM ON LINE
C
  104    IF (IM .GE. MN) GO TO 106
         IS = MAX(1,IP-MN)
         IT = IP-IM-1
         IF (IS .GT. IT) GO TO 106
         DO 105 II=IS,IT
            IF (AA .LE. Z(II,JP)) GO TO 112
  105    CONTINUE
  106    IS = IP+2
         IT = MIN(M,IP+MN)
         IF (IS .GT. IT) GO TO 109
         DO 108 II=IS,IT
            IF (IOFFP.EQ.0 .OR. Z(II,JP).NE.SPVAL) GO TO 107
            IP = II-1
            GO TO 125
  107       IF (AA .LE. Z(II,JP)) GO TO 112
  108    CONTINUE
C
C WE HAVE MAXIMUM ON LINE - DO TWO-DIMENSIONAL TEST FOR MAXIMUM OF FIELD
C
  109    JS = MAX(1,JP-NM)
         JT = MIN(N,JP+NM)
         IS = MAX(1,IP-MN)
         IT = MIN(M,IP+MN)
         DO 111 JK=JS,JT
            IF (JK .EQ. JP) GO TO 111
            DO 110 IK=IS,IT
               IF (Z(IK,JK).GE.AA .OR.
     1             (IOFFP.NE.0 .AND. Z(IK,JK).EQ.SPVAL)) GO TO 112
  110       CONTINUE
  111    CONTINUE
C
         X = REAL(IP)
         Y = REAL(JP)
         CALL WTSTR ( FX(X,Y),FY(X,Y),'H',ISIZEM,0,0 )
         CALL FL2INT ( FX(X,Y),FY(X,Y),IFX,IFY )
         IFY = IFY*SCLY
         CALL ENCD (AA,ASH,IA,NC,IOFFDT)
         MY = IFY - IH1
         TMY = CPUY ( MY )
         CALL WTSTR ( FX(X,Y),TMY,IA(1:NC),ISIZEM,0,0 )
  112    IM = 1
         IF (IP-MM1) 113,127,127
C
C CONTROL RETURNS TO STATEMENT 20 AS LONG AS THE FUNCTION IS DECREASING
C ALONG THE LINE - WE SEEK A POSSIBLE MINIMUM
C
  113    IP = IP+1
         AA = AN
         IF (IP .EQ. MM1) GO TO 116
         AN = Z(IP+1,JP)
         IF (IOFFP.NE.0 .AND. AN.EQ.SPVAL) GO TO 125
         IF (AA-AN) 116,115,114
  114    IM = IM+1
         GO TO 113
  115    IM = 0
         GO TO 113
C
C FUNCTION INCREASED - TEST FOR MINIMUM ON LINE
C
  116    IF (IM .GE. MN) GO TO 118
         IS = MAX(1,IP-MN)
         IT = IP-IM-1
         IF (IS .GT. IT) GO TO 118
         DO 117 II=IS,IT
            IF (AA .GE. Z(II,JP)) GO TO 124
  117    CONTINUE
  118    IS = IP+2
         IT = MIN(M,IP+MN)
         IF (IS .GT. IT) GO TO 121
         DO 120 II=IS,IT
            IF (IOFFP.EQ.0 .OR. Z(II,JP).NE.SPVAL) GO TO 119
            IP = II-1
            GO TO 125
  119       IF (AA .GE. Z(II,JP)) GO TO 124
  120    CONTINUE
C
C WE HAVE MINIMUM ON LINE - DO TWO-DIMENSIONAL TEST FOR MINIMUM OF FIELD
C
  121    JS = MAX(1,JP-NM)
         JT = MIN(N,JP+NM)
         IS = MAX(1,IP-MN)
         IT = MIN(M,IP+MN)
         DO 123 JK=JS,JT
            IF (JK .EQ. JP) GO TO 123
            DO 122 IK=IS,IT
               IF (Z(IK,JK).LE.AA .OR.
     1             (IOFFP.NE.0 .AND. Z(IK,JK).EQ.SPVAL)) GO TO 124
  122       CONTINUE
  123    CONTINUE
C
         X = REAL(IP)
         Y = REAL(JP)
         CALL WTSTR (FX(X,Y),FY(X,Y),'L',ISIZEM,0,0 )
         CALL FL2INT (FX(X,Y),FY(X,Y),IFX,IFY)
         IFY = IFY*SCLY
         CALL ENCD (AA,ASH,IA,NC,IOFFDT)
         MY = IFY-IH1
         TMY = CPUY ( MY )
         CALL WTSTR ( FX(X,Y),TMY,IA(1:NC),ISIZEM,0,0 )
  124    IM = 1
         IF (IP-MM1) 101,127,127
C
C SKIP SPECIAL VALUES ON LINE
C
  125    IM = 0
  126    IP = IP+1
         IF (IP .GE. MM1) GO TO 127
         IF (IOFFP.NE.0 .AND. Z(IP+1,JP).EQ.SPVAL) GO TO 125
         IM = IM+1
         IF (IM .LE. MN) GO TO 126
         IM = 1
         AN = Z(IP+1,JP)
         IF (Z(IP,JP)-AN) 101,103,113
C
  127 CONTINUE
C
      RETURN
C
C ****************************** ENTRY PNTVAL **************************
C     ENTRY PNTVAL (Z,L,MM,NN,JSIZEM,AASH,JOFFDT)
C
  128 CONTINUE
      II = (M-1+24)/24
      JJ = (N-1+48)/48
      NIQ = 1
      NJQ = 1
      DO 130 J=NJQ,N,JJ
         Y = J
         DO 129 I=NIQ,M,II
            X = I
            ZZ = Z(I,J)
            IF (IOFFP.NE.0 .AND. ZZ.EQ.SPVAL) GO TO 129
            CALL ENCD (ZZ,ASH,IA,NC,IOFFDT)
            CALL WTSTR ( FX(X,Y),FY(X,Y),IA(1:NC),ISIZEM,0,0 )
  129    CONTINUE
  130 CONTINUE
      RETURN
      END
