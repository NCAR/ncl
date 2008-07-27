C
C       $Id: quick.f,v 1.5 2008-07-27 00:23:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE QUICK (Z,L,M,N,NL,CL)
      SAVE
C
C QUICK CONTOURS Z
C
      DIMENSION       Z(L,N)     ,CL(NL)
C
      COMMON /CONRE1/ IOFFP      ,SPVAL
C
C     FX(X,Y) = X
C     FY(X,Y) = Y
C
      C(P1,P2,B) = B+(P1-CV)/(P1-P2)
C
      IDUB = 0
      JUMP = 2
      IF (IOFFP .NE. 0) JUMP = 1
      DO 128 JP1=2,N
         J = JP1-1
         Y1 = J
         Y2 = JP1
         DO 127 IP1=2,M
            I = IP1-1
            X1 = I
            X2 = IP1
            H1 = Z(I,J)
            H2 = Z(I,JP1)
            H3 = Z(IP1,JP1)
            H4 = Z(IP1,J)
            GO TO (101,102) , JUMP
  101       IF (H1.EQ.SPVAL .OR. H2.EQ.SPVAL .OR. H3.EQ.SPVAL .OR.
     1          H4.EQ.SPVAL) GO TO 127
  102       DO 126 K=1,NL
               CV = CL(K)
C
C FIND WHAT SITUATION FOR THIS CV IN THIS CELL
C
               IF (H1-CV) 103,110,110
  103          IF (H2-CV) 104,106,106
  104          IF (H3-CV) 105,107,107
  105          IF (H4-CV) 127,127,117
  106          IF (H3-CV) 108,109,109
  107          IF (H4-CV) 118,119,119
  108          IF (H4-CV) 120,121,121
  109          IF (H4-CV) 122,123,123
  110          IF (H2-CV) 111,113,113
  111          IF (H3-CV) 112,114,114
  112          IF (H4-CV) 123,122,122
  113          IF (H3-CV) 115,116,116
  114          IF (H4-CV) 124,120,120
  115          IF (H4-CV) 119,118,118
  116          IF (H4-CV) 117,126,126
C
C INTERPOLATE LINE SEGMENT ENDPOINTS
C
  117          XA = C(H1,H4,X1)
               YB = C(H4,H3,Y1)
               XB = X2
               YA = Y1
               GO TO 125
  118          XA = C(H2,H3,X1)
               YB = C(H4,H3,Y1)
               XB = X2
               YA = Y2
               GO TO 125
  119          XA = C(H1,H4,X1)
               XB = C(H2,H3,X1)
               YA = Y1
               YB = Y2
               GO TO 125
  120          XB = C(H2,H3,X1)
               YA = C(H1,H2,Y1)
               XA = X1
               YB = Y2
               IDUB = 0
               GO TO 125
  121          IDUB = 1
               GO TO 117
  122          YA = C(H1,H2,Y1)
               YB = C(H4,H3,Y1)
               XA = X1
               XB = X2
               GO TO 125
  123          XB = C(H1,H4,X1)
               YA = C(H1,H2,Y1)
               XA = X1
               YB = Y1
               IDUB = 0
               GO TO 125
  124          IDUB = -1
               GO TO 118
  125          XX1 = FX(XA,YA)
               YY1 = FY(XA,YA)
               XX2 = FX(XB,YB)
               YY2 = FY(XB,YB)
C
C DRAW LINE SEGMENT
C
               CALL LINED ( XX1, YY1, XX2, YY2 )
               IF (IDUB) 123,126,120
  126       CONTINUE
  127    CONTINUE
  128 CONTINUE
C
C
      RETURN
      END
