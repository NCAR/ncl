C
C	$Id: bound.f,v 1.4 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BOUND (Z,MX,NNX,NNY,SSP)
      DIMENSION       Z(MX,NNY)   ,PX(2)   ,PY(2)
C
C BOUND DRAWS A POLYGONAL BOUNDRY AROUND ANY SPECIAL-VALUE AREAS IN Z.
C
      SAVE
      NX = NNX
      NY = NNY
C
C VERTICAL LINES
C
      SP = SSP
      DO 103 IP1=3,NX
         I = IP1-1
         PX(1) = I
         PX(2) = I
         IM1 = I-1
         DO 102 JP1=2,NY
            PY(2) = JP1
            J = JP1-1
            PY(1) = J
            KLEFT = 0
            IF (Z(IM1,J).EQ.SP .OR. Z(IM1,JP1).EQ.SP) KLEFT = 1
            KCENT = 0
            IF (Z(I,J).EQ.SP .OR. Z(I,JP1).EQ.SP) KCENT = 1
            KRIGT = 0
            IF (Z(IP1,J).EQ.SP .OR. Z(IP1,JP1).EQ.SP) KRIGT = 1
            JUMP = KLEFT*4+KCENT*2+KRIGT+1
            GO TO (102,101,102,102,101,102,102,102,102),JUMP
  101       CALL GPL (2,PX,PY)
  102    CONTINUE
  103 CONTINUE
C
C HORIZONTAL
C
      DO 106 JP1=3,NY
         J = JP1-1
         PY(1) = J
         PY(2) = J
         JM1 = J-1
         DO 105 IP1=2,NX
            PX(2) = IP1
            I = IP1-1
            PX(1) = I
            KLOWR = 0
            IF (Z(I,JM1).EQ.SP .OR. Z(IP1,JM1).EQ.SP) KLOWR = 1
            KCENT = 0
            IF (Z(I,J).EQ.SP .OR. Z(IP1,J).EQ.SP) KCENT = 1
            KUPER = 0
            IF (Z(I,JP1).EQ.SP .OR. Z(IP1,JP1).EQ.SP) KUPER = 1
            JUMP = KLOWR*4+KCENT*2+KUPER+1
            GO TO (105,104,105,105,104,105,105,105,105),JUMP
  104       CALL GPL (2,PX,PY)
  105    CONTINUE
  106 CONTINUE
      RETURN
      END
