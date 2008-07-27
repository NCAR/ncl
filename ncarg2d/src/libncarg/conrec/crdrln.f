C
C       $Id: crdrln.f,v 1.6 2008-07-27 00:16:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CRDRLN (Z,L,MM,NN)
      SAVE
      DIMENSION       Z(L,NN)
C
C THIS ROUTINE TRACES A CONTOUR LINE WHEN GIVEN THE BEGINNING BY STLINE.
C TRANSFORMATIONS CAN BE ADDED BY DELETING THE STATEMENT FUNCTIONS FOR
C FX AND FY IN CRDRLN AND MINMAX AND ADDING EXTERNAL FUNCTIONS.
C X=1. AT Z(1,J), X=REAL(M) AT Z(M,J). X TAKES ON NON-INTEGER VALUES.
C Y=1. AT Z(I,1), Y=REAL(N) AT Z(I,N). Y TAKES ON NON-INTEGER VALUES.
C
      COMMON /CONRE2/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(2000)   ,NR
      COMMON /CONRE1/ IOFFP      ,SPVAL
      COMMON /CONRE3/ IXBITS     ,IYBITS
      LOGICAL         IPEN       ,IPENO
C
C
C     STATEMENT FUNCTIONS TO BE REPLACED IF NON-IDENTITY
C     TRANSFORMATIONS ARE DESIRED.
C
C     FX(X,Y) = X
C     FY(X,Y) = Y
C
      IXYPAK(IXX,IYY) = ISHIFT(IXX,IYBITS)+IYY
      C(P1,P2) = (P1-CV)/(P1-P2)
C
      DATA IPEN,IPENO/.TRUE.,.TRUE./
C
      M = MM
      N = NN
      IF (IOFFP .EQ. 0) GO TO 101
      JUMP=1
      GO TO 102
  101 JUMP=2
  102 IX0 = IX
      IY0 = IY
      IS0 = IS
      IF (IOFFP .EQ. 0) GO TO 103
      IX2 = IX+INX(IS)
      IY2 = IY+INY(IS)
      IPEN = Z(IX,IY).NE.SPVAL .AND. Z(IX2,IY2).NE.SPVAL
      IPENO = IPEN
  103 IF (IDX .EQ. 0) GO TO 104
      Y = IY
      ISUB = IX+IDX
      X = C(Z(IX,IY),Z(ISUB,IY))*REAL(IDX)+REAL(IX)
      GO TO 105
  104 X = IX
      ISUB = IY+IDY
      Y = C(Z(IX,IY),Z(IX,ISUB))*REAL(IDY)+REAL(IY)
  105 CALL FRSTD (FX(X,Y),FY(X,Y))
  106 IS = IS+1
      IF (IS .GT. 8) IS = IS-8
      IDX = INX(IS)
      IDY = INY(IS)
      IX2 = IX+IDX
      IY2 = IY+IDY
      IF (ISS .NE. 0) GO TO 107
      IF (IX2.GT.M .OR. IY2.GT.N .OR. IX2.LT.1 .OR. IY2.LT.1) GO TO 120
  107 IF (CV-Z(IX2,IY2)) 108,108,109
  108 IS = IS+4
      IX = IX2
      IY = IY2
      GO TO 106
  109 IF (IS/2*2 .EQ. IS) GO TO 106
      GO TO (110,112) , JUMP
  110 ISBIG = IS+(8-IS)/6*8
      IX3 = IX+INX(ISBIG-1)
      IY3 = IY+INY(ISBIG-1)
      IX4 = IX+INX(ISBIG-2)
      IY4 = IY+INY(ISBIG-2)
      IPENO = IPEN
      IF (ISS .NE. 0) GO TO 111
      IF (IX3.GT.M .OR. IY3.GT.N .OR. IX3.LT.1 .OR. IY3.LT.1) GO TO 120
      IF (IX4.GT.M .OR. IY4.GT.N .OR. IX4.LT.1 .OR. IY4.LT.1) GO TO 120
  111 IPEN = Z(IX,IY).NE.SPVAL .AND. Z(IX2,IY2).NE.SPVAL .AND.
     1       Z(IX3,IY3).NE.SPVAL .AND. Z(IX4,IY4).NE.SPVAL
  112 IF (IDX .EQ. 0) GO TO 113
      Y = IY
      ISUB = IX+IDX
      X = C(Z(IX,IY),Z(ISUB,IY))*REAL(IDX)+REAL(IX)
      GO TO 114
  113 X = IX
      ISUB = IY+IDY
      Y = C(Z(IX,IY),Z(IX,ISUB))*REAL(IDY)+REAL(IY)
  114 GO TO (115,117) , JUMP
  115 IF (.NOT.IPEN) GO TO 118
      IF (IPENO) GO TO 116
C
C END OF LINE SEGMENT
C
      CALL LASTD
      CALL FRSTD (FX(XOLD,YOLD),FY(XOLD,YOLD))
C
C CONTINUE LINE SEGMENT
C
  116 CONTINUE
  117 CALL VECTD (FX(X,Y),FY(X,Y))
  118 XOLD = X
      YOLD = Y
      IF (IS .NE. 1) GO TO 119
      NP = NP+1
      IF (NP .GT. NR) GO TO 120
      IR(NP) = IXYPAK(IX,IY)
  119 IF (ISS .EQ. 0) GO TO 106
      IF (IX.NE.IX0 .OR. IY.NE.IY0 .OR. IS.NE.IS0) GO TO 106
C
C END OF LINE
C
  120 CALL LASTD
      RETURN
      END
