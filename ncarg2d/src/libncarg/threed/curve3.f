C
C $Id: curve3.f,v 1.6 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CURVE3 (U,V,W,N)
      SAVE
      DIMENSION       U(N)       ,V(N)       ,W(N)
      CALL TRN32T (U(1),V(1),W(1),X,Y,ZDUM,2)
      CALL PLOTIT (32*INT(X),32*INT(Y),0)
      NN = N
      IF (NN .LT. 2) RETURN
      DO  10 I=2,NN
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL TRN32T (UU,VV,WW,X,Y,ZDUM,2)
         CALL PLOTIT (32*INT(X),32*INT(Y),1)
   10 CONTINUE
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
