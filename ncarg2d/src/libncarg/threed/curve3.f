C
C $Id: curve3.f,v 1.2 1993-03-14 19:01:06 kennison Exp $
C
      SUBROUTINE CURVE3 (U,V,W,N)
      SAVE
      DIMENSION       U(N)       ,V(N)       ,W(N)
      CALL TRN32T (U(1),V(1),W(1),X,Y,ZDUM,2)
      CALL PLOTIT (32*IFIX(X),32*IFIX(Y),0)
      NN = N
      IF (NN .LT. 2) RETURN
      DO  10 I=2,NN
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL TRN32T (UU,VV,WW,X,Y,ZDUM,2)
         CALL PLOTIT (32*IFIX(X),32*IFIX(Y),1)
   10 CONTINUE
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
