C
C $Id: vect3.f,v 1.2 1993-03-14 19:01:32 kennison Exp $
C
      SUBROUTINE VECT3 (U,V,W)
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      IIX = 32*IFIX(X)
      IIY = 32*IFIX(Y)
      CALL PLOTIT  (IIX,IIY,1)
      RETURN
      END
