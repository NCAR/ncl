C
C $Id: frst3.f,v 1.2 1993-03-14 19:01:14 kennison Exp $
C
      SUBROUTINE FRST3 (U,V,W)
      XDUM = 5.
      CALL TRN32T (U,V,W,X,Y,XDUM,2)
      CALL PLOTIT  (32*IFIX(X),32*IFIX(Y),0)
      RETURN
      END
