C
C $Id: bcftol.f,v 1.1 1999-09-21 17:07:32 kennison Exp $
C
      REAL FUNCTION BCFTOL(BX,BY)
C
C  Returns the maximum undirected distance that the line connecting 
C  the points  (BX(1),BY(1))  and  (BX(4),BY(4))  is away from the
C  actual Bezier curve whose control points are given in BX and BY.
C
      DIMENSION BX(4),BY(4)
C
      D1 = BCDP2L(BX(2),BY(2),BX(1),BY(1),BX(4),BY(4))
      D2 = BCDP2L(BX(3),BY(3),BX(1),BY(1),BX(4),BY(4))
C 
      BCFTOL = MAX(D1,D2)
C
      RETURN
      END
