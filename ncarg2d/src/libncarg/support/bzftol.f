C
C	$Id: bzftol.f,v 1.2 1992-11-17 19:10:06 fred Exp $
C
      REAL FUNCTION BZFTOL(BX,BY)
C
C  Returns the maximum undirected distance that the line connecting 
C  the points  (BX(1),BY(1))  and  (BX(4),BY(4))  is away from the
C  actual Bezier curve whose control points are given in BX and BY.
C
      DIMENSION BX(4),BY(4)
C
      D1 = BZDP2L(BX(2),BY(2),BX(1),BY(1),BX(4),BY(4))
      D2 = BZDP2L(BX(3),BY(3),BX(1),BY(1),BX(4),BY(4))
C 
      BZFTOL = MAX(D1,D2)
C
      RETURN
      END
