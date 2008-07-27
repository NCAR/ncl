C
C $Id: bcftol.f,v 1.4 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
