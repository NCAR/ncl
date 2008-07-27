C
C $Id: vect3.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VECT3 (U,V,W)
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      IIX = 32*INT(X)
      IIY = 32*INT(Y)
      CALL PLOTIT  (IIX,IIY,1)
      RETURN
      END
