C
C $Id: frst3.f,v 1.6 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FRST3 (U,V,W)
      XDUM = 5.
      CALL TRN32T (U,V,W,X,Y,XDUM,2)
      CALL PLOTIT  (32*INT(X),32*INT(Y),0)
      RETURN
      END
