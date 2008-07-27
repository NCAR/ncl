C
C $Id: line3.f,v 1.6 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LINE3 (UA,VA,WA,UB,VB,WB)
      CALL TRN32T (UA,VA,WA,XA,YA,XDUM,2)
      CALL TRN32T (UB,VB,WB,XB,YB,XDUM,2)
      IIX = 32*INT(XB)
      IIY = 32*INT(YB)
      CALL PLOTIT (32*INT(XA),32*INT(YA),0)
      CALL PLOTIT (IIX,IIY,1)
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT (0,0,0)
      RETURN
      END
