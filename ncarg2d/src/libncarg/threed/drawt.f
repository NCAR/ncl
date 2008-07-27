C
C $Id: drawt.f,v 1.6 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DRAWT (IXA,IYA,IXB,IYB)
      CALL PLOTIT(32*IXA,32*IYA,0)
      IIX = 32*IXB
      IIY = 32*IYB
      CALL PLOTIT(IIX,IIY,1)
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
