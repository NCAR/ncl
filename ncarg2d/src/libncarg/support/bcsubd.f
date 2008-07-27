C
C $Id: bcsubd.f,v 1.4 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCSUBD(BX,BY,SX,SY,TX,TY)
C
C  Do a midpoint subdivision of the input Bezier curve whose control 
C  points are in BX and BY to create two Bezier curves whose control
C  points are in SX, SY and TX, TY.
C
      DIMENSION BX(4),BY(4),SX(4),SY(4),TX(4),TY(4)
C
      SX(1) = BX(1)
      SY(1) = BY(1)
      SX(2) = .5*(BX(1)+BX(2))
      SY(2) = .5*(BY(1)+BY(2))
      TMPX  = .5*(BX(2)+BX(3))
      TMPY  = .5*(BY(2)+BY(3))
      SX(3) = .5*(SX(2)+TMPX)
      SY(3) = .5*(SY(2)+TMPY)
      TX(4) = BX(4)
      TY(4) = BY(4)
      TX(3) = .5*(BX(3)+BX(4))
      TY(3) = .5*(BY(3)+BY(4))
      TX(2) = .5*(TMPX+TX(3))
      TY(2) = .5*(TMPY+TY(3))
      SX(4) = .5*(SX(3)+TX(2))
      SY(4) = .5*(SY(3)+TY(2))
      TX(1) = SX(4)
      TY(1) = SY(4)
C
      RETURN
      END
