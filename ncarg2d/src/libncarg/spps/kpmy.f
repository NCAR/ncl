C
C $Id: kpmy.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KPMY (IY)
C
C Given a Y coordinate IY in the plotter system, KPMY(IY) is a Y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMY=INT(32767.*REAL(IY-1)/(2.**MY-1.))
      RETURN
      END
