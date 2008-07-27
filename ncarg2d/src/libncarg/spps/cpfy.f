C
C $Id: cpfy.f,v 1.7 2008-07-27 00:17:23 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION CPFY (IY)
C
C Given a Y coordinate IY in the plotter system, CPFY(IY) is a Y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      CPFY=REAL(IY-1)/(2.**MY-1.)
      RETURN
      END
