C
C $Id: kfpy.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KFPY (RY)
C
C Given a Y coordinate RY in the fractional system, KFPY(RY) is a Y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KFPY=1+INT(RY*(2.**MX-1.))
      RETURN
      END
