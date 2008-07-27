C
C $Id: kfpx.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KFPX (RX)
C
C Given an X coordinate RX in the fractional system, KFPX(RX) is an X
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KFPX=1+INT(RX*(2.**MX-1.))
      RETURN
      END
