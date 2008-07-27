C
C $Id: kfmx.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KFMX (RX)
C
C Given an X coordinate RX in the fractional system, KFMX(RX) is an X
C coordinate in the metacode system.
C
      KFMX=INT(RX*32767.)
      RETURN
      END
