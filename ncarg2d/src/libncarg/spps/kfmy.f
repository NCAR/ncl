C
C $Id: kfmy.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KFMY (RY)
C
C Given a Y coordinate RY in the fractional system, KFMY(RY) is a Y
C coordinate in the metacode system.
C
      KFMY=INT(RY*32767.)
      RETURN
      END
