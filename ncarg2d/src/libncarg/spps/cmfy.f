C
C $Id: cmfy.f,v 1.7 2008-07-27 00:17:23 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION CMFY (IY)
C
C Given a Y coordinate IY in the metacode system, CMFY(IY) is a Y
C coordinate in the fractional system.
C
      CMFY=REAL(IY)/32767.
      RETURN
      END
