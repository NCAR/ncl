C
C $Id: cmfx.f,v 1.7 2008-07-27 00:17:23 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION CMFX (IX)
C
C Given an X coordinate IX in the metacode system, CMFX(IX) is an X
C coordinate in the fractional system.
C
      CMFX=REAL(IX)/32767.
      RETURN
      END
