C
C $Id: mpfnme.f,v 1.8 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      CHARACTER*128 FUNCTION MPFNME (IAIN,ILVL)
        INTEGER       IAIN,ILVL
        CHARACTER*128 MDFNME
        MPFNME=MDFNME(IAIN,ILVL)
        RETURN
      END
