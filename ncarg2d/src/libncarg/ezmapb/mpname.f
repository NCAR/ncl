C
C $Id: mpname.f,v 1.8 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      CHARACTER*64 FUNCTION MPNAME (IAIN)
        INTEGER      IAIN
        CHARACTER*64 MDNAME
        MPNAME=MDNAME(IAIN)
        RETURN
      END
