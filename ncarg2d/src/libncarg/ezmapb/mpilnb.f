C
C $Id: mpilnb.f,v 1.7 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MPILNB (CHRS)
        CHARACTER*(*) CHRS
        MPILNB=MDILNB(CHRS)
        RETURN
      END
