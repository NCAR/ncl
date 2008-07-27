C
C $Id: mdifnb.f,v 1.2 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDIFNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MDIFNB(CHRS) is the index of the first non-blank in the
C character string CHRS.
C
C Declare local variables.
C
        INTEGER I
C
        DO 101 I=1,LEN(CHRS)
          IF (CHRS(I:I).NE.' ') THEN
            MDIFNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MDIFNB=1
C
        RETURN
C
      END
