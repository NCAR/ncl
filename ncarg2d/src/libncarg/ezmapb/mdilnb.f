C
C $Id: mdilnb.f,v 1.2 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDILNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MDILNB(CHRS) is the index of the last non-blank in the
C character string CHRS.
C
C Declare local variables.
C
        INTEGER I
C
        DO 101 I=LEN(CHRS),1,-1
          IF (CHRS(I:I).NE.' ') THEN
            MDILNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MDILNB=1
C
        RETURN
C
      END
