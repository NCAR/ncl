C
C $Id: slstln.f,v 1.5 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION SLSTLN (STRING)
C
C Compute the position of the first non-blank character in STRING
C from the right.
C
        CHARACTER*(*) STRING
C
        LNTH=LEN(STRING)
C
        DO 101 I=1,LNTH
          NCHR=LNTH+1-I
          IF (STRING(NCHR:NCHR).NE.' ') THEN
            SLSTLN=NCHR
            RETURN
          END IF
  101   CONTINUE
C
        SLSTLN=1
        RETURN
C
      END
