C
C $Id: lnbpcs.f,v 1.4 2008-07-27 00:17:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION LNBPCS (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of LNBPCS(CHRS), where CHRS is a character string of
C arbitrary length starting with a non-blank, is the position of
C the last character in CHRS which is non-blank (the "Length of
C the Non-Blank Portion of the Character String").  For an all-blank
C string, the value 1 is returned.
C
        NCHS=LEN(CHRS)
C
        DO 101 I=NCHS,1,-1
          IF (CHRS(I:I).NE.' ') THEN
            LNBPCS=I
            RETURN
          END IF
  101   CONTINUE
C
        LNBPCS=1
C
C Done.
C
        RETURN
C
      END
