C
C $Id: slstln.f,v 1.2 1995-07-28 18:38:11 kennison Exp $
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
