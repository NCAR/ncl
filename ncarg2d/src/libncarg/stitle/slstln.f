C
C $Id: slstln.f,v 1.1 1993-01-14 00:30:04 kennison Exp $
C
      INTEGER FUNCTION SLSTLN(STRING)
C
C  Compute the position of the first non-blank character in the
C  STRING from the right.
C
      CHARACTER*(*) STRING
C
      ILNTH = LEN(STRING)
C
      DO 101 I=1,ILNTH
        NCHR = ILNTH+1-I
        IF (STRING(NCHR:NCHR) .NE. ' ') THEN
          SLSTLN = NCHR
          RETURN
        ENDIF
  101 CONTINUE
C
      SLSTLN = 1
      RETURN
C
      END
