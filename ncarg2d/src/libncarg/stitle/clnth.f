C
C	$Id: clnth.f,v 1.1.1.1 1992-04-17 22:33:01 ncargd Exp $
C
      INTEGER FUNCTION CLNTH(STRING)
C
C  Compute the position of the first non-blank character in the
C  STRING from the right.
C
      CHARACTER*(*) STRING
C
      ILNTH = LEN(STRING)
      DO 10 I=1,ILNTH
      NCHR = ILNTH+1-I
      IF (STRING(NCHR:NCHR) .NE. ' ') THEN
        CLNTH = NCHR
        RETURN
      ENDIF
   10 CONTINUE
      CLNTH = 1
      RETURN
      END
