C
C	$Id: hststr.f,v 1.1.1.1 1992-04-17 22:31:53 ncargd Exp $
C
C
C
C *****************************************************
C
        SUBROUTINE HSTSTR(LABEL,FIRST,LAST)
C
C *****************************************************
C
C  THIS CALCULATES THE POSITION OF THE FIRST NON-BLANK CHARACTER
C  AND THE POSITION OF THE LAST NON-BLANK CHARACTER IN LABEL
C
      INTEGER   FIRST, LAST
        CHARACTER*(*) LABEL
C
        LAST = LEN(LABEL)
        DO 860 I = 1,LAST
            IF (LABEL(I:I) .NE. ' ') GOTO 870
 860  CONTINUE
 870  FIRST = I
        IF (FIRST .NE. LAST) THEN
            DO 880 J = LAST,FIRST,-1
                  IF (LABEL(J:J) .NE. ' ') THEN
                  LAST = J
                  GOTO 890
                  ENDIF
 880      CONTINUE
 890  CONTINUE
        ENDIF
      RETURN
        END
