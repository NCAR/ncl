C
C $Id: hststr.f,v 1.6 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
