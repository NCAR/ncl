      SUBROUTINE ULIBER (IERR,MESS,LMESS)
C
      CHARACTER*(*) MESS
C
C SUBROUTINE ULIBER (IERR,MESS,LMESS)
C
C PURPOSE                TO PRINT AN ERROR NUMBER AND AN ERROR MESSAGE
C                        OR JUST AN ERROR MESSAGE.
C
C USAGE                  CALL ULIBER (IERR,MESS,LMESS)
C
C ARGUMENTS
C ON INPUT               IERR
C                          THE ERROR NUMBER (PRINTED ONLY IF NON-ZERO).
C
C                        MESS
C                          MESSAGE TO BE PRINTED ( < 130 CHARACTERS)
C
C                        LMESS
C                          NO LONGER USED
C
C ARGUMENTS
C ON OUTPUT              NONE
C
C I/O                    THE MESSAGE IS WRITEN TO UNIT I1MACH(4).
C
C ******************************************************************
C
      IERU=I1MACH(4)
      IF (IERR.NE.0) WRITE (IERU,1001) IERR
      WRITE (IERU,1002) MESS
      RETURN
C
 1001 FORMAT ('0IERR=',I5)
 1002 FORMAT (A)
C
      END
