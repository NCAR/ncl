      SUBROUTINE ULIBER (IERR,MESS,LMESS)
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
C                          MESSAGE TO BE PRINTED.
C
C                        LMESS
C                          NUMBER OF CHARACTERS IN MESS (.LE. 130).
C
C ARGUMENTS
C ON OUTPUT              NONE
C
C I/O                    THE MESSAGE IS WRITEN TO UNIT 101.
C ******************************************************************
C
      REAL MESS(1)
C
      IF (IERR.NE.0) WRITE (101,1001) IERR
      NWORDS=(LMESS+7)/8
      WRITE (101,1002) (MESS(I),I=1,NWORDS)
      RETURN
C
 1001 FORMAT (6H0IERR=,I5)
 1002 FORMAT (16A8,A2)
C
      END
