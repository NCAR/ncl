C
C   $Id: openpw.F.sed,v 1.1 1992-09-25 19:35:56 ncargd Exp $
C
      SUBROUTINE OPENPW (IUNIT)
      CHARACTER*128 FILENM
      SAVE IOPEN
      DATA IOPEN / 0 /
      IF (IOPEN.EQ.0) THEN
      CALL GNGPAT (FILENM,'SED_DBDIR',ISTATUS)
      IF (ISTATUS .NE. -1) THEN
          DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                  FILENM(I:I+9)='/pwritdata'
                  GO TO 102
              END IF
 101      CONTINUE
          GO TO 104
      ELSE
         GO TO 103
      ENDIF
#if defined(ultrix) && defined(mips)
 102  OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +    READONLY,ERR=104)
#else
 102  OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +    ERR=104)
#endif
      IOPEN=1
      END IF
      RETURN
 103  WRITE (6,*) FILENM
      GO TO 105
 104  WRITE (6,*) 'ERROR IN OPENING ',FILENM
 105  CONTINUE
      STOP
      END
