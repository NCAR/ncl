C
C   $Id: openmp.F.sed,v 1.4 1993-12-03 21:14:00 kennison Exp $
C
      SUBROUTINE OPENMP (IUNIT)
      CHARACTER*128 FILENM
          DATA FILENM / ' ' /
      SAVE IOPEN
      DATA IOPEN / 0 /
      IF (IOPEN.EQ.0) THEN
      CALL GNGPAT (FILENM,'SED_DBDIR',ISTAT)
      IF (ISTAT .NE. -1) THEN
          DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                  FILENM(I:I+9)='/ezmapdata'
                  GOTO 104
              ENDIF
 101      CONTINUE
         GO TO 105
      ELSE
              DO 102 I=2,128
              LENEM=I
                  IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
 102      CONTINUE
 103      PRINT * , 'OPENMP - ',FILENM(1:LENEM-1)
          STOP
      ENDIF
#if defined(ultrix) && defined(mips)
 104  OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +    READONLY,ERR=105)
#else
 104  OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +    ERR=105)
#endif
      IOPEN=1
      ENDIF
      RETURN
 105  WRITE (6,*) 'ERROR OPENING EZMAP DATA FILE - FILE NAME: ',FILENM
      STOP
      END
