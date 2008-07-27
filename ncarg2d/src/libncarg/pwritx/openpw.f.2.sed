C
C   $Id: openpw.f.2.sed,v 1.2 2008-07-27 03:53:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE OPENPW (IUNIT)
      CHARACTER*128 FILENM
          DATA FILENM / ' ' /
      SAVE IOPEN
      DATA IOPEN / 0 /
      IF (IOPEN.EQ.0) THEN
      CALL GNGPAT (FILENM,'SED_DBDIR',ISTAT)
      IF (ISTAT .NE. -1) THEN
          DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                  FILENM(I:I+9)='/pwritdata'
                  GO TO 104
              END IF
 101      CONTINUE
          GO TO 105
      ELSE
          DO 102 I=2,128
              LENEM=I
              IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
 102     CONTINUE
 103     PRINT * , 'OPENPW - ',FILENM(1:LENEM-1)
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
      END IF
      RETURN
 105  WRITE (6,*) 'ERROR IN OPENING ',FILENM
      STOP
      END
