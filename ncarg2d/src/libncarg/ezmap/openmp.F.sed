C
C $Id: openmp.F.sed,v 1.5 1994-03-22 22:19:12 kennison Exp $
C
      SUBROUTINE OPENMP (IUNIT)
        CHARACTER*128 FILENM
        CHARACTER*131 CTMP
        SAVE IOPEN
        DATA FILENM / ' ' /
        DATA IOPEN  /  0  /
        IF (IOPEN.EQ.0) THEN
          CALL GNGPAT (FILENM,'SED_DBDIR',ISTAT)
          IF (ISTAT.NE.-1) THEN
            DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                FILENM(I:I+9)='/ezmapdata'
                GO TO 104
              ENDIF
 101        CONTINUE
            GO TO 105
          ELSE
            DO 102 I=2,128
              LENEM=I
              IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
 102        CONTINUE
 103        CTMP='OPENMP - '//FILENM(1:LENEM-1)
            CALL SETER (CTMP(1:MIN(LENEM+8,131)),1,1)
            RETURN
          ENDIF
#if defined(ultrix) && defined(mips)
 104      OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                                READONLY,ERR=105)
#else
 104      OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                                         ERR=105)
#endif
          IOPEN=1
        END IF
        RETURN
  105   CTMP='OPENMP - ERROR OPENING EZMAP DATA FILE '//FILENM
        DO 106 I=131,1,-1
          IF (CTMP(I:I).NE.' ') THEN
            IEND=I
            GO TO 107
          END IF
  106   CONTINUE
  107   CALL SETER (CTMP(1:IEND),2,1)
        RETURN
      END
