C
C $Id: pcfopn.F.sed,v 1.6 1994-03-17 22:32:33 kennison Exp $
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
        CHARACTER*128 FILENM
        CHARACTER*131 CTMP
        DATA FILENM / ' ' /
        IF (NFNT.EQ.0) THEN
          CALL GNGPAT (FILENM,'SED_DBDIR',ISTAT)
          IF (ISTAT .NE. -1) THEN
            DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                FILENM(I:I+9)='/pwritdata'
                GO TO 104
              END IF
  101       CONTINUE
            GO TO 105
          ELSE
            DO 102 I=2,128
              LENEM=I
              IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
  102       CONTINUE
  103       CTMP='PCFOPN - '//FILENM(1:LENEM-1)
            CALL SETER (CTMP(1:MIN(LENEM+8,131)),1,1)
            RETURN
          END IF
#if defined(ultrix) && defined(mips)
  104     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                   IOSTAT=IOST,READONLY,ERR=105)
#else
  104     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                            IOSTAT=IOST,ERR=105)
#endif
          REWIND IBNU
        ELSE
          CALL BOFRED (IBNU,NFNT,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFOPN - ERROR OPENING FONT'',I5)') NFNT
            CALL SETER (CTMP(1:32),2,1)
            RETURN
          END IF
        END IF
        RETURN
  105   CTMP='PCFOPN - ERROR OPENING PWRITX DATA FILE '//FILENM
        DO 106 I=131,1,-1
          IF (CTMP(I:I).NE.' ') THEN
            IEND=I
            GO TO 107
          END IF
  106   CONTINUE
  107   CALL SETER (CTMP(1:IEND),3,1)
        RETURN
      END
