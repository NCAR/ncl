C
C $Id: pcfopn.F.sed,v 1.5 1992-11-17 18:46:27 kennison Exp $
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
        CHARACTER*128 FILENM
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
  103       PRINT * , 'PCFOPN - ',FILENM(1:LENEM-1)
            STOP
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
             PRINT * , 'PCFOPN - ERROR OPENING FONT ',NFNT
             STOP
          END IF
        END IF
        RETURN
  105   PRINT * , 'PCFOPN - ERROR OPENING PWRITX DATA FILE ',FILENM
        PRINT * , 'PCFOPN - IOSTAT FROM OPEN STATEMENT IS ',IOST
        STOP
      END
