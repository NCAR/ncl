C
C $Id: pcfopn.f.sed,v 1.1 1995-07-07 20:09:38 kennison Exp $
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
        CHARACTER*128 FLNM
        CHARACTER*131 CTMP
        IF (NFNT.EQ.0) THEN
          FLNM=' '
          CALL GNGPAT (FLNM,'SED_DBDIR',ISTA)
          IF (ISTA .NE. -1) THEN
            DO 101 I=1,115
              IF (FLNM(I:I).EQ.CHAR(0)) THEN
                FLNM(I:I+13)='/PlotcharData'//CHAR(0)
                GO TO 104
              END IF
  101       CONTINUE
            GO TO 105
          ELSE
            DO 102 I=2,128
              LENEM=I
              IF (FLNM(I:I).EQ.CHAR(0)) GO TO 103
  102       CONTINUE
  103       CTMP='PCFOPN - '//FLNM(1:LENEM-1)
            CALL SETER (CTMP(1:MIN(LENEM+8,131)),1,1)
            RETURN
          END IF
  104     CALL NGOFRO (FLNM,IBNU,ISTA)
          IF (ISTA.NE.0) GO TO 105
        ELSE
          CALL BOFRED (IBNU,NFNT,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFOPN - ERROR OPENING FONT'',I5)') NFNT
            CALL SETER (CTMP(1:32),2,1)
            RETURN
          END IF
        END IF
        RETURN
  105   CTMP='PCFOPN - ERROR OPENING PWRITX DATA FILE '//FLNM
        DO 106 I=131,1,-1
          IF (CTMP(I:I).NE.' ') THEN
            IEND=I
            GO TO 107
          END IF
  106   CONTINUE
  107   CALL SETER (CTMP(1:IEND),3,1)
        RETURN
      END
