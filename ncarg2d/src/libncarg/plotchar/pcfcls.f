C
C $Id: pcfcls.f,v 1.4 1995-07-07 20:09:37 kennison Exp $
C
      SUBROUTINE PCFCLS (IBNU,NFNT)
        CHARACTER*32 CTMP
        IF (NFNT.EQ.0) THEN
          CALL NGCLFI (IBNU)
        ELSE
          CALL BCLRED (IBNU,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFCLS - ERROR CLOSING FONT'',I5)') NFNT
            CALL SETER (CTMP,1,1)
            RETURN
          END IF
        END IF
        RETURN
      END
