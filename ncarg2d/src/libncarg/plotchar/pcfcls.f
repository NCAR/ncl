C
C       $Id: pcfcls.f,v 1.2 1992-11-17 18:46:17 kennison Exp $
C
      SUBROUTINE PCFCLS (IBNU,NFNT)
        IF (NFNT.EQ.0) THEN
          CLOSE (UNIT=IBNU,STATUS='KEEP',ERR=101)
        ELSE
          CALL BCLRED (IBNU,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            PRINT * , 'PCFCLS - ERROR CLOSING FONT ',NFNT
            STOP
          END IF
        END IF
        RETURN
  101   PRINT * , 'PCFCLS - ERROR CLOSING PWRITX DATABASE'
        STOP
      END
