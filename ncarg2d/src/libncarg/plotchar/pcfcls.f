C
C $Id: pcfcls.f,v 1.3 1994-03-17 22:05:00 kennison Exp $
C
      SUBROUTINE PCFCLS (IBNU,NFNT)
        CHARACTER*32 CTMP
        IF (NFNT.EQ.0) THEN
          CLOSE (UNIT=IBNU,STATUS='KEEP',ERR=101)
        ELSE
          CALL BCLRED (IBNU,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFCLS - ERROR CLOSING FONT'',I5)') NFNT
            CALL SETER (CTMP,1,1)
            RETURN
          END IF
        END IF
        RETURN
  101   CALL SETER ('PCFCLS - ERROR CLOSING PWRITX DATABASE',2,1)
        RETURN
      END
