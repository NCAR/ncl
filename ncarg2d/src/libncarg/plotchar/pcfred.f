C
C $Id: pcfred.f,v 1.4 1995-07-07 20:09:38 kennison Exp $
C
      SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
        CHARACTER*32 CTMP
        DIMENSION IBUF(LBUF)
        IF (NFNT.EQ.0) THEN
          CALL NGRDIN (IBNU,IBUF,LBUF,ISTA)
          IF (ISTA.NE.LBUF) THEN
            CALL SETER ('PCFRED - ERROR READING PWRITX DATABASE',2,1)
          END IF
        ELSE
          CALL PCBNRD (IBNU,LBUF,IBUF,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFRED - ERROR READING FONT'',I5)') NFNT
            CALL SETER (CTMP,1,1)
            RETURN
          END IF
        END IF
        RETURN
      END
