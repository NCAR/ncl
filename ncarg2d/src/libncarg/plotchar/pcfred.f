C
C $Id: pcfred.f,v 1.2 1992-11-17 18:46:30 kennison Exp $
C
      SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
        DIMENSION IBUF(LBUF)
        IF (NFNT.EQ.0) THEN
          READ (IBNU,ERR=101) (IBUF(I),I=1,LBUF)
        ELSE
          CALL PCBNRD (IBNU,LBUF,IBUF,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            PRINT * , 'PCFRED - ERROR READING FONT ',NFNT
            STOP
          END IF
        END IF
        RETURN
  101   PRINT * , 'PCFRED - ERROR READING PWRITX DATABASE'
        STOP
      END
