C
C $Id: pcfred.f,v 1.7 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
