C
C $Id: pcfcls.f,v 1.7 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
