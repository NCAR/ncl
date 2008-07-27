C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSA1S (NI,XI,YI,KNOTS,NO,XO,YO,NWRK,WORK,IER)
C
      DIMENSION XI(NI),YI(NI),XO(NO),YO(NO),WORK(NWRK)
C
C  Check on the number of knots.
C

      IF (KNOTS .LT. 4) THEN
        CALL CFAERR (202,' CSA1S - must have at least four knots',38)       
        IER = 202
        RETURN
      ENDIF
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. KNOTS*(KNOTS+3)) THEN
        CALL CFAERR (203,' CSA1S - workspace too small',28)
        IER = 203
        RETURN
      ENDIF
C
C  Call the expanded entry.
C
      WTS = -1.
      SSMTH = 0.
      NDERIV = 0
      CALL CSA1XS (NI,XI,YI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,
     +             NWRK,WORK,IER)
C
      RETURN
      END
