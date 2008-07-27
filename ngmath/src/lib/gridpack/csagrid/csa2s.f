C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSA2S (NI,XI,UI,KNOTS,NXO,NYO,XO,YO,UO,
     +                  NWRK,WORK,IER)
C
      DIMENSION XI(2,NI),UI(NI),KNOTS(2),XO(NXO),YO(NYO),
     +          UO(NXO,NYO),WORK(NWRK),NDERIV(2)
      DATA NDERIV/0,0/
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)
C
      DO 20 I=1,2
        IF (KNOTS(I) .LT. 4) THEN
          CALL CFAERR (202,' CSA2S - must have at least four knots in ev
     +ery coordinate direction',68)       
          IER = 202
          RETURN
        ENDIF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. NTOT*(NTOT+3)) THEN
        CALL CFAERR (203,' CSA2S - workspace too small',28)
        IER = 203
        RETURN
      ENDIF
C
C  Call the expanded entry.
C
      WTS = -1.
      SSMTH = 0.
      CALL CSA2XS (NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,
     +             NXO,NYO,XO,YO,UO,NWRK,WORK,IER)
      IF (IERR .NE. 0) RETURN
C
      RETURN
      END
