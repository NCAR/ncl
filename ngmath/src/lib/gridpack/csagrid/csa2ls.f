C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSA2LS (NI,XI,UI,KNOTS,NO,XO,YO,UO,NWRK,WORK,IER)
C
      DIMENSION XI(2,NI),UI(NI),KNOTS(2),XO(NO),YO(NO),
     +          UO(NO),WORK(NWRK),NDERIV(2)
      DATA NDERIV/0,0/
C
      NTOT = KNOTS(1)*KNOTS(2)
      DO 20 I=1,2
        IF (KNOTS(I) .LT. 4) THEN
          CALL CFAERR (202,' CSA2LS - must have at least four knots in e
     +very coordinate direction',69)       
          IER = 202
          RETURN
        ENDIF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. NTOT*(NTOT+3)) THEN
        CALL CFAERR (203,' CSA2LS - workspace too small',29)
        IER = 203
        RETURN
      ENDIF
C
      SSMTH =   0.
      WTS =   -1.
      CALL CSA2LXS (NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,UO,
     +              NWRK,WORK,IER)
C
      RETURN
      END
