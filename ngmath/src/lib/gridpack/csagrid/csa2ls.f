      SUBROUTINE CSA2LS (NI,XI,UI,KNOTS,NO,XO,YO,UO,NWRK,WORK,IER)
C
      DIMENSION XI(2,NI),UI(NI),KNOTS(2),XO(NO),YO(NO),
     +          UO(NO),WORK(NWRK),NDERIV(2)
      DATA NDERIV/0,0/
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)
      IF (NTOT .GT. NI) THEN
        CALL CFAERR (201,' CSA2LS - cannot have more knots than input da
     +ta points',55)
      ENDIF
C
      DO 20 I=1,2
        IF (KNOTS(I) .LT. 4) THEN
          CALL CFAERR (202,' CSA2LS - must have at least four knots in e
     +very coordinate direction',69)       
          RETURN
        ENDIF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. NTOT*(NTOT+3)) THEN
        CALL CFAERR (203,' CSA2LS - workspace too small',29)
        RETURN
      ENDIF
C
      SMTH =   0.
      WTS =   -1.
      CALL CSA2LXS (NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,UO,
     +              NWRK,WORK,IER)
C
      RETURN
      END
