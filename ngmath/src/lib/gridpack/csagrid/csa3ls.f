      SUBROUTINE CSA3LS (NI,XI,UI,KNOTS,NO,XO,YO,ZO,UO,NWRK,WORK,IER)
C
C  Alternate entry where the output points are specified as a list.
C
      DIMENSION XI(3,NI),UI(NI),KNOTS(3),XO(NO),YO(NO),
     +          ZO(NO),UO(NO),WORK(NWRK),NDERIV(3)
      DATA NDERIV/0,0,0/
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)*KNOTS(3)
C
      DO 20 I=1,3
        IF (KNOTS(I) .LT. 4) THEN
          CALL CFAERR (202,' CSA3LS - must have at least four knots in e
     +very coordinate direction',69)       
        ENDIF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. NTOT*(NTOT+3)) THEN
        CALL CFAERR (203,' CSA3LS - workspace too small',29)
      ENDIF
C
C  Invoke the expanded function.
C
      WTS = -1.
      SSMTH = 0.
      CALL CSA3LXS (NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,ZO,UO,
     +              NWRK,WORK,IER)
      IF (IERR .NE. 0) RETURN
C
      RETURN
      END
