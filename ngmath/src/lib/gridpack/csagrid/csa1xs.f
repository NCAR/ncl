      SUBROUTINE CSA1XS (NI,XI,YI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,
     +                   NWRK,WORK,IER)
C
      DIMENSION XI(NI),YI(NI),WTS(*),XO(NO),YO(NO),WORK(NWRK)
C
C  Check on the number of knots.
C
      IF (KNOTS .LT. 4) THEN
        CALL CFAERR (202,' CSA1S - must have at least four knots',38)       
      ENDIF
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. KNOTS*(KNOTS+3)) THEN
        CALL CFAERR (203,' CSA1S - workspace too small',28)
      ENDIF
C
C  Calculate the min and max for the knots as the minimum value of
C  the input data and output data and the maximum of the input and
C  output data.
C
      XMN = XI(1)
      XMX = XI(1)
      DO 10 I=2,NI
        XMN = MIN(XMN,XI(I))
        XMX = MAX(XMX,XI(I))
   10 CONTINUE
      XMN = MIN(XMN,XO(1))
      XMX = MAX(XMX,XO(NO))
C
C  Find the coefficients.
C
      CALL SPLCW(1,XI,1,YI,WTS,NI,XMN,XMX,KNOTS,SSMTH,WORK,KNOTS,       
     +           WORK(KNOTS+1),NWRK-KNOTS,IERR)
      IF (IERR .NE. 0) RETURN
C
C  Calculate the approximated values.
C
      DO 20 I=1,NO
        YO(I) = SPLDE(1,XO(I),NDERIV,WORK,XMN,XMX,KNOTS,IER)
   20 CONTINUE
C
      RETURN
      END
