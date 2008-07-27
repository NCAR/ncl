C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSA3XS (NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NXO,NYO,NZO,
     +                  XO,YO,ZO,UO,NWRK,WORK,IER)
C
      DIMENSION XI(3,NI),UI(NI),WTS(*),KNOTS(3),XO(NXO),YO(NYO),
     +          ZO(NZO),UO(NXO,NYO,NZO),WORK(NWRK),NDERIV(3)
      DIMENSION XMN(3),XMX(3),XV(3)
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)*KNOTS(3)
C
      DO 20 I=1,3
        IF (KNOTS(I) .LT. 4) THEN
          CALL CFAERR (202,' CSA3XS - must have at least four knots in e
     +very coordinate direction',68)       
          IER = 202
          RETURN
        ENDIF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. NTOT*(NTOT+3)) THEN
        CALL CFAERR (203,' CSA3XS - workspace too small',29)
        IER = 202
        RETURN
      ENDIF
C
C  Calculate the min and max for the knots as the minimum value of
C  the input data and output data and the maximum of the input and
C  output data.
C
      DO 30 J=1,3
        XMN(J) = XI(J,1)
        XMX(J) = XI(J,1)
        DO 10 I=2,NI
          XMN(J) = MIN(XMN(J),XI(J,I))
          XMX(J) = MAX(XMX(J),XI(J,I))
   10   CONTINUE
   30 CONTINUE
      XMN(1) = MIN(XMN(1),XO(1))
      XMX(1) = MAX(XMX(1),XO(NXO))
      XMN(2) = MIN(XMN(2),YO(1))
      XMX(2) = MAX(XMX(2),YO(NYO))
      XMN(3) = MIN(XMN(3),ZO(1))
      XMX(3) = MAX(XMX(3),ZO(NZO))
C
C  Find the coefficients.
C
      CALL SPLCW(3,XI,3,UI,WTS,NI,XMN,XMX,KNOTS,SSMTH,WORK,NTOT,       
     +           WORK(NTOT+1),NWRK-NTOT,IERR)
      IF (IERR .NE. 0) RETURN
C
C  Calculate the approximated values (coefficients are stored at
C  the beginnig of WORK).
C
      DO 60 I=1,NXO
        XV(1) = XO(I)
        DO 40 J=1,NYO
          XV(2) = YO(J)
          DO 50 K=1,NZO
            XV(3) = ZO(K)
            UO(I,J,K) = SPLDE(3,XV,NDERIV,WORK,XMN,XMX,KNOTS,IER)
            IF (IERR .NE. 0) RETURN
   50     CONTINUE
   40   CONTINUE
   60 CONTINUE
C
      RETURN
      END
