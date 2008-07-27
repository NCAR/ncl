C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSA3D(NI,XI,UI,KNOTS,NXO,NYO,NZO,XO,YO,ZO,UO,NWRK,WORK,
     +                 IER)
      DOUBLE PRECISION XI
      DOUBLE PRECISION UI
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION ZO
      DOUBLE PRECISION UO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION WTS
      DOUBLE PRECISION SSMTH
C
      DIMENSION XI(3,NI),UI(NI),KNOTS(3),XO(NXO),YO(NYO),ZO(NZO),
     +          UO(NXO,NYO,NZO),WORK(NWRK),NDERIV(3)
      DATA NDERIV/0,0,0/
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)*KNOTS(3)
C
      DO 20 I = 1,3
          IF (KNOTS(I).LT.4) THEN
              CALL CFAERR(202,
     +' CSA3D - must have at least four knots in every coordinate direct
     +ion',68)
              IER = 202
              RETURN
          END IF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK.LT.NTOT* (NTOT+3)) THEN
          CALL CFAERR(203,' CSA3D - workspace too small',28)
          IER = 203
          RETURN
      END IF
C
C  Invoke the expanded function.
C
      WTS = -1.D0
      SSMTH = 0.D0
      CALL CSA3XD(NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NXO,NYO,NZO,XO,YO,ZO,
     +            UO,NWRK,WORK,IER)
      IF (IERR.NE.0) RETURN
C
      RETURN
      END
