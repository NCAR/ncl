c -------------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE QDTRNDMSG(Y,NPTS,YMSG,IOPT,IER,C)
      IMPLICIT NONE
      INTEGER NPTS, IOPT, IER
      DOUBLE PRECISION Y(NPTS)
      DOUBLE PRECISION YMSG, C(3)
C NCLEND
c                                      local
      INTEGER I
      DOUBLE PRECISION FN , SUMX , SUMY , SUMX2 , SUMX3 , SUMX4
     +               , SUMXY , SUMX2Y , XS , D , YAVE 

      IER = 0

      IF (NPTS.LT.2) IER = 1
      IF (IER.NE.0) RETURN

      FN   = 0.0D0
      SUMY = 0.0D0
      DO I = 1,NPTS
         IF (Y(I).NE.YMSG) THEN
             FN   = FN + 1.0D0
             SUMY = SUMY + Y(I)
         END IF
      END DO

      IF (FN.GT.0.0D0) THEN
          YAVE = SUMY/FN
      ELSE
          IER  = 1
          YAVE = YMSG
       END IF

      IF (FN.EQ.1.0D0) THEN
          IER = 5
      END IF

      IF (IER.NE.0) THEN
          RETURN
      END IF
c                                 remove mean
      DO I = 1,NPTS
         IF (Y(I).NE.YMSG) THEN
             Y(I)   = Y(I) - YAVE
         END IF
      END DO
c     
C QUADRATIC DETRENDING
C
      FN    = 0.0D0
      SUMX  = 0.0D0
      SUMY  = 0.0D0
      SUMX2 = 0.0D0
      SUMX3 = 0.0D0
      SUMX4 = 0.0D0
      SUMXY = 0.0D0
      SUMX2Y= 0.0D0
      DO I = 1,NPTS
         IF (Y(I).NE.YMSG) THEN
             FN    = FN + 1.0D0
             XS    = DBLE(I)
             SUMX  = SUMX  + XS
             SUMY  = SUMY  + Y(I)
             SUMX2 = SUMX2 + XS**2
             SUMX3 = SUMX3 + XS**3
             SUMX4 = SUMX4 + XS**4
             SUMXY = SUMXY + XS*Y(I)
             SUMX2Y= SUMX2Y+ XS**2*Y(I)
         END IF
      END DO
      D    = FN* (SUMX2*SUMX4-SUMX3**2) +
     +            SUMX* (SUMX3*SUMX2-SUMX*SUMX4) +
     +            SUMX2* (SUMX*SUMX3-SUMX2**2)
      C(1) = (SUMX2*SUMX4-SUMX3**2)*SUMY +
     +       (SUMX3*SUMX2-SUMX*SUMX4)*SUMXY +
     +       (SUMX*SUMX3-SUMX2**2)*SUMX2Y
      C(2) = (SUMX3*SUMX2-SUMX*SUMX4)*SUMY +
     +       (FN*SUMX4-SUMX2**2)*SUMXY +
     +       (SUMX*SUMX2-FN*SUMX3)*SUMX2Y
      C(3) = (SUMX*SUMX3-SUMX2**2)*SUMY +
     +       (SUMX*SUMX2-FN*SUMX3)*SUMXY + (FN*SUMX2-SUMX**2)*SUMX2Y
      C(1) = C(1)/D
      C(2) = C(2)/D
      C(3) = C(3)/D

      DO I = 1,NPTS
         IF (Y(I).NE.YMSG) THEN
             Y(I) = Y(I) - (C(1)+C(2)*DBLE(I)+C(3)*DBLE(I)**2)
         END IF
      END DO

C JAN 14, 2013: IOPT=1 [True] matches 'dtrend_msg'
C IOPT=1 means True in NCL
      IF (IOPT.EQ.1) RETURN

C Add the mean back in
C C C IF (IOPT.NE.0) THEN 
          DO I = 1,NPTS
             IF (Y(I).NE.YMSG) THEN
                 Y(I)   = Y(I) + YAVE
             END IF
          END DO
C C C END IF
c     
      RETURN
      END
