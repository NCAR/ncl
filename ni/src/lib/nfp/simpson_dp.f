C NCLFORTSTART
      SUBROUTINE DSIMPEQ (NUM,DX,Y,ANS)
      IMPLICIT NONE
      INTEGER  NUM
      DOUBLE PRECISION DX,Y(NUM),ANS
C NCLEND
C
C THE NSSL "simpsn.f" DOES NOT WORK. THIS IS NOW JUST A FRONT-END
C DRIVER THAT ALLOWS THE SAME INTERFACE TO BE USED BUT IT INVOKES
C THE *SLIGHTLY* LESS ACCURATE "simpne.f" TO PERFORM THE INTEGRATION.
C
C SOURCE                 NCAR's NSSL [simpsn.f]
C ARGUMENTS
C
C ON INPUT               DX
C                          A SINGLE VARIABLE SPECIFYING THE INCREMENT
C                          FOR THE EQUALLY SPACED ABSCISSAS.
C
C                        Y
C                          VECTOR OF ORDINATES.
C
C                        NUM
C                          LENGTH OF VECTOR Y (SHOULD BE .GT. 2).
C
C ON OUTPUT              ANS
C                          ANSWER [set to -9999. if error]
C
C ALGORITHM              SIMPSON'S RULE AND LAGRANGIAN INTERPOLATION
C
C ACCURACY               FOR EQUALLY SPACED ABSCISSAE THE ERROR IS
C                        PROPORTIONAL TO  H**4 TIMES THE FOURTH
C                        DERIVATIVE OF F  AT C, WHERE C IS SOME
C                        POINT WITHIN THE LIMITS OF INTEGRATION.
C                        FOR UNEQUALLY SPACED ABSCISSAE THE ERROR
C                        IS PROPORTIONAL TO THE FOURTH DERIVATIVE
C                        OF F AT X TIMES THE FOURTH POWER OF THE
C                        MAXIMUM DISTANCE BETWEEN ADJACENT ABSCISSAE.
C **********************************************************************

C LOCAL
      DOUBLE PRECISION X(NUM)
      INTEGER I

C CREATE A 1D EQUALLY SPACED ARRAY
      DO I=1,NUM
         X(I) = DBLE(I-1)*DX
      END DO

C INVOKE THE SIMPSON RULE FOR UNEQUALLY SPACED DATA
      CALL DSIMPNE2(NUM,X,Y,ANS)

      RETURN
      END
C ========================================================
C NCLFORTSTART
      SUBROUTINE DSIMPNE(NUM,X,Y,YMSG,ANS)
      IMPLICIT NONE
      INTEGER  NUM
      DOUBLE PRECISION X(NUM), Y(NUM), ANS, YMSG
C NCLEND
      INTEGER N,NUSE
      DOUBLE PRECISION XX(NUM), YY(NUM)

C STRIP MSG VALUES
      NUSE = 0
      DO N=1,NUM
         IF (Y(N).NE.YMSG) THEN
             NUSE = NUSE + 1
             XX(NUSE) = X(N)
             YY(NUSE) = Y(N)
         END IF
      END DO

      CALL DSIMPNE2(NUSE,XX,YY,ANS)

      RETURN
      END
C ---------------------------------------------------------
      SUBROUTINE DSIMPNE2(NUM,X,Y,ANS)
      IMPLICIT NONE
      INTEGER  NUM
      DOUBLE PRECISION X(NUM), Y(NUM), ANS
C
C SOURCE                 NCAR's NSSL [simpne.f]
C
C DIMENSION OF           X(NUM),Y(NUM)
C ARGUMENTS
C
C PURPOSE                TO INTEGRATE OVER A GIVEN SET OF ORDINATES
C                        BY ANALYTICALLY INTEGRATING A 3-POINT
C                        LAGRANGIAN INTERPOLATION POLYNOMIAL
C                        FITTING THE ORDINATES.
C
C ARGUMENTS
C
C ON INPUT               X
C                          THIS IS A VECTOR OF LENGTH NUM SPECIFYING
C                          THE UNEQUALLY SPACED ABSCISSAS.
C
C                        Y
C                          VECTOR OF ORDINATES.
C
C                        NUM
C                          LENGTH OF VECTOR Y (SHOULD BE .GT. 2).
C
C **********************************************************************
      DOUBLE PRECISION DEL(3),PI(3),G(3),THIRD,HALF,TWO3R
      DOUBLE PRECISION X1,X2,X3,E,F,FEINTS,DELPRD,SUMT
      INTEGER IER,N,I

      IF (NUM.LE.2) THEN
          IER = 32
          ANS = -9999.D0
          RETURN
      END IF

      IER = 0
      N   = 1
      ANS = 0.D0

      THIRD = 1.D0/3.D0
      HALF  = 1.D0/2.D0
      TWO3R = 2.D0/3.D0

  105 CONTINUE
      X1 = X(N)
      X2 = X(N+1)
      X3 = X(N+2)
      E  = X3*X3 - X1*X1
      F  = X3*X3*X3 - X1*X1*X1
      FEINTS = X3 - X1
      DEL(1) = X3 - X2
      DEL(2) = -FEINTS
      DEL(3) = X2 - X1
      G(1)   = X2 + X3
      G(2)   = X1 + X3
      G(3)   = X1 + X2
      PI(1)  = X2*X3
      PI(2)  = X1*X3
      PI(3)  = X1*X2
      DELPRD = DEL(1)*DEL(2)*DEL(3)

      SUMT = 0.D0
      DO I = 1,3
          SUMT = SUMT + Y(N-1+I)*DEL(I)*
     +           (THIRD*F-G(I)*HALF*E+PI(I)*FEINTS)
      END DO
      ANS = ANS - SUMT/DELPRD

      N = N + 2
      IF (NUM.GT. (N+1)) GO TO 105

      IF (MOD(NUM,2).NE.0) RETURN

      N  = NUM - 2
      X3 = X(NUM)
      X2 = X(NUM-1)
      X1 = X(NUM-2)
      E  = X3*X3 - X2*X2
      F  = X3*X3*X3 - X2*X2*X2
      FEINTS = X3 - X2
      DEL(1) = FEINTS
      DEL(2) = X1 - X3
      DEL(3) = X2 - X1
      G(1)   = X2 + X3
      G(2)   = X1 + X3
      G(3)   = X1 + X2
      PI(1)  = X2*X3
      PI(2)  = X1*X3
      PI(3)  = X1*X2
      DELPRD = DEL(1)*DEL(2)*DEL(3)

  107 SUMT = 0.D0
      DO I = 1,3
          SUMT = SUMT + Y(N-1+I)*DEL(I)*
     +           (THIRD*F-G(I)*HALF*E+PI(I)*FEINTS)
      END DO

      ANS = ANS - SUMT/DELPRD
      RETURN
      END
