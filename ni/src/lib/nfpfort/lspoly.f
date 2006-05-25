C NCLFORTSTART
      SUBROUTINE DLSPOLY(N,M,X,Y,WGT,COEF,IER)
      INTEGER N,M,IER
      DOUBLE PRECISION X(M),Y(M),WGT(M),COEF(N)
C NCLEND

C NCL:   coef = lspoly(x,y,wgt,n)

C AUTOMATIC ARRAYS AND LOCAL VARIABLES
      INTEGER IC(N)
      DOUBLE PRECISION WRK(N,N),WRK2(M)
      INTEGER I
C
C TEST INPUT PARAMETERS
C .   IER=1:  M LESS THAN N IN LSPOLY
C .   IER=2:  DEGREE OF POLYNOMIAL GREATER THAN 4 IN LSPOLY **WARNING**
C .   IER=3:  MATRIX SINGULAR IN LSPOLY
C
      IER = 0
      IF (M.LE.N) IER = 1
      IF (N.GT.5) IER = 2
      IF (IER.NE.0) THEN
          DO I = 1,N
              COEF(I) = 1D20
          END DO
          RETURN
      END IF
C
      CALL DLSPLY2(M,N,X,Y,COEF,WGT,WRK,WRK2,IC,IER)
C
      RETURN
      END
C----------------------------------------------------------------------
C ++++ ORIGINAL LSPOLY DOCUMENTATION ++++
C     SUBROUTINE DLSPOLY (N,M,X,Y,W,C,WRK,IER)
C DIMENSION OF           X(M),Y(M),W(M),C(N),WRK(N**2+N+M)
C ARGUMENTS
C
C LATEST REVISION        FEBRUARY 1985
C
C PURPOSE                GIVEN A SET OF DATA (X(I),Y(I)), I = 1,...,M,
C                        LSPOLY CALCULATES A SET OF COEFFICIENTS FOR A
C                        WEIGHTED LEAST SQUARES POLYNOMIAL FIT TO THE
C                        GIVEN DATA.  IT IS NECESSARY THAT M (THE NUMBER
C                        OF DATA POINTS) BE GREATER THAN OR EQUAL TO N
C                        (THE NUMBER OF COEFFICIENTS).
C
C USAGE                  CALL DLSPOLY (N,M,X,Y,W,C,WRK,IER)
C
C ARGUMENTS
C
C ON INPUT               N
C                          AN INTEGER VARIABLE SET EQUAL TO THE NUMBER
C                          OF COEFFICIENTS DESIRED (I.E., N-1 WILL BE
C                          THE DEGREE OF THE POLYNOMIAL).
C
C                        M
C                          AN INTEGER VARIABLE SET EQUAL TO THE NUMBER
C                          OF DATA POINTS.  M MUST BE GREATER THAN OR
C                          EQUAL TO N.
C
C                        X
C                          A REAL ARRAY WITH DIMENSION AT LEAST M.  ON
C                          INPUT, X CONTAINS THE ABSCISSA VALUES OF THE
C                          DATA POINTS.
C
C                        Y
C                          A REAL ARRAY WITH DIMENSION AT LEAST M.  ON
C                          INPUT, Y CONTAINS THE ORDINATE VALUES OF THE
C                          DATA POINTS.
C
C                        W
C                          A REAL ARRAY WITH DIMENSION AT LEAST M.  ON
C                          INPUT, W IS A VECTOR OF WEIGHTS FOR A
C                          WEIGHTED LEAST SQUARES MODEL.  IF NO
C                          WEIGHTING IS DESIRED, THE ELEMENTS OF W
C                          SHOULD BE SET TO 1 .
C
C                        WRK
C                          A REAL ARRAY WITH DIMENSION AT LEAST N*N+N+M.
C                          WRK IS USED INTERNALLY FOR WORKING STORAGE.
C
C ON OUTPUT              C
C                          A REAL ARRAY WITH DIMENSION AT LEAST N.  ON
C                          OUTPUT, C CONTAINS THE COEFFICIENTS FOR THE
C                          POLYNOMIAL.  C(1) WILL CONTAIN THE CONSTANT
C                          TERM.
C
C                        IER
C                          AN INTEGER ERROR FLAG
C                          = 1  IF M .LT. N.
C                          = 2  IF N .GT. 5 (NON-FATAL).
C                          = 3  IF THE LINEAR SYSTEM IS SINGULAR.
C                          = 0  OTHERWISE.
C
C I/O                    ALL MESSAGES ARE PRINTED USING ROUTINE ULIBER.
C
C                        FOR IER = 1, THE FOLLOWING MESSAGE IS PRINTED.
C                            M LESS THAN N IN LSPOLY
C
C                        FOR IER = 2, THE FOLLOWING MESSAGE IS PRINTED.
C                            **WARNING** DEGREE OF POLYNOMIAL GREATER
C                                        THAN 4 IN LSPOLY
C
C                        FOR IER = 3, THE FOLLOWING MESSAGE IS PRINTED.
C                            *MATRIX SINGULAR IN LSPOLY*
C
C HISTORY                LSPOLY WAS WRITTEN IN THE EARLY 1970'S
C                        BY MEMBERS OF NCAR'S SCIENTIFIC COMPUTING
C                        DIVISION.  LSPOLY REPLACES FORMER ULIB
C                        ROUTINE LSTSQR.
C
C PORTABILITY            FORTRAN 66.
C
C ALGORITHM              LSPOLY FORMS THE NORMAL EQUATIONS AND SOLVES
C                        THE RESULTING SQUARE LINEAR SYSTEM USING
C                        GAUSSIAN ELIMINATION WITH FULL PIVOTING.
C
C ACCURACY               FOR LOWER ORDER POLYNOMIALS (N .LE. 5), LSPOLY
C                        CAN BE EXPECTED TO GIVE SATISFACTORY RESULTS.
C                        AS THE DEGREE OF THE FITTING POLYNOMIAL IS
C                        INCREASED, THE PROBLEM BECOMES NUMERICALLY
C                        UNSTABLE.  IF A HIGHER DEGREE POLYNOMIAL FIT
C                        IS DESIRED, THEN ROUTINE RGRSN1 SHOULD
C                        BE USED.
C
C **********************************************************************
c c c SUBROUTINE DLSPOLY (N,M,X,Y,W,C,WRK,IER)
c c c DIMENSION       X(M)       ,Y(M)       ,W(M)       ,C(N)       ,
c c c1                WRK(1)
c c c SAVE
c c c I1 = N*N+1
c c c I2 = N*N+M+1
c c c CALL DLSPLY2 (M,N,X,Y,C,W,WRK,WRK(I1),WRK(I2),IER)
c c c RETURN
c c   END
C **************************************************************
C the following was modified to provide explicit array sizes
C **************************************************************
      SUBROUTINE DLSPLY2(M,N,X,Y,C,W,WRK,WRK2,IC,IER)
C
      IMPLICIT NONE
      INTEGER M,N,IC(N),IER
      DOUBLE PRECISION X(M),Y(M),W(M),WRK(N,N),WRK2(M)
      DOUBLE PRECISION C(N)
C
      DOUBLE PRECISION C1,HOLD,A,AMXA,AMXFS
      INTEGER I,J,NM1,NP1,NMI,K,KP1,ICT,ICNMI,IRT

C
C FORM NORMAL EQUATIONS
C
      DO I = 1,M
          WRK2(I) = W(I)
      END DO

      DO J = 1,N
          WRK(1,J) = 0.D0
          C(J) = 0.D0
          DO I = 1,M
              WRK(1,J) = WRK(1,J) + WRK2(I)
              C(J) = C(J) + WRK2(I)*Y(I)
              WRK2(I) = WRK2(I)*X(I)
          END DO
      END DO

      IF (N.NE.1) THEN
          DO J = 2,N
              WRK(J,N) = 0.D0
              DO I = 1,M
                  WRK(J,N) = WRK(J,N) + WRK2(I)
                  WRK2(I) = WRK2(I)*X(I)
              END DO
          END DO
          DO I = 2,N
              DO J = 2,N
                  WRK(I,J-1) = WRK(I-1,J)
              END DO
          END DO
      END IF
      NM1 = N - 1

      IF (N.EQ.1) GO TO 240
C
C DECOMPOSE MATRIX USING GAUSSIAN ELIMINATION WITH FULL PIVOTING
C
      NP1 = N + 1
      DO K = 1,NM1
          KP1 = K + 1
          AMXA = 0.D0
          DO J = K,N
              DO I = K,N
                  A = ABS(WRK(I,J))
                  IF ((AMXA-A).LT.0.D0) THEN
                      AMXA = A
                      IRT = I
                      ICT = J
                  END IF
              END DO
          END DO
          IC(K) = ICT
          IF ((IRT-K).NE.0) THEN
              DO J = 1,N
                  HOLD = WRK(IRT,J)
                  WRK(IRT,J) = WRK(K,J)
                  WRK(K,J) = HOLD
              END DO
              HOLD = C(IRT)
              C(IRT) = C(K)
              C(K) = HOLD
          END IF
          IF ((ICT-K).NE.0) THEN
              DO I = 1,N
                  HOLD = WRK(I,ICT)
                  WRK(I,ICT) = WRK(I,K)
                  WRK(I,K) = HOLD
              END DO
          END IF
          IF (K.EQ.1) AMXFS = ABS(AMXA)
          IF ((ABS(AMXA/DBLE(K))+AMXFS).EQ.AMXFS) GO TO 290
          C1 = 1.D0/AMXA
          DO J = KP1,N
              WRK(K,J) = WRK(K,J)*C1
          END DO
          C(K) = C(K)*C1
          DO I = KP1,N
              DO J = KP1,N
                  WRK(I,J) = WRK(I,J) - WRK(I,K)*WRK(K,J)
              END DO
              C(I) = C(I) - WRK(I,K)*C(K)
          END DO
      END DO
C
C SOLVE FOR COEFFICIENTS USING BACK SUBSTITUTION
C
      IF ((ABS(WRK(N,N)/DBLE(N))+AMXFS).EQ.AMXFS) GO TO 290
  240 C(N) = C(N)/WRK(N,N)
      IF (N.EQ.1) RETURN
      DO I = 1,NM1
          NMI = N - I
          DO K = NMI,NM1
              C(NMI) = C(NMI) - WRK(NMI,K+1)*C(K+1)
          END DO
      END DO
C
C PERMUTE SOLUTION VECTOR
C
      DO I = 1,NM1
          NMI = N - I
          ICNMI = IC(NMI)
          IF (ICNMI-NMI.NE.0) THEN
              HOLD = C(ICNMI)
              C(ICNMI) = C(NMI)
              C(NMI) = HOLD
          END IF
      END DO
      RETURN

  290 IER = 3
c c c CALL ULIBER (33,28H *MATRIX SINGULAR IN LSPOLY*,28)
      RETURN
      END
