C NCLFORTSTART
      SUBROUTINE DZREGR1(N,M,M2,Y,YMSG,X,XMSG,C,RESID,CON,CNORM,WK,YY,
     +                   COV,XSD,XMEAN,A,AINV,S)
      IMPLICIT NONE
      INTEGER  N,M,M2
      DOUBLE PRECISION Y(N),X(N,M),C(M),CNORM(M),RESID(N),XMSG,YMSG,CON 
      DOUBLE PRECISION WK(M,M2)
      DOUBLE PRECISION YY(N),COV(M,M),XSD(M),XMEAN(M)
      DOUBLE PRECISION A(M,M),AINV(M,M),S(M)
C NCLEND

C NCL:    C = reg_multlin (y[*], x[*][*], opt)

C Nomenclature:
C .   C      - the raw partial correlation coef
C .   CNORM  - the standardized partial correlation coef
C
C The WK array used to be allocated inside the DZREGR2 subroutine,
C but this caused a problem for some compilers. The C wrapper for
C this function is now allocating this array and passing it in.
C Same issue for YY, COV, XSD, and XMEAN.
C
C ADJUSTABLE ARRAYS (LOCAL)  (the adj arrays were moved to the driver routine)
      INTEGER I,J,IERROR,NPTUSED,IER
      DOUBLE PRECISION VAR,YSD,YMEAN
      DATA   IERROR /1/

C IF ANY X HAS A MISSING VALUES THEN SET THE CORRESPONDING Y to YMSG
C .   THIS ELIMINATES ANY X/Y MISSING VALUES FROM THE COMPUTATIONS.

      DO I=1,N
         YY(I) = Y(I)
        DO J=1,M
           IF (X(I,J).EQ.XMSG) THEN
               YY(I) = YMSG
               GO TO 10
           END IF
        END DO
   10    CONTINUE
      END DO

C RESID ARE CALCULATED USING THE RAW PARTIAL REGRESSION COEF

      CALL DZREGR2(IERROR,N,M,M2,YY,YMSG,X,C,RESID,COV,WK,A,AINV,S)

C CALCULATE THE CONSTANT TERM
C .   CON = YMEAN - c(1)*XMEAN_1 - c(2)*XMEAN_2 - ... -c(J)*XMEAN_J
      
      CALL DSTAT2(YY,N,YMSG,YMEAN,VAR,YSD,NPTUSED,IER)

      CON  = YMEAN
      DO J = 1,M
          CALL DSTAT2(X(1,J),N,XMSG,XMEAN(J),VAR,XSD(J),NPTUSED,IER)
          CON       = CON - C(J)*XMEAN(J)
          CNORM(J)  = C(J)*XSD(J)/YSD  
      END DO

      RETURN
      END
C --------------------------------------------------------
      SUBROUTINE DZREGR2(IERROR,N,M,M2,T,TMSG,F,C,RESID,COV,WK,
     +                   A,AINV,S)
      IMPLICIT NONE
      INTEGER  IERROR,N,M,M2
      DOUBLE PRECISION T(N),F(N,M),C(M),RESID(N),COV(M,M),TMSG
      DOUBLE PRECISION WK(M,M2)
      DOUBLE PRECISION A(M,M),AINV(M,M),S(M)

C The modelled regession time series T(I) (I=1,2,...,N) is given by
C (see vector notation on the RIGHT):
C                           M
C                   T(I) = SUM C(J)*F(I,J)        <== T=FC         (1)
C                          J=1
C where C(J)=regression coefficient for J-th input surrogate time
C series F(I,J)
C
C Method: Least squares analysis applied to (1) yields the following
C linear matrix equation:
C                         AC = S                  <==> (F'F)C=F'T  (2)
C
C where C = model coefficient vector to be solved and S = data source
C vector.  It follows that
C                           N
C                   S(J) = SUM T(I)*F(I,J)        <== F'T          (3)
C                          I=1
C and                         N
C                   A(J,K) = SUM F(I,J)*F(I,K)    <== F'F          (4)
C                            I=1
C NOTE(s): The solution is (in vector notation):
C                               -1        -1
C                       C = (F'F)  F'T = A  S                      (5)
C
C The covariance matrix of the C coefficients is:
C                                     2    -1
C                          COV = sigma (F'F)                       (6)
C where
C                            2    1   N             2
C                       sigma  = --- SUM Residual(I)               (7)
C                                N-M I=1
C with Residual(I) being the original time series T(I) MINUS the time
C series T(I) modelled by (1) above.  Under the null hypothesis
C (all cross correlations between the coefficients are zero) that
C Var(T|F)=sigma**2 I, where I is the N X N unit matrix.
C
C Input Data:
C   IERROR = Error flag. If set to 1, standard error analysis is
C            performed for the coefficient vector C. Setting IERROR
C            to any other integer SKIPS this
C   N = Time series length
C   M = Number of surrogate series F applied in (1)
C   T(N) = measured time series being modelled

C   TMSG has replaced TMIN, TMAX for NCL reasons
C   TMIN,TMAX = Values of input series T that are less than TMIN
C          or greater than TMAX will NOT be included in the
C          regression (these are the flagging values)

C   F(N,M) = J-th (J=1,2,..,M) surrogate series, each of length N

C Output Data:
C   C(M) = solved coefficient vector
C   RESID(N) = Residual error time series (Original series T(I)
C              MINUS the modelled T(I) from (1) above)
C   COV(M,M) = Covariance matrix for coefficients C (SKIPPED if
C              IERROR is not equal to 1)
C EXAMPLE:
C      PARAMETER(IERROR=1,TMIN=-1.1,TMAX=1.1)
C      PARAMETER(N=20,M=7)
C      REAL T(N),F(N,M),WK(M,2*M),A(M,M),AINV(M,M),S(M),C(M),
C     & RESID(N),COV(M,M)
C
C      DO I=1,N
C        T(I)=COS(2.*I)
C        F(I,1)=1.0
C        F(I,2)=COS(2.05*I)
C        F(I,3)=SIN(0.4*I)
C        F(I,4)=COS(0.6*I)
C        F(I,5)=SIN(0.8*I)
C        F(I,6)=COS(0.3*I)
C        F(I,7)=SIN(0.9*I)
C      ENDDO
C      CALL ZREGR(IERROR,N,M,T,TMIN,TMAX,F,WK,A,AINV,S,C,RESID,COV)
C      WRITE(*,*)'I,T(I),T_model(I)'
C      DO I=1,N
C        WRITE(*,*) I,T(I),T(I)-RESID(I)
C      ENDDO
C
C      STOP
C      END
C
C Address questions/comments to:
C   Dr. Jerry Ziemke
C   NASA/Goddard Space Flight Center            PH: (301) 614-6034
C   Code 916                                   FAX: (301) 614-5903
C   Greenbelt, MD  20771
C   (Affil: UMBC GEST, Baltimore, MD)
C   e-mail: ziemke@jwocky.gsfc.nasa.gov
C
C Source is from:
C   http://code916.gsfc.nasa.gov/Data_services/cloud_slice/regress.html


C local ... some are adjustable arrays (had to move these to the driver routine)
      INTEGER NGOOD,I,J,K,L,IPRINT,ICHECK
      DOUBLE PRECISION U,SSUM,SIGSQR

      NGOOD = 0
      DO I = 1,N
C C C   IF ((T(I).GT.TMIN).AND.(T(I).LT.TMAX)) NGOOD=NGOOD+1
          IF (T(I).NE.TMSG) NGOOD = NGOOD + 1
      END DO

      DO J = 1,M
          S(J) = 0.D0
          DO I = 1,N
C C C     IF ((T(I).GT.TMIN).AND.(T(I).LT.TMAX)) THEN
              IF (T(I).NE.TMSG) THEN
C                                          ! <== S=F'T
                  S(J) = S(J) + T(I)*F(I,J)
              END IF
          END DO
      END DO

C     !Construct matrix A:
      DO K = 1,M
          DO J = 1,M
              A(J,K) = 0.D0
              DO I = 1,N
C C C       IF ((T(I).GT.TMIN).AND.(T(I).LT.TMAX)) THEN
                  IF (T(I).NE.TMSG) THEN
C                                         ! <== A=F'F
                      A(J,K) = A(J,K) + F(I,J)*F(I,K)
                  END IF
              END DO
          END DO
      END DO

C               ! ==> If 1, print out solution each CALL
      IPRINT = 0
C               ! ==> If 1, print out error analysis each CALL
      ICHECK = 0

C     !Solve for coef vector C:
      DO I = 1,M
          DO J = 1,M
              WK(I,J) = A(I,J)
          END DO
      END DO

      DO I = 1,M
          DO J = M + 1,2*M
              IF (M+I.EQ.J) THEN
                  WK(I,J) = 1.0D0
              ELSE
                  WK(I,J) = 0.0D0
              END IF
          END DO
      END DO

      DO K = 1,M
          DO I = 1,M
              IF (I.NE.K) THEN
                  IF (WK(K,K).EQ.0.0D0) THEN
                      L = 1
   30                 IF (WK(L,K).EQ.0.0D0) THEN
                          L = L + 1
                          GO TO 30
                      END IF
                      DO J = K,2*M
                          WK(K,J) = WK(K,J) + WK(L,J)
                      END DO
                  END IF
                  U = -WK(I,K)/WK(K,K)
                  DO J = K + 1,2*M
                      WK(I,J) = WK(I,J) + U*WK(K,J)
                  END DO
              END IF
          END DO
      END DO

      DO J = 1,M
          DO I = 1,M
              AINV(I,J) = WK(I,J+M)/WK(I,I)
          END DO
      END DO

      IF (IPRINT.EQ.1) THEN
          DO J = 1,M
              WRITE (*,FMT='(1X,A7,I3,A19)') 'Column ',J,
     +          ' of inverse matrix:'
              DO I = 1,M
                  WRITE (*,FMT=*) A(I,J)
              END DO
          END DO
      END IF

      IF (ICHECK.EQ.1) THEN
          WRITE (*,FMT=*)
     +      '-----------------------------------------------'
          WRITE (*,FMT=*) 'Simple error check by multiplying A by AINV:'
          WRITE (*,FMT=*)
     +      '(this should ideally yield the identity matrix)'
          DO J = 1,M
              WRITE (*,FMT='(1X,A7,I3,A27)') 'Column ',J,
     +          ' of product matrix A*AINV:'
              DO I = 1,M
                  SSUM = 0.0D0
                  DO K = 1,M
                      SSUM = SSUM + A(I,K)*AINV(K,J)
                  END DO
                  WRITE (*,FMT=*) SSUM
              END DO
          END DO
      END IF

C     !Coefficient vector solution:
      DO I = 1,M
          C(I) = 0.D0
          DO J = 1,M
              C(I) = C(I) + AINV(I,J)*S(J)
          END DO
      END DO

      IF (IERROR.EQ.1) THEN
          DO I = 1,N
C C C     IF ((T(I).GT.TMIN).AND.(T(I).LT.TMAX)) THEN
              IF (T(I).NE.TMSG) THEN
                  SSUM = 0.D0
                  DO J = 1,M
                      SSUM = SSUM + C(J)*F(I,J)
                  END DO
                  RESID(I) = T(I) - SSUM
              ELSE
                  RESID(I) = TMSG
              END IF
          END DO

          SSUM = 0.D0
          DO I = 1,N
C C C     IF ((T(I).GT.TMIN).AND.(T(I).LT.TMAX)) SSUM=SSUM+RESID(I)**2
              IF (T(I).NE.TMSG) SSUM = SSUM + RESID(I)**2
          END DO

          SIGSQR = SSUM/ (NGOOD-M)

          DO K = 1,M
              DO J = 1,M
                  COV(J,K) = SIGSQR*AINV(J,K)
              END DO
          END DO
      END IF

      RETURN
      END
