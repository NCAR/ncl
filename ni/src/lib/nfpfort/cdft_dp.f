c ------------
c NCLFORTSTART
      SUBROUTINE DCDFCDFTT(N, P, T, DF, IER )
c NCL: p = cdft_t(p,df)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),DF(N)
c output
      INTEGER IER
      DOUBLE PRECISION T(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 2 : Calculate T from P and DF
c              Q not returned
      WHICH = 2
      BOUND = -999.D0
      DO I=1,N
         Q = 1.0D0 - P(I)
         CALL CDFT(WHICH,P(I),Q,T(I),DF(I),IER,BOUND)
         IF (IER.NE.0) CALL DCDFTERR('CDFT_T',IER)
      END DO

      RETURN
      END
c ------------
c NCLFORTSTART
      SUBROUTINE DCDFCDFTP(N, P, T, DF, IER )
c NCL: p = cdft_p(t,df)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION T(N),DF(N)
c output
      INTEGER IER
      DOUBLE PRECISION P(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 1 : Calculate P from T and DF
c              Q not returned
      WHICH = 1
      BOUND = -999.D0
      DO I=1,N
         Q = 1.0D0 - P(I)
         CALL CDFT(WHICH,P(I),Q,T(I),DF(I),IER,BOUND)
         IF (IER.NE.0) CALL DCDFTERR('CDFT_T',IER)
      END DO

      RETURN
      END
c ------------
      SUBROUTINE DCDFTERR(NAME,IER)
c error messages for cdft_{t, p} functions
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER

      IF (IER.NE.0) THEN
          IF (IER.LT.0) PRINT *,NAME,': bad input parameter, IER=', IER
          IF (IER.EQ.1) PRINT *,NAME,': lower bound error'
          IF (IER.EQ.2) PRINT *,NAME,': upper bound error'
          IF (IER.EQ.3) PRINT *,NAME,': P+Q .ne. 1'
      END IF
      RETURN
      END
