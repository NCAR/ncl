c
c interfaces for various Cumalative Distribution Functions
c based upon: www/stat.unipg.it/stat/dcdflib
c

c ================================================================
c NCLFORTSTART
      SUBROUTINE DCDFBINP(N,S,XN,PR,P,IER)
c NCL: p = cdfbin_p(s,xn,pr)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION S(N),XN(N),PR(N)
c output
      INTEGER IER
      DOUBLE PRECISION P(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,OMPR,BOUND

C iwhich = 1 : Calculate P and Q from S, XN, PR
c              Q, OMPR not returned

      WHICH = 1

      DO I=1,N
         Q    = 1.D0 - P(I)
         OMPR = 1.D0 - PR(I)
         CALL CDFBIN(WHICH,P(I),Q,S(I),XN(I),PR(I),OMPR,IER,BOUND)
         IF (IER.NE.0) CALL CDFBERR('CDFBIN_P',IER)
      END DO

      RETURN
      END
c ------------
c NCLFORTSTART
      SUBROUTINE DCDFBINS(N,P,XN,PR,S,IER)
c NCL: s = cdfbin_s(p,xn,pr)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),XN(N),PR(N)
c output
      INTEGER IER
      DOUBLE PRECISION S(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,OMPR,BOUND

C iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR

      WHICH = 2
      DO I=1,N
         Q    = 1.0D0 - P(I)
         OMPR = 1.0D0 - PR(I)
         CALL CDFBIN(WHICH,P(I),Q,S(I),XN(I),PR(I),OMPR,IER,BOUND)
         IF (IER.NE.0) CALL CDFBERR('CDFBIN_X',IER)
      END DO

      RETURN
      END
c ------------
c NCLFORTSTART
      SUBROUTINE DCDFBINXN(N,P,S,PR,XN,IER)
c NCL: n = cdfbin_xn(p,s,pr)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),S(N),PR(N)
c output
      INTEGER IER
      DOUBLE PRECISION XN(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,OMPR,BOUND

C iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR

      WHICH = 3
      DO I=1,N
         OMPR = 1.0D0 - PR(I)
         Q    = 1.0D0 - P(I)
         CALL CDFBIN(WHICH,P(I),Q,S(I),XN(I),PR(I),OMPR,IER,BOUND)
         IF (IER.NE.0) CALL CDFBERR('CDFBIN_XN',IER)
      END DO

      RETURN
      END
c ------------
c NCLFORTSTART
      SUBROUTINE DCDFBINPR(N,P,S,XN,PR,IER)
c NCL: pr = cdfbin_pr(p,s,xn)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),S(N),XN(N)
c output
      INTEGER IER
      DOUBLE PRECISION PR(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,OMPR,BOUND

C iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN
c              OMPR not returned
      WHICH = 4
      DO I=1,N
         Q    = 1.0D0 - P(I)
         OMPR = 1.0D0 - PR(I)
         CALL CDFBIN(WHICH,P(I),Q,S(I),XN(I),PR(I),OMPR,IER,BOUND)
         IF (IER.NE.0) CALL CDFBERR('CDFBIN_PR',IER)
      END DO

      RETURN
      END

c ------------
      SUBROUTINE CDFBERR(NAME,IER)
c error messages for cdfbin_{p,x,n,pr} functions
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER

      IF (IER.NE.0) THEN
          IF (IER.EQ.1) PRINT *,NAME,': lower bound error'
          IF (IER.EQ.2) PRINT *,NAME,': upper bound error'
          IF (IER.EQ.3) PRINT *,NAME,': P+Q .ne. 1'
          IF (IER.EQ.4) PRINT *,NAME,': PR + OMPR .ne. 1'
      END IF
      RETURN
      END

c ================================================================
c NCLFORTSTART
      SUBROUTINE DCDFGAMP(N,X,SHAPE,SCALE,P,IER)
c NCL: p = cdfgam_p(x,shape,scale)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION X(N),SHAPE(N),SCALE(N)
c output
      INTEGER IER
      DOUBLE PRECISION P(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 1 : Calculate P and Q from X,SHAPE and SCALE
c              Q not returned
      WHICH = 1
      BOUND = -999.D0
      DO I=1,N
         CALL CDFGAM(WHICH,P(I),Q,X(I),SHAPE(I),SCALE(I),IER,BOUND)
         IF (IER.NE.0) CALL CDFGERR('CDFGAM_P',IER)
      END DO

      RETURN
      END

c ------------
c NCLFORTSTART
      SUBROUTINE DCDFGAMX(N,P,SHAPE,SCALE,X,IER)
c NCL: x = cdfgam_x(p,shape,scale)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),SHAPE(N),SCALE(N)
c output
      INTEGER IER
      DOUBLE PRECISION X(N)
C NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 2 : Calculate X from P,Q,SHAPE and SCALE

      WHICH = 2
      BOUND = -999.D0

      DO I=1,N
         Q = 1.0D0 - P(I)
         CALL CDFGAM(WHICH,P(I),Q,X(I),SHAPE(I),SCALE(I),IER,BOUND)
         IF (IER.NE.0) CALL CDFGERR('CDFGAM_X',IER)
      END DO

      RETURN
      END

c ------------
      SUBROUTINE CDFGERR(NAME,IER)
c error messages for cdfgam_{p,x} functions
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER

      IF (IER.NE.0) THEN
          IF (IER.EQ.1) PRINT *,NAME,': lower bound error'
          IF (IER.EQ.2) PRINT *,NAME,': upper bound error'
          IF (IER.EQ.3) PRINT *,NAME,': P+Q .ne. 1'
          IF (IER.EQ.10)PRINT *,NAME,': gamma/inverse gamma err'
          IF (IER.LT.0) PRINT *,NAME,': illegal input'
      END IF

      RETURN
      END

c ================================================================
c NCLFORTSTART
      SUBROUTINE DCDFNORP(N,X,MEAN,SD,P,IER)
c NCL: p = cdfnor_p(x,mean,sd)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION X(N),MEAN(N),SD(N)
c output
      INTEGER IER
      DOUBLE PRECISION P(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 1 : Calculate P and Q from X,MEAN and SD
c              Q not returned

      WHICH = 1
      DO I=1,N
         CALL CDFNOR(WHICH,P(I),Q,X(I),MEAN(I),SD(I),IER,BOUND)
         IF (IER.NE.0) CALL CDFNERR('CDFNOR_P',IER)
      END DO

      RETURN
      END

c ------------
c NCLFORTSTART
      SUBROUTINE DCDFNORX(N,P,MEAN,SD,X,IER)
c NCL: x = cdfnor_x(p,mean,sd)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION P(N),MEAN(N),SD(N)
c output
      INTEGER IER
      DOUBLE PRECISION X(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 2 : Calculate X from P,Q,MEAN and SD
      WHICH = 2
      DO I=1,N
         Q = 1.0D0 - P(I)
         CALL CDFNOR(WHICH,P(I),Q,X(I),MEAN(I),SD(I),IER,BOUND)
         IF (IER.NE.0) CALL CDFNERR('CDFNOR_X',IER)
      END DO

      RETURN
      END
c ------------
      SUBROUTINE CDFNERR(NAME,IER)
c error messages for cdfnor_{p,x} functions
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER

      IF (IER.NE.0) THEN
          IF (IER.EQ.1) PRINT *,NAME,': lower bound error'
          IF (IER.EQ.2) PRINT *,NAME,': upper bound error'
          IF (IER.EQ.3) PRINT *,NAME,': P+Q .ne. 1'
      END IF

      RETURN
      END
c ------------
c NCLFORTSTART
      SUBROUTINE DCDFCHIP(N,X,DF,P,IER)
c NCL: p = cdfchi_p(x,df)
      IMPLICIT NONE
c input
      INTEGER N
      DOUBLE PRECISION X(N),DF(N)
c output
      INTEGER IER
      DOUBLE PRECISION P(N)
c NCLEND
c local
      INTEGER WHICH, I
      DOUBLE PRECISION Q,BOUND

C iwhich = 1 : Calculate P and Q from X and DF
c              Q not returned
      WHICH = 1
      BOUND = -999.D0
      DO I=1,N
         CALL CDFCHI(WHICH,P(I),Q,X(I),DF(I),IER,BOUND)
         IF (IER.NE.0) CALL CDFGERR('CDFCHI_P',IER)
      END DO

      RETURN
      END
