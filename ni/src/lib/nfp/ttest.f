C NCLFORTSTART
      SUBROUTINE DTTEST(AVE1,VAR1,S1,AVE2,VAR2,S2,IFLAG,ALPHA,TVAL,IER)
c      implicit none

C NCL: prob = ttest (ave1,var1,s1,ave2,var2,s2,iflag,tval_opt)

c calculate student t-statistic + prob

c nomenclature:

c input  ave1,var1,s1 - average, variance and size (sample 1)
c .      ave2,var2,s2 - average, variance and size (sample 2)
c .                     Ideally, s1 and s2 >=30
c output alpha        - level of significance
c .                     probability that a random variable
c .                     following the students t distribution
c .                     will exceed tval in absolute value.
c .      tval         - t-value

c .                     e.g.,if df=14. and tval=1.761 then alpha=0.10.
c .                          for a 2 tailed test alpha=0.10 (or 10%)
c .                          means that the critical region has 5%
c .                          in each tail. note: some users set
c .                                  prob = 1.-alpha
c .                          upon return to the calling program.
c .      iflag        - set to False if population variances are equal
c .                     set to True if population variances are unequal
c .      ier          - error code

      DOUBLE PRECISION AVE1,VAR1,S1,AVE2,VAR2,S2,TVAL,ALPHA
      INTEGER IER
C NCLEND
      INTEGER IFLAG
      DOUBLE PRECISION DF,VARP
      DOUBLE PRECISION DBETAISLATEC

      IER = 0
C
C These following tests are done in the NCL C wrapper.
C
C      IF (S1.LT.2.0D0 .OR. S2.LT.2.0D0) IER = 1
C      IF (VAR1.LE.0.0D0 .OR. VAR2.LE.0.0D0) IER = IER + 10
C      IF (IER.NE.0) RETURN

      IF (IFLAG.EQ.0) THEN
          DF = S1 + S2 - 2.D0
          VARP = ((S1-1.D0)*VAR1+ (S2-1.D0)*VAR2)/DF
          TVAL = (AVE1-AVE2)/SQRT(VARP* (1.D0/S1+1.D0/S2))
      ELSE
          DF = (VAR1/S1+VAR2/S2)**2/ ((VAR1/S1)**2/ (S1-1.D0)+
     +         (VAR2/S2)**2/ (S2-1.D0))
          TVAL = (AVE1-AVE2)/SQRT(VAR1/S1+VAR2/S2)
C
C This following test is done in the NCL C wrapper.
C      ELSE
C          IER = -999
      END IF

      ALPHA = DBETAISLATEC(DF/ (DF+TVAL**2),0.5D0*DF,0.5D0)

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DFTEST(VAR1,VAR2,S1,S2,ALPHA,IER)
c c c implicit none

C NCL: prob = ftest (var1,var2,s1,s2)

c calculate student t-statistic + prob

c nomenclature:

c input  var1,s1      - variance and size (sample 1)
c .      var2,s2      - variance and size (sample 2)
c output alpha        - level of significance;
c .                     probability that a random variable
c .                     following the students t distribution
c .                     will exceed tval in absolute value.
c .      ier          - error code
c .                     ier.ne.0 then alpha=errval

      DOUBLE PRECISION VAR1,S1,VAR2,S2,ALPHA
      INTEGER IER
C NCLEND
      DOUBLE PRECISION DF1,DF2,FRAT
      DOUBLE PRECISION DBETAISLATEC

      IER = 0
      IF (S1.LT.2.0D0 .OR. S2.LT.2.0D0) IER = 1
      IF (VAR1.LE.0.0D0 .OR. VAR2.LE.0.0D0) IER = IER + 10
      IF (IER.NE.0) RETURN

      IF (VAR1.GT.VAR2) THEN
          FRAT = VAR1/VAR2
          DF1 = S1 - 1.D0
          DF2 = S2 - 1.D0
      ELSE
          FRAT = VAR2/VAR1
          DF1 = S2 - 1.D0
          DF2 = S1 - 1.D0
      END IF

      ALPHA = DBETAISLATEC(DF2/ (DF2+DF1*FRAT),0.5D0*DF2,0.5D0*DF1) +
     +        (1.D0-DBETAISLATEC(DF1/ (DF1+DF2/FRAT),0.5D0*DF1,
     +        0.5D0*DF2))

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DRTEST(R,N,PROB)
      IMPLICIT NONE
C Input
      INTEGER N
      DOUBLE PRECISION R
C Output
      DOUBLE PRECISION PROB
C NCLEND

C NCL: prob = rtest (r, n)

c nomenclature:
c Input
c .   r     - linear correlation coef
c .   n     - number of points used to calculate "r"
c Output
c .   prob  - calculated significance [0. < prob < 1.]

c Local variables

c c c real     betai
      DOUBLE PRECISION DBETAISLATEC
      DOUBLE PRECISION DF,TVAL

c calculate the significance level
c .   [see Numerical Recipies: Linear Correlation]
c .   [also: Climatic Change, WMO Tech Note 79, p66
c .          says "tval" formula is good for xn>=8]

      DF = N - 2.D0
      TVAL = R*SQRT(DF/ (1.D0-DMIN1(R*R,0.999D0)))
c c c prob  = betai ( 0.5*df, 0.5, df/(df+tval**2) )
      PROB = DBETAISLATEC(DF/ (DF+TVAL**2),0.5D0*DF,0.5D0)

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DEQVSIZ(X,NPTS,XMSG,SIGLVL,NEQV)
      IMPLICIT NONE
C Input
      INTEGER NPTS
      DOUBLE PRECISION X(NPTS),XMSG,SIGLVL
C Output
      INTEGER NEQV
C NCLEND

C NCL: neqv = eqv_sample_size (x,siglvl)
c      Only eqv size returned in NCL

C This assumes a red-noise process [r1>0.]

c nomenclature:
c Input
c .   x      - series
c .   npts   - number of pts
c .   xmsg   - missing code
c .   siglvl - significance level (eg: 0.05) at which the lag-1
c .            autocorrelation is to be tested.
c Output
c .   neqv   - Number of independent samples

c Local variables

c c c real     betai
      DOUBLE PRECISION DBETAISLATEC
      DOUBLE PRECISION XMEAN,XVAR,XSD,XCOV,XN,DF,SUM,TVAL,ALPHA,R1
      INTEGER N,LAG,JER,NPTUSED

      NEQV = -999

c compute 1st two moments

      CALL DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSED,JER)
      IF (NPTUSED.LE.0) THEN
          RETURN
      ELSEIF (NPTUSED.EQ.1 .OR. XVAR.EQ.0.D0) THEN
          NEQV = 1
          RETURN
      END IF

c compute lag-1 autocorrelation coef

      LAG  = 1
      XCOV = 0.D0
      XN   = 0.D0
      DO N = 2,NPTS
          IF (X(N).NE.XMSG .AND. X(N-LAG).NE.XMSG) THEN
              XCOV = XCOV + ((X(N)-XMEAN)* (X(N-LAG)-XMEAN))
              XN = XN + 1.D0
          END IF
      END DO

      IF (XN.GE.2.D0 .AND. XVAR.GT.0.D0) THEN
          R1 = (XCOV/ (XN-1.D0))/XVAR
      ELSE
          R1   = XMSG
          NEQV = -999
          RETURN
      END IF

c the approach below does not work when R1<0.

      IF (R1.LE.0.0D0) THEN
          NEQV = NPTUSED
          RETURN
      END IF

c calculate the significance level
c .   [see Numerical Recipies: Linear Correlation]
c .   [also: Climatic Change, WMO Tech Note 79, p66
c .          says "tval" formula is good for xn>=8]

      DF = NPTUSED - 2.D0
      TVAL = R1*SQRT(DF/ (1.D0-DMIN1(R1*R1,0.999D0)))
c c c alpha = betai ( 0.5*df, 0.5, df/(df+tval**2) )
      ALPHA = DBETAISLATEC(DF/ (DF+TVAL**2),0.5D0*DF,0.5D0)

c test r1 to see if it is sig different from zero at user
c specified level.

      NEQV = NPTUSED
      IF (ALPHA.LE.SIGLVL) THEN
          IF (XN.GE.50.D0 .AND. ABS(R1).NE.1.D0) THEN
              NEQV = XN* (1.D0-R1)/ (1.D0+R1)
          ELSE
              SUM = 1.0D0
              DO N = 1,NPTS
                  IF (X(N).NE.XMSG) THEN
                      SUM = SUM + 2.D0* (1.D0-N/XN)*R1**N
                  END IF
              END DO
              NEQV = XN/SUM
          END IF
          NEQV = MIN(NEQV,NPTUSED)
      END IF

      RETURN
      END
