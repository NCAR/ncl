C NCLFORTSTART
      subroutine chisub (p,df,chi)
      real p,df             ! input
      real chi              ! output
C NCLEND
C     SharedObject:   CHIIN::chisub(p,df,chi)  declare "chi" before call
C**********************************************************************
C      THIS IS AN INTERFACE THAT EMULATES IMSL's "CHIIN" FUNCTION
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    REAL
C
C     DF <--> Degrees of freedom of the
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    REAL
C
C
C     CHI<--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                   REAL  
C 
C********************END INPUT ARGS***************************
C
C                   INTERNAL STUFF
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C               10 indicates error returned from cumgam.  See
C                  references in cdfgam
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C

c local [temporary]

      integer which, status
      double precision q,x,bound
 
      which = 2                        ! to mimic IMSL "chiin"
      q     = 1.0d+0 - dble(p)
      call cdfchi(which,dble(p),q,x,dble(df),status,bound)
   
      chi = sngl(x)

      return
      end
      DOUBLE PRECISION FUNCTION algdiv(a,b)
C-----------------------------------------------------------------------
C
C     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B .GE. 8
C
C                         --------
C
C     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY
C     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X).
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION c,c0,c1,c2,c3,c4,c5,d,h,s11,s3,s5,s7,s9,t,u,v,w,
     +                 x,x2
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alnrel
      EXTERNAL alnrel
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog
C     ..
C     .. Data statements ..
      DATA c0/.833333333333333D-01/,c1/-.277777777760991D-02/,
     +     c2/.793650666825390D-03/,c3/-.595202931351870D-03/,
     +     c4/.837308034031215D-03/,c5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C------------------------
      IF (a.LE.b) GO TO 10
      h = b/a
      c = 1.0D0/ (1.0D0+h)
      x = h/ (1.0D0+h)
      d = a + (b-0.5D0)
      GO TO 20

   10 h = a/b
      c = h/ (1.0D0+h)
      x = 1.0D0/ (1.0D0+h)
      d = b + (a-0.5D0)
C
C                SET SN = (1 - X**N)/(1 - X)
C
   20 x2 = x*x
      s3 = 1.0D0 + (x+x2)
      s5 = 1.0D0 + (x+x2*s3)
      s7 = 1.0D0 + (x+x2*s5)
      s9 = 1.0D0 + (x+x2*s7)
      s11 = 1.0D0 + (x+x2*s9)
C
C                SET W = DEL(B) - DEL(A + B)
C
      t = (1.0D0/b)**2
      w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t + c0
      w = w* (c/b)
C
C                    COMBINE THE RESULTS
C
      u = d*alnrel(a/b)
      v = a* (dlog(b)-1.0D0)
      IF (u.LE.v) GO TO 30
      algdiv = (w-v) - u
      RETURN

   30 algdiv = (w-u) - v
      RETURN

      END
      DOUBLE PRECISION FUNCTION alngam(x)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION ALNGAM(X)
C                 double precision LN of the GAMma function
C
C
C                              Function
C
C
C     Returns the natural logarithm of GAMMA(X).
C
C
C                              Arguments
C
C
C     X --> value at which scaled log gamma is to be returned
C                    X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     If X .le. 6.0, then use recursion to get X below 3
C     then apply rational approximation number 5236 of
C     Hart et al, Computer Approximations, John Wiley and
C     Sons, NY, 1968.
C
C     If X .gt. 6.0, then use recursion to get X to at least 12 and
C     then use formula 5423 of the same source.
C
C**********************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION hln2pi
      PARAMETER (hln2pi=0.91893853320467274178D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION offset,prod,xx
      INTEGER i,n
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION coef(5),scoefd(4),scoefn(9)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION devlpl
      EXTERNAL devlpl
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC log,dble,int
C     ..
C     .. Data statements ..
      DATA scoefn(1)/0.62003838007127258804D2/,
     +     scoefn(2)/0.36036772530024836321D2/,
     +     scoefn(3)/0.20782472531792126786D2/,
     +     scoefn(4)/0.6338067999387272343D1/,
     +     scoefn(5)/0.215994312846059073D1/,
     +     scoefn(6)/0.3980671310203570498D0/,
     +     scoefn(7)/0.1093115956710439502D0/,
     +     scoefn(8)/0.92381945590275995D-2/,
     +     scoefn(9)/0.29737866448101651D-2/
      DATA scoefd(1)/0.62003838007126989331D2/,
     +     scoefd(2)/0.9822521104713994894D1/,
     +     scoefd(3)/-0.8906016659497461257D1/,
     +     scoefd(4)/0.1000000000000000000D1/
      DATA coef(1)/0.83333333333333023564D-1/,
     +     coef(2)/-0.27777777768818808D-2/,
     +     coef(3)/0.79365006754279D-3/,coef(4)/-0.594997310889D-3/,
     +     coef(5)/0.8065880899D-3/
C     ..
C     .. Executable Statements ..
      IF (.NOT. (x.LE.6.0D0)) GO TO 70
      prod = 1.0D0
      xx = x
      IF (.NOT. (x.GT.3.0D0)) GO TO 30
   10 IF (.NOT. (xx.GT.3.0D0)) GO TO 20
      xx = xx - 1.0D0
      prod = prod*xx
      GO TO 10

   20 CONTINUE
   30 IF (.NOT. (x.LT.2.0D0)) GO TO 60
   40 IF (.NOT. (xx.LT.2.0D0)) GO TO 50
      prod = prod/xx
      xx = xx + 1.0D0
      GO TO 40

   50 CONTINUE
   60 alngam = devlpl(scoefn,9,xx-2.0D0)/devlpl(scoefd,4,xx-2.0D0)
C
C
C     COMPUTE RATIONAL APPROXIMATION TO GAMMA(X)
C
C
      alngam = alngam*prod
      alngam = log(alngam)
      GO TO 110

   70 offset = hln2pi
C
C
C     IF NECESSARY MAKE X AT LEAST 12 AND CARRY CORRECTION IN OFFSET
C
C
      n = int(12.0D0-x)
      IF (.NOT. (n.GT.0)) GO TO 90
      prod = 1.0D0
      DO 80,i = 1,n
          prod = prod* (x+dble(i-1))
   80 CONTINUE
      offset = offset - log(prod)
      xx = x + dble(n)
      GO TO 100

   90 xx = x
C
C
C     COMPUTE POWER SERIES
C
C
  100 alngam = devlpl(coef,5,1.0D0/ (xx**2))/xx
      alngam = alngam + offset + (xx-0.5D0)*log(xx) - xx
  110 RETURN

      END
      DOUBLE PRECISION FUNCTION alnrel(a)
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION LN(1 + A)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION p1,p2,p3,q1,q2,q3,t,t2,w,x
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog
C     ..
C     .. Data statements ..
      DATA p1/-.129418923021993D+01/,p2/.405303492862024D+00/,
     +     p3/-.178874546012214D-01/
      DATA q1/-.162752256355323D+01/,q2/.747811014037616D+00/,
     +     q3/-.845104217945565D-01/
C     ..
C     .. Executable Statements ..
C--------------------------
      IF (abs(a).GT.0.375D0) GO TO 10
      t = a/ (a+2.0D0)
      t2 = t*t
      w = (((p3*t2+p2)*t2+p1)*t2+1.0D0)/ (((q3*t2+q2)*t2+q1)*t2+1.0D0)
      alnrel = 2.0D0*t*w
      RETURN
C
   10 x = 1.D0 + dble(a)
      alnrel = dlog(x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION apser(a,b,x,eps)
C-----------------------------------------------------------------------
C     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR
C     A .LE. MIN(EPS,EPS*B), B*X .LE. 1, AND X .LE. 0.5. USED WHEN
C     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION aj,bx,c,g,j,s,t,tol
C     ..
C     .. External Functions ..
      DOUBLE PRECISION psi
      EXTERNAL psi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dlog
C     ..
C     .. Data statements ..
C--------------------
      DATA g/.577215664901533D0/
C     ..
C     .. Executable Statements ..
C--------------------
      bx = b*x
      t = x - bx
      IF (b*eps.GT.2.D-2) GO TO 10
      c = dlog(x) + psi(b) + g + t
      GO TO 20

   10 c = dlog(bx) + g + t
C
   20 tol = 5.0D0*eps*abs(c)
      j = 1.0D0
      s = 0.0D0
   30 j = j + 1.0D0
      t = t* (x-bx/j)
      aj = t/j
      s = s + aj
      IF (abs(aj).GT.tol) GO TO 30
C
      apser = -a* (c+s)
      RETURN

      END
      DOUBLE PRECISION FUNCTION basym(a,b,lambda,eps)
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B.
C     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED.
C     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT
C     A AND B ARE GREATER THAN OR EQUAL TO 15.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,lambda
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION bsum,dsum,e0,e1,f,h,h2,hn,j0,j1,r,r0,r1,s,sum,t,
     +                 t0,t1,u,w,w0,z,z0,z2,zn,znm1
      INTEGER i,im1,imj,j,m,mm1,mmj,n,np1,num
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION a0(21),b0(21),c(21),d(21)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION bcorr,erfc1,rlog1
      EXTERNAL bcorr,erfc1,rlog1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp,sqrt
C     ..
C     .. Data statements ..
C------------------------
C     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP
C            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN.
C            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1.
C
C------------------------
C     E0 = 2/SQRT(PI)
C     E1 = 2**(-3/2)
C------------------------
      DATA num/20/
      DATA e0/1.12837916709551D0/,e1/.353553390593274D0/
C     ..
C     .. Executable Statements ..
C------------------------
      basym = 0.0D0
      IF (a.GE.b) GO TO 10
      h = a/b
      r0 = 1.0D0/ (1.0D0+h)
      r1 = (b-a)/b
      w0 = 1.0D0/sqrt(a* (1.0D0+h))
      GO TO 20

   10 h = b/a
      r0 = 1.0D0/ (1.0D0+h)
      r1 = (b-a)/a
      w0 = 1.0D0/sqrt(b* (1.0D0+h))
C
   20 f = a*rlog1(-lambda/a) + b*rlog1(lambda/b)
      t = exp(-f)
      IF (t.EQ.0.0D0) RETURN
      z0 = sqrt(f)
      z = 0.5D0* (z0/e1)
      z2 = f + f
C
      a0(1) = (2.0D0/3.0D0)*r1
      c(1) = -0.5D0*a0(1)
      d(1) = -c(1)
      j0 = (0.5D0/e0)*erfc1(1,z0)
      j1 = e1
      sum = j0 + d(1)*w0*j1
C
      s = 1.0D0
      h2 = h*h
      hn = 1.0D0
      w = w0
      znm1 = z
      zn = z2
      DO 70 n = 2,num,2
          hn = h2*hn
          a0(n) = 2.0D0*r0* (1.0D0+h*hn)/ (n+2.0D0)
          np1 = n + 1
          s = s + hn
          a0(np1) = 2.0D0*r1*s/ (n+3.0D0)
C
          DO 60 i = n,np1
              r = -0.5D0* (i+1.0D0)
              b0(1) = r*a0(1)
              DO 40 m = 2,i
                  bsum = 0.0D0
                  mm1 = m - 1
                  DO 30 j = 1,mm1
                      mmj = m - j
                      bsum = bsum + (j*r-mmj)*a0(j)*b0(mmj)
   30             CONTINUE
                  b0(m) = r*a0(m) + bsum/m
   40         CONTINUE
              c(i) = b0(i)/ (i+1.0D0)
C
              dsum = 0.0D0
              im1 = i - 1
              DO 50 j = 1,im1
                  imj = i - j
                  dsum = dsum + d(imj)*c(j)
   50         CONTINUE
              d(i) = - (dsum+c(i))
   60     CONTINUE
C
          j0 = e1*znm1 + (n-1.0D0)*j0
          j1 = e1*zn + n*j1
          znm1 = z2*znm1
          zn = z2*zn
          w = w0*w
          t0 = d(n)*w*j0
          w = w0*w
          t1 = d(np1)*w*j1
          sum = sum + (t0+t1)
          IF ((abs(t0)+abs(t1)).LE.eps*sum) GO TO 80
   70 CONTINUE
C
   80 u = exp(-bcorr(a,b))
      basym = e0*t*u*sum
      RETURN

      END
      DOUBLE PRECISION FUNCTION bcorr(a0,b0)
C-----------------------------------------------------------------------
C
C     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE
C     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A).
C     IT IS ASSUMED THAT A0 .GE. 8 AND B0 .GE. 8.
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a0,b0
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,c,c0,c1,c2,c3,c4,c5,h,s11,s3,s5,s7,s9,t,w,x,
     +                 x2
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dmax1,dmin1
C     ..
C     .. Data statements ..
      DATA c0/.833333333333333D-01/,c1/-.277777777760991D-02/,
     +     c2/.793650666825390D-03/,c3/-.595202931351870D-03/,
     +     c4/.837308034031215D-03/,c5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C------------------------
      a = dmin1(a0,b0)
      b = dmax1(a0,b0)
C
      h = a/b
      c = h/ (1.0D0+h)
      x = 1.0D0/ (1.0D0+h)
      x2 = x*x
C
C                SET SN = (1 - X**N)/(1 - X)
C
      s3 = 1.0D0 + (x+x2)
      s5 = 1.0D0 + (x+x2*s3)
      s7 = 1.0D0 + (x+x2*s5)
      s9 = 1.0D0 + (x+x2*s7)
      s11 = 1.0D0 + (x+x2*s9)
C
C                SET W = DEL(B) - DEL(A + B)
C
      t = (1.0D0/b)**2
      w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t + c0
      w = w* (c/b)
C
C                   COMPUTE  DEL(A) + W
C
      t = (1.0D0/a)**2
      bcorr = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/a + w
      RETURN

      END
      DOUBLE PRECISION FUNCTION betaln(a0,b0)
C-----------------------------------------------------------------------
C     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
C-----------------------------------------------------------------------
C     E = 0.5*LN(2*PI)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a0,b0
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,c,e,h,u,v,w,z
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,alnrel,bcorr,gamln,gsumln
      EXTERNAL algdiv,alnrel,bcorr,gamln,gsumln
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog,dmax1,dmin1
C     ..
C     .. Data statements ..
      DATA e/.918938533204673D0/
C     ..
C     .. Executable Statements ..
C--------------------------
      a = dmin1(a0,b0)
      b = dmax1(a0,b0)
      IF (a.GE.8.0D0) GO TO 100
      IF (a.GE.1.0D0) GO TO 20
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .LT. 1
C-----------------------------------------------------------------------
      IF (b.GE.8.0D0) GO TO 10
      betaln = gamln(a) + (gamln(b)-gamln(a+b))
      RETURN

   10 betaln = gamln(a) + algdiv(a,b)
      RETURN
C-----------------------------------------------------------------------
C                PROCEDURE WHEN 1 .LE. A .LT. 8
C-----------------------------------------------------------------------
   20 IF (a.GT.2.0D0) GO TO 40
      IF (b.GT.2.0D0) GO TO 30
      betaln = gamln(a) + gamln(b) - gsumln(a,b)
      RETURN

   30 w = 0.0D0
      IF (b.LT.8.0D0) GO TO 60
      betaln = gamln(a) + algdiv(a,b)
      RETURN
C
C                REDUCTION OF A WHEN B .LE. 1000
C
   40 IF (b.GT.1000.0D0) GO TO 80
      n = a - 1.0D0
      w = 1.0D0
      DO 50 i = 1,n
          a = a - 1.0D0
          h = a/b
          w = w* (h/ (1.0D0+h))
   50 CONTINUE
      w = dlog(w)
      IF (b.LT.8.0D0) GO TO 60
      betaln = w + gamln(a) + algdiv(a,b)
      RETURN
C
C                 REDUCTION OF B WHEN B .LT. 8
C
   60 n = b - 1.0D0
      z = 1.0D0
      DO 70 i = 1,n
          b = b - 1.0D0
          z = z* (b/ (a+b))
   70 CONTINUE
      betaln = w + dlog(z) + (gamln(a)+ (gamln(b)-gsumln(a,b)))
      RETURN
C
C                REDUCTION OF A WHEN B .GT. 1000
C
   80 n = a - 1.0D0
      w = 1.0D0
      DO 90 i = 1,n
          a = a - 1.0D0
          w = w* (a/ (1.0D0+a/b))
   90 CONTINUE
      betaln = (dlog(w)-n*dlog(b)) + (gamln(a)+algdiv(a,b))
      RETURN
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .GE. 8
C-----------------------------------------------------------------------
  100 w = bcorr(a,b)
      h = a/b
      c = h/ (1.0D0+h)
      u = - (a-0.5D0)*dlog(c)
      v = b*alnrel(h)
      IF (u.LE.v) GO TO 110
      betaln = (((-0.5D0*dlog(b)+e)+w)-v) - u
      RETURN

  110 betaln = (((-0.5D0*dlog(b)+e)+w)-u) - v
      RETURN

      END
      DOUBLE PRECISION FUNCTION bfrac(a,b,x,y,lambda,eps)
C-----------------------------------------------------------------------
C     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B .GT. 1.
C     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,lambda,x,y
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION alpha,an,anp1,beta,bn,bnp1,c,c0,c1,e,n,p,r,r0,s,
     +                 t,w,yp1
C     ..
C     .. External Functions ..
      DOUBLE PRECISION brcomp
      EXTERNAL brcomp
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs
C     ..
C     .. Executable Statements ..
C--------------------
      bfrac = brcomp(a,b,x,y)
      IF (bfrac.EQ.0.0D0) RETURN
C
      c = 1.0D0 + lambda
      c0 = b/a
      c1 = 1.0D0 + 1.0D0/a
      yp1 = y + 1.0D0
C
      n = 0.0D0
      p = 1.0D0
      s = a + 1.0D0
      an = 0.0D0
      bn = 1.0D0
      anp1 = 1.0D0
      bnp1 = c/c1
      r = c1/c
C
C        CONTINUED FRACTION CALCULATION
C
   10 n = n + 1.0D0
      t = n/a
      w = n* (b-n)*x
      e = a/s
      alpha = (p* (p+c0)*e*e)* (w*x)
      e = (1.0D0+t)/ (c1+t+t)
      beta = n + w/s + e* (c+n*yp1)
      p = 1.0D0 + t
      s = s + 2.0D0
C
C        UPDATE AN, BN, ANP1, AND BNP1
C
      t = alpha*an + beta*anp1
      an = anp1
      anp1 = t
      t = alpha*bn + beta*bnp1
      bn = bnp1
      bnp1 = t
C
      r0 = r
      r = anp1/bnp1
      IF (abs(r-r0).LE.eps*r) GO TO 20
C
C        RESCALE AN, BN, ANP1, AND BNP1
C
      an = an/bnp1
      bn = bn/bnp1
      anp1 = r
      bnp1 = 1.0D0
      GO TO 10
C
C                 TERMINATION
C
   20 bfrac = bfrac*r
      RETURN

      END
      SUBROUTINE bgrat(a,b,x,y,w,eps,ierr)
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR IX(A,B) WHEN A IS LARGER THAN B.
C     THE RESULT OF THE EXPANSION IS ADDED TO W. IT IS ASSUMED
C     THAT A .GE. 15 AND B .LE. 1.  EPS IS THE TOLERANCE USED.
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,w,x,y
      INTEGER ierr
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION bm1,bp2n,cn,coef,dj,j,l,lnx,n2,nu,p,q,r,s,sum,t,
     +                 t2,u,v,z
      INTEGER i,n,nm1
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION c(30),d(30)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,alnrel,gam1
      EXTERNAL algdiv,alnrel,gam1
C     ..
C     .. External Subroutines ..
      EXTERNAL grat1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dlog,exp
C     ..
C     .. Executable Statements ..
C
      bm1 = (b-0.5D0) - 0.5D0
      nu = a + 0.5D0*bm1
      IF (y.GT.0.375D0) GO TO 10
      lnx = alnrel(-y)
      GO TO 20

   10 lnx = dlog(x)
   20 z = -nu*lnx
      IF (b*z.EQ.0.0D0) GO TO 70
C
C                 COMPUTATION OF THE EXPANSION
C                 SET R = EXP(-Z)*Z**B/GAMMA(B)
C
      r = b* (1.0D0+gam1(b))*exp(b*dlog(z))
      r = r*exp(a*lnx)*exp(0.5D0*bm1*lnx)
      u = algdiv(b,a) + b*dlog(nu)
      u = r*exp(-u)
      IF (u.EQ.0.0D0) GO TO 70
      CALL grat1(b,z,r,p,q,eps)
C
      v = 0.25D0* (1.0D0/nu)**2
      t2 = 0.25D0*lnx*lnx
      l = w/u
      j = q/r
      sum = j
      t = 1.0D0
      cn = 1.0D0
      n2 = 0.0D0
      DO 50 n = 1,30
          bp2n = b + n2
          j = (bp2n* (bp2n+1.0D0)*j+ (z+bp2n+1.0D0)*t)*v
          n2 = n2 + 2.0D0
          t = t*t2
          cn = cn/ (n2* (n2+1.0D0))
          c(n) = cn
          s = 0.0D0
          IF (n.EQ.1) GO TO 40
          nm1 = n - 1
          coef = b - n
          DO 30 i = 1,nm1
              s = s + coef*c(i)*d(n-i)
              coef = coef + b
   30     CONTINUE
   40     d(n) = bm1*cn + s/n
          dj = d(n)*j
          sum = sum + dj
          IF (sum.LE.0.0D0) GO TO 70
          IF (abs(dj).LE.eps* (sum+l)) GO TO 60
   50 CONTINUE
C
C                    ADD THE RESULTS TO W
C
   60 ierr = 0
      w = w + u*sum
      RETURN
C
C               THE EXPANSION CANNOT BE COMPUTED
C
   70 ierr = 1
      RETURN

      END
      DOUBLE PRECISION FUNCTION bpser(a,b,x,eps)
C-----------------------------------------------------------------------
C     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1
C     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a0,apb,b0,c,n,sum,t,tol,u,w,z
      INTEGER i,m
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,betaln,gam1,gamln1
      EXTERNAL algdiv,betaln,gam1,gamln1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,dmin1,exp
C     ..
C     .. Executable Statements ..
C
      bpser = 0.0D0
      IF (x.EQ.0.0D0) RETURN
C-----------------------------------------------------------------------
C            COMPUTE THE FACTOR X**A/(A*BETA(A,B))
C-----------------------------------------------------------------------
      a0 = dmin1(a,b)
      IF (a0.LT.1.0D0) GO TO 10
      z = a*dlog(x) - betaln(a,b)
      bpser = exp(z)/a
      GO TO 100

   10 b0 = dmax1(a,b)
      IF (b0.GE.8.0D0) GO TO 90
      IF (b0.GT.1.0D0) GO TO 40
C
C            PROCEDURE FOR A0 .LT. 1 AND B0 .LE. 1
C
      bpser = x**a
      IF (bpser.EQ.0.0D0) RETURN
C
      apb = a + b
      IF (apb.GT.1.0D0) GO TO 20
      z = 1.0D0 + gam1(apb)
      GO TO 30

   20 u = dble(a) + dble(b) - 1.D0
      z = (1.0D0+gam1(u))/apb
C
   30 c = (1.0D0+gam1(a))* (1.0D0+gam1(b))/z
      bpser = bpser*c* (b/apb)
      GO TO 100
C
C         PROCEDURE FOR A0 .LT. 1 AND 1 .LT. B0 .LT. 8
C
   40 u = gamln1(a0)
      m = b0 - 1.0D0
      IF (m.LT.1) GO TO 60
      c = 1.0D0
      DO 50 i = 1,m
          b0 = b0 - 1.0D0
          c = c* (b0/ (a0+b0))
   50 CONTINUE
      u = dlog(c) + u
C
   60 z = a*dlog(x) - u
      b0 = b0 - 1.0D0
      apb = a0 + b0
      IF (apb.GT.1.0D0) GO TO 70
      t = 1.0D0 + gam1(apb)
      GO TO 80

   70 u = dble(a0) + dble(b0) - 1.D0
      t = (1.0D0+gam1(u))/apb
   80 bpser = exp(z)* (a0/a)* (1.0D0+gam1(b0))/t
      GO TO 100
C
C            PROCEDURE FOR A0 .LT. 1 AND B0 .GE. 8
C
   90 u = gamln1(a0) + algdiv(a0,b0)
      z = a*dlog(x) - u
      bpser = (a0/a)*exp(z)
  100 IF (bpser.EQ.0.0D0 .OR. a.LE.0.1D0*eps) RETURN
C-----------------------------------------------------------------------
C                     COMPUTE THE SERIES
C-----------------------------------------------------------------------
      sum = 0.0D0
      n = 0.0D0
      c = 1.0D0
      tol = eps/a
  110 n = n + 1.0D0
      c = c* (0.5D0+ (0.5D0-b/n))*x
      w = c/ (a+n)
      sum = sum + w
      IF (abs(w).GT.tol) GO TO 110
      bpser = bpser* (1.0D0+a*sum)
      RETURN

      END
      SUBROUTINE bratio(a,b,x,y,w,w1,ierr)
C-----------------------------------------------------------------------
C
C            EVALUATION OF THE INCOMPLETE BETA FUNCTION IX(A,B)
C
C                     --------------------
C
C     IT IS ASSUMED THAT A AND B ARE NONNEGATIVE, AND THAT X .LE. 1
C     AND Y = 1 - X.  BRATIO ASSIGNS W AND W1 THE VALUES
C
C                      W  = IX(A,B)
C                      W1 = 1 - IX(A,B)
C
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C     IF NO INPUT ERRORS ARE DETECTED THEN IERR IS SET TO 0 AND
C     W AND W1 ARE COMPUTED. OTHERWISE, IF AN ERROR IS DETECTED,
C     THEN W AND W1 ARE ASSIGNED THE VALUE 0 AND IERR IS SET TO
C     ONE OF THE FOLLOWING VALUES ...
C
C        IERR = 1  IF A OR B IS NEGATIVE
C        IERR = 2  IF A = B = 0
C        IERR = 3  IF X .LT. 0 OR X .GT. 1
C        IERR = 4  IF Y .LT. 0 OR Y .GT. 1
C        IERR = 5  IF X + Y .NE. 1
C        IERR = 6  IF X = A = 0
C        IERR = 7  IF Y = B = 0
C
C--------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WARFARE CENTER
C        DAHLGREN, VIRGINIA
C     REVISED ... NOV 1991
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,w,w1,x,y
      INTEGER ierr
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a0,b0,eps,lambda,t,x0,y0,z
      INTEGER ierr1,ind,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION apser,basym,bfrac,bpser,bup,fpser,spmpar
      EXTERNAL apser,basym,bfrac,bpser,bup,fpser,spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL bgrat
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dmax1,dmin1
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     ****** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST
C            FLOATING POINT NUMBER FOR WHICH 1.0 + EPS .GT. 1.0
C
      eps = spmpar(1)
C
C-----------------------------------------------------------------------
      w = 0.0D0
      w1 = 0.0D0
      IF (a.LT.0.0D0 .OR. b.LT.0.0D0) GO TO 270
      IF (a.EQ.0.0D0 .AND. b.EQ.0.0D0) GO TO 280
      IF (x.LT.0.0D0 .OR. x.GT.1.0D0) GO TO 290
      IF (y.LT.0.0D0 .OR. y.GT.1.0D0) GO TO 300
      z = ((x+y)-0.5D0) - 0.5D0
      IF (abs(z).GT.3.0D0*eps) GO TO 310
C
      ierr = 0
      IF (x.EQ.0.0D0) GO TO 210
      IF (y.EQ.0.0D0) GO TO 230
      IF (a.EQ.0.0D0) GO TO 240
      IF (b.EQ.0.0D0) GO TO 220
C
      eps = dmax1(eps,1.D-15)
      IF (dmax1(a,b).LT.1.D-3*eps) GO TO 260
C
      ind = 0
      a0 = a
      b0 = b
      x0 = x
      y0 = y
      IF (dmin1(a0,b0).GT.1.0D0) GO TO 40
C
C             PROCEDURE FOR A0 .LE. 1 OR B0 .LE. 1
C
      IF (x.LE.0.5D0) GO TO 10
      ind = 1
      a0 = b
      b0 = a
      x0 = y
      y0 = x
C
   10 IF (b0.LT.dmin1(eps,eps*a0)) GO TO 90
      IF (a0.LT.dmin1(eps,eps*b0) .AND. b0*x0.LE.1.0D0) GO TO 100
      IF (dmax1(a0,b0).GT.1.0D0) GO TO 20
      IF (a0.GE.dmin1(0.2D0,b0)) GO TO 110
      IF (x0**a0.LE.0.9D0) GO TO 110
      IF (x0.GE.0.3D0) GO TO 120
      n = 20
      GO TO 140
C
   20 IF (b0.LE.1.0D0) GO TO 110
      IF (x0.GE.0.3D0) GO TO 120
      IF (x0.GE.0.1D0) GO TO 30
      IF ((x0*b0)**a0.LE.0.7D0) GO TO 110
   30 IF (b0.GT.15.0D0) GO TO 150
      n = 20
      GO TO 140
C
C             PROCEDURE FOR A0 .GT. 1 AND B0 .GT. 1
C
   40 IF (a.GT.b) GO TO 50
      lambda = a - (a+b)*x
      GO TO 60

   50 lambda = (a+b)*y - b
   60 IF (lambda.GE.0.0D0) GO TO 70
      ind = 1
      a0 = b
      b0 = a
      x0 = y
      y0 = x
      lambda = abs(lambda)
C
   70 IF (b0.LT.40.0D0 .AND. b0*x0.LE.0.7D0) GO TO 110
      IF (b0.LT.40.0D0) GO TO 160
      IF (a0.GT.b0) GO TO 80
      IF (a0.LE.100.0D0) GO TO 130
      IF (lambda.GT.0.03D0*a0) GO TO 130
      GO TO 200

   80 IF (b0.LE.100.0D0) GO TO 130
      IF (lambda.GT.0.03D0*b0) GO TO 130
      GO TO 200
C
C            EVALUATION OF THE APPROPRIATE ALGORITHM
C
   90 w = fpser(a0,b0,x0,eps)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
  100 w1 = apser(a0,b0,x0,eps)
      w = 0.5D0 + (0.5D0-w1)
      GO TO 250
C
  110 w = bpser(a0,b0,x0,eps)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
  120 w1 = bpser(b0,a0,y0,eps)
      w = 0.5D0 + (0.5D0-w1)
      GO TO 250
C
  130 w = bfrac(a0,b0,x0,y0,lambda,15.0D0*eps)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
  140 w1 = bup(b0,a0,y0,x0,n,eps)
      b0 = b0 + n
  150 CALL bgrat(b0,a0,y0,x0,w1,15.0D0*eps,ierr1)
      w = 0.5D0 + (0.5D0-w1)
      GO TO 250
C
  160 n = b0
      b0 = b0 - n
      IF (b0.NE.0.0D0) GO TO 170
      n = n - 1
      b0 = 1.0D0
  170 w = bup(b0,a0,y0,x0,n,eps)
      IF (x0.GT.0.7D0) GO TO 180
      w = w + bpser(a0,b0,x0,eps)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
  180 IF (a0.GT.15.0D0) GO TO 190
      n = 20
      w = w + bup(a0,b0,x0,y0,n,eps)
      a0 = a0 + n
  190 CALL bgrat(a0,b0,x0,y0,w,15.0D0*eps,ierr1)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
  200 w = basym(a0,b0,lambda,100.0D0*eps)
      w1 = 0.5D0 + (0.5D0-w)
      GO TO 250
C
C               TERMINATION OF THE PROCEDURE
C
  210 IF (a.EQ.0.0D0) GO TO 320
  220 w = 0.0D0
      w1 = 1.0D0
      RETURN
C
  230 IF (b.EQ.0.0D0) GO TO 330
  240 w = 1.0D0
      w1 = 0.0D0
      RETURN
C
  250 IF (ind.EQ.0) RETURN
      t = w
      w = w1
      w1 = t
      RETURN
C
C           PROCEDURE FOR A AND B .LT. 1.E-3*EPS
C
  260 w = b/ (a+b)
      w1 = a/ (a+b)
      RETURN
C
C                       ERROR RETURN
C
  270 ierr = 1
      RETURN

  280 ierr = 2
      RETURN

  290 ierr = 3
      RETURN

  300 ierr = 4
      RETURN

  310 ierr = 5
      RETURN

  320 ierr = 6
      RETURN

  330 ierr = 7
      RETURN

      END
      DOUBLE PRECISION FUNCTION brcmp1(mu,a,b,x,y)
C-----------------------------------------------------------------------
C          EVALUATION OF  EXP(MU) * (X**A*Y**B/BETA(A,B))
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,x,y
      INTEGER mu
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a0,apb,b0,c,const,e,h,lambda,lnx,lny,t,u,v,x0,y0,
     +                 z
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,alnrel,bcorr,betaln,esum,gam1,gamln1,rlog1
      EXTERNAL algdiv,alnrel,bcorr,betaln,esum,gam1,gamln1,rlog1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,dmin1,exp,sqrt
C     ..
C     .. Data statements ..
C-----------------
C     CONST = 1/SQRT(2*PI)
C-----------------
      DATA const/.398942280401433D0/
C     ..
C     .. Executable Statements ..
C
      a0 = dmin1(a,b)
      IF (a0.GE.8.0D0) GO TO 130
C
      IF (x.GT.0.375D0) GO TO 10
      lnx = dlog(x)
      lny = alnrel(-x)
      GO TO 30

   10 IF (y.GT.0.375D0) GO TO 20
      lnx = alnrel(-y)
      lny = dlog(y)
      GO TO 30

   20 lnx = dlog(x)
      lny = dlog(y)
C
   30 z = a*lnx + b*lny
      IF (a0.LT.1.0D0) GO TO 40
      z = z - betaln(a,b)
      brcmp1 = esum(mu,z)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .LT. 1 OR B .LT. 1
C-----------------------------------------------------------------------
   40 b0 = dmax1(a,b)
      IF (b0.GE.8.0D0) GO TO 120
      IF (b0.GT.1.0D0) GO TO 70
C
C                   ALGORITHM FOR B0 .LE. 1
C
      brcmp1 = esum(mu,z)
      IF (brcmp1.EQ.0.0D0) RETURN
C
      apb = a + b
      IF (apb.GT.1.0D0) GO TO 50
      z = 1.0D0 + gam1(apb)
      GO TO 60

   50 u = dble(a) + dble(b) - 1.D0
      z = (1.0D0+gam1(u))/apb
C
   60 c = (1.0D0+gam1(a))* (1.0D0+gam1(b))/z
      brcmp1 = brcmp1* (a0*c)/ (1.0D0+a0/b0)
      RETURN
C
C                ALGORITHM FOR 1 .LT. B0 .LT. 8
C
   70 u = gamln1(a0)
      n = b0 - 1.0D0
      IF (n.LT.1) GO TO 90
      c = 1.0D0
      DO 80 i = 1,n
          b0 = b0 - 1.0D0
          c = c* (b0/ (a0+b0))
   80 CONTINUE
      u = dlog(c) + u
C
   90 z = z - u
      b0 = b0 - 1.0D0
      apb = a0 + b0
      IF (apb.GT.1.0D0) GO TO 100
      t = 1.0D0 + gam1(apb)
      GO TO 110

  100 u = dble(a0) + dble(b0) - 1.D0
      t = (1.0D0+gam1(u))/apb
  110 brcmp1 = a0*esum(mu,z)* (1.0D0+gam1(b0))/t
      RETURN
C
C                   ALGORITHM FOR B0 .GE. 8
C
  120 u = gamln1(a0) + algdiv(a0,b0)
      brcmp1 = a0*esum(mu,z-u)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .GE. 8 AND B .GE. 8
C-----------------------------------------------------------------------
  130 IF (a.GT.b) GO TO 140
      h = a/b
      x0 = h/ (1.0D0+h)
      y0 = 1.0D0/ (1.0D0+h)
      lambda = a - (a+b)*x
      GO TO 150

  140 h = b/a
      x0 = 1.0D0/ (1.0D0+h)
      y0 = h/ (1.0D0+h)
      lambda = (a+b)*y - b
C
  150 e = -lambda/a
      IF (abs(e).GT.0.6D0) GO TO 160
      u = rlog1(e)
      GO TO 170

  160 u = e - dlog(x/x0)
C
  170 e = lambda/b
      IF (abs(e).GT.0.6D0) GO TO 180
      v = rlog1(e)
      GO TO 190

  180 v = e - dlog(y/y0)
C
  190 z = esum(mu,- (a*u+b*v))
      brcmp1 = const*sqrt(b*x0)*z*exp(-bcorr(a,b))
      RETURN

      END
      DOUBLE PRECISION FUNCTION brcomp(a,b,x,y)
C-----------------------------------------------------------------------
C               EVALUATION OF X**A*Y**B/BETA(A,B)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,x,y
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a0,apb,b0,c,const,e,h,lambda,lnx,lny,t,u,v,x0,y0,
     +                 z
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,alnrel,bcorr,betaln,gam1,gamln1,rlog1
      EXTERNAL algdiv,alnrel,bcorr,betaln,gam1,gamln1,rlog1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,dmin1,exp,sqrt
C     ..
C     .. Data statements ..
C-----------------
C     CONST = 1/SQRT(2*PI)
C-----------------
      DATA const/.398942280401433D0/
C     ..
C     .. Executable Statements ..
C
      brcomp = 0.0D0
      IF (x.EQ.0.0D0 .OR. y.EQ.0.0D0) RETURN
      a0 = dmin1(a,b)
      IF (a0.GE.8.0D0) GO TO 130
C
      IF (x.GT.0.375D0) GO TO 10
      lnx = dlog(x)
      lny = alnrel(-x)
      GO TO 30

   10 IF (y.GT.0.375D0) GO TO 20
      lnx = alnrel(-y)
      lny = dlog(y)
      GO TO 30

   20 lnx = dlog(x)
      lny = dlog(y)
C
   30 z = a*lnx + b*lny
      IF (a0.LT.1.0D0) GO TO 40
      z = z - betaln(a,b)
      brcomp = exp(z)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .LT. 1 OR B .LT. 1
C-----------------------------------------------------------------------
   40 b0 = dmax1(a,b)
      IF (b0.GE.8.0D0) GO TO 120
      IF (b0.GT.1.0D0) GO TO 70
C
C                   ALGORITHM FOR B0 .LE. 1
C
      brcomp = exp(z)
      IF (brcomp.EQ.0.0D0) RETURN
C
      apb = a + b
      IF (apb.GT.1.0D0) GO TO 50
      z = 1.0D0 + gam1(apb)
      GO TO 60

   50 u = dble(a) + dble(b) - 1.D0
      z = (1.0D0+gam1(u))/apb
C
   60 c = (1.0D0+gam1(a))* (1.0D0+gam1(b))/z
      brcomp = brcomp* (a0*c)/ (1.0D0+a0/b0)
      RETURN
C
C                ALGORITHM FOR 1 .LT. B0 .LT. 8
C
   70 u = gamln1(a0)
      n = b0 - 1.0D0
      IF (n.LT.1) GO TO 90
      c = 1.0D0
      DO 80 i = 1,n
          b0 = b0 - 1.0D0
          c = c* (b0/ (a0+b0))
   80 CONTINUE
      u = dlog(c) + u
C
   90 z = z - u
      b0 = b0 - 1.0D0
      apb = a0 + b0
      IF (apb.GT.1.0D0) GO TO 100
      t = 1.0D0 + gam1(apb)
      GO TO 110

  100 u = dble(a0) + dble(b0) - 1.D0
      t = (1.0D0+gam1(u))/apb
  110 brcomp = a0*exp(z)* (1.0D0+gam1(b0))/t
      RETURN
C
C                   ALGORITHM FOR B0 .GE. 8
C
  120 u = gamln1(a0) + algdiv(a0,b0)
      brcomp = a0*exp(z-u)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .GE. 8 AND B .GE. 8
C-----------------------------------------------------------------------
  130 IF (a.GT.b) GO TO 140
      h = a/b
      x0 = h/ (1.0D0+h)
      y0 = 1.0D0/ (1.0D0+h)
      lambda = a - (a+b)*x
      GO TO 150

  140 h = b/a
      x0 = 1.0D0/ (1.0D0+h)
      y0 = h/ (1.0D0+h)
      lambda = (a+b)*y - b
C
  150 e = -lambda/a
      IF (abs(e).GT.0.6D0) GO TO 160
      u = rlog1(e)
      GO TO 170

  160 u = e - dlog(x/x0)
C
  170 e = lambda/b
      IF (abs(e).GT.0.6D0) GO TO 180
      v = rlog1(e)
      GO TO 190

  180 v = e - dlog(y/y0)
C
  190 z = exp(- (a*u+b*v))
      brcomp = const*sqrt(b*x0)*z*exp(-bcorr(a,b))
      RETURN

      END
      DOUBLE PRECISION FUNCTION bup(a,b,x,y,n,eps)
C-----------------------------------------------------------------------
C     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INTEGER.
C     EPS IS THE TOLERANCE USED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,x,y
      INTEGER n
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ap1,apb,d,l,r,t,w
      INTEGER i,k,kp1,mu,nm1
C     ..
C     .. External Functions ..
      DOUBLE PRECISION brcmp1,exparg
      EXTERNAL brcmp1,exparg
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp
C     ..
C     .. Executable Statements ..
C
C          OBTAIN THE SCALING FACTOR EXP(-MU) AND
C             EXP(MU)*(X**A*Y**B/BETA(A,B))/A
C
      apb = a + b
      ap1 = a + 1.0D0
      mu = 0
      d = 1.0D0
      IF (n.EQ.1 .OR. a.LT.1.0D0) GO TO 10
      IF (apb.LT.1.1D0*ap1) GO TO 10
      mu = abs(exparg(1))
      k = exparg(0)
      IF (k.LT.mu) mu = k
      t = mu
      d = exp(-t)
C
   10 bup = brcmp1(mu,a,b,x,y)/a
      IF (n.EQ.1 .OR. bup.EQ.0.0D0) RETURN
      nm1 = n - 1
      w = d
C
C          LET K BE THE INDEX OF THE MAXIMUM TERM
C
      k = 0
      IF (b.LE.1.0D0) GO TO 50
      IF (y.GT.1.D-4) GO TO 20
      k = nm1
      GO TO 30

   20 r = (b-1.0D0)*x/y - a
      IF (r.LT.1.0D0) GO TO 50
      k = nm1
      t = nm1
      IF (r.LT.t) k = r
C
C          ADD THE INCREASING TERMS OF THE SERIES
C
   30 DO 40 i = 1,k
          l = i - 1
          d = ((apb+l)/ (ap1+l))*x*d
          w = w + d
   40 CONTINUE
      IF (k.EQ.nm1) GO TO 70
C
C          ADD THE REMAINING TERMS OF THE SERIES
C
   50 kp1 = k + 1
      DO 60 i = kp1,nm1
          l = i - 1
          d = ((apb+l)/ (ap1+l))*x*d
          w = w + d
          IF (d.LE.eps*w) GO TO 70
   60 CONTINUE
C
C               TERMINATE THE PROCEDURE
C
   70 bup = bup*w
      RETURN

      END
      SUBROUTINE cdfbet(which,p,q,x,y,a,b,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFBET( WHICH, P, Q, X, Y, A, B, STATUS, BOUND )
C               Cumulative Distribution Function
C                         BETa Distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the beta distribution given
C     values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,Y,A and B
C               iwhich = 2 : Calculate X and Y from P,Q,A and B
C               iwhich = 3 : Calculate A from P,Q,X,Y and B
C               iwhich = 4 : Calculate B from P,Q,X,Y and A
C
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: [0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of beta density.
C            Input range: [0,1].
C            Search range: [0,1]
C                    DOUBLE PRECISION X
C
C     Y <--> 1-X.
C            Input range: [0,1].
C            Search range: [0,1]
C            X + Y = 1.0.
C                    DOUBLE PRECISION Y
C
C     A <--> The first parameter of the beta density.
C            Input range: (0, +infinity).
C            Search range: [1D-300,1D300]
C                    DOUBLE PRECISION A
C
C     B <--> The second parameter of the beta density.
C            Input range: (0, +infinity).
C            Search range: [1D-300,1D300]
C                    DOUBLE PRECISION B
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if X + Y .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Cumulative distribution function  (P)  is calculated directly by
C     code associated with the following reference.
C
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                              Note
C
C
C     The beta density is proportional to
C               t^(A-1) * (1-t)^(B-1)
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
      DOUBLE PRECISION one
      PARAMETER (one=1.0D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,bound,p,q,x,y
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,xhi,xlo,cum,ccum,xy,pq
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL cumbet,dinvr,dstinv,dstzr,dzror
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LT.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LT.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 150
C
C     X
C
      IF (.NOT. ((x.LT.0.0D0).OR. (x.GT.1.0D0))) GO TO 140
      IF (.NOT. (x.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      GO TO 130

  120 bound = 1.0D0
  130 status = -4
      RETURN

  140 CONTINUE
  150 IF (which.EQ.2) GO TO 190
C
C     Y
C
      IF (.NOT. ((y.LT.0.0D0).OR. (y.GT.1.0D0))) GO TO 180
      IF (.NOT. (y.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = -5
      RETURN

  180 CONTINUE
  190 IF (which.EQ.3) GO TO 210
C
C     A
C
      IF (.NOT. (a.LE.0.0D0)) GO TO 200
      bound = 0.0D0
      status = -6
      RETURN

  200 CONTINUE
  210 IF (which.EQ.4) GO TO 230
C
C     B
C
      IF (.NOT. (b.LE.0.0D0)) GO TO 220
      bound = 0.0D0
      status = -7
      RETURN

  220 CONTINUE
  230 IF (which.EQ.1) GO TO 270
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 260
      IF (.NOT. (pq.LT.0.0D0)) GO TO 240
      bound = 0.0D0
      GO TO 250

  240 bound = 1.0D0
  250 status = 3
      RETURN

  260 CONTINUE
  270 IF (which.EQ.2) GO TO 310
C
C     X + Y
C
      xy = x + y
      IF (.NOT. (abs(((xy)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 300
      IF (.NOT. (xy.LT.0.0D0)) GO TO 280
      bound = 0.0D0
      GO TO 290

  280 bound = 1.0D0
  290 status = 4
      RETURN

  300 CONTINUE
  310 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Calculating P and Q
C
          CALL cumbet(x,y,a,b,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating X and Y
C
          CALL dstzr(0.0D0,1.0D0,atol,tol)
          IF (.NOT. (qporq)) GO TO 340
          status = 0
          CALL dzror(status,x,fx,xlo,xhi,qleft,qhi)
          y = one - x
  320     IF (.NOT. (status.EQ.1)) GO TO 330
          CALL cumbet(x,y,a,b,cum,ccum)
          fx = cum - p
          CALL dzror(status,x,fx,xlo,xhi,qleft,qhi)
          y = one - x
          GO TO 320

  330     GO TO 370

  340     status = 0
          CALL dzror(status,y,fx,xlo,xhi,qleft,qhi)
          x = one - y
  350     IF (.NOT. (status.EQ.1)) GO TO 360
          CALL cumbet(x,y,a,b,cum,ccum)
          fx = ccum - q
          CALL dzror(status,y,fx,xlo,xhi,qleft,qhi)
          x = one - y
          GO TO 350

  360     CONTINUE
  370     IF (.NOT. (status.EQ.-1)) GO TO 400
          IF (.NOT. (qleft)) GO TO 380
          status = 1
          bound = 0.0D0
          GO TO 390

  380     status = 2
          bound = 1.0D0
  390     CONTINUE
  400     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Computing A
C
          a = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,a,fx,qleft,qhi)
  410     IF (.NOT. (status.EQ.1)) GO TO 440
          CALL cumbet(x,y,a,b,cum,ccum)
          IF (.NOT. (qporq)) GO TO 420
          fx = cum - p
          GO TO 430

  420     fx = ccum - q
  430     CALL dinvr(status,a,fx,qleft,qhi)
          GO TO 410

  440     IF (.NOT. (status.EQ.-1)) GO TO 470
          IF (.NOT. (qleft)) GO TO 450
          status = 1
          bound = zero
          GO TO 460

  450     status = 2
          bound = inf
  460     CONTINUE
  470     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Computing B
C
          b = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,b,fx,qleft,qhi)
  480     IF (.NOT. (status.EQ.1)) GO TO 510
          CALL cumbet(x,y,a,b,cum,ccum)
          IF (.NOT. (qporq)) GO TO 490
          fx = cum - p
          GO TO 500

  490     fx = ccum - q
  500     CALL dinvr(status,b,fx,qleft,qhi)
          GO TO 480

  510     IF (.NOT. (status.EQ.-1)) GO TO 540
          IF (.NOT. (qleft)) GO TO 520
          status = 1
          bound = zero
          GO TO 530

  520     status = 2
          bound = inf
  530     CONTINUE
  540 END IF

      RETURN
C
      END
      SUBROUTINE cdfbin(which,p,q,s,xn,pr,ompr,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFBIN ( WHICH, P, Q, S, XN, PR, OMPR, STATUS, BOUND )
C               Cumulative Distribution Function
C                         BINomial distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the binomial
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
C               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
C               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
C               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN
C                    INTEGER WHICH
C
C     P <--> The cumulation from 0 to S of the binomial distribution.
C            (Probablility of S or fewer successes in XN trials each
C            with probability of success PR.)
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: [0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     S <--> The number of successes observed.
C            Input range: [0, XN]
C            Search range: [0, XN]
C                    DOUBLE PRECISION S
C
C     XN  <--> The number of binomial trials.
C              Input range: (0, +infinity).
C              Search range: [1E-300, 1E300]
C                    DOUBLE PRECISION XN
C
C     PR  <--> The probability of success in each binomial trial.
C              Input range: [0,1].
C              Search range: [0,1]
C                    DOUBLE PRECISION PR
C
C     OMPR  <--> 1-PR
C              Input range: [0,1].
C              Search range: [0,1]
C              PR + OMPR = 1.0
C                    DOUBLE PRECISION OMPR
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if PR + OMPR .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  binomial
C     distribution  to  the  cumulative incomplete    beta distribution.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************

C     .. Parameters ..
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
      DOUBLE PRECISION one
      PARAMETER (one=1.0D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,p,q,pr,ompr,s,xn
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,xhi,xlo,cum,ccum,pq,prompr
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,dstzr,dzror,cumbin
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).AND. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LT.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LT.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.3) GO TO 130
C
C     XN
C
      IF (.NOT. (xn.LE.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -5
      RETURN

  120 CONTINUE
  130 IF (which.EQ.2) GO TO 170
C
C     S
C
      IF (.NOT. ((s.LT.0.0D0).OR. ((which.NE.3).AND.
     +    (s.GT.xn)))) GO TO 160
      IF (.NOT. (s.LT.0.0D0)) GO TO 140
      bound = 0.0D0
      GO TO 150

  140 bound = xn
  150 status = -4
      RETURN

  160 CONTINUE
  170 IF (which.EQ.4) GO TO 210
C
C     PR
C
      IF (.NOT. ((pr.LT.0.0D0).OR. (pr.GT.1.0D0))) GO TO 200
      IF (.NOT. (pr.LT.0.0D0)) GO TO 180
      bound = 0.0D0
      GO TO 190

  180 bound = 1.0D0
  190 status = -6
      RETURN

  200 CONTINUE
  210 IF (which.EQ.4) GO TO 250
C
C     OMPR
C
      IF (.NOT. ((ompr.LT.0.0D0).OR. (ompr.GT.1.0D0))) GO TO 240
      IF (.NOT. (ompr.LT.0.0D0)) GO TO 220
      bound = 0.0D0
      GO TO 230

  220 bound = 1.0D0
  230 status = -7
      RETURN

  240 CONTINUE
  250 IF (which.EQ.1) GO TO 290
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 280
      IF (.NOT. (pq.LT.0.0D0)) GO TO 260
      bound = 0.0D0
      GO TO 270

  260 bound = 1.0D0
  270 status = 3
      RETURN

  280 CONTINUE
  290 IF (which.EQ.4) GO TO 330
C
C     PR + OMPR
C
      prompr = pr + ompr
      IF (.NOT. (abs(((prompr)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 320
      IF (.NOT. (prompr.LT.0.0D0)) GO TO 300
      bound = 0.0D0
      GO TO 310

  300 bound = 1.0D0
  310 status = 4
      RETURN

  320 CONTINUE
  330 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          CALL cumbin(s,xn,pr,ompr,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating S
C
          s = 5.0D0
          CALL dstinv(0.0D0,xn,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,s,fx,qleft,qhi)
  340     IF (.NOT. (status.EQ.1)) GO TO 370
          CALL cumbin(s,xn,pr,ompr,cum,ccum)
          IF (.NOT. (qporq)) GO TO 350
          fx = cum - p
          GO TO 360

  350     fx = ccum - q
  360     CALL dinvr(status,s,fx,qleft,qhi)
          GO TO 340

  370     IF (.NOT. (status.EQ.-1)) GO TO 400
          IF (.NOT. (qleft)) GO TO 380
          status = 1
          bound = 0.0D0
          GO TO 390

  380     status = 2
          bound = xn
  390     CONTINUE
  400     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating XN
C
          xn = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,xn,fx,qleft,qhi)
  410     IF (.NOT. (status.EQ.1)) GO TO 440
          CALL cumbin(s,xn,pr,ompr,cum,ccum)
          IF (.NOT. (qporq)) GO TO 420
          fx = cum - p
          GO TO 430

  420     fx = ccum - q
  430     CALL dinvr(status,xn,fx,qleft,qhi)
          GO TO 410

  440     IF (.NOT. (status.EQ.-1)) GO TO 470
          IF (.NOT. (qleft)) GO TO 450
          status = 1
          bound = zero
          GO TO 460

  450     status = 2
          bound = inf
  460     CONTINUE
  470     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Calculating PR and OMPR
C
          CALL dstzr(0.0D0,1.0D0,atol,tol)
          IF (.NOT. (qporq)) GO TO 500
          status = 0
          CALL dzror(status,pr,fx,xlo,xhi,qleft,qhi)
          ompr = one - pr
  480     IF (.NOT. (status.EQ.1)) GO TO 490
          CALL cumbin(s,xn,pr,ompr,cum,ccum)
          fx = cum - p
          CALL dzror(status,pr,fx,xlo,xhi,qleft,qhi)
          ompr = one - pr
          GO TO 480

  490     GO TO 530

  500     status = 0
          CALL dzror(status,ompr,fx,xlo,xhi,qleft,qhi)
          pr = one - ompr
  510     IF (.NOT. (status.EQ.1)) GO TO 520
          CALL cumbin(s,xn,pr,ompr,cum,ccum)
          fx = ccum - q
          CALL dzror(status,ompr,fx,xlo,xhi,qleft,qhi)
          pr = one - ompr
          GO TO 510

  520     CONTINUE
  530     IF (.NOT. (status.EQ.-1)) GO TO 560
          IF (.NOT. (qleft)) GO TO 540
          status = 1
          bound = 0.0D0
          GO TO 550

  540     status = 2
          bound = 1.0D0
  550     CONTINUE
  560 END IF

      RETURN

      END
      SUBROUTINE cdfchi(which,p,q,x,df,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND )
C               Cumulative Distribution Function
C               CHI-Square distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the chi-square
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next three argument
C               values is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from X and DF
C               iwhich = 2 : Calculate X from P,Q and DF
C               iwhich = 3 : Calculate DF from P,Q and X
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     DF <--> Degrees of freedom of the
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DF
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C               10 indicates error returned from cumgam.  See
C                  references in cdfgam
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of
C     Mathematical Functions   (1966) is used   to reduce the chisqure
C     distribution to the incomplete distribution.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,df,p,q,x
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,cum,ccum,pq,porq
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumchi
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.3))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 3.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
C
C     X
C
      IF (.NOT. (x.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
C
C     DF
C
      IF (.NOT. (df.LE.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.1) GO TO 190
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 180
      IF (.NOT. (pq.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = 3
      RETURN

  180 CONTINUE
  190 IF (which.EQ.1) GO TO 220
C
C     Select the minimum of P or Q
C
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 200
      porq = p
      GO TO 210

  200 porq = q
  210 CONTINUE
C
C     Calculate ANSWERS
C
  220 IF ((1).EQ. (which)) THEN
C
C     Calculating P and Q
C
          status = 0
          CALL cumchi(x,df,p,q)
          IF (porq.GT.1.5D0) THEN
              status = 10
              RETURN

          END IF

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating X
C
          x = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,x,fx,qleft,qhi)
  230     IF (.NOT. (status.EQ.1)) GO TO 270
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 240
          fx = cum - p
          GO TO 250

  240     fx = ccum - q
  250     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 260
          status = 10
          RETURN

  260     CALL dinvr(status,x,fx,qleft,qhi)
          GO TO 230

  270     IF (.NOT. (status.EQ.-1)) GO TO 300
          IF (.NOT. (qleft)) GO TO 280
          status = 1
          bound = 0.0D0
          GO TO 290

  280     status = 2
          bound = inf
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating DF
C
          df = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,df,fx,qleft,qhi)
  310     IF (.NOT. (status.EQ.1)) GO TO 350
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 320
          fx = cum - p
          GO TO 330

  320     fx = ccum - q
  330     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 340
          status = 10
          RETURN

  340     CALL dinvr(status,df,fx,qleft,qhi)
          GO TO 310

  350     IF (.NOT. (status.EQ.-1)) GO TO 380
          IF (.NOT. (qleft)) GO TO 360
          status = 1
          bound = zero
          GO TO 370

  360     status = 2
          bound = inf
  370     CONTINUE
  380 END IF

      RETURN

      END
      SUBROUTINE cdfchn(which,p,q,x,df,pnonc,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFCHN( WHICH, P, Q, X, DF, PNONC, STATUS, BOUND )
C               Cumulative Distribution Function
C               Non-central Chi-Square
C
C
C                              Function
C
C
C     Calculates any one parameter of the non-central chi-square
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next three argument
C               values is to be calculated from the others.
C               Input range: 1..4
C               iwhich = 1 : Calculate P and Q from X and DF
C               iwhich = 2 : Calculate X from P,DF and PNONC
C               iwhich = 3 : Calculate DF from P,X and PNONC
C               iwhich = 3 : Calculate PNONC from P,X and DF
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the non-central chi-square
C            distribution.
C            Input range: [0, 1-1E-16).
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Q is not used by this subroutine and is only included
C            for similarity with other cdf* routines.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     DF <--> Degrees of freedom of the non-central
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DF
C
C     PNONC <--> Non-centrality parameter of the non-central
C                chi-square distribution.
C                Input range: [0, +infinity).
C                Search range: [0,1E4]
C                    DOUBLE PRECISION PNONC
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.4.25   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to compute the cumulative
C     distribution function.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                            WARNING
C
C     The computation time  required for this  routine is proportional
C     to the noncentrality  parameter  (PNONC).  Very large  values of
C     this parameter can consume immense  computer resources.  This is
C     why the search range is bounded by 10,000.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tent4
      PARAMETER (tent4=1.0D4)
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,one,inf
      PARAMETER (zero=1.0D-300,one=1.0D0-1.0D-16,inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,df,p,q,pnonc,x
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,cum,ccum
      LOGICAL qhi,qleft
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumchn
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C

      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.one))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = one
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.2) GO TO 90
C
C     X
C
      IF (.NOT. (x.LT.0.0D0)) GO TO 80
      bound = 0.0D0
      status = -4
      RETURN

   80 CONTINUE
   90 IF (which.EQ.3) GO TO 110
C
C     DF
C
      IF (.NOT. (df.LE.0.0D0)) GO TO 100
      bound = 0.0D0
      status = -5
      RETURN

  100 CONTINUE
  110 IF (which.EQ.4) GO TO 130
C
C     PNONC
C
      IF (.NOT. (pnonc.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -6
      RETURN

  120 CONTINUE
C
C     Calculate ANSWERS
C
  130 IF ((1).EQ. (which)) THEN
C
C     Calculating P and Q
C
          CALL cumchn(x,df,pnonc,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating X
C
          x = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,x,fx,qleft,qhi)
  140     IF (.NOT. (status.EQ.1)) GO TO 150
          CALL cumchn(x,df,pnonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,x,fx,qleft,qhi)
          GO TO 140

  150     IF (.NOT. (status.EQ.-1)) GO TO 180
          IF (.NOT. (qleft)) GO TO 160
          status = 1
          bound = 0.0D0
          GO TO 170

  160     status = 2
          bound = inf
  170     CONTINUE
  180     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating DF
C
          df = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,df,fx,qleft,qhi)
  190     IF (.NOT. (status.EQ.1)) GO TO 200
          CALL cumchn(x,df,pnonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,df,fx,qleft,qhi)
          GO TO 190

  200     IF (.NOT. (status.EQ.-1)) GO TO 230
          IF (.NOT. (qleft)) GO TO 210
          status = 1
          bound = zero
          GO TO 220

  210     status = 2
          bound = inf
  220     CONTINUE
  230     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Calculating PNONC
C
          pnonc = 5.0D0
          CALL dstinv(0.0D0,tent4,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,pnonc,fx,qleft,qhi)
  240     IF (.NOT. (status.EQ.1)) GO TO 250
          CALL cumchn(x,df,pnonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,pnonc,fx,qleft,qhi)
          GO TO 240

  250     IF (.NOT. (status.EQ.-1)) GO TO 280
          IF (.NOT. (qleft)) GO TO 260
          status = 1
          bound = zero
          GO TO 270

  260     status = 2
          bound = tent4
  270     CONTINUE
  280 END IF

      RETURN

      END
      SUBROUTINE cdff(which,p,q,f,dfn,dfd,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFF( WHICH, P, Q, F, DFN, DFD, STATUS, BOUND )
C               Cumulative Distribution Function
C               F distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the F distribution
C     given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from F,DFN and DFD
C               iwhich = 2 : Calculate F from P,Q,DFN and DFD
C               iwhich = 3 : Calculate DFN from P,Q,F and DFD
C               iwhich = 4 : Calculate DFD from P,Q,F and DFN
C                    INTEGER WHICH
C
C       P <--> The integral from 0 to F of the f-density.
C              Input range: [0,1].
C                    DOUBLE PRECISION P
C
C       Q <--> 1-P.
C              Input range: (0, 1].
C              P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C       F <--> Upper limit of integration of the f-density.
C              Input range: [0, +infinity).
C              Search range: [0,1E300]
C                    DOUBLE PRECISION F
C
C     DFN < --> Degrees of freedom of the numerator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFN
C
C     DFD < --> Degrees of freedom of the denominator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFD
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to reduce the computation
C     of the  cumulative  distribution function for the  F  variate to
C     that of an incomplete beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C                              WARNING
C
C     The value of the  cumulative  F distribution is  not necessarily
C     monotone in  either degrees of freedom.  There  thus may  be two
C     values  that  provide a given CDF  value.   This routine assumes
C     monotonicity and will find an arbitrary one of the two values.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,dfd,dfn,f,p,q
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION pq,fx,cum,ccum
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumf
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
C
C     F
C
      IF (.NOT. (f.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
C
C     DFN
C
      IF (.NOT. (dfn.LE.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.4) GO TO 170
C
C     DFD
C
      IF (.NOT. (dfd.LE.0.0D0)) GO TO 160
      bound = 0.0D0
      status = -6
      RETURN

  160 CONTINUE
  170 IF (which.EQ.1) GO TO 210
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 200
      IF (.NOT. (pq.LT.0.0D0)) GO TO 180
      bound = 0.0D0
      GO TO 190

  180 bound = 1.0D0
  190 status = 3
      RETURN

  200 CONTINUE
  210 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          CALL cumf(f,dfn,dfd,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating F
C
          f = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,f,fx,qleft,qhi)
  220     IF (.NOT. (status.EQ.1)) GO TO 250
          CALL cumf(f,dfn,dfd,cum,ccum)
          IF (.NOT. (qporq)) GO TO 230
          fx = cum - p
          GO TO 240

  230     fx = ccum - q
  240     CALL dinvr(status,f,fx,qleft,qhi)
          GO TO 220

  250     IF (.NOT. (status.EQ.-1)) GO TO 280
          IF (.NOT. (qleft)) GO TO 260
          status = 1
          bound = 0.0D0
          GO TO 270

  260     status = 2
          bound = inf
  270     CONTINUE
  280     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating DFN
C
          dfn = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,dfn,fx,qleft,qhi)
  290     IF (.NOT. (status.EQ.1)) GO TO 320
          CALL cumf(f,dfn,dfd,cum,ccum)
          IF (.NOT. (qporq)) GO TO 300
          fx = cum - p
          GO TO 310

  300     fx = ccum - q
  310     CALL dinvr(status,dfn,fx,qleft,qhi)
          GO TO 290

  320     IF (.NOT. (status.EQ.-1)) GO TO 350
          IF (.NOT. (qleft)) GO TO 330
          status = 1
          bound = zero
          GO TO 340

  330     status = 2
          bound = inf
  340     CONTINUE
  350     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Calculating DFD
C
          dfd = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,dfd,fx,qleft,qhi)
  360     IF (.NOT. (status.EQ.1)) GO TO 390
          CALL cumf(f,dfn,dfd,cum,ccum)
          IF (.NOT. (qporq)) GO TO 370
          fx = cum - p
          GO TO 380

  370     fx = ccum - q
  380     CALL dinvr(status,dfd,fx,qleft,qhi)
          GO TO 360

  390     IF (.NOT. (status.EQ.-1)) GO TO 420
          IF (.NOT. (qleft)) GO TO 400
          status = 1
          bound = zero
          GO TO 410

  400     status = 2
          bound = inf
  410     CONTINUE
  420 END IF

      RETURN

      END
      SUBROUTINE cdffnc(which,p,q,f,dfn,dfd,phonc,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFFNC( WHICH, P, Q, F, DFN, DFD, PNONC, STATUS, BOUND
C               Cumulative Distribution Function
C               Non-central F distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the Non-central F
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next five argument
C               values is to be calculated from the others.
C               Legal range: 1..5
C               iwhich = 1 : Calculate P and Q from F,DFN,DFD and PNONC
C               iwhich = 2 : Calculate F from P,Q,DFN,DFD and PNONC
C               iwhich = 3 : Calculate DFN from P,Q,F,DFD and PNONC
C               iwhich = 4 : Calculate DFD from P,Q,F,DFN and PNONC
C               iwhich = 5 : Calculate PNONC from P,Q,F,DFN and DFD
C                    INTEGER WHICH
C
C       P <--> The integral from 0 to F of the non-central f-density.
C              Input range: [0,1-1E-16).
C                    DOUBLE PRECISION P
C
C       Q <--> 1-P.
C            Q is not used by this subroutine and is only included
C            for similarity with other cdf* routines.
C                    DOUBLE PRECISION Q
C
C       F <--> Upper limit of integration of the non-central f-density.
C              Input range: [0, +infinity).
C              Search range: [0,1E300]
C                    DOUBLE PRECISION F
C
C     DFN < --> Degrees of freedom of the numerator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFN
C
C     DFD < --> Degrees of freedom of the denominator sum of squares.
C               Must be in range: (0, +infinity).
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFD
C
C     PNONC <-> The non-centrality parameter
C               Input range: [0,infinity)
C               Search range: [0,1E4]
C                    DOUBLE PRECISION PHONC
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.6.20   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to compute the cumulative
C     distribution function.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C                            WARNING
C
C     The computation time  required for this  routine is proportional
C     to the noncentrality  parameter  (PNONC).  Very large  values of
C     this parameter can consume immense  computer resources.  This is
C     why the search range is bounded by 10,000.
C
C                              WARNING
C
C     The  value  of the  cumulative  noncentral F distribution is not
C     necessarily monotone in either degrees  of freedom.  There  thus
C     may be two values that provide a given  CDF value.  This routine
C     assumes monotonicity  and will find  an arbitrary one of the two
C     values.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tent4
      PARAMETER (tent4=1.0D4)
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,one,inf
      PARAMETER (zero=1.0D-300,one=1.0D0-1.0D-16,inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,dfd,dfn,f,p,q,phonc
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,cum,ccum
      LOGICAL qhi,qleft
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumfnc
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.5))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 5.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.one))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = one
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.2) GO TO 90
C
C     F
C
      IF (.NOT. (f.LT.0.0D0)) GO TO 80
      bound = 0.0D0
      status = -4
      RETURN

   80 CONTINUE
   90 IF (which.EQ.3) GO TO 110
C
C     DFN
C
      IF (.NOT. (dfn.LE.0.0D0)) GO TO 100
      bound = 0.0D0
      status = -5
      RETURN

  100 CONTINUE
  110 IF (which.EQ.4) GO TO 130
C
C     DFD
C
      IF (.NOT. (dfd.LE.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -6
      RETURN

  120 CONTINUE
  130 IF (which.EQ.5) GO TO 150
C
C     PHONC
C
      IF (.NOT. (phonc.LT.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -7
      RETURN

  140 CONTINUE
C
C     Calculate ANSWERS
C
  150 IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          CALL cumfnc(f,dfn,dfd,phonc,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating F
C
          f = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,f,fx,qleft,qhi)
  160     IF (.NOT. (status.EQ.1)) GO TO 170
          CALL cumfnc(f,dfn,dfd,phonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,f,fx,qleft,qhi)
          GO TO 160

  170     IF (.NOT. (status.EQ.-1)) GO TO 200
          IF (.NOT. (qleft)) GO TO 180
          status = 1
          bound = 0.0D0
          GO TO 190

  180     status = 2
          bound = inf
  190     CONTINUE
  200     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating DFN
C
          dfn = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,dfn,fx,qleft,qhi)
  210     IF (.NOT. (status.EQ.1)) GO TO 220
          CALL cumfnc(f,dfn,dfd,phonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,dfn,fx,qleft,qhi)
          GO TO 210

  220     IF (.NOT. (status.EQ.-1)) GO TO 250
          IF (.NOT. (qleft)) GO TO 230
          status = 1
          bound = zero
          GO TO 240

  230     status = 2
          bound = inf
  240     CONTINUE
  250     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Calculating DFD
C
          dfd = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,dfd,fx,qleft,qhi)
  260     IF (.NOT. (status.EQ.1)) GO TO 270
          CALL cumfnc(f,dfn,dfd,phonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,dfd,fx,qleft,qhi)
          GO TO 260

  270     IF (.NOT. (status.EQ.-1)) GO TO 300
          IF (.NOT. (qleft)) GO TO 280
          status = 1
          bound = zero
          GO TO 290

  280     status = 2
          bound = inf
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((5).EQ. (which)) THEN
C
C     Calculating PHONC
C
          phonc = 5.0D0
          CALL dstinv(0.0D0,tent4,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,phonc,fx,qleft,qhi)
  310     IF (.NOT. (status.EQ.1)) GO TO 320
          CALL cumfnc(f,dfn,dfd,phonc,cum,ccum)
          fx = cum - p
          CALL dinvr(status,phonc,fx,qleft,qhi)
          GO TO 310

  320     IF (.NOT. (status.EQ.-1)) GO TO 350
          IF (.NOT. (qleft)) GO TO 330
          status = 1
          bound = 0.0D0
          GO TO 340

  330     status = 2
          bound = tent4
  340     CONTINUE
  350 END IF

      RETURN

      END
      SUBROUTINE cdfgam(which,p,q,x,shape,scale,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFGAM( WHICH, P, Q, X, SHAPE, SCALE, STATUS, BOUND )
C               Cumulative Distribution Function
C                         GAMma Distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the gamma
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,SHAPE and SCALE
C               iwhich = 2 : Calculate X from P,Q,SHAPE and SCALE
C               iwhich = 3 : Calculate SHAPE from P,Q,X and SCALE
C               iwhich = 4 : Calculate SCALE from P,Q,X and SHAPE
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the gamma density.
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C
C     X <--> The upper limit of integration of the gamma density.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     SHAPE <--> The shape parameter of the gamma density.
C                Input range: (0, +infinity).
C                Search range: [1E-300,1E300]
C                  DOUBLE PRECISION SHAPE
C
C
C     SCALE <--> The scale parameter of the gamma density.
C                Input range: (0, +infinity).
C                Search range: (1E-300,1E300]
C                   DOUBLE PRECISION SCALE
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                10 if the gamma or inverse gamma routine cannot
C                   compute the answer.  Usually happens only for
C                   X and SHAPE very large (gt 1E10 or more)
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Cumulative distribution function (P) is calculated directly by
C     the code associated with:
C
C     DiDinato, A. R. and Morris, A. H. Computation of the  incomplete
C     gamma function  ratios  and their  inverse.   ACM  Trans.  Math.
C     Softw. 12 (1986), 377-393.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                              Note
C
C
C
C     The gamma density is proportional to
C       T**(SHAPE - 1) * EXP(- SCALE * T)
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,p,q,scale,shape,x
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION xx
      DOUBLE PRECISION fx,xscale,cum,ccum,pq,porq
      INTEGER ierr
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL gaminv,dinvr,dstinv,cumgam
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0d0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
C
C     X
C
      IF (.NOT. (x.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
C
C     SHAPE
C
      IF (.NOT. (shape.LE.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.4) GO TO 170
C
C     SCALE
C
      IF (.NOT. (scale.LE.0.0D0)) GO TO 160
      bound = 0.0D0
      status = -6
      RETURN

  160 CONTINUE
  170 IF (which.EQ.1) GO TO 210
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 200
      IF (.NOT. (pq.LT.0.0D0)) GO TO 180
      bound = 0.0D0
      GO TO 190

  180 bound = 1.0D0
  190 status = 3
      RETURN

  200 CONTINUE
  210 IF (which.EQ.1) GO TO 240
C
C     Select the minimum of P or Q
C
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 220
      porq = p
      GO TO 230

  220 porq = q
  230 CONTINUE
C
C     Calculate ANSWERS
C
  240 IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          status = 0
          xscale = x*scale
          CALL cumgam(xscale,shape,p,q)
          IF (porq.GT.1.5D0) status = 10

      ELSE IF ((2).EQ. (which)) THEN
C
C     Computing X
C
          CALL gaminv(shape,xx,-1.0D0,p,q,ierr)
          IF (ierr.LT.0.0D0) THEN
              status = 10
              RETURN

          ELSE
              x = xx/scale
              status = 0
          END IF

      ELSE IF ((3).EQ. (which)) THEN
C
C     Computing SHAPE
C
          shape = 5.0D0
          xscale = x*scale
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,shape,fx,qleft,qhi)
  250     IF (.NOT. (status.EQ.1)) GO TO 290
          CALL cumgam(xscale,shape,cum,ccum)
          IF (.NOT. (qporq)) GO TO 260
          fx = cum - p
          GO TO 270

  260     fx = ccum - q
  270     IF (.NOT. ((qporq.AND. (cum.GT.1.5D0)).OR.
     +        ((.NOT.qporq).AND. (ccum.GT.1.5D0)))) GO TO 280
          status = 10
          RETURN

  280     CALL dinvr(status,shape,fx,qleft,qhi)
          GO TO 250

  290     IF (.NOT. (status.EQ.-1)) GO TO 320
          IF (.NOT. (qleft)) GO TO 300
          status = 1
          bound = zero
          GO TO 310

  300     status = 2
          bound = inf
  310     CONTINUE
  320     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Computing SCALE
C
          CALL gaminv(shape,xx,-1.0D0,p,q,ierr)
          IF (ierr.LT.0.0D0) THEN
              status = 10
              RETURN

          ELSE
              scale = xx/x
              status = 0
          END IF

      END IF

      RETURN

      END
      SUBROUTINE cdfnbn(which,p,q,s,xn,pr,ompr,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFNBN ( WHICH, P, S, XN, PR, STATUS, BOUND )
C               Cumulative Distribution Function
C               Negative BiNomial distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the negative binomial
C     distribution given values for the others.
C
C     The  cumulative  negative   binomial  distribution  returns  the
C     probability that there  will be  F or fewer failures before  the
C     XNth success in binomial trials each of which has probability of
C     success PR.
C
C     The individual term of the negative binomial is the probability of
C     S failures before XN successes and is
C          Choose( S, XN+S-1 ) * PR^(XN) * (1-PR)^S
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
C               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
C               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
C               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN
C                    INTEGER WHICH
C
C     P <--> The cumulation from 0 to S of the  negative
C            binomial distribution.
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     S <--> The upper limit of cumulation of the binomial distribution.
C            There are F or fewer failures before the XNth success.
C            Input range: [0, +infinity).
C            Search range: [0, 1E300]
C                    DOUBLE PRECISION S
C
C     XN  <--> The number of successes.
C              Input range: [0, +infinity).
C              Search range: [0, 1E300]
C                    DOUBLE PRECISION XN
C
C     PR  <--> The probability of success in each binomial trial.
C              Input range: [0,1].
C              Search range: [0,1].
C                    DOUBLE PRECISION PR
C
C     OMPR  <--> 1-PR
C              Input range: [0,1].
C              Search range: [0,1]
C              PR + OMPR = 1.0
C                    DOUBLE PRECISION OMPR
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if PR + OMPR .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.5.26   of   Abramowitz  and  Stegun,  Handbook   of
C     Mathematical Functions (1966) is used  to  reduce calculation of
C     the cumulative distribution  function to that of  an  incomplete
C     beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION inf
      PARAMETER (inf=1.0D300)
      DOUBLE PRECISION one
      PARAMETER (one=1.0D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,p,q,pr,ompr,s,xn
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,xhi,xlo,pq,prompr,cum,ccum
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,dstzr,dzror,cumnbn
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
C
C     S
C
      IF (.NOT. (s.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
C
C     XN
C
      IF (.NOT. (xn.LT.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.4) GO TO 190
C
C     PR
C
      IF (.NOT. ((pr.LT.0.0D0).OR. (pr.GT.1.0D0))) GO TO 180
      IF (.NOT. (pr.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = -6
      RETURN

  180 CONTINUE
  190 IF (which.EQ.4) GO TO 230
C
C     OMPR
C
      IF (.NOT. ((ompr.LT.0.0D0).OR. (ompr.GT.1.0D0))) GO TO 220
      IF (.NOT. (ompr.LT.0.0D0)) GO TO 200
      bound = 0.0D0
      GO TO 210

  200 bound = 1.0D0
  210 status = -7
      RETURN

  220 CONTINUE
  230 IF (which.EQ.1) GO TO 270
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 260
      IF (.NOT. (pq.LT.0.0D0)) GO TO 240
      bound = 0.0D0
      GO TO 250

  240 bound = 1.0D0
  250 status = 3
      RETURN

  260 CONTINUE
  270 IF (which.EQ.4) GO TO 310
C
C     PR + OMPR
C
      prompr = pr + ompr
      IF (.NOT. (abs(((prompr)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 300
      IF (.NOT. (prompr.LT.0.0D0)) GO TO 280
      bound = 0.0D0
      GO TO 290

  280 bound = 1.0D0
  290 status = 4
      RETURN

  300 CONTINUE
  310 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          CALL cumnbn(s,xn,pr,ompr,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating S
C
          s = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,s,fx,qleft,qhi)
  320     IF (.NOT. (status.EQ.1)) GO TO 350
          CALL cumnbn(s,xn,pr,ompr,cum,ccum)
          IF (.NOT. (qporq)) GO TO 330
          fx = cum - p
          GO TO 340

  330     fx = ccum - q
  340     CALL dinvr(status,s,fx,qleft,qhi)
          GO TO 320

  350     IF (.NOT. (status.EQ.-1)) GO TO 380
          IF (.NOT. (qleft)) GO TO 360
          status = 1
          bound = 0.0D0
          GO TO 370

  360     status = 2
          bound = inf
  370     CONTINUE
  380     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating XN
C
          xn = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,xn,fx,qleft,qhi)
  390     IF (.NOT. (status.EQ.1)) GO TO 420
          CALL cumnbn(s,xn,pr,ompr,cum,ccum)
          IF (.NOT. (qporq)) GO TO 400
          fx = cum - p
          GO TO 410

  400     fx = ccum - q
  410     CALL dinvr(status,xn,fx,qleft,qhi)
          GO TO 390

  420     IF (.NOT. (status.EQ.-1)) GO TO 450
          IF (.NOT. (qleft)) GO TO 430
          status = 1
          bound = 0.0D0
          GO TO 440

  430     status = 2
          bound = inf
  440     CONTINUE
  450     CONTINUE

      ELSE IF ((4).EQ. (which)) THEN
C
C     Calculating PR and OMPR
C
          CALL dstzr(0.0D0,1.0D0,atol,tol)
          IF (.NOT. (qporq)) GO TO 480
          status = 0
          CALL dzror(status,pr,fx,xlo,xhi,qleft,qhi)
          ompr = one - pr
  460     IF (.NOT. (status.EQ.1)) GO TO 470
          CALL cumnbn(s,xn,pr,ompr,cum,ccum)
          fx = cum - p
          CALL dzror(status,pr,fx,xlo,xhi,qleft,qhi)
          ompr = one - pr
          GO TO 460

  470     GO TO 510

  480     status = 0
          CALL dzror(status,ompr,fx,xlo,xhi,qleft,qhi)
          pr = one - ompr
  490     IF (.NOT. (status.EQ.1)) GO TO 500
          CALL cumnbn(s,xn,pr,ompr,cum,ccum)
          fx = ccum - q
          CALL dzror(status,ompr,fx,xlo,xhi,qleft,qhi)
          pr = one - ompr
          GO TO 490

  500     CONTINUE
  510     IF (.NOT. (status.EQ.-1)) GO TO 540
          IF (.NOT. (qleft)) GO TO 520
          status = 1
          bound = 0.0D0
          GO TO 530

  520     status = 2
          bound = 1.0D0
  530     CONTINUE
  540 END IF

      RETURN

      END
      SUBROUTINE cdfnor(which,p,q,x,mean,sd,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFNOR( WHICH, P, Q, X, MEAN, SD, STATUS, BOUND )
C               Cumulative Distribution Function
C               NORmal distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the normal
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH  --> Integer indicating  which of the  next  parameter
C     values is to be calculated using values  of the others.
C     Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,MEAN and SD
C               iwhich = 2 : Calculate X from P,Q,MEAN and SD
C               iwhich = 3 : Calculate MEAN from P,Q,X and SD
C               iwhich = 4 : Calculate SD from P,Q,X and MEAN
C                    INTEGER WHICH
C
C     P <--> The integral from -infinity to X of the normal density.
C            Input range: (0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X < --> Upper limit of integration of the normal-density.
C             Input range: ( -infinity, +infinity)
C                    DOUBLE PRECISION X
C
C     MEAN <--> The mean of the normal density.
C               Input range: (-infinity, +infinity)
C                    DOUBLE PRECISION MEAN
C
C     SD <--> Standard Deviation of the normal density.
C             Input range: (0, +infinity).
C                    DOUBLE PRECISION SD
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C
C
C     A slightly modified version of ANORM from
C
C     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
C     Package of Special Function Routines and Test Drivers"
C     acm Transactions on Mathematical Software. 19, 22-32.
C
C     is used to calulate the  cumulative standard normal distribution.
C
C     The rational functions from pages  90-95  of Kennedy and Gentle,
C     Statistical  Computing,  Marcel  Dekker, NY,  1980 are  used  as
C     starting values to Newton's Iterations which compute the inverse
C     standard normal.  Therefore no  searches  are necessary for  any
C     parameter.
C
C     For X < -15, the asymptotic expansion for the normal is used  as
C     the starting value in finding the inverse standard normal.
C     This is formula 26.2.12 of Abramowitz and Stegun.
C
C
C                              Note
C
C
C      The normal density is proportional to
C      exp( - 0.5 * (( X - MEAN)/SD)**2)
C
C
C**********************************************************************
C     .. Parameters ..
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,mean,p,q,sd,x
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION z,pq
C     ..
C     .. External Functions ..

      DOUBLE PRECISION dinvnr,spmpar
      EXTERNAL dinvnr,spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL cumnor
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      status = 0
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LE.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LE.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.1) GO TO 150
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 140
      IF (.NOT. (pq.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      GO TO 130

  120 bound = 1.0D0
  130 status = 3
      RETURN

  140 CONTINUE
  150 IF (which.EQ.4) GO TO 170
C
C     SD
C
      IF (.NOT. (sd.LE.0.0D0)) GO TO 160
      bound = 0.0D0
      status = -6
      RETURN

  160 CONTINUE
C
C     Calculate ANSWERS
C
  170 IF ((1).EQ. (which)) THEN
C
C     Computing P
C
          z = (x-mean)/sd
          CALL cumnor(z,p,q)

      ELSE IF ((2).EQ. (which)) THEN
C
C     Computing X
C
          z = dinvnr(p,q)
          x = sd*z + mean

      ELSE IF ((3).EQ. (which)) THEN
C
C     Computing the MEAN
C
          z = dinvnr(p,q)
          mean = x - sd*z

      ELSE IF ((4).EQ. (which)) THEN
C
C     Computing SD
C
          z = dinvnr(p,q)
          sd = (x-mean)/z
      END IF

      RETURN

      END
      SUBROUTINE cdfpoi(which,p,q,s,xlam,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFPOI( WHICH, P, Q, S, XLAM, STATUS, BOUND )
C               Cumulative Distribution Function
C               POIsson distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the Poisson
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which  argument
C               value is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from S and XLAM
C               iwhich = 2 : Calculate A from P,Q and XLAM
C               iwhich = 3 : Calculate XLAM from P,Q and S
C                    INTEGER WHICH
C
C        P <--> The cumulation from 0 to S of the poisson density.
C               Input range: [0,1].
C                    DOUBLE PRECISION P
C
C        Q <--> 1-P.
C               Input range: (0, 1].
C               P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C        S <--> Upper limit of cumulation of the Poisson.
C               Input range: [0, +infinity).
C               Search range: [0,1E300]
C                    DOUBLE PRECISION S
C
C     XLAM <--> Mean of the Poisson distribution.
C               Input range: [0, +infinity).
C               Search range: [0,1E300]
C                    DOUBLE PRECISION XLAM
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.4.21  of   Abramowitz  and   Stegun,   Handbook  of
C     Mathematical Functions (1966) is used  to reduce the computation
C     of  the cumulative distribution function to that  of computing a
C     chi-square, hence an incomplete gamma function.
C
C     Cumulative  distribution function  (P) is  calculated  directly.
C     Computation of other parameters involve a seach for a value that
C     produces  the desired value of  P.   The  search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION inf
      PARAMETER (inf=1.0D300)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,p,q,s,xlam
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,cum,ccum,pq
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumpoi
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.3))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 3.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
C
C     S
C
      IF (.NOT. (s.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
C
C     XLAM
C
      IF (.NOT. (xlam.LT.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.1) GO TO 190
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 180
      IF (.NOT. (pq.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = 3
      RETURN

  180 CONTINUE
  190 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Calculating P
C
          CALL cumpoi(s,xlam,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Calculating S
C
          s = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,s,fx,qleft,qhi)
  200     IF (.NOT. (status.EQ.1)) GO TO 230
          CALL cumpoi(s,xlam,cum,ccum)
          IF (.NOT. (qporq)) GO TO 210
          fx = cum - p
          GO TO 220

  210     fx = ccum - q
  220     CALL dinvr(status,s,fx,qleft,qhi)
          GO TO 200

  230     IF (.NOT. (status.EQ.-1)) GO TO 260
          IF (.NOT. (qleft)) GO TO 240
          status = 1
          bound = 0.0D0
          GO TO 250

  240     status = 2
          bound = inf
  250     CONTINUE
  260     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Calculating XLAM
C
          xlam = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,xlam,fx,qleft,qhi)
  270     IF (.NOT. (status.EQ.1)) GO TO 300
          CALL cumpoi(s,xlam,cum,ccum)
          IF (.NOT. (qporq)) GO TO 280
          fx = cum - p
          GO TO 290

  280     fx = ccum - q
  290     CALL dinvr(status,xlam,fx,qleft,qhi)
          GO TO 270

  300     IF (.NOT. (status.EQ.-1)) GO TO 330
          IF (.NOT. (qleft)) GO TO 310
          status = 1
          bound = 0.0D0
          GO TO 320

  310     status = 2
          bound = inf
  320     CONTINUE
  330 END IF

      RETURN

      END
      SUBROUTINE cdft(which,p,q,t,df,status,bound)
C**********************************************************************
C
C      SUBROUTINE CDFT( WHICH, P, Q, T, DF, STATUS, BOUND )
C               Cumulative Distribution Function
C                         T distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the t distribution given
C     values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which  argument
C               values is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from T and DF
C               iwhich = 2 : Calculate T from P,Q and DF
C               iwhich = 3 : Calculate DF from P,Q and T
C                    INTEGER WHICH
C
C        P <--> The integral from -infinity to t of the t-density.
C              Input range: (0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C        T <--> Upper limit of integration of the t-density.
C               Input range: ( -infinity, +infinity).
C               Search range: [ -1E300, 1E300 ]
C                    DOUBLE PRECISION T
C
C        DF <--> Degrees of freedom of the t-distribution.
C                Input range: (0 , +infinity).
C                Search range: [1e-300, 1E10]
C                    DOUBLE PRECISION DF
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.5.27  of   Abramowitz   and  Stegun,   Handbook   of
C     Mathematical Functions  (1966) is used to reduce the computation
C     of the cumulative distribution function to that of an incomplete
C     beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-300,inf=1.0D300)
      DOUBLE PRECISION maxdf
      PARAMETER (maxdf=1.0d10)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION bound,df,p,q,t
      INTEGER status,which
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION fx,cum,ccum,pq
      LOGICAL qhi,qleft,qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar,dt1
      EXTERNAL spmpar,dt1
C     ..
C     .. External Subroutines ..
      EXTERNAL dinvr,dstinv,cumt
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((which.LT.1).OR. (which.GT.3))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 3.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
C
C     P
C
      IF (.NOT. ((p.LE.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LE.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
C
C     Q
C
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.3) GO TO 130
C
C     DF
C
      IF (.NOT. (df.LE.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -5
      RETURN

  120 CONTINUE
  130 IF (which.EQ.1) GO TO 170
C
C     P + Q
C
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 160
      IF (.NOT. (pq.LT.0.0D0)) GO TO 140
      bound = 0.0D0
      GO TO 150

  140 bound = 1.0D0
  150 status = 3
      RETURN

  160 CONTINUE
  170 IF (.NOT. (which.EQ.1)) qporq = p .LE. q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (which)) THEN
C
C     Computing P and Q
C
          CALL cumt(t,df,p,q)
          status = 0

      ELSE IF ((2).EQ. (which)) THEN
C
C     Computing T
C
C     .. Get initial approximation for T
C
          t = dt1(p,q,df)
          CALL dstinv(-inf,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,t,fx,qleft,qhi)
  180     IF (.NOT. (status.EQ.1)) GO TO 210
          CALL cumt(t,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 190
          fx = cum - p
          GO TO 200

  190     fx = ccum - q
  200     CALL dinvr(status,t,fx,qleft,qhi)
          GO TO 180

  210     IF (.NOT. (status.EQ.-1)) GO TO 240
          IF (.NOT. (qleft)) GO TO 220
          status = 1
          bound = -inf
          GO TO 230

  220     status = 2
          bound = inf
  230     CONTINUE
  240     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
C
C     Computing DF
C
          df = 5.0D0
          CALL dstinv(zero,maxdf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,df,fx,qleft,qhi)
  250     IF (.NOT. (status.EQ.1)) GO TO 280
          CALL cumt(t,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 260
          fx = cum - p
          GO TO 270

  260     fx = ccum - q
  270     CALL dinvr(status,df,fx,qleft,qhi)
          GO TO 250

  280     IF (.NOT. (status.EQ.-1)) GO TO 310
          IF (.NOT. (qleft)) GO TO 290
          status = 1
          bound = zero
          GO TO 300

  290     status = 2
          bound = maxdf
  300     CONTINUE
  310 END IF

      RETURN

      END
      SUBROUTINE cumbet(x,y,a,b,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMBET(X,Y,A,B,CUM,CCUM)
C          Double precision cUMulative incomplete BETa distribution
C
C
C                              Function
C
C
C     Calculates the cdf to X of the incomplete beta distribution
C     with parameters a and b.  This is the integral from 0 to x
C     of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
C
C
C                              Arguments
C
C
C     X --> Upper limit of integration.
C                                        X is DOUBLE PRECISION
C
C     Y --> 1 - X.
C                                        Y is DOUBLE PRECISION
C
C     A --> First parameter of the beta distribution.
C                                        A is DOUBLE PRECISION
C
C     B --> Second parameter of the beta distribution.
C                                        B is DOUBLE PRECISION
C
C     CUM <-- Cumulative incomplete beta distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative incomplete beta distribution.
C                                        CCUM is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Calls the routine BRATIO.
C
C                                   References
C
C     Didonato, Armido R. and Morris, Alfred H. Jr. (1992) Algorithim
C     708 Significant Digit Computation of the Incomplete Beta Function
C     Ratios. ACM ToMS, Vol.18, No. 3, Sept. 1992, 360-373.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION x,y,a,b,cum,ccum
C     ..
C     .. Local Scalars ..
      INTEGER ierr
C     ..
C     .. External Routines ..
      EXTERNAL bratio
C     ..
C     .. Executable Statements ..
      IF (.NOT. (x.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 IF (.NOT. (y.LE.0.0D0)) GO TO 20
      cum = 1.0D0
      ccum = 0.0D0
      RETURN

   20 CALL bratio(a,b,x,y,cum,ccum,ierr)

C     Call bratio routine


      RETURN

      END
      SUBROUTINE cumbin(s,xn,pr,ompr,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMBIN(S,XN,PBIN,OMPR,CUM,CCUM)
C                    CUmulative BINomial distribution
C
C
C                              Function
C
C
C     Returns the probability   of 0  to  S  successes in  XN   binomial
C     trials, each of which has a probability of success, PBIN.
C
C
C                              Arguments
C
C
C     S --> The upper limit of cumulation of the binomial distribution.
C                                                  S is DOUBLE PRECISION
C
C     XN --> The number of binomial trials.
C                                                  XN is DOUBLE PRECISIO
C
C     PBIN --> The probability of success in each binomial trial.
C                                                  PBIN is DOUBLE PRECIS
C
C     OMPR --> 1 - PBIN
C                                                  OMPR is DOUBLE PRECIS
C
C     CUM <-- Cumulative binomial distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative binomial distribution.
C                                                  CCUM is DOUBLE PRECIS

C
C
C                              Method
C
C
C     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  binomial
C     distribution  to  the  cumulative    beta distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION pr,ompr,s,xn,cum,ccum
C     ..
C     .. External Subroutines ..
      EXTERNAL cumbet
C     ..
C     .. Executable Statements ..
      IF (.NOT. (s.LT.xn)) GO TO 10
      CALL cumbet(pr,ompr,s+1.0D0,xn-s,ccum,cum)
      GO TO 20

   10 cum = 1.0D0
      ccum = 0.0D0
   20 RETURN

      END
      SUBROUTINE cumchi(x,df,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE FUNCTION CUMCHI(X,DF,CUM,CCUM)
C             CUMulative of the CHi-square distribution
C
C
C                              Function
C
C
C     Calculates the cumulative chi-square distribution.
C
C
C                              Arguments
C
C
C     X       --> Upper limit of integration of the
C                 chi-square distribution.
C                                                 X is DOUBLE PRECISION
C
C     DF      --> Degrees of freedom of the
C                 chi-square distribution.
C                                                 DF is DOUBLE PRECISION
C
C     CUM <-- Cumulative chi-square distribution.
C                                                 CUM is DOUBLE PRECISIO
C
C     CCUM <-- Compliment of Cumulative chi-square distribution.
C                                                 CCUM is DOUBLE PRECISI
C
C
C                              Method
C
C
C     Calls incomplete gamma function (CUMGAM)
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION df,x,cum,ccum
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,xx
C     ..
C     .. External Subroutines ..
      EXTERNAL cumgam
C     ..
C     .. Executable Statements ..
      a = df*0.5D0
      xx = x*0.5D0
      CALL cumgam(xx,a,cum,ccum)
      RETURN

      END
      SUBROUTINE cumchn(x,df,pnonc,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMCHN(X,DF,PNONC,CUM,CCUM)
C             CUMulative of the Non-central CHi-square distribution
C
C
C                              Function
C
C
C     Calculates     the       cumulative      non-central    chi-square
C     distribution, i.e.,  the probability   that  a   random   variable
C     which    follows  the  non-central chi-square  distribution,  with
C     non-centrality  parameter    PNONC  and   continuous  degrees   of
C     freedom DF, is less than or equal to X.
C
C
C                              Arguments
C
C
C     X       --> Upper limit of integration of the non-central
C                 chi-square distribution.
C                                                 X is DOUBLE PRECISION
C
C     DF      --> Degrees of freedom of the non-central
C                 chi-square distribution.
C                                                 DF is DOUBLE PRECISION
C
C     PNONC   --> Non-centrality parameter of the non-central
C                 chi-square distribution.
C                                                 PNONC is DOUBLE PRECIS
C
C     CUM <-- Cumulative non-central chi-square distribution.
C                                                 CUM is DOUBLE PRECISIO
C
C     CCUM <-- Compliment of Cumulative non-central chi-square distribut
C                                                 CCUM is DOUBLE PRECISI

C
C
C                              Method
C
C
C     Uses  formula  26.4.25   of  Abramowitz  and  Stegun, Handbook  of
C     Mathematical    Functions,  US   NBS   (1966)    to calculate  the
C     non-central chi-square.
C
C
C                              Variables
C
C
C     EPS     --- Convergence criterion.  The sum stops when a
C                 term is less than EPS*SUM.
C                                                 EPS is DOUBLE PRECISIO
C
C     NTIRED  --- Maximum number of terms to be evaluated
C                 in each sum.
C                                                 NTIRED is INTEGER
C
C     QCONV   --- .TRUE. if convergence achieved -
C                 i.e., program did not stop on NTIRED criterion.
C                                                 QCONV is LOGICAL
C
C     CCUM <-- Compliment of Cumulative non-central
C              chi-square distribution.
C                                                 CCUM is DOUBLE PRECISI
C
C**********************************************************************
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION df,pnonc,x,cum,ccum
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION adj,centaj,centwt,chid2,dfd2,eps,lcntaj,lcntwt,
     +                 lfact,pcent,pterm,sum,sumadj,term,wt,xnonc,xx
      INTEGER i,icent,iterb,iterf,ntired
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alngam
      EXTERNAL alngam
C     ..
C     .. External Subroutines ..
      EXTERNAL cumchi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC log,exp,dble,int
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION dg
      LOGICAL qsmall,qtired
C     ..
C     .. Data statements ..
      DATA ntired,eps/1000,1.0D-5/
C     ..
C     .. Statement Function definitions ..
      qtired(i) = i .GT. ntired
      qsmall(xx) = sum .LT. 1.0D-20 .OR. xx .LT. eps*sum
      dg(i) = df + 2.0D0*dble(i)
C     ..
C     .. Executable Statements ..
C
      IF (.NOT. (x.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 IF (.NOT. (pnonc.LE.1.0D-10)) GO TO 20
C
C
C     When non-centrality parameter is (essentially) zero,
C     use cumulative chi-square distribution
C
C
      CALL cumchi(x,df,cum,ccum)
      RETURN

   20 xnonc = pnonc/2.0D0
C**********************************************************************
C
C     The following code calcualtes the weight, chi-square, and
C     adjustment term for the central term in the infinite series.
C     The central term is the one in which the poisson weight is
C     greatest.  The adjustment term is the amount that must
C     be subtracted from the chi-square to move up two degrees
C     of freedom.
C
C**********************************************************************
      icent = int(xnonc)
      IF (icent.EQ.0) icent = 1
      chid2 = x/2.0D0
C
C
C     Calculate central weight term
C
C
      lfact = alngam(dble(icent+1))
      lcntwt = -xnonc + icent*log(xnonc) - lfact
      centwt = exp(lcntwt)
C
C
C     Calculate central chi-square
C
C
      CALL cumchi(x,dg(icent),pcent,ccum)
C
C
C     Calculate central adjustment term
C
C
      dfd2 = dg(icent)/2.0D0
      lfact = alngam(1.0D0+dfd2)
      lcntaj = dfd2*log(chid2) - chid2 - lfact
      centaj = exp(lcntaj)
      sum = centwt*pcent
C**********************************************************************
C
C     Sum backwards from the central term towards zero.
C     Quit whenever either
C     (1) the zero term is reached, or
C     (2) the term gets small relative to the sum, or
C     (3) More than NTIRED terms are totaled.
C
C**********************************************************************
      iterb = 0
      sumadj = 0.0D0
      adj = centaj
      wt = centwt
      i = icent
C
      GO TO 40

   30 IF (qtired(iterb) .OR. qsmall(term) .OR. i.EQ.0) GO TO 50
   40 dfd2 = dg(i)/2.0D0
C
C
C     Adjust chi-square for two fewer degrees of freedom.
C     The adjusted value ends up in PTERM.
C
C
      adj = adj*dfd2/chid2
      sumadj = sumadj + adj
      pterm = pcent + sumadj
C
C
C     Adjust poisson weight for J decreased by one
C
C
      wt = wt* (i/xnonc)
      term = wt*pterm
      sum = sum + term
      i = i - 1
      iterb = iterb + 1
      GO TO 30

   50 iterf = 0
C**********************************************************************
C
C     Now sum forward from the central term towards infinity.
C     Quit when either
C     (1) the term gets small relative to the sum, or
C     (2) More than NTIRED terms are totaled.
C
C**********************************************************************
      sumadj = centaj
      adj = centaj
      wt = centwt
      i = icent
C
      GO TO 70

   60 IF (qtired(iterf) .OR. qsmall(term)) GO TO 80
C
C
C     Update weights for next higher J
C
C
   70 wt = wt* (xnonc/ (i+1))
C
C
C     Calculate PTERM and add term to sum
C
C
      pterm = pcent - sumadj
      term = wt*pterm
      sum = sum + term
C
C
C     Update adjustment term for DF for next iteration
C
C
      i = i + 1
      dfd2 = dg(i)/2.0D0
      adj = adj*chid2/dfd2
      sumadj = sumadj + adj
      iterf = iterf + 1
      GO TO 60

   80 cum = sum
      ccum = 0.5D0 + (0.5D0-cum)
C
      RETURN

      END
      SUBROUTINE cumf(f,dfn,dfd,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMF(F,DFN,DFD,CUM,CCUM)
C                    CUMulative F distribution
C
C
C                              Function
C
C
C     Computes  the  integral from  0  to  F of  the f-density  with DFN
C     and DFD degrees of freedom.
C
C
C                              Arguments
C
C
C     F --> Upper limit of integration of the f-density.
C                                                  F is DOUBLE PRECISION
C
C     DFN --> Degrees of freedom of the numerator sum of squares.
C                                                  DFN is DOUBLE PRECISI
C
C     DFD --> Degrees of freedom of the denominator sum of squares.
C                                                  DFD is DOUBLE PRECISI
C
C     CUM <-- Cumulative f distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative f distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula  26.5.28 of  Abramowitz and   Stegun   is  used to  reduce
C     the cumulative F to a cumulative beta distribution.
C
C
C                              Note
C
C
C     If F is less than or equal to 0, 0 is returned.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION dfd,dfn,f,cum,ccum
C     ..
C     .. Local Scalars ..

      DOUBLE PRECISION dsum,prod,xx,yy
      INTEGER ierr
C     ..
C     .. Parameters ..
      DOUBLE PRECISION half
      PARAMETER (half=0.5D0)
      DOUBLE PRECISION done
      PARAMETER (done=1.0D0)
C     ..
C     .. External Subroutines ..
      EXTERNAL bratio
C     ..
C     .. Executable Statements ..

      IF (.NOT. (f.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 prod = dfn*f
C
C     XX is such that the incomplete beta with parameters
C     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
C
C     YY is 1 - XX
C
C     Calculate the smaller of XX and YY accurately
C
      dsum = dfd + prod
      xx = dfd/dsum
      IF (xx.GT.half) THEN
          yy = prod/dsum
          xx = done - yy

      ELSE
          yy = done - xx
      END IF

      CALL bratio(dfd*half,dfn*half,xx,yy,ccum,cum,ierr)
      RETURN

      END
      SUBROUTINE cumfnc(f,dfn,dfd,pnonc,cum,ccum)
C**********************************************************************
C
C               F -NON- -C-ENTRAL F DISTRIBUTION
C
C
C
C                              Function
C
C
C     COMPUTES NONCENTRAL F DISTRIBUTION WITH DFN AND DFD
C     DEGREES OF FREEDOM AND NONCENTRALITY PARAMETER PNONC
C
C
C                              Arguments
C
C
C     X --> UPPER LIMIT OF INTEGRATION OF NONCENTRAL F IN EQUATION
C
C     DFN --> DEGREES OF FREEDOM OF NUMERATOR
C
C     DFD -->  DEGREES OF FREEDOM OF DENOMINATOR
C
C     PNONC --> NONCENTRALITY PARAMETER.
C
C     CUM <-- CUMULATIVE NONCENTRAL F DISTRIBUTION
C
C     CCUM <-- COMPLIMENT OF CUMMULATIVE
C
C
C                              Method
C
C
C     USES FORMULA 26.6.20 OF REFERENCE FOR INFINITE SERIES.
C     SERIES IS CALCULATED BACKWARD AND FORWARD FROM J = LAMBDA/2
C     (THIS IS THE TERM WITH THE LARGEST POISSON WEIGHT) UNTIL
C     THE CONVERGENCE CRITERION IS MET.
C
C     FOR SPEED, THE INCOMPLETE BETA FUNCTIONS ARE EVALUATED
C     BY FORMULA 26.5.16.
C
C
C               REFERENCE
C
C
C     HANDBOOD OF MATHEMATICAL FUNCTIONS
C     EDITED BY MILTON ABRAMOWITZ AND IRENE A. STEGUN
C     NATIONAL BUREAU OF STANDARDS APPLIED MATEMATICS SERIES - 55
C     MARCH 1965
C     P 947, EQUATIONS 26.6.17, 26.6.18
C
C
C                              Note
C
C
C     THE SUM CONTINUES UNTIL A SUCCEEDING TERM IS LESS THAN EPS
C     TIMES THE SUM (OR THE SUM IS LESS THAN 1.0E-20).  EPS IS
C     SET TO 1.0E-4 IN A DATA STATEMENT WHICH CAN BE CHANGED.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION dfd,dfn,pnonc,f,cum,ccum
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION dsum,dummy,prod,xx,yy
      DOUBLE PRECISION adn,aup,b,betdn,betup,centwt,dnterm,eps,sum,
     +                 upterm,xmult,xnonc,x
      INTEGER i,icent,ierr
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alngam
      EXTERNAL alngam
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC log,dble,exp
C     ..
C     .. Statement Functions ..
      LOGICAL qsmall
C     ..
C     .. External Subroutines ..
      EXTERNAL bratio,cumf
C     ..
C     .. Parameters ..
      DOUBLE PRECISION half
      PARAMETER (half=0.5D0)
      DOUBLE PRECISION done
      PARAMETER (done=1.0D0)
C     ..
C     .. Data statements ..
      DATA eps/1.0D-4/
C     ..
C     .. Statement Function definitions ..
      qsmall(x) = sum .LT. 1.0D-20 .OR. x .LT. eps*sum
C     ..
C     .. Executable Statements ..
C
      IF (.NOT. (f.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 IF (.NOT. (pnonc.LT.1.0D-10)) GO TO 20
C
C     Handle case in which the non-centrality parameter is
C     (essentially) zero.

      CALL cumf(f,dfn,dfd,cum,ccum)
      RETURN

   20 xnonc = pnonc/2.0D0

C     Calculate the central term of the poisson weighting factor.

      icent = xnonc
      IF (icent.EQ.0) icent = 1

C     Compute central weight term

      centwt = exp(-xnonc+icent*log(xnonc)-alngam(dble(icent+1)))

C     Compute central incomplete beta term
C     Assure that minimum of arg to beta and 1 - arg is computed
C          accurately.

      prod = dfn*f
      dsum = dfd + prod
      yy = dfd/dsum
      IF (yy.GT.half) THEN
          xx = prod/dsum
          yy = done - xx

      ELSE
          xx = done - yy
      END IF

      CALL bratio(dfn*half+dble(icent),dfd*half,xx,yy,betdn,dummy,ierr)
      adn = dfn/2.0D0 + dble(icent)
      aup = adn
      b = dfd/2.0D0
      betup = betdn
      sum = centwt*betdn

C     Now sum terms backward from icent until convergence or all done

      xmult = centwt
      i = icent
      dnterm = exp(alngam(adn+b)-alngam(adn+1.0D0)-alngam(b)+
     +         adn*log(xx)+b*log(yy))
   30 IF (qsmall(xmult*betdn) .OR. i.LE.0) GO TO 40
      xmult = xmult* (i/xnonc)
      i = i - 1
      adn = adn - 1
      dnterm = (adn+1)/ ((adn+b)*xx)*dnterm
      betdn = betdn + dnterm
      sum = sum + xmult*betdn
      GO TO 30

   40 i = icent + 1

C     Now sum forwards until convergence

      xmult = centwt
      IF ((aup-1+b).EQ.0) THEN
          upterm = exp(-alngam(aup)-alngam(b)+ (aup-1)*log(xx)+
     +             b*log(yy))

      ELSE
          upterm = exp(alngam(aup-1+b)-alngam(aup)-alngam(b)+
     +             (aup-1)*log(xx)+b*log(yy))
      END IF

      GO TO 60

   50 IF (qsmall(xmult*betup)) GO TO 70
   60 xmult = xmult* (xnonc/i)
      i = i + 1
      aup = aup + 1
      upterm = (aup+b-2.0D0)*xx/ (aup-1)*upterm
      betup = betup - upterm
      sum = sum + xmult*betup
      GO TO 50

   70 cum = sum

      ccum = 0.5D0 + (0.5D0-cum)
      RETURN

      END
      SUBROUTINE cumgam(x,a,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMGAM(X,A,CUM,CCUM)
C           Double precision cUMulative incomplete GAMma distribution
C
C
C                              Function
C
C
C     Computes   the  cumulative        of    the     incomplete   gamma
C     distribution, i.e., the integral from 0 to X of
C          (1/GAM(A))*EXP(-T)*T**(A-1) DT
C     where GAM(A) is the complete gamma function of A, i.e.,
C          GAM(A) = integral from 0 to infinity of
C                    EXP(-T)*T**(A-1) DT
C
C
C                              Arguments
C
C
C     X --> The upper limit of integration of the incomplete gamma.
C                                                X is DOUBLE PRECISION
C
C     A --> The shape parameter of the incomplete gamma.
C                                                A is DOUBLE PRECISION
C
C     CUM <-- Cumulative incomplete gamma distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative incomplete gamma distribution.
C                                                CCUM is DOUBLE PRECISIO
C
C
C                              Method
C
C
C     Calls the routine GRATIO.
C
C**********************************************************************
C
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,x,cum,ccum
C     ..
C     .. External Routines ..
      EXTERNAL gratio
C     ..
C     .. Executable Statements ..
      IF (.NOT. (x.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 CALL gratio(a,x,cum,ccum,0)

C     Call gratio routine

      RETURN

      END
      SUBROUTINE cumnbn(s,xn,pr,ompr,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMNNBN(S,XN,PR,OMPR,CUM,CCUM)
C                    CUmulative Negative BINomial distribution
C
C
C                              Function
C
C
C     Returns the probability that it there will be S or fewer failures
C     before there are XN successes, with each binomial trial having
C     a probability of success PR.
C
C     Prob(# failures = S | XN successes, PR)  =
C                        ( XN + S - 1 )
C                        (            ) * PR^XN * (1-PR)^S
C                        (      S     )
C
C
C                              Arguments
C
C
C     S --> The number of failures
C                                                  S is DOUBLE PRECISION
C
C     XN --> The number of successes
C                                                  XN is DOUBLE PRECISIO
C
C     PR --> The probability of success in each binomial trial.
C                                                  PR is DOUBLE PRECISIO
C
C     OMPR --> 1 - PR
C                                                  OMPR is DOUBLE PRECIS
C
C     CUM <-- Cumulative negative binomial distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative negative binomial distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula  26.5.26    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  negative
C     binomial distribution to the cumulative beta distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION pr,ompr,s,xn,cum,ccum
C     ..
C     .. External Subroutines ..
      EXTERNAL cumbet
C     ..
C     .. Executable Statements ..
      CALL cumbet(pr,ompr,xn,s+1.D0,cum,ccum)
      RETURN

      END
      SUBROUTINE cumnor(arg,result,ccum)
C**********************************************************************
C
C     SUBROUINE CUMNOR(X,RESULT,CCUM)
C
C
C                              Function
C
C
C     Computes the cumulative  of    the  normal   distribution,   i.e.,
C     the integral from -infinity to x of
C          (1/sqrt(2*pi)) exp(-u*u/2) du
C
C     X --> Upper limit of integration.
C                                        X is DOUBLE PRECISION
C
C     RESULT <-- Cumulative normal distribution.
C                                        RESULT is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative normal distribution.
C                                        CCUM is DOUBLE PRECISION
C
C
C     Renaming of function ANORM from:
C
C     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
C     Package of Special Function Routines and Test Drivers"
C     acm Transactions on Mathematical Software. 19, 22-32.
C
C     with slight modifications to return ccum and to deal with
C     machine constants.
C
C**********************************************************************
C
C
C Original Comments:
C------------------------------------------------------------------
C
C This function evaluates the normal distribution function:
C
C                              / x
C                     1       |       -t*t/2
C          P(x) = ----------- |      e       dt
C                 sqrt(2 pi)  |
C                             /-oo
C
C   The main computation evaluates near-minimax approximations
C   derived from those in "Rational Chebyshev approximations for
C   the error function" by W. J. Cody, Math. Comp., 1969, 631-637.
C   This transportable program uses rational functions that
C   theoretically approximate the normal distribution function to
C   at least 18 significant decimal digits.  The accuracy achieved
C   depends on the arithmetic system, the compiler, the intrinsic
C   functions, and proper selection of the machine-dependent
C   constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants.
C
C   MIN   = smallest machine representable number.
C
C   EPS   = argument below which anorm(x) may be represented by
C           0.5  and above which  x*x  will not underflow.
C           A conservative value is the largest machine number X
C           such that   1.0 + X = 1.0   to machine precision.
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns  ANORM = 0     for  ARG .LE. XLOW.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, EXP
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: March 15, 1992
C
C------------------------------------------------------------------
      INTEGER i
      DOUBLE PRECISION a,arg,b,c,d,del,eps,half,p,one,q,result,sixten,
     +                 temp,sqrpi,thrsh,root32,x,xden,xnum,y,xsq,zero,
     +                 min,ccum
      DIMENSION a(5),b(4),c(9),d(8),p(6),q(5)
C------------------------------------------------------------------
C  External Function
C------------------------------------------------------------------
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
C------------------------------------------------------------------
C  Mathematical constants
C
C  SQRPI = 1 / sqrt(2*pi), ROOT32 = sqrt(32), and
C  THRSH is the argument for which anorm = 0.75.
C------------------------------------------------------------------
      DATA one,half,zero,sixten/1.0D0,0.5D0,0.0D0,1.60D0/,
     +     sqrpi/3.9894228040143267794D-1/,thrsh/0.66291D0/,
     +     root32/5.656854248D0/
C------------------------------------------------------------------
C  Coefficients for approximation in first interval
C------------------------------------------------------------------
      DATA a/2.2352520354606839287D00,1.6102823106855587881D02,
     +     1.0676894854603709582D03,1.8154981253343561249D04,
     +     6.5682337918207449113D-2/
      DATA b/4.7202581904688241870D01,9.7609855173777669322D02,
     +     1.0260932208618978205D04,4.5507789335026729956D04/
C------------------------------------------------------------------
C  Coefficients for approximation in second interval
C------------------------------------------------------------------
      DATA c/3.9894151208813466764D-1,8.8831497943883759412D00,
     +     9.3506656132177855979D01,5.9727027639480026226D02,
     +     2.4945375852903726711D03,6.8481904505362823326D03,
     +     1.1602651437647350124D04,9.8427148383839780218D03,
     +     1.0765576773720192317D-8/
      DATA d/2.2266688044328115691D01,2.3538790178262499861D02,
     +     1.5193775994075548050D03,6.4855582982667607550D03,
     +     1.8615571640885098091D04,3.4900952721145977266D04,
     +     3.8912003286093271411D04,1.9685429676859990727D04/
C------------------------------------------------------------------
C  Coefficients for approximation in third interval
C------------------------------------------------------------------
      DATA p/2.1589853405795699D-1,1.274011611602473639D-1,
     +     2.2235277870649807D-2,1.421619193227893466D-3,
     +     2.9112874951168792D-5,2.307344176494017303D-2/
      DATA q/1.28426009614491121D00,4.68238212480865118D-1,
     +     6.59881378689285515D-2,3.78239633202758244D-3,
     +     7.29751555083966205D-5/
C------------------------------------------------------------------
C  Machine dependent constants
C------------------------------------------------------------------
      eps = spmpar(1)*0.5D0
      min = spmpar(2)
C------------------------------------------------------------------
      x = arg
      y = abs(x)
      IF (y.LE.thrsh) THEN
C------------------------------------------------------------------
C  Evaluate  anorm  for  |X| <= 0.66291
C------------------------------------------------------------------
          xsq = zero
          IF (y.GT.eps) xsq = x*x
          xnum = a(5)*xsq
          xden = xsq
          DO 10 i = 1,3
              xnum = (xnum+a(i))*xsq
              xden = (xden+b(i))*xsq
   10     CONTINUE
          result = x* (xnum+a(4))/ (xden+b(4))
          temp = result
          result = half + temp
          ccum = half - temp
C------------------------------------------------------------------
C  Evaluate  anorm  for 0.66291 <= |X| <= sqrt(32)
C------------------------------------------------------------------
      ELSE IF (y.LE.root32) THEN
          xnum = c(9)*y
          xden = y
          DO 20 i = 1,7
              xnum = (xnum+c(i))*y
              xden = (xden+d(i))*y
   20     CONTINUE
          result = (xnum+c(8))/ (xden+d(8))
          xsq = aint(y*sixten)/sixten
          del = (y-xsq)* (y+xsq)
          result = exp(-xsq*xsq*half)*exp(-del*half)*result
          ccum = one - result
          IF (x.GT.zero) THEN
              temp = result
              result = ccum
              ccum = temp
          END IF
C------------------------------------------------------------------
C  Evaluate  anorm  for |X| > sqrt(32)
C------------------------------------------------------------------
      ELSE
          result = zero
          xsq = one/ (x*x)
          xnum = p(6)*xsq
          xden = xsq
          DO 30 i = 1,4
              xnum = (xnum+p(i))*xsq
              xden = (xden+q(i))*xsq
   30     CONTINUE
          result = xsq* (xnum+p(5))/ (xden+q(5))
          result = (sqrpi-result)/y
          xsq = aint(x*sixten)/sixten
          del = (x-xsq)* (x+xsq)
          result = exp(-xsq*xsq*half)*exp(-del*half)*result
          ccum = one - result
          IF (x.GT.zero) THEN
              temp = result
              result = ccum
              ccum = temp
          END IF

      END IF

      IF (result.LT.min) result = 0.0D0
      IF (ccum.LT.min) ccum = 0.0D0
C------------------------------------------------------------------
C  Fix up for negative argument, erf, etc.
C------------------------------------------------------------------
C----------Last card of ANORM ----------
      END
      SUBROUTINE cumpoi(s,xlam,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMPOI(S,XLAM,CUM,CCUM)
C                    CUMulative POIsson distribution
C
C
C                              Function
C
C
C     Returns the  probability  of  S   or  fewer events in  a   Poisson
C     distribution with mean XLAM.
C
C
C                              Arguments
C
C
C     S --> Upper limit of cumulation of the Poisson.
C                                                  S is DOUBLE PRECISION
C
C     XLAM --> Mean of the Poisson distribution.
C                                                  XLAM is DOUBLE PRECIS
C
C     CUM <-- Cumulative poisson distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative poisson distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Uses formula  26.4.21   of   Abramowitz and  Stegun,  Handbook  of
C     Mathematical   Functions  to reduce   the   cumulative Poisson  to
C     the cumulative chi-square distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION s,xlam,cum,ccum
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION chi,df
C     ..
C     .. External Subroutines ..
      EXTERNAL cumchi
C     ..
C     .. Executable Statements ..
      df = 2.0D0* (s+1.0D0)
      chi = 2.0D0*xlam
      CALL cumchi(chi,df,ccum,cum)
      RETURN

      END
      SUBROUTINE cumt(t,df,cum,ccum)
C**********************************************************************
C
C     SUBROUTINE CUMT(T,DF,CUM,CCUM)
C                    CUMulative T-distribution
C
C
C                              Function
C
C
C     Computes the integral from -infinity to T of the t-density.
C
C
C                              Arguments
C
C
C     T --> Upper limit of integration of the t-density.
C                                                  T is DOUBLE PRECISION
C
C     DF --> Degrees of freedom of the t-distribution.
C                                                  DF is DOUBLE PRECISIO
C
C     CUM <-- Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C     CCUM <-- Compliment of Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula 26.5.27   of     Abramowitz  and   Stegun,    Handbook  of
C     Mathematical Functions  is   used   to  reduce the  t-distribution
C     to an incomplete beta.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION df,t,cum,ccum
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION xx,a,oma,tt,yy,dfptt
C     ..
C     .. External Subroutines ..
      EXTERNAL cumbet
C     ..
C     .. Executable Statements ..
      tt = t*t
      dfptt = df + tt
      xx = df/dfptt
      yy = tt/dfptt
      CALL cumbet(xx,yy,0.5D0*df,0.5D0,a,oma)
      IF (.NOT. (t.LE.0.0D0)) GO TO 10
      cum = 0.5D0*a
      ccum = oma + cum
      GO TO 20

   10 ccum = 0.5D0*a
      cum = oma + ccum
   20 RETURN

      END
      DOUBLE PRECISION FUNCTION dbetrm(a,b)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DBETRM( A, B )
C          Double Precision Sterling Remainder for Complete
C                    Beta Function
C
C
C                              Function
C
C
C     Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
C     where Lgamma is the log of the (complete) gamma function
C
C     Let ZZ be approximation obtained if each log gamma is approximated
C     by Sterling's formula, i.e.,
C     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
C
C     Returns Log(Beta(A,B)) - ZZ
C
C
C                              Arguments
C
C
C     A --> One argument of the Beta
C                    DOUBLE PRECISION A
C
C     B --> The other argument of the Beta
C                    DOUBLE PRECISION B
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b
C     ..
C     .. External Functions ..
      DOUBLE PRECISION dstrem
      EXTERNAL dstrem
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC max,min
C     ..
C     .. Executable Statements ..

C     Try to sum from smallest to largest
      dbetrm = -dstrem(a+b)
      dbetrm = dbetrm + dstrem(max(a,b))
      dbetrm = dbetrm + dstrem(min(a,b))
      RETURN

      END
      DOUBLE PRECISION FUNCTION devlpl(a,n,x)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DEVLPL(A,N,X)
C              Double precision EVALuate a PoLynomial at X
C
C
C                              Function
C
C
C     returns
C          A(1) + A(2)*X + ... + A(N)*X**(N-1)
C
C
C                              Arguments
C
C
C     A --> Array of coefficients of the polynomial.
C                                        A is DOUBLE PRECISION(N)
C
C     N --> Length of A, also degree of polynomial - 1.
C                                        N is INTEGER
C
C     X --> Point at which the polynomial is to be evaluated.
C                                        X is DOUBLE PRECISION
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
      INTEGER n
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION a(n)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION term
      INTEGER i
C     ..
C     .. Executable Statements ..
      term = a(n)
      DO 10,i = n - 1,1,-1
          term = a(i) + term*x
   10 CONTINUE
      devlpl = term
      RETURN

      END
      DOUBLE PRECISION FUNCTION dexpm1(x)
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION dexpm1(x)
C            Evaluation of the function EXP(X) - 1
C
C
C                              Arguments
C
C
C     X --> Argument at which exp(x)-1 desired
C                    DOUBLE PRECISION X
C
C
C                              Method
C
C
C     Renaming of function rexp from code of:
C
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION p1,p2,q1,q2,q3,q4,w
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp
C     ..
C     .. Data statements ..
      DATA p1/.914041914819518D-09/,p2/.238082361044469D-01/,
     +     q1/-.499999999085958D+00/,q2/.107141568980644D+00/,
     +     q3/-.119041179760821D-01/,q4/.595130811860248D-03/
C     ..
C     .. Executable Statements ..
C
      IF (abs(x).GT.0.15D0) GO TO 10
      dexpm1 = x* (((p2*x+p1)*x+1.0D0)/
     +         ((((q4*x+q3)*x+q2)*x+q1)*x+1.0D0))
      RETURN
C
   10 w = exp(x)
      IF (x.GT.0.0D0) GO TO 20
      dexpm1 = (w-0.5D0) - 0.5D0
      RETURN

   20 dexpm1 = w* (0.5D0+ (0.5D0-1.0D0/w))
      RETURN

      END
      DOUBLE PRECISION FUNCTION dinvnr(p,q)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DINVNR(P,Q)
C     Double precision NoRmal distribution INVerse
C
C
C                              Function
C
C
C     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
C     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
C
C
C                              Arguments
C
C
C     P --> The probability whose normal deviate is sought.
C                    P is DOUBLE PRECISION
C
C     Q --> 1-P
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start
C     value for the Newton method of finding roots.
C
C
C                              Note
C
C
C     If P or Q .lt. machine EPS returns +/- DINVNR(EPS)
C
C**********************************************************************
C     .. Parameters ..
      INTEGER maxit
      PARAMETER (maxit=100)
      DOUBLE PRECISION eps
      PARAMETER (eps=1.0D-13)
      DOUBLE PRECISION r2pi
      PARAMETER (r2pi=0.3989422804014326D0)
      DOUBLE PRECISION nhalf
      PARAMETER (nhalf=-0.5D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION p,q
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION strtx,xcur,cum,ccum,pp,dx
      INTEGER i
      LOGICAL qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION stvaln
      EXTERNAL stvaln
C     ..
C     .. External Subroutines ..
      EXTERNAL cumnor
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION dennor,x

      dennor(x) = r2pi*exp(nhalf*x*x)
C     ..
C     .. Executable Statements ..
C
C     FIND MINIMUM OF P AND Q
C
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 10
      pp = p
      GO TO 20

   10 pp = q
C
C     INITIALIZATION STEP
C
   20 strtx = stvaln(pp)
      xcur = strtx
C
C     NEWTON INTERATIONS
C
      DO 30,i = 1,maxit
          CALL cumnor(xcur,cum,ccum)
          dx = (cum-pp)/dennor(xcur)
          xcur = xcur - dx
          IF (abs(dx/xcur).LT.eps) GO TO 40
   30 CONTINUE
      dinvnr = strtx
C
C     IF WE GET HERE, NEWTON HAS FAILED
C
      IF (.NOT.qporq) dinvnr = -dinvnr
      RETURN
C
C     IF WE GET HERE, NEWTON HAS SUCCEDED
C
   40 dinvnr = xcur
      IF (.NOT.qporq) dinvnr = -dinvnr
      RETURN

      END
      SUBROUTINE dinvr(status,x,fx,qleft,qhi)
C**********************************************************************
C
C     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI)
C          Double precision
C          bounds the zero of the function and invokes zror
C                    Reverse Communication
C
C
C                              Function
C
C
C     Bounds the    function  and  invokes  ZROR   to perform the   zero
C     finding.  STINVR  must  have   been  called  before this   routine
C     in order to set its parameters.
C
C
C                              Arguments
C
C
C     STATUS <--> At the beginning of a zero finding problem, STATUS
C                 should be set to 0 and INVR invoked.  (The value
C                 of parameters other than X will be ignored on this cal
C
C                 When INVR needs the function evaluated, it will set
C                 STATUS to 1 and return.  The value of the function
C                 should be set in FX and INVR again called without
C                 changing any of its other parameters.
C
C                 When INVR has finished without error, it will return
C                 with STATUS 0.  In that case X is approximately a root
C                 of F(X).
C
C                 If INVR cannot bound the function, it returns status
C                 -1 and sets QLEFT and QHI.
C                         INTEGER STATUS
C
C     X <-- The value of X at which F(X) is to be evaluated.
C                         DOUBLE PRECISION X
C
C     FX --> The value of F(X) calculated when INVR returns with
C            STATUS = 1.
C                         DOUBLE PRECISION FX
C
C     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
C          case it is .TRUE. If the stepping search terminated
C          unsucessfully at SMALL.  If it is .FALSE. the search
C          terminated unsucessfully at BIG.
C                    QLEFT is LOGICAL
C
C     QHI <-- Defined only if QMFINV returns .FALSE.  In that
C          case it is .TRUE. if F(X) .GT. Y at the termination
C          of the search and .FALSE. if F(X) .LT. Y at the
C          termination of the search.
C                    QHI is LOGICAL

C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,zabsst,zabsto,zbig,zrelst,zrelto,zsmall,
     +                 zstpmu
      INTEGER status
      LOGICAL qhi,qleft
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION absstp,abstol,big,fbig,fsmall,relstp,reltol,
     +                 small,step,stpmul,xhi,xlb,xlo,xsave,xub,yy,zx,zy,
     +                 zz
      INTEGER i99999
      LOGICAL qbdd,qcond,qdum1,qdum2,qincr,qlim,qok,qup
C     ..
C     .. External Subroutines ..
      EXTERNAL dstzr,dzror
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,max,min
C     ..
C     .. Statement Functions ..
      LOGICAL qxmon
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Statement Function definitions ..
      qxmon(zx,zy,zz) = zx .LE. zy .AND. zy .LE. zz
C     ..
C     .. Executable Statements ..

      IF (status.GT.0) GO TO 310

      qcond = .NOT. qxmon(small,x,big)
      IF (qcond) STOP ' SMALL, X, BIG not monotone in INVR'
      xsave = x
C
C     See that SMALL and BIG bound the zero and set QINCR
C
      x = small
C     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 300

   10 fsmall = fx
      x = big
C     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 300

   20 fbig = fx
      qincr = fbig .GT. fsmall
      IF (.NOT. (qincr)) GO TO 50
      IF (fsmall.LE.0.0D0) GO TO 30
      status = -1
      qleft = .TRUE.
      qhi = .TRUE.
      RETURN

   30 IF (fbig.GE.0.0D0) GO TO 40
      status = -1
      qleft = .FALSE.
      qhi = .FALSE.
      RETURN

   40 GO TO 80

   50 IF (fsmall.GE.0.0D0) GO TO 60
      status = -1
      qleft = .TRUE.
      qhi = .FALSE.
      RETURN

   60 IF (fbig.LE.0.0D0) GO TO 70
      status = -1
      qleft = .FALSE.
      qhi = .TRUE.
      RETURN

   70 CONTINUE
   80 x = xsave
      step = max(absstp,relstp*abs(x))
C      YY = F(X) - Y
C     GET-FUNCTION-VALUE
      ASSIGN 90 TO i99999
      GO TO 300

   90 yy = fx
      IF (.NOT. (yy.EQ.0.0D0)) GO TO 100
      status = 0
      qok = .TRUE.
      RETURN

  100 qup = (qincr .AND. (yy.LT.0.0D0)) .OR.
     +      (.NOT.qincr .AND. (yy.GT.0.0D0))
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     HANDLE CASE IN WHICH WE MUST STEP HIGHER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (.NOT. (qup)) GO TO 170
      xlb = xsave
      xub = min(xlb+step,big)
      GO TO 120

  110 IF (qcond) GO TO 150
C      YY = F(XUB) - Y
  120 x = xub
C     GET-FUNCTION-VALUE
      ASSIGN 130 TO i99999
      GO TO 300

  130 yy = fx
      qbdd = (qincr .AND. (yy.GE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.LE.0.0D0))
      qlim = xub .GE. big
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 140
      step = stpmul*step
      xlb = xub
      xub = min(xlb+step,big)
  140 GO TO 110

  150 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 160
      status = -1
      qleft = .FALSE.
      qhi = .NOT. qincr
      x = big
      RETURN

  160 GO TO 240
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     HANDLE CASE IN WHICH WE MUST STEP LOWER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  170 xub = xsave
      xlb = max(xub-step,small)
      GO TO 190

  180 IF (qcond) GO TO 220
C      YY = F(XLB) - Y
  190 x = xlb
C     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 300

  200 yy = fx
      qbdd = (qincr .AND. (yy.LE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.GE.0.0D0))
      qlim = xlb .LE. small
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 210
      step = stpmul*step
      xub = xlb
      xlb = max(xub-step,small)
  210 GO TO 180

  220 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 230
      status = -1
      qleft = .TRUE.
      qhi = qincr
      x = small
      RETURN

  230 CONTINUE
  240 CALL dstzr(xlb,xub,abstol,reltol)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      status = 0
      GO TO 260

  250 IF (.NOT. (status.EQ.1)) GO TO 290
  260 CALL dzror(status,x,fx,xlo,xhi,qdum1,qdum2)
      IF (.NOT. (status.EQ.1)) GO TO 280
C     GET-FUNCTION-VALUE
      ASSIGN 270 TO i99999
      GO TO 300

  270 CONTINUE
  280 GO TO 250

  290 x = xlo
      status = 0
      RETURN

      ENTRY dstinv(zsmall,zbig,zabsst,zrelst,zstpmu,zabsto,zrelto)
C**********************************************************************
C
C      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL,
C     +                   ABSTOL, RELTOL )
C      Double Precision - SeT INverse finder - Reverse Communication
C
C
C                              Function
C
C
C     Concise Description - Given a monotone function F finds X
C     such that F(X) = Y.  Uses Reverse communication -- see invr.
C     This routine sets quantities needed by INVR.
C
C          More Precise Description of INVR -
C
C     F must be a monotone function, the results of QMFINV are
C     otherwise undefined.  QINCR must be .TRUE. if F is non-
C     decreasing and .FALSE. if F is non-increasing.
C
C     QMFINV will return .TRUE. if and only if F(SMALL) and
C     F(BIG) bracket Y, i. e.,
C          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
C          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
C
C     if QMFINV returns .TRUE., then the X returned satisfies
C     the following condition.  let
C               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
C     then if QINCR is .TRUE.,
C          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
C     and if QINCR is .FALSE.
C          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
C
C
C                              Arguments
C
C
C     SMALL --> The left endpoint of the interval to be
C          searched for a solution.
C                    SMALL is DOUBLE PRECISION
C
C     BIG --> The right endpoint of the interval to be
C          searched for a solution.
C                    BIG is DOUBLE PRECISION
C
C     ABSSTP, RELSTP --> The initial step size in the search
C          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
C                    ABSSTP is DOUBLE PRECISION
C                    RELSTP is DOUBLE PRECISION
C
C     STPMUL --> When a step doesn't bound the zero, the step
C                size is multiplied by STPMUL and another step
C                taken.  A popular value is 2.0
C                    DOUBLE PRECISION STPMUL
C
C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
C          of the solution.  See function for a precise definition.
C                    ABSTOL is DOUBLE PRECISION
C                    RELTOL is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Compares F(X) with Y for the input value of X then uses QINCR
C     to determine whether to step left or right to bound the
C     desired x.  the initial step size is
C          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
C     Iteratively steps right or left until it bounds X.
C     At each step which doesn't bound X, the step size is doubled.
C     The routine is careful never to step beyond SMALL or BIG.  If
C     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
C     after setting QLEFT and QHI.
C
C     If X is successfully bounded then Algorithm R of the paper
C     'Two Efficient Algorithms with Guaranteed Convergence for
C     Finding a Zero of a Function' by J. C. P. Bus and
C     T. J. Dekker in ACM Transactions on Mathematical
C     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
C     to find the zero of the function F(X)-Y. This is routine
C     QRZERO.
C
C**********************************************************************
      small = zsmall
      big = zbig
      absstp = zabsst
      relstp = zrelst
      stpmul = zstpmu
      abstol = zabsto
      reltol = zrelto
      RETURN

      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
C     TO GET-FUNCTION-VALUE
  300 status = 1
      RETURN

  310 CONTINUE
      GO TO i99999

      END
      DOUBLE PRECISION FUNCTION dlanor(x)
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION DLANOR( X )
C           Double precision Logarith of the Asymptotic Normal
C
C
C                              Function
C
C
C      Computes the logarithm of the cumulative normal distribution
C      from abs( x ) to infinity for abs( x ) >= 5.
C
C
C                              Arguments
C
C
C      X --> Value at which cumulative normal to be evaluated
C                     DOUBLE PRECISION X
C
C
C                              Method
C
C
C      23 term expansion of formula 26.2.12 of Abramowitz and Stegun.
C      The relative error at X = 5 is about 0.5E-5.
C
C
C                              Note
C
C
C      ABS(X) must be >= 5 else there is an error stop.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION dlsqpi
      PARAMETER (dlsqpi=0.91893853320467274177D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION approx,correc,xx,xx2
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION coef(12)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION devlpl,dln1px
      EXTERNAL devlpl,dln1px
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,log
C     ..
C     .. Data statements ..
      DATA coef/-1.0D0,3.0D0,-15.0D0,105.0D0,-945.0D0,10395.0D0,
     +     -135135.0D0,2027025.0D0,-34459425.0D0,654729075.0D0,
     +     -13749310575D0,316234143225.0D0/
C     ..
C     .. Executable Statements ..

      xx = abs(x)
      IF (xx.LT.5.0D0) STOP ' Argument too small in DLANOR'

      approx = -dlsqpi - 0.5D0*xx*xx - log(xx)

      xx2 = xx*xx
      correc = devlpl(coef,12,1.0D0/xx2)/xx2
      correc = dln1px(correc)

      dlanor = approx + correc

      RETURN

      END
      DOUBLE PRECISION FUNCTION dln1mx(x)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLN1MX(X)
C               Double precision LN(1-X)
C
C
C                              Function
C
C
C     Returns ln(1-x) for small x (good accuracy if x .le. 0.1).
C     Note that the obvious code of
C               LOG(1.0-X)
C     won't work for small X because 1.0-X loses accuracy
C
C
C                              Arguments
C
C
C     X --> Value for which ln(1-x) is desired.
C                                        X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     If X > 0.1, the obvious code above is used ELSE
C     The Taylor series for 1-x is expanded to 20 terms.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. External Functions ..
      DOUBLE PRECISION dln1px
      EXTERNAL dln1px
C     ..
C     .. Executable Statements ..

      dln1mx = dln1px(-x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION dln1px(a)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLN1PX(X)
C               Double precision LN(1+X)
C
C
C                              Function
C
C
C     Returns ln(1+x)
C     Note that the obvious code of
C               LOG(1.0+X)
C     won't work for small X because 1.0+X loses accuracy
C
C
C                              Arguments
C
C
C     X --> Value for which ln(1-x) is desired.
C                                        X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Renames ALNREL from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION LN(1 + A)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION p1,p2,p3,q1,q2,q3,t,t2,w,x
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog
C     ..
C     .. Data statements ..
      DATA p1/-.129418923021993D+01/,p2/.405303492862024D+00/,
     +     p3/-.178874546012214D-01/
      DATA q1/-.162752256355323D+01/,q2/.747811014037616D+00/,
     +     q3/-.845104217945565D-01/
C     ..
C     .. Executable Statements ..
C--------------------------
      IF (abs(a).GT.0.375D0) GO TO 10
      t = a/ (a+2.0D0)
      t2 = t*t
      w = (((p3*t2+p2)*t2+p1)*t2+1.0D0)/ (((q3*t2+q2)*t2+q1)*t2+1.0D0)
      dln1px = 2.0D0*t*w
      RETURN
C
   10 x = 1.D0 + dble(a)
      dln1px = dlog(x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION dlnbet(a0,b0)
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION DLNBET( A, B )
C          Double precision LN of the complete BETa
C
C
C                              Function
C
C
C     Returns the natural log of the complete beta function,
C     i.e.,
C
C                  ln( Gamma(a)*Gamma(b) / Gamma(a+b)
C
C
C                              Arguments
C
C
C   A,B --> The (symmetric) arguments to the complete beta
C                  DOUBLE PRECISION A, B
C
C
C                              Method
C
C
C     Renames BETALN from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
C-----------------------------------------------------------------------
C     E = 0.5*LN(2*PI)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a0,b0
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,c,e,h,u,v,w,z
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION algdiv,alnrel,bcorr,gamln,gsumln
      EXTERNAL algdiv,alnrel,bcorr,gamln,gsumln
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog,dmax1,dmin1
C     ..
C     .. Data statements ..
      DATA e/.918938533204673D0/
C     ..
C     .. Executable Statements ..
C--------------------------
      a = dmin1(a0,b0)
      b = dmax1(a0,b0)
      IF (a.GE.8.0D0) GO TO 100
      IF (a.GE.1.0D0) GO TO 20
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .LT. 1
C-----------------------------------------------------------------------
      IF (b.GE.8.0D0) GO TO 10
      dlnbet = gamln(a) + (gamln(b)-gamln(a+b))
      RETURN

   10 dlnbet = gamln(a) + algdiv(a,b)
      RETURN
C-----------------------------------------------------------------------
C                PROCEDURE WHEN 1 .LE. A .LT. 8
C-----------------------------------------------------------------------
   20 IF (a.GT.2.0D0) GO TO 40
      IF (b.GT.2.0D0) GO TO 30
      dlnbet = gamln(a) + gamln(b) - gsumln(a,b)
      RETURN

   30 w = 0.0D0
      IF (b.LT.8.0D0) GO TO 60
      dlnbet = gamln(a) + algdiv(a,b)
      RETURN
C
C                REDUCTION OF A WHEN B .LE. 1000
C
   40 IF (b.GT.1000.0D0) GO TO 80
      n = a - 1.0D0
      w = 1.0D0
      DO 50 i = 1,n
          a = a - 1.0D0
          h = a/b
          w = w* (h/ (1.0D0+h))
   50 CONTINUE
      w = dlog(w)
      IF (b.LT.8.0D0) GO TO 60
      dlnbet = w + gamln(a) + algdiv(a,b)
      RETURN
C
C                 REDUCTION OF B WHEN B .LT. 8
C
   60 n = b - 1.0D0
      z = 1.0D0
      DO 70 i = 1,n
          b = b - 1.0D0
          z = z* (b/ (a+b))
   70 CONTINUE
      dlnbet = w + dlog(z) + (gamln(a)+ (gamln(b)-gsumln(a,b)))
      RETURN
C
C                REDUCTION OF A WHEN B .GT. 1000
C
   80 n = a - 1.0D0
      w = 1.0D0
      DO 90 i = 1,n
          a = a - 1.0D0
          w = w* (a/ (1.0D0+a/b))
   90 CONTINUE
      dlnbet = (dlog(w)-n*dlog(b)) + (gamln(a)+algdiv(a,b))
      RETURN
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .GE. 8
C-----------------------------------------------------------------------
  100 w = bcorr(a,b)
      h = a/b
      c = h/ (1.0D0+h)
      u = - (a-0.5D0)*dlog(c)
      v = b*alnrel(h)
      IF (u.LE.v) GO TO 110
      dlnbet = (((-0.5D0*dlog(b)+e)+w)-v) - u
      RETURN

  110 dlnbet = (((-0.5D0*dlog(b)+e)+w)-u) - v
      RETURN

      END
      DOUBLE PRECISION FUNCTION dlngam(a)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLNGAM(X)
C                 Double precision LN of the GAMma function
C
C
C                              Function
C
C
C     Returns the natural logarithm of GAMMA(X).
C
C
C                              Arguments
C
C
C     X --> value at which scaled log gamma is to be returned
C                    X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Renames GAMLN from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS
C          NAVAL SURFACE WARFARE CENTER
C          DAHLGREN, VIRGINIA
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION c0,c1,c2,c3,c4,c5,d,t,w
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION gamln1
      EXTERNAL gamln1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog
C     ..
C     .. Data statements ..
C--------------------------
      DATA d/.418938533204673D0/
      DATA c0/.833333333333333D-01/,c1/-.277777777760991D-02/,
     +     c2/.793650666825390D-03/,c3/-.595202931351870D-03/,
     +     c4/.837308034031215D-03/,c5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
      IF (a.GT.0.8D0) GO TO 10
      dlngam = gamln1(a) - dlog(a)
      RETURN

   10 IF (a.GT.2.25D0) GO TO 20
      t = (a-0.5D0) - 0.5D0
      dlngam = gamln1(t)
      RETURN
C
   20 IF (a.GE.10.0D0) GO TO 40
      n = a - 1.25D0
      t = a
      w = 1.0D0
      DO 30 i = 1,n
          t = t - 1.0D0
          w = t*w
   30 CONTINUE
      dlngam = gamln1(t-1.0D0) + dlog(w)
      RETURN
C
   40 t = (1.0D0/a)**2
      w = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/a
      dlngam = (d+w) + (a-0.5D0)* (dlog(a)-1.0D0)
      END
      DOUBLE PRECISION FUNCTION dstrem(z)
      IMPLICIT DOUBLE PRECISION (a-h,o-p,r-z),INTEGER (i-n),LOGICAL (q)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DSTREM( Z )
C             Double precision Sterling Remainder
C
C
C                              Function
C
C
C     Returns   Log(Gamma(Z))  -  Sterling(Z)  where   Sterling(Z)  is
C     Sterling's Approximation to Log(Gamma(Z))
C
C     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
C
C
C                              Arguments
C
C
C     Z --> Value at which Sterling remainder calculated
C           Must be positive.
C                  DOUBLE PRECISION Z
C
C
C                              Method
C
C
C
C     If Z >= 6 uses 9 terms of series in Bernoulli numbers
C     (Values calculated using Maple)
C     Otherwise computes difference explicitly
C
C**********************************************************************

C     .. Parameters ..
      DOUBLE PRECISION hln2pi
      PARAMETER (hln2pi=0.91893853320467274178D0)
      INTEGER ncoef
      PARAMETER (ncoef=10)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION sterl
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION coef(ncoef)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION devlpl,dlngam
      EXTERNAL devlpl,dlngam
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC log
C     ..
C     .. Data statements ..
      DATA coef/0.0D0,0.0833333333333333333333333333333D0,
     +     -0.00277777777777777777777777777778D0,
     +     0.000793650793650793650793650793651D0,
     +     -0.000595238095238095238095238095238D0,
     +     0.000841750841750841750841750841751D0,
     +     -0.00191752691752691752691752691753D0,
     +     0.00641025641025641025641025641026D0,
     +     -0.0295506535947712418300653594771D0,
     +     0.179644372368830573164938490016D0/
C     ..
C     .. Executable Statements ..

C    For information, here are the next 11 coefficients of the
C    remainder term in Sterling's formula
C            -1.39243221690590111642743221691
C            13.4028640441683919944789510007
C            -156.848284626002017306365132452
C            2193.10333333333333333333333333
C            -36108.7712537249893571732652192
C            691472.268851313067108395250776
C            -0.152382215394074161922833649589D8
C            0.382900751391414141414141414141D9
C            -0.108822660357843910890151491655D11
C            0.347320283765002252252252252252D12
C            -0.123696021422692744542517103493D14
C

      IF (z.LE.0.0D0) STOP 'Zero or negative argument in DSTREM'
      IF (.NOT. (z.GT.6.0D0)) GO TO 10
      dstrem = devlpl(coef,10,1.0D0/z**2)*z
      GO TO 20

   10 sterl = hln2pi + (z-0.5D0)*log(z) - z
      dstrem = dlngam(z) - sterl
   20 RETURN

      END
      DOUBLE PRECISION FUNCTION dt1(p,q,df)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DT1(P,Q,DF)
C     Double precision Initalize Approximation to
C           INVerse of the cumulative T distribution
C
C
C                              Function
C
C
C     Returns  the  inverse   of  the T   distribution   function, i.e.,
C     the integral from 0 to INVT of the T density is P. This is an
C     initial approximation
C
C
C                              Arguments
C
C
C     P --> The p-value whose inverse from the T distribution is
C          desired.
C                    P is DOUBLE PRECISION
C
C     Q --> 1-P.
C                    Q is DOUBLE PRECISION
C
C     DF --> Degrees of freedom of the T distribution.
C                    DF is DOUBLE PRECISION
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION df,p,q
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION denpow,sum,term,x,xp,xx
      INTEGER i
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION coef(5,4),denom(4)
      INTEGER ideg(4)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION dinvnr,devlpl
      EXTERNAL dinvnr,devlpl
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs
C     ..
C     .. Data statements ..
      DATA (coef(i,1),i=1,5)/1.0D0,1.0D0,3*0.0D0/
      DATA (coef(i,2),i=1,5)/3.0D0,16.0D0,5.0D0,2*0.0D0/
      DATA (coef(i,3),i=1,5)/-15.0D0,17.0D0,19.0D0,3.0D0,0.0D0/
      DATA (coef(i,4),i=1,5)/-945.0D0,-1920.0D0,1482.0D0,776.0D0,79.0D0/
      DATA ideg/2,3,4,5/
      DATA denom/4.0D0,96.0D0,384.0D0,92160.0D0/
C     ..
C     .. Executable Statements ..
      x = abs(dinvnr(p,q))
      xx = x*x
      sum = x
      denpow = 1.0D0
      DO 10,i = 1,4
          term = devlpl(coef(1,i),ideg(i),xx)*x
          denpow = denpow*df
          sum = sum + term/ (denpow*denom(i))
   10 CONTINUE
      IF (.NOT. (p.GE.0.5D0)) GO TO 20
      xp = sum
      GO TO 30

   20 xp = -sum
   30 dt1 = xp
      RETURN

      END
      SUBROUTINE dzror(status,x,fx,xlo,xhi,qleft,qhi)
C**********************************************************************
C
C     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI)
C     Double precision ZeRo of a function -- Reverse Communication
C
C
C                              Function
C
C
C     Performs the zero finding.  STZROR must have been called before
C     this routine in order to set its parameters.
C
C
C                              Arguments
C
C
C     STATUS <--> At the beginning of a zero finding problem, STATUS
C                 should be set to 0 and ZROR invoked.  (The value
C                 of other parameters will be ignored on this call.)
C
C                 When ZROR needs the function evaluated, it will set
C                 STATUS to 1 and return.  The value of the function
C                 should be set in FX and ZROR again called without
C                 changing any of its other parameters.
C
C                 When ZROR has finished without error, it will return
C                 with STATUS 0.  In that case (XLO,XHI) bound the answe
C
C                 If ZROR finds an error (which implies that F(XLO)-Y an
C                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
C                 this case, XLO and XHI are undefined.
C                         INTEGER STATUS
C
C     X <-- The value of X at which F(X) is to be evaluated.
C                         DOUBLE PRECISION X
C
C     FX --> The value of F(X) calculated when ZROR returns with
C            STATUS = 1.
C                         DOUBLE PRECISION FX
C
C     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
C             inverval in X containing the solution below.
C                         DOUBLE PRECISION XLO
C
C     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
C             inverval in X containing the solution above.
C                         DOUBLE PRECISION XHI
C
C     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
C                at XLO.  If it is .FALSE. the search terminated
C                unsucessfully at XHI.
C                    QLEFT is LOGICAL
C
C     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
C              search and .FALSE. if F(X) .LT. Y at the
C              termination of the search.
C                    QHI is LOGICAL
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,xhi,xlo,zabstl,zreltl,zxhi,zxlo
      INTEGER status
      LOGICAL qhi,qleft
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,abstol,b,c,d,fa,fb,fc,fd,fda,fdb,m,mb,p,q,
     +                 reltol,tol,w,xxhi,xxlo,zx
      INTEGER ext,i99999
      LOGICAL first,qrzero
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,max,sign
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION ftol
C     ..
C     .. Statement Function definitions ..
      ftol(zx) = 0.5D0*max(abstol,reltol*abs(zx))
C     ..
C     .. Executable Statements ..

      IF (status.GT.0) GO TO 280
      xlo = xxlo
      xhi = xxhi
      b = xlo
      x = xlo
C     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 270

   10 fb = fx
      xlo = xhi
      a = xlo
      x = xlo
C     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 270
C
C     Check that F(ZXLO) < 0 < F(ZXHI)  or
C                F(ZXLO) > 0 > F(ZXHI)
C
   20 IF (.NOT. (fb.LT.0.0D0)) GO TO 40
      IF (.NOT. (fx.LT.0.0D0)) GO TO 30
      status = -1
      qleft = fx .LT. fb
      qhi = .FALSE.
      RETURN

   30 CONTINUE
   40 IF (.NOT. (fb.GT.0.0D0)) GO TO 60
      IF (.NOT. (fx.GT.0.0D0)) GO TO 50
      status = -1
      qleft = fx .GT. fb
      qhi = .TRUE.
      RETURN

   50 CONTINUE
   60 fa = fx
C
      first = .TRUE.
   70 c = a
      fc = fa
      ext = 0
   80 IF (.NOT. (abs(fc).LT.abs(fb))) GO TO 100
      IF (.NOT. (c.NE.a)) GO TO 90
      d = a
      fd = fa
   90 a = b
      fa = fb
      xlo = c
      b = xlo
      fb = fc
      c = a
      fc = fa
  100 tol = ftol(xlo)
      m = (c+b)*.5D0
      mb = m - b
      IF (.NOT. (abs(mb).GT.tol)) GO TO 240
      IF (.NOT. (ext.GT.3)) GO TO 110
      w = mb
      GO TO 190

  110 tol = sign(tol,mb)
      p = (b-a)*fb
      IF (.NOT. (first)) GO TO 120
      q = fa - fb
      first = .FALSE.
      GO TO 130

  120 fdb = (fd-fb)/ (d-b)
      fda = (fd-fa)/ (d-a)
      p = fda*p
      q = fdb*fa - fda*fb
  130 IF (.NOT. (p.LT.0.0D0)) GO TO 140
      p = -p
      q = -q
  140 IF (ext.EQ.3) p = p*2.0D0
      IF (.NOT. ((p*1.0D0).EQ.0.0D0.OR.p.LE. (q*tol))) GO TO 150
      w = tol
      GO TO 180

  150 IF (.NOT. (p.LT. (mb*q))) GO TO 160
      w = p/q
      GO TO 170

  160 w = mb
  170 CONTINUE
  180 CONTINUE
  190 d = a
      fd = fa
      a = b
      fa = fb
      b = b + w
      xlo = b
      x = xlo
C     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 270

  200 fb = fx
      IF (.NOT. ((fc*fb).GE.0.0D0)) GO TO 210
      GO TO 70

  210 IF (.NOT. (w.EQ.mb)) GO TO 220
      ext = 0
      GO TO 230

  220 ext = ext + 1
  230 GO TO 80

  240 xhi = c
      qrzero = (fc.GE.0.0D0 .AND. fb.LE.0.0D0) .OR.
     +         (fc.LT.0.0D0 .AND. fb.GE.0.0D0)
      IF (.NOT. (qrzero)) GO TO 250
      status = 0
      GO TO 260

  250 status = -1
  260 RETURN

      ENTRY dstzr(zxlo,zxhi,zabstl,zreltl)
C**********************************************************************
C
C     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL )
C     Double precision SeT ZeRo finder - Reverse communication version
C
C
C                              Function
C
C
C
C     Sets quantities needed by ZROR.  The function of ZROR
C     and the quantities set is given here.
C
C     Concise Description - Given a function F
C     find XLO such that F(XLO) = 0.
C
C          More Precise Description -
C
C     Input condition. F is a double precision function of a single
C     double precision argument and XLO and XHI are such that
C          F(XLO)*F(XHI)  .LE.  0.0
C
C     If the input condition is met, QRZERO returns .TRUE.
C     and output values of XLO and XHI satisfy the following
C          F(XLO)*F(XHI)  .LE. 0.
C          ABS(F(XLO)  .LE. ABS(F(XHI)
C          ABS(XLO-XHI)  .LE. TOL(X)
C     where
C          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
C
C     If this algorithm does not find XLO and XHI satisfying
C     these conditions then QRZERO returns .FALSE.  This
C     implies that the input condition was not met.
C
C
C                              Arguments
C
C
C     XLO --> The left endpoint of the interval to be
C           searched for a solution.
C                    XLO is DOUBLE PRECISION
C
C     XHI --> The right endpoint of the interval to be
C           for a solution.
C                    XHI is DOUBLE PRECISION
C
C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
C                      of the solution.  See function for a
C                      precise definition.
C                    ABSTOL is DOUBLE PRECISION
C                    RELTOL is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Algorithm R of the paper 'Two Efficient Algorithms with
C     Guaranteed Convergence for Finding a Zero of a Function'
C     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
C     Mathematical Software, Volume 1, no. 4 page 330
C     (Dec. '75) is employed to find the zero of F(X)-Y.
C
C**********************************************************************
      xxlo = zxlo
      xxhi = zxhi
      abstol = zabstl
      reltol = zreltl
      RETURN

      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
C     TO GET-FUNCTION-VALUE
  270 status = 1
      RETURN

  280 CONTINUE
      GO TO i99999

      END
      DOUBLE PRECISION FUNCTION erf(x)
C-----------------------------------------------------------------------
C             EVALUATION OF THE REAL ERROR FUNCTION
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ax,bot,c,t,top,x2
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION a(5),b(3),p(8),q(8),r(5),s(4)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp,sign
C     ..
C     .. Data statements ..
C-------------------------
C-------------------------
C-------------------------
C-------------------------
      DATA c/.564189583547756D0/
      DATA a(1)/.771058495001320D-04/,a(2)/-.133733772997339D-02/,
     +     a(3)/.323076579225834D-01/,a(4)/.479137145607681D-01/,
     +     a(5)/.128379167095513D+00/
      DATA b(1)/.301048631703895D-02/,b(2)/.538971687740286D-01/,
     +     b(3)/.375795757275549D+00/
      DATA p(1)/-1.36864857382717D-07/,p(2)/5.64195517478974D-01/,
     +     p(3)/7.21175825088309D+00/,p(4)/4.31622272220567D+01/,
     +     p(5)/1.52989285046940D+02/,p(6)/3.39320816734344D+02/,
     +     p(7)/4.51918953711873D+02/,p(8)/3.00459261020162D+02/
      DATA q(1)/1.00000000000000D+00/,q(2)/1.27827273196294D+01/,
     +     q(3)/7.70001529352295D+01/,q(4)/2.77585444743988D+02/,
     +     q(5)/6.38980264465631D+02/,q(6)/9.31354094850610D+02/,
     +     q(7)/7.90950925327898D+02/,q(8)/3.00459260956983D+02/
      DATA r(1)/2.10144126479064D+00/,r(2)/2.62370141675169D+01/,
     +     r(3)/2.13688200555087D+01/,r(4)/4.65807828718470D+00/,
     +     r(5)/2.82094791773523D-01/
      DATA s(1)/9.41537750555460D+01/,s(2)/1.87114811799590D+02/,
     +     s(3)/9.90191814623914D+01/,s(4)/1.80124575948747D+01/
C     ..
C     .. Executable Statements ..
C-------------------------
      ax = abs(x)
      IF (ax.GT.0.5D0) GO TO 10
      t = x*x
      top = ((((a(1)*t+a(2))*t+a(3))*t+a(4))*t+a(5)) + 1.0D0
      bot = ((b(1)*t+b(2))*t+b(3))*t + 1.0D0
      erf = x* (top/bot)
      RETURN
C
   10 IF (ax.GT.4.0D0) GO TO 20
      top = ((((((p(1)*ax+p(2))*ax+p(3))*ax+p(4))*ax+p(5))*ax+p(6))*ax+
     +      p(7))*ax + p(8)
      bot = ((((((q(1)*ax+q(2))*ax+q(3))*ax+q(4))*ax+q(5))*ax+q(6))*ax+
     +      q(7))*ax + q(8)
      erf = 0.5D0 + (0.5D0-exp(-x*x)*top/bot)
      IF (x.LT.0.0D0) erf = -erf
      RETURN
C
   20 IF (ax.GE.5.8D0) GO TO 30
      x2 = x*x
      t = 1.0D0/x2
      top = (((r(1)*t+r(2))*t+r(3))*t+r(4))*t + r(5)
      bot = (((s(1)*t+s(2))*t+s(3))*t+s(4))*t + 1.0D0
      erf = (c-top/ (x2*bot))/ax
      erf = 0.5D0 + (0.5D0-exp(-x2)*erf)
      IF (x.LT.0.0D0) erf = -erf
      RETURN
C
   30 erf = sign(1.0D0,x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION erfc1(ind,x)
C-----------------------------------------------------------------------
C         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
C
C          ERFC1(IND,X) = ERFC(X)            IF IND = 0
C          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
      INTEGER ind
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ax,bot,c,e,t,top,w
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION a(5),b(3),p(8),q(8),r(5),s(4)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION exparg
      EXTERNAL exparg
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,exp
C     ..
C     .. Data statements ..
C-------------------------
C-------------------------
C-------------------------
C-------------------------
      DATA c/.564189583547756D0/
      DATA a(1)/.771058495001320D-04/,a(2)/-.133733772997339D-02/,
     +     a(3)/.323076579225834D-01/,a(4)/.479137145607681D-01/,
     +     a(5)/.128379167095513D+00/
      DATA b(1)/.301048631703895D-02/,b(2)/.538971687740286D-01/,
     +     b(3)/.375795757275549D+00/
      DATA p(1)/-1.36864857382717D-07/,p(2)/5.64195517478974D-01/,
     +     p(3)/7.21175825088309D+00/,p(4)/4.31622272220567D+01/,
     +     p(5)/1.52989285046940D+02/,p(6)/3.39320816734344D+02/,
     +     p(7)/4.51918953711873D+02/,p(8)/3.00459261020162D+02/
      DATA q(1)/1.00000000000000D+00/,q(2)/1.27827273196294D+01/,
     +     q(3)/7.70001529352295D+01/,q(4)/2.77585444743988D+02/,
     +     q(5)/6.38980264465631D+02/,q(6)/9.31354094850610D+02/,
     +     q(7)/7.90950925327898D+02/,q(8)/3.00459260956983D+02/
      DATA r(1)/2.10144126479064D+00/,r(2)/2.62370141675169D+01/,
     +     r(3)/2.13688200555087D+01/,r(4)/4.65807828718470D+00/,
     +     r(5)/2.82094791773523D-01/
      DATA s(1)/9.41537750555460D+01/,s(2)/1.87114811799590D+02/,
     +     s(3)/9.90191814623914D+01/,s(4)/1.80124575948747D+01/
C     ..
C     .. Executable Statements ..
C-------------------------
C
C                     ABS(X) .LE. 0.5
C
      ax = abs(x)
      IF (ax.GT.0.5D0) GO TO 10
      t = x*x
      top = ((((a(1)*t+a(2))*t+a(3))*t+a(4))*t+a(5)) + 1.0D0
      bot = ((b(1)*t+b(2))*t+b(3))*t + 1.0D0
      erfc1 = 0.5D0 + (0.5D0-x* (top/bot))
      IF (ind.NE.0) erfc1 = exp(t)*erfc1
      RETURN
C
C                  0.5 .LT. ABS(X) .LE. 4
C
   10 IF (ax.GT.4.0D0) GO TO 20
      top = ((((((p(1)*ax+p(2))*ax+p(3))*ax+p(4))*ax+p(5))*ax+p(6))*ax+
     +      p(7))*ax + p(8)
      bot = ((((((q(1)*ax+q(2))*ax+q(3))*ax+q(4))*ax+q(5))*ax+q(6))*ax+
     +      q(7))*ax + q(8)
      erfc1 = top/bot
      GO TO 40
C
C                      ABS(X) .GT. 4
C
   20 IF (x.LE.-5.6D0) GO TO 60
      IF (ind.NE.0) GO TO 30
      IF (x.GT.100.0D0) GO TO 70
      IF (x*x.GT.-exparg(1)) GO TO 70
C
   30 t = (1.0D0/x)**2
      top = (((r(1)*t+r(2))*t+r(3))*t+r(4))*t + r(5)
      bot = (((s(1)*t+s(2))*t+s(3))*t+s(4))*t + 1.0D0
      erfc1 = (c-t*top/bot)/ax
C
C                      FINAL ASSEMBLY
C
   40 IF (ind.EQ.0) GO TO 50
      IF (x.LT.0.0D0) erfc1 = 2.0D0*exp(x*x) - erfc1
      RETURN

   50 w = dble(x)*dble(x)
      t = w
      e = w - dble(t)
      erfc1 = ((0.5D0+ (0.5D0-e))*exp(-t))*erfc1
      IF (x.LT.0.0D0) erfc1 = 2.0D0 - erfc1
      RETURN
C
C             LIMIT VALUE FOR LARGE NEGATIVE X
C
   60 erfc1 = 2.0D0
      IF (ind.NE.0) erfc1 = 2.0D0*exp(x*x)
      RETURN
C
C             LIMIT VALUE FOR LARGE POSITIVE X
C                       WHEN IND = 0
C
   70 erfc1 = 0.0D0
      RETURN

      END
      DOUBLE PRECISION FUNCTION esum(mu,x)
C-----------------------------------------------------------------------
C                    EVALUATION OF EXP(MU + X)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
      INTEGER mu
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION w
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC exp
C     ..
C     .. Executable Statements ..

      IF (x.GT.0.0D0) GO TO 10
C
      IF (mu.LT.0) GO TO 20
      w = mu + x
      IF (w.GT.0.0D0) GO TO 20
      esum = exp(w)
      RETURN
C
   10 IF (mu.GT.0) GO TO 20
      w = mu + x
      IF (w.LT.0.0D0) GO TO 20
      esum = exp(w)
      RETURN
C
   20 w = mu
      esum = exp(w)*exp(x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION exparg(l)
C--------------------------------------------------------------------
C     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
C     EXP(W) CAN BE COMPUTED.
C
C     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
C     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
C
C     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
C--------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER l
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION lnb
      INTEGER b,m
C     ..
C     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
C     ..
C     .. Executable Statements ..
C
      b = ipmpar(4)
      IF (b.NE.2) GO TO 10
      lnb = .69314718055995D0
      GO TO 40

   10 IF (b.NE.8) GO TO 20
      lnb = 2.0794415416798D0
      GO TO 40

   20 IF (b.NE.16) GO TO 30
      lnb = 2.7725887222398D0
      GO TO 40

   30 lnb = dlog(dble(b))
C
   40 IF (l.EQ.0) GO TO 50
      m = ipmpar(9) - 1
      exparg = 0.99999D0* (m*lnb)
      RETURN

   50 m = ipmpar(10)
      exparg = 0.99999D0* (m*lnb)
      RETURN

      END
      DOUBLE PRECISION FUNCTION fpser(a,b,x,eps)
C-----------------------------------------------------------------------
C
C                 EVALUATION OF I (A,B)
C                                X
C
C          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5.
C
C-----------------------------------------------------------------------
C
C                  SET  FPSER = X**A
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b,eps,x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION an,c,s,t,tol
C     ..
C     .. External Functions ..
      DOUBLE PRECISION exparg
      EXTERNAL exparg
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dlog,exp
C     ..
C     .. Executable Statements ..

      fpser = 1.0D0
      IF (a.LE.1.D-3*eps) GO TO 10
      fpser = 0.0D0
      t = a*dlog(x)
      IF (t.LT.exparg(1)) RETURN
      fpser = exp(t)
C
C                NOTE THAT 1/B(A,B) = B
C
   10 fpser = (b/a)*fpser
      tol = eps/a
      an = a + 1.0D0
      t = x
      s = t/an
   20 an = an + 1.0D0
      t = x*t
      c = t/an
      s = s + c
      IF (abs(c).GT.tol) GO TO 20
C
      fpser = fpser* (1.0D0+a*s)
      RETURN

      END
      DOUBLE PRECISION FUNCTION gam1(a)
C     ------------------------------------------------------------------
C     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 .LE. A .LE. 1.5
C     ------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION bot,d,s1,s2,t,top,w
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION p(7),q(5),r(9)
C     ..
C     .. Data statements ..
C     -------------------
C     -------------------
C     -------------------
C     -------------------
      DATA p(1)/.577215664901533D+00/,p(2)/-.409078193005776D+00/,
     +     p(3)/-.230975380857675D+00/,p(4)/.597275330452234D-01/,
     +     p(5)/.766968181649490D-02/,p(6)/-.514889771323592D-02/,
     +     p(7)/.589597428611429D-03/
      DATA q(1)/.100000000000000D+01/,q(2)/.427569613095214D+00/,
     +     q(3)/.158451672430138D+00/,q(4)/.261132021441447D-01/,
     +     q(5)/.423244297896961D-02/
      DATA r(1)/-.422784335098468D+00/,r(2)/-.771330383816272D+00/,
     +     r(3)/-.244757765222226D+00/,r(4)/.118378989872749D+00/,
     +     r(5)/.930357293360349D-03/,r(6)/-.118290993445146D-01/,
     +     r(7)/.223047661158249D-02/,r(8)/.266505979058923D-03/,
     +     r(9)/-.132674909766242D-03/
      DATA s1/.273076135303957D+00/,s2/.559398236957378D-01/
C     ..
C     .. Executable Statements ..
C     -------------------
      t = a
      d = a - 0.5D0
      IF (d.GT.0.0D0) t = d - 0.5D0
      IF (t) 40,10,20
C
   10 gam1 = 0.0D0
      RETURN
C
   20 top = (((((p(7)*t+p(6))*t+p(5))*t+p(4))*t+p(3))*t+p(2))*t + p(1)
      bot = (((q(5)*t+q(4))*t+q(3))*t+q(2))*t + 1.0D0
      w = top/bot
      IF (d.GT.0.0D0) GO TO 30
      gam1 = a*w
      RETURN

   30 gam1 = (t/a)* ((w-0.5D0)-0.5D0)
      RETURN
C
   40 top = (((((((r(9)*t+r(8))*t+r(7))*t+r(6))*t+r(5))*t+r(4))*t+r(3))*
     +      t+r(2))*t + r(1)
      bot = (s2*t+s1)*t + 1.0D0
      w = top/bot
      IF (d.GT.0.0D0) GO TO 50
      gam1 = a* ((w+0.5D0)+0.5D0)
      RETURN

   50 gam1 = t*w/a
      RETURN

      END
      SUBROUTINE gaminv(a,x,x0,p,q,ierr)
C ----------------------------------------------------------------------
C            INVERSE INCOMPLETE GAMMA RATIO FUNCTION
C
C     GIVEN POSITIVE A, AND NONEGATIVE P AND Q WHERE P + Q = 1.
C     THEN X IS COMPUTED WHERE P(A,X) = P AND Q(A,X) = Q. SCHRODER
C     ITERATION IS EMPLOYED. THE ROUTINE ATTEMPTS TO COMPUTE X
C     TO 10 SIGNIFICANT DIGITS IF THIS IS POSSIBLE FOR THE
C     PARTICULAR COMPUTER ARITHMETIC BEING USED.
C
C                      ------------
C
C     X IS A VARIABLE. IF P = 0 THEN X IS ASSIGNED THE VALUE 0,
C     AND IF Q = 0 THEN X IS SET TO THE LARGEST FLOATING POINT
C     NUMBER AVAILABLE. OTHERWISE, GAMINV ATTEMPTS TO OBTAIN
C     A SOLUTION FOR P(A,X) = P AND Q(A,X) = Q. IF THE ROUTINE
C     IS SUCCESSFUL THEN THE SOLUTION IS STORED IN X.
C
C     X0 IS AN OPTIONAL INITIAL APPROXIMATION FOR X. IF THE USER
C     DOES NOT WISH TO SUPPLY AN INITIAL APPROXIMATION, THEN SET
C     X0 .LE. 0.
C
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C     WHEN THE ROUTINE TERMINATES, IERR HAS ONE OF THE FOLLOWING
C     VALUES ...
C
C       IERR =  0    THE SOLUTION WAS OBTAINED. ITERATION WAS
C                    NOT USED.
C       IERR.GT.0    THE SOLUTION WAS OBTAINED. IERR ITERATIONS
C                    WERE PERFORMED.
C       IERR = -2    (INPUT ERROR) A .LE. 0
C       IERR = -3    NO SOLUTION WAS OBTAINED. THE RATIO Q/A
C                    IS TOO LARGE.
C       IERR = -4    (INPUT ERROR) P + Q .NE. 1
C       IERR = -6    20 ITERATIONS WERE PERFORMED. THE MOST
C                    RECENT VALUE OBTAINED FOR X IS GIVEN.
C                    THIS CANNOT OCCUR IF X0 .LE. 0.
C       IERR = -7    ITERATION FAILED. NO VALUE IS GIVEN FOR X.
C                    THIS MAY OCCUR WHEN X IS APPROXIMATELY 0.
C       IERR = -8    A VALUE FOR X HAS BEEN OBTAINED, BUT THE
C                    ROUTINE IS NOT CERTAIN OF ITS ACCURACY.
C                    ITERATION CANNOT BE PERFORMED IN THIS
C                    CASE. IF X0 .LE. 0, THIS CAN OCCUR ONLY
C                    WHEN P OR Q IS APPROXIMATELY 0. IF X0 IS
C                    POSITIVE THEN THIS CAN OCCUR WHEN A IS
C                    EXCEEDINGLY CLOSE TO X AND A IS EXTREMELY
C                    LARGE (SAY A .GE. 1.E20).
C ----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WEAPONS CENTER
C        DAHLGREN, VIRGINIA
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,p,q,x,x0
      INTEGER ierr
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a0,a1,a2,a3,am1,amax,ap1,ap2,ap3,apn,b,b1,b2,b3,
     +                 b4,c,c1,c2,c3,c4,c5,d,e,e2,eps,g,h,ln10,pn,qg,qn,
     +                 r,rta,s,s2,sum,t,tol,u,w,xmax,xmin,xn,y,z
      INTEGER iop
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION amin(2),bmin(2),dmin(2),emin(2),eps0(2)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alnrel,gamln,gamln1,gamma,rcomp,spmpar
      EXTERNAL alnrel,gamln,gamln1,gamma,rcomp,spmpar
C     ..
C     .. External Subroutines ..
      EXTERNAL gratio
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,exp,sqrt
C     ..
C     .. Data statements ..
C     -------------------
C     LN10 = LN(10)
C     C = EULER CONSTANT
C     -------------------
C     -------------------
C     -------------------
C     -------------------
      DATA ln10/2.302585D0/
      DATA c/.577215664901533D0/
      DATA a0/3.31125922108741D0/,a1/11.6616720288968D0/,
     +     a2/4.28342155967104D0/,a3/.213623493715853D0/
      DATA b1/6.61053765625462D0/,b2/6.40691597760039D0/,
     +     b3/1.27364489782223D0/,b4/.036117081018842D0/
      DATA eps0(1)/1.D-10/,eps0(2)/1.D-08/
      DATA amin(1)/500.0D0/,amin(2)/100.0D0/
      DATA bmin(1)/1.D-28/,bmin(2)/1.D-13/
      DATA dmin(1)/1.D-06/,dmin(2)/1.D-04/
      DATA emin(1)/2.D-03/,emin(2)/6.D-03/
      DATA tol/1.D-5/
C     ..
C     .. Executable Statements ..
C     -------------------
C     ****** E, XMIN, AND XMAX ARE MACHINE DEPENDENT CONSTANTS.
C            E IS THE SMALLEST NUMBER FOR WHICH 1.0 + E .GT. 1.0.
C            XMIN IS THE SMALLEST POSITIVE NUMBER AND XMAX IS THE
C            LARGEST POSITIVE NUMBER.
C
      e = spmpar(1)
      xmin = spmpar(2)
      xmax = spmpar(3)
C     -------------------
      x = 0.0D0
      IF (a.LE.0.0D0) GO TO 300
      t = dble(p) + dble(q) - 1.D0
      IF (abs(t).GT.e) GO TO 320
C
      ierr = 0
      IF (p.EQ.0.0D0) RETURN
      IF (q.EQ.0.0D0) GO TO 270
      IF (a.EQ.1.0D0) GO TO 280
C
      e2 = 2.0D0*e
      amax = 0.4D-10/ (e*e)
      iop = 1
      IF (e.GT.1.D-10) iop = 2
      eps = eps0(iop)
      xn = x0
      IF (x0.GT.0.0D0) GO TO 160
C
C        SELECTION OF THE INITIAL APPROXIMATION XN OF X
C                       WHEN A .LT. 1
C
      IF (a.GT.1.0D0) GO TO 80
      g = gamma(a+1.0D0)
      qg = q*g
      IF (qg.EQ.0.0D0) GO TO 360
      b = qg/a
      IF (qg.GT.0.6D0*a) GO TO 40
      IF (a.GE.0.30D0 .OR. b.LT.0.35D0) GO TO 10
      t = exp(- (b+c))
      u = t*exp(t)
      xn = t*exp(u)
      GO TO 160
C
   10 IF (b.GE.0.45D0) GO TO 40
      IF (b.EQ.0.0D0) GO TO 360
      y = -dlog(b)
      s = 0.5D0 + (0.5D0-a)
      z = dlog(y)
      t = y - s*z
      IF (b.LT.0.15D0) GO TO 20
      xn = y - s*dlog(t) - dlog(1.0D0+s/ (t+1.0D0))
      GO TO 220

   20 IF (b.LE.0.01D0) GO TO 30
      u = ((t+2.0D0* (3.0D0-a))*t+ (2.0D0-a)* (3.0D0-a))/
     +    ((t+ (5.0D0-a))*t+2.0D0)
      xn = y - s*dlog(t) - dlog(u)
      GO TO 220

   30 c1 = -s*z
      c2 = -s* (1.0D0+c1)
      c3 = s* ((0.5D0*c1+ (2.0D0-a))*c1+ (2.5D0-1.5D0*a))
      c4 = -s* (((c1/3.0D0+ (2.5D0-1.5D0*a))*c1+ ((a-6.0D0)*a+7.0D0))*
     +     c1+ ((11.0D0*a-46)*a+47.0D0)/6.0D0)
      c5 = -s* ((((-c1/4.0D0+ (11.0D0*a-17.0D0)/6.0D0)*c1+ ((-3.0D0*a+
     +     13.0D0)*a-13.0D0))*c1+0.5D0* (((2.0D0*a-25.0D0)*a+72.0D0)*a-
     +     61.0D0))*c1+ (((25.0D0*a-195.0D0)*a+477.0D0)*a-379.0D0)/
     +     12.0D0)
      xn = ((((c5/y+c4)/y+c3)/y+c2)/y+c1) + y
      IF (a.GT.1.0D0) GO TO 220
      IF (b.GT.bmin(iop)) GO TO 220
      x = xn
      RETURN
C
   40 IF (b*q.GT.1.D-8) GO TO 50
      xn = exp(- (q/a+c))
      GO TO 70

   50 IF (p.LE.0.9D0) GO TO 60
      xn = exp((alnrel(-q)+gamln1(a))/a)
      GO TO 70

   60 xn = exp(dlog(p*g)/a)
   70 IF (xn.EQ.0.0D0) GO TO 310
      t = 0.5D0 + (0.5D0-xn/ (a+1.0D0))
      xn = xn/t
      GO TO 160
C
C        SELECTION OF THE INITIAL APPROXIMATION XN OF X
C                       WHEN A .GT. 1
C
   80 IF (q.LE.0.5D0) GO TO 90
      w = dlog(p)
      GO TO 100

   90 w = dlog(q)
  100 t = sqrt(-2.0D0*w)
      s = t - (((a3*t+a2)*t+a1)*t+a0)/ ((((b4*t+b3)*t+b2)*t+b1)*t+1.0D0)
      IF (q.GT.0.5D0) s = -s
C
      rta = sqrt(a)
      s2 = s*s
      xn = a + s*rta + (s2-1.0D0)/3.0D0 + s* (s2-7.0D0)/ (36.0D0*rta) -
     +     ((3.0D0*s2+7.0D0)*s2-16.0D0)/ (810.0D0*a) +
     +     s* ((9.0D0*s2+256.0D0)*s2-433.0D0)/ (38880.0D0*a*rta)
      xn = dmax1(xn,0.0D0)
      IF (a.LT.amin(iop)) GO TO 110
      x = xn
      d = 0.5D0 + (0.5D0-x/a)
      IF (abs(d).LE.dmin(iop)) RETURN
C
  110 IF (p.LE.0.5D0) GO TO 130
      IF (xn.LT.3.0D0*a) GO TO 220
      y = - (w+gamln(a))
      d = dmax1(2.0D0,a* (a-1.0D0))
      IF (y.LT.ln10*d) GO TO 120
      s = 1.0D0 - a
      z = dlog(y)
      GO TO 30

  120 t = a - 1.0D0
      xn = y + t*dlog(xn) - alnrel(-t/ (xn+1.0D0))
      xn = y + t*dlog(xn) - alnrel(-t/ (xn+1.0D0))
      GO TO 220
C
  130 ap1 = a + 1.0D0
      IF (xn.GT.0.70D0*ap1) GO TO 170
      w = w + gamln(ap1)
      IF (xn.GT.0.15D0*ap1) GO TO 140
      ap2 = a + 2.0D0
      ap3 = a + 3.0D0
      x = exp((w+x)/a)
      x = exp((w+x-dlog(1.0D0+ (x/ap1)* (1.0D0+x/ap2)))/a)
      x = exp((w+x-dlog(1.0D0+ (x/ap1)* (1.0D0+x/ap2)))/a)
      x = exp((w+x-dlog(1.0D0+ (x/ap1)* (1.0D0+ (x/ap2)* (1.0D0+
     +    x/ap3))))/a)
      xn = x
      IF (xn.GT.1.D-2*ap1) GO TO 140
      IF (xn.LE.emin(iop)*ap1) RETURN
      GO TO 170
C
  140 apn = ap1
      t = xn/apn
      sum = 1.0D0 + t
  150 apn = apn + 1.0D0
      t = t* (xn/apn)
      sum = sum + t
      IF (t.GT.1.D-4) GO TO 150
      t = w - dlog(sum)
      xn = exp((xn+t)/a)
      xn = xn* (1.0D0- (a*dlog(xn)-xn-t)/ (a-xn))
      GO TO 170
C
C                 SCHRODER ITERATION USING P
C
  160 IF (p.GT.0.5D0) GO TO 220
  170 IF (p.LE.1.D10*xmin) GO TO 350
      am1 = (a-0.5D0) - 0.5D0
  180 IF (a.LE.amax) GO TO 190
      d = 0.5D0 + (0.5D0-xn/a)
      IF (abs(d).LE.e2) GO TO 350
C
  190 IF (ierr.GE.20) GO TO 330
      ierr = ierr + 1
      CALL gratio(a,xn,pn,qn,0)
      IF (pn.EQ.0.0D0 .OR. qn.EQ.0.0D0) GO TO 350
      r = rcomp(a,xn)
      IF (r.EQ.0.0D0) GO TO 350
      t = (pn-p)/r
      w = 0.5D0* (am1-xn)
      IF (abs(t).LE.0.1D0 .AND. abs(w*t).LE.0.1D0) GO TO 200
      x = xn* (1.0D0-t)
      IF (x.LE.0.0D0) GO TO 340
      d = abs(t)
      GO TO 210
C
  200 h = t* (1.0D0+w*t)
      x = xn* (1.0D0-h)
      IF (x.LE.0.0D0) GO TO 340
      IF (abs(w).GE.1.0D0 .AND. abs(w)*t*t.LE.eps) RETURN
      d = abs(h)
  210 xn = x
      IF (d.GT.tol) GO TO 180
      IF (d.LE.eps) RETURN
      IF (abs(p-pn).LE.tol*p) RETURN
      GO TO 180
C
C                 SCHRODER ITERATION USING Q
C
  220 IF (q.LE.1.D10*xmin) GO TO 350
      am1 = (a-0.5D0) - 0.5D0
  230 IF (a.LE.amax) GO TO 240
      d = 0.5D0 + (0.5D0-xn/a)
      IF (abs(d).LE.e2) GO TO 350
C
  240 IF (ierr.GE.20) GO TO 330
      ierr = ierr + 1
      CALL gratio(a,xn,pn,qn,0)
      IF (pn.EQ.0.0D0 .OR. qn.EQ.0.0D0) GO TO 350
      r = rcomp(a,xn)
      IF (r.EQ.0.0D0) GO TO 350
      t = (q-qn)/r
      w = 0.5D0* (am1-xn)
      IF (abs(t).LE.0.1D0 .AND. abs(w*t).LE.0.1D0) GO TO 250
      x = xn* (1.0D0-t)
      IF (x.LE.0.0D0) GO TO 340
      d = abs(t)
      GO TO 260
C
  250 h = t* (1.0D0+w*t)
      x = xn* (1.0D0-h)
      IF (x.LE.0.0D0) GO TO 340
      IF (abs(w).GE.1.0D0 .AND. abs(w)*t*t.LE.eps) RETURN
      d = abs(h)
  260 xn = x
      IF (d.GT.tol) GO TO 230
      IF (d.LE.eps) RETURN
      IF (abs(q-qn).LE.tol*q) RETURN
      GO TO 230
C
C                       SPECIAL CASES
C
  270 x = xmax
      RETURN
C
  280 IF (q.LT.0.9D0) GO TO 290
      x = -alnrel(-p)
      RETURN

  290 x = -dlog(q)
      RETURN
C
C                       ERROR RETURN
C
  300 ierr = -2
      RETURN
C
  310 ierr = -3
      RETURN
C
  320 ierr = -4
      RETURN
C
  330 ierr = -6
      RETURN
C
  340 ierr = -7
      RETURN
C
  350 x = xn
      ierr = -8
      RETURN
C
  360 x = xmax
      ierr = -8
      RETURN

      END
      DOUBLE PRECISION FUNCTION gamln(a)
C-----------------------------------------------------------------------
C            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS
C          NAVAL SURFACE WARFARE CENTER
C          DAHLGREN, VIRGINIA
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION c0,c1,c2,c3,c4,c5,d,t,w
      INTEGER i,n
C     ..
C     .. External Functions ..
      DOUBLE PRECISION gamln1
      EXTERNAL gamln1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog
C     ..
C     .. Data statements ..
C--------------------------
      DATA d/.418938533204673D0/
      DATA c0/.833333333333333D-01/,c1/-.277777777760991D-02/,
     +     c2/.793650666825390D-03/,c3/-.595202931351870D-03/,
     +     c4/.837308034031215D-03/,c5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
      IF (a.GT.0.8D0) GO TO 10
      gamln = gamln1(a) - dlog(a)
      RETURN

   10 IF (a.GT.2.25D0) GO TO 20
      t = (a-0.5D0) - 0.5D0
      gamln = gamln1(t)
      RETURN
C
   20 IF (a.GE.10.0D0) GO TO 40
      n = a - 1.25D0
      t = a
      w = 1.0D0
      DO 30 i = 1,n
          t = t - 1.0D0
          w = t*w
   30 CONTINUE
      gamln = gamln1(t-1.0D0) + dlog(w)
      RETURN
C
   40 t = (1.0D0/a)**2
      w = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/a
      gamln = (d+w) + (a-0.5D0)* (dlog(a)-1.0D0)
      END
      DOUBLE PRECISION FUNCTION gamln1(a)
C-----------------------------------------------------------------------
C     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 .LE. A .LE. 1.25
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION p0,p1,p2,p3,p4,p5,p6,q1,q2,q3,q4,q5,q6,r0,r1,r2,
     +                 r3,r4,r5,s1,s2,s3,s4,s5,w,x
C     ..
C     .. Data statements ..
C----------------------
      DATA p0/.577215664901533D+00/,p1/.844203922187225D+00/,
     +     p2/-.168860593646662D+00/,p3/-.780427615533591D+00/,
     +     p4/-.402055799310489D+00/,p5/-.673562214325671D-01/,
     +     p6/-.271935708322958D-02/
      DATA q1/.288743195473681D+01/,q2/.312755088914843D+01/,
     +     q3/.156875193295039D+01/,q4/.361951990101499D+00/,
     +     q5/.325038868253937D-01/,q6/.667465618796164D-03/
      DATA r0/.422784335098467D+00/,r1/.848044614534529D+00/,
     +     r2/.565221050691933D+00/,r3/.156513060486551D+00/,
     +     r4/.170502484022650D-01/,r5/.497958207639485D-03/
      DATA s1/.124313399877507D+01/,s2/.548042109832463D+00/,
     +     s3/.101552187439830D+00/,s4/.713309612391000D-02/,
     +     s5/.116165475989616D-03/
C     ..
C     .. Executable Statements ..
C----------------------
      IF (a.GE.0.6D0) GO TO 10
      w = ((((((p6*a+p5)*a+p4)*a+p3)*a+p2)*a+p1)*a+p0)/
     +    ((((((q6*a+q5)*a+q4)*a+q3)*a+q2)*a+q1)*a+1.0D0)
      gamln1 = -a*w
      RETURN
C
   10 x = (a-0.5D0) - 0.5D0
      w = (((((r5*x+r4)*x+r3)*x+r2)*x+r1)*x+r0)/
     +    (((((s5*x+s4)*x+s3)*x+s2)*x+s1)*x+1.0D0)
      gamln1 = x*w
      RETURN

      END
      DOUBLE PRECISION FUNCTION gamma(a)
C-----------------------------------------------------------------------
C
C         EVALUATION OF THE GAMMA FUNCTION FOR REAL ARGUMENTS
C
C                           -----------
C
C     GAMMA(A) IS ASSIGNED THE VALUE 0 WHEN THE GAMMA FUNCTION CANNOT
C     BE COMPUTED.
C
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C          NAVAL SURFACE WEAPONS CENTER
C          DAHLGREN, VIRGINIA
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION bot,d,g,lnx,pi,r1,r2,r3,r4,r5,s,t,top,w,x,z
      INTEGER i,j,m,n
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION p(7),q(7)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION exparg,spmpar
      EXTERNAL exparg,spmpar
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,exp,int,mod,sin
C     ..
C     .. Data statements ..
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C--------------------------
C--------------------------
      DATA pi/3.1415926535898D0/
      DATA d/.41893853320467274178D0/
      DATA p(1)/.539637273585445D-03/,p(2)/.261939260042690D-02/,
     +     p(3)/.204493667594920D-01/,p(4)/.730981088720487D-01/,
     +     p(5)/.279648642639792D+00/,p(6)/.553413866010467D+00/,
     +     p(7)/1.0D0/
      DATA q(1)/-.832979206704073D-03/,q(2)/.470059485860584D-02/,
     +     q(3)/.225211131035340D-01/,q(4)/-.170458969313360D+00/,
     +     q(5)/-.567902761974940D-01/,q(6)/.113062953091122D+01/,
     +     q(7)/1.0D0/
      DATA r1/.820756370353826D-03/,r2/-.595156336428591D-03/,
     +     r3/.793650663183693D-03/,r4/-.277777777770481D-02/,
     +     r5/.833333333333333D-01/
C     ..
C     .. Executable Statements ..
C--------------------------
      gamma = 0.0D0
      x = a
      IF (abs(a).GE.15.0D0) GO TO 110
C-----------------------------------------------------------------------
C            EVALUATION OF GAMMA(A) FOR ABS(A) .LT. 15
C-----------------------------------------------------------------------
      t = 1.0D0
      m = int(a) - 1
C
C     LET T BE THE PRODUCT OF A-J WHEN A .GE. 2
C
      IF (m) 40,30,10
   10 DO 20 j = 1,m
          x = x - 1.0D0
          t = x*t
   20 CONTINUE
   30 x = x - 1.0D0
      GO TO 80
C
C     LET T BE THE PRODUCT OF A+J WHEN A .LT. 1
C
   40 t = a
      IF (a.GT.0.0D0) GO TO 70
      m = -m - 1
      IF (m.EQ.0) GO TO 60
      DO 50 j = 1,m
          x = x + 1.0D0
          t = x*t
   50 CONTINUE
   60 x = (x+0.5D0) + 0.5D0
      t = x*t
      IF (t.EQ.0.0D0) RETURN
C
   70 CONTINUE
C
C     THE FOLLOWING CODE CHECKS IF 1/T CAN OVERFLOW. THIS
C     CODE MAY BE OMITTED IF DESIRED.
C
      IF (abs(t).GE.1.D-30) GO TO 80
      IF (abs(t)*spmpar(3).LE.1.0001D0) RETURN
      gamma = 1.0D0/t
      RETURN
C
C     COMPUTE GAMMA(1 + X) FOR  0 .LE. X .LT. 1
C
   80 top = p(1)
      bot = q(1)
      DO 90 i = 2,7
          top = p(i) + x*top
          bot = q(i) + x*bot
   90 CONTINUE
      gamma = top/bot
C
C     TERMINATION
C
      IF (a.LT.1.0D0) GO TO 100
      gamma = gamma*t
      RETURN

  100 gamma = gamma/t
      RETURN
C-----------------------------------------------------------------------
C            EVALUATION OF GAMMA(A) FOR ABS(A) .GE. 15
C-----------------------------------------------------------------------
  110 IF (abs(a).GE.1.D3) RETURN
      IF (a.GT.0.0D0) GO TO 120
      x = -a
      n = x
      t = x - n
      IF (t.GT.0.9D0) t = 1.0D0 - t
      s = sin(pi*t)/pi
      IF (mod(n,2).EQ.0) s = -s
      IF (s.EQ.0.0D0) RETURN
C
C     COMPUTE THE MODIFIED ASYMPTOTIC SUM
C
  120 t = 1.0D0/ (x*x)
      g = ((((r1*t+r2)*t+r3)*t+r4)*t+r5)/x
C
C     ONE MAY REPLACE THE NEXT STATEMENT WITH  LNX = ALOG(X)
C     BUT LESS ACCURACY WILL NORMALLY BE OBTAINED.
C
      lnx = dlog(x)
C
C     FINAL ASSEMBLY
C
      z = x
      g = (d+g) + (z-0.5D0)* (lnx-1.D0)
      w = g
      t = g - dble(w)
      IF (w.GT.0.99999D0*exparg(0)) RETURN
      gamma = exp(w)* (1.0D0+t)
      IF (a.LT.0.0D0) gamma = (1.0D0/ (gamma*s))/x
      RETURN

      END
      SUBROUTINE grat1(a,x,r,p,q,eps)
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,eps,p,q,r,x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a2n,a2nm1,am0,an,an0,b2n,b2nm1,c,cma,g,h,j,l,sum,
     +                 t,tol,w,z
C     ..
C     .. External Functions ..
      DOUBLE PRECISION erf,erfc1,gam1,rexp
      EXTERNAL erf,erfc1,gam1,rexp
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dlog,exp,sqrt
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
C                      P(A,X) AND Q(A,X)
C
C     IT IS ASSUMED THAT A .LE. 1.  EPS IS THE TOLERANCE TO BE USED.
C     THE INPUT ARGUMENT R HAS THE VALUE E**(-X)*X**A/GAMMA(A).
C-----------------------------------------------------------------------
      IF (a*x.EQ.0.0D0) GO TO 120
      IF (a.EQ.0.5D0) GO TO 100
      IF (x.LT.1.1D0) GO TO 10
      GO TO 60
C
C             TAYLOR SERIES FOR P(A,X)/X**A
C
   10 an = 3.0D0
      c = x
      sum = x/ (a+3.0D0)
      tol = 0.1D0*eps/ (a+1.0D0)
   20 an = an + 1.0D0
      c = -c* (x/an)
      t = c/ (a+an)
      sum = sum + t
      IF (abs(t).GT.tol) GO TO 20
      j = a*x* ((sum/6.0D0-0.5D0/ (a+2.0D0))*x+1.0D0/ (a+1.0D0))
C
      z = a*dlog(x)
      h = gam1(a)
      g = 1.0D0 + h
      IF (x.LT.0.25D0) GO TO 30
      IF (a.LT.x/2.59D0) GO TO 50
      GO TO 40

   30 IF (z.GT.-.13394D0) GO TO 50
C
   40 w = exp(z)
      p = w*g* (0.5D0+ (0.5D0-j))
      q = 0.5D0 + (0.5D0-p)
      RETURN
C
   50 l = rexp(z)
      w = 0.5D0 + (0.5D0+l)
      q = (w*j-l)*g - h
      IF (q.LT.0.0D0) GO TO 90
      p = 0.5D0 + (0.5D0-q)
      RETURN
C
C              CONTINUED FRACTION EXPANSION
C
   60 a2nm1 = 1.0D0
      a2n = 1.0D0
      b2nm1 = x
      b2n = x + (1.0D0-a)
      c = 1.0D0
   70 a2nm1 = x*a2n + c*a2nm1
      b2nm1 = x*b2n + c*b2nm1
      am0 = a2nm1/b2nm1
      c = c + 1.0D0
      cma = c - a
      a2n = a2nm1 + cma*a2n
      b2n = b2nm1 + cma*b2n
      an0 = a2n/b2n
      IF (abs(an0-am0).GE.eps*an0) GO TO 70
      q = r*an0
      p = 0.5D0 + (0.5D0-q)
      RETURN
C
C                SPECIAL CASES
C
   80 p = 0.0D0
      q = 1.0D0
      RETURN
C
   90 p = 1.0D0
      q = 0.0D0
      RETURN
C
  100 IF (x.GE.0.25D0) GO TO 110
      p = erf(sqrt(x))
      q = 0.5D0 + (0.5D0-p)
      RETURN

  110 q = erfc1(0,sqrt(x))
      p = 0.5D0 + (0.5D0-q)
      RETURN
C
  120 IF (x.LE.a) GO TO 80
      GO TO 90

      END
      SUBROUTINE gratio(a,x,ans,qans,ind)
C ----------------------------------------------------------------------
C        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
C                      P(A,X) AND Q(A,X)
C
C                        ----------
C
C     IT IS ASSUMED THAT A AND X ARE NONNEGATIVE, WHERE A AND X
C     ARE NOT BOTH 0.
C
C     ANS AND QANS ARE VARIABLES. GRATIO ASSIGNS ANS THE VALUE
C     P(A,X) AND QANS THE VALUE Q(A,X). IND MAY BE ANY INTEGER.
C     IF IND = 0 THEN THE USER IS REQUESTING AS MUCH ACCURACY AS
C     POSSIBLE (UP TO 14 SIGNIFICANT DIGITS). OTHERWISE, IF
C     IND = 1 THEN ACCURACY IS REQUESTED TO WITHIN 1 UNIT OF THE
C     6-TH SIGNIFICANT DIGIT, AND IF IND .NE. 0,1 THEN ACCURACY
C     IS REQUESTED TO WITHIN 1 UNIT OF THE 3RD SIGNIFICANT DIGIT.
C
C     ERROR RETURN ...
C        ANS IS ASSIGNED THE VALUE 2 WHEN A OR X IS NEGATIVE,
C     WHEN A*X = 0, OR WHEN P(A,X) AND Q(A,X) ARE INDETERMINANT.
C     P(A,X) AND Q(A,X) ARE COMPUTATIONALLY INDETERMINANT WHEN
C     X IS EXCEEDINGLY CLOSE TO A AND A IS EXTREMELY LARGE.
C ----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WEAPONS CENTER
C        DAHLGREN, VIRGINIA
C     --------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,ans,qans,x
      INTEGER ind
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a2n,a2nm1,acc,alog10,am0,amn,an,an0,apn,b2n,
     +                 b2nm1,c,c0,c1,c2,c3,c4,c5,c6,cma,d10,d20,d30,d40,
     +                 d50,d60,d70,e,e0,g,h,j,l,r,rt2pin,rta,rtpi,rtx,s,
     +                 sum,t,t1,third,tol,twoa,u,w,x0,y,z
      INTEGER i,iop,m,max,n
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION acc0(3),big(3),d0(13),d1(12),d2(10),d3(8),d4(6),
     +                 d5(4),d6(2),e00(3),wk(20),x00(3)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION erf,erfc1,gam1,gamma,rexp,rlog,spmpar
      EXTERNAL erf,erfc1,gam1,gamma,rexp,rlog,spmpar
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,exp,int,sqrt
C     ..
C     .. Data statements ..
C     --------------------
C     --------------------
C     ALOG10 = LN(10)
C     RT2PIN = 1/SQRT(2*PI)
C     RTPI   = SQRT(PI)
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
      DATA acc0(1)/5.D-15/,acc0(2)/5.D-7/,acc0(3)/5.D-4/
      DATA big(1)/20.0D0/,big(2)/14.0D0/,big(3)/10.0D0/
      DATA e00(1)/.25D-3/,e00(2)/.25D-1/,e00(3)/.14D0/
      DATA x00(1)/31.0D0/,x00(2)/17.0D0/,x00(3)/9.7D0/
      DATA alog10/2.30258509299405D0/
      DATA rt2pin/.398942280401433D0/
      DATA rtpi/1.77245385090552D0/
      DATA third/.333333333333333D0/
      DATA d0(1)/.833333333333333D-01/,d0(2)/-.148148148148148D-01/,
     +     d0(3)/.115740740740741D-02/,d0(4)/.352733686067019D-03/,
     +     d0(5)/-.178755144032922D-03/,d0(6)/.391926317852244D-04/,
     +     d0(7)/-.218544851067999D-05/,d0(8)/-.185406221071516D-05/,
     +     d0(9)/.829671134095309D-06/,d0(10)/-.176659527368261D-06/,
     +     d0(11)/.670785354340150D-08/,d0(12)/.102618097842403D-07/,
     +     d0(13)/-.438203601845335D-08/
      DATA d10/-.185185185185185D-02/,d1(1)/-.347222222222222D-02/,
     +     d1(2)/.264550264550265D-02/,d1(3)/-.990226337448560D-03/,
     +     d1(4)/.205761316872428D-03/,d1(5)/-.401877572016461D-06/,
     +     d1(6)/-.180985503344900D-04/,d1(7)/.764916091608111D-05/,
     +     d1(8)/-.161209008945634D-05/,d1(9)/.464712780280743D-08/,
     +     d1(10)/.137863344691572D-06/,d1(11)/-.575254560351770D-07/,
     +     d1(12)/.119516285997781D-07/
      DATA d20/.413359788359788D-02/,d2(1)/-.268132716049383D-02/,
     +     d2(2)/.771604938271605D-03/,d2(3)/.200938786008230D-05/,
     +     d2(4)/-.107366532263652D-03/,d2(5)/.529234488291201D-04/,
     +     d2(6)/-.127606351886187D-04/,d2(7)/.342357873409614D-07/,
     +     d2(8)/.137219573090629D-05/,d2(9)/-.629899213838006D-06/,
     +     d2(10)/.142806142060642D-06/
      DATA d30/.649434156378601D-03/,d3(1)/.229472093621399D-03/,
     +     d3(2)/-.469189494395256D-03/,d3(3)/.267720632062839D-03/,
     +     d3(4)/-.756180167188398D-04/,d3(5)/-.239650511386730D-06/,
     +     d3(6)/.110826541153473D-04/,d3(7)/-.567495282699160D-05/,
     +     d3(8)/.142309007324359D-05/
      DATA d40/-.861888290916712D-03/,d4(1)/.784039221720067D-03/,
     +     d4(2)/-.299072480303190D-03/,d4(3)/-.146384525788434D-05/,
     +     d4(4)/.664149821546512D-04/,d4(5)/-.396836504717943D-04/,
     +     d4(6)/.113757269706784D-04/
      DATA d50/-.336798553366358D-03/,d5(1)/-.697281375836586D-04/,
     +     d5(2)/.277275324495939D-03/,d5(3)/-.199325705161888D-03/,
     +     d5(4)/.679778047793721D-04/
      DATA d60/.531307936463992D-03/,d6(1)/-.592166437353694D-03/,
     +     d6(2)/.270878209671804D-03/
      DATA d70/.344367606892378D-03/
C     ..
C     .. Executable Statements ..
C     --------------------
C     ****** E IS A MACHINE DEPENDENT CONSTANT. E IS THE SMALLEST
C            FLOATING POINT NUMBER FOR WHICH 1.0 + E .GT. 1.0 .
C
      e = spmpar(1)
C
C     --------------------
      IF (a.LT.0.0D0 .OR. x.LT.0.0D0) GO TO 430
      IF (a.EQ.0.0D0 .AND. x.EQ.0.0D0) GO TO 430
      IF (a*x.EQ.0.0D0) GO TO 420
C
      iop = ind + 1
      IF (iop.NE.1 .AND. iop.NE.2) iop = 3
      acc = dmax1(acc0(iop),e)
      e0 = e00(iop)
      x0 = x00(iop)
C
C            SELECT THE APPROPRIATE ALGORITHM
C
      IF (a.GE.1.0D0) GO TO 10
      IF (a.EQ.0.5D0) GO TO 390
      IF (x.LT.1.1D0) GO TO 160
      t1 = a*dlog(x) - x
      u = a*exp(t1)
      IF (u.EQ.0.0D0) GO TO 380
      r = u* (1.0D0+gam1(a))
      GO TO 250
C
   10 IF (a.GE.big(iop)) GO TO 30
      IF (a.GT.x .OR. x.GE.x0) GO TO 20
      twoa = a + a
      m = int(twoa)
      IF (twoa.NE.dble(m)) GO TO 20
      i = m/2
      IF (a.EQ.dble(i)) GO TO 210
      GO TO 220

   20 t1 = a*dlog(x) - x
      r = exp(t1)/gamma(a)
      GO TO 40
C
   30 l = x/a
      IF (l.EQ.0.0D0) GO TO 370
      s = 0.5D0 + (0.5D0-l)
      z = rlog(l)
      IF (z.GE.700.0D0/a) GO TO 410
      y = a*z
      rta = sqrt(a)
      IF (abs(s).LE.e0/rta) GO TO 330
      IF (abs(s).LE.0.4D0) GO TO 270
C
      t = (1.0D0/a)**2
      t1 = (((0.75D0*t-1.0D0)*t+3.5D0)*t-105.0D0)/ (a*1260.0D0)
      t1 = t1 - y
      r = rt2pin*rta*exp(t1)
C
   40 IF (r.EQ.0.0D0) GO TO 420
      IF (x.LE.dmax1(a,alog10)) GO TO 50
      IF (x.LT.x0) GO TO 250
      GO TO 100
C
C                 TAYLOR SERIES FOR P/R
C
   50 apn = a + 1.0D0
      t = x/apn
      wk(1) = t
      DO 60 n = 2,20
          apn = apn + 1.0D0
          t = t* (x/apn)
          IF (t.LE.1.D-3) GO TO 70
          wk(n) = t
   60 CONTINUE
      n = 20
C
   70 sum = t
      tol = 0.5D0*acc
   80 apn = apn + 1.0D0
      t = t* (x/apn)
      sum = sum + t
      IF (t.GT.tol) GO TO 80
C
      max = n - 1
      DO 90 m = 1,max
          n = n - 1
          sum = sum + wk(n)
   90 CONTINUE
      ans = (r/a)* (1.0D0+sum)
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
C
C                 ASYMPTOTIC EXPANSION
C
  100 amn = a - 1.0D0
      t = amn/x
      wk(1) = t
      DO 110 n = 2,20
          amn = amn - 1.0D0
          t = t* (amn/x)
          IF (abs(t).LE.1.D-3) GO TO 120
          wk(n) = t
  110 CONTINUE
      n = 20
C
  120 sum = t
  130 IF (abs(t).LE.acc) GO TO 140
      amn = amn - 1.0D0
      t = t* (amn/x)
      sum = sum + t
      GO TO 130
C
  140 max = n - 1
      DO 150 m = 1,max
          n = n - 1
          sum = sum + wk(n)
  150 CONTINUE
      qans = (r/x)* (1.0D0+sum)
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
C
C             TAYLOR SERIES FOR P(A,X)/X**A
C
  160 an = 3.0D0
      c = x
      sum = x/ (a+3.0D0)
      tol = 3.0D0*acc/ (a+1.0D0)
  170 an = an + 1.0D0
      c = -c* (x/an)
      t = c/ (a+an)
      sum = sum + t
      IF (abs(t).GT.tol) GO TO 170
      j = a*x* ((sum/6.0D0-0.5D0/ (a+2.0D0))*x+1.0D0/ (a+1.0D0))
C
      z = a*dlog(x)
      h = gam1(a)
      g = 1.0D0 + h
      IF (x.LT.0.25D0) GO TO 180
      IF (a.LT.x/2.59D0) GO TO 200
      GO TO 190

  180 IF (z.GT.-.13394D0) GO TO 200
C
  190 w = exp(z)
      ans = w*g* (0.5D0+ (0.5D0-j))
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
C
  200 l = rexp(z)
      w = 0.5D0 + (0.5D0+l)
      qans = (w*j-l)*g - h
      IF (qans.LT.0.0D0) GO TO 380
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
C
C             FINITE SUMS FOR Q WHEN A .GE. 1
C                 AND 2*A IS AN INTEGER
C
  210 sum = exp(-x)
      t = sum
      n = 1
      c = 0.0D0
      GO TO 230
C
  220 rtx = sqrt(x)
      sum = erfc1(0,rtx)
      t = exp(-x)/ (rtpi*rtx)
      n = 0
      c = -0.5D0
C
  230 IF (n.EQ.i) GO TO 240
      n = n + 1
      c = c + 1.0D0
      t = (x*t)/c
      sum = sum + t
      GO TO 230

  240 qans = sum
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
C
C              CONTINUED FRACTION EXPANSION
C
  250 tol = dmax1(5.0D0*e,acc)
      a2nm1 = 1.0D0
      a2n = 1.0D0
      b2nm1 = x
      b2n = x + (1.0D0-a)
      c = 1.0D0
  260 a2nm1 = x*a2n + c*a2nm1
      b2nm1 = x*b2n + c*b2nm1
      am0 = a2nm1/b2nm1
      c = c + 1.0D0
      cma = c - a
      a2n = a2nm1 + cma*a2n
      b2n = b2nm1 + cma*b2n
      an0 = a2n/b2n
      IF (abs(an0-am0).GE.tol*an0) GO TO 260
C
      qans = r*an0
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
C
C                GENERAL TEMME EXPANSION
C
  270 IF (abs(s).LE.2.0D0*e .AND. a*e*e.GT.3.28D-3) GO TO 430
      c = exp(-y)
      w = 0.5D0*erfc1(1,sqrt(y))
      u = 1.0D0/a
      z = sqrt(z+z)
      IF (l.LT.1.0D0) z = -z
      IF (iop-2) 280,290,300
C
  280 IF (abs(s).LE.1.D-3) GO TO 340
      c0 = ((((((((((((d0(13)*z+d0(12))*z+d0(11))*z+d0(10))*z+d0(9))*z+
     +     d0(8))*z+d0(7))*z+d0(6))*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*
     +     z+d0(1))*z - third
      c1 = (((((((((((d1(12)*z+d1(11))*z+d1(10))*z+d1(9))*z+d1(8))*z+
     +     d1(7))*z+d1(6))*z+d1(5))*z+d1(4))*z+d1(3))*z+d1(2))*z+d1(1))*
     +     z + d10
      c2 = (((((((((d2(10)*z+d2(9))*z+d2(8))*z+d2(7))*z+d2(6))*z+
     +     d2(5))*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20
      c3 = (((((((d3(8)*z+d3(7))*z+d3(6))*z+d3(5))*z+d3(4))*z+d3(3))*z+
     +     d3(2))*z+d3(1))*z + d30
      c4 = (((((d4(6)*z+d4(5))*z+d4(4))*z+d4(3))*z+d4(2))*z+d4(1))*z +
     +     d40
      c5 = (((d5(4)*z+d5(3))*z+d5(2))*z+d5(1))*z + d50
      c6 = (d6(2)*z+d6(1))*z + d60
      t = ((((((d70*u+c6)*u+c5)*u+c4)*u+c3)*u+c2)*u+c1)*u + c0
      GO TO 310
C
  290 c0 = (((((d0(6)*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*z+d0(1))*z -
     +     third
      c1 = (((d1(4)*z+d1(3))*z+d1(2))*z+d1(1))*z + d10
      c2 = d2(1)*z + d20
      t = (c2*u+c1)*u + c0
      GO TO 310
C
  300 t = ((d0(3)*z+d0(2))*z+d0(1))*z - third
C
  310 IF (l.LT.1.0D0) GO TO 320
      qans = c* (w+rt2pin*t/rta)
      ans = 0.5D0 + (0.5D0-qans)
      RETURN

  320 ans = c* (w-rt2pin*t/rta)
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
C
C               TEMME EXPANSION FOR L = 1
C
  330 IF (a*e*e.GT.3.28D-3) GO TO 430
      c = 0.5D0 + (0.5D0-y)
      w = (0.5D0-sqrt(y)* (0.5D0+ (0.5D0-y/3.0D0))/rtpi)/c
      u = 1.0D0/a
      z = sqrt(z+z)
      IF (l.LT.1.0D0) z = -z
      IF (iop-2) 340,350,360
C
  340 c0 = ((((((d0(7)*z+d0(6))*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*z+
     +     d0(1))*z - third
      c1 = (((((d1(6)*z+d1(5))*z+d1(4))*z+d1(3))*z+d1(2))*z+d1(1))*z +
     +     d10
      c2 = ((((d2(5)*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20
      c3 = (((d3(4)*z+d3(3))*z+d3(2))*z+d3(1))*z + d30
      c4 = (d4(2)*z+d4(1))*z + d40
      c5 = (d5(2)*z+d5(1))*z + d50
      c6 = d6(1)*z + d60
      t = ((((((d70*u+c6)*u+c5)*u+c4)*u+c3)*u+c2)*u+c1)*u + c0
      GO TO 310
C
  350 c0 = (d0(2)*z+d0(1))*z - third
      c1 = d1(1)*z + d10
      t = (d20*u+c1)*u + c0
      GO TO 310
C
  360 t = d0(1)*z - third
      GO TO 310
C
C                     SPECIAL CASES
C
  370 ans = 0.0D0
      qans = 1.0D0
      RETURN
C
  380 ans = 1.0D0
      qans = 0.0D0
      RETURN
C
  390 IF (x.GE.0.25D0) GO TO 400
      ans = erf(sqrt(x))
      qans = 0.5D0 + (0.5D0-ans)
      RETURN

  400 qans = erfc1(0,sqrt(x))
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
C
  410 IF (abs(s).LE.2.0D0*e) GO TO 430
  420 IF (x.LE.a) GO TO 370
      GO TO 380
C
C                     ERROR RETURN
C
  430 ans = 2.0D0
      RETURN

      END
      DOUBLE PRECISION FUNCTION gsumln(a,b)
C-----------------------------------------------------------------------
C          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
C          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION x
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alnrel,gamln1
      EXTERNAL alnrel,gamln1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
C     ..
C     .. Executable Statements ..
      x = dble(a) + dble(b) - 2.D0
      IF (x.GT.0.25D0) GO TO 10
      gsumln = gamln1(1.0D0+x)
      RETURN

   10 IF (x.GT.1.25D0) GO TO 20
      gsumln = gamln1(x) + alnrel(x)
      RETURN

   20 gsumln = gamln1(x-1.0D0) + dlog(x* (1.0D0+x))
      RETURN

      END
      INTEGER FUNCTION ipmpar(i)
C-----------------------------------------------------------------------
C
C     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
C     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
C     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...
C
C  INTEGERS.
C
C     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM
C
C               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.
C
C     IPMPAR(1) = A, THE BASE.
C
C     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS.
C
C     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS.
C
C     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
C     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
C     NONZERO NUMBERS ARE REPRESENTED IN THE FORM
C
C               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)
C
C               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
C               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.
C
C     IPMPAR(4) = B, THE BASE.
C
C  SINGLE-PRECISION
C
C     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.
C
C     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.
C
C     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION
C
C     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.
C
C     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.
C
C     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.
C
C-----------------------------------------------------------------------
C
C     TO DEFINE THIS FUNCTION FOR THE COMPUTER BEING USED, ACTIVATE
C     THE DATA STATMENTS FOR THE COMPUTER BY REMOVING THE C FROM
C     COLUMN 1. (ALL THE OTHER DATA STATEMENTS SHOULD HAVE C IN
C     COLUMN 1.)
C
C-----------------------------------------------------------------------
C
C     IPMPAR IS AN ADAPTATION OF THE FUNCTION I1MACH, WRITTEN BY
C     P.A. FOX, A.D. HALL, AND N.L. SCHRYER (BELL LABORATORIES).
C     IPMPAR WAS FORMED BY A.H. MORRIS (NSWC). THE CONSTANTS ARE
C     FROM BELL LABORATORIES, NSWC, AND OTHER SOURCES.
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER i
C     ..
C     .. Local Arrays ..
      INTEGER imach(10)
C     ..
C     .. Data statements ..
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C     DATA IMACH( 1) /   2 /
C     DATA IMACH( 2) /  31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /  16 /
C     DATA IMACH( 5) /   6 /
C     DATA IMACH( 6) / -64 /
C     DATA IMACH( 7) /  63 /
C     DATA IMACH( 8) /  14 /
C     DATA IMACH( 9) / -64 /
C     DATA IMACH(10) /  63 /
C
C     MACHINE CONSTANTS FOR THE AT&T 3B SERIES, AT&T
C     PC 7300, AND AT&T 6300.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   33 /
C     DATA IMACH( 3) / 8589934591 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -256 /
C     DATA IMACH( 7) /  255 /
C     DATA IMACH( 8) /   60 /
C     DATA IMACH( 9) / -256 /
C     DATA IMACH(10) /  255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   39 /
C     DATA IMACH( 3) / 549755813887 /
C     DATA IMACH( 4) /    8 /
C     DATA IMACH( 5) /   13 /
C     DATA IMACH( 6) /  -50 /
C     DATA IMACH( 7) /   76 /
C     DATA IMACH( 8) /   26 /
C     DATA IMACH( 9) /  -50 /
C     DATA IMACH(10) /   76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C     DATA IMACH( 1) /      2 /
C     DATA IMACH( 2) /     39 /
C     DATA IMACH( 3) / 549755813887 /
C     DATA IMACH( 4) /      8 /
C     DATA IMACH( 5) /     13 /
C     DATA IMACH( 6) /    -50 /
C     DATA IMACH( 7) /     76 /
C     DATA IMACH( 8) /     26 /
C     DATA IMACH( 9) / -32754 /
C     DATA IMACH(10) /  32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
C     ARITHMETIC (NOS OPERATING SYSTEM).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   48 /
C     DATA IMACH( 3) / 281474976710655 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   48 /
C     DATA IMACH( 6) / -974 /
C     DATA IMACH( 7) / 1070 /
C     DATA IMACH( 8) /   95 /
C     DATA IMACH( 9) / -926 /
C     DATA IMACH(10) / 1070 /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 995 64 BIT
C     ARITHMETIC (NOS/VE OPERATING SYSTEM).
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    63 /
C     DATA IMACH( 3) / 9223372036854775807 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    48 /
C     DATA IMACH( 6) / -4096 /
C     DATA IMACH( 7) /  4095 /
C     DATA IMACH( 8) /    96 /
C     DATA IMACH( 9) / -4096 /
C     DATA IMACH(10) /  4095 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    63 /
C     DATA IMACH( 3) / 9223372036854775807 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    47 /
C     DATA IMACH( 6) / -8189 /
C     DATA IMACH( 7) /  8190 /
C     DATA IMACH( 8) /    94 /
C     DATA IMACH( 9) / -8099 /
C     DATA IMACH(10) /  8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /   16 /
C     DATA IMACH( 5) /    6 /
C     DATA IMACH( 6) /  -64 /
C     DATA IMACH( 7) /   63 /
C     DATA IMACH( 8) /   14 /
C     DATA IMACH( 9) /  -64 /
C     DATA IMACH(10) /   63 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   23 /
C     DATA IMACH( 3) / 8388607 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   38 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000
C     AND DPS 8/70 SERIES.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   63 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   39 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   55 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -126 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
C     5/7/9 AND THE SEL SYSTEMS 85/86.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /   16 /
C     DATA IMACH( 5) /    6 /
C     DATA IMACH( 6) /  -64 /
C     DATA IMACH( 7) /   63 /
C     DATA IMACH( 8) /   14 /
C     DATA IMACH( 9) /  -64 /
C     DATA IMACH(10) /   63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC.
C
C      DATA imach(1)/2/
C      DATA imach(2)/31/
C      DATA imach(3)/2147483647/
C      DATA imach(4)/2/
C      DATA imach(5)/24/
C      DATA imach(6)/-125/
C      DATA imach(7)/128/
C      DATA imach(8)/53/
C      DATA imach(9)/-1021/
C      DATA imach(10)/1024/
C
C     MACHINE CONSTANTS FOR THE MACINTOSH II - ABSOFT
C     MACFORTRAN II.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE MICROVAX - VMS FORTRAN.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   54 /
C     DATA IMACH( 9) / -101 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   62 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS IRIS-4D
C     SERIES (MIPS R3000 PROCESSOR).
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
C
      DATA IMACH( 1) /     2 /
      DATA IMACH( 2) /    31 /
      DATA IMACH( 3) / 2147483647 /
      DATA IMACH( 4) /     2 /
      DATA IMACH( 5) /    24 /
      DATA IMACH( 6) /  -125 /
      DATA IMACH( 7) /   128 /
      DATA IMACH( 8) /    53 /
      DATA IMACH( 9) / -1021 /
      DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   60 /
C     DATA IMACH( 9) /-1024 /
C     DATA IMACH(10) / 1023 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
      ipmpar = imach(i)
      RETURN

      END
      DOUBLE PRECISION FUNCTION psi(xx)
C---------------------------------------------------------------------
C
C                 EVALUATION OF THE DIGAMMA FUNCTION
C
C                           -----------
C
C     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT
C     BE COMPUTED.
C
C     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV
C     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY
C     CODY, STRECOK AND THACHER.
C
C---------------------------------------------------------------------
C     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK
C     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY
C     A.H. MORRIS (NSWC).
C---------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION xx
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION aug,den,dx0,piov4,sgn,upper,w,x,xmax1,xmx0,
     +                 xsmall,z
      INTEGER i,m,n,nq
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION p1(7),p2(4),q1(6),q2(4)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION spmpar
      INTEGER ipmpar
      EXTERNAL spmpar,ipmpar
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,cos,dble,dlog,dmin1,int,sin
C     ..
C     .. Data statements ..
C---------------------------------------------------------------------
C
C     PIOV4 = PI/4
C     DX0 = ZERO OF PSI TO EXTENDED PRECISION
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
C     PSI(X) / (X - X0),  0.5 .LE. X .LE. 3.0
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
C     PSI(X) - LN(X) + 1 / (2*X),  X .GT. 3.0
C
C---------------------------------------------------------------------
      DATA piov4/.785398163397448D0/
      DATA dx0/1.461632144968362341262659542325721325D0/
      DATA p1(1)/.895385022981970D-02/,p1(2)/.477762828042627D+01/,
     +     p1(3)/.142441585084029D+03/,p1(4)/.118645200713425D+04/,
     +     p1(5)/.363351846806499D+04/,p1(6)/.413810161269013D+04/,
     +     p1(7)/.130560269827897D+04/
      DATA q1(1)/.448452573429826D+02/,q1(2)/.520752771467162D+03/,
     +     q1(3)/.221000799247830D+04/,q1(4)/.364127349079381D+04/,
     +     q1(5)/.190831076596300D+04/,q1(6)/.691091682714533D-05/
      DATA p2(1)/-.212940445131011D+01/,p2(2)/-.701677227766759D+01/,
     +     p2(3)/-.448616543918019D+01/,p2(4)/-.648157123766197D+00/
      DATA q2(1)/.322703493791143D+02/,q2(2)/.892920700481861D+02/,
     +     q2(3)/.546117738103215D+02/,q2(4)/.777788548522962D+01/
C     ..
C     .. Executable Statements ..
C---------------------------------------------------------------------
C
C     MACHINE DEPENDENT CONSTANTS ...
C
C        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
C                 WITH ENTIRELY INTEGER REPRESENTATION.  ALSO USED
C                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
C                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
C                 PSI MAY BE REPRESENTED AS ALOG(X).
C
C        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X)
C                 MAY BE REPRESENTED BY 1/X.
C
C---------------------------------------------------------------------
      xmax1 = ipmpar(3)
      xmax1 = dmin1(xmax1,1.0D0/spmpar(1))
      xsmall = 1.D-9
C---------------------------------------------------------------------
      x = xx
      aug = 0.0D0
      IF (x.GE.0.5D0) GO TO 50
C---------------------------------------------------------------------
C     X .LT. 0.5,  USE REFLECTION FORMULA
C     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
C---------------------------------------------------------------------
      IF (abs(x).GT.xsmall) GO TO 10
      IF (x.EQ.0.0D0) GO TO 100
C---------------------------------------------------------------------
C     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
C     FOR  PI*COTAN(PI*X)
C---------------------------------------------------------------------
      aug = -1.0D0/x
      GO TO 40
C---------------------------------------------------------------------
C     REDUCTION OF ARGUMENT FOR COTAN
C---------------------------------------------------------------------
   10 w = -x
      sgn = piov4
      IF (w.GT.0.0D0) GO TO 20
      w = -w
      sgn = -sgn
C---------------------------------------------------------------------
C     MAKE AN ERROR EXIT IF X .LE. -XMAX1
C---------------------------------------------------------------------
   20 IF (w.GE.xmax1) GO TO 100
      nq = int(w)
      w = w - dble(nq)
      nq = int(w*4.0D0)
      w = 4.0D0* (w-dble(nq)*.25D0)
C---------------------------------------------------------------------
C     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
C     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
C     QUADRANT AND DETERMINE SIGN
C---------------------------------------------------------------------
      n = nq/2
      IF ((n+n).NE.nq) w = 1.0D0 - w
      z = piov4*w
      m = n/2
      IF ((m+m).NE.n) sgn = -sgn
C---------------------------------------------------------------------
C     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X)
C---------------------------------------------------------------------
      n = (nq+1)/2
      m = n/2
      m = m + m
      IF (m.NE.n) GO TO 30
C---------------------------------------------------------------------
C     CHECK FOR SINGULARITY
C---------------------------------------------------------------------
      IF (z.EQ.0.0D0) GO TO 100
C---------------------------------------------------------------------
C     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
C     SIN/COS AS A SUBSTITUTE FOR TAN
C---------------------------------------------------------------------
      aug = sgn* ((cos(z)/sin(z))*4.0D0)
      GO TO 40

   30 aug = sgn* ((sin(z)/cos(z))*4.0D0)
   40 x = 1.0D0 - x
   50 IF (x.GT.3.0D0) GO TO 70
C---------------------------------------------------------------------
C     0.5 .LE. X .LE. 3.0
C---------------------------------------------------------------------
      den = x
      upper = p1(1)*x
C
      DO 60 i = 1,5
          den = (den+q1(i))*x
          upper = (upper+p1(i+1))*x
   60 CONTINUE
C
      den = (upper+p1(7))/ (den+q1(6))
      xmx0 = dble(x) - dx0
      psi = den*xmx0 + aug
      RETURN
C---------------------------------------------------------------------
C     IF X .GE. XMAX1, PSI = LN(X)
C---------------------------------------------------------------------
   70 IF (x.GE.xmax1) GO TO 90
C---------------------------------------------------------------------
C     3.0 .LT. X .LT. XMAX1
C---------------------------------------------------------------------
      w = 1.0D0/ (x*x)
      den = w
      upper = p2(1)*w
C
      DO 80 i = 1,3
          den = (den+q2(i))*w
          upper = (upper+p2(i+1))*w
   80 CONTINUE
C
      aug = upper/ (den+q2(4)) - 0.5D0/x + aug
   90 psi = aug + dlog(x)
      RETURN
C---------------------------------------------------------------------
C     ERROR RETURN
C---------------------------------------------------------------------
  100 psi = 0.0D0
      RETURN

      END
      DOUBLE PRECISION FUNCTION rcomp(a,x)
C     -------------------
C     EVALUATION OF EXP(-X)*X**A/GAMMA(A)
C     -------------------
C     RT2PIN = 1/SQRT(2*PI)
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION rt2pin,t,t1,u
C     ..
C     .. External Functions ..
      DOUBLE PRECISION gam1,gamma,rlog
      EXTERNAL gam1,gamma,rlog
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dlog,exp,sqrt
C     ..
C     .. Data statements ..
      DATA rt2pin/.398942280401433D0/
C     ..
C     .. Executable Statements ..
C     -------------------
      rcomp = 0.0D0
      IF (a.GE.20.0D0) GO TO 20
      t = a*dlog(x) - x
      IF (a.GE.1.0D0) GO TO 10
      rcomp = (a*exp(t))* (1.0D0+gam1(a))
      RETURN

   10 rcomp = exp(t)/gamma(a)
      RETURN
C
   20 u = x/a
      IF (u.EQ.0.0D0) RETURN
      t = (1.0D0/a)**2
      t1 = (((0.75D0*t-1.0D0)*t+3.5D0)*t-105.0D0)/ (a*1260.0D0)
      t1 = t1 - a*rlog(u)
      rcomp = rt2pin*sqrt(a)*exp(t1)
      RETURN

      END
      DOUBLE PRECISION FUNCTION rexp(x)
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION EXP(X) - 1
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION p1,p2,q1,q2,q3,q4,w
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp
C     ..
C     .. Data statements ..
      DATA p1/.914041914819518D-09/,p2/.238082361044469D-01/,
     +     q1/-.499999999085958D+00/,q2/.107141568980644D+00/,
     +     q3/-.119041179760821D-01/,q4/.595130811860248D-03/
C     ..
C     .. Executable Statements ..
C-----------------------
      IF (abs(x).GT.0.15D0) GO TO 10
      rexp = x* (((p2*x+p1)*x+1.0D0)/ ((((q4*x+q3)*x+q2)*x+q1)*x+1.0D0))
      RETURN
C
   10 w = exp(x)
      IF (x.GT.0.0D0) GO TO 20
      rexp = (w-0.5D0) - 0.5D0
      RETURN

   20 rexp = w* (0.5D0+ (0.5D0-1.0D0/w))
      RETURN

      END
      DOUBLE PRECISION FUNCTION rlog(x)
C     -------------------
C     COMPUTATION OF  X - 1 - LN(X)
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,p0,p1,p2,q1,q2,r,t,u,w,w1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
C     ..
C     .. Data statements ..
C     -------------------
      DATA a/.566749439387324D-01/
      DATA b/.456512608815524D-01/
      DATA p0/.333333333333333D+00/,p1/-.224696413112536D+00/,
     +     p2/.620886815375787D-02/
      DATA q1/-.127408923933623D+01/,q2/.354508718369557D+00/
C     ..
C     .. Executable Statements ..
C     -------------------
      IF (x.LT.0.61D0 .OR. x.GT.1.57D0) GO TO 40
      IF (x.LT.0.82D0) GO TO 10
      IF (x.GT.1.18D0) GO TO 20
C
C              ARGUMENT REDUCTION
C
      u = (x-0.5D0) - 0.5D0
      w1 = 0.0D0
      GO TO 30
C
   10 u = dble(x) - 0.7D0
      u = u/0.7D0
      w1 = a - u*0.3D0
      GO TO 30
C
   20 u = 0.75D0*dble(x) - 1.D0
      w1 = b + u/3.0D0
C
C               SERIES EXPANSION
C
   30 r = u/ (u+2.0D0)
      t = r*r
      w = ((p2*t+p1)*t+p0)/ ((q2*t+q1)*t+1.0D0)
      rlog = 2.0D0*t* (1.0D0/ (1.0D0-r)-r*w) + w1
      RETURN
C
C
   40 r = (x-0.5D0) - 0.5D0
      rlog = r - dlog(x)
      RETURN

      END
      DOUBLE PRECISION FUNCTION rlog1(x)
C-----------------------------------------------------------------------
C             EVALUATION OF THE FUNCTION X - LN(1 + X)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION x
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,h,p0,p1,p2,q1,q2,r,t,w,w1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
C     ..
C     .. Data statements ..
C------------------------
      DATA a/.566749439387324D-01/
      DATA b/.456512608815524D-01/
      DATA p0/.333333333333333D+00/,p1/-.224696413112536D+00/,
     +     p2/.620886815375787D-02/
      DATA q1/-.127408923933623D+01/,q2/.354508718369557D+00/
C     ..
C     .. Executable Statements ..
C------------------------
      IF (x.LT.-0.39D0 .OR. x.GT.0.57D0) GO TO 40
      IF (x.LT.-0.18D0) GO TO 10
      IF (x.GT.0.18D0) GO TO 20
C
C              ARGUMENT REDUCTION
C
      h = x
      w1 = 0.0D0
      GO TO 30
C
   10 h = dble(x) + 0.3D0
      h = h/0.7D0
      w1 = a - h*0.3D0
      GO TO 30
C
   20 h = 0.75D0*dble(x) - 0.25D0
      w1 = b + h/3.0D0
C
C               SERIES EXPANSION
C
   30 r = h/ (h+2.0D0)
      t = r*r
      w = ((p2*t+p1)*t+p0)/ ((q2*t+q1)*t+1.0D0)
      rlog1 = 2.0D0*t* (1.0D0/ (1.0D0-r)-r*w) + w1
      RETURN
C
C
   40 w = (x+0.5D0) + 0.5D0
      rlog1 = x - dlog(w)
      RETURN

      END
      DOUBLE PRECISION FUNCTION spmpar(i)
C-----------------------------------------------------------------------
C
C     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
C     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
C     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
C     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
C     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
C
C        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
C
C        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
C
C        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
C
C-----------------------------------------------------------------------
C     WRITTEN BY
C        ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WARFARE CENTER
C        DAHLGREN VIRGINIA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
C     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
C     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER i
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION b,binv,bm1,one,w,z
      INTEGER emax,emin,ibeta,m
C     ..
C     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble
C     ..
C     .. Executable Statements ..
C
      IF (i.GT.1) GO TO 10
      b = ipmpar(4)
      m = ipmpar(8)
      spmpar = b** (1-m)
      RETURN
C
   10 IF (i.GT.2) GO TO 20
      b = ipmpar(4)
      emin = ipmpar(9)
      one = dble(1)
      binv = one/b
      w = b** (emin+2)
      spmpar = ((w*binv)*binv)*binv
      RETURN
C
   20 ibeta = ipmpar(4)
      m = ipmpar(8)
      emax = ipmpar(10)
C
      b = ibeta
      bm1 = ibeta - 1
      one = dble(1)
      z = b** (m-1)
      w = ((z-one)*b+bm1)/ (b*z)
C
      z = b** (emax-2)
      spmpar = ((w*z)*b)*b
      RETURN

      END
      DOUBLE PRECISION FUNCTION stvaln(p)
C
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION STVALN(P)
C                    STarting VALue for Neton-Raphon
C                calculation of Normal distribution Inverse
C
C
C                              Function
C
C
C     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
C     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
C
C
C                              Arguments
C
C
C     P --> The probability whose normal deviate is sought.
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980.
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION p
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION sign,y,z
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION xden(5),xnum(5)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION devlpl
      EXTERNAL devlpl
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,log,sqrt
C     ..
C     .. Data statements ..
      DATA xnum/-0.322232431088D0,-1.000000000000D0,-0.342242088547D0,
     +     -0.204231210245D-1,-0.453642210148D-4/
      DATA xden/0.993484626060D-1,0.588581570495D0,0.531103462366D0,
     +     0.103537752850D0,0.38560700634D-2/
C     ..
C     .. Executable Statements ..
      IF (.NOT. (p.LE.0.5D0)) GO TO 10
      sign = -1.0D0
      z = p
      GO TO 20

   10 sign = 1.0D0
      z = 1.0D0 - p
   20 y = sqrt(-2.0D0*log(z))
      stvaln = y + devlpl(xnum,5,y)/devlpl(xden,5,y)
      stvaln = sign*stvaln
      RETURN

      END
