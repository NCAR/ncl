C NCLFORTSTART
      SUBROUTINE DTAPER(X,N,P,XT,IOPT)
      IMPLICIT NONE
C NCL: xTaper = taper(x,p,iopt)

c this subroutine applies split-cosine-bell tapering to the series x.
c .   the series will be tapered to the mean of x.
c .   See Bloomfield's "Intro to Fourier .."
c .   This is used prior performing an fft on non-cyclic data

c arguments:
c .   x   series to be tapered (tapering done in place)
c .       **missing data not allowed**
c .   n   series length
c .   p   the proportion of the time series to be tapered
c .       [p=0.10 means 10 %]
c .   xt  tapered series
c .   iopt option not used ... set to zero
C                                            INPUT
      INTEGER N,IOPT
      DOUBLE PRECISION X(N),P
C                                            OUTPUT
      DOUBLE PRECISION XT(N)
C NCLEND
      INTEGER I,M
      DOUBLE PRECISION WEIGHT,PI,PIM,XAV
      DATA PI/3.141592653589D0/

      XAV = 0.0D0
      DO I = 1,N
          XT(I) = X(I)
          XAV = XAV + X(I)
      END DO
      XAV = XAV/N

      M = MAX0(1,INT(P*DBLE(N)+0.5D0)/2)
      PIM = PI/DBLE(M)

      DO I = 1,M
          WEIGHT = 0.5D0 - 0.5D0*COS(PIM* (DBLE(I)-0.5D0))
          XT(I) = (X(I)-XAV)*WEIGHT + XAV
          XT(N+1-I) = (X(N+1-I)-XAV)*WEIGHT + XAV
      END DO

c .   cft correction factor for taper (not used , for info only)
c c c cft = 0.5*(128.-93.*p)/(8.-5.*p)**2

      RETURN
      END
