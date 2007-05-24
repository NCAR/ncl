C NCLFORTSTART
      SUBROUTINE DFILTRQ(NWT,FCA,FCB,NSIGMA,IHP,WT,RESP,FREQ,IER)
      IMPLICIT NONE

c NCL:  wt = filwgts_lancos (nwt, ihp, fca,fcb,nsigma)
c       wt@freq = freq
c       wt@resp = resp

c routine to calculate filter wgts and the associated response function

c see an article by c. duchon (u. of oklahoma):
c                   j. applied meteorology;  august,1979; pp 1016-1022
c                   "lanczos filtering in one and two dimensions"

      INTEGER NWT,IHP,IER
      DOUBLE PRECISION FCA,FCB,NSIGMA
      DOUBLE PRECISION WT(NWT)
      DOUBLE PRECISION FREQ(2*NWT-1),RESP(2*NWT-1)
c c c real    freq(*), resp(*)
C NCLEND

c input parameters :
c
c     nwt     total number of weights (an odd number; nwt.ge.3)
c             the more wgts .... the better the filter but there
c             is a greater loss of data.
c
c     fca     the cut-off freq of the ideal high or
c             low-pass filter: [ 0.0 < fca < 0.5 ]
c
c     fcb     used only when a band-pass filter is desired,
c             in which case it is the cut-off freq for the
c             second low-pass filter:  [ fca < fcb < 0.5 ]
c
c     nsigma  the power of the sigma factor.  it is
c             greater than or equal to zero.
c             note: nsigma=1 to 3 is usually good enough.
c
c     ihp     if low-pass filter ihp = 0,
c             if high-pass ihp = 1,
c             if band-pass ihp = 2.
c
c     work    work array of length nwt
c
c output parameters :
c
c     wt      vector containing the computed wgts. its length is nwt.
c             the central weight is located at nwt/2+1.
c
c     resp    the array of responses at freq intvls
c             of 0.5/(2*nwt-2)  cycles/data intvl from the
c             origin to the nyquist.  its length is 2*nwt-1.
c
c     freq    the array of freqs at intvls of 0.5/(2*nwt-2)
c             cycles/data intvl at which the responses
c             are calculated.  its length is 2*nwt-1 .
c
c     ier     simple error code
c                                                   ! automatic
      DOUBLE PRECISION WORK(NWT)
      DOUBLE PRECISION PI,TWOPI,FNW,SUM,FRQINT
      INTEGER NW,N,NWP1,I,J,NF

      IER = 0
C      IF (NWT.LT.3) IER = 1
C      IF (IHP.LT.0 .OR. IHP.GT.2) IER = IER + 10
C      IF (FCA.LT.0.0D0 .OR. FCA.GT.0.5D0) IER = IER + 100
C      IF (IHP.EQ.2) THEN
C          IF (FCB.LT.0.0D0 .OR. FCB.GT.0.5D0) IER = IER + 1000
C          IF (FCB.LT.FCA) IER = IER + 10000
C      END IF

      IF (IER.NE.0) RETURN

      PI = 4.D0*ATAN(1.0D0)
      TWOPI = 2.D0*PI
      NW = (NWT-1)/2
      FNW = DBLE(NW)

c compute weights

      CALL DFILWTQ(WT,NWT,NW,FCA,PI,TWOPI,NSIGMA)
c                               ! alter weights to get high-pass filter.
      IF (IHP.EQ.1) THEN
          WT(1) = 1.0D0 - WT(1)
          DO I = 2,NW + 1
              WT(I) = -WT(I)
          END DO
      ELSE IF (IHP.EQ.2) THEN
c                               ! compute wgts of 2nd low-pass filt
c                               ! save 1st set of low pass filter wgts
          DO I = 1,NW + 1
              WORK(I) = WT(I)
          END DO
          CALL DFILWTQ(WT,NWT,NW,FCB,PI,TWOPI,NSIGMA)
c                               ! alter weights to get band-pass filter
          DO I = 1,NW + 1
              WT(I) = WT(I) - WORK(I)
          END DO
      END IF

c compute response fctn

      SUM = 0.0D0
      NF = 2*NWT - 1
      FREQ(1) = 0.0D0
      FRQINT = 0.5D0/DBLE(NF-1)
      DO J = 2,NW + 1
          SUM = SUM + 2.0D0*WT(J)
      END DO
      RESP(1) = SUM + WT(1)

      DO I = 2,NF
          SUM = 0.0D0
          FREQ(I) = DBLE(I-1)*FRQINT
          DO J = 2,NW + 1
              SUM = SUM + WT(J)*COS(TWOPI*FREQ(I)*DBLE(J-1))
          END DO
          RESP(I) = WT(1) + 2.0D0*SUM
      END DO

c make wt symmetric

      NWP1 = NW + 1
      DO N = 1,NW
          WORK(N) = WT(N+1)
      END DO
      WT(NWP1) = WT(1)
      DO N = 1,NW
          WT(N) = WORK(NWP1-N)
          WT(NWP1+N) = WORK(N)
      END DO

      RETURN
      END
c ----------------------------------------------------------
      SUBROUTINE DFILWTQ(WT,NWT,NW,FC,PI,TWOPI,NSIGMA)
      IMPLICIT NONE

c compute and normalize filter wgts

      INTEGER NWT,NW
      DOUBLE PRECISION WT(NWT),FC,PI,TWOPI,NSIGMA
c                                                ! local
      DOUBLE PRECISION ARG,FNW,SINX,SINY,SUM
      INTEGER I

      ARG = TWOPI*FC
      FNW = DBLE(NW)

      WT(1) = 2.0D0*FC
      DO I = 1,NW
          SINX = SIN(ARG*DBLE(I))/ (PI*DBLE(I))
          SINY = FNW*SIN(DBLE(I)*PI/FNW)/ (DBLE(I)*PI)
          WT(I+1) = SINX*SINY**NSIGMA
      END DO

c                                                ! normalize weights
      SUM = WT(1)
      DO I = 2,NW + 1
          SUM = SUM + 2.0D0*WT(I)
      END DO
      DO I = 1,NW + 1
          WT(I) = WT(I)/SUM
      END DO

      RETURN
      END

C NCLFORTSTART
      subroutine filwgtnormal (nwt, w, itype, sigma, ier)
      implicit none
      integer  nwt, itype, ier
      double precision     w(nwt), sigma
C NCLEND
c
c NCL: wgts = filwgts(nwt, sigma, itype)
c currently this only does gaussian (normal) weights
c
      double precision    pi, c1, c2, mean, delx, x, wsum
      integer n

      ier  = 0
      mean = 0.0d0

      pi   = 4.d0*atan(1.d0)
      c1   = 1.d0/(sigma*sqrt(2.d0*pi))
      c2   = -0.5d0/sigma**2

c                   delx = fspan(-1, 1, nwt)
      delx = 2.d0/(nwt-1)   

c c c if (itype.eq.0) then
          x    = -(1.d0+delx)
          wsum = 0.0d0
          do n=1,nwt
             x = x+delx
             w(n) = c1*exp(c2*(x-mean)**2)
             wsum = wsum+w(n)
          end do
c c c else
c c c     ier = 1
c c c     print ("filwgts: unsupported option")
c c c end if

      do n=1,nwt
         w(n) = w(n)/wsum
      end do

      return
      end
