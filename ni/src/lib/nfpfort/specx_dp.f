      SUBROUTINE DSPECX(X,NX,IOPT,JAVE,PCT,SCL,WORK,LWORK,FRQ,SPCX,NSPC,
     +                  SINFO,IER)
      DOUBLE PRECISION PCT
      DOUBLE PRECISION SCL
      DOUBLE PRECISION COEF
      DOUBLE PRECISION XINFO
      DOUBLE PRECISION SPVAL
      DOUBLE PRECISION X1X
      DOUBLE PRECISION FRQSP
      DOUBLE PRECISION DOF

      INTEGER NX,IOPT,JAVE,LWORK,NSPC,IER
      DOUBLE PRECISION X(NX),FRQ(*),SPCX(*),WORK(LWORK),SINFO(50)


c specx will perform a spectral analysis on the series x of length nx.

c comment :
c .   common block /spblk3/ contains assorted info the user may find
c .   useful.  it was used primarily to debug this routine.

c nomenclature:

c .   input
c .
c .   x         - real series containing the data (unchanged upon
c .               return)
c .   nx        - number of points in x
c .               nx need not be a power of 2 but the fft will run
c .               optimally if nx is highly factorable.
c .   iopt      - detrending option
c .               iopt<0 : do no detrending.
c .               iopt=0 : remove series mean (minimum recommended
c .                        option)
c .               iopt=1 : remove the series mean and lst sqrs linear
c .                        trend
c .               iopt=2 : remove the series mean and lst sqrs
c .                        quadratic trend
c .                        know what you are doing before applying
c .                        this option
c .   jave      - smoothing to be performed on the periodiogram
c .               this should be an odd number (>=3) , if not, the
c .               routine will force it to the next largest odd number.
c .               jave=0 : do no smoothing
c .                        spcx contains raw spectral estimates 
c .                        (periodogram)
c .               jave>0 : average jave periodogram estimates together
c .                        utilizing modified daniell smoothing (good
c .                        stability but may lead to large bias ).
c .                        all weights are 1/jave except wgt(1) and
c .                        wgt(jave) which are 1/(2*jave). this is 
c .                        the recommended option.
c .   pct       - % of the series to be tapered [0.<=pct>=1.]
c .               if pct=0. no tapering will be done
c .               if pct=1. the whole series is effected
c .               a pct of 0.10 is common. tapering should always be
c .               done unless the data is strictly periodic.
c .   scl       - scaling factor (there are 4 special cases.)
c .               scl = -1.: spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals one.
c .               scl = 0. : spcx estimates are scaled so that the sum
c .                          of the spectral estimates is the same as
c .                          the variance of the detrended (optional)
c .                          input series
c .               scl = 1. : spcx estimates are as calculated  by g13cbf
c .               scl = 2. : spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals the
c .                          variance of the detrended (optional)
c .                          input series.
c .               scl= 2 or -1. is recommended
c .   work      - work array
c .               if jave.ne.0 then upon return work(1 thru nspc) will
c .               contain the raw spectral estimates [i.e. , 
c .               periodogram]
c .   lwork     - length of work [ must be >= 5*nx + 17 +iabs(jave) + 1]
c .
c .   output
c .
c .   frq       - frequency vector of length nspc
c .   spcx      - spectrum : vector of length nspc
c .               spcx(1)     - this will be the **mean* of the
c .                             detrended-tapered series. it
c .                             is not a variance. it is not the
c .                             spectral estimate at freq = 0
c .               spcx(2)     - spectral estimate at freq = 1/nx
c .               spcx(3)     - spectral estimate at freq = 2/nx
c .                           .
c .               spcx(nspc) - spectral estimate at freq = 0.5
c .   nspc      - the number of spectral estimates returned (=nx/2+1)
c .   sinfo     - vector of length 10 ==> 23 for NCL
c .        (1)  - equivalent degrees of freedom
c .        (2)  - lag1 autocor of detrended/tapered x series
c .        (3)  - lag1 autocor of detrended/tapered y series for specxy
c .        (4)  - unused
c .        (5)  - freq spacing between estimates
c .        (6)  - band width (=sinfo(1)*sinfo(5)*0.5)
c .        (7)  - mult factor for  5% limit
c .        (8)  - mult factor for 95% limit
c .        (9)  - dof for series via spectrum shape(blackmon-tukey)
c .        (10) - unused
c .   ier       - error code
c .               ier=0 : no error
c .               ier'0 : see code

c sample call :

c .   real x(nx) , frq(nx/2+1) , spcx(nx/2+1)
c .              , workk(5*nx+18+iabs(jave)) , sinfo(50)
c .   iopt = 0     : remove mean
c .   jave = 11    : block average 11 periodogram est together
c .   pct  = 0.10  : taper 10% of the series
c .   scl  = 1.0   : no scaling. leave ``as"
c .   call specx  (x,nx,iopt,jave,pct,scl,work,lwork
c .              ,frq,spcx,nspc,frqsp,dof,ier)

c common block /spblk / contains other potentially useful info

c .   coef      - contains coef utilized to remove trend (if any)
c .   xinfo(1)  - mean of x prior to tapering / detrending
c .        (2)  - variance of x prior to tapering / detrending
c .        (3)  - variance of x after detrending and prior to tapering
c .        (4)  - raw sum of spcx(2 thru nspc) with no scaling or
c .               smoothing
c .               this is the variance of the detrended - tapered
c .               series.
c .        (5)  - raw sum of spcx(2 thru nspc) after smoothing
c .        (6)  - scaling utilized to normalize the spectrum to xinfo(3)
c .        (7)  - white noise level [=scale*xinfo(3)/(nspc-1)]
c .        (8)  - correction factor to dof due to tapering  (cftapr)
c .        (9)  - unused
c .        (10) - unused

      COMMON /SPBLK/COEF(3),XINFO(10)

      DATA SPVAL/1.D+36/

c check input for errors

      IER = 0
      IF (NX.LE.3) IER = 1
      IF (IOPT.GT.2) IER = IER + 20
      IF (PCT.LT.0.D0 .OR. PCT.GT.1.0D0) IER = IER + 300
      IF (LWORK.LT. (5*NX+18+IABS(JAVE))) IER = IER + 4000
      IF (IABS(JAVE).GT. (NX/2)) IER = IER + 50000

c c c do 3 n=1,nx
C check for missing values
c c c if (x(n).ne.spval) go to 4
c c 3 continue
c c c ier = ier+60000
    4 X1X = X(1)
      DO 5 N = 1,NX
C check for series of constant values
          IF (X(N).NE.X1X) GO TO 6
    5 CONTINUE
      IER = IER + 700000

C set common block to spval
    6 DO 7 N = 1,3
    7 COEF(N) = SPVAL
      DO 8 N = 1,10
          SINFO(N) = SPVAL
    8 XINFO(N) = SPVAL
      DO 88 N = 11,50
   88 SINFO(N) = SPVAL

      DO 9 N = 1,(NX/2+1)
          FRQ(N) = SPVAL
C set spectral estimates to spval
    9 SPCX(N) = SPVAL
      NSPC = 0

      IF (IER.NE.0) THEN
C return to calling prog
          RETURN
      END IF

c initilize pointers for work vector

      NX2 = NX/2
      NSPC = NX2 + 1
C pointer to real fourier coef vector
      KPTRC = NX + 1
C pointer to imag fourier coef vector
      KPTIC = KPTRC + NX2
C pointer to work vector for ezfft
      KPTSAV = KPTIC + NX2
C pointer to wgt  vector
      KPTWGT = KPTSAV + (3*NX+17)

c transfer the input series x to the work array

      DO 10 N = 1,NX
   10 WORK(N) = X(N)
      DO 15 N = NX + 1,LWORK
   15 WORK(N) = SPVAL

c initilize fft work space

      CALL DEZFFTI(NX,WORK(KPTSAV))

c spectral driver

      NLAG1 = 2
      CALL DSPECXD(WORK,NX,IOPT,JAVE,PCT,SCL,FRQ,SPCX,NSPC,FRQSP,DOF,
     +            IER,WORK(KPTRC),WORK(KPTIC),WORK(KPTSAV),WORK(KPTWGT),
     +            SPVAL,NX2,COEF,XINFO,SINFO,NLAG1)

C NCL add to sinfo
      DO 133 N = 1,10
          SINFO(10+N) = XINFO(N)
  133 CONTINUE
      DO 134 N = 1,3
          SINFO(30+N) = COEF(N)
  134 CONTINUE

      RETURN
      END
c -------------------------------------------------------------------
      SUBROUTINE DSPECXY(X,Y,NX,IOPT,JAVE,PCT,SCL,WORK,LWORK,FRQ,SPCX,
     +                   SPCY,COSPC,QUSPC,COHER,PHASE,NSPC,SINFO,IER)
      DOUBLE PRECISION PCT
      DOUBLE PRECISION SCL
      DOUBLE PRECISION XCOEF
      DOUBLE PRECISION XINFO
      DOUBLE PRECISION YCOEF
      DOUBLE PRECISION YINFO
      DOUBLE PRECISION SPVAL
      DOUBLE PRECISION X1X
      DOUBLE PRECISION Y1Y
      DOUBLE PRECISION FRQSP
      DOUBLE PRECISION DOF
      DOUBLE PRECISION SCLCS
      DOUBLE PRECISION SCLCOH

c specxy will perform a spectral/cross spc analysis on series x and y.

c nomenclature:

c .   input
c .
c .   x         - real series containing the data (unchanged upon
c .               return)
c .   y         - real series containing the data (unchanged upon
c .               return)
c .   nx        - number of points in x and y (need not be a power of 2)
c .   iopt      - detrending option
c .               iopt<0 : do no detrending.
c .               iopt=0 : remove series mean (minimum recommended 
c .                        option)
c .               iopt=1 : remove the series mean and lst sqrs linear
c .                        trend
c .               iopt=2 : remove the series mean and lst sqrs
c .                        quadratic trend
c .                        know what you are doing before applying
c .                        this option
c .   jave      - no. of spectral estimates to be averaged
c .   pct       - % of the series to be tapered [0.<=pct>=1.]
c .               if pct=0. no tapering will be done
c .               if pct=1. the whole series is effected
c .               a pct of 0.10 is common. tapering should always be
c .               done unless the data is strictly periodic.
c .   scl       - scaling factor (there are 4 special cases.)
c .               scl = -1.: spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals one.
c .               scl = 0. : spcx estimates are scaled so that the sum
c .                          of the spectral estimates is the same as
c .                          the variance of the detrended (optional)
c .                          input series
c .               scl = 1. : spcx estimates are as calculated  by g13cbf
c .               scl = 2. : spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals the
c .                          variance of the detrended (optional) 
c .                          input series.
c .   work      - work array
c .   lwork     - length of work [ must be >= 10*nx ]
c .
c .   output
c .
c .   spcx,spcy - spectrum vector of length nspc for x and y series
c .               spcx(1)    - spectral estimate at freq = 0 (ignore)
c .               spcx(nspc) - spectral estimate at freq = 0.5
c .   cospc     - cospectrum : vector of length nspc
c .               this is the real part of the cross spectrum. it
c .               measures the extent to which there are oscillations
c .               with the same phase in the two series (or with
c .               opposite sign ,i.e. , with a phase shift of half a
c .               cycle). In other words : it measures the
c .               contribution of different frequencies to the total
c .               cross-covariance at zero lag.
c .   quspc     - quadrature spectrum  : vector of length nspc
c .               this is the imaginary part of the cross spectrum. it
c .               measures the extent to which there are oscillations
c .               with a phase difference of a quarter cycle in either
c .               direction. i.e., it measures the contribution of
c .               different frequencies to the total cross-covariance
c .               of the series when all harmonics of one series
c .               are delayed a quarter cycle relative to the other
c .               relative to the other series.
c .   coher     - coherence squared  : vector of length nspc
c .               this is analogous to the square of the correlation
c .               coef except that the coherence squared is a
c .               function of frequency.
c .   phase     - phase (in degrees)  : vector of length nspc.
C)
c .               a positive phase indicates that y leads x (i think
c .   nspc      - the number of spectral estimates returned
c .               routine is set up so that nspc=nx/2+1
c .   sinfo     - must be dimensioned at least 10 in calling prog
c .               sinfo(1)  - degrees of freedom  (dof)
c .                           the dof for the cospc,quspc,coher and
c .                            phase is approximately dof-2 .
c .                    (2)  - lag1 autocor of detrended/tapered x series
c .                    (3)  - lag1 autocor of detrended/tapered y series
c .                    (4)  - unused
c .                    (5)  - freq spacing at which values should be
c .                           plotted
c .                    (6)  - `normal' bandwidth
c .                    (7)  - mult factor for  5% limit (imsl)
c .                    (8)  - mult factor for 95% limit (imsl)
c .                    (9)  - unused
c .                    (10) - unused
c .   ier       - error code
c .               ier=0 : no error
c .               ier<0 : error detected by spcng2: see code
c .               ier>0 : error detected by nag routine

c common blocks /spblkx,spblky/ contain other potentially useful info

c .   x/ycoef     - contains coef utilized to remove trend (if any)
c .   x/yinfo(1)  - mean of x prior to tapering / detrending
c .          (2)  - variance of x prior to tapering / detrending
c .          (3)  - variance of x after detrending and prior to tapering
c .          (4)  - raw sum of spcx(2 thru nspc) after smoothing
c .          (5)  - same as xinfo(4)
c .          (6)  - scaling utilized to normalize the spectrum to
c .                 xinfo(3)
c .          (7)  - white noise level [=xinfo(3)/(nspc-1)]
c .          (8)  - correction factor to dof due to tapering  (cftapr)
c .          (9)  - unused
c .          (10) - unused

      DOUBLE PRECISION X(NX),Y(NX),FRQ(*),SPCX(*),SPCY(*),WORK(LWORK),
     +                 COSPC(*),QUSPC(*),PHASE(*),COHER(*),SINFO(50)

      COMMON /SPBLKX/XCOEF(3),XINFO(10)
      COMMON /SPBLKY/YCOEF(3),YINFO(10)

      DATA SPVAL/1.D+36/

c check input for errors

      IER = 0
      IF (NX.LE.3) IER = 1
      IF (IOPT.GT.2) IER = IER + 20
      IF (PCT.LT.0.D0 .OR. PCT.GT.1.0D0) IER = IER + 300
      IF (LWORK.LT.10*NX) IER = IER + 4000
      IF (IABS(JAVE).GT. (NX/2)) IER = IER + 50000

      DO 2 N = 1,NX
C chk for msg values
          IF (X(N).EQ.SPVAL .OR. Y(N).EQ.SPVAL) THEN
              IER = IER + 60000
              GO TO 3
          END IF
    2 CONTINUE
    3 X1X = X(1)
      DO 4 N = 1,NX
C check for series of constant values
          IF (X(N).NE.X1X) GO TO 5
    4 CONTINUE
      IER = IER + 700000
      GO TO 6
    5 Y1Y = Y(1)
      DO 55 N = 1,NX
C check for series of constant values
          IF (Y(N).NE.Y1Y) GO TO 6
   55 CONTINUE
      IER = IER + 700000

    6 IF (IER.NE.0) THEN
C set common block to spval
          DO 7 N = 1,3
              XCOEF(N) = SPVAL
    7     YCOEF(N) = SPVAL
          DO 8 N = 1,10
              SINFO(N) = SPVAL
              YINFO(N) = SPVAL
    8     XINFO(N) = SPVAL
          DO 88 N = 11,50
   88     SINFO(N) = SPVAL
          DO 9 N = 1,(NX/2+1)
              FRQ(N) = SPVAL
              COSPC(N) = SPVAL
              QUSPC(N) = SPVAL
              COHER(N) = SPVAL
              PHASE(N) = SPVAL
              SPCY(N) = SPVAL
    9     SPCX(N) = SPVAL
          NSPC = 0
C return to calling prog
          RETURN
      END IF

c initilize pointers for work vector

      NX2 = NX/2
      NSPC = NX2 + 1
C pointer to real fourier coef vec x
      KPTRCX = NX + 1
C pointer to imag fourier coef vec x
      KPTICX = KPTRCX + NX2
C pointer to real fourier coef vec y
      KPTRCY = KPTICX + NX2
C pointer to imag fourier coef vec y
      KPTICY = KPTRCY + NX2
C pointer to imag fourier coef vec y
      KPTSAV = KPTICY + NX2
C pointer to wgt  vector
      KPTWGT = KPTSAV + (3*NX+17)

c transfer the input series x to the work array

      DO 10 N = 1,NX
   10 WORK(N) = X(N)
      DO 15 N = NX + 1,LWORK
   15 WORK(N) = SPVAL

c initilize fft work space

      CALL DEZFFTI(NX,WORK(KPTSAV))

c spectral driver (x vector)

      NLAG1 = 2
      CALL DSPECXD(WORK,NX,IOPT,JAVE,PCT,SCL,FRQ,SPCX,NSPC,FRQSP,DOF,
     +            IER,WORK(KPTRCX),WORK(KPTICX),WORK(KPTSAV),
     +            WORK(KPTWGT),SPVAL,NX2,XCOEF,XINFO,SINFO,NLAG1)

c transfer the input series y to the work array

      DO 20 N = 1,NX
   20 WORK(N) = Y(N)

c spectral driver (y vector)

      NLAG1 = 3
      CALL DSPECXD(WORK,NX,IOPT,JAVE,PCT,SCL,FRQ,SPCY,NSPC,FRQSP,DOF,
     +            IER,WORK(KPTRCY),WORK(KPTICY),WORK(KPTSAV),
     +            WORK(KPTWGT),SPVAL,NX2,YCOEF,YINFO,SINFO,NLAG1)

c compute the cospectrum and quadrature spectrum

      SCLCS = SQRT(XINFO(6)*YINFO(6))
      COSPC(1) = SPCX(1)*SPCY(1)
      QUSPC(1) = SPCX(1)*SPCY(1)
      DO 60 N = 0,NX2 - 1
          COSPC(N+2) = SCLCS* (WORK(KPTRCX+N)*WORK(KPTRCY+N)+
     +                 WORK(KPTICX+N)*WORK(KPTICY+N))
C minus sign is to match nag output
   60 QUSPC(N+2) = -SCLCS* (WORK(KPTICX+N)*WORK(KPTRCY+N)-
     +             WORK(KPTRCX+N)*WORK(KPTICY+N))

      KOPT = 1
      JAVODD = 2*IABS(JAVE/2) + 1
C can use work again
      CALL DSWRNAV(COSPC(2),NSPC-1,WORK(KPTWGT),JAVODD,KOPT,SPVAL,WORK,
     +            IER)
      CALL DSWRNAV(QUSPC(2),NSPC-1,WORK(KPTWGT),JAVODD,KOPT,SPVAL,WORK,
     +            IER)

c compute the coherence and phase now that the co/quspc have been
c calculated

      SCLCOH = 1.0D0
      DO 70 N = 1,NSPC
          PHASE(N) = ATAN2(QUSPC(N),COSPC(N))*57.29578D0
          IF (SPCX(N).NE.0.D0 .AND. SPCY(N).NE.0.D0) THEN
              COHER(N) = SCLCOH* (COSPC(N)**2+QUSPC(N)**2)/
     +                   (SPCX(N)*SPCY(N))
          ELSE
              COHER(N) = 0.D0
          END IF
   70 CONTINUE

C NCL add to sinfo
      DO 133 N = 1,10
          SINFO(10+N) = XINFO(N)
          SINFO(20+N) = YINFO(N)
  133 CONTINUE
      DO 134 N = 1,3
          SINFO(30+N) = XCOEF(N)
          SINFO(33+N) = YCOEF(N)
  134 CONTINUE

      RETURN
      END
c --------------------------------------------------------------------
      SUBROUTINE DSPECXD(X,NX,IOPT,JAVE,PCT,SCL,FRQ,SPC,NSPC,FRQSP,DOF,
     +                   IER,CR,CI,SAVE,WGT,SPVAL,NX2,COEF,XINFO,SINFO,
     +                   NLAG1)
      DOUBLE PRECISION PCT
      DOUBLE PRECISION SCL
      DOUBLE PRECISION FRQSP
      DOUBLE PRECISION DOF
      DOUBLE PRECISION SPVAL
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION PI
      DOUBLE PRECISION WGTSQ
      DOUBLE PRECISION CFTAPR
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XMEANX
      DOUBLE PRECISION XVARX
      DOUBLE PRECISION AZERO
      DOUBLE PRECISION WGTSUM

      DOUBLE PRECISION X(NX),FRQ(NSPC),SPC(NSPC),SAVE(*),WGT(*),CR(NX2),
     +                 CI(NX2),COEF(3),XINFO(10),SINFO(50)
      PARAMETER (MXLAG=1,XMSG=1.D+36)
      DOUBLE PRECISION ACV(0:MXLAG),ACR(0:MXLAG)

      DATA PI/3.141592653589D0/

c initilize some constants

      DF   = 1.0D0/DBLE(NX)
      NXP1 = NX + 1
      NXM1 = NX - 1
      NSPCM1 = NSPC - 1
      FRQSP = 1.D0/DBLE(NX)
      WGTSQ = 1.0D0
      DOF = 2.0D0
      CFTAPR = 1.0D0
      SCALE = SCL
      DO 20 N = 1,10
   20 XINFO(N) = SPVAL

c perform detrending option (iopt)
c .   the mean and variance will always be calculated
c .   note : if iopt=1 or 2 then the variance of x on output [xinfo(3)]
c .   may be considerably different (reduced) from the variance of x
c .   on input [xinfo(2)]. xinfo(1) will contain the mean of the
c .   input series.

      CALL DDTRNDX (X,NX,IOPT,XINFO(1),XINFO(2),XINFO(3),COEF,IER)

c perform tapering on detrended x vector
c .   the tapering procedure alters the variance of the input
c .   series and as a result lowers the dof associated with each
c .   spectral estimate. if pct is 0.05 or 0.10 then the effect is
c .   small.
c .   the purpose of tapering when viewed from the frequency domain
c .   is to suppress large side lobes in the effective filter obtained
c .   with the raw transform. in the time domain , the object of
c .   tapering is to round off potential discontinuities at each end
c .   of the finite segment being analyzed.

      IF (PCT.GT.0.0D0) THEN
          IF (IOPT.GE.0) THEN
              XMEAN = 0.0D0
          ELSE
              XMEAN = XINFO(1)
          END IF
          CALL DTAPERX(X,NX,PCT,XMEAN,CFTAPR)
      END IF

c calculate lag1 autocorrelation

      XMEANX = XMSG
      XVARX = XMSG
      CALL DESAUTO(X,NX,XMSG,XMEANX,XVARX,MXLAG,ACV,ACR,IER)
      SINFO(NLAG1) = ACR(1)

c perform an fft on the detrended-tapered series with zeros added

      CALL DEZFFTF(NX,X,AZERO,CR,CI,SAVE)

c calculate the raw spectrum  sum the spectral estimates

      SPC(1) = AZERO
      DO 40 N = 2,NSPC
   40 SPC(N) = (CR(N-1)**2+CI(N-1)**2)

      XINFO(4) = 0.0D0
      DO 41 N = 2,NSPC
   41 XINFO(4) = XINFO(4) + SPC(N)
      XINFO(5) = XINFO(4)

c smooth the raw spectral estimates (if desired)

C make sure its odd
      IF (IABS(JAVE).GT.2) THEN
          JAVODD = 2*IABS(JAVE/2) + 1
          DO 70 N = 2,JAVODD - 1
   70     WGT(N) = 1.00D0
          WGT(1) = 0.5D0
          WGT(JAVODD) = 0.5D0

c normalize the sum of the wgts to one then sum the square of the
c .   normalized wgts to calculate the deg of freedom

          WGTSUM = 0.0D0
          DO 120 N = 1,JAVODD
  120     WGTSUM = WGTSUM + WGT(N)

          DO 123 N = 1,JAVODD
  123     WGT(N) = WGT(N)/WGTSUM

          WGTSQ = 0.0D0
          DO 125 N = 1,JAVODD
  125     WGTSQ = WGTSQ + WGT(N)**2

c perform weighted running ave
c .   kopt = 1 means to utilize symmetric averaging at the ends of
c .   the spectrum. note : spc(1) is not utilized. if the user does
c .   want this estimate included in the averaging change the (2) to
c .   (1) for both spc and x and nspcm1 to nspc.

          KOPT = 1
          X(1) = SPC(1)
          CALL DSWRNAV(SPC(2),NSPCM1,WGT,JAVODD,KOPT,SPVAL,X(2),IER)

      END IF

c sum the spectral estimates
C only half the width

      XINFO(5) = (SPC(2)+SPC(NSPC))*0.5D0*DF
      DO 150 N = 3,NSPC-1
  150 XINFO(5) = XINFO(5) + SPC(N)*DF

      IF (SCL.EQ.0.D0) THEN
C scale sum spcx to input variance
          XINFO(6) = XINFO(3)/XINFO(5)
C white noise level
          XINFO(7) = XINFO(3)/DBLE(NSPC)
      ELSE IF (SCL.EQ.1.D0) THEN
C keep spcx as returned by g13cbf
          XINFO(6) = 1.0D0
C white noise level
          XINFO(7) = XINFO(5)/DBLE(NSPC-1)
      ELSE IF (SCL.EQ.-1.D0) THEN
C scal spcx estimate to 1.0
          XINFO(6) = 1.0D0*DBLE(NX)/XINFO(5)
C white noise level
          XINFO(7) = 1.0D0/DBLE(NSPC)
C white noise level
c        xinfo(7) = 1.0*float(nx)/float(nspc)
      ELSE IF (SCL.EQ.2.D0) THEN
C area under curve=total variance
          XINFO(6) = XINFO(3)/XINFO(5)
C white noise level
          XINFO(7) = XINFO(3)/DBLE(NSPC)
      ELSE
C user specified scaling
          XINFO(6) = SCL
C white noise level
          XINFO(7) = SCL*XINFO(5)/DBLE(NSPC)
      END IF

      DO 250 N = 2,NSPC
  250 SPC(N) = SPC(N)*XINFO(6)

      XINFO(8) = CFTAPR

c correct the dof for :
c .   (1) the tapering , if any  [the factor cftapr]
c .   (2) the weighting utilized [the factor wgtsq]

      DOF = DOF/ (CFTAPR*WGTSQ)


      SINFO(1) = DOF
      SINFO(5) = FRQSP
C see jones pg 202: murphy + katz
      SINFO(6) = SINFO(1)*SINFO(5)*0.5D0
      IF (DOF.GT.0.50D0) THEN
C imsl
c imsl   sinfo(7) = chiin (0.05,dof)/dof
c imsl   sinfo(8) = chiin (0.95,dof)/dof
          IFAIL = 0
          JFAIL = 0
C nag
c nag    sinfo(7) = g01ccf (0.05,dof,ifail)/dof
c nag    sinfo(8) = g01ccf (0.95,dof,jfail)/dof
          SINFO(7) = SPVAL
          SINFO(8) = SPVAL
      ELSE
          SINFO(7) = SPVAL
          SINFO(8) = SPVAL
      END IF

      DO 270 N = 1,NSPC
  270 FRQ(N) = SINFO(5)*DBLE(N-1)

c c c call specdf  (frq,spc,nspc,sinfo(9),ier)

      RETURN
      END
c --------------------------------------------------------------------
      SUBROUTINE DTAPERX(X,N,P,XAV,CFT)
      DOUBLE PRECISION X
      DOUBLE PRECISION P
      DOUBLE PRECISION XAV
      DOUBLE PRECISION CFT
      DOUBLE PRECISION PI
      DOUBLE PRECISION PIM
      DOUBLE PRECISION WEIGHT
c
c this subroutine applies split-cosine-bell tapering tothe series x.
c .   the series will be tapered to the mean of x.
c
c arguments:
c .   x   series to be tapered (tapering done in place)
c .   n   series length
c .   p   the proportion of the time series to be tapered [p=0.10 means
c .   xav mean of the input series x (i.e., value to which the series
c .       should be tapered). if the series mean has been removed then
c .       xav = 0.0
c .   cft correction factor for taper (not used , for info only)
c
      DIMENSION X(N)
      DATA PI/3.141592653589D0/

      M = MAX0(1,INT(P*DBLE(N)+0.5D0)/2)
      PIM = PI/DBLE(M)
      DO I = 1,M
          WEIGHT = 0.5D0 - 0.5D0*COS(PIM* (DBLE(I)-0.5D0))
          X(I) = (X(I)-XAV)*WEIGHT + XAV
          X(N+1-I) = (X(N+1-I)-XAV)*WEIGHT + XAV
      END DO
      CFT = 0.5D0* (128.D0-93.D0*P)/ (8.D0-5.D0*P)**2
      RETURN
      END
c -------------------------------------------------------------------
      SUBROUTINE DSWRNAV(X,NPTS,WGT,NPTAV,KOPT,SPVL,WORK,IER)
      DOUBLE PRECISION X
      DOUBLE PRECISION WGT
      DOUBLE PRECISION SPVL
      DOUBLE PRECISION WORK
      DOUBLE PRECISION WSUM
      DOUBLE PRECISION XNAV
c
c this routine will perform a weighted running average on x
c .   several options are given for end points
c
c nomenclature:
c .   x         - on input the series to be smoothed
c .               on output the smoothed series
c .   npts      - no of pts in x
c .   wgt       - vector of length nptav containing the wgts
c .               e.g. , nptav=3 : wgt(1) = 1. , wgt(2) = 2. , wgt(3) =
c .                             or wgt(1) = .25, wgt(2) = .50, wgt(3) =
c .   nptav     - no. of pts to be utilized in the running average
c .               nptav must be odd and >= 3
c .   kopt      - end-point option
c .               kopt < 0 : utilize cyclic conditions
c .                          e.g.,  nptav=3 at n=1
c .                          x(1)=(wgt(1)*x(npts)+wgt(2)*x(1)+ 
c .                                wgt(3)*x(2))/(wgt(1)+wgt(2)+wgt(3))
c .               kopt = 0 : set unsmoothed beginning and end pts to
c .                          spvl
c .                          e.g.,  nptav=3 , x(1) = spvl and x(npts) =
c .               kopt > 0 : utilize reflective (symmetric) conditions
c .                          e.g.,  nptav=3 at n=1
c .                          x(1)=(wgt(1)*x(2)+wgt(2)*x(1)+wgt(3)*x(2))
c .                              /(wgt(1)+wgt(2)+wgt(3))
c .   spvl      - if kopt=0 set end points to this special value
c .               spvl = 0. or 1.e+36 or the series mean are common
c .   work      - work vector of length npts
c .               on return this will contain the unsmoothed series
c .   ier       - error flag
c .               ier = 0 : no errors
c .               ier = -11 : npts @ 0
c .               ier = -12 : npts < nptav
c .               ier = -13 : nptav is not odd
c
      DIMENSION X(NPTS),WORK(NPTS),WGT(NPTAV)

      IER = 0
      IF (NPTS.LT.1) IER = -11
      IF (NPTAV.GT.NPTS) IER = -12
      IF (MOD(NPTAV,2).EQ.0) IER = -13
      IF (IER.LT.0) RETURN

c dup the vectors

      DO N = 1,NPTS
          WORK(N) = X(N)
      END DO

      IF (NPTAV.LT.3) RETURN

c sum the wgts

      WSUM = 0.0D0
      DO N = 1,NPTAV
          WSUM = WSUM + WGT(N)
      END DO
      IF (WSUM.GT.1.0D0) THEN
          XNAV = 1.0D0/WSUM
      ELSE
          XNAV = 1.0D0
      END IF

c do the middle pts first

      NAV = NPTAV
      NAV2 = NAV/2
      NSTRT = NAV2 + 1
      NEND = NPTS - NAV2

      DO N = NSTRT,NEND
          MSTRT = N - NAV2
          MEND = N + NAV2
          ML = 0
          WSUM = 0.D0
          DO M = MSTRT,MEND
              ML = ML + 1
              WSUM = WSUM + WORK(M)*WGT(ML)
          END DO
          X(N) = WSUM*XNAV
      END DO
      IF (KOPT.NE.0) THEN
          NAV2P2 = NAV2 + 2
          DO N = 1,NAV2
              MEND = NAV2 + N
              MLSTRT = NAV2P2 - N
              ML = MLSTRT - 1
              WSUM = 0.0D0
              DO M = 1,MEND
                  ML = ML + 1
                  WSUM = WSUM + WORK(M)*WGT(ML)
              END DO

              MEND = NAV2P2 - N
              ML = MLSTRT
              DO M = 2,MEND
                  ML = ML - 1
                  MLOC = M
                  IF (KOPT.LT.0) MLOC = NPTS + 1 - M
                  WSUM = WSUM + WORK(MLOC)*WGT(ML)
              END DO

              X(N) = WSUM*XNAV
          END DO

c do the final portion of x

          NPTS2 = 2*NPTS
          NSTRT = NPTS - NAV2 + 1
          DO N = NSTRT,NPTS
              MSTRT = N - NAV2
              MEND = N + NAV2
              ML = 0
              WSUM = 0.0D0
              DO M = MSTRT,MEND
                  ML = ML + 1
                  MLOC = M
                  IF (MLOC.GT.NPTS .AND. KOPT.GT.0) MLOC = NPTS2 - MLOC
                  IF (MLOC.GT.NPTS .AND. KOPT.LT.0) MLOC = MLOC - NPTS
                  WSUM = WSUM + WORK(MLOC)*WGT(ML)
              END DO
              X(N) = WSUM*XNAV
          END DO
          RETURN
      END IF
      DO N = 1,NAV2
          X(N) = SPVL
      END DO
      NSTRT = NPTS - NAV2 + 1
      DO N = NSTRT,NPTS
          X(N) = SPVL
      END DO

      RETURN
      END
