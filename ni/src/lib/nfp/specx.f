
      subroutine specx  (x, nx ,iopt,jave,pct,scl,work,lwork
     1                  ,frq,spcx,nspc,sinfo,ier)
 
      integer nx, iopt, jave, lwork, nspc, ier
      real x(nx) , frq(*), spcx(*) , work(lwork) , sinfo(50)

 
c specx will perform a spectral analysis on the series x of length nx.
 
c comment :
c .   common block /spblk3/ contains assorted info the user may find
c .   useful.  it was used primarily to debug this routine.
 
c nomenclature:
 
c .   input
c .
c .   x         - real series containing the data (unchanged upon return)
c .   nx        - number of points in x
c .               nx need not be a power of 2 but the fft will run
c .               optimally if nx is highly factorable.
c .   iopt      - detrending option
c .               iopt<0 : do no detrending.
c .               iopt=0 : remove series mean (minimum recommended option)
c .               iopt=1 : remove the series mean and lst sqrs linear trend
c .               iopt=2 : remove the series mean and lst sqrs quadratic trend
c .                        know what you are doing before applying this option
c .   jave      - smoothing to be performed on the periodiogram
c .               this should be an odd number (>=3) , if not, the routine
c .               will force it to the next largest odd number.
c .               jave=0 : do no smoothing
c .                        spcx contains raw spectral estimates (periodogram)
c .               jave>0 : average jave periodogram estimates together
c .                        utilizing modified daniell smoothing (good
c .                        stability but may lead to large bias ).
c .                        all weights are 1/jave except wgt(1) and wgt(jave)
c .                        which are 1/(2*jave). this is the recommended option.
c .   pct       - % of the series to be tapered [0.<=pct>=1.]
c .               if pct=0. no tapering will be done
c .               if pct=1. the whole series is effected
c .               a pct of 0.10 is common. tapering should always be
c .               done unless the data is strictly periodic.
c .   scl       - scaling factor (there are 4 special cases.)
c .               scl = -1.: spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals one.
c .               scl = 0. : spcx estimates are scaled so that the sum
c .                          of the spectral estimates is the same as the
c .                          variance of the detrended (optional) input series
c .               scl = 1. : spcx estimates are as calculated  by g13cbf
c .               scl = 2. : spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals the
c .                          variance of the detrended (optional) input series.
c .               scl= 2 or -1. is recommended
c .   work      - work array
c .               if jave.ne.0 then upon return work(1 thru nspc) will
c .               contain the raw spectral estimates [i.e. , periodogram]
c .   lwork     - length of work [ must be >= 5*nx + 17 +iabs(jave)]
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
c .              , workk(5*nx+17+iabs(jave)) , sinfo(50)
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
c .        (4)  - raw sum of spcx(2 thru nspc) with no scaling or smoothing
c .               this is the variance of the detrended - tapered series.
c .        (5)  - raw sum of spcx(2 thru nspc) after smoothing
c .        (6)  - scaling utilized to normalize the spectrum to xinfo(3)
c .        (7)  - white noise level [=scale*xinfo(3)/(nspc-1)]
c .        (8)  - correction factor to dof due to tapering  (cftapr)
c .        (9)  - unused
c .        (10) - unused
 
      common /spblk / coef(3) , xinfo(10)
 
      data spval   /1.e+36/
 
c check input for errors
 
      ier = 0
      if (nx.le.3) ier = 1
      if (iopt.gt.2) ier = ier+20
      if (pct.lt.0. .or. pct.gt.1.0) ier = ier+300
      if (lwork.lt.(5*nx+17+iabs(jave))) ier = ier+4000
      if (iabs(jave).gt.(nx/2)) ier = ier+50000
 
c c c do 3 n=1,nx
c c c if (x(n).ne.spval) go to 4    ! check for missing values
c c 3 continue
c c c ier = ier+60000
    4 x1x = x(1)
      do 5 n=1,nx
      if (x(n).ne.x1x) go to 6      ! check for series of constant values
    5 continue
      ier = ier+700000
 
    6 do 7 n=1,3                ! set common block to spval
    7 coef(n) = spval
      do 8 n=1,10
      sinfo(n) = spval
    8 xinfo(n) = spval
      do 88 n=11,50
   88 sinfo(n) = spval
   
      do 9 n=1,amax0(1,nx/2+1)
      frq(n) = spval
    9 spcx(n)= spval             ! set spectral estimates to spval
      nspc   = 0

      if (ier.ne.0) then
         return                     ! return to calling prog
      endif
 
c initilize pointers for work vector
 
      nx2    = nx/2
      nspc   = nx2+1
      kptrc  = nx+1                 ! pointer to real fourier coef vector
      kptic  = kptrc + nx2          ! pointer to imag fourier coef vector
      kptsav = kptic + nx2          ! pointer to work vector for ezfft
      kptwgt = kptsav+(3*nx+17)     ! pointer to wgt  vector
 
c transfer the input series x to the work array
 
      do 10 n=1,nx
   10 work(n) = x(n)
      do 15 n=nx+1,lwork
   15 work(n) = spval
 
c initilize fft work space
 
      call ezffti (nx,work(kptsav))
 
c spectral driver
 
      nlag1 = 2
      call specxd (work,nx,iopt,jave,pct,scl
     1            ,frq,spcx,nspc,frqsp,dof,ier
     2            ,work(kptrc),work(kptic),work(kptsav),work(kptwgt)
     3            ,spval,nx2,coef,xinfo,sinfo,nlag1)
 
      do 133 n=1,10              ! NCL add to sinfo
         sinfo(10+n) = xinfo(n)
  133 continue
      do 134 n=1,3
         sinfo(30+n) = coef(n)
  134 continue

      return
      end
c -----------------------------------------------------------------------
      subroutine specxy (x,y, nx ,iopt,jave,pct,scl,work,lwork
     1                  ,frq,spcx,spcy,cospc,quspc,coher,phase
     2                  ,nspc,sinfo,ier)
 
c specxy will perform a spectral/cross spc analysis on series x and y.
 
c nomenclature:
 
c .   input
c .
c .   x         - real series containing the data (unchanged upon return)
c .   y         - real series containing the data (unchanged upon return)
c .   nx        - number of points in x and y (need not be a power of 2)
c .   iopt      - detrending option
c .               iopt<0 : do no detrending.
c .               iopt=0 : remove series mean (minimum recommended option)
c .               iopt=1 : remove the series mean and lst sqrs linear trend
c .               iopt=2 : remove the series mean and lst sqrs quadratic trend
c .                        know what you are doing before applying this option
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
c .                          of the spectral estimates is the same as the
c .                          variance of the detrended (optional) input series
c .               scl = 1. : spcx estimates are as calculated  by g13cbf
c .               scl = 2. : spcx estimates are scaled so that the area
c .                          under the curve [ s(f)*df ] equals the
c .                          variance of the detrended (optional) input series.
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
c .               with the same phase in the two series (or with opposite
c .               sign ,i.e. , with a phase shift of half a cycle).
c .               in other words : it measures the contribution of
c .               different frequencies to the total cross-covariance
c .               at zero lag.
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
c .               a positive phase indicates that y leads x (i think!)
c .   nspc      - the number of spectral estimates returned
c .               routine is set up so that nspc=nx/2+1
c .   sinfo     - must be dimensioned at least 10 in calling prog
c .               sinfo(1)  - degrees of freedom  (dof)
c .                           the dof for the cospc,quspc,coher and phase
c .                           is approximately dof-2 .
c .                    (2)  - lag1 autocor of detrended/tapered x series
c .                    (3)  - lag1 autocor of detrended/tapered y series
c .                    (4)  - unused
c .                    (5)  - freq spacing at which values should be plotted
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
c .          (6)  - scaling utilized to normalize the spectrum to xinfo(3)
c .          (7)  - white noise level [=xinfo(3)/(nspc-1)]
c .          (8)  - correction factor to dof due to tapering  (cftapr)
c .          (9)  - unused
c .          (10) - unused
 
      real      x(nx) , y(nx) , frq(*) , spcx(*) , spcy(*) , work(lwork)
     1        , cospc(*) , quspc(*) , phase(*) , coher(*) , sinfo(50)
 
      common /spblkx/ xcoef(3) , xinfo(10)
      common /spblky/ ycoef(3) , yinfo(10)
 
      data spval   /1.e+36/
 
c check input for errors
 
      ier = 0
      if (nx.le.3) ier = 1
      if (iopt.gt.2) ier = ier+20
      if (pct.lt.0. .or. pct.gt.1.0) ier = ier+300
      if (lwork.lt.10*nx) ier = ier+4000
      if (iabs(jave).gt.(nx/2)) ier = ier+50000
 
      do 2 n=1,nx
      if (x(n).eq.spval .or. y(n).eq.spval) then   ! chk for msg values
         ier = ier+60000
         go to 3
      endif
   2  continue
   3  x1x = x(1)
      do 4 n=1,nx
      if (x(n).ne.x1x) go to 5   ! check for series of constant values
   4  continue
      ier = ier+700000
      go to 6
   5  y1y = y(1)
      do 55 n=1,nx
      if (y(n).ne.y1y) go to 6   ! check for series of constant values
  55  continue
      ier = ier+700000
 
   6  if (ier.ne.0) then
         do 7 n=1,3                ! set common block to spval
         xcoef(n) = spval
   7     ycoef(n) = spval
         do 8 n=1,10
         sinfo(n) = spval
         yinfo(n) = spval
   8     xinfo(n) = spval
         do 88 n=11,50
   88    sinfo(n) = spval
         do 9 n=1,amax0(1,nx/2+1)
         frq(n)   = spval
         cospc(n) = spval
         quspc(n) = spval
         coher(n) = spval
         phase(n) = spval
         spcy(n)  = spval
   9     spcx(n)  = spval
         nspc     = 0
         return                     ! return to calling prog
      endif
 
c initilize pointers for work vector
 
      nx2    = nx/2
      nspc   = nx2+1
      kptrcx = nx+1                 ! pointer to real fourier coef vec x
      kpticx = kptrcx + nx2         ! pointer to imag fourier coef vec x
      kptrcy = kpticx + nx2         ! pointer to real fourier coef vec y
      kpticy = kptrcy + nx2         ! pointer to imag fourier coef vec y
      kptsav = kpticy + nx2         ! pointer to imag fourier coef vec y
      kptwgt = kptsav+(3*nx+17)     ! pointer to wgt  vector
 
c transfer the input series x to the work array
 
      do 10 n=1,nx
   10 work(n) = x(n)
      do 15 n=nx+1,lwork
   15 work(n) = spval
 
c initilize fft work space
 
      call ezffti (nx,work(kptsav))
 
c spectral driver (x vector)
 
      nlag1 = 2
      call specxd (work,nx,iopt,jave,pct,scl
     1            ,frq,spcx,nspc,frqsp,dof,ier
     2            ,work(kptrcx),work(kpticx)
     3            ,work(kptsav),work(kptwgt)
     4            ,spval,nx2,xcoef,xinfo,sinfo,nlag1)
 
c transfer the input series y to the work array
 
      do 20 n=1,nx
   20 work(n) = y(n)
 
c spectral driver (y vector)
 
      nlag1 = 3
      call specxd (work,nx,iopt,jave,pct,scl
     1            ,frq,spcy,nspc,frqsp,dof,ier
     2            ,work(kptrcy),work(kpticy)
     3            ,work(kptsav),work(kptwgt)
     4            ,spval,nx2,ycoef,yinfo,sinfo,nlag1)
 
c compute the cospectrum and quadrature spectrum
 
      sclcs = sqrt( xinfo(6)*yinfo(6) )
      cospc(1) = spcx(1)*spcy(1)
      quspc(1) = spcx(1)*spcy(1)
      do 60 n=0,nx2-1
      cospc(n+2) = sclcs*
     1   (work(kptrcx+n)*work(kptrcy+n)+work(kpticx+n)*work(kpticy+n))
   60 quspc(n+2) = -sclcs*            ! minus sign is to match nag output
     1   (work(kpticx+n)*work(kptrcy+n)-work(kptrcx+n)*work(kpticy+n))
 
      kopt    = 1
      javodd = 2*iabs(jave/2)+1
      call swrnav  (cospc(2),nspc-1,work(kptwgt)
     1            ,javodd,kopt,spval,work,ier)       ! can use work again
      call swrnav  (quspc(2),nspc-1,work(kptwgt)
     1            ,javodd,kopt,spval,work,ier)
 
c compute the coherence and phase now that the co/quspc have been calculated
 
      sclcoh = 1.0
      do 70 n=1,nspc
      phase(n) = atan2( quspc(n),cospc(n))*57.29578
      if (spcx(n).ne.0. .and. spcy(n).ne.0.) then
         coher(n) = sclcoh*(cospc(n)**2+quspc(n)**2)/(spcx(n)*spcy(n))
      else
         coher(n) = 0.
      endif
   70 continue
 
      do 133 n=1,10              ! NCL add to sinfo
         sinfo(10+n) = xinfo(n)
         sinfo(20+n) = yinfo(n)
  133 continue
      do 134 n=1,3
         sinfo(30+n) = xcoef(n)
         sinfo(33+n) = ycoef(n)
  134 continue
 
      return
      end
c -----------------------------------------------------------------------
      subroutine specxd  (x, nx ,iopt,jave,pct,scl
     1                   ,frq,spc,nspc,frqsp,dof,ier
     2                   ,cr,ci,save,wgt
     3                   ,spval,nx2,coef,xinfo,sinfo,nlag1)
 
      real      x(nx) , frq(nspc) , spc(nspc) , save(*) , wgt(*)
     1                , cr(nx2)   , ci(nx2)
     2                , coef(3)   , xinfo(10) , sinfo(50)
      parameter (mxlag=1, xmsg=1.e+36)
      real acv(0:mxlag), acr(0:mxlag)
      character label*96
 
      data pi      /3.141592653589/
 
c initilize some constants
 
      nxp1 = nx+1
      nxm1 = nx-1
      nspcm1 = nspc-1
      frqsp  = 1./float(nx)
      wgtsq  = 1.0
      dof    = 2.0
      cftapr = 1.0
      scale  = scl
      do 20 n=1,10
   20 xinfo(n) = spval
 
c perform detrending option (iopt)
c .   the mean and variance will always be calculated
c .   note : if iopt=1 or 2 then the variance of x on output [xinfo(3)]
c .   may be considerably different (reduced) from the variance of x
c .   on input [xinfo(2)]. xinfo(1) will contain the mean of the input series.
 
      call dtrndxd (x,nx,iopt,xinfo(1),xinfo(2),xinfo(3),coef,ier)
 
c perform tapering on detrended x vector
c .   the tapering procedure alters the variance of the input
c .   series and as a result lowers the dof associated with each spectral
c .   estimate. if pct is 0.05 or 0.10 then the effect is small.
c .   the purpose of tapering when viewed from the frequency domain is to
c .   suppress large side lobes in the effective filter obtained with
c .   the raw transform. in the time domain , the object of tapering is
c .   to round off potential discontinuities at each end of the finite
c .   segment being analyzed.
 
      if (pct.gt.0.0) then
          if (iopt.ge.0) then
              xmean = 0.0
          else
              xmean = xinfo(1)
          endif
          call taperx (x,nx,pct,xmean,cftapr)
      endif
 
c calculate lag1 autocorrelation
 
      xmeanx = xmsg
      xvarx  = xmsg
      call esauto (x,nx,xmsg,xmeanx,xvarx,mxlag,acv,acr,ier)
      sinfo(nlag1) = acr(1)
 
c perform an fft on the detrended-tapered series with zeros added
 
      call ezfftf (nx,x,azero,cr,ci,save)
 
c calculate the raw spectrum  sum the spectral estimates
 
      spc(1) = azero
      do 40 n=2,nspc
   40 spc(n) = (cr(n-1)**2 + ci(n-1)**2)
 
      xinfo(4) = 0.0
      do 41 n=2,nspc
   41 xinfo(4) = xinfo(4) + spc(n)
      xinfo(5) = xinfo(4)
 
c smooth the raw spectral estimates (if desired)
 
      if (iabs(jave).gt.2) then ! make sure its odd
         javodd = 2*iabs(jave/2)+1
 
         do 70 n=2,javodd-1
   70    wgt(n) = 1.00
         wgt(1) = 0.5
         wgt(javodd) = 0.5
 
c normalize the sum of the wgts to one then sum the square of the
c .   normalized wgts to calculate the deg of freedom
 
         wgtsum = 0.0
         do 120 n=1,javodd
  120    wgtsum = wgtsum + wgt(n)
 
         do 123 n=1,javodd
  123    wgt(n) = wgt(n)/wgtsum
 
         wgtsq = 0.0
         do 125 n=1,javodd
  125    wgtsq = wgtsq + wgt(n)**2
 
c perform weighted running ave
c .   kopt = 1 means to utilize symmetric averaging at the ends of the spectrum.
c .   note : spc(1) is not utilized. if the user does want this estimate
c .   included in the averaging change the (2) to (1) for both spc and x
c .   and nspcm1 to nspc.
 
         kopt    = 1
         x(1) = spc(1)
         call swrnav  (spc(2),nspcm1,wgt
     1               ,javodd,kopt,spval,x(2),ier)
 
c sum the smoothed spectral estimates
 
c        xinfo(5) = 0.0
         xinfo(5) = ( spc(2)+spc(nspc) )*0.5   ! only half the width
         do 150 n=3,nspc-1                     ! n=2,nspc
  150    xinfo(5) = xinfo(5) + spc(n)
 
      endif
 
      if (scl.eq.0.) then
         xinfo(6) = xinfo(3)/xinfo(5)          ! scale sum spcx to input variance
         xinfo(7) = xinfo(3)/float(nspc)       ! white noise level
      elseif (scl.eq.1.) then
         xinfo(6) = 1.0                        ! keep spcx as returned by g13cbf
         xinfo(7) = xinfo(5)/float(nspc-1)     ! white noise level
      elseif (scl.eq.-1.) then
         xinfo(6) = 1.0*float(nx)/xinfo(5)     ! scal spcx estimate to 1.0
         xinfo(7) = 1.0/float(nspc)            ! white noise level
c        xinfo(7) = 1.0*float(nx)/float(nspc)  ! white noise level
      elseif (scl.eq.2.) then
         xinfo(6) = xinfo(3)*float(nx)/xinfo(5)! area under curve=total variance
         xinfo(7) = xinfo(3)/float(nspc)       ! white noise level
      else
         xinfo(6) = scl                        ! user specified scaling
         xinfo(7) = scl*xinfo(5)/float(nspc)   ! white noise level
      endif
 
      do 250 n=2,nspc
  250 spc(n) = spc(n)*xinfo(6)
 
      xinfo(8) = cftapr
 
c correct the dof for :
c .   (1) the tapering , if any  [the factor cftapr]
c .   (2) the weighting utilized [the factor wgtsq]
 
      dof = dof /(cftapr*wgtsq)
 
 
      sinfo(1) = dof
      sinfo(5) = frqsp
      sinfo(6) = sinfo(1)*sinfo(5)*0.5      ! see jones pg 202: murphy + katz
      if (dof.gt.0.50) then
c imsl   sinfo(7) = chiin (0.05,dof)/dof    ! imsl
c imsl   sinfo(8) = chiin (0.95,dof)/dof
         ifail = 0
         jfail = 0
c nag    sinfo(7) = g01ccf (0.05,dof,ifail)/dof   ! nag
c nag    sinfo(8) = g01ccf (0.95,dof,jfail)/dof
         sinfo(7) = spval
         sinfo(8) = spval
      else
         sinfo(7) = spval
         sinfo(8) = spval
      endif
 
      do 270 n=1,nspc
  270 frq(n) = sinfo(5)*real(n-1)
 
c c c call specdf  (frq,spc,nspc,sinfo(9),ier)
 
      return
      end
c -----------------------------------------------------------------------
      subroutine dtrndxd(x, npts, iopt, xmean, xvari, xvaro, c, ier)
c
c detrend the series x
c
c nomenclature :
c
c .   input
c
c .   x         - series to be detrended
c .   npts      - length of x
c .   iopt      - detrending option
c .               iopt < 0 : calculate the mean and variance of x then return
c .               iopt = 0 : remove the mean only
c .               iopt = 1 : remove the mean and use linear lst sqrs to
c .               iopt = 2 : remove the mean and use quadratic lst sqrs
c .
c .   output
c .
c .   xmean     - mean of the original input  series (output)
c .   xvari     - variance of the original input series (output)
c .   xvaro     - variance of the detrended series (output)
c .               note : xvari = xvaro when iopt @ 0
c .   c         - coefficients of the least squares polynomial (output)
c .               must be dimensioned (3) in the calling program
c .               c(1)  constant
c .               c(2)  coef of first power [slope if iopt=1]
c .               c(3)  coef of second power
c .   ier       - error code
c
      real x(npts), c(3)
      double precision xm, xv, xn

      c(1) = 0.
      c(2) = 0.
      c(3) = 0.

      ier = 0
      if (npts .lt. 2) ier = -21
      if (ier .ne. 0) return 

      n = npts

c calculate the mean and variance of the input series

      xm = 0.d0
      xv = 0.d0
      xn = dble(real(n))
      do i = 1, n
         xm = xm + dble(x(i))
         xv = xv + dprod(x(i),x(i))
      end do
      xv = (xv - xm*xm/xn)/xn
      xmean = sngl(xm/xn)
      xvari = sngl(xv)
      xvaro = xvari

      if (iopt .lt. 0) return 

c must at least want the series mean removed (iopt=0)

      do i = 1, n
         x(i) = x(i) - xmean
      end do

      if (iopt .eq. 1) then

c must also want least squares trend line removed

         c(2) = 0.
         xcntr = float(n + 1)*0.5
         do i = 1, n
            c(2) = c(2) + x(i)*(float(i) - xcntr)
         end do

         c(2) = c(2)*12./float(n*(n*n - 1))
         c(1) = xmean - c(2)*xcntr               ! y-intercept

         do i = 1, n
            x(i) = x(i) - c(2)*(float(i) - xcntr)
         end do

      endif

c calculate the variance of the detrended series
c .   xbar should be zero to machine accuracy subtract it out anyway

      xm = 0.d0
      xv = 0.d0
      do i = 1, n
         xm = xm + dble(x(i))
         xv = xv + dprod(x(i),x(i))
      end do
      xv = (xv - xm*xm/xn)/xn
      xvaro = sngl(xv)
      xbar  = sngl(xm/xn)

      do i = 1, n
         x(i) = x(i) - xbar
      end do

      return 
      end 
c -----------------------------------------------------------------------
      subroutine taperx(x, n, p, xav, cft)
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
      dimension x(n)
      data pi/ 3.141592653589/ 

      m = max0(1,int(p*float(n) + 0.5)/2)
      pim = pi/float(m)
      do i = 1, m
         weight   = 0.5 - 0.5*cos(pim*(float(i) - 0.5))
         x(i)     = (x(i)-xav)*weight + xav
         x(n+1-i) = (x(n+1-i)-xav)*weight + xav
      end do
      cft = 0.5*(128. - 93.*p)/(8. - 5.*p)**2
      return 
      end 
c -----------------------------------------------------------------------
      subroutine swrnav(x, npts, wgt, nptav, kopt, spvl, work, ier)
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
c .                          x(1)=(wgt(1)*x(npts)+wgt(2)*x(1)+wgt(3)*x(2))
c .                              /(wgt(1)+wgt(2)+wgt(3))
c .               kopt = 0 : set unsmoothed beginning and end pts to spvl
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
      dimension x(npts), work(npts), wgt(nptav)

      ier = 0
      if (npts .lt. 1) ier = -11
      if (nptav .gt. npts) ier = -12
      if (mod(nptav,2) .eq. 0) ier = -13
      if (ier .lt. 0) return 

c dup the vectors

      do n = 1, npts
         work(n) = x(n)
      end do

      if (nptav .lt. 3) return 

c sum the wgts

      wsum = 0.0
      do n = 1, nptav
         wsum = wsum + wgt(n)
      end do
      if (wsum .gt. 1.0) then
         xnav = 1.0/wsum
      else
         xnav = 1.0
      endif

c do the middle pts first

      nav = nptav
      nav2 = nav/2
      nstrt = nav2 + 1
      nend = npts - nav2

      do n = nstrt, nend
         mstrt= n - nav2
         mend = n + nav2
         ml   = 0
         wsum = 0.
         do m = mstrt, mend
            ml = ml + 1
            wsum = wsum + work(m)*wgt(ml)
         end do
         x(n) = wsum*xnav
      end do
      if (kopt .ne. 0) then
         nav2p2 = nav2 + 2
         do n = 1, nav2
            mend = nav2 + n
            mlstrt = nav2p2 - n
            ml = mlstrt - 1
            wsum = 0.0
            do m = 1, mend
               ml = ml + 1
               wsum = wsum + work(m)*wgt(ml)
            end do
 
            mend = nav2p2 - n
            ml = mlstrt
            do m = 2, mend
               ml = ml - 1
               mloc = m
               if (kopt .lt. 0) mloc = npts + 1 - m
               wsum = wsum + work(mloc)*wgt(ml)
            end do

            x(n) = wsum*xnav
         end do

c do the final portion of x

         npts2 = 2*npts
         nstrt = npts - nav2 + 1
         do n = nstrt, npts
            mstrt = n - nav2
            mend = n + nav2
            ml = 0
            wsum = 0.0
            do m = mstrt, mend
               ml = ml + 1
               mloc = m
               if (mloc.gt.npts .and. kopt.gt.0) mloc = npts2 - mloc
               if (mloc.gt.npts .and. kopt.lt.0) mloc = mloc - npts
               wsum = wsum + work(mloc)*wgt(ml)
            end do
            x(n) = wsum*xnav
         end do
         return 
      endif
      do n = 1, nav2
         x(n) = spvl
      end do
      nstrt = npts - nav2 + 1
      do n = nstrt, npts
         x(n) = spvl
      end do

      return 
      end 
