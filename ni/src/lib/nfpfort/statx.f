c ----------------------------------------------------------------------
      subroutine stat2 (x,npts,xmsg,xmean,xvar,xsd,nptused,ier)
 
c this routine will calculate estimates of the first two  moments
c .   of the vector x containing missing data.
 
c input arguments:
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
 
c output arguments:
c .   xmean    - mean of x (first moment)
c .   xvar     - sample variance
c .   xsd      - sqrt( xvar )
c .   nptused  - no. of points used to calculate the estimates
c .   ier      - if (ier.ne.0) an error has occurred
 
c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)
 
      integer npts  , nptused, ier
      real x(1:npts), xmsg, xmean, xvar, xsd
      double precision x1, x2, x3, temp
 
      xmean   = xmsg
      xvar    = xmsg
      xsd     = xmsg
      nptused = 0
 
      if (npts.lt.1) then
         ier = 1
         return
      endif
 
      ier = 0
      x1  = 0.d0
      x2  = 0.d0
      xn  = 0.
      do n=1,npts
         if (x(n).ne.xmsg) then
             xn = xn + 1.0
             x1 = x1 + dble(x(n))
             x2 = x2 + dprod( x(n),x(n) )  
         endif
      enddo
 
      nptused = xn
 
      if ((xn-1.).gt.0.) then
         x3    = x1*x1/dble(xn)
         temp  = (x2-x3)/dble(xn-1.)
         xvar  = sngl(dmax1( temp  , 0.d0 )) ! prevent possible roundoff
         xsd   = sqrt( xvar )
         xmean = sngl(x1/dble(xn))
      elseif ((xn-1.).eq.0.) then
         xmean = sngl(x1)
         xvar  = 0.0
         xsd   = 0.0
      else
         ier = 2    ! error code for all msg values
      endif
 
      return
      end
c ----------------------------------------------------------------------
      subroutine stat2t (x,npts,xmsg,xmeant,xvart,xsdt,nptused
     1                  ,work,ptrim,ier)
 
c this routine will calculate trimmed estimates of the first
c .   two moments of the vector x containing missing data.
 
c input arguments:
c .   x        - input vector
c .   npts     - length of x  (npts.ge.5 if pctrim.gt.0.)
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   work     - work vector of length npts
c .              upon return work contains x in ascending order
c .   ptrim    - portion of the series to be trimmed (0.<= pctrim <1.)
c .              if (pctrim.eq.0.) then
c .                  calculate whole series mean and st dev
c .              else
c .                  a minimum of two pts will be trimmed
c .                  a maximum of npts-2 will be trimmed
c .              endif
c .
c .              note: if u want to be very conservative and only
c .                    trim the min and max values then set
c .                    ptrim to some very small value (e.g. 1.e-36)
c .
c .              ex: npts=45, ptrim=0.50 then
c .                  0.5*45=22 pts will be trimmed (11 on each end)
 
c output arguments:
c .   xmeant   - mean of trimmed series x
c .   xvart    - sample trimmed variance
c .   xsdt     - sqrt( xvart ): st deviation of trimmed series
c .   nptused  - no. of points used to calculate the trimmed estimates
c .   ier      - if (ier.ne.0) an error has occurred
 
c note : uncalculated quantities are set to xmsg (nptused set to zero)
 
      real x(1:npts)  , work(1:npts)
 
      ier     = 0
      xmeant  = xmsg
      xvart   = xmsg
      xsdt    = xmsg
      nptused = 0
 
      if (npts.lt.1) then
         ier = 1
         return
      endif
 
      if (ptrim.eq.0.) then
          call stat2 (x,npts,xmsg,xmeant,xvart,xsdt,nptused,ier)
          return
      elseif (ptrim.eq.1.) then
          ier = 2     ! cannot trim whole series
          return
      endif
 
c sort x into numerically ascending order
c .   first transfer the input array to the work array
c .   excluding missing pts
 
      mpts = 0
      do n=1,npts
         if (x(n).ne.xmsg) then
             mpts       = mpts+1
             work(mpts) = x(n)
         endif
      enddo
 
      if (mpts.lt.1) then
          ier = 3                   ! series contains all msg values
          return
      elseif (mpts.lt.npts) then
          do m=mpts+1,npts          ! fill the rest of work with msg code
             work(m) = xmsg
          enddo
      endif
 
      call sortu(work,mpts)         ! sort series in ascending order
 
      mtrim = max0( int( float(mpts)*0.5*ptrim ), 1)   ! no. of values trimmed
                                                       ! from one side
 
      if ((mpts-2*mtrim).gt.0) then
          call stat2 (work(mtrim+1),mpts-2*mtrim,xmsg,xmeant
     1               ,xvart,xsdt,nptused,ier)
      else
          ier = 4                   ! not enough trimmed values
      endif
 
      return
      end
c ----------------------------------------------------------------------
      subroutine stat4 (x,npts,xmsg,xmean,xvar,xsd,xskew,xkurt
     1                 ,nptused,ier)
 
c this routine will calculate estimates of the first four moments
c .   of the vector x containing missing data.
 
c input arguments:
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
 
c output arguments:
c .   xmean    - mean of x (first moment)
c .   xvar     - sample variance of x (second moment)
c .   xsd      - sqrt( xvar )
c .   xskew    - coef. of skewness (third moment)
c .              departure from symmetry. if skew>0 [skew<0] the
c .              distribution trails off to the right [left] .
c .   xkurt    - coef. of kurtosis (fourth moment)
c .              the normal distribution has a kurtosis of 3 . this
c .              value is subtracted from the calculated kurtosis.
c .              thus neg values are possible and the returned value
c .              is kurtosis relative to the normal distribution.
c .              if (kurt-3) > 0 [<0] it is usually more sharply peaked
c .              [flatter] than the normal distribution. (leptokurtic and
c .              platykurtic , respectively.) e.g. , a rectangular function has
c .              a kurtosis of 1.8 or 1.8-3 = -1.2 relative to the normal
c .              distribution.
c .   nptused  - no. of points used to calculate the estimates
c .   ier      - if (ier.ne.0) an error has occurred
 
c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)
 
      real x(1:npts)
      double precision x1, x2, x3, x4, dxvar
 
      xmean = xmsg
      xvar  = xmsg
      xsd   = xmsg
      xskew = xmsg
      xkurt = xmsg
      nptused = 0
 
 
      if (npts.lt.1) then
         ier = 1
         return
      endif
 
      ier = 0
      x1  = 0.d0
      x2  = 0.d0
      xn  = 0.
      do n=1,npts
         if (x(n).ne.xmsg) then
             xn = xn + 1.0
             x1 = x1 + dble(x(n))
             x2 = x2 + dprod( x(n),x(n) )
         endif
      enddo
 
      nptused = xn
 
      if ((xn-1.).gt.0.) then
         x3    = x1*x1/dble(xn)
         dxvar = dmax1( (x2-x3)/dble(xn-1.) , 0.d0 ) !prevent possible roundoff
         xvar  = sngl( dxvar )
         xsd   = sqrt( xvar )
         xmean = sngl(x1/dble(xn))
 
         if (dxvar.gt.0.d0) then
             x1 = 0.d0
             x2 = 0.d0
             x3 = 0.d0
             x4 = 0.d0
             do n=1,npts
                if (x(n).ne.xmsg) then
                    x4 = dble(x(n)-xmean)
                    x1 = x4**3
                    x2 = x2 + x1
                    x3 = x3 + x1*x4
                endif
             enddo
             xskew = sngl( (x2/dsqrt(dxvar)**3)/dble(xn))
             xkurt = sngl( (x3/(dxvar*dxvar))/dble(xn)) - 3.
         endif
      elseif ((xn-1.).eq.0.) then
         xmean = sngl(x1)
         xvar  = 0.0
         xsd   = 0.0
      else
         ier = 2    ! error code for all msg values
         return
      endif
 
      return
      end
c ----------------------------------------------------------------------
      subroutine medmrng (x,work,npts,xmsg,xmedian,xmrange
     1                   ,xrange,nptused,ier)
 
c this routine will find the median, range and mid-range
c .   of the input  vector x containing missing data.
 
c arguments :
c .   x        - input vector (series)
c .   work     - work vector of length npts. upon output
c .              work(1) thru work(nptused) are in numerically ascending
c .              order. work(nptused+1) thru work(npts) will be filled
c .              the missing value code.
c .   npts     - length of x
c .   xmsg     - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .   xmedian  - median value of vector x
c .   xmrange  - midrange
c .   xrange   - range of x
c .   nptused  - no. of points used in computing various quantities
c .   ier      - if (ier.ne.0) an error has occurred
 
c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)
 
      real x(1:npts) , work(1:npts)
 
      xmedian = xmsg
      xmrange = xmsg
      xrange  = xmsg
      nptused = 0
 
      if (npts.lt.1) then
         ier = 1
         return
      endif
      ier     = 0
 
c sort x into numerically ascending order
c .   first transfer the input array to the work array
c .   excluding missing pts
 
      mpts = 0
      do n=1,npts
         if (x(n).ne.xmsg) then
             mpts       = mpts+1
             work(mpts) = x(n)
         endif
      enddo
 
      if (mpts.lt.1) then
          ier = 2                     ! series contains all msg values
          return
      endif
 
      call sortu(work,mpts)
 
      if (mpts.lt.npts) then
          do n=mpts+1,npts            ! fill the rest of work with msg code
             work(n) = xmsg
          enddo
      endif
 
      xmrange = 0.5*( work(mpts)+work(1) )
      xrange  =       work(mpts)-work(1)
 
      if (amod(float(mpts),2.).eq.0.) then
          xmedian = 0.5*(work(mpts/2)+work(mpts/2+1) )
      else
          xmedian  = work((mpts+1)/2)
      endif
 
      nptused = mpts          ! for nomenclature compatibility
 
      return
      end
c ----------------------------------------------------------------------
      subroutine esauto (x,npts,xmsg,xmean,xvar,mxlag,acv,acr,ier)
 
c this routine will estimate the autocovariances and autocorrelations
c .   for the input vector x with missing data. the normalizing factor
c .   in the denominator (xvar) is kept constant.
 
c arguments :
c .   x        - input vector
c .   npts     - length of vector x
c .   xmsg     - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .   xmean    - mean of the input vector x
c .   xvar     - sample variance  of the input vector x
c .              note: if xmean and/or xvar = xmsg this routine will
c .                    calculate the quantities and return them
c .   mxlag    - max lag to be estimated  [0 <= mxlag <= npts ]
c .   acv      - sample autocovariances  [vector of length .ge. (mxlag+1) ]
c .   acr      - sample autocorrelations [vector of length .ge. (mxlag+1) ]
 
c .              acv(0)     = xvar (lag 0  )  : acr(0)       = 1.00 ( lag 0  )
c .              acv(1)     =      (lag 1  )  : acr(1)       =      ( lag 1  )
c .                         .                              .
c .                         .                              .
c .              acv(mxlag) =      (lag mxlag): acr(mxlag) =      ( lag mxlag)
 
c .   ier      - if (ier.ne.0) an error has occurred
 
c note :  uncalculated quantities are set to xmsg
 
      real x(1:npts) , acv(0:mxlag) , acr(0:mxlag)
      double precision x1
 
      ier = 0
      if (npts.lt.2) ier = 1
      if (mxlag.lt.0 .or. mxlag.gt.npts) ier = 2
      if (xvar .le.0. .and. xvar .ne.xmsg)    ier = 3
      if (ier.ne.0) then
          do lag=0,max0 (0, min0(mxlag,npts) )
             acv(lag) = xmsg
             acr(lag) = xmsg
          enddo
          return
      endif
 
      if (xmean.eq.xmsg .or. xvar.eq.xmsg) then
         call stat2 (x,npts,xmsg,xmean,xvar,xsd,nptused,jer)
         if (jer.ne.0) then
             ier = -jer
             return
         elseif (xvar.eq.0.) then
             ier = -5
             return
         endif
      endif
 
      acv(0) = xvar
      acr(0) = 1.00
 
      if (mxlag.eq.0) return       ! i hope somebody wants more than this
 
      do lag=1,mxlag
         x1 = 0.d0
         xn = 0.
       do n=1,npts-lag
          if (x(n).ne.xmsg .and. x(n+lag).ne.xmsg) then
              x1 = x1 + dprod( (x(n+lag)-xmean) , (x(n)-xmean) )
              xn = xn + 1.
          endif
       enddo
         if (xn.ge.2.) then
             acv(lag) = sngl(x1/dble(xn-1.))
             acr(lag) = acv(lag)/xvar
         else
             acv(lag) = xmsg
             acr(lag) = xmsg
         endif
      enddo
 
      return
      end
c ----------------------------------------------------------------------
      subroutine escros (x,y,npts,xmsg,ymsg,xmean,ymean,xsd,ysd
     1                  ,mxlag,ccv,ccr,ier)
 
 
c this routine will estimate the crosscovariances and crosscorrelations
c .   for the input series x and y. missing data is allowed.
 
c .   to get the (+) and (-) ccv and ccr two calls must be made. the
c .   first with the calling sequence above and the second with the
c .   x and y series reversed (also the other arguments) . note
c .   that  the second ccv(0) and ccr(0) are redundant.
 
c arguments :
c .   x,y      - input vectors with which the calculations are to take place.
c .   npts     - length of vectors x and y
c .   xmsg,ymsg- missing code: if no msg values set to some number
c .                            which will not be encountered.
c .              xmsg will be used to fill missing values
c .   xmean    - mean of the x vector (if xmean=xmsg: program will calculate)
c .   ymean    - mean of the y vector (if ymean=ymsg: program will calculate)
c .   xsd      - st. dev. of x vector (if xsd  =xmsg: program will calculate)
c .   ysd      - st. dev. of y vector (if ysd  =ymsg: program will calculate)
c .   mxlag    - max lag crosscovariance/correlation to be estimated
c .   ccv      - vector of length .ge. (mxlag+1) containing sample cross cov
c .   ccr      - vector of length .ge. (mxlag+1) containing sample cross cor
 
c .                ccv(0)     = xyvar (lag 0  )  : ccr(0)     =  ( lag 0   )
c .                ccv(1)     =       (lag 1  )  : ccr(1)     =  ( lag 1   )
c .                           .                               .
c .                           .                               .
c .                ccv(mxlag) =       (lag mxlag): ccr(mxlag) =  ( lag mxlag)
 
c .   ier      - if (ier.ne.0) an error has occurred
 
c note : uncalculated quantities are set to xmsg
 
      real x(1:npts) , y(1:npts) , ccv(0:mxlag) , ccr(0:mxlag)
      double precision xy1
 
      ier = 0
      if (npts.lt.2) ier = 1
      if (mxlag.lt.0 .or. mxlag.gt.npts) ier = 2
      if ((xsd.ne.xmsg .and. xsd.le.0.) .or.
     *    (ysd.ne.ymsg .and. ysd.le.0.)) ier = 3
      do lag=0,max0( 0, min0(mxlag,npts) )
         ccv(lag) = xmsg
         ccr(lag) = xmsg
      enddo
      if (ier.ne.0) return
 
      if (xmean.eq.xmsg .or. xsd.eq.xmsg) then
         call stat2 (x,npts,xmsg,xmean,xvar,xsd,nptused,jer)
         if (jer.ne.0) then
             ier = -jer
         elseif (xsd.eq.0.) then
             ier = -5              ! x must be a series of constant values
         elseif (nptused.eq.0) then
             ier = -6              ! x must be a series of missing values
         endif
         if (ier.ne.0) return
      endif
 
      if (ymean.eq.ymsg .or. ysd.eq.ymsg) then
         call stat2 (y,npts,ymsg,ymean,yvar,ysd,nptused,jer)
         if (jer.ne.0) then
             ier = -(jer+100)
         elseif (ysd.eq.0.) then
             ier = -105            ! y must be a series of constant values
         elseif (nptused.eq.0) then
             ier = -106            ! y must be a series of missing values
         endif
         if (ier.ne.0) return
      endif
 
      xsdysd = sngl(1.d0/dprod(xsd,ysd))
 
      do lag=0,mxlag
         xyn = 0.
         xy1 = 0.d0
       do n=1,npts-lag
          if (x(n).ne.xmsg .and. y(n+lag).ne.ymsg) then
              xy1 = xy1 + dprod((y(n+lag)-ymean),(x(n)-xmean))
              xyn = xyn + 1.
          endif
       enddo
         if (xyn.ge.2.) then
             ccv(lag) = sngl(xy1/dble(xyn-1.))
             ccr(lag) = ccv(lag)*xsdysd
         else
             ccv(lag) = xmsg
             ccr(lag) = xmsg
         endif
      enddo
 
      return
      end
c ----------------------------------------------------------------------
      subroutine rmvave  (x,npts,xmsg,xmean,ier)
 
c this routine will remove the mean of vector x from each pt.
c .   missing pts are excepted.
 
c arguments :
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xmean    - mean of the input vector (x)
c .   ier      - if (ier.ne.0) an error has occurred
 
      real x(1:npts)
 
      if (npts.lt.1) then
          ier = 1
          return
      else
          ier = 0
      endif
 
      do n=1,npts
         if (x(n).ne.xmsg) x(n) = x(n) - xmean
      enddo
 
      return
      end
c ----------------------------------------------------------
      subroutine stndx (x,npts,xmsg,xmean,xsd,ier)
 
c standardize the vector x
 
c arguments :
c .   x        - vector to be standardized
c .   npts     - length of vector x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xmean    - mean of the input vector
c .   xsd      - standard deviation of the series
c .   ier      - if (ier.ne.0) an error has ocurred
 
      real x(1:npts)
 
      ier = 0
      if (npts.lt.1) then
          ier = 1
      elseif (xsd.le.0.) then
          ier = 2
      endif
      if (ier.ne.0) return
 
      do n=1,npts
         if (x(n).ne.xmsg) x(n) = (x(n)-xmean)/xsd
      enddo
 
      return
      end
c ----------------------------------------------------------------------
      subroutine nmdata (x,npts,xmsg,ier)
 
c normalize the vector x by the absolute value of its largest element
 
c arguments :
c .   x        - series (vector) to be normalized
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   ier      - if (ier.ne.0) an error has occurred
 
      real x(1:npts)
 
      ier = 0
      if (npts.lt.1) then
          ier = 1
          return
      endif
 
c find the first non-missing pt
 
      do n=1,npts
      if (x(n).ne.xmsg) then
          nstrt = n
          go to 20
      endif
      enddo
 
      ier = 1      ! must be all msg values
      return
 
   20 xmax = abs( x(nstrt) )
      do n=nstrt,npts
         if (x(n).ne.xmsg) xmax = amax1( abs(x(n)) , xmax )
      enddo
 
      if (xmax.ne.0.) then
          do n=nstrt,npts
             if (x(n).ne.xmsg) x(n) = x(n)/xmax
          enddo
      else
          ier = 2 ! must be all zeros
      endif
 
      return
      end
c ----------------------------------------------------------------------
      subroutine errchk (x,npts,xmean,xsd,xmsg,xband,locerr,npter,ier)
 
c this routine will check a series of data for potential gross errors
c .   (i.e., wild points). the series will be scanned and the locations
c .   of possibly bad points are stored. this routine
c .   assumes that the series x is normally distributed.
 
c input arguments:
c .   x        - series (vector) to be checked
c .   npts     - length of x
c .   xmean    - series mean (... or median is the user chooses)
c .   xsd      - series standard deviation
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xband    - standard deviation width to be allowed
c .              (e.g., xband=3.0; check values beyond 99%)
 
c output arguments:
c .   locerr   - integer vector which will contain the subscript location of
c .              potential gross errors. for example: if series x has
c .              two pts more than xband from xmean and these pts are
c .              located at x(17) and x(204) then locerr(1) = 17 and
c .              locerr(2) = 204.
c .   npter    - no. of pts in locerr. in the above example it would be
c .              two. if a value of zero is returned no gross errors
c .              have been detected.
c .   ier      - if (ier.ne.0) an error has occurred
 
      real    x(1:npts)
      integer locerr(1:npts)
 
      ier = 0
      if (npts.lt.1) then
          ier = 1
      elseif (xsd.eq.0.) then
          ier = 2
      endif
      if (ier.ne.0) return
 
      nn     = 0
      npter  = 0
      xwidth = xband*xsd
      do n=1,npts
         if (x(n).ne.xmsg .and. abs(x(n)-xmean).gt.xwidth) then
             npter         = npter+1   ! must be a potential gross error
             locerr(npter) = n
         endif
      enddo
 
      return
      end
c ----------------------------------------------------------------------
      subroutine dotpr (x,y,npts,xmsg,ymsg,ans,nptused,ier)
 
c compute the dot product for series containing msg values
 
c arguments :
c .   x,y       - input series
c .   npts      - length of x and y
c .   xmsg,ymsg - missing code: if there are no msg values
c .                             set xmsg,ymsg to some value which will
c .                             not be encountered.
c .   ans       - answer (dot product)
c .   nptused   - number of points used to compute the dot product
c .   ier       - if (ier.ne.0) an error has occurred
 
      dimension x(1:npts) , y(1:npts)
      double precision temp
 
      ier = 0
      if (npts.lt.1) then
          ier = 1
          return
      endif
 
      temp    = 0.d0
      nptused = 0
      do n=1,npts
         if (x(n).ne.xmsg .and. y(n).ne.ymsg ) then
             temp    = temp + dprod( x(n),y(n) )
             nptused = nptused + 1
         endif
      enddo

      ans = sngl(temp)
 
      return
      end
c ----------------------------------------------------------------------
      subroutine vcvmns (x,nrow,ncol,nrt,ncs,xmsg,vcm,lvcm,ier)
 
c this routine will calculate the variance-covariance matrix (vcm)
c .   of the array x containing missing data. obviously if x does contain
c .   missing data then vcm is only an approximation.
 
c note : conventional storage is utilized for vcm
 
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c     vcm      - var-cov matrix
c     lvcm     - not used in this routine
c     ier      - error code (if ier=-1 then vcm contains missing entry)
 
      real x(1:nrow,1:ncol) , vcm(1:ncol,1:ncol)
      double precision xca, xcb, xcacb   
 
      ier=0
 
c calculate the var-cov between columns (stations)
 
      do nca=1,ncs
       do ncb=nca,ncs
          xn    = 0.
          xca   = 0.
          xcb   = 0.
          xcacb = 0.
        do i=1,nrt
           if (x(i,nca).ne.xmsg .and. x(i,ncb).ne.xmsg) then
               xn    = xn  + 1.
               xca   = xca + dble(x(i,nca))
               xcb   = xcb + dble(x(i,ncb))
               xcacb = xcacb + dprod( x(i,nca),x(i,ncb) )
           endif
        enddo    ! end "nrt"
          if (xn.ge.2.) then
              vcm(nca,ncb)= sngl((xcacb-(xca*xcb)/dble(xn))/dble(xn-1.))
              vcm(ncb,nca)= vcm(nca,ncb)
          else
              ier = -1
              vcm(nca,ncb) = xmsg
              vcm(ncb,nca) = xmsg
          endif
       enddo     ! end "ncs"
      enddo      ! end "ncs"
 
      return
      end
c ----------------------------------------------------------------------
      subroutine vcmssm (x,nrow,ncol,nrt,ncs,xmsg,vcm,lvcm,ier)
 
c this routine will calculate the variance-couariance matrix (vcm)
c .   of the array x containing missing data. obviously if x does contain
c .   missing data then vcm is only an approximation.
 
c note : symmetric storage mode is utilized for vcm to save space.
 
c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c output:
c     vcm      - var-cov matrix
c     lvcm     - length of vcm
c     ier      - error code (if ier=-1 then vcm contains missing entry)
 
      dimension x(1:nrow,1:ncol) , vcm(*)
      double precision xca, xcb, xcacb   
 
      ier  = 0
      if (nrow.lt.1 .or. ncol.lt.1) ier = ier+1
      if (nrt .lt.1 .or. ncs .lt.1) ier = ier+10
      if (ier.ne.0) return
 
      lvcm = ncs*(ncs+1)/2
 
c calculate the var-cov between columns (stations)
 
      nn = 0
      do nca=1,ncs
       do ncb=1,nca
          xn    = 0.
          xca   = 0.d0
          xcb   = 0.d0
          xcacb = 0.d0
          nn    = nn + 1
        do i=1,nrt
           if (x(i,nca).ne.xmsg .and. x(i,ncb).ne.xmsg) then
               xn    = xn + 1.
               xca   = xca + dble(x(i,nca))
               xcb   = xcb + dble(x(i,ncb))
               xcacb = xcacb + dprod( x(i,nca) , x(i,ncb) )
           endif
        enddo
          if (xn.ge.2.) then
              vcm(nn) = sngl( (xcacb-(xca*xcb)/dble(xn) )/dble(xn-1.)) 
          elseif (xn.eq.1.) THEN
             vcm(nn) = ( xcacb-(xca*xcb)/xn )
          else
              ier     = -1
              vcm(nn) = xmsg
          endif
       enddo
      enddo
 
      return
      end
c
c ----------------------------------------------------------------------
      subroutine cormns (x,nrow,ncol,nrt,ncs,xmsg,crm,lcrm,ier)
 
c this routine will calculate the correlation matrix   (crm)
c .   of the array x containing missing data. obviously if x does contain
c .   missing data then crm is only an approximation.
 
c note : conventional storage
 
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c     crm      - correlation matrix  [full]
c     lcrm     - dummy [not used in this routine]
c     ier      - error code (if ier=-1 then crm contains missing entry)
 
      real x(1:nrow,1:ncol) , crm(1:ncol,1:ncol)
      double precision xca, xcb, xca2, xcb2, xcacb, xvara, xvarb, temp 
 
      ier  = 0
      lcrm = ncs*(ncs+1)/2
 
c calculate the var-cov between columns (stations)
c .   then standardize
 
      do nca=1,ncs
       do ncb=1,nca
          xn    = 0.
          xca   = 0.d0
          xcb   = 0.d0
          xca2  = 0.d0
          xcb2  = 0.d0
          xcacb = 0.d0
        do i=1,nrt
           if (x(i,nca).ne.xmsg .and. x(i,ncb).ne.xmsg) then
               xn    = xn + 1.
               xca   = xca + dble(x(i,nca))
               xcb   = xcb + dble(x(i,ncb))
               xca2  = xca2 + dprod(x(i,nca),x(i,nca))
               xcb2  = xcb2 + dprod(x(i,ncb),x(i,ncb))
              xcacb = xcacb + dprod(x(i,nca),x(i,ncb))
          endif
        enddo    
          if (xn.ge.2.) then
              xvara   =  ( xca2-((xca*xca)/dble(xn)) )/dble(xn-1.)
              xvarb   =  ( xcb2-((xcb*xcb)/dble(xn)) )/dble(xn-1.)
              if (xvara.gt.0. .and. xvarb.gt.0.) then
                  temp  = (xcacb-((xca*xcb)/dble(xn)))/dble(xn-1.)
                  crm(ncb,nca) = sngl(temp/(dsqrt(xvara)*dsqrt(xvarb)))
                  crm(nca,ncb) = crm(ncb,nca) ! symmetric 
              else
                  ier     = -1
                  crm(ncb,nca) = xmsg
                  crm(nca,ncb) = xmsg
              endif
          else
              ier     = -1
              crm(ncb,nca) = xmsg
              crm(nca,ncb) = xmsg
          endif
       enddo
      enddo
 
      return
      end
c ----------------------------------------------------------------------
      subroutine crmssm (x,nrow,ncol,nrt,ncs,xmsg,crm,lcrm,ier)
 
c this routine will calculate the correlation matrix   (crm)
c .   of the array x containing missing data. obviously if x does contain
c .   missing data then crm is only an approximation.
 
c note : symmetric storage mode is utilized for crm to save space.
 
c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c output:
c     crm      - correlation matrix
c     lcrm     - length of crm
c     ier      - error code (if ier=-1 then crm contains missing entry)
 
      real x(1:nrow,1:ncol) , crm(*)
      double precision xca, xcb, xca2, xcb2, xcacb, xvara, xvarb, temp 
 
      ier  = 0
      if (nrow.lt.1 .or. ncol.lt.1) ier = ier+1
      if (nrt .lt.1 .or. ncs .lt.1) ier = ier+10
      if (ier.ne.0) return
 
      lcrm = ncs*(ncs+1)/2
 
c calculate the var-cov between columns (stations)
 
      nn   = 0
      do nca=1,ncs
       do ncb=1,nca
          xn    = 0.
          xca   = 0.d0
          xcb   = 0.d0
          xca2  = 0.d0
          xcb2  = 0.d0
          xcacb = 0.d0
          nn    = nn + 1
        do i=1,nrt
           if (x(i,nca).ne.xmsg .and. x(i,ncb).ne.xmsg) then
               xn    = xn + 1.
               xca   = xca   + dble(x(i,nca))
               xcb   = xcb   + dble(x(i,ncb))
               xca2  = xca2  + dprod(x(i,nca),x(i,nca))
               xcb2  = xcb2  + dprod(x(i,ncb),x(i,ncb))
               xcacb = xcacb + dprod(x(i,nca),x(i,ncb))
           endif
        enddo
          if (xn.ge.2.) then
              xvara   =  ( xca2-((xca*xca)/dble(xn)) )/dble(xn-1.)
              xvarb   =  ( xcb2-((xcb*xcb)/dble(xn)) )/dble(xn-1.)
              if (xvara.gt.0.d0 .and. xvarb.gt.0.d0) then
                  temp    = ( xcacb-((xca*xcb)/dble(xn)) )/dble(xn-1.)
                  crm(nn) = sngl( temp/(dsqrt(xvara)*dsqrt(xvarb)) )
              else
                  ier     = -1
                  crm(nn) = xmsg
              endif
           elseif (xn.eq.1.) then
              xvara   =  ( xca2-((xca*xca)/(xn)) )
              xvarb   =  ( xcb2-((xcb*xcb)/(xn)) )
              if (xvara.gt.0. .and. xvarb.gt.0.) then
                  crm(nn) = ( xcacb-((xca*xcb)/(xn)) )
                  crm(nn) = crm(nn)/(sqrt(xvara)*sqrt(xvarb))
              else
                  ier     = -1
                  crm(nn) = xmsg
              endif
          else
              ier     = -1
              crm(nn) = xmsg
          endif
       enddo
      enddo
 
      return
      end

c ----------------------------------------------------------------------
      subroutine ccorc (y,x,nrow,ncol,nrt,ncs,xmsg,corc,ier)
 
c this routine will calculate the correlation coef between a
c .   vector (station) y and a matrix x containing observations from
c .   many different stations
 
c     y        - vector (station data) with which data matrix x is to
c                be correlated
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c     corc     - correlation coef. vector
c                corc(1) contain correlation coef between y and x(nrt,1)
c                corc(2) contain correlation coef between y and x(nrt,2) etc.
c     ier      - error code (if ier=-1 then corc contains missing entry)
 
      real x(1:nrow,1:ncol) , y(*) , corc(*)
      double precision xy, temp
 
c calculate the mean and var of y
 
      call stat2 (y,nrt,xmsg,ymean,yvar,ysd,nptusy,ier)
      if (ier.ne.0) return
      if (ysd .eq.0.) then
          do nc=1,ncs
             corc(nc) = xmsg
          enddo
          ier = 201
          return
      endif
 
c calculate the cross correlation coef
c .   calculate the mean and variance of column nc in the data array
 
      do nc=1,ncs
         corc(nc) = xmsg
         call stat2 (x(1,nc),nrt,xmsg,xmean,xvar,xsd,nptusx,jer)
         if (jer.ne.0) then
             ier = 205
             go to 40
         endif
         if (xsd.ne.0.) then
             xn = 0.
             xy = 0.d0
             do n=1,nrt
                if (y(n).ne.xmsg .and. x(n,nc).ne.xmsg) then
                    xn = xn + 1.
                    xy = xy + dprod( (y(n)-ymean),(x(n,nc)-xmean) ) 
               endif
             enddo
             if (xn.gt.2.) then
                 temp     = xy/dble(xn-1.)
                 corc(nc) = sngl(temp/dprod(xsd,ysd))
             endif
         endif
   40    continue
      enddo
 
      return
      end
c ------------------------------------------------------------------------------
      subroutine ssmiox (x,ncs)
 
c output a symmetric storage mode matrix [x]
 
c note: format statement have to be changed
 
c .   x(1)
c .   x(2)  x(3)
c .   x(4)  x(5)  x(6)
c .   etc.
 
      real x(1:*)
 
      data ipr/6/
 
      ncend=0
      do nc=1,ncs
         ncstrt = ncend+1
         ncend  = ncstrt+nc-1
         write(ipr,'(2x,i5,10(1x,f12.3),/, (7x,(10(1x,f12.3))))')
     *               nc,(x(n),n=ncstrt,ncend)
      enddo
 
      return
      end
c ------------------------------------------------------------
      real function epsmach (ipr)

      integer ipr    ! =1 print else no print
      real    eps

      eps = 1.0
    1 eps = 0.5*eps
      if ((1.0+eps) .gt. 1.0) go to 1
      epsmach = 2.0*eps
      if (ipr.eq.1) write (*,"('EPSMACH: eps=',e15.7)") epsmach

      return
      end

c ----------------------------------------------------------------------
      subroutine sortu  (a,la)                                          0340

c sort type "real"
 
      integer            la                                             0360
      real               a(la)                                          0370
 
      integer            iu(21),il(21),i,m,j,k,ij,l                     0390
      real               t,tt,r                                         0400
 
      m=1                                                               0420
      i=1                                                               0430
      j=la                                                              0440
      r=.375                                                            0450
      if (la.le.0) return                                               0460
   10 if (i .eq. j) go to 55                                            0470
   15 if (r .gt. .5898437) go to 20                                     0480
      r=r+3.90625e-2                                                    0490
      go to 25                                                          0500
   20 r=r-.21875                                                        0510
   25 k=i                                                               0520
c                                  select a central element of the      0530
c                                  array and save it in location t      0540
      ij=i+(j-i)*r                                                      0550
      t=a(ij)                                                           0560
c                                  if first element of array is greater 0570
c                                  than t, interchange with t           0580
      if (a(i) .le. t) go to 30                                         0590
      a(ij)=a(i)                                                        0600
      a(i)=t                                                            0610
      t=a(ij)                                                           0620
   30 l=j                                                               0630
c                                  if last element of array is less than0640
c                                  t, interchange with t                0650
      if (a(j) .ge. t) go to 40                                         0660
      a(ij)=a(j)                                                        0670
      a(j)=t                                                            0680
      t=a(ij)                                                           0690
c                                  if first element of array is greater 0700
c                                  than t, interchange with t           0710
      if (a(i) .le. t) go to 40                                         0720
      a(ij)=a(i)                                                        0730
      a(i)=t                                                            0740
      t=a(ij)                                                           0750
      go to 40                                                          0760
   35 if(a(l).eq.a(k)) go to 40                                         0770
      tt=a(l)                                                           0780
      a(l)=a(k)                                                         0790
      a(k)=tt                                                           0800
c                                  find an element in the second half of0810
c                                  the array which is smaller than t    0820
   40 l=l-1                                                             0830
      if (a(l) .gt. t) go to 40                                         0840
c                                  find an element in the first half of 0850
c                                  the array which is greater than t    0860
   45 k=k+1                                                             0870
      if (a(k) .lt. t) go to 45                                         0880
c                                  interchange these elements           0890
      if (k .le. l) go to 35                                            0900
c                                  save upper and lower subscripts of   0910
c                                  the array yet to be sorted           0920
      if (l-i .le. j-k) go to 50                                        0930
      il(m)=i                                                           0940
      iu(m)=l                                                           0950
      i=k                                                               0960
      m=m+1                                                             0970
      go to 60                                                          0980
   50 il(m)=k                                                           0990
      iu(m)=j                                                           1000
      j=l                                                               1010
      m=m+1                                                             1020
      go to 60                                                          1030
c                                  begin again on another portion of    1040
c                                  the unsorted array                   1050
   55 m=m-1                                                             1060
      if (m .eq. 0) return                                              1070
      i=il(m)                                                           1080
      j=iu(m)                                                           1090
   60 if (j-i .ge. 11) go to 25                                         1100
      if (i .eq. 1) go to 10                                            1110
      i=i-1                                                             1120
   65 i=i+1                                                             1130
      if (i .eq. j) go to 55                                            1140
      t=a(i+1)                                                          1150
      if (a(i) .le. t) go to 65                                         1160
      k=i                                                               1170
   70 a(k+1)=a(k)                                                       1180
      k=k-1                                                             1190
      if (t .lt. a(k)) go to 70                                         1200
      a(k+1)=t                                                          1210
      go to 65                                                          1220
      end                                                               1230
c ----------------------------------------------------------------------
      subroutine isortu  (a,la)
 
c sort type "integer"
 
      integer            la
      integer            a(la)
 
      integer            iu(21),il(21),i,m,j,k,ij,l
      integer            t,tt
      real               r
 
      m=1
      i=1
      j=la
      r=.375
      if (la.le.0) return
   10 if (i .eq. j) go to 55
   15 if (r .gt. .5898437) go to 20
      r=r+3.90625e-2
      go to 25
   20 r=r-.21875
   25 k=i
c                                  select a central element of the
c                                  array and save it in location t
      ij=i+(j-i)*r
      t=a(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (a(i) .le. t) go to 30
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
   30 l=j
c                                  if last element of array is less than
c                                  t, interchange with t
      if (a(j) .ge. t) go to 40
      a(ij)=a(j)
      a(j)=t
      t=a(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (a(i) .le. t) go to 40
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      go to 40
   35 if(a(l).eq.a(k)) go to 40
      tt=a(l)
      a(l)=a(k)
      a(k)=tt
c                                  find an element in the second half of
c                                  the array which is smaller than t
   40 l=l-1
      if (a(l) .gt. t) go to 40
c                                  find an element in the first half of
c                                  the array which is greater than t
   45 k=k+1
      if (a(k) .lt. t) go to 45
c                                  interchange these elements
      if (k .le. l) go to 35
c                                  save upper and lower subscripts of
c                                  the array yet to be sorted
      if (l-i .le. j-k) go to 50
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 60
   50 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 60
c                                  begin again on another portion of
c                                  the unsorted array
   55 m=m-1
      if (m .eq. 0) return
      i=il(m)
      j=iu(m)
   60 if (j-i .ge. 11) go to 25
      if (i .eq. 1) go to 10
      i=i-1
   65 i=i+1
      if (i .eq. j) go to 55
      t=a(i+1)
      if (a(i) .le. t) go to 65
      k=i
   70 a(k+1)=a(k)
      k=k-1
      if (t .lt. a(k)) go to 70
      a(k+1)=t
      go to 65
      end


