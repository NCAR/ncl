c -------------------------------------------------------------
      subroutine regcoef (x,y,npts,dmsg         ! input
     *                   ,rcoef,tval,nptxy      ! output
     *                   ,xave,yave,ier)        ! output
      implicit none

c NCL:  rcoef = regcoef (x,y,tval,nptxy)
c NCL:  rcoef = regline (x,y,tval,nptxy,xave,yave)

c this routine will calculate the regression coef (trend, slope,..),
c .   x and y averages, the t-statistic and the number of pts used 
c .   missing data are allowed.
          
c .   see: brownlee
c .        statistical theory and methodology
c .        j wiley 1965   pgs: 281-284     QA276  .B77

c arguments :      
c .   x,y      - input vectors 
c .   npts     - length of vectors x and y    
c .   dmsg     - missing code: if no msg values set to some number  
c .                            which will not be encountered.      
c .              dmsg will be used to fill missing values         
c .   rcoef    - slope (trend ... regression coef)
c .   tval     - t-statistic (null hypothesis: H0: r=0.0)
c .   nptxy    - number of points used
c .   xave     - average of x
c .   yave     - average of y
c .   ier      - if (ier.ne.0) an error has occurred    
                                                       
      integer npts, nptxy                     ! input
      real    x(1:npts) , y(1:npts), dmsg     ! input
      integer ier                             ! output
      real    rcoef, tval, yave, xave         ! output

      logical debug             ! local 
      integer n
      real    rnull, xyn, df 
      double precision xsum, ysum, x2sum, y2sum, xysum, 
     +                 xvar, yvar, xyvar, ssqreg, ssq, vb, sqrtvb
                                
      ier   = 0                  
      tval  = dmsg
      rcoef = dmsg
      if (npts.lt.2) ier = 1  
      if (ier.ne.0) return 

      rnull = 0.0              ! null hypothesis
                                                   
      xyn   = 0.0
      xsum  = 0.d0
      ysum  = 0.d0
      x2sum = 0.d0
      y2sum = 0.d0
      xysum = 0.d0
      do n=1,npts
         if (x(n).ne.dmsg .and. y(n).ne.dmsg) then   
             xsum   = xsum   + dble(x(n))
             ysum   = ysum   + dble(y(n))
             x2sum  = x2sum  + dprod(x(n),x(n))
             y2sum  = y2sum  + dprod(y(n),y(n))
             xysum  = xysum  + dprod(x(n),y(n))
             xyn    = xyn    + 1.                       
         endif                                   
      enddo

      nptxy = xyn

      if (xyn.lt.1.) then
          ier = 5         ! all msg values
          return
      elseif (xyn.lt.3.) then
          ier = 6         ! not enough data
          return
      endif

      xave   = sngl(xsum)/xyn
      yave   = sngl(ysum)/xyn

      xvar   = x2sum-xsum*xsum/dble(xyn)
      yvar   = y2sum-ysum*ysum/dble(xyn)
      xyvar  = xysum-xsum*ysum/dble(xyn)

      rcoef  = sngl(xyvar/xvar)             ! regression coef (b in book)
      ssqreg = xyvar*(xyvar/xvar)           ! sum of squares due to regression
      ssq    = (yvar-ssqreg)/dble(xyn-2.)
      vb     = ssq/xvar                     ! v[b] in book {variance of B} 
      sqrtvb = dsqrt(vb)
      tval   = (rcoef-rnull)/sngl(dsqrt(vb))! t-statistic 
      df     = xyn-2.                       ! degrees of freedom 

      debug = .false.
      if (debug) then 
          write (*,'(///,'' trtest: debug'')')
          write (*,'('' xsum  ='',f12.5)')   sngl(xsum)
          write (*,'('' ysum  ='',f12.5)')   sngl(ysum)
          write (*,'('' x2sum ='',f12.5)')   sngl(x2sum)
          write (*,'('' y2sum ='',f12.5)')   sngl(y2sum)
          write (*,'('' xysum ='',f12.5)')   sngl(xysum)
          write (*,'('' xvar  ='',f12.5)')   sngl(xvar)
          write (*,'('' yvar  ='',f12.9)')   sngl(yvar)
          write (*,'('' xyvar ='',f12.9)')   sngl(xyvar)
          write (*,'('' ssqreg='',f12.9)')   sngl(ssqreg)
          write (*,'('' ssq   ='',f12.9)')   sngl(ssq)   
          write (*,'('' rcoef ='',f12.9)')   rcoef
          write (*,'('' rnull ='',f12.9)')   rnull
          write (*,'('' vb    ='',f15.9)')   sngl(vb)    
          write (*,'('' sqrtvb='',f15.9)')   sngl(sqrtvb)
          write (*,'('' tval  ='',f15.9)')   tval   
      endif  

      return
      end
