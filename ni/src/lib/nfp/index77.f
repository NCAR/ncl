c --------------------------------------------------------------
      subroutine indx77 (x,y,nmos,nyrs,xmsg,iprnt  ! input
     *                   ,work,lwork                ! input
     *                   ,zi,zni,ier)               ! output

c routine to call indx77x which actually does the calculations
c .   this is just a short calling list 
c NOTE: indx77x can also be called directly 
c .   DSJ: this is a variant of index routines

      integer nmos           ! number of "months" (eg: 12, 4, 1)
     *      , nyrs           ! number of years of data
     *      , iprnt          ! print flag (=0 mean do not print)
     *      , lwork          ! length of work (>= 2*nmos*nyrs + 4*nmos + nyrs)
     *      , ier            ! error code  

      real    x(nmos,nyrs)   ! monthly data from station/grid pt 1
     *   ,    y(nmos,nyrs)   ! monthly data from station/grid pt 2
     *   ,    xmsg           ! missing code (if any)
     *   ,    zi(nmos,nyrs)  ! index
     *   ,    zni(nmos,nyrs) ! noise index
     *   ,    work(lwork)    ! work array that will be partitioned

                             ! local  
      real    xastd          ! overall st dev of x anomalies 
     *   ,    yastd          ! overall st dev of y anomalies 
     

      ier = 0
      if (nmos.le.0 .or. nyrs.le.0)  ier = ier + 1
      if (lwork.lt.(2*nmos*nyrs+4*nmos+nyrs)) ier = 19

c Check if either input arrays contains all missing values.
      ixmiss = 1
      iymiss = 1
      do nyr=1,nyrs
        do nmo=1,nmos
           if (x(nmo,nyr).ne.xmsg) then
              ixmiss = 0
           endif
           if (y(nmo,nyr).ne.xmsg) then
              iymiss = 0
           endif
         enddo
      enddo
      if(ixmiss.eq.1.or.iymiss.eq.1) then
         ier = 2
         return
      endif

      if (ier.ne.0) then
          do nyr=1,nyrs
            do nmo=1,nmos
               zi (nmo,nyr) = xmsg
               zni(nmo,nyr) = xmsg
            enddo
          enddo

          return
      endif

                             ! set up pointers (addresses in work array)
      i1 = 1                 ! beginning of "xa"   (see indx77x)
      i2 = i1 + nmos*nyrs    ! beginning of "ya"   (see indx77x)
      i3 = i2 + nmos*nyrs    ! beginning of "xave" (see indx77x)
      i4 = i3 + nmos         ! beginning of "yave" (see indx77x)
      i5 = i4 + nmos         ! beginning of "xstd" (see indx77x)
      i6 = i5 + nmos         ! beginning of "ystd" (see indx77x)
      i7 = i6 + nmos         ! beginning of "wyrs" (see indx77x)

      call indx77x (x,y,nmos,nyrs,xmsg,iprnt            ! input
     *               ,work(i1),work(i2),work(i3),work(i4)! input
     *               ,work(i5),work(i6),work(i7)         ! input
     *               ,xastd,yastd,zi,zni,ier)            ! output

      return
      end
c -----------------------------------------------------------
      subroutine indx77x  (x,y,nmos,nyrs,xmsg,iprnt        ! input
     *                     ,xa,ya,xave,yave,xstd,ystd,wyrs  ! input/output
     *                     ,xastd,yastd,zi,zni,ier)         ! output

c This is the most commonly used routine.

c given two series of year-month values (eg: slp) calculate
c .  an "index" (eg: Southern Oscillation Index)
c .  the way Kevin Trenberth suggests doing it.
c .  This means using the overall anomaly standard deviation
c .  to normalize the anomalies.

c index is computed as zi = x/sigma(x) - y/sigma(y) (normalized stuff)

c Trenberth (1984), "Signal versus Noise in the Southern Oscillation"
c .  Monthly Weather Review 112:326-332

c code written for clarity

      integer nmos           ! number of "months" (eg: 12, 4, 1)
     *      , nyrs           ! number of years of data
     *      , iprnt          ! print flag (=0 means do not print)
     *      , ier            ! error code  

      real    x(nmos,nyrs)   ! monthly data from station/grid pt 1
     *   ,    y(nmos,nyrs)   ! monthly data from station/grid pt 2
     *   ,    xmsg           ! missing code (if any)
     *   ,    zi(nmos,nyrs)  ! index
     *   ,    zni(nmos,nyrs) ! noise index

                             ! upon exit the following will contain
      real    xa(nmos,nyrs)  ! monthly anomalies from average
     *   ,    ya(nmos,nyrs)     
     *   ,    xave(nmos)     ! monthly averages
     *   ,    yave(nmos)   
     *   ,    xstd(nmos)     ! monthly st dev (interannual variability)
     *   ,    ystd(nmos) 
     *   ,    wyrs(nyrs)     ! work array 
     *   ,    xastd          ! overall st dev of xa anomalies 
     *   ,    yastd          ! overall st dev of ya anomalies 

      ier = 0
      if (nmos.le.0 .or. nyrs.le.0)  ier = ier + 1
      if (ier.ne.0) return

c initilize index to msg values

      do nyr=1,nyrs
        do nmo=1,nmos
           zi (nmo,nyr) = xmsg
           zni(nmo,nyr) = xmsg
        enddo
      enddo

c calculate each months long-term mean and standard deviation
c .   (long-term <==> climatological)

      do nmo=1,nmos  
        do nyr=1,nyrs
           wyrs(nyr) = x(nmo,nyr)
        enddo
         call stat2 (wyrs,nyrs,xmsg,xave(nmo),xvar,xstd(nmo)
     *              ,kntx,ier)
           
        do nyr=1,nyrs
           wyrs(nyr) = y(nmo,nyr)
        enddo
         call stat2 (wyrs,nyrs,xmsg,yave(nmo),yvar,ystd(nmo)
     *              ,knty,ier)
      enddo

c calculate anomalies from climatogical mean

      do nyr=1,nyrs
        do nmo=1,nmos
           if (x(nmo,nyr).ne.xmsg .and. xave(nmo).ne.xmsg) then
               xa(nmo,nyr) = x(nmo,nyr)-xave(nmo)
           else
               xa(nmo,nyr) = xmsg
           endif
           if (y(nmo,nyr).ne.xmsg .and. yave(nmo).ne.xmsg) then
               ya(nmo,nyr) = y(nmo,nyr)-yave(nmo)
           else
               ya(nmo,nyr) = xmsg
           endif
        enddo
      enddo

c calculate the overall standard deviation of the anomalies
c .   (ie: use anomalies from all year-months together)

      call stat2 (xa,nmos*nyrs,xmsg,xabar,xavar,xastd,kntxa,ier)
      call stat2 (ya,nmos*nyrs,xmsg,yabar,yavar,yastd,kntya,ier)

c calculate each index 

      if (xastd.ne.xmsg .and. xastd.gt.0.0 .and.
     *    yastd.ne.xmsg .and. yastd.gt.0.0 ) then  
          do nyr=1,nyrs
            do nmo=1,nmos
               if (xa(nmo,nyr).ne.xmsg .and. ya(nmo,nyr).ne.xmsg) then
                   zi (nmo,nyr) = xa(nmo,nyr)/xastd - ya(nmo,nyr)/yastd 
                   zni(nmo,nyr) = xa(nmo,nyr)/xastd + ya(nmo,nyr)/yastd 
               endif
            enddo
          enddo
      else
          ier = ier + 10
      endif  

      if (iprnt.ne.0) then
         do nyr=1,nyrs
            write (*,"(' SINDEX: ',i4,12(1x,f7.1))") 
     *                   nyr, (zi(nmo,nyr),nmo=1,nmos)
         enddo
         if (iprnt.lt.0) then
            do nyr=1,nyrs
               write (*,"(' NINDEX: ',i4,12(1x,f7.1))") 
     *              nyr, (zni(nmo,nyr),nmo=1,nmos)
            enddo
         endif
      endif

      return
      end
