c ----------------------------------------------------------------------
      subroutine vibeta  (p,x,nlev,xmsg,linlog,psfc,xsfc
     *                   ,pbot,ptop,plvcrt,vint,ier)
      implicit none

c NCL: vint = vibeta (p,x,linlog,psfc,pbot,ptop)
c NCL:        plvcrt = p(nlev)  do internally
c NCL:        xsfc   = x(1)     do internally

c vertical integral using ket/as:
c .   "diagnostic studies using global analyses"

c .   the approach here is somewhat crude 'n lude.
c .   i did it to preserve a one-to-one correspondence
c .   between subscripts in the above paper and the
c .   program below.

c .   also, this is *far* more complicated then is
c .   generally needed. It was wriiten to handle
c .   real raob data which have missing values in
c .   the middle of a sounding 

c nomenclature:
c .   p      - pressure levels 
c .   x      - quantity to be integrated
c .   nlev   - number of levels
c .   xmsg   - missing data code
c .   linlog - linear (=1) or log (=2) interpolation
c .   psfc   - surface pressure
c .   xsfc   - value of quantity to be integrated at the surface
c .   pbot   - lower limit of integration
c .   ptop   - upper limit of integration
c .   plvcrt - minimum pressure level which sounding must
c .            reach for integration to proceed (plvcrt.le.ptop)
c .   vint   - result
c .   ier    - error code

      integer nlev, linlog, ier 
      real    p(nlev), x(nlev)
     *   ,    psfc, xsfc, pbot, ptop, plvcrt, xmsg, vint

c local vectors and variables

      integer klvl, nl1, nl, nll, nlmax, nltop, nlsav, nl2, nlx 
      real    zero, pzero, xzero, slope, pa, pc, psfcx 
      logical debug, pflag 

      parameter (klvl=300)
      real       pp(klvl), xx(klvl)
     *   ,       pi(0:klvl), xi(0:klvl)
     *   ,       beta(0:klvl), delp(0:klvl)

      data       zero  /0./
      data       pzero /0./
      data       xzero /0./

c error checking

      ier  = 0
      if (nlev.lt.3)      ier = ier+1
      if (klvl.lt.2*nlev) ier = ier+10         ! make klvl larger
      if (psfc.eq.xmsg)   ier = ier+100     
      if (plvcrt.lt.ptop) ier = ier+1000
      if (ptop.ge.pbot  ) ier = ier+10000

      if (ier.ne.0) then
          vint = xmsg
          return
      endif

c collapse the sounding to eliminate msg data
c .   this is for raobs which have data missing in the
c .   middle of a sounding

      nll   = 0                              ! total number of levels with data
      nlx   = 0                              ! last lvl with data
      pflag = .false.
      call initvc (pp,klvl,xmsg,ier)         ! initilize to msg code
      call initvc (xx,klvl,zero,ier)         ! =0. means not included in integral
      do nl=1,nlev  
         if (p(nl).ne.xmsg .and. x(nl).ne.xmsg .and.
     *       p(nl).lt.psfc ) then      
             nlx     = nl
             nll     = nll + 1
             pp(nll) = p(nl)
             xx(nll) = x(nl)
             if (aint(pp(nll)).eq.aint(ptop) ) pflag = .true.
         endif
      enddo   
      nlmax = nll          ! no. levels with data above sfc 

c make sure there are at least several levels.

      if (nlmax.le.3) then
          ier  = -999
          vint = xmsg
          return
      endif

c explicitly fill the top levels with zeros

      if (nlmax.lt.nlev) then
          do nl=nlx+1,nlev
             nll = nll+1
             pp(nll+1) = p(nl) 
             xx(nll+1) = 0.0        ! contribute nothing to the integral
          enddo
      endif

c explicitly add a ptop  level if neccessary. [remember the daily 
c .   raob data may not have all levels.] 
c .   Generally, this if block is not executed.

      if (pp(nlmax).gt.ptop) then
          nlmax = nlmax+1         ! add level at ptop 
          pp(nlmax) = ptop        ! assume p=0, x=0 is next level above.
          if (linlog.eq.1) then   
              slope     = (xx(nlmax-1)-xzero)/(pp(nlmax-1)-pzero)    
              xx(nlmax) = xzero + (pp(nlmax)-pzero)*slope
          elseif (linlog.eq.2) then
              slope     = xx(nlmax-1)/alog(pp(nlmax-1))
              xx(nlmax) = xzero + alog(pp(nlmax))*slope
          endif

      elseif (.not. pflag) then
          call copyvc (pp(1),beta(1),nlmax,ier)  ! beta=temporary storage
          call copyvc (xx(1),delp(1),nlmax,ier)  ! delp=temporary storage
          do nl=1,nlmax-1                        ! determine where
             if (aint(pp(nl  )).gt.ptop .and.    ! ptop should be inserted
     *           aint(pp(nl+1)).lt.ptop) then
                 nltop = nl+1                    ! subscript location for ptop
                 nlsav = nltop
                 go to 25
             endif
          enddo 
          ier = 1000000
          write (*,'(/,'' sub vibeta: problem: ier='',i9)')
          return

   25     nl1 = nltop
          nl2 = nltop-1
          if (linlog.eq.1) then
              slope = (xx(nl2)-xx(nl1))/(pp(nl2)-pp(nl1))
              xx(nltop) = xx(nl1) + (ptop-pp(nl1))*slope  
          elseif (linlog.eq.2) then
              pa    = alog(pp(nl2))
              pc    = alog(pp(nl1))
              slope = (xx(nl2)-xx(nl1))/(pa-pc) 
              xx(nltop) = xx(nl1) + (alog(ptop)-pc)*slope  
          endif

          pp(nltop) = ptop         

          do nl=nlsav,nlmax     ! push all levels up one subscript
             xx(nl+1)  = delp(nl)
             pp(nl+1)  = beta(nl)
          enddo
          nlmax     = nlmax+1
      endif

c the pp and xx vectors now hold "good" data. 
c .   pi and xi will be the vectors used by the integration routine.
c .   the levels associated with pi and xi correspond to
c .   the levels in fig 1 of trenberth (jc, 1991, p708)

      call initvc (pi(0),(klvl+1),zero,ier)
      call initvc (xi(0),(klvl+1),zero,ier)

      pi(0) = pbot
      do nl=1,nlmax-1
         pi(2*nl-1)    =  pp(nl)                ! odd : levels with data
         pi(2*nl  )    = (pp(nl)+pp(nl+1))*0.5  ! even: intermediate levels
      enddo
      pi(2*nlmax-1) =  pp(nlmax)
      pi(2*nlmax  ) = (pp(nlmax)+zero)*0.5

c determine the subscript of the pi vector which corresponds to ptop
c .   over ride the last two pi levels if (2*nlmax-1).ne.nltop

      nltop = 0
      do nl=1,2*nlmax-1
         if (aint(pi(nl)).eq.aint(ptop)) nltop = nl ! will be odd level
      enddo
      if (nltop.eq.0 .or. mod(nltop,2).eq.0) then
          write (*,'(/,'' sub viket: nltop problem='',i5)') nltop
          stop
      endif
      pi(2*nltop-1) = ptop
      pi(2*nltop  ) = (ptop+zero)*0.5

c interpolate "xi" at intermediate levels using
c .   linear (linlog=1) or log (linlog=2) interpolation.
c .   do not use the sfc pressure [pp(0)].

      if (psfc.ne.xmsg) then
          psfcx = psfc
      else
          psfcx = pp(1)
      endif

c c c if (xsfc.ne.xmsg) then
c c c     xi(0) = xsfc
c c c else
c c c     xi(0) = xx(1)
c c c endif
      call int2p2 (pp(1),xx(1),nlmax
     *           ,pi(1),xi(1),nltop
     *           ,linlog,xmsg,ier) 

c determine beta and delp 
c .   odd levels correspond to actual raob data

      call initvc (delp(0),(klvl+1),zero,ier)
      call initvc (beta(0),(klvl+1),zero,ier)

      do nl=1,nltop
         delp(nl) = pi(nl-1)-pi(nl+1)
      enddo
     
      do nl=1,nltop
         if (pi(nl-1).ge.psfcx .and. pi(nl+1).lt.psfcx) then
             beta(nl) = (psfcx-pi(nl+1))/(pi(nl-1)-pi(nl+1))
         elseif (pi(nl-1).gt.ptop .and. pi(nl+1).lt.ptop) then
             beta(nl) = (pi(nl-1)-ptop)/(pi(nl-1)-pi(nl+1))
         elseif (pi(nl).lt.pbot .and. pi(nl).ge.ptop) then
             beta(nl) = 1.0
         endif
      enddo

c calculate the integral (odd levels only)

      vint = 0.0
      do nl=1,nltop,2
         vint = vint + beta(nl)*xi(nl)*delp(nl)
      enddo

c optional print

      debug = .false.
      if (debug) then
          write (*,'(/,'' sub vibeta: vint='',3i5,2(1x,f10.2),2f8.1,/)')     
     *                    nlmax,2*nlmax-1,nltop, vint, xsfc, psfc, psfcx
          write (*,'(3x,''nl'',8x,''pp'',8x,''xx from viket'')')
c c c     do nl=1,nlmax
c c c     if (pp(nl).ge.ptop) then
c c c         write (*,'(i5,1x,f9.3,1x,f9.6 )') 
c c c*                  nl, pp(nl),xx(nl)
c c c     endif
c c c     enddo
                  
          write (*,'(//,3x,''nl'',8x,''pi'',8x,''xi''
     *                 ,6x,''delp'',6x,''beta from vibeta'')')
          do nl=0,nltop
c c c     if (pi(nl).ge.ptop) then
              write (*,'(i5,1x,f9.3,1x,f9.6,2(1x,f9.3) )') 
     *                  nl, pi(nl),xi(nl), delp(nl),beta(nl)
c c c     endif
          enddo
      endif
      
      return
      end
c ------------------------------------------------------
      subroutine int2p2 (pin,xin,npin,pout,xout,npout
     *                 ,iflag,xmsg,ier)
      implicit none

c routine to interpolate from one set of pressure levels
c .   to another set  using linear or ln(p) interpolation

c CAUTION: THIS ROUTINE HAS NOT BEEN EXTENSIVELY TESTED 

c nomenclature:

c .   pin    - input pressure levels. The pin should
c .            be in decending order (e.g., 1000,900,825,..)
c .            pin(1)>pin(2)>...>pin(npin)
c .   xin    - data at input pressure levels
c .   npin   - number of input pressure levels
c .   pout   - output pressure levels (input by user)
c .            decending order required.
c .   xout   - data at output pressure levels
c .   npout  - number of output pressure levels
c .   iflag  - if iflag=1 user linear interp in pressure
c .            if iflag=2 user linear interp in ln(pressure)
c .   xmsg   - missing data code. if none, set to some number
c .            which will not be encountered (e.g., 1.e+36)
c .   ier    - error code

      integer npin, npout, iflag, ier      ! input types
      real pin (npin),  xin (npin),  xmsg
     *   , pout(npout), xout(npout)

      integer nplvl, np, nl, nin, nlmax 
      parameter (nplvl=200)                ! local vector space
      real    p(nplvl), x(nplvl)
      real    slope, pa, pb, pc 

c error check

      ier = 0
      if (npin.lt.1 .or. npout.lt.1)  ier = ier + 1
      if (iflag.lt.1 .or. iflag.gt.2) ier = ier + 10
      if (npin.gt.nplvl) ier = ier+100     ! increase vector size
      if (ier.ne.0) then
          if (npout.gt.0) then
              do np=1,npout
                 xout(np) = xmsg
              enddo
          endif
          return
      endif

c eliminate levels with missing data. This can easily
c .   happen with observational data.

      nl = 0
      do nin=1,npin
         if (xin(nin).ne.xmsg .and. pin(nin).ne.xmsg) then
             nl = nl+1
             p(nl) = pin(nin)
             x(nl) = xin(nin)
         endif
      enddo
      nlmax = nl
      if (nlmax.eq.0) then
          ier = ier+1000                 ! all missing data
          return
      endif

c perform the interpolation
c                                                      ( p ,x)
c ------------------------- p(nl+1), x(nl+1)   example (200,5)
c .
c ------------------------- pout(np), xout(np)         (250,?)
c .
c ------------------------- p(nl)  , x(nl)             (300,10)

      do np=1,npout
         xout(np) = xmsg                    
       do nl=1,nlmax
          if (pout(np).eq.p(nl)) then
              xout(np) = x(nl)
          elseif (nl.lt.nlmax) then
              if (pout(np).lt.p(nl) .and. pout(np).gt.p(nl+1)) then  
                  if (iflag.eq.1) then
                  slope    = (x(nl)-x(nl+1))/(p(nl)-p(nl+1))
                  xout(np) = x(nl+1) + slope*(pout(np)-p(nl+1))
              else
                  pa = alog( p(nl)    )
                  pb = alog( pout(np) )
                  pc = alog( p(nl+1)  )
                  slope    = (x(nl)-x(nl+1))/(pa-pc)
                  xout(np) = x(nl+1) + slope*(pb-pc)
              endif
          endif
      endif
      enddo 
      enddo
  
      return
      end
c ------------------------------------------------
      subroutine initvc (x,npts,value,ier)  
      implicit none
                                           
c set all elements of a vector to a value 
                                         
c arguments :                                                   
c .   x        - vector to be set to the value                 
c .   npts     - number of points to be set                   
c .   value    - value that each element of x is to be set to
c .   ier      - if (ier.ne.0) an error has occured         
                                                           
      integer npts, ier
      real    x(1:npts), value                                      

      integer n                ! local
                                                         
      ier = 0                                           
      if (npts.lt.1) then                            
         ier = 1                                      
         return                                     
      endif                                        
                                                  
      do n=1,npts                             
         x(n) = value                              
      enddo
                                               
      return                                  
      end                                    
c -----------------------------------------------
      subroutine copyvc (x,y,npts,ier)                                          
      implicit none
                                                                                
c establish a duplicate vector                                                  
                                                                                
c arguments :                                                                   
c .   x        - input vector                                                   
c .   y        - output vector (same as x)                                      
c .   npts     - number of points                                               
c .   ier      - if (ier.ne.0) an error has occured   
                                                                                
      integer npts, ier
      real    x(1:npts) , y(1:npts)

      integer n
                                                                                
      ier = 0                                                                   
      if (npts.lt.1) then                                                       
         ier = 1                                                                
         return                                                                 
      endif                                                                     
                                                                                
      do n=1,npts                                                            
         y(n) = x(n)      
      enddo
                                                                                
      return                                                                    
      end                                                                       
