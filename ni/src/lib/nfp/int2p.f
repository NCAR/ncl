      subroutine int2p(pin,xin,p,x,npin,pout,xout,npout,linlog,xmsg,ier)
      implicit none

c routine to interpolate from one set of pressure levels
c .   to another set  using linear or ln(p) interpolation
c
c NCL: xout = int2p (pin,xin,pout,linlog)
c
c nomenclature:
c
c .   pin    - input pressure levels. The pin should
c .            be in decending order (e.g., 1000,900,825,..)
c .            pin(1)>pin(2)>...>pin(npin)
c .   xin    - data at input pressure levels
c .   npin   - number of input pressure levels
c .   pout   - output pressure levels (input by user)
c .            decending order required.
c .   xout   - data at output pressure levels
c .   npout  - number of output pressure levels
c .   linlog - if linlog=1 use linear interp in pressure
c .            if linlog anything but =1 linear interp in ln(pressure)
c .   xmsg   - missing data code. if none, set to some number
c .            which will not be encountered (e.g., 1.e+36)
c .   ier    - error code

      integer npin, npout, linlog, ier           ! input types
      real pin(npin), xin(npin), pout(npout), xmsg  
      real xout(npout)                           ! output

      integer j1, np, nl, nin, nlmax
      real    slope, pa, pb, pc 
      real    p(npin), x(npin)           

c error check

      ier = 0
      if (npin.lt.1 .or. npout.lt.1) ier = ier + 1
      if (ier .ne. 0) then
         if (npout .gt. 0) then
            do np = 1, npout
               xout(np) = xmsg
            end do
         endif
         return 
      endif
c
c eliminate levels with missing data. This can easily
c .   happen with observational data.
c
      nl = 0
      do nin = 1, npin
         if (xin(nin).ne.xmsg .and. pin(nin).ne.xmsg) then
            nl = nl + 1
            p(nl) = pin(nin)
            x(nl) = xin(nin)
         endif
      end do
      nlmax = nl
      if (nlmax .eq. 0) then
         ier = ier + 1000                        ! all missing data
         return 
      endif
c
c perform the interpolation
c                                                      ( p ,x)
c ------------------------- p(nl+1), x(nl+1)   example (200,5)
c .
c ------------------------- pout(np), xout(np)         (250,?)
c .
c ------------------------- p(nl)  , x(nl)             (300,10)
c
      do np = 1, npout
         xout(np) = xmsg
         do nl = 1, nlmax
            if (pout(np) .eq. p(nl)) then
               xout(np) = x(nl)
            else if (nl .lt. nlmax) then
               if (pout(np).lt.p(nl) .and. pout(np).gt.p(nl+1)) then
                  if (linlog .eq. 1) then
                     slope = (x(nl)-x(nl+1))/(p(nl)-p(nl+1))
                     xout(np) = x(nl+1) + slope*(pout(np)-p(nl+1))
                  else
                     pa = alog(p(nl))
                     pb = alog(pout(np))
                     pc = alog(p(nl+1))
                     slope = (x(nl)-x(nl+1))/(pa - pc)
                     xout(np) = x(nl+1) + slope*(pb - pc)
                  endif
               endif
            endif
         end do
      end do
c
      return 
      end 
 
