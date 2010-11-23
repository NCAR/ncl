C NCLFORTSTART
      subroutine patcor1 (mlon,nlat,x,y,xmsg,ymsg,w, r, ier)

c compute the patternanomaly correlation: 1-dimensional weight array 

c nomenclature:
c INPUT  
c .    mlon, nlat  - dimension sizes
c .    x,y         - arrays/grids 
c .    xmsg,ymsg   - missing codes [if none set to some value]
c .    w           - contains weights specified by user [eg: gaussian wgts]
c OUTPUT 
c .    r           - anomaly (pattern) correlation
c .    ier         - error code
c .                  =0  no errors
c .                  =1  x or y contains all msg values
c .                  =2  x or y contains all constant values
c
      implicit none
c                                               !  INPUT
      integer  mlon, nlat, ier
      double precision x(mlon,nlat), y(mlon,nlat), w(nlat), xmsg, ymsg
c                                               !  OUTPUT
      double precision r
C NCLEND
c                                               !  LOCAL
      integer ml, nl
      double precision xave , yave , wsum
      double precision xycov, xanom2, yanom2

      ier    = 0

c wgted area mean

      xave = 0.0d0
      yave = 0.0d0
      wsum = 0.0d0
      do nl=1,nlat
        do ml=1,mlon
           if (x(ml,nl).ne.xmsg .and. y(ml,nl).ne.ymsg) then 
               xave = xave + w(nl)*x(ml,nl)
               yave = yave + w(nl)*y(ml,nl)
               wsum = wsum + w(nl)
           end if
        end do
      end do

c all missing?

      if (wsum.eq.0.0d0) then
          ier = 1
          r   = xmsg
          return
      end if
      
      xave = xave/wsum
      yave = yave/wsum

c covariance and anomalies

      xycov  = 0.0d0
      xanom2 = 0.0d0
      yanom2 = 0.0d0
      
      do nl=1,nlat
        do ml=1,mlon
           if (x(ml,nl).ne.xmsg .and. y(ml,nl).ne.ymsg) then 
               xycov  = xycov  + w(nl)*(x(ml,nl)-xave)*(y(ml,nl)-yave)
               xanom2 = xanom2 + w(nl)*(x(ml,nl)-xave)**2
               yanom2 = yanom2 + w(nl)*(y(ml,nl)-yave)**2
           end if
        end do
      end do

      if (xanom2.gt.0.0d0 .and. yanom2.gt.0.0d0) then
          r   = xycov/(sqrt(xanom2)*sqrt(yanom2))
      else
          ier = 2
          r   = xmsg
      end if

      return
      end
c ======================================================
C NCLFORTSTART
      subroutine patcor2 (mlon,nlat,x,y,xmsg,ymsg,w, r, ier)

c compute the pattern anomaly correlation: 2-dimensional weight array 

c nomenclature:
c INPUT  
c .    mlon, nlat  - dimension sizes
c .    x,y         - arrays/grids 
c .    xmsg,ymsg   - missing codes [if none set to some value]
c .    w           - contains weights specified by user [eg: POP area]
c OUTPUT 
c .    r           - anomaly (pattern) correlation
c .    ier         - error code
c .                  =0  no errors
c .                  =1  x or y contains all msg values
c .                  =2  x or y contains all constant values
c
      implicit none
c                                               !  INPUT
      integer  mlon, nlat, ier
      double precision x(mlon,nlat), y(mlon,nlat), w(mlon,nlat)
     $               , xmsg, ymsg
c                                               !  OUTPUT
      double precision r
C NCLEND
c                                               !  LOCAL
      integer ml, nl
      double precision xave , yave , wsum
      double precision xycov, xanom2, yanom2

      ier    = 0

c wgted area mean

      xave = 0.0d0
      yave = 0.0d0
      wsum = 0.0d0
      do nl=1,nlat
        do ml=1,mlon
           if (x(ml,nl).ne.xmsg .and. y(ml,nl).ne.ymsg) then 
               xave = xave + w(ml,nl)*x(ml,nl)
               yave = yave + w(ml,nl)*y(ml,nl)
               wsum = wsum + w(ml,nl)
           end if
        end do
      end do

c all missing?

      if (wsum.eq.0.0d0) then
          ier = 1
          r   = xmsg
          return
      end if
      
      xave = xave/wsum
      yave = yave/wsum

c covariance and anomalies

      xycov  = 0.0d0
      xanom2 = 0.0d0
      yanom2 = 0.0d0
      
      do nl=1,nlat
        do ml=1,mlon
           if (x(ml,nl).ne.xmsg .and. y(ml,nl).ne.ymsg) then 
               xycov  = xycov  +w(ml,nl)*(x(ml,nl)-xave)*(y(ml,nl)-yave)
               xanom2 = xanom2 +w(ml,nl)*(x(ml,nl)-xave)**2
               yanom2 = yanom2 +w(ml,nl)*(y(ml,nl)-yave)**2
           end if
        end do
      end do

      if (xanom2.gt.0.0d0 .and. yanom2.gt.0.0d0) then
          r   = xycov/(sqrt(xanom2)*sqrt(yanom2))
      else
          ier = 2
          r   = xmsg
      end if

      return
      end
