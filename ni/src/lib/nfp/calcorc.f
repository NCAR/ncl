      subroutine calcorc (x,xave,xstd,xmsg,nxy,y,ymsg,corc,ier)
      implicit none
 
c this routine will calculate the cross correlation coef between 
c .   series x and series y
c this routine was originally meant to be in the inner portion
c .   of a loop
 
c     x         - input data 1D array  ( unchanged on output)
c     xave,xstd - ave and st. dev of x (input)
c     xmsg      - missing data code (if none set to some no. not encountered)
c     nxy       - length of x and y 1D vectors
c     y         - input data 1D array  ( unchanged on output)
c     ymsg      - missing data code (if none set to some no. not encountered)
c     corc      - correlation coef            
c     ier       - error code (if ier=-1 then corc contains missing entry)
c --------------- input arguments
      integer nxy       
      real x(nxy) , y(nxy) ,xave,xstd,xmsg,ymsg
c --------------- output arguments
      integer ier
      real corc
c --------------- local stuff
      integer n, nptusx, nptusy
      real xvar,yave,yvar,ystd,rn,xy,covar

      corc= xmsg
      if (xave.eq.xmsg .or. xstd.eq.xmsg) then
          call stat2 (x,nxy,xmsg,xave,xvar,xstd,nptusx,ier)
          if (ier.ne.0 .or. xstd.eq.0.0) then  
              ier = 201
              return
          end if
      end if
 
c calculate the mean and var of y
 
      call stat2 (y,nxy,ymsg,yave,yvar,ystd,nptusy,ier)
      if (ier.ne.0) return
      if (ystd .eq.0.) then
          ier = 203
          return
      endif
 
c calculate the cross correlation coef
c .   calculate the mean and variance of column nc in the data array
 
      rn = 0.
      xy = 0.
      do n=1,nxy
         if (x(n).ne.xmsg .and. y(n).ne.ymsg) then
             rn = rn + 1.
             xy = xy + (y(n)-yave)*(x(n)-xave)
         endif
      enddo
      if (rn.gt.2.) then
          covar = xy/(rn-1.)
          corc  = covar/(xstd*ystd)
      endif
 
      return
      end




