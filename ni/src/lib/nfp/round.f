      subroutine rndncl(npts, xin, ismsg, xmsg, xout)
      implicit none
      integer  npts, ismsg
      double precision xin(npts), xout(npts), xmsg

      integer  n

      do n=1,npts
         if (ismsg.eq.0.or.(ismsg.eq.1.and.xin(n).ne.xmsg)) then 
             xout(n) = anint(xin(n))
         else
             xout(n) = xmsg 
         end if
      end do

      return
      end 
