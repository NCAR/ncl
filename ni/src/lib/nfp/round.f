      subroutine rndncl(npts, xin, ismsg, xmsg, xout)
      implicit none
      integer  npts, ismsg
      double precision xin(npts), xout(npts), xmsg

      integer  n

      do n=1,npts
         if (.not.ismsg.or.(ismsg.and.xin(n).ne.xmsg)) then 
             xout(n) = anint(xin(n))
         else
             xout(n) = xmsg 
         end if
      end do

      return
      end 
