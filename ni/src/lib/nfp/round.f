      subroutine rndncl(npts, xin, ismsg, xmsg, xout, iopt)
      implicit none
      integer  npts, ismsg, iopt
      double precision xin(npts), xout(npts), xmsg

      integer  n
C
C This is to take care of the case where the user wants the output to
C be returned by integers, and the missing value happens to not be
C an integer, like -9999.9.
C
      if (iopt.eq.3.and.ismsg.eq.1) then
        xmsg = anint(xmsg)
      end if

      do n=1,npts
         if (ismsg.eq.0.or.(ismsg.eq.1.and.xin(n).ne.xmsg)) then 
             xout(n) = anint(xin(n))
         else
             xout(n) = xmsg 
         end if
      end do

      return
      end 
