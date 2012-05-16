C NCLFORTSTART
      subroutine dimavgwgt(nx, x, xmsg, wgt, iopt, xavg)
      implicit none
C                                 INPUT
      integer  nx, iopt
      double precision x(nx), wgt(nx), xmsg
C                                 OUTPUT
      double precision xavg
C NCLEND
      integer n, npts
      double precision xsum, wsum 

C NCL:  xWgtAvg = dim_avg_wgt(x, wgt, opt)

      xavg = xmsg
      xsum = 0.d0
      wsum = 0.d0
      npts = 0

      do n=1,nx
         if (x(n).ne.xmsg) then
             xsum = xsum + x(n)*wgt(n)
             wsum = wsum + wgt(n)
             npts = npts + 1
         end if
      end do

      if (wsum.gt.0.d0) then
          if ((iopt.eq.0 .and. npts.eq.nx) .or. iopt.eq.1 ) then
              xavg = xsum/wsum
              return
          end if

          if (iopt.gt.1 .and. npts.ge.iopt) then
              xavg = xsum/wsum
              return
          end if
      end if

      return
      end
