C NCLFORTSTART
      subroutine dimsumwgt(nx, x, xmsg, wgt, iopt, sumx)
      implicit none
C                                 INPUT
      integer  nx, iopt
      double precision x(nx), wgt(nx), xmsg
C                                 OUTPUT
      double precision sumx
C NCLEND
      integer n, npts
      double precision xsum 

C NCL:  xWgtSum = dim_sum_wgt(x, wgt, opt)

      sumx = xmsg
      xsum = 0.d0
      npts = 0

      do n=1,nx
         if (x(n).ne.xmsg) then
             xsum = xsum + x(n)*wgt(n)
             npts = npts + 1
         end if
      end do

      if (npts.gt.0) then
          if ((iopt.eq.0 .and. npts.eq.nx) .or. iopt.eq.1) then
              sumx = xsum
              return
          end if

          if (iopt.gt.1 .and. npts.ge.iopt) then
              sumx = xsum
              return
          end if
      end if

      return
      end
