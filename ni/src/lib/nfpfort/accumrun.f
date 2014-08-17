C NCLORTSTART
      subroutine dacumrun(p,np,pmsg,nrun,pac,nmsg,iopt)
      implicit none
c                               INPUT
      integer np, nrun, iopt
      double precision p(np), pmsg
c                               OUTPUT
      integer nmsg
      double precision pac(np)
C NCLEND
c                               LOCAL
      integer n, m

c 1st (nrun-1) are msg

      do n=1,nrun-1
         pac(n) = pmsg
      end do

c Sum nrun 'p' values and store them in appropriate 'pac' location
c .   If any run encounters a msg value:
c .   iopt=0: (a) count; set 'pac=pmsg'; (c) terminate current run
c .   iopt=1: (a) count total # missing values; (b) continue w run

      nmsg = 0

      do n=nrun,np
         pac(n) = 0.0d0
        do m=0,nrun-1
           if (p(n-m).ne.pmsg) then
               pac(n) = pac(n) + p(n-m)
           else
               nmsg   = nmsg + 1
               if (iopt.eq.0) then
                   pac(n) = pmsg
                   go to 10
               end if
           end if
        end do
   10    continue
      end do

      return
      end
