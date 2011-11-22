C NCLFORTSTART
      subroutine gamfitd3 (datarr, n, dmsg, pcrit
     +                    ,alpha, beta, gamm, pzero,ier)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Estimate incomplete gamma parameters.
c
c Input:
c	datarr - data array
c	n - size of datarr
c
c Output:
c	alpha, beta, gamma - gamma paarameters
c	pzero - probability of zero.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
c                                                        INPUT
      integer n, ier
      double precision datarr(n), dmsg, pcrit
c                                                        OUTPUT
      double precision alpha, beta, gamm, pzero
c NCLEND
c                                                        LOCAL 
      integer i, nact, nmsg
      double precision    sum, sumlog, av, pobs, pc

      alpha = dmsg
      beta  = dmsg
      gamm  = dmsg
      pzero = dmsg

      ier = 0
      if (n .le. 0) then
          ier   = 1
c c c     write(*, *) 'Error in gamfit - empty data array'
          return
      end if

      sum    = 0.0d0
      sumlog = 0.0d0
      pzero  = 0.0d0
      nact   = 0
      nmsg   = 0

c	compute sums and counts

      do i = 1, n
         if (datarr(i) .ne. dmsg) then
             if (datarr(i) .gt. 0.0d0) then
                 sum    = sum + datarr(i)
                 sumlog = sumlog + log(datarr(i))
                 nact   = nact + 1
             else
                 pzero  = pzero + 1.0d0
             end if
         else
             nmsg = nmsg+1
         end if
      end do

      pobs = nact + pzero        
      pc   = 100*pobs/n
      if (pc.lt.pcrit) then
          ier = 2
c c c     write(*, *) 'too few observations'
          return
      end if

      pzero = 100*pzero/pobs
      if (nact .gt. 0) av = sum / nact

c	Bogus data array but do something reasonable
      if (nact .eq. 1) then
          alpha = 0.0d0
          gamm  = 1.0d0
          beta  = av
          return
      end if

c	They were all zeroes.
      if (pzero .eq. 1) then
          alpha = 0.0d0
          gamm  = 1.0d0
          beta  = av
          return
      end if

c	Use MLE  (Maximum Liklihood Estimate)

      alpha = log(av)-(sumlog/nact)
      gamm  = (1.0d0+sqrt(1.0d0+4.0d0*alpha/3.0d0))/(4.0d0*alpha)
      beta  = av/gamm

      return
      end
