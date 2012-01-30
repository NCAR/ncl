C NCLFORTSTART
      subroutine gamfitd3 (datarr, n, dmsg, pcrit, invscale
     +                    ,alpha , scale, shape, pzero,ier)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c based upon: www.unicam.it/matinf/pasef/docs/CDIIIsem/home/pidieffe/spi.pdf
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Estimate incomplete gamma parameters.
c
c Input:
c	datarr - data array
c	n      - size of datarr
c	dmsg   - missing data
c	pcrit  - scalar (int,float,double) specifying % non missing data
c	invscale -  if invscale=1, then return (1/beta)
c
c Output:
c	alpha (???), beta (scale), eta (shape) - gamma parameters
c	pzero - probability of zero.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
c                                                        INPUT
      integer n, invscale,  ier
      double precision datarr(n), dmsg, pcrit
c                                                        OUTPUT
      double precision shape, scale, alpha, pzero
c NCLEND
c                                                        LOCAL 
      integer i, nact, nmsg
      double precision    sum, sumlog, av, pobs, pc
      double precision    beta, eta
c     double precision    s, kappa

      shape = dmsg
      scale = dmsg
      alpha = dmsg

      beta  = scale
      eta   = shape
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
c                             pobs id the total # of non-missing obs
      pobs = nact + pzero        
      pc   = 100d0*pobs/n
      if (pc.lt.pcrit) then
          ier = 2
c c c     write(*, *) 'too few observations'
          return
      end if

      pzero = pzero/pobs
      if (nact .gt. 0) av = sum / nact

c	Bogus data array but do something reasonable
      if (nact .eq. 1) then
          alpha = 0.0d0
          eta   = 1.0d0
          beta  = av
          return
      end if

c	They were all zeroes.
      if (pzero .eq. 1) then
          alpha = 0.0d0
          eta   = 1.0d0
          beta  = av
          return
      end if

c	Use MLE  (Maximum Liklihood Estimate)
c       A Note on the Gamma Distribution
c       Thom (1958): Monthly Weather Review, pp 117-122.
c       eqn 22 for gamma; just above eqn 21 "A" => alpha


      alpha = log(av)-(sumlog/nact)
      eta   = (1.0d0+sqrt(1.0d0+4.0d0*alpha/3.0d0))/(4.0d0*alpha)
      beta  = av/eta

c ------------
c http://en.wikipedia.org/wiki/Gamma_distribution#Maximum_likelihood_estimation
c alpha is "s" at the above url
c eta (shape)  is approximately "kappa" at the above url
c       The numbers for eta and kappa are very similar

c c c s     = alpha
c c c kappa = (3-s+sqrt((s-3)**2 + 24*s))/(12*s)
c ------------

      if (invscale.eq.1 .and. beta.gt.0.0d0) then
          beta = 1.0d0/beta
      end if

      shape = eta  
      scale = beta

      return
      end
