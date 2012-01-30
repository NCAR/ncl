C NCLFORTSTART
      subroutine spigamd (ntim, pp, pmsg, nrun, index)
      implicit none
      integer ntim, nrun 
      double precision pp(ntim), index(ntim), pmsg
C NCLEND
c compute SPI: Standardized Precipitation Index using gamma distribution

c local/dynamic
      integer maxyrs, j, i, im, n
      double precision tmparr(ntim/12+1)
      double precision alpha(12), beta(12), gamm(12), pzero(12)
      double precision anvnrmd, gamcdfd

      maxyrs = ntim/12
c
c	The first nrun-1 index values will be missing.
c
      do j=1,nrun-1
         index(j) = pmsg
      end do
c
c	Sum nrun precip. values;
c	store them in the appropriate index location.
c
c	If any value is missing; set the sum to missing.
c
      do j=nrun,ntim
         index(j) = 0.0
        do i=0,nrun-1
           if (pp(j-i) .ne. pmsg) then
               index(j) = index(j) + pp(j-i)
           else
               index(j) = pmsg
               goto 30
           endif
       end do
  30   continue
      end do
c
c	For nrun<12, the monthly distributions will be substantially
c       different. So we need to compute gamma parameters for
c	each month starting with the (nrun-1)th.
c
      do i=0,11
        n=0
        do j=nrun+i,ntim,12
           if(index(j) .ne. pmsg) then
              n=n+1
              tmparr(n) = index(j)
           endif
        end do   
        im = mod (nrun+i-1, 12) + 1
c
c	Here's where we do the fitting.
c
        call gamfitd(tmparr,n, alpha(im), beta(im), gamm(im), pzero(im))

      end do
c
c	Replace precip. sums stored in index with SPI's
c
      do j = nrun, ntim
         im = mod (j-1,12) + 1
         if (index(j) .ne. pmsg) then
c
c	Get the probability
c
             index(j) = gamcdfd(beta(im), gamm(im), pzero(im), index(j))
c
c	Convert prob. to z value.
c
             index(j) = anvnrmd(index(j))
         endif
      end do

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc
c	input prob; return z.
c	See Abromowitz and Stegun _Handbook of Mathematical Functions_, p. 933
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc
      double precision function anvnrmd (prob)
      implicit none
      double precision sign, prob, t, c0, c1, c2, d1, d2, d3
      data c0, c1, c2 /2.515517, 0.802853, 0.010328/
      data d1, d2, d3 /1.432788, 0.189269, 0.001308/

      if (prob .gt. 0.5) then
          sign = 1.0
          prob = 1.0 - prob
      else
          sign = -1.0
      endif
      if (prob .lt. 0.0) then
          write(*, *) 'Error in anvnrmd(). Prob. not in [0,1.0]'
          anvnrmd = 0.0
          return
      endif
      if (prob .eq. 0.0) then
          anvnrmd = 1.0e37 * sign
          return
      endif
      t       = sqrt(log (1.0 / (prob * prob)))
      anvnrmd = (sign * (t - ((((c2 * t) + c1) * t) + c0) /
     1          ((((((d3*t)+d2)*t)+d1)*t)+1.0)))
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Estimate incomplete gamma parameters.
c
c Input:
c	datarr - data array
c	n - size of datarr
c
c Output:
c	alpha, beta, gamma - gamma paarameters
c	pzero - probability of zero.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine gamfitd (datarr, n, alpha, beta, gamm, pzero)
      implicit none
      integer n
      double precision datarr(*), alpha, beta, gamm, pzero

      integer nact, i
      double precision sum, sumlog, av 

      if (n .le. 0) then
          write(*, *) 'Error in gamfitd - empty data array'
          stop
      endif
      sum    = 0.0
      sumlog = 0.0
      pzero  = 0.0
      nact   = 0

c	compute sums
      do 10 i = 1, n
      if (datarr(i) .gt. 0.0) then
          sum = sum + datarr(i)
          sumlog = sumlog + log(datarr(i))
          nact = nact + 1
      else
          pzero = pzero + 1
      endif
   10 continue
      pzero = pzero / n
      if (nact .ne. 0.0) av = sum / nact

c	Bogus data array but do something reasonable
      if(nact .eq. 1) then
         alpha = 0.0
         gamm  = 1.0
         beta  = av
         return
      endif

c	They were all zeroes.
      if(pzero .eq. 1.0) then
         alpha = 0.0
         gamm  = 1.0
         beta  = av
         return
      endif

c	Use MLE 
c       A Note on the Gamma Distribution
c       Thom (1958): Monthly Weather Review, pp 117-122.
c       eqn 22 for gamma; just above eqn 21 "A" => alpha

      alpha = log(av) - sumlog / nact
      gamm  = (1.0 + sqrt (1.0 + 4.0 * alpha / 3.0)) / (4.0 * alpha)
      beta  = av / gamm
      
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Compute probability of a<=x using incomplete gamma parameters.
c
c Input:
c	beta, gamma - gamma parameters
c	pzero - probability of zero.
c	x - value.
c
c Return:
c	Probability a<=x.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function gamcdfd (beta, gamm, pzero, x)
      implicit none
      double precision beta, gamm, pzero, x
      double precision gammapd

      if(x .le. 0.0) then
         gamcdfd = pzero
      else
         gamcdfd = pzero + (1.0 - pzero) * gammapd (gamm, x / beta)
      endif

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Compute inverse gamma function i.e. return x given p where CDF(x) = p.
c
c Input:
c	beta, gamma - gamma parameters
c	pzero - probability of zero.
c	prob - probability.
c
c Return:
c	x as above.
c
c Method:
c	We use a simple binary search to first bracket out initial
c	guess and then to refine our guesses until two guesses are within
c	tolerance (eps). Is there a better way to do this?
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function gaminvd (beta, gamm, pzero, prob)
      implicit none
      double precision beta, gamm, pzero, prob

      integer niter
      double precision eps, phigh, thigh, tlow, t, p
      data eps /1.0e-7/
      double precision gamcdfd

c	Check if prob < prob of zero
      if (prob .le. pzero) then
          gaminvd = 0.0
          return
      endif

c	Otherwise adjust prob
      prob = (prob - pzero) / (1.0 - pzero)

c	Make initial guess. Keep doubling estimate until prob is
c	bracketed.
      thigh = 2.0*eps
 10   continue
      phigh = gamcdfd (beta, gamm, pzero, thigh)
      if(phigh .ge. prob) goto 20
      thigh = thigh*2.0
      goto 10
 20   continue
      tlow = thigh / 2.0
c     Iterate to find root.
      niter = 0
 30   continue
      if((thigh - tlow) .le. eps) goto 40
         niter = niter + 1
         t = (tlow + thigh) / 2.0
         p = gamcdfd (beta, gamm, pzero, t)
         if (p .lt. prob) then
             tlow = t
         else
            thigh = t
         endif
      goto 30
 40   continue

      gaminvd = (tlow + thigh) / 2.0
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Functions for the incomplete gamma functions P and Q
c
c	1 /x-ta-1
c P(a,x)=--------| e t dt, a>0
c	Gamma(x)/ 0
c
c Q(a,x)=1-P(a,x)
c
c Reference: Press, Flannery, Teukolsky, and Vetterling,
c	_Numerical Recipes_, pp. 160-163
c
c Thanks to kenny@cs.uiuc.edu
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Evaluate P(a,x) by its series representation.
c
      double precision function gamserd (a, x)
      implicit none
      double precision a, x

      integer maxitr, n, iwarn
      double precision eps, ap, sum, del, gln
      double precision gammlnd

c     Maximum number of iterations, and bound on error.
      parameter (maxitr=100, eps=3.0e-7)
      data iwarn /0/

      gln = gammlnd (a)

      if (x .eq. 0.0) then
          gamserd = 0.0
          return
      endif

      ap  = a
      sum = 1.0 / a
      del = sum
      do 10 n = 1, maxitr
         ap  = ap + 1.0
         del = del * (x / ap)
         sum = sum + del
         if (abs (del) .lt. eps * abs (sum)) goto 20
   10 continue

      iwarn = iwarn + 1
c djs if (iwarn .lt. 20) then
c djs     write (*, *) 'gamserd(',a,x,'): not converging.'
c djs     write (*, *) 'Approximate value of ',sum,' + /-',del,' used.'
c djs endif
 20   continue

      gamserd= sum*exp(-x+a*log(x)-gln)

      return
      end
c
c	Evaluate P(a,x) in its continued fraction representation.
c
      double precision function gammcfd (a, x)
      implicit none
      double precision a, x

      integer maxitr, nwarn, n
      double precision    eps, gln, gold, a0,a1,b0,b1,fac,an,ana,anf,g
      parameter (maxitr=200, eps=3.0d-7)
      data nwarn / 0 /, g / 0.0 /

      double precision gammlnd

      gln  = gammlnd (a)
      gold = 0.0
      a0   = 1.0
      a1   = x
      b0   = 0.0
      b1   = 1.0
      fac  = 1.0

      do 10 n = 1, maxitr
         an  = n
         ana = an - a
         a0  = (a1 + a0 * ana) * fac
         b0  = (b1 + b0 * ana) * fac
         anf = an * fac
         a1  = x * a0 + anf * a1
         b1  = x * b0 + anf * b1
         if (a1 .ne. 0.0) then
             fac = 1.0 / a1
             g = b1 * fac
             if (abs((g - gold) / g) .lt. eps) goto 20
             gold = g
         endif
 10   continue

      nwarn = nwarn + 1
c djs if (nwarn .lt. 20) then
c djs     write (*, *) 'gammcfd(',a,x,'): not converging.'
c djs     write (*, *) 'Inaccurate value of ', g, ' +/- ',
c djs1	  abs(g - gold), ' used.'
c djs endif
 20   continue

      gammcfd= g*exp(-x+a*log(x)-gln)
      return
      end
c
c	Evaluate the incomplete gamma function P(a,x), choosing the most
c	appropriate representation.
c
      double precision function gammapd (a, x)
      implicit none
      double precision a, x
      double precision gamserd, gammcfd

      if (x .lt. a + 1.0) then
          gammapd = gamserd (a, x)
      else
          gammapd = 1.0 - gammcfd (a, x)
      endif

      return
      end
c
c	Evaluate the incomplete gamma function Q(a,x), choosing the most
c	appropriate representation.
c
      double precision function gammaqd (a, x)
      implicit none
      double precision a, x
      double precision gamserd, gammcfd

      if (x .lt. a + 1.0) then
          gammaqd = 1.0 - gamserd (a, x)
      else
          gammaqd = gammcfd (a, x)
      endif
      return
      end
c
c	For those who don't have a ln(gamma) function.
c
      double precision function gammlnd(xx)
      implicit none
      double precision xx
       
      integer j
      double precision cof(6), x, tmp, ser
      data cof /76.18009173, -86.50532033, 24.01409822, -1.231739516,
     1           0.120858003e-2, -0.536382e-5/

      x   = xx - 1.0
      tmp = x + 5.5
      tmp = tmp - (x+0.5) * log(tmp)
      ser = 1.0

      do 10 j = 1, 5
         x   = x + 1.0
         ser = ser + cof(j) / x
 10   continue

      gammlnd = -tmp + log(2.50662827465 * ser)

      return
      end

