C
C $Id:$
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding double precision routine.
C
      subroutine curvpp (n,x,y,p,d,isw,s,eps,ys,ysp,sigma,
     *                   td,tsd1,hd,hsd1,hsd2,rd,rsd1,rsd2,
     *                   rnm1,rn,v,ierr)
C
      integer n,isw,ierr
      real x(n),y(n),p,d(n),s,eps,ys(n),ysp(n),sigma,td(n),
     *     tsd1(n),hd(n),hsd1(n),hsd2(n),rd(n),rsd1(n),
     *     rsd2(n),rnm1(n),rn(n),v(n)
c
c                                 coded by alan kaylor cline
c                              from fitpack -- april 8, 1991
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a periodic smoothing spline under tension. for a
c given increasing sequence of abscissae (x(i)), i = 1,...,n
c and associated ordinates (y(i)), i = 1,...,n, letting p be
c the period, x(n+1) = x(1)+p, and y(n+1) = y(1), the
c function determined minimizes the summation from i = 1 to
c n of the square of the second derivative of f plus sigma
c squared times the difference of the first derivative of f
c and (f(x(i+1))-f(x(i)))/(x(i+1)-x(i)) squared, over all
c functions f with period p and two continuous derivatives
c such that the summation of the square of
c (f(x(i))-y(i))/d(i) is less than or equal to a given
c constant s, where (d(i)), i = 1,...,n are a given set of
c observation weights. the function determined is a periodic
c spline under tension with third derivative discontinuities
c at (x(i)) i = 1,...,n (and all periodic translations of
c these values). for actual computation of points on the
c curve it is necessary to call the function curvp2.
c
c on input--
c
c   n is the number of values to be smoothed (n.ge.2).
c
c   x is an array of the n increasing abscissae of the
c   values to be smoothed.
c
c   y is an array of the n ordinates of the values to be
c   smoothed, (i. e. y(k) is the functional value
c   corresponding to x(k) ).
c
c   p is the period (p .gt. x(n)-x(1)).
c
c   d is a parameter containing the observation weights.
c   this may either be an array of length n or a scalar
c   (interpreted as a constant). the value of d
c   corresponding to the observation (x(k),y(k)) should
c   be an approximation to the standard deviation of error.
c
c   isw contains a switch indicating whether the parameter
c   d is to be considered a vector or a scalar,
c          = 0 if d is an array of length n,
c          = 1 if d is a scalar.
c
c   s contains the value controlling the smoothing. this
c   must be non-negative. for s equal to zero, the
c   subroutine does interpolation, larger values lead to
c   smoother funtions. if parameter d contains standard
c   deviation estimates, a reasonable value for s is
c   float(n).
c
c   eps contains a tolerance on the relative precision to
c   which s is to be interpreted. this must be greater than
c   or equal to zero and less than equal or equal to one. a
c   reasonable value for eps is sqrt(2./float(n)).
c
c   ys is an array of length at least n.
c
c   ysp is an array of length at least n.
c
c   sigma contains the tension factor. this value indicates
c   the degree to which the first derivative part of the
c   smoothing functional is emphasized. if sigma is nearly
c   zero (e. g. .001) the resulting curve is approximately a
c   cubic spline. if sigma is large (e. g. 50.) the
c   resulting curve is nearly a polygonal line. if sigma
c   equals zero a cubic spline results. a standard value for
c   sigma is approximately 1.
c
c and
c
c   td, tsd1, hd, hsd1, hsd2, rd, rsd1, rsd2, rnm1, rn, and
c   v are arrays of length at least n which are used for
c   scratch storage.
c
c on output--
c
c   ys contains the smoothed ordinate values.
c
c   ysp contains the values of the second derivative of the
c   smoothed curve at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if s is negative,
c        = 3 if eps is negative or greater than one,
c        = 4 if x-values are not strictly increasing,
c        = 5 if a d-value is non-positive,
c        = 6 if p is less than or equal to x(n)-x(1).
c
c and
c
c   n, x, y, d, isw, s, eps, and sigma are unaltered.
c
c this subroutine references package modules ftstore, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      if (n .lt. 2) go to 25
      if (s .lt. 0.) go to 26
      if (eps .lt. 0. .or. eps .gt. 1.) go to 27
      if (p .le. x(n)-x(1)) go to 30
      ierr = 0
      q = 0.
      rsd1(1) = 0.
      rsd2(1) = 0.
      rsd2(2) = 0.
      rsd1(n-1) = 0.
      rsd2(n-1) = 0.
      rsd2(n) = 0.
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/p
c
c form t matrix and second differences of y into ys
c
      nm1 = n-1
      nm2 = n-2
      nm3 = n-3
      delxi1 = x(1)+p-x(n)
      delyi1 = (y(1)-y(n))/delxi1
      call terms (dim1,tsd1(1),sigmap,delxi1)
      hsd1(1) = 1./delxi1
      do 1 i = 1,n
        ip1 = i+1
        if (i .eq. n) ip1 = 1
        delxi = x(ip1)-x(i)
        if (i .eq. n) delxi = x(1)+p-x(n)
        if (delxi .le. 0.) go to 28
        delyi = (y(ip1)-y(i))/delxi
        ys(i) = delyi-delyi1
        call terms (di,tsd1(ip1),sigmap,delxi)
        td(i) = di+dim1
        hd(i) = -(1./delxi+1./delxi1)
        hsd1(ip1) = 1./delxi
        delxi1 = delxi
        delyi1 = delyi
    1   dim1 = di
      hsd11 = hsd1(1)
      if (n .ge. 3) go to 2
      tsd1(2) = tsd1(1)+tsd1(2)
      tsd1(1) = 0.
      hsd1(2) = hsd1(1)+hsd1(2)
      hsd1(1) = 0.
c
c calculate lower and upper tolerances
c
    2 sl = s*(1.-eps)
      su = s*(1.+eps)
      if (d(1) .le. 0.) go to 29
      if (isw .eq. 1) go to 5
c
c form h matrix - d array
c
      betapp = hsd1(n)*d(n)*d(n)
      betap = hsd1(1)*d(1)*d(1)
      alphap = hd(n)*d(n)*d(n)
      im1 = n
      sumd = 0.
      sumy = 0.
      do 3 i = 1,n
        disq = d(i)*d(i)
        sumd = sumd+1./disq
        sumy = sumy+y(i)/disq
        ip1 = i+1
        if (i .eq. n) ip1 = 1
        alpha = hd(i)*disq
        if (d(ip1) .le. 0.) go to 29
        hsd1ip = hsd1(ip1)
        if (i .eq. n) hsd1ip = hsd11
        beta = hsd1ip*d(ip1)*d(ip1)
        hd(i) = (hsd1(i)*d(im1))**2+alpha*hd(i)
     *                             +beta*hsd1ip
        hsd2(i) = hsd1(i)*betapp
        hsd1(i) = hsd1(i)*(alpha+alphap)
        im1 = i
        alphap = alpha
        betapp = betap
    3   betap = beta
      if (n .eq. 3) hsd1(3) = hsd1(3)+hsd2(2)
c
c test for straight line fit
c
      con = sumy/sumd
      sum = 0.
      do 4 i = 1,n
    4   sum = sum+((y(i)-con)/d(i))**2
      if (sum .le. su) go to 23
      go to 8
c
c form h matrix - d constant
c
    5 sl = d(1)*d(1)*sl
      su = d(1)*d(1)*su
      hsd1p = hsd1(n)
      hdim1 = hd(n)
      sumy = 0.
      do 6 i = 1,n
        sumy = sumy+y(i)
        hsd1ip = hsd11
        if (i .lt. n) hsd1ip = hsd1(i+1)
        hdi = hd(i)
        hd(i) = hsd1(i)*hsd1(i)+hdi*hdi+hsd1ip*hsd1ip
        hsd2(i) = hsd1(i)*hsd1p
        hsd1p = hsd1(i)
        hsd1(i) = hsd1p*(hdi+hdim1)
    6   hdim1 = hdi
      if (n .eq. 3) hsd1(3) = hsd1(3)+hsd2(2)
c
c test for straight line fit
c
      con = sumy/float(n)
      sum = 0.
      do 7 i = 1,n
    7   sum = sum+(y(i)-con)**2
      if (sum .le. su) go to 23
c
c top of iteration
c cholesky factorization of q*t+h into r
c
c
c i = 1
c
    8 rd(1) = 1./(q*td(1)+hd(1))
      rnm1(1) = hsd2(1)
      yspnm1 = ys(nm1)
      rn(1) = q*tsd1(1)+hsd1(1)
      yspn = ys(n)
      ysp(1) = ys(1)
      rsd1i = q*tsd1(2)+hsd1(2)
      rsd1(2) = rsd1i*rd(1)
      sumnm1 = 0.
      sum2 = 0.
      sumn = 0.
      if (n .eq. 3) go to 11
      if (n .eq. 2) go to 12
c
c i = 2
c
      rd(2) = 1./(q*td(2)+hd(2)-rsd1i*rsd1(2))
      rnm1(2) = -rnm1(1)*rsd1(2)
      rn(2) = hsd2(2)-rn(1)*rsd1(2)
      ysp(2) = ys(2)-rsd1(2)*ysp(1)
      if (n .eq. 4) go to 10
      do 9 i = 3,nm2
        rsd2i = hsd2(i)
        rsd1i = q*tsd1(i)+hsd1(i)-rsd2i*rsd1(i-1)
        rsd2(i) = rsd2i*rd(i-2)
        rsd1(i) = rsd1i*rd(i-1)
        rd(i) = 1./(q*td(i)+hd(i)-rsd1i*rsd1(i)
     *                           -rsd2i*rsd2(i))
        rnm1(i) = -rnm1(i-2)*rsd2(i)-rnm1(i-1)*rsd1(i)
        rnm1t = rnm1(i-2)*rd(i-2)
        sumnm1 = sumnm1+rnm1t*rnm1(i-2)
        rnm1(i-2) = rnm1t
        sum2 = sum2+rnm1t*rn(i-2)
        yspnm1 = yspnm1-rnm1t*ysp(i-2)
        rn(i) = -rn(i-2)*rsd2(i)-rn(i-1)*rsd1(i)
        rnt = rn(i-2)*rd(i-2)
        sumn = sumn+rnt*rn(i-2)
        rn(i-2) = rnt
        yspn = yspn-rnt*ysp(i-2)
    9   ysp(i) = ys(i)-rsd1(i)*ysp(i-1)-rsd2(i)*ysp(i-2)
c
c i = n-3
c
   10 rnm1(nm3) = hsd2(nm1)+rnm1(nm3)
      rnm1(nm2) = rnm1(nm2)-hsd2(nm1)*rsd1(nm2)
      rnm1t = rnm1(nm3)*rd(nm3)
      sumnm1 = sumnm1+rnm1t*rnm1(nm3)
      rnm1(nm3) = rnm1t
      sum2 = sum2+rnm1t*rn(nm3)
      yspnm1 = yspnm1-rnm1t*ysp(nm3)
      rnt = rn(nm3)*rd(nm3)
      sumn = sumn+rnt*rn(nm3)
      rn(nm3) = rnt
      yspn = yspn-rnt*ysp(nm3)
c
c i = n-2
c
   11 rnm1(nm2) = q*tsd1(nm1)+hsd1(nm1)+rnm1(nm2)
      rnm1t = rnm1(nm2)*rd(nm2)
      sumnm1 = sumnm1+rnm1t*rnm1(nm2)
      rnm1(nm2) = rnm1t
      rn(nm2) = hsd2(n)+rn(nm2)
      sum2 = sum2+rnm1t*rn(nm2)
      yspnm1 = yspnm1-rnm1t*ysp(nm2)
      rnt = rn(nm2)*rd(nm2)
      sumn = sumn+rnt*rn(nm2)
      rn(nm2) = rnt
      yspn = yspn-rnt*ysp(nm2)
c
c i = n-1
c
   12 rd(nm1) = 1./(q*td(nm1)+hd(nm1)-sumnm1)
      ysp(nm1) = yspnm1
      rn(nm1) = q*tsd1(n)+hsd1(n)-sum2
      rnt = rn(nm1)*rd(nm1)
      sumn = sumn+rnt*rn(nm1)
      rn(nm1) = rnt
      yspn = yspn-rnt*ysp(nm1)
c
c i = n
c
      rdn = q*td(n)+hd(n)-sumn
      rd(n) = 0.
c
c Modified test based on communication with Alan Cline, 21-May-2002.
c Problems with the original test (rdn .gt. 0.) surfaced in a demo
c example run on a Linux system.
c
      if (rdn .gt. 0. .and. q .ne. 0.) rd(n) = 1./rdn
      ysp(n) = yspn
c
c back solve of r(transpose)* r * ysp = ys
c
      ysp(n) = rd(n)*ysp(n)
      ysp(nm1) = rd(nm1)*ysp(nm1)-rn(nm1)*ysp(n)
      if (n .eq. 2) go to 14
      yspn = ysp(n)
      yspnm1 = ysp(nm1)
      do 13 ibak = 1,nm2
        i = nm1-ibak
   13   ysp(i) = rd(i)*ysp(i)-rsd1(i+1)*ysp(i+1)
     *                  -rsd2(i+2)*ysp(i+2)-rnm1(i)*yspnm1
     *                  -rn(i)*yspn
   14 sum = 0.
      delyi1 = (ysp(1)-ysp(n))/(x(1)+p-x(n))
      if (isw .eq. 1) go to 16
c
c calculation of residual norm
c  - d array
c
      do 15 i = 1,nm1
        delyi = (ysp(i+1)-ysp(i))/(x(i+1)-x(i))
        v(i) = (delyi-delyi1)*d(i)*d(i)
        sum = sum+v(i)*(delyi-delyi1)
   15   delyi1 = delyi
      delyi = (ysp(1)-ysp(n))/(x(1)+p-x(n))
      v(n) = (delyi-delyi1)*d(n)*d(n)
      go to 18
c
c calculation of residual norm
c  - d constant
c
   16 do 17 i = 1,nm1
        delyi = (ysp(i+1)-ysp(i))/(x(i+1)-x(i))
        v(i) = delyi-delyi1
        sum = sum+v(i)*(delyi-delyi1)
   17   delyi1 = delyi
      delyi = (ysp(1)-ysp(n))/(x(1)+p-x(n))
      v(n) = delyi-delyi1
   18   sum = sum+v(n)*(delyi-delyi1)
c
c test for convergence
c
      if (sum .le. su) go to 21
c
c calculation of newton correction
c
      f = 0.
      g = 0.
      rnm1sm = 0.
      rnsm = 0.
      im1 = n
      if (n .eq. 2) go to 20
      wim2 = 0.
      wim1 = 0.
      do 19 i = 1,nm2
        tui = tsd1(i)*ysp(im1)+td(i)*ysp(i)
     *                        +tsd1(i+1)*ysp(i+1)
        wi = tui-rsd1(i)*wim1-rsd2(i)*wim2
        rnm1sm = rnm1sm-rnm1(i)*wi
        rnsm = rnsm-rn(i)*wi
        f = f+tui*ysp(i)
        g = g+wi*wi*rd(i)
        im1 = i
        wim2 = wim1
   19   wim1 = wi
   20 tui = tsd1(nm1)*ysp(im1)+td(nm1)*ysp(nm1)
     *      +tsd1(n)*ysp(n)
      wi = tui+rnm1sm
      f = f+tui*ysp(nm1)
      g = g+wi*wi*rd(nm1)
      tui = tsd1(n)*ysp(nm1)+td(n)*ysp(n)
     *      +tsd1(1)*ysp(1)
      wi = tui+rnsm-rn(nm1)*wi
      f = f+tui*ysp(n)
      g = g+wi*wi*rd(n)
      h = f-q*g
      if (h .le. 0.) go to 21
c
c update q - newton step
c
      step = (sum-sqrt(sum*sl))/h
      if (sl .ne. 0.) step = step*sqrt(sum/sl)
      if (ftstore(q+step) .eq. q) go to 21
      q = q+step
      go to 8
c
c store smoothed y-values and second derivatives
c
   21 do 22 i = 1,n
        ys(i) = y(i)-v(i)
   22   ysp(i) = q*ysp(i)
      return
c
c store constant ys and zero ysp
c
   23 do 24 i = 1,n
        ys(i) = con
   24   ysp(i) = 0.
      return
c
c n less than 2
c
   25 ierr = 1
      return
c
c s negative
c
   26 ierr = 2
      return
c
c eps negative or greater than 1
c
   27 ierr = 3
      return
c
c x-values not strictly increasing
c
   28 ierr = 4
      return
c
c weight non-positive
c
   29 ierr = 5
      return
c
c incorrect period
c
   30 ierr = 6
      return
      end
