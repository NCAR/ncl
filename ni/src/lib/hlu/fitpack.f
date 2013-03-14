C
C       $Id: fitpack.f,v
C
CThis new version of FITPACK distributed by netlib is about 20% of 
Cthe total package in terms of characters, lines of code, and num-
Cber of subprograms. However, these 25 subprograms represent about
C95% of usages of the package.  What has been omitted are such ca-
Cpabilities as:
C  1. Automatic tension determination,
C  2. Derivatives, arclengths, and enclosed areas for planar 
C     curves,
C  3. Three dimensional curves,
C  4. Special surface fitting using equispacing assumptions,
C  5. Surface fitting in annular, wedge, polar, toroidal, lunar,
C     and spherical geometries,
C  6. B-splines in tension generation and usage,
C  7. General surface fitting in three dimensional space.
C
C(The code previously circulated in netlib is less than 10% of the
Ctotal  package  and is more than a decade old.  Its usage is dis-
Ccouraged.)
C
CPlease note:  Two versions of the subroutine snhcsh are included.
CBoth serve the same purpose:  obtaining approximations to certain
Chyperbolic trigonometric-like functions.  The first is less accu-
Crate (but more efficient) than the second.  Installers should se- 
Clect the one with the precision they desire.
C
CInterested parties can obtain the entire package on disk or  tape
Cfrom Pleasant  Valley Software, 8603 Altus Cove, Austin TX (USA),
C78759 at a cost of $495 US. A 340 page manual  is  available  for
C30 US dollars per  copy.  The  package  includes  examples and
Cmachine readable documentation.
C
      subroutine nhlcurv1 (n,x,y,slp1,slpn,islpsw,yp,temp,
     *                  sigma,ierr)
c
      integer n,islpsw,ierr
      real x(n),y(n),slp1,slpn,yp(n),temp(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute an interpolatory spline under tension through
c a sequence of functional values. the slopes at the two
c ends of the curve may be specified or omitted.  for actual
c computation of points on the curve it is necessary to call
c the function nhlcurv2.
c
c on input--
c
c   n is the number of values to be interpolated (n.ge.2).
c
c   x is an array of the n increasing abscissae of the
c   functional values.
c
c   y is an array of the n ordinates of the values, (i. e.
c   y(k) is the functional value corresponding to x(k) ).
c
c   slp1 and slpn contain the desired values for the first
c   derivative of the curve at x(1) and x(n), respectively.
c   the user may omit values for either or both of these
c   parameters and signal this with islpsw.
c
c   islpsw contains a switch indicating which slope data
c   should be used and which should be estimated by this
c   subroutine,
c          = 0 if slp1 and slpn are to be used,
c          = 1 if slp1 is to be used but not slpn,
c          = 2 if slpn is to be used but not slp1,
c          = 3 if both slp1 and slpn are to be estimated
c              internally.
c
c   yp is an array of length at least n.
c
c   temp is an array of length at least n which is used for
c   scratch storage.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e.g. .001) the resulting curve is approximately a
c   cubic spline. if abs(sigma) is large (e.g. 50.) the
c   resulting curve is nearly a polygonal line. if sigma
c   equals zero a cubic spline results.  a standard value
c   for sigma is approximately 1. in absolute value.
c
c on output--
c
c   yp contains the values of the second derivative of the
c   curve at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if x-values are not strictly increasing.
c
c and
c
c   n, x, y, slp1, slpn, islpsw and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      nm1 = n-1
      np1 = n+1
      ierr = 0
      if (n .le. 1) go to 8
      if (x(n) .le. x(1)) go to 9
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/(x(n)-x(1))
c
c approximate end slopes
c
      if (islpsw .ge. 2) go to 1
      slpp1 = slp1
      go to 2
    1 delx1 = x(2)-x(1)
      delx2 = delx1+delx1
      if (n .gt. 2) delx2 = x(3)-x(1)
      if (delx1 .le. 0. .or. delx2 .le. delx1) go to 9
      call ceez (delx1,delx2,sigmap,c1,c2,c3,n)
      slpp1 = c1*y(1)+c2*y(2)
      if (n .gt. 2) slpp1 = slpp1+c3*y(3)
    2 if (islpsw .eq. 1 .or. islpsw .eq. 3) go to 3
      slppn = slpn
      go to 4
    3 delxn = x(n)-x(nm1)
      delxnm = delxn+delxn
      if (n .gt. 2) delxnm = x(n)-x(n-2)
      if (delxn .le. 0. .or. delxnm .le. delxn) go to 9
      call ceez (-delxn,-delxnm,sigmap,c1,c2,c3,n)
      slppn = c1*y(n)+c2*y(nm1)
      if (n .gt. 2) slppn = slppn+c3*y(n-2)
c
c set up right hand side and tridiagonal system for yp and
c perform forward elimination
c
    4 delx1 = x(2)-x(1)
      if (delx1 .le. 0.) go to 9
      dx1 = (y(2)-y(1))/delx1
      call terms (diag1,sdiag1,sigmap,delx1)
      yp(1) = (dx1-slpp1)/diag1
      temp(1) = sdiag1/diag1
      if (n .eq. 2) go to 6
      do 5 i = 2,nm1
        delx2 = x(i+1)-x(i)
        if (delx2 .le. 0.) go to 9
        dx2 = (y(i+1)-y(i))/delx2
        call terms (diag2,sdiag2,sigmap,delx2)
        diag = diag1+diag2-sdiag1*temp(i-1)
        yp(i) = (dx2-dx1-sdiag1*yp(i-1))/diag
        temp(i) = sdiag2/diag
        dx1 = dx2
        diag1 = diag2
    5   sdiag1 = sdiag2
    6 diag = diag1-sdiag1*temp(nm1)
      yp(n) = (slppn-dx1-sdiag1*yp(nm1))/diag
c
c perform back substitution
c
      do 7 i = 2,n
        ibak = np1-i
    7   yp(ibak) = yp(ibak)-temp(ibak)*yp(ibak+1)
      return
c
c too few points
c
    8 ierr = 1
      return
c
c x-values not strictly increasing
c
    9 ierr = 2
      return
      end

      subroutine nhlcurv2 (t,ret,n,x,y,yp,sigma)
c
      integer n
      real t,x(n),y(n),yp(n),sigma,ret
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function interpolates a curve at a given point
c using a spline under tension. the subroutine nhlcurv1 should
c be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   t contains a real value to be mapped onto the interpo-
c   lating curve.
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   yp is an array of second derivative values of the curve
c   at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, yp, and sigma should be input
c unaltered from the output of nhlcurv1.
c
c on output--
c
c   nhlcurv2 contains the interpolated value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      im1 = intrvl(t,x,n)
      i = im1+1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/(x(n)-x(1))
c
c set up and perform interpolation
c
      del1 = t-x(im1)
      del2 = x(i)-t
      dels = x(i)-x(im1)
      sum = (y(i)*del1+y(im1)*del2)/dels
      if (sigmap .ne. 0.) go to 1
      ret = sum-del1*del2*(yp(i)*(del1+dels)+yp(im1)*
     *        (del2+dels))/(6.*dels)
      return
    1 sigdel = sigmap*dels
      call snhcsh (ss,dummy,sigdel,-1)
      call snhcsh (s1,dummy,sigmap*del1,-1)
      call snhcsh (s2,dummy,sigmap*del2,-1)
      ret = sum+(yp(i)*del1*(s1-ss)+yp(im1)*del2*(s2-ss))/
     *            (sigdel*sigmap*(1.+ss))
      return
      end

