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
      function curvd (t,n,x,y,yp,sigma)
c
      integer n
      real t,x(n),y(n),yp(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function differentiates a curve at a given point
c using a spline under tension. the subroutine nhlcurv1 should
c be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   t contains a real value at which the derivative is to be
c   determined.
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
c   curvd contains the derivative value.
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
c set up and perform differentiation
c
      del1 = t-x(im1)
      del2 = x(i)-t
      dels = x(i)-x(im1)
      sum = (y(i)-y(im1))/dels
      if (sigmap .ne. 0.) go to 1
      curvd = sum+(yp(i)*(2.*del1*del1-del2*(del1+dels))-
     *             yp(im1)*(2.*del2*del2-del1*(del2+dels)))
     *             /(6.*dels)
      return
    1 sigdel = sigmap*dels
      call snhcsh (ss,dummy,sigdel,-1)
      call snhcsh (dummy,c1,sigmap*del1,1)
      call snhcsh (dummy,c2,sigmap*del2,1)
      curvd = sum+(yp(i)*(c1-ss)-yp(im1)*(c2-ss))/
     *        (sigdel*sigmap*(1.+ss))
      return
      end
      function curvi (xl,xu,n,x,y,yp,sigma)
c
      integer n
      real xl,xu,x(n),y(n),yp(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function integrates a curve specified by a spline
c under tension between two given limits. the subroutine
c nhlcurv1 should be called earlier to determine necessary
c parameters.
c
c on input--
c
c   xl and xu contain the upper and lower limits of inte-
c   gration, respectively. (sl need not be less than or
c   equal to xu, curvi (xl,xu,...) .eq. -curvi (xu,xl,...) ).
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   yp is an array from subroutine nhlcurv1 containing
c   the values of the second derivatives at the nodes.
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
c   curvi contains the integral value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/(x(n)-x(1))
c
c determine actual upper and lower bounds
c
      xxl = xl
      xxu = xu
      ssign = 1.
      if (xl .lt. xu) go to 1
      xxl = xu
      xxu = xl
      ssign = -1.
      if (xl .gt. xu) go to 1
c
c return zero if xl .eq. xu
c
      curvi = 0.
      return
c
c search for proper intervals
c
    1 ilm1 = intrvl (xxl,x,n)
      il = ilm1+1
      ium1 = intrvl (xxu,x,n)
      iu = ium1+1
      if (il .eq. iu) go to 8
c
c integrate from xxl to x(il)
c
      sum = 0.
      if (xxl .eq. x(il)) go to 3
      del1 = xxl-x(ilm1)
      del2 = x(il)-xxl
      dels = x(il)-x(ilm1)
      t1 = (del1+dels)*del2/(2.*dels)
      t2 = del2*del2/(2.*dels)
      sum = t1*y(il)+t2*y(ilm1)
      if (sigma .eq. 0.) go to 2
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      sum = sum+((dels*dels*(cs-ss/2.)-del1*del1*(c1-ss/2.))
     *           *yp(il)+del2*del2*(c2-ss/2.)*yp(ilm1))/
     *          (sigmap*sigmap*dels*(1.+ss))
      go to 3
    2 sum = sum-t1*t1*dels*yp(il)/6.
     *         -t2*(del1*(del2+dels)+dels*dels)*yp(ilm1)/12.
c
c integrate over interior intervals
c
    3 if (iu-il .eq. 1) go to 6
      ilp1 = il+1
      do 5 i = ilp1,ium1
        dels = x(i)-x(i-1)
        sum = sum+(y(i)+y(i-1))*dels/2.
        if (sigma .eq. 0.) go to 4
        call snhcsh (ss,cs,sigmap*dels,3)
        sum = sum+(yp(i)+yp(i-1))*dels*(cs-ss/2.)/
     *            (sigmap*sigmap*(1.+ss))
        go to 5
    4   sum = sum-(yp(i)+yp(i-1))*dels*dels*dels/24.
    5   continue
c
c integrate from x(iu-1) to xxu
c
    6 if (xxu .eq. x(ium1)) go to 10
      del1 = xxu-x(ium1)
      del2 = x(iu)-xxu
      dels = x(iu)-x(ium1)
      t1 = del1*del1/(2.*dels)
      t2 = (del2+dels)*del1/(2.*dels)
      sum = sum+t1*y(iu)+t2*y(ium1)
      if (sigma .eq. 0.) go to 7
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      sum = sum+(yp(iu)*del1*del1*(c1-ss/2.)+yp(ium1)*
     *          (dels*dels*(cs-ss/2.)-del2*del2*(c2-ss/2.)))
     *         /(sigmap*sigmap*dels*(1.+ss))
      go to 10
    7 sum = sum-t1*(del2*(del1+dels)+dels*dels)*yp(iu)/12.
     *         -t2*t2*dels*yp(ium1)/6.
      go to 10
c
c integrate from xxl to xxu
c
    8 delu1 = xxu-x(ium1)
      delu2 = x(iu)-xxu
      dell1 = xxl-x(ium1)
      dell2 = x(iu)-xxl
      dels = x(iu)-x(ium1)
      deli = xxu-xxl
      t1 = (delu1+dell1)*deli/(2.*dels)
      t2 = (delu2+dell2)*deli/(2.*dels)
      sum = t1*y(iu)+t2*y(ium1)
      if (sigma .eq. 0.) go to 9
      call snhcsh (dummy,cu1,sigmap*delu1,2)
      call snhcsh (dummy,cu2,sigmap*delu2,2)
      call snhcsh (dummy,cl1,sigmap*dell1,2)
      call snhcsh (dummy,cl2,sigmap*dell2,2)
      call snhcsh (ss,dummy,sigmap*dels,-1)
      sum = sum+(yp(iu)*(delu1*delu1*(cu1-ss/2.)
     *            -dell1*dell1*(cl1-ss/2.))
     *          +yp(ium1)*(dell2*dell2*(cl2-ss/2.)
     *            -delu2*delu2*(cu2-ss/2.)))/
     *          (sigmap*sigmap*dels*(1.+ss))
      go to 10
    9 sum = sum-t1*(delu2*(dels+delu1)+dell2*(dels+dell1))*
     *             yp(iu)/12.
     *         -t2*(dell1*(dels+dell2)+delu1*(dels+delu2))*
     *             yp(ium1)/12.
c
c correct sign and return
c
   10 curvi = ssign*sum
      return
      end
      subroutine curvp1 (n,x,y,p,yp,temp,sigma,ierr)
c
      integer n,ierr
      real x(n),y(n),p,yp(n),temp(1),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a periodic interpolatory spline under tension
c through a sequence of functional values. for actual ends
c of the curve may be specified or omitted.  for actual
c computation of points on the curve it is necessary to call
c the function curvp2.
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
c   p is the period (p .gt. x(n)-x(1)).
c
c   yp is an array of length at least n.
c
c   temp is an array of length at least 2*n which is used
c   for scratch storage.
c
c and
c
c   sigma contains the tension factor.  this value indicates
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
c        = 2 if p is less than or equal to x(n)-x(1),
c        = 3 if x-values are not strictly increasing.
c
c and
c
c  n, x, y, and sigma are unaltered.
c
c this subroutine references package modules terms and
c snhcsh.
c
c-----------------------------------------------------------
c
      nm1 = n-1
      np1 = n+1
      ierr = 0
      if (n .le. 1) go to 6
      if (p .le. x(n)-x(1) .or. p .le. 0.) go to 7
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/p
c
c set up right hand side and tridiagonal system for yp and
c perform forward elimination
c
      delx1 = p-(x(n)-x(1))
      dx1 = (y(1)-y(n))/delx1
      call terms (diag1,sdiag1,sigmap,delx1)
      delx2 = x(2)-x(1)
      if (delx2 .le. 0.) go to 8
      dx2 = (y(2)-y(1))/delx2
      call terms (diag2,sdiag2,sigmap,delx2)
      diag = diag1+diag2
      yp(1) = (dx2-dx1)/diag
      temp(np1) = -sdiag1/diag
      temp(1) = sdiag2/diag
      dx1 = dx2
      diag1 = diag2
      sdiag1 = sdiag2
      if (n .eq. 2) go to 2
      do 1 i = 2,nm1
        npi = n+i
        delx2 = x(i+1)-x(i)
        if (delx2 .le. 0.) go to 8
        dx2 = (y(i+1)-y(i))/delx2
        call terms (diag2,sdiag2,sigmap,delx2)
        diag = diag1+diag2-sdiag1*temp(i-1)
        yp(i) = (dx2-dx1-sdiag1*yp(i-1))/diag
        temp(npi) = -temp(npi-1)*sdiag1/diag
        temp(i) = sdiag2/diag
        dx1 = dx2
        diag1 = diag2
    1   sdiag1 = sdiag2
    2 delx2 = p-(x(n)-x(1))
      dx2 = (y(1)-y(n))/delx2
      call terms (diag2,sdiag2,sigmap,delx2)
      yp(n) = dx2-dx1
      temp(nm1) = temp(2*n-1)-temp(nm1)
      if (n .eq. 2) go to 4
c
c perform first step of back substitution
c
      do 3 i = 3,n
        ibak = np1-i
        npibak =n+ibak
        yp(ibak) = yp(ibak)-temp(ibak)*yp(ibak+1)
    3   temp(ibak) =temp(npibak)-temp(ibak)*temp(ibak+1)
    4 yp(n) = (yp(n)-sdiag2*yp(1)-sdiag1*yp(nm1))/
     *        (diag1+diag2+sdiag2*temp(1)+sdiag1*temp(nm1))
c
c perform second step of back substitution
c
      ypn =   yp(n)
      do 5 i = 1,nm1
    5    yp(i) = yp(i)+temp(i)*ypn
      return
c
c too few points
c
    6 ierr = 1
      return
c
c period too small
c
    7 ierr = 2
      return
c
c x-values not strictly increasing
c
    8 ierr = 3
      return
      end
      function curvp2 (t,n,x,y,p,yp,sigma)
c
      integer n
      real t,x(n),y(n),p,yp(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function interpolates a curve at a given point using
c a periodic spline under tension. the subroutine curvp1
c should be called earlier to determine certain necessary
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
c   p contains the period.
c
c   yp is an array of second derivative values of the curve
c   at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, p, yp, and sigma should be input
c unaltered from the output of curvp1.
c
c on output--
c
c   curvp2 contains the interpolated value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvp and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      im1 = intrvp (t,x,n,p,tp)
      i = im1+1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/p
c
c set up and perform interpolation
c
      del1 = tp-x(im1)
      if (im1 .eq. n) go to 1
      del2 = x(i)-tp
      dels = x(i)-x(im1)
      go to 2
    1 i = 1
      del2 = x(1)+p-tp
      dels = p-(x(n)-x(1))
    2 sum = (y(i)*del1+y(im1)*del2)/dels
      if (sigmap .ne. 0.) go to 3
      curvp2 = sum-del1*del2*(yp(i)*(del1+dels)+yp(im1)*
     *        (del2+dels))/(6.*dels)
      return
    3 sigdel = sigmap*dels
      call snhcsh (ss,dummy,sigdel,-1)
      call snhcsh (s1,dummy,sigmap*del1,-1)
      call snhcsh (s2,dummy,sigmap*del2,-1)
      curvp2 = sum+(yp(i)*del1*(s1-ss)+yp(im1)*del2*(s2-ss))/
     *            (sigdel*sigmap*(1.+ss))
      return
      end
      function curvpi (xl,xu,n,x,y,p,yp,sigma)
c
      integer n
      real xl,xu,x(n),y(n),p,yp(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function integrates a curve specified by a periodic
c spline under tension between two given limits. the
c subroutine curvp1 should be called earlier to determine
c necessary parameters.
c
c on input--
c
c   xl and xu contain the upper and lower limits of inte-
c   gration, respectively. (sl need not be less than or
c   equal to xu, curvpi (xl,xu,...) .eq. -curvpi (xu,xl,...) ).
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   p contains the period.
c
c   yp is an array from subroutine curvp1 containing
c   the values of the second derivatives at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, p, yp, and sigma should be input
c unaltered from the output of curvp1.
c
c on output--
c
c
c   curvpi contains the integral value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvp and
c snhcsh.
c
c--------------------------------------------------------------
c
      integer uper
      logical bdy
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/p
c
c determine actual upper and lower bounds
c
      x1pp = x(1)+p
      isign = 1
      ilm1 = intrvp (xl,x,n,p,xxl)
      lper = int((xl-x(1))/p)
      if (xl .lt. x(1)) lper = lper-1
      ium1 = intrvp (xu,x,n,p,xxu)
      uper = int((xu-x(1))/p)
      if (xu .lt. x(1)) uper = uper-1
      ideltp = uper-lper
      bdy = float(ideltp)*(xxu-xxl) .lt. 0.
      if ((ideltp .eq. 0 .and. xxu .lt. xxl) .or.
     *    ideltp .lt. 0) isign = -1
      if (bdy) ideltp = ideltp-isign
      if (xxu .ge. xxl) go to 1
      xsave = xxl
      xxl = xxu
      xxu = xsave
      isave = ilm1
      ilm1 = ium1
      ium1 = isave
    1 il = ilm1+1
      if (ilm1 .eq. n) il = 1
      xil = x(il)
      if (ilm1 .eq. n) xil = x1pp
      iu = ium1+1
      if (ium1 .eq. n) iu = 1
      xiu = x(iu)
      if (ium1 .eq. n) xiu = x1pp
      s1 = 0.
      if (ilm1 .eq. 1 .or. (ideltp .eq. 0 .and.
     *    .not. bdy)) go to 4
c
c integrate from x(1) to x(ilm1), store in s1
c
      do 3 i = 2,ilm1
        dels = x(i)-x(i-1)
        s1 = s1+(y(i)+y(i-1))*dels/2.
        if (sigma .eq. 0.) go to 2
        call snhcsh (ss,cs,sigmap*dels,3)
        s1 = s1+(yp(i)+yp(i-1))*dels*(cs-ss/2.)/
     *            (sigmap*sigmap*(1.+ss))
        go to 3
    2   s1 = s1-(yp(i)+yp(i-1))*dels*dels*dels/24.
    3   continue
    4 s2 = 0.
      if (x(ilm1) .ge. xxl .or. (ideltp .eq. 0
     *    .and. .not. bdy)) go to 6
c
c integrate from x(ilm1) to xxl, store in s2
c
      del1 = xxl-x(ilm1)
      del2 = xil-xxl
      dels = xil-x(ilm1)
      t1 = del1*del1/(2.*dels)
      t2 = (del2+dels)*del1/(2.*dels)
      s2 = t1*y(il)+t2*y(ilm1)
      if (sigma .eq. 0.) go to 5
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      s2 = s2+(yp(il)*del1*del1*(c1-ss/2.)+yp(ilm1)*
     *          (dels*dels*(cs-ss/2.)-del2*del2*(c2-ss/2.)))
     *         /(sigmap*sigmap*dels*(1.+ss))
      go to 6
    5 s2 = s2-t1*(del2*(del1+dels)
     *     +dels*dels)*yp(il)/12.
     *     -t2*t2*dels*yp(ilm1)/6.
    6 s3 = 0.
      if (xxl .ge. xil .or. (ideltp .eq. 0 .and. bdy)
     *    .or. ilm1 .eq. ium1) go to 8
c
c integrate from xxl to xil, store in s3
c
      del1 = xxl-x(ilm1)
      del2 = xil-xxl
      dels = xil-x(ilm1)
      t1 = (del1+dels)*del2/(2.*dels)
      t2 = del2*del2/(2.*dels)
      s3 = t1*y(il)+t2*y(ilm1)
      if (sigma .eq. 0.) go to 7
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      s3 = s3+((dels*dels*(cs-ss/2.)-del1*del1*(c1-ss/2.))
     *           *yp(il)+del2*del2*(c2-ss/2.)*yp(ilm1))/
     *          (sigmap*sigmap*dels*(1.+ss))
      go to 8
    7 s3 = s3-t1*t1*dels*yp(il)/6.
     *     -t2*(del1*(del2+dels)+dels*dels)*
     *     yp(ilm1)/12.
    8 s4 = 0.
      if (ilm1 .ge. ium1-1 .or. (ideltp .eq. 0 .and. bdy))
     *   go to 11
c
c integrate from xil to x(ium1), store in s4
c
      ilp1 = il+1
      do 10 i = ilp1,ium1
        dels = x(i)-x(i-1)
        s4 = s4+(y(i)+y(i-1))*dels/2.
        if (sigma .eq. 0.) go to 9
        call snhcsh (ss,cs,sigmap*dels,3)
        s4 = s4+(yp(i)+yp(i-1))*dels*(cs-ss/2.)/
     *            (sigmap*sigmap*(1.+ss))
        go to 10
    9   s4 = s4-(yp(i)+yp(i-1))*dels*dels*dels/24.
   10   continue
   11 s5 = 0.
      if (x(ium1) .ge. xxu .or. (ideltp .eq. 0 .and. bdy)
     *    .or. ilm1 .eq. ium1) go to 13
c
c integrate from x(ium1) to xxu, store in s5
c
      del1 = xxu-x(ium1)
      del2 = xiu-xxu
      dels = xiu-x(ium1)
      t1 = del1*del1/(2.*dels)
      t2 = (del2+dels)*del1/(2.*dels)
      s5 = t1*y(iu)+t2*y(ium1)
      if (sigma .eq. 0.) go to 12
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      s5 = s5+(yp(iu)*del1*del1*(c1-ss/2.)+yp(ium1)*
     *          (dels*dels*(cs-ss/2.)-del2*del2*(c2-ss/2.)))
     *         /(sigmap*sigmap*dels*(1.+ss))
      go to 13
   12 s5 = s5-t1*(del2*(del1+dels)
     *     +dels*dels)*yp(iu)/12.
     *     -t2*t2*dels*yp(ium1)/6.
   13 s6 = 0.
      if (xxu .ge. xiu .or. (ideltp .eq. 0 .and.
     *    .not. bdy)) go to 15
c
c integrate from xxu to xiu, store in s6
c
      del1 = xxu-x(ium1)
      del2 = xiu-xxu
      dels = xiu-x(ium1)
      t1 = (del1+dels)*del2/(2.*dels)
      t2 = del2*del2/(2.*dels)
      s6 = t1*y(iu)+t2*y(ium1)
      if (sigma .eq. 0.) go to 14
      call snhcsh (dummy,c1,sigmap*del1,2)
      call snhcsh (dummy,c2,sigmap*del2,2)
      call snhcsh (ss,cs,sigmap*dels,3)
      s6 = s6+((dels*dels*(cs-ss/2.)-del1*del1*(c1-ss/2.))
     *           *yp(iu)+del2*del2*(c2-ss/2.)*yp(ium1))/
     *          (sigmap*sigmap*dels*(1.+ss))
      go to 15
   14 s6 = s6-t1*t1*dels*yp(iu)/6.
     *     -t2*(del1*(del2+dels)+dels*dels)*
     *     yp(ium1)/12.
   15 s7 = 0.
      if (iu .eq. 1 .or. (ideltp .eq. 0 .and. .not. bdy))
     *    go to 18
c
c integrate from xiu to x1pp, store in s7
c
      np1 = n+1
      iup1 = iu+1
      do 17 ii = iup1,np1
        im1 = ii-1
        i = ii
        if (i .eq. np1) i=1
        dels = x(i)-x(im1)
        if (dels .le. 0.) dels=dels+p
        s7 = s7+(y(i)+y(im1))*dels/2.
        if (sigma .eq. 0.) go to 16
        call snhcsh (ss,cs,sigmap*dels,3)
        s7 = s7+(yp(i)+yp(im1))*dels*(cs-ss/2.)/
     *            (sigmap*sigmap*(1.+ss))
        go to 17
   16   s7 = s7-(yp(i)+yp(im1))*dels*dels*dels/24.
   17   continue
   18 s8 = 0.
      if (ilm1 .lt. ium1 .or. (ideltp .eq. 0 .and. bdy))
     *   go to 20
c
c integrate from xxl to xxu, store in s8
c
      delu1 = xxu-x(ium1)
      delu2 = xiu-xxu
      dell1 = xxl-x(ium1)
      dell2 = xiu-xxl
      dels = xiu-x(ium1)
      deli = xxu-xxl
      t1 = (delu1+dell1)*deli/(2.*dels)
      t2 = (delu2+dell2)*deli/(2.*dels)
      s8 = t1*y(iu)+t2*y(ium1)
      if (sigma .eq. 0.) go to 19
      call snhcsh (dummy,cu1,sigmap*delu1,2)
      call snhcsh (dummy,cu2,sigmap*delu2,2)
      call snhcsh (dummy,cl1,sigmap*dell1,2)
      call snhcsh (dummy,cl2,sigmap*dell2,2)
      call snhcsh (ss,dummy,sigmap*dels,-1)
      s8 = s8+(yp(iu)*(delu1*delu1*(cu1-ss/2.)
     *            -dell1*dell1*(cl1-ss/2.))
     *          +yp(ium1)*(dell2*dell2*(cl2-ss/2.)
     *            -delu2*delu2*(cu2-ss/2.)))/
     *          (sigmap*sigmap*dels*(1.+ss))
      go to 20
   19 s8 = s8-t1*(delu2*(dels+delu1)
     *     +dell2*(dels+dell1))*yp(iu)/12.
     *     -t2*(dell1*(dels+dell2)
     *     +delu1*(dels+delu2))*yp(ium1)/12.
   20 so = s1+s2+s6+s7
      si = s3+s4+s5+s8
      if (bdy) go to 21
      curvpi = float(ideltp)*(so+si)+float(isign)*si
      return
   21 curvpi = float(ideltp)*(so+si)+float(isign)*so
      return
      end
      subroutine kurv1 (n,x,y,slp1,slpn,islpsw,xp,yp,temp,s,
     *                  sigma,ierr)
c
      integer n,islpsw,ierr
      real x(n),y(n),slp1,slpn,xp(n),yp(n),temp(n),s(n),
     *     sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a spline under tension forming a curve in the
c plane and passing through a sequence of pairs (x(1),y(1)),
c ...,(x(n),y(n)). for actual computation of points on the
c curve it is necessary to call the subroutine kurv2.
c
c on input--
c
c   n is the number of points to be interpolated (n.ge.2).
c
c   x is an array containing the n x-coordinates of the
c   points.
c
c   y is an array containing the n y-coordinates of the
c   points. (adjacent x-y pairs must be distinct, i. e.
c   either x(i) .ne. x(i+1) or y(i) .ne. y(i+1), for
c   i = 1,...,n-1.)
c
c   slp1 and slpn contain the desired values for the angles
c   (in radians) of the slope at (x(1),y(1)) and (x(n),y(n))
c   respectively. the angles are measured counter-clock-
c   wise from the x-axis and the positive sense of the curve
c   is assumed to be that moving from point 1 to point n.
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
c   xp and yp are arrays of length at least n.
c
c   temp is an array of length at least n which is used
c   for scratch storage.
c
c   s is an array of length at least n.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e.g. .001) the resulting curve is approximately a cubic
c   spline. if abs(sigma) is large (e. g. 50.) the resulting
c   curve is nearly a polygonal line. if sigma equals zero a
c   cubic spline results. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   xp and yp contain information about the curvature of the
c   curve at the given nodes.
c
c   s contains the polygonal arclengths of the curve.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if adjacent coordinate pairs coincide.
c
c and
c
c   n, x, y, slp1, slpn, islpsw, and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      nm1 = n-1
      np1 = n+1
      ierr = 0
      if (n .le. 1) go to 11
c
c determine polygonal arclengths
c
      s(1) = 0.
      do 1 i = 2,n
        im1 = i-1
    1   s(i) = s(im1)+sqrt((x(i)-x(im1))**2+
     *         (y(i)-y(im1))**2)
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/s(n)
c
c approximate end slopes
c
      if (islpsw .ge. 2) go to 2
      slpp1x = cos(slp1)
      slpp1y = sin(slp1)
      go to 4
    2 dels1 = s(2)-s(1)
      dels2 = dels1+dels1
      if (n .gt. 2) dels2 = s(3)-s(1)
      if (dels1 .eq. 0. .or. dels2 .eq. 0.) go to 12
      call ceez (dels1,dels2,sigmap,c1,c2,c3,n)
      sx = c1*x(1)+c2*x(2)
      sy = c1*y(1)+c2*y(2)
      if (n .eq. 2) go to 3
      sx = sx+c3*x(3)
      sy = sy+c3*y(3)
    3 delt = sqrt(sx*sx+sy*sy)
      slpp1x = sx/delt
      slpp1y = sy/delt
    4 if (islpsw .eq. 1 .or. islpsw .eq. 3) go to 5
      slppnx = cos(slpn)
      slppny = sin(slpn)
      go to 7
    5 delsn = s(n)-s(nm1)
      delsnm = delsn+delsn
      if (n .gt. 2) delsnm = s(n)-s(n-2)
      if (delsn .eq. 0. .or. delsnm .eq. 0.) go to 12
      call ceez (-delsn,-delsnm,sigmap,c1,c2,c3,n)
      sx = c1*x(n)+c2*x(nm1)
      sy = c1*y(n)+c2*y(nm1)
      if (n .eq. 2) go to 6
      sx = sx+c3*x(n-2)
      sy = sy+c3*y(n-2)
    6 delt = sqrt(sx*sx+sy*sy)
      slppnx = sx/delt
      slppny = sy/delt
c
c set up right hand sides and tridiagonal system for xp and
c yp and perform forward elimination
c
    7 dx1 = (x(2)-x(1))/s(2)
      dy1 = (y(2)-y(1))/s(2)
      call terms (diag1,sdiag1,sigmap,s(2))
      xp(1) = (dx1-slpp1x)/diag1
      yp(1) = (dy1-slpp1y)/diag1
      temp(1) = sdiag1/diag1
      if (n .eq. 2) go to 9
      do 8 i = 2,nm1
        dels2 = s(i+1)-s(i)
        if (dels2 .eq. 0.) go to 12
        dx2 = (x(i+1)-x(i))/dels2
        dy2 = (y(i+1)-y(i))/dels2
        call terms (diag2,sdiag2,sigmap,dels2)
        diag = diag1+diag2-sdiag1*temp(i-1)
        diagin = 1./diag
        xp(i) = (dx2-dx1-sdiag1*xp(i-1))*diagin
        yp(i) = (dy2-dy1-sdiag1*yp(i-1))*diagin
        temp(i) = sdiag2*diagin
        dx1 = dx2
        dy1 = dy2
        diag1 = diag2
    8   sdiag1 = sdiag2
    9 diag = diag1-sdiag1*temp(nm1)
      xp(n) = (slppnx-dx1-sdiag1*xp(nm1))/diag
      yp(n) = (slppny-dy1-sdiag1*yp(nm1))/diag
c
c perform back substitution
c
      do 10 i = 2,n
        ibak = np1-i
        xp(ibak) = xp(ibak)-temp(ibak)*xp(ibak+1)
   10   yp(ibak) = yp(ibak)-temp(ibak)*yp(ibak+1)
      return
c
c too few points
c
   11 ierr = 1
      return
c
c coincident adjacent points
c
   12 ierr = 2
      return
      end
      subroutine kurv2 (t,xs,ys,n,x,y,xp,yp,s,sigma)
c
      integer n
      real t,xs,ys,x(n),y(n),xp(n),yp(n),s(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a curve in the plane. the subroutine
c kurv1 should be called earlier to determine certain
c necessary parameters. the resulting curve has a parametric
c representation both of whose components are splines under
c tension and functions of the polygonal arclength
c parameter.
c
c on input--
c
c   t contains a real value to be mapped to a point on the
c   curve. the interval (0.,1.) is mapped onto the entire
c   curve, with 0. mapping to (x(1),y(1)) and 1. mapping
c   to (x(n),y(n)). values outside this interval result in
c   extrapolation.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurv1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s, and sigma should be
c input unaltered from the output of kurv1.
c
c on output--
c
c   xs and ys contain the x- and y-coordinates of the image
c   point on the curve.
c
c none of the input parameters are altered.
c
c this subroutine references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      tn = s(n)*t
      im1 = intrvl(tn,s,n)
      i = im1+1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/s(n)
c
c set up and perform interpolation
c
      del1 = tn-s(im1)
      del2 = s(i)-tn
      dels = s(i)-s(im1)
      sumx = (x(i)*del1+x(im1)*del2)/dels
      sumy = (y(i)*del1+y(im1)*del2)/dels
      if (sigmap .ne. 0.) go to 1
      d = del1*del2/(6.*dels)
      c1 = (del1+dels)*d
      c2 = (del2+dels)*d
      xs = sumx-xp(i)*c1-xp(im1)*c2
      ys = sumy-yp(i)*c1-yp(im1)*c2
      return
    1 sigdel = sigmap*dels
      call snhcsh(ss,dummy,sigdel,-1)
      call snhcsh(s1,dummy,sigmap*del1,-1)
      call snhcsh(s2,dummy,sigmap*del2,-1)
      d = sigdel*sigmap*(1.+ss)
      c1 = del1*(s1-ss)/d
      c2 = del2*(s2-ss)/d
      xs = sumx+xp(i)*c1+xp(im1)*c2
      ys = sumy+yp(i)*c1+yp(im1)*c2
      return
      end
      subroutine kurvd (t,xs,ys,xst,yst,xstt,ystt,n,x,y,xp,
     *                  yp,s,sigma)
c
      integer n
      real t,xs,ys,xst,yst,xstt,ystt,x(n),y(n),xp(n),yp(n),
     *     s(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a curve in the plane. it also
c returns the first and second derivatives of the component
c functions. the subroutine kurv1 should be called earlier
c to determine certain necessary parameters. the resulting
c curve has a parametric representation both of whose
c components are splines under tension and functions of the
c polygonal arclength parameter.
c
c on input--
c
c   t contains a real value to be mapped to a point on the
c   curve. the interval (0.,1.) is mapped onto the entire
c   curve, with 0. mapping to (x(1),y(1)) and 1. mapping
c   to (x(n),y(n)). values outside this interval result in
c   extrapolation.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurv1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s, and sigma should be
c input unaltered from the output of kurv1.
c
c on output--
c
c   xs and ys contain the x- and y-coordinates of the image
c   point on the curve. xst and yst contain the first
c   derivatives of the x- and y-components of the mapping
c   with respect to t. xstt and ystt contain the second
c   derivatives of the x- and y-components of the mapping
c   with respect to t.
c
c none of the input parameters are altered.
c
c this subroutine references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      tn = s(n)*t
      im1 = intrvl(tn,s,n)
      i = im1+1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n-1)/s(n)
c
c set up and perform interpolation
c
      del1 = tn-s(im1)
      del2 = s(i)-tn
      dels = s(i)-s(im1)
      sumx = (x(i)*del1+x(im1)*del2)/dels
      sumy = (y(i)*del1+y(im1)*del2)/dels
      sumxt = s(n)*(x(i)-x(im1))/dels
      sumyt = s(n)*(y(i)-y(im1))/dels
      if (sigmap .ne. 0.) go to 1
      dels6 = 6.*dels
      d = del1*del2/dels6
      c1 = -(del1+dels)*d
      c2 = -(del2+dels)*d
      dels6 = dels6/s(n)
      ct1 = (2.*del1*del1-del2*(del1+dels))/dels6
      ct2 = -(2.*del2*del2-del1*(del2+dels))/dels6
      dels = dels/(s(n)*s(n))
      ctt1 = del1/dels
      ctt2 = del2/dels
      go to 2
    1 sigdel = sigmap*dels
      call snhcsh (ss,dummy,sigdel,-1)
      call snhcsh (s1,co1,sigmap*del1,0)
      call snhcsh (s2,co2,sigmap*del2,0)
      d = sigdel*sigmap*(1.+ss)
      c1 = del1*(s1-ss)/d
      c2 = del2*(s2-ss)/d
      ct1 = (co1-ss)*s(n)/d
      ct2 = -(co2-ss)*s(n)/d
      ctt1 = del1*(1.+s1)*s(n)*s(n)/(dels*(1.+ss))
      ctt2 = del2*(1.+s2)*s(n)*s(n)/(dels*(1.+ss))
    2 xs = sumx+c1*xp(i)+c2*xp(im1)
      ys = sumy+c1*yp(i)+c2*yp(im1)
      xst = sumxt+ct1*xp(i)+ct2*xp(im1)
      yst = sumyt+ct1*yp(i)+ct2*yp(im1)
      xstt = ctt1*xp(i)+ctt2*xp(im1)
      ystt = ctt1*yp(i)+ctt2*yp(im1)
      return
      end
      subroutine kurvp1 (n,x,y,xp,yp,temp,s,sigma,ierr)
c
      integer n,ierr
      real x(n),y(n),xp(n),yp(n),temp(1),s(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a spline under tension forming a closed curve in
c the plane and passing through a sequence of pairs
c (x(1),y(1)),...,(x(n),y(n)). for actual computation of
c points on the curve it is necessary to call the subroutine
c kurvp2.
c
c on input--
c
c   n is the number of points to be interpolated (n.ge.2).
c
c   x is an array containing the n x-coordinates of the
c   points.
c
c   y is an array containing the n y-coordinates of the
c   points. (adjacent x-y pairs must be distinct, i. e.
c   either x(i) .ne. x(i+1) or y(i) .ne. y(i+1), for
c   i = 1,...,n-1 and either x(1) .ne. x(n) or y(1) .ne. y(n).)
c
c   xp and yp are arrays of length at least n.
c
c   temp is an array of length at least 2*n which is used
c   for scratch storage.
c
c   s is an array of length at least n.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e.g. .001) the resulting curve is approximately a cubic
c   spline. if abs(sigma) is large (e. g. 50.) the resulting
c   curve is nearly a polygonal line. if sigma equals zero a
c   cubic spline results. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   xp and yp contain information about the curvature of the
c   curve at the given nodes.
c
c   s contains the polygonal arclengths of the curve.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if adjacent coordinate pairs coincide.
c
c and
c
c   n, x, y, and sigma are unaltered,
c
c this subroutine references package modules terms and
c snhcsh.
c
c-----------------------------------------------------------
c
      nm1 = n-1
      np1 = n+1
      ierr = 0
      if (n .le. 1) go to 7
c
c determine polygonal arclengths
c
      s(1) = sqrt((x(n)-x(1))**2+(y(n)-y(1))**2)
      do 1 i = 2,n
        im1 = i-1
    1   s(i) = s(im1)+sqrt((x(i)-x(im1))**2+
     *         (y(i)-y(im1))**2)
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/s(n)
c
c set up right hand sides of tridiagonal (with corner
c elements) linear system for xp and yp
c
      dels1 = s(1)
      if (dels1 .eq. 0.) go to 8
      dx1 = (x(1)-x(n))/dels1
      dy1 = (y(1)-y(n))/dels1
      call terms(diag1,sdiag1,sigmap,dels1)
      dels2 = s(2)-s(1)
      if (dels2 .eq. 0.) go to 8
      dx2 = (x(2)-x(1))/dels2
      dy2 = (y(2)-y(1))/dels2
      call terms(diag2,sdiag2,sigmap,dels2)
      diag = diag1+diag2
      diagin = 1./diag
      xp(1) = (dx2-dx1)*diagin
      yp(1) = (dy2-dy1)*diagin
      temp(np1) = -sdiag1*diagin
      temp(1) = sdiag2*diagin
      dx1 = dx2
      dy1 = dy2
      diag1 = diag2
      sdiag1 = sdiag2
      if (n .eq. 2) go to 3
      do 2 i = 2,nm1
        npi = n+i
        dels2 = s(i+1)-s(i)
        if (dels2 .eq. 0.) go to 8
        dx2 = (x(i+1)-x(i))/dels2
        dy2 = (y(i+1)-y(i))/dels2
        call terms(diag2,sdiag2,sigmap,dels2)
        diag = diag1+diag2-sdiag1*temp(i-1)
        diagin = 1./diag
        xp(i) = (dx2-dx1-sdiag1*xp(i-1))*diagin
        yp(i) = (dy2-dy1-sdiag1*yp(i-1))*diagin
        temp(npi) = -temp(npi-1)*sdiag1*diagin
        temp(i) = sdiag2*diagin
        dx1 = dx2
        dy1 = dy2
        diag1 = diag2
    2   sdiag1 = sdiag2
    3 dels2 = s(1)
      dx2 = (x(1)-x(n))/dels2
      dy2 = (y(1)-y(n))/dels2
      call terms(diag2,sdiag2,sigmap,dels2)
      xp(n) = dx2-dx1
      yp(n) = dy2-dy1
      temp(nm1) = temp(2*n-1)-temp(nm1)
      if (n.eq.2) go to 5
c
c perform first step of back substitution
c
      do 4 i = 3,n
        ibak = np1-i
        npibak = n+ibak
        xp(ibak) = xp(ibak)-temp(ibak)*xp(ibak+1)
        yp(ibak) = yp(ibak)-temp(ibak)*yp(ibak+1)
    4   temp(ibak) = temp(npibak)-temp(ibak)*temp(ibak+1)
    5 xp(n) = (xp(n)-sdiag2*xp(1)-sdiag1*xp(nm1))/
     *        (diag1+diag2+sdiag2*temp(1)+sdiag1*temp(nm1))
      yp(n) = (yp(n)-sdiag2*yp(1)-sdiag1*yp(nm1))/
     *        (diag1+diag2+sdiag2*temp(1)+sdiag1*temp(nm1))
c
c perform second step of back substitution
c
      xpn = xp(n)
      ypn = yp(n)
      do 6 i = 1,nm1
        xp(i) = xp(i)+temp(i)*xpn
    6   yp(i) = yp(i)+temp(i)*ypn
      return
c
c too few points
c
    7 ierr = 1
      return
c
c coincident adjacent points
c
    8 ierr = 2
      return
      end
      subroutine kurvp2 (t,xs,ys,n,x,y,xp,yp,s,sigma)
c
      integer n
      real t,xs,ys,x(n),y(n),xp(n),yp(n),s(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a closed curve in the plane. the
c subroutine kurvp1 should be called earlier to determine
c certain necessary parameters. the resulting curve has a
c parametric representation both of whose components are
c periodic splines under tension and functions of the poly-
c gonal arclength parameter.
c
c on input--
c
c   t contains a value to be mapped onto the curve. the
c   interval (0.,1.) is mapped onto the entire closed curve
c   with both 0. and 1. mapping to (x(1),y(1)). the mapping
c   is periodic with period one thus any interval of the
c   form (tt,tt+1.) maps onto the entire curve.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurvp1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s and sigma should
c be input unaltered from the output of kurvp1.
c
c on output--
c
c   xs and ys contain the x- and y-coordinates of the image
c   point on the curve.
c
c none of the input parameters are altered.
c
c this subroutine references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      tn = t-float(ifix(t))
      if (tn .lt. 0.) tn = tn+1.
      tn = s(n)*tn+s(1)
      im1 = n
      if (tn .lt. s(n)) im1 = intrvl(tn,s,n)
      i = im1+1
      if (i .gt. n) i = 1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/s(n)
c
c set up and perform interpolation
c
      si = s(i)
      if (im1 .eq. n) si = s(n)+s(1)
      del1 = tn-s(im1)
      del2 = si-tn
      dels = si-s(im1)
      sumx = (x(i)*del1+x(im1)*del2)/dels
      sumy = (y(i)*del1+y(im1)*del2)/dels
      if (sigmap .ne. 0.) go to 1
      d = del1*del2/(6.*dels)
      c1 = (del1+dels)*d
      c2 = (del2+dels)*d
      xs = sumx-xp(i)*c1-xp(im1)*c2
      ys = sumy-yp(i)*c1-yp(im1)*c2
      return
    1 sigdel = sigmap*dels
      call snhcsh(ss,dummy,sigdel,-1)
      call snhcsh(s1,dummy,sigmap*del1,-1)
      call snhcsh(s2,dummy,sigmap*del2,-1)
      d = sigdel*sigmap*(1.+ss)
      ci = del1*(s1-ss)/d
      cim1 = del2*(s2-ss)/d
      xs = sumx+xp(i)*ci+xp(im1)*cim1
      ys = sumy+yp(i)*ci+yp(im1)*cim1
      return
      end
      subroutine kurvpd (t,xs,ys,xst,yst,xstt,ystt,n,x,y,xp,
     *                  yp,s,sigma)
c
      integer n
      real t,xs,ys,xst,yst,xstt,ystt,x(n),y(n),xp(n),yp(n),
     *     s(n),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a closed curve in the plane. it also
c returns the first and second derivatives of the component
c functions. the subroutine kurvp1 should be called earlier
c to determine certain necessary parameters. the resulting
c curve has a parametric representation both of whose
c components are periodic splines under tension and
c functions of the polygonal arclength parameter.
c
c on input--
c
c   t contains a value to be mapped onto the curve. the
c   interval (0.,1.) is mapped onto the entire closed curve
c   with both 0. and 1. mapping to (x(1),y(1)). the mapping
c   is periodic with period one thus any interval of the
c   form (tt,tt+1.) maps onto the entire curve.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurvp1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s and sigma should
c be input unaltered from the output of kurvp1.
c
c on output--
c
c   xs and ys contain the x- and y-coordinates of the image
c   point on the curve. xst and yst contain the first
c   derivatives of the x- and y-components of the mapping
c   with respect to t. xstt and ystt contain the second
c   derivatives of the x- and y-components of the mapping
c   with respect to t.
c
c none of the input parameters are altered.
c
c this subroutine references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c determine interval
c
      tn = t-float(ifix(t))
      if (tn .lt. 0.) tn = tn+1.
      tn = s(n)*tn+s(1)
      im1 = n
      if (tn .lt. s(n)) im1 = intrvl(tn,s,n)
      i = im1+1
      if (i .gt. n) i = 1
c
c denormalize tension factor
c
      sigmap = abs(sigma)*float(n)/s(n)
c
c set up and perform interpolation
c
      si = s(i)
      if (im1 .eq. n) si = s(n)+s(1)
      del1 = tn-s(im1)
      del2 = si-tn
      dels = si-s(im1)
      sumx = (x(i)*del1+x(im1)*del2)/dels
      sumy = (y(i)*del1+y(im1)*del2)/dels
      sumxt = s(n)*(x(i)-x(im1))/dels
      sumyt = s(n)*(y(i)-y(im1))/dels
      if (sigmap .ne. 0.) go to 1
      dels6 = 6.*dels
      d = del1*del2/dels6
      c1 = -(del1+dels)*d
      c2 = -(del2+dels)*d
      dels6 = dels6/s(n)
      ct1 = (2.*del1*del1-del2*(del1+dels))/dels6
      ct2 = -(2.*del2*del2-del1*(del2+dels))/dels6
      dels = dels/(s(n)*s(n))
      ctt1 = del1/dels
      ctt2 = del2/dels
      go to 2
    1 sigdel = sigmap*dels
      call snhcsh (ss,dummy,sigdel,-1)
      call snhcsh (s1,co1,sigmap*del1,0)
      call snhcsh (s2,co2,sigmap*del2,0)
      d = sigdel*sigmap*(1.+ss)
      c1 = del1*(s1-ss)/d
      c2 = del2*(s2-ss)/d
      ct1 = (co1-ss)*s(n)/d
      ct2 = -(co2-ss)*s(n)/d
      ctt1 = del1*(1.+s1)*s(n)*s(n)/(dels*(1.+ss))
      ctt2 = del2*(1.+s2)*s(n)*s(n)/(dels*(1.+ss))
    2 xs = sumx+c1*xp(i)+c2*xp(im1)
      ys = sumy+c1*yp(i)+c2*yp(im1)
      xst = sumxt+ct1*xp(i)+ct2*xp(im1)
      yst = sumyt+ct1*yp(i)+ct2*yp(im1)
      xstt = ctt1*xp(i)+ctt2*xp(im1)
      ystt = ctt1*yp(i)+ctt2*yp(im1)
      return
      end
      subroutine surf1 (m,n,x,y,z,iz,zx1,zxm,zy1,zyn,zxy11,
     *                  zxym1,zxy1n,zxymn,islpsw,zp,temp,
     *                  sigma,ierr)
c
      integer m,n,iz,islpsw,ierr
      real x(m),y(n),z(iz,n),zx1(n),zxm(n),zy1(m),zyn(m),
     *     zxy11,zxym1,zxy1n,zxymn,zp(m,n,3),temp(1),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute an interpolatory surface passing through a rect-
c angular grid of functional values. the surface determined
c can be represented as the tensor product of splines under
c tension. the x- and y-partial derivatives around the
c boundary and the x-y-partial derivatives at the four
c corners may be specified or omitted. for actual mapping
c of points onto the surface it is necessary to call the
c function surf2.
c
c on input--
c
c   m is the number of grid lines in the x-direction, i. e.
c   lines parallel to the y-axis (m .ge. 2).
c
c   n is the number of grid lines in the y-direction, i. e.
c   lines parallel to the x-axis (n .ge. 2).
c
c   x is an array of the m x-coordinates of the grid lines
c   in the x-direction. these should be strictly increasing.
c
c   y is an array of the n y-coordinates of the grid lines
c   in the y-direction. these should be strictly increasing.
c
c   z is an array of the m * n functional values at the grid
c   points, i. e. z(i,j) contains the functional value at
c   (x(i),y(j)) for i = 1,...,m and j = 1,...,n.
c
c   iz is the row dimension of the matrix z used in the
c   calling program (iz .ge. m).
c
c   zx1 and zxm are arrays of the m x-partial derivatives
c   of the function along the x(1) and x(m) grid lines,
c   respectively. thus zx1(j) and zxm(j) contain the x-part-
c   ial derivatives at the points (x(1),y(j)) and
c   (x(m),y(j)), respectively, for j = 1,...,n. either of
c   these parameters will be ignored (and approximations
c   supplied internally) if islpsw so indicates.
c
c   zy1 and zyn are arrays of the n y-partial derivatives
c   of the function along the y(1) and y(n) grid lines,
c   respectively. thus zy1(i) and zyn(i) contain the y-part-
c   ial derivatives at the points (x(i),y(1)) and
c   (x(i),y(n)), respectively, for i = 1,...,m. either of
c   these parameters will be ignored (and estimations
c   supplied internally) if islpsw so indicates.
c
c   zxy11, zxym1, zxy1n, and zxymn are the x-y-partial
c   derivatives of the function at the four corners,
c   (x(1),y(1)), (x(m),y(1)), (x(1),y(n)), and (x(m),y(n)),
c   respectively. any of the parameters will be ignored (and
c   estimations supplied internally) if islpsw so indicates.
c
c   islpsw contains a switch indicating which boundary
c   derivative information is user-supplied and which
c   should be estimated by this subroutine. to determine
c   islpsw, let
c        i1 = 0 if zx1 is user-supplied (and = 1 otherwise),
c        i2 = 0 if zxm is user-supplied (and = 1 otherwise),
c        i3 = 0 if zy1 is user-supplied (and = 1 otherwise),
c        i4 = 0 if zyn is user-supplied (and = 1 otherwise),
c        i5 = 0 if zxy11 is user-supplied
c                                       (and = 1 otherwise),
c        i6 = 0 if zxym1 is user-supplied
c                                       (and = 1 otherwise),
c        i7 = 0 if zxy1n is user-supplied
c                                       (and = 1 otherwise),
c        i8 = 0 if zxymn is user-supplied
c                                       (and = 1 otherwise),
c   then islpsw = i1 + 2*i2 + 4*i3 + 8*i4 + 16*i5 + 32*i6
c                   + 64*i7 + 128*i8
c   thus islpsw = 0 indicates all derivative information is
c   user-supplied and islpsw = 255 indicates no derivative
c   information is user-supplied. any value between these
c   limits is valid.
c
c   zp is an array of at least 3*m*n locations.
c
c   temp is an array of at least n+n+m locations which is
c   used for scratch storage.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e. g. .001) the resulting surface is approximately the
c   tensor product of cubic splines. if abs(sigma) is large
c   (e. g. 50.) the resulting surface is approximately
c   bi-linear. if sigma equals zero tensor products of
c   cubic splines result. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   zp contains the values of the xx-, yy-, and xxyy-partial
c   derivatives of the surface at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2 or m is less than 2,
c        = 2 if the x-values or y-values are not strictly
c            increasing.
c
c and
c
c   m, n, x, y, z, iz, zx1, zxm, zy1, zyn, zxy11, zxym1,
c   zxy1n, zxymn, islpsw, and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      mm1 = m-1
      mp1 = m+1
      nm1 = n-1
      np1 = n+1
      npm = n+m
      ierr = 0
      if (n .le. 1 .or. m .le. 1) go to 46
      if (y(n) .le. y(1)) go to 47
c
c denormalize tension factor in y-direction
c
      sigmay = abs(sigma)*float(n-1)/(y(n)-y(1))
c
c obtain y-partial derivatives along y = y(1)
c
      if ((islpsw/8)*2 .ne. (islpsw/4)) go to 2
      do 1 i = 1,m
    1   zp(i,1,1) = zy1(i)
      go to 5
    2 dely1 = y(2)-y(1)
      dely2 = dely1+dely1
      if (n .gt. 2) dely2 = y(3)-y(1)
      if (dely1 .le. 0. .or. dely2 .le. dely1) go to 47
      call ceez (dely1,dely2,sigmay,c1,c2,c3,n)
      do 3 i = 1,m
    3   zp(i,1,1) = c1*z(i,1)+c2*z(i,2)
      if (n .eq. 2) go to 5
      do 4 i = 1,m
    4   zp(i,1,1) = zp(i,1,1)+c3*z(i,3)
c
c obtain y-partial derivatives along y = y(n)
c
    5 if ((islpsw/16)*2 .ne. (islpsw/8)) go to 7
      do 6 i = 1,m
        npi = n+i
    6   temp(npi) = zyn(i)
      go to 10
    7 delyn = y(n)-y(nm1)
      delynm = delyn+delyn
      if (n .gt. 2) delynm = y(n)-y(n-2)
      if (delyn .le. 0. .or. delynm .le. delyn) go to 47
      call ceez (-delyn,-delynm,sigmay,c1,c2,c3,n)
      do 8 i = 1,m
        npi = n+i
    8   temp(npi) = c1*z(i,n)+c2*z(i,nm1)
      if (n .eq. 2) go to 10
      do 9 i = 1,m
        npi = n+i
    9   temp(npi) = temp(npi)+c3*z(i,n-2)
   10 if (x(m) .le. x(1)) go to 47
c
c denormalize tension factor in x-direction
c
      sigmax = abs(sigma)*float(m-1)/(x(m)-x(1))
c
c obtain x-partial derivatives along x = x(1)
c
      if ((islpsw/2)*2 .ne. islpsw) go to 12
      do 11 j = 1,n
   11   zp(1,j,2) = zx1(j)
      if ((islpsw/32)*2 .eq. (islpsw/16) .and.
     *    (islpsw/128)*2  .eq. (islpsw/64)) go to 15
   12 delx1 = x(2)-x(1)
      delx2 = delx1+delx1
      if (m .gt. 2) delx2 = x(3)-x(1)
      if (delx1 .le. 0. .or. delx2 .le. delx1) go to 47
      call ceez (delx1,delx2,sigmax,c1,c2,c3,m)
      if ((islpsw/2)*2 .eq. islpsw) go to 15
      do 13 j = 1,n
   13   zp(1,j,2) = c1*z(1,j)+c2*z(2,j)
      if (m .eq. 2) go to 15
      do 14 j = 1,n
   14   zp(1,j,2) = zp(1,j,2)+c3*z(3,j)
c
c obtain x-y-partial derivative at (x(1),y(1))
c
   15 if ((islpsw/32)*2 .ne. (islpsw/16)) go to 16
      zp(1,1,3) = zxy11
      go to 17
   16 zp(1,1,3) = c1*zp(1,1,1)+c2*zp(2,1,1)
      if (m .gt. 2) zp(1,1,3) = zp(1,1,3)+c3*zp(3,1,1)
c
c obtain x-y-partial derivative at (x(1),y(n))
c
   17 if ((islpsw/128)*2 .ne. (islpsw/64)) go to 18
      zxy1ns = zxy1n
      go to 19
   18 zxy1ns = c1*temp(n+1)+c2*temp(n+2)
      if (m .gt. 2) zxy1ns = zxy1ns+c3*temp(n+3)
c
c obtain x-partial derivative along x = x(m)
c
   19 if ((islpsw/4)*2 .ne. (islpsw/2)) go to 21
      do 20 j = 1,n
        npmpj = npm+j
   20   temp(npmpj) = zxm(j)
      if ((islpsw/64)*2 .eq. (islpsw/32) .and.
     *    (islpsw/256)*2 .eq. (islpsw/128)) go to 24
   21 delxm = x(m)-x(mm1)
      delxmm = delxm+delxm
      if (m .gt. 2) delxmm = x(m)-x(m-2)
      if (delxm .le. 0. .or. delxmm .le. delxm) go to 47
      call ceez (-delxm,-delxmm,sigmax,c1,c2,c3,m)
      if ((islpsw/4)*2 .eq. (islpsw/2)) go to 24
      do 22 j = 1,n
        npmpj = npm+j
   22   temp(npmpj) = c1*z(m,j)+c2*z(mm1,j)
      if (m .eq. 2) go to 24
      do 23 j = 1,n
        npmpj = npm+j
   23   temp(npmpj) = temp(npmpj)+c3*z(m-2,j)
c
c obtain x-y-partial derivative at (x(m),y(1))
c
   24 if ((islpsw/64)*2 .ne. (islpsw/32)) go to 25
      zp(m,1,3) = zxym1
      go to 26
   25 zp(m,1,3) = c1*zp(m,1,1)+c2*zp(mm1,1,1)
      if (m .gt. 2) zp(m,1,3) = zp(m,1,3)+c3*zp(m-2,1,1)
c
c obtain x-y-partial derivative at (x(m),y(n))
c
   26 if ((islpsw/256)*2 .ne. (islpsw/128)) go to 27
      zxymns = zxymn
      go to 28
   27 zxymns = c1*temp(npm)+c2*temp(npm-1)
      if (m .gt. 2) zxymns = zxymns+c3*temp(npm-2)
c
c set up right hand sides and tridiagonal system for y-grid
c perform forward elimination
c
   28 del1 = y(2)-y(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 29 i = 1,m
   29   zp(i,2,1) = deli*(z(i,2)-z(i,1))
      zp(1,2,3) = deli*(zp(1,2,2)-zp(1,1,2))
      zp(m,2,3) = deli*(temp(npm+2)-temp(npm+1))
      call terms (diag1,sdiag1,sigmay,del1)
      diagi = 1./diag1
      do 30 i = 1,m
   30   zp(i,1,1) = diagi*(zp(i,2,1)-zp(i,1,1))
      zp(1,1,3) = diagi*(zp(1,2,3)-zp(1,1,3))
      zp(m,1,3) = diagi*(zp(m,2,3)-zp(m,1,3))
      temp(1) = diagi*sdiag1
      if (n .eq. 2) go to 34
      do 33 j = 2,nm1
        jm1 = j-1
        jp1 = j+1
        npmpj = npm+j
        del2 = y(jp1)-y(j)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 31 i = 1,m
   31     zp(i,jp1,1) = deli*(z(i,jp1)-z(i,j))
        zp(1,jp1,3) = deli*(zp(1,jp1,2)-zp(1,j,2))
        zp(m,jp1,3) = deli*(temp(npmpj+1)-temp(npmpj))
        call terms (diag2,sdiag2,sigmay,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(jm1))
        do 32 i = 1,m
   32     zp(i,j,1) = diagin*(zp(i,jp1,1)-zp(i,j,1)-
     *                        sdiag1*zp(i,jm1,1))
        zp(1,j,3) = diagin*(zp(1,jp1,3)-zp(1,j,3)-
     *                      sdiag1*zp(1,jm1,3))
        zp(m,j,3) = diagin*(zp(m,jp1,3)-zp(m,j,3)-
     *                      sdiag1*zp(m,jm1,3))
        temp(j) = diagin*sdiag2
        diag1 = diag2
   33   sdiag1 = sdiag2
   34 diagin = 1./(diag1-sdiag1*temp(nm1))
      do 35 i = 1,m
        npi = n+i
   35   zp(i,n,1) = diagin*(temp(npi)-zp(i,n,1)-
     *                      sdiag1*zp(i,nm1,1))
      zp(1,n,3) = diagin*(zxy1ns-zp(1,n,3)-
     *                    sdiag1*zp(1,nm1,3))
      temp(n) = diagin*(zxymns-zp(m,n,3)-
     *                  sdiag1*zp(m,nm1,3))
c
c perform back substitution
c
      do 37 j = 2,n
        jbak = np1-j
        jbakp1 = jbak+1
        t = temp(jbak)
        do 36 i = 1,m
   36     zp(i,jbak,1) = zp(i,jbak,1)-t*zp(i,jbakp1,1)
        zp(1,jbak,3) = zp(1,jbak,3)-t*zp(1,jbakp1,3)
   37   temp(jbak) = zp(m,jbak,3)-t*temp(jbakp1)
c
c set up right hand sides and tridiagonal system for x-grid
c perform forward elimination
c
      del1 = x(2)-x(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 38 j = 1,n
        zp(2,j,2) = deli*(z(2,j)-z(1,j))
   38   zp(2,j,3) = deli*(zp(2,j,1)-zp(1,j,1))
      call terms (diag1,sdiag1,sigmax,del1)
      diagi = 1./diag1
      do 39 j = 1,n
        zp(1,j,2) = diagi*(zp(2,j,2)-zp(1,j,2))
   39   zp(1,j,3) = diagi*(zp(2,j,3)-zp(1,j,3))
      temp(n+1) = diagi*sdiag1
      if (m  .eq. 2) go to 43
      do 42 i = 2,mm1
        im1 = i-1
        ip1 = i+1
        npi = n+i
        del2 = x(ip1)-x(i)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 40 j = 1,n
          zp(ip1,j,2) = deli*(z(ip1,j)-z(i,j))
   40     zp(ip1,j,3) = deli*(zp(ip1,j,1)-zp(i,j,1))
        call terms (diag2,sdiag2,sigmax,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(npi-1))
        do 41 j = 1,n
          zp(i,j,2) = diagin*(zp(ip1,j,2)-zp(i,j,2)-
     *                        sdiag1*zp(im1,j,2))
   41     zp(i,j,3) = diagin*(zp(ip1,j,3)-zp(i,j,3)-
     *                        sdiag1*zp(im1,j,3))
        temp(npi) = diagin*sdiag2
        diag1 = diag2
   42   sdiag1 = sdiag2
   43 diagin = 1./(diag1-sdiag1*temp(npm-1))
      do 44 j = 1,n
        npmpj = npm+j
        zp(m,j,2) = diagin*(temp(npmpj)-zp(m,j,2)-
     *                      sdiag1*zp(mm1,j,2))
   44   zp(m,j,3) = diagin*(temp(j)-zp(m,j,3)-
     *                      sdiag1*zp(mm1,j,3))
c
c perform back substitution
c
      do 45 i = 2,m
        ibak = mp1-i
        ibakp1 = ibak+1
        npibak = n+ibak
        t = temp(npibak)
        do 45 j = 1,n
          zp(ibak,j,2) = zp(ibak,j,2)-t*zp(ibakp1,j,2)
   45     zp(ibak,j,3) = zp(ibak,j,3)-t*zp(ibakp1,j,3)
      return
c
c too few points
c
   46 ierr = 1
      return
c
c points not strictly increasing
c
   47 ierr = 2
      return
      end
      function surf2 (xx,yy,m,n,x,y,z,iz,zp,sigma)
c
      integer m,n,iz
      real xx,yy,x(m),y(n),z(iz,n),zp(m,n,3),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function interpolates a surface at a given coordinate
c pair using a bi-spline under tension. the subroutine surf1
c should be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   xx and yy contain the x- and y-coordinates of the point
c   to be mapped onto the interpolating surface.
c
c   m and n contain the number of grid lines in the x- and
c   y-directions, respectively, of the rectangular grid
c   which specified the surface.
c
c   x and y are arrays containing the x- and y-grid values,
c   respectively, each in increasing order.
c
c   z is a matrix containing the m * n functional values
c   corresponding to the grid values (i. e. z(i,j) is the
c   surface value at the point (x(i),y(j)) for i = 1,...,m
c   and j = 1,...,n).
c
c   iz contains the row dimension of the array z as declared
c   in the calling program.
c
c   zp is an array of 3*m*n locations stored with the
c   various surface derivative information determined by
c   surf1.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters m, n, x, y, z, iz, zp, and sigma should be
c input unaltered from the output of surf1.
c
c on output--
c
c   surf2 contains the interpolated surface value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c inline one dimensional cubic spline interpolation
c
      hermz (f1,f2,fp1,fp2) = (f2*del1+f1*del2)/dels-del1*
     *                        del2*(fp2*(del1+dels)+
     *                              fp1*(del2+dels))/
     *                        (6.*dels)
c
c inline one dimensional spline under tension interpolation
c
      hermnz (f1,f2,fp1,fp2,sigmap) = (f2*del1+f1*del2)/dels
     *          +(fp2*del1*(sinhm1-sinhms)
     *           +fp1*del2*(sinhm2-sinhms)
     *          )/(sigmap*sigmap*dels*(1.+sinhms))
c
c denormalize tension factor in x and y direction
c
      sigmax = abs(sigma)*float(m-1)/(x(m)-x(1))
      sigmay = abs(sigma)*float(n-1)/(y(n)-y(1))
c
c determine y interval
c
      jm1 = intrvl (yy,y,n)
      j = jm1+1
c
c determine x interval
c
      im1 = intrvl (xx,x,m)
      i = im1+1
      del1 = yy-y(jm1)
      del2 = y(j)-yy
      dels = y(j)-y(jm1)
      if (sigmay .ne. 0.) go to 1
c
c perform four interpolations in y-direction
c
      zim1 = hermz(z(i-1,j-1),z(i-1,j),zp(i-1,j-1,1),
     *                                  zp(i-1,j,1))
      zi = hermz(z(i,j-1),z(i,j),zp(i,j-1,1),zp(i,j,1))
      zxxim1 = hermz(zp(i-1,j-1,2),zp(i-1,j,2),
     *                zp(i-1,j-1,3),zp(i-1,j,3))
      zxxi = hermz(zp(i,j-1,2),zp(i,j,2),
     *              zp(i,j-1,3),zp(i,j,3))
      go to 2
    1 call snhcsh (sinhm1,dummy,sigmay*del1,-1)
      call snhcsh (sinhm2,dummy,sigmay*del2,-1)
      call snhcsh (sinhms,dummy,sigmay*dels,-1)
      zim1 = hermnz(z(i-1,j-1),z(i-1,j),zp(i-1,j-1,1),
     *               zp(i-1,j,1),sigmay)
      zi = hermnz(z(i,j-1),z(i,j),zp(i,j-1,1),zp(i,j,1),
     *             sigmay)
      zxxim1 = hermnz(zp(i-1,j-1,2),zp(i-1,j,2),
     *                 zp(i-1,j-1,3),zp(i-1,j,3),sigmay)
      zxxi = hermnz(zp(i,j-1,2),zp(i,j,2),
     *               zp(i,j-1,3),zp(i,j,3),sigmay)
c
c perform final interpolation in x-direction
c
    2 del1 = xx-x(im1)
      del2 = x(i)-xx
      dels = x(i)-x(im1)
      if (sigmax .ne. 0.) go to 3
      surf2 = hermz(zim1,zi,zxxim1,zxxi)
      return
    3 call snhcsh (sinhm1,dummy,sigmax*del1,-1)
      call snhcsh (sinhm2,dummy,sigmax*del2,-1)
      call snhcsh (sinhms,dummy,sigmax*dels,-1)
      surf2 = hermnz(zim1,zi,zxxim1,zxxi,sigmax)
      return
      end
      subroutine ceez (del1,del2,sigma,c1,c2,c3,n)
c
      real del1,del2,sigma,c1,c2,c3
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the coefficients c1, c2, and c3
c used to determine endpoint slopes. specifically, if
c function values y1, y2, and y3 are given at points x1, x2,
c and x3, respectively, the quantity c1*y1 + c2*y2 + c3*y3
c is the value of the derivative at x1 of a spline under
c tension (with tension factor sigma) passing through the
c three points and having third derivative equal to zero at
c x1. optionally, only two values, c1 and c2 are determined.
c
c on input--
c
c   del1 is x2-x1 (.gt. 0.).
c
c   del2 is x3-x1 (.gt. 0.). if n .eq. 2, this parameter is
c   ignored.
c
c   sigma is the tension factor.
c
c and
c
c   n is a switch indicating the number of coefficients to
c   be returned. if n .eq. 2 only two coefficients are
c   returned. otherwise all three are returned.
c
c on output--
c
c   c1, c2, and c3 contain the coefficients.
c
c none of the input parameters are altered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (n .eq. 2) go to 2
      if (sigma .ne. 0.) go to 1
      del = del2-del1
c
c tension .eq. 0.
c
      c1 = -(del1+del2)/(del1*del2)
      c2 = del2/(del1*del)
      c3 = -del1/(del2*del)
      return
c
c tension .ne. 0.
c
    1 call snhcsh (dummy,coshm1,sigma*del1,1)
      call snhcsh (dummy,coshm2,sigma*del2,1)
      delp = sigma*(del2+del1)/2.
      delm = sigma*(del2-del1)/2.
      call snhcsh (sinhmp,dummy,delp,-1)
      call snhcsh (sinhmm,dummy,delm,-1)
      denom = coshm1*(del2-del1)-2.*del1*delp*delm*
     *        (1.+sinhmp)*(1.+sinhmm)
      c1 = 2.*delp*delm*(1.+sinhmp)*(1.+sinhmm)/denom
      c2 = -coshm2/denom
      c3 = coshm1/denom
      return
c
c two coefficients
c
    2 c1 = -1./del1
      c2 = -c1
      return
      end
      function intrvl (t,x,n)
c
      integer n
      real t,x(n)
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function determines the index of the interval
c (determined by a given increasing sequence) in which
c a given value lies.
c
c on input--
c
c   t is the given value.
c
c   x is a vector of strictly increasing values.
c
c and
c
c   n is the length of x (n .ge. 2).
c
c on output--
c
c   intrvl returns an integer i such that
c
c          i =  1       if         e   t .lt. x(2)  ,
c          i =  n-1     if x(n-1) .le. t            ,
c          otherwise       x(i)  .le. t .le. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      save i
      data i /1/
c
      tt = t
c
c check for illegal i
c
      if (i .ge. n) i = n/2
c
c check old interval and extremes
c
      if (tt .lt. x(i)) then
        if (tt .le. x(2)) then
          i = 1
          intrvl = 1
          return
        else
          il = 2
          ih = i
        end if
      else if (tt .le. x(i+1)) then
        intrvl = i
        return
      else if (tt .ge. x(n-1)) then
        i = n-1
        intrvl = n-1
        return
      else
        il = i+1
        ih = n-1
      end if
c
c binary search loop
c
    1 i = (il+ih)/2
      if (tt .lt. x(i)) then
         ih = i
      else if (tt .gt. x(i+1)) then
         il = i+1
      else
         intrvl = i
         return
      end if
      go to 1
      end
      function intrvp (t,x,n,p,tp)
c
      integer n
      real t,x(n),p,tp
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function determines the index of the interval
c (determined by a given increasing sequence) in which a
c given value lies, after translating the value to within
c the correct period.  it also returns this translated value.
c
c on input--
c
c   t is the given value.
c
c   x is a vector of strictly increasing values.
c
c   n is the length of x (n .ge. 2).
c
c and
c
c   p contains the period.
c
c on output--
c
c   tp contains a translated value of t (i. e. x(1) .le. tp,
c   tp .lt. x(1)+p, and tp = t + k*p for some integer k).
c
c   intrvl returns an integer i such that
c
c          i = 1       if             tp .lt. x(2)  ,
c          i = n       if   x(n) .le. tp            ,
c          otherwise       x(i)  .le. tp .lt. x(i+1),
c
c none of the input parameters are altered.
c
c-----------------------------------------------------------
c
      save i
      data i /1/
c
      nper = (t-x(1))/p
      tp = t-float(nper)*p
      if (tp .lt. x(1)) tp = tp+p
      tt = tp
c
c check for illegal i
c
      if (i .ge. n) i = n/2
c
c check old interval and extremes
c
      if (tt .lt. x(i)) then
        if (tt .le. x(2)) then
          i = 1
          intrvp = 1
          return
        else
          il = 2
          ih = i
        end if
      else if (tt .le. x(i+1)) then
        intrvp = i
        return
      else if (tt .ge. x(n)) then
        i = n
        intrvp = n
        return
      else
        il = i+1
        ih = n
      end if
c
c binary search loop
c
    1 i = (il+ih)/2
      if (tt .lt. x(i)) then
         ih = i
      else if (tt .gt. x(i+1)) then
         il = i+1
      else
         intrvp = i
         return
      end if
      go to 1
      end
c      subroutine snhcsh (sinhm,coshm,x,isw)
c
c      integer isw
c      real sinhm,coshm,x
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine returns approximations to
c       sinhm(x) = sinh(x)/x-1
c       coshm(x) = cosh(x)-1
c and
c       coshmm(x) = (cosh(x)-1-x*x/2)/(x*x)
c with relative error less than 1.0e-6
c
c on input--
c
c   x contains the value of the independent variable.
c
c   isw indicates the function desired
c           = -1 if only sinhm is desired,
c           =  0 if both sinhm and coshm are desired,
c           =  1 if only coshm is desired,
c           =  2 if only coshmm is desired,
c           =  3 if both sinhm and coshmm are desired.
c
c on output--
c
c   sinhm contains the value of sinhm(x) if isw .le. 0 or
c   isw .eq. 3 (sinhm is unaltered if isw .eq.1 or isw .eq.
c   2).
c
c   coshm contains the value of coshm(x) if isw .eq. 0 or
c   isw .eq. 1 and contains the value of coshmm(x) if isw
c   .ge. 2 (coshm is unaltered if isw .eq. -1).
c
c and
c
c   x and isw are unaltered.
c
c-----------------------------------------------------------
c
c      data sp13/.3029390e-5/,
c     *     sp12/.1975135e-3/,
c     *     sp11/.8334261e-2/,
c     *     sp10/.1666665e0/
c      data sp24/.3693467e-7/,
c     *     sp23/.2459974e-5/,
c     *     sp22/.2018107e-3/,
c     *     sp21/.8315072e-2/,
c     *     sp20/.1667035e0/
c      data sp33/.6666558e-5/,
c     *     sp32/.6646307e-3/,
c     *     sp31/.4001477e-1/,
c     *     sq32/.2037930e-3/,
c     *     sq31/-.6372739e-1/,
c     *     sq30/.6017497e1/
c      data sp43/.2311816e-4/,
c     *     sp42/.2729702e-3/,
c     *     sp41/.9868757e-1/,
c     *     sq42/.1776637e-3/,
c     *     sq41/-.7549779e-1/,
c     *     sq40/.9110034e1/
c      data cp4/.2982628e-6/,
c     *     cp3/.2472673e-4/,
c     *     cp2/.1388967e-2/,
c     *     cp1/.4166665e-1/,
c     *     cp0/.5000000e0/
cc
c      ax = abs(x)
c      if (isw .ge. 0) go to 5
cc
cc sinhm approximation
cc
c      if (ax .gt. 4.45) go to 2
c      xs = ax*ax
c      if (ax .gt. 2.3) go to 1
cc
cc sinhm approximation on (0.,2.3)
cc
c      sinhm = xs*(((sp13*xs+sp12)*xs+sp11)*xs+sp10)
c      return
cc
cc sinhm approximation on (2.3,4.45)
cc
c    1 sinhm = xs*((((sp24*xs+sp23)*xs+sp22)*xs+sp21)
c     .               *xs+sp20)
c      return
c    2 if (ax .gt. 7.65) go to 3
cc
cc sinhm approximation on (4.45,7.65)
cc
c      xs = ax*ax
c      sinhm = xs*(((sp33*xs+sp32)*xs+sp31)*xs+1.)/
c     .             ((sq32*xs+sq31)*xs+sq30)
c      return
c    3 if (ax .gt. 10.1) go to 4
cc
cc sinhm approximation on (7.65,10.1)
cc
c      xs = ax*ax
c      sinhm = xs*(((sp43*xs+sp42)*xs+sp41)*xs+1.)/
c     .             ((sq42*xs+sq41)*xs+sq40)
c      return
cc
cc sinhm approximation above 10.1
cc
c    4 sinhm = exp(ax)/(ax+ax)-1.
c      return
cc
cc coshm and (possibly) sinhm approximation
cc
c    5 if (isw .ge. 2) go to 7
c      if (ax .gt. 2.3) go to 6
c      xs = ax*ax
c      coshm = xs*((((cp4*xs+cp3)*xs+cp2)*xs+cp1)*xs+cp0)
c      if (isw .eq. 0) sinhm = xs*(((sp13*xs+sp12)*xs+sp11)
c     .                              *xs+sp10)
c      return
c    6 expx = exp(ax)
c      coshm = (expx+1./expx)/2.-1.
c      if (isw .eq. 0) sinhm = (expx-1./expx)/(ax+ax)-1.
c      return
cc
cc coshmm and (possibly) sinhm approximation
cc
c    7 xs = ax*ax
c      if (ax .gt. 2.3) go to 8
c      coshm = xs*(((cp4*xs+cp3)*xs+cp2)*xs+cp1)
c      if (isw .eq. 3) sinhm = xs*(((sp13*xs+sp12)*xs+sp11)
c     .                              *xs+sp10)
c      return
c    8 expx = exp(ax)
c      coshm = ((expx+1./expx-xs)/2.-1.)/xs
c      if (isw .eq. 3) sinhm = (expx-1./expx)/(ax+ax)-1.
c      return
c      end
      subroutine snhcsh (sinhm,coshm,x,isw)
c
      integer isw
      real sinhm,coshm,x
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine returns approximations to
c       sinhm(x) = sinh(x)/x-1
c       coshm(x) = cosh(x)-1
c and
c       coshmm(x) = (cosh(x)-1-x*x/2)/(x*x)
c with relative error less than 4.0e-14.
c
c on input--
c
c   x contains the value of the independent variable.
c
c   isw indicates the function desired
c           = -1 if only sinhm is desired,
c           =  0 if both sinhm and coshm are desired,
c           =  1 if only coshm is desired,
c           =  2 if only coshmm is desired,
c           =  3 if both sinhm and coshmm are desired.
c
c on output--
c
c   sinhm contains the value of sinhm(x) if isw .le. 0 or
c   isw .eq. 3 (sinhm is unaltered if isw .eq.1 or isw .eq.
c   2).
c
c   coshm contains the value of coshm(x) if isw .eq. 0 or
c   isw .eq. 1 and contains the value of coshmm(x) if isw
c   .ge. 2 (coshm is unaltered if isw .eq. -1).
c
c and
c
c   x and isw are unaltered.
c
c-----------------------------------------------------------
c
      data sp14/.227581660976348e-7/,
     *     sp13/.612189863171694e-5/,
     *     sp12/.715314759211209e-3/,
     *     sp11/.398088289992973e-1/,
     *     sq12/.206382701413725e-3/,
     *     sq11/-.611470260009508e-1/,
     *     sq10/.599999999999986e+1/
      data sp25/.129094158037272e-9/,
     *     sp24/.473731823101666e-7/,
     *     sp23/.849213455598455e-5/,
     *     sp22/.833264803327242e-3/,
     *     sp21/.425024142813226e-1/,
     *     sq22/.106008515744821e-3/,
     *     sq21/-.449855169512505e-1/,
     *     sq20/.600000000268619e+1/
      data sp35/.155193945864942e-9/,
     *     sp34/.511529451668737e-7/,
     *     sp33/.884775635776784e-5/,
     *     sp32/.850447617691392e-3/,
     *     sp31/.428888148791777e-1/,
     *     sq32/.933128831061610e-4/,
     *     sq31/-.426677570538507e-1/,
     *     sq30/.600000145086489e+1/
      data sp45/.188070632058331e-9/,
     *     sp44/.545792817714192e-7/,
     *     sp43/.920119535795222e-5/,
     *     sp42/.866559391672985e-3/,
     *     sp41/.432535234960858e-1/,
     *     sq42/.824891748820670e-4/,
     *     sq41/-.404938841672262e-1/,
     *     sq40/.600005006283834e+1/
      data cp5/.552200614584744e-9/,
     *     cp4/.181666923620944e-6/,
     *     cp3/.270540125846525e-4/,
     *     cp2/.206270719503934e-2/,
     *     cp1/.744437205569040e-1/,
     *     cq2/.514609638642689e-4/,
     *     cq1/-.177792255528382e-1/,
     *     cq0/.200000000000000e+1/
      data zp4/.664418805876835e-8/,
     *     zp3/.218274535686385e-5/,
     *     zp2/.324851059327161e-3/,
     *     zp1/.244515150174258e-1/,
     *     zq2/.616165782306621e-3/,
     *     zq1/-.213163639579425e0/,
     *     zq0/.240000000000000e+2/
c
      ax = abs(x)
      if (isw .ge. 0) go to 5
c
c sinhm approximation
c
      if (ax .gt. 3.9) go to 2
      xs = ax*ax
      if (ax .gt. 2.2) go to 1
c
c sinhm approximation on (0.,2.2)
c
      sinhm = xs*((((sp14*xs+sp13)*xs+sp12)*xs+sp11)*xs+1.)/
     .             ((sq12*xs+sq11)*xs+sq10)
      return
c
c sinhm approximation on (2.2,3.9)
c
    1 sinhm = xs*(((((sp25*xs+sp24)*xs+sp23)*xs+sp22)*xs+sp21)
     .        *xs+1.)/((sq22*xs+sq21)*xs+sq20)
      return
    2 if (ax .gt. 5.1) go to 3
c
c sinhm approximation on (3.9,5.1)
c
      xs = ax*ax
      sinhm = xs*(((((sp35*xs+sp34)*xs+sp33)*xs+sp32)*xs+sp31)
     .        *xs+1.)/((sq32*xs+sq31)*xs+sq30)
      return
    3 if (ax .gt. 6.1) go to 4
c
c sinhm approximation on (5.1,6.1)
c
      xs = ax*ax
      sinhm = xs*(((((sp45*xs+sp44)*xs+sp43)*xs+sp42)*xs+sp41)
     .        *xs+1.)/((sq42*xs+sq41)*xs+sq40)
      return
c
c sinhm approximation above 6.1
c
    4 expx = exp(ax)
      sinhm = (expx-1./expx)/(ax+ax)-1.
      return
c
c coshm and (possibly) sinhm approximation
c
    5 if (isw .ge. 2) go to 7
      if (ax .gt. 2.2) go to 6
      xs = ax*ax
      coshm = xs*(((((cp5*xs+cp4)*xs+cp3)*xs+cp2)*xs+cp1)
     .        *xs+1.)/((cq2*xs+cq1)*xs+cq0)
      if (isw .eq. 0) sinhm = xs*((((sp14*xs+sp13)*xs+sp12)
     .          *xs+sp11)*xs+1.)/((sq12*xs+sq11)*xs+sq10)
      return
    6 expx = exp(ax)
      coshm = (expx+1./expx)/2.-1.
      if (isw .eq. 0) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
c
c coshmm and (possibly) sinhm approximation
c
    7 xs = ax*ax
      if (ax .gt. 2.2) go to 8
      coshm = xs*((((zp4*xs+zp3)*xs+zp2)*xs+zp1)*xs+1.)/
     .             ((zq2*xs+zq1)*xs+zq0)
      if (isw .eq. 3) sinhm = xs*((((sp14*xs+sp13)*xs+sp12)
     .          *xs+sp11)*xs+1.)/((sq12*xs+sq11)*xs+sq10)
      return
    8 expx = exp(ax)
      coshm = ((expx+1./expx-xs)/2.-1.)/xs
      if (isw .eq. 3) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
      end
      subroutine terms (diag,sdiag,sigma,del)
c
      real diag,sdiag,sigma,del
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine computes the diagonal and superdiagonal
c terms of the tridiagonal linear system associated with
c spline under tension interpolation.
c
c on input--
c
c   sigma contains the tension factor.
c
c and
c
c   del contains the step size.
c
c on output--
c
c                sigma*del*cosh(sigma*del) - sinh(sigma*del)
c   diag = del*--------------------------------------------.
c                     (sigma*del)**2 * sinh(sigma*del)
c
c                   sinh(sigma*del) - sigma*del
c   sdiag = del*----------------------------------.
c                (sigma*del)**2 * sinh(sigma*del)
c
c and
c
c   sigma and del are unaltered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (sigma .ne. 0.) go to 1
      diag = del/3.
      sdiag = del/6.
      return
    1 sigdel = sigma*del
      call snhcsh (sinhm,coshm,sigdel,0)
      denom = sigma*sigdel*(1.+sinhm)
      diag = (coshm-sinhm)/denom
      sdiag = sinhm/denom
      return
      end

