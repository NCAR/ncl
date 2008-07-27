C
C $Id: kurv1.f,v 1.5 2008-07-27 03:10:11 haley Exp $
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
