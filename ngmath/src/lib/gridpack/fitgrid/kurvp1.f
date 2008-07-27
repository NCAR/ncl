C
C $Id: kurvp1.f,v 1.5 2008-07-27 03:10:11 haley Exp $
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
