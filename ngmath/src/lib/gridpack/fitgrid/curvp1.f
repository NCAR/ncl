C
C $Id: curvp1.f,v 1.5 2008-07-27 03:10:10 haley Exp $
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
