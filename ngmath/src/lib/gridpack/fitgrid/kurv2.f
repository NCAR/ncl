C
C $Id: kurv2.f,v 1.5 2008-07-27 03:10:11 haley Exp $
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
