C
C $Id: kurvd.f,v 1.5 2008-07-27 03:10:11 haley Exp $
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
