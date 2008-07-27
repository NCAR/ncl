C
C $Id: kurvp2.f,v 1.5 2008-07-27 03:10:12 haley Exp $
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
