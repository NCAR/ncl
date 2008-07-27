C
C $Id: curvd.f,v 1.5 2008-07-27 03:10:10 haley Exp $
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
c using a spline under tension. the subroutine curv1 should
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
c unaltered from the output of curv1.
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
