C
C $Id: curvi.f,v 1.5 2008-07-27 03:10:10 haley Exp $
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
c curv1 should be called earlier to determine necessary
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
c   yp is an array from subroutine curv1 containing
c   the values of the second derivatives at the nodes.
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
