C
C $Id: curvpi.f,v 1.5 2008-07-27 03:10:10 haley Exp $
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
