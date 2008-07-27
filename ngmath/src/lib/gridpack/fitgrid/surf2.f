C
C $Id: surf2.f,v 1.5 2008-07-27 03:10:12 haley Exp $
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
