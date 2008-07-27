C
C $Id: surf1.f,v 1.5 2008-07-27 03:10:12 haley Exp $
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
      subroutine surf1 (m,n,x,y,z,iz,zx1,zxm,zy1,zyn,zxy11,
     *                  zxym1,zxy1n,zxymn,islpsw,zp,temp,
     *                  sigma,ierr)
c
      integer m,n,iz,islpsw,ierr
      real x(m),y(n),z(iz,n),zx1(n),zxm(n),zy1(m),zyn(m),
     *     zxy11,zxym1,zxy1n,zxymn,zp(m,n,3),temp(1),sigma
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute an interpolatory surface passing through a rect-
c angular grid of functional values. the surface determined
c can be represented as the tensor product of splines under
c tension. the x- and y-partial derivatives around the
c boundary and the x-y-partial derivatives at the four
c corners may be specified or omitted. for actual mapping
c of points onto the surface it is necessary to call the
c function surf2.
c
c on input--
c
c   m is the number of grid lines in the x-direction, i. e.
c   lines parallel to the y-axis (m .ge. 2).
c
c   n is the number of grid lines in the y-direction, i. e.
c   lines parallel to the x-axis (n .ge. 2).
c
c   x is an array of the m x-coordinates of the grid lines
c   in the x-direction. these should be strictly increasing.
c
c   y is an array of the n y-coordinates of the grid lines
c   in the y-direction. these should be strictly increasing.
c
c   z is an array of the m * n functional values at the grid
c   points, i. e. z(i,j) contains the functional value at
c   (x(i),y(j)) for i = 1,...,m and j = 1,...,n.
c
c   iz is the row dimension of the matrix z used in the
c   calling program (iz .ge. m).
c
c   zx1 and zxm are arrays of the m x-partial derivatives
c   of the function along the x(1) and x(m) grid lines,
c   respectively. thus zx1(j) and zxm(j) contain the x-part-
c   ial derivatives at the points (x(1),y(j)) and
c   (x(m),y(j)), respectively, for j = 1,...,n. either of
c   these parameters will be ignored (and approximations
c   supplied internally) if islpsw so indicates.
c
c   zy1 and zyn are arrays of the n y-partial derivatives
c   of the function along the y(1) and y(n) grid lines,
c   respectively. thus zy1(i) and zyn(i) contain the y-part-
c   ial derivatives at the points (x(i),y(1)) and
c   (x(i),y(n)), respectively, for i = 1,...,m. either of
c   these parameters will be ignored (and estimations
c   supplied internally) if islpsw so indicates.
c
c   zxy11, zxym1, zxy1n, and zxymn are the x-y-partial
c   derivatives of the function at the four corners,
c   (x(1),y(1)), (x(m),y(1)), (x(1),y(n)), and (x(m),y(n)),
c   respectively. any of the parameters will be ignored (and
c   estimations supplied internally) if islpsw so indicates.
c
c   islpsw contains a switch indicating which boundary
c   derivative information is user-supplied and which
c   should be estimated by this subroutine. to determine
c   islpsw, let
c        i1 = 0 if zx1 is user-supplied (and = 1 otherwise),
c        i2 = 0 if zxm is user-supplied (and = 1 otherwise),
c        i3 = 0 if zy1 is user-supplied (and = 1 otherwise),
c        i4 = 0 if zyn is user-supplied (and = 1 otherwise),
c        i5 = 0 if zxy11 is user-supplied
c                                       (and = 1 otherwise),
c        i6 = 0 if zxym1 is user-supplied
c                                       (and = 1 otherwise),
c        i7 = 0 if zxy1n is user-supplied
c                                       (and = 1 otherwise),
c        i8 = 0 if zxymn is user-supplied
c                                       (and = 1 otherwise),
c   then islpsw = i1 + 2*i2 + 4*i3 + 8*i4 + 16*i5 + 32*i6
c                   + 64*i7 + 128*i8
c   thus islpsw = 0 indicates all derivative information is
c   user-supplied and islpsw = 255 indicates no derivative
c   information is user-supplied. any value between these
c   limits is valid.
c
c   zp is an array of at least 3*m*n locations.
c
c   temp is an array of at least n+n+m locations which is
c   used for scratch storage.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e. g. .001) the resulting surface is approximately the
c   tensor product of cubic splines. if abs(sigma) is large
c   (e. g. 50.) the resulting surface is approximately
c   bi-linear. if sigma equals zero tensor products of
c   cubic splines result. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   zp contains the values of the xx-, yy-, and xxyy-partial
c   derivatives of the surface at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2 or m is less than 2,
c        = 2 if the x-values or y-values are not strictly
c            increasing.
c
c and
c
c   m, n, x, y, z, iz, zx1, zxm, zy1, zyn, zxy11, zxym1,
c   zxy1n, zxymn, islpsw, and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      mm1 = m-1
      mp1 = m+1
      nm1 = n-1
      np1 = n+1
      npm = n+m
      ierr = 0
      if (n .le. 1 .or. m .le. 1) go to 46
      if (y(n) .le. y(1)) go to 47
c
c denormalize tension factor in y-direction
c
      sigmay = abs(sigma)*float(n-1)/(y(n)-y(1))
c
c obtain y-partial derivatives along y = y(1)
c
      if ((islpsw/8)*2 .ne. (islpsw/4)) go to 2
      do 1 i = 1,m
    1   zp(i,1,1) = zy1(i)
      go to 5
    2 dely1 = y(2)-y(1)
      dely2 = dely1+dely1
      if (n .gt. 2) dely2 = y(3)-y(1)
      if (dely1 .le. 0. .or. dely2 .le. dely1) go to 47
      call ceez (dely1,dely2,sigmay,c1,c2,c3,n)
      do 3 i = 1,m
    3   zp(i,1,1) = c1*z(i,1)+c2*z(i,2)
      if (n .eq. 2) go to 5
      do 4 i = 1,m
    4   zp(i,1,1) = zp(i,1,1)+c3*z(i,3)
c
c obtain y-partial derivatives along y = y(n)
c
    5 if ((islpsw/16)*2 .ne. (islpsw/8)) go to 7
      do 6 i = 1,m
        npi = n+i
    6   temp(npi) = zyn(i)
      go to 10
    7 delyn = y(n)-y(nm1)
      delynm = delyn+delyn
      if (n .gt. 2) delynm = y(n)-y(n-2)
      if (delyn .le. 0. .or. delynm .le. delyn) go to 47
      call ceez (-delyn,-delynm,sigmay,c1,c2,c3,n)
      do 8 i = 1,m
        npi = n+i
    8   temp(npi) = c1*z(i,n)+c2*z(i,nm1)
      if (n .eq. 2) go to 10
      do 9 i = 1,m
        npi = n+i
    9   temp(npi) = temp(npi)+c3*z(i,n-2)
   10 if (x(m) .le. x(1)) go to 47
c
c denormalize tension factor in x-direction
c
      sigmax = abs(sigma)*float(m-1)/(x(m)-x(1))
c
c obtain x-partial derivatives along x = x(1)
c
      if ((islpsw/2)*2 .ne. islpsw) go to 12
      do 11 j = 1,n
   11   zp(1,j,2) = zx1(j)
      if ((islpsw/32)*2 .eq. (islpsw/16) .and.
     *    (islpsw/128)*2  .eq. (islpsw/64)) go to 15
   12 delx1 = x(2)-x(1)
      delx2 = delx1+delx1
      if (m .gt. 2) delx2 = x(3)-x(1)
      if (delx1 .le. 0. .or. delx2 .le. delx1) go to 47
      call ceez (delx1,delx2,sigmax,c1,c2,c3,m)
      if ((islpsw/2)*2 .eq. islpsw) go to 15
      do 13 j = 1,n
   13   zp(1,j,2) = c1*z(1,j)+c2*z(2,j)
      if (m .eq. 2) go to 15
      do 14 j = 1,n
   14   zp(1,j,2) = zp(1,j,2)+c3*z(3,j)
c
c obtain x-y-partial derivative at (x(1),y(1))
c
   15 if ((islpsw/32)*2 .ne. (islpsw/16)) go to 16
      zp(1,1,3) = zxy11
      go to 17
   16 zp(1,1,3) = c1*zp(1,1,1)+c2*zp(2,1,1)
      if (m .gt. 2) zp(1,1,3) = zp(1,1,3)+c3*zp(3,1,1)
c
c obtain x-y-partial derivative at (x(1),y(n))
c
   17 if ((islpsw/128)*2 .ne. (islpsw/64)) go to 18
      zxy1ns = zxy1n
      go to 19
   18 zxy1ns = c1*temp(n+1)+c2*temp(n+2)
      if (m .gt. 2) zxy1ns = zxy1ns+c3*temp(n+3)
c
c obtain x-partial derivative along x = x(m)
c
   19 if ((islpsw/4)*2 .ne. (islpsw/2)) go to 21
      do 20 j = 1,n
        npmpj = npm+j
   20   temp(npmpj) = zxm(j)
      if ((islpsw/64)*2 .eq. (islpsw/32) .and.
     *    (islpsw/256)*2 .eq. (islpsw/128)) go to 24
   21 delxm = x(m)-x(mm1)
      delxmm = delxm+delxm
      if (m .gt. 2) delxmm = x(m)-x(m-2)
      if (delxm .le. 0. .or. delxmm .le. delxm) go to 47
      call ceez (-delxm,-delxmm,sigmax,c1,c2,c3,m)
      if ((islpsw/4)*2 .eq. (islpsw/2)) go to 24
      do 22 j = 1,n
        npmpj = npm+j
   22   temp(npmpj) = c1*z(m,j)+c2*z(mm1,j)
      if (m .eq. 2) go to 24
      do 23 j = 1,n
        npmpj = npm+j
   23   temp(npmpj) = temp(npmpj)+c3*z(m-2,j)
c
c obtain x-y-partial derivative at (x(m),y(1))
c
   24 if ((islpsw/64)*2 .ne. (islpsw/32)) go to 25
      zp(m,1,3) = zxym1
      go to 26
   25 zp(m,1,3) = c1*zp(m,1,1)+c2*zp(mm1,1,1)
      if (m .gt. 2) zp(m,1,3) = zp(m,1,3)+c3*zp(m-2,1,1)
c
c obtain x-y-partial derivative at (x(m),y(n))
c
   26 if ((islpsw/256)*2 .ne. (islpsw/128)) go to 27
      zxymns = zxymn
      go to 28
   27 zxymns = c1*temp(npm)+c2*temp(npm-1)
      if (m .gt. 2) zxymns = zxymns+c3*temp(npm-2)
c
c set up right hand sides and tridiagonal system for y-grid
c perform forward elimination
c
   28 del1 = y(2)-y(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 29 i = 1,m
   29   zp(i,2,1) = deli*(z(i,2)-z(i,1))
      zp(1,2,3) = deli*(zp(1,2,2)-zp(1,1,2))
      zp(m,2,3) = deli*(temp(npm+2)-temp(npm+1))
      call terms (diag1,sdiag1,sigmay,del1)
      diagi = 1./diag1
      do 30 i = 1,m
   30   zp(i,1,1) = diagi*(zp(i,2,1)-zp(i,1,1))
      zp(1,1,3) = diagi*(zp(1,2,3)-zp(1,1,3))
      zp(m,1,3) = diagi*(zp(m,2,3)-zp(m,1,3))
      temp(1) = diagi*sdiag1
      if (n .eq. 2) go to 34
      do 33 j = 2,nm1
        jm1 = j-1
        jp1 = j+1
        npmpj = npm+j
        del2 = y(jp1)-y(j)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 31 i = 1,m
   31     zp(i,jp1,1) = deli*(z(i,jp1)-z(i,j))
        zp(1,jp1,3) = deli*(zp(1,jp1,2)-zp(1,j,2))
        zp(m,jp1,3) = deli*(temp(npmpj+1)-temp(npmpj))
        call terms (diag2,sdiag2,sigmay,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(jm1))
        do 32 i = 1,m
   32     zp(i,j,1) = diagin*(zp(i,jp1,1)-zp(i,j,1)-
     *                        sdiag1*zp(i,jm1,1))
        zp(1,j,3) = diagin*(zp(1,jp1,3)-zp(1,j,3)-
     *                      sdiag1*zp(1,jm1,3))
        zp(m,j,3) = diagin*(zp(m,jp1,3)-zp(m,j,3)-
     *                      sdiag1*zp(m,jm1,3))
        temp(j) = diagin*sdiag2
        diag1 = diag2
   33   sdiag1 = sdiag2
   34 diagin = 1./(diag1-sdiag1*temp(nm1))
      do 35 i = 1,m
        npi = n+i
   35   zp(i,n,1) = diagin*(temp(npi)-zp(i,n,1)-
     *                      sdiag1*zp(i,nm1,1))
      zp(1,n,3) = diagin*(zxy1ns-zp(1,n,3)-
     *                    sdiag1*zp(1,nm1,3))
      temp(n) = diagin*(zxymns-zp(m,n,3)-
     *                  sdiag1*zp(m,nm1,3))
c
c perform back substitution
c
      do 37 j = 2,n
        jbak = np1-j
        jbakp1 = jbak+1
        t = temp(jbak)
        do 36 i = 1,m
   36     zp(i,jbak,1) = zp(i,jbak,1)-t*zp(i,jbakp1,1)
        zp(1,jbak,3) = zp(1,jbak,3)-t*zp(1,jbakp1,3)
   37   temp(jbak) = zp(m,jbak,3)-t*temp(jbakp1)
c
c set up right hand sides and tridiagonal system for x-grid
c perform forward elimination
c
      del1 = x(2)-x(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 38 j = 1,n
        zp(2,j,2) = deli*(z(2,j)-z(1,j))
   38   zp(2,j,3) = deli*(zp(2,j,1)-zp(1,j,1))
      call terms (diag1,sdiag1,sigmax,del1)
      diagi = 1./diag1
      do 39 j = 1,n
        zp(1,j,2) = diagi*(zp(2,j,2)-zp(1,j,2))
   39   zp(1,j,3) = diagi*(zp(2,j,3)-zp(1,j,3))
      temp(n+1) = diagi*sdiag1
      if (m  .eq. 2) go to 43
      do 42 i = 2,mm1
        im1 = i-1
        ip1 = i+1
        npi = n+i
        del2 = x(ip1)-x(i)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 40 j = 1,n
          zp(ip1,j,2) = deli*(z(ip1,j)-z(i,j))
   40     zp(ip1,j,3) = deli*(zp(ip1,j,1)-zp(i,j,1))
        call terms (diag2,sdiag2,sigmax,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(npi-1))
        do 41 j = 1,n
          zp(i,j,2) = diagin*(zp(ip1,j,2)-zp(i,j,2)-
     *                        sdiag1*zp(im1,j,2))
   41     zp(i,j,3) = diagin*(zp(ip1,j,3)-zp(i,j,3)-
     *                        sdiag1*zp(im1,j,3))
        temp(npi) = diagin*sdiag2
        diag1 = diag2
   42   sdiag1 = sdiag2
   43 diagin = 1./(diag1-sdiag1*temp(npm-1))
      do 44 j = 1,n
        npmpj = npm+j
        zp(m,j,2) = diagin*(temp(npmpj)-zp(m,j,2)-
     *                      sdiag1*zp(mm1,j,2))
   44   zp(m,j,3) = diagin*(temp(j)-zp(m,j,3)-
     *                      sdiag1*zp(mm1,j,3))
c
c perform back substitution
c
      do 45 i = 2,m
        ibak = mp1-i
        ibakp1 = ibak+1
        npibak = n+ibak
        t = temp(npibak)
        do 45 j = 1,n
          zp(ibak,j,2) = zp(ibak,j,2)-t*zp(ibakp1,j,2)
   45     zp(ibak,j,3) = zp(ibak,j,3)-t*zp(ibakp1,j,3)
      return
c
c too few points
c
   46 ierr = 1
      return
c
c points not strictly increasing
c
   47 ierr = 2
      return
      end
