      subroutine vors (nv, nf, v, a, b, c, nd)
      implicit none              ! arguments
      integer  nv, nf, nd
      real     v(nd,nf), a(nv), b(nv), c(nv)

      integer  i, j, kr, m, n      ! local 
      real     t, pi, twopi, reps, aa, bb, cc, dd
     &       , xn, xd, y, cy, sy, temp 

      real     sumf, scpf

c +++ very old fortran 66 ++++ used arithmetic "if"
c         I have made some minor changes made
c
c ***** This gives the same answers as IMSL's FROTA *****
c *****      with w=1.0, norm=1, maxit=30           *****
c
c nv    - # of variables [rows]
c nf    - # of factors   [columns]
c v     - matrix to be rotated 
c a     - % variation of rotated factors
c b     - communalities of variables [%]
c c     - work vector
c nd    - lead dimension [# rows] of v in the main program
c         
c Orthogonal Rotation according to varimax  criteria.
c Kaiser row normalization is used prior to rotation.
c ie: On input, each row is normalized by dividing each row
c     by the square root of the sum of its squared elements. 
c     Before returning each row is denormalized.

c Each possible pair of factor vectors [principal components]
c    is rotated to maximize the column-variance criterion
c    in turn until a complete pass through all combinations
c    does not result in any rotations of more than "reps" radians.    
c The percentages of trace are recomputed for each factor vector
c    and returned in vector "a".
c The percentages of trace are computed for each row of the
c    rotated matrix and returned in vector "b". When  computed
c    as proportions (row sums of squares of the loading matrix)
c    they are called "communalities" and are not affected by the 
c    rotation process. This will be exactly 100% only when all
c    the variance of the particular variable is completely
c    accounted for by the extracted factors.
c
c D.J. Veldman
c Fortran Programming for the Behavioral Sciences
c Holt, Rinehart, Winston, 1967
c pp 214-215
 
      t = nv
      pi = 4.*atan(1.)
      twopi = 2.*pi
c c c reps = pi/180.                           ! epsilon (radians)=1 deg
      reps = 0.0001                            ! this matches IMSL
 
      do i = 1, nv                             ! normalize the rows of v
         b(i) = sqrt(sumf(v,(-i),(-nf),nd))
         do j = 1, nf
            v(i,j) = v(i,j)/b(i)
         end do
      end do
 
   10 continue
      kr = 0
      do m = 1, nf
         do n = m, nf
            if (m .ne. n) then
                              ! compute angle of rotation
               do i = 1, nv
                  a(i) = v(i,m)*v(i,m) - v(i,n)*v(i,n)
                  c(i) = 2.0*v(i,m)*v(i,n)
               end do
 
               aa = sumf(a,1,nv,nd)
               bb = sumf(c,1,nv,nd)
               cc = sumf(a,1,(-nv),nd) - sumf(c,1,(-nv),nd)
               dd = scpf(a,c,1,1,nv,nd)*2.0
               xn = dd - 2.0*aa*bb/t
               xd = cc - (aa*aa - bb*bb)/t
                               ! compute angle of rotation
               y = atan(xn/xd)
               if (xd .lt. 0.) then
                  if (xn .ge. 0.) y = y + twopi
                  y = y - pi
               endif
               y = y/4.0
               if (abs(y) .lt. reps) go to 1002

               kr = kr + 1     ! count rotations
               cy = cos(y)
               sy = sin(y)
               do i = 1, nv    ! rotate the axis
                  temp = v(i,m)*cy + v(i,n)*sy
                  v(i,n) = v(i,n)*cy - v(i,m)*sy
                  v(i,m) = temp
               end do
            endif
 1002       continue
         end do
      end do
      if (kr .gt. 0) go to 10
                               ! denormailize rows of v
      do j = 1, nf
         do i = 1, nv
            v(i,j) = v(i,j)*b(i)
         end do
         a(j) = (sumf(v,j,(-nv),nd)/t)*100.      ! % variation
      end do
 
      do i = 1, nv
         b(i) = b(i)*b(i)*100.                   ! % communitalities
      end do
 
      return 
      end 
c ------------------------------------
      real function sumf (x, kk, nn, nd)
      implicit none
      integer  kk, nn, nd
      real     x(nd,1)
 
c         awkward due to arithmetic if in orig code
c computes sum of x or x**2 from a vector
c x   = input array
c nn  = number of values to be summed.
c       if (nn.lt.0) sum of x**2 is computed
c kk  = row or column number if x is a matrix.
c       kk=1 means x is a vector
c       kk>1 means it is a column vector
c       kk<0 means it is a   row  vector
c nd  = actual first dimension of x in the
c       aclling routine

      integer  n, k, i   ! local
 
      sumf = 0.0
      n = iabs(nn)
      k = iabs(kk)
      if (nn .le. 0) then
         if (nn .eq. 0) go to 55
         if (kk .lt. 0) go to 15
         if (kk .eq. 0) go to 55
         go to 25
      endif
      if (kk .lt. 0) go to 35
      if (kk .eq. 0) go to 55
      go to 45
   15 continue
      do i = 1, n
         sumf = sumf + x(k,i)*x(k,i)
      end do
      return 
 
   25 continue
      do i = 1, n
         sumf = sumf + x(i,k)*x(i,k)
      end do
      return 
 
   35 continue
      do i = 1, n
         sumf = sumf + x(k,i)
      end do
      return 
 
   45 continue
      do i = 1, n
         sumf = sumf + x(i,k)
      end do
   55 continue
      return 
 
      end 
c ------------------------------------------------
      real function scpf (x, y, kx, ky, n, nd)
      implicit none
      integer  kx, ky, n, nd
      real     x(nd,1), y(nd,1)
 
      integer  j, k, i     ! local 
 
c         awkward due to arithmetic if in orig code
c computes the sum of cross products (scalar product) of two vectors
c x,y - input arrays [may be the same]
c kx,ky - row  or column numbers for x and y (if matrices)
c         set=1 if vector
c         if kx or ky is positive and not 1, it is a column vector
c         if kx or ky is negative and not 1, it is a   row  vector
c n     - the number of products to be summed, elements of each vector
c nd    - actual first dimension of x in the calling routine
 
      j = iabs(kx)
      k = iabs(ky)
      scpf = 0.0
 
      if (kx .le. 0) then
         if (kx .eq. 0) go to 55
         if (ky .lt. 0) go to 15
         if (ky .eq. 0) go to 55
         go to 25
      endif
      if (ky .lt. 0) go to 35
      if (ky .eq. 0) go to 55
      go to 45
   15 continue
      do i = 1, n
         scpf = scpf + x(j,i)*y(k,i)
      end do
      return 
 
   25 continue
      do i = 1, n
         scpf = scpf + x(j,i)*y(i,k)
      end do
      return 
 
   35 continue
      do i = 1, n
         scpf = scpf + x(i,j)*y(k,i)
      end do
      return 
 
   45 continue
      do i = 1, n
         scpf = scpf + x(i,j)*y(i,k)
      end do
      return 
 
   55 continue
      return 
      end 
