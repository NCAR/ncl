      subroutine smth9 (x,wrk,ni,nj,p,q,xmsg,lwrap,ier)
 
      logical lwrap
      integer ni, nj, ier
      real    x(ni,nj), wrk(ni,nj), p, q, xmsg
c***********************************************************************
c modified j. olson routine
c***********************************************************************
c
c this routine does 9-point smoothing using the equation:
c
c   f0 = f0 + (p/4)*(f2+f4+f6+f8-4*f0) + (q/4)*(f1+f3+f5+f7-4*f0)
c
c  where the grid is:
c
c      1-------------8---------------7
c      |             |               |
c      |             |               |
c      |             |               |
c      |             |               |
c      2-------------0---------------6
c      |             |               |
c      |             |               |
c      |             |               |
c      |             |               |
c      3-------------4---------------5
c
c   arguments :
c .
c .   x        - 2-d input/output array
c .   wrk      - 2-d work array
c .   ni       - dimension
c .   nj       - dimension
c .   p        - first  weight (suggested value of  0.50)
c .   q        - second weight (suggested value of  0.25)
c .   xmsg     - value of missing points
c .   lwrap    - logical flag to include wraparound points in smoothing
c .              if lwrap = .true. , smooth left and right endpoints
c .              if lwrap = .false., no smoothing
c .
c  notes:
c
c       1)  if a point or any of its neighbors is missing, the point is
c           not smoothed
c       2)  this routine does not smooth the edges, just the interior
c           with the exception that the left and right edges are smoothed
c           when "lwrap" is true.
c       3)  array has to have at least 3 points in each direction
c       4)  smoothed results are returned via the original input array
c
c***********************************************************************
 
c determine if size of array is sufficient
 
      ier      = 0
      if (ni .lt. 3 .or. nj .lt. 3) then
          ier = 1
          write (*,'(/,'' sub smth9: error'',2i5)') ni,nj
          return
      endif
 
      po4      = p/4.
      qo4      = q/4.
 
c set the wrk array to msg
 
      do i=1,ni
         do j=1,nj
            wrk(i,j) = xmsg
         enddo
      enddo
 
c are endpoints to be smoothed?
 
      if(lwrap) then
         nib      = 1
         nie      = ni
      else
         nib      = 2
         nie      = ni-1
      endif
      njb      = 2
      nje      = nj-1
 
c  smooth
 
      do j= njb,nje
        do i= nib,nie
           jm1    = j-1
           jp1    = j+1
           im1    = i-1
           ip1    = i+1
           if (im1 .lt.  1) im1 = ni
           if (ip1 .gt. ni) ip1 =  1
 
           if (x(  i,  j) .eq. xmsg .or. x(im1,jp1) .eq. xmsg .or.
     *         x(im1,  j) .eq. xmsg .or. x(im1,jm1) .eq. xmsg .or.
     *         x(  i,jm1) .eq. xmsg .or. x(ip1,jm1) .eq. xmsg .or.
     *         x(ip1,  j) .eq. xmsg .or. x(ip1,jp1) .eq. xmsg .or.
     *         x(  i,jp1) .eq. xmsg                      ) then
               wrk(i,j) = x(i,j)      ! original (unsmoothed) value
           else
               term1 = po4*(x(im1,  j)+x(  i,jm1)+x(ip1,  j)+x(  i,jp1)-
     *                                                    4.*x(  i,  j))
               term2 = qo4*(x(im1,jp1)+x(im1,jm1)+x(ip1,jm1)+x(ip1,jp1)-
     *                                                   4.*x(  i,  j))
               wrk(i,j) = x(i,j)+term1+term2
           endif
        enddo
      enddo
 
c transfer back to original array
 
      do j= njb,nje
        do i= nib,nie
           x(i,j) = wrk(i,j)
        enddo
      enddo
 
      return
      end
