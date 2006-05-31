c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
      subroutine ihgeod(m,idp,jdp,x,y,z)
      dimension x(idp,jdp,5),y(idp,jdp,5),z(idp,jdp,5)
c
c     m         is the number of points on the edge of a 
c               single geodesic triangle
c
c     x,y,z     the coordinates of the geodesic points on 
c               the sphere are x(i,j,k), y(i,j,k), z(i,j,k)
c               where i=1,...,m+m-1; j=1,...,m; and k=1,...,5.
c               the indices are defined on the unfolded 
c               icosahedron as follows for the case m=3
c
c                north pole
c
c                 (5,1)          0      l
c        i     (4,1) (5,2)              a    (repeated for
c           (3,1) (4,2) (5,3)  theta1   t    k=2,3,4,5 in
c        (2,1) (3,2) (4,3)              i        -->
c     (1,1) (2,2) (3,3)        theta2   t    the longitudinal 
c        (1,2) (2,3)                    u    direction)
c           (1,3)                pi     d
c      j                                e
c         south pole
c
c                total number of points is 10*(m-1)**2+2
c                total number of triangles is 20*(m-1)**2
c                total number of edges is 30*(m-1)**2
c
      pi = 4.*atan(1.)
      dphi = .4*pi
      beta = cos(dphi)
      theta1 = acos(beta/(1.-beta))
      theta2 = pi-theta1
      hdphi = dphi/2.
      tdphi = 3.*hdphi
      do k=1,5
	phi = (k-1)*dphi
	call stoc(1.,theta2,phi,x1,y1,z1)
	call stoc(1.,pi,phi+hdphi,x2,y2,z2)
	call stoc(1.,theta2,phi+dphi,x3,y3,z3)
	dxi = (x2-x1)/(m-1)
	dyi = (y2-y1)/(m-1)
	dzi = (z2-z1)/(m-1)
	dxj = (x3-x2)/(m-1)
	dyj = (y3-y2)/(m-1)
	dzj = (z3-z2)/(m-1)
	do i=1,m
	  xs = x1 + (i-1)*dxi
	  ys = y1 + (i-1)*dyi
	  zs = z1 + (i-1)*dzi
	  do j=1,i
	    x(j,i,k) = xs + (j-1)*dxj
	    y(j,i,k) = ys + (j-1)*dyj
	    z(j,i,k) = zs + (j-1)*dzj
	  end do
	end do
	call stoc(1.,theta1,phi+hdphi,x4,y4,z4)
	dxi = (x3-x4)/(m-1)
	dyi = (y3-y4)/(m-1)
	dzi = (z3-z4)/(m-1)
	dxj = (x4-x1)/(m-1)
	dyj = (y4-y1)/(m-1)
	dzj = (z4-z1)/(m-1)
	do j=1,m
	  xs = x1 + (j-1)*dxj
	  ys = y1 + (j-1)*dyj
	  zs = z1 + (j-1)*dzj
	  do i=1,j
	    x(j,i,k) = xs + (i-1)*dxi
	    y(j,i,k) = ys + (i-1)*dyi
	    z(j,i,k) = zs + (i-1)*dzi
	  end do
	end do
	call stoc(1.,theta1,phi+tdphi,x5,y5,z5)
	dxj = (x5-x3)/(m-1)
	dyj = (y5-y3)/(m-1)
	dzj = (z5-z3)/(m-1)
	do i=1,m
	  xs = x4 + (i-1)*dxi
	  ys = y4 + (i-1)*dyi
	  zs = z4 + (i-1)*dzi
	  do j=1,i
	    x(j+m-1,i,k) = xs + (j-1)*dxj
	    y(j+m-1,i,k) = ys + (j-1)*dyj
	    z(j+m-1,i,k) = zs + (j-1)*dzj
	  end do
	end do
	call stoc(1.,0.,phi+dphi,x6,y6,z6)
	dxi = (x5-x6)/(m-1)
	dyi = (y5-y6)/(m-1)
	dzi = (z5-z6)/(m-1)
	dxj = (x6-x4)/(m-1)
	dyj = (y6-y4)/(m-1)
	dzj = (z6-z4)/(m-1)
	do j=1,m
	  xs = x4 + (j-1)*dxj
	  ys = y4 + (j-1)*dyj
	  zs = z4 + (j-1)*dzj
	  do i=1,j
	    x(j+m-1,i,k) = xs + (i-1)*dxi
	    y(j+m-1,i,k) = ys + (i-1)*dyi
	    z(j+m-1,i,k) = zs + (i-1)*dzi
	  end do
	end do
      end do
      do k=1,5
	do j=1,m+m-1
	  do i=1,m
	    call ctos(x(j,i,k),y(j,i,k),z(j,i,k),rad,theta,phi)
	    call stoc(1.,theta,phi,x(j,i,k),y(j,i,k),z(j,i,k))
	  end do
	end do
      end do
      return
      end
      subroutine ctos(x,y,z,r,theta,phi)
      r1 = x*x+y*y
      if(r1 .ne. 0.) go to 10
      phi = 0.
      theta = 0.
      if(z .lt. 0.) theta = 4.*atan(1.)
      return
   10 r = sqrt(r1+z*z)
      r1 = sqrt(r1) 
      phi = atan2(y,x)
      theta = atan2(r1,z)
      return
      end
      subroutine stoc(r,theta,phi,x,y,z)
      st = sin(theta)
      x = r*st*cos(phi)
      y = r*st*sin(phi)
      z = r*cos(theta)
      return
      end
