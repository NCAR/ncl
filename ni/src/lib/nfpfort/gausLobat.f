c***********************************************************************
c Find the Gauss-Lobatto collocation points xGL(i) and the
c corresponding weights.
c***********************************************************************
C NCL:  gau = gausLobat (npts)
C NCLFORTSTART
      subroutine   GausLobat(xGL,weight,npts)
      implicit     none
c                             INPUT
      integer      npts
c                             OUTPUT [return like NCL 'gaus']
      double precision weight(npts), xGL(npts)
C NCLEND
      integer      n
c                            return G-L wgts and 'lat'
      call   GaussLobatto(xGL,weight,npts)
c                            switch sign: make -1 to +1 [symmetric]
      if (xGL(1).gt.xGL(2)) then
          do n=1,npts
             xGL(n) = -xGL(n)
          end do
      end if
c                            make range [-90,90]
      do n=1,npts
         xGL(n) = xGL(n)*90.d0
      end do

      return
      end
c *********************************************************************
c ....Find the Gauss-Lobatto weights associated with the GL latitudes
c ....Requested by Erik Kluzek
c *********************************************************************
C NCL:  gauLat = gausLobatWgt (xGLAT)
C NCLFORTSTART
      subroutine findglw (xGLAT, weight, npts)
      implicit   none
c                             INPUT  [xGLAT are [+90 to -90]
      integer    npts
      double precision     xGLAT(npts) 
c                             OUTPUT [1D array: double]
      double precision     weight(npts)
C NCLEND
      integer    N, m
      double precision     dt, func0, func1, func2, temp

      N  = npts-1
      dt = 2.d0/(N*(N+1.d0))

      do m = 1,N+1
        temp = xGLAT(m)/90.d0
        call jacobf(func0,func1,func2,temp,N)
        weight(m) = dt / (func0**2)
c c c   write(6,"('FINDGLW:',i4,4f12.5)") m,func0,xGLAT(m),weight(m)
      enddo

      return
      end
c *************** COMPUTATIONAL ROUTINES **********************
      subroutine   GaussLobatto(xGL,weight,npts)
      implicit     none
      integer      npts
      double precision       weight(npts), xGL(npts)

      integer      N, nh, nr, m
      double precision       pi,dt, xh,xl, func0,func1,func2
      data         pi/3.1415926535897932384d0/

      N = npts-1

c.....Find 1st half of the roots by direct solution

      nh = N/2
      dt = pi/(2.d0*N+1.d0)

      do m = 1,nh
        xh = cos( (2.d0*m-1.d0)*dt)
        xl = cos( 2.d0*m*dt)
        call NewtonRaphson1(xl,xh,weight(m),N)
      enddo

      nr = N-2*nh

      if (nr.ne.0) then
c.......N is odd
        weight(nh+1) = 0.d0
      endif

c.....Find 2nd half of the roots by symmetry

      do m = nh+nr+1,N
        weight(m) =-weight(N-m+1)
      enddo

c.....Find 1st half of the roots by direct solution

      xGL(1) = 1.d0
      nh = (N-1)/2

      do m = 1,nh
        xh = weight(m)
        xl = weight(m+1)
        call NewtonRaphson2(xl,xh,xGL(m+1),N)
      enddo

      nr = N-1-2*nh

      if (nr.ne.0) then
c.......N is odd
        xGL(nh+2) = 0.d0
      endif

c.....Find 2nd half of the roots by symmetry

      do m = nh+nr+2,N+1
        xGL(m) =-xGL(N+2-m)
      enddo

c.....Find the Gauss-Lobatto weights

      N  = npts-1
      dt = 2.d0/(N*(N+1.d0))

      do m = 1,N+1
        call jacobf(func0,func1,func2,xGL(m),N)
        weight(m) = dt / (func0**2)
      enddo

      return
      end

c**********************************************************************
c Combination of Newton-Raphson iterations and Bisection method.
c xl and xh are the initial bracketing of the root xm.
c**********************************************************************

      subroutine   NewtonRaphson1(xl,xh,xm,N)
      implicit     none

      integer      k,ITERMAX,N
      double precision  xl,xh,xm,xn, func0,func1,func2, error, TOLERANCE
      parameter   (ITERMAX=100, TOLERANCE=1.d-15)

c.....Find function values at bracketing points

      call jacobf(xm,func1,func2,xl,N)
      call jacobf(xn,func1,func2,xh,N)

      if (xm*xn.gt.0.d0) then
         write(6,*)' root is not bracketed'
         return
      endif

c.....Rearrange xl and xh so that f(xl)< 0.d0 < f(xh)

      if (xm.gt.0.d0) then
         xm = xl
         xl = xh
         xh = xm
      endif

c.....Initial width of bracketed interval

      error = abs(xh-xl)

c.....Initialize guess for root

      xm = 0.5d0*(xl+xh)

      k = 0

      do while (error.gt.TOLERANCE .and. k.lt.ITERMAX)
         k = k+1
         call jacobf(func0,func1,func2,xm,N)

c........Adjust the bracketing of the root
         if (func0.gt.0.d0) then
            xh = xm
         else
            xl = xm
         endif

c........Tentative newton-Raphson step

         xn = xm - func0/func1

         if ((xn-xl)*(xn-xh).gt.0.d0) then
c...........Bisection step
            xm = 0.5d0* (xh+xl)
            error = abs(xh-xl)
         else
c...........Newton-Raphson step successfull
            error = abs(xn-xm)
            xm = xn
         endif
      enddo

c.....Check if solution converged

      if (error.gt.TOLERANCE) then
         write(6,*) 'solution did not converge'
      endif

      return
      end

c**********************************************************************
c Combination of Newton-Raphson iterations and Bisection method.
c xl and xh are the initial bracketing of the root xm.
c**********************************************************************

      subroutine   NewtonRaphson2(xl,xh,xm,N)
      implicit     none

      integer      k,ITERMAX,N
      double precision xl,xh,xm,xn, func0,func1,func2, error, TOLERANCE
      parameter   (ITERMAX=100, TOLERANCE=1.d-15)

c.....Find function values at bracketing points

      call jacobf(func0,xm,func2,xl,N)
      call jacobf(func0,xn,func2,xh,N)

      if (xm*xn.gt.0.d0) then
         write(6,*)' root is not bracketed'
         return
      endif

c.....Rearrange xl and xh so that f(xl)< 0.d0 < f(xh)

      if (xm.gt.0.d0) then
         xm = xl
         xl = xh
         xh = xm
      endif

c.....Initial width of bracketed interval

      error = abs(xh-xl)

c.....Initialize guess for root

      xm = 0.5d0*(xl+xh)

      k = 0

      do while (error.gt.TOLERANCE .and. k.lt.ITERMAX)
         k = k+1
         call jacobf(func0,func1,func2,xm,N)

c........Adjust the bracketing of the root
         if (func1.gt.0.d0) then
            xh = xm
         else
            xl = xm
         endif

c........Tentative newton-Raphson step

         xn = xm - func1/func2

         if ((xn-xl)*(xn-xh).gt.0.d0) then
c...........Bisection step
            xm = 0.5d0* (xh+xl)
            error = abs(xh-xl)
         else
c...........Newton-Raphson step successfull
            error = abs(xn-xm)
            xm = xn
         endif
      enddo

c.....Check if solution converged

      if (error.gt.TOLERANCE) then
         write(6,*) 'solution did not converge'
      endif

      return
      end

c**********************************************************************
c Compute the Jacobi polynomials and their derivatives using
c the recurrence relationship.
c p0_3 = Legendre polynomial of degree N
c p1_3 = 1st derivative of Legendre polynomial of degree N at x
c p2_3 = 2nd derivative of Legendre polynomial of degree N at x
c**********************************************************************

      subroutine   jacobf(p0_3,p1_3,p2_3,x,N)
      implicit     none

      integer      N, k
      double precision p0_1, p0_2, p0_3, p1_1, p1_2
     *               , p1_3, p2_1, p2_2, p2_3, a1, a2, x

      p0_3 = 1.d0
      p1_3 = 0.d0
      p2_3 = 0.d0

      if (N.ne.0) then
        p0_2 = p0_3
        p1_2 = p1_3
        p2_2 = p2_3
        p0_3 = x
        p1_3 = 1.d0
        p2_3 = 0.d0

        do k = 2,N
          p0_1 = p0_2
          p1_1 = p1_2
          p2_1 = p2_2
          p0_2 = p0_3
          p1_2 = p1_3
          p2_2 = p2_3

          a1 = (2.d0*k-1.d0) / k
          a2 = (k-1.d0) / k

          p0_3 = a1*x*p0_2 - a2*p0_1
          p1_3 = a1*x*p1_2 - a2*p1_1 + a1*p0_2
          p2_3 = a1*x*p2_2 - a2*p2_1 + 2.d0*a1*p1_2
        enddo
      endif

      return
      end

c**********************************************************************
c Compute the Jacobi polynomials using c the recurrence relationship.
c p_3 = Legendre polynomial of degree N
c p_2 = Legendre polynomial of degree N-1 at x
c p_1 = Legendre polynomial of degree N-2 at x
c**********************************************************************

      function     Legendre(x,N)
      implicit     none

      integer      N, k
      double precision p_1, p_2, p_3, x, Legendre

      p_3 = 1.d0

      if (N.ne.0) then
        p_2 = p_3
        p_3 = x
        do k = 2,N
          p_1 = p_2
          p_2 = p_3
          p_3 = ( (2.d0*k-1.d0)*x*p_2 - (k-1.d0)*p_1 ) / k
        enddo
      endif

      Legendre = p_3

      return
      end

c***********************************************************************
c Compute the matrix giving the first derivative to the Cardinal 
c Legendre functions. To compute the first derivative just form the
c matrix product [dcpdxv]{u}={u_ksi}. Reapply previous formula for 
c higher order derivative.

c npts:number of collocation points 1<=i<=npts
c dcpdxv(i,j): derivative of pressure cardinal function i evaluated at 
c 		velocity node j
c***********************************************************************

      subroutine   CoefPgOnVg(dcpdxv,xvg,xpg,legep,npts,nptp)
      implicit     none

      integer      npts,nptp,N,i,j
      double precision  
     *             Legendre,fact,f1,f2,func0,func1,func2,dis,
     *             xvg(npts),xpg(nptp),legep(nptp),dcpdxv(nptp,npts)

      N = nptp-1

      do i = 1,nptp
        legep(i) = Legendre(xpg(i),N)
      enddo

      fact = N*(N+1)

      do j = 1,npts
        call jacobf(func0,func1,func2,xvg(j),N)
        f1 = fact*func0
        f2 = (1.d0-xvg(j)**2) * func1

        do i = 1,nptp
          if (xvg(j).ne.xpg(i)) then
            dis = xvg(j) - xpg(i)
            dcpdxv(i,j) = (f1*dis+f2) / (fact*legep(i)*dis*dis)
          else
            dcpdxv(i,j) = 0.d0
          endif
        enddo
      enddo

      dcpdxv(nptp,npts) =-0.25d0*fact
      dcpdxv(1,1) = 0.25d0*fact

      return
      end
