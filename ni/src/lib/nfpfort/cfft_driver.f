C NCLFORTSTART
      subroutine cfftfdriver (nmx,x,acoef,bcoef)
      implicit none
c                                      ! INPUT       
      integer  nmx
      double precision x(nmx)
c                                      ! OUTPUT       
      double precision acoef(nmx), bcoef(nmx) 
c NCLEND
c
c NCL:  coef = cfftf( x )
c                                      ! LOCAL        
      integer  n
      double precision work(4*nmx+25)
      double complex   carr(nmx)       

      call cffti(nmx,work)               
c                                      ! create complex input
      do n=1,nmx
         carr(n) = cmplx( x(n), 0.0d0 )     
      end do

      call cfftf(nmx, carr, work)

      do n=1,nmx
         acoef(n) = dble ( carr(n) ) 
         bcoef(n) = dimag( carr(n) )
      end do

      return
      end

C NCLFORTSTART
      subroutine cfftbdriver (nmx,x,acoef,bcoef)
      implicit none
c                                      ! INPUT       
      integer  nmx
      double precision acoef(nmx), bcoef(nmx) 
c                                      ! OUTPUT       
      double precision x(nmx)
c NCLEND
c
c NCL:  coef = cfftb( acoef, bcoef )
c                                      ! LOCAL        
      integer  n
      double precision work(4*nmx+25)
      double complex   carr(nmx)       

      call cffti(nmx,work)               
c                                      ! create complex input
      do n=1,nmx
         carr(n) = cmplx( acoef(n), bcoef(n))     
      end do

      call cfftb(nmx, carr, work)
c                                      ! reconstruct 
      do n=1,nmx
         x(n) = dble( carr(n) ) / nmx
      end do

      return
      end
