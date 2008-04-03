C NCLFORTSTART
      subroutine cfftfdriver (nmx,x,acoef,bcoef,work,nwrk)
      implicit none
c                                      ! INPUT       
      integer  nmx
      double precision x(nmx)
      double precision work(nwrk)      ! 4*nmx+25
c                                      ! OUTPUT       
      double precision acoef(nmx), bcoef(nmx) 
c NCLEND
c
c NCL:  coef = cfftf( x )              ! coef(2,...)
c                                      ! LOCAL        
      integer  n
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
      subroutine cfftbdriver (nmx,x,acoef,bcoef,work,nwrk)
      implicit none
c                                      ! INPUT       
      integer  nmx
      double precision acoef(nmx), bcoef(nmx) 
c                                      ! OUTPUT       
      double precision x(nmx)
      double precision work(nwrk)      ! 4*nmx+25
c NCLEND
c
c NCL:  coef = cfftb( acoef, bcoef )
c                                      ! LOCAL        
      integer  n
      double complex   carr(nmx)       

      call cffti(nmx,work)               
c                                      ! create complex input
      do n=1,nmx
         carr(n) = cmplx( acoef(n), bcoef(n))     
      end do

      call cfftb(nmx, carr, work)
c                                      ! reconstruct ... normalize 
      do n=1,nmx
         x(n) = dble( carr(n) ) / nmx
      end do

      return
      end

C NCLFORTSTART
      subroutine frqcfft (npts,frq)
      implicit none
c
c SPECIAL to generate frequencies for frq attribute: cfftf
c .   Need to allow for a back transform so no order change
c                                                  ! input
      integer npts
c                                                  ! output
      double precision frq(npts)
C NCLEND
c                                                  ! local
      integer n
      double precision df
c                                      frequency interval
      df  =  1.0d0/npts
c                                      generate frequencies
      do n=1,npts
         frq(n) = (n-1)*df
         if (frq(n).gt.0.5d0) frq(n) = frq(n) - 1.0d0
      end do

      return
      end
