C NCLFORTSTART
      subroutine cfftfdriver (nmx,xr,xi,acoef,bcoef,work,nw)
      implicit none
c                                      ! INPUT       
      integer  nmx,nw
      double precision xr(nmx),xi(nmx),work(nw)
c                                      ! OUTPUT       
      double precision acoef(nmx), bcoef(nmx) 
c NCLEND
c
c NCL:  coef = cfftf( xr, xi, opt)     ! coef(2,...)
c                                      ! LOCAL        
      integer  n
c     double precision work(4*nmx+25)  
      double complex   carr(nmx)       

      call cffti(nmx,work)               
c                                      ! create complex input
      do n=1,nmx
         carr(n) = cmplx( xr(n), xi(n) )     
      end do

      call cfftf(nmx, carr, work)

      do n=1,nmx
         acoef(n) = dble ( carr(n) ) 
         bcoef(n) = dimag( carr(n) )
      end do

      return
      end

C NCLFORTSTART
      subroutine cfftbdriver (nmx,xr,xi,acoef,bcoef,work,nw)
      implicit none
c                                      ! INPUT       
      integer  nmx,nw
      double precision acoef(nmx), bcoef(nmx), work(nw)
c                                      ! OUTPUT       
      double precision xr(nmx), xi(nmx)
c NCLEND
c
c NCL:  coef = cfftb( acoef, bcoef, opt )
c                                      ! LOCAL        
      integer  n
c      double precision work(4*nmx+25)
      double complex   carr(nmx)       

      call cffti(nmx,work)               
c                                      ! create complex input
      do n=1,nmx
         carr(n) = cmplx( acoef(n), bcoef(n))     
      end do

      call cfftb(nmx, carr, work)
c                                      ! reconstruct ... normalize 
      do n=1,nmx
         xr(n) = dble ( carr(n) ) / nmx
         xi(n) = dimag( carr(n) ) / nmx
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

C NCLFORTSTART
      subroutine cfftffrqreorder (npts,frqi,cfai,cfbi,frqo,cfao,cfbo)
      implicit none
c
c SPECIAL to reorder frequencies to range from -0.5 to +0.5
c .   Generally, for graphics reasons only. 
c                                                  ! input
      integer npts
      double precision frqi(npts), cfai(npts), cfbi(npts) 
c                                                  ! output
      double precision frqo(npts), cfao(npts), cfbo(npts)
C NCLEND
c
c NCL: cf_reorder = cfftf_frq_reorder( cf )
c                                                  ! local
      integer n, n2

      n2  = npts/2
c                             for clarity use two differnt sections
      if (mod(npts,2).eq.0) then
c                                                  ! even         
          do n=1,n2
             frqo(n) = frqi(n+n2)
             cfao(n) = cfai(n+n2)
             cfbo(n) = cfbi(n+n2)
          end do

          if (frqo(1).gt.0.0d0) frqo(1) = -frqo(1) 
    
          do n=1,n2
             frqo(n2+n) = frqi(n)
             cfao(n2+n) = cfai(n)
             cfbo(n2+n) = cfbi(n)
          end do
      else
c                                                  ! odd          
          do n=1,n2
             frqo(n)    = frqi(n+n2+1)
             cfao(n)    = cfai(n+n2+1)
             cfbo(n)    = cfbi(n+n2+1)
          end do

          do n=1,n2+1
             frqo(n2+n) = frqi(n)
             cfao(n2+n) = cfai(n)
             cfbo(n2+n) = cfbi(n)
          end do
      end if

      return
      end
