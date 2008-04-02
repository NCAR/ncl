C NCLFORTSTART
      subroutine frqcfft (npts,fmsg,frq)
      implicit none
c                                                  ! input
      integer npts
      double precision fmsg
c                                                  ! output
      double precision frq(npts)
C NCLEND
c                                                  ! local
      integer n
      double precision df

      df     = 1.0d0/(npts-2)
      frq(1) = fmsg
      do n=2,npts
         frq(n) = -0.5d0 + (n-2)*df
      end do

      return
      end
