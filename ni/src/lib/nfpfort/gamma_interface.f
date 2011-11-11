C NCLFORTSTART
      subroutine gammacomplete (nx, xin, xout)
      implicit none
c                              INPUT
      integer  nx
      double precision xin(nx)
c                              OUTPUT
      double precision xout(nx)
C NCLEND
C NCL: gma = gamma( x )
c WRAPIT -L $NCARG_ROOT/lib -l nfpfort gamma_interface.f 
c                              LOCAL
      integer n
      double precision dgammaslatec

      do n=1,nx
         xout(n) = dgammaslatec ( xin(n) )
      end do

      return
      end 
