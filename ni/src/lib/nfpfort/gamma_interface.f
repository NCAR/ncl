C NCLFORTSTART
      subroutine gammacomplete (nx, xin, xout, has_msg, xmsg)
      implicit none
c                              INPUT
      integer  nx, has_msg
      double precision xin(nx), xmsg
c                              OUTPUT
      double precision xout(nx)
C NCLEND
C NCL: gma = gamma( x )
c WRAPIT -L $NCARG_ROOT/lib -l nfpfort gamma_interface.f 
c                              LOCAL
      integer n
      double precision dgammaslatec

      if(has_msg.eq.0) then
        do n=1,nx
          xout(n) = dgammaslatec ( xin(n) )
        end do
      else
        do n=1,nx
           if(xin(n) .ne. xmsg) then
              xout(n) = dgammaslatec ( xin(n) )
           else
              xout(n) = xmsg
           end if
        end do
      endif

      return
      end 
