C NCLFORTSTART
      subroutine derrf (x, result)
      implicit none
      double precision x, result
C NCLEND
c ncl result = erf (x)
      double precision erf
c c c external erf
      result = erf(x)
      return
      end

C NCLFORTSTART
      subroutine derrcf (ind, x, result)
      implicit none
      integer  ind
      double precision x, result
C NCLEND
c ncl result = erfc (ind, x)
      double precision erfc1
c c c external errc1
      result = erfc1(ind,x)
      return
      end
