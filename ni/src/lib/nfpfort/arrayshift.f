C NCLFORTSTART
      subroutine arrayshift(x, nrow, ncol, kmode)
      implicit none
      integer  nrow, ncol, kmode
      double precision x(nrow,ncol)
c NCLEND           

c local (dynamic): may want to allocate in interface and pass as argument
      double precision tmp(nrow,ncol)

c inline functions
      integer i, j, iwrap, jwrap
      iwrap(i) = mod(i-1+nrow/2,nrow)+1
      jwrap(j) = mod(j-1+ncol/2,ncol)+1
      
      if (kmode.eq.0) then
          do j=1,ncol
            do i=1,nrow
               tmp(iwrap(i),jwrap(j)) = x(i,j)
            end do
          end do
      end if
      
      if (kmode.lt.0) then
          do j=1,ncol
            do i=1,nrow
               tmp(i,jwrap(j)) = x(i,j)
            end do
          end do
      end if
      
      if (kmode.gt.0) then
          do j=1,ncol
            do i=1,nrow
               tmp(iwrap(i),j) = x(i,j)
            end do
          end do
      end if
      
      do j=1,ncol
        do i=1,nrow
           x(i,j) = tmp(i,j)
        end do
      end do

      return
      end
