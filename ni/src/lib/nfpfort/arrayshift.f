C NCLFORTSTART
      subroutine arrayshift(x, nrow, ncol, kmode, tmp)
      implicit none
c                                        ! INPUT
      integer  nrow, ncol, kmode
      double precision x(nrow,ncol)
c                                        ! OUTPUT
      double precision tmp(nrow,ncol)
c NCLEND           
c ---------------------------------------
c inline functions
      integer i, j, iwrap, jwrap
      iwrap(i) = mod(i-1+nrow/2,nrow)+1
      jwrap(j) = mod(j-1+ncol/2,ncol)+1
c ---------------------------------------    
 
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
      
      return
      end

C --------------------------------------------------------
c kmode is ignored. it is just a place holder for consistenvy
C --------------------------------------------------------

C NCLFORTSTART
      subroutine arrayshift1(x, n, kmode, tmp)
      implicit none
c                                        ! INPUT
      integer  n, kmode
      double precision x(n)
c                                        ! OUTPUT
      double precision tmp(n)
c NCLEND           

c inline functions
      integer i, iwrap
      iwrap(i) = mod(i-1+n/2,n)+1
      
      do i=1,n
         tmp(iwrap(i)) = x(i)
      end do
      
      return
      end

