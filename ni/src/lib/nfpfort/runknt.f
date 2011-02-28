      subroutine numrun2(nx, ntim, xin, optknt, xknt )
      implicit none
C
C knt = dim_numrun_n( x, optknt, n)
C                                                      INPUT
      integer  nx, ntim, optknt, optend
      integer  xin(nx,ntim) 
C                                                      OUTPUT
      integer  xknt(nx,ntim) 
C                                                      LOCAL 
      integer  n, j, t, knt, yin(0:nx+1)

      if (optknt.eq.0) then
          do t=1,ntim
            do n=1,nx
               xknt(n,t) = 0
              do j=1,nx-n+1
                 if (all(xin(j:j+n-1,t).eq.1)) then
                     xknt(n,t) = xknt(n,t)+1
                 end if
              end do
            end do
          end do

          return
      end if

c count unique sequences: pad ending with 0 to force a count

      if (optknt.eq.1) then
          do t=1,ntim

             yin(0)    = 0
             yin(1:nx) = xin(:,t)
             yin(nx+1) = 0

            do n=1,nx
               xknt(n,t) = 0
              do j=1,nx-n+1
                 if (yin(j-1).eq.0          .and. 
     +               all(yin(j:j+n-1).eq.1) .and.
     +               yin(j+n).eq.0        ) then
                     xknt(n,t) = xknt(n,t)+1
                 end if
              end do
            end do
          end do
          return
      end if

      return
      end
C ------
      subroutine numrun1(nx, xin, optknt, xknt )
      implicit none
C
C knt = dim_numrun_n( x, optknt, n)
C                                                      INPUT
      integer  nx, optknt, optend
      integer  xin(nx) 
C                                                      OUTPUT
      integer  xknt(nx) 
C                                                      LOCAL 
      integer  n, j, knt, yin(0:nx+1)

      if (optknt.eq.0) then
          do n=1,nx
             xknt(n) = 0
            do j=1,nx-n+1
               if (all(xin(j:j+n-1).eq.1)) then
                   xknt(n) = xknt(n)+1
               end if
            end do
          end do
          return
      end if

c count unique sequences: pad ending with 0 to force a count

      if (optknt.eq.1) then
          yin(0)    = 0
          yin(1:nx) = xin
          yin(nx+1) = 0

          do n=1,nx
             xknt(n) = 0
            do j=1,nx-n+1
               if (yin(j-1).eq.0          .and. 
     +             all(yin(j:j+n-1).eq.1) .and.
     +             yin(j+n).eq.0        ) then
                   xknt(n) = xknt(n)+1
               end if
            end do
          end do
          return
      end if

      return
      end
