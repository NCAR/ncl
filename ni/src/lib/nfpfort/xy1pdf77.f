C NCLFORTSTART
      subroutine x1pdf77(nx,x,xmsg, nbx, pdf
     *                   ,nbxp1,binxbnd,ipcnt,ier)
      implicit none
      integer  nx, nbx, nbxp1, ipcnt,ier
      double precision x(nx), xmsg, binxbnd(nbxp1)
C                                    ! output
      double precision pdf(nbx)
C NCLEND
      integer n, nb, kx, unit, msgflg 

      ier = 0
      do nb=1,nbx
         pdf(nb) = 0.0d0
      end do

c number of non-missing values

      kx = 0
      do n=1,nx 
         if (x(n).ne.xmsg) kx = kx + 1
      end do

c Valid data check

      if (kx.eq.0) then
          ier = 1
          return
      end if

c Very large arrays can result in slow execution times. 
c .   To minimize unnecessary do loop checks, see if missing
c .   values are present.

      msgflg = 1
      if (kx.eq.nx) msgflg = 0

c Binning

      if (msgflg.eq.1) then
            do nb=1,nbx
              do n=1,nx 
                 if (x(n).ne.xmsg .and.
     *               x(n).ge.binxbnd(nb)  .and. 
     *               x(n).lt.binxbnd(nb+1)) then
                     pdf(nb) = pdf(nb) + 1
                 end if
              end do
            end do
      else
            do nb=1,nbx
              do n=1,nx 
                 if (x(n).ge.binxbnd(nb)  .and. 
     *               x(n).lt.binxbnd(nb+1)) then
                     pdf(nb) = pdf(nb) + 1
                 end if
              end do
            end do
      end if

c special case for last bin

      do n=1,nx 
         if (x(n).eq.binxbnd(nbx+1)) then
             pdf(nbx) = pdf(nbx) + 1
         end if
      end do

c change return units to % if flag set

      unit = 1
      if (ipcnt.eq.1) then
          unit = 100
          do nb=1,nbx
             pdf(nb) = unit*(pdf(nb)/kx)
          end do
      end if

      return
      end
