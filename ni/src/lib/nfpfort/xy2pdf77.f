C NCLFORTSTART
      subroutine xy2pdf77(nxy,x,y, xmsg,ymsg, nby, mbx, pdf
     *                   ,mbxp1,nbyp1,binxbnd, binybnd,ipcnt,ier)
      implicit none
      integer  nxy, nby, mbx, mbxp1, nbyp1, ipcnt,ier
      double precision x(nxy), y(nxy), xmsg, ymsg
     *                ,binxbnd(mbxp1), binybnd(nbyp1)
      double precision pdf(mbx,nby)
C NCLEND
      integer m, n, mb, nb, kxy, unit, msgflg 

      ier = 0
      do nb=1,nby
        do mb=1,mbx
           pdf(mb,nb) = 0.0d0
        end do
      end do

      kxy = 0
      do n=1,nxy 
         if (x(n).ne.xmsg .and. y(n).ne.ymsg) kxy = kxy + 1
      end do

c Valid data check

      if (kxy.eq.0) then
          ier = 1
          return
      end if

c Very large arrays can result in slow execution times. 
c .   To minimize unnecessary do loop checks, see if missing
c .   values are present.

      msgflg = 1
      if (kxy.eq.nxy) msgflg = 0

c Binning

      if (msgflg.eq.1) then
          do nb=1,nby
            do mb=1,mbx
              do n=1,nxy 
                 if (x(n).ne.xmsg .and. y(n).ne.ymsg) then
                     if (x(n).ge.binxbnd(mb)  .and. 
     *                   x(n).lt.binxbnd(mb+1).and. 
     *                   y(n).ge.binybnd(nb)  .and.
     *                   y(n).lt.binybnd(nb+1)) then 
                         pdf(mb,nb) = pdf(mb,nb) + 1
                     end if
                 end if
              end do
            end do
          end do
      else
          do nb=1,nby
            do mb=1,mbx
               do n=1,nxy 
                  if (x(n).ge.binxbnd(mb)  .and. 
     *                x(n).lt.binxbnd(mb+1).and. 
     *                y(n).ge.binybnd(nb)  .and.
     *                y(n).lt.binybnd(nb+1)) then 
                      pdf(mb,nb) = pdf(mb,nb) + 1
                  end if
               end do
            end do
          end do
      end if

c change return units to % if flag set

      unit = 1
      if (ipcnt.eq.1) then
          unit = 100
      end if

      do nb=1,nby
        do mb=1,mbx
           pdf(mb,nb) = unit*(pdf(mb,nb)/kxy)
        end do
      end do

      return
      end
