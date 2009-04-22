C NCLFORTSTART
      subroutine xy2pdf90(nxy,x,y, xmsg,ymsg, nby, mbx, pdf
     *                   ,mbxp1,nbyp1,binxbnd, binybnd,ipcnt,ier)
      implicit none
      integer  nxy, nby, mbx, mbxp1,nbyp1, ipcnt,ier
      double precision x(nxy), y(nxy), xmsg, ymsg
     *                ,binxbnd(mbxp1), binybnd(nbyp1)
      double precision pdf(mbx,nby)
C NCLEND
      integer m, n, mb, nb, kxy, unit, msgflg
 
c Initialize; Count valid values 

      ier   = 0
      pdf   = 0.0
      kxy   = count(x.ne.xmsg .and. y.ne.ymsg)

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

c binning

      if (msgflg.eq.1) then
          do nb=1,nby
            do mb=1,mbx
               pdf(mb,nb) = count(x.ne.xmsg .and. y.ne.ymsg .and.    
     *                            x.ge.binxbnd(mb)  .and.
     *                            x.lt.binxbnd(mb+1).and. 
     *                            y.ge.binybnd(nb)  .and.
     *                            y.lt.binybnd(nb+1))
            end do
          end do
      else
          do nb=1,nby
            do mb=1,mbx
               pdf(mb,nb) = count(x.ge.binxbnd(mb)  .and.
     *                            x.lt.binxbnd(mb+1).and. 
     *                            y.ge.binybnd(nb)  .and.
     *                            y.lt.binybnd(nb+1))
            end do
          end do
      end if

      unit = 1
      if (ipcnt.eq.1) then
          unit = 100
      end if

      pdf = unit*(pdf/kxy)

      return
      end
