C NCLFORTSTART
c tst subroutine arealinint2dx (mxi,nyi,ngrd,xi,yi,zi,wxi,wyi,zimsg
c tst+                         ,mcyc,ncyc,mxo,nyo,xo,yo,zo,critpc
c tst+                         ,debug,ier)
c tst implicit none
c                                               ! INPUT
c tst integer          mxi, nyi, ngrd, mxo, nyo,
c tst+                 mcyc, ncyc, debug, ier  
c tst double precision zi(mxi,nyi,ngrd), xi(mxi), yi(nyi)
c tst double precision wxi(mxi), wyi(nyi), zimsg, critpc
c tst double precision xo(mxo) , yo(nyo)
c                                               ! OUTPUT
c tst double precision zo(mxo,nyo,ngrd)
C NCLEND
c                                               ! LOCAL
c MH: The interface should allocate.
c tst double precision xilft(mxi), xirgt(mxi), dxi(mxi)
c tst double precision yibot(nyi), yitop(nyi), dyi(nyi)
c tst double precision xolft(mxo), xorgt(mxo)
c tst double precision fracx(mxi,mxo), fracy(nyi,nyo)  
c tst double precision ziwrk(mxi,nyi), zowrk(mxo,nyo)
c tst double precision yiwrk(nyi), yowrk(nyo)
c tst integer          indx(2,mxo), indy(2,nyo)

c                  ! interface sets the following
c tst ier    = 0
c tst ncyc   = 0
c tst debug  = 0
c tst critpc = 100 ! this may change via attribi]ute

c tst do n=1,mxi
c tst    wxi(n) = 1.0d0   ! always
c tst end do

c tst                  ( if wyi is a scalar)
c tst do n=1,nyi
c tst    wyi(n) = 1.0d0
c tst end do

c tst call arealinint2da (mxi,nyi,ngrd,xi,yi,zi,wxi,wyi,zimsg
c tst+                   ,mcyc,ncyc,mxo,nyo,xo,yo,zo,critpc,debug,ier
c tst+                   ,xilft,xirgt, yibot, yitop, dyi
c tst+                   ,xolft, xorgt, yiwrk, yowrk
c tst+                   ,fracx, fracy, ziwrk, zowrk, indx, indy)

c tst return
c tst end

C NCLFORTSTART
      subroutine arealinint2da(mxi,nyi,ngrd,xi,yi,zi,wxi,wyi,zimsg
     +                        ,mcyc,ncyc,mxo,nyo,xo,yo,zo,critpc,debug
     +                        ,ier
     +                        ,xilft,xirgt, yibot, yitop, dyi
     +                        ,xolft, xorgt, yiwrk, yowrk
     +                        ,fracx, fracy, ziwrk, zowrk, indx, indy)
      implicit none
c                                               ! INPUT
      integer          mxi, nyi, ngrd, mxo, nyo,
     +                 mcyc, ncyc, debug, ier  
      double precision zi(mxi,nyi,ngrd), xi(mxi), yi(nyi)
      double precision wxi(mxi), wyi(nyi), zimsg, critpc
      double precision xo(mxo) , yo(nyo)

      double precision xilft(mxi), xirgt(mxi), dxi(mxi)
      double precision yibot(nyi), yitop(nyi), dyi(nyi)
      double precision xolft(mxo), xorgt(mxo)
      double precision fracx(mxi,mxo), fracy(nyi,nyo)  
      double precision ziwrk(mxi,nyi), zowrk(mxo,nyo)
      double precision yiwrk(nyi), yowrk(nyo)
      integer          indx(2,mxo), indy(2,nyo)
c                                               ! OUTPUT
      double precision zo(mxo,nyo,ngrd)
C NCLEND
c                                               ! LOCAL
      integer          ni, mi, ng, no, mo, kmsg, kbox,
     +                 nn, mm, niStrt, niLast, miStrt, miLast,
     +                 monoxi, monoyi, monoxo, monoyo, 
     +                 ierx, iery
      double precision sz, sw, dwf, boxpc

c The rather cumbersome code below allows users to input
c to input arrays in which the yi/yo coordinates can be 
c in arbitrary mono order: (a) Both mono {in/de}creasing,
c (b) one mono {in/de}creasing and the other {de/in}creasing.
  
      ier = 0

c just in case of early termination: initializing returned zo to msg

      do ng=1,ngrd
        do no=1,nyo
          do mo=1,mxo
             zo(mo,no,ng) = zimsg
          end do
        end do
      end do

c check to see if yi is monotonically increasing or decreasing

      monoyi = 1
      if ((yi(2)-yi(1)).lt.0.0d0) monoyi = -1
      monoyo = 1
      if ((yo(2)-yo(1)).lt.0.0d0) monoyo = -1
      
      monoxi = 1
      if ((xi(2)-xi(1)).lt.0.0d0) then
          monoxi = -1
          ier    = -2
c          print *, "code does not handle monotonically decreasing xi"
          return
      end if

      monoxo = 1
      if ((xo(2)-xo(1)).lt.0.0d0) then
          monoxo = -1
          ier    = -2
c          print *, "code does not handle monotonically decreasing xo"
          return
      end if


c define area cells: take advantage of the fact that this is
c .   regular rectilinear grid. The grid spacing may be unequal.   
c If the 'y' direction is mono decreasing then reorder

      call areacells (mxi, mxo, xi, xo, mcyc
     +               ,xilft, xirgt, dxi, fracx, indx, debug) 

c                       ! reorder or not ?               
      if (monoyi.gt.0) then
c                       ! no reorder necessary
          do ni=1,nyi
             yiwrk(ni) = yi(ni)
          end do
      else
c                       ! reorder the 'yi' to mono increasing
          do ni=1,nyi
             yiwrk(ni) = yi(nyi-ni+1)
          end do
      end if

      if (monoyo.gt.0) then
          do no=1,nyo
             yowrk(no) = yo(no)
          end do  
      else
          do no=1,nyo
             yowrk(no) = yo(nyo-no+1)
          end do
      end if
c                       ! yiwrk and yowrk are both mono increasing
      call areacells (nyi, nyo, yiwrk, yowrk, ncyc
     +               ,yibot, yitop, dyi, fracy, indy, debug) 

      if (debug.eq.1) then
          do mo=1,mxo
             print *,"indx: ",mo,indx(1,mo),indx(2,mo) 
          end do
          do no=1,nyo
             print *,"indy: ",no,indy(1,no),indy(2,no) 
          end do
      end if

c check to make sure that the output grid is lower resolution
c .   conchkres requires monotonic input
c .   Return if either the 'xo' or 'yo' is higher
c .   resolution than the input grid.

      call conchkres (xi,mxi,xo,mxo,ierx) 
      call conchkres (yiwrk,nyi,yowrk,nyo,iery) 
      if (ierx.ne.0 .or. iery.ne.0) then
          ier = -5
          return
      end if

c for each grid and  for each output point (mo,no,ng) 
c .   reverse the order if input was decreasing in 'y'

      do ng=1,ngrd
c                      ! separate sections/loops for efficiency
         if (monoyi.gt.0) then
c                      ! no reorddr necessary
             do ni=1,nyi
               do mi=1,mxi
                  ziwrk(mi,ni) = zi(mi,ni,ng)
               end do
             end do
         else
c                      ! reorder the 'yi' indices [ note:  nn ]  
             do ni=1,nyi
                nn = nyi-ni+1
               do mi=1,mxi
                  ziwrk(mi,ni) = zi(mi,nn,ng)
               end do
             end do
         end if
c                      ! initialize interpolated grid
         do no=1,nyo
           do mo=1,mxo
              zowrk(mo,no) = zimsg    
           end do
         end do
c                      ! loop over each output point
        do no=1,nyo
          do mo=1,mxo

c extract the input indices that affect the output grid values. 
c .   indx/indy are the 'pointers' to the sub-area of the 
c .   output grid.

             miStrt = indx(1,mo)
             miLast = indx(2,mo)
             niStrt = indy(1,no)
             niLast = indy(2,no)

             if (niStrt.gt.0 .and. miStrt.gt.0) then

c compute the weighted sum value for the output grid point
  
                 kbox = 0
                 kmsg = 0
                 sz   = 0.0d0
                 sw   = 0.0d0

                 do nn=niStrt,niLast
                   do mm=miStrt,miLast
                      kbox = kbox+1
c c c                 if (zi(mm,nn,ng).ne.zimsg) then
                      if (ziwrk(mm,nn).ne.zimsg) then
                          dwf= wxi(mm)*wyi(nn)*fracx(mm,mo)*fracy(nn,no) 
c c c                     sz = sz + zi(mm,nn,ng)*dwf
                          sz = sz + ziwrk(mm,nn)*dwf
                          sw = sw + dwf
                      else
                          kmsg = kmsg+1
c critpc=100.0 means all values must be present. Skip out at 1st msg value
                          if (critpc.eq.100d0) go to 10
                      end if
                   end do
                 end do
c boxpc: percent 'good' values 
                 boxpc = 100.0d0*(dble(kbox-kmsg)/dble(kbox))

c compute the weighted area value for the output grid point
c .   only if there were no msg pts.

                 if (sw.gt.0.0d0 .and. boxpc.ge.critpc ) then
c c c                zo(mo,no,ng) = sz/sw
                     zowrk(mo,no) = sz/sw
                     if (debug.eq.1 .and. ng.eq.1) then
                         print *, mo,no,zowrk(mo,no)," : ",boxpc
                     end if
                 end if
10               continue

             end if     ! if (zi(mi,ni,ng).ne.zimsg )

          end do        ! end: do mo=1,mxo
        end do          ! end: do no=1,nxo

c                       ! place in return grid: separate for efficiency      
        if (monoyo.gt.0) then
c                       ! no reorder necessary
            do no=1,nyo
              do mo=1,mxo
                 zo(mo,no,ng) = zowrk(mo,no)    
              end do
            end do
        else
c                       ! reorder the 'y'  [ note:  nn ]                
            do no=1,nyo
               nn = nyo-no+1
              do mo=1,mxo
                 zo(mo,no,ng) = zowrk(mo,nn)    
              end do
	    end do
        end if

      end do            ! end: do ng=1,ngrd

      return
      end

      subroutine areacells (nxi, nxo, xi, xo, icyc
     +                     ,xilft, xirgt, dxi, fracx, indx, debug) 
 
c This was originally written for "x" but it works for
c .   *any* mononotonically increasing 1d array
c .   lft<==>bot , rgt<==>top , dx<==>dy , fracx<==>fracy , indx<==>indy
c .   nxi<==>nyi , nxo<==>nyo

      implicit          none
      integer           nxi, nxo, icyc, debug
      double precision  xi(nxi), xilft(nxi), xirgt(nxi), dxi(nxi)
      double precision  xo(nxo), xolft(nxo), xorgt(nxo)

      integer           indx(2,nxo)
      double precision  fracx(nxi,nxo)

      integer ni, no

c left and right cell boundaries for input and output 

      do ni=2,nxi-1
         xilft(ni) = 0.5d0*(xi(ni-1)+xi(ni))
         xirgt(ni) = 0.5d0*(xi(ni+1)+xi(ni))
      end do

      do no=2,nxo-1
         xolft(no) = 0.5d0*(xo(no-1)+xo(no))
         xorgt(no) = 0.5d0*(xo(no+1)+xo(no))
      end do
c                                 left/right boundaries
      xirgt(1)   =  0.5d0*(xi(2)  +xi(1))
      xorgt(1)   =  0.5d0*(xo(2)  +xo(1))
      xilft(nxi) =  0.5d0*(xi(nxi)+xi(nxi-1))
      xolft(nxo) =  0.5d0*(xo(nxo)+xo(nxo-1))

c non-periodic [icyc=0] or periodic [icyc=1]  left/right boundaries

      if (icyc.eq.0) then
c                                 half width ends
          xilft(1)   = xi(1)
          xolft(1)   = xo(1)
          xirgt(nxi) = xi(nxi)
          xorgt(nxo) = xo(nxo)
      else
c                                      full width ends
          xilft(1)   = xi(1)   - 0.5d0*(xi(2)-xi(1))
          xolft(1)   = xo(1)   - 0.5d0*(xo(2)-xo(1))
          xirgt(nxi) = xi(nxi) + 0.5d0*(xi(nxi)-xi(nxi-1))
          xorgt(nxo) = xo(nxo) + 0.5d0*(xo(nxo)-xo(nxo-1))
      end if
c                                nominal (full width) dx
      do ni=1,nxi
         dxi(ni) = xirgt(ni)-xilft(ni)
      end do

c for each output point: 
c .   find portion of xi cell boundaries 
c .   within each output [xo] cell boundary
c
      do no=1,nxo
         indx(1,no) = 0     
         indx(2,no) = 0     

c Find all xi points between the xo cell coundaries 
c NCL would use the "ind" function for the "i-1,nxi" loop
c .   isave = ind(xi.ge.xolft(n) .and. xi.le.xorgt(n))
c f90 could use the "where" construct

         do ni=1,nxi
            fracx(ni,no)  = 0.0d0

c skip if outside cell boundaries: xi < xolft   xi > xorgt
c .         xi < xolft   go to next iteration
c .         xi > xorgt   go to next output point

            if (xi(ni).lt. xolft(no)) go to 10       
            if (xi(ni).gt. xorgt(no)) go to 20   

c xi must be within xo cell boundaries:  xolft <= xi  <= xorgt      

            if (xilft(ni).ge.xolft(no) .and. xirgt(ni).le.xorgt(no))then

c xi cell boundaries totally  within xo cell boundaries
c               xolft     [xilft    xi     xirgt]     xorgt 
c                         [-------  dx  --------]     xorgt 

                fracx(ni,no) = 1.0d0

            elseif(xilft(ni).lt.xolft(no) .and. 
     +             xirgt(ni).ge.xolft(no) .and. 
     +             xi(ni).ge.xolft(no)  ) then

c                                       xolft     xi        xorgt 
c left cell boundary overlap:    xilft              xirgt      
c                                       [---- dx------]

                fracx(ni,no) = (xirgt(ni)-xolft(no))/dxi(ni)

            elseif(xirgt(ni).gt.xorgt(no) .and. 
     +             xilft(ni).lt.xorgt(no) .and.  
     +             xi(ni).le.xorgt(no)   ) then
 
c                                      xolft       xi      xorgt 
c right cell boundary overlap:              xilft               xirgt
c                                             [------ dx -------]

                fracx(ni,no) = (xorgt(no)-xilft(ni))/dxi(ni)
            end if

c for efficiency when computing the values, 
c .    save the 'start' and 'end' subscripts

            if (indx(1,no).eq.0) indx(1,no) = ni
            indx(2,no) = ni

   10       continue               ! if (xi(ni).lt. xolft(no)) go to 10       
            end do                 ! end "ni"

   20    continue                  ! if (xi(ni).gt. xorgt(no)) go to 20   
         end do                    ! end "no"

      return
      end


      subroutine conchkres (zi,nzi,zo,nzo,ier)
      implicit none
      integer  nzi, nzo, ier
      double precision zi(nzi), zo(nzo)

      integer  n
      double precision dzimin, dzomin

      ier = 0

      dzimin = 1d20 
      do n=2,nzi
         if (abs(zi(n)-zi(n-1)) .lt. dzimin) dzimin = abs(zi(n)-zi(n-1))
      end do

      dzomin = 1d20 
      do n=2,nzo
         if (abs(zo(n)-zo(n-1)) .lt. dzomin) dzomin = abs(zo(n)-zo(n-1))
      end do

      if (dzomin.lt.dzimin) ier = 5

      return
      end
