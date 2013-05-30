C NCLFORTSTART
      subroutine poisxy1 ( xio, mx,ny, xmsg
     +           , guess, gtype, nscan, epsx, relc, mscan, ier)
      implicit none
      integer             mx, ny, nscan, guess, gtype, mscan, ier
      double precision    xio(mx,ny), xmsg, epsx, relc
C NCLEND
C
C NCL:    xout = fill_msg_grid(xio, gtype, nscan, eps, guess, opt)
      integer             nmsg, n, m
      double precision    resmax

      ier  = 0
      nmsg = 0
      do n=1,ny
        do m=1,mx
           if (xio(m,n).eq.xmsg) nmsg = nmsg + 1
        end do
      end do

c if no missing values return the original array

      if (nmsg.eq.0) return

      call poisxy2(xio,mx,ny,xmsg,nscan,epsx,relc,guess,gtype
     +            ,resmax, mscan)

      return
      end

      subroutine poisxy2(a,il,jl,amsg,maxscn,crit,relc,guess,gtype
     +                  ,resmax, mscan)
c=======================================================================
c Not sure of the original source. ? MOM ?
c=======================================================================
c     inputs:
c     a       = array with missing areas to be filled. 
c     il      = number of points along 1st dimension to be filled
c     jl      = number of points along 2nd dimension to be filled
c     maxscn  = maximum number of passes allowed in relaxation
c     crit    = criterion for ending relaxation before "maxscn" limit
c     relc    = relaxation constant
c     gtype   = 0 : not cyclic in x
c               1 : cyclic in x
c     guess   = 0 : use 0.0 as an initial guess
c             = 1 : at each "y" use the average values for that "y"
c                   think zonal averages
c     outputs:
c
c     a       = array with interpolated values 
c               non missing areas remain unchanged.
c     resmax  = max residual
c
c
      implicit   none
      logical    done
      integer    il, jl, maxscn, guess, gtype, mscan
      double precision       a(il,jl), amsg, crit, resmax

c local
c     sor     = scratch area

      integer    i, j, n, im1, ip1, jm1, jp1
      double precision       p25, relc
      double precision       sor(il,jl), res, aavg
c
      p25 = 0.25d0 

      do j=1,jl
         n    = 0
         aavg = 0.0d0
        do i=1,il
          if (a(i,j) .eq. amsg) then
              sor(i,j) = relc
          else
              n        = n + 1
              aavg     = aavg+ a(i,j)
              sor(i,j) = 0.0d0
          endif
        end do

	if (n.gt.0) then
            aavg = aavg/n
        end if

	if (guess.eq.0) then
            do i=1,il
               if (a(i,j) .eq. amsg) a(i,j) = 0.0d0
            end do
        elseif (guess.eq.1) then
            do i=1,il
              if (a(i,j) .eq. amsg) a(i,j) = aavg
            end do
        end if

      end do

c-----------------------------------------------------------------------
c     iterate until errors are acceptable.
c-----------------------------------------------------------------------
c     
      mscan = 0
100   continue
        resmax = 0.0d0
        done   = .false.
        mscan  = mscan + 1
        do j=1,jl
	   jp1 = j+1
	   jm1 = j-1
           if (j.eq.1 ) jm1 = 2
           if (j.eq.jl) jp1 = jl-1
          do i=1,il
c                                      only work on missing value locations
             if (sor(i,j).ne.0.0) then 
   	         im1 = i-1
       	         ip1 = i+1
c                                      cyclic in x           
    	         if (i.eq.1  .and. gtype.eq.1) im1 = il 
    	         if (i.eq.il .and. gtype.eq.1) ip1 = 1
c                                      not cyclic in x           
    	         if (i.eq.1  .and. gtype.eq.0) im1 = 2
    	         if (i.eq.il .and. gtype.eq.0) ip1 = il-1 
    
                 res = p25*(a(im1,j)+a(ip1,j)+a(i,jm1)+a(i,jp1)) -a(i,j)
                 res     = res*sor(i,j)
                 a(i,j)  = a(i,j) + res
                 resmax  = max(abs(res),resmax)
             end if
          end do
        end do
c
      if (resmax .le. crit .or. n.eq.maxscn)  done = .true.
c
      if (.not. done .and. mscan .lt. maxscn) go to 100
c
c     write (6,99) mscan, resmax
c  99 format (1x,'==> Extrapolated  mscan=',i4
c    &,       ' scans.  max residual=', g14.7)

      return
      end
