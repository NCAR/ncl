c orig code [ sp2_util.orig ]: John Adams
c .          check arguments to make sure calls are correct
c
      subroutine geomatv(nlon,nlat,u,v,work)
c
c pay special attention to subscript order
c .   nlon is first   [ u(nlon,nlat),v(nlon,nlat) ]
c
c     when invoked, this subroutine converts the nlon by nlat
c     geophysical vector field (u,v) to nlat by nlon mathematical
c     coordinates.  work is an UNSAVED work space of length at
c     least nlon*nlat
c
      implicit none
      integer nlon,nlat,i,j
      real u(nlon,nlat),v(nlon,nlat),work(*)
c
c     negate v
c
      do j=1,nlon
	do i=1,nlat
	  v(j,i) = -v(j,i)
	end do
      end do
c
c     traspose u,v
c
      call tplatx(nlon,nlat,u,work)
      call tplatx(nlon,nlat,v,work)
c
c     reverse (co)latitude subscript order
c
      call clatx(nlat,nlon,u)
      call clatx(nlat,nlon,v)

      return
      end
c ---------------------------------------------------------
      subroutine matgeov(nlat,nlon,u,v,work)  
c
c pay special attention to subscript order
c .   nlat is first dim  [ u(nlat,nlon), v(nlat,nlon) ]
c
c     when invoked, this subroutine converts the nlat by nlon
c     vector u & v, given in mathematical spherical coordinates, to
c     nlon by nlat vector field given in geophysical spherical
c     coordinates.  work is an UNSAVED work space of length at least
c     nlon*nlat
c
      implicit none
      integer nlon,nlat,i,j
      real u(nlat,nlon),v(nlat,nlon),work(*)
c
c     negate v
c
      do j=1,nlon
	do i=1,nlat
	  v(i,j) = -v(i,j)
	end do
      end do
c
c     reverse (co)latitude subscript order
c
      call clatx(nlat,nlon,u)
      call clatx(nlat,nlon,v)
c
c     transpose array
c
      call tplatx(nlat,nlon,u,work)
      call tplatx(nlat,nlon,v,work)

      return
      end
c -------------------------------------------------------
      subroutine geomat(nlon,nlat,s,work)
c
c pay special attention to subscript order
c .   nlon is first dim  [ s(nlon,nlat) ]
c
c     when invoked, this subroutine converts the nlon by nlat
c     scalar s, given in geophysical spherical coordinates, to
c     a nlat by nlon array given in mathematical spherical
c     coordinates.  work is an UNSAVED work space of length at
c     least nlon*nlat
c
      implicit none
      integer nlon,nlat
      real s(*),work(*)
c
c     traspose array
c
      call tplatx(nlon,nlat,s,work)
c
c     reverse (co)latitude subscript order
c
      call clatx(nlat,nlon,s)
      return
      end
c -------------------------------------------------------------
      subroutine matgeo(nlat,nlon,s,work)
c
c pay special attention to subscript order
c .   nlat is first dim  [ s(nlat,nlon) ]
c
c     when invoked, this subroutine converts the nlat by nlon
c     array s, given in mathematical spherical coordinates, to
c     a nlon by nlat array given in geophysical spherical
c     coordinates.  work is an UNSAVED work space of length at least
c     nlon*nlat
c
      implicit none
      integer nlon,nlat
      real s(*),work(*)
c
c     reverse (co)latitude subscript order
c
      call clatx(nlat,nlon,s)
c
c     transpose array
c
      call tplatx(nlat,nlon,s,work)
      return
      end
c -------------------------------------------------------------
      subroutine tplatx(n,m,data,work)
c
c     transpose the n by m array data to a m by n array data
c     work must be at least n*m words long
c
      implicit none
      integer n,m,i,j,ij,ji
      real data(*),work(*)
 
      do j=1,m
	do i=1,n
	  ij = (j-1)*n+i
	  work(ij) = data(ij)
	end do
      end do
 
      do i=1,n
	do j=1,m
	  ji = (i-1)*m+j
	  ij = (j-1)*n+i
	  data(ji) = work(ij)
	end do
      end do
 
      return
      end
c ----------------------------------------------------
      subroutine clatx(nlat,nlon,data)
c
c     reverse order of latitude (colatitude) grids
c
      implicit none
      integer nlat,nlon,nlat2,i,ib,j
      real data(nlat,nlon),temp
  
      nlat2 = nlat/2
      do i=1,nlat2
	ib = nlat-i+1
	do j=1,nlon
	  temp = data(i,j)
	  data(i,j) = data(ib,j)
	  data(ib,j) = temp
	end do
      end do
 
      return
      end
c ------------------------------------
      subroutine chkerr (string1, string2, ier,jer,ker,mer)
      implicit none

      integer       ier, jer, ker, mer 
      character*(*) string1, string2

      if (ier.ne.0 .or. jer.ne.0 .or. ker.ne.0 .or. mer.ne.0) then 
          write (*,'('' ERROR: '',a,'' : '',a
     *              ,'' : ier,jer,ker,mer='',4i3)')    
     *                  string1, string2, ier,jer,ker,mer
          stop
      endif

      return
      end
c ---------------------------------------------------------
      subroutine trctpr (nlat,mwave,mdab,ndab,nt,a,b,ier)
      implicit   none

c truncate spectral coefficients
c .   (possibly) taper the spectral coef

      integer nlat, mwave, mdab, ndab, nt, ier
      real a(mdab,ndab,nt), b(mdab,ndab,nt)

      real       con
      integer    ntwgt, m, n, iwave, jp, jw, j, k, mw2
      parameter (ntwgt=500)
      real  twgt(ntwgt)

      ier = 0
      if (mwave.eq.0) return 

      mw2 = iabs(mwave)+2
 
      do k=1,nt
       do n=mw2,nlat
        do m=1,n
           a(m,n,k) = 0.0
           b(m,n,k) = 0.0
        enddo
       enddo
      enddo

      if (mwave.gt.0) return
      
      iwave = iabs(mwave)
                                 ! perform exponential tapering 
      jp = max0(iwave/10,1)      ! exponent; determines fall-off rate
      jw = jp*10                 ! coef which has wgt=exp(-1)

      if ((iwave+1).gt.ntwgt) then
         write (*,'(''taprwv: ntwgt exceeded='',2i5)') ntwgt,(iwave+1)  
         return
      endif
          
      con = 1./real(jw*(jw+1))
      do j=0,iwave
         twgt(j+1) = exp(-(real(j*(j+1))*con)**jp)
c c c    write (*,'(''taprwv: j, twgt(j)='',2i5,1x,f15.7)') 
c c c*                        j, (j+1),twgt(j+1)   
      enddo
                                  ! now wgt the coef by the wgts
      do k=1,nt 
       do n=iwave+1,1,-1          ! traverse the diagonal
         do m=1,n
            a(m,n,k) = a(m,n,k)*twgt(n)
            b(m,n,k) = b(m,n,k)*twgt(n)
         enddo
       enddo
      enddo

      return
      end
c ----------------------------------------------------
      subroutine geoscl (mlon,nlat,nt,x,scale,ner)
      implicit none
      integer  mlon, nlat, nt, ner
      real     x(mlon,nlat,nt), scale

      integer  n, ml, nl               

c scale an array

      ner = 0
      if (scale.eq.1.0) return

      do n=1,nt
       do nl=1,nlat
        do ml=1,mlon
           x(ml,nl,n) = x(ml,nl,n)*scale 
        enddo
       enddo
      enddo

      return
      end
