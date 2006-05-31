c
c     a subroutine for testing all scalar analysis and synthesis subroutines
c
      program tsha
c
c     set dimensions with parameter statements
c
      parameter(nnlat= 15,nnlon= 18, nnt = 3)
c     parameter(nnlat=14,nnlon=20,nnt=3)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav= 5*nnlat*nnlat*nnlon)
      parameter (lldwork = nnlat*(nnlat+4))
      double precision dwork(lldwork)
      dimension work(lleng),wsave(llsav)
      dimension a(nnlat,nnlat,nnt),b(nnlat,nnlat,nnt),s(nnlat,nnlon,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      double precision dtheta, dwts
c
c     set dimension variables
c
      nlat = nnlat
      nlon = nnlon
      lwork = lleng
      lsave = llsav
      nt = nnt
      call iout(nlat,4hnlat)
      call iout(nlon,4hnlon)
      call iout(nt,4h  nt)
      isym = 0
c
c     set equally spaced colatitude and longitude increments
c
      pi = 4.0*atan(1.0)
      dphi = (pi+pi)/nlon
      dlat = pi/(nlat-1)
c
c     compute nlat gaussian points in thetag
c
      ldwork = lldwork
      call gaqd(nlat,dtheta,dwts,dwork,ldwork,ier)
      do  i=1,nlat
	thetag(i) = dtheta(i)
      end do
      call name(4hgaqd)
      call iout(ier,4h ier)
      call vecout(thetag,4hthtg,nlat)
c
c     test all analysis and synthesis subroutines
c
      do icase=1,4
c
c     icase=1 test shaec,shsec
c     icase=2 test shaes,shses
c     icase=3 test shagc,shsgc
c     icase=4 test shags,shsgs
c
      call name(4h****)
      call name(4h****)
      call iout(icase,4hicas)
c
c
c     set scalar field as (x*y*z)**k) restricted to the sphere
c
      do k=1,nt
	do j=1,nlon
	  phi = (j-1)*dphi
	  sinp = sin(phi)
	  cosp = cos(phi)
	  do i=1,nlat
	    theta = (i-1)*dlat
	    if (icase.gt.2) theta=thetag(i)
	    cost = cos(theta)
	    sint = sin(theta)
	    xyzk = (sint*(sint*cost*sinp*cosp))**k
c           s(i,j,k) = exp(xyzk)
	    s(i,j,k) = xyzk
	  end do
	end do
c     call iout(k,4h   k)
c     call aout(s(1,1,k),4h   s,nlat,nlon)
      end do

      do l=1,lsave
	wsave(l) = 0.0
      end do
      if (icase.eq.1) then

      call name(4h**ec)
      call shaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hshai)
      call iout(ierror,4hierr)

      call shaec(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)

      call shseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hshsi)
      call iout(ierror,4hierr)

      call shsec(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hshs )
      call iout(ierror,4hierr)

      else if (icase.eq.2) then

      call name(4h**es)
      call shaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)

      call name(4hshai)
      call iout(ierror,4hierr)

      call shaes(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)

      call shsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)

      call name(4hshsi)
      call iout(ierror,4hierr)

      call shses(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hshs )
      call iout(ierror,4hierr)

      else if (icase.eq.3) then

      call name(4h**gc)

      call shagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hshai)
      call iout(ierror,4hierr)

      call shagc(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)

      call shsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hshsi)
      call iout(ierror,4hierr)

      call shsgc(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hshs )
      call iout(ierror,4hierr)

      else if (icase.eq.4) then

      call name(4h**gs)

      call shagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)

      call name(4hshai)
      call iout(ierror,4hierr)

      call shags(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)

      call shsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshsi)
      call iout(ierror,4hierr)

      call shsgs(nlat,nlon,isym,nt,s,nlat,nlon,a,b,nlat,nlat,wsave,
     +lsave,work,lwork,ierror)

      call name(4hshs )
      call iout(ierror,4hierr)
      end if
c
c     compute "error" in s
c
      err2 = 0.0
      do k=1,nt
	do j=1,nlon
	  phi = (j-1)*dphi
	  sinp = sin(phi)
	  cosp = cos(phi)
	  do i=1,nlat
	    theta = (i-1)*dlat
	    if (icase.gt.2) theta = thetag(i)
	    cost = cos(theta)
	    sint = sin(theta)
	    xyzk = (sint*(sint*cost*sinp*cosp))**k
c           err2 = err2+ (exp(xyzk)-s(i,j,k))**2
	    err2 = err2 + (xyzk-s(i,j,k))**2
	  end do
	end do
c     call iout(k,4h   k)
c     call aout(s(1,1,k),4h   s,nlat,nlon)
      end do
      err2 = sqrt(err2/(nt*nlat*nlon))
      call vout(err2,4herr2)
      end do
      end
      subroutine iout(ivar,nam)
      real nam
      write(6,10) nam , ivar
   10 format(1h a4, 3h = ,i8)
      return
      end
c
      subroutine vout(var,nam)
      real nam
      write(6,10) nam , var
   10 format(1h a4,3h = ,e12.5)
      return
      end
c
      subroutine name(nam)
      real nam
      write(6,100) nam
  100 format(1h a8)
      return
      end
c
      subroutine vecout(vec,nam,len)
      dimension vec(len)
      real nam
      write(6,109) nam, (vec(l),l=1,len)
  109 format(1h a4,/(1h 8e11.4))
      return
      end
