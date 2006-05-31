c
c     1/976
c
c     a subroutine for testing all vector laplacian and its inverse
c
c     (1) first set the vector function (rotation about equator) using
c         v = cos(phi) and w = -cos(theta)*sin(phi)
c
c     (2) set vector laplacian ananlytically
c         vlap = -2.*cos(phi)=-2.*v, wlap = -2.*w
c         (i.e., L(v,w) = -2.*(v,w) so (v,w) is an eigenfunction for the
c         vector Laplacian with eigenvalue -2.
c
c     (3) compute the coefficients br,bi,cr,ci of (v,w) using vector analysis
c
c     (3) compute the vector laplacian of (v,w) using vlapec,vlapes,vlapgc,vlapgs
c
c     (4) compare (3) with (2)
c
c     (5) invert (4) and compare with (v,w)
c
      program tvlap
c
c     set dimensions with parameter statements
c
      parameter(nnlat=29 ,nnlon= 16, nnt = 1)
      parameter (mmdbc = (nnlon+2)/2)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav=15*nnlat*nnlat*nnlon)
      parameter (lldwork = 4*nnlat*nnlat )
      double precision dwork(lldwork)
      dimension work(lleng),wsave(llsav)
      dimension br(mmdbc,nnlat,nnt),bi(mmdbc,nnlat,nnt)
      dimension cr(mmdbc,nnlat,nnt),ci(mmdbc,nnlat,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      dimension v(nnlat,nnlon,nnt),w(nnlat,nnlon,nnt)
      dimension vlap(nnlat,nnlon,nnt),wlap(nnlat,nnlon,nnt)

      double precision dtheta, dwts
c
c     set dimension variables
c
      nlat = nnlat
      nlon = nnlon
      nmax = max0(nlat,nlon)
      mdbc = mmdbc

      lwork = lleng
      lsave = llsav
      nt = nnt
      call iout(nlat,4hnlat)
      call iout(nlon,4hnlon)
      call iout(nt,4h  nt)
      isym = 0
      ityp = 0
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
c     test all divergence and inverse divergence subroutines
c
      do icase=1,4
      call name(4h****)
      call name(4h****)
      call iout(icase,4hicas)
c
c     set vector field v,w
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
	    if (k.eq.1) then
	    v(i,j,k) = cosp
	    w(i,j,k) = -cost*sinp
	    vlap(i,j,k) = -2.0*v(i,j,k)
	    wlap(i,j,k) = -2.0*w(i,j,k)
	    end if
	  end do
	end do
      end do

      if (nmax.lt.10) then
      do kk=1,nt
      call iout(kk,4h**kk)
      call aout(v(1,1,kk),4h   v,nlat,nlon)
      call aout(w(1,1,kk),4h   w,nlat,nlon)
      call aout(vlap(1,1,kk),4hvlap,nlat,nlon)
      call aout(wlap(1,1,kk),4hwlap,nlat,nlon)
      end do
      end if

      if (icase.eq.1) then

      call name(4h**ec)
c
c     analyze vector field
c
      call vhaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,mdbc,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(br(1,1,kk),4h  br,nlat,nlat)
c     call aout(bi(1,1,kk),4h  bi,nlat,nlat)
c     call aout(cr(1,1,kk),4h  cr,nlat,nlat)
c     call aout(ci(1,1,kk),4h  ci,nlat,nlat)
c     end do
c     end if
c
c     compute vector laplacian
c

      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vlapec(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvlap )
      call iout(ierror,4hierr)


      else if (icase.eq.2) then

      call name(4h**es)
c
c     analyze vector field
c
      call vhaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaes(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdbc,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhae)
      call iout(ierror,4hierr)

      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vlapes(nlat,nlon,isym,nt,vlap,wlap,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvlap )
      call iout(ierror,4hierr)

      else if (icase .eq.3 ) then

      call name(4h**gc)
c
c     analyze vector field
c
      call vhagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhagc(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,mdbc,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(br(1,1,kk),4h  br,nlat,nlat)
c     call aout(bi(1,1,kk),4h  bi,nlat,nlat)
c     call aout(cr(1,1,kk),4h  cr,nlat,nlat)
c     call aout(ci(1,1,kk),4h  ci,nlat,nlat)
c     end do
c     end if
c
c     compute vector laplacian
c

      call vhsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vlapgc(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvlap )
      call iout(ierror,4hierr)

      else if (icase .eq. 4) then

      call name(4h**gs)
c
c     analyze vector field
c
      call vhagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhags(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,mdbc,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(br(1,1,kk),4h  br,nlat,nlat)
c     call aout(bi(1,1,kk),4h  bi,nlat,nlat)
c     call aout(cr(1,1,kk),4h  cr,nlat,nlat)
c     call aout(ci(1,1,kk),4h  ci,nlat,nlat)
c     end do
c     end if
c
c     compute vector laplacian
c

      call vhsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vlapgs(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvlap )
      call iout(ierror,4hierr)

      end if

      if (nmax.lt.10) then
      do kk=1,nt
      call iout(kk,4h**kk)
      call aout(vlap(1,1,kk),4hvlap,nlat,nlon)
      call aout(wlap(1,1,kk),4hwlap,nlat,nlon)
      end do
      end if
c
c     compute "error" in vlap,wlap
c
      err2v = 0.0
      err2w =0.0
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    if (k.eq.1) then
	      err2v = err2v+(vlap(i,j,k)+2.*v(i,j,k))**2
	      err2w = err2w+(wlap(i,j,k)+2.*w(i,j,k))**2
	    end if
	  end do
	end do
      end do
c
c     set and print least squares error in vlap,wlap
c
      err2v = sqrt(err2v/(nt*nlat*nlon))
      err2w = sqrt(err2w/(nt*nlon*nlat))
      call vout(err2v,4herrv)
      call vout(err2w,4herrw)
c
c     now recompute (v,w) inverting (vlap,wlap) ivlap codes
c
      do kk=1,nt
      do j=1,nlon
      do i=1,nlat
      v(i,j,kk) = 0.0
      w(i,j,kk) = 0.0
      end do
      end do
      end do


      if (icase.eq.1) then

      call name(4h**ec)
c
c     analyze vector field (vlap,wlap)
c
      call vhaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,
     +br,bi,cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call ivlapec(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hivlp)
      call iout(ierror,4hierr)

      else if (icase.eq.2) then

      call name(4h**es)
c
c     analyze vector field (vlap,wlap)
c
      call vhaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaes(nlat,nlon,isym,nt,vlap,wlap,nlat,nlon,
     +br,bi,cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call ivlapes(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,pertrb,ierror)
      call name(4hivlp)
      call iout(ierror,4hierr)

      else if (icase .eq. 3) then

      call name(4h**gc)

c
c     analyze vector field (vlap,wlap)
c
      call vhagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhagc(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,
     +br,bi,cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vhsgci(nlat,nlon,wsave,lsave,work,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call ivlapgc(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hivlp)
      call iout(ierror,4hierr)

      else if (icase .eq. 4) then

      call name(4h**gs)

c
c     analyze vector field (vlap,wlap)
c
      call vhagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhags(nlat,nlon,ityp,nt,vlap,wlap,nlat,nlon,
     +br,bi,cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vhsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call ivlapgs(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,
     +cr,ci,mdbc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hivlp)
      call iout(ierror,4hierr)

      end if


      if (nmax.lt.10) then
      do kk=1,nt
      call iout(kk,4h**kk)
      call aout(v(1,1,kk),4h   v,nlat,nlon)
      call aout(w(1,1,kk),4h   w,nlat,nlon)
      end do
      end if

c
c     compare this v,w with original
c
      err2v = 0.0
      err2w = 0.0
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
	    if (k.eq.1) then
	      ve = cosp
	      we = -cost*sinp
	    end if
	    err2v = err2v + (v(i,j,k)-ve)**2
	    err2w = err2w + (w(i,j,k)-we)**2
	  end do
	end do
      end do
      err2v = sqrt(err2v/(nlat*nlon*nt))
      err2w = sqrt(err2w/(nlat*nlon*nt))
      call vout(err2v,4herrv)
      call vout(err2w,4herrw)

c
c     end of icase loop
c
      end do
      end
