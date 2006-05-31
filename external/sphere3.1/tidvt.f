c
c     3/97
c
c     a subroutine for testing all divergence,vorticity and idvt(ec,es,gc,gs) routines
c
c     (1) first set a valid vector field by setting a stream function sf and velocity
c         potential function sv as polys in x,y,z restricted to the sphere.  Then
c         derive (v,w) and dv,vt from sf and sv analytically by differentiation.
c         (see tvha.f)
c
c     (2) compute the coefficients br,bi,cr,ci of (v,w) using vector analysis
c
c     (3) compute the divergence and vorticity of (v,w) using div,vrt (es,ec,gc,gs)
c
c     (4) compare with divergence and vorticity from (1)
c
c     (5) invert dv,vt with idvt(ec,es,gc,gs) and compare with vector field from (1)
c
      program tidvt
c
c     set dimensions with parameter statements
c
      parameter(nnlat= 25,nnlon= 16, nnt = 3)
      parameter(mmdab = (nnlon+2)/2, mmdb = (nnlon+1)/2)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav=15*nnlat*nnlat*nnlon)
      dimension work(lleng),wsave(llsav)
      parameter (lldwork = 4*nnlat*nnlat )
      double precision dwork(lldwork)
      dimension br(mmdb,nnlat,nnt),bi(mmdb,nnlat,nnt)
      dimension cr(mmdb,nnlat,nnt),ci(mmdb,nnlat,nnt)
      dimension ad(mmdab,nnlat,nnt),bd(mmdab,nnlat,nnt)
      dimension av(mmdab,nnlat,nnt),bv(mmdab,nnlat,nnt)
      dimension dv(nnlat,nnlon,nnt),vt(nnlat,nnlon,nnt)
      dimension dvsav(nnlat,nnlon,nnt),vtsav(nnlat,nnlon,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      dimension v(nnlat,nnlon,nnt),w(nnlat,nnlon,nnt)
      dimension vsav(nnlat,nnlon,nnt),wsav(nnlat,nnlon,nnt)
      dimension ptrbd(nnt),ptrbv(nnt)
      double precision dtheta, dwts
c
c     set dimension variables
c
      nlat = nnlat
      nlon = nnlon
      mdab = mmdab
      mdb = mmdb
      nmax = max0(nlat,nlon)

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
c
c     icase=1 corresponds to "ec"
c     icase=2 corresponds to "es"
c     icase=3 corresponds to "gc"
c     icase=4 corresponds to "gs"

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
	    x = sint*cosp
	    y = sint*sinp
	    z = cost
	    if (k .eq.1) then
c              sf = x
c              sv = y
c
c          v = -1/sint*dstdp + dsvdt
c
c          w =  1/sint*dsvdp + dstdt
c
c          dv = 1/sint*[d(sint*v)/dt + dwdp]  = dvdt + ct/st*v + 1/st*dwdp
c
c          vt = 1/sint*[-dv/dp + d(sint*w)/dt) = dwdt + ct/st*w - 1/st*dvdp

	       v(i,j,k) = sinp + cost*sinp
	       w(i,j,k) = cosp + cost*cosp
	       dv(i,j,k) = -2.0*sint*sinp
	       vt(i,j,k) = -2.0*sint*cosp
	    else if (k.eq.2) then
C              sf = y
c              sv = z
	       v(i,j,k) = -cosp-sint
	       w(i,j,k) = cost*sinp
	       vt(i,j,k) = -2.*sint*sinp
	       dv(i,j,k) = -2.*cost
	    else if (k.eq.3) then
c              st = x
c              sv = z
	       v(i,j,k) = sinp - sint
	       w(i,j,k) = cost*cosp
	       vt(i,j,k) = -2.*sint*cosp
	       dv(i,j,k) = -2.*cost
	    end if
c
c      save derived vector field,vorticity,divergence for latter comparison
c
	    vtsav(i,j,k) = vt(i,j,k)
	    dvsav(i,j,k) = dv(i,j,k)
	    vsav(i,j,k) = v(i,j,k)
	    wsav(i,j,k) = w(i,j,k)
	  end do
	end do
      end do

      if (icase.eq.1) then

      call name(4h**ec)
c
c     analyze vector field
c
      call vhaeci(nlat,nlon,wsave,lsave,work,lwork,dwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdb,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)
c
c     compute divergence,vorticity of (v,w) in dv,vt
c

      call shseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshsi)
      call iout(ierror,4hierr)

      call divec(nlat,nlon,isym,nt,dv,nlat,nlon,br,bi,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hdiv )
      call iout(ierror,4hierr)

      call vrtec(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hvrt )
      call iout(ierror,4hierr)


      else if (icase.eq.2) then

      call name(4h**es)
c
c     analyze vector field
c
      call vhaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)
      call vhaes(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdb,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call shsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshsi)
      call iout(ierror,4hierr)

      call dives(nlat,nlon,isym,nt,dv,nlat,nlon,br,bi,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hdiv )
      call iout(ierror,4hierr)
      call vrtes(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hvrt )
      call iout(ierror,4hierr)

      else if (icase .eq. 3) then

      call name(4h**gc)
c
c     analyze vector field
c
      call vhagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)
      call vhagc(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdb,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call shsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshsi)
      call iout(ierror,4hierr)

      call divgc(nlat,nlon,isym,nt,dv,nlat,nlon,br,bi,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hdiv )
      call iout(ierror,4hierr)
      call vrtgc(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hvrt )
      call iout(ierror,4hierr)

      else if (icase .eq. 4) then

      call name(4h**gs)
c
c     analyze vector field
c
      call vhagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)
      call vhags(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdb,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call shsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshsi)
      call iout(ierror,4hierr)

      call divgs(nlat,nlon,isym,nt,dv,nlat,nlon,br,bi,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hdiv )
      call iout(ierror,4hierr)
      call vrtgs(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdb,nlat,
     +wsave,lsave,work,lwork,ierror)
      call name(4hvrt )
      call iout(ierror,4hierr)

      end if
c
c     compute "error" in dv,vt
c
      err2d = 0.0
      err2v = 0.0
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	      err2d = err2d + (dv(i,j,k)-dvsav(i,j,k))**2
	      err2v = err2v + (vt(i,j,k)-vtsav(i,j,k))**2
	  end do
	end do
      end do
c
c     set and print least squares error in dv
c
      err2d = sqrt(err2d/(nt*nlat*nlon))
      call vout(err2d,4herrd)
      err2v = sqrt(err2v/(nt*nlat*nlon))
      call vout(err2v,4herrv)
c
c     now recompute (v,w) inverting dv,vt using idvt(ec,es,gc,gs)
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


      call shaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shaec(nlat,nlon,isym,nt,dv,nlat,nlon,ad,bd,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call shaec(nlat,nlon,isym,nt,vt,nlat,nlon,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hidvi)
      call iout(ierror,4hierr)

      call idvtec(nlat,nlon,isym,nt,v,w,nlat,nlon,ad,bd,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ptrbd,ptrbv,ierror)
      call name(4hidvt)
      call iout(ierror,4hierr)
      call vecout(prtbd,4hprtd,nt)
      call vecout(prtbv,4hprtv,nt)

      else if (icase.eq.2) then

      call name(4h**es)

      call shaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shaes(nlat,nlon,isym,nt,dv,nlat,nlon,ad,bd,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call shaes(nlat,nlon,isym,nt,vt,nlat,nlon,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hidvi)
      call iout(ierror,4hierr)

      call idvtes(nlat,nlon,isym,nt,v,w,nlat,nlon,ad,bd,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ptrbd,ptrbv,ierror)
      call name(4hidvt)
      call iout(ierror,4hierr)
      call vecout(prtbd,4hprtd,nt)
      call vecout(prtbv,4hprtv,nt)

      else if (icase.eq.3) then

      call name(4h**gc)

      call shagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shagc(nlat,nlon,isym,nt,dv,nlat,nlon,ad,bd,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call shagc(nlat,nlon,isym,nt,vt,nlat,nlon,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hidvi)
      call iout(ierror,4hierr)

      call idvtgc(nlat,nlon,isym,nt,v,w,nlat,nlon,ad,bd,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ptrbd,ptrbv,ierror)
      call name(4hidvt)
      call iout(ierror,4hierr)
      call vecout(prtbd,4hprtd,nt)
      call vecout(prtbv,4hprtv,nt)

      else if (icase.eq.4) then

      call name(4h**gs)

      call shagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shags(nlat,nlon,isym,nt,dv,nlat,nlon,ad,bd,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call shags(nlat,nlon,isym,nt,vt,nlat,nlon,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)

      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hidvi)
      call iout(ierror,4hierr)

      call idvtgs(nlat,nlon,isym,nt,v,w,nlat,nlon,ad,bd,av,bv,
     +mdab,nlat,wsave,lsave,work,lwork,ptrbd,ptrbv,ierror)
      call name(4hidvt)
      call iout(ierror,4hierr)
      call vecout(prtbd,4hprtd,nt)
      call vecout(prtbv,4hprtv,nt)

      end if

c
c     compare this v,w with original derived from sf,sv
c
      err2v = 0.0
      err2w = 0.0
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    err2v = err2v + (v(i,j,k)-vsav(i,j,k))**2
	    err2w = err2w + (w(i,j,k)-wsav(i,j,k))**2
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
