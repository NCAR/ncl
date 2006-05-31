c
c     6/98
c
c     a subroutine for testing all vorticity and ivnerse vorticity routines
c
c
c     (1) first set a stream function and velocity potential scalar fields as
c         polys in x,y,z restricted to the sphere
c
c     (2) derive a vector field (v,w) from (1)
c
c     (3) compute the vorticity vt of (2) and compare with the vorticity
c         computed analytically
c
c     (4) compute vector field (ve,we) using br,bi,cr,ci from (v,w) with
c         br=bi=0.0
c
c     (5) invert the vorticity in (3) and compare with (4)
c

      program tvrt
c
c     set dimensions with parameter statements
c
      parameter(nnlat= 24,nnlon= 14, nnt = 3)
      parameter (mmdab = (nnlon+2)/2, mmdc = (nnlon+1)/2)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav=5*nnlat*nnlat*nnlon)
      dimension work(lleng),wsave(llsav)
      parameter (lldwork = 4*nnlat*nnlat)
      dimension dwork(lldwork)
      dimension br(mmdc,nnlat,nnt),bi(mmdc,nnlat,nnt)
      dimension cr(mmdc,nnlat,nnt),ci(mmdc,nnlat,nnt)
      dimension a(mmdab,nnlat,nnt),b(mmdab,nnlat,nnt)
      dimension vt(nnlat,nnlon,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      dimension v(nnlat,nnlon,nnt),w(nnlat,nnlon,nnt)
      dimension ve(nnlat,nnlon,nnt),we(nnlat,nnlon,nnt)
      dimension pertrb(nnt)
      double precision dtheta, dwts
c
c     set dimension variables
c
      nlat = nnlat
      nlon = nnlon
      nmax = max0(nlat,nlon)
      mdab = mmdab
      mdc = mmdc
      lwork = lleng
      lsave = llsav
      ldwork = lldwork
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
      call gaqd(nlat,dtheta,dwts,dwork,ldwork,ier)
      do  i=1,nlat
	thetag(i) = dtheta(i)
      end do
      call name(4hgaqd)
      call iout(ier,4h ier)
      call vecout(thetag,4hthtg,nlat)
c
c     test all vorticity subroutines
c
      do icase=1,4
      call name(4h****)
      call name(4h****)
      call iout(icase,4hicas)
c
c
c     set scalar stream and velocity potential fields as polys in x,y,z
c     and then set v,w from st,sv scalar fields
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
	    dxdt = cost*cosp
	    dxdp = -sint*sinp
	    dydt = cost*sinp
	    dydp = sint*cosp
	    dzdt = -sint
	    dzdp = 0.0
	    if (k.eq.1) then
c              st(i,j,k) = x
c              sv(i,j,k) = y
c
c          v = -1/sin(theta)*dstdp + dsvdt
c
c          w =  1/sin(theta)*dsvdp + dstdt
c
	       v(i,j,k) = sinp + cost*sinp
	       w(i,j,k) = cosp + cost*cosp
	       vt(i,j,k) = -2.0*sint*cosp
	    else if (k.eq.2) then
C              st = y
c              sv = z
	       v(i,j,k) = -cosp-sint
	       w(i,j,k) = cost*sinp
c         sint*vt = -dvdp + sint*dwdt + cost*w
c                 = sinp + sint*(-sint*sinp)+cost*cost*sinp
c                 = sinp + (cost**2-sint**2)*sinp
	       vt(i,j,k) = -2.*sint*sinp
	    else if (k.eq.3) then
c           st = x
c           sv = z
	    v(i,j,k) = sinp - sint
	    w(i,j,k) = cost*cosp
c     sint*vt = -cosp-sint*sint*sinp+cost*cost*cosp
c             = -cosp + (1-2.*sint**2)*cosp =
	    vt(i,j,k) = -2.*sint*cosp
	    end if
	  end do
	end do
      end do

c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(v(1,1,kk),4h   v,nlat,nlon)
c     call aout(w(1,1,kk),4h   w,nlat,nlon)
c     call aout(vt(1,1,kk),4h  vt,nlat,nlon)
c     end do

      if (icase.eq.1) then

      call name(4h**ec)
c
c     analyze vector field
c
      call vhaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,isym,nt,v,w,nlat,nlon,br,bi,cr,ci,mdc,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(br(1,1,kk),4h  br,mdc,nlat)
c     call aout(bi(1,1,kk),4h  bi,mdc,nlat)
c     call aout(cr(1,1,kk),4h  cr,mdc,nlat)
c     call aout(ci(1,1,kk),4h  ci,mdc,nlat)
c     end do
c     end if
c
c     compute vorticity of (v,w) in vt
c

      call shseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hvrti)
      call iout(ierror,4hierr)

      call vrtec(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdc,nlat,
     +wsave,lsave,work,lwork,ierror)

      call name(4hvrt )
      call iout(ierror,4hierr)
      call iout(nlat,4hnlat)
      call iout(nlon,4hnlon)

      else if (icase.eq.2) then

      call name(4h**es)
      call shsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)

      call name(4hvrti)
      call iout(ierror,4hierr)

      call vrtes(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdc,nlat,
     +wsave,lsave,work,lwork,ierror)

      call name(4hvrt )
      call iout(ierror,4hierr)

      else if (icase .eq. 3) then

      call name(4h**gc)

      call shsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hvrti)
      call iout(ierror,4hierr)

      call vrtgc(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdc,nlat,
     +wsave,lsave,work,lwork,ierror)

      call name(4hvrt )
      call iout(ierror,4hierr)

      else if (icase .eq. 4) then

      call name(4h**gs)

      call shsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)

      call name(4hvrti)
      call iout(ierror,4hierr)

      call vrtgs(nlat,nlon,isym,nt,vt,nlat,nlon,cr,ci,mdc,nlat,
     +wsave,lsave,work,lwork,ierror)

      call name(4hvrt )
      call iout(ierror,4hierr)
      end if

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(vt(1,1,kk),4h  vt,nlat,nlon)
c     end do
c     end if
c
c     compute "error" in vt
c
      err2 = 0.0
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
	    dxdt = cost*cosp
	    d2xdt2 = -sint*cosp
	    dxdp = -sint*sinp
	    d2xdp2 = -sint*cosp
	    dydt = cost*sinp
	    d2ydt2 = -sint*sinp
	    dydp = sint*cosp
	    d2ydp2 = -sint*sinp
	    dzdt = -sint
	    d2zdt2 = -cost
	    dzdp = 0.0
	    d2zdp2 = 0.0
	    if (k.eq.1) then
	       vte = -2.0*sint*cosp
	    else if (k.eq.2) then
	       vte = -2.*sint*sinp
	    else if (k.eq.3) then
	    vte = -2.*sint*cosp
	    end if
	       err2 = err2 + (vt(i,j,k)-vte)**2
	  end do
	end do
      end do
      err2 = sqrt(err2/(nt*nlat*nlon))
      call vout(err2,4herr2)
c
c     now recompute (v,w) inverting vt using ivrt(ec,es,gc,gs)
c     and compare with (ve,we) generated by synthesizing br,bi,cr,ci
c     with br=bi=0.0
c

      do k=1,nt
	do i=1,mdc
	  do j=1,nlat
	    br(i,j,k) = 0.0
	    bi(i,j,k) = 0.0
	  end do
	end do
      end do

      if (icase.eq.1) then

      call name(4h**ec)

c
c     set vector field (ve,we) with br=bi=0.0 for comparison with inverted vt
c
      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsec(nlat,nlon,ityp,nt,ve,we,nlat,nlon,br,bi,cr,ci,
     +           mdc,nlat,wsave,lsave,work,lwork,ierror)

      call name(4hvhs )
      call iout(ierror,4hierr)

      call shaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shaec(nlat,nlon,isym,nt,vt,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(a(1,1,kk),4h   a,nlat,nlat)
c     call aout(b(1,1,kk),4h   b,nlat,nlat)
c     end do
c     end if

      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call ivrtec(nlat,nlon,isym,nt,v,w,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,pertrb,ierror)
      call name(4hivrt)
      call iout(ierror,4hierr)
      call vout(pertrb,4hprtb)

      else if (icase.eq.2) then

      call name(4h**es)
c
c     set vector field (ve,we) with br=bi=0.0 for comparison with inverted vt
c
      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhses(nlat,nlon,ityp,nt,ve,we,nlat,nlon,br,bi,cr,ci,
     +           mdc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhs )
      call iout(ierror,4hierr)


      call shaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shaes(nlat,nlon,isym,nt,vt,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hivti)
      call iout(ierror,4hierr)

      call ivrtes(nlat,nlon,isym,nt,v,w,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,pertrb,ierror)
      call name(4hivrt)
      call iout(ierror,4hierr)
      call vout(pertrb,4hprtb)

      else if (icase.eq.3) then

      call name(4h**gc)
c
c     set vector field (ve,we) with br=bi=0.0 for comparison with inverted vt
c
      call vhsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsgc(nlat,nlon,ityp,nt,ve,we,nlat,nlon,br,bi,cr,ci,
     +           mdc,nlat,wsave,lsave,work,lwork,ierror)

      call name(4hvhs )
      call iout(ierror,4hierr)

      call shagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shagc(nlat,nlon,isym,nt,vt,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hivti)
      call iout(ierror,4hierr)

      call ivrtgc(nlat,nlon,isym,nt,v,w,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,pertrb,ierror)

      call name(4hivrt)
      call iout(ierror,4hierr)
      call vout(pertrb,4hprtb)

      else if (icase.eq.4) then

      call name(4h**gs)
c
c     set vector field (ve,we) with br=bi=0.0 for comparison with inverted vt
c
      call vhsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsgs(nlat,nlon,ityp,nt,ve,we,nlat,nlon,br,bi,cr,ci,
     +           mdc,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhs )
      call iout(ierror,4hierr)

      call shagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hshai)
      call iout(ierror,4hierr)

      call shags(nlat,nlon,isym,nt,vt,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,ierror)
      call name(4hsha )
      call iout(ierror,4hierr)
      call iout(lsave,4hlsav)
      call iout(lwork,4hlwrk)

      call vhsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hivti)
      call iout(ierror,4hierr)

      call ivrtgs(nlat,nlon,isym,nt,v,w,nlat,nlon,a,b,
     +mdab,nlat,wsave,lsave,work,lwork,pertrb,ierror)
      call name(4hivrt)
      call iout(ierror,4hierr)
      call vout(pertrb,4hprtb)

      end if


c     if (nmax.lt.10) then
c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(v(1,1,kk),4h   v,nlat,nlon)
c     call aout(w(1,1,kk),4h   w,nlat,nlon)
c     end do
c     end if

c
c     compare this v,w with ve,we
c
      err2v = 0.0
      err2w = 0.0
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    err2v = err2v + (v(i,j,k)-ve(i,j,k))**2
	    err2w = err2w + (w(i,j,k)-we(i,j,k))**2
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
