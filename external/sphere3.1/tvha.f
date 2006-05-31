c
c     11/96
c
c     a subroutine for testing all vector analysis and synthesis subroutines
c
c     (1) first a scalar stream function and a scalar velocity potential function
c         are set in st,sv by restricting polys in x,y,z to the sphere surface
c
c     (2) the vector vield (v,w) is set by analytically differenting the scalar fields in (1)
c         using the standard formula relating a vector field and the stream and velocity
c         potential scalar fields in colatitude X longitude spherical coordinates
c
c          v = -1/sin(theta)*d(st)/dphi + d(sv)/dtheta
c
c          w =  1/sin(theta)*d(sv)/dphi + d(st)/dtheta
c
c     (3) a vector analysis is performed on (v,w)
c
c     (4) a vector synthesis is performed using coeffs from (3)
c
c     (5) the synthesized vector field from (4) is compared with the vector field from (2)
c
c     note:  vhaec,vhaes,vhagc,vhags,vhsec,vhses,vhsgc,vhsgs are all tested!!
c
      program tvha
c
c     set dimensions with parameter statements
c
      parameter(nnlat= 25,nnlon= 19, nnt = 2)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav= 5*nnlat*nnlat*nnlon)
      parameter (lldwork = 4*nnlat*nnlat )
      double precision dwork(lldwork)
      dimension work(lleng),wsave(llsav)
      dimension br(nnlat,nnlat,nnt),bi(nnlat,nnlat,nnt)
      dimension cr(nnlat,nnlat,nnt),ci(nnlat,nnlat,nnt)
      dimension st(nnlat,nnlon,nnt),sv(nnlat,nnlon,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      dimension v(nnlat,nnlon,nnt),w(nnlat,nnlon,nnt)
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
c     test all analysis and synthesis subroutines
c
      do icase=1,4
c
c     icase=1 test vhaec,vhsec
c     icase=2 test vhaes,vhses
c     icase=3 test vhagc,vhsgc
c     icase=4 test vhags,vhsgs
c
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
c           if (icase.gt.2) theta=thetag(i)
	    if (icase.eq.3 .or. icase.eq.4) theta = thetag(i)
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
	    st(i,j,k) = x*y
	    sv(i,j,k) = y*z
	    dstdt = x*dydt+y*dxdt
	    dstdp = x*dydp+y*dxdp
	    dsvdt = y*dzdt+z*dydt
	    dsvdp = y*dzdp+z*dydp
	    v(i,j,k) = -(cosp*dydp+sinp*dxdp) + dsvdt
	    w(i,j,k) = sinp*dzdp + cost*cosp + dstdt
	    else if (k.eq.2) then
	       st(i,j,k) = x*z
	       sv(i,j,k) = x*y
	       dstdp = x*dzdp+z*dxdp
	       dstdt = x*dzdt+z*dxdt
	       dsvdp = x*dydp+y*dxdp
	       dsvdt = x*dydt+y*dxdt
c
c          v = -1/sin(theta)*d(st)/dphi + d(sv)/dtheta
c
c          w =  1/sin(theta)*d(sv)/dphi + d(st)/dtheta
c
	       v(i,j,k) = z*sinp + dsvdt
	       w(i,j,k) = cosp*dydp+ sinp*dxdp + dstdt
	    end if
	  end do
	end do
      end do

c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(v(1,1,kk),4h   v,nlat,nlon)
c     call aout(w(1,1,kk),4h   w,nlat,nlon)
c     end do

      if (icase.eq.1) then

      call name(4h**ec)

      call vhaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     call aout(br,4h  br,nlat,nlat)
c     call aout(bi,4h  bi,nlat,nlat)
c     call aout(cr,4h  cr,nlat,nlat)
c     call aout(ci,4h  ci,nlat,nlat)

c
c     now synthesize v,w from br,bi,cr,ci and compare with original
c
      call vhseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsec(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhs )
      call iout(ierror,4hierr)

c     call aout(v,4h   v,nlat,nlon)
c     call aout(w,4h   w,nlat,nlon)

      else if (icase.eq.2) then

      call name(4h**es)

      call vhaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaes(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)

      call name(4hvha )
      call iout(ierror,4hierr)

c     call aout(br,4h  br,nlat,nlat)
c     call aout(bi,4h  bi,nlat,nlat)
c     call aout(cr,4h  cr,nlat,nlat)
c     call aout(ci,4h  ci,nlat,nlat)

c
c     now synthesize v,w from br,bi,cr,ci and compare with original
c
      call vhsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhses(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)

      call name(4hvhs )
      call iout(ierror,4hierr)

      else if (icase.eq.3) then

      call name(4h**gc)

      call name(4hvhgi)
      call iout(nlat,4hnlat)

      call vhagci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhagc(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     call aout(br,4h  br,nlat,nlat)
c     call aout(bi,4h  bi,nlat,nlat)
c     call aout(cr,4h  cr,nlat,nlat)
c     call aout(ci,4h  ci,nlat,nlat)

c
c     now synthesize v,w from br,bi,cr,ci and compare with original
c
      call vhsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsgc(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhs )
      call iout(ierror,4hierr)

c     call aout(v,4h   v,nlat,nlon)
c     call aout(w,4h   w,nlat,nlon)
c     call exit(0)

c
c **** problem with vhags.f, function indx not defined!!!! talk to Paul
c

      else if (icase.eq.4) then

      call name(4h**gs)

      call vhagsi(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhags(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c     call aout(br,4h  br,nlat,nlat)
c     call aout(bi,4h  bi,nlat,nlat)
c     call aout(cr,4h  cr,nlat,nlat)
c     call aout(ci,4h  ci,nlat,nlat)


c
c     now synthesize v,w from br,bi,cr,ci and compare with original
c
      call vhsgsi(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhsi)
      call iout(ierror,4hierr)

      call vhsgs(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvhs )
      call iout(ierror,4hierr)

      end if


c     do kk=1,nt
c     call iout(kk,4h**kk)
c     call aout(v(1,1,kk),4h   v,nlat,nlon)
c     call aout(w(1,1,kk),4h   w,nlat,nlon)
c     end do

c
c     compute "error" in v,w
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
	    st(i,j,k) = x*y
	    sv(i,j,k) = y*z
	    dstdt = x*dydt+y*dxdt
	    dstdp = x*dydp+y*dxdp
	    dsvdt = y*dzdt+z*dydt
	    dsvdp = y*dzdp+z*dydp
	    ve = -(cosp*dydp+sinp*dxdp) + dsvdt
	    we = sinp*dzdp + cost*cosp + dstdt
	    else if (k.eq.2) then
	       st(i,j,k) = x*z
	       sv(i,j,k) = x*y
	       dstdp = x*dzdp+z*dxdp
	       dstdt = x*dzdt+z*dxdt
	       dsvdp =  x*dydp+y*dxdp
	       dsvdt = x*dydt+y*dxdt
	       ve = z*sinp + dsvdt
	       we = cosp*dydp+ sinp*dxdp + dstdt
	    end if
	    err2v = err2v + (v(i,j,k) - ve)**2
	    err2w = err2w + (w(i,j,k) - we)**2
	  end do
	end do
      end do
c
c     set and print least squares error in v,w
c
      err2v = sqrt(err2v/(nt*nlat*nlon))
      err2w = sqrt(err2w/(nt*nlat*nlon))
      call vout(err2v,4herrv)
      call vout(err2w,4herrw)
c
c     end of icase loop
c
      end do
      end
