c
c     4/97
c
c     a subroutine for testing all theta derivative subroutines
c     vtses,vtsec,vtsgs,vtsgc
c
c
c     (1) first set a valid vector field (v,w) in terms of x,y,z
c         cartesian coordinates
c
c     (2) analytically compute (vt,wt) from (1)
c
c     (3) compute (vt,wt) using vtses,vtsec,vtsgs,vtsgc and compare with (2)
c
      program tvts
c
c     set dimensions with parameter statements
c
      parameter(nnlat= 25,nnlon= 19, nnt = 3)
      parameter (lleng= 5*nnlat*nnlat*nnlon,llsav= 5*nnlat*nnlat*nnlon)
      dimension work(lleng),wsave(llsav)
      parameter (lldwork = 4*nnlat*nnlat )
      double precision dwork(lldwork)
      dimension br(nnlat,nnlat,nnt),bi(nnlat,nnlat,nnt)
      dimension cr(nnlat,nnlat,nnt),ci(nnlat,nnlat,nnt)
      dimension thetag(nnlat),dtheta(nnlat),dwts(nnlat)
      dimension v(nnlat,nnlon,nnt),w(nnlat,nnlon,nnt)
      dimension vt(nnlat,nnlon,nnt),wt(nnlat,nnlon,nnt)
      dimension vtsav(nnlat,nnlon,nnt),wtsav(nnlat,nnlon,nnt)
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
c     test all theta derivative subroutines
c
      do icase=1,4
c
c     icase=1 test vtsec
c     icase=2 test vtses
c     icase=3 test vtsgc
c     icase=4 test vtsgs
c
      call name(4h****)
      call name(4h****)
      call iout(icase,4hicas)
c
c
c     set vector field v,w and compute theta derivatives in (vtsav,wtsav)
c
      do k=1,nt
	do j=1,nlon
	  phi = (j-1)*dphi
	  sinp = sin(phi)
	  cosp = cos(phi)
	  do i=1,nlat
	    theta = (i-1)*dlat
	    if (icase.eq.3 .or. icase.eq.4) theta = thetag(i)
	    cost = cos(theta)
	    sint = sin(theta)
c
c    set x,y,z and their theta derivatives at colatitude theta and longitude p
c
	    x = sint*cosp
	    dxdt = cost*cosp
	    y = sint*sinp
	    dydt = cost*sinp
	    z = cost
	    dzdt = -sint
c
c     set (v,w) field corresponding to stream function
c     S = exp(y)+exp(-z) and velocity potential function
c     P = exp(x)+exp(z)
c
	      ex = exp(x)
	      ey = exp(y)
	      ez = exp(z)
	      emz = exp(-z)
	      w(i,j,k) =-ex*sinp+emz*sint+ey*cost*sinp
	      v(i,j,k) =-ey*cosp-ez*sint+ex*cost*cosp
c
c     set theta derivatives differentiating w,v above
c
	     wtsav(i,j,k) = -ex*dxdt*sinp+emz*(-dzdt*sint+cost)
     +                      +ey*sinp*(dydt*cost-sint)
	     vtsav(i,j,k) = -ey*dydt*cosp-ez*(dzdt*sint+cost)
     +                      +ex*cosp*(dxdt*cost-sint)
	  end do
	end do
      end do

c     call a3out(wtsav,4hwtsv,nlat,nlon,nt)
c     call a3out(vtsav,4hvtsv,nlat,nlon,nt)



      if (icase.eq.1) then

      call name(4h**ec)

      call vhaeci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaec(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

c
c     now compute theta derivatives of v,w
c
      call vtseci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)

      call name(4hvtsi)
      call iout(ierror,4hierr)

      call vtsec(nlat,nlon,ityp,nt,vt,wt,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvts )
      call iout(ierror,4hierr)

      else if (icase.eq.2) then

      call name(4h**es)

      call vhaesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhaes(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vtsesi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvtsi)
      call iout(ierror,4hierr)

      call vtses(nlat,nlon,ityp,nt,vt,wt,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvts )
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

c
c     now synthesize v,w from br,bi,cr,ci and compare with original
c
      call vtsgci(nlat,nlon,wsave,lsave,dwork,ldwork,ierror)
      call name(4hvtsi)
      call iout(ierror,4hierr)

      call vtsgc(nlat,nlon,ityp,nt,vt,wt,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvts )
      call iout(ierror,4hierr)

      else if (icase.eq.4) then

      call name(4h**gs)
      call vhagsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvhai)
      call iout(ierror,4hierr)

      call vhags(nlat,nlon,ityp,nt,v,w,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvha )
      call iout(ierror,4hierr)

      call vtsgsi(nlat,nlon,wsave,lsave,work,lwork,dwork,ldwork,ierror)
      call name(4hvtsi)
      call iout(ierror,4hierr)

      call vtsgs(nlat,nlon,ityp,nt,vt,wt,nlat,nlon,br,bi,cr,ci,nlat,
     +nlat,wsave,lsave,work,lwork,ierror)
      call name(4hvts )
      call iout(ierror,4hierr)

      end if

c     call a3out(wt,4hwt  ,nlat,nlon,nt)
c     call a3out(vt,4hvt  ,nlat,nlon,nt)

c
c     compute "error" in vt,wt
c
      err2v = 0.0
      err2w = 0.0
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    err2v = err2v + (vt(i,j,k) - vtsav(i,j,k))**2
	    err2w = err2w + (wt(i,j,k) - wtsav(i,j,k))**2
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




