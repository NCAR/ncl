c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file sphcom.f
c
c     this file must be loaded with all driver level files
c     in spherepack3.0.  it includes undocumented subroutines
c     called by some or all of the drivers
c
      subroutine dnlfk (m,n,cp)
c
c     cp requires n/2+1 double precision locations
c
      double precision cp,fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,
     1       t1,t2,pm1,sc10,sc20,sc40
      dimension       cp(1)
      parameter (sc10=1024.d0)
      parameter (sc20=sc10*sc10)
      parameter (sc40=sc20*sc20)
c
      cp(1) = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if(n-1) 2,3,5
    2 cp(1) = dsqrt(2.d0)
      return
    3 if(ma .ne. 0) go to 4
      cp(1) = dsqrt(1.5d0)
      return
    4 cp(1) = dsqrt(.75d0)
      if(m .eq. -1) cp(1) = -cp(1)
      return
    5 if(mod(n+ma,2) .ne. 0) go to 10
      nmms2 = (n-ma)/2
      fnum = n+ma+1
      fnmh = n-ma+1
      pm1 = 1.d0
      go to 15
   10 nmms2 = (n-ma-1)/2
      fnum = n+ma+2
      fnmh = n-ma+2
      pm1 = -1.d0
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
 15   t1 = 1.d0/sc20
      nex = 20
      fden = 2.d0
      if(nmms2 .lt. 1) go to 20
      do 18 i=1,nmms2
      t1 = fnum*t1/fden
      if(t1 .gt. sc20) then
      t1 = t1/sc40
      nex = nex+40
      end if
      fnum = fnum+2.
      fden = fden+2.
   18 continue
   20 t1 = t1/2.d0**(n-1-nex)
      if(mod(ma/2,2) .ne. 0) t1 = -t1
      t2 = 1. 
      if(ma .eq. 0) go to 26
      do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
   25 continue
   26 cp2 = t1*dsqrt((n+.5d0)*t2)
      fnnp1 = n*(n+1)
      fnmsq = fnnp1-2.d0*ma*ma
      l = (n+1)/2
      if(mod(n,2) .eq. 0 .and. mod(ma,2) .eq. 0) l = l+1
      cp(l) = cp2
      if(m .ge. 0) go to 29
      if(mod(ma,2) .ne. 0) cp(l) = -cp(l)
   29 if(l .le. 1) return
      fk = n
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = 2.*(fk*fk-fnmsq)
      cp(l-1) = b1*cp(l)/a1
   30 l = l-1
      if(l .le. 1) return
      fk = fk-2.
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = -2.*(fk*fk-fnmsq)
      c1 = (fk+1.)*(fk+2.)-fnnp1
      cp(l-1) = -(b1*cp(l)+c1*cp(l+1))/a1
      go to 30
      end
      subroutine dnlft (m,n,theta,cp,pb)
      double precision cp(*),pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(m,2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = .5*cp(1)
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb+cp(k+1)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb+cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
      subroutine dnlftd (m,n,theta,cp,pb)
c
c     computes the derivative of pmn(theta) with respect to theta
c
      dimension cp(1)
      double precision cp,pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(abs(m),2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = 0.d0
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb-2.d0*k*cp(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+2.d0*k*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb-(2.d0*k-1)*cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+(2.d0*k-1)*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
      subroutine legin(mode,l,nlat,m,w,pmn,km)
c     this subroutine computes legendre polynomials for n=m,...,l-1
c     and  i=1,...,late (late=((nlat+mod(nlat,2))/2)gaussian grid
c     in pmn(n+1,i,km) using swarztrauber's recursion formula.
c     the vector w contains quantities precomputed in shigc.
c     legin must be called in the order m=0,1,...,l-1
c     (e.g., if m=10 is sought it must be preceded by calls with
c     m=0,1,2,...,9 in that order)
      dimension w(1),pmn(1)
c     set size of pole to equator gaussian grid
      late = (nlat+mod(nlat,2))/2
c     partition w (set pointers for p0n,p1n,abel,bbel,cbel,pmn)
      i1 = 1+nlat
      i2 = i1+nlat*late
      i3 = i2+nlat*late
      i4 = i3+(2*nlat-l)*(l-1)/2
      i5 = i4+(2*nlat-l)*(l-1)/2
      call legin1(mode,l,nlat,late,m,w(i1),w(i2),w(i3),w(i4),
     1            w(i5),pmn,km)
      return
      end
      subroutine legin1(mode,l,nlat,late,m,p0n,p1n,abel,bbel,cbel,
     1                  pmn,km)
      dimension p0n(nlat,late),p1n(nlat,late)
      dimension abel(1),bbel(1),cbel(1),pmn(nlat,late,3)
      data km0,km1,km2/ 1,2,3/
      save km0,km1,km2
c     define index function used in storing triangular
c     arrays for recursion coefficients (functions of (m,n))
c     for 2.le.m.le.n-1 and 2.le.n.le.l-1
      indx(m,n) = (n-1)*(n-2)/2+m-1
c     for l.le.n.le.nlat and 2.le.m.le.l
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1

c     set do loop indices for full or half sphere
      ms = m+1
      ninc = 1
      if (mode.eq.1) then
c     only compute pmn for n-m odd
      ms = m+2
      ninc = 2
      else if (mode.eq.2) then
c     only compute pmn for n-m even
      ms = m+1
      ninc = 2
      end if


      if (m.gt.1) then
      do 100 np1=ms,nlat,ninc
      n = np1-1
      imn = indx(m,n)
      if (n.ge.l) imn = imndx(m,n)
      do 100 i=1,late
      pmn(np1,i,km0) = abel(imn)*pmn(n-1,i,km2)
     1            +bbel(imn)*pmn(n-1,i,km0)
     2            -cbel(imn)*pmn(np1,i,km2)
  100 continue

      else if (m.eq.0) then
      do 101 np1=ms,nlat,ninc
      do 101 i=1,late
      pmn(np1,i,km0) = p0n(np1,i)
  101 continue

      else if (m.eq.1) then
      do 102 np1=ms,nlat,ninc
      do 102 i=1,late
      pmn(np1,i,km0) = p1n(np1,i)
  102 continue
      end if

c     permute column indices
c     km0,km1,km2 store m,m-1,m-2 columns
      kmt = km0
      km0 = km2
      km2 = km1
      km1 = kmt
c     set current m index in output param km
      km = kmt
      return
      end


      subroutine zfin (isym,nlat,nlon,m,z,i3,wzfin)
      dimension       z(1)        ,wzfin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzfin is 2*lim+3*labc
c
      call zfin1 (isym,nlat,m,z,imid,i3,wzfin,wzfin(iw1),wzfin(iw2),
     1            wzfin(iw3),wzfin(iw4))
      return
      end
      subroutine zfin1 (isym,nlat,m,z,imid,i3,zz,z1,a,b,c)
      dimension       z(imid,nlat,3),zz(imid,1),z1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      z(i,np1,i3) = zz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      z(i,np1,i3) = z1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(isym .eq. 1) go to 36
      do 85 i=1,imid
      z(i,m+1,i3) = a(ns)*z(i,m-1,i1)-c(ns)*z(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(isym .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      z(i,m+2,i3) = a(ns)*z(i,m,i1)-c(ns)*z(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(isym .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(isym .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      z(i,np1,i3) = a(ns)*z(i,np1-2,i1)+b(ns)*z(i,np1-2,i3)
     1                              -c(ns)*z(i,np1,i1)
   75 continue
   80 return
      end
      subroutine zfinit (nlat,nlon,wzfin,dwork)
      dimension       wzfin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzfin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of dwork is nlat+2
c
      call zfini1 (nlat,nlon,imid,wzfin,wzfin(iw1),dwork,
     1                                       dwork(nlat/2+1))
      return
      end
      subroutine zfini1 (nlat,nlon,imid,z,abc,cz,work)
c
c     abc must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations
c     where mmax = min0(nlat,nlon/2+1)
c     cz and work must each have nlat+1 locations
c
      dimension z(imid,nlat,2),abc(1)
      double precision pi,dt,th,zh,cz(*),work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dnzfk(nlat,m,n,cz,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dnzft(nlat,m,n,th,cz,zh)
      z(i,np1,mp1) = zh
  165 continue
      z(1,np1,mp1) = .5*z(1,np1,mp1)
  160 continue
      call rabcp(nlat,nlon,abc)
      return
      end
      subroutine dnzfk(nlat,m,n,cz,work)
c
c     dnzfk computes the coefficients in the trigonometric
c     expansion of the z functions that are used in spherical
c     harmonic analysis.
c
      dimension  cz(1),work(1)
c
c     cz and work must both have nlat/2+1 locations
c
      double precision sum,sc1,t1,t2,work,cz
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dnlfk(m,n,work)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo = n/2+1
      do 5 idx=1,lc
      i = idx+idx-2
      sum = work(1)/(1.d0-i*i)
      if(kdo.lt.2) go to 29
      do 6 kp1=2,kdo
      k = kp1-1
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
8     sum = sum+work(kp1)*(t1+t2)/(t1*t2)
6     continue
29    cz(idx) = sc1*sum
5     continue
      return
c
c     n even, m odd
c
4     kdo = n/2
      do 9 idx=1,lc
      i = idx+idx-2
      sum = 0.
      do 101 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
12    sum=sum+work(k)*(t1-t2)/(t1*t2)
101   continue
      cz(idx) = sc1*sum
9     continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      do 15 idx=1,lc
      i = idx+idx-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
18    sum=sum+work(k)*(t1+t2)/(t1*t2)
16    continue
      cz(idx)=sc1*sum
15    continue
      return
c
c     n odd, m odd
c
14    kdo = (n+1)/2
      do 19 idx=1,lc
      i = idx+idx-3
      sum=0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
22    sum=sum+work(k)*(t1-t2)/(t1*t2)
20    continue
      cz(idx)=sc1*sum
19    continue
      return
      end
      subroutine dnzft(nlat,m,n,th,cz,zh)
      dimension cz(1)
      double precision cz,zh,th,cdt,sdt,cth,sth,chh
      zh = 0.
      cdt = dcos(th+th)
      sdt = dsin(th+th)
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod)20,20,10
   10 lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      if(nmod)1,1,2
    1 if(mmod)3,3,4
c
c     nlat odd n even m even
c
    3 zh = .5*(cz(1)+cz(lc)*dcos(2*lq*th))
      cth = cdt
      sth = sdt
      do 201 k=2,lq
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  201 continue
      return
c
c     nlat odd n even m odd
c
    4 cth = cdt
      sth = sdt
      do 202 k=1,ls
c     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  202 continue
      return
c
c     nlat odd n odd, m even
c
    2 if(mmod)5,5,6
    5 cth = dcos(th)
      sth = dsin(th)
      do 203 k=1,lq
c     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  203 continue
      return
c
c     nlat odd n odd m odd
c
    6 cth = dcos(th)
      sth = dsin(th)
      do 204 k=1,lq
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  204 continue
      return
   20 lc = nlat/2
      lq = lc-1
      if(nmod)30,30,80
   30 if(mmod)40,40,60
c
c     nlat even n even m even
c
   40 zh = .5*cz(1)
      cth = cdt
      sth = sdt
      do 50 k=2,lc
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   50 continue
      return
c
c     nlat even n even m odd
c
   60 cth = cdt
      sth = sdt
      do 70 k=1,lq
c     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   70 continue
      return
c
c     nlat even n odd m even
c
   80 if(mmod)90,90,110
   90 zh = .5*cz(lc)*dcos((nlat-1)*th)
      cth = dcos(th)
      sth = dsin(th)
      do 100 k=1,lq
c     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  100 continue
      return
c
c     nlat even n odd m odd
c
  110 cth = dcos(th)
      sth = dsin(th)
      do 120 k=1,lq
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  120 continue
      return
      end
      subroutine alin (isym,nlat,nlon,m,p,i3,walin)
      dimension       p(1)        ,walin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of walin is ((5*l-7)*l+6)/2
c
      call alin1 (isym,nlat,m,p,imid,i3,walin,walin(iw1),walin(iw2),
     1            walin(iw3),walin(iw4))
      return
      end
      subroutine alin1 (isym,nlat,m,p,imid,i3,pz,p1,a,b,c)
      dimension       p(imid,nlat,3),pz(imid,1),p1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      p(i,np1,i3) = pz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      p(i,np1,i3) = p1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(isym .eq. 1) go to 36
      do 85 i=1,imid
      p(i,m+1,i3) = a(ns)*p(i,m-1,i1)-c(ns)*p(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(isym .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      p(i,m+2,i3) = a(ns)*p(i,m,i1)-c(ns)*p(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(isym .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(isym .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      p(i,np1,i3) = a(ns)*p(i,np1-2,i1)+b(ns)*p(i,np1-2,i3)
     1                              -c(ns)*p(i,np1,i1)
   75 continue
   80 return
      end
      subroutine alinit (nlat,nlon,walin,dwork)
      dimension       walin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of walin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of work is nlat+1
c
      call alini1 (nlat,nlon,imid,walin,walin(iw1),dwork)
      return
      end
      subroutine alini1 (nlat,nlon,imid,p,abc,cp)
      dimension p(imid,nlat,2),abc(1),cp(1)
      double precision pi,dt,th,cp,ph
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dnlfk (m,n,cp)
      do 160 i=1,imid
      th = (i-1)*dt
      call dnlft (m,n,th,cp,ph)
      p(i,np1,mp1) = ph
  160 continue
      call rabcp(nlat,nlon,abc)
      return
      end
      subroutine rabcp(nlat,nlon,abc)
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the associated legendre fuctions. array abc
c     must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations.
c
      dimension abc(1)
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcp1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcp1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing pbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      dimension a(1),b(1),c(1)
      mmax = min0(nlat,nlon/2+1)
      do 215 mp1=3,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      a(ns) = sqrt((tm+1.)*(tm-2.)/temp)
      c(ns) = sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      a(ns) = sqrt((tm+3.)*(tm-2.)/temp)
      c(ns) = sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      a(ns) = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(cn*fnmm*(fnmm-1.)/temp)
      c(ns) = sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine sea1(nlat,nlon,imid,z,idz,zin,wzfin,dwork)
      dimension z(idz,*),zin(imid,nlat,3),wzfin(*)
      double precision dwork(*)
      call zfinit(nlat,nlon,wzfin,dwork)
      mmax = min0(nlat,nlon/2+1)
      do 33 mp1=1,mmax
      m = mp1-1
      call zfin (0,nlat,nlon,m,zin,i3,wzfin)
      do 33 np1=mp1,nlat
      mn = m*(nlat-1)-(m*(m-1))/2+np1
      do 33 i=1,imid
      z(mn,i) = zin(i,np1,i3)
   33 continue
      return
      end
      subroutine ses1(nlat,nlon,imid,p,pin,walin,dwork)
      dimension p(imid,*),pin(imid,nlat,3),walin(*)
      double precision dwork(*)
      call alinit (nlat,nlon,walin,dwork)
      mmax = min0(nlat,nlon/2+1)
      do 10 mp1=1,mmax
      m = mp1-1
      call alin(0,nlat,nlon,m,pin,i3,walin)
      do 10 np1=mp1,nlat
      mn = m*(nlat-1)-(m*(m-1))/2+np1
      do 10 i=1,imid
      p(i,mn) = pin(i,np1,i3)
   10 continue
      return
      end
      subroutine zvinit (nlat,nlon,wzvin,dwork)
      dimension       wzvin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzvin is 
c         2*nlat*imid +3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     the length of dwork is nlat+2
c
      call zvini1 (nlat,nlon,imid,wzvin,wzvin(iw1),dwork,
     1                                    dwork(nlat/2+2))
      return
      end
      subroutine zvini1 (nlat,nlon,imid,zv,abc,czv,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czv and work must each have nlat/2+1  locations
c
      dimension zv(imid,nlat,2),abc(1)
      double precision pi,dt,czv(1),zvh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dzvk(nlat,m,n,czv,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dzvt(nlat,m,n,th,czv,zvh)
      zv(i,np1,mp1) = zvh
  165 continue
      zv(1,np1,mp1) = .5*zv(1,np1,mp1)
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine zwinit (nlat,nlon,wzwin,dwork)
      dimension       wzwin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzvin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call zwini1 (nlat,nlon,imid,wzwin,wzwin(iw1),dwork,
     1                                        dwork(nlat/2+2))
      return
      end
      subroutine zwini1 (nlat,nlon,imid,zw,abc,czw,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czw and work must each have nlat+1 locations
c
      dimension zw(imid,nlat,2),abc(1)
      double precision  pi,dt,czw(1),zwh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dzwk(nlat,m,n,czw,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dzwt(nlat,m,n,th,czw,zwh)
      zw(i,np1,m) = zwh
  165 continue
      zw(1,np1,m) = .5*zw(1,np1,m)
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine zvin (ityp,nlat,nlon,m,zv,i3,wzvin)
      dimension       zv(1)        ,wzvin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzvin is 2*lim+3*labc
c
      call zvin1 (ityp,nlat,m,zv,imid,i3,wzvin,wzvin(iw1),wzvin(iw2),
     1            wzvin(iw3),wzvin(iw4))
      return
      end
      subroutine zvin1 (ityp,nlat,m,zv,imid,i3,zvz,zv1,a,b,c)
      dimension       zv(imid,nlat,3),zvz(imid,1),zv1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      zv(i,np1,i3) = zvz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      zv(i,np1,i3) = zv1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      zv(i,m+1,i3) = a(ns)*zv(i,m-1,i1)-c(ns)*zv(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      zv(i,m+2,i3) = a(ns)*zv(i,m,i1)-c(ns)*zv(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      zv(i,np1,i3) = a(ns)*zv(i,np1-2,i1)+b(ns)*zv(i,np1-2,i3)
     1                              -c(ns)*zv(i,np1,i1)
   75 continue
   80 return
      end
      subroutine zwin (ityp,nlat,nlon,m,zw,i3,wzwin)
      dimension       zw(1)        ,wzwin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzwin is 2*lim+3*labc
c
      call zwin1 (ityp,nlat,m,zw,imid,i3,wzwin,wzwin(iw1),wzwin(iw2),
     1            wzwin(iw3),wzwin(iw4))
      return
      end
      subroutine zwin1 (ityp,nlat,m,zw,imid,i3,zw1,zw2,a,b,c)
      dimension       zw(imid,nlat,3),zw1(imid,1),zw2(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-2)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=2,nlat
      do 45 i=1,imid
      zw(i,np1,i3) = zw1(i,np1)
   45 continue
      return
   30 do 50 np1=3,nlat
      do 50 i=1,imid
      zw(i,np1,i3) = zw2(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      zw(i,m+1,i3) = a(ns)*zw(i,m-1,i1)-c(ns)*zw(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      zw(i,m+2,i3) = a(ns)*zw(i,m,i1)-c(ns)*zw(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      zw(i,np1,i3) = a(ns)*zw(i,np1-2,i1)+b(ns)*zw(i,np1-2,i3)
     1                              -c(ns)*zw(i,np1,i1)
   75 continue
   80 return
      end
      subroutine vbinit (nlat,nlon,wvbin,dwork)
      dimension wvbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call vbini1 (nlat,nlon,imid,wvbin,wvbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine vbini1 (nlat,nlon,imid,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat+1 locations
c
      dimension vb(imid,nlat,2),abc(1)
      double precision pi,dt,cvb(1),th,vbh,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvbk(m,n,cvb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dvbt(m,n,th,cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wbinit (nlat,nlon,wwbin,dwork)
      dimension       wwbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call wbini1 (nlat,nlon,imid,wwbin,wwbin(iw1),dwork,
     1                                        dwork(nlat/2+2))
      return
      end
      subroutine wbini1 (nlat,nlon,imid,wb,abc,cwb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      dimension wb(imid,nlat,2),abc(1)
      double precision pi,dt,cwb(1),wbh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwbk(m,n,cwb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dwbt(m,n,th,cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine vbin (ityp,nlat,nlon,m,vb,i3,wvbin)
      dimension       vb(1)        ,wvbin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wvbin is 2*lim+3*labc
c
      call vbin1 (ityp,nlat,m,vb,imid,i3,wvbin,wvbin(iw1),wvbin(iw2),
     1            wvbin(iw3),wvbin(iw4))
      return
      end
      subroutine vbin1 (ityp,nlat,m,vb,imid,i3,vbz,vb1,a,b,c)
      dimension       vb(imid,nlat,3),vbz(imid,1),vb1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      vb(i,np1,i3) = vbz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      vb(i,np1,i3) = vb1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      vb(i,m+1,i3) = a(ns)*vb(i,m-1,i1)-c(ns)*vb(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      vb(i,m+2,i3) = a(ns)*vb(i,m,i1)-c(ns)*vb(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      vb(i,np1,i3) = a(ns)*vb(i,np1-2,i1)+b(ns)*vb(i,np1-2,i3)
     1                              -c(ns)*vb(i,np1,i1)
   75 continue
   80 return
      end
      subroutine wbin (ityp,nlat,nlon,m,wb,i3,wwbin)
      dimension       wb(1)        ,wwbin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wwbin is 2*lim+3*labc
c
      call wbin1 (ityp,nlat,m,wb,imid,i3,wwbin,wwbin(iw1),wwbin(iw2),
     1            wwbin(iw3),wwbin(iw4))
      return
      end
      subroutine wbin1 (ityp,nlat,m,wb,imid,i3,wb1,wb2,a,b,c)
      dimension       wb(imid,nlat,3),wb1(imid,1),wb2(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-2)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=2,nlat
      do 45 i=1,imid
      wb(i,np1,i3) = wb1(i,np1)
   45 continue
      return
   30 do 50 np1=3,nlat
      do 50 i=1,imid
      wb(i,np1,i3) = wb2(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      wb(i,m+1,i3) = a(ns)*wb(i,m-1,i1)-c(ns)*wb(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      wb(i,m+2,i3) = a(ns)*wb(i,m,i1)-c(ns)*wb(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      wb(i,np1,i3) = a(ns)*wb(i,np1-2,i1)+b(ns)*wb(i,np1-2,i3)
     1                              -c(ns)*wb(i,np1,i1)
   75 continue
   80 return
      end
       subroutine dzvk(nlat,m,n,czv,work)
c
c     subroutine dzvk computes the coefficients in the trigonometric
c     expansion of the quadrature function zvbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of wbarv(n,m,theta)
c
c     m      the order (superscript) of wbarv(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czv     the fourier coefficients of zvbar(n,m,theta).
c
      dimension czv(1),work(1)
      double precision czv,sc1,sum,work,t1,t2
      if(n .le. 0) return
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dvbk(m,n,work,czv)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even, m even
c
      kdo = n/2
      do 9 id=1,lc
      i = id+id-2
      sum = 0.
      do 10 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   10 continue
      czv(id) = sc1*sum
    9 continue
      return
c
c     n even, m odd
c
    2 kdo = n/2
      do 5 id=1,lc
      i = id+id-2
      sum = 0.
      do 6 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
    6 continue
      czv(id) = sc1*sum
    5 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd, m even
c
      kdo = (n+1)/2
      do 19 id=1,lc
      i = id+id-3
      sum = 0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   20 continue
      czv(id) = sc1*sum
   19 continue
      return
c
c     n odd, m odd
c
    3 kdo = (n+1)/2
      do 15 id=1,lc
      i = id+id-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
   16 continue
      czv(id) = sc1*sum
   15 continue
      return
      end
      subroutine dzvt(nlat,m,n,th,czv,zvh)
c
c     subroutine dzvt tabulates the function zvbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zvbar(n,m,theta)
c
c     m      the order (superscript) of zvbar(n,m,theta)
c
c     czv     the fourier coefficients of zvbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zvh     zvbar(m,n,theta) evaluated at theta = th
c
      dimension czv(1)
      double precision th,czv,zvh,cth,sth,cdt,sdt,chh
      zvh = 0.
      if(n .le. 0) return
      lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      cth = dcos(th)
      sth = dsin(th)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod .eq. 0) go to 50
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     nlat odd  n even  m even
c
      do 10 k=1,ls
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     nlat odd  n even  m odd
c
    2 zvh = .5*czv(1)
      do 20 k=2,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      zvh = zvh+.5*czv(lc)*dcos((nlat-1)*th)
      return
    1 if(mmod .ne. 0) go to 3
c
c     nlat odd  n odd  m even
c
      do 30 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   30 continue
      return
c
c     nlat odd  n odd  m odd
c
    3 do 40 k=1,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   40 continue
      return
   50 if(nmod .ne. 0) go to 51
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 52
c
c     nlat even  n even  m even
c
      do 55 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   55 continue
      return
c
c     nlat even  n even  m odd
c
   52 zvh = .5*czv(1)
      do 57 k=2,lc
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   57 continue
      return
   51 if(mmod .ne. 0) go to 53
c
c     nlat even  n odd  m even
c
      do 58 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   58 continue
      return
c
c     nlat even  n odd  m odd
c
   53 zvh = .5*czv(lc)*dcos((nlat-1)*th)
      do 60 k=1,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   60 continue
      return
      end
      subroutine dzwk(nlat,m,n,czw,work)
c
c     subroutine dzwk computes the coefficients in the trigonometric
c     expansion of the quadrature function zwbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czw     the fourier coefficients of zwbar(n,m,theta).
c
      dimension czw(1),work(1)
      double precision czw,work,sc1,sum,t1,t2
      if(n .le. 0) return
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dwbk(m,n,work,czw)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even, m even
c
      kdo = n/2
      do 19 id=1,lc
      i = id+id-3
      sum = 0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   20 continue
      czw(id) = sc1*sum
   19 continue
      return
c
c     n even, m odd
c
    2 kdo = n/2
      do 15 id=1,lc
      i = id+id-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
   16 continue
      czw(id) = sc1*sum
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd, m even
c
      kdo = (n-1)/2
      do 9 id=1,lc
      i = id+id-2
      sum = 0.
      do 10 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   10 continue
      czw(id) = sc1*sum
    9 continue
      return
c
c     n odd, m odd
c
    3 kdo = (n+1)/2
      do 5 id=1,lc
      i = id+id-2
      sum = work(1)/(1.d0-i*i)
      if(kdo .lt. 2) go to 29
      do 6 kp1=2,kdo
      k = kp1-1
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(kp1)*(t1+t2)/(t1*t2)
    6 continue
   29 czw(id) = sc1*sum
    5 continue
      return
      end
      subroutine dzwt(nlat,m,n,th,czw,zwh)
c
c     subroutine dzwt tabulates the function zwbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c            nlat must be an odd integer
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     czw     the fourier coefficients of zwbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zwh     zwbar(m,n,theta) evaluated at theta = th
c
      dimension czw(1)
      double precision czw,zwh,th,cth,sth,cdt,sdt,chh
      zwh = 0.
      if(n .le. 0) return
      lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      cth = dcos(th)
      sth = dsin(th)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod .eq. 0) go to 50
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     nlat odd  n even  m even
c
      do 30 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   30 continue
      return
c
c     nlat odd  n even  m odd
c
    2 do 40 k=1,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   40 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     nlat odd  n odd  m even
c
      do 10 k=1,ls
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     nlat odd  n odd  m odd
c
    3 zwh = .5*czw(1)
      do 20 k=2,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      zwh = zwh+.5*czw(lc)*dcos((nlat-1)*th)
      return
   50 if(nmod .ne. 0) go to 51
      if(mmod .ne. 0) go to 52
c
c     nlat even  n even  m even
c
      do 55 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   55 continue
      return
c
c     nlat even  n even  m odd
c
   52 zwh = .5*czw(lc)*dcos((nlat-1)*th)
      do 60 k=1,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   60 continue
      return
   51 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 53
c
c     nlat even  n odd  m even
c
      do 65 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   65 continue
      return
c
c     nlat even  n odd  m odd
c
   53 zwh = .5*czw(1)
      do 70 k=2,lc
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   70 continue
      return
      end
      subroutine dvbk(m,n,cv,work)
      double precision cv(1),work(1),fn,fk,cf
      cv(1) = 0.
      if(n .le. 0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(modn .ne. 0) go to 70
      ncv = n/2
      if(ncv .eq. 0) return
      fk = 0.
      if(modm .ne. 0) go to 60
c
c     n even m even
c
      do 55 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*work(l+1)/srnp1
   55 continue
      return
c
c     n even m odd
c
   60 do 65 l=1,ncv
      fk = fk+2.
      cv(l) = fk*work(l)/srnp1
   65 continue
      return
   70 ncv = (n+1)/2
      fk = -1.
      if(modm .ne. 0) go to 80
c
c     n odd m even
c
      do 75 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*work(l)/srnp1
   75 continue
      return
c
c     n odd m odd
c
   80 do 85 l=1,ncv
      fk = fk+2.
      cv(l) = fk*work(l)/srnp1
   85 continue
      return
      end
      subroutine dwbk(m,n,cw,work)
      double precision cw(1),work(1),fn,cf,srnp1
      cw(1) = 0.
      if(n.le.0 .or. m.le.0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(m .eq. 0) go to 50
      if(modn .ne. 0) go to 30
      l = n/2
      if(l .eq. 0) go to 50
      if(modm .ne. 0) go to 20
c
c     n even m even
c
      cw(l) = -cf*work(l+1)
   10 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      go to 10
c
c     n even m odd
c
   20 cw(l) = cf*work(l)
   25 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)+cf*work(l)
      go to 25
   30 if(modm .ne. 0) go to 40
      l = (n-1)/2
      if(l .eq. 0) go to 50
c
c     n odd m even
c
      cw(l) = -cf*work(l+1)
   35 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      go to 35
c
c     n odd m odd
c
   40 l = (n+1)/2
      cw(l) = cf*work(l)
   45 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)+cf*work(l)
      go to 45
   50 return
      end
      subroutine dvbt(m,n,theta,cv,vh)
      dimension cv(1)
      double precision cv,vh,theta,cth,sth,cdt,sdt,chh
      vh = 0.
      if(n.eq.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncv = n/2
      do 10 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncv = n/2
      do 15 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncv = (n+1)/2
      do 20 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncv = (n+1)/2
      do 25 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine dwbt(m,n,theta,cw,wh)
      dimension cw(1)
      double precision theta,cw,wh,cth,sth,cdt,sdt,chh
      wh = 0.
      if(n.le.0 .or. m.le.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod=mod(m,2)
      nmod=mod(n,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncw = n/2
      do 10 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncw = n/2
      do 8 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
    8 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncw = (n-1)/2
      do 20 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncw = (n+1)/2
      wh = .5*cw(1)
      if(ncw.lt.2) return
      do 25 k=2,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine rabcv(nlat,nlon,abc)
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the functions vbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      dimension abc(1)
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcv1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcv1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing vbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      dimension a(1),b(1),c(1)
      mmax = min0(nlat,(nlon+1)/2)
      if(mmax .lt. 3) return
      do 215 mp1=3,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      tpn = (fm-2.)*(fm-1.)/(fm*(fm+1.))
      a(ns) = sqrt(tpn*(tm+1.)*(tm-2.)/temp)
      c(ns) = sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      tpn = (fm-1.)*fm/((fm+1.)*(fm+2.))
      a(ns) = sqrt(tpn*(tm+3.)*(tm-2.)/temp)
      c(ns) = sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      tpn = (fn-2.)*(fn-1.)/(fn*(fn+1.))
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      a(ns) = sqrt(tpn*cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(tpn*cn*fnmm*(fnmm-1.)/temp)
      c(ns) = sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine rabcw(nlat,nlon,abc)
c
c     subroutine rabcw computes the coefficients in the recurrence
c     relation for the functions wbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      dimension abc(1)
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcw1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcw1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing wbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      dimension a(1),b(1),c(1)
      mmax = min0(nlat,(nlon+1)/2)
      if(mmax .lt. 4) return
      do 215 mp1=4,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      tpn = (fm-2.)*(fm-1.)/(fm*(fm+1.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*(tm+1.)*(tm-2.)/temp)
      c(ns) = tph*sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      tpn = (fm-1.)*fm/((fm+1.)*(fm+2.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*(tm+3.)*(tm-2.)/temp)
      c(ns) = tph*sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      tpn = (fn-2.)*(fn-1.)/(fn*(fn+1.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(tpn*cn*fnmm*(fnmm-1.)/temp)
      c(ns) = tph*sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine vtinit (nlat,nlon,wvbin,dwork)
      dimension       wvbin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call vtini1 (nlat,nlon,imid,wvbin,wvbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine vtini1 (nlat,nlon,imid,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      dimension vb(imid,nlat,2),abc(1),cvb(1)
      double precision pi,dt,cvb,th,vbh,work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvtk(m,n,cvb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dvtt(m,n,th,cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wtinit (nlat,nlon,wwbin,dwork)
      dimension       wwbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call wtini1 (nlat,nlon,imid,wwbin,wwbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine wtini1 (nlat,nlon,imid,wb,abc,cwb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      dimension wb(imid,nlat,2),abc(1)
      double precision pi,dt,cwb(*),wbh,th,work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwtk(m,n,cwb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dwtt(m,n,th,cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine vtgint (nlat,nlon,theta,wvbin,work)
      dimension       wvbin(*)
      double precision theta(*), work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call vtgit1 (nlat,nlon,imid,theta,wvbin,wvbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine vtgit1 (nlat,nlon,imid,theta,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1   locations
c
      dimension vb(imid,nlat,2),abc(*)
      double precision theta(*),cvb(*),work(*),vbh
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvtk(m,n,cvb,work)
      do 165 i=1,imid
      call dvtt(m,n,theta(i),cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wtgint (nlat,nlon,theta,wwbin,work)
      dimension       wwbin(*)
      double precision theta(*), work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call wtgit1 (nlat,nlon,imid,theta,wwbin,wwbin(iw1),
     1                        work,work(nlat/2+2))
      return
      end
      subroutine wtgit1 (nlat,nlon,imid,theta,wb,abc,cwb,work)
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      dimension wb(imid,nlat,2),abc(1)
      double precision theta(*), cwb(*), work(*), wbh
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwtk(m,n,cwb,work)
      do 165 i=1,imid
      call dwtt(m,n,theta(i),cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine dvtk(m,n,cv,work)
      double precision cv(*),work(*),fn,fk,cf,srnp1
      cv(1) = 0.
      if(n .le. 0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(modn .ne. 0) go to 70
      ncv = n/2
      if(ncv .eq. 0) return
      fk = 0.
      if(modm .ne. 0) go to 60
c
c     n even m even
c
      do 55 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l+1)/srnp1
   55 continue
      return
c
c     n even m odd
c
   60 do 65 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   65 continue
      return
   70 ncv = (n+1)/2
      fk = -1.
      if(modm .ne. 0) go to 80
c
c     n odd m even
c
      do 75 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   75 continue
      return
c
c     n odd m odd
c
   80 do 85 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   85 continue
      return
      end
      subroutine dwtk(m,n,cw,work)
      double precision cw(*),work(*),fn,cf,srnp1
      cw(1) = 0.
      if(n.le.0 .or. m.le.0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(m .eq. 0) go to 50
      if(modn .ne. 0) go to 30
      l = n/2
      if(l .eq. 0) go to 50
      if(modm .ne. 0) go to 20
c
c     n even m even
c
      cw(l) = -cf*work(l+1)
   10 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      cw(l+1) = (l+l+1)*cw(l+1)
      go to 10
c
c     n even m odd
c
   20 cw(l) = cf*work(l)
   25 l = l-1
      if(l) 50,27,26
   26 cw(l) = cw(l+1)+cf*work(l)
   27 cw(l+1) = -(l+l+1)*cw(l+1)
      go to 25
   30 if(modm .ne. 0) go to 40
      l = (n-1)/2
      if(l .eq. 0) go to 50
c
c     n odd m even
c
      cw(l) = -cf*work(l+1)
   35 l = l-1
      if(l) 50,37,36
   36 cw(l) = cw(l+1)-cf*work(l+1)
   37 cw(l+1) = (l+l+2)*cw(l+1)
      go to 35
c
c     n odd m odd
c
   40 l = (n+1)/2
      cw(l) = cf*work(l)
   45 l = l-1
      if(l) 50,47,46
   46 cw(l) = cw(l+1)+cf*work(l)
   47 cw(l+1) = -(l+l)*cw(l+1)
      go to 45
   50 return
      end
      subroutine dvtt(m,n,theta,cv,vh)
      dimension cv(1)
      double precision cv,vh,theta,cth,sth,cdt,sdt,chh
      vh = 0.
      if(n.eq.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncv = n/2
      do 10 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncv = n/2
      do 15 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncv = (n+1)/2
      do 20 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncv = (n+1)/2
      do 25 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine dwtt(m,n,theta,cw,wh)
      dimension cw(1)
      double precision theta,cw,wh,cth,sth,cdt,sdt,chh
      wh = 0.
      if(n.le.0 .or. m.le.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod=mod(m,2)
      nmod=mod(n,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncw = n/2
      do 10 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncw = n/2
      do 8 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
    8 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncw = (n-1)/2
      do 20 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncw = (n+1)/2
      wh = 0.
      if(ncw.lt.2) return
      do 25 k=2,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine vbgint (nlat,nlon,theta,wvbin,work)
      dimension       wvbin(1)
      double precision theta(*),work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call vbgit1 (nlat,nlon,imid,theta,wvbin,wvbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine vbgit1 (nlat,nlon,imid,theta,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      dimension vb(imid,nlat,2),abc(1)
      double precision cvb(1),theta(1),vbh,work(1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvbk(m,n,cvb,work)
      do 165 i=1,imid
      call dvbt(m,n,theta(i),cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wbgint (nlat,nlon,theta,wwbin,work)
      dimension       wwbin(1)
      double precision work(*),theta(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call wbgit1 (nlat,nlon,imid,theta,wwbin,wwbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine wbgit1 (nlat,nlon,imid,theta,wb,abc,cwb,work)
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      dimension wb(imid,nlat,2),abc(1)
      double precision cwb(1),theta(1),wbh,work(1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwbk(m,n,cwb,work)
      do 165 i=1,imid
      call dwbt(m,n,theta(i),cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end

