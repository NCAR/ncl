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
c ... visgeo.f
c
c     contains documentation and code for subroutine visgeo
c
      SUBROUTINE VISGEO (M,IDP,JDP,X,Y,Z,H,EYER,EYELAT,EYELON,
     1                          WORK,LWORK,IWORK,LIWORK,IERROR)
c
c     subroutine visgeo will display a function on the sphere
c     as a solid. ie. as a "lumpy" sphere. visgeo calls subroutine
c     vsurf to produce the visible surface rendering. X, Y, and Z
c     are the points on an icosahedral geodesic computed by
c     subroutine geopts available in spherepack.
c
c     requires routines visgeo1 ctos stoc vsurf vsurf1
c                       prjct box
c
c     visgeo uses the ncar graphics package.
c     compile with: ncargf77 (all programs above)
c
c     execute with:  a.out
c
c     on screen display with:  ctrans -d x11 gmeta
c                          
c     print with:  ctrans -d ps.color gmeta > gmeta.ps
c                  lpr -P(your printer) gmeta.ps 
c
c
c     input parameters
c
c     m        the number of points on one edge of the icosahedron
c
c     idp,jdp  the first and second dimensions of the three
c              dimensional arrays x, y, z, and h.
c
c     x,y,z    the coordinates of the geodesic points on 
c              the unit sphere computed by subroutine geopts.
c              the indices are defined on the unfolded 
c              icosahedron as follows for the case m=3
c
c                north pole
c
c                 (5,1)          0      l
c        i     (4,1) (5,2)              a    (repeated for
c           (3,1) (4,2) (5,3)  theta1   t    k=2,3,4,5 in
c        (2,1) (3,2) (4,3)              i        -->
c     (1,1) (2,2) (3,3)        theta2   t    the longitudinal 
c        (1,2) (2,3)                    u    direction)
c           (1,3)                pi     d
c      j                                e
c         south pole
c
c            total number of vertices is  10*(m-1)**2+2
c            total number of triangles is 20*(m-1)**2
c
c     h      a three dimensional array that contains the discrete
c            function to be displayed. h(i,j,k) is the distance from
c            the center of the sphere to the "lumpy" surface at the
c             point [x(i,j,k),y(i,j,k),z(i,j,k)] on the unit sphere.
c
c     eyer   the distance from the center of the sphere to the eye.
c
c     eyelat the colatitudinal coordinate of the eye (in degrees).
c
c     eyelon the longitudinal  coordinate of the eye (in degrees).
c
c     idp    the first dimension of the array h as it appears in
c            the program that calls visgeo
c
c     jdp    the second dimension of the array h as it appears in
c            the program that calls visgeo
c
c     work   a real work array 
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls visgeo. lwork must be at least 
c                       480*(m-1)**2.
c
c     iwork  an integer work array
c
c     liwork the dimension of the array iwork as it appears in the
c            program that calls visgeo. liwork must be at least 
c                       140*(m-1)**2.
c
c     input parameter
c
c     ierror = 0    no error
c            = 1    h(i,j,k) is less than zero for some i,j,k.
c            = 2    eyer is less than h(i,j,k) for some i,k,k.
c            = 3    lwork  is less than 480*(m-1)**2
c            = 4    liwork is less than 140*(m-1)**2
c
      dimension h(idp,jdp,5),x(idp,jdp,5),y(idp,jdp,5),z(idp,jdp,5),
     1                                                        work(*)
      INTEGER IWORK(*)
      mmsq = (m-1)**2
      ierror = 3
      if(lwork .lt. 480*mmsq) return
      ierror = 4
      if(liwork .lt. 140*mmsq) return
      do 10 k=1,5
      do 10 j=1,m
      do 10 i=1,m+m-1
      if(h(i,j,k) .ge. 0.) go to 15
      ierror = 1
      return
   15 if(eyer .gt. h(i,j,k)) go to 10
      ierror = 2
      return
   10 continue
      ierror = 0
      lt = 20*(m-1)**2
      lg = 5*m*(m+m-1)
      i1 = 1
      i2 = i1+lt
      i3 = i2+lt
      i4 = i3+lt
      i5 = i4+lt
      i6 = i5+lt
      i7 = i6+lt
      i8 = i7+lt
      i9 = i8+lt
      i10 = i9+lt
      i11 = i10+lt
      i12 = i11
      i13 = i12+lg
      i14 = i13+lg
      call visgeo1 (m,idp,jdp,h,eyer,eyelat,eyelon,x,y,z,
     1work(i1),work(i2),work(i3),work(i4),work(i5),work(i6),
     2work(i7),work(i8),work(i9),IWORK(1),work(i11),
     3work(i12),work(i13),work(i14),IWORK(lt+1))
      return
      end
      SUBROUTINE VISGEO1(M,IDP,JDP,H,EYER,EYELAT,EYELON,
     1    XI,YI,ZI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,ITYPE,WORK,X,Y,Z,IWORK)
      dimension h(idp,jdp,5),xi(idp,jdp,5),yi(idp,jdp,5),zi(idp,jdp,5),
     1x1(*),y1(*),z1(*),x2(*),y2(*),z2(*),x3(*),y3(*),z3(*),itype(*),
     2work(*),x(m+m-1,m,5),y(m+m-1,m,5),z(m+m-1,m,5)
      INTEGER IWORK(*)
c
c     the * above refers to 20*(m-1)**2 locations which is the
c     number of triangles
c
      do 10 k=1,5
      do 10 j=1,m
      do 10 i=1,m+m-1
      call ctos(xi(i,j,k),yi(i,j,k),zi(i,j,k),rad,theta,elambda)
      call stoc(h(i,j,k),theta,elambda,x(i,j,k),y(i,j,k),z(i,j,k))
   10 continue  
      ntri = 0
      do 20 k=1,5
      do 20 j=1,m-1
      do 20 i=1,m+m-2
      ntri = ntri+1
      x1(ntri) = x(i,j,k)
      y1(ntri) = y(i,j,k)
      z1(ntri) = z(i,j,k)
      x2(ntri) = x(i+1,j+1,k)
      y2(ntri) = y(i+1,j+1,k)
      z2(ntri) = z(i+1,j+1,k)
      x3(ntri) = x(i+1,j,k)
      y3(ntri) = y(i+1,j,k)
      z3(ntri) = z(i+1,j,k)
      itype(ntri) = 13
      ntri = ntri+1
      x1(ntri) = x(i,j,k)
      y1(ntri) = y(i,j,k)
      z1(ntri) = z(i,j,k)
      x2(ntri) = x(i+1,j+1,k)
      y2(ntri) = y(i+1,j+1,k)
      z2(ntri) = z(i+1,j+1,k)
      x3(ntri) = x(i,j+1,k)
      y3(ntri) = y(i,j+1,k)
      z3(ntri) = z(i,j+1,k)
      itype(ntri) = 3
 20   continue
c      write(6,22) ntri
 22   format(i10)
c      write(6,23) (x1(l2),y1(l2),z1(l2),x2(l2),y2(l2),z2(l2),
c     1             x3(l2),y3(l2),z3(l2),l2=1,ntri)
c 23   format(9f10.7)
c
      pi = 4.*atan(1.)
      dtr = pi/180.
      xeye=eyer*sin(dtr*eyelat)
      yeye=xeye*sin(dtr*eyelon)
      xeye=xeye*cos(dtr*eyelon)
      zeye=eyer*cos(dtr*eyelat)
      CALL VSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,
     1                 X3,Y3,Z3,ITYPE,WORK,IWORK)
      return
      end
      subroutine ctos(x,y,z,r,theta,phi)
      r1 = x*x+y*y
      if(r1 .ne. 0.) go to 10
      phi = 0.
      theta = 0.
      if(z .lt. 0.) theta = 4.*atan(1.)
      return
   10 r = sqrt(r1+z*z)
      r1 = sqrt(r1) 
      phi = atan2(y,x)
      theta = atan2(r1,z)
      return
      end
      subroutine stoc(r,theta,phi,x,y,z)
      st = sin(theta)
      x = r*st*cos(phi)
      y = r*st*sin(phi)
      z = r*cos(theta)
      return
      end
      SUBROUTINE VSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,
     1                 X3,Y3,Z3,ITYPE,WORK,IWORK)
c
c    subroutine vsurf is like subroutine hidel except the triangles
c    are categorized. vsurf is also like solid except triangles rather
c    than lines are covered.
c
c     written by paul n. swarztrauber, national center for atmospheric
c     research, p.o. box 3000, boulder, colorado, 80307  
c
c    this program plots visible lines for the surface defined
c    by the input 3-d triangles with corners at (x1,y1,z1), (x2,y2,z2)
c    and (x3,y3,z3). the sides of these these triangles may or
c    may not be plotted depending on itype. if itype is 1 then the
c    side between points (x1,y1,z1) and (x2,y2,z2) is plotted if it
c    is visible. if itype is 2 then the side between (x2,y2,z2)
c    and (x3,y3,z3) is plotted. if itype is 3 then the visible portion
c    of the side between (x3,y3,z3) and (x1,y1,z1) is plotted.
c    any combination is possible by specifying itype to be one
c    of the following values: 0,1,2,3,12,13,23,123.
c
c    the length of real    array  work must be at least 14*ntri
c
c    the length of integer array iwork must be at least  6*ntri
c
c
c    the vertices of the triangles are renumbered by vsurf so that
c    their projections are orientated counterclockwise. the user need
c    only be aware that the vertices may be renumbered by vsurf.
c
      dimension x1(ntri),y1(ntri),z1(ntri),x2(ntri),y2(ntri),z2(ntri),
     1          x3(ntri),y3(ntri),z3(ntri),itype(ntri),work(14*ntri)
      INTEGER IWORK(6*NTRI)
c
      call vsurf1(xeye,yeye,zeye,ntri,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     1 itype,work,work(ntri+1),work(2*ntri+1),work(3*ntri+1),
     2 work(4*ntri+1),work(5*ntri+1),work(6*ntri+1),work(7*ntri+1),
     3 work(8*ntri+1),work(9*ntri+1),work(10*ntri+1),work(11*ntri+1),
     3 work(12*ntri+1),work(13*ntri+1),IWORK,IWORK(NTRI+1),
     4 IWORK(2*NTRI+1),IWORK(4*NTRI+1))
      return
      end
      subroutine vsurf1(xeye,yeye,zeye,ntri,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     1 itype,px1,py1,px2,py2,px3,py3,vx1,vy1,vx2,vy2,vx3,vy3,tl,tr,kh,
     2 next,istart,ifinal)
c
      dimension x1(ntri),y1(ntri),z1(ntri),x2(ntri),y2(ntri),z2(ntri),
     1          x3(ntri),y3(ntri),z3(ntri),itype(ntri),
     2          px1(ntri),py1(ntri),px2(ntri),py2(ntri),
     3          px3(ntri),py3(ntri),vx1(ntri),vy1(ntri),
     4          vx2(ntri),vy2(ntri),vx3(ntri),vy3(ntri),
     5          tl(ntri),tr(ntri),next(ntri),kh(ntri),
     6          istart(2*ntri),ifinal(2*ntri),ltp(3),
     7          ird(11),ip2(11),nct(11),ncv(11),last(11)
c
      real l2e
      double precision le2
c
c     compute projections of 3-d points
c
      le2 = .6931471805599453094172321d0
      l2e = 1.d0/le2
      fntri = ntri
      irmax = .5*l2e*log(fntri)
      irmax = min(irmax,10)
      irmp1 = irmax+1
      do 4 icv=1,11
      ncv(icv) = 0
    4 continue
      nct(1) = 0
      ip2(1) = 1
      ird(1) = 0
      isize = 4
      do 7 irp1=2,irmp1
      ir = irp1-1
      nct(irp1) = 0
      ip2(irp1) = 2**ir
      ird(irp1) = ird(ir)+isize
      isize = (ip2(irp1)+1)**2
    7 continue 
      isxm = ird(irmp1)+isize+1
      do 8 isx=1,isxm
      istart(isx) = 0
      ifinal(isx) = 0
    8 continue
      do 6 i=1,ntri
      next(i) = 0
    6 continue 
      call prjct(0,xeye,yeye,zeye,x,y,z,dum1,dum2)
c      write(6,127) ntri
  127 format(' ntri in hidel', i5)
      do 86 k=1,ntri
      call prjct(1,xeye,yeye,zeye,x1(k),y1(k),z1(k),px1(k),py1(k))
      call prjct(1,xeye,yeye,zeye,x2(k),y2(k),z2(k),px2(k),py2(k))
      call prjct(1,xeye,yeye,zeye,x3(k),y3(k),z3(k),px3(k),py3(k))
        if (k .lt. 3) then
c          write(6,333) xeye,yeye,zeye,x1(k),y1(k),z1(k),px1(k),py1(k)
  333     format(' xeye, etc.',8e8.1)
        endif
   86 continue
c
c     orientate triangles counter clockwise
c
      do 70 k=1,ntri
      cprod = (px2(k)-px1(k))*(py3(k)-py1(k))-(py2(k)-py1(k))
     1       *(px3(k)-px1(k))
c      if(cprod.eq.0.) write(6,79) k,px1(k),px2(k),px3(k),
c     -                              py1(k),py2(k),py3(k)
   79 format('  cprod=0 at k=', i5,6e9.2)
      if(cprod.ge.0.) go to 70
      px1h = px1(k)
      py1h = py1(k)
      px1(k) = px2(k)
      py1(k) = py2(k)
      px2(k) = px1h
      py2(k) = py1h
      x1hold = x1(k)
      y1hold = y1(k)
      z1hold = z1(k)
      x1(k) = x2(k)
      y1(k) = y2(k)
      z1(k) = z2(k)
      x2(k) = x1hold
      y2(k) = y1hold
      z2(k) = z1hold
      ityp = itype(k)
      if(ityp.eq.2) itype(k) = 3
      if(ityp.eq.3) itype(k) = 2
      if(ityp.eq.12) itype(k) = 13
      if(ityp.eq.13) itype(k) = 12
   70 continue
c
c     set screen limits
c
      pmax = px1(1)
      pmin = px1(1)
      do 87 k=1,ntri
      pmin = amin1(pmin,px1(k),py1(k),px2(k),py2(k),px3(k),py3(k))
      pmax = amax1(pmax,px1(k),py1(k),px2(k),py2(k),px3(k),py3(k))
   87 continue
      pmin = 1.1*pmin
      pmax = 1.1*pmax
      call set(0.,1.,0.,1.,pmin,pmax,pmin,pmax,1)
      xmin = amin1(px1(1),px2(1),px3(1)) 
      xmax = amax1(px1(1),px2(1),px3(1)) 
      ymin = amin1(py1(1),py2(1),py3(1)) 
      ymax = amax1(py1(1),py2(1),py3(1)) 
      do 1 i=2,ntri
      xmin = amin1(xmin,px1(i),px2(i),px3(i)) 
      xmax = amax1(xmax,px1(i),px2(i),px3(i)) 
      ymin = amin1(ymin,py1(i),py2(i),py3(i)) 
      ymax = amax1(ymax,py1(i),py2(i),py3(i)) 
    1 continue
      dmx = xmax-xmin
      dmy = ymax-ymin
      if(dmx .gt. dmy) go to 2 
      c = ymin
      d = ymax
      xmid = .5*(xmin+xmax)
      hdy = .5*dmy
      a = xmid-hdy
      b = xmid+hdy
      go to 3
    2 a = xmin
      b = xmax
      ymid = .5*(ymin+ymax)
      hdx = .5*dmx
      c = ymid-hdx
      d = ymid+hdx
    3 hgr = b-a
c
c     categorize triangles
c
      do 100 i=1,ntri
      xmin = amin1(px1(i),px2(i),px3(i))
      xmax = amax1(px1(i),px2(i),px3(i))
      ymin = amin1(py1(i),py2(i),py3(i))
      ymax = amax1(py1(i),py2(i),py3(i))
      dxt = amax1(xmax-xmin,ymax-ymin)
      if(dxt .gt. 0.) go to 10
      ir = irmax
      go to 20
   10 ir = l2e*log(hgr/dxt)  
      ir = min(ir,irmax)
   20 irp1 = ir+1
      nct(irp1) = nct(irp1)+1
      hr = hgr/ip2(irp1)
      xmid = .5*(xmin+xmax)
      id = (xmid-a)/hr+1.5
      ymid = .5*(ymin+ymax)
      jd = (ymid-c)/hr+1.5
      ijd = ip2(irp1)+1
      isx = id+(jd-1)*ijd+ird(irp1)
      ifx = ifinal(isx)
      if(ifx .gt. 0) go to 50
      istart(isx) = i
      go to 60
   50 next(ifx) = i 
   60 ifinal(isx) = i
  100 continue
c      write(6,106) tcat,(irp1,nct(irp1),irp1=1,irmp1)
  106 format(' time to categorize   ', e15.6/(' ir+1',i3,' ntri',i7))
c
c     sort triangles into boxes
c     
      l = 0
      do 30 irp1=1,irmp1
      if(nct(irp1) .eq. 0) go to 30
      ist = ird(irp1)+1    
      isd = ip2(irp1)+1
      call box(isd,istart(ist),next,l,ifinal)
      last(irp1) = l+1
   30 continue
      do 35 irp1=1,irmp1
      il = ird(irp1)+(ip2(irp1)+1)**2+1
      if(istart(il) .eq. 0) istart(il) = last(irp1)
   35 continue
c      write(6,31) tsort,l,ntri
   31 format(' time to sort  ', e15.6,'   l', i8,'   ntri',i8)
      do 90 k=1,ntri
      vx1(k) = px2(k)-px1(k)
      vy1(k) = py2(k)-py1(k)
      vx2(k) = px3(k)-px2(k)
      vy2(k) = py3(k)-py2(k)
      vx3(k) = px1(k)-px3(k)
      vy3(k) = py1(k)-py3(k)
   90 continue
      tl1 = 0.
      tl2 = 0.
      maxs = 0
      do 500 ir2=1,irmp1
      if(nct(ir2) .eq. 0) go to 500
      ist = ird(ir2)    
      isd = ip2(ir2)+1
      do 490 j2=1,isd
      do 480 i2=1,isd
      ist = ist+1
      ls = istart(ist)
      lf = istart(ist+1)-1
      if(lf .lt. ls) go to 480
c
c     define coverings
c
      kcv = 0
      i2m = i2-1
      j2m = j2-1
      do 300 ir1=1,irmp1
      if(nct(ir1) .eq. 0) go to 300
      if(ir1 .ge. ir2) go to 260
      irdp = 2**(ir2-ir1)
      i1s = (i2m-1)/irdp
      i1f = (i2m+1)/irdp
      if = i2m+1-i1f*irdp
      if(if .gt. 0) i1f = i1f+1
      j1s = (j2m-1)/irdp
      j1f = (j2m+1)/irdp
      jf = j2m+1-j1f*irdp
      if(jf .gt. 0) j1f = j1f+1
      go to 270
  260 irdp = 2**(ir1-ir2)
      i1s = irdp*(i2m-1)
      i1f = irdp*(i2m+1)
      j1s = irdp*(j2m-1)
      j1f = irdp*(j2m+1)
  270 ijd = ip2(ir1)+1
      i1s = max(i1s+1,1)
      i1f = min(i1f+1,ijd)
      j1s = max(j1s+1,1)
      j1f = min(j1f+1,ijd)
      ixh = (j1s-2)*ijd+ird(ir1)
      ixs = i1s+ixh
      ixf = i1f+ixh
      do 290 j1=j1s,j1f
      ixs = ixs+ijd
      kds = istart(ixs)
      ixf = ixf+ijd
      kdf = istart(ixf+1)-1
      if(kdf .lt. kds) go to 290
      do 280 kd=kds,kdf
      kcv = kcv+1
      kh(kcv) = ifinal(kd) 
  280 continue
  290 continue
  300 continue
      do 310 icv=1,10    
      if(kcv .le. ncv(icv)) go to 310
      ncv(icv) = kcv 
      go to 320
  310 continue
c
c
  320 do 470 ldo=ls,lf
      l = ifinal(ldo)
      ith = itype(l)
      if(ith .eq. 0) go to 470
      ltp(1) = 0
      ltp(2) = 0
      ltp(3) = 0
      id1 = ith/100 
      ith = ith-100*id1
      id2 = ith/10 
      id3 = ith-10*id2
      if(id1 .ne. 0) ltp(id1) = 1
      if(id2 .ne. 0) ltp(id2) = 1
      if(id3 .ne. 0) ltp(id3) = 1
c     if((ith.eq.123) .or. (ith.eq.12) .or.(ith.eq.13)) ltp(1) = 1
c     if((ith.eq.123) .or. (ith.eq.23) .or.(ith.eq.12)) ltp(2) = 1
c     if((ith.eq.123) .or. (ith.eq.13) .or.(ith.eq.23)) ltp(3) = 1
      do 460 ns=1,3
      go to (101,102,103),ns
  101 if(ltp(ns) .eq. 0) go to 460
      px4 = px1(l)
      py4 = py1(l)
      px5 = px2(l)
      py5 = py2(l)
      x4 = x1(l)
      y4 = y1(l)
      z4 = z1(l)
      x5 = x2(l)
      y5 = y2(l)
      z5 = z2(l)
      go to 105
  102 if(ltp(ns) .eq. 0) go to 460
      px4 = px2(l)
      py4 = py2(l)
      px5 = px3(l)
      py5 = py3(l)
      x4 = x2(l)
      y4 = y2(l)
      z4 = z2(l)
      x5 = x3(l)
      y5 = y3(l)
      z5 = z3(l)
      go to 105
  103 if(ltp(ns) .eq. 0) go to 460
      px4 = px1(l)
      py4 = py1(l)
      px5 = px3(l)
      py5 = py3(l)
      x4 = x1(l)
      y4 = y1(l)
      z4 = z1(l)
      x5 = x3(l)
      y5 = y3(l)
      z5 = z3(l)
  105 x54 = px5-px4
      y54 = py5-py4
      nseg = 0
      do 440 kd=1,kcv
      k = kh(kd) 
      c17 = vx1(k)*y54-vy1(k)*x54
      c27 = vx2(k)*y54-vy2(k)*x54
      c37 = vx3(k)*y54-vy3(k)*x54
      c14 = vy1(k)*(px4-px1(k))-vx1(k)*(py4-py1(k))
      c25 = vy2(k)*(px4-px2(k))-vx2(k)*(py4-py2(k))
      c36 = vy3(k)*(px4-px3(k))-vx3(k)*(py4-py3(k))
      tmin = 0.
      tmax = 1.
      if(c17) 151,152,153
  151 tmax = amin1(c14/c17,tmax)   
      go to 154
  152 if(c14) 154,440,440
  153 tmin = amax1(c14/c17,tmin)
  154 if(c27) 155,156,157
  155 tmax = amin1(c25/c27,tmax)   
      go to 158
  156 if(c25) 158,440,440
  157 tmin = amax1(c25/c27,tmin)
  158 if(c37) 159,160,161
  159 tmax = amin1(c36/c37,tmax)   
      go to 162
  160 if(c36) 162,440,440
  161 tmin = amax1(c36/c37,tmin)
  162 if(tmax-tmin .lt. .00001) go to 440
      xpl = x4+tmin*(x5-x4)
      ypl = y4+tmin*(y5-y4)
      zpl = z4+tmin*(z5-z4)
      xpr = x4+tmax*(x5-x4)
      ypr = y4+tmax*(y5-y4)
      zpr = z4+tmax*(z5-z4)
c
c     the projections of line and plane intersect
c     now determine if plane covers line
c
      vx1t = x2(k)-x1(k)
      vy1t = y2(k)-y1(k)
      vz1t = z2(k)-z1(k)
      vx2t = x3(k)-x1(k)
      vy2t = y3(k)-y1(k)
      vz2t = z3(k)-z1(k)
      apl = vy1t*vz2t-vy2t*vz1t
      bpl = vx2t*vz1t-vx1t*vz2t
      cpl = vx1t*vy2t-vx2t*vy1t
      dpl = apl*x1(k)+bpl*y1(k)+cpl*z1(k)
      vx3t = xpl-xeye
      vy3t = ypl-yeye
      vz3t = zpl-zeye
      den = apl*vx3t+bpl*vy3t+cpl*vz3t
      til = 0.
      if(den .eq. 0.) go to 410
      til = (dpl-apl*xeye-bpl*yeye-cpl*zeye)/den
  410 vx3t = xpr-xeye
      vy3t = ypr-yeye
      vz3t = zpr-zeye
      den = apl*vx3t+bpl*vy3t+cpl*vz3t
      tir = 0.
      if(den .eq. 0.) go to 412
      tir = (dpl-apl*xeye-bpl*yeye-cpl*zeye)/den
  412 if(til.ge..99999.and.tir.ge..99999) go to 440
      if(til.lt.1..and.tir.lt.1.) go to 164
      vx3t = xpr-xpl
      vy3t = ypr-ypl
      vz3t = zpr-zpl
      den = apl*vx3t+bpl*vy3t+cpl*vz3t
      tim = 0.
      if(den .eq. 0.) go to 414
      tim = (dpl-apl*xpl-bpl*ypl-cpl*zpl)/den
  414 thold = tmin+tim*(tmax-tmin)
      if(til.ge.1.) go to 163
      tmax = thold
      go to 164
  163 tmin = thold
  164 nseg = nseg+1
      tl(nseg) = tmin
      tr(nseg) = tmax
  440 continue
      maxs = max0(maxs,nseg)
      if(nseg-1)171,180,172
  171 call line(px4,py4,px5,py5)
      go to 460
c
c     order the segments according to left end point tl(k)
c
  172 do 173 k=2,nseg
      do 173 i=k,nseg
      if(tl(k-1).le.tl(i)) go to 173
      tlh = tl(k-1)
      trh = tr(k-1)
      tl(k-1) = tl(i)
      tr(k-1) = tr(i)
      tl(i) = tlh
      tr(i) = trh
  173 continue
c
c     eliminate segment overlap
c
      k1 = 1
      k2 = 1
  174 k2 = k2+1
      if(k2.gt.nseg) go to 176
      if(tr(k1).lt.tl(k2)) go to 175
      tr(k1) = amax1(tr(k1),tr(k2))
      go to 174
  175 k1 = k1+1
      tl(k1) = tl(k2)
      tr(k1) = tr(k2)
      go to 174
  176 nseg = k1
c
c     plot all segments of the line
c
  180 do 181 ks =1,nseg
      kb = nseg-ks+1
      tl(kb+1) = tr(kb)
      tr(kb) = tl(kb)
  181 continue
      tl(1) = 0.
      tr(nseg+1) = 1.
      nsegp = nseg+1
      do 450 k=1,nsegp
      if(abs(tr(k)-tl(k)).lt..000001) go to 450
      xa = px4+tl(k)*(px5-px4)
      ya = py4+tl(k)*(py5-py4)
      xb = px4+tr(k)*(px5-px4)
      yb = py4+tr(k)*(py5-py4)
      call line(xa,ya,xb,yb)
  450 continue
  460 continue
  470 continue
  480 continue
  490 continue
  500 continue
c      write(6,903) tl1,tl2
  903 format(' time to cover', e15.6/
     1       ' time to test ', e15.6)
c      write(6,904) maxs
  904 format(' maximum number of segments', i5)
c      write(6,250) (ncv(icv),icv=1,10)
  250 format('  the ten largest coverings'/(10i5))
      call frame
      end
      subroutine prjct(init,xeye,yeye,zeye,x,y,z,px,py)
c
c     subroutine prjct projects the point x,y,z onto a plane through
c     the origin that is perpendicular to a line between the origin
c     and the eye. the projection is along the line between the eye
c     and the point x,y,z. px and py are the coordinates of the
c     projection in the plane.
c     (version 2 , 12-10-82)
c
      save
      if(init.ne.0) go to 1
      rads1 = xeye**2+yeye**2
      rads2 = rads1+zeye**2
      d1 = sqrt(rads1)
      d2 = sqrt(rads2)
      if(d1.ne.0.) go to 2
      cx1 = 1.
      cy1 = 0.
      cx2 = 0.
      cy2 = 1.
      cz2 = 0.
      cx3 = 0.
      cy3 = 0.
      cz3 = 1.
      return
    2 cx1 = -yeye/d1
      cy1 = xeye/d1
      cx2 = -xeye*zeye/(d1*d2)
      cy2 = -yeye*zeye/(d1*d2)
      cz2 = d1/d2
      cx3 = xeye/d2
      cy3 = yeye/d2
      cz3 = zeye/d2
      return
    1 x1 = cx1*x+cy1*y
      y1 = cx2*x+cy2*y+cz2*z
      z1 = cx3*x+cy3*y+cz3*z
      ratio = d2/(d2-z1)
      px = ratio*x1
      py = ratio*y1
      return
      end
      subroutine box(isd,istart,next,l,list)
      dimension istart(isd,isd),next(1),list(1)
      do 30 jd=1,isd
      do 10 id=1,isd
      idx = istart(id,jd)
      istart(id,jd) = l+1
      if(idx .eq. 0) go to 10
   20 l = l+1
      list(l) = idx
      if(next(idx) .eq. 0) go to 10
      idx = next(idx)
      go to 20
   10 continue
   30 continue
      return
      end
