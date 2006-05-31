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
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file shaec.f
c
c     this file contains code and documentation for subroutines
c     shaec and shaeci
c
c ... files which must be loaded with shaec.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshaec,lshaec,work,lwork,ierror)
c
c     subroutine shaec performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on an equally spaced grid.  the
c     associated legendre functions are recomputed rather than stored
c     as they are in subroutine shaes.  the analysis is described
c     below at output parameters a,b.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shaec,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the colatitude
c            point theta(i) = (i-1)*pi/(nlat-1) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.
c
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shaec.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shaec.  jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shaec. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshaec an array which must be initialized by subroutine shaeci.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shaec. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                    nlat*(nt*nlon+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                    l2*(nt*nlon+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shsec. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     2. the normalized z functions for m even
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       the integral from tau = 0 to tau = pi of
c                       cos(k*theta)*cos(k*tau)*pbar(m,n,tau)*sin(tau)
c                       (first and last terms in this sum are divided
c                       by 2)
c
c     3. the normalized z functions for m odd
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       of the integral from tau = 0 to tau = pi of
c                       sin(k*theta)*sin(k*tau)*pbar(m,n,tau)*sin(tau)
c
c     4. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon
c                       of g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon
c                       of g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c     5. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       c(m+1,i)*zbar(m,n,theta(i))
c                       (first and last terms in this sum are
c                       divided by 2)
c
c     b(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       s(m+1,i)*zbar(m,n,theta(i))
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshaec
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
c
c     subroutine shaeci initializes the array wshaec which can then
c     be used repeatedly by subroutine shaec.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaeci. the array wshaec is an output
c            parameter which is described below. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     dwork  a double precision dwork array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shaeci.  ldwork  must be at least
c            nlat+1.
c
c
c     output parameters
c
c     wshaec an array which is initialized for use by subroutine shaec.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshaec
c            = 4  error in the specification of ldwork
c
c
c *******************************************************************
      subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshaec,lshaec,work,lwork,ierror)
      dimension g(idg,jdg,*),a(mdab,ndab,*),b(mdab,ndab,*),wshaec(*),
     1          work(*)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      if((isym.eq.0 .and. idg.lt.nlat) .or.
     1   (isym.ne.0 .and. idg.lt.(nlat+1)/2)) return
      ierror = 6
      if(jdg .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshaec .lt. lzz1+labc+nlon+15) return
      ierror = 10
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork .lt. nln+max0(ls*nlon,3*nlat*imid)) return
      ierror = 0
      ist = 0
      if(isym .eq. 0) ist = imid
      iw1 = lzz1+labc+1
      call shaec1(nlat,isym,nt,g,idg,jdg,a,b,mdab,ndab,imid,ls,nlon,
     1     work,work(ist+1),work(nln+1),work(nln+1),wshaec,wshaec(iw1))
      return
      end
      subroutine shaec1(nlat,isym,nt,g,idgs,jdgs,a,b,mdab,ndab,imid,
     1                idg,jdg,ge,go,work,zb,wzfin,whrfft)
c
c     whrfft must have at least nlon+15 locations
c     wzfin must have 2*l*(nlat+1)/2 + ((l-3)*l+2)/2 locations
c     zb must have 3*l*(nlat+1)/2 locations
c     work must have ls*nlon locations
c
      dimension g(idgs,jdgs,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          ge(idg,jdg,1),go(idg,jdg,1),zb(imid,nlat,3),wzfin(1),
     3          whrfft(1),work(1)
      ls = idg
      nlon = jdg
      mmax = min0(nlat,nlon/2+1)
      mdo = mmax
      if(mdo+mdo-1 .gt. nlon) mdo = mmax-1
      nlp1 = nlat+1
      tsn = 2./nlon
      fsn = 4./nlon
      modl = mod(nlat,2)
      imm1 = imid
      if(modl .ne. 0) imm1 = imid-1
      if(isym .ne. 0) go to 15
      do 5 k=1,nt
      do 5 i=1,imm1
      do 5 j=1,nlon
      ge(i,j,k) = tsn*(g(i,j,k)+g(nlp1-i,j,k))
      go(i,j,k) = tsn*(g(i,j,k)-g(nlp1-i,j,k))
    5 continue
      go to 30
   15 do 20 k=1,nt
      do 20 i=1,imm1
      do 20 j=1,nlon
      ge(i,j,k) = fsn*g(i,j,k)
   20 continue
      if(isym .eq. 1) go to 27
   30 if(modl .eq. 0) go to 27
      do 25 k=1,nt
      do 25 j=1,nlon
      ge(imid,j,k) = tsn*g(imid,j,k)
   25 continue
   27 do 35 k=1,nt
      call hrfftf(ls,nlon,ge(1,1,k),ls,whrfft,work)
      if(mod(nlon,2) .ne. 0) go to 35
      do 36 i=1,ls
      ge(i,nlon,k) = .5*ge(i,nlon,k)
   36 continue
   35 continue
      do 40 k=1,nt
      do 40 mp1=1,mmax
      do 40 np1=mp1,nlat
      a(mp1,np1,k) = 0.
      b(mp1,np1,k) = 0.
   40 continue
      if(isym .eq. 1) go to 145
      call zfin (2,nlat,nlon,0,zb,i3,wzfin)
      do 110 k=1,nt
      do 110 i=1,imid
      do 110 np1=1,nlat,2
      a(1,np1,k) = a(1,np1,k)+zb(i,np1,i3)*ge(i,1,k)
  110 continue
      ndo = nlat
      if(mod(nlat,2) .eq. 0) ndo = nlat-1
      do 120 mp1=2,mdo
      m = mp1-1
      call zfin (2,nlat,nlon,m,zb,i3,wzfin)
      do 120 k=1,nt
      do 120 i=1,imid
      do 120 np1=mp1,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+zb(i,np1,i3)*ge(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+zb(i,np1,i3)*ge(i,2*mp1-1,k)
  120 continue
      if(mdo .eq. mmax .or. mmax .gt. ndo) go to 135
      call zfin (2,nlat,nlon,mdo,zb,i3,wzfin)
      do 130 k=1,nt
      do 130 i=1,imid
      do 130 np1=mmax,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+zb(i,np1,i3)*ge(i,2*mmax-2,k)
  130 continue
  135 if(isym .eq. 2) return
  145 call zfin (1,nlat,nlon,0,zb,i3,wzfin)
      do 150 k=1,nt
      do 150 i=1,imm1
      do 150 np1=2,nlat,2
      a(1,np1,k) = a(1,np1,k)+zb(i,np1,i3)*go(i,1,k)
  150 continue
      ndo = nlat
      if(mod(nlat,2) .ne. 0) ndo = nlat-1
      do 160 mp1=2,mdo
      m = mp1-1
      mp2 = mp1+1
      call zfin (1,nlat,nlon,m,zb,i3,wzfin)
      do 160 k=1,nt
      do 160 i=1,imm1
      do 160 np1=mp2,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+zb(i,np1,i3)*go(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+zb(i,np1,i3)*go(i,2*mp1-1,k)
  160 continue
      mp2 = mmax+1
      if(mdo .eq. mmax .or. mp2 .gt. ndo) return
      call zfin (1,nlat,nlon,mdo,zb,i3,wzfin)
      do 170 k=1,nt
      do 170 i=1,imm1
      do 170 np1=mp2,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+zb(i,np1,i3)*go(i,2*mmax-2,k)
  170 continue
      return
      end

      subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
      dimension wshaec(lshaec)
      double precision dwork(ldwork)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      imid = (nlat+1)/2
      mmax = min0(nlat,nlon/2+1)
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshaec .lt. lzz1+labc+nlon+15) return
      ierror = 4
      if(ldwork .lt. nlat+1) return
      ierror = 0
      call zfinit (nlat,nlon,wshaec,dwork)
      iw1 = lzz1+labc+1
      call hrffti(nlon,wshaec(iw1))
      return
      end
