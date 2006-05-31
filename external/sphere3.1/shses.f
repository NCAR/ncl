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
c
c ... file shses.f
c
c     this file contains code and documentation for subroutines
c     shses and shsesi
c
c ... files which must be loaded with shses.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shses(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshses,lshses,work,lwork,ierror)
c
c     subroutine shses performs the spherical harmonic synthesis
c     on the arrays a and b and stores the result in the array g.
c     the synthesis is performed on an equally spaced grid.  the
c     associated legendre functions are stored rather than recomputed
c     as they are in subroutine shsec.  the synthesis is described
c     below at output parameter g.
c
c *** required files from spherepack2
c
c     sphcom.f, hrfft.f
c
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
c     isym   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the synthesis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the synthesis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls shses,
c            the arrays g,a and b can be three dimensional in which
c            case multiple syntheses will be performed.  the third
c            index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shses.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shses.  jdg must be at least nlon.
c
c     a,b    two or three dimensional arrays (see the input parameter
c            nt) that contain the coefficients in the spherical harmonic
c            expansion of g(i,j) given below at the definition of the
c            output parameter g.  a(m,n) and b(m,n) are defined for
c            indices m=1,...,mmax and n=m,...,nlat where mmax is the
c            maximum (plus one) longitudinal wave number given by
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shses. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shses. ndab must be at least nlat
c
c     wshses an array which must be initialized by subroutine shsesi.
c            once initialized, wshses can be used repeatedly by shses
c            as long as nlon and nlat remain unchanged.  wshses must
c            not be altered between calls of shses.
c
c     lshses the dimension of the array wshses as it appears in the
c            program that calls shses. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshses must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shses.  define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c               (nt+1)*nlat*nlon
c
c            if isym is nonzero lwork must be at least
c
c               (nt+1)*l2*nlon.
c
c     **************************************************************
c
c     output parameters
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the spherical harmonic synthesis of
c            the arrays a and b at the colatitude point theta(i) =
c            (i-1)*pi/(nlat-1) and longitude point phi(j) =
c            (j-1)*2*pi/nlon. the index ranges are defined above at
c            at the input parameter isym.  for isym=0, g(i,j) is
c            given by the the equations listed below.  symmetric
c            versions are used when isym is greater than zero.
c
c     the normalized associated legendre functions are given by
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     define the maximum (plus one) longitudinal wave number
c     as   mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c          mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then g(i,j) = the sum from n=0 to n=nlat-1 of
c
c                   .5*pbar(0,n,theta(i))*a(1,n+1)
c
c              plus the sum from m=1 to m=mmax-1 of
c
c                   the sum from n=m to n=nlat-1 of
c
c              pbar(m,n,theta(i))*(a(m+1,n+1)*cos(m*phi(j))
c                                    -b(m+1,n+1)*sin(m*phi(j)))
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
c            = 9  error in the specification of lshses
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shsesi(nlat,nlon,wshses,lshses,work,lwork,dwork,
c    +                  ldwork,ierror)
c
c     subroutine shsesi initializes the array wshses which can then
c     be used repeatedly by subroutine shses.
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
c     lshses the dimension of the array wshses as it appears in the
c            program that calls shsesi. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshses must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a real   work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in
c            the program that calls shsesi.  define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwork must be at least
c
c               5*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2
c
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shsesi.  ldwork must be at least nlat+1
c
c
c     output parameters
c
c     wshses an array which is initialized for use by subroutine shses.
c            once initialized, wshses can be used repeatedly by shses
c            as long as nlon and nlat remain unchanged.  wshses must
c            not be altered between calls of shses.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshses
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c
c ****************************************************************
      subroutine shses(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshses,lshses,work,lwork,ierror)
      dimension g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),wshses(1),
     1          work(1)
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
      lpimn = (imid*mmax*(nlat+nlat-mmax+1))/2
      if(lshses .lt. lpimn+nlon+15) return
      ierror = 10
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork.lt. nln+ls*nlon) return
      ierror = 0
      ist = 0
      if(isym .eq. 0) ist = imid
      call shses1(nlat,isym,nt,g,idg,jdg,a,b,mdab,ndab,wshses,imid,
     1       ls,nlon,work,work(ist+1),work(nln+1),wshses(lpimn+1))
      return
      end
      subroutine shses1(nlat,isym,nt,g,idgs,jdgs,a,b,mdab,ndab,p,imid,
     1                  idg,jdg,ge,go,work,whrfft)
      dimension g(idgs,jdgs,1),a(mdab,ndab,1),b(mdab,ndab,1),p(imid,1),
     1          ge(idg,jdg,1),go(idg,jdg,1),work(1),whrfft(1)
      ls = idg
      nlon = jdg
      mmax = min0(nlat,nlon/2+1)
      mdo = mmax
      if(mdo+mdo-1 .gt. nlon) mdo = mmax-1
      nlp1 = nlat+1
      modl = mod(nlat,2)
      imm1 = imid
      if(modl .ne. 0) imm1 = imid-1
      do 80 k=1,nt
      do 80 j=1,nlon
      do 80 i=1,ls
      ge(i,j,k) = 0.
 8000 continue
  800 continue
   80 continue
      if(isym .eq. 1) go to 125
      do 100 k=1,nt
      do 100 np1=1,nlat,2
      do 100 i=1,imid
      ge(i,1,k)=ge(i,1,k)+a(1,np1,k)*p(i,np1)
  100 continue
      ndo = nlat
      if(mod(nlat,2) .eq. 0) ndo = nlat-1
      do 110 mp1=2,mdo
      m = mp1-1
      mb = m*(nlat-1)-(m*(m-1))/2
      do 110 np1=mp1,ndo,2
      mn = mb+np1
      do 110 k=1,nt
      do 110 i=1,imid
      ge(i,2*mp1-2,k) = ge(i,2*mp1-2,k)+a(mp1,np1,k)*p(i,mn)
      ge(i,2*mp1-1,k) = ge(i,2*mp1-1,k)+b(mp1,np1,k)*p(i,mn)
  110 continue
      if(mdo .eq. mmax .or. mmax .gt. ndo) go to 122
      mb = mdo*(nlat-1)-(mdo*(mdo-1))/2
      do 120 np1=mmax,ndo,2
      mn = mb+np1
      do 120 k=1,nt
      do 120 i=1,imid
      ge(i,2*mmax-2,k) = ge(i,2*mmax-2,k)+a(mmax,np1,k)*p(i,mn)
  120 continue
  122 if(isym .eq. 2) go to 155
  125 do 140 k=1,nt
      do 140 np1=2,nlat,2
      do 140 i=1,imm1
      go(i,1,k)=go(i,1,k)+a(1,np1,k)*p(i,np1)
  140 continue
      ndo = nlat
      if(mod(nlat,2) .ne. 0) ndo = nlat-1
      do 150 mp1=2,mdo
      mp2 = mp1+1
      m = mp1-1
      mb = m*(nlat-1)-(m*(m-1))/2
      do 150 np1=mp2,ndo,2
      mn = mb+np1
      do 150 k=1,nt
      do 150 i=1,imm1
      go(i,2*mp1-2,k) = go(i,2*mp1-2,k)+a(mp1,np1,k)*p(i,mn)
      go(i,2*mp1-1,k) = go(i,2*mp1-1,k)+b(mp1,np1,k)*p(i,mn)
  150 continue
      mp2 = mmax+1
      if(mdo .eq. mmax .or. mp2 .gt. ndo) go to 155
      mb = mdo*(nlat-1)-(mdo*(mdo-1))/2
      do 152 np1=mp2,ndo,2
      mn = mb+np1
      do 152 k=1,nt
      do 152 i=1,imm1
      go(i,2*mmax-2,k) = go(i,2*mmax-2,k)+a(mmax,np1,k)*p(i,mn)
  152 continue
  155 do 160 k=1,nt
      if(mod(nlon,2) .ne. 0) go to 157
      do 156 i=1,ls
      ge(i,nlon,k) = 2.*ge(i,nlon,k)
  156 continue
  157 call hrfftb(ls,nlon,ge(1,1,k),ls,whrfft,work)
  160 continue
      if(isym .ne. 0) go to 180
      do 170 k=1,nt
      do 170 j=1,nlon
      do 175 i=1,imm1
      g(i,j,k) = .5*(ge(i,j,k)+go(i,j,k))
      g(nlp1-i,j,k) = .5*(ge(i,j,k)-go(i,j,k))
  175 continue
      if(modl .eq. 0) go to 170
      g(imid,j,k) = .5*ge(imid,j,k)
  170 continue
      return
  180 do 185 k=1,nt
      do 185 i=1,imid
      do 185 j=1,nlon
      g(i,j,k) = .5*ge(i,j,k)
  185 continue
      return
      end

      subroutine shsesi(nlat,nlon,wshses,lshses,work,lwork,dwork,
     +                  ldwork,ierror)
      dimension wshses(*),work(*)
      double precision dwork(*)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      mmax = min0(nlat,nlon/2+1)
      imid = (nlat+1)/2
      lpimn = (imid*mmax*(nlat+nlat-mmax+1))/2
      if(lshses .lt. lpimn+nlon+15) return
      ierror = 4
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lwork .lt. 5*nlat*imid + labc) return
      ierror = 5
      if (ldwork .lt. nlat+1) return
      ierror = 0
      iw1 = 3*nlat*imid+1
      CALL SES1(NLAT,NLON,IMID,WSHSES,WORK,WORK(IW1),DWORK)
      call hrffti(nlon,wshses(lpimn+1))
      return
      end
