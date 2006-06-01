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
c
c ... file vtsgc.f
c
c     this file includes documentation and code for
c     subroutines vtsgc and vtsgci
c
c ... files which must be loaded with vtsgc.f
c
c     sphcom.f, hrfft.f, vhagc.f, vhsgc.f,gaqd.f
c
c
c     subroutine vtsgc(nlat,nlon,ityp,nt,vt,wt,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvts,lwvts,work,lwork,ierror)
c
c     given the vector harmonic analysis br,bi,cr, and ci (computed
c     by subroutine vhagc) of some vector function (v,w), this
c     subroutine computes the vector function (vt,wt) which is
c     the derivative of (v,w) with respect to colatitude theta. vtsgc
c     is similar to vhsgc except the vector harmonics are replaced by
c     their derivative with respect to colatitude with the result that
c     (vt,wt) is computed instead of (v,w). vt(i,j) is the derivative
c     of the colatitudinal component v(i,j) at the gaussian colatitude
c     theta(i) and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (vt,wt) is given below at the definition of
c     output parameters vt,wt.
c
c     input parameters
c
c     nlat   the number of gaussian colatitudinal grid points theta(i)
c            such that 0 < theta(1) <...< theta(nlat) < pi. they are
c            computed by subroutine gaqd which is called by this
c            subroutine. if nlat is odd the equator is
c            theta((nlat+1)/2). if nlat is even the equator lies
c            half way between theta(nlat/2) and theta(nlat/2+1). nlat
c            must be at least 3. note: if (v,w) is symmetric about
c            the equator (see parameter ityp below) the number of
c            colatitudinal grid points is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     ityp   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere. i.e. the arrays
c                 vt(i,j),wt(i,j) are computed for i=1,...,nlat and
c                 j=1,...,nlon.
c
c            = 1  no symmetries exist about the equator however the
c                 the coefficients cr and ci are zero which implies
c                 that the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the calculations are performed on the entire sphere.
c                 i.e. the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat and j=1,...,nlon.
c
c            = 2  no symmetries exist about the equator however the
c                 the coefficients br and bi are zero which implies
c                 that the divergence of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the calculations are performed on the entire sphere.
c                 i.e. the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat and j=1,...,nlon.
c
c            = 3  vt is odd and wt is even about the equator. the
c                 synthesis is performed on the northern hemisphere
c                 only.  i.e., if nlat is odd the arrays vt(i,j)
c                 and wt(i,j) are computed for i=1,...,(nlat+1)/2
c                 and j=1,...,nlon. if nlat is even the arrays
c                 are computed for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  vt is odd and wt is even about the equator and the
c                 coefficients cr and ci are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 5  vt is odd and wt is even about the equator and the
c                 coefficients br and bi are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 6  vt is even and wt is odd about the equator. the
c                 synthesis is performed on the northern hemisphere
c                 only.  i.e., if nlat is odd the arrays vt(i,j),wt(i,j)
c                 are computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.
C*PL*ERROR* Comment line too long
c                 if nlat is even the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  vt is even and wt is odd about the equator and the
c                 coefficients cr and ci are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 8  vt is even and wt is odd about the equator and the
c                 coefficients br and bi are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls vtsgc,
c            the arrays vt,wt,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays vt,wt as it appears in
c            the program that calls vtsgc. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays vt,wt as it appears in
c            the program that calls vtsgc. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            of (v,w) as computed by subroutine vhagc.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsgc. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsgc. ndab must be at
c            least nlat.
c
c     wvts   an array which must be initialized by subroutine vtsgci.
c            once initialized, wvts can be used repeatedly by vtsgc
c            as long as nlon and nlat remain unchanged.  wvts must
c            not be altered between calls of vtsgc.
c
c     lwvts  the dimension of the array wvts as it appears in the
c            program that calls vtsgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwvts must be at least
c
c            4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vtsgc. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c                    nlat*(2*nt*nlon+max0(6*l2,nlon))
c
c            if ityp .gt. 2 then lwork must be at least
c
c                    l2*(2*nt*nlon+max0(6*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     vt,wt  two or three dimensional arrays (see input parameter nt)
c            in which the derivative of (v,w) with respect to
c            colatitude theta is stored. vt(i,j),wt(i,j) contain the
c            derivatives at gaussian colatitude points theta(i)
c            and longitude phi(j) = (j-1)*2*pi/nlon. the index ranges
c            are defined above at the input parameter ityp. vt and wt
c            are computed from the formulas for v and w given in
c            subroutine vhsgc but with vbar and wbar replaced with
c            their derivatives with respect to colatitude. these
c            derivatives are denoted by vtbar and wtbar.
c
c   in terms of real variables this expansion takes the form
c
c             for i=1,...,nlat and  j=1,...,nlon
c
c     vt(i,j) = the sum from n=1 to n=nlat-1 of
c
c               .5*br(1,n+1)*vtbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c       (br(m+1,n+1)*vtbar(m,n,theta(i))
c                   -ci(m+1,n+1)*wtbar(m,n,theta(i)))*cos(m*phi(j))
c      -(bi(m+1,n+1)*vtbar(m,n,theta(i))
c                   +cr(m+1,n+1)*wtbar(m,n,theta(i)))*sin(m*phi(j))
c
c    and for i=1,...,nlat and  j=1,...,nlon
c
c     wt(i,j) = the sum from n=1 to n=nlat-1 of
c
c              -.5*cr(1,n+1)*vtbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c      -(cr(m+1,n+1)*vtbar(m,n,theta(i))
c                   +bi(m+1,n+1)*wtbar(m,n,theta(i)))*cos(m*phi(j))
c      +(ci(m+1,n+1)*vtbar(m,n,theta(i))
c                   -br(m+1,n+1)*wtbar(m,n,theta(i)))*sin(m*phi(j))
c
c
c      br(m+1,nlat),bi(m+1,nlat),cr(m+1,nlat), and ci(m+1,nlat) are
c      assumed zero for m even.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of ityp
c            = 4  error in the specification of nt
c            = 5  error in the specification of idvw
c            = 6  error in the specification of jdvw
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lwvts
c            = 10 error in the specification of lwork
c
c
c *******************************************************************
c
c     subroutine vtsgci(nlat,nlon,wvts,lwvts,dwork,ldwork,ierror)
c
c     subroutine vtsgci initializes the array wvts which can then be
c     used repeatedly by subroutine vtsgc until nlat or nlon is changed.
c
c     input parameters
c
c     nlat   the number of gaussian colatitudinal grid points theta(i)
c            such that 0 < theta(1) <...< theta(nlat) < pi. they are
c            computed by subroutine gaqd which is called by this
c            subroutine. if nlat is odd the equator is
c            theta((nlat+1)/2). if nlat is even the equator lies
c            half way between theta(nlat/2) and theta(nlat/2+1). nlat
c            must be at least 3. note: if (v,w) is symmetric about
c            the equator (see parameter ityp below) the number of
c            colatitudinal grid points is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     lwvts  the dimension of the array wvts as it appears in the
c            program that calls vtsgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwvts must be at least
c
c            4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
C*PL*ERROR* Comment line too long
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls vtsgc. ldwork must be at least
c            3*nlat+2
c
c     **************************************************************
c
c     output parameters
c
c     wvts   an array which is initialized for use by subroutine vtsgc.
c            once initialized, wvts can be used repeatedly by vtsgc
c            as long as nlat or nlon remain unchanged.  wvts must not
c            be altered between calls of vtsgc.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lwvts
c            = 4  error in the specification of lwork
c
c
c
c **********************************************************************
c
      SUBROUTINE DVTSGC(NLAT,NLON,ITYP,NT,VT,WT,IDVW,JDVW,BR,BI,CR,CI,
     +                 MDAB,NDAB,WVTS,LWVTS,WORK,LWORK,IERROR)
      DOUBLE PRECISION VT
      DOUBLE PRECISION WT
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVTS
      DOUBLE PRECISION WORK
c
      DIMENSION VT(IDVW,JDVW,1),WT(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),WORK(1),
     +          WVTS(1)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IF (ITYP.LT.0 .OR. ITYP.GT.8) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IMID = (NLAT+1)/2
      IF ((ITYP.LE.2.AND.IDVW.LT.NLAT) .OR.
     +    (ITYP.GT.2.AND.IDVW.LT.IMID)) RETURN
      IERROR = 6
      IF (JDVW.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MDAB.LT.MMAX) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LWVTS.LT.2* (LZZ1+LABC)+NLON+15) RETURN
      IERROR = 10
      IF (ITYP.LE.2 .AND. LWORK.LT.NLAT*
     +    (2*NT*NLON+MAX0(6*IMID,NLON))) RETURN
      IF (ITYP.GT.2 .AND. LWORK.LT.IMID*
     +    (2*NT*NLON+MAX0(6*NLAT,NLON))) RETURN
      IERROR = 0
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IST = 0
      IF (ITYP.LE.2) IST = IMID
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL
      IW5 = IW4 + 3*IMID*NLAT
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      LWZVIN = LZZ1 + LABC
      JW1 = LWZVIN + 1
      JW2 = JW1 + LWZVIN
      CALL DVTSGC1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,VT,WT,MDAB,NDAB,BR,
     +            BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +            WORK(IW4),WORK(IW5),WVTS,WVTS(JW1),WVTS(JW2))
      RETURN
      END
      SUBROUTINE DVTSGC1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,VT,WT,MDAB,
     +                  NDAB,BR,BI,CR,CI,IDV,VTE,VTO,WTE,WTO,VB,WB,
     +                  WVBIN,WWBIN,WRFFT)
      DOUBLE PRECISION VT
      DOUBLE PRECISION WT
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION VTE
      DOUBLE PRECISION VTO
      DOUBLE PRECISION WTE
      DOUBLE PRECISION WTO
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION WVBIN
      DOUBLE PRECISION WWBIN
      DOUBLE PRECISION WRFFT
      DIMENSION VT(IDVW,JDVW,1),WT(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),
     +          VTE(IDV,NLON,1),VTO(IDV,NLON,1),WTE(IDV,NLON,1),
     +          WTO(IDV,NLON,1),WVBIN(1),WWBIN(1),WRFFT(1),
     +          VB(IMID,NLAT,3),WB(IMID,NLAT,3)

      NLP1 = NLAT + 1
      MLAT = MOD(NLAT,2)
      MLON = MOD(NLON,2)
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IMM1 = IMID
      IF (MLAT.NE.0) IMM1 = IMID - 1
      DO 10 K = 1,NT
          DO 10 J = 1,NLON
              DO 10 I = 1,IDV
                  VTE(I,J,K) = 0.D0
                  WTE(I,J,K) = 0.D0
   10 CONTINUE
      NDO1 = NLAT
      NDO2 = NLAT
      IF (MLAT.NE.0) NDO1 = NLAT - 1
      IF (MLAT.EQ.0) NDO2 = NLAT - 1
   18 ITYPP = ITYP + 1
      GO TO (1,100,200,300,400,500,600,700,800) ITYPP
c
c     case ityp=0   no symmetries
c
    1 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 15 K = 1,NT
          DO 15 NP1 = 2,NDO2,2
              DO 15 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
   15 CONTINUE
      DO 16 K = 1,NT
          DO 16 NP1 = 3,NDO1,2
              DO 16 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
   16 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 30 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 26
          DO 25 K = 1,NT
              DO 24 NP1 = MP1,NDO1,2
                  DO 23 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
   23             CONTINUE
                  IF (MLAT.EQ.0) GO TO 24
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,NP1,IV)
   24         CONTINUE
   25     CONTINUE
   26     IF (MP2.GT.NDO2) GO TO 30
          DO 29 K = 1,NT
              DO 28 NP1 = MP2,NDO2,2
                  DO 27 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
   27             CONTINUE
                  IF (MLAT.EQ.0) GO TO 28
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,NP1,IW)
   28         CONTINUE
   29     CONTINUE
   30 CONTINUE
      GO TO 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
  100 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 115 K = 1,NT
          DO 115 NP1 = 2,NDO2,2
              DO 115 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  115 CONTINUE
      DO 116 K = 1,NT
          DO 116 NP1 = 3,NDO1,2
              DO 116 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  116 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 130 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 126
          DO 125 K = 1,NT
              DO 124 NP1 = MP1,NDO1,2
                  DO 123 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  123             CONTINUE
                  IF (MLAT.EQ.0) GO TO 124
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  124         CONTINUE
  125     CONTINUE
  126     IF (MP2.GT.NDO2) GO TO 130
          DO 129 K = 1,NT
              DO 128 NP1 = MP2,NDO2,2
                  DO 127 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  127             CONTINUE
                  IF (MLAT.EQ.0) GO TO 128
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  128         CONTINUE
  129     CONTINUE
  130 CONTINUE
      GO TO 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
  200 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 215 K = 1,NT
          DO 215 NP1 = 2,NDO2,2
              DO 215 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  215 CONTINUE
      DO 216 K = 1,NT
          DO 216 NP1 = 3,NDO1,2
              DO 216 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  216 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 230 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 226
          DO 225 K = 1,NT
              DO 224 NP1 = MP1,NDO1,2
                  DO 223 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  223             CONTINUE
                  IF (MLAT.EQ.0) GO TO 224
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  224         CONTINUE
  225     CONTINUE
  226     IF (MP2.GT.NDO2) GO TO 230
          DO 229 K = 1,NT
              DO 228 NP1 = MP2,NDO2,2
                  DO 227 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  227             CONTINUE
                  IF (MLAT.EQ.0) GO TO 228
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  228         CONTINUE
  229     CONTINUE
  230 CONTINUE
      GO TO 950
c
c     case ityp=3   v odd,  w even
c
  300 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 315 K = 1,NT
          DO 315 NP1 = 2,NDO2,2
              DO 315 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  315 CONTINUE
      DO 316 K = 1,NT
          DO 316 NP1 = 3,NDO1,2
              DO 316 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  316 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 330 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 326
          DO 325 K = 1,NT
              DO 324 NP1 = MP1,NDO1,2
                  DO 323 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  323             CONTINUE
                  IF (MLAT.EQ.0) GO TO 324
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  324         CONTINUE
  325     CONTINUE
  326     IF (MP2.GT.NDO2) GO TO 330
          DO 329 K = 1,NT
              DO 328 NP1 = MP2,NDO2,2
                  DO 327 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  327             CONTINUE
                  IF (MLAT.EQ.0) GO TO 328
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  328         CONTINUE
  329     CONTINUE
  330 CONTINUE
      GO TO 950
c
c     case ityp=4   v odd,  w even, and both cr and ci equal zero
c
  400 CALL DVBIN(1,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 415 K = 1,NT
          DO 415 NP1 = 2,NDO2,2
              DO 415 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  415 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 430 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(1,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(1,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP2.GT.NDO2) GO TO 430
          DO 429 K = 1,NT
              DO 428 NP1 = MP2,NDO2,2
                  DO 427 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  427             CONTINUE
                  IF (MLAT.EQ.0) GO TO 428
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  428         CONTINUE
  429     CONTINUE
  430 CONTINUE
      GO TO 950
c
c     case ityp=5   v odd,  w even,     br and bi equal zero
c
  500 CALL DVBIN(2,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 516 K = 1,NT
          DO 516 NP1 = 3,NDO1,2
              DO 516 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  516 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 530 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(2,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(2,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 530
          DO 525 K = 1,NT
              DO 524 NP1 = MP1,NDO1,2
                  DO 523 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  523             CONTINUE
                  IF (MLAT.EQ.0) GO TO 524
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  524         CONTINUE
  525     CONTINUE
  530 CONTINUE
      GO TO 950
c
c     case ityp=6   v even  ,  w odd
c
  600 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 615 K = 1,NT
          DO 615 NP1 = 2,NDO2,2
              DO 615 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  615 CONTINUE
      DO 616 K = 1,NT
          DO 616 NP1 = 3,NDO1,2
              DO 616 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  616 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 630 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 626
          DO 625 K = 1,NT
              DO 624 NP1 = MP1,NDO1,2
                  DO 623 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  623             CONTINUE
                  IF (MLAT.EQ.0) GO TO 624
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  624         CONTINUE
  625     CONTINUE
  626     IF (MP2.GT.NDO2) GO TO 630
          DO 629 K = 1,NT
              DO 628 NP1 = MP2,NDO2,2
                  DO 627 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  627             CONTINUE
                  IF (MLAT.EQ.0) GO TO 628
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  628         CONTINUE
  629     CONTINUE
  630 CONTINUE
      GO TO 950
c
c     case ityp=7   v even, w odd   cr and ci equal zero
c
  700 CALL DVBIN(2,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 716 K = 1,NT
          DO 716 NP1 = 3,NDO1,2
              DO 716 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  716 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 730 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(2,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(2,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 730
          DO 725 K = 1,NT
              DO 724 NP1 = MP1,NDO1,2
                  DO 723 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,NP1,IW)
  723             CONTINUE
                  IF (MLAT.EQ.0) GO TO 724
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  724         CONTINUE
  725     CONTINUE
  730 CONTINUE
      GO TO 950
c
c     case ityp=8   v even,  w odd   br and bi equal zero
c
  800 CALL DVBIN(1,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 815 K = 1,NT
          DO 815 NP1 = 2,NDO2,2
              DO 815 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  815 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 830 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(1,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(1,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP2.GT.NDO2) GO TO 830
          DO 829 K = 1,NT
              DO 828 NP1 = MP2,NDO2,2
                  DO 827 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,NP1,IV)
  827             CONTINUE
                  IF (MLAT.EQ.0) GO TO 828
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  828         CONTINUE
  829     CONTINUE
  830 CONTINUE
  950 DO 14 K = 1,NT
          CALL DHRFFTB(IDV,NLON,VTE(1,1,K),IDV,WRFFT,VB)
          CALL DHRFFTB(IDV,NLON,WTE(1,1,K),IDV,WRFFT,VB)
   14 CONTINUE
      IF (ITYP.GT.2) GO TO 12
      DO 60 K = 1,NT
          DO 60 J = 1,NLON
              DO 60 I = 1,IMM1
                  VT(I,J,K) = .5D0* (VTE(I,J,K)+VTO(I,J,K))
                  WT(I,J,K) = .5D0* (WTE(I,J,K)+WTO(I,J,K))
                  VT(NLP1-I,J,K) = .5D0* (VTE(I,J,K)-VTO(I,J,K))
                  WT(NLP1-I,J,K) = .5D0* (WTE(I,J,K)-WTO(I,J,K))
   60 CONTINUE
      GO TO 13
   12 DO 11 K = 1,NT
          DO 11 J = 1,NLON
              DO 11 I = 1,IMM1
                  VT(I,J,K) = .5D0*VTE(I,J,K)
                  WT(I,J,K) = .5D0*WTE(I,J,K)
   11 CONTINUE
   13 IF (MLAT.EQ.0) RETURN
      DO 65 K = 1,NT
          DO 65 J = 1,NLON
              VT(IMID,J,K) = .5D0*VTE(IMID,J,K)
              WT(IMID,J,K) = .5D0*WTE(IMID,J,K)
   65 CONTINUE
      RETURN
      END
      SUBROUTINE DVTSGCI(NLAT,NLON,WVTS,LWVTS,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WVTS
c
      DIMENSION WVTS(LWVTS)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IMID = (NLAT+1)/2
      LZZ1 = 2*NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LWVTS.LT.2* (LZZ1+LABC)+NLON+15) RETURN
      IERROR = 4
c     if(lwork .lt. 2*nlat*(nlat+2)) return
      IF (LDWORK.LT.3*NLAT+2) RETURN
c     call gaqd(nlat,work,work(2*nlat+1),work(4*nlat+1),lwork,ierr)
      LDWK = 1
      CALL DGAQD(NLAT,DWORK,DWORK(NLAT+1),DWORK(2*NLAT+1),LDWK,IERR)
      IERROR = 5
      IF (IERR.NE.0) RETURN
      IERROR = 0
c     call vtgint (nlat,nlon,work,wvts,work(4*nlat+1))
      CALL DVTGINT(NLAT,NLON,DWORK,WVTS,DWORK(2*NLAT+1))
      LWVBIN = LZZ1 + LABC
      IW1 = LWVBIN + 1
c     call wtgint (nlat,nlon,work,wvts(iw1),work(4*nlat+1))
      CALL DWTGINT(NLAT,NLON,DWORK,WVTS(IW1),DWORK(2*NLAT+1))
      IW2 = IW1 + LWVBIN
      CALL DHRFFTI(NLON,WVTS(IW2))
      RETURN
      END
