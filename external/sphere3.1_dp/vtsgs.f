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
c ... file vtsgs.f
c
c     this file includes documentation and code for
c     subroutines vtsgs and vtsgsi
c
c ... files which must be loaded with vtsgs.f
c
c     sphcom.f, hrfft.f, vhags.f, vhsgs.f,gaqd.f
c
c
c     subroutine vtsgs(nlat,nlon,ityp,nt,vt,wt,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvts,lwvts,work,lwork,ierror)
c
c     given the vector harmonic analysis br,bi,cr, and ci (computed
c     by subroutine vhags) of some vector function (v,w), this
c     subroutine computes the vector function (vt,wt) which is
c     the derivative of (v,w) with respect to colatitude theta. vtsgs
c     is similar to vhsgs except the vector harmonics are replaced by
c     their derivative with respect to colatitude with the result that
c     (vt,wt) is computed instead of (v,w). vt(i,j) is the derivative
c     of the colatitudinal component v(i,j) at the gaussian colatitude
c     point theta(i) and longitude phi(j) = (j-1)*2*pi/nlon. the
c     spectral representation of (vt,wt) is given below at output
c     parameters vt,wt.
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
c     nt     the number of syntheses.  in the program that calls vtsgs,
c            the arrays vt,wt,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays vt,wt as it appears in
c            the program that calls vtsgs. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays vt,wt as it appears in
c            the program that calls vtsgs. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            of (v,w) as computed by subroutine vhags.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsgs. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsgs. ndab must be at
c            least nlat.
c
c     wvts   an array which must be initialized by subroutine vtsgsi.
c            once initialized, wvts can be used repeatedly by vtsgs
c            as long as nlon and nlat remain unchanged.  wvts must
c            not be altered between calls of vtsgs.
c
c     lwvts  the dimension of the array wvts as it appears in the
c            program that calls vtsgs. define
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
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vtsgs. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c                       (2*nt+1)*nlat*nlon
c
c            if ityp .gt. 2 then lwork must be at least
c
c                        (2*nt+1)*l2*nlon
c
c     **************************************************************
c
c     output parameters
c
c     vt,wt  two or three dimensional arrays (see input parameter nt)
c            in which the derivative of (v,w) with respect to
c            colatitude theta is stored. vt(i,j),wt(i,j) contain the
c            derivatives at gaussian colatitude points theta(i) for
c            i=1,...,nlat and longitude phi(j) = (j-1)*2*pi/nlon.
c            the index ranges are defined above at the input parameter
c            ityp. vt and wt are computed from the formulas for v and
c            w given in subroutine vhsgs but with vbar and wbar replaced
c           with their derivatives with respect to colatitude. these
c            derivatives are denoted by vtbar and wtbar.
c
c
c   *************************************************************
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
c     subroutine vtsgsi(nlat,nlon,wvts,lwvts,work,lwork,dwork,ldwork,
c    +                  ierror)
c
c     subroutine vtsgsi initializes the array wvts which can then be
c     used repeatedly by subroutine vtsgs until nlat or nlon is changed.
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
c            program that calls vtsgs. define
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
c                  l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vtsgs. lwork must be at least
c
c            3*(max0(l1-2,0)*(nlat+nlat-l1-1))/2+(5*l2+2)*nlat
c
C*PL*ERROR* Comment line too long
c     dwork  a double precision work array that does not have to be saved
c
c     ldwork the length of dwork.  ldwork must be at least
c            3*nlat+2
c
c     **************************************************************
c
c     output parameters
c
c     wvts   an array which is initialized for use by subroutine vtsgs.
c            once initialized, wvts can be used repeatedly by vtsgs
c            as long as nlat or nlon remain unchanged.  wvts must not
c            be altered between calls of vtsgs.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lwvts
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c
      SUBROUTINE DVTSGS(NLAT,NLON,ITYP,NT,VT,WT,IDVW,JDVW,BR,BI,CR,CI,
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
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      IF (LWVTS.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 10
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IF (LWORK.LT.LNL+LNL+IDV*NLON) RETURN
      IERROR = 0
      IST = 0
      IF (ITYP.LE.2) IST = IMID
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL
      JW1 = LZIMN + 1
      JW2 = JW1 + LZIMN
      CALL DVTSGS1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,VT,WT,MDAB,NDAB,BR,
     +            BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +            WORK(IW4),IDZ,WVTS,WVTS(JW1),WVTS(JW2))
      RETURN
      END
      SUBROUTINE DVTSGS1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,VT,WT,MDAB,
     +                  NDAB,BR,BI,CR,CI,IDV,VTE,VTO,WTE,WTO,WORK,IDZ,
     +                  VB,WB,WRFFT)
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
      DOUBLE PRECISION WORK
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION WRFFT
      DIMENSION VT(IDVW,JDVW,1),WT(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),
     +          VTE(IDV,NLON,1),VTO(IDV,NLON,1),WTE(IDV,NLON,1),
     +          WTO(IDV,NLON,1),WORK(1),WRFFT(1),VB(IMID,1),WB(IMID,1)

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
c     case m = 0
c
    1 DO 15 K = 1,NT
          DO 15 NP1 = 2,NDO2,2
              DO 15 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
   15 CONTINUE
      DO 16 K = 1,NT
          DO 16 NP1 = 3,NDO1,2
              DO 16 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
   16 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 30 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 26
          DO 25 K = 1,NT
              DO 24 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 23 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
   23             CONTINUE
                  IF (MLAT.EQ.0) GO TO 24
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,MN)
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,MN)
   24         CONTINUE
   25     CONTINUE
   26     IF (MP2.GT.NDO2) GO TO 30
          DO 29 K = 1,NT
              DO 28 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 27 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
   27             CONTINUE
                  IF (MLAT.EQ.0) GO TO 28
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,MN)
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,MN)
   28         CONTINUE
   29     CONTINUE
   30 CONTINUE
      GO TO 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
c     case m = 0
c
  100 DO 115 K = 1,NT
          DO 115 NP1 = 2,NDO2,2
              DO 115 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  115 CONTINUE
      DO 116 K = 1,NT
          DO 116 NP1 = 3,NDO1,2
              DO 116 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  116 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 130 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 126
          DO 125 K = 1,NT
              DO 124 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 123 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  123             CONTINUE
                  IF (MLAT.EQ.0) GO TO 124
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,MN)
  124         CONTINUE
  125     CONTINUE
  126     IF (MP2.GT.NDO2) GO TO 130
          DO 129 K = 1,NT
              DO 128 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 127 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  127             CONTINUE
                  IF (MLAT.EQ.0) GO TO 128
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,MN)
  128         CONTINUE
  129     CONTINUE
  130 CONTINUE
      GO TO 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
c     case m = 0
c
  200 DO 215 K = 1,NT
          DO 215 NP1 = 2,NDO2,2
              DO 215 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  215 CONTINUE
      DO 216 K = 1,NT
          DO 216 NP1 = 3,NDO1,2
              DO 216 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  216 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 230 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 226
          DO 225 K = 1,NT
              DO 224 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 223 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  223             CONTINUE
                  IF (MLAT.EQ.0) GO TO 224
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,MN)
  224         CONTINUE
  225     CONTINUE
  226     IF (MP2.GT.NDO2) GO TO 230
          DO 229 K = 1,NT
              DO 228 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 227 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  227             CONTINUE
                  IF (MLAT.EQ.0) GO TO 228
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,MN)
  228         CONTINUE
  229     CONTINUE
  230 CONTINUE
      GO TO 950
c
c     case ityp=3   v odd,  w even
c
c     case m = 0
c
  300 DO 315 K = 1,NT
          DO 315 NP1 = 2,NDO2,2
              DO 315 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  315 CONTINUE
      DO 316 K = 1,NT
          DO 316 NP1 = 3,NDO1,2
              DO 316 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  316 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 330 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 326
          DO 325 K = 1,NT
              DO 324 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 323 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  323             CONTINUE
                  IF (MLAT.EQ.0) GO TO 324
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,MN)
  324         CONTINUE
  325     CONTINUE
  326     IF (MP2.GT.NDO2) GO TO 330
          DO 329 K = 1,NT
              DO 328 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 327 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  327             CONTINUE
                  IF (MLAT.EQ.0) GO TO 328
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,MN)
  328         CONTINUE
  329     CONTINUE
  330 CONTINUE
      GO TO 950
c
c     case ityp=4   v odd,  w even, and both cr and ci equal zero
c
c     case m = 0
c
  400 DO 415 K = 1,NT
          DO 415 NP1 = 2,NDO2,2
              DO 415 I = 1,IMM1
                  VTO(I,1,K) = VTO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  415 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 430 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 430
          DO 429 K = 1,NT
              DO 428 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 427 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  427             CONTINUE
                  IF (MLAT.EQ.0) GO TO 428
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(IMID,MN)
  428         CONTINUE
  429     CONTINUE
  430 CONTINUE
      GO TO 950
c
c     case ityp=5   v odd,  w even,     br and bi equal zero
c
c     case m = 0
c
  500 DO 516 K = 1,NT
          DO 516 NP1 = 3,NDO1,2
              DO 516 I = 1,IMID
                  WTE(I,1,K) = WTE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  516 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 530 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 530
          DO 525 K = 1,NT
              DO 524 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 523 I = 1,IMM1
                      VTO(I,2*MP1-2,K) = VTO(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTO(I,2*MP1-1,K) = VTO(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTE(I,2*MP1-2,K) = WTE(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTE(I,2*MP1-1,K) = WTE(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  523             CONTINUE
                  IF (MLAT.EQ.0) GO TO 524
                  WTE(IMID,2*MP1-2,K) = WTE(IMID,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(IMID,MN)
                  WTE(IMID,2*MP1-1,K) = WTE(IMID,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(IMID,MN)
  524         CONTINUE
  525     CONTINUE
  530 CONTINUE
      GO TO 950
c
c     case ityp=6   v even  ,  w odd
c
c     case m = 0
c
  600 DO 615 K = 1,NT
          DO 615 NP1 = 2,NDO2,2
              DO 615 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  615 CONTINUE
      DO 616 K = 1,NT
          DO 616 NP1 = 3,NDO1,2
              DO 616 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  616 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 630 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 626
          DO 625 K = 1,NT
              DO 624 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 623 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  623             CONTINUE
                  IF (MLAT.EQ.0) GO TO 624
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,MN)
  624         CONTINUE
  625     CONTINUE
  626     IF (MP2.GT.NDO2) GO TO 630
          DO 629 K = 1,NT
              DO 628 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 627 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  627             CONTINUE
                  IF (MLAT.EQ.0) GO TO 628
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,MN)
  628         CONTINUE
  629     CONTINUE
  630 CONTINUE
      GO TO 950
c
c     case ityp=7   v even, w odd   cr and ci equal zero
c
c     case m = 0
c
  700 DO 716 K = 1,NT
          DO 716 NP1 = 3,NDO1,2
              DO 716 I = 1,IMID
                  VTE(I,1,K) = VTE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  716 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 730 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 730
          DO 725 K = 1,NT
              DO 724 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 723 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) +
     +                                   BR(MP1,NP1,K)*VB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   BI(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   BI(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) +
     +                                   BR(MP1,NP1,K)*WB(I,MN)
  723             CONTINUE
                  IF (MLAT.EQ.0) GO TO 724
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(IMID,MN)
  724         CONTINUE
  725     CONTINUE
  730 CONTINUE
      GO TO 950
c
c     case ityp=8   v even,  w odd,   br and bi equal zero
c
c     case m = 0
c
  800 DO 815 K = 1,NT
          DO 815 NP1 = 2,NDO2,2
              DO 815 I = 1,IMM1
                  WTO(I,1,K) = WTO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  815 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 830 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 830
          DO 829 K = 1,NT
              DO 828 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 827 I = 1,IMM1
                      VTE(I,2*MP1-2,K) = VTE(I,2*MP1-2,K) -
     +                                   CI(MP1,NP1,K)*WB(I,MN)
                      VTE(I,2*MP1-1,K) = VTE(I,2*MP1-1,K) +
     +                                   CR(MP1,NP1,K)*WB(I,MN)
                      WTO(I,2*MP1-2,K) = WTO(I,2*MP1-2,K) -
     +                                   CR(MP1,NP1,K)*VB(I,MN)
                      WTO(I,2*MP1-1,K) = WTO(I,2*MP1-1,K) -
     +                                   CI(MP1,NP1,K)*VB(I,MN)
  827             CONTINUE
                  IF (MLAT.EQ.0) GO TO 828
                  VTE(IMID,2*MP1-2,K) = VTE(IMID,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(IMID,MN)
                  VTE(IMID,2*MP1-1,K) = VTE(IMID,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(IMID,MN)
  828         CONTINUE
  829     CONTINUE
  830 CONTINUE
  950 DO 14 K = 1,NT
          CALL DHRFFTB(IDV,NLON,VTE(1,1,K),IDV,WRFFT,WORK)
          CALL DHRFFTB(IDV,NLON,WTE(1,1,K),IDV,WRFFT,WORK)
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
      SUBROUTINE DVTSGSI(NLAT,NLON,WVTS,LWVTS,WORK,LWORK,DWORK,LDWORK,
     +                  IERROR)
      DOUBLE PRECISION WVTS
      DOUBLE PRECISION WORK
c
c     define imid = (nlat+1)/2 and mmax = min0(nlat,(nlon+1)/2)
c     the length of wvts is imid*mmax*(nlat+nlat-mmax+1)+nlon+15
c     and the length of work is labc+5*nlat*imid+2*nlat where
c     labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c
      DIMENSION WVTS(LWVTS),WORK(LWORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      MMAX = MIN0(NLAT,NLON/2+1)
      IMID = (NLAT+1)/2
      LZIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
      IF (LWVTS.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 4
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      LVIN = 3*NLAT*IMID
      LWVBIN = 2*NLAT*IMID + LABC
      LTHETA = NLAT + NLAT
      IF (LWORK.LT.LVIN+LWVBIN+LTHETA) RETURN
      IERROR = 5
      IF (LDWORK.LT.3*NLAT+2) RETURN
      IERROR = 0
      IW1 = LVIN + 1
      IW2 = IW1 + LWVBIN
      JW1 = NLAT + 1
      JW2 = JW1 + NLAT
      CALL DVETG1(NLAT,NLON,IMID,WVTS,WVTS(LZIMN+1),WORK,WORK(IW1),
     +            DWORK,DWORK(JW1),DWORK(JW2),IERROR)
      IF (IERROR.NE.0) RETURN
      CALL DHRFFTI(NLON,WVTS(2*LZIMN+1))
      RETURN
      END
      SUBROUTINE DVETG1(NLAT,NLON,IMID,VB,WB,VIN,WVBIN,THETA,WTS,DWORK,
     +                 IERROR)
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION VIN
      DOUBLE PRECISION WVBIN
      DIMENSION VB(IMID,*),WB(IMID,*),VIN(IMID,NLAT,3),WVBIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*),THETA(*),WTS(*)

      MMAX = MIN0(NLAT,NLON/2+1)
      LDWORK = 1
      CALL DGAQD(NLAT,THETA,WTS,DWORK,LDWORK,IERR)
      IF (IERR.EQ.0) GO TO 10
      IERROR = 10 + IERR
      RETURN
   10 CALL DVTGINT(NLAT,NLON,THETA,WVBIN,DWORK)
      DO 33 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DVBIN(0,NLAT,NLON,M,VIN,I3,WVBIN)
          DO 33 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 33 I = 1,IMID
                  VB(I,MN) = VIN(I,NP1,I3)
   33 CONTINUE
      CALL DWTGINT(NLAT,NLON,THETA,WVBIN,DWORK)
      DO 34 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DWBIN(0,NLAT,NLON,M,VIN,I3,WVBIN)
          DO 34 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 34 I = 1,IMID
                  WB(I,MN) = VIN(I,NP1,I3)
   34 CONTINUE
      RETURN
      END
