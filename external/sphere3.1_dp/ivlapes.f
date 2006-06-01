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
c
c ... file ivlapes.f
c
c     this file includes documentation and code for
c     subroutine ivlapes
c
c ... files which must be loaded with ivlapes.f
c
c     sphcom.f, hrfft.f, vhaes.f, vhses.f
c
c
c
c     subroutine ivlapes(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +mdbc,ndbc,wvhses,lvhses,work,lwork,ierror)
c
c
c     subroutine ivlapes computes a the vector field (v,w) whose vector
c     laplacian is (vlap,wlap).  w and wlap are east longitudinal
C*PL*ERROR* Comment line too long
c     components of the vectors.  v and vlap are colatitudinal components
C*PL*ERROR* Comment line too long
c     of the vectors.  br,bi,cr, and ci are the vector harmonic coefficients
c     of (vlap,wlap).  these must be precomputed by vhaes and are input
c     parameters to ivlapes.  (v,w) have the same symmetry or lack of
c     symmetry about the about the equator as (vlap,wlap).  the input
c     parameters ityp,nt,mdbc,ndbc must have the same values used by
c     vhaes to compute br,bi,cr, and ci for (vlap,wlap).
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
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
C*PL*ERROR* Comment line too long
c     ityp   this parameter should have the same value input to subroutine
c            vhaes to compute the coefficients br,bi,cr, and ci for the
c            vector field (vlap,wlap).  ityp is set as follows:
c
C*PL*ERROR* Comment line too long
c            = 0  no symmetries exist in (vlap,wlap) about the equator. (v,w)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 arrays v(i,j) and w(i,j) for i=1,...,nlat and j=1,...,nlon.
c
C*PL*ERROR* Comment line too long
c            = 1  no symmetries exist in (vlap,wlap) about the equator. (v,w)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 v(i,j) and w(i,j) for i=1,...,nlat and j=1,...,nlon.  the
C*PL*ERROR* Comment line too long
c                 vorticity of (vlap,wlap) is zero so the coefficients cr and
C*PL*ERROR* Comment line too long
c                 ci are zero and are not used.  the vorticity of (v,w) is
c                 also zero.
c
c
C*PL*ERROR* Comment line too long
c            = 2  no symmetries exist in (vlap,wlap) about the equator. (v,w)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 w(i,j) and v(i,j) for i=1,...,nlat and j=1,...,nlon.  the
C*PL*ERROR* Comment line too long
c                 divergence of (vlap,wlap) is zero so the coefficients br and
C*PL*ERROR* Comment line too long
c                 bi are zero and are not used.  the divergence of (v,w) is
c                 also zero.
c
c            = 3  wlap is antisymmetric and vlap is symmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is antisymmetric and v is symmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays v(i,j),
C*PL*ERROR* Comment line too long
c                 w(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays v(i,j),w(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  wlap is antisymmetric and vlap is symmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is antisymmetric and v is symmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays v(i,j),
C*PL*ERROR* Comment line too long
c                 w(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays v(i,j),w(i,j) for
C*PL*ERROR* Comment line too long
c                 i=1,...,nlat/2 and j=1,...,nlon.  the vorticity of (vlap,
c                 wlap) is zero so the coefficients cr,ci are zero and
c                 are not used. the vorticity of (v,w) is also zero.
c
c            = 5  wlap is antisymmetric and vlap is symmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is antisymmetric and v is symmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
C*PL*ERROR* Comment line too long
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
C*PL*ERROR* Comment line too long
c                 i=1,...,nlat/2 and j=1,...,nlon.  the divergence of (vlap,
c                 wlap) is zero so the coefficients br,bi are zero and
c                 are not used. the divergence of (v,w) is also zero.
c
c
c            = 6  wlap is symmetric and vlap is antisymmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is symmetric and v is antisymmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
C*PL*ERROR* Comment line too long
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  wlap is symmetric and vlap is antisymmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is symmetric and v is antisymmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
C*PL*ERROR* Comment line too long
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
C*PL*ERROR* Comment line too long
c                 i=1,...,nlat/2 and j=1,...,nlon.  the vorticity of (vlap,
c                 wlap) is zero so the coefficients cr,ci are zero and
c                 are not used. the vorticity of (v,w) is also zero.
c
c            = 8  wlap is symmetric and vlap is antisymmetric about the
C*PL*ERROR* Comment line too long
c                 equator. consequently w is symmetric and v is antisymmetric.
C*PL*ERROR* Comment line too long
c                 (v,w) is computed and stored on the northern hemisphere
C*PL*ERROR* Comment line too long
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
C*PL*ERROR* Comment line too long
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
C*PL*ERROR* Comment line too long
c                 i=1,...,nlat/2 and j=1,...,nlon.  the divergence of (vlap,
c                 wlap) is zero so the coefficients br,bi are zero and
c                 are not used. the divergence of (v,w) is also zero.
c
c
C*PL*ERROR* Comment line too long
c     nt     nt is the number of vector fields (vlap,wlap). some computational
c            efficiency is obtained for multiple fields.  in the program
c            that calls ivlapes, the arrays v,w,br,bi,cr and ci can be
C*PL*ERROR* Comment line too long
c            three dimensional corresponding to an indexed multiple vector
C*PL*ERROR* Comment line too long
c            field.  in this case multiple vector synthesis will be performed
c            to compute the (v,w) for each field (vlap,wlap).  the third
C*PL*ERROR* Comment line too long
c            index is the synthesis index which assumes the values k=1,...,nt.
c            for a single synthesis set nt=1.  the description of the
c            remaining parameters is simplified by assuming that nt=1 or
c            that all arrays are two dimensional.
c
c   idvw     the first dimension of the arrays w and v as it appears in
C*PL*ERROR* Comment line too long
c            the program that calls ivlapes.  if ityp=0,1, or 2  then idvw
C*PL*ERROR* Comment line too long
c            must be at least nlat.  if ityp > 2 and nlat is even then idvw
C*PL*ERROR* Comment line too long
c            must be at least nlat/2. if ityp > 2 and nlat is odd then idvw
c            must be at least (nlat+1)/2.
c
c   jdvw     the second dimension of the arrays w and v as it appears in
c            the program that calls ivlapes. jdvw must be at least nlon.
c
c
c   br,bi    two or three dimensional arrays (see input parameter nt)
c   cr,ci    that contain vector spherical harmonic coefficients of the
c            vector field (vlap,wlap) as computed by subroutine vhaes.
c            br,bi,cr and ci must be computed by vhaes prior to calling
c            ivlapes.  if ityp=1,4, or 7 then cr,ci are not used and can
c            be dummy arguments.  if ityp=2,5, or 8 then br,bi are not
c            used and can be dummy arguments.
c
c    mdbc    the first dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls ivlapes.  mdbc must be
c            at least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndbc    the second dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls ivlapes. ndbc must be at
c            least nlat.
c
c    wvhses  an array which must be initialized by subroutine vhsesi.
c            once initialized, wvhses
c            can be used repeatedly by ivlapes as long as nlat and nlon
c            remain unchanged.  wvhses must not be altered between calls
c            of ivlapes.
c
c    lvhses  the dimension of the array wvhses as it appears in the
c            program that calls ivlapes.  let
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            let
c
c               lsavmin = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c            then lvhses must be greater than or equal to lsavmin
c            (see ierror=9 below).
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls ivlapes. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if ityp .le. 2 then
c
c               (2*nt+1)*nlat*nlon + nlat*(4*nt*l1+1)
c
c            or if ityp .gt. 2 then
c
c               (2*nt+1)*l2*nlon + nlat*(4*nt*l1+1)
c
c            will suffice as a length for lwork.
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c    v,w     two or three dimensional arrays (see input parameter nt) that
C*PL*ERROR* Comment line too long
c            contain a vector field whose vector laplacian is (vlap,wlap).
C*PL*ERROR* Comment line too long
c            w(i,j) is the east longitude and v(i,j) is the colatitudinal
c            component of the vector. v(i,j) and w(i,j) are given on the
c            sphere at the colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            for i=1,...,nlat and east longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for j=1,...,nlon.
c
C*PL*ERROR* Comment line too long
c            let cost and sint be the cosine and sine at colatitude theta.
C*PL*ERROR* Comment line too long
c            let d( )/dlambda  and d( )/dtheta be the first order partial
C*PL*ERROR* Comment line too long
c            derivatives in longitude and colatitude.  let sf be either v
c            or w.  define:
c
c                 del2s(sf) = [d(sint*d(sf)/dtheta)/dtheta +
c                               2            2
c                              d (sf)/dlambda /sint]/sint
c
c            then the vector laplacian of (v,w) in (vlap,wlap) satisfies
c
c                 vlap = del2s(v) + (2*cost*dw/dlambda - v)/sint**2
c
c            and
c
c                 wlap = del2s(w) - (2*cost*dv/dlambda + w)/sint**2
c
c
C*PL*ERROR* Comment line too long
c  ierror    a parameter which flags errors in input parameters as follows:
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of idvw
c
c            = 6  error in the specification of jdvw
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lvhses
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c
c     end of documentation for ivlapes
c
c **********************************************************************
c
      SUBROUTINE DIVLAPES(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,
     +                   MDBC,NDBC,WVHSES,LVHSES,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHSES
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT)
      DIMENSION BR(MDBC,NDBC,NT),BI(MDBC,NDBC,NT)
      DIMENSION CR(MDBC,NDBC,NT),CI(MDBC,NDBC,NT)
      DIMENSION WVHSES(LVHSES),WORK(LWORK)

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
      IF (MDBC.LT.MMAX) RETURN
      IERROR = 8
      IF (NDBC.LT.NLAT) RETURN
      IERROR = 9
c
c     set minimum and verify saved workspace length
c
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      LSAVMIN = LZIMN + LZIMN + NLON + 15
      IF (LVHSES.LT.LSAVMIN) RETURN
c
c     set minimum and verify unsaved work space length
c
      MN = MMAX*NLAT*NT
      L2 = (NLAT+1)/2
      L1 = MIN0(NLAT, (NLON+1)/2)
      IF (ITYP.LE.2) THEN
          LWKMIN = (2*NT+1)*NLAT*NLON + NLAT* (4*NT*L1+1)
      ELSE
          LWKMIN = (2*NT+1)*L2*NLON + NLAT* (4*NT*L1+1)
      END IF
      IF (LWORK.LT.LWKMIN) RETURN
      IERROR = 0
c
c     set work space pointers for vector laplacian coefficients
c
      IF (ITYP.EQ.0 .OR. ITYP.EQ.3 .OR. ITYP.EQ.6) THEN
          IBR = 1
          IBI = IBR + MN
          ICR = IBI + MN
          ICI = ICR + MN
      ELSE IF (ITYP.EQ.1 .OR. ITYP.EQ.4 .OR. ITYP.EQ.7) THEN
          IBR = 1
          IBI = IBR + MN
          ICR = IBI + MN
          ICI = ICR
      ELSE
          IBR = 1
          IBI = 1
          ICR = IBI + MN
          ICI = ICR + MN
      END IF
      IFN = ICI + MN
      IWK = IFN + NLAT
      IF (ITYP.EQ.0 .OR. ITYP.EQ.3 .OR. ITYP.EQ.6) THEN
          LIWK = LWORK - 4*MN - NLAT
      ELSE
          LIWK = LWORK - 2*MN - NLAT
      END IF
      CALL DIVLAPES1(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,WORK(IBR),
     +               WORK(IBI),WORK(ICR),WORK(ICI),MMAX,WORK(IFN),MDBC,
     +               NDBC,BR,BI,CR,CI,WVHSES,LVHSES,WORK(IWK),LIWK,
     +               IERROR)
      RETURN
      END

      SUBROUTINE DIVLAPES1(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BRVW,BIVW,
     +                    CRVW,CIVW,MMAX,FNN,MDBC,NDBC,BR,BI,CR,CI,
     +                    WSAVE,LSAVE,WK,LWK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BRVW
      DOUBLE PRECISION BIVW
      DOUBLE PRECISION CRVW
      DOUBLE PRECISION CIVW
      DOUBLE PRECISION FNN
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT)
      DIMENSION FNN(NLAT),BRVW(MMAX,NLAT,NT),BIVW(MMAX,NLAT,NT)
      DIMENSION CRVW(MMAX,NLAT,NT),CIVW(MMAX,NLAT,NT)
      DIMENSION BR(MDBC,NDBC,NT),BI(MDBC,NDBC,NT)
      DIMENSION CR(MDBC,NDBC,NT),CI(MDBC,NDBC,NT)
      DIMENSION WSAVE(LSAVE),WK(LWK)
c
c     preset coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          FNN(N) = -1.0D0/ (FN* (FN+1.D0))
    1 CONTINUE
c
c     set (u,v) coefficients from br,bi,cr,ci
c
      IF (ITYP.EQ.0 .OR. ITYP.EQ.3 .OR. ITYP.EQ.6) THEN
c
c     all coefficients needed
c
          DO 2 K = 1,NT
              DO 3 N = 1,NLAT
                  DO 4 M = 1,MMAX
                      BRVW(M,N,K) = 0.0D0
                      BIVW(M,N,K) = 0.0D0
                      CRVW(M,N,K) = 0.0D0
                      CIVW(M,N,K) = 0.0D0
    4             CONTINUE
    3         CONTINUE
              DO 5 N = 2,NLAT
                  BRVW(1,N,K) = FNN(N)*BR(1,N,K)
                  BIVW(1,N,K) = FNN(N)*BI(1,N,K)
                  CRVW(1,N,K) = FNN(N)*CR(1,N,K)
                  CIVW(1,N,K) = FNN(N)*CI(1,N,K)
    5         CONTINUE
              DO 6 M = 2,MMAX
                  DO 7 N = M,NLAT
                      BRVW(M,N,K) = FNN(N)*BR(M,N,K)
                      BIVW(M,N,K) = FNN(N)*BI(M,N,K)
                      CRVW(M,N,K) = FNN(N)*CR(M,N,K)
                      CIVW(M,N,K) = FNN(N)*CI(M,N,K)
    7             CONTINUE
    6         CONTINUE
    2     CONTINUE
      ELSE IF (ITYP.EQ.1 .OR. ITYP.EQ.4 .OR. ITYP.EQ.7) THEN
c
c     vorticity is zero so cr,ci=0 not used
c
          DO 12 K = 1,NT
              DO 13 N = 1,NLAT
                  DO 14 M = 1,MMAX
                      BRVW(M,N,K) = 0.0D0
                      BIVW(M,N,K) = 0.0D0
   14             CONTINUE
   13         CONTINUE
              DO 15 N = 2,NLAT
                  BRVW(1,N,K) = FNN(N)*BR(1,N,K)
                  BIVW(1,N,K) = FNN(N)*BI(1,N,K)
   15         CONTINUE
              DO 16 M = 2,MMAX
                  DO 17 N = M,NLAT
                      BRVW(M,N,K) = FNN(N)*BR(M,N,K)
                      BIVW(M,N,K) = FNN(N)*BI(M,N,K)
   17             CONTINUE
   16         CONTINUE
   12     CONTINUE
      ELSE
c
c     divergence is zero so br,bi=0 not used
c
          DO 22 K = 1,NT
              DO 23 N = 1,NLAT
                  DO 24 M = 1,MMAX
                      CRVW(M,N,K) = 0.0D0
                      CIVW(M,N,K) = 0.0D0
   24             CONTINUE
   23         CONTINUE
              DO 25 N = 2,NLAT
                  CRVW(1,N,K) = FNN(N)*CR(1,N,K)
                  CIVW(1,N,K) = FNN(N)*CI(1,N,K)
   25         CONTINUE
              DO 26 M = 2,MMAX
                  DO 27 N = M,NLAT
                      CRVW(M,N,K) = FNN(N)*CR(M,N,K)
                      CIVW(M,N,K) = FNN(N)*CI(M,N,K)
   27             CONTINUE
   26         CONTINUE
   22     CONTINUE
      END IF
c
c     sythesize coefs into vector field (v,w)
c
      CALL DVHSES(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BRVW,BIVW,CRVW,CIVW,
     +           MMAX,NLAT,WSAVE,LSAVE,WK,LWK,IERROR)
      RETURN
      END
