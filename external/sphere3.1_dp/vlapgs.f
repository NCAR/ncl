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
c ... file vlapgs.f
c
c     this file includes documentation and code for
c     subroutine vlapgs          i
c
c ... files which must be loaded with vlapgs.f
c
c     sphcom.f, hrfft.f, vhags.f, vhsgs.f, gaqd.f
c
c
c
C*PL*ERROR* Comment line too long
c     subroutine vlapgs(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,br,bi,cr,ci,
c    +mdbc,ndbc,wvhsgs,lvhsgs,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients (br,bi,cr,ci)
C*PL*ERROR* Comment line too long
c     precomputed by subroutine vhags for a vector field (v,w), subroutine
c     vlapgs computes the vector laplacian of the vector field (v,w)
c     in (vlap,wlap) (see the definition of the vector laplacian at
c     the output parameter description of vlap,wlap below).  w and wlap
c     are east longitudinal components of the vectors.  v and vlap are
c     colatitudinal components of the vectors.  the laplacian components
C*PL*ERROR* Comment line too long
c     in (vlap,wlap) have the same symmetry or lack of symmetry about the
C*PL*ERROR* Comment line too long
c     equator as (v,w).  the input parameters ityp,nt,mdbc,nbdc must have
C*PL*ERROR* Comment line too long
c     the same values used by vhags to compute br,bi,cr, and ci for (v,w).
c     vlap(i,j) and wlap(i,j) are given on the sphere at the gaussian
C*PL*ERROR* Comment line too long
c     colatitude theta(i) (see nlat as input parameter) and east longitude
c     lambda(j) = (j-1)*2*pi/nlon for i=1,...,nlat and j=1,...,nlon.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
C*PL*ERROR* Comment line too long
c            full sphere. these lie in the interval (0,pi) and are computed
C*PL*ERROR* Comment line too long
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
C*PL*ERROR* Comment line too long
c            if nlat is odd the equator will be included as the grid point
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
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
c            vhags to compute the coefficients br,bi,cr, and ci for the
c            vector field (v,w).  ityp is set as follows:
c
C*PL*ERROR* Comment line too long
c            = 0  no symmetries exist in (v,w) about the equator. (vlap,wlap)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
c
c
C*PL*ERROR* Comment line too long
c            = 1  no symmetries exist in (v,w) about the equator. (vlap,wlap)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
C*PL*ERROR* Comment line too long
c                 the vorticity of (v,w) is zero so the coefficients cr and
C*PL*ERROR* Comment line too long
c                 ci are zero and are not used.  the vorticity of (vlap,wlap)
c                 is also zero.
c
c
C*PL*ERROR* Comment line too long
c            = 2  no symmetries exist in (v,w) about the equator. (vlap,wlap)
C*PL*ERROR* Comment line too long
c                 is computed and stored on the entire sphere in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
C*PL*ERROR* Comment line too long
c                 the divergence of (v,w) is zero so the coefficients br and
C*PL*ERROR* Comment line too long
c                 bi are zero and are not used.  the divergence of (vlap,wlap)
c                 is also zero.
c
C*PL*ERROR* Comment line too long
c            = 3  w is antisymmetric and v is symmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
C*PL*ERROR* Comment line too long
c            = 4  w is antisymmetric and v is symmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
C*PL*ERROR* Comment line too long
c                 vorticity of (v,w) is zero so the coefficients cr,ci are
c                 zero and are not used. the vorticity of (vlap,wlap) is
c                 also zero.
c
C*PL*ERROR* Comment line too long
c            = 5  w is antisymmetric and v is symmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 divergence of (v,w) is zero so the coefficients br,bi
C*PL*ERROR* Comment line too long
c                 are zero and are not used. the divergence of (vlap,wlap)
c                 is also zero.
c
c
C*PL*ERROR* Comment line too long
c            = 6  w is symmetric and v is antisymmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
C*PL*ERROR* Comment line too long
c            = 7  w is symmetric and v is antisymmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
C*PL*ERROR* Comment line too long
c                 vorticity of (v,w) is zero so the coefficients cr,ci are
c                 zero and are not used. the vorticity of (vlap,wlap) is
c                 also zero.
c
C*PL*ERROR* Comment line too long
c            = 8  w is symmetric and v is antisymmetric about the equator.
C*PL*ERROR* Comment line too long
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
C*PL*ERROR* Comment line too long
c                 hemisphere only.  if nlat is odd, storage is in the arrays
C*PL*ERROR* Comment line too long
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 divergence of (v,w) is zero so the coefficients br,bi
C*PL*ERROR* Comment line too long
c                 are zero and are not used. the divergence of (vlap,wlap)
c                 is also zero.
c
c
C*PL*ERROR* Comment line too long
c     nt     nt is the number of vector fields (v,w).  some computational
c            efficiency is obtained for multiple fields.  in the program
c            that calls vlapgs, the arrays vlap,wlap,br,bi,cr and ci
C*PL*ERROR* Comment line too long
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple vector synthesis will
C*PL*ERROR* Comment line too long
c            be performed to compute the vector laplacian for each field.
C*PL*ERROR* Comment line too long
c            the third index is the synthesis index which assumes the values
C*PL*ERROR* Comment line too long
c            k=1,...,nt.  for a single synthesis set nt=1.  the description
C*PL*ERROR* Comment line too long
c            of the remaining parameters is simplified by assuming that nt=1
c            or that all arrays are two dimensional.
c
C*PL*ERROR* Comment line too long
c   idvw     the first dimension of the arrays vlap and wlap as it appears
C*PL*ERROR* Comment line too long
c            in the program that calls vlapgs.  if ityp=0,1, or 2  then idvw
C*PL*ERROR* Comment line too long
c            must be at least nlat.  if ityp > 2 and nlat is even then idvw
C*PL*ERROR* Comment line too long
c            must be at least nlat/2. if ityp > 2 and nlat is odd then idvw
c            must be at least (nlat+1)/2.
c
C*PL*ERROR* Comment line too long
c   jdvw     the second dimension of the arrays vlap and wlap as it appears
C*PL*ERROR* Comment line too long
c            in the program that calls vlapgs. jdvw must be at least nlon.
c
c
c   br,bi    two or three dimensional arrays (see input parameter nt)
c   cr,ci    that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c            br,bi,cr and ci must be computed by vhags prior to calling
c            vlapgs.  if ityp=1,4, or 7 then cr,ci are not used and can
c            be dummy arguments.  if ityp=2,5, or 8 then br,bi are not
c            used and can be dummy arguments.
c
c    mdbc    the first dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls vlapgs.  mdbc must be
c            at least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndbc    the second dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls vlapgs. ndbc must be at
c            least nlat.
c
c    wvhsgs  an array which must be initialized by subroutine vlapgsi
c            (or equivalently by vhsgsi).  once initialized, wvhsgs
c            can be used repeatedly by vlapgs as long as nlat and nlon
c            remain unchanged.  wvhsgs must not be altered between calls
c            of vlapgs.
c
c     lvhsgs the dimension of the array wvhsgs as it appears in the
c            program that calls vlapgs.  define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgs must be at least
c
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vlapgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
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
c    vlap,   two or three dimensional arrays (see input parameter nt) that
C*PL*ERROR* Comment line too long
c    wlap    contain the vector laplacian of the field (v,w).  wlap(i,j) is
C*PL*ERROR* Comment line too long
c            the east longitude component and vlap(i,j) is the colatitudinal
c            component of the vector laplacian.  the definition of the
c            vector laplacian follows:
c
C*PL*ERROR* Comment line too long
c            let cost and sint be the cosine and sine at colatitude theta.
C*PL*ERROR* Comment line too long
c            let d( )/dlambda  and d( )/dtheta be the first order partial
C*PL*ERROR* Comment line too long
c            derivatives in longitude and colatitude.  let del2 be the scalar
c            laplacian operator
c
c                 del2(s) = [d(sint*d(s)/dtheta)/dtheta +
c                             2            2
c                            d (s)/dlambda /sint]/sint
c
c            then the vector laplacian opeator
c
c                 dvel2(v,w) = (vlap,wlap)
c
c            is defined by
c
c                 vlap = del2(v) - (2*cost*dw/dlambda + v)/sint**2
c
c                 wlap = del2(w) + (2*cost*dv/dlambda - w)/sint**2
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
c            = 9  error in the specification of lvhsgs
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c
c     end of documentation for vlapgs
c
c **********************************************************************
c
      SUBROUTINE DVLAPGS(NLAT,NLON,ITYP,NT,VLAP,WLAP,IDVW,JDVW,BR,BI,CR,
     +                  CI,MDBC,NDBC,WVHSGS,LVHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION VLAP
      DOUBLE PRECISION WLAP
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHSGS
      DOUBLE PRECISION WORK
      DIMENSION VLAP(IDVW,JDVW,NT),WLAP(IDVW,JDVW,NT)
      DIMENSION BR(MDBC,NDBC,NT),BI(MDBC,NDBC,NT)
      DIMENSION CR(MDBC,NDBC,NT),CI(MDBC,NDBC,NT)
      DIMENSION WVHSGS(LVHSGS),WORK(LWORK)

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
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      LSAVMIN = LZIMN + LZIMN + NLON + 15
      IF (LVHSGS.LT.LSAVMIN) RETURN
c
c     verify unsaved work space length
c
      MN = MMAX*NLAT*NT
      L2 = (NLAT+1)/2
      L1 = MMAX
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
      LIWK = LWORK - 4*MN - NLAT
      CALL DVLAPGS1(NLAT,NLON,ITYP,NT,VLAP,WLAP,IDVW,JDVW,WORK(IBR),
     +             WORK(IBI),WORK(ICR),WORK(ICI),MMAX,WORK(IFN),MDBC,
     +             NDBC,BR,BI,CR,CI,WVHSGS,LVHSGS,WORK(IWK),LIWK,IERROR)
      RETURN
      END

      SUBROUTINE DVLAPGS1(NLAT,NLON,ITYP,NT,VLAP,WLAP,IDVW,JDVW,BRLAP,
     +                   BILAP,CRLAP,CILAP,MMAX,FNN,MDB,NDB,BR,BI,CR,
     +                   CI,WSAVE,LSAVE,WK,LWK,IERROR)
      DOUBLE PRECISION VLAP
      DOUBLE PRECISION WLAP
      DOUBLE PRECISION BRLAP
      DOUBLE PRECISION BILAP
      DOUBLE PRECISION CRLAP
      DOUBLE PRECISION CILAP
      DOUBLE PRECISION FNN
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION VLAP(IDVW,JDVW,NT),WLAP(IDVW,JDVW,NT)
      DIMENSION FNN(NLAT),BRLAP(MMAX,NLAT,NT),BILAP(MMAX,NLAT,NT)
      DIMENSION CRLAP(MMAX,NLAT,NT),CILAP(MMAX,NLAT,NT)
      DIMENSION BR(MDB,NDB,NT),BI(MDB,NDB,NT)
      DIMENSION CR(MDB,NDB,NT),CI(MDB,NDB,NT)
      DIMENSION WSAVE(LSAVE),WK(LWK)
c
c     preset coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          FNN(N) = -FN* (FN+1.D0)
    1 CONTINUE
c
c     set laplacian coefficients from br,bi,cr,ci
c
      IF (ITYP.EQ.0 .OR. ITYP.EQ.3 .OR. ITYP.EQ.6) THEN
c
c     all coefficients needed
c
          DO 2 K = 1,NT
              DO 3 N = 1,NLAT
                  DO 4 M = 1,MMAX
                      BRLAP(M,N,K) = 0.0D0
                      BILAP(M,N,K) = 0.0D0
                      CRLAP(M,N,K) = 0.0D0
                      CILAP(M,N,K) = 0.0D0
    4             CONTINUE
    3         CONTINUE
              DO 5 N = 2,NLAT
                  BRLAP(1,N,K) = FNN(N)*BR(1,N,K)
                  BILAP(1,N,K) = FNN(N)*BI(1,N,K)
                  CRLAP(1,N,K) = FNN(N)*CR(1,N,K)
                  CILAP(1,N,K) = FNN(N)*CI(1,N,K)
    5         CONTINUE
              DO 6 M = 2,MMAX
                  DO 7 N = M,NLAT
                      BRLAP(M,N,K) = FNN(N)*BR(M,N,K)
                      BILAP(M,N,K) = FNN(N)*BI(M,N,K)
                      CRLAP(M,N,K) = FNN(N)*CR(M,N,K)
                      CILAP(M,N,K) = FNN(N)*CI(M,N,K)
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
                      BRLAP(M,N,K) = 0.0D0
                      BILAP(M,N,K) = 0.0D0
   14             CONTINUE
   13         CONTINUE
              DO 15 N = 2,NLAT
                  BRLAP(1,N,K) = FNN(N)*BR(1,N,K)
                  BILAP(1,N,K) = FNN(N)*BI(1,N,K)
   15         CONTINUE
              DO 16 M = 2,MMAX
                  DO 17 N = M,NLAT
                      BRLAP(M,N,K) = FNN(N)*BR(M,N,K)
                      BILAP(M,N,K) = FNN(N)*BI(M,N,K)
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
                      CRLAP(M,N,K) = 0.0D0
                      CILAP(M,N,K) = 0.0D0
   24             CONTINUE
   23         CONTINUE
              DO 25 N = 2,NLAT
                  CRLAP(1,N,K) = FNN(N)*CR(1,N,K)
                  CILAP(1,N,K) = FNN(N)*CI(1,N,K)
   25         CONTINUE
              DO 26 M = 2,MMAX
                  DO 27 N = M,NLAT
                      CRLAP(M,N,K) = FNN(N)*CR(M,N,K)
                      CILAP(M,N,K) = FNN(N)*CI(M,N,K)
   27             CONTINUE
   26         CONTINUE
   22     CONTINUE
      END IF
c
c     sythesize coefs into vector field (vlap,wlap)
c
      CALL DVHSGS(NLAT,NLON,ITYP,NT,VLAP,WLAP,IDVW,JDVW,BRLAP,BILAP,
     +           CRLAP,CILAP,MMAX,NLAT,WSAVE,LSAVE,WK,LWK,IERROR)
      RETURN
      END
