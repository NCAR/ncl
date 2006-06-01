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
c ... file vrtgs.f
c
c     this file includes documentation and code for
c     subroutine divgs          i
c
c ... files which must be loaded with vrtgs.f
c
c     sphcom.f, hrfft.f, vhgsc.f, shsgs.f, gaqd.f
c
c     subroutine vrtgs(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
c    +                 wshsgs,lshsgs,work,lwork,ierror)
c
C*PL*ERROR* Comment line too long
c     given the vector spherical harmonic coefficients cr and ci, precomputed
c     by subroutine vhags for a vector field (v,w), subroutine vrtgs
c     computes the vorticity of the vector field in the scalar array
c     vort.  vort(i,j) is the vorticity at the gaussian colatitude
c     theta(i) (see nlat as input parameter) and longitude
c     lambda(j) = (j-1)*2*pi/nlon on the sphere.  i.e.,
c
c            vort(i,j) =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c     where sint = sin(theta(i)).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     cr,ci were precomputed.  required associated legendre polynomials
c     are stored rather than recomputed as they are in subroutine vrtgc.
c
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than 3. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   a parameter which determines whether the vorticity is
c            computed on the full or half sphere as follows:
c
c      = 0
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case the
c            vorticity is neither symmetric nor antisymmetric about
c            the equator.  the vorticity is computed on the entire
c            sphere.  i.e., in the array vort(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the vorticity is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vort(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vort(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the vorticity is antisymmetric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vort(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vort(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c      nt    nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            in the program that calls vrtgs, the arrays cr,ci, and vort
C*PL*ERROR* Comment line too long
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute the vorticity for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
C*PL*ERROR* Comment line too long
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     ivrt   the first dimension of the array vort as it appears in
c            the program that calls vrtgs. if isym = 0 then ivrt
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then ivrt must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then ivrt must be at least (nlat+1)/2.
c
c     jvrt   the second dimension of the array vort as it appears in
c            the program that calls vrtgs. jvrt must be at least nlon.
c
c    cr,ci   two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c     ***    cr and ci must be computed by vhags prior to calling
c            vrtgs.
c
c      mdc   the first dimension of the arrays cr and ci as it
c            appears in the program that calls vrtgs. mdc must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c      ndc   the second dimension of the arrays cr and ci as it
c            appears in the program that calls vrtgs. ndc must be at
c            least nlat.
c
c   wshsgs   an array which must be initialized by subroutine shsgsi.
c            once initialized,
c            wshsgs can be used repeatedly by vrtgs as long as nlon
c            and nlat remain unchanged.  wshsgs must not be altered
c            between calls of vrtgs
c
c   lshsgs   the dimension of the array wshsgs   as it appears in the
c            program that calls vrtgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c    lwork   the dimension of the array work as it appears in the
c            program that calls vrtgs. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            if isym = 0 then lwork must be at least
c
c               nlat*((nt+1)*nlon+2*nt*l1+1)
c
c            if isym > 0 then lwork must be at least
c
c               (nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
c     vort   a two or three dimensional array (see input parameter nt)
c            that contains the vorticity of the vector field (v,w)
C*PL*ERROR* Comment line too long
c            whose coefficients cr,ci where computed by subroutine vhags.
c            vort(i,j) is the vorticity at the gaussian colatitude point
c            theta(i) and longitude point lambda(j) = (j-1)*2*pi/nlon.
c            the index ranges are defined above at the input parameter
c            isym.
c
c
c   ierror   an error parameter which indicates fatal errors with input
c            parameters when returned positive.
c          = 0  no errors
c          = 1  error in the specification of nlat
c          = 2  error in the specification of nlon
c          = 3  error in the specification of isym
c          = 4  error in the specification of nt
c          = 5  error in the specification of ivrt
c          = 6  error in the specification of jvrt
c          = 7  error in the specification of mdc
c          = 8  error in the specification of ndc
c          = 9  error in the specification of lshsgs
c          = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DVRTGS(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +                 WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION VORT
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK

      DIMENSION VORT(IVRT,JVRT,NT),CR(MDC,NDC,NT),CI(MDC,NDC,NT)
      DIMENSION WSHSGS(LSHSGS),WORK(LWORK)
c
c     check input parameters
c
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (ISYM.LT.0 .OR. ISYM.GT.2) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IMID = (NLAT+1)/2
      IF ((ISYM.EQ.0.AND.IVRT.LT.NLAT) .OR.
     +    (ISYM.GT.0.AND.IVRT.LT.IMID)) RETURN
      IERROR = 6
      IF (JVRT.LT.NLON) RETURN
      IERROR = 7
      IF (MDC.LT.MIN0(NLAT, (NLON+1)/2)) RETURN
      MMAX = MIN0(NLAT, (NLON+2)/2)
      IERROR = 8
      IF (NDC.LT.NLAT) RETURN
      IERROR = 9
      IMID = (NLAT+1)/2
      LPIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
      L2 = (NLAT+MOD(NLAT,2))/2
      L1 = MIN0((NLON+2)/2,NLAT)
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
      IERROR = 10
c
c     verify unsaved work space (add to what shses requires, file f3)
c
c
c     set first dimension for a,b (as requried by shses)
c
      MAB = MIN0(NLAT,NLON/2+1)
      MN = MAB*NLAT*NT
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
      IF (LWORK.LT.NLN+LS*NLON+2*MN+NLAT) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IS = IB + MN
      IWK = IS + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DVRTGS1(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +            WORK(IA),WORK(IB),MAB,WORK(IS),WSHSGS,LSHSGS,
     +            WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DVRTGS1(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +                  A,B,MAB,SQNN,WSAV,LWSAV,WK,LWK,IERROR)
      DOUBLE PRECISION VORT
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION WSAV
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION VORT(IVRT,JVRT,NT),CR(MDC,NDC,NT),CI(MDC,NDC,NT)
      DIMENSION A(MAB,NLAT,NT),B(MAB,NLAT,NT),SQNN(NLAT)
      DIMENSION WSAV(LWSAV),WK(LWK)
c
c     set coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          SQNN(N) = SQRT(FN* (FN+1.D0))
    1 CONTINUE
c
c     compute divergence scalar coefficients for each vector field
c
      DO 2 K = 1,NT
          DO 3 N = 1,NLAT
              DO 4 M = 1,MAB
                  A(M,N,K) = 0.0D0
                  B(M,N,K) = 0.0D0
    4         CONTINUE
    3     CONTINUE
c
c     compute m=0 coefficients
c
          DO 5 N = 2,NLAT
              A(1,N,K) = SQNN(N)*CR(1,N,K)
              B(1,N,K) = SQNN(N)*CI(1,N,K)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          MMAX = MIN0(NLAT, (NLON+1)/2)
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  A(M,N,K) = SQNN(N)*CR(M,N,K)
                  B(M,N,K) = SQNN(N)*CI(M,N,K)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     synthesize a,b into vort
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,A,B,MAB,NLAT,WSAV,
     +           LWSAV,WK,LWK,IERROR)
      RETURN
      END
