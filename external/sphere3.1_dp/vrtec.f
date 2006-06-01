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
c
c ... file vrtec.f
c
c     this file includes documentation and code for
c     subroutine divec          i
c
c ... files which must be loaded with vrtec.f
c
c     sphcom.f, hrfft.f, vhaec.f,shsec.f
c
c     subroutine vrtec(nlat,nlon,isym,nt,vt,ivrt,jvrt,cr,ci,mdc,ndc,
c    +                 wshsec,lshsec,work,lwork,ierror)
c
C*PL*ERROR* Comment line too long
c     given the vector spherical harmonic coefficients cr and ci, precomputed
c     by subroutine vhaec for a vector field (v,w), subroutine vrtec
c     computes the vorticity of the vector field in the scalar array
c     vt.  vt(i,j) is the vorticity at the colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     on the sphere.  i.e.,
c
c            vt(i,j) =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c     where sint = sin(theta(i)).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     cr,ci were precomputed.  required associated legendre polynomials
c     are recomputed rather than stored as they are in subroutine vrtes.
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
c            sphere.  i.e., in the array vt(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the vorticity is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vt(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vt(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the vorticity is antisymmetric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vt(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vt(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c      nt    nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            in the program that calls vrtec, the arrays cr,ci, and vort
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
c     ivrt   the first dimension of the array vt as it appears in
c            the program that calls vrtec. if isym = 0 then ivrt
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then ivrt must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then ivrt must be at least (nlat+1)/2.
c
c     jvrt   the second dimension of the array vt as it appears in
c            the program that calls vrtec. jvrt must be at least nlon.
c
c    cr,ci   two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhaec.
c     ***    cr and ci must be computed by vhaec prior to calling
c            vrtec.
c
c      mdc   the first dimension of the arrays cr and ci as it
c            appears in the program that calls vrtec. mdc must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c      ndc   the second dimension of the arrays cr and ci as it
c            appears in the program that calls vrtec. ndc must be at
c            least nlat.
c
c   wshsec   an array which must be initialized by subroutine shseci.
c            once initialized,
c            wshsec can be used repeatedly by vrtec as long as nlon
c            and nlat remain unchanged.  wshsec must not be altered
c            between calls of vrtec
c
c   lshsec   the dimension of the array wshsec as it appears in the
c            program that calls vrtec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c    lwork   the dimension of the array work as it appears in the
c            program that calls vrtec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            if isym is zero then lwork must be at least
c
c               nlat*(nt*nlon+max0(3*l2,nlon)+2*nt*l1+1)
c
c            if isym is not zero then lwork must be at least
c
c               l2*(nt*nlon+max0(3*nlat,nlon)) + nlat*(2*nt*l1+1)
c
c
c
c     **************************************************************
c
c     output parameters
c
c
c     vt     a two or three dimensional array (see input parameter nt)
c            that contains the vorticity of the vector field (v,w)
C*PL*ERROR* Comment line too long
c            whose coefficients cr,ci where computed by subroutine vhaec.
c            vt(i,j) is the vorticity at the colatitude point theta(i) =
c            (i-1)*pi/(nlat-1) and longitude point lambda(j) =
c            (j-1)*2*pi/nlon. the index ranges are defined above at the
c            input parameter isym.
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
c          = 9  error in the specification of lshsec
c          = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DVRTEC(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +                 WSHSEC,LSHSEC,WORK,LWORK,IERROR)
      DOUBLE PRECISION VORT
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WSHSEC
      DOUBLE PRECISION WORK

      DIMENSION VORT(IVRT,JVRT,NT),CR(MDC,NDC,NT),CI(MDC,NDC,NT)
      DIMENSION WSHSEC(LSHSEC),WORK(LWORK)
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
c
c     verify saved work space (same as shec)
c
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LSHSEC.LT.LZZ1+LABC+NLON+15) RETURN
      IERROR = 10
c
c     verify unsaved work space (add to what shec requires)
c
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
c
c     set first dimension for a,b (as requried by shsec)
c
      MAB = MIN0(NLAT,NLON/2+1)
      MN = MAB*NLAT*NT
c     if(lwork.lt.nln+max0(ls*nlon,3*nlat*imid)+2*mn+nlat) return
      L1 = MIN0(NLAT, (NLON+2)/2)
      L2 = (NLAT+1)/2
      IF (ISYM.EQ.0) THEN
          LWKMIN = NLAT* (NT*NLON+MAX0(3*L2,NLON)+2*NT*L1+1)
      ELSE
          LWKMIN = L2* (NT*NLON+MAX0(3*NLAT,NLON)) + NLAT* (2*NT*L1+1)
      END IF
      IF (LWORK.LT.LWKMIN) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IS = IB + MN
      IWK = IS + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DVRTEC1(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +            WORK(IA),WORK(IB),MAB,WORK(IS),WSHSEC,LSHSEC,
     +            WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DVRTEC1(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,CR,CI,MDC,NDC,
     +                  A,B,MAB,SQNN,WSHSEC,LSHSEC,WK,LWK,IERROR)
      DOUBLE PRECISION VORT
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION WSHSEC
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION VORT(IVRT,JVRT,NT),CR(MDC,NDC,NT),CI(MDC,NDC,NT)
      DIMENSION A(MAB,NLAT,NT),B(MAB,NLAT,NT),SQNN(NLAT)
      DIMENSION WSHSEC(LSHSEC),WK(LWK)
c
c     set coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          SQNN(N) = SQRT(FN* (FN+1.D0))
    1 CONTINUE
c
c     compute vorticity scalar coefficients for each vector field
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
c     compute m>0 coefficients, use mmax from vector coef range
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
      CALL DSHSEC(NLAT,NLON,ISYM,NT,VORT,IVRT,JVRT,A,B,MAB,NLAT,WSHSEC,
     +           LSHSEC,WK,LWK,IERROR)
      RETURN
      END
