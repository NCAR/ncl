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
c ... file trvsph.f
c
c     this file contains documentation and code for subroutine trvsph
c
c ... required files
c
c     sphcom.f, hrfft.f, gaqd.f, vhaec.f, vhsec.f, vhagc.f, vhsgc.f
c
c     subroutine trvsph (intl,igrida,nlona,nlata,iveca,ua,va,
c    +igridb,nlonb,nlatb,ivecb,ub,vb,wsave,lsave,lsvmin,work,
c    +lwork,lwkmin,dwork,ldwork,ier)
c
c *** author
c
c     John C. Adams (NCAR 1997), email: johnad@ncar.ucar.edu
c
c *** purpose
c
C*PL*ERROR* Comment line too long
c     subroutine trvsph transfers vector data given in (ua,va) on a grid on
C*PL*ERROR* Comment line too long
c     the full sphere to vector data in (ub,vb) on a grid on the full sphere.
C*PL*ERROR* Comment line too long
c     the grids on which (ua,va) is given and (ub,vb) is generated can be
C*PL*ERROR* Comment line too long
c     specified independently of each other (see the input arguments igrida,
C*PL*ERROR* Comment line too long
c     igridb,iveca,ivecb).  ua and ub are the east longitudinal components of
C*PL*ERROR* Comment line too long
c     the given and transformed vector fields.  va is either the latitudinal
c     or colatitudinal component of the given vector field (see iveca).
c     vb is either the latitudinal or colatitudinal component of the
C*PL*ERROR* Comment line too long
c     transformed vector field (see ivecb).  for transferring scalar data
c     on the sphere, use subroutine trssph.
c
C*PL*ERROR* Comment line too long
c *   notice that scalar and vector quantities are fundamentally different
C*PL*ERROR* Comment line too long
c     on the sphere.  for example, vectors are discontinuous and multiple
C*PL*ERROR* Comment line too long
c     valued at the poles.  scalars are continuous and single valued at the
C*PL*ERROR* Comment line too long
c     poles. erroneous results would be produced if one attempted to transfer
c     vector fields between grids with subroutine trssph applied to each
c     component of the vector.
c
c *** underlying grid assumptions and a description
c
C*PL*ERROR* Comment line too long
c     discussions with the ncar scd data support group and others indicate
C*PL*ERROR* Comment line too long
c     there is no standard grid for storing observational or model generated
c     data on the sphere.  subroutine trvsph was designed to handle most
C*PL*ERROR* Comment line too long
c     cases likely to be encountered when moving data from one grid format
c     to another.
c
C*PL*ERROR* Comment line too long
c     the grid on which (ua,va) is given must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
C*PL*ERROR* Comment line too long
c     longitude, which can be either the first or second dimension of ua,va
C*PL*ERROR* Comment line too long
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of ua,va, has south
c     to north or north to south orientation with increasing subscript
c     value in ua,va (see the argument igrida).
c
C*PL*ERROR* Comment line too long
c     the grid on which ub,vb is generated must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
C*PL*ERROR* Comment line too long
c     longitude, which can be either the first or second dimension of ub,vb
C*PL*ERROR* Comment line too long
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of ub,vb, has south
c     to north or north to south orientation with increasing subscript
c     value in db (see the argument igridb).
c
c     let nlon be either nlona or nlonb (the number of grid points in
c     longitude.  the longitude grid subdivides [0,2pi) into nlon spaced
c     points
c
c          (j-1)*2.*pi/nlon  (j=1,...,nlon).
c
C*PL*ERROR* Comment line too long
c     it is not necessary to communicate to subroutine trvsph whether the
C*PL*ERROR* Comment line too long
c     underlying grids are in latitude or colatitude.  it is only necessary
C*PL*ERROR* Comment line too long
c     to communicate whether they run south to north or north to south with
C*PL*ERROR* Comment line too long
c     increasing subscripts.  a brief discussion of latitude and colatitude
c     follows.  equally spaced latitude grids are assumed to subdivide
c     [-pi/2,pi/2] with the south pole at -pi/2 and north pole at pi/2.
C*PL*ERROR* Comment line too long
c     equally spaced colatitude grids subdivide [0,pi] with the north pole
C*PL*ERROR* Comment line too long
c     at 0 and south pole at pi.  equally spaced partitions on the sphere
C*PL*ERROR* Comment line too long
c     include both poles.  gaussian latitude grids subdivide (-pi/2,pi/2)
C*PL*ERROR* Comment line too long
c     and gaussian colatitude grids subdivide (0,pi).  gaussian grids do not
C*PL*ERROR* Comment line too long
c     include the poles.  the gaussian grid points are uniquely determined by
c     the size of the partition.  they can be computed in colatitude in
C*PL*ERROR* Comment line too long
c     (0,pi) (north to south) in double precision by the spherepack subroutine
C*PL*ERROR* Comment line too long
c     gaqd.  let nlat be nlata or nlatb if either the ua,va or ub,vb grid is
c     gaussian.  let
c
c        north pole                             south pole
c        ----------                             ----------
c           0.0    <  cth(1) < ... < cth(nlat)  <   pi
c
c
c     be nlat gaussian colatitude points in the interval (0,pi) and let
c
c        south pole                        north pole
c        ----------                        ----------
c           -pi/2  < th(1) < ... < th(nlat) < pi/2
c
C*PL*ERROR* Comment line too long
c     be nlat gaussian latitude points in the open interval (-pi/2,pi/2).
c     these are related by
c
c          th(i) = -pi/2 + cth(i)  (i=1,...,nlat)
c
C*PL*ERROR* Comment line too long
c     if the (ua,va) or (ub,vb) grid is equally spaced in (co)latitude then
c
c          ctht(i) = (i-1)*pi/(nlat-1)
c                                               (i=1,...,nlat)
c          tht(i) = -pi/2 + (i-1)*pi/(nlat-1)
c
C*PL*ERROR* Comment line too long
c     define the equally spaced (north to south) colatitude and (south to
c     north) latitude grids.
c
c *** method (simplified description)
c
c    (1)
c
C*PL*ERROR* Comment line too long
c     the vector field (ua,va) is reformated to a vector field in mathematical
C*PL*ERROR* Comment line too long
c     spherical coordinates using array transpositions, subscript reordering
c     and negation of va as necessary (see arguments igrida,iveca).
c
c     (2)
c
c     a vector harmonic analysis is performed on the result from (1)
c
c     (3)
c
c     a vector harmonic synthesis is performed on the (ub,vb) grid
c     using as many coefficients from (2) as possible (i.e., as
c     as is consistent with the size of the ub,vb grid).
c
c     (4)
c
c     the vector field generated in (3) is transformed from mathematical
c     spherical coordinates to the form flagged by ivecb and igridb in
C*PL*ERROR* Comment line too long
c     (ub,vb) using array transpositions, subscript reordering and negation
c     as necessary
c
c
c *** advantages
c
c     the use of vector spherical harmonics to transfer vector data is
c     highly accurate and preserves properties of vectors on the sphere.
c     the method produces a weighted least squares fit to vector data in
C*PL*ERROR* Comment line too long
c     which waves are resolved uniformly on the full sphere.  high frequencies
C*PL*ERROR* Comment line too long
c     induced by closeness of grid points near the poles (due to computational
C*PL*ERROR* Comment line too long
c     or observational errors) are smoothed.  the method is consistent with
C*PL*ERROR* Comment line too long
c     methods used to generate vector data in numerical spectral models based
C*PL*ERROR* Comment line too long
c     on spherical harmonics.  for more discussion of these and related issues,
c     see "on the spectral approximation of discrete scalar and vector
C*PL*ERROR* Comment line too long
c     functions on the sphere," siam j. numer. anal., vol. 16, december 1979,
c     pp. 934-949, by paul swarztrauber.
c
c
c *** comment
c
C*PL*ERROR* Comment line too long
c     on a nlon by nlat or nlat by nlon grid (gaussian or equally spaced)
c     spherical harmonic analysis generates and synthesis utilizes
c     min0(nlat,(nlon+2)/2)) by nlat coefficients.  consequently, for
c     ua,va and ub,vb,  if either
c
c             min0(nlatb,(nlonb+2)/2) < min0(nlata,(nlona+2)/2)
c
c     or if
c
c             nlatb < nlata
c
C*PL*ERROR* Comment line too long
c     then all the coefficients generated by an analysis of ua,va cannot be
C*PL*ERROR* Comment line too long
c     used in the synthesis which generates ub,vb.  in this case "information"
C*PL*ERROR* Comment line too long
c     can be lost in generating ub,vb.  more precisely, information will be
C*PL*ERROR* Comment line too long
c     lost if the analysis of ua,va yields nonzero coefficients which are
c     outside the coefficient bounds determined by the ub,vb grid. still
c     transference with vector spherical harmonics will yield results
c     consistent with grid resolution and is highly accurate.
c
c *** input arguments
c
c ... intl
c
C*PL*ERROR* Comment line too long
c     an initialization argument which should be zero on an initial call to
c     trvsph.  intl should be one if trvsph is being recalled and
c
c          igrida,nlona,nlata,iveca,igridb,nlonb,nlatb,ivecb
c
C*PL*ERROR* Comment line too long
c     have not changed from the previous call.  if any of these arguments have
C*PL*ERROR* Comment line too long
c     changed intl=0 must be used to avoid undetectable errors.  when allowed,
C*PL*ERROR* Comment line too long
c     calls with intl=1 bypass redundant computation and save time.  it can
c     be used when transferring multiple vector data sets with the same
c     underlying grids.
c
c ... igrida
c
C*PL*ERROR* Comment line too long
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the given vector data (ua,va) as follows:
c
c     igrida(1)
c
c     = -1
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ua,va is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs north to south with increasing subscript value
c
c     = +1
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ua,va is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs south to north with increasing subscript value
c
c     = -2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ua,va is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north
c     to south with increasing subscript value
c
c     = +2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ua,va is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs south
c     north with increasing subscript value
c
c     igrida(2)
c
c     = 0 if the underlying grid for ua,va is a nlona by nlata
c
c     = 1 if the underlying grid for ua,va is a nlata by nlona
c
c
c ... nlona
c
C*PL*ERROR* Comment line too long
c     the number of longitude points on the uniform grid which partitions
C*PL*ERROR* Comment line too long
c     [0,2pi) for the given vector (ua,va).  nlona is also the first or second
C*PL*ERROR* Comment line too long
c     dimension of ua,va (see igrida(2)) in the program which calls trvsph.
C*PL*ERROR* Comment line too long
c     nlona determines the grid increment in longitude as 2*pi/nlona. for
C*PL*ERROR* Comment line too long
c     example nlona = 72 for a five degree grid.  nlona must be greater than
c     or equal to 4.  the efficiency of the computation is improved when
c     nlona is a product of small prime numbers
c
c ... nlata
c
c     the number of points in the latitude (or colatitude) grid for the
c     given vector (ua,va).  nlata is also the first or second dimension
c     of ua and va (see igrida(2)) in the program which calls trvsph.
C*PL*ERROR* Comment line too long
c     if nlata is odd then the equator will be located at the (nlata+1)/2
c     gaussian grid point.  if nlata is even then the equator will be
c     located half way between the nlata/2 and nlata/2+1 grid points.
c
c ... iveca
c
c     if iveca=0 is input then va is the latitudinal component of the
c     given vector field. if iveca=1 then va is the colatitudinal
c     compoenent of the given vector field.  in either case, ua must
c     be the east longitudinal component of the given vector field.
c
c *** note:
c     igrida(1)=-1 or igrida(1)=-2, igrida(2)=1, and iveca=1 corresponds
c     to the "usual" mathematical spherical coordinate system required
C*PL*ERROR* Comment line too long
c     by most of the drivers in spherepack2.  igrida(1)=1 or igrida(1)=2,
c     igrida(2)=0, and iveca=0 corresponds to the "usual" geophysical
c     spherical coordinate system.
c
c
c ... ua
c
c     ua is the east longitudinal component of the given vector field.
C*PL*ERROR* Comment line too long
c     ua must be dimensioned nlona by nlata in the program calling trvsph if
C*PL*ERROR* Comment line too long
c     igrida(2) = 0.  ua must be dimensioned nlata by nlona in the program
C*PL*ERROR* Comment line too long
c     calling trvsph if igrida(2) = 1.  if ua is not properly dimensioned
C*PL*ERROR* Comment line too long
c     and if the latitude (colatitude) values do not run south to north or
C*PL*ERROR* Comment line too long
c     north to south as flagged by igrida(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c
c ... va
c
c     va is either the latitudinal or colatitudinal componenet of the
c     given vector field (see iveca).  va must be dimensioned nlona by
c     nlata in the program calling trvsph if igrida(2)=0.  va must be
c     dimensioned nlata by nlona in the program calling trvsph if
c     igrida(2)=1.  if va is not properly dimensioned or if the latitude
c     (colatitude) values do not run south to north or north to south
c     as flagged by igrida(1) (this cannot be checked!) then incorrect
c     results will be produced.
c
c ... igridb
c
C*PL*ERROR* Comment line too long
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the transformed vector (ub,vb) as follows:
c
c     igridb(1)
c
c     = -1
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ub,vb is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     north to south
c
c     = +1
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ub,vb is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     south to north
c
c     = -2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ub,vb is a gaussian partition
C*PL*ERROR* Comment line too long
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north to
c     south
c
c     = +2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for ub,vb is a gaussian partition
C*PL*ERROR* Comment line too long
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs south to
c     north
c
c     igridb(2)
c
c     = 0 if the underlying grid for ub,vb is a nlonb by nlatb
c
c     = 1 if the underlying grid for ub,vb is a nlatb by nlonb
c
c
c ... nlonb
c
C*PL*ERROR* Comment line too long
c     the number of longitude points on the uniform grid which partitions
C*PL*ERROR* Comment line too long
c     [0,2pi) for the transformed vector (ub,vb).  nlonb is also the first or
C*PL*ERROR* Comment line too long
c     second dimension of ub and vb (see igridb(2)) in the program which calls
C*PL*ERROR* Comment line too long
c     trvsph.  nlonb determines the grid increment in longitude as 2*pi/nlonb.
C*PL*ERROR* Comment line too long
c     for example nlonb = 72 for a five degree grid.  nlonb must be greater
C*PL*ERROR* Comment line too long
c     than or equal to 4.  the efficiency of the computation is improved when
c     nlonb is a product of small prime numbers
c
c ... nlatb
c
c     the number of points in the latitude (or colatitude) grid for the
C*PL*ERROR* Comment line too long
c     transformed vector (ub,vb).  nlatb is also the first or second dimension
c     of ub and vb (see igridb(2)) in the program which calls trvsph.
C*PL*ERROR* Comment line too long
c     if nlatb is odd then the equator will be located at the (nlatb+1)/2
c     gaussian grid point.  if nlatb is even then the equator will be
c     located half way between the nlatb/2 and nlatb/2+1 grid points.
c
c ... ivecb
c
c     if ivecb=0 is input then vb is the latitudinal component of the
c     given vector field. if ivecb=1 then vb is the colatitudinal
c     compoenent of the given vector field.  in either case, ub must
c     be the east longitudinal component of the given vector field.
c
c *** note:
c     igridb(1)=-1 or igridb(1)=-2, igridb(2)=1, and ivecb=1 corresponds
c     to the "usual" mathematical spherical coordinate system required
C*PL*ERROR* Comment line too long
c     by most of the drivers in spherepack2.  igridb(1)=1 or igridb(1)=2,
c     igridb(2)=0, and ivecb=0 corresponds to the "usual" geophysical
c     spherical coordinate system.
c
c ... wsave
c
c     a saved work space array that can be utilized repeatedly by trvsph
c     as long as the arguments nlata,nlona,nlatb,nlonb remain unchanged.
C*PL*ERROR* Comment line too long
c     wsave is set by a intl=0 call to trvsph.  wsave must not be altered
c     when trvsph is being recalled with intl=1.
c
c ... lsave
c
c     the dimension of the work space wsave as it appears in the program
c     that calls trvsph.  the minimum required value of lsave for the
C*PL*ERROR* Comment line too long
c     current set of input arguments is set in the output argument lsvmin.
C*PL*ERROR* Comment line too long
c     it can be determined by calling trvsph with lsave=0 and printing lsvmin.
c
c          la1 = min0(nlata,(nlona+1)/2), la2 = (nlata+1)/2
c
c          lb1 = min0(nlatb,(nlonb+1)/2), lb2 = (nlatb+1)/2
c
C*PL*ERROR* Comment line too long
c          lwa = 4*nlata*la2+3*max0(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15
c
c          lwb = 4*nlatb*lb2+3*max0(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15
c
c      then
c
c          lsvmin = lwa + lwb
c
c      is the minimal required work space length of wsave
c
c
c ... work
c
c     a work array that does not have to be preserved
c
c ... lwork
c
c     the dimension of the array work as it appears in the program that
c     calls trvsph. the minimum required value of lwork for the current
c     set of input arguments is set in the output argument lwkmin.
c     it can be determined by calling trvsph with lwork=0 and printing
C*PL*ERROR* Comment line too long
c     lwkmin.  an estimate for lwork follows.  let nlat = max0(nlata,nlatb),
C*PL*ERROR* Comment line too long
c     nlon = max0(nlona,nlonb) and l1 = min0(nlat,(nlon+2)/2).  with these
c     these definitions, the quantity
c
c            2*nlat*(8*l1 + 4*nlon + 3)
c
c     will suffice as a length for the unsaved work space.  this formula
c     may overestimate the required minimum value for lwork.  the exact
c     minimum value can be predetermined by calling trvsph wtih lwork=0
c     and printout of lwkmin.
c
c ... dwork
c
c     a double precision work array that does not have to be preserved.
c
c ... ldwork
c
c     the length of dwork in the routine calling trvsph
c     Let
c
c       nlat = max0(nlata,nlatb)
c
c     ldwork must be at least 2*nlat*(nlat+1)+1
c
c
c *** output arguments
c
c
c ... ub
c
C*PL*ERROR* Comment line too long
c     a two dimensional array that contains the east longitudinal component
c     of the transformed vector data.  ub
C*PL*ERROR* Comment line too long
c     must be dimensioned nlonb by nlatb in the program calling trvsph if
c     igridb(2)=0.  ub must be dimensioned nlatb by nlonb in the program
c     calling trvsph if igridb(2)=1.  if ub is not properly dimensioned
C*PL*ERROR* Comment line too long
c     and if the latitude (colatitude) values do not run south to north or
C*PL*ERROR* Comment line too long
c     north to south as flagged by igrdb(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c
c ... vb
c
C*PL*ERROR* Comment line too long
c     a two dimensional array that contains the latitudinal or colatitudinal
c     component of the transformed vector data (see ivecb).
C*PL*ERROR* Comment line too long
c     vb must be dimensioned nlonb by nlatb in the program calling trvsph if
c     igridb(2)=0.  vb must be dimensioned nlatb by nlonb in the program
c     calling trvsph if igridb(2)=1.  if vb is not properly dimensioned
C*PL*ERROR* Comment line too long
c     and if the latitude (colatitude) values do not run south to north or
C*PL*ERROR* Comment line too long
c     north to south as flagged by igrdb(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c ... lsvmin
c
c     the minimum length of the saved work space in wsave.
c     lsvmin is computed even if lsave < lsvmin (ier = 10).
c
c ... lwkmin
c
c     the minimum length of the unsaved work space in work.
c     lwkmin is computed even if lwork < lwkmin (ier = 11).
c
c
c *** error argument
c
c ... ier = 0  if no errors are detected
c
c         = 1  if intl is not 0 or 1
c
c         = 2  if igrida(1) is not -1 or +1 or -2 or +2
c
c         = 3  if igrida(2) is not 0 or 1
c
c         = 4  if nlona is less than 4
c
c         = 5  if nlata is less than 3
c
c         = 6  if iveca is not 0 or 1
c
c         = 7  if igridb(1) is not -1 or +1 or -2 or +2
c
c         = 8  if igridb(2) is not 0 or 1
c
c         = 9  if nlonb is less than 4
c
c         =10  if nlatb is less than 3
c
c         =11  if ivecb is not 0 or 1
c
C*PL*ERROR* Comment line too long
c         =12  if there is insufficient saved work space (lsave < lsvmin)
c
C*PL*ERROR* Comment line too long
c         =13  if there is insufficient unsaved work space (lwork < lwkmin)
c
c         =14  indicates failure in an eigenvalue routine which computes
c              gaussian weights and points
c
c         =15  if ldwork is too small (insufficient double precision
c              unsaved work space)
c
c *****************************************************
c *****************************************************
c
c     end of argument description ... code follows
c
c *****************************************************
c *****************************************************
c
      SUBROUTINE DTRVSPH(INTL,IGRIDA,NLONA,NLATA,IVECA,UA,VA,IGRIDB,
     +                  NLONB,NLATB,IVECB,UB,VB,WSAVE,LSAVE,LSVMIN,WORK,
     +                  LWORK,LWKMIN,DWORK,LDWORK,IER)
      IMPLICIT NONE
      INTEGER INTL,IGRIDA(2),NLONA,NLATA,IGRIDB(2),NLONB,NLATB
      INTEGER IVECA,IVECB,LSAVE,LSVMIN,LWORK,LWKMIN,LDWORK,IER
      DOUBLE PRECISION UA(*),VA(*),UB(*),VB(*),WSAVE(*),WORK(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)
      INTEGER IG,IGRDA,IGRDB,LA1,LA2,LB1,LB2,LWA,LWB
      INTEGER IABR,IABI,IACR,IACI,IBBR,IBBI,IBCR,IBCI
      INTEGER NLAT,LWK1,LWK2,LW,IW,JB,NT,ITYP
c
C*PL*ERROR* Comment line too long
c     include a save statement to ensure local variables in trvsph, set during
c     an intl=0 call, are preserved if trvsph is recalled with intl=1
c
      SAVE
c
c     check input arguments
c
      IER = 1
      IF (INTL* (INTL-1).NE.0) RETURN
      IER = 2
      IG = IGRIDA(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 3
      IG = IGRIDA(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 4
      IF (NLONA.LT.4) RETURN
      IER = 5
      IF (NLATA.LT.3) RETURN
      IER = 6
      IF (IVECA* (IVECA-1).NE.0) RETURN
      IER = 7
      IG = IGRIDB(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 8
      IG = IGRIDB(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 9
      IF (NLONB.LT.4) RETURN
      IER = 10
      IF (NLATB.LT.3) RETURN
      IER = 11
      IF (IVECB* (IVECB-1).NE.0) RETURN
      IER = 0
      IGRDA = IABS(IGRIDA(1))
      IGRDB = IABS(IGRIDB(1))
      IF (INTL.EQ.0) THEN
          LA1 = MIN0(NLATA, (NLONA+1)/2)
          LA2 = (NLATA+1)/2
          LB1 = MIN0(NLATB, (NLONB+1)/2)
          LB2 = (NLATB+1)/2
c
c     saved space for analysis on a grid
c
          LWA = 4*NLATA*LA2 + 3*MAX0(LA1-2,0)* (2*NLATA-LA1-1) + LA2 +
     +          NLONA + 15
c
c     set saved work space length for synthesis on b grid
c
          LWB = 4*NLATB*LB2 + 3*MAX0(LB1-2,0)* (2*NLATB-LB1-1) + NLONB +
     +          15
c
c     set minimum required saved work space length
c
          LSVMIN = LWA + LWB
c
c     set wsave pointer
c
          JB = 1 + LWA
c
c     set pointers for vector spherical harmonic coefs in work
c
          IABR = 1
          IABI = IABR + LA1*NLATA
          IACR = IABI + LA1*NLATA
          IACI = IACR + LA1*NLATA
          IBBR = IACI + LA1*NLATA
          IBBI = IBBR + LB1*NLATB
          IBCR = IBBI + LB1*NLATB
          IBCI = IBCR + LB1*NLATB
c
c     set pointers for remaining work
c
          IW = IBCI + LB1*NLATB
c
c     set remaining work space length in lw
c
          LW = LWORK - IW
c
c     compute unsaved space for analysis and synthesis
c
          LWK1 = 2*NLATA* (2*NLONA+MAX0(6*LA2,NLONA))
          LWK2 = 2*NLATB* (2*NLONB+MAX0(6*LB2,NLONB))
c
c     set minimum unsaved work space required by trvsph
c
          LWKMIN = IW + MAX0(LWK1,LWK2)
c
c     set error flags if saved or unsaved work space is insufficient
c
          IER = 12
          IF (LSAVE.LT.LSVMIN) RETURN
          IER = 13
          IF (LWORK.LT.LWKMIN) RETURN
          IER = 15
          NLAT = MAX0(NLATA,NLATB)
          IF (LDWORK.LT.2*NLAT* (NLAT+1)+1) RETURN
          IER = 0
          IF (IGRDA.EQ.1) THEN
c
c     initialize wsave for equally spaced analysis
c
              CALL DVHAECI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
          ELSE
c
c     initialize wsave for gaussian analysis
c
              CALL DVHAGCI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 14
                  RETURN
              END IF
          END IF

          IF (IGRDB.EQ.2) THEN
c
c     initialize wsave for gaussian synthesis
c
              CALL DVHSGCI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 14
                  RETURN
              END IF
          ELSE
c
c     initialize wsave for equally spaced synthesis
c
              CALL DVHSECI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
          END IF
c
c     end of initialization (intl=0) call
c
      END IF
c
C*PL*ERROR* Comment line too long
c     convert the vector field (ua,va) to mathematical spherical coordinates
c
      IF (IGRIDA(2).EQ.0) THEN
          CALL DTRVPLAT(NLONA,NLATA,UA,WORK)
          CALL DTRVPLAT(NLONA,NLATA,VA,WORK)
      END IF
      IF (IGRIDA(1).GT.0) THEN
          CALL DCOVLAT(NLATA,NLONA,UA)
          CALL DCOVLAT(NLATA,NLONA,VA)
      END IF
      IF (IVECA.EQ.0) THEN
          CALL DNEGV(NLATA,NLONA,VA)
      END IF
      NT = 1
      ITYP = 0
c
c     analyze vector field
c
      IF (IGRDA.EQ.2) THEN
          CALL DVHAGC(NLATA,NLONA,ITYP,NT,VA,UA,NLATA,NLONA,WORK(IABR),
     +               WORK(IABI),WORK(IACR),WORK(IACI),LA1,NLATA,WSAVE,
     +               LWA,WORK(IW),LW,IER)
      ELSE
          CALL DVHAEC(NLATA,NLONA,ITYP,NT,VA,UA,NLATA,NLONA,WORK(IABR),
     +               WORK(IABI),WORK(IACR),WORK(IACI),LA1,NLATA,WSAVE,
     +               LWA,WORK(IW),LW,IER)
      END IF
c
c     transfer a grid coefficients to b grid coefficients
c
      CALL DTRVAB(LA1,NLATA,WORK(IABR),WORK(IABI),WORK(IACR),
     +            WORK(IACI),LB1,NLATB,WORK(IBBR),WORK(IBBI),
     +            WORK(IBCR),WORK(IBCI))
c
c     synthesize on b grid
c
      IF (IGRDB.EQ.1) THEN
          CALL DVHSEC(NLATB,NLONB,ITYP,NT,VB,UB,NLATB,NLONB,WORK(IBBR),
     +               WORK(IBBI),WORK(IBCR),WORK(IBCI),LB1,NLATB,
     +               WSAVE(JB),LWB,WORK(IW),LW,IER)
      ELSE
          CALL DVHSGC(NLATB,NLONB,ITYP,NT,VB,UB,NLATB,NLONB,WORK(IBBR),
     +               WORK(IBBI),WORK(IBCR),WORK(IBCI),LB1,NLATB,
     +               WSAVE(JB),LWB,WORK(IW),LW,IER)
      END IF
c
C*PL*ERROR* Comment line too long
c     restore a grid and b grid vector fields (now in math coordinates) to
c     agree with grid flags in igrida,iveca,igridb,ivecb
c
      IF (IVECA.EQ.0) THEN
          CALL DNEGV(NLATA,NLONA,VA)
      END IF
      IF (IVECB.EQ.0) THEN
          CALL DNEGV(NLATB,NLONB,VB)
      END IF
      IF (IGRIDA(1).GT.0) THEN
          CALL DCOVLAT(NLATA,NLONA,UA)
          CALL DCOVLAT(NLATA,NLONA,VA)
      END IF
      IF (IGRIDB(1).GT.0) THEN
          CALL DCOVLAT(NLATB,NLONB,UB)
          CALL DCOVLAT(NLATB,NLONB,VB)
      END IF
      IF (IGRIDA(2).EQ.0) THEN
          CALL DTRVPLAT(NLATA,NLONA,UA,WORK)
          CALL DTRVPLAT(NLATA,NLONA,VA,WORK)
      END IF
      IF (IGRIDB(2).EQ.0) THEN
          CALL DTRVPLAT(NLATB,NLONB,UB,WORK)
          CALL DTRVPLAT(NLATB,NLONB,VB,WORK)
      END IF
      RETURN
      END
      SUBROUTINE DNEGV(NLAT,NLON,V)
c
c     negate (co)latitudinal vector componenet
c
      IMPLICIT NONE
      INTEGER NLAT,NLON,I,J
      DOUBLE PRECISION V(NLAT,NLON)

      DO J = 1,NLON
          DO I = 1,NLAT
              V(I,J) = -V(I,J)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DTRVAB(MA,NA,ABR,ABI,ACR,ACI,MB,NB,BBR,BBI,BCR,BCI)
      IMPLICIT NONE
      INTEGER MA,NA,MB,NB,I,J,M,N
      DOUBLE PRECISION ABR(MA,NA),ABI(MA,NA),ACR(MA,NA),ACI(MA,NA)
      DOUBLE PRECISION BBR(MB,NB),BBI(MB,NB),BCR(MB,NB),BCI(MB,NB)
c
c     set coefficients for b grid from coefficients for a grid
c
      M = MIN0(MA,MB)
      N = MIN0(NA,NB)
      DO J = 1,N
          DO I = 1,M
              BBR(I,J) = ABR(I,J)
              BBI(I,J) = ABI(I,J)
              BCR(I,J) = ACR(I,J)
              BCI(I,J) = ACI(I,J)
          END DO
      END DO
c
c     set coefs outside triangle to zero
c
      DO I = M + 1,MB
          DO J = 1,NB
              BBR(I,J) = 0.0D0
              BBI(I,J) = 0.0D0
              BCR(I,J) = 0.0D0
              BCI(I,J) = 0.0D0
          END DO
      END DO
      DO J = N + 1,NB
          DO I = 1,MB
              BBR(I,J) = 0.0D0
              BBI(I,J) = 0.0D0
              BCR(I,J) = 0.0D0
              BCI(I,J) = 0.0D0
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DTRVPLAT(N,M,DATA,WORK)
c
c     transpose the n by m array data to a m by n array data
c     work must be at least n*m words long
c
      IMPLICIT NONE
      INTEGER N,M,I,J,IJ,JI
      DOUBLE PRECISION DATA(*),WORK(*)

      DO J = 1,M
          DO I = 1,N
              IJ = (J-1)*N + I
              WORK(IJ) = DATA(IJ)
          END DO
      END DO
      DO I = 1,N
          DO J = 1,M
              JI = (I-1)*M + J
              IJ = (J-1)*N + I
              DATA(JI) = WORK(IJ)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DCOVLAT(NLAT,NLON,DATA)
c
c     reverse order of latitude (colatitude) grids
c
      IMPLICIT NONE
      INTEGER NLAT,NLON,NLAT2,I,IB,J
      DOUBLE PRECISION DATA(NLAT,NLON),TEMP

      NLAT2 = NLAT/2
      DO I = 1,NLAT2
          IB = NLAT - I + 1
          DO J = 1,NLON
              TEMP = DATA(I,J)
              DATA(I,J) = DATA(IB,J)
              DATA(IB,J) = TEMP
          END DO
      END DO
      RETURN
      END
