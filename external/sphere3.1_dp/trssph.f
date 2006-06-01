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
c ... file trssph.f
c
c     contains documentation and code for subroutine trssph
c
c ... required files
c
c     sphcom.f, hrfft.f, gaqd.f, shaec.f, shsec.f, shagc.f, shsgc.f
c
c
c     subroutine trssph(intl,igrida,nlona,nlata,da,igridb,nlonb,nlatb,
c    +db,wsave,lsave,lsvmin,work,lwork,lwkmin,dwork,ldwork,ier)
c
c *** purpose
c
C*PL*ERROR* Comment line too long
c     subroutine trssph transfers data given in array da on a grid on the
c     full sphere to data in array db on a grid on the full sphere.  the
c     grids on which da is given and db is generated can be specified
C*PL*ERROR* Comment line too long
c     independently of each other (see description below and the arguments
c     igrida,igridb).  for transferring vector data on the sphere, use
c     subroutine trvsph.

C*PL*ERROR* Comment line too long
c     notice that scalar and vector quantities are fundamentally different
C*PL*ERROR* Comment line too long
c     on the sphere.  for example, vectors are discontinuous and multiple
C*PL*ERROR* Comment line too long
c     valued at the poles.  scalars are continuous and single valued at the
C*PL*ERROR* Comment line too long
c     poles. erroneous results would be produced if one attempted to transfer
c     vector fields between grids with subroutine trssph applied to each
c     component of the vector.
c
c
c *** underlying grid assumptions and a description
c
C*PL*ERROR* Comment line too long
c     discussions with the ncar scd data support group and others indicate
C*PL*ERROR* Comment line too long
c     there is no standard grid for storing observational or model generated
c     data on the sphere.  subroutine trssph was designed to handle most
C*PL*ERROR* Comment line too long
c     cases likely to be encountered when moving data from one grid format
c     to another.
c
c     the grid on which da is given must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
C*PL*ERROR* Comment line too long
c     longitude, which can be either the first or second dimension of da,
C*PL*ERROR* Comment line too long
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of da, has south
c     to north or north to south orientation with increasing subscript
c     value in da (see the argument igrida).
c
C*PL*ERROR* Comment line too long
c     the grid on which db is generated must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
C*PL*ERROR* Comment line too long
c     longitude, which can be either the first or second dimension of db,
C*PL*ERROR* Comment line too long
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of db, has south
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
c     it is not necessary to communicate to subroutine trssph whether the
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
c     gaqd.  let nlat be nlata or nlatb if either the da or db grid is
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
c     if the da or db grid is equally spaced in (co)latitude then
c
c          ctht(i) = (i-1)*pi/(nlat-1)
c                                               (i=1,...,nlat)
c          tht(i) = -pi/2 + (i-1)*pi/(nlat-1)
c
C*PL*ERROR* Comment line too long
c     define the equally spaced (north to south) colatitude and (south to
c     north) latitude grids.
c
c
c *** method (simplified description)
c
C*PL*ERROR* Comment line too long
c     for simplicity, assume da is a nlat by nlon data tabulation and da(i,j)
c     is the value at latitude theta(i) and longitude phi(j).  then
C*PL*ERROR* Comment line too long
c     coefficients a(m,n) and b(m,n) can be determined so that da(i,j) is
c     approximated by the sum
c
c         l-1  n
C*PL*ERROR* Comment line too long
c     (a) sum sum pbar(m,n,theta(i))*(a(m,n)*cos(m*phi(j)+b(m,n)*sin(m*phi(j))
c         n=0 m=0
c
C*PL*ERROR* Comment line too long
c     here pbar(n,m,theta) are the normalized associated legendre functions
C*PL*ERROR* Comment line too long
c     and l = min0(nlat,(nlon+2)/2).  the determination of a(m,n) and b(m,n)
C*PL*ERROR* Comment line too long
c     is called spherical harmonic analysis. a sum of this form can then be
c     used to regenerate the data in db on the new grid with the known
C*PL*ERROR* Comment line too long
c     a(m,n) and b(m,n).  this is referred to spherical harmonic synthesis.
C*PL*ERROR* Comment line too long
c     analysis and synthesis subroutines from the software package spherepack,
c     are used for these purposes.
c
C*PL*ERROR* Comment line too long
c     if da or db is not in mathematical spherical coordinates then array
C*PL*ERROR* Comment line too long
c     transposition and/or subscript reordering is used prior to harmonic
c     analysis and after harmonic synthesis.
c
c *** advantages
c
C*PL*ERROR* Comment line too long
c     the use of surface spherical harmonics to transfer spherical grid data
C*PL*ERROR* Comment line too long
c     has advantages over pointwise grid interpolation schemes on the sphere.
C*PL*ERROR* Comment line too long
c     it is highly accurate.  if p(x,y,z) is any polynomial of degree n or
C*PL*ERROR* Comment line too long
c     less in x,y,z cartesian coordinates which is restricted to the surface
C*PL*ERROR* Comment line too long
c     of the sphere, then p is exactly represented by sums of the form (a)
C*PL*ERROR* Comment line too long
c     whenever n = mino(nlat,nlon/2) (i.e., transfers with spherical harmonics
C*PL*ERROR* Comment line too long
c     have n(th) order accuracy.  by way of contrast, bilinear interpolation
C*PL*ERROR* Comment line too long
c     schemes are exact for polynomials of degree one.  bicubic interpolation
c     is exact only for polynomials of degree three or less.  the method
C*PL*ERROR* Comment line too long
c     also produces a weighted least squares fit to the data in which waves
C*PL*ERROR* Comment line too long
c     are resolved uniformly on the full sphere.  high frequencies, induced
c     by closeness of grid points near the poles (due to computational
c     or observational errors) are smoothed.  finally, the method is
C*PL*ERROR* Comment line too long
c     consistent with methods used to generate data in numerical spectral
C*PL*ERROR* Comment line too long
c     models based on spherical harmonics.  for more discussion of these and
C*PL*ERROR* Comment line too long
c     related issues,  see the article: "on the spectral approximation of
C*PL*ERROR* Comment line too long
c     discrete scalar and vector functions on the sphere," siam j. numer.
c     anal., vol 16. dec 1979, pp. 934-949, by paul swarztrauber.
c
c
c *** comment
c
C*PL*ERROR* Comment line too long
c     on a nlon by nlat or nlat by nlon grid (gaussian or equally spaced)
c     spherical harmonic analysis generates and synthesis utilizes
c     min0(nlat,(nlon+2)/2)) by nlat coefficients.  consequently, for
c     da and db,  if either
c
c             min0(nlatb,(nlonb+2)/2) < min0(nlata,(nlona+2)/2)
c
c     or if
c
c             nlatb < nlata
c
C*PL*ERROR* Comment line too long
c     then all the coefficients generated by an analysis of da cannot be used
C*PL*ERROR* Comment line too long
c     in the synthesis which generates db.  in this case "information" can be
C*PL*ERROR* Comment line too long
c     lost in generating db.  more precisely, information will be lost if the
C*PL*ERROR* Comment line too long
c     analysis of da yields nonzero coefficients which are outside the bounds
C*PL*ERROR* Comment line too long
c     determined by the db grid.  nevertheless, transference of values with
C*PL*ERROR* Comment line too long
c     spherical harmonics will yield results consistent with grid resolution
c     and is highly accurate.
c
c
c *** input arguments
c
c ... intl
c
C*PL*ERROR* Comment line too long
c     an initialization argument which should be zero on an initial call to
c     trssph.  intl should be one if trssph is being recalled and
c
c          igrida,nlona,nlata,igridb,nlonb,nlatb
c
C*PL*ERROR* Comment line too long
c     have not changed from the previous call.  if any of these arguments
C*PL*ERROR* Comment line too long
c     have changed, intl=0 must be used to avoid undetectable errors.  calls
C*PL*ERROR* Comment line too long
c     with intl=1 bypass redundant computation and save time.  it can be used
C*PL*ERROR* Comment line too long
c     when transferring multiple data sets with the same underlying grids.
c
c
c ... igrida
c
C*PL*ERROR* Comment line too long
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the given data array da as follows:
c
c     igrida(1)
c
c     = -1
c     if the latitude (or colatitude) grid for da is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs north to south
c
c     = +1
c     if the latitude (or colatitude) grid for da is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs south to north
c
c     = -2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for da is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north
c     to south
c
c     = +2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for da is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs south
c     north
c
c     igrida(2)
c
c     = 0 if the underlying grid for da is a nlona by nlata
c
c     = 1 if the underlying grid for da is a nlata by nlona
c
c
c ... nlona
c
C*PL*ERROR* Comment line too long
c     the number of longitude points on the uniform grid which partitions
C*PL*ERROR* Comment line too long
c     [0,2pi) for the given data array da.  nlona is also the first or second
c     dimension of da (see igrida(2)) in the program which calls trssph.
C*PL*ERROR* Comment line too long
c     nlona determines the grid increment in longitude as 2*pi/nlona. for
C*PL*ERROR* Comment line too long
c     example nlona = 72 for a five degree grid.  nlona must be greater than
c     or equal to 4.  the efficiency of the computation is improved when
c     nlona is a product of small prime numbers
c
c ... nlata
c
c     the number of points in the latitude (or colatitude) grid
c     for the given data array da.  nlata is also the first or second
c     dimension of da (see igrida(2)) in the program which calls trssph.
C*PL*ERROR* Comment line too long
c     if nlata is odd then the equator will be located at the (nlata+1)/2
c     gaussian grid point.  if nlata is even then the equator will be
c     located half way between the nlata/2 and nlata/2+1 grid points.
c
c *** note:
c     igrida(1)=-1 or igrida(1)=-2 and igrida(2)=1 corresponds to
c     the "usual" mathematical spherical coordinate system required
c     by most of the drivers in spherepack2.  igrida(1)=1 or igrida(1)=2
c     and igrida(2)=0 corresponds to the "usual" geophysical spherical
c     coordinate system.
c
c ... da
c
c     a two dimensional array that contains the data to be transferred.
C*PL*ERROR* Comment line too long
c     da must be dimensioned nlona by nlata in the program calling trssph if
C*PL*ERROR* Comment line too long
c     igrida(2) = 0.  da must be dimensioned nlata by nlona in the program
C*PL*ERROR* Comment line too long
c     calling trssph if igrida(2) = 1.  if da is not properly dimensioned
C*PL*ERROR* Comment line too long
c     and if the latitude (colatitude) values do not run south to north or
C*PL*ERROR* Comment line too long
c     north to south as flagged by igrida(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c ... igridb
c
C*PL*ERROR* Comment line too long
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the transformed data array db as follows:
c
c     igridb(1)
c
c     = -1
c     if the latitude (or colatitude) grid for db is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     north to south
c
c     = +1
c     if the latitude (or colatitude) grid for db is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     south to north
c
c     = -2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for db is a gaussian partition
C*PL*ERROR* Comment line too long
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north to
c     south
c
c     = +2
C*PL*ERROR* Comment line too long
c     if the latitude (or colatitude) grid for db is a gaussian partition
C*PL*ERROR* Comment line too long
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs south to
c     north
c
c
c     igridb(2)
c
c     = 0 if the underlying grid for db is a nlonb by nlatb
c
c     = 1 if the underlying grid for db is a nlatb by nlonb
c
c
c ... nlonb
c
C*PL*ERROR* Comment line too long
c     the number of longitude points on the uniform grid which partitions
C*PL*ERROR* Comment line too long
c     [0,2pi) for the transformed data array db.  nlonb is also the first or
c     second dimension of db (see igridb(2)) in the program which calls
C*PL*ERROR* Comment line too long
c     trssph.  nlonb determines the grid increment in longitude as 2*pi/nlonb.
C*PL*ERROR* Comment line too long
c     for example nlonb = 72 for a five degree grid.  nlonb must be greater
C*PL*ERROR* Comment line too long
c     than or equal to 4.  the efficiency of the computation is improved when
c     nlonb is a product of small prime numbers
c
c ... nlatb
c
c     the number of points in the latitude (or colatitude) grid
C*PL*ERROR* Comment line too long
c     for the transformed data array db.  nlatb is also the first or second
c     dimension of db (see igridb(2)) in the program which calls trssph.
C*PL*ERROR* Comment line too long
c     if nlatb is odd then the equator will be located at the (nlatb+1)/2
c     gaussian grid point.  if nlatb is even then the equator will be
c     located half way between the nlatb/2 and nlatb/2+1 grid points.
c
c ... wsave
c
c     a saved work space array that can be utilized repeatedly by trssph
c     as long as the arguments nlata,nlona,nlatb,nlonb remain unchanged.
C*PL*ERROR* Comment line too long
c     wsave is set by a intl=0 call to trssph.  wsave must not be altered
c     when trssph is being recalled with intl=1.
c
c ... lsave
c
c     the dimension of the work space wsave as it appears in the program
c     that calls trssph.  the minimum required value of lsave for the
C*PL*ERROR* Comment line too long
c     current set of input arguments is set in the output argument lsvmin.
C*PL*ERROR* Comment line too long
c     it can be determined by calling trssph with lsave=0 and printing lsvmin.
c     let
c
c          lwa =  2*nlata*la2+3*((la1-2)*(nlata+nlata-la1-1))/2+nlona+15
c
c     if the grid for da is equally spaced in (co)latitude.  let
c
c          lwa = nlata*(2*la2+3*la1-2)+3*la1*(1-la1)/2+nlona+15
c
c     if the grid for da is gaussian in (co)latitude.
c     let
c
c          lwb = nlatb*(2*lb2+3*lb1-2)+3*lb1*(1-lb1)/2+nlonb+15
c
c     if the grid for db is gaussian in (co)latitude.  let
c
c          lwb = 2*nlatb*lb2+3*((lb1-2)*(nlatb+nlatb-lb1-1))/2+nlonb+15
c
c     if the grid for db is equally spaced in (co)latitude.  then
c     the quantity
c
c          lwa + lwb
c
c     is the minimum required length of wsave.  this value is returned
C*PL*ERROR* Comment line too long
c     in the output argument lsvmin even if lsave is to small (ierror=10)
c
c ... work
c
c     a real work array that does not have to be preserved
c
c ... lwork
c
c     the dimension of the array work as it appears in the program
C*PL*ERROR* Comment line too long
c     calling trssph. the minimum required value of lwork for the current
c     set of input arguments is set in the output argument lwkmin.
c     it can be determined by calling trssph with lwork=0 and printing
c     lwkmin.  an estimate for lwork follows.  let nlat,nlon,l1,l2 be
c     defined by
c
c       nlat = max0(nlata,nlatb), nlon = nax0(nlona,nlonb),
c       l1 = min0(nlat,(nlon+2)/2), l2 = (nlat+1)/2
c
c     then the quantity
c
c          nlat*(4*l1+nlon+2*nlat+4)+3*((l1-2)*2*(2*nlat-l1-1))/2
c
c     will suffice as a length for the unsaved work space.
c
C*PL*ERROR* Comment line too long
c  *  both of the formulas above for lsave and lwork may overestimate the
C*PL*ERROR* Comment line too long
c     required minimum values.  they can be predetermined by calling trssph
c     with lsave=lwork=0 and printout of lsvmin and lwkmin.
c
c ... dwork
c
c     a double precision work array that does not have to be preserved.
c
c ... ldwork
c
c     The length of dwork in the routine calling trssph.
c     Let
c
c       nlat = max0(nlata,nlatb)
c
c     ldwork must be at least nlat*(nlat+4)
c
c *** output arguments
c
c
c ... db
c
c     a two dimensional array that contains the transformed data.  db
C*PL*ERROR* Comment line too long
c     must be dimensioned nlonb by nlatb in the program calling trssph if
c     igridb(2) = 0 or 1.  db must be dimensioned nlatb by nlonb in the
c     program calling trssph if igridb(2) = 1.  if db is not properly
C*PL*ERROR* Comment line too long
c     dimensioned and if the latitude (colatitude) values do not run south
C*PL*ERROR* Comment line too long
c     north or north to south as flagged by igrdb(1) (this cannot be checked!)
c     then incorrect results will be produced.
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
c         = 6  if igridb(1) is not -1 or +1 or -2 or +2
c
c         = 7  if igridb(2) is not 0 or 1
c
c         = 8  if nlonb is less than 4
c
c         = 9  if nlatb is less than 3

C*PL*ERROR* Comment line too long
c         =10  if there is insufficient saved work space (lsave < lsvmin)
c
C*PL*ERROR* Comment line too long
c         =11  if there is insufficient unsaved work space (lwork < lwkmin)
c
c         =12  indicates failure in an eigenvalue routine which computes
c              gaussian weights and points
c
C*PL*ERROR* Comment line too long
c         =13  if ldwork is too small (insufficient unsaved double precision
c              work space)
c
c *****************************************************
c *****************************************************
c
c     end of argument description ... code follows
c
c *****************************************************
c *****************************************************
c
      SUBROUTINE DTRSSPH(INTL,IGRIDA,NLONA,NLATA,DA,IGRIDB,NLONB,NLATB,
     +                  DB,WSAVE,LSAVE,LSVMIN,WORK,LWORK,LWKMIN,DWORK,
     +                  LDWORK,IER)
      IMPLICIT NONE
      INTEGER INTL,IGRIDA(2),NLONA,NLATA,IGRIDB(2),NLONB,NLATB
      INTEGER LSAVE,LSVMIN,LWORK,LWKMIN,LDWORK,IER
      DOUBLE PRECISION DA(*),DB(*),WSAVE(*),WORK(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)
      INTEGER IG,IGRDA,IGRDB,LA1,LA2,LB1,LB2,LWA,LWB,IAA,IAB,IBA,IBB
      INTEGER LWK3,LWK4,LW,IW,JB,NT,ISYM,NLAT
c
C*PL*ERROR* Comment line too long
c     include a save statement to ensure local variables in trssph, set during
c     an intl=0 call, are preserved if trssph is recalled with intl=1
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
      IG = IGRIDB(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 7
      IG = IGRIDB(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 8
      IF (NLONB.LT.4) RETURN
      IER = 9
      IF (NLATB.LT.3) RETURN
      IER = 0

      IGRDA = IABS(IGRIDA(1))
      IGRDB = IABS(IGRIDB(1))
      IF (INTL.EQ.0) THEN
          LA1 = MIN0(NLATA, (NLONA+2)/2)
          LA2 = (NLATA+1)/2
          LB1 = MIN0(NLATB, (NLONB+2)/2)
          LB2 = (NLATB+1)/2
c
c     set saved work space length for analysis
c
          IF (IGRDA.EQ.1) THEN
c
c     saved space for analysis on  equally spaced grid
c
              LWA = 2*NLATA*LA2 + 3* ((LA1-2)* (NLATA+NLATA-LA1-1))/2 +
     +              NLONA + 15
          ELSE
c
c     saved space for analysis on gaussian grid
c
              LWA = NLATA* (2*LA2+3*LA1-2) + 3*LA1* (1-LA1)/2 + NLONA +
     +              15
          END IF
c
c     set wsave pointer
c
          JB = 1 + LWA
c
c     set pointers for spherical harmonic coefs
c
          IAA = 1
          IBA = IAA + LA1*NLATA
          IAB = IBA + LA1*NLATA
          IF (IGRDB.EQ.2) THEN
c
c     set saved work space length for gaussian synthesis
c
              LWB = NLATB* (2*LB2+3*LB1-2) + 3*LB1* (1-LB1)/2 + NLONB +
     +              15
          ELSE
c
c     set saved work space length for equally spaced synthesis
c
              LWB = 2*NLATB*LB2 + 3* ((LB1-2)* (NLATB+NLATB-LB1-1))/2 +
     +              NLONB + 15
          END IF
c
c     set minimum saved work space length
c
          LSVMIN = LWA + LWB
c
c     set remaining harmonic pointer
c
          IBB = IAB + LB1*NLATB
c
c     set pointers for remaining work
c
          IW = IBB + LB1*NLATB
c
c     set remaining work space length in lw
c
          LW = LWORK - IW
          LWK3 = NLATA*NLONA*2
          LWK4 = NLATB*NLONB*2
c
c     set minimum unsaved work space required by trssph
c
          LWKMIN = IW + MAX0(LWK3,LWK4)
c
c     set error flags if saved or unsaved work spaces are insufficient
c
          IER = 10
          IF (LSAVE.LT.LSVMIN) RETURN
          IER = 11
          IF (LWORK.LT.LWKMIN) RETURN
          IER = 13
          NLAT = MAX0(NLATA,NLATB)
          IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
          IER = 0
          IF (IGRDA.EQ.1) THEN
c
c     initialize wsave for equally spaced analysis
c
              CALL DSHAECI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
          ELSE
c
c     initialize wsave for gaussian analysis
c
              CALL DSHAGCI(NLATA,NLONA,WSAVE,LWA,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 12
                  RETURN
              END IF
          END IF

          IF (IGRDB.EQ.2) THEN
c
c     initialize wsave for gaussian synthesis
c
              CALL DSHSGCI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
              IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
                  IER = 12
                  RETURN
              END IF
          ELSE
c
c     initialize wsave for equally spaced synthesis
c
              CALL DSHSECI(NLATB,NLONB,WSAVE(JB),LWB,DWORK,LDWORK,IER)
          END IF
c
c     end of initialization (intl=0) call
c
      END IF
c
c     transpose and/or reorder (co)latitude if necessary for da
c     (arrays must have latitude (colatitude) as the first dimension
c     and run north to south for spherepack software)
c
      IF (IGRIDA(2).EQ.0) CALL DTRSPLAT(NLONA,NLATA,DA,WORK)
      IF (IGRIDA(1).GT.0) CALL DCONVLAT(NLATA,NLONA,DA)

      NT = 1
      ISYM = 0
      IF (IGRDA.EQ.2) THEN
c
c     do spherical harmonic analysis of "adjusted" da on gaussian grid
c
          CALL DSHAGC(NLATA,NLONA,ISYM,NT,DA,NLATA,NLONA,WORK(IAA),
     +               WORK(IBA),LA1,NLATA,WSAVE,LWA,WORK(IW),LW,IER)
      ELSE
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic analysis of "adjusted" da on equally spaced grid
c
          CALL DSHAEC(NLATA,NLONA,ISYM,NT,DA,NLATA,NLONA,WORK(IAA),
     +               WORK(IBA),LA1,NLATA,WSAVE,LWA,WORK(IW),LW,IER)
      END IF
c
c     transfer da grid coefficients to db grid coefficients
c     truncating to zero as necessary
c
      CALL DTRAB(LA1,NLATA,WORK(IAA),WORK(IBA),LB1,NLATB,WORK(IAB),
     +          WORK(IBB))

      IF (IGRDB.EQ.1) THEN
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic synthesis on nlatb by nlonb equally spaced grid
c
          CALL DSHSEC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE(JB),LWB,WORK(IW),LW,IER)
      ELSE
c
c     do spherical harmonic synthesis on nlatb by nlonb gaussian grid
c
          CALL DSHSGC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE(JB),LWB,WORK(IW),LW,IER)
      END IF
c
C*PL*ERROR* Comment line too long
c     both da,db are currently latitude by longitude north to south arrays
c     restore da and set db to agree with flags in igrida and igridb
c
      IF (IGRIDA(1).GT.0) CALL DCONVLAT(NLATA,NLONA,DA)
      IF (IGRIDB(1).GT.0) CALL DCONVLAT(NLATB,NLONB,DB)
      IF (IGRIDA(2).EQ.0) CALL DTRSPLAT(NLATA,NLONA,DA,WORK)
      IF (IGRIDB(2).EQ.0) CALL DTRSPLAT(NLATB,NLONB,DB,WORK)
      RETURN
      END

      SUBROUTINE DTRAB(MA,NA,AA,BA,MB,NB,AB,BB)
      IMPLICIT NONE
      INTEGER MA,NA,MB,NB,I,J,M,N
      DOUBLE PRECISION AA(MA,NA),BA(MA,NA),AB(MB,NB),BB(MB,NB)
c
c     set coefficients for b grid from coefficients for a grid
c
      M = MIN0(MA,MB)
      N = MIN0(NA,NB)
      DO J = 1,N
          DO I = 1,M
              AB(I,J) = AA(I,J)
              BB(I,J) = BA(I,J)
          END DO
      END DO
c
c     set coefs outside triangle to zero
c
      DO I = M + 1,MB
          DO J = 1,NB
              AB(I,J) = 0.0D0
              BB(I,J) = 0.0D0
          END DO
      END DO
      DO J = N + 1,NB
          DO I = 1,MB
              AB(I,J) = 0.0D0
              BB(I,J) = 0.0D0
          END DO
      END DO
      RETURN
      END

      SUBROUTINE DTRSPLAT(N,M,DATA,WORK)
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

      SUBROUTINE DCONVLAT(NLAT,NLON,DATA)
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
