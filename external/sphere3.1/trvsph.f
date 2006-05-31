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
c     subroutine trvsph transfers vector data given in (ua,va) on a grid on
c     the full sphere to vector data in (ub,vb) on a grid on the full sphere.
c     the grids on which (ua,va) is given and (ub,vb) is generated can be
c     specified independently of each other (see the input arguments igrida,
c     igridb,iveca,ivecb).  ua and ub are the east longitudinal components of
c     the given and transformed vector fields.  va is either the latitudinal
c     or colatitudinal component of the given vector field (see iveca).
c     vb is either the latitudinal or colatitudinal component of the
c     transformed vector field (see ivecb).  for transferring scalar data
c     on the sphere, use subroutine trssph.
c
c *   notice that scalar and vector quantities are fundamentally different
c     on the sphere.  for example, vectors are discontinuous and multiple
c     valued at the poles.  scalars are continuous and single valued at the
c     poles. erroneous results would be produced if one attempted to transfer
c     vector fields between grids with subroutine trssph applied to each
c     component of the vector.
c
c *** underlying grid assumptions and a description
c
c     discussions with the ncar scd data support group and others indicate
c     there is no standard grid for storing observational or model generated
c     data on the sphere.  subroutine trvsph was designed to handle most
c     cases likely to be encountered when moving data from one grid format
c     to another.
c
c     the grid on which (ua,va) is given must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
c     longitude, which can be either the first or second dimension of ua,va
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of ua,va, has south
c     to north or north to south orientation with increasing subscript
c     value in ua,va (see the argument igrida).
c
c     the grid on which ub,vb is generated must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
c     longitude, which can be either the first or second dimension of ub,vb
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
c     it is not necessary to communicate to subroutine trvsph whether the
c     underlying grids are in latitude or colatitude.  it is only necessary
c     to communicate whether they run south to north or north to south with
c     increasing subscripts.  a brief discussion of latitude and colatitude
c     follows.  equally spaced latitude grids are assumed to subdivide
c     [-pi/2,pi/2] with the south pole at -pi/2 and north pole at pi/2.
c     equally spaced colatitude grids subdivide [0,pi] with the north pole
c     at 0 and south pole at pi.  equally spaced partitions on the sphere
c     include both poles.  gaussian latitude grids subdivide (-pi/2,pi/2)
c     and gaussian colatitude grids subdivide (0,pi).  gaussian grids do not
c     include the poles.  the gaussian grid points are uniquely determined by
c     the size of the partition.  they can be computed in colatitude in
c     (0,pi) (north to south) in double precision by the spherepack subroutine
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
c     be nlat gaussian latitude points in the open interval (-pi/2,pi/2).
c     these are related by
c
c          th(i) = -pi/2 + cth(i)  (i=1,...,nlat)
c
c     if the (ua,va) or (ub,vb) grid is equally spaced in (co)latitude then
c
c          ctht(i) = (i-1)*pi/(nlat-1)
c                                               (i=1,...,nlat)
c          tht(i) = -pi/2 + (i-1)*pi/(nlat-1)
c
c     define the equally spaced (north to south) colatitude and (south to
c     north) latitude grids.
c
c *** method (simplified description)
c
c    (1)
c
c     the vector field (ua,va) is reformated to a vector field in mathematical
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
c     (ub,vb) using array transpositions, subscript reordering and negation
c     as necessary
c
c
c *** advantages
c
c     the use of vector spherical harmonics to transfer vector data is
c     highly accurate and preserves properties of vectors on the sphere.
c     the method produces a weighted least squares fit to vector data in
c     which waves are resolved uniformly on the full sphere.  high frequencies
c     induced by closeness of grid points near the poles (due to computational
c     or observational errors) are smoothed.  the method is consistent with
c     methods used to generate vector data in numerical spectral models based
c     on spherical harmonics.  for more discussion of these and related issues,
c     see "on the spectral approximation of discrete scalar and vector
c     functions on the sphere," siam j. numer. anal., vol. 16, december 1979,
c     pp. 934-949, by paul swarztrauber.
c
c
c *** comment
c
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
c     then all the coefficients generated by an analysis of ua,va cannot be
c     used in the synthesis which generates ub,vb.  in this case "information"
c     can be lost in generating ub,vb.  more precisely, information will be
c     lost if the analysis of ua,va yields nonzero coefficients which are
c     outside the coefficient bounds determined by the ub,vb grid. still
c     transference with vector spherical harmonics will yield results
c     consistent with grid resolution and is highly accurate.
c
c *** input arguments
c
c ... intl
c
c     an initialization argument which should be zero on an initial call to
c     trvsph.  intl should be one if trvsph is being recalled and
c
c          igrida,nlona,nlata,iveca,igridb,nlonb,nlatb,ivecb
c
c     have not changed from the previous call.  if any of these arguments have
c     changed intl=0 must be used to avoid undetectable errors.  when allowed,
c     calls with intl=1 bypass redundant computation and save time.  it can
c     be used when transferring multiple vector data sets with the same
c     underlying grids.
c
c ... igrida
c
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the given vector data (ua,va) as follows:
c
c     igrida(1)
c
c     = -1
c     if the latitude (or colatitude) grid for ua,va is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs north to south with increasing subscript value
c
c     = +1
c     if the latitude (or colatitude) grid for ua,va is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     runs south to north with increasing subscript value
c
c     = -2
c     if the latitude (or colatitude) grid for ua,va is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north
c     to south with increasing subscript value
c
c     = +2
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
c     the number of longitude points on the uniform grid which partitions
c     [0,2pi) for the given vector (ua,va).  nlona is also the first or second
c     dimension of ua,va (see igrida(2)) in the program which calls trvsph.
c     nlona determines the grid increment in longitude as 2*pi/nlona. for
c     example nlona = 72 for a five degree grid.  nlona must be greater than
c     or equal to 4.  the efficiency of the computation is improved when
c     nlona is a product of small prime numbers
c
c ... nlata
c
c     the number of points in the latitude (or colatitude) grid for the
c     given vector (ua,va).  nlata is also the first or second dimension
c     of ua and va (see igrida(2)) in the program which calls trvsph.
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
c     by most of the drivers in spherepack2.  igrida(1)=1 or igrida(1)=2,
c     igrida(2)=0, and iveca=0 corresponds to the "usual" geophysical
c     spherical coordinate system.
c
c
c ... ua
c
c     ua is the east longitudinal component of the given vector field.
c     ua must be dimensioned nlona by nlata in the program calling trvsph if
c     igrida(2) = 0.  ua must be dimensioned nlata by nlona in the program
c     calling trvsph if igrida(2) = 1.  if ua is not properly dimensioned
c     and if the latitude (colatitude) values do not run south to north or
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
c     an integer vector dimensioned two which identifies the underlying grid
c     on the full sphere for the transformed vector (ub,vb) as follows:
c
c     igridb(1)
c
c     = -1
c     if the latitude (or colatitude) grid for ub,vb is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     north to south
c
c     = +1
c     if the latitude (or colatitude) grid for ub,vb is an equally spaced
c     partition of [-pi/2,pi/2] ( or [0,pi] ) including the poles which
c     south to north
c
c     = -2
c     if the latitude (or colatitude) grid for ub,vb is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north to
c     south
c
c     = +2
c     if the latitude (or colatitude) grid for ub,vb is a gaussian partition
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
c     the number of longitude points on the uniform grid which partitions
c     [0,2pi) for the transformed vector (ub,vb).  nlonb is also the first or
c     second dimension of ub and vb (see igridb(2)) in the program which calls
c     trvsph.  nlonb determines the grid increment in longitude as 2*pi/nlonb.
c     for example nlonb = 72 for a five degree grid.  nlonb must be greater
c     than or equal to 4.  the efficiency of the computation is improved when
c     nlonb is a product of small prime numbers
c
c ... nlatb
c
c     the number of points in the latitude (or colatitude) grid for the
c     transformed vector (ub,vb).  nlatb is also the first or second dimension
c     of ub and vb (see igridb(2)) in the program which calls trvsph.
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
c     by most of the drivers in spherepack2.  igridb(1)=1 or igridb(1)=2,
c     igridb(2)=0, and ivecb=0 corresponds to the "usual" geophysical
c     spherical coordinate system.
c
c ... wsave
c
c     a saved work space array that can be utilized repeatedly by trvsph
c     as long as the arguments nlata,nlona,nlatb,nlonb remain unchanged.
c     wsave is set by a intl=0 call to trvsph.  wsave must not be altered
c     when trvsph is being recalled with intl=1.
c
c ... lsave
c
c     the dimension of the work space wsave as it appears in the program
c     that calls trvsph.  the minimum required value of lsave for the
c     current set of input arguments is set in the output argument lsvmin.
c     it can be determined by calling trvsph with lsave=0 and printing lsvmin.
c
c          la1 = min0(nlata,(nlona+1)/2), la2 = (nlata+1)/2
c
c          lb1 = min0(nlatb,(nlonb+1)/2), lb2 = (nlatb+1)/2
c
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
c     lwkmin.  an estimate for lwork follows.  let nlat = max0(nlata,nlatb),
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
c     a two dimensional array that contains the east longitudinal component
c     of the transformed vector data.  ub
c     must be dimensioned nlonb by nlatb in the program calling trvsph if
c     igridb(2)=0.  ub must be dimensioned nlatb by nlonb in the program
c     calling trvsph if igridb(2)=1.  if ub is not properly dimensioned
c     and if the latitude (colatitude) values do not run south to north or
c     north to south as flagged by igrdb(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c
c ... vb
c
c     a two dimensional array that contains the latitudinal or colatitudinal
c     component of the transformed vector data (see ivecb).
c     vb must be dimensioned nlonb by nlatb in the program calling trvsph if
c     igridb(2)=0.  vb must be dimensioned nlatb by nlonb in the program
c     calling trvsph if igridb(2)=1.  if vb is not properly dimensioned
c     and if the latitude (colatitude) values do not run south to north or
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
c         =12  if there is insufficient saved work space (lsave < lsvmin)
c
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
      subroutine trvsph (intl,igrida,nlona,nlata,iveca,ua,va,
     +igridb,nlonb,nlatb,ivecb,ub,vb,wsave,lsave,lsvmin,work,
     +lwork,lwkmin,dwork,ldwork,ier)
      implicit none
      integer intl,igrida(2),nlona,nlata,igridb(2),nlonb,nlatb
      integer iveca,ivecb,lsave,lsvmin,lwork,lwkmin,ldwork,ier
      real ua(*),va(*),ub(*),vb(*),wsave(*),work(*)
      double precision dwork(*)
      integer ig,igrda,igrdb,la1,la2,lb1,lb2,lwa,lwb
      integer iabr,iabi,iacr,iaci,ibbr,ibbi,ibcr,ibci
      integer nlat,lwk1,lwk2,lw,iw,jb,nt,ityp
c
c     include a save statement to ensure local variables in trvsph, set during
c     an intl=0 call, are preserved if trvsph is recalled with intl=1
c
      save
c
c     check input arguments
c
      ier = 1
      if (intl*(intl-1).ne.0) return
      ier = 2
      ig = igrida(1)
      if ((ig-1)*(ig+1)*(ig-2)*(ig+2).ne.0) return
      ier = 3
      ig = igrida(2)
      if (ig*(ig-1).ne.0) return
      ier = 4
      if (nlona .lt. 4) return
      ier = 5
      if (nlata .lt.3) return
      ier = 6
      if (iveca*(iveca-1).ne.0) return
      ier = 7
      ig = igridb(1)
      if ((ig-1)*(ig+1)*(ig-2)*(ig+2).ne.0) return
      ier = 8
      ig = igridb(2)
      if (ig*(ig-1).ne.0) return
      ier = 9
      if (nlonb .lt.4) return
      ier = 10
      if (nlatb .lt.3) return
      ier = 11
      if (ivecb*(ivecb-1).ne.0) return
      ier = 0
      igrda = iabs(igrida(1))
      igrdb = iabs(igridb(1))
      if (intl.eq.0) then
      la1 = min0(nlata,(nlona+1)/2)
      la2 = (nlata+1)/2
      lb1 = min0(nlatb,(nlonb+1)/2)
      lb2 = (nlatb+1)/2
c
c     saved space for analysis on a grid
c
      lwa = 4*nlata*la2+3*max0(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15
c
c     set saved work space length for synthesis on b grid
c
      lwb = 4*nlatb*lb2+3*max0(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15
c
c     set minimum required saved work space length
c
      lsvmin = lwa + lwb
c
c     set wsave pointer
c
      jb = 1+lwa
c
c     set pointers for vector spherical harmonic coefs in work
c
      iabr = 1
      iabi = iabr + la1*nlata
      iacr = iabi + la1*nlata
      iaci = iacr + la1*nlata
      ibbr = iaci + la1*nlata
      ibbi = ibbr + lb1*nlatb
      ibcr = ibbi + lb1*nlatb
      ibci = ibcr + lb1*nlatb
c
c     set pointers for remaining work
c
      iw = ibci + lb1*nlatb
c
c     set remaining work space length in lw
c
      lw = lwork - iw
c
c     compute unsaved space for analysis and synthesis
c
      lwk1 = 2*nlata*(2*nlona+max0(6*la2,nlona))
      lwk2 = 2*nlatb*(2*nlonb+max0(6*lb2,nlonb))
c
c     set minimum unsaved work space required by trvsph
c
      lwkmin = iw + max0(lwk1,lwk2)
c
c     set error flags if saved or unsaved work space is insufficient
c
      ier = 12
      if (lsave .lt. lsvmin) return
      ier = 13
      if (lwork .lt. lwkmin) return
      ier = 15
      nlat = max0(nlata,nlatb)
      if (ldwork .lt. 2*nlat*(nlat+1)+1) return
      ier = 0
      if (igrda .eq. 1) then
c
c     initialize wsave for equally spaced analysis
c
      call vhaeci(nlata,nlona,wsave,lwa,dwork,ldwork,ier)
      else
c
c     initialize wsave for gaussian analysis
c
      call vhagci(nlata,nlona,wsave,lwa,dwork,ldwork,ier)
      if (ier.ne.0) then
c
c     flag failure in spherepack gaussian software
c
      ier = 14
      return
      end if
      end if

      if (igrdb .eq. 2) then
c
c     initialize wsave for gaussian synthesis
c
      call vhsgci(nlatb,nlonb,wsave(jb),lwb,dwork,ldwork,ier)
      if (ier.ne.0) then
c
c     flag failure in spherepack gaussian software
c
      ier = 14
      return
      end if
      else
c
c     initialize wsave for equally spaced synthesis
c
      call vhseci(nlatb,nlonb,wsave(jb),lwb,dwork,ldwork,ier)
      end if
c
c     end of initialization (intl=0) call
c
      end if
c
c     convert the vector field (ua,va) to mathematical spherical coordinates
c
      if (igrida(2).eq.0) then
	call trvplat(nlona,nlata,ua,work)
	call trvplat(nlona,nlata,va,work)
      end if
      if (igrida(1) .gt. 0) then
	call covlat(nlata,nlona,ua)
	call covlat(nlata,nlona,va)
      end if
      if (iveca .eq. 0) then
	call negv(nlata,nlona,va)
      end if
      nt = 1
      ityp = 0
c
c     analyze vector field
c
      if (igrda .eq. 2) then
	call vhagc(nlata,nlona,ityp,nt,va,ua,nlata,nlona,work(iabr),
     +  work(iabi),work(iacr),work(iaci),la1,nlata,wsave,lwa,work(iw),
     +  lw,ier)
      else
	call vhaec(nlata,nlona,ityp,nt,va,ua,nlata,nlona,work(iabr),
     +  work(iabi),work(iacr),work(iaci),la1,nlata,wsave,lwa,work(iw),
     +  lw,ier)
      end if
c
c     transfer a grid coefficients to b grid coefficients
c
      call trvab(la1,nlata,work(iabr),work(iabi),work(iacr),work(iaci),
     +           lb1,nlatb,work(ibbr),work(ibbi),work(ibcr),work(ibci))
c
c     synthesize on b grid
c
      if (igrdb .eq. 1) then
      call vhsec(nlatb,nlonb,ityp,nt,vb,ub,nlatb,nlonb,work(ibbr),
     +work(ibbi),work(ibcr),work(ibci),lb1,nlatb,wsave(jb),lwb,
     +work(iw),lw,ier)
      else
      call vhsgc(nlatb,nlonb,ityp,nt,vb,ub,nlatb,nlonb,work(ibbr),
     +work(ibbi),work(ibcr),work(ibci),lb1,nlatb,wsave(jb),lwb,work(iw),
     +lw,ier)
      end if
c
c     restore a grid and b grid vector fields (now in math coordinates) to
c     agree with grid flags in igrida,iveca,igridb,ivecb
c
      if (iveca .eq. 0) then
	call negv(nlata,nlona,va)
      end if
      if (ivecb .eq. 0) then
	call negv(nlatb,nlonb,vb)
      end if
      if (igrida(1).gt. 0) then
	call covlat(nlata,nlona,ua)
	call covlat(nlata,nlona,va)
      end if
      if (igridb(1) .gt. 0) then
	call covlat(nlatb,nlonb,ub)
	call covlat(nlatb,nlonb,vb)
      end if
      if (igrida(2) .eq. 0) then
	call trvplat(nlata,nlona,ua,work)
	call trvplat(nlata,nlona,va,work)
      end if
      if (igridb(2) .eq. 0) then
	call trvplat(nlatb,nlonb,ub,work)
	call trvplat(nlatb,nlonb,vb,work)
      end if
      return
      end
      subroutine negv(nlat,nlon,v)
c
c     negate (co)latitudinal vector componenet
c
      implicit none
      integer nlat,nlon,i,j
      real v(nlat,nlon)
      do j=1,nlon
	do i=1,nlat
	  v(i,j) = -v(i,j)
	end do
      end do
      return
      end
      subroutine trvab(ma,na,abr,abi,acr,aci,mb,nb,bbr,bbi,bcr,bci)
      implicit none
      integer ma,na,mb,nb,i,j,m,n
      real abr(ma,na),abi(ma,na),acr(ma,na),aci(ma,na)
      real bbr(mb,nb),bbi(mb,nb),bcr(mb,nb),bci(mb,nb)
c
c     set coefficients for b grid from coefficients for a grid
c
      m = min0(ma,mb)
      n = min0(na,nb)
      do j=1,n
	do i=1,m
	  bbr(i,j) = abr(i,j)
	  bbi(i,j) = abi(i,j)
	  bcr(i,j) = acr(i,j)
	  bci(i,j) = aci(i,j)
	end do
      end do
c
c     set coefs outside triangle to zero
c
      do i=m+1,mb
	do j=1,nb
	  bbr(i,j) = 0.0
	  bbi(i,j) = 0.0
	  bcr(i,j) = 0.0
	  bci(i,j) = 0.0
	end do
      end do
      do j=n+1,nb
	do i=1,mb
	  bbr(i,j) = 0.0
	  bbi(i,j) = 0.0
	  bcr(i,j) = 0.0
	  bci(i,j) = 0.0
	end do
      end do
      return
      end
      subroutine trvplat(n,m,data,work)
c
c     transpose the n by m array data to a m by n array data
c     work must be at least n*m words long
c
      implicit none
      integer n,m,i,j,ij,ji
      real data(*),work(*)
      do j=1,m
	do i=1,n
	  ij = (j-1)*n+i
	  work(ij) = data(ij)
	end do
      end do
      do i=1,n
	do j=1,m
	  ji = (i-1)*m+j
	  ij = (j-1)*n+i
	  data(ji) = work(ij)
	end do
      end do
      return
      end
      subroutine covlat(nlat,nlon,data)
c
c     reverse order of latitude (colatitude) grids
c
      implicit none
      integer nlat,nlon,nlat2,i,ib,j
      real data(nlat,nlon),temp
      nlat2 = nlat/2
      do i=1,nlat2
	ib = nlat-i+1
	do j=1,nlon
	  temp = data(i,j)
	  data(i,j) = data(ib,j)
	  data(ib,j) = temp
	end do
      end do
      return
      end
