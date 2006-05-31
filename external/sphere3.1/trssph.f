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
c     subroutine trssph transfers data given in array da on a grid on the
c     full sphere to data in array db on a grid on the full sphere.  the
c     grids on which da is given and db is generated can be specified
c     independently of each other (see description below and the arguments
c     igrida,igridb).  for transferring vector data on the sphere, use
c     subroutine trvsph.

c     notice that scalar and vector quantities are fundamentally different
c     on the sphere.  for example, vectors are discontinuous and multiple
c     valued at the poles.  scalars are continuous and single valued at the
c     poles. erroneous results would be produced if one attempted to transfer
c     vector fields between grids with subroutine trssph applied to each
c     component of the vector.
c
c
c *** underlying grid assumptions and a description
c
c     discussions with the ncar scd data support group and others indicate
c     there is no standard grid for storing observational or model generated
c     data on the sphere.  subroutine trssph was designed to handle most
c     cases likely to be encountered when moving data from one grid format
c     to another.
c
c     the grid on which da is given must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
c     longitude, which can be either the first or second dimension of da,
c     subdivides [0,2pi) excluding the periodic point 2pi.  (co)latitude,
c     which can be the second or first dimension of da, has south
c     to north or north to south orientation with increasing subscript
c     value in da (see the argument igrida).
c
c     the grid on which db is generated must be equally spaced in longitude
c     and either equally spaced or gaussian in latitude (or colatitude).
c     longitude, which can be either the first or second dimension of db,
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
c     it is not necessary to communicate to subroutine trssph whether the
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
c     define the equally spaced (north to south) colatitude and (south to
c     north) latitude grids.
c
c
c *** method (simplified description)
c
c     for simplicity, assume da is a nlat by nlon data tabulation and da(i,j)
c     is the value at latitude theta(i) and longitude phi(j).  then
c     coefficients a(m,n) and b(m,n) can be determined so that da(i,j) is
c     approximated by the sum
c
c         l-1  n
c     (a) sum sum pbar(m,n,theta(i))*(a(m,n)*cos(m*phi(j)+b(m,n)*sin(m*phi(j))
c         n=0 m=0
c
c     here pbar(n,m,theta) are the normalized associated legendre functions
c     and l = min0(nlat,(nlon+2)/2).  the determination of a(m,n) and b(m,n)
c     is called spherical harmonic analysis. a sum of this form can then be
c     used to regenerate the data in db on the new grid with the known
c     a(m,n) and b(m,n).  this is referred to spherical harmonic synthesis.
c     analysis and synthesis subroutines from the software package spherepack,
c     are used for these purposes.
c
c     if da or db is not in mathematical spherical coordinates then array
c     transposition and/or subscript reordering is used prior to harmonic
c     analysis and after harmonic synthesis.
c
c *** advantages
c
c     the use of surface spherical harmonics to transfer spherical grid data
c     has advantages over pointwise grid interpolation schemes on the sphere.
c     it is highly accurate.  if p(x,y,z) is any polynomial of degree n or
c     less in x,y,z cartesian coordinates which is restricted to the surface
c     of the sphere, then p is exactly represented by sums of the form (a)
c     whenever n = mino(nlat,nlon/2) (i.e., transfers with spherical harmonics
c     have n(th) order accuracy.  by way of contrast, bilinear interpolation
c     schemes are exact for polynomials of degree one.  bicubic interpolation
c     is exact only for polynomials of degree three or less.  the method
c     also produces a weighted least squares fit to the data in which waves
c     are resolved uniformly on the full sphere.  high frequencies, induced
c     by closeness of grid points near the poles (due to computational
c     or observational errors) are smoothed.  finally, the method is
c     consistent with methods used to generate data in numerical spectral
c     models based on spherical harmonics.  for more discussion of these and
c     related issues,  see the article: "on the spectral approximation of
c     discrete scalar and vector functions on the sphere," siam j. numer.
c     anal., vol 16. dec 1979, pp. 934-949, by paul swarztrauber.
c
c
c *** comment
c
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
c     then all the coefficients generated by an analysis of da cannot be used
c     in the synthesis which generates db.  in this case "information" can be
c     lost in generating db.  more precisely, information will be lost if the
c     analysis of da yields nonzero coefficients which are outside the bounds
c     determined by the db grid.  nevertheless, transference of values with
c     spherical harmonics will yield results consistent with grid resolution
c     and is highly accurate.
c
c
c *** input arguments
c
c ... intl
c
c     an initialization argument which should be zero on an initial call to
c     trssph.  intl should be one if trssph is being recalled and
c
c          igrida,nlona,nlata,igridb,nlonb,nlatb
c
c     have not changed from the previous call.  if any of these arguments
c     have changed, intl=0 must be used to avoid undetectable errors.  calls
c     with intl=1 bypass redundant computation and save time.  it can be used
c     when transferring multiple data sets with the same underlying grids.
c
c
c ... igrida
c
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
c     if the latitude (or colatitude) grid for da is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north
c     to south
c
c     = +2
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
c     the number of longitude points on the uniform grid which partitions
c     [0,2pi) for the given data array da.  nlona is also the first or second
c     dimension of da (see igrida(2)) in the program which calls trssph.
c     nlona determines the grid increment in longitude as 2*pi/nlona. for
c     example nlona = 72 for a five degree grid.  nlona must be greater than
c     or equal to 4.  the efficiency of the computation is improved when
c     nlona is a product of small prime numbers
c
c ... nlata
c
c     the number of points in the latitude (or colatitude) grid
c     for the given data array da.  nlata is also the first or second
c     dimension of da (see igrida(2)) in the program which calls trssph.
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
c     da must be dimensioned nlona by nlata in the program calling trssph if
c     igrida(2) = 0.  da must be dimensioned nlata by nlona in the program
c     calling trssph if igrida(2) = 1.  if da is not properly dimensioned
c     and if the latitude (colatitude) values do not run south to north or
c     north to south as flagged by igrida(1) (this cannot be checked!) then
c     incorrect results will be produced.
c
c ... igridb
c
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
c     if the latitude (or colatitude) grid for db is a gaussian partition
c     of (-pi/2,pi/2) ( or (0,pi) ) excluding the poles which runs north to
c     south
c
c     = +2
c     if the latitude (or colatitude) grid for db is a gaussian partition
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
c     the number of longitude points on the uniform grid which partitions
c     [0,2pi) for the transformed data array db.  nlonb is also the first or
c     second dimension of db (see igridb(2)) in the program which calls
c     trssph.  nlonb determines the grid increment in longitude as 2*pi/nlonb.
c     for example nlonb = 72 for a five degree grid.  nlonb must be greater
c     than or equal to 4.  the efficiency of the computation is improved when
c     nlonb is a product of small prime numbers
c
c ... nlatb
c
c     the number of points in the latitude (or colatitude) grid
c     for the transformed data array db.  nlatb is also the first or second
c     dimension of db (see igridb(2)) in the program which calls trssph.
c     if nlatb is odd then the equator will be located at the (nlatb+1)/2
c     gaussian grid point.  if nlatb is even then the equator will be
c     located half way between the nlatb/2 and nlatb/2+1 grid points.
c
c ... wsave
c
c     a saved work space array that can be utilized repeatedly by trssph
c     as long as the arguments nlata,nlona,nlatb,nlonb remain unchanged.
c     wsave is set by a intl=0 call to trssph.  wsave must not be altered
c     when trssph is being recalled with intl=1.
c
c ... lsave
c
c     the dimension of the work space wsave as it appears in the program
c     that calls trssph.  the minimum required value of lsave for the
c     current set of input arguments is set in the output argument lsvmin.
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
c     in the output argument lsvmin even if lsave is to small (ierror=10)
c
c ... work
c
c     a real work array that does not have to be preserved
c
c ... lwork
c
c     the dimension of the array work as it appears in the program
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
c  *  both of the formulas above for lsave and lwork may overestimate the
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
c     must be dimensioned nlonb by nlatb in the program calling trssph if
c     igridb(2) = 0 or 1.  db must be dimensioned nlatb by nlonb in the
c     program calling trssph if igridb(2) = 1.  if db is not properly
c     dimensioned and if the latitude (colatitude) values do not run south
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

c         =10  if there is insufficient saved work space (lsave < lsvmin)
c
c         =11  if there is insufficient unsaved work space (lwork < lwkmin)
c
c         =12  indicates failure in an eigenvalue routine which computes
c              gaussian weights and points
c
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
      SUBROUTINE TRSSPH (INTL,IGRIDA,NLONA,NLATA,DA,IGRIDB,NLONB,NLATB,
     +DB,WSAVE,LSAVE,LSVMIN,WORK,LWORK,LWKMIN,DWORK,LDWORK,IER)
      implicit none
      integer intl,igrida(2),nlona,nlata,igridb(2),nlonb,nlatb
      integer lsave,lsvmin,lwork,lwkmin,ldwork,ier
      real da(*),db(*),wsave(*),work(*)
      double precision dwork(*)
      integer ig,igrda,igrdb,la1,la2,lb1,lb2,lwa,lwb,iaa,iab,iba,ibb
      integer lwk3,lwk4,lw,iw,jb,nt,isym,nlat
c
c     include a save statement to ensure local variables in trssph, set during
c     an intl=0 call, are preserved if trssph is recalled with intl=1
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
      ig = igridb(1)
      if ((ig-1)*(ig+1)*(ig-2)*(ig+2).ne.0) return
      ier = 7
      ig = igridb(2)
      if (ig*(ig-1).ne.0) return
      ier = 8
      if (nlonb .lt.4) return
      ier = 9
      if (nlatb .lt.3) return
      ier = 0

      igrda = iabs(igrida(1))
      igrdb = iabs(igridb(1))
      if (intl.eq.0) then
      la1 = min0(nlata,(nlona+2)/2)
      la2 = (nlata+1)/2
      lb1 = min0(nlatb,(nlonb+2)/2)
      lb2 = (nlatb+1)/2
c
c     set saved work space length for analysis
c
      if (igrda .eq. 1) then
c
c     saved space for analysis on  equally spaced grid
c
      lwa =  2*nlata*la2+3*((la1-2)*(nlata+nlata-la1-1))/2+nlona+15
      else
c
c     saved space for analysis on gaussian grid
c
      lwa = nlata*(2*la2+3*la1-2)+3*la1*(1-la1)/2+nlona+15
      end if
c
c     set wsave pointer
c
      jb = 1+lwa
c
c     set pointers for spherical harmonic coefs
c
      iaa = 1
      iba = iaa+la1*nlata
      iab = iba+la1*nlata
      if (igrdb .eq. 2) then
c
c     set saved work space length for gaussian synthesis
c
      lwb = nlatb*(2*lb2+3*lb1-2)+3*lb1*(1-lb1)/2+nlonb+15
      else
c
c     set saved work space length for equally spaced synthesis
c
      lwb = 2*nlatb*lb2+3*((lb1-2)*(nlatb+nlatb-lb1-1))/2+nlonb+15
      end if
c
c     set minimum saved work space length
c
      lsvmin = lwa + lwb
c
c     set remaining harmonic pointer
c
      ibb = iab+lb1*nlatb
c
c     set pointers for remaining work
c
      iw = ibb+lb1*nlatb
c
c     set remaining work space length in lw
c
      lw = lwork - iw
      lwk3 = nlata*nlona*2
      lwk4 = nlatb*nlonb*2
c
c     set minimum unsaved work space required by trssph
c
      lwkmin = iw + max0(lwk3,lwk4)
c
c     set error flags if saved or unsaved work spaces are insufficient
c
      ier = 10
      if (lsave .lt. lsvmin) return
      ier = 11
      if (lwork .lt. lwkmin) return
      ier = 13
      nlat = max0(nlata,nlatb)
      if (ldwork .lt. nlat*(nlat+4)) return
      ier = 0
      if (igrda .eq. 1) then
c
c     initialize wsave for equally spaced analysis
c
      call shaeci(nlata,nlona,wsave,lwa,dwork,ldwork,ier)
      else
c
c     initialize wsave for gaussian analysis
c
      call shagci(nlata,nlona,wsave,lwa,dwork,ldwork,ier)
      if (ier.ne.0) then
c
c     flag failure in spherepack gaussian software
c
      ier = 12
      return
      end if
      end if

      if (igrdb .eq. 2) then
c
c     initialize wsave for gaussian synthesis
c
      call shsgci(nlatb,nlonb,wsave(jb),lwb,dwork,ldwork,ier)
      if (ier.ne.0) then
c
c     flag failure in spherepack gaussian software
c
      ier = 12
      return
      end if
      else
c
c     initialize wsave for equally spaced synthesis
c
      call shseci(nlatb,nlonb,wsave(jb),lwb,dwork,ldwork,ier)
      end if
c
c     end of initialization (intl=0) call
c
      end if
c
c     transpose and/or reorder (co)latitude if necessary for da
c     (arrays must have latitude (colatitude) as the first dimension
c     and run north to south for spherepack software)
c
      if (igrida(2) .eq. 0) call trsplat(nlona,nlata,da,work)
      if (igrida(1) .gt. 0) call convlat(nlata,nlona,da)

      nt = 1
      isym = 0
      if (igrda .eq. 2) then
c
c     do spherical harmonic analysis of "adjusted" da on gaussian grid
c
      call shagc(nlata,nlona,isym,nt,da,nlata,nlona,work(iaa),
     +work(iba),la1,nlata,wsave,lwa,work(iw),lw,ier)
      else
c
c     do spherical harmonic analysis of "adjusted" da on equally spaced grid
c
      call shaec(nlata,nlona,isym,nt,da,nlata,nlona,work(iaa),
     +work(iba),la1,nlata,wsave,lwa,work(iw),lw,ier)
      end if
c
c     transfer da grid coefficients to db grid coefficients
c     truncating to zero as necessary
c
      call trab(la1,nlata,work(iaa),work(iba),lb1,nlatb,work(iab),
     +          work(ibb))

      if (igrdb .eq. 1) then
c
c     do spherical harmonic synthesis on nlatb by nlonb equally spaced grid
c
      call shsec(nlatb,nlonb,isym,nt,db,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave(jb),lwb,work(iw),lw,ier)
      else
c
c     do spherical harmonic synthesis on nlatb by nlonb gaussian grid
c
      call shsgc(nlatb,nlonb,isym,nt,db,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave(jb),lwb,work(iw),lw,ier)
      end if
c
c     both da,db are currently latitude by longitude north to south arrays
c     restore da and set db to agree with flags in igrida and igridb
c
      if (igrida(1) .gt. 0) call convlat(nlata,nlona,da)
      if (igridb(1) .gt. 0) call convlat(nlatb,nlonb,db)
      if (igrida(2) .eq. 0) call trsplat(nlata,nlona,da,work)
      if (igridb(2) .eq. 0) call trsplat(nlatb,nlonb,db,work)
      return
      end

      subroutine trab(ma,na,aa,ba,mb,nb,ab,bb)
      implicit none
      integer ma,na,mb,nb,i,j,m,n
      real aa(ma,na),ba(ma,na),ab(mb,nb),bb(mb,nb)
c
c     set coefficients for b grid from coefficients for a grid
c
      m = min0(ma,mb)
      n = min0(na,nb)
      do j=1,n
	do i=1,m
	  ab(i,j) = aa(i,j)
	  bb(i,j) = ba(i,j)
	end do
      end do
c
c     set coefs outside triangle to zero
c
      do i=m+1,mb
	do j=1,nb
	  ab(i,j) = 0.0
	  bb(i,j) = 0.0
	end do
      end do
      do j=n+1,nb
	do i=1,mb
	  ab(i,j) = 0.0
	  bb(i,j) = 0.0
	end do
      end do
      return
      end

      subroutine trsplat(n,m,data,work)
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

      subroutine convlat(nlat,nlon,data)
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
