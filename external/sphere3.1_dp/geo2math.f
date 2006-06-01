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
c  .                          SPHEREPACK                         .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... geo2math.f
c
c     file geo2math.f contains subroutines for converting scalar and
c     vector fields between geophysical and mathematical spherical
c     coordinates.  The latter is required when using most spherepack
c     software.  The four main subroutines in geo2math.f are described
c     as follows:
c
c     (1) subroutine geo2maths(ig,nlon,nlat,sg,sm,work)
c
c         converts the nlon by nlat scalar field sg given in
c         geophysical coordinates to the nlat by nlon scalar
c         field sm given in mathematical coordinates. sg and sm
c         can be identical in the program calling geo2maths.
c
c     (2) subroutine math2geos(ig,nlat,nlon,sm,sg,work)
c
c         converts the nlat by nlon scalar field sm given in
c         mathematical coordinates to the nlon by nlat scalar
c         field sg given in geophysical coordinates.  sm and
c         sg can be identical in the program calling math2geos.
c
c     (3) subroutine geo2mathv(ig,nlon,nlat,ug,vg,vm,wm,work)
c
c         converts the nlon by nlat vector field (ug,vg) given
c         in geophysical coordinates to the nlat by nlon vector
c         field (vm,wm) in mathematical coordinates.  ug and wm
c         can be identical in the program calling geo2mathv.  vg
c         and vm can be identical in the program calling geo2mathv.
c
c     (4) subroutine math2geov(ig,nlat,nlon,vm,wm,ug,vg,work)
c
c         converts the nlat by nlon vector field (vm,wm) given
c         in mathematical coordinates to the nlon by nlat vector
c         field (ug,vg) in spherical coordinates.  vm and vg can
c         be identical in the program calling math2geov.  wm and
c         ug can be identical in the program calling math2geov.
c
c *** (1),(2),(3),(4) argument description.
c
c ... ig
c
c     = 0 if the latitude values in the geophysical arrays sg,ug,vg are
c         ordered south to north with increasing latitude subscript
c         i=1,2,...,nlat.
c
c     = 1 if the latitude values in the geophysical arrays sg,ug,vg are
c         ordered north to south with increasing latitude subscript
c         i=1,2,...,nlat.
c
c ... nlon
c
c     the number of distinct londitude points.  nlon determines
c     the grid increment in longitude as 2*pi/nlon. nlon is the first
c     dimension of the geophysical arrays sg,ug,vg and the second
c     dimension of the mathematical arrays sm,vm,wm.  The longitude
c     grid is given by phi(j) = (j-1)*2*pi/nlon j=1,...,nlon.
c
c ... nlat
c
c     the number of distinct latitude and colatitude points and the
c     first dimension of sm,vm,wm and second dimension of sg,ug,vg.
c     If the (co)latitude grid is equally spaced then the grid increment
C*PL*ERROR* Comment line too long
c     is dlat=pi/(nlat-1).  In this case the south to north latitude grid
c     underlying is
c
c        lat(i) = -0.5*pi + (i-1)*dlat (i=1,...,nlat)
c
c     and the north to south colatitude grid underlying sm,vm,wm is
c
c        colat(i) = (i-1)*dlat  (i=1,...,nlat)
c
C*PL*ERROR* Comment line too long
c     If the grid is Gaussian let thetag(i) be the north to south colatitude
c     grid (as computed by the spherepack routine gaqd).  In this case
c
c        colat(i) = thetag(i) (i=1,...,nlat)
c
c     and
c
c        lat(i) = -0.5*pi + thetag(i) (i=1,...,nlat)
c
c     In either case lat(i) = colat(nlat-i+1) for all i.
c     If nlat is odd the equator is located at the (nlat+1)/2
c     latitude or colatitude point.  If nlat is even the equator is
c     half way between the nlat/2 and nlat/2+1 latitude or colatitude
c     points.  The equally spaced (co)latitude grid includes the poles.
c     The Gaussian grid excludes the poles.
c
c ... sg,sm
c
c     In (1),(2) sg is a nlon by nlat array containing the scalar field
C*PL*ERROR* Comment line too long
c     in geophysical coordinates.  Latitude values in sg are ordered from
c     the southern to the northern hemisphere with increasing latitude
c     subscript if ig = 0 or ordered from the northern hemisphere to the
C*PL*ERROR* Comment line too long
c     southern hemisphere if ig = 1.  sm is a nlat by nlon array containing
C*PL*ERROR* Comment line too long
c     the scalar field in mathematical coordinates.  Colatitude values in sm
C*PL*ERROR* Comment line too long
c     are ordered from the north to the south hemisphere  with increasing
C*PL*ERROR* Comment line too long
c     colatitude subscript (i=1,...,nlat).  The (co)latitude grid for sg and
C*PL*ERROR* Comment line too long
c     sm can be equally spaced or Gaussian.  sg and sm can be equivalenced or
C*PL*ERROR* Comment line too long
c     be identical in the routine calling geo2maths or math2geos.  sg and
c     sm are related by
c
c          sm(nlat-i+1,j) = sg(j,i)    (if ig = 0)
c
c     or
c
c          sm(i,j) = sg(j,i)            (if ig = 1)
c
C*PL*ERROR* Comment line too long
c     for i=1,...,nlat and j=1,...,nlon. This formula is not used because
c     the two arrays can be equivalenced or identical arguments in the
c     program calling geo2maths or math2geos.
c
c ... ug,vg,vm,wm
c
c     In (3),(4) ug is a nlon by nlat array containing the longitudinal
c     vector component.  vg is a nlon by nlat array containing the
c     latitudinal vector component.  Values in (ug,vg) are ordered
c     from the southern to the northern hemisphere with increasing
c     latitude subscript if ig = 0 or from the northern to southern
c     hemisphere if ig = 1.  vm is a nlat by nlon array containing the
c     the colatitudinal vector component.  wm is a nlat by nlon array
c     containing the east longitudinal vector component.  Values in
c     (vm,wm) are ordered from the northern to the southern hemisphere
c     with increasing colatitude subscript.  The (co)latitude grid for
c     both vector fields can be equally spaced or Gaussian.  ug,wm and
c     vg,vm can be equivalenced or be identical in the program calling
c     geo2mathv or math2geov.  They are related by
c
c          ug(j,nlat-i+1) =  wm(i,j)
c                                      (ig = 0)
c          vg(j,nlat-i+1) = -vm(i,j)
c
c     or
c
c          ug(j,i) =  wm(i,j)
c                                      (ig = 1)
c          vg(j,i) = -vm(i,j)
c
c
c     for i=1,...,nlat and j=1,...,nlon.  These formulas are not
c     used because ug,wm and vg,vm can be equivalenced or identical
c     arguments in the program calling math2geov or geo2mathv.
c
c     Let ib = nlat-i+1 for i=1,...,nlat.  Summarizing:
c     sg(j,i) or ug(j,i),vg(j,i) are values at (phi(j),lat(i)) if ig = 0
C*PL*ERROR* Comment line too long
c     sg(j,i) or ug(j,i),vg(j,i) are values at (phi(j),lat(ib)) if ig = 1
c     sm(i,j) or vm(i,j),wm(i,j) are values at (colat(i),phi(j))
c
c ... work is an unsaved real work space of length at least nlon*nlat
C*PL*ERROR* Comment line too long
c     in the routine calling (1),(2),(3), or (4).  It is used to simplify
c     a nonsquare array transposition in case it is required.
c
c *** example (1)
c
C*PL*ERROR* Comment line too long
c     suppose you wish to compute the divergence of (ug,vg) on a Gaussian
C*PL*ERROR* Comment line too long
c     grid in geophysical coordinates using the stored Legendre polynomial
c     routines on SPHEREPACK 2.0.
c
c     (1) call geo2mathv to set vm,wm from ug,vg
c
C*PL*ERROR* Comment line too long
c     (2) call vhags to compute the vector harmonic coefficients of vm,wm
c
C*PL*ERROR* Comment line too long
c     (3) call divgs with the coefficients from (2) to compute the divergence
c         dv in mathematical spherical coordinates on the UNIT sphere.
c
c     (4) call math2geos to convert the scalar divergence dv back to
c         geophysical spherical coordinates.
c
c     (5) divide dv by R (the radius of the earth) to compute divergence
c         on the earth (scaling from unit sphere computation in (3)).
c
c *** example (2)
c
c     suppose you wish to compute a vector field (ug,vg) corresponding
c     to  a given divergence dvg and vorticity vtg (all in geophysical
c     coordinates) on an equally spaced (co)latitude grid using the
c     computed Legendre polynomial software.
c
c     (1) call geo2maths to set dvm from dvg
c
c     (2) call geo2maths to set vtm from vts
c
c     (3) call shaec to compute the scalar harmonic coefficients of dvm
c
c     (4) call shaec to compute the scalar harmonic coefficients of vtm
c
C*PL*ERROR* Comment line too long
c     (5) call idvtec to compute (vm,wm) using the coefficients from (3),(4).
c
c     (6) call math2geov to set (ug,vg) from (vm,wm)
c
c     (7) multiply (ug,vg) by the earth's radius R for scaling
c         from the unit sphere computation in (5)
c
c *** END OF DOCUMENTATION ... CODE FOLLOWS:
c
c
      SUBROUTINE DGEO2MATHS(IG,NLON,NLAT,SG,SM,WORK)
      IMPLICIT NONE
      INTEGER IG,NLON,NLAT,I,J,IJ
      DOUBLE PRECISION SG(NLON,NLAT),SM(NLAT,NLON),WORK(*)
c
c     transpose sg into sm and reverse colatitude subscript order
c     if necessary
c
      DO I = 1,NLAT
          DO J = 1,NLON
              IJ = (J-1)*NLAT + I
              WORK(IJ) = SG(J,I)
          END DO
      END DO
      IF (IG.EQ.0) THEN
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  SM(NLAT-I+1,J) = WORK(IJ)
              END DO
          END DO
      ELSE
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  SM(I,J) = WORK(IJ)
              END DO
          END DO
      END IF
      RETURN
      END

      SUBROUTINE DMATH2GEOS(IG,NLAT,NLON,SM,SG,WORK)
      IMPLICIT NONE
      INTEGER IG,NLON,NLAT,I,J,IJ
      DOUBLE PRECISION SM(NLAT,NLON),SG(NLON,NLAT),WORK(*)
c
c     transpose sm into sg and reverse colatitude subscript order
c     if necessary
c
      DO I = 1,NLAT
          DO J = 1,NLON
              IJ = (J-1)*NLAT + I
              WORK(IJ) = SM(I,J)
          END DO
      END DO
      IF (IG.EQ.0) THEN
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  SG(J,NLAT-I+1) = WORK(IJ)
              END DO
          END DO
      ELSE
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  SG(J,I) = WORK(IJ)
              END DO
          END DO
      END IF
      RETURN
      END

      SUBROUTINE DGEO2MATHV(IG,NLON,NLAT,UG,VG,VM,WM,WORK)
      IMPLICIT NONE
      INTEGER IG,NLON,NLAT,I,J,IJ
      DOUBLE PRECISION UG(NLON,NLAT),VG(NLON,NLAT),WORK(*)
      DOUBLE PRECISION VM(NLAT,NLON),WM(NLAT,NLON)
c
c     convert vg to vm, ug to wm
c
      IF (IG.EQ.0) THEN
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = VG(J,I)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  VM(NLAT-I+1,J) = -WORK(IJ)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = UG(J,I)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WM(NLAT-I+1,J) = WORK(IJ)
              END DO
          END DO
      ELSE
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = VG(J,I)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  VM(I,J) = -WORK(IJ)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = UG(J,I)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WM(I,J) = WORK(IJ)
              END DO
          END DO
      END IF
      RETURN
      END

      SUBROUTINE DMATH2GEOV(IG,NLAT,NLON,VM,WM,UG,VG,WORK)
      IMPLICIT NONE
      INTEGER IG,NLON,NLAT,I,J,IJ
      DOUBLE PRECISION VM(NLAT,NLON),WM(NLAT,NLON),WORK(*)
      DOUBLE PRECISION UG(NLON,NLAT),VG(NLON,NLAT)
c
c     convert vm to vg, wm to ug
c
      IF (IG.EQ.0) THEN
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = VM(I,J)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  VG(J,NLAT-I+1) = -WORK(IJ)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = WM(I,J)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  UG(J,NLAT-I+1) = WORK(IJ)
              END DO
          END DO
      ELSE
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = VM(I,J)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  VG(J,I) = -WORK(IJ)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  WORK(IJ) = WM(I,J)
              END DO
          END DO
          DO I = 1,NLAT
              DO J = 1,NLON
                  IJ = (J-1)*NLAT + I
                  UG(J,I) = WORK(IJ)
              END DO
          END DO
      END IF
      RETURN
      END
