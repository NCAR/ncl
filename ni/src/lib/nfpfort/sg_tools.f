      INTEGER FUNCTION GCCONVEX (NPTS, QLAT, QLON)
C
C  This function determines if the spherical polygon
C  (QLAT(I),QLON(I)),I=1,NPTS) is convex.  One is returned
C  if so, zero otherwise.  The input polygon must have at
C  least three points and be closed.
C
      DOUBLE PRECISION GCDANGLE,QLAT(NPTS),QLON(NPTS)
C
C  Find the initial direction.
C
      IDIR = SIGN(1.D0,GCDANGLE(QLAT(1),QLON(1),QLAT(2),QLON(2),
     +            QLAT(3),QLON(3)))
      DO 10 I=2,NPTS
        ISGN = SIGN(1.D0,GCDANGLE(QLAT(I),QLON(I),
     +                       QLAT(MOD(I+1,NPTS)),QLON(MOD(I+1,NPTS)),
     +                       QLAT(MOD(I+2,NPTS)),QLON(MOD(I+2,NPTS))))
        IF (ISGN .NE. IDIR) THEN
          GCCONVEX = 0
          RETURN
        ENDIF
   10 CONTINUE
      GCCONVEX = 1
      RETURN
C
      END
      INTEGER FUNCTION GCINOUT (PLAT, PLON, QLAT, QLON, NPTS, WORK)
C
C  This function determines if the coordinate (PLAT,PLON) is
C  inside or outside of the spherical patch
C  (QLAT(I),QLON(I)),I=1,NPTS).  Zero is returned if
C  in, non-zero otherwise.  WORK is a work array
C  dimensioned for 4*NPTS.  A point on the boundary is
C  considered to be inside.
C
      DOUBLE PRECISION PLAT, PLON, QLAT(NPTS), QLON(NPTS), WORK(4,NPTS)
      DOUBLE PRECISION P(4), D2R
      DATA D2R/0.017453292519943D0/
      INTEGER GCONARC
C

C
C  Check that the input polygon is closed.
C
      IF (QLAT(NPTS).NE.QLAT(1) .OR. QLON(NPTS).NE.QLON(1)) THEN
        WRITE(6,500) 
  500   FORMAT('GCINOUT: The input spherical polygon must be closed.')

        GCINOUT = -1
        RETURN
      ENDIF
C
      DO 10 I=1,NPTS
        WORK(1,I) = DCOS(D2R*QLAT(I))
        WORK(2,I) = DSIN(D2R*QLAT(I))
        WORK(3,I) = DCOS(D2R*QLON(I))
        WORK(4,I) = DSIN(D2R*QLON(I))
   10 CONTINUE
      P(1) = DCOS(D2R*PLAT)
      P(2) = DSIN(D2R*PLAT)
      P(3) = DCOS(D2R*PLON)
      P(4) = DSIN(D2R*PLON)

C
C  Return if inside (may still be on boundary).
C
C       
      GCINOUT = ICAPDP(P,WORK,NPTS)
      IF (GCINOUT .EQ. 0) THEN
        RETURN
      ENDIF
C
C  Check if on boundary.
C
      DO 20 I=1,NPTS-1
        IF (GCONARC(PLAT,PLON,QLAT(I),QLON(I),QLAT(I+1),QLON(I+1),
     +               1.D-10) .EQ. 1) THEN
          GCINOUT = 0
          RETURN
        ENDIF
   20 CONTINUE
C     
C  Default return flags outside.
C
      GCINOUT = 1
      RETURN
      END
      DOUBLE PRECISION FUNCTION GCDIST(LAT1,LON1,LAT2,LON2)
C
C  Computes the shortest (undirected) angular distance between the points
C  on the globe (LAT1,LON1) and (LAT2,LON2).
C
      DOUBLE PRECISION AT(4), BT(4), D2R, XADGCDP
      DOUBLE PRECISION LAT1,LON1,LAT2,LON2
      DATA D2R/0.017453292519943D0/
C
      AT(1) = COS(D2R*LAT1)
      AT(2) = SIN(D2R*LAT1)
      AT(3) = COS(D2R*LON1)
      AT(4) = SIN(D2R*LON1)
C
      BT(1) = COS(D2R*LAT2)
      BT(2) = SIN(D2R*LAT2)
      BT(3) = COS(D2R*LON2)
      BT(4) = SIN(D2R*LON2)
C
      GCDIST = XADGCDP(AT,BT)
C
      RETURN
      END
      INTEGER FUNCTION GCONARC(PLAT,PLON,LAT1,LON1,LAT2,LON2,TOL)
C 
C  Determines whether (PLAT,PLON) lies on the arc of the
C  great circle formed by (LAT1,LON1) and (LAT2,LON2) within 
C  a tolerance of TOL.  The arc is taken to be the shorter of 
C  the two possible arcs between (LAT1,LON1) and (LAT2,LON2).
C  Return 1 if on arc, 0 otherwise.
C
      DOUBLE PRECISION PLAT,PLON,LAT1,LON1,LAT2,LON2,TOL
      DOUBLE PRECISION ADIST,BDIST,CDIST,GCPNT2GC,GCDIST
      ADIST = GCPNT2GC(LAT1,LON1,LAT2,LON2,PLAT,PLON)
      IF (ABS(ADIST) .LE. TOL) THEN
        BDIST = ABS(GCDIST(PLAT,PLON,LAT1,LON1)) + 
     +          ABS(GCDIST(PLAT,PLON,LAT2,LON2))
        CDIST = ABS(GCDIST(LAT1,LON1,LAT2,LON2))
        IF (ABS(BDIST - CDIST) .LE. TOL) THEN
          GCONARC = 1
        ELSE
          GCONARC = 0
        ENDIF
      ELSE
        GCONARC = 0
      ENDIF
      RETURN 
      END
      DOUBLE PRECISION FUNCTION GCCONVERT(ANGLE,TYPE)
C
C  Converts an angle (in degrees) along a great circle on the earth
C  as per:
C
C  TYPE -- A string indicating the units you want to convert
C          to. Legal values are:
C
C          "radians"
C          "meters" 
C          "kilometers"
C          "feet"     
C          "miles"   
C
      DOUBLE PRECISION ANGLE
      CHARACTER*(*) TYPE
      DOUBLE PRECISION D2R
      DATA D2R/0.017453292519943D0/
C
C  Since the radius of the earth is not a precise number, the
C  following conversion ratios need not be double precision.
C
      DOUBLE PRECISION R2M, M2F
      DATA R2M,M2F/6371220.,3.2808/
C
      IF (TYPE(1:2).EQ.'ra' .OR. TYPE(1:2).EQ.'RA') THEN
        GCCONVERT = D2R*ANGLE
      ELSE IF (TYPE(1:2).EQ.'me' .OR. TYPE(1:2).EQ.'ME') THEN
        GCCONVERT = D2R*ANGLE*R2M
      ELSE IF (TYPE(1:2).EQ.'ki' .OR. TYPE(1:2).EQ.'KI') THEN
        GCCONVERT = D2R*ANGLE*R2M/1000.
      ELSE IF (TYPE(1:2).EQ.'fe' .OR. TYPE(1:2).EQ.'FE') THEN
        GCCONVERT = D2R*ANGLE*R2M*M2F
      ELSE IF (TYPE(1:2).EQ.'mi' .OR. TYPE(1:2).EQ.'MI') THEN
        GCCONVERT = D2R*ANGLE*R2M*M2F/5280.
      ELSE
        PRINT *,'GCCONVERT: unrecognized conversion type'
      ENDIF
C
      RETURN
      END
      SUBROUTINE GCINTERP(LAT1, LON1, LAT2, LON2, NPTS, OLAT, OLON)
C
C  Interpolates NPTS points along a great circle from (LAT1,LON1) to
C  (LAT2,LON2) and returns them in the OLAT and OLON arrays.  The 
C  beginning and ending points are included in the result, so, for
C  example, if NPTS=2, then only the end points are returned.  
C 
      DOUBLE PRECISION LAT1, LON1, LAT2, LON2, OLAT(NPTS), OLON(NPTS)
      DOUBLE PRECISION NLAT1, NLON1, NLAT2, NLON2, NORMANG
      DOUBLE PRECISION D2R,R2D
      DATA D2R,R2D/0.017453292519943D0,57.2957795130823D0/
C
      NLAT1 = R2D*NORMANG(D2R*LAT1)
      NLON1 = R2D*NORMANG(D2R*LON1)
      NLAT2 = R2D*NORMANG(D2R*LAT2)
      NLON2 = R2D*NORMANG(D2R*LON2)
C     
      OLAT(1)    = NLAT1
      OLON(1)    = NLON1
      OLAT(NPTS) = NLAT2
      OLON(NPTS) = NLON2
      IF (NPTS .EQ. 2) THEN
        RETURN
      ENDIF
      CALL DMAPGCI (LAT1,LON1,LAT2,LON2,NPTS-2,OLAT(2),OLON(2))
      RETURN
      END
      DOUBLE PRECISION FUNCTION GCQAREA(LAT1, LON1, LAT2, LON2,
     +                                  LAT3, LON3, LAT4, LON4,
     +                                  IER)
C
C  Finds the area of a convex quadrilateral patch on a sphere whose 
C  vertices are given in degrees as lat/lon pairs.
C
      DOUBLE PRECISION GCTAREA
      DOUBLE PRECISION LAT1, LON1, LAT2, LON2, LAT3, LON3, LAT4, LON4
      DOUBLE PRECISION GC1, GC2
      INTEGER IER, IER1, IER2
C
      GC1 = ABS(GCTAREA(LAT1, LON1, LAT2, LON2, LAT3, LON3, IER1))
      GC2 = ABS(GCTAREA(LAT1, LON1, LAT3, LON3, LAT4, LON4, IER2))
      IF(IER1.EQ.0.and.IER2.EQ.0) THEN
        GCQAREA = GC1 + GC2
        IER = 0
      ELSE
        GCQAREA = 1.D30
        IER = 1
      END IF
C     
      RETURN
      END      
      DOUBLE PRECISION FUNCTION GCTAREA(LAT1, LON1, LAT2, LON2,
     +                                  LAT3, LON3, IER)
C
C  Finds the area of a spherical triangle by finding the three
C  spherical angles and subtracting off PI.
C
      DOUBLE PRECISION LAT1, LON1, LAT2, LON2, LAT3, LON3
      DOUBLE PRECISION GCDANGLE,ALPHA,BETA,GAMMA,PI
      DOUBLE PRECISION D2R
      INTEGER IER
      DATA D2R/0.017453292519943D0/
C
      PI = 2.D0*DASIN(1.D0)
C
      IF (.NOT. ( ((LAT1.EQ.LAT2).AND.(LON1.EQ.LON2)) .OR.
     +            ((LAT1.EQ.LAT3).AND.(LON1.EQ.LON3)) .OR.
     +            ((LAT2.EQ.LAT3).AND.(LON2.EQ.LON3)) )) THEN
        ALPHA  = ABS(D2R*GCDANGLE(LAT1, LON1, LAT2, LON2, LAT3, LON3))
        BETA   = ABS(D2R*GCDANGLE(LAT2, LON2, LAT3, LON3, LAT1, LON1))
        GAMMA  = ABS(D2R*GCDANGLE(LAT3, LON3, LAT1, LON1, LAT2, LON2))
        GCTAREA = ALPHA+BETA+GAMMA - PI
        IER = 0
      ELSE
        WRITE(6,500) 
  500   FORMAT('GCTAREA: The three input points must be distinct.')   
        GCTAREA = 1.D30
        IER = 1
      ENDIF
C
      RETURN
      END      
      DOUBLE PRECISION FUNCTION GCPNT2GC(LAT1, LON1, LAT2, LON2,
     +                                    LAT3, LON3)
C
C  Distance from a point (LAT3,LON3) to a great circle defined by the
C  arc from (LAT1,LON1) to (LAT2,LON2).
C
      DOUBLE PRECISION LAT1, LON1, LAT2, LON2, LAT3, LON3
      DOUBLE PRECISION AT(4), BT(4), CT(4), D2R, XDPGCDP
      DATA D2R/0.017453292519943D0/
C
      AT(1) = COS(D2R*LAT1)
      AT(2) = SIN(D2R*LAT1)
      AT(3) = COS(D2R*LON1)
      AT(4) = SIN(D2R*LON1)
C
      BT(1) = COS(D2R*LAT2)
      BT(2) = SIN(D2R*LAT2)
      BT(3) = COS(D2R*LON2)
      BT(4) = SIN(D2R*LON2)
C
      CT(1) = COS(D2R*LAT3)
      CT(2) = SIN(D2R*LAT3)
      CT(3) = COS(D2R*LON3)
      CT(4) = SIN(D2R*LON3)
C
      GCPNT2GC = XDPGCDP (AT,BT,CT)
C 
      RETURN
      END 
      DOUBLE PRECISION FUNCTION XDPGCDP (AQDP,BQDP,CQDP)
C
C (XDPGCDP = Distance of Point from Great Circle, Double Precision)
C 
C Author: David Kennison
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4)
C
C This function, given points A, B, and C on the globe, returns the
C directed distance, in degrees of arc, from the great circle through
C A and B to the point C, positive if the point C is in the hemisphere
C to the "left" of the great circle and negative otherwise.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION RTOD
        DOUBLE PRECISION XCB2,YCB2,ZCB2
        DOUBLE PRECISION      YCB3,ZCB3
        DOUBLE PRECISION XCC2,YCC2,ZCC2
        DOUBLE PRECISION      YCC3,ZCC3
        DOUBLE PRECISION DNOM
C
        DOUBLE PRECISION XADGCDP
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823D0 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
        XCB2=BQDP(1)*BQDP(3)*AQDP(3)+BQDP(1)*BQDP(4)*AQDP(4)
        YCB2=BQDP(1)*BQDP(4)*AQDP(3)-BQDP(1)*BQDP(3)*AQDP(4)
        ZCB2=BQDP(2)
C
        YCB3=YCB2
        ZCB3=ZCB2*AQDP(1)-XCB2*AQDP(2)
C
C Do the same for C.
C
        XCC2=CQDP(1)*CQDP(3)*AQDP(3)+CQDP(1)*CQDP(4)*AQDP(4)
        YCC2=CQDP(1)*CQDP(4)*AQDP(3)-CQDP(1)*CQDP(3)*AQDP(4)
        ZCC2=CQDP(2)
C
        YCC3=YCC2
        ZCC3=ZCC2*AQDP(1)-XCC2*AQDP(2)
C
C Rotate C about the X axis by the angle ALPH required to put B on the
C equator.
C
C The following code returns the distance from A to C (in the degenerate 
C case when A and B are the same point as well).
C
        IF (YCB3.NE.0.D0 .OR. ZCB3.NE.0.D0) THEN
          DNOM   = SQRT(YCB3*YCB3+ZCB3*ZCB3)
          XDPGCDP = RTOD*ASIN((ZCC3*YCB3-YCC3*ZCB3)/DNOM)
        ELSE
          XDPGCDP = XADGCDP(AQDP,CQDP)
        END IF
C
        RETURN
      END
      DOUBLE PRECISION FUNCTION XADGCDP (AQDP,BQDP)
C
C (XADGCDP = Angle in Degrees along Great Circle, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4)
C
C This function returns the shortest great circle distance, in degrees,
C between two points, A and B, on the surface of the globe.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION RDTT
C
C Define a multiplicative constant to convert from radians to degrees
C and multiply by two.
C
        DATA RDTT / 114.5915590261646D0 /
C
        XADGCDP=RDTT*
     +         ASIN(SQRT((AQDP(1)*AQDP(3)-BQDP(1)*BQDP(3))**2+
     +                   (AQDP(1)*AQDP(4)-BQDP(1)*BQDP(4))**2+
     +                   (AQDP(2)        -BQDP(2)        )**2)/2.D0)
C
        RETURN
C
      END
      SUBROUTINE LATLON2CART(LAT,LON,X,Y,Z)
C
C  Converts a (lat,lon) coordinate to Cartesian coordinates on the
C  unit sphere.
C
      DOUBLE PRECISION LAT,LON,X,Y,Z,RLAT,RLON,NORMANG,PI,D2R
      DATA D2R/0.017453292519943D0/
C
      IF (LAT .GT. 90.D0 .OR. LAT .LT. -90.D0) THEN
        WRITE (6,500) LAT
  500   FORMAT('LATLON2CART: Latitude value out of range',F20.15)
        X = 1.D20
        Y = 1.D20
        Z = 1.D20
        RETURN
      ENDIF
C
      RLON = NORMANG(D2R*LON)
      RLAT = D2R*(90.D0 - LAT)
      PI = 2.D0*DASIN(1.D0)
      X = DSIN(RLAT)*DCOS(RLON)
      Y = DSIN(RLAT)*DSIN(RLON)
      Z = DCOS(RLAT)
C      
      RETURN
      END
      DOUBLE PRECISION FUNCTION NORMANG(ANGLE)
C
C  Adjusts the input ANGLE (in radians) by adding or subtracting 
C  enough multiples of 2*PI so that the result lies in the range
C  [-PI,PI)
C
      DOUBLE PRECISION ANGLE, PI, RVAL
      INTEGER I
C
      PI = 2.D0*DASIN(1.D0)
C
      RVAL = ANGLE 
      DO 10 I=1,50
        IF (RVAL .LT. -PI) THEN
          RVAL = RVAL+2.D0*PI
        ELSE
          GO TO 30
        ENDIF
   10 CONTINUE
   30 CONTINUE
      DO 20 I=1,50
        IF (RVAL .GE. PI) THEN
          RVAL = RVAL-2.D0*PI
        ELSE
          GO TO 40
        ENDIF
   20 CONTINUE
   40 CONTINUE
C
      NORMANG = RVAL
      RETURN
      END
      DOUBLE PRECISION FUNCTION GCAANGLE(LAT1, LON1, LAT2, LON2,
     +                                   LAT3, LON3, LAT4, LON4)
C
C  Find the (acute) angle between two great circles given two arcs
C  defining the circles - one circle containing the arc between
C  (lat1, lon1) and (lat2,lon2) and the other containing the arc
C  between (lat3, lon3) and (lat4,lon4).
C
      DOUBLE PRECISION LAT1,LON1,LAT2,LON2,LAT3,LON3,LAT4,LON4
      DOUBLE PRECISION PTS1(3,3),PTS2(3,3),ANGPLANES
C
C  Get Cartesian coordinates for all four points.
C
      CALL LATLON2CART(LAT1,LON1,PTS1(1,1),PTS1(1,2),PTS1(1,3))
      CALL LATLON2CART(LAT2,LON2,PTS1(2,1),PTS1(2,2),PTS1(2,3))
      PTS1(3,1) = 0.D0
      PTS1(3,2) = 0.D0
      PTS1(3,3) = 0.D0
C
      CALL LATLON2CART(LAT3,LON3,PTS2(1,1),PTS2(1,2),PTS2(1,3))
      CALL LATLON2CART(LAT4,LON4,PTS2(2,1),PTS2(2,2),PTS2(2,3))
      PTS2(3,1) = 0.D0
      PTS2(3,2) = 0.D0
      PTS2(3,3) = 0.D0
C
      GCAANGLE = ANGPLANES (PTS1, PTS2)
C          
      RETURN
      END
      DOUBLE PRECISION FUNCTION GCDANGLE(LAT1, LON1, LAT2, LON2,
     +                                    LAT3, LON3)
C
C  Find the directed angle in degrees between two great circles that 
C  intersect in a given point.  Given three points on the globe 
C  A = (LAT1,LON1), B = (LAT2,LON2), and C = (LAT3,LON3) this function 
C  finds the angle between the great circles that contain the arcs
C  AB and AC.  The returned angle is positive if C is in the hemisphere 
C  to the "left" of the great circle through A and B, negative otherwise.
C  
      DOUBLE PRECISION D2R,XABGCDP,LAT1,LON1,LAT2,LON2,LAT3,LON3
      PARAMETER (D2R=0.017453292519943D0)
C
      DOUBLE PRECISION PT1(4),PT2(4),PT3(4)
C        
      PT1(1) = DCOS(D2R*LAT1)
      PT1(2) = DSIN(D2R*LAT1)
      PT1(3) = DCOS(D2R*LON1)
      PT1(4) = DSIN(D2R*LON1)
C
      PT2(1) = DCOS(D2R*LAT2)
      PT2(2) = DSIN(D2R*LAT2)
      PT2(3) = DCOS(D2R*LON2)
      PT2(4) = DSIN(D2R*LON2)
C
      PT3(1) = DCOS(D2R*LAT3)
      PT3(2) = DSIN(D2R*LAT3)
      PT3(3) = DCOS(D2R*LON3)
      PT3(4) = DSIN(D2R*LON3)
C
      GCDANGLE = XABGCDP(PT1,PT2,PT3)
C          
      RETURN
      END
      DOUBLE PRECISION FUNCTION ANGPLANES (PTS1, PTS2)
C
C  Finds the angle between two planes, one plane determined
C  by the three points in three space by PTS1 and the second 
C  by the three points in in PTS2.
C
      DOUBLE PRECISION PTS1(3,3), PTS2(3,3), CF1(4), CF2(4)
      DOUBLE PRECISION DOTP,PNORM,THETA,TMP
C
      DOUBLE PRECISION R2D
      DATA R2D / 57.2957795130823D0 /
C
C  Get the coefficients for the equations of the planes.
C
      CALL EQNPLANE(PTS1,CF1)
      CALL EQNPLANE(PTS2,CF2)
C
C  Calculate the cosine of the angle using:
C
C     COS(THETA) = (norm_vct(pts1) dot norm_vct(pts2))/
C                   (norm(pts1) * norm(pts2))
C
      DOTP = 0.D0
      DO 10 I=1,3
        DOTP = DOTP + CF1(I)*CF2(I)
   10 CONTINUE
      PNORM = (DSQRT(CF1(1)*CF1(1) + CF1(2)*CF1(2) + CF1(3)*CF1(3))) *
     +        (DSQRT(CF2(1)*CF2(1) + CF2(2)*CF2(2) + CF2(3)*CF2(3)))
      IF (PNORM .EQ. 0.D0) THEN
        WRITE(6,500)
  500   FORMAT('EQPLANE: Input points must be distinct.')
      ENDIF
      THETA = DOTP/PNORM
      TMP = R2D*DACOS(THETA)
      IF (TMP .GT. 90.D0) TMP = 180.D0-TMP
      ANGPLANES = TMP
C
      RETURN
      END
      SUBROUTINE EQNPLANE(PNT, COEF)
C
C  Finds the coefficients of the equation of a plane:
C
C    A*X + B*Y + C*Z = D
C
C  given three points in three-space in array PNT.
C  The four coefficients are returned in the COEF array.
C
      DOUBLE PRECISION PNT(3,3), COEF(4)
      DOUBLE PRECISION X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3
C
      X1 = PNT(1,1)
      X2 = PNT(2,1)
      X3 = PNT(3,1)
      Y1 = PNT(1,2)
      Y2 = PNT(2,2)
      Y3 = PNT(3,2)
      Z1 = PNT(1,3)
      Z2 = PNT(2,3)
      Z3 = PNT(3,3)
C
      COEF(1) =  (Y1*(Z2-Z3) - Z1*(Y2-Y3) + (Y2*Z3 - Z2*Y3))
      COEF(2) = -(X1*(Z2-Z3) - Z1*(X2-X3) + (X2*Z3 - Z2*X3))
      COEF(3) =  (X1*(Y2-Y3) - Y1*(X2-X3) + (X2*Y3 - Y2*X3))
      COEF(4) =  X1*(Y2*Z3 - Z2*Y3) - Y1*(X2*Z3 - Z2*X3) + 
     +             Z1*(X2*Y3 - X3*Y2)       
C
      RETURN
      END 
      DOUBLE PRECISION FUNCTION XABGCDP (AQDP,BQDP,CQDP)
C
C (XABGCDP = Angle Between Great Circles, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4)
C
C This function, given information about the points A, B, and C on the
C sphere, returns the angle, in degrees, from the great circle through
C A and B to the great circle through A and C, positive if the point C
C is in the hemisphere to the "left" of the great circle through A and
C B, negative otherwise.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
        DOUBLE PRECISION RTOD
C       DOUBLE PRECISION XCB1,YCB1,ZCB1
        DOUBLE PRECISION XCB2,YCB2,ZCB2
        DOUBLE PRECISION      YCB3,ZCB3
C       DOUBLE PRECISION XCC1,YCC1,ZCC1
        DOUBLE PRECISION XCC2,YCC2,ZCC2
        DOUBLE PRECISION      YCC3,ZCC3
        DOUBLE PRECISION      YCC4,ZCC4
        DOUBLE PRECISION DNOM,CANG,SANG
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823D0 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQDP(1)*BQDP(3)
C       YCB1=BQDP(1)*BQDP(4)
C       ZCB1=BQDP(2)
C
C       XCB2=XCB1*AQDP(3)+YCB1*AQDP(4)
C       YCB2=YCB1*AQDP(3)-XCB1*AQDP(4)
C       ZCB2=ZCB1
C
        XCB2=BQDP(1)*BQDP(3)*AQDP(3)+BQDP(1)*BQDP(4)*AQDP(4)
        YCB2=BQDP(1)*BQDP(4)*AQDP(3)-BQDP(1)*BQDP(3)*AQDP(4)
        ZCB2=BQDP(2)
C
C       XCB3=XCB2*AQDP(1)+ZCB2*AQDP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQDP(1)-XCB2*AQDP(2)
C
C Do the same for the point C.
C
C       XCC1=CQDP(1)*CQDP(3)
C       YCC1=CQDP(1)*CQDP(4)
C       ZCC1=CQDP(2)
C
C       XCC2=XCC1*AQDP(3)+YCC1*AQDP(4)
C       YCC2=YCC1*AQDP(3)-XCC1*AQDP(4)
C       ZCC2=ZCC1
C
        XCC2=CQDP(1)*CQDP(3)*AQDP(3)+CQDP(1)*CQDP(4)*AQDP(4)
        YCC2=CQDP(1)*CQDP(4)*AQDP(3)-CQDP(1)*CQDP(3)*AQDP(4)
        ZCC2=CQDP(2)
C
C       XCC3=XCC2*AQDP(1)+ZCC2*AQDP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQDP(1)-XCC2*AQDP(2)
C
C Now, rotate C about the X axis by an amount which would put B on the
C equator.
C
        IF (YCB3.NE.0D0.OR.ZCB3.NE.0.D0) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          CANG=YCB3/DNOM
          SANG=ZCB3/DNOM
        ELSE
          CANG=1.D0
          SANG=0.D0
        END IF
C
C       XCC4=XCC3
        YCC4=YCC3*CANG+ZCC3*SANG
        ZCC4=ZCC3*CANG-YCC3*SANG
C
C The angle between the great circles is now easily computed.
C
        IF (YCC4.NE.0.D0.OR.ZCC4.NE.0.D0) THEN
          XABGCDP=RTOD*DATAN2(ZCC4,YCC4)
        ELSE
          XABGCDP=0.D0
        END IF
C
C Done.
C
        RETURN
C
      END
      FUNCTION ICAPDP(PQDP,QQDP,NPIQ)
C
C (ICAPDP = Integer Check Along Polygon, Double Precision)
C
        DOUBLE PRECISION PQDP(4),QQDP(4,NPIQ)
C
C The value of this function is zero if and only if the point P is
C inside the smaller of the two portions of the sphere formed by the
C boundary Q, which is the edge of a "polygon" defined by the points
C (QQSP(I)), for I from 1 to NPIQ.  (The edge of the polygon is formed
C of shortest great circle routes from point to point.)  The first and
C last points of Q should be identical.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION ADST,ACAPDP,XADGCDP
C
C If the total angle swept out by a vector tangent to the sphere at the
C point P and pointing in the direction of the shortest great circle
C route to a point tracing Q is near zero, then both the point P and its
C antipodal point P' are in the same area, which must therefore be the
C larger of the two areas created by Q.
C
        IF (ABS(ACAPDP(PQDP,QQDP,NPIQ)).LT.180.D0) THEN
          ICAPDP=1
          RETURN
        END IF
C
C Otherwise, P is in one of the two areas and P' is in the other, so
C we perform a somewhat heuristic test to see if P is in the smaller
C of the two areas: we compute the average distance from P to Q and
C see if it is less than 90 degrees or more than 90 degrees.
C
        ADST=0.D0
C
C Sum over all distinct points of Q:
C
        DO 101 I=1,NPIQ-1
          ADST=ADST+XADGCDP(PQDP,QQDP(1,I))
  101   CONTINUE
C
C Set the function value.
C
        ADST=ADST/DBLE(NPIQ-1)
C
        IF (ADST.LT.90.D0) THEN
          ICAPDP=0
        ELSE
          ICAPDP=1
        END IF
C
        RETURN
      END
      DOUBLE PRECISION FUNCTION ACAPDP (PQDP,QQDP,NPIQ)
C
C (ACAPDP = Angular Change Along Polyline, Double Precision)
C
        DOUBLE PRECISION PQDP(4),QQDP(4,NPIQ)
C
C The value of this function is the total angle swept out by a vector
C tangent to the sphere at the point P and pointing in the direction of
C the shortest great circle route to a point tracing Q, a "polyline"
C defined by the points (QQDP(I)), for I from 1 to NPIQ.  (The edge of
C the polyline is formed of shortest great circle routes from point to
C point.)
C
C If the first and last points of Q are the same, the polyline becomes
C a closed polygon dividing the surface of the sphere into two areas -
C one to the left, and one to the right, of Q.  Let P' denote the point
C opposite P on the sphere.  Note that P' = (-PLAT,PLON+180).  In theory
C (ignoring computational inaccuracies), the function can only have
C three possible values: +360, if P is to the left of Q and P' is to
C the right of Q; -360, if P is to the right of Q and P' is to the
C left of Q; and zero, if both P and P' are on the same side of Q (left
C or right, but we don't know which).
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION ANCH,XABGCDP
C
        ANCH=0.D0
C
C Trace the edge:
C
        DO 101 I=1,NPIQ-1
          ANCH=ANCH+XABGCDP(PQDP,QQDP(1,I),QQDP(1,I+1))
  101   CONTINUE
C
C Set the function value.
C
        ACAPDP=ANCH
C
        RETURN
C
      END
      INTEGER FUNCTION GCCWISE(RLAT,RLON,NPTS)
C
C  Given NPTS pairs of (RLAT,RLON) values, this function 
C  returns 1 if they are entered in counterclockwise order
C  and 0 if in clockwise order.
C
      DOUBLE PRECISION RLAT(NPTS),RLON(NPTS)
      DOUBLE PRECISION AREA, X, Y, XP, YP
C
C  Check that the input polygon is closed.
C
      IF (RLAT(NPTS).NE.RLAT(1) .OR. RLON(NPTS).NE.RLON(1)) THEN
        WRITE(6,500) 
  500   FORMAT('GCINOUT: The input spherical polygon must be closed.')
        GCCWISE = -1
        RETURN
      ENDIF
C
      X = 0.D0
      Y = 0.D0
      AREA = 0.D0
C
C  Project the points onto a plane touching the globe at 
C  (RLAT(1),RLON(1)) and use the algebraic sign of 
C  the area computation formula.  Since (RLAT(1),RLON(1)) is 
C  projected onto Cartesian coordinate (0,0), the initial X and
C  Y values are 0., as specified above.
C
      DO 10 I=1,NPTS-1
        CALL GPROJ(RLAT(1),RLON(1),RLAT(I+1),RLON(I+1),XP,YP)
        AREA = AREA + X*YP - XP*Y
        X = XP
        Y = YP
   10 CONTINUE
C
      IF (AREA .LE. 0.D0) THEN
        GCCWISE = 0
      ELSE
        GCCWISE = 1
      ENDIF
C
      RETURN
      END
      SUBROUTINE GPROJ(PHI0, LAMBDA0, PHI, LAMBDA, X, Y)
C
C  Find the projection onto the plane tangent to the globe at
C  (PHI0,LAMBDA0) of the point on the globe (PHI,LAMBDA).
C  The projected point is returned in Cartesian coordinate
C  (X,Y).
C
      DOUBLE PRECISION PHI0, LAMBDA0, PHI, LAMBDA, X, Y
      DOUBLE PRECISION SIN0, COS0, SINP, COSP, SIND, COSD, DENOM
      DOUBLE PRECISION D2R
      PARAMETER (D2R = 0.017453292519943D0)
C
      SIN0 = SIN(D2R*PHI0)
      COS0 = COS(D2R*PHI0)
      SINP = SIN(D2R*PHI)
      COSP = COS(D2R*PHI)
      SIND = SIN(D2R*(LAMBDA-LAMBDA0))
      COSD = COS(D2R*(LAMBDA-LAMBDA0))
      DENOM = SIN0*SINP + COS0*COSP*COSD
      X = (COSP*SIND)/DENOM
      Y = (COS0*SINP - SIN0*COSP*COSD)/DENOM
C
      RETURN
      END
