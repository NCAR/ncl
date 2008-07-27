C
C	$Id: csvorod.f,v 1.5 2008-07-27 03:10:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSVOROD(NPTS,RLATI,RLONI,NI,NF,IWK,RWK,NC,RLATO,RLONO,
     +                    RC,NCA,NUMV,NV,IER)
      DOUBLE PRECISION RWK(*),RLATI,RLONI,RLATO,RLONO,RC
C
C  This subroutine returns vertices for a Voronoi polygon
C  enclosing a given input coordinate.
C
C  Arguments:
C
C    NPTS  (Input)
C      The number of input data points (NPTS > 3)
C    RLATI  (Input)
C      An array, dimensioned for NPTS, containing the latitudes
C      of the input coordinates, in degrees.
C    RLONI  (Input)
C      An array, dimensioned for NPTS, containing the longitudes
C      of the input coordinates, in degrees.
C    NI (Input)
C      The index of the input coordinate for which you want
C      to determine the Voronoi polygon (0 .LE. NI .LE. NPTS).
C    NF (Input)
C      Flag indicating if this is the first call to CSVORO
C      to retrieve Voronoi polygons for this dataset (1=yes,
C      0=no).  Calls subsequent to the first call for a given
C      dataset are much faster than the first call.
C    IWK (Input)
C      Integer work space dimensioned for 27*NPTS.
C    RWK (Input)
C      Double precision work space dimensioned for 9*NPTS.
C    NC (Input)
C      The maximum size of the output arrays RLATO, RLONO, and RC.
C      NC should be 2*NPTS.
C    RLATO (Output)
C      An array of latitude values for the Voronoi indices.  These
C      are circumcenters of circles passing through the Delaunay
C      triangles.  If a coordinate is a boundary point, then
C      the circle may pass through certain "pseudo points" that
C      have been added to the original dataset in order to complete
C      the Voronoi polygon.  RLATO is returned in degrees.
C    RLONO (Output)
C      An array of longitude values for the Voronoi indices.  These
C      are circumcenters of circles passing through the Delaunay
C      triangles.  If a coordinate is a boundary point, then
C      the circle may pass through certain "pseudo points" that
C      have been added to the original dataset in order to complete
C      the Voronoi polygon.  RLONO is returned in degrees.
C    RC (Output)
C      Array containing the arc length (in degrees) of the angle
C      between a circumcenter and its associated triangle vertices.
C    NCA (Output)
C      The actual number of circumcenters returned in RLATO and RLONO.
C      This number may be larger than NPTS if the input dataset has
C      boundary points, since certain "pseudo points" may have been
C      added to the original dataset in order to complete the Voronoi
C      polygon set.
C    NUMV (Output)
C      The number of vertices in the Voronoi polygon enclosing
C      the coordinate (RLATI(NI),RLONI(NI)).
C    NV (Output)
C      An array (dimensioned for NPTS) containing NUMV indices for
C      the Voronoi polygon enclosing the coordinate (RLATI(NI),RLONI(NI)).
C      The indices returned in this array refer to the coordinates
C      returned in RLATO and RLONO.  For example, if the integer
C      "J" is an element of the NV array, then (RLATO(J),RLONO(J))
C      is a vertex of the Voronoi polygon enclosing (RLATI(NI),RLONI(NI)).
C      The indices in NV list out the vertices of the Voronoi in
C      counter-clockwise order.
C    IER (Output)
C      An error return value as per:
C         IER =  0  no error.
C         IER =  1  NPTS < 3.
C         IER =  4  first three coordinates are collinear.
C         IER =  6  internal error
C         IER = -L  if coordinates L and M coincide for some
C                   M > L >= 1 .
C
      PARAMETER (D2R=0.017453293D0,R2D=57.295778D0)
      DIMENSION RLATI(NPTS),RLONI(NPTS),IWK(*),RLATO(NC),RLONO(NC),
     +          RC(NC),NV(NPTS)
C
      SAVE
C
      IF (NF.EQ.1) THEN
C
C       Workspace position   Associated Variable     Size
C       ------------------   -------------------   --------
C         IWK(        1)            LIST            6*NPTS
C         IWK( 6*NPTS+1)            LPTR            6*NPTS
C         IWK(12*NPTS+1)            LEND              NPTS
C         IWK(13*NPTS+1)            NEAR              NPTS
C         IWK(14*NPTS+1)            NEXT              NPTS
C         IWK(15*NPTS+1)            LTRI            6*NPTS
C         IWK(21*NPTS+1)            LISTC           6*NPTS
C
C
C  Convert RLATI and RLONI to radians.
C
        N = NPTS 
        DO 7 I=1,N
          RWK(3*N+I) = D2R*RLATI(I)
          RWK(4*N+I) = D2R*RLONI(I)
    7   CONTINUE
C
C  Then convert to Cartesian coordinates.
C
        CALL CSTRANSD(N,RWK(3*N+1),RWK(4*N+1),RWK(1),
     +               RWK(N+1),RWK(2*N+1))
C
        CALL CSTRMESH(NPTS,RWK(1),RWK(N+1),RWK(2*N+1),
     +                IWK(1),IWK(6*NPTS+1),
     +                IWK(12*NPTS+1),LNEW,IWK(13*NPTS+1),
     +                IWK(14*NPTS+1),RWK(3*N+1),IER)
        IF (IER.EQ.0) THEN
            GO TO 210
        ELSE IF (IER.EQ.-1) THEN
            IER = 1
            GO TO 200
        ELSE IF (IER.EQ.-2) THEN
            IER = 4
            GO TO 200
        ELSE IF (IER.EQ.-3) THEN
            IER = 6
            GO TO 200
        ELSE IF (IER.GT.0) THEN
            IER = -IER
            GO TO 200
        ELSE
            IER = 6
            GO TO 200
C
        END IF
  210   CONTINUE
C
        N2 = NPTS*2
        DO 10 I = 1,N2
            RWK(3*N+I) = -99999.D0
   10   CONTINUE
C
C  Get the pointers to the Voronoi polygon vertices.
C
        CALL CSCRLIST(NPTS,NPTS,RWK(1),RWK(N+1),RWK(2*N+1),
     +                IWK(1),IWK(12*NPTS+1),
     +                IWK(6*NPTS+1),LNEW,IWK(15*NPTS+1),
     +                IWK(21*NPTS+1),NB,
     +                RWK(3*N+1),RWK(5*N+1),RWK(7*N+1),IER)
        IF (IER.EQ.0) THEN
            DO 160 K=1,N2
              RLATO(K) = R2D*RWK(3*N+K)
              RLONO(K) = R2D*RWK(5*N+K)
              RC(K)    = R2D*RWK(7*N+K)
  160       CONTINUE 
            GO TO 220
        ELSE IF (IER.EQ.1) THEN
            GO TO 200
        ELSE IF (IER.EQ.2) THEN
            IER = 10
            GO TO 200
        ELSE IF (IER.EQ.3) THEN
            IER = 11
            GO TO 200
        ELSE
            IER = 6
            GO TO 200
        END IF
  220   CONTINUE
C
C  Determine the number of circumcenters returned.  Remember that
C  the RWK array was set to all -99999.D0 before getting the vertices.
C
        DO 20 I = 1,N2
          IF (RWK(3*N+I).EQ.-99999.D0) THEN
            NCA = I - 1
            GO TO 25
          END IF
          IF (I.EQ.N2) NCA = N2
   20     CONTINUE
   25   CONTINUE
      END IF
C
C  Return the indices for the desired Voronoi polygon.
C
      LPL = IWK(12*NPTS+NI)
      NUMV = 1
      NV(NUMV) = IWK(21*NPTS+LPL)
      LP = LPL
   30 CONTINUE
      LP = IWK(6*NPTS+LP)
      NUMV = NUMV + 1
      NV(NUMV) = IWK(21*NPTS+LP)
      IF (LP.NE.LPL) GO TO 30
      RETURN
C
  200 CONTINUE
      CALL CSSERR('CSVOROD',IER)
      NCA = 0
      NUMV = 0
      RETURN
C
      END
