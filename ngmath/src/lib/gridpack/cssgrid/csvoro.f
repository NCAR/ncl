      SUBROUTINE CSVORO(NPTS,X,Y,Z,NI,NF,IWK,RWK,NC,XC,YC,ZC,RC,
     +                  NCA,NUMV,NV,IER)
C
C  This subroutine returns vertices for a Voronoi polygon
C  enclosing a given input coordinate.
C
C  Arguments:
C
C    NPTS  (Input)
C      The number of input data points (NPTS > 3)
C    X  (Input)
C      An array containing the X Cartesian coordinates of the 
C      input data points. 
C    Y  (Input)
C      An array containing the Y Cartesian coordinates of the 
C      input data points. 
C    Z  (Input)
C      An array containing the Z Cartesian coordinates of the 
C      input data points. 
C    NI (Input)
C      The index of the input coordinate for which you want
C      to determine the Voronoi polygon.
C    NF (Input)
C      Flag indicating if this is the first call to CSVORO
C      to retrieve Voronoi polygons for this dataset (1=yes,
C      0=no).  Calls subsequent to the first call for a given
C      dataset are much faster than the first call.
C    IWK (Input)
C      Integer work space dimensioned for 27*NPTS.
C    RWK (Input)
C      Real work space dimensioned for NPTS.
C    NC (Input)
C      The maximum size of the output arrays XC, YC, ZC, and RC.  
C      NC should be 2*NPTS.
C    XC (Output)
C      The X Cartesian coordinates for the Voronoi indices.  These
C      are circumcenters of circles passing through the Delaunay
C      triangles.  If a coordinate is a boundary point, then
C      the circle may pass through certain "pseudo points" that
C      have been added to the original dataset in order to complete
C      the Voronoi polygon.
C    YC (Output)
C      The Y Cartesian coordinates for the Voronoi indices.  
C    ZC (Output)
C      The Z Cartesian coordinates for the Voronoi indices.  
C    RC (Output)
C      Array containing the arc length (in degrees) of the angle
C      between a circumcenter and its associated triangle vertices.
C    NCA (Output)
C      The actual number of circumcenters returned in XC, YC and ZC.
C      This number may be larger than NPTS if the input dataset has
C      boundary points since certain "pseudo points" may have been 
C      added to the original dataset in order to complete the Voronoi 
C      polygon set.
C    NUMV (Output)
C      The number of vertices in the Voronoi polygon enclosing
C      the coordinate (X(NI),Y(NI),Z(NI).
C    NV (Output)
C      An array (dimensioned for NPTS) containing NUMV indices for
C      the Voronoi polygon enclosing the coordinate (X(NI),Y(NI),Z(NI).
C      The indices returned in this array refer to the coordinates
C      returned in XC, YC, and ZC.  For example, if the integer
C      "J" is an element of the NV array, then (XC(J),YC(J),ZC(J))
C      is a vertex of the Voronoi polygon enclosing (X(NI),Y(NI),Z(NI).
C      The indices in NV list out the vertices of the Voronoi in
C      counter-clockwise order.
C    IER (Output)
C      An error return value as per:
C         IER =  0  no error.
C         IER = -1  NPTS < 3.
C         IER = -2  first three coordinates are collinear.
C         IER = -3  internal error
C         IER =  L  if coordinates L and M coincide for some
C                   M > L.
C
      DIMENSION X(NPTS),Y(NPTS),Z(NPTS),IWK(*),RWK(NPTS),
     +          XC(NC),YC(NC),ZC(NC),RC(NC),NV(NPTS)
C
      SAVE
C
      IF (NF .EQ. 1) THEN
C
C    Workspace position   Associated Variable     Size
C    ------------------   -------------------   --------
C      IWK(        1)            LIST            6*NPTS
C      IWK( 6*NPTS+1)            LPTR            6*NPTS
C      IWK(12*NPTS+1)            LEND              NPTS
C      IWK(13*NPTS+1)            NEAR              NPTS
C      IWK(14*NPTS+1)            NEXT              NPTS
C      IWK(15*NPTS+1)            LTRI            6*NPTS
C      IWK(21*NPTS+1)            LISTC           6*NPTS
C
        CALL CSTRMESH(NPTS,X,Y,Z,IWK(1),IWK(6*NPTS+1),IWK(12*NPTS+1),
     +                LNEW,IWK(13*NPTS+1),IWK(14*NPTS+1),RWK(1),IER)
        IF (IER .NE. 0) RETURN
        N2 = NPTS*2
        DO 10 I=1,N2
          XC(I) = -99999.
   10   CONTINUE
C
C  Get the pointers to the Voronoi polygon vertices.
C
        CALL CSCRLIST(NPTS,NPTS,X,Y,Z,IWK(1),IWK(12*NPTS+1),
     +                IWK(6*NPTS+1),LNEW,IWK(15*NPTS+1),
     +                IWK(21*NPTS+1),NB,XC,YC,ZC,RC,IER)
        IF (IER .NE. 0) RETURN
C
C  Determine the number of circumcenters returned.
C
        DO 20 I=1,N2
          IF(XC(I) .EQ. -99999.) THEN
            NCA = I-1
            GO TO 25
          ENDIF
          IF (I .EQ. N2) NCA = N2
   20   CONTINUE
   25   CONTINUE
      ENDIF
C
C  Return the indices for the desired Voronoi polygon.
C
      LPL = IWK(12*NPTS+NI)
      NUMV = 1
      NV(NUMV) = IWK(21*NPTS+LPL)
      LP  = LPL
  100 CONTINUE
        LP = IWK(6*NPTS+LP)
        NUMV = NUMV+1
        NV(NUMV) = IWK(21*NPTS+LP)
      IF (LP .NE. LPL) GO TO 100
C
      RETURN
      END
