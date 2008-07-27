C
C	$Id: csinside.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      LOGICAL FUNCTION CSINSIDE (P,LV,XV,YV,ZV,NV,LISTV, IER)
      INTEGER LV, NV, LISTV(NV), IER
      DOUBLE PRECISION P(3), XV(LV), YV(LV), ZV(LV)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   12/27/93
C
C   This function locates a point P relative to a polygonal
C region R on the surface of the unit sphere, returning
C CSINSIDE = TRUE if and only if P is contained in R.  R is
C defined by a cyclically ordered sequence of vertices which
C form a positively-oriented simple closed curve.  Adjacent
C vertices need not be distinct but the curve must not be
C self-intersecting.  Also, while polygon edges are by defi-
C nition restricted to a single hemisphere, R is not so
C restricted.  Its interior is the region to the left as the
C vertices are traversed in order.
C
C   The algorithm consists of selecting a point Q in R and
C then finding all points at which the great circle defined
C by P and Q intersects the boundary of R.  P lies inside R
C if and only if there is an even number of intersection
C points between Q and P.  Q is taken to be a point immedi-
C ately to the left of a directed boundary edge -- the first
C one that results in no consistency-check failures.
C
C   If P is close to the polygon boundary, the problem is
C ill-conditioned and the decision may be incorrect.  Also,
C an incorrect decision may result from a poor choice of Q
C (if, for example, a boundary edge lies on the great cir-
C cle defined by P and Q).  A more reliable result could be
C obtained by a sequence of calls to CSINSIDE with the ver-
C tices cyclically permuted before each call (to alter the
C choice of Q).
C
C
C On input:
C
C       P = Array of length 3 containing the Cartesian
C           coordinates of the point (unit vector) to be
C           located.
C
C       LV = Length of arrays XV, YV, and ZV.
C
C       XV,YV,ZV = Arrays of length LV containing the Carte-
C                  sian coordinates of unit vectors (points
C                  on the unit sphere).  These values are
C                  not tested for validity.
C
C       NV = Number of vertices in the polygon.  3 .LE. NV
C            .LE. LV.
C
C       LISTV = Array of length NV containing the indexes
C               (for XV, YV, and ZV) of a cyclically-ordered
C               (and CCW-ordered) sequence of vertices that
C               define R.  The last vertex (indexed by
C               LISTV(NV)) is followed by the first (indexed
C               by LISTV(1)).  LISTV entries must be in the
C               range 1 to LV.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSINSIDE = TRUE if and only if P lies inside R unless
C                IER .NE. 0, in which case the value is not
C                altered.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if LV or NV is outside its valid
C                     range.
C             IER = 2 if a LISTV entry is outside its valid
C                     range.
C             IER = 3 if the polygon boundary was found to
C                     be self-intersecting.  This error will
C                     not necessarily be detected.
C             IER = 4 if every choice of Q (one for each
C                     boundary edge) led to failure of some
C                     internal consistency check.  The most
C                     likely cause of this error is invalid
C                     input:  P = (0,0,0), a null or self-
C                     intersecting polygon, etc.
C
C Module required by CSINSIDE:  CSINTRSC
C
C Intrinsic function called by CSINSIDE:  SQRT
C
C***********************************************************
C
      INTEGER I1, I2, IERR, IMX, K, K0, N, NI
      LOGICAL EVEN, LFT1, LFT2, PINR, QINR
      DOUBLE PRECISION B(3), BP, BQ, CN(3), D, EPS, PN(3),
     .                 Q(3), QN(3), QNRM, V1(3), V2(3),
     .                 VN(3), VNRM
C
C Local parameters:
C
C B =         Intersection point between the boundary and
C               the great circle defined by P and Q
C BP,BQ =     <B,P> and <B,Q>, respectively, maximized over
C               intersection points B that lie between P and
C               Q (on the shorter arc) -- used to find the
C               closest intersection points to P and Q
C CN =        Q X P = normal to the plane of P and Q
C D =         Dot product <B,P> or <B,Q>
C EPS =       Parameter used to define Q as the point whose
C               orthogonal distance to (the midpoint of)
C               boundary edge V1->V2 is approximately EPS/
C               (2*Cos(A/2)), where <V1,V2> = Cos(A).
C EVEN =      TRUE iff an even number of intersection points
C               lie between P and Q (on the shorter arc)
C I1,I2 =     Indexes (LISTV elements) of a pair of adjacent
C               boundary vertices (endpoints of a boundary
C               edge)
C IERR =      Error flag for calls to CSINTRSC (not tested)
C IMX =       Local copy of LV and maximum value of I1 and
C               I2
C K =         DO-loop index and LISTV index
C K0 =        LISTV index of the first endpoint of the
C               boundary edge used to compute Q
C LFT1,LFT2 = Logical variables associated with I1 and I2 in
C               the boundary traversal:  TRUE iff the vertex
C               is strictly to the left of Q->P (<V,CN> > 0)
C N =         Local copy of NV
C NI =        Number of intersections (between the boundary
C               curve and the great circle P-Q) encountered
C PINR =      TRUE iff P is to the left of the directed
C               boundary edge associated with the closest
C               intersection point to P that lies between P
C               and Q (a left-to-right intersection as
C               viewed from Q), or there is no intersection
C               between P and Q (on the shorter arc)
C PN,QN =     P X CN and CN X Q, respectively:  used to
C               locate intersections B relative to arc Q->P
C Q =         (V1 + V2 + EPS*VN/VNRM)/QNRM, where V1->V2 is
C               the boundary edge indexed by LISTV(K0) ->
C               LISTV(K0+1)
C QINR =      TRUE iff Q is to the left of the directed
C               boundary edge associated with the closest
C               intersection point to Q that lies between P
C               and Q (a right-to-left intersection as
C               viewed from Q), or there is no intersection
C               between P and Q (on the shorter arc)
C QNRM =      Euclidean norm of V1+V2+EPS*VN/VNRM used to
C               compute (normalize) Q
C V1,V2 =     Vertices indexed by I1 and I2 in the boundary
C               traversal
C VN =        V1 X V2, where V1->V2 is the boundary edge
C               indexed by LISTV(K0) -> LISTV(K0+1)
C VNRM =      Euclidean norm of VN
C
      DATA EPS/1.D-3/
C
C Store local parameters, test for error 1, and initialize
C   K0.
C
      IMX = LV
      N = NV
      IF (N .LT. 3  .OR.  N .GT. IMX) GO TO 11
      K0 = 0
      I1 = LISTV(1)
      IF (I1 .LT. 1  .OR.  I1 .GT. IMX) GO TO 12
C
C Increment K0 and set Q to a point immediately to the left
C   of the midpoint of edge V1->V2 = LISTV(K0)->LISTV(K0+1):
C   Q = (V1 + V2 + EPS*VN/VNRM)/QNRM, where VN = V1 X V2.
C
    1 K0 = K0 + 1
      IF (K0 .GT. N) GO TO 14
      I1 = LISTV(K0)
      IF (K0 .LT. N) THEN
        I2 = LISTV(K0+1)
      ELSE
        I2 = LISTV(1)
      ENDIF
      IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
      VN(1) = YV(I1)*ZV(I2) - ZV(I1)*YV(I2)
      VN(2) = ZV(I1)*XV(I2) - XV(I1)*ZV(I2)
      VN(3) = XV(I1)*YV(I2) - YV(I1)*XV(I2)
      VNRM = SQRT(VN(1)*VN(1) + VN(2)*VN(2) + VN(3)*VN(3))
      IF (VNRM .EQ. 0.D0) GO TO 1
      Q(1) = XV(I1) + XV(I2) + EPS*VN(1)/VNRM
      Q(2) = YV(I1) + YV(I2) + EPS*VN(2)/VNRM
      Q(3) = ZV(I1) + ZV(I2) + EPS*VN(3)/VNRM
      QNRM = SQRT(Q(1)*Q(1) + Q(2)*Q(2) + Q(3)*Q(3))
      Q(1) = Q(1)/QNRM
      Q(2) = Q(2)/QNRM
      Q(3) = Q(3)/QNRM
C
C Compute CN = Q X P, PN = P X CN, and QN = CN X Q.
C
      CN(1) = Q(2)*P(3) - Q(3)*P(2)
      CN(2) = Q(3)*P(1) - Q(1)*P(3)
      CN(3) = Q(1)*P(2) - Q(2)*P(1)
      IF (CN(1) .EQ. 0.D0  .AND.  CN(2) .EQ. 0.D0  .AND.
     .    CN(3) .EQ. 0.D0) GO TO 1
      PN(1) = P(2)*CN(3) - P(3)*CN(2)
      PN(2) = P(3)*CN(1) - P(1)*CN(3)
      PN(3) = P(1)*CN(2) - P(2)*CN(1)
      QN(1) = CN(2)*Q(3) - CN(3)*Q(2)
      QN(2) = CN(3)*Q(1) - CN(1)*Q(3)
      QN(3) = CN(1)*Q(2) - CN(2)*Q(1)
C
C Initialize parameters for the boundary traversal.
C
      NI = 0
      EVEN = .TRUE.
      BP = -2.0D0
      BQ = -2.0D0
      PINR = .TRUE.
      QINR = .TRUE.
      I2 = LISTV(N)
      IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
      LFT2 = CN(1)*XV(I2) + CN(2)*YV(I2) +
     .       CN(3)*ZV(I2) .GT. 0.D0
C
C Loop on boundary arcs I1->I2.
C
      DO 2 K = 1,N
        I1 = I2
        LFT1 = LFT2
        I2 = LISTV(K)
        IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
        LFT2 = CN(1)*XV(I2) + CN(2)*YV(I2) +
     .         CN(3)*ZV(I2) .GT. 0.D0
        IF (LFT1 .EQV. LFT2) GO TO 2
C
C   I1 and I2 are on opposite sides of Q->P.  Compute the
C     point of intersection B.
C
        NI = NI + 1
        V1(1) = XV(I1)
        V1(2) = YV(I1)
        V1(3) = ZV(I1)
        V2(1) = XV(I2)
        V2(2) = YV(I2)
        V2(3) = ZV(I2)
        CALL CSINTRSC (V1,V2,CN, B,IERR)
C
C   B is between Q and P (on the shorter arc) iff
C     B Forward Q->P and B Forward P->Q       iff
C     <B,QN> > 0 and <B,PN> > 0.
C
        IF (B(1)*QN(1) + B(2)*QN(2) + B(3)*QN(3) .GT. 0.D0
     .      .AND.
     .      B(1)*PN(1) + B(2)*PN(2) + B(3)*PN(3) .GT. 0.D0)
     .    THEN
C
C   Update EVEN, BQ, QINR, BP, and PINR.
C
          EVEN = .NOT. EVEN
          D = B(1)*Q(1) + B(2)*Q(2) + B(3)*Q(3)
          IF (D .GT. BQ) THEN
            BQ = D
            QINR = LFT2
          ENDIF
          D = B(1)*P(1) + B(2)*P(2) + B(3)*P(3)
          IF (D .GT. BP) THEN
            BP = D
            PINR = LFT1
          ENDIF
        ENDIF
    2   CONTINUE
C
C Test for consistency:  NI must be even and QINR must be
C   TRUE.
C
      IF (NI .NE. 2*(NI/2)  .OR.  .NOT. QINR) GO TO 1
C
C Test for error 3:  different values of PINR and EVEN.
C
      IF (PINR .NEQV. EVEN) GO TO 13
C
C No error encountered.
C
      IER = 0
      CSINSIDE = EVEN
      RETURN
C
C LV or NV is outside its valid range.
C
   11 IER = 1
      RETURN
C
C A LISTV entry is outside its valid range.
C
   12 IER = 2
      RETURN
C
C The polygon boundary is self-intersecting.
C
   13 IER = 3
      RETURN
C
C Consistency tests failed for all values of Q.
C
   14 IER = 4
      RETURN
      END
