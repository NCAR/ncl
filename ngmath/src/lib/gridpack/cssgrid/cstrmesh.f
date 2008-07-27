C
C	$Id: cstrmesh.f,v 1.6 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRMESH (N,X,Y,Z, LIST,LPTR,LEND,LNEW,NEAR,
     +                     NEXT,DIST,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), LNEW, NEAR(N),
     +        NEXT(N), IER
      DOUBLE PRECISION X(N), Y(N), Z(N), DIST(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/08/99
C
C   This subroutine creates a Delaunay triangulation of a
C set of N arbitrarily distributed points, referred to as
C nodes, on the surface of the unit sphere.  The Delaunay
C triangulation is defined as a set of (spherical) triangles
C with the following five properties:
C
C  1)  The triangle vertices are nodes.
C  2)  No triangle contains a node other than its vertices.
C  3)  The interiors of the triangles are pairwise disjoint.
C  4)  The union of triangles is the convex hull of the set
C        of nodes (the smallest convex set that contains
C        the nodes).  If the nodes are not contained in a
C        single hemisphere, their convex hull is the en-
C        tire sphere and there are no boundary nodes.
C        Otherwise, there are at least three boundary nodes.
C  5)  The interior of the circumcircle of each triangle
C        contains no node.
C
C The first four properties define a triangulation, and the
C last property results in a triangulation which is as close
C as possible to equiangular in a certain sense and which is
C uniquely defined unless four or more nodes lie in a common
C plane.  This property makes the triangulation well-suited
C for solving closest-point problems and for triangle-based
C interpolation.
C
C   The algorithm has expected time complexity O(N*log(N))
C for most nodal distributions.
C
C   Spherical coordinates (latitude and longitude) may be
C converted to Cartesian coordinates by Subroutine CSTRANSD.
C
C   The following is a list of the software package modules
C which a user may wish to call directly:
C
C  CSADDNOD - Updates the triangulation by appending a new
C             node.
C
C  CSAREAS  - Returns the area of a spherical triangle.
C
C  CSBNODES - Returns an array containing the indexes of the
C             boundary nodes (if any) in counterclockwise
C             order.  Counts of boundary nodes, triangles,
C             and arcs are also returned.
C
C  CSCIRCUM - Returns the circumcenter of a spherical trian-
C             gle.
C
C  CSCRLIST - Returns the set of triangle circumcenters
C             (Voronoi vertices) and circumradii associated
C             with a triangulation.
C
C  CSDELARC - Deletes a boundary arc from a triangulation.
C
C  CSDELNOD - Updates the triangulation with a nodal deletion.
C
C  CSEDGE   - Forces an arbitrary pair of nodes to be connec-
C             ted by an arc in the triangulation.
C
C  CSGETNP  - Determines the ordered sequence of L closest
C             nodes to a given node, along with the associ-
C             ated distances.
C
C  CSINSIDE - Locates a point relative to a polygon on the
C             surface of the sphere.
C
C  CSINTRSC - Returns the point of intersection between a
C             pair of great circle arcs.
C
C  CSJRAND  - Generates a uniformly distributed pseudo-random
C             integer.
C
C  CSLEFT   - Locates a point relative to a great circle.
C
C  CSNEARND - Returns the index of the nearest node to an
C             arbitrary point, along with its squared
C             distance.
C
C  CSSCOORD - Converts a point from Cartesian coordinates to
C             spherical coordinates.
C
C  CSSTORE  - Forces a value to be stored in main memory so
C             that the precision of floating point numbers
C             in memory locations rather than registers is
C             computed.
C
C  CSTRANSD - Transforms spherical coordinates into Cartesian
C             coordinates on the unit sphere for input to
C             Subroutine CSTRMESH.
C
C  CSTRLIST - Converts the triangulation data structure to a
C             triangle list more suitable for use in a fin-
C             ite element code.
C
C  CSTRLPRT - Prints the triangle list created by Subroutine
C             CSTRLIST.
C
C  CSTRMESH - Creates a Delaunay triangulation of a set of
C             nodes.
C
C  CSTRPRNT - Prints the triangulation data structure and,
C             optionally, the nodal coordinates.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of distinct nodes.  (X(K),Y(K),
C               Z(K)) is referred to as node K, and K is re-
C               ferred to as a nodal index.  It is required
C               that X(K)**2 + Y(K)**2 + Z(K)**2 = 1 for all
C               K.  The first three nodes must not be col-
C               linear (lie on a common great circle).
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C       NEAR,NEXT,DIST = Work space arrays of length at
C                        least N.  The space is used to
C                        efficiently determine the nearest
C                        triangulation node to each un-
C                        processed node for use by CSADDNOD.
C
C On output:
C
C       LIST = Set of nodal indexes which, along with LPTR,
C              LEND, and LNEW, define the triangulation as a
C              set of N adjacency lists -- counterclockwise-
C              ordered sequences of neighboring nodes such
C              that the first and last neighbors of a bound-
C              ary node are boundary nodes (the first neigh-
C              bor of an interior node is arbitrary).  In
C              order to distinguish between interior and
C              boundary nodes, the last neighbor of each
C              boundary node is represented by the negative
C              of its index.
C
C       LPTR = Set of pointers (LIST indexes) in one-to-one
C              correspondence with the elements of LIST.
C              LIST(LPTR(I)) indexes the node which follows
C              LIST(I) in cyclical counterclockwise order
C              (the first neighbor follows the last neigh-
C              bor).
C
C       LEND = Set of pointers to adjacency lists.  LEND(K)
C              points to the last neighbor of node K for
C              K = 1,...,N.  Thus, LIST(LEND(K)) < 0 if and
C              only if K is a boundary node.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).  LIST, LPTR,
C              LEND, and LNEW are not altered if IER < 0,
C              and are incomplete if IER > 0.
C
C       NEAR,NEXT,DIST = Garbage.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if N < 3 on input.
C             IER = -2 if the first three nodes are
C                      collinear.
C             IER =  L if nodes L and M coincide for some
C                      M > L.  The data structure represents
C                      a triangulation of nodes 1 to M-1 in
C                      this case.
C
C Modules required by CSTRMESH:  CSADDNOD, CSBDYADD, CSCOVSPH,
C                                CSINSERT, CSINTADD, CSJRAND,
C                                CSLEFT, CSLSTPTR, CSSTORE, CSSWAP,
C                                CSSWPTST, CSTRFIND
C
C Intrinsic function called by CSTRMESH:  ABS
C
C***********************************************************
C
      INTEGER I, I0, J, K, LP, LPL, NEXTI, NN
      LOGICAL CSLEFT
      DOUBLE PRECISION D, D1, D2, D3
C
C Local parameters:
C
C D =        (Negative cosine of) distance from node K to
C              node I
C D1,D2,D3 = Distances from node K to nodes 1, 2, and 3,
C              respectively
C I,J =      Nodal indexes
C I0 =       Index of the node preceding I in a sequence of
C              unprocessed nodes:  I = NEXT(I0)
C K =        Index of node to be added and DO-loop index:
C              K > 3
C LP =       LIST index (pointer) of a neighbor of K
C LPL =      Pointer to the last neighbor of K
C NEXTI =    NEXT(I)
C NN =       Local copy of N
C
      NN = N
      IF (NN .LT. 3) THEN
        IER = -1
        RETURN
      ENDIF
C
C Store the first triangle in the linked list.
C
      IF ( .NOT. CSLEFT (X(1),Y(1),Z(1),X(2),Y(2),Z(2),
     .                 X(3),Y(3),Z(3)) ) THEN
C
C   The first triangle is (3,2,1) = (2,1,3) = (1,3,2).
C
        LIST(1) = 3
        LPTR(1) = 2
        LIST(2) = -2
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 1
        LPTR(3) = 4
        LIST(4) = -3
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 2
        LPTR(5) = 6
        LIST(6) = -1
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSEIF ( .NOT. CSLEFT(X(2),Y(2),Z(2),X(1),Y(1),Z(1),
     .                    X(3),Y(3),Z(3)) )
     .       THEN
C
C   The first triangle is (1,2,3):  3 Strictly Left 1->2,
C     i.e., node 3 lies in the left hemisphere defined by
C     arc 1->2.
C
        LIST(1) = 2
        LPTR(1) = 2
        LIST(2) = -3
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 3
        LPTR(3) = 4
        LIST(4) = -1
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 1
        LPTR(5) = 6
        LIST(6) = -2
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSE
C
C   The first three nodes are collinear.
C
        IER = -2
        RETURN
      ENDIF
C
C Initialize LNEW and test for N = 3.
C
      LNEW = 7
      IF (NN .EQ. 3) THEN
        IER = 0
        RETURN
      ENDIF
C
C A nearest-node data structure (NEAR, NEXT, and DIST) is
C   used to obtain an expected-time (N*log(N)) incremental
C   algorithm by enabling constant search time for locating
C   each new node in the triangulation.
C
C For each unprocessed node K, NEAR(K) is the index of the
C   triangulation node closest to K (used as the starting
C   point for the search in Subroutine CSTRFIND) and DIST(K)
C   is an increasing function of the arc length (angular
C   distance) between nodes K and NEAR(K):  -Cos(a) for arc
C   length a.
C
C Since it is necessary to efficiently find the subset of
C   unprocessed nodes associated with each triangulation
C   node J (those that have J as their NEAR entries), the
C   subsets are stored in NEAR and NEXT as follows:  for
C   each node J in the triangulation, I = NEAR(J) is the
C   first unprocessed node in J's set (with I = 0 if the
C   set is empty), L = NEXT(I) (if I > 0) is the second,
C   NEXT(L) (if L > 0) is the third, etc.  The nodes in each
C   set are initially ordered by increasing indexes (which
C   maximizes efficiency) but that ordering is not main-
C   tained as the data structure is updated.
C
C Initialize the data structure for the single triangle.
C
      NEAR(1) = 0
      NEAR(2) = 0
      NEAR(3) = 0
      DO 1 K = NN,4,-1
        D1 = -(X(K)*X(1) + Y(K)*Y(1) + Z(K)*Z(1))
        D2 = -(X(K)*X(2) + Y(K)*Y(2) + Z(K)*Z(2))
        D3 = -(X(K)*X(3) + Y(K)*Y(3) + Z(K)*Z(3))
        IF (D1 .LE. D2  .AND.  D1 .LE. D3) THEN
          NEAR(K) = 1
          DIST(K) = D1
          NEXT(K) = NEAR(1)
          NEAR(1) = K
        ELSEIF (D2 .LE. D1  .AND.  D2 .LE. D3) THEN
          NEAR(K) = 2
          DIST(K) = D2
          NEXT(K) = NEAR(2)
          NEAR(2) = K
        ELSE
          NEAR(K) = 3
          DIST(K) = D3
          NEXT(K) = NEAR(3)
          NEAR(3) = K
        ENDIF
    1   CONTINUE
C
C Add the remaining nodes
C
      DO 6 K = 4,NN
        CALL CSADDNOD (NEAR(K),K,X,Y,Z, LIST,LPTR,LEND,
     .               LNEW, IER)
        IF (IER .NE. 0) RETURN
C
C Remove K from the set of unprocessed nodes associated
C   with NEAR(K).
C
        I = NEAR(K)
        IF (NEAR(I) .EQ. K) THEN
          NEAR(I) = NEXT(K)
        ELSE
          I = NEAR(I)
    2     I0 = I
            I = NEXT(I0)
            IF (I .NE. K) GO TO 2
          NEXT(I0) = NEXT(K)
        ENDIF
        NEAR(K) = 0
C
C Loop on neighbors J of node K.
C
        LPL = LEND(K)
        LP = LPL
    3   LP = LPTR(LP)
          J = ABS(LIST(LP))
C
C Loop on elements I in the sequence of unprocessed nodes
C   associated with J:  K is a candidate for replacing J
C   as the nearest triangulation node to I.  The next value
C   of I in the sequence, NEXT(I), must be saved before I
C   is moved because it is altered by adding I to K's set.
C
          I = NEAR(J)
    4     IF (I .EQ. 0) GO TO 5
          NEXTI = NEXT(I)
C
C Test for the distance from I to K less than the distance
C   from I to J.
C
          D = -(X(I)*X(K) + Y(I)*Y(K) + Z(I)*Z(K))
          IF (D .LT. DIST(I)) THEN
C
C Replace J by K as the nearest triangulation node to I:
C   update NEAR(I) and DIST(I), and remove I from J's set
C   of unprocessed nodes and add it to K's set.
C
            NEAR(I) = K
            DIST(I) = D
            IF (I .EQ. NEAR(J)) THEN
              NEAR(J) = NEXTI
            ELSE
              NEXT(I0) = NEXTI
            ENDIF
            NEXT(I) = NEAR(K)
            NEAR(K) = I
          ELSE
            I0 = I
          ENDIF
C
C Bottom of loop on I.
C
          I = NEXTI
          GO TO 4
C
C Bottom of loop on neighbors J.
C
    5     IF (LP .NE. LPL) GO TO 3
    6   CONTINUE
      RETURN
      END
