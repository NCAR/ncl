C
C	$Id: cscrlist.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSCRLIST (N,NCOL,X,Y,Z,LIST,LEND, LPTR,LNEW,
     .                   LTRI, LISTC,NB,RLATO,RLONO,RC,IER)
      INTEGER  N, NCOL, LIST(*), LEND(N), LPTR(*), LNEW,
     .         LTRI(6,NCOL), LISTC(*), NB, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), RLATO(*), RLONO(*),
     .                 RC(*), XD, YD, ZD, PNM
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/05/98
C
C   Given a Delaunay triangulation of nodes on the surface
C of the unit sphere, this subroutine returns the set of
C triangle circumcenters corresponding to Voronoi vertices,
C along with the circumradii and a list of triangle indexes
C LISTC stored in one-to-one correspondence with LIST/LPTR
C entries.
C
C   A triangle circumcenter is the point (unit vector) lying
C at the same angular distance from the three vertices and
C contained in the same hemisphere as the vertices.  (Note
C that the negative of a circumcenter is also equidistant
C from the vertices.)  If the triangulation covers the sur-
C face, the Voronoi vertices are the circumcenters of the
C triangles in the Delaunay triangulation.  LPTR, LEND, and
C LNEW are not altered in this case.
C
C   On the other hand, if the nodes are contained in a sin-
C gle hemisphere, the triangulation is implicitly extended
C to the entire surface by adding pseudo-arcs (of length
C greater than 180 degrees) between boundary nodes forming
C pseudo-triangles whose 'circumcenters' are included in the
C list.  This extension to the triangulation actually con-
C sists of a triangulation of the set of boundary nodes in
C which the swap test is reversed (a non-empty circumcircle
C test).  The negative circumcenters are stored as the
C pseudo-triangle 'circumcenters'.  LISTC, LPTR, LEND, and
C LNEW contain a data structure corresponding to the ex-
C tended triangulation (Voronoi diagram), but LIST is not
C altered in this case.  Thus, if it is necessary to retain
C the original (unextended) triangulation data structure,
C copies of LPTR and LNEW must be saved before calling this
C routine.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C           Note that, if N = 3, there are only two Voronoi
C           vertices separated by 180 degrees, and the
C           Voronoi regions are not well defined.
C
C       NCOL = Number of columns reserved for LTRI.  This
C              must be at least NB-2, where NB is the number
C              of boundary nodes.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes (unit vectors).
C
C       LIST = Integer array containing the set of adjacency
C              lists.  Refer to Subroutine CSTRMESH.
C
C       LEND = Set of pointers to ends of adjacency lists.
C              Refer to Subroutine CSTRMESH.
C
C The above parameters are not altered by this routine.
C
C       LPTR = Array of pointers associated with LIST.  Re-
C              fer to Subroutine CSTRMESH.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).
C
C       LTRI = Integer work space array dimensioned 6 by
C              NCOL, or unused dummy parameter if NB = 0.
C
C       LISTC = Integer array of length at least 3*NT, where
C               NT = 2*N-4 is the number of triangles in the
C               triangulation (after extending it to cover
C               the entire surface if necessary).
C
C       RLATO,RLONO,RC = Arrays of length NT = 2*N-4.
C
C On output:
C
C       LPTR = Array of pointers associated with LISTC:
C              updated for the addition of pseudo-triangles
C              if the original triangulation contains
C              boundary nodes (NB > 0).
C
C       LNEW = Pointer to the first empty location in LISTC
C              and LPTR (list length plus one).  LNEW is not
C              altered if NB = 0.
C
C       LTRI = Triangle list whose first NB-2 columns con-
C              tain the indexes of a clockwise-ordered
C              sequence of vertices (first three rows)
C              followed by the LTRI column indexes of the
C              triangles opposite the vertices (or 0
C              denoting the exterior region) in the last
C              three rows.  This array is not generally of
C              any use.
C
C       LISTC = Array containing triangle indexes (indexes
C               to RLATO, RLONO, and RC) stored in 1-1 corres-
C               pondence with LIST/LPTR entries (or entries
C               that would be stored in LIST for the
C               extended triangulation):  the index of tri-
C               angle (N1,N2,N3) is stored in LISTC(K),
C               LISTC(L), and LISTC(M), where LIST(K),
C               LIST(L), and LIST(M) are the indexes of N2
C               as a neighbor of N1, N3 as a neighbor of N2,
C               and N1 as a neighbor of N3.  The Voronoi
C               region associated with a node is defined by
C               the CCW-ordered sequence of circumcenters in
C               one-to-one correspondence with its adjacency
C               list (in the extended triangulation).
C
C       NB = Number of boundary nodes unless IER = 1.
C
C       RLATO,RLONO = Arrays containing the lat/lon coordi-
C                     nates of the triangle circumcenters
C                     (Voronoi vertices).  The first NB-2 entries
C                     correspond to pseudo-triangles if NB > 0.
C
C       RC = Array containing circumradii (the arc lengths
C            or angles between the circumcenters and associ-
C            ated triangle vertices) in 1-1 correspondence
C            with circumcenters.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N < 3.
C             IER = 2 if NCOL < NB-2.
C             IER = 3 if a triangle is degenerate (has ver-
C                     tices lying on a common geodesic).
C
C Modules required by CSCRLIST:  CSCIRCUM, CSLSTPTR, CSSWPTST
C
C Intrinsic functions called by CSCRLIST:  ABS, ACOS
C
C***********************************************************
C
      INTEGER CSLSTPTR
      INTEGER I1, I2, I3, I4, IERR, KT, KT1, KT2, KT11,
     .        KT12, KT21, KT22, LP, LPL, LPN, N0, N1, N2,
     .        N3, N4, NM2, NN, NT
      LOGICAL CSSWPTST
      LOGICAL SWP
      DOUBLE PRECISION C(3), T, V1(3), V2(3), V3(3)
C
C Local parameters:
C
C C =         Circumcenter returned by Subroutine CSCIRCUM
C I1,I2,I3 =  Permutation of (1,2,3):  LTRI row indexes
C I4 =        LTRI row index in the range 1 to 3
C IERR =      Error flag for calls to CSCIRCUM
C KT =        Triangle index
C KT1,KT2 =   Indexes of a pair of adjacent pseudo-triangles
C KT11,KT12 = Indexes of the pseudo-triangles opposite N1
C               and N2 as vertices of KT1
C KT21,KT22 = Indexes of the pseudo-triangles opposite N1
C               and N2 as vertices of KT2
C LP,LPN =    LIST pointers
C LPL =       LIST pointer of the last neighbor of N1
C N0 =        Index of the first boundary node (initial
C               value of N1) in the loop on boundary nodes
C               used to store the pseudo-triangle indexes
C               in LISTC
C N1,N2,N3 =  Nodal indexes defining a triangle (CCW order)
C               or pseudo-triangle (clockwise order)
C N4 =        Index of the node opposite N2 -> N1
C NM2 =       N-2
C NN =        Local copy of N
C NT =        Number of pseudo-triangles:  NB-2
C SWP =       Logical variable set to TRUE in each optimiza-
C               tion loop (loop on pseudo-arcs) iff a swap
C               is performed
C V1,V2,V3 =  Vertices of triangle KT = (N1,N2,N3) sent to
C               Subroutine CSCIRCUM
C
      NN = N
      NB = 0
      NT = 0
      IF (NN .LT. 3) GO TO 21
C
C Search for a boundary node N1.
C
      DO 1 N1 = 1,NN
        LP = LEND(N1)
        IF (LIST(LP) .LT. 0) GO TO 2
    1   CONTINUE
C
C The triangulation already covers the sphere.
C
      GO TO 9
C
C There are NB .GE. 3 boundary nodes.  Add NB-2 pseudo-
C   triangles (N1,N2,N3) by connecting N3 to the NB-3
C   boundary nodes to which it is not already adjacent.
C
C   Set N3 and N2 to the first and last neighbors,
C     respectively, of N1.
C
    2 N2 = -LIST(LP)
      LP = LPTR(LP)
      N3 = LIST(LP)
C
C   Loop on boundary arcs N1 -> N2 in clockwise order,
C     storing triangles (N1,N2,N3) in column NT of LTRI
C     along with the indexes of the triangles opposite
C     the vertices.
C
    3 NT = NT + 1
        IF (NT .LE. NCOL) THEN
          LTRI(1,NT) = N1
          LTRI(2,NT) = N2
          LTRI(3,NT) = N3
          LTRI(4,NT) = NT + 1
          LTRI(5,NT) = NT - 1
          LTRI(6,NT) = 0
        ENDIF
        N1 = N2
        LP = LEND(N1)
        N2 = -LIST(LP)
        IF (N2 .NE. N3) GO TO 3
C
      NB = NT + 2
      IF (NCOL .LT. NT) GO TO 22
      LTRI(4,NT) = 0
      IF (NT .EQ. 1) GO TO 7
C
C Optimize the exterior triangulation (set of pseudo-
C   triangles) by applying swaps to the pseudo-arcs N1-N2
C   (pairs of adjacent pseudo-triangles KT1 and KT2 > KT1).
C   The loop on pseudo-arcs is repeated until no swaps are
C   performed.
C
    4 SWP = .FALSE.
      DO 6 KT1 = 1,NT-1
        DO 5 I3 = 1,3
          KT2 = LTRI(I3+3,KT1)
          IF (KT2 .LE. KT1) GO TO 5
C
C   The LTRI row indexes (I1,I2,I3) of triangle KT1 =
C     (N1,N2,N3) are a cyclical permutation of (1,2,3).
C
          IF (I3 .EQ. 1) THEN
            I1 = 2
            I2 = 3
          ELSEIF (I3 .EQ. 2) THEN
            I1 = 3
            I2 = 1
          ELSE
            I1 = 1
            I2 = 2
          ENDIF
          N1 = LTRI(I1,KT1)
          N2 = LTRI(I2,KT1)
          N3 = LTRI(I3,KT1)
C
C   KT2 = (N2,N1,N4) for N4 = LTRI(I,KT2), where
C     LTRI(I+3,KT2) = KT1.
C
          IF (LTRI(4,KT2) .EQ. KT1) THEN
            I4 = 1
          ELSEIF (LTRI(5,KT2) .EQ. KT1) THEN
            I4 = 2
          ELSE
            I4 = 3
          ENDIF
          N4 = LTRI(I4,KT2)
C
C   The empty circumcircle test is reversed for the pseudo-
C     triangles.  The reversal is implicit in the clockwise
C     ordering of the vertices.
C
          IF ( .NOT. CSSWPTST(N1,N2,N3,N4,X,Y,Z) ) GO TO 5
C
C   Swap arc N1-N2 for N3-N4.  KTij is the triangle opposite
C     Nj as a vertex of KTi.
C
          SWP = .TRUE.
          KT11 = LTRI(I1+3,KT1)
          KT12 = LTRI(I2+3,KT1)
          IF (I4 .EQ. 1) THEN
            I2 = 2
            I1 = 3
          ELSEIF (I4 .EQ. 2) THEN
            I2 = 3
            I1 = 1
          ELSE
            I2 = 1
            I1 = 2
          ENDIF
          KT21 = LTRI(I1+3,KT2)
          KT22 = LTRI(I2+3,KT2)
          LTRI(1,KT1) = N4
          LTRI(2,KT1) = N3
          LTRI(3,KT1) = N1
          LTRI(4,KT1) = KT12
          LTRI(5,KT1) = KT22
          LTRI(6,KT1) = KT2
          LTRI(1,KT2) = N3
          LTRI(2,KT2) = N4
          LTRI(3,KT2) = N2
          LTRI(4,KT2) = KT21
          LTRI(5,KT2) = KT11
          LTRI(6,KT2) = KT1
C
C   Correct the KT11 and KT22 entries that changed.
C
          IF (KT11 .NE. 0) THEN
            I4 = 4
            IF (LTRI(4,KT11) .NE. KT1) THEN
              I4 = 5
              IF (LTRI(5,KT11) .NE. KT1) I4 = 6
            ENDIF
            LTRI(I4,KT11) = KT2
          ENDIF
          IF (KT22 .NE. 0) THEN
            I4 = 4
            IF (LTRI(4,KT22) .NE. KT2) THEN
              I4 = 5
              IF (LTRI(5,KT22) .NE. KT2) I4 = 6
            ENDIF
            LTRI(I4,KT22) = KT1
          ENDIF
    5     CONTINUE
    6   CONTINUE
      IF (SWP) GO TO 4
C
C Compute and store the negative circumcenters and radii of
C   the pseudo-triangles in the first NT positions.
C
    7 DO 8 KT = 1,NT
        N1 = LTRI(1,KT)
        N2 = LTRI(2,KT)
        N3 = LTRI(3,KT)
        V1(1) = X(N1)
        V1(2) = Y(N1)
        V1(3) = Z(N1)
        V2(1) = X(N2)
        V2(2) = Y(N2)
        V2(3) = Z(N2)
        V3(1) = X(N3)
        V3(2) = Y(N3)
        V3(3) = Z(N3)
        CALL CSCIRCUM (V1,V2,V3, C,IERR)
        IF (IERR .NE. 0) GO TO 23
C
C   Store the negative circumcenter and radius (computed
C     from <V1,C>).
C
        
        XD = C(1)
        YD = C(2)
        ZD = C(3)
        CALL CSSCOORDD(XD,YD,ZD,RLATO(KT),RLONO(KT),PNM)
        T = V1(1)*C(1) + V1(2)*C(2) + V1(3)*C(3)
        IF (T .LT. -1.0D0) T = -1.0D0
        IF (T .GT. 1.0D0) T = 1.0D0
        RC(KT) = ACOS(T)
    8   CONTINUE
C
C Compute and store the circumcenters and radii of the
C   actual triangles in positions KT = NT+1, NT+2, ...
C   Also, store the triangle indexes KT in the appropriate
C   LISTC positions.
C
    9 KT = NT
C
C   Loop on nodes N1.
C
      NM2 = NN - 2
      DO 12 N1 = 1,NM2
        LPL = LEND(N1)
        LP = LPL
        N3 = LIST(LP)
C
C   Loop on adjacent neighbors N2,N3 of N1 for which N2 > N1
C     and N3 > N1.
C
   10   LP = LPTR(LP)
          N2 = N3
          N3 = ABS(LIST(LP))
          IF (N2 .LE. N1  .OR.  N3 .LE. N1) GO TO 11
          KT = KT + 1
C
C   Compute the circumcenter C of triangle KT = (N1,N2,N3).
C
          V1(1) = X(N1)
          V1(2) = Y(N1)
          V1(3) = Z(N1)
          V2(1) = X(N2)
          V2(2) = Y(N2)
          V2(3) = Z(N2)
          V3(1) = X(N3)
          V3(2) = Y(N3)
          V3(3) = Z(N3)
          CALL CSCIRCUM (V1,V2,V3, C,IERR)
          IF (IERR .NE. 0) GO TO 23
C
C   Store the circumcenter, radius and triangle index.
C
          XD = C(1)
          YD = C(2)
          ZD = C(3)
          CALL CSSCOORDD(XD,YD,ZD,RLATO(KT),RLONO(KT),PNM)
          T = V1(1)*C(1) + V1(2)*C(2) + V1(3)*C(3)
          IF (T .LT. -1.0D0) T = -1.0D0
          IF (T .GT. 1.0D0) T = 1.0D0
          RC(KT) = ACOS(T)
C
C   Store KT in LISTC(LPN), where Abs(LIST(LPN)) is the
C     index of N2 as a neighbor of N1, N3 as a neighbor
C     of N2, and N1 as a neighbor of N3.
C
          LPN = CSLSTPTR(LPL,N2,LIST,LPTR)
          LISTC(LPN) = KT
          LPN = CSLSTPTR(LEND(N2),N3,LIST,LPTR)
          LISTC(LPN) = KT
          LPN = CSLSTPTR(LEND(N3),N1,LIST,LPTR)
          LISTC(LPN) = KT
   11     IF (LP .NE. LPL) GO TO 10
   12   CONTINUE
      IF (NT .EQ. 0) GO TO 20
C
C Store the first NT triangle indexes in LISTC.
C
C   Find a boundary triangle KT1 = (N1,N2,N3) with a
C     boundary arc opposite N3.
C
      KT1 = 0
   13 KT1 = KT1 + 1
      IF (LTRI(4,KT1) .EQ. 0) THEN
        I1 = 2
        I2 = 3
        I3 = 1
        GO TO 14
      ELSEIF (LTRI(5,KT1) .EQ. 0) THEN
        I1 = 3
        I2 = 1
        I3 = 2
        GO TO 14
      ELSEIF (LTRI(6,KT1) .EQ. 0) THEN
        I1 = 1
        I2 = 2
        I3 = 3
        GO TO 14
      ENDIF
      GO TO 13
   14 N1 = LTRI(I1,KT1)
      N0 = N1
C
C   Loop on boundary nodes N1 in CCW order, storing the
C     indexes of the clockwise-ordered sequence of triangles
C     that contain N1.  The first triangle overwrites the
C     last neighbor position, and the remaining triangles,
C     if any, are appended to N1's adjacency list.
C
C   A pointer to the first neighbor of N1 is saved in LPN.
C
   15 LP = LEND(N1)
      LPN = LPTR(LP)
      LISTC(LP) = KT1
C
C   Loop on triangles KT2 containing N1.
C
   16 KT2 = LTRI(I2+3,KT1)
      IF (KT2 .NE. 0) THEN
C
C   Append KT2 to N1's triangle list.
C
        LPTR(LP) = LNEW
        LP = LNEW
        LISTC(LP) = KT2
        LNEW = LNEW + 1
C
C   Set KT1 to KT2 and update (I1,I2,I3) such that
C     LTRI(I1,KT1) = N1.
C
        KT1 = KT2
        IF (LTRI(1,KT1) .EQ. N1) THEN
          I1 = 1
          I2 = 2
          I3 = 3
        ELSEIF (LTRI(2,KT1) .EQ. N1) THEN
          I1 = 2
          I2 = 3
          I3 = 1
        ELSE
          I1 = 3
          I2 = 1
          I3 = 2
        ENDIF
        GO TO 16
      ENDIF
C
C   Store the saved first-triangle pointer in LPTR(LP), set
C     N1 to the next boundary node, test for termination,
C     and permute the indexes:  the last triangle containing
C     a boundary node is the first triangle containing the
C     next boundary node.
C
      LPTR(LP) = LPN
      N1 = LTRI(I3,KT1)
      IF (N1 .NE. N0) THEN
        I4 = I3
        I3 = I2
        I2 = I1
        I1 = I4
        GO TO 15
      ENDIF
C
C No errors encountered.
C
   20 IER = 0
      RETURN
C
C N < 3.
C
   21 IER = 1
      RETURN
C
C Insufficient space reserved for LTRI.
C
   22 IER = 2
      RETURN
C
C Error flag returned by CSCIRCUM: KT indexes a null triangle.
C
   23 IER = 3
      RETURN
      END
