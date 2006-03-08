

      PROGRAM CTEX01
C
C
C D E S C R I P T I O N ------------------------------------------------
C
C
C This example illustrates the use of CONPACKT to draw contours on a
C simple triangular mesh derived from a rectangular grid in the plane
C and it shows, in great detail, how the mesh was constructed.  If you
C want to learn how to do that, this is the best example to study; if
C not, you may find one of the other examples somewhat easier to follow,
C since, in general, those examples use one of the two canned routines
C in CONPACKT to construct a triangular mesh.
C
C
C D E C L A R A T I O N S ----------------------------------------------
C
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.  Use one
C of the following:
C
C       PARAMETER (IERF=6,LUNI=2,IWTY=1 ,IWID=1)  !  NCGM
C       PARAMETER (IERF=6,LUNI=2,IWTY=8 ,IWID=1)  !  X Windows
C       PARAMETER (IERF=6,LUNI=2,IWTY=20,IWID=1)  !  PostScript
C       PARAMETER (IERF=6,LUNI=2,IWTY=11,IWID=1)  !  PDF, Portrait
C       PARAMETER (IERF=6,LUNI=2,IWTY=12,IWID=1)  !  PDF, Landscape
C
        PARAMETER (IERF=6,LUNI=2,IWTYPE=1,IWTY=IWTYPE,IWID=1)
C
C Assume we have a regular rectangular grid, dimensioned IDIM x JDIM,
C where IDIM is the number of points in a horizontal row and JDIM is
C the number of rows.
C
        PARAMETER (IDIM=7,JDIM=7,IDM1=IDIM-1,JDM1=JDIM-1)
C
C Let the values of X corresponding to the left and right edges of the
C grid be XMIN and XMAX and the values of Y corresponding to the bottom
C and top edges of the grid be YMIN and YMAX.
C
        PARAMETER (XMIN=-1.,XMAX=+1.,YMIN=-1.,YMAX=+1.)
C
C An arithmetic statement function Z(X,Y), appearing below, defines
C the data field to be contoured on the mesh.
C
C We derive a triangular mesh from the rectangular grid by dividing
C each rectangular cell in half along one of its diagonals (from lower
C left to upper right in half of them and from upper left to lower right
C in the other half).  To represent the triangular mesh, we use three
C singly-dimensioned arrays: RPNT holds information about points; IEDG,
C information about edges; and ITRI, information about triangles.  The
C elements of each array form "nodes" having lengths as follows:
C
        PARAMETER (LOPN=4)
        PARAMETER (LOEN=5)
        PARAMETER (LOTN=4)
C
C The four elements of a point node, in RPNT, are
C
C   1. the X coordinate of the point;
C   2. the Y coordinate of the point;
C   3. the Z coordinate of the point (always 0 in this example);
C   4. the field value at the point;
C
C The five elements of an edge node, in IEDG, are
C
C   1. the "base index", in RPNT, of point 1 of the edge;
C   2. the "base index", in RPNT, of point 2 of the edge;
C   3. the index, in ITRI, of the pointer to the edge in the node for
C      the triangle to its left (-1 if there is no triangle there);
C   4. the index, in ITRI, of the pointer to the edge in the node for
C      the triangle to its right (-1 if there is no triangle there);
C   5. a utility flag for use by algorithms that scan the structure.
C
C (The "left" and "right" sides of an edge are defined as they would be
C by an observer standing at point 1 of the edge, looking toward point
C 2 of the edge.  It is possible, if there are "holes" in the mesh, that
C there will be no triangle to the left or to the right of an edge, but
C there must be a triangle on one side or the other.)
C
C The four elements of a triangle node, in ITRI, are
C
C   1. the "base index", in IEDG, of edge 1 of the triangle;
C   2. the "base index", in IEDG, of edge 2 of the triangle;
C   3. the "base index", in IEDG, of edge 3 of the triangle;
C   4. a flag set non-zero to block use of the triangle, effectively
C      removing it from the mesh.
C
C The edges pointed to by a triangle node must be in counterclockwise
C order, as viewed from above the mesh.
C
C The "base index" of a point node, an edge node, or a triangle node is
C a non-negative multiple of the length of the node, to which can be
C added an offset to get the index of a particular element of the node.
C Suppose I is the base index of a triangle node; then ITRI(I+1) is
C the triangle node's first element, whose value is the base index of
C the node in IEDG that describes the first edge of the triangle.  So
C IEDG(ITRI(I+1)+2) is the second element of that edge node (whose value
C is the base index of the node describing the second point of the first
C edge of the triangle with base index I.  Similarly, then, we see that
C RPNT(IEDG(ITRI(I+1)+2)+3) is the third (Z) coordinate of the second
C point of the first edge of the triangle with base index I.
C
C The pointers from edge nodes back to triangle nodes (elements 3 and 4)
C allow CONPACKT to navigate the mesh, moving from triangle to triangle
C as it follows a contour line; the pointers are a bit trickily defined:
C if IPTE is the base index of an edge node and IEDG(IPTE+3) is zero or
C more, saying that there is a triangle to the left of the edge, then
C IEDG(IPTE+3) is the actual index of that element of the triangle node
C that points to the edge node; i.e., ITRI(IEDG(IPTE+3))=IPTE.  The base
C index of the triangle node is IPTL, where IPTL is the largest multiple
C of LOTN which is less than IEDG(IPTE+3), as defined by the formula
C IPTL=LOTN*((IEDG(IPTE+3)-1)/LOTN), and the index of the pointer to
C the edge within the triangle node is IPIL=IEDG(IPTE+3)-IPTL (which
C will have the value 1, 2, or 3); observe that IEDG(IPTL+IPIL)=IPTE.
C Similar comments apply to element 4 of an edge node, which, if zero or
C more, points into the triangle node defining the triangle to the right
C of the edge: ITRI(IEDG(IPTE+4))=IPTE; the base index of the node for
C the triangle to the right is IPTR=LOTN*((IEDG(IPTE+4)-1)/LOTN); the
C index of the pointer to the edge within the triangle node is IPIR=
C IEDG(IPTE+4)-IPTR; and, again, observe that IEDG(IPTR+IPIR)=IPTE.
C
C Compute the number of point nodes, edge nodes, and triangle nodes that
C the triangular mesh arrays will need to hold.  The formulas for the
C numbers of point nodes and triangle nodes ought to be quite obvious.
C The one for the number of edge nodes may not be quite so obvious; it
C is based on the facts that each cell of the rectangular grid contains
C one horizontal edge, one vertical edge, and one diagonal edge; that
C there are IDM1 more horizontal edges along the top of the grid; and
C that there are JDM1 more vertical edges along the right edge of the
C grid.
C
        PARAMETER (NOPN=IDIM*JDIM)
        PARAMETER (NOEN=3*IDM1*JDM1+IDM1+JDM1)
        PARAMETER (NOTN=2*IDM1*JDM1)
C
C Now, compute the amount of space we will need in the arrays.
C
        PARAMETER (NPNT=NOPN*LOPN)
        PARAMETER (NEDG=NOEN*LOEN)
        PARAMETER (NTRI=NOTN*LOTN)
C
C Declare the arrays to hold the point nodes, edge nodes, and triangle
C nodes defining the triangular mesh.
C
        DIMENSION RPNT(NPNT),IEDG(NEDG),ITRI(NTRI)
C
C Declare real and integer workspaces needed by CONPACKT.
C
        PARAMETER (LRWK=10000,LIWK=1000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare a character temporary for use in labeling.
C
        CHARACTER*3 CTMP
C
C Declare the area map array needed to do solid fill.
C
        PARAMETER (LAMA=40000)
C
        DIMENSION IAMA(LAMA)
C
C Declare workspace arrays to be used in calls to the AREAS routine
C ARSCAM.
C
        PARAMETER (NCRA=4000,NGPS=2)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare external the routine that does color fill of contour bands.
C
        EXTERNAL DCFOCB
C
C
C A R I T H M E T I C   S T A T E M E N T   F U N C T I O N S ----------
C
C
C This function Z(X,Y) defines the field of data to be contoured on the
C mesh; the first line defines it as 10 times the square of the distance
C from the origin (a paraboloid); the commented-out version defines a
C "monkey saddle".
C
        Z(X,Y)=10.*(X*X+Y*Y)
C       Z(X,Y)=10.*(X*X-Y*Y)
C
C The rest of the arithmetic statement functions are used to enumerate
C the points, edges, and triangles of the rectangular mesh, so that we
C can assign each of them to a unique position in the arrays defining
C the point nodes, the edge nodes, and the triangle nodes of the
C triangular mesh.  (The first frame produced by this program shows the
C enumerations of the points, edges, and triangles, as determined by
C the functions, and will be helpful while reading their descriptions.)
C
C The value of the function INPT(I,J) is a unique integer between 1 and
C IDIM*JDIM, to be assigned to the point of the rectangular mesh having
C indices (I,J).  INPT enumerates the IDIM*JDIM points of the mesh, row
C by row, from bottom to top, and, within each row, from left to right.
C
        INPT(I,J)=(J-1)*IDIM+I
C
C The value of the function INE1(I,J) is a unique integer between 1 and
C IDM1*JDIM, to be assigned to the horizontal edge of the rectangular
C mesh whose left end point has indices (I,J) and whose right end point
C has indices (I+1,J).  INE1 enumerates the IDM1*JDIM horizontal edges
C of the mesh, row by row, from bottom to top, and, within each row,
C from left to right.
C
        INE1(I,J)=(J-1)*IDM1+I
C
C The value of the function INE2(I,J) is a unique integer between
C IDM1*JDIM+1 and IDM1*JDIM+IDIM*JDM1, to be assigned to the vertical
C edge of the rectangular mesh whose bottom end point has indices (I,J)
C and whose top end point has indices (I,J+1).  INE2 enumerates the
C IDIM*JDM1 vertical edges of the mesh, row by row, from bottom to top,
C and, within each row, from left to right.
C
        INE2(I,J)=(J-1)*IDIM+I+IDM1*JDIM
C
C The value of the function INE3(I,J) is a unique integer between
C IDM1*JDIM+IDIM*JDM1+1 and IDM1*JDIM+IDIM*JDM1+IDM1*JDM1, to be
C assigned to a diagonal of the cell of the rectangular mesh with
C lower left corner at indices (I,J).  INE3 enumerates the IDM1*JDM1
C diagonal edges of the mesh, row by row, from bottom to top, and,
C within each row, from left to right.
C
        INE3(I,J)=(J-1)*IDM1+I+IDM1*JDIM+IDIM*JDM1
C
C The value of the function INTR(I,J,K) is a unique integer between 1
C and 2*IDM1*JDM1, to be assigned to triangle K (where K is a 1 or a 2)
C in the cell of the rectangular mesh with lower left corner at indices
C (I,J).  INTR enumerates the triangles of the mesh, row by row, from
C bottom to top; within each row, from left to right; and within each
C cell of the rectangular mesh, from one half to the other.
C
        INTR(I,J,K)=2*((J-1)*IDM1+I-1)+K
C
C Each of the functions IBPT(I,J), IBE1(I,J), IBE2(I,J), IBE3(I,J), and
C IBTR(I,J,K) generates the base index of a point node, an edge node,
C or a triangle node of a particular type.  In each case, we use the
C ordinal number of the node, minus one, times the length of the node.
C
        IBPT(I,J  )=(INPT(I,J  )-1)*LOPN
        IBE1(I,J  )=(INE1(I,J  )-1)*LOEN
        IBE2(I,J  )=(INE2(I,J  )-1)*LOEN
        IBE3(I,J  )=(INE3(I,J  )-1)*LOEN
        IBTR(I,J,K)=(INTR(I,J,K)-1)*LOTN
C
C
C C R E A T E   A   T R I A N G U L A R   M E S H ----------------------
C
C
C Define the points of the mesh.  There are IDIM points per row on each
C of JDIM rows.  The point with indices (I,J) in the rectangular mesh
C is assigned to the point node with base index IBPT(I,J).  Its X and Y
C coordinates are interpolated between XMIN and XMAX and between YMIN
C and YMAX, respectively, and it is given the data value defined by the
C function Z.
C
        DO 102 I=1,IDIM
          DO 101 J=1,JDIM
            IBIP=IBPT(I,J)
            RPNT(IBIP+1)=XMIN+(REAL(I-1)/REAL(IDM1))*(XMAX-XMIN)
            RPNT(IBIP+2)=YMIN+(REAL(J-1)/REAL(JDM1))*(YMAX-YMIN)
            RPNT(IBIP+3)=0.
            RPNT(IBIP+4)=Z(RPNT(IBIP+1),RPNT(IBIP+2))
  101     CONTINUE
  102   CONTINUE
C
C Define the edges of the mesh, each of which joins two adjacent points
C of it, in three groups.
C
C The first group of edges are horizontal; there are IDIM-1 per row on
C each of JDIM rows.  Each joins two points with rectangular indices
C (I,J) and (I+1,J), for some I in [1,IDIM-1] and some J in [1,JDIM],
C and is assigned to the edge node with base index IBE1(I,J).  Its first
C two elements are base indices of the points that it joins; its third
C and fourth elements indicate that there are no triangles to its left
C and right, respectively (which may change when we define the triangle
C list); and its fifth element is initialized to zero.
C
        DO 104 I=1,IDM1
          DO 103 J=1,JDIM
            IBIE=IBE1(I,J)
            IEDG(IBIE+1)=IBPT(I  ,J  )
            IEDG(IBIE+2)=IBPT(I+1,J  )
            IEDG(IBIE+3)=-1
            IEDG(IBIE+4)=-1
            IEDG(IBIE+5)=0
  103     CONTINUE
  104   CONTINUE
C
C The second group of edges are vertical; there are IDIM per row on
C each of JDIM-1 rows.  Each joins two points with rectangular indices
C (I,J) and (I,J+1), for some I in [1,IDIM] and some J in [1,JDIM-1],
C and is assigned to the edge node with base index IBE2(I,J).  Its first
C two elements are base indices of the points that it joins; its third
C and fourth elements indicate that there are no triangles to its left
C and right, respectively (which may change when we define the triangle
C list); and its fifth element is initialized to zero.
C
        DO 106 I=1,IDIM
          DO 105 J=1,JDM1
            IBIE=IBE2(I,J)
            IEDG(IBIE+1)=IBPT(I  ,J  )
            IEDG(IBIE+2)=IBPT(I  ,J+1)
            IEDG(IBIE+3)=-1
            IEDG(IBIE+4)=-1
            IEDG(IBIE+5)=0
  105     CONTINUE
  106   CONTINUE
C
C The third group of edges are diagonals; there are IDIM-1 per row on
C each of JDIM-1 rows.  Each joins two points with rectangular indices
C (I,J) and (I+1,J+1), if the sum of I and J is even, or (I,J+1) and
C (I+1,J), if the sum of I and J is odd, for some I in [1,IDIM-1] and
C some J in [1,JDIM-1], and is assigned to the edge node with base index
C IBE3(I,J).  Its first two elements are base indices of the points that
C it joins; its third and fourth elements indicate that there are no
C triangles to its left and right, respectively (which may change when
C we define the triangle list); and its fifth element is initialized to
C zero.  (Using different diagonals in a checkerboard fashion yields a
C more pleasant pattern to look at than always using the same diagonal
C would.)
C
        DO 108 I=1,IDM1
          DO 107 J=1,JDM1
            IBIE=IBE3(I,J)
            IF (MOD(I+J,2).EQ.0)
     +      THEN
C             diagonal is from lower left to upper right
              IEDG(IBIE+1)=IBPT(I  ,J  )
              IEDG(IBIE+2)=IBPT(I+1,J+1)
              IEDG(IBIE+3)=-1
              IEDG(IBIE+4)=-1
              IEDG(IBIE+5)=0
            ELSE
C             diagonal is from upper left to lower right
              IEDG(IBIE+1)=IBPT(I  ,J+1)
              IEDG(IBIE+2)=IBPT(I+1,J  )
              IEDG(IBIE+3)=-1
              IEDG(IBIE+4)=-1
              IEDG(IBIE+5)=0
            END IF
  107     CONTINUE
  108   CONTINUE
C
C Finally, we define the triangles of the mesh.  The following diagrams
C should be useful in understanding the code that follows:
C
C   CASE 1:  MOD(I+J,2)=0
C
C   +-----a-----+
C   |         . |   In this case, triangles 1 and 2 are in the lower
C   |  2    .   |   right and upper left corners of the rectangular
C   b     c     b   cell, respectively.  The edges of each are labeled
C   |   .    1  |   a (a horizontal edge), b (a vertical edge), and c
C   | .         |   (a diagonal edge).
C   +-----a-----+
C
C   CASE 2:  MOD(I+J,2)=1
C
C   +-----b-----+
C   | .         |   In this case, triangles 1 and 2 are in the lower
C   |   .    2  |   left and upper right corners of the rectangular
C   a     c     a   cell, respectively.  The edges of each are labeled
C   |  1    .   |   a (a vertical edge), b (a horizontal edge), and c
C   |         . |   (a diagonal edge).
C   +-----b-----+
C
C There are JDIM-1 rows of rectangles, with IDIM-1 rectangles in each
C row; each rectangle gives rise to two triangles.  A triangle node
C has the base index IBTR(I,J,K), where I and J specify the lower left
C corner of a cell of the rectangular mesh and K is 1 or 2 for the lower
C and upper triangles in the cell, respectively.  Elements 1, 2, and 3
C of the triangle node are base indices of the edge nodes for the edges
C of the triangle; these are arranged in the order "abc" to ensure that
C they are given in counterclockwise order, as required by CONPACKT.
C Element 4 of the triangle node is a zero, indicating that the triangle
C is not "blocked".  (Later, certain triangles will be blocked as an
C illustration of the capability.)
C
C As each triangle is defined, we also modify element 3 or element 4,
C as appropriate, in each of the edge nodes defining its edges so as
C to point back into the triangle node.  It becomes important, in doing
C this, to remember that the ordering of the points used to define the
C edges is such that the horizontal edges "point" from left to right,
C the vertical edges from bottom to top, and the diagonal edges from
C lower left to upper right (when K=1) or from upper left to lower
C right (when K=2); these conventions tell us whether a given triangle
C is to the left or to the right of a given edge.
C
C NOTE (03/07/2006): For procedural reasons, I needed to remove the
C exclamation-point comments from the following code.  That made the
C code much harder to follow, so I'm putting in two copies of it, the
C first of which has C's in column 1 and the second of which has the
C exclamation-point comments removed.
C
C Original version (easier to follow):
C
C       DO 111 I=1,IDM1
C         DO 110 J=1,JDM1
C           DO 109 K=1,2
C             IBIT=IBTR(I,J,K)
C             IF (MOD(I+J,2).EQ.0)
C    +        THEN ! diagonal is from lower left to upper right
C               IF (K.EQ.1) THEN ! lower right triangle
C                 ITRI(IBIT+1)=IBE1(I  ,J  )  ! edge a
C                 ITRI(IBIT+2)=IBE2(I+1,J  )  ! edge b
C                 ITRI(IBIT+3)=IBE3(I  ,J  )  ! edge c
C                 ITRI(IBIT+4)=0              ! triangle unblocked
C                 IEDG(ITRI(IBIT+1)+3)=IBIT+1 ! triangle left of edge a
C                 IEDG(ITRI(IBIT+2)+3)=IBIT+2 ! triangle left of edge b
C                 IEDG(ITRI(IBIT+3)+4)=IBIT+3 ! triangle right of edge c
C               ELSE                          ! upper left triangle
C                 ITRI(IBIT+1)=IBE1(I  ,J+1)  ! edge a
C                 ITRI(IBIT+2)=IBE2(I  ,J  )  ! edge b
C                 ITRI(IBIT+3)=IBE3(I  ,J  )  ! edge c
C                 ITRI(IBIT+4)=0              ! triangle unblocked
C                 IEDG(ITRI(IBIT+1)+4)=IBIT+1 ! triangle right of edge a
C                 IEDG(ITRI(IBIT+2)+4)=IBIT+2 ! triangle right of edge b
C                 IEDG(ITRI(IBIT+3)+3)=IBIT+3 ! triangle left of edge c
C               END IF
C             ELSE  !  diagonal is from upper left to lower right
C               IF (K.EQ.1) THEN              ! lower left triangle
C                 ITRI(IBIT+1)=IBE2(I  ,J  )  ! edge a
C                 ITRI(IBIT+2)=IBE1(I  ,J  )  ! edge b
C                 ITRI(IBIT+3)=IBE3(I  ,J  )  ! edge c
C                 ITRI(IBIT+4)=0              ! triangle unblocked
C                 IEDG(ITRI(IBIT+1)+4)=IBIT+1 ! triangle right of edge a
C                 IEDG(ITRI(IBIT+2)+3)=IBIT+2 ! triangle left of edge b
C                 IEDG(ITRI(IBIT+3)+4)=IBIT+3 ! triangle right of edge c
C               ELSE                          ! upper right triangle
C                 ITRI(IBIT+1)=IBE2(I+1,J  )  ! edge a
C                 ITRI(IBIT+2)=IBE1(I  ,J+1)  ! edge b
C                 ITRI(IBIT+3)=IBE3(I  ,J  )  ! edge c
C                 ITRI(IBIT+4)=0              ! triangle unblocked
C                 IEDG(ITRI(IBIT+1)+3)=IBIT+1 ! triangle left of edge a
C                 IEDG(ITRI(IBIT+2)+4)=IBIT+2 ! triangle right of edge b
C                 IEDG(ITRI(IBIT+3)+3)=IBIT+3 ! triangle left of edge c
C               END IF
C             END IF
C 109       CONTINUE
C 110     CONTINUE
C 111   CONTINUE
C
C Version with exclamation-point comments removed (for compiler):
C
        DO 111 I=1,IDM1
          DO 110 J=1,JDM1
            DO 109 K=1,2
              IBIT=IBTR(I,J,K)
              IF (MOD(I+J,2).EQ.0)
     +        THEN
                IF (K.EQ.1) THEN
                  ITRI(IBIT+1)=IBE1(I  ,J  )
                  ITRI(IBIT+2)=IBE2(I+1,J  )
                  ITRI(IBIT+3)=IBE3(I  ,J  )
                  ITRI(IBIT+4)=0
                  IEDG(ITRI(IBIT+1)+3)=IBIT+1
                  IEDG(ITRI(IBIT+2)+3)=IBIT+2
                  IEDG(ITRI(IBIT+3)+4)=IBIT+3
                ELSE
                  ITRI(IBIT+1)=IBE1(I  ,J+1)
                  ITRI(IBIT+2)=IBE2(I  ,J  )
                  ITRI(IBIT+3)=IBE3(I  ,J  )
                  ITRI(IBIT+4)=0
                  IEDG(ITRI(IBIT+1)+4)=IBIT+1
                  IEDG(ITRI(IBIT+2)+4)=IBIT+2
                  IEDG(ITRI(IBIT+3)+3)=IBIT+3
                END IF
              ELSE
                IF (K.EQ.1) THEN
                  ITRI(IBIT+1)=IBE2(I  ,J  )
                  ITRI(IBIT+2)=IBE1(I  ,J  )
                  ITRI(IBIT+3)=IBE3(I  ,J  )
                  ITRI(IBIT+4)=0
                  IEDG(ITRI(IBIT+1)+4)=IBIT+1
                  IEDG(ITRI(IBIT+2)+3)=IBIT+2
                  IEDG(ITRI(IBIT+3)+4)=IBIT+3
                ELSE
                  ITRI(IBIT+1)=IBE2(I+1,J  )
                  ITRI(IBIT+2)=IBE1(I  ,J+1)
                  ITRI(IBIT+3)=IBE3(I  ,J  )
                  ITRI(IBIT+4)=0
                  IEDG(ITRI(IBIT+1)+3)=IBIT+1
                  IEDG(ITRI(IBIT+2)+4)=IBIT+2
                  IEDG(ITRI(IBIT+3)+3)=IBIT+3
                END IF
              END IF
  109       CONTINUE
  110     CONTINUE
  111   CONTINUE
C
C The triangular mesh is complete.  For illustrative purposes, we will
C block some triangles (numbered 29, 30, 31, 32, 41, 42, 43, and 44) in
C the center of the mesh (but only when IDIM and JDIM are both 7, since
C these specific numbers are used for the triangles blocked).
C
        IF (IDIM.EQ.7.AND.JDIM.EQ.7) THEN
          ITRI(28*LOTN+4)=1
          ITRI(29*LOTN+4)=1
          ITRI(30*LOTN+4)=1
          ITRI(31*LOTN+4)=1
          ITRI(40*LOTN+4)=1
          ITRI(41*LOTN+4)=1
          ITRI(42*LOTN+4)=1
          ITRI(43*LOTN+4)=1
        END IF
C
C
C O P E N   A N D   I N I T I A L I Z E   G K S ------------------------
C
C
C Open GKS.
C
        CALL GOPKS (IERF,0)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define a basic set of colors (0 = white, background; 1 = black,
C foreground; 2 = yellow; 3 = magenta; 4 = red; 5 = cyan; 6 = green;
C 7 = blue; 8 = a yellow for unblocked portions of the mesh; and 9 =
C a yellow for blocked portions of the mesh).
C
        CALL GSCR   (IWID, 0,1.,1.,1.)
        CALL GSCR   (IWID, 1,0.,0.,0.)
        CALL GSCR   (IWID, 2,1.,1.,0.)
        CALL GSCR   (IWID, 3,1.,0.,1.)
        CALL GSCR   (IWID, 4,1.,0.,0.)
        CALL GSCR   (IWID, 5,0.,1.,1.)
        CALL GSCR   (IWID, 6,0.,1.,0.)
        CALL GSCR   (IWID, 7,0.,0.,1.)
        CALL GSCR   (IWID, 8,.8,.8,0.)
        CALL GSCR   (IWID, 9,1.,1.,0.)
C
C Define 100 colors, associated with color indices 151 through 250, to
C be used for color-filled contour bands and in cell arrays, ranging
C from blue to red.
C
        CALL DFCLRS (IWID,151,250,0.,0.,1.,1.,0.,0.)
C
C
C S E T   I N T E R N A L   P A R A M E T E R S ------------------------
C
C
C Tell PLOTCHAR to use one of the filled fonts and to outline each
C character.
C
        CALL PCSETI ('FN',25)
        CALL PCSETI ('OF',1)
C
C Tell CONPACKT to use a smaller viewport, to match what is used in
C drawing the picture of the triangular mesh.
C
        CALL CTSETR ('VPL - VIEWPORT LEFT  ',.1)
        CALL CTSETR ('VPR - VIEWPORT RIGHT ',.9)
        CALL CTSETR ('VPB - VIEWPORT TOP   ',.1)
        CALL CTSETR ('VPT - VIEWPORT BOTTOM',.9)
C
C Tell CONPACKT to draw the mesh's outer boundary in red.
C
        CALL CTSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CTSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CTSETI ('CLC - CONTOUR LEVEL COLOR INDEX',4)
C
C Tell CONPACKT to use bigger labels on contours drawn by the dash
C package and to put them a little closer together.
C
        CALL CTSETR ('DPS - DASH PATTERN SIZE (CHARACTERS)',.015)
        CALL CTSETR ('DPV - DASH PATTERN VECTOR LENGTH',.0025)
C
C
C D R A W   A   P I C T U R E   O F   T H E   M E S H ------------------
C
C
C The picture is drawn only if IDIM and JDIM are both 7, because it has
C some features specific to that case.
C
        IF (IDIM.NE.7.OR.JDIM.NE.7) GO TO 117
C
C Do a SET call enabling the use of the X and Y coordinates in the mesh.
C
        CALL SET    (.10,.90,.10,.90,XMIN,XMAX,YMIN,YMAX,1)
C
C Label the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.965),'Simple Triangular Mesh Derived
     + From Rectangular Mesh',.014,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.940), 'The mesh is in yellow, with a
     + lighter shade where triangles are blocked.',.011,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.033),'Point numbers are in blue; edg
     +e numbers, in green; triangle numbers, in red.',.011,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.011), 'Triangles 29-32 and 41-44 are
     + blocked; no contours will be drawn there.',.011,0.,0.)
C
C Label the edges of the mesh.
C
        DO 112 I=1,IDIM
          WRITE (CTMP,'(''I='',I1)') I
          CALL PLCHMQ (XMIN+(REAL(I-1)/REAL(IDIM-1))*(XMAX-XMIN),
     +                 CFUY(CUFY(YMIN)-.040),
     +                 CTMP,.020,0.,0.)
  112   CONTINUE
C
        DO 113 J=1,JDIM
          WRITE (CTMP,'(''J='',I1)') J
          CALL PLCHMQ (CFUX(CUFX(XMIN)-.040),
     +                 YMIN+(REAL(J-1)/REAL(JDIM-1))*(YMAX-YMIN),
     +                 CTMP,.020,0.,1.)
  113   CONTINUE
C
C Draw the mesh in two shades of yellow, to distinguish blocked portions
C of it from unblocked portions.
C
        CALL DRWMSH (RPNT,NPNT,LOPN,
     +               IEDG,NEDG,LOEN,
     +               ITRI,NTRI,LOTN,
     +               8,9)
C
C Label the points of the mesh, in blue.
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (7)
        CALL GSLWSC (2.)
C
        DO 114 IPTP=0,NPNT-LOPN,LOPN
          CALL LABLIT (IPTP/LOPN+1,.024,0.,RPNT(IPTP+1),RPNT(IPTP+2))
  114   CONTINUE
C
C Label the edges of the mesh, in green.
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (6)
        CALL GSLWSC (2.)
C
        DO 115 IPTE=0,NEDG-LOEN,LOEN
          CALL LABLIT (IPTE/LOEN+1,.012,
     +                 ATAN2(RPNT(IEDG(IPTE+2)+2)-RPNT(IEDG(IPTE+1)+2),
     +                       RPNT(IEDG(IPTE+2)+1)-RPNT(IEDG(IPTE+1)+1)),
     +                   .5*(RPNT(IEDG(IPTE+1)+1)+RPNT(IEDG(IPTE+2)+1)),
     +                   .5*(RPNT(IEDG(IPTE+1)+2)+RPNT(IEDG(IPTE+2)+2)))
  115   CONTINUE
C
C Label the triangles of the mesh, in red.
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (4)
        CALL GSLWSC (2.)
C
        DO 116 IPTT=0,NTRI-LOTN,LOTN
          CALL LABLIT (IPTT/LOTN+1,.018,0.,
     +                 (RPNT(IEDG(ITRI(IPTT+1)+1)+1)+
     +                  RPNT(IEDG(ITRI(IPTT+1)+2)+1)+
     +                  RPNT(IEDG(ITRI(IPTT+2)+1)+1)+
     +                  RPNT(IEDG(ITRI(IPTT+2)+2)+1)+
     +                  RPNT(IEDG(ITRI(IPTT+3)+1)+1)+
     +                  RPNT(IEDG(ITRI(IPTT+3)+2)+1))/6.,
     +                 (RPNT(IEDG(ITRI(IPTT+1)+1)+2)+
     +                  RPNT(IEDG(ITRI(IPTT+1)+2)+2)+
     +                  RPNT(IEDG(ITRI(IPTT+2)+1)+2)+
     +                  RPNT(IEDG(ITRI(IPTT+2)+2)+2)+
     +                  RPNT(IEDG(ITRI(IPTT+3)+1)+2)+
     +                  RPNT(IEDG(ITRI(IPTT+3)+2)+2))/6.)
  116   CONTINUE
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (1)
        CALL GSLWSC (1.)
C
C Advance the frame.
C
        CALL FRAME
C
C
C D R A W   A   S I M P L E   C O N T O U R   P L O T ------------------
C
C
C Label the frame.
C
  117   CALL PLCHHQ (CFUX(.5),CFUY(.950), 'Contours on Triangular Mesh',
     +.018,0.,0.)
C
C Initialize CONPACKT.
C
        CALL CTMESH (RPNT,NPNT,LOPN,
     +               IEDG,NEDG,LOEN,
     +               ITRI,NTRI,LOTN,
     +               RWRK,LRWK,
     +               IWRK,LIWK)
C
C Draw the mesh in two shades of yellow, to distinguish blocked portions
C of it from unblocked portions.
C
        CALL DRWMSH (RPNT,NPNT,LOPN,
     +               IEDG,NEDG,LOEN,
     +               ITRI,NTRI,LOTN,
     +               8,9)
C
C Draw contour lines with labels written by the dash package.
C
        CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Add informational and high/low labels.
C
        CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Advance the frame.
C
        CALL FRAME
C
C
C D R A W   A   C O L O R - F I L L E D   C O N T O U R   P L O T ------
C
C
C Label the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.950), 'Colored Contour Bands on Tria
     +ngular Mesh',.018,0.,0.)
C
C Initialize an area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put contour lines in the area map.
C
        CALL CTCLAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
C Scan the area map and deliver the areas defined by it to the routine
C DCFOCB.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DCFOCB)
C
C Advance the frame.
C
        CALL FRAME
C
C
C C L O S E   G K S   A N D   Q U I T ----------------------------------
C
C
C Close GKS.
C
        CALL GDAWK (IWID)
        CALL GCLWK (IWID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END


      SUBROUTINE DFCLRS (IWID,IOFC,IOLC,REDF,GRNF,BLUF,REDL,GRNL,BLUL)
C
C This routine defines color indices IOFC through IOLC on workstation
C IWID by interpolating values from REDF/GRNF/BLUF to REDL/GRNL/BLUL.
C
        DO 101 I=IOFC,IOLC
          P=REAL(IOLC-I)/REAL(IOLC-IOFC)
          Q=REAL(I-IOFC)/REAL(IOLC-IOFC)
          CALL GSCR (IWID,I,P*REDF+Q*REDL,P*GRNF+Q*GRNL,P*BLUF+Q*BLUL)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DRWMSH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   ICI1,ICI2)
C
        DIMENSION RPNT(NPNT),IEDG(NEDG),ITRI(NTRI)
C
C Draw the edges of the triangular mesh defined by the arguments.  Use
C color index ICI1 to draw edges in areas that are unblocked and color
C index ICI2 to draw edges in areas that are blocked.
C
        DO 101 IPTE=0,NEDG-LOEN,LOEN
C
          IFLL=0
C
          IF (IEDG(IPTE+3).GE.0) THEN
            IF (ITRI(LOTN*((IEDG(IPTE+3)-1)/LOTN)+4).EQ.0) IFLL=1
          END IF
C
          IFLR=0
C
          IF (IEDG(IPTE+4).GE.0) THEN
            IF (ITRI(LOTN*((IEDG(IPTE+4)-1)/LOTN)+4).EQ.0) IFLR=1
          END IF
C
          IF (IFLL.NE.0.OR.IFLR.NE.0) THEN
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (ICI1)
          ELSE
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (ICI2)
          END IF
C
          CALL LINE   (RPNT(IEDG(IPTE+1)+1),RPNT(IEDG(IPTE+1)+2),
     +                 RPNT(IEDG(IPTE+2)+1),RPNT(IEDG(IPTE+2)+2))
C
  101   CONTINUE
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (1)
C
        RETURN
C
      END


      SUBROUTINE LABLIT (INUM,SIZE,ANGR,XPOS,YPOS)
C
        CHARACTER*8 CNUM
C
C Generate a label expressing the value of the integer INUM and place it
C on the frame at the position (XPOS,YPOS) in the current user system,
C using characters of size SIZE (in the fractional coordinate system),
C and an angle of ANGR (in radians).
C
C Conversion factor, radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Encode the integer into CNUM.
C
        WRITE (CNUM,'(I8)') INUM
C
C Write the label and return.  The loop finds the first non-blank in the
C label so that we can use sub-string notation in the call to PLCHMQ.
C
        DO 101 I=1,8
          IF (CNUM(I:I).NE.' ') THEN
            CALL PLCHMQ (XPOS,YPOS,CNUM(I:8),SIZE,RTOD*ANGR,0.)
            RETURN
          END IF
  101   CONTINUE
C
C If no non-blank character is found, return, having done nothing.
C
        RETURN
C
      END


      SUBROUTINE DCFOCB (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine fills the area defined by the points (XCRA(I),YCRA(I)),
C for I = 1 to NCRA, if and only if none of the area identifiers for
C the area are negative.  The color used is determined from the area
C identifier of the area relative to group 3; we assume that 100 colors
C are defined having color indices 151 through 250 and interpolate to
C determine which of these colors to use.
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C Retrieve the number of contour levels being used.
C
        CALL CTGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
C
C If the number of contour levels is non-zero and the area has more
C than two points, fill it.
C
        IF (NOCL.NE.0.AND.NCRA.GT.2) THEN
C
          IAI3=-1
C
          DO 101 I=1,NGPS
            IF (IAGI(I).EQ.3) IAI3=IAAI(I)
  101     CONTINUE
C
          IF (IAI3.GE.1.AND.IAI3.LE.NOCL+1) THEN
            CALL GSFACI (151+INT(((REAL(IAI3)-.5)/REAL(NOCL+1))*100.))
            CALL GFA    (NCRA,XCRA,YCRA)
          ELSE IF (IAI3.EQ.1001) THEN
            CALL GSFACI (2)
            CALL GFA    (NCRA,XCRA,YCRA)
          ELSE IF (IAI3.EQ.1002) THEN
            CALL GSFACI (3)
            CALL GFA    (NCRA,XCRA,YCRA)
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END
