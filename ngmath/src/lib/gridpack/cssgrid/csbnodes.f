C
C	$Id: csbnodes.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSBNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT)
      INTEGER N, LIST(*), LPTR(*), LEND(N), NODES(*), NB,
     .        NA, NT
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/26/96
C
C   Given a triangulation of N nodes on the unit sphere
C created by Subroutine CSTRMESH, this subroutine returns an
C array containing the indexes (if any) of the counterclock-
C wise-ordered sequence of boundary nodes -- the nodes on
C the boundary of the convex hull of the set of nodes.  (The
C boundary is empty if the nodes do not lie in a single
C hemisphere.)  The numbers of boundary nodes, arcs, and
C triangles are also returned.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C The above parameters are not altered by this routine.
C
C       NODES = Integer array of length at least NB
C               (NB .LE. N).
C
C On output:
C
C       NODES = Ordered sequence of boundary node indexes
C               in the range 1 to N (in the first NB loca-
C               tions).
C
C       NB = Number of boundary nodes.
C
C       NA,NT = Number of arcs and triangles, respectively,
C               in the triangulation.
C
C Modules required by CSBNODES:  None
C
C***********************************************************
C
      INTEGER K, LP, N0, NN, NST
C
C Local parameters:
C
C K =   NODES index
C LP =  LIST pointer
C N0 =  Boundary node to be added to NODES
C NN =  Local copy of N
C NST = First element of nodes (arbitrarily chosen to be
C         the one with smallest index)
C
      NN = N
C
C Search for a boundary node.
C
      DO 1 NST = 1,NN
        LP = LEND(NST)
        IF (LIST(LP) .LT. 0) GO TO 2
    1   CONTINUE
C
C The triangulation contains no boundary nodes.
C
      NB = 0
      NA = 3*(NN-2)
      NT = 2*(NN-2)
      RETURN
C
C NST is the first boundary node encountered.  Initialize
C   for traversal of the boundary.
C
    2 NODES(1) = NST
      K = 1
      N0 = NST
C
C Traverse the boundary in counterclockwise order.
C
    3 LP = LEND(N0)
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (N0 .EQ. NST) GO TO 4
        K = K + 1
        NODES(K) = N0
        GO TO 3
C
C Store the counts.
C
    4 NB = K
      NT = 2*N - NB - 2
      NA = NT + N - 1
      RETURN
      END
