C
C	$Id: csgetnp.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGETNP (X,Y,Z,LIST,LPTR,LEND,L, NPTS, DF,
     .                  IER)
      INTEGER LIST(*), LPTR(*), LEND(*), L, NPTS(L), IER
      DOUBLE PRECISION X(*), Y(*), Z(*), DF
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   Given a Delaunay triangulation of N nodes on the unit
C sphere and an array NPTS containing the indexes of L-1
C nodes ordered by angular distance from NPTS(1), this sub-
C routine sets NPTS(L) to the index of the next node in the
C sequence -- the node, other than NPTS(1),...,NPTS(L-1),
C that is closest to NPTS(1).  Thus, the ordered sequence
C of K closest nodes to N1 (including N1) may be determined
C by K-1 calls to CSGETNP with NPTS(1) = N1 and L = 2,3,...,K
C for K .GE. 2.
C
C   The algorithm uses the property of a Delaunay triangula-
C tion that the K-th closest node to N1 is a neighbor of one
C of the K-1 closest nodes to N1.
C
C
C On input:
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C       LIST,LPTR,LEND = Triangulation data structure.  Re-
C                        fer to Subroutine CSTRMESH.
C
C       L = Number of nodes in the sequence on output.  2
C           .LE. L .LE. N.
C
C The above parameters are not altered by this routine.
C
C       NPTS = Array of length .GE. L containing the indexes
C              of the L-1 closest nodes to NPTS(1) in the
C              first L-1 locations.
C
C On output:
C
C       NPTS = Array updated with the index of the L-th
C              closest node to NPTS(1) in position L unless
C              IER = 1.
C
C       DF = Value of an increasing function (negative cos-
C            ine) of the angular distance between NPTS(1)
C            and NPTS(L) unless IER = 1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if L < 2.
C
C Modules required by CSGETNP:  None
C
C Intrinsic function called by CSGETNP:  ABS
C
C***********************************************************
C
      INTEGER I, LM1, LP, LPL, N1, NB, NI, NP
      DOUBLE PRECISION DNB, DNP, X1, Y1, Z1
C
C Local parameters:
C
C DNB,DNP =  Negative cosines of the angular distances from
C              N1 to NB and to NP, respectively
C I =        NPTS index and DO-loop index
C LM1 =      L-1
C LP =       LIST pointer of a neighbor of NI
C LPL =      Pointer to the last neighbor of NI
C N1 =       NPTS(1)
C NB =       Neighbor of NI and candidate for NP
C NI =       NPTS(I)
C NP =       Candidate for NPTS(L)
C X1,Y1,Z1 = Coordinates of N1
C
      LM1 = L - 1
      IF (LM1 .LT. 1) GO TO 6
      IER = 0
C
C Store N1 = NPTS(1) and mark the elements of NPTS.
C
      N1 = NPTS(1)
      X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      DO 1 I = 1,LM1
        NI = NPTS(I)
        LEND(NI) = -LEND(NI)
    1   CONTINUE
C
C Candidates for NP = NPTS(L) are the unmarked neighbors
C   of nodes in NPTS.  DNP is initially greater than -cos(PI)
C   (the maximum distance).
C
      DNP = 2.D0
C
C Loop on nodes NI in NPTS.
C
      DO 4 I = 1,LM1
        NI = NPTS(I)
        LPL = -LEND(NI)
        LP = LPL
C
C Loop on neighbors NB of NI.
C
    2   NB = ABS(LIST(LP))
          IF (LEND(NB) .LT. 0) GO TO 3
C
C NB is an unmarked neighbor of NI.  Replace NP if NB is
C   closer to N1.
C
          DNB = -(X(NB)*X1 + Y(NB)*Y1 + Z(NB)*Z1)
          IF (DNB .GE. DNP) GO TO 3
          NP = NB
          DNP = DNB
    3     LP = LPTR(LP)
          IF (LP .NE. LPL) GO TO 2
    4   CONTINUE
      NPTS(L) = NP
      DF = DNP
C
C Unmark the elements of NPTS.
C
      DO 5 I = 1,LM1
        NI = NPTS(I)
        LEND(NI) = -LEND(NI)
    5   CONTINUE
      RETURN
C
C L is outside its valid range.
C
    6 IER = 1
      RETURN
      END
