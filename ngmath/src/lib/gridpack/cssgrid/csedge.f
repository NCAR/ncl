C
C	$Id: csedge.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSEDGE (IN1,IN2,X,Y,Z, LWK,IWK,LIST,LPTR,
     .                 LEND, IER)
      INTEGER IN1, IN2, LWK, IWK(2,*), LIST(*), LPTR(*),
     .        LEND(*), IER
      DOUBLE PRECISION X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/30/98
C
C   Given a triangulation of N nodes and a pair of nodal
C indexes IN1 and IN2, this routine swaps arcs as necessary
C to force IN1 and IN2 to be adjacent.  Only arcs which
C intersect IN1-IN2 are swapped out.  If a Delaunay triangu-
C lation is input, the resulting triangulation is as close
C as possible to a Delaunay triangulation in the sense that
C all arcs other than IN1-IN2 are locally optimal.
C
C   A sequence of calls to CSEDGE may be used to force the
C presence of a set of edges defining the boundary of a non-
C convex and/or multiply connected region, or to introduce
C barriers into the triangulation.  Note that Subroutine
C CSGETNP will not necessarily return closest nodes if the
C triangulation has been constrained by a call to CSEDGE.
C However, this is appropriate in some applications, such
C as triangle-based interpolation on a nonconvex domain.
C
C
C On input:
C
C       IN1,IN2 = Indexes (of X, Y, and Z) in the range 1 to
C                 N defining a pair of nodes to be connected
C                 by an arc.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C The above parameters are not altered by this routine.
C
C       LWK = Number of columns reserved for IWK.  This must
C             be at least NI -- the number of arcs that
C             intersect IN1-IN2.  (NI is bounded by N-3.)
C
C       IWK = Integer work array of length at least 2*LWK.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C On output:
C
C       LWK = Number of arcs which intersect IN1-IN2 (but
C             not more than the input value of LWK) unless
C             IER = 1 or IER = 3.  LWK = 0 if and only if
C             IN1 and IN2 were adjacent (or LWK=0) on input.
C
C       IWK = Array containing the indexes of the endpoints
C             of the new arcs other than IN1-IN2 unless
C             IER > 0 or LWK = 0.  New arcs to the left of
C             IN1->IN2 are stored in the first K-1 columns
C             (left portion of IWK), column K contains
C             zeros, and new arcs to the right of IN1->IN2
C             occupy columns K+1,...,LWK.  (K can be deter-
C             mined by searching IWK for the zeros.)
C
C       LIST,LPTR,LEND = Data structure updated if necessary
C                        to reflect the presence of an arc
C                        connecting IN1 and IN2 unless IER >
C                        0.  The data structure has been
C                        altered if IER >= 4.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if IN1 < 1, IN2 < 1, IN1 = IN2,
C                     or LWK < 0 on input.
C             IER = 2 if more space is required in IWK.
C                     Refer to LWK.
C             IER = 3 if IN1 and IN2 could not be connected
C                     due to either an invalid data struc-
C                     ture or collinear nodes (and floating
C                     point error).
C             IER = 4 if an error flag other than IER = 1
C                     was returned by CSOPTIM.
C             IER = 5 if error flag 1 was returned by CSOPTIM.
C                     This is not necessarily an error, but
C                     the arcs other than IN1-IN2 may not
C                     be optimal.
C
C   An error message is written to the standard output unit
C in the case of IER = 3 or IER = 4.
C
C Modules required by CSEDGE:  CSLEFT, CSLSTPTR, CSOPTIM, CSSWAP,
C                              CSSWPTST
C
C Intrinsic function called by CSEDGE:  ABS
C
C***********************************************************
C
      LOGICAL CSLEFT
      INTEGER I, IERR, IWC, IWCP1, IWEND, IWF, IWL, LFT, LP,
     .        LP21, LPL, N0, N1, N1FRST, N1LST, N2, NEXT,
     .        NIT, NL, NR
      DOUBLE PRECISION DP12, DP1L, DP1R, DP2L, DP2R, X0, X1,
     .                 X2, Y0, Y1, Y2, Z0, Z1, Z2
C
C Local parameters:
C
C DPij =     Dot product <Ni,Nj>
C I =        DO-loop index and column index for IWK
C IERR =     Error flag returned by Subroutine CSOPTIM
C IWC =      IWK index between IWF and IWL -- NL->NR is
C              stored in IWK(1,IWC)->IWK(2,IWC)
C IWCP1 =    IWC + 1
C IWEND =    Input or output value of LWK
C IWF =      IWK (column) index of the first (leftmost) arc
C              which intersects IN1->IN2
C IWL =      IWK (column) index of the last (rightmost) are
C              which intersects IN1->IN2
C LFT =      Flag used to determine if a swap results in the
C              new arc intersecting IN1-IN2 -- LFT = 0 iff
C              N0 = IN1, LFT = -1 implies N0 CSLEFT IN1->IN2,
C              and LFT = 1 implies N0 CSLEFT IN2->IN1
C LP =       List pointer (index for LIST and LPTR)
C LP21 =     Unused parameter returned by CSSWAP
C LPL =      Pointer to the last neighbor of IN1 or NL
C N0 =       Neighbor of N1 or node opposite NR->NL
C N1,N2 =    Local copies of IN1 and IN2
C N1FRST =   First neighbor of IN1
C N1LST =    (Signed) last neighbor of IN1
C NEXT =     Node opposite NL->NR
C NIT =      Flag or number of iterations employed by CSOPTIM
C NL,NR =    Endpoints of an arc which intersects IN1-IN2
C              with NL CSLEFT IN1->IN2
C X0,Y0,Z0 = Coordinates of N0
C X1,Y1,Z1 = Coordinates of IN1
C X2,Y2,Z2 = Coordinates of IN2
C
C
C Store IN1, IN2, and LWK in local variables and test for
C   errors.
C
      N1 = IN1
      N2 = IN2
      IWEND = LWK
      IF (N1 .LT. 1  .OR.  N2 .LT. 1  .OR.  N1 .EQ. N2  .OR.
     .    IWEND .LT. 0) GO TO 31
C
C Test for N2 as a neighbor of N1.  LPL points to the last
C   neighbor of N1.
C
      LPL = LEND(N1)
      N0 = ABS(LIST(LPL))
      LP = LPL
    1 IF (N0 .EQ. N2) GO TO 30
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (LP .NE. LPL) GO TO 1
C
C Initialize parameters.
C
      IWL = 0
      NIT = 0
C
C Store the coordinates of N1 and N2.
C
    2 X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      X2 = X(N2)
      Y2 = Y(N2)
      Z2 = Z(N2)
C
C Set NR and NL to adjacent neighbors of N1 such that
C   NR CSLEFT N2->N1 and NL CSLEFT N1->N2,
C   (NR Forward N1->N2 or NL Forward N1->N2), and
C   (NR Forward N2->N1 or NL Forward N2->N1).
C
C   Initialization:  Set N1FRST and N1LST to the first and
C     (signed) last neighbors of N1, respectively, and
C     initialize NL to N1FRST.
C
      LPL = LEND(N1)
      N1LST = LIST(LPL)
      LP = LPTR(LPL)
      N1FRST = LIST(LP)
      NL = N1FRST
      IF (N1LST .LT. 0) GO TO 4
C
C   N1 is an interior node.  Set NL to the first candidate
C     for NR (NL CSLEFT N2->N1).
C
    3 IF (CSLEFT(X2,Y2,Z2,X1,Y1,Z1,X(NL),Y(NL),Z(NL))) GO TO 4
        LP = LPTR(LP)
        NL = LIST(LP)
        IF (NL .NE. N1FRST) GO TO 3
C
C   All neighbors of N1 are strictly left of N1->N2.
C
      GO TO 5
C
C   NL = LIST(LP) CSLEFT N2->N1.  Set NR to NL and NL to the
C     following neighbor of N1.
C
    4 NR = NL
        LP = LPTR(LP)
        NL = ABS(LIST(LP))
        IF (CSLEFT(X1,Y1,Z1,X2,Y2,Z2,X(NL),Y(NL),Z(NL)) ) THEN
C
C   NL CSLEFT N1->N2 and NR CSLEFT N2->N1.  The Forward tests
C     are employed to avoid an error associated with
C     collinear nodes.
C
          DP12 = X1*X2 + Y1*Y2 + Z1*Z2
          DP1L = X1*X(NL) + Y1*Y(NL) + Z1*Z(NL)
          DP2L = X2*X(NL) + Y2*Y(NL) + Z2*Z(NL)
          DP1R = X1*X(NR) + Y1*Y(NR) + Z1*Z(NR)
          DP2R = X2*X(NR) + Y2*Y(NR) + Z2*Z(NR)
          IF ( (DP2L-DP12*DP1L .GE. 0.D0  .OR.
     .          DP2R-DP12*DP1R .GE. 0.D0)  .AND.
     .         (DP1L-DP12*DP2L .GE. 0.D0  .OR.
     .          DP1R-DP12*DP2R .GE. 0.D0) ) GO TO 6
C
C   NL-NR does not intersect N1-N2.  However, there is
C     another candidate for the first arc if NL lies on
C     the line N1-N2.
C
          IF ( .NOT. CSLEFT(X2,Y2,Z2,X1,Y1,Z1,X(NL),Y(NL),
     .                    Z(NL)) ) GO TO 5
        ENDIF
C
C   Bottom of loop.
C
        IF (NL .NE. N1FRST) GO TO 4
C
C Either the triangulation is invalid or N1-N2 lies on the
C   convex hull boundary and an edge NR->NL (opposite N1 and
C   intersecting N1-N2) was not found due to floating point
C   error.  Try interchanging N1 and N2 -- NIT > 0 iff this
C   has already been done.
C
    5 IF (NIT .GT. 0) GO TO 33
      NIT = 1
      N1 = N2
      N2 = IN1
      GO TO 2
C
C Store the ordered sequence of intersecting edges NL->NR in
C   IWK(1,IWL)->IWK(2,IWL).
C
    6 IWL = IWL + 1
      IF (IWL .GT. IWEND) GO TO 32
      IWK(1,IWL) = NL
      IWK(2,IWL) = NR
C
C   Set NEXT to the neighbor of NL which follows NR.
C
      LPL = LEND(NL)
      LP = LPTR(LPL)
C
C   Find NR as a neighbor of NL.  The search begins with
C     the first neighbor.
C
    7 IF (LIST(LP) .EQ. NR) GO TO 8
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 7
C
C   NR must be the last neighbor, and NL->NR cannot be a
C     boundary edge.
C
      IF (LIST(LP) .NE. NR) GO TO 33
C
C   Set NEXT to the neighbor following NR, and test for
C     termination of the store loop.
C
    8 LP = LPTR(LP)
      NEXT = ABS(LIST(LP))
      IF (NEXT .EQ. N2) GO TO 9
C
C   Set NL or NR to NEXT.
C
      IF ( CSLEFT(X1,Y1,Z1,X2,Y2,Z2,X(NEXT),Y(NEXT),Z(NEXT)) )
     .    THEN
        NL = NEXT
      ELSE
        NR = NEXT
      ENDIF
      GO TO 6
C
C IWL is the number of arcs which intersect N1-N2.
C   Store LWK.
C
    9 LWK = IWL
      IWEND = IWL
C
C Initialize for edge swapping loop -- all possible swaps
C   are applied (even if the new arc again intersects
C   N1-N2), arcs to the left of N1->N2 are stored in the
C   left portion of IWK, and arcs to the right are stored in
C   the right portion.  IWF and IWL index the first and last
C   intersecting arcs.
C
      IWF = 1
C
C Top of loop -- set N0 to N1 and NL->NR to the first edge.
C   IWC points to the arc currently being processed.  LFT
C   .LE. 0 iff N0 CSLEFT N1->N2.
C
   10 LFT = 0
      N0 = N1
      X0 = X1
      Y0 = Y1
      Z0 = Z1
      NL = IWK(1,IWF)
      NR = IWK(2,IWF)
      IWC = IWF
C
C   Set NEXT to the node opposite NL->NR unless IWC is the
C     last arc.
C
   11 IF (IWC .EQ. IWL) GO TO 21
      IWCP1 = IWC + 1
      NEXT = IWK(1,IWCP1)
      IF (NEXT .NE. NL) GO TO 16
      NEXT = IWK(2,IWCP1)
C
C   NEXT RIGHT N1->N2 and IWC .LT. IWL.  Test for a possible
C     swap.
C
      IF ( .NOT. CSLEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 14
      IF (LFT .GE. 0) GO TO 12
      IF ( .NOT. CSLEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 14
C
C   Replace NL->NR with N0->NEXT.
C
      CALL CSSWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = N0
      IWK(2,IWC) = NEXT
      GO TO 15
C
C   Swap NL-NR for N0-NEXT, shift columns IWC+1,...,IWL to
C     the left, and store N0-NEXT in the right portion of
C     IWK.
C
   12 CALL CSSWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 13 I = IWCP1,IWL
        IWK(1,I-1) = IWK(1,I)
        IWK(2,I-1) = IWK(2,I)
   13   CONTINUE
      IWK(1,IWL) = N0
      IWK(2,IWL) = NEXT
      IWL = IWL - 1
      NR = NEXT
      GO TO 11
C
C   A swap is not possible.  Set N0 to NR.
C
   14 N0 = NR
      X0 = X(N0)
      Y0 = Y(N0)
      Z0 = Z(N0)
      LFT = 1
C
C   Advance to the next arc.
C
   15 NR = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   NEXT CSLEFT N1->N2, NEXT .NE. N2, and IWC .LT. IWL.
C     Test for a possible swap.
C
   16 IF ( .NOT. CSLEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 19
      IF (LFT .LE. 0) GO TO 17
      IF ( .NOT. CSLEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 19
C
C   Replace NL->NR with NEXT->N0.
C
      CALL CSSWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = NEXT
      IWK(2,IWC) = N0
      GO TO 20
C
C   Swap NL-NR for N0-NEXT, shift columns IWF,...,IWC-1 to
C     the right, and store N0-NEXT in the left portion of
C     IWK.
C
   17 CALL CSSWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 18 I = IWC-1,IWF,-1
        IWK(1,I+1) = IWK(1,I)
        IWK(2,I+1) = IWK(2,I)
   18   CONTINUE
      IWK(1,IWF) = N0
      IWK(2,IWF) = NEXT
      IWF = IWF + 1
      GO TO 20
C
C   A swap is not possible.  Set N0 to NL.
C
   19 N0 = NL
      X0 = X(N0)
      Y0 = Y(N0)
      Z0 = Z(N0)
      LFT = -1
C
C   Advance to the next arc.
C
   20 NL = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   N2 is opposite NL->NR (IWC = IWL).
C
   21 IF (N0 .EQ. N1) GO TO 24
      IF (LFT .LT. 0) GO TO 22
C
C   N0 RIGHT N1->N2.  Test for a possible swap.
C
      IF ( .NOT. CSLEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X2,Y2,Z2) )
     .  GO TO 10
C
C   Swap NL-NR for N0-N2 and store N0-N2 in the right
C     portion of IWK.
C
      CALL CSSWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWL) = N0
      IWK(2,IWL) = N2
      IWL = IWL - 1
      GO TO 10
C
C   N0 CSLEFT N1->N2.  Test for a possible swap.
C
   22 IF ( .NOT. CSLEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X2,Y2,Z2) )
     .  GO TO 10
C
C   Swap NL-NR for N0-N2, shift columns IWF,...,IWL-1 to the
C     right, and store N0-N2 in the left portion of IWK.
C
      CALL CSSWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      I = IWL
   23 IWK(1,I) = IWK(1,I-1)
      IWK(2,I) = IWK(2,I-1)
      I = I - 1
      IF (I .GT. IWF) GO TO 23
      IWK(1,IWF) = N0
      IWK(2,IWF) = N2
      IWF = IWF + 1
      GO TO 10
C
C IWF = IWC = IWL.  Swap out the last arc for N1-N2 and
C   store zeros in IWK.
C
   24 CALL CSSWAP (N2,N1,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = 0
      IWK(2,IWC) = 0
C
C Optimization procedure --
C
      IER = 0
      IF (IWC .GT. 1) THEN
C
C   Optimize the set of new arcs to the left of IN1->IN2.
C
        NIT = 4*(IWC-1)
        CALL CSOPTIM (X,Y,Z,IWC-1, LIST,LPTR,LEND,NIT,
     .              IWK, IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 34
        IF (IERR .EQ. 1) IER = 5
      ENDIF
      IF (IWC .LT. IWEND) THEN
C
C   Optimize the set of new arcs to the right of IN1->IN2.
C
        NIT = 4*(IWEND-IWC)
        CALL CSOPTIM (X,Y,Z,IWEND-IWC, LIST,LPTR,LEND,NIT,
     .              IWK(1,IWC+1), IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 34
        IF (IERR .EQ. 1) GO TO 35
      ENDIF
      IF (IER .EQ. 5) GO TO 35
C
C Successful termination (IER = 0).
C
      RETURN
C
C IN1 and IN2 were adjacent on input.
C
   30 IER = 0
      RETURN
C
C Invalid input parameter.
C
   31 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   32 IER = 2
      RETURN
C
C Invalid triangulation data structure or collinear nodes
C   on convex hull boundary.
C
   33 IER = 3
      WRITE (*,130) IN1, IN2
  130 FORMAT (//5X,'*** Error in CSEDGE:  Invalid triangula',
     .        'tion or null triangles on boundary'/
     .        9X,'IN1 =',I4,', IN2=',I4/)
      RETURN
C
C Error flag (other than 1) returned by CSOPTIM.
C
   34 IER = 4
      WRITE (*,140) NIT, IERR
  140 FORMAT (//5X,'*** Error in CSOPTIM (called from CSEDGE):',
     .        '  NIT = ',I4,', IER = ',I1,' ***'/)
      RETURN
C
C Error flag 1 returned by CSOPTIM.
C
   35 IER = 5
      RETURN
      END
