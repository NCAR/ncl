C
C	$Id: csoptim.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSOPTIM (X,Y,Z,NA, LIST,LPTR,LEND,NIT,
     .                  IWK, IER)
      INTEGER NA, LIST(*), LPTR(*), LEND(*), NIT, IWK(2,NA),
     .        IER
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
C   Given a set of NA triangulation arcs, this subroutine
C optimizes the portion of the triangulation consisting of
C the quadrilaterals (pairs of adjacent triangles) which
C have the arcs as diagonals by applying the circumcircle
C test and appropriate swaps to the arcs.
C
C   An iteration consists of applying the swap test and
C swaps to all NA arcs in the order in which they are
C stored.  The iteration is repeated until no swap occurs
C or NIT iterations have been performed.  The bound on the
C number of iterations may be necessary to prevent an
C infinite loop caused by cycling (reversing the effect of a
C previous swap) due to floating point inaccuracy when four
C or more nodes are nearly cocircular.
C
C
C On input:
C
C       X,Y,Z = Arrays containing the nodal coordinates.
C
C       NA = Number of arcs in the set.  NA .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C       NIT = Maximum number of iterations to be performed.
C             NIT = 4*NA should be sufficient.  NIT .GE. 1.
C
C       IWK = Integer array dimensioned 2 by NA containing
C             the nodal indexes of the arc endpoints (pairs
C             of endpoints are stored in columns).
C
C On output:
C
C       LIST,LPTR,LEND = Updated triangulation data struc-
C                        ture reflecting the swaps.
C
C       NIT = Number of iterations performed.
C
C       IWK = Endpoint indexes of the new set of arcs
C             reflecting the swaps.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if a swap occurred on the last of
C                     MAXIT iterations, where MAXIT is the
C                     value of NIT on input.  The new set
C                     of arcs is not necessarily optimal
C                     in this case.
C             IER = 2 if NA < 0 or NIT < 1 on input.
C             IER = 3 if IWK(2,I) is not a neighbor of
C                     IWK(1,I) for some I in the range 1
C                     to NA.  A swap may have occurred in
C                     this case.
C             IER = 4 if a zero pointer was returned by
C                     Subroutine CSSWAP.
C
C Modules required by CSOPTIM:  CSLSTPTR, CSSWAP, CSSWPTST
C
C Intrinsic function called by CSOPTIM:  ABS
C
C***********************************************************
C
      INTEGER I, IO1, IO2, ITER, LP, LP21, LPL, LPP, MAXIT,
     .        N1, N2, NNA
      LOGICAL CSSWPTST
      LOGICAL SWP
C
C Local parameters:
C
C I =       Column index for IWK
C IO1,IO2 = Nodal indexes of the endpoints of an arc in IWK
C ITER =    Iteration count
C LP =      LIST pointer
C LP21 =    Parameter returned by CSSWAP (not used)
C LPL =     Pointer to the last neighbor of IO1
C LPP =     Pointer to the node preceding IO2 as a neighbor
C             of IO1
C MAXIT =   Input value of NIT
C N1,N2 =   Nodes opposite IO1->IO2 and IO2->IO1,
C             respectively
C NNA =     Local copy of NA
C SWP =     Flag set to TRUE iff a swap occurs in the
C             optimization loop
C
      NNA = NA
      MAXIT = NIT
      IF (NNA .LT. 0  .OR.  MAXIT .LT. 1) GO TO 7
C
C Initialize iteration count ITER and test for NA = 0.
C
      ITER = 0
      IF (NNA .EQ. 0) GO TO 5
C
C Top of loop --
C   SWP = TRUE iff a swap occurred in the current iteration.
C
    1 IF (ITER .EQ. MAXIT) GO TO 6
      ITER = ITER + 1
      SWP = .FALSE.
C
C   Inner loop on arcs IO1-IO2 --
C
      DO 4 I = 1,NNA
        IO1 = IWK(1,I)
        IO2 = IWK(2,I)
C
C   Set N1 and N2 to the nodes opposite IO1->IO2 and
C     IO2->IO1, respectively.  Determine the following:
C
C     LPL = pointer to the last neighbor of IO1,
C     LP = pointer to IO2 as a neighbor of IO1, and
C     LPP = pointer to the node N2 preceding IO2.
C
        LPL = LEND(IO1)
        LPP = LPL
        LP = LPTR(LPP)
    2   IF (LIST(LP) .EQ. IO2) GO TO 3
          LPP = LP
          LP = LPTR(LPP)
          IF (LP .NE. LPL) GO TO 2
C
C   IO2 should be the last neighbor of IO1.  Test for no
C     arc and bypass the swap test if IO1 is a boundary
C     node.
C
        IF (ABS(LIST(LP)) .NE. IO2) GO TO 8
        IF (LIST(LP) .LT. 0) GO TO 4
C
C   Store N1 and N2, or bypass the swap test if IO1 is a
C     boundary node and IO2 is its first neighbor.
C
    3   N2 = LIST(LPP)
        IF (N2 .LT. 0) GO TO 4
        LP = LPTR(LP)
        N1 = ABS(LIST(LP))
C
C   Test IO1-IO2 for a swap, and update IWK if necessary.
C
        IF ( .NOT. CSSWPTST(N1,N2,IO1,IO2,X,Y,Z) ) GO TO 4
        CALL CSSWAP (N1,N2,IO1,IO2, LIST,LPTR,LEND, LP21)
        IF (LP21 .EQ. 0) GO TO 9
        SWP = .TRUE.
        IWK(1,I) = N1
        IWK(2,I) = N2
    4   CONTINUE
      IF (SWP) GO TO 1
C
C Successful termination.
C
    5 NIT = ITER
      IER = 0
      RETURN
C
C MAXIT iterations performed without convergence.
C
    6 NIT = MAXIT
      IER = 1
      RETURN
C
C Invalid input parameter.
C
    7 NIT = 0
      IER = 2
      RETURN
C
C IO2 is not a neighbor of IO1.
C
    8 NIT = ITER
      IER = 3
      RETURN
C
C Zero pointer returned by CSSWAP.
C
    9 NIT = ITER
      IER = 4
      RETURN
      END
