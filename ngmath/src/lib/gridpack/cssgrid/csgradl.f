C
C	$Id: csgradl.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGRADL (N,K,X,Y,Z,W,LIST,LPTR,LEND, G,IER)
      INTEGER N, K, LIST(*), LPTR(*), LEND(N), IER
      DOUBLE PRECISION X(N), Y(N), Z(N), W(N), G(3)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/24/96
C
C   Given a triangulation of a set of nodes on the unit
C sphere with their associated data values W, this routine
C estimates a gradient vector at node K as follows:  the
C coordinate system is rotated so that K becomes the north
C pole, node K and a set of nearby nodes are projected
C orthogonally onto the X-Y plane (in the new coordinate
C system), a quadratic is fitted in a weighted least squares
C sense to the data values at the projected nodes such that
C the value (associated with K) at (0,0) is interpolated, X
C and Y-partial derivative estimates DX and DY are computed
C by differentiating the quadratic at (0,0), and the esti-
C mated gradient G is obtained by rotating (DX,DY,0) back to
C the original coordinate system.  Note that G lies in the
C plane tangent to the sphere at node K, i.e., G is orthogo-
C nal to the unit vector represented by node K.  A Marquardt
C stabilization factor is used if necessary to ensure a
C well-conditioned least squares system, and a unique solu-
C tion exists unless the nodes are collinear.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 7.
C
C       K = Node at which the gradient is sought.  1 .LE. K
C           .LE. N.
C
C       X,Y,Z = Arrays containing the Cartesian coordinates
C               of the nodes.
C
C       W = Array containing the data values at the nodes.
C           W(I) is associated with (X(I),Y(I),Z(I)) for
C           I = 1,...,N.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       G = X, Y, and Z components (in that order) of the
C           estimated gradient at node K unless IER < 0.
C
C       IER = Error indicator:
C             IER .GE. 6 if no errors were encountered.
C                        IER contains the number of nodes
C                        (including K) used in the least
C                        squares fit.
C             IER = -1 if N or K is outside its valid range.
C             IER = -2 if the least squares system has no
C                      unique solution due to duplicate or
C                      collinear nodes.
C
C STRIPACK module required by CSGRADL:  CSGETNP
C
C SSRFPACK modules required by CSGRADL:  CSAPLYR, CSAPLYRT,
C                                        CSCONSTR, CSGIVENS,
C                                        CSROTATE, CSSETUP
C
C Intrinsic functions called by CSGRADL:  ABS, MIN, DBLE, SQRT
C
C***********************************************************
C
      INTEGER   LMN, LMX
      PARAMETER (LMN=10,  LMX=30)
      INTEGER I, IERR, IM1, IP1, J, JP1, KK, L, LM1, LMAX,
     .        LMIN, LNP, NN, NP, NPTS(LMX)
      DOUBLE PRECISION A(6,6), AV, AVSQ, C, CX, CY, DF,
     .                 DMIN, DTOL, DX, DY, RF, RIN, RTOL, S,
     .                 SF, SUM, SX, SY, WK, WT, XP, YP, ZP
C
      DATA    RTOL/1.D-6/, DTOL/.01D0/, SF/1.D0/
C
C Local parameters:
C
C A =         Transpose of the (upper triangle of the) aug-
C               mented regression matrix
C AV =        Root-mean-square distance (in the rotated
C               coordinate system) between the origin and
C               the nodes (other than K) in the least
C               squares fit.  The first 3 columns of A**T
C               are scaled by 1/AVSQ, the next 2 by 1/AV.
C AVSQ =      AV*AV:  accumulated in SUM
C C,S =       Components of the plane rotation used to
C               triangularize the regression matrix
C CX,SX =     Components of a plane rotation about the X-
C               axis which, together with CY and SY, define
C               a mapping from node K to the north pole
C               (0,0,1)
C CY,SY =     Components of a plane rotation about the Y-
C               axis
C DF =        Negative Z component (in the rotated coordi-
C               nate system) of an element NP of NPTS --
C               increasing function of the angular distance
C               between K and NP.  DF lies in the interval
C               (-1,1).
C DMIN =      Minimum of the magnitudes of the diagonal
C               elements of the triangularized regression
C               matrix
C DTOL =      Tolerance for detecting an ill-conditioned
C               system (DMIN is required to be at least
C               DTOL)
C DX,DY =     X and Y components of the estimated gradient
C               in the rotated coordinate system
C I,J =       Loop indexes
C IERR =      Error flag for calls to CSGETNP (not checked)
C IM1,IP1 =   I-1, I+1
C JP1 =       J+1
C KK =        Local copy of K
C L =         Number of columns of A**T to which a rotation
C               is applied
C LM1 =       LMIN-1
C LMIN,LMAX = Min(LMN,N), Min(LMX,N)
C LMN,LMX =   Minimum and maximum values of LNP for N
C               sufficiently large.  In most cases LMN-1
C               nodes are used in the fit.  7 .LE. LMN .LE.
C               LMX.
C LNP =       Length of NPTS or LMAX+1
C NN =        Local copy of N
C NP =        Element of NPTS to be added to the system
C NPTS =      Array containing the indexes of a sequence of
C               nodes ordered by angular distance from K.
C               NPTS(1)=K and the first LNP-1 elements of
C               NPTS are used in the least squares fit.
C               unless LNP = LMAX+1, NPTS(LNP) determines R
C               (see RIN).
C RF =        Value of DF associated with NPTS(LNP) unless
C               LNP = LMAX+1 (see RIN)
C RIN =       Inverse of a radius of influence R which
C               enters into WT:  R = 1+RF unless all ele-
C               ments of NPTS are used in the fit (LNP =
C               LMAX+1), in which case R is the distance
C               function associated with some point more
C               distant from K than NPTS(LMAX)
C RTOL =      Tolerance for determining LNP (and hence R):
C               if the increase in DF between two successive
C               elements of NPTS is less than RTOL, they are
C               treated as being the same distance from node
C               K and an additional node is added
C SF =        Marquardt stabilization factor used to damp
C               out the first 3 solution components (second
C               partials of the quadratic) when the system
C               is ill-conditioned.  Increasing SF results
C               in more damping (a more nearly linear fit).
C SUM =       Sum of squared Euclidean distances (in the
C               rotated coordinate system) between the
C               origin and the nodes used in the least
C               squares fit
C WK =        W(K) -- data value at node K
C WT =        Weight for the equation coreesponding to NP:
C               WT = (R-D)/(R*D) = 1/D - RIN, where D = 1-ZP
C               is associated with NP
C XP,YP,ZP =  Coordinates of NP in the rotated coordinate
C               system unless ZP < 0, in which case
C               (XP,YP,0) lies on the equator
C
      NN = N
      KK = K
      WK = W(KK)
C
C Check for errors and initialize LMIN, LMAX.
C
      IF (NN .LT. 7  .OR.  KK .LT. 1  .OR.  KK .GT. NN)
     .   GO TO 13
      LMIN = MIN(LMN,NN)
      LMAX = MIN(LMX,NN)
C
C Compute NPTS, LNP, AVSQ, AV, and R.
C   Set NPTS to the closest LMIN-1 nodes to K.  DF contains
C   the negative Z component (in the rotated coordinate
C   system) of the new node on return from CSGETNP.
C
      SUM = 0.
      NPTS(1) = KK
      LM1 = LMIN - 1
      DO 1 LNP = 2,LM1
        CALL CSGETNP (X,Y,Z,LIST,LPTR,LEND,LNP, NPTS, DF,IERR)
        SUM = SUM + 1. - DF*DF
    1   CONTINUE
C
C   Add additional nodes to NPTS until the increase in
C     R = 1+RF is at least RTOL.
C
      DO 2 LNP = LMIN,LMAX
        CALL CSGETNP (X,Y,Z,LIST,LPTR,LEND,LNP, NPTS, RF,IERR)
        IF (RF-DF .GE. RTOL) GO TO 3
        SUM = SUM + 1. - RF*RF
    2   CONTINUE
C
C   Use all LMAX nodes in the least squares fit.  R is
C     arbitrarily increased by 5 percent.
C
      RF = 1.05*RF + .05
      LNP = LMAX + 1
C
C   There are LNP-2 equations corresponding to nodes
C     NPTS(2),...,NPTS(LNP-1).
C
    3 AVSQ = SUM/DBLE(LNP-2)
      AV = SQRT(AVSQ)
      RIN = 1./(1.+RF)
C
C Construct the rotation.
C
      CALL CSCONSTR (X(KK),Y(KK),Z(KK), CX,SX,CY,SY)
C
C Set up the first 5 equations of the augmented regression
C   matrix (transposed) as the columns of A, and zero out
C   the lower triangle (upper triangle of A) with Givens
C   rotations.
C
      DO 5 I = 1,5
        NP = NPTS(I+1)
        CALL CSAPLYR (X(NP),Y(NP),Z(NP),CX,SX,CY,SY, XP,YP,ZP)
        WT = 1./(1.-ZP) - RIN
        CALL CSSETUP (XP,YP,W(NP),WK,AV,AVSQ,WT, A(1,I))
        IF (I .EQ. 1) GO TO 5
        IM1 = I - 1
        DO 4 J = 1,IM1
          JP1 = J + 1
          L = 6 - J
          CALL CSGIVENS ( A(J,J),A(J,I), C,S)
          CALL CSROTATE (L,C,S, A(JP1,J),A(JP1,I) )
    4     CONTINUE
    5   CONTINUE
C
C Add the additional equations to the system using
C   the last column of A.  I .LE. LNP.
C
      I = 7
    6   IF (I .EQ. LNP) GO TO 8
        NP = NPTS(I)
        CALL CSAPLYR (X(NP),Y(NP),Z(NP),CX,SX,CY,SY, XP,YP,ZP)
        WT = 1./(1.-ZP) - RIN
        CALL CSSETUP (XP,YP,W(NP),WK,AV,AVSQ,WT, A(1,6))
        DO 7 J = 1,5
          JP1 = J + 1
          L = 6 - J
          CALL CSGIVENS ( A(J,J),A(J,6), C,S)
          CALL CSROTATE (L,C,S, A(JP1,J),A(JP1,6) )
    7     CONTINUE
        I = I + 1
        GO TO 6
C
C Test the system for ill-conditioning.
C
    8 DMIN = MIN( ABS(A(1,1)),ABS(A(2,2)),ABS(A(3,3)),
     .            ABS(A(4,4)),ABS(A(5,5)) )
      IF (DMIN .GE. DTOL) GO TO 12
      IF (LNP .LE. LMAX) THEN
C
C Add another node to the system and increase R.
C   I = LNP.
C
        LNP = LNP + 1
        IF (LNP .LE. LMAX) CALL CSGETNP (X,Y,Z,LIST,LPTR,LEND,
     .                                 LNP,NPTS, RF,IERR)
        RIN = 1./(1.05*(1.+RF))
        GO TO 6
      ENDIF
C
C Stabilize the system by damping second partials.  Add
C   multiples of the first three unit vectors to the first
C   three equations.
C
      DO 11 I = 1,3
        A(I,6) = SF
        IP1 = I + 1
        DO 9 J = IP1,6
          A(J,6) = 0.
    9     CONTINUE
        DO 10 J = I,5
          JP1 = J + 1
          L = 6 - J
          CALL CSGIVENS ( A(J,J),A(J,6), C,S)
          CALL CSROTATE (L,C,S, A(JP1,J),A(JP1,6) )
   10     CONTINUE
   11   CONTINUE
C
C Test the linear portion of the stabilized system for
C   ill-conditioning.
C
      DMIN = MIN( ABS(A(4,4)),ABS(A(5,5)) )
      IF (DMIN .LT. DTOL) GO TO 14
C
C Solve the 2 by 2 triangular system for the estimated
C   partial derivatives.
C
   12 DY = A(6,5)/A(5,5)
      DX = (A(6,4) - A(5,4)*DY)/A(4,4)/AV
      DY = DY/AV
C
C Rotate the gradient (DX,DY,0) back into the original
C   coordinate system.
C
      CALL CSAPLYRT (DX,DY,CX,SX,CY,SY, G)
      IER = LNP - 1
      RETURN
C
C N or K is outside its valid range.
C
   13 IER = -1
      RETURN
C
C No unique solution due to collinear nodes.
C
   14 IER = -2
      RETURN
      END
