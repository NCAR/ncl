C
C       $Id: shqshep.f,v 1.6 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHQSHEP (N,X,Y,Z,F,NQ,NW,NR, LCELL,LNEXT,
     +                   XYZMIN,XYZDEL,RMAX,RSQ,A,IER)
      INTEGER N, NQ, NW, NR, LCELL(NR,NR,NR), LNEXT(N), IER
      REAL    X(N), Y(N), Z(N), F(N), XYZMIN(3), XYZDEL(3),
     +        RMAX, RSQ(N), A(9,N)
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C                                                   01/08/90
C
C  This subroutine computes a set of parameters A and RSQ
C  defining a smooth (once continuously differentiable) tri-
C  variate function Q(X,Y,Z) which interpolates data values
C  F at scattered nodes (X,Y,Z).  The interpolant Q may be
C  evaluated at an arbitrary point by function SH3VAL, and
C  its first derivatives are computed by subroutine SH3GRD.
C  The interpolation scheme is a modified quadratic Shepard
C  method --
C
C  Q = (W(1)*Q(1)+W(2)*Q(2)+..+W(N)*Q(N))/(W(1)+W(2)+..+W(N))
C
C  For trivariate functions W(K) and Q(K).  The nodal functions
C  are given by
C
C  Q(K)(X,Y,Z) = A(1,K)*DX**2 + A(2,K)*DX*DY + A(3,K)*DY**2 +
C                A(4,K)*DX*DZ + A(5,K)*DY*DZ + A(6,K)*DZ**2 +
C                A(7,K)*DX + A(8,K)*DY + A(9,K)*DZ + F(K)
C
C  where DX = (X-X(K)), DY = (Y-Y(K)), and DZ = (Z-Z(K)).
C  Thus, Q(K) is a quadratic function which interpolates the
C  data value at node K.  Its coefficients A(,K) are obtained
C  by a weighted least squares fit to the closest NQ data
C  points with weights similar to W(K).  Note that the radius
C  of influence for the least squares fit is fixed for each
C  K, but varies with K.
C
C  The weights are taken to be
C
C      W(K)(X,Y,Z) = ( (R(K)-D(K))+ / R(K)*D(K) )**2
C
C  where (R(K)-D(K))+ = 0 if R(K) .LE. D(K), and D(K)(X,Y,Z)
C  is the Euclidean distance between (X,Y,Z) and node K.  The
C  radius of influence R(K) varies with K and is chosen so
C  that NW nodes are within the radius.  Note that W(K) is
C  not defined at node (X(K),Y(K),Z(K)), but Q(X,Y,Z) has
C  limit F(K) as (X,Y,Z) approaches (X(K),Y(K),Z(K)).
C
C  ON INPUT --
C
C    N = Number of nodes and associated data values.
C        N .GE. 10.
C
C    X,Y,Z = Arrays of length N containing the Cartesian
C            coordinates of the nodes.
C
C    F = Array of length N containing the data values
C        in one-to-one correspondence with the nodes.
C
C    NQ = Number of data points to be used in the least
C         squares fit for coefficients defining the nodal
C         functions Q(K).  A recommended value is NQ =
C         17.  9 .LE. NQ .LE. MIN(40,N-1).
C
C    NW = Number of nodes within (and defining) the radii
C         of influence R(K) which enter into the weights
C         W(K).  For N sufficiently large, a recommended
C         value is NW = 32.  1 .LE. NW .LE. MIN(40,N-1).
C
C    NR = Number of rows, columns, and planes in the cell
C         grid defined in subroutine SHSTORE3.  A box con-
C         taining the nodes is partitioned into cells in
C         order to increase search efficiency.  NR =
C         (N/3)**(1/3) is recommended.  NR .GE. 1.
C
C  The above parameters are not altered by this routine.
C
C    LCELL = Array of length .GE. NR**3.
C
C    LNEXT = Array of length .GE. N.
C
C    XYZMIN,XYZDEL = Arrays of length .GE. 3.
C
C    RSQ = Array of length .GE. N.
C
C    A = Array of length .GE. 9N.
C
C  ON OUTPUT --
C
C    LCELL = NR by NR by NR array of nodal indices associated
C            with cells.  Refer to SHSTORE3.
C
C    LNEXT = Array of length N containing next-node indices.
C            Refer to SHSTORE3.
C
C    XYZMIN,XYZDEL = Arrays of length 3 containing minimum
C                    nodal coordinates and cell dimensions,
C                    respectively.  Refer to SHSTORE3.
C
C    RMAX = Square root of the largest element in RSQ --
C           maximum radius R(K).
C
C    RSQ = Array containing the squares of the radii R(K)
C          which enter into the weights W(K).
C
C    A = 9 by N array containing the coefficients for
C        quadratic nodal function Q(K) in column K.
C
C  Note that the above output parameters are not defined
C  unless IER = 0.
C
C    IER = Error indicator --
C      IER = 3 - NQ must > 9
C      IER = 4 - NW must be at least 1
C      IER = 5 - must have N < NQ < 41
C      IER = 6 - must have N < NW < 41
C      IER = 7 - cell grid dimensions must be positive
C      IER = 8 - duplicate input points encountered
C      IER = 9 - collinear input, no unique solution
C
C  MODULES REQUIRED BY SHQSHEP -- SHGETNP3, SHGIVENS, SHROT,
C                                SHSETUP, SHSTORE3
C
C  INTRINSIC FUNCTIONS CALLED BY SHQSHEP -- ABS, AMIN1, FLOAT,
C                                          MAX0, MIN0, SQRT
C
C***********************************************************
C
      INTEGER I, IB, IERR, IP1, IRM1, IROW, J, JP1, K, LMAX,
     +        LNP, NEQ, NN, NNQ, NNR, NNW, NP, NPTS(40),
     +        NQWMAX
      REAL    AV, AVSQ, B(10,10), C, DMIN, DTOL, FK, RQ, RS,
     +        RSMX, RSOLD, RTOL, RWS, S, SF, SUM, T, XK, YK,
     +        ZK, XYZDL(3), XYZMN(3)
C
      DATA    RTOL/1.E-5/, DTOL/.01/, SF/1./
C
C  LOCAL PARAMETERS --
C
C  AV =     Root-mean-square distance between K and the
C           nodes in the least squares system (unless
C           additional nodes are introduced for stability).
C           The first 6 columns of the matrix are scaled
C           by 1/AVSQ, the last 3 by 1/AV.
C  AVSQ =   AV*AV
C  B =      Transpose of the augmented regression matrix.
C  C =      First component of the plane rotation used to
C           zero the lower triangle of B**T -- computed
C           by subroutine SHGIVENS
C  DMIN =   Minimum of the magnitudes of the diagonal
C           elements of the regression matrix after
C           zeros are introduced below the diagonal
C  DTOL =   Tolerance for detecting an ill-conditioned
C           system.  The system is accepted when DMIN .GE. DTOL
C  FK =     Data value at node K -- F(K)
C  I =      Index for A, B, NPTS, XYZMIN, XYZMN, XYZDEL, and XYZDL
C  IB =     Do-loop index for back solve
C  IERR =   Error flag for the call to SHSTORE3
C  IP1 =    I+1
C  IRM1 =   IROW-1
C  IROW =   Row index for B
C  J =      Index for A and B
C  JP1 =    J+1
C  K =      Nodal function index and column index for A
C  LMAX =   Maximum number of NPTS elements (must be consistent
C           with the dimension statement above)
C  LNP =    Current length of NPTS
C  NEQ =    Number of equations in the least squares fit
C  NN =     Local copy of N
C  NNQ =    Local copy of NQ
C  NNR =    Local copy of NR
C  NNW =    Local copy of NW
C  NP =     NPTS element
C  NPTS =   Array containing the indices of a sequence of
C           nodes to be used in the least squares fit
C           or to compute RSQ.  The nodes are ordered by
C           distance from K and the last element
C           (usually indexed by LNP) is used only to
C           determine RQ, or RSQ(K) if NW .GT. NQ
C  NQWMAX = MAX(NQ,NW)
C  RQ =     Radius of influence which enters into the
C           weights for Q(K) (see subroutine SHSETUP)
C  RS =     Squared distance between K and NPTS(LNP) --
C           used to compute RQ and RSQ(K)
C  RSMX =   Maximum RSQ element encountered
C  RSOLD =  Squared distance between K and NPTS(LNP-1) --
C           used to compute a relative change in RS
C           between succeeding NPTS elements
C  RTOL =   Tolerance for detecting a sufficiently large
C           relative change in RS.  If the change is
C           not greater than RTOL, the nodes are
C           treated as being the same distance from K
C  RWS =    Current value of RSQ(K)
C  S =      Second component of the plane Givens rotation
C  SF =     Marquardt stabilization factor used to damp
C           out the first 6 solution components (second
C           partials of the quadratic) when the system
C           is ill-conditioned.  As SF increases, the
C           fitting function approaches a linear
C  SUM =    Sum of squared Euclidean distances between
C           node K and the nodes used in the least
C           squares fit (unless additional nodes are
C           added for stability)
C  T =      Temporary variable for accumulating a scalar
C           product in the back solve
C  XK =     Coordinate of node K -- X(K)
C  YK =     Coordinate of node K -- Y(K)
C  ZK =     Coordinate of node K -- Z(K)
C  XYZDL =  Local variables for XYZDEL
C  XYZMN =  Local variables for XYZMIN
C
      NN = N
      NNQ = NQ
      NNW = NW
      NNR = NR
      NQWMAX = MAX0(NNQ,NNW)
      LMAX = MIN0(40,NN-1)
      IF (9 .GT. NNQ) THEN
        GO TO 30
      ELSE IF (1 .GT. NNW) THEN
        GO TO 31
      ELSE IF (NNQ .GT. LMAX) THEN
        GO TO 32
      ELSE IF (NNW .GT. LMAX) THEN
        GO TO 33
      ELSE IF (NNR .LT. 1) THEN
        GO TO 34
      ENDIF
C
C  Create the cell data structure, and initialize RSMX.
C
      CALL SHSTORE3 (NN,X,Y,Z,NNR, LCELL,LNEXT,XYZMN,XYZDL,
     +             IERR)
      IF (IERR .NE. 0) GO TO 22
      RSMX = 0.
C
C  Outer loop on node K
C
      DO 18 K = 1,NN
        XK = X(K)
        YK = Y(K)
        ZK = Z(K)
        FK = F(K)
C
C  Mark node K to exclude it from the search for nearest neighbors.
C
        LNEXT(K) = -LNEXT(K)
C
C  Initialize for loop on NPTS.
C
        RS = 0.
        SUM = 0.
        RWS = 0.
        RQ = 0.
        LNP = 0
C
C  Compute NPTS, LNP, RWS, NEQ, RQ, and AVSQ.
C
    1   SUM = SUM + RS
          IF (LNP .EQ. LMAX) GO TO 3
          LNP = LNP + 1
          RSOLD = RS
          CALL SHGETNP3 (XK,YK,ZK,X,Y,Z,NNR,LCELL,LNEXT,XYZMN,
     +                 XYZDL, NP,RS)
          IF (RS .EQ. 0.) GO TO 21
          NPTS(LNP) = NP
          IF ( (RS-RSOLD)/RS .LT.  RTOL ) GO TO 1
          IF (RWS .EQ. 0.  .AND.  LNP .GT. NNW) RWS = RS
          IF (RQ .NE. 0.  .OR.  LNP .LE. NNQ) GO TO 2
C
C  RQ = 0 (not yet computed) and LNP .GT. NQ.  RQ =
C  SQRT(RS) is sufficiently large to (strictly) include
C  NQ nodes.  The least squares fit will include NEQ =
C  LNP-1 equations for 9 .LE. NQ .LE. NEQ .LT. LMAX .LE. N-1.
C
          NEQ = LNP - 1
          RQ = SQRT(RS)
          AVSQ = SUM/FLOAT(NEQ)
C
C  Bottom of loop -- test for termination.
C
    2     IF (LNP .GT. NQWMAX) GO TO 4
          GO TO 1
C
C  All LMAX nodes are included in NPTS.  RWS and/or RQ**2 is
C  (arbitrarily) taken to be 10 percent larger than the
C  distance RS to the last node included.
C
    3   IF (RWS .EQ. 0.) RWS = 1.1*RS
        IF (RQ .NE. 0.) GO TO 4
          NEQ = LMAX
          RQ = SQRT(1.1*RS)
          AVSQ = SUM/FLOAT(NEQ)
C
C  Store RSQ(K), update RSMX if necessary, and compute AV.
C
    4   RSQ(K) = RWS
        IF (RWS .GT. RSMX) RSMX = RWS
        AV = SQRT(AVSQ)
C
C  Set up the augmented regression matrix (transposed) as the
C  columns of B, and zero out the lower triangle (upper
C  triangle of B) with Givens rotations -- QR decomposition
C  with orthogonal matrix Q not stored.
C
        I = 0
    5     I = I + 1
          NP = NPTS(I)
          IROW = MIN0(I,10)
          CALL SHSETUP (XK,YK,ZK,FK,X(NP),Y(NP),Z(NP),F(NP),
     +                 AV,AVSQ,RQ, B(1,IROW))
          IF (I .EQ. 1) GO TO 5
          IRM1 = IROW-1
          DO 6 J = 1,IRM1
            JP1 = J + 1
            CALL SHGIVENS (B(J,J),B(J,IROW),C,S)
    6       CALL SHROT (10-J,C,S,B(JP1,J),B(JP1,IROW))
          IF (I .LT. NEQ) GO TO 5
C
C  Test the system for ill-conditioning.
C
        DMIN = AMIN1( ABS(B(1,1)),ABS(B(2,2)),ABS(B(3,3)),
     +                ABS(B(4,4)),ABS(B(5,5)),ABS(B(6,6)),
     +                ABS(B(7,7)),ABS(B(8,8)),ABS(B(9,9)) )
        IF (DMIN*RQ .GE. DTOL) GO TO 13
        IF (NEQ .EQ. LMAX) GO TO 10
C
C  Increase RQ and add another equation to the system to
C  improve the conditioning.  The number of NPTS elements
C  is also increased if necessary.
C
    7   RSOLD = RS
        NEQ = NEQ + 1
        IF (NEQ .EQ. LMAX) GO TO 9
        IF (NEQ .EQ. LNP) GO TO 8
C
C  NEQ .LT. LNP
C
        NP = NPTS(NEQ+1)
        RS = (X(NP)-XK)**2 + (Y(NP)-YK)**2 + (Z(NP)-ZK)**2
        IF ( (RS-RSOLD)/RS .LT. RTOL ) GO TO 7
        RQ = SQRT(RS)
        GO TO 5
C
C  Add an element to NPTS.
C
    8   LNP = LNP + 1
        CALL SHGETNP3 (XK,YK,ZK,X,Y,Z,NNR,LCELL,LNEXT,XYZMN,
     +               XYZDL, NP,RS)
        IF (NP .EQ. 0) GO TO 21
        NPTS(LNP) = NP
        IF ( (RS-RSOLD)/RS .LT. RTOL ) GO TO 7
        RQ = SQRT(RS)
        GO TO 5
C
    9   RQ = SQRT(1.1*RS)
        GO TO 5
C
C  Stabilize the system by damping second partials -- add
C  multiples of the first six unit vectors to the first
C  six equations.
C
   10   DO 12 I = 1,6
          B(I,10) = SF
          IP1 = I + 1
          DO 11 J = IP1,10
   11       B(J,10) = 0.
          DO 12 J = I,9
            JP1 = J + 1
            CALL SHGIVENS (B(J,J),B(J,10),C,S)
   12       CALL SHROT (10-J,C,S,B(JP1,J),B(JP1,10))
C
C  Test the stabilized system for ill-conditioning.
C
        DMIN = AMIN1( ABS(B(1,1)),ABS(B(2,2)),ABS(B(3,3)),
     +                ABS(B(4,4)),ABS(B(5,5)),ABS(B(6,6)),
     +                ABS(B(7,7)),ABS(B(8,8)),ABS(B(9,9)) )
        IF (DMIN*RQ .LT. DTOL) GO TO 22
C
C  Solve the 9 by 9 triangular system for the coefficients
C
   13   DO 15 IB = 1,9
          I = 10-IB
          T = 0.
          IF (I .EQ. 9) GO TO 15
          IP1 = I + 1
          DO 14 J = IP1,9
   14       T = T + B(J,I)*A(J,K)
   15     A(I,K) = (B(10,I)-T)/B(I,I)
C
C  Scale the coefficients to adjust for the column scaling.
C
        DO 16 I = 1,6
   16     A(I,K) = A(I,K)/AVSQ
        A(7,K) = A(7,K)/AV
        A(8,K) = A(8,K)/AV
        A(9,K) = A(9,K)/AV
C
C  Unmark k and the elements of NPTS.
C
        LNEXT(K) = -LNEXT(K)
        DO 17 I = 1,LNP
          NP = NPTS(I)
   17     LNEXT(NP) = -LNEXT(NP)
   18   CONTINUE
C
C  No errors encountered.
C
      DO 19 I = 1,3
        XYZMIN(I) = XYZMN(I)
   19   XYZDEL(I) = XYZDL(I)
      RMAX = SQRT(RSMX)
      IER = 0
      RETURN
C
C  N, NQ, NW, or NR is out of range.
C
   30 CONTINUE
      CALL SHERR (3,'SHQSHEP - number of data points used in least squar
     +es fit must be > 9',69)
      IER = 3
      RETURN
   31 CONTINUE
      CALL SHERR (4,'SHQSHEP - NFL (number of points used to calculate w
     +eights) must be at least 1',76)
      IER = 4
      RETURN
   32 CONTINUE
      CALL SHERR (5,'SHQSHEP - number of points used in least squares fi
     +t too large',61)
      IER = 5
      RETURN
   33 CONTINUE
      CALL SHERR (6,'SHQSHEP - number of points used in calculating weig
     +hts too large',63)
      IER = 6
      RETURN
   34 CONTINUE
      CALL SHERR (7,'SHQSHEP - cell grid dimensions must be positive',
     +            47)
      IER = 7
      RETURN
C
C  Duplicate nodes were encountered by SHGETNP3.
C
   21 CONTINUE
      CALL SHERR (8,'SHQSHEP - duplicate input points encountered',44)
      IER = 8
      RETURN
C
C  No unique solution due to collinear nodes.
C
   22 CONTINUE
      DO 23 I = 1,3
        XYZMIN(I) = XYZMN(I)
        XYZDEL(I) = XYZDL(I)
   23 CONTINUE
      CALL SHERR (9,'SHQSHEP - collinear input, no unique solution',45)       
      IER = 9
C
      RETURN
      END
