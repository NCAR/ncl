C
C	$Id: cssgrid.f,v 1.12 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSGRID(N,RLATI,RLONI,F,NI,NJ,RLATO,RLONO,FF,
     +                     IWK,WK,IER)
      DOUBLE PRECISION RLATD,RLOND,WK(*),TFVAL,DGMX,STOL,DSMX
      INTEGER N,NI,NJ,IWK(*),IER
      REAL RLATI(N),RLONI(N),F(N),RLATO(NI),RLONO(NJ),FF(NI,NJ)
C
      include 'cscomn.h'
C
C***********************************************************
C
C                               Simplified entry to SSRFPACK
C                                                 Fred Clare
C                                                  NCAR, SCD
C                                                   12/10/99
C
C  This subroutine provides a simple interface to SSRFPACK.
C  This is a single precision version that is nothing more
C  than an interface to the double precision version.
C  It triangulates the input data and does interpolation at
C  the specified grid points.  Certain internal parameters are
C  assigned default values.
C
C  On input:
C
C    N = Number of nodes.  N .GE. 3.
C
C    RLATI,RLONI = Arrays containing lat/lon coordinates of the nodes,
C                  in degrees.
C
C    F     = Array containing data values.  F(I) is associated with
C            (RLATI(I),RLON(I)) for I = 1 to N.
C
C    NI,NJ = Number of rows and columns in the uniform grid.
C            1 .LE. NI .LE. NROW and 1 .LE. NJ.
C
C    RLATO,RLONO = Arrays of length NI and NJ, respectively, containing
C                  the latitudes and longitudes of the grid lines, in
C                  degrees.
C
C    IWK  = An integer workspace of length 27*N.
C
C           The workspace is used as follows:
C
C             IWK(     1)-IWK( 6*N)  --  LIST.
C             IWK( 6*N+1)-IWK(12*N)  --  LPTR.
C             IWK(12*N+1)-IWK(13*N)  --  LEND.
C             IWK(13*N+1)-IWK(14*N)  --  NEAR.
C             IWK(14*N+1)-IWK(15*N)  --  NEXT.
C
C    WK   = A *DOUBLE PRECISION* workspace of length 13*N.
C           This workspace must be double precision since this
C           routine is simply a single precision interface to the
C           primary double precision code.
C
C           The workspace is used as follows:
C
C             WK(     1)-WK(   N)  --  Double precision version of F.
C             WK(   N+1)-WK( 2*N)  --  Double precision version of X,
C                                      where (X(I),Y(I),Z(I)) is the
C                                      Cartestian coordinate on the unit
C                                      sphere equivalent to (PLAT(I),PLON(I)).
C             WK( 2*N+1)-WK( 3*N)  --  Double precision version of Y.
C             WK( 3*N+1)-WK( 4*N)  --  Double precision version of Z.
C             WK( 4*N+1)-WK(10*N)  --  First used to store the radian
C                                      equivalent to RLATI, then used for
C                                      double precision version of SIGMA.
C             WK(10*N+1)-WK(13*N)  --  First used to store the radian
C                                      equivalent to RLONI, then used for
C                                      double precision version of GRAD.
C
C  On output:
C
C    FF   = Interpolated values at the grid points if IER .GE. 0 .
C           FF(I,J) = F(PLAT(I),PLON(J)) for I = 1,...,NI and
C           J = 1,...,NJ.
C
C    IER = Error indicator:
C             =  0 - no error.
C             =  1 - invalid number of input points (must be
C                    greater than 3).
C             =  4 - first three nodes are collinear.
C             =  6 - internal algorithm error - please report this.
C             = 10 - insufficient space for the triangulation
C                    (must be >= number of boundary nodes minus 2).
C             = 11 - degenerate triangle (two vertices lie on
C                    same geodesic).
C             = -L - coordinates L and M coincide for some
C                    M  > L >= 1 (coordinate numbering
C                    starting at 1).
C
C***********************************************************
C
C  Scale factor for converting from degrees to radians.
C
      PARAMETER (D2R=0.017453293)
C
C  Parameters for random number usage.
C
      PARAMETER (EPSILON=0.00001,IRMAX=32767)
C
C  Use pre-calculated estimated gradients.
C
      PARAMETER (IFLGG=1)
C
      INTEGER I,J,IERR,IST,NN,NST,NX,NY
      DATA NST/1/
      DATA IX,IY,IZ/1,2,3/
C
C Local parameters:
C
C I,J =   DO-loop indexes
C IERR =  Error flag for calls to CSGRADL and CSINTRC1
C IST =   Parameter for CSINTRC1
C NN =    Local copy of N
C NST =   Initial value for IST
C NX,NY = Local copies of NI and NJ
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL CSBLDA
C
C  Zero out the workspaces.
C
      DO 11 I=1,27*N
        IWK(I) = 0
   11 CONTINUE
      DO 12 I=1,13*N
        WK(I) = 0.D0
   12 CONTINUE
C
      IF (N .LT. 4) GO TO 110
      NN = N
      NNEW = N
      NX = NI
      NY = NJ
      IF (NN.LT.3) THEN
          GO TO 110
      ELSE IF (NX.LT.1) THEN
          GO TO 200
      ELSE IF (NY.LT.1) THEN
          GO TO 210
      END IF
C
C  Convert RLATI and RLONI to radians.
C
      DO 7 I=1,NN
        WK(4*N+I) = DBLE(D2R*RLATI(I))
        WK(5*N+I) = DBLE(D2R*RLONI(I))
    7 CONTINUE
C
C  Then convert to Cartesian coordinates.
C
      CALL CSTRANSD(NN,WK(4*N+1),WK(5*N+1),WK(N+1),WK(2*N+1),WK(3*N+1))        
C
      IST = NST
C
C  Do the triangulation.
C
      CALL CSTRMESH(NNEW,WK(N+1),WK(2*N+1),WK(3*N+1),IWK(1),IWK(6*NN+1),
     +              IWK(12*NN+1),LNEW,IWK(13*N+1),IWK(14*N+1),WK(1),IER)
      IF (IER.EQ.0) THEN
          GO TO 100
      ELSE IF (IER.EQ.-1) THEN
          GO TO 110
      ELSE IF (IER.EQ.-2) THEN
          GO TO 120
      ELSE IF (IER.EQ.-3) THEN
          GO TO 130
      ELSE IF (IER.GT.0) THEN
          NERR = -IER
          GO TO 140
      ELSE
          GO TO 130
      END IF
C
  100 CONTINUE
C
C  Copy the input values into a double precision array.
C
      DO 4 I=1,NN
        WK(I) = DBLE(F(I))
    4 CONTINUE 
C
C  Set up the SIGMA array.
C
      IF (ICSIG .EQ. 0) THEN
        IFLGS = 1
C
C  Zero SIGMA array.
C
        DO 70 I=1,6*NN
          WK(4*NN+I) = 0.
   70   CONTINUE
      ELSE
C
C  Constant SIGMA.
C
        WK(4*N+1) = USSIG
        IFLGS = 0
      ENDIF
C
C  Zero GRAD array.
C
      DO 90 I = 1,3*NN
        WK(10*NN+I) = 0.
   90 CONTINUE
C
C  Iterate to get global gradient estimates.  The number of iterations
C  is chosen as a fixed number here.  If a constant SIGMA is
C  chosen, then no interation is done.  This loop could be changed to
C  exit when the difference between two successive gradient
C  estimates yields a maximum difference less than some tolerance,
C  like 0.01 .  This would require storing the estimated gradients
C  for the comparison.
C
      IF (ICSIG .NE. 0) THEN 
        ITERMAX = 1
      ELSE
        ITERMAX = 6
      ENDIF
C
      DO 80 ITER=1,ITERMAX
        NITG = NUMIT
        DGMX = TOLIC
        STOL = TOLSG 
C
C  Use local or global gradients depending on IGFLG.
C
        IF (IGFLG .EQ. 1) THEN
          CALL CSGRADG(NN,WK(N+1),WK(2*N+1),WK(3*N+1),WK(1),IWK(1),
     +                 IWK(6*N+1),IWK(12*N+1),IFLGS,WK(4*N+1),NITG,
     +                 DGMX,WK(10*N+1),IER)
          IF (IER .EQ. -1) THEN
            CALL CSSERR('CSGRADG',13)
            RETURN
          ELSE IF (IER .EQ. -2) THEN
            CALL CSSERR('CSGRADG',4)
            RETURN
          ELSE IF (IER .EQ. -3) THEN
            CALL CSSERR('CSGRADG',12)
            RETURN
          ELSE IF (IER .EQ. 1) THEN
            IER = 0
          ENDIF
        ELSE
          DO 8 K=1,NN
            CALL CSGRADL(NN,K,WK(N+1),WK(2*N+1),WK(3*N+1),WK(1),
     +                   IWK(1),IWK(6*N+1),IWK(12*N+1),
     +                   WK(10*N+3*(K-1)+1),IER)
            IF (IER .EQ. -1) THEN
              CALL CSSERR('CSGRADL',1)
              RETURN
            ELSE IF (IER .EQ. -2) THEN
              CALL CSSERR('CSGRADL',4)
              RETURN
            ELSE IF (IER .GE. 6) THEN
              IER = 0
            ENDIF
    8     CONTINUE
        ENDIF
C
C  Calculate SIGMAs if requested.
C
        IF (ICSIG .EQ. 0) THEN
          CALL CSGETSIG(NN,WK(N+1),WK(2*N+1),WK(3*N+1),WK(1),IWK(1),
     +                  IWK(6*N+1),IWK(12*N+1),WK(10*N+1),STOL,
     +                  WK(4*N+1),DSMX,IER)
          IF (IER .EQ. -1) THEN
            CALL CSSERR('CSGETSIG',1)
            RETURN
          ELSE IF (IER .EQ. -2) THEN
            CALL CSSERR('CSGETSIG',12)
            RETURN
          ELSE IF (IER .GE. 0) THEN
            IER = 0
          ENDIF
        ENDIF
   80 CONTINUE
C
C Compute uniform grid points and interpolated values.
C
      DO 3 J = 1,NY
        RLOND = D2R*RLONO(J)          
        DO 2 I = 1,NX
          RLATD = D2R*RLATO(I)          
          CALL CSINTRC1(NNEW,RLATD,RLOND,WK(N+1),WK(2*N+1),
     +                  WK(3*N+1),WK(1),IWK(1),IWK(6*NN+1),
     +                  IWK(12*NN+1),IFLGS,WK(4*N+1),IFLGG,WK(10*N+1),
     +                  IST,TFVAL,IERR)
          IF (IERR.EQ.0) THEN
              FF(I,J) = REAL(TFVAL)
              GO TO 2
          ELSE IF (IERR.EQ.1) THEN
              GO TO 110
          ELSE IF (IERR.EQ.4) THEN
              GO TO 120
          ELSE IF (IERR.EQ.7) THEN
              GO TO 170
          ELSE IF (IERR.EQ.8) THEN
              GO TO 180
          ELSE IF (IERR.EQ.9) THEN
              GO TO 190
          ELSE
              GO TO 130
          END IF
    2   CONTINUE
    3 CONTINUE
      RETURN
C
  110 CONTINUE
      IER = 1
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  120 CONTINUE
      IER = 4
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  130 CONTINUE
      IER = 6
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  140 CONTINUE
      IER = NERR
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  150 CONTINUE
      IER = 10
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  160 CONTINUE
      IER = 11
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  170 CONTINUE
      IER = 7
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  180 CONTINUE
      IER = 8
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  190 CONTINUE
      IER = 9
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  200 CONTINUE
      IER = 2
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
  210 CONTINUE
      IER = 3
      CALL CSSERR('CSSGRID',IER)
      RETURN
C
      END
