      SUBROUTINE CSSGRID (N,X,Y,Z,F, NI,NJ,PLAT,PLON,FF, IWK,WK,IER)
      INTEGER N, NI, NJ, IWK(*), IER
      REAL    X(N), Y(N), Z(N), F(N), PLAT(NI), PLON(NJ), 
     +        FF(NI,NJ),  WK(N)
C
C***********************************************************
C
C                               Simplified entry to SSRFPACK
C                                                 Fred Clare
C                                                  NCAR, SCD
C                                                   03/04/99
C
C  This subroutine provides a simple interface to SSRFPACK.
C  It triangulates the input data and does interpolation at
C  the specified grid points.  Certain internal parameters are
C  assigned default values.
C
C  On input:
C
C    N = Number of nodes.  N .GE. 3.
C
C    X,Y,Z = Arrays containing Cartesian coordinates of the nodes.  
C            X(I)**2 + Y(I)**2 + Z(I)**2 = 1 for I = 1 to N.
C
C    F     = Array containing data values.  F(I) is associated
C            (X(I),Y(I),Z(I)).
C
C    NI,NJ = Number of rows and columns in the uniform grid.  
C            1 .LE. NI .LE. NROW and 1 .LE. NJ.
C
C    PLAT,PLON = Arrays of length NI and NJ, respectively, containing 
C                the latitudes and longitudes of the grid lines.
C
C    FF   = NROW by NCOL array with NROW .GE. NI and NCOL .GE. NJ.
C
C    IWK  = An integer workspace of length 15*N.
C
C    RWK  = A real workspace of length N.
C
C  On output:
C
C    FF   = Interpolated values at the grid points if IER .GE. 0 .
C           FF(I,J) = F(PLAT(I),PLON(J)) for I = 1,...,NI and 
C           J = 1,...,NJ.
C
C    IER = Error indicator:
C            = -1 if N, NI, or NJ is outside its valid range.
C            = -2 if the first three nodes are collinear.
C            = -3 if extrapolation failed due to the uniform grid 
C                 extending too far beyond the triangulation boundary.
C
C  STRIPACK modules required by CSUNIF:  CSGETNP, CSJRAND, CSLSTPTR,
C                                        CSSTORE, CSTRFIND
C
C  SSRFPACK modules required by CSUNIF:  CSAPLYR, CSAPLYRT, CSARCINT,
C                                        CSARCLEN, CSCONSTR,
C                                        CSFVAL, CSGIVENS, CSGRADL,
C                                        CSHVAL, CSINTRC1,
C                                        CSROTATE, CSSETUP,
C                                        CSSNHCSH
C
C***********************************************************
C
C  For this routine we select a uniform tension factor of 1.
C
      PARAMETER (IFLGS=0, SIGMA=1.)
C
C  Choose to estimate gradients at each triangle vertex and not
C  save them.
C
      PARAMETER (IFL=0)
C
      INTEGER I, J, IERR, IST, NN, NST, NX, NY
      DATA    NST/1/
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
      NN = N
      NX = NI
      NY = NJ
      IF (NN .LT. 3) THEN
        GO TO 110
      ELSE IF (NX .LT. 1) THEN
        GO TO 200
      ELSE IF (NY .LT. 1) THEN
        GO TO 210
      ENDIF
      IST = NST
C
C  Do the triangulation.
C
      CALL CSTRMESH (NN,X,Y,Z, IWK(1),IWK(6*NN+1),IWK(12*NN+1), LNEW,
     +             IWK(13*N+1),IWK(14*N+1),WK(1),IER)
      IF (IER .EQ. 0) THEN
        GO TO 100
      ELSE IF (IER .EQ. -1) THEN
        GO TO 110
      ELSE IF (IER .EQ. -2) THEN
        GO TO 120
      ELSE IF (IER .EQ. -3) THEN
        GO TO 130
      ELSE IF (IER .GT. 0) THEN
        NERR = -IER
        GO TO 140
      ELSE
        GO TO 130
      ENDIF
C
C Compute uniform grid points and interpolated values.
C
      DO 3 J = 1,NY
        DO 2 I = 1,NX
          CALL CSINTRC1 (NN,PLAT(I),PLON(J),X,Y,Z,F,
     +                   IWK(1),IWK(6*NN+1),IWK(12*NN+1),
     +                   IFLGS,SIGMA,IFL,GRAD,IST,FF(I,J),IERR)
          IF (IERR .EQ. 0) THEN
            GO TO 100
          ELSE IF (IERR .EQ. 1) THEN
            GO TO 110
          ELSE IF (IERR .EQ. 4) THEN
            GO TO 120
          ELSE IF (IERR .EQ. 7) THEN
            GO TO 170
          ELSE IF (IERR .EQ. 8) THEN
            GO TO 180
          ELSE IF (IERR .EQ. 9) THEN
            GO TO 190
          ELSE
            GO TO 130
          ENDIF
    2     CONTINUE
    3   CONTINUE
      RETURN
C
  100 CONTINUE
      IER = 0
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
