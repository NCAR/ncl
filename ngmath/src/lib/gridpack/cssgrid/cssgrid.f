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
C   This subroutine provides a simple interface to SSRFPACK.
C It triangulates the input data and does interpolation at
C the specified grid points.  Certain internal parameters are
C assigned default values.
C
C On input:
C
C       N = Number of nodes.  N .GE. 3.
C
C       X,Y,Z = Arrays containing Cartesian coordinates of
C               the nodes.  X(I)**2 + Y(I)**2 + Z(I)**2 = 1
C               for I = 1 to N.
C
C       F = Array containing data values.  F(I) is associ-
C           ated with (X(I),Y(I),Z(I)).
C
C       NI,NJ = Number of rows and columns in the uniform
C               grid.  1 .LE. NI .LE. NROW and 1 .LE. NJ.
C
C       PLAT,PLON = Arrays of length NI and NJ, respective-
C                   ly, containing the latitudes and
C                   longitudes of the grid lines.
C
C       FF = NROW by NCOL array with NROW .GE. NI and NCOL
C            .GE. NJ.
C
C       IWK  =  An integer workspace of length 15*N.
C
C       RWK  =  A real workspace of length N.
C
C On output:
C
C       FF = Interpolated values at the grid points if IER
C            .GE. 0.  FF(I,J) = F(PLAT(I),PLON(J)) for I =
C            1,...,NI and J = 1,...,NJ.
C
C       IER = Error indicator:
C             IER = K if no errors were encountered and K
C                     grid points required extrapolation for
C                     K .GE. 0.
C             IER = -1 if N, NI, or NJ is outside its valid range.
C             IER = -2 if the first three nodes are collinear.
C             IER = -3 if extrapolation failed due to the
C                      uniform grid extending too far beyond
C                      the triangulation boundary.
C
C STRIPACK modules required by CSUNIF:  CSGETNP, CSJRAND, CSLSTPTR,
C                                       CSSTORE, CSTRFIND
C
C SSRFPACK modules required by CSUNIF:  CSAPLYR, CSAPLYRT, CSARCINT,
C                                       CSARCLEN, CSCONSTR,
C                                       CSFVAL, CSGIVENS, CSGRADL,
C                                       CSHVAL, CSINTRC1,
C                                       CSROTATE, CSSETUP,
C                                       CSSNHCSH
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
      INTEGER I, J, IERR, IST, NEX, NN, NST, NX, NY
      DATA    NST/1/
C
C Local parameters:
C
C I,J =   DO-loop indexes
C IERR =  Error flag for calls to CSGRADL and CSINTRC1
C IST =   Parameter for CSINTRC1
C NEX =   Number of grid points exterior to the triangula-
C           tion boundary (number of extrapolated values)
C NN =    Local copy of N
C NST =   Initial value for IST
C NX,NY = Local copies of NI and NJ
C
      NN = N
      NX = NI
      NY = NJ
      IF (NN.LT.1 .OR. NX.LT.1  .OR. NY.LT.1) GO TO 4
      IST = NST
C
C  Do the triangulation.
C
      CALL CSTRMESH (NN,X,Y,Z, IWK(1),IWK(6*NN+1),IWK(12*NN+1), LNEW,
     +             IWK(13*N+1),IWK(14*N+1),WK(1),IER)
C
C Compute uniform grid points and interpolated values.
C
      NEX = 0
      DO 3 J = 1,NY
        DO 2 I = 1,NX
          CALL CSINTRC1 (NN,PLAT(I),PLON(J),X,Y,Z,F,
     +                   IWK(1),IWK(6*NN+1),IWK(12*NN+1),
     +                   IFLGS,SIGMA,IFL,GRAD,IST,FF(I,J),IERR)
          IF (IERR .LT. 0) GO TO 5
          NEX = NEX + IERR
    2     CONTINUE
    3   CONTINUE
      IER = NEX
      RETURN
C
C N, NI or NJ is outside its valid range.
C
    4 IER = -1
      RETURN
C
C Error in CSGRADL or CSINTRC1.
C
    5 IER = IERR
      RETURN
      END
