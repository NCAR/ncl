C
C	$Id: csunif.f,v 1.5 2008-07-27 03:10:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSUNIF (N,X,Y,Z,F,LIST,LPTR,LEND,IFLGS,SIGMA,
     .                 NROW,NI,NJ,PLAT,PLON,IFLGG, GRAD, FF,
     .                 IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IFLGS, NROW, NI,
     .        NJ, IFLGG, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), F(N), SIGMA(*),
     .                 PLAT(NI), PLON(NJ), GRAD(3,N),
     .                 FF(NROW,NJ)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/25/96
C
C   Given a Delaunay triangulation of a set of nodes on the
C unit sphere, along with data values and tension factors
C associated with the triangulation arcs, this routine
C interpolates the data values to a uniform grid for such
C applications as contouring.  The interpolant is once con-
C tinuously differentiable.  Extrapolation is performed at
C grid points exterior to the triangulation when the nodes
C do not cover the entire sphere.
C
C On input:
C
C       N = Number of nodes.  N .GE. 3 and N .GE. 7 if
C           IFLAG .NE. 1.
C
C       X,Y,Z = Arrays containing Cartesian coordinates of
C               the nodes.  X(I)**2 + Y(I)**2 + Z(I)**2 = 1
C               for I = 1 to N.
C
C       F = Array containing data values.  F(I) is associ-
C           ated with (X(I),Y(I),Z(I)).
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       IFLGS = Tension factor option:
C               IFLGS .LE. 0 if a single uniform tension
C                            factor is to be used.
C               IFLGS .GE. 1 if variable tension is desired.
C
C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
C               array containing tension factors associated
C               with arcs in one-to-one correspondence with
C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
C               programs CSGETSIG, CSSIG0, CSSIG1, and CSSIG2.
C
C       NROW = Number of rows in the dimension statement of
C              FF.
C
C       NI,NJ = Number of rows and columns in the uniform
C               grid.  1 .LE. NI .LE. NROW and 1 .LE. NJ.
C
C       PLAT,PLON = Arrays of length NI and NJ, respective-
C                   ly, containing the latitudes and
C                   longitudes of the grid lines.
C
C       IFLGG = Option indicator:
C               IFLGG = 0 if gradient estimates at the ver-
C                         tices of a triangle are to be
C                         recomputed for each grid point in
C                         the triangle and not saved.
C               IFLGG = 1 if gradient estimates are input in
C                         GRAD.
C               IFLGG = 2 if gradient estimates are to be
C                         computed once for each node (by
C                         CSGRADL) and saved in GRAD.
C
C The above parameters are not altered by this routine.
C
C       GRAD = 3 by N array whose columns contain the X, Y,
C              and Z components (in that order) of the grad-
C              ients at the nodes if IFLGG = 1, array of
C              sufficient size if IFLGG = 2, or dummy para-
C              meter if IFLGG = 0.
C
C Gradient estimates may be computed by Subroutines CSGRADL or
C   CSGRADG if IFLGG = 1.
C
C       FF = NROW by NCOL array with NROW .GE. NI and NCOL
C            .GE. NJ.
C
C On output:
C
C       GRAD = Array containing estimated gradients as de-
C              fined above if IFLGG = 2 and IER .GE. 0.
C              GRAD is not altered if IFLGG .NE. 2.
C
C       FF = Interpolated values at the grid points if IER
C            .GE. 0.  FF(I,J) = F(PLAT(I),PLON(J)) for I =
C            1,...,NI and J = 1,...,NJ.
C
C       IER = Error indicator:
C             IER = K if no errors were encountered and K
C                     grid points required extrapolation for
C                     K .GE. 0.
C             IER = -1 if N, NI, NJ, or IFLGG is outside its
C                      valid range.
C             IER = -2 if the nodes are collinear.
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
      INTEGER I, J, IERR, IFL, IST, NEX, NN, NST, NX, NY
      DATA    NST/1/
C
C Local parameters:
C
C I,J =   DO-loop indexes
C IERR =  Error flag for calls to CSGRADL and CSINTRC1
C IFL =   Local copy of IFLGG
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
      IFL = IFLGG
      IF (NX .LT. 1  .OR.  NX .GT. NROW  .OR.  NY .LT. 1
     .   .OR.  IFL .LT. 0  .OR.  IFL .GT. 2) GO TO 4
      IST = NST
      IF (IFL .EQ. 2) THEN
C
C Compute gradient estimates at the nodes.
C
        DO 1 I = 1,NN
          CALL CSGRADL (NN,I,X,Y,Z,F,LIST,LPTR,
     .                LEND, GRAD(1,I),IERR)
          IF (IERR .LT. 0) GO TO 5
    1     CONTINUE
        IFL = 1
      ENDIF
C
C Compute uniform grid points and interpolated values.
C
      NEX = 0
      DO 3 J = 1,NY
        DO 2 I = 1,NX
          CALL CSINTRC1 (NN,PLAT(I),PLON(J),X,Y,Z,F,LIST,LPTR,
     .                 LEND,IFLGS,SIGMA,IFL,
     .                 GRAD, IST, FF(I,J),IERR)
          IF (IERR .LT. 0) GO TO 5
          NEX = NEX + IERR
    2     CONTINUE
    3   CONTINUE
      IER = NEX
      RETURN
C
C NI, NJ, or IFLGG is outside its valid range.
C
    4 IER = -1
      RETURN
C
C Error in CSGRADL or CSINTRC1.
C
    5 IER = IERR
      RETURN
      END
